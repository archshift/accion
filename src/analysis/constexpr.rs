use std::collections::HashMap;
use std::mem;

use crate::ast::{self, AstNodeWrap};
use crate::analysis::{Visitor};
use crate::analysis::scoping::{Scopes, ScopeId};
use crate::analysis::typing::Types;
use crate::values::{self, Value};

#[derive(Debug)]
pub(crate) struct Environment {
    inner: HashMap<String, Value>,
}

impl Environment {
    fn from_scope(scopes: &Scopes, id: ScopeId) -> Self {
        let mut out = Self {
            inner: HashMap::new(),
        };
        let scope = scopes.get(id);
        for decl in scope.decls() {
            out.inner.insert(decl.name.clone(), Value::Undefined);
        }
        out
    }
    fn add(&mut self, name: &'static str, val: Value) {
        let old = self.inner.insert(name.into(), val);
        if let Some(Value::Undefined) = old {}
        else { panic!("Attempted redefine value `{}` (prev={:?}, new={:?}) in same scope!", name, old, self.inner[name]) }
    }
    fn get(&self, name: &'static str) -> &Value {
        &self.inner[name]
    }
}

#[derive(Debug)]
pub(crate) struct Interpreter<'a> {
    pub(crate) environments: HashMap<ScopeId, Vec<Environment>>,
    pub(crate) frames: Vec<ScopeId>,
    pub(crate) scopes: &'a Scopes,
    pub(crate) types: &'a mut Types,
    yield_val: Value,
}
impl Interpreter<'_> {
    fn top_frame(&mut self) -> &mut Environment {
        let scope = self.frames.last().unwrap();
        self.environments.get_mut(scope)
            .unwrap()
            .last_mut()
            .unwrap()
    }
    fn push_frame(&mut self, scope: ScopeId) {
        self.frames.push(scope);
        let env = self.environments
            .entry(scope)
            .or_insert_with(Vec::new);
        env.push(Environment::from_scope(self.scopes, scope));
    }
    fn pop_frame(&mut self) {
        if let Some(frame) = self.frames.pop() {
            self.environments.get_mut(&frame)
                .unwrap()
                .pop();
        }
    }
    fn define(&mut self, name: &'static str, val: Value) {
        self.top_frame()
            .add(name, val)
    }
    fn take_yield(&mut self) -> Value {
        mem::replace(&mut self.yield_val, Value::Undefined)
    }
}

pub fn analyze(ast: &'static ast::Ast, scopes: &Scopes, types: &mut Types) {
    let mut ctx = Interpreter {
        frames: Vec::new(),
        environments: HashMap::new(),
        scopes,
        types,
        yield_val: Value::Undefined
    };
    ctx.push_frame(Scopes::global());
    ctx.visit_ast(ast);
    dbg!(ctx.frames);
    dbg!(ctx.environments);
}

impl Visitor for Interpreter<'_> {
    fn visit_expr_binary(&mut self, node: &'static ast::ExprBinary) {
        let (left, right) = node.operands();
        self.visit_expr(left);
        let left = self.take_yield();
        self.visit_expr(right);
        let right = self.take_yield();
        
        self.yield_val = match node.operator() {
            ast::BinaryOp::Add => match (left, right) {
                (Value::Int(left), Value::Int(right)) => Value::Int(left + right),
                _ => Value::Undefined
            },
            ast::BinaryOp::Sub => match (left, right) {
                (Value::Int(left), Value::Int(right)) => Value::Int(left - right),
                _ => Value::Undefined
            },
            ast::BinaryOp::Mul => match (left, right) {
                (Value::Int(left), Value::Int(right)) => Value::Int(left * right),
                _ => Value::Undefined
            },
            ast::BinaryOp::Div => unimplemented!(),
            ast::BinaryOp::Mod => unimplemented!(),
            ast::BinaryOp::Eq => Value::Bool(left == right),
            ast::BinaryOp::Prepend => match right {
                Value::List(list) => Value::List(list.prepended(left)),
                _ => Value::Undefined
            },
            ast::BinaryOp::LastUnit => unimplemented!(),
        };
    }
    fn visit_expr_unary(&mut self, node: &'static ast::ExprUnary) {
        let operand = node.operand();
        self.visit_expr(operand);
        let operand = self.take_yield();
        
        self.yield_val = match node.operator() {
            ast::UnaryOp::Head => match operand {
                Value::List(list) => list.head().unwrap().clone(),
                _ => unimplemented!()
            },
            ast::UnaryOp::Tail => match operand {
                Value::List(list) => Value::List(list.tail()),
                _ => unimplemented!()
            },
            ast::UnaryOp::Negate => unimplemented!(),
        };
    }
    fn visit_expr_fn_call(&mut self, node: &'static ast::ExprFnCall) {
        self.visit_expr(node.callee());
        let callee = self.take_yield();
        let callee_node = if let Value::Fn(callee_node) = callee {
            callee_node
        } else {
            unreachable!()
        };
        let callee_val = callee_node.val();

        let arg_vals: values::FnArgs = node.args()
            .map(|e| { self.visit_expr(e); self.take_yield() })
            .collect();
        let arg_vals = arg_vals.into_iter();
        let arg_names = callee_node.args()
            .map(|a| a.name());
        
        let new_scope = self.scopes.node_scope(callee_val.node_id())
            .unwrap();
        self.push_frame(new_scope);
        for (name, val) in arg_names.zip(arg_vals) {
            self.define(name, val)
        }

        self.visit_expr(callee_val);
        self.pop_frame();
    }
    
    fn visit_expr_if(&mut self, node: &'static ast::ExprIf) {
        self.visit_expr(node.cond());
        let cond_val = self.take_yield();
        let chosen = match cond_val {
            Value::Bool(true) => node.then_expr(),
            Value::Bool(false) => node.else_expr(),
            _ => unreachable!()
        };
        self.visit_expr(chosen);
    }
    fn visit_expr_if_case(&mut self, node: &'static ast::ExprIfCase) {
        self.visit_expr(node.cond());
        let cond_val = self.take_yield();
        for case in node.cases() {
            let chosen = match case {
                ast::IfCase::OnVal(lit, val) => {
                    self.visit_literal(*lit);
                    if self.yield_val != cond_val { continue }
                    val
                }
                ast::IfCase::Else(out) => out
            };

            self.visit_expr(*chosen);
            break
        }
    }
    
    fn visit_expr_entype(&mut self, node: &'static ast::ExprEntype) {
        let target = node.target();
        self.visit_expr(node.ty());
        let ty =
            if let Value::Type(t) = self.take_yield() { t }
            else { panic!("Can't entype with non-type value!") };
        
        unimplemented!()
    }
    
    fn visit_expr_var_decl(&mut self, node: &'static ast::ExprVarDecl) {
        self.visit_expr(node.val());
        let val = self.take_yield();
        let name = node.name();
        self.define(name.name(), val);
    }

    fn visit_expr_fn_decl(&mut self, node: &'static ast::ExprFnDecl) {
        let val = Value::Fn(node);
        if let Some(name) = node.name() {
            self.define(name.name(), val.clone());
        }
        self.yield_val = val;
    }

    fn visit_expr_literal(&mut self, node: &'static ast::ExprLiteral) {
        self.visit_literal(node.literal());
    }
    fn visit_expr_ident(&mut self, node: &'static ast::ExprIdent) {
        self.visit_ident(node.ident());
    }
    fn visit_expr_curry(&mut self, _: &'static ast::ExprCurry) {
        unimplemented!()
    }

    fn visit_literal_str(&mut self, node: &'static ast::LiteralString) {
        self.yield_val = Value::String(node.val())
    }
    fn visit_literal_int(&mut self, node: &'static ast::LiteralInt) {
        self.yield_val = Value::Int(node.val())
    }
    fn visit_literal_bool(&mut self, node: &'static ast::LiteralBool) {
        self.yield_val = Value::Bool(node.val())
    }
    fn visit_literal_nil(&mut self, _: &'static ast::LiteralNil) {
        self.yield_val = Value::List(values::ValList::empty())
    }
    fn visit_ident(&mut self, node: &'static ast::Ident) {
        let name = node.name();
        let scope = self.frames.last().unwrap();
        let decl = self.scopes.resolve(*scope, name)
            .unwrap();
        // Only support referencing either global or immediately local vars for now
        assert!(Some(&decl.scope) == self.frames.last()
             || Some(&decl.scope) == self.frames.first());
        self.yield_val =
            self.environments[&decl.scope]
            .last()
            .expect("Could not find frame for decl!")
            .get(name)
            .clone();
    }
}
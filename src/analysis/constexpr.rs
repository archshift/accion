use std::collections::HashMap;
use std::mem;
use std::rc::Rc;
use std::cell::RefCell;

use derive_newtype::NewType;

use crate::ast;
use crate::analysis::{Visitor};
use crate::analysis::typing::Types;
use crate::values::{self, Value};

#[derive(NewType, Debug, Clone)]
struct RcEnv(Rc<RefCell<Environment>>);
impl RcEnv {
    fn new(parent: Option<RcEnv>) -> Self {
        Self(Rc::new(RefCell::new(
            Environment {
                inner: HashMap::new(),
                parent
            }
        )))
    }
}

#[derive(Debug)]
pub(crate) struct Environment {
    inner: HashMap<String, Value>,
    parent: Option<RcEnv>,
}

impl Environment {
    fn add(&mut self, name: &'static str, val: Value) -> Result<(), String> {
        let old = self.inner.insert(name.into(), val);
        match old {
            None => Ok(()),
            _ => Err(format!("Attempted redefine value `{}` (prev={:?}, new={:?}) in same scope!",
                    name, old, self.inner[name])),
        }
    }
}

fn err_recover<V>(res: Result<V, String>, loc: &ast::Location, or: V) -> V {
    match res {
        Ok(out) => out,
        Err(e) => { eprintln!("error: {:?}  {}", loc, e); or }
    }
}

#[derive(Debug)]
pub(crate) struct Interpreter<'a> {
    frames: Vec<RcEnv>,
    yield_val: Value,
    pub(crate) types: &'a mut Types,
}
impl Interpreter<'_> {
    fn push_frame(&mut self) {
        let parent = self.frames.last().cloned();
        self.frames.push(RcEnv::new(parent))
    }
    fn pop_frame(&mut self) {
        self.frames.pop();
    }
    fn define(&mut self, name: &'static str, val: Value) -> Result<(), String> {
        self.frames.last()
            .unwrap()
            .borrow_mut()
            .add(name, val)
    }
    fn take_yield(&mut self) -> Value {
        mem::replace(&mut self.yield_val, Value::Undefined)
    }
    
    fn get(&self, name: &'static str) -> Result<Value, String> {
        let mut parent = self.frames.last()
            .map(|p| p.clone());
            
        while let Some(p) = parent {
            let env = p.borrow();
            if let Some(val) = env.inner.get(name) {
                return Ok(val.clone())
            }

            parent = env.parent.clone();
        }
        Err(format!("Could not find value with name `{}`!", name))
    }
}

pub fn analyze(ast: &'static ast::Ast, types: &mut Types) {
    let mut ctx = Interpreter {
        frames: Vec::new(),
        types,
        yield_val: Value::Undefined
    };
    ctx.push_frame();
    ctx.visit_ast(ast);
    dbg!(ctx.frames);
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
                (Value::Int(left), Value::Int(right)) => Value::Int(left.wrapping_sub(right)),
                _ => Value::Undefined
            },
            ast::BinaryOp::Mul => match (left, right) {
                (Value::Int(left), Value::Int(right)) => Value::Int(left * right),
                _ => Value::Undefined
            },
            ast::BinaryOp::Div => match (left, right) {
                (Value::Int(left), Value::Int(right)) => Value::Int(left / right),
                _ => Value::Undefined
            },
            ast::BinaryOp::Mod => match (left, right) {
                (Value::Int(left), Value::Int(right)) => Value::Int(left % right),
                _ => Value::Undefined
            },
            ast::BinaryOp::Eq => Value::Bool(left == right),
            ast::BinaryOp::Prepend => match right {
                Value::List(list) => Value::List(list.prepended(left)),
                _ => Value::Undefined
            },
            ast::BinaryOp::LastUnit => right,
        };
    }
    fn visit_expr_unary(&mut self, node: &'static ast::ExprUnary) {
        let operand = node.operand();
        self.visit_expr(operand);
        let operand = self.take_yield();
        
        self.yield_val = match node.operator() {
            ast::UnaryOp::Head => match operand {
                Value::List(list) => {
                    let res = list.head()
                        .ok_or_else(|| "Taking head of empty list!".to_owned());
                    err_recover(res, node.loc(), &Value::Undefined)
                        .clone()
                }
                _ => Value::Undefined,
            },
            ast::UnaryOp::Tail => match operand {
                Value::List(list) => Value::List(list.tail()),
                _ => Value::Undefined,
            },
            ast::UnaryOp::Negate => match operand {
                Value::Int(val) => Value::Int(!val + 1),
                _ => Value::Undefined
            },
        };
    }
    fn visit_expr_fn_call(&mut self, node: &'static ast::ExprFnCall) {
        self.visit_expr(node.callee());
        let callee = self.take_yield();

        let arg_vals: values::FnArgs = node.args()
            .map(|e| { self.visit_expr(e); self.take_yield() })
            .collect();
        let arg_vals = arg_vals.into_iter();

        let callee_node = if let Value::Fn(callee_node) = callee {
            callee_node
        } else {
            self.yield_val = Value::Undefined;
            return
        };
        let callee_val = callee_node.val();
        let arg_names = callee_node.args()
            .map(|a| a.name());
        
        self.push_frame();
        for (name, val) in arg_names.zip(arg_vals) {
            let res = self.define(name, val);
            err_recover(res, node.loc(), ());
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
        let res = self.define(name.name(), val);
        err_recover(res, node.loc(), ());
    }

    fn visit_expr_fn_decl(&mut self, node: &'static ast::ExprFnDecl) {
        let val = Value::Fn(node);
        if let Some(name) = node.name() {
            let res = self.define(name.name(), val.clone());
            err_recover(res, node.loc(), ());
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
        let res = self.get(name);
        self.yield_val = err_recover(res, node.loc(), Value::Undefined);
    }
}
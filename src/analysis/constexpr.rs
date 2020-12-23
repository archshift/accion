use std::collections::HashMap;
use std::mem;
use std::rc::Rc;
use std::cell::RefCell;

use derive_newtype::NewType;

use crate::{types::Purity, ast::{self, AstNodeWrap}};
use crate::analysis::{Visitor};
use crate::analysis::scoping::{Scopes, Definition};
use crate::analysis::purity::Purities;
use crate::values::{self, Value};
use crate::interpreter::Interpreter;
use crate::types::{TypeStore, TypeId};

#[derive(NewType, Debug, Clone)]
struct RcEnv(Rc<RefCell<Environment>>);
impl RcEnv {
    fn new(parent: Option<RcEnv>) -> Self {
        Self(Rc::new(RefCell::new(
            Environment {
                inner: HashMap::new(),
                entypes: HashMap::new(),
                parent
            }
        )))
    }
}

#[derive(Debug)]
pub(crate) struct Environment {
    inner: HashMap<String, Value>,
    entypes: HashMap<String, TypeId>,
    parent: Option<RcEnv>,
}

impl Environment {
    fn add(&mut self, name: &str, val: Value) -> Result<(), String> {
        let old = self.inner.insert(name.into(), val);
        match old {
            None => Ok(()),
            _ => Err(format!("Attempted to redefine value `{}` (prev={:?}, new={:?}) in same scope!",
                    name, old, self.inner[name])),
        }
    }
    fn get(&self, name: &str) -> Result<Value, String> {
        if let Some(val) = self.inner.get(name) {
            return Ok(val.clone())
        }

        let mut parent = self.parent.clone();
        while let Some(p) = parent {
            let env = p.borrow();
            if let Some(val) = env.inner.get(name) {
                return Ok(val.clone())
            }

            parent = env.parent.clone();
        }
        Err(format!("Could not find value with name `{}`!", name))
    }

    fn add_entype(&mut self, name: &str, ty: TypeId) {
        let old = self.entypes.insert(name.into(), ty);
        assert!(old.is_none());
    }
    fn get_entype(&self, name: &str) -> Result<TypeId, String> {
        self.entypes.get(name)
            .copied()
            .ok_or_else(|| format!("Could not find entype with name `{}`!", name))
    }
}

fn err_recover<V>(res: Result<V, String>, loc: &ast::Location, or: V) -> V {
    match res {
        Ok(out) => out,
        Err(e) => { eprintln!("error: {:?}  {}", loc, e); or }
    }
}

#[derive(Debug)]
pub(crate) struct ConstEvalCtx<'a> {
    frames: Vec<RcEnv>,
    types: &'a mut TypeStore,
    scopes: &'a Scopes,
    purity: &'a Purities,
}
impl Interpreter for ConstEvalCtx<'_> {
    fn push_frame(&mut self) {
        let parent = self.frames.last().cloned();
        self.frames.push(RcEnv::new(parent))
    }
    fn pop_frame(&mut self) {
        self.frames.pop();
    }
    fn define(&mut self, name: &str, val: Value) -> Result<(), String> {
        self.frames.last()
            .unwrap()
            .borrow_mut()
            .add(name, val)
    }
    fn get(&self, name: &str) -> Result<Value, String> {
        self.frames.last()
            .ok_or_else(|| "No frame found!".to_owned())
            .and_then(|p| p.borrow().get(name))
    }
}
impl ConstEvalCtx<'_> {
    fn visit_pure<T: AstNodeWrap>(&mut self, node: &T) -> Value {
        let any_node = node.as_any();
        let purity = self.purity.node_purity(any_node.clone());
        match purity {
            Some(Purity::Pure) | Some(Purity::Any) => self.visit_node(&any_node),
            _ => Value::Undefined,
        }
    }

    fn define_entype(&mut self, name: &str, val: TypeId) {
        self.frames.last()
            .unwrap()
            .borrow_mut()
            .add_entype(name, val)
    }
}

pub struct ConstVals {
    global_frame: RcEnv
}

impl ConstVals {
    pub fn entype_val(&self, decl: &Definition) -> Result<TypeId, String> {
        self.global_frame.0.borrow()
            .get_entype(&decl.name)
    }

    pub fn node_val(&self, node: ast::AstNode) -> Option<Value> {
        let name = match node {
            ast::AstNode::ExprVarDecl(vd) => vd.name().clone(),
            ast::AstNode::ExprFnDecl(vd) =>
                if let Some(name) = vd.name() { name.clone() }
                else { return None }
            _ => return None
        };
        self.global_frame.0.borrow()
            .get(name.name())
            .ok()
    }
}

pub fn analyze(ast: &Rc<ast::Ast>, types: &mut TypeStore, purity: &Purities, scopes: &Scopes) -> ConstVals {
    let mut ctx = ConstEvalCtx {
        frames: Vec::new(),
        types,
        scopes,
        purity,
    };
    ctx.push_frame();

    for (name, builtin) in &scopes.builtins {
        ctx.define(name, builtin.val.clone())
            .unwrap();
    }

    ctx.visit_ast(ast);

    ConstVals {
        global_frame: ctx.frames.into_iter().next().unwrap()
    }
}

impl Visitor for ConstEvalCtx<'_> {
    type Ret = Value;

    fn visit_ast(&mut self, ast: &Rc<ast::Ast>) -> Value {
        for decl in ast.decls() {
            self.visit_pure(decl);
        }
        Value::Undefined
    }
    fn visit_expr_binary(&mut self, node: &Rc<ast::ExprBinary>) -> Value {
        let (left, right) = node.operands();
        let left = self.visit_pure(left);
        let right = self.visit_pure(right);
        
        match node.operator() {
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
        }
    }
    fn visit_expr_unary(&mut self, node: &Rc<ast::ExprUnary>) -> Value {
        let operand = node.operand();
        let operand = self.visit_pure(operand);
        
        match node.operator() {
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
        }
    }
    fn visit_expr_fn_call(&mut self, node: &Rc<ast::ExprFnCall>) -> Value {
        let callee = self.visit_pure(node.callee());

        let arg_vals: values::FnArgs = node.args()
            .map(|e| self.visit_pure(e))
            .collect();

        match callee {
            Value::Fn(callee_node) => {
                let arg_vals = arg_vals.into_iter();
                let callee_val = callee_node.val();
                let arg_names = callee_node.args()
                    .map(|a| a.name());
                
                self.push_frame();
                for (name, val) in arg_names.zip(arg_vals) {
                    let res = self.define(name, val);
                    err_recover(res, node.loc(), ());
                }

                let out = self.visit_pure(callee_val);
                self.pop_frame();
                out
            }
            Value::TypeFn(func) => {
                func(arg_vals, &mut self.types)
            }
            Value::BuiltinFn(func) => {
                func(arg_vals, self)
            }
            _ => Value::Undefined
        }
    }
    
    fn visit_expr_if(&mut self, node: &Rc<ast::ExprIf>) -> Value {
        let cond_val = self.visit_pure(node.cond());
        let chosen = match cond_val {
            Value::Bool(true) => node.then_expr(),
            Value::Bool(false) => node.else_expr(),
            Value::Undefined => return Value::Undefined,
            _ => unreachable!()
        };
        self.visit_pure(chosen)
    }
    fn visit_expr_if_case(&mut self, node: &Rc<ast::ExprIfCase>) -> Value {
        let cond_val = self.visit_pure(node.cond());
        for case in node.cases() {
            let chosen = match case {
                ast::IfCase::OnVal(lit, val) => {
                    if self.visit_pure(lit) != cond_val { continue }
                    val
                }
                ast::IfCase::Else(out) => out
            };

            return self.visit_pure(chosen)
        }
        Value::Undefined
    }
    
    fn visit_expr_entype(&mut self, node: &Rc<ast::ExprEntype>) -> Value {
        let target = node.target();
        let out = self.visit_pure(node.ty());
        let ty =
            if let Value::Type(t) = out { t }
            else { panic!("Can't entype with non-type value!") };
        self.define_entype(target.name(), ty);
        out
    }
    
    fn visit_expr_var_decl(&mut self, node: &Rc<ast::ExprVarDecl>) -> Value {
        let val = self.visit_pure(node.val());
        let name = node.name();
        let res = self.define(name.name(), val.clone());
        err_recover(res, node.loc(), ());
        val
    }

    fn visit_expr_fn_decl(&mut self, node: &Rc<ast::ExprFnDecl>) -> Value {
        let val = Value::Fn(node.clone());
        if let Some(name) = node.name() {
            let res = self.define(name.name(), val.clone());
            err_recover(res, node.loc(), ());
        }
        val
    }

    fn visit_expr_literal(&mut self, node: &Rc<ast::ExprLiteral>) -> Value {
        self.visit_pure(node.literal())
    }
    fn visit_expr_ident(&mut self, node: &Rc<ast::ExprIdent>) -> Value {
        self.visit_ident(node.ident())
    }
    fn visit_expr_curry(&mut self, _: &Rc<ast::ExprCurry>) -> Value {
        unimplemented!()
    }

    fn visit_literal_str(&mut self, node: &Rc<ast::LiteralString>) -> Value {
        Value::String(node.val().into())
    }
    fn visit_literal_int(&mut self, node: &Rc<ast::LiteralInt>) -> Value {
        Value::Int(*node.val())
    }
    fn visit_literal_bool(&mut self, node: &Rc<ast::LiteralBool>) -> Value {
        Value::Bool(*node.val())
    }
    fn visit_literal_nil(&mut self, _: &Rc<ast::LiteralNil>) -> Value {
        Value::List(values::ValList::empty())
    }
    fn visit_ident(&mut self, node: &Rc<ast::Ident>) -> Value {
        let name = node.name();
        let res = self.get(name);
        err_recover(res, node.loc(), Value::Undefined)
    }
}
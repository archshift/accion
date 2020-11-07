use std::cmp::{PartialEq, Eq};
use std::ptr;
use std::any::Any;
use std::fmt;
use std::rc::Rc;
use smallvec::SmallVec;

use crate::ast;
use crate::types::TypeId;
use crate::lliter::adapt_ll;

pub type FnArgs = SmallVec<[Value; 4]>;
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ValList(Option<Rc<ValListItem>>);
impl ValList {
    pub fn empty() -> Self {
        Self(None)
    }
    pub fn iter(&self) -> impl Iterator<Item=&Value> {
        let inner = self.0.as_ref();
        adapt_ll(
            inner.map(|item| item.as_ref()), 
            |i| i.tail.0.as_ref().map(|i| i.as_ref()))
        .map(|i| &i.head)
    }
}
#[derive(Clone, Debug, PartialEq, Eq)]
struct ValListItem {
    head: Value,
    tail: ValList,
}



#[derive(Clone)]
pub enum Value {
    Int(u64),
    Bool(bool),
    String(&'static str),
    Type(TypeId),
    List(ValList),
    Fn(&'static ast::ExprFnDecl),
    BuiltinFn(fn(FnArgs, &mut dyn Any) -> Value),
    Undefined,
}

impl PartialEq for Value {
    // TODO: potentially misleading that Value::Undefined != Value::Undefined
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (Self::Int(i), Self::Int(j)) => i == j,
            (Self::Bool(i), Self::Bool(j)) => i == j,
            (Self::String(i), Self::String(j)) => i == j,
            (Self::Type(i), Self::Type(j)) => i == j,
            (Self::List(i), Self::List(j)) => i == j,
            (Self::Fn(i), Self::Fn(j)) => ptr::eq(i, j),
            _ => false
        }
    }
}
impl Eq for Value { }

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Int(i) => write!(f, "Value({})", i),
            Value::Bool(b) => write!(f, "Value({:?})", b),
            Value::String(s) => write!(f, "Value({:?})", s),
            Value::Type(t) => write!(f, "Value(Type {:?})", t),
            Value::List(fn_) => write!(f, "Value({:?})", fn_),
            Value::Fn(fn_) => write!(f, "Value(Fn {:?})", fn_),
            Value::BuiltinFn(_) => write!(f, "Value(BuiltinFn)"),
            Value::Undefined => write!(f, "Value(Undefined)"),
        }
    }
}

use crate::types::TypeId;

pub enum Value {
    Int(u64),
    String(String),
    Type(TypeId),
    Undefined
}
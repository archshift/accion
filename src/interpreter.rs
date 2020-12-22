use crate::values::Value;

pub trait Interpreter {
    fn push_frame(&mut self);
    fn pop_frame(&mut self);
    fn define(&mut self, name: &str, val: Value) -> Result<(), String>;
    fn get(&self, name: &str) -> Result<Value, String>;
}
use crate::ast::{Ast};

#[derive(Debug)]
pub enum Error {
    ParseError,
}

pub fn parse_stdin() -> Result<&'static Ast, Error> {
    let mut ast: Option<&'static Ast> = None;
    extern {
        fn parse_stdin(ast_out: &mut Option<&'static Ast>) -> i32;
    }
    let err = unsafe { parse_stdin(&mut ast) };
    if err == 0 {
        Ok(ast.unwrap())
    } else {
        Err(Error::ParseError)
    }
}

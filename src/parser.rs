use crate::ast::{Ast};
use std::ptr;
use std::rc::Rc;

#[derive(Debug)]
pub enum Error {
    ParseError,
}

#[repr(C)]
enum YyinType {
    Stdin,
    File,
    Mem
}

pub enum ParseInput<'a> {
    Stdin,
    File(&'a str),
    StrBuf(&'a str),
}

pub fn parse(input: ParseInput) -> Result<Rc<Ast>, Error> {
    let mut ast: Option<&'static Ast> = None;
    extern {
        fn parse_yyin(ast_out: &mut Option<&'static Ast>, ytype: YyinType, mem: *const u8, limit: usize) -> i32;
    }

    let (ytype, mem, limit) = match input {
        ParseInput::Stdin => (YyinType::Stdin, ptr::null(), 0),
        ParseInput::File(name) => (YyinType::File, name.as_ptr(), 0),
        ParseInput::StrBuf(buf) => (YyinType::Mem, buf.as_ptr(), buf.len())
    };

    let err = unsafe { parse_yyin(&mut ast, ytype, mem, limit) };
    if err == 0 {
        Ok(unsafe { Rc::from_raw(ast.unwrap()) })
    } else {
        Err(Error::ParseError)
    }
}

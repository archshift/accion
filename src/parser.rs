use std::ptr;
use std::ffi::CStr;

use crate::ast::{Ast};

pub mod raw {
    #![allow(non_upper_case_globals)]
    #![allow(non_camel_case_types)]
    #![allow(non_snake_case)]
    #![allow(dead_code)]
    #![allow(improper_ctypes)]

    include!(concat!(env!("OUT_DIR"), "/parser.rs"));
}

#[derive(Debug)]
pub enum Error {
    ParseError,
}

pub fn parse_stdin() -> Result<Ast, Error> {
    let mut ast: *mut raw::ast_program = ptr::null_mut();
    let err = unsafe { raw::parse_stdin(&mut ast) };
    if err == 0 {
        Ok(Ast::wrap(ast))
    } else {
        Err(Error::ParseError)
    }
}

unsafe fn c_str<'a>(ptr: *const i8) -> &'a str {
    CStr::from_ptr(ptr)
        .to_str()
        .expect("UTF-8 error decoding C string")
}

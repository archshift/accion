use std::ptr;
mod expr;

mod bindings {
    #![allow(non_upper_case_globals)]
    #![allow(non_camel_case_types)]
    #![allow(non_snake_case)]
    #![allow(dead_code)]
    #![allow(improper_ctypes)]

    include!(concat!(env!("OUT_DIR"), "/parser.rs"));
}

fn main() {
    let mut ast: *mut bindings::ast_program = ptr::null_mut();
    let err = unsafe { bindings::parse_stdin(&mut ast) };
    if err == 0 {
        unsafe {
            bindings::print_program(ast);
        }
    }
    println!("parse output: {}", err);
}

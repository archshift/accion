mod parser;
mod ast;
mod sexp;
mod lliter;
mod analysis;
mod disjoint_set;
mod builtins;

use crate::analysis::{scoping, typing};

fn main() {
    let res = parser::parse_stdin();
    println!("{:?}", res.as_ref().unwrap());
    if let Ok(ast) = res {
        let mut types = typing::TypeStore::new();
        let builtins = builtins::make_builtins(&mut types);
        let scopes = scoping::analyze(&ast, builtins);
        println!("{:#?}", scopes);
        let types = typing::analyze(&ast, types, &scopes);
        println!("{:#?}", types);
    }
}

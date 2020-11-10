mod parser;
pub mod ast;
mod sexp;
mod lliter;
mod analysis;
mod disjoint_set;
mod builtins;
mod id_map;
mod types;
mod values;

use std::env::args;
use crate::analysis::{scoping, typing, constexpr};

fn next_arg<'a>(it: &mut impl Iterator<Item=String>, storage: &'a mut Option<String>) -> Option<&'a str> {
    *storage = it.next();
    storage.as_ref().map(|s| s.as_str())
}

fn main() {
    let mut args = args();
    let mut arg_storage = None;

    let ast = parser::parse_stdin()
        .expect("parse error");
        
    let _prgm = next_arg(&mut args, &mut arg_storage);
    let cmd = next_arg(&mut args, &mut arg_storage);
    
    if let Some("ast") = cmd {
        println!("{:?}", ast);
        return
    }

    let mut types = types::TypeStore::new();    
    let builtins = builtins::make_builtins(&mut types);
    let scopes = scoping::analyze(&ast, builtins);
    
    if let Some("scopes") = cmd {
        println!("{:#?}", scopes);
        return
    }

    let mut types = typing::analyze(&ast, types, &scopes);

    if let Some("types") = cmd {
        analysis::preorder(ast, |node| {
            if let Some(ty) = types.node_type(node.as_dyn().node_id()) {
                println!("{:30}  ~  {:?}", types.store.format_ty(ty), node);
            }
        });
        return
    }

    constexpr::analyze(&ast, &mut types);
}

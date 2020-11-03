mod parser;
mod ast;
mod sexp;
mod lliter;
mod analysis;
mod disjoint_set;
mod builtins;
mod id_map;
mod types;

use crate::analysis::{scoping, typing};

fn main() {
    let res = parser::parse_stdin();
    if let Ok(ast) = res {
        let mut types = types::TypeStore::new();
        let builtins = builtins::make_builtins(&mut types);
        let scopes = scoping::analyze(&ast, builtins);
        println!("{:#?}", scopes);
        let types = typing::analyze(&ast, types, &scopes);
        println!("{:#?}", types);

        analysis::preorder(ast, |node| {
            if let Some(ty) = types.node_type(node.as_dyn().node_id()) {
                println!("{:?}  ~  {:?}", ty, node);
            }
        });
    }
}

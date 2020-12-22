use std::env::args;
use accion::analysis::{self, scoping, typing, purity, constexpr};
use accion::{types, builtins, parser};
use accion::ast::AstNodeWrap;

fn next_arg<'a>(it: &mut impl Iterator<Item=String>, storage: &'a mut Option<String>) -> Option<&'a str> {
    *storage = it.next();
    storage.as_ref().map(|s| s.as_str())
}

fn main() {
    let mut args = args();
    let mut arg_storage = None;

    let ast = parser::parse(parser::ParseInput::Stdin)
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

    let purity = purity::analyze(&ast, &scopes);

    if let Some("purity") = cmd {
        analysis::preorder(&ast, |node| {
            if let Some(node_purity) = purity.node_purity(node.as_any()) {
                println!("{:10?}  ~  {:?}", node_purity, node);
            }
        });
        return
    }

    let constexpr = constexpr::analyze(&ast, &mut types, &purity, &scopes);

    let types = typing::analyze(&ast, types, &scopes, &constexpr);

    if let Some("types") = cmd {
        analysis::preorder(&ast, |node| {
            if let Some(ty) = types.node_type(node.node_id()) {
                println!("{:30}  ~  {:?}", types.store.format_ty(ty), node);
            }
        });
        return
    }
}

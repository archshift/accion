mod parser;
mod ast;
mod sexp;
mod lliter;
mod scoping;
mod typing;
mod disjoint_set;

fn main() {
    let res = parser::parse_stdin();
    println!("{:?}", res.as_ref().unwrap());
    if let Ok(ast) = res {
        let scopes = scoping::analyze(&ast);
        println!("{:#?}", scopes);
        let types = typing::analyze(&ast, &scopes);
        println!("{:#?}", types);
    }
}

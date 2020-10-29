mod expr;
mod parser;
mod ast;
mod sexp;
mod lliter;

fn main() {
    let res = parser::parse_stdin();
    println!("{:?}", res.unwrap());
}

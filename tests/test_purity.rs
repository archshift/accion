use accion::parser;
use accion::ast::{self, AstNodeWrap};
use accion::analysis::{scoping, purity};
use accion::builtins;
use accion::types;
use types::Purity;

use std::rc::Rc;

mod utils;

struct TestPurityCtx {
    ast: Rc<ast::Ast>,
    purity: purity::Purities,
}

impl TestPurityCtx {
    fn analyze(program: &str) -> Self {
        let ast = parser::parse(parser::ParseInput::StrBuf(program));
        let ast = ast.unwrap();
    
        let mut types = types::TypeStore::new();
        let builtins = builtins::make_builtins(&mut types);
        let scopes = scoping::analyze(&ast, builtins);
        let purity = purity::analyze(&ast, &scopes);
        Self {
            ast,
            purity,
        }
    }

    fn assert_impure<T: AstNodeWrap>(&self, node: &T) {
        assert_eq!(self.purity.node_purity(node.as_any()), Some(Purity::Impure));
    }

    fn assert_pure<T: AstNodeWrap>(&self, node: &T) {
        assert_ne!(self.purity.node_purity(node.as_any()), Some(Purity::Impure));
    }
}

#[test]
fn pure_valued_impure_fn() {
    let program = r"
        print!(x) := debug(x)
    ";
    let ctx = TestPurityCtx::analyze(program);

    let decls: Vec<_> = ctx.ast.decls().collect();
    let print = unwrap_node!(decls[0], ExprFnDecl);
    ctx.assert_pure(&print);
    ctx.assert_pure(print.val());
}

#[test]
fn impure_valued_impure_fn() {
    let program = r"
        print!(x) := debug(x)
        print2!(x) := print!(x)
    ";
    let ctx = TestPurityCtx::analyze(program);

    let decls: Vec<_> = ctx.ast.decls().collect();
    let print2 = unwrap_node!(decls[1], ExprFnDecl);
    fndecl_check_name!(print2, "print2");
    ctx.assert_pure(&print2);
    ctx.assert_impure(print2.val());
}

#[test]
#[should_panic]
fn impure_valued_pure_fn() {
    let program = r"
        print!(x) := debug(x)
        print2(x) := print!(x)
    ";
    TestPurityCtx::analyze(program);
}

#[test]
fn pure_global() {
    let program = r#"
        printed := debug("hello")
    "#;
    let ctx = TestPurityCtx::analyze(program);

    let decls: Vec<_> = ctx.ast.decls().collect();
    let printed = unwrap_node!(decls[0], ExprVarDecl);
    ctx.assert_pure(&printed);
}

#[test]
#[should_panic]
fn impure_global() {
    let program = r#"
        print!(x) := debug(x)
        printed := print!("hello")
    "#;
    TestPurityCtx::analyze(program);
}

#[test]
fn impure_closure_capture() {
    let program = r#"
        print!(x) := debug(x)
        main!(x) :=
            let impure_x := print!(x);
            let debug_impure_x!() := debug(impure_x);
            debug_impure_x!()
    "#;
    let ctx = TestPurityCtx::analyze(program);

    let decls: Vec<_> = ctx.ast.decls().collect();
    let main = unwrap_node!(decls[1], ExprFnDecl);
    ctx.assert_pure(&main);

    let main_val = unwrap_node!(main.val(), ExprBinary);
    let impure_x = unwrap_node!(main_val.left(), ExprVarDecl);
    vardecl_check_name!(impure_x, "impure_x");
    ctx.assert_impure(&main_val);
    ctx.assert_impure(&impure_x);

    let main_val = unwrap_node!(main_val.right(), ExprBinary);
    let debug_impure_x = unwrap_node!(main_val.left(), ExprFnDecl);
    fndecl_check_name!(debug_impure_x, "debug_impure_x");
    ctx.assert_impure(&main_val);
    ctx.assert_pure(&debug_impure_x);
    ctx.assert_impure(debug_impure_x.val());

    let main_val = unwrap_node!(main_val.right(), ExprFnCall);
    ctx.assert_impure(&main_val);
}
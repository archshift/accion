use accion::parser;
use accion::ast::{self, AstNodeWrap};
use accion::analysis::{scoping, purity, constexpr};
use accion::builtins;
use accion::types::TypeStore;
use accion::values::Value;

use std::rc::Rc;

mod utils;

struct TestConstEvalCtx {
    ast: Rc<ast::Ast>,
    constexpr: constexpr::ConstVals,
}

impl TestConstEvalCtx {
    fn analyze(program: &str) -> Self {
        let ast = parser::parse(parser::ParseInput::StrBuf(program));
        let ast = ast.unwrap();
    
        let mut types = TypeStore::new();
        let builtins = builtins::make_builtins(&mut types);
        let scopes = scoping::analyze(&ast, builtins);
        let purity = purity::analyze(&ast, &scopes);
        let constexpr = constexpr::analyze(&ast, &mut types, &purity, &scopes);

        Self {
            ast,
            constexpr,
        }
    }
}

#[test]
fn global_var_simple_constexpr() {
    let program = r"
        x := 1 + 1
    ";
    let ctx = TestConstEvalCtx::analyze(program);

    let decls: Vec<_> = ctx.ast.decls().collect();
    let x = unwrap_node!(decls[0], ExprVarDecl);
    vardecl_check_name!(x, "x");

    let x_val = ctx.constexpr.node_val(x.as_any());
    assert_eq!(x_val, Some(Value::Int(2)));
}

#[test]
fn global_var_complex_constexpr() {
    let program = r"
        addone(x) := x + 1
        x := addone(1)
    ";
    let ctx = TestConstEvalCtx::analyze(program);

    let decls: Vec<_> = ctx.ast.decls().collect();
    let addone = unwrap_node!(decls[0], ExprFnDecl);
    let x = unwrap_node!(decls[1], ExprVarDecl);
    
    fndecl_check_name!(addone, "addone");
    vardecl_check_name!(x, "x");

    let addone_val = ctx.constexpr.node_val(addone.as_any());
    assert_eq!(addone_val, Some(Value::Fn(addone)));

    let x_val = ctx.constexpr.node_val(x.as_any());
    assert_eq!(x_val, Some(Value::Int(2)));
}

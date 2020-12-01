use accion::mktype;
use accion::parser;
use accion::ast::{self, AstNodeWrap};
use accion::analysis::{scoping, purity, typing, constexpr};
use accion::builtins;
use accion::types::{Type, TypeStore};

struct TestTyExprCtx {
    ast: &'static ast::Ast,
    typing: typing::Types,
}

impl TestTyExprCtx {
    fn analyze(program: &str) -> Self {
        let ast = parser::parse(parser::ParseInput::StrBuf(program));
        let ast = ast.unwrap();
    
        let mut types = TypeStore::new();
        let builtins = builtins::make_builtins(&mut types);
        let scopes = scoping::analyze(ast, builtins);
        let purity = purity::analyze(ast, &scopes);
        let constexpr = constexpr::analyze(ast, &mut types, &purity, &scopes);
        let typing = typing::analyze(ast, types, &scopes, &constexpr);

        Self {
            ast,
            typing,
        }
    }
}

macro_rules! unwrap_node {
    ($node:expr, $ty:ident) => {{
        if let ast::AstNode::$ty(inner) = $node.as_any() { inner }
        else { panic!("Expected AST node {} but got val {:?}", stringify!($ty), $node) }
    }};
}

macro_rules! fndecl_check_name {
    ($node:expr, $name:expr) => {
        assert_eq!($node.name().unwrap().name(), $name);
    };
}

macro_rules! vardecl_check_name {
    ($node:expr, $name:expr) => {
        assert_eq!($node.name().name(), $name);
    };
}

#[test]
fn global_var_simple_typeexpr() {
    let program = r"
        int ~ Int
        int := 0
    ";
    let mut ctx = TestTyExprCtx::analyze(program);

    let decls: Vec<_> = ctx.ast.decls().collect();
    let int = unwrap_node!(decls[1], ExprVarDecl);
    vardecl_check_name!(int, "int");

    let int_type_id = ctx.typing.node_type(int.node_id()).unwrap();
    assert_eq!(int_type_id, ctx.typing.store.add(Type::Int))
}

#[test]
fn global_fn_simple_typeexpr() {
    let program = r"
        fn ~ Fn(Int -> Int -> nil)
        fn(x) := x
    ";
    let mut ctx = TestTyExprCtx::analyze(program);

    let decls: Vec<_> = ctx.ast.decls().collect();
    let fn_ = unwrap_node!(decls[1], ExprFnDecl);
    fndecl_check_name!(fn_, "fn");

    let fn_type_id = ctx.typing.node_type(fn_.node_id()).unwrap();
    let fn_type_expected = mktype!(ctx.typing.store, (Fn Pure ((Int)) (Int)));
    assert_eq!(fn_type_id, fn_type_expected);
}

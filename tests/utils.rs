#[macro_export]
macro_rules! unwrap_node {
    ($node:expr, $ty:ident) => {{
        if let ast::AstNode::$ty(inner) = $node.as_any() { inner }
        else { panic!("Expected AST node {} but got val {:?}", stringify!($ty), $node) }
    }};
}

#[macro_export]
macro_rules! fndecl_check_name {
    ($node:expr, $name:expr) => {
        assert_eq!($node.name().unwrap().name(), $name);
    };
}

#[macro_export]
macro_rules! vardecl_check_name {
    ($node:expr, $name:expr) => {
        assert_eq!($node.name().name(), $name);
    };
}
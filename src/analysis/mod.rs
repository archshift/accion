pub mod scoping;
pub mod typing;
pub mod constexpr;
pub mod purity;
pub mod backend;

use std::rc::Rc;

use crate::ast;

trait Visitor {
    type Ret;

    fn visit_ast(&mut self, _ast: &Rc<ast::Ast>) -> Self::Ret;

    fn visit_expr(&mut self, node: &Rc<ast::Expr>) -> Self::Ret {
        match node.as_ref() {
            ast::Expr::Binary(e) => self.visit_expr_binary(e),
            ast::Expr::Unary(e) => self.visit_expr_unary(e),
            ast::Expr::FnCall(e) => self.visit_expr_fn_call(e),
            ast::Expr::If(e) => self.visit_expr_if(e),
            ast::Expr::IfCase(e) => self.visit_expr_if_case(e),
            ast::Expr::Entype(e) => self.visit_expr_entype(e),
            ast::Expr::VarDecl(e) => self.visit_expr_var_decl(e),
            ast::Expr::FnDecl(e) => self.visit_expr_fn_decl(e),
            ast::Expr::Literal(e) => self.visit_expr_literal(e),
            ast::Expr::Ident(e) => self.visit_expr_ident(e),
            ast::Expr::Curry(e) => self.visit_expr_curry(e),
        }
    }
    fn visit_expr_binary(&mut self, node: &Rc<ast::ExprBinary>) -> Self::Ret;
    fn visit_expr_unary(&mut self, node: &Rc<ast::ExprUnary>) -> Self::Ret;
    fn visit_expr_fn_call(&mut self, node: &Rc<ast::ExprFnCall>) -> Self::Ret;
    fn visit_expr_if(&mut self, node: &Rc<ast::ExprIf>) -> Self::Ret;
    fn visit_expr_if_case(&mut self, node: &Rc<ast::ExprIfCase>) -> Self::Ret;
    fn visit_expr_entype(&mut self, node: &Rc<ast::ExprEntype>) -> Self::Ret;
    fn visit_expr_var_decl(&mut self, node: &Rc<ast::ExprVarDecl>) -> Self::Ret;
    fn visit_expr_fn_decl(&mut self, node: &Rc<ast::ExprFnDecl>) -> Self::Ret;
    fn visit_expr_literal(&mut self, node: &Rc<ast::ExprLiteral>) -> Self::Ret;
    fn visit_expr_ident(&mut self, node: &Rc<ast::ExprIdent>) -> Self::Ret;
    fn visit_expr_curry(&mut self, node: &Rc<ast::ExprCurry>) -> Self::Ret;

    fn visit_literal(&mut self, node: &Rc<ast::Literal>) -> Self::Ret {
        match node.as_ref() {
            ast::Literal::Int(l) => self.visit_literal_int(l),
            ast::Literal::String(l) => self.visit_literal_str(l),
            ast::Literal::Bool(l) => self.visit_literal_bool(l),
            ast::Literal::Nil(l) => self.visit_literal_nil(l),
        }
    }
    fn visit_literal_str(&mut self, node: &Rc<ast::LiteralString>) -> Self::Ret;
    fn visit_literal_int(&mut self, node: &Rc<ast::LiteralInt>) -> Self::Ret;
    fn visit_literal_bool(&mut self, node: &Rc<ast::LiteralBool>) -> Self::Ret;
    fn visit_literal_nil(&mut self, node: &Rc<ast::LiteralNil>) -> Self::Ret;
    fn visit_ident(&mut self, node: &Rc<ast::Ident>) -> Self::Ret;

    fn visit_node(&mut self, node: &ast::AstNode) -> Self::Ret {
        match node {
            ast::AstNode::Ast(n) => self.visit_ast(&n),
            ast::AstNode::ExprUnary(n) => self.visit_expr_unary(&n),
            ast::AstNode::ExprBinary(n) => self.visit_expr_binary(&n),
            ast::AstNode::ExprIdent(n) => self.visit_expr_ident(&n),
            ast::AstNode::ExprLiteral(n) => self.visit_expr_literal(&n),
            ast::AstNode::ExprIf(n) => self.visit_expr_if(&n),
            ast::AstNode::ExprIfCase(n) => self.visit_expr_if_case(&n),
            ast::AstNode::ExprVarDecl(n) => self.visit_expr_var_decl(&n),
            ast::AstNode::ExprFnDecl(n) => self.visit_expr_fn_decl(&n),
            ast::AstNode::ExprEntype(n) => self.visit_expr_entype(&n),
            ast::AstNode::ExprFnCall(n) => self.visit_expr_fn_call(&n),
            ast::AstNode::ExprCurry(n) => self.visit_expr_curry(&n),
            ast::AstNode::Ident(n) => self.visit_ident(&n),
            ast::AstNode::LiteralInt(n) => self.visit_literal_int(&n),
            ast::AstNode::LiteralString(n) => self.visit_literal_str(&n),
            ast::AstNode::LiteralBool(n) => self.visit_literal_bool(&n),
            ast::AstNode::LiteralNil(n) => self.visit_literal_nil(&n),
        }
    }
}

pub fn preorder(ast: ast::AstNode, mut f: impl FnMut(&ast::AstNode)) {
    use crate::ast::AstNodeWrap;

    let mut upcoming = vec![vec![ast.as_any()]];
    let mut idxs = vec![0usize];

    while let Some(idx) = idxs.last_mut() {
        let children = upcoming.last().unwrap();

        if let Some(child) = children.get(*idx) {
            // Found the child. Add its own children to the stack.
            let mut new_children = Vec::new();
            child.push_children(&mut new_children);

            f(child);
            
            *idx += 1;
            upcoming.push(new_children);
            idxs.push(0);
        } else {
            // End of children. Go up a level
            idxs.pop();
            upcoming.pop();
        }
    }
}

pub fn postorder(node: ast::AstNode, f: &mut impl FnMut(&ast::AstNode)) {
    use crate::ast::AstNodeWrap;
    let mut children = Vec::new();
    node.push_children(&mut children);
    for child in children {
        postorder(child, f);
    }
    f(&node);
}

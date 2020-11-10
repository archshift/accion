pub mod scoping;
pub mod typing;
pub mod constexpr;

use crate::ast;

trait Visitor {
    fn visit_ast(&mut self, ast: &'static ast::Ast) {
        for decl in ast.decls() {
            self.visit_expr(decl);
        }
    }

    fn visit_expr(&mut self, node: ast::Expr) {
        match node {
            ast::Expr::Binary(e) => self.visit_expr_binary(&e),
            ast::Expr::Unary(e) => self.visit_expr_unary(&e),
            ast::Expr::FnCall(e) => self.visit_expr_fn_call(&e),
            ast::Expr::If(e) => self.visit_expr_if(&e),
            ast::Expr::IfCase(e) => self.visit_expr_if_case(&e),
            ast::Expr::Entype(e) => self.visit_expr_entype(&e),
            ast::Expr::VarDecl(e) => self.visit_expr_var_decl(&e),
            ast::Expr::FnDecl(e) => self.visit_expr_fn_decl(&e),
            ast::Expr::Literal(e) => self.visit_expr_literal(&e),
            ast::Expr::Ident(e) => self.visit_expr_ident(&e),
            ast::Expr::Curry(e) => self.visit_expr_curry(&e),
        }
    }
    fn visit_expr_binary(&mut self, node: &'static ast::ExprBinary);
    fn visit_expr_unary(&mut self, node: &'static ast::ExprUnary);
    fn visit_expr_fn_call(&mut self, node: &'static ast::ExprFnCall);
    fn visit_expr_if(&mut self, node: &'static ast::ExprIf);
    fn visit_expr_if_case(&mut self, node: &'static ast::ExprIfCase);
    fn visit_expr_entype(&mut self, node: &'static ast::ExprEntype);
    fn visit_expr_var_decl(&mut self, node: &'static ast::ExprVarDecl);
    fn visit_expr_fn_decl(&mut self, node: &'static ast::ExprFnDecl);
    fn visit_expr_literal(&mut self, node: &'static ast::ExprLiteral);
    fn visit_expr_ident(&mut self, node: &'static ast::ExprIdent);
    fn visit_expr_curry(&mut self, node: &'static ast::ExprCurry);

    fn visit_literal(&mut self, node: ast::Literal) {
        match node {
            ast::Literal::Int(l) => self.visit_literal_int(&l),
            ast::Literal::String(l) => self.visit_literal_str(&l),
            ast::Literal::Bool(l) => self.visit_literal_bool(&l),
            ast::Literal::Nil(l) => self.visit_literal_nil(&l),
        }
    }
    fn visit_literal_str(&mut self, node: &'static ast::LiteralString);
    fn visit_literal_int(&mut self, node: &'static ast::LiteralInt);
    fn visit_literal_bool(&mut self, node: &'static ast::LiteralBool);
    fn visit_literal_nil(&mut self, node: &'static ast::LiteralNil);
    fn visit_ident(&mut self, node: &'static ast::Ident);

    fn visit_node(&mut self, node: &ast::AstNode) {
        match node {
            ast::AstNode::Ast(n) => self.visit_ast(n),
            ast::AstNode::ExprUnary(n) => self.visit_expr_unary(n),
            ast::AstNode::ExprBinary(n) => self.visit_expr_binary(n),
            ast::AstNode::ExprIdent(n) => self.visit_expr_ident(n),
            ast::AstNode::ExprLiteral(n) => self.visit_expr_literal(n),
            ast::AstNode::ExprIf(n) => self.visit_expr_if(n),
            ast::AstNode::ExprIfCase(n) => self.visit_expr_if_case(n),
            ast::AstNode::ExprVarDecl(n) => self.visit_expr_var_decl(n),
            ast::AstNode::ExprFnDecl(n) => self.visit_expr_fn_decl(n),
            ast::AstNode::ExprEntype(n) => self.visit_expr_entype(n),
            ast::AstNode::ExprFnCall(n) => self.visit_expr_fn_call(n),
            ast::AstNode::ExprCurry(n) => self.visit_expr_curry(n),
            ast::AstNode::Ident(n) => self.visit_ident(n),
            ast::AstNode::LiteralInt(n) => self.visit_literal_int(n),
            ast::AstNode::LiteralString(n) => self.visit_literal_str(n),
            ast::AstNode::LiteralBool(n) => self.visit_literal_bool(n),
            ast::AstNode::LiteralNil(n) => self.visit_literal_nil(n),
        }
    }
}

pub fn preorder(ast: &'static ast::Ast, mut f: impl FnMut(&ast::AstNode)) {
    use crate::ast::AstNodeWrap;

    let mut upcoming = vec![vec![ast.as_any()]];
    let mut idxs = vec![0usize];

    while let Some(idx) = idxs.last_mut() {
        let children = upcoming.last().unwrap();

        if let Some(child) = children.get(*idx) {
            // Found the child. Add its own children to the stack.
            let mut new_children = Vec::new();
            child.as_dyn().push_children(&mut new_children);

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

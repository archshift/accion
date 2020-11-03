pub mod scoping;
pub mod typing;

use crate::ast;

trait Visitor {
    fn visit_ast(&mut self, ast: &ast::Ast) {
        for decl in ast.decls() {
            self.visit_expr(&decl);
        }
    }

    fn visit_expr(&mut self, node: &ast::AnyExpr) {
        match node.select() {
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
    fn visit_expr_binary(&mut self, node: &ast::ExprBinary);
    fn visit_expr_unary(&mut self, node: &ast::ExprUnary);
    fn visit_expr_fn_call(&mut self, node: &ast::ExprFnCall);
    fn visit_expr_if(&mut self, node: &ast::ExprIf);
    fn visit_expr_if_case(&mut self, node: &ast::ExprIfCase);
    fn visit_expr_entype(&mut self, node: &ast::ExprEntype);
    fn visit_expr_var_decl(&mut self, node: &ast::ExprVarDecl);
    fn visit_expr_fn_decl(&mut self, node: &ast::ExprFnDecl);
    fn visit_expr_literal(&mut self, node: &ast::ExprLiteral);
    fn visit_expr_ident(&mut self, node: &ast::ExprIdent);
    fn visit_expr_curry(&mut self, node: &ast::ExprCurry);

    fn visit_literal(&mut self, node: &ast::AnyLiteral) {
        match node.select() {
            ast::Literal::Int(l) => self.visit_literal_int(&l),
            ast::Literal::String(l) => self.visit_literal_str(&l),
        }
    }
    fn visit_literal_str(&mut self, node: &ast::LiteralString);
    fn visit_literal_int(&mut self, node: &ast::LiteralInt);
    fn visit_ident(&mut self, node: &ast::Ident);
}

pub fn preorder(ast: ast::Ast, mut f: impl FnMut(&ast::AstNode)) {
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
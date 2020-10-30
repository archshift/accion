use std::collections::HashMap;

use crate::ast::{self, AstNode};
use dynstack::{DynStack, dyn_push};
use smallvec::SmallVec;

#[derive(Copy, Clone, Hash, PartialEq, Eq, Debug)]
pub enum DeclKind {
    Type,
    Val
}

#[derive(Debug)]
pub struct Declaration {
    name: String,
    kind: DeclKind,
    scope: ScopeId,
}

type Scope = HashMap<(String, DeclKind), Declaration>;
type ScopeId = ast::AstNodeId;
type Scopes = HashMap<ScopeId, Scope>;

type DeclsOut = SmallVec<[Declaration; 1]>;

pub struct NodeStack {
    scope_ids: Vec<ScopeId>,
    nodes: DynStack<dyn Scoping>
}
impl NodeStack {
    fn new() -> Self {
        Self {
            scope_ids: Vec::new(),
            nodes: DynStack::new()
        }
    }
    
    fn len(&self) -> usize {
        self.scope_ids.len()
    }

    fn push<N: 'static + Scoping>(&mut self, scope: ScopeId, node: N) {
        let node: N = node;
        self.scope_ids.push(scope);
        dyn_push!(self.nodes, node);
    }

    fn peek(&self) -> Option<(ScopeId, &dyn Scoping)> {
        if self.len() != 0 {
            self.scope_ids.last().copied()
                .zip(self.nodes.peek())
        } else {
            None
        }
    }

    fn remove_last(&mut self) {
        self.scope_ids.pop();
        self.nodes.remove_last();
    }

    fn move_from(&mut self, other: &mut Self) {
        let scope_node_iter =
            other.scope_ids.drain(..)
                .zip(other.nodes.iter_mut());

        for (scope, node) in scope_node_iter {
            self.scope_ids.push(scope);
            unsafe {
                self.nodes.push(node);
            }
        }
        while other.nodes.forget_last() {}
    }
}

pub fn analyze<S: 'static + Scoping>(root: S) -> Scopes {
    let mut scopes = Scopes::new();
    let mut s = NodeStack::new();
    let mut next_s = NodeStack::new();
    
    s.push(root.node_id(), root);

    while let Some((scope, node)) = s.peek() {
        node.analyze_scope(scope, &mut next_s);

        for decl in node.declaration(scope) {
            let scope = scopes.entry(decl.scope)
                .or_insert_with(Scope::new);
            let decl_key = (decl.name.clone(), decl.kind);
            if scope.contains_key(&decl_key) {
                panic!("Cannot reassign ident in the same scope! Previous decl: {:?}; new decl: {:?}", scope[&decl_key], &decl);
            }
            scope.insert(decl_key, decl);
        }

        s.remove_last();
        s.move_from(&mut next_s);
    }
    scopes
}

pub trait Scoping : ast::AstNode {
    fn analyze_scope(&self, scope: ScopeId, next_nodes: &mut NodeStack);
    fn declaration(&self, _scope: ScopeId) -> DeclsOut {
        DeclsOut::new()
    }
}

impl Scoping for ast::Ast {
    fn analyze_scope(&self, scope: ScopeId, next_nodes: &mut NodeStack) {
        for decl in self.decls() {
            next_nodes.push(scope, decl);
        }
    }
}

impl Scoping for ast::AnyExpr {
    fn analyze_scope(&self, scope: ScopeId, next_nodes: &mut NodeStack) {
        match self.select() {
            ast::Expr::Binary(e) => e.analyze_scope(scope, next_nodes),
            ast::Expr::Unary(e) => e.analyze_scope(scope, next_nodes),
            ast::Expr::FnCall(e) => e.analyze_scope(scope, next_nodes),
            ast::Expr::If(e) => e.analyze_scope(scope, next_nodes),
            ast::Expr::IfCase(e) => e.analyze_scope(scope, next_nodes),
            ast::Expr::Entype(e) => e.analyze_scope(scope, next_nodes),
            ast::Expr::VarDecl(e) => e.analyze_scope(scope, next_nodes),
            ast::Expr::FnDecl(e) => e.analyze_scope(scope, next_nodes),
            ast::Expr::Literal(_) => {},
            ast::Expr::Ident(_) => {},
            ast::Expr::Curry(_) => {},
        }
    }
    fn declaration(&self, scope: ScopeId) -> DeclsOut {
        match self.select() {
            ast::Expr::Entype(e) => e.declaration(scope),
            ast::Expr::VarDecl(e) => e.declaration(scope),
            ast::Expr::FnDecl(e) => e.declaration(scope),
            ast::Expr::Binary(_)
            | ast::Expr::Unary(_)
            | ast::Expr::FnCall(_)
            | ast::Expr::If(_)
            | ast::Expr::IfCase(_)
            | ast::Expr::Literal(_)
            | ast::Expr::Ident(_)
            | ast::Expr::Curry(_)
            => DeclsOut::new()
        }
    }
}

impl Scoping for ast::ExprBinary {
    fn analyze_scope(&self, scope: ScopeId, next_nodes: &mut NodeStack) {
        let (op1, op2) = self.operands();
        next_nodes.push(scope, op1);
        next_nodes.push(scope, op2);
    }
}
impl Scoping for ast::ExprUnary {
    fn analyze_scope(&self, scope: ScopeId, next_nodes: &mut NodeStack) {
        next_nodes.push(scope, self.operand());
    }
}
impl Scoping for ast::ExprFnCall {
    fn analyze_scope(&self, scope: ScopeId, next_nodes: &mut NodeStack) {
        next_nodes.push(scope, self.callee());
        for arg in self.args() {
            next_nodes.push(scope, arg);
        }
    }
}
impl Scoping for ast::ExprIf {
    fn analyze_scope(&self, scope: ScopeId, next_nodes: &mut NodeStack) {
        let then_expr = self.then_expr();
        let else_expr = self.else_expr();
        next_nodes.push(scope, self.cond());
        next_nodes.push(then_expr.node_id(), then_expr);
        next_nodes.push(else_expr.node_id(), else_expr);
    }
}
impl Scoping for ast::ExprIfCase {
    fn analyze_scope(&self, scope: ScopeId, next_nodes: &mut NodeStack) {
        next_nodes.push(scope, self.cond());
        for case in self.cases() {
            match case {
                | ast::IfCase::OnVal(_, val)
                | ast::IfCase::Else(val)
                => next_nodes.push(val.node_id(), val)
            }
        }
    }
}
impl Scoping for ast::ExprEntype {
    fn analyze_scope(&self, scope: ScopeId, next_nodes: &mut NodeStack) {
        next_nodes.push(scope, self.ty());
    }
    fn declaration(&self, scope: ScopeId) -> DeclsOut {
        let mut out = DeclsOut::new();
        out.push(Declaration {
            name: self.target().name().to_owned(),
            kind: DeclKind::Type,
            scope
        });
        out
    }
}
impl Scoping for ast::ExprVarDecl {
    fn analyze_scope(&self, scope: ScopeId, next_nodes: &mut NodeStack) {
        next_nodes.push(scope, self.val());
    }
    fn declaration(&self, scope: ScopeId) -> DeclsOut {
        let mut out = DeclsOut::new();
        out.push(Declaration {
            name: self.name().name().to_owned(),
            kind: DeclKind::Val,
            scope
        });
        out
    }
}
impl Scoping for ast::ExprFnDecl {
    fn analyze_scope(&self, scope: ScopeId, next_nodes: &mut NodeStack) {
        next_nodes.push(scope, self.val());
    }
    fn declaration(&self, scope: ScopeId) -> DeclsOut {
        let mut out = DeclsOut::new();

        if let Some(name) = self.name() {
            out.push(Declaration {
                name: name.name().to_owned(),
                kind: DeclKind::Val,
                scope
            });
        }

        let body_scope = self.val().node_id();
        for arg in self.args() {
            out.push(Declaration {
                name: arg.name().to_owned(),
                kind: DeclKind::Val,
                scope: body_scope
            });
        }
        out
    }
}

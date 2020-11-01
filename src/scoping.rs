use std::collections::HashMap;

use crate::ast::{self, AstNode};
use dynstack::{DynStack, dyn_push};
use smallvec::SmallVec;
use derive_newtype::NewType;

#[derive(Debug)]
pub struct Declaration {
    name: String,
    scope: ScopeId,
    pub node_id: ast::AstNodeId,
    pub impure_fn: bool,
}

#[derive(Debug)]
pub struct Scope {
    decls: HashMap<String, Declaration>,
    parent: ScopeId
}

impl Scope {
    fn new(parent: ScopeId) -> Self {
        Self {
            decls: HashMap::new(),
            parent
        }
    }

    pub fn resolve<'a>(&'a self, name: &str) -> Option<&'a Declaration> {
        self.decls.get(name)
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub struct ScopeId(usize);
pub type NodeScopes = HashMap<ast::AstNodeId, ScopeId>;
type DeclsOut = SmallVec<[Declaration; 1]>;

#[derive(NewType)]
struct NodeStack(DynStack<dyn Scoping>);
impl NodeStack {
    fn new() -> Self {
        Self(DynStack::new())
    }
    
    fn push<N: 'static + Scoping>(&mut self, node: N) {
        let node: N = node;
        dyn_push!(**self, node);
    }

    fn move_from(&mut self, other: &mut Self) {
        for node in other.iter_mut() {
            unsafe {
                (**self).push(node);
            }
        }
        while other.forget_last() {}
    }
}

struct ScopingCtx {
    scopes: Vec<Scope>,
    node_scopes: NodeScopes,
    added_nodes: NodeStack,
    scope: ScopeId,
}

impl ScopingCtx {
    fn new_scope(&mut self, parent: ScopeId) -> ScopeId {
        self.scopes.push(Scope::new(parent));
        ScopeId(self.scopes.len()-1)
    }
    fn scope_and_queue<N: 'static + Scoping>(&mut self, scope: ScopeId, node: N) {
        let old = self.node_scopes.insert(node.node_id(), scope);
        assert!(old.is_none());
        self.added_nodes.push(node);
    }
    fn node_scope_id(&self, node: ast::AstNodeId) -> Option<ScopeId> {
        self.node_scopes.get(&node).copied()
    }
}

#[derive(Debug)]
pub struct Scopes {
    scopes: Vec<Scope>,
    node_scopes: NodeScopes,
    pub node_count: usize,
}

impl Scopes {
    pub fn node_scope(&self, node: ast::AstNodeId) -> Option<&Scope> {
        self.node_scopes.get(&node)
            .map(|id| &self.scopes[id.0])
    }

    pub fn resolve<'a>(&'a self, mut scope: &'a Scope, name: &str) -> Option<&'a Declaration> {
        while {
            let decl = scope.decls.get(name);
            if decl.is_some() {
                return decl
            }
            scope.parent.0 != !0
        } { scope = &self.scopes[scope.parent.0] }
        
        None
    }
}

pub fn analyze(root: &ast::Ast) -> Scopes {
    let mut ctx = ScopingCtx {
        scopes: Vec::new(),
        node_scopes: NodeScopes::new(),
        added_nodes: NodeStack::new(),
        scope: ScopeId(!0)
    };

    let mut s = NodeStack::new();
    let mut count = 0;

    ctx.scope = ctx.new_scope(ctx.scope);
    ctx.scope_and_queue(ctx.scope, root.clone());
    s.move_from(&mut ctx.added_nodes);

    while let Some(node) = s.peek() {
        ctx.scope = ctx.node_scope_id(node.node_id()).unwrap();
        node.analyze_scope(&mut ctx);
        count += 1;
        
        for decl in node.declaration(&mut ctx) {
            let scope = &mut ctx.scopes[decl.scope.0];
            if scope.decls.contains_key(&decl.name) {
                panic!("Cannot reassign ident in the same scope! Previous decl: {:?}; new decl: {:?}",
                scope.decls[&decl.name], &decl);
            }
            scope.decls.insert(decl.name.clone(), decl);
        }
        
        s.remove_last();
        s.move_from(&mut ctx.added_nodes);
    }
    
    Scopes {
        scopes: ctx.scopes,
        node_scopes: ctx.node_scopes,
        node_count: count, // TODO: this doesn't really belong here
    }
}

trait Scoping : ast::AstNode {
    fn analyze_scope(&self, ctx: &mut ScopingCtx);
    fn declaration(&self, _ctx: &mut ScopingCtx) -> DeclsOut {
        DeclsOut::new()
    }
}

impl Scoping for ast::Ast {
    fn analyze_scope(&self, ctx: &mut ScopingCtx) {
        for decl in self.decls() {
            ctx.scope_and_queue(ctx.scope, decl);
        }
    }
}

impl Scoping for ast::AnyExpr {
    fn analyze_scope(&self, ctx: &mut ScopingCtx) {
        match self.select() {
            ast::Expr::Binary(e) => e.analyze_scope(ctx),
            ast::Expr::Unary(e) => e.analyze_scope(ctx),
            ast::Expr::FnCall(e) => e.analyze_scope(ctx),
            ast::Expr::If(e) => e.analyze_scope(ctx),
            ast::Expr::IfCase(e) => e.analyze_scope(ctx),
            ast::Expr::Entype(e) => e.analyze_scope(ctx),
            ast::Expr::VarDecl(e) => e.analyze_scope(ctx),
            ast::Expr::FnDecl(e) => e.analyze_scope(ctx),
            ast::Expr::Literal(_) => {},
            ast::Expr::Ident(_) => {},
            ast::Expr::Curry(_) => {},
        }
    }
    fn declaration(&self, ctx: &mut ScopingCtx) -> DeclsOut {
        match self.select() {
            ast::Expr::Entype(e) => e.declaration(ctx),
            ast::Expr::VarDecl(e) => e.declaration(ctx),
            ast::Expr::FnDecl(e) => e.declaration(ctx),
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
    fn analyze_scope(&self, ctx: &mut ScopingCtx) {
        let (op1, op2) = self.operands();
        ctx.scope_and_queue(ctx.scope, op1);
        ctx.scope_and_queue(ctx.scope, op2);
    }
}
impl Scoping for ast::ExprUnary {
    fn analyze_scope(&self, ctx: &mut ScopingCtx) {
        ctx.scope_and_queue(ctx.scope, self.operand());
    }
}
impl Scoping for ast::ExprFnCall {
    fn analyze_scope(&self, ctx: &mut ScopingCtx) {
        ctx.scope_and_queue(ctx.scope, self.callee());
        for arg in self.args() {
            ctx.scope_and_queue(ctx.scope, arg);
        }
    }
}
impl Scoping for ast::ExprIf {
    fn analyze_scope(&self, ctx: &mut ScopingCtx) {
        let then_scope = ctx.new_scope(ctx.scope);
        let else_scope = ctx.new_scope(ctx.scope);
        ctx.scope_and_queue(ctx.scope, self.cond());
        ctx.scope_and_queue(then_scope, self.then_expr());
        ctx.scope_and_queue(else_scope, self.else_expr());
    }
}
impl Scoping for ast::ExprIfCase {
    fn analyze_scope(&self, ctx: &mut ScopingCtx) {
        ctx.scope_and_queue(ctx.scope, self.cond());
        let new_scope = ctx.new_scope(ctx.scope);
        for case in self.cases() {
            match case {
                | ast::IfCase::OnVal(_, val)
                | ast::IfCase::Else(val)
                => ctx.scope_and_queue(new_scope, val)
            }
        }
    }
}
impl Scoping for ast::ExprEntype {
    fn analyze_scope(&self, ctx: &mut ScopingCtx) {
        ctx.scope_and_queue(ctx.scope, self.ty());
    }
}
impl Scoping for ast::ExprVarDecl {
    fn analyze_scope(&self, ctx: &mut ScopingCtx) {
        ctx.scope_and_queue(ctx.scope, self.val());
    }
    fn declaration(&self, ctx: &mut ScopingCtx) -> DeclsOut {
        let mut out = DeclsOut::new();
        out.push(Declaration {
            name: self.name().name().to_owned(),
            scope: ctx.scope,
            node_id: self.node_id(),
            impure_fn: false,
        });
        out
    }
}
impl Scoping for ast::ExprFnDecl {
    fn analyze_scope(&self, ctx: &mut ScopingCtx) {
        let body_scope = ctx.new_scope(ctx.scope);
        ctx.scope_and_queue(body_scope, self.val());
    }
    fn declaration(&self, ctx: &mut ScopingCtx) -> DeclsOut {
        let mut out = DeclsOut::new();
        
        if let Some(name) = self.name() {
            out.push(Declaration {
                name: name.name().to_owned(),
                scope: ctx.scope,
                node_id: name.node_id(),
                impure_fn: self.pure(),
            });
        }
        
        let body_scope = ctx.node_scope_id(self.val().node_id()).unwrap();
        for arg in self.args() {
            out.push(Declaration {
                name: arg.name().to_owned(),
                scope: body_scope,
                node_id: self.node_id(),
                impure_fn: false,
            });
        }
        out
    }
}

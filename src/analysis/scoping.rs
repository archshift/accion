use std::collections::HashMap;
use std::fmt;

use crate::analysis::{preorder, Visitor};
use crate::ast::{self, AstNodeWrap};
use crate::builtins::BuiltinMap;

#[derive(Debug)]
pub struct FwdDeclaration {
    pub name: String,
    pub scope: ScopeId,
    pub entype: &'static ast::ExprEntype,
}

#[derive(Debug)]
pub struct Definition {
    pub name: String,
    pub scope: ScopeId,
    pub node: ast::AstNode,
    pub entype: Option<&'static ast::ExprEntype>,
    pub impure_fn: bool,
}

#[derive(Debug)]
enum Declaration {
    FwdDecl(FwdDeclaration),
    Defn(Definition),
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

    pub fn resolve<'a>(&'a self, name: &str) -> Option<&'a Definition> {
        if let Some(Declaration::Defn(decl)) = self.decls.get(name) {
            Some(decl)
        } else {
            None
        }
    }
}

#[derive(Eq, PartialEq, Hash, Copy, Clone)]
pub struct ScopeId(usize);
impl fmt::Debug for ScopeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ScopeId({})", self.0)
    }
}

pub type NodeScopes = HashMap<ast::AstNodeId, ScopeId>;

struct ScopingCtx {
    scopes: Vec<Scope>,
    node_scopes: NodeScopes,
    scope: ScopeId,
    builtins: BuiltinMap,
}

impl ScopingCtx {
    fn new_scope(&mut self, parent: ScopeId) -> ScopeId {
        self.scopes.push(Scope::new(parent));
        ScopeId(self.scopes.len()-1)
    }
    fn set_scope(&mut self, node: ast::AstNodeId, scope: ScopeId) {
        let old = self.node_scopes.insert(node, scope);
        assert!(old.is_none());
    }
    fn node_scope_id(&self, node: ast::AstNodeId) -> Option<ScopeId> {
        self.node_scopes.get(&node).copied()
    }
    fn fwd_decl(&mut self, decl: FwdDeclaration) {
        let scope = &mut self.scopes[decl.scope.0];
        if self.builtins.contains_key(&decl.name) {
            panic!("Cannot shadow builtin function with decl {:?}!", decl.name);
        }
        match scope.decls.get(&decl.name) {
            None =>
                scope.decls.insert(decl.name.clone(), Declaration::FwdDecl(decl)),
            Some(Declaration::FwdDecl(old)) =>
                panic!("Cannot redeclare ident in the same scope! Previous decl: {:?}; new decl: {:?}",
                    old, &decl),
            Some(Declaration::Defn(old)) =>
                panic!("Cannot redeclare ident in the same scope! Previous decl: {:?}; new decl: {:?}",
                    old, &decl),
        };
    }
    fn add_defn(&mut self, decl: Definition) {
        let scope = &mut self.scopes[decl.scope.0];
        if self.builtins.contains_key(&decl.name) {
            panic!("Cannot shadow builtin function with decl {:?}!", decl.name);
        }
        match scope.decls.remove(&decl.name) {
            None =>
                scope.decls.insert(decl.name.clone(), Declaration::Defn(decl)),
            Some(Declaration::FwdDecl(old)) =>
                scope.decls.insert(decl.name.clone(), Declaration::Defn(
                    Definition {
                        entype: Some(old.entype),
                        ..decl
                    }
                )),
            Some(Declaration::Defn(old)) =>
                panic!("Cannot reassign ident in the same scope! Previous decl: {:?}; new decl: {:?}",
                    old, &decl),
        };
    }
}

#[derive(Debug)]
pub struct Scopes {
    scopes: Vec<Scope>,
    node_scopes: NodeScopes,
    pub builtins: BuiltinMap,
}

impl Scopes {
    pub fn node_scope(&self, node: ast::AstNodeId) -> Option<ScopeId> {
        self.node_scopes.get(&node).copied()
    }

    pub fn resolve<'a>(&'a self, mut scope_id: ScopeId, name: &str) -> Option<&'a Definition> {
        while scope_id.0 != !0 {
            let scope = &self.scopes[scope_id.0];
            let decl = scope.resolve(name);
            if decl.is_some() {
                return decl
            }
            scope_id = scope.parent;
        }
        None
    }
}

pub fn analyze(root: &'static ast::Ast, builtins: BuiltinMap) -> Scopes {
    let mut ctx = ScopingCtx {
        scopes: Vec::new(),
        node_scopes: NodeScopes::new(),
        scope: ScopeId(!0),
        builtins,
    };

    let global_scope = ctx.new_scope(ctx.scope);
    ctx.set_scope(root.node_id(), global_scope);

    preorder(root, |node| {
        let node_id = node.node_id();
        if let Some(scope) = ctx.node_scope_id(node_id) {
            ctx.scope = scope;
            ctx.visit_node(node);
        }
    });

    Scopes {
        scopes: ctx.scopes,
        node_scopes: ctx.node_scopes,
        builtins: ctx.builtins,
    }
}

impl Visitor for ScopingCtx {
    fn visit_ast(&mut self, node: &ast::Ast) {
        for decl in node.decls() {
            self.set_scope(decl.node_id(), self.scope);
        }
    }
    fn visit_expr_binary(&mut self, node: &ast::ExprBinary) {
        let (op1, op2) = node.operands();
        self.set_scope(op1.node_id(), self.scope);
        self.set_scope(op2.node_id(), self.scope);
    }
    fn visit_expr_unary(&mut self, node: &ast::ExprUnary) {
        self.set_scope(node.operand().node_id(), self.scope);
    }
    fn visit_expr_fn_call(&mut self, node: &ast::ExprFnCall) {
        self.set_scope(node.callee().node_id(), self.scope);
        for arg in node.args() {
            self.set_scope(arg.node_id(), self.scope);
        }
    }
    fn visit_expr_if(&mut self, node: &ast::ExprIf) {
        let then_scope = self.new_scope(self.scope);
        let else_scope = self.new_scope(self.scope);
        self.set_scope(node.cond().node_id(), self.scope);
        self.set_scope(node.then_expr().node_id(), then_scope);
        self.set_scope(node.else_expr().node_id(), else_scope);
    }
    fn visit_expr_if_case(&mut self, node: &ast::ExprIfCase) {
        self.set_scope(node.cond().node_id(), self.scope);
        let new_scope = self.new_scope(self.scope);
        for case in node.cases() {
            match case {
                | ast::IfCase::OnVal(_, val)
                | ast::IfCase::Else(val)
                => self.set_scope(val.node_id(), new_scope)
            }
        }
    }
    fn visit_expr_entype(&mut self, node: &'static ast::ExprEntype) {
        self.set_scope(node.target().node_id(), self.scope);
        self.set_scope(node.ty().node_id(), self.scope);
        self.fwd_decl(FwdDeclaration {
            name: node.target().name().to_owned(),
            scope: self.scope,
            entype: node
        });
    }
    fn visit_expr_var_decl(&mut self, node: &'static ast::ExprVarDecl) {
        self.set_scope(node.val().node_id(), self.scope);
        self.add_defn(Definition {
            name: node.name().name().to_owned(),
            scope: self.scope,
            node: node.as_any(),
            entype: None,
            impure_fn: false,
        });
    }
    fn visit_expr_fn_decl(&mut self, node: &'static ast::ExprFnDecl) {
        let body_scope = self.new_scope(self.scope);
        self.set_scope(node.val().node_id(), body_scope);
        
        if let Some(name) = node.name() {
            self.add_defn(Definition {
                name: name.name().to_owned(),
                scope: self.scope,
                node: node.as_any(),
                entype: None,
                impure_fn: !node.pure(),
            });
        }
        for arg in node.args() {
            self.add_defn(Definition {
                name: arg.name().to_owned(),
                scope: body_scope,
                node: arg.as_any(),
                entype: None,
                impure_fn: false,
            });
        }
    }

    fn visit_expr_literal(&mut self, _: &ast::ExprLiteral) {}
    fn visit_expr_curry(&mut self, _: &ast::ExprCurry) {}
    fn visit_expr_ident(&mut self, _: &ast::ExprIdent) {}
    fn visit_literal_int(&mut self, _: &ast::LiteralInt) {}
    fn visit_literal_str(&mut self, _: &ast::LiteralString) {}
    fn visit_literal_bool(&mut self, _: &ast::LiteralBool) {}
    fn visit_literal_nil(&mut self, _: &ast::LiteralNil) {}
    fn visit_ident(&mut self, _: &ast::Ident) {}
}

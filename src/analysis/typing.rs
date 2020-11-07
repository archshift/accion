use std::collections::HashMap;

use crate::ast::{self, AstNodeWrap};
use crate::analysis::{preorder, Visitor, scoping::Scopes};
use crate::disjoint_set::DisjointSet;
use crate::types::*;
use smallvec::SmallVec;

type NodeTypes = HashMap<ast::AstNodeId, TypeId>;
type TypesEq = DisjointSet<TypeId>;

struct TypeAnalysis<'a> {
    scopes: &'a Scopes,
    types: TypeStore,
    eq_types: TypesEq,
    node_types: NodeTypes,
}

impl<'a> TypeAnalysis<'a> {
    fn type_node(&mut self, node_id: ast::AstNodeId, ty: TypeId) {
        if let Some(old) = self.node_types.insert(node_id, ty) {
            self.eq_types.join(&ty, &old);
        }
    }
    fn equate_nodes(&mut self, nodes: &[ast::AstNodeId]) {
        let type_var = self.types.add_typevar();
        for node in nodes {
            self.type_node(*node, type_var);
        }
    }
}

#[derive(Debug)]
pub struct Types {
    pub store: TypeStore,
    node_types: NodeTypes,
}
impl Types {
    pub fn node_type(&self, node: ast::AstNodeId) -> Option<TypeId> {
        if let Some(mut id) = self.node_types.get(&node).copied() {
            let mut ty;
            while let BaseType::TypeVarResolved(_, inner) = { 
                ty = self.store.query(id);
                &ty.base
            } {
                id = *inner;
            }
            Some(id)
        } else {
            None
        }
    }
}

fn reduce_types(ctx: &mut TypeAnalysis) -> Result<(), TypeCollateError> {
    let groups = ctx.eq_types.groups();
    for group in groups {
        let mut it = group.iter().copied();
        let first = it.next().unwrap();
        it.try_fold(first,
            |acc, new| ctx.types.collate_types(acc, new)
        )?;
    }
    Ok(())
}

pub fn analyze(root: &'static ast::Ast, types: TypeStore, scopes: &Scopes) -> Types {
    let mut ctx = TypeAnalysis {
        scopes,
        types,
        eq_types: TypesEq::new(1000),
        node_types: NodeTypes::new(),
    };
    
    preorder(root, |node| {
        ctx.visit_node(node);
    });

    if let Err(e) = reduce_types(&mut ctx) {
        e.print(&ctx.types);
        panic!("Type resolution error!");
    }

    Types {
        store: ctx.types,
        node_types: ctx.node_types,
    }
}

const ANY_BOOL: Type = Type::new(BaseType::Bool);
const ANY_INT: Type = Type::new(BaseType::Int);
const ANY_STRING: Type = Type::new(BaseType::String);
const ANY_CURRY: Type = Type::new(BaseType::Curry);
const PURE_META: Type = Type { base: BaseType::Type, purity: Purity::Pure };

trait Typing : ast::AstNodeWrap {
    fn analyze_type(&self, ctx: &mut TypeAnalysis);
}

impl Visitor for TypeAnalysis<'_> {
    fn visit_expr_entype(&mut self, node: &'static ast::ExprEntype) {
        let meta_id = self.types.add(PURE_META);
        let target = node.target();
        let ty = node.ty();

        let name = target.name();
        let scope = self.scopes.node_scope(node.node_id()).unwrap();
        let decl = scope.resolve(name);
        if decl.is_none() {
            panic!("Error: could not find ident `{}` in scope", name);
        }
        let decl = decl.unwrap();

        self.type_node(node.node_id(), meta_id);
        self.type_node(ty.node_id(), meta_id);

        let type_expr = Type { base: BaseType::TypeExpr(ty.node_id()), purity: Purity::Pure };
        let type_expr_id = self.types.add(type_expr);
        self.type_node(decl.node_id, type_expr_id);
    }

    fn visit_expr_literal(&mut self, node: &'static ast::ExprLiteral) {
        let lit = node.literal();
        self.equate_nodes(&[node.node_id(), lit.node_id()]);
    }

    fn visit_expr_ident(&mut self, node: &'static ast::ExprIdent) {
        let ident = node.ident();
        let name = ident.name();
        
        if let Some(builtin) = self.scopes.builtins.get(name) {
            self.type_node(node.node_id(), builtin.ty);
            return;
        }
        
        let scope = self.scopes.node_scope(node.node_id()).unwrap();
        
        let decl = self.scopes.resolve(scope, name);
        if decl.is_none() {
            panic!("Error: referenced unknown ident `{}`", name);
        }
        let decl = decl.unwrap();
        
        self.equate_nodes(&[node.node_id(), decl.node_id]);
    }

    fn visit_expr_if(&mut self, node: &'static ast::ExprIf) {
        let bool_id = self.types.add(ANY_BOOL);

        let cond = node.cond();
        // Pre-select a type for the cond expression
        self.type_node(cond.node_id(), bool_id);

        let then_expr = node.then_expr();
        let else_expr = node.else_expr();
        self.equate_nodes(&[node.node_id(), then_expr.node_id(), else_expr.node_id()]);
    }

    fn visit_expr_if_case(&mut self, node: &'static ast::ExprIfCase) {
        let cond = node.cond();
        let mut cond_ty_nodes = SmallVec::<[ast::AstNodeId; 4]>::new();
        let mut val_ty_nodes = SmallVec::<[ast::AstNodeId; 4]>::new();

        cond_ty_nodes.push(cond.node_id());
        val_ty_nodes.push(node.node_id());
        
        for case in node.cases() {
            match case {
                ast::IfCase::OnVal(lit, val) => {
                    cond_ty_nodes.push(lit.node_id());
                    val_ty_nodes.push(val.node_id());
                }
                ast::IfCase::Else(val) => {
                    val_ty_nodes.push(val.node_id());
                }
            }
        }
        
        self.equate_nodes(&cond_ty_nodes);
        self.equate_nodes(&val_ty_nodes);
    }

    fn visit_expr_var_decl(&mut self, node: &'static ast::ExprVarDecl) {
        let val = node.val();
        self.equate_nodes(&[node.node_id(), val.node_id()]);
    }

    fn visit_expr_fn_decl(&mut self, node: &'static ast::ExprFnDecl) {
        let name = node.name();
        let val = node.val();

        let mut arg_types = FnArgTypes::new();
        for arg in node.args() {
            let type_var = self.types.add_typevar();
            self.type_node(arg.node_id(), type_var);
            arg_types.push(type_var);            
        }
        let ret_type = self.types.add_typevar();
        self.type_node(val.node_id(), ret_type);
        
        let fn_type = Type {
            base: BaseType::Fn(arg_types, ret_type),
            purity: if node.pure() { Purity::Pure } else { Purity::Impure }
        };
        let fn_type_id = self.types.add(fn_type);
        self.type_node(node.node_id(), fn_type_id);
        if let Some(name) = name {
            self.type_node(name.node_id(), fn_type_id);
        }
    }

    fn visit_expr_curry(&mut self, node: &'static ast::ExprCurry) {
        let curry_id = self.types.add(ANY_CURRY);
        self.type_node(node.node_id(), curry_id);
    }

    fn visit_expr_unary(&mut self, node: &'static ast::ExprUnary) {
        let inner = node.operand();
        match node.operator() {
            | ast::UnaryOp::Negate => {
                let int_id = self.types.add(ANY_INT);
                self.type_node(node.node_id(), int_id);
                self.type_node(inner.node_id(), int_id);
            }
            | ast::UnaryOp::Head => {
                let type_var = self.types.add_typevar();
                let type_var_list = Type::new(BaseType::List(type_var));
                let type_var_list_id = self.types.add(type_var_list);
                self.type_node(node.node_id(), type_var);
                self.type_node(inner.node_id(), type_var_list_id);
            }
            | ast::UnaryOp::Tail => {
                let type_var = self.types.add_typevar();
                let type_var_list = Type::new(BaseType::List(type_var));
                let type_var_list_id = self.types.add(type_var_list);
                self.type_node(node.node_id(), type_var_list_id);
                self.type_node(inner.node_id(), type_var_list_id);
            }
        }
    }

    fn visit_expr_binary(&mut self, node: &'static ast::ExprBinary) {
        let (left, right) = node.operands();
        match node.operator() {
            | ast::BinaryOp::Add
            | ast::BinaryOp::Sub
            | ast::BinaryOp::Mul
            | ast::BinaryOp::Div
            | ast::BinaryOp::Mod
            => {
                let int_id = self.types.add(ANY_INT);
                self.type_node(node.node_id(), int_id);
                self.type_node(left.node_id(), int_id);
                self.type_node(right.node_id(), int_id);
            }
            | ast::BinaryOp::Prepend => {
                let type_var = self.types.add_typevar();
                let type_var_list = Type::new(BaseType::List(type_var));
                let type_var_list_id = self.types.add(type_var_list);
                self.type_node(left.node_id(), type_var);
                self.type_node(right.node_id(), type_var_list_id);
            }
            | ast::BinaryOp::Eq
            => {
                let bool_id = self.types.add(ANY_BOOL);
                self.type_node(node.node_id(), bool_id);
                self.equate_nodes(&[left.node_id(), right.node_id()]);
            }
            | ast::BinaryOp::LastUnit =>
                self.equate_nodes(&[node.node_id(), right.node_id()])
        }
    }

    fn visit_expr_fn_call(&mut self, node: &ast::ExprFnCall) {
        let callee = node.callee();
        
        let mut arg_types = FnArgTypes::new();
        for arg in node.args() {
            let type_var = self.types.add_typevar();
            self.type_node(arg.node_id(), type_var);
            arg_types.push(type_var);
            
        }
        let ret_type = self.types.add_typevar();

        let fn_type = Type {
            base: BaseType::Fn(arg_types, ret_type),
            purity: if node.pure() { Purity::Pure } else { Purity::Impure }
        };
        let fn_type_id = self.types.add(fn_type);
        self.type_node(callee.node_id(), fn_type_id);
    }

    fn visit_literal_int(&mut self, node: &'static ast::LiteralInt) {
        let ty_id = self.types.add(ANY_INT);
        self.type_node(node.node_id(), ty_id);
    }

    fn visit_literal_str(&mut self, node: &'static ast::LiteralString) {
        let ty_id = self.types.add(ANY_STRING);
        self.type_node(node.node_id(), ty_id);
    }

    fn visit_literal_bool(&mut self, node: &'static ast::LiteralBool) {
        let ty_id = self.types.add(ANY_BOOL);
        self.type_node(node.node_id(), ty_id);
    }

    fn visit_literal_nil(&mut self, node: &'static ast::LiteralNil) {
        let type_var = self.types.add_typevar();
        let generic_list = Type::new(BaseType::List(type_var));
        let generic_list_id = self.types.add(generic_list);
        self.type_node(node.node_id(), generic_list_id);
    }

    fn visit_ident(&mut self, _: &ast::Ident) { }
}
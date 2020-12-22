use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{self, AstNodeWrap};
use crate::analysis::{preorder, Visitor, scoping::Scopes, constexpr::ConstVals};
use crate::disjoint_set::DisjointSet;
use crate::types::*;
use smallvec::SmallVec;

type NodeTypes = HashMap<ast::AstNodeId, TypeId>;
type NodeEntyped = HashMap<ast::AstNodeId, ast::AstNode /* type expr */>;
type TypesEq = DisjointSet<TypeId>;

struct TypeAnalysis<'a> {
    scopes: &'a Scopes,
    constexpr: &'a ConstVals,
    types: TypeStore,
    eq_types: TypesEq,
    node_types: NodeTypes,
    node_entyped: NodeEntyped,
}

impl<'a> TypeAnalysis<'a> {
    fn type_node(&mut self, node: ast::AstNode, ty: TypeId) {
        if let Some(old) = self.node_types.insert(node.node_id(), ty) {
            self.eq_types.join(&ty, &old);
        }
    }
    fn equate_nodes(&mut self, nodes: &[ast::AstNode]) {
        let type_var = self.types.add_typevar();
        for node in nodes {
            self.type_node(node.clone(), type_var);
        }
    }
}

#[derive(Debug)]
pub struct Types {
    pub store: TypeStore,
    node_types: NodeTypes,
    node_entyped: NodeEntyped,
}
impl Types {
    pub fn node_type(&self, node: ast::AstNodeId) -> Option<TypeId> {
        if let Some(mut id) = self.node_types.get(&node).copied() {
            let mut ty;
            while let Type::TypeVarResolved(_, inner) = { 
                ty = self.store.query(id);
                &ty
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

pub fn analyze(root: &Rc<ast::Ast>, types: TypeStore, scopes: &Scopes, constexpr: &ConstVals) -> Types {
    let mut ctx = TypeAnalysis {
        scopes,
        types,
        constexpr,
        eq_types: TypesEq::new(1000),
        node_types: NodeTypes::new(),
        node_entyped: NodeEntyped::new(),
    };
    
    preorder(root, |node| {
        println!("{:?}", node);
        ctx.visit_node(node);
    });

    if let Err(e) = reduce_types(&mut ctx) {
        e.print(&ctx.types);
        panic!("Type resolution error!");
    }

    for (_node, ty) in ctx.node_types.iter_mut() {
        *ty = ctx.types.simplify(*ty);
    }

    Types {
        store: ctx.types,
        node_types: ctx.node_types,
        node_entyped: ctx.node_entyped
    }
}

trait Typing : ast::AstNodeWrap {
    fn analyze_type(&self, ctx: &mut TypeAnalysis);
}

impl Visitor for TypeAnalysis<'_> {
    type Ret = ();
    fn visit_ast(&mut self, _ast: &Rc<ast::Ast>) {}
    
    fn visit_expr_entype(&mut self, node: &Rc<ast::ExprEntype>) {
        let meta_id = self.types.add(Type::Type);
        let target = node.target();
        let ty = node.ty();

        let name = target.name();
        let scope = self.scopes.node_scope(target.node_id()).unwrap();
        let decl = self.scopes.resolve(scope, name);
        if decl.is_none() {
            panic!("Error: could not find ident `{}` in scope", name);
        }
        let decl = decl.unwrap();

        self.type_node(node.as_any(), meta_id);
        self.type_node(ty.as_any(), meta_id);
        self.type_node(decl.node.clone(), self.constexpr.entype_val(decl).unwrap());
    }

    fn visit_expr_literal(&mut self, node: &Rc<ast::ExprLiteral>) {
        let lit = node.literal();
        self.equate_nodes(&[node.as_any(), lit.as_any()]);
    }

    fn visit_expr_ident(&mut self, node: &Rc<ast::ExprIdent>) {
        let ident = node.ident();
        let name = ident.name();
        
        if let Some(builtin) = self.scopes.builtins.get(name) {
            self.type_node(node.as_any(), builtin.ty);
            return;
        }
        
        let scope = self.scopes.node_scope(node.node_id()).unwrap();
        
        let decl = self.scopes.resolve(scope, name);
        if decl.is_none() {
            panic!("Error: referenced unknown ident `{}`", name);
        }
        let decl = decl.unwrap();
        
        self.equate_nodes(&[node.as_any(), decl.node.clone()]);
    }

    fn visit_expr_if(&mut self, node: &Rc<ast::ExprIf>) {
        let bool_id = self.types.add(Type::Bool);

        let cond = node.cond();
        // Pre-select a type for the cond expression
        self.type_node(cond.as_any(), bool_id);

        let then_expr = node.then_expr();
        let else_expr = node.else_expr();
        self.equate_nodes(&[node.as_any(), then_expr.as_any(), else_expr.as_any()]);
    }

    fn visit_expr_if_case(&mut self, node: &Rc<ast::ExprIfCase>) {
        let cond = node.cond();
        let mut cond_ty_nodes = SmallVec::<[ast::AstNode; 4]>::new();
        let mut val_ty_nodes = SmallVec::<[ast::AstNode; 4]>::new();

        cond_ty_nodes.push(cond.as_any());
        val_ty_nodes.push(node.as_any());
        
        for case in node.cases() {
            match case {
                ast::IfCase::OnVal(lit, val) => {
                    cond_ty_nodes.push(lit.as_any());
                    val_ty_nodes.push(val.as_any());
                }
                ast::IfCase::Else(val) => {
                    val_ty_nodes.push(val.as_any());
                }
            }
        }
        
        self.equate_nodes(&cond_ty_nodes);
        self.equate_nodes(&val_ty_nodes);
    }

    fn visit_expr_var_decl(&mut self, node: &Rc<ast::ExprVarDecl>) {
        let val = node.val();
        self.equate_nodes(&[node.as_any(), val.as_any()]);
    }

    fn visit_expr_fn_decl(&mut self, node: &Rc<ast::ExprFnDecl>) {
        let name = node.name();
        let val = node.val();

        let mut arg_types = FnArgTypes::new();
        for arg in node.args() {
            let type_var = self.types.add_typevar();
            self.type_node(arg.as_any(), type_var);
            arg_types.push(type_var);            
        }
        let ret_type = self.types.add_typevar();
        self.type_node(val.as_any(), ret_type);
        
        let fn_type = Type::Fn(arg_types, ret_type,
                if node.pure() { Purity::Pure } else { Purity::Impure });
        let fn_type_id = self.types.add(fn_type);
        self.type_node(node.as_any(), fn_type_id);
        if let Some(name) = name {
            self.type_node(name.as_any(), fn_type_id);
        }
    }

    fn visit_expr_curry(&mut self, node: &Rc<ast::ExprCurry>) {
        let curry_id = self.types.add(Type::Curry);
        self.type_node(node.as_any(), curry_id);
    }

    fn visit_expr_unary(&mut self, node: &Rc<ast::ExprUnary>) {
        let inner = node.operand();
        match node.operator() {
            | ast::UnaryOp::Negate => {
                let int_id = self.types.add(Type::Int);
                self.type_node(node.as_any(), int_id);
                self.type_node(inner.as_any(), int_id);
            }
            | ast::UnaryOp::Head => {
                let type_var = self.types.add_typevar();
                let type_var_list = Type::List(type_var);
                let type_var_list_id = self.types.add(type_var_list);
                self.type_node(node.as_any(), type_var);
                self.type_node(inner.as_any(), type_var_list_id);
            }
            | ast::UnaryOp::Tail => {
                let type_var = self.types.add_typevar();
                let type_var_list = Type::List(type_var);
                let type_var_list_id = self.types.add(type_var_list);
                self.type_node(node.as_any(), type_var_list_id);
                self.type_node(inner.as_any(), type_var_list_id);
            }
        }
    }

    fn visit_expr_binary(&mut self, node: &Rc<ast::ExprBinary>) {
        let (left, right) = node.operands();
        match node.operator() {
            | ast::BinaryOp::Add
            | ast::BinaryOp::Sub
            | ast::BinaryOp::Mul
            | ast::BinaryOp::Div
            | ast::BinaryOp::Mod
            => {
                let int_id = self.types.add(Type::Int);
                self.type_node(node.as_any(), int_id);
                self.type_node(left.as_any(), int_id);
                self.type_node(right.as_any(), int_id);
            }
            | ast::BinaryOp::Prepend => {
                let type_var = self.types.add_typevar();
                let type_var_list = Type::List(type_var);
                let type_var_list_id = self.types.add(type_var_list);
                self.type_node(left.as_any(), type_var);
                self.type_node(right.as_any(), type_var_list_id);
            }
            | ast::BinaryOp::Eq
            => {
                let bool_id = self.types.add(Type::Bool);
                self.type_node(node.as_any(), bool_id);
                self.equate_nodes(&[left.as_any(), right.as_any()]);
            }
            | ast::BinaryOp::LastUnit =>
                self.equate_nodes(&[node.as_any(), right.as_any()])
        }
    }

    fn visit_expr_fn_call(&mut self, node: &Rc<ast::ExprFnCall>) {
        let callee = node.callee();

        let mut arg_types = FnArgTypes::new();
        for arg in node.args() {
            let type_var = self.types.add_typevar();
            self.type_node(arg.as_any(), type_var);
            arg_types.push(type_var);
            
        }
        let ret_type = self.types.add_typevar();
        self.type_node(node.as_any(), ret_type);

        let fn_type = Type::Fn(arg_types, ret_type,
                if node.pure() { Purity::Pure } else { Purity::Impure });
        let fn_type_id = self.types.add(fn_type);
        self.type_node(callee.as_any(), fn_type_id);
    }

    fn visit_literal_int(&mut self, node: &Rc<ast::LiteralInt>) {
        let ty_id = self.types.add(Type::Int);
        self.type_node(node.as_any(), ty_id);
    }

    fn visit_literal_str(&mut self, node: &Rc<ast::LiteralString>) {
        let ty_id = self.types.add(Type::String);
        self.type_node(node.as_any(), ty_id);
    }

    fn visit_literal_bool(&mut self, node: &Rc<ast::LiteralBool>) {
        let ty_id = self.types.add(Type::Bool);
        self.type_node(node.as_any(), ty_id);
    }

    fn visit_literal_nil(&mut self, node: &Rc<ast::LiteralNil>) {
        let type_var = self.types.add_typevar();
        let generic_list = Type::List(type_var);
        let generic_list_id = self.types.add(generic_list);
        self.type_node(node.as_any(), generic_list_id);
    }

    fn visit_ident(&mut self, _: &Rc<ast::Ident>) { }
}
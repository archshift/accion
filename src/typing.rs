use std::collections::HashMap;
use crate::ast::{self, AstNode};
use crate::scoping::Scopes;
use crate::disjoint_set::DisjointSet;
use smallvec::SmallVec;
use dynstack::{DynStack, dyn_push};
use derive_newtype::NewType;

type FnArgTypes = SmallVec<[TypeId; 4]>;

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
enum BaseType {
    Int,
    Bool,
    String,
    Curry,
    Type,
    List(TypeId),
    Fn(FnArgTypes, TypeId),
    TypeVar(usize),
    TypeExpr(ast::AstNodeId),
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
enum Purity {
    Pure,
    Impure,
    Any
}

impl Purity {
    fn mix(self, other: Purity) -> Purity {
        match (self, other) {
            (Purity::Any, other)
            | (other, Purity::Any) => other,
            (Purity::Pure, Purity::Pure) => Purity::Pure,
            (Purity::Impure, Purity::Impure) => Purity::Impure,
            _ => panic!("Purity mismatch!")
        }
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
struct Type {
    base: BaseType,
    purity: Purity,
}
impl Type {
    const fn new(base: BaseType) -> Self {
        Self {
            base, purity: Purity::Any
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
struct TypeId(usize);
type NodeTypes = HashMap<ast::AstNodeId, TypeId>;
type NodeTypesEq = DisjointSet<ast::AstNodeId>;

#[derive(NewType)]
struct NodeStack(DynStack<dyn Typing>);
impl NodeStack {
    fn new() -> Self {
        Self(DynStack::new())
    }
    
    fn push<N: 'static + Typing>(&mut self, node: N) {
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

#[derive(Default, Debug)]
struct TypeStore {
    types: Vec<Type>,
    ids: HashMap<Type, TypeId>,
}
impl TypeStore {
    fn new() -> Self {
        Self {
            types: Vec::new(),
            ids: HashMap::new()
        }
    }
    fn add(&mut self, ty: Type) -> TypeId {
        let next_id = self.types.len();
        let out = *self.ids.entry(ty.clone())
            .or_insert(TypeId(next_id) );
        if out.0 == next_id {
            self.types.push(ty);
        }
        out
    }
    fn add_typevar(&mut self) -> TypeId {
        let next_id = self.types.len();
        self.add(Type::new(BaseType::TypeVar(next_id)))
    }
    fn query(&self, ty: TypeId) -> &Type {
        &self.types[ty.0]
    }
    fn join(&mut self, ty1: TypeId, ty2: TypeId) -> TypeId {
        let first = self.query(ty1);
        let second = self.query(ty2);
        assert_eq!(first.base, second.base, "Type mismatch!");
        let purity = first.purity.mix(second.purity);
        let new_base = first.base.clone();

        self.add(Type {
            base: new_base,
            purity
        })
    }
}

struct TypingCtx<'a> {
    scopes: &'a Scopes,
    types: TypeStore,
    node_types: NodeTypes,
    added_nodes: NodeStack,
    eq_nodes: NodeTypesEq,
}

impl<'a> TypingCtx<'a> {
    fn type_node(&mut self, node_id: ast::AstNodeId, ty: TypeId) -> TypeId {
        let types = &mut self.types;
        self.node_types.entry(node_id)
            .and_modify(|old| *old = types.join(*old, ty))
            .or_insert(ty)
            .clone()
    }
    fn equate_nodes(&mut self, node1: ast::AstNodeId, node2: ast::AstNodeId) {
        self.eq_nodes.join(&node1, &node2);
    }
    fn enqueue<N: 'static + Typing>(&mut self, node: N) {
        self.added_nodes.push(node);
    }
}

#[derive(Debug)]
pub struct Types {
    types: TypeStore,
    node_types: NodeTypes,
}

pub fn analyze(root: &ast::Ast, scopes: &Scopes) -> Types {
    let mut ctx = TypingCtx {
        scopes,
        types: TypeStore::new(),
        node_types: NodeTypes::new(),
        added_nodes: NodeStack::new(),
        eq_nodes: NodeTypesEq::new(scopes.node_count * 2) // TODO: hack,
    };
    
    let mut s = NodeStack::new();

    ctx.enqueue(root.clone());
    s.move_from(&mut ctx.added_nodes);

    while let Some(node) = s.peek() {
        node.analyze_type(&mut ctx);
        s.remove_last();

        s.move_from(&mut ctx.added_nodes);
    }

    Types {
        types: ctx.types,
        node_types: ctx.node_types,
    }
}

const ANY_BOOL: Type = Type::new(BaseType::Bool);
const ANY_INT: Type = Type::new(BaseType::Int);
const ANY_STRING: Type = Type::new(BaseType::String);
const ANY_CURRY: Type = Type::new(BaseType::Curry);
const PURE_META: Type = Type { base: BaseType::Type, purity: Purity::Pure };

trait Typing : ast::AstNode {
    fn analyze_type(&self, ctx: &mut TypingCtx);
}

impl Typing for ast::Ast {
    fn analyze_type(&self, ctx: &mut TypingCtx) {
        for decl in self.decls() {
            ctx.enqueue(decl);
        }
    }
}

impl Typing for ast::AnyExpr {
    fn analyze_type(&self, ctx: &mut TypingCtx) {
        match self.select() {
            ast::Expr::Binary(e) => e.analyze_type(ctx),
            ast::Expr::Unary(e) => e.analyze_type(ctx),
            ast::Expr::FnCall(e) => e.analyze_type(ctx),
            ast::Expr::If(e) => e.analyze_type(ctx),
            ast::Expr::IfCase(e) => e.analyze_type(ctx),
            ast::Expr::Entype(e) => e.analyze_type(ctx),
            ast::Expr::VarDecl(e) => e.analyze_type(ctx),
            ast::Expr::FnDecl(e) => e.analyze_type(ctx),
            ast::Expr::Literal(e) => e.analyze_type(ctx),
            ast::Expr::Ident(e) => e.analyze_type(ctx),
            ast::Expr::Curry(e) => e.analyze_type(ctx),
        }
    }
}
impl Typing for ast::ExprEntype {
    fn analyze_type(&self, ctx: &mut TypingCtx) {
        let meta_id = ctx.types.add(PURE_META);
        let target = self.target();
        let ty = self.ty();

        let name = target.name();
        let scope = ctx.scopes.node_scope(self.node_id()).unwrap();
        let decl = scope.resolve(name);
        if decl.is_none() {
            panic!("Error: could not find ident `{}` in scope", name);
        }
        let decl = decl.unwrap();

        ctx.type_node(self.node_id(), meta_id);
        ctx.equate_nodes(self.node_id(), ty.node_id());

        let type_expr = Type { base: BaseType::TypeExpr(ty.node_id()), purity: Purity::Pure };
        let type_expr_id = ctx.types.add(type_expr);
        ctx.type_node(decl.node_id, type_expr_id);

        ctx.enqueue(ty);
    }
}
impl Typing for ast::ExprLiteral {
    fn analyze_type(&self, ctx: &mut TypingCtx) {
        let lit = self.literal();
        ctx.equate_nodes(self.node_id(), lit.node_id());
        ctx.enqueue(lit);
    }
}
impl Typing for ast::ExprIdent {
    fn analyze_type(&self, ctx: &mut TypingCtx) {
        let meta_id = ctx.types.add(PURE_META);
        let meta_list = ctx.types.add(Type::new(BaseType::List(meta_id)));

        let ident = self.ident();
        let name = ident.name();
        let ty = match name {
            | "Int"
            | "Str"
            => meta_id,
            | "Fn"
            => ctx.types.add(Type::new(BaseType::Fn(
                    FnArgTypes::from_slice(&[meta_list]),
                    meta_id
                ))
            ),
            | "List"
            => ctx.types.add(Type::new(BaseType::Fn(
                    FnArgTypes::from_slice(&[meta_id]),
                    meta_id
                ))
            ),

            _ => {
                let scope = ctx.scopes.node_scope(self.node_id()).unwrap();
                
                let decl = ctx.scopes.resolve(scope, name);
                if decl.is_none() {
                    panic!("Error: referenced unknown ident `{}`", name);
                }
                let decl = decl.unwrap();
                
                ctx.equate_nodes(self.node_id(), decl.node_id);
                return;
            }
        };

        ctx.type_node(self.node_id(), ty);
    }
}
impl Typing for ast::ExprIf {
    fn analyze_type(&self, ctx: &mut TypingCtx) {
        let bool_id = ctx.types.add(ANY_BOOL);

        let cond = self.cond();
        // Pre-select a type for the cond expression
        ctx.type_node(cond.node_id(), bool_id);
        ctx.enqueue(cond);

        let then_expr = self.then_expr();
        let else_expr = self.else_expr();
        ctx.equate_nodes(self.node_id(), then_expr.node_id());
        ctx.equate_nodes(self.node_id(), else_expr.node_id());
        ctx.enqueue(then_expr);
        ctx.enqueue(else_expr);
    }
}
impl Typing for ast::ExprIfCase {
    fn analyze_type(&self, ctx: &mut TypingCtx) {
        let cond = self.cond();
        
        for case in self.cases() {
            match case {
                ast::IfCase::OnVal(lit, val) => {
                    ctx.equate_nodes(cond.node_id(), lit.node_id());
                    ctx.equate_nodes(self.node_id(), val.node_id());
                    ctx.enqueue(lit);
                    ctx.enqueue(val);
                }
                ast::IfCase::Else(val) => {
                    ctx.equate_nodes(self.node_id(), val.node_id());
                    ctx.enqueue(val);
                }
            }
        }
        ctx.enqueue(cond);
    }
}
impl Typing for ast::ExprVarDecl {
    fn analyze_type(&self, ctx: &mut TypingCtx) {
        let val = self.val();
        ctx.equate_nodes(self.node_id(), val.node_id());
        ctx.enqueue(val);
    }
}
impl Typing for ast::ExprFnDecl {
    fn analyze_type(&self, ctx: &mut TypingCtx) {
        let val = self.val();

        let mut arg_types = FnArgTypes::new();
        for arg in self.args() {
            let type_var = ctx.types.add_typevar();
            ctx.type_node(arg.node_id(), type_var);
            arg_types.push(type_var);            
        }
        let ret_type = ctx.types.add_typevar();
        ctx.type_node(val.node_id(), ret_type);
        
        let fn_type = Type {
            base: BaseType::Fn(arg_types, ret_type),
            purity: if self.pure() { Purity::Pure } else { Purity::Impure }
        };
        let fn_type_id = ctx.types.add(fn_type);
        ctx.type_node(self.node_id(), fn_type_id);
        ctx.enqueue(self.val());
    }
}
impl Typing for ast::ExprCurry {
    fn analyze_type(&self, ctx: &mut TypingCtx) {
        let curry_id = ctx.types.add(ANY_CURRY);
        ctx.type_node(self.node_id(), curry_id);
    }
}
impl Typing for ast::ExprUnary {
    fn analyze_type(&self, ctx: &mut TypingCtx) {
        let inner = self.operand();
        match self.operator() {
            | ast::UnaryOp::Negate => {
                let int_id = ctx.types.add(ANY_INT);
                ctx.type_node(self.node_id(), int_id);
                ctx.equate_nodes(self.node_id(), inner.node_id());
            }
            | ast::UnaryOp::Head => {
                let type_var = ctx.types.add_typevar();
                let type_var_list = Type::new(BaseType::List(type_var));
                let type_var_list_id = ctx.types.add(type_var_list);
                ctx.type_node(self.node_id(), type_var);
                ctx.type_node(inner.node_id(), type_var_list_id);
            }
            | ast::UnaryOp::Tail => {
                let type_var = ctx.types.add_typevar();
                let type_var_list = Type::new(BaseType::List(type_var));
                let type_var_list_id = ctx.types.add(type_var_list);
                ctx.type_node(self.node_id(), type_var_list_id);
                ctx.type_node(inner.node_id(), type_var_list_id);
            }
        }
        ctx.enqueue(inner);
    }
}
impl Typing for ast::ExprBinary {
    fn analyze_type(&self, ctx: &mut TypingCtx) {
        let (left, right) = self.operands();
        match self.operator() {
            | ast::BinaryOp::Add
            | ast::BinaryOp::Sub
            | ast::BinaryOp::Mul
            | ast::BinaryOp::Div
            | ast::BinaryOp::Mod
            => {
                let int_id = ctx.types.add(ANY_INT);
                ctx.type_node(self.node_id(), int_id);
                ctx.equate_nodes(left.node_id(), right.node_id());
                ctx.equate_nodes(self.node_id(), right.node_id());
            }
            | ast::BinaryOp::Prepend => {
                let type_var = ctx.types.add_typevar();
                let type_var_list = Type::new(BaseType::List(type_var));
                let type_var_list_id = ctx.types.add(type_var_list);
                ctx.type_node(left.node_id(), type_var);
                ctx.type_node(right.node_id(), type_var_list_id);
            }
            | ast::BinaryOp::LastUnit =>
                ctx.equate_nodes(self.node_id(), right.node_id())
        }
        ctx.enqueue(left);
        ctx.enqueue(right);
    }
}
impl Typing for ast::ExprFnCall {
    fn analyze_type(&self, ctx: &mut TypingCtx) {
        let callee = self.callee();
        
        let mut arg_types = FnArgTypes::new();
        for arg in self.args() {
            let type_var = ctx.types.add_typevar();
            ctx.type_node(arg.node_id(), type_var);
            arg_types.push(type_var);
            
            ctx.enqueue(arg);
        }
        let ret_type = ctx.types.add_typevar();

        let fn_type = Type {
            base: BaseType::Fn(arg_types, ret_type),
            purity: if self.pure() { Purity::Pure } else { Purity::Impure }
        };
        let fn_type_id = ctx.types.add(fn_type);
        ctx.type_node(callee.node_id(), fn_type_id);

        ctx.enqueue(callee);
    }
}

impl Typing for ast::AnyLiteral {
    fn analyze_type(&self, ctx: &mut TypingCtx) {
        let ty = match self.select() {
            ast::Literal::Int(_) => ANY_INT,
            ast::Literal::String(_) => ANY_STRING,
        };
        let ty_id = ctx.types.add(ty);
        ctx.type_node(self.node_id(), ty_id);
    }
}
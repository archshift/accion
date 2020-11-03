use std::collections::HashMap;
use std::fmt;

use crate::ast::{self, AstNodeWrap};
use crate::analysis::{preorder, Visitor, scoping::Scopes};
use crate::disjoint_set::DisjointSet;
use smallvec::SmallVec;

pub type FnArgTypes = SmallVec<[TypeId; 4]>;

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum BaseType {
    Int,
    Bool,
    String,
    Curry,
    Type,
    List(TypeId),
    Fn(FnArgTypes, TypeId),
    TypeExpr(ast::AstNodeId),
    
    TypeVar(usize),
    TypeVarResolved(usize, TypeId),
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Purity {
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
pub struct Type {
    pub base: BaseType,
    pub purity: Purity,
}
impl Type {
    pub const fn new(base: BaseType) -> Self {
        Self {
            base, purity: Purity::Any
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct TypeId(usize);
impl fmt::Debug for TypeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "TypeId({})", self.0)
    }
}

type NodeTypes = HashMap<ast::AstNodeId, TypeId>;
type TypesEq = DisjointSet<TypeId>;

pub struct TypeStore {
    types: Vec<Type>,
    ids: HashMap<Type, TypeId>,
    eq_types: TypesEq,
}
impl TypeStore {
    pub fn new() -> Self {
        Self {
            types: Vec::new(),
            ids: HashMap::new(),
            eq_types: TypesEq::new(1000),
        }
    }
    pub fn add(&mut self, ty: Type) -> TypeId {
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
    fn reify(&mut self, id: TypeId, real: TypeId) -> TypeId {
        let new_purity = self.types[real.0].purity;
        let old = &mut self.types[id.0];
        self.ids.remove(old);
        
        old.purity = old.purity.mix(new_purity);

        if let BaseType::TypeVar(tv_id) = old.base {
            old.base = BaseType::TypeVarResolved(tv_id, real);
        }
        self.ids.insert(old.clone(), id);
        id
    }
    fn join(&mut self, ty1: TypeId, ty2: TypeId) -> Result<TypeId, String> {
        let first = self.query(ty1);
        let second = self.query(ty2);
        let first_base = first.base.clone();
        let second_base = second.base.clone();
        
        let chain_err = |e: String| {
            format!("{}\nCaused by join of {:?}: {:?} and {:?}: {:?}",
                e, ty1, first_base,
                ty2, second_base)
        };
        
        let purity = first.purity.mix(second.purity);
        let most_specific = match (&first_base, &second_base) {
            // Can't call join on post-reduced types
            | (BaseType::TypeVarResolved(_, _), _)
            | (_, BaseType::TypeVarResolved(_, _))
            => unreachable!(),

            // Fully-compatible type assignments
            | (out @ BaseType::Int, BaseType::Int)
            | (out @ BaseType::Bool, BaseType::Bool)
            | (out @ BaseType::String, BaseType::String)
            | (out @ BaseType::Type, BaseType::Type)
            | (out @ BaseType::Curry, BaseType::Curry)
            // Late-stage type resolution
            | (out, BaseType::TypeVar(_))
            | (BaseType::TypeVar(_), out)
            | (out, BaseType::TypeExpr(_))
            | (BaseType::TypeExpr(_), out)
            => out.clone(),

            // Match the inner types
            | (BaseType::List(e), BaseType::List(f))
            => BaseType::List(self.join(*e, *f)
                    .map_err(chain_err)?),

            // Match the inner types
            | (BaseType::Fn(args1, ret1),
                BaseType::Fn(args2, ret2))
            => {
                if args1.len() != args2.len() {
                    return Err(format!(
                        "Tried to resolve fn calls with different arg list lengths! {:?} vs {:?}",
                        first.base, second.base
                    ));
                }
                let new_args: Result<SmallVec<_>, String> =
                    args1.iter().copied()
                    .zip(args2.iter().copied())
                    .map(|p| self.join(p.0, p.1))
                    .map(|r| r.map_err(chain_err))
                    .collect();
                let new_ret = self.join(*ret1, *ret2)
                    .map_err(chain_err)?;
                BaseType::Fn(new_args?, new_ret)
            }

            (x, y) => {
                return Err(format!(
                    "Type mismatch between {:?}: {:?} and {:?}: {:?}!",
                    ty1, x,
                    ty2, y
                ));
            }
        };

        self.eq_types.join(&ty1, &ty2);

        Ok(self.add(Type {
            base: most_specific,
            purity
        }))
    }
}
impl fmt::Debug for TypeStore {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut kvs: Vec<(Type, TypeId)> =
            self.ids.iter()
            .map(|(ty, id)| (ty.clone(), id.clone()))
            .collect();
        
        kvs.sort_by(|(_, TypeId(a)), (_, TypeId(b))| a.cmp(b));
        
        let mut dbg_s = f.debug_struct("TypeStore");
        for (ty, ty_id) in kvs {
            dbg_s.field(&ty_id.0.to_string(), &ty);
        }
        dbg_s.field("eq_types", &self.eq_types);
        dbg_s.finish()
    }
}

struct TypeAnalysis<'a> {
    scopes: &'a Scopes,
    types: TypeStore,
    node_types: NodeTypes,
}

impl<'a> TypeAnalysis<'a> {
    fn type_node(&mut self, node_id: ast::AstNodeId, ty: TypeId) {
        if let Some(old) = self.node_types.insert(node_id, ty) {
            let new_type = self.types.join(old, ty)
                .map_err(|e| {
                    format!("{}\nCaused by assigning type of node {:?} to {:?}: {:?}",
                        e, node_id, ty, self.types.query(ty))
                })
                .unwrap();
            self.node_types.insert(node_id, new_type);
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
    types: TypeStore,
    pub node_types: NodeTypes,
}
impl Types {
    pub fn node_type(&self, node: ast::AstNodeId) -> Option<&Type> {
        if let Some(mut id) = self.node_types.get(&node).copied() {
            let mut ty;
            while let BaseType::TypeVarResolved(_, inner) = { 
                ty = self.types.query(id);
                &ty.base
            } {
                id = *inner;
            }
            Some(ty)
        } else {
            None
        }
    }
}

fn reduce_types2(types: &mut TypeStore, first: TypeId, second: TypeId) -> TypeId {
    if first == second {
        return first;
    }

    let first_ty = types.query(first);
    let second_ty = types.query(second);
    let first_base = first_ty.base.clone();
    let second_base = second_ty.base.clone();
    
    match (first_base, second_base) {
        (BaseType::TypeVarResolved(_, inner), _) => {
            reduce_types2(types, inner, second)
        }
        (_, BaseType::TypeVarResolved(_, inner)) => {
            reduce_types2(types, first, inner)
        }
        (BaseType::TypeVar(_), _) => {
            types.reify(first, second)
        }
        (_, BaseType::TypeVar(_)) => {
            types.reify(second, first)
        }
        (BaseType::Fn(args1, ret1), BaseType::Fn(args2, ret2)) => {
            for arg in args1.iter().zip(args2.iter()) {
                reduce_types2(types, *arg.0, *arg.1);
            }
            reduce_types2(types, ret1, ret2);
            first
        }
        (BaseType::List(ty1), BaseType::List(ty2)) => {
            reduce_types2(types, ty1, ty2);
            first
        }

        (a, b) if a == b => first,

        (a, b) => unimplemented!("reify {:?}, {:?}", a, b)
    }
}

fn reduce_types(types: &mut TypeStore) {
    let groups = types.eq_types.groups();
    for group in groups {
        let mut it = group.iter().copied();
        let first = it.next().unwrap();
        it.fold(first, |acc, new| reduce_types2(types, acc, new));
    }
}

pub fn analyze(root: &ast::Ast, types: TypeStore, scopes: &Scopes) -> Types {
    let mut ctx = TypeAnalysis {
        scopes,
        types,
        node_types: NodeTypes::new(),
    };
    
    preorder(root.clone(), |node| {
        ctx.visit_node(node);
    });

    reduce_types(&mut ctx.types);

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

trait Typing : ast::AstNodeWrap {
    fn analyze_type(&self, ctx: &mut TypeAnalysis);
}

impl Visitor for TypeAnalysis<'_> {
    fn visit_expr_entype(&mut self, node: &ast::ExprEntype) {
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

    fn visit_expr_literal(&mut self, node: &ast::ExprLiteral) {
        let lit = node.literal();
        self.equate_nodes(&[node.node_id(), lit.node_id()]);
    }

    fn visit_expr_ident(&mut self, node: &ast::ExprIdent) {
        let ident = node.ident();
        let name = ident.name();
        
        if name == "nil" {
            let type_var = self.types.add_typevar();
            let generic_list = Type::new(BaseType::List(type_var));
            let generic_list_id = self.types.add(generic_list);
            self.type_node(node.node_id(), generic_list_id);
            return;
        }
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

    fn visit_expr_if(&mut self, node: &ast::ExprIf) {
        let bool_id = self.types.add(ANY_BOOL);

        let cond = node.cond();
        // Pre-select a type for the cond expression
        self.type_node(cond.node_id(), bool_id);

        let then_expr = node.then_expr();
        let else_expr = node.else_expr();
        self.equate_nodes(&[node.node_id(), then_expr.node_id(), else_expr.node_id()]);
    }

    fn visit_expr_if_case(&mut self, node: &ast::ExprIfCase) {
        let cond = node.cond();
        let mut cond_ty_nodes = SmallVec::<[ast::AstNodeId; 4]>::new();
        let mut val_ty_nodes = SmallVec::<[ast::AstNodeId; 4]>::new();

        cond_ty_nodes.push(cond.node_id());
        
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

    fn visit_expr_var_decl(&mut self, node: &ast::ExprVarDecl) {
        let val = node.val();
        self.equate_nodes(&[node.node_id(), val.node_id()]);
    }

    fn visit_expr_fn_decl(&mut self, node: &ast::ExprFnDecl) {
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

    fn visit_expr_curry(&mut self, node: &ast::ExprCurry) {
        let curry_id = self.types.add(ANY_CURRY);
        self.type_node(node.node_id(), curry_id);
    }

    fn visit_expr_unary(&mut self, node: &ast::ExprUnary) {
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

    fn visit_expr_binary(&mut self, node: &ast::ExprBinary) {
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

    fn visit_literal_int(&mut self, node: &ast::LiteralInt) {
        let ty_id = self.types.add(ANY_INT);
        self.type_node(node.node_id(), ty_id);
    }

    fn visit_literal_str(&mut self, node: &ast::LiteralString) {
        let ty_id = self.types.add(ANY_STRING);
        self.type_node(node.node_id(), ty_id);
    }

    fn visit_ident(&mut self, _: &ast::Ident) { }
}
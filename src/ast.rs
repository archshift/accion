use std::fmt;
use std::ffi;

use crate::parser::raw;
use crate::lliter::adapt_ll;
use crate::sexp::SExp;

use num_traits::cast::FromPrimitive;
use num_derive::FromPrimitive;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct AstNodeId(usize);
impl fmt::Debug for AstNodeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "AstNodeId({})", self.0)
    }
}

pub trait AstNodeWrap : fmt::Debug {
    fn node_id(&self) -> AstNodeId;
    fn as_any(self) -> AstNode;
}

macro_rules! wrap {
    ($node:ident, $raw_node:path) => {
        #[repr(transparent)]
        pub struct $node {
            inner: *const $raw_node,
        }

        impl AstNodeWrap for $node {
            fn node_id(&self) -> AstNodeId {
                AstNodeId(self.inner as usize)
            }
            fn as_any(self) -> AstNode {
                AstNode::$node(self)
            }
        }
        impl $node {
            #[allow(dead_code)]
            pub(crate) fn wrap(ptr: *const $raw_node) -> Self {
                Self { inner: ptr }
            }

            #[allow(dead_code)]
            fn unwrap(&self) -> &$raw_node {
                unsafe { self.inner.as_ref() }.unwrap()
            }
        }
    };
}

wrap!(Ast, raw::ast_program);
wrap!(AnyExpr, raw::ast_expr);
wrap!(ExprUnary, raw::ast_expr);
wrap!(ExprBinary, raw::ast_expr);
wrap!(ExprIdent, raw::ast_expr);
wrap!(ExprLiteral, raw::ast_expr);
wrap!(ExprIf, raw::ast_expr);
wrap!(ExprIfCase, raw::ast_expr);
wrap!(ExprVarDecl, raw::ast_expr);
wrap!(ExprFnDecl, raw::ast_expr);
wrap!(ExprEntype, raw::ast_expr);
wrap!(ExprFnCall, raw::ast_expr);
wrap!(ExprCurry, raw::ast_expr);
wrap!(Ident, raw::ast_ident);
wrap!(AnyLiteral, raw::ast_literal);
wrap!(LiteralInt, raw::ast_literal);
wrap!(LiteralString, raw::ast_literal);

pub enum AstNode {
    Ast(Ast),
    AnyExpr(AnyExpr),
    ExprUnary(ExprUnary),
    ExprBinary(ExprBinary),
    ExprIdent(ExprIdent),
    ExprLiteral(ExprLiteral),
    ExprIf(ExprIf),
    ExprIfCase(ExprIfCase),
    ExprVarDecl(ExprVarDecl),
    ExprFnDecl(ExprFnDecl),
    ExprEntype(ExprEntype),
    ExprFnCall(ExprFnCall),
    ExprCurry(ExprCurry),
    Ident(Ident),
    AnyLiteral(AnyLiteral),
    LiteralInt(LiteralInt),
    LiteralString(LiteralString),
}


impl Ast {
    pub fn decls(&self) -> impl Iterator<Item=AnyExpr> {
        let program = self.unwrap();
        
        unsafe { adapt_ll(program.exprs.as_ref(), |e| e.tail.as_ref()) }
            .map(|n| AnyExpr { inner: n.head })
    }
}
impl Clone for Ast {
    fn clone(&self) -> Self {
        Self { inner: self.inner }
    }
}
impl fmt::Debug for Ast {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        SExp::new(f, "program")?
            .add_all(self.decls())?
            .finish()
    }
}


impl Ident {
    pub fn name(&self) -> &str {
        unsafe { ffi::CStr::from_ptr(self.unwrap().name) }
            .to_str()
            .unwrap()
    }
}
impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        SExp::new(f, "ident")?
            .add(&self.name())?
            .finish()
    }
}


impl AnyLiteral {
    fn ty(&self) -> raw::ast_literal_type {
        self.unwrap().ty
    }
    pub fn select(&self) -> Literal {
        match self.ty() {
            raw::ast_literal_type_AST_LITERAL_STRING =>
                Literal::String(LiteralString { inner: self.inner }),
            raw::ast_literal_type_AST_LITERAL_INT =>
                Literal::Int(LiteralInt { inner: self.inner }),
            _ => unimplemented!()
        }
    }
}
impl fmt::Debug for AnyLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.select().fmt(f)
    }
}
pub enum Literal {
    String(LiteralString),
    Int(LiteralInt),
}
impl fmt::Debug for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(e) => e.fmt(f),
            Self::Int(e) => e.fmt(f),
        }
    }
}


impl LiteralString {
    fn val(&self) -> &str {
        let s = unsafe {
            self.unwrap().__bindgen_anon_1.str_
        };
        unsafe { ffi::CStr::from_ptr(s) }
            .to_str()
            .unwrap()
    }
}
impl fmt::Debug for LiteralString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        SExp::new(f, "literal")?
            .add(&self.val())?
            .finish()
    }
}

impl LiteralInt {
    fn val(&self) -> u64 {
        unsafe {
            self.unwrap()
                .__bindgen_anon_1
                .num
        }
    }
}
impl fmt::Debug for LiteralInt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        SExp::new(f, "literal")?
            .add(&self.val())?
            .finish()
    }
}


impl AnyExpr {
    fn ty(&self) -> raw::ast_expr_type {
        self.unwrap().ty
    }

    pub fn select(&self) -> Expr {
        match self.ty() {
            raw::ast_expr_type_AST_EXPR_IDENT =>
                Expr::Ident(ExprIdent { inner: self.inner }),
            raw::ast_expr_type_AST_EXPR_LITERAL =>
                Expr::Literal(ExprLiteral { inner: self.inner }),
            raw::ast_expr_type_AST_EXPR_UNARY =>
                Expr::Unary(ExprUnary { inner: self.inner }),
            raw::ast_expr_type_AST_EXPR_BINOP =>
                Expr::Binary(ExprBinary { inner: self.inner }),
            raw::ast_expr_type_AST_EXPR_IF =>
                Expr::If(ExprIf { inner: self.inner }),
            raw::ast_expr_type_AST_EXPR_IF_CASE =>
                Expr::IfCase(ExprIfCase { inner: self.inner }),
            raw::ast_expr_type_AST_EXPR_VAR_DECL =>
                Expr::VarDecl(ExprVarDecl { inner: self.inner }),
            raw::ast_expr_type_AST_EXPR_FN_DECL =>
                Expr::FnDecl(ExprFnDecl { inner: self.inner }),
            raw::ast_expr_type_AST_EXPR_ENTYPE =>
                Expr::Entype(ExprEntype { inner: self.inner }),
            raw::ast_expr_type_AST_EXPR_FN_CALL =>
                Expr::FnCall(ExprFnCall { inner: self.inner }),
            raw::ast_expr_type_AST_EXPR_CURRY =>
                Expr::Curry(ExprCurry { inner: self.inner }),
            _ => unreachable!()
        }
    }

    pub fn unwrap_ident(&self) -> Ident {
        if let Expr::Ident(i) = self.select() {
            i.ident()
        } else {
            panic!("Syntax error: decl name must be an ident");
        }
    }
}
impl fmt::Debug for AnyExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.select().fmt(f)
    }
}


pub enum Expr {
    Unary(ExprUnary),
    Binary(ExprBinary),
    Literal(ExprLiteral),
    Ident(ExprIdent),
    If(ExprIf),
    IfCase(ExprIfCase),
    VarDecl(ExprVarDecl),
    FnDecl(ExprFnDecl),
    Entype(ExprEntype),
    FnCall(ExprFnCall),
    Curry(ExprCurry),
}
impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unary(e) => e.fmt(f),
            Self::Binary(e) => e.fmt(f),
            Self::Literal(e) => e.fmt(f),
            Self::Ident(e) => e.fmt(f),
            Self::If(e) => e.fmt(f),
            Self::IfCase(e) => e.fmt(f),
            Self::VarDecl(e) => e.fmt(f),
            Self::FnDecl(e) => e.fmt(f),
            Self::Entype(e) => e.fmt(f),
            Self::FnCall(e) => e.fmt(f),
            Self::Curry(e) => e.fmt(f),
        }
    }
}

#[repr(u32)]
#[derive(Eq, PartialEq, Copy, Clone, Debug, FromPrimitive)]
pub enum UnaryOp {
    Head = raw::ast_operator_AST_OP_HEAD,
    Tail = raw::ast_operator_AST_OP_TAIL,
    Negate = raw::ast_operator_AST_OP_NEGATE,
}

#[repr(u32)]
#[derive(Eq, PartialEq, Copy, Clone, Debug, FromPrimitive)]
pub enum BinaryOp {
    Add = raw::ast_operator_AST_OP_ADD,
    Sub = raw::ast_operator_AST_OP_SUB,
    Mul = raw::ast_operator_AST_OP_MUL,
    Div = raw::ast_operator_AST_OP_DIV,
    Mod = raw::ast_operator_AST_OP_MOD,
    Prepend = raw::ast_operator_AST_OP_PREPEND,
    LastUnit = raw::ast_operator_AST_OP_COMPOUND,
    Eq = raw::ast_operator_AST_OP_EQ,
}

impl ExprUnary {
    pub fn operator(&self) -> UnaryOp {
        let raw_op = unsafe {
            self.unwrap()
                .__bindgen_anon_1
                .__bindgen_anon_1
                .op
        };
        UnaryOp::from_u32(raw_op).unwrap()
    }

    pub fn operand(&self) -> AnyExpr {
        let inner = unsafe {
            self.unwrap()
                .__bindgen_anon_1
                .__bindgen_anon_1
                .__bindgen_anon_1
                .inner
        };
        AnyExpr { inner }
    }
}
impl fmt::Debug for ExprUnary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        SExp::new(f, "expr unary")?
            .add(&self.operator())?
            .add(&self.operand())?
            .finish()
    }
}

impl ExprBinary {
    pub fn operator(&self) -> BinaryOp {
        let raw_op = unsafe {
            self.unwrap()
                .__bindgen_anon_1
                .__bindgen_anon_1
                .op
        };
        BinaryOp::from_u32(raw_op).unwrap()
    }

    pub fn operands(&self) -> (AnyExpr, AnyExpr) {
        let s = unsafe {
            &self.unwrap()
                .__bindgen_anon_1
                .__bindgen_anon_1
                .__bindgen_anon_1
                .__bindgen_anon_1
        };
        (AnyExpr { inner: s.left }, AnyExpr { inner: s.right })
    }
}
impl fmt::Debug for ExprBinary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (op1, op2) = self.operands();
        SExp::new(f, "expr binary")?
            .add(&self.operator())?
            .add(&op1)?
            .add(&op2)?
            .finish()
    }
}

impl ExprLiteral {
    pub fn literal(&self) -> AnyLiteral {
        let inner = unsafe {
            self.unwrap().__bindgen_anon_1.literal
        };
        AnyLiteral { inner }
    }
}
impl fmt::Debug for ExprLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        SExp::new(f, "expr literal")?
            .add(&self.literal())?
            .finish()
    }
}

impl ExprIdent {
    pub fn ident(&self) -> Ident {
        let inner = unsafe {
            self.unwrap().__bindgen_anon_1.ident
        };
        Ident { inner }
    }
}
impl fmt::Debug for ExprIdent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        SExp::new(f, "expr ident")?
            .add(&self.ident())?
            .finish()
    }
}

impl ExprIf {
    pub fn cond(&self) -> AnyExpr {
        let inner = unsafe {
            self.unwrap()
                .__bindgen_anon_1
                .__bindgen_anon_3
                .cond
        };
        AnyExpr { inner }
    }

    pub fn then_expr(&self) -> AnyExpr {
        let inner = unsafe {
            self.unwrap()
                .__bindgen_anon_1
                .__bindgen_anon_3
                .__bindgen_anon_1
                .__bindgen_anon_1
                .then
        };
        AnyExpr { inner }
    }

    pub fn else_expr(&self) -> AnyExpr {
        let inner = unsafe {
            self.unwrap()
                .__bindgen_anon_1
                .__bindgen_anon_3
                .__bindgen_anon_1
                .__bindgen_anon_1
                .els
        };
        AnyExpr { inner }
    }
}
impl fmt::Debug for ExprIf {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        SExp::new(f, "expr if")?
            .add(&self.cond())?
            .add(&self.then_expr())?
            .add(&self.else_expr())?
            .finish()
    }
}

pub enum IfCase {
    OnVal(AnyLiteral, AnyExpr),
    Else(AnyExpr)
}
impl fmt::Debug for IfCase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::OnVal(l, e) =>
                SExp::new(f, "case")?
                .add(&l)?
                .add(&e)?
                .finish(),
            Self::Else(e) =>
                SExp::new(f, "case else")?
                .add(&e)?
                .finish()
        }
    }
}

impl ExprIfCase {
    pub fn cond(&self) -> AnyExpr {
        let inner = unsafe {
            self.unwrap()
                .__bindgen_anon_1
                .__bindgen_anon_3
                .cond
        };
        AnyExpr { inner }
    }

    pub fn cases(&self) -> impl Iterator<Item=IfCase> {
        let cases = unsafe {
            self.unwrap()
                .__bindgen_anon_1
                .__bindgen_anon_3
                .__bindgen_anon_1
                .cases
        };
        let map_case = |n: &raw::ast_case_list| {
            let e =  AnyExpr { inner: n.val };
            if n.match_.is_null() {
                IfCase::Else(e)
            } else {
                IfCase::OnVal(AnyLiteral { inner: n.match_ }, e)
            }
        };
        unsafe { adapt_ll(cases.as_ref(), |e| e.tail.as_ref()) }
            .map(map_case)
    }
}
impl fmt::Debug for ExprIfCase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        SExp::new(f, "expr ifcase")?
            .add(&self.cond())?
            .add_all(self.cases())?
            .finish()
    }
}

impl ExprEntype {
    fn target_expr(&self) -> AnyExpr {
        let inner = unsafe {
            self.unwrap()
                .__bindgen_anon_1
                .__bindgen_anon_2
                .name
        };
        AnyExpr { inner }
    }

    pub fn target(&self) -> Ident {
        self.target_expr().unwrap_ident()
    }

    pub fn ty(&self) -> AnyExpr {
        let inner = unsafe {
            self.unwrap()
                .__bindgen_anon_1
                .__bindgen_anon_2
                .val
        };
        AnyExpr { inner }
    }
}
impl fmt::Debug for ExprEntype {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        SExp::new(f, "expr entype")?
            .add(&self.target())?
            .add(&self.ty())?
            .finish()
    }
}

impl ExprVarDecl {
    fn name_expr(&self) -> AnyExpr {
        let inner = unsafe {
            self.unwrap()
                .__bindgen_anon_1
                .__bindgen_anon_2
                .name
        };
        AnyExpr { inner }
    }

    pub fn name(&self) -> Ident {
        self.name_expr().unwrap_ident()
    }

    pub fn val(&self) -> AnyExpr {
        let inner = unsafe {
            self.unwrap()
                .__bindgen_anon_1
                .__bindgen_anon_2
                .val
        };
        AnyExpr { inner }
    }
}
impl fmt::Debug for ExprVarDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        SExp::new(f, "expr var_decl")?
            .add(&self.name())?
            .add(&self.val())?
            .finish()
    }
}

impl ExprFnDecl {
    fn name_expr(&self) -> Option<AnyExpr> {
        let inner = unsafe {
            self.unwrap()
                .__bindgen_anon_1
                .__bindgen_anon_2
                .name
        };
        if inner.is_null() {
            None
        } else {
            Some(AnyExpr { inner })
        }
    }

    pub fn name(&self) -> Option<Ident> {
        self.name_expr()
            .map(|e| e.unwrap_ident())
    }

    pub fn val(&self) -> AnyExpr {
        let inner = unsafe {
            self.unwrap()
                .__bindgen_anon_1
                .__bindgen_anon_2
                .val
        };
        AnyExpr { inner }
    }

    pub fn args(&self) -> impl Iterator<Item=Ident> {
        let args = unsafe {
            self.unwrap()
                .__bindgen_anon_1
                .__bindgen_anon_2
                .__bindgen_anon_1
                .__bindgen_anon_2
                .arg_names
        };
        unsafe { adapt_ll(args.as_ref(), |e| e.tail.as_ref()) }
            .map(|n| AnyExpr { inner: n.head })
            .map(|e| e.unwrap_ident())
    }

    pub fn pure(&self) -> bool {
        unsafe {
            self.unwrap()
                .__bindgen_anon_1
                .__bindgen_anon_2
                .__bindgen_anon_1
                .__bindgen_anon_2
                .pure_decl
        }
    }
}
impl fmt::Debug for ExprFnDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            
        let s =
            SExp::new(f, "expr fn_decl")?
                .add(&if self.pure() { "pure" } else { "impure" })?;
        
        if let Some(c) = self.name() {
            s.add(&c)
        } else {
            s.add(&"lambda")
        }?
            .add_all(self.args())?
            .add(&self.val())?
            .finish()
    }
}

impl ExprFnCall {
    pub fn callee(&self) -> AnyExpr {
        let inner = unsafe {
            self.unwrap()
                .__bindgen_anon_1
                .__bindgen_anon_4
                .callee
        };
        AnyExpr { inner }
    }

    pub fn args(&self) -> impl Iterator<Item=AnyExpr> {
        let args = unsafe {
            self.unwrap()
                .__bindgen_anon_1
                .__bindgen_anon_4
                .args
        };
        unsafe { adapt_ll(args.as_ref(), |e| e.tail.as_ref()) }
            .map(|n| AnyExpr { inner: n.head })
    }

    pub fn pure(&self) -> bool {
        unsafe {
            self.unwrap()
                .__bindgen_anon_1
                .__bindgen_anon_4
                .pure_call
        }
    }
}
impl fmt::Debug for ExprFnCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        SExp::new(f, "expr fn_call")?
            .add(&if self.pure() { "pure" } else { "impure" })?
            .add(&self.callee())?
            .add_all(self.args())?
            .finish()
    }
}

impl fmt::Debug for ExprCurry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        SExp::new(f, "curry")?.finish()
    }
}
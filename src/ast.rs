use std::fmt;
use std::ffi;

use crate::lliter::adapt_ll;
use crate::sexp::SExp;

#[repr(C)]
pub struct Location {
    lline: u32,
    lcol: u32,
    rline: u32,
    rcol: u32
}

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
    fn push_children(&self, dst: &mut Vec<AstNode>);
}

#[derive(Copy, Clone)]
pub enum AstNode {
    Ast(&'static Ast),
    ExprUnary(&'static ExprUnary),
    ExprBinary(&'static ExprBinary),
    ExprIdent(&'static ExprIdent),
    ExprLiteral(&'static ExprLiteral),
    ExprIf(&'static ExprIf),
    ExprIfCase(&'static ExprIfCase),
    ExprVarDecl(&'static ExprVarDecl),
    ExprFnDecl(&'static ExprFnDecl),
    ExprEntype(&'static ExprEntype),
    ExprFnCall(&'static ExprFnCall),
    ExprCurry(&'static ExprCurry),
    Ident(&'static Ident),
    LiteralInt(&'static LiteralInt),
    LiteralString(&'static LiteralString),
    LiteralBool(&'static LiteralBool),
    LiteralNil(&'static LiteralNil),
}
impl AstNode {
    pub fn as_dyn(&self) -> &dyn AstNodeWrap {
        match self {
            Self::Ast(n) => n,
            Self::ExprUnary(n) => n,
            Self::ExprBinary(n) => n,
            Self::ExprIdent(n) => n,
            Self::ExprLiteral(n) => n,
            Self::ExprIf(n) => n,
            Self::ExprIfCase(n) => n,
            Self::ExprVarDecl(n) => n,
            Self::ExprFnDecl(n) => n,
            Self::ExprEntype(n) => n,
            Self::ExprFnCall(n) => n,
            Self::ExprCurry(n) => n,
            Self::Ident(n) => n,
            Self::LiteralInt(n) => n,
            Self::LiteralString(n) => n,
            Self::LiteralBool(n) => n,
            Self::LiteralNil(n) => n,
        }
    }
}
impl fmt::Debug for AstNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.as_dyn().fmt(f)
    }
}

pub struct Ast {
    decls: Option<&'static StaticLL<Expr>>,
}
impl Ast {
    pub fn decls(&self) -> impl Iterator<Item=Expr> {
        adapt_ll(self.decls, |e| e.tail)
            .map(|n| *n.head)
    }
}
impl AstNodeWrap for &'static Ast {
    fn as_any(self) -> AstNode {
        AstNode::Ast(self)
    }
    fn node_id(&self) -> AstNodeId {
        AstNodeId(*self as *const Ast as usize)
    }
    fn push_children(&self, dst: &mut Vec<AstNode>) {
        dst.extend(self.decls().map(|d| d.as_any()));
    }
}
impl fmt::Debug for Ast {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        SExp::new(f, "program")?
            .add_all(self.decls())?
            .finish()
    }
}

pub struct Ident {
    loc: Location,
    name: &'static str,
}
impl Ident {
    pub fn name(&self) -> &str {
        self.name
    }
}
impl AstNodeWrap for &'static Ident {
    fn as_any(self) -> AstNode {
        AstNode::Ident(self)
    }
    fn node_id(&self) -> AstNodeId {
        AstNodeId(*self as *const Ident as usize)
    }
    fn push_children(&self, _: &mut Vec<AstNode>) { }
}
impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        SExp::new(f, "ident")?
            .add(&self.name())?
            .finish()
    }
}


#[derive(Copy, Clone)]
#[repr(C)]
pub enum Literal {
    String(&'static LiteralString),
    Int(&'static LiteralInt),
    Bool(&'static LiteralBool),
    Nil(&'static LiteralNil),
}
impl AstNodeWrap for Literal {
    fn node_id(&self) -> AstNodeId {
        match self {
            Literal::Int(l) => l.node_id(),
            Literal::String(l) => l.node_id(),
            Literal::Bool(l) => l.node_id(),
            Literal::Nil(l) => l.node_id(),
        }
    }
    fn as_any(self) -> AstNode {
        match self {
            Literal::Int(l) => l.as_any(),
            Literal::String(l) => l.as_any(),
            Literal::Bool(l) => l.as_any(),
            Literal::Nil(l) => l.as_any(),
        }
    }
    fn push_children(&self, dst: &mut Vec<AstNode>) {
        match self {
            Literal::Int(l) => l.push_children(dst),
            Literal::String(l) => l.push_children(dst),
            Literal::Bool(l) => l.push_children(dst),
            Literal::Nil(l) => l.push_children(dst),
        }
    }
}
impl fmt::Debug for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(e) => e.fmt(f),
            Self::Int(e) => e.fmt(f),
            Self::Bool(e) => e.fmt(f),
            Self::Nil(e) => e.fmt(f),
        }
    }
}

pub struct LiteralString {
    loc: Location,
    val: &'static str,
}
impl LiteralString {
    pub fn val(&self) -> &str {
        self.val
    }
}
impl AstNodeWrap for &'static LiteralString {
    fn as_any(self) -> AstNode {
        AstNode::LiteralString(self)
    }
    fn node_id(&self) -> AstNodeId {
        AstNodeId(*self as *const LiteralString as usize)
    }
    fn push_children(&self, _: &mut Vec<AstNode>) { }
}
impl fmt::Debug for LiteralString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        SExp::new(f, "literal")?
            .add(&self.val())?
            .finish()
    }
}

pub struct LiteralInt {
    loc: Location,
    val: u64,
}
impl LiteralInt {
    pub fn val(&self) -> u64 {
        self.val
    }
}
impl AstNodeWrap for &'static LiteralInt {
    fn as_any(self) -> AstNode {
        AstNode::LiteralInt(self)
    }
    fn node_id(&self) -> AstNodeId {
        AstNodeId(*self as *const LiteralInt as usize)
    }
    fn push_children(&self, _: &mut Vec<AstNode>) { }
}
impl fmt::Debug for LiteralInt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        SExp::new(f, "literal")?
        .add(&self.val())?
        .finish()
    }
}

pub struct LiteralBool {
    loc: Location,
    val: bool,
}
impl LiteralBool {
    pub fn val(&self) -> bool {
        self.val
    }
}
impl AstNodeWrap for &'static LiteralBool {
    fn as_any(self) -> AstNode {
        AstNode::LiteralBool(self)
    }
    fn node_id(&self) -> AstNodeId {
        AstNodeId(*self as *const LiteralBool as usize)
    }
    fn push_children(&self, _: &mut Vec<AstNode>) { }
}
impl fmt::Debug for LiteralBool {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        SExp::new(f, "literal")?
        .add(&self.val())?
        .finish()
    }
}

pub struct LiteralNil {
    loc: Location,
}
impl AstNodeWrap for &'static LiteralNil {
    fn as_any(self) -> AstNode {
        AstNode::LiteralNil(self)
    }
    fn node_id(&self) -> AstNodeId {
        AstNodeId(*self as *const LiteralNil as usize)
    }
    fn push_children(&self, _: &mut Vec<AstNode>) { }
}
impl fmt::Debug for LiteralNil {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        SExp::new(f, "literal nil")?
        .finish()
    }
}

#[derive(Copy, Clone)]
#[repr(C)]
pub enum Expr {
    Unary(&'static ExprUnary),
    Binary(&'static ExprBinary),
    Literal(&'static ExprLiteral),
    Ident(&'static ExprIdent),
    If(&'static ExprIf),
    IfCase(&'static ExprIfCase),
    VarDecl(&'static ExprVarDecl),
    FnDecl(&'static ExprFnDecl),
    Entype(&'static ExprEntype),
    FnCall(&'static ExprFnCall),
    Curry(&'static ExprCurry),
}
impl Expr {
    pub fn unwrap_ident(&self) -> &'static Ident {
        if let Expr::Ident(i) = self {
            i.ident()
        } else {
            panic!("Syntax error: decl name must be an ident");
        }
    }
}
impl AstNodeWrap for Expr {
    fn as_any(self) -> AstNode {
        match self {
            Expr::Unary(e) => e.as_any(),
            Expr::Binary(e) => e.as_any(),
            Expr::Literal(e) => e.as_any(),
            Expr::Ident(e) => e.as_any(),
            Expr::If(e) => e.as_any(),
            Expr::IfCase(e) => e.as_any(),
            Expr::VarDecl(e) => e.as_any(),
            Expr::FnDecl(e) => e.as_any(),
            Expr::Entype(e) => e.as_any(),
            Expr::FnCall(e) => e.as_any(),
            Expr::Curry(e) => e.as_any(),
        }
    }
    fn node_id(&self) -> AstNodeId {
        match self {
            Expr::Unary(e) => e.node_id(),
            Expr::Binary(e) => e.node_id(),
            Expr::Literal(e) => e.node_id(),
            Expr::Ident(e) => e.node_id(),
            Expr::If(e) => e.node_id(),
            Expr::IfCase(e) => e.node_id(),
            Expr::VarDecl(e) => e.node_id(),
            Expr::FnDecl(e) => e.node_id(),
            Expr::Entype(e) => e.node_id(),
            Expr::FnCall(e) => e.node_id(),
            Expr::Curry(e) => e.node_id(),
        }
    }
    fn push_children(&self, dst: &mut Vec<AstNode>) {
        match self {
            Expr::Unary(e) => e.push_children(dst),
            Expr::Binary(e) => e.push_children(dst),
            Expr::Literal(e) => e.push_children(dst),
            Expr::Ident(e) => e.push_children(dst),
            Expr::If(e) => e.push_children(dst),
            Expr::IfCase(e) => e.push_children(dst),
            Expr::VarDecl(e) => e.push_children(dst),
            Expr::FnDecl(e) => e.push_children(dst),
            Expr::Entype(e) => e.push_children(dst),
            Expr::FnCall(e) => e.push_children(dst),
            Expr::Curry(e) => e.push_children(dst),
        }
    }
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
#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum UnaryOp {
    Head,
    Tail,
    Negate,
}

#[repr(u32)]
#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Prepend,
    LastUnit,
    Eq,
}

pub struct ExprUnary {
    loc: Location,
    operator: UnaryOp,
    operand: Expr,
}
impl ExprUnary {
    pub fn operator(&self) -> UnaryOp {
        self.operator
    }
    pub fn operand(&self) -> Expr {
        self.operand
    }
}
impl AstNodeWrap for &'static ExprUnary {
    fn as_any(self) -> AstNode {
        AstNode::ExprUnary(self)
    }
    fn node_id(&self) -> AstNodeId {
        AstNodeId(*self as *const ExprUnary as usize)
    }
    fn push_children(&self, dst: &mut Vec<AstNode>) {
        dst.push(self.operand().as_any())
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

pub struct ExprBinary {
    loc: Location,
    operator: BinaryOp,
    left: Expr,
    right: Expr,
}
impl ExprBinary {
    pub fn operator(&self) -> BinaryOp {
        self.operator
    }
    pub fn operands(&self) -> (Expr, Expr) {
        (self.left, self.right)
    }
}
impl AstNodeWrap for &'static ExprBinary {
    fn as_any(self) -> AstNode {
        AstNode::ExprBinary(self)
    }
    fn node_id(&self) -> AstNodeId {
        AstNodeId(*self as *const ExprBinary as usize)
    }
    fn push_children(&self, dst: &mut Vec<AstNode>) {
        let (left, right) = self.operands();
        dst.push(left.as_any());
        dst.push(right.as_any());
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

pub struct ExprLiteral {
    loc: Location,
    literal: Literal
}
impl ExprLiteral {
    pub fn literal(&self) -> Literal {
        self.literal
    }
}
impl AstNodeWrap for &'static ExprLiteral {
    fn as_any(self) -> AstNode {
        AstNode::ExprLiteral(self)
    }
    fn node_id(&self) -> AstNodeId {
        AstNodeId(*self as *const ExprLiteral as usize)
    }
    fn push_children(&self, dst: &mut Vec<AstNode>) {
        dst.push(self.literal().as_any())
    }
}
impl fmt::Debug for ExprLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        SExp::new(f, "expr literal")?
            .add(&self.literal())?
            .finish()
    }
}

pub struct ExprIdent {
    loc: Location,
    ident: &'static Ident
}
impl ExprIdent {
    pub fn ident(&self) -> &'static Ident {
        self.ident
    }
}
impl AstNodeWrap for &'static ExprIdent {
    fn as_any(self) -> AstNode {
        AstNode::ExprIdent(self)
    }
    fn node_id(&self) -> AstNodeId {
        AstNodeId(*self as *const ExprIdent as usize)
    }
    fn push_children(&self, dst: &mut Vec<AstNode>) {
        dst.push(self.ident().as_any())
    }
}
impl fmt::Debug for ExprIdent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        SExp::new(f, "expr ident")?
            .add(&self.ident())?
            .finish()
    }
}

pub struct ExprIf {
    loc: Location,
    cond: Expr,
    then_expr: Expr,
    else_expr: Expr
}
impl ExprIf {
    pub fn cond(&self) -> Expr {
        self.cond
    }
    pub fn then_expr(&self) -> Expr {
        self.then_expr
    }
    pub fn else_expr(&self) -> Expr {
        self.else_expr
    }
}
impl AstNodeWrap for &'static ExprIf {
    fn as_any(self) -> AstNode {
        AstNode::ExprIf(self)
    }
    fn node_id(&self) -> AstNodeId {
        AstNodeId(*self as *const ExprIf as usize)
    }
    fn push_children(&self, dst: &mut Vec<AstNode>) {
        dst.push(self.cond().as_any());
        dst.push(self.then_expr().as_any());
        dst.push(self.else_expr().as_any());
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
    OnVal(Literal, Expr),
    Else(Expr)
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

pub struct ExprIfCase {
    loc: Location,
    cond: Expr,
    cases: Option<&'static StaticLL<IfCase>>,
}
impl ExprIfCase {
    pub fn cond(&self) -> Expr {
        self.cond
    }

    pub fn cases(&self) -> impl Iterator<Item=&'static IfCase> {
        adapt_ll(self.cases, |e| e.tail)
            .map(|n| n.head)
    }
}
impl AstNodeWrap for &'static ExprIfCase {
    fn as_any(self) -> AstNode {
        AstNode::ExprIfCase(self)
    }
    fn node_id(&self) -> AstNodeId {
        AstNodeId(*self as *const ExprIfCase as usize)
    }
    fn push_children(&self, dst: &mut Vec<AstNode>) {
        dst.push(self.cond().as_any());
        for c in self.cases() {
            match c {
                IfCase::OnVal(l, v) => {
                    dst.push(l.as_any());
                    dst.push(v.as_any());
                }
                IfCase::Else(v) => {
                    dst.push(v.as_any());
                }
            }
        }
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

pub struct ExprEntype {
    loc: Location,
    target: Expr,
    ty: Expr
}
impl ExprEntype {
    pub fn target(&self) -> &'static Ident {
        self.target.unwrap_ident()
    }

    pub fn ty(&self) -> Expr {
        self.ty
    }
}
impl AstNodeWrap for &'static ExprEntype {
    fn as_any(self) -> AstNode {
        AstNode::ExprEntype(self)
    }
    fn node_id(&self) -> AstNodeId {
        AstNodeId(*self as *const ExprEntype as usize)
    }
    fn push_children(&self, dst: &mut Vec<AstNode>) {
        dst.push(self.target().as_any());
        dst.push(self.ty().as_any());
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

pub struct ExprVarDecl {
    loc: Location,
    name: Expr,
    val: Expr
}
impl ExprVarDecl {
    pub fn name(&self) -> &'static Ident {
        self.name.unwrap_ident()
    }

    pub fn val(&self) -> Expr {
        self.val
    }
}
impl AstNodeWrap for &'static ExprVarDecl {
    fn as_any(self) -> AstNode {
        AstNode::ExprVarDecl(self)
    }
    fn node_id(&self) -> AstNodeId {
        AstNodeId(*self as *const ExprVarDecl as usize)
    }
    fn push_children(&self, dst: &mut Vec<AstNode>) {
        dst.push(self.name().as_any());
        dst.push(self.val().as_any());
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

pub struct ExprFnDecl {
    loc: Location,
    name: Option<Expr>,
    val: Expr,
    args: Option<&'static StaticLL<Expr>>,
    pure: bool
}
impl ExprFnDecl {
    pub fn name(&self) -> Option<&'static Ident> {
        self.name
            .map(|e| e.unwrap_ident())
    }
    pub fn val(&self) -> Expr {
        self.val
    }
    pub fn args(&self) -> impl Iterator<Item=&'static Ident> {
        adapt_ll(self.args, |e| e.tail)
            .map(|e| e.head.unwrap_ident())
    }

    pub fn pure(&self) -> bool {
        self.pure
    }
}
impl AstNodeWrap for &'static ExprFnDecl {
    fn as_any(self) -> AstNode {
        AstNode::ExprFnDecl(self)
    }
    fn node_id(&self) -> AstNodeId {
        AstNodeId(*self as *const ExprFnDecl as usize)
    }
    fn push_children(&self, dst: &mut Vec<AstNode>) {
        if let Some(name) = self.name() {
            dst.push(name.as_any());
        }
        dst.extend(self.args().map(|a| a.as_any()));
        dst.push(self.val().as_any());
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

pub struct ExprFnCall {
    loc: Location,
    callee: Expr,
    args: Option<&'static StaticLL<Expr>>,
    pure: bool,
}
impl ExprFnCall {
    pub fn callee(&self) -> Expr {
        self.callee
    }

    pub fn args(&self) -> impl Iterator<Item=Expr> {
        adapt_ll(self.args, |e| e.tail)
            .map(|n| *n.head)
    }

    pub fn pure(&self) -> bool {
        self.pure
    }
}
impl AstNodeWrap for &'static ExprFnCall {
    fn as_any(self) -> AstNode {
        AstNode::ExprFnCall(self)
    }
    fn node_id(&self) -> AstNodeId {
        AstNodeId(*self as *const ExprFnCall as usize)
    }
    fn push_children(&self, dst: &mut Vec<AstNode>) {
        dst.push(self.callee().as_any());
        dst.extend(self.args().map(|a| a.as_any()));
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

pub struct ExprCurry {
    loc: Location
}
impl AstNodeWrap for &'static ExprCurry {
    fn as_any(self) -> AstNode {
        AstNode::ExprCurry(self)
    }
    fn node_id(&self) -> AstNodeId {
        AstNodeId(*self as *const ExprCurry as usize)
    }
    fn push_children(&self, _: &mut Vec<AstNode>) { }
}
impl fmt::Debug for ExprCurry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        SExp::new(f, "curry")?.finish()
    }
}


use std::os::raw::c_char;

fn static_ref<T: 'static>(val: T) -> &'static T {
    Box::leak(Box::new(val))
}

#[repr(C)]
struct StaticLL<T: 'static> {
    head: &'static T,
    tail: Option<&'static StaticLL<T>>
}

pub struct ExprLL(StaticLL<Expr>);
pub struct CaseLL(StaticLL<IfCase>);

#[no_mangle]
pub extern fn ast_create(decls: Option<&'static ExprLL>) -> &'static Ast {
    static_ref(Ast { decls: decls.map(|e| &e.0) })
}

#[no_mangle]
pub extern fn ast_add_expr_literal(loc: Location, literal: Literal) -> Expr {
    Expr::Literal(
        static_ref(ExprLiteral {
            loc, literal
        })
    )
}

#[no_mangle]
pub extern fn ast_add_expr_ident(loc: Location, ident: &'static Ident) -> Expr {
    Expr::Ident(
        static_ref(ExprIdent {
            loc, ident
        })
    )
}

#[no_mangle]
pub extern fn ast_add_expr_unary(loc: Location, operator: UnaryOp, operand: Expr) -> Expr {
    Expr::Unary(
        static_ref(ExprUnary {
            loc, operator, operand
        })
    )
}

#[no_mangle]
pub extern fn ast_add_expr_binary(loc: Location, left: Expr, operator: BinaryOp, right: Expr) -> Expr {
    Expr::Binary(
        static_ref(ExprBinary {
            loc, left, operator, right
        })
    )
}

#[no_mangle]
pub extern fn ast_add_expr_if(loc: Location, cond: Expr, then_expr: Expr, else_expr: Expr) -> Expr {
    Expr::If(
        static_ref(ExprIf {
            loc, cond, then_expr, else_expr
        })
    )
}

#[no_mangle]
pub extern fn ast_add_expr_if_case(loc: Location, cond: Expr, cases: Option<&'static CaseLL>) -> Expr {
    let cases = cases.map(|e| &e.0);
    Expr::IfCase(
        static_ref(ExprIfCase {
            loc, cond, cases
        })
    )
}

#[no_mangle]
pub extern fn ast_add_expr_var_decl(loc: Location, name: Expr, val: Expr) -> Expr {
    Expr::VarDecl(
        static_ref(ExprVarDecl {
            loc, name, val
        })
    )
}

#[no_mangle]
pub extern fn ast_add_expr_entype(loc: Location, target: Expr, ty: Expr) -> Expr {
    Expr::Entype(
        static_ref(ExprEntype {
            loc, target, ty
        })
    )
}

#[no_mangle]
pub extern fn ast_add_expr_fn_decl(loc: Location, pure: bool, name: Option<&Expr>, args: Option<&'static ExprLL>, val: Expr) -> Expr {
    let args = args.map(|e| &e.0);
    Expr::FnDecl(
        static_ref(ExprFnDecl {
            loc, pure, name: name.copied(), args, val
        })
    )
}

#[no_mangle]
pub extern fn ast_add_expr_fn_call(loc: Location, pure: bool, callee: Expr, args: Option<&'static ExprLL>) -> Expr {
    let args = args.map(|e| &e.0);
    Expr::FnCall(
        static_ref(ExprFnCall {
            loc, pure, callee, args
        })
    )
}

#[no_mangle]
pub extern fn ast_add_expr_curry(loc: Location, ) -> Expr {
    Expr::Curry(
        static_ref(ExprCurry { loc })
    )
}

#[no_mangle]
pub extern fn ast_add_literal_int(loc: Location, val: u64) -> Literal {
    Literal::Int(
        static_ref(LiteralInt { loc, val })
    )
}

#[no_mangle]
pub extern fn ast_add_literal_str(loc: Location, s: *const c_char) -> Literal {
    let s = unsafe { ffi::CStr::from_ptr(s) }
        .to_str()
        .unwrap();
    Literal::String(
        static_ref(LiteralString { loc, val: s })
    )
}

#[no_mangle]
pub extern fn ast_add_literal_bool(loc: Location, b: bool) -> Literal {
    Literal::Bool(
        static_ref(LiteralBool { loc, val: b })
    )
}

#[no_mangle]
pub extern fn ast_add_literal_nil(loc: Location) -> Literal {
    Literal::Nil(
        static_ref(LiteralNil { loc })
    )
}

#[no_mangle]
pub extern fn ast_add_ident(loc: Location, name: *const c_char) -> &'static Ident {
    let name = unsafe { ffi::CStr::from_ptr(name) }
        .to_str()
        .unwrap();
    static_ref(Ident { loc, name })
}

#[no_mangle]
pub extern fn ast_prepend_expr_list(item: Expr, list: Option<&'static ExprLL>) -> &'static ExprLL {
    static_ref(ExprLL(StaticLL {
        head: static_ref(item),
        tail: list.map(|e| &e.0)
    }))
}

#[no_mangle]
pub extern fn ast_prepend_case_list(case: Option<&Literal>, then: Expr, list: Option<&'static CaseLL>) -> &'static CaseLL {
    static_ref(CaseLL(StaticLL {
        head: if let Some(case) = case {
            static_ref(IfCase::OnVal(*case, then))
        } else {
            static_ref(IfCase::Else(then))
        },
        tail: list.map(|e| &e.0)
    }))
}

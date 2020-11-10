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
impl fmt::Debug for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "input:{}:{} .. {}:{}", self.lline, self.lcol, self.rline, self.rcol)
    }
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
    fn loc(&self) -> &Location;
    fn push_children(&self, dst: &mut Vec<AstNode>);
}

pub struct Ast {
    decls: Option<&'static StaticLL<Expr>>,
}
impl Ast {
    pub fn decls(&self) -> impl Iterator<Item=&'static Expr> {
        adapt_ll(self.decls, |e| e.tail)
            .map(|n| n.head)
    }
}
impl AstNodeWrap for &'static Ast {
    fn as_any(self) -> AstNode {
        AstNode::Ast(self)
    }
    fn node_id(&self) -> AstNodeId {
        AstNodeId(*self as *const Ast as usize)
    }
    fn loc(&self) -> &Location {
        unimplemented!()
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


macro_rules! node_select {
    ($name:ident, [ $( $elem:ident($ty:ty) ),* ]) => {
        #[derive(Copy, Clone)]
        #[repr(C)]
        pub enum $name {
            $( $elem($ty), )*
        }
        impl AstNodeWrap for $name {
            fn node_id(&self) -> AstNodeId {
                match self {
                    $( Self::$elem(e) => e.node_id(), )*
                }
            }
            fn as_any(self) -> AstNode {
                match self {
                    $( Self::$elem(e) => e.as_any(), )*
                }
            }
            fn loc(&self) -> &Location {
                match self {
                    $( Self::$elem(e) => e.loc(), )*
                }
            }
            fn push_children(&self, dst: &mut Vec<AstNode>) {
                match self {
                    $( Self::$elem(e) => e.push_children(dst), )*
                }
            }
        }
        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                match self {
                    $( Self::$elem(e) => e.fmt(f), )*
                }
            }
        }
    };
}

node_select!(AstNode, [
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
    LiteralNil(&'static LiteralNil)
]);

node_select!(Literal, [
    String(&'static LiteralString),
    Int(&'static LiteralInt),
    Bool(&'static LiteralBool),
    Nil(&'static LiteralNil)
]);

node_select!(Expr, [
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
    Curry(&'static ExprCurry)
]);
impl Expr {
    pub fn unwrap_ident(&self) -> &'static Ident {
        if let Expr::Ident(i) = self {
            i.ident()
        } else {
            panic!("Syntax error: decl name must be an ident");
        }
    }
}


macro_rules! node {
    ($name:ident, $str:expr, { $($inner:ident: $in_ty:ty),* }, [ $($child:ident: $ty:ty),* ]) => {
        pub struct $name {
            loc: Location,
            $( $inner : $in_ty, )*
            $( $child : $ty, )*
        }
        impl $name {
            $( pub fn $child(&self) -> $ty { self.$child } )*
            $( pub fn $inner(&self) -> $in_ty { self.$inner } )*
        }
        impl AstNodeWrap for &'static $name {
            fn as_any(self) -> AstNode {
                AstNode::$name(self)
            }
            fn node_id(&self) -> AstNodeId {
                AstNodeId(*self as *const $name as usize)
            }
            fn loc(&self) -> &Location {
                &self.loc
            }
            fn push_children(&self, _dst: &mut Vec<AstNode>) {
                $( _dst.push(self.$child().as_any()); )*
            }
        }
        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                SExp::new(f, $str)?
                    $( .add(&self.$inner())? )*
                    $( .add(&self.$child())? )*
                    .finish()
            }
        }
    };
}

node!(Ident, "ident", { name: &'static str }, []);
node!(LiteralString, "literal",
    { val: &'static str }, []);
node!(LiteralInt, "literal",
    { val: u64 }, []);
node!(LiteralBool, "literal",
    { val: bool }, []);
node!(LiteralNil, "literal nil", {}, []);

node!(ExprCurry, "curry", {}, []);
node!(ExprLiteral, "expr literal",
    {}, [ literal: &'static Literal ]);
node!(ExprIdent, "expr ident",
    {}, [ ident: &'static Ident ]);
node!(ExprIf, "expr if",
    {}, [ cond: &'static Expr, then_expr: &'static Expr, else_expr: &'static Expr ]);
node!(ExprEntype, "expr entype",
    {}, [ target: &'static Ident, ty: &'static Expr ]);
node!(ExprVarDecl, "expr var_decl",
    {}, [ name: &'static Ident, val: &'static Expr ]);
node!(ExprUnary, "expr unary",
    { operator: UnaryOp },
    [ operand: &'static Expr ]);
node!(ExprBinary, "expr binary",
    { operator: BinaryOp },
    [ left: &'static Expr, right: &'static Expr ]);
impl ExprBinary {
    pub fn operands(&self) -> (&'static Expr, &'static Expr) {
        (self.left, self.right)
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

pub enum IfCase {
    OnVal(&'static Literal, &'static Expr),
    Else(&'static Expr)
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
    cond: &'static Expr,
    cases: Option<&'static StaticLL<IfCase>>,
}
impl ExprIfCase {
    pub fn cond(&self) -> &'static Expr {
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
    fn loc(&self) -> &Location {
        &self.loc
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

pub struct ExprFnDecl {
    loc: Location,
    name: Option<&'static Expr>,
    val: &'static Expr,
    args: Option<&'static StaticLL<Expr>>,
    pure: bool
}
impl ExprFnDecl {
    pub fn name(&self) -> Option<&'static Ident> {
        self.name
            .map(|e| e.unwrap_ident())
    }
    pub fn val(&self) -> &'static Expr {
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
    fn loc(&self) -> &Location {
        &self.loc
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
    callee: &'static Expr,
    args: Option<&'static StaticLL<Expr>>,
    pure: bool,
}
impl ExprFnCall {
    pub fn callee(&self) -> &'static Expr {
        self.callee
    }
    pub fn args(&self) -> impl Iterator<Item=&'static Expr> {
        adapt_ll(self.args, |e| e.tail)
            .map(|n| n.head)
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
    fn loc(&self) -> &Location {
        &self.loc
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
pub extern fn ast_add_expr_literal(loc: Location, literal: &'static Literal) -> &'static Expr {
    static_ref(Expr::Literal(
        static_ref(ExprLiteral {
            loc, literal
        })
    ))
}

#[no_mangle]
pub extern fn ast_add_expr_ident(loc: Location, ident: &'static Ident) -> &'static Expr {
    static_ref(Expr::Ident(
        static_ref(ExprIdent {
            loc, ident
        })
    ))
}

#[no_mangle]
pub extern fn ast_add_expr_unary(loc: Location, operator: UnaryOp, operand: &'static Expr) -> &'static Expr {
    static_ref(Expr::Unary(
        static_ref(ExprUnary {
            loc, operator, operand
        })
    ))
}

#[no_mangle]
pub extern fn ast_add_expr_binary(loc: Location, left: &'static Expr, operator: BinaryOp, right: &'static Expr) -> &'static Expr {
    static_ref(Expr::Binary(
        static_ref(ExprBinary {
            loc, left, operator, right
        })
    ))
}

#[no_mangle]
pub extern fn ast_add_expr_if(loc: Location, cond: &'static Expr, then_expr: &'static Expr, else_expr: &'static Expr) -> &'static Expr {
    static_ref(Expr::If(
        static_ref(ExprIf {
            loc, cond, then_expr, else_expr
        })
    ))
}

#[no_mangle]
pub extern fn ast_add_expr_if_case(loc: Location, cond: &'static Expr, cases: Option<&'static CaseLL>) -> &'static Expr {
    let cases = cases.map(|e| &e.0);
    static_ref(Expr::IfCase(
        static_ref(ExprIfCase {
            loc, cond, cases
        })
    ))
}

#[no_mangle]
pub extern fn ast_add_expr_var_decl(loc: Location, name: &'static Expr, val: &'static Expr) -> &'static Expr {
    static_ref(Expr::VarDecl(
        static_ref(ExprVarDecl {
            loc, name: name.unwrap_ident(), val
        })
    ))
}

#[no_mangle]
pub extern fn ast_add_expr_entype(loc: Location, target: &'static Expr, ty: &'static Expr) -> &'static Expr {
    static_ref(Expr::Entype(
        static_ref(ExprEntype {
            loc, target: target.unwrap_ident(), ty
        })
    ))
}

#[no_mangle]
pub extern fn ast_add_expr_fn_decl(loc: Location, pure: bool, name: Option<&'static Expr>, args: Option<&'static ExprLL>, val: &'static Expr) -> &'static Expr {
    let args = args.map(|e| &e.0);
    static_ref(Expr::FnDecl(
        static_ref(ExprFnDecl {
            loc, pure, name, args, val
        })
    ))
}

#[no_mangle]
pub extern fn ast_add_expr_fn_call(loc: Location, pure: bool, callee: &'static Expr, args: Option<&'static ExprLL>) -> &'static Expr {
    let args = args.map(|e| &e.0);
    static_ref(Expr::FnCall(
        static_ref(ExprFnCall {
            loc, pure, callee, args
        })
    ))
}

#[no_mangle]
pub extern fn ast_add_expr_curry(loc: Location) -> &'static Expr {
    static_ref(Expr::Curry(
        static_ref(ExprCurry { loc })
    ))
}

#[no_mangle]
pub extern fn ast_add_literal_int(loc: Location, val: u64) -> &'static Literal {
    static_ref(Literal::Int(
        static_ref(LiteralInt { loc, val })
    ))
}

#[no_mangle]
pub extern fn ast_add_literal_str(loc: Location, s: *const c_char) -> &'static Literal {
    let s = unsafe { ffi::CStr::from_ptr(s) }
        .to_str()
        .unwrap();
    static_ref(Literal::String(
        static_ref(LiteralString { loc, val: s })
    ))
}

#[no_mangle]
pub extern fn ast_add_literal_bool(loc: Location, b: bool) -> &'static Literal {
    static_ref(Literal::Bool(
        static_ref(LiteralBool { loc, val: b })
    ))
}

#[no_mangle]
pub extern fn ast_add_literal_nil(loc: Location) -> &'static Literal {
    static_ref(Literal::Nil(
        static_ref(LiteralNil { loc })
    ))
}

#[no_mangle]
pub extern fn ast_add_ident(loc: Location, name: *const c_char) -> &'static Ident {
    let name = unsafe { ffi::CStr::from_ptr(name) }
        .to_str()
        .unwrap();
    static_ref(Ident { loc, name })
}

#[no_mangle]
pub extern fn ast_prepend_expr_list(item: &'static Expr, list: Option<&'static ExprLL>) -> &'static ExprLL {
    static_ref(ExprLL(StaticLL {
        head: item,
        tail: list.map(|e| &e.0)
    }))
}

#[no_mangle]
pub extern fn ast_prepend_case_list(case: Option<&'static Literal>, then: &'static Expr, list: Option<&'static CaseLL>) -> &'static CaseLL {
    static_ref(CaseLL(StaticLL {
        head: if let Some(case) = case {
            static_ref(IfCase::OnVal(case, then))
        } else {
            static_ref(IfCase::Else(then))
        },
        tail: list.map(|e| &e.0)
    }))
}

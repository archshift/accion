use core::panic;
use std::fmt;
use std::ffi;
use std::rc::Rc;
use std::fmt::Debug;

use crate::lliter;
use crate::sexp::SExp;

#[repr(C)]
#[derive(Copy, Clone)]
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
    fn loc(&self) -> &Location;
    fn as_any(&self) -> AstNode;
    fn push_children(&self, dst: &mut Vec<AstNode>);
}

pub struct Ast {
    decls: StaticLL<Box<Expr>>,
}
impl Ast {
    pub fn decls(&self) -> impl Iterator<Item=&Expr> {
        self.decls.iter()
            .map(|e| e.as_ref())
    }
}
impl AstNodeWrap for Rc<Ast> {
    fn as_any(&self) -> AstNode {
        AstNode::Ast(self.clone())
    }
    fn node_id(&self) -> AstNodeId {
        AstNodeId(self.as_ref() as *const Ast as usize)
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
    ($name:ident, [ $( $elem:ident($ty:ty) ),* $(,)? ]) => {
        #[repr(C)]
        #[derive(Clone)]
        pub enum $name {
            $( $elem($ty), )*
        }
        impl AstNodeWrap for $name {
            fn node_id(&self) -> AstNodeId {
                match self {
                    $( $name::$elem(e) => e.node_id(), )*
                }
            }
            fn as_any(&self) -> AstNode {
                match self {
                    $( Self::$elem(e) => e.as_any(), )*
                }
            }
            fn loc(&self) -> &Location {
                match self {
                    $( $name::$elem(e) => { e.loc() } )*
                }
            }
            fn push_children<'_a>(&'_a self, dst: &mut Vec<AstNode>) {
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
    Ast(Rc<Ast>),
    ExprUnary(Rc<ExprUnary>),
    ExprBinary(Rc<ExprBinary>),
    ExprIdent(Rc<ExprIdent>),
    ExprLiteral(Rc<ExprLiteral>),
    ExprIf(Rc<ExprIf>),
    ExprIfCase(Rc<ExprIfCase>),
    ExprVarDecl(Rc<ExprVarDecl>),
    ExprFnDecl(Rc<ExprFnDecl>),
    ExprEntype(Rc<ExprEntype>),
    ExprFnCall(Rc<ExprFnCall>),
    ExprCurry(Rc<ExprCurry>),
    Ident(Rc<Ident>),
    LiteralInt(Rc<LiteralInt>),
    LiteralString(Rc<LiteralString>),
    LiteralBool(Rc<LiteralBool>),
    LiteralNil(Rc<LiteralNil>),
]);

node_select!(Literal, [
    String(Rc<LiteralString>),
    Int(Rc<LiteralInt>),
    Bool(Rc<LiteralBool>),
    Nil(Rc<LiteralNil>),
]);

node_select!(Expr, [
    Unary(Rc<ExprUnary>),
    Binary(Rc<ExprBinary>),
    Literal(Rc<ExprLiteral>),
    Ident(Rc<ExprIdent>),
    If(Rc<ExprIf>),
    IfCase(Rc<ExprIfCase>),
    VarDecl(Rc<ExprVarDecl>),
    FnDecl(Rc<ExprFnDecl>),
    Entype(Rc<ExprEntype>),
    FnCall(Rc<ExprFnCall>),
    Curry(Rc<ExprCurry>),
]);
impl Expr {
    pub fn unwrap_ident(&self) -> &Rc<Ident> {
        if let Expr::Ident(i) = self {
            i.ident()
        } else {
            panic!("Syntax error: decl name must be an ident");
        }
    }
}


macro_rules! node {
    ($name:ident, $str:expr, { $($inner:ident: $in_ty:ty),* }, [ $($child:ident: $ty:ty),* ]) => {
        #[derive(Clone)]
        pub struct $name {
            loc: Location,
            $( $inner : $in_ty, )*
            $( $child : $ty, )*
        }
        impl $name {
            $( pub fn $child(&self) -> &$ty { &self.$child } )*
            $( pub fn $inner(&self) -> &$in_ty { &self.$inner } )*
        }
        impl AstNodeWrap for Rc<$name> {
            fn as_any(&self) -> AstNode {
                AstNode::$name(self.clone())
            }
            fn node_id(&self) -> AstNodeId {
                AstNodeId(self.as_ref() as *const $name as usize)
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

node!(LiteralInt, "literal",
    { val: u64 }, []);
node!(LiteralBool, "literal",
    { val: bool }, []);
node!(LiteralNil, "literal nil", {}, []);
node!(LiteralString, "literal",
    { _val: ffi::CString }, []);
impl LiteralString {
    pub fn val(&self) -> &str {
        self._val.to_str().unwrap()
    }
}

node!(Ident, "ident", { _name: ffi::CString }, []);
impl Ident {
    pub fn name(&self) -> &str {
        self._name.to_str().unwrap()
    }
}

node!(ExprCurry, "curry", {}, []);
node!(ExprLiteral, "expr literal",
    {}, [ literal: Literal ]);
node!(ExprIdent, "expr ident",
    {}, [ ident: Rc<Ident> ]);
node!(ExprIf, "expr if",
    {}, [ cond: Expr, then_expr: Expr, else_expr: Expr ]);
node!(ExprEntype, "expr entype",
    {}, [ target: Rc<Ident>, ty: Expr ]);
node!(ExprVarDecl, "expr var_decl",
    {}, [ name: Rc<Ident>, val: Expr ]);
node!(ExprUnary, "expr unary",
    { operator: UnaryOp },
    [ operand: Expr ]);
node!(ExprBinary, "expr binary",
    { operator: BinaryOp },
    [ left: Expr, right: Expr ]);
impl ExprBinary {
    pub fn operands(&self) -> (&Expr, &Expr) {
        (&self.left, &self.right)
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

#[derive(Clone)]
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

#[derive(Clone)]
pub struct ExprIfCase {
    loc: Location,
    cond: Expr,
    cases: StaticLL<IfCase>,
}
impl ExprIfCase {
    pub fn cond(&self) -> &Expr {
        &self.cond
    }
    pub fn cases(&self) -> impl Iterator<Item=&IfCase> {
        self.cases.iter()
    }
}
impl AstNodeWrap for Rc<ExprIfCase> {
    fn as_any(&self) -> AstNode {
        AstNode::ExprIfCase(self.clone())
    }
    fn node_id(&self) -> AstNodeId {
        AstNodeId(self.as_ref() as *const ExprIfCase as usize)
    }
    fn loc(&self) -> &Location {
        &self.loc
    }
    fn push_children<'a>(&'a self, dst: &mut Vec<AstNode>) {
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

#[derive(Clone)]
pub struct ExprFnDecl {
    loc: Location,
    name: Option<Expr>,
    val: Expr,
    args: StaticLL<Box<Expr>>,
    pure: bool
}
impl ExprFnDecl {
    pub fn name(&self) -> Option<&Rc<Ident>> {
        self.name.as_ref()
            .map(|e| e.unwrap_ident())
    }
    pub fn val(&self) -> &Expr {
        &self.val
    }
    pub fn args(&self) -> impl Iterator<Item=&Rc<Ident>> {
        self.args.iter()
            .map(|e| e.unwrap_ident())
    }
    pub fn pure(&self) -> bool {
        self.pure
    }
}
impl AstNodeWrap for Rc<ExprFnDecl> {
    fn as_any(&self) -> AstNode {
        AstNode::ExprFnDecl(self.clone())
    }
    fn node_id(&self) -> AstNodeId {
        AstNodeId(self.as_ref() as *const ExprFnDecl as usize)
    }
    fn loc(&self) -> &Location {
        &self.loc
    }
    fn push_children<'a>(&'a self, dst: &mut Vec<AstNode>) {
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

#[derive(Clone)]
pub struct ExprFnCall {
    loc: Location,
    callee: Expr,
    args: StaticLL<Box<Expr>>,
    pure: bool,
}
impl ExprFnCall {
    pub fn callee(&self) -> &Expr {
        &self.callee
    }
    pub fn args(&self) -> impl Iterator<Item=&Expr> {
        self.args.iter()
            .map(|e| e.as_ref())
    }
    pub fn pure(&self) -> bool {
        self.pure
    }
}
impl AstNodeWrap for Rc<ExprFnCall> {
    fn as_any(&self) -> AstNode {
        AstNode::ExprFnCall(self.clone())
    }
    fn node_id(&self) -> AstNodeId {
        AstNodeId(self.as_ref() as *const ExprFnCall as usize)
    }
    fn loc(&self) -> &Location {
        &self.loc
    }
    fn push_children<'a>(&'a self, dst: &mut Vec<AstNode>) {
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

fn static_ref<T: 'static>(val: T) -> *const T {
    Rc::into_raw(Rc::new(val))
}
fn static_mut<T: 'static>(val: T) -> *mut T {
    Box::into_raw(Box::new(val))
}

#[derive(Clone)]
struct StaticLLNode<T> {
    head: T,
    tail: StaticLL<T>
}

#[derive(Clone)]
struct StaticLL<T>(Option<Box<StaticLLNode<T>>>);
impl<T> StaticLL<T> {
    fn iter(&self) -> impl Iterator<Item=&T> {
        lliter::adapt_ll(self.0.as_ref(), |h| h.tail.0.as_ref())
            .map(|i| &i.head)
    }
}

#[repr(transparent)]
pub struct ExprLLNode(StaticLLNode<Box<Expr>>);
#[repr(transparent)]
pub struct CaseLL(StaticLLNode<IfCase>);

#[no_mangle]
pub unsafe extern fn ast_create(decls: Option<&'static mut ExprLLNode>) -> *const Ast {
    let ll = StaticLL(decls.map(|e| Box::from_raw(&mut e.0)));
    static_ref(Ast { decls: ll })
}

#[no_mangle]
pub unsafe extern fn ast_add_expr_literal(loc: Location, literal: &'static mut Literal) -> *mut Expr {
    static_mut(Expr::Literal(
        Rc::new(ExprLiteral {
            loc,
            literal: *Box::from_raw(literal)
        })
    ))
}

#[no_mangle]
pub unsafe extern fn ast_add_expr_ident(loc: Location, ident: &'static Ident) -> *mut Expr {
    static_mut(Expr::Ident(
        Rc::new(ExprIdent {
            loc, ident: Rc::from_raw(ident)
        })
    ))
}

#[no_mangle]
pub unsafe extern fn ast_add_expr_unary(loc: Location, operator: UnaryOp, operand: &'static mut Expr) -> *mut Expr {
    static_mut(Expr::Unary(
        Rc::new(ExprUnary {
            loc,
            operator,
            operand: *Box::from_raw(operand)
        })
    ))
}

#[no_mangle]
pub unsafe extern fn ast_add_expr_binary(loc: Location, left: &'static mut Expr, operator: BinaryOp, right: &'static mut Expr) -> *mut Expr {
    static_mut(Expr::Binary(
        Rc::new(ExprBinary {
            loc,
            left: *Box::from_raw(left),
            operator,
            right: *Box::from_raw(right)
        })
    ))
}

#[no_mangle]
pub unsafe extern fn ast_add_expr_if(loc: Location, cond: &'static mut Expr, then_expr: &'static mut Expr, else_expr: &'static mut Expr) -> *mut Expr {
    static_mut(Expr::If(
        Rc::new(ExprIf {
            loc,
            cond: *Box::from_raw(cond),
            then_expr: *Box::from_raw(then_expr),
            else_expr: *Box::from_raw(else_expr)
        })
    ))
}

#[no_mangle]
pub unsafe extern fn ast_add_expr_if_case(loc: Location, cond: &'static mut Expr, cases: Option<&'static mut CaseLL>) -> *mut Expr {
    let cases = cases.map(|e| Box::from_raw(&mut e.0));
    static_mut(Expr::IfCase(
        Rc::new(ExprIfCase {
            loc,
            cond: *Box::from_raw(cond),
            cases: StaticLL(cases)
        })
    ))
}

#[no_mangle]
pub unsafe extern fn ast_add_expr_var_decl(loc: Location, name: &'static mut Expr, val: &'static mut Expr) -> *mut Expr {
    let name = Box::from_raw(name);
    let name = name.unwrap_ident().clone();
    static_mut(Expr::VarDecl(
        Rc::new(ExprVarDecl {
            loc,
            name,
            val: *Box::from_raw(val)
        })
    ))
}

#[no_mangle]
pub unsafe extern fn ast_add_expr_entype(loc: Location, target: &'static mut Expr, ty: &'static mut Expr) -> *mut Expr {
    let target = Box::from_raw(target);
    let target = target.unwrap_ident().clone();
    static_mut(Expr::Entype(
         Rc::new(ExprEntype {
            loc,
            target,
            ty: *Box::from_raw(ty)
        })
    ))
}

#[no_mangle]
pub unsafe extern fn ast_add_expr_fn_decl(loc: Location, pure: bool, name: Option<&'static mut Expr>, args: Option<&'static mut ExprLLNode>, val: &'static mut Expr) -> *mut Expr {
    let args = args.map(|e| Box::from_raw(&mut e.0));
    static_mut(Expr::FnDecl(
        Rc::new(ExprFnDecl {
            loc,
            pure,
            name: name.map(|n| *Box::from_raw(n)),
            args: StaticLL(args),
            val: *Box::from_raw(val)
        })
    ))
}

#[no_mangle]
pub unsafe extern fn ast_add_expr_fn_call(loc: Location, pure: bool, callee: &'static mut Expr, args: Option<&'static mut ExprLLNode>) -> *mut Expr {
    let args = args.map(|e| Box::from_raw(&mut e.0));
    static_mut(Expr::FnCall(
        Rc::new(ExprFnCall {
            loc,
            pure,
            callee: *Box::from_raw(callee),
            args: StaticLL(args)
        })
    ))
}

#[no_mangle]
pub extern fn ast_add_expr_curry(loc: Location) -> *mut Expr {
    static_mut(Expr::Curry(
        Rc::new(ExprCurry { loc })
    ))
}

#[no_mangle]
pub extern fn ast_add_literal_int(loc: Location, val: u64) -> *mut Literal {
    static_mut(Literal::Int(
        Rc::new(LiteralInt { loc, val })
    ))
}

#[no_mangle]
pub extern fn ast_strdup(s: *const c_char) -> *mut c_char {
    unsafe { ffi::CStr::from_ptr(s) }
        .to_owned()
        .into_raw()
}

#[no_mangle]
pub extern fn ast_add_literal_str(loc: Location, s: *mut c_char) -> *mut Literal {
    let s = unsafe { ffi::CString::from_raw(s) };
    static_mut(Literal::String(
        Rc::new(LiteralString { loc, _val: s })
    ))
}

#[no_mangle]
pub extern fn ast_add_literal_bool(loc: Location, b: bool) -> *mut Literal {
    static_mut(Literal::Bool(
        Rc::new(LiteralBool { loc, val: b })
    ))
}

#[no_mangle]
pub extern fn ast_add_literal_nil(loc: Location) -> *mut Literal {
    static_mut(Literal::Nil(
        Rc::new(LiteralNil { loc })
    ))
}

#[no_mangle]
pub extern fn ast_add_ident(loc: Location, name: *mut c_char) -> *const Ident {
    let name = unsafe { ffi::CString::from_raw(name) };
    static_ref(Ident { loc, _name: name })
}

#[no_mangle]
pub unsafe extern fn ast_prepend_expr_list(item: &'static mut Expr, list: Option<&'static mut ExprLLNode>) -> *mut ExprLLNode {
    let item = Box::from_raw(item);
    static_mut(ExprLLNode(StaticLLNode {
        head: item,
        tail: StaticLL(list.map(|e| Box::from_raw(&mut e.0)))
    }))
}

#[no_mangle]
pub unsafe extern fn ast_prepend_case_list(case: Option<&'static mut Literal>, then: &'static mut Expr, list: Option<&'static mut CaseLL>) -> *mut CaseLL {
    static_mut(
        CaseLL(StaticLLNode {
            head: if let Some(case) = case {
                IfCase::OnVal(*Box::from_raw(case), *Box::from_raw(then))
            } else {
                IfCase::Else(*Box::from_raw(then))
            },
            tail: StaticLL(list.map(|e| Box::from_raw(&mut e.0)))
        })
    )
}

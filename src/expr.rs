use smallvec::SmallVec as SVec;

enum Type {
    Void,
    Int,
    List(Box<Type>),
    Fn(Box<Type>, SVec<[Box<Type>; 4]>, Purity),
}

struct Ident {
    name: String
}

enum Operation {
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    Prepend(Box<Expr>, Box<Expr>),
    Negate(Box<Expr>),
    Head(Box<Expr>),
    Tail(Box<Expr>),
    Call(Box<Expr>, Purity, SVec<[Box<Expr>; 4]>),
    Unit(Box<Expr>),
    LastUnit(SVec<[Box<Expr>; 5]>),
}

enum Purity {
    Pure, Impure
}

enum Declaration {
    Var(Ident),
    Fn(Ident, Purity, SVec<[Box<Expr>; 4]>),
}

struct Expr {
    ty: Type,
    operation: Operation,
    decls: SVec<[Box<Declaration>; 4]>,
    purity: Purity,
}


use std::fmt;

use crate::id_map::{self, IdMap};
use derive_newtype::NewType;
use smallvec::SmallVec;

#[macro_export]
macro_rules! mktype {
    ($ctx:expr, ( $ty:ident )) => {{
        use $crate::types::Type;
        $ctx.add(Type::$ty)
    }};
    ($ctx:expr, ( Fn $purity:ident ( $($arg:tt)+ ) $ret:tt )) => {{
        use $crate::types::{Type, FnArgTypes, Purity};
        let mut args = FnArgTypes::new();
        $( args.push( mktype!($ctx, $arg) ); )+
        let ret = mktype!($ctx, $ret);
        $ctx.add(Type::Fn(args, ret, Purity::$purity))
    }};
    ($ctx:expr, ( $ty:ident $inner:tt )) => {{
        use $crate::types::Type;
        let inner = mktype!($ctx, $inner );
        $ctx.add(Type::$ty(inner))
    }};
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, NewType)]
pub struct TypeId(id_map::Id);
impl fmt::Debug for TypeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Type{:?}", self.0)
    }
}

pub type FnArgTypes = SmallVec<[TypeId; 4]>;

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Type {
    Int,
    Bool,
    String,
    Curry,
    Type,
    List(TypeId),
    Fn(FnArgTypes, TypeId, Purity),
    
    TypeVar(usize /* id */),
    TypeVarResolved(usize /* id */, TypeId),
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Purity {
    Pure,
    Impure,
    Any
}

impl Purity {
    pub fn mix(self, other: Purity) -> Purity {
        match (self, other) {
            (Purity::Any, other)
            | (other, Purity::Any) => other,
            (Purity::Pure, Purity::Pure) => Purity::Pure,
            (Purity::Impure, Purity::Impure) => Purity::Impure,
            _ => panic!("Purity mismatch!")
        }
    }
}

#[derive(Default)]
pub struct TypeCollateError {
    chain: Vec<(TypeId, TypeId)>,
}
impl TypeCollateError {
    pub fn print(self, types: &TypeStore) {
        let mut iter = self.chain.into_iter();

        let (t1, t2) = iter.next().unwrap();
        eprintln!("Error resolving types {} = {}",
            types.format_ty(t1), types.format_ty(t2));
        
        for (t1, t2) in iter {
            eprintln!("  While resolving {} = {}",
                types.format_ty(t1), types.format_ty(t2));
        }
    }
}

pub struct TypeStore {
    types: IdMap<Type>,
}
impl TypeStore {
    pub fn new() -> Self {
        Self {
            types: IdMap::new(),
        }
    }

    pub fn add(&mut self, ty: Type) -> TypeId {
        self.types.add(ty).into()
    }
    pub fn add_typevar(&mut self) -> TypeId {
        let next_id = self.types.len();
        self.add(Type::TypeVar(next_id))
    }
    pub fn query(&self, ty: TypeId) -> &Type {
        &self.types.get(*ty).unwrap()
    }

    pub fn reify(&mut self, id: TypeId, real: TypeId) -> TypeId {
        self.types.update_with(*id, |old| {
            if let Type::TypeVar(tv_id) = old {
                *old = Type::TypeVarResolved(*tv_id, real);
            }
        });

        real
    }
    pub fn collate_types(&mut self, first: TypeId, second: TypeId) -> Result<TypeId, TypeCollateError> {
        if first == second {
            return Ok(first);
        }

        let first_ty = self.query(first).clone();
        let second_ty = self.query(second).clone();

        let chain_err = |mut e: TypeCollateError| {
            e.chain.push((first, second));
            e
        };
        
        match (first_ty, second_ty) {
            (Type::TypeVarResolved(_, inner), _) => {
                self.collate_types(inner, second)
            }
            (_, Type::TypeVarResolved(_, inner)) => {
                self.collate_types(first, inner)
            }
            (Type::TypeVar(_), _) => {
                Ok(self.reify(first, second))
            }
            (_, Type::TypeVar(_)) => {
                Ok(self.reify(second, first))
            }
            (Type::Fn(args1, ret1, purity1), Type::Fn(args2, ret2, purity2)) => {
                if purity1 != purity2 || args1.len() != args2.len() {
                    return Err(chain_err(Default::default()))
                }

                for arg in args1.iter().zip(args2.iter()) {
                    self.collate_types(*arg.0, *arg.1)
                        .map_err(chain_err)?;
                }
                self.collate_types(ret1, ret2)
                    .map_err(chain_err)?;
                Ok(first)
            }
            (Type::List(ty1), Type::List(ty2)) => {
                self.collate_types(ty1, ty2)
                    .map_err(chain_err)?;
                Ok(first)
            }

            (a, b) if a == b => Ok(first),

            (_, _) => Err(chain_err(Default::default()))
        }
    }

    pub fn format_ty(&self, ty: TypeId) -> String {
        use std::fmt::Write;
        let ty = self.query(ty);
        match &ty {
            Type::String => "String".into(),
            Type::Int => "Int".into(),
            Type::Bool => "Bool".into(),
            Type::Type => "Meta".into(),
            Type::Curry => "...".into(),
            Type::TypeVar(i) => format!("'{}", i),
            Type::TypeVarResolved(_, inner)
                => self.format_ty(*inner),
            Type::List(inner)
                => format!("List({})", self.format_ty(*inner)),
            Type::Fn(args, ret, purity) => {
                let impure_bang = if *purity == Purity::Impure { "!" } else { "" };
                let mut out: String = format!("Fn{}(", impure_bang);
                for arg in args {
                    write!(&mut out, "{} -> ", self.format_ty(*arg))
                        .unwrap();
                }
                out.push_str(&self.format_ty(*ret));
                out.push(')');
                out
            }
        }
    }

    pub fn simplify(&mut self, id: TypeId) -> TypeId {
        let ty = self.query(id).clone();
        match &ty {
            | Type::String | Type::Int | Type::Bool
            | Type::Type | Type::Curry | Type::TypeVar(_)
                => id,
            
            | Type::TypeVarResolved(_, inner)
                => self.simplify(*inner),

            | Type::List(inner) => {
                let inner = self.simplify(*inner);
                self.add(Type::List(inner))
            },

            | Type::Fn(args, ret, purity) => {
                let out_args = args.iter()
                    .map(|id| self.simplify(*id))
                    .collect();
                let out_ret = self.simplify(*ret);
                self.add(Type::Fn(out_args, out_ret, *purity))
            }
        }
    }
}
impl fmt::Debug for TypeStore {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut kvs: Vec<(id_map::Id, Type)> =
            self.types.iter()
            .map(|(id, ty)| (id, ty.clone()))
            .collect();
        
        kvs.sort_by(|(a, _), (b, _)| a.cmp(b));
        
        let mut dbg_s = f.debug_struct("TypeStore");
        for (ty_id, ty) in kvs {
            dbg_s.field(&ty_id.0.to_string(), &ty);
        }
        dbg_s.finish()
    }
}
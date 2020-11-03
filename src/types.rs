use std::fmt;

use crate::id_map::{self, IdMap};
use crate::ast;
use derive_newtype::NewType;
use smallvec::SmallVec;

#[derive(Copy, Clone, Eq, PartialEq, Hash, NewType)]
pub struct TypeId(id_map::Id);
impl fmt::Debug for TypeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Type{:?}", self.0)
    }
}

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

    fn dependent_types(&self, ty: TypeId) -> SmallVec<[TypeId; 4]> {
        match &self.base {
            | BaseType::Int | BaseType::Bool | BaseType::String
            | BaseType::Curry | BaseType::Type | BaseType::TypeExpr(_)
            | BaseType::TypeVar(_)
            => SmallVec::new(),

            | BaseType::List(inner)
            | BaseType::TypeVarResolved(_, inner)
            => SmallVec::from_slice(&[*inner]),

            | BaseType::Fn(args, ret)
            => {
                let mut out = args.clone();
                out.push(*ret);
                out
            }
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
        self.add(Type::new(BaseType::TypeVar(next_id)))
    }
    pub fn query(&self, ty: TypeId) -> &Type {
        &self.types.get(*ty).unwrap()
    }
    pub fn reify(&mut self, id: TypeId, real: TypeId) -> TypeId {
        let new_purity = self.query(real).purity;

        self.types.update_with(*id, |old| {
            old.purity = old.purity.mix(new_purity);

            if let BaseType::TypeVar(tv_id) = old.base {
                old.base = BaseType::TypeVarResolved(tv_id, real);
            }
        });

        id
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
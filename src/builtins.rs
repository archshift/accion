use std::collections::HashMap;

use crate::analysis::typing::{BaseType, Purity, Type, FnArgTypes, TypeStore, TypeId};

pub type BuiltinMap = HashMap<String, Builtin>;

#[derive(Debug)]
pub struct Builtin {
    pub name: String,
    pub ty: TypeId,
}

pub fn make_builtins(types: &mut TypeStore) -> BuiltinMap {
    let mut map = BuiltinMap::new();
    let mut add_builtin = |name: &str, type_id: TypeId| {
        map.insert(name.into(), Builtin { name: name.into(), ty: type_id });
    };

    let int_id = types.add(Type::new(BaseType::Int));
    let meta_id = types.add(Type { base: BaseType::Type, purity: Purity::Pure });
    let meta_list = types.add(Type::new(BaseType::List(meta_id)));

    let mut add_syscall = |name: &str, num_args: usize| {
        let syscall_args = FnArgTypes::from_elem(int_id, num_args);
        let fn_ty = Type {
            base: BaseType::Fn(syscall_args, int_id),
            purity: Purity::Impure
        };
        add_builtin(name, types.add(fn_ty));
    };

    add_syscall("syscall1", 1);
    add_syscall("syscall2", 2);
    add_syscall("syscall3", 3);
    add_syscall("syscall4", 4);
    add_syscall("syscall5", 5);

    add_builtin("Int", meta_id);
    add_builtin("Str", meta_id);
            
    add_builtin("Fn",
        types.add(Type::new(BaseType::Fn(
                FnArgTypes::from_slice(&[meta_list]),
                meta_id
            ))
        ));

    add_builtin("List",
        types.add(Type::new(BaseType::Fn(
                FnArgTypes::from_slice(&[meta_id]),
                meta_id
            ))
        ));

    map
}
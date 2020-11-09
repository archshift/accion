use std::collections::HashMap;

use crate::types::{Type, TypeId, TypeStore, BaseType, Purity, FnArgTypes};
use crate::values::Value;
use crate::analysis::constexpr;

pub type BuiltinMap = HashMap<String, Builtin>;

#[derive(Debug)]
pub struct Builtin {
    pub name: String,
    pub ty: TypeId,
    pub val: Value,
}

pub fn make_builtins(types: &mut TypeStore) -> BuiltinMap {
    let mut map = BuiltinMap::new();
    let mut add_builtin = |name: &str, type_id: TypeId, val: Value| {
        map.insert(name.into(), Builtin { name: name.into(), ty: type_id, val });
    };

    let int_id = types.add(Type::new(BaseType::Int));
    let str_id = types.add(Type::new(BaseType::String));
    let bool_id = types.add(Type::new(BaseType::Bool));
    let meta_id = types.add(Type { base: BaseType::Type, purity: Purity::Pure });
    let meta_list = types.add(Type::new(BaseType::List(meta_id)));

    let mut add_syscall = |name: &str, num_args: usize| {
        let syscall_args = FnArgTypes::from_elem(int_id, num_args);
        let fn_ty = Type {
            base: BaseType::Fn(syscall_args, int_id),
            purity: Purity::Impure
        };
        add_builtin(name, types.add(fn_ty), Value::Undefined);
    };

    add_syscall("syscall1", 1);
    add_syscall("syscall2", 2);
    add_syscall("syscall3", 3);
    add_syscall("syscall4", 4);
    add_syscall("syscall5", 5);

    add_builtin("Int", meta_id, Value::Type(int_id));
    add_builtin("Str", meta_id, Value::Type(str_id));
    add_builtin("Bool", meta_id, Value::Type(bool_id));
            
    add_builtin("Fn",
        types.add(Type::new(BaseType::Fn(
                FnArgTypes::from_slice(&[meta_list]),
                meta_id
            ))
        ),
        Value::BuiltinFn(|args, interp| {
            let interp: &mut constexpr::Interpreter = interp.downcast_mut().unwrap();

            if let &[ Value::List(ref l) ] = &args[..] {
                let mut fn_types: FnArgTypes = l.iter()
                    .map(|v|
                        if let Value::Type(t) = v { *t }
                        else { panic!("Builtin `Fn` should only have Type arguments!") })
                    .collect();

                let ret = fn_types.pop()
                    .expect("Builtin `Fn` should always have a return type");

                Value::Type(interp.types.store.add(Type {
                    base: BaseType::Fn(fn_types, ret),
                    purity: Purity::Pure
                }))
            } else {
                panic!("Error: Should only call builtin `Fn` with one arg: `Arg1 -> Arg2 -> ... -> Ret`");
            }
        })
    );

    add_builtin("List",
        types.add(Type::new(BaseType::Fn(
            FnArgTypes::from_slice(&[meta_id]),
            meta_id
        ))),
        Value::BuiltinFn(|args, interp| {
            let interp: &mut constexpr::Interpreter = interp.downcast_mut().unwrap();

            if let &[ Value::Type(t) ] = &args[..] {
                Value::Type(interp.types.store.add(Type {
                    base: BaseType::List(t),
                    purity: Purity::Pure
                }))
            } else {
                panic!("Error: Should only call builtin `List` with one arg: `InnerType`");
            }
        })
    );

    map
}
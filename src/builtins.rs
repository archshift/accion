use std::collections::HashMap;
use std::char;
use std::convert::TryInto;

use crate::mktype;
use crate::types::{Type, TypeId, TypeStore, Purity, FnArgTypes};
use crate::values::{Value, ValList};

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

    let int_id = types.add(Type::Int);
    let str_id = types.add(Type::String);
    let bool_id = types.add(Type::Bool);
    let meta_id = types.add( Type::Type);

    let mut add_syscall = |name: &str, num_args: usize| {
        let syscall_args = FnArgTypes::from_elem(int_id, num_args);
        let fn_ty = Type::Fn(syscall_args, int_id, Purity::Impure);
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
    add_builtin("Type", meta_id, Value::Type(meta_id));
            
    add_builtin("Fn",
        mktype!(types, (Fn Pure ((List (Type))) (Type))),
        Value::TypeFn(|args, types| {
            if let &[ Value::List(ref l) ] = &args[..] {
                let mut fn_types: FnArgTypes = l.iter()
                    .map(|v|
                        if let Value::Type(t) = v { *t }
                        else { panic!("Builtin `Fn` should only have Type arguments!") })
                    .collect();

                let ret = fn_types.pop()
                    .expect("Builtin `Fn` should always have a return type");

                Value::Type(types.add(Type::Fn(fn_types, ret, Purity::Pure)))
            } else {
                panic!("Error: Should only call builtin `Fn` with one arg: `Arg1 -> Arg2 -> ... -> Ret`");
            }
        })
    );

    add_builtin("List",
        mktype!(types, (Fn Pure ((Type)) (Type))),
        Value::TypeFn(|args, types| {
            if let &[ Value::Type(t) ] = &args[..] {
                Value::Type(types.add(Type::List(t)))
            } else {
                panic!("Error: Should only call builtin `List` with one arg: `InnerType`");
            }
        })
    );

    let dbg_typevar = types.add_typevar();
    add_builtin("debug",
        types.add(Type::Fn(
            FnArgTypes::from_slice(&[dbg_typevar]),
            dbg_typevar,
            Purity::Pure
        )),
        Value::BuiltinFn(|mut args, _interp| {
            if args.len() != 1 {
                panic!("Error: Should only call builtin `chars` with one arg");
            }
            let val = args.pop().unwrap();
            eprintln!("dbg: {:?}", val);
            val
        })
    );

    add_builtin("chars",
        mktype!(types, (Fn Pure ((String)) (Int))),
        Value::BuiltinFn(|args, _interp| {
            if let &[ Value::String(ref s) ] = &args[..] {
                let mut head = ValList::empty();
                for c in s.chars().rev() {
                    head = head.prepended(Value::Int(c as u64))
                }
                Value::List(head)
            } else {
                panic!("Error: Should only call builtin `chars` with one arg: `Str`");
            }
        })
    );

    add_builtin("from_chars",
        mktype!(types, (Fn Pure ((Int)) (String))),
        Value::BuiltinFn(|args, _interp| {
            if let &[ Value::List(ref l) ] = &args[..] {
                let mut out = String::new();
                for item in l.iter() {
                    if let Value::Int(c) = item {
                        if let Ok(c32) = (*c).try_into() {
                            if let Some(c) = char::from_u32(c32) {
                                out.push(c);
                                continue;
                            }
                        }
                    }
                    panic!("Error: Items in list passed to `chars` are not valid Unicode!");
                }
                Value::String(out)
            } else {
                panic!("Error: Should only call builtin `chars` with one arg: `Str`");
            }
        })
    );

    map
}
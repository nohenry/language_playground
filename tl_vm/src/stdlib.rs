use std::{fs::File, io::Read, process::CommandEnvs, sync::Arc};

use linked_hash_map::LinkedHashMap;
use tl_core::{ast::GenericParameter, Module};
use tl_util::Rf;

use crate::{
    const_value::{ConstValue, ConstValueKind, Type},
    intrinsics::{IntrinsicType, IntrinsicUpcast},
    scope::{Scope, ScopeValue},
};

pub fn std_module() -> Arc<Module> {
    let file_path = "test_files/std.xl";
    let mut file = File::open(file_path).unwrap();

    let mut input = String::new();
    file.read_to_string(&mut input).unwrap();

    let (module, errors) = Module::parse_str(&input, "std");
    for error in errors {
        println!("{error}")
    }

    Arc::new(module)
}

pub fn fill_module(module_rf: Rf<Scope>) {
    let mut module = module_rf.borrow_mut();

    let io = module.insert(
        module_rf.clone(),
        "io".to_string(),
        ScopeValue::Module(Arc::new(Module::empty("io"))),
        0,
    );
    fill_io(&io);

    let mem = module.insert(
        module_rf.clone(),
        "mem".to_string(),
        ScopeValue::Module(Arc::new(Module::empty("mem"))),
        0,
    );
    fill_mem(&mem);
}

pub fn fill_io(module_rf: &Rf<Scope>) {
    create_func(
        &module_rf,
        "print",
        [("data".to_string(), Type::String)].into_iter(),
        Type::Empty,
        Arc::new(|params| {
            if let Some(data) = params.get("data") {
                if let Some(data) = data.resolve_ref() {
                    let ScopeValue::ConstValue(cv) = &data.borrow().value else {
                        return ConstValue::empty()
                    };
                    println!("{}", cv)
                } else {
                    println!("{}", data)
                }
            }
            ConstValue::empty()
        }),
    );
}

pub fn fill_mem(module: &Rf<Scope>) {
    let slice_sym = create_intrinsinc_type(module, "Slice", Rf::new(types::Slice {}).upcast());

    let slice_sym_func = slice_sym.clone();
    create_func(
        &module,
        "alloc",
        [
        //     (
        //     "size".to_string(),
        //     Type::Integer {
        //         width: 64,
        //         signed: false,
        //     },
        // )
        ]
        .into_iter(),
        Type::Symbol(slice_sym.clone()),
        Arc::new(move |params| ConstValue {
            kind: ConstValueKind::IntrinsicStorage(Rf::new(types::Slice {}).upcast(), vec![]),
            ty: Type::Intrinsic(slice_sym_func.clone()),
        }),
    );
}

pub mod types {
    use crate::intrinsics::IntrinsicType;

    #[derive(Clone)]
    pub struct Slice {}

    impl IntrinsicType for Slice {}
}

fn create_func<P: Iterator<Item = (String, Type)>>(
    module: &Rf<Scope>,
    name: &str,
    p: P,
    r: Type,
    func: Arc<dyn Fn(&LinkedHashMap<String, ConstValue>) -> ConstValue + Sync + Send>,
) -> Rf<Scope> {
    let mut mo = module.borrow_mut();

    let sym = mo.insert(module.clone(), name.to_string(), ScopeValue::Root, 0);

    let cv = ScopeValue::ConstValue(ConstValue {
        kind: ConstValueKind::NativeFunction {
            rf: sym,
            callback: func,
        },
        ty: Type::Function {
            parameters: LinkedHashMap::from_iter(p),
            return_type: Box::new(r),
        },
    });

    mo.update(name, cv).unwrap()
}

fn create_intrinsinc_type(
    module: &Rf<Scope>,
    name: &str,
    data: Rf<dyn IntrinsicType + Send + Sync>,
) -> Rf<Scope> {
    let mut mo = module.borrow_mut();

    let sym = mo.insert(module.clone(), name.to_string(), ScopeValue::Root, 0);

    let cv = ScopeValue::IntrinsicStruct {
        initial_value: data,
    };

    mo.update(name, cv).unwrap()
}

fn create_generic_intrinsinc_type(
    module: &Rf<Scope>,
    name: &str,
    generics: Vec<GenericParameter>,
    data: Rf<dyn IntrinsicType + Send + Sync>,
) -> Rf<Scope> {
    let mut mo = module.borrow_mut();

    let sym = mo.insert(module.clone(), name.to_string(), ScopeValue::Root, 0);

    let cv = ScopeValue::IntrinsicStructTemplate {
        initial_value: data,
        generics,
    };

    mo.update(name, cv).unwrap()
}

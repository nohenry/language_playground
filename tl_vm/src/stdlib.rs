use std::{fs::File, io::Read, sync::Arc};

use linked_hash_map::LinkedHashMap;
use tl_core::{ast::GenericParameter, Module};
use tl_util::Rf;

use tl_evaluator::{
    evaluation_type::EvaluationType,
    evaluation_value::EvaluationValue,
    scope::{
        intrinsics::{IntrinsicType, IntrinsicUpcast},
        scope::{Scope, ScopeValue},
    },
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

pub fn fill_module<T: EvaluationType<Value = V>, V: EvaluationValue<Type = T>>(
    module_rf: Rf<Scope<T, V>>,
) {
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

pub fn fill_io<T: EvaluationType<Value = V>, V: EvaluationValue<Type = T>>(
    module_rf: &Rf<Scope<T, V>>,
) {
    create_func(
        &module_rf,
        "print",
        [("data".to_string(), T::string())].into_iter(),
        T::empty(),
        Arc::new(|params| {
            if let Some(data) = params.get("data") {
                if let Some(data) = data.resolve_ref() {
                    let ScopeValue::EvaluationValue(cv) = &data.borrow().value else {
                        return V::empty(self.type_provider.as_ref())
                    };
                    println!("{}", cv)
                } else {
                    println!("{}", data)
                }
            }
            V::empty(self.type_provider.as_ref())
        }),
    );
}

pub fn fill_mem<T: EvaluationType<Value = V>, V: EvaluationValue<Type = T>>(
    module: &Rf<Scope<T, V>>,
) {
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
        T::symbol(slice_sym.clone()),
        Arc::new(move |_params| {
            V::intrinsic_storage(
                slice_sym_func.clone(),
                Rf::new(types::Slice {}).upcast(),
                vec![],
            )
        }),
    );
}

pub mod types {
    use tl_evaluator::scope::intrinsics::IntrinsicType;

    #[derive(Clone)]
    pub struct Slice {}

    impl IntrinsicType for Slice {}
}

fn create_func<
    T: EvaluationType<Value = V>,
    V: EvaluationValue<Type = T>,
    P: Iterator<Item = (String, T)>,
>(
    module: &Rf<Scope<T, V>>,
    name: &str,
    p: P,
    r: T,
    func: Arc<dyn Fn(&LinkedHashMap<String, V>) -> V + Sync + Send>,
) -> Rf<Scope<T, V>> {
    let mut mo = module.borrow_mut();

    let sym = mo.insert(module.clone(), name.to_string(), ScopeValue::Root, 0);

    let cv = ScopeValue::EvaluationValue(V::native_function(
        func,
        LinkedHashMap::from_iter(p),
        r,
        sym,
    ));

    // ConstValue {
    //     kind: ConstValueKind::NativeFunction {
    //         rf: sym,
    //         callback: func,
    //     },
    //     ty: Type::Function {
    //         parameters: LinkedHashMap::from_iter(p),
    //         return_type: Box::new(r),
    //     },
    // }

    mo.update(name, cv).unwrap()
}

fn create_intrinsinc_type<T: EvaluationType<Value = V>, V: EvaluationValue<Type = T>>(
    module: &Rf<Scope<T, V>>,
    name: &str,
    data: Rf<dyn IntrinsicType + Send + Sync>,
) -> Rf<Scope<T, V>> {
    let mut mo = module.borrow_mut();

    let _sym = mo.insert(module.clone(), name.to_string(), ScopeValue::Root, 0);

    let cv = ScopeValue::IntrinsicStruct {
        initial_value: data,
    };

    mo.update(name, cv).unwrap()
}

fn create_generic_intrinsinc_type<T: EvaluationType<Value = V>, V: EvaluationValue<Type = T>>(
    module: &Rf<Scope<T, V>>,
    name: &str,
    generics: Vec<GenericParameter>,
    data: Rf<dyn IntrinsicType + Send + Sync>,
) -> Rf<Scope<T, V>> {
    let mut mo = module.borrow_mut();

    let _sym = mo.insert(module.clone(), name.to_string(), ScopeValue::Root, 0);

    let cv = ScopeValue::IntrinsicStructTemplate {
        initial_value: data,
        generics,
    };

    mo.update(name, cv).unwrap()
}

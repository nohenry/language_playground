#![feature(return_position_impl_trait_in_trait)]
#![feature(impl_trait_projections)]

use std::{
    fs::{File, OpenOptions},
    io::Read,
    path::Path,
    rc::Rc,
    sync::Arc,
};

use inkwell::{
    context::Context,
    types::{AnyType, AnyTypeEnum},
    AddressSpace,
};
use linked_hash_map::LinkedHashMap;
use llvm_type::LlvmType;
use llvm_value::LlvmValue;
use tl_core::Module;
use tl_evaluator::{
    evaluation_type::EvaluationTypeProvider,
    evaluation_value::EvaluationValue,
    evaluator::Evaluator,
    pass::{EvaluationPass, MemberPass, TypeFirst},
    scope::scope::Scope,
};
use tl_util::{format::TreeDisplay, Rf};

pub mod context;
pub mod llvm_type;
pub mod llvm_value;

#[cfg(windows)]
const LINE_ENDING: &str = "\r\n";
#[cfg(not(windows))]
const LINE_ENDING: &str = "\n";

pub struct LlvmTypeProvider<'a> {
    module: inkwell::module::Module<'a>,
    context: &'a inkwell::context::Context,
}

impl<'a> EvaluationTypeProvider<'a> for LlvmTypeProvider<'a> {
    type Type = LlvmType<'a>;

    fn empty(&self) -> Self::Type {
        LlvmType::Empty(self.context.void_type())
    }

    fn string(&self) -> Self::Type {
        todo!()
    }

    fn integer(&self, width: u8, signed: bool) -> Self::Type {
        LlvmType::Integer {
            signed,
            llvm_type: self.context.custom_width_int_type(width as _),
        }
    }

    fn cinteger(&self) -> Self::Type {
        LlvmType::CoercibleInteger
    }

    fn float(&self, width: u8) -> Self::Type {
        let ty = match width {
            16 => self.context.f16_type(),
            32 => self.context.f32_type(),
            64 => self.context.f64_type(),
            128 => self.context.f128_type(),
            _ => panic!("Unsupported float width!"),
        };
        LlvmType::Float(ty)
    }

    fn cfloat(&self) -> Self::Type {
        LlvmType::CoercibleFloat
    }

    fn bool(&self) -> Self::Type {
        LlvmType::Boolean(self.context.bool_type())
    }

    fn function(&self) -> Self::Type {
        todo!()
    }

    fn symbol(
        &self,
        symbol: Rf<
            Scope<Self::Type, <Self::Type as tl_evaluator::evaluation_type::EvaluationType>::Value>,
        >,
    ) -> Self::Type {
        LlvmType::Symbol(symbol)
    }

    fn rf(&self, base_type: Self::Type) -> Self::Type {
        let ptr_ty = base_type.llvm_ptr_ty(AddressSpace::default());
        LlvmType::Ref {
            base_type: Box::new(base_type),
            llvm_type: ptr_ty,
        }
    }

    fn intrinsic(
        &self,
        symbol: Rf<
            Scope<Self::Type, <Self::Type as tl_evaluator::evaluation_type::EvaluationType>::Value>,
        >,
    ) -> Self::Type {
        LlvmType::Intrinsic(symbol)
    }

    fn function_def(
        &self,
        body: tl_core::ast::Statement,
        parameters: LinkedHashMap<String, LlvmType<'a>>,
        return_type: LlvmType<'a>,
        node: tl_util::Rf<Scope<LlvmType<'a>, LlvmValue<'a>>>,
    ) -> LlvmValue<'a> {
        LlvmValue::function(body, parameters, return_type, node)
    }
}

pub fn run_file<P: AsRef<Path> + std::fmt::Display>(path: P) {
    let mut file = match File::open(path.as_ref()) {
        Ok(file) => file,
        Err(e) => {
            eprintln!("{e}");
            eprintln!("File: {path}");
            return;
        }
    };

    let mut input = String::new();
    file.read_to_string(&mut input).unwrap();

    let (module, errors) = Module::parse_str(&input, "mymod");
    for error in errors {
        println!("{error}")
    }
    let module = Arc::new(module);

    let lines: Vec<&str> = input.split(LINE_ENDING).collect();


    // ** LLVM setup
    let llvm_context = Context::create();

    // let i32_type = llvm_context.i32_type();
    // let func_type = i32_type.fn_type(&[i32_type.into()], false);
    // let func = llvm_module.add_function("main", func_type, None);

    // {
    //     // let output = OpenOptions::new().write(true).create(true).open("output.ll").unwrap();
    //     let path = Path::new("output.bc");
    // }

    // ** Codegen

    {
        
        let llvm_module = llvm_context.create_module("mymod");
        let symbol_tree = Rf::new(Scope::<LlvmType, LlvmValue>::root());
    
        let context = Rc::new(LlvmTypeProvider {
            context: &llvm_context,
            module: llvm_module,
        });
        

        {
            let code_pass = Evaluator::<LlvmType, LlvmValue, LlvmTypeProvider, TypeFirst>::new(
                symbol_tree.clone(),
                module.clone(),
                context.clone(),
                1,
            );
            code_pass.evaluate();
            let code_pass_state = code_pass.finish();

            let code_pass =
                Evaluator::<LlvmType, LlvmValue, LlvmTypeProvider, MemberPass>::new_with_state(
                    code_pass_state,
                    context.clone(),
                    module.clone(),
                );
            code_pass.evaluate();
            let code_pass_state = code_pass.finish();

            println!("{}", symbol_tree.format());

            let evaluator = Evaluator::<LlvmType, LlvmValue, LlvmTypeProvider, EvaluationPass>::new(
                module,
                code_pass_state.scope,
                context.clone(),
            );
            let _values = evaluator.evaluate();

            println!("{}", symbol_tree.format());

            for error in &code_pass_state.errors {
                error.print(path.as_ref().as_os_str().to_str().unwrap(), &lines);
            }

            for error in &evaluator.state.read().unwrap().errors {
                error.print(path.as_ref().as_os_str().to_str().unwrap(), &lines);
            }

            // drop(context)
        }


        // drop(context)
    }

    // drop(symbol_tree);
    drop(llvm_context);
}

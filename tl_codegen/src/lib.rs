#![feature(return_position_impl_trait_in_trait)]
#![feature(impl_trait_projections)]
#![feature(box_patterns)]
#![feature(iter_intersperse)]

use std::{
    fs::{File, OpenOptions},
    io::Read,
    path::Path,
    rc::Rc,
    sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard},
};

use inkwell::{
    basic_block::BasicBlock,
    context::Context,
    types::{AnyType, AnyTypeEnum},
    AddressSpace,
};
use linked_hash_map::LinkedHashMap;
use llvm_type::LlvmType;
use llvm_value::LlvmValue;
use tl_core::{
    ast::{PunctuationList, Statement},
    Module,
};
use tl_evaluator::{
    evaluation_type::EvaluationTypeProvider,
    evaluation_value::EvaluationValue,
    evaluator::Evaluator,
    pass::{EvaluationPass, MemberPass, TypeFirst},
    scope::scope::{Scope, ScopeValue},
};
use tl_util::{format::TreeDisplay, Rf};

use crate::evaluator::LlvmEvaluator;

pub mod context;
// pub mod evaluator;
pub mod eval;
pub mod llvm_type;
pub mod llvm_value;

#[cfg(windows)]
const LINE_ENDING: &str = "\r\n";
#[cfg(not(windows))]
const LINE_ENDING: &str = "\n";

pub struct LlvmContextState<'a> {
    current_block: BasicBlock<'a>,
}

pub struct LlvmContext<'a> {
    module: inkwell::module::Module<'a>,
    builder: inkwell::builder::Builder<'a>,
    context: &'a inkwell::context::Context,
    state: RwLock<LlvmContextState<'a>>,
}

impl<'a> LlvmContext<'a> {
    fn rstate(&self) -> RwLockReadGuard<'_, LlvmContextState<'a>> {
        self.state.read().unwrap()
    }

    fn wstate(&self) -> RwLockWriteGuard<'_, LlvmContextState<'a>> {
        self.state.write().unwrap()
    }
}

impl<'a> EvaluationTypeProvider for LlvmContext<'a> {
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
        LlvmType::CoercibleInteger(self.context.i64_type())
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
        LlvmType::CoercibleFloat(self.context.f64_type())
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
        LlvmValue::function("", body, parameters, return_type, node, self)
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

    // ** Codegen

    {
        let llvm_module = llvm_context.create_module("mymod");
        let llvm_builder = llvm_context.create_builder();
        let symbol_tree = Rf::new(Scope::<LlvmType, LlvmValue>::root());

        let bb = {
            let ty = llvm_context.void_type().fn_type(&[], false);
            let val = llvm_module.add_function("main", ty, None);
            let bb = llvm_context.append_basic_block(val.clone(), "entry");

            llvm_builder.position_at_end(bb);

            bb
        };

        let context = Rc::new(LlvmContext {
            context: &llvm_context,
            module: llvm_module,
            builder: llvm_builder,
            state: RwLock::new(LlvmContextState { current_block: bb }),
        });

        {
            let code_pass = LlvmEvaluator::<TypeFirst>::new(
                symbol_tree.clone(),
                module.clone(),
                1,
                context.clone(),
            );
            code_pass.evaluate();
            let code_pass_state = code_pass.finish();

            let code_pass = LlvmEvaluator::<MemberPass>::new_with_state(
                code_pass_state,
                module.clone(),
                context.clone(),
            );
            code_pass.evaluate();
            let mut code_pass_state = code_pass.finish();

            println!("{}", symbol_tree.format());

            let evaluator = LlvmEvaluator::<EvaluationPass>::new(
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
        }

        let path = Path::new("output.bc");
        context.module.write_bitcode_to_path(path);
    }
}

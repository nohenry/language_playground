#![feature(return_position_impl_trait_in_trait)]

use std::{fs::File, io::Read, path::Path, sync::Arc};

use inkwell::context::Context;
use llvm_type::LlvmType;
use llvm_value::LlvmValue;
use tl_core::Module;
use tl_evaluator::{
    evaluation_type::EvaluationTypeProvider,
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

#[derive(Clone)]
pub struct LlvmTypeProvider<'a> {
    context: &'a inkwell::context::Context,
}

impl EvaluationTypeProvider for LlvmTypeProvider<'_> {
    type Type = LlvmType;

    fn empty(&self) -> Self::Type {
        todo!()
    }

    fn string(&self) -> Self::Type {
        todo!()
    }

    fn integer(&self, width: u8, signed: bool) -> Self::Type {
        todo!()
    }

    fn cinteger(&self) -> Self::Type {
        todo!()
    }

    fn float(&self, width: u8) -> Self::Type {
        todo!()
    }

    fn cfloat(&self) -> Self::Type {
        todo!()
    }

    fn bool(&self) -> Self::Type {
        todo!()
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
        todo!()
    }

    fn rf(&self, base_type: Self::Type) -> Self::Type {
        todo!()
    }

    fn intrinsic(
        &self,
        symbol: Rf<
            Scope<Self::Type, <Self::Type as tl_evaluator::evaluation_type::EvaluationType>::Value>,
        >,
    ) -> Self::Type {
        todo!()
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

    let symbol_tree = Rf::new(Scope::<LlvmType, LlvmValue>::root());

    // ** LLVM setup
    let llvm_context = Context::create();

    // ** Codegen
    let context = LlvmTypeProvider {
        context: &llvm_context,
    };

    let code_pass = Evaluator::<LlvmType, LlvmValue, LlvmTypeProvider, TypeFirst>::new(
        symbol_tree.clone(),
        module.clone(),
        context.clone(),
        1,
    );
    code_pass.evaluate();
    let code_pass_state = code_pass.finish();

    let code_pass = Evaluator::<LlvmType, LlvmValue, LlvmTypeProvider, MemberPass>::new_with_state(
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
        context
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

#![feature(iter_intersperse)]
#![feature(box_patterns)]
#![feature(hasher_prefixfree_extras)]
use std::{fs::File, io::Read, path::Path, sync::Arc};

use evaluator::Evaluator;
use tl_core::Module;
use tl_util::{format::TreeDisplay, Rf};

use crate::{
    pass::CodePass,
    scope::{Scope, ScopeValue},
    stdlib::{fill_module, std_module},
};

pub mod const_value;
pub mod error;
pub mod evaluator;
pub mod intrinsics;
pub mod pass;
pub mod scope;
pub mod stdlib;

#[cfg(windows)]
const LINE_ENDING: &str = "\r\n";
#[cfg(not(windows))]
const LINE_ENDING: &str = "\n";

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

    let symbol_tree = Rf::new(Scope::root());

    {
        let std_module = std_module();

        let code_pass = CodePass::new(symbol_tree.clone(), std_module.clone(), 0);
        let code_pass_state = code_pass.run();
        let std_mod_scope = code_pass_state.scope.module.clone();

        let evaluator = Evaluator::new(std_module, code_pass_state.scope);
        let values = evaluator.evaluate();

        for error in &code_pass_state.errors {
            error.print("std.xl", &lines);
        }

        for error in &evaluator.state.read().unwrap().errors {
            error.print("std.xl", &lines);
        }

        for value in values {
            println!("{value}");
        }

        fill_module(std_mod_scope);
    }

    let code_pass = CodePass::new(symbol_tree.clone(), module.clone(), 1);
    let code_pass_state = code_pass.run();
    println!("{}", symbol_tree.format());

    let evaluator = Evaluator::new(module, code_pass_state.scope);
    let values = evaluator.evaluate();
    println!("{}", symbol_tree.format());

    for error in &code_pass_state.errors {
        error.print(path.as_ref().as_os_str().to_str().unwrap(), &lines);
    }

    for error in &evaluator.state.read().unwrap().errors {
        error.print(path.as_ref().as_os_str().to_str().unwrap(), &lines);
    }

    for value in values {
        // println!("{value}");
    }

    // println!("{}", symbol_tree.format());
}

use std::{fs::File, io::Read, path::Path};

use colored::Colorize;
use tl_core::Module;
use tl_util::{
    format::{SemanticType, TreeDisplay},
    macros::mcr,
    Rf,
};

struct Poo {

}

fn main() {
    let mut m = Rf::new(0);


    // let red= m.clone();
    // *m.borrow_mut() = 7;

    // tl_vm::run_file("test_files/test.xl");
    // tl_codegen::run_file("test_files/llvm_test.xl");
    tl_codegen_llvm::run_file("test_files/test1.xl")
}

// pub fn run_file<P: AsRef<Path> + std::fmt::Display>(path: P) {
//     let mut file = match File::open(path.as_ref()) {
//         Ok(file) => file,
//         Err(e) => {
//             eprintln!("{e}");
//             eprintln!("File: {path}");
//             return;
//         }
//     };

//     let mut input = String::new();
//     file.read_to_string(&mut input).unwrap();

//     let (module, errors) = Module::parse_str(&input, "mymod");

//     println!("{}", module.semantic_format());

//     for error in errors {
//         println!("{error}")
//     }
// }

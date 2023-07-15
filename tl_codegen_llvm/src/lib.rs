use std::{fs::File, io::Read, path::Path};

use tl_util::format::TreeDisplay;

mod code_type;
mod module;
mod resolve;
mod symbol;
mod type_check;

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

    let (module, errors) = tl_core::Module::parse_str(&input, "mymod");

    println!("{}", module.semantic_format());

    // let module::Module::new();
    let resolves = resolve::Resolve::resolve(module);
    println!("{}", resolves);

    for error in errors {
        println!("{error}")
    }
}

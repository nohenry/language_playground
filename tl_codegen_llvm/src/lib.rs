#![feature(type_changing_struct_update)]
use std::{fs::File, io::{Write, Read}, path::Path};

use tl_util::format::TreeDisplay;

mod code_type;
mod module;
mod resolve;
mod symbol;
mod type_check;
// mod lower;

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

    for error in errors {
        println!("{error}")
    }

    let ctx = inkwell::context::Context::create();

    use inkwell::targets::*;
    Target::initialize_all(&InitializationConfig::default());
    let target =
        Target::from_triple(&TargetMachine::get_default_triple()).expect("Unable to get target");

    let target_machine = target
        .create_target_machine(
            &TargetMachine::get_default_triple(),
            TargetMachine::get_host_cpu_name().to_str().unwrap(),
            TargetMachine::get_host_cpu_features().to_str().unwrap(),
            inkwell::OptimizationLevel::Default,
            RelocMode::Default,
            CodeModel::Default,
        )
        .expect("Unable to create target machine");

    let codegen_module = module::Module::new(&ctx, target_machine);
    let resolve =
        resolve::Resolve::<resolve::TypePass>::new(&module, codegen_module);
    let resolve = resolve.resolve(&module);
    let resolve = resolve.resolve(&module);
    let resolve = resolve.resolve(&module);
    let resolve = resolve.resolve(&module);

    println!("{}", resolve);

    let output = resolve.codegen_module.module.to_string();

    let mut file = File::create("out.ir").unwrap();
    write!(file, "{}", output).unwrap();

    // println!("{}", resolves);

    // let module = lower::Lowerer::lower(module, resolves);
    // println!("{}", module.semantic_format());
}

#[derive(Clone)]
pub struct Type<'a> {
    llvm: inkwell::types::AnyTypeEnum<'a>,
    signed: bool,
    mutable: bool,
    path: resolve::Path,
}

impl<'a> std::fmt::Debug for Type<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.llvm.print_to_string())?;
        if self.signed {
            write!(f, ":s")?;
        }

        if self.mutable {
            write!(f, ":m")?;
        }

        if self.path.path.len() > 0 {
            write!(f, ":{}", self.path)?;
        }

        Ok(())
    }
}

impl<'ctx> Type<'ctx> {
    pub fn ptr_type(&self, address_space: inkwell::AddressSpace) -> Type<'ctx> {
        use inkwell::types::{AnyType, BasicType};
        Type {
            llvm: inkwell::types::BasicTypeEnum::try_from(self.llvm)
                .expect("Unable to convert to basic type!")
                .ptr_type(inkwell::AddressSpace::default())
                .as_any_type_enum(),
            signed: false,
            mutable: self.mutable,
            path: resolve::Path::std("pointer")
                .with_generic([self.path.clone().into()].into_iter()),
        }
    }

    pub fn array_type(&self, size: u32) -> Type<'_> {
        use inkwell::types::{AnyType, BasicType};
        Type {
            llvm: inkwell::types::BasicTypeEnum::try_from(self.llvm)
                .expect("Unable to convert to basic type!")
                .array_type(size)
                .as_any_type_enum(),
            signed: false,
            mutable: false,
            path: resolve::Path::std("array")
                .with_generic([self.path.clone().into(), (size as u64).into()].into_iter()),
        }
    }
}

pub trait TypeBuilder<'ctx> {
    fn signed(self, signed: bool) -> Type<'ctx>;
    fn mutable(self, mutable: bool) -> Type<'ctx>;
    fn path(self, path: resolve::Path) -> Type<'ctx>;
    fn to_type(self) -> Type<'ctx>;
}

impl<'ctx, T: inkwell::types::AnyType<'ctx>> TypeBuilder<'ctx> for T {
    fn signed(self, signed: bool) -> Type<'ctx> {
        Type {
            llvm: self.as_any_type_enum(),
            signed,
            mutable: false,
            path: resolve::Path::empty(),
        }
    }

    fn mutable(self, mutable: bool) -> Type<'ctx> {
        Type {
            llvm: self.as_any_type_enum(),
            signed: false,
            mutable,
            path: resolve::Path::empty(),
        }
    }

    fn path(self, path: resolve::Path) -> Type<'ctx> {
        Type {
            llvm: self.as_any_type_enum(),
            signed: false,
            mutable: false,
            path,
        }
    }

    fn to_type(self) -> Type<'ctx> {
        Type {
            llvm: self.as_any_type_enum(),
            signed: false,
            mutable: false,
            path: resolve::Path::empty(),
        }
    }
}

impl<'ctx> TypeBuilder<'ctx> for Type<'ctx> {
    fn signed(self, signed: bool) -> Type<'ctx> {
        Type { signed, ..self }
    }

    fn mutable(self, mutable: bool) -> Type<'ctx> {
        Type { mutable, ..self }
    }

    fn path(self, path: resolve::Path) -> Type<'ctx> {
        Type { path, ..self }
    }

    fn to_type(self) -> Type<'ctx> {
        self
    }
}

pub struct Value<'a> {
    ty: Type<'a>,
    llvm: inkwell::values::AnyValueEnum<'a>,
}

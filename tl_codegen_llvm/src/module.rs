// use crate::symbol::SymbolTable;

pub struct Module<'a> {
    // pub major_scopes: Vec<SymbolTable>,
    pub target_machine: inkwell::targets::TargetMachine,

    pub context: &'a inkwell::context::Context,
    pub module: inkwell::module::Module<'a>,
    pub builder: inkwell::builder::Builder<'a>,
}

impl<'a> Module<'a> {
    pub fn new(
        ctx: &'a inkwell::context::Context,
        target_machine: inkwell::targets::TargetMachine,
    ) -> Module<'a> {
        let module = ctx.create_module("mymod");

        Module {
            // major_scopes: vec![SymbolTable::new()],
            target_machine,

            context: ctx,
            module,
            builder: ctx.create_builder(),
        }
    }
}

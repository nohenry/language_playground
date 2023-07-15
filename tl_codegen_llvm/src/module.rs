use crate::symbol::SymbolTable;

pub struct Module {
    major_scopes: Vec<SymbolTable>,
}

impl Module {
    pub fn new() -> Module {
        Module {
            major_scopes: vec![SymbolTable::new()],
        }
    }
}

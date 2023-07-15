use std::collections::HashMap;

use crate::code_type::Type;

pub enum SymbolKind {
    Local,
    Global,
}

pub struct Symbol {
    kind: SymbolKind,
    symbol_type: Type,
}

pub struct SymbolTable {
    symbols: HashMap<String, Symbol>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            symbols: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }
}

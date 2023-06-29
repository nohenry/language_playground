use crate::code_type::Type;


pub enum SymbolKind {
    Local,
    Global
}


pub struct Symbol {
    name: String, 
    kind: SymbolKind,
    symbol_type: Type,
}
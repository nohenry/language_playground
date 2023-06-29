pub enum TypeKind {
    Integer { width: u8, signed: bool },
}

pub struct Type {
    kind: TypeKind
}

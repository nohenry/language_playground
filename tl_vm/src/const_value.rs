use std::{
    borrow::Borrow,
    fmt::{Debug, Display, Write},
    hash::Hasher,
    sync::Arc,
};

use linked_hash_map::LinkedHashMap;
use tl_core::ast::Statement;
use tl_util::{
    format::{Grouper, NodeDisplay, TreeDisplay},
    Rf,
};

use crate::scope::{Scope, ScopeValue};

#[derive(Clone, Eq)]
pub enum Type {
    Empty,
    CoercibleInteger,
    CoercibleFloat,
    Integer {
        width: u8,
        signed: bool,
    },
    Float {
        width: u8,
    },
    Boolean,
    Function {
        parameters: LinkedHashMap<String, Type>,
        return_parameters: LinkedHashMap<String, Type>,
    },
    String,
    Symbol(Rf<Scope>),
    Ref {
        base_type: Box<Type>,
    },
    Ident(String),
    Tuple(Vec<Type>),
    StructInitializer {
        members: LinkedHashMap<String, Type>,
    },
    StructInstance {
        rf: Option<Rf<Scope>>,
        members: LinkedHashMap<String, Type>,
    },
}

impl std::hash::Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            Self::Integer { width, signed } => {
                state.write_u8(*width);
                state.write_u8(if *signed { 1 } else { 0 });
            }
            Self::Float { width } => {
                state.write_u8(*width);
            }
            Self::Function {
                parameters,
                return_parameters,
            } => {
                parameters.hash(state);
                return_parameters.hash(state);
            }
            // Self::Ref { base_type } => {
            //     base_type.hash(state);
            // }
            Self::Ident(s) => {
                s.hash(state);
            }
            Self::Tuple(tys) => {
                tys.hash(state);
            }
            Self::StructInitializer { members } => {
                members.hash(state);
            }
            Self::StructInstance {
                rf: Some(rf),
                members,
            } => {
                Scope::hash(rf, state);
                members.hash(state);
            }
            Self::StructInstance { rf: None, members } => {
                members.hash(state);
            }
            Self::Symbol(sym) => {
                Scope::hash(sym, state);
            }
            _ => (),
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::Integer {
                    width: l_width,
                    signed: l_signed,
                },
                Self::Integer {
                    width: r_width,
                    signed: r_signed,
                },
            ) => l_width == r_width && l_signed == r_signed,
            (Self::Float { width: l_width }, Self::Float { width: r_width }) => l_width == r_width,
            (
                Self::Function {
                    parameters: l_parameters,
                    return_parameters: l_return_parameters,
                },
                Self::Function {
                    parameters: r_parameters,
                    return_parameters: r_return_parameters,
                },
            ) => l_parameters == r_parameters && l_return_parameters == r_return_parameters,
            (Self::Symbol(l0), Self::Symbol(r0)) => l0 == r0,
            (Self::Ident(l0), Self::Ident(r0)) => l0 == r0,
            (Self::Tuple(l0), Self::Tuple(r0)) => l0 == r0,
            (
                Self::StructInstance {
                    rf: l_rf,
                    members: l_members,
                },
                Self::StructInstance {
                    rf: r_rf,
                    members: r_members,
                },
            ) => l_rf == r_rf && l_members == r_members,
            (Self::StructInstance { rf: Some(l_rf), .. }, Self::Symbol(sym)) => l_rf == sym,
            (Self::Symbol(sym), Self::StructInstance { rf: Some(l_rf), .. }) => sym == l_rf,
            (Self::Symbol(sym), right) | (right, Self::Symbol(sym)) => {
                let sym = sym.borrow();
                if let ScopeValue::TypeAlias { ident, ty } = &sym.value {
                    return &**ty == right;
                }
                false
            }
            (Self::Empty, Self::Empty) => true,
            (Self::Boolean, Self::Boolean) => true,
            (Self::String, Self::String) => true,
            (Self::Ref { base_type: bty_l }, Self::Ref { base_type: bty_r }) => bty_l == bty_r,
            _ => false
            // _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

// impl PartialEq for Type {
//     fn eq(&self, other: &Self) -> bool {
//         match (self, other) {
//             (Self::Integer { width: l_width, signed: l_signed }, Self::Integer { width: r_width, signed: r_signed }) => l_width == r_width && l_signed == r_signed,
//             (Self::Float { width: l_width }, Self::Float { width: r_width }) => l_width == r_width,
//             (Self::Function { parameters: l_parameters, return_parameters: l_return_parameters }, Self::Function { parameters: r_parameters, return_parameters: r_return_parameters }) => l_parameters == r_parameters && l_return_parameters == r_return_parameters,
//             (Self::Symbol(l0), Self::Symbol(r0)) => Rf::eq(l0, r0),
//             (Self::Ident(l0), Self::Ident(r0)) => l0 == r0,
//             (Self::Tuple(l0), Self::Tuple(r0)) => l0 == r0,
//             (Self::RecordInstance { members: l_members }, Self::RecordInstance { members: r_members }) => l_members == r_members,
//             _ => core::mem::discriminant(self) == core::mem::discriminant(other),
//         }
//     }
// }

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String => f.write_str("string"),
            Self::Boolean => f.write_str("bool"),
            Self::Float { width, .. } => write!(f, "f{width}"),
            Self::Integer {
                width,
                signed: true,
                ..
            } => write!(f, "i{width}"),
            Self::Integer {
                width,
                signed: false,
                ..
            } => write!(f, "u{width}"),
            Self::Empty => f.write_str("()"),
            Self::CoercibleInteger => f.write_str("{integer}"),
            Self::CoercibleFloat => f.write_str("{float}"),
            // Self::Ref { } => f.write_str("ref"),
            Self::Ref { base_type } => {
                write!(f, "{}@", base_type)
            }
            // Self::Ref { symbol, offset }=>{
            //     if let ScopeValue::ConstValue(cv) = &symbol.borrow().value {
            //         if let Some(offset) = offset {
            //             return write!(f, "{}.{}@", offset, cv.ty)
            //         } else {
            //             return write!(f, "{}@", cv.ty)
            //         }
            //     }
            //     write!(f, "unknown@")
            // },
            Self::Function {
                parameters,
                return_parameters,
            } => {
                write!(f, "(")?;
                let mut iter = parameters.iter();

                if let Some(val) = iter.next() {
                    write!(f, "{} {}", val.1, val.0)?;
                }
                for val in iter {
                    write!(f, " ,{} {}", val.1, val.0)?;
                }
                write!(f, ") -> (")?;
                let mut iter = return_parameters.iter();

                if let Some(val) = iter.next() {
                    write!(f, "{} {}", val.1, val.0)?;
                }
                for val in iter {
                    write!(f, " ,{} {}", val.1, val.0)?;
                }
                write!(f, ")")
            }
            Self::Symbol(rs) => {
                let rs = rs.borrow();
                match &rs.value {
                    ScopeValue::Struct { ident, members } => {
                        write!(f, "{ident}: (")?;
                        let mut iter = members.iter();

                        if let Some(val) = iter.next() {
                            write!(f, "{} {}", val.1, val.0)?;
                        }
                        for val in iter {
                            write!(f, " ,{} {}", val.1, val.0)?;
                        }
                        write!(f, ")")?;
                    }
                    ScopeValue::TypeAlias { ident, ty } => {
                        write!(f, "{ident} = {ty}")?;
                    }
                    _ => (),
                }

                Ok(())
            }
            Self::StructInstance { members, .. } | Self::StructInitializer { members } => {
                write!(f, "(")?;
                let mut iter = members.iter();

                if let Some(val) = iter.next() {
                    write!(f, "{} {}", val.1, val.0)?;
                }
                for val in iter {
                    write!(f, " ,{} {}", val.1, val.0)?;
                }
                write!(f, ")")
            }
            Self::Ident(i) => f.write_str(i),
            Self::Tuple(ty) => {
                write!(f, "(")?;
                let mut iter = ty.iter();

                if let Some(val) = iter.next() {
                    write!(f, "{val}")?;
                }
                for val in iter {
                    write!(f, " ,{val}")?;
                }
                write!(f, ")")
            }
        }
    }
}

impl NodeDisplay for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Boolean => f.write_str("bool"),
            Self::String => f.write_str("string"),
            Self::Symbol(ty) => {
                let ty = ty.borrow();
                write!(f, "Symbol {}", ty.name)
            }
            Self::StructInitializer { .. } => write!(f, "Struct Initializer"),
            Self::StructInstance { .. } => write!(f, "Struct Instance"),
            Self::Tuple(_) => write!(f, "Tuple"),
            Self::Empty => write!(f, "Empty"),
            Self::CoercibleInteger => write!(f, "Coercible Integer"),
            Self::CoercibleFloat => write!(f, "Coercible Float"),
            Self::Ref { .. } => write!(f, "Reference"),
            Self::Float { width, .. } => write!(f, "f{width}"),
            Self::Integer {
                width,
                signed: true,
            } => write!(f, "i{width}"),
            Self::Integer {
                width,
                signed: false,
            } => write!(f, "u{width}"),
            Self::Function { .. } => f.write_str("Function"),
            Self::Ident(ident) => f.write_str(ident),
        }
    }
}

impl Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Type as NodeDisplay>::fmt(self, f)
    }
}

impl TreeDisplay for Type {
    fn num_children(&self) -> usize {
        match self {
            Type::Function { .. } => 2,
            Type::Tuple(tu) => tu.len(),
            Type::StructInstance { members, .. } => members.len(),
            Type::StructInitializer { members } => members.len(),
            Type::Ref { .. } => 1,
            Type::Symbol(_) => 1,
            _ => 0,
        }
    }

    fn child_at(&self, _index: usize) -> Option<&dyn TreeDisplay> {
        match self {
            Type::Function {
                parameters,
                return_parameters,
            } => match _index {
                0 => Some(parameters),
                1 => Some(return_parameters),
                _ => None,
            },
            Type::Tuple(tu) => {
                if let Some(ty) = tu.get(_index) {
                    Some(ty)
                } else {
                    None
                }
            }
            Type::StructInstance { .. } => None,
            // Type::Ref {  } => None,
            Type::Ref { base_type } => Some(&**base_type),

            _ => None,
        }
    }

    fn child_at_bx<'a>(&'a self, _index: usize) -> Box<dyn TreeDisplay<()> + 'a> {
        match self {
            Type::StructInstance { members, .. } => members.child_at_bx(_index),
            Type::StructInitializer { members, .. } => members.child_at_bx(_index),
            Type::Symbol(sym) => {
                // let sym = sym.borrow();
                Box::new(sym.borrow())
                // Box::new(Grouper("".to_string(), sym.child_at(1).unwrap()))
            }
            _ => panic!(),
        }
    }
}

#[derive(Clone)]
pub enum ConstValueKind {
    Empty,
    Integer {
        value: u64,
    },
    Float {
        value: f64,
    },
    String {
        string: String,
    },
    Bool {
        value: bool,
    },
    Ref {
        // symbol: Rf<Scope>,
        base: Box<ConstValue>,
        offset: Option<String>,
    },
    Function {
        rf: Rf<Scope>,
        body: Statement,
    },
    NativeFunction {
        rf: Rf<Scope>,
        callback: Arc<
            dyn Fn(&LinkedHashMap<String, ConstValue>) -> LinkedHashMap<String, ConstValue>
                + Sync
                + Send,
        >,
    },
    Symbol(Rf<Scope>),
    Tuple(Vec<ConstValue>),
    StructInitializer {
        members: LinkedHashMap<String, ConstValue>,
    },
    StructInstance {
        rf: Rf<Scope>,
        members: LinkedHashMap<String, ConstValue>,
    },
}

impl Display for ConstValueKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConstValueKind::Empty => f.write_str("()"),
            ConstValueKind::Bool { value } => write!(f, "{value}"),
            ConstValueKind::Integer { value } => write!(f, "{value}"),
            ConstValueKind::Float { value } => write!(f, "{value}"),
            ConstValueKind::String { string } => write!(f, "{string}"),
            ConstValueKind::Ref { base, offset } => {
                write!(f, "{}", base)?;
                // if let ScopeValue::ConstValue(cv) = &symbol.borrow().value {
                if let Some(offset) = offset {
                    return write!(f, ".{}@", offset);
                } else {
                    return f.write_char('@');
                }
                // }
                write!(f, "unknown@")
            }
            ConstValueKind::Function { body, .. } => write!(f, "{}", body.format()),
            ConstValueKind::NativeFunction { .. } => write!(f, "Native Function"),
            ConstValueKind::Tuple(list) => {
                let mut iter = list.iter();
                let Some(item) = iter.next() else {
                    return writeln!(f, "()");
                };
                write!(f, "{}", item.kind)?;
                for item in iter {
                    write!(f, ", {}", item.kind)?;
                }
                Ok(())
            }
            ConstValueKind::StructInstance { members, .. }
            | ConstValueKind::StructInitializer { members } => {
                let mut iter = members.iter();
                write!(f, "{{ ")?;
                let Some(item) = iter.next() else {
                    return writeln!(f, "() }}");
                };
                write!(f, "{}: {}", item.0, item.1)?;
                for item in iter {
                    write!(f, ", {}: {}", item.0, item.1)?;
                }
                write!(f, " }}")?;
                Ok(())
            }
            ConstValueKind::Symbol(_) => f.write_str("Symbol"),
        }
    }
}

impl ConstValueKind {
    pub fn as_integer(&self) -> u64 {
        match self {
            ConstValueKind::Integer { value } => *value,
            _ => panic!(),
        }
    }

    pub fn as_float(&self) -> f64 {
        match self {
            ConstValueKind::Float { value } => *value,
            _ => panic!(),
        }
    }

    pub fn as_record_instance(&self) -> (&Rf<Scope>, &LinkedHashMap<String, ConstValue>) {
        match self {
            ConstValueKind::StructInstance { rf, members } => (rf, members),
            _ => panic!(),
        }
    }

    pub fn as_empty(&self) -> bool {
        matches!(self, ConstValueKind::Empty)
    }
}

impl NodeDisplay for ConstValueKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ConstValueKind::Empty => write!(f, "Empty"),
            ConstValueKind::Integer { value } => write!(f, "Integer: {value}"),
            ConstValueKind::Bool { value } => write!(f, "Boolean: {value}"),
            ConstValueKind::Float { value } => write!(f, "Float: {value}"),
            ConstValueKind::Ref { .. } => write!(f, "Reference"),
            ConstValueKind::String { string } => write!(f, "String: {string}"),
            ConstValueKind::Function { .. } => write!(f, "Function"),
            ConstValueKind::NativeFunction { .. } => write!(f, "Native Function"),
            ConstValueKind::Tuple(_) => write!(f, "Tuple"),
            ConstValueKind::StructInitializer { .. } => write!(f, "Struct Initializer"),
            ConstValueKind::StructInstance { .. } => write!(f, "Record Instance"),
            ConstValueKind::Symbol(_) => write!(f, "Symbol"),
        }
    }
}

impl TreeDisplay for ConstValueKind {
    fn num_children(&self) -> usize {
        match self {
            ConstValueKind::Function { .. } => 1,
            ConstValueKind::Tuple(list) => list.len(),
            ConstValueKind::StructInitializer { members, .. } => members.len(),
            ConstValueKind::StructInstance { members, .. } => members.len(),
            ConstValueKind::Ref {
                base,
                offset: Some(_),
            } => 2,
            ConstValueKind::Ref { base, .. } => 1,
            ConstValueKind::Symbol(_) => 1,
            _ => 0,
        }
    }

    fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay<()>> {
        match self {
            ConstValueKind::Function { body, .. } => match index {
                0 => Some(body),
                _ => None,
            },
            ConstValueKind::Tuple(tu) => {
                if let Some(val) = tu.get(index) {
                    Some(val)
                } else {
                    None
                }
            }
            ConstValueKind::StructInstance { .. } => None,
            ConstValueKind::Ref { base, offset } => match index {
                0 => Some(&**base),
                1 => offset.as_ref().map::<&dyn TreeDisplay, _>(|f| f),
                _ => None,
            },
            _ => None,
        }
    }

    fn child_at_bx<'a>(&'a self, index: usize) -> Box<dyn TreeDisplay<()> + 'a> {
        match self {
            ConstValueKind::StructInstance { members, .. } => members.child_at_bx(index),
            ConstValueKind::StructInitializer { members } => members.child_at_bx(index),
            ConstValueKind::Symbol(sym) => Box::new(sym.borrow()),
            // ConstValueKind::Ref { base, offset } => Box::new(base),
            _ => panic!(),
        }
    }
}

#[derive(Clone)]
pub struct ConstValue {
    pub ty: Type,
    pub kind: ConstValueKind,
}

impl ConstValue {
    pub fn empty() -> ConstValue {
        ConstValue {
            ty: Type::Empty,
            kind: ConstValueKind::Empty,
        }
    }

    pub fn default_for(ty: &Type) -> ConstValue {
        let kind = match ty {
            Type::Empty => ConstValueKind::Empty,
            Type::Integer { .. } => ConstValueKind::Integer { value: 0 },
            Type::Float { .. } => ConstValueKind::Float { value: 0.0 },
            _ => ConstValueKind::Empty,
        };

        ConstValue {
            ty: ty.clone(),
            kind,
        }
    }

    pub fn resolve_ref(&self) -> Option<Rf<Scope>> {
        match (&self.ty, &self.kind) {
            (
                Type::Ref {
                    base_type: box Type::Ref { .. },
                },
                ConstValueKind::Symbol(sym),
            ) => {
                let value = sym.borrow();
                if let ScopeValue::ConstValue(cv) = &value.value {
                    return cv.resolve_ref();
                }
                return Some(sym.clone())
            },
            (Type::Ref { .. }, ConstValueKind::Symbol(sym)) => return Some(sym.clone()),
            (Type::Ref { .. }, ConstValueKind::Ref { base, offset: None }) => {
                return base.resolve_ref();
            }
            (
                Type::Ref { .. },
                ConstValueKind::Ref {
                    base,
                    offset: Some(index),
                },
            ) => {
                let base = base.resolve_ref();
                let Some(base) = base else {
                    return None;
                };

                let base = base.borrow();
                let Some(child) = base.children.get(index) else {
                    return None
                };

                return Some(child.clone());
            }
            _ => None,
        }
    }

    pub fn resolve_ref_value(&self) -> Option<ConstValue> {
        let Some(sym) = self.resolve_ref() else {
            return None
        };

        let value = sym.borrow();

        let ScopeValue::ConstValue(cv) = &value.value else {
            return None;
        };

        Some(cv.clone())
    }

    pub fn string(str: String) -> ConstValue {
        ConstValue {
            ty: Type::String,
            kind: ConstValueKind::String { string: str },
        }
    }

    pub fn integer(value: u64, width: u8, signed: bool) -> ConstValue {
        ConstValue {
            kind: ConstValueKind::Integer { value },
            ty: Type::Integer { width, signed },
        }
    }

    pub fn cinteger(value: u64) -> ConstValue {
        ConstValue {
            kind: ConstValueKind::Integer { value },
            ty: Type::CoercibleInteger,
        }
    }

    pub fn float(value: f64, width: u8) -> ConstValue {
        ConstValue {
            kind: ConstValueKind::Float { value },
            ty: Type::Float { width },
        }
    }

    pub fn cfloat(value: f64) -> ConstValue {
        ConstValue {
            kind: ConstValueKind::Float { value },
            ty: Type::CoercibleFloat,
        }
    }

    pub fn bool(value: bool) -> ConstValue {
        ConstValue {
            kind: ConstValueKind::Bool { value },
            ty: Type::Boolean,
        }
    }

    pub fn func(
        body: Statement,
        parameters: LinkedHashMap<String, Type>,
        return_parameters: LinkedHashMap<String, Type>,
        node: Rf<Scope>,
    ) -> ConstValue {
        ConstValue {
            kind: ConstValueKind::Function { body, rf: node },
            ty: Type::Function {
                parameters,
                return_parameters,
            },
        }
    }

    pub fn tuple(values: Vec<ConstValue>) -> ConstValue {
        let types: Vec<_> = values.iter().map(|val| val.ty.clone()).collect();
        ConstValue {
            kind: ConstValueKind::Tuple(values),
            ty: Type::Tuple(types),
        }
    }

    pub fn record_initializer(values: LinkedHashMap<String, ConstValue>) -> ConstValue {
        let types = values.values().map(|val| val.ty.clone());
        let ty = LinkedHashMap::from_iter(values.keys().cloned().zip(types));

        ConstValue {
            ty: Type::StructInitializer { members: ty },
            kind: ConstValueKind::StructInitializer { members: values },
        }
    }

    pub fn record_instance(
        sym: Rf<Scope>,
        // values: LinkedHashMap<String, ConstValue>,
    ) -> ConstValue {
        // let types = values.values().map(|val| val.ty.clone());
        // let ty = LinkedHashMap::from_iter(values.keys().cloned().zip(types));

        ConstValue {
            ty: Type::StructInstance {
                rf: Some(sym.clone()),
                members: LinkedHashMap::new(),
            },
            kind: ConstValueKind::StructInstance {
                rf: sym,
                members: LinkedHashMap::new(),
            },
        }
    }

    pub fn sym_reference(sym: &Rf<Scope>, ty: Type) -> ConstValue {
        // println!("Ref {} ", sym.borrow().format());
        ConstValue {
            // ty: Type::Symbol(sym.clone()),
            ty: Type::Ref {
                base_type: Box::new(ty),
            },
            kind: ConstValueKind::Symbol(sym.clone()),
        }
        // ConstValue {
        //     ty: Type::Ref {
        //         base_type: Box::new(ty),
        //     },
        //     kind: ConstValueKind::Ref {
        //         base: Box::new(ConstValue {
        //             ty: Type::Symbol(sym.clone()),
        //             kind: ConstValueKind::Empty,
        //         }),
        //         offset: None,
        //     },
        // }
    }

    /// Creates a reference value with a member access expression.
    ///
    /// `left` should be a value reference to scope value
    /// `right` is an index into that scope values children
    pub fn reference(left: ConstValue, right: String, right_ty: Type) -> ConstValue {
        ConstValue {
            ty: Type::Ref {
                base_type: Box::new(right_ty),
            },
            kind: ConstValueKind::Ref {
                base: Box::new(left),
                offset: Some(right),
            },
        }
    }

    pub fn try_implicit_cast(&self, ty: &Type) -> Option<ConstValue> {
        match (self, ty) {
            (
                ConstValue {
                    kind: ConstValueKind::Integer { value },
                    ty: Type::CoercibleInteger,
                },
                Type::Integer { width, signed },
            ) => Some(ConstValue::integer(*value, *width, *signed)),
            (
                ConstValue {
                    kind: ConstValueKind::Float { value },
                    ty: Type::CoercibleFloat,
                },
                Type::Float { width },
            ) => Some(ConstValue::float(*value, *width)),
            (_, Type::Symbol(sym)) => {
                let sym = sym.borrow();
                if let ScopeValue::TypeAlias { ident, ty } = &sym.value {
                    return self.try_implicit_cast(ty);
                }
                None
            }
            _ => None,
        }
    }
}

impl Display for ConstValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.kind, f)
    }
}

impl NodeDisplay for ConstValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str("Const Value")
    }
}

impl TreeDisplay for ConstValue {
    fn num_children(&self) -> usize {
        2
    }

    fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay<()>> {
        match index {
            0 => Some(&self.ty),
            1 => Some(&self.kind),
            _ => None,
        }
    }
}

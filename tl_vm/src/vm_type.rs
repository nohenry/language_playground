use std::{
    fmt::{Debug, Display},
    hash::Hasher,
};

use linked_hash_map::LinkedHashMap;
use tl_evaluator::{
    evaluation_type::EvaluationType,
    scope::scope::{Scope, ScopeValue},
};
use tl_util::{
    format::{NodeDisplay, TreeDisplay},
    Rf,
};

use crate::vm_value::VmValue;

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
        // return_parameters: LinkedHashMap<String, Type>,
        return_type: Box<Type>,
    },
    String,
    Symbol(Rf<Scope<Self, VmValue>>),
    Ref {
        base_type: Box<Type>,
    },
    Ident(String),
    Tuple(Vec<Type>),
    StructInitializer {
        members: LinkedHashMap<String, Type>,
    },
    StructInstance {
        rf: Option<Rf<Scope<Self, VmValue>>>,
        members: LinkedHashMap<String, Type>,
    },
    Intrinsic(Rf<Scope<Self, VmValue>>),
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
                return_type,
            } => {
                parameters.hash(state);
                return_type.hash(state);
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
                    return_type: l_return_parameters,
                },
                Self::Function {
                    parameters: r_parameters,
                    return_type: r_return_parameters,
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

            (Self::Intrinsic(l), Self::Intrinsic(r)) => l == r,
            (Self::Intrinsic(l), Self::Symbol(r)) => l == r,
            (Self::Symbol(l), Self::Intrinsic(r)) => l == r,

            (Self::StructInstance { rf: Some(l_rf), .. }, Self::Symbol(sym)) => l_rf == sym,
            (Self::Symbol(sym), Self::StructInstance { rf: Some(l_rf), .. }) => sym == l_rf,

            (Self::Symbol(sym), right) | (right, Self::Symbol(sym)) => {
                let sym = sym.borrow();
                if let ScopeValue::TypeAlias { ty, .. } = &sym.value {
                    return &**ty == right;
                }
                false
            }

            (Self::Empty, Self::Empty) => true,
            (Self::Boolean, Self::Boolean) => true,
            (Self::String, Self::String) => true,
            (Self::Ref { base_type: bty_l }, Self::Ref { base_type: bty_r }) => bty_l == bty_r,
            _ => false, // _ => core::mem::discriminant(self) == core::mem::discriminant(other),
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
                return_type,
            } => {
                write!(f, "(")?;
                let mut iter = parameters.iter();

                if let Some(val) = iter.next() {
                    write!(f, "{} {}", val.1, val.0)?;
                }
                for val in iter {
                    write!(f, " ,{} {}", val.1, val.0)?;
                }
                write!(f, ") -> {}", return_type)
                // let mut iter = return_parameters.iter();

                // if let Some(val) = iter.next() {
                //     write!(f, "{} {}", val.1, val.0)?;
                // }
                // for val in iter {
                //     write!(f, " ,{} {}", val.1, val.0)?;
                // }
                // write!(f, ")")
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
            Self::Intrinsic(_) => {
                write!(f, "Intrinsic Data")
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
            Self::Intrinsic(_) => {
                write!(f, "Intrinsic Data")
            }
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
            // Type::Symbol(_) => 1,
            _ => 0,
        }
    }

    fn child_at(&self, _index: usize) -> Option<&dyn TreeDisplay> {
        match self {
            Type::Function {
                parameters,
                return_type,
            } => match _index {
                0 => Some(parameters),
                1 => Some(&**return_type),
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
            _ => panic!(),
        }
    }
}

impl EvaluationType for Type {
    type Value = VmValue;

    fn is_empty(&self) -> bool {
        match self {
            Type::Empty => true,
            _ => false,
        }
    }

    fn is_string(&self) -> bool {
        match self {
            Type::String => true,
            _ => false,
        }
    }

    fn is_integer(&self) -> bool {
        match self {
            Type::Integer { .. } => true,
            _ => false,
        }
    }

    fn is_cinteger(&self) -> bool {
        match self {
            Type::CoercibleInteger { .. } => true,
            _ => false,
        }
    }

    fn is_float(&self) -> bool {
        match self {
            Type::Float { .. } => true,
            _ => false,
        }
    }

    fn is_cfloat(&self) -> bool {
        match self {
            Type::CoercibleFloat { .. } => true,
            _ => false,
        }
    }

    fn is_bool(&self) -> bool {
        match self {
            Type::Boolean { .. } => true,
            _ => false,
        }
    }

    fn is_function(&self) -> bool {
        match self {
            Type::Function { .. } => true,
            _ => false,
        }
    }

    fn is_symbol(&self) -> bool {
        match self {
            Type::Symbol { .. } => true,
            _ => false,
        }
    }

    fn is_ref(&self) -> bool {
        match self {
            Type::Ref { .. } => true,
            _ => false,
        }
    }

    fn is_intrinsic(&self) -> bool {
        match self {
            Type::Intrinsic { .. } => true,
            _ => false,
        }
    }

    fn empty() -> Self {
        Type::Empty
    }

    fn string() -> Self {
        Type::String
    }

    fn integer(width: u8, signed: bool) -> Self {
        Type::Integer { width, signed }
    }

    fn cinteger() -> Self {
        Type::CoercibleInteger
    }

    fn float(width: u8) -> Self {
        Type::Float { width }
    }

    fn cfloat() -> Self {
        Type::CoercibleFloat
    }

    fn bool() -> Self {
        Type::Boolean
    }

    fn function() -> Self {
        // Type::Function { parameters: (), return_type: () }
        todo!()
    }

    fn symbol(symbol: tl_util::Rf<tl_evaluator::scope::scope::Scope<Self, Self::Value>>) -> Self {
        Type::Symbol(symbol)
    }

    fn rf(base_type: Self) -> Self {
        Type::Ref {
            base_type: Box::new(base_type),
        }
    }

    fn intrinsic(
        symbol: tl_util::Rf<tl_evaluator::scope::scope::Scope<Self, Self::Value>>,
    ) -> Self {
        Type::Intrinsic(symbol)
    }

    fn integer_width(&self) -> u8 {
        match self {
            Type::Integer { width, .. } => *width,
            _ => 0,
        }
    }

    fn integer_signed(&self) -> bool {
        match self {
            Type::Integer { signed, .. } => *signed,
            _ => false,
        }
    }

    fn float_width(&self) -> u8 {
        match self {
            Type::Float { width } => *width,
            _ => 0,
        }
    }

    fn function_parameters(self) -> impl Iterator<Item = (String, Self)> {
        match self {
            Type::Function { parameters, .. } => parameters.into_iter(),
            _ => panic!("Expected to be function!"),
        }
    }

    fn function_parameters_rf(&self) -> impl Iterator<Item = (&String, &Self)> {
        match self {
            Type::Function { parameters, .. } => parameters.iter(),
            _ => panic!("Expected to be function!"),
        }
    }

    fn set_function_parameters(&mut self, parameters: impl Iterator<Item = (String, Self)>) {
        match self {
            Type::Function { parameters: ps, .. } => {
                *ps = LinkedHashMap::from_iter(parameters);
            }
            _ => panic!("Expected to be function!"),
        }
    }

    fn function_return(&self) -> &Self {
        match self {
            Type::Function { return_type, .. } => &**return_type,
            _ => panic!("Expected to be function!"),
        }
    }

    fn set_function_return(&mut self, ty: Self) {
        match self {
            Type::Function { return_type, .. } => {
                *return_type = Box::new(ty);
            }
            _ => panic!("Expected to be function!"),
        }
    }

    fn symbol_rf(&self) -> &tl_util::Rf<tl_evaluator::scope::scope::Scope<Self, Self::Value>> {
        match self {
            Type::Symbol(sym) => sym,
            _ => panic!("Expected to be symbol!"),
        }
    }

    fn intrinsic_rf(&self) -> &tl_util::Rf<tl_evaluator::scope::scope::Scope<Self, Self::Value>> {
        match self {
            Type::Intrinsic(sym) => sym,
            _ => panic!("Expected to be intrinsic!"),
        }
    }
}

use std::fmt::{Debug, Display, Pointer};

use inkwell::{
    types::{
        AnyTypeEnum, BasicMetadataTypeEnum, BasicTypeEnum, FloatType, FunctionType, IntType,
        PointerType, StructType, VoidType,
    },
    AddressSpace,
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

use crate::{llvm_value::LlvmValue, LlvmContext};

#[derive(Clone, Eq)]
pub enum LlvmType<'a> {
    CoercibleInteger(IntType<'a>),
    CoercibleFloat(FloatType<'a>),
    Integer {
        signed: bool,
        llvm_type: IntType<'a>,
    },
    Float(FloatType<'a>),
    Boolean(IntType<'a>),
    Function {
        parameters: LinkedHashMap<String, LlvmType<'a>>,
        return_type: Box<LlvmType<'a>>,
        llvm_type: FunctionType<'a>,
    },
    Symbol(Rf<Scope<Self, LlvmValue<'a>>>),
    Ref {
        base_type: Box<LlvmType<'a>>,
        llvm_type: Option<PointerType<'a>>,
    },
    Tuple {
        types: Vec<LlvmType<'a>>,
        llvm_type: StructType<'a>,
    },
    StructInitializer {
        members: LinkedHashMap<String, LlvmType<'a>>,
        llvm_type: StructType<'a>,
    },
    StructInstance {
        rf: Option<Rf<Scope<Self, LlvmValue<'a>>>>,
        members: LinkedHashMap<String, LlvmType<'a>>,
        llvm_type: StructType<'a>,
    },
    Intrinsic(Rf<Scope<Self, LlvmValue<'a>>>),
    Empty(VoidType<'a>),
}

impl Debug for LlvmType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as NodeDisplay>::fmt(self, f)
    }
}

impl std::hash::Hash for LlvmType<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match &self {
            // LlvmType::Float(f) => state.write_u64(f. f.size_of().get_zero_extended_constant()),
            LlvmType::Integer { signed, llvm_type } => {
                state.write_u32(llvm_type.get_bit_width());
                state.write_u8(if *signed { 1 } else { 0 });
            }
            LlvmType::Function {
                parameters,
                return_type,
                llvm_type,
            } => {
                parameters.hash(state);
                return_type.hash(state);
            }
            LlvmType::StructInitializer { members, llvm_type } => {
                members.hash(state);
            }
            LlvmType::StructInstance { members, .. } => {
                members.hash(state);
            }
            LlvmType::Symbol(sym) => {
                Scope::hash(sym, state);
            }
            LlvmType::Intrinsic(sym) => {
                Scope::hash(sym, state);
            }
            _ => (),
        }
    }
}

impl PartialEq for LlvmType<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::Integer {
                    signed: l_signed,
                    llvm_type: lty,
                },
                Self::Integer {
                    signed: r_signed,
                    llvm_type: rty,
                },
            ) => lty.get_bit_width() == rty.get_bit_width() && l_signed == r_signed,
            (Self::Float(lty), Self::Float(rty)) => {
                lty == rty
                // self.get_float_width() == other.get_float_width()
            }
            (
                Self::Function {
                    parameters: l_parameters,
                    return_type: l_return_parameters,
                    ..
                },
                Self::Function {
                    parameters: r_parameters,
                    return_type: r_return_parameters,
                    ..
                },
            ) => l_parameters == r_parameters && l_return_parameters == r_return_parameters,
            (Self::Symbol(l0), Self::Symbol(r0)) => l0 == r0,
            (Self::Tuple { types: l0, .. }, Self::Tuple { types: r0, .. }) => l0 == r0,
            (
                Self::StructInstance {
                    rf: l_rf,
                    members: l_members,
                    ..
                },
                Self::StructInstance {
                    rf: r_rf,
                    members: r_members,
                    ..
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

            (Self::Empty(_), Self::Empty(_)) => true,
            (Self::Boolean(_), Self::Boolean(_)) => true,
            // (Self::String, Self::String) => true,
            (
                Self::Ref {
                    base_type: bty_l, ..
                },
                Self::Ref {
                    base_type: bty_r, ..
                },
            ) => bty_l == bty_r,
            _ => false, // _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl Display for LlvmType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self, f)
    }
}

impl NodeDisplay for LlvmType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Boolean { .. } => f.write_str("bool"),
            // Self::String => f.write_str("string"),
            Self::Symbol(ty) => {
                let ty = ty.borrow();
                write!(f, "Symbol {}", ty.name)
            }
            Self::StructInitializer { .. } => write!(f, "Struct Initializer"),
            Self::StructInstance { .. } => write!(f, "Struct Instance"),
            Self::Tuple { .. } => write!(f, "Tuple"),
            Self::Empty { .. } => write!(f, "Empty"),
            Self::CoercibleInteger(_) => write!(f, "Coercible Integer"),
            Self::CoercibleFloat(_) => write!(f, "Coercible Float"),
            Self::Ref { .. } => write!(f, "Reference"),
            Self::Float { .. } => write!(f, "f"),
            Self::Integer {
                signed: true,
                llvm_type,
            } => write!(f, "i{}", llvm_type.get_bit_width()),
            Self::Integer {
                signed: false,
                llvm_type,
            } => write!(f, "u{}", llvm_type.get_bit_width()),
            Self::Function { .. } => f.write_str("Function"),
            // Self::Ident(ident) => f.write_str(ident),
            Self::Intrinsic(_) => {
                write!(f, "Intrinsic Data")
            }
        }
    }
}

impl TreeDisplay for LlvmType<'_> {
    fn num_children(&self) -> usize {
        match self {
            LlvmType::Function { .. } => 2,
            LlvmType::Tuple { types, .. } => types.len(),
            LlvmType::StructInstance { members, .. } => members.len(),
            LlvmType::StructInitializer { members, .. } => members.len(),
            LlvmType::Ref { .. } => 1,
            // Type::Symbol(_) => 1,
            _ => 0,
        }
    }

    fn child_at(&self, _index: usize) -> Option<&dyn TreeDisplay> {
        match self {
            LlvmType::Function {
                parameters,
                return_type,
                ..
            } => match _index {
                0 => Some(parameters),
                1 => Some(&**return_type),
                _ => None,
            },
            LlvmType::Tuple { types, .. } => {
                if let Some(ty) = types.get(_index) {
                    Some(ty)
                } else {
                    None
                }
            }
            LlvmType::StructInstance { .. } => None,
            // Type::Ref {  } => None,
            LlvmType::Ref { base_type, .. } => Some(&**base_type),

            _ => None,
        }
    }

    fn child_at_bx<'a>(&'a self, _index: usize) -> Box<dyn TreeDisplay<()> + 'a> {
        match self {
            LlvmType::StructInstance { members, .. } => members.child_at_bx(_index),
            LlvmType::StructInitializer { members, .. } => members.child_at_bx(_index),
            _ => panic!(),
        }
    }
}

impl<'a> EvaluationType for LlvmType<'a> {
    type Value = LlvmValue<'a>;

    fn is_empty(&self) -> bool {
        match self {
            LlvmType::Empty(_) => true,
            _ => false,
        }
    }

    fn is_string(&self) -> bool {
        match self {
            // LlvmType::String => true,
            _ => false,
        }
    }

    fn is_integer(&self) -> bool {
        match self {
            LlvmType::Integer { .. } => true,
            _ => false,
        }
    }

    fn is_cinteger(&self) -> bool {
        match self {
            LlvmType::CoercibleInteger { .. } => true,
            _ => false,
        }
    }

    fn is_float(&self) -> bool {
        match self {
            LlvmType::Float { .. } => true,
            _ => false,
        }
    }

    fn is_cfloat(&self) -> bool {
        match self {
            LlvmType::CoercibleFloat { .. } => true,
            _ => false,
        }
    }

    fn is_bool(&self) -> bool {
        match self {
            LlvmType::Boolean { .. } => true,
            _ => false,
        }
    }

    fn is_function(&self) -> bool {
        match self {
            LlvmType::Function { .. } => true,
            _ => false,
        }
    }

    fn is_symbol(&self) -> bool {
        match self {
            LlvmType::Symbol { .. } => true,
            _ => false,
        }
    }

    fn is_ref(&self) -> bool {
        match self {
            LlvmType::Ref { .. } => true,
            _ => false,
        }
    }

    fn is_intrinsic(&self) -> bool {
        match self {
            LlvmType::Intrinsic { .. } => true,
            _ => false,
        }
    }

    fn integer_width(&self) -> u8 {
        match self {
            LlvmType::Integer { llvm_type, .. } => llvm_type.get_bit_width() as _,
            _ => 0,
        }
    }

    fn integer_signed(&self) -> bool {
        match self {
            LlvmType::Integer { signed, .. } => *signed,
            _ => false,
        }
    }

    fn float_width(&self) -> u8 {
        0
    }

    fn function_parameters(self) -> impl Iterator<Item = (String, Self)> {
        match self {
            LlvmType::Function { parameters, .. } => parameters.into_iter(),
            _ => panic!("Expected to be function!"),
        }
    }

    fn function_parameters_rf(&self) -> impl Iterator<Item = (&String, &Self)> {
        match self {
            LlvmType::Function { parameters, .. } => parameters.iter(),
            _ => panic!("Expected to be function!"),
        }
    }

    fn set_function_parameters(&mut self, parameters: impl Iterator<Item = (String, Self)>) {
        match self {
            LlvmType::Function { parameters: ps, .. } => {
                *ps = LinkedHashMap::from_iter(parameters);
            }
            _ => panic!("Expected to be function!"),
        }
    }

    fn function_return(&self) -> &Self {
        match self {
            LlvmType::Function { return_type, .. } => &**return_type,
            _ => panic!("Expected to be function!"),
        }
    }

    fn set_function_return(&mut self, ty: Self) {
        match self {
            LlvmType::Function { return_type, .. } => {
                *return_type = Box::new(ty);
            }
            _ => panic!("Expected to be function!"),
        }
    }

    fn symbol_rf(&self) -> &tl_util::Rf<tl_evaluator::scope::scope::Scope<Self, Self::Value>> {
        match self {
            LlvmType::Symbol(sym) => sym,
            _ => panic!("Expected to be symbol!"),
        }
    }

    fn intrinsic_rf(&self) -> &tl_util::Rf<tl_evaluator::scope::scope::Scope<Self, Self::Value>> {
        match self {
            LlvmType::Intrinsic(sym) => sym,
            _ => panic!("Expected to be intrinsic!"),
        }
    }
}

impl<'a> LlvmType<'a> {
    pub fn llvm_type(&self) -> Option<AnyTypeEnum<'a>> {
        let ty = match self {
            LlvmType::Empty(void) => (*void).into(),
            LlvmType::Boolean(bool) => (*bool).into(),
            LlvmType::Float(f) => (*f).into(),
            LlvmType::Integer { llvm_type, .. } => (*llvm_type).into(),
            LlvmType::CoercibleInteger(llvm_type) => (*llvm_type).into(),
            LlvmType::CoercibleFloat(llvm_type) => (*llvm_type).into(),
            LlvmType::Function { llvm_type, .. } => (*llvm_type).into(),
            LlvmType::Ref {
                llvm_type: Some(llvm_type),
                ..
            } => (*llvm_type).into(),
            LlvmType::StructInitializer { llvm_type, .. } => (*llvm_type).into(),
            LlvmType::StructInstance { llvm_type, .. } => (*llvm_type).into(),
            LlvmType::Tuple { llvm_type, .. } => (*llvm_type).into(),
            _ => return None,
        };

        Some(ty)
    }

    pub fn llvm_basic_type(&self) -> Option<BasicTypeEnum<'a>> {
        let ty = match self {
            LlvmType::Boolean(bool) => (*bool).into(),
            LlvmType::Float(f) => (*f).into(),
            LlvmType::Integer { llvm_type, .. } => (*llvm_type).into(),
            LlvmType::CoercibleInteger(llvm_type) => (*llvm_type).into(),
            LlvmType::CoercibleFloat(llvm_type) => (*llvm_type).into(),
            LlvmType::Ref {
                llvm_type: Some(llvm_type),
                ..
            } => (*llvm_type).into(),
            LlvmType::StructInitializer { llvm_type, .. } => (*llvm_type).into(),
            LlvmType::StructInstance { llvm_type, .. } => (*llvm_type).into(),
            LlvmType::Tuple { llvm_type, .. } => (*llvm_type).into(),
            _ => return None,
        };

        Some(ty)
    }

    pub fn llvm_fn_type(&self, params: &[BasicMetadataTypeEnum<'a>]) -> Option<FunctionType<'a>> {
        let ty = match self {
            LlvmType::Empty(void) => (*void).fn_type(params, false),
            LlvmType::Boolean(bool) => (*bool).fn_type(params, false),
            LlvmType::Float(f) => (*f).fn_type(params, false),
            LlvmType::Integer { llvm_type, .. } => (*llvm_type).fn_type(params, false),
            LlvmType::CoercibleInteger(llvm_type) => (*llvm_type).fn_type(params, false),
            LlvmType::CoercibleFloat(llvm_type) => (*llvm_type).fn_type(params, false),
            // LlvmType::Function { llvm_type, .. } => (*llvm_type).fn_type(params, false),
            LlvmType::Ref {
                llvm_type: Some(llvm_type),
                ..
            } => (*llvm_type).fn_type(params, false),
            LlvmType::StructInitializer { llvm_type, .. } => (*llvm_type).fn_type(params, false),
            LlvmType::StructInstance { llvm_type, .. } => (*llvm_type).fn_type(params, false),
            LlvmType::Tuple { llvm_type, .. } => (*llvm_type).fn_type(params, false),
            _ => return None,
        };

        Some(ty)
    }

    pub fn llvm_ptr_ty(&self, addrspac: AddressSpace) -> Option<PointerType<'a>> {
        let llvm_type = self.llvm_type()?;
        let ty = match llvm_type {
            AnyTypeEnum::ArrayType(array) => array.ptr_type(addrspac),
            AnyTypeEnum::FloatType(f) => f.ptr_type(addrspac),
            AnyTypeEnum::IntType(i) => i.ptr_type(addrspac),
            AnyTypeEnum::PointerType(p) => p.ptr_type(addrspac),
            AnyTypeEnum::StructType(p) => p.ptr_type(addrspac),
            AnyTypeEnum::VectorType(v) => v.ptr_type(addrspac),
            _ => return None,
        };

        Some(ty)
    }

    pub fn get_float_width(&self, ctx: &LlvmContext<'a>) -> u8 {
        match self {
            Self::Float(f) | Self::CoercibleFloat(f) => {
                if f == &ctx.context.f128_type() {
                    128
                } else if f == &ctx.context.f64_type() {
                    64
                } else if f == &ctx.context.f32_type() {
                    32
                } else if f == &ctx.context.f16_type() {
                    16
                } else {
                    0
                }
            }
            _ => 0,
        }
    }
}

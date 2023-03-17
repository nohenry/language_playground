use std::fmt::{Debug, Display, Pointer};

use inkwell::types::{FloatType, FunctionType, IntType, PointerType, StructType, VoidType};
use linked_hash_map::LinkedHashMap;
use tl_evaluator::{evaluation_type::EvaluationType, scope::scope::Scope};
use tl_util::{
    format::{NodeDisplay, TreeDisplay},
    Rf,
};

use crate::llvm_value::LlvmValue;

#[derive(Clone, PartialEq, Eq)]
pub enum LlvmType {
    CoercibleInteger,
    CoercibleFloat,
    Integer {
        signed: bool,
        llvm_type: IntType<'static>,
    },
    Float(FloatType<'static>),
    Boolean(IntType<'static>),
    Function {
        parameters: LinkedHashMap<String, LlvmType>,
        return_type: Box<LlvmType>,
        llvm_type: FunctionType<'static>,
    },
    Symbol(Rf<Scope<Self, LlvmValue>>),
    Ref {
        base_type: Box<LlvmType>,
        llvm_type: PointerType<'static>,
    },
    Tuple {
        types: Vec<LlvmType>,
        llvm_type: StructType<'static>,
    },
    StructInitializer {
        members: LinkedHashMap<String, LlvmType>,
        llvm_type: StructType<'static>,
    },
    StructInstance {
        rf: Option<Rf<Scope<Self, LlvmValue>>>,
        members: LinkedHashMap<String, LlvmType>,
        llvm_type: StructType<'static>,
    },
    Intrinsic(Rf<Scope<Self, LlvmValue>>),
    Empty(VoidType<'static>),
}

impl Debug for LlvmType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as NodeDisplay>::fmt(self, f)
    }
}

impl std::hash::Hash for LlvmType {
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

impl Display for LlvmType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self, f)
    }
}

impl NodeDisplay for LlvmType {
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
            Self::CoercibleInteger => write!(f, "Coercible Integer"),
            Self::CoercibleFloat => write!(f, "Coercible Float"),
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

impl TreeDisplay for LlvmType {
    fn num_children(&self) -> usize {
        0
    }

    fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay<()>> {
        None
    }
}

impl EvaluationType for LlvmType {
    type Value = LlvmValue;

    fn is_empty(&self) -> bool {
        todo!()
    }

    fn is_string(&self) -> bool {
        todo!()
    }

    fn is_integer(&self) -> bool {
        todo!()
    }

    fn is_cinteger(&self) -> bool {
        todo!()
    }

    fn is_float(&self) -> bool {
        todo!()
    }

    fn is_cfloat(&self) -> bool {
        todo!()
    }

    fn is_bool(&self) -> bool {
        todo!()
    }

    fn is_function(&self) -> bool {
        todo!()
    }

    fn is_symbol(&self) -> bool {
        todo!()
    }

    fn is_ref(&self) -> bool {
        todo!()
    }

    fn is_intrinsic(&self) -> bool {
        todo!()
    }

    fn integer_width(&self) -> u8 {
        todo!()
    }

    fn integer_signed(&self) -> bool {
        todo!()
    }

    fn float_width(&self) -> u8 {
        todo!()
    }

    fn function_parameters(self) -> impl Iterator<Item = (String, Self)> {
        [].into_iter()
    }

    fn function_parameters_rf(&self) -> impl Iterator<Item = (&String, &Self)> {
        [].into_iter()
    }

    fn set_function_parameters(&mut self, parameters: impl Iterator<Item = (String, Self)>) {
        todo!()
    }

    fn function_return(&self) -> &Self {
        todo!()
    }

    fn set_function_return(&mut self, ty: Self) {
        todo!()
    }

    fn symbol_rf(&self) -> &tl_util::Rf<tl_evaluator::scope::scope::Scope<Self, Self::Value>> {
        todo!()
    }

    fn intrinsic_rf(&self) -> &tl_util::Rf<tl_evaluator::scope::scope::Scope<Self, Self::Value>> {
        todo!()
    }
}

use std::{
    fmt::{Debug, Display, Write},
    sync::Arc,
};

use inkwell::{
    types::StructType,
    values::{FloatValue, FunctionValue, IntValue, PointerValue, StructValue},
    AddressSpace,
};
use linked_hash_map::LinkedHashMap;
use tl_core::ast::{Statement, Type};
use tl_evaluator::{
    evaluation_type::{EvaluationTypeProvider, EvaluationType},
    evaluation_value::EvaluationValue,
    scope::{
        intrinsics::IntrinsicType,
        scope::{Scope, ScopeValue},
    },
};
use tl_util::{
    format::{NodeDisplay, TreeDisplay},
    Rf,
};

use crate::llvm_type::LlvmType;

#[derive(Clone)]
pub enum LlvmValueKind<'a> {
    Empty,
    Integer {
        value: u64,
        llvm_value: IntValue<'a>,
    },
    Float {
        value: f64,
        llvm_value: FloatValue<'a>,
    },
    String {
        string: String,
    },
    Bool {
        value: bool,
        llvm_value: IntValue<'a>,
    },
    Ref {
        // symbol: Rf<Scope>,
        base: Box<LlvmValue<'a>>,
        offset: Option<String>,
        llvm_value: PointerValue<'a>,
    },
    Function {
        rf: Rf<Scope<LlvmType<'a>, LlvmValue<'a>>>,
        body: Statement,
        llvm_value: FunctionValue<'a>,
    },
    NativeFunction {
        rf: Rf<Scope<LlvmType<'a>, LlvmValue<'a>>>,
        callback: Arc<dyn Fn(&LinkedHashMap<String, LlvmValue<'a>>) -> LlvmValue<'a> + Sync + Send>,
    },
    Symbol(Rf<Scope<LlvmType<'a>, LlvmValue<'a>>>),
    Tuple(Vec<LlvmValue<'a>>, StructValue<'a>),
    StructInitializer {
        members: LinkedHashMap<String, LlvmValue<'a>>,
        llvm_value: StructValue<'a>,
    },
    StructInstance {
        rf: Rf<Scope<LlvmType<'a>, LlvmValue<'a>>>,
        members: LinkedHashMap<String, LlvmValue<'a>>,
        llvm_value: StructValue<'a>,
    },
    IntrinsicStorage(Rf<dyn IntrinsicType + Sync + Send>, Vec<LlvmType<'a>>),
}

impl Display for LlvmValueKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LlvmValueKind::Empty => f.write_str("()"),
            LlvmValueKind::Bool { value, .. } => write!(f, "{value}"),
            LlvmValueKind::Integer { value, .. } => write!(f, "{value}"),
            LlvmValueKind::Float { value, .. } => write!(f, "{value}"),
            LlvmValueKind::String { string } => write!(f, "{string}"),
            LlvmValueKind::Ref { base, offset, .. } => {
                write!(f, "{}", base)?;
                // if let ScopeValue::ConstValue(cv) = &symbol.borrow().value {
                if let Some(offset) = offset {
                    return write!(f, ".{}@", offset);
                } else {
                    return f.write_char('@');
                }
                // }
            }
            LlvmValueKind::Function { body, .. } => write!(f, "{}", body.format()),
            LlvmValueKind::NativeFunction { .. } => write!(f, "Native Function"),
            LlvmValueKind::Tuple(list, _) => {
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
            LlvmValueKind::StructInstance { members, .. }
            | LlvmValueKind::StructInitializer { members, .. } => {
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
            LlvmValueKind::Symbol(_) => f.write_str("Symbol"),
            LlvmValueKind::IntrinsicStorage(_, _) => f.write_str("Intrinsic"),
        }
    }
}

impl<'a> LlvmValueKind<'a> {
    pub fn as_integer(&self) -> u64 {
        match self {
            LlvmValueKind::Integer { value, .. } => *value,
            _ => panic!(),
        }
    }

    pub fn as_float(&self) -> f64 {
        match self {
            LlvmValueKind::Float { value, .. } => *value,
            _ => panic!(),
        }
    }

    pub fn as_record_instance(
        &self,
    ) -> (
        &Rf<Scope<LlvmType<'a>, LlvmValue<'a>>>,
        &LinkedHashMap<String, LlvmValue<'a>>,
    ) {
        match self {
            LlvmValueKind::StructInstance { rf, members, .. } => (rf, members),
            _ => panic!(),
        }
    }

    pub fn as_empty(&self) -> bool {
        matches!(self, LlvmValueKind::Empty)
    }
}

impl NodeDisplay for LlvmValueKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LlvmValueKind::Empty => write!(f, "Empty"),
            LlvmValueKind::Integer { value, .. } => write!(f, "Integer: {value}"),
            LlvmValueKind::Bool { value, .. } => write!(f, "Boolean: {value}"),
            LlvmValueKind::Float { value, .. } => write!(f, "Float: {value}"),
            LlvmValueKind::Ref { .. } => write!(f, "Reference"),
            LlvmValueKind::String { string } => write!(f, "String: {string}"),
            LlvmValueKind::Function { .. } => write!(f, "Function"),
            LlvmValueKind::NativeFunction { .. } => write!(f, "Native Function"),
            LlvmValueKind::Tuple(_, _) => write!(f, "Tuple"),
            LlvmValueKind::StructInitializer { .. } => write!(f, "Struct Initializer"),
            LlvmValueKind::StructInstance { .. } => write!(f, "Record Instance"),
            LlvmValueKind::Symbol(_) => write!(f, "Symbol"),
            LlvmValueKind::IntrinsicStorage(_, _) => write!(f, "Intrinsic"),
        }
    }
}

impl TreeDisplay for LlvmValueKind<'_> {
    fn num_children(&self) -> usize {
        match self {
            LlvmValueKind::Function { .. } => 1,
            LlvmValueKind::Tuple(list, _) => list.len(),
            LlvmValueKind::StructInitializer { members, .. } => members.len(),
            LlvmValueKind::StructInstance { members, .. } => members.len(),
            LlvmValueKind::Ref {
                offset: Some(_), ..
            } => 2,
            LlvmValueKind::Ref { .. } => 1,
            LlvmValueKind::Symbol(_) => 1,
            _ => 0,
        }
    }

    fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay<()>> {
        match self {
            LlvmValueKind::Function { body, .. } => match index {
                0 => Some(body),
                _ => None,
            },
            LlvmValueKind::Tuple(tu, _) => {
                if let Some(val) = tu.get(index) {
                    Some(val)
                } else {
                    None
                }
            }
            LlvmValueKind::StructInstance { .. } => None,
            LlvmValueKind::Ref { base, offset, .. } => match index {
                0 => Some(&**base),
                1 => offset.as_ref().map::<&dyn TreeDisplay, _>(|f| f),
                _ => None,
            },
            _ => None,
        }
    }

    fn child_at_bx<'a>(&'a self, index: usize) -> Box<dyn TreeDisplay<()> + 'a> {
        match self {
            LlvmValueKind::StructInstance { members, .. } => members.child_at_bx(index),
            LlvmValueKind::StructInitializer { members, .. } => members.child_at_bx(index),
            LlvmValueKind::Symbol(sym) => Box::new(sym.borrow()),
            // ConstValueKind::Ref { base, offset } => Box::new(base),
            _ => panic!(),
        }
    }
}

#[derive(Clone)]
pub struct LlvmValue<'a> {
    ty: LlvmType<'a>,
    // llvm_value: inkwell::values::AnyValueEnum<'a>,
    kind: LlvmValueKind<'a>,
}

impl Display for LlvmValue<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        NodeDisplay::fmt(self, f)
    }
}

impl Debug for LlvmValue<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        NodeDisplay::fmt(self, f)
    }
}

impl NodeDisplay for LlvmValue<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // Display::fmt(&self, f)
        Ok(())
    }
}

impl TreeDisplay for LlvmValue<'_> {
    fn num_children(&self) -> usize {
        0
    }

    fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay<()>> {
        None
    }
}

impl<'a> EvaluationValue for LlvmValue<'a> {
    type Type = LlvmType<'a>;

    fn get_struct_members_rf(&self) -> impl Iterator<Item = (&String, &Self)> {
        [].into_iter()
    }

    fn get_struct_members(self) -> impl Iterator<Item = (String, Self)> {
        [].into_iter()
    }

    fn get_struct_member(&self, name: &str) -> Option<&Self> {
        todo!()
    }

    fn get_struct_member_mut(&mut self, name: &str) -> Option<&mut Self> {
        todo!()
    }

    fn create_struct_instance<'b>(
        sym: tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>>,
        tp: &impl EvaluationTypeProvider<'b, Type = Self::Type>,
    ) -> Self {
        todo!()
    }

    fn is_struct_instance(&self) -> bool {
        todo!()
    }

    fn create_struct_initializer<'b>(
        values: LinkedHashMap<String, Self>,
        tp: &impl EvaluationTypeProvider<'b, Type = Self::Type>,
    ) -> Self {
        todo!()
    }

    fn is_struct_initializer(&self) -> bool {
        todo!()
    }

    fn empty<'b>(tp: &impl EvaluationTypeProvider<'b, Type = Self::Type>) -> Self {
        LlvmValue {
            ty: tp.empty(),
            kind: LlvmValueKind::Empty,
        }
    }

    fn is_empty(&self) -> bool {
        match &self.kind {
            LlvmValueKind::Empty => true,
            _ => false,
        }
    }

    fn default_for(ty: &Self::Type) -> Self {
        let kind = match ty {
            LlvmType::Empty(_) => LlvmValueKind::Empty,
            LlvmType::Integer { llvm_type, .. } => LlvmValueKind::Integer { value: 0, llvm_value: llvm_type.const_zero() },
            LlvmType::Float(llvm_type) => LlvmValueKind::Float { value: 0.0, llvm_value: llvm_type.const_zero() },
            _ => LlvmValueKind::Empty,
        };

        LlvmValue {
            ty: ty.clone(),
            kind,
        }
    }

    fn resolve_ref(
        &self,
    ) -> Option<tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>>> {
        match (&self.ty, &self.kind) {
            (
                LlvmType::Ref {
                    base_type: box LlvmType::Ref { .. },
                    ..
                },
                LlvmValueKind::Symbol(sym),
            ) => {
                let value = sym.borrow();
                if let ScopeValue::EvaluationValue(cv) = &value.value {
                    return cv.resolve_ref();
                }
                return Some(sym.clone());
            }
            (LlvmType::Ref { .. }, LlvmValueKind::Symbol(sym)) => return Some(sym.clone()),
            (
                LlvmType::Ref { .. },
                LlvmValueKind::Ref {
                    base, offset: None, ..
                },
            ) => {
                return base.resolve_ref();
            }
            (
                LlvmType::Ref { .. },
                LlvmValueKind::Ref {
                    base,
                    offset: Some(index),
                    ..
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

    fn resolve_ref_value(&self) -> Option<Self> {
        let Some(sym) = self.resolve_ref() else {
            return None
        };

        let value = sym.borrow();

        let ScopeValue::EvaluationValue(cv) = &value.value else {
            return None;
        };

        Some(cv.clone())
    }

    fn string<'b>(str: String, tp: &impl EvaluationTypeProvider<'b, Type = Self::Type>) -> Self {
        todo!()
    }

    fn is_string(&self) -> bool {
        todo!()
    }

    fn string_value(&self) -> &str {
        todo!()
    }

    fn integer<'b>(
        value: u64,
        width: u8,
        signed: bool,
        tp: &impl EvaluationTypeProvider<'b, Type = Self::Type>,
    ) -> Self {
        let ty = tp.integer(width, signed);
        let val = ty
            .llvm_type()
            .unwrap()
            .into_int_type()
            .const_int(value, false);

        LlvmValue {
            ty,
            kind: LlvmValueKind::Integer {
                value,
                llvm_value: val,
            },
        }
    }

    fn is_integer(&self) -> bool {
        match &self.kind {
            LlvmValueKind::Integer { .. } => true,
            _ => false,
        }
    }

    fn integer_width(&self) -> u8 {
        self.get_type().integer_width()
    }

    fn is_signed(&self) -> bool {
        self.get_type().integer_signed()
    }

    fn integer_value(&self) -> u64 {
        match &self.kind {
            LlvmValueKind::Integer { value, .. } => *value,
            _ => 0,
        }
    }

    fn cinteger<'b>(value: u64, tp: &impl EvaluationTypeProvider<'b, Type = Self::Type>) -> Self {
        let ty = tp.cinteger();
        let val = ty
            .llvm_type()
            .unwrap()
            .into_int_type()
            .const_int(value, false);

        LlvmValue {
            ty,
            kind: LlvmValueKind::Integer {
                value,
                llvm_value: val.into(),
            },
        }
    }

    fn is_cinteger(&self) -> bool {
        match &self.ty {
            LlvmType::CoercibleInteger(_) => true,
            _ => false,
        }
    }

    fn float<'b>(
        value: f64,
        width: u8,
        tp: &impl EvaluationTypeProvider<'b, Type = Self::Type>,
    ) -> Self {
        let ty = tp.float(width);
        let val = ty.llvm_type().unwrap().into_float_type().const_float(value);

        LlvmValue {
            ty,
            kind: LlvmValueKind::Float {
                value,
                llvm_value: val.into(),
            },
        }
    }

    fn is_float(&self) -> bool {
        match &self.kind {
            LlvmValueKind::Integer { .. } => true,
            _ => false,
        }
    }

    fn float_width(&self) -> u8 {
        self.ty.get_float_width()
    }

    fn float_value(&self) -> f64 {
        match &self.kind {
            LlvmValueKind::Float { value, .. } => *value,
            _ => 0.0,
        }
    }

    fn cfloat<'b>(value: f64, tp: &impl EvaluationTypeProvider<'b, Type = Self::Type>) -> Self {
        let ty = tp.cfloat();
        let val = ty.llvm_type().unwrap().into_float_type().const_float(value);

        LlvmValue {
            ty,
            kind: LlvmValueKind::Float {
                value,
                llvm_value: val.into(),
            },
        }
    }

    fn is_cfloat(&self) -> bool {
        match &self.ty {
            LlvmType::CoercibleFloat(_) => true,
            _ => false,
        }
    }

    fn bool<'b>(value: bool, tp: &impl EvaluationTypeProvider<'b, Type = Self::Type>) -> Self {
        let ty = tp.bool();
        let val = ty
            .llvm_type()
            .unwrap()
            .into_int_type()
            .const_int(if value { 1 } else { 0 }, false);

        LlvmValue {
            ty,
            kind: LlvmValueKind::Bool {
                value,
                llvm_value: val.into(),
            },
        }
    }

    fn is_bool(&self) -> bool {
        match &self.kind {
            LlvmValueKind::Bool { .. } => true,
            _ => false,
        }
    }

    fn bool_value(&self) -> bool {
        match &self.kind {
            LlvmValueKind::Bool { value, .. } => *value,
            _ => false,
        }
    }

    fn function<'b>(
        body: tl_core::ast::Statement,
        parameters: LinkedHashMap<String, Self::Type>,
        return_type: Self::Type,
        node: tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>>,
        tp: &impl EvaluationTypeProvider<'b, Type = Self::Type>,
    ) -> Self {
        todo!()
    }

    fn is_function(&self) -> bool {
        match &self.kind {
            LlvmValueKind::Function { .. } => true,
            _ => false,
        }
    }

    fn function_body(&self) -> &tl_core::ast::Statement {
        match &self.kind {
            LlvmValueKind::Function { body, .. } => body,
            _ => panic!("Expected to be function!"),
        }
    }

    fn function_rf(&self) -> &tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>> {
        match &self.kind {
            LlvmValueKind::Function { rf, .. } => rf,
            _ => panic!("Expected to be function!"),
        }
    }

    fn native_function<'b>(
        callback: std::sync::Arc<dyn Fn(&LinkedHashMap<String, Self>) -> Self + Sync + Send>,
        parameters: LinkedHashMap<String, Self::Type>,
        return_type: Self::Type,
        node: tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>>,
        tp: &impl EvaluationTypeProvider<'b, Type = Self::Type>,
    ) -> Self {
        todo!()
    }

    fn is_native_function(&self) -> bool {
        match &self.kind {
            LlvmValueKind::NativeFunction { .. } => true,
            _ => false,
        }
    }

    fn native_function_callback(
        &self,
    ) -> &std::sync::Arc<dyn Fn(&LinkedHashMap<String, Self>) -> Self + Sync + Send> {
        match &self.kind {
            LlvmValueKind::NativeFunction { callback, .. } => callback,
            _ => panic!("Expected to be native function!"),
        }
    }

    fn native_function_rf(
        &self,
    ) -> &tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>> {
        match &self.kind {
            LlvmValueKind::NativeFunction { rf, .. } => rf,
            _ => panic!("Expected to be native function!"),
        }
    }

    fn tuple<'b>(
        values: Vec<Self>,
        tp: &impl EvaluationTypeProvider<'b, Type = Self::Type>,
    ) -> Self {
        todo!()
    }

    fn is_tuple(&self) -> bool {
        match &self.kind {
            LlvmValueKind::Tuple(_, _) => true,
            _ => false,
        }
    }

    fn tuple_value(self) -> Vec<Self> {
        match self.kind {
            LlvmValueKind::Tuple(s, _) => s,
            _ => panic!("Expected to be tuple!"),
        }
    }

    fn tuple_value_rf(&self) -> &Vec<Self> {
        match &self.kind {
            LlvmValueKind::Tuple(s, _) => s,
            _ => panic!("Expected to be tuple!"),
        }
    }

    fn reference<'b>(
        left: Self,
        right: String,
        right_ty: Self::Type,
        tp: &impl EvaluationTypeProvider<'b, Type = Self::Type>,
    ) -> Self {
        let rtype = right_ty.llvm_ptr_ty(AddressSpace::default());
        // let pt = left.
        todo!()
        // LlvmValue {
        //     ty: LlvmType::Ref {
        //         base_type: Box::new(right_ty),
        //         llvm_type: rtype
        //     },
        //     kind: LlvmValueKind::Ref {
        //         base: Box::new(left),
        //         offset: Some(right),
        //         llvm_value:
        //     },
        // }
    }

    fn is_reference(&self) -> bool {
        match &self.kind {
            LlvmValueKind::Ref { .. } => true,
            _ => false,
        }
    }

    fn sym_reference<'b>(
        sym: &tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>>,
        ty: Self::Type,
        tp: &impl EvaluationTypeProvider<'b, Type = Self::Type>,
    ) -> Self {
        todo!()
    }

    fn intrinsic_storage<'b>(
        sym: tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>>,
        storage: tl_util::Rf<dyn tl_evaluator::scope::intrinsics::IntrinsicType + Sync + Send>,
        generics: Vec<Self::Type>,
        tp: &impl EvaluationTypeProvider<'b, Type = Self::Type>,
    ) -> Self {
        LlvmValue {
            kind: LlvmValueKind::IntrinsicStorage(storage, generics),
            ty: LlvmType::Intrinsic(sym),
        }
    }

    fn try_implicit_cast<'b>(
        &self,
        ty: &Self::Type,
        tp: &impl EvaluationTypeProvider<'b, Type = Self::Type>,
    ) -> Option<Self> {
        match (self, ty) {
            (
                LlvmValue {
                    kind: LlvmValueKind::Integer { value, .. },
                    ty: LlvmType::CoercibleInteger(_),
                },
                LlvmType::Integer { signed, llvm_type },
            ) => Some(LlvmValue::integer(
                *value,
                llvm_type.get_bit_width() as _,
                *signed,
                tp,
            )),
            (
                LlvmValue {
                    kind: LlvmValueKind::Float { value, .. },
                    ty: LlvmType::CoercibleFloat(_),
                },
                LlvmType::Float(f),
            ) => Some(LlvmValue::float(*value, ty.get_float_width(), tp)),
            (_, LlvmType::Symbol(sym)) => {
                let sym = sym.borrow();
                if let ScopeValue::TypeAlias { ty, .. } = &sym.value {
                    return self.try_implicit_cast(ty, tp);
                }
                None
            }
            _ => None,
        }
    }

    fn get_type(&self) -> &Self::Type {
        &self.ty
    }

    fn get_type_mut(&mut self) -> &mut Self::Type {
        &mut self.ty
    }

    fn to_type(self) -> Self::Type {
        self.ty
    }
}

impl<'a> Into<LlvmType<'a>> for LlvmValue<'a> {
    fn into(self) -> LlvmType<'a> {
        self.to_type()
    }
}

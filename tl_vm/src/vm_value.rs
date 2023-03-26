use std::{
    fmt::{Display, Write},
    sync::Arc,
};

use linked_hash_map::LinkedHashMap;
use tl_core::ast::Statement;
use tl_util::{
    format::{NodeDisplay, TreeDisplay},
    Rf,
};

use tl_evaluator::{
    evaluation_type::EvaluationType,
    scope::scope::{Scope, ScopeValue},
};
use tl_evaluator::{evaluation_value::EvaluationValue, scope::intrinsics::IntrinsicType};

use crate::vm_type::Type;

#[derive(Clone)]
pub enum VmValueKind {
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
        base: Box<VmValue>,
        offset: Option<String>,
    },
    Function {
        rf: Rf<Scope<Type, VmValue>>,
        body: Statement,
    },
    NativeFunction {
        rf: Rf<Scope<Type, VmValue>>,
        callback: Arc<dyn Fn(&LinkedHashMap<String, VmValue>) -> VmValue + Sync + Send>,
    },
    Symbol(Rf<Scope<Type, VmValue>>),
    Tuple(Vec<VmValue>),
    StructInitializer {
        members: LinkedHashMap<String, VmValue>,
    },
    StructInstance {
        rf: Rf<Scope<Type, VmValue>>,
        members: LinkedHashMap<String, VmValue>,
    },
    IntrinsicStorage(Rf<dyn IntrinsicType + Sync + Send>, Vec<Type>),
}

impl Display for VmValueKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VmValueKind::Empty => f.write_str("()"),
            VmValueKind::Bool { value } => write!(f, "{value}"),
            VmValueKind::Integer { value } => write!(f, "{value}"),
            VmValueKind::Float { value } => write!(f, "{value}"),
            VmValueKind::String { string } => write!(f, "{string}"),
            VmValueKind::Ref { base, offset } => {
                write!(f, "{}", base)?;
                // if let ScopeValue::ConstValue(cv) = &symbol.borrow().value {
                if let Some(offset) = offset {
                    return write!(f, ".{}@", offset);
                } else {
                    return f.write_char('@');
                }
                // }
            }
            VmValueKind::Function { body, .. } => write!(f, "{}", body.format()),
            VmValueKind::NativeFunction { .. } => write!(f, "Native Function"),
            VmValueKind::Tuple(list) => {
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
            VmValueKind::StructInstance { members, .. }
            | VmValueKind::StructInitializer { members } => {
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
            VmValueKind::Symbol(_) => f.write_str("Symbol"),
            VmValueKind::IntrinsicStorage(_, _) => f.write_str("Intrinsic"),
        }
    }
}

impl VmValueKind {
    pub fn as_integer(&self) -> u64 {
        match self {
            VmValueKind::Integer { value } => *value,
            _ => panic!(),
        }
    }

    pub fn as_float(&self) -> f64 {
        match self {
            VmValueKind::Float { value } => *value,
            _ => panic!(),
        }
    }

    pub fn as_record_instance(
        &self,
    ) -> (&Rf<Scope<Type, VmValue>>, &LinkedHashMap<String, VmValue>) {
        match self {
            VmValueKind::StructInstance { rf, members } => (rf, members),
            _ => panic!(),
        }
    }

    pub fn as_empty(&self) -> bool {
        matches!(self, VmValueKind::Empty)
    }
}

impl NodeDisplay for VmValueKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            VmValueKind::Empty => write!(f, "Empty"),
            VmValueKind::Integer { value } => write!(f, "Integer: {value}"),
            VmValueKind::Bool { value } => write!(f, "Boolean: {value}"),
            VmValueKind::Float { value } => write!(f, "Float: {value}"),
            VmValueKind::Ref { .. } => write!(f, "Reference"),
            VmValueKind::String { string } => write!(f, "String: {string}"),
            VmValueKind::Function { .. } => write!(f, "Function"),
            VmValueKind::NativeFunction { .. } => write!(f, "Native Function"),
            VmValueKind::Tuple(_) => write!(f, "Tuple"),
            VmValueKind::StructInitializer { .. } => write!(f, "Struct Initializer"),
            VmValueKind::StructInstance { .. } => write!(f, "Record Instance"),
            VmValueKind::Symbol(_) => write!(f, "Symbol"),
            VmValueKind::IntrinsicStorage(_, _) => write!(f, "Intrinsic"),
        }
    }
}

impl TreeDisplay for VmValueKind {
    fn num_children(&self) -> usize {
        match self {
            VmValueKind::Function { .. } => 1,
            VmValueKind::Tuple(list) => list.len(),
            VmValueKind::StructInitializer { members, .. } => members.len(),
            VmValueKind::StructInstance { members, .. } => members.len(),
            VmValueKind::Ref {
                offset: Some(_), ..
            } => 2,
            VmValueKind::Ref { .. } => 1,
            VmValueKind::Symbol(_) => 1,
            _ => 0,
        }
    }

    fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay<()>> {
        match self {
            VmValueKind::Function { body, .. } => match index {
                0 => Some(body),
                _ => None,
            },
            VmValueKind::Tuple(tu) => {
                if let Some(val) = tu.get(index) {
                    Some(val)
                } else {
                    None
                }
            }
            VmValueKind::StructInstance { .. } => None,
            VmValueKind::Ref { base, offset } => match index {
                0 => Some(&**base),
                1 => offset.as_ref().map::<&dyn TreeDisplay, _>(|f| f),
                _ => None,
            },
            _ => None,
        }
    }

    fn child_at_bx<'a>(&'a self, index: usize) -> Box<dyn TreeDisplay<()> + 'a> {
        match self {
            VmValueKind::StructInstance { members, .. } => members.child_at_bx(index),
            VmValueKind::StructInitializer { members } => members.child_at_bx(index),
            VmValueKind::Symbol(sym) => Box::new(sym.borrow()),
            // ConstValueKind::Ref { base, offset } => Box::new(base),
            _ => panic!(),
        }
    }
}

#[derive(Clone)]
pub struct VmValue {
    pub ty: Type,
    pub kind: VmValueKind,
}

impl Display for VmValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.kind, f)
    }
}

impl NodeDisplay for VmValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str("Const Value")
    }
}

impl TreeDisplay for VmValue {
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

impl EvaluationValue for VmValue {
    type Type = crate::vm_value::Type;

    fn get_struct_members_rf(&self) -> impl Iterator<Item = (&String, &Self)> {
        match &self.kind {
            VmValueKind::StructInitializer { members } => members.iter(),
            VmValueKind::StructInstance { members, .. } => members.iter(),
            _ => panic!("Expected struct initializer or instance!"),
        }
    }

    fn get_struct_members(self) -> impl Iterator<Item = (String, Self)> {
        match self.kind {
            VmValueKind::StructInitializer { members } => members.into_iter(),
            VmValueKind::StructInstance { members, .. } => members.into_iter(),
            _ => panic!("Expected struct initializer or instance!"),
        }
    }

    fn get_struct_member(&self, name: &str) -> Option<&Self> {
        match &self.kind {
            VmValueKind::StructInitializer { members } => members.get(name),
            VmValueKind::StructInstance { members, .. } => members.get(name),
            _ => panic!("Expected struct initializer or instance!"),
        }
    }

    fn get_struct_member_mut(&mut self, name: &str) -> Option<&mut Self> {
        match &mut self.kind {
            VmValueKind::StructInitializer { members } => members.get_mut(name),
            VmValueKind::StructInstance { members, .. } => members.get_mut(name),
            _ => panic!("Expected struct initializer or instance!"),
        }
    }

    fn create_struct_instance(
        sym: tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>>,
    ) -> Self {
        VmValue {
            ty: Type::StructInstance {
                rf: Some(sym.clone()),
                members: LinkedHashMap::new(),
            },
            kind: VmValueKind::StructInstance {
                rf: sym,
                members: LinkedHashMap::new(),
            },
        }
    }

    fn is_struct_instance(&self) -> bool {
        match &self.kind {
            VmValueKind::StructInstance { .. } => true,
            _ => false,
        }
    }

    fn create_struct_initializer(values: linked_hash_map::LinkedHashMap<String, Self>) -> Self {
        let types = values.values().map(|val| val.ty.clone());
        let ty = LinkedHashMap::from_iter(values.keys().cloned().zip(types));

        VmValue {
            ty: Type::StructInitializer { members: ty },
            kind: VmValueKind::StructInitializer { members: values },
        }
    }

    fn is_struct_initializer(&self) -> bool {
        match &self.kind {
            VmValueKind::StructInitializer { .. } => true,
            _ => false,
        }
    }

    fn empty() -> Self {
        Self {
            kind: VmValueKind::Empty,
            ty: Type::empty(),
        }
    }

    fn is_empty(&self) -> bool {
        match &self.kind {
            VmValueKind::Empty => true,
            _ => false,
        }
    }

    fn default_for(ty: &Self::Type) -> Self {
        let kind = match ty {
            Type::Empty => VmValueKind::Empty,
            Type::Integer { .. } => VmValueKind::Integer { value: 0 },
            Type::Float { .. } => VmValueKind::Float { value: 0.0 },
            _ => VmValueKind::Empty,
        };

        VmValue {
            ty: ty.clone(),
            kind,
        }
    }

    fn resolve_ref(
        &self,
    ) -> Option<tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>>> {
        match (&self.ty, &self.kind) {
            (
                Type::Ref {
                    base_type: box Type::Ref { .. },
                },
                VmValueKind::Symbol(sym),
            ) => {
                let value = sym.borrow();
                if let ScopeValue::EvaluationValue(cv) = &value.value {
                    return cv.resolve_ref();
                }
                return Some(sym.clone());
            }
            (Type::Ref { .. }, VmValueKind::Symbol(sym)) => return Some(sym.clone()),
            (Type::Ref { .. }, VmValueKind::Ref { base, offset: None }) => {
                return base.resolve_ref();
            }
            (
                Type::Ref { .. },
                VmValueKind::Ref {
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

    fn string(str: String) -> Self {
        VmValue {
            ty: Type::String,
            kind: VmValueKind::String { string: str },
        }
    }

    fn is_string(&self) -> bool {
        match &self.kind {
            VmValueKind::String { .. } => true,
            _ => false,
        }
    }

    fn string_value(&self) -> &str {
        match &self.kind {
            VmValueKind::String { string } => string,
            _ => panic!("Expected to be string!"),
        }
    }

    fn integer(value: u64, width: u8, signed: bool) -> Self {
        VmValue {
            kind: VmValueKind::Integer { value },
            ty: Type::Integer { width, signed },
        }
    }

    fn is_integer(&self) -> bool {
        match &self.kind {
            VmValueKind::Integer { .. } => true,
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
            VmValueKind::Integer { value } => *value,
            _ => 0,
        }
    }

    fn cinteger(value: u64) -> Self {
        VmValue {
            kind: VmValueKind::Integer { value },
            ty: Type::CoercibleInteger,
        }
    }

    fn is_cinteger(&self) -> bool {
        match &self.ty {
            Type::CoercibleInteger => true,
            _ => false,
        }
    }

    fn float(value: f64, width: u8) -> Self {
        VmValue {
            kind: VmValueKind::Float { value },
            ty: Type::Float { width },
        }
    }

    fn is_float(&self) -> bool {
        match &self.kind {
            VmValueKind::Integer { .. } => true,
            _ => false,
        }
    }

    fn float_width(&self) -> u8 {
        self.get_type().float_width()
    }

    fn float_value(&self) -> f64 {
        match &self.kind {
            VmValueKind::Float { value } => *value,
            _ => 0.0,
        }
    }

    fn cfloat(value: f64) -> Self {
        VmValue {
            kind: VmValueKind::Float { value },
            ty: Type::CoercibleFloat,
        }
    }

    fn is_cfloat(&self) -> bool {
        match &self.ty {
            Type::CoercibleFloat => true,
            _ => false,
        }
    }

    fn bool(value: bool) -> Self {
        VmValue {
            kind: VmValueKind::Bool { value },
            ty: Type::Boolean,
        }
    }

    fn is_bool(&self) -> bool {
        match &self.kind {
            VmValueKind::Bool { .. } => true,
            _ => false,
        }
    }

    fn bool_value(&self) -> bool {
        match &self.kind {
            VmValueKind::Bool { value } => *value,
            _ => false,
        }
    }

    fn function(
        body: tl_core::ast::Statement,
        parameters: linked_hash_map::LinkedHashMap<String, Self::Type>,
        return_type: Self::Type,
        node: tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>>,
    ) -> Self {
        VmValue {
            kind: VmValueKind::Function { body, rf: node },
            ty: Type::Function {
                parameters,
                return_type: Box::new(return_type),
            },
        }
    }

    fn is_function(&self) -> bool {
        match &self.kind {
            VmValueKind::Function { .. } => true,
            _ => false,
        }
    }

    fn function_body(&self) -> &tl_core::ast::Statement {
        match &self.kind {
            VmValueKind::Function { body, .. } => body,
            _ => panic!("Expected to be function!"),
        }
    }

    fn function_rf(&self) -> &tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>> {
        match &self.kind {
            VmValueKind::Function { rf, .. } => rf,
            _ => panic!("Expected to be function!"),
        }
    }

    fn native_function(
        callback: std::sync::Arc<
            dyn Fn(&linked_hash_map::LinkedHashMap<String, Self>) -> Self + Sync + Send,
        >,
        parameters: linked_hash_map::LinkedHashMap<String, Self::Type>,
        return_type: Self::Type,
        node: tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>>,
    ) -> Self {
        VmValue {
            kind: VmValueKind::NativeFunction { callback, rf: node },
            ty: Type::Function {
                parameters,
                return_type: Box::new(return_type),
            },
        }
    }

    fn is_native_function(&self) -> bool {
        match &self.kind {
            VmValueKind::NativeFunction { .. } => true,
            _ => false,
        }
    }

    fn native_function_callback(
        &self,
    ) -> &std::sync::Arc<dyn Fn(&linked_hash_map::LinkedHashMap<String, Self>) -> Self + Sync + Send>
    {
        match &self.kind {
            VmValueKind::NativeFunction { callback, .. } => callback,
            _ => panic!("Expected to be native function!"),
        }
    }

    fn native_function_rf(
        &self,
    ) -> &tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>> {
        match &self.kind {
            VmValueKind::NativeFunction { rf, .. } => rf,
            _ => panic!("Expected to be native function!"),
        }
    }

    fn tuple(values: Vec<Self>) -> Self {
        let types: Vec<_> = values.iter().map(|val| val.ty.clone()).collect();
        VmValue {
            kind: VmValueKind::Tuple(values),
            ty: Type::Tuple(types),
        }
    }

    fn is_tuple(&self) -> bool {
        match &self.kind {
            VmValueKind::Tuple(_) => true,
            _ => false,
        }
    }

    fn tuple_value(self) -> Vec<Self> {
        match self.kind {
            VmValueKind::Tuple(s) => s,
            _ => panic!("Expected to be tuple!"),
        }
    }

    fn tuple_value_rf(&self) -> &Vec<Self> {
        match &self.kind {
            VmValueKind::Tuple(s) => s,
            _ => panic!("Expected to be tuple!"),
        }
    }

    fn reference(left: Self, right: String, right_ty: Self::Type) -> Self {
        VmValue {
            ty: Type::Ref {
                base_type: Box::new(right_ty),
            },
            kind: VmValueKind::Ref {
                base: Box::new(left),
                offset: Some(right),
            },
        }
    }

    fn is_reference(&self) -> bool {
        match &self.kind {
            VmValueKind::Ref { .. } => true,
            _ => false,
        }
    }

    fn sym_reference(
        sym: &tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>>,
        ty: Self::Type,
    ) -> Self {
        VmValue {
            ty: Type::Ref {
                base_type: Box::new(ty),
            },
            kind: VmValueKind::Symbol(sym.clone()),
        }
    }

    fn intrinsic_storage(
        sym: Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>>,
        storage: Rf<dyn IntrinsicType + Sync + Send>,
        generics: Vec<Type>,
    ) -> Self {
        VmValue {
            kind: VmValueKind::IntrinsicStorage(storage, generics),
            ty: Type::intrinsic(sym),
        }
    }

    fn try_implicit_cast(&self, ty: &Self::Type) -> Option<Self> {
        match (self, ty) {
            (
                VmValue {
                    kind: VmValueKind::Integer { value },
                    ty: Type::CoercibleInteger,
                },
                Type::Integer { width, signed },
            ) => Some(VmValue::integer(*value, *width, *signed)),
            (
                VmValue {
                    kind: VmValueKind::Float { value },
                    ty: Type::CoercibleFloat,
                },
                Type::Float { width },
            ) => Some(VmValue::float(*value, *width)),
            (_, Type::Symbol(sym)) => {
                let sym = sym.borrow();
                if let ScopeValue::TypeAlias { ty, .. } = &sym.value {
                    return self.try_implicit_cast(ty);
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

impl Into<Type> for VmValue {
    fn into(self) -> Type {
        self.to_type()
    }
}

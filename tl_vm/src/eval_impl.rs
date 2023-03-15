use linked_hash_map::LinkedHashMap;
use tl_evaluator::{evaluation_type::EvaluationType, evaluation_value::EvaluationValue, scope::{scope::ScopeValue, intrinsics::IntrinsicType}};
use tl_util::Rf;

use crate::const_value::{Type, ConstValue, ConstValueKind};


impl EvaluationType for Type {
    type Value = ConstValue;

    fn is_empty(&self) -> bool {
        match self {
            Type::Empty => true,
            _ => false
        }
    }

    fn is_string(&self) -> bool {
        match self {
            Type::String => true,
            _ => false
        }
    }

    fn is_integer(&self) -> bool {
        match self {
            Type::Integer { .. }=> true,
            _ => false
        }
    }

    fn is_cinteger(&self) -> bool {
        match self {
            Type::CoercibleInteger { .. }=> true,
            _ => false
        }
    }

    fn is_float(&self) -> bool {
        match self {
            Type::Float { .. }=> true,
            _ => false
        }
    }

    fn is_cfloat(&self) -> bool {
        match self {
            Type::CoercibleFloat { .. }=> true,
            _ => false
        }
    }

    fn is_bool(&self) -> bool {
        match self {
            Type::Boolean { .. }=> true,
            _ => false
        }
    }

    fn is_function(&self) -> bool {
        match self {
            Type::Function { .. }=> true,
            _ => false
        }
    }

    fn is_symbol(&self) -> bool {
        match self {
            Type::Symbol { .. }=> true,
            _ => false
        }
    }

    fn is_ref(&self) -> bool {
        match self {
            Type::Ref { .. }=> true,
            _ => false
        }
    }

    fn is_intrinsic(&self) -> bool {
        match self {
            Type::Intrinsic { .. }=> true,
            _ => false
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
        Type::Ref { base_type: Box::new(base_type) }
    }

    fn intrinsic(symbol: tl_util::Rf<tl_evaluator::scope::scope::Scope<Self, Self::Value>>) -> Self {
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
            },
            _ => panic!("Expected to be function!"),
        }
    }

    fn function_return(&self) -> &Self {
        match self {
            Type::Function { return_type, .. } => {
                &**return_type
            },
            _ => panic!("Expected to be function!"),
        }
    }

    fn set_function_return(&mut self, ty: Self) {
        match self {
            Type::Function { return_type, .. } => {
                *return_type = Box::new(ty);
            },
            _ => panic!("Expected to be function!"),
        }
    }

    fn symbol_rf(&self) -> &tl_util::Rf<tl_evaluator::scope::scope::Scope<Self, Self::Value>> {
        match self {
            Type::Symbol(sym) => {
                sym
            },
            _ => panic!("Expected to be symbol!"),
        }
    }

    fn intrinsic_rf(&self) -> &tl_util::Rf<tl_evaluator::scope::scope::Scope<Self, Self::Value>> {
        match self {
            Type::Intrinsic(sym) => {
                sym
            },
            _ => panic!("Expected to be intrinsic!"),
        }
    }
}

impl EvaluationValue for ConstValue {
    type Type = crate::const_value::Type;

    fn get_struct_members_rf(&self) -> impl Iterator<Item = (&String, &Self)> {
        match &self.kind {
            ConstValueKind::StructInitializer { members } => members.iter(),
            ConstValueKind::StructInstance { members, .. } => members.iter(),
            _ => panic!("Expected struct initializer or instance!")
        }
    }

    fn get_struct_members(self) -> impl Iterator<Item = (String, Self)> {
        match self.kind {
            ConstValueKind::StructInitializer { members } => members.into_iter(),
            ConstValueKind::StructInstance { members, .. } => members.into_iter(),
            _ => panic!("Expected struct initializer or instance!")
        }
    }

    fn get_struct_member(&self, name: &str) -> Option<&Self> {
        match &self.kind {
            ConstValueKind::StructInitializer { members } => members.get(name),
            ConstValueKind::StructInstance { members, .. } => members.get(name),
            _ => panic!("Expected struct initializer or instance!")
        }
    }

    fn get_struct_member_mut(&mut self, name: &str) -> Option<&mut Self> {
        match &mut self.kind {
            ConstValueKind::StructInitializer { members } => members.get_mut(name),
            ConstValueKind::StructInstance { members, .. } => members.get_mut(name),
            _ => panic!("Expected struct initializer or instance!")
        }
    }

    fn create_struct_instance(sym: tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>>) -> Self {
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

    fn is_struct_instance(&self) -> bool {
        match &self.kind {
            ConstValueKind::StructInstance { .. } => true,
            _ => false,
        }
    }

    fn create_struct_initializer(values: linked_hash_map::LinkedHashMap<String, Self>) -> Self {
        let types = values.values().map(|val| val.ty.clone());
        let ty = LinkedHashMap::from_iter(values.keys().cloned().zip(types));

        ConstValue {
            ty: Type::StructInitializer { members: ty },
            kind: ConstValueKind::StructInitializer { members: values },
        }
    }

    fn is_struct_initializer(&self) -> bool {
        match &self.kind {
            ConstValueKind::StructInitializer { .. } => true,
            _ => false,
        }
    }

    fn empty() -> Self {
        Self {
            kind: ConstValueKind::Empty,
            ty: Type::empty(),
        }
    }

    fn is_empty(&self) -> bool {
        match &self.kind {
            ConstValueKind::Empty => true,
            _ => false
        }
    }

    fn default_for(ty: &Self::Type) -> Self {
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

    fn resolve_ref(&self) -> Option<tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>>> {
        match (&self.ty, &self.kind) {
            (
                Type::Ref {
                    base_type: box Type::Ref { .. },
                },
                ConstValueKind::Symbol(sym),
            ) => {
                let value = sym.borrow();
                if let ScopeValue::EvaluationValue(cv) = &value.value {
                    return cv.resolve_ref();
                }
                return Some(sym.clone());
            }
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
        ConstValue {
            ty: Type::String,
            kind: ConstValueKind::String { string: str },
        }
    }

    fn is_string(&self) -> bool {
        match &self.kind {
            ConstValueKind::String { .. } => true,
            _ => false,
        }
    }

    fn string_value(&self) -> &str {
        match &self.kind {
            ConstValueKind::String { string } => string,
            _ => panic!("Expected to be string!"),
        }
    }

    fn integer(value: u64, width: u8, signed: bool) -> Self {
        ConstValue {
            kind: ConstValueKind::Integer { value },
            ty: Type::Integer { width, signed },
        }
    }

    fn is_integer(&self) -> bool {
        match &self.kind {
            ConstValueKind::Integer { .. } => true,
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
            ConstValueKind::Integer { value } => *value,
            _ => 0,
        }
    }

    fn cinteger(value: u64) -> Self {
        ConstValue {
            kind: ConstValueKind::Integer { value },
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
        ConstValue {
            kind: ConstValueKind::Float { value },
            ty: Type::Float { width },
        }
    }

    fn is_float(&self) -> bool {
        match &self.kind {
            ConstValueKind::Integer { .. } => true,
            _ => false,
        }
    }

    fn float_width(&self) -> u8 {
        self.get_type().float_width()
    }

    fn float_value(&self) -> f64 {
        match &self.kind {
            ConstValueKind::Float { value } => *value,
            _ => 0.0,
        }
    }

    fn cfloat(value: f64) -> Self {
        ConstValue {
            kind: ConstValueKind::Float { value },
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
        ConstValue {
            kind: ConstValueKind::Bool { value },
            ty: Type::Boolean,
        }
    }

    fn is_bool(&self) -> bool {
        match &self.kind {
            ConstValueKind::Bool { .. } => true,
            _ => false,
        }
    }

    fn bool_value(&self) -> bool {
        match &self.kind {
            ConstValueKind::Bool { value } => *value,
            _ => false,
        }
    }

    fn function(
        body: tl_core::ast::Statement,
        parameters: linked_hash_map::LinkedHashMap<String, Self::Type>,
        return_type: Self::Type,
        node: tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>>,
    ) -> Self {
        ConstValue {
            kind: ConstValueKind::Function { body, rf: node },
            ty: Type::Function {
                parameters,
                return_type: Box::new(return_type),
            },
        }
    }

    fn is_function(&self) -> bool {
        match &self.kind {
            ConstValueKind::Function { .. } => true,
            _ => false,
        }
    }

    fn function_body(&self) -> &tl_core::ast::Statement {
        match &self.kind {
            ConstValueKind::Function { body, .. } => body,
            _ => panic!("Expected to be function!"),
        }
    }

    fn function_rf(&self) -> &tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>> {
        match &self.kind {
            ConstValueKind::Function { rf, .. } => rf,
            _ => panic!("Expected to be function!"),
        }
    }

    fn native_function(
        callback: std::sync::Arc<dyn Fn(&linked_hash_map::LinkedHashMap<String, Self>) -> Self + Sync + Send>,
        parameters: linked_hash_map::LinkedHashMap<String, Self::Type>,
        return_type: Self::Type,
        node: tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>>,
    ) -> Self {
        ConstValue {
            kind: ConstValueKind::NativeFunction { callback, rf: node },
            ty: Type::Function {
                parameters,
                return_type: Box::new(return_type),
            },
        }
    }

    fn is_native_function(&self) -> bool {
        match &self.kind {
            ConstValueKind::NativeFunction { .. } => true,
            _ => false,
        }
    }

    fn native_function_callback(&self) -> &std::sync::Arc<dyn Fn(&linked_hash_map::LinkedHashMap<String, Self>) -> Self + Sync + Send> {
        match &self.kind {
            ConstValueKind::NativeFunction { callback, .. } => callback,
            _ => panic!("Expected to be native function!"),
        }
    }

    fn native_function_rf(&self) -> &tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>> {
        match &self.kind {
            ConstValueKind::NativeFunction { rf, .. } => rf,
            _ => panic!("Expected to be native function!"),
        }
    }

    fn tuple(values: Vec<Self>) -> Self {
        let types: Vec<_> = values.iter().map(|val| val.ty.clone()).collect();
        ConstValue {
            kind: ConstValueKind::Tuple(values),
            ty: Type::Tuple(types),
        }
    }

    fn is_tuple(&self) -> bool {
        match &self.kind {
            ConstValueKind::Tuple(_)=> true,
            _ => false,
        }
    }

    fn tuple_value(self) -> Vec<Self> {
        match self.kind {
            ConstValueKind::Tuple(s) => s,
            _ => panic!("Expected to be tuple!"),
        }
    }

    fn tuple_value_rf(&self) -> &Vec<Self> {
        match &self.kind {
            ConstValueKind::Tuple(s) => s,
            _ => panic!("Expected to be tuple!"),
        }
    }

    fn reference(left: Self, right: String, right_ty: Self::Type) -> Self {
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

    fn is_reference(&self) -> bool {
        match &self.kind {
            ConstValueKind::Ref { .. }=> true,
            _ => false,
        }
    }

    fn sym_reference(sym: &tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>>, ty: Self::Type) -> Self {
        ConstValue {
            ty: Type::Ref {
                base_type: Box::new(ty),
            },
            kind: ConstValueKind::Symbol(sym.clone()),
        }
    }

    fn intrinsic_storage(sym: Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>>, storage: Rf<dyn IntrinsicType + Sync + Send>, generics: Vec<Type>) -> Self {
        ConstValue {
            kind: ConstValueKind::IntrinsicStorage(storage, generics),
            ty: Type::intrinsic(sym),
        }
    }

    fn try_implicit_cast(&self, ty: &Self::Type) -> Option<Self> {
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

impl Into<Type> for ConstValue {
    fn into(self) -> Type {
        self.to_type() 
    }
}
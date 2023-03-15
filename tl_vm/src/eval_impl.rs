use tl_evaluator::{evaluation_type::EvaluationType, evaluation_value::EvaluationValue};

use crate::const_value::{Type, ConstValue};


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
        // Type::Symbol(symbol)
        todo!()
    }

    fn rf(base_type: Self) -> Self {
        todo!()
    }

    fn intrinsic(symbol: tl_util::Rf<tl_evaluator::scope::scope::Scope<Self, Self::Value>>) -> Self {
        todo!()
    }

    fn integer_width(&self) -> u8 {
        todo!()
    }

    fn integer_signed(&self) -> bool {
        todo!()
    }

    fn function_parameters(&self) -> impl Iterator<Item = (String, Self)> {
        todo!()
    }

    fn function_parameters_rf(&self) -> impl Iterator<Item = (&String, &Self)> {
        todo!()
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

impl EvaluationValue for ConstValue {
    type Type = crate::const_value::Type;

    fn get_struct_members_rf(&self) -> impl Iterator<Item = (&String, &Self)> {
        todo!()
    }

    fn get_struct_members(self) -> impl Iterator<Item = (String, Self)> {
        todo!()
    }

    fn get_struct_member(&self, name: &str) -> Option<&Self> {
        todo!()
    }

    fn get_struct_member_mut(&mut self, name: &str) -> Option<&mut Self> {
        todo!()
    }

    fn create_struct_instance(sym: tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>>) -> Self {
        todo!()
    }

    fn is_struct_instance(&self) -> bool {
        todo!()
    }

    fn create_struct_initializer(values: linked_hash_map::LinkedHashMap<String, Self>) -> Self {
        todo!()
    }

    fn is_struct_initializer(&self) -> bool {
        todo!()
    }

    fn empty() -> Self {
        todo!()
    }

    fn is_empty() -> bool {
        todo!()
    }

    fn default_for(ty: &Self::Type) -> Self {
        todo!()
    }

    fn resolve_ref(&self) -> Option<tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>>> {
        todo!()
    }

    fn resolve_ref_value(&self) -> Option<Self> {
        todo!()
    }

    fn string(str: String) -> Self {
        todo!()
    }

    fn is_string(&self) -> bool {
        todo!()
    }

    fn string_value(&self) -> &str {
        todo!()
    }

    fn integer(value: u64, width: u8, signed: bool) -> Self {
        todo!()
    }

    fn is_integer(&self) -> bool {
        todo!()
    }

    fn integer_width(&self) -> u8 {
        todo!()
    }

    fn is_signed(&self) -> bool {
        todo!()
    }

    fn integer_value(&self) -> u64 {
        todo!()
    }

    fn cinteger(value: u64) -> Self {
        todo!()
    }

    fn is_cinteger(&self) -> bool {
        todo!()
    }

    fn float(value: f64, width: u8) -> Self {
        todo!()
    }

    fn is_float(&self) -> bool {
        todo!()
    }

    fn float_width(&self) -> u8 {
        todo!()
    }

    fn float_value(&self) -> f64 {
        todo!()
    }

    fn cfloat(value: f64) -> Self {
        todo!()
    }

    fn is_cfloat(&self) -> bool {
        todo!()
    }

    fn bool(value: bool) -> Self {
        todo!()
    }

    fn is_bool(&self) -> bool {
        todo!()
    }

    fn bool_value(&self) -> bool {
        todo!()
    }

    fn function(
        body: tl_core::ast::Statement,
        parameters: linked_hash_map::LinkedHashMap<String, Self::Type>,
        return_type: Self::Type,
        node: tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>>,
    ) -> Self {
        todo!()
    }

    fn is_function(&self) -> bool {
        todo!()
    }

    fn function_body(&self) -> &tl_core::ast::Statement {
        todo!()
    }

    fn function_rf(&self) -> &tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>> {
        todo!()
    }

    fn native_function(
        callback: std::sync::Arc<dyn Fn(&linked_hash_map::LinkedHashMap<String, Self>) -> Self + Sync + Send>,
        parameters: linked_hash_map::LinkedHashMap<String, Self::Type>,
        return_type: Self::Type,
        node: tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>>,
    ) -> Self {
        todo!()
    }

    fn is_native_function(&self) -> bool {
        todo!()
    }

    fn native_function_callback(&self) -> &std::sync::Arc<dyn Fn(&linked_hash_map::LinkedHashMap<String, Self>) -> Self + Sync + Send> {
        todo!()
    }

    fn native_function_rf(&self) -> &tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>> {
        todo!()
    }

    fn tuple(values: Vec<Self>) -> Self {
        todo!()
    }

    fn is_tuple(&self) -> bool {
        todo!()
    }

    fn tuple_value(&self) -> Vec<Self> {
        todo!()
    }

    fn tuple_value_rf(&self) -> Vec<Self> {
        todo!()
    }

    fn reference(left: Self, right: String, right_ty: Self::Type) -> Self {
        todo!()
    }

    fn sym_reference(sym: &tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>>, ty: Self::Type) -> Self {
        todo!()
    }

    fn try_implicit_cast(&self, ty: &Self::Type) -> Option<Self> {
        todo!()
    }

    fn get_type(&self) -> &Self::Type {
        todo!()
    }

    fn get_type_mut(&mut self) -> &mut Self::Type {
        todo!()
    }

    fn to_type(self) -> Self::Type {
        todo!()
    }
}

impl Into<Type> for ConstValue {
    fn into(self) -> Type {
        self.ty 
    }
}
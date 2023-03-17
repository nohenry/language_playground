use std::fmt::{Debug, Display};

use linked_hash_map::LinkedHashMap;
use tl_evaluator::evaluation_value::EvaluationValue;
use tl_util::format::{NodeDisplay, TreeDisplay};

use crate::llvm_type::LlvmType;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LlvmValue {
    ty: LlvmType,
    llvm_value: inkwell::values::AnyValueEnum<'static>,
}

impl Display for LlvmValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self, f)
    }
}

impl NodeDisplay for LlvmValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        Display::fmt(&self, f)
    }
}

impl TreeDisplay for LlvmValue {
    fn num_children(&self) -> usize {
        0 
    }

    fn child_at(&self, index: usize) -> Option<&dyn TreeDisplay<()>> {
        None
    }
}

impl EvaluationValue for LlvmValue {
    type Type = LlvmType;

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

    fn create_struct_instance(
        sym: tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>>,
    ) -> Self {
        todo!()
    }

    fn is_struct_instance(&self) -> bool {
        todo!()
    }

    fn create_struct_initializer(values: LinkedHashMap<String, Self>) -> Self {
        todo!()
    }

    fn is_struct_initializer(&self) -> bool {
        todo!()
    }

    fn empty() -> Self {
        todo!()
    }

    fn is_empty(&self) -> bool {
        todo!()
    }

    fn default_for(ty: &Self::Type) -> Self {
        todo!()
    }

    fn resolve_ref(
        &self,
    ) -> Option<tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>>> {
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
        parameters: LinkedHashMap<String, Self::Type>,
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
        callback: std::sync::Arc<dyn Fn(&LinkedHashMap<String, Self>) -> Self + Sync + Send>,
        parameters: LinkedHashMap<String, Self::Type>,
        return_type: Self::Type,
        node: tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>>,
    ) -> Self {
        todo!()
    }

    fn is_native_function(&self) -> bool {
        todo!()
    }

    fn native_function_callback(
        &self,
    ) -> &std::sync::Arc<dyn Fn(&LinkedHashMap<String, Self>) -> Self + Sync + Send> {
        todo!()
    }

    fn native_function_rf(
        &self,
    ) -> &tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>> {
        todo!()
    }

    fn tuple(values: Vec<Self>) -> Self {
        todo!()
    }

    fn is_tuple(&self) -> bool {
        todo!()
    }

    fn tuple_value(self) -> Vec<Self> {
        todo!()
    }

    fn tuple_value_rf(&self) -> &Vec<Self> {
        todo!()
    }

    fn reference(left: Self, right: String, right_ty: Self::Type) -> Self {
        todo!()
    }

    fn is_reference(&self) -> bool {
        todo!()
    }

    fn sym_reference(
        sym: &tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>>,
        ty: Self::Type,
    ) -> Self {
        todo!()
    }

    fn intrinsic_storage(
        sym: tl_util::Rf<tl_evaluator::scope::scope::Scope<Self::Type, Self>>,
        storage: tl_util::Rf<dyn tl_evaluator::scope::intrinsics::IntrinsicType + Sync + Send>,
        generics: Vec<Self::Type>,
    ) -> Self {
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
        self.ty
    }
}

impl Into<LlvmType> for LlvmValue {
    fn into(self) -> LlvmType {
        self.to_type()
    }
}

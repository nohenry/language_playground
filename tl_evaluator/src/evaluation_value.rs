use linked_hash_map::LinkedHashMap;
use std::{fmt::Display, sync::Arc};
use tl_core::ast::Statement;
use tl_util::{
    format::{NodeDisplay, TreeDisplay},
    Rf,
};

use crate::{
    evaluation_type::EvaluationType,
    scope::{intrinsics::IntrinsicType, scope::Scope},
};

pub trait EvaluationValue:
    Sized + Clone + Into<Self::Type> + Display + NodeDisplay + TreeDisplay + 'static
{
    type Type: EvaluationType<Value = Self>;

    /* Struct Methods */
    fn get_struct_members_rf(&self) -> impl Iterator<Item = (&String, &Self)>;
    fn get_struct_members(self) -> impl Iterator<Item = (String, Self)>;
    fn get_struct_member(&self, name: &str) -> Option<&Self>;
    fn get_struct_member_mut(&mut self, name: &str) -> Option<&mut Self>;
    fn create_struct_instance(sym: Rf<Scope<Self::Type, Self>>) -> Self;
    fn is_struct_instance(&self) -> bool;

    fn create_struct_initializer(values: LinkedHashMap<String, Self>) -> Self;
    fn is_struct_initializer(&self) -> bool;

    fn empty() -> Self;
    fn is_empty(&self) -> bool;
    fn default_for(ty: &Self::Type) -> Self;
    fn resolve_ref(&self) -> Option<Rf<Scope<Self::Type, Self>>>;
    fn resolve_ref_value(&self) -> Option<Self>;

    fn string(str: String) -> Self;
    fn is_string(&self) -> bool;
    fn string_value(&self) -> &str;

    fn integer(value: u64, width: u8, signed: bool) -> Self;
    fn is_integer(&self) -> bool;
    fn integer_width(&self) -> u8;
    fn is_signed(&self) -> bool;
    fn integer_value(&self) -> u64;

    fn cinteger(value: u64) -> Self;
    fn is_cinteger(&self) -> bool;

    fn float(value: f64, width: u8) -> Self;
    fn is_float(&self) -> bool;
    fn float_width(&self) -> u8;
    fn float_value(&self) -> f64;

    fn cfloat(value: f64) -> Self;
    fn is_cfloat(&self) -> bool;

    fn bool(value: bool) -> Self;
    fn is_bool(&self) -> bool;
    fn bool_value(&self) -> bool;

    fn function(
        body: Statement,
        parameters: LinkedHashMap<String, Self::Type>,
        return_type: Self::Type,
        node: Rf<Scope<Self::Type, Self>>,
    ) -> Self;
    fn is_function(&self) -> bool;
    fn function_body(&self) -> &Statement;
    fn function_rf(&self) -> &Rf<Scope<Self::Type, Self>>;

    fn native_function(
        callback: Arc<dyn Fn(&LinkedHashMap<String, Self>) -> Self + Sync + Send>,
        parameters: LinkedHashMap<String, Self::Type>,
        return_type: Self::Type,
        node: Rf<Scope<Self::Type, Self>>,
    ) -> Self;
    fn is_native_function(&self) -> bool;
    fn native_function_callback(
        &self,
    ) -> &Arc<dyn Fn(&LinkedHashMap<String, Self>) -> Self + Sync + Send>;
    fn native_function_rf(&self) -> &Rf<Scope<Self::Type, Self>>;

    fn tuple(values: Vec<Self>) -> Self;
    fn is_tuple(&self) -> bool;
    fn tuple_value(self) -> Vec<Self>;
    fn tuple_value_rf(&self) -> &Vec<Self>;

    /// Creates a reference value with a member access expression.
    ///
    /// `left` should be a value reference to scope value
    /// `right` is an index into that scope values children
    fn reference(left: Self, right: String, right_ty: Self::Type) -> Self;
    fn is_reference(&self) -> bool;

    fn sym_reference(sym: &Rf<Scope<Self::Type, Self>>, ty: Self::Type) -> Self;

    fn intrinsic_storage(
        sym: Rf<Scope<Self::Type, Self>>,
        storage: Rf<dyn IntrinsicType + Sync + Send>,
        generics: Vec<Self::Type>,
    ) -> Self;

    fn try_implicit_cast(&self, ty: &Self::Type) -> Option<Self>;

    fn get_type(&self) -> &Self::Type;
    fn get_type_mut(&mut self) -> &mut Self::Type;
    fn to_type(self) -> Self::Type;
}

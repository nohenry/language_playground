use std::{
    fmt::{Debug, Display},
    hash::Hash,
};

use linked_hash_map::LinkedHashMap;
use tl_util::Rf;

use crate::{evaluation_value::EvaluationValue, scope::scope::Scope};

pub trait EvaluationType: Sized + Clone + Hash + PartialEq + Eq + Display + Debug {
    type Value: EvaluationValue<Type = Self>;

    fn is_empty(&self) -> bool;
    fn is_string(&self) -> bool;
    fn is_integer(&self) -> bool;
    fn is_cinteger(&self) -> bool;
    fn is_float(&self) -> bool;
    fn is_cfloat(&self) -> bool;
    fn is_bool(&self) -> bool;
    fn is_function(&self) -> bool;
    fn is_symbol(&self) -> bool;
    fn is_ref(&self) -> bool;
    fn is_intrinsic(&self) -> bool;

    // fn empty() -> Self;
    // fn string() -> Self;
    // fn integer(width: u8, signed: bool) -> Self;
    // fn cinteger() -> Self;
    // fn float(width: u8) -> Self;
    // fn cfloat() -> Self;
    // fn bool() -> Self;
    // fn function() -> Self;
    // fn symbol(symbol: Rf<Scope<Self, Self::Value>>) -> Self;
    // fn rf(base_type: Self) -> Self;
    // fn intrinsic(symbol: Rf<Scope<Self, Self::Value>>) -> Self;

    fn integer_width(&self) -> u8;
    fn integer_signed(&self) -> bool;

    fn float_width(&self) -> u8;

    fn function_parameters(self) -> impl Iterator<Item = (String, Self)>;
    fn function_parameters_rf(&self) -> impl Iterator<Item = (&String, &Self)>;
    fn set_function_parameters(&mut self, parameters: impl Iterator<Item = (String, Self)>);
    // fn function_parameters_mut(&self) -> impl Iterator<Item = (&mut String, &mut Self)>;
    fn function_return(&self) -> &Self;
    fn set_function_return(&mut self, ty: Self);

    fn symbol_rf(&self) -> &Rf<Scope<Self, Self::Value>>;
    fn intrinsic_rf(&self) -> &Rf<Scope<Self, Self::Value>>;
}

pub trait EvaluationTypeProvider {
    type Type: EvaluationType;

    fn empty(&self) -> Self::Type;
    fn string(&self) -> Self::Type;
    fn integer(&self, width: u8, signed: bool) -> Self::Type;
    fn cinteger(&self) -> Self::Type;
    fn float(&self, width: u8) -> Self::Type;
    fn cfloat(&self) -> Self::Type;
    fn bool(&self) -> Self::Type;
    fn function(&self) -> Self::Type;
    fn symbol(
        &self,
        symbol: Rf<Scope<Self::Type, <Self::Type as EvaluationType>::Value>>,
    ) -> Self::Type;
    fn rf(&self, base_type: Self::Type) -> Self::Type;
    fn intrinsic(
        &self,
        symbol: Rf<Scope<Self::Type, <Self::Type as EvaluationType>::Value>>,
    ) -> Self::Type;

    // fn inform_function_declaration(&self);
    fn function_def(
        &self,
        body: tl_core::ast::Statement,
        parameters: LinkedHashMap<String, Self::Type>,
        return_type: Self::Type,
        node: tl_util::Rf<Scope<Self::Type, <Self::Type as EvaluationType>::Value>>,
    ) -> <Self::Type as EvaluationType>::Value;
}

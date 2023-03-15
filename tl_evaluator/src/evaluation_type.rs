use std::{hash::Hash, fmt::{Display, Debug}};

use tl_util::Rf;

use crate::{scope::scope::Scope, evaluation_value::EvaluationValue};


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

    fn empty() -> Self;
    fn string() -> Self;
    fn integer(width: u8, signed: bool) -> Self;
    fn cinteger() -> Self;
    fn float(width: u8) -> Self;
    fn cfloat() -> Self;
    fn bool() -> Self;
    fn function() -> Self;
    fn symbol(symbol: Rf<Scope<Self, Self::Value>>) -> Self;
    fn rf(base_type: Self) -> Self;
    fn intrinsic(symbol: Rf<Scope<Self, Self::Value>>) -> Self;

    fn integer_width(&self) -> u8;
    fn integer_signed(&self) -> bool;

    fn function_parameters(&self) -> impl Iterator<Item = (String, Self)>;
    fn function_parameters_rf(&self) -> impl Iterator<Item = (&String, &Self)>;
    fn set_function_parameters(&mut self, parameters: impl Iterator<Item = (String, Self)>);
    // fn function_parameters_mut(&self) -> impl Iterator<Item = (&mut String, &mut Self)>;
    fn function_return(&self) -> &Self;
    fn set_function_return(&mut self, ty: Self);

    fn symbol_rf(&self) -> &Rf<Scope<Self, Self::Value>>;
    fn intrinsic_rf(&self) -> &Rf<Scope<Self, Self::Value>>;

}
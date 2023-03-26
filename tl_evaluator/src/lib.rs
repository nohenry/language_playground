#![feature(hasher_prefixfree_extras)]
#![feature(return_position_impl_trait_in_trait)]
#![feature(box_patterns)]
#![feature(iter_intersperse)]

pub mod evaluation_type;
pub mod evaluation_value;
pub mod evaluator;
pub mod pass;

pub mod scope;

pub mod error;

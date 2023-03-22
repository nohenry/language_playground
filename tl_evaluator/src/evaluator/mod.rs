use std::marker::PhantomData;
use std::rc::Rc;
use std::sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard};

use linked_hash_map::LinkedHashMap;
use tl_core::ast::{ArgList, EnclosedList, Param, ParamaterList};
use tl_core::token::Range;
use tl_core::Module;
use tl_util::Rf;

use crate::error::{EvaluationError, EvaluationErrorKind, TypeHint};
use crate::evaluation_type::{EvaluationType, EvaluationTypeProvider};
use crate::evaluation_value::EvaluationValue;
use crate::pass::{EvaluationPass, MemberPass, Pass, TypeFirst};
use crate::scope::scope::{Scope, ScopeValue};
use crate::scope::scope_manager::ScopeManager;

// mod expression;
// mod statement;
// mod types;

pub struct EvaluatorState<T: EvaluationType<Value = V>, V: EvaluationValue<Type = T>> {
    pub scope: ScopeManager<T, V>,
    pub errors: Vec<EvaluationError<T>>,
}

pub trait Evaluator<P: Pass> {
    type Type<'a>: EvaluationType<Value = Self::Value<'a>> + 'a;
    type Value<'a>: EvaluationValue<Type = Self::Type<'a>> + 'a;
}



// pub struct Evaluator<'a, T: EvaluationType<Value = V>, V: EvaluationValue<Type = T>, TP: EvaluationTypeProvider<'a, Type = T>, P: Pass> {
//     module: Arc<Module>,
//     pub state: RwLock<EvaluatorState<T, V>>,
//     pass: PhyyantomData<&'a P>,
//     pub type_provider: Rc<TP>,
// }



use std::marker::PhantomData;
use std::sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard};

use linked_hash_map::LinkedHashMap;
use tl_core::ast::{ArgList, EnclosedList, Param, ParamaterList};
use tl_core::token::Range;
use tl_core::Module;
use tl_util::Rf;

use crate::error::{EvaluationError, EvaluationErrorKind, TypeHint};
use crate::evaluation_type::EvaluationType;
use crate::evaluation_value::EvaluationValue;
use crate::pass::{EvaluationPass, Pass};
use crate::scope::scope::Scope;
use crate::scope::scope_manager::ScopeManager;

mod expression;
mod statement;
mod types;

pub struct EvaluatorState<T: EvaluationType<Value = V>, V: EvaluationValue<Type = T>> {
    pub scope: ScopeManager<T, V>,
    pub errors: Vec<EvaluationError<T>>,
}

pub struct Evaluator<T: EvaluationType<Value = V>, V: EvaluationValue<Type = T>, P: Pass> {
    module: Arc<Module>,
    pub state: RwLock<EvaluatorState<T, V>>,
    pass: PhantomData<P>,
}

impl<T: EvaluationType<Value = V>, V: EvaluationValue<Type = T>, P: Pass> Evaluator<T, V, P> {
    pub fn new(module: Arc<Module>, scope_manager: ScopeManager<T, V>) -> Evaluator<T, V, P> {
        Evaluator {
            module,
            state: RwLock::new(EvaluatorState {
                scope: scope_manager,
                errors: Vec::new(),
            }),
            pass: PhantomData,
        }
    }

    fn rstate(&self) -> RwLockReadGuard<'_, EvaluatorState<T, V>> {
        self.state.read().unwrap()
    }

    fn wstate(&self) -> RwLockWriteGuard<'_, EvaluatorState<T, V>> {
        self.state.write().unwrap()
    }
}

impl<T: EvaluationType<Value = V>, V: EvaluationValue<Type = T>> Evaluator<T, V, EvaluationPass> {
    fn evaluate_args(&self, args: &ArgList, index: usize) -> Vec<V> {
        args.iter_items()
            .map(|expr| self.evaluate_expression(expr, index))
            .collect()
    }
}

impl<T: EvaluationType<Value = V>, V: EvaluationValue<Type = T>, P: Pass> Evaluator<T, V, P> {
    pub fn evaluate_params(&self, params: &ParamaterList) -> LinkedHashMap<String, T> {
        let iter = params.items.iter_items().filter_map(|f| {
            if let (Some(ident), Some(ty)) = (&f.name, &f.ty) {
                Some((ident.as_str().to_string(), self.evaluate_type(ty)))
            } else {
                None
            }
        });
        LinkedHashMap::from_iter(iter)
    }

    pub fn evaluate_struct_members(
        &self,
        members: &EnclosedList<Param>,
    ) -> LinkedHashMap<String, T> {
        let iter = members.iter_items().filter_map(|f| {
            if let (Some(ident), Some(ty)) = (&f.name, &f.ty) {
                Some((ident.as_str().to_string(), self.evaluate_type(ty)))
            } else {
                None
            }
        });
        LinkedHashMap::from_iter(iter)
    }

    pub fn evaluate_struct_init(
        &self,
        symbol: &Rf<Scope<T, V>>,
        members: &LinkedHashMap<String, T>,
        args: &LinkedHashMap<String, V>,
        member_range_provider: impl Fn(usize) -> Option<Range>,
        full_range: Range,
    ) -> (V, LinkedHashMap<String, V>) {
        let arg_len = args.len();
        let len_off = members.len() != args.len();

        let arg_vals: LinkedHashMap<_, _> = members
            .iter()
            .enumerate()
            .filter_map(|(i, (name, ty))| {
                if let Some(arg) = args.get(name) {
                    let arg = arg.try_implicit_cast(ty).unwrap_or_else(|| arg.clone());

                    if arg.get_type() == ty {
                        return Some((name.clone(), arg));
                    } else {
                        self.add_error(EvaluationError {
                            kind: EvaluationErrorKind::TypeMismatch(
                                arg.into(),
                                ty.clone(),
                                TypeHint::StructMember,
                            ),
                            range: member_range_provider(i).unwrap_or(full_range),
                        });
                        return None;
                    }
                }
                None
            })
            .collect();

        if len_off {
            // If the number of arguments doesn't match the record
            self.add_error(EvaluationError {
                kind: EvaluationErrorKind::ArgCountMismatch(arg_len as _, members.len() as _),
                range: full_range,
            });
        } else if arg_vals.len() == members.len() {
            // Everything good!
            return (V::create_struct_instance(symbol.clone()), arg_vals);
        }
        (V::empty(), LinkedHashMap::new())
    }

    pub fn add_error(&self, error: EvaluationError<T>) {
        self.wstate().errors.push(error)
    }
}

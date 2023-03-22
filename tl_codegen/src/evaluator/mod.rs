use std::{
    marker::PhantomData,
    rc::Rc,
    sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard},
};

use linked_hash_map::LinkedHashMap;
use tl_core::{ast::{ArgList, ParamaterList, EnclosedList, Param}, Module, token::Range};
use tl_evaluator::{
    evaluator::{Evaluator, EvaluatorState},
    pass::{EvaluationPass, MemberPass, Pass, TypeFirst},
    scope::{
        scope::{Scope, ScopeValue},
        scope_manager::ScopeManager,
    }, error::EvaluationError,
};
use tl_util::Rf;

use crate::{llvm_type::LlvmType, llvm_value::LlvmValue};

mod statement;

pub struct LlvmEvaluator<'a, P: Pass> {
    module: Arc<Module>,
    pub state: RwLock<EvaluatorState<LlvmType<'a>, LlvmValue<'a>>>,
    pd: PhantomData<&'a P>,
}

impl<'a, P: Pass> Evaluator<P> for LlvmEvaluator<'a, P> {
    type Type<'b> = LlvmType<'b>;
    type Value<'b> = LlvmValue<'b>;
}

impl<'a> LlvmEvaluator<'a, TypeFirst> {
    pub fn new(
        root: Rf<Scope<LlvmType<'a>, LlvmValue<'a>>>,
        module: Arc<Module>,
        index: usize,
    ) -> LlvmEvaluator<'a, TypeFirst> {
        let scope = Rf::new(Scope::new(
            root.clone(),
            module.name.to_string(),
            ScopeValue::Module(module.clone()),
            index,
        ));

        LlvmEvaluator {
            module,
            state: RwLock::new(EvaluatorState {
                scope: ScopeManager::new(root, scope),
                errors: Vec::new(),
            }),
            pd: PhantomData,
        }
    }
}

impl<'a> LlvmEvaluator<'a, MemberPass> {
    pub fn new(
        root: Rf<Scope<LlvmType<'a>, LlvmValue<'a>>>,
        module: Arc<Module>,
        index: usize,
    ) -> LlvmEvaluator<'a, MemberPass> {
        let scope = Rf::new(Scope::new(
            root.clone(),
            module.name.to_string(),
            ScopeValue::Module(module.clone()),
            index,
        ));

        LlvmEvaluator {
            module,
            state: RwLock::new(EvaluatorState {
                scope: ScopeManager::new(root, scope),
                errors: Vec::new(),
            }),
            pd: PhantomData,
        }
    }

    pub fn new_with_state(
        state: EvaluatorState<LlvmType<'a>, LlvmValue<'a>>,
        module: Arc<Module>,
    ) -> LlvmEvaluator<'a, MemberPass> {
        LlvmEvaluator {
            module,
            state: RwLock::new(state),
            pd: PhantomData,
        }
    }
}

impl<'a> LlvmEvaluator<'a, EvaluationPass> {
    pub fn new(
        module: Arc<Module>,
        scope_manager: ScopeManager<LlvmType<'a>, LlvmValue<'a>>,
    ) -> LlvmEvaluator<'a, EvaluationPass> {
        LlvmEvaluator {
            module,
            state: RwLock::new(EvaluatorState {
                scope: scope_manager,
                errors: Vec::new(),
            }),
            pd: PhantomData,
        }
    }
}

impl<'a, P: Pass> LlvmEvaluator<'a, P> {
    fn rstate(&self) -> RwLockReadGuard<'_, EvaluatorState<LlvmType<'a>, LlvmValue<'a>>> {
        self.state.read().unwrap()
    }

    fn wstate(&self) -> RwLockWriteGuard<'_, EvaluatorState<LlvmType<'a>, LlvmValue<'a>>> {
        self.state.write().unwrap()
    }

    pub fn finish(self) -> EvaluatorState<LlvmType<'a>, LlvmValue<'a>> {
        self.state.into_inner().unwrap()
    }
}

impl<'a> LlvmEvaluator<'a, EvaluationPass> {
    fn evaluate_args(&self, args: &ArgList, index: usize) -> Vec<LlvmValue<'a>> {
        // args.iter_items()
        //     .map(|expr| self.evaluate_expression(expr, index))
        //     .collect()
        Vec::new()
    }
}

impl<'a, P: Pass> LlvmEvaluator<'a, P> {
    pub fn evaluate_params(&self, params: &ParamaterList) -> LinkedHashMap<String, LlvmType<'a>> {
        todo!()
        // let iter = params.items.iter_items().filter_map(|f| {
        //     if let (Some(ident), Some(ty)) = (&f.name, &f.ty) {
        //         Some((ident.as_str().to_string(), self.evaluate_type(ty)))
        //     } else {
        //         None
        //     }
        // });
        // LinkedHashMap::from_iter(iter)
    }

    pub fn evaluate_struct_members(
        &self,
        members: &EnclosedList<Param>,
    ) -> LinkedHashMap<String, LlvmType<'a>> {
        todo!()
        // let iter = members.iter_items().filter_map(|f| {
        //     if let (Some(ident), Some(ty)) = (&f.name, &f.ty) {
        //         Some((ident.as_str().to_string(), self.evaluate_type(ty)))
        //     } else {
        //         None
        //     }
        // });
        // LinkedHashMap::from_iter(iter)
    }

    pub fn evaluate_struct_init(
        &self,
        symbol: &Rf<Scope<LlvmType<'a>, LlvmValue<'a>>>,
        members: &LinkedHashMap<String, LlvmType<'a>>,
        args: &LinkedHashMap<String, LlvmValue<'a>>,
        member_range_provider: impl Fn(usize) -> Option<Range>,
        full_range: Range,
    ) -> (LlvmValue<'a>, LinkedHashMap<String, LlvmValue<'a>>) {
        // (LlvmValue::)
        todo!()
        /*
        let arg_len = args.len();
        let len_off = members.len() != args.len();

        let arg_vals: LinkedHashMap<_, _> = members
            .iter()
            .enumerate()
            .filter_map(|(i, (name, ty))| {
                if let Some(arg) = args.get(name) {
                    let arg = arg
                        .try_implicit_cast(ty, self.type_provider.as_ref())
                        .unwrap_or_else(|| arg.clone());

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
            return (
                V::create_struct_instance(symbol.clone(), self.type_provider.as_ref()),
                arg_vals,
            );
        }
        (V::empty(self.type_provider.as_ref()), LinkedHashMap::new())
        */
    }

    pub fn add_error(&self, error: EvaluationError<LlvmType<'a>>) {
        self.wstate().errors.push(error)
    }
}

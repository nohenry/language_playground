use std::{sync::{Arc, RwLock}, marker::PhantomData, rc::Rc};

use tl_core::Module;
use tl_evaluator::{pass::Pass, evaluator::EvaluatorState};

use crate::LlvmContext;


pub struct LlvmEvaluator<'a, P: Pass> {
    module: Arc<Module>,
    pub state: RwLock<EvaluatorState<LlvmType<'a>, LlvmValue<'a>>>,
    pd: PhantomData<&'a P>,
    pub context: Rc<LlvmContext<'a>>,
}
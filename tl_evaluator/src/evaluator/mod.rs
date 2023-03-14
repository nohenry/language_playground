
use crate::evaluation_type::EvaluationType;
use crate::evaluation_value::EvaluationValue;

pub struct EvaluatorState {
    pub scope: ScopeManager,
    pub errors: Vec<EvaluationError>,
}

pub struct Evaluator<T: EvaluationType, V: EvaluationValue> {

}
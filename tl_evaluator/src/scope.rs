
use crate::evaluation_type::EvaluationType;
use crate::evaluation_value::EvaluationValue;

use linked_hash_map::LinkedHashMap;

pub enum ScopeValue<T: EvaluationType, V: EvaluationValue> {
    EvaluationValue(V),
    Struct {
        ident: String,
        members: LinkedHashMap<String, T>,
    },
}
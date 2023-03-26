pub trait Pass {}

pub struct TypeFirst;
impl Pass for TypeFirst {}

pub struct TypeSecond;
impl Pass for TypeSecond {}

pub struct MemberPass;
impl Pass for MemberPass {}

pub struct EvaluationPass;
impl Pass for EvaluationPass {}

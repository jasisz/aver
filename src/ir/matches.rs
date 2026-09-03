use super::WrapperKind;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DispatchLiteral {
    Int(i64),
    Float(String),
    Bool(bool),
    Str(String),
    Unit,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SemanticDispatchPattern {
    Literal(DispatchLiteral),
    EmptyList,
    NoneValue,
    WrapperTag(WrapperKind),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BoolMatchShape {
    pub true_arm_index: usize,
    pub false_arm_index: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BoolCompareOp {
    Eq,
    Lt,
    Gt,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ListMatchShape {
    pub empty_arm_index: usize,
    pub cons_arm_index: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DispatchBindingPlan {
    None,
    WrapperPayload(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DispatchArmPlan {
    pub pattern: SemanticDispatchPattern,
    pub arm_index: usize,
    pub binding: DispatchBindingPlan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DispatchDefaultPlan {
    pub arm_index: usize,
    pub binding_name: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DispatchTableShape {
    pub entries: Vec<DispatchArmPlan>,
    pub default_arm: Option<DispatchDefaultPlan>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MatchDispatchPlan {
    Bool(BoolMatchShape),
    List(ListMatchShape),
    Table(DispatchTableShape),
}

mod calls;
mod matches;

pub use calls::{
    CallLowerCtx, CallPlan, SemanticCallee, SemanticConstructor, TailCallPlan, WrapperKind,
    classify_call_plan, classify_callee, classify_constructor_name, classify_tail_call_plan,
    expr_to_dotted_name, is_builtin_namespace,
};
pub use matches::{
    BoolMatchShape, DispatchArmPlan, DispatchBindingPlan, DispatchDefaultPlan, DispatchLiteral,
    DispatchTableShape, ListMatchShape, MatchDispatchPlan, SemanticDispatchPattern,
    classify_bool_match_shape, classify_dispatch_pattern, classify_dispatch_table_shape,
    classify_list_match_shape, classify_match_dispatch_plan,
};

#[cfg(test)]
mod tests;

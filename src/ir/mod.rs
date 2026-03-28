mod body;
mod calls;
mod leaf;
mod matches;

pub use body::{
    BodyBindingPlan, BodyExprPlan, BodyPlan, ThinBodyCtx, ThinBodyPlan, ThinKind,
    classify_body_expr_plan, classify_body_plan, classify_thin_body_plan, classify_thin_fn_def,
};
pub use calls::{
    CallLowerCtx, CallPlan, ForwardArg, ForwardCallPlan, SemanticCallee, SemanticConstructor,
    TailCallPlan, WrapperKind, classify_call_plan, classify_callee, classify_constructor_name,
    classify_forward_call_parts, classify_forward_call_plan, classify_forward_fn_body,
    classify_tail_call_plan, expr_to_dotted_name, is_builtin_namespace,
};
pub use leaf::{LeafOp, classify_leaf_op};
pub use matches::{
    BoolCompareOp, BoolMatchShape, BoolSubjectPlan, DispatchArmPlan, DispatchBindingPlan,
    DispatchDefaultPlan, DispatchLiteral, DispatchTableShape, ListMatchShape, MatchDispatchPlan,
    SemanticDispatchPattern, classify_bool_match_shape, classify_bool_match_shape_from_patterns,
    classify_bool_subject_plan, classify_dispatch_pattern, classify_dispatch_table_shape,
    classify_dispatch_table_shape_from_patterns, classify_list_match_shape,
    classify_list_match_shape_from_patterns, classify_match_dispatch_plan,
    classify_match_dispatch_plan_from_patterns,
};

#[cfg(test)]
mod tests;

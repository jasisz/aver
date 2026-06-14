pub mod alias;
mod alloc_info;
mod analyze;
mod body;
mod buffer_build;
mod calls;
pub mod dump;
pub mod escape;
pub mod hir;
pub mod identity;
mod interp_lower;
pub mod interval;
pub mod last_use;
mod leaf;
mod matches;
/// Core MIR — executable middle-end. Phase 1 of #252 (the 0.24
/// epic) is doc-only; see `src/ir/mir/mod.rs` for the overview and
/// `src/ir/mir/RFC.md` for the full design.
pub mod mir;
mod pass_diag;
pub mod pipeline;
pub mod proof_ir;
pub mod symbol_table;
pub mod vars;

pub use analyze::{AnalysisResult, BodyShape, FnAnalysis, NeutralAllocPolicy, analyze};
pub use identity::{BuiltinId, CtorId, FnId, FnKey, LawKey, ModuleId, TypeId, TypeKey};
pub use interval::{
    Bound, Interval, IntervalAnalysisResult, OpClass, RefinedTypeInterval,
    analyze as interval_analyze,
};
pub use pipeline::{
    FnCountChange, NonTailEntry, PassDiagnostic, PassReport, PipelineConfig, PipelineResult,
    PipelineStage, TypecheckMode,
};
pub use proof_ir::{
    DecreaseProof, EscapePairSpec, FloorDivShrink, FloorWindowFigure, FnContract, FuelMetric,
    LawTheorem, MapUpdatePostconditionKind, Measure, NativeIntCountdownBody, Predicate,
    PreservationProof, ProofIR, ProofStrategy, Quantifier, QuantifierType, RecursionContract,
    RefinedTypeDecl, SmartGuard, StringEscapeRoundtripPin, UnclassifiedFn,
};
pub use symbol_table::{CtorEntry, FnEntry, ModuleEntry, SymbolTable, TypeEntry};

pub use alloc_info::{
    AllocPolicy, compute_alloc_info, count_alloc_sites_in_fn, count_alloc_sites_in_program,
};
pub use body::{
    BodyBindingPlan, BodyExprPlan, BodyPlan, ThinBodyCtx, ThinBodyPlan, ThinKind,
    classify_body_expr_plan, classify_body_plan, classify_thin_body_plan, classify_thin_fn_def,
    thin_body_plan_is_parent_thin_candidate, thin_kind_is_parent_thin_candidate,
};
pub use buffer_build::{
    BufferBuildShape, ConsumerKind, FusionSite, compute_buffer_build_sinks, find_fusion_sites,
    rewrite_fusion_sites, run_buffer_build_pass, synthesize_buffered_variants,
};
pub use calls::{
    CallLowerCtx, CallPlan, ForwardArg, ForwardCallPlan, SemanticCallee, SemanticConstructor,
    TailCallPlan, WrapperKind, classify_call_plan, classify_callee, classify_constructor_name,
    classify_forward_call_parts, classify_forward_call_plan, classify_forward_fn_body,
    classify_tail_call_plan, expr_to_dotted_name, is_builtin_namespace,
};
pub use interp_lower::lower_interpolation_pass;
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

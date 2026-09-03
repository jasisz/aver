pub mod alias;
mod alloc_info;
mod analyze;
mod body;
mod buffer_build;
mod calls;
mod chars_fusion;
pub mod dump;
pub mod escape;
pub mod hir;
pub mod identity;
mod interp_lower;
pub mod interval;
pub mod last_use;
mod matches;
/// Core MIR — executable middle-end. Phase 1 of #252 (the 0.24
/// epic) is doc-only; see `src/ir/mir/mod.rs` for the overview and
/// `src/ir/mir/RFC.md` for the full design.
pub mod mir;
mod pass_diag;
pub mod pipeline;
pub mod proof_ir;
mod string_index;
pub mod symbol_table;
pub mod vars;

pub use analyze::{AnalysisResult, FnAnalysis, NeutralAllocPolicy, analyze};
pub(crate) use buffer_build::{
    INTERNAL_BUFFER_TYPE, INTERNAL_BYTE_BUILDER_TYPE, INTERNAL_BYTE_PAYLOAD_TYPE,
};
pub use identity::{BuiltinId, CtorId, FnId, FnKey, LawKey, ModuleId, TypeId, TypeKey};
pub use interval::{
    Bound, Interval, IntervalAnalysisResult, OpClass, RefinedTypeInterval,
    analyze as interval_analyze,
};
pub use pipeline::{
    AstView, FnCountChange, NonTailEntry, PassDiagnostic, PassReport, PipelineConfig,
    PipelineResult, PipelineStage, TypecheckMode,
};
pub use proof_ir::{
    DecreaseProof, EscapePairSpec, FloorDivShrink, FloorWindowFigure, FnContract, FuelMetric,
    LawTheorem, MapUpdatePostconditionKind, Measure, NativeIntCountdownBody, Predicate,
    PreservationProof, ProofIR, ProofStrategy, Quantifier, QuantifierType, RecursionContract,
    RefinedTypeDecl, SmartGuard, StringEscapeRoundtripPin, UnclassifiedFn, WrapperDriver,
};
pub use string_index::{StringIndexPassReport, has_string_index_shape, run_string_index_pass};
pub use symbol_table::{CtorEntry, FnEntry, ModuleEntry, SymbolTable, TypeEntry};

pub use alloc_info::{
    AllocPolicy, compute_alloc_info, count_alloc_sites_in_fn, count_alloc_sites_in_program,
};
pub use body::{ThinKind, thin_kind_is_parent_thin_candidate};
pub use buffer_build::{
    BufferBuildPassReport, BufferBuildShape, ByteSinkDecline, ConsumerKind, FusionSite,
    ListBuildDecline, ListBuildKind, ListBuildPassReport, ListBuildShape,
    compute_buffer_build_sinks, find_fusion_sites, has_list_build_shape, rewrite_fusion_sites,
    run_buffer_build_pass, run_byte_sink_pass, run_list_build_pass, synthesize_buffered_variants,
};
pub use calls::{
    CallPlan, SemanticConstructor, WrapperKind, expr_to_dotted_name, is_builtin_namespace,
};
pub use chars_fusion::{
    CharsFusionDecline, CharsFusionPassReport, has_fusable_shape, run_chars_fusion_pass,
};
pub use interp_lower::lower_interpolation_pass;
pub use matches::{
    BoolCompareOp, BoolMatchShape, DispatchArmPlan, DispatchBindingPlan, DispatchDefaultPlan,
    DispatchLiteral, DispatchTableShape, ListMatchShape, MatchDispatchPlan,
    SemanticDispatchPattern,
};

#[cfg(test)]
mod tests;

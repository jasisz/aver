/// Top-level Aver items → Lean 4 items (defs, inductives, structures, examples).
mod fn_def;
pub(in crate::codegen::lean) mod fuel;
mod lex_list;
mod render;
mod type_def;
mod verify;

pub use fn_def::{emit_fn_def, emit_fn_def_proof, emit_mutual_group, emit_mutual_group_proof};
pub use fuel::PROOF_FUEL_EXHAUSTED_MSG;
pub(super) use fuel::{law_fuel_simp_names, law_string_pos_rank};
pub use type_def::{
    emit_inhabited_instance, emit_recursive_decidable_eq, emit_recursive_measure,
    emit_type_def_in_scope,
};
pub(crate) use verify::law_as_lemma_statement;
pub(super) use verify::law_given_domain_values;
pub(crate) use verify::law_theorem_base;
pub use verify::{emit_decision, emit_verify_block};

// Aliases into the parent `lean` module so the submodules' `super::<item>`
// paths keep resolving exactly as they did when this was a single file.
pub(super) use super::{
    LAW_CLASS_BOUNDED_DOMAIN, LAW_CLASS_MARKER_PREFIX, LAW_CLASS_UNIVERSAL, VerifyEmitMode,
    bound_expr_to_lean, expr, law_auto, recurrence, sample_literal, shared,
    sizeof_measure_param_indices, types,
};

/// Check if a sum type is self-referencing (any variant field mentions the type name).
// Recursive-type / field-type predicates moved to `codegen::common` so
// all three backends share a single source of truth. Re-exported below
// as pub(crate) for existing call sites in this backend.
pub(crate) use crate::codegen::common::{
    is_pure_fn, is_recursive_product, is_recursive_sum as is_recursive_type, is_recursive_type_def,
    type_def_name,
};

//! Type readers for the wasm-gc body emitter.
//!
//! After Step 3 (the typed-ABI refactor), the wasm-gc backend no longer
//! performs ad-hoc inference. Every `Spanned<Expr>` reaching codegen has
//! its `Type` stamped by the type checker (`Spanned::ty()`). The
//! accessors here are thin readers that panic if the type was not
//! stamped — that means either the type checker did not run before
//! codegen, or a synthesised AST node skipped `Spanned::set_ty(...)`
//! (interp_lower / buffer_build are the usual suspects). Both are bugs
//! to fix at the source, not to paper over with a fallback here.
//!
//! Schema-level helpers that were previously co-located with inference
//! (option-/result-shape match recognition) stay in this module — they
//! describe the program's static AST shape, not its inferred types.

use wasm_encoder::ValType;

use crate::ast::{Expr, MatchArm, Pattern, Spanned};
use crate::types::Type;

use super::super::WasmGcError;
use super::super::types::{TypeRegistry, aver_to_wasm};

// ---------------------------------------------------------------------------
// Match-arm shape predicates (schema-level — no type inference involved)
// ---------------------------------------------------------------------------

/// True when a match arm matches against `Option.Some(_)` or
/// `Option.None`. Used to opt the surrounding match into the
/// dedicated tag-based dispatch path (instead of the generic
/// `ref.test` cascade for user variants).
pub(super) fn arm_is_option_pattern(arm: &MatchArm) -> bool {
    if let Pattern::Constructor(name, _) = &arm.pattern {
        let bare = name.rsplit('.').next().unwrap_or(name);
        return name == "Option.Some"
            || name == "Option.None"
            || ((bare == "Some" || bare == "None") && name.starts_with("Option"));
    }
    false
}

/// True when a match arm targets `Result.Ok(_)` or `Result.Err(_)`.
pub(super) fn arm_is_result_pattern(arm: &MatchArm) -> bool {
    if let Pattern::Constructor(name, _) = &arm.pattern {
        let bare = name.rsplit('.').next().unwrap_or(name);
        return name == "Result.Ok"
            || name == "Result.Err"
            || ((bare == "Ok" || bare == "Err") && name.starts_with("Result"));
    }
    false
}

// ---------------------------------------------------------------------------
// Typed-AST accessors
// ---------------------------------------------------------------------------

/// Inferred Aver type for a `Spanned<Expr>`. Panics if the type checker
/// did not stamp this node — that is a pipeline bug, not a recoverable
/// codegen condition (see module doc).
#[track_caller]
pub(super) fn aver_type_of(expr: &Spanned<Expr>) -> &Type {
    expr.ty().unwrap_or_else(|| {
        panic!(
            "wasm-gc emit: expression has no type — typecheck must run before codegen \
             (Step 0 setter or synthesised AST without set_ty); offending node: {:?}",
            expr.node
        )
    })
}

/// Display string of the stamped Aver type. Most of the existing
/// lowering machinery is keyed on the canonical type-name string
/// (`record_field_type`, `aver_to_wasm`, registry canonical lookups),
/// so a single `display()` per call site keeps the diff small.
#[track_caller]
pub(super) fn aver_type_str_of(expr: &Spanned<Expr>) -> String {
    aver_type_of(expr).display()
}

/// WASM machine type for a `Spanned<Expr>`. Same panic contract as
/// `aver_type_of`; `Ok(None)` for Unit (no value pushed).
#[track_caller]
pub(super) fn wasm_type_of(
    expr: &Spanned<Expr>,
    registry: &TypeRegistry,
) -> Result<Option<ValType>, WasmGcError> {
    aver_to_wasm(&aver_type_str_of(expr), Some(registry))
}

/// Display string of the stamped Aver type in the canonical no-whitespace
/// form used by wasm-gc registries. Generic constructors must already be
/// resolved by the type checker before codegen reaches this reader.
#[track_caller]
pub(super) fn aver_type_canonical(
    expr: &Spanned<Expr>,
    _return_type: &str,
    _registry: &TypeRegistry,
) -> String {
    let raw = aver_type_str_of(expr);
    raw.chars().filter(|c| !c.is_whitespace()).collect()
}

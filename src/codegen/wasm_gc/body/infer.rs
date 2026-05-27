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

use crate::ast::Spanned;
use crate::types::Type;

use super::super::WasmGcError;
use super::super::types::{TypeRegistry, aver_to_wasm};

// ---------------------------------------------------------------------------
// Match-arm shape predicates (schema-level — no type inference involved)
// ---------------------------------------------------------------------------

/// True when a match arm matches against `Option.Some(_)` /
/// `Option.None`. Used to opt the surrounding match into the
/// dedicated tag-based dispatch path (instead of the generic
/// `ref.test` cascade for user variants). Reads identity off
/// [`crate::ir::hir::ResolvedCtor::Builtin`] — post Phase E PR 9.1
/// every reachable arm carries the typed ctor, so the pre-resolve
/// source-shape variant is gone.
pub(super) fn arm_is_option_pattern_resolved(arm: &crate::ir::hir::ResolvedMatchArm) -> bool {
    use crate::ir::hir::{BuiltinCtor, ResolvedCtor, ResolvedPattern};
    matches!(
        &arm.pattern,
        ResolvedPattern::Ctor(
            ResolvedCtor::Builtin(BuiltinCtor::OptionSome | BuiltinCtor::OptionNone),
            _,
        )
    )
}

/// True when a match arm matches against `Result.Ok(_)` /
/// `Result.Err(_)`. Same Phase E shape as
/// [`arm_is_option_pattern_resolved`].
pub(super) fn arm_is_result_pattern_resolved(arm: &crate::ir::hir::ResolvedMatchArm) -> bool {
    use crate::ir::hir::{BuiltinCtor, ResolvedCtor, ResolvedPattern};
    matches!(
        &arm.pattern,
        ResolvedPattern::Ctor(
            ResolvedCtor::Builtin(BuiltinCtor::ResultOk | BuiltinCtor::ResultErr),
            _,
        )
    )
}

// ---------------------------------------------------------------------------
// Typed-AST accessors
// ---------------------------------------------------------------------------

/// Inferred Aver type for a typed `Spanned<T>`. Panics if the type
/// checker did not stamp this node — that is a pipeline bug, not a
/// recoverable codegen condition (see module doc).
///
/// Generic over the wrapped node type so the readers work uniformly
/// against both `Spanned<crate::ast::Expr>` (source-shape, used by
/// the pre-PR-9.x dispatch sites that haven't migrated yet) and
/// `Spanned<crate::ir::hir::ResolvedExpr>` (resolved, used by the
/// migrated emitters). `Spanned::ty()` is generic over `T` already,
/// so the readers don't care which IR shape the node carries.
#[track_caller]
pub(super) fn aver_type_of<T: std::fmt::Debug>(expr: &Spanned<T>) -> &Type {
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
pub(super) fn aver_type_str_of<T: std::fmt::Debug>(expr: &Spanned<T>) -> String {
    aver_type_of(expr).display()
}

/// WASM machine type for a typed `Spanned<T>`. Same panic contract
/// as `aver_type_of`; `Ok(None)` for Unit (no value pushed).
#[track_caller]
pub(super) fn wasm_type_of<T: std::fmt::Debug>(
    expr: &Spanned<T>,
    registry: &TypeRegistry,
) -> Result<Option<ValType>, WasmGcError> {
    aver_to_wasm(&aver_type_str_of(expr), Some(registry))
}

/// Display string of the stamped Aver type in the canonical no-whitespace
/// form used by wasm-gc registries. Generic constructors must already be
/// resolved by the type checker before codegen reaches this reader.
#[track_caller]
pub(super) fn aver_type_canonical<T: std::fmt::Debug>(
    expr: &Spanned<T>,
    _return_type: &str,
    _registry: &TypeRegistry,
) -> String {
    let raw = aver_type_str_of(expr);
    raw.chars().filter(|c| !c.is_whitespace()).collect()
}

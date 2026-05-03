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

/// Display string of the stamped Aver type **with context-driven
/// recovery for the common Type::Unknown loophole**: when the
/// typechecker stamps a generic type with one or more `Unknown`
/// branches (e.g. `Map.empty()` reports `Map<Unknown, Unknown>`,
/// `List.empty()` reports `List<Unknown>`, `Option.None` reports
/// `Option<Unknown>`), recover the missing branch from:
///   1. A single registered instantiation of that generic in the
///      registry (covers the common "fn returns the only Map<K,V> in
///      the program" shape).
///   2. The enclosing fn's return type (covers tail-position
///      constructors like `fn f() -> List<Int>; []` or
///      `fn g() -> Result<T,E>; Result.Ok(v)`).
///
/// Returning the original (Unknown-bearing) display is the last
/// resort — callers that need a registered canonical will then surface
/// a clear "no helpers registered" error.
#[track_caller]
pub(super) fn aver_type_canonical(
    expr: &Spanned<Expr>,
    return_type: &str,
    registry: &TypeRegistry,
) -> String {
    let raw = aver_type_str_of(expr);
    let stripped: String = raw.chars().filter(|c| !c.is_whitespace()).collect();
    if !stripped.contains("Unknown") {
        return stripped;
    }
    // Try the enclosing fn's return type first if it shares the head
    // (`Map<...>` vs `List<...>` etc.) — it carries both K and V from
    // the source signature.
    let return_canonical: String = return_type.chars().filter(|c| !c.is_whitespace()).collect();
    let head = stripped
        .split_once('<')
        .map(|(h, _)| h)
        .unwrap_or(stripped.as_str());
    if return_canonical.starts_with(&format!("{head}<")) && !return_canonical.contains("Unknown") {
        return return_canonical;
    }
    // Single-instantiation registry fallback.
    let single = match head {
        "Map" if registry.map_order.len() == 1 => Some(&registry.map_order[0]),
        "List" if registry.list_order.len() == 1 => Some(&registry.list_order[0]),
        "Option" if registry.option_order.len() == 1 => Some(&registry.option_order[0]),
        "Result" if registry.result_order.len() == 1 => Some(&registry.result_order[0]),
        "Vector" if registry.vector_order.len() == 1 => Some(&registry.vector_order[0]),
        _ => None,
    };
    if let Some(c) = single {
        return c.clone();
    }
    // Last resort: return the Unknown-bearing string and let the
    // caller surface a clear validation error.
    stripped
}

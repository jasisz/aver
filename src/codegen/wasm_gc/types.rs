//! Aver type → wasm-gc representation.
//!
//! Phase 2 covers primitives: `Int → i64`, `Float → f64`, `Bool → i32`,
//! `Unit → empty`. Phase 3 will add struct types for `List`/`Tuple`/
//! `Record`/`Constructor`. The mapping is deliberately type-direct —
//! no NaN-boxing, no value-tagging — so once we know the static type,
//! lowering picks one branch and stays there.

use wasm_encoder::ValType;

use super::WasmGcError;

/// Resolve an Aver type-annotation string (as it appears in fn signatures
/// and `params: Vec<(String, String)>`) to a wasm value type, or to
/// "no result" when the type is `Unit`.
///
/// Returns `Ok(None)` for `Unit` so callers can pick the empty result
/// list, and an error for anything Phase 2 doesn't yet handle.
pub(super) fn aver_to_wasm(type_str: &str) -> Result<Option<ValType>, WasmGcError> {
    match type_str.trim() {
        "Int" => Ok(Some(ValType::I64)),
        "Float" => Ok(Some(ValType::F64)),
        "Bool" => Ok(Some(ValType::I32)),
        "Unit" => Ok(None),
        // Anything else lands in phase 3+; refuse explicitly so future
        // calls to this helper from a half-implemented phase produce a
        // clear "you skipped a step" error rather than a malformed
        // wasm module.
        other => Err(WasmGcError::Unimplemented(match other {
            "String" => "phase 3 — String lowering",
            _ if other.starts_with("List<") => "phase 3 — List<T>",
            _ if other.starts_with("Tuple<") => "phase 3 — Tuple",
            _ if other.starts_with("Map<") => "phase 3 — Map<K,V>",
            _ if other.starts_with("Vector<") => "phase 3 — Vector<T>",
            _ if other.starts_with("Result<") => "phase 3 — Result",
            _ if other.starts_with("Option<") => "phase 3 — Option",
            _ => "phase 3 — user-defined type",
        })),
    }
}

/// Result-list shape for a wasm function signature derived from an
/// Aver return type. `Unit` yields the empty list so the wasm fn has
/// zero results.
pub(super) fn return_results(type_str: &str) -> Result<Vec<ValType>, WasmGcError> {
    Ok(aver_to_wasm(type_str)?.into_iter().collect())
}

/// Param-list shape for a wasm function signature derived from an
/// Aver parameter list. `Unit`-typed params are filtered out (wasm
/// has no zero-width values; an Aver `Unit` parameter carries no
/// information at runtime anyway).
pub(super) fn param_types(params: &[(String, String)]) -> Result<Vec<ValType>, WasmGcError> {
    let mut out = Vec::with_capacity(params.len());
    for (_, ty) in params {
        if let Some(v) = aver_to_wasm(ty)? {
            out.push(v);
        }
    }
    Ok(out)
}

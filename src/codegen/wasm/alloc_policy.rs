//! WASM-backend allocation policy.
//!
//! Conservative whitelist: a builtin is "non-allocating" only if we are
//! certain it produces no heap object on the WASM ABI. Everything else
//! is treated as allocating, so misclassification can only ever cost us
//! a missed optimization, never a soundness bug (a fn flagged as
//! allocating still gets the GC framing it had before this analysis).
//!
//! Pure-non-alloc set covers:
//!   - integer arithmetic predicates / minmax / abs / mod-as-i64
//!   - float arithmetic, transcendentals, predicates, constants
//!   - cheap conversions (Int↔Float, Char.toCode)
//!   - container metadata (length / contains / starts-with / etc.) that
//!     returns a primitive
//!
//! Constructors: any constructor applied to a payload is treated as
//! allocating under the WASM ABI (`WRAP_SOME`/`WRAP_OK`/record handles
//! all need a heap slot). Nullary constructors (e.g. `Option.None`) don't
//! reach `constructor_allocates` with `has_payload=true`.

use crate::ir::AllocPolicy;

/// Conservative WASM allocation policy.
pub struct WasmAllocPolicy;

impl AllocPolicy for WasmAllocPolicy {
    fn builtin_allocates(&self, name: &str) -> bool {
        !is_pure_non_alloc_builtin(name)
    }

    fn constructor_allocates(&self, _name: &str, has_payload: bool) -> bool {
        // Every user/built-in constructor with a payload heap-allocates a
        // wrapper on the WASM ABI. Nullary constructors stay inline.
        has_payload
    }
}

/// Whitelist of builtins that produce only primitive values (no heap
/// allocation) on the current WASM ABI. Adding a name here unlocks the
/// no-alloc optimization for any fn that calls it; missing names cost a
/// missed optimization, not correctness.
fn is_pure_non_alloc_builtin(name: &str) -> bool {
    matches!(
        name,
        // Integer arithmetic & predicates returning Int/Bool.
        "Int.abs"
            | "Int.min"
            | "Int.max"
            | "Int.toFloat"
            // Float arithmetic, transcendentals, constants, predicates.
            | "Float.abs"
            | "Float.floor"
            | "Float.ceil"
            | "Float.round"
            | "Float.min"
            | "Float.max"
            | "Float.sin"
            | "Float.cos"
            | "Float.sqrt"
            | "Float.pow"
            | "Float.atan2"
            | "Float.pi"
            | "Float.fromInt"
            // Char ↔ code-point. `Char.fromCode` returns Option<String>
            // (allocates the Some wrapper around a heap String); only the
            // reverse direction is pure.
            | "Char.toCode"
            // String predicates / length — return Bool/Int.
            | "String.len"
            | "String.byteLength"
            | "String.startsWith"
            | "String.endsWith"
            | "String.contains"
            // Collection metadata returning Int/Bool.
            | "List.len"
            | "List.contains"
            | "Vector.len"
            | "Map.size"
            | "Map.contains"
            | "Set.size"
            | "Set.contains"
            // Bool ops.
            | "Bool.and"
            | "Bool.or"
            | "Bool.not"
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pure_math_is_non_alloc() {
        let p = WasmAllocPolicy;
        assert!(!p.builtin_allocates("Float.sin"));
        assert!(!p.builtin_allocates("Float.sqrt"));
        assert!(!p.builtin_allocates("Int.abs"));
        assert!(!p.builtin_allocates("Int.toFloat"));
    }

    #[test]
    fn allocating_builtins_flagged() {
        let p = WasmAllocPolicy;
        assert!(p.builtin_allocates("String.fromInt"));
        assert!(p.builtin_allocates("List.prepend"));
        assert!(p.builtin_allocates("Map.set"));
        assert!(p.builtin_allocates("Map.get"));
        assert!(p.builtin_allocates("String.charAt"));
    }

    #[test]
    fn constructors_with_payload_allocate() {
        let p = WasmAllocPolicy;
        assert!(p.constructor_allocates("Option.Some", true));
        assert!(p.constructor_allocates("Result.Ok", true));
        assert!(p.constructor_allocates("Shape.Circle", true));
        // Nullary constructors don't go through this path; if they did,
        // we'd say they're free.
        assert!(!p.constructor_allocates("Option.None", false));
    }

    #[test]
    fn unknown_builtin_is_conservatively_allocating() {
        let p = WasmAllocPolicy;
        assert!(p.builtin_allocates("Future.weird_op"));
    }
}

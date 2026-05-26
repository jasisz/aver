//! VM-backend allocation policy.
//!
//! Mirrors `WasmAllocPolicy` for the same set of pure builtins, but
//! the VM's NaN-boxed Value can in principle keep more constructors
//! inline (e.g. `Option.Some(Int)` lives in tag bits, no heap obj).
//!
//! For 0.14.2 we stay conservative — same whitelist as WASM. The
//! NaN-box-aware extensions are easy to add later as additional pure
//! cases (`builtin_allocates` returning `false` for more names).

use crate::ir::AllocPolicy;

/// VM allocation-policy snapshot. Mirrors `WasmAllocPolicy`'s
/// non-allocating builtin whitelist. Kept for symmetry / future
/// callers; the Phase E VM compile path consumes
/// `PipelineResult.analysis.fn_analyses[name].allocates` instead of
/// re-deriving alloc info, so no production caller instantiates
/// this directly today.
#[allow(dead_code)]
pub struct VmAllocPolicy;

impl AllocPolicy for VmAllocPolicy {
    fn builtin_allocates(&self, name: &str) -> bool {
        !is_pure_non_alloc_builtin(name)
    }

    fn constructor_allocates(&self, _name: &str, has_payload: bool) -> bool {
        // NaN-box variants without a payload (`Option.None`, `Result.Err`
        // applied to a primitive seed, etc.) ride in tag bits. Variants
        // with a payload still need a heap arena entry today, so be
        // conservative and treat them as allocating.
        has_payload
    }
}

#[allow(dead_code)]
fn is_pure_non_alloc_builtin(name: &str) -> bool {
    matches!(
        name,
        "Int.abs"
            | "Int.min"
            | "Int.max"
            | "Float.fromInt"
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
            | "Char.toCode"
            | "String.len"
            | "String.byteLength"
            | "String.startsWith"
            | "String.endsWith"
            | "String.contains"
            | "List.len"
            | "List.contains"
            | "Vector.len"
            | "Map.size"
            | "Map.contains"
            | "Set.size"
            | "Set.contains"
            | "Bool.and"
            | "Bool.or"
            | "Bool.not"
    )
}

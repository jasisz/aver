//! Builtin functions emitted as per-module helper fns.
//!
//! ## Strategy
//!
//! Aver's builtin namespace (`Int.toString`, `List.prepend`, `Map.get`,
//! `String.concat`, …) splits two ways:
//!
//! - **Pure builtins** — `Int.toString`, `List.prepend`, `Map.empty`,
//!   `Vector.get`, etc. — produce wasm-side data. We emit each one as
//!   a *local helper function* inside the user's wasm module on first
//!   use. Same pattern rustc uses for stdlib helpers in its wasm
//!   output. No separate runtime module, no host dependency. Helpers
//!   that aren't used get DCE'd by `wasm-opt -Oz`.
//!
//! - **Effectful builtins** — `Console.print`, `Http.get`, `File.read`
//!   — go through `(import "aver" "...")` so the host (browser /
//!   workerd / wasmtime+wasi) supplies the implementation. This is
//!   the same shape the legacy backend uses for effects, just
//!   without the `aver_runtime.wasm` middleman. Host-native fast
//!   paths, zero wasm overhead. Lives in `effects.rs` (separate file
//!   because the registration shape differs — imports declared in
//!   the import section, not emitted as helper fns).
//!
//! ## Lifecycle
//!
//! 1. **Discovery** — `module::emit_module` walks the IR before fn
//!    bodies emit and calls `BuiltinRegistry::register_used_builtins`
//!    which scans for dotted callees against the known builtin set.
//!    Each unique builtin gets a slot reserved in the type and
//!    function sections.
//! 2. **Call site emit** — `body::emit_dotted_builtin` looks up the
//!    builtin in the registry, gets its wasm fn idx, and emits
//!    `call $builtin_idx`. Same shape as user fn calls — the wasm
//!    validator can't tell the difference.
//! 3. **Helper bodies** — emitted after user fns, same code section,
//!    each one using the standard `Function::new` shape.
//!
//! ## Module layout
//!
//! - `mod.rs` (this file) — `BuiltinRegistry`, dispatch by name,
//!   public emit API.
//! - `int.rs` — `Int.*` builtins.
//! - `float.rs` — `Float.*` builtins.
//! - (more later: `list.rs`, `string.rs`, `map.rs`, `vector.rs`)

#![allow(dead_code)]
//! Phase 3c entry point — scaffold today, wiring lands when the
//! string representation decision is made (array i8 vs stringref vs
//! linear-memory-backed struct). Until then this module compiles
//! clean, documents the design, and provides `BuiltinRegistry`
//! ready for `module.rs` to consume.

use std::collections::HashMap;

use wasm_encoder::{Function, ValType};

use super::WasmGcError;

mod float;
mod int;

/// Per-module registry of builtins that need a helper fn slot.
/// Populated during the pre-emit walk; consumed when fn types and
/// helper bodies get emitted.
#[derive(Default)]
pub(super) struct BuiltinRegistry {
    /// Dotted name (`"Int.toString"`) → slot info (wasm fn idx,
    /// signature, the emit thunk that fills in the helper body).
    used: HashMap<String, BuiltinSlot>,
    /// Insertion order — preserves emit order so wasm fn indices are
    /// stable across (deterministic) runs.
    order: Vec<String>,
}

#[derive(Clone)]
pub(super) struct BuiltinSlot {
    pub(super) wasm_fn_idx: u32,
    pub(super) wasm_type_idx: u32,
    pub(super) params: Vec<ValType>,
    pub(super) results: Vec<ValType>,
    pub(super) name: &'static str,
}

impl BuiltinRegistry {
    pub(super) fn new() -> Self {
        Self::default()
    }

    /// Register `name` if it's a known builtin and we haven't seen it
    /// yet. `next_wasm_fn_idx` and `next_type_idx` are the slot
    /// counters the caller maintains; we return the (possibly new)
    /// slot info.
    pub(super) fn register(
        &mut self,
        name: &str,
        next_wasm_fn_idx: &mut u32,
        next_type_idx: &mut u32,
    ) -> Result<Option<BuiltinSlot>, WasmGcError> {
        if let Some(existing) = self.used.get(name) {
            return Ok(Some(existing.clone()));
        }
        let Some(spec) = lookup_spec(name) else {
            return Ok(None);
        };
        let slot = BuiltinSlot {
            wasm_fn_idx: *next_wasm_fn_idx,
            wasm_type_idx: *next_type_idx,
            params: spec.params,
            results: spec.results,
            name: spec.canonical_name,
        };
        *next_wasm_fn_idx += 1;
        *next_type_idx += 1;
        self.used.insert(name.to_string(), slot.clone());
        self.order.push(name.to_string());
        Ok(Some(slot))
    }

    pub(super) fn lookup(&self, name: &str) -> Option<&BuiltinSlot> {
        self.used.get(name)
    }

    /// Iterate registered builtins in insertion order — useful for
    /// emitting type entries and code bodies in a stable sequence.
    pub(super) fn iter_in_order(&self) -> impl Iterator<Item = &BuiltinSlot> {
        self.order.iter().map(move |n| &self.used[n])
    }

    /// Emit the body of every registered builtin into `codes`, in
    /// the same order they were declared. The caller has already
    /// emitted matching entries in the type and function sections.
    pub(super) fn emit_helper_bodies(
        &self,
        codes: &mut wasm_encoder::CodeSection,
    ) -> Result<(), WasmGcError> {
        for slot in self.iter_in_order() {
            let mut func = build_helper(slot)?;
            // Each helper closes its own End; build_helper already
            // emits it. Just push the function record.
            let _ = &mut func;
            codes.function(&func);
        }
        Ok(())
    }
}

/// Per-builtin static spec — what its wasm signature is and which
/// emit thunk fills the body.
struct BuiltinSpec {
    canonical_name: &'static str,
    params: Vec<ValType>,
    results: Vec<ValType>,
    /// Emits the body (instructions up to and including `End`) into
    /// the supplied `Function`.
    body: fn(&mut Function) -> Result<(), WasmGcError>,
}

fn lookup_spec(name: &str) -> Option<BuiltinSpec> {
    int::SPECS
        .iter()
        .chain(float::SPECS.iter())
        .find(|s| s.canonical_name == name)
        .map(|s| BuiltinSpec {
            canonical_name: s.canonical_name,
            params: s.params.to_vec(),
            results: s.results.to_vec(),
            body: s.body,
        })
}

/// Static spec entry — `&[ValType]` slices used at the const level
/// because each builtin's signature is fixed at compile time.
pub(super) struct StaticBuiltin {
    pub(super) canonical_name: &'static str,
    pub(super) params: &'static [ValType],
    pub(super) results: &'static [ValType],
    pub(super) body: fn(&mut Function) -> Result<(), WasmGcError>,
}

fn build_helper(slot: &BuiltinSlot) -> Result<Function, WasmGcError> {
    let body_fn = lookup_spec(slot.name)
        .ok_or(WasmGcError::Validation(format!(
            "registered builtin `{}` has no spec",
            slot.name
        )))?
        .body;
    // Helpers don't need extra locals at the top level — each spec
    // declares its own via wasm `local` in the body. To keep the
    // shape uniform we let the body-fn add its locals via a
    // pre-emit hook in the future; for now they're all "primitives
    // in, primitive out" and don't need extra locals.
    let mut func = Function::new([]);
    body_fn(&mut func)?;
    Ok(func)
}

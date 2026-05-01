//! Builtin functions emitted as per-module helper fns.
//!
//! ## Strategy
//!
//! Aver's builtin namespace splits two ways:
//!
//! - **Pure builtins** (`Int.toString`, `List.prepend`, `Map.empty`,
//!   `Vector.get`, …) — emitted as local helper fns inside the user's
//!   wasm module on first use. Same pattern rustc uses for stdlib in
//!   its wasm output. No external runtime, no host dependency.
//!   Helpers that aren't reached get DCE'd by `wasm-opt -Oz`.
//!
//! - **Effectful builtins** (`Console.print`, `Http.get`, …) — go
//!   through `(import "aver" "...")` so the host supplies the impl.
//!   Same shape the legacy backend uses for effects, just without
//!   the `aver_runtime.wasm` middleman. Lives in `effects.rs` (TBA).
//!
//! ## String representation
//!
//! `String = (ref null (array i8))` — engine-managed UTF-8 byte
//! sequence. Decision rationale in `../README.md` ("Where builtins
//! live"). Alternatives considered:
//!
//! - **stringref** `(ref string)` — proposal was deprecated in
//!   2024-2025 in favour of JS String Builtins.
//! - **JS String Builtins** (`(import "wasm:js-string" ...)`) —
//!   stage-4 standardized, but requires host cooperation. Wasmtime
//!   doesn't ship it natively (would need our `Linker::func_wrap`
//!   for every string op); browsers and workerd do. Future opt-in
//!   as `aver compile --strings=js-builtins` for browser-only
//!   deployments where the zero-copy JS interop matters.
//! - **Linear memory + `(struct (i32 ptr) (i32 len))`** — works on
//!   any wasm runtime but reintroduces the linear-memory + bump-
//!   allocator complexity we left behind by going to wasm-gc.
//!
//! `(array i8)` is engine-managed (GC handles allocation), runs on
//! any wasm-gc runtime, and matches our "no custom runtime" thesis.
//!
//! ## Lifecycle
//!
//! 1. **Discovery** — `module::emit_module` walks the IR before fn
//!    bodies emit and registers each used dotted-builtin via
//!    `BuiltinRegistry::register`.
//! 2. **Slot allocation** — after user fn types are reserved,
//!    `assign_slots` allocates a wasm fn idx and type idx per
//!    registered builtin.
//! 3. **Call site emit** — `body.rs` looks up the builtin in the
//!    registry and emits `call $idx`.
//! 4. **Helper bodies** — emitted after user fns by
//!    `emit_helper_bodies`, with full access to the `TypeRegistry`
//!    for concrete struct/array type indices.
//!
//! ## Status (phase 3c, in progress)
//!
//! Architecture and registry are wired. The first concrete helper
//! body (`Int.toString`) is the next chunk of work — it's a digit-
//! conversion loop that allocates an `(array i8)` and fills it via
//! `array.new_default` + `array.set` × N. Roughly 50 lines of raw
//! wasm encoding. Until it lands, calls to `Int.toString` (and the
//! other builtins listed in `BuiltinName`) surface a clear "phase
//! 3c body not implemented" error pointing here.

use std::collections::HashMap;

use wasm_encoder::{CodeSection, Function, Instruction, ValType};

use super::WasmGcError;
use super::types::TypeRegistry;

/// Curated set of pure-side builtins phase 3c+ implements. Adding a
/// new builtin: extend this enum + `from_dotted` + `signature` +
/// `emit_helper_body`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum BuiltinName {
    IntToString,
}

impl BuiltinName {
    pub(super) fn from_dotted(s: &str) -> Option<Self> {
        match s {
            "Int.toString" => Some(Self::IntToString),
            _ => None,
        }
    }

    pub(super) fn canonical(self) -> &'static str {
        match self {
            Self::IntToString => "Int.toString",
        }
    }

    pub(super) fn params(self, _registry: &TypeRegistry) -> Result<Vec<ValType>, WasmGcError> {
        match self {
            Self::IntToString => Ok(vec![ValType::I64]),
        }
    }

    pub(super) fn results(self, registry: &TypeRegistry) -> Result<Vec<ValType>, WasmGcError> {
        match self {
            Self::IntToString => Ok(vec![string_ref_ty(registry)?]),
        }
    }

    /// Emit the full helper body (including trailing `End`) into a
    /// fresh `Function`. Called once per registered builtin during
    /// `emit_helper_bodies`.
    pub(super) fn emit_helper_body(
        self,
        _registry: &TypeRegistry,
    ) -> Result<Function, WasmGcError> {
        match self {
            Self::IntToString => emit_int_to_string_stub(),
        }
    }
}

/// Per-module registry of used builtins.
#[derive(Default)]
pub(super) struct BuiltinRegistry {
    /// Insertion order — wasm fn indices and type indices follow it.
    order: Vec<BuiltinName>,
    wasm_fn_idx: HashMap<BuiltinName, u32>,
    wasm_type_idx: HashMap<BuiltinName, u32>,
}

impl BuiltinRegistry {
    pub(super) fn new() -> Self {
        Self::default()
    }

    pub(super) fn register(&mut self, name: BuiltinName) {
        if !self.order.contains(&name) {
            self.order.push(name);
        }
    }

    pub(super) fn iter(&self) -> impl Iterator<Item = BuiltinName> + '_ {
        self.order.iter().copied()
    }

    pub(super) fn assign_slots(&mut self, next_wasm_fn_idx: &mut u32, next_type_idx: &mut u32) {
        for name in self.order.iter().copied() {
            self.wasm_fn_idx.insert(name, *next_wasm_fn_idx);
            self.wasm_type_idx.insert(name, *next_type_idx);
            *next_wasm_fn_idx += 1;
            *next_type_idx += 1;
        }
    }

    pub(super) fn lookup_wasm_fn_idx(&self, name: BuiltinName) -> Option<u32> {
        self.wasm_fn_idx.get(&name).copied()
    }

    pub(super) fn lookup_wasm_type_idx(&self, name: BuiltinName) -> Option<u32> {
        self.wasm_type_idx.get(&name).copied()
    }

    pub(super) fn emit_helper_bodies(
        &self,
        codes: &mut CodeSection,
        registry: &TypeRegistry,
    ) -> Result<(), WasmGcError> {
        for name in self.iter() {
            let func = name.emit_helper_body(registry)?;
            codes.function(&func);
        }
        Ok(())
    }
}

/// `(ref null $string_array)` — shared String repr.
fn string_ref_ty(registry: &TypeRegistry) -> Result<ValType, WasmGcError> {
    let idx = registry
        .string_array_type_idx
        .ok_or(WasmGcError::Validation(
            "builtin requires String repr but no string type slot was allocated".into(),
        ))?;
    Ok(ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(idx),
    }))
}

/// Phase-3c stub for `Int.toString`. Real body — digit-conversion
/// loop building `(array i8)` of ASCII bytes — lands in the next
/// commit. Today: `unreachable` so a program that actually calls
/// `Int.toString` fails fast at runtime with a wasm trap, not an
/// invalid-module error. The fn signature is correct so other
/// scenarios that only TYPE through `Int.toString` (e.g. main
/// returns Unit and the call result is dropped) won't validate-fail.
fn emit_int_to_string_stub() -> Result<Function, WasmGcError> {
    let mut func = Function::new([]);
    func.instruction(&Instruction::Unreachable);
    func.instruction(&Instruction::End);
    Ok(func)
}

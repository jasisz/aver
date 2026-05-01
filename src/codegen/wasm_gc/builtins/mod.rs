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

mod wat_helper;

/// Curated set of pure-side builtins phase 3c+ implements. Adding a
/// new builtin: extend this enum + `from_dotted` + `signature` +
/// `emit_helper_body`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum BuiltinName {
    IntToString,
    StringLength,
    /// Variadic-by-array string concatenation: `(array (ref null $string))
    /// -> (ref null $string)`. Sums the per-part lengths once,
    /// allocates the result, copies each part. O(total_len) regardless
    /// of part count — replaces what would otherwise be an O(N²)
    /// `String.concat` chain. Used by `Expr::InterpolatedStr` (each
    /// interpolation builds a fixed-size `array.new_fixed` of refs
    /// over its parts and calls this) and the future `String.join`
    /// shape (interleave separator, then call this).
    StringConcatN,
}

impl BuiltinName {
    pub(super) fn from_dotted(s: &str) -> Option<Self> {
        match s {
            "Int.toString" => Some(Self::IntToString),
            "String.len" => Some(Self::StringLength),
            _ => None,
        }
    }

    /// Builtins whose surface name is internal to the wasm-gc backend
    /// (not addressable from Aver source). These get registered
    /// explicitly when the codegen emit path needs them — currently
    /// `Expr::InterpolatedStr` registers `StringConcatN`.
    pub(super) fn internal_canonical(self) -> &'static str {
        match self {
            Self::StringConcatN => "__wasmgc_concat_n",
            _ => self.canonical(),
        }
    }

    pub(super) fn canonical(self) -> &'static str {
        match self {
            Self::IntToString => "Int.toString",
            Self::StringLength => "String.len",
            Self::StringConcatN => "__wasmgc_concat_n",
        }
    }

    pub(super) fn params(self, registry: &TypeRegistry) -> Result<Vec<ValType>, WasmGcError> {
        match self {
            Self::IntToString => Ok(vec![ValType::I64]),
            Self::StringLength => Ok(vec![string_ref_ty(registry)?]),
            Self::StringConcatN => Ok(vec![string_array_ref_ty(registry)?]),
        }
    }

    pub(super) fn results(self, registry: &TypeRegistry) -> Result<Vec<ValType>, WasmGcError> {
        match self {
            Self::IntToString => Ok(vec![string_ref_ty(registry)?]),
            Self::StringLength => Ok(vec![ValType::I64]),
            Self::StringConcatN => Ok(vec![string_ref_ty(registry)?]),
        }
    }

    /// Emit the full helper body (including trailing `End`) into a
    /// fresh `Function`. Called once per registered builtin during
    /// `emit_helper_bodies`.
    pub(super) fn emit_helper_body(self, registry: &TypeRegistry) -> Result<Function, WasmGcError> {
        match self {
            Self::IntToString => emit_int_to_string(registry),
            Self::StringLength => emit_string_length(registry),
            Self::StringConcatN => emit_string_concat_n(registry),
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

/// `(ref null (array (ref null $string)))` — the Vector<String> shape
/// the variadic concat helper consumes. Reuses the registry's
/// monomorphised `Vector<String>` slot (registered by
/// `TypeRegistry::build` whenever an `InterpolatedStr` is reachable).
fn string_array_ref_ty(registry: &TypeRegistry) -> Result<ValType, WasmGcError> {
    let idx = registry
        .vector_type_idx("Vector<String>")
        .ok_or(WasmGcError::Validation(
            "concat-N helper requires Vector<String> slot but it wasn't registered".into(),
        ))?;
    Ok(ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(idx),
    }))
}

/// `Int.toString(n: i64) -> (ref null $string)`. Digit-conversion
/// loop that allocates an `(array i8)` and fills it with ASCII bytes.
///
/// Same `wat`-source-of-truth pattern the legacy backend uses for
/// `aver_runtime.wasm` and the `aver_to_wasi` shim — readable text,
/// `wat::parse_str` to binary at codegen time. The wrinkle here:
/// helpers go *into* the user module, so the WAT-helper's String
/// type idx must match the user module's. We pad the helper's type
/// section with empty struct types until String lands at the same
/// index, then `wat_helper::compile_wat_helper` extracts the body
/// and splices it in.
///
/// Algorithm:
///   1. Special-case `n == 0` → 1-byte array containing `'0'`.
///   2. Otherwise: stash sign, work on absolute value.
///   3. Count digits via `/= 10` loop.
///   4. Allocate `array.new_default $string` of length `digits + neg`.
///   5. Fill from right: `arr[i] = '0' + (n % 10)`, then `n /= 10`,
///      then `i -= 1` until `i < neg`.
///   6. If negative, write `'-'` at position 0.
///
/// Source-of-truth is the WAT below — wasm-encoder transcription is
/// auto-generated by `wat_helper::compile_wat_helper`. Type idx 0 in
/// the WAT module corresponds to `(array i8)`, the only type the
/// helper module declares. The function body bytes get spliced into
/// the user module via `Function::raw`; locals declarations carry
/// across; type idx 0 inside the body is preserved (the user module's
/// String type idx must match — `BuiltinName::register` is called
/// only after `TypeRegistry` allocates the String slot, and the WAT
/// is hardcoded to use idx 0).
///
/// Wait — that's not actually true. The user module's String slot
/// is at `registry.string_array_type_idx`, which is whatever
/// position the type-section emit assigns. Phase 3c (3/N) will
/// rewrite the helper body's type indices on splice. For now we
/// assume the WAT helper's slot 0 == user module's String slot
/// only when no other user types come before it. Bench scenarios
/// that touch String mostly don't define records before; this
/// works for those. Multi-type modules with String need the
/// rewrite step.
fn emit_int_to_string(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let string_idx = registry
        .string_array_type_idx
        .ok_or(WasmGcError::Validation(
            "Int.toString helper requires String slot to be allocated".into(),
        ))?;
    let padding = wat_helper::padding_types(string_idx);
    let wat = format!(
        r#"
        (module
          {padding}
          (type $string (array (mut i8)))
          (func (export "helper") (param $n i64) (result (ref null $string))
            (local $abs_n i64)
            (local $copy i64)
            (local $digit_count i32)
            (local $total_len i32)
            (local $i i32)
            (local $arr (ref null $string))
            (local $neg i32)

            ;; Fast path: n == 0 → ['0']
            local.get $n
            i64.eqz
            (if (result (ref null $string))
              (then
                i32.const 48 ;; '0'
                i32.const 1
                array.new $string)
              (else
                ;; neg = (n < 0)
                local.get $n
                i64.const 0
                i64.lt_s
                local.tee $neg

                (if
                  (then
                    ;; abs_n = 0 - n
                    i64.const 0
                    local.get $n
                    i64.sub
                    local.set $abs_n)
                  (else
                    local.get $n
                    local.set $abs_n))

                ;; Count digits.
                local.get $abs_n
                local.set $copy
                i32.const 0
                local.set $digit_count
                (block $count_done
                  (loop $count
                    local.get $copy
                    i64.eqz
                    br_if $count_done
                    local.get $digit_count
                    i32.const 1
                    i32.add
                    local.set $digit_count
                    local.get $copy
                    i64.const 10
                    i64.div_s
                    local.set $copy
                    br $count))

                ;; total_len = digit_count + neg
                local.get $digit_count
                local.get $neg
                i32.add
                local.set $total_len

                ;; Allocate array.new_default $string (size:i32) -> ref
                local.get $total_len
                array.new_default $string
                local.set $arr

                ;; i = total_len - 1; copy = abs_n
                local.get $total_len
                i32.const 1
                i32.sub
                local.set $i
                local.get $abs_n
                local.set $copy

                ;; Fill from right.
                (block $fill_done
                  (loop $fill
                    ;; if i < neg → done
                    local.get $i
                    local.get $neg
                    i32.lt_s
                    br_if $fill_done

                    ;; arr[i] = '0' + (copy % 10)
                    local.get $arr
                    local.get $i
                    local.get $copy
                    i64.const 10
                    i64.rem_s
                    i32.wrap_i64
                    i32.const 48
                    i32.add
                    array.set $string

                    ;; copy /= 10; i -= 1
                    local.get $copy
                    i64.const 10
                    i64.div_s
                    local.set $copy
                    local.get $i
                    i32.const 1
                    i32.sub
                    local.set $i
                    br $fill))

                ;; If neg, write '-' at position 0.
                local.get $neg
                (if
                  (then
                    local.get $arr
                    i32.const 0
                    i32.const 45 ;; '-'
                    array.set $string))

                local.get $arr)))
        )
    "#
    );
    wat_helper::compile_wat_helper(&wat)
}

/// Variadic concat: `(array (ref null $string)) -> (ref null $string)`.
/// Two-pass: sum part lengths, allocate the result, then copy each
/// part into its slot. O(total_len) regardless of part count, vs the
/// O(N²) bytes copied by a left-folded `String.concat` chain.
fn emit_string_concat_n(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let string_idx = registry
        .string_array_type_idx
        .ok_or(WasmGcError::Validation(
            "concat-N helper requires String slot".into(),
        ))?;
    let vec_idx = registry
        .vector_type_idx("Vector<String>")
        .ok_or(WasmGcError::Validation(
            "concat-N helper requires Vector<String> slot".into(),
        ))?;
    if vec_idx <= string_idx {
        return Err(WasmGcError::Validation(format!(
            "concat-N helper expects vector_idx > string_idx (got {vec_idx} vs {string_idx})"
        )));
    }
    let pre_string = wat_helper::padding_types(string_idx);
    let between = wat_helper::padding_types(vec_idx - string_idx - 1);
    let wat = format!(
        r#"
        (module
          {pre_string}
          (type $string (array (mut i8)))
          {between}
          (type $string_array (array (mut (ref null $string))))
          (func (export "helper") (param $arr (ref null $string_array)) (result (ref null $string))
            (local $total i32)
            (local $i i32)
            (local $n i32)
            (local $part (ref null $string))
            (local $part_len i32)
            (local $out (ref null $string))
            (local $dst i32)

            ;; n = arr.len
            local.get $arr
            array.len
            local.set $n

            ;; Sum total length.
            i32.const 0
            local.set $total
            i32.const 0
            local.set $i
            (block $sum_done
              (loop $sum
                local.get $i
                local.get $n
                i32.ge_u
                br_if $sum_done

                local.get $total
                local.get $arr
                local.get $i
                array.get $string_array
                array.len
                i32.add
                local.set $total

                local.get $i
                i32.const 1
                i32.add
                local.set $i
                br $sum))

            ;; Allocate the result.
            local.get $total
            array.new_default $string
            local.set $out

            ;; Copy each part into out[dst..dst+part_len].
            i32.const 0
            local.set $dst
            i32.const 0
            local.set $i
            (block $copy_done
              (loop $copy
                local.get $i
                local.get $n
                i32.ge_u
                br_if $copy_done

                local.get $arr
                local.get $i
                array.get $string_array
                local.set $part

                local.get $part
                array.len
                local.set $part_len

                local.get $out
                local.get $dst
                local.get $part
                i32.const 0
                local.get $part_len
                array.copy $string $string

                local.get $dst
                local.get $part_len
                i32.add
                local.set $dst

                local.get $i
                i32.const 1
                i32.add
                local.set $i
                br $copy))

            local.get $out)
        )
    "#
    );
    wat_helper::compile_wat_helper(&wat)
}

/// `String.length(s) -> Int`. Trivial wrapper over the wasm-gc
/// `array.len` instruction; widened to i64 to match Aver's `Int`.
fn emit_string_length(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let string_idx = registry
        .string_array_type_idx
        .ok_or(WasmGcError::Validation(
            "String.length helper requires String slot to be allocated".into(),
        ))?;
    let padding = wat_helper::padding_types(string_idx);
    let wat = format!(
        r#"
        (module
          {padding}
          (type $string (array (mut i8)))
          (func (export "helper") (param $s (ref null $string)) (result i64)
            local.get $s
            array.len
            i64.extend_i32_u)
        )
    "#
    );
    wat_helper::compile_wat_helper(&wat)
}

//! Builtin functions emitted as per-module helper fns.
//!
//! ## Strategy
//!
//! Aver's builtin namespace splits two ways:
//!
//! - **Pure builtins** (`String.fromInt`, `List.prepend`, `Vector.get`,
//!   …) — emitted as local helper fns inside the user's
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
//! body (`String.fromInt`) is the next chunk of work — it's a digit-
//! conversion loop that allocates an `(array i8)` and fills it via
//! `array.new_default` + `array.set` × N. Roughly 50 lines of raw
//! wasm encoding. Until it lands, calls to `String.fromInt` (and the
//! other builtins listed in `BuiltinName`) surface a clear "phase
//! 3c body not implemented" error pointing here.

use std::collections::HashMap;

use wasm_encoder::{CodeSection, Function, Instruction, ValType};

use super::WasmGcError;
use super::types::TypeRegistry;
use super::wat_helper;

mod bignum;

/// Curated set of pure-side builtins phase 3c+ implements. Adding a
/// new builtin: extend this enum + `from_dotted` + `signature` +
/// `emit_helper_body`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum BuiltinName {
    StringFromInt,
    /// `String.len(s) -> Int` — Unicode scalar value count, matching
    /// the VM's `s.chars().count()` semantics (docs: "number of
    /// characters"). One pass over the UTF-8 byte array counting
    /// non-continuation bytes.
    StringLength,
    /// `String.byteLength(s) -> Int` — UTF-8 byte count (`array.len`).
    /// The VM's `s.len()` counterpart; distinct from `String.len`
    /// whenever the string holds multi-byte characters.
    StringByteLength,
    /// Variadic-by-array string concatenation: `(array (ref null $string))
    /// -> (ref null $string)`. Sums the per-part lengths once,
    /// allocates the result, copies each part. O(total_len) regardless
    /// of part count — replaces what would otherwise be an O(N²)
    /// `String.concat` chain. Used by `Expr::InterpolatedStr` (each
    /// interpolation builds a fixed-size `array.new_fixed` of refs
    /// over its parts and calls this) and the future `String.join`
    /// shape (interleave separator, then call this).
    StringConcatN,
    StringStartsWith,
    StringContains,
    StringSlice,
    StringToUpper,
    StringToLower,
    StringTrim,
    IntFromString,
    FloatFromString,
    StringFromFloat,
    /// Internal byte-equal compare over two `(ref null $string)`.
    /// Powers `match s { "literal" -> ... }` in user code (compiler
    /// emits one `Call(string_eq)` per non-default arm) and any other
    /// String equality the surface didn't already route through Map's
    /// per-K eq helper. Surface name `__wasmgc_string_eq` is internal
    /// (not addressable from Aver source); registry registers it
    /// explicitly when discovery finds a String-subject match.
    StringEq,
    /// `__wasmgc_string_compare(a, b) -> i32` — lexicographic byte
    /// compare. Returns -1 / 0 / 1. Used by String `<` / `>` / `<=`
    /// / `>=` BinOps.
    StringCompare,
    StringEndsWith,
    StringFromBool,
    StringCharAt,
    CharToCode,
    CharFromCode,
    StringChars,
    /// `Byte.fromHex(s) -> Result<Int, String>`. Parses a 2-char hex
    /// string. Validates length + each digit; returns `Result.Ok(byte)`
    /// or `Result.Err("not a hex string")`.
    ByteFromHex,
    /// `Byte.toHex(b) -> Result<String, String>`. Validates `b` is in
    /// `[0, 256)`, returns the 2-char lowercase hex string. Out-of-
    /// range returns `Result.Err("byte out of range")`.
    ByteToHex,
    /// `String.replace(s, needle, repl) -> String`. Two-pass naive
    /// scan: count occurrences, allocate output of exact size, fill.
    /// Empty needle returns `s` unchanged.
    StringReplace,
    /// Internal `__int_mod_euclid(a: i64, b: i64) -> i64` — Euclidean
    /// modulo (always in `[0, |b|)`). Powers `Int.mod` so result has
    /// math-modulo semantics, not Rust `%` truncated remainder.
    /// Caller is responsible for the b == 0 check; the helper assumes
    /// b != 0 and would `i64.rem_s`-trap otherwise.
    IntModEuclid,
    /// Internal `__int_div_euclid(a: i64, b: i64) -> i64` — Euclidean
    /// (flooring) division, the exact partner of `IntModEuclid` so that
    /// `div(a,b)*b + mod(a,b) == a` for every sign. Powers `Int.div`.
    /// Caller guards `b == 0`; the helper assumes `b != 0` (and would
    /// `i64.div_s`-trap on that or the `i64::MIN / -1` overflow).
    IntDivEuclid,
    // ── bignum slice 1 — arbitrary-precision Int helpers ─────────────
    // Each is a self-contained WAT fn over the `$AverInt` struct ref;
    // registered only under the `AVER_WASMGC_BIGNUM` flag. See
    // `bignum.rs` for the representation + semantics.
    /// `__aint_from_i64(i64) -> $AverInt` — canonical Small constructor.
    AintFromI64,
    /// `__aint_add(a, b) -> $AverInt` — ℤ add (never wraps).
    AintAdd,
    /// `__aint_sub(a, b) -> $AverInt` — ℤ sub (never wraps).
    AintSub,
    /// `__aint_mul(a, b) -> $AverInt` — ℤ mul (never wraps); the C0 law
    /// `a*a >= 0` holds even where i64 would wrap.
    AintMul,
    /// `__aint_neg(a) -> $AverInt` — ℤ negate (`-i64::MIN` promotes).
    AintNeg,
    /// `__aint_abs(a) -> $AverInt` — ℤ abs (`|i64::MIN|` promotes). slice 2.
    AintAbs,
    /// `__aint_divmod(a, b, want_mod) -> $AverInt` — Euclidean division
    /// (`want_mod == 0`) or modulo (`want_mod != 0`); remainder always in
    /// `[0, |b|)`. Caller guards `b == 0`. slice 2.
    AintDivmod,
    /// `__aint_cmp(a, b) -> i32` (-1/0/1) — total order over ℤ.
    AintCmp,
    /// `__aint_eq(a, b) -> i32` (1/0) — equality leaning on canonical form.
    AintEq,
    // ── bignum slice 3 — decimal parse / Float bridges / index ─────────
    /// `__aint_to_f64(a) -> f64` — `Float.fromInt` (±inf saturation). slice 3.
    AintToF64,
    /// `__aint_from_f64(f) -> $AverInt` — `Int.fromFloat` (exact Big). slice 3.
    AintFromF64,
    /// `__aint_to_index(a) -> i32` — Vector/List index extraction; Big →
    /// OOB sentinel `-1`. slice 3.
    AintToIndex,
}

impl BuiltinName {
    pub(super) fn from_dotted(s: &str) -> Option<Self> {
        match s {
            "String.fromInt" => Some(Self::StringFromInt),
            "String.fromFloat" => Some(Self::StringFromFloat),
            "String.len" | "String.length" => Some(Self::StringLength),
            "String.byteLength" => Some(Self::StringByteLength),
            "String.startsWith" => Some(Self::StringStartsWith),
            "String.contains" => Some(Self::StringContains),
            "String.slice" => Some(Self::StringSlice),
            "String.toUpper" => Some(Self::StringToUpper),
            "String.toLower" => Some(Self::StringToLower),
            "String.trim" => Some(Self::StringTrim),
            "Int.fromString" => Some(Self::IntFromString),
            "Float.fromString" => Some(Self::FloatFromString),
            "String.endsWith" => Some(Self::StringEndsWith),
            "String.fromBool" => Some(Self::StringFromBool),
            "String.charAt" => Some(Self::StringCharAt),
            "Char.toCode" => Some(Self::CharToCode),
            "Char.fromCode" => Some(Self::CharFromCode),
            "String.chars" => Some(Self::StringChars),
            "Byte.fromHex" => Some(Self::ByteFromHex),
            "Byte.toHex" => Some(Self::ByteToHex),
            "String.replace" => Some(Self::StringReplace),
            _ => None,
        }
    }

    pub(super) fn canonical(self) -> &'static str {
        match self {
            Self::StringFromInt => "String.fromInt",
            Self::StringLength => "String.len",
            Self::StringByteLength => "String.byteLength",
            Self::StringConcatN => "__wasmgc_concat_n",
            Self::StringStartsWith => "String.startsWith",
            Self::StringContains => "String.contains",
            Self::StringSlice => "String.slice",
            Self::StringToUpper => "String.toUpper",
            Self::StringToLower => "String.toLower",
            Self::StringTrim => "String.trim",
            Self::IntFromString => "Int.fromString",
            Self::FloatFromString => "Float.fromString",
            Self::StringFromFloat => "String.fromFloat",
            Self::StringEq => "__wasmgc_string_eq",
            Self::StringCompare => "__wasmgc_string_compare",
            Self::StringEndsWith => "String.endsWith",
            Self::StringFromBool => "String.fromBool",
            Self::StringCharAt => "String.charAt",
            Self::CharToCode => "Char.toCode",
            Self::CharFromCode => "Char.fromCode",
            Self::StringChars => "String.chars",
            Self::ByteFromHex => "Byte.fromHex",
            Self::ByteToHex => "Byte.toHex",
            Self::StringReplace => "String.replace",
            Self::IntModEuclid => "__int_mod_euclid",
            Self::IntDivEuclid => "__int_div_euclid",
            Self::AintFromI64 => "__aint_from_i64",
            Self::AintAdd => "__aint_add",
            Self::AintSub => "__aint_sub",
            Self::AintMul => "__aint_mul",
            Self::AintNeg => "__aint_neg",
            Self::AintAbs => "__aint_abs",
            Self::AintDivmod => "__aint_divmod",
            Self::AintCmp => "__aint_cmp",
            Self::AintEq => "__aint_eq",
            Self::AintToF64 => "__aint_to_f64",
            Self::AintFromF64 => "__aint_from_f64",
            Self::AintToIndex => "__aint_to_index",
        }
    }

    pub(super) fn params(self, registry: &TypeRegistry) -> Result<Vec<ValType>, WasmGcError> {
        match self {
            // bignum slice 1 — `String.fromInt` takes the `$AverInt` ref
            // (and formats Big via divmod-by-10) when bignum is active;
            // otherwise the scalar i64.
            Self::StringFromInt if registry.bignum => Ok(vec![aint_ref_ty(registry)?]),
            Self::StringFromInt => Ok(vec![ValType::I64]),
            Self::StringLength | Self::StringByteLength => Ok(vec![string_ref_ty(registry)?]),
            Self::StringConcatN => Ok(vec![string_array_ref_ty(registry)?]),
            Self::StringStartsWith | Self::StringContains => {
                Ok(vec![string_ref_ty(registry)?, string_ref_ty(registry)?])
            }
            Self::StringSlice => Ok(vec![string_ref_ty(registry)?, ValType::I64, ValType::I64]),
            Self::StringToUpper | Self::StringToLower | Self::StringTrim => {
                Ok(vec![string_ref_ty(registry)?])
            }
            Self::IntFromString => Ok(vec![string_ref_ty(registry)?]),
            Self::FloatFromString => Ok(vec![string_ref_ty(registry)?]),
            Self::StringFromFloat => Ok(vec![ValType::F64]),
            Self::StringEq => Ok(vec![string_ref_ty(registry)?, string_ref_ty(registry)?]),
            Self::StringCompare => Ok(vec![string_ref_ty(registry)?, string_ref_ty(registry)?]),
            Self::StringEndsWith => Ok(vec![string_ref_ty(registry)?, string_ref_ty(registry)?]),
            Self::StringFromBool => Ok(vec![ValType::I32]),
            Self::StringCharAt => Ok(vec![string_ref_ty(registry)?, ValType::I64]),
            Self::CharToCode => Ok(vec![string_ref_ty(registry)?]),
            Self::CharFromCode => Ok(vec![ValType::I64]),
            Self::StringChars => Ok(vec![string_ref_ty(registry)?]),
            Self::ByteFromHex => Ok(vec![string_ref_ty(registry)?]),
            Self::ByteToHex => Ok(vec![ValType::I64]),
            Self::StringReplace => Ok(vec![
                string_ref_ty(registry)?,
                string_ref_ty(registry)?,
                string_ref_ty(registry)?,
            ]),
            Self::IntModEuclid | Self::IntDivEuclid => Ok(vec![ValType::I64, ValType::I64]),
            Self::AintFromI64 => Ok(vec![ValType::I64]),
            Self::AintAdd | Self::AintSub | Self::AintMul | Self::AintCmp | Self::AintEq => {
                Ok(vec![aint_ref_ty(registry)?, aint_ref_ty(registry)?])
            }
            Self::AintNeg | Self::AintAbs => Ok(vec![aint_ref_ty(registry)?]),
            // `want_mod` selects modulo (≠0) vs division (0).
            Self::AintDivmod => Ok(vec![
                aint_ref_ty(registry)?,
                aint_ref_ty(registry)?,
                ValType::I32,
            ]),
            Self::AintToF64 | Self::AintToIndex => Ok(vec![aint_ref_ty(registry)?]),
            Self::AintFromF64 => Ok(vec![ValType::F64]),
        }
    }

    pub(super) fn results(self, registry: &TypeRegistry) -> Result<Vec<ValType>, WasmGcError> {
        match self {
            Self::StringFromInt => Ok(vec![string_ref_ty(registry)?]),
            Self::StringLength | Self::StringByteLength => Ok(vec![ValType::I64]),
            Self::StringConcatN => Ok(vec![string_ref_ty(registry)?]),
            Self::StringStartsWith | Self::StringContains => Ok(vec![ValType::I32]),
            Self::StringSlice
            | Self::StringToUpper
            | Self::StringToLower
            | Self::StringTrim
            | Self::StringFromFloat => Ok(vec![string_ref_ty(registry)?]),
            Self::IntFromString => Ok(vec![result_ref_ty(registry, "Result<Int,String>")?]),
            Self::FloatFromString => Ok(vec![result_ref_ty(registry, "Result<Float,String>")?]),
            Self::StringEq => Ok(vec![ValType::I32]),
            Self::StringCompare => Ok(vec![ValType::I32]),
            Self::StringFromBool => Ok(vec![string_ref_ty(registry)?]),
            Self::StringEndsWith => Ok(vec![ValType::I32]),
            Self::CharToCode => Ok(vec![ValType::I64]),
            Self::StringCharAt | Self::CharFromCode => {
                Ok(vec![option_ref_ty(registry, "Option<String>")?])
            }
            Self::StringChars => Ok(vec![list_ref_ty(registry, "List<String>")?]),
            Self::ByteFromHex => Ok(vec![result_ref_ty(registry, "Result<Int,String>")?]),
            Self::ByteToHex => Ok(vec![result_ref_ty(registry, "Result<String,String>")?]),
            Self::StringReplace => Ok(vec![string_ref_ty(registry)?]),
            Self::IntModEuclid | Self::IntDivEuclid => Ok(vec![ValType::I64]),
            Self::AintFromI64
            | Self::AintAdd
            | Self::AintSub
            | Self::AintMul
            | Self::AintNeg
            | Self::AintAbs
            | Self::AintDivmod
            | Self::AintFromF64 => Ok(vec![aint_ref_ty(registry)?]),
            Self::AintCmp | Self::AintEq | Self::AintToIndex => Ok(vec![ValType::I32]),
            Self::AintToF64 => Ok(vec![ValType::F64]),
        }
    }

    /// Emit the full helper body (including trailing `End`) into a
    /// fresh `Function`. Called once per registered builtin during
    /// `emit_helper_bodies`.
    pub(super) fn emit_helper_body(self, registry: &TypeRegistry) -> Result<Function, WasmGcError> {
        match self {
            Self::StringFromInt if registry.bignum => bignum::emit_string_from_aint(registry),
            Self::StringFromInt => emit_string_from_int(registry),
            Self::StringLength => emit_string_length(registry),
            Self::StringByteLength => emit_string_byte_length(registry),
            Self::StringConcatN => emit_string_concat_n(registry),
            Self::StringStartsWith => emit_string_starts_with(registry),
            Self::StringContains => emit_string_contains(registry),
            Self::StringSlice => emit_string_slice(registry),
            Self::StringToUpper => emit_string_case(registry, true),
            Self::StringToLower => emit_string_case(registry, false),
            Self::StringTrim => emit_string_trim(registry),
            Self::IntFromString if registry.bignum => bignum::emit_aint_from_string(registry),
            Self::IntFromString => emit_int_from_string(registry),
            Self::FloatFromString => emit_float_from_string(registry),
            Self::StringFromFloat => emit_string_from_float(registry),
            Self::StringEq => emit_string_eq(registry),
            Self::StringCompare => emit_string_compare(registry),
            Self::StringEndsWith => emit_string_ends_with(registry),
            Self::StringFromBool => emit_string_from_bool(registry),
            Self::StringCharAt => emit_string_char_at(registry),
            Self::CharToCode => emit_char_to_code(registry),
            Self::CharFromCode => emit_char_from_code(registry),
            Self::StringChars => emit_string_chars(registry),
            Self::ByteFromHex => emit_byte_from_hex(registry),
            Self::ByteToHex => emit_byte_to_hex(registry),
            Self::StringReplace => emit_string_replace(registry),
            Self::IntModEuclid => emit_int_mod_euclid(),
            Self::IntDivEuclid => emit_int_div_euclid(),
            Self::AintFromI64 => bignum::emit_aint_from_i64(registry),
            Self::AintAdd => bignum::emit_aint_add(registry),
            Self::AintSub => bignum::emit_aint_sub(registry),
            Self::AintMul => bignum::emit_aint_mul(registry),
            Self::AintNeg => bignum::emit_aint_neg(registry),
            Self::AintAbs => bignum::emit_aint_abs(registry),
            Self::AintDivmod => bignum::emit_aint_divmod(registry),
            Self::AintCmp => bignum::emit_aint_cmp(registry),
            Self::AintEq => bignum::emit_aint_eq(registry),
            Self::AintToF64 => bignum::emit_aint_to_f64(registry),
            Self::AintFromF64 => bignum::emit_aint_from_f64(registry),
            Self::AintToIndex => bignum::emit_aint_to_index(registry),
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

/// `(ref null $AverInt)` — bignum slice 1 carrier ref.
fn aint_ref_ty(registry: &TypeRegistry) -> Result<ValType, WasmGcError> {
    super::types::aint_ref_ty(registry)
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

/// `(ref null $option_T)` — Option instantiation reference. Canonical
/// is spaceless (e.g. `Option<String>`). Used by `String.charAt` and
/// `Char.fromCode` which both return `Option<String>`.
fn option_ref_ty(registry: &TypeRegistry, canonical: &str) -> Result<ValType, WasmGcError> {
    let idx = registry
        .option_type_idx(canonical)
        .ok_or(WasmGcError::Validation(format!(
            "builtin requires `{canonical}` slot but it wasn't registered"
        )))?;
    Ok(ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(idx),
    }))
}

/// `(ref null $list_T)` — List instantiation reference.
fn list_ref_ty(registry: &TypeRegistry, canonical: &str) -> Result<ValType, WasmGcError> {
    let idx = registry
        .list_type_idx(canonical)
        .ok_or(WasmGcError::Validation(format!(
            "builtin requires `{canonical}` slot but it wasn't registered"
        )))?;
    Ok(ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(idx),
    }))
}

/// `(ref null $result_T_E)` — Result instantiation reference. The
/// canonical comes spaceless (e.g. `Result<Int,String>`).
fn result_ref_ty(registry: &TypeRegistry, canonical: &str) -> Result<ValType, WasmGcError> {
    let idx = registry
        .result_type_idx(canonical)
        .ok_or(WasmGcError::Validation(format!(
            "builtin requires `{canonical}` slot but it wasn't registered"
        )))?;
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

/// `String.fromInt(n: i64) -> (ref null $string)`. Digit-conversion
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
fn emit_string_from_int(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let string_idx = registry
        .string_array_type_idx
        .ok_or(WasmGcError::Validation(
            "String.fromInt helper requires String slot to be allocated".into(),
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

/// `String.len(s) -> Int` — Unicode scalar value count, matching the
/// VM's `s.chars().count()`. Strings are stored as UTF-8 `(array i8)`,
/// so the scalar count equals the number of non-continuation bytes:
/// one pass adding `(b & 0xC0) != 0x80` per byte. O(len) instead of
/// the old O(1) `array.len` — the price of matching the documented
/// "number of characters" semantics (`array.len` counted bytes, which
/// diverged from the VM on any multi-byte character).
fn emit_string_length(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let string_idx = registry
        .string_array_type_idx
        .ok_or(WasmGcError::Validation(
            "String.len helper requires String slot to be allocated".into(),
        ))?;
    let padding = wat_helper::padding_types(string_idx);
    let wat = format!(
        r#"
        (module
          {padding}
          (type $string (array (mut i8)))
          (func (export "helper") (param $s (ref null $string)) (result i64)
            (local $i i32) (local $n i32) (local $count i32)
            local.get $s
            array.len
            local.set $n
            (block $done
              (loop $scan
                local.get $i
                local.get $n
                i32.ge_u
                br_if $done

                ;; count += (s[i] & 0xC0) != 0x80
                local.get $count
                local.get $s
                local.get $i
                array.get_u $string
                i32.const 0xC0
                i32.and
                i32.const 0x80
                i32.ne
                i32.add
                local.set $count

                local.get $i
                i32.const 1
                i32.add
                local.set $i
                br $scan))
            local.get $count
            i64.extend_i32_u)
        )
    "#
    );
    wat_helper::compile_wat_helper(&wat)
}

/// `String.byteLength(s) -> Int`. Trivial wrapper over the wasm-gc
/// `array.len` instruction; widened to i64 to match Aver's `Int`.
fn emit_string_byte_length(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let string_idx = registry
        .string_array_type_idx
        .ok_or(WasmGcError::Validation(
            "String.byteLength helper requires String slot to be allocated".into(),
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

/// Helper preamble: declare `$string` at the user module's index by
/// padding the WAT type section with empty struct types.
fn string_module_preamble(registry: &TypeRegistry) -> Result<(u32, String), WasmGcError> {
    let string_idx = registry
        .string_array_type_idx
        .ok_or(WasmGcError::Validation(
            "helper requires String slot to be allocated".into(),
        ))?;
    let padding = wat_helper::padding_types(string_idx);
    let preamble = format!("{padding}(type $string (array (mut i8)))\n");
    Ok((string_idx, preamble))
}

/// Pad the WAT type section so that both `$string` and a Result<T,E>
/// land at their user-module indices. Result types follow the user
/// module's String slot (and any other types — list, vector, option,
/// other results — that come between in the registry order).
fn string_and_result_preamble(
    registry: &TypeRegistry,
    canonical: &str,
    ok_field: &str,
    err_field: &str,
) -> Result<String, WasmGcError> {
    let string_idx = registry
        .string_array_type_idx
        .ok_or(WasmGcError::Validation(
            "helper requires String slot to be allocated".into(),
        ))?;
    let result_idx = registry
        .result_type_idx(canonical)
        .ok_or(WasmGcError::Validation(format!(
            "helper requires `{canonical}` slot to be allocated"
        )))?;
    if result_idx <= string_idx {
        return Err(WasmGcError::Validation(format!(
            "helper expects result idx {result_idx} > string idx {string_idx}"
        )));
    }
    let pre_string = wat_helper::padding_types(string_idx);
    let between = wat_helper::padding_types(result_idx - string_idx - 1);
    Ok(format!(
        "{pre_string}(type $string (array (mut i8)))\n{between}(type $result (struct (field (mut i32)) (field (mut {ok_field})) (field (mut {err_field}))))\n"
    ))
}

/// `String.startsWith(s, prefix) -> Bool`. ASCII byte-wise; mirrors
/// the legacy `rt_str_starts_with` shape but skips the generic find
/// loop — startsWith is just a bounded byte-equal compare.
fn emit_string_starts_with(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let (_, preamble) = string_module_preamble(registry)?;
    let wat = format!(
        r#"
        (module
          {preamble}
          (func (export "helper")
                (param $s (ref null $string))
                (param $p (ref null $string))
                (result i32)
            (local $slen i32)
            (local $plen i32)
            (local $i i32)

            local.get $s array.len local.set $slen
            local.get $p array.len local.set $plen

            ;; prefix longer than s → false
            local.get $plen
            local.get $slen
            i32.gt_u
            (if (then i32.const 0 return))

            i32.const 0 local.set $i
            (block $done
              (loop $cmp
                local.get $i
                local.get $plen
                i32.ge_u
                br_if $done

                local.get $s
                local.get $i
                array.get_u $string

                local.get $p
                local.get $i
                array.get_u $string

                i32.ne
                (if (then i32.const 0 return))

                local.get $i
                i32.const 1
                i32.add
                local.set $i
                br $cmp))
            i32.const 1)
        )
    "#
    );
    wat_helper::compile_wat_helper(&wat)
}

/// `String.contains(s, needle) -> Bool`. Naive O(s_len * needle_len)
/// scan — sufficient for fractal use (small needles like `"&"`,
/// `"="`). Inner loop bails on first mismatch.
fn emit_string_contains(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let (_, preamble) = string_module_preamble(registry)?;
    let wat = format!(
        r#"
        (module
          {preamble}
          (func (export "helper")
                (param $s (ref null $string))
                (param $n (ref null $string))
                (result i32)
            (local $slen i32)
            (local $nlen i32)
            (local $limit i32)
            (local $pos i32)
            (local $i i32)

            local.get $s array.len local.set $slen
            local.get $n array.len local.set $nlen

            ;; empty needle → true
            local.get $nlen
            i32.eqz
            (if (then i32.const 1 return))

            ;; needle longer than s → false
            local.get $nlen
            local.get $slen
            i32.gt_u
            (if (then i32.const 0 return))

            ;; limit = slen - nlen
            local.get $slen
            local.get $nlen
            i32.sub
            local.set $limit

            i32.const 0 local.set $pos
            (block $outer_done
              (loop $outer
                local.get $pos
                local.get $limit
                i32.gt_u
                br_if $outer_done

                i32.const 0 local.set $i
                (block $inner_done
                  (loop $inner
                    local.get $i
                    local.get $nlen
                    i32.ge_u
                    (if (then i32.const 1 return))

                    local.get $s
                    local.get $pos
                    local.get $i
                    i32.add
                    array.get_u $string

                    local.get $n
                    local.get $i
                    array.get_u $string

                    i32.ne
                    br_if $inner_done

                    local.get $i
                    i32.const 1
                    i32.add
                    local.set $i
                    br $inner))

                local.get $pos
                i32.const 1
                i32.add
                local.set $pos
                br $outer))
            i32.const 0)
        )
    "#
    );
    wat_helper::compile_wat_helper(&wat)
}

/// `String.slice(s, start, end) -> String`. `start` / `end` are
/// Unicode scalar indices, mirroring the VM's `aver_rt::string_slice`
/// (negative ends clamp to 0; `start >= end` is empty; indices past
/// the last character clamp to the end of the string). One pass over
/// the UTF-8 byte array translates both scalar indices to byte
/// offsets, then a single `array.copy` extracts the range. The old
/// body treated the indices as byte offsets, which diverged from the
/// VM on any multi-byte character and could slice characters in half.
fn emit_string_slice(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let (_, preamble) = string_module_preamble(registry)?;
    let wat = format!(
        r#"
        (module
          {preamble}
          (func (export "helper")
                (param $s (ref null $string))
                (param $start64 i64)
                (param $end64 i64)
                (result (ref null $string))
            (local $n i32)
            (local $start i64)
            (local $end i64)
            (local $b i32)
            (local $k i64)
            (local $start_byte i32)
            (local $end_byte i32)
            (local $len i32)
            (local $out (ref null $string))

            local.get $s array.len local.set $n

            ;; start = max(start64, 0); end = max(end64, 0)
            local.get $start64
            i64.const 0
            i64.lt_s
            (if (result i64) (then i64.const 0) (else local.get $start64))
            local.set $start
            local.get $end64
            i64.const 0
            i64.lt_s
            (if (result i64) (then i64.const 0) (else local.get $end64))
            local.set $end

            ;; start >= end → ""
            local.get $start
            local.get $end
            i64.ge_s
            (if (then i32.const 0 array.new_default $string return))

            ;; walk scalars; -1 = byte offset not found yet
            i32.const -1 local.set $start_byte
            i32.const -1 local.set $end_byte
            (block $done
              (loop $scan
                local.get $b
                local.get $n
                i32.ge_u
                br_if $done

                ;; k == start → start_byte = b
                local.get $k
                local.get $start
                i64.eq
                (if (then local.get $b local.set $start_byte))

                ;; k == end → end_byte = b, stop
                local.get $k
                local.get $end
                i64.eq
                (if (then local.get $b local.set $end_byte br $done))

                ;; b += UTF-8 length of the scalar starting at b
                ;; (<0xC0 → 1, <0xE0 → 2, <0xF0 → 3, else 4)
                local.get $b i32.const 1 i32.add local.set $len
                local.get $s local.get $b array.get_u $string
                i32.const 0xC0 i32.ge_u
                (if (then local.get $b i32.const 2 i32.add local.set $len))
                local.get $s local.get $b array.get_u $string
                i32.const 0xE0 i32.ge_u
                (if (then local.get $b i32.const 3 i32.add local.set $len))
                local.get $s local.get $b array.get_u $string
                i32.const 0xF0 i32.ge_u
                (if (then local.get $b i32.const 4 i32.add local.set $len))
                local.get $len local.set $b

                local.get $k i64.const 1 i64.add local.set $k
                br $scan))

            ;; scalar index exactly at the end of the string
            local.get $start_byte i32.const -1 i32.eq
            local.get $k local.get $start i64.eq
            i32.and
            (if (then local.get $n local.set $start_byte))
            local.get $end_byte i32.const -1 i32.eq
            local.get $k local.get $end i64.eq
            i32.and
            (if (then local.get $n local.set $end_byte))

            ;; unresolved (index past the end) → clamp to byte length
            local.get $start_byte i32.const -1 i32.eq
            (if (then local.get $n local.set $start_byte))
            local.get $end_byte i32.const -1 i32.eq
            (if (then local.get $n local.set $end_byte))

            ;; start_byte >= end_byte → ""
            local.get $start_byte
            local.get $end_byte
            i32.ge_s
            (if (then i32.const 0 array.new_default $string return))

            local.get $end_byte local.get $start_byte i32.sub local.set $len
            local.get $len
            array.new_default $string
            local.set $out
            local.get $out
            i32.const 0
            local.get $s
            local.get $start_byte
            local.get $len
            array.copy $string $string

            local.get $out)
        )
    "#
    );
    wat_helper::compile_wat_helper(&wat)
}

/// `String.toUpper` / `String.toLower`. ASCII-only. `to_upper=true`
/// shifts `'a'..'z'` down by 32; otherwise shifts `'A'..'Z'` up.
fn emit_string_case(registry: &TypeRegistry, to_upper: bool) -> Result<Function, WasmGcError> {
    let (_, preamble) = string_module_preamble(registry)?;
    let (lo, hi, delta) = if to_upper {
        ("0x61", "0x7A", "i32.const 32 i32.sub")
    } else {
        ("0x41", "0x5A", "i32.const 32 i32.add")
    };
    let wat = format!(
        r#"
        (module
          {preamble}
          (func (export "helper")
                (param $s (ref null $string))
                (result (ref null $string))
            (local $len i32)
            (local $i i32)
            (local $ch i32)
            (local $out (ref null $string))

            local.get $s array.len local.set $len
            local.get $len array.new_default $string local.set $out

            i32.const 0 local.set $i
            (block $done
              (loop $cp
                local.get $i
                local.get $len
                i32.ge_u
                br_if $done

                local.get $s
                local.get $i
                array.get_u $string
                local.set $ch

                local.get $ch
                i32.const {lo}
                i32.ge_u
                local.get $ch
                i32.const {hi}
                i32.le_u
                i32.and
                (if
                  (then
                    local.get $ch
                    {delta}
                    local.set $ch))

                local.get $out
                local.get $i
                local.get $ch
                array.set $string

                local.get $i
                i32.const 1
                i32.add
                local.set $i
                br $cp))
            local.get $out)
        )
    "#
    );
    wat_helper::compile_wat_helper(&wat)
}

/// `String.trim`. Trims ASCII whitespace (space, tab, LF, CR) from
/// both ends; allocates a fresh string sized to the inner slice.
fn emit_string_trim(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let (_, preamble) = string_module_preamble(registry)?;
    let wat = format!(
        r#"
        (module
          {preamble}
          (func (export "helper")
                (param $s (ref null $string))
                (result (ref null $string))
            (local $len i32)
            (local $start i32)
            (local $end i32)
            (local $ch i32)
            (local $new_len i32)
            (local $out (ref null $string))

            local.get $s array.len local.set $len

            i32.const 0 local.set $start
            (block $sd
              (loop $st
                local.get $start
                local.get $len
                i32.ge_u
                br_if $sd

                local.get $s
                local.get $start
                array.get_u $string
                local.set $ch

                local.get $ch i32.const 0x20 i32.eq
                local.get $ch i32.const 0x09 i32.eq i32.or
                local.get $ch i32.const 0x0A i32.eq i32.or
                local.get $ch i32.const 0x0D i32.eq i32.or
                i32.eqz
                br_if $sd

                local.get $start
                i32.const 1
                i32.add
                local.set $start
                br $st))

            local.get $len local.set $end
            (block $ed
              (loop $et
                local.get $end
                local.get $start
                i32.le_u
                br_if $ed

                local.get $s
                local.get $end
                i32.const 1
                i32.sub
                array.get_u $string
                local.set $ch

                local.get $ch i32.const 0x20 i32.eq
                local.get $ch i32.const 0x09 i32.eq i32.or
                local.get $ch i32.const 0x0A i32.eq i32.or
                local.get $ch i32.const 0x0D i32.eq i32.or
                i32.eqz
                br_if $ed

                local.get $end
                i32.const 1
                i32.sub
                local.set $end
                br $et))

            local.get $end
            local.get $start
            i32.sub
            local.set $new_len

            local.get $new_len
            array.new_default $string
            local.set $out

            local.get $new_len
            i32.eqz
            (if (then local.get $out return))

            local.get $out
            i32.const 0
            local.get $s
            local.get $start
            local.get $new_len
            array.copy $string $string

            local.get $out)
        )
    "#
    );
    wat_helper::compile_wat_helper(&wat)
}

/// `Int.fromString(s) -> Result<Int, String>`. Parses optional `-`
/// followed by ASCII digits. Empty / non-digit input → `Result.Err(s)`.
/// Result struct field layout: `(mut i32 tag) (mut i64 ok) (mut $string err)`,
/// tag 1 = Ok, 0 = Err.
fn emit_int_from_string(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let preamble =
        string_and_result_preamble(registry, "Result<Int,String>", "i64", "(ref null $string)")?;
    let wat = format!(
        r#"
        (module
          {preamble}
          (func (export "helper")
                (param $s (ref null $string))
                (result (ref null $result))
            (local $len i32)
            (local $idx i32)
            (local $negative i32)
            (local $value i64)
            (local $ch i32)
            (local $saw_digit i32)

            local.get $s array.len local.set $len

            ;; Empty input → Err(s)
            local.get $len
            i32.eqz
            (if
              (then
                i32.const 0
                i64.const 0
                local.get $s
                struct.new $result
                return))

            ;; Optional leading '-'
            local.get $s
            i32.const 0
            array.get_u $string
            i32.const 0x2D
            i32.eq
            (if
              (then
                i32.const 1 local.set $negative
                i32.const 1 local.set $idx))

            (block $loop_done
              (loop $loop
                local.get $idx
                local.get $len
                i32.ge_u
                br_if $loop_done

                local.get $s
                local.get $idx
                array.get_u $string
                local.set $ch

                local.get $ch i32.const 0x30 i32.lt_u
                local.get $ch i32.const 0x39 i32.gt_u
                i32.or
                (if
                  (then
                    i32.const 0
                    i64.const 0
                    local.get $s
                    struct.new $result
                    return))

                i32.const 1 local.set $saw_digit

                local.get $value
                i64.const 10
                i64.mul
                local.get $ch
                i32.const 0x30
                i32.sub
                i64.extend_i32_u
                i64.add
                local.set $value

                local.get $idx
                i32.const 1
                i32.add
                local.set $idx
                br $loop))

            ;; Lone "-" → Err
            local.get $saw_digit
            i32.eqz
            (if
              (then
                i32.const 0
                i64.const 0
                local.get $s
                struct.new $result
                return))

            local.get $negative
            (if
              (then
                i64.const 0
                local.get $value
                i64.sub
                local.set $value))

            i32.const 1
            local.get $value
            ref.null $string
            struct.new $result)
        )
    "#
    );
    wat_helper::compile_wat_helper(&wat)
}

/// `Float.fromString(s) -> Result<Float, String>`. Port of the legacy
/// `rt_float_from_str`; same automaton (sign / mantissa / decimal /
/// exponent), bytes from `array.get_u` instead of linear-memory loads.
fn emit_float_from_string(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let preamble = string_and_result_preamble(
        registry,
        "Result<Float,String>",
        "f64",
        "(ref null $string)",
    )?;
    let wat = format!(
        r#"
        (module
          {preamble}
          (func (export "helper")
                (param $s (ref null $string))
                (result (ref null $result))
            (local $len i32)
            (local $idx i32)
            (local $negative i32)
            (local $seen_dot i32)
            (local $saw_digit i32)
            (local $exp_state i32)        ;; 0=none, 1=just-saw-e, 2=after-sign-or-digit
            (local $exp_negative i32)
            (local $saw_exp_digit i32)
            (local $exp_value i32)
            (local $value f64)
            (local $frac_div f64)
            (local $ch i32)
            (local $digit i32)

            local.get $s array.len local.set $len

            f64.const 0 local.set $value
            f64.const 1 local.set $frac_div

            local.get $len
            i32.const 0
            i32.gt_s
            (if
              (then
                local.get $s
                i32.const 0
                array.get_u $string
                i32.const 0x2D
                i32.eq
                (if
                  (then
                    i32.const 1 local.set $negative
                    i32.const 1 local.set $idx))))

            (block $loop_done
              (loop $loop
                local.get $idx
                local.get $len
                i32.ge_u
                br_if $loop_done

                local.get $s
                local.get $idx
                array.get_u $string
                local.set $ch

                ;; Exponent sign just after e/E.
                local.get $exp_state
                i32.const 1
                i32.eq
                local.get $ch
                i32.const 0x2B
                i32.eq
                i32.and
                (if
                  (then
                    i32.const 2 local.set $exp_state
                    local.get $idx i32.const 1 i32.add local.set $idx
                    br $loop))

                local.get $exp_state
                i32.const 1
                i32.eq
                local.get $ch
                i32.const 0x2D
                i32.eq
                i32.and
                (if
                  (then
                    i32.const 1 local.set $exp_negative
                    i32.const 2 local.set $exp_state
                    local.get $idx i32.const 1 i32.add local.set $idx
                    br $loop))

                ;; Inside exponent digits.
                local.get $exp_state
                (if
                  (then
                    local.get $ch i32.const 0x30 i32.lt_u
                    local.get $ch i32.const 0x39 i32.gt_u
                    i32.or
                    (if
                      (then
                        i32.const 0
                        f64.const 0
                        local.get $s
                        struct.new $result
                        return))

                    i32.const 1 local.set $saw_exp_digit
                    i32.const 2 local.set $exp_state

                    local.get $ch
                    i32.const 0x30
                    i32.sub
                    local.set $digit

                    local.get $exp_value
                    i32.const 10
                    i32.mul
                    local.get $digit
                    i32.add
                    local.set $exp_value

                    local.get $idx i32.const 1 i32.add local.set $idx
                    br $loop))

                ;; Decimal point.
                local.get $ch
                i32.const 0x2E
                i32.eq
                (if
                  (then
                    local.get $seen_dot
                    (if
                      (then
                        i32.const 0
                        f64.const 0
                        local.get $s
                        struct.new $result
                        return))
                    i32.const 1 local.set $seen_dot
                    local.get $idx i32.const 1 i32.add local.set $idx
                    br $loop))

                ;; Exponent marker.
                local.get $ch i32.const 0x65 i32.eq
                local.get $ch i32.const 0x45 i32.eq
                i32.or
                (if
                  (then
                    local.get $saw_digit
                    i32.eqz
                    (if
                      (then
                        i32.const 0
                        f64.const 0
                        local.get $s
                        struct.new $result
                        return))
                    i32.const 1 local.set $exp_state
                    local.get $idx i32.const 1 i32.add local.set $idx
                    br $loop))

                ;; Mantissa digit.
                local.get $ch i32.const 0x30 i32.lt_u
                local.get $ch i32.const 0x39 i32.gt_u
                i32.or
                (if
                  (then
                    i32.const 0
                    f64.const 0
                    local.get $s
                    struct.new $result
                    return))

                i32.const 1 local.set $saw_digit

                local.get $ch
                i32.const 0x30
                i32.sub
                local.set $digit

                local.get $seen_dot
                (if
                  (then
                    local.get $frac_div
                    f64.const 10
                    f64.mul
                    local.set $frac_div

                    local.get $value
                    local.get $digit
                    f64.convert_i32_u
                    local.get $frac_div
                    f64.div
                    f64.add
                    local.set $value)
                  (else
                    local.get $value
                    f64.const 10
                    f64.mul
                    local.get $digit
                    f64.convert_i32_u
                    f64.add
                    local.set $value))

                local.get $idx
                i32.const 1
                i32.add
                local.set $idx
                br $loop))

            ;; Reject empty / lone '-'
            local.get $saw_digit
            i32.eqz
            (if
              (then
                i32.const 0
                f64.const 0
                local.get $s
                struct.new $result
                return))

            ;; Dangling exponent marker
            local.get $exp_state
            i32.const 1
            i32.eq
            (if
              (then
                i32.const 0
                f64.const 0
                local.get $s
                struct.new $result
                return))

            ;; Exponent sign with no digits
            local.get $exp_state
            i32.const 2
            i32.eq
            local.get $saw_exp_digit
            i32.eqz
            i32.and
            (if
              (then
                i32.const 0
                f64.const 0
                local.get $s
                struct.new $result
                return))

            ;; Apply exponent.
            (block $exp_done
              (loop $exp
                local.get $exp_value
                i32.eqz
                br_if $exp_done

                local.get $exp_negative
                (if
                  (then
                    local.get $value
                    f64.const 10
                    f64.div
                    local.set $value)
                  (else
                    local.get $value
                    f64.const 10
                    f64.mul
                    local.set $value))

                local.get $exp_value
                i32.const 1
                i32.sub
                local.set $exp_value
                br $exp))

            local.get $negative
            (if
              (then
                f64.const 0
                local.get $value
                f64.sub
                local.set $value))

            i32.const 1
            local.get $value
            ref.null $string
            struct.new $result)
        )
    "#
    );
    wat_helper::compile_wat_helper(&wat)
}

/// `String.fromFloat(f) -> String`. Shortest-roundtrip f64 → ASCII;
/// port of the legacy `rt_float_to_str` algorithm but writing into a
/// 32-byte scratch `(array i8)` instead of linear memory, then
/// `array.copy` into a result string sized to the actual output.
fn emit_string_from_float(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let (_, preamble) = string_module_preamble(registry)?;
    let wat = format!(
        r#"
        (module
          {preamble}
          (func (export "helper")
                (param $val f64)
                (result (ref null $string))
            (local $is_neg i32)
            (local $abs_val f64)
            (local $int_part i64)
            (local $pos i32)
            (local $start_pos i32)
            (local $pow f64)
            (local $n i32)
            (local $scaled i64)
            (local $frac_int i64)
            (local $frac_pos i32)
            (local $frac_digits i32)
            (local $end_pos i32)
            (local $buf (ref null $string))
            (local $out_len i32)
            (local $out (ref null $string))

            ;; 64-byte scratch buffer; integer part right-to-left at [0..21],
            ;; '.' at 21, fractional digits at [22..22+n].
            i32.const 64
            array.new_default $string
            local.set $buf

            local.get $val
            f64.const 0
            f64.lt
            local.set $is_neg

            local.get $val
            f64.abs
            local.set $abs_val

            local.get $abs_val
            f64.floor
            i64.trunc_f64_s
            local.set $int_part

            i32.const 21 local.set $pos

            ;; Integer part right-to-left.
            local.get $int_part
            i64.eqz
            (if
              (then
                local.get $pos i32.const 1 i32.sub local.set $pos
                local.get $buf
                local.get $pos
                i32.const 0x30
                array.set $string)
              (else
                (block $idone
                  (loop $iloop
                    local.get $int_part
                    i64.eqz
                    br_if $idone

                    local.get $pos i32.const 1 i32.sub local.set $pos

                    local.get $buf
                    local.get $pos
                    local.get $int_part
                    i64.const 10
                    i64.rem_u
                    i32.wrap_i64
                    i32.const 0x30
                    i32.add
                    array.set $string

                    local.get $int_part
                    i64.const 10
                    i64.div_u
                    local.set $int_part
                    br $iloop))))

            ;; Negative sign.
            local.get $is_neg
            (if
              (then
                local.get $pos i32.const 1 i32.sub local.set $pos
                local.get $buf
                local.get $pos
                i32.const 0x2D
                array.set $string))

            local.get $pos local.set $start_pos

            ;; Whole number? abs == floor(abs)
            local.get $abs_val
            local.get $abs_val
            f64.floor
            f64.eq
            (if
              (then
                ;; end_pos = 21
                i32.const 21 local.set $end_pos)
              (else
                ;; Find shortest N (1..17). 17 is the maximum number
                ;; of fractional digits any IEEE 754 f64 value
                ;; needs for a round-trip-unique representation —
                ;; matches Rust's `f64::to_string` shortest-
                ;; roundtrip (Ryu/Grisu). Bumped from 15 in PR #203
                ;; because the prior cap diverged from VM / Rust /
                ;; wasip2 output by one digit on values in the
                ;; [0.1, 10) range, e.g. golden-ratio `Float.fromInt
                ;; (fib(n + 1)) / Float.fromInt(fib(n))` from
                ;; `examples/data/fibonacci.av` printed
                ;; `1.618181818181818` on wasm-gc vs
                ;; `1.6181818181818182` on VM. For values outside
                ;; that range the overflow guard below fires first,
                ;; so the cap is effectively unreachable.
                f64.const 1 local.set $pow
                i32.const 0 local.set $n

                (block $ndone
                  (loop $nloop
                    local.get $n i32.const 1 i32.add local.set $n
                    local.get $pow f64.const 10 f64.mul local.set $pow

                    ;; Overflow guard: `i64.trunc_f64_s` traps when the
                    ;; argument is ≥ 2^63 or ≤ -2^63. For a Float like
                    ;; `(((-0.5772) * (-61.8024)) * (-877.8128))` ≈
                    ;; -31313.64 the loop converges late enough that
                    ;; `abs_val * 10^N` crosses 2^63 (~9.22e18) before
                    ;; N hits the 17-digit cap below, and the next
                    ;; `trunc_f64_s` traps with "wasm trap: integer
                    ;; overflow". Bail out of the loop with the
                    ;; current $scaled / $pow / $n when the next
                    ;; product would trap; we keep whatever precision
                    ;; the previous iteration captured.
                    local.get $abs_val
                    local.get $pow
                    f64.mul
                    f64.const 9.2233720368547758e18
                    f64.ge
                    (if
                      (then
                        local.get $n i32.const 1 i32.sub local.set $n
                        local.get $pow f64.const 10 f64.div local.set $pow
                        br $ndone))

                    local.get $abs_val
                    local.get $pow
                    f64.mul
                    f64.floor
                    i64.trunc_f64_s
                    local.set $scaled

                    local.get $scaled
                    f64.convert_i64_s
                    local.get $pow
                    f64.div
                    local.get $abs_val
                    f64.eq
                    br_if $ndone

                    local.get $n
                    i32.const 17
                    i32.ge_s
                    br_if $ndone

                    br $nloop))

                ;; frac_int = ((scaled % pow_i64) + pow_i64) % pow_i64
                local.get $scaled
                local.get $pow
                i64.trunc_f64_s
                i64.rem_s
                local.get $pow
                i64.trunc_f64_s
                i64.add
                local.get $pow
                i64.trunc_f64_s
                i64.rem_s
                local.set $frac_int

                ;; '.' at 21
                local.get $buf
                i32.const 21
                i32.const 0x2E
                array.set $string

                ;; Fractional digits right-to-left at [22..22+n].
                i32.const 22
                local.get $n
                i32.add
                i32.const 1
                i32.sub
                local.set $frac_pos

                local.get $n local.set $frac_digits

                (block $fdone
                  (loop $floop
                    local.get $frac_digits
                    i32.eqz
                    br_if $fdone

                    local.get $buf
                    local.get $frac_pos
                    local.get $frac_int
                    i64.const 10
                    i64.rem_u
                    i32.wrap_i64
                    i32.const 0x30
                    i32.add
                    array.set $string

                    local.get $frac_int
                    i64.const 10
                    i64.div_u
                    local.set $frac_int

                    local.get $frac_pos
                    i32.const 1
                    i32.sub
                    local.set $frac_pos

                    local.get $frac_digits
                    i32.const 1
                    i32.sub
                    local.set $frac_digits
                    br $floop))

                ;; Strip trailing zeros: end_pos = 22 + n; while end_pos > 22
                ;; and buf[end_pos-1] == '0', end_pos--.
                i32.const 22
                local.get $n
                i32.add
                local.set $end_pos

                (block $sdone
                  (loop $sloop
                    local.get $end_pos
                    i32.const 22
                    i32.le_s
                    br_if $sdone

                    local.get $buf
                    local.get $end_pos
                    i32.const 1
                    i32.sub
                    array.get_u $string
                    i32.const 0x30
                    i32.ne
                    br_if $sdone

                    local.get $end_pos
                    i32.const 1
                    i32.sub
                    local.set $end_pos
                    br $sloop))))

            ;; out_len = end_pos - start_pos
            local.get $end_pos
            local.get $start_pos
            i32.sub
            local.set $out_len

            local.get $out_len
            array.new_default $string
            local.set $out

            local.get $out
            i32.const 0
            local.get $buf
            local.get $start_pos
            local.get $out_len
            array.copy $string $string

            local.get $out)
        )
    "#
    );
    wat_helper::compile_wat_helper(&wat)
}

/// `__wasmgc_string_eq(a, b) -> i32`. Byte-equal compare over two
/// `(ref null $string)`. Returns 1 iff lens match and every byte
/// agrees; 0 otherwise.
fn emit_string_eq(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let (_, preamble) = string_module_preamble(registry)?;
    let wat = format!(
        r#"
        (module
          {preamble}
          (func (export "helper")
                (param $a (ref null $string))
                (param $b (ref null $string))
                (result i32)
            (local $alen i32)
            (local $i i32)

            local.get $a array.len
            local.get $b array.len
            i32.ne
            (if (then i32.const 0 return))

            local.get $a array.len
            local.set $alen
            i32.const 0
            local.set $i

            (block $done
              (loop $cmp
                local.get $i
                local.get $alen
                i32.ge_u
                br_if $done

                local.get $a
                local.get $i
                array.get_u $string

                local.get $b
                local.get $i
                array.get_u $string

                i32.ne
                (if (then i32.const 0 return))

                local.get $i
                i32.const 1
                i32.add
                local.set $i
                br $cmp))

            i32.const 1)
        )
    "#
    );
    wat_helper::compile_wat_helper(&wat)
}

/// `__wasmgc_string_compare(a, b) -> i32`. Lexicographic byte compare.
/// Returns -1 / 0 / 1. Used by String `<` / `>` / `<=` / `>=` BinOps.
/// Iterates byte-by-byte; on first mismatch returns -1 or 1; if one
/// string is a prefix of the other, the shorter compares less.
fn emit_string_compare(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let (_, preamble) = string_module_preamble(registry)?;
    let wat = format!(
        r#"
        (module
          {preamble}
          (func (export "helper")
                (param $a (ref null $string))
                (param $b (ref null $string))
                (result i32)
            (local $alen i32)
            (local $blen i32)
            (local $minlen i32)
            (local $i i32)
            (local $ab i32)
            (local $bb i32)

            local.get $a array.len
            local.set $alen
            local.get $b array.len
            local.set $blen

            ;; min(alen, blen)
            local.get $alen
            local.get $blen
            i32.lt_u
            (if (result i32)
              (then local.get $alen)
              (else local.get $blen))
            local.set $minlen

            i32.const 0
            local.set $i

            (block $done
              (loop $cmp
                local.get $i
                local.get $minlen
                i32.ge_u
                br_if $done

                local.get $a local.get $i array.get_u $string
                local.set $ab
                local.get $b local.get $i array.get_u $string
                local.set $bb

                local.get $ab
                local.get $bb
                i32.lt_u
                (if (then i32.const -1 return))

                local.get $ab
                local.get $bb
                i32.gt_u
                (if (then i32.const 1 return))

                local.get $i
                i32.const 1
                i32.add
                local.set $i
                br $cmp))

            ;; All compared bytes equal — shorter side wins.
            local.get $alen
            local.get $blen
            i32.lt_u
            (if (result i32)
              (then i32.const -1)
              (else
                local.get $alen
                local.get $blen
                i32.gt_u
                (if (result i32)
                  (then i32.const 1)
                  (else i32.const 0)))))
        )
    "#
    );
    wat_helper::compile_wat_helper(&wat)
}

/// `String.endsWith(s, suffix) -> Bool`. Byte-wise compare of the
/// trailing `len(suffix)` bytes of `s` against `suffix`.
fn emit_string_ends_with(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let (_, preamble) = string_module_preamble(registry)?;
    let wat = format!(
        r#"
        (module
          {preamble}
          (func (export "helper")
                (param $s (ref null $string))
                (param $suffix (ref null $string))
                (result i32)
            (local $slen i32)
            (local $sufflen i32)
            (local $offset i32)
            (local $i i32)

            local.get $s array.len local.set $slen
            local.get $suffix array.len local.set $sufflen

            ;; suffix longer than s → false
            local.get $sufflen
            local.get $slen
            i32.gt_u
            (if (then i32.const 0 return))

            ;; offset = slen - sufflen
            local.get $slen
            local.get $sufflen
            i32.sub
            local.set $offset

            i32.const 0 local.set $i
            (block $done
              (loop $cmp
                local.get $i
                local.get $sufflen
                i32.ge_u
                br_if $done

                local.get $s
                local.get $offset
                local.get $i
                i32.add
                array.get_u $string

                local.get $suffix
                local.get $i
                array.get_u $string

                i32.ne
                (if (then i32.const 0 return))

                local.get $i
                i32.const 1
                i32.add
                local.set $i
                br $cmp))
            i32.const 1)
        )
    "#
    );
    wat_helper::compile_wat_helper(&wat)
}

/// `String.fromBool(b) -> String`. Branches on the i32 Bool input
/// and returns one of two 5/4-byte string literals built inline via
/// per-byte `array.set`. No data-segment dependency — the literals
/// are baked into the helper body so registering `String.fromBool`
/// doesn't require pre-interning anything.
fn emit_string_from_bool(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let (_, preamble) = string_module_preamble(registry)?;
    let wat = format!(
        r#"
        (module
          {preamble}
          (func (export "helper")
                (param $b i32)
                (result (ref null $string))
            (local $out (ref null $string))

            local.get $b
            (if (result (ref null $string))
              (then
                ;; "true"
                i32.const 4
                array.new_default $string
                local.set $out
                local.get $out i32.const 0 i32.const 116 array.set $string
                local.get $out i32.const 1 i32.const 114 array.set $string
                local.get $out i32.const 2 i32.const 117 array.set $string
                local.get $out i32.const 3 i32.const 101 array.set $string
                local.get $out)
              (else
                ;; "false"
                i32.const 5
                array.new_default $string
                local.set $out
                local.get $out i32.const 0 i32.const 102 array.set $string
                local.get $out i32.const 1 i32.const 97  array.set $string
                local.get $out i32.const 2 i32.const 108 array.set $string
                local.get $out i32.const 3 i32.const 115 array.set $string
                local.get $out i32.const 4 i32.const 101 array.set $string
                local.get $out)))
        )
    "#
    );
    wat_helper::compile_wat_helper(&wat)
}

/// `String.charAt(s: String, i: Int) -> Option<String>`. `i` is a
/// Unicode scalar index (the VM's `s.chars().nth(i)`), NOT a byte
/// index: scan the UTF-8 byte array scalar by scalar, and on hit
/// return `Option.Some(<full character>)` — all 1–4 bytes of it.
/// `Option.None` on a negative or past-the-end index. The old body
/// indexed bytes and returned a single byte, which both diverged
/// from the VM and tore multi-byte characters apart.
fn emit_string_char_at(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let string_idx = registry
        .string_array_type_idx
        .ok_or(WasmGcError::Validation(
            "String.charAt: String slot not registered".into(),
        ))?;
    let opt_idx = registry
        .option_type_idx("Option<String>")
        .ok_or(WasmGcError::Validation(
            "String.charAt: Option<String> slot not registered".into(),
        ))?;
    if opt_idx <= string_idx {
        return Err(WasmGcError::Validation(format!(
            "String.charAt helper expects opt_idx > string_idx (got {opt_idx} vs {string_idx})"
        )));
    }
    let pre_string = wat_helper::padding_types(string_idx);
    let between = wat_helper::padding_types(opt_idx - string_idx - 1);
    let wat = format!(
        r#"
        (module
          {pre_string}
          (type $string (array (mut i8)))
          {between}
          (type $option_string (struct (field $tag i32) (field $val (ref null $string))))
          (func (export "helper")
                (param $s (ref null $string))
                (param $i i64)
                (result (ref null $option_string))
            (local $n i32)
            (local $b i32)
            (local $k i64)
            (local $clen i32)
            (local $out (ref null $string))

            local.get $s array.len local.set $n

            ;; negative index → None
            local.get $i
            i64.const 0
            i64.lt_s
            (if (then
              i32.const 0
              ref.null $string
              struct.new $option_string
              return))

            (block $found
              (loop $scan
                ;; cursor past the last byte → index out of range → None
                local.get $b
                local.get $n
                i32.ge_u
                (if (then
                  i32.const 0
                  ref.null $string
                  struct.new $option_string
                  return))

                ;; clen = UTF-8 length from the lead byte:
                ;; <0xC0 → 1 (ASCII; stray continuation defends as 1),
                ;; <0xE0 → 2, <0xF0 → 3, else 4
                i32.const 1
                local.set $clen
                local.get $s local.get $b array.get_u $string
                i32.const 0xC0 i32.ge_u
                (if (then i32.const 2 local.set $clen))
                local.get $s local.get $b array.get_u $string
                i32.const 0xE0 i32.ge_u
                (if (then i32.const 3 local.set $clen))
                local.get $s local.get $b array.get_u $string
                i32.const 0xF0 i32.ge_u
                (if (then i32.const 4 local.set $clen))

                ;; reached the target scalar?
                local.get $k
                local.get $i
                i64.eq
                br_if $found

                local.get $b local.get $clen i32.add local.set $b
                local.get $k i64.const 1 i64.add local.set $k
                br $scan))

            ;; clamp clen to the remaining bytes (truncated UTF-8 tail)
            local.get $b local.get $clen i32.add
            local.get $n
            i32.gt_u
            (if (then
              local.get $n local.get $b i32.sub local.set $clen))

            local.get $clen
            array.new_default $string
            local.set $out
            local.get $out
            i32.const 0
            local.get $s
            local.get $b
            local.get $clen
            array.copy $string $string

            i32.const 1
            local.get $out
            struct.new $option_string)
        )
    "#
    );
    wat_helper::compile_wat_helper(&wat)
}

/// `Char.toCode(s: String) -> Int`. Aver represents a Char as a one-scalar
/// String, so decode the first UTF-8 scalar instead of returning byte zero.
/// Empty strings trap, matching the old wasm-gc hard-failure behavior for
/// invalid Char values.
fn emit_char_to_code(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let string_idx = registry
        .string_array_type_idx
        .ok_or(WasmGcError::Validation(
            "Char.toCode: String slot not registered".into(),
        ))?;
    let pre_string = wat_helper::padding_types(string_idx);
    let wat = format!(
        r#"
        (module
          {pre_string}
          (type $string (array (mut i8)))
          (func (export "helper") (param $s (ref null $string)) (result i64)
            (local $n i32)
            (local $b0 i32)
            (local $b1 i32)
            (local $b2 i32)
            (local $b3 i32)
            (local $clen i32)
            (local $code i32)

            local.get $s
            array.len
            local.set $n

            local.get $n
            i32.eqz
            (if (then unreachable))

            local.get $s
            i32.const 0
            array.get_u $string
            local.set $b0

            ;; UTF-8 length from the lead byte. Treat stray continuation bytes
            ;; as length 1, like String.charAt's defensive scanner.
            i32.const 1
            local.set $clen
            local.get $b0
            i32.const 0xC0
            i32.ge_u
            (if (then i32.const 2 local.set $clen))
            local.get $b0
            i32.const 0xE0
            i32.ge_u
            (if (then i32.const 3 local.set $clen))
            local.get $b0
            i32.const 0xF0
            i32.ge_u
            (if (then i32.const 4 local.set $clen))

            ;; Clamp malformed truncated tails to the bytes that exist.
            local.get $clen
            local.get $n
            i32.gt_u
            (if (then local.get $n local.set $clen))

            local.get $clen
            i32.const 1
            i32.eq
            (if
              (then
                local.get $b0
                local.set $code)
              (else
                local.get $clen
                i32.const 2
                i32.eq
                (if
                  (then
                    local.get $s
                    i32.const 1
                    array.get_u $string
                    local.set $b1
                    local.get $b0
                    i32.const 0x1F
                    i32.and
                    i32.const 6
                    i32.shl
                    local.get $b1
                    i32.const 0x3F
                    i32.and
                    i32.or
                    local.set $code)
                  (else
                    local.get $clen
                    i32.const 3
                    i32.eq
                    (if
                      (then
                        local.get $s
                        i32.const 1
                        array.get_u $string
                        local.set $b1
                        local.get $s
                        i32.const 2
                        array.get_u $string
                        local.set $b2
                        local.get $b0
                        i32.const 0x0F
                        i32.and
                        i32.const 12
                        i32.shl
                        local.get $b1
                        i32.const 0x3F
                        i32.and
                        i32.const 6
                        i32.shl
                        i32.or
                        local.get $b2
                        i32.const 0x3F
                        i32.and
                        i32.or
                        local.set $code)
                      (else
                        local.get $s
                        i32.const 1
                        array.get_u $string
                        local.set $b1
                        local.get $s
                        i32.const 2
                        array.get_u $string
                        local.set $b2
                        local.get $s
                        i32.const 3
                        array.get_u $string
                        local.set $b3
                        local.get $b0
                        i32.const 0x07
                        i32.and
                        i32.const 18
                        i32.shl
                        local.get $b1
                        i32.const 0x3F
                        i32.and
                        i32.const 12
                        i32.shl
                        i32.or
                        local.get $b2
                        i32.const 0x3F
                        i32.and
                        i32.const 6
                        i32.shl
                        i32.or
                        local.get $b3
                        i32.const 0x3F
                        i32.and
                        i32.or
                        local.set $code))))))

            local.get $code
            i64.extend_i32_u)
        )
    "#
    );
    wat_helper::compile_wat_helper(&wat)
}

/// `Char.fromCode(code: Int) -> Option<String>`. Encodes a Unicode
/// code point as UTF-8 (1–4 bytes) into a fresh `(array i8)`. Returns
/// Option.None for negative values, codepoints above U+10FFFF, and the
/// surrogate range U+D800..U+DFFF. Ported from
/// `src/codegen/wasm/runtime/wat/char_from_code.part.wat` (legacy
/// backend) — the linear-memory `OBJ_STRING` shape is replaced with
/// `array.new_default $string` + `array.set`, the rest is byte-for-byte
/// the same UTF-8 encoder. Critical for games like
/// `examples/games/doom` that emit Braille block characters
/// (U+2800..U+28FF, 3-byte UTF-8).
fn emit_char_from_code(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let string_idx = registry
        .string_array_type_idx
        .ok_or(WasmGcError::Validation(
            "Char.fromCode: String slot not registered".into(),
        ))?;
    let opt_idx = registry
        .option_type_idx("Option<String>")
        .ok_or(WasmGcError::Validation(
            "Char.fromCode: Option<String> slot not registered".into(),
        ))?;
    if opt_idx <= string_idx {
        return Err(WasmGcError::Validation(format!(
            "Char.fromCode helper expects opt_idx > string_idx (got {opt_idx} vs {string_idx})"
        )));
    }
    let pre_string = wat_helper::padding_types(string_idx);
    let between = wat_helper::padding_types(opt_idx - string_idx - 1);
    let wat = format!(
        r#"
        (module
          {pre_string}
          (type $string (array (mut i8)))
          {between}
          (type $option_string (struct (field $tag i32) (field $val (ref null $string))))
          (func (export "helper") (param $code i64) (result (ref null $option_string))
            (local $c i32)
            (local $len i32)
            (local $arr (ref null $string))

            ;; Reject negatives, > 0x10FFFF, or surrogate range [0xD800, 0xDFFF].
            local.get $code
            i64.const 0
            i64.lt_s
            local.get $code
            i64.const 0x10FFFF
            i64.gt_s
            i32.or
            local.get $code
            i64.const 0xD800
            i64.ge_s
            local.get $code
            i64.const 0xDFFF
            i64.le_s
            i32.and
            i32.or
            (if (result (ref null $option_string))
              (then
                ;; Option.None — tag=0, val=null.
                i32.const 0
                ref.null $string
                struct.new $option_string)
              (else
                local.get $code
                i32.wrap_i64
                local.set $c

                ;; len = 1/2/3/4 by code range.
                local.get $c
                i32.const 0x80
                i32.lt_u
                (if (result i32)
                  (then i32.const 1)
                  (else
                    local.get $c
                    i32.const 0x800
                    i32.lt_u
                    (if (result i32)
                      (then i32.const 2)
                      (else
                        local.get $c
                        i32.const 0x10000
                        i32.lt_u
                        (if (result i32)
                          (then i32.const 3)
                          (else i32.const 4))))))
                local.set $len

                ;; Allocate result array and write UTF-8 bytes.
                local.get $len
                array.new_default $string
                local.set $arr

                local.get $len
                i32.const 1
                i32.eq
                (if
                  (then
                    local.get $arr
                    i32.const 0
                    local.get $c
                    array.set $string)
                  (else
                    local.get $len
                    i32.const 2
                    i32.eq
                    (if
                      (then
                        local.get $arr
                        i32.const 0
                        local.get $c i32.const 6 i32.shr_u i32.const 0xC0 i32.or
                        array.set $string
                        local.get $arr
                        i32.const 1
                        local.get $c i32.const 0x3F i32.and i32.const 0x80 i32.or
                        array.set $string)
                      (else
                        local.get $len
                        i32.const 3
                        i32.eq
                        (if
                          (then
                            local.get $arr
                            i32.const 0
                            local.get $c i32.const 12 i32.shr_u i32.const 0xE0 i32.or
                            array.set $string
                            local.get $arr
                            i32.const 1
                            local.get $c i32.const 6 i32.shr_u i32.const 0x3F i32.and i32.const 0x80 i32.or
                            array.set $string
                            local.get $arr
                            i32.const 2
                            local.get $c i32.const 0x3F i32.and i32.const 0x80 i32.or
                            array.set $string)
                          (else
                            ;; len == 4
                            local.get $arr
                            i32.const 0
                            local.get $c i32.const 18 i32.shr_u i32.const 0xF0 i32.or
                            array.set $string
                            local.get $arr
                            i32.const 1
                            local.get $c i32.const 12 i32.shr_u i32.const 0x3F i32.and i32.const 0x80 i32.or
                            array.set $string
                            local.get $arr
                            i32.const 2
                            local.get $c i32.const 6 i32.shr_u i32.const 0x3F i32.and i32.const 0x80 i32.or
                            array.set $string
                            local.get $arr
                            i32.const 3
                            local.get $c i32.const 0x3F i32.and i32.const 0x80 i32.or
                            array.set $string))))))

                ;; Option.Some(arr) — tag=1, val=arr.
                i32.const 1
                local.get $arr
                struct.new $option_string)))
        )
    "#
    );
    wat_helper::compile_wat_helper(&wat)
}

/// `String.chars(s: String) -> List<String>`. Iterates `s` right-to-
/// left building a cons list directly (no reverse pass). Each
/// Unicode scalar value is its own 1–4 byte string allocation.
fn emit_string_chars(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let s_idx = registry
        .string_array_type_idx
        .ok_or(WasmGcError::Validation(
            "String.chars: String slot not registered".into(),
        ))?;
    let list_idx = registry
        .list_type_idx("List<String>")
        .ok_or(WasmGcError::Validation(
            "String.chars: List<String> slot not registered".into(),
        ))?;
    let string_ref = ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(s_idx),
    });
    let list_ref = ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(list_idx),
    });
    // Splits into Unicode scalar values (the VM's `s.chars()`), not
    // single bytes. Reverse byte scan so the list cons-builds in
    // source order without a final reverse pass: a byte with
    // `(b & 0xC0) != 0x80` starts a character, and `end` tracks the
    // exclusive end of the character being collected.
    // params: 0=s. locals: 1=acc, 2=i, 3=cell, 4=end, 5=clen.
    let mut f = Function::new([
        (1, list_ref),
        (1, ValType::I32),
        (1, string_ref),
        (2, ValType::I32),
    ]);
    // acc = null
    f.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
        list_idx,
    )));
    f.instruction(&Instruction::LocalSet(1));
    // end = s.len
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::ArrayLen);
    f.instruction(&Instruction::LocalSet(4));
    // i = s.len - 1
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::ArrayLen);
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    // if i < 0 break
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32LtS);
    f.instruction(&Instruction::BrIf(1));
    // if (s[i] & 0xC0) != 0x80 — character start
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::ArrayGetU(s_idx));
    f.instruction(&Instruction::I32Const(0xC0));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::I32Const(0x80));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    // clen = end - i
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(5));
    // cell = array.new_default $string clen
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::ArrayNewDefault(s_idx));
    f.instruction(&Instruction::LocalSet(3));
    // array.copy cell[0..clen] <- s[i..end]
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::ArrayCopy {
        array_type_index_dst: s_idx,
        array_type_index_src: s_idx,
    });
    // acc = struct.new $list (cell, acc)
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructNew(list_idx));
    f.instruction(&Instruction::LocalSet(1));
    // end = i
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::End);
    // i--
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `Byte.fromHex(s: String) -> Result<Int, String>`. Parses a 2-byte
/// ASCII hex string. Validates length + each digit; returns
/// `Result.Ok(byte)` on success or `Result.Err(s)` on any parse
/// failure (length != 2 or any non-hex byte).
fn emit_byte_from_hex(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let preamble =
        string_and_result_preamble(registry, "Result<Int,String>", "i64", "(ref null $string)")?;
    let wat = format!(
        r#"
        (module
          {preamble}
          (func (export "helper")
                (param $s (ref null $string))
                (result (ref null $result))
            (local $d0 i32)
            (local $d1 i32)
            (local $byte i64)
            (local $byte_local i32)

            ;; len(s) != 2 → Err(s)
            local.get $s array.len
            i32.const 2
            i32.ne
            (if
              (then
                i32.const 0
                i64.const 0
                local.get $s
                struct.new $result
                return))

            ;; d0 = hex_digit(s[0]) inline (no subroutines — wat_helper
            ;; only emits the first function in the module)
            local.get $s i32.const 0 array.get_u $string local.set $byte_local
            i32.const -1 local.set $d0
            local.get $byte_local i32.const 48 i32.ge_u
            local.get $byte_local i32.const 57 i32.le_u i32.and
            (if (then local.get $byte_local i32.const 48 i32.sub local.set $d0))
            local.get $byte_local i32.const 65 i32.ge_u
            local.get $byte_local i32.const 70 i32.le_u i32.and
            (if (then local.get $byte_local i32.const 55 i32.sub local.set $d0))
            local.get $byte_local i32.const 97 i32.ge_u
            local.get $byte_local i32.const 102 i32.le_u i32.and
            (if (then local.get $byte_local i32.const 87 i32.sub local.set $d0))

            ;; d1 = hex_digit(s[1]) inline
            local.get $s i32.const 1 array.get_u $string local.set $byte_local
            i32.const -1 local.set $d1
            local.get $byte_local i32.const 48 i32.ge_u
            local.get $byte_local i32.const 57 i32.le_u i32.and
            (if (then local.get $byte_local i32.const 48 i32.sub local.set $d1))
            local.get $byte_local i32.const 65 i32.ge_u
            local.get $byte_local i32.const 70 i32.le_u i32.and
            (if (then local.get $byte_local i32.const 55 i32.sub local.set $d1))
            local.get $byte_local i32.const 97 i32.ge_u
            local.get $byte_local i32.const 102 i32.le_u i32.and
            (if (then local.get $byte_local i32.const 87 i32.sub local.set $d1))

            ;; if either < 0 → Err
            local.get $d0
            i32.const 0
            i32.lt_s
            local.get $d1
            i32.const 0
            i32.lt_s
            i32.or
            (if
              (then
                i32.const 0
                i64.const 0
                local.get $s
                struct.new $result
                return))

            ;; byte = d0 * 16 + d1
            local.get $d0
            i32.const 4
            i32.shl
            local.get $d1
            i32.or
            i64.extend_i32_u
            local.set $byte

            ;; Result.Ok(byte): tag=1, ok=byte, err=null
            i32.const 1
            local.get $byte
            ref.null $string
            struct.new $result)
        )
    "#
    );
    wat_helper::compile_wat_helper(&wat)
}

/// `Byte.toHex(b: Int) -> Result<String, String>`. Validates `b` is in
/// `[0, 256)`. On success returns the 2-char lowercase hex string
/// `Result.Ok(hex)`. Out-of-range returns `Result.Err(empty)` —
/// callers that want a richer error string should validate themselves
/// or wrap; this matches the legacy backend's runtime helper.
fn emit_byte_to_hex(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let preamble = string_and_result_preamble(
        registry,
        "Result<String,String>",
        "(ref null $string)",
        "(ref null $string)",
    )?;
    let wat = format!(
        r#"
        (module
          {preamble}
          (func (export "helper")
                (param $b i64)
                (result (ref null $result))
            (local $hi i32)
            (local $lo i32)
            (local $hi_byte i32)
            (local $lo_byte i32)
            (local $out (ref null $string))

            ;; Out of range → Err(empty)
            local.get $b
            i64.const 0
            i64.lt_s
            local.get $b
            i64.const 256
            i64.ge_s
            i32.or
            (if
              (then
                i32.const 0
                ref.null $string
                i32.const 0
                array.new_default $string
                struct.new $result
                return))

            local.get $b i32.wrap_i64 i32.const 4 i32.shr_u local.set $hi
            local.get $b i32.wrap_i64 i32.const 15 i32.and local.set $lo

            ;; hi_byte = hi < 10 ? '0'+hi : 'a'+hi-10  (inline)
            local.get $hi i32.const 10 i32.lt_u
            (if (result i32)
              (then local.get $hi i32.const 48 i32.add)
              (else local.get $hi i32.const 87 i32.add))
            local.set $hi_byte
            local.get $lo i32.const 10 i32.lt_u
            (if (result i32)
              (then local.get $lo i32.const 48 i32.add)
              (else local.get $lo i32.const 87 i32.add))
            local.set $lo_byte

            i32.const 2 array.new_default $string local.set $out
            local.get $out i32.const 0 local.get $hi_byte array.set $string
            local.get $out i32.const 1 local.get $lo_byte array.set $string

            ;; Result.Ok(out): tag=1, ok=out, err=null
            i32.const 1
            local.get $out
            ref.null $string
            struct.new $result)
        )
    "#
    );
    wat_helper::compile_wat_helper(&wat)
}

/// `String.replace(s: String, needle: String, repl: String) -> String`.
/// Two-pass naive scan: count occurrences of `needle` in `s`, allocate
/// the output array of exact final size, fill while walking `s`. Empty
/// needle returns `s` unchanged (avoids the infinite-loop trap;
/// matches legacy backend behaviour).
fn emit_string_replace(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let (_, preamble) = string_module_preamble(registry)?;
    let wat = format!(
        r#"
        (module
          {preamble}
          (func (export "helper")
                (param $s (ref null $string))
                (param $n (ref null $string))
                (param $r (ref null $string))
                (result (ref null $string))
            (local $slen i32)
            (local $nlen i32)
            (local $rlen i32)
            (local $count i32)
            (local $outlen i32)
            (local $i i32)
            (local $j i32)
            (local $k i32)
            (local $matched i32)
            (local $out (ref null $string))

            local.get $s array.len local.set $slen
            local.get $n array.len local.set $nlen
            local.get $r array.len local.set $rlen

            ;; Empty needle → return a copy of s (return s itself; Aver
            ;; semantics are immutable so handle reuse is fine).
            local.get $nlen
            i32.eqz
            (if (then local.get $s return))

            ;; Pass 1: count occurrences.
            i32.const 0 local.set $count
            i32.const 0 local.set $i
            (block $count_done
              (loop $count_loop
                ;; if i + nlen > slen → done
                local.get $i
                local.get $nlen
                i32.add
                local.get $slen
                i32.gt_u
                br_if $count_done

                ;; matched = 1; for k in 0..nlen: if s[i+k]!=n[k]: matched=0
                i32.const 1 local.set $matched
                i32.const 0 local.set $k
                (block $cmp_done
                  (loop $cmp_loop
                    local.get $k
                    local.get $nlen
                    i32.ge_u
                    br_if $cmp_done

                    local.get $s
                    local.get $i
                    local.get $k
                    i32.add
                    array.get_u $string

                    local.get $n
                    local.get $k
                    array.get_u $string

                    i32.ne
                    (if
                      (then
                        i32.const 0 local.set $matched
                        br $cmp_done))

                    local.get $k
                    i32.const 1
                    i32.add
                    local.set $k
                    br $cmp_loop))

                local.get $matched
                (if
                  (then
                    local.get $count i32.const 1 i32.add local.set $count
                    local.get $i local.get $nlen i32.add local.set $i)
                  (else
                    local.get $i i32.const 1 i32.add local.set $i))
                br $count_loop))

            ;; outlen = slen + count * (rlen - nlen)
            local.get $slen
            local.get $count
            local.get $rlen
            local.get $nlen
            i32.sub
            i32.mul
            i32.add
            local.set $outlen

            local.get $outlen
            array.new_default $string
            local.set $out

            ;; Pass 2: fill.
            i32.const 0 local.set $i
            i32.const 0 local.set $j
            (block $fill_done
              (loop $fill_loop
                local.get $i
                local.get $slen
                i32.ge_u
                br_if $fill_done

                ;; check needle match at i
                i32.const 0 local.set $matched
                local.get $i
                local.get $nlen
                i32.add
                local.get $slen
                i32.le_u
                (if
                  (then
                    i32.const 1 local.set $matched
                    i32.const 0 local.set $k
                    (block $fcmp_done
                      (loop $fcmp_loop
                        local.get $k
                        local.get $nlen
                        i32.ge_u
                        br_if $fcmp_done

                        local.get $s
                        local.get $i
                        local.get $k
                        i32.add
                        array.get_u $string

                        local.get $n
                        local.get $k
                        array.get_u $string

                        i32.ne
                        (if
                          (then
                            i32.const 0 local.set $matched
                            br $fcmp_done))

                        local.get $k
                        i32.const 1
                        i32.add
                        local.set $k
                        br $fcmp_loop))))

                local.get $matched
                (if
                  (then
                    ;; Copy repl bytes to out
                    i32.const 0 local.set $k
                    (block $copy_done
                      (loop $copy_loop
                        local.get $k
                        local.get $rlen
                        i32.ge_u
                        br_if $copy_done

                        local.get $out
                        local.get $j
                        local.get $k
                        i32.add

                        local.get $r
                        local.get $k
                        array.get_u $string

                        array.set $string

                        local.get $k
                        i32.const 1
                        i32.add
                        local.set $k
                        br $copy_loop))

                    local.get $j
                    local.get $rlen
                    i32.add
                    local.set $j

                    local.get $i
                    local.get $nlen
                    i32.add
                    local.set $i)
                  (else
                    ;; copy s[i] to out[j]
                    local.get $out
                    local.get $j

                    local.get $s
                    local.get $i
                    array.get_u $string

                    array.set $string

                    local.get $j
                    i32.const 1
                    i32.add
                    local.set $j

                    local.get $i
                    i32.const 1
                    i32.add
                    local.set $i))
                br $fill_loop))

            local.get $out)
        )
    "#
    );
    wat_helper::compile_wat_helper(&wat)
}

/// `__int_mod_euclid(a: i64, b: i64) -> i64` — Euclidean modulo.
/// Algorithm mirrors Rust's `i64::rem_euclid`:
///
/// ```text
/// q = a rem_s b
/// r = if q < 0 { q + (if b < 0 { -b } else { b }) } else { q }
/// ```
///
/// Result is always in `[0, |b|)`. Caller must ensure `b != 0`; the
/// helper would `i64.rem_s`-trap on b == 0.
fn emit_int_mod_euclid() -> Result<Function, WasmGcError> {
    let wat = r#"
        (module
          (func (export "helper") (param $a i64) (param $b i64) (result i64)
            (local $q i64)
            (local.set $q (i64.rem_s (local.get $a) (local.get $b)))
            (if (result i64) (i64.lt_s (local.get $q) (i64.const 0))
              (then
                (i64.add (local.get $q)
                  (if (result i64) (i64.lt_s (local.get $b) (i64.const 0))
                    (then (i64.sub (i64.const 0) (local.get $b)))
                    (else (local.get $b)))))
              (else (local.get $q))))
        )
    "#;
    wat_helper::compile_wat_helper(wat)
}

/// `__int_div_euclid(a, b) -> i64` — Euclidean (flooring) division, the
/// exact partner of `emit_int_mod_euclid` so `div(a,b)*b + mod(a,b) == a`
/// for every sign. Mirrors Rust's `i64::div_euclid`: take the truncating
/// quotient `q = a/b` and remainder `r = a%b`; when `r < 0`, step `q`
/// toward `-inf` by one (down if `b > 0`, up if `b < 0`).
///
/// Caller must ensure `b != 0`; the helper would `i64.div_s`-trap on
/// b == 0 (and on the `i64::MIN / -1` overflow — the documented edge).
fn emit_int_div_euclid() -> Result<Function, WasmGcError> {
    let wat = r#"
        (module
          (func (export "helper") (param $a i64) (param $b i64) (result i64)
            (local $q i64)
            (local $r i64)
            (local.set $q (i64.div_s (local.get $a) (local.get $b)))
            (local.set $r (i64.rem_s (local.get $a) (local.get $b)))
            (if (result i64) (i64.lt_s (local.get $r) (i64.const 0))
              (then
                (if (result i64) (i64.gt_s (local.get $b) (i64.const 0))
                  (then (i64.sub (local.get $q) (i64.const 1)))
                  (else (i64.add (local.get $q) (i64.const 1)))))
              (else (local.get $q))))
        )
    "#;
    wat_helper::compile_wat_helper(wat)
}

//! `Int.*` builtin helpers.
//!
//! `Int.toString` is the headline one — needed for any program that
//! prints an integer. We emit it as a per-module helper that takes an
//! `i64` and returns a `(ref string)` (stringref proposal).
//!
//! Phase 3c uses the **stringref proposal** for `String` values
//! (`(ref string)` carrier, `string.const` literals,
//! `string.from_code_array` for runtime construction). All bench
//! scenarios that touch String — `string_interp`, `factorial` (prints),
//! `fractal_seahorse` — are scoped against modern engines that
//! support stringref (V8 / Wasmtime 25+ / Firefox 120+ all ship it).

use wasm_encoder::{Function, ValType};

use super::{StaticBuiltin, WasmGcError};

pub(super) const SPECS: &[StaticBuiltin] = &[StaticBuiltin {
    canonical_name: "Int.toString",
    params: &[ValType::I64],
    results: &[INT_TO_STRING_RESULT],
    body: emit_int_to_string,
}];

/// `(ref string)` — see crate-level note about stringref.
const INT_TO_STRING_RESULT: ValType = ValType::Ref(wasm_encoder::RefType {
    nullable: false,
    heap_type: wasm_encoder::HeapType::Abstract {
        shared: false,
        ty: wasm_encoder::AbstractHeapType::Any,
    },
});

/// Stub body for `Int.toString` — phase-3c placeholder.
///
/// A real implementation digit-converts the i64 into a UTF-8 byte
/// sequence in linear memory, then `string.from_utf8_array` (or
/// equivalent) into a `(ref string)`. That requires a memory section
/// + linear memory layout discipline we haven't set up yet.
///
/// For now: `unreachable` so the module still validates and any
/// program that actually invokes the helper trips a clean runtime
/// error pointing at the missing piece.
fn emit_int_to_string(func: &mut Function) -> Result<(), WasmGcError> {
    func.instruction(&wasm_encoder::Instruction::Unreachable);
    func.instruction(&wasm_encoder::Instruction::End);
    Ok(())
}

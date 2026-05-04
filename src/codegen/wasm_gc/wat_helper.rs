//! WAT-as-source-of-truth for builtin helper bodies.
//!
//! Writing complex helpers (digit-conversion loops, hashmap probing,
//! UTF-8 walks, etc.) in raw `wasm_encoder::Instruction` calls is
//! error-prone — every label, every type idx, every br_if has to be
//! threaded by hand. WAT text is the format the wasm spec uses, the
//! one engine devs and human readers both understand, and the one
//! that copy-pastes from spec docs.
//!
//! This module bridges WAT-text → wasm-encoder Function:
//!
//! 1. Caller provides a WAT template — a tiny standalone module with
//!    one fn marked as the helper. Type indices that need to match
//!    the user module are passed via `{placeholder}` substitution
//!    (we use a small `replace` step rather than a full templating
//!    engine — every helper has 1-3 substitutions max).
//! 2. `wat::parse_str` lowers the text to a binary wasm module.
//! 3. `wasmparser` walks the parsed module and finds the helper
//!    function's body (locals + expression bytes).
//! 4. We extract the locals declaration and the raw expression bytes,
//!    then build `wasm_encoder::Function::new(locals).raw(expr_bytes)`
//!    so the host code section accepts them.
//!
//! The result: each helper lives as readable WAT in a `&'static str`,
//! gets compiled to wasm bytes once at codegen time, and lands in the
//! user module as if it were hand-written wasm-encoder calls.

use wasm_encoder::{Function, ValType};
use wasmparser::{Parser, Payload};

use super::WasmGcError;

/// Pad the WAT module's type section with `n` empty struct types
/// so that subsequent named types align with their user-module
/// indices. Used by helpers that reference user-module types
/// (String, List, Map, …) — the helper module needs to assign the
/// same index to those types as the user module does, otherwise
/// the spliced-in body's `array.new_default <idx>` instructions
/// reference the wrong type.
///
/// Returns a WAT fragment to inject right after `(module`.
pub(in crate::codegen::wasm_gc) fn padding_types(n: u32) -> String {
    let mut s = String::with_capacity(n as usize * 16);
    for _ in 0..n {
        s.push_str("(type (struct))\n");
    }
    s
}

/// Compile a WAT helper template into a `wasm_encoder::Function` ready
/// to be appended to the user module's code section.
///
/// `wat_source` must be a complete WAT module containing exactly one
/// `func`. Any type/global/data declarations needed (e.g. the `(array
/// i8)` type the helper references) live inside the same WAT module
/// and are discarded after parsing — only the function body bytes
/// transfer to the user module. Type indices in the WAT body refer
/// to types declared in the WAT module itself; the caller must
/// ensure those indices match the user module's slots (use the
/// placeholder substitution shape — declare the same types in the
/// same order as the user module's TypeRegistry).
///
/// Returns the compiled Function whose locals + body bytes match the
/// WAT helper.
pub(in crate::codegen::wasm_gc) fn compile_wat_helper(wat_source: &str) -> Result<Function, WasmGcError> {
    let module_bytes = wat::parse_str(wat_source)
        .map_err(|e| WasmGcError::Validation(format!("wat parse: {e}")))?;

    let mut found_locals: Option<Vec<ValType>> = None;
    let mut found_body: Option<Vec<u8>> = None;

    for payload in Parser::new(0).parse_all(&module_bytes) {
        let payload =
            payload.map_err(|e| WasmGcError::Validation(format!("wasm parse helper: {e}")))?;
        if let Payload::CodeSectionEntry(body) = payload {
            // Helper modules contain exactly one function — take the
            // first one and stop.
            let mut locals: Vec<ValType> = Vec::new();
            let mut locals_reader = body
                .get_locals_reader()
                .map_err(|e| WasmGcError::Validation(format!("locals reader: {e}")))?;
            let count = locals_reader.get_count();
            for _ in 0..count {
                let (n, ty) = locals_reader
                    .read()
                    .map_err(|e| WasmGcError::Validation(format!("locals read: {e}")))?;
                let val_ty = wasmparser_to_encoder_valtype(ty)?;
                for _ in 0..n {
                    locals.push(val_ty);
                }
            }

            // The expression bytes start right after the locals
            // declaration. wasmparser doesn't expose a direct
            // "expr_range" but `OperatorsReader::original_position`
            // gives the offset of the first instruction; we slice
            // from there to the end of the body.
            let ops_reader = body
                .get_operators_reader()
                .map_err(|e| WasmGcError::Validation(format!("ops reader: {e}")))?;
            let expr_start = ops_reader.original_position();
            let body_range = body.range();
            let expr_bytes = module_bytes[expr_start..body_range.end].to_vec();

            found_locals = Some(locals);
            found_body = Some(expr_bytes);
            break;
        }
    }

    let locals = found_locals.ok_or(WasmGcError::Validation(
        "wat helper has no function body".into(),
    ))?;
    let body = found_body.ok_or(WasmGcError::Validation(
        "wat helper missing body bytes".into(),
    ))?;

    let local_groups: Vec<(u32, ValType)> = compress_locals(&locals);
    let mut func = Function::new(local_groups);
    func.raw(body);
    Ok(func)
}

/// Group consecutive same-type locals into `(count, type)` pairs the
/// `Function::new` constructor expects.
fn compress_locals(locals: &[ValType]) -> Vec<(u32, ValType)> {
    let mut out: Vec<(u32, ValType)> = Vec::new();
    for ty in locals {
        if let Some(last) = out.last_mut()
            && last.1 == *ty
        {
            last.0 += 1;
        } else {
            out.push((1, *ty));
        }
    }
    out
}

fn wasmparser_to_encoder_valtype(ty: wasmparser::ValType) -> Result<ValType, WasmGcError> {
    use wasmparser::ValType as PT;
    match ty {
        PT::I32 => Ok(ValType::I32),
        PT::I64 => Ok(ValType::I64),
        PT::F32 => Ok(ValType::F32),
        PT::F64 => Ok(ValType::F64),
        PT::V128 => Ok(ValType::V128),
        PT::Ref(rt) => {
            let nullable = rt.is_nullable();
            let heap_type = match rt.heap_type() {
                wasmparser::HeapType::Abstract { shared, ty } => {
                    use wasm_encoder::AbstractHeapType as E;
                    use wasmparser::AbstractHeapType as P;
                    let mapped = match ty {
                        P::Func => E::Func,
                        P::Extern => E::Extern,
                        P::Any => E::Any,
                        P::None => E::None,
                        P::NoExtern => E::NoExtern,
                        P::NoFunc => E::NoFunc,
                        P::Eq => E::Eq,
                        P::Struct => E::Struct,
                        P::Array => E::Array,
                        P::I31 => E::I31,
                        P::Exn => E::Exn,
                        P::NoExn => E::NoExn,
                        P::Cont => E::Cont,
                        P::NoCont => E::NoCont,
                    };
                    wasm_encoder::HeapType::Abstract { shared, ty: mapped }
                }
                wasmparser::HeapType::Concrete(idx) => match idx {
                    wasmparser::UnpackedIndex::Module(i) => wasm_encoder::HeapType::Concrete(i),
                    other => {
                        return Err(WasmGcError::Validation(format!(
                            "wat helper local has non-module concrete heap idx: {other:?}"
                        )));
                    }
                },
            };
            Ok(ValType::Ref(wasm_encoder::RefType {
                nullable,
                heap_type,
            }))
        }
    }
}

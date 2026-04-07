/// WASM backend for the Aver compiler.
///
/// Transforms Aver AST -> WASM binary module (.wasm).
/// Uses native WASM types: Int → i64, Bool → i64 (0/1), Float → f64 bits in i64.
/// Pure programs need zero runtime — arithmetic is native WASM instructions.
mod emitter;
mod expr;
mod runtime;
mod types;
pub(crate) mod value;

use crate::codegen::CodegenContext;

/// Emit a WASM binary module from the Aver codegen context.
pub fn emit_wasm(ctx: &CodegenContext) -> Result<Vec<u8>, String> {
    emitter::build_wasm_module(ctx)
}

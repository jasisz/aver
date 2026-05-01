//! WebAssembly GC + tail-call backend.
//!
//! Parallel to `src/codegen/wasm/` until bench numbers decide which one
//! survives 0.16. See `README.md` for the design notes (why this exists,
//! type representation, what's deliberately out of scope).
//!
//! Status: Phase 1 — module scaffold. `compile_to_wasm_gc` returns a
//! placeholder error until a hello-world emitter lands in `module.rs`.

use crate::ast::TopLevel;
use crate::ir::AnalysisResult;

mod body;
mod builtins;
mod effects;
mod maps;
mod module;
#[cfg(test)]
mod tests;
mod types;

#[derive(Debug)]
pub enum WasmGcError {
    Unimplemented(&'static str),
    Validation(String),
}

impl std::fmt::Display for WasmGcError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unimplemented(what) => write!(f, "wasm-gc: not implemented yet — {what}"),
            Self::Validation(msg) => write!(f, "wasm-gc: validation failed — {msg}"),
        }
    }
}

impl std::error::Error for WasmGcError {}

/// Compile post-pipeline IR (`items`) to a WebAssembly GC module.
///
/// Returns the binary `.wasm` bytes on success. The module imports
/// nothing by default — Phase 1 only handles `fn main() -> Int`. Larger
/// shapes (effects, WASI bridging) come in later phases or get cut as
/// out of scope for the probe.
pub fn compile_to_wasm_gc(
    items: &[TopLevel],
    _analysis: Option<&AnalysisResult>,
) -> Result<Vec<u8>, WasmGcError> {
    module::emit_module(items)
}

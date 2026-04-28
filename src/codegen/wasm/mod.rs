/// WASM backend for the Aver compiler.
///
/// Transforms Aver AST → WASM binary module (.wasm).
/// Uses native WASM types: Int → i64, Float → f64, Bool → i32, heap types → i32 ptr.
///
/// Default output uses `aver/*` import ABI — host provides effect implementations.
/// `--adapter wasi` mode emits WASI imports for standalone wasmtime execution.
pub(crate) mod abi;
mod emitter;
mod expr;
mod runtime;
mod types;
pub(crate) mod value;

pub use runtime::{build_aver_to_wasi_wasm, build_runtime_wasm};

use crate::codegen::CodegenContext;

/// Which import ABI to emit in the WASM module.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum WasmAdapter {
    /// Default: `aver/*` imports. Requires a host that provides capabilities.
    #[default]
    Aver,
    /// Compatibility: `wasi_snapshot_preview1` imports. Works with standalone wasmtime.
    Wasi,
    /// Fetch-style JS host (Cloudflare Workers, Deno, Bun, …).
    /// `aver/*` imports stay the same shape, but field access on
    /// `HttpRequest` and construction of `HttpResponse` lower to
    /// host calls (`request_method/url/body`, `response_text`)
    /// instead of OBJ_RECORD reads/writes — the host owns the
    /// actual request/response objects, the guest never deref's
    /// the opaque handles.
    Fetch,
}

/// Emit a WASM binary module from the Aver codegen context.
pub fn emit_wasm(ctx: &CodegenContext) -> Result<Vec<u8>, String> {
    emit_wasm_with_adapter(ctx, WasmAdapter::default(), None)
}

/// Emit a WASM binary module with the specified import ABI adapter
/// and an optional HTTP handler — when `handler` is `Some(name)`,
/// the named top-level fn is also exported as `aver_http_handle`
/// for fetch-style deployment packs (Cloudflare Workers, Fastly
/// Compute) that route requests through it.
pub fn emit_wasm_with_adapter(
    ctx: &CodegenContext,
    adapter: WasmAdapter,
    handler: Option<&str>,
) -> Result<Vec<u8>, String> {
    emitter::build_wasm_module(ctx, adapter, handler)
}

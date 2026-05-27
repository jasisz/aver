//! WebAssembly GC + tail-call backend.
//!
//! Parallel to `src/codegen/wasm/` until bench numbers decide which one
//! survives 0.16. See `README.md` for the design notes (why this exists,
//! type representation, what's deliberately out of scope).
//!
//! Status: Phase 1 — module scaffold. `compile_to_wasm_gc` returns a
//! placeholder error until a hello-world emitter lands in `module.rs`.
//!
//! ## Phase E (resolved AST) migration plan
//!
//! Issue #147 phase E migrates every backend from consuming
//! `crate::ast::Expr` / `FnBody` / `Pattern` to consuming the
//! resolved-HIR shapes in `crate::ir::hir`. The wasm-gc backend is
//! migrated across three PRs because it's the largest backend
//! (~36k lines, 8 files actively walking `Expr`):
//!
//! - **PR 9** (foundation) — shared `CodegenContext::resolve_fn_def` /
//!   `resolve_expr` / `resolve_stmt` / `resolve_pattern` methods
//!   promoted from `rust/toplevel.rs`. No wasm-gc behaviour change;
//!   gives the follow-up PRs a one-line lookup boundary.
//! - **PR 9.1** — `body/emit.rs` (3.1k) migrates to take
//!   `&Spanned<ResolvedExpr>`; cascades through `emit_list_literal`,
//!   `emit_map_literal`, `emit_constructor`, `emit_match`,
//!   `emit_tail_call`, etc. The remaining body subfiles
//!   (`body/{slots, builtins, builtins_wasip2, infer, eq_helpers,
//!   hash_helpers}.rs`) migrate alongside since they share the
//!   `Spanned<Expr>` borrows.
//! - **PR 9.2** — `module.rs` orchestration (6.3k),
//!   `types_discovery.rs` / `types.rs` / `flatten.rs` (program-level
//!   walkers that register types / fns before emit). After this PR
//!   the wasm-gc backend reads `ctx.resolved_fn_defs` /
//!   `resolved_module_fn_defs` exclusively; the `&FnDef` borrows in
//!   `module.rs` survive only as the source-shape metadata that the
//!   resolver doesn't reproduce (e.g. param annotation source
//!   strings used by the WIT signature emitter).
//!
//! The byte-identical gate for every wasm-gc snapshot
//! (`order_total.wasm` md5 `61e45b325279e17e1b0d8dce3fde43f9`, the
//! game suite, the wasip2 stress fixtures) holds across each PR — no
//! generated wasm shifts until the migration is fully through.

use crate::ast::TopLevel;
use crate::ir::AnalysisResult;

mod body;
mod builtins;
mod effects;
mod flatten;
mod lists;
mod maps;
mod module;
#[cfg(test)]
mod tests;
mod types;
mod types_discovery;
mod wasip2_helpers;
mod wasip2_http;
mod wasip2_http_server;
mod wasip2_imports;
mod wasip2_tcp;
mod wat_helper;

pub use flatten::flatten_multimodule;

/// Backend lowering mode — selects the host-bridge shape the wasm-gc
/// emitter targets.
///
/// `AverBridge` is the original single-target output: `aver/*` host
/// imports (e.g. `("aver", "console_print")`), `_start: () -> ()`
/// export, JS-host-visible `__rt_string_*` exports. Used by browsers,
/// Cloudflare Workers, embedded wasmtime via `aver run --wasm-gc`.
///
/// `Wasip2` is direct WIT lowering for `--target wasip2`: canonical-
/// ABI imports of `wasi:cli/*` and `wasi:io/streams`, an export
/// named `wasi:cli/run@0.2.4#run` with `() -> i32`, internal-only
/// linear memory. The wasm-gc core wrapped via
/// `wit_component::ComponentEncoder` becomes a WASI 0.2 component;
/// no preview-1 adapter, no compatibility bridge. See `docs/wasip2.md`
/// and `feedback_aver_no_preview1_adapter` for the architectural
/// decision.
///
/// Phase 1.2b1 of 0.18 "Span" introduces this enum as the single
/// hinge: every per-target branching call inside the wasm-gc
/// emitter consults it. Earlier phases shipped only `AverBridge`
/// (single-flavor output).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TargetMode {
    AverBridge,
    Wasip2,
}

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

/// Compile post-pipeline IR (`items`) to a WebAssembly GC module
/// in the `AverBridge` target shape — `aver/*` host imports + the
/// `__rt_*` JS bridge + `_start` export. This is the path used by
/// `--target wasm-gc` (browsers, Workers, embedded wasmtime via
/// `aver run --wasm-gc`).
pub fn compile_to_wasm_gc(
    items: &[TopLevel],
    _analysis: Option<&AnalysisResult>,
) -> Result<Vec<u8>, WasmGcError> {
    module::emit_module_with(items, None, TargetMode::AverBridge)
}

/// Same as `compile_to_wasm_gc` but exports a JS-callable
/// `aver_http_handle(method, url, query, body, country) ->
/// (status, body)` wrapper around the named user fn (whose
/// signature must be `(HttpRequest) -> HttpResponse`). Equivalent
/// to the legacy backend's `--bridge fetch`.
pub fn compile_to_wasm_gc_with_handler(
    items: &[TopLevel],
    _analysis: Option<&AnalysisResult>,
    handler: Option<&str>,
) -> Result<Vec<u8>, WasmGcError> {
    module::emit_module_with(items, handler, TargetMode::AverBridge)
}

/// Compile post-pipeline IR (`items`) to a WebAssembly GC module
/// in the `Wasip2` target shape — canonical-ABI imports for
/// `wasi:cli/*` and `wasi:io/streams`, `wasi:cli/run@0.2.4#run`
/// export with `() -> i32`. The returned core bytes are intended
/// to be wrapped by `wit_component::ComponentEncoder` into a
/// `.component.wasm` (handled by `src/codegen/wasip2/wrap.rs`).
///
/// Phase 1.2b1 of 0.18 "Span" — at this commit the entry point
/// exists but `module::emit_module_with` does not yet branch on
/// the target, so the produced bytes are identical to
/// `compile_to_wasm_gc`. Subsequent commits in this phase wire
/// the actual canonical-ABI shape.
pub fn compile_to_wasm_gc_for_wasip2(
    items: &[TopLevel],
    _analysis: Option<&AnalysisResult>,
) -> Result<Vec<u8>, WasmGcError> {
    module::emit_module_with(items, None, TargetMode::Wasip2)
}

/// `--target wasip2 --world wasi:http/proxy` entry — Phase 3 / 0.19.
/// `handler` names the user-source fn `(HttpRequest) -> HttpResponse`
/// reachable from `main`'s trailing `HttpServer.listen(port, handler)`
/// call (extracted upstream by the CLI). The wasm-gc emitter swaps
/// the `wasi:cli/run` entry-point for `wasi:http/incoming-handler#
/// handle`, decodes the host-supplied incoming-request resource into
/// an Aver `HttpRequest`, runs `handler`, encodes the returned
/// `HttpResponse` into an outgoing-response, and calls
/// `response-outparam.set` — the host (`wasmtime serve` / Spin /
/// wasmCloud) then writes the response bytes back to the client.
///
/// `main` still gets emitted as a regular wasm fn but is never
/// invoked at runtime (the proxy world has no `_start`); the
/// `HttpServer.listen` call inside it lowers to a no-op so body
/// validation passes. The `port` arg is ignored at codegen time —
/// the host's listener flag (`--http=:N`) decides the bind socket.
pub fn compile_to_wasm_gc_for_wasip2_with_handler(
    items: &[TopLevel],
    _analysis: Option<&AnalysisResult>,
    handler: &str,
) -> Result<Vec<u8>, WasmGcError> {
    module::emit_module_with(items, Some(handler), TargetMode::Wasip2)
}

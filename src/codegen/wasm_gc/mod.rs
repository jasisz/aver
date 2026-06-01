//! WebAssembly GC + tail-call backend.
//!
//! Parallel to `src/codegen/wasm/` until bench numbers decide which one
//! survives 0.16. See `README.md` for the design notes (why this exists,
//! type representation, what's deliberately out of scope).
//!
//! Status: Phase 1 — module scaffold. `compile_to_wasm_gc` returns a
//! placeholder error until a hello-world emitter lands in `module.rs`.
//!
//! ## Phase E (resolved AST) migration — shipped
//!
//! Issue #147 phase E migrates every backend from consuming
//! `crate::ast::Expr` / `FnBody` / `Pattern` to consuming the
//! resolved-HIR shapes in `crate::ir::hir`. The wasm-gc backend
//! shipped across five PRs:
//!
//! - **PR 9.0.1** — `body/infer.rs` type readers generic over
//!   `Spanned<T: Debug>` so they work uniformly against
//!   `Spanned<Expr>` and `Spanned<ResolvedExpr>`.
//! - **PR 9** (foundation) — shared `CodegenContext::resolve_fn_def`
//!   / `resolve_expr` / `resolve_stmt` / `resolve_pattern` methods.
//!   Behaviour-neutral one-line lookup boundary for follow-ups.
//! - **PR 9.1** — `body/emit.rs` (+ `body/{slots, builtins,
//!   builtins_wasip2}.rs`) take `&Spanned<ResolvedExpr>`. The
//!   `Expr::FnCall(Attr(Ident("X"), member), args)` recognition
//!   tree collapses to `ResolvedCallee` / `ResolvedCtor` dispatch.
//! - **PR 9.2** — `types_discovery.rs` / `types.rs` / `module.rs`
//!   program-level walkers consume `&ResolvedFnDef` /
//!   `&Spanned<ResolvedExpr>`. After this PR every walker /
//!   emitter reads identity off the resolved HIR; the source-shape
//!   `&FnDef` borrows survive only for signature metadata
//!   (`fd.return_type` / `fd.params` as source strings, kept
//!   verbatim for WIT signature emission and param-annotation
//!   passthrough).
//! - **PR 9.3a** — `CodegenContext::resolve_fn_def` keys lookup by
//!   `FnKey`/`FnId` via `SymbolTable::fn_id_of`, not bare name.
//!   Closes the bare-name-flat-search smell that worked only
//!   because `flatten_multimodule` prefixes dep fn names.
//! - **PR 9.3b** — [`view::WasmGcLinkedView`] promotes the post-
//!   flatten resolver rebuild to a first-class backend-link stage.
//!   The pipeline stays module-aware; wasm-gc owns its single-
//!   module link view as a legal codegen concept, not a transitional
//!   hack. `fn_def_by_id(FnId)` mirrors the FnId-keyed pattern
//!   from PR 9.3a.
//! - **PR 9.3c** — [`body::FnMap`] keyed by `FnId` only (was
//!   `by_name: HashMap<String, FnEntry>` pre-9.3c). `Call(Fn(id))`
//!   and `ResolvedExpr::TailCall { target }` dispatch through
//!   `fn_map.by_id.get(&id)` so call lowering matches the FnId-
//!   keyed identity layer end to end.
//!
//! The byte-identical gate for every wasm-gc snapshot
//! (`order_total.wasm` md5 `61e45b325279e17e1b0d8dce3fde43f9`, the
//! game suite, the wasip2 stress fixtures) held across each PR.

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
mod view;
mod wasip2_helpers;
mod wasip2_http;
mod wasip2_http_server;
mod wasip2_imports;
mod wasip2_tcp;
mod wat_helper;

pub use body::{CoverageReport, coverage_report};
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
///
/// # Epic #170 Phase 6 contract — input shape
///
/// **Input is post-pipeline AST `&[TopLevel]`, not
/// `&ResolvedProgramView`**, and this is intentional. The wasm-gc
/// backend runs its own [`backend-link-stage`](self::view): a
/// `flatten_multimodule` pass collapses dep modules into the entry
/// items with module-prefixed names (`Fractal.render` →
/// `Fractal_render`), then [`WasmGcLinkedView`] runs a fresh resolver
/// pass against the flattened slice. The pre-link `ResolvedProgramView`
/// from the pipeline keyed against the pre-flatten `SymbolTable`
/// doesn't match the post-link namespace, so this backend builds
/// its own post-link view internally rather than consuming the
/// shared one.
///
/// **What this means for identity safety**: post-flatten lookups
/// inside the backend go through `WasmGcLinkedView::fn_def_by_id`
/// (opaque `FnId`), not bare-name walks over the flattened items.
/// Cross-module same-bare-name fns get distinct `FnId`s by virtue
/// of the prefix-mangle, and the link view's resolver pass
/// canonicalises them.
///
/// [`WasmGcLinkedView`]: self::view::WasmGcLinkedView
pub fn compile_to_wasm_gc(
    items: &[TopLevel],
    _analysis: Option<&AnalysisResult>,
) -> Result<Vec<u8>, WasmGcError> {
    module::emit_module_with(items, None, TargetMode::AverBridge, true).map(|(bytes, _)| bytes)
}

/// `compile_to_wasm_gc` with an explicit MIR toggle.
/// `enable_mir == true` is the production path (`compile_to_wasm_gc`):
/// the MIR body emitter runs per-fn with a `ResolvedExpr` fallback.
/// `enable_mir == false` forces the `ResolvedExpr` emitter everywhere.
/// Returns `(module_bytes, mir_emitted_fn_count)` — the count is how
/// many fns the MIR body emitter actually rendered (the real
/// `emit_fn_body_via_mir` decision, not the structural coverage
/// predicate). The byte-differential test compiles both ways and
/// asserts the bytes are identical (proving the MIR walk is byte-for-
/// byte faithful) and that the count is non-zero (proving the MIR path
/// was actually exercised, so byte-identity isn't vacuous).
#[doc(hidden)]
pub fn compile_to_wasm_gc_mir_toggle(
    items: &[TopLevel],
    _analysis: Option<&AnalysisResult>,
    enable_mir: bool,
) -> Result<(Vec<u8>, usize), WasmGcError> {
    module::emit_module_with(items, None, TargetMode::AverBridge, enable_mir)
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
    module::emit_module_with(items, handler, TargetMode::AverBridge, true).map(|(bytes, _)| bytes)
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
    module::emit_module_with(items, None, TargetMode::Wasip2, true).map(|(bytes, _)| bytes)
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
    module::emit_module_with(items, Some(handler), TargetMode::Wasip2, true).map(|(bytes, _)| bytes)
}

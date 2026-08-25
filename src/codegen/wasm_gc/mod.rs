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

use std::collections::HashMap;

use crate::ast::TopLevel;
use crate::ir::AnalysisResult;

mod body;
mod builtins;
pub(crate) mod effects;
mod flatten;
mod lists;
mod maps;
mod module;
mod packed_sequences;
#[cfg(test)]
mod tests;
mod types;
pub(crate) use types::{OPTION_SOME_TAG, RESULT_OK_TAG};
mod types_discovery;
mod view;
mod wasip2_capability_imports;
mod wasip2_disk_bytes;
mod wasip2_disk_read_at;
mod wasip2_helpers;
mod wasip2_http;
mod wasip2_http_handler;
mod wasip2_imports;
mod wasip2_tcp;
mod wat_helper;

pub use body::{CoverageReport, coverage_report};
pub use flatten::{CapabilityFunctionSurface, flatten_multimodule};

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

#[derive(Debug)]
pub struct WasmGcCompileOutput {
    pub bytes: Vec<u8>,
    pub mir_count: usize,
    pub fragment_plans: Vec<crate::codegen::cert::FragmentPlanArtifact>,
}

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
    module::emit_module_with(
        items,
        None,
        TargetMode::AverBridge,
        &HashMap::new(),
        None,
        true,
    )
    .map(|(bytes, _, _)| bytes)
}

/// Compile a FLATTENED multi-module program, threading the identity-
/// preserving qualified type-name aliases that [`flatten_multimodule`]
/// returned. Entry-side local-binding annotations may spell a dep type
/// qualified (`o: Dep.Octets`); the pre-flatten typechecker's stamps keep
/// that spelling into codegen, and the wasm-gc registry resolves it to the
/// sole-declarer dep type's proof-derived layout facts through these
/// aliases. Callers that did not flatten (single-module programs) should
/// keep using the plain `compile_to_wasm_gc*` entries — those compile with
/// an empty alias map.
pub fn compile_to_wasm_gc_flattened(
    items: &[TopLevel],
    _analysis: Option<&AnalysisResult>,
    handler: Option<&str>,
    target: TargetMode,
    type_aliases: &HashMap<String, String>,
) -> Result<WasmGcCompileOutput, WasmGcError> {
    compile_to_wasm_gc_flattened_with_options(items, _analysis, handler, target, type_aliases, true)
}

/// Internal differential hook for exercising the representation-independent
/// boxed `List<Int>` lowering without a process-global environment switch.
#[doc(hidden)]
pub fn compile_to_wasm_gc_flattened_with_options(
    items: &[TopLevel],
    _analysis: Option<&AnalysisResult>,
    handler: Option<&str>,
    target: TargetMode,
    type_aliases: &HashMap<String, String>,
    packed_sequences_enabled: bool,
) -> Result<WasmGcCompileOutput, WasmGcError> {
    module::emit_module_with(
        items,
        handler,
        target,
        type_aliases,
        None,
        packed_sequences_enabled,
    )
    .map(|(bytes, mir_count, fragment_plans)| WasmGcCompileOutput {
        bytes,
        mir_count,
        fragment_plans,
    })
}

/// Compile a flattened wasip2 core whose custom imports and bridge
/// helpers are driven by the same plan used for WIT metadata.
pub fn compile_to_wasm_gc_flattened_with_capabilities(
    items: &[TopLevel],
    _analysis: Option<&AnalysisResult>,
    handler: Option<&str>,
    type_aliases: &HashMap<String, String>,
    capabilities: &crate::codegen::wasip2::CapabilityWitPlan,
) -> Result<WasmGcCompileOutput, WasmGcError> {
    module::emit_module_with(
        items,
        handler,
        TargetMode::Wasip2,
        type_aliases,
        Some(capabilities),
        true,
    )
    .map(|(bytes, mir_count, fragment_plans)| WasmGcCompileOutput {
        bytes,
        mir_count,
        fragment_plans,
    })
}

/// `compile_to_wasm_gc` returning the MIR-coverage count alongside the
/// module bytes. MIR is the only codegen path; the count is how many
/// fns the MIR body emitter actually rendered (the real
/// `emit_fn_body_via_mir` Some/None decision, not the structural
/// coverage predicate). Fns it doesn't cover get an `unreachable`
/// trap-stub body. The single-file differential test uses this to hold
/// a coverage floor — a covered construct silently regressing to a trap
/// stub drops the count below the floor and fails CI.
#[doc(hidden)]
pub fn compile_to_wasm_gc_with_mir_count(
    items: &[TopLevel],
    _analysis: Option<&AnalysisResult>,
) -> Result<(Vec<u8>, usize), WasmGcError> {
    module::emit_module_with(
        items,
        None,
        TargetMode::AverBridge,
        &HashMap::new(),
        None,
        true,
    )
    .map(|(bytes, mir_count, _)| (bytes, mir_count))
}

/// Same as `compile_to_wasm_gc` but exports a JS-callable
/// `aver_http_handle(method, url, query, body, country) ->
/// (status, body)` wrapper around the named user fn (whose
/// signature must be `(HttpRequest) -> Http.Response`). Equivalent
/// to the legacy backend's `--bridge fetch`.
pub fn compile_to_wasm_gc_with_handler(
    items: &[TopLevel],
    _analysis: Option<&AnalysisResult>,
    handler: Option<&str>,
) -> Result<Vec<u8>, WasmGcError> {
    module::emit_module_with(
        items,
        handler,
        TargetMode::AverBridge,
        &HashMap::new(),
        None,
        true,
    )
    .map(|(bytes, _, _)| bytes)
}

/// Same bytes as [`compile_to_wasm_gc_with_handler`], plus the expression
/// fragment plans that were actually used to emit certified plan-first islands.
pub fn compile_to_wasm_gc_with_handler_and_cert_plans(
    items: &[TopLevel],
    _analysis: Option<&AnalysisResult>,
    handler: Option<&str>,
) -> Result<WasmGcCompileOutput, WasmGcError> {
    module::emit_module_with(
        items,
        handler,
        TargetMode::AverBridge,
        &HashMap::new(),
        None,
        true,
    )
    .map(|(bytes, mir_count, fragment_plans)| WasmGcCompileOutput {
        bytes,
        mir_count,
        fragment_plans,
    })
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
    module::emit_module_with(items, None, TargetMode::Wasip2, &HashMap::new(), None, true)
        .map(|(bytes, _, _)| bytes)
}

/// `--target wasip2 --world wasi:http/proxy` entry — Phase 3 / 0.19.
/// `handler` names the user-source fn `(HttpRequest) -> Http.Response`
/// explicitly selected by the CLI. The wasm-gc emitter swaps
/// the `wasi:cli/run` entry-point for `wasi:http/incoming-handler#
/// handle`, decodes the host-supplied incoming-request resource into
/// an Aver `HttpRequest`, runs `handler`, encodes the returned
/// `Http.Response` into an outgoing-response, and calls
/// `response-outparam.set` — the host (`wasmtime serve` / Spin /
/// wasmCloud) then writes the response bytes back to the client.
///
/// `main` still gets emitted as a regular wasm fn but is never
/// invoked at runtime (the proxy world has no `_start`). The host's
/// listener configuration decides the bind socket.
pub fn compile_to_wasm_gc_for_wasip2_with_handler(
    items: &[TopLevel],
    _analysis: Option<&AnalysisResult>,
    handler: &str,
) -> Result<Vec<u8>, WasmGcError> {
    module::emit_module_with(
        items,
        Some(handler),
        TargetMode::Wasip2,
        &HashMap::new(),
        None,
        true,
    )
    .map(|(bytes, _, _)| bytes)
}

/// True when `bytes` names its map insert helpers — the `name` section
/// the emitter writes for a program that instantiates a `Map`, so that
/// a stop inside one is attributable in the backtrace instead of
/// reading `<wasm function N>` (see `maps.rs`).
///
/// Read by the `--optimize` path, which is about to destroy exactly
/// that: `wasm-opt` drops the section, and inlines the helper besides.
/// Malformed bytes answer "no names" — this decides whether to print a
/// note, and the real decoders report bad input with a better message.
pub fn carries_capacity_helper_names(bytes: &[u8]) -> bool {
    use wasmparser::{KnownCustom, Name, Parser, Payload};
    for payload in Parser::new(0).parse_all(bytes).flatten() {
        let Payload::CustomSection(section) = payload else {
            continue;
        };
        let KnownCustom::Name(names) = section.as_known() else {
            continue;
        };
        for subsection in names.flatten() {
            let Name::Function(map) = subsection else {
                continue;
            };
            for naming in map.into_iter().flatten() {
                if naming.name.starts_with(maps::CAPACITY_HELPER_NAME_PREFIX) {
                    return true;
                }
            }
        }
    }
    false
}

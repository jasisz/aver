//! `--target wasip2` POC — proves the direct-WIT-lowering toolchain:
//!
//!   wat → wasm-encoder core module
//!     → embed `component-type:<world>` metadata custom section
//!       (encoded via `wit-component::metadata` from a `wit-parser::Resolve`
//!        we build out of an Aver-emitted WIT source string)
//!     → `wit-component::ComponentEncoder::module(...).encode()`
//!       (no preview-1 adapter)
//!     → wasmparser validation as a Component Model artifact.
//!
//! Three cases:
//!
//! 1. `pipeline_smoke_no_effects` — minimal core wasm with no
//!    imports/exports beyond `_start`, wrapped via the public
//!    `wasip2::compile_to_component` API. Confirms the metadata-
//!    based wrap path lines up under the versions pinned in
//!    `Cargo.toml`. Empty world (`aver:user/command {}`) for now;
//!    Phase 1.2 grows this to `include wasi:cli/command;` once
//!    WASI WIT bundles are wired.
//!
//! 2. `gc_types_inside_core_validate_at_component_boundary` —
//!    same wrap path, but the inner core module declares a
//!    `(struct (field $val (mut i32)))` rec-group type and uses
//!    `struct.new` / `struct.get` / `struct.set` from within
//!    `_start`. The component boundary stays canonical (no GC ref
//!    crosses out). Pins decision `Wasip2ComponentTarget` from
//!    `decisions/architecture.av`: per-instantiation Map/List/
//!    Vector helpers can stay inside the user core module while
//!    the public surface stays WIT-canonical.
//!
//! 3. `rejects_http_proxy_world_in_phase_1` — pins the Phase 3 /
//!    0.19 reject path so flipping it later is a visible diff.
//!
//! These tests do NOT exercise the Aver compiler. Their job is to
//! confirm the `wit-component` + `wit-parser` + `wasmparser` triple
//! holds up under direct-WIT lowering, before Phase 1.2 starts
//! mapping Aver effects onto WIT/WASI imports.

#![cfg(feature = "wasip2")]

use aver::codegen::wasip2;

fn validate_component(bytes: &[u8]) {
    let mut validator =
        wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::all());
    validator
        .validate_all(bytes)
        .expect("wasmparser must accept the produced component");
    assert!(!bytes.is_empty(), "component must be non-empty");
}

#[test]
fn pipeline_smoke_no_effects() {
    // Trivial core: empty `_start`, nothing else. No imports, no
    // GC types, no LM accesses. Smallest possible "this is a wasm
    // module" the wrap path has to handle.
    let core_wasm = wat::parse_str(
        r#"
(module
  (func (export "_start")
    nop))
"#,
    )
    .expect("wat parses");

    let (component, wit) =
        wasip2::compile_to_component(&core_wasm, wasip2::Wasip2World::CliCommand)
            .expect("compile_to_component should succeed for an empty CliCommand world");

    validate_component(&component);

    assert!(
        wit.contains("package aver:user"),
        "WIT carries our generated package: {wit}"
    );
    assert!(
        wit.contains("world command"),
        "WIT declares the local world identifier: {wit}"
    );
}

#[test]
fn gc_types_inside_core_validate_at_component_boundary() {
    // Core module that USES GC types internally. The exported
    // entrypoint takes/returns canonical types only — so the
    // component boundary never sees the GC ref. Mirrors how a
    // real `aver compile --target wasip2` artifact will look:
    // per-instantiation `Map<K,V>` / `List<T>` / records live
    // inside the core, public surface stays WIT-canonical.
    let core_wasm = wat::parse_str(
        r#"
(module
  (rec
    (type $box (struct (field $val (mut i32)))))
  (func (export "_start")
    (local $b (ref $box))
    i32.const 42
    struct.new $box
    local.set $b
    local.get $b
    local.get $b
    struct.get $box $val
    i32.const 1
    i32.add
    struct.set $box $val
    nop))
"#,
    )
    .expect("wat parses (with GC types)");

    let (component, _wit) =
        wasip2::compile_to_component(&core_wasm, wasip2::Wasip2World::CliCommand)
            .expect("GC types inside core must validate when the boundary stays canonical");

    validate_component(&component);
}

#[test]
fn rejects_http_proxy_world_in_phase_1() {
    // Phase 3 / 0.19 — `wasi:http/proxy` is rejected up front so
    // the failure mode is clear (NotImplemented, not Wrap). Tracks
    // decision in `decisions/architecture.av::Wasip2ComponentTarget`
    // and the phasing in `docs/wasip2.md`.
    let core_wasm = wat::parse_str(r#"(module (func (export "_start")))"#)
        .expect("trivial wat parses");

    let err = wasip2::compile_to_component(&core_wasm, wasip2::Wasip2World::HttpProxy)
        .expect_err("HttpProxy world should be rejected in 0.18 Phase 1");

    let msg = format!("{err}");
    assert!(
        msg.contains("not yet implemented"),
        "rejection mentions Phase 3 status: {msg}"
    );
    assert!(
        msg.contains("wasi:http/proxy"),
        "error names the offending world: {msg}"
    );
}

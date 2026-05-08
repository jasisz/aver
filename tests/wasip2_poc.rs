//! `--target wasip2` POC — proves the toolchain stack:
//!
//!   wat → wasm-encoder core module
//!     → wit_component::ComponentEncoder + preview1 adapter
//!       → wasmparser validation as a Component Model artifact.
//!
//! Two cases:
//!   1. Plain core wasm with a `wasi_snapshot_preview1::fd_write`
//!      import — the canonical "hello world" against the COMMAND
//!      adapter (wasi:cli/command world).
//!   2. Same shape but the core module declares and uses GC types
//!      (struct.new / struct.get) inside the body. The component
//!      boundary stays canonical (i32 in / i32 out); the GC types
//!      live entirely inside the user module. Pins the architectural
//!      assumption from `decisions/architecture.av`
//!      (`Wasip2ComponentTarget`).
//!
//! These tests are POC-only: they do NOT exercise the Aver
//! compiler. Their job is to confirm the wit-component +
//! wasi-preview1-component-adapter-provider + wasmparser
//! triple lines up under the versions pinned in `Cargo.toml`,
//! before Phase 1 of 0.18 "Span" lands compiler-side wiring.

#![cfg(feature = "wasip2")]

use wasi_preview1_component_adapter_provider::{
    WASI_SNAPSHOT_PREVIEW1_ADAPTER_NAME, WASI_SNAPSHOT_PREVIEW1_COMMAND_ADAPTER,
};
use wit_component::ComponentEncoder;

fn validate_component(bytes: &[u8]) {
    let mut validator =
        wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::all());
    validator
        .validate_all(bytes)
        .expect("wasmparser must accept the produced component");
    assert!(!bytes.is_empty(), "component must be non-empty");
}

fn wrap_as_component(core_wasm: &[u8]) -> Vec<u8> {
    ComponentEncoder::default()
        .module(core_wasm)
        .expect("ComponentEncoder accepts the core module")
        .validate(true)
        .adapter(
            WASI_SNAPSHOT_PREVIEW1_ADAPTER_NAME,
            WASI_SNAPSHOT_PREVIEW1_COMMAND_ADAPTER,
        )
        .expect("preview1 command adapter attaches")
        .encode()
        .expect("ComponentEncoder produces a component")
}

#[test]
fn wasip2_poc_pipeline_smoke_hello_world() {
    let wat_source = r#"
(module
  (import "wasi_snapshot_preview1" "fd_write"
    (func $fd_write (param i32 i32 i32 i32) (result i32)))
  (memory (export "memory") 1)
  (data (i32.const 0) "Hello, wasip2!\n")
  (func (export "_start")
    ;; iov[0].iov_base = 0
    i32.const 32
    i32.const 0
    i32.store
    ;; iov[0].iov_len = 15
    i32.const 36
    i32.const 15
    i32.store
    ;; fd_write(fd=1, iovs=32, iovs_len=1, nwritten=40)
    i32.const 1
    i32.const 32
    i32.const 1
    i32.const 40
    call $fd_write
    drop))
"#;
    let core_wasm = wat::parse_str(wat_source).expect("wat parses");
    let component = wrap_as_component(&core_wasm);
    validate_component(&component);
}

#[test]
fn wasip2_poc_gc_types_inside_core_validate_at_component_boundary() {
    // Core module that USES GC types internally. The exported
    // entrypoint takes/returns canonical types only — so the
    // component boundary never sees the GC ref. Mirrors how a
    // real `aver compile --target wasip2` artifact will look:
    // per-instantiation `Map<K,V>` / `List<T>` / records live
    // inside the core, public surface stays WIT-canonical.
    let wat_source = r#"
(module
  (rec
    (type $box (struct (field $val (mut i32)))))
  (import "wasi_snapshot_preview1" "fd_write"
    (func $fd_write (param i32 i32 i32 i32) (result i32)))
  (memory (export "memory") 1)
  (func (export "_start")
    (local $b (ref $box))
    ;; b = struct.new $box (i32.const 42)
    i32.const 42
    struct.new $box
    local.set $b
    ;; b.val = b.val + 1
    local.get $b
    local.get $b
    struct.get $box $val
    i32.const 1
    i32.add
    struct.set $box $val
    ;; drop the GC ref (held by local), do not return it
    nop))
"#;
    let core_wasm = wat::parse_str(wat_source).expect("wat parses (with GC types)");
    let component = wrap_as_component(&core_wasm);
    validate_component(&component);
}

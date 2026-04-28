//! WASM runtime entry point.
//!
//! Every runtime fn (alloc/heap GC, obj inspection, str/list/map/vec
//! ops, numeric formatting) lives in the WAT runtime under
//! `runtime/wat/*.part.wat` and is imported by user.wasm from the
//! `aver_runtime` module. This Rust crate just exposes the index
//! tables (`indices`) and the WAT → wasm bytes builder
//! (`wat_module`).
//!
//! Under `--target wasm --bridge wasi`, an additional shim module
//! (`runtime/wat/aver_to_wasi.wat`) translates `aver/*` host calls
//! into `wasi_snapshot_preview1.*` so the program runs standalone
//! under wasmtime.

mod indices;
mod wat_module;

use wasm_encoder::Function;

pub use indices::{
    AverRuntimeImports, RuntimeFuncIndices, emit_base_type_section, lookup_type_index,
    rt_type_index,
};
pub use wat_module::{build_aver_to_wasi_wasm, build_runtime_wasm};

/// Scratch area for IO in linear memory. Reserved: bytes 0-127.
/// Layout: [0..7] iovec, [8..11] nwritten, [16..37] int_buf,
///         [40] newline/scratch byte, [48..95] float_buf (48 bytes)
pub const IO_SCRATCH_SIZE: u32 = 128;
pub(crate) const IO_IOVEC: u32 = 0;
pub(crate) const IO_NWRITTEN: u32 = 8;
pub(crate) const IO_INT_BUF: u32 = 16;
pub const NEWLINE_ADDR: u32 = 40;
pub(crate) const IO_FLOAT_BUF: u32 = 48; // 48 bytes for float digits (48..95)

/// Emit local runtime function bodies. Always empty today — every
/// runtime fn lives in `aver_runtime` (imported). Kept as a hook for
/// any future Rust-side helper that prefers wasm-encoder over WAT.
pub fn emit_runtime_functions(_rt: &RuntimeFuncIndices) -> Vec<Function> {
    Vec::new()
}

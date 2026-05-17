//! Phase 4 (0.20) — `Tcp.*` helper emitters on `--target wasip2`.
//!
//! Organised by helper family:
//! - [`connect`] — `__rt_tcp_connect`: DNS resolve + create-socket +
//!   start/finish-connect + pool slot allocate + Tcp.Connection
//!   materialise pipeline (the big one, ~700 lines on its own).
//! - [`ids`]      — `__rt_tcp_format_id` / `__rt_tcp_parse_id` for
//!   the `"tcp-N"` slot-id round-trip.
//! - [`io`]       — `__rt_tcp_write_line` / `__rt_tcp_read_line`.
//! - [`lifecycle`] — `__rt_tcp_close` / `__rt_tcp_send` /
//!   `__rt_tcp_ping` (the resource-teardown + orchestrators).
//!
//! Per-helper Index / HelperFns bundles + the matching `emit_*`
//! functions are re-exported here so callers (`module.rs`,
//! `wasip2_tcp_wireup.rs`) get the same surface they had when this
//! module was a single 1945-line file.

use wasm_encoder::{Function, Instruction};

mod connect;
mod ids;
mod io;
mod lifecycle;
pub(in crate::codegen::wasm_gc) mod wireup;

pub(super) use connect::{
    TcpConnectDns, TcpConnectHelperFns, TcpConnectIndices, TcpConnectMaterialize, TcpConnectPool,
    TcpConnectSocket, emit_tcp_connect_stub,
};
pub(super) use ids::{emit_tcp_format_id, emit_tcp_parse_id};
pub(super) use io::{
    TcpReadLineHelperFns, TcpReadLineIndices, TcpWriteLineHelperFns, TcpWriteLineIndices,
    emit_tcp_read_line, emit_tcp_write_line,
};
pub(super) use lifecycle::{
    TcpCloseHelperFns, TcpCloseIndices, TcpPingHelperFns, TcpPingIndices, TcpSendHelperFns,
    TcpSendIndices, emit_tcp_close, emit_tcp_ping, emit_tcp_send,
};

/// Phase 4.2.2f — restore the `bump_alloc_ptr` global to the
/// cursor saved on helper entry. Inlined at every exit so the
/// helper's `cabi_realloc` calls reclaim their scratch on return.
/// Cheaper than a real fn (two i32 instructions), keeps the
/// rewind pattern visible at every callsite.
///
/// Visible to every submodule via `super::restore_bump`.
pub(super) fn restore_bump(f: &mut Function, l_saved_alloc: u32, bump_alloc_ptr_global: u32) {
    f.instruction(&Instruction::LocalGet(l_saved_alloc));
    f.instruction(&Instruction::GlobalSet(bump_alloc_ptr_global));
}

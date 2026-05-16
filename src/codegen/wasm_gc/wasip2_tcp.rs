//! Phase 4 (0.20) — `Tcp.*` helper emitters on `--target wasip2`.
//!
//! Currently ships `__rt_tcp_connect` with a partial body:
//!
//! - **Prolog (Phase 4.2.2a)** — lazy network handle init. The
//!   `network_handle: i32 = -1` global is checked; on first call the
//!   `wasi:sockets/instance-network.instance-network` import runs and
//!   the resulting resource handle lands in the global. Subsequent
//!   calls re-use the cached handle.
//! - **Body (Phase 4.2.1)** — still a STUB: ignores the host / port
//!   args and returns
//!   `Result.Err("tcp: connect not yet implemented")`.
//!
//! Real DNS resolve → create-tcp-socket → start/finish-connect →
//! pool-slot allocation → `Tcp.Connection` materialise pipeline
//! lands in follow-up phases 4.2.2b → 4.2.5.
//!
//! The stub exists to flush every wiring path end-to-end:
//! - `lowers_on_wasip2` + slot registration (Phase 4.1a)
//! - module globals + Wasip2Lowering field (Phase 4.1b)
//! - `effect_check` graduation
//! - call-site dispatch in `body/builtins.rs`
//! - helper fn allocation + body emit in `module.rs`
//!
//! With those wired, replacing the stub body in Phase 4.2.2+ is a
//! pure code-change inside this file — every consumer keeps working
//! and the test gate stays stable.

use wasm_encoder::{Function, Instruction, ValType};

/// Slot bundle for `__rt_tcp_connect`. Mirrors `HttpGetIndices`'s
/// shape — type idxs + helper fn idxs the body needs to resolve.
/// Phase 4.2.2a adds the network-handle global so the prolog can
/// lazy-init it; the stub body otherwise still ignores host / port.
pub(super) struct TcpConnectIndices {
    /// `(param (ref null $string)) (param i64) (result (ref null
    /// $result_tcp_connection_string))` function type idx.
    pub fn_type: u32,
    /// Allocated wasm fn idx — call-site emitters reference this
    /// when lowering `Tcp.connect`.
    pub fn_idx: u32,
    /// Wasm-gc type idx for `(array i8)` String repr — needed by
    /// the stub body to materialise the placeholder error message.
    pub string_type_idx: u32,
    /// Passive data segment idx carrying the bytes
    /// `"tcp: connect not yet implemented"`. The stub builds an Aver
    /// String from this via `array.new_data`. Replaced with the
    /// real per-failure-mode segments in Phase 4.2.2+.
    pub stub_err_segment_idx: u32,
    /// Length of the placeholder error bytes.
    pub stub_err_len: u32,
}

/// Helper fn idxs + global idxs the body calls. Phase 4.2.2a adds
/// the `instance-network` lazy fetch import plus the matching cache
/// global; Phase 4.2.1's Result.Err factory stays in for the
/// still-stubbed return path.
pub(super) struct TcpConnectHelperFns {
    /// `__rt_result_tcp_connection_string_err(message: ref string)
    /// -> ref Result<Tcp.Connection, String>`. The stub passes the
    /// placeholder error string straight through.
    pub result_err_fn: u32,
    /// `wasi:sockets/instance-network.instance-network: func() ->
    /// network`. Phase 4.2.2a calls this once per program; the
    /// resulting resource handle lands in `network_handle_global`.
    pub instance_network_fn: u32,
    /// `network_handle: i32 = -1` mutable global. -1 sentinel ⇒
    /// "not yet fetched"; the prolog runs `instance-network()` and
    /// stores the resulting resource handle here for program
    /// lifetime. Phase 4.2.2b onwards reads it back to thread into
    /// `resolve-addresses` / `start-connect`.
    pub network_handle_global: u32,
}

/// Emit the body for `__rt_tcp_connect`. Phase 4.2.2a layout:
///
/// 1. Lazy-init `network_handle` from
///    `wasi:sockets/instance-network.instance-network` on first call.
/// 2. STUB tail (Phase 4.2.1): ignore the (host, port) params, push
///    the placeholder error string, wrap via the Result.Err factory,
///    return.
///
/// The host / port args reach the locals (`local 0`, `local 1`) but
/// the body never reads them yet — that lands in 4.2.2b when the DNS
/// resolve loop replaces the stub tail.
pub(super) fn emit_tcp_connect_stub(
    indices: &TcpConnectIndices,
    helpers: &TcpConnectHelperFns,
) -> Function {
    // No locals beyond the two params. Params: 0 = host (ref string),
    // 1 = port (i64). Both unused today — wired in 4.2.2b+.
    let mut f = Function::new::<Vec<(u32, ValType)>>(Vec::new());

    // ── Phase 4.2.2a — lazy network handle init. ───────────────
    // Pattern mirrors Console.print's stdout cache: if the global
    // is still the -1 sentinel, fetch the host network resource
    // handle once and stash it. The resource is program-lifetime
    // (wasmtime cleans up at component exit), so no drop helper.
    f.instruction(&Instruction::GlobalGet(helpers.network_handle_global));
    f.instruction(&Instruction::I32Const(-1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Call(helpers.instance_network_fn));
    f.instruction(&Instruction::GlobalSet(helpers.network_handle_global));
    f.instruction(&Instruction::End);

    // ── Stub tail (Phase 4.2.1) — replace in 4.2.2b. ───────────
    // Push placeholder bytes onto the stack as a fresh Aver String:
    //   i32.const 0                 ; offset into the data segment
    //   i32.const stub_err_len      ; size in bytes
    //   array.new_data $string $seg
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(indices.stub_err_len as i32));
    f.instruction(&Instruction::ArrayNewData {
        array_type_index: indices.string_type_idx,
        array_data_index: indices.stub_err_segment_idx,
    });

    // Wrap in Result.Err via the factory. The factory takes the
    // String ref on the stack and returns a Result struct ref.
    f.instruction(&Instruction::Call(helpers.result_err_fn));
    f.instruction(&Instruction::End);
    f
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stub_emit_compiles() {
        // Smoke test — the body emitter must produce a well-formed
        // wasm Function with no panics on synthetic indices. Actual
        // wasm validation runs at module-emit time.
        let indices = TcpConnectIndices {
            fn_type: 0,
            fn_idx: 0,
            string_type_idx: 1,
            stub_err_segment_idx: 0,
            stub_err_len: b"tcp: connect not yet implemented".len() as u32,
        };
        let helpers = TcpConnectHelperFns {
            result_err_fn: 2,
            instance_network_fn: 3,
            network_handle_global: 0,
        };
        let _f = emit_tcp_connect_stub(&indices, &helpers);
    }
}

//! Phase 4 (0.20) — `Tcp.*` helper emitters on `--target wasip2`.
//!
//! Currently ships only `__rt_tcp_connect` as a STUB body: it ignores
//! the host / port args and returns
//! `Result.Err("tcp: connect not yet implemented")`. The real DNS
//! resolve → create-tcp-socket → start/finish-connect → pool-slot
//! allocation → `Tcp.Connection` materialise pipeline lands in
//! follow-up phases 4.2.2 → 4.2.5.
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
/// Phase 4.2.1 keeps the bundle minimal (string slot + result-Err
/// factory only) because the stub doesn't touch DNS / sockets yet.
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

/// Helper fn idxs the stub body calls. Phase 4.2.1 only needs the
/// Result.Err factory; later phases will grow this bundle with the
/// wasi:sockets imports, factories, pool helpers, etc.
pub(super) struct TcpConnectHelperFns {
    /// `__rt_result_tcp_connection_string_err(message: ref string)
    /// -> ref Result<Tcp.Connection, String>`. The stub passes the
    /// placeholder error string straight through.
    pub result_err_fn: u32,
}

/// Emit the stub body for `__rt_tcp_connect`. Throws away the
/// `host` and `port` params (the real pipeline will consume them in
/// Phase 4.2.2+), materialises the placeholder error string from
/// the passive data segment, and returns
/// `Result.Err("tcp: connect not yet implemented")`.
pub(super) fn emit_tcp_connect_stub(
    indices: &TcpConnectIndices,
    helpers: &TcpConnectHelperFns,
) -> Function {
    // No locals beyond the two params. Params: 0 = host (ref string),
    // 1 = port (i64). Both unused on the stub path — Aver runtime
    // never observes them because the call returns Err immediately.
    let mut f = Function::new::<Vec<(u32, ValType)>>(Vec::new());

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
        let helpers = TcpConnectHelperFns { result_err_fn: 2 };
        let _f = emit_tcp_connect_stub(&indices, &helpers);
    }
}

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
    /// Phase 4.2.2b — passive data segment + length for the bytes
    /// `"tcp: dns resolve failed"`. Returned via Result.Err when
    /// `resolve-addresses` itself fails (host syntactically invalid,
    /// resolver permanent failure). Per-error-code dispatch lands
    /// in a follow-up — the v1 generic message keeps the body small.
    pub dns_err_segment_idx: u32,
    pub dns_err_len: u32,
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
    /// Phase 4.2.2b — `cabi_realloc(old, old_sz, align, new_sz)`
    /// exported allocator. Used to grab the small retptr block for
    /// the `resolve-addresses` result.
    pub cabi_realloc_fn: u32,
    /// Phase 4.2.2b — `__rt_string_to_lm(s: ref string) -> i32`
    /// (length). Marshals the Aver String's bytes into LM[0..len];
    /// `resolve-addresses` reads the host name from that range.
    pub str_to_lm_fn: u32,
    /// Phase 4.2.2b — `wasi:sockets/ip-name-lookup.resolve-addresses:
    /// func(network: borrow<network>, name: string) -> result<
    ///   resolve-address-stream, error-code>`. Canonical-ABI
    /// signature: `(network, name_ptr, name_len, retptr) -> ()`;
    /// retptr is 8 bytes (tag@0, stream_handle/err_code@4).
    pub resolve_addresses_fn: u32,
    /// Phase 4.2.2b — `[resource-drop]resolve-address-stream`.
    /// Phase 4.2.2b1 drops the stream immediately on the happy
    /// path; Phase 4.2.2b2 keeps it live while looping
    /// `resolve-next-address` and drops once the first IPv4 has
    /// been pulled.
    pub drop_resolve_stream_fn: u32,
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
/// Single shared retptr / pollable-input scratch block allocated
/// once per `Tcp.connect` call via `cabi_realloc(0, 0, 4, 64)`.
/// Cuts the per-call allocator traffic from 3-4 small bumps down
/// to a single 64-byte chunk and gives later phases stable offsets
/// to load from. Layout (every entry 16-aligned to leave slack):
///
/// | offset | size (B) | purpose                                |
/// |--------|----------|----------------------------------------|
/// | +0     | 8        | retptr — `resolve-addresses` result    |
/// | +16    | 24       | retptr — `resolve-next-address` result |
/// | +48    | 8        | retptr — `wasi:io/poll.poll` (out list) |
/// | +56    | 4        | input  — pollable handle for `poll`     |
///
/// Phases 4.2.2b1 only touches +0; b2 adds +16, +48, +56. The
/// block stays live for the entire helper invocation (Aver
/// `Tcp.connect` is one synchronous call), so no per-step
/// reallocation.
const SCRATCH_BLOCK_SIZE: i32 = 64;
const SCRATCH_OFFSET_RESOLVE: u32 = 0;

pub(super) fn emit_tcp_connect_stub(
    indices: &TcpConnectIndices,
    helpers: &TcpConnectHelperFns,
) -> Function {
    use wasm_encoder::{BlockType, MemArg};
    // Locals beyond the two params (0=host: ref string, 1=port: i64):
    //   local 2 = host_len (i32)         — bytes written by str_to_lm
    //   local 3 = scratch (i32)          — base of the 64-byte retptr block
    //   local 4 = resolve_stream (i32)   — handle from resolve-addresses Ok
    let mut f = Function::new(vec![(3u32, ValType::I32)]);
    let l_host_len: u32 = 2;
    let l_scratch: u32 = 3;
    let l_resolve_stream: u32 = 4;

    let mem4_resolve = MemArg {
        offset: u64::from(SCRATCH_OFFSET_RESOLVE),
        align: 2,
        memory_index: 0,
    };
    let mem4_resolve_off4 = MemArg {
        offset: u64::from(SCRATCH_OFFSET_RESOLVE + 4),
        align: 2,
        memory_index: 0,
    };
    let mem1_resolve = MemArg {
        offset: u64::from(SCRATCH_OFFSET_RESOLVE),
        align: 0,
        memory_index: 0,
    };

    // ── Phase 4.2.2a — lazy network handle init. ───────────────
    // Pattern mirrors Console.print's stdout cache: if the global
    // is still the -1 sentinel, fetch the host network resource
    // handle once and stash it. The resource is program-lifetime
    // (wasmtime cleans up at component exit), so no drop helper.
    f.instruction(&Instruction::GlobalGet(helpers.network_handle_global));
    f.instruction(&Instruction::I32Const(-1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::Call(helpers.instance_network_fn));
    f.instruction(&Instruction::GlobalSet(helpers.network_handle_global));
    f.instruction(&Instruction::End);

    // ── Phase 4.2.2b — shared scratch block + DNS resolve. ─────
    // Step 1 — marshal host into LM[0..host_len].
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Call(helpers.str_to_lm_fn));
    f.instruction(&Instruction::LocalSet(l_host_len));

    // Step 2 — allocate the shared 64-byte scratch block via
    // cabi_realloc (one bump per `Tcp.connect`, reused by every
    // retptr / pollable-input slot the body needs). Layout
    // documented next to `SCRATCH_BLOCK_SIZE` above.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(SCRATCH_BLOCK_SIZE));
    f.instruction(&Instruction::Call(helpers.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_scratch));

    // Step 3 — resolve-addresses(network, host_ptr=0, host_len, retptr).
    // retptr lands at scratch[+0..+8].
    f.instruction(&Instruction::GlobalGet(helpers.network_handle_global));
    f.instruction(&Instruction::I32Const(0)); // host_ptr (LM start)
    f.instruction(&Instruction::LocalGet(l_host_len));
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::Call(helpers.resolve_addresses_fn));

    // Step 4 — read the Result tag at scratch[+0]. On Err,
    // materialise "tcp: dns resolve failed" and return early; on
    // Ok, fall through to the (still-stubbed) tail.
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_resolve));
    f.instruction(&Instruction::If(BlockType::Empty));
    // Err path: build the dns-error string + Result.Err + early return.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(indices.dns_err_len as i32));
    f.instruction(&Instruction::ArrayNewData {
        array_type_index: indices.string_type_idx,
        array_data_index: indices.dns_err_segment_idx,
    });
    f.instruction(&Instruction::Call(helpers.result_err_fn));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // Step 5 — Ok branch: pull the stream handle out of scratch[+4]
    // and drop it. Phase 4.2.2b2 will keep the handle live and loop
    // `resolve-next-address`; today we have nothing to do with it.
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load(mem4_resolve_off4));
    f.instruction(&Instruction::LocalSet(l_resolve_stream));
    f.instruction(&Instruction::LocalGet(l_resolve_stream));
    f.instruction(&Instruction::Call(helpers.drop_resolve_stream_fn));
    let _ = mem4_resolve; // reserved for the future option / ipv4 octet loads

    // ── Stub tail (Phase 4.2.1) — replace in 4.2.2b2. ──────────
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
            dns_err_segment_idx: 1,
            dns_err_len: b"tcp: dns resolve failed".len() as u32,
        };
        let helpers = TcpConnectHelperFns {
            result_err_fn: 2,
            instance_network_fn: 3,
            network_handle_global: 0,
            cabi_realloc_fn: 4,
            str_to_lm_fn: 5,
            resolve_addresses_fn: 6,
            drop_resolve_stream_fn: 7,
        };
        let _f = emit_tcp_connect_stub(&indices, &helpers);
    }
}

//! Resource-teardown + orchestration helpers — `__rt_tcp_close`,
//! `__rt_tcp_send`, `__rt_tcp_ping`.
//!
//! `close` releases the pool slot's streams + socket and surfaces
//! `Err("tcp: unknown connection")` on stale / null / already-closed
//! handles (cross-backend parity with `aver-rt::tcp::close`).
//!
//! `send` is a full one-shot ephemeral pipeline (Phase 4.7+ fix #16):
//! inline DNS + create-tcp-socket + start/finish-connect → raw
//! chunked write → shutdown(send) → read-to-EOF (capped at 10 MiB)
//! → drop streams + socket. No pool slot involved; matches
//! `aver-rt::tcp::send`'s line-agnostic semantics (no trailing `\r\n`
//! on the request, full response collected until the peer closes).
//!
//! `ping` reuses the same dial prologue (Phase 4.7+ fix #21) — DNS +
//! create-tcp-socket + start/finish-connect → drop streams + socket
//! → `Result.Ok(())`. Ephemeral, no pool slot, so a program holding
//! 256 live `Tcp.connect` handles can still ping freely. Matches
//! `aver-rt::tcp::ping`'s pool-free dial-and-drop shape.

use wasm_encoder::{Function, Instruction, ValType};

use super::restore_bump;

/// Phase 4.3 — `__rt_tcp_close(conn: ref Tcp.Connection) ->
/// ref Result<Unit, String>` slot bundle.
pub(in crate::codegen::wasm_gc) struct TcpCloseIndices {
    pub fn_type: u32,
    pub fn_idx: u32,
    pub tcp_connection_type_idx: u32,
    pub tcp_slot_type_idx: u32,
    pub tcp_pool_type_idx: u32,
    /// String slot type idx — needed by the stale-conn `Err`
    /// payload (`"tcp: unknown connection"` materialised from
    /// `unknown_segment_idx`).
    pub string_type_idx: u32,
    /// Phase 4.7+ — `"tcp: unknown connection"` data segment.
    /// Aligns wasip2 close with `aver-rt::tcp::close` which returns
    /// `Err("Tcp.close: unknown connection 'tcp-N'")` on stale ids
    /// (VM / self-host / wasm-gc AverBridge). Wasip2 drops the
    /// method-name + connection-id substring because the message
    /// is built from a static segment, not a runtime format.
    pub unknown_segment_idx: u32,
    pub unknown_len: u32,
}

pub(in crate::codegen::wasm_gc) struct TcpCloseHelperFns {
    /// `__rt_tcp_parse_id(id: ref string) -> i32` — extracts the
    /// pool slot index from the `"tcp-N"` id stored in the
    /// `Tcp.Connection` record.
    pub parse_id_fn: u32,
    /// `cabi_realloc(0, 0, 1, 2) -> ptr` — 2-byte retptr for the
    /// shutdown call. Result is ignored (best-effort close).
    pub cabi_realloc_fn: u32,
    /// `wasi:sockets/tcp.[method]tcp-socket.shutdown(this,
    ///   shutdown-type, retptr) -> ()`. Phase 4.3 always passes
    /// `both = 2` to flush both directions before dropping.
    pub shutdown_fn: u32,
    /// `[resource-drop]input-stream` / `output-stream` / `tcp-socket`.
    pub drop_input_stream_fn: u32,
    pub drop_output_stream_fn: u32,
    pub drop_tcp_socket_fn: u32,
    /// `__rt_result_unit_string_ok()` factory.
    pub result_ok_fn: u32,
    /// `__rt_result_unit_string_err(message)` factory — used by the
    /// Phase 4.7+ stale / null-pool / already-closed guards to match
    /// `aver-rt::tcp::close` semantics across backends.
    pub result_err_fn: u32,
    /// `tcp_pool: ref null $tcp_pool` global.
    pub tcp_pool_global: u32,
    /// Phase 4.2.2f — see `TcpConnectHelperFns::bump_alloc_ptr_global`.
    pub bump_alloc_ptr_global: u32,
}
/// `__rt_tcp_send(host, port, data) -> ref Result<String, String>`.
///
/// Phase 4.7+ pass 4 fix #16/#17/#18 — full ephemeral rewrite.
/// `aver-rt::tcp::send` opens a fresh `TcpStream`, writes the
/// request bytes, `shutdown(Write)` to signal end-of-request,
/// then `read_to_end` until EOF capped at 10 MiB; no `CONNECTIONS`
/// pool entry is created or consulted. Wasip2 used to go through
/// `__rt_tcp_connect` (pool-allocating) + `__rt_tcp_close`, which
/// inherited pool-limit semantics and conflated stream-error with
/// EOF.
///
/// The rewrite inlines DNS resolve + create-tcp-socket +
/// start/finish-connect (duplicating the `__rt_tcp_connect` body
/// minus its pool-slot allocation), then does the write +
/// shutdown(send) + chunked read pipeline directly on local
/// socket / stream handles, and drops everything via
/// `[resource-drop]` imports at the end. The pool stays
/// untouched, so a program holding 256 live `Tcp.connect`
/// handles can still issue a `Tcp.send` to a different peer.
pub(in crate::codegen::wasm_gc) struct TcpSendIndices {
    pub fn_type: u32,
    pub fn_idx: u32,
    pub string_type_idx: u32,
    /// `Result<String, String>` struct type idx — the send fn's own
    /// return type. We materialise the final Ok(...) via
    /// `struct.new` here rather than a factory call because the
    /// response string is constructed inline from the read buffer.
    pub result_string_string_type_idx: u32,
    /// DNS + socket + connect failure messages (mirrors
    /// `TcpConnectIndices` since the dialing pipeline lives inline
    /// in this body — Phase 4.7+ pass 4 didn't extract a shared
    /// `__rt_tcp_dial` helper).
    pub dns_err_segment_idx: u32,
    pub dns_err_len: u32,
    pub no_addr_segment_idx: u32,
    pub no_addr_len: u32,
    pub sock_err_segment_idx: u32,
    pub sock_err_len: u32,
    pub conn_err_segment_idx: u32,
    pub conn_err_len: u32,
    pub port_err_segment_idx: u32,
    pub port_err_len: u32,
    /// `"tcp: write failed"`.
    pub write_err_segment_idx: u32,
    pub write_err_len: u32,
    /// Phase 4.7+ fix #17 — `"tcp: stream error"`. Surfaces when
    /// `blocking-read` returns `stream-error.last-operation-failed`
    /// (real I/O error, not a clean half-close).
    pub stream_err_segment_idx: u32,
    pub stream_err_len: u32,
    /// Phase 4.7+ fix #18 — `"tcp: response exceeds 10 MiB limit"`.
    pub size_err_segment_idx: u32,
    pub size_err_len: u32,
}

pub(in crate::codegen::wasm_gc) struct TcpSendHelperFns {
    // Dialing pipeline — same shape as TcpConnectHelperFns minus
    // the pool / Tcp.Connection materialise concerns.
    pub instance_network_fn: u32,
    pub network_handle_global: u32,
    pub resolve_addresses_fn: u32,
    pub resolve_next_address_fn: u32,
    pub drop_resolve_stream_fn: u32,
    pub stream_subscribe_fn: u32,
    pub poll_fn: u32,
    pub drop_pollable_fn: u32,
    pub create_tcp_socket_fn: u32,
    pub start_connect_fn: u32,
    pub finish_connect_fn: u32,
    pub socket_subscribe_fn: u32,
    pub drop_tcp_socket_fn: u32,
    pub drop_input_stream_fn: u32,
    pub drop_output_stream_fn: u32,
    pub str_to_lm_fn: u32,
    pub cabi_realloc_fn: u32,
    /// `wasi:io/streams.[method]output-stream.blocking-write-and-flush`.
    pub blocking_write_fn: u32,
    /// `wasi:io/streams.[method]input-stream.blocking-read`.
    pub blocking_read_fn: u32,
    /// `wasi:sockets/tcp.[method]tcp-socket.shutdown`. Called with
    /// `shutdown-type.send = 1` after the payload write so the
    /// peer sees an orderly half-close and can flush its response.
    pub shutdown_fn: u32,
    pub bump_alloc_ptr_global: u32,
    /// `__rt_result_string_string_err(message)` — every error path
    /// surfaces through here for cross-backend message parity.
    pub result_string_string_err_fn: u32,
}

/// `__rt_tcp_send_bytes(host, port, data) ->
/// ref Result<List<Int>, String>`.
pub(in crate::codegen::wasm_gc) struct TcpSendBytesIndices {
    pub fn_type: u32,
    pub fn_idx: u32,
    pub string_type_idx: u32,
    pub list_int_type_idx: u32,
    pub result_list_int_string_type_idx: u32,
    pub aint_struct_type_idx: Option<u32>,
    pub dns_err_segment_idx: u32,
    pub dns_err_len: u32,
    pub no_addr_segment_idx: u32,
    pub no_addr_len: u32,
    pub sock_err_segment_idx: u32,
    pub sock_err_len: u32,
    pub conn_err_segment_idx: u32,
    pub conn_err_len: u32,
    pub port_err_segment_idx: u32,
    pub port_err_len: u32,
    pub write_err_segment_idx: u32,
    pub write_err_len: u32,
    pub stream_err_segment_idx: u32,
    pub stream_err_len: u32,
    pub size_err_segment_idx: u32,
    pub size_err_len: u32,
}

pub(in crate::codegen::wasm_gc) struct TcpSendBytesHelperFns {
    pub instance_network_fn: u32,
    pub network_handle_global: u32,
    pub resolve_addresses_fn: u32,
    pub resolve_next_address_fn: u32,
    pub drop_resolve_stream_fn: u32,
    pub stream_subscribe_fn: u32,
    pub poll_fn: u32,
    pub drop_pollable_fn: u32,
    pub create_tcp_socket_fn: u32,
    pub start_connect_fn: u32,
    pub finish_connect_fn: u32,
    pub socket_subscribe_fn: u32,
    pub drop_tcp_socket_fn: u32,
    pub drop_input_stream_fn: u32,
    pub drop_output_stream_fn: u32,
    pub str_to_lm_fn: u32,
    pub cabi_realloc_fn: u32,
    pub blocking_write_fn: u32,
    pub blocking_read_fn: u32,
    pub shutdown_fn: u32,
    pub bump_alloc_ptr_global: u32,
    pub string_from_lm_fn: u32,
    pub aint_from_i64_fn: Option<u32>,
    pub aint_to_string_fn: Option<u32>,
}

#[derive(Clone, Copy)]
struct TcpSendCommonIndices {
    string_type_idx: u32,
    dns_err_segment_idx: u32,
    dns_err_len: u32,
    no_addr_segment_idx: u32,
    no_addr_len: u32,
    sock_err_segment_idx: u32,
    sock_err_len: u32,
    conn_err_segment_idx: u32,
    conn_err_len: u32,
    port_err_segment_idx: u32,
    port_err_len: u32,
    write_err_segment_idx: u32,
    write_err_len: u32,
    stream_err_segment_idx: u32,
    stream_err_len: u32,
    size_err_segment_idx: u32,
    size_err_len: u32,
}

#[derive(Clone, Copy)]
struct TcpSendCommonHelperFns {
    instance_network_fn: u32,
    network_handle_global: u32,
    resolve_addresses_fn: u32,
    resolve_next_address_fn: u32,
    drop_resolve_stream_fn: u32,
    stream_subscribe_fn: u32,
    poll_fn: u32,
    drop_pollable_fn: u32,
    create_tcp_socket_fn: u32,
    start_connect_fn: u32,
    finish_connect_fn: u32,
    socket_subscribe_fn: u32,
    drop_tcp_socket_fn: u32,
    drop_input_stream_fn: u32,
    drop_output_stream_fn: u32,
    str_to_lm_fn: u32,
    cabi_realloc_fn: u32,
    blocking_write_fn: u32,
    blocking_read_fn: u32,
    shutdown_fn: u32,
    bump_alloc_ptr_global: u32,
}

#[derive(Clone, Copy)]
enum TcpSendFlavor {
    String {
        result_type_idx: u32,
        result_err_fn: u32,
    },
    Bytes {
        list_int_type_idx: u32,
        result_type_idx: u32,
        aint_struct_type_idx: Option<u32>,
        string_from_lm_fn: u32,
        aint_from_i64_fn: Option<u32>,
        aint_to_string_fn: Option<u32>,
    },
}

fn emit_lm_append_literal(f: &mut Function, cursor_local: u32, bytes: &[u8]) {
    use wasm_encoder::MemArg;

    let mem1 = MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    };
    for byte in bytes {
        f.instruction(&Instruction::LocalGet(cursor_local));
        f.instruction(&Instruction::I32Const(i32::from(*byte)));
        f.instruction(&Instruction::I32Store8(mem1));
        f.instruction(&Instruction::LocalGet(cursor_local));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::LocalSet(cursor_local));
    }
}

fn emit_lm_append_u64_decimal(
    f: &mut Function,
    value_local: u32,
    cursor_local: u32,
    digit_cursor_local: u32,
    digit_end_local: u32,
) {
    use wasm_encoder::{BlockType, MemArg};

    let mem1 = MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    };

    f.instruction(&Instruction::LocalGet(cursor_local));
    f.instruction(&Instruction::I32Const(20));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(digit_end_local));
    f.instruction(&Instruction::LocalGet(digit_end_local));
    f.instruction(&Instruction::LocalSet(digit_cursor_local));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(digit_cursor_local));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(digit_cursor_local));
    f.instruction(&Instruction::LocalGet(digit_cursor_local));
    f.instruction(&Instruction::LocalGet(value_local));
    f.instruction(&Instruction::I64Const(10));
    f.instruction(&Instruction::I64RemU);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Const(i32::from(b'0')));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Store8(mem1));
    f.instruction(&Instruction::LocalGet(value_local));
    f.instruction(&Instruction::I64Const(10));
    f.instruction(&Instruction::I64DivU);
    f.instruction(&Instruction::LocalSet(value_local));
    f.instruction(&Instruction::LocalGet(value_local));
    f.instruction(&Instruction::I64Eqz);
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(digit_cursor_local));
    f.instruction(&Instruction::LocalGet(digit_end_local));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(cursor_local));
    f.instruction(&Instruction::LocalGet(digit_cursor_local));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::I32Store8(mem1));
    f.instruction(&Instruction::LocalGet(cursor_local));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(cursor_local));
    f.instruction(&Instruction::LocalGet(digit_cursor_local));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(digit_cursor_local));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
}

struct TcpByteRangeError {
    string_type_idx: u32,
    list_int_type_idx: u32,
    result_type_idx: u32,
    string_from_lm_fn: u32,
    cabi_realloc_fn: u32,
    bump_alloc_ptr_global: u32,
    saved_alloc_local: u32,
    byte_value_local: u32,
    byte_index_local: u32,
    message_cursor_local: u32,
    digit_cursor_local: u32,
    digit_end_local: u32,
    formatted_value_local: Option<u32>,
}

fn emit_byte_range_error(f: &mut Function, ctx: &TcpByteRangeError) {
    use wasm_encoder::HeapType;

    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(1));
    if let Some(formatted_value_local) = ctx.formatted_value_local {
        f.instruction(&Instruction::LocalGet(formatted_value_local));
        f.instruction(&Instruction::ArrayLen);
        f.instruction(&Instruction::I32Const(96));
        f.instruction(&Instruction::I32Add);
    } else {
        f.instruction(&Instruction::I32Const(128));
    }
    f.instruction(&Instruction::Call(ctx.cabi_realloc_fn));
    f.instruction(&Instruction::Drop);

    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(ctx.message_cursor_local));
    emit_lm_append_literal(f, ctx.message_cursor_local, b"Tcp.sendBytes: byte ");

    if let Some(formatted_value_local) = ctx.formatted_value_local {
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::LocalSet(ctx.digit_cursor_local));
        f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
        f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
        f.instruction(&Instruction::LocalGet(ctx.digit_cursor_local));
        f.instruction(&Instruction::LocalGet(formatted_value_local));
        f.instruction(&Instruction::ArrayLen);
        f.instruction(&Instruction::I32GeU);
        f.instruction(&Instruction::BrIf(1));
        f.instruction(&Instruction::LocalGet(ctx.message_cursor_local));
        f.instruction(&Instruction::LocalGet(formatted_value_local));
        f.instruction(&Instruction::LocalGet(ctx.digit_cursor_local));
        f.instruction(&Instruction::ArrayGetU(ctx.string_type_idx));
        f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }));
        f.instruction(&Instruction::LocalGet(ctx.message_cursor_local));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::LocalSet(ctx.message_cursor_local));
        f.instruction(&Instruction::LocalGet(ctx.digit_cursor_local));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::LocalSet(ctx.digit_cursor_local));
        f.instruction(&Instruction::Br(0));
        f.instruction(&Instruction::End);
        f.instruction(&Instruction::End);
    } else {
        f.instruction(&Instruction::LocalGet(ctx.byte_value_local));
        f.instruction(&Instruction::I64Const(0));
        f.instruction(&Instruction::I64LtS);
        f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
        emit_lm_append_literal(f, ctx.message_cursor_local, b"-");
        f.instruction(&Instruction::I64Const(0));
        f.instruction(&Instruction::LocalGet(ctx.byte_value_local));
        f.instruction(&Instruction::I64Sub);
        f.instruction(&Instruction::LocalSet(ctx.byte_value_local));
        f.instruction(&Instruction::End);
        emit_lm_append_u64_decimal(
            f,
            ctx.byte_value_local,
            ctx.message_cursor_local,
            ctx.digit_cursor_local,
            ctx.digit_end_local,
        );
    }

    emit_lm_append_literal(f, ctx.message_cursor_local, b" at index ");
    f.instruction(&Instruction::LocalGet(ctx.byte_index_local));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::LocalSet(ctx.byte_value_local));
    emit_lm_append_u64_decimal(
        f,
        ctx.byte_value_local,
        ctx.message_cursor_local,
        ctx.digit_cursor_local,
        ctx.digit_end_local,
    );
    emit_lm_append_literal(
        f,
        ctx.message_cursor_local,
        " is out of range (0–255)".as_bytes(),
    );

    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::RefNull(HeapType::Concrete(
        ctx.list_int_type_idx,
    )));
    f.instruction(&Instruction::LocalGet(ctx.message_cursor_local));
    f.instruction(&Instruction::Call(ctx.string_from_lm_fn));
    f.instruction(&Instruction::StructNew(ctx.result_type_idx));
    restore_bump(f, ctx.saved_alloc_local, ctx.bump_alloc_ptr_global);
    f.instruction(&Instruction::Return);
}

pub(in crate::codegen::wasm_gc) fn emit_tcp_send(
    indices: &TcpSendIndices,
    helpers: &TcpSendHelperFns,
) -> Function {
    emit_tcp_send_impl(
        TcpSendCommonIndices {
            string_type_idx: indices.string_type_idx,
            dns_err_segment_idx: indices.dns_err_segment_idx,
            dns_err_len: indices.dns_err_len,
            no_addr_segment_idx: indices.no_addr_segment_idx,
            no_addr_len: indices.no_addr_len,
            sock_err_segment_idx: indices.sock_err_segment_idx,
            sock_err_len: indices.sock_err_len,
            conn_err_segment_idx: indices.conn_err_segment_idx,
            conn_err_len: indices.conn_err_len,
            port_err_segment_idx: indices.port_err_segment_idx,
            port_err_len: indices.port_err_len,
            write_err_segment_idx: indices.write_err_segment_idx,
            write_err_len: indices.write_err_len,
            stream_err_segment_idx: indices.stream_err_segment_idx,
            stream_err_len: indices.stream_err_len,
            size_err_segment_idx: indices.size_err_segment_idx,
            size_err_len: indices.size_err_len,
        },
        TcpSendCommonHelperFns {
            instance_network_fn: helpers.instance_network_fn,
            network_handle_global: helpers.network_handle_global,
            resolve_addresses_fn: helpers.resolve_addresses_fn,
            resolve_next_address_fn: helpers.resolve_next_address_fn,
            drop_resolve_stream_fn: helpers.drop_resolve_stream_fn,
            stream_subscribe_fn: helpers.stream_subscribe_fn,
            poll_fn: helpers.poll_fn,
            drop_pollable_fn: helpers.drop_pollable_fn,
            create_tcp_socket_fn: helpers.create_tcp_socket_fn,
            start_connect_fn: helpers.start_connect_fn,
            finish_connect_fn: helpers.finish_connect_fn,
            socket_subscribe_fn: helpers.socket_subscribe_fn,
            drop_tcp_socket_fn: helpers.drop_tcp_socket_fn,
            drop_input_stream_fn: helpers.drop_input_stream_fn,
            drop_output_stream_fn: helpers.drop_output_stream_fn,
            str_to_lm_fn: helpers.str_to_lm_fn,
            cabi_realloc_fn: helpers.cabi_realloc_fn,
            blocking_write_fn: helpers.blocking_write_fn,
            blocking_read_fn: helpers.blocking_read_fn,
            shutdown_fn: helpers.shutdown_fn,
            bump_alloc_ptr_global: helpers.bump_alloc_ptr_global,
        },
        TcpSendFlavor::String {
            result_type_idx: indices.result_string_string_type_idx,
            result_err_fn: helpers.result_string_string_err_fn,
        },
    )
}

pub(in crate::codegen::wasm_gc) fn emit_tcp_send_bytes(
    indices: &TcpSendBytesIndices,
    helpers: &TcpSendBytesHelperFns,
) -> Function {
    emit_tcp_send_impl(
        TcpSendCommonIndices {
            string_type_idx: indices.string_type_idx,
            dns_err_segment_idx: indices.dns_err_segment_idx,
            dns_err_len: indices.dns_err_len,
            no_addr_segment_idx: indices.no_addr_segment_idx,
            no_addr_len: indices.no_addr_len,
            sock_err_segment_idx: indices.sock_err_segment_idx,
            sock_err_len: indices.sock_err_len,
            conn_err_segment_idx: indices.conn_err_segment_idx,
            conn_err_len: indices.conn_err_len,
            port_err_segment_idx: indices.port_err_segment_idx,
            port_err_len: indices.port_err_len,
            write_err_segment_idx: indices.write_err_segment_idx,
            write_err_len: indices.write_err_len,
            stream_err_segment_idx: indices.stream_err_segment_idx,
            stream_err_len: indices.stream_err_len,
            size_err_segment_idx: indices.size_err_segment_idx,
            size_err_len: indices.size_err_len,
        },
        TcpSendCommonHelperFns {
            instance_network_fn: helpers.instance_network_fn,
            network_handle_global: helpers.network_handle_global,
            resolve_addresses_fn: helpers.resolve_addresses_fn,
            resolve_next_address_fn: helpers.resolve_next_address_fn,
            drop_resolve_stream_fn: helpers.drop_resolve_stream_fn,
            stream_subscribe_fn: helpers.stream_subscribe_fn,
            poll_fn: helpers.poll_fn,
            drop_pollable_fn: helpers.drop_pollable_fn,
            create_tcp_socket_fn: helpers.create_tcp_socket_fn,
            start_connect_fn: helpers.start_connect_fn,
            finish_connect_fn: helpers.finish_connect_fn,
            socket_subscribe_fn: helpers.socket_subscribe_fn,
            drop_tcp_socket_fn: helpers.drop_tcp_socket_fn,
            drop_input_stream_fn: helpers.drop_input_stream_fn,
            drop_output_stream_fn: helpers.drop_output_stream_fn,
            str_to_lm_fn: helpers.str_to_lm_fn,
            cabi_realloc_fn: helpers.cabi_realloc_fn,
            blocking_write_fn: helpers.blocking_write_fn,
            blocking_read_fn: helpers.blocking_read_fn,
            shutdown_fn: helpers.shutdown_fn,
            bump_alloc_ptr_global: helpers.bump_alloc_ptr_global,
        },
        TcpSendFlavor::Bytes {
            list_int_type_idx: indices.list_int_type_idx,
            result_type_idx: indices.result_list_int_string_type_idx,
            aint_struct_type_idx: indices.aint_struct_type_idx,
            string_from_lm_fn: helpers.string_from_lm_fn,
            aint_from_i64_fn: helpers.aint_from_i64_fn,
            aint_to_string_fn: helpers.aint_to_string_fn,
        },
    )
}

fn emit_tcp_send_impl(
    indices: TcpSendCommonIndices,
    helpers: TcpSendCommonHelperFns,
    flavor: TcpSendFlavor,
) -> Function {
    use wasm_encoder::{BlockType, HeapType, MemArg, RefType};

    let s_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.string_type_idx),
    });

    // Locals (params 0=host, 1=port: i64, 2=data):
    //   3  = saved_alloc   (i32) — bump-rewind cursor
    //   4  = host_len      (i32)
    //   5  = scratch       (i32) — 64-byte retptr block
    //   6  = resolve_strm  (i32)
    //   7  = pollable      (i32)
    //   8  = ipv4_a        (i32)
    //   9  = ipv4_b        (i32)
    //   10 = ipv4_c        (i32)
    //   11 = ipv4_d        (i32)
    //   12 = socket        (i32)
    //   13 = in_handle     (i32)
    //   14 = out_handle    (i32)
    //   15 = data_len      (i32)
    //   16 = off           (i32) — chunked-write cursor
    //   17 = retptr        (i32) — 12-byte stream-result retptr
    //   18 = buf_ptr       (i32)
    //   19 = buf_cap       (i32)
    //   20 = buf_len       (i32)
    //   21 = read_ptr      (i32)
    //   22 = read_len      (i32)
    //   23 = new_cap       (i32)
    //   24 = j             (i32) — array-copy index
    //   25 = byte_value    (i64, sendBytes only)
    //   25/26 = arr        (ref string) — response materialisation
    //   27 = list_cursor   (ref List<Int>, sendBytes only)
    let l_saved_alloc: u32 = 3;
    let l_host_len: u32 = 4;
    let l_scratch: u32 = 5;
    let l_resolve_strm: u32 = 6;
    let l_pollable: u32 = 7;
    let l_ipv4_a: u32 = 8;
    let l_ipv4_b: u32 = 9;
    let l_ipv4_c: u32 = 10;
    let l_ipv4_d: u32 = 11;
    let l_socket: u32 = 12;
    let l_in_handle: u32 = 13;
    let l_out_handle: u32 = 14;
    let l_data_len: u32 = 15;
    let l_off: u32 = 16;
    let l_retptr: u32 = 17;
    let l_buf_ptr: u32 = 18;
    let l_buf_cap: u32 = 19;
    let l_buf_len: u32 = 20;
    let l_read_ptr: u32 = 21;
    let l_read_len: u32 = 22;
    let l_new_cap: u32 = 23;
    let l_j: u32 = 24;
    let (mut f, l_byte_value, l_arr, l_list_cursor) = match flavor {
        TcpSendFlavor::String { .. } => (
            Function::new(vec![(22u32, ValType::I32), (1u32, s_ref)]),
            None,
            25,
            None,
        ),
        TcpSendFlavor::Bytes {
            list_int_type_idx, ..
        } => {
            let list_ref = ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(list_int_type_idx),
            });
            (
                Function::new(vec![
                    (22u32, ValType::I32),
                    (1u32, ValType::I64),
                    (1u32, s_ref),
                    (1u32, list_ref),
                ]),
                Some(25),
                26,
                Some(27),
            )
        }
    };

    // Scratch layout — copied from `emit_tcp_connect_stub` so the
    // duplicated DNS pipeline stays consistent with connect's
    // retptr / pollable offsets.
    const SCRATCH_BLOCK_SIZE: i32 = 64;
    const SCRATCH_OFFSET_RESOLVE: u32 = 0;
    const SCRATCH_OFFSET_NEXT: u32 = 16;
    const SCRATCH_OFFSET_POLL: u32 = 48;
    const SCRATCH_OFFSET_POLLABLE_IN: u32 = 56;

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
    let mem4_resolve_off8 = MemArg {
        offset: u64::from(SCRATCH_OFFSET_RESOLVE + 8),
        align: 2,
        memory_index: 0,
    };
    let mem1_resolve = MemArg {
        offset: u64::from(SCRATCH_OFFSET_RESOLVE),
        align: 0,
        memory_index: 0,
    };
    let mem1_resolve_off4 = MemArg {
        offset: u64::from(SCRATCH_OFFSET_RESOLVE + 4),
        align: 0,
        memory_index: 0,
    };
    let mem1_next_outer = MemArg {
        offset: u64::from(SCRATCH_OFFSET_NEXT),
        align: 0,
        memory_index: 0,
    };
    let mem1_next_option = MemArg {
        offset: u64::from(SCRATCH_OFFSET_NEXT + 2),
        align: 0,
        memory_index: 0,
    };
    let mem1_next_variant = MemArg {
        offset: u64::from(SCRATCH_OFFSET_NEXT + 4),
        align: 0,
        memory_index: 0,
    };
    let mem1_next_octet_a = MemArg {
        offset: u64::from(SCRATCH_OFFSET_NEXT + 6),
        align: 0,
        memory_index: 0,
    };
    let mem1_next_octet_b = MemArg {
        offset: u64::from(SCRATCH_OFFSET_NEXT + 7),
        align: 0,
        memory_index: 0,
    };
    let mem1_next_octet_c = MemArg {
        offset: u64::from(SCRATCH_OFFSET_NEXT + 8),
        align: 0,
        memory_index: 0,
    };
    let mem1_next_octet_d = MemArg {
        offset: u64::from(SCRATCH_OFFSET_NEXT + 9),
        align: 0,
        memory_index: 0,
    };
    let mem4_pollable_in = MemArg {
        offset: u64::from(SCRATCH_OFFSET_POLLABLE_IN),
        align: 2,
        memory_index: 0,
    };
    let mem1 = MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    };
    let mem4_o4 = MemArg {
        offset: 4,
        align: 2,
        memory_index: 0,
    };
    let mem4_o8 = MemArg {
        offset: 8,
        align: 2,
        memory_index: 0,
    };

    // Shared `Err` emitter — picks one of the per-stage segments and
    // wraps it in the return carrier for the selected send flavor.
    let string_type_idx = indices.string_type_idx;
    let bump_alloc_ptr_global = helpers.bump_alloc_ptr_global;
    let emit_err_with_segment = |f: &mut Function, seg: u32, len: u32| {
        match flavor {
            TcpSendFlavor::String { result_err_fn, .. } => {
                f.instruction(&Instruction::I32Const(0));
                f.instruction(&Instruction::I32Const(len as i32));
                f.instruction(&Instruction::ArrayNewData {
                    array_type_index: string_type_idx,
                    array_data_index: seg,
                });
                f.instruction(&Instruction::Call(result_err_fn));
            }
            TcpSendFlavor::Bytes {
                list_int_type_idx,
                result_type_idx,
                ..
            } => {
                f.instruction(&Instruction::I32Const(0));
                f.instruction(&Instruction::RefNull(HeapType::Concrete(list_int_type_idx)));
                f.instruction(&Instruction::I32Const(0));
                f.instruction(&Instruction::I32Const(len as i32));
                f.instruction(&Instruction::ArrayNewData {
                    array_type_index: string_type_idx,
                    array_data_index: seg,
                });
                f.instruction(&Instruction::StructNew(result_type_idx));
            }
        }
        restore_bump(f, l_saved_alloc, bump_alloc_ptr_global);
        f.instruction(&Instruction::Return);
    };

    // ── Prolog. ────────────────────────────────────────────────
    f.instruction(&Instruction::GlobalGet(helpers.bump_alloc_ptr_global));
    f.instruction(&Instruction::LocalSet(l_saved_alloc));

    if let TcpSendFlavor::Bytes {
        list_int_type_idx,
        result_type_idx,
        aint_struct_type_idx,
        string_from_lm_fn,
        aint_to_string_fn,
        ..
    } = flavor
    {
        let l_byte_value = l_byte_value.expect("bytes flavor has byte-value local");
        let l_list_cursor = l_list_cursor.expect("bytes flavor has list cursor local");

        f.instruction(&Instruction::LocalGet(2));
        f.instruction(&Instruction::LocalSet(l_list_cursor));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::LocalSet(l_data_len));
        f.instruction(&Instruction::Block(BlockType::Empty));
        f.instruction(&Instruction::Loop(BlockType::Empty));
        f.instruction(&Instruction::LocalGet(l_list_cursor));
        f.instruction(&Instruction::RefIsNull);
        f.instruction(&Instruction::BrIf(1));

        if let Some(aint_type_idx) = aint_struct_type_idx {
            f.instruction(&Instruction::LocalGet(l_list_cursor));
            f.instruction(&Instruction::StructGet {
                struct_type_index: list_int_type_idx,
                field_index: 0,
            });
            f.instruction(&Instruction::StructGet {
                struct_type_index: aint_type_idx,
                field_index: 0,
            });
            f.instruction(&Instruction::LocalSet(l_byte_value));
            f.instruction(&Instruction::LocalGet(l_list_cursor));
            f.instruction(&Instruction::StructGet {
                struct_type_index: list_int_type_idx,
                field_index: 0,
            });
            f.instruction(&Instruction::StructGet {
                struct_type_index: aint_type_idx,
                field_index: 1,
            });
            f.instruction(&Instruction::RefIsNull);
            f.instruction(&Instruction::I32Eqz);
        } else {
            f.instruction(&Instruction::LocalGet(l_list_cursor));
            f.instruction(&Instruction::StructGet {
                struct_type_index: list_int_type_idx,
                field_index: 0,
            });
            f.instruction(&Instruction::LocalSet(l_byte_value));
            f.instruction(&Instruction::I32Const(0));
        }
        f.instruction(&Instruction::LocalGet(l_byte_value));
        f.instruction(&Instruction::I64Const(0));
        f.instruction(&Instruction::I64LtS);
        f.instruction(&Instruction::I32Or);
        f.instruction(&Instruction::LocalGet(l_byte_value));
        f.instruction(&Instruction::I64Const(255));
        f.instruction(&Instruction::I64GtS);
        f.instruction(&Instruction::I32Or);
        f.instruction(&Instruction::If(BlockType::Empty));
        let formatted_value_local = if aint_struct_type_idx.is_some() {
            f.instruction(&Instruction::LocalGet(l_list_cursor));
            f.instruction(&Instruction::StructGet {
                struct_type_index: list_int_type_idx,
                field_index: 0,
            });
            f.instruction(&Instruction::Call(
                aint_to_string_fn.expect("bignum sendBytes needs String.fromInt"),
            ));
            f.instruction(&Instruction::LocalSet(l_arr));
            Some(l_arr)
        } else {
            None
        };
        emit_byte_range_error(
            &mut f,
            &TcpByteRangeError {
                string_type_idx,
                list_int_type_idx,
                result_type_idx,
                string_from_lm_fn,
                cabi_realloc_fn: helpers.cabi_realloc_fn,
                bump_alloc_ptr_global,
                saved_alloc_local: l_saved_alloc,
                byte_value_local: l_byte_value,
                byte_index_local: l_data_len,
                message_cursor_local: l_off,
                digit_cursor_local: l_retptr,
                digit_end_local: l_j,
                formatted_value_local,
            },
        );
        f.instruction(&Instruction::End);

        f.instruction(&Instruction::LocalGet(l_list_cursor));
        f.instruction(&Instruction::StructGet {
            struct_type_index: list_int_type_idx,
            field_index: 1,
        });
        f.instruction(&Instruction::LocalSet(l_list_cursor));
        f.instruction(&Instruction::LocalGet(l_data_len));
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::LocalSet(l_data_len));
        f.instruction(&Instruction::Br(0));
        f.instruction(&Instruction::End);
        f.instruction(&Instruction::End);
    }

    // Port validation (matches the connect prolog).
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::I64LtS);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Const(65535));
    f.instruction(&Instruction::I64GtS);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_err_with_segment(&mut f, indices.port_err_segment_idx, indices.port_err_len);
    f.instruction(&Instruction::End);

    // Lazy `network` handle.
    f.instruction(&Instruction::GlobalGet(helpers.network_handle_global));
    f.instruction(&Instruction::I32Const(-1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::Call(helpers.instance_network_fn));
    f.instruction(&Instruction::GlobalSet(helpers.network_handle_global));
    f.instruction(&Instruction::End);

    // ── DNS resolve (mirrors connect.rs, Phase 4.2.2b + b2). ──
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Call(helpers.str_to_lm_fn));
    f.instruction(&Instruction::LocalSet(l_host_len));

    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(SCRATCH_BLOCK_SIZE));
    f.instruction(&Instruction::Call(helpers.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_scratch));

    f.instruction(&Instruction::GlobalGet(helpers.network_handle_global));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(l_host_len));
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::Call(helpers.resolve_addresses_fn));

    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_resolve));
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_err_with_segment(&mut f, indices.dns_err_segment_idx, indices.dns_err_len);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load(mem4_resolve_off4));
    f.instruction(&Instruction::LocalSet(l_resolve_strm));
    let _ = mem4_resolve;

    // resolve-next-address loop, first IPv4 wins. Matches connect.rs
    // exactly including the Phase 4.7+ fix #1 (Ok(None) → Err) +
    // the IPv6 skip.
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));

    f.instruction(&Instruction::LocalGet(l_resolve_strm));
    f.instruction(&Instruction::Call(helpers.stream_subscribe_fn));
    f.instruction(&Instruction::LocalSet(l_pollable));

    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::LocalGet(l_pollable));
    f.instruction(&Instruction::I32Store(mem4_pollable_in));

    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Const(SCRATCH_OFFSET_POLLABLE_IN as i32));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Const(SCRATCH_OFFSET_POLL as i32));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::Call(helpers.poll_fn));

    f.instruction(&Instruction::LocalGet(l_pollable));
    f.instruction(&Instruction::Call(helpers.drop_pollable_fn));

    f.instruction(&Instruction::LocalGet(l_resolve_strm));
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Const(SCRATCH_OFFSET_NEXT as i32));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::Call(helpers.resolve_next_address_fn));

    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_next_outer));
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_resolve_strm));
    f.instruction(&Instruction::Call(helpers.drop_resolve_stream_fn));
    emit_err_with_segment(&mut f, indices.no_addr_segment_idx, indices.no_addr_len);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_next_option));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_resolve_strm));
    f.instruction(&Instruction::Call(helpers.drop_resolve_stream_fn));
    emit_err_with_segment(&mut f, indices.no_addr_segment_idx, indices.no_addr_len);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_next_variant));
    f.instruction(&Instruction::BrIf(0));

    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_next_octet_a));
    f.instruction(&Instruction::LocalSet(l_ipv4_a));
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_next_octet_b));
    f.instruction(&Instruction::LocalSet(l_ipv4_b));
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_next_octet_c));
    f.instruction(&Instruction::LocalSet(l_ipv4_c));
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_next_octet_d));
    f.instruction(&Instruction::LocalSet(l_ipv4_d));

    f.instruction(&Instruction::Br(1));
    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // block

    f.instruction(&Instruction::LocalGet(l_resolve_strm));
    f.instruction(&Instruction::Call(helpers.drop_resolve_stream_fn));

    // ── create-tcp-socket + start/finish-connect. ─────────────
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::Call(helpers.create_tcp_socket_fn));

    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_resolve));
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_err_with_segment(&mut f, indices.sock_err_segment_idx, indices.sock_err_len);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load(mem4_resolve_off4));
    f.instruction(&Instruction::LocalSet(l_socket));

    f.instruction(&Instruction::LocalGet(l_socket));
    f.instruction(&Instruction::GlobalGet(helpers.network_handle_global));
    f.instruction(&Instruction::I32Const(0)); // ip-socket-address.ipv4
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalGet(l_ipv4_a));
    f.instruction(&Instruction::LocalGet(l_ipv4_b));
    f.instruction(&Instruction::LocalGet(l_ipv4_c));
    f.instruction(&Instruction::LocalGet(l_ipv4_d));
    for _ in 0..6 {
        f.instruction(&Instruction::I32Const(0));
    }
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::Call(helpers.start_connect_fn));

    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_resolve));
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_socket));
    f.instruction(&Instruction::Call(helpers.drop_tcp_socket_fn));
    emit_err_with_segment(&mut f, indices.conn_err_segment_idx, indices.conn_err_len);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(l_socket));
    f.instruction(&Instruction::Call(helpers.socket_subscribe_fn));
    f.instruction(&Instruction::LocalSet(l_pollable));

    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::LocalGet(l_pollable));
    f.instruction(&Instruction::I32Store(mem4_pollable_in));

    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Const(SCRATCH_OFFSET_POLLABLE_IN as i32));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Const(SCRATCH_OFFSET_POLL as i32));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::Call(helpers.poll_fn));

    f.instruction(&Instruction::LocalGet(l_pollable));
    f.instruction(&Instruction::Call(helpers.drop_pollable_fn));

    f.instruction(&Instruction::LocalGet(l_socket));
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::Call(helpers.finish_connect_fn));

    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_resolve));
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_socket));
    f.instruction(&Instruction::Call(helpers.drop_tcp_socket_fn));
    emit_err_with_segment(&mut f, indices.conn_err_segment_idx, indices.conn_err_len);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load(mem4_resolve_off4));
    f.instruction(&Instruction::LocalSet(l_in_handle));
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load(mem4_resolve_off8));
    f.instruction(&Instruction::LocalSet(l_out_handle));

    let _ = mem1_resolve_off4; // mark used for the canonical-ABI layout doc

    // ── Phase 2 — write request + half-close + read response. ─

    // Marshal payload into LM[0..data_len] + bump-cursor advance
    // past `data_len` so the response retptr / buffer don't land
    // on top of the payload (Phase 4.7+ fix #6).
    match flavor {
        TcpSendFlavor::String { .. } => {
            f.instruction(&Instruction::LocalGet(2));
            f.instruction(&Instruction::Call(helpers.str_to_lm_fn));
            f.instruction(&Instruction::LocalSet(l_data_len));
        }
        TcpSendFlavor::Bytes {
            list_int_type_idx,
            aint_struct_type_idx,
            ..
        } => {
            let l_list_cursor = l_list_cursor.expect("bytes flavor has list cursor local");
            f.instruction(&Instruction::I32Const(0));
            f.instruction(&Instruction::I32Const(0));
            f.instruction(&Instruction::I32Const(1));
            f.instruction(&Instruction::LocalGet(l_data_len));
            f.instruction(&Instruction::Call(helpers.cabi_realloc_fn));
            f.instruction(&Instruction::Drop);
            f.instruction(&Instruction::LocalGet(2));
            f.instruction(&Instruction::LocalSet(l_list_cursor));
            f.instruction(&Instruction::I32Const(0));
            f.instruction(&Instruction::LocalSet(l_off));
            f.instruction(&Instruction::Block(BlockType::Empty));
            f.instruction(&Instruction::Loop(BlockType::Empty));
            f.instruction(&Instruction::LocalGet(l_list_cursor));
            f.instruction(&Instruction::RefIsNull);
            f.instruction(&Instruction::BrIf(1));
            f.instruction(&Instruction::LocalGet(l_off));
            f.instruction(&Instruction::LocalGet(l_list_cursor));
            f.instruction(&Instruction::StructGet {
                struct_type_index: list_int_type_idx,
                field_index: 0,
            });
            if let Some(aint_type_idx) = aint_struct_type_idx {
                f.instruction(&Instruction::StructGet {
                    struct_type_index: aint_type_idx,
                    field_index: 0,
                });
            }
            f.instruction(&Instruction::I32WrapI64);
            f.instruction(&Instruction::I32Store8(mem1));
            f.instruction(&Instruction::LocalGet(l_list_cursor));
            f.instruction(&Instruction::StructGet {
                struct_type_index: list_int_type_idx,
                field_index: 1,
            });
            f.instruction(&Instruction::LocalSet(l_list_cursor));
            f.instruction(&Instruction::LocalGet(l_off));
            f.instruction(&Instruction::I32Const(1));
            f.instruction(&Instruction::I32Add);
            f.instruction(&Instruction::LocalSet(l_off));
            f.instruction(&Instruction::Br(0));
            f.instruction(&Instruction::End);
            f.instruction(&Instruction::End);
        }
    }

    f.instruction(&Instruction::LocalGet(l_data_len));
    f.instruction(&Instruction::I32Const(15));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(-16));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::GlobalGet(helpers.bump_alloc_ptr_global));
    f.instruction(&Instruction::I32GtU);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_data_len));
    f.instruction(&Instruction::I32Const(15));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(-16));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::GlobalSet(helpers.bump_alloc_ptr_global));
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(12));
    f.instruction(&Instruction::Call(helpers.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr));

    // Cleanup-and-Err lambdas — used on every error path past
    // this point (after we've owned the streams + socket).
    let drop_input_stream_fn = helpers.drop_input_stream_fn;
    let drop_output_stream_fn = helpers.drop_output_stream_fn;
    let drop_tcp_socket_fn = helpers.drop_tcp_socket_fn;
    let write_err_segment_idx = indices.write_err_segment_idx;
    let write_err_len = indices.write_err_len;
    let emit_drop_then_write_err = |f: &mut Function| {
        f.instruction(&Instruction::LocalGet(l_in_handle));
        f.instruction(&Instruction::Call(drop_input_stream_fn));
        f.instruction(&Instruction::LocalGet(l_out_handle));
        f.instruction(&Instruction::Call(drop_output_stream_fn));
        f.instruction(&Instruction::LocalGet(l_socket));
        f.instruction(&Instruction::Call(drop_tcp_socket_fn));
        emit_err_with_segment(f, write_err_segment_idx, write_err_len);
    };
    let stream_err_segment_idx = indices.stream_err_segment_idx;
    let stream_err_len = indices.stream_err_len;
    let emit_drop_then_stream_err = |f: &mut Function| {
        f.instruction(&Instruction::LocalGet(l_in_handle));
        f.instruction(&Instruction::Call(drop_input_stream_fn));
        f.instruction(&Instruction::LocalGet(l_out_handle));
        f.instruction(&Instruction::Call(drop_output_stream_fn));
        f.instruction(&Instruction::LocalGet(l_socket));
        f.instruction(&Instruction::Call(drop_tcp_socket_fn));
        emit_err_with_segment(f, stream_err_segment_idx, stream_err_len);
    };
    let size_err_segment_idx = indices.size_err_segment_idx;
    let size_err_len = indices.size_err_len;
    let emit_drop_then_size_err = |f: &mut Function| {
        f.instruction(&Instruction::LocalGet(l_in_handle));
        f.instruction(&Instruction::Call(drop_input_stream_fn));
        f.instruction(&Instruction::LocalGet(l_out_handle));
        f.instruction(&Instruction::Call(drop_output_stream_fn));
        f.instruction(&Instruction::LocalGet(l_socket));
        f.instruction(&Instruction::Call(drop_tcp_socket_fn));
        emit_err_with_segment(f, size_err_segment_idx, size_err_len);
    };

    // Chunked write LM[0..data_len] on out_handle.
    let blocking_write_fn = helpers.blocking_write_fn;
    super::super::wasip2_helpers::emit_chunked_blocking_write(
        &mut f,
        l_data_len,
        l_off,
        blocking_write_fn,
        &|f| {
            f.instruction(&Instruction::LocalGet(l_out_handle));
        },
        &|f| {
            f.instruction(&Instruction::LocalGet(l_retptr));
        },
        Some(&emit_drop_then_write_err),
    );

    // shutdown(socket, send=1, retptr) — host knows we're done
    // writing and can start flushing its response.
    f.instruction(&Instruction::LocalGet(l_socket));
    f.instruction(&Instruction::I32Const(1)); // shutdown-type.send
    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::Call(helpers.shutdown_fn));

    // Response buffer — starts at 4 KiB, doubles on demand, capped
    // at 10 MiB total payload (Phase 4.7+ fix #18).
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Const(4096));
    f.instruction(&Instruction::Call(helpers.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_buf_ptr));
    f.instruction(&Instruction::I32Const(4096));
    f.instruction(&Instruction::LocalSet(l_buf_cap));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(l_buf_len));

    // Read loop.
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));

    f.instruction(&Instruction::LocalGet(l_in_handle));
    f.instruction(&Instruction::I64Const(4096));
    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::Call(helpers.blocking_read_fn));

    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    // Err arm: variant tag at retptr+4. 0 = last-operation-failed,
    // 1 = closed. Phase 4.7+ fix #17: closed is a clean half-close
    // (treat as EOF, surface accumulated buffer); last-operation-
    // failed is a real I/O error.
    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::I32Load8U(MemArg {
        offset: 4,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_drop_then_stream_err(&mut f);
    f.instruction(&Instruction::End);
    // Closed variant → break out of loop with Ok(buf).
    f.instruction(&Instruction::Br(2));
    f.instruction(&Instruction::End);

    // Ok arm — (data_ptr, data_len) at retptr+4 / retptr+8.
    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::I32Load(mem4_o4));
    f.instruction(&Instruction::LocalSet(l_read_ptr));
    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::I32Load(mem4_o8));
    f.instruction(&Instruction::LocalSet(l_read_len));

    f.instruction(&Instruction::LocalGet(l_read_len));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::Br(2));
    f.instruction(&Instruction::End);

    // Size cap (Phase 4.7+ fix #18) — 10 MiB total payload.
    f.instruction(&Instruction::LocalGet(l_buf_len));
    f.instruction(&Instruction::LocalGet(l_read_len));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(10 * 1024 * 1024));
    f.instruction(&Instruction::I32GtU);
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_drop_then_size_err(&mut f);
    f.instruction(&Instruction::End);

    // Grow buffer if needed.
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_buf_len));
    f.instruction(&Instruction::LocalGet(l_read_len));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(l_buf_cap));
    f.instruction(&Instruction::I32LeU);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(l_buf_cap));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::LocalSet(l_new_cap));
    f.instruction(&Instruction::LocalGet(l_buf_ptr));
    f.instruction(&Instruction::LocalGet(l_buf_cap));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(l_new_cap));
    f.instruction(&Instruction::Call(helpers.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_buf_ptr));
    f.instruction(&Instruction::LocalGet(l_new_cap));
    f.instruction(&Instruction::LocalSet(l_buf_cap));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // grow loop
    f.instruction(&Instruction::End); // grow block

    // memory.copy LM[read_ptr..read_ptr+read_len] →
    // LM[buf_ptr+buf_len..]
    f.instruction(&Instruction::LocalGet(l_buf_ptr));
    f.instruction(&Instruction::LocalGet(l_buf_len));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(l_read_ptr));
    f.instruction(&Instruction::LocalGet(l_read_len));
    f.instruction(&Instruction::MemoryCopy {
        src_mem: 0,
        dst_mem: 0,
    });

    f.instruction(&Instruction::LocalGet(l_buf_len));
    f.instruction(&Instruction::LocalGet(l_read_len));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l_buf_len));

    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // read loop
    f.instruction(&Instruction::End); // read block

    // ── Resource cleanup. ─────────────────────────────────────
    f.instruction(&Instruction::LocalGet(l_in_handle));
    f.instruction(&Instruction::Call(helpers.drop_input_stream_fn));
    f.instruction(&Instruction::LocalGet(l_out_handle));
    f.instruction(&Instruction::Call(helpers.drop_output_stream_fn));
    f.instruction(&Instruction::LocalGet(l_socket));
    f.instruction(&Instruction::Call(helpers.drop_tcp_socket_fn));

    match flavor {
        TcpSendFlavor::String {
            result_type_idx, ..
        } => {
            f.instruction(&Instruction::LocalGet(l_buf_len));
            f.instruction(&Instruction::ArrayNewDefault(indices.string_type_idx));
            f.instruction(&Instruction::LocalSet(l_arr));
            f.instruction(&Instruction::I32Const(0));
            f.instruction(&Instruction::LocalSet(l_j));
            f.instruction(&Instruction::Block(BlockType::Empty));
            f.instruction(&Instruction::Loop(BlockType::Empty));
            f.instruction(&Instruction::LocalGet(l_j));
            f.instruction(&Instruction::LocalGet(l_buf_len));
            f.instruction(&Instruction::I32GeU);
            f.instruction(&Instruction::BrIf(1));
            f.instruction(&Instruction::LocalGet(l_arr));
            f.instruction(&Instruction::LocalGet(l_j));
            f.instruction(&Instruction::LocalGet(l_buf_ptr));
            f.instruction(&Instruction::LocalGet(l_j));
            f.instruction(&Instruction::I32Add);
            f.instruction(&Instruction::I32Load8U(mem1));
            f.instruction(&Instruction::ArraySet(indices.string_type_idx));
            f.instruction(&Instruction::LocalGet(l_j));
            f.instruction(&Instruction::I32Const(1));
            f.instruction(&Instruction::I32Add);
            f.instruction(&Instruction::LocalSet(l_j));
            f.instruction(&Instruction::Br(0));
            f.instruction(&Instruction::End);
            f.instruction(&Instruction::End);

            f.instruction(&Instruction::I32Const(1));
            f.instruction(&Instruction::LocalGet(l_arr));
            f.instruction(&Instruction::RefNull(HeapType::Concrete(
                indices.string_type_idx,
            )));
            f.instruction(&Instruction::StructNew(result_type_idx));
        }
        TcpSendFlavor::Bytes {
            list_int_type_idx,
            result_type_idx,
            aint_struct_type_idx,
            aint_from_i64_fn,
            ..
        } => {
            let l_list_cursor = l_list_cursor.expect("bytes flavor has list cursor local");
            f.instruction(&Instruction::RefNull(HeapType::Concrete(list_int_type_idx)));
            f.instruction(&Instruction::LocalSet(l_list_cursor));
            f.instruction(&Instruction::LocalGet(l_buf_len));
            f.instruction(&Instruction::LocalSet(l_j));
            f.instruction(&Instruction::Block(BlockType::Empty));
            f.instruction(&Instruction::Loop(BlockType::Empty));
            f.instruction(&Instruction::LocalGet(l_j));
            f.instruction(&Instruction::I32Eqz);
            f.instruction(&Instruction::BrIf(1));
            f.instruction(&Instruction::LocalGet(l_j));
            f.instruction(&Instruction::I32Const(1));
            f.instruction(&Instruction::I32Sub);
            f.instruction(&Instruction::LocalSet(l_j));
            f.instruction(&Instruction::LocalGet(l_buf_ptr));
            f.instruction(&Instruction::LocalGet(l_j));
            f.instruction(&Instruction::I32Add);
            f.instruction(&Instruction::I32Load8U(mem1));
            f.instruction(&Instruction::I64ExtendI32U);
            if aint_struct_type_idx.is_some() {
                f.instruction(&Instruction::Call(
                    aint_from_i64_fn.expect("bignum sendBytes needs __aint_from_i64"),
                ));
            }
            f.instruction(&Instruction::LocalGet(l_list_cursor));
            f.instruction(&Instruction::StructNew(list_int_type_idx));
            f.instruction(&Instruction::LocalSet(l_list_cursor));
            f.instruction(&Instruction::Br(0));
            f.instruction(&Instruction::End);
            f.instruction(&Instruction::End);

            f.instruction(&Instruction::I32Const(1));
            f.instruction(&Instruction::LocalGet(l_list_cursor));
            f.instruction(&Instruction::RefNull(HeapType::Concrete(
                indices.string_type_idx,
            )));
            f.instruction(&Instruction::StructNew(result_type_idx));
        }
    }
    restore_bump(&mut f, l_saved_alloc, helpers.bump_alloc_ptr_global);
    f.instruction(&Instruction::End);
    f
}

/// Phase 4.7+ pass 5 fix #21 — `__rt_tcp_ping(host, port) ->
/// ref Result<Unit, String>` slot bundle. Pre-pass-5 ping routed
/// through `__rt_tcp_connect` (pool-allocating) then
/// `__rt_tcp_close`; a program holding 256 live `Tcp.connect`
/// handles would then see `Tcp.ping` fail with `"tcp: connection
/// limit reached (256 max)"` even though `aver-rt::tcp::ping`
/// makes a fresh, pool-free socket and drops it. Pass 5 inlines
/// the same DNS + create + connect prologue `Tcp.send` uses, then
/// drops streams + socket and returns `Result.Ok(())` — no pool
/// involvement, mirroring VM/self-host semantics.
///
/// v1 still has no 1-second connect timeout — wasi-sockets
/// `start-connect` is best-effort and may block longer than
/// expected. A timeout-race variant lands as a follow-up once we
/// surface `subscribe-duration` + multi-pollable `poll` as a
/// general capability.
pub(in crate::codegen::wasm_gc) struct TcpPingIndices {
    pub fn_type: u32,
    pub fn_idx: u32,
    pub string_type_idx: u32,
    /// `"tcp: dns resolve failed"` data segment + length.
    pub dns_err_segment_idx: u32,
    pub dns_err_len: u32,
    /// `"tcp: dns no addresses"` data segment + length.
    pub no_addr_segment_idx: u32,
    pub no_addr_len: u32,
    /// `"tcp: socket create failed"` data segment + length.
    pub sock_err_segment_idx: u32,
    pub sock_err_len: u32,
    /// `"tcp: connect failed"` data segment + length.
    pub conn_err_segment_idx: u32,
    pub conn_err_len: u32,
    /// `"tcp: port out of range"` data segment + length.
    pub port_err_segment_idx: u32,
    pub port_err_len: u32,
}

pub(in crate::codegen::wasm_gc) struct TcpPingHelperFns {
    pub instance_network_fn: u32,
    pub network_handle_global: u32,
    pub resolve_addresses_fn: u32,
    pub resolve_next_address_fn: u32,
    pub drop_resolve_stream_fn: u32,
    pub stream_subscribe_fn: u32,
    pub poll_fn: u32,
    pub drop_pollable_fn: u32,
    pub create_tcp_socket_fn: u32,
    pub start_connect_fn: u32,
    pub finish_connect_fn: u32,
    pub socket_subscribe_fn: u32,
    pub drop_tcp_socket_fn: u32,
    pub drop_input_stream_fn: u32,
    pub drop_output_stream_fn: u32,
    pub str_to_lm_fn: u32,
    pub cabi_realloc_fn: u32,
    pub bump_alloc_ptr_global: u32,
    pub result_unit_string_ok_fn: u32,
    pub result_unit_string_err_fn: u32,
}

pub(in crate::codegen::wasm_gc) fn emit_tcp_ping(
    indices: &TcpPingIndices,
    helpers: &TcpPingHelperFns,
) -> Function {
    use wasm_encoder::{BlockType, MemArg};

    // Locals beyond the two params (0=host: ref string, 1=port: i64):
    //   2  = saved_alloc   (i32) — bump-rewind cursor
    //   3  = host_len      (i32)
    //   4  = scratch       (i32) — 64-byte retptr block
    //   5  = resolve_strm  (i32)
    //   6  = pollable      (i32)
    //   7..=10 = ipv4 octets (i32)
    //   11 = socket        (i32)
    let mut f = Function::new(vec![(10u32, ValType::I32)]);
    let l_saved_alloc: u32 = 2;
    let l_host_len: u32 = 3;
    let l_scratch: u32 = 4;
    let l_resolve_strm: u32 = 5;
    let l_pollable: u32 = 6;
    let l_ipv4_a: u32 = 7;
    let l_ipv4_b: u32 = 8;
    let l_ipv4_c: u32 = 9;
    let l_ipv4_d: u32 = 10;
    let l_socket: u32 = 11;

    // Scratch layout — same as send / connect.
    const SCRATCH_BLOCK_SIZE: i32 = 64;
    const SCRATCH_OFFSET_RESOLVE: u32 = 0;
    const SCRATCH_OFFSET_NEXT: u32 = 16;
    const SCRATCH_OFFSET_POLL: u32 = 48;
    const SCRATCH_OFFSET_POLLABLE_IN: u32 = 56;

    let mem4_resolve_off4 = MemArg {
        offset: u64::from(SCRATCH_OFFSET_RESOLVE + 4),
        align: 2,
        memory_index: 0,
    };
    let mem4_resolve_off8 = MemArg {
        offset: u64::from(SCRATCH_OFFSET_RESOLVE + 8),
        align: 2,
        memory_index: 0,
    };
    let mem1_resolve = MemArg {
        offset: u64::from(SCRATCH_OFFSET_RESOLVE),
        align: 0,
        memory_index: 0,
    };
    let mem1_next_outer = MemArg {
        offset: u64::from(SCRATCH_OFFSET_NEXT),
        align: 0,
        memory_index: 0,
    };
    let mem1_next_option = MemArg {
        offset: u64::from(SCRATCH_OFFSET_NEXT + 2),
        align: 0,
        memory_index: 0,
    };
    let mem1_next_variant = MemArg {
        offset: u64::from(SCRATCH_OFFSET_NEXT + 4),
        align: 0,
        memory_index: 0,
    };
    let mem1_next_octet_a = MemArg {
        offset: u64::from(SCRATCH_OFFSET_NEXT + 6),
        align: 0,
        memory_index: 0,
    };
    let mem1_next_octet_b = MemArg {
        offset: u64::from(SCRATCH_OFFSET_NEXT + 7),
        align: 0,
        memory_index: 0,
    };
    let mem1_next_octet_c = MemArg {
        offset: u64::from(SCRATCH_OFFSET_NEXT + 8),
        align: 0,
        memory_index: 0,
    };
    let mem1_next_octet_d = MemArg {
        offset: u64::from(SCRATCH_OFFSET_NEXT + 9),
        align: 0,
        memory_index: 0,
    };
    let mem4_pollable_in = MemArg {
        offset: u64::from(SCRATCH_OFFSET_POLLABLE_IN),
        align: 2,
        memory_index: 0,
    };

    let string_type_idx = indices.string_type_idx;
    let result_err_fn = helpers.result_unit_string_err_fn;
    let bump_alloc_ptr_global = helpers.bump_alloc_ptr_global;
    let emit_err_with_segment = |f: &mut Function, seg: u32, len: u32| {
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(len as i32));
        f.instruction(&Instruction::ArrayNewData {
            array_type_index: string_type_idx,
            array_data_index: seg,
        });
        f.instruction(&Instruction::Call(result_err_fn));
        restore_bump(f, l_saved_alloc, bump_alloc_ptr_global);
        f.instruction(&Instruction::Return);
    };

    // ── Prolog. ────────────────────────────────────────────────
    f.instruction(&Instruction::GlobalGet(helpers.bump_alloc_ptr_global));
    f.instruction(&Instruction::LocalSet(l_saved_alloc));

    // Port validation.
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::I64LtS);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Const(65535));
    f.instruction(&Instruction::I64GtS);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_err_with_segment(&mut f, indices.port_err_segment_idx, indices.port_err_len);
    f.instruction(&Instruction::End);

    // Lazy `network` handle.
    f.instruction(&Instruction::GlobalGet(helpers.network_handle_global));
    f.instruction(&Instruction::I32Const(-1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::Call(helpers.instance_network_fn));
    f.instruction(&Instruction::GlobalSet(helpers.network_handle_global));
    f.instruction(&Instruction::End);

    // ── DNS resolve. ───────────────────────────────────────────
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Call(helpers.str_to_lm_fn));
    f.instruction(&Instruction::LocalSet(l_host_len));

    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(SCRATCH_BLOCK_SIZE));
    f.instruction(&Instruction::Call(helpers.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_scratch));

    f.instruction(&Instruction::GlobalGet(helpers.network_handle_global));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(l_host_len));
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::Call(helpers.resolve_addresses_fn));

    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_resolve));
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_err_with_segment(&mut f, indices.dns_err_segment_idx, indices.dns_err_len);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load(mem4_resolve_off4));
    f.instruction(&Instruction::LocalSet(l_resolve_strm));

    // resolve-next-address loop, first IPv4 wins.
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));

    f.instruction(&Instruction::LocalGet(l_resolve_strm));
    f.instruction(&Instruction::Call(helpers.stream_subscribe_fn));
    f.instruction(&Instruction::LocalSet(l_pollable));

    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::LocalGet(l_pollable));
    f.instruction(&Instruction::I32Store(mem4_pollable_in));

    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Const(SCRATCH_OFFSET_POLLABLE_IN as i32));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Const(SCRATCH_OFFSET_POLL as i32));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::Call(helpers.poll_fn));

    f.instruction(&Instruction::LocalGet(l_pollable));
    f.instruction(&Instruction::Call(helpers.drop_pollable_fn));

    f.instruction(&Instruction::LocalGet(l_resolve_strm));
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Const(SCRATCH_OFFSET_NEXT as i32));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::Call(helpers.resolve_next_address_fn));

    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_next_outer));
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_resolve_strm));
    f.instruction(&Instruction::Call(helpers.drop_resolve_stream_fn));
    emit_err_with_segment(&mut f, indices.no_addr_segment_idx, indices.no_addr_len);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_next_option));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_resolve_strm));
    f.instruction(&Instruction::Call(helpers.drop_resolve_stream_fn));
    emit_err_with_segment(&mut f, indices.no_addr_segment_idx, indices.no_addr_len);
    f.instruction(&Instruction::End);

    // ip-address variant tag — IPv6 (tag != 0) skipped, IPv4 latches.
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_next_variant));
    f.instruction(&Instruction::BrIf(0));

    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_next_octet_a));
    f.instruction(&Instruction::LocalSet(l_ipv4_a));
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_next_octet_b));
    f.instruction(&Instruction::LocalSet(l_ipv4_b));
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_next_octet_c));
    f.instruction(&Instruction::LocalSet(l_ipv4_c));
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_next_octet_d));
    f.instruction(&Instruction::LocalSet(l_ipv4_d));

    f.instruction(&Instruction::Br(1));
    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // block

    f.instruction(&Instruction::LocalGet(l_resolve_strm));
    f.instruction(&Instruction::Call(helpers.drop_resolve_stream_fn));

    // ── create-tcp-socket + start/finish-connect. ─────────────
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::Call(helpers.create_tcp_socket_fn));

    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_resolve));
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_err_with_segment(&mut f, indices.sock_err_segment_idx, indices.sock_err_len);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load(mem4_resolve_off4));
    f.instruction(&Instruction::LocalSet(l_socket));

    f.instruction(&Instruction::LocalGet(l_socket));
    f.instruction(&Instruction::GlobalGet(helpers.network_handle_global));
    f.instruction(&Instruction::I32Const(0)); // ip-socket-address.ipv4
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalGet(l_ipv4_a));
    f.instruction(&Instruction::LocalGet(l_ipv4_b));
    f.instruction(&Instruction::LocalGet(l_ipv4_c));
    f.instruction(&Instruction::LocalGet(l_ipv4_d));
    for _ in 0..6 {
        f.instruction(&Instruction::I32Const(0));
    }
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::Call(helpers.start_connect_fn));

    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_resolve));
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_socket));
    f.instruction(&Instruction::Call(helpers.drop_tcp_socket_fn));
    emit_err_with_segment(&mut f, indices.conn_err_segment_idx, indices.conn_err_len);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(l_socket));
    f.instruction(&Instruction::Call(helpers.socket_subscribe_fn));
    f.instruction(&Instruction::LocalSet(l_pollable));

    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::LocalGet(l_pollable));
    f.instruction(&Instruction::I32Store(mem4_pollable_in));

    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Const(SCRATCH_OFFSET_POLLABLE_IN as i32));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Const(SCRATCH_OFFSET_POLL as i32));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::Call(helpers.poll_fn));

    f.instruction(&Instruction::LocalGet(l_pollable));
    f.instruction(&Instruction::Call(helpers.drop_pollable_fn));

    f.instruction(&Instruction::LocalGet(l_socket));
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::Call(helpers.finish_connect_fn));

    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_resolve));
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_socket));
    f.instruction(&Instruction::Call(helpers.drop_tcp_socket_fn));
    emit_err_with_segment(&mut f, indices.conn_err_segment_idx, indices.conn_err_len);
    f.instruction(&Instruction::End);

    // Connect succeeded. Drop the streams + socket and return Ok(()).
    // No pool slot was ever claimed, so a 256-conn-full pool can
    // still issue any number of pings.
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load(mem4_resolve_off4));
    f.instruction(&Instruction::Call(helpers.drop_input_stream_fn));
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load(mem4_resolve_off8));
    f.instruction(&Instruction::Call(helpers.drop_output_stream_fn));
    f.instruction(&Instruction::LocalGet(l_socket));
    f.instruction(&Instruction::Call(helpers.drop_tcp_socket_fn));

    f.instruction(&Instruction::Call(helpers.result_unit_string_ok_fn));
    restore_bump(&mut f, l_saved_alloc, helpers.bump_alloc_ptr_global);
    f.instruction(&Instruction::End);
    f
}

/// Phase 4.3 emit — `__rt_tcp_close` body. Trust contract:
/// `conn` came out of a successful `Tcp.connect` on this run, so
/// the pool slot at `parse_id(conn.id)` is guaranteed to be a
/// non-null `$tcp_slot` ref (Phase 4.2.2d stored it via array.set).
/// Slots marked `in_use = 0` (already-closed) make `close` a
/// no-op so the call stays idempotent.
pub(in crate::codegen::wasm_gc) fn emit_tcp_close(
    indices: &TcpCloseIndices,
    helpers: &TcpCloseHelperFns,
) -> Function {
    use wasm_encoder::BlockType;
    // Locals beyond param 0 = conn (ref Tcp.Connection):
    //   1 = parsed_id (i32)        — full counter value
    //   2 = slot_idx  (i32)        — parsed_id & 255
    //   3 = slot      (ref $tcp_slot)
    //   4 = retptr    (i32)
    let slot_ref = ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(indices.tcp_slot_type_idx),
    });
    // local 5 = saved_alloc (Phase 4.2.2f bump-heap rewind).
    let mut f = Function::new(vec![
        (2u32, ValType::I32),
        (1u32, slot_ref),
        (2u32, ValType::I32),
    ]);
    let l_parsed_id: u32 = 1;
    let l_slot_idx: u32 = 2;
    let l_slot: u32 = 3;
    let l_retptr: u32 = 4;
    let l_saved_alloc: u32 = 5;

    // Save bump_alloc_ptr — restored on every exit (idempotence
    // guard Return + final End).
    f.instruction(&Instruction::GlobalGet(helpers.bump_alloc_ptr_global));
    f.instruction(&Instruction::LocalSet(l_saved_alloc));

    // Shared `Err("tcp: unknown connection")` emitter used by every
    // stale-conn guard. Matches `aver-rt::tcp::close` semantics —
    // stale ids surface as `Err`, not silent `Ok` no-ops (Phase
    // 4.7+ cross-backend alignment).
    let unknown_segment_idx = indices.unknown_segment_idx;
    let unknown_len = indices.unknown_len;
    let string_type_idx = indices.string_type_idx;
    let result_err_fn = helpers.result_err_fn;
    let bump_alloc_ptr_global = helpers.bump_alloc_ptr_global;
    let emit_unknown_err = |f: &mut Function| {
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(unknown_len as i32));
        f.instruction(&Instruction::ArrayNewData {
            array_type_index: string_type_idx,
            array_data_index: unknown_segment_idx,
        });
        f.instruction(&Instruction::Call(result_err_fn));
        restore_bump(f, l_saved_alloc, bump_alloc_ptr_global);
        f.instruction(&Instruction::Return);
    };

    // parsed_id = parse_id(conn.id) — full monotonic counter value
    // baked into the id string at connect time.
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_connection_type_idx,
        field_index: 0, // id
    });
    f.instruction(&Instruction::Call(helpers.parse_id_fn));
    f.instruction(&Instruction::LocalSet(l_parsed_id));

    // Null-pool guard (Phase 4.7+ fix #8): `tcp_pool` stays null
    // until the first real `Tcp.connect` lazy-inits it; if a
    // user-crafted (or pre-connect) `Tcp.Connection` reaches close,
    // surface the same Err as every other stale-conn path rather
    // than trap on `array.get null`.
    f.instruction(&Instruction::GlobalGet(helpers.tcp_pool_global));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_unknown_err(&mut f);
    f.instruction(&Instruction::End);

    // Phase 4.7+ fix #14 — id_value scan. The slot index is no
    // longer `parsed_id & 255`; connect's pool allocator picks the
    // first non-busy slot in pool order, so close has to walk the
    // pool looking for the slot whose `id_value == parsed_id` and
    // `in_use == 1`. Walking off the end without a match means the
    // handle is stale (closed and re-claimed, or never live).
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(l_slot_idx));
    f.instruction(&Instruction::Block(BlockType::Empty)); // $found
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_slot_idx));
    f.instruction(&Instruction::I32Const(256));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_unknown_err(&mut f);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::GlobalGet(helpers.tcp_pool_global));
    f.instruction(&Instruction::LocalGet(l_slot_idx));
    f.instruction(&Instruction::ArrayGet(indices.tcp_pool_type_idx));
    f.instruction(&Instruction::LocalSet(l_slot));
    f.instruction(&Instruction::LocalGet(l_slot));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    // Non-null slot: check id_value match + in_use.
    f.instruction(&Instruction::LocalGet(l_slot));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_slot_type_idx,
        field_index: 4, // id_value
    });
    f.instruction(&Instruction::LocalGet(l_parsed_id));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::LocalGet(l_slot));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_slot_type_idx,
        field_index: 3, // in_use
    });
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::BrIf(2)); // matching live slot → break $found
    f.instruction(&Instruction::End); // non-null check
    f.instruction(&Instruction::LocalGet(l_slot_idx));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l_slot_idx));
    f.instruction(&Instruction::Br(0)); // continue scan
    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // $found block — l_slot is the match

    // Drop input-stream.
    f.instruction(&Instruction::LocalGet(l_slot));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_slot_type_idx,
        field_index: 1, // in_stream
    });
    f.instruction(&Instruction::Call(helpers.drop_input_stream_fn));

    // Drop output-stream.
    f.instruction(&Instruction::LocalGet(l_slot));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_slot_type_idx,
        field_index: 2, // out_stream
    });
    f.instruction(&Instruction::Call(helpers.drop_output_stream_fn));

    // shutdown(socket, both=2, retptr=cabi_realloc(2)). Result is
    // ignored — the socket is about to be dropped anyway, and the
    // POSIX semantics treat shutdown failure on an already-closed
    // peer as a no-op.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Const(2));
    f.instruction(&Instruction::Call(helpers.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr));
    f.instruction(&Instruction::LocalGet(l_slot));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_slot_type_idx,
        field_index: 0, // socket
    });
    f.instruction(&Instruction::I32Const(2)); // shutdown-type.both
    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::Call(helpers.shutdown_fn));

    // Drop socket.
    f.instruction(&Instruction::LocalGet(l_slot));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_slot_type_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::Call(helpers.drop_tcp_socket_fn));

    // Mark slot in_use = 0 — subsequent close() calls become no-ops.
    f.instruction(&Instruction::LocalGet(l_slot));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::StructSet {
        struct_type_index: indices.tcp_slot_type_idx,
        field_index: 3,
    });

    f.instruction(&Instruction::Call(helpers.result_ok_fn));
    restore_bump(&mut f, l_saved_alloc, helpers.bump_alloc_ptr_global);
    f.instruction(&Instruction::End);
    f
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn close_emit_compiles() {
        let indices = TcpCloseIndices {
            fn_type: 0,
            fn_idx: 0,
            tcp_connection_type_idx: 1,
            tcp_slot_type_idx: 2,
            tcp_pool_type_idx: 3,
            string_type_idx: 4,
            unknown_segment_idx: 0,
            unknown_len: b"tcp: unknown connection".len() as u32,
        };
        let helpers = TcpCloseHelperFns {
            parse_id_fn: 4,
            cabi_realloc_fn: 5,
            shutdown_fn: 6,
            drop_input_stream_fn: 7,
            drop_output_stream_fn: 8,
            drop_tcp_socket_fn: 9,
            result_ok_fn: 10,
            result_err_fn: 11,
            tcp_pool_global: 0,
            bump_alloc_ptr_global: 1,
        };
        let _f = emit_tcp_close(&indices, &helpers);
    }
}

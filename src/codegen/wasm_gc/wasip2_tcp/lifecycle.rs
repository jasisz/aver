//! Resource-teardown + orchestration helpers — `__rt_tcp_close`,
//! `__rt_tcp_send`, `__rt_tcp_ping`. Close releases the pool slot's
//! streams + socket and flips `in_use = 0` so subsequent calls
//! become no-ops. Send / ping are thin orchestrators built on top
//! of connect + writeLine + readLine + close.

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
    /// `__rt_result_unit_string_ok()` factory. `Tcp.close` is
    /// idempotent — even a stale slot returns `Ok(())`.
    pub result_ok_fn: u32,
    /// `tcp_pool: ref null $tcp_pool` global.
    pub tcp_pool_global: u32,
    /// Phase 4.2.2f — see `TcpConnectHelperFns::bump_alloc_ptr_global`.
    pub bump_alloc_ptr_global: u32,
}
/// Phase 4.5a — `__rt_tcp_send(host, port, data) ->
/// ref Result<String, String>` slot bundle. Orchestrates the
/// full one-shot pipeline (connect + writeLine + readLine + close)
/// by calling the per-method helpers and threading their Result
/// values through manual tag inspection.
pub(in crate::codegen::wasm_gc) struct TcpSendIndices {
    pub fn_type: u32,
    pub fn_idx: u32,
    pub string_type_idx: u32,
    /// `Result<Tcp.Connection, String>` struct type idx — `connect`
    /// returns it; the orchestrator reads its tag + extracts the
    /// `ok` (conn) and `err` (string) fields.
    pub result_tcp_conn_string_type_idx: u32,
    /// `Result<Unit, String>` — `writeLine`'s return shape.
    pub result_unit_string_type_idx: u32,
}

pub(in crate::codegen::wasm_gc) struct TcpSendHelperFns {
    pub tcp_connect_fn: u32,
    pub tcp_write_line_fn: u32,
    pub tcp_read_line_fn: u32,
    pub tcp_close_fn: u32,
    /// `__rt_result_string_string_err(message) ->
    ///   ref Result<String, String>`. Used when an earlier stage's
    /// Err needs to be re-wrapped under the send-side result type
    /// (e.g. connect failed → send returns Result<String, String>::Err).
    pub result_string_string_err_fn: u32,
}

pub(in crate::codegen::wasm_gc) fn emit_tcp_send(
    indices: &TcpSendIndices,
    helpers: &TcpSendHelperFns,
) -> Function {
    use wasm_encoder::{BlockType, HeapType, RefType};

    // Carrier strategy: stash the connect helper's
    // `ref Result<Tcp.Connection, String>` and the write helper's
    // `ref Result<Unit, String>` in their own typed locals, then
    // re-extract the `Tcp.Connection` field whenever a downstream
    // call needs it. Result types are unrelated heap shapes — wasm
    // doesn't have subtyping between two struct types here, so
    // every locally-typed value has to live in a local that matches
    // the producer's return type exactly.
    let result_tcp_conn_string_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.result_tcp_conn_string_type_idx),
    });
    let result_unit_string_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.result_unit_string_type_idx),
    });
    let mut f = Function::new(vec![
        (1u32, result_tcp_conn_string_ref),
        (1u32, result_unit_string_ref),
    ]);
    let l_connect_result: u32 = 3;
    let l_write_result: u32 = 4;

    // connect_result = __rt_tcp_connect(host, port)
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(helpers.tcp_connect_fn));
    f.instruction(&Instruction::LocalSet(l_connect_result));

    // tag == 0 (Err) → re-wrap connect_result.err as
    // Result<String, String>.Err and return.
    f.instruction(&Instruction::LocalGet(l_connect_result));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.result_tcp_conn_string_type_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_connect_result));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.result_tcp_conn_string_type_idx,
        field_index: 2,
    });
    f.instruction(&Instruction::Call(helpers.result_string_string_err_fn));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // Ok arm — push (conn, data) and call write_line.
    f.instruction(&Instruction::LocalGet(l_connect_result));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.result_tcp_conn_string_type_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::Call(helpers.tcp_write_line_fn));
    f.instruction(&Instruction::LocalSet(l_write_result));

    // tag == 0 (Err) → close (best-effort) + re-wrap write_result.err.
    f.instruction(&Instruction::LocalGet(l_write_result));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.result_unit_string_type_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_connect_result));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.result_tcp_conn_string_type_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::Call(helpers.tcp_close_fn));
    f.instruction(&Instruction::Drop);
    f.instruction(&Instruction::LocalGet(l_write_result));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.result_unit_string_type_idx,
        field_index: 2,
    });
    f.instruction(&Instruction::Call(helpers.result_string_string_err_fn));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // read_result = __rt_tcp_read_line(conn) — already
    // Result<String, String>; bubbles up as the function's own
    // return value, with a best-effort close in between.
    f.instruction(&Instruction::LocalGet(l_connect_result));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.result_tcp_conn_string_type_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::Call(helpers.tcp_read_line_fn));
    f.instruction(&Instruction::LocalGet(l_connect_result));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.result_tcp_conn_string_type_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::Call(helpers.tcp_close_fn));
    f.instruction(&Instruction::Drop);
    let _ = indices.string_type_idx;
    f.instruction(&Instruction::End);
    f
}

/// Phase 4.5b — `__rt_tcp_ping(host, port) -> ref Result<Unit, String>`
/// slot bundle. Light wrapper around connect + close:
///
///   1. connect_result = __rt_tcp_connect(host, port)
///   2. tag == 0 (Err) → re-wrap as Result<Unit, String>.Err
///   3. Ok → drop conn (best-effort close) → Result.Ok(())
///
/// v1 has no 1-second connect timeout — wasi-sockets `start-connect`
/// is best-effort and may block longer than expected. A timeout-race
/// variant lands as a follow-up once we surface
/// `subscribe-duration` + multi-pollable `poll` to source as a
/// general capability.
pub(in crate::codegen::wasm_gc) struct TcpPingIndices {
    pub fn_type: u32,
    pub fn_idx: u32,
    pub result_tcp_conn_string_type_idx: u32,
}

pub(in crate::codegen::wasm_gc) struct TcpPingHelperFns {
    pub tcp_connect_fn: u32,
    pub tcp_close_fn: u32,
    pub result_unit_string_ok_fn: u32,
    pub result_unit_string_err_fn: u32,
}

pub(in crate::codegen::wasm_gc) fn emit_tcp_ping(
    indices: &TcpPingIndices,
    helpers: &TcpPingHelperFns,
) -> Function {
    use wasm_encoder::{BlockType, HeapType, RefType};

    let result_tcp_conn_string_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.result_tcp_conn_string_type_idx),
    });
    let mut f = Function::new(vec![(1u32, result_tcp_conn_string_ref)]);
    let l_connect_result: u32 = 2;

    // connect_result = __rt_tcp_connect(host, port)
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(helpers.tcp_connect_fn));
    f.instruction(&Instruction::LocalSet(l_connect_result));

    // tag == 0 (Err) → re-wrap err as Result<Unit, String>.Err.
    f.instruction(&Instruction::LocalGet(l_connect_result));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.result_tcp_conn_string_type_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_connect_result));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.result_tcp_conn_string_type_idx,
        field_index: 2,
    });
    f.instruction(&Instruction::Call(helpers.result_unit_string_err_fn));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // Ok — close conn (best-effort, drop result) + return Ok(()).
    f.instruction(&Instruction::LocalGet(l_connect_result));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.result_tcp_conn_string_type_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::Call(helpers.tcp_close_fn));
    f.instruction(&Instruction::Drop);

    f.instruction(&Instruction::Call(helpers.result_unit_string_ok_fn));
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

    // parsed_id = parse_id(conn.id) — full monotonic counter value
    // baked into the id string at connect time. The pool slot index
    // is the low 8 bits; the upper bits are the freshness tag we
    // cross-check against `slot.id_value` below (Phase 4.7 fix #2).
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_connection_type_idx,
        field_index: 0, // id
    });
    f.instruction(&Instruction::Call(helpers.parse_id_fn));
    f.instruction(&Instruction::LocalSet(l_parsed_id));

    // slot_idx = parsed_id & 255
    f.instruction(&Instruction::LocalGet(l_parsed_id));
    f.instruction(&Instruction::I32Const(255));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(l_slot_idx));

    // slot = tcp_pool[slot_idx]
    f.instruction(&Instruction::GlobalGet(helpers.tcp_pool_global));
    f.instruction(&Instruction::LocalGet(l_slot_idx));
    f.instruction(&Instruction::ArrayGet(indices.tcp_pool_type_idx));
    f.instruction(&Instruction::LocalSet(l_slot));

    // Idempotence / stale-id guard. Three early-return paths, all
    // returning Ok(()) so Tcp.close stays a safe no-op:
    //   1. slot is null (this slot index has never been claimed)
    //   2. slot.id_value != parsed_id (pool wrapped; we're holding
    //      a `Tcp.Connection` from a previous generation)
    //   3. slot.in_use == 0 (already closed)
    f.instruction(&Instruction::LocalGet(l_slot));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::Call(helpers.result_ok_fn));
    restore_bump(&mut f, l_saved_alloc, helpers.bump_alloc_ptr_global);
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(l_slot));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_slot_type_idx,
        field_index: 4, // id_value
    });
    f.instruction(&Instruction::LocalGet(l_parsed_id));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::Call(helpers.result_ok_fn));
    restore_bump(&mut f, l_saved_alloc, helpers.bump_alloc_ptr_global);
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(l_slot));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_slot_type_idx,
        field_index: 3, // in_use
    });
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::Call(helpers.result_ok_fn));
    restore_bump(&mut f, l_saved_alloc, helpers.bump_alloc_ptr_global);
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

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
        };
        let helpers = TcpCloseHelperFns {
            parse_id_fn: 4,
            cabi_realloc_fn: 5,
            shutdown_fn: 6,
            drop_input_stream_fn: 7,
            drop_output_stream_fn: 8,
            drop_tcp_socket_fn: 9,
            result_ok_fn: 10,
            tcp_pool_global: 0,
            bump_alloc_ptr_global: 1,
        };
        let _f = emit_tcp_close(&indices, &helpers);
    }
}

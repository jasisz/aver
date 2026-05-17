//! Resource-teardown + orchestration helpers — `__rt_tcp_close`,
//! `__rt_tcp_send`, `__rt_tcp_ping`.
//!
//! `close` releases the pool slot's streams + socket and surfaces
//! `Err("tcp: unknown connection")` on stale / null / already-closed
//! handles (cross-backend parity with `aver-rt::tcp::close`).
//!
//! `send` is a full one-shot pipeline: connect → raw chunked write
//! → shutdown(send) → read-to-EOF → close. Matches
//! `aver-rt::tcp::send`'s line-agnostic semantics (Phase 4.7+
//! fix #9): no trailing `\r\n` on the request, the full response
//! collected until the peer closes.
//!
//! `ping` stays a thin connect + close wrapper.

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
/// Phase 4.7+ fix #9 — full rewrite to match `aver-rt::tcp::send`
/// semantics. The previous orchestrator chained `Tcp.writeLine` +
/// `Tcp.readLine` + `Tcp.close`, which appended `\r\n` to the
/// payload and stopped reading at the first `\n` — wrong for any
/// non-line-oriented protocol (HTTP body, raw binary echo, redis
/// RESP arrays bigger than one frame). aver-rt sends the raw
/// bytes, calls `shutdown(Write)` to signal end-of-request, and
/// reads the response until EOF. This emitter does the same on
/// top of wasi-sockets: connect → bump_alloc payload → chunked
/// blocking_write → shutdown(send) → blocking_read 4 KiB at a
/// time into a growable buffer → close → wrap as Ok(response).
pub(in crate::codegen::wasm_gc) struct TcpSendIndices {
    pub fn_type: u32,
    pub fn_idx: u32,
    pub string_type_idx: u32,
    /// `Result<Tcp.Connection, String>` — what `Tcp.connect` returns.
    pub result_tcp_conn_string_type_idx: u32,
    /// `Result<String, String>` struct type idx — the send fn's own
    /// return type. We materialise the final Ok(...) via
    /// `struct.new` here rather than a factory call because the
    /// response string is constructed inline from the read buffer.
    pub result_string_string_type_idx: u32,
    pub tcp_connection_type_idx: u32,
    pub tcp_slot_type_idx: u32,
    pub tcp_pool_type_idx: u32,
    /// `"tcp: write failed"` — re-used by the per-chunk write
    /// failure branch (matches the writeLine error shape so users
    /// reading the error string get consistent prefixes).
    pub write_err_segment_idx: u32,
    pub write_err_len: u32,
}

pub(in crate::codegen::wasm_gc) struct TcpSendHelperFns {
    pub tcp_connect_fn: u32,
    pub tcp_close_fn: u32,
    pub parse_id_fn: u32,
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
    pub tcp_pool_global: u32,
    pub bump_alloc_ptr_global: u32,
    /// `__rt_result_string_string_err(message)` — every error path
    /// surfaces through here for cross-backend message parity.
    pub result_string_string_err_fn: u32,
}

pub(in crate::codegen::wasm_gc) fn emit_tcp_send(
    indices: &TcpSendIndices,
    helpers: &TcpSendHelperFns,
) -> Function {
    use wasm_encoder::{BlockType, HeapType, MemArg, RefType};

    let result_tcp_conn_string_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.result_tcp_conn_string_type_idx),
    });
    let conn_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.tcp_connection_type_idx),
    });
    let slot_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.tcp_slot_type_idx),
    });
    let s_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.string_type_idx),
    });

    // Locals (params 0=host, 1=port, 2=data):
    //   3  = connect_result (ref Result<Tcp.Connection, String>)
    //   4  = conn           (ref Tcp.Connection)
    //   5  = slot_idx       (i32)
    //   6  = slot           (ref $tcp_slot)
    //   7  = in_handle      (i32)
    //   8  = out_handle     (i32)
    //   9  = data_len       (i32)
    //   10 = off            (i32) — chunked-write cursor
    //   11 = retptr         (i32) — 12-byte stream-result retptr
    //   12 = buf_ptr        (i32) — growable response buffer base
    //   13 = buf_cap        (i32)
    //   14 = buf_len        (i32)
    //   15 = read_ptr       (i32) — payload-pointer from retptr+4
    //   16 = read_len       (i32) — payload-length from retptr+8
    //   17 = new_cap        (i32) — temporary for buffer doubling
    //   18 = saved_alloc    (i32) — Phase 4.2.2f rewind
    //   19 = arr            (ref string) — response materialisation
    //   20 = j              (i32) — array-copy index
    let mut f = Function::new(vec![
        (1u32, result_tcp_conn_string_ref),
        (1u32, conn_ref),
        (1u32, ValType::I32),
        (1u32, slot_ref),
        (12u32, ValType::I32),
        (1u32, s_ref),
        (1u32, ValType::I32),
    ]);
    let l_connect_result: u32 = 3;
    let l_conn: u32 = 4;
    let l_slot_idx: u32 = 5;
    let l_slot: u32 = 6;
    let l_in_handle: u32 = 7;
    let l_out_handle: u32 = 8;
    let l_data_len: u32 = 9;
    let l_off: u32 = 10;
    let l_retptr: u32 = 11;
    let l_buf_ptr: u32 = 12;
    let l_buf_cap: u32 = 13;
    let l_buf_len: u32 = 14;
    let l_read_ptr: u32 = 15;
    let l_read_len: u32 = 16;
    let l_new_cap: u32 = 17;
    let l_saved_alloc: u32 = 18;
    let l_arr: u32 = 19;
    let l_j: u32 = 20;

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

    f.instruction(&Instruction::GlobalGet(helpers.bump_alloc_ptr_global));
    f.instruction(&Instruction::LocalSet(l_saved_alloc));

    // Shared `Result<String, String>::Err("tcp: write failed")`
    // emitter for every mid-flight failure path.
    let write_err_segment_idx = indices.write_err_segment_idx;
    let write_err_len = indices.write_err_len;
    let string_type_idx = indices.string_type_idx;
    let result_string_string_err_fn = helpers.result_string_string_err_fn;
    let bump_alloc_ptr_global = helpers.bump_alloc_ptr_global;
    let emit_write_err = |f: &mut Function| {
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(write_err_len as i32));
        f.instruction(&Instruction::ArrayNewData {
            array_type_index: string_type_idx,
            array_data_index: write_err_segment_idx,
        });
        f.instruction(&Instruction::Call(result_string_string_err_fn));
        restore_bump(f, l_saved_alloc, bump_alloc_ptr_global);
        f.instruction(&Instruction::Return);
    };

    // Step 1 — connect(host, port). Err short-circuits the whole
    // pipeline with a re-wrapped Result<String, String>::Err.
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(helpers.tcp_connect_fn));
    f.instruction(&Instruction::LocalSet(l_connect_result));

    f.instruction(&Instruction::LocalGet(l_connect_result));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.result_tcp_conn_string_type_idx,
        field_index: 0, // tag
    });
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_connect_result));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.result_tcp_conn_string_type_idx,
        field_index: 2, // err message
    });
    f.instruction(&Instruction::Call(helpers.result_string_string_err_fn));
    restore_bump(&mut f, l_saved_alloc, helpers.bump_alloc_ptr_global);
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // Step 2 — extract conn from Ok, look up the slot's streams.
    // `Tcp.connect` just wrote slot[slot_idx], so null / generation
    // / in_use guards aren't strictly needed here, but the
    // bump-allocator advance below will trip on a missing slot
    // anyway — keep this branch tight.
    f.instruction(&Instruction::LocalGet(l_connect_result));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.result_tcp_conn_string_type_idx,
        field_index: 1, // ok value
    });
    f.instruction(&Instruction::LocalSet(l_conn));

    f.instruction(&Instruction::LocalGet(l_conn));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_connection_type_idx,
        field_index: 0, // id
    });
    f.instruction(&Instruction::Call(helpers.parse_id_fn));
    f.instruction(&Instruction::I32Const(255));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(l_slot_idx));

    f.instruction(&Instruction::GlobalGet(helpers.tcp_pool_global));
    f.instruction(&Instruction::LocalGet(l_slot_idx));
    f.instruction(&Instruction::ArrayGet(indices.tcp_pool_type_idx));
    f.instruction(&Instruction::LocalSet(l_slot));

    f.instruction(&Instruction::LocalGet(l_slot));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_slot_type_idx,
        field_index: 1, // in_stream
    });
    f.instruction(&Instruction::LocalSet(l_in_handle));
    f.instruction(&Instruction::LocalGet(l_slot));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_slot_type_idx,
        field_index: 2, // out_stream
    });
    f.instruction(&Instruction::LocalSet(l_out_handle));

    // Step 3 — marshal `data` into LM[0..data_len]. Same bump
    // advance as `Tcp.writeLine` (Phase 4.7+ fix #6) so the
    // following cabi_realloc calls don't overwrite payload bytes
    // past 64 KiB.
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::Call(helpers.str_to_lm_fn));
    f.instruction(&Instruction::LocalSet(l_data_len));

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

    // Step 4 — 12-byte retptr (shared by every blocking_write +
    // blocking_read + shutdown call below).
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(12));
    f.instruction(&Instruction::Call(helpers.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr));

    // Step 5 — chunked blocking-write LM[0..data_len] on out_stream.
    // No '\r\n' suffix — the raw bytes go on the wire exactly as
    // the user wrote them, matching `aver-rt::tcp::send`.
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
        Some(&|f| {
            // Write error → close the connection best-effort, then
            // surface Err("tcp: write failed"). We call
            // `tcp_close_fn(conn)` rather than open-coding the
            // shutdown + drops since close already handles the
            // bookkeeping.
            f.instruction(&Instruction::LocalGet(l_conn));
            f.instruction(&Instruction::Call(helpers.tcp_close_fn));
            f.instruction(&Instruction::Drop);
            emit_write_err(f);
        }),
    );

    // Step 6 — shutdown(socket, send=1, retptr). Signals end-of-
    // request to the peer so it can finish processing and start
    // streaming its response. We ignore the retptr tag — even on
    // shutdown failure we still try to read whatever the peer
    // has already pushed onto the wire.
    f.instruction(&Instruction::LocalGet(l_slot));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_slot_type_idx,
        field_index: 0, // socket
    });
    f.instruction(&Instruction::I32Const(1)); // shutdown-type.send
    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::Call(helpers.shutdown_fn));

    // Step 7 — allocate the response buffer (starts at 4 KiB,
    // doubles on demand). Phase 4.2.2f bump-rewind on exit
    // reclaims this scratch.
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

    // Step 8 — read loop. blocking_read(in_stream, 4096, retptr).
    //   retptr tag at +0:
    //     1 (Err)            → EOF / stream-error; exit loop.
    //     0 (Ok) + len == 0  → EOF; exit loop.
    //     0 (Ok) + len > 0   → append LM[ptr..ptr+len] to buffer,
    //                          continue.
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
    f.instruction(&Instruction::Br(2));
    f.instruction(&Instruction::End);

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

    // Grow buf if needed (double until len + read_len fits).
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

    // Memory.copy LM[read_ptr..+read_len] → LM[buf_ptr + buf_len].
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

    // Step 9 — best-effort close (drop streams + shutdown + drop
    // socket + slot.in_use = 0). Ignore the Result.
    f.instruction(&Instruction::LocalGet(l_conn));
    f.instruction(&Instruction::Call(helpers.tcp_close_fn));
    f.instruction(&Instruction::Drop);

    // Step 10 — materialise response: copy LM[buf_ptr..+buf_len]
    // into a fresh `(array i8)` and wrap as Result.Ok.
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
    f.instruction(&Instruction::End); // copy loop
    f.instruction(&Instruction::End); // copy block

    // Build `Result<String, String>` { tag=1 (Ok), ok=arr, err=null }
    // inline — no factory call because the response string is a
    // freshly-built array, not a pre-existing value.
    f.instruction(&Instruction::I32Const(1)); // tag = 1 (Ok)
    f.instruction(&Instruction::LocalGet(l_arr));
    f.instruction(&Instruction::RefNull(HeapType::Concrete(
        indices.string_type_idx,
    )));
    f.instruction(&Instruction::StructNew(
        indices.result_string_string_type_idx,
    ));
    restore_bump(&mut f, l_saved_alloc, helpers.bump_alloc_ptr_global);
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
    // baked into the id string at connect time. The pool slot index
    // is the low 8 bits; the upper bits are the freshness tag we
    // cross-check against `slot.id_value` below.
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

    // Null-pool guard (Phase 4.7+ fix #8). The `Tcp.Connection`
    // record is non-opaque, so a program can hand-craft one and
    // pass it to `Tcp.close` before any `Tcp.connect`. In that
    // case `tcp_pool` is still null (lazy-init in connect) and
    // `array.get` would trap. Surface the same Err message as the
    // other stale-conn paths.
    f.instruction(&Instruction::GlobalGet(helpers.tcp_pool_global));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_unknown_err(&mut f);
    f.instruction(&Instruction::End);

    // slot = tcp_pool[slot_idx]
    f.instruction(&Instruction::GlobalGet(helpers.tcp_pool_global));
    f.instruction(&Instruction::LocalGet(l_slot_idx));
    f.instruction(&Instruction::ArrayGet(indices.tcp_pool_type_idx));
    f.instruction(&Instruction::LocalSet(l_slot));

    // Stale-conn guards, each returning Err to match aver-rt:
    //   1. slot is null (slot index never claimed)
    //   2. slot.id_value != parsed_id (pool wrapped; stale ref)
    //   3. slot.in_use == 0 (already closed)
    f.instruction(&Instruction::LocalGet(l_slot));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_unknown_err(&mut f);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(l_slot));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_slot_type_idx,
        field_index: 4, // id_value
    });
    f.instruction(&Instruction::LocalGet(l_parsed_id));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_unknown_err(&mut f);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(l_slot));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_slot_type_idx,
        field_index: 3, // in_use
    });
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_unknown_err(&mut f);
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

//! `__rt_tcp_write_line` and `__rt_tcp_read_line` — the per-connection
//! I/O helpers. Both look up the pool slot via the shared
//! `__rt_tcp_parse_id` helper and reach the underlying wasi:io stream
//! via `slot.{in_stream, out_stream}`.

use wasm_encoder::{Function, Instruction, ValType};

use super::restore_bump;

/// Phase 4.4a — `__rt_tcp_write_line(conn, line) -> ref Result<Unit, String>`
/// slot bundle. Reuses the close-side `parse_id` plus the
/// connection-pool globals/types; adds the output-stream side
/// helpers + a per-chunk write error message.
pub(in crate::codegen::wasm_gc) struct TcpWriteLineIndices {
    pub fn_type: u32,
    pub fn_idx: u32,
    pub string_type_idx: u32,
    pub tcp_connection_type_idx: u32,
    pub tcp_slot_type_idx: u32,
    pub tcp_pool_type_idx: u32,
    /// `"tcp: write failed"` data segment + length. Materialised
    /// when any blocking-write-and-flush chunk returns a non-zero
    /// retptr tag.
    pub write_err_segment_idx: u32,
    pub write_err_len: u32,
}

pub(in crate::codegen::wasm_gc) struct TcpWriteLineHelperFns {
    /// `__rt_tcp_parse_id` — shared with Tcp.close.
    pub parse_id_fn: u32,
    /// `__rt_string_to_lm(s: ref string) -> i32` — copies the
    /// Aver String's bytes into LM[0..len] and returns len.
    pub str_to_lm_fn: u32,
    /// `cabi_realloc(0, 0, 4, 12) -> ptr` — 12-byte retptr block
    /// for `blocking-write-and-flush` (host writes `result<_,
    /// stream-error>` with tag@0 + 12-byte stream-error payload).
    pub cabi_realloc_fn: u32,
    /// `wasi:io/streams.[method]output-stream.blocking-write-and-flush`.
    /// Per-chunk error sets the Err return; the helper closes the
    /// loop early via `Return` from inside the shared chunked
    /// emitter.
    pub blocking_write_fn: u32,
    /// `__rt_result_unit_string_ok()` / `_err(s)` factories — the
    /// happy-path / write-failed-path completions.
    pub result_ok_fn: u32,
    pub result_err_fn: u32,
    /// `tcp_pool: ref null $tcp_pool` global.
    pub tcp_pool_global: u32,
    /// Phase 4.2.2f — see `TcpConnectHelperFns::bump_alloc_ptr_global`.
    pub bump_alloc_ptr_global: u32,
}

/// Phase 4.4a emit — `__rt_tcp_write_line` body. Trust contract
/// matches `Tcp.close`: `conn` came out of `Tcp.connect`, so the
/// pool slot is live. v1 ignores the `in_use == 0` case and
/// writes regardless; `Tcp.writeLine` after `Tcp.close` is a
/// program bug that surfaces as a wasi `last-operation-failed`
/// stream error and becomes `Result.Err("tcp: write failed")`.
pub(in crate::codegen::wasm_gc) fn emit_tcp_write_line(
    indices: &TcpWriteLineIndices,
    helpers: &TcpWriteLineHelperFns,
) -> Function {
    use wasm_encoder::{BlockType, MemArg};
    // Locals beyond params (0=conn, 1=line):
    //   2 = parsed_id (i32)
    //   3 = slot_idx  (i32) — parsed_id & 255
    //   4 = slot      (ref $tcp_slot)
    //   5 = len       (i32) — bytes the bridge wrote to LM
    //   6 = off       (i32) — chunked write cursor for the line bytes
    //   7 = retptr    (i32) — 12-byte blocking-write retptr
    //   8 = nl_ptr    (i32) — 1-byte buffer holding '\n' (side write)
    let slot_ref = ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(indices.tcp_slot_type_idx),
    });
    // local 9 = saved_alloc (Phase 4.2.2f bump-heap rewind).
    let mut f = Function::new(vec![
        (2u32, ValType::I32),
        (1u32, slot_ref),
        (5u32, ValType::I32),
    ]);
    let l_parsed_id: u32 = 2;
    let l_slot_idx: u32 = 3;
    let l_slot: u32 = 4;
    let l_len: u32 = 5;
    let l_off: u32 = 6;
    let l_retptr: u32 = 7;
    let l_nl_ptr: u32 = 8;
    let l_saved_alloc: u32 = 9;

    let mem1_zero = MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    };

    // Materialise + return a `Result.Err("tcp: write failed")` —
    // used by every stale-slot / null-slot guard below. Avoids
    // duplicating the segment-load + factory-call + bump-rewind
    // dance at each early exit.
    let emit_stale_err = |f: &mut Function| {
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(indices.write_err_len as i32));
        f.instruction(&Instruction::ArrayNewData {
            array_type_index: indices.string_type_idx,
            array_data_index: indices.write_err_segment_idx,
        });
        f.instruction(&Instruction::Call(helpers.result_err_fn));
        restore_bump(f, l_saved_alloc, helpers.bump_alloc_ptr_global);
        f.instruction(&Instruction::Return);
    };

    // Save bump_alloc_ptr — restored on every exit.
    f.instruction(&Instruction::GlobalGet(helpers.bump_alloc_ptr_global));
    f.instruction(&Instruction::LocalSet(l_saved_alloc));

    // parsed_id = parse_id(conn.id); slot_idx = parsed_id & 255.
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_connection_type_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::Call(helpers.parse_id_fn));
    f.instruction(&Instruction::LocalSet(l_parsed_id));
    f.instruction(&Instruction::LocalGet(l_parsed_id));
    f.instruction(&Instruction::I32Const(255));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(l_slot_idx));

    // tcp_pool / slot / generation / in_use guards — each surfaces
    // as `Result.Err("tcp: write failed")` because there is no real
    // out-stream to write to. The null-pool branch matches Phase
    // 4.7+ fix #8: a hand-crafted `Tcp.Connection` reaches
    // `Tcp.writeLine` before any `Tcp.connect` runs.
    f.instruction(&Instruction::GlobalGet(helpers.tcp_pool_global));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_stale_err(&mut f);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::GlobalGet(helpers.tcp_pool_global));
    f.instruction(&Instruction::LocalGet(l_slot_idx));
    f.instruction(&Instruction::ArrayGet(indices.tcp_pool_type_idx));
    f.instruction(&Instruction::LocalSet(l_slot));

    f.instruction(&Instruction::LocalGet(l_slot));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_stale_err(&mut f);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(l_slot));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_slot_type_idx,
        field_index: 4, // id_value
    });
    f.instruction(&Instruction::LocalGet(l_parsed_id));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_stale_err(&mut f);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(l_slot));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_slot_type_idx,
        field_index: 3, // in_use
    });
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_stale_err(&mut f);
    f.instruction(&Instruction::End);

    // Marshal line → LM[0..len] via the shared bridge helper.
    // `__rt_string_to_lm` writes the payload at LM[0..len] and grows
    // memory by `ceil(len/65536)` pages — exactly enough for the
    // line bytes themselves.
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(helpers.str_to_lm_fn));
    f.instruction(&Instruction::LocalSet(l_len));

    // Phase 4.7+ fix #6 — protect the payload from the bump
    // allocator. `bump_alloc_ptr` starts at 65536 (page 2); any
    // payload longer than 64KiB spills into page 2+, which is
    // exactly the region cabi_realloc hands out. Without this
    // adjustment the trailing newline buffer + retptr below would
    // overwrite payload bytes past offset 65536, silently corrupting
    // anything we just wrote.
    //
    // Strategy: advance the cursor to `max(current, align_up(len,
    // 16))` so subsequent cabi_realloc calls land strictly past the
    // payload. The 16-byte alignment matches the rest of the
    // wasip2 allocator's contract.
    f.instruction(&Instruction::LocalGet(l_len));
    f.instruction(&Instruction::I32Const(15));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(-16));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::GlobalGet(helpers.bump_alloc_ptr_global));
    f.instruction(&Instruction::I32GtU);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_len));
    f.instruction(&Instruction::I32Const(15));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(-16));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::GlobalSet(helpers.bump_alloc_ptr_global));
    f.instruction(&Instruction::End);

    // nl_ptr = cabi_realloc(0, 0, 1, 1); LM[nl_ptr] = '\n'.
    // Now safely past `len` thanks to the bump advance above.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(helpers.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_nl_ptr));
    f.instruction(&Instruction::LocalGet(l_nl_ptr));
    f.instruction(&Instruction::I32Const(0x0a));
    f.instruction(&Instruction::I32Store8(mem1_zero));

    // Allocate a 12-byte retptr for the per-chunk write result —
    // reused both by the line-bytes chunk loop and the newline tail.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(12));
    f.instruction(&Instruction::Call(helpers.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr));

    // Chunked write loop. `on_chunk_err` builds the "tcp: write
    // failed" Err and returns immediately — leaves a Result ref
    // on the stack and bails out of the function.
    let string_type_idx = indices.string_type_idx;
    let write_err_segment_idx = indices.write_err_segment_idx;
    let write_err_len = indices.write_err_len;
    let result_err_fn = helpers.result_err_fn;
    let tcp_slot_type_idx = indices.tcp_slot_type_idx;
    let blocking_write_fn = helpers.blocking_write_fn;
    let bump_alloc_ptr_global = helpers.bump_alloc_ptr_global;
    super::super::wasip2_helpers::emit_chunked_blocking_write(
        &mut f,
        l_len,
        l_off,
        blocking_write_fn,
        &|f| {
            f.instruction(&Instruction::LocalGet(l_slot));
            f.instruction(&Instruction::StructGet {
                struct_type_index: tcp_slot_type_idx,
                field_index: 2, // out_stream
            });
        },
        &|f| {
            f.instruction(&Instruction::LocalGet(l_retptr));
        },
        Some(&|f| {
            f.instruction(&Instruction::I32Const(0));
            f.instruction(&Instruction::I32Const(write_err_len as i32));
            f.instruction(&Instruction::ArrayNewData {
                array_type_index: string_type_idx,
                array_data_index: write_err_segment_idx,
            });
            f.instruction(&Instruction::Call(result_err_fn));
            restore_bump(f, l_saved_alloc, bump_alloc_ptr_global);
            f.instruction(&Instruction::Return);
        }),
    );

    // Line bytes written. Now flush the trailing '\n' as a separate
    // 1-byte payload — same out-stream, same retptr, single call.
    // The chunk-write loop doesn't fit here because there's no
    // chunking budget worth iterating for one byte; we inline the
    // call + tag check directly.
    let mem1 = MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    };
    f.instruction(&Instruction::LocalGet(l_slot));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_slot_type_idx,
        field_index: 2, // out_stream
    });
    f.instruction(&Instruction::LocalGet(l_nl_ptr));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::Call(helpers.blocking_write_fn));

    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(indices.write_err_len as i32));
    f.instruction(&Instruction::ArrayNewData {
        array_type_index: indices.string_type_idx,
        array_data_index: indices.write_err_segment_idx,
    });
    f.instruction(&Instruction::Call(helpers.result_err_fn));
    restore_bump(&mut f, l_saved_alloc, helpers.bump_alloc_ptr_global);
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // Loop + newline write finished without errors → Ok(()).
    f.instruction(&Instruction::Call(helpers.result_ok_fn));
    restore_bump(&mut f, l_saved_alloc, helpers.bump_alloc_ptr_global);
    f.instruction(&Instruction::End);
    f
}

/// Phase 4.4b — `__rt_tcp_read_line(conn) -> ref Result<String, String>`
/// slot bundle. Body loops 1-byte `blocking-read` on the slot's
/// input-stream, accumulating bytes into a growable cabi_realloc'd
/// buffer until `\n`, host EOF, or stream-error. Mirrors
/// `__rt_console_read_line` (Phase 1.3.4) — the only structural
/// difference is the source handle: stdin global there, slot's
/// `in_stream` field here.
pub(in crate::codegen::wasm_gc) struct TcpReadLineIndices {
    pub fn_type: u32,
    pub fn_idx: u32,
    pub string_type_idx: u32,
    pub result_type_idx: u32,
    pub tcp_connection_type_idx: u32,
    pub tcp_slot_type_idx: u32,
    pub tcp_pool_type_idx: u32,
    /// `"tcp: eof"` data segment + length. Materialised when the
    /// host returns Err or empty Ok and no bytes were collected.
    pub eof_segment_idx: u32,
    pub eof_len: u32,
}

pub(in crate::codegen::wasm_gc) struct TcpReadLineHelperFns {
    pub parse_id_fn: u32,
    pub cabi_realloc_fn: u32,
    /// `wasi:io/streams.[method]input-stream.blocking-read`.
    pub blocking_read_fn: u32,
    pub tcp_pool_global: u32,
    /// Phase 4.2.2f — see `TcpConnectHelperFns::bump_alloc_ptr_global`.
    pub bump_alloc_ptr_global: u32,
}

pub(in crate::codegen::wasm_gc) fn emit_tcp_read_line(
    indices: &TcpReadLineIndices,
    helpers: &TcpReadLineHelperFns,
) -> Function {
    use wasm_encoder::{BlockType, HeapType, MemArg, RefType};

    let s_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.string_type_idx),
    });
    let slot_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.tcp_slot_type_idx),
    });

    // Locals beyond param 0 = conn (ref Tcp.Connection):
    //   1  = parsed_id    (i32)
    //   2  = slot_idx     (i32) — parsed_id & 255
    //   3  = slot         (ref $tcp_slot)
    //   4  = in_handle    (i32) — cached struct.get of slot.in_stream
    //   5  = buf_ptr      (i32)
    //   6  = buf_cap      (i32)
    //   7  = buf_len      (i32)
    //   8  = retptr       (i32)
    //   9  = byte         (i32)
    //   10 = j            (i32)
    //   11 = data_ptr     (i32)
    //   12 = data_len     (i32)
    //   13 = should_err   (i32)
    //   14 = new_cap      (i32)
    //   15 = saved_alloc  (i32) — Phase 4.2.2f bump-heap rewind
    //   16 = arr          (ref string)
    let mut f = Function::new(vec![
        (2u32, ValType::I32),
        (1u32, slot_ref),
        (12u32, ValType::I32),
        (1u32, s_ref),
    ]);
    let l_parsed_id: u32 = 1;
    let l_slot_idx: u32 = 2;
    let l_slot: u32 = 3;
    let l_in_handle: u32 = 4;
    let l_buf_ptr: u32 = 5;
    let l_buf_cap: u32 = 6;
    let l_buf_len: u32 = 7;
    let l_retptr: u32 = 8;
    let l_byte: u32 = 9;
    let l_j: u32 = 10;
    let l_data_ptr: u32 = 11;
    let l_data_len: u32 = 12;
    let l_should_err: u32 = 13;
    let l_new_cap: u32 = 14;
    let l_saved_alloc: u32 = 15;
    let l_arr: u32 = 16;

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
    let mem1 = MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    };

    // Save bump_alloc_ptr — restored on every exit (EOF Err Return +
    // final Ok End).
    f.instruction(&Instruction::GlobalGet(helpers.bump_alloc_ptr_global));
    f.instruction(&Instruction::LocalSet(l_saved_alloc));

    // parsed_id = parse_id(conn.id); slot_idx = parsed_id & 255;
    // slot = tcp_pool[slot_idx]; then null / id_value / in_use
    // guards.  Stale references surface as `Result.Err("tcp: eof")`
    // because the underlying input-stream isn't ours to read from.
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_connection_type_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::Call(helpers.parse_id_fn));
    f.instruction(&Instruction::LocalSet(l_parsed_id));
    f.instruction(&Instruction::LocalGet(l_parsed_id));
    f.instruction(&Instruction::I32Const(255));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(l_slot_idx));

    let result_type_idx = indices.result_type_idx;
    let string_type_idx = indices.string_type_idx;
    let eof_segment_idx = indices.eof_segment_idx;
    let eof_len = indices.eof_len;
    let bump_alloc_ptr_global = helpers.bump_alloc_ptr_global;
    let emit_stale_eof = |f: &mut Function| {
        f.instruction(&Instruction::I32Const(0)); // tag = 0 (Err)
        f.instruction(&Instruction::RefNull(HeapType::Concrete(string_type_idx)));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(eof_len as i32));
        f.instruction(&Instruction::ArrayNewData {
            array_type_index: string_type_idx,
            array_data_index: eof_segment_idx,
        });
        f.instruction(&Instruction::StructNew(result_type_idx));
        restore_bump(f, l_saved_alloc, bump_alloc_ptr_global);
        f.instruction(&Instruction::Return);
    };

    // Null-pool guard (Phase 4.7+ fix #8) — a hand-crafted
    // `Tcp.Connection` can reach `Tcp.readLine` before any
    // `Tcp.connect` has lazy-initialised the pool global. Surface
    // the same `Err("tcp: eof")` as every other stale-conn path.
    f.instruction(&Instruction::GlobalGet(helpers.tcp_pool_global));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_stale_eof(&mut f);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::GlobalGet(helpers.tcp_pool_global));
    f.instruction(&Instruction::LocalGet(l_slot_idx));
    f.instruction(&Instruction::ArrayGet(indices.tcp_pool_type_idx));
    f.instruction(&Instruction::LocalSet(l_slot));

    f.instruction(&Instruction::LocalGet(l_slot));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_stale_eof(&mut f);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(l_slot));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_slot_type_idx,
        field_index: 4, // id_value
    });
    f.instruction(&Instruction::LocalGet(l_parsed_id));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_stale_eof(&mut f);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(l_slot));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_slot_type_idx,
        field_index: 3, // in_use
    });
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_stale_eof(&mut f);
    f.instruction(&Instruction::End);

    // in_handle = slot.in_stream — cached so the inner loop just
    // does a local.get instead of struct.get every iteration.
    f.instruction(&Instruction::LocalGet(l_slot));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_slot_type_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalSet(l_in_handle));

    // Alloc initial 256-byte buffer.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Const(256));
    f.instruction(&Instruction::Call(helpers.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_buf_ptr));
    f.instruction(&Instruction::I32Const(256));
    f.instruction(&Instruction::LocalSet(l_buf_cap));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(l_buf_len));

    // Alloc 12-byte retptr.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(12));
    f.instruction(&Instruction::Call(helpers.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr));

    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(l_should_err));

    // Outer block + inner loop.
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));

    // blocking-read(in_handle, 1, retptr).
    f.instruction(&Instruction::LocalGet(l_in_handle));
    f.instruction(&Instruction::I64Const(1));
    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::Call(helpers.blocking_read_fn));

    // Result tag at retptr+0.
    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_buf_len));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalSet(l_should_err));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::Br(2));
    f.instruction(&Instruction::End);

    // Ok branch — (data_ptr, data_len) at +4 / +8.
    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::I32Load(mem4_o4));
    f.instruction(&Instruction::LocalSet(l_data_ptr));
    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::I32Load(mem4_o8));
    f.instruction(&Instruction::LocalSet(l_data_len));

    // Empty Ok = EOF.
    f.instruction(&Instruction::LocalGet(l_data_len));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_buf_len));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalSet(l_should_err));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::Br(2));
    f.instruction(&Instruction::End);

    // byte = LM[data_ptr].
    f.instruction(&Instruction::LocalGet(l_data_ptr));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::LocalSet(l_byte));

    // '\n' ends the line.
    f.instruction(&Instruction::LocalGet(l_byte));
    f.instruction(&Instruction::I32Const(10));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::Br(2));
    f.instruction(&Instruction::End);

    // '\r' silently skipped (Windows-style newline tolerance).
    f.instruction(&Instruction::LocalGet(l_byte));
    f.instruction(&Instruction::I32Const(13));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::Br(1));
    f.instruction(&Instruction::End);

    // Grow buffer if full.
    f.instruction(&Instruction::LocalGet(l_buf_len));
    f.instruction(&Instruction::LocalGet(l_buf_cap));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::If(BlockType::Empty));
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
    f.instruction(&Instruction::End);

    // LM[buf_ptr + buf_len] = byte; buf_len += 1.
    f.instruction(&Instruction::LocalGet(l_buf_ptr));
    f.instruction(&Instruction::LocalGet(l_buf_len));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(l_byte));
    f.instruction(&Instruction::I32Store8(mem1));
    f.instruction(&Instruction::LocalGet(l_buf_len));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l_buf_len));

    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // outer block

    // Result building. Err path materialises "tcp: eof" from the
    // pre-registered data segment, Ok copies LM[buf_ptr..buf_ptr+buf_len]
    // into a fresh `(array i8)`.
    f.instruction(&Instruction::LocalGet(l_should_err));
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::I32Const(0)); // result tag = 0 (Err)
    f.instruction(&Instruction::RefNull(HeapType::Concrete(
        indices.string_type_idx,
    )));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(indices.eof_len as i32));
    f.instruction(&Instruction::ArrayNewData {
        array_type_index: indices.string_type_idx,
        array_data_index: indices.eof_segment_idx,
    });
    f.instruction(&Instruction::StructNew(indices.result_type_idx));
    restore_bump(&mut f, l_saved_alloc, helpers.bump_alloc_ptr_global);
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // Ok arm — copy LM[buf_ptr..buf_ptr+buf_len] into a fresh GC array.
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

    // Stack: tag=1, ok=arr, err=null
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(l_arr));
    f.instruction(&Instruction::RefNull(HeapType::Concrete(
        indices.string_type_idx,
    )));
    f.instruction(&Instruction::StructNew(indices.result_type_idx));
    restore_bump(&mut f, l_saved_alloc, helpers.bump_alloc_ptr_global);
    f.instruction(&Instruction::End);
    f
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_line_emit_compiles() {
        let indices = TcpReadLineIndices {
            fn_type: 0,
            fn_idx: 0,
            string_type_idx: 1,
            result_type_idx: 2,
            tcp_connection_type_idx: 3,
            tcp_slot_type_idx: 4,
            tcp_pool_type_idx: 5,
            eof_segment_idx: 6,
            eof_len: b"tcp: eof".len() as u32,
        };
        let helpers = TcpReadLineHelperFns {
            parse_id_fn: 7,
            cabi_realloc_fn: 8,
            blocking_read_fn: 9,
            tcp_pool_global: 0,
            bump_alloc_ptr_global: 1,
        };
        let _f = emit_tcp_read_line(&indices, &helpers);
    }

    #[test]
    fn write_line_emit_compiles() {
        let indices = TcpWriteLineIndices {
            fn_type: 0,
            fn_idx: 0,
            string_type_idx: 1,
            tcp_connection_type_idx: 2,
            tcp_slot_type_idx: 3,
            tcp_pool_type_idx: 4,
            write_err_segment_idx: 5,
            write_err_len: b"tcp: write failed".len() as u32,
        };
        let helpers = TcpWriteLineHelperFns {
            parse_id_fn: 6,
            str_to_lm_fn: 7,
            cabi_realloc_fn: 8,
            blocking_write_fn: 9,
            result_ok_fn: 10,
            result_err_fn: 11,
            tcp_pool_global: 0,
            bump_alloc_ptr_global: 1,
        };
        let _f = emit_tcp_write_line(&indices, &helpers);
    }
}

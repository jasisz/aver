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
    use wasm_encoder::MemArg;
    // Locals beyond params (0=conn, 1=line):
    //   2 = slot_idx (i32)
    //   3 = slot     (ref $tcp_slot)
    //   4 = len      (i32) — bytes written to LM (line + '\n')
    //   5 = off      (i32) — chunked write cursor
    //   6 = retptr   (i32) — 12-byte blocking-write retptr
    let slot_ref = ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(indices.tcp_slot_type_idx),
    });
    // local 7 = saved_alloc (Phase 4.2.2f bump-heap rewind).
    let mut f = Function::new(vec![
        (1u32, ValType::I32),
        (1u32, slot_ref),
        (4u32, ValType::I32),
    ]);
    let l_slot_idx: u32 = 2;
    let l_slot: u32 = 3;
    let l_len: u32 = 4;
    let l_off: u32 = 5;
    let l_retptr: u32 = 6;
    let l_saved_alloc: u32 = 7;

    let mem1_zero = MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    };

    // Save bump_alloc_ptr — restored on every exit.
    f.instruction(&Instruction::GlobalGet(helpers.bump_alloc_ptr_global));
    f.instruction(&Instruction::LocalSet(l_saved_alloc));

    // slot_idx = parse_id(conn.id)
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_connection_type_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::Call(helpers.parse_id_fn));
    f.instruction(&Instruction::LocalSet(l_slot_idx));

    // slot = tcp_pool[slot_idx]
    f.instruction(&Instruction::GlobalGet(helpers.tcp_pool_global));
    f.instruction(&Instruction::LocalGet(l_slot_idx));
    f.instruction(&Instruction::ArrayGet(indices.tcp_pool_type_idx));
    f.instruction(&Instruction::LocalSet(l_slot));

    // Marshal line → LM[0..len] via the shared bridge helper.
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(helpers.str_to_lm_fn));
    f.instruction(&Instruction::LocalSet(l_len));

    // Append '\n' at LM[len]; bump len.
    f.instruction(&Instruction::LocalGet(l_len));
    f.instruction(&Instruction::I32Const(0x0a));
    f.instruction(&Instruction::I32Store8(mem1_zero));
    f.instruction(&Instruction::LocalGet(l_len));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l_len));

    // Allocate a 12-byte retptr for the per-chunk write result.
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

    // Loop finished without errors → Ok(()).
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
    //   1  = slot_idx     (i32)
    //   2  = slot         (ref $tcp_slot)
    //   3  = in_handle    (i32) — cached struct.get of slot.in_stream
    //   4  = buf_ptr      (i32)
    //   5  = buf_cap      (i32)
    //   6  = buf_len      (i32)
    //   7  = retptr       (i32)
    //   8  = byte         (i32)
    //   9  = j            (i32)
    //   10 = data_ptr     (i32)
    //   11 = data_len     (i32)
    //   12 = should_err   (i32)
    //   13 = new_cap      (i32)
    //   14 = saved_alloc  (i32) — Phase 4.2.2f bump-heap rewind
    //   15 = arr          (ref string)
    let mut f = Function::new(vec![
        (1u32, ValType::I32),
        (1u32, slot_ref),
        (12u32, ValType::I32),
        (1u32, s_ref),
    ]);
    let l_slot_idx: u32 = 1;
    let l_slot: u32 = 2;
    let l_in_handle: u32 = 3;
    let l_buf_ptr: u32 = 4;
    let l_buf_cap: u32 = 5;
    let l_buf_len: u32 = 6;
    let l_retptr: u32 = 7;
    let l_byte: u32 = 8;
    let l_j: u32 = 9;
    let l_data_ptr: u32 = 10;
    let l_data_len: u32 = 11;
    let l_should_err: u32 = 12;
    let l_new_cap: u32 = 13;
    let l_saved_alloc: u32 = 14;
    let l_arr: u32 = 15;

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

    // slot_idx = parse_id(conn.id); slot = tcp_pool[slot_idx].
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_connection_type_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::Call(helpers.parse_id_fn));
    f.instruction(&Instruction::LocalSet(l_slot_idx));
    f.instruction(&Instruction::GlobalGet(helpers.tcp_pool_global));
    f.instruction(&Instruction::LocalGet(l_slot_idx));
    f.instruction(&Instruction::ArrayGet(indices.tcp_pool_type_idx));
    f.instruction(&Instruction::LocalSet(l_slot));
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

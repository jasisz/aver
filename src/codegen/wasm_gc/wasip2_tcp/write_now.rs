//! Native WASI 0.2 lowering for `Tcp.writeNow`.
//!
//! `__rt_tcp_write_now(conn, payload) -> ref Result<Int, String>`. The
//! nominal `Bytes` carrier is projected into linear memory exactly as
//! `Tcp.writeBytes` does, but instead of the chunked blocking write the helper
//! asks `output-stream.check-write` how many bytes the stream accepts right
//! now, writes at most that many with the never-blocking `output-stream.write`,
//! requests a `flush` without waiting for it, and returns the accepted count.
//! A zero permit is `Ok(0)` (the socket would block), never an error. Any
//! stream error poisons the pool slot, as the blocking write does.

use wasm_encoder::{Function, Instruction, ValType};

use super::io::emit_poison_slot;
use super::restore_bump;

pub(in crate::codegen::wasm_gc) struct TcpWriteNowIndices {
    pub fn_type: u32,
    pub fn_idx: u32,
    pub string_type_idx: u32,
    pub bytes_type_idx: u32,
    pub list_int_type_idx: u32,
    pub aint_struct_type_idx: u32,
    pub tcp_connection_type_idx: u32,
    pub tcp_slot_type_idx: u32,
    pub tcp_pool_type_idx: u32,
    pub malformed_segment_idx: u32,
    pub malformed_len: u32,
    pub write_err_segment_idx: u32,
    pub write_err_len: u32,
    pub unknown_segment_idx: u32,
    pub unknown_len: u32,
}

pub(in crate::codegen::wasm_gc) struct TcpWriteNowHelperFns {
    pub parse_id_fn: u32,
    pub cabi_realloc_fn: u32,
    /// `wasi:io/streams.[method]output-stream.check-write`.
    pub check_write_fn: u32,
    /// `wasi:io/streams.[method]output-stream.write`.
    pub write_fn: u32,
    /// `wasi:io/streams.[method]output-stream.flush`.
    pub flush_fn: u32,
    /// `__rt_result_int_string_ok(int)`.
    pub result_ok_fn: u32,
    /// `__rt_result_int_string_err(message)`.
    pub result_err_fn: u32,
    pub aint_from_i64_fn: u32,
    pub tcp_pool_global: u32,
    pub drop_input_stream_fn: u32,
    pub drop_output_stream_fn: u32,
    pub drop_tcp_socket_fn: u32,
    pub bump_alloc_ptr_global: u32,
    pub bytes_unpack_fn: Option<u32>,
}

pub(in crate::codegen::wasm_gc) fn emit_tcp_write_now(
    indices: &TcpWriteNowIndices,
    helpers: &TcpWriteNowHelperFns,
) -> Function {
    use wasm_encoder::{BlockType, HeapType, MemArg, RefType};

    let slot_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.tcp_slot_type_idx),
    });
    let list_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.list_int_type_idx),
    });
    // Params: 0=conn, 1=Bytes. Locals: 2=parsed_id, 3=slot_idx,
    // 4=slot, 5=len, 6=off, 7=retptr, 8=saved_alloc, 9=list, 10=byte,
    // 11=permit, 12=accepted.
    let mut f = Function::new(vec![
        (2u32, ValType::I32),
        (1u32, slot_ref),
        (4u32, ValType::I32),
        (1u32, list_ref),
        (1u32, ValType::I64),
        (1u32, ValType::I64),
        (1u32, ValType::I32),
    ]);
    let l_parsed_id = 2;
    let l_slot_idx = 3;
    let l_slot = 4;
    let l_len = 5;
    let l_off = 6;
    let l_retptr = 7;
    let l_saved_alloc = 8;
    let l_list = 9;
    let l_byte = 10;
    let l_permit = 11;
    let l_accepted = 12;
    let mem1 = MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    };
    let mem8_permit = MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    };

    let emit_err = |f: &mut Function, segment_idx: u32, len: u32| {
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(len as i32));
        f.instruction(&Instruction::ArrayNewData {
            array_type_index: indices.string_type_idx,
            array_data_index: segment_idx,
        });
        f.instruction(&Instruction::Call(helpers.result_err_fn));
        restore_bump(f, l_saved_alloc, helpers.bump_alloc_ptr_global);
        f.instruction(&Instruction::Return);
    };
    // A stream error after I/O started: poison the slot, then Err.
    let emit_poisoned_err = |f: &mut Function| {
        emit_poison_slot(
            f,
            l_slot,
            indices.tcp_slot_type_idx,
            helpers.drop_input_stream_fn,
            helpers.drop_output_stream_fn,
            helpers.drop_tcp_socket_fn,
        );
        emit_err(f, indices.write_err_segment_idx, indices.write_err_len);
    };
    let emit_ok_count = |f: &mut Function| {
        f.instruction(&Instruction::LocalGet(l_accepted));
        f.instruction(&Instruction::I64ExtendI32U);
        f.instruction(&Instruction::Call(helpers.aint_from_i64_fn));
        f.instruction(&Instruction::Call(helpers.result_ok_fn));
        restore_bump(f, l_saved_alloc, helpers.bump_alloc_ptr_global);
        f.instruction(&Instruction::Return);
    };

    f.instruction(&Instruction::GlobalGet(helpers.bump_alloc_ptr_global));
    f.instruction(&Instruction::LocalSet(l_saved_alloc));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_connection_type_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::Call(helpers.parse_id_fn));
    f.instruction(&Instruction::LocalSet(l_parsed_id));

    f.instruction(&Instruction::GlobalGet(helpers.tcp_pool_global));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_err(&mut f, indices.unknown_segment_idx, indices.unknown_len);
    f.instruction(&Instruction::End);

    // Find the live persistent slot by its full monotonic id.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(l_slot_idx));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_slot_idx));
    f.instruction(&Instruction::I32Const(256));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_err(&mut f, indices.unknown_segment_idx, indices.unknown_len);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::GlobalGet(helpers.tcp_pool_global));
    f.instruction(&Instruction::LocalGet(l_slot_idx));
    f.instruction(&Instruction::ArrayGet(indices.tcp_pool_type_idx));
    f.instruction(&Instruction::LocalSet(l_slot));
    f.instruction(&Instruction::LocalGet(l_slot));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_slot));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_slot_type_idx,
        field_index: 4,
    });
    f.instruction(&Instruction::LocalGet(l_parsed_id));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::LocalGet(l_slot));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_slot_type_idx,
        field_index: 3,
    });
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::BrIf(2));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(l_slot_idx));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l_slot_idx));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    // First pass validates the private carrier and computes its byte length.
    f.instruction(&Instruction::LocalGet(1));
    if let Some(unpack_fn) = helpers.bytes_unpack_fn {
        f.instruction(&Instruction::Call(unpack_fn));
    } else {
        f.instruction(&Instruction::StructGet {
            struct_type_index: indices.bytes_type_idx,
            field_index: 0,
        });
    }
    f.instruction(&Instruction::LocalSet(l_list));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(l_len));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_list));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(l_list));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.list_int_type_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.aint_struct_type_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::LocalGet(l_list));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.list_int_type_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.aint_struct_type_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::LocalTee(l_byte));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::I64LtS);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::LocalGet(l_byte));
    f.instruction(&Instruction::I64Const(255));
    f.instruction(&Instruction::I64GtS);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_err(&mut f, indices.malformed_segment_idx, indices.malformed_len);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(l_list));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.list_int_type_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalSet(l_list));
    f.instruction(&Instruction::LocalGet(l_len));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l_len));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    // An empty payload accepts zero bytes without touching the stream.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(l_accepted));
    f.instruction(&Instruction::LocalGet(l_len));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_ok_count(&mut f);
    f.instruction(&Instruction::End);

    // Reserve enough LM and materialise the bytes at offset zero.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(l_len));
    f.instruction(&Instruction::Call(helpers.cabi_realloc_fn));
    f.instruction(&Instruction::Drop);
    f.instruction(&Instruction::LocalGet(1));
    if let Some(unpack_fn) = helpers.bytes_unpack_fn {
        f.instruction(&Instruction::Call(unpack_fn));
    } else {
        f.instruction(&Instruction::StructGet {
            struct_type_index: indices.bytes_type_idx,
            field_index: 0,
        });
    }
    f.instruction(&Instruction::LocalSet(l_list));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(l_off));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_list));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(l_off));
    f.instruction(&Instruction::LocalGet(l_list));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.list_int_type_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.aint_struct_type_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Store8(mem1));
    f.instruction(&Instruction::LocalGet(l_list));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.list_int_type_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalSet(l_list));
    f.instruction(&Instruction::LocalGet(l_off));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l_off));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    // Keep allocator scratch past the payload, then allocate one 16-byte
    // retptr shared by check-write, write, and flush.
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
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Const(16));
    f.instruction(&Instruction::Call(helpers.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr));

    // check-write: how much the stream accepts right now.
    f.instruction(&Instruction::LocalGet(l_slot));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_slot_type_idx,
        field_index: 2,
    });
    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::Call(helpers.check_write_fn));
    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_poisoned_err(&mut f);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::I64Load(mem8_permit));
    f.instruction(&Instruction::LocalSet(l_permit));
    // accepted = min(permit, len); a zero permit is Ok(0).
    f.instruction(&Instruction::LocalGet(l_permit));
    f.instruction(&Instruction::LocalGet(l_len));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64LtU);
    f.instruction(&Instruction::If(BlockType::Result(ValType::I32)));
    f.instruction(&Instruction::LocalGet(l_permit));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(l_len));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalSet(l_accepted));
    f.instruction(&Instruction::LocalGet(l_accepted));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_ok_count(&mut f);
    f.instruction(&Instruction::End);

    // write(out_stream, LM[0..accepted]) never blocks within the permit.
    f.instruction(&Instruction::LocalGet(l_slot));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_slot_type_idx,
        field_index: 2,
    });
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(l_accepted));
    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::Call(helpers.write_fn));
    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_poisoned_err(&mut f);
    f.instruction(&Instruction::End);

    // flush: request delivery without waiting for it.
    f.instruction(&Instruction::LocalGet(l_slot));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_slot_type_idx,
        field_index: 2,
    });
    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::Call(helpers.flush_fn));
    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_poisoned_err(&mut f);
    f.instruction(&Instruction::End);

    emit_ok_count(&mut f);
    f.instruction(&Instruction::End);
    f
}

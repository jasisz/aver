//! Native WASI 0.2 lowering for `Tcp.readSome`.
//!
//! Unlike `Tcp.readBytes`, this helper performs exactly one
//! `input-stream.blocking-read(maxBytes)` call. A closed stream is a clean EOF
//! and becomes `Ok(Bytes.empty)`; `last-operation-failed` poisons the pool slot
//! because the stream position is no longer trustworthy.

use wasm_encoder::{Function, Instruction, ValType};

use super::io::emit_poison_slot;
use super::restore_bump;

pub(in crate::codegen::wasm_gc) struct TcpReadSomeIndices {
    pub fn_type: u32,
    pub fn_idx: u32,
    pub string_type_idx: u32,
    pub tcp_connection_type_idx: u32,
    pub tcp_slot_type_idx: u32,
    pub tcp_pool_type_idx: u32,
    pub aint_struct_type_idx: u32,
    pub list_int_type_idx: u32,
    pub positive_segment_idx: u32,
    pub positive_len: u32,
    pub limit_segment_idx: u32,
    pub limit_len: u32,
    pub read_limit_segment_idx: u32,
    pub read_limit_len: u32,
    pub read_error_segment_idx: u32,
    pub read_error_len: u32,
    pub unknown_segment_idx: u32,
    pub unknown_len: u32,
}

pub(in crate::codegen::wasm_gc) struct TcpReadSomeHelperFns {
    pub parse_id_fn: u32,
    pub cabi_realloc_fn: u32,
    pub blocking_read_fn: u32,
    pub result_ok_fn: u32,
    pub result_err_fn: u32,
    pub aint_from_i64_fn: u32,
    pub tcp_pool_global: u32,
    pub drop_input_stream_fn: u32,
    pub drop_output_stream_fn: u32,
    pub drop_tcp_socket_fn: u32,
    pub bump_alloc_ptr_global: u32,
}

pub(in crate::codegen::wasm_gc) fn emit_tcp_read_some(
    indices: &TcpReadSomeIndices,
    helpers: &TcpReadSomeHelperFns,
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
    // Params: 0=conn, 1=maxBytes. Locals: parsed id, slot idx, slot,
    // input handle, max i64, retptr, data ptr/len, reverse cursor,
    // saved bump pointer, result list.
    let mut function = Function::new(vec![
        (2, ValType::I32),
        (1, slot_ref),
        (1, ValType::I32),
        (1, ValType::I64),
        (5, ValType::I32),
        (1, list_ref),
    ]);
    let parsed_id = 2;
    let slot_idx = 3;
    let slot = 4;
    let input = 5;
    let max_bytes = 6;
    let retptr = 7;
    let data_ptr = 8;
    let data_len = 9;
    let cursor = 10;
    let saved_alloc = 11;
    let list = 12;
    let mem1 = MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    };
    let mem1_error_variant = MemArg {
        offset: 4,
        align: 0,
        memory_index: 0,
    };
    let mem4_data_ptr = MemArg {
        offset: 4,
        align: 2,
        memory_index: 0,
    };
    let mem4_data_len = MemArg {
        offset: 8,
        align: 2,
        memory_index: 0,
    };
    let emit_error = |function: &mut Function, segment: u32, len: u32| {
        function.instruction(&Instruction::I32Const(0));
        function.instruction(&Instruction::I32Const(len as i32));
        function.instruction(&Instruction::ArrayNewData {
            array_type_index: indices.string_type_idx,
            array_data_index: segment,
        });
        function.instruction(&Instruction::Call(helpers.result_err_fn));
        restore_bump(function, saved_alloc, helpers.bump_alloc_ptr_global);
        function.instruction(&Instruction::Return);
    };

    function.instruction(&Instruction::GlobalGet(helpers.bump_alloc_ptr_global));
    function.instruction(&Instruction::LocalSet(saved_alloc));

    // Preserve arbitrary-precision validation as a catchable Result.Err.
    function.instruction(&Instruction::LocalGet(1));
    function.instruction(&Instruction::StructGet {
        struct_type_index: indices.aint_struct_type_idx,
        field_index: 1,
    });
    function.instruction(&Instruction::RefIsNull);
    function.instruction(&Instruction::I32Eqz);
    function.instruction(&Instruction::If(BlockType::Empty));
    emit_error(
        &mut function,
        indices.read_limit_segment_idx,
        indices.read_limit_len,
    );
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::LocalGet(1));
    function.instruction(&Instruction::StructGet {
        struct_type_index: indices.aint_struct_type_idx,
        field_index: 0,
    });
    function.instruction(&Instruction::LocalSet(max_bytes));
    function.instruction(&Instruction::LocalGet(max_bytes));
    function.instruction(&Instruction::I64Const(0));
    function.instruction(&Instruction::I64LeS);
    function.instruction(&Instruction::If(BlockType::Empty));
    emit_error(
        &mut function,
        indices.positive_segment_idx,
        indices.positive_len,
    );
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::LocalGet(max_bytes));
    function.instruction(&Instruction::I64Const(10 * 1024 * 1024));
    function.instruction(&Instruction::I64GtS);
    function.instruction(&Instruction::If(BlockType::Empty));
    emit_error(&mut function, indices.limit_segment_idx, indices.limit_len);
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::LocalGet(0));
    function.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_connection_type_idx,
        field_index: 0,
    });
    function.instruction(&Instruction::Call(helpers.parse_id_fn));
    function.instruction(&Instruction::LocalSet(parsed_id));
    function.instruction(&Instruction::GlobalGet(helpers.tcp_pool_global));
    function.instruction(&Instruction::RefIsNull);
    function.instruction(&Instruction::If(BlockType::Empty));
    emit_error(
        &mut function,
        indices.unknown_segment_idx,
        indices.unknown_len,
    );
    function.instruction(&Instruction::End);

    // Find the live pool slot by full monotonic id.
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalSet(slot_idx));
    function.instruction(&Instruction::Block(BlockType::Empty));
    function.instruction(&Instruction::Loop(BlockType::Empty));
    function.instruction(&Instruction::LocalGet(slot_idx));
    function.instruction(&Instruction::I32Const(256));
    function.instruction(&Instruction::I32GeU);
    function.instruction(&Instruction::If(BlockType::Empty));
    emit_error(
        &mut function,
        indices.unknown_segment_idx,
        indices.unknown_len,
    );
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::GlobalGet(helpers.tcp_pool_global));
    function.instruction(&Instruction::LocalGet(slot_idx));
    function.instruction(&Instruction::ArrayGet(indices.tcp_pool_type_idx));
    function.instruction(&Instruction::LocalSet(slot));
    function.instruction(&Instruction::LocalGet(slot));
    function.instruction(&Instruction::RefIsNull);
    function.instruction(&Instruction::I32Eqz);
    function.instruction(&Instruction::If(BlockType::Empty));
    function.instruction(&Instruction::LocalGet(slot));
    function.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_slot_type_idx,
        field_index: 4,
    });
    function.instruction(&Instruction::LocalGet(parsed_id));
    function.instruction(&Instruction::I32Eq);
    function.instruction(&Instruction::LocalGet(slot));
    function.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_slot_type_idx,
        field_index: 3,
    });
    function.instruction(&Instruction::I32And);
    function.instruction(&Instruction::BrIf(2));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::LocalGet(slot_idx));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(slot_idx));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::LocalGet(slot));
    function.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_slot_type_idx,
        field_index: 1,
    });
    function.instruction(&Instruction::LocalSet(input));
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Const(12));
    function.instruction(&Instruction::Call(helpers.cabi_realloc_fn));
    function.instruction(&Instruction::LocalSet(retptr));

    function.instruction(&Instruction::LocalGet(input));
    function.instruction(&Instruction::LocalGet(max_bytes));
    function.instruction(&Instruction::LocalGet(retptr));
    function.instruction(&Instruction::Call(helpers.blocking_read_fn));
    function.instruction(&Instruction::LocalGet(retptr));
    function.instruction(&Instruction::I32Load8U(mem1));
    function.instruction(&Instruction::If(BlockType::Empty));
    // Err(closed) is clean EOF. Err(last-operation-failed) poisons.
    function.instruction(&Instruction::LocalGet(retptr));
    function.instruction(&Instruction::I32Load8U(mem1_error_variant));
    function.instruction(&Instruction::I32Eqz);
    function.instruction(&Instruction::If(BlockType::Empty));
    emit_poison_slot(
        &mut function,
        slot,
        indices.tcp_slot_type_idx,
        helpers.drop_input_stream_fn,
        helpers.drop_output_stream_fn,
        helpers.drop_tcp_socket_fn,
    );
    emit_error(
        &mut function,
        indices.read_error_segment_idx,
        indices.read_error_len,
    );
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalSet(data_ptr));
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalSet(data_len));
    function.instruction(&Instruction::Else);
    function.instruction(&Instruction::LocalGet(retptr));
    function.instruction(&Instruction::I32Load(mem4_data_ptr));
    function.instruction(&Instruction::LocalSet(data_ptr));
    function.instruction(&Instruction::LocalGet(retptr));
    function.instruction(&Instruction::I32Load(mem4_data_len));
    function.instruction(&Instruction::LocalSet(data_len));
    function.instruction(&Instruction::End);

    // Build the private Bytes List<Int> carrier in reverse wire order.
    function.instruction(&Instruction::RefNull(HeapType::Concrete(
        indices.list_int_type_idx,
    )));
    function.instruction(&Instruction::LocalSet(list));
    function.instruction(&Instruction::LocalGet(data_len));
    function.instruction(&Instruction::LocalSet(cursor));
    function.instruction(&Instruction::Block(BlockType::Empty));
    function.instruction(&Instruction::Loop(BlockType::Empty));
    function.instruction(&Instruction::LocalGet(cursor));
    function.instruction(&Instruction::I32Eqz);
    function.instruction(&Instruction::BrIf(1));
    function.instruction(&Instruction::LocalGet(cursor));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Sub);
    function.instruction(&Instruction::LocalSet(cursor));
    function.instruction(&Instruction::LocalGet(data_ptr));
    function.instruction(&Instruction::LocalGet(cursor));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::I32Load8U(mem1));
    function.instruction(&Instruction::I64ExtendI32U);
    function.instruction(&Instruction::Call(helpers.aint_from_i64_fn));
    function.instruction(&Instruction::LocalGet(list));
    function.instruction(&Instruction::StructNew(indices.list_int_type_idx));
    function.instruction(&Instruction::LocalSet(list));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::LocalGet(list));
    function.instruction(&Instruction::Call(helpers.result_ok_fn));
    restore_bump(&mut function, saved_alloc, helpers.bump_alloc_ptr_global);
    function.instruction(&Instruction::End);
    function
}

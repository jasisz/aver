//! Native WASI 0.2 lowering for `Tcp.poll`.
//!
//! Each live connection's input stream is subscribed to a `pollable`; a
//! duration pollable occupies the final slot and represents timeout. The
//! returned dense poll indices are mapped back to the caller's `Map<Int,
//! Tcp.Connection>` keys, sorted with Aver's arbitrary-precision comparator,
//! and materialised without narrowing the IDs.

use wasm_encoder::{Function, Instruction, ValType};

use super::restore_bump;

pub(in crate::codegen::wasm_gc) struct TcpPollIndices {
    pub fn_type: u32,
    pub fn_idx: u32,
    pub string_type_idx: u32,
    pub result_type_idx: u32,
    pub list_int_type_idx: u32,
    pub aint_struct_type_idx: u32,
    pub map_type_idx: u32,
    pub map_keys_array_type_idx: u32,
    pub map_values_array_type_idx: u32,
    pub int_key_box_type_idx: u32,
    pub tcp_connection_type_idx: u32,
    pub tcp_slot_type_idx: u32,
    pub tcp_pool_type_idx: u32,
    pub negative_segment_idx: u32,
    pub negative_len: u32,
    pub poll_limit_segment_idx: u32,
    pub poll_limit_len: u32,
    pub unknown_segment_idx: u32,
    pub unknown_len: u32,
}

pub(in crate::codegen::wasm_gc) struct TcpPollHelperFns {
    pub parse_id_fn: u32,
    pub cabi_realloc_fn: u32,
    pub input_subscribe_fn: u32,
    pub timeout_subscribe_fn: u32,
    pub poll_fn: u32,
    pub drop_pollable_fn: u32,
    pub aint_cmp_fn: u32,
    pub tcp_pool_global: u32,
    pub bump_alloc_ptr_global: u32,
}

fn emit_key_at(
    function: &mut Function,
    keys_local: u32,
    map_index_local: u32,
    keys_array_type_idx: u32,
    key_box_type_idx: u32,
) {
    function.instruction(&Instruction::LocalGet(keys_local));
    function.instruction(&Instruction::LocalGet(map_index_local));
    function.instruction(&Instruction::ArrayGet(keys_array_type_idx));
    function.instruction(&Instruction::StructGet {
        struct_type_index: key_box_type_idx,
        field_index: 0,
    });
}

fn emit_drop_pollables(
    function: &mut Function,
    input_ptr: u32,
    total: u32,
    cursor: u32,
    drop_pollable_fn: u32,
) {
    use wasm_encoder::{BlockType, MemArg};
    let mem4 = MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    };
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalSet(cursor));
    function.instruction(&Instruction::Block(BlockType::Empty));
    function.instruction(&Instruction::Loop(BlockType::Empty));
    function.instruction(&Instruction::LocalGet(cursor));
    function.instruction(&Instruction::LocalGet(total));
    function.instruction(&Instruction::I32GeU);
    function.instruction(&Instruction::BrIf(1));
    function.instruction(&Instruction::LocalGet(input_ptr));
    function.instruction(&Instruction::LocalGet(cursor));
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Mul);
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::I32Load(mem4));
    function.instruction(&Instruction::Call(drop_pollable_fn));
    function.instruction(&Instruction::LocalGet(cursor));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(cursor));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);
}

pub(in crate::codegen::wasm_gc) fn emit_tcp_poll(
    indices: &TcpPollIndices,
    helpers: &TcpPollHelperFns,
) -> Function {
    use wasm_encoder::{BlockType, HeapType, MemArg, RefType};

    let slot_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.tcp_slot_type_idx),
    });
    let keys_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.map_keys_array_type_idx),
    });
    let values_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.map_values_array_type_idx),
    });
    let connection_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.tcp_connection_type_idx),
    });
    let key_box_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.int_key_box_type_idx),
    });
    let list_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.list_int_type_idx),
    });
    // Params 0=Map, 1=timeout. Locals 2..23 are i32 scratch/cursors;
    // 24..29 are typed GC refs.
    let mut function = Function::new(vec![
        (22, ValType::I32),
        (1, slot_ref),
        (1, keys_ref),
        (1, values_ref),
        (1, connection_ref),
        (1, key_box_ref),
        (1, list_ref),
    ]);
    let saved_alloc = 2;
    let capacity = 3;
    let map_cursor = 4;
    let connection_count = 5;
    let total_pollables = 6;
    let input_ptr = 7;
    let mapping_ptr = 8;
    let retptr = 9;
    let parsed_id = 10;
    let pool_cursor = 11;
    let pollable = 12;
    let output_ptr = 13;
    let output_len = 14;
    let output_cursor = 15;
    let ready_len = 16;
    let ready_map_index = 17;
    let sort_cursor = 18;
    let sort_hole = 19;
    let sort_key_index = 20;
    let previous_index = 21;
    let cleanup_cursor = 22;
    let timer_index = 23;
    let slot = 24;
    let keys = 25;
    let values = 26;
    let connection = 27;
    let key_box = 28;
    let list = 29;
    let mem4 = MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    };
    let mem4_output_len = MemArg {
        offset: 4,
        align: 2,
        memory_index: 0,
    };
    let emit_error = |function: &mut Function, segment: u32, len: u32| {
        emit_drop_pollables(
            function,
            input_ptr,
            total_pollables,
            cleanup_cursor,
            helpers.drop_pollable_fn,
        );
        function.instruction(&Instruction::I32Const(0));
        function.instruction(&Instruction::RefNull(HeapType::Concrete(
            indices.list_int_type_idx,
        )));
        function.instruction(&Instruction::I32Const(0));
        function.instruction(&Instruction::I32Const(len as i32));
        function.instruction(&Instruction::ArrayNewData {
            array_type_index: indices.string_type_idx,
            array_data_index: segment,
        });
        function.instruction(&Instruction::StructNew(indices.result_type_idx));
        restore_bump(function, saved_alloc, helpers.bump_alloc_ptr_global);
        function.instruction(&Instruction::Return);
    };

    function.instruction(&Instruction::GlobalGet(helpers.bump_alloc_ptr_global));
    function.instruction(&Instruction::LocalSet(saved_alloc));
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalSet(total_pollables));

    // timeoutMs must fit i64 and be non-negative.
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
        indices.poll_limit_segment_idx,
        indices.poll_limit_len,
    );
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::LocalGet(1));
    function.instruction(&Instruction::StructGet {
        struct_type_index: indices.aint_struct_type_idx,
        field_index: 0,
    });
    function.instruction(&Instruction::I64Const(0));
    function.instruction(&Instruction::I64LtS);
    function.instruction(&Instruction::If(BlockType::Empty));
    emit_error(
        &mut function,
        indices.negative_segment_idx,
        indices.negative_len,
    );
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::LocalGet(0));
    function.instruction(&Instruction::StructGet {
        struct_type_index: indices.map_type_idx,
        field_index: 1,
    });
    function.instruction(&Instruction::LocalSet(capacity));

    // One dense pollable array and one dense-index → Map-slot array.
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::LocalGet(capacity));
    function.instruction(&Instruction::I32Const(2));
    function.instruction(&Instruction::I32Mul);
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Mul);
    function.instruction(&Instruction::Call(helpers.cabi_realloc_fn));
    function.instruction(&Instruction::LocalSet(input_ptr));
    function.instruction(&Instruction::LocalGet(input_ptr));
    function.instruction(&Instruction::LocalGet(capacity));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Mul);
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(mapping_ptr));
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Const(8));
    function.instruction(&Instruction::Call(helpers.cabi_realloc_fn));
    function.instruction(&Instruction::LocalSet(retptr));

    // Empty maps have null backing arrays; only project them when cap > 0.
    function.instruction(&Instruction::LocalGet(capacity));
    function.instruction(&Instruction::I32Eqz);
    function.instruction(&Instruction::I32Eqz);
    function.instruction(&Instruction::If(BlockType::Empty));
    function.instruction(&Instruction::LocalGet(0));
    function.instruction(&Instruction::StructGet {
        struct_type_index: indices.map_type_idx,
        field_index: 2,
    });
    function.instruction(&Instruction::LocalSet(keys));
    function.instruction(&Instruction::LocalGet(0));
    function.instruction(&Instruction::StructGet {
        struct_type_index: indices.map_type_idx,
        field_index: 3,
    });
    function.instruction(&Instruction::LocalSet(values));
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalSet(map_cursor));
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalSet(connection_count));
    function.instruction(&Instruction::Block(BlockType::Empty));
    function.instruction(&Instruction::Loop(BlockType::Empty));
    function.instruction(&Instruction::LocalGet(map_cursor));
    function.instruction(&Instruction::LocalGet(capacity));
    function.instruction(&Instruction::I32GeU);
    function.instruction(&Instruction::BrIf(1));
    function.instruction(&Instruction::LocalGet(keys));
    function.instruction(&Instruction::LocalGet(map_cursor));
    function.instruction(&Instruction::ArrayGet(indices.map_keys_array_type_idx));
    function.instruction(&Instruction::LocalTee(key_box));
    function.instruction(&Instruction::RefIsNull);
    function.instruction(&Instruction::I32Eqz);
    function.instruction(&Instruction::If(BlockType::Empty));
    function.instruction(&Instruction::LocalGet(values));
    function.instruction(&Instruction::LocalGet(map_cursor));
    function.instruction(&Instruction::ArrayGet(indices.map_values_array_type_idx));
    function.instruction(&Instruction::LocalSet(connection));
    function.instruction(&Instruction::LocalGet(connection));
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
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalSet(pool_cursor));
    function.instruction(&Instruction::Block(BlockType::Empty));
    function.instruction(&Instruction::Loop(BlockType::Empty));
    function.instruction(&Instruction::LocalGet(pool_cursor));
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
    function.instruction(&Instruction::LocalGet(pool_cursor));
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
    function.instruction(&Instruction::LocalGet(pool_cursor));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(pool_cursor));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::LocalGet(slot));
    function.instruction(&Instruction::StructGet {
        struct_type_index: indices.tcp_slot_type_idx,
        field_index: 1,
    });
    function.instruction(&Instruction::Call(helpers.input_subscribe_fn));
    function.instruction(&Instruction::LocalSet(pollable));
    function.instruction(&Instruction::LocalGet(input_ptr));
    function.instruction(&Instruction::LocalGet(connection_count));
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Mul);
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalGet(pollable));
    function.instruction(&Instruction::I32Store(mem4));
    function.instruction(&Instruction::LocalGet(mapping_ptr));
    function.instruction(&Instruction::LocalGet(connection_count));
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Mul);
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalGet(map_cursor));
    function.instruction(&Instruction::I32Store(mem4));
    function.instruction(&Instruction::LocalGet(connection_count));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(connection_count));
    function.instruction(&Instruction::LocalGet(connection_count));
    function.instruction(&Instruction::LocalSet(total_pollables));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::LocalGet(map_cursor));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(map_cursor));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);

    // Add the timeout pollable last. Saturate ns when ms*1_000_000 would
    // exceed u64; such a timeout is observationally "effectively forever".
    function.instruction(&Instruction::LocalGet(connection_count));
    function.instruction(&Instruction::LocalSet(timer_index));
    function.instruction(&Instruction::LocalGet(1));
    function.instruction(&Instruction::StructGet {
        struct_type_index: indices.aint_struct_type_idx,
        field_index: 0,
    });
    function.instruction(&Instruction::I64Const(18_446_744_073_709));
    function.instruction(&Instruction::I64GtU);
    function.instruction(&Instruction::If(BlockType::Result(ValType::I64)));
    function.instruction(&Instruction::I64Const(-1));
    function.instruction(&Instruction::Else);
    function.instruction(&Instruction::LocalGet(1));
    function.instruction(&Instruction::StructGet {
        struct_type_index: indices.aint_struct_type_idx,
        field_index: 0,
    });
    function.instruction(&Instruction::I64Const(1_000_000));
    function.instruction(&Instruction::I64Mul);
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::Call(helpers.timeout_subscribe_fn));
    function.instruction(&Instruction::LocalSet(pollable));
    function.instruction(&Instruction::LocalGet(input_ptr));
    function.instruction(&Instruction::LocalGet(timer_index));
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Mul);
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalGet(pollable));
    function.instruction(&Instruction::I32Store(mem4));
    function.instruction(&Instruction::LocalGet(connection_count));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(total_pollables));

    function.instruction(&Instruction::LocalGet(input_ptr));
    function.instruction(&Instruction::LocalGet(total_pollables));
    function.instruction(&Instruction::LocalGet(retptr));
    function.instruction(&Instruction::Call(helpers.poll_fn));
    function.instruction(&Instruction::LocalGet(retptr));
    function.instruction(&Instruction::I32Load(mem4));
    function.instruction(&Instruction::LocalSet(output_ptr));
    function.instruction(&Instruction::LocalGet(retptr));
    function.instruction(&Instruction::I32Load(mem4_output_len));
    function.instruction(&Instruction::LocalSet(output_len));
    emit_drop_pollables(
        &mut function,
        input_ptr,
        total_pollables,
        cleanup_cursor,
        helpers.drop_pollable_fn,
    );
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalSet(total_pollables));

    // Rewrite ready dense indices to Map slot indices, excluding the timer.
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalSet(output_cursor));
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalSet(ready_len));
    function.instruction(&Instruction::Block(BlockType::Empty));
    function.instruction(&Instruction::Loop(BlockType::Empty));
    function.instruction(&Instruction::LocalGet(output_cursor));
    function.instruction(&Instruction::LocalGet(output_len));
    function.instruction(&Instruction::I32GeU);
    function.instruction(&Instruction::BrIf(1));
    function.instruction(&Instruction::LocalGet(output_ptr));
    function.instruction(&Instruction::LocalGet(output_cursor));
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Mul);
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::I32Load(mem4));
    function.instruction(&Instruction::LocalTee(ready_map_index));
    function.instruction(&Instruction::LocalGet(timer_index));
    function.instruction(&Instruction::I32LtU);
    function.instruction(&Instruction::If(BlockType::Empty));
    function.instruction(&Instruction::LocalGet(mapping_ptr));
    function.instruction(&Instruction::LocalGet(ready_map_index));
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Mul);
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::I32Load(mem4));
    function.instruction(&Instruction::LocalSet(ready_map_index));
    function.instruction(&Instruction::LocalGet(output_ptr));
    function.instruction(&Instruction::LocalGet(ready_len));
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Mul);
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalGet(ready_map_index));
    function.instruction(&Instruction::I32Store(mem4));
    function.instruction(&Instruction::LocalGet(ready_len));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(ready_len));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::LocalGet(output_cursor));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(output_cursor));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);

    // Insertion-sort ready Map slots by their arbitrary-precision Int keys.
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::LocalSet(sort_cursor));
    function.instruction(&Instruction::Block(BlockType::Empty));
    function.instruction(&Instruction::Loop(BlockType::Empty));
    function.instruction(&Instruction::LocalGet(sort_cursor));
    function.instruction(&Instruction::LocalGet(ready_len));
    function.instruction(&Instruction::I32GeU);
    function.instruction(&Instruction::BrIf(1));
    function.instruction(&Instruction::LocalGet(output_ptr));
    function.instruction(&Instruction::LocalGet(sort_cursor));
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Mul);
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::I32Load(mem4));
    function.instruction(&Instruction::LocalSet(sort_key_index));
    function.instruction(&Instruction::LocalGet(sort_cursor));
    function.instruction(&Instruction::LocalSet(sort_hole));
    function.instruction(&Instruction::Block(BlockType::Empty));
    function.instruction(&Instruction::Loop(BlockType::Empty));
    function.instruction(&Instruction::LocalGet(sort_hole));
    function.instruction(&Instruction::I32Eqz);
    function.instruction(&Instruction::BrIf(1));
    function.instruction(&Instruction::LocalGet(output_ptr));
    function.instruction(&Instruction::LocalGet(sort_hole));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Sub);
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Mul);
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::I32Load(mem4));
    function.instruction(&Instruction::LocalSet(previous_index));
    emit_key_at(
        &mut function,
        keys,
        sort_key_index,
        indices.map_keys_array_type_idx,
        indices.int_key_box_type_idx,
    );
    emit_key_at(
        &mut function,
        keys,
        previous_index,
        indices.map_keys_array_type_idx,
        indices.int_key_box_type_idx,
    );
    function.instruction(&Instruction::Call(helpers.aint_cmp_fn));
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::I32LtS);
    function.instruction(&Instruction::I32Eqz);
    function.instruction(&Instruction::BrIf(1));
    function.instruction(&Instruction::LocalGet(output_ptr));
    function.instruction(&Instruction::LocalGet(sort_hole));
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Mul);
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalGet(previous_index));
    function.instruction(&Instruction::I32Store(mem4));
    function.instruction(&Instruction::LocalGet(sort_hole));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Sub);
    function.instruction(&Instruction::LocalSet(sort_hole));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::LocalGet(output_ptr));
    function.instruction(&Instruction::LocalGet(sort_hole));
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Mul);
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalGet(sort_key_index));
    function.instruction(&Instruction::I32Store(mem4));
    function.instruction(&Instruction::LocalGet(sort_cursor));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(sort_cursor));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);

    // Build List<Int> in reverse sorted order, preserving original key refs.
    function.instruction(&Instruction::RefNull(HeapType::Concrete(
        indices.list_int_type_idx,
    )));
    function.instruction(&Instruction::LocalSet(list));
    function.instruction(&Instruction::LocalGet(ready_len));
    function.instruction(&Instruction::LocalSet(output_cursor));
    function.instruction(&Instruction::Block(BlockType::Empty));
    function.instruction(&Instruction::Loop(BlockType::Empty));
    function.instruction(&Instruction::LocalGet(output_cursor));
    function.instruction(&Instruction::I32Eqz);
    function.instruction(&Instruction::BrIf(1));
    function.instruction(&Instruction::LocalGet(output_cursor));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Sub);
    function.instruction(&Instruction::LocalSet(output_cursor));
    function.instruction(&Instruction::LocalGet(output_ptr));
    function.instruction(&Instruction::LocalGet(output_cursor));
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Mul);
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::I32Load(mem4));
    function.instruction(&Instruction::LocalSet(ready_map_index));
    emit_key_at(
        &mut function,
        keys,
        ready_map_index,
        indices.map_keys_array_type_idx,
        indices.int_key_box_type_idx,
    );
    function.instruction(&Instruction::LocalGet(list));
    function.instruction(&Instruction::StructNew(indices.list_int_type_idx));
    function.instruction(&Instruction::LocalSet(list));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::LocalGet(list));
    function.instruction(&Instruction::RefNull(HeapType::Concrete(
        indices.string_type_idx,
    )));
    function.instruction(&Instruction::StructNew(indices.result_type_idx));
    restore_bump(&mut function, saved_alloc, helpers.bump_alloc_ptr_global);
    function.instruction(&Instruction::End);
    function
}

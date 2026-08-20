//! Direct WASI 0.2 lowering for bounded positional disk reads.
//!
//! `Disk.readBytesAt` deliberately does not reuse the whole-file read helper:
//! the offset is passed to `read-via-stream`, and `blocking-read` is capped by
//! the remaining requested length. EOF is a successful short read.

use wasm_encoder::{BlockType, Function, HeapType, Instruction, MemArg, RefType, ValType};

pub(super) struct DiskReadBytesAtIndices {
    pub fn_type: u32,
    pub fn_idx: u32,
    pub string_type_idx: u32,
    pub aint_struct_type_idx: u32,
    pub list_int_type_idx: u32,
}

#[allow(clippy::too_many_arguments)]
pub(super) fn emit_disk_read_bytes_at(
    indices: &DiskReadBytesAtIndices,
    preopen_global: u32,
    cabi_realloc_fn: u32,
    str_to_lm_fn: u32,
    get_directories_fn: u32,
    open_at_fn: u32,
    read_via_stream_fn: u32,
    blocking_read_fn: u32,
    drop_descriptor_fn: u32,
    drop_input_stream_fn: u32,
    result_ok_fn: u32,
    result_err_fn: u32,
    aint_from_i64_fn: u32,
) -> Function {
    let string_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.string_type_idx),
    });
    let list_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.list_int_type_idx),
    });

    // Params: path, offset, length.
    // i32 locals: preopen, path_len, three retptrs, fd, stream, buffer ptr/
    // len, chunk ptr/len, clamped copy len, reverse index, preopen list ptr/len,
    // buffer capacity and candidate capacity. i64 locals: validated offset,
    // length, and the bounded per-call request. Ref locals: error String and
    // reverse-built List<Int> payload.
    let mut f = Function::new(vec![
        (17, ValType::I32),
        (3, ValType::I64),
        (1, string_ref),
        (1, list_ref),
    ]);
    let p_path = 0;
    let p_offset = 1;
    let p_length = 2;
    let l_preopen = 3;
    let l_path_len = 4;
    let l_retptr_open = 5;
    let l_retptr_stream = 6;
    let l_retptr_read = 7;
    let l_fd = 8;
    let l_stream = 9;
    let l_buf_ptr = 10;
    let l_buf_len = 11;
    let l_data_ptr = 12;
    let l_data_len = 13;
    let l_copy_len = 14;
    let l_index = 15;
    let l_list_ptr = 16;
    let l_list_len = 17;
    let l_buf_cap = 18;
    let l_new_cap = 19;
    let l_offset = 20;
    let l_length = 21;
    let l_request = 22;
    let l_error = 23;
    let l_list = 24;

    let mem1 = MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    };
    let mem4 = MemArg {
        offset: 0,
        align: 2,
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

    let emit_err = |f: &mut Function, message: &[u8]| {
        f.instruction(&Instruction::I32Const(message.len() as i32));
        f.instruction(&Instruction::ArrayNewDefault(indices.string_type_idx));
        f.instruction(&Instruction::LocalSet(l_error));
        for (index, byte) in message.iter().enumerate() {
            f.instruction(&Instruction::LocalGet(l_error));
            f.instruction(&Instruction::I32Const(index as i32));
            f.instruction(&Instruction::I32Const(i32::from(*byte)));
            f.instruction(&Instruction::ArraySet(indices.string_type_idx));
        }
        f.instruction(&Instruction::LocalGet(l_error));
        f.instruction(&Instruction::Call(result_err_fn));
        f.instruction(&Instruction::Return);
    };

    // Both Int arguments must fit the wasm/WASI scalar fast path and be
    // non-negative. The requested length may exceed wasm's address space when
    // EOF arrives first; only an actually oversized result is rejected later.
    f.instruction(&Instruction::LocalGet(p_offset));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.aint_struct_type_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_err(&mut f, b"offset exceeds WASI host range");
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(p_offset));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.aint_struct_type_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::LocalTee(l_offset));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::I64LtS);
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_err(&mut f, b"offset must be non-negative");
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(p_length));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.aint_struct_type_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_err(&mut f, b"length exceeds WASI host range");
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(p_length));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.aint_struct_type_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::LocalTee(l_length));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::I64LtS);
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_err(&mut f, b"length must be non-negative");
    f.instruction(&Instruction::End);
    // Lazy-fetch and cache the first filesystem preopen.
    f.instruction(&Instruction::GlobalGet(preopen_global));
    f.instruction(&Instruction::LocalSet(l_preopen));
    f.instruction(&Instruction::LocalGet(l_preopen));
    f.instruction(&Instruction::I32Const(-1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::Call(cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr_open));
    f.instruction(&Instruction::LocalGet(l_retptr_open));
    f.instruction(&Instruction::Call(get_directories_fn));
    f.instruction(&Instruction::LocalGet(l_retptr_open));
    f.instruction(&Instruction::I32Load(mem4));
    f.instruction(&Instruction::LocalSet(l_list_ptr));
    f.instruction(&Instruction::LocalGet(l_retptr_open));
    f.instruction(&Instruction::I32Load(mem4_o4));
    f.instruction(&Instruction::LocalSet(l_list_len));
    f.instruction(&Instruction::LocalGet(l_list_len));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_list_ptr));
    f.instruction(&Instruction::I32Load(mem4));
    f.instruction(&Instruction::LocalTee(l_preopen));
    f.instruction(&Instruction::GlobalSet(preopen_global));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(l_preopen));
    f.instruction(&Instruction::I32Const(-1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_err(&mut f, b"no preopens");
    f.instruction(&Instruction::End);

    // Open the path for reading.
    f.instruction(&Instruction::LocalGet(p_path));
    f.instruction(&Instruction::Call(str_to_lm_fn));
    f.instruction(&Instruction::LocalSet(l_path_len));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::Call(cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr_open));
    f.instruction(&Instruction::LocalGet(l_preopen));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(l_path_len));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(l_retptr_open));
    f.instruction(&Instruction::Call(open_at_fn));
    f.instruction(&Instruction::LocalGet(l_retptr_open));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_err(&mut f, b"open failed");
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(l_retptr_open));
    f.instruction(&Instruction::I32Load(mem4_o4));
    f.instruction(&Instruction::LocalSet(l_fd));

    // Ask WASI for a stream beginning at the requested offset.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::Call(cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr_stream));
    f.instruction(&Instruction::LocalGet(l_fd));
    f.instruction(&Instruction::LocalGet(l_offset));
    f.instruction(&Instruction::LocalGet(l_retptr_stream));
    f.instruction(&Instruction::Call(read_via_stream_fn));
    f.instruction(&Instruction::LocalGet(l_retptr_stream));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_fd));
    f.instruction(&Instruction::Call(drop_descriptor_fn));
    emit_err(&mut f, b"read-via-stream failed");
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(l_retptr_stream));
    f.instruction(&Instruction::I32Load(mem4_o4));
    f.instruction(&Instruction::LocalSet(l_stream));

    // Start with at most 4 KiB rather than reserving the caller's entire upper
    // bound. The buffer grows only when bytes actually arrive.
    f.instruction(&Instruction::I32Const(4096));
    f.instruction(&Instruction::LocalSet(l_buf_cap));
    f.instruction(&Instruction::LocalGet(l_length));
    f.instruction(&Instruction::I64Const(4096));
    f.instruction(&Instruction::I64LtU);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_length));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(l_buf_cap));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(l_buf_cap));
    f.instruction(&Instruction::Call(cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_buf_ptr));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(l_buf_len));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(12));
    f.instruction(&Instruction::Call(cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr_read));

    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_buf_len));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::LocalGet(l_length));
    f.instruction(&Instruction::I64GeU);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(l_buf_len));
    f.instruction(&Instruction::I32Const(i32::MAX));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_stream));
    f.instruction(&Instruction::Call(drop_input_stream_fn));
    f.instruction(&Instruction::LocalGet(l_fd));
    f.instruction(&Instruction::Call(drop_descriptor_fn));
    emit_err(&mut f, b"read result exceeds WASI memory range");
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(l_length));
    f.instruction(&Instruction::LocalGet(l_buf_len));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64Sub);
    f.instruction(&Instruction::LocalSet(l_request));
    f.instruction(&Instruction::LocalGet(l_request));
    f.instruction(&Instruction::I64Const(65_536));
    f.instruction(&Instruction::I64GtU);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::I64Const(65_536));
    f.instruction(&Instruction::LocalSet(l_request));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(l_stream));
    f.instruction(&Instruction::LocalGet(l_request));
    f.instruction(&Instruction::LocalGet(l_retptr_read));
    f.instruction(&Instruction::Call(blocking_read_fn));

    f.instruction(&Instruction::LocalGet(l_retptr_read));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_retptr_read));
    f.instruction(&Instruction::I32Load8U(MemArg {
        offset: 4,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::Br(3));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(l_stream));
    f.instruction(&Instruction::Call(drop_input_stream_fn));
    f.instruction(&Instruction::LocalGet(l_fd));
    f.instruction(&Instruction::Call(drop_descriptor_fn));
    emit_err(&mut f, b"read failed");
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(l_retptr_read));
    f.instruction(&Instruction::I32Load(mem4_o4));
    f.instruction(&Instruction::LocalSet(l_data_ptr));
    f.instruction(&Instruction::LocalGet(l_retptr_read));
    f.instruction(&Instruction::I32Load(mem4_o8));
    f.instruction(&Instruction::LocalTee(l_data_len));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));

    // Clamp a misbehaving host to the requested upper bound before copying.
    f.instruction(&Instruction::LocalGet(l_data_len));
    f.instruction(&Instruction::LocalSet(l_copy_len));
    f.instruction(&Instruction::LocalGet(l_copy_len));
    f.instruction(&Instruction::LocalGet(l_request));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32GtU);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_request));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(l_copy_len));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(l_copy_len));
    f.instruction(&Instruction::I32Const(i32::MAX));
    f.instruction(&Instruction::LocalGet(l_buf_len));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32GtU);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_stream));
    f.instruction(&Instruction::Call(drop_input_stream_fn));
    f.instruction(&Instruction::LocalGet(l_fd));
    f.instruction(&Instruction::Call(drop_descriptor_fn));
    emit_err(&mut f, b"read result exceeds WASI memory range");
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(l_buf_len));
    f.instruction(&Instruction::LocalGet(l_copy_len));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(l_buf_cap));
    f.instruction(&Instruction::I32GtU);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_buf_cap));
    f.instruction(&Instruction::LocalSet(l_new_cap));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_new_cap));
    f.instruction(&Instruction::LocalGet(l_buf_len));
    f.instruction(&Instruction::LocalGet(l_copy_len));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(l_new_cap));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::LocalSet(l_new_cap));
    f.instruction(&Instruction::LocalGet(l_new_cap));
    f.instruction(&Instruction::I32Const(i32::MAX));
    f.instruction(&Instruction::I32GtU);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::I32Const(i32::MAX));
    f.instruction(&Instruction::LocalSet(l_new_cap));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(l_buf_ptr));
    f.instruction(&Instruction::LocalGet(l_buf_cap));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(l_new_cap));
    f.instruction(&Instruction::Call(cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_buf_ptr));
    f.instruction(&Instruction::LocalGet(l_new_cap));
    f.instruction(&Instruction::LocalSet(l_buf_cap));
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(l_buf_ptr));
    f.instruction(&Instruction::LocalGet(l_buf_len));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(l_data_ptr));
    f.instruction(&Instruction::LocalGet(l_copy_len));
    f.instruction(&Instruction::MemoryCopy {
        src_mem: 0,
        dst_mem: 0,
    });
    f.instruction(&Instruction::LocalGet(l_buf_len));
    f.instruction(&Instruction::LocalGet(l_copy_len));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l_buf_len));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(l_stream));
    f.instruction(&Instruction::Call(drop_input_stream_fn));
    f.instruction(&Instruction::LocalGet(l_fd));
    f.instruction(&Instruction::Call(drop_descriptor_fn));

    // Build the source-defined Bytes carrier from the exact short-read length.
    f.instruction(&Instruction::RefNull(HeapType::Concrete(
        indices.list_int_type_idx,
    )));
    f.instruction(&Instruction::LocalSet(l_list));
    f.instruction(&Instruction::LocalGet(l_buf_len));
    f.instruction(&Instruction::LocalSet(l_index));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_index));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(l_index));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(l_index));
    f.instruction(&Instruction::LocalGet(l_buf_ptr));
    f.instruction(&Instruction::LocalGet(l_index));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::Call(aint_from_i64_fn));
    f.instruction(&Instruction::LocalGet(l_list));
    f.instruction(&Instruction::StructNew(indices.list_int_type_idx));
    f.instruction(&Instruction::LocalSet(l_list));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(l_list));
    f.instruction(&Instruction::Call(result_ok_fn));
    f.instruction(&Instruction::End);
    f
}

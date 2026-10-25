//! Direct WASI 0.2 lowering for bounded positional disk reads.
//!
//! `Disk.readBytesAt` deliberately does not reuse the whole-file read helper:
//! the offset is passed to `read-via-stream`, and `blocking-read` is capped by
//! the remaining requested length. EOF is a successful short read.
//!
//! Linear memory is only a landing area here: every chunk the host hands back
//! is copied into a GC byte array before the next read, so the next chunk
//! lands where the one before it did, and the helper gives back all the
//! linear memory it took when it returns. A file of any size is read in the
//! same few kilobytes of linear memory (`emit_stream_into_array`, shared with
//! `Disk.readText`).

use wasm_encoder::{BlockType, Function, HeapType, Instruction, MemArg, RefType, ValType};

pub(super) struct DiskReadBytesAtIndices {
    pub fn_type: u32,
    pub fn_idx: u32,
    pub string_type_idx: u32,
    pub aint_struct_type_idx: u32,
    pub list_int_type_idx: u32,
    /// The proof-packed `Bytes` array and `Result<Bytes, String>` struct types.
    pub packed_bytes: Option<(u32, u32)>,
}

#[allow(clippy::too_many_arguments)]
pub(super) fn emit_disk_read_bytes_at(
    indices: &DiskReadBytesAtIndices,
    preopen_global: u32,
    bump_global: u32,
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
    // i32 locals: preopen, path_len, three retptrs, fd, stream, bytes read,
    // chunk ptr/len, copy and reverse index, preopen list ptr/len, and the
    // linear-memory cursor on entry and after the call's own scratch. i64
    // locals: validated offset, length, and the bounded per-call request. Ref
    // locals: error String, reverse-built List<Int> payload, and the byte
    // array the chunks are copied into with its grown replacement.
    let mut f = Function::new(vec![
        (15, ValType::I32),
        (3, ValType::I64),
        (1, string_ref),
        (1, list_ref),
        (2, string_ref),
        (
            1,
            bytes_out_ref(indices.packed_bytes, indices.string_type_idx),
        ),
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
    let l_buf_len = 10;
    let l_data_ptr = 11;
    let l_data_len = 12;
    let l_index = 13;
    let l_list_ptr = 14;
    let l_list_len = 15;
    let l_saved = 16;
    let l_mark = 17;
    let l_offset = 18;
    let l_length = 19;
    let l_request = 20;
    let l_error = 21;
    let l_list = 22;
    let l_buf = 23;
    let l_fresh = 24;
    let l_out = 25;

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
        f.instruction(&Instruction::LocalGet(l_saved));
        f.instruction(&Instruction::GlobalSet(bump_global));
        f.instruction(&Instruction::LocalGet(l_error));
        f.instruction(&Instruction::Call(result_err_fn));
        f.instruction(&Instruction::Return);
    };

    f.instruction(&Instruction::GlobalGet(bump_global));
    f.instruction(&Instruction::LocalSet(l_saved));

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
    f.instruction(&Instruction::LocalGet(l_length));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Const(4096));
    f.instruction(&Instruction::LocalGet(l_length));
    f.instruction(&Instruction::I64Const(4096));
    f.instruction(&Instruction::I64LtU);
    f.instruction(&Instruction::Select);
    f.instruction(&Instruction::ArrayNewDefault(indices.string_type_idx));
    f.instruction(&Instruction::LocalSet(l_buf));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(l_buf_len));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(12));
    f.instruction(&Instruction::Call(cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr_read));
    f.instruction(&Instruction::GlobalGet(bump_global));
    f.instruction(&Instruction::LocalSet(l_mark));
    emit_stream_into_array(
        &mut f,
        &StreamLocals {
            stream: l_stream,
            retptr: l_retptr_read,
            mark: l_mark,
            data_ptr: l_data_ptr,
            data_len: l_data_len,
            index: l_index,
            len: l_buf_len,
            buf: l_buf,
            fresh: l_fresh,
            bound: Some((l_length, l_request)),
        },
        indices.string_type_idx,
        bump_global,
        blocking_read_fn,
        &|f, message| {
            f.instruction(&Instruction::LocalGet(l_stream));
            f.instruction(&Instruction::Call(drop_input_stream_fn));
            f.instruction(&Instruction::LocalGet(l_fd));
            f.instruction(&Instruction::Call(drop_descriptor_fn));
            emit_err(f, message);
        },
    );

    f.instruction(&Instruction::LocalGet(l_stream));
    f.instruction(&Instruction::Call(drop_input_stream_fn));
    f.instruction(&Instruction::LocalGet(l_fd));
    f.instruction(&Instruction::Call(drop_descriptor_fn));
    f.instruction(&Instruction::LocalGet(l_saved));
    f.instruction(&Instruction::GlobalSet(bump_global));

    emit_ok_bytes(
        &mut f,
        &BytesOk {
            string_type: indices.string_type_idx,
            list_type: indices.list_int_type_idx,
            aint_from_i64: aint_from_i64_fn,
            result_ok: result_ok_fn,
            packed: indices.packed_bytes,
        },
        l_buf,
        l_buf_len,
        l_out,
        l_list,
        l_index,
    );
    f.instruction(&Instruction::End);
    f
}

/// How a read helper hands over its bytes: straight into the proof-packed
/// `Bytes` array when the program has one, otherwise as the boxed `List<Int>`
/// the Ok factory takes (and packs, when it packs).
pub(super) struct BytesOk {
    pub string_type: u32,
    pub list_type: u32,
    pub aint_from_i64: u32,
    pub result_ok: u32,
    /// The packed `Bytes` array type and the `Result<Bytes, String>` struct.
    pub packed: Option<(u32, u32)>,
}

/// The type of the local `emit_ok_bytes` builds a packed result in; unused
/// (and typed as the string array) when `Bytes` is not packed.
pub(super) fn bytes_out_ref(packed: Option<(u32, u32)>, string_type: u32) -> ValType {
    ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(packed.map_or(string_type, |(array, _)| array)),
    })
}

/// Leaves `Result.Ok` of the first `len` bytes of the string-typed array in
/// `buf`. `out` has the type `bytes_out_ref` gives, `list` is a `List<Int>`
/// local and `index` an i32 one.
pub(super) fn emit_ok_bytes(
    f: &mut Function,
    ok: &BytesOk,
    buf: u32,
    len: u32,
    out: u32,
    list: u32,
    index: u32,
) {
    if let Some((array, result)) = ok.packed {
        f.instruction(&Instruction::I32Const(1));
        f.instruction(&Instruction::LocalGet(len));
        f.instruction(&Instruction::ArrayNewDefault(array));
        f.instruction(&Instruction::LocalTee(out));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::LocalGet(buf));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::LocalGet(len));
        f.instruction(&Instruction::ArrayCopy {
            array_type_index_dst: array,
            array_type_index_src: ok.string_type,
        });
        f.instruction(&Instruction::LocalGet(out));
        f.instruction(&Instruction::RefNull(HeapType::Concrete(ok.string_type)));
        f.instruction(&Instruction::StructNew(result));
        return;
    }
    // Cons from the end so the list keeps the file's order.
    f.instruction(&Instruction::RefNull(HeapType::Concrete(ok.list_type)));
    f.instruction(&Instruction::LocalSet(list));
    f.instruction(&Instruction::LocalGet(len));
    f.instruction(&Instruction::LocalSet(index));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(index));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(index));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(index));
    f.instruction(&Instruction::LocalGet(buf));
    f.instruction(&Instruction::LocalGet(index));
    f.instruction(&Instruction::ArrayGetU(ok.string_type));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::Call(ok.aint_from_i64));
    f.instruction(&Instruction::LocalGet(list));
    f.instruction(&Instruction::StructNew(ok.list_type));
    f.instruction(&Instruction::LocalSet(list));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(list));
    f.instruction(&Instruction::Call(ok.result_ok));
}

/// The locals of a host helper that `emit_stream_into_array` reads and sets.
pub(super) struct StreamLocals {
    /// The open `input-stream` handle.
    pub stream: u32,
    /// A 12-byte area for `blocking-read`'s result.
    pub retptr: u32,
    /// The bump cursor just past the helper's own scratch; each chunk lands here.
    pub mark: u32,
    pub data_ptr: u32,
    pub data_len: u32,
    pub index: u32,
    /// Bytes copied into `buf` so far.
    pub len: u32,
    /// The GC byte array the chunks are copied into; its length is the capacity.
    pub buf: u32,
    pub fresh: u32,
    /// For a bounded read, the i64 locals holding the most bytes to read and the
    /// per-call request; `None` reads to the end of the stream.
    pub bound: Option<(u32, u32)>,
}

/// Reads `stream` to its end, or to the bound, into the GC byte array in
/// `buf`, growing it by doubling. The bump cursor goes back to `mark` before
/// every `blocking-read`, so the host's allocation for each chunk reuses the
/// linear memory of the chunk before it, already copied out. `fail` emits the
/// helper's cleanup and error return for a failed read.
pub(super) fn emit_stream_into_array(
    f: &mut Function,
    l: &StreamLocals,
    array_type: u32,
    bump_global: u32,
    blocking_read_fn: u32,
    fail: &dyn Fn(&mut Function, &[u8]),
) {
    let byte = MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    };
    let word_at = |offset| MemArg {
        offset,
        align: 2,
        memory_index: 0,
    };
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    if let Some((limit, request)) = l.bound {
        f.instruction(&Instruction::LocalGet(l.len));
        f.instruction(&Instruction::I64ExtendI32U);
        f.instruction(&Instruction::LocalGet(limit));
        f.instruction(&Instruction::I64GeU);
        f.instruction(&Instruction::BrIf(1));
        f.instruction(&Instruction::LocalGet(limit));
        f.instruction(&Instruction::LocalGet(l.len));
        f.instruction(&Instruction::I64ExtendI32U);
        f.instruction(&Instruction::I64Sub);
        f.instruction(&Instruction::LocalSet(request));
        f.instruction(&Instruction::LocalGet(request));
        f.instruction(&Instruction::I64Const(65_536));
        f.instruction(&Instruction::I64GtU);
        f.instruction(&Instruction::If(BlockType::Empty));
        f.instruction(&Instruction::I64Const(65_536));
        f.instruction(&Instruction::LocalSet(request));
        f.instruction(&Instruction::End);
    }
    f.instruction(&Instruction::LocalGet(l.mark));
    f.instruction(&Instruction::GlobalSet(bump_global));
    f.instruction(&Instruction::LocalGet(l.stream));
    match l.bound {
        Some((_, request)) => f.instruction(&Instruction::LocalGet(request)),
        None => f.instruction(&Instruction::I64Const(65_536)),
    };
    f.instruction(&Instruction::LocalGet(l.retptr));
    f.instruction(&Instruction::Call(blocking_read_fn));

    // Err: a closed stream (stream-error tag 1 at +4) is the end of the file.
    f.instruction(&Instruction::LocalGet(l.retptr));
    f.instruction(&Instruction::I32Load8U(byte));
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l.retptr));
    f.instruction(&Instruction::I32Load8U(MemArg {
        offset: 4,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::BrIf(2));
    fail(f, b"read failed");
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(l.retptr));
    f.instruction(&Instruction::I32Load(word_at(4)));
    f.instruction(&Instruction::LocalSet(l.data_ptr));
    f.instruction(&Instruction::LocalGet(l.retptr));
    f.instruction(&Instruction::I32Load(word_at(8)));
    f.instruction(&Instruction::LocalTee(l.data_len));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    if let Some((_, request)) = l.bound {
        // Clamp a misbehaving host to the requested upper bound.
        f.instruction(&Instruction::LocalGet(l.data_len));
        f.instruction(&Instruction::LocalGet(request));
        f.instruction(&Instruction::I32WrapI64);
        f.instruction(&Instruction::I32GtU);
        f.instruction(&Instruction::If(BlockType::Empty));
        f.instruction(&Instruction::LocalGet(request));
        f.instruction(&Instruction::I32WrapI64);
        f.instruction(&Instruction::LocalSet(l.data_len));
        f.instruction(&Instruction::End);
    }
    f.instruction(&Instruction::LocalGet(l.data_len));
    f.instruction(&Instruction::I32Const(i32::MAX));
    f.instruction(&Instruction::LocalGet(l.len));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32GtU);
    f.instruction(&Instruction::If(BlockType::Empty));
    fail(f, b"read result exceeds WASI memory range");
    f.instruction(&Instruction::End);

    // Grow to twice the capacity, at most i32::MAX, at least what is needed.
    f.instruction(&Instruction::LocalGet(l.len));
    f.instruction(&Instruction::LocalGet(l.data_len));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(l.buf));
    f.instruction(&Instruction::ArrayLen);
    f.instruction(&Instruction::I32GtU);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l.buf));
    f.instruction(&Instruction::ArrayLen);
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::LocalTee(l.index));
    f.instruction(&Instruction::I32Const(i32::MAX));
    f.instruction(&Instruction::LocalGet(l.index));
    f.instruction(&Instruction::I32Const(i32::MAX));
    f.instruction(&Instruction::I32LeU);
    f.instruction(&Instruction::Select);
    f.instruction(&Instruction::LocalTee(l.index));
    f.instruction(&Instruction::LocalGet(l.len));
    f.instruction(&Instruction::LocalGet(l.data_len));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(l.index));
    f.instruction(&Instruction::LocalGet(l.len));
    f.instruction(&Instruction::LocalGet(l.data_len));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::Select);
    f.instruction(&Instruction::ArrayNewDefault(array_type));
    f.instruction(&Instruction::LocalTee(l.fresh));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(l.buf));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(l.len));
    f.instruction(&Instruction::ArrayCopy {
        array_type_index_dst: array_type,
        array_type_index_src: array_type,
    });
    f.instruction(&Instruction::LocalGet(l.fresh));
    f.instruction(&Instruction::LocalSet(l.buf));
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(l.index));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l.index));
    f.instruction(&Instruction::LocalGet(l.data_len));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(l.buf));
    f.instruction(&Instruction::LocalGet(l.len));
    f.instruction(&Instruction::LocalGet(l.index));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(l.data_ptr));
    f.instruction(&Instruction::LocalGet(l.index));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Load8U(byte));
    f.instruction(&Instruction::ArraySet(array_type));
    f.instruction(&Instruction::LocalGet(l.index));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l.index));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(l.len));
    f.instruction(&Instruction::LocalGet(l.data_len));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l.len));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
}

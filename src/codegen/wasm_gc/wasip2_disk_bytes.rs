//! Whole-file binary and metadata `Disk.*` adapters for direct WASI 0.2 lowering.
//!
//! The filesystem pipeline already moves file contents as raw octets; its
//! `String` carrier is an `(array i8)` with no UTF-8 transcoding. These small
//! The byte adapters reuse that battle-tested open/read/write/drop machinery while
//! converting only the guest-visible carrier between nominal `Bytes` and the
//! internal raw array. Invalid UTF-8 therefore remains byte-for-byte intact.

use wasm_encoder::{BlockType, Function, HeapType, Instruction, RefType, ValType};

pub(super) struct DiskReadBytesIndices {
    pub fn_type: u32,
    pub fn_idx: u32,
    pub string_type_idx: u32,
    pub result_string_string_type_idx: u32,
    pub list_int_type_idx: u32,
}

pub(super) struct DiskWriteBytesIndices {
    pub fn_type: u32,
    pub fn_idx: u32,
    pub string_type_idx: u32,
    pub bytes_type_idx: u32,
    pub list_int_type_idx: u32,
    pub aint_struct_type_idx: u32,
    pub result_unit_string_type_idx: u32,
}

pub(super) struct DiskSizeIndices {
    pub fn_type: u32,
    pub fn_idx: u32,
    pub string_type_idx: u32,
    pub aint_struct_type_idx: u32,
    pub result_int_string_type_idx: u32,
}

#[allow(clippy::too_many_arguments)]
pub(super) fn emit_disk_size(
    indices: &DiskSizeIndices,
    preopen_global: u32,
    cabi_realloc_fn: u32,
    str_to_lm_fn: u32,
    get_directories_fn: u32,
    stat_at_fn: u32,
    aint_from_i64_fn: u32,
) -> Function {
    use wasm_encoder::MemArg;

    let string_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.string_type_idx),
    });
    // Param 0 = path. Locals 1..=5 are preopen/path-len/retptr/list-ptr/
    // list-len; local 6 holds an error String.
    let mut f = Function::new(vec![(5, ValType::I32), (1, string_ref)]);
    let l_preopen = 1;
    let l_path_len = 2;
    let l_retptr = 3;
    let l_list_ptr = 4;
    let l_list_len = 5;
    let l_error = 6;
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
    let mem1 = MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    };
    // result payload starts at retptr+8; descriptor-stat.size is its third
    // field at +16, hence +24 from the result base.
    let mem8_size = MemArg {
        offset: 24,
        align: 3,
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
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::RefNull(HeapType::Concrete(
            indices.aint_struct_type_idx,
        )));
        f.instruction(&Instruction::LocalGet(l_error));
        f.instruction(&Instruction::StructNew(indices.result_int_string_type_idx));
        f.instruction(&Instruction::Return);
    };

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
    f.instruction(&Instruction::LocalSet(l_retptr));
    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::Call(get_directories_fn));
    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::I32Load(mem4));
    f.instruction(&Instruction::LocalSet(l_list_ptr));
    f.instruction(&Instruction::LocalGet(l_retptr));
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

    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Call(str_to_lm_fn));
    f.instruction(&Instruction::LocalSet(l_path_len));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Const(96));
    f.instruction(&Instruction::Call(cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_retptr));
    f.instruction(&Instruction::LocalGet(l_preopen));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(l_path_len));
    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::Call(stat_at_fn));
    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::I32Load8U(mem1));
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_err(&mut f, b"stat failed");
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::I64Load(mem8_size));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::I64LtS);
    f.instruction(&Instruction::If(BlockType::Empty));
    emit_err(&mut f, b"file size exceeds Int host range");
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(l_retptr));
    f.instruction(&Instruction::I64Load(mem8_size));
    f.instruction(&Instruction::Call(aint_from_i64_fn));
    f.instruction(&Instruction::RefNull(HeapType::Concrete(
        indices.string_type_idx,
    )));
    f.instruction(&Instruction::StructNew(indices.result_int_string_type_idx));
    f.instruction(&Instruction::End);
    f
}

/// Convert the raw `(array i8)` success payload of the internal read-text
/// filesystem helper into nominal `Bytes`; its Err payload passes through.
pub(super) fn emit_disk_read_bytes(
    indices: &DiskReadBytesIndices,
    read_text_fn: u32,
    result_ok_fn: u32,
    result_err_fn: u32,
    aint_from_i64_fn: u32,
) -> Function {
    let result_string_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.result_string_string_type_idx),
    });
    let string_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.string_type_idx),
    });
    let list_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.list_int_type_idx),
    });
    // Param 0 = path. Locals: 1 = Result<String,String>, 2 = raw array,
    // 3 = reverse-built List<Int>, 4 = descending byte index.
    let mut f = Function::new(vec![
        (1, result_string_ref),
        (1, string_ref),
        (1, list_ref),
        (1, ValType::I32),
    ]);
    let l_result = 1;
    let l_raw = 2;
    let l_list = 3;
    let l_index = 4;

    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Call(read_text_fn));
    f.instruction(&Instruction::LocalSet(l_result));

    // Result tag: 1 = Ok, 0 = Err.
    f.instruction(&Instruction::LocalGet(l_result));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.result_string_string_type_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_result));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.result_string_string_type_idx,
        field_index: 2,
    });
    f.instruction(&Instruction::Call(result_err_fn));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(l_result));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.result_string_string_type_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalSet(l_raw));
    f.instruction(&Instruction::RefNull(HeapType::Concrete(
        indices.list_int_type_idx,
    )));
    f.instruction(&Instruction::LocalSet(l_list));
    f.instruction(&Instruction::LocalGet(l_raw));
    f.instruction(&Instruction::ArrayLen);
    f.instruction(&Instruction::LocalSet(l_index));

    // Cons from the end so the private linked list preserves file order.
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_index));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(l_index));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(l_index));
    f.instruction(&Instruction::LocalGet(l_raw));
    f.instruction(&Instruction::LocalGet(l_index));
    f.instruction(&Instruction::ArrayGetU(indices.string_type_idx));
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

/// Convert nominal `Bytes` to an internal raw `(array i8)` and delegate to
/// the existing write-text or append-text filesystem helper. No character
/// encoding is performed; the array is merely a byte carrier.
pub(super) fn emit_disk_write_bytes(
    indices: &DiskWriteBytesIndices,
    write_text_fn: u32,
    bytes_unpack_fn: Option<u32>,
    effect: &'static str,
) -> Function {
    let string_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.string_type_idx),
    });
    let list_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(indices.list_int_type_idx),
    });
    // Params: 0 = path, 1 = Bytes. Locals: 2 = raw array, 3 = list,
    // 4 = byte length, 5 = forward index, 6 = current small Int.
    let mut f = Function::new(vec![
        (1, string_ref),
        (1, list_ref),
        (2, ValType::I32),
        (1, ValType::I64),
    ]);
    let l_raw = 2;
    let l_list = 3;
    let l_len = 4;
    let l_index = 5;
    let l_byte = 6;

    let emit_payload_list = |f: &mut Function| {
        f.instruction(&Instruction::LocalGet(1));
        if let Some(unpack_fn) = bytes_unpack_fn {
            f.instruction(&Instruction::Call(unpack_fn));
        } else {
            f.instruction(&Instruction::StructGet {
                struct_type_index: indices.bytes_type_idx,
                field_index: 0,
            });
        }
        f.instruction(&Instruction::LocalSet(l_list));
    };
    let malformed = format!("{effect}: malformed Bytes carrier");
    let emit_malformed = |f: &mut Function| {
        let bytes = malformed.as_bytes();
        f.instruction(&Instruction::I32Const(bytes.len() as i32));
        f.instruction(&Instruction::ArrayNewDefault(indices.string_type_idx));
        f.instruction(&Instruction::LocalSet(l_raw));
        for (index, byte) in bytes.iter().enumerate() {
            f.instruction(&Instruction::LocalGet(l_raw));
            f.instruction(&Instruction::I32Const(index as i32));
            f.instruction(&Instruction::I32Const(i32::from(*byte)));
            f.instruction(&Instruction::ArraySet(indices.string_type_idx));
        }
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::LocalGet(l_raw));
        f.instruction(&Instruction::StructNew(indices.result_unit_string_type_idx));
        f.instruction(&Instruction::Return);
    };

    // Validate the opaque carrier and count octets.
    emit_payload_list(&mut f);
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
    emit_malformed(&mut f);
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

    f.instruction(&Instruction::LocalGet(l_len));
    f.instruction(&Instruction::ArrayNewDefault(indices.string_type_idx));
    f.instruction(&Instruction::LocalSet(l_raw));
    emit_payload_list(&mut f);
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(l_index));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_list));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(l_raw));
    f.instruction(&Instruction::LocalGet(l_index));
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
    f.instruction(&Instruction::ArraySet(indices.string_type_idx));
    f.instruction(&Instruction::LocalGet(l_list));
    f.instruction(&Instruction::StructGet {
        struct_type_index: indices.list_int_type_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalSet(l_list));
    f.instruction(&Instruction::LocalGet(l_index));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l_index));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(l_raw));
    f.instruction(&Instruction::Call(write_text_fn));
    f.instruction(&Instruction::End);
    f
}

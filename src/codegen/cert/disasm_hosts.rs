fn host_op(op: &wasmparser::Operator<'_>) -> HostOp {
    match op {
        wasmparser::Operator::LocalGet { local_index } => HostOp::LocalGet(*local_index),
        wasmparser::Operator::LocalSet { local_index } => HostOp::LocalSet(*local_index),
        wasmparser::Operator::I32Const { value } => HostOp::I32Const(*value),
        wasmparser::Operator::ArrayLen => HostOp::ArrayLen,
        wasmparser::Operator::ArrayGetU { array_type_index } => {
            HostOp::ArrayGetU(*array_type_index)
        }
        wasmparser::Operator::ArrayGet { array_type_index } => HostOp::ArrayGet(*array_type_index),
        wasmparser::Operator::ArrayNewDefault { array_type_index } => {
            HostOp::ArrayNewDefault(*array_type_index)
        }
        wasmparser::Operator::ArrayCopy {
            array_type_index_dst,
            array_type_index_src,
        } => HostOp::ArrayCopy(*array_type_index_dst, *array_type_index_src),
        wasmparser::Operator::I32Ne => HostOp::I32Ne,
        wasmparser::Operator::I32GeU => HostOp::I32GeU,
        wasmparser::Operator::I32Add => HostOp::I32Add,
        wasmparser::Operator::If { .. } => HostOp::If,
        wasmparser::Operator::Block { .. } => HostOp::Block,
        wasmparser::Operator::Loop { .. } => HostOp::Loop,
        wasmparser::Operator::Br { relative_depth } => HostOp::Br(*relative_depth),
        wasmparser::Operator::BrIf { relative_depth } => HostOp::BrIf(*relative_depth),
        wasmparser::Operator::Return => HostOp::Return,
        wasmparser::Operator::End => HostOp::End,
        _ => HostOp::Other,
    }
}

fn is_string_eq_host(
    entry: &CodeEntry,
    params: &[TyKind],
    result: Option<TyKind>,
    string_byte_array_types: &std::collections::HashSet<u32>,
) -> bool {
    let [TyKind::Ref { idx: lhs, .. }, TyKind::Ref { idx: rhs, .. }] = params else {
        return false;
    };
    if lhs != rhs || result != Some(TyKind::I32) || entry.nlocals != 2 || !entry.calls.is_empty() {
        return false;
    }
    use HostOp::*;
    let t = *lhs;
    if !string_byte_array_types.contains(&t) {
        return false;
    }
    let expected = [
        LocalGet(0),
        ArrayLen,
        LocalGet(1),
        ArrayLen,
        I32Ne,
        If,
        I32Const(0),
        Return,
        End,
        LocalGet(0),
        ArrayLen,
        LocalSet(2),
        I32Const(0),
        LocalSet(3),
        Block,
        Loop,
        LocalGet(3),
        LocalGet(2),
        I32GeU,
        BrIf(1),
        LocalGet(0),
        LocalGet(3),
        ArrayGetU(t),
        LocalGet(1),
        LocalGet(3),
        ArrayGetU(t),
        I32Ne,
        If,
        I32Const(0),
        Return,
        End,
        LocalGet(3),
        I32Const(1),
        I32Add,
        LocalSet(3),
        Br(0),
        End,
        End,
        I32Const(1),
        End,
    ];
    entry.host_ops.as_slice() == expected
}

/// Identify the `String.concat` runtime helper by its byte-exact opcode shape.
///
/// The compiler lowers `String.concat` (and the `[s1, ..., sN].join` pattern)
/// to a fixed two-loop helper: the first loop sums the byte-lengths of every
/// element array, `array.new_default` allocates the result, and the second
/// loop `array.copy`s each element into place. The helper takes ONE argument
/// (the container array of string-arrays) and returns the byte-concatenated
/// array. As with [`is_string_eq_host`], the match is byte-exact — a helper
/// with the right signature but a different body is NOT recognised, so a
/// tampered helper fails re-derivation at verify time.
///
/// `container_ty` is the type index of the container array (array of
/// string-arrays); `byte_ty` is the type index of both the element string
/// arrays and the result byte array. Both must be `(array (mut i8))`
/// composites — pinned by membership in `string_byte_array_types`.
fn is_string_concat_host(
    entry: &CodeEntry,
    params: &[TyKind],
    result: Option<TyKind>,
    string_byte_array_types: &std::collections::HashSet<u32>,
) -> bool {
    // Signature: `(ref null container_ty) -> (ref null byte_ty)`, one arg.
    let [TyKind::Ref { idx: container_ty, .. }] = params else {
        return false;
    };
    let Some(TyKind::Ref { idx: result_ty, .. }) = result else {
        return false;
    };
    // The helper has exactly 7 locals (i32 i32 i32 ref i32 ref i32) and no
    // outgoing calls — everything it does is inline loops + array ops.
    if entry.nlocals != 7 || !entry.calls.is_empty() {
        return false;
    }
    let container = *container_ty;
    let byte = result_ty;
    // Both the result/element type and the container type must be byte arrays.
    // (The container is an array of byte-arrays, so its element type is the
    // byte array type; we check the byte type membership directly, and below
    // we pin the container type into the array.get/len operands.)
    if !string_byte_array_types.contains(&byte) {
        return false;
    }
    use HostOp::*;
    // First loop: total length = sum of element array lengths.
    // Second loop: allocate result, array.copy each element in place.
    let expected = [
        // --- length-accumulation loop ---
        LocalGet(0),
        ArrayLen,
        LocalSet(3),
        I32Const(0),
        LocalSet(1),
        I32Const(0),
        LocalSet(2),
        Block,
        Loop,
        LocalGet(2),
        LocalGet(3),
        I32GeU,
        BrIf(1),
        LocalGet(1),
        LocalGet(0),
        LocalGet(2),
        ArrayGet(container),
        ArrayLen,
        I32Add,
        LocalSet(1),
        LocalGet(2),
        I32Const(1),
        I32Add,
        LocalSet(2),
        Br(0),
        End,
        End,
        // --- allocation + copy loop ---
        LocalGet(1),
        ArrayNewDefault(byte),
        LocalSet(6),
        I32Const(0),
        LocalSet(7),
        I32Const(0),
        LocalSet(2),
        Block,
        Loop,
        LocalGet(2),
        LocalGet(3),
        I32GeU,
        BrIf(1),
        LocalGet(0),
        LocalGet(2),
        ArrayGet(container),
        LocalSet(4),
        LocalGet(4),
        ArrayLen,
        LocalSet(5),
        LocalGet(6),
        LocalGet(7),
        LocalGet(4),
        I32Const(0),
        LocalGet(5),
        ArrayCopy(byte, byte),
        LocalGet(7),
        LocalGet(5),
        I32Add,
        LocalSet(7),
        LocalGet(2),
        I32Const(1),
        I32Add,
        LocalSet(2),
        Br(0),
        End,
        End,
        LocalGet(6),
        End,
    ];
    entry.host_ops.as_slice() == expected
}

fn resolve_data_ops(ops: Vec<Op>, data_segments: &[Option<Vec<u8>>]) -> Vec<Op> {
    ops.into_iter()
        .map(|op| match op {
            Op::ArrayNewDataUnresolved {
                type_idx,
                data_idx,
                offset,
                len,
            } if offset == 0 && len >= 0 => {
                let Some(Some(bytes)) = data_segments.get(data_idx as usize) else {
                    return Op::Other;
                };
                if bytes.len() == len as usize {
                    Op::ArrayNewData {
                        type_idx,
                        data_idx,
                        bytes: bytes.clone(),
                    }
                } else {
                    Op::Other
                }
            }
            Op::ArrayNewDataUnresolved { .. } => Op::Other,
            other => other,
        })
        .collect()
}

fn heap_type_index(hty: wasmparser::HeapType) -> Option<u32> {
    match hty {
        wasmparser::HeapType::Concrete(idx) | wasmparser::HeapType::Exact(idx) => {
            idx.as_module_index()
        }
        wasmparser::HeapType::Abstract { .. } => None,
    }
}

type DisasmResult = (
    Vec<UserFn>,
    // Int box helper (`__rt_aint_from_i64`) export index. `None` when the
    // module does not export it — a legal module shape (no Int arithmetic);
    // every integer-family recognizer then declines fail-closed instead of
    // aborting the whole analysis.
    Option<u32>,
    std::collections::HashSet<u32>,
    Option<u32>,
    std::collections::HashMap<u32, HostRole>,
    FragHostTable,
    // Byte-derived struct context: module struct type index -> field count.
    // The plan checker validates every `struct.get.user` node against it.
    std::collections::HashMap<u32, u32>,
);

/// The first `i64` arithmetic operator in a helper body. Strictly narrower
/// than the other host-shape evidence: the plan-first host-role table binds
/// behavioural add, subtract, and multiply to distinct byte-derived helpers.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FirstI64Arith {
    Add,
    Sub,
    Mul,
}

/// Byte-for-byte mirror of the certificate decoder's first-arith body scan.
///
/// The Lean decoder walks raw instruction boundaries with a fixed vocabulary
/// and fails closed on any encoding outside it. The producer runs this exact
/// mirror over every carrier-binop-signature body so a module whose role scan
/// the decoder cannot complete is refused at certification time — no package
/// is ever emitted whose module-wide role pin can close against no manifest
/// value. Returns `None` when the decoder's scan would fail, `Some(first)`
/// with the first `i64` add/sub/mul marker (or `Some(None)` for none) when it
/// succeeds.
fn kernel_first_arith_scan(bytes: &[u8]) -> Option<Option<FirstI64Arith>> {
    // Canonical unsigned LEB128, at most `fuel` bytes, overlong-zero rejected.
    fn skip_uleb(bytes: &[u8], cursor: &mut usize) -> Option<u64> {
        let mut value: u64 = 0;
        let mut shift: u32 = 0;
        for _ in 0..5 {
            let byte = *bytes.get(*cursor)?;
            *cursor += 1;
            value |= u64::from(byte & 0x7f) << shift;
            if byte < 128 {
                if shift != 0 && byte == 0 {
                    return None;
                }
                return Some(value);
            }
            shift += 7;
        }
        None
    }
    // Signed LEB128, at most 10 bytes; only the cursor movement matters here.
    fn skip_sleb(bytes: &[u8], cursor: &mut usize) -> Option<()> {
        for _ in 0..10 {
            let byte = *bytes.get(*cursor)?;
            *cursor += 1;
            if byte < 128 {
                return Some(());
            }
        }
        None
    }
    fn skip_block_type(bytes: &[u8], cursor: &mut usize) -> Option<()> {
        let byte = *bytes.get(*cursor)?;
        if byte == 0x40 || matches!(byte, 0x7b..=0x7f) {
            *cursor += 1;
            Some(())
        } else if byte == 0x63 || byte == 0x64 {
            *cursor += 1;
            skip_sleb(bytes, cursor)
        } else {
            skip_sleb(bytes, cursor)
        }
    }

    let mut cursor = 0usize;
    loop {
        if cursor == bytes.len() {
            return Some(None);
        }
        let op = bytes[cursor];
        cursor += 1;
        match op {
            0x7c => return Some(Some(FirstI64Arith::Add)),
            0x7d => return Some(Some(FirstI64Arith::Sub)),
            0x7e => return Some(Some(FirstI64Arith::Mul)),
            // Single-byte numeric / comparison / conversion opcodes.
            0x45..=0xc4 => {}
            // Immediate-free control and parametric opcodes.
            0x0b | 0x05 | 0x0f | 0x00 | 0x01 | 0x1a | 0x1b | 0xd1 => {}
            // One index immediate.
            0x20 | 0x21 | 0x22 | 0x23 | 0x24 | 0x0c | 0x0d | 0x10 | 0x12 => {
                skip_uleb(bytes, &mut cursor)?;
            }
            // br_table: count, then count+1 label indices.
            0x0e => {
                let count = skip_uleb(bytes, &mut cursor)?;
                for _ in 0..count.checked_add(1)? {
                    skip_uleb(bytes, &mut cursor)?;
                }
            }
            // call_indirect: type index + table index.
            0x11 => {
                skip_uleb(bytes, &mut cursor)?;
                skip_uleb(bytes, &mut cursor)?;
            }
            // block / loop / if.
            0x02..=0x04 => skip_block_type(bytes, &mut cursor)?,
            // i32.const / i64.const.
            0x41 | 0x42 => skip_sleb(bytes, &mut cursor)?,
            0x43 => {
                if bytes.len() - cursor < 4 {
                    return None;
                }
                cursor += 4;
            }
            0x44 => {
                if bytes.len() - cursor < 8 {
                    return None;
                }
                cursor += 8;
            }
            // ref.null <heaptype s33>.
            0xd0 => skip_sleb(bytes, &mut cursor)?,
            // ref.func <funcidx>.
            0xd2 => {
                skip_uleb(bytes, &mut cursor)?;
            }
            // GC prefix: the decoder's admitted subset only.
            0xfb => {
                let sub = skip_uleb(bytes, &mut cursor)?;
                match sub {
                    0x00 | 0x01 | 0x06 | 0x07 | 0x0b | 0x0c | 0x0d | 0x0e => {
                        skip_uleb(bytes, &mut cursor)?;
                    }
                    0x02 | 0x05 | 0x08 | 0x09 => {
                        skip_uleb(bytes, &mut cursor)?;
                        skip_uleb(bytes, &mut cursor)?;
                    }
                    0x0f => {}
                    0x14..=0x17 => skip_sleb(bytes, &mut cursor)?,
                    _ => return None,
                }
            }
            _ => return None,
        }
    }
}

fn disassemble(wasm_bytes: &[u8]) -> Result<DisasmResult, String> {
    use wasmparser::{
        CompositeInnerType, DataKind, Operator, Parser, Payload, StorageType, ValType,
    };

    // Admission gate: never trust a byte-derived fact from a module that is not
    // well-typed wasm. Full validation runs BEFORE any rederivation reads a
    // section, so a forged result type, a nullability-mismatched signature, or
    // malformed/trailing section bytes are all rejected up front rather than
    // slipping past the relevant-subset slicer. `Validator::new()` enables the
    // GC / tail-call / function-reference proposals the backend emits (all
    // default-on), matching the emitter's own feature set.
    wasmparser::Validator::new()
        .validate_all(wasm_bytes)
        .map_err(|e| format!("wasm module failed validation: {e}"))?;

    let mut num_imported_funcs: u32 = 0;
    // `module.name` of every FUNCTION import, in index order. Purely diagnostic:
    // a call into one of these is a call into the host capability surface, which
    // no certified template admits, and the decline reason names it.
    let mut imported_func_names: Vec<String> = Vec::new();
    // defined-function index -> declared type index
    let mut func_type_idx: Vec<u32> = Vec::new();
    // type index -> byte-level signature (param kinds, FULL result-kind vector)
    // for func types. The complete result vector is retained (not just the first
    // result) so a verbatim route can require EXACTLY one result of the
    // recognized kind — a zero-result or two-result declaration is rejected.
    let mut type_sigs: std::collections::HashMap<u32, (Vec<TyKind>, Vec<TyKind>)> =
        std::collections::HashMap::new();
    // type index -> struct field count
    let mut struct_field_counts: std::collections::HashMap<u32, u32> =
        std::collections::HashMap::new();
    // type indices for the string byte-array carrier `(array (mut i8))`.
    let mut string_byte_array_types: std::collections::HashSet<u32> =
        std::collections::HashSet::new();
    // export name -> func index
    let mut exports: Vec<(String, u32)> = Vec::new();
    let mut code_entries: Vec<CodeEntry> = Vec::new();
    let mut next_code_entry_start: Option<usize> = None;
    let mut data_segments: Vec<Option<Vec<u8>>> = Vec::new();
    let mut carrier: Option<u32> = None;
    // The limb array type index the Int carrier's middle field references. The
    // arith helper bodies read/write this array type, so the acceptance pin
    // needs it declared alongside the carrier.
    let mut limb: Option<u32> = None;
    let mut next_type_idx: u32 = 0;
    // The certificate decoder's whole-module role scan declines any
    // non-function import; record the fact so a module that also carries the
    // Int box helper is refused instead of certified into an unverifiable
    // package.
    let mut has_non_function_import = false;

    for payload in Parser::new(0).parse_all(wasm_bytes) {
        let payload = payload.map_err(|e| format!("wasm parse: {e}"))?;
        match payload {
            Payload::TypeSection(reader) => {
                for rg in reader {
                    let rg = rg.map_err(|e| format!("type read: {e}"))?;
                    for sub in rg.into_types() {
                        let idx = next_type_idx;
                        next_type_idx += 1;
                        match &sub.composite_type.inner {
                            CompositeInnerType::Func(ft) => {
                                let kind = |vt: &ValType| match vt {
                                    ValType::I64 => TyKind::I64,
                                    ValType::I32 => TyKind::I32,
                                    ValType::F64 => TyKind::F64,
                                    ValType::Ref(rt) => match heap_type_index(rt.heap_type()) {
                                        Some(t) => TyKind::Ref {
                                            nullable: rt.is_nullable(),
                                            idx: t,
                                        },
                                        None => match rt.heap_type() {
                                            wasmparser::HeapType::Abstract {
                                                ty: wasmparser::AbstractHeapType::Eq,
                                                ..
                                            } => TyKind::Eqref,
                                            // Kernel parity: exact references are not plain 0x63 s33 refs.
                                            wasmparser::HeapType::Exact(_) => TyKind::Other,
                                            _ => TyKind::Other,
                                        },
                                    },
                                    _ => TyKind::Other,
                                };
                                type_sigs.insert(
                                    idx,
                                    (
                                        ft.params().iter().map(kind).collect(),
                                        ft.results().iter().map(kind).collect(),
                                    ),
                                );
                            }
                            // Int carrier: 3 fields, {i64, ref, i32}.
                            CompositeInnerType::Struct(st)
                                if carrier.is_none()
                                    && st.fields.len() == 3
                                    && matches!(
                                        st.fields[0].element_type,
                                        StorageType::Val(ValType::I64)
                                    )
                                    && matches!(
                                        st.fields[2].element_type,
                                        StorageType::Val(ValType::I32)
                                    ) =>
                            {
                                carrier = Some(idx);
                                struct_field_counts.insert(idx, st.fields.len() as u32);
                                // The middle field is `(ref null $mag)`; capture
                                // the declared limb array type index it targets.
                                if let StorageType::Val(ValType::Ref(rt)) =
                                    st.fields[1].element_type
                                {
                                    limb = heap_type_index(rt.heap_type());
                                }
                            }
                            CompositeInnerType::Struct(st) => {
                                struct_field_counts.insert(idx, st.fields.len() as u32);
                            }
                            CompositeInnerType::Array(at)
                                if matches!(at.0.element_type, StorageType::I8) =>
                            {
                                string_byte_array_types.insert(idx);
                            }
                            _ => {}
                        }
                    }
                }
            }
            Payload::ImportSection(reader) => {
                // Compact import encoding groups imports; iterate each group.
                for group in reader {
                    let group = group.map_err(|e| format!("import read: {e}"))?;
                    for imp in group {
                        let (_, imp) = imp.map_err(|e| format!("import read: {e}"))?;
                        if let wasmparser::TypeRef::Func(_) = imp.ty {
                            num_imported_funcs += 1;
                            imported_func_names.push(format!("{}.{}", imp.module, imp.name));
                        } else {
                            has_non_function_import = true;
                        }
                    }
                }
            }
            Payload::FunctionSection(reader) => {
                for t in reader {
                    func_type_idx.push(t.map_err(|e| format!("func read: {e}"))?);
                }
            }
            Payload::ExportSection(reader) => {
                for ex in reader {
                    let ex = ex.map_err(|e| format!("export read: {e}"))?;
                    if ex.kind == wasmparser::ExternalKind::Func {
                        exports.push((ex.name.to_string(), ex.index));
                    }
                }
            }
            Payload::CodeSectionStart { range, size, .. } => {
                next_code_entry_start = Some(
                    range
                        .end
                        .checked_sub(size as usize)
                        .ok_or_else(|| "code section size is outside its byte range".to_string())?,
                );
            }
            Payload::CodeSectionEntry(body) => {
                let entry_start = next_code_entry_start
                    .ok_or_else(|| "code entry appeared before code section start".to_string())?;
                let entry_end = body.range().end;
                let code_entry_bytes = wasm_bytes
                    .get(entry_start..entry_end)
                    .ok_or_else(|| {
                        format!(
                            "code entry byte range {entry_start}..{entry_end} is outside wasm module"
                        )
                    })?
                    .to_vec();
                next_code_entry_start = Some(entry_end);
                let mut nlocals = 0usize;
                let mut lr = body
                    .get_locals_reader()
                    .map_err(|e| format!("locals reader: {e}"))?;
                for _ in 0..lr.get_count() {
                    let (n, _ty) = lr.read().map_err(|e| format!("locals read: {e}"))?;
                    nlocals += n as usize;
                }
                let mut ops = Vec::new();
                let mut calls = Vec::new();
                let mut has_loop_or_branch = false;
        let mut saw_i64_add = false;
        let mut saw_i64_mul = false;
        let mut saw_i64_sub = false;
                let mut first_i64_arith = None;
                let mut first_arith_strict = None;
                let mut first_unsupported_op: Option<String> = None;
                let mut host_ops = Vec::new();
                let mut opr = body
                    .get_operators_reader()
                    .map_err(|e| format!("ops reader: {e}"))?;
                // Body bytes after the locals vector — the exact slice the
                // certificate decoder's first-arith scan walks.
                let arith_scan_start = opr.original_position();
                let kernel_arith_scan = wasm_bytes
                    .get(arith_scan_start..entry_end)
                    .and_then(kernel_first_arith_scan);
                while !opr.eof() {
                    let op = opr.read().map_err(|e| format!("op read: {e}"))?;
                    host_ops.push(host_op(&op));
                    let mapped = match op {
                        Operator::LocalGet { local_index } => Op::LocalGet(local_index),
                        Operator::LocalSet { local_index } => Op::LocalSet(local_index),
                        Operator::I64Const { value } => Op::I64Const(value),
                        Operator::I32Const { value } => Op::I32Const(value),
                        Operator::F64Const { value } => Op::F64Const(value.bits()),
                        Operator::RefTestNonNull { hty } | Operator::RefTestNullable { hty } => {
                            heap_type_index(hty).map(Op::RefTest).unwrap_or(Op::Other)
                        }
                        Operator::RefCastNonNull { hty } | Operator::RefCastNullable { hty } => {
                            heap_type_index(hty).map(Op::RefCast).unwrap_or(Op::Other)
                        }
                        Operator::StructNew { struct_type_index } => Op::StructNew(
                            struct_type_index,
                            struct_field_counts
                                .get(&struct_type_index)
                                .copied()
                                .unwrap_or(0),
                        ),
                        Operator::StructGet {
                            struct_type_index,
                            field_index,
                        } => Op::StructGet(struct_type_index, field_index),
                        Operator::ArrayNewData {
                            array_type_index,
                            array_data_index,
                        } => {
                            let literal_operands =
                                match (ops.get(ops.len().wrapping_sub(2)), ops.last()) {
                                    (Some(Op::I32Const(0)), Some(Op::I32Const(len))) => Some(*len),
                                    _ => None,
                                };
                            if let Some(len) = literal_operands {
                                Op::ArrayNewDataUnresolved {
                                    type_idx: array_type_index,
                                    data_idx: array_data_index,
                                    offset: 0,
                                    len,
                                }
                            } else {
                                Op::Other
                            }
                        }
                        Operator::RefNull { hty } => Op::RefNull(heap_type_index(hty)),
                        Operator::RefIsNull => Op::RefIsNull,
                        Operator::I64Eq => Op::I64Eq,
                        Operator::I64LeS => Op::I64LeS,
                        Operator::I64LtS => Op::I64LtS,
                        Operator::I64GeS => Op::I64GeS,
                        Operator::I64GtS => Op::I64GtS,
                        Operator::F64Add => Op::F64Add,
                        Operator::F64Mul => Op::F64Mul,
                        Operator::F64Le => Op::F64Le,
                        Operator::F64Ge => Op::F64Ge,
                        Operator::F64Lt => Op::F64Lt,
                        Operator::F64Gt => Op::F64Gt,
                        Operator::F64Eq => Op::F64Eq,
                        Operator::I64Add => {
                            saw_i64_add = true;
                            first_i64_arith.get_or_insert(HostRole::Add);
                            first_arith_strict.get_or_insert(FirstI64Arith::Add);
                            Op::Other
                        }
                        Operator::I64Sub => {
                            saw_i64_sub = true;
                            first_i64_arith.get_or_insert(HostRole::Sub);
                            first_arith_strict.get_or_insert(FirstI64Arith::Sub);
                            Op::Other
                        }
                        Operator::I64Mul => {
                            saw_i64_mul = true;
                            first_i64_arith.get_or_insert(HostRole::Mul);
                            first_arith_strict.get_or_insert(FirstI64Arith::Mul);
                            Op::Other
                        }
                        Operator::I32Eq => Op::I32Eq,
                        Operator::I32LtS => Op::I32LtS,
                        Operator::I32GtS => Op::I32GtS,
                        Operator::I32LtU => Op::I32LtU,
                        Operator::I32GeS => Op::I32GeS,
                        Operator::I32And => Op::I32And,
                        Operator::ArrayLen => Op::ArrayLen,
                        Operator::ArrayGet { array_type_index } => {
                            Op::ArrayGet(array_type_index)
                        }
                        Operator::If { .. } => Op::If,
                        Operator::Else => Op::Else,
                        Operator::End => Op::End,
                        Operator::Call { function_index } => {
                            calls.push(function_index);
                            Op::Call(function_index)
                        }
                        Operator::ReturnCall { function_index } => {
                            calls.push(function_index);
                            Op::ReturnCall(function_index)
                        }
                        Operator::Loop { .. }
                        | Operator::Block { .. }
                        | Operator::Br { .. }
                        | Operator::BrIf { .. }
                        | Operator::BrTable { .. } => {
                            has_loop_or_branch = true;
                            Op::Other
                        }
                        Operator::ArrayNewFixed {
                            array_type_index,
                            array_size,
                        } => Op::ArrayNewFixed(array_type_index, array_size),
                        _ => {
                            // Record what the body actually used, so the
                            // decline reason names the instruction instead of
                            // guessing at a family. Reading `op` here is free:
                            // no arm above moves anything out of it.
                            if first_unsupported_op.is_none() {
                                first_unsupported_op = Some(operator_name(&op));
                            }
                            Op::Other
                        }
                    };
                    ops.push(mapped);
                }
                let host_role = match (saw_i64_add, saw_i64_mul, saw_i64_sub) {
                    (true, false, false) => Some(HostRole::Add),
                    (false, true, false) => Some(HostRole::Mul),
                    (false, false, true) => Some(HostRole::Sub),
                    _ => first_i64_arith,
                };
                code_entries.push(CodeEntry {
                    nlocals,
                    code_entry_bytes,
                    ops,
                    calls,
                    has_loop_or_branch,
                    first_unsupported_op,
                    host_role,
                    first_arith_strict,
                    kernel_arith_scan,
                    host_ops,
                });
            }
            Payload::DataSection(reader) => {
                for data in reader {
                    let data = data.map_err(|e| format!("data read: {e}"))?;
                    match data.kind {
                        DataKind::Passive => data_segments.push(Some(data.data.to_vec())),
                        DataKind::Active { .. } => data_segments.push(None),
                    }
                }
            }
            _ => {}
        }
    }

    // Runtime helper names never certified as code. `__aint_to_index` is the
    // named host-role export the fused vector-read contract binds, exactly
    // like `__rt_aint_from_i64` for box; `__aint_cmp` and `__aint_eq` are the
    // same kind of named export for the two Int comparison host roles.
    let is_runtime = |name: &str| {
        name.starts_with("__rt_")
            || name.starts_with("__caller")
            || name == "__aint_to_index"
            || name == "__aint_cmp"
            || name == "__aint_eq"
            || name == "_start"
            || name == "memory"
    };

    // Int box helper, exact by export name. A module without Int arithmetic
    // legitimately has no such export: keep it `None` so the integer-family
    // recognizers never match (fail-closed decline per export), while the
    // carrier-free classes still get their shot at certification.
    let box_idx = exports
        .iter()
        .find(|(n, _)| n == "__rt_aint_from_i64")
        .map(|(_, i)| *i);

    // `__aint_to_index` helper, exact by export name (mirror of `box`; twin
    // of `CertDecode.AddSub.toIndexIdx`). Absent in modules with no fused
    // vector read; `None` keeps the fused-read recognizer fail-closed.
    let to_index_idx = exports
        .iter()
        .find(|(n, _)| n == "__aint_to_index")
        .map(|(_, i)| *i);

    // The two Int comparison helpers, likewise exact by export name (twins of
    // `CertDecode.AddSub.cmpIdx`/`eqIdx`). They are NOT derived from body shape
    // like add/sub/mul: both declare the same function type and return a raw
    // `i32`, so `is_carrier_binop` rightly excludes them and nothing but the
    // export name separates the three-way helper from the equality one.
    let cmp_idx = exports
        .iter()
        .find(|(n, _)| n == "__aint_cmp")
        .map(|(_, i)| *i);
    let eq_idx = exports
        .iter()
        .find(|(n, _)| n == "__aint_eq")
        .map(|(_, i)| *i);

    // No behavioural (body-shape) scan is added for `cmp`/`eq`, and the
    // carrier-binop signature test below deliberately keeps excluding them:
    // both return a raw `i32` rather than a carrier, so they are not carrier
    // binops, and their two bodies would need a role classifier of their own.
    // Their roles are export-name-derived instead, which is also what the
    // wall's `cmpIdx`/`eqIdx` decoders read. Nothing else is needed for the
    // refusal path either: the two exports only ever appear alongside the Int
    // box helper, so the `box_idx.is_some()` block below already refuses every
    // module whose role table the certificate decoder cannot resolve.
    let is_carrier_binop = |def_idx: usize| -> bool {
        let Some(c) = carrier else {
            return false;
        };
        let Some((params, results)) = func_type_idx.get(def_idx).and_then(|ti| type_sigs.get(ti))
        else {
            return false;
        };
        let is_carrier_ref = |t: &TyKind| matches!(t, TyKind::Ref { idx, .. } if *idx == c);
        matches!(params.as_slice(), [a, b] if is_carrier_ref(a) && is_carrier_ref(b))
            && matches!(results.as_slice(), [r] if is_carrier_ref(r))
    };

    // Producer-side mirror of the verifier's module-wide role decode. A module
    // that carries the Int box helper must let the certificate decoder resolve
    // its whole host-role table; when the decoder's scan would fail, NO
    // manifest value can satisfy the acceptance pin, so emitting a package
    // would only defer the failure to `aver cert verify`. Refuse honestly here
    // instead, naming the reason.
    if box_idx.is_some() {
        if carrier.is_none() {
            return Err(
                "module exports the Int box helper `__rt_aint_from_i64` but declares no Int \
                 carrier struct type; the certificate decoder cannot resolve its host-role \
                 table, so no certificate for this module can verify"
                    .to_string(),
            );
        }
        if has_non_function_import {
            return Err(
                "module exports the Int box helper `__rt_aint_from_i64` and also declares a \
                 non-function import; the certificate decoder declines such modules, so no \
                 certificate for this module can verify"
                    .to_string(),
            );
        }
        for (def_idx, entry) in code_entries.iter().enumerate() {
            if !is_carrier_binop(def_idx) {
                continue;
            }
            let strict = entry.first_arith_strict;
            match entry.kernel_arith_scan {
                Some(first) if first == strict => {}
                Some(_) | None => {
                    return Err(format!(
                        "module exports the Int box helper `__rt_aint_from_i64` but function \
                         index {} has the Int carrier-binop signature and a body the \
                         certificate decoder's role scan cannot classify; the module-wide \
                         host-role table is undecodable, so no certificate for this module \
                         can verify",
                        num_imported_funcs + def_idx as u32,
                    ));
                }
            }
        }
    }

    // user export name -> wasm func index
    let mut user_exports: Vec<(String, u32)> = exports
        .iter()
        .filter(|(n, _)| !is_runtime(n))
        .cloned()
        .collect();
    user_exports.sort_by_key(|(_, i)| *i);

    let user_idx_set: std::collections::HashSet<u32> =
        user_exports.iter().map(|(_, i)| *i).collect();

    let host_roles = code_entries
        .iter()
        .enumerate()
        .filter_map(|(def_idx, entry)| {
            let sig = func_type_idx
                .get(def_idx)
                .and_then(|ti| type_sigs.get(ti))
                .cloned()
                .unwrap_or((Vec::new(), Vec::new()));
            let result0 = sig.1.first().copied();
            let role = if is_string_eq_host(entry, &sig.0, result0, &string_byte_array_types) {
                Some(HostRole::StringEq)
            } else if is_string_concat_host(entry, &sig.0, result0, &string_byte_array_types) {
                Some(HostRole::StringConcat)
            } else {
                entry.host_role
            };
            role.map(|role| (num_imported_funcs + def_idx as u32, role))
        })
        .collect::<std::collections::HashMap<_, _>>();

    // A table entry binds the behavioural role, so a candidate must have the
    // exact carrier-binop signature (`[ref carrier, ref carrier] -> ref carrier`)
    // AND the corresponding i64 operator as the FIRST arithmetic operator in
    // its body. The `mul` helper's umag loops also contain `i64.add`, but its
    // fast path multiplies first. If the module
    // does not determine a UNIQUE candidate, the role stays unbound (`None`)
    // and every plan citing it declines fail-closed — never guess by index
    // order. `box` is the exported `__rt_aint_from_i64`, exact by name
    // (unbound when the module has no such export). `sub`
    // is derived exactly like `add` and `mul`, each with its own uniqueness
    // check. All four roles are surfaced to Lean and bound in the artifact.
    let frag_host_table = {
        let strict_binop_candidates = |arith: FirstI64Arith| -> Vec<u32> {
            code_entries
                .iter()
                .enumerate()
                .filter(|(def_idx, entry)| {
                    entry.first_arith_strict == Some(arith) && is_carrier_binop(*def_idx)
                })
                .map(|(def_idx, _)| num_imported_funcs + def_idx as u32)
                .collect()
        };
        let unique = |candidates: Vec<u32>| -> Option<u32> {
            match candidates.as_slice() {
                [only] => Some(*only),
                _ => None,
            }
        };
        let add_idx = unique(strict_binop_candidates(FirstI64Arith::Add));
        let mul_idx = unique(strict_binop_candidates(FirstI64Arith::Mul));
        let sub_idx = unique(strict_binop_candidates(FirstI64Arith::Sub));
        // The add/sub/mul helpers call the four bignum sub-routines by function
        // index; read those indices out of the add helper's call sites and
        // bucket them by their distinct signatures. The producer only needs the
        // honest indices — the checker template-pins them, so a misread fails
        // closed rather than certifying a wrong body. Signatures:
        //   decompose: 1 param -> 2 results;  normalize: 2 params -> 1 result;
        //   strip:     1 param -> 1 result;    umagCmp:   4 params -> 1 result.
        let mut decompose_idx = None;
        let mut normalize_idx = None;
        let mut strip_idx = None;
        let mut umag_cmp_idx = None;
        let add_entry = add_idx
            .and_then(|add_fn| add_fn.checked_sub(num_imported_funcs))
            .and_then(|add_def| code_entries.get(add_def as usize));
        if let Some(entry) = add_entry {
            let mut seen = std::collections::HashSet::new();
            for &callee in &entry.calls {
                if !seen.insert(callee) {
                    continue;
                }
                let Some(callee_def) = callee.checked_sub(num_imported_funcs) else {
                    continue;
                };
                let Some(type_idx) = func_type_idx.get(callee_def as usize).copied() else {
                    continue;
                };
                let Some((params, results)) = type_sigs.get(&type_idx) else {
                    continue;
                };
                match (params.len(), results.len()) {
                    (1, 2) => decompose_idx = decompose_idx.or(Some(callee)),
                    (2, 1) => normalize_idx = normalize_idx.or(Some(callee)),
                    (1, 1) => strip_idx = strip_idx.or(Some(callee)),
                    (4, _) => umag_cmp_idx = umag_cmp_idx.or(Some(callee)),
                    _ => {}
                }
            }
        }
        FragHostTable {
            box_idx,
            add_idx,
            mul_idx,
            sub_idx,
            to_index_idx,
            cmp_idx,
            eq_idx,
            limb_idx: limb,
            decompose_idx,
            normalize_idx,
            strip_idx,
            umag_cmp_idx,
        }
    };

    // Resolve one declared wasm type against this module's own type section so
    // a decline reason can name the source-level shape. Diagnostic only.
    let value_shape = |ty: &TyKind| match ty {
        TyKind::Ref { idx, .. } if Some(*idx) == carrier => ValShape::Int,
        TyKind::Ref { idx, .. } if string_byte_array_types.contains(idx) => ValShape::Str,
        TyKind::Ref { .. } | TyKind::Eqref => ValShape::UserRef,
        TyKind::I64 | TyKind::I32 | TyKind::F64 => ValShape::Scalar,
        TyKind::Other => ValShape::Raw,
    };

    let mut user_fns = Vec::new();
    for (name, wasm_idx) in user_exports {
        let Some(def_idx) = wasm_idx.checked_sub(num_imported_funcs) else {
            continue;
        };
        let Some(entry) = code_entries.get(def_idx as usize).cloned() else {
            continue;
        };
        let ops = resolve_data_ops(entry.ops, &data_segments);
        let Some(type_idx) = func_type_idx.get(def_idx as usize).copied() else {
            continue;
        };
        let (params, results) = type_sigs
            .get(&type_idx)
            .cloned()
            .unwrap_or((Vec::new(), Vec::new()));
        let result = results.first().copied();
        let param_shapes = params.iter().map(&value_shape).collect();
        let result_shapes = results.iter().map(&value_shape).collect();
        user_fns.push(UserFn {
            name,
            wasm_idx,
            type_idx,
            arity: params.len(),
            params,
            result,
            results,
            nlocals: entry.nlocals,
            code_entry_bytes: entry.code_entry_bytes,
            ops,
            host_capability_calls: entry
                .calls
                .iter()
                .filter_map(|c| imported_func_names.get(*c as usize).cloned())
                .collect(),
            param_shapes,
            result_shapes,
            first_unsupported_op: entry.first_unsupported_op,
            calls: entry.calls,
            has_loop_or_branch: entry.has_loop_or_branch,
        });
    }

    Ok((
        user_fns,
        box_idx,
        user_idx_set,
        carrier,
        host_roles,
        frag_host_table,
        struct_field_counts,
    ))
}

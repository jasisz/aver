type DisasmResult = (
    Vec<UserFn>,
    u32,
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
    let mut next_type_idx: u32 = 0;

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
                let mut host_ops = Vec::new();
                let mut opr = body
                    .get_operators_reader()
                    .map_err(|e| format!("ops reader: {e}"))?;
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
                        Operator::F64Add => Op::F64Add,
                        Operator::F64Mul => Op::F64Mul,
                        Operator::F64Le => Op::F64Le,
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
                        Operator::I32LtS => Op::I32LtS,
                        Operator::I32GtS => Op::I32GtS,
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
                        _ => Op::Other,
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
                    host_role,
                    first_arith_strict,
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

    // Runtime helper names never certified as code.
    let is_runtime = |name: &str| {
        name.starts_with("__rt_")
            || name.starts_with("__caller")
            || name == "_start"
            || name == "memory"
    };

    let box_idx = exports
        .iter()
        .find(|(n, _)| n == "__rt_aint_from_i64")
        .map(|(_, i)| *i)
        .ok_or_else(|| "module has no __rt_aint_from_i64 box helper".to_string())?;

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
    // order. `box` is the exported `__rt_aint_from_i64`, exact by name. `sub`
    // is derived exactly like `add` and `mul`, each with its own uniqueness
    // check. All four roles are surfaced to Lean and bound in the artifact.
    let frag_host_table = {
        let is_carrier_binop = |def_idx: usize| -> bool {
            let Some(c) = carrier else {
                return false;
            };
            let Some((params, results)) = func_type_idx
                .get(def_idx)
                .and_then(|ti| type_sigs.get(ti))
            else {
                return false;
            };
            let is_carrier_ref = |t: &TyKind| matches!(t, TyKind::Ref { idx, .. } if *idx == c);
            matches!(params.as_slice(), [a, b] if is_carrier_ref(a) && is_carrier_ref(b))
                && matches!(results.as_slice(), [r] if is_carrier_ref(r))
        };
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
        FragHostTable {
            box_idx: Some(box_idx),
            add_idx: unique(strict_binop_candidates(FirstI64Arith::Add)),
            mul_idx: unique(strict_binop_candidates(FirstI64Arith::Mul)),
            sub_idx: unique(strict_binop_candidates(FirstI64Arith::Sub)),
        }
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

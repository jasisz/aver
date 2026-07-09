#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StringConcatChunkPlan {
    pub data_idx: u32,
    pub bytes: Vec<u8>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StringConcatPlan {
    pub prefixes: Vec<StringConcatChunkPlan>,
    pub suffixes: Vec<StringConcatChunkPlan>,
}

fn string_concat_plan_from_cert(c: &Cert) -> Option<StringConcatPlan> {
    let Cert::StringConcatVerbatimMatch {
        prefixes, suffixes, ..
    } = c.inner()
    else {
        return None;
    };
    Some(StringConcatPlan {
        prefixes: prefixes
            .iter()
            .map(verbatim_array_chunk)
            .collect::<Option<Vec<_>>>()?,
        suffixes: suffixes
            .iter()
            .map(verbatim_array_chunk)
            .collect::<Option<Vec<_>>>()?,
    })
}

fn verbatim_array_chunk(value: &VerbatimDefault) -> Option<StringConcatChunkPlan> {
    match value {
        VerbatimDefault::Array {
            data_idx, bytes, ..
        } => Some(StringConcatChunkPlan {
            data_idx: *data_idx,
            bytes: bytes.clone(),
        }),
        VerbatimDefault::Null | VerbatimDefault::F64Bits(_) => None,
    }
}

fn string_concat_sym_plan_from_cert(c: &Cert) -> Option<SymPlan> {
    string_concat_plan_from_cert(c).map(|plan| string_concat_sym_plan_from_plan(&plan))
}

fn string_concat_sym_plan_from_plan(plan: &StringConcatPlan) -> SymPlan {
    let mut nodes = Vec::new();
    let mut args = Vec::new();
    for chunk in &plan.prefixes {
        args.push(push_sym_string_node(
            &mut nodes,
            SymNodeKind::ConstStringBytes(chunk.bytes.clone()),
        ));
    }
    args.push(push_sym_string_node(
        &mut nodes,
        SymNodeKind::Param { index: 0 },
    ));
    for chunk in &plan.suffixes {
        args.push(push_sym_string_node(
            &mut nodes,
            SymNodeKind::ConstStringBytes(chunk.bytes.clone()),
        ));
    }
    let result = push_sym_string_node(
        &mut nodes,
        SymNodeKind::Prim {
            op: SymPrim::StringConcat,
            args,
        },
    );
    SymPlan {
        params: vec![SymTy::String],
        result: SymTy::String,
        body: SymBlock { nodes, result },
    }
}

fn push_sym_string_node(nodes: &mut Vec<SymNode>, kind: SymNodeKind) -> SymValueId {
    let id = SymValueId(nodes.len());
    nodes.push(SymNode {
        id,
        ty: SymTy::String,
        kind,
    });
    id
}

fn string_concat_sidecar(name: &str, plan: &StringConcatPlan) -> FragmentPlanSidecar {
    let text = string_concat_plan_text(plan);
    FragmentPlanSidecar {
        path: string_concat_plan_path(name),
        sha256: sha256_hex(text.as_bytes()),
        text,
    }
}

fn string_concat_plan_path(name: &str) -> String {
    format!("fragments/{}.string-concat-v1.plan", hex(name.as_bytes()))
}

fn string_concat_plan_text(plan: &StringConcatPlan) -> String {
    let mut out = String::new();
    out.push_str("aver.string-fragment.plan.v1\n");
    out.push_str("profile string-concat-v1\n");
    out.push_str("params string\n");
    out.push_str("result string\n");
    out.push_str("concat\n");
    for chunk in &plan.prefixes {
        out.push_str("  prefix data=");
        out.push_str(&chunk.data_idx.to_string());
        out.push_str(" hex=");
        out.push_str(&hex(&chunk.bytes));
        out.push('\n');
    }
    out.push_str("  input index=0\n");
    for chunk in &plan.suffixes {
        out.push_str("  suffix data=");
        out.push_str(&chunk.data_idx.to_string());
        out.push_str(" hex=");
        out.push_str(&hex(&chunk.bytes));
        out.push('\n');
    }
    out.push_str("end\n");
    out
}

fn string_concat_plan_lean_value(plan: &StringConcatPlan) -> String {
    fn chunks(chunks: &[StringConcatChunkPlan]) -> String {
        chunks
            .iter()
            .map(|chunk| {
                format!(
                    "({{ dataIdx := {}, bytes := {} }} : StringConcatChunk)",
                    chunk.data_idx,
                    render_byte_list(&chunk.bytes)
                )
            })
            .collect::<Vec<_>>()
            .join(", ")
    }

    format!(
        "({{ profile := \"string-concat-v1\", prefixes := [{}], suffixes := [{}] }} : StringConcatRawPlan)",
        chunks(&plan.prefixes),
        chunks(&plan.suffixes)
    )
}

pub fn parse_string_concat_plan(text: &str) -> Result<StringConcatPlan, String> {
    let mut lines = text.lines();
    expect_plan_line(&mut lines, "aver.string-fragment.plan.v1")?;
    expect_plan_line(&mut lines, "profile string-concat-v1")?;
    expect_plan_line(&mut lines, "params string")?;
    expect_plan_line(&mut lines, "result string")?;
    expect_plan_line(&mut lines, "concat")?;

    let mut prefixes = Vec::new();
    let mut suffixes = Vec::new();
    let mut seen_input = false;
    let mut seen_end = false;
    for raw in lines.by_ref() {
        let line = raw.trim();
        if line == "end" {
            seen_end = true;
            break;
        }
        if line == "input index=0" {
            if seen_input {
                return Err("string-concat plan contains more than one input marker".to_string());
            }
            seen_input = true;
            continue;
        }
        if let Some(rest) = line.strip_prefix("prefix data=") {
            if seen_input {
                return Err("string-concat plan has a prefix after the input marker".to_string());
            }
            prefixes.push(parse_plan_chunk(rest)?);
            continue;
        }
        if let Some(rest) = line.strip_prefix("suffix data=") {
            if !seen_input {
                return Err("string-concat plan has a suffix before the input marker".to_string());
            }
            suffixes.push(parse_plan_chunk(rest)?);
            continue;
        }
        return Err(format!("unexpected string-concat plan line `{line}`"));
    }
    if !seen_end {
        return Err("string-concat plan is missing `end`".to_string());
    }
    if !seen_input {
        return Err("string-concat plan is missing `input index=0`".to_string());
    }
    if lines.any(|line| !line.trim().is_empty()) {
        return Err("string-concat plan has trailing content after `end`".to_string());
    }
    Ok(StringConcatPlan { prefixes, suffixes })
}

fn lower_string_concat_plan(
    plan: &StringConcatPlan,
    result_ty: u32,
    container_ty: u32,
    concat_func_idx: u32,
) -> Result<Vec<Op>, String> {
    let mut ops = Vec::new();
    for chunk in &plan.prefixes {
        push_string_concat_chunk_ops(&mut ops, result_ty, chunk)?;
    }
    ops.push(Op::LocalGet(0));
    for chunk in &plan.suffixes {
        push_string_concat_chunk_ops(&mut ops, result_ty, chunk)?;
    }
    let part_count = plan.prefixes.len() + 1 + plan.suffixes.len();
    let part_count = u32::try_from(part_count)
        .map_err(|_| "string-concat plan has too many parts".to_string())?;
    ops.push(Op::ArrayNewFixed(container_ty, part_count));
    ops.push(Op::Call(concat_func_idx));
    Ok(ops)
}

fn lower_string_concat_plan_code_entry_bytes(
    plan: &StringConcatPlan,
    carrier: u32,
    result_ty: u32,
    container_ty: u32,
    concat_func_idx: u32,
) -> Result<Vec<u8>, String> {
    let body = lower_string_concat_plan_body_bytes(
        plan,
        carrier,
        result_ty,
        container_ty,
        concat_func_idx,
    )?;
    let body_len = u32::try_from(body.len())
        .map_err(|_| "string-concat body is too large to encode".to_string())?;
    let mut out = Vec::new();
    push_u32_leb(&mut out, body_len);
    out.extend(body);
    Ok(out)
}

fn lower_string_concat_plan_body_bytes(
    plan: &StringConcatPlan,
    carrier: u32,
    result_ty: u32,
    container_ty: u32,
    concat_func_idx: u32,
) -> Result<Vec<u8>, String> {
    let mut out = Vec::new();
    push_u32_leb(&mut out, 1);
    push_u32_leb(&mut out, 1);
    out.push(0x63);
    push_u32_leb(&mut out, carrier);
    for chunk in &plan.prefixes {
        push_string_concat_chunk_bytes(&mut out, result_ty, chunk)?;
    }
    out.push(0x20);
    push_u32_leb(&mut out, 0);
    for chunk in &plan.suffixes {
        push_string_concat_chunk_bytes(&mut out, result_ty, chunk)?;
    }
    let part_count = plan.prefixes.len() + 1 + plan.suffixes.len();
    let part_count = u32::try_from(part_count)
        .map_err(|_| "string-concat plan has too many parts".to_string())?;
    out.push(0xfb);
    push_u32_leb(&mut out, 0x08);
    push_u32_leb(&mut out, container_ty);
    push_u32_leb(&mut out, part_count);
    out.push(0x10);
    push_u32_leb(&mut out, concat_func_idx);
    out.push(0x0b);
    Ok(out)
}

fn push_string_concat_chunk_ops(
    ops: &mut Vec<Op>,
    result_ty: u32,
    chunk: &StringConcatChunkPlan,
) -> Result<(), String> {
    let len = i32::try_from(chunk.bytes.len())
        .map_err(|_| "string-concat literal chunk is too large".to_string())?;
    ops.push(Op::I32Const(0));
    ops.push(Op::I32Const(len));
    ops.push(Op::ArrayNewData {
        type_idx: result_ty,
        data_idx: chunk.data_idx,
        bytes: chunk.bytes.clone(),
    });
    Ok(())
}

fn push_string_concat_chunk_bytes(
    out: &mut Vec<u8>,
    result_ty: u32,
    chunk: &StringConcatChunkPlan,
) -> Result<(), String> {
    let len = i32::try_from(chunk.bytes.len())
        .map_err(|_| "string-concat literal chunk is too large".to_string())?;
    out.push(0x41);
    push_i32_leb(out, 0);
    out.push(0x41);
    push_i32_leb(out, len);
    out.push(0xfb);
    push_u32_leb(out, 0x09);
    push_u32_leb(out, result_ty);
    push_u32_leb(out, chunk.data_idx);
    Ok(())
}

fn expect_plan_line<'a>(
    lines: &mut std::str::Lines<'a>,
    expected: &str,
) -> Result<(), String> {
    let actual = lines
        .next()
        .ok_or_else(|| format!("expected string-concat plan line `{expected}`"))?
        .trim();
    if actual == expected {
        Ok(())
    } else {
        Err(format!(
            "expected string-concat plan line `{expected}`, got `{actual}`"
        ))
    }
}

fn parse_hex_bytes(raw: &str) -> Result<Vec<u8>, String> {
    if raw.len() % 2 != 0 {
        return Err(format!("hex byte string has odd length: `{raw}`"));
    }
    let mut bytes = Vec::with_capacity(raw.len() / 2);
    let mut chars = raw.as_bytes().chunks_exact(2);
    for pair in &mut chars {
        let hi = hex_nibble(pair[0])
            .ok_or_else(|| format!("hex byte string contains non-hex digit: `{raw}`"))?;
        let lo = hex_nibble(pair[1])
            .ok_or_else(|| format!("hex byte string contains non-hex digit: `{raw}`"))?;
        bytes.push((hi << 4) | lo);
    }
    Ok(bytes)
}

fn parse_plan_chunk(raw: &str) -> Result<StringConcatChunkPlan, String> {
    let Some((data_idx, hex_bytes)) = raw.split_once(" hex=") else {
        return Err(format!(
            "string-concat chunk must use `data=<idx> hex=<bytes>`, got `{raw}`"
        ));
    };
    let data_idx = data_idx
        .parse::<u32>()
        .map_err(|_| format!("string-concat chunk has invalid data index `{data_idx}`"))?;
    Ok(StringConcatChunkPlan {
        data_idx,
        bytes: parse_hex_bytes(hex_bytes)?,
    })
}

fn hex_nibble(b: u8) -> Option<u8> {
    match b {
        b'0'..=b'9' => Some(b - b'0'),
        b'a'..=b'f' => Some(b - b'a' + 10),
        _ => None,
    }
}

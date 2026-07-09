#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StringConcatPlan {
    pub prefixes: Vec<Vec<u8>>,
    pub suffixes: Vec<Vec<u8>>,
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
            .map(verbatim_array_bytes)
            .collect::<Option<Vec<_>>>()?,
        suffixes: suffixes
            .iter()
            .map(verbatim_array_bytes)
            .collect::<Option<Vec<_>>>()?,
    })
}

fn verbatim_array_bytes(value: &VerbatimDefault) -> Option<Vec<u8>> {
    match value {
        VerbatimDefault::Array { bytes, .. } => Some(bytes.clone()),
        VerbatimDefault::Null | VerbatimDefault::F64Bits(_) => None,
    }
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
    for bytes in &plan.prefixes {
        out.push_str("  prefix hex=");
        out.push_str(&hex(bytes));
        out.push('\n');
    }
    out.push_str("  input index=0\n");
    for bytes in &plan.suffixes {
        out.push_str("  suffix hex=");
        out.push_str(&hex(bytes));
        out.push('\n');
    }
    out.push_str("end\n");
    out
}

fn string_concat_plan_lean_value(plan: &StringConcatPlan) -> String {
    fn chunks(chunks: &[Vec<u8>]) -> String {
        chunks
            .iter()
            .map(|bytes| render_byte_list(bytes))
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
        if let Some(hex_bytes) = line.strip_prefix("prefix hex=") {
            if seen_input {
                return Err("string-concat plan has a prefix after the input marker".to_string());
            }
            prefixes.push(parse_hex_bytes(hex_bytes)?);
            continue;
        }
        if let Some(hex_bytes) = line.strip_prefix("suffix hex=") {
            if !seen_input {
                return Err("string-concat plan has a suffix before the input marker".to_string());
            }
            suffixes.push(parse_hex_bytes(hex_bytes)?);
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

fn hex_nibble(b: u8) -> Option<u8> {
    match b {
        b'0'..=b'9' => Some(b - b'0'),
        b'a'..=b'f' => Some(b - b'a' + 10),
        _ => None,
    }
}

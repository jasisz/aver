#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ConstructFieldPlan {
    Local(u32),
    Null,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConstructPlan {
    pub arity: usize,
    pub fields: Vec<ConstructFieldPlan>,
}

fn construct_plan_from_cert(c: &Cert) -> Option<ConstructPlan> {
    let Cert::AdtConstructor {
        arity,
        nlocals,
        struct_idx,
        field_count,
        fields,
        ops,
        ..
    } = c.inner()
    else {
        return None;
    };
    let fields = fields
        .iter()
        .map(|field| match field {
            ConstructorField::Local(index) => Some(ConstructFieldPlan::Local(*index)),
            ConstructorField::Null => Some(ConstructFieldPlan::Null),
        })
        .collect::<Option<Vec<_>>>()?;
    let plan = ConstructPlan {
        arity: *arity,
        fields,
    };
    if *nlocals != construct_plan_nlocals(&plan)
        || *field_count as usize != plan.fields.len()
        || check_construct_plan(&plan).is_err()
        || lower_construct_plan(&plan, *struct_idx).ok().as_ref() != Some(ops)
    {
        return None;
    }
    Some(plan)
}

fn construct_plan_nlocals(_plan: &ConstructPlan) -> usize {
    // `lower_construct_plan_body_bytes` declares one nullable carrier scratch
    // local, independently of constructor arity/field count.
    1
}

fn construct_plan_path(name: &str) -> String {
    format!("fragments/{}.construct-v1.plan", hex(name.as_bytes()))
}

fn construct_sidecar(name: &str, plan: &ConstructPlan) -> FragmentPlanSidecar {
    let text = construct_plan_text(plan);
    FragmentPlanSidecar {
        path: construct_plan_path(name),
        sha256: sha256_hex(text.as_bytes()),
        text,
    }
}

fn construct_plan_text(plan: &ConstructPlan) -> String {
    let mut out = String::new();
    out.push_str("aver.construct-fragment.plan.v1\n");
    out.push_str("profile construct-v1\n");
    out.push_str(&format!("arity {}\n", plan.arity));
    out.push_str("fields\n");
    for field in &plan.fields {
        match field {
            ConstructFieldPlan::Local(index) => {
                out.push_str(&format!("  local index={index}\n"));
            }
            ConstructFieldPlan::Null => out.push_str("  null\n"),
        }
    }
    out.push_str("end\n");
    out
}

fn construct_plan_lean_value(plan: &ConstructPlan) -> String {
    format!(
        "({{ profile := \"construct-v1\", arity := {}, fields := [{}] }} : ConstructRawPlan)",
        plan.arity,
        plan.fields
            .iter()
            .map(construct_field_lean_value)
            .collect::<Vec<_>>()
            .join(", ")
    )
}

fn construct_field_lean_value(field: &ConstructFieldPlan) -> String {
    match field {
        ConstructFieldPlan::Local(index) => format!(".local {index}"),
        ConstructFieldPlan::Null => ".null".to_string(),
    }
}

pub fn parse_construct_plan(text: &str) -> Result<ConstructPlan, String> {
    let mut lines = text.lines();
    expect_construct_plan_line(&mut lines, "aver.construct-fragment.plan.v1")?;
    expect_construct_plan_line(&mut lines, "profile construct-v1")?;
    let arity = parse_construct_nat_line(&mut lines, "arity")? as usize;
    expect_construct_plan_line(&mut lines, "fields")?;
    let mut fields = Vec::new();
    let mut seen_end = false;
    for raw in lines.by_ref() {
        let line = raw.trim();
        if line == "end" {
            seen_end = true;
            break;
        }
        if line == "null" {
            fields.push(ConstructFieldPlan::Null);
            continue;
        }
        if let Some(index) = line.strip_prefix("local index=") {
            let index = index
                .parse::<u32>()
                .map_err(|_| format!("construct plan has invalid local index `{index}`"))?;
            fields.push(ConstructFieldPlan::Local(index));
            continue;
        }
        return Err(format!("unexpected construct plan line `{line}`"));
    }
    if !seen_end {
        return Err("construct plan is missing `end`".to_string());
    }
    if lines.any(|line| !line.trim().is_empty()) {
        return Err("construct plan has trailing content after `end`".to_string());
    }
    Ok(ConstructPlan {
        arity,
        fields,
    })
}

fn lower_construct_plan(plan: &ConstructPlan, struct_idx: u32) -> Result<Vec<Op>, String> {
    check_construct_plan(plan)?;
    let mut ops = Vec::new();
    for field in &plan.fields {
        match field {
            ConstructFieldPlan::Local(index) => ops.push(Op::LocalGet(*index)),
            ConstructFieldPlan::Null => ops.push(Op::RefNull(Some(struct_idx))),
        }
    }
    let field_count = u32::try_from(plan.fields.len())
        .map_err(|_| "construct plan has too many fields".to_string())?;
    ops.push(Op::StructNew(struct_idx, field_count));
    Ok(ops)
}

fn lower_construct_plan_code_entry_bytes(
    plan: &ConstructPlan,
    carrier: u32,
    struct_idx: u32,
) -> Result<Vec<u8>, String> {
    let body = lower_construct_plan_body_bytes(plan, carrier, struct_idx)?;
    let body_len = u32::try_from(body.len())
        .map_err(|_| "construct body is too large to encode".to_string())?;
    let mut out = Vec::new();
    push_u32_leb(&mut out, body_len);
    out.extend(body);
    Ok(out)
}

fn lower_construct_plan_body_bytes(
    plan: &ConstructPlan,
    carrier: u32,
    struct_idx: u32,
) -> Result<Vec<u8>, String> {
    check_construct_plan(plan)?;
    let mut out = Vec::new();
    push_u32_leb(&mut out, 1);
    push_u32_leb(&mut out, 1);
    out.push(0x63);
    push_s33_heap_idx(&mut out, carrier);
    for field in &plan.fields {
        match field {
            ConstructFieldPlan::Local(index) => {
                out.push(0x20);
                push_u32_leb(&mut out, *index);
            }
            ConstructFieldPlan::Null => {
                out.push(0xd0);
                push_s33_heap_idx(&mut out, struct_idx);
            }
        }
    }
    out.push(0xfb);
    push_u32_leb(&mut out, 0x00);
    push_u32_leb(&mut out, struct_idx);
    out.push(0x0b);
    Ok(out)
}

fn check_construct_plan(plan: &ConstructPlan) -> Result<(), String> {
    if plan.arity == 0 {
        return Err("construct-v1 requires at least one source argument".to_string());
    }
    if plan.fields.is_empty() {
        return Err("construct-v1 requires at least one target field".to_string());
    }
    let mut locals = Vec::new();
    for field in &plan.fields {
        match field {
            ConstructFieldPlan::Local(index) => {
                let index_usize = *index as usize;
                if index_usize >= plan.arity {
                    return Err(format!(
                        "construct field local {index} is outside arity {}",
                        plan.arity
                    ));
                }
                locals.push(index_usize);
            }
            ConstructFieldPlan::Null => {}
        }
    }
    locals.sort_unstable();
    locals.dedup();
    let expected = (0..plan.arity).collect::<Vec<_>>();
    if locals != expected {
        return Err(format!(
            "construct fields must use every source argument exactly once, got {locals:?}"
        ));
    }
    Ok(())
}

fn expect_construct_plan_line<'a>(
    lines: &mut std::str::Lines<'a>,
    expected: &str,
) -> Result<(), String> {
    let actual = lines
        .next()
        .ok_or_else(|| format!("expected construct plan line `{expected}`"))?
        .trim();
    if actual == expected {
        Ok(())
    } else {
        Err(format!(
            "expected construct plan line `{expected}`, got `{actual}`"
        ))
    }
}

fn parse_construct_nat_line<'a>(
    lines: &mut std::str::Lines<'a>,
    label: &str,
) -> Result<u32, String> {
    let raw = lines
        .next()
        .ok_or_else(|| format!("expected construct plan `{label}` line"))?
        .trim();
    let value = raw
        .strip_prefix(label)
        .and_then(|rest| rest.strip_prefix(' '))
        .ok_or_else(|| format!("expected construct plan `{label}` line, got `{raw}`"))?;
    value
        .parse::<u32>()
        .map_err(|_| format!("construct plan `{label}` is not a u32: `{value}`"))
}

#[cfg(test)]
mod construct_plan_tests {
    use super::*;

    #[test]
    fn construct_plan_roundtrips_and_lowers() {
        let plan = ConstructPlan {
            arity: 1,
            fields: vec![ConstructFieldPlan::Local(0)],
        };
        let text = construct_plan_text(&plan);
        assert_eq!(parse_construct_plan(&text).unwrap(), plan);
        assert_eq!(
            lower_construct_plan(&plan, 7).unwrap(),
            vec![Op::LocalGet(0), Op::StructNew(7, 1)]
        );
        assert_eq!(
            lower_construct_plan_code_entry_bytes(&plan, 18, 7).unwrap(),
            vec![10, 1, 1, 99, 18, 32, 0, 251, 0, 7, 11]
        );
    }

    #[test]
    fn list_singleton_null_tail_uses_byte_derived_struct_index() {
        let plan = ConstructPlan {
            arity: 1,
            fields: vec![ConstructFieldPlan::Local(0), ConstructFieldPlan::Null],
        };
        assert_eq!(
            lower_construct_plan(&plan, 25).unwrap(),
            vec![
                Op::LocalGet(0),
                Op::RefNull(Some(25)),
                Op::StructNew(25, 2),
            ]
        );
        assert!(!construct_plan_text(&plan).contains("struct "));
    }
}

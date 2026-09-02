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

fn construct_val_type_lean_value(ty: TyKind) -> Option<String> {
    match ty {
        TyKind::I32 => Some(".i32".to_string()),
        TyKind::I64 => Some(".i64".to_string()),
        TyKind::F64 => Some(".f64".to_string()),
        TyKind::Eqref => Some(".eqref".to_string()),
        TyKind::Ref {
            nullable: true,
            idx,
        } => Some(format!(".nullableRef {idx}")),
        TyKind::Ref {
            nullable: false, ..
        }
        | TyKind::Other => None,
    }
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

#[cfg(test)]
mod construct_plan_tests {
    use super::*;

    #[test]
    fn construct_plan_lowers_to_its_ops_and_code_entry() {
        let plan = ConstructPlan {
            arity: 1,
            fields: vec![ConstructFieldPlan::Local(0)],
        };
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
    }
}

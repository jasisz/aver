// Byte-first `field-projection-v1` plan builder for tuple-destructuring
// projections. The plan names only the selected field. Every representation
// fact used by lowering is recovered from validated module bytes and carried by
// the claim: struct index/count, selected result-reference shape, Int carrier,
// function binding, and exact code entry.

#[derive(Clone, Copy, PartialEq, Eq)]
enum FieldProjectionResultTy {
    Eqref,
    NullableRef(u32),
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct FieldProjectionRawPlan {
    field_idx: u32,
}

fn field_projection_result_ty(ty: TyKind) -> Option<FieldProjectionResultTy> {
    match ty {
        TyKind::Eqref => Some(FieldProjectionResultTy::Eqref),
        TyKind::Ref {
            nullable: true,
            idx,
        } => Some(FieldProjectionResultTy::NullableRef(idx)),
        _ => None,
    }
}

fn field_projection_plan_from_cert(
    c: &Cert,
) -> Option<(FieldProjectionRawPlan, FieldProjectionResultTy)> {
    let Cert::FieldProjection {
        nlocals,
        carrier,
        struct_idx,
        field_count,
        field_idx,
        result_ty,
        code_entry_bytes,
        ops,
        ..
    } = c.inner()
    else {
        return None;
    };
    let result_ty = field_projection_result_ty(*result_ty)?;
    let plan = FieldProjectionRawPlan {
        field_idx: *field_idx,
    };
    if *field_count != 2 || *field_idx >= *field_count || *nlocals != 3 {
        return None;
    }
    if lower_field_projection_ops(&plan, *struct_idx) != *ops {
        return None;
    }
    if lower_field_projection_code_entry(&plan, *carrier, *struct_idx, result_ty)
        != *code_entry_bytes
    {
        return None;
    }
    Some((plan, result_ty))
}

fn lower_field_projection_ops(plan: &FieldProjectionRawPlan, struct_idx: u32) -> Vec<Op> {
    vec![
        Op::LocalGet(0),
        Op::LocalSet(2),
        Op::LocalGet(2),
        Op::RefCast(struct_idx),
        Op::StructGet(struct_idx, plan.field_idx),
        Op::LocalSet(1),
        Op::LocalGet(1),
    ]
}

fn push_field_projection_ref_ty(out: &mut Vec<u8>, ty: FieldProjectionResultTy) {
    match ty {
        FieldProjectionResultTy::Eqref => out.push(0x6d),
        FieldProjectionResultTy::NullableRef(idx) => {
            out.push(0x63);
            push_s33_heap_idx(out, idx);
        }
    }
}

fn lower_field_projection_code_entry(
    plan: &FieldProjectionRawPlan,
    carrier: u32,
    struct_idx: u32,
    result_ty: FieldProjectionResultTy,
) -> Vec<u8> {
    let mut body = vec![0x03, 0x01];
    push_field_projection_ref_ty(&mut body, result_ty);
    body.extend_from_slice(&[0x01, 0x6d, 0x01, 0x63]);
    push_s33_heap_idx(&mut body, carrier);
    body.extend_from_slice(&[0x20, 0x00, 0x21, 0x02, 0x20, 0x02, 0xfb, 0x16]);
    push_s33_heap_idx(&mut body, struct_idx);
    body.extend_from_slice(&[0xfb, 0x02]);
    push_u32_leb(&mut body, struct_idx);
    push_u32_leb(&mut body, plan.field_idx);
    body.extend_from_slice(&[0x21, 0x01, 0x20, 0x01, 0x0b]);
    let mut out = Vec::new();
    push_u32_leb(&mut out, body.len() as u32);
    out.extend_from_slice(&body);
    out
}

fn field_projection_plan_lean_value(plan: &FieldProjectionRawPlan) -> String {
    format!(
        "({{ profile := \"field-projection-v1\", fieldIdx := {} }} : FieldProjectionRawPlan)",
        plan.field_idx
    )
}

fn field_projection_result_ty_lean_value(ty: FieldProjectionResultTy) -> String {
    match ty {
        FieldProjectionResultTy::Eqref => "(.eqref)".to_string(),
        FieldProjectionResultTy::NullableRef(idx) => format!("(.nullableRef {idx})"),
    }
}

#[cfg(test)]
mod field_projection_plan_tests {
    use super::*;

    #[test]
    fn canonical_projection_has_three_locals_and_exact_body() {
        let plan = FieldProjectionRawPlan { field_idx: 1 };
        let bytes = lower_field_projection_code_entry(
            &plan,
            2,
            3,
            FieldProjectionResultTy::NullableRef(2),
        );
        assert_eq!(bytes[1], 3, "three local declaration groups");
        assert_eq!(lower_field_projection_ops(&plan, 3).len(), 7);
    }
}

// Recognised proof faces of expression-fragment plans. These are pure plan
// pattern-matchers (no byte analysis), shared between the producer's MIR
// adapter — which gates plan emission on a face existing — and the engine's
// classifier, so they live in the `plans` layer.

/// The straight-line integer face of a host-call expr fragment: exactly
/// `add(param0, box(k))` over one Int parameter. This is the only host-call
/// fragment shape with a rendered proof face today; any other host-call plan
/// fail-closes classification.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FragIntAddFace {
    pub k: i64,
    pub box_idx: u32,
    pub add_idx: u32,
}

pub fn expr_fragment_int_add_face(plan: &ExprFragmentPlan) -> Option<FragIntAddFace> {
    if plan.params.as_slice() != [FragTy::IntCarrier] || plan.result != FragTy::IntCarrier {
        return None;
    }
    let [n0, n1, n2, n3] = plan.body.nodes.as_slice() else {
        return None;
    };
    if plan.body.result != FragValueId(3) {
        return None;
    }
    let (FragNodeKind::Local { index: 0 }, FragNodeKind::ConstI64(k)) = (&n0.kind, &n1.kind) else {
        return None;
    };
    let FragNodeKind::HostCall {
        role: FragHostRole::Box,
        func_idx: box_idx,
        args: box_args,
    } = &n2.kind
    else {
        return None;
    };
    let FragNodeKind::HostCall {
        role: FragHostRole::Add,
        func_idx: add_idx,
        args: add_args,
    } = &n3.kind
    else {
        return None;
    };
    if box_args.as_slice() != [FragValueId(1)]
        || add_args.as_slice() != [FragValueId(0), FragValueId(2)]
    {
        return None;
    }
    if n0.ty != FragTy::IntCarrier
        || n1.ty != FragTy::I64
        || n2.ty != FragTy::IntCarrier
        || n3.ty != FragTy::IntCarrier
    {
        return None;
    }
    Some(FragIntAddFace {
        k: *k,
        box_idx: *box_idx,
        add_idx: *add_idx,
    })
}

/// The verbatim field-projection face of an ADT-ref expr fragment: exactly
/// `struct.get ty field∈{0,1}` of the single reference parameter, returned
/// unchanged. This is the only fragment shape admitting `AdtRef` values today;
/// any other ADT-ref plan fail-closes on producer and verifier alike.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FragProjectFace {
    pub struct_idx: u32,
    pub field_idx: u32,
}

pub fn expr_fragment_project_face(plan: &ExprFragmentPlan) -> Option<FragProjectFace> {
    if plan.params.as_slice() != [FragTy::AdtRef] || plan.result != FragTy::AdtRef {
        return None;
    }
    let [n0, n1] = plan.body.nodes.as_slice() else {
        return None;
    };
    if plan.body.result != FragValueId(1) {
        return None;
    }
    let FragNodeKind::Local { index: 0 } = n0.kind else {
        return None;
    };
    let FragNodeKind::StructGetUser {
        ty_idx,
        field,
        value,
    } = n1.kind
    else {
        return None;
    };
    if value != FragValueId(0) || field > 1 {
        return None;
    }
    if n0.ty != FragTy::AdtRef || n1.ty != FragTy::AdtRef {
        return None;
    }
    Some(FragProjectFace {
        struct_idx: ty_idx,
        field_idx: field,
    })
}

fn frag_block_touches_adt_ref(block: &FragBlock) -> bool {
    block.nodes.iter().any(|node| {
        node.ty == FragTy::AdtRef
            || match &node.kind {
                FragNodeKind::StructGetUser { .. } => true,
                FragNodeKind::If {
                    then_block,
                    else_block,
                    ..
                } => {
                    frag_block_touches_adt_ref(then_block) || frag_block_touches_adt_ref(else_block)
                }
                _ => false,
            }
    })
}

/// Whether a plan involves opaque user-ADT references anywhere (params, result
/// or body). Such plans are admitted ONLY through the field-projection face.
pub fn expr_fragment_plan_touches_adt_ref(plan: &ExprFragmentPlan) -> bool {
    plan.params.contains(&FragTy::AdtRef)
        || plan.result == FragTy::AdtRef
        || frag_block_touches_adt_ref(&plan.body)
}

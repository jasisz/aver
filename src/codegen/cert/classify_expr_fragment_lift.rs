fn expr_fragment_ty_from_wasm_param(ty: &TyKind, carrier: u32) -> Option<FragTy> {
    match ty {
        TyKind::F64 => Some(FragTy::F64),
        TyKind::I32 => Some(FragTy::BoolI32),
        TyKind::Ref(idx) if *idx == carrier => Some(FragTy::IntCarrier),
        _ => None,
    }
}

fn expr_fragment_ty_from_wasm_result(ty: TyKind, carrier: u32) -> Option<FragTy> {
    match ty {
        TyKind::F64 => Some(FragTy::F64),
        TyKind::I32 => Some(FragTy::BoolI32),
        TyKind::Ref(idx) if idx == carrier => Some(FragTy::IntCarrier),
        _ => None,
    }
}

/// The byte-derived host-role table for one disassembled module: `box` is the
/// exported `__rt_aint_from_i64`, `add` is the body-shape carrier-add role
/// (deterministically the smallest such index). Plans never contribute
/// indices; they must cite exactly these.
fn frag_host_table_from_disasm(
    box_idx: u32,
    host_roles: &std::collections::HashMap<u32, HostRole>,
) -> FragHostTable {
    FragHostTable {
        box_idx: Some(box_idx),
        add_idx: host_roles
            .iter()
            .filter(|(_, role)| **role == HostRole::Add)
            .map(|(idx, _)| *idx)
            .min(),
    }
}

/// The Lean `List (HostRole × Nat)` literal of the byte-derived host-role
/// table for a whole module. `aver cert verify` splices this into its kernel
/// witness (and the emitter into `Plans.lean`/`Artifact.lean`), so source-plan
/// encoding always runs against byte-derived indices, never plan-supplied
/// ones.
pub fn byte_derived_frag_host_table_lean(wasm_bytes: &[u8]) -> Result<String, String> {
    let (_user_fns, box_idx, _user_idx_set, _carrier, host_roles) = disassemble(wasm_bytes)?;
    Ok(frag_host_table_from_disasm(box_idx, &host_roles).lean_value())
}

/// Every `call` in a candidate expr-fragment body must resolve through the
/// byte-derived host-role table; any other callee fail-closes the sidecar
/// gate (recursion, user calls, unknown helpers).
fn frag_calls_resolvable(calls: &[u32], table: &FragHostTable) -> bool {
    calls
        .iter()
        .all(|idx| Some(*idx) == table.box_idx || Some(*idx) == table.add_idx)
}

/// Fail-closed validation that every `hostCall` node in a checked plan cites
/// exactly the byte-derived index for its role.
fn check_plan_host_calls(block: &FragBlock, table: &FragHostTable) -> Result<(), String> {
    for node in &block.nodes {
        match &node.kind {
            FragNodeKind::HostCall { role, func_idx, .. }
                if table.lookup(*role) != Some(*func_idx) =>
            {
                return Err(format!(
                    "plan hostcall v{} cites function {} for role `{}`, but the \
                     byte-derived host-role table resolves it to {:?}",
                    node.id.0,
                    func_idx,
                    role.plan_tag(),
                    table.lookup(*role)
                ));
            }
            FragNodeKind::HostCall { .. } => {}
            FragNodeKind::If {
                then_block,
                else_block,
                ..
            } => {
                check_plan_host_calls(then_block, table)?;
                check_plan_host_calls(else_block, table)?;
            }
            _ => {}
        }
    }
    Ok(())
}

/// Whether a checked plan contains any `hostCall` node.
fn plan_has_host_calls(block: &FragBlock) -> bool {
    block.nodes.iter().any(|node| match &node.kind {
        FragNodeKind::HostCall { .. } => true,
        FragNodeKind::If {
            then_block,
            else_block,
            ..
        } => plan_has_host_calls(then_block) || plan_has_host_calls(else_block),
        _ => false,
    })
}

/// The straight-line integer face of a host-call expr fragment: exactly
/// `add(param0, box(k))` over one Int parameter. This is the only host-call
/// fragment shape with a rendered proof face today; any other host-call plan
/// fail-closes classification.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct FragIntAddFace {
    pub(crate) k: i64,
    pub(crate) box_idx: u32,
    pub(crate) add_idx: u32,
}

fn expr_fragment_int_add_face(plan: &ExprFragmentPlan) -> Option<FragIntAddFace> {
    if plan.params.as_slice() != [FragTy::IntCarrier] || plan.result != FragTy::IntCarrier {
        return None;
    }
    let [n0, n1, n2, n3] = plan.body.nodes.as_slice() else {
        return None;
    };
    if plan.body.result != FragValueId(3) {
        return None;
    }
    let (FragNodeKind::Local { index: 0 }, FragNodeKind::ConstI64(k)) = (&n0.kind, &n1.kind)
    else {
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

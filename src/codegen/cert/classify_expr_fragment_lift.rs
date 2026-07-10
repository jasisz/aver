fn expr_fragment_ty_from_wasm_param(ty: &TyKind, carrier: u32) -> Option<FragTy> {
    match ty {
        TyKind::F64 => Some(FragTy::F64),
        TyKind::I32 => Some(FragTy::BoolI32),
        TyKind::Ref(idx) if *idx == carrier => Some(FragTy::IntCarrier),
        // Any other concrete reference is an opaque user-ADT/record reference.
        // Fail-closed downstream: plans over `AdtRef` are accepted ONLY when
        // they match the exact field-projection face.
        TyKind::Ref(_) => Some(FragTy::AdtRef),
        _ => None,
    }
}

fn expr_fragment_ty_from_wasm_result(ty: TyKind, carrier: u32) -> Option<FragTy> {
    match ty {
        TyKind::F64 => Some(FragTy::F64),
        TyKind::I32 => Some(FragTy::BoolI32),
        TyKind::Ref(idx) if idx == carrier => Some(FragTy::IntCarrier),
        TyKind::Ref(_) => Some(FragTy::AdtRef),
        _ => None,
    }
}

/// The Lean `List (HostRole × Nat)` literal of the byte-derived host-role
/// table for a whole module. `aver cert verify` splices this into its kernel
/// witness (and the emitter into `Plans.lean`/`Artifact.lean`), so source-plan
/// encoding always runs against byte-derived indices, never plan-supplied
/// ones. The table itself is derived inside `disassemble` (exact carrier-binop
/// signature + first-i64-arith body shape + uniqueness, fail-closed).
pub fn byte_derived_frag_host_table_lean(wasm_bytes: &[u8]) -> Result<String, String> {
    let (_user_fns, _box_idx, _user_idx_set, _carrier, _host_roles, host_table, _struct_field_counts) =
        disassemble(wasm_bytes)?;
    Ok(host_table.lean_value())
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


/// The verbatim field-projection face of an ADT-ref expr fragment: exactly
/// `struct.get ty field∈{0,1}` of the single reference parameter, returned
/// unchanged. This is the only fragment shape admitting `AdtRef` values today;
/// any other ADT-ref plan fail-closes on producer and verifier alike.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct FragProjectFace {
    pub(crate) struct_idx: u32,
    pub(crate) field_idx: u32,
}

fn expr_fragment_project_face(plan: &ExprFragmentPlan) -> Option<FragProjectFace> {
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
                } => frag_block_touches_adt_ref(then_block) || frag_block_touches_adt_ref(else_block),
                _ => false,
            }
    })
}

/// Whether a plan involves opaque user-ADT references anywhere (params, result
/// or body). Such plans are admitted ONLY through the field-projection face.
fn expr_fragment_plan_touches_adt_ref(plan: &ExprFragmentPlan) -> bool {
    plan.params.contains(&FragTy::AdtRef)
        || plan.result == FragTy::AdtRef
        || frag_block_touches_adt_ref(&plan.body)
}

/// Fail-closed validation of every `struct.get.user` node against the
/// byte-derived module struct context: the cited type index must be a real
/// module struct type, must not be the Int carrier, and the field must be
/// inside the struct's field count — the projection twin of the hostCall
/// func-idx-vs-role-table check.
fn check_plan_struct_gets(
    block: &FragBlock,
    carrier: u32,
    struct_field_counts: &std::collections::HashMap<u32, u32>,
) -> Result<(), String> {
    for node in &block.nodes {
        match &node.kind {
            FragNodeKind::StructGetUser { ty_idx, field, .. } => {
                let Some(count) = struct_field_counts.get(ty_idx) else {
                    return Err(format!(
                        "plan struct.get.user v{} cites type {} outside the module's struct types",
                        node.id.0, ty_idx
                    ));
                };
                if *ty_idx == carrier {
                    return Err(format!(
                        "plan struct.get.user v{} cites the Int carrier type {}",
                        node.id.0, ty_idx
                    ));
                }
                if field >= count {
                    return Err(format!(
                        "plan struct.get.user v{} cites field {} outside struct {}'s {} fields",
                        node.id.0, field, ty_idx, count
                    ));
                }
            }
            FragNodeKind::If {
                then_block,
                else_block,
                ..
            } => {
                check_plan_struct_gets(then_block, carrier, struct_field_counts)?;
                check_plan_struct_gets(else_block, carrier, struct_field_counts)?;
            }
            _ => {}
        }
    }
    Ok(())
}

fn collect_sym_block_named_tys(block: &SymBlock, out: &mut Vec<String>) {
    for node in &block.nodes {
        if let SymTy::Named(name) = &node.ty
            && !out.contains(name)
        {
            out.push(name.clone());
        }
        if let SymNodeKind::If {
            then_block,
            else_block,
            ..
        } = &node.kind
        {
            collect_sym_block_named_tys(then_block, out);
            collect_sym_block_named_tys(else_block, out);
        }
    }
}

fn check_sym_block_projection_owners(block: &SymBlock) -> Result<(), String> {
    for node in &block.nodes {
        match &node.kind {
            SymNodeKind::ProjectField {
                type_name, value, ..
            } => {
                let got = block.nodes.get(value.0).map(|n| n.ty.clone());
                if got != Some(SymTy::Named(type_name.clone())) {
                    return Err(format!(
                        "project.field v{} claims owner type `{type_name}`, but its value is declared `{}`",
                        node.id.0,
                        got.map(|ty| ty.plan_tag()).unwrap_or_else(|| "<missing>".to_string())
                    ));
                }
            }
            SymNodeKind::If {
                then_block,
                else_block,
                ..
            } => {
                check_sym_block_projection_owners(then_block)?;
                check_sym_block_projection_owners(else_block)?;
            }
            _ => {}
        }
    }
    Ok(())
}

/// Fail-closed intra-plan consistency for source-level type names. Names are
/// producer-asserted annotations with the MODEL trust story (see
/// docs/certification.md "Read surface"): the kernel-checked content of a
/// projection claim is the byte-derived struct identity (type index + field
/// index), never the name. What CAN be checked is internal consistency, so a
/// relabel must be total across the artifact or decline:
/// - every `named:` source type used anywhere in the plan (params, result,
///   node types) must be anchored by a `project.field` owner — and therefore
///   bound to a byte-derived struct index by the struct table; unanchored
///   names decline;
/// - every projection's claimed owner must be exactly the declared type of
///   the value it projects from.
fn check_sym_plan_named_consistency(plan: &SymPlan) -> Result<(), String> {
    let owners = sym_plan_project_type_names(plan);
    let mut used = Vec::new();
    for ty in plan.params.iter().chain(std::iter::once(&plan.result)) {
        if let SymTy::Named(name) = ty
            && !used.contains(name)
        {
            used.push(name.clone());
        }
    }
    collect_sym_block_named_tys(&plan.body, &mut used);
    for name in &used {
        if !owners.contains(name) {
            return Err(format!(
                "source type `{name}` is never projected, so no byte-derived struct binding anchors it"
            ));
        }
    }
    check_sym_block_projection_owners(&plan.body)
}

/// Byte-derived struct table for ONE export's source plan: the plan's
/// `project.field` type names bound to the export body's own (unique,
/// non-carrier) `struct.get` type index. Names come from the untrusted
/// sidecar, indices come ONLY from the module bytes, and the byte-exact gate
/// then pins the pairing. Fail-closed: no projections → empty table; more
/// than one projected type name or more than one distinct byte-level
/// `struct.get` type → decline.
fn byte_derived_frag_struct_table(
    sym_plan: &SymPlan,
    f: &UserFn,
    carrier: u32,
    struct_field_counts: &std::collections::HashMap<u32, u32>,
) -> Result<FragStructTable, String> {
    let names = sym_plan_project_type_names(sym_plan);
    if names.is_empty() {
        return Ok(FragStructTable::default());
    }
    let [name] = names.as_slice() else {
        return Err(format!(
            "source plan projects {} distinct types; the field-projection face admits one",
            names.len()
        ));
    };
    let mut tys = f
        .ops
        .iter()
        .filter_map(|op| match op {
            Op::StructGet(t, _) if *t != carrier => Some(*t),
            _ => None,
        })
        .collect::<Vec<_>>();
    tys.sort_unstable();
    tys.dedup();
    let [ty_idx] = tys.as_slice() else {
        return Err(format!(
            "export body must contain exactly one non-carrier struct.get type to bind `{name}`, found {}",
            tys.len()
        ));
    };
    if !struct_field_counts.contains_key(ty_idx) {
        return Err(format!(
            "byte-derived struct.get type {ty_idx} is not a module struct type"
        ));
    }
    let mut table = FragStructTable::default();
    table.insert(name, *ty_idx);
    Ok(table)
}

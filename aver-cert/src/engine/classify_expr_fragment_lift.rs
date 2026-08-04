fn expr_fragment_ty_from_wasm_param(ty: &TyKind, carrier: u32) -> Option<FragTy> {
    match ty {
        TyKind::F64 => Some(FragTy::F64),
        TyKind::I32 => Some(FragTy::BoolI32),
        TyKind::Ref { idx, .. } if *idx == carrier => Some(FragTy::IntCarrier),
        // Any other concrete reference is an opaque user-ADT/record reference.
        // Fail-closed downstream: plans over `AdtRef` are accepted ONLY when
        // they match the exact field-projection face.
        TyKind::Ref { .. } => Some(FragTy::AdtRef),
        _ => None,
    }
}

fn expr_fragment_ty_from_wasm_result(ty: TyKind, carrier: u32) -> Option<FragTy> {
    match ty {
        TyKind::F64 => Some(FragTy::F64),
        TyKind::I32 => Some(FragTy::BoolI32),
        TyKind::Ref { idx, .. } if idx == carrier => Some(FragTy::IntCarrier),
        TyKind::Ref { .. } => Some(FragTy::AdtRef),
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

/// Raw Rust-classifier result used by the permanent kernel differential. The
/// production trust path is `AcceptedArtifact.arithTableCheck`: `box` and
/// `toIndex` are bound by their runtime export names and each declared
/// add/sub/mul index is pinned to its synthesized helper body. This helper
/// keeps the Rust classifier available as an independent transition oracle
/// over the full fixture corpus; it is not what the certificate trusts.
pub fn byte_derived_frag_host_role_indices(
    wasm_bytes: &[u8],
) -> Result<FragHostRoleIndices, String> {
    let (
        _user_fns,
        _box_idx,
        _user_idx_set,
        _carrier,
        _host_roles,
        host_table,
        _struct_field_counts,
    ) = disassemble(wasm_bytes)?;
    Ok((
        host_table.box_idx,
        host_table.add_idx,
        host_table.mul_idx,
        host_table.sub_idx,
        host_table.to_index_idx,
        host_table.cmp_idx,
        host_table.eq_idx,
    ))
}

/// Raw Rust F5-classifier result used by the permanent kernel differential.
/// Entries are sorted by function index and include every independent match;
/// there is deliberately no uniqueness filter for string roles.
pub fn byte_derived_string_host_roles(
    wasm_bytes: &[u8],
) -> Result<StringHostRoles, String> {
    let (
        _user_fns,
        _box_idx,
        _user_idx_set,
        _carrier,
        host_roles,
        _host_table,
        _struct_field_counts,
    ) = disassemble(wasm_bytes)?;
    Ok(string_host_roles(&host_roles))
}

/// Every `call` in a candidate expr-fragment body must resolve through the
/// byte-derived host-role table; any other callee fail-closes producer
/// classification (recursion, user calls, unknown helpers).
fn frag_calls_resolvable(calls: &[u32], table: &FragHostTable) -> bool {
    calls
        .iter()
        .all(|idx| {
            Some(*idx) == table.box_idx
                || Some(*idx) == table.add_idx
                || Some(*idx) == table.mul_idx
                || Some(*idx) == table.sub_idx
                || Some(*idx) == table.to_index_idx
                || Some(*idx) == table.cmp_idx
                || Some(*idx) == table.eq_idx
        })
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
            FragNodeKind::VectorGetOrDefault {
                to_index_idx,
                box_idx,
                ..
            } => {
                if table.to_index_idx != Some(*to_index_idx) {
                    return Err(format!(
                        "plan fused vector read v{} cites function {} for role `to_index`,                          but the byte-derived host-role table resolves it to {:?}",
                        node.id.0, to_index_idx, table.to_index_idx
                    ));
                }
                if table.box_idx != Some(*box_idx) {
                    return Err(format!(
                        "plan fused vector read v{} cites function {} for role `box`,                          but the byte-derived host-role table resolves it to {:?}",
                        node.id.0, box_idx, table.box_idx
                    ));
                }
            }
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

// `FragIntAddFace` / `FragProjectFace` and their recognisers moved to
// `expr_fragment_faces.rs` (`plans` layer): the producer's MIR adapter gates
// plan emission on the same faces the classifier admits.

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
            FragNodeKind::VectorGetOrDefault { arr_ty, .. } if *arr_ty == carrier => {
                return Err(format!(
                    "plan fused vector read v{} cites the Int carrier type {} as its array",
                    node.id.0, arr_ty
                ));
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
        match &node.kind {
            SymNodeKind::TagMatch { hit, miss, .. } => {
                collect_sym_block_named_tys(hit, out);
                collect_sym_block_named_tys(miss, out);
            }
            SymNodeKind::If {
                then_block,
                else_block,
                ..
            } => {
                collect_sym_block_named_tys(then_block, out);
                collect_sym_block_named_tys(else_block, out);
            }
            _ => {}
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
            SymNodeKind::TagMatch { hit, miss, .. } => {
                check_sym_block_projection_owners(hit)?;
                check_sym_block_projection_owners(miss)?;
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
/// non-carrier) `struct.get` type index. Names come from the source plan,
/// indices come only from the module bytes, and artifact acceptance pins the
/// pairing. Fail-closed: no projections → empty table; more
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
    // The fused vector read binds its one type name to the body's own
    // `array.get` type; every struct-shaped face binds a `struct.get` type.
    let uses_vector_get = sym_plan_has_vector_get(sym_plan);
    let mut tys = f
        .ops
        .iter()
        .filter_map(|op| match op {
            Op::StructGet(t, _) if !uses_vector_get && *t != carrier => Some(*t),
            Op::ArrayGet(t) if uses_vector_get && *t != carrier => Some(*t),
            _ => None,
        })
        .collect::<Vec<_>>();
    tys.sort_unstable();
    tys.dedup();
    let [ty_idx] = tys.as_slice() else {
        return Err(format!(
            "export body must contain exactly one non-carrier {} type to bind `{name}`, found {}",
            if uses_vector_get {
                "array.get"
            } else {
                "struct.get"
            },
            tys.len()
        ));
    };
    if !uses_vector_get && !struct_field_counts.contains_key(ty_idx) {
        return Err(format!(
            "byte-derived struct.get type {ty_idx} is not a module struct type"
        ));
    }
    let mut table = FragStructTable::default();
    table.insert(name, *ty_idx);
    Ok(table)
}

/// Whether a source plan contains the monolithic fused vector-read node.
fn sym_plan_has_vector_get(plan: &SymPlan) -> bool {
    plan.body
        .nodes
        .iter()
        .any(|node| matches!(node.kind, SymNodeKind::VectorGetOrDefault { .. }))
}

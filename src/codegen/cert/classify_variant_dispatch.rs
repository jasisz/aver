/// General variant dispatch: walk a `ref.test` chain whose hit arms each
/// reduce to one recognised leaf and whose terminal else is a boxed constant.
/// Anything off this grammar returns `None` (falls through to the honest
/// decline reasons). Recognition keys on the parsed tree only — no full
/// opcode sequence is pinned, so arm count, order and per-arm semantics are
/// byte-derived.
fn nr_variant_dispatch(
    f: &UserFn,
    body: &StructuralBody,
    box_idx: u32,
    carrier: Option<u32>,
    host_roles: &std::collections::HashMap<u32, HostRole>,
) -> Option<Cert> {
    let carrier = carrier?;
    // Typed admission: the byte signature must be exactly one nullable,
    // concrete nominal sum-root reference in and one Int carrier out. Full
    // wasm validation has already proved that every tested constructor is a
    // subtype of that parameter root.
    if !matches!(f.params.as_slice(), [TyKind::Ref { nullable: true, .. }])
        || !matches!(f.results.as_slice(),
            [TyKind::Ref { nullable: true, idx }] if *idx == carrier)
    {
        return None;
    }
    let (arms, default_k) = dispatch_chain(&body.tree, box_idx, host_roles)?;
    if arms.is_empty() {
        return None;
    }
    // No duplicate variant tags.
    let mut tags: Vec<u32> = arms.iter().map(|(t, _)| *t).collect();
    tags.sort_unstable();
    tags.dedup();
    if tags.len() != arms.len() {
        return None;
    }
    // At most one host helper per contract role across all arms.
    let mut add_idx = None;
    let mut sub_idx = None;
    for op in body.normalized_ops.iter() {
        let Op::Call(idx) = op else { continue };
        match host_roles.get(idx) {
            Some(HostRole::Add) => {
                if add_idx.is_some_and(|a: u32| a != *idx) {
                    return None;
                }
                add_idx = Some(*idx);
            }
            Some(HostRole::Sub) => {
                if sub_idx.is_some_and(|s: u32| s != *idx) {
                    return None;
                }
                sub_idx = Some(*idx);
            }
            Some(HostRole::StringEq) | Some(HostRole::StringConcat) => return None,
            None => {}
        }
    }
    Some(Cert::VariantDispatch {
        name: f.name.clone(),
        self_idx: f.wasm_idx,
        nlocals: f.nlocals,
        carrier,
        box_idx,
        add_idx,
        sub_idx,
        arms,
        default_k,
        code_entry_bytes: f.code_entry_bytes.clone(),
        ops: strip_trailing_end(&f.ops).to_vec(),
    })
}

/// Parse `[localGet 0, refTest t, ifElse hit els]` where `els` continues the
/// chain or terminates in a boxed constant. Returns the arms in dispatch order
/// plus the default constant.
fn dispatch_chain(
    nodes: &[InstrNode],
    box_idx: u32,
    host_roles: &std::collections::HashMap<u32, HostRole>,
) -> Option<(Vec<(u32, ArmLeaf)>, i64)> {
    let [
        InstrNode::Op(Op::LocalGet(0)),
        InstrNode::Op(Op::RefTest(tag)),
        InstrNode::IfElse(hit, els),
    ] = nodes
    else {
        // Terminal else: a boxed integer constant.
        return match nodes {
            [InstrNode::Op(Op::I64Const(k)), InstrNode::Op(Op::Call(b))] if *b == box_idx => {
                Some((Vec::new(), *k))
            }
            _ => None,
        };
    };
    if has_branch(hit) {
        return None;
    }
    let leaf = leaf_of_arm(&node_ops(hit), *tag, box_idx, host_roles)?;
    let (mut rest, default_k) = dispatch_chain(els, box_idx, host_roles)?;
    rest.insert(0, (*tag, leaf));
    Some((rest, default_k))
}

/// Classify one hit arm as a leaf. The arm must open with the payload
/// projection `localGet 0; refCast tag; structGet tag 0`; the remainder is
/// either empty (projection), a boxed constant fed to a contracted host with
/// the payload first, or — through the emitter's one-local spill — the
/// constant first. Anything else: no leaf.
fn leaf_of_arm(
    ops: &[Op],
    tag: u32,
    box_idx: u32,
    host_roles: &std::collections::HashMap<u32, HostRole>,
) -> Option<ArmLeaf> {
    use Op::*;
    let rest = match ops {
        [LocalGet(0), RefCast(t), StructGet(t2, 0), rest @ ..] if t == &tag && t2 == &tag => rest,
        _ => return None,
    };
    let role = |idx: &u32| host_roles.get(idx).copied();
    match rest {
        [] => Some(ArmLeaf::Proj),
        // payload first: x op k
        [I64Const(k), Call(b), Call(h)] if *b == box_idx => Some(ArmLeaf::HostOp {
            role: role(h)?,
            k: *k,
            const_first: false,
        }),
        // constant first through the spill local: k op x
        [LocalSet(n), I64Const(k), Call(b), LocalGet(n2), Call(h)] if *b == box_idx && n == n2 => {
            Some(ArmLeaf::HostOp {
                role: role(h)?,
                k: *k,
                const_first: true,
            })
        }
        _ => None,
    }
}

fn structural_body(
    f: &UserFn,
    box_idx: Option<u32>,
    user_idx_set: &std::collections::HashSet<u32>,
    host_roles: &std::collections::HashMap<u32, HostRole>,
) -> Option<StructuralBody> {
    if f.has_loop_or_branch {
        return None;
    }
    let ops = strip_trailing_end(&f.ops);
    if ops
        .iter()
        .any(|op| matches!(op, Op::Other | Op::ReturnCall(_)))
    {
        return None;
    }
    for op in ops {
        if let Op::Call(idx) = op {
            if *idx == f.wasm_idx || user_idx_set.contains(idx) {
                return None;
            }
            // With no box helper in the module, every non-host call is
            // unresolvable — strictly fewer bodies parse (fail-closed).
            if Some(*idx) != box_idx && !host_roles.contains_key(idx) {
                return None;
            }
        }
    }
    let normalized_ops = normalize_local_hops(ops);
    let mut pos = 0usize;
    let tree = parse_instr_tree(&normalized_ops, &mut pos, false)?;
    if pos != normalized_ops.len() {
        return None;
    }
    Some(StructuralBody {
        normalized_ops,
        tree,
    })
}

fn parse_instr_tree(ops: &[Op], pos: &mut usize, nested: bool) -> Option<Vec<InstrNode>> {
    let mut out = Vec::new();
    while *pos < ops.len() {
        match &ops[*pos] {
            Op::Else | Op::End if nested => break,
            Op::If => {
                *pos += 1;
                let then_b = parse_instr_tree(ops, pos, true)?;
                if !matches!(ops.get(*pos), Some(Op::Else)) {
                    return None;
                }
                *pos += 1;
                let else_b = parse_instr_tree(ops, pos, true)?;
                if !matches!(ops.get(*pos), Some(Op::End)) {
                    return None;
                }
                *pos += 1;
                out.push(InstrNode::IfElse(then_b, else_b));
            }
            Op::Else | Op::End => return None,
            op => {
                out.push(InstrNode::Op(op.clone()));
                *pos += 1;
            }
        }
    }
    Some(out)
}

fn normalize_local_hops(ops: &[Op]) -> Vec<Op> {
    let mut aliases = std::collections::HashMap::<u32, u32>::new();
    let mut out = Vec::new();
    let mut i = 0usize;
    while i < ops.len() {
        if let (Some(Op::LocalGet(src)), Some(Op::LocalSet(dst))) = (ops.get(i), ops.get(i + 1)) {
            let src = *aliases.get(src).unwrap_or(src);
            aliases.insert(*dst, src);
            i += 2;
            continue;
        }
        let op = match &ops[i] {
            Op::LocalGet(idx) => Op::LocalGet(*aliases.get(idx).unwrap_or(idx)),
            other => other.clone(),
        };
        out.push(op);
        i += 1;
    }

    let mut changed = true;
    while changed {
        changed = false;
        let mut compact = Vec::new();
        let mut j = 0usize;
        while j < out.len() {
            if j + 2 < out.len()
                && matches!(
                    out[j],
                    Op::StructGet(..)
                        | Op::RefCast(..)
                        | Op::I64Const(..)
                        | Op::I32Const(..)
                        | Op::F64Const(..)
                        | Op::RefNull(_)
                        | Op::ArrayNewData { .. }
                )
                && matches!((&out[j + 1], &out[j + 2]), (Op::LocalSet(a), Op::LocalGet(b)) if a == b)
            {
                compact.push(out[j].clone());
                j += 3;
                changed = true;
            } else {
                compact.push(out[j].clone());
                j += 1;
            }
        }
        out = compact;
    }
    out
}

fn flat_ops(nodes: &[InstrNode]) -> Vec<&Op> {
    let mut out = Vec::new();
    collect_flat_ops(nodes, &mut out);
    out
}

fn collect_flat_ops<'a>(nodes: &'a [InstrNode], out: &mut Vec<&'a Op>) {
    for node in nodes {
        match node {
            InstrNode::Op(op) => out.push(op),
            InstrNode::IfElse(then_b, else_b) => {
                collect_flat_ops(then_b, out);
                collect_flat_ops(else_b, out);
            }
        }
    }
}

fn node_ops(nodes: &[InstrNode]) -> Vec<Op> {
    flat_ops(nodes).into_iter().cloned().collect()
}

fn has_branch(nodes: &[InstrNode]) -> bool {
    nodes.iter().any(|node| match node {
        InstrNode::Op(_) => false,
        InstrNode::IfElse(..) => true,
    })
}

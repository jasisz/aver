/// Structurally recognise fuel self-recursion from the parsed instruction tree:
///   f n     = if n≤0 then BASE else n + f (n-1)        (body-consumed, arity 1)
///   f n acc = if n≤0 then acc  else f (n-1) (acc + n)   (tail accumulator, arity 2)
/// The carrier-sign predicate preamble, the descent (`n-1`) and the combinator
/// (host `add` or `mul`) are pinned; the BASE literal of the arity-1 shape is DATA (any
/// value, recovered from the bytes, not the fixed `0`). Recognition keys on the
/// parsed tree and a symbolic step evaluator; no full opcode sequence is pinned.
fn recognize_fueled_recursion(
    f: &UserFn,
    box_idx: u32,
    carrier: Option<u32>,
    host_roles: &std::collections::HashMap<u32, HostRole>,
    model_ops: &std::collections::HashMap<String, char>,
) -> Option<Cert> {
    use Op::*;
    let carrier = carrier?;
    if f.arity != 1 && f.arity != 2 {
        return None;
    }
    let ops = strip_trailing_end(&f.ops);
    // Only the self-call, box, and contracted host helpers may be called; no
    // foreign user calls, no opaque ops. Must actually recurse.
    let mut recurses = false;
    for op in ops {
        match op {
            Op::Call(idx) | Op::ReturnCall(idx) => {
                if *idx == f.wasm_idx {
                    recurses = true;
                } else if *idx != box_idx && !host_roles.contains_key(idx) {
                    return None;
                }
            }
            Op::Other => return None,
            _ => {}
        }
    }
    if !recurses {
        return None;
    }
    let normalized = normalize_local_hops(ops);
    let mut pos = 0usize;
    let tree = parse_instr_tree(&normalized, &mut pos, false)?;
    if pos != normalized.len() {
        return None;
    }
    // preamble: localGet 0; structGet carrier 1; refIsNull;
    //           IfElse(sign-predicate); IfElse(base-arm, step-arm)
    let [
        InstrNode::Op(LocalGet(0)),
        InstrNode::Op(StructGet(c1, 1)),
        InstrNode::Op(RefIsNull),
        InstrNode::IfElse(pred_small, pred_big),
        InstrNode::IfElse(base_arm, step_arm),
    ] = tree.as_slice()
    else {
        return None;
    };
    if *c1 != carrier {
        return None;
    }
    // n≤0 predicate: small = [localGet 0, structGet c 0, i64Const 0, i64LeS];
    //               big   = [localGet 0, structGet c 2, i32Const 0, i32LtS]
    let small_ok = matches!(
        node_ops(pred_small).as_slice(),
        [LocalGet(0), StructGet(cc, 0), I64Const(0), I64LeS] if *cc == carrier
    );
    let big_ok = matches!(
        node_ops(pred_big).as_slice(),
        [LocalGet(0), StructGet(cc, 2), I32Const(0), I32LtS] if *cc == carrier
    );
    if !small_ok || !big_ok {
        return None;
    }
    let is_host = |idx: &u32, role: HostRole| host_roles.get(idx) == Some(&role);
    if f.arity == 1 {
        // base arm: [i64Const k, call box] — any literal k (the data-driven base).
        let base_k = match node_ops(base_arm).as_slice() {
            [I64Const(k), Call(b)] if *b == box_idx => *k,
            _ => return None,
        };
        // step arm: `<op>(_, _)` combining the self-call `f(sub(n,1))` with the
        // input `n` or a boxed constant, in either operand order — recovered by
        // symbolically executing the straight-line step (descent pinned to n-1).
        let (sub_idx, add_idx, rec_first, other) =
            parse_body_step(&node_ops(step_arm), box_idx, f.wasm_idx, host_roles)?;
        // The source model chooses the semantic operator; the byte-derived host
        // role must independently agree with that choice.
        let combinator = match model_ops.get(&f.name) {
            Some('+') => Combinator::Add,
            Some('*') => Combinator::Mul,
            _ => return None,
        };
        let expected_role = match combinator {
            Combinator::Add => HostRole::Add,
            Combinator::Mul => HostRole::Mul,
        };
        if host_roles.get(&add_idx) != Some(&expected_role) {
            return None;
        }
        // The anti-vacuity guards evaluate the model at fixed samples; a large
        // multiplier or base can exceed i128 — decline fail-closed rather than
        // overflow-panic in the emitter.
        if [3i64, 0, -4]
            .iter()
            .any(|&s| eval_body_recursion(s, base_k, other, combinator).is_none())
        {
            return None;
        }
        Some(Cert::Recursive {
            name: f.name.clone(),
            self_idx: f.wasm_idx,
            type_idx: f.type_idx,
            nlocals: f.nlocals,
            carrier,
            box_idx,
            add_idx,
            sub_idx,
            base_k,
            rec_first,
            other,
            combinator,
            code_entry_bytes: f.code_entry_bytes.clone(),
        })
    } else {
        // arity 2: accumulator tail recursion; base arm returns the accumulator.
        if !matches!(node_ops(base_arm).as_slice(), [LocalGet(1)]) {
            return None;
        }
        // step arm: f(n-1, acc+n) as
        //   [localGet 0, i64Const 1, call box, call SUB, localGet 1, localGet 0, call ADD, returnCall SELF]
        let (sub_idx, add_idx) = match node_ops(step_arm).as_slice() {
            [
                LocalGet(0),
                I64Const(1),
                Call(b),
                Call(sub),
                LocalGet(1),
                LocalGet(0),
                Call(add),
                ReturnCall(sc),
            ] if *b == box_idx
                && *sc == f.wasm_idx
                && is_host(sub, HostRole::Sub)
                && is_host(add, HostRole::Add) =>
            {
                (*sub, *add)
            }
            _ => return None,
        };
        Some(Cert::AccumulatorRecursive {
            name: f.name.clone(),
            self_idx: f.wasm_idx,
            type_idx: f.type_idx,
            nlocals: f.nlocals,
            carrier,
            box_idx,
            add_idx,
            sub_idx,
            code_entry_bytes: f.code_entry_bytes.clone(),
        })
    }
}

/// Recognise one mutual-recursion member body from the parsed tree:
///   `f n = if n≤0 then base else g (n-1)` — a tail cross-call to another user
/// function. Returns `(base_k, cross_idx, sub_idx)`; descent pinned to `n-1`.
/// Shares the carrier-sign preamble/predicate with `recognize_fueled_recursion`.
fn recognize_mutual_member(
    f: &UserFn,
    box_idx: u32,
    carrier: u32,
    user_idx_set: &std::collections::HashSet<u32>,
    host_roles: &std::collections::HashMap<u32, HostRole>,
) -> Option<(i64, u32, u32)> {
    use Op::*;
    if f.arity != 1 {
        return None;
    }
    let ops = strip_trailing_end(&f.ops);
    // The only call is the box; the only tail-call is one other user function; no
    // self-call, no foreign user CALL, no opaque ops.
    let mut cross: Option<u32> = None;
    for op in ops {
        match op {
            Op::ReturnCall(idx) => {
                if *idx == f.wasm_idx || !user_idx_set.contains(idx) {
                    return None;
                }
                if cross.is_some_and(|c| c != *idx) {
                    return None;
                }
                cross = Some(*idx);
            }
            Op::Call(idx) if *idx != box_idx && !host_roles.contains_key(idx) => {
                return None;
            }
            Op::Other => return None,
            _ => {}
        }
    }
    let cross = cross?;
    let normalized = normalize_local_hops(ops);
    let mut pos = 0usize;
    let tree = parse_instr_tree(&normalized, &mut pos, false)?;
    if pos != normalized.len() {
        return None;
    }
    let [
        InstrNode::Op(LocalGet(0)),
        InstrNode::Op(StructGet(c1, 1)),
        InstrNode::Op(RefIsNull),
        InstrNode::IfElse(pred_small, pred_big),
        InstrNode::IfElse(base_arm, step_arm),
    ] = tree.as_slice()
    else {
        return None;
    };
    if *c1 != carrier {
        return None;
    }
    if !matches!(
        node_ops(pred_small).as_slice(),
        [LocalGet(0), StructGet(cc, 0), I64Const(0), I64LeS] if *cc == carrier
    ) || !matches!(
        node_ops(pred_big).as_slice(),
        [LocalGet(0), StructGet(cc, 2), I32Const(0), I32LtS] if *cc == carrier
    ) {
        return None;
    }
    let base_k = match node_ops(base_arm).as_slice() {
        [I64Const(k), Call(b)] if *b == box_idx => *k,
        _ => return None,
    };
    // step arm: [localGet 0, i64Const 1, call box, call SUB, returnCall CROSS]
    let sub_idx = match node_ops(step_arm).as_slice() {
        [LocalGet(0), I64Const(1), Call(b), Call(sub), ReturnCall(cc)]
            if *b == box_idx && *cc == cross && host_roles.get(sub) == Some(&HostRole::Sub) =>
        {
            *sub
        }
        _ => return None,
    };
    Some((base_k, cross, sub_idx))
}

/// Recognise a mutually-recursive SCC starting from `f`: follow the tail
/// cross-calls; the chain must return to `f` (a simple cycle) with every member
/// on it a mutual member. Produces this member's `Cert::MutualRecursion` carrying
/// the whole SCC (sorted by `self_idx` so the checker re-derives the same set).
fn recognize_mutual_scc(
    f: &UserFn,
    box_idx: u32,
    carrier: Option<u32>,
    user_idx_set: &std::collections::HashSet<u32>,
    fns: &std::collections::HashMap<u32, &UserFn>,
    host_roles: &std::collections::HashMap<u32, HostRole>,
) -> Option<Cert> {
    let carrier = carrier?;
    let (_, _, sub_idx) = recognize_mutual_member(f, box_idx, carrier, user_idx_set, host_roles)?;
    // Walk the cross-call chain from f; it must close back to f.
    let mut cycle: Vec<MutualMember> = Vec::new();
    let mut cur = f.wasm_idx;
    loop {
        let uf = fns.get(&cur)?;
        let (base_k, cross_idx, s) =
            recognize_mutual_member(uf, box_idx, carrier, user_idx_set, host_roles)?;
        if s != sub_idx {
            return None; // all members share one sub contract
        }
        cycle.push(MutualMember {
            name: uf.name.clone(),
            self_idx: cur,
            type_idx: uf.type_idx,
            nlocals: uf.nlocals,
            base_k,
            cross_idx,
            code_entry_bytes: uf.code_entry_bytes.clone(),
        });
        cur = cross_idx;
        if cur == f.wasm_idx {
            break;
        }
        if cycle.len() > 64 || cycle.iter().any(|m| m.self_idx == cur) {
            return None; // not a simple cycle through f
        }
    }
    if cycle.len() < 2 {
        return None; // a 1-cycle is ordinary self-recursion, handled elsewhere
    }
    let mut scc = cycle;
    scc.sort_by_key(|m| m.self_idx);
    let position = scc.iter().position(|m| m.self_idx == f.wasm_idx)?;
    Some(Cert::MutualRecursion {
        name: f.name.clone(),
        self_idx: f.wasm_idx,
        carrier,
        box_idx,
        sub_idx,
        position,
        scc,
    })
}

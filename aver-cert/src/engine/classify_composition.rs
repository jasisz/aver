enum CompositionOutcome {
    /// The caller and its whole closure classify — a composition certificate.
    Certified(Box<Cert>),
    /// The caller IS a unary user-call chain, but its closure leaves the
    /// certified classes (an out-of-class callee, or a cycle). The specific,
    /// honest reason the caller declines.
    Declined(String),
    /// The caller is not a unary composition chain at all — let the ordinary
    /// decline reasons apply.
    NotApplicable,
}

/// Recognise one function as a straight-line integer shape usable inside a
/// composition closure: a self-sum (`x + x`) or a unary chain of user calls.
/// Returns `None` for anything else (a runtime/ADT/branch body, wrong arity).
fn classify_leaf_shape(
    f: &UserFn,
    user_idx_set: &std::collections::HashSet<u32>,
) -> Option<LeafShape> {
    use Op::*;
    if f.arity != 1 {
        return None;
    }
    let ops = strip_trailing_end(&f.ops);
    // Self-sum: [localGet 0, localGet 0, call add] where `add` is a host helper.
    if let [LocalGet(0), LocalGet(0), Call(a)] = ops
        && *a != f.wasm_idx
        && !user_idx_set.contains(a)
    {
        return Some(LeafShape::SelfSum { add_idx: *a });
    }
    // Unary chain: [localGet 0, call c1, ..., call cm] (m >= 1), each ci a user
    // function other than the caller itself. No other opcodes.
    if let Some((LocalGet(0), rest)) = ops.split_first()
        && !rest.is_empty()
        && rest.iter().all(|op| match op {
            Call(c) => *c != f.wasm_idx && user_idx_set.contains(c),
            _ => false,
        })
    {
        let calls = rest
            .iter()
            .map(|op| match op {
                Call(c) => *c,
                _ => unreachable!(),
            })
            .collect();
        return Some(LeafShape::Chain { calls });
    }
    None
}

/// Try to certify `f` as a cross-function composition. `f` qualifies only if its
/// own body is a unary user-call chain; then its transitive call closure must be
/// wholly covered by the straight-line integer shapes.
fn try_composition(
    f: &UserFn,
    box_idx: Option<u32>,
    carrier: Option<u32>,
    user_idx_set: &std::collections::HashSet<u32>,
    fns: &std::collections::HashMap<u32, &UserFn>,
) -> CompositionOutcome {
    // Only a unary CHAIN caller (one that actually calls other user functions)
    // is a composition; a self-sum / non-chain body is handled elsewhere.
    match classify_leaf_shape(f, user_idx_set) {
        Some(LeafShape::Chain { .. }) => {
            let Some(carrier) = carrier else {
                return CompositionOutcome::Declined(
                    "carrier struct type not found in module".to_string(),
                );
            };
            let mut closure: std::collections::HashMap<u32, ClosureEntry> =
                std::collections::HashMap::new();
            let mut path: Vec<u32> = Vec::new();
            if let Err(reason) =
                collect_closure(f.wasm_idx, fns, user_idx_set, &mut closure, &mut path)
            {
                return CompositionOutcome::Declined(reason);
            }
            let mut entries: Vec<ClosureEntry> = closure.into_values().collect();
            entries.sort_by_key(|e| e.self_idx);
            let (has_add, has_sub, has_box) = closure_contracts(&entries);
            let _ = box_idx;
            CompositionOutcome::Certified(Box::new(Cert::Composition {
                name: f.name.clone(),
                self_idx: f.wasm_idx,
                carrier,
                closure: entries,
                has_add,
                has_sub,
                has_box,
            }))
        }
        _ => CompositionOutcome::NotApplicable,
    }
}

/// DFS the call graph from `idx`, classifying every reached function as a
/// straight-line integer shape. `path` is the active DFS stack (cycle guard);
/// `closure` collects each entry once. Fails closed on any out-of-class callee
/// or any cycle.
fn collect_closure(
    idx: u32,
    fns: &std::collections::HashMap<u32, &UserFn>,
    user_idx_set: &std::collections::HashSet<u32>,
    closure: &mut std::collections::HashMap<u32, ClosureEntry>,
    path: &mut Vec<u32>,
) -> Result<(), String> {
    if closure.contains_key(&idx) {
        return Ok(());
    }
    if path.contains(&idx) {
        let name = fns.get(&idx).map(|f| f.name.as_str()).unwrap_or("?");
        return Err(format!(
            "cycle in the call graph through user function `{name}`; composition requires an acyclic closure"
        ));
    }
    let Some(uf) = fns.get(&idx) else {
        return Err(
            "a callee in the composition closure is not an in-module user function".to_string(),
        );
    };
    let Some(shape) = classify_leaf_shape(uf, user_idx_set) else {
        return Err(format!(
            "callee `{}` is outside the certified composition classes (not a unary self-sum or a unary user-call chain)",
            uf.name
        ));
    };
    path.push(idx);
    if let LeafShape::Chain { calls } = &shape {
        for c in calls {
            collect_closure(*c, fns, user_idx_set, closure, path)?;
        }
    }
    path.pop();
    closure.insert(
        idx,
        ClosureEntry {
            name: uf.name.clone(),
            self_idx: idx,
            type_idx: uf.type_idx,
            nlocals: uf.nlocals,
            code_entry_bytes: uf.code_entry_bytes.clone(),
            ops: strip_trailing_end(&uf.ops).to_vec(),
            shape,
        },
    );
    Ok(())
}

/// `(has_add, has_sub, has_box)` runtime contracts consumed across the closure.
/// v1 leaves consume only carrier `add`; the flags keep the manifest honest as
/// the leaf vocabulary grows.
fn closure_contracts(entries: &[ClosureEntry]) -> (bool, bool, bool) {
    let mut has_add = false;
    for e in entries {
        if let LeafShape::SelfSum { .. } = e.shape {
            has_add = true;
        }
    }
    (has_add, false, false)
}

fn strip_trailing_end(ops: &[Op]) -> &[Op] {
    match ops.last() {
        Some(Op::End) => &ops[..ops.len() - 1],
        _ => ops,
    }
}

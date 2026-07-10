fn nr_straightline(
    f: &UserFn,
    body: &StructuralBody,
    box_idx: u32,
    carrier: Option<u32>,
    host_roles: &std::collections::HashMap<u32, HostRole>,
) -> Option<Cert> {
    use Op::*;
    if has_branch(&body.tree) {
        return None;
    }
    let [LocalGet(0), I64Const(k), Call(b), Call(a)] = body.normalized_ops.as_slice() else {
        return None;
    };
    if *b != box_idx || host_roles.get(a) != Some(&HostRole::Add) {
        return None;
    }
    Some(Cert::StraightLine {
        name: f.name.clone(),
        self_idx: f.wasm_idx,
        nlocals: f.nlocals,
        carrier: carrier?,
        k: *k,
        box_idx,
        add_idx: *a,
    })
}

fn nr_adt_constructor(
    f: &UserFn,
    body: &StructuralBody,
    box_idx: u32,
    carrier: Option<u32>,
) -> Option<Cert> {
    use Op::*;
    if has_branch(&body.tree) || f.arity == 0 || f.arity > 2 {
        return None;
    }
    let ops = &body.normalized_ops;
    let (last, prefix) = ops.split_last()?;
    let StructNew(struct_idx, field_count) = last else {
        return None;
    };
    if *struct_idx == carrier? {
        return None;
    }
    let mut fields = Vec::new();
    for op in prefix {
        match op {
            LocalGet(i) if (*i as usize) < f.arity => fields.push(ConstructorField::Local(*i)),
            RefNull(_) => fields.push(ConstructorField::Null),
            _ => return None,
        }
    }
    if fields.len() != *field_count as usize {
        return None;
    }
    let mut seen_locals = fields
        .iter()
        .filter_map(|field| match field {
            ConstructorField::Local(i) => Some(*i),
            ConstructorField::Null => None,
        })
        .collect::<Vec<_>>();
    seen_locals.sort_unstable();
    seen_locals.dedup();
    if seen_locals != (0..f.arity as u32).collect::<Vec<_>>() {
        return None;
    }
    if f.calls.iter().any(|c| *c == f.wasm_idx || *c != box_idx) {
        return None;
    }
    Some(Cert::AdtConstructor {
        name: f.name.clone(),
        self_idx: f.wasm_idx,
        code_idx: f.code_idx,
        type_idx: f.type_idx,
        nlocals: f.nlocals,
        carrier: carrier?,
        struct_idx: *struct_idx,
        field_count: *field_count,
        arity: f.arity,
        fields,
        ops: strip_trailing_end(&f.ops).to_vec(),
    })
}

fn nr_field_projection(f: &UserFn, body: &StructuralBody, carrier: Option<u32>) -> Option<Cert> {
    use Op::*;
    if has_branch(&body.tree) || !f.calls.is_empty() {
        return None;
    }
    let carrier = carrier?;
    let mut gets = body
        .normalized_ops
        .iter()
        .filter_map(|op| match op {
            StructGet(t, field) => Some((*t, *field)),
            _ => None,
        })
        .collect::<Vec<_>>();
    if gets.len() != 1 {
        return None;
    }
    let (struct_idx, field_idx) = gets.pop()?;
    if struct_idx == carrier || field_idx > 1 {
        return None;
    }
    Some(Cert::FieldProjection {
        name: f.name.clone(),
        self_idx: f.wasm_idx,
        nlocals: f.nlocals,
        carrier,
        struct_idx,
        field_idx,
        ops: strip_trailing_end(&f.ops).to_vec(),
    })
}

fn nr_ref_dispatch_match(
    f: &UserFn,
    body: &StructuralBody,
    box_idx: u32,
    carrier: Option<u32>,
    _host_roles: &std::collections::HashMap<u32, HostRole>,
) -> Option<Cert> {
    let carrier = carrier?;
    if body
        .normalized_ops
        .iter()
        .take_while(|op| !matches!(op, Op::RefTest(_)))
        .any(|op| matches!(op, Op::StructNew(..)))
    {
        return None;
    }
    for pair in body.tree.windows(2) {
        let [
            InstrNode::Op(Op::RefTest(hit)),
            InstrNode::IfElse(hit_arm, miss_arm),
        ] = pair
        else {
            continue;
        };
        if *hit == carrier {
            continue;
        }
        let hit_ops = node_ops(hit_arm);
        if !hit_ops
            .iter()
            .any(|op| matches!(op, Op::StructGet(t, 0) if t == hit))
        {
            continue;
        }
        let miss_ops = node_ops(miss_arm);
        if matches!(miss_ops.as_slice(), [Op::I64Const(0), Op::Call(b)] if *b == box_idx) {
            return Some(Cert::WidenedIntMatch {
                name: f.name.clone(),
                self_idx: f.wasm_idx,
                nlocals: f.nlocals,
                carrier,
                hit_variant_idx: *hit,
                box_idx,
                ops: strip_trailing_end(&f.ops).to_vec(),
            });
        }
        // Typed admission: exactly one user ADT (eqref) value in — a two-parameter
        // dispatch keeps a byte-identical code entry, so a unary obligation must
        // not be certified for a binary export — and exactly one result of the
        // kind the default implies (a nullable ref here; the plan path additionally
        // pins the exact `[eqref] -> [(ref null) resultHeapTy]` signature in-kernel).
        if f.calls.is_empty()
            && matches!(f.params.as_slice(), [TyKind::Eqref])
            && let Some(default) = verbatim_default_from_ops(&miss_ops)
            && verbatim_results_ok(&f.results, &default)
        {
            return Some(Cert::VerbatimWidenedMatch {
                name: f.name.clone(),
                self_idx: f.wasm_idx,
                nlocals: f.nlocals,
                carrier,
                hit_variant_idx: *hit,
                default,
                code_entry_bytes: f.code_entry_bytes.clone(),
                ops: strip_trailing_end(&f.ops).to_vec(),
            });
        }
    }
    None
}

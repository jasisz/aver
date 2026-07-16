struct ClassifierContext<'a> {
    host_roles: &'a std::collections::HashMap<u32, HostRole>,
    struct_field_counts: &'a std::collections::HashMap<u32, u32>,
    model_ops: &'a std::collections::HashMap<String, char>,
}

fn classify_without_expr_fragment(
    f: &UserFn,
    box_idx: Option<u32>,
    carrier: Option<u32>,
    user_idx_set: &std::collections::HashSet<u32>,
    fns: &std::collections::HashMap<u32, &UserFn>,
    context: &ClassifierContext<'_>,
) -> Result<Cert, String> {
    // Fuel self-recursion (single-argument `n + f(n-1)` / `n * f(n-1)` and
    // two-argument accumulator), recognised structurally from the instruction
    // tree. The base value is data (any literal / the accumulator) and the
    // combinator operation comes from the model, not a pinned constant.
    if let Some(cert) = recognize_fueled_recursion(
        f,
        box_idx,
        carrier,
        context.host_roles,
        context.model_ops,
    ) {
        return Ok(cert);
    }

    // Mutual-recursion SCC (each member tail-calls the next; the cycle closes),
    // proven by one conjunction fuel-induction over the whole SCC where every
    // cross-call is discharged by the matching conjunct of the induction
    // hypothesis (the `mutual_sim` shape validated in
    // probe-artifacts/mutual-fuel-probe/MutualSpike-twodefs.lean). The lowest-
    // `self_idx` member emits the shared code table + bridge + `mutual_sim`;
    // every member emits its own obligation citing the matching conjunct.
    if let Some(cert) = recognize_mutual_scc(
        f,
        box_idx,
        carrier,
        user_idx_set,
        fns,
        context.host_roles,
    ) {
        return Ok(cert);
    }

    if let Some(cert) = walk_nonrecursive(
        f,
        box_idx,
        carrier,
        user_idx_set,
        context.host_roles,
        context.struct_field_counts,
    ) {
        return Ok(Cert::NonRecursive {
            inner: Box::new(cert),
        });
    }

    // ---- decline with an honest reason -----------------------------------
    // Arity 2 is a supported signature (the accumulator-recursion template),
    // so a 2-argument function that did not match falls through to the
    // shape-based reasons below instead of a contradictory signature message.
    if f.arity != 1 && f.arity != 2 {
        return Err(format!(
            "unsupported signature ({} params); Stage-B templates cover one-argument Int functions and two-argument accumulator recursion",
            f.arity
        ));
    }
    if f.has_loop_or_branch {
        return Err(
            "body uses loops/branches outside the certified straight-line/recursive fragment"
                .to_string(),
        );
    }
    // Cross-function composition: a unary chain caller whose entire call closure
    // is itself certified by the straight-line integer shapes. A chain caller
    // whose closure leaves the classes declines with a specific reason.
    match try_composition(f, box_idx, carrier, user_idx_set, fns) {
        CompositionOutcome::Certified(c) => return Ok(*c),
        CompositionOutcome::Declined(reason) => return Err(reason),
        CompositionOutcome::NotApplicable => {}
    }
    let calls_other_user = f
        .calls
        .iter()
        .any(|c| *c != f.wasm_idx && user_idx_set.contains(c));
    if calls_other_user {
        return Err(
            "calls other user functions (cross-function / mutual recursion), outside Stage-B scope"
                .to_string(),
        );
    }
    if f.ops.iter().any(|o| matches!(o, Op::Other)) {
        return Err(
            "body uses opcodes outside the certified fragment (strings / ADTs / effects / tail calls)"
                .to_string(),
        );
    }
    // A module can legitimately carry no Int runtime at all. Say so instead
    // of the generic template list: every integer-family template cites the
    // box helper, so it can never match here, and the carrier-free templates
    // above already had their shot.
    if box_idx.is_none() {
        return Err(
            "body does not match a carrier-free certified template, and the module has no Int carrier host helpers (`__rt_aint_from_i64`); integer-family certification requires the Int carrier"
                .to_string(),
        );
    }
    Err("body does not match a certified template (straight-line add-constant, single-argument self-recursion, two-argument accumulator recursion, or non-recursive ADT constructor/projection/match)".to_string())
}

#[derive(Clone)]
enum InstrNode {
    Op(Op),
    IfElse(Vec<InstrNode>, Vec<InstrNode>),
}

struct StructuralBody {
    normalized_ops: Vec<Op>,
    tree: Vec<InstrNode>,
}

fn walk_nonrecursive(
    f: &UserFn,
    box_idx: Option<u32>,
    carrier: Option<u32>,
    user_idx_set: &std::collections::HashSet<u32>,
    host_roles: &std::collections::HashMap<u32, HostRole>,
    struct_field_counts: &std::collections::HashMap<u32, u32>,
) -> Option<Cert> {
    if f.arity == 0 {
        return None;
    }
    let body = structural_body(f, box_idx, user_idx_set, host_roles)?;
    nr_adt_constructor(f, &body, box_idx, carrier)
        .or_else(|| nr_field_projection(f, &body, carrier, struct_field_counts))
        .or_else(|| nr_ref_dispatch_match(f, &body, box_idx, carrier, host_roles))
        .or_else(|| nr_verbatim_variant_dispatch(f, &body, carrier))
        .or_else(|| nr_string_eq_verbatim_match(f, &body, carrier, host_roles))
        .or_else(|| nr_string_concat_verbatim_match(f, &body, carrier, host_roles))
        .or_else(|| nr_variant_dispatch(f, &body, box_idx, carrier, host_roles))
}

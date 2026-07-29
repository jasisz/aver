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

    // ---- decline with the blocker that actually applies --------------------
    //
    // Every template above has already had its shot, so this section only
    // reports. It reports the blocker that survives fixing the others, in this
    // order:
    //
    //   1. effects. A body that calls a host capability is not a pure function
    //      of its arguments, so no signature and no rewrite of the rest of the
    //      body could make it a simulation of the source model.
    //   2. a module with no Int carrier, where every remaining template is out
    //      of reach whatever this one export looks like.
    //   3. what the export DECLARES — parameter shapes, then result shapes.
    //      No rewrite of a body removes a String parameter or a record result.
    //   4. what the body USES: control flow, then instructions outside the
    //      opcode vocabulary.
    //   5. what the body CALLS, which the composition and mutual-recursion
    //      routes above do cover in specific shapes.
    //   6. the arity-shaped template misses, LAST. A parameter count is only
    //      ever the blocker once nothing else is, and reporting it first said
    //      "unsupported signature" to hundreds of exports whose signature was
    //      not the problem at all.
    //
    // Composition keeps the first word because it is still a certifying route:
    // its chain callers are unary Int-carrier functions that call other user
    // functions, so any check below would otherwise pre-empt a certificate.
    match try_composition(f, box_idx, carrier, user_idx_set, fns) {
        CompositionOutcome::Certified(c) => return Ok(*c),
        CompositionOutcome::Declined(reason) => return Err(reason),
        CompositionOutcome::NotApplicable => {}
    }
    if let Some(capability) = f.host_capability_calls.first() {
        return Err(format!(
            "calls the host capability `{capability}`; certified templates simulate pure bodies, never effects"
        ));
    }
    // A module can legitimately carry no Int runtime at all. Say so before any
    // per-function shape complaint: every integer-family template cites the box
    // helper, and the carrier-free templates above already had their shot, so
    // in such a module nothing this export could be rewritten into would bind.
    // Blaming its parameters here would point at a fix that cannot work.
    if box_idx.is_none() {
        return Err(
            "body does not match a carrier-free certified template, and the module has no Int carrier host helpers (`__rt_aint_from_i64`); integer-family certification requires the Int carrier"
                .to_string(),
        );
    }
    if let Some((position, shape)) = f
        .param_shapes
        .iter()
        .enumerate()
        .find(|(_, shape)| shape.outside_scalar_fragment())
    {
        return Err(format!(
            "parameter {} is {}; a value of that shape is certified only by the projection, variant-dispatch and String templates, and this body matches none of them",
            position + 1,
            shape.describe()
        ));
    }
    if f.results.is_empty() {
        return Err(
            "returns nothing; every certified template simulates a function that returns one value"
                .to_string(),
        );
    }
    if f.results.len() > 1 {
        return Err(format!(
            "returns {} values; every certified template simulates a function that returns one",
            f.results.len()
        ));
    }
    if let Some(shape) = f
        .result_shapes
        .first()
        .filter(|shape| shape.outside_scalar_fragment())
    {
        return Err(format!(
            "returns {}; a value of that shape is built only by the constructor, projection, variant-dispatch and String templates, and this body matches none of them",
            shape.describe()
        ));
    }
    if f.has_loop_or_branch {
        return Err(
            "body uses loops/branches outside the certified straight-line/recursive fragment"
                .to_string(),
        );
    }
    if f.ops.iter().any(|o| matches!(o, Op::Other)) {
        return Err(match &f.first_unsupported_op {
            Some(instruction) => format!(
                "body uses the wasm instruction `{instruction}`, which is outside the certified fragment"
            ),
            None => "body uses wasm instructions outside the certified fragment".to_string(),
        });
    }
    let calls_other_user = f
        .calls
        .iter()
        .any(|c| *c != f.wasm_idx && user_idx_set.contains(c));
    if calls_other_user {
        return Err(
            "calls other user functions; only the composition and mutual-recursion templates cross function boundaries, and this body fits neither"
                .to_string(),
        );
    }
    // Only now can a parameter count be the honest answer: the signature is
    // scalar, the body is pure straight-line code that calls nobody, and still
    // nothing matched. Name the family templates the arity ruled out rather
    // than implying the signature is unsupported in general — the arity-free
    // templates (field projection above all) accept any parameter count.
    if f.arity == 0 {
        return Err(
            "takes no parameters; every certified template simulates a function of at least one argument"
                .to_string(),
        );
    }
    if f.arity > 2 {
        return Err(format!(
            "takes {} parameters; the arity-free templates did not match this body, and the recursion, ADT-construction, variant-dispatch, String and composition templates take one or two arguments",
            f.arity
        ));
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

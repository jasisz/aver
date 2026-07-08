/// An `Int` literal for Lean source: negatives parenthesised so `.i64Const -7`
/// does not misparse; non-negatives bare (byte-identical to the shipped `0`).
fn lean_int_lit(k: i64) -> String {
    if k < 0 {
        format!("({k})")
    } else {
        k.to_string()
    }
}

/// `i128` variant for anti-vacuity guard values, which can exceed `i64` for a
/// multiplication recursion (`Int` in Lean is unbounded, so the wide value is a
/// faithful guard).
fn lean_int_lit128(k: i128) -> String {
    if k < 0 {
        format!("({k})")
    } else {
        k.to_string()
    }
}

/// The `CodeTbl` VALUE (the `fun fn => ...` lambda, no `def` wrapper) a
/// certified body decodes to. This is the term the checker splices, verbatim,
/// into `CheckerWitness.lean` and pins with `rfl` against
/// `manifest.obligations.map (·.code)`, so a `{name}Code` def in the cert's
/// `Module.lean` that diverges from the bytes fails the kernel witness. Kept
/// byte-identical to the RHS `render_code_def` emits so the emitted `Module.lean`
/// is unchanged.
fn render_code_value(c: &Cert) -> String {
    match c.inner() {
        Cert::StraightLine {
            self_idx,
            nlocals,
            k,
            box_idx,
            add_idx,
            ..
        } => format!(
            "fun fn =>\n  \
             if fn = {self_idx} then some ⟨1, {nlocals}, \
             [.localGet 0, .i64Const ({k}), .call {box_idx}, .call {add_idx}]⟩ else none",
        ),
        Cert::Recursive {
            self_idx,
            nlocals,
            carrier,
            box_idx,
            add_idx,
            sub_idx,
            base_k,
            rec_first,
            other,
            ..
        } => {
            let base = lean_int_lit(*base_k);
            // The step arm pushes the two `add` operands (the recursive result and
            // the other operand) in their recognised order, then calls `add`.
            let rec_ops = format!(
                ".localGet 0, .i64Const 1, .call {box_idx}, .call {sub_idx}, .call {self_idx}"
            );
            let other_ops = match other {
                BodyOperand::Input => ".localGet 0".to_string(),
                BodyOperand::Const(k) => format!(".i64Const {}, .call {box_idx}", lean_int_lit(*k)),
            };
            let (a_ops, b_ops) = if *rec_first {
                (&rec_ops, &other_ops)
            } else {
                (&other_ops, &rec_ops)
            };
            let step = format!("{a_ops}, {b_ops}, .call {add_idx}");
            format!(
                "fun fn =>\n  \
                 if fn = {self_idx} then some ⟨1, {nlocals},\n    \
                 [ .localGet 0, .localSet 1,\n      \
                 .localGet 1, .structGet {carrier} 1, .refIsNull,\n      \
                 .ifElse [.localGet 1, .structGet {carrier} 0, .i64Const 0, .i64LeS]\n              \
                 [.localGet 1, .structGet {carrier} 2, .i32Const 0, .i32LtS],\n      \
                 .ifElse [.i64Const {base}, .call {box_idx}]\n              \
                 [{step}] ]⟩\n  else none",
            )
        }
        Cert::AccumulatorRecursive {
            self_idx,
            nlocals,
            carrier,
            box_idx,
            add_idx,
            sub_idx,
            ..
        } => format!(
            "fun fn =>\n  \
             if fn = {self_idx} then some ⟨2, {nlocals},\n    \
             [ .localGet 0, .localSet 2,\n      \
             .localGet 2, .structGet {carrier} 1, .refIsNull,\n      \
             .ifElse [.localGet 2, .structGet {carrier} 0, .i64Const 0, .i64LeS]\n              \
             [.localGet 2, .structGet {carrier} 2, .i32Const 0, .i32LtS],\n      \
             .ifElse [.localGet 1]\n              \
             [.localGet 0, .i64Const 1, .call {box_idx}, .call {sub_idx}, \
             .localGet 1, .localGet 0, .call {add_idx}, .returnCall {self_idx}] ]⟩\n  else none",
        ),
        Cert::AdtConstructor {
            self_idx,
            nlocals,
            ops,
            ..
        }
        | Cert::FieldProjection {
            self_idx,
            nlocals,
            ops,
            ..
        }
        | Cert::WidenedIntMatch {
            self_idx,
            nlocals,
            ops,
            ..
        }
        | Cert::VerbatimWidenedMatch {
            self_idx,
            nlocals,
            ops,
            ..
        }
        | Cert::VerbatimVariantDispatch {
            self_idx,
            nlocals,
            ops,
            ..
        }
        | Cert::StringEqVerbatimMatch {
            self_idx,
            nlocals,
            ops,
            ..
        }
        | Cert::StringConcatVerbatimMatch {
            self_idx,
            nlocals,
            ops,
            ..
        }
        | Cert::ExprFragment {
            self_idx,
            nlocals,
            ops,
            ..
        }
        | Cert::VariantDispatch {
            self_idx,
            nlocals,
            ops,
            ..
        } => format!(
            "fun fn =>\n  \
             if fn = {self_idx} then some ⟨{arity}, {nlocals}, {body}⟩ else none",
            arity = c.arity(),
            body = render_ops_value(ops),
        ),
        Cert::Composition { closure, .. } => render_closure_code_value(closure),
        // Shared code table over the whole SCC: one `if fn = self then …` arm per
        // member, in `self_idx` order. Each arm is the fixed mutual body shape
        // (base boxes the member's literal; the else tail-calls the member's
        // cross target on `n-1`) — the same lossless template `Cert::Recursive`
        // uses, so it is byte-identical to the recognised bytes and to the term
        // the checker re-derives.
        Cert::MutualRecursion {
            scc,
            carrier,
            box_idx,
            sub_idx,
            ..
        } => {
            let mut s = String::from("fun fn =>\n  ");
            for (i, m) in scc.iter().enumerate() {
                let kw = if i == 0 { "if" } else { "else if" };
                let base = lean_int_lit(m.base_k);
                s.push_str(&format!(
                    "{kw} fn = {self_idx} then some ⟨1, {nlocals},\n    \
                     [ .localGet 0, .localSet 1,\n      \
                     .localGet 1, .structGet {carrier} 1, .refIsNull,\n      \
                     .ifElse [.localGet 1, .structGet {carrier} 0, .i64Const 0, .i64LeS]\n              \
                     [.localGet 1, .structGet {carrier} 2, .i32Const 0, .i32LtS],\n      \
                     .ifElse [.i64Const {base}, .call {box_idx}]\n              \
                     [.localGet 0, .i64Const 1, .call {box_idx}, .call {sub_idx}, .returnCall {cross}] ]⟩\n  ",
                    self_idx = m.self_idx,
                    nlocals = m.nlocals,
                    cross = m.cross_idx,
                ));
            }
            s.push_str("else none");
            s
        }
        Cert::NonRecursive { .. } => unreachable!(),
    }
}

/// The multi-entry `CodeTbl` VALUE for a composition: one `if fn = i then …`
/// arm per function in the caller's whole call closure, in `self_idx` order.
/// The checker re-derives this from the bytes and pins the WHOLE table with one
/// `rfl`, so every callee body the caller's proof reduces through is byte-bound.
fn render_closure_code_value(closure: &[ClosureEntry]) -> String {
    let mut s = String::from("fun fn =>\n  ");
    for (i, e) in closure.iter().enumerate() {
        let kw = if i == 0 { "if" } else { "else if" };
        s.push_str(&format!(
            "{kw} fn = {idx} then some ⟨1, {nlocals}, {body}⟩\n  ",
            idx = e.self_idx,
            nlocals = e.nlocals,
            body = render_ops_value(&e.ops),
        ));
    }
    s.push_str("else none");
    s
}

/// The `Obligation.host` builder VALUE for a certified body, FULLY EXPANDED to
/// the box/add/sub wiring on the byte-derived indices — deliberately NOT a
/// reference to `CertModule.{name}Host` (which an attacker edits). The checker
/// splices this and pins it with `rfl` against `manifest.obligations.map
/// (·.host)`, so a nerfed host (e.g. `fun _ _ _ => none`, which would make
/// `holds` vacuous even with an honest `code`) fails the kernel witness.
/// Definitionally equal to the honest `render_host_def` builder.
fn render_host_value(c: &Cert) -> String {
    match c.inner() {
        Cert::StraightLine {
            carrier,
            box_idx,
            add_idx,
            ..
        } => format!(
            "fun add _ _ _ _ => fun fn =>\n    \
             if fn = {box_idx} then some (1, boxRef {carrier})\n    \
             else if fn = {add_idx} then some (2, add)\n    else none",
        ),
        Cert::Recursive {
            carrier,
            box_idx,
            add_idx,
            sub_idx,
            combinator,
            ..
        } => {
            let cp = combinator.param();
            format!(
                "fun add sub mul _ _ => fun fn =>\n    \
                 if fn = {box_idx} then some (1, boxRef {carrier})\n    \
                 else if fn = {add_idx} then some (2, {cp})\n    \
                 else if fn = {sub_idx} then some (2, sub)\n    else none",
            )
        }
        Cert::AccumulatorRecursive {
            carrier,
            box_idx,
            add_idx,
            sub_idx,
            ..
        } => format!(
            "fun add sub _ _ _ => fun fn =>\n    \
             if fn = {box_idx} then some (1, boxRef {carrier})\n    \
             else if fn = {add_idx} then some (2, add)\n    \
             else if fn = {sub_idx} then some (2, sub)\n    else none",
        ),
        Cert::AdtConstructor { .. }
        | Cert::FieldProjection { .. }
        | Cert::VerbatimWidenedMatch { .. }
        | Cert::VerbatimVariantDispatch { .. }
        | Cert::ExprFragment { .. } => "fun _ _ _ _ _ => fun _ => none".to_string(),
        Cert::StringEqVerbatimMatch { string_eq_idx, .. } => format!(
            "fun _ _ _ stringEq _ => fun fn =>\n    \
             if fn = {string_eq_idx} then some (2, stringEq)\n    else none"
        ),
        Cert::StringConcatVerbatimMatch {
            string_concat_idx,
            result_ty,
            ..
        } => format!(
            "fun _ _ _ _ stringConcat => fun fn =>\n    \
             if fn = {string_concat_idx} then some (1, stringConcat {result_ty})\n    else none"
        ),
        Cert::WidenedIntMatch {
            carrier, box_idx, ..
        } => format!(
            "fun _ _ _ _ _ => fun fn =>\n    \
             if fn = {box_idx} then some (1, boxRef {carrier})\n    else none",
        ),
        Cert::VariantDispatch {
            carrier,
            box_idx,
            add_idx,
            sub_idx,
            ..
        } => {
            let a = if add_idx.is_some() { "add" } else { "_" };
            let s = if sub_idx.is_some() { "sub" } else { "_" };
            let mut chain = format!("if fn = {box_idx} then some (1, boxRef {carrier})");
            if let Some(i) = add_idx {
                chain.push_str(&format!("\n    else if fn = {i} then some (2, add)"));
            }
            if let Some(i) = sub_idx {
                chain.push_str(&format!("\n    else if fn = {i} then some (2, sub)"));
            }
            format!("fun {a} {s} _ _ _ => fun fn =>\n    {chain}\n    else none")
        }
        Cert::Composition { closure, .. } => {
            format!(
                "fun add _sub _ _ _ => fun fn =>\n    {}",
                compose_host_arms(closure)
            )
        }
        // Fully-expanded shared host (box + sub), definitionally equal to the
        // obligation's `fun _ sub _ => CertModule.{primary}Host sub`.
        Cert::MutualRecursion {
            carrier,
            box_idx,
            sub_idx,
            ..
        } => format!(
            "fun _ sub _ _ _ => fun fn =>\n    \
             if fn = {box_idx} then some (1, boxRef {carrier})\n    \
             else if fn = {sub_idx} then some (2, sub)\n    else none",
        ),
        Cert::NonRecursive { .. } => unreachable!(),
    }
}

/// The host-table arms for a composition closure: each carrier-`add` helper the
/// closure calls wired to the `add` contract parameter, terminated by `none`.
/// v1 leaves consume only `add`; the arms grow with the leaf vocabulary.
fn compose_host_arms(closure: &[ClosureEntry]) -> String {
    let mut adds: Vec<u32> = closure
        .iter()
        .filter_map(|e| match e.shape {
            LeafShape::SelfSum { add_idx } => Some(add_idx),
            LeafShape::Chain { .. } => None,
        })
        .collect();
    adds.sort_unstable();
    adds.dedup();
    let mut s = String::new();
    for (i, a) in adds.iter().enumerate() {
        let kw = if i == 0 { "if" } else { "else if" };
        s.push_str(&format!("{kw} fn = {a} then some (2, add)\n    "));
    }
    s.push_str("else none");
    s
}

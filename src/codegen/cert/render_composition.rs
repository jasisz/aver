/// Post-order (callees-before-callers) topological order of a composition
/// closure, starting the DFS at the caller so the caller comes last. Every
/// closure is an acyclic user-call DAG (enforced by `collect_closure`).
fn compose_topo_order(caller_idx: u32, closure: &[ClosureEntry]) -> Vec<u32> {
    let by_idx: std::collections::HashMap<u32, &ClosureEntry> =
        closure.iter().map(|e| (e.self_idx, e)).collect();
    let mut order = Vec::new();
    let mut seen = std::collections::HashSet::new();
    fn dfs(
        idx: u32,
        by_idx: &std::collections::HashMap<u32, &ClosureEntry>,
        seen: &mut std::collections::HashSet<u32>,
        order: &mut Vec<u32>,
    ) {
        if !seen.insert(idx) {
            return;
        }
        if let Some(e) = by_idx.get(&idx)
            && let LeafShape::Chain { calls } = &e.shape
        {
            for c in calls {
                dfs(*c, by_idx, seen, order);
            }
        }
        order.push(idx);
    }
    dfs(caller_idx, &by_idx, &mut seen, &mut order);
    order
}

/// Evaluate a closure entry's integer model on a concrete input (for the
/// anti-vacuity `native_decide` guard values). Mirrors the leaf models exactly.
fn compose_eval(idx: u32, x: i64, by_idx: &std::collections::HashMap<u32, &ClosureEntry>) -> i64 {
    match by_idx.get(&idx).map(|e| &e.shape) {
        Some(LeafShape::SelfSum { .. }) => x + x,
        Some(LeafShape::Chain { calls }) => {
            let mut acc = x;
            for c in calls {
                acc = compose_eval(*c, acc, by_idx);
            }
            acc
        }
        None => x,
    }
}

/// Longest chain of code-calls from `idx` down to a leaf (fuel budget for the
/// `native_decide` guards: each level burns one unit in `wFuncN`).
fn compose_depth(idx: u32, by_idx: &std::collections::HashMap<u32, &ClosureEntry>) -> usize {
    match by_idx.get(&idx).map(|e| &e.shape) {
        Some(LeafShape::Chain { calls }) => {
            1 + calls
                .iter()
                .map(|c| compose_depth(*c, by_idx))
                .max()
                .unwrap_or(0)
        }
        _ => 1,
    }
}

/// The cross-function composition certificate: a simulation lemma per closure
/// entry over the caller's SHARED code table (callee lemmas first, the caller's
/// `_wasm_certified` last), the anti-vacuity guards, and the schema obligation.
/// Content-blind: the only per-function inputs are DATA (the closure entries,
/// their call indices and model names), never a hand-tuned proof.
fn render_composition_cert(c: &Cert) -> String {
    let Cert::Composition {
        name,
        self_idx,
        carrier,
        closure,
        ..
    } = c
    else {
        unreachable!()
    };
    let by_idx: std::collections::HashMap<u32, &ClosureEntry> =
        closure.iter().map(|e| (e.self_idx, e)).collect();
    let lemma_name = |idx: u32| -> String {
        if idx == *self_idx {
            format!("{name}_wasm_certified")
        } else {
            format!("{name}__sim_{idx}")
        }
    };
    let sig = |concl_model: &str| -> String {
        format!(
            "    (S : CarrierSpec {carrier}) (add sub : List WVal → Option WVal)\n\
             \x20   (hadd : ∀ a b va vb w, S.Repr a va → S.Repr b vb → add [va, vb] = some w → S.Repr (a + b) w)\n\
             \x20   (hsub : ∀ a b va vb w, S.Repr a va → S.Repr b vb → sub [va, vb] = some w → S.Repr (a - b) w) :\n\
             \x20   ∀ (fuel : Nat) (x : Int) (v w : WVal), S.Repr x v →\n\
             \x20     wFuncN {name}Code ({name}Host add sub) fuel {{IDX}} [v] = some w → S.Repr ({concl_model}) w"
        )
    };

    let mut s = format!(
        "/-! ### {name} — cross-function composition certificate (carrier type {carrier}) -/\n\n"
    );

    for idx in compose_topo_order(*self_idx, closure) {
        let e = by_idx[&idx];
        let head = format!(
            "theorem {}\n{}",
            lemma_name(idx),
            sig(&format!("{} x", e.name))
        )
        .replace("{IDX}", &idx.to_string());
        match &e.shape {
            LeafShape::SelfSum { .. } => {
                s.push_str(&format!(
                    "-- callee `{ename}`: self-sum leaf, over the shared closure table.\n{head} := by\n  \
                     intro fuel x v w hv hrun\n  \
                     cases fuel with\n  \
                     | zero => simp only [wFuncN, reduceCtorEq] at hrun\n  \
                     | succ f =>\n      \
                     rcases hc : add [v, v] with _ | r <;>\n        \
                     simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, popArgs, initLocals, hc] at hrun\n      \
                     subst hrun\n      \
                     exact hadd x x v v r hv hv hc\n\n",
                    ename = e.name,
                ));
            }
            LeafShape::Chain { calls } => {
                let mut body = String::new();
                // one `rcases … <;> simp … at hrun` per call site (threading m1, m2, …).
                for (i, c_idx) in calls.iter().enumerate() {
                    let arg = if i == 0 {
                        "[v]".to_string()
                    } else {
                        format!("[m{i}]")
                    };
                    body.push_str(&format!(
                        "      rcases h{h} : wFuncN {name}Code ({name}Host add sub) f {c_idx} {arg} with _ | m{h} <;>\n        \
                         simp [wFuncN, wRunF, {name}Code, {name}Host, popArgs, initLocals, h{h}] at hrun\n",
                        h = i + 1,
                    ));
                }
                body.push_str("      subst hrun\n");
                // cite the callee simulation lemma at each site, threading the model.
                let mut model_arg = "x".to_string();
                for (i, c_idx) in calls.iter().enumerate() {
                    let (vin, hrepr) = if i == 0 {
                        ("v".to_string(), "hv".to_string())
                    } else {
                        (format!("m{i}"), format!("r{i}"))
                    };
                    body.push_str(&format!(
                        "      have r{h} := {lem} S add sub hadd hsub f ({model_arg}) {vin} m{h} {hrepr} h{h}\n",
                        h = i + 1,
                        lem = lemma_name(*c_idx),
                    ));
                    model_arg = format!("{} ({})", by_idx[c_idx].name, model_arg);
                }
                body.push_str(&format!("      exact r{}\n\n", calls.len()));
                s.push_str(&format!(
                    "-- {label} `{ename}`: unary user-call chain; cites each callee lemma.\n{head} := by\n  \
                     intro fuel x v w hv hrun\n  \
                     cases fuel with\n  \
                     | zero => simp only [wFuncN, reduceCtorEq] at hrun\n  \
                     | succ f =>\n{body}",
                    ename = e.name,
                    label = if idx == *self_idx { "caller" } else { "callee" },
                ));
            }
        }
    }

    s.push_str(&format!("#print axioms {name}_wasm_certified\n\n"));

    // anti-vacuity guards: run the whole closure on concrete inputs.
    let g_fuel = compose_depth(*self_idx, &by_idx) + 2;
    let g3 = compose_eval(*self_idx, 3, &by_idx);
    let gm5 = compose_eval(*self_idx, -5, &by_idx);
    s.push_str(&format!(
        "-- anti-vacuity: the emitted closure actually RUNS on concrete inputs.\n\
         def {name}HostRef : HostTbl := {name}Host (addRef {carrier}) (subRef {carrier})\n\
         example :\n    \
         ((wFuncN {name}Code {name}HostRef {g_fuel} {self_idx} [carrierSmall {carrier} 3]).bind carrierToInt) = some ({g3}) := by\n  \
         native_decide\n\
         example :\n    \
         ((wFuncN {name}Code {name}HostRef {g_fuel} {self_idx} [carrierSmall {carrier} (-5)]).bind carrierToInt) = some ({gm5}) := by\n  \
         native_decide\n\n"
    ));

    // the schema obligation: bridge the caller lemma to `Obligation.holds`.
    s.push_str(&format!(
        "/-- Schema-shaped simulation obligation for `{name}` (composed by the single\n\
        \x20   final theorem): the emitted body simulates `{name}` by citing its callees. -/\n\
         theorem {name}_simulates : AverCert.Schema.Obligation.holds {name}Ob := by\n  \
         intro S add sub mul stringEq stringConcat hadd hsub hmul hStringEq hStringConcat fuel ns vs w hrepr hrun\n  \
         simp only [{name}Ob, AverCert.Schema.Obligation.holds] at hrun ⊢\n  \
         obtain ⟨hrepr, harity⟩ := hrepr\n  \
         cases hrepr with\n  \
         | nil => simp at harity\n  \
         | cons hv htail =>\n    \
         rename_i n v ns vs\n    \
         cases htail with\n    \
         | nil =>\n      \
         simpa [AverCert.Schema.intRepr] using\n        \
         {name}_wasm_certified S add sub hadd hsub fuel n v w hv hrun\n    \
         | cons _ _ => simp at harity\n"
    ));

    s
}

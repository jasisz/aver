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

fn composition_member_claim_lean_value(entry: &ClosureEntry) -> String {
    format!(
        "({{ exportNameBytes := {}, exportName := {}, \
         plan := AverCert.Plans.{}CompositionPlan }} : \
         AverCert.AcceptedArtifact.CompositionMemberClaim)",
        render_byte_list(entry.name.as_bytes()),
        lean_str(&entry.name),
        entry.name,
    )
}

fn composition_members_lean_value(closure: &[ClosureEntry]) -> String {
    format!(
        "[{}]",
        closure
            .iter()
            .map(composition_member_claim_lean_value)
            .collect::<Vec<_>>()
            .join(", ")
    )
}

fn composition_claim_lean_value(c: &Cert, add_idx: u32) -> String {
    let Cert::Composition {
        name,
        carrier,
        closure,
        ..
    } = c.inner()
    else {
        unreachable!()
    };
    let member_names = closure
        .iter()
        .map(|entry| lean_str(&entry.name))
        .collect::<Vec<_>>()
        .join(", ");
    format!(
        "({{ exportName := {}, carrier := {carrier}, hostTable := {}, \
         memberNames := [{member_names}], obligation := AverCert.{name}Ob }} : \
         AverCert.AcceptedArtifact.CompositionClaim)",
        lean_str(name),
        composition_host_table_lean_value(add_idx),
    )
}

fn composition_claim_acceptance_proof(
    c: &Cert,
    strict: FragHostTable,
) -> String {
    let Cert::Composition {
        carrier, closure, ..
    } = c.inner()
    else {
        unreachable!()
    };
    let plans = composition_plans_from_cert(c, strict)
        .expect("audited composition has byte-derived member plans");
    let add_idx = strict
        .add_idx
        .expect("plan-backed composition has strict add host");
    let funcs = composition_func_table(closure);
    let mut named_proof = "trivial".to_string();
    for (entry, plan) in plans.iter().rev() {
        let body = render_ops_value(
            &lower_composition_plan(plan, add_idx, &funcs)
                .expect("composition member lowers against closure table"),
        );
        let code_entry = render_byte_list(
            &composition_code_entry_bytes(plan, *carrier, add_idx, &funcs)
                .expect("composition member byte-lowers against closure table"),
        );
        let binding = format!(
            "({{ funcIdx := {}, codeIdx := {}, typeIdx := {}, codeEntry := {} }} : \
             AverCert.WasmSlice.FuncBinding)",
            entry.self_idx, entry.code_idx, entry.type_idx, code_entry
        );
        let member_proof = format!(
            "⟨rfl, ⟨({body}), ({code_entry}), {binding}, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩"
        );
        named_proof = format!("⟨{member_proof}, {named_proof}⟩");
    }
    format!("⟨rfl, ⟨rfl, ⟨rfl, ⟨rfl, ⟨rfl, ⟨rfl, {named_proof}⟩⟩⟩⟩⟩⟩")
}

/// Option-(b) composition bridge. Root glue is discharged by the audited
/// generic; generated lemmas remain only for the non-root member semantics
/// that the generic deliberately consumes through `MemberFact`.
fn render_composition_semantic_bridge(c: &Cert, analysis: &Analysis) -> String {
    let Cert::Composition {
        name,
        self_idx,
        carrier,
        closure,
        ..
    } = c.inner()
    else {
        unreachable!()
    };
    let add_idx = analysis
        .frag_host_table
        .add_idx
        .expect("composition bridge has strict add host");
    let by_idx: std::collections::HashMap<u32, &ClosureEntry> =
        closure.iter().map(|e| (e.self_idx, e)).collect();
    let root = by_idx[self_idx];
    let LeafShape::Chain { calls: root_calls } = &root.shape else {
        unreachable!("composition root is a chain")
    };
    let callees = root_calls
        .iter()
        .map(|idx| lean_str(&by_idx[idx].name))
        .collect::<Vec<_>>();
    let callees_lean = format!("[{}]", callees.join(", "));
    let lemma_name = |idx: u32| -> String { format!("{name}__compositionMember_{idx}") };
    let model_name = format!("{name}CompositionModel");
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
        "/-! ### {name} — option-(b) composition semantic bridge (carrier type {carrier}) -/\n\n"
    );
    s.push_str(&format!(
        "def {model_name} (member : String) (x : Int) : Int :=\n  {}\n\n",
        closure
            .iter()
            .map(|entry| format!(
                "if member = {} then {} x else",
                lean_str(&entry.name),
                entry.name
            ))
            .chain(std::iter::once("x".to_string()))
            .collect::<Vec<_>>()
            .join(" ")
    ));
    let model_simp = closure
        .iter()
        .map(|entry| entry.name.as_str())
        .collect::<Vec<_>>()
        .join(", ");

    for idx in compose_topo_order(*self_idx, closure) {
        if idx == *self_idx {
            continue;
        }
        let e = by_idx[&idx];
        let head = format!(
            "theorem {}\n{}",
            lemma_name(idx),
            sig(&format!("{model_name} {} x", lean_str(&e.name)))
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
                     simpa [{model_name}, {model_simp}] using hadd x x v v r hv hv hc\n\n",
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
                    model_arg = format!(
                        "{model_name} {} ({model_arg})",
                        lean_str(&by_idx[c_idx].name)
                    );
                }
                body.push_str(&format!(
                    "      simpa [{model_name}, {model_simp}] using r{}\n\n",
                    calls.len()
                ));
                s.push_str(&format!(
                    "-- callee `{ename}`: unary user-call chain; cites each member lemma.\n{head} := by\n  \
                     intro fuel x v w hv hrun\n  \
                     cases fuel with\n  \
                     | zero => simp only [wFuncN, reduceCtorEq] at hrun\n  \
                     | succ f =>\n{body}",
                    ename = e.name,
                ));
            }
        }
    }

    let members = composition_members_lean_value(closure);
    let all_members = format!(
        "[{}]",
        composition_member_plans(analysis)
            .iter()
            .map(|(entry, _)| composition_member_claim_lean_value(entry))
            .collect::<Vec<_>>()
            .join(", ")
    );
    let claim = composition_claim_lean_value(c, add_idx);
    let acceptance = composition_claim_acceptance_proof(c, analysis.frag_host_table);
    s.push_str(&format!(
        "def {name}CompositionMembers : List AverCert.AcceptedArtifact.CompositionMemberClaim :=\n  \
         {members}\n\n\
         def {name}CompositionClaim : AverCert.AcceptedArtifact.CompositionClaim :=\n  \
         {claim}\n\n\
         theorem {name}_compositionClaimAccepted :\n    \
         AverCert.AcceptedArtifact.compositionClaimAccepted\n      \
         AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen\n      \
         {name}CompositionMembers {name}CompositionClaim := by\n  \
         dsimp [{name}CompositionMembers, {name}CompositionClaim,\n    \
         AverCert.AcceptedArtifact.compositionClaimAccepted,\n    \
         AverCert.AcceptedArtifact.compositionFuncTable,\n    \
         AverCert.AcceptedArtifact.compositionMemberBinding,\n    \
         AverCert.AcceptedArtifact.compositionNamedMembersAccepted,\n    \
         AverCert.AcceptedArtifact.compositionMemberPlanAccepted,\n    \
         AverCert.AcceptedArtifact.compositionMemberForName,\n    \
         AverCert.AcceptedArtifact.compositionClosureBound,\n    \
         AverCert.AcceptedArtifact.compositionEdges,\n    \
         AverCert.AcceptedArtifact.compositionPlanCallees,\n    \
         AverCert.AcceptedArtifact.compositionEdgesDescend,\n    \
         AverCert.AcceptedArtifact.compositionReachClosure,\n    \
         AverCert.AcceptedArtifact.compositionReachStep,\n    \
         AverCert.AcceptedArtifact.compositionEdgeLookup,\n    \
         AverCert.AcceptedArtifact.stringListNodup,\n    \
         AverCert.AcceptedArtifact.stringListSetEq]\n  \
         exact {acceptance}\n\n"
    ));

    let direct_callee = by_idx[root_calls
        .first()
        .expect("composition root has a direct callee")];
    let direct_member = composition_member_claim_lean_value(direct_callee);
    let direct_plan = composition_plan_for_entry(
        direct_callee,
        &closure
            .iter()
            .map(|entry| (entry.self_idx, entry.name.clone()))
            .collect(),
    )
    .expect("direct composition member has a plan");
    let funcs = composition_func_table(closure);
    let direct_body = render_ops_value(
        &lower_composition_plan(&direct_plan, add_idx, &funcs)
            .expect("direct composition member lowers"),
    );
    let direct_code_entry = render_byte_list(
        &composition_code_entry_bytes(&direct_plan, *carrier, add_idx, &funcs)
            .expect("direct composition member byte-lowers"),
    );
    let direct_binding = format!(
        "({{ funcIdx := {}, codeIdx := {}, typeIdx := {}, codeEntry := {} }} : \
         AverCert.WasmSlice.FuncBinding)",
        direct_callee.self_idx,
        direct_callee.code_idx,
        direct_callee.type_idx,
        direct_code_entry,
    );
    s.push_str(&format!(
        "theorem {name}_compositionSemanticBridge :\n    \
         V3Master.compositionClaimSemanticBridge\n      \
         ({{ modBytes := AverCert.ArtifactBytes.modBytes,\n         \
         modLen := AverCert.ArtifactBytes.modLen, manifest := AverCert.manifest,\n         \
         symFragmentClaims := [], stringEqClaims := [], stringConcatClaims := [],\n         \
         constructClaims := [], recursionClaims := [], mutualRecursionClaims := [],\n         \
         verbatimClaims := [], intDispatchClaims := [], fieldProjectionClaims := [],\n         \
         compositionMembers := {all_members},\n         \
         compositionClaims := [{name}CompositionClaim], closureFuel := 0,\n         \
         closureClaim := {{ roots := [], helpers := [], admitted := [] }} }} :\n        \
         AverCert.AcceptedArtifact.ArtifactData)\n      \
         {name}CompositionClaim {callees_lean} := by\n  \
         refine ⟨rfl, ?_⟩\n  \
         intro S add sub mul stringEq stringConcat\n    \
         hAdd hSub hMul hStringEq hStringConcat ns vs hDom\n  \
         dsimp [{name}CompositionClaim, AverCert.{name}Ob] at ns vs hDom ⊢\n  \
         rcases hDom with ⟨hRepr, hLen⟩\n  \
         cases hRepr with\n  \
         | nil => simp at hLen\n  \
         | cons hv htail =>\n      \
         rename_i n v ns' vs'\n      \
         cases htail with\n      \
         | cons _ _ => simp at hLen\n      \
         | nil =>\n          \
         refine ⟨n, v, {model_name}, {name}, rfl, hv, ?_, ?_, ?_⟩\n          \
         · intro input\n            \
         simp [{model_name}, V3Composition.evalCompositionCalls, {model_simp}]\n          \
         · intro w hw\n            \
         simpa [AverCert.Schema.intRepr] using hw\n          \
         · intro funcTable hTable member hMember\n            \
         have hMember' : member = {} := by simpa using hMember\n            \
         subst member\n            \
         exact ⟨⟨\n              \
         {direct_member},\n              \
         by rfl,\n              \
         {},\n              \
         V3Master.compositionFuncIdx_eq_binding\n                \
         AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen\n                \
         {all_members} funcTable {} {direct_member}\n                \
         {direct_binding} hTable (by rfl) (by rfl),\n              \
         by simp [CertModule.{name}Host],\n              \
         {direct_body},\n              \
         by rfl,\n              \
         {} S add sub hAdd hSub\n            \
         ⟩⟩\n\n\
         #print axioms {name}_compositionSemanticBridge\n",
        lean_str(&direct_callee.name),
        direct_callee.self_idx,
        lean_str(&direct_callee.name),
        lemma_name(direct_callee.self_idx),
    ));

    s
}

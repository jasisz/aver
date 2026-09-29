// Included from source_bridges.rs, under the engine feature.
// Export assembly is independent of the artifact-wide tables: its premises
// are the steps of this export's call closure and its own source image.
// BridgeProof binds those premises to the authoritative plans afterwards.

const BRIDGE_ASSEMBLY_MODULE: &str = "BridgeAssembly";

fn export_split_cases(b: &BridgedFn) -> String {
    // Split all encoded sums, including nested ones, on every goal.
    let splits: Vec<_> = b.params.iter().enumerate()
        .filter_map(|(i, p)| rcases_pattern(p).map(|pat| format!("rcases x{i} with {pat}")))
        .collect();
    if splits.is_empty() { String::new() } else { format!("{}; ", splits.join(" <;> ")) }
}

fn render_export_assembly(bridge: &SourceBridge, f: u32, plan: &BridgePlan, s: &mut String) {
    let b = &plan.fns[&f];
    let closure = closure_of(f, &plan.fns);
    let members = closure.iter().map(u32::to_string).collect::<Vec<_>>().join(", ");
    let steps = render_steps_proof(&closure);
    let mut hypotheses = String::new();
    for g in &closure {
        let callees = plan.fns[g].callees.iter().map(u32::to_string).collect::<Vec<_>>().join(", ");
        hypotheses.push_str(&format!(
            "\n    (step_{g} : AverCert.GrammarBridge.Step fns I [{callees}] {g})"
        ));
    }
    if bridge.kind == BridgeKind::Exact {
        // No program-wide depth table: changing an unrelated call graph
        // cannot invalidate this export's closure assembly.
        s.push_str(&format!("def depth_{f} : _root_.Nat → _root_.Nat := fun g =>\n  match g with\n"));
        for g in &closure { s.push_str(&format!("  | {g} => {}\n", plan.depth[g])); }
        s.push_str("  | _ => 0\n\n");
    }
    let binders = param_binders(&b.params);
    let names = binder_names(b.params.len());
    let intros = if names.is_empty() { String::new() } else { format!(" {}", names.join(" ")) };
    let mut fresh = 0;
    let args = crate::bridge_statement::encoded_args(&b.params, &mut fresh);
    let result = b.result.encode(
        &crate::bridge_statement::source_call(&b.model, b.params.len()), &mut fresh,
    );
    let model_at = format!("AverCert.AcceptedArtifact.modelOf fns fuel {f} {args}");
    let split_cases = export_split_cases(b);
    let image_simps = format!(
        "I_{f}, dec_{f}, img_{f}, AverCert.GrammarBridge.decodeStr_strBytes, \
         decListInt_enc, decListBool_enc, decListString_enc"
    );
    let image = format!(
        "(by {split_cases}all_goals first | rfl | (simp [{image_simps}]; done) | \
         (simp [{image_simps}] <;> (repeat' apply And.intro) <;> rfl))"
    );
    let (statement, proof) = match bridge.kind {
        BridgeKind::Exact => {
            let result_at = format!("{model_at} = _root_.Option.some ({result})");
            let result_at = if binders.is_empty() { result_at } else { format!("∀ {binders}, {result_at}") };
            (
                format!("∃ (k : _root_.Nat), ∀ (fuel : _root_.Nat), k ≤ fuel → {result_at}"),
                format!(
                    "refine ⟨{}, ?_⟩; intro fuel hk{intros}; \
                     exact AverCert.GrammarBridge.exact_of_step fns I [{members}] depth_{f} \
                     {steps} fuel {f} (by decide) \
                     (Nat.lt_of_lt_of_le (by decide +kernel) hk) _ _ {image}",
                    plan.depth[&f] + 1,
                ),
            )
        }
        BridgeKind::Adequate => (
            format!(
                "∀ (fuel : _root_.Nat) {binders} (v : AverCert.Grammar.SVal), \
                 {model_at} = _root_.Option.some v → v = {result}"
            ),
            format!(
                "intro fuel{intros} v h; \
                 exact AverCert.GrammarBridge.bridge_of_step fns I [{members}] \
                 {steps} fuel {f} (by decide) _ v _ h {image}"
            ),
        ),
    };
    s.push_str(&format!(
        "/-- Assemble the call-closure proof of `{}` independently of any artifact. -/\n\
         theorem assembly_{f} (fns : _root_.List AverCert.Schema.FnEntry)\n    \
           (I : AverCert.GrammarBridge.Table)\n    \
           (I_{f} : ∀ a, I {f} a = (dec_{f} a).map img_{f}){hypotheses} :\n    \
           {statement} := by\n  first\n  \
         | (set_option maxHeartbeats {EXPORT_HEARTBEATS} in\n      ({proof}))\n  \
         | sorry\n\n",
        b.model,
    ));
}

fn export_assembly_binding(f: u32, plan: &BridgePlan) -> String {
    let steps: String = closure_of(f, &plan.fns).iter().map(|g| format!(" step_{g}")).collect();
    format!("exact assembly_{f} AverCert.Plans.fnPlans I I_{f}{steps}")
}

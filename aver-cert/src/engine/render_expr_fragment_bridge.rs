/// Integer and Bool source fragments are within the audited symbolic fragment
/// grammar. Float fragments are deliberately excluded: their bit-level source
/// models remain bespoke because floating-point semantics are outside the
/// integer/Bool model.
fn expr_fragment_uses_audited_generic(c: &Cert) -> bool {
    let Cert::ExprFragment {
        source_plan: Some(source_plan),
        plan,
        ..
    } = c.inner()
    else {
        return false;
    };
    source_plan
        .params
        .iter()
        .all(|ty| matches!(ty, SymTy::Int | SymTy::Bool))
        && matches!(source_plan.result, SymTy::Int | SymTy::Bool)
        && plan
            .params
            .iter()
            .all(|ty| matches!(ty, FragTy::IntCarrier | FragTy::BoolI32))
        && matches!(plan.result, FragTy::IntCarrier | FragTy::BoolI32)
}

fn expr_fragment_source_model(c: &Cert, model_info: &ModelInfo) -> String {
    debug_assert!(expr_fragment_uses_audited_generic(c));
    let model_name = c.model_lean_name(model_info);
    match c.arity() {
        1 => model_name,
        // The obligation domain is the right-nested product
        // `FragParams.denote` builds, so the model uncurries the source
        // function over the same `p.1, p.2.1, …, p.2…2` accessors the
        // obligation emitter uses (`expr_fragment_dom_accessor`).
        arity => {
            let args = (0..arity)
                .map(|index| format!(" {}", expr_fragment_dom_accessor("p", index, arity)))
                .collect::<String>();
            format!("fun p => {model_name}{args}")
        }
    }
}

fn expr_fragment_claim_lean_value(
    c: &Cert,
    host_table: FragHostTable,
    struct_table_lean: &str,
) -> String {
    debug_assert!(expr_fragment_uses_audited_generic(c) || c.tag_dispatch_face().is_some());
    let name = c.name();
    format!(
        "({{ exportNameBytes := {}, exportName := {}, carrier := {}, \
         hostTable := {}, structTable := {struct_table_lean}, \
         plan := AverCert.Plans.{name}SymPlan, obligation := AverCert.{name}Ob }} : \
         AverCert.AcceptedArtifact.SymFragmentClaim)",
        render_byte_list(name.as_bytes()),
        lean_str(name),
        c.carrier(),
        host_table.lean_value(),
    )
}

/// Render the `{name}_exprFragmentClaimAccepted` theorem as a SPLIT proof:
/// witness data (the lowered `WInstr` body, code-entry bytes, function binding)
/// as named `def`s and each acceptance conjunct as its OWN leaf theorem, then an
/// aggregate that combines the already-checked constants. This mirrors
/// `render_sym_claim_bundles` in `render_project.rs` and exists for the SAME
/// reason: emitting one monolithic `exact ⟨…giant nested tuple…⟩` submits the
/// whole witness to the kernel in a single `addDecl` and holds it live, which
/// peaked `Certificate.lean` at ~6.5 GB — co-equal with, and independent of, the
/// artifact root's own peak. Splitting each piece into its own declaration drops
/// the peak to the largest single leaf (a `modBytes` decode / type-section walk,
/// each ≤ ~1.2 GB). The leaf conjuncts are stated over `AverCert.Plans.{name}Plan`
/// (the encoded plan), definitionally the value the aggregate reduces to, so the
/// aggregate re-runs no heavy reduction.
///
/// `claim_target` is what `symFragmentClaimAccepted` is applied to — the `claim`
/// value inline, or a `{name}TagDispatchClaim` def the caller already emitted.
fn render_expr_fragment_claim_accepted_split(
    c: &Cert,
    claim_target: &str,
    host_table_lean: &str,
) -> String {
    let Cert::ExprFragment {
        carrier,
        self_idx,
        type_idx,
        plan,
        ..
    } = c.inner()
    else {
        unreachable!()
    };
    let name = c.name();
    let carrier = *carrier;
    let code_entry_bytes = lower_expr_fragment_plan_code_entry_bytes(plan, carrier)
        .expect("generic expr-fragment plan lowers to code-entry bytes");
    let code_entry_bytes = render_byte_list(&code_entry_bytes);
    let lowered_body = lower_expr_fragment_plan(plan, carrier)
        .map(|ops| render_ops_value(&ops))
        .expect("generic expr-fragment plan lowers to WInstr body");
    let export_name_bytes = render_byte_list(name.as_bytes());
    let plan_ref = format!("AverCert.Plans.{name}Plan");
    let body = format!("{name}ClaimBody");
    let code_entry = format!("{name}ClaimCodeEntry");
    let binding = format!("{name}ClaimBinding");
    let carrier_bound = format!("{name}ClaimCarrierBound");
    let host_types = format!("{name}ClaimHostTableFuncTypes");
    let check_plan = format!("{name}ClaimCheckPlan");
    let lower_body = format!("{name}ClaimLowerBody");
    let lower_code = format!("{name}ClaimLowerCodeEntry");
    let func_binding = format!("{name}ClaimFuncBinding");
    let func_type = format!("{name}ClaimFuncTypeMatches");
    let nominal = format!("{name}ClaimNominalTypes");
    let accepted = format!("{name}_exprFragmentClaimAccepted");
    format!(
        "-- Witness data as named constants so no large literal is baked into the\n\
         -- aggregate acceptance term (the source of this file's memory peak).\n\
         def {body} : List CertPrelude.WInstr := {lowered_body}\n\n\
         def {code_entry} : AverCert.WasmSlice.ByteSeq := {code_entry_bytes}\n\n\
         def {binding} : AverCert.WasmSlice.FuncBinding :=\n  \
           {{ funcIdx := {self_idx}, typeIdx := {type_idx}, codeEntry := {code_entry} }}\n\n\
         -- One leaf theorem per acceptance conjunct, each checked and freed in its\n\
         -- own `addDecl`.\n\
         theorem {carrier_bound} :\n  \
           AverCert.AcceptedArtifact.symFragmentCarrierBound AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {carrier} {host_table_lean} {plan_ref} = true := by\n  \
           rfl\n\n\
         theorem {host_types} :\n  \
           AverCert.WasmSlice.hostTableFuncTypesMatch AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {carrier} {host_table_lean} = true := by\n  \
           rfl\n\n\
         theorem {check_plan} :\n  \
           AverCert.PlanCheck.checkExprFragmentRawPlan {plan_ref} = true := by\n  \
           rfl\n\n\
         theorem {lower_body} :\n  \
           AverCert.PlanLower.lowerExprFragmentBody {carrier} {plan_ref} = some {body} := by\n  \
           rfl\n\n\
         theorem {lower_code} :\n  \
           AverCert.PlanBytes.lowerExprFragmentCodeEntry {carrier} {plan_ref} = some {code_entry} := by\n  \
           rfl\n\n\
         theorem {func_binding} :\n  \
           AverCert.WasmSlice.exactFuncBindingForExport AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {export_name_bytes} {code_entry} = some {binding} := by\n  \
           rfl\n\n\
         theorem {func_type} :\n  \
           AverCert.WasmSlice.exprFragmentFuncTypeMatches AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {binding}.typeIdx {carrier} {plan_ref}.params {plan_ref}.result = true := by\n  \
           rfl\n\n\
         theorem {nominal} :\n  \
           AverCert.WasmSlice.exprFragmentNominalTypesMatch AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {binding}.typeIdx {carrier} {plan_ref} = true := by\n  \
           rfl\n\n\
         -- Aggregate: combine the already-checked leaf constants.\n\
         theorem {accepted} :\n  \
           AverCert.AcceptedArtifact.symFragmentClaimAccepted\n    \
             AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {claim_target} := by\n  \
           unfold AverCert.AcceptedArtifact.symFragmentClaimAccepted\n  \
           exact ⟨{carrier_bound}, {host_types}, rfl, rfl, ⟨{body}, {code_entry}, {binding}, ⟨⟨{check_plan}, {lower_body}, {lower_code}, {func_binding}⟩, {func_type}, {nominal}, rfl, rfl⟩⟩⟩\n"
    )
}

fn render_expr_fragment_semantic_bridge(
    c: &Cert,
    host_table: FragHostTable,
    struct_table_lean: &str,
    model_info: &ModelInfo,
) -> String {
    if let Some(face) = c.tag_dispatch_face() {
        return render_expr_fragment_tag_dispatch_semantic_bridge(
            c,
            face,
            host_table,
            struct_table_lean,
        );
    }
    debug_assert!(expr_fragment_uses_audited_generic(c));
    if let Some(face) = c.int_add_face() {
        return render_expr_fragment_int_add_semantic_bridge(
            c,
            face,
            host_table,
            struct_table_lean,
            model_info,
        );
    }
    render_expr_fragment_int_bool_semantic_bridge(c, host_table, struct_table_lean, model_info)
}

fn render_expr_fragment_tag_dispatch_semantic_bridge(
    c: &Cert,
    face: FragTagDispatchFace,
    host_table: FragHostTable,
    struct_table_lean: &str,
) -> String {
    let name = c.name();
    let carrier = c.carrier();
    let claim_name = format!("{name}TagDispatchClaim");
    let claim = expr_fragment_claim_lean_value(c, host_table, struct_table_lean);
    let host_table_lean = host_table.lean_value();
    let claim_accepted = render_expr_fragment_claim_accepted_split(c, &claim_name, &host_table_lean);
    let tag = lean_int_lit(face.tag);
    let then_c = lean_int_lit(face.then_c);
    let else_c = lean_int_lit(face.else_c);
    let arm = |constant: &str| {
        format!(
            r#"  · refine ⟨[.structv {opt_idx} [.i32v x.1, x.2]], [.structv {opt_idx} [.i32v x.1, x.2], .null],
      carrierSmall {carrier} ({constant}), rfl, rfl, ?_, ?_, ?_⟩
    · simp [ExprFragmentSoundness.blockCallsOK, ExprFragmentSoundness.nodesCallsOK,
        ExprFragmentSoundness.kindCallsOK, AverCert.Plans.{name}Plan, {claim_name}, AverCert.{name}Ob,
        AverCert.StandardFace.tagDispatchHost]
    · simp [ExprFragmentSemantics.evalSymRawPlan, {claim_name}, AverCert.{name}Ob, AverCert.StandardFace.tagDispatchHost,
        show AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan {host_table_lean} {struct_table_lean}
          AverCert.Plans.{name}SymPlan = some AverCert.Plans.{name}Plan from rfl,
        AverCert.Plans.{name}Plan, ExprFragmentSemantics.runBlock,
        AverCert.PlanLower.maxFuel, ExprFragmentSemantics.runBlockFuel,
        ExprFragmentSemantics.runNodesFuel, ExprFragmentSemantics.finishWith,
        AverCert.AcceptedArtifact.exprFragmentNLocals, initLocals, PlanLower.popExpected,
        PlanLower.popExpectedAll, PlanLower.primInstr, ExprFragmentSemantics.runPrim,
        wRunF, boxRef, popArgs, carrierSmall, b32, hx]
    · simpa [{claim_name}, AverCert.{name}Ob, AverCert.Schema.intRepr, hx]
        using S.smallIntro ({constant} : Int)
"#,
            opt_idx = face.opt_idx,
        )
    };
    let then_arm = arm(&then_c);
    let else_arm = arm(&else_c);
    format!(
        r#"/-! ### {name} — operational tag-dispatch expr-fragment semantic bridge -/

def {claim_name} : AverCert.AcceptedArtifact.SymFragmentClaim := {claim}

{claim_accepted}
theorem {name}_exprFragmentSemanticBridge :
    AcceptanceSoundness.exprFragmentSemanticBridge {claim_name}
      AverCert.Plans.{name}Plan := by
  refine ⟨rfl, ?_⟩
  intro S add sub mul stringEq stringConcat toIndex hAdd hSub hMul hStringEq hStringConcat
    _hToIndex fuel x vs w hDom hRun
  dsimp only [{claim_name}, AverCert.{name}Ob] at x hDom ⊢
  subst hDom
  by_cases hx : x.1 = {tag}
{then_arm}{else_arm}
#print axioms {name}_exprFragmentSemanticBridge
"#
    )
}

fn render_expr_fragment_int_add_semantic_bridge(
    c: &Cert,
    face: FragIntAddFace,
    host_table: FragHostTable,
    struct_table_lean: &str,
    model_info: &ModelInfo,
) -> String {
    let name = c.name();
    let model_name = c.model_lean_name(model_info);
    let carrier = c.carrier();
    let k = lean_int_lit(face.k);
    let claim = expr_fragment_claim_lean_value(c, host_table, struct_table_lean);
    let host_table_lean = host_table.lean_value();
    let claim_accepted = render_expr_fragment_claim_accepted_split(c, &claim, &host_table_lean);
    format!(
        r#"/-! ### {name} — option-(b) integer expr-fragment semantic bridge -/

{claim_accepted}
theorem {name}_exprFragmentSemanticBridge :
    AcceptanceSoundness.exprFragmentSemanticBridge {claim}
      AverCert.Plans.{name}Plan := by
  refine ⟨rfl, ?_⟩
  intro S add sub mul stringEq stringConcat toIndex
    hAdd hSub hMul hStringEq hStringConcat _hToIndex fuel ns vs out hDom hRun
  dsimp [AverCert.{name}Ob] at ns vs hDom hRun ⊢
  rcases hDom with ⟨hRepr, hLen⟩
  cases hRepr with
  | nil => simp at hLen
  | cons hv htail =>
      rename_i n v ns' vs'
      cases htail with
      | cons _ _ => simp at hLen
      | nil =>
          cases hc : add [v, carrierSmall {carrier} ({k})] with
          | none =>
              simp [wFuncN, wRunF, CertModule.{name}Code, CertModule.{name}Host,
                boxRef, popArgs, initLocals, hc] at hRun
          | some result =>
              refine ⟨[v], [v, .null], result, rfl, rfl, ?_, ?_, ?_⟩
              · simp [ExprFragmentSoundness.blockCallsOK,
                  ExprFragmentSoundness.nodesCallsOK,
                  ExprFragmentSoundness.kindCallsOK, AverCert.Plans.{name}Plan,
                  CertModule.{name}Code, CertModule.{name}Host]
              · simp only [ExprFragmentSemantics.evalSymRawPlan]
                rw [show AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
                  {host_table_lean} {struct_table_lean}
                  AverCert.Plans.{name}SymPlan =
                    some AverCert.Plans.{name}Plan by rfl]
                simp [AverCert.Plans.{name}Plan,
                  AverCert.PlanLower.maxFuel, ExprFragmentSemantics.runBlock,
                  ExprFragmentSemantics.runBlockFuel,
                  ExprFragmentSemantics.runNodesFuel,
                  ExprFragmentSemantics.finishWith,
                  AverCert.AcceptedArtifact.exprFragmentNLocals,
                  CertModule.{name}Code, CertModule.{name}Host, boxRef,
                  popArgs, initLocals, carrierSmall, hc] <;>
                try simp_all [ExprFragmentSemantics.runBlockFuel,
                  ExprFragmentSemantics.runNodesFuel,
                  ExprFragmentSemantics.finishWith, PlanLower.popExpected,
                  PlanLower.popExpectedAll, PlanLower.primInstr,
                  ExprFragmentSemantics.runPrim, wRunF,
                  AverCert.AcceptedArtifact.exprFragmentNLocals,
                  CertModule.{name}Code, CertModule.{name}Host, boxRef,
                  popArgs, initLocals, hc, carrierSmall, b32] <;>
                try simp_all [ExprFragmentSemantics.runBlockFuel,
                  ExprFragmentSemantics.runNodesFuel,
                  ExprFragmentSemantics.finishWith, PlanLower.popExpected,
                  PlanLower.popExpectedAll, PlanLower.primInstr,
                  ExprFragmentSemantics.runPrim, wRunF,
                  AverCert.AcceptedArtifact.exprFragmentNLocals,
                  CertModule.{name}Code, CertModule.{name}Host, boxRef,
                  popArgs, initLocals, hc, carrierSmall, b32]
              · simpa [AverCert.Schema.intRepr, {model_name}] using
                  hAdd n ({k}) v (carrierSmall {carrier} ({k})) result hv
                    (S.smallIntro ({k})) hc

#print axioms {name}_exprFragmentSemanticBridge
"#
    )
}

/// `expr_fragment_bool_expr` with constant-condition `if`s folded away: the
/// canonical-input null guards render as `if (true) …`, and a `by_cases`
/// hypothesis must be stated in the same normal form simp leaves in the goal
/// (the folded branch), or it never matches as a rewrite.
fn expr_fragment_reduced_bool_expr<F>(block: &FragBlock, id: FragValueId, local: &F) -> String
where
    F: Fn(u32, FragTy) -> String,
{
    let node = block.node(id).expect("fragment node exists");
    if let FragNodeKind::If {
        cond,
        then_block,
        else_block,
    } = &node.kind
    {
        let c = expr_fragment_bool_expr(block, *cond, local);
        if c == "true" {
            return expr_fragment_reduced_bool_expr(then_block, then_block.result, local);
        }
        if c == "false" {
            return expr_fragment_reduced_bool_expr(else_block, else_block.result, local);
        }
    }
    expr_fragment_bool_expr(block, id, local)
}

/// Conditions that steer the interpreter's `ifElse` branch selection and are
/// neither constant (the canonical-input null guards reduce to `true`) nor a
/// Bool parameter (those are split once by `cases a{i}`). Only these need a
/// case split: every other comparison flows through the run as a symbolic
/// `b32 P` value that simp's stock decide/ite lemmas normalize.
fn collect_expr_fragment_steering_conditions<F>(
    block: &FragBlock,
    local: &F,
    out: &mut Vec<String>,
) where
    F: Fn(u32, FragTy) -> String,
{
    for node in &block.nodes {
        if let FragNodeKind::If {
            cond,
            then_block,
            else_block,
        } = &node.kind
        {
            let steering = match block.node(*cond).map(|n| &n.kind) {
                // A local condition is covered elsewhere, not skipped: the
                // checker types an `ifElse` condition as `BoolI32`, and a local
                // carries its parameter's type, so such a condition is a
                // Boolean parameter and the script already splits on it with
                // `cases`. A constant one reduces away inside the single
                // simplification step.
                Some(FragNodeKind::Local { .. }) | Some(FragNodeKind::ConstBool(_)) | None => {
                    false
                }
                Some(_) => {
                    let rendered = expr_fragment_reduced_bool_expr(block, *cond, local);
                    rendered != "true" && rendered != "false"
                }
            };
            if steering {
                let rendered = expr_fragment_reduced_bool_expr(block, *cond, local);
                if !out.contains(&rendered) {
                    out.push(rendered);
                }
            }
            collect_expr_fragment_steering_conditions(then_block, local, out);
            collect_expr_fragment_steering_conditions(else_block, local, out);
        }
    }
}

fn expr_fragment_bridge_eval_tactic(
    plan: &ExprFragmentPlan,
    name: &str,
    host_table_lean: &str,
    struct_table_lean: &str,
    evalset: &str,
) -> String {
    // Case splits are limited to what actually steers the interpreter's
    // control flow: Bool params (split once by `cases`) and non-constant
    // `ifElse` conditions (short-circuit encodings). Comparison atoms that
    // only produce values — the whole eager-conjunction class, whose null
    // guards are constant under the canonical `carrierSmall`/`b32` inputs —
    // are never split: the interpreter's comparison and `.i32And` clauses
    // return `b32 P` symbolically and simp's stock decide/ite lemmas close
    // the payload equality. This keeps elaboration linear in the atom count
    // for eager conjunctions, where the old every-atom `by_cases` 2^n split
    // peaked past physical memory at six atoms.
    let mut steps = plan
        .params
        .iter()
        .enumerate()
        .filter(|(_, ty)| **ty == FragTy::BoolI32)
        .map(|(i, _)| format!("cases a{i}"))
        .collect::<Vec<_>>();
    let mut conds = Vec::new();
    collect_expr_fragment_steering_conditions(
        &plan.body,
        &|idx, _ty| format!("a{idx}"),
        &mut conds,
    );
    for (i, cond) in conds.iter().enumerate() {
        steps.push(format!("by_cases h{i} : {cond}"));
    }
    let hints = if conds.is_empty() {
        String::new()
    } else {
        format!(
            ", {}",
            (0..conds.len())
                .map(|i| format!("h{i}"))
                .collect::<Vec<_>>()
                .join(", ")
        )
    };
    let simp = format!(
        "simp [{evalset}, PlanLower.popExpected, PlanLower.popExpectedAll, \
         PlanLower.primInstr, ExprFragmentSemantics.runPrim, carrierSmall, ge_iff_le{hints}]"
    );
    let first = if steps.is_empty() {
        simp
    } else {
        format!("{} <;> {simp}", steps.join(" <;> "))
    };
    format!(
        "    simp only [ExprFragmentSemantics.evalSymRawPlan]\n    \
         rw [show AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan\n      \
         {host_table_lean} {struct_table_lean} AverCert.Plans.{name}SymPlan =\n      \
         some AverCert.Plans.{name}Plan by rfl]\n    \
         {first}"
    )
}

fn render_expr_fragment_int_bool_semantic_bridge(
    c: &Cert,
    host_table: FragHostTable,
    struct_table_lean: &str,
    model_info: &ModelInfo,
) -> String {
    let Cert::ExprFragment {
        name,
        carrier,
        plan,
        ..
    } = c.inner()
    else {
        unreachable!()
    };
    let model_name = c.model_lean_name(model_info);
    debug_assert_eq!(plan.result, FragTy::BoolI32);
    let claim = expr_fragment_claim_lean_value(c, host_table, struct_table_lean);
    let result = expr_fragment_wval_expr(plan, &|idx, _ty| format!("a{idx}"));
    let input_values = plan
        .params
        .iter()
        .enumerate()
        .map(|(idx, ty)| match ty {
            FragTy::IntCarrier => format!("carrierSmall {carrier} a{idx}"),
            FragTy::BoolI32 => format!("b32 a{idx}"),
            _ => unreachable!("generic integer/Bool fragment input"),
        })
        .collect::<Vec<_>>();
    let inputs = format!("[{}]", input_values.join(", "));
    let mut locals = input_values;
    locals.push(".null".to_string());
    let locals = format!("[{}]", locals.join(", "));
    let (dom_name, unpack) = match plan.params.len() {
        1 => ("a0".to_string(), String::new()),
        // Right-nested product domain: the flat anonymous-constructor
        // pattern `⟨a0, a1, …⟩` destructures `A × (B × (…))` exactly, so
        // one `rcases` unpacks any arity.
        arity => (
            "p".to_string(),
            format!(
                "  rcases p with ⟨{}⟩\n",
                (0..arity)
                    .map(|index| format!("a{index}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        ),
    };
    let evalset = format!(
        "AverCert.Plans.{name}Plan, AverCert.PlanLower.maxFuel, \
         ExprFragmentSemantics.runBlock, ExprFragmentSemantics.runBlockFuel, \
         ExprFragmentSemantics.runNodesFuel, ExprFragmentSemantics.finishWith, \
         AverCert.AcceptedArtifact.exprFragmentNLocals, \
         CertModule.{name}Code, CertModule.{name}Host, wFuncN, wRunF, f, b32, \
         popArgs, initLocals, {model_name}"
    );
    let host_table_lean = host_table.lean_value();
    let claim_accepted = render_expr_fragment_claim_accepted_split(c, &claim, &host_table_lean);
    let eval_tactic = expr_fragment_bridge_eval_tactic(
        plan,
        name,
        &host_table_lean,
        struct_table_lean,
        &evalset,
    );
    format!(
        r#"/-! ### {name} — option-(b) integer/Bool expr-fragment semantic bridge -/

{claim_accepted}
theorem {name}_exprFragmentSemanticBridge :
    AcceptanceSoundness.exprFragmentSemanticBridge {claim}
      AverCert.Plans.{name}Plan := by
  refine ⟨rfl, ?_⟩
  intro S add sub mul stringEq stringConcat toIndex
    hAdd hSub hMul hStringEq hStringConcat _hToIndex fuel {dom_name} vs out hDom hRun
  dsimp [AverCert.{name}Ob] at {dom_name} vs hDom hRun ⊢
{unpack}  subst vs
  refine ⟨{inputs}, {locals}, {result}, rfl, rfl, ?_, ?_, ?_⟩
  · simp [ExprFragmentSoundness.blockCallsOK,
      ExprFragmentSoundness.nodesCallsOK, ExprFragmentSoundness.kindCallsOK,
      AverCert.Plans.{name}Plan, CertModule.{name}Code, CertModule.{name}Host]
  ·
{eval_tactic}
  · simp [{model_name}, AverCert.Schema.boolRepr, b32]

#print axioms {name}_exprFragmentSemanticBridge
"#
    )
}

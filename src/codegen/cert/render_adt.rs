fn render_adt_constructor_cert(c: &Cert, model_info: &ModelInfo) -> String {
    let c = c.inner();
    let Cert::AdtConstructor {
        name,
        self_idx,
        carrier,
        struct_idx,
        ..
    } = c
    else {
        unreachable!()
    };
    debug_assert!(adt_constructor_uses_model(c, model_info));
    let sig = model_info.fns.get(name);
    let _ = sig
        .and_then(|s| model_info.inductives.get(&s.ret))
        .and_then(|i| i.ctors.first());
    format!(
        r#"/-! ### {name} — ADT constructor certificate (carrier type {carrier}) -/

theorem {name}_wasm_certified (host : HostTbl) :
    ∀ (v : WVal), wFuncN {name}Code host 1 {self_idx} [v] = some (.structv {struct_idx} [v]) := by
  intro v
  simp [wFuncN, {name}Code, wRunF, popArgs, initLocals]

#print axioms {name}_wasm_certified

-- Executable tripwire: run the constructor on an Int 7 and decode field 0 of the
-- built struct back to `some 7`. Unlike a bare `= none` (which a TRAP also
-- satisfies), this forces the emitted body to genuinely pack its argument.
example :
    ((wFuncN {name}Code {name}Host 1 {self_idx} [carrierSmall {carrier} 7]).bind
      (fun r => match r with
        | .structv _ (f :: _) => carrierToInt f
        | _ => none))
      = some 7 := by native_decide

theorem {name}_simulates : AverCert.Schema.Obligation.holds {name}Ob := by
  intro S add sub mul stringEq stringConcat hadd hsub hmul hStringEq hStringConcat fuel n vs w hrepr hrun
  simp only [{name}Ob, AverCert.Schema.Obligation.holds] at hrun ⊢
  obtain ⟨v, rfl, hv⟩ := hrepr
  cases fuel with
  | zero => simp [wFuncN] at hrun
  | succ f =>
      simp [wFuncN, wRunF, {name}Code, popArgs, initLocals] at hrun
      subst hrun
      exact ⟨v, rfl, by simpa [AverCert.Schema.intRepr] using hv⟩
"#
    )
}

/// The per-class ingredients of one ADT-match certificate: the pieces that
/// differ between the fully-modelled variant dispatch and the widened Int match,
/// spliced into ONE shared `cases o` skeleton by [`render_adt_match_cert`].
struct AdtMatchPlan {
    doc_title: String,
    /// The theorem header up to (not including) the trailing ` :` — the binder
    /// block differs (variant carries the add/sub host contracts, widened none).
    header: String,
    ty: String,
    /// The domain-representation relation: the shared, faithful `{ty}Repr` when
    /// every constructor is modelled, the per-function weak `{name}DomRepr` when
    /// only one integer-payload variant is projected.
    repr: String,
    host_term: String,
    host_ref_def: String,
    /// The explicit per-constructor `cases o` arms, already joined.
    cases: String,
    guards: String,
    /// The `_simulates` citation of `{name}_wasm_certified` (its argument list
    /// differs by the host contracts).
    sim_cite: String,
}

/// Leaf-polymorphic ADT variant-match certificate: ONE explicit `cases o` proof
/// spine shared by the fully-modelled variant dispatch (every constructor
/// faithfully represented, closing through the runtime add/sub contracts) and
/// the widened Int match (one integer-payload constructor projected faithfully,
/// every other constructor opaque and boxing the default). The walker emits the
/// spine; each constructor closes with its OWN honest closer against its OWN
/// honest domain representation, so neither faithfulness level is weakened to fit
/// the other (the exact fold that a single shared `Repr` would have banned).
fn render_adt_match_cert(c: &Cert, model_info: &ModelInfo) -> String {
    let inner = c.inner();
    let name = inner.name();
    let self_idx = inner.self_idx();
    let plan = match inner {
        Cert::VariantDispatch { .. } => adt_match_variant_plan(inner, model_info),
        Cert::WidenedIntMatch { .. } => adt_match_widened_plan(inner, model_info),
        _ => unreachable!(),
    };
    let AdtMatchPlan {
        doc_title,
        header,
        ty,
        repr,
        host_term,
        host_ref_def,
        cases,
        guards,
        sim_cite,
    } = match plan {
        Ok(p) => p,
        Err(early) => return early,
    };
    format!(
        r#"{doc_title}

{header} :
    ∀ (fuel : Nat) (o : {ty}) (v w : WVal), {repr} S o v →
      wFuncN {name}Code {host_term} fuel {self_idx} [v] = some w →
      S.Repr ({name} o) w := by
  intro fuel
  cases fuel with
  | zero => intro o v w hv hrun; simp [wFuncN] at hrun
  | succ f =>
      intro o v w hv hrun
      cases o with
{cases}

#print axioms {name}_wasm_certified

{host_ref_def}
{guards}

theorem {name}_simulates : AverCert.Schema.Obligation.holds {name}Ob := by
  intro S add sub mul stringEq stringConcat hadd hsub hmul hStringEq hStringConcat fuel o vs w hrepr hrun
  simp only [{name}Ob, AverCert.Schema.Obligation.holds] at hrun ⊢
  obtain ⟨v, rfl, hv⟩ := hrepr
  simpa [AverCert.Schema.intRepr] using {sim_cite}
"#
    )
}

/// The variant-dispatch plan: every constructor of a fully-modelled inductive is
/// faithful (`{ty}Repr`); its arm is a payload projection, a payload/const host
/// combination, or the terminal default. Closes through the add/sub contracts.
fn adt_match_variant_plan(c: &Cert, model_info: &ModelInfo) -> Result<AdtMatchPlan, String> {
    let Cert::VariantDispatch {
        name,
        self_idx,
        carrier,
        add_idx,
        sub_idx,
        arms,
        default_k,
        ..
    } = c
    else {
        unreachable!()
    };
    let ty = model_info
        .fns
        .get(name)
        .and_then(|s| s.params.first())
        .map(|s| s.as_str())
        .unwrap_or("Op");
    let repr = format!("{ty}Repr");
    let host_ref = format!("{name}HostRef");
    let Some(ind) = model_info.inductives.get(ty) else {
        return Err(format!(
            "-- {name}: no source inductive for {ty}\nexample : False := by decide\n"
        ));
    };
    let base = arms.iter().map(|(t, _)| *t).min().unwrap_or(0);
    let arm_of_tag =
        |tag: u32| -> Option<&ArmLeaf> { arms.iter().find(|(t, _)| *t == tag).map(|(_, l)| l) };
    let simp_run = format!(
        "simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, b32, popArgs, initLocals] at hrun"
    );

    let mut cases = Vec::new();
    let mut guards = Vec::new();
    for (i, ctor) in ind.ctors.iter().enumerate() {
        let tag = base + i as u32;
        let cn = &ctor.name;
        let payload = !ctor.fields.is_empty();
        if arm_of_tag(tag).is_some() && !payload {
            return Err(format!(
                "-- {name}: variant tag mapping mismatch ({cn} is nullary but tag {tag} is dispatched)\nexample : False := by decide\n"
            ));
        }
        match arm_of_tag(tag) {
            Some(ArmLeaf::Proj) => {
                cases.push(format!(
                    "      | {cn} x =>\n          obtain ⟨cx, rfl, hcx⟩ := hv\n          {simp_run}\n          subst hrun\n          simpa [{name}] using hcx"
                ));
                guards.push(format!(
                    "example : ((wFuncN {name}Code {host_ref} 4 {self_idx} [.structv {tag} [carrierSmall {carrier} 5]]).bind carrierToInt)\n    = some 5 := by native_decide"
                ));
            }
            Some(ArmLeaf::HostOp {
                role,
                k,
                const_first,
            }) => {
                let (hostfn, hyp) = match role {
                    HostRole::Add => ("add", "hadd"),
                    HostRole::Sub => ("sub", "hsub"),
                    HostRole::StringEq | HostRole::StringConcat => unreachable!(),
                };
                let (operands, cite) = if *const_first {
                    (
                        format!("[carrierSmall {carrier} ({k}), cx]"),
                        format!(
                            "{hyp} ({k}) x (carrierSmall {carrier} ({k})) cx w' (S.smallIntro ({k})) hcx hs"
                        ),
                    )
                } else {
                    (
                        format!("[cx, carrierSmall {carrier} ({k})]"),
                        format!(
                            "{hyp} x ({k}) cx (carrierSmall {carrier} ({k})) w' hcx (S.smallIntro ({k})) hs"
                        ),
                    )
                };
                cases.push(format!(
                    "      | {cn} x =>\n          obtain ⟨cx, rfl, hcx⟩ := hv\n          {simp_run}\n          rcases hs : {hostfn} {operands} with _ | w' <;> simp [hs] at hrun\n          subst hrun\n          have := {cite}\n          simpa [{name}] using this"
                ));
                let sample = 5i64;
                let expected = if *const_first {
                    match role {
                        HostRole::Add => k + sample,
                        HostRole::Sub => k - sample,
                        HostRole::StringEq | HostRole::StringConcat => unreachable!(),
                    }
                } else {
                    match role {
                        HostRole::Add => sample + k,
                        HostRole::Sub => sample - k,
                        HostRole::StringEq | HostRole::StringConcat => unreachable!(),
                    }
                };
                guards.push(format!(
                    "example : ((wFuncN {name}Code {host_ref} 4 {self_idx} [.structv {tag} [carrierSmall {carrier} {sample}]]).bind carrierToInt)\n    = some ({expected}) := by native_decide"
                ));
            }
            None => {
                if payload {
                    cases.push(format!(
                        "      | {cn} x =>\n          obtain ⟨cx, rfl, hcx⟩ := hv\n          {simp_run}\n          subst hrun\n          simpa [{name}] using S.smallIntro ({default_k})"
                    ));
                    guards.push(format!(
                        "example : ((wFuncN {name}Code {host_ref} 4 {self_idx} [.structv {tag} [carrierSmall {carrier} 5]]).bind carrierToInt)\n    = some ({default_k}) := by native_decide"
                    ));
                } else {
                    cases.push(format!(
                        "      | {cn} =>\n          subst hv\n          {simp_run}\n          subst hrun\n          simpa [{name}] using S.smallIntro ({default_k})"
                    ));
                    guards.push(format!(
                        "example : ((wFuncN {name}Code {host_ref} 4 {self_idx} [.structv {tag} []]).bind carrierToInt)\n    = some ({default_k}) := by native_decide"
                    ));
                }
            }
        }
    }
    let add_ref = if add_idx.is_some() {
        format!("(addRef {carrier})")
    } else {
        "(fun _ => none)".to_string()
    };
    let sub_ref = if sub_idx.is_some() {
        format!("(subRef {carrier})")
    } else {
        "(fun _ => none)".to_string()
    };
    Ok(AdtMatchPlan {
        doc_title: format!(
            "/-! ### {name} — general variant dispatch certificate (carrier type {carrier}) -/"
        ),
        header: format!(
            "theorem {name}_wasm_certified\n    (S : CarrierSpec {carrier})\n    (add sub : List WVal → Option WVal)\n    (hadd : ∀ a b va vb w, S.Repr a va → S.Repr b vb → add [va, vb] = some w → S.Repr (a + b) w)\n    (hsub : ∀ a b va vb w, S.Repr a va → S.Repr b vb → sub [va, vb] = some w → S.Repr (a - b) w)"
        ),
        ty: ty.to_string(),
        repr,
        host_term: format!("({name}Host add sub)"),
        host_ref_def: format!("def {host_ref} : HostTbl := {name}Host {add_ref} {sub_ref}"),
        cases: cases.join("\n"),
        guards: guards.join("\n"),
        sim_cite: format!("{name}_wasm_certified S add sub hadd hsub fuel o v w hv hrun"),
    })
}

/// The widened-match plan: the single integer-payload constructor is faithful
/// (projects its carrier), every other constructor is opaque — represented only
/// as "some struct of a different type" and boxing the default `0`. The honest
/// per-function `{name}DomRepr` keeps the heterogeneous variants opaque instead
/// of forcing them into an integer carrier they cannot inhabit.
fn adt_match_widened_plan(c: &Cert, model_info: &ModelInfo) -> Result<AdtMatchPlan, String> {
    let Cert::WidenedIntMatch {
        name,
        self_idx,
        carrier,
        hit_variant_idx,
        ..
    } = c
    else {
        unreachable!()
    };
    let Some((_ty, ind, hit_ctor)) = widened_match_info(c, model_info) else {
        return Err(format!(
            "-- {name}: widened match info unavailable\nexample : False := by decide\n"
        ));
    };
    let ty = model_info
        .fns
        .get(name)
        .and_then(|s| s.params.first())
        .cloned()
        .unwrap_or_else(|| "Op".to_string());
    let other_idx = if *hit_variant_idx == 0 { 1 } else { 0 };

    let mut cases = Vec::new();
    for ctor in &ind.ctors {
        if ctor.name == hit_ctor {
            cases.push(format!(
                "      | {cn} x =>\n          simp only [{name}DomRepr] at hv\n          obtain ⟨cx, rfl, hcx⟩ := hv\n          simp [wFuncN, wRunF, {name}Code, {name}Host, b32, popArgs, initLocals] at hrun\n          subst hrun\n          simpa [{name}] using hcx",
                cn = ctor.name,
            ));
        } else {
            let binders = match ctor.fields.len() {
                0 => String::new(),
                1 => " x".to_string(),
                n => " _".repeat(n),
            };
            cases.push(format!(
                "      | {cn}{binders} =>\n          simp only [{name}DomRepr] at hv\n          obtain ⟨t, fs, rfl, hne⟩ := hv\n          simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, b32, popArgs, initLocals, hne] at hrun\n          subst hrun\n          simpa [{name}] using S.smallIntro 0",
                cn = ctor.name,
            ));
        }
    }
    let guards = format!(
        "example :\n    ((wFuncN {name}Code {name}HostRef 4 {self_idx}\n        [.structv {hit_variant_idx} [carrierSmall {carrier} 42]]).bind carrierToInt)\n      = some 42 := by native_decide\nexample :\n    ((wFuncN {name}Code {name}HostRef 4 {self_idx}\n        [.structv {other_idx} []]).bind carrierToInt)\n      = some 0 := by native_decide"
    );
    Ok(AdtMatchPlan {
        doc_title: format!(
            "/-! ### {name} — widened Int match certificate (carrier type {carrier}) -/"
        ),
        header: format!("theorem {name}_wasm_certified (S : CarrierSpec {carrier})"),
        ty,
        repr: format!("{name}DomRepr"),
        host_term: format!("{name}Host"),
        host_ref_def: format!(
            "-- Executable tripwires: the projected variant reads its carrier, every other\n-- variant boxes the default `0`. A trapping body yields `none`, forcing a run.\ndef {name}HostRef : HostTbl := {name}Host"
        ),
        cases: cases.join("\n"),
        guards,
        sim_cite: format!("{name}_wasm_certified S fuel o v w hv hrun"),
    })
}

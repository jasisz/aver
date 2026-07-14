/// Option-(b) residual for one model-bearing Int dispatch.  The generated
/// theorem performs only source-ADT case analysis and builds the matching
/// `EvalCascade`; byte lowering, host simulation and fuel reasoning live in
/// the audited `IntDispatchSoundness` / `DischargeIntDispatch` wall.
fn render_int_dispatch_semantic_bridge(
    c: &Cert,
    model_info: &ModelInfo,
    strict: FragHostTable,
) -> String {
    let Some(plan) = int_dispatch_plan_from_cert(c, strict) else {
        return format!(
            "-- {}: no byte-matching int-dispatch plan\nexample : False := by decide\n",
            c.name()
        );
    };
    match c.inner() {
        Cert::VariantDispatch { .. } => render_variant_dispatch_semantic_bridge(c, model_info, &plan),
        Cert::WidenedIntMatch { .. } => render_widened_int_semantic_bridge(c, model_info, &plan),
        _ => unreachable!(),
    }
}

fn render_variant_dispatch_semantic_bridge(
    c: &Cert,
    model_info: &ModelInfo,
    plan: &IntDispatchRawPlan,
) -> String {
    let Cert::VariantDispatch {
        name,
        carrier,
        arms,
        ..
    } = c.inner()
    else {
        unreachable!()
    };
    let Some(ty) = model_info
        .fns
        .get(name)
        .and_then(|sig| sig.params.first())
    else {
        return format!(
            "-- {name}: source model parameter type unavailable\nexample : False := by decide\n"
        );
    };
    let Some(ind) = model_info.inductives.get(ty) else {
        return format!(
            "-- {name}: source inductive `{ty}` unavailable\nexample : False := by decide\n"
        );
    };
    let Some(base) = arms.iter().map(|(tag, _)| *tag).min() else {
        return format!("-- {name}: empty dispatch\nexample : False := by decide\n");
    };

    let mut cases = Vec::new();
    for (position, ctor) in ind.ctors.iter().enumerate() {
        let tag = base + position as u32;
        let binders = constructor_binders(ctor.fields.len());
        if ctor.fields.is_empty() {
            let source_value = format!("{name} (.{})", ctor.name);
            let term = match render_eval_cascade_term(
                &plan.body,
                tag,
                "[]",
                &source_value,
                None,
            ) {
                Ok(term) => term,
                Err(error) => return render_dispatch_bridge_failure(name, &error),
            };
            cases.push(format!(
                "  | {ctor} =>\n      subst v\n      refine ⟨{tag}, [], {source_value}, rfl, ?_, ?_⟩\n      · simpa [AverCert.Plans.{name}IntDispatchPlan, {name}, IntDispatchSoundness.evalLeaf] using\n          ({term})\n      · intro w hw\n        simpa [AverCert.Schema.intRepr] using hw",
                ctor = ctor.name,
            ));
        } else if ctor.fields.as_slice() == ["Int"] {
            let source_value = format!("{name} (.{ctor} x)", ctor = ctor.name);
            let term = match render_eval_cascade_term(
                &plan.body,
                tag,
                "[cx]",
                &source_value,
                Some(("x", "cx", "hcx")),
            ) {
                Ok(term) => term,
                Err(error) => return render_dispatch_bridge_failure(name, &error),
            };
            cases.push(format!(
                "  | {ctor} x =>\n      obtain ⟨cx, rfl, hcx⟩ := hv\n      refine ⟨{tag}, [cx], {source_value}, rfl, ?_, ?_⟩\n      · simpa [AverCert.Plans.{name}IntDispatchPlan, {name}, IntDispatchSoundness.evalLeaf] using\n          ({term})\n      · intro w hw\n        simpa [AverCert.Schema.intRepr] using hw",
                ctor = ctor.name,
            ));
        } else {
            cases.push(format!(
                "  | {ctor}{binders} => simp [{ty}Repr] at hv",
                ctor = ctor.name,
            ));
        }
    }

    format!(
        r#"/-! ### {name} — option-(b) Int-dispatch semantic bridge -/

theorem {name}_intDispatchSemanticBridge :
    ∀ (S : CarrierSpec {carrier}) (x : {ty}) (vs : List WVal),
      (∃ v, vs = [v] ∧ {ty}Repr S x v) →
      ∃ tag fields n,
        vs = [.structv tag fields] ∧
        IntDispatchSoundness.EvalCascade S
          AverCert.Plans.{name}IntDispatchPlan.body tag fields n ∧
        ∀ w, S.Repr n w → AverCert.Schema.intRepr S ({name} x) w := by
  intro S x vs hDom
  obtain ⟨v, rfl, hv⟩ := hDom
  cases x with
{cases}

#print axioms {name}_intDispatchSemanticBridge
"#,
        cases = cases.join("\n"),
    )
}

fn render_widened_int_semantic_bridge(
    c: &Cert,
    model_info: &ModelInfo,
    plan: &IntDispatchRawPlan,
) -> String {
    let Cert::WidenedIntMatch {
        name,
        carrier,
        hit_variant_idx,
        ..
    } = c.inner()
    else {
        unreachable!()
    };
    let Some((ty, ind, hit_ctor)) = widened_match_info(c, model_info) else {
        return format!(
            "-- {name}: widened source model information unavailable\nexample : False := by decide\n"
        );
    };

    let mut cases = Vec::new();
    for ctor in &ind.ctors {
        let binders = constructor_binders(ctor.fields.len());
        let source_ctor = constructor_application(&ctor.name, ctor.fields.len());
        let source_value = format!("{name} ({source_ctor})");
        if ctor.name == hit_ctor {
            let term = match render_eval_cascade_term(
                &plan.body,
                *hit_variant_idx,
                "[cx]",
                &source_value,
                Some(("x", "cx", "hcx")),
            ) {
                Ok(term) => term,
                Err(error) => return render_dispatch_bridge_failure(name, &error),
            };
            cases.push(format!(
                "  | {ctor} x =>\n      obtain ⟨cx, rfl, hcx⟩ := hv\n      refine ⟨{hit_variant_idx}, [cx], {source_value}, rfl, ?_, ?_⟩\n      · simpa [AverCert.Plans.{name}IntDispatchPlan, {name}, IntDispatchSoundness.evalLeaf] using\n          ({term})\n      · intro w hw\n        simpa [AverCert.Schema.intRepr] using hw",
                ctor = ctor.name,
            ));
        } else {
            let term = match render_widened_miss_term(
                &plan.body,
                *hit_variant_idx,
                "tag",
                "fields",
                &source_value,
            ) {
                Ok(term) => term,
                Err(error) => return render_dispatch_bridge_failure(name, &error),
            };
            cases.push(format!(
                "  | {ctor}{binders} =>\n      obtain ⟨tag, fields, rfl, hne⟩ := hv\n      refine ⟨tag, fields, {source_value}, rfl, ?_, ?_⟩\n      · simpa [AverCert.Plans.{name}IntDispatchPlan, {name}, IntDispatchSoundness.evalLeaf] using\n          ({term})\n      · intro w hw\n        simpa [AverCert.Schema.intRepr] using hw",
                ctor = ctor.name,
            ));
        }
    }

    format!(
        r#"/-! ### {name} — option-(b) widened-Int semantic bridge -/

theorem {name}_intDispatchSemanticBridge :
    ∀ (S : CarrierSpec {carrier}) (x : {ty}) (vs : List WVal),
      (∃ v, vs = [v] ∧ {name}DomRepr S x v) →
      ∃ tag fields n,
        vs = [.structv tag fields] ∧
        IntDispatchSoundness.EvalCascade S
          AverCert.Plans.{name}IntDispatchPlan.body tag fields n ∧
        ∀ w, S.Repr n w → AverCert.Schema.intRepr S ({name} x) w := by
  intro S x vs hDom
  obtain ⟨v, rfl, hv⟩ := hDom
  cases x with
{cases}

#print axioms {name}_intDispatchSemanticBridge
"#,
        cases = cases.join("\n"),
    )
}

fn render_eval_cascade_term(
    cascade: &IntDispatchCascade,
    tag: u32,
    fields: &str,
    result: &str,
    payload: Option<(&str, &str, &str)>,
) -> Result<String, String> {
    match cascade {
        IntDispatchCascade::Default(k) => Ok(format!(
            "IntDispatchSoundness.EvalCascade.default ({k}) {tag} {fields}"
        )),
        IntDispatchCascade::Test {
            ty_idx,
            hit,
            rest,
        } if *ty_idx == tag => {
            let Some((x, cx, hcx)) = payload else {
                return Err(format!(
                    "dispatch tag {tag} selects a payload leaf for a non-payload constructor"
                ));
            };
            Ok(format!(
                "IntDispatchSoundness.EvalCascade.hit {ty_idx} ({leaf}) ({rest}) {fields} {x} {cx} rfl {hcx}",
                leaf = int_dispatch_leaf_lean_value(hit),
                rest = int_dispatch_cascade_lean_value(rest),
            ))
        }
        IntDispatchCascade::Test {
            ty_idx,
            hit,
            rest,
        } => {
            let tail = render_eval_cascade_term(rest, tag, fields, result, payload)?;
            Ok(format!(
                "IntDispatchSoundness.EvalCascade.miss {ty_idx} {tag} ({leaf}) ({rest}) {fields} ({result}) (by decide) ({tail})",
                leaf = int_dispatch_leaf_lean_value(hit),
                rest = int_dispatch_cascade_lean_value(rest),
            ))
        }
    }
}

fn render_widened_miss_term(
    cascade: &IntDispatchCascade,
    hit_tag: u32,
    tag: &str,
    fields: &str,
    result: &str,
) -> Result<String, String> {
    let IntDispatchCascade::Test {
        ty_idx,
        hit,
        rest,
    } = cascade
    else {
        return Err("widened plan has no test root".to_string());
    };
    if *ty_idx != hit_tag || !matches!(rest.as_ref(), IntDispatchCascade::Default(0)) {
        return Err("widened plan is not one projected hit plus default zero".to_string());
    }
    Ok(format!(
        "IntDispatchSoundness.EvalCascade.miss {ty_idx} {tag} ({leaf}) ({rest}) {fields} ({result}) hne (IntDispatchSoundness.EvalCascade.default (0) {tag} {fields})",
        leaf = int_dispatch_leaf_lean_value(hit),
        rest = int_dispatch_cascade_lean_value(rest),
    ))
}

fn constructor_binders(field_count: usize) -> String {
    match field_count {
        0 => String::new(),
        1 => " x".to_string(),
        count => (0..count).map(|i| format!(" x{i}")).collect::<String>(),
    }
}

fn constructor_application(name: &str, field_count: usize) -> String {
    match field_count {
        0 => format!(".{name}"),
        1 => format!(".{name} x"),
        count => format!(
            ".{name} {}",
            (0..count)
                .map(|i| format!("x{i}"))
                .collect::<Vec<_>>()
                .join(" ")
        ),
    }
}

fn render_dispatch_bridge_failure(name: &str, error: &str) -> String {
    format!("-- {name}: {error}\nexample : False := by decide\n")
}

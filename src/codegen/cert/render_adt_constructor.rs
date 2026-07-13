/// Option-(b) residual for one model-bearing ADT constructor. The generated
/// theorem relates the named source model to the plan-derived constructed
/// value; byte lowering, host simulation and fuel reasoning live in the
/// audited `V3ConstructVerbatim` / `V3DischargeConstruct` wall.
fn render_adt_constructor_semantic_bridge(c: &Cert, model_info: &ModelInfo) -> String {
    let c = c.inner();
    let Cert::AdtConstructor {
        name,
        carrier,
        struct_idx,
        ..
    } = c
    else {
        unreachable!()
    };
    debug_assert!(adt_constructor_uses_model(c, model_info));
    let sig = model_info
        .fns
        .get(name)
        .expect("model-bearing ADT constructor has a source signature");
    let ret = &sig.ret;

    format!(
        r#"/-! ### {name} — option-(b) constructor semantic bridge -/

theorem {name}_constructSemanticBridge :
    ∀ (S : CarrierSpec {carrier}) (n : Int) (args : List WVal),
      (∃ v, args = [v] ∧ AverCert.Schema.intRepr S n v) →
      args.length = AverCert.Plans.{name}ConstructPlan.arity ∧
      {ret}Repr S ({name} n)
        (.structv {struct_idx}
          (V3ConstructVerbatim.constructModelFields
            (args ++ List.replicate 1 .null)
            AverCert.Plans.{name}ConstructPlan.fields)) := by
  intro S n args hDom
  obtain ⟨v, rfl, hv⟩ := hDom
  constructor
  · rfl
  · cases n <;>
      simpa [{name}, AverCert.Plans.{name}ConstructPlan,
        V3ConstructVerbatim.constructModelFields,
        V3ConstructVerbatim.constructModelField, {ret}Repr,
        AverCert.Schema.intRepr] using hv

#print axioms {name}_constructSemanticBridge
"#
    )
}

/// The option-(b) final arm for one model-bearing ADT constructor. All byte
/// and plan facts reduce from emitted data; the only non-canonical argument is
/// the generated source-model semantic bridge in `Certificate.lean`.
fn render_adt_constructor_final_arm(c: &Cert, model_info: &ModelInfo) -> String {
    let c = c.inner();
    let Cert::AdtConstructor {
        name,
        self_idx,
        carrier,
        struct_idx,
        ops,
        ..
    } = c
    else {
        unreachable!()
    };
    debug_assert!(adt_constructor_uses_model(c, model_info));
    let sig = model_info
        .fns
        .get(name)
        .expect("model-bearing ADT constructor has a source signature");
    let ret = &sig.ret;
    let body = render_ops_value(ops);

    format!(
        "exact V3Master.construct_canonical_discharges \
         (exportName := \"{name}\") (carrier := {carrier}) \
         (structIdx := {struct_idx}) (self := {self_idx}) \
         (plan := AverCert.Plans.{name}ConstructPlan) \
         (code := CertModule.{name}Code) \
         (host := fun _ _ _ _ _ => CertModule.{name}Host) \
         (Dom := Int) (Cod := {ret}) \
         (domRepr := fun S n args => ∃ v, args = [v] ∧ AverCert.Schema.intRepr S n v) \
         (codRepr := fun S x w => {ret}Repr S x w) (model := {name}) \
         (hCheck := by rfl) (body := {body}) \
         (hLow := by rfl) (hCode := by rfl) \
         (hSemantic := CertProofs.{name}_constructSemanticBridge)"
    )
}

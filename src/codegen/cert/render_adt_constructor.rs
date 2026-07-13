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

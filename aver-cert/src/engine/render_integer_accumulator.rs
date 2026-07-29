fn accumulator_fuel_pieces(c: &Cert, model_info: &ModelInfo) -> FuelPieces {
    let q = c.model_lean_name(model_info);
    let Cert::AccumulatorRecursive {
        name,
        self_idx,
        carrier,
        ..
    } = c
    else {
        unreachable!()
    };
    let g3 = eval_accumulator(3, 0);
    let g4 = eval_accumulator(3, 4);
    let gneg = eval_accumulator(-4, 9);
    FuelPieces {
        doc_kind: "accumulator self-recursive",
        cert_kind: "accumulator-recursive",
        vars: "n acc",
        bridge: format!(
            r#"-- model-side fuel bridge (fuel induction; the IH is quantified over both args).
theorem {name}_fuel_irrel :
    ∀ (t k1 k2 : Nat) (n acc : Int), n.natAbs < t → n.natAbs < k1 → n.natAbs < k2 →
      {q}__fuel k1 n acc = {q}__fuel k2 n acc := by
  intro t
  induction t with
  | zero => intro k1 k2 n acc ht _ _; omega
  | succ t ih =>
      intro k1 k2 n acc ht h1 h2
      cases k1 with
      | zero => omega
      | succ m1 =>
      cases k2 with
      | zero => omega
      | succ m2 =>
      by_cases hn : n ≤ 0
      · simp [{q}__fuel, hn]
      · have hrec := ih m1 m2 (n - 1) (acc + n) (by omega) (by omega) (by omega)
        simp only [{q}__fuel]
        rw [if_neg hn, if_neg hn, hrec]

theorem {name}_fuel_stable (k : Nat) (n acc : Int) (h : n.natAbs < k) :
    {q}__fuel k n acc = {q} n acc :=
  {name}_fuel_irrel (n.natAbs + k + 1) k (n.natAbs + 1) n acc (by omega) h (by omega)

theorem {name}_step (n acc : Int) (hn : ¬ n ≤ 0) :
    {q} n acc = {q} (n - 1) (acc + n) := by
  have h0 : {q} n acc = {q}__fuel (n.natAbs + 1) n acc := rfl
  rw [h0]
  simp only [{q}__fuel]
  rw [if_neg hn, {name}_fuel_stable n.natAbs (n - 1) (acc + n) (by omega)]

theorem {name}_base (n acc : Int) (hn : n ≤ 0) : {q} n acc = acc := by
  have h0 : {q} n acc = {q}__fuel (n.natAbs + 1) n acc := rfl
  rw [h0]; simp [{q}__fuel, hn]"#
        ),
        comb_hyps: r#"    (add sub : List WVal → Option WVal)
    (hAdd : ∀ a b va vb w, Repr a va → Repr b vb → add [va, vb] = some w → Repr (a + b) w)
    (hSub : ∀ a b va vb w, Repr a va → Repr b vb → sub [va, vb] = some w → Repr (a - b) w) :"#
            .to_string(),
        concl: format!(
            r#"    ∀ (fuel : Nat) (n acc : Int) (vn vacc w : WVal), Repr n vn → Repr acc vacc →
      wFuncN {name}Code ({name}Host add sub) fuel {self_idx} [vn, vacc] = some w →
      Repr ({q} n acc) w := by"#
        ),
        zero_body: "      intro n acc vn vacc w hvn hvacc hrun\n      simp [wFuncN] at hrun"
            .to_string(),
        succ_body: format!(
            r#"      intro n acc vn vacc w hvn hvacc hrun
      rcases hcar n vn hvn with ⟨s, sg, rfl⟩ | ⟨s, lty, les, sg, rfl⟩
      · have hs := hsmall_elim n s sg hvn
        subst hs
        by_cases hle : s ≤ (0 : Int)
        · simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, b32,
            popArgs, initLocals, hle] at hrun
          rw [{name}_base s acc hle, ← hrun]
          exact hvacc
        · simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, b32,
            popArgs, initLocals, hle] at hrun
          rcases hsub : sub [.structv {carrier} [.i64v s, .null, .i32v sg], carrierSmall {carrier} 1] with _ | vd
          · simp [hsub] at hrun
          · simp only [hsub] at hrun
            have hrd : Repr (s - 1) vd :=
              hSub s 1 _ _ vd hvn (hsmall_intro 1) hsub
            rcases hadd : add [vacc, .structv {carrier} [.i64v s, .null, .i32v sg]] with _ | va
            · simp [hadd] at hrun
            · simp only [hadd] at hrun
              have hra : Repr (acc + s) va :=
                hAdd acc s _ _ va hvacc hvn hadd
              rcases hrec : wFuncN {name}Code ({name}Host add sub) fuel {self_idx} [vd, va] with _ | vr
              · simp [hrec] at hrun
              · simp only [hrec, Option.some.injEq] at hrun
                rw [{name}_step s acc hle, ← hrun]
                exact ih (s - 1) (acc + s) vd va vr hrd hra hrec
      · obtain ⟨hsign, hne⟩ := hbig n s lty les sg hvn
        by_cases hlt : sg < (0 : Int)
        · have hn0 : n ≤ 0 := by have := hsign.mp hlt; omega
          simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, b32,
            popArgs, initLocals, hlt] at hrun
          rw [{name}_base n acc hn0, ← hrun]
          exact hvacc
        · have hn0 : ¬ n ≤ 0 := by
            intro hle
            have : ¬ n < 0 := fun h => hlt (hsign.mpr h)
            omega
          simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, b32,
            popArgs, initLocals, hlt] at hrun
          rcases hsub : sub [.structv {carrier} [.i64v s, .arr lty les, .i32v sg], carrierSmall {carrier} 1] with _ | vd
          · simp [hsub] at hrun
          · simp only [hsub] at hrun
            have hrd : Repr (n - 1) vd :=
              hSub n 1 _ _ vd hvn (hsmall_intro 1) hsub
            rcases hadd : add [vacc, .structv {carrier} [.i64v s, .arr lty les, .i32v sg]] with _ | va
            · simp [hadd] at hrun
            · simp only [hadd] at hrun
              have hra : Repr (acc + n) va :=
                hAdd acc n _ _ va hvacc hvn hadd
              rcases hrec : wFuncN {name}Code ({name}Host add sub) fuel {self_idx} [vd, va] with _ | vr
              · simp [hrec] at hrun
              · simp only [hrec, Option.some.injEq] at hrun
                rw [{name}_step n acc hn0, ← hrun]
                exact ih (n - 1) (acc + n) vd va vr hrd hra hrec"#
        ),
        total: String::new(),
        faithful_concl: format!(
            r#"    ∀ (fuel : Nat) (n acc : Int) (vn vacc w : WVal), Repr n vn → Repr acc vacc →
      wFuncN {name}Code ({name}Host add sub) fuel {self_idx} [vn, vacc] = some w →
      ∃ m : Int, Repr m w ∧ m = {q} n acc :="#
        ),
        faithful_body: format!(
            r#"  fun fuel n acc vn vacc w hvn hvacc hrun =>
    ⟨{q} n acc,
     {name}_wasm_certified Repr hcar hsmall_intro hsmall_elim hbig add sub hAdd hSub fuel n acc vn
       vacc w hvn hvacc hrun,
     rfl⟩"#
        ),
        guards: format!(
            r#"def {name}HostRef : HostTbl := {name}Host (addRef {carrier}) (subRef {carrier})
example :
    ((wFuncN {name}Code {name}HostRef 20 {self_idx} [carrierSmall {carrier} 3, carrierSmall {carrier} 0]).bind carrierToInt)
      = some ({g3}) := by native_decide
example :
    ((wFuncN {name}Code {name}HostRef 20 {self_idx} [carrierSmall {carrier} 3, carrierSmall {carrier} 4]).bind carrierToInt)
      = some ({g4}) := by native_decide
example :
    ((wFuncN {name}Code {name}HostRef 20 {self_idx} [carrierSmall {carrier} (-4), carrierSmall {carrier} 9]).bind carrierToInt)
      = some {gneg} := by native_decide"#
        ),
        simulates: format!(
            r#"theorem {name}_simulates : AverCert.Schema.Obligation.holds {name}Ob := by
  intro S add sub mul stringEq stringConcat toIndex hadd hsub hmul hStringEq hStringConcat _hToIndex fuel ns vs w hrepr hrun
  simp only [{name}Ob, AverCert.Schema.Obligation.holds] at hrun ⊢
  obtain ⟨hrepr, harity⟩ := hrepr
  cases hrepr with
  | nil =>
      simp at harity
  | cons hvn htail =>
      rename_i n vn ns1 vs1
      cases htail with
      | nil =>
          simp at harity
      | cons hvacc htail2 =>
          rename_i acc vacc ns2 vs2
          cases htail2 with
          | nil =>
              simpa [AverCert.Schema.intRepr] using {name}_wasm_certified S.Repr S.car S.smallIntro S.smallElim S.bigElim
                add sub hadd hsub fuel n acc vn vacc w hvn hvacc hrun
          | cons _ _ =>
              simp at harity"#
        ),
    }
}

// Composition rendering lives in render_composition.rs.

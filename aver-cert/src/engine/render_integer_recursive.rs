fn recursive_fuel_pieces(c: &Cert, model_info: &ModelInfo) -> FuelPieces {
    let q = c.model_lean_name(model_info);
    let Cert::Recursive {
        name,
        self_idx,
        carrier,
        box_idx,
        add_idx,
        sub_idx,
        base_k,
        rec_first,
        other,
        combinator,
        ..
    } = c
    else {
        unreachable!()
    };
    let (rec_first, other, combinator) = (*rec_first, *other, *combinator);
    // Combinator contract, selected by the model operator: `+`/host `add` vs
    // `*`/host `mul`. `op` is the Lean operator, `cparam` the host param the
    // theorem binds, `chyp` its contract hypothesis, `cref` the reference face for
    // the anti-vacuity guard, `coblig` the obligation's contract in `_simulates`.
    let (op, cparam, chyp, cref, coblig) = match combinator {
        Combinator::Add => ("+", "add", "hAdd", "addRef", "hadd"),
        Combinator::Mul => ("*", "mul", "hMul", "mulRef", "hmul"),
    };
    // base + anti-vacuity guard values are data-driven from the recognised base
    // and combinator.
    let base = lean_int_lit(*base_k);
    // Guards are validated to fit i128 at recognition; expect that here.
    let ev = |s: i64| {
        eval_body_recursion(s, *base_k, other, combinator)
            .expect("guard fits (validated at recognition)")
    };
    let g3 = ev(3);
    let g0 = lean_int_lit128(ev(0));
    let gneg = lean_int_lit128(ev(-4));
    let _ = (box_idx, add_idx, sub_idx);
    // The combinator combines the recursive result `{name}(n-1)` with `other`
    // (the input, or a boxed constant); the proof cites `chyp` with the operands
    // in their recognised order.
    let other_expr = |input: &str| match other {
        BodyOperand::Input => input.to_string(),
        BodyOperand::Const(k) => lean_int_lit(k),
    };
    let step_rhs = {
        let rec_expr = format!("{q} (n - 1)");
        if rec_first {
            format!("{rec_expr} {op} {}", other_expr("n"))
        } else {
            format!("{} {op} {rec_expr}", other_expr("n"))
        }
    };
    // Per carrier arm: `input` names the model int (`s` small / `n` big) and
    // `input_wval` its byte form; produce the `add [..]` operand list and the
    // `hAdd ..` argument prefix (everything before the trailing `hadd`).
    let combinator_arm = |input: &str, input_wval: &str| -> (String, String, String) {
        let other_wval = match other {
            BodyOperand::Input => input_wval.to_string(),
            BodyOperand::Const(k) => format!("carrierSmall {carrier} {}", lean_int_lit(k)),
        };
        let other_repr = match other {
            BodyOperand::Input => "hv".to_string(),
            BodyOperand::Const(k) => format!("(hsmall_intro {})", lean_int_lit(k)),
        };
        let rec_int = format!("({q} ({input} - 1))");
        if rec_first {
            (
                format!("[vr, {other_wval}]"),
                format!("{rec_int} {} _ _ wa hrr {other_repr}", other_expr(input)),
                format!("{rec_int} {} _ _ hrr {other_repr}", other_expr(input)),
            )
        } else {
            (
                format!("[{other_wval}, vr]"),
                format!("{} {rec_int} _ _ wa {other_repr} hrr", other_expr(input)),
                format!("{} {rec_int} _ _ {other_repr} hrr", other_expr(input)),
            )
        }
    };
    let (add_small, hadd_small, hadd_tot_small) = combinator_arm(
        "s",
        &format!(".structv {carrier} [.i64v s, .null, .i32v sg]"),
    );
    let (add_big, hadd_big, hadd_tot_big) = combinator_arm(
        "n",
        &format!(".structv {carrier} [.i64v s, .arr lty les, .i32v sg]"),
    );
    let total = if combinator == Combinator::Add {
        let total_repr_hyps = recursion_repr_hyps(*carrier);
        format!(
            r#"/-- Fuel-parametric progress from the checked `natAbs` measure and
    add/sub totality on represented values. -/
theorem {name}_wasm_total_aux
{total_repr_hyps}
    (add sub : List WVal → Option WVal)
    (hAdd : ∀ a b va vb w, Repr a va → Repr b vb → add [va, vb] = some w → Repr (a + b) w)
    (hSub : ∀ a b va vb w, Repr a va → Repr b vb → sub [va, vb] = some w → Repr (a - b) w)
    (hAddTot : ∀ a b va vb, Repr a va → Repr b vb → ∃ w, add [va, vb] = some w)
    (hSubTot : ∀ a b va vb, Repr a va → Repr b vb → ∃ w, sub [va, vb] = some w) :
    ∀ (fuel : Nat) (n : Int) (v : WVal), Repr n v → n.natAbs < fuel →
      ∃ w, wFuncN {name}Code ({name}Host add sub) fuel {self_idx} [v] = some w ∧
        Repr ({q} n) w := by
  intro fuel
  induction fuel with
  | zero => intro n v hv hlt; omega
  | succ fuel ih =>
      intro n v hv hlt
      rcases hcar n v hv with ⟨s, sg, rfl⟩ | ⟨s, lty, les, sg, rfl⟩
      · have hs := hsmall_elim n s sg hv
        subst hs
        by_cases hle : s ≤ (0 : Int)
        · refine ⟨carrierSmall {carrier} {base}, ?_, ?_⟩
          · simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, b32,
              popArgs, initLocals, hle]
          · rw [{name}_base s hle]; exact hsmall_intro {base}
        · obtain ⟨vd, hsub⟩ := hSubTot s 1 _ (carrierSmall {carrier} 1) hv (hsmall_intro 1)
          have hrd : Repr (s - 1) vd := hSub s 1 _ _ vd hv (hsmall_intro 1) hsub
          obtain ⟨vr, hrec, hrr⟩ := ih (s - 1) vd hrd (by omega)
          obtain ⟨wa, hadd⟩ := hAddTot {hadd_tot_small}
          refine ⟨wa, ?_, ?_⟩
          · simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, b32,
              popArgs, initLocals, hle, hsub, hrec, hadd]
          · rw [{name}_step s hle]; exact hAdd {hadd_small} hadd
      · obtain ⟨hsign, hne⟩ := hbig n s lty les sg hv
        by_cases hlt2 : sg < (0 : Int)
        · have hn0 : n ≤ 0 := by have := hsign.mp hlt2; omega
          refine ⟨carrierSmall {carrier} {base}, ?_, ?_⟩
          · simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, b32,
              popArgs, initLocals, hlt2]
          · rw [{name}_base n hn0]; exact hsmall_intro {base}
        · have hn0 : ¬ n ≤ 0 := by
            intro hle
            have : ¬ n < 0 := fun h => hlt2 (hsign.mpr h)
            omega
          obtain ⟨vd, hsub⟩ := hSubTot n 1 _ (carrierSmall {carrier} 1) hv (hsmall_intro 1)
          have hrd : Repr (n - 1) vd := hSub n 1 _ _ vd hv (hsmall_intro 1) hsub
          obtain ⟨vr, hrec, hrr⟩ := ih (n - 1) vd hrd (by omega)
          obtain ⟨wa, hadd⟩ := hAddTot {hadd_tot_big}
          refine ⟨wa, ?_, ?_⟩
          · simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, b32,
              popArgs, initLocals, hlt2, hsub, hrec, hadd]
          · rw [{name}_step n hn0]; exact hAdd {hadd_big} hadd

#print axioms {name}_wasm_total_aux

/-- TOTAL correctness at the fuel selected by the checked `intNatAbs` witness. -/
theorem {name}_wasm_total
{total_repr_hyps}
    (add sub : List WVal → Option WVal)
    (hAdd : ∀ a b va vb w, Repr a va → Repr b vb → add [va, vb] = some w → Repr (a + b) w)
    (hSub : ∀ a b va vb w, Repr a va → Repr b vb → sub [va, vb] = some w → Repr (a - b) w)
    (hAddTot : ∀ a b va vb, Repr a va → Repr b vb → ∃ w, add [va, vb] = some w)
    (hSubTot : ∀ a b va vb, Repr a va → Repr b vb → ∃ w, sub [va, vb] = some w) :
    ∀ (n : Int) (v : WVal), Repr n v →
      ∃ w, wFuncN {name}Code ({name}Host add sub) (n.natAbs + 1) {self_idx} [v] = some w ∧
        Repr ({q} n) w :=
  fun n v hv =>
    {name}_wasm_total_aux Repr hcar hsmall_intro hsmall_elim hbig add sub hAdd hSub
      hAddTot hSubTot (n.natAbs + 1) n v hv (by omega)

#print axioms {name}_wasm_total

theorem {name}_simulates_total : AverCert.Schema.Obligation.holdsTotal {name}Ob := by
  intro S add sub mul stringEq stringConcat toIndex cmp eq hadd hsub hmul hStringEq hStringConcat
    _hToIndex _hCmp _hEq hAddTot hSubTot x vs hDom
  simp only [{name}Ob] at hDom ⊢
  obtain ⟨hrepr, harity⟩ := hDom
  cases hrepr with
  | nil => simp at harity
  | cons hv htail =>
      rename_i n v ns vs
      cases htail with
      | nil =>
          refine ⟨n, v, [], rfl, hv, ?_⟩
          simpa [{name}Ob, AverCert.Schema.intRepr] using
            {name}_wasm_total S.Repr S.car S.smallIntro S.smallElim S.bigElim
              add sub hadd hsub hAddTot hSubTot n v hv
      | cons _ _ => simp at harity
"#
        )
    } else {
        String::new()
    };
    FuelPieces {
        doc_kind: "self-recursive",
        cert_kind: "recursive",
        vars: "n",
        bridge: format!(
            r#"-- model-side fuel bridge (the cap-induction pattern at R = 1).
theorem {name}_fuel_irrel :
    ∀ (t k1 k2 : Nat) (n : Int), n.natAbs < t → n.natAbs < k1 → n.natAbs < k2 →
      {q}__fuel k1 n = {q}__fuel k2 n := by
  intro t
  induction t with
  | zero => intro k1 k2 n ht _ _; omega
  | succ t ih =>
      intro k1 k2 n ht h1 h2
      cases k1 with
      | zero => omega
      | succ m1 =>
      cases k2 with
      | zero => omega
      | succ m2 =>
      by_cases hn : n ≤ 0
      · simp [{q}__fuel, hn]
      · have hrec := ih m1 m2 (n - 1) (by omega) (by omega) (by omega)
        simp only [{q}__fuel]
        rw [if_neg hn, if_neg hn, hrec]

theorem {name}_fuel_stable (k : Nat) (n : Int) (h : n.natAbs < k) :
    {q}__fuel k n = {q} n :=
  {name}_fuel_irrel (n.natAbs + k + 1) k (n.natAbs + 1) n (by omega) h (by omega)

theorem {name}_step (n : Int) (hn : ¬ n ≤ 0) : {q} n = {step_rhs} := by
  have h0 : {q} n = {q}__fuel (n.natAbs + 1) n := rfl
  rw [h0]
  simp only [{q}__fuel]
  rw [if_neg hn, {name}_fuel_stable n.natAbs (n - 1) (by omega)]

theorem {name}_base (n : Int) (hn : n ≤ 0) : {q} n = {base} := by
  have h0 : {q} n = {q}__fuel (n.natAbs + 1) n := rfl
  rw [h0]; simp [{q}__fuel, hn]"#
        ),
        comb_hyps: format!(
            r#"    ({cparam} sub : List WVal → Option WVal)
    ({chyp} : ∀ a b va vb w, Repr a va → Repr b vb → {cparam} [va, vb] = some w → Repr (a {op} b) w)
    (hSub : ∀ a b va vb w, Repr a va → Repr b vb → sub [va, vb] = some w → Repr (a - b) w) :"#
        ),
        concl: format!(
            r#"    ∀ (fuel : Nat) (n : Int) (v w : WVal), Repr n v →
      wFuncN {name}Code ({name}Host {cparam} sub) fuel {self_idx} [v] = some w →
      Repr ({q} n) w := by"#
        ),
        zero_body: "      intro n v w hv hrun\n      simp [wFuncN] at hrun".to_string(),
        succ_body: format!(
            r#"      intro n v w hv hrun
      rcases hcar n v hv with ⟨s, sg, rfl⟩ | ⟨s, lty, les, sg, rfl⟩
      · have hs := hsmall_elim n s sg hv
        subst hs
        by_cases hle : s ≤ (0 : Int)
        · simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, b32,
            popArgs, initLocals, hle] at hrun
          rw [{name}_base s hle, ← hrun]
          exact hsmall_intro {base}
        · simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, b32,
            popArgs, initLocals, hle] at hrun
          rcases hsub : sub [.structv {carrier} [.i64v s, .null, .i32v sg], carrierSmall {carrier} 1] with _ | vd
          · simp [hsub] at hrun
          · simp only [hsub] at hrun
            have hrd : Repr (s - 1) vd :=
              hSub s 1 _ _ vd hv (hsmall_intro 1) hsub
            rcases hrec : wFuncN {name}Code ({name}Host {cparam} sub) fuel {self_idx} [vd] with _ | vr
            · simp [hrec] at hrun
            · simp only [hrec] at hrun
              have hrr : Repr ({q} (s - 1)) vr := ih (s - 1) vd vr hrd hrec
              rcases hadd : {cparam} {add_small} with _ | wa
              · simp [hadd] at hrun
              · simp only [hadd, Option.some.injEq] at hrun
                rw [{name}_step s hle, ← hrun]
                exact {chyp} {hadd_small} hadd
      · obtain ⟨hsign, hne⟩ := hbig n s lty les sg hv
        by_cases hlt : sg < (0 : Int)
        · have hn0 : n ≤ 0 := by have := hsign.mp hlt; omega
          simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, b32,
            popArgs, initLocals, hlt] at hrun
          rw [{name}_base n hn0, ← hrun]
          exact hsmall_intro {base}
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
              hSub n 1 _ _ vd hv (hsmall_intro 1) hsub
            rcases hrec : wFuncN {name}Code ({name}Host {cparam} sub) fuel {self_idx} [vd] with _ | vr
            · simp [hrec] at hrun
            · simp only [hrec] at hrun
              have hrr : Repr ({q} (n - 1)) vr := ih (n - 1) vd vr hrd hrec
              rcases hadd : {cparam} {add_big} with _ | wa
              · simp [hadd] at hrun
              · simp only [hadd, Option.some.injEq] at hrun
                rw [{name}_step n hn0, ← hrun]
                exact {chyp} {hadd_big} hadd"#
        ),
        total,
        faithful_concl: format!(
            r#"    ∀ (fuel : Nat) (n : Int) (v w : WVal), Repr n v →
      wFuncN {name}Code ({name}Host {cparam} sub) fuel {self_idx} [v] = some w →
      ∃ m : Int, Repr m w ∧ m = {q} n :="#
        ),
        faithful_body: format!(
            r#"  fun fuel n v w hv hrun =>
    ⟨{q} n,
     {name}_wasm_certified Repr hcar hsmall_intro hsmall_elim hbig {cparam} sub {chyp} hSub fuel n v w hv hrun,
     rfl⟩"#
        ),
        guards: format!(
            r#"def {name}HostRef : HostTbl := {name}Host ({cref} {carrier}) (subRef {carrier})
example :
    ((wFuncN {name}Code {name}HostRef 20 {self_idx} [carrierSmall {carrier} 3]).bind carrierToInt)
      = some ({g3}) := by native_decide
example :
    ((wFuncN {name}Code {name}HostRef 20 {self_idx} [carrierSmall {carrier} 0]).bind carrierToInt)
      = some {g0} := by native_decide
example :
    ((wFuncN {name}Code {name}HostRef 20 {self_idx} [carrierSmall {carrier} (-4)]).bind carrierToInt)
      = some {gneg} := by native_decide"#
        ),
        simulates: format!(
            r#"theorem {name}_simulates : AverCert.Schema.Obligation.holds {name}Ob := by
  intro S add sub mul stringEq stringConcat toIndex cmp eq hadd hsub hmul hStringEq hStringConcat _hToIndex _hCmp _hEq fuel ns vs w hrepr hrun
  simp only [{name}Ob, AverCert.Schema.Obligation.holds] at hrun ⊢
  obtain ⟨hrepr, harity⟩ := hrepr
  cases hrepr with
  | nil =>
      simp at harity
  | cons hv htail =>
      rename_i n v ns vs
      cases htail with
      | nil =>
          simpa [AverCert.Schema.intRepr] using {name}_wasm_certified S.Repr S.car S.smallIntro S.smallElim S.bigElim
            {cparam} sub {coblig} hsub fuel n v w hv hrun
      | cons _ _ =>
          simp at harity"#
        ),
    }
}

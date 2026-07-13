/// One conjunct's proof block inside `{primary}_mutual_sim`: the carrier-sign
/// dispatch for member `m`, whose base arm boxes `m.base_k` and whose step arm
/// tail-calls its cross target (self index `m.cross_idx`, model `cross_name`),
/// discharged by the matching conjunct `cross_ih` of the fuel induction
/// hypothesis. A verbatim port of the validated probe block, parameterised by
/// name, carrier and the byte-derived indices/literals.
fn mutual_sim_member_block(
    primary: &str,
    c_ty: u32,
    m: &MutualMember,
    cross_name: &str,
    cross_ih: &str,
) -> String {
    let base_lit = lean_int_lit(m.base_k);
    let cs = m.cross_idx;
    let mn = &m.name;
    format!(
        r#"      · intro n v w hv hrun
        rcases hcar n v hv with ⟨s, sg, rfl⟩ | ⟨s, lty, les, sg, rfl⟩
        · have hs := hsmall_elim n s sg hv; subst hs
          by_cases hle : s ≤ (0 : Int)
          · simp [wFuncN, wRunF, {primary}Code, {primary}Host, boxRef, b32, popArgs, initLocals, hle] at hrun
            rw [{mn}_base s hle, ← hrun]; exact hsmall_intro {base_lit}
          · simp [wFuncN, wRunF, {primary}Code, {primary}Host, boxRef, b32, popArgs, initLocals, hle] at hrun
            rcases hsub : sub [.structv {c_ty} [.i64v s, .null, .i32v sg], carrierSmall {c_ty} 1] with _ | vd
            · simp [hsub] at hrun
            · simp only [hsub] at hrun
              have hrd : Repr (s - 1) vd := hSub s 1 _ _ vd hv (hsmall_intro 1) hsub
              rcases hrec : wFuncN {primary}Code ({primary}Host sub) fuel {cs} [vd] with _ | vr
              · simp [hrec] at hrun
              · simp only [hrec] at hrun
                have hrr : Repr ({cross_name} (s - 1)) vr := {cross_ih} (s - 1) vd vr hrd hrec
                rw [{mn}_step s hle]; rw [Option.some.injEq] at hrun; rw [← hrun]; exact hrr
        · obtain ⟨hsign, hne⟩ := hbig n s lty les sg hv
          by_cases hlt : sg < (0 : Int)
          · have hn0 : n ≤ 0 := by have := hsign.mp hlt; omega
            simp [wFuncN, wRunF, {primary}Code, {primary}Host, boxRef, b32, popArgs, initLocals, hlt] at hrun
            rw [{mn}_base n hn0, ← hrun]; exact hsmall_intro {base_lit}
          · have hn0 : ¬ n ≤ 0 := by
              intro hle; have : ¬ n < 0 := fun h => hlt (hsign.mpr h); omega
            simp [wFuncN, wRunF, {primary}Code, {primary}Host, boxRef, b32, popArgs, initLocals, hlt] at hrun
            rcases hsub : sub [.structv {c_ty} [.i64v s, .arr lty les, .i32v sg], carrierSmall {c_ty} 1] with _ | vd
            · simp [hsub] at hrun
            · simp only [hsub] at hrun
              have hrd : Repr (n - 1) vd := hSub n 1 _ _ vd hv (hsmall_intro 1) hsub
              rcases hrec : wFuncN {primary}Code ({primary}Host sub) fuel {cs} [vd] with _ | vr
              · simp [hrec] at hrun
              · simp only [hrec] at hrun
                have hrr : Repr ({cross_name} (n - 1)) vr := {cross_ih} (n - 1) vd vr hrd hrec
                rw [{mn}_step n hn0]; rw [Option.some.injEq] at hrun; rw [← hrun]; exact hrr"#,
    )
}

/// Forward-progress mirror of `mutual_sim_member_block`. The cross call is
/// supplied by the matching conjunction IH, while sub-totality supplies the
/// concrete descended carrier value. There is no arithmetic combinator in the
/// mutual countdown family, so add-totality is intentionally unused here.
fn mutual_total_member_block(
    primary: &str,
    c_ty: u32,
    m: &MutualMember,
    cross_ih: &str,
) -> String {
    let base_lit = lean_int_lit(m.base_k);
    let mn = &m.name;
    format!(
        r#"      · intro n v hv hbound
        rcases hcar n v hv with ⟨s, sg, rfl⟩ | ⟨s, lty, les, sg, rfl⟩
        · have hs := hsmall_elim n s sg hv; subst hs
          by_cases hle : s ≤ (0 : Int)
          · refine ⟨carrierSmall {c_ty} {base_lit}, ?_, ?_⟩
            · simp [wFuncN, wRunF, {primary}Code, {primary}Host, boxRef, b32,
                popArgs, initLocals, hle]
            · rw [{mn}_base s hle]; exact hsmall_intro {base_lit}
          · obtain ⟨vd, hsub⟩ := hSubTot s 1 _ (carrierSmall {c_ty} 1) hv (hsmall_intro 1)
            have hrd : Repr (s - 1) vd := hSub s 1 _ _ vd hv (hsmall_intro 1) hsub
            obtain ⟨vr, hrec, hrr⟩ := {cross_ih} (s - 1) vd hrd (by omega)
            refine ⟨vr, ?_, ?_⟩
            · simp [wFuncN, wRunF, {primary}Code, {primary}Host, boxRef, b32,
                popArgs, initLocals, hle, hsub, hrec]
            · rw [{mn}_step s hle]; exact hrr
        · obtain ⟨hsign, hne⟩ := hbig n s lty les sg hv
          by_cases hlt : sg < (0 : Int)
          · have hn0 : n ≤ 0 := by have := hsign.mp hlt; omega
            refine ⟨carrierSmall {c_ty} {base_lit}, ?_, ?_⟩
            · simp [wFuncN, wRunF, {primary}Code, {primary}Host, boxRef, b32,
                popArgs, initLocals, hlt]
            · rw [{mn}_base n hn0]; exact hsmall_intro {base_lit}
          · have hn0 : ¬ n ≤ 0 := by
              intro hle; have : ¬ n < 0 := fun h => hlt (hsign.mpr h); omega
            obtain ⟨vd, hsub⟩ := hSubTot n 1 _ (carrierSmall {c_ty} 1) hv (hsmall_intro 1)
            have hrd : Repr (n - 1) vd := hSub n 1 _ _ vd hv (hsmall_intro 1) hsub
            obtain ⟨vr, hrec, hrr⟩ := {cross_ih} (n - 1) vd hrd (by omega)
            refine ⟨vr, ?_, ?_⟩
            · simp [wFuncN, wRunF, {primary}Code, {primary}Host, boxRef, b32,
                popArgs, initLocals, hlt, hsub, hrec]
            · rw [{mn}_step n hn0]; exact hrr"#,
    )
}

/// The certificate block for a mutually-recursive SCC. Emitted ONCE, by the
/// primary (lowest-`self_idx`) member; the shared block proves ONE conjunction
/// `induction fuel` over all members (`{primary}_mutual_sim`), each cross-call
/// discharged by the matching conjunct of the IH, on top of a conjunction
/// fuel-irrelevance bridge and per-member base/step lemmas. Every member's
/// obligation cites its conjunct. (Two-member SCC for now; the k>2 conjunction
/// is a follow-up.)
fn render_mutual_recursion_cert(c: &Cert) -> String {
    let Cert::MutualRecursion {
        scc,
        carrier,
        position,
        ..
    } = c.inner()
    else {
        unreachable!()
    };
    if *position != 0 {
        // The shared block is emitted once, by the primary member.
        return String::new();
    }
    let k = scc.len();
    let c_ty = *carrier;
    let primary = &scc[0].name;
    // The SCC position of each member's cross-call target.
    let cross_pos: Vec<usize> = scc
        .iter()
        .map(|m| {
            scc.iter()
                .position(|x| x.self_idx == m.cross_idx)
                .expect("cross target is an SCC member")
        })
        .collect();
    let holes = std::iter::repeat_n("?_", k).collect::<Vec<_>>().join(", ");

    let mut s = String::new();
    s.push_str(&format!(
        "/-! ### {primary} — mutual-recursive SCC certificate (carrier type {c_ty}) -/\n\n"
    ));

    // ---- model-side fuel bridge (conjunction; one cap-induction at R = 1) ----
    let bridge_conj = scc
        .iter()
        .map(|m| format!("{m}__fuel k1 n = {m}__fuel k2 n", m = m.name))
        .collect::<Vec<_>>()
        .join(" ∧ ");
    let all_fuel = scc
        .iter()
        .map(|m| format!("{}__fuel", m.name))
        .collect::<Vec<_>>()
        .join(", ");
    let hb_names = (0..k)
        .map(|i| format!("hb{i}"))
        .collect::<Vec<_>>()
        .join(", ");
    let bridge_rewrites = scc
        .iter()
        .enumerate()
        .map(|(p, m)| {
            format!(
                "        · simp only [{m}__fuel]; rw [if_neg hn, if_neg hn, hb{cp}]",
                m = m.name,
                cp = cross_pos[p],
            )
        })
        .collect::<Vec<_>>()
        .join("\n");
    s.push_str(&format!(
        r#"-- model-side fuel bridge over the whole SCC (the cap-induction pattern at R = 1).
theorem {primary}_fuel_irrel :
    ∀ (t k1 k2 : Nat) (n : Int), n.natAbs < t → n.natAbs < k1 → n.natAbs < k2 →
      {bridge_conj} := by
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
      · refine ⟨{holes}⟩ <;> simp [{all_fuel}, hn]
      · have hstep : (n - 1).natAbs < t := by
          have h1n : (1 : Int) ≤ n := by omega
          have h2n : (n - 1).natAbs = n.natAbs - 1 := by omega
          omega
        obtain ⟨{hb_names}⟩ := ih m1 m2 (n - 1) hstep (by omega) (by omega)
        refine ⟨{holes}⟩
{bridge_rewrites}

"#,
    ));

    // ---- per-member base/step lemmas -----------------------------------------
    for (p, m) in scc.iter().enumerate() {
        let cn = &scc[cross_pos[p]].name;
        let base_lit = lean_int_lit(m.base_k);
        let proj = conjunct_proj(cross_pos[p], k);
        let mn = &m.name;
        s.push_str(&format!(
            r#"theorem {mn}_base (n : Int) (hn : n ≤ 0) : {mn} n = {base_lit} := by
  simp [{mn}, {mn}__fuel, hn]

theorem {mn}_step (n : Int) (hn : ¬ n ≤ 0) : {mn} n = {cn} (n - 1) := by
  simp only [{mn}, {mn}__fuel]
  rw [if_neg hn]
  show {cn}__fuel n.natAbs (n - 1) = {cn} (n - 1)
  rw [{cn}]
  exact ({primary}_fuel_irrel (n.natAbs + (n - 1).natAbs + 2) n.natAbs ((n - 1).natAbs + 1)
    (n - 1) (by omega) (by omega) (by omega)){proj}

"#,
        ));
    }

    // ---- the conjunction simulation theorem (one `induction fuel`) -----------
    let sim_conj = scc
        .iter()
        .map(|m| {
            format!(
                "      (∀ n v w, Repr n v →\n        wFuncN {primary}Code ({primary}Host sub) fuel {self_idx} [v] = some w → Repr ({m} n) w)",
                self_idx = m.self_idx,
                m = m.name,
            )
        })
        .collect::<Vec<_>>()
        .join(" ∧\n");
    let ih_names = (0..k)
        .map(|i| format!("ih{i}"))
        .collect::<Vec<_>>()
        .join(", ");
    let member_blocks = scc
        .iter()
        .enumerate()
        .map(|(p, m)| {
            mutual_sim_member_block(
                primary,
                c_ty,
                m,
                &scc[cross_pos[p]].name,
                &format!("ih{}", cross_pos[p]),
            )
        })
        .collect::<Vec<_>>()
        .join("\n");
    s.push_str(&format!(
        r#"/-- THE CERTIFICATE THEOREM: partial correctness of the VERBATIM emitted mutual
    bodies against the generated models, for ALL n : ℤ, as ONE conjunction proven
    by a single `induction fuel` (each cross-call discharged by the matching IH
    conjunct). -/
theorem {primary}_mutual_sim
    (Repr : Int → WVal → Prop)
    (hcar : ∀ n v, Repr n v →
      (∃ s sg, v = .structv {c_ty} [.i64v s, .null, .i32v sg]) ∨
      (∃ s lty les sg, v = .structv {c_ty} [.i64v s, .arr lty les, .i32v sg]))
    (hsmall_intro : ∀ k : Int, Repr k (carrierSmall {c_ty} k))
    (hsmall_elim : ∀ n s sg, Repr n (.structv {c_ty} [.i64v s, .null, .i32v sg]) → s = n)
    (hbig : ∀ n s lty les sg,
      Repr n (.structv {c_ty} [.i64v s, .arr lty les, .i32v sg]) → ((sg < 0) ↔ (n < 0)) ∧ n ≠ 0)
    (sub : List WVal → Option WVal)
    (hSub : ∀ a b va vb w, Repr a va → Repr b vb → sub [va, vb] = some w → Repr (a - b) w) :
    ∀ (fuel : Nat),
{sim_conj} := by
  intro fuel
  induction fuel with
  | zero =>
      refine ⟨{holes}⟩ <;> · intro n v w hv hrun; simp [wFuncN] at hrun
  | succ fuel ih =>
      obtain ⟨{ih_names}⟩ := ih
      refine ⟨{holes}⟩
{member_blocks}

#print axioms {primary}_mutual_sim

"#,
    ));

    // ---- total correctness over the same conjunction induction -------------
    let total_aux_conj = scc
        .iter()
        .map(|m| {
            format!(
                "      (∀ n v, Repr n v → n.natAbs < fuel →\n        ∃ w, wFuncN {primary}Code ({primary}Host sub) fuel {self_idx} [v] = some w ∧ Repr ({m} n) w)",
                self_idx = m.self_idx,
                m = m.name,
            )
        })
        .collect::<Vec<_>>()
        .join(" ∧\n");
    let total_member_blocks = scc
        .iter()
        .enumerate()
        .map(|(p, m)| {
            mutual_total_member_block(
                primary,
                c_ty,
                m,
                &format!("ih{}", cross_pos[p]),
            )
        })
        .collect::<Vec<_>>()
        .join("\n");
    let total_conj = scc
        .iter()
        .map(|m| {
            format!(
                "    (∀ n v, Repr n v →\n      ∃ w, wFuncN {primary}Code ({primary}Host sub) (n.natAbs + 1) {self_idx} [v] = some w ∧ Repr ({m} n) w)",
                self_idx = m.self_idx,
                m = m.name,
            )
        })
        .collect::<Vec<_>>()
        .join(" ∧\n");
    let total_fixed_blocks = (0..k)
        .map(|p| {
            let proj = conjunct_proj(p, k);
            format!(
                "  · intro n v hv\n    exact ({primary}_mutual_total_aux Repr hcar hsmall_intro hsmall_elim hbig sub hSub hSubTot (n.natAbs + 1)){proj} n v hv (by omega)"
            )
        })
        .collect::<Vec<_>>()
        .join("\n");
    s.push_str(&format!(
        r#"/-- Fuel-parametric progress for every member of the byte-closed integer
    countdown SCC, proved by one conjunction induction. -/
theorem {primary}_mutual_total_aux
    (Repr : Int → WVal → Prop)
    (hcar : ∀ n v, Repr n v →
      (∃ s sg, v = .structv {c_ty} [.i64v s, .null, .i32v sg]) ∨
      (∃ s lty les sg, v = .structv {c_ty} [.i64v s, .arr lty les, .i32v sg]))
    (hsmall_intro : ∀ k : Int, Repr k (carrierSmall {c_ty} k))
    (hsmall_elim : ∀ n s sg, Repr n (.structv {c_ty} [.i64v s, .null, .i32v sg]) → s = n)
    (hbig : ∀ n s lty les sg,
      Repr n (.structv {c_ty} [.i64v s, .arr lty les, .i32v sg]) → ((sg < 0) ↔ (n < 0)) ∧ n ≠ 0)
    (sub : List WVal → Option WVal)
    (hSub : ∀ a b va vb w, Repr a va → Repr b vb → sub [va, vb] = some w → Repr (a - b) w)
    (hSubTot : ∀ a b va vb, Repr a va → Repr b vb → ∃ w, sub [va, vb] = some w) :
    ∀ (fuel : Nat),
{total_aux_conj} := by
  intro fuel
  induction fuel with
  | zero =>
      refine ⟨{holes}⟩ <;> · intro n v hv hbound; omega
  | succ fuel ih =>
      obtain ⟨{ih_names}⟩ := ih
      refine ⟨{holes}⟩
{total_member_blocks}

#print axioms {primary}_mutual_total_aux

/-- TOTAL correctness of the SCC at the fuel selected by each member's checked
    `intNatAbs` witness. The statement is a conjunction, so no member can be
    promoted independently of its cross-call hypotheses. -/
theorem {primary}_mutual_total
    (Repr : Int → WVal → Prop)
    (hcar : ∀ n v, Repr n v →
      (∃ s sg, v = .structv {c_ty} [.i64v s, .null, .i32v sg]) ∨
      (∃ s lty les sg, v = .structv {c_ty} [.i64v s, .arr lty les, .i32v sg]))
    (hsmall_intro : ∀ k : Int, Repr k (carrierSmall {c_ty} k))
    (hsmall_elim : ∀ n s sg, Repr n (.structv {c_ty} [.i64v s, .null, .i32v sg]) → s = n)
    (hbig : ∀ n s lty les sg,
      Repr n (.structv {c_ty} [.i64v s, .arr lty les, .i32v sg]) → ((sg < 0) ↔ (n < 0)) ∧ n ≠ 0)
    (sub : List WVal → Option WVal)
    (hSub : ∀ a b va vb w, Repr a va → Repr b vb → sub [va, vb] = some w → Repr (a - b) w)
    (hSubTot : ∀ a b va vb, Repr a va → Repr b vb → ∃ w, sub [va, vb] = some w) :
{total_conj} := by
  refine ⟨{holes}⟩
{total_fixed_blocks}

#print axioms {primary}_mutual_total

"#,
    ));

    // Stable per-export theorem names used by the JSON report.
    for (p, m) in scc.iter().enumerate() {
        let proj = conjunct_proj(p, k);
        let mn = &m.name;
        s.push_str(&format!(
            r#"theorem {mn}_wasm_total
    (Repr : Int → WVal → Prop)
    (hcar : ∀ n v, Repr n v →
      (∃ s sg, v = .structv {c_ty} [.i64v s, .null, .i32v sg]) ∨
      (∃ s lty les sg, v = .structv {c_ty} [.i64v s, .arr lty les, .i32v sg]))
    (hsmall_intro : ∀ k : Int, Repr k (carrierSmall {c_ty} k))
    (hsmall_elim : ∀ n s sg, Repr n (.structv {c_ty} [.i64v s, .null, .i32v sg]) → s = n)
    (hbig : ∀ n s lty les sg,
      Repr n (.structv {c_ty} [.i64v s, .arr lty les, .i32v sg]) → ((sg < 0) ↔ (n < 0)) ∧ n ≠ 0)
    (sub : List WVal → Option WVal)
    (hSub : ∀ a b va vb w, Repr a va → Repr b vb → sub [va, vb] = some w → Repr (a - b) w)
    (hSubTot : ∀ a b va vb, Repr a va → Repr b vb → ∃ w, sub [va, vb] = some w) :
    ∀ n v, Repr n v →
      ∃ w, wFuncN {primary}Code ({primary}Host sub) (n.natAbs + 1) {self_idx} [v] = some w ∧
        Repr ({mn} n) w :=
  ({primary}_mutual_total Repr hcar hsmall_intro hsmall_elim hbig sub hSub hSubTot){proj}

#print axioms {mn}_wasm_total

"#,
            self_idx = m.self_idx,
        ));
    }

    // ---- anti-vacuity: the emitted bodies actually RUN -----------------------
    s.push_str(&format!(
        "-- anti-vacuity: the emitted bodies actually RUN on concrete inputs.\n\
         def {primary}HostRef : HostTbl := {primary}Host (subRef {c_ty})\n"
    ));
    for (p, m) in scc.iter().enumerate() {
        for &input in &[4i64, 3, -1] {
            let expected = eval_mutual(scc, p, input);
            s.push_str(&format!(
                "example :\n    \
                 ((wFuncN {primary}Code {primary}HostRef 40 {self_idx} [carrierSmall {c_ty} {inp}]).bind carrierToInt)\n      \
                 = some {exp} := by native_decide\n",
                self_idx = m.self_idx,
                inp = lean_int_lit(input),
                exp = lean_int_lit128(expected as i128),
            ));
        }
    }

    // ---- per-member schema obligations (each cites its conjunct) -------------
    for (p, m) in scc.iter().enumerate() {
        let proj = conjunct_proj(p, k);
        let mn = &m.name;
        s.push_str(&format!(
            r#"
/-- Schema-shaped simulation obligation for `{mn}` (composed by the single final
    theorem): the emitted mutual body simulates the model `{mn}` via the matching
    conjunct of `{primary}_mutual_sim`. -/
theorem {mn}_simulates : AverCert.Schema.Obligation.holds {mn}Ob := by
  intro S add sub mul stringEq stringConcat hadd hsub hmul hStringEq hStringConcat fuel ns vs w hrepr hrun
  simp only [{mn}Ob, AverCert.Schema.Obligation.holds] at hrun ⊢
  obtain ⟨hrepr, harity⟩ := hrepr
  cases hrepr with
  | nil =>
      simp at harity
  | cons hv htail =>
      rename_i n v ns vs
      cases htail with
      | nil =>
          simpa [AverCert.Schema.intRepr] using ({primary}_mutual_sim S.Repr S.car S.smallIntro S.smallElim S.bigElim
            sub hsub fuel){proj} n v w hv hrun
      | cons _ _ =>
          simp at harity
"#,
        ));
        s.push_str(&format!(
            r#"
theorem {mn}_simulates_total : AverCert.Schema.Obligation.holdsTotal {mn}Ob := by
  intro S add sub mul stringEq stringConcat hadd hsub hmul hStringEq hStringConcat
    hAddTot hSubTot x vs hDom
  simp only [{mn}Ob] at hDom ⊢
  obtain ⟨hrepr, harity⟩ := hDom
  cases hrepr with
  | nil => simp at harity
  | cons hv htail =>
      rename_i n v ns vs
      cases htail with
      | nil =>
          refine ⟨n, v, [], rfl, hv, ?_⟩
          simpa [{mn}Ob, AverCert.Schema.intRepr] using
            {mn}_wasm_total S.Repr S.car S.smallIntro S.smallElim S.bigElim
              sub hsub hSubTot n v hv
      | cons _ _ => simp at harity
"#,
        ));
    }

    s
}

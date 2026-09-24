-- law: Domain.ScriptParse.parse.directPushRunsPastTheEnd
-- target: btc-laws
-- module: Domain.ScriptParse
-- theorem: parse_law_directPushRunsPastTheEnd
-- parity run: Lean bounded, Dafny universal

theorem core_simp : STATEMENT := by
  intro n h
  simp only [Bool.and_eq_true, decide_eq_true_eq, ge_iff_le] at h
  obtain ⟨h1, h2⟩ := h
  have hpd : Domain.Opcode.isPushData n = true := by
    simp only [Domain.Opcode.isPushData, Bool.or_eq_true, Bool.and_eq_true, decide_eq_true_eq]
    omega
  have hw : Domain.Opcode.pushWidth n = 0 := by
    unfold Domain.Opcode.pushWidth
    split <;> omega
  have hn : ¬ ((0 : Int) = n) := by omega
  simp [Domain.ScriptParse.parse, Domain.ScriptParse.from', Domain.ScriptParse.nextOp,
    Domain.ScriptParse.pushOf, Domain.ScriptParse.taken, hpd, hw, hn]

theorem core_rw : STATEMENT := by
  intro n h
  simp only [Bool.and_eq_true, decide_eq_true_eq, ge_iff_le] at h
  obtain ⟨h1, h2⟩ := h
  have hpd : Domain.Opcode.isPushData n = true := by
    simp only [Domain.Opcode.isPushData, Bool.or_eq_true, Bool.and_eq_true, decide_eq_true_eq]
    omega
  have hw : Domain.Opcode.pushWidth n = 0 := by
    unfold Domain.Opcode.pushWidth
    split <;> omega
  rw [Domain.ScriptParse.parse, Domain.ScriptParse.from', Domain.ScriptParse.nextOp]
  simp only [hpd, ite_true, ↓reduceIte]
  rw [Domain.ScriptParse.pushOf, hw]
  rw [Domain.ScriptParse.taken]
  have hn : ¬ (((([] : List Int).take n.toNat).length : Int) == n) = true := by simp; omega
  simp only [hn, ite_false, Bool.false_eq_true, ↓reduceIte]

theorem core_grind : STATEMENT := by
  intro n h
  simp only [Bool.and_eq_true, decide_eq_true_eq, ge_iff_le] at h
  grind [Domain.ScriptParse.parse, Domain.ScriptParse.from', Domain.ScriptParse.nextOp,
    Domain.ScriptParse.pushOf, Domain.ScriptParse.taken, Domain.Opcode.isPushData, Domain.Opcode.pushWidth]

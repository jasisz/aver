-- law: ParityProbe.bip341Set.isValidHashType (probe)
-- target: btc-probe
-- module: ParityProbe
-- theorem: bip341Set_law_isValidHashType
-- parity run: Lean hard error (breaks the build), Dafny universal

theorem core_split_omega : STATEMENT := by
  intro h
  unfold bip341Set Domain.Bip341.validHashType
  split <;> simp_all <;> omega

theorem core_split_first : STATEMENT := by
  intro h
  unfold bip341Set Domain.Bip341.validHashType
  split <;> first
    | decide
    | rfl
    | (simp only [Bool.or_eq_false_iff, Bool.and_eq_false_iff, decide_eq_false_iff_not]; omega)
    | (simp; omega)
    | omega

theorem core_grind : STATEMENT := by
  intro h
  grind [bip341Set, Domain.Bip341.validHashType]

-- law: Domain.Remainder.signsDifferent_8_2_2.branchPositive
-- target: k5
-- module: Domain.Remainder
-- theorem: signsDifferent_8_2_2_law_branchPositive
-- parity run: Lean sorry, Dafny postcondition failure (neither; extra, not one of the 17)

-- What the exporter emits today (unfold, grind), for reference.
theorem core_auto : STATEMENT := by
  intro x y z h
  simp only [Domain.Rational.absInt, Domain.Remainder.signsDifferent_8_2_2, Bool.and_eq_true, decide_eq_true_eq] at h ⊢
  grind

-- Core lemmas only: one product-sign lemma picked by hand, factors by omega.
theorem core_lemmas : STATEMENT := by
  intro x y z h
  simp only [Domain.Rational.absInt, Domain.Remainder.signsDifferent_8_2_2, Bool.and_eq_true, decide_eq_true_eq] at h ⊢
  exact Int.mul_nonpos_of_nonneg_of_nonpos (by omega) (by omega)

-- Kit: the product-sign fact for the product in the claim, then omega.
theorem kit_sign_omega : STATEMENT := by
  intro x y z h
  have s := AverKit.mul_sign (x - y) (y - z)
  simp only [Domain.Rational.absInt, Domain.Remainder.signsDifferent_8_2_2, Bool.and_eq_true, decide_eq_true_eq] at h ⊢
  omega

theorem kit_sign_grind : STATEMENT := by
  intro x y z h
  have s := AverKit.mul_sign (x - y) (y - z)
  simp only [Domain.Rational.absInt, Domain.Remainder.signsDifferent_8_2_2, Bool.and_eq_true, decide_eq_true_eq] at h ⊢
  grind

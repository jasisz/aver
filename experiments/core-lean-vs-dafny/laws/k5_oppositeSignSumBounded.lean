-- law: Domain.Remainder.oppositeSignSumBounded.guarded
-- target: k5
-- module: Domain.Remainder
-- theorem: oppositeSignSumBounded_law_guarded
-- parity run: Lean sorry, Dafny universal

-- What the exporter emits today (unfold, grind), for reference.
theorem core_auto : STATEMENT := by
  intro alpha beta h
  simp only [Domain.Rational.absInt, Domain.Remainder.maxInt, Domain.Remainder.oppositeSignSumBounded, decide_eq_true_eq] at h ⊢
  grind

-- Core lemmas only: the two strict product-sign facts, then omega.
theorem core_lemmas : STATEMENT := by
  intro alpha beta h
  simp only [Domain.Rational.absInt, Domain.Remainder.maxInt, Domain.Remainder.oppositeSignSumBounded, decide_eq_true_eq] at h ⊢
  have s1 : 0 < alpha → 0 < beta → 0 < alpha * beta := Int.mul_pos
  have s2 : alpha < 0 → beta < 0 → 0 < alpha * beta := Int.mul_pos_of_neg_of_neg
  omega

-- Kit: the product-sign fact for the product in the hypothesis, then omega.
theorem kit_sign_omega : STATEMENT := by
  intro alpha beta h
  have s := AverKit.mul_sign alpha beta
  simp only [Domain.Rational.absInt, Domain.Remainder.maxInt, Domain.Remainder.oppositeSignSumBounded, decide_eq_true_eq] at h ⊢
  omega

-- Kit: the same fact, closed by grind.
theorem kit_sign_grind : STATEMENT := by
  intro alpha beta h
  have s := AverKit.mul_sign alpha beta
  simp only [Domain.Rational.absInt, Domain.Remainder.maxInt, Domain.Remainder.oppositeSignSumBounded, decide_eq_true_eq] at h ⊢
  grind

-- Core lemmas only, cited as implications, closed by grind.
theorem core_lemmas_grind : STATEMENT := by
  intro alpha beta h
  have s1 : 0 < alpha → 0 < beta → 0 < alpha * beta := Int.mul_pos
  have s2 : alpha < 0 → beta < 0 → 0 < alpha * beta := Int.mul_pos_of_neg_of_neg
  simp only [Domain.Rational.absInt, Domain.Remainder.maxInt, Domain.Remainder.oppositeSignSumBounded, decide_eq_true_eq] at h ⊢
  grind

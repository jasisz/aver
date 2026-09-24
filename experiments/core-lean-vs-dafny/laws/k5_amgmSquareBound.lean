-- law: Domain.FloorLaws.squareLeSumOfSquares.amgmSquareBound
-- target: k5
-- module: Domain.FloorLaws
-- theorem: squareLeSumOfSquares_law_amgmSquareBound
-- parity run: Lean bounded, Dafny universal

theorem core_auto : STATEMENT := by
  intro a b h
  simp only [Domain.FloorLaws.squareLeSumOfSquares, decide_eq_true_eq] at h ⊢
  grind

-- Core lemmas only: square nonnegativity by a sign case split.
theorem core_lemmas : STATEMENT := by
  intro a b h
  simp only [Domain.FloorLaws.squareLeSumOfSquares, decide_eq_true_eq]
  rcases Int.le_total 0 b with hb | hb
  · have := Int.mul_nonneg hb hb
    omega
  · have := Int.mul_nonneg_of_nonpos_of_nonpos hb hb
    omega

-- Kit: the atom square is nonnegative.
theorem kit_sq_omega : STATEMENT := by
  intro a b h
  have := AverKit.sq_nonneg b
  simp only [Domain.FloorLaws.squareLeSumOfSquares, decide_eq_true_eq]
  omega

theorem kit_sq_grind : STATEMENT := by
  intro a b h
  have := AverKit.sq_nonneg b
  simp only [Domain.FloorLaws.squareLeSumOfSquares, decide_eq_true_eq]
  grind

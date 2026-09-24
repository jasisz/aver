-- law: Domain.Recip.reciprocalBound.lemma_8_1_1
-- target: k5
-- module: Domain.Recip
-- theorem: reciprocalBound_law_lemma_8_1_1
-- parity run: Lean sorry, Dafny universal

-- Generic automation only: unfold the rational order and let grind try.
theorem core_auto : STATEMENT := by
  intro d sd2 a0 a1 s1 s2 h
  simp only [Bool.and_eq_true] at h
  obtain ⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨_, _⟩, _⟩, _⟩, hc4⟩, _⟩, _⟩, _⟩, _⟩, _⟩, hc10⟩ := h
  simp only [Domain.Recip.reciprocalBound, Domain.Rational.lessThan, decide_eq_true_eq] at hc4 hc10 ⊢
  grind

-- The exporter's own emitted chain (its helper lemmas copied verbatim, core
-- lemmas only), with the one wrong name fixed: the export unfolds
-- `LawsEntry.reciprocalBound`, which does not exist, so the whole arm fails
-- and the law falls back to `sorry`.
theorem helper_sq_nonneg (x : Int) : 0 ≤ x * x := by
  cases Int.le_total 0 x with
  | inl h => exact Int.mul_nonneg h h
  | inr h => exact Int.mul_nonneg_of_nonpos_of_nonpos h h
theorem helper_sq_pos {x : Int} (hx : x ≠ 0) : 0 < x * x := by
  cases Int.lt_or_gt_of_ne hx with
  | inl h => exact Int.mul_pos_of_neg_of_neg h h
  | inr h => exact Int.mul_pos h h
theorem helper_frac_lt_imp_le (a b : Domain.Rational.Fraction) (h : Domain.Rational.lessThan a b = true) :
    Domain.Rational.isNonNeg (Domain.Rational.minus b a) = true := by
  simp only [Domain.Rational.lessThan, Domain.Rational.isNonNeg, Domain.Rational.minus, decide_eq_true_eq, ge_iff_le] at h ⊢
  have hid : (b.top*a.bottom - a.top*b.bottom) * (b.bottom*a.bottom)
           = (b.top*b.bottom)*(a.bottom*a.bottom) - (a.top*a.bottom)*(b.bottom*b.bottom) := by grind
  omega
theorem helper_frac_le_trans (a b cc : Domain.Rational.Fraction) (hb : b.bottom ≠ 0)
    (hab : Domain.Rational.isNonNeg (Domain.Rational.minus b a) = true) (hbc : Domain.Rational.isNonNeg (Domain.Rational.minus cc b) = true) :
    Domain.Rational.isNonNeg (Domain.Rational.minus cc a) = true := by
  simp only [Domain.Rational.isNonNeg, Domain.Rational.minus, decide_eq_true_eq, ge_iff_le] at hab hbc ⊢
  have hP : 0 ≤ ((b.top*a.bottom - a.top*b.bottom) * (b.bottom*a.bottom)) * (cc.bottom*cc.bottom) :=
    Int.mul_nonneg hab (helper_sq_nonneg cc.bottom)
  have hQ : 0 ≤ ((cc.top*b.bottom - b.top*cc.bottom) * (cc.bottom*b.bottom)) * (a.bottom*a.bottom) :=
    Int.mul_nonneg hbc (helper_sq_nonneg a.bottom)
  have hid : ((b.top*a.bottom - a.top*b.bottom) * (b.bottom*a.bottom)) * (cc.bottom*cc.bottom)
              + ((cc.top*b.bottom - b.top*cc.bottom) * (cc.bottom*b.bottom)) * (a.bottom*a.bottom)
            = (b.bottom*b.bottom) * ((cc.top*a.bottom - a.top*cc.bottom) * (cc.bottom*a.bottom)) := by
    grind
  have hsum : 0 ≤ (b.bottom*b.bottom) * ((cc.top*a.bottom - a.top*cc.bottom) * (cc.bottom*a.bottom)) := by
    rw [← hid]; omega
  exact Int.nonneg_of_mul_nonneg_right hsum (helper_sq_pos hb)
theorem helper_lessThan_right_bottom_ne (x y : Domain.Rational.Fraction) (h : Domain.Rational.lessThan x y = true) : y.bottom ≠ 0 := by
  intro h0
  simp only [Domain.Rational.lessThan, decide_eq_true_eq] at h
  rw [h0] at h
  simp only [Int.mul_zero, Int.zero_mul] at h
  omega
theorem helper_frac_lt_le_trans (a b cc : Domain.Rational.Fraction) (hc : cc.bottom ≠ 0)
    (hab : Domain.Rational.lessThan a b = true) (hbc : Domain.Rational.isNonNeg (Domain.Rational.minus cc b) = true) :
    Domain.Rational.lessThan a cc = true := by
  have hb : b.bottom ≠ 0 := helper_lessThan_right_bottom_ne a b hab
  simp only [Domain.Rational.lessThan, Domain.Rational.isNonNeg, Domain.Rational.minus, decide_eq_true_eq, ge_iff_le] at hab hbc ⊢
  have hab' : 0 < (b.top*a.bottom - a.top*b.bottom) * (b.bottom*a.bottom) := by
    have hid_ab : (b.top*a.bottom - a.top*b.bottom) * (b.bottom*a.bottom)
                = (b.top*b.bottom)*(a.bottom*a.bottom) - (a.top*a.bottom)*(b.bottom*b.bottom) := by grind
    omega
  have hP : 0 < ((b.top*a.bottom - a.top*b.bottom) * (b.bottom*a.bottom)) * (cc.bottom*cc.bottom) :=
    Int.mul_pos hab' (helper_sq_pos hc)
  have hQ : 0 ≤ ((cc.top*b.bottom - b.top*cc.bottom) * (cc.bottom*b.bottom)) * (a.bottom*a.bottom) :=
    Int.mul_nonneg hbc (helper_sq_nonneg a.bottom)
  have hid : ((b.top*a.bottom - a.top*b.bottom) * (b.bottom*a.bottom)) * (cc.bottom*cc.bottom)
              + ((cc.top*b.bottom - b.top*cc.bottom) * (cc.bottom*b.bottom)) * (a.bottom*a.bottom)
            = (b.bottom*b.bottom) * ((cc.top*a.bottom - a.top*cc.bottom) * (cc.bottom*a.bottom)) := by
    grind
  have hsum : 0 < (b.bottom*b.bottom) * ((cc.top*a.bottom - a.top*cc.bottom) * (cc.bottom*a.bottom)) := by
    rw [← hid]; omega
  have hT3nn : 0 ≤ (cc.top*a.bottom - a.top*cc.bottom) * (cc.bottom*a.bottom) :=
    Int.nonneg_of_mul_nonneg_right (Int.le_of_lt hsum) (helper_sq_pos hb)
  have hT3ne : (cc.top*a.bottom - a.top*cc.bottom) * (cc.bottom*a.bottom) ≠ 0 := by
    intro h0; rw [h0, Int.mul_zero] at hsum; omega
  have hid_goal : (cc.top*a.bottom - a.top*cc.bottom) * (cc.bottom*a.bottom)
                = (cc.top*cc.bottom)*(a.bottom*a.bottom) - (a.top*a.bottom)*(cc.bottom*cc.bottom) := by grind
  omega

set_option maxHeartbeats 4000000 in
theorem core_exporter_chain_fixed : STATEMENT := by
  intro d sd2 a0 a1 s1 s2 h_when
  simp only [Bool.and_eq_true] at h_when
  obtain ⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨_, _⟩, _⟩, _⟩, hc4⟩, _⟩, _⟩, _⟩, _⟩, _⟩, hc10⟩ := h_when
  simp only [Domain.Recip.reciprocalBound]
  have h0 := hc4
  have e1 := helper_frac_lt_imp_le (Domain.Rational.plus (Domain.Rational.times a1 a1) s2) (Domain.Recip.finalErrorBudget) hc10
  have g2 : Domain.Rational.lessThan (Domain.Recip.finalErrorBudget) (Domain.Recip.reciprocalErrorBound) = true := by decide
  have e2 := helper_frac_lt_imp_le (Domain.Recip.finalErrorBudget) (Domain.Recip.reciprocalErrorBound) g2
  have acc := helper_frac_le_trans (Domain.Rational.plus (Domain.Rational.times a1 a1) s2) (Domain.Recip.finalErrorBudget) (Domain.Recip.reciprocalErrorBound) (by decide) e1 e2
  exact helper_frac_lt_le_trans (Domain.Rational.absFraction (Domain.Rational.minus (Domain.Rational.times d sd2) Domain.Rational.oneFraction)) (Domain.Rational.plus (Domain.Rational.times a1 a1) s2) (Domain.Recip.reciprocalErrorBound) (by decide) h0 acc

-- Kit: transitivity of the cross-multiplied order, cited twice.
theorem kit_cross_lt_trans : STATEMENT := by
  intro d sd2 a0 a1 s1 s2 h
  simp only [Bool.and_eq_true] at h
  obtain ⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨_, _⟩, _⟩, _⟩, hc4⟩, _⟩, _⟩, _⟩, _⟩, _⟩, hc10⟩ := h
  have g : Domain.Rational.lessThan Domain.Recip.finalErrorBudget Domain.Recip.reciprocalErrorBound = true := by
    decide
  simp only [Domain.Recip.reciprocalBound, Domain.Rational.lessThan, decide_eq_true_eq] at hc4 hc10 g ⊢
  exact AverKit.cross_lt_trans hc4 (AverKit.cross_lt_trans hc10 g)

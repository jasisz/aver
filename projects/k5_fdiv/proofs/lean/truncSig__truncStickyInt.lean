-- HAND PROOF (labeled, kernel-checked) for truncSig.truncStickyInt -- the integer-model
-- trunc-sticky composition (Section 7.3 shape). The generic engine cannot find this; this
-- body is spliced verbatim after `:= by` and re-checked by lake every build.
-- COMPOSITION BY CITATION: it cites the generated theorems `pow2_law_homomorphism` and
-- `pow2_law_positive` (both auto-proven, emitted earlier in the same module).
-- Pure core: axioms = {propext, Classical.choice, Quot.sound}. No Mathlib, no sorry.
  have peel : ∀ a d : Int, d ≠ 0 → floorDiv a d = a / d := by
    intro a d hd
    have hne : ¬ ((d == 0) = true) := by simp only [beq_iff_eq]; exact hd
    simp only [floorDiv]
    rw [if_neg hne]
    simp only [Except.withDefault]
  have half_lemma : ∀ (q s S : Int), 0 ≤ s → s < 2 → 0 < S →
      (2 * q + s) / (2 * S) = q / S := by
    intro q s S hs0 hs2 hS
    have hc : (2 * S) ≠ 0 := by omega
    have hmod0 : 0 ≤ q % S := Int.emod_nonneg q (by omega)
    have hmodS : q % S < S := Int.emod_lt_of_pos q hS
    have hdec : S * (q / S) + q % S = q := Int.ediv_add_emod q S
    have e1 : 2 * q + s = (2 * (q % S) + s) + (2 * S) * (q / S) := by
      rw [Int.mul_assoc]; omega
    rw [e1, Int.add_mul_ediv_left _ _ hc]
    have hz : (2 * (q % S) + s) / (2 * S) = 0 := Int.ediv_eq_zero_of_lt (by omega) (by omega)
    rw [hz]; omega
  have ppos : ∀ k : Int, 1 ≤ pow2 k := by
    intro k
    have h := pow2_law_positive k
    simpa only [ge_iff_le, eq_iff_iff, iff_true] using h
  have hom : ∀ m n : Int, 0 ≤ m → 0 ≤ n → pow2 (m + n) = pow2 m * pow2 n := by
    intro m n hm hn
    apply pow2_law_homomorphism m n
    simp only [Bool.and_eq_true, decide_eq_true_eq, ge_iff_le]
    exact ⟨hm, hn⟩
  intro A dn jt h_when
  simp only [Bool.and_eq_true, decide_eq_true_eq, ge_iff_le] at h_when
  obtain ⟨hdn, hjt⟩ := h_when
  have hPpos : 0 < pow2 dn := by have := ppos dn; omega
  have hSpos : 0 < pow2 (jt - 1) := by have := ppos (jt - 1); omega
  have hQ : pow2 jt = 2 * pow2 (jt - 1) := by
    rw [pow2.eq_def]; rw [if_neg (show ¬(jt ≤ 0) by omega)]
  have hPS : pow2 ((dn + jt) - 1) = pow2 dn * pow2 (jt - 1) := by
    rw [← hom dn (jt - 1) hdn (by omega)]; congr 1; omega
  have hPne : pow2 dn ≠ 0 := by omega
  have hQne : pow2 jt ≠ 0 := by rw [hQ]; omega
  have hPSne : pow2 ((dn + jt) - 1) ≠ 0 := by have := ppos ((dn + jt) - 1); omega
  have hAPS : A / (pow2 dn * pow2 (jt - 1)) = (A / pow2 dn) / pow2 (jt - 1) :=
    (Int.ediv_ediv_of_nonneg (Int.le_of_lt hPpos)).symm
  simp only [truncSig, stickySig]
  rw [peel A (pow2 dn) hPne, peel _ (pow2 jt) hQne, peel A (pow2 ((dn + jt) - 1)) hPSne]
  rw [hQ, hPS, hAPS]
  split
  · rename_i hcond
    have h0 := half_lemma (A / pow2 dn) 0 (pow2 (jt - 1)) (by omega) (by omega) hSpos
    simp only [Int.add_zero] at h0
    exact h0
  · rename_i hcond
    exact half_lemma (A / pow2 dn) 1 (pow2 (jt - 1)) (by omega) (by omega) hSpos

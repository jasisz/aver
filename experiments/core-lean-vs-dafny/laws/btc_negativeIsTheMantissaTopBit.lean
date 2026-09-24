-- law: Domain.Chainwork.negative.isTheMantissaTopBit
-- target: btc-laws
-- module: Domain.Chainwork
-- theorem: negative_law_isTheMantissaTopBit
-- parity run: Lean sorry, Dafny postcondition failure (neither)

theorem core_auto : STATEMENT := by
  intro bits
  simp [Domain.Chainwork.negative, AverBits.and, AverBits.mag]
  omega

theorem core_grind : STATEMENT := by
  intro bits
  grind [Domain.Chainwork.negative, AverBits.and, AverBits.mag]

-- Kit: split on the sign, read the mask through Nat, one bit lemma, omega.
theorem kit_mask : STATEMENT := by
  intro bits
  unfold Domain.Chainwork.negative
  rcases Int.lt_or_le bits 0 with hn | hp
  · rw [AverKit.and_of_neg bits 8388608 8388608 (by decide) hn, AverKit.nat_land_bit _ 8388608 23 (by decide)]
    simp only [Bool.eq_iff_iff, beq_iff_eq, bne_iff_ne]
    omega
  · rw [AverKit.and_of_nonneg bits 8388608 8388608 (by decide) hp, AverKit.nat_land_bit _ 8388608 23 (by decide)]
    simp only [Bool.eq_iff_iff, beq_iff_eq, bne_iff_ne]
    omega

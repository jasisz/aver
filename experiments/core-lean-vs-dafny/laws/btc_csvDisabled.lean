-- law: ParityProbe.csvDisabled.isBitThirtyOne (probe, #352)
-- target: btc-probe
-- module: ParityProbe
-- theorem: csvDisabled_law_isBitThirtyOne
-- parity run: Lean sorry, Dafny postcondition failure (neither)

theorem core_auto : STATEMENT := by
  intro v
  simp [csvDisabled, AverBits.and, AverBits.mag]
  omega

theorem core_grind : STATEMENT := by
  intro v
  grind [csvDisabled, AverBits.and, AverBits.mag]

-- Kit: split on the sign, read the mask through Nat, one bit lemma, omega.
theorem kit_mask : STATEMENT := by
  intro v
  unfold csvDisabled
  rcases Int.lt_or_le v 0 with hn | hp
  · rw [AverKit.and_of_neg v 2147483648 2147483648 (by decide) hn, AverKit.nat_land_bit _ 2147483648 31 (by decide)]
    simp only [Bool.eq_iff_iff, beq_iff_eq, bne_iff_ne]
    omega
  · rw [AverKit.and_of_nonneg v 2147483648 2147483648 (by decide) hp, AverKit.nat_land_bit _ 2147483648 31 (by decide)]
    simp only [Bool.eq_iff_iff, beq_iff_eq, bne_iff_ne]
    omega

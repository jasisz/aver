-- law: ParityProbe.anyoneCanPayMask.agreesWithIsAnyoneCanPay (probe, #353)
-- target: btc-probe
-- module: ParityProbe
-- theorem: anyoneCanPayMask_law_agreesWithIsAnyoneCanPay
-- parity run: Lean sorry, Dafny postcondition failure (neither)

theorem core_auto : STATEMENT := by
  intro h
  simp [anyoneCanPayMask, Domain.Sighash.isAnyoneCanPay, AverBits.and, AverBits.mag]
  omega

theorem core_grind : STATEMENT := by
  intro h
  grind [anyoneCanPayMask, Domain.Sighash.isAnyoneCanPay, AverBits.and, AverBits.mag]

-- Kit: split on the sign, read the mask through Nat, one bit lemma, omega.
theorem kit_mask : STATEMENT := by
  intro h
  unfold anyoneCanPayMask Domain.Sighash.isAnyoneCanPay
  rcases Int.lt_or_le h 0 with hn | hp
  · rw [AverKit.and_of_neg h 128 128 (by decide) hn, AverKit.nat_land_bit _ 128 7 (by decide)]
    simp only [Bool.eq_iff_iff, beq_iff_eq, bne_iff_ne]
    omega
  · rw [AverKit.and_of_nonneg h 128 128 (by decide) hp, AverKit.nat_land_bit _ 128 7 (by decide)]
    simp only [Bool.eq_iff_iff, beq_iff_eq, bne_iff_ne]
    omega

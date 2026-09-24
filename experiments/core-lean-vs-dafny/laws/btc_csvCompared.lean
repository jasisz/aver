-- law: ParityProbe.csvCompared.isTypeBitAndCount (probe, #352, mask 0x0040FFFF)
-- target: btc-probe
-- module: ParityProbe
-- theorem: csvCompared_law_isTypeBitAndCount
-- parity run: Lean sorry, Dafny postcondition failure (neither)

theorem core_auto : STATEMENT := by
  intro v
  simp [csvCompared, AverBits.and, AverBits.mag]
  omega

-- Kit: 0x0040FFFF = 2^16 * 64 + (2^16 - 1): split at bit 16, then one bit and one low run.
theorem kit_mask : STATEMENT := by
  intro v
  unfold csvCompared
  rcases Int.lt_or_le v 0 with hn | hp
  · rw [AverKit.and_of_neg v 4259839 4259839 (by decide) hn,
      AverKit.nat_land_split _ 4259839 65536 16 64 65535 (by decide) (by decide) (by decide),
      AverKit.nat_land_bit _ 64 6 (by decide),
      AverKit.nat_land_low _ 65535 65536 16 (by decide) (by decide)]
    omega
  · rw [AverKit.and_of_nonneg v 4259839 4259839 (by decide) hp,
      AverKit.nat_land_split _ 4259839 65536 16 64 65535 (by decide) (by decide) (by decide),
      AverKit.nat_land_bit _ 64 6 (by decide),
      AverKit.nat_land_low _ 65535 65536 16 (by decide) (by decide)]
    omega

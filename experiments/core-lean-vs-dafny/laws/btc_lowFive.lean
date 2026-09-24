-- law: ParityProbe.lowFive.isModThirtyTwo (probe, #353)
-- target: btc-probe
-- module: ParityProbe
-- theorem: lowFive_law_isModThirtyTwo
-- parity run: Lean sorry, Dafny postcondition failure (neither)

theorem core_auto : STATEMENT := by
  intro h
  simp [lowFive, AverBits.and, AverBits.mag]
  omega

-- Core lemma `Nat.and_two_pow_sub_one_eq_mod`, cited by hand (the dafny-parity what-if).
theorem core_lemma : STATEMENT := by
  intro h
  have land31 : ∀ x : Nat, Nat.land x 31 = x % 32 := fun x => Nat.and_two_pow_sub_one_eq_mod x 5
  unfold lowFive AverBits.and AverBits.mag
  by_cases hh : h < 0
  · simp only [hh, if_true, show ¬((31 : Int) < 0) by decide, if_false,
      show (31 : Int).toNat = 31 from rfl, land31]
    omega
  · simp only [hh, if_false, show ¬((31 : Int) < 0) by decide,
      show (31 : Int).toNat = 31 from rfl, land31]
    omega

theorem kit_mask : STATEMENT := by
  intro h
  unfold lowFive
  rcases Int.lt_or_le h 0 with hn | hp
  · rw [AverKit.and_of_neg h 31 31 (by decide) hn, AverKit.nat_land_low _ 31 32 5 (by decide) (by decide)]
    omega
  · rw [AverKit.and_of_nonneg h 31 31 (by decide) hp, AverKit.nat_land_low _ 31 32 5 (by decide) (by decide)]
    omega

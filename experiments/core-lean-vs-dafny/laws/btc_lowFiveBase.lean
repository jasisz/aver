-- law: ParityProbe.lowFiveBase.agreesWithBaseOf (probe, #353)
-- target: btc-probe
-- module: ParityProbe
-- theorem: lowFiveBase_law_agreesWithBaseOf
-- parity run: Lean sorry, Dafny postcondition failure (neither)

theorem core_auto : STATEMENT := by
  intro h
  simp [lowFiveBase, Domain.Sighash.baseOf, AverBits.and, AverBits.mag]

theorem core_lemma : STATEMENT := by
  intro h
  have land31 : ∀ x : Nat, Nat.land x 31 = x % 32 := fun x => Nat.and_two_pow_sub_one_eq_mod x 5
  have e : AverBits.and h 31 = h % 32 := by
    unfold AverBits.and AverBits.mag
    by_cases hh : h < 0
    · simp only [hh, if_true, show ¬((31 : Int) < 0) by decide, if_false,
        show (31 : Int).toNat = 31 from rfl, land31]
      omega
    · simp only [hh, if_false, show ¬((31 : Int) < 0) by decide,
        show (31 : Int).toNat = 31 from rfl, land31]
      omega
  unfold lowFiveBase
  rw [e]
  unfold Domain.Sighash.baseOf
  rw [Int.emod_emod]

theorem kit_mask : STATEMENT := by
  intro h
  have e : AverBits.and h 31 = h % 32 := by
    rcases Int.lt_or_le h 0 with hn | hp
    · rw [AverKit.and_of_neg h 31 31 (by decide) hn, AverKit.nat_land_low _ 31 32 5 (by decide) (by decide)]
      omega
    · rw [AverKit.and_of_nonneg h 31 31 (by decide) hp, AverKit.nat_land_low _ 31 32 5 (by decide) (by decide)]
      omega
  unfold lowFiveBase
  rw [e]
  first
  | (unfold Domain.Sighash.baseOf; rw [Int.emod_emod])
  | (simp only [Domain.Sighash.baseOf, Int.emod_emod])

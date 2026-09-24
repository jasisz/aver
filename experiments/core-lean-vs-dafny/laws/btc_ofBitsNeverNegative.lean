-- law: Domain.Chainwork.ofBits.neverNegative
-- target: btc-laws
-- module: Domain.Chainwork
-- theorem: ofBits_law_neverNegative
-- parity run: Lean sorry, Dafny postcondition failure (neither)

theorem core_auto : STATEMENT := by
  intro bits
  simp [Domain.Chainwork.ofBits, Domain.Chainwork.ceiling, Domain.Chainwork.perTarget,
    Domain.Block.scaled, Domain.Block.targetOf, Domain.Chainwork.usable]
  grind

-- Two accumulator-sign helpers by induction on the countdown (the shape a
-- generic "accumulator keeps its sign" step would emit), then core lemmas.
theorem helper_powerOf_nonneg (base : Int) (hb : 0 ≤ base) (n : Nat) :
    ∀ (e acc : Int), e.toNat ≤ n → 0 ≤ acc → 0 ≤ Domain.Block.powerOf base e acc := by
  induction n with
  | zero =>
    intro e acc he ha
    unfold Domain.Block.powerOf
    split
    · exact ha
    · omega
  | succ n ih =>
    intro e acc he ha
    unfold Domain.Block.powerOf
    split
    · exact ha
    · exact ih (e - 1) (acc * base) (by omega) (Int.mul_nonneg ha hb)

theorem helper_doubled_nonneg (n : Nat) :
    ∀ (t acc : Int), t.toNat ≤ n → 0 ≤ acc → 0 ≤ Domain.Chainwork.doubled t acc := by
  induction n with
  | zero =>
    intro t acc ht ha
    unfold Domain.Chainwork.doubled
    split
    · exact ha
    · omega
  | succ n ih =>
    intro t acc ht ha
    unfold Domain.Chainwork.doubled
    split
    · exact ha
    · exact ih (t - 1) (acc * 2) (by omega) (Int.mul_nonneg ha (by decide))

theorem helper_targetOf_nonneg (b : Int) : 0 ≤ Domain.Block.targetOf b := by
  have hm : 0 ≤ b % 16777216 := Int.emod_nonneg _ (by decide)
  have hp : ∀ e : Int, 0 ≤ Domain.Block.powerOf 256 e 1 := fun e =>
    helper_powerOf_nonneg 256 (by decide) e.toNat e 1 (Nat.le_refl _) (by decide)
  unfold Domain.Block.targetOf Domain.Block.scaled
  split
  · exact Int.mul_nonneg hm (hp _)
  · split
    · simp [Except.withDefault]
    · simp only [Except.withDefault]
      exact Int.ediv_nonneg hm (hp _)

theorem core_helpers : STATEMENT := by
  intro bits
  have hc : 0 ≤ Domain.Chainwork.ceiling := by
    unfold Domain.Chainwork.ceiling
    exact helper_doubled_nonneg 256 256 1 (by decide) (by decide)
  have ht := helper_targetOf_nonneg bits
  simp only [ge_iff_le, decide_eq_true_eq]
  unfold Domain.Chainwork.ofBits Domain.Chainwork.perTarget
  split
  · split
    · simp [Except.withDefault]
    · simp only [Except.withDefault]
      exact Int.ediv_nonneg hc (by omega)
  · exact Int.le_refl 0

-- law: Domain.StackItem.isMinimalPush.directPushIsMinimalUnlessSmallNumber
-- target: btc-interp
-- module: Domain.StackItem
-- theorem: isMinimalPush_law_directPushIsMinimalUnlessSmallNumber
-- parity run: Lean bounded, Dafny universal

theorem core_simp_grind : STATEMENT := by
  intro b h
  simp only [Bool.and_eq_true, decide_eq_true_eq, ge_iff_le] at h
  simp [Domain.StackItem.isMinimalPush, Domain.StackItem.singleByteMinimal, Domain.StackItem.asNumber,
    Domain.StackItem.valued, Domain.StackItem.signedBy, Domain.StackItem.bigEndian,
    Domain.StackItem.unsigned, Domain.StackItem.smallOrDirect]
  grind

theorem core_cases : STATEMENT := by
  intro b h
  simp only [Bool.and_eq_true, decide_eq_true_eq, ge_iff_le] at h
  obtain ⟨h0, h255⟩ := h
  have hv : Domain.StackItem.asNumber [b] = (if 128 ≤ b then 0 - (b - 128) else b) := by
    simp [Domain.StackItem.asNumber, Domain.StackItem.valued, Domain.StackItem.signedBy,
      Domain.StackItem.bigEndian, Domain.StackItem.unsigned]
    split <;> omega
  simp only [Domain.StackItem.isMinimalPush, List.length_singleton, Nat.cast_one,
    Domain.StackItem.singleByteMinimal, hv, Domain.StackItem.smallOrDirect]
  by_cases hb : 128 ≤ b
  · simp only [hb, ite_true, ↓reduceIte]
    by_cases h129 : b = 129
    · subst h129; decide
    · by_cases hs : 1 ≤ b ∧ b ≤ 16
      · omega
      · simp_all
        omega
  · simp only [hb, ite_false, ↓reduceIte]
    by_cases hs : 1 ≤ b ∧ b ≤ 16
    · simp_all
      omega
    · simp_all
      omega

theorem core_grind : STATEMENT := by
  intro b h
  simp only [Bool.and_eq_true, decide_eq_true_eq, ge_iff_le] at h
  grind [Domain.StackItem.isMinimalPush, Domain.StackItem.singleByteMinimal, Domain.StackItem.asNumber,
    Domain.StackItem.valued, Domain.StackItem.signedBy, Domain.StackItem.bigEndian,
    Domain.StackItem.unsigned, Domain.StackItem.smallOrDirect]

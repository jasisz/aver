-- law: Domain.Segment.place.staysUnderCap
-- target: btc-laws
-- module: Domain.Segment
-- theorem: place_law_staysUnderCap
-- parity run: Lean bounded, Dafny universal

theorem core_grind : STATEMENT := by
  intro state blockBytes h
  grind [Domain.Segment.place, Domain.Segment.rolls, Domain.Segment.rolled, Domain.Segment.appended,
    Domain.Segment.headerBytes, Domain.Segment.capBytes]

theorem core_split_omega : STATEMENT := by
  intro state blockBytes h
  simp only [Bool.and_eq_true, decide_eq_true_eq, Domain.Segment.headerBytes, Domain.Segment.capBytes] at h ⊢
  unfold Domain.Segment.place
  split
  · simp only [Domain.Segment.rolled, Domain.Segment.headerBytes]
    omega
  · rename_i hr
    simp only [Domain.Segment.appended, Domain.Segment.headerBytes]
    simp only [Domain.Segment.rolls, Domain.Segment.headerBytes, Domain.Segment.capBytes] at hr
    split at hr
    · simp only [decide_eq_true_eq] at hr
      omega
    · omega

theorem core_simp_split : STATEMENT := by
  intro state blockBytes h
  simp only [Domain.Segment.place, Domain.Segment.rolls, Domain.Segment.rolled, Domain.Segment.appended,
    Domain.Segment.headerBytes, Domain.Segment.capBytes, Bool.and_eq_true, decide_eq_true_eq] at h ⊢
  split <;> simp_all <;> omega

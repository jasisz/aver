-- law: Domain.Watchdog.unheard.aPoolWithSomebodyKeepsTheRule
-- target: btc-laws
-- module: Domain.Watchdog
-- theorem: unheard_law_aPoolWithSomebodyKeepsTheRule
-- parity run: Lean bounded, Dafny universal

theorem core_grind : STATEMENT := by
  intro silentMs peers dialling h
  grind [Domain.Watchdog.unheard, Domain.Watchdog.unheardFor]

theorem core_cases : STATEMENT := by
  intro silentMs peers dialling h
  simp only [decide_eq_true_eq] at h
  have hp : (peers == 0) = false := by simp; omega
  unfold Domain.Watchdog.unheard Domain.Watchdog.unheardFor
  by_cases hs : silentMs < 150000 <;> simp [hs, hp]

theorem core_cases_bool : STATEMENT := by
  intro silentMs peers dialling h
  simp only [decide_eq_true_eq] at h
  have hp : peers ≠ 0 := by omega
  unfold Domain.Watchdog.unheard Domain.Watchdog.unheardFor
  cases dialling <;> by_cases hs : silentMs < 150000 <;> simp_all

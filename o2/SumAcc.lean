import AverCommon

set_option linter.unusedVariables false

/-- Tail-recursive sum: returns acc + total(xs). -/
def sumTR (xs : List Int) (acc : Int) : Int :=
  match h_11 : xs with
  | [] => acc
  | h :: t => sumTR t (acc + h)
termination_by xs.length
decreasing_by
  decreasing_tactic

/-- Sum of a list via a tail-recursive worker with neutral initial accumulator. -/
def sum (xs : List Int) : Int :=
  sumTR xs 0

/-- Direct-recurrence sum: matches the algebraic content sum/sumTR encode. -/
def sumDirect (xs : List Int) : Int :=
  match h_32 : xs with
  | [] => 0
  | h :: t => (h + sumDirect t)
termination_by xs.length
decreasing_by
  decreasing_tactic

example : sumTR [] 0 = 0 := by native_decide
example : sumTR [] 7 = 7 := by native_decide
example : sumTR [1, 2, 3] 0 = 6 := by native_decide
example : sumTR [1, 2, 3] 4 = 10 := by native_decide

example : sum [] = 0 := by native_decide
example : sum [1, 2, 3] = 6 := by native_decide
example : sum [42] = 42 := by native_decide

example : sumDirect [] = 0 := by native_decide
example : sumDirect [1, 2, 3] = 6 := by native_decide
example : sumDirect [42] = 42 := by native_decide

-- verify law sum.equalsDirect (4 cases)
-- given xs: List<Int> = [[], [1], [1, 2, 3], [4, 5, 6, 7]]
theorem sumTR_acc (xs : List Int) (a : Int) : sumTR xs a = a + sumTR xs 0 := by
  induction xs generalizing a with
  | nil => simp [sumTR]
  | cons h t ih => simp only [sumTR]; rw [ih (a + h), ih (0 + h)]; omega
theorem sum_law_equalsDirect : ∀ (xs : List Int), sum xs = sumDirect xs := by
  intro xs
  induction xs with
  | nil => simp [sum, sumTR, sumDirect]
  | cons h t ih =>
    simp only [sum, sumTR, sumDirect]
    rw [sumTR_acc t (0 + h)]
    simp only [sum] at ih
    omega
set_option synthInstance.maxSize 4096 in
theorem sum_law_equalsDirect_checked_domain : (sum [] = sumDirect []) ∧ (sum [1] = sumDirect [1]) ∧ (sum [1, 2, 3] = sumDirect [1, 2, 3]) ∧ (sum [4, 5, 6, 7] = sumDirect [4, 5, 6, 7]) := by native_decide
theorem sum_law_equalsDirect_sample_1 : sum [] = sumDirect [] := by native_decide
theorem sum_law_equalsDirect_sample_2 : sum [1] = sumDirect [1] := by native_decide
theorem sum_law_equalsDirect_sample_3 : sum [1, 2, 3] = sumDirect [1, 2, 3] := by native_decide
theorem sum_law_equalsDirect_sample_4 : sum [4, 5, 6, 7] = sumDirect [4, 5, 6, 7] := by native_decide

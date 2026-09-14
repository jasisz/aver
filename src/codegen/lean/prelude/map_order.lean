/-- Only key types whose runtime order is represented exactly receive this
    evidence. In particular, the append-order fallback has no instance. -/
class AverLawfulKeyOrder (α : Type u) [AverKeyOrder α] : Prop where
  flip (a b : α) : a ≠ b → AverKeyOrder.lt a b = !AverKeyOrder.lt b a
  trans (a b c : α) : AverKeyOrder.lt a b = true →
    AverKeyOrder.lt b c = true → AverKeyOrder.lt a c = true

instance : AverLawfulKeyOrder Int where
  flip a b h := by
    change decide (a < b) = !decide (b < a)
    by_cases hab : a < b
    · have hba : ¬ b < a := by omega
      simp [hab, hba]
    · have hba : b < a := by omega
      simp [hab, hba]
  trans a b c h₁ h₂ := by
    change decide (a < c) = true
    change decide (a < b) = true at h₁
    change decide (b < c) = true at h₂
    simp only [decide_eq_true_eq] at *
    omega

instance : AverLawfulKeyOrder String where
  flip a b h := by
    change decide (a < b) = !decide (b < a)
    rcases Std.lt_trichotomy a b with hab | heq | hba
    · have hn : ¬ b < a := fun hba => Std.lt_irrefl (Std.lt_trans hab hba)
      simp [hab, hn]
    · exact False.elim (h heq)
    · have hn : ¬ a < b := fun hab => Std.lt_irrefl (Std.lt_trans hab hba)
      simp [hba, hn]
  trans a b c h₁ h₂ := by
    change decide (a < c) = true
    change decide (a < b) = true at h₁
    change decide (b < c) = true at h₂
    simp only [decide_eq_true_eq] at *
    exact Std.lt_trans h₁ h₂

instance : AverLawfulKeyOrder Bool where
  flip a b h := by cases a <;> cases b <;> simp_all [AverKeyOrder.lt]
  trans a b c h₁ h₂ := by cases a <;> cases b <;> cases c <;> simp_all [AverKeyOrder.lt]

namespace AverMap

private theorem replace_comm [DecidableEq α] (m : List (α × β)) (a b : α) (v w : β)
    (h : a ≠ b) : replace (replace m a v) b w = replace (replace m b w) a v := by
  induction m with
  | nil => rfl
  | cons p tl ih =>
    rcases p with ⟨k, x⟩
    by_cases ha : a = k <;> by_cases hb : b = k <;>
      simp_all [replace_cons]

private theorem replace_insert_ne [DecidableEq α] [AverKeyOrder α]
    (m : List (α × β)) (a b : α) (v w : β) (h : a ≠ b) :
    replace (insert a v m) b w = insert a v (replace m b w) := by
  induction m with
  | nil => simp [insert, replace_cons, replace_nil, Ne.symm h]
  | cons p tl ih =>
    rcases p with ⟨k, x⟩
    by_cases hb : b = k
    · subst k
      cases hh : AverKeyOrder.lt a b <;>
        simp [insert, replace_cons, Ne.symm h, hh, ih]
    · cases hh : AverKeyOrder.lt a k <;>
        simp [insert, replace_cons, hb, Ne.symm h, hh, ih]

private theorem insert_comm [DecidableEq α] [AverKeyOrder α] [AverLawfulKeyOrder α]
    (m : List (α × β)) (a b : α) (v w : β) (h : a ≠ b) :
    insert b w (insert a v m) = insert a v (insert b w m) := by
  have hf := AverLawfulKeyOrder.flip a b h
  induction m with
  | nil => cases hab : AverKeyOrder.lt a b <;>
      simp_all [insert]
  | cons p tl ih =>
    rcases p with ⟨k, x⟩
    have ht₁ := AverLawfulKeyOrder.trans a b k
    have ht₂ := AverLawfulKeyOrder.trans b a k
    cases hab : AverKeyOrder.lt a b <;>
      cases hak : AverKeyOrder.lt a k <;>
      cases hbk : AverKeyOrder.lt b k <;>
      simp_all [insert]

/-- Distinct-key updates commute for the three faithfully ordered key types.
    The statement even covers non-canonical lists quantified by the model. -/
theorem set_set_comm [DecidableEq α] [AverKeyOrder α] [AverLawfulKeyOrder α]
    (m : List (α × β)) (a b : α) (v w : β) (h : a ≠ b) :
    set (set m a v) b w = set (set m b w) a v := by
  have ha := has_set_other m b a w h
  have hb := has_set_other m a b v (Ne.symm h)
  change (if has (set m a v) b then replace (set m a v) b w else insert b w (set m a v)) =
    (if has (set m b w) a then replace (set m b w) a v else insert a v (set m b w))
  rw [ha, hb]
  cases hma : has m a <;> cases hmb : has m b <;>
    simp only [set, hma, hmb, Bool.false_eq_true, ↓reduceIte]
  · exact insert_comm m a b v w h
  · exact replace_insert_ne m a b v w h
  · exact (replace_insert_ne m b a w v (Ne.symm h)).symm
  · exact replace_comm m a b v w h

end AverMap

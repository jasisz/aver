import Knowledge

/- This companion imports the actual Aver export. It composes its audited
   one-step laws; there is no replacement implementation of merge or mergeAll. -/
namespace KnowledgeSchedules
open Knowledge

def Admitted (xs : List Delta) : Prop := ∀ d ∈ xs, admit d = true
def Agreeing (xs : List Delta) : Prop := ∀ a ∈ xs, ∀ b ∈ xs, agrees a b = true

private theorem mergeAll_fold (k : Knowledge) (xs : List Delta) :
    mergeAll k xs = xs.foldl merge k := by
  induction xs generalizing k with
  | nil => simp [mergeAll]
  | cons d ds ih => simpa only [mergeAll, List.foldl_cons] using ih (merge k d)

/-- Every finite permutation, not an enumeration of chosen reorderings. -/
theorem permutation (k : Knowledge) (xs ys : List Delta)
    (admitted : Admitted xs) (agreeing : Agreeing xs) (same : xs.Perm ys) :
    mergeAll k xs = mergeAll k ys := by
  rw [mergeAll_fold, mergeAll_fold]
  apply same.foldl_eq'
  intro a ha b hb state
  exact merge_law_commutative state a b (by simp [admitted a ha, admitted b hb, agreeing a ha b hb])

private theorem absorb_member (xs : List Delta) (a : Delta) (member : a ∈ xs)
    (idem : ∀ k, merge (merge k a) a = merge k a)
    (comm : ∀ b ∈ xs, ∀ k, merge (merge k a) b = merge (merge k b) a)
    (k : Knowledge) : mergeAll (merge k a) xs = mergeAll k xs := by
  induction xs generalizing k with
  | nil => simp at member
  | cons b rest ih =>
    rcases List.mem_cons.mp member with heq | hrest
    · subst b
      simp only [mergeAll, idem]
    · simp only [mergeAll]
      rw [comm b (by simp) k]
      exact ih hrest (fun d hd => comm d (by simp [hd])) (merge k b)

private theorem absorb_subset (prior xs : List Delta) (subset : prior ⊆ xs)
    (admitted : Admitted xs) (agreeing : Agreeing xs) (k : Knowledge) :
    mergeAll (mergeAll k prior) xs = mergeAll k xs := by
  induction prior generalizing k with
  | nil => simp [mergeAll]
  | cons a rest ih =>
    have ha := subset List.mem_cons_self
    have hr : rest ⊆ xs := fun _ hd => subset (List.mem_cons_of_mem a hd)
    simp only [mergeAll]
    rw [ih hr (merge k a)]
    apply absorb_member xs a ha
    · intro state
      exact merge_law_idempotent state a (admitted a ha)
    · intro b hb state
      exact merge_law_commutative state a b (by simp [admitted a ha, admitted b hb, agreeing a ha b hb])

/-- Multiplicity is irrelevant as well: membership equality suffices. -/
theorem same_contributions (k : Knowledge) (xs ys : List Delta)
    (admitted : Admitted xs) (agreeing : Agreeing xs)
    (same : ∀ d, d ∈ xs ↔ d ∈ ys) : mergeAll k xs = mergeAll k ys := by
  have xy : xs ⊆ ys := fun d hd => (same d).mp hd
  have yx : ys ⊆ xs := fun d hd => (same d).mpr hd
  have ay : Admitted ys := fun d hd => admitted d (yx hd)
  have gy : Agreeing ys := fun a ha b hb => agreeing a (yx ha) b (yx hb)
  have combined : xs ++ ys ⊆ xs := by
    intro d hd
    rcases List.mem_append.mp hd with hx | hy
    · exact hx
    · exact yx hy
  have crossed := permutation k (xs ++ ys) (ys ++ xs)
    (fun d hd => admitted d (combined hd))
    (fun a ha b hb => agreeing a (combined ha) b (combined hb))
    (List.perm_append_comm)
  rw [mergeAll_law_appends, mergeAll_law_appends] at crossed
  rw [absorb_subset xs ys xy ay gy, absorb_subset ys xs yx admitted agreeing] at crossed
  exact crossed.symm

/-- A schedule is any finite sequence of batches, including empty batches. -/
def runBatches (k : Knowledge) (batches : List (List Delta)) : Knowledge :=
  batches.foldl mergeAll k

theorem flatten_batches (k : Knowledge) (batches : List (List Delta)) :
    runBatches k batches = mergeAll k batches.flatten := by
  induction batches generalizing k with
  | nil => simp [mergeAll, runBatches]
  | cons batch rest ih =>
    simp only [runBatches, List.foldl_cons, List.flatten_cons]
    rw [mergeAll_law_appends]
    exact ih (mergeAll k batch)

/-- Arbitrary ordering, batch boundaries and duplicate counts yield the same
    final Knowledge, for the same admitted, pairwise agreeing contributions. -/
theorem any_schedule (k : Knowledge) (first second : List (List Delta))
    (admitted : Admitted first.flatten) (agreeing : Agreeing first.flatten)
    (same : ∀ d, d ∈ first.flatten ↔ d ∈ second.flatten) :
    runBatches k first = runBatches k second := by
  rw [flatten_batches, flatten_batches]
  exact same_contributions k first.flatten second.flatten admitted agreeing same

/-- Growth is consistent at each actual intermediate state. This is explicit:
    peer admission alone cannot establish consistency with arbitrary initial k. -/
def ConsistentRun (k : Knowledge) : List Delta → Prop
  | [] => True
  | d :: rest => admit d = true ∧ consistent k d = true ∧ ConsistentRun (merge k d) rest

theorem body_stable_run (k : Knowledge) (xs : List Delta) (hash : String) (value : Bytes.Bytes)
    (growth : ConsistentRun k xs) (known : body k hash = some value) :
    body (mergeAll k xs) hash = some value := by
  induction xs generalizing k with
  | nil => simpa only [mergeAll] using known
  | cons d rest ih =>
    obtain ⟨admitted, consistent, remaining⟩ := growth
    simp only [mergeAll]
    apply ih (merge k d) remaining
    exact body_law_stable k hash value d (by simp [known, admitted, consistent])

theorem verdict_stable_run (k : Knowledge) (xs : List Delta) (block context : String) (value : Verdict)
    (growth : ConsistentRun k xs) (known : verdict k block context = some value) :
    verdict (mergeAll k xs) block context = some value := by
  induction xs generalizing k with
  | nil => simpa only [mergeAll] using known
  | cons d rest ih =>
    obtain ⟨admitted, consistent, remaining⟩ := growth
    simp only [mergeAll]
    apply ih (merge k d) remaining
    exact verdict_law_stable k block context value d (by simp [known, admitted, consistent])

end KnowledgeSchedules

#print axioms KnowledgeSchedules.permutation
#print axioms KnowledgeSchedules.same_contributions
#print axioms KnowledgeSchedules.any_schedule
#print axioms KnowledgeSchedules.body_stable_run
#print axioms KnowledgeSchedules.verdict_stable_run

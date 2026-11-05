-- Set-shaped checks on sorted numeric keys.
import DeclaredLayout

set_option linter.unusedSimpArgs false

namespace AverCert.SortedKeys
open AverCert.AcceptedArtifact

/-! ### Merge sort on numbers

The set checks index numbers in balanced trees (`WasmSlice.orderedSet`).
The kernel pays each insertion with a walk through the tree's rebalancing
code, a few thousand steps per number, while a merge of two sorted lists pays
one comparison of two numerals per element. The checks below sort the keys
once and then walk the sorted lists; the lemmas show that what they decide
implies the tree-based check. -/

def merge : Nat → List Nat → List Nat → List Nat
  | 0, xs, ys => xs ++ ys
  | _ + 1, [], ys => ys
  | _ + 1, x :: xs, [] => x :: xs
  | f + 1, x :: xs, y :: ys =>
      if x ≤ y then x :: merge f xs (y :: ys) else y :: merge f (x :: xs) ys

theorem merge_perm : ∀ (f : Nat) (xs ys : List Nat), (merge f xs ys).Perm (xs ++ ys)
  | 0, xs, ys => List.Perm.refl _
  | _ + 1, [], ys => List.Perm.refl _
  | _ + 1, x :: xs, [] => by simp [merge]
  | f + 1, x :: xs, y :: ys => by
      unfold merge
      split
      · exact (merge_perm f xs (y :: ys)).cons x
      · have h := (merge_perm f (x :: xs) ys).cons y
        exact h.trans (by simpa using (List.perm_middle (a := y) (l₁ := x :: xs) (l₂ := ys)).symm)

def mergePairs (f : Nat) : List (List Nat) → List (List Nat)
  | a :: b :: rest => merge f a b :: mergePairs f rest
  | rest => rest

theorem mergePairs_perm (f : Nat) : ∀ runs : List (List Nat),
    (mergePairs f runs).flatten.Perm runs.flatten
  | a :: b :: rest => by
      simp only [mergePairs, List.flatten_cons, ← List.append_assoc]
      exact (merge_perm f a b).append (mergePairs_perm f rest)
  | [] => List.Perm.refl _
  | [_] => List.Perm.refl _

def sortRuns (f : Nat) : Nat → List (List Nat) → List Nat
  | 0, runs => runs.flatten
  | _ + 1, [] => []
  | _ + 1, [r] => r
  | k + 1, a :: b :: rest => sortRuns f k (mergePairs f (a :: b :: rest))

theorem sortRuns_perm (f : Nat) : ∀ (k : Nat) (runs : List (List Nat)),
    (sortRuns f k runs).Perm runs.flatten
  | 0, _ => List.Perm.refl _
  | _ + 1, [] => List.Perm.refl _
  | _ + 1, [r] => by simp [sortRuns]
  | k + 1, a :: b :: rest => by
      simp only [sortRuns]
      exact (sortRuns_perm f k _).trans (mergePairs_perm f _)

/-- Merge sort, bottom up: every merge is given enough steps for the whole
    list, and 64 rounds halve any list to one run. -/
def msort (xs : List Nat) : List Nat := sortRuns (xs.length + 1) 64 (xs.map (fun x => [x]))

theorem msort_perm (xs : List Nat) : (msort xs).Perm xs := by
  unfold msort
  refine (sortRuns_perm _ _ _).trans ?_
  induction xs with
  | nil => exact List.Perm.refl _
  | cons x xs ih => simpa using ih.cons x

/-- Strictly increasing. -/
def strictly : List Nat → Bool
  | x :: y :: rest => decide (x < y) && strictly (y :: rest)
  | _ => true

theorem strictly_tail {x : Nat} {xs : List Nat} (h : strictly (x :: xs) = true) : strictly xs = true := by
  cases xs with
  | nil => rfl
  | cons y ys =>
      simp only [strictly, Bool.and_eq_true, decide_eq_true_eq] at h
      exact h.2

theorem strictly_lt : ∀ {x : Nat} {xs : List Nat}, strictly (x :: xs) = true → ∀ y ∈ xs, x < y
  | _, [], _, _, hy => by cases hy
  | x, z :: zs, h, y, hy => by
      simp only [strictly, Bool.and_eq_true, decide_eq_true_eq] at h
      cases hy with
      | head => exact h.1
      | tail _ hy' => exact Nat.lt_trans h.1 (strictly_lt h.2 y hy')

theorem strictly_nodup : ∀ {xs : List Nat}, strictly xs = true → xs.Nodup
  | [], _ => List.nodup_nil
  | x :: _, h => List.nodup_cons.mpr
      ⟨fun hx => Nat.lt_irrefl x (strictly_lt h x hx), strictly_nodup (strictly_tail h)⟩

/-- `xs` itself when it is already strictly increasing, and `msort xs`
    otherwise. A producer that declares a list in sorted order saves the
    kernel the sort, and a list in any other order is sorted: the result is
    a permutation of `xs` either way (`sortedOr_perm`), so the order a
    package declares can only change how long a check takes. -/
def sortedOr (xs : List Nat) : List Nat := if strictly xs then xs else msort xs

theorem sortedOr_perm (xs : List Nat) : (sortedOr xs).Perm xs := by
  unfold sortedOr
  split
  · exact List.Perm.refl _
  · exact msort_perm xs

theorem nodup_of_msort {xs : List Nat} (h : strictly (msort xs) = true) : xs.Nodup :=
  (msort_perm xs).nodup_iff.mp (strictly_nodup h)

/-- Every element of `s` is in `t`, walking both in order. -/
def subsetW : Nat → List Nat → List Nat → Bool
  | _, [], _ => true
  | 0, _ :: _, _ => false
  | _ + 1, _ :: _, [] => false
  | f + 1, x :: xs, y :: ys =>
      if x = y then subsetW f xs ys else if y < x then subsetW f (x :: xs) ys else false

theorem subsetW_mem : ∀ {f : Nat} {s t : List Nat}, subsetW f s t = true → ∀ x ∈ s, x ∈ t
  | _, [], _, _, _, hx => by cases hx
  | 0, _ :: _, _, h, _, _ => by simp [subsetW] at h
  | _ + 1, _ :: _, [], h, _, _ => by simp [subsetW] at h
  | f + 1, a :: as, b :: bs, h, x, hx => by
      unfold subsetW at h
      split at h
      · rename_i hab
        subst hab
        cases hx with
        | head => exact List.mem_cons_self
        | tail _ hx' => exact List.mem_cons_of_mem _ (subsetW_mem h x hx')
      · split at h
        · exact List.mem_cons_of_mem _ (subsetW_mem h x hx)
        · cases h

/-! ### Distinct small numbers, on a bitmap

`decide (List.Nodup xs)` compares every pair, a quadratic walk the kernel
pays through the `Decidable` instances. Numbers below a bound are instead
checked in one pass, keeping the ones seen as the bits of one numeral: a
bit test and an `or` per element, which the kernel does on the numeral
directly. The bound keeps that numeral small: a number at or above it
fails the check instead of being set. -/

/-- The numbers of `xs` as the bits of one numeral. -/
def bitsOf : List Nat → Nat
  | [] => 0
  | x :: xs => bitsOf xs ||| (1 <<< x)

theorem testBit_or_shift (bits x y : Nat) :
    (bits ||| (1 <<< x)).testBit y = true ↔ bits.testBit y = true ∨ y = x := by
  rw [Nat.testBit_or, Nat.shiftLeft_eq, Nat.one_mul, Nat.testBit_two_pow, Bool.or_eq_true,
    decide_eq_true_eq]
  exact or_congr Iff.rfl ⟨fun h => h.symm, fun h => h.symm⟩

theorem testBit_bitsOf : ∀ (xs : List Nat) (y : Nat), (bitsOf xs).testBit y = true ↔ y ∈ xs
  | [], y => by simp [bitsOf]
  | x :: xs, y => by
      rw [bitsOf, testBit_or_shift, testBit_bitsOf xs y, List.mem_cons]
      exact Or.comm

/-- Every number of `xs` is below `bound`, none repeats, and none is a bit
    of `bits`. -/
def nodupBits (bound : Nat) : List Nat → Nat → Bool
  | [], _ => true
  | x :: xs, bits =>
      decide (x < bound) && !bits.testBit x && nodupBits bound xs (bits ||| (1 <<< x))

theorem nodupBits_sound : ∀ {bound : Nat} {xs : List Nat} {bits : Nat},
    nodupBits bound xs bits = true → xs.Nodup ∧ ∀ x ∈ xs, x < bound ∧ bits.testBit x = false
  | _, [], _, _ => ⟨List.nodup_nil, fun _ hx => by cases hx⟩
  | bound, x :: xs, bits, h => by
      simp only [nodupBits, Bool.and_eq_true, decide_eq_true_eq, Bool.not_eq_true'] at h
      obtain ⟨⟨hlt, hfresh⟩, hrest⟩ := h
      obtain ⟨hnd, hall⟩ := nodupBits_sound hrest
      -- An element of the tail is not a bit of `bits ||| 1 <<< x`: not `x`, and
      -- not a bit of `bits`.
      have htail : ∀ y ∈ xs, y < bound ∧ bits.testBit y = false ∧ y ≠ x := by
        intro y hy
        obtain ⟨hy1, hy2⟩ := hall y hy
        have hn : ¬ ((bits ||| (1 <<< x)).testBit y = true) := by simp [hy2]
        rw [testBit_or_shift] at hn
        refine ⟨hy1, ?_, fun e => hn (Or.inr e)⟩
        cases hb : bits.testBit y
        · rfl
        · exact absurd (Or.inl hb) hn
      refine ⟨List.nodup_cons.mpr ⟨fun hx => (htail x hx).2.2 rfl, hnd⟩, ?_⟩
      intro y hy
      cases hy with
      | head => exact ⟨hlt, hfresh⟩
      | tail _ hy' => exact ⟨(htail y hy').1, (htail y hy').2.1⟩

/-- The bound `indicesDistinctBits` holds function indices to. A module has
    far fewer functions; an index above it fails the check. -/
def indexBound : Nat := 16777216

/-- `indicesDistinct` in one pass: the role indices (eleven of them, some
    far above any function index when a role is absent) are compared
    pairwise, and the planned indices go through the bitmap, starting from
    the bits of the roles below the bound. -/
def indicesDistinctBits (M : AverCert.Grammar.MCtx) (fns : List AverCert.Schema.FnEntry) : Bool :=
  decide (roleIndices M).Nodup &&
  nodupBits indexBound (fns.map (·.funcIdx)) (bitsOf ((roleIndices M).filter (· < indexBound)))

theorem indicesDistinct_of_bits {M : AverCert.Grammar.MCtx} {fns : List AverCert.Schema.FnEntry}
    (h : indicesDistinctBits M fns = true) : indicesDistinct M fns = true := by
  unfold indicesDistinctBits at h
  simp only [Bool.and_eq_true, decide_eq_true_eq] at h
  obtain ⟨hroles, hbits⟩ := h
  obtain ⟨hnd, hall⟩ := nodupBits_sound hbits
  unfold indicesDistinct
  refine decide_eq_true (List.nodup_append.mpr ⟨hroles, hnd, ?_⟩)
  intro a ha b hb hab
  subst hab
  obtain ⟨hlt, hfresh⟩ := hall a hb
  have hin : a ∈ (roleIndices M).filter (· < indexBound) :=
    List.mem_filter.mpr ⟨ha, decide_eq_true hlt⟩
  rw [← testBit_bitsOf] at hin
  rw [hin] at hfresh
  cases hfresh

/-! ### What the balanced-tree checks decide -/

theorem foldl_contains {α : Type} [Ord α] [Std.TransOrd α] [Std.LawfulEqOrd α] :
    ∀ (xs : List α) (t : Std.TreeSet α compare) (a : α),
      (xs.foldl (fun set value => set.insert value) t).contains a = true ↔ t.contains a = true ∨ a ∈ xs
  | [], t, a => by simp
  | x :: xs, t, a => by
      rw [List.foldl_cons, foldl_contains xs (t.insert x) a, Std.TreeSet.contains_insert]
      simp only [Bool.or_eq_true, beq_iff_eq, Std.LawfulEqCmp.compare_eq_iff_eq, List.mem_cons]
      constructor
      · rintro ((rfl | h) | h)
        · exact Or.inr (Or.inl rfl)
        · exact Or.inl h
        · exact Or.inr (Or.inr h)
      · rintro (h | rfl | h)
        · exact Or.inl (Or.inr h)
        · exact Or.inl (Or.inl rfl)
        · exact Or.inr h

theorem orderedSet_contains {α : Type} [Ord α] [Std.TransOrd α] [Std.LawfulEqOrd α] (xs : List α)
    (a : α) : (AverCert.WasmSlice.orderedSet xs).contains a = true ↔ a ∈ xs := by
  unfold AverCert.WasmSlice.orderedSet
  rw [foldl_contains]
  simp

theorem foldl_size {α : Type} [Ord α] [Std.TransOrd α] [Std.LawfulEqOrd α] :
    ∀ (xs : List α) (t : Std.TreeSet α compare), xs.Nodup → (∀ y ∈ xs, t.contains y = false) →
      (xs.foldl (fun set value => set.insert value) t).size = t.size + xs.length
  | [], t, _, _ => by simp
  | x :: xs, t, hnd, hout => by
      rw [List.foldl_cons]
      have hx := hout x List.mem_cons_self
      obtain ⟨hnx, hnd'⟩ := List.nodup_cons.mp hnd
      rw [foldl_size xs (t.insert x) hnd' (fun y hy => ?_), Std.TreeSet.size_insert]
      · simp [hx]; omega
      · rw [Std.TreeSet.contains_insert]
        simp only [Bool.or_eq_false_iff, beq_eq_false_iff_ne, ne_eq,
          Std.LawfulEqCmp.compare_eq_iff_eq]
        exact ⟨fun h => hnx (h ▸ hy), hout y (List.mem_cons_of_mem _ hy)⟩

theorem natListNodup_of_nodup {xs : List Nat} (h : xs.Nodup) :
    AverCert.WasmSlice.natListNodup xs = true := by
  unfold AverCert.WasmSlice.natListNodup AverCert.WasmSlice.indexedNodup
    AverCert.WasmSlice.orderedSet
  dsimp only
  rw [foldl_size xs _ h (fun y _ => by simp)]
  simp

theorem mem_sortedOr {xs : List Nat} {x : Nat} : x ∈ sortedOr xs ↔ x ∈ xs := (sortedOr_perm xs).mem_iff

theorem nodup_of_sortedOr {xs : List Nat} (h : strictly (sortedOr xs) = true) : xs.Nodup :=
  (sortedOr_perm xs).nodup_iff.mp (strictly_nodup h)

/-! ### Export names read through their keys -/

theorem mapM_key_names : ∀ (es : List AverCert.WasmSlice.ExportEntry),
    (es.mapM exportEntryKey).map (fun ks => ks.map (·.name)) =
      AverCert.WasmSlice.seqKeys (es.map (·.name))
  | [] => rfl
  | e :: es => by
      have ih := mapM_key_names es
      unfold AverCert.WasmSlice.seqKeys at ih ⊢
      simp only [List.mapM_cons, List.map_cons]
      have hk : exportEntryKey e = (AverCert.WasmSlice.seqKey e.name).map
          (fun name => ({ name := name, kind := e.kind, idx := e.idx } : ExportKey)) := rfl
      rw [hk]
      cases h1 : AverCert.WasmSlice.seqKey e.name <;>
        cases h2 : es.mapM exportEntryKey <;> simp_all <;> (rw [← ih]; rfl)

/-! ### Closure isolation on sorted lists and a membership bitmap -/

theorem natSetEq_of_sorted {xs ys : List Nat}
    (h1 : subsetW ((sortedOr xs).length + (sortedOr ys).length + 1) (sortedOr xs) (sortedOr ys) = true)
    (h2 : subsetW ((sortedOr ys).length + (sortedOr xs).length + 1) (sortedOr ys) (sortedOr xs) = true) :
    AverCert.WasmSlice.natSetEq xs ys = true := by
  unfold AverCert.WasmSlice.natSetEq AverCert.WasmSlice.indexedSetEq AverCert.WasmSlice.indexedSubset
  simp only [Bool.and_eq_true, List.all_eq_true, orderedSet_contains]
  exact ⟨fun x hx => mem_sortedOr.mp (subsetW_mem h1 x (mem_sortedOr.mpr hx)),
    fun y hy => mem_sortedOr.mp (subsetW_mem h2 y (mem_sortedOr.mpr hy))⟩

/-- `natSetEq` on sorted lists. -/
def setEqSorted (xs ys : List Nat) : Bool :=
  let sx := sortedOr xs
  let sy := sortedOr ys
  subsetW (sx.length + sy.length + 1) sx sy && subsetW (sy.length + sx.length + 1) sy sx

theorem natSetEq_of_setEqSorted {xs ys : List Nat} (h : setEqSorted xs ys = true) :
    AverCert.WasmSlice.natSetEq xs ys = true := by
  unfold setEqSorted at h
  simp only [Bool.and_eq_true] at h
  exact natSetEq_of_sorted h.1 h.2

theorem natMem_iff : ∀ (x : Nat) (xs : List Nat), AverCert.WasmSlice.natMem x xs = true ↔ x ∈ xs
  | _, [] => by simp [AverCert.WasmSlice.natMem]
  | x, y :: ys => by
      simp only [AverCert.WasmSlice.natMem, Bool.or_eq_true, beq_iff_eq, natMem_iff x ys,
        List.mem_cons]

/-- `DeclaredLayout.closureFoldWith`, with the seen functions also kept as a
    bitmap, so that asking whether a function was seen is one numeral test. -/
def closureFoldB (look : Nat → Option AverCert.WasmSlice.ByteSeq) :
    Nat → List Nat → List Nat → Nat → Option (List Nat)
  | 0, [], seen, _ => some seen
  | 0, _ :: _, _, _ => none
  | _ + 1, [], seen, _ => some seen
  | fuel + 1, func :: work, seen, bits =>
      if bits.testBit func then closureFoldB look fuel work seen bits
      else
        match (look func).bind AverCert.WasmSlice.scanClosureCodeEntry with
        | some callees => closureFoldB look fuel (callees ++ work) (func :: seen) (bits ||| (1 <<< func))
        | none => none

theorem closureFoldB_eq (look : Nat → Option AverCert.WasmSlice.ByteSeq) :
    ∀ (fuel : Nat) (work seen : List Nat) (bits : Nat), (∀ x, bits.testBit x = true ↔ x ∈ seen) →
      closureFoldB look fuel work seen bits =
        AverCert.DeclaredLayout.closureFoldWith look fuel work seen
  | 0, [], _, _, _ => rfl
  | 0, _ :: _, _, _, _ => rfl
  | _ + 1, [], _, _, _ => rfl
  | fuel + 1, func :: work, seen, bits, hb => by
      have hmem : bits.testBit func = AverCert.WasmSlice.natMem func seen := by
        apply Bool.eq_iff_iff.mpr
        rw [hb, natMem_iff]
      simp only [closureFoldB, AverCert.DeclaredLayout.closureFoldWith, hmem]
      split
      · exact closureFoldB_eq look fuel work seen bits hb
      · cases (look func).bind AverCert.WasmSlice.scanClosureCodeEntry with
        | none => rfl
        | some callees =>
            simp only
            apply closureFoldB_eq look fuel _ _ _
            intro x
            rw [Nat.testBit_or, Nat.shiftLeft_eq, Nat.one_mul, Nat.testBit_two_pow, Bool.or_eq_true,
              hb, List.mem_cons, decide_eq_true_eq]
            constructor
            · rintro (h | rfl)
              · exact Or.inr h
              · exact Or.inl rfl
            · rintro (rfl | h)
              · exact Or.inr rfl
              · exact Or.inl h

/-- `DeclaredLayout.closureIsolationL`, on sorted lists and the bitmap fold. -/
def closureIsolationS (artifact : ArtifactData) (L : AverCert.DeclaredLayout.Layout) : Bool :=
  let claim := artifact.closureClaim
  let certified := artifact.manifest.obligations.map (fun obligation => obligation.self)
  strictly (sortedOr claim.roots) &&
  strictly (sortedOr claim.helpers) &&
  strictly (sortedOr claim.admitted) &&
  setEqSorted claim.roots certified &&
  claim.roots.all (fun root => !AverCert.WasmSlice.natMem root claim.helpers) &&
  setEqSorted claim.admitted (claim.roots ++ claim.helpers) &&
  AverCert.WasmSlice.noSharedMemory artifact.modBytes artifact.modLen &&
  match closureFoldB (L.entryAt artifact.modBytes) artifact.closureFuel claim.roots [] 0 with
  | some actual => setEqSorted actual claim.admitted
  | none => false

theorem closureIsolationL_of_S {artifact : ArtifactData} {L : AverCert.DeclaredLayout.Layout}
    (h : closureIsolationS artifact L = true) :
    AverCert.DeclaredLayout.closureIsolationL artifact L = true := by
  unfold closureIsolationS at h
  unfold AverCert.DeclaredLayout.closureIsolationL
  simp only [Bool.and_eq_true] at h ⊢
  obtain ⟨⟨⟨⟨⟨⟨⟨h1, h2⟩, h3⟩, h4⟩, h5⟩, h6⟩, h7⟩, h8⟩ := h
  refine ⟨⟨⟨⟨⟨⟨⟨natListNodup_of_nodup (nodup_of_sortedOr h1), natListNodup_of_nodup (nodup_of_sortedOr h2)⟩,
    natListNodup_of_nodup (nodup_of_sortedOr h3)⟩, natSetEq_of_setEqSorted h4⟩, h5⟩,
    natSetEq_of_setEqSorted h6⟩, h7⟩, ?_⟩
  rw [← closureFoldB_eq _ _ _ _ 0 (fun x => by simp)]
  split at h8
  · rename_i actual hact
    rw [hact]
    exact natSetEq_of_setEqSorted h8
  · cases h8

end AverCert.SortedKeys

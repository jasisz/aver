-- Set-shaped checks on sorted numeric keys.
import DeclaredLayout

set_option linter.unusedSimpArgs false

namespace AverCert.SortedKeys
open AverCert.AcceptedArtifact

/-! ### Merge sort on numbers

The accounting checks index names in balanced trees (`WasmSlice.orderedSet`).
The kernel pays each insertion with a walk through the tree's rebalancing
code, a few thousand steps per name, while a merge of two sorted lists pays
one comparison of two numerals per element. The checks below sort the keys
once and then walk the sorted lists; the lemmas show that what they decide
implies the tree-based accounting. -/

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

/-- No element of `s` is in `t`, walking both (strictly increasing) in order. -/
def disjointW : Nat → List Nat → List Nat → Bool
  | _, [], _ => true
  | _, _ :: _, [] => true
  | 0, _ :: _, _ :: _ => false
  | f + 1, x :: xs, y :: ys =>
      if x = y then false else if x < y then disjointW f xs (y :: ys) else disjointW f (x :: xs) ys

theorem disjointW_sound : ∀ {f : Nat} {s t : List Nat}, strictly s = true → strictly t = true →
    disjointW f s t = true → ∀ x ∈ s, x ∈ t → False
  | _, [], _, _, _, _, _, hx, _ => by cases hx
  | _, _ :: _, [], _, _, _, _, _, ht => by cases ht
  | 0, _ :: _, _ :: _, _, _, h, _, _, _ => by simp [disjointW] at h
  | f + 1, a :: as, b :: bs, hs, ht, h, x, hx, hmem => by
      unfold disjointW at h
      split at h
      · cases h
      · rename_i hne
        split at h
        · rename_i hlt
          cases hx with
          | head =>
              cases hmem with
              | head => exact hne rfl
              | tail _ hmem' => exact Nat.lt_asymm hlt (strictly_lt ht _ hmem')
          | tail _ hx' => exact disjointW_sound (strictly_tail hs) ht h x hx' hmem
        · rename_i hge
          cases hmem with
          | head =>
              cases hx with
              | head => exact hne rfl
              | tail _ hx' => exact Nat.lt_irrefl _ (Nat.lt_of_lt_of_le (strictly_lt hs _ hx')
                  (Nat.le_of_not_lt (by omega)))
          | tail _ hmem' => exact disjointW_sound hs (strictly_tail ht) h x hx hmem'

/-- Every element of `as` is in `cs`, or its quotient by `2 ^ 64` is in `ds`:
    a walk over three lists in order. -/
def cover : Nat → List Nat → List Nat → List Nat → Bool
  | _, [], _, _ => true
  | 0, _ :: _, _, _ => false
  | f + 1, a :: as, cs, ds =>
      match cs, ds with
      | c :: cs', ds =>
          if a = c then cover f as cs' ds
          else match ds with
            | d :: ds' => if a / 18446744073709551616 = d then cover f as (c :: cs') ds' else false
            | [] => false
      | [], d :: ds' => if a / 18446744073709551616 = d then cover f as [] ds' else false
      | [], [] => false

theorem cover_sound : ∀ {f : Nat} {as cs ds : List Nat}, cover f as cs ds = true →
    ∀ a ∈ as, a ∈ cs ∨ a / 18446744073709551616 ∈ ds
  | _, [], _, _, _, _, ha => by cases ha
  | 0, _ :: _, _, _, h, _, _ => by simp [cover] at h
  | f + 1, x :: xs, cs, ds, h, a, ha => by
      unfold cover at h
      split at h
      · rename_i c cs' ds0
        split at h
        · rename_i hxc
          cases ha with
          | head => exact Or.inl (hxc ▸ List.mem_cons_self)
          | tail _ ha' =>
              rcases cover_sound h a ha' with h1 | h1
              · exact Or.inl (List.mem_cons_of_mem _ h1)
              · exact Or.inr h1
        · split at h
          · rename_i d ds'
            split at h
            · rename_i hxd
              cases ha with
              | head => exact Or.inr (hxd ▸ List.mem_cons_self)
              | tail _ ha' =>
                  rcases cover_sound h a ha' with h1 | h1
                  · exact Or.inl h1
                  · exact Or.inr (List.mem_cons_of_mem _ h1)
            · cases h
          · cases h
      · rename_i d ds'
        split at h
        · rename_i hxd
          cases ha with
          | head => exact Or.inr (hxd ▸ List.mem_cons_self)
          | tail _ ha' =>
              rcases cover_sound h a ha' with h1 | h1
              · exact Or.inl h1
              · exact Or.inr (List.mem_cons_of_mem _ h1)
        · cases h
      · cases h

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

/-- The derived order on export keys is lexicographic on its three numbers. -/
theorem compare_exportKey (a b : ExportKey) :
    compare a b = compareLex (compareOn ExportKey.name)
      (compareLex (compareOn ExportKey.kind) (compareOn ExportKey.idx)) a b := by
  cases a; cases b
  simp only [compare, instOrdExportKey.ord, compareLex, compareOn]
  rename_i n1 k1 i1 n2 k2 i2
  generalize compareOfLessAndEq i1 i2 = o
  cases o <;> rfl

instance : Std.TransOrd ExportKey := by
  have h : (compare : ExportKey → ExportKey → Ordering) = compareLex (compareOn ExportKey.name)
      (compareLex (compareOn ExportKey.kind) (compareOn ExportKey.idx)) := by
    funext a b; exact compare_exportKey a b
  unfold Std.TransOrd
  rw [h]
  infer_instance

instance : Std.LawfulEqOrd ExportKey where
  eq_of_compare {a b} h := by
    rw [compare_exportKey] at h
    simp only [compareLex_eq_eq, compareOn, Std.LawfulEqCmp.compare_eq_iff_eq] at h
    cases a; cases b
    simp only at h
    obtain ⟨h1, h2, h3⟩ := h
    subst h1; subst h2; subst h3; rfl

/-! ### The export accounting on sorted keys -/

/-- An export key as one number: its name key above its kind and index. -/
def entryNum (k : ExportKey) : Nat := k.name * 18446744073709551616 + k.kind * 4294967296 + k.idx

def keyBounded (k : ExportKey) : Bool := decide (k.kind < 4294967296) && decide (k.idx < 4294967296)

theorem entryNum_div {k : ExportKey} (h : keyBounded k = true) :
    entryNum k / 18446744073709551616 = k.name := by
  simp only [keyBounded, Bool.and_eq_true, decide_eq_true_eq] at h
  unfold entryNum; omega

theorem entryNum_inj {a b : ExportKey} (ha : keyBounded a = true) (hb : keyBounded b = true)
    (h : entryNum a = entryNum b) : a = b := by
  simp only [keyBounded, Bool.and_eq_true, decide_eq_true_eq] at ha hb
  unfold entryNum at h
  cases a; cases b
  simp only [ExportKey.mk.injEq]
  simp only at ha hb h
  refine ⟨?_, ?_, ?_⟩ <;> omega

/-- The accounting of the actual export keys `A` against the certified keys `C`
    and the declared name keys `D`, decided on sorted lists. -/
def accountedSorted (A C : List ExportKey) (D : List Nat) : Bool :=
  let an := msort (A.map (·.name))
  let cn := msort (C.map (·.name))
  let dn := msort D
  let ae := msort (A.map entryNum)
  let ce := msort (C.map entryNum)
  A.all keyBounded && C.all keyBounded &&
  strictly an && strictly cn && strictly dn &&
  disjointW (cn.length + dn.length + 1) cn dn &&
  cover (ae.length + 1) ae ce dn &&
  subsetW (ce.length + ae.length + 1) ce ae &&
  subsetW (dn.length + an.length + 1) dn an

/-- `exportsAccountedOf`, with the module's export entries given. -/
def exportsAccountedFast (actual certified : List AverCert.WasmSlice.ExportEntry)
    (declared : List AverCert.WasmSlice.ByteSeq) : Bool :=
  match actual.mapM exportEntryKey, certified.mapM exportEntryKey,
      AverCert.WasmSlice.seqKeys declared with
  | some A, some C, some D => accountedSorted A C D
  | _, _, _ => false

theorem mem_msort {xs : List Nat} {x : Nat} : x ∈ msort xs ↔ x ∈ xs := (msort_perm xs).mem_iff

theorem nodup_of_msort {xs : List Nat} (h : strictly (msort xs) = true) : xs.Nodup :=
  (msort_perm xs).nodup_iff.mp (strictly_nodup h)

theorem exportsAccountedOf_of_fast {n len : Nat}
    {E : Option (List AverCert.WasmSlice.ExportEntry)}
    (hE : AverCert.WasmSlice.enumExports n len = E)
    {certified : List AverCert.WasmSlice.ExportEntry} {declared : List AverCert.WasmSlice.ByteSeq}
    (h : (match E with
      | some actual => exportsAccountedFast actual certified declared
      | none => false) = true) :
    exportsAccountedOf n len certified declared = true := by
  unfold exportsAccountedOf
  rw [hE]
  cases E with
  | none => cases h
  | some actual =>
      simp only at h ⊢
      unfold exportsAccountedFast at h
      split at h
      · rename_i A C D hA hC hD
        simp only [hA, hC, hD]
        unfold accountedSorted at h
        simp only [Bool.and_eq_true, List.all_eq_true] at h
        obtain ⟨⟨⟨⟨⟨⟨⟨⟨hAb, hCb⟩, han⟩, hcn⟩, hdn⟩, hdis⟩, hcov⟩, hsub1⟩, hsub2⟩ := h
        simp only [Bool.and_eq_true, List.all_eq_true, Bool.or_eq_true, Bool.not_eq_true',
          orderedSet_contains]
        refine ⟨⟨⟨⟨⟨⟨natListNodup_of_nodup (nodup_of_msort han),
          natListNodup_of_nodup (nodup_of_msort hcn)⟩, natListNodup_of_nodup (nodup_of_msort hdn)⟩,
          ?_⟩, ?_⟩, ?_⟩, ?_⟩
        · intro name hname
          apply Bool.eq_false_iff.mpr
          intro hmem
          rw [orderedSet_contains] at hmem
          exact disjointW_sound hcn hdn hdis name (mem_msort.mpr hname) (mem_msort.mpr hmem)
        · intro e he
          rcases cover_sound hcov (entryNum e) (mem_msort.mpr (List.mem_map_of_mem he)) with h1 | h1
          · left
            obtain ⟨c, hc, hce⟩ := List.mem_map.mp (mem_msort.mp h1)
            rw [entryNum_inj (hCb c hc) (hAb e he) hce] at hc
            exact hc
          · right
            rw [entryNum_div (hAb e he)] at h1
            exact mem_msort.mp h1
        · intro c hc
          have := subsetW_mem hsub1 (entryNum c) (mem_msort.mpr (List.mem_map_of_mem hc))
          obtain ⟨a, ha, hae⟩ := List.mem_map.mp (mem_msort.mp this)
          rw [← entryNum_inj (hAb a ha) (hCb c hc) hae]
          exact ha
        · intro d hd
          exact mem_msort.mp (subsetW_mem hsub2 d (mem_msort.mpr hd))
      · cases h

/-! ### Distinct export names, from the accounting -/

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

/-- The accounting decides that the module's export names are distinct, so a
    check that needs them distinct reads it from there instead of deciding it
    again. -/
theorem exportNamesDistinct_of_accounted {n len : Nat}
    {certified : List AverCert.WasmSlice.ExportEntry} {declared : List AverCert.WasmSlice.ByteSeq}
    (h : exportsAccountedOf n len certified declared = true) :
    AverCert.DeclaredLayout.exportNamesDistinct n len = true := by
  unfold exportsAccountedOf at h
  unfold AverCert.DeclaredLayout.exportNamesDistinct byteSeqListNodup
  unfold AverCert.WasmSlice.enumExports at h
  cases hE : CertDecode.decodeRawExports n len with
  | none => simp [hE] at h
  | some actual =>
      simp only [hE] at h ⊢
      split at h
      · rename_i A C D hA hC hD
        have hk := mapM_key_names actual
        rw [hA, Option.map_some] at hk
        rw [← hk]
        simp only [Bool.and_eq_true] at h
        exact h.1.1.1.1.1.1
      · cases h

/-! ### Closure isolation on sorted lists and a membership bitmap -/

theorem natSetEq_of_sorted {xs ys : List Nat}
    (h1 : subsetW ((msort xs).length + (msort ys).length + 1) (msort xs) (msort ys) = true)
    (h2 : subsetW ((msort ys).length + (msort xs).length + 1) (msort ys) (msort xs) = true) :
    AverCert.WasmSlice.natSetEq xs ys = true := by
  unfold AverCert.WasmSlice.natSetEq AverCert.WasmSlice.indexedSetEq AverCert.WasmSlice.indexedSubset
  simp only [Bool.and_eq_true, List.all_eq_true, orderedSet_contains]
  exact ⟨fun x hx => mem_msort.mp (subsetW_mem h1 x (mem_msort.mpr hx)),
    fun y hy => mem_msort.mp (subsetW_mem h2 y (mem_msort.mpr hy))⟩

/-- `natSetEq` on sorted lists. -/
def setEqSorted (xs ys : List Nat) : Bool :=
  let sx := msort xs
  let sy := msort ys
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
  strictly (msort claim.roots) &&
  strictly (msort claim.helpers) &&
  strictly (msort claim.admitted) &&
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
  refine ⟨⟨⟨⟨⟨⟨⟨natListNodup_of_nodup (nodup_of_msort h1), natListNodup_of_nodup (nodup_of_msort h2)⟩,
    natListNodup_of_nodup (nodup_of_msort h3)⟩, natSetEq_of_setEqSorted h4⟩, h5⟩,
    natSetEq_of_setEqSorted h6⟩, h7⟩, ?_⟩
  rw [← closureFoldB_eq _ _ _ _ 0 (fun x => by simp)]
  split at h8
  · rename_i actual hact
    rw [hact]
    exact natSetEq_of_setEqSorted h8
  · cases h8

end AverCert.SortedKeys

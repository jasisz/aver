-- The data section in blocks, the type section by index, and the type table
-- and helper types read from them.
import ScaleClosure

set_option linter.unusedSimpArgs false

namespace AverCert.ScaleTables
open CertDecode AverCert.ByteWindow AverCert.ScaleBytes AverCert.ScaleLayout AverCert.ScaleExports
open AverCert.Schema AverCert.Grammar AverCert.TypeTable AverCert.DeclaredLayout

/-! ### A section's head

The count, first entry and length of a section, read on a window at its
declared header: `exportsHead` for any section id, and the section's absence
when it has no header (then no entry is declared). -/

def sectionHead (cs : List Nat) (len : Nat) (hs : List Nat) (id e0 : Nat) (ls : List Nat) :
    Bool :=
  framingOk cs len hs &&
  match headersAt cs len 8 hs with
  | some S =>
      match S.find? (fun e => e.1 == id) with
      | some (_, start, size) =>
          match readU (window 1024 cs start (min 5 size)) (min 5 size) with
          | some (cnt, _, rest) =>
              cnt == ls.length && e0 == start + (min 5 size - rest) &&
                ls.sum == size - (min 5 size - rest)
          | none => false
      | none => ls.isEmpty
  | none => false

theorem sectionHead_spec {cs : List Nat} {n len : Nat} {hs : List Nat} {id e0 : Nat}
    {ls : List Nat} (hn : join 1024 cs = n) (hfit : chunksFit 1024 cs = true)
    (h : sectionHead cs len hs id e0 ls = true) :
    (modulePayload id n len = none ∧ ls = []) ∨
    ∃ start size k, modulePayload id n len = some (slice n start size, size) ∧
      readU (slice n start size) size =
        some (ls.length, slice n e0 (size - k), size - k) ∧ ls.sum = size - k := by
  unfold sectionHead at h
  simp only [Bool.and_eq_true] at h
  obtain ⟨hfr, hm⟩ := h
  split at hm
  · rename_i S hS
    have hpay := modulePayload_of_framing hn hfit hfr hS id
    split at hm
    · rename_i id' start size hfind
      rw [hfind] at hpay
      split at hm
      · rename_i cnt w' rest hU
        simp only [Bool.and_eq_true, beq_iff_eq] at hm
        obtain ⟨⟨rfl, rfl⟩, hsum⟩ := hm
        rw [window_eq (by decide) hfit, hn] at hU
        obtain ⟨-, hread⟩ := readU_window_slice (S := size) (by omega) hU
        exact Or.inr ⟨start, size, min 5 size - rest, hpay, hread, hsum⟩
      · cases hm
    · rename_i hfind
      rw [hfind] at hpay
      simp only [List.isEmpty_iff] at hm
      exact Or.inl ⟨hpay, hm⟩
  · cases hm

/-! ### The data section

A data segment is read on its own window (`readDataSeg`, the step of
`CertDecode.decDataVec`). `dataWalk` reads the section a block of segments at a
time from a declared offset, and checks every declared String segment
(`TypeTable.strSegs`, a literal and its segment index) whose index is the
segment it reads. -/

/-- One passive data segment: the flag `0x01`, then its bytes as a name. -/
def readDataSeg (n len : Nat) : Option (List Nat × Nat × Nat) :=
  if len == 0 then none
  else if (n &&& 0xff) == 0x01 then readName (n >>> 8) (len - 1)
  else none

theorem decDataVec_eq_vec : ∀ k n len, decDataVec k n len = vec readDataSeg k n len
  | 0, _, _ => rfl
  | k + 1, n, len => by
      simp only [decDataVec, vec, readDataSeg, readName, decDataVec_eq_vec k]
      by_cases h0 : len = 0
      · simp [h0]
      · by_cases hf : (n &&& 0xff) = 0x01
        · simp only [h0, hf, beq_iff_eq, ↓reduceIte, Bool.false_eq_true]
          rcases readU (n >>> 8) (len - 1) with _ | ⟨bc, n2, len2⟩
          · rfl
          · by_cases hb : bc ≤ len2
            · simp only [hb, ↓reduceIte]
              rcases vec readDataSeg k (n2 >>> (8 * bc)) (len2 - bc) with _ | ⟨rest, n4, len4⟩ <;> rfl
            · simp [hb]
        · simp [h0, hf]

theorem readDataSeg_ext : Ext readDataSeg := by
  intro w l x w' l' hw h
  unfold readDataSeg at h
  by_cases h0 : l = 0
  · simp [h0] at h
  simp only [h0, beq_iff_eq, ↓reduceIte] at h
  split at h
  · rename_i hf
    obtain ⟨hn1, hl1, hext⟩ := readName_ext (shr8_lt h0 hw) h
    refine ⟨hn1, by omega, fun R L => ?_⟩
    unfold readDataSeg
    have hL : (l + L == 0) = false := by simp; omega
    simp only [hL, Bool.false_eq_true, ↓reduceIte, byte_ext h0, hf, shr8_ext h0,
      show l + L - 1 = l - 1 + L by omega]
    exact hext R L
  · cases h

/-- A block of segments `k, k + 1, …` of the given lengths from `off`, each
    read on its window, with every declared String segment whose index falls
    in the block checked against it: the next index and offset. -/
def dataBlock (cs : List Nat) (SS : List (List Nat × Nat)) (k off : Nat) (ls : List Nat) :
    Option (Nat × Nat) :=
  match (winsAt cs off ls).mapM (whole readDataSeg) with
  | none => none
  | some segs =>
      if SS.all (fun x => !(decide (k ≤ x.2) && decide (x.2 < k + ls.length)) ||
          segs[x.2 - k]? == some x.1) then some (k + ls.length, off + ls.sum)
      else none

/-- Segments `k, …` of the given lengths from `off` decode on their windows,
    and every declared String segment of an index among them holds its
    literal. -/
def DataOk (cs : List Nat) (SS : List (List Nat × Nat)) (k off : Nat) (ls : List Nat) : Prop :=
  ∃ segs : List (List Nat), (winsAt cs off ls).mapM (whole readDataSeg) = some segs ∧
    ∀ x ∈ SS, k ≤ x.2 → x.2 < k + ls.length → segs[x.2 - k]? = some x.1

theorem winsAt_append (cs : List Nat) : ∀ (off : Nat) (l1 l2 : List Nat),
    winsAt cs off (l1 ++ l2) = winsAt cs off l1 ++ winsAt cs (off + l1.sum) l2
  | _, [], _ => by simp [winsAt]
  | off, l :: ls, l2 => by
      simp only [List.cons_append, winsAt, List.sum_cons, winsAt_append cs (off + l) ls l2,
        Nat.add_assoc]

theorem dataOk_of_block {cs : List Nat} {SS : List (List Nat × Nat)} {k off k1 off1 : Nat}
    {ls : List Nat} (h : dataBlock cs SS k off ls = some (k1, off1)) :
    k1 = k + ls.length ∧ off1 = off + ls.sum ∧ DataOk cs SS k off ls := by
  unfold dataBlock at h
  split at h
  · cases h
  · rename_i segs hsegs
    split at h
    · rename_i hc
      simp only [Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl⟩ := h
      refine ⟨rfl, rfl, segs, hsegs, fun x hx h1 h2 => ?_⟩
      have := List.all_eq_true.mp hc x hx
      simp only [Bool.or_eq_true, Bool.not_eq_true', Bool.and_eq_false_iff, decide_eq_false_iff_not,
        beq_iff_eq] at this
      rcases this with (h | h) | h
      · omega
      · omega
      · exact h
    · cases h

/-- No segments. -/
theorem dataOk_nil {cs : List Nat} {SS : List (List Nat × Nat)} {k off : Nat} :
    DataOk cs SS k off [] :=
  ⟨[], rfl, fun x _ h1 h2 => by simp at h2; omega⟩

/-- The last block of a walk. -/
theorem dataOk_last {cs : List Nat} {SS : List (List Nat × Nat)} {k off k1 off1 : Nat}
    {ls : List Nat} (h : dataBlock cs SS k off ls = some (k1, off1)) : DataOk cs SS k off ls :=
  (dataOk_of_block h).2.2

/-- A block, and the blocks after it. -/
theorem dataOk_cons {cs : List Nat} {SS : List (List Nat × Nat)} {k off k1 off1 : Nat}
    {l1 l2 : List Nat} (h1 : dataBlock cs SS k off l1 = some (k1, off1))
    (h2 : DataOk cs SS k1 off1 l2) : DataOk cs SS k off (l1 ++ l2) := by
  obtain ⟨rfl, rfl, segs1, hs1, hp1⟩ := dataOk_of_block h1
  obtain ⟨segs2, hs2, hp2⟩ := h2
  have hlen : segs1.length = l1.length := by
    rw [mapM_length hs1, winsAt_length]
  refine ⟨segs1 ++ segs2, ?_, fun x hx ha hb => ?_⟩
  · rw [winsAt_append, List.mapM_append, hs1, hs2]; rfl
  · by_cases hx1 : x.2 < k + l1.length
    · rw [List.getElem?_append_left (by omega)]
      exact hp1 x hx (by omega) hx1
    · rw [List.getElem?_append_right (by omega), hlen,
        show x.2 - k - l1.length = x.2 - (k + l1.length) by omega]
      exact hp2 x hx (by omega) (by simp at hb; omega)

/-- The data section from its head and its segments decoded on their
    windows. -/
theorem decodeData_of_cut {cs : List Nat} {n len : Nat} {hs : List Nat} {d0 : Nat}
    {ls : List Nat} {segs : List (List Nat)}
    (hn : join 1024 cs = n) (hfit : chunksFit 1024 cs = true)
    (hH : sectionHead cs len hs 11 d0 ls = true)
    (hD : (winsAt cs d0 ls).mapM (whole readDataSeg) = some segs) :
    decodeData n len = some segs := by
  rcases sectionHead_spec hn hfit hH with ⟨hpay, rfl⟩ | ⟨start, size, k, hpay, hread, hsum⟩
  · simp only [winsAt, List.mapM_nil, Option.pure_def, Option.some.injEq] at hD
    subst hD
    unfold decodeData
    rw [hpay]
  · unfold decodeData
    rw [hpay]
    simp only [hread]
    rw [decDataVec_eq_vec]
    have hw := winsAt_eq hn hfit ls d0 (size - k) (by omega)
    rw [hw] at hD
    have hv := vec_of_windows readDataSeg_ext ls (slice n d0 (size - k)) (size - k) segs
      (by omega) hD
    rw [hv, hsum, Nat.sub_self]
    rfl

/-! ### The String literals of the plans -/

/-- Every String literal of a plan has a declared segment. -/
def litsDeclared (SS : List (List Nat × Nat)) (p : FnPlan) : Bool :=
  (exprLits p.body).all fun b => SS.any fun x => x.1 == b

/-- `TypeTable.dataConfirmed` from the data section's head, its blocks, the
    declared segments' indices below the segment count, and every plan's
    literals declared. -/
theorem dataConfirmed_of_blocks {cs : List Nat} {n len : Nat} {hs : List Nat} {d0 : Nat}
    {ls : List Nat} {s : Subject} {tt : TypeTable} {fns : List FnEntry}
    (hn : join 1024 cs = n) (hfit : chunksFit 1024 cs = true)
    (hH : sectionHead cs len hs 11 d0 ls = true)
    (hW : DataOk cs tt.strSegs 0 d0 ls)
    (hB : tt.strSegs.all (fun x => decide (x.2 < ls.length)) = true)
    (hP : fns.all (fun e => litsDeclared tt.strSegs e.plan) = true) :
    dataConfirmed n len s tt fns = true := by
  obtain ⟨segs, hsegs, hpin⟩ := hW
  have hd := decodeData_of_cut hn hfit hH hsegs
  rw [List.all_eq_true] at hB
  have hpin' : ∀ x ∈ tt.strSegs, segs[x.2]? = some x.1 := by
    intro x hx
    have := hpin x hx (Nat.zero_le _) (by simpa using hB x hx)
    simpa using this
  unfold dataConfirmed
  rw [hd]
  simp only [Bool.and_eq_true, List.all_eq_true, beq_iff_eq]
  refine ⟨fun x hx => hpin' x hx, fun e he => ?_⟩
  have hl := List.all_eq_true.mp hP e he
  unfold litsDeclared at hl
  unfold DataPin
  rw [List.all_eq_true] at hl ⊢
  intro b hb
  obtain ⟨x, hx, hxb⟩ := List.any_eq_true.mp (hl b hb)
  rw [beq_iff_eq] at hxb
  have hfind : (tt.strSegs.find? fun y => decide (y.1 = b)).isSome = true := by
    rw [List.find?_isSome]
    exact ⟨x, hx, by simp [hxb]⟩
  obtain ⟨y, hy⟩ := Option.isSome_iff_exists.mp hfind
  have hy1 : y.1 = b := by simpa using List.find?_some hy
  have hymem : y ∈ tt.strSegs := List.mem_of_find?_eq_some hy
  have hseg : (mctxOf s tt fns).strSeg b = y.2 := by
    simp only [mctxOf, hy, Option.map_some, idxOr]
  rw [beq_iff_eq, hseg, hpin' y hymem, hy1]


/-! ### The type section by index

`TL` declares every top-level entry of the type section by module offset and
length, and `RL` every subtype of the rec group that opens the section.
`typesTiled` requires `TL`'s lengths to be the type cut the type walk read and
its entries to tile the section from the walk's first entry. `recTiled` reads
the rec group's head on a window (`0x4e` and the subtype count) and requires
`RL`'s subtypes to tile the rest of the group. With every subtype decoding on
its own window (`subOk`, a block of subtypes per declaration) and every
top-level entry after the first starting with another byte than `0x4e`
(`notRec`), the decoded type section is the subtypes in order, then the
single entries in order (`decodeTypes_of_layout`). A type is then read on its
own window by its index (`typeAt`), and the rec group's raw entries likewise
(`firstRecGroup_of_layout`, `rawAt`): no check walks a list of entries to find
one, and none decodes an entry it does not read. -/

/-- Entry `j` of a layout on its chunk window. -/
def subWin (cs : List Nat) (L : Layout) (j : Nat) : Nat × Nat :=
  (window 1024 cs (L.off j) (L.len j), L.len j)

/-- Entry `j`'s single subtype, decoded on its window. -/
def subEntry (cs : List Nat) (L : Layout) (j : Nat) : TypeEntry :=
  entryOf readTypeEntry noTypeEntry (subWin cs L j)

def subOk (cs : List Nat) (RL : Layout) (j : Nat) : Bool :=
  (whole readTypeEntry (subWin cs RL j)).isSome

def notRec (cs : List Nat) (TL : Layout) (k : Nat) : Bool :=
  window 1024 cs (TL.off k) 1 != 0x4e

def recTiled (cs : List Nat) (TL RL : Layout) : Bool :=
  decide (0 < TL.count) && decide (0 < TL.len 0) && window 1024 cs (TL.off 0) 1 == 0x4e &&
  match readU (window 1024 cs (TL.off 0 + 1) (min 5 (TL.len 0 - 1))) (min 5 (TL.len 0 - 1)) with
  | some (cnt, _, rest) =>
      cnt == RL.count &&
        tiled RL 0 RL.count (TL.off 0 + 1 + (min 5 (TL.len 0 - 1) - rest)) (TL.off 0 + TL.len 0)
  | none => false

def typesTiled (TL : Layout) (t0 : Nat) (ls : List Nat) : Bool :=
  ls == (List.range' 0 TL.count).map TL.len && tiled TL 0 TL.count t0 (t0 + ls.sum)

def subEntries (cs : List Nat) (RL : Layout) : List TypeEntry :=
  (List.range RL.count).map (subEntry cs RL)

def topEntries (cs : List Nat) (TL : Layout) : List TypeEntry :=
  (List.range' 1 (TL.count - 1)).map (subEntry cs TL)

/-- The type at index `t`: a subtype of the opening rec group, or a single
    top-level entry after it. -/
def typeAt (cs : List Nat) (TL RL : Layout) (t : Nat) : Option TypeEntry :=
  if t < RL.count then some (subEntry cs RL t)
  else if t - RL.count + 1 < TL.count then some (subEntry cs TL (t - RL.count + 1))
  else none

theorem winsAt_tiled {cs : List Nat} {L : Layout} :
    ∀ (r k pos stop : Nat), tiled L k r pos stop = true →
      winsAt cs pos ((List.range' k r).map L.len) = (List.range' k r).map (subWin cs L)
  | 0, _, _, _, _ => rfl
  | r + 1, k, pos, stop, h => by
      simp only [tiled, Bool.and_eq_true, beq_iff_eq] at h
      obtain ⟨rfl, h⟩ := h
      simp only [List.range'_succ, List.map_cons, winsAt, subWin, List.cons.injEq, true_and]
      exact winsAt_tiled r (k + 1) _ stop h

theorem slice_byte (n off l : Nat) (hl : 0 < l) : slice n off l &&& 0xff = slice n off 1 := by
  rw [land_ff, show (256 : Nat) = 2 ^ (8 * 1) from rfl, slice_mod n off 1 l hl]

theorem mapM_subOk {cs : List Nat} {n : Nat} (hn : join 1024 cs = n)
    (hfit : chunksFit 1024 cs = true) {L : Layout} :
    ∀ (k r : Nat), allRange (subOk cs L) k r = true →
      ((List.range' k r).map fun j => (slice n (L.off j) (L.len j), L.len j)).mapM
        (whole readTypeEntry) = some ((List.range' k r).map (subEntry cs L))
  | _, 0, _ => rfl
  | k, r + 1, h => by
      simp only [allRange, Bool.and_eq_true] at h
      obtain ⟨hk, hrest⟩ := h
      unfold subOk at hk
      obtain ⟨e, he⟩ := Option.isSome_iff_exists.mp hk
      have hs : whole readTypeEntry (slice n (L.off k) (L.len k), L.len k) = some e := by
        have := he
        unfold subWin at this
        rw [window_eq (by decide) hfit, hn] at this
        exact this
      simp only [List.range'_succ, List.map_cons, List.mapM_cons, hs, Option.bind_eq_bind,
        Option.bind_some, mapM_subOk hn hfit (k + 1) r hrest, Option.pure_def, Nat.mul_one]
      rw [subEntry, entryOf_eq he]

/-- The rec group's head and subtypes read on windows. -/
theorem recHead_spec {cs : List Nat} {n : Nat} (hn : join 1024 cs = n)
    (hfit : chunksFit 1024 cs = true) {TL RL : Layout} (hR : recTiled cs TL RL = true) :
    0 < TL.count ∧ 0 < TL.len 0 ∧ slice n (TL.off 0) 1 = 0x4e ∧
    ∃ r, TL.off 0 + 1 ≤ r ∧ r ≤ TL.off 0 + TL.len 0 ∧
      tiled RL 0 RL.count r (TL.off 0 + TL.len 0) = true ∧
      ∀ S, TL.len 0 - 1 ≤ S → readU (slice n (TL.off 0 + 1) S) S =
        some (RL.count, slice n r (S - (r - (TL.off 0 + 1))), S - (r - (TL.off 0 + 1))) := by
  unfold recTiled at hR
  simp only [Bool.and_eq_true, decide_eq_true_eq, beq_iff_eq] at hR
  obtain ⟨⟨⟨hc, hl⟩, hb⟩, hm⟩ := hR
  rw [window_eq (by decide) hfit, hn] at hb
  refine ⟨hc, hl, hb, ?_⟩
  split at hm
  · rename_i cnt w' rest hU
    simp only [Bool.and_eq_true, beq_iff_eq] at hm
    obtain ⟨rfl, htile⟩ := hm
    rw [window_eq (by decide) hfit, hn] at hU
    have hrest := (readU_ext (slice_lt _ _ _) hU).2.1
    refine ⟨_, by omega, by omega, htile, fun S hS => ?_⟩
    obtain ⟨-, hread⟩ := readU_window_slice (S := S) (by omega) hU
    rw [show TL.off 0 + 1 + (min 5 (TL.len 0 - 1) - rest) - (TL.off 0 + 1) =
      min 5 (TL.len 0 - 1) - rest by omega]
    exact hread
  · cases hm

theorem readRecEntry_of_layout {cs : List Nat} {n : Nat} (hn : join 1024 cs = n)
    (hfit : chunksFit 1024 cs = true) {TL RL : Layout} (hR : recTiled cs TL RL = true)
    (hS : allRange (subOk cs RL) 0 RL.count = true) :
    whole readRecEntry (subWin cs TL 0) = some (subEntries cs RL) := by
  obtain ⟨hc, hl, hb, r, hr1, hr2, htile, hread⟩ := recHead_spec hn hfit hR
  obtain ⟨hsum, hwin⟩ := tiled_spec (n := n) RL.count 0 r htile
  have hU := hread (TL.len 0 - 1) (Nat.le_refl _)
  have hw := hwin (TL.len 0 - 1 - (r - (TL.off 0 + 1))) (by omega)
  have hm := mapM_subOk hn hfit 0 RL.count hS
  rw [← hw] at hm
  have hv := vec_of_windows readTypeEntry_ext _ _ (TL.len 0 - 1 - (r - (TL.off 0 + 1))) _
    (by omega) hm
  simp only [List.length_map, List.length_range'] at hv
  unfold subWin whole
  rw [window_eq (by decide) hfit, hn]
  unfold readRecEntry
  have hl0 : (TL.len 0 == 0) = false := by simp; omega
  rw [slice_shr n (TL.off 0) (TL.len 0) 1 (by omega), slice_byte n _ _ hl, hb]
  simp only [hl0, Bool.false_eq_true, ↓reduceIte, beq_self_eq_true, hU,
    readTypeEntries_eq_vec, hv]
  rw [show TL.len 0 - 1 - (r - (TL.off 0 + 1)) - ((List.range' 0 RL.count).map RL.len).sum = 0 by
    omega]
  simp only [subEntries, List.range_eq_range']

/-- A top-level entry that does not open a rec group decodes as its one
    subtype. -/
theorem single_of_notRec {cs : List Nat} {n : Nat} (hn : join 1024 cs = n)
    (hfit : chunksFit 1024 cs = true) {TL : Layout} {k : Nat} (hk : notRec cs TL k = true)
    {g : List TypeEntry} (h : whole readRecEntry (subWin cs TL k) = some g) :
    g = [subEntry cs TL k] := by
  unfold notRec at hk
  unfold subWin whole at h
  rw [window_eq (by decide) hfit, hn] at h hk
  unfold readRecEntry at h
  by_cases hl : TL.len k = 0
  · simp [hl] at h
  have hl0 : (TL.len k == 0) = false := by simp; omega
  rw [slice_byte n _ _ (by omega)] at h
  have hne : (slice n (TL.off k) 1 == 0x4e) = false := by simpa using hk
  simp only [hl0, hne, Bool.false_eq_true, ↓reduceIte] at h
  cases he : readTypeEntry (slice n (TL.off k) (TL.len k)) (TL.len k) with
  | none => simp [he] at h
  | some p =>
      obtain ⟨e, w1, l1⟩ := p
      simp only [he, Option.map_some] at h
      split at h
      · rename_i x w' hx
        simp only [Option.some.injEq, Prod.mk.injEq] at hx h
        obtain ⟨rfl, -, rfl⟩ := hx
        subst h
        have : whole readTypeEntry (subWin cs TL k) = some e := by
          unfold subWin whole
          rw [window_eq (by decide) hfit, hn, he]
          rfl
        rw [subEntry, entryOf_eq this]
      · cases h

theorem flatten_singles {cs : List Nat} {n : Nat} (hn : join 1024 cs = n)
    (hfit : chunksFit 1024 cs = true) {TL : Layout} :
    ∀ (k r : Nat) (gs : List (List TypeEntry)), allRange (notRec cs TL) k r = true →
      ((List.range' k r).map (subWin cs TL)).mapM (whole readRecEntry) = some gs →
      gs.flatten = (List.range' k r).map (subEntry cs TL)
  | _, 0, gs, _, h => by
      simp only [List.range'_zero, List.map_nil, List.mapM_nil, Option.pure_def,
        Option.some.injEq] at h
      subst h; rfl
  | k, r + 1, gs, hN, h => by
      simp only [allRange, Bool.and_eq_true] at hN
      simp only [List.range'_succ, List.map_cons, List.mapM_cons, Option.bind_eq_bind,
        Option.pure_def] at h
      cases hg : whole readRecEntry (subWin cs TL k) with
      | none => simp [hg] at h
      | some g =>
          cases hgs : ((List.range' (k + 1) r).map (subWin cs TL)).mapM (whole readRecEntry) with
          | none => simp [hg, hgs] at h
          | some gs' =>
              simp only [hg, hgs, Option.bind_some, Option.some.injEq] at h
              subst h
              rw [List.flatten_cons, single_of_notRec hn hfit hN.1 hg,
                flatten_singles hn hfit (k + 1) r gs' hN.2 hgs]
              rfl

/-- The decoded type section, from the type walk and the declared layouts. -/
theorem decodeTypes_of_layout {cs : List Nat} {n len : Nat} {hs : List Nat} {t0 : Nat}
    {ls : List Nat} {SB : List Nat} {S : Nat} {sigs : List (Nat × CertDecode.StringHost.Sig)}
    {T off' : Nat} {sb' : List Nat} {TL RL : Layout}
    (hn : join 1024 cs = n) (hfit : chunksFit 1024 cs = true)
    (hH : AverCert.ScaleTypes.typesHead cs len hs t0 ls = true)
    (hW : AverCert.ScaleTypes.walkTypes cs SB S sigs 0 t0 ls SB = some (T, off', sb'))
    (hT : typesTiled TL t0 ls = true) (hR : recTiled cs TL RL = true)
    (hS : allRange (subOk cs RL) 0 RL.count = true)
    (hN : allRange (notRec cs TL) 1 (TL.count - 1) = true) :
    decodeTypes n len = some (typeInfoOf (subEntries cs RL ++ topEntries cs TL)) := by
  obtain ⟨gs, hG, -⟩ := AverCert.ScaleTypes.walkTypes_spec ls 0 t0 SB T off' sb' hW
  rw [decodeTypes_of_cut (AverCert.ScaleTypes.typesCut_of_walk hn hfit hH hG)]
  unfold typesTiled at hT
  simp only [Bool.and_eq_true, beq_iff_eq] at hT
  obtain ⟨hls, htile⟩ := hT
  have hc := (recHead_spec hn hfit hR).1
  rw [hls, winsAt_tiled _ _ _ _ htile] at hG
  have hsplit : List.range' 0 TL.count = 0 :: List.range' 1 (TL.count - 1) := by
    obtain ⟨c, hc'⟩ : ∃ c, TL.count = c + 1 := ⟨TL.count - 1, by omega⟩
    rw [hc', List.range'_succ]
    simp
  rw [hsplit] at hG
  simp only [List.map_cons, List.mapM_cons, Option.bind_eq_bind, Option.pure_def] at hG
  rw [readRecEntry_of_layout hn hfit hR hS] at hG
  cases hgs : ((List.range' 1 (TL.count - 1)).map (subWin cs TL)).mapM (whole readRecEntry) with
  | none => simp [hgs] at hG
  | some gs' =>
      simp only [hgs, Option.bind_some, Option.some.injEq] at hG
      subst hG
      rw [List.flatten_cons, flatten_singles hn hfit 1 (TL.count - 1) gs' hN hgs]
      rfl

theorem typeAt_eq {cs : List Nat} {TL RL : Layout} (hc : 0 < TL.count) (t : Nat) :
    (subEntries cs RL ++ topEntries cs TL)[t]? = typeAt cs TL RL t := by
  unfold typeAt subEntries topEntries
  by_cases h1 : t < RL.count
  · rw [List.getElem?_append_left (by simpa using h1)]
    simp [h1, List.getElem?_range h1]
  · rw [List.getElem?_append_right (by simp; omega)]
    simp only [List.length_map, List.length_range, h1, ↓reduceIte, List.getElem?_map]
    by_cases h2 : t - RL.count + 1 < TL.count
    · rw [List.getElem?_range' (by omega)]
      simp only [h2, ↓reduceIte]
      simp only [Option.map_some, Option.some.injEq]
      congr 1; omega
    · rw [List.getElem?_eq_none (by simp; omega)]
      simp [h2]

/-- A type-section lookup, read on the type's own window. -/
theorem typeSectionMatches_at {cs : List Nat} {n len : Nat} {TL RL : Layout}
    (h : decodeTypes n len = some (typeInfoOf (subEntries cs RL ++ topEntries cs TL)))
    (hc : 0 < TL.count) (check : TypeEntry → Bool) (t : Nat) :
    AverCert.WasmSlice.typeSectionMatches check n len t =
      match typeAt cs TL RL t with
      | some e => check e
      | none => false := by
  unfold AverCert.WasmSlice.typeSectionMatches
  rw [h]
  simp only [typeInfoOf, List.getElem?_toArray, typeAt_eq hc]
  cases typeAt cs TL RL t <;> rfl

/-! ### The opening rec group, raw -/

/-- `readTypeEntry` with the entry's bytes. -/
def readTypeEntryRaw (n len : Nat) : Option ((List Nat × TypeEntry) × Nat × Nat) :=
  match readTypeEntry n len with
  | some (e, n1, len1) => some ((takeBytes (len - len1) n, e), n1, len1)
  | none => none

theorem readEntriesRaw_eq_vec : ∀ k n len,
    readEntriesRaw k n len = (vec readTypeEntryRaw k n len).map (·.1)
  | 0, _, _ => rfl
  | k + 1, n, len => by
      simp only [readEntriesRaw, vec, readTypeEntryRaw, readEntriesRaw_eq_vec k]
      rcases readTypeEntry n len with _ | ⟨e, n1, l1⟩
      · rfl
      · simp only []
        rcases vec readTypeEntryRaw k n1 l1 with _ | ⟨xs, n2, l2⟩ <;> rfl

theorem readTypeEntryRaw_ext : Ext readTypeEntryRaw := by
  intro w l x w' l' hw h
  unfold readTypeEntryRaw at h
  split at h
  · rename_i e w1 l1 he
    simp only [Option.some.injEq, Prod.mk.injEq] at h
    obtain ⟨rfl, rfl, rfl⟩ := h
    obtain ⟨hw1, hl1, hx⟩ := readTypeEntry_ext hw he
    refine ⟨hw1, hl1, fun R L => ?_⟩
    unfold readTypeEntryRaw
    rw [hx R L]
    simp only [show l + L - (l1 + L) = l - l1 by omega, takeBytes_ext (Nat.sub_le l l1) hw]
  · cases h

/-- The rec group's raw entries: each subtype's bytes and its decoding. -/
def subRaw (cs : List Nat) (RL : Layout) : List (List Nat × TypeEntry) :=
  (List.range RL.count).map fun j => (takeBytes (RL.len j) (subWin cs RL j).1, subEntry cs RL j)

def rawAt (cs : List Nat) (RL : Layout) (i : Nat) : Option (List Nat × TypeEntry) :=
  if i < RL.count then some (takeBytes (RL.len i) (subWin cs RL i).1, subEntry cs RL i) else none

theorem rawAt_eq (cs : List Nat) (RL : Layout) (i : Nat) : (subRaw cs RL)[i]? = rawAt cs RL i := by
  unfold subRaw rawAt
  by_cases h : i < RL.count <;> simp [h]

theorem whole_raw (w : Nat × Nat) :
    whole readTypeEntryRaw w = (whole readTypeEntry w).map (fun e => (takeBytes w.2 w.1, e)) := by
  unfold whole readTypeEntryRaw
  cases readTypeEntry w.1 w.2 with
  | none => rfl
  | some p =>
      obtain ⟨e, w1, l1⟩ := p
      cases l1 <;> simp

theorem mapM_subRaw {cs : List Nat} {n : Nat} (hn : join 1024 cs = n)
    (hfit : chunksFit 1024 cs = true) {L : Layout} :
    ∀ (k r : Nat), allRange (subOk cs L) k r = true →
      ((List.range' k r).map fun j => (slice n (L.off j) (L.len j), L.len j)).mapM
        (whole readTypeEntryRaw) = some ((List.range' k r).map fun j =>
          (takeBytes (L.len j) (subWin cs L j).1, subEntry cs L j))
  | _, 0, _ => rfl
  | k, r + 1, h => by
      simp only [allRange, Bool.and_eq_true] at h
      obtain ⟨hk, hrest⟩ := h
      unfold subOk at hk
      obtain ⟨e, he⟩ := Option.isSome_iff_exists.mp hk
      have hs : whole readTypeEntry (slice n (L.off k) (L.len k), L.len k) = some e := by
        have := he
        unfold subWin at this
        rw [window_eq (by decide) hfit, hn] at this
        exact this
      simp only [List.range'_succ, List.map_cons, List.mapM_cons, whole_raw, hs, Option.map_some,
        Option.bind_eq_bind, Option.bind_some, mapM_subRaw hn hfit (k + 1) r hrest,
        Option.pure_def, Nat.mul_one]
      rw [subEntry, entryOf_eq he, subWin, window_eq (by decide) hfit, hn]

theorem firstRecGroup_of_layout {cs : List Nat} {n len : Nat} {hs : List Nat} {t0 : Nat}
    {ls : List Nat} {TL RL : Layout}
    (hn : join 1024 cs = n) (hfit : chunksFit 1024 cs = true)
    (hH : AverCert.ScaleTypes.typesHead cs len hs t0 ls = true)
    (hT : typesTiled TL t0 ls = true) (hR : recTiled cs TL RL = true)
    (hS : allRange (subOk cs RL) 0 RL.count = true) :
    firstRecGroup n len = some (subRaw cs RL) := by
  obtain ⟨start, size, k, hpay, hread, hsum⟩ := AverCert.ScaleTypes.typesHead_spec hn hfit hH
  obtain ⟨hc, hl, hb, r, hr1, hr2, htile, hreadU⟩ := recHead_spec hn hfit hR
  unfold typesTiled at hT
  simp only [Bool.and_eq_true, beq_iff_eq] at hT
  obtain ⟨hls, htop⟩ := hT
  -- The first top-level entry starts the section and lies inside it.
  have ht0 : TL.off 0 = t0 := by
    cases hcount : TL.count with
    | zero => omega
    | succ c =>
        rw [hcount] at htop
        simp only [tiled, Bool.and_eq_true, beq_iff_eq] at htop
        exact htop.1
  have hlen0 : TL.len 0 ≤ ls.sum := by
    rw [hls]
    cases hcount : TL.count with
    | zero => omega
    | succ c => simp [List.range'_succ]
  obtain ⟨hsumr, hwin⟩ := tiled_spec (n := n) RL.count 0 r htile
  have hS1 : ls.sum = size - k := hsum
  unfold firstRecGroup typeSectionStart
  rw [hpay]
  simp only [hread]
  have hnz : (size - k == 0) = false := by simp; omega
  rw [← ht0, slice_byte n _ _ (by omega), show slice n (TL.off 0) 1 = 0x4e by rw [ht0] at hb ⊢; exact hb,
    slice_shr n (TL.off 0) (size - k) 1 (by omega)]
  simp only [hnz, Bool.false_eq_true, ↓reduceIte, beq_self_eq_true,
    hreadU (size - k - 1) (by omega)]
  rw [readEntriesRaw_eq_vec]
  have hraw := mapM_subRaw hn hfit 0 RL.count hS
  rw [← hwin (size - k - 1 - (r - (TL.off 0 + 1))) (by omega)] at hraw
  have hv := vec_of_windows readTypeEntryRaw_ext _ _ (size - k - 1 - (r - (TL.off 0 + 1))) _
    (by omega) hraw
  simp only [List.length_map, List.length_range'] at hv
  rw [hv]
  simp only [Option.map_some, subRaw, List.range_eq_range']

/-! ### The type table on the rec group, read by index

`TypeTable.typeTableConfirmed` reads the opening rec group as a list,
`grp[idx]?` for every declaration. `tableBody` is the same check with the
entries read through a lookup (`tableBody_eq`), so the rec group's entries are
read on their own windows by index (`rawAt`). -/

section Look
variable (M : MCtx) (look : Nat → Option (List Nat × TypeEntry))

def structIsL (idx : Nat) (ts : List Ty) : Bool :=
  match look idx, storagesOf M ts with
  | some (_, e), some ss => structStorages e == some ss
  | _, _ => false

def arrayIsL (idx : Nat) (st : CertDecode.StorageType) : Bool :=
  match look idx with
  | some (_, ⟨_, .arrayType f⟩) => f.storage == st
  | _ => false

def rootIsL (idx : Nat) : Bool :=
  match look idx with
  | some (_, e) => e == ⟨.sub [], .structType []⟩
  | none => false

def ctorIsL (root : Nat) (c : Nat × List Ty) : Bool :=
  structIsL M look c.1 c.2 &&
    (match look c.1 with
     | some (_, e) => e.form == .subFinal [root]
     | none => false)

def recordConfirmedL (r : RecordDecl) : Bool :=
  match r.fields with
  | [f] => valTyD M f == some (.ref 0x63 (Int.ofNat r.struct)) && decide (r.struct < 4294967296)
  | _ => decide (2 ≤ r.fields.length) && structIsL M look r.struct r.fields

def S3PinL (tid ncs : Nat) : Bool :=
  (List.range ncs).all fun c =>
    match (look (M.ctorStruct tid c)).map (·.1), ctorEntryHeader (M.sumRoot tid) with
    | some e, some h => h.isPrefixOf e
    | _, _ => false

def sumConfirmedL (d : SumDecl) : Bool :=
  rootIsL look d.root && d.ctors.all (ctorIsL M look d.root) && sumOk M d.tid &&
    S3PinL M look d.tid d.ctors.length

def carrierConfirmedL (cst : Option (Option Nat)) (tt : TypeTable) : Bool :=
  match cst, tt.carrier, tt.mag with
  | some (some c), some c', some m =>
      c == c' && decide (c < 4294967296) && decide (m < 4294967296) &&
        arrayIsL look m (.val (.numeric 0x7e)) &&
        (match look c with
         | some (_, ⟨_, .structType fs⟩) => (fs[1]?).map (·.storage) == some (refTo m)
         | _ => false)
  | some none, none, none => true
  | _, _, _ => false

/-- The rec group part of `typeTableConfirmed`, reading entries through `look`,
    `glen` the group's length and `cst` the carrier state. -/
def tableBody (cst : Option (Option Nat)) (glen : Nat) (tt : TypeTable) : Bool :=
  keysUnique tt &&
  carrierConfirmedL look cst tt &&
  natNodup (ownedStructs tt) &&
  tt.records.all (recordConfirmedL M look) &&
  tt.sums.all (sumConfirmedL M look) &&
  tt.options.all (fun o => structIsL M look o.2 [.bool, o.1]) &&
  tt.results.all (fun r => structIsL M look r.2.2 [.bool, r.1, r.2.1]) &&
  tt.lists.all (fun l => structIsL M look l.2 [l.1, .list l.1]) &&
  tt.vecs.all (fun v => match storagesOf M [v.1] with
    | some [st] => arrayIsL look v.2 st
    | _ => false) &&
  tt.opaques.all (fun o => decide (o.2 < glen)) &&
  (match tt.str with
   | some i => arrayIsL look i (.packed 0x78)
   | none => true) &&
  (match tt.strVec, tt.str with
   | some v, some i => arrayIsL look v (refTo i)
   | none, _ => true
   | some _, none => false)

end Look

theorem S3Pin_eq (M : MCtx) (grp : List (List Nat × TypeEntry)) (tid ncs : Nat) :
    S3Pin M tid ncs (grp.map (·.1)) = S3PinL M (fun i => grp[i]?) tid ncs := by
  unfold S3Pin S3PinL
  simp only [List.getElem?_map]
  rfl

theorem structIs_eq (M : MCtx) (grp : List (List Nat × TypeEntry)) (idx : Nat) (ts : List Ty) :
    structIs M grp idx ts = structIsL M (fun i => grp[i]?) idx ts := rfl

theorem arrayIs_eq (grp : List (List Nat × TypeEntry)) (idx : Nat) (st : CertDecode.StorageType) :
    arrayIs grp idx st = arrayIsL (fun i => grp[i]?) idx st := rfl

theorem recordConfirmed_eq (M : MCtx) (grp : List (List Nat × TypeEntry)) :
    recordConfirmed M grp = recordConfirmedL M (fun i => grp[i]?) := by
  funext r; rfl

theorem sumConfirmed_eq (M : MCtx) (grp : List (List Nat × TypeEntry)) :
    sumConfirmed M grp = sumConfirmedL M (fun i => grp[i]?) := by
  funext d
  unfold sumConfirmed sumConfirmedL
  rw [S3Pin_eq]
  rfl

theorem carrierConfirmed_eq (n len : Nat) (grp : List (List Nat × TypeEntry)) (tt : TypeTable) :
    carrierConfirmed n len grp tt =
      carrierConfirmedL (fun i => grp[i]?) (CertDecode.carrierState n len) tt := rfl

/-- `typeTableConfirmed`, with the rec group's entries read through a
    lookup. -/
theorem typeTableConfirmed_eq (n len : Nat) (s : Subject) (tt : TypeTable) (fns : List FnEntry) :
    typeTableConfirmed n len s tt fns =
      ((decodeTypes n len).isSome &&
        match firstRecGroup n len with
        | none => false
        | some grp =>
            tableBody (mctxOf s tt fns) (fun i => grp[i]?) (CertDecode.carrierState n len)
              grp.length tt) := by
  unfold typeTableConfirmed
  cases firstRecGroup n len with
  | none => rfl
  | some grp =>
      simp only [sumConfirmed_eq, recordConfirmed_eq, structIs_eq, arrayIs_eq, carrierConfirmed_eq]
      rfl

/-- `typeTableConfirmed` from the declared layouts and the rec group part
    read by index. -/
theorem typeTableConfirmed_of_layout {cs : List Nat} {n len : Nat} {hs : List Nat} {t0 : Nat}
    {ls : List Nat} {SB : List Nat} {S : Nat} {sigs : List (Nat × CertDecode.StringHost.Sig)}
    {T off' : Nat} {sb' : List Nat} {TL RL : Layout} {s : Subject} {tt : TypeTable}
    {fns : List FnEntry}
    (hn : join 1024 cs = n) (hfit : chunksFit 1024 cs = true)
    (hH : AverCert.ScaleTypes.typesHead cs len hs t0 ls = true)
    (hW : AverCert.ScaleTypes.walkTypes cs SB S sigs 0 t0 ls SB = some (T, off', sb'))
    (hT : typesTiled TL t0 ls = true) (hR : recTiled cs TL RL = true)
    (hS : allRange (subOk cs RL) 0 RL.count = true)
    (hN : allRange (notRec cs TL) 1 (TL.count - 1) = true)
    (h : tableBody (mctxOf s tt fns) (rawAt cs RL)
      (some (firstCarrier 0 (subEntries cs RL ++ topEntries cs TL))) RL.count tt = true) :
    typeTableConfirmed n len s tt fns = true := by
  have hd := decodeTypes_of_layout hn hfit hH hW hT hR hS hN
  have hg := firstRecGroup_of_layout hn hfit hH hT hR hS
  have hcst : CertDecode.carrierState n len =
      some (firstCarrier 0 (subEntries cs RL ++ topEntries cs TL)) := by
    unfold CertDecode.carrierState; rw [hd]; rfl
  have hlook : (fun i => (subRaw cs RL)[i]?) = rawAt cs RL := funext (rawAt_eq cs RL)
  have hglen : (subRaw cs RL).length = RL.count := by simp [subRaw]
  rw [typeTableConfirmed_eq, hd, hg, hcst]
  simp only [Option.isSome_some, Bool.true_and]
  rw [hlook, hglen]
  exact h

/-! ### Helper exports by site

`boxIdx`, `toIndexIdx` and `cmpIdx` search the export section for a helper's
name. With the export names distinct, the entry the package declares at a
site (`ScaleLayout.exportSite`: an entry start, decoded on its own window) is
the one the search finds (`helperIdx_of_site`), so no check decodes the
export section to find a helper. -/

theorem helperIdx_of_site {cs : List Nat} {n len : Nat} {hs : List Nat} {e0 B : Nat}
    {ls : List Nat} {off l fi : Nat} {name : List Nat}
    (hn : join 1024 cs = n) (hfit : chunksFit 1024 cs = true)
    (hH : exportsHead cs len hs e0 ls = true)
    (hcut : (decodeRawExportsCut n len ls).isSome = true) (hB : startBits 0 ls = B)
    (hX : exportNamesDistinct n len = true) (hsite : exportSite cs e0 B off l name fi = true) :
    Chars.helperIdx n len name = some fi := by
  obtain ⟨E, hE⟩ := Option.isSome_iff_exists.mp hcut
  obtain ⟨p, hp⟩ := exportSite_mem hn hfit hH hE hB hsite
  unfold exportNamesDistinct at hX
  rw [decodeRawExports_of_cut hE] at hX
  unfold Chars.helperIdx
  rw [decodeRawExports_of_cut hE]
  exact findExportFuncIndex_of_pos (byteSeqListNodup_nodup hX) hp

theorem carrierHelperAbsent_of_site {cs : List Nat} {n len : Nat} {hs : List Nat} {e0 B : Nat}
    {ls : List Nat} {off l fi : Nat}
    (hn : join 1024 cs = n) (hfit : chunksFit 1024 cs = true)
    (hH : exportsHead cs len hs e0 ls = true)
    (hcut : (decodeRawExportsCut n len ls).isSome = true) (hB : startBits 0 ls = B)
    (hX : exportNamesDistinct n len = true)
    (hsite : exportSite cs e0 B off l (AverCert.AcceptedArtifact.stringBytes "__rt_aint_from_i64") fi = true) :
    CertDecode.AddSub.carrierHelperAbsent n len = false := by
  have h := helperIdx_of_site hn hfit hH hcut hB hX hsite
  rw [Chars.carrierHelperAbsent_eq]
  unfold Chars.helperIdx at h
  cases hr : decodeRawExports n len with
  | none => rfl
  | some raw =>
      rw [hr] at h
      simp only [Option.bind_some] at h
      simp [h]

/-! ### Claim axes over blocks of obligations

`ScaleLayout.checkedBits` asks whether some obligation is total, and whether
some is total with the `mul` role; each obligation's policy is its call
group's termination check. `polOf` is the pair of answers for one obligation;
the package states the pairs of a block of obligations per declaration
(`pols_block`, joined by `pols_cons`), and `checkedBits_of_pols` reads the two
answers from them. -/

def polOf (o : Obligation) : Bool × Bool :=
  (o.policy == .simulatesModelTotally,
    o.policy == .simulatesModelTotally && o.totalityRole == .mul)

/-- `checkedBits` with the policy answers read from `ps`. -/
def checkedPols (artifact : AverCert.AcceptedArtifact.ArtifactData) (rs : List Nat) (ps : List (Bool × Bool)) : Bool :=
  let m := artifact.manifest
  let M := mctxOf m.subject m.types m.fnPlans
  rs.length == m.fnPlans.length &&
    (useOfBits M rs (ps.any (·.1)) (ps.any (·.2))).contracts == m.subject.contracts

theorem pols_cons {α : Type} {l : List α} {f : α → Bool × Bool} {k m : Nat}
    {p1 p2 : List (Bool × Bool)} (h1 : ((l.drop k).take m).map f = p1)
    (h2 : (l.drop (k + m)).map f = p2) : (l.drop k).map f = p1 ++ p2 := by
  rw [← h1, ← h2, ← List.map_append]
  congr 1
  rw [← List.drop_drop]
  exact (List.take_append_drop m (l.drop k)).symm

theorem checkedBits_of_pols {artifact : AverCert.AcceptedArtifact.ArtifactData} {rs : List Nat} {ps : List (Bool × Bool)}
    (hp : (artifact.manifest.obligations.drop 0).map polOf = ps)
    (h : checkedPols artifact rs ps = true) : checkedBits artifact rs = true := by
  rw [List.drop_zero] at hp
  subst hp
  unfold checkedPols at h
  unfold checkedBits
  simp only [List.any_map] at h
  exact h

end AverCert.ScaleTables

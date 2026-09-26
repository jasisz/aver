-- Declared module layout, confirmed once against the decoders.
import AcceptedArtifactCore

namespace AverCert.DeclaredLayout
open AverCert.Schema AverCert.AcceptedArtifact AverCert.TypeTable AverCert.Grammar

/-! ### Packed tables -/

/-- Entry `i` of a table of `w`-bit numbers packed into one numeral, entry 0
    in the lowest bits. The kernel reads it with two GMP operations. -/
def packedAt (t w i : Nat) : Nat := (t >>> (w * i)) % 2 ^ w

theorem packedAt_zero (c t w : Nat) (hc : c < 2 ^ w) : packedAt (c + 2 ^ w * t) w 0 = c := by
  simp only [packedAt, Nat.mul_zero, Nat.shiftRight_zero]
  rw [Nat.add_mul_mod_self_left, Nat.mod_eq_of_lt hc]

theorem packedAt_succ (c t w i : Nat) (hc : c < 2 ^ w) :
    packedAt (c + 2 ^ w * t) w (i + 1) = packedAt t w i := by
  simp only [packedAt, Nat.shiftRight_eq_div_pow]
  rw [Nat.mul_succ, Nat.pow_add, Nat.mul_comm (2 ^ (w * i)) (2 ^ w), ← Nat.div_div_eq_div_mul]
  congr 2
  rw [Nat.add_mul_div_left _ _ (Nat.two_pow_pos w), Nat.div_eq_of_lt hc, Nat.zero_add]


/-! ### The declared function layout

The producer declares, for every defined function `k` (code-section order),
its function-section type index and the byte offset and length of its code
entry (size prefix included), as packed tables. `layoutConfirmed` decodes the
module ONCE and confirms every declared entry by equality against the
decoders; the lemmas below then answer every decoder query about a function
from the declaration, with no further decoding. -/

structure Layout where
  imports : Nat
  count : Nat
  width : Nat
  types : Nat
  offsets : Nat
  lengths : Nat

namespace Layout

def ty (L : Layout) (k : Nat) : Nat := packedAt L.types L.width k
def off (L : Layout) (k : Nat) : Nat := packedAt L.offsets L.width k
def len (L : Layout) (k : Nat) : Nat := packedAt L.lengths L.width k

/-- The declared code entry of defined function `k`, as the exact slice. -/
def entryN (L : Layout) (n k : Nat) : Nat := CertDecode.isolateBytes (n >>> (8 * L.off k)) (L.len k)

def entry (L : Layout) (n k : Nat) : AverCert.WasmSlice.ByteSeq :=
  CertDecode.takeBytes (L.len k) (L.entryN n k)

/-- The code entry of function index `f`, when it is a defined function. -/
def entryAt (L : Layout) (n f : Nat) : Option AverCert.WasmSlice.ByteSeq :=
  if L.imports ≤ f ∧ f - L.imports < L.count then some (L.entry n (f - L.imports)) else none

end Layout

def locsMatch (n : Nat) (L : Layout) : Nat → List Nat → List CertDecode.CodeLoc → Bool
  | _, [], [] => true
  | k, t :: ts, loc :: locs =>
      t == L.ty k && loc.entryLen == L.len k && loc.entryN == L.entryN n k &&
        locsMatch n L (k + 1) ts locs
  | _, _, _ => false

/-- The declared layout is what the decoders read: the imported function
    count, and for every defined function its type index and exact code
    entry. The function and code sections are decoded in full (and must be
    exhausted), so the declaration covers every function and hides nothing. -/
def layoutConfirmed (n len : Nat) (L : Layout) : Bool :=
  match CertDecode.funcImportBase n len, CertDecode.decodeFuncTypes n len,
      CertDecode.codeLocs n len with
  | some nimp, some fts, some locs =>
      nimp == L.imports && fts.length == L.count && locsMatch n L 0 fts locs.toList
  | _, _, _ => false

theorem locsMatch_spec {n : Nat} {L : Layout} :
    ∀ {k : Nat} {ts : List Nat} {locs : List CertDecode.CodeLoc},
      locsMatch n L k ts locs = true →
      ts.length = locs.length ∧ ∀ j, j < ts.length →
        ts[j]? = some (L.ty (k + j)) ∧ ∃ loc, locs[j]? = some loc ∧
          loc.entryLen = L.len (k + j) ∧ loc.entryN = L.entryN n (k + j)
  | _, [], [], _ => ⟨rfl, fun j hj => absurd hj (Nat.not_lt_zero j)⟩
  | _, [], _ :: _, h => by simp [locsMatch] at h
  | _, _ :: _, [], h => by simp [locsMatch] at h
  | k, t :: ts, loc :: locs, h => by
      simp only [locsMatch, Bool.and_eq_true, beq_iff_eq] at h
      obtain ⟨⟨⟨ht, hl⟩, hn⟩, hrest⟩ := h
      obtain ⟨hlen, hget⟩ := locsMatch_spec hrest
      refine ⟨by simp [hlen], fun j hj => ?_⟩
      cases j with
      | zero => exact ⟨by simp [ht], loc, rfl, hl, hn⟩
      | succ j =>
          have := hget j (by simpa using hj)
          simpa [Nat.add_assoc, Nat.add_comm 1 j] using this

theorem codeEntryByFuncIndex_of_layout {n len : Nat} {L : Layout}
    (h : layoutConfirmed n len L = true) (f : Nat) :
    AverCert.WasmSlice.codeEntryByFuncIndex n len f = L.entryAt n f := by
  unfold layoutConfirmed at h
  split at h
  · rename_i nimp fts locs himp hfts hlocs
    simp only [Bool.and_eq_true, beq_iff_eq] at h
    obtain ⟨⟨rfl, hcount⟩, hm⟩ := h
    obtain ⟨hlen, hget⟩ := locsMatch_spec hm
    simp only [AverCert.WasmSlice.codeEntryByFuncIndex, AverCert.WasmSlice.codeIndexByFuncIndex,
      AverCert.WasmSlice.importedFuncCount, himp, AverCert.WasmSlice.codeEntryByCodeIndex, hlocs,
      Layout.entryAt]
    by_cases hlo : L.imports ≤ f
    · simp only [hlo, ↓reduceIte, true_and]
      by_cases hhi : f - L.imports < L.count
      · obtain ⟨_, loc, hloc, hl, hn⟩ := hget (f - L.imports) (by omega)
        simp only [Array.getElem?_toList] at hloc
        simp [hloc, hhi, hl, hn, Layout.entry]
      · have : locs[f - L.imports]? = none := by
          rw [← Array.getElem?_toList]
          apply List.getElem?_eq_none
          simp only [Array.length_toList] at hlen ⊢; omega
        simp [this, hhi]
    · simp [hlo]
  · simp at h

theorem funcBindingByFuncIndex_of_layout {n len : Nat} {L : Layout}
    (h : layoutConfirmed n len L = true) {f : Nat} (hlo : L.imports ≤ f)
    (hhi : f - L.imports < L.count) :
    AverCert.WasmSlice.funcBindingByFuncIndex n len f =
      some ⟨f, L.ty (f - L.imports), L.entry n (f - L.imports)⟩ := by
  have hcode := codeEntryByFuncIndex_of_layout h f
  unfold layoutConfirmed at h
  split at h
  · rename_i nimp fts locs himp hfts hlocs
    simp only [Bool.and_eq_true, beq_iff_eq] at h
    obtain ⟨⟨rfl, hcount⟩, hm⟩ := h
    obtain ⟨hlen, hget⟩ := locsMatch_spec hm
    obtain ⟨hty, _⟩ := hget (f - L.imports) (by omega)
    simp only [AverCert.WasmSlice.codeEntryByFuncIndex, AverCert.WasmSlice.codeIndexByFuncIndex,
      AverCert.WasmSlice.importedFuncCount, himp, hlo, ↓reduceIte, Layout.entryAt, hhi, and_self]
      at hcode
    simp only [AverCert.WasmSlice.funcBindingByFuncIndex, AverCert.WasmSlice.codeIndexByFuncIndex,
      AverCert.WasmSlice.importedFuncCount, himp, hlo, ↓reduceIte,
      AverCert.WasmSlice.typeIndexByCodeIndex, hfts, hty, hcode, Nat.zero_add]
  · simp at h


theorem funcBindingByFuncIndex_of_layout_out {n len : Nat} {L : Layout}
    (h : layoutConfirmed n len L = true) {f : Nat}
    (hout : ¬(L.imports ≤ f ∧ f - L.imports < L.count)) :
    AverCert.WasmSlice.funcBindingByFuncIndex n len f = none := by
  have hcode := codeEntryByFuncIndex_of_layout h f
  simp only [Layout.entryAt, hout, ↓reduceIte] at hcode
  unfold AverCert.WasmSlice.funcBindingByFuncIndex
  unfold AverCert.WasmSlice.codeEntryByFuncIndex at hcode
  split
  · rename_i k hk
    rw [hk] at hcode
    simp only at hcode
    rw [hcode]
    split
    · rename_i hne; cases hne
    · rfl
  · rfl

/-! ### Helper function types over the declared layout -/

/-- `roleTypePinned`, with the helper's type index read from the layout. -/
def roleTypePinnedL (L : Layout) (n len idx : Nat) (params results : List CertDecode.ValType) :
    Bool :=
  if idx < 4294967296 then
    if L.imports ≤ idx ∧ idx - L.imports < L.count then
      AverCert.WasmSlice.typeSectionMatches
        (AverCert.WasmSlice.checkFuncTypeExact params results) n len (L.ty (idx - L.imports))
    else false
  else true

theorem roleTypePinned_of_layout {n len : Nat} {L : Layout} (h : layoutConfirmed n len L = true)
    (idx : Nat) (params results : List CertDecode.ValType) :
    roleTypePinned n len idx params results = roleTypePinnedL L n len idx params results := by
  unfold roleTypePinned roleTypePinnedL
  by_cases hin : L.imports ≤ idx ∧ idx - L.imports < L.count
  · rw [funcBindingByFuncIndex_of_layout h hin.1 hin.2]
    simp [hin]
  · rw [funcBindingByFuncIndex_of_layout_out h hin]
    simp [hin]

/-- `roleTypesPinned` over the layout. -/
def roleTypesPinnedL (L : Layout) (n len : Nat) (M : MCtx) : Bool :=
  let c := refN M.carrier
  roleTypePinnedL L n len M.box [.numeric 0x7e] [c] &&
  roleTypePinnedL L n len M.add [c, c] [c] &&
  roleTypePinnedL L n len M.sub [c, c] [c] &&
  roleTypePinnedL L n len M.mul [c, c] [c] &&
  roleTypePinnedL L n len M.cmp [c, c] [.numeric 0x7f] &&
  roleTypePinnedL L n len M.eq [c, c] [.numeric 0x7f] &&
  roleTypePinnedL L n len M.toIndex [c] [.numeric 0x7f] &&
  roleTypePinnedL L n len M.streq [refN M.str, refN M.str] [.numeric 0x7f] &&
  roleTypePinnedL L n len M.concat [refN M.strVec] [refN M.str] &&
  roleTypePinnedL L n len M.divmod [c, c, .numeric 0x7f] [c]

/-- `plansAcceptedRest` with the helper types read over the layout. -/
def plansAcceptedRestL (artifact : ArtifactData) (L : Layout) : Bool :=
  let m := artifact.manifest
  let M := mctxOf m.subject m.types m.fnPlans
  indicesDistinct M m.fnPlans &&
  typeTableConfirmed artifact.modBytes artifact.modLen m.subject m.types m.fnPlans &&
  dataConfirmed artifact.modBytes artifact.modLen m.subject m.types m.fnPlans &&
  roleTypesPinnedL L artifact.modBytes artifact.modLen M &&
  declsWellFormed m.subject m.types m.fnPlans &&
  consPinned m.types m.fnPlans

/-- `plansAcceptedRestL` from its conjuncts, each proved on its own: one
    kernel check over all of them keeps every intermediate term of every
    conjunct alive until the last one ends. -/
theorem plansAcceptedRestL_of_parts {artifact : ArtifactData} {L : Layout}
    (hidx : indicesDistinct (mctxOf artifact.manifest.subject artifact.manifest.types
      artifact.manifest.fnPlans) artifact.manifest.fnPlans = true)
    (htypes : typeTableConfirmed artifact.modBytes artifact.modLen artifact.manifest.subject
      artifact.manifest.types artifact.manifest.fnPlans = true)
    (hdata : dataConfirmed artifact.modBytes artifact.modLen artifact.manifest.subject
      artifact.manifest.types artifact.manifest.fnPlans = true)
    (hroles : roleTypesPinnedL L artifact.modBytes artifact.modLen (mctxOf artifact.manifest.subject
      artifact.manifest.types artifact.manifest.fnPlans) = true)
    (heqref : eqrefConfined artifact.manifest.types artifact.manifest.fnPlans = true)
    (hnewtypes : newtypesGrounded artifact.manifest.types = true)
    (hinhabited : typesInhabited (mctxOf artifact.manifest.subject artifact.manifest.types
      artifact.manifest.fnPlans) artifact.manifest.types artifact.manifest.fnPlans = true)
    (hcons : consPinned artifact.manifest.types artifact.manifest.fnPlans = true) :
    plansAcceptedRestL artifact L = true := by
  simp only [plansAcceptedRestL, declsWellFormed, hidx, htypes, hdata, hroles, heqref, hnewtypes,
    hinhabited, hcons, Bool.and_self, Bool.and_true, Bool.true_and]

theorem plansAcceptedRest_of_layout {artifact : ArtifactData} {L : Layout}
    (hL : layoutConfirmed artifact.modBytes artifact.modLen L = true)
    (h : plansAcceptedRestL artifact L = true) : plansAcceptedRest artifact = true := by
  unfold plansAcceptedRest
  unfold plansAcceptedRestL at h
  simp only [roleTypesPinned, roleTypePinned_of_layout hL]
  exact h

/-! ### Distinct export names -/

theorem foldl_insert_nodup :
    ∀ (xs : List Nat) (t : Std.TreeSet Nat compare),
      (xs.foldl (fun set value => set.insert value) t).size ≤ t.size + xs.length ∧
      ((xs.foldl (fun set value => set.insert value) t).size = t.size + xs.length →
        xs.Nodup ∧ ∀ x ∈ xs, t.contains x = false)
  | [], t => by simp
  | x :: xs, t => by
      obtain ⟨hle, heq⟩ := foldl_insert_nodup xs (t.insert x)
      have hsz := Std.TreeSet.size_insert (t := t) (k := x)
      simp only [List.foldl_cons, List.length_cons]
      by_cases hx : t.contains x = true
      · simp only [hsz, hx, ↓reduceIte] at hle heq
        refine ⟨by omega, fun h => ?_⟩
        omega
      · simp only [hsz, hx, Bool.false_eq_true, ↓reduceIte] at hle heq
        refine ⟨by omega, fun h => ?_⟩
        obtain ⟨hnd, hout⟩ := heq (by omega)
        refine ⟨List.nodup_cons.mpr ⟨fun hmem => ?_, hnd⟩, ?_⟩
        · have := hout x hmem
          rw [Std.TreeSet.contains_insert] at this
          simp at this
        · intro y hy
          cases hy with
          | head => exact Bool.eq_false_iff.mpr hx
          | tail _ hmem =>
              have := hout y hmem
              rw [Std.TreeSet.contains_insert] at this
              simp only [Bool.or_eq_false_iff] at this
              exact this.2

theorem natListNodup_nodup {xs : List Nat} (h : AverCert.WasmSlice.natListNodup xs = true) :
    xs.Nodup := by
  simp only [AverCert.WasmSlice.natListNodup, AverCert.WasmSlice.indexedNodup,
    AverCert.WasmSlice.orderedSet, beq_iff_eq] at h
  have := (foldl_insert_nodup xs Std.TreeSet.empty).2
  have h0 : (Std.TreeSet.empty : Std.TreeSet Nat compare).size = 0 := by simp
  rw [h0, Nat.zero_add] at this
  exact (this h).1

theorem mapM_cons_some {f : α → Option β} {x : α} {xs : List α} {keys : List β}
    (h : (x :: xs).mapM f = some keys) :
    ∃ k ks, f x = some k ∧ xs.mapM f = some ks ∧ keys = k :: ks := by
  cases hx : f x with
  | none => simp [List.mapM_cons, hx] at h
  | some k =>
      cases hxs : xs.mapM f with
      | none => simp [List.mapM_cons, hx, hxs] at h
      | some ks =>
          simp only [List.mapM_cons, hx, hxs, Option.bind_eq_bind, Option.bind_some,
            Option.pure_def, Option.some.injEq] at h
          exact ⟨k, ks, rfl, rfl, h.symm⟩

theorem mapM_seqKey_mem :
    ∀ {xs : List AverCert.WasmSlice.ByteSeq} {keys : List Nat},
      xs.mapM AverCert.WasmSlice.seqKey = some keys →
      ∀ y ∈ xs, ∃ k ∈ keys, AverCert.WasmSlice.seqKey y = some k
  | [], _, _, _, hy => by cases hy
  | x :: xs, keys, h, y, hy => by
      obtain ⟨k, ks, hk, hks, rfl⟩ := mapM_cons_some h
      cases hy with
      | head => exact ⟨k, List.mem_cons_self, hk⟩
      | tail _ hmem =>
          obtain ⟨k', hk', hy'⟩ := mapM_seqKey_mem hks y hmem
          exact ⟨k', List.mem_cons_of_mem _ hk', hy'⟩

theorem mapM_seqKey_nodup :
    ∀ {xs : List AverCert.WasmSlice.ByteSeq} {keys : List Nat},
      xs.mapM AverCert.WasmSlice.seqKey = some keys → keys.Nodup → xs.Nodup
  | [], _, _, _ => List.nodup_nil
  | x :: xs, keys, h, hnd => by
      obtain ⟨k, ks, hk, hks, rfl⟩ := mapM_cons_some h
      have hnd' := List.nodup_cons.mp hnd
      refine List.nodup_cons.mpr ⟨fun hmem => ?_, mapM_seqKey_nodup hks hnd'.2⟩
      obtain ⟨k', hk'mem, hk'⟩ := mapM_seqKey_mem hks x hmem
      have : k = k' := by rw [hk] at hk'; exact Option.some.inj hk'
      exact hnd'.1 (this ▸ hk'mem)

theorem byteSeqListNodup_nodup {xs : List AverCert.WasmSlice.ByteSeq}
    (h : byteSeqListNodup xs = true) : xs.Nodup := by
  unfold byteSeqListNodup at h
  split at h
  · rename_i keys hkeys
    exact mapM_seqKey_nodup hkeys (natListNodup_nodup h)
  · cases h

/-- Every export name of the module is distinct (decided on numeric name keys). -/
def exportNamesDistinct (n len : Nat) : Bool :=
  match CertDecode.decodeRawExports n len with
  | some entries => byteSeqListNodup (entries.map (·.name))
  | none => false

theorem findExportFuncIndex_of_pos :
    ∀ {E : List AverCert.WasmSlice.ExportEntry} {p : Nat} {nm : AverCert.WasmSlice.ByteSeq}
      {fi : Nat}, (E.map (·.name)).Nodup → E[p]? = some ⟨nm, 0, fi⟩ →
      AverCert.WasmSlice.findExportFuncIndex nm E = some fi
  | [], _, _, _, _, h => by simp at h
  | e :: E, 0, nm, fi, _, h => by
      simp only [List.getElem?_cons_zero, Option.some.injEq] at h
      subst h
      simp [AverCert.WasmSlice.findExportFuncIndex]
  | e :: E, p + 1, nm, fi, hnd, h => by
      simp only [List.getElem?_cons_succ] at h
      simp only [List.map_cons, List.nodup_cons] at hnd
      have hne : e.name ≠ nm := by
        intro heq
        apply hnd.1
        rw [heq]
        exact List.mem_map.mpr ⟨_, List.mem_of_getElem? h, rfl⟩
      simp only [AverCert.WasmSlice.findExportFuncIndex, hne, and_false, ↓reduceIte]
      exact findExportFuncIndex_of_pos hnd.2 h


/-! ### Declared function types -/

/-- A declared function type: its type index, parameters and results. -/
abbrev FnType := Nat × List CertDecode.ValType × List CertDecode.ValType

/-- The declared function types against the decoded type entries, in one
    walk: the declarations are in strictly increasing index order, and each
    is the exact function type of the entry at its index. -/
def typesMatch : Nat → List CertDecode.TypeEntry → List FnType → Bool
  | _, _, [] => true
  | _, [], _ :: _ => false
  | i, e :: es, x :: xs =>
      if i == x.1 then AverCert.WasmSlice.checkFuncTypeExact x.2.1 x.2.2 e && typesMatch (i + 1) es xs
      else if i < x.1 then typesMatch (i + 1) es (x :: xs)
      else false

/-- Every declared function type is the type-section entry at its index. -/
def fnTypesConfirmed (n len : Nat) (fts : List FnType) : Bool :=
  match CertDecode.decodeTypes n len with
  | some info => typesMatch 0 info.entries fts
  | none => false

theorem typesMatch_spec :
    ∀ {i : Nat} {es : List CertDecode.TypeEntry} {fts : List FnType},
      typesMatch i es fts = true → ∀ x ∈ fts, i ≤ x.1 ∧ ∃ e, es[x.1 - i]? = some e ∧
        AverCert.WasmSlice.checkFuncTypeExact x.2.1 x.2.2 e = true
  | _, _, [], _, x, hx => by cases hx
  | _, [], _ :: _, h, _, _ => by simp [typesMatch] at h
  | i, e :: es, y :: ys, h, x, hx => by
      unfold typesMatch at h
      by_cases hi : i = y.1
      · subst hi
        simp only [beq_self_eq_true, ↓reduceIte, Bool.and_eq_true] at h
        obtain ⟨hc, hrest⟩ := h
        cases hx with
        | head => exact ⟨Nat.le_refl _, e, by simp, hc⟩
        | tail _ hmem =>
            obtain ⟨hle, e', he', hc'⟩ := typesMatch_spec hrest x hmem
            refine ⟨by omega, e', ?_, hc'⟩
            have : x.1 - y.1 = (x.1 - (y.1 + 1)) + 1 := by omega
            rw [this, List.getElem?_cons_succ]
            exact he'
      · have hne : (i == y.1) = false := by simpa using hi
        simp only [hne, Bool.false_eq_true, ↓reduceIte] at h
        by_cases hlt : i < y.1
        · simp only [hlt, ↓reduceIte] at h
          obtain ⟨hle, e', he', hc'⟩ := typesMatch_spec h x hx
          refine ⟨by omega, e', ?_, hc'⟩
          have : x.1 - i = (x.1 - (i + 1)) + 1 := by omega
          rw [this, List.getElem?_cons_succ]
          exact he'
        · simp [hlt] at h

theorem typeSectionMatches_of_confirmed {n len : Nat} {fts : List FnType}
    (h : fnTypesConfirmed n len fts = true) {x : FnType} (hx : x ∈ fts) :
    AverCert.WasmSlice.typeSectionMatches (AverCert.WasmSlice.checkFuncTypeExact x.2.1 x.2.2)
      n len x.1 = true := by
  unfold fnTypesConfirmed at h
  split at h
  · rename_i info hinfo
    obtain ⟨_, e, he, hc⟩ := typesMatch_spec h x hx
    have hidx : info.entryIndex = info.entries.toArray := by
      unfold CertDecode.decodeTypes at hinfo
      split at hinfo
      · cases hinfo
      · split at hinfo
        · cases hinfo
        · split at hinfo
          · cases hinfo; rfl
          · cases hinfo
    simp only [AverCert.WasmSlice.typeSectionMatches, hinfo, hidx, List.getElem?_toArray]
    simp only [Nat.sub_zero] at he
    rw [he]
    exact hc
  · cases h

/-! ### One planned function against the declarations -/

/-- What a plan entry declares about its function beyond the plan: its name
    as characters (checked against the entry's name by `rfl`), the position
    of its export entry in the export section, and the position of its
    function type in the declared function types. -/
structure FnDecl where
  name : List Char
  exportPos : Nat
  sigPos : Nat

/-- `entryAccepted` with every module fact read from a confirmed
    declaration: the code entry from the layout (one slice), the type index
    from the layout and its type from the declared function types, and an
    exported function's export entry at its declared position. No section is
    decoded and nothing is searched. -/
def entryFast (n : Nat) (L : Layout) (fts : List FnType)
    (E : List AverCert.WasmSlice.ExportEntry) (M : MCtx) (fns : List FnEntry)
    (e : FnEntry) (d : FnDecl) : Bool :=
  planTyped M e.plan && callsOrdered fns e &&
  decide (L.imports ≤ e.funcIdx) && decide (e.funcIdx - L.imports < L.count) &&
  match codeEntryBytes M e.plan, e.plan.sig.params.mapM (valTyD M), valTyD M e.plan.sig.ret with
  | some bytes, some ps, some r =>
      L.entry n (e.funcIdx - L.imports) == bytes &&
      fts[d.sigPos]? == some (L.ty (e.funcIdx - L.imports), ps, [r]) &&
      (!e.exported || E[d.exportPos]? == some ⟨d.name.map Char.toNat, 0, e.funcIdx⟩)
  | _, _, _ => false

theorem entryAccepted_of_fast {n len : Nat} {L : Layout} {fts : List FnType}
    {E : List AverCert.WasmSlice.ExportEntry} {M : MCtx} {fns : List FnEntry}
    {e : FnEntry} {d : FnDecl}
    (hL : layoutConfirmed n len L = true) (hT : fnTypesConfirmed n len fts = true)
    (hE : CertDecode.decodeRawExports n len = some E) (hX : (E.map (·.name)).Nodup)
    (hname : stringBytes e.name = d.name.map Char.toNat)
    (h : entryFast n L fts E M fns e d = true) : entryAccepted n len M fns e = true := by
  unfold entryFast at h
  simp only [Bool.and_eq_true, decide_eq_true_eq] at h
  obtain ⟨⟨⟨⟨htyped, hcalls⟩, hlo⟩, hhi⟩, hm⟩ := h
  have hbind := funcBindingByFuncIndex_of_layout hL hlo hhi
  split at hm
  · rename_i bytes ps r hbytes hps hr
    simp only [Bool.and_eq_true, beq_iff_eq, Bool.or_eq_true, Bool.not_eq_true'] at hm
    obtain ⟨⟨hcode, hsig⟩, hexp⟩ := hm
    have hmem : (L.ty (e.funcIdx - L.imports), ps, [r]) ∈ fts := List.mem_of_getElem? hsig
    have htype := typeSectionMatches_of_confirmed hT hmem
    simp only [entryAccepted, htyped, hcalls, Bool.true_and, Bool.and_true, boundFunction, hbytes]
    have hpin : sigPinned n len M e.plan.sig (L.ty (e.funcIdx - L.imports)) = true := by
      simp only [sigPinned, hps, hr]
      exact htype
    cases hx : e.exported
    · simp only [Bool.false_eq_true, ↓reduceIte, hbind, Option.filter_some, hcode, BEq.rfl,
        hpin, Bool.and_self]
    · have hpos : E[d.exportPos]? = some ⟨stringBytes e.name, 0, e.funcIdx⟩ := by
        rw [hname]
        rcases hexp with hno | hyes
        · rw [hx] at hno; cases hno
        · exact hyes
      have hfind := findExportFuncIndex_of_pos hX hpos
      simp only [↓reduceIte, AverCert.WasmSlice.exactFuncBindingForExport,
        AverCert.WasmSlice.funcBindingForExport, AverCert.WasmSlice.exportFuncIndex, hE, hfind,
        hbind, Option.filter_some, hcode, decide_true, ↓reduceIte, BEq.rfl, hpin, Bool.and_self]
  · cases hm

/-- The per-entry checks of a run of plan entries, with the export section
    decoded once for the run. -/
def entriesFast (n len : Nat) (L : Layout) (fts : List FnType) (M : MCtx)
    (fns : List FnEntry) (es : List FnEntry) (ds : List FnDecl) : Bool :=
  match CertDecode.decodeRawExports n len with
  | some E => es.length == ds.length && (es.zip ds).all (fun p => entryFast n L fts E M fns p.1 p.2)
  | none => false

theorem entries_of_fast {n len : Nat} {L : Layout} {fts : List FnType} {M : MCtx}
    {fns : List FnEntry} {es : List FnEntry} {ds : List FnDecl}
    (hL : layoutConfirmed n len L = true) (hT : fnTypesConfirmed n len fts = true)
    (hX : exportNamesDistinct n len = true)
    (hnames : es.map (·.name) = ds.map (fun d => String.ofList d.name))
    (h : entriesFast n len L fts M fns es ds = true) :
    es.all (entryAccepted n len M fns) = true := by
  unfold entriesFast at h
  unfold exportNamesDistinct at hX
  split at h
  · rename_i E hE
    rw [hE] at hX
    have hnd := byteSeqListNodup_nodup hX
    simp only [Bool.and_eq_true, beq_iff_eq, List.all_eq_true] at h
    obtain ⟨hlen, hall⟩ := h
    apply List.all_eq_true.mpr
    intro e he
    obtain ⟨i, hi, rfl⟩ := List.getElem_of_mem he
    have hi' : i < ds.length := hlen ▸ hi
    have hz : (es[i], ds[i]) ∈ es.zip ds := by
      rw [List.mem_iff_getElem]
      exact ⟨i, by simp only [List.length_zip]; omega, by simp⟩
    have hname : stringBytes es[i].name = ds[i].name.map Char.toNat := by
      have := congrArg (fun l => l[i]?) hnames
      simp only [List.getElem?_map, List.getElem?_eq_getElem hi, List.getElem?_eq_getElem hi',
        Option.map_some, Option.some.injEq] at this
      rw [this, stringBytes_ofList]
    exact entryAccepted_of_fast hL hT hE hnd hname (hall _ hz)
  · cases h


/-! ### Closure isolation over the declared layout -/

/-- `WasmSlice.closureFold` with the code entries read through `look`. -/
def closureFoldWith (look : Nat → Option AverCert.WasmSlice.ByteSeq) :
    Nat → List Nat → List Nat → Option (List Nat)
  | 0, [], seen => some seen
  | 0, _ :: _, _ => none
  | _fuel + 1, [], seen => some seen
  | fuel + 1, func :: work, seen =>
      if AverCert.WasmSlice.natMem func seen then
        closureFoldWith look fuel work seen
      else
        match (look func).bind AverCert.WasmSlice.scanClosureCodeEntry with
        | some callees => closureFoldWith look fuel (callees ++ work) (func :: seen)
        | none => none

theorem closureFold_eq_with (n len : Nat) :
    ∀ (fuel : Nat) (work seen : List Nat),
      AverCert.WasmSlice.closureFold n len fuel work seen =
        closureFoldWith (AverCert.WasmSlice.codeEntryByFuncIndex n len) fuel work seen
  | 0, [], _ => rfl
  | 0, _ :: _, _ => rfl
  | _ + 1, [], _ => rfl
  | fuel + 1, func :: work, seen => by
      simp only [AverCert.WasmSlice.closureFold, closureFoldWith,
        AverCert.WasmSlice.scanClosureBody]
      split
      · exact closureFold_eq_with n len fuel work seen
      · cases AverCert.WasmSlice.codeEntryByFuncIndex n len func with
        | none => rfl
        | some entry =>
            simp only [Option.bind_some]
            cases AverCert.WasmSlice.scanClosureCodeEntry entry with
            | none => rfl
            | some callees => exact closureFold_eq_with n len fuel _ _

/-- `closureIsolation`, reading the code entries from a confirmed layout. -/
def closureIsolationL (artifact : ArtifactData) (L : Layout) : Bool :=
  let claim := artifact.closureClaim
  let certified := artifact.manifest.obligations.map (fun obligation => obligation.self)
  AverCert.WasmSlice.natListNodup claim.roots &&
  AverCert.WasmSlice.natListNodup claim.helpers &&
  AverCert.WasmSlice.natListNodup claim.admitted &&
  AverCert.WasmSlice.natSetEq claim.roots certified &&
  claim.roots.all (fun root => !AverCert.WasmSlice.natMem root claim.helpers) &&
  AverCert.WasmSlice.natSetEq claim.admitted (claim.roots ++ claim.helpers) &&
  AverCert.WasmSlice.noSharedMemory artifact.modBytes artifact.modLen &&
  match closureFoldWith (L.entryAt artifact.modBytes) artifact.closureFuel claim.roots [] with
  | some actual => AverCert.WasmSlice.natSetEq actual claim.admitted
  | none => false

theorem closureIsolation_of_layout {artifact : ArtifactData} {L : Layout}
    (hL : layoutConfirmed artifact.modBytes artifact.modLen L = true)
    (h : closureIsolationL artifact L = true) : closureIsolation artifact = true := by
  have hlook : AverCert.WasmSlice.codeEntryByFuncIndex artifact.modBytes artifact.modLen =
      L.entryAt artifact.modBytes := funext (codeEntryByFuncIndex_of_layout hL)
  unfold closureIsolation
  simp only [closureFold_eq_with, hlook]
  exact h

/-! ### Helper bodies over the declared layout -/

/-- `bodyBytesAtFuncIndex` read from the layout: the code entry without its
    size prefix. -/
def Layout.bodyAt (L : Layout) (n idx : Nat) : Option (List Nat) :=
  if L.imports ≤ idx ∧ idx - L.imports < L.count then
    match CertDecode.readU (L.entryN n (idx - L.imports)) (L.len (idx - L.imports)) with
    | some (esz, bodyN, _) => some (CertDecode.takeBytes esz bodyN)
    | none => none
  else none

theorem bodyBytesAtFuncIndex_of_layout {n len : Nat} {L : Layout}
    (h : layoutConfirmed n len L = true) (idx : Nat) :
    bodyBytesAtFuncIndex n len idx = L.bodyAt n idx := by
  unfold layoutConfirmed at h
  split at h
  · rename_i nimp fts locs himp hfts hlocs
    simp only [Bool.and_eq_true, beq_iff_eq] at h
    obtain ⟨⟨rfl, hcount⟩, hm⟩ := h
    obtain ⟨hlen, hget⟩ := locsMatch_spec hm
    simp only [bodyBytesAtFuncIndex, himp, hlocs, Layout.bodyAt]
    by_cases hlo : L.imports ≤ idx
    · simp only [hlo, ↓reduceIte, true_and]
      by_cases hhi : idx - L.imports < L.count
      · obtain ⟨_, loc, hloc, hl, hn⟩ := hget (idx - L.imports) (by omega)
        simp only [Array.getElem?_toList] at hloc
        simp only [hloc, hhi, ↓reduceIte, hl, hn, Nat.zero_add]
        rfl
      · have : locs[idx - L.imports]? = none := by
          rw [← Array.getElem?_toList]
          apply List.getElem?_eq_none
          simp only [Array.length_toList] at hlen ⊢; omega
        simp [this, hhi]
    · simp [hlo]
  · simp at h

theorem arithRoleCheck_of_layout {n len : Nat} {L : Layout}
    (h : layoutConfirmed n len L = true) (role : ArithTemplateDerisk.ArithRole)
    (idx? : Option Nat) (p : ArithTemplateDerisk.ArithHostParams) :
    arithRoleCheck n len role idx? p =
      match idx? with
      | none => true
      | some idx => L.bodyAt n idx == some (ArithTemplateDerisk.arithHelperBody role p) := by
  unfold arithRoleCheck
  cases idx? with
  | none => rfl
  | some idx => simp only [bodyBytesAtFuncIndex_of_layout h]


/-! ### String helper roles with a signature-shape index

`StringHost.roleTable` classifies every defined function by the signature of
its type, read from an array by type index; the kernel pays a walk of the
decoded type list for each function. Only a function whose signature has the
shape of an eq or concat helper can be classified, so `roleTableFast` first
folds the signature list into a bitmap of those shapes (one numeral, read
with two GMP operations per function) and reads a signature only for such a
function. `roleTableFast_eq` shows it is `roleTable`. -/

namespace StringFast
open CertDecode.StringHost

/-- A signature of the shape an eq (`[ref, ref] → i32`) or concat
    (`[ref] → ref`) helper has. -/
def candShape (sbat : List Nat) : Option CertDecode.StringHost.Sig → Bool
  | some ([Ty.ref l, Ty.ref r], Ty.i32 :: _) => l == r && sbat.contains l
  | some ([Ty.ref _], Ty.ref b :: _) => sbat.contains b
  | _ => false

/-- Bit `i` is set when signature `i` has a helper shape. -/
def shapeBits (sbat : List Nat) : List (Option CertDecode.StringHost.Sig) → Nat
  | [] => 0
  | s :: ss => (if candShape sbat s then 1 else 0) + 2 * shapeBits sbat ss

theorem shapeBits_at (sbat : List Nat) : ∀ (ts : List (Option CertDecode.StringHost.Sig)) (i : Nat),
    packedAt (shapeBits sbat ts) 1 i = if candShape sbat ((ts[i]?).getD none) then 1 else 0
  | [], i => by simp [shapeBits, packedAt, candShape]
  | s :: ss, 0 => by
      have h := packedAt_zero (if candShape sbat s then 1 else 0) (shapeBits sbat ss) 1 (by split <;> decide)
      simpa [shapeBits] using h
  | s :: ss, i + 1 => by
      have h := packedAt_succ (if candShape sbat s then 1 else 0) (shapeBits sbat ss) 1 i (by split <;> decide)
      simp only [Nat.pow_one] at h
      simp only [shapeBits, h, List.getElem?_cons_succ]
      exact shapeBits_at sbat ss i

/-- `classify` over a signature lookup function. -/
def classifyBy (nimp : Nat) (sbat : List Nat) (look : Nat → Option CertDecode.StringHost.Sig) :
    Nat → List Nat → List (Nat × Nat × Nat) → List (Nat × Role)
  | _,       [],        _  => []
  | _,       _ :: _,    [] => []
  | def_idx, ty :: tys, (nloc, bodyN, bodyLen) :: locs =>
      match classifyOne sbat (look ty) nloc bodyN bodyLen with
      | some role => (nimp + def_idx, role) :: classifyBy nimp sbat look (def_idx+1) tys locs
      | none => classifyBy nimp sbat look (def_idx+1) tys locs

theorem classify_eq_by (nimp : Nat) (sbat : List Nat) (tsigs : Array (Option CertDecode.StringHost.Sig)) :
    ∀ (d : Nat) (tys : List Nat) (locs : List (Nat × Nat × Nat)),
      classify nimp sbat tsigs d tys locs =
        classifyBy nimp sbat (fun ty => (tsigs[ty]?).getD none) d tys locs
  | _, [], _ => by simp [classify, classifyBy]
  | _, _ :: _, [] => by simp [classify, classifyBy]
  | d, ty :: tys, (nloc, bodyN, bodyLen) :: locs => by
      simp only [classify, classifyBy]
      rw [classify_eq_by nimp sbat tsigs (d + 1) tys locs]
      rfl

theorem classifyBy_congr (nimp : Nat) (sbat : List Nat) (f g : Nat → Option CertDecode.StringHost.Sig)
    (h : ∀ ty nloc bodyN bodyLen,
      classifyOne sbat (f ty) nloc bodyN bodyLen = classifyOne sbat (g ty) nloc bodyN bodyLen) :
    ∀ (d : Nat) (tys : List Nat) (locs : List (Nat × Nat × Nat)),
      classifyBy nimp sbat f d tys locs = classifyBy nimp sbat g d tys locs
  | _, [], _ => by simp [classifyBy]
  | _, _ :: _, [] => by simp [classifyBy]
  | d, ty :: tys, (nloc, bodyN, bodyLen) :: locs => by
      simp only [classifyBy, h ty nloc bodyN bodyLen,
        classifyBy_congr nimp sbat f g h (d + 1) tys locs]

theorem eqCandidate_some {s : Option CertDecode.StringHost.Sig} {nloc lhs : Nat}
    (h : eqCandidate s nloc = some lhs) :
    ∃ rs, s = some ([Ty.ref lhs, Ty.ref lhs], Ty.i32 :: rs) := by
  unfold eqCandidate at h
  split at h
  · split at h
    · rename_i l r rs
      by_cases hc : (l == r && nloc == 2) = true
      · simp only [hc, ↓reduceIte] at h
        simp only [Bool.and_eq_true, beq_iff_eq] at hc
        obtain ⟨rfl, _⟩ := hc
        cases h
        exact ⟨rs, rfl⟩
      · simp only [hc, Bool.false_eq_true, ↓reduceIte, reduceCtorEq] at h
    · cases h
  · cases h

theorem concatCandidate_some {s : Option CertDecode.StringHost.Sig} {nloc c b : Nat}
    (h : concatCandidate s nloc = some (c, b)) :
    ∃ rs, s = some ([Ty.ref c], Ty.ref b :: rs) := by
  unfold concatCandidate at h
  split at h
  · split at h
    · rename_i c' b' rs
      by_cases hc : (nloc == 7) = true
      · simp only [hc, ↓reduceIte] at h
        cases h
        exact ⟨rs, rfl⟩
      · simp only [hc, Bool.false_eq_true, ↓reduceIte, reduceCtorEq] at h
    · cases h
  · cases h

theorem classifyOne_of_not_shape (sbat : List Nat) (s : Option CertDecode.StringHost.Sig)
    (hs : candShape sbat s = false) (nloc bodyN bodyLen : Nat) :
    classifyOne sbat s nloc bodyN bodyLen = none := by
  unfold classifyOne
  split
  · rename_i lhs he
    obtain ⟨rs, rfl⟩ := eqCandidate_some he
    have : lhs ∉ sbat := by simpa [candShape] using hs
    simp [this]
  · split
    · rename_i c b hc
      obtain ⟨rs, rfl⟩ := concatCandidate_some hc
      have : b ∉ sbat := by simpa [candShape] using hs
      simp [this]
    · rfl

/-- The signature lookup through the shape bitmap. -/
def sigLook (sbat : List Nat) (ts : List (Option CertDecode.StringHost.Sig)) (ty : Nat) : Option CertDecode.StringHost.Sig :=
  if packedAt (shapeBits sbat ts) 1 ty == 1 then (ts[ty]?).getD none else none

theorem classifyOne_sigLook (sbat : List Nat) (ts : List (Option CertDecode.StringHost.Sig)) (ty nloc bodyN bodyLen : Nat) :
    classifyOne sbat (sigLook sbat ts ty) nloc bodyN bodyLen =
      classifyOne sbat ((ts[ty]?).getD none) nloc bodyN bodyLen := by
  unfold sigLook
  rw [shapeBits_at]
  by_cases hs : candShape sbat ((ts[ty]?).getD none) = true
  · simp [hs]
  · have hs' : candShape sbat ((ts[ty]?).getD none) = false := by simpa using hs
    have h0 : ((if candShape sbat ((ts[ty]?).getD none) = true then 1 else 0) == 1) = false := by
      simp [hs']
    simp only [h0, Bool.false_eq_true, ↓reduceIte]
    rw [classifyOne_of_not_shape sbat _ hs', classifyOne_of_not_shape sbat none rfl]

/-- `roleTable`, reading signatures through the shape bitmap. -/
def roleTableFast (n len : Nat) : Option (List (Nat × Role)) :=
  match decodeTypeSigs n len, CertDecode.decodeFuncTypes n len,
        CertDecode.funcImportBase n len, bodyLocs n len with
  | some (tsigs, sbat), some ftys, some nimp, some locs =>
      some (classifyBy nimp sbat (sigLook sbat tsigs.toList) 0 ftys locs)
  | _, _, _, _ => none

theorem roleTableFast_eq (n len : Nat) : roleTableFast n len = roleTable n len := by
  unfold roleTableFast roleTable
  cases decodeTypeSigs n len with
  | none => rfl
  | some p =>
      obtain ⟨tsigs, sbat⟩ := p
      cases CertDecode.decodeFuncTypes n len with
      | none => rfl
      | some ftys =>
          cases CertDecode.funcImportBase n len with
          | none => rfl
          | some nimp =>
              cases bodyLocs n len with
              | none => rfl
              | some locs =>
                  show some _ = some _
                  rw [classify_eq_by]
                  congr 1
                  apply classifyBy_congr
                  intro ty nloc bodyN bodyLen
                  rw [classifyOne_sigLook, Array.getElem?_toList]

end StringFast


/-! ### Names as characters

The kernel has no fast path for String values: converting one to its bytes
or comparing two rebuilds their UTF-8 arrays, in time quadratic in their
length. A package states the Strings a check reads as character lists
(a literal is definitionally `String.ofList` of its characters, which the
kernel checks by `rfl` without building bytes), and the lemmas below turn
each check into the same check over the characters' code points. -/

namespace Chars

theorem map_toNat_inj : ∀ {a b : List Char}, a.map Char.toNat = b.map Char.toNat → a = b
  | [], [], _ => rfl
  | [], _ :: _, h => by simp at h
  | _ :: _, [], h => by simp at h
  | x :: xs, y :: ys, h => by
      simp only [List.map_cons, List.cons.injEq] at h
      rw [Char.toNat_inj.mp h.1, map_toNat_inj h.2]

theorem stringBytes_inj {s t : String} (h : stringBytes s = stringBytes t) : s = t := by
  have : s.toList = t.toList := map_toNat_inj h
  simpa using congrArg String.ofList this

theorem stringBytes_append (s t : String) : stringBytes (s ++ t) = stringBytes s ++ stringBytes t := by
  simp [stringBytes, String.toList_append]

/-- Membership of a String pair, decided on the pairs' bytes. -/
theorem contains_pair_bytes (reg : List (String × String)) (s t : String) :
    reg.contains (s, t) = (reg.map capabilityBytes).contains (stringBytes s, stringBytes t) := by
  apply Bool.eq_iff_iff.mpr
  simp only [List.contains_iff_mem, List.mem_map, capabilityBytes, Prod.mk.injEq]
  constructor
  · intro h; exact ⟨(s, t), h, rfl, rfl⟩
  · rintro ⟨⟨a, b⟩, hab, ha, hb⟩
    simp only at ha hb
    rw [stringBytes_inj ha, stringBytes_inj hb] at hab
    exact hab

/-- `customCapabilityImport` over the capability's bytes. -/
def customCapabilityImportBytes (moduleBytes operationBytes : AverCert.WasmSlice.ByteSeq) : Bool :=
  let modulePrefix := stringBytes "aver:user/cap-n"
  let operationPrefix := stringBytes "op-n"
  let operationTail := operationBytes.drop operationPrefix.length
  modulePrefix.isPrefixOf moduleBytes &&
  customCapabilityModuleTail 0 (moduleBytes.drop modulePrefix.length) &&
  operationPrefix.isPrefixOf operationBytes &&
  !operationTail.isEmpty && operationTail.length % 2 == 0 &&
  operationTail.all lowerHexByte

theorem customCapabilityImport_bytes (c : String × String) :
    customCapabilityImport c = customCapabilityImportBytes (stringBytes c.1) (stringBytes c.2) :=
  rfl

/-- `importsWithinCapabilities` with the declared capabilities as characters. -/
def importsWithinCapabilitiesChars (artifact : ArtifactData)
    (caps : List (List Char × List Char)) : Bool :=
  let bytes := caps.map (fun c => (c.1.map Char.toNat, c.2.map Char.toNat))
  byteSeqListNodup (bytes.map (fun c => c.1 ++ [46] ++ c.2)) &&
  bytes.all (fun c =>
    ((AverCert.Schema.capabilityRegistryForTarget artifact.manifest.subject.target).map
        capabilityBytes).contains c ||
      customCapabilityImportBytes c.1 c.2) &&
  match AverCert.WasmSlice.enumImportNames artifact.modBytes artifact.modLen with
  | some actual => actual == bytes
  | none => false

theorem importsWithinCapabilities_of_chars (artifact : ArtifactData)
    (caps : List (List Char × List Char))
    (hcaps : artifact.manifest.subject.capabilities =
      caps.map (fun c => (String.ofList c.1, String.ofList c.2)))
    (h : importsWithinCapabilitiesChars artifact caps = true) :
    importsWithinCapabilities artifact = true := by
  unfold importsWithinCapabilitiesChars at h
  unfold importsWithinCapabilities
  rw [hcaps]
  have hdot : stringBytes "." = [46] := by decide
  simp only [List.map_map, Function.comp_def, stringListNodup, stringBytes_append,
    stringBytes_ofList, hdot, List.all_map, customCapabilityImport_bytes, contains_pair_bytes,
    capabilityBytes] at h ⊢
  exact h

/-- A code point list names `s` exactly when its characters do, for a `s`
    without the character 0 (every other code point `Char.ofNat` maps to
    itself or to 0). -/
theorem mkName_beq (ns : List Nat) (s : String) (hs : ∀ c ∈ s.toList, c.toNat ≠ 0) :
    (CertDecode.mkName ns == s) = (ns == stringBytes s) := by
  apply Bool.eq_iff_iff.mpr
  simp only [beq_iff_eq, CertDecode.mkName, stringBytes]
  constructor
  · intro h
    have hl : ns.map Char.ofNat = s.toList := by rw [← h]; simp
    rw [← hl, List.map_map]
    calc ns = ns.map id := (List.map_id ns).symm
      _ = ns.map (Char.toNat ∘ Char.ofNat) := List.map_congr_left (fun n hn => ?_)
    have hc : Char.ofNat n ∈ s.toList := hl ▸ List.mem_map_of_mem hn
    have hz := hs _ hc
    simp only [id, Function.comp_apply]
    by_cases hv : n.isValidChar
    · simp [Char.ofNat, hv, Char.ofNatAux]
    · simp [Char.ofNat, hv] at hz
  · intro h
    rw [h, List.map_map]
    have : (Char.ofNat ∘ Char.toNat) = id := by funext c; simp
    simp [this]

/-- The first function export named `s`, read on the raw export entries. -/
theorem functionExports_find (s : String) (hs : ∀ c ∈ s.toList, c.toNat ≠ 0) :
    ∀ raw : List CertDecode.ExportEntry,
      ((CertDecode.functionExports raw).find? (fun e => e.1 == s)).map Prod.snd =
        AverCert.WasmSlice.findExportFuncIndex (stringBytes s) raw
  | [] => rfl
  | e :: raw => by
      simp only [CertDecode.functionExports, AverCert.WasmSlice.findExportFuncIndex]
      by_cases hk : e.kind = 0
      · simp only [hk, beq_self_eq_true, ↓reduceIte, List.find?_cons, mkName_beq _ _ hs, true_and]
        by_cases hn : e.name = stringBytes s
        · simp [hn]
        · have hb : (e.name == stringBytes s) = false := by simpa using hn
          simp only [hb, hn, ↓reduceIte]
          exact functionExports_find s hs raw
      · have : (e.kind == 0) = false := by simpa using hk
        simp only [this, Bool.false_eq_true, ↓reduceIte, hk, false_and]
        exact functionExports_find s hs raw

theorem functionExports_all (s : String) (hs : ∀ c ∈ s.toList, c.toNat ≠ 0) :
    ∀ raw : List CertDecode.ExportEntry,
      (CertDecode.functionExports raw).all (fun e => e.1 != s) =
        (AverCert.WasmSlice.findExportFuncIndex (stringBytes s) raw).isNone
  | [] => rfl
  | e :: raw => by
      simp only [CertDecode.functionExports, AverCert.WasmSlice.findExportFuncIndex]
      by_cases hk : e.kind = 0
      · simp only [hk, beq_self_eq_true, ↓reduceIte, List.all_cons, bne, mkName_beq _ _ hs, true_and]
        by_cases hn : e.name = stringBytes s
        · simp [hn]
        · have hb : (e.name == stringBytes s) = false := by simpa using hn
          simp only [hb, hn, ↓reduceIte, Bool.not_false, Bool.true_and]
          exact functionExports_all s hs raw
      · have : (e.kind == 0) = false := by simpa using hk
        simp only [this, Bool.false_eq_true, ↓reduceIte, hk, false_and]
        exact functionExports_all s hs raw

/-- The export index of a helper's name, on the raw export entries. -/
def helperIdx (n len : Nat) (name : AverCert.WasmSlice.ByteSeq) : Option Nat :=
  (CertDecode.decodeRawExports n len).bind (AverCert.WasmSlice.findExportFuncIndex name)

theorem boxIdx_eq (n len : Nat) :
    CertDecode.AddSub.boxIdx n len = helperIdx n len (stringBytes "__rt_aint_from_i64") := by
  unfold CertDecode.AddSub.boxIdx helperIdx CertDecode.decodeExports
  cases CertDecode.decodeRawExports n len with
  | none => rfl
  | some raw => exact functionExports_find _ (by decide) raw

theorem toIndexIdx_eq (n len : Nat) :
    CertDecode.AddSub.toIndexIdx n len = helperIdx n len (stringBytes "__aint_to_index") := by
  unfold CertDecode.AddSub.toIndexIdx helperIdx CertDecode.decodeExports
  cases CertDecode.decodeRawExports n len with
  | none => rfl
  | some raw => exact functionExports_find _ (by decide) raw

theorem cmpIdx_eq (n len : Nat) :
    CertDecode.AddSub.cmpIdx n len = helperIdx n len (stringBytes "__aint_cmp") := by
  unfold CertDecode.AddSub.cmpIdx helperIdx CertDecode.decodeExports
  cases CertDecode.decodeRawExports n len with
  | none => rfl
  | some raw => exact functionExports_find _ (by decide) raw

theorem carrierHelperAbsent_eq (n len : Nat) :
    CertDecode.AddSub.carrierHelperAbsent n len =
      match CertDecode.decodeRawExports n len with
      | some raw =>
          (AverCert.WasmSlice.findExportFuncIndex (stringBytes "__rt_aint_from_i64") raw).isNone
      | none => false := by
  unfold CertDecode.AddSub.carrierHelperAbsent CertDecode.decodeExports
  cases CertDecode.decodeRawExports n len with
  | none => rfl
  | some raw => exact functionExports_all _ (by decide) raw

end Chars

end AverCert.DeclaredLayout

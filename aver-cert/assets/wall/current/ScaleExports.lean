-- The export section in blocks, and the export accounting read from them.
import ScaleLayout
import SortedKeys

set_option linter.unusedSimpArgs false

namespace AverCert.ScaleExports
open CertDecode AverCert.ByteWindow AverCert.ScaleBytes AverCert.ScaleLayout AverCert.DeclaredLayout
open AverCert.Schema AverCert.AcceptedArtifact

/-! ### Names read as ASCII bytes

The accounting compares a manifest name through `stringBytes`, its code
points. The kernel reads a String literal's code points by decoding its UTF-8
bytes back into characters, about 10 ms a name. Its bytes it reads directly:
a literal is `String.ofList` of its characters, whose bytes are their UTF-8
encoding. When every byte is below 128, every character is one byte and the
bytes are the code points (`stringBytes_of_ascii`). A name with any other byte
has no reading here, and every check that reads it fails. -/

/-- A String's UTF-8 bytes, when all of them are ASCII. -/
def asciiBytes (s : String) : Option (List Nat) :=
  let bs := s.toByteArray.data.toList.map UInt8.toNat
  if bs.all (· < 128) then some bs else none

theorem utf8EncodeChar_ascii (c : Char)
    (h : ((String.utf8EncodeChar c).map UInt8.toNat).all (· < 128) = true) :
    (String.utf8EncodeChar c).map UInt8.toNat = [c.toNat] := by
  have hv : c.val.toNat = c.toNat := rfl
  unfold String.utf8EncodeChar at h ⊢
  simp only [hv] at h ⊢
  by_cases h1 : c.toNat ≤ 127
  · simp only [h1, ite_true, List.map_cons, List.map_nil, UInt8.toNat_ofNat', List.cons.injEq,
      and_true]
    omega
  · exfalso
    simp only [h1, ite_false] at h
    by_cases h2 : c.toNat ≤ 0x7ff
    · simp only [h2, ite_true, List.map_cons, List.map_nil, UInt8.toNat_ofNat', List.all_cons,
        Bool.and_eq_true, decide_eq_true_eq] at h
      omega
    · simp only [h2, ite_false] at h
      by_cases h3 : c.toNat ≤ 0xffff
      · simp only [h3, ite_true, List.map_cons, List.map_nil, UInt8.toNat_ofNat', List.all_cons,
          Bool.and_eq_true, decide_eq_true_eq] at h
        omega
      · simp only [h3, ite_false, List.map_cons, List.map_nil, UInt8.toNat_ofNat', List.all_cons,
          Bool.and_eq_true, decide_eq_true_eq] at h
        omega

theorem flatMap_ascii : ∀ (cs : List Char),
    ((cs.flatMap String.utf8EncodeChar).map UInt8.toNat).all (· < 128) = true →
    (cs.flatMap String.utf8EncodeChar).map UInt8.toNat = cs.map Char.toNat
  | [], _ => rfl
  | c :: cs, h => by
      simp only [List.flatMap_cons, List.map_append, List.all_append, Bool.and_eq_true] at h
      simp only [List.flatMap_cons, List.map_append, List.map_cons]
      rw [utf8EncodeChar_ascii c h.1, flatMap_ascii cs h.2]
      rfl

/-- A name's ASCII bytes are its code points. -/
theorem stringBytes_of_ascii {s : String} {bs : List Nat} (h : asciiBytes s = some bs) :
    stringBytes s = bs := by
  unfold asciiBytes at h
  have hs : s.toByteArray.data.toList = s.toList.flatMap String.utf8EncodeChar := by
    conv => lhs; rw [← String.ofList_toList (s := s)]
    rw [String.toByteArray_ofList]
    unfold List.utf8Encode
    exact List.toList_data_toByteArray
  rw [hs] at h
  simp only at h
  split at h
  · rename_i hall
    simp only [Option.some.injEq] at h
    subst h
    exact (flatMap_ascii _ hall).symm
  · cases h

/-! ### The export walk

The emitter writes the export section sorted by the wall's name key
(`WasmSlice.seqKey`), which export order is free to be, and the package lists
the declared-uncertified exports in the same order. `walkExports` reads the
section one entry at a time from a declared offset, each entry on its own
chunk window, and requires every name's key above the previous one. An entry
that is a function export of a claimed function (`claimedEntry`: kind 0, its
index a bit of `claimed`) is accounted for by that index alone; every other
entry must be the next declared-uncertified name, read as ASCII bytes. The
walk takes the declared list in pieces, one per block of entries, so a block
of the section is one declaration (`walk_append`). -/

/-- The name `b` has a key above the key of the previous name `p`. -/
def keyAfter : Option (List Nat) → List Nat → Bool
  | none, b => (AverCert.WasmSlice.seqKey b).isSome
  | some a, b =>
      match AverCert.WasmSlice.seqKey a, AverCert.WasmSlice.seqKey b with
      | some x, some y => decide (x < y)
      | _, _ => false

/-- A function export of a claimed function: kind 0, with its function index
    a bit of `claimed` (the claimed indices as `SortedKeys.bitsOf`). -/
def claimedEntry (claimed : Nat) (e : AverCert.WasmSlice.ExportEntry) : Bool :=
  e.kind == 0 && claimed.testBit e.idx

/-- The export entries of the given lengths from `off`, after the name `p`,
    against the declared names `ds`: the last name and the offset after the
    last entry. -/
def walkExports (cs : List Nat) (claimed : Nat) :
    Option (List Nat) → Nat → List Nat → List (String × String) →
      Option (Option (List Nat) × Nat)
  | p, off, [], [] => some (p, off)
  | _, _, [], _ :: _ => none
  | p, off, l :: ls, ds =>
      match whole readExportEntry (window 1024 cs off l, l) with
      | none => none
      | some e =>
          if keyAfter p e.name then
            if claimedEntry claimed e then walkExports cs claimed (some e.name) (off + l) ls ds
            else
              match ds with
              | d :: ds' =>
                  if asciiBytes d.1 == some e.name then
                    walkExports cs claimed (some e.name) (off + l) ls ds'
                  else none
              | [] => none
          else none

/-- One block of entries that uses up its piece of the declared names, then
    the rest. -/
theorem walk_append {cs : List Nat} {claimed : Nat} :
    ∀ {l1 : List Nat} {p : Option (List Nat)} {off : Nat} {d1 : List (String × String)}
      {p' : Option (List Nat)} {off' : Nat} (l2 : List Nat) (d2 : List (String × String)),
      walkExports cs claimed p off l1 d1 = some (p', off') →
      walkExports cs claimed p off (l1 ++ l2) (d1 ++ d2) = walkExports cs claimed p' off' l2 d2
  | [], p, off, [], p', off', l2, d2, h => by
      simp only [walkExports, Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl⟩ := h
      rfl
  | [], _, _, _ :: _, _, _, _, _, h => by simp [walkExports] at h
  | l :: ls, p, off, d1, p', off', l2, d2, h => by
      rw [List.cons_append, walkExports.eq_3]
      rw [walkExports.eq_3] at h
      cases he : whole readExportEntry (window 1024 cs off l, l) with
      | none => simp [he] at h
      | some e =>
          simp only [he] at h ⊢
          cases hk : keyAfter p e.name with
          | false => simp [hk] at h
          | true =>
              simp only [hk, ite_true] at h ⊢
              cases hb : claimedEntry claimed e with
              | true =>
                  simp only [hb, ite_true] at h ⊢
                  exact walk_append l2 d2 h
              | false =>
                  simp only [hb, Bool.false_eq_true, ite_false] at h ⊢
                  cases d1 with
                  | nil => simp at h
                  | cons d ds' =>
                      simp only [List.cons_append] at h ⊢
                      cases hd : (asciiBytes d.1 == some e.name) with
                      | false => simp [hd] at h
                      | true =>
                          simp only [hd, ite_true] at h ⊢
                          exact walk_append l2 d2 h

/-- A block of entries that uses up its piece of the declared names, and a
    walk over the rest. -/
theorem walk_cons {cs : List Nat} {claimed : Nat} {l1 l2 : List Nat}
    {d1 d2 : List (String × String)} {p p' : Option (List Nat)} {off off' : Nat}
    {r : Option (Option (List Nat) × Nat)}
    (h1 : walkExports cs claimed p off l1 d1 = some (p', off'))
    (h2 : walkExports cs claimed p' off' l2 d2 = r) :
    walkExports cs claimed p off (l1 ++ l2) (d1 ++ d2) = r :=
  (walk_append l2 d2 h1).trans h2

/-! ### What the walk reads -/

/-- The windows of consecutive entries of the given lengths from `off`. -/
def winsAt (cs : List Nat) : Nat → List Nat → List (Nat × Nat)
  | _, [] => []
  | off, l :: ls => (window 1024 cs off l, l) :: winsAt cs (off + l) ls

/-- Every name's key above the one before, the first above `p`'s. -/
def chainAfter : Option (List Nat) → List (List Nat) → Bool
  | _, [] => true
  | p, n :: ns => keyAfter p n && chainAfter (some n) ns

theorem winsAt_length (cs : List Nat) : ∀ (off : Nat) (ls : List Nat),
    (winsAt cs off ls).length = ls.length
  | _, [] => rfl
  | off, _ :: ls => by simp [winsAt, winsAt_length cs _ ls]

theorem whole_export_zero (w : Nat) : whole readExportEntry (w, 0) = none := by
  simp [whole, readExportEntry, readName, readU, uleb]

theorem walk_spec {cs : List Nat} {claimed : Nat} :
    ∀ (ls : List Nat) (p : Option (List Nat)) (off : Nat) (ds : List (String × String))
      (p' : Option (List Nat)) (off' : Nat),
      walkExports cs claimed p off ls ds = some (p', off') →
      ∃ E : List AverCert.WasmSlice.ExportEntry,
        (winsAt cs off ls).mapM (whole readExportEntry) = some E ∧
        chainAfter p (E.map (·.name)) = true ∧
        ds.map (fun d => asciiBytes d.1) =
          (E.filter (fun e => !claimedEntry claimed e)).map (fun e => some e.name)
  | [], p, off, [], p', off', _ => ⟨[], rfl, rfl, rfl⟩
  | [], _, _, _ :: _, _, _, h => by simp [walkExports] at h
  | l :: ls, p, off, ds, p', off', h => by
      simp only [walkExports] at h
      split at h
      · cases h
      · rename_i e he
        split at h
        · rename_i hk
          split at h
          · rename_i hb
            obtain ⟨E, hE, hc, hd⟩ := walk_spec ls (some e.name) (off + l) ds p' off' h
            refine ⟨e :: E, ?_, ?_, ?_⟩
            · simp only [winsAt, List.mapM_cons, he, hE, Option.bind_eq_bind, Option.bind_some,
                Option.pure_def]
            · simp only [List.map_cons, chainAfter, hk, hc, Bool.and_self]
            · simp only [List.filter_cons, hb, Bool.not_true, Bool.false_eq_true, ↓reduceIte]
              exact hd
          · rename_i hb
            split at h
            · rename_i d ds'
              split at h
              · rename_i hdm
                obtain ⟨E, hE, hc, hd⟩ := walk_spec ls (some e.name) (off + l) ds' p' off' h
                refine ⟨e :: E, ?_, ?_, ?_⟩
                · simp only [winsAt, List.mapM_cons, he, hE, Option.bind_eq_bind,
                    Option.bind_some, Option.pure_def]
                · simp only [List.map_cons, chainAfter, hk, hc, Bool.and_self]
                · simp only [beq_iff_eq] at hdm
                  simp only [List.filter_cons, hb, Bool.not_false, ↓reduceIte, List.map_cons,
                    hdm, hd]
              · cases h
            · cases h
        · cases h

/-- The windows of a walk, inside a slice that holds them, are the section's
    cut windows. -/
theorem winsAt_eq {cs : List Nat} {n : Nat} (hn : join 1024 cs = n)
    (hfit : chunksFit 1024 cs = true) :
    ∀ (ls : List Nat) (off S : Nat), ls.sum ≤ S →
      winsAt cs off ls = seqWin (slice n off S) ls
  | [], _, _, _ => rfl
  | l :: ls, off, S, h => by
      simp only [List.sum_cons] at h
      simp only [winsAt, seqWin, List.cons.injEq]
      refine ⟨?_, ?_⟩
      · rw [window_eq (by decide) hfit, hn, slice_mod n off l S (by omega)]
      · rw [slice_shr n off S l (by omega)]
        exact winsAt_eq hn hfit ls (off + l) (S - l) (by omega)

/-- A walk over the whole export section, from the first entry the head
    names, decodes the section: every declared length is an entry. -/
theorem exportsCut_of_walk {cs : List Nat} {n len : Nat} {hs : List Nat} {e0 : Nat}
    {ls : List Nat} {E : List AverCert.WasmSlice.ExportEntry}
    (hn : join 1024 cs = n) (hfit : chunksFit 1024 cs = true)
    (hH : exportsHead cs len hs e0 ls = true)
    (hE : (winsAt cs e0 ls).mapM (whole readExportEntry) = some E) :
    decodeRawExportsCut n len ls = some E := by
  obtain ⟨start, size, k, hpay, hread, hsum⟩ := exportsHead_spec hn hfit hH
  unfold decodeRawExportsCut
  rw [hpay]
  simp only [hread, BEq.rfl, hsum, Bool.and_self, ↓reduceIte]
  rw [cutWin_eq, ← winsAt_eq hn hfit ls e0 (size - k) (by omega)]
  exact hE

/-! ### Keys in order -/

theorem keyAfter_some {p : Option (List Nat)} {b : List Nat} (h : keyAfter p b = true) :
    ∃ k, AverCert.WasmSlice.seqKey b = some k ∧
      ∀ a, p = some a → ∃ x, AverCert.WasmSlice.seqKey a = some x ∧ x < k := by
  cases p with
  | none =>
      simp only [keyAfter, Option.isSome_iff_exists] at h
      obtain ⟨k, hk⟩ := h
      exact ⟨k, hk, fun a ha => by cases ha⟩
  | some a =>
      simp only [keyAfter] at h
      split at h
      · rename_i x y hx hy
        refine ⟨y, hy, fun a' ha' => ?_⟩
        cases ha'
        exact ⟨x, hx, by simpa using h⟩
      · cases h

/-- A chain's names all have keys, in increasing order, each above `p`'s. -/
theorem chain_keys : ∀ {p : Option (List Nat)} {ns : List (List Nat)},
    chainAfter p ns = true →
    ∃ ks : List Nat, ns.mapM AverCert.WasmSlice.seqKey = some ks ∧
      AverCert.SortedKeys.strictly ks = true ∧
      ∀ a, p = some a → ∀ k ∈ ks, ∃ x, AverCert.WasmSlice.seqKey a = some x ∧ x < k
  | _, [], _ => ⟨[], rfl, rfl, fun _ _ k hk => by cases hk⟩
  | p, n :: ns, h => by
      simp only [chainAfter, Bool.and_eq_true] at h
      obtain ⟨k, hk, hlt⟩ := keyAfter_some h.1
      obtain ⟨ks, hks, hs, hbelow⟩ := chain_keys h.2
      refine ⟨k :: ks, ?_, ?_, ?_⟩
      · simp only [List.mapM_cons, hk, hks, Option.bind_eq_bind, Option.bind_some, Option.pure_def]
      · cases ks with
        | nil => rfl
        | cons k' ks' =>
            simp only [AverCert.SortedKeys.strictly, Bool.and_eq_true, decide_eq_true_eq]
            obtain ⟨x, hx, hxk⟩ := hbelow n rfl k' List.mem_cons_self
            rw [hk] at hx
            cases hx
            exact ⟨hxk, hs⟩
      · intro a ha k' hk'
        obtain ⟨x, hx, hxk⟩ := hlt a ha
        refine ⟨x, hx, ?_⟩
        cases hk' with
        | head => exact hxk
        | tail _ hk' =>
            obtain ⟨y, hy, hyk⟩ := hbelow n rfl k' hk'
            rw [hk] at hy
            cases hy
            exact Nat.lt_trans hxk hyk

/-- A chain's names are distinct. -/
theorem chain_nodup {p : Option (List Nat)} {ns : List (List Nat)} (h : chainAfter p ns = true) :
    ns.Nodup := by
  obtain ⟨ks, hks, hs, -⟩ := chain_keys h
  exact mapM_seqKey_nodup hks (AverCert.SortedKeys.strictly_nodup hs)

/-- The export names are distinct when a walk reads the whole section. -/
theorem exportNamesDistinct_of_walk {cs : List Nat} {n len : Nat} {hs : List Nat}
    {e0 claimed : Nat} {ls : List Nat} {ds : List (String × String)}
    {p' : Option (List Nat)} {off' : Nat}
    (hn : join 1024 cs = n) (hfit : chunksFit 1024 cs = true)
    (hH : exportsHead cs len hs e0 ls = true)
    (hW : walkExports cs claimed none e0 ls ds = some (p', off')) :
    exportNamesDistinct n len = true := by
  obtain ⟨E, hE, hc, -⟩ := walk_spec ls none e0 ds p' off' hW
  obtain ⟨ks, hks, hs, -⟩ := chain_keys hc
  unfold exportNamesDistinct byteSeqListNodup
  rw [decodeRawExports_of_cut (exportsCut_of_walk hn hfit hH hE)]
  simp only
  unfold AverCert.WasmSlice.seqKeys
  rw [hks]
  exact AverCert.SortedKeys.natListNodup_of_nodup (AverCert.SortedKeys.strictly_nodup hs)

/-- The export section decodes on its declared cut when a walk reads it. -/
theorem exportsCutOk_of_walk {cs : List Nat} {n len : Nat} {hs : List Nat}
    {e0 claimed : Nat} {ls : List Nat} {ds : List (String × String)}
    {p' : Option (List Nat)} {off' : Nat}
    (hn : join 1024 cs = n) (hfit : chunksFit 1024 cs = true)
    (hH : exportsHead cs len hs e0 ls = true)
    (hW : walkExports cs claimed none e0 ls ds = some (p', off')) :
    (decodeRawExportsCut n len ls).isSome = true := by
  obtain ⟨E, hE, -, -⟩ := walk_spec ls none e0 ds p' off' hW
  rw [exportsCut_of_walk hn hfit hH hE]
  rfl

/-! ### Lists read through `mapM` -/

theorem mapM_length {α β : Type} {f : α → Option β} :
    ∀ {l : List α} {ys : List β}, l.mapM f = some ys → ys.length = l.length
  | [], ys, h => by
      simp only [List.mapM_nil, Option.pure_def, Option.some.injEq] at h
      subst h; rfl
  | x :: xs, ys, h => by
      obtain ⟨k, ks, -, hks, rfl⟩ := mapM_cons_some h
      simp [mapM_length hks]

theorem mapM_mem {α β : Type} {f : α → Option β} :
    ∀ {l : List α} {ys : List β}, l.mapM f = some ys → ∀ y, y ∈ ys ↔ ∃ x ∈ l, f x = some y
  | [], ys, h, y => by
      simp only [List.mapM_nil, Option.pure_def, Option.some.injEq] at h
      subst h; simp
  | x :: xs, ys, h, y => by
      obtain ⟨k, ks, hk, hks, rfl⟩ := mapM_cons_some h
      rw [List.mem_cons, mapM_mem hks y]
      constructor
      · rintro (rfl | ⟨x', hx', hfx⟩)
        · exact ⟨x, List.mem_cons_self, hk⟩
        · exact ⟨x', List.mem_cons_of_mem _ hx', hfx⟩
      · rintro ⟨x', hx', hfx⟩
        cases hx' with
        | head => rw [hk] at hfx; exact Or.inl (Option.some.inj hfx).symm
        | tail _ hx' => exact Or.inr ⟨x', hx', hfx⟩

theorem mapM_some {α β : Type} {f : α → Option β} :
    ∀ {l : List α}, (∀ x ∈ l, (f x).isSome = true) → ∃ ys, l.mapM f = some ys
  | [], _ => ⟨[], rfl⟩
  | x :: xs, h => by
      obtain ⟨y, hy⟩ := Option.isSome_iff_exists.mp (h x List.mem_cons_self)
      obtain ⟨ys, hys⟩ := mapM_some (l := xs) (fun x' hx' => h x' (List.mem_cons_of_mem _ hx'))
      exact ⟨y :: ys, by simp [List.mapM_cons, hy, hys]⟩

/-- A `mapM` of an injective reading keeps a list's elements distinct. -/
theorem mapM_nodup {α β : Type} {f : α → Option β}
    (hinj : ∀ {x x' : α} {y : β}, f x = some y → f x' = some y → x = x') :
    ∀ {l : List α} {ys : List β}, l.mapM f = some ys → l.Nodup → ys.Nodup
  | [], ys, h, _ => by
      simp only [List.mapM_nil, Option.pure_def, Option.some.injEq] at h
      subst h; exact List.nodup_nil
  | x :: xs, ys, h, hnd => by
      obtain ⟨k, ks, hk, hks, rfl⟩ := mapM_cons_some h
      obtain ⟨hx, hxs⟩ := List.nodup_cons.mp hnd
      refine List.nodup_cons.mpr ⟨fun hmem => ?_, mapM_nodup hinj hks hxs⟩
      obtain ⟨x', hx', hfx'⟩ := (mapM_mem hks k).mp hmem
      exact hx (hinj hk hfx' ▸ hx')

theorem exportEntryKey_isSome {e : AverCert.WasmSlice.ExportEntry}
    (h : (AverCert.WasmSlice.seqKey e.name).isSome = true) : (exportEntryKey e).isSome = true := by
  unfold exportEntryKey
  obtain ⟨k, hk⟩ := Option.isSome_iff_exists.mp h
  simp [hk]

theorem exportEntryKey_name {e : AverCert.WasmSlice.ExportEntry} {k : ExportKey}
    (h : exportEntryKey e = some k) : AverCert.WasmSlice.seqKey e.name = some k.name := by
  unfold exportEntryKey at h
  obtain ⟨a, ha, rfl⟩ := Option.map_eq_some_iff.mp h
  exact ha

/-! ### The accounting from set facts -/

/-- `exportsAccountedOf`, from what it decides stated as facts about the
    actual entries, the claimed indices and the declared names. -/
theorem exportsAccountedOf_of_props {n len : Nat}
    {E : List AverCert.WasmSlice.ExportEntry} {C : List Nat} {D : List AverCert.WasmSlice.ByteSeq}
    (hE : AverCert.WasmSlice.enumExports n len = some E)
    (hkE : ∀ e ∈ E, (AverCert.WasmSlice.seqKey e.name).isSome = true)
    (hnE : (E.map (·.name)).Nodup) (hnD : D.Nodup)
    (hcov : ∀ e ∈ E, (e.kind = 0 ∧ e.idx ∈ C) ∨ e.name ∈ D)
    (hDE : ∀ d ∈ D, d ∈ E.map (·.name)) :
    exportsAccountedOf n len C D = true := by
  have hkD : ∀ d ∈ D, (AverCert.WasmSlice.seqKey d).isSome = true := by
    intro d hd
    obtain ⟨e, he, rfl⟩ := List.mem_map.mp (hDE d hd)
    exact hkE e he
  obtain ⟨A, hA⟩ := mapM_some (f := exportEntryKey) (fun e he => exportEntryKey_isSome (hkE e he))
  obtain ⟨DK, hDK⟩ := mapM_some (f := AverCert.WasmSlice.seqKey) hkD
  have hAN : (E.map (·.name)).mapM AverCert.WasmSlice.seqKey = some (A.map (·.name)) := by
    have := AverCert.SortedKeys.mapM_key_names E
    rw [hA, Option.map_some] at this
    exact this.symm
  have hseq : ∀ {x x' : AverCert.WasmSlice.ByteSeq} {y : Nat},
      AverCert.WasmSlice.seqKey x = some y → AverCert.WasmSlice.seqKey x' = some y → x = x' :=
    fun h h' => AverCert.WasmSlice.seqKey_inj h h'
  unfold exportsAccountedOf
  rw [hE]
  simp only [hA]
  unfold AverCert.WasmSlice.seqKeys
  rw [hDK]
  simp only [Bool.and_eq_true, List.all_eq_true, exportEntryAccounted, Bool.or_eq_true,
    beq_iff_eq, AverCert.SortedKeys.orderedSet_contains]
  refine ⟨⟨⟨?_, ?_⟩, ?_⟩, ?_⟩
  · exact AverCert.SortedKeys.natListNodup_of_nodup (mapM_nodup hseq hAN hnE)
  · exact AverCert.SortedKeys.natListNodup_of_nodup (mapM_nodup hseq hDK hnD)
  · intro a ha
    obtain ⟨e, he, hea⟩ := (mapM_mem hA a).mp ha
    have hka : a.kind = e.kind ∧ a.idx = e.idx := by
      unfold exportEntryKey at hea
      obtain ⟨k, -, rfl⟩ := Option.map_eq_some_iff.mp hea
      exact ⟨rfl, rfl⟩
    rcases hcov e he with ⟨hk, hi⟩ | hd
    · exact Or.inl ⟨hka.1.trans hk, hka.2 ▸ hi⟩
    · exact Or.inr ((mapM_mem hDK a.name).mpr ⟨e.name, hd, exportEntryKey_name hea⟩)
  · intro k hk
    obtain ⟨d, hd, hdk⟩ := (mapM_mem hDK k).mp hk
    obtain ⟨e, he, rfl⟩ := List.mem_map.mp (hDE d hd)
    obtain ⟨a, ha⟩ := Option.isSome_iff_exists.mp (exportEntryKey_isSome (hkE e he))
    refine List.mem_map.mpr ⟨a, (mapM_mem hA a).mpr ⟨e, he, ha⟩, ?_⟩
    have := exportEntryKey_name ha
    rw [hdk] at this
    exact (Option.some.inj this).symm

/-- Names of a list's elements, equal as a list to a mapped list. -/
theorem map_of_map_some {α β γ : Type} {f : α → Option γ} {g : α → γ} {h : β → γ}
    (hfg : ∀ a c, f a = some c → g a = c) :
    ∀ (as : List α) (bs : List β), as.map f = bs.map (fun b => some (h b)) → as.map g = bs.map h
  | [], [], _ => rfl
  | a :: as, b :: bs, heq => by
      simp only [List.map_cons, List.cons.injEq] at heq ⊢
      exact ⟨hfg a _ heq.1, map_of_map_some hfg as bs heq.2⟩
  | [], _ :: _, heq => by simp at heq
  | _ :: _, [], heq => by simp at heq

/-! ### The accounting from the walk -/

/-- The export accounting, from a walk over the whole export section and the
    claimed indices' bits. The walk reads every entry, requires the names'
    keys increasing, and matches every entry that is not a function export of
    a claimed index with the next declared name. -/
theorem exportsAccounted_of_walk {n len : Nat} {m : Manifest} {cs hs : List Nat}
    {e0 claimed : Nat} {ls : List Nat} {p' : Option (List Nat)} {off' : Nat}
    (hn : join 1024 cs = n) (hfit : chunksFit 1024 cs = true)
    (hH : exportsHead cs len hs e0 ls = true)
    (hW : walkExports cs claimed none e0 ls m.subject.declaredUncertified = some (p', off'))
    (hC : AverCert.SortedKeys.bitsOf (claimedIndices m) = claimed) :
    exportsAccountedOf n len (claimedIndices m) (declaredUncertifiedNames m) = true := by
  obtain ⟨E, hEw, hc, hd⟩ := walk_spec ls none e0 m.subject.declaredUncertified p' off' hW
  have hcut := exportsCut_of_walk hn hfit hH hEw
  have hnE : (E.map (·.name)).Nodup := chain_nodup hc
  have hkE : ∀ e ∈ E, (AverCert.WasmSlice.seqKey e.name).isSome = true := by
    obtain ⟨ks, hks, -, -⟩ := chain_keys hc
    intro e he
    obtain ⟨k, -, hk⟩ := mapM_seqKey_mem hks e.name (List.mem_map_of_mem he)
    simp [hk]
  -- The declared names are the entries that are not claimed exports, in order.
  have hD : declaredUncertifiedNames m =
      (E.filter (fun e => !claimedEntry claimed e)).map (·.name) := by
    unfold declaredUncertifiedNames
    exact map_of_map_some (fun a c h => stringBytes_of_ascii h) _ _ hd
  rw [hD]
  have hsub : ((E.filter (fun e => !claimedEntry claimed e)).map (·.name)).Sublist
      (E.map (·.name)) := List.Sublist.map _ List.filter_sublist
  refine exportsAccountedOf_of_props (decodeRawExports_of_cut hcut) hkE hnE
    (List.Nodup.sublist hsub hnE) ?_ (fun d hd => hsub.subset hd)
  intro e he
  cases hb : claimedEntry claimed e
  · exact Or.inr (List.mem_map_of_mem (List.mem_filter.mpr ⟨he, by simp [hb]⟩))
  · left
    simp only [claimedEntry, Bool.and_eq_true, beq_iff_eq] at hb
    refine ⟨hb.1, ?_⟩
    rw [← hC] at hb
    exact (AverCert.SortedKeys.testBit_bitsOf _ _).mp hb.2

end AverCert.ScaleExports

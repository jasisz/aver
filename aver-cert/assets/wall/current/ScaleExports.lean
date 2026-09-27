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
whose start is a bit of `cert` (the declared starts of the planned exports'
entries) is left to the plans, which read each of them at its site; every
other entry must be the next declared-uncertified name, read as ASCII bytes.
The walk takes the declared list in pieces, one per block of entries, so a
block of the section is one declaration (`walk_append`). -/

/-- The name `b` has a key above the key of the previous name `p`. -/
def keyAfter : Option (List Nat) → List Nat → Bool
  | none, b => (AverCert.WasmSlice.seqKey b).isSome
  | some a, b =>
      match AverCert.WasmSlice.seqKey a, AverCert.WasmSlice.seqKey b with
      | some x, some y => decide (x < y)
      | _, _ => false

/-- The export entries of the given lengths from `off`, after the name `p`,
    against the declared names `ds`: the last name and the offset after the
    last entry. -/
def walkExports (cs : List Nat) (e0 cert : Nat) :
    Option (List Nat) → Nat → List Nat → List (String × String) →
      Option (Option (List Nat) × Nat)
  | p, off, [], [] => some (p, off)
  | _, _, [], _ :: _ => none
  | p, off, l :: ls, ds =>
      match whole readExportEntry (window 1024 cs off l, l) with
      | none => none
      | some e =>
          if keyAfter p e.name then
            if cert.testBit (off - e0) then walkExports cs e0 cert (some e.name) (off + l) ls ds
            else
              match ds with
              | d :: ds' =>
                  if asciiBytes d.1 == some e.name then
                    walkExports cs e0 cert (some e.name) (off + l) ls ds'
                  else none
              | [] => none
          else none

/-- One block of entries that uses up its piece of the declared names, then
    the rest. -/
theorem walk_append {cs : List Nat} {e0 cert : Nat} :
    ∀ {l1 : List Nat} {p : Option (List Nat)} {off : Nat} {d1 : List (String × String)}
      {p' : Option (List Nat)} {off' : Nat} (l2 : List Nat) (d2 : List (String × String)),
      walkExports cs e0 cert p off l1 d1 = some (p', off') →
      walkExports cs e0 cert p off (l1 ++ l2) (d1 ++ d2) = walkExports cs e0 cert p' off' l2 d2
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
              cases hb : cert.testBit (off - e0) with
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
theorem walk_cons {cs : List Nat} {e0 cert : Nat} {l1 l2 : List Nat}
    {d1 d2 : List (String × String)} {p p' : Option (List Nat)} {off off' : Nat}
    {r : Option (Option (List Nat) × Nat)}
    (h1 : walkExports cs e0 cert p off l1 d1 = some (p', off'))
    (h2 : walkExports cs e0 cert p' off' l2 d2 = r) :
    walkExports cs e0 cert p off (l1 ++ l2) (d1 ++ d2) = r :=
  (walk_append l2 d2 h1).trans h2

/-! ### What the walk reads -/

/-- The windows of consecutive entries of the given lengths from `off`. -/
def winsAt (cs : List Nat) : Nat → List Nat → List (Nat × Nat)
  | _, [] => []
  | off, l :: ls => (window 1024 cs off l, l) :: winsAt cs (off + l) ls

/-- The offsets of consecutive entries of the given lengths from `off`. -/
def offsAt : Nat → List Nat → List Nat
  | _, [] => []
  | off, l :: ls => off :: offsAt (off + l) ls

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

theorem walk_spec {cs : List Nat} {e0 cert : Nat} :
    ∀ (ls : List Nat) (p : Option (List Nat)) (off : Nat) (ds : List (String × String))
      (p' : Option (List Nat)) (off' : Nat),
      walkExports cs e0 cert p off ls ds = some (p', off') →
      ∃ E : List AverCert.WasmSlice.ExportEntry,
        (winsAt cs off ls).mapM (whole readExportEntry) = some E ∧
        chainAfter p (E.map (·.name)) = true ∧
        ds.map (fun d => asciiBytes d.1) =
          ((E.zip (offsAt off ls)).filter (fun q => !cert.testBit (q.2 - e0))).map
            (fun q => some q.1.name) ∧
        (∀ l ∈ ls, 0 < l)
  | [], p, off, [], p', off', _ => ⟨[], rfl, rfl, rfl, fun _ h => by cases h⟩
  | [], _, _, _ :: _, _, _, h => by simp [walkExports] at h
  | l :: ls, p, off, ds, p', off', h => by
      simp only [walkExports] at h
      split at h
      · cases h
      · rename_i e he
        have hl : 0 < l := by
          rcases Nat.eq_zero_or_pos l with h0 | h0
          · subst h0; rw [whole_export_zero] at he; cases he
          · exact h0
        split at h
        · rename_i hk
          split at h
          · rename_i hb
            obtain ⟨E, hE, hc, hd, hpos⟩ := walk_spec ls (some e.name) (off + l) ds p' off' h
            refine ⟨e :: E, ?_, ?_, ?_, ?_⟩
            · simp only [winsAt, List.mapM_cons, he, hE, Option.bind_eq_bind, Option.bind_some,
                Option.pure_def]
            · simp only [List.map_cons, chainAfter, hk, hc, Bool.and_self]
            · simp only [offsAt, List.zip_cons_cons, List.filter_cons, hb, Bool.not_true,
                Bool.false_eq_true, ↓reduceIte]
              exact hd
            · intro x hx
              cases hx with
              | head => exact hl
              | tail _ hx => exact hpos x hx
          · rename_i hb
            split at h
            · rename_i d ds'
              split at h
              · rename_i hdm
                obtain ⟨E, hE, hc, hd, hpos⟩ := walk_spec ls (some e.name) (off + l) ds' p' off' h
                refine ⟨e :: E, ?_, ?_, ?_, ?_⟩
                · simp only [winsAt, List.mapM_cons, he, hE, Option.bind_eq_bind,
                    Option.bind_some, Option.pure_def]
                · simp only [List.map_cons, chainAfter, hk, hc, Bool.and_self]
                · simp only [beq_iff_eq] at hdm
                  simp only [offsAt, List.zip_cons_cons, List.filter_cons, hb, Bool.not_false,
                    ↓reduceIte, List.map_cons, hdm, hd]
                · intro x hx
                  cases hx with
                  | head => exact hl
                  | tail _ hx => exact hpos x hx
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
    {e0 cert : Nat} {ls : List Nat} {ds : List (String × String)}
    {p' : Option (List Nat)} {off' : Nat}
    (hn : join 1024 cs = n) (hfit : chunksFit 1024 cs = true)
    (hH : exportsHead cs len hs e0 ls = true)
    (hW : walkExports cs e0 cert none e0 ls ds = some (p', off')) :
    exportNamesDistinct n len = true := by
  obtain ⟨E, hE, hc, -, -⟩ := walk_spec ls none e0 ds p' off' hW
  obtain ⟨ks, hks, hs, -⟩ := chain_keys hc
  unfold exportNamesDistinct byteSeqListNodup
  rw [decodeRawExports_of_cut (exportsCut_of_walk hn hfit hH hE)]
  simp only
  unfold AverCert.WasmSlice.seqKeys
  rw [hks]
  exact AverCert.SortedKeys.natListNodup_of_nodup (AverCert.SortedKeys.strictly_nodup hs)

/-- The export section decodes on its declared cut when a walk reads it. -/
theorem exportsCutOk_of_walk {cs : List Nat} {n len : Nat} {hs : List Nat}
    {e0 cert : Nat} {ls : List Nat} {ds : List (String × String)}
    {p' : Option (List Nat)} {off' : Nat}
    (hn : join 1024 cs = n) (hfit : chunksFit 1024 cs = true)
    (hH : exportsHead cs len hs e0 ls = true)
    (hW : walkExports cs e0 cert none e0 ls ds = some (p', off')) :
    (decodeRawExportsCut n len ls).isSome = true := by
  obtain ⟨E, hE, -, -, -⟩ := walk_spec ls none e0 ds p' off' hW
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

theorem exportEntryKey_inj {e e' : AverCert.WasmSlice.ExportEntry} {k : ExportKey}
    (h : exportEntryKey e = some k) (h' : exportEntryKey e' = some k) : e = e' := by
  unfold exportEntryKey at h h'
  obtain ⟨a, ha, rfl⟩ := Option.map_eq_some_iff.mp h
  obtain ⟨b, hb, hab⟩ := Option.map_eq_some_iff.mp h'
  simp only [ExportKey.mk.injEq] at hab
  obtain ⟨rfl, hk, hi⟩ := hab
  have hn := AverCert.WasmSlice.seqKey_inj hb ha
  cases e; cases e'
  simp_all

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
    actual, certified and declared lists. -/
theorem exportsAccountedOf_of_props {n len : Nat}
    {E C : List AverCert.WasmSlice.ExportEntry} {D : List AverCert.WasmSlice.ByteSeq}
    (hE : AverCert.WasmSlice.enumExports n len = some E)
    (hkE : ∀ e ∈ E, (AverCert.WasmSlice.seqKey e.name).isSome = true)
    (hnE : (E.map (·.name)).Nodup) (hnC : (C.map (·.name)).Nodup) (hnD : D.Nodup)
    (hdisj : ∀ c ∈ C, c.name ∉ D)
    (hcov : ∀ e ∈ E, e ∈ C ∨ e.name ∈ D)
    (hCE : ∀ c ∈ C, c ∈ E) (hDE : ∀ d ∈ D, d ∈ E.map (·.name)) :
    exportsAccountedOf n len C D = true := by
  have hkC : ∀ c ∈ C, (AverCert.WasmSlice.seqKey c.name).isSome = true :=
    fun c hc => hkE c (hCE c hc)
  have hkD : ∀ d ∈ D, (AverCert.WasmSlice.seqKey d).isSome = true := by
    intro d hd
    obtain ⟨e, he, rfl⟩ := List.mem_map.mp (hDE d hd)
    exact hkE e he
  obtain ⟨A, hA⟩ := mapM_some (f := exportEntryKey) (fun e he => exportEntryKey_isSome (hkE e he))
  obtain ⟨CK, hCK⟩ := mapM_some (f := exportEntryKey) (fun c hc => exportEntryKey_isSome (hkC c hc))
  obtain ⟨DK, hDK⟩ := mapM_some (f := AverCert.WasmSlice.seqKey) hkD
  have hAN : (E.map (·.name)).mapM AverCert.WasmSlice.seqKey = some (A.map (·.name)) := by
    have := AverCert.SortedKeys.mapM_key_names E
    rw [hA, Option.map_some] at this
    exact this.symm
  have hCN : (C.map (·.name)).mapM AverCert.WasmSlice.seqKey = some (CK.map (·.name)) := by
    have := AverCert.SortedKeys.mapM_key_names C
    rw [hCK, Option.map_some] at this
    exact this.symm
  have hseq : ∀ {x x' : AverCert.WasmSlice.ByteSeq} {y : Nat},
      AverCert.WasmSlice.seqKey x = some y → AverCert.WasmSlice.seqKey x' = some y → x = x' :=
    fun h h' => AverCert.WasmSlice.seqKey_inj h h'
  unfold exportsAccountedOf
  rw [hE]
  simp only [hA, hCK]
  unfold AverCert.WasmSlice.seqKeys
  rw [hDK]
  simp only [Bool.and_eq_true, List.all_eq_true, Bool.or_eq_true, Bool.not_eq_true',
    AverCert.SortedKeys.orderedSet_contains]
  refine ⟨⟨⟨⟨⟨⟨?_, ?_⟩, ?_⟩, ?_⟩, ?_⟩, ?_⟩, ?_⟩
  · exact AverCert.SortedKeys.natListNodup_of_nodup (mapM_nodup hseq hAN hnE)
  · exact AverCert.SortedKeys.natListNodup_of_nodup (mapM_nodup hseq hCN hnC)
  · exact AverCert.SortedKeys.natListNodup_of_nodup (mapM_nodup hseq hDK hnD)
  · intro k hk
    apply Bool.eq_false_iff.mpr
    intro hmem
    rw [AverCert.SortedKeys.orderedSet_contains] at hmem
    obtain ⟨cn, hcn, hck⟩ := (mapM_mem hCN k).mp hk
    obtain ⟨d, hd, hdk⟩ := (mapM_mem hDK k).mp hmem
    obtain ⟨c, hc, rfl⟩ := List.mem_map.mp hcn
    exact hdisj c hc (hseq hck hdk ▸ hd)
  · intro a ha
    obtain ⟨e, he, hea⟩ := (mapM_mem hA a).mp ha
    rcases hcov e he with hc | hd
    · exact Or.inl ((mapM_mem hCK a).mpr ⟨e, hc, hea⟩)
    · exact Or.inr ((mapM_mem hDK a.name).mpr ⟨e.name, hd, exportEntryKey_name hea⟩)
  · intro k hk
    obtain ⟨c, hc, hck⟩ := (mapM_mem hCK k).mp hk
    exact (mapM_mem hA k).mpr ⟨c, hCE c hc, hck⟩
  · intro k hk
    obtain ⟨d, hd, hdk⟩ := (mapM_mem hDK k).mp hk
    obtain ⟨e, he, rfl⟩ := List.mem_map.mp (hDE d hd)
    obtain ⟨a, ha⟩ := Option.isSome_iff_exists.mp (exportEntryKey_isSome (hkE e he))
    refine List.mem_map.mpr ⟨a, (mapM_mem hA a).mpr ⟨e, he, ha⟩, ?_⟩
    have := exportEntryKey_name ha
    rw [hdk] at this
    exact (Option.some.inj this).symm

/-! ### The planned exports' sites

Every planned export's plan check reads its export entry at a declared site
(`ScaleLayout.exportSite`): an entry start of the section, whose window is the
export of the plan's name at its function. `certBitsOf` requires the sites of
the planned exports distinct and at or after the first entry, and returns them
as bits relative to it: the `cert` bits the walk leaves to the plans. -/

/-- The planned exports' sites as bits relative to `e0`, from `bits`. -/
def certBitsOf (e0 : Nat) : List FnEntry → List (Nat × Nat) → Nat → Option Nat
  | [], [], bits => some bits
  | e :: es, x :: xs, bits =>
      if e.exported then
        if decide (e0 ≤ x.1) && !bits.testBit (x.1 - e0) then
          certBitsOf e0 es xs (bits ||| (1 <<< (x.1 - e0)))
        else none
      else certBitsOf e0 es xs bits
  | _, _, _ => none

theorem certBitsOf_spec {e0 : Nat} : ∀ {fns : List FnEntry} {xs : List (Nat × Nat)} {bits cert : Nat},
    certBitsOf e0 fns xs bits = some cert →
    fns.length = xs.length ∧
    (((fns.zip xs).filter (·.1.exported)).map (·.2.1)).Nodup ∧
    (∀ q ∈ (fns.zip xs).filter (·.1.exported), e0 ≤ q.2.1 ∧ bits.testBit (q.2.1 - e0) = false) ∧
    ∀ t, cert.testBit t = true ↔
      (bits.testBit t = true ∨ ∃ q ∈ (fns.zip xs).filter (·.1.exported), q.2.1 - e0 = t)
  | [], [], bits, cert, h => by
      simp only [certBitsOf, Option.some.injEq] at h
      subst h
      simp
  | e :: es, x :: xs, bits, cert, h => by
      simp only [certBitsOf] at h
      split at h
      · rename_i hex
        split at h
        · rename_i hc
          simp only [Bool.and_eq_true, decide_eq_true_eq, Bool.not_eq_true'] at hc
          obtain ⟨hle, hfree⟩ := hc
          obtain ⟨hlen, hnd, hall, hbits⟩ := certBitsOf_spec h
          have hfilter : (List.zip (e :: es) (x :: xs)).filter (·.1.exported) =
              (e, x) :: (es.zip xs).filter (·.1.exported) := by
            simp [List.filter_cons, hex]
          rw [hfilter]
          refine ⟨by simp [hlen], ?_, ?_, ?_⟩
          · refine List.nodup_cons.mpr ⟨fun hmem => ?_, hnd⟩
            obtain ⟨q, hq, hqx⟩ := List.mem_map.mp hmem
            obtain ⟨-, hq2⟩ := hall q hq
            have : (bits ||| (1 <<< (x.1 - e0))).testBit (q.2.1 - e0) = true :=
              (AverCert.SortedKeys.testBit_or_shift _ _ _).mpr (Or.inr (by rw [hqx]))
            rw [hq2] at this
            cases this
          · intro q hq
            cases hq with
            | head => exact ⟨hle, hfree⟩
            | tail _ hq =>
                obtain ⟨hq1, hq2⟩ := hall q hq
                refine ⟨hq1, ?_⟩
                cases hb : bits.testBit (q.2.1 - e0)
                · rfl
                · have : (bits ||| (1 <<< (x.1 - e0))).testBit (q.2.1 - e0) = true :=
                    (AverCert.SortedKeys.testBit_or_shift _ _ _).mpr (Or.inl hb)
                  rw [hq2] at this; cases this
          · intro t
            rw [hbits t, AverCert.SortedKeys.testBit_or_shift]
            constructor
            · rintro ((h1 | h1) | ⟨q, hq, hqt⟩)
              · exact Or.inl h1
              · exact Or.inr ⟨(e, x), List.mem_cons_self, h1.symm⟩
              · exact Or.inr ⟨q, List.mem_cons_of_mem _ hq, hqt⟩
            · rintro (h1 | ⟨q, hq, hqt⟩)
              · exact Or.inl (Or.inl h1)
              · cases hq with
                | head => exact Or.inl (Or.inr hqt.symm)
                | tail _ hq => exact Or.inr ⟨q, hq, hqt⟩
        · cases h
      · rename_i hex
        obtain ⟨hlen, hnd, hall, hbits⟩ := certBitsOf_spec h
        have hfilter : (List.zip (e :: es) (x :: xs)).filter (·.1.exported) =
            (es.zip xs).filter (·.1.exported) := by
          simp [List.filter_cons, hex]
        rw [hfilter]
        exact ⟨by simp [hlen], hnd, hall, hbits⟩
  | [], _ :: _, _, _, h => by simp [certBitsOf] at h
  | _ :: _, [], _, _, h => by simp [certBitsOf] at h

/-- Every planned export's site, from the plan checks. -/
theorem sites_of_plansFrom {cs : List Nat} {L : Layout} {fts : List FnType} {M : AverCert.Grammar.MCtx}
    {F : List FnEntry} {e0 B : Nat} :
    ∀ (fns : List FnEntry) (ds : List FnDecl) (rs : List Nat) (xs : List (Nat × Nat)),
      plansFrom cs L fts M F e0 B fns ds rs xs = true →
      fns.map (·.name) = ds.map (fun d => String.ofList d.name) →
      ∀ q ∈ fns.zip xs, q.1.exported = true →
        exportSite cs e0 B q.2.1 q.2.2 (stringBytes q.1.name) q.1.funcIdx = true
  | [], _, _, _, _, _, q, hq, _ => by cases hq
  | e :: es, d :: ds, r :: rs, x :: xs, h, hnm, q, hq, hex => by
      simp only [List.map_cons, List.cons.injEq] at hnm
      simp only [plansFrom, planOne, Bool.and_eq_true] at h
      obtain ⟨⟨⟨-, hx⟩, -⟩, hrest⟩ := h
      cases hq with
      | head =>
          simp only at hex
          simp only [hex, Bool.not_true, Bool.false_or] at hx
          rw [hnm.1, stringBytes_ofList]
          exact hx
      | tail _ hq => exact sites_of_plansFrom es ds rs xs hrest hnm.2 q hq hex
  | _ :: _, [], _, _, _, hnm, _, _, _ => by simp at hnm
  | _ :: _, _ :: _, [], _, h, _, _, _, _ => by cases ‹List (Nat × Nat)› <;> simp [plansFrom] at h
  | _ :: _, _ :: _, _ :: _, [], h, _, _, _, _ => by simp [plansFrom] at h

/-- A site names an entry of the section: its position, whose offset is the
    site's and whose entry is the site's. -/
theorem exportSite_at {cs : List Nat} {n len : Nat} {hs : List Nat} {e0 B : Nat}
    {ls : List Nat} {E : List AverCert.WasmSlice.ExportEntry} {off l fi : Nat}
    {name : List Nat} (hn : join 1024 cs = n) (hfit : chunksFit 1024 cs = true)
    (h : exportsHead cs len hs e0 ls = true) (hE : decodeRawExportsCut n len ls = some E)
    (hB : startBits 0 ls = B) (hsite : exportSite cs e0 B off l name fi = true) :
    ∃ p : Nat, p < ls.length ∧ e0 + (ls.take p).sum = off ∧
      E[p]? = some (⟨name, 0, fi⟩ : AverCert.WasmSlice.ExportEntry) := by
  simp only [exportSite, Bool.and_eq_true, decide_eq_true_eq, beq_iff_eq] at hsite
  obtain ⟨⟨hle, hbit⟩, hw⟩ := hsite
  rw [← hB] at hbit
  obtain ⟨p, hp, hoff⟩ := startBits_spec ls 0 (off - e0) hbit
  have hat : e0 + (ls.take p).sum = off := by omega
  refine ⟨p, hp, hat, ?_⟩
  rw [exportWindow_eq hn hfit h hE p, List.getElem?_eq_getElem hp, Option.bind_some]
  rw [window_eq (by decide) hfit, hn] at hw
  rw [hat]
  cases hq : whole readExportEntry (slice n off ls[p], ls[p]) with
  | none =>
      have := exportWindow_some hn hfit h hE p hp
      rw [hat, hq] at this
      cases this
  | some y => rw [whole_unique readExportEntry_ext hq hw]

/-! ### Offsets and positions -/

theorem offsAt_length : ∀ (off : Nat) (ls : List Nat), (offsAt off ls).length = ls.length
  | _, [] => rfl
  | off, _ :: ls => by simp [offsAt, offsAt_length _ ls]

theorem offsAt_get : ∀ (off : Nat) (ls : List Nat) (p : Nat), p < ls.length →
    (offsAt off ls)[p]? = some (off + (ls.take p).sum)
  | _, [], _, hp => by simp at hp
  | off, l :: ls, 0, _ => by simp [offsAt]
  | off, l :: ls, p + 1, hp => by
      simp only [List.length_cons] at hp
      simp only [offsAt, List.getElem?_cons_succ, List.take_succ_cons, List.sum_cons]
      rw [offsAt_get (off + l) ls p (by omega), Nat.add_assoc]

theorem offsAt_ge : ∀ (off : Nat) (ls : List Nat), ∀ o ∈ offsAt off ls, off ≤ o
  | _, [], _, h => by cases h
  | off, l :: ls, o, h => by
      simp only [offsAt, List.mem_cons] at h
      rcases h with rfl | h
      · exact Nat.le_refl _
      · exact Nat.le_trans (Nat.le_add_right _ _) (offsAt_ge (off + l) ls o h)

/-- Entries of positive length start at distinct offsets. -/
theorem offsAt_nodup : ∀ (off : Nat) (ls : List Nat), (∀ l ∈ ls, 0 < l) → (offsAt off ls).Nodup
  | _, [], _ => List.nodup_nil
  | off, l :: ls, h => by
      simp only [offsAt]
      refine List.nodup_cons.mpr ⟨fun hmem => ?_, offsAt_nodup (off + l) ls
        (fun x hx => h x (List.mem_cons_of_mem _ hx))⟩
      have := offsAt_ge (off + l) ls off hmem
      have := h l List.mem_cons_self
      omega

/-- The certified entries, from the obligations of the plans. -/
theorem certifiedExportEntries_of_plans {m : Manifest} {fns : List FnEntry}
    (hO : m.obligations = obligationsOf m.subject m.types fns) :
    certifiedExportEntries m = (fns.filter (·.exported)).map (fun e =>
      ({ name := stringBytes e.name, kind := 0, idx := e.funcIdx } :
        AverCert.WasmSlice.ExportEntry)) := by
  unfold certifiedExportEntries
  rw [hO]
  unfold obligationsOf
  rw [List.map_map]
  rfl

theorem filter_zip_fst {α β γ : Type} (P : α → Bool) (f : α → γ) :
    ∀ (as : List α) (bs : List β), as.length = bs.length →
      ((as.zip bs).filter (fun q => P q.1)).map (fun q => f q.1) = (as.filter P).map f
  | [], [], _ => rfl
  | a :: as, b :: bs, h => by
      simp only [List.length_cons, Nat.add_right_cancel_iff] at h
      simp only [List.zip_cons_cons, List.filter_cons]
      split <;> simp [filter_zip_fst P f as bs h]
  | [], _ :: _, h => by simp at h
  | _ :: _, [], h => by simp at h

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

theorem inj_of_nodup_map {α β : Type} {f : α → β} :
    ∀ {l : List α}, (l.map f).Nodup → ∀ {a b : α}, a ∈ l → b ∈ l → f a = f b → a = b
  | [], _, _, _, ha, _, _ => by cases ha
  | x :: xs, h, a, b, ha, hb, hab => by
      simp only [List.map_cons, List.nodup_cons, List.mem_map] at h
      obtain ⟨hx, hxs⟩ := h
      cases ha with
      | head =>
          cases hb with
          | head => rfl
          | tail _ hb => exact absurd ⟨b, hb, hab.symm⟩ hx
      | tail _ ha =>
          cases hb with
          | head => exact absurd ⟨a, ha, hab⟩ hx
          | tail _ hb => exact inj_of_nodup_map hxs ha hb hab

theorem nodup_map_of_inj {α β : Type} {f : α → β} :
    ∀ {l : List α}, (∀ a ∈ l, ∀ b ∈ l, f a = f b → a = b) → l.Nodup → (l.map f).Nodup
  | [], _, _ => List.nodup_nil
  | x :: xs, hinj, hnd => by
      obtain ⟨hx, hxs⟩ := List.nodup_cons.mp hnd
      simp only [List.map_cons]
      refine List.nodup_cons.mpr ⟨fun hmem => ?_, nodup_map_of_inj
        (fun a ha b hb => hinj a (List.mem_cons_of_mem _ ha) b (List.mem_cons_of_mem _ hb)) hxs⟩
      obtain ⟨y, hy, hyx⟩ := List.mem_map.mp hmem
      exact hx (hinj y (List.mem_cons_of_mem _ hy) x List.mem_cons_self hyx ▸ hy)

theorem nodup_of_map {α β : Type} {f : α → β} {l : List α} (h : (l.map f).Nodup) : l.Nodup :=
  List.Pairwise.of_map f (fun _ _ hne heq => hne (heq ▸ rfl)) h

/-! ### The accounting from the walk and the plans -/

/-- The export accounting, from a walk over the whole export section, the
    plan checks (which read every planned export at its site) and the sites'
    bits. The walk reads every entry, requires the names' keys increasing, and
    matches every entry outside the sites' bits with the next declared name;
    the plans read every planned export at its site. -/
theorem exportsAccounted_of_walk {n len : Nat} {m : Manifest} {cs hs : List Nat}
    {e0 B cert : Nat} {ls : List Nat} {p' : Option (List Nat)} {off' : Nat}
    {L : Layout} {fts : List FnType} {M : AverCert.Grammar.MCtx} {F : List FnEntry}
    {fns : List FnEntry} {ds : List FnDecl} {rs : List Nat} {xs : List (Nat × Nat)}
    (hn : join 1024 cs = n) (hfit : chunksFit 1024 cs = true)
    (hH : exportsHead cs len hs e0 ls = true)
    (hB : startBits 0 ls = B)
    (hW : walkExports cs e0 cert none e0 ls m.subject.declaredUncertified = some (p', off'))
    (hP : plansFrom cs L fts M F e0 B fns ds rs xs = true)
    (hnames : fns.map (·.name) = ds.map (fun d => String.ofList d.name))
    (hC : certBitsOf e0 fns xs 0 = some cert)
    (hO : m.obligations = obligationsOf m.subject m.types fns) :
    exportsAccountedOf n len (certifiedExportEntries m) (declaredUncertifiedNames m) = true := by
  obtain ⟨E, hEw, hc, hd, hpos⟩ :=
    walk_spec ls none e0 m.subject.declaredUncertified p' off' hW
  have hcut := exportsCut_of_walk hn hfit hH hEw
  have hlenE : E.length = ls.length := by
    rw [mapM_length hEw, winsAt_length]
  obtain ⟨T, hT⟩ : ∃ T, T = E.zip (offsAt e0 ls) := ⟨_, rfl⟩
  rw [← hT] at hd
  have hTfst : T.map (·.1) = E := by
    rw [hT]; exact List.map_fst_zip (by rw [offsAt_length, hlenE]; exact Nat.le_refl _)
  have hTsnd : T.map (·.2) = offsAt e0 ls := by
    rw [hT]; exact List.map_snd_zip (by rw [offsAt_length, hlenE]; exact Nat.le_refl _)
  have hnE : (E.map (·.name)).Nodup := chain_nodup hc
  have hkE : ∀ e ∈ E, (AverCert.WasmSlice.seqKey e.name).isSome = true := by
    obtain ⟨ks, hks, -, -⟩ := chain_keys hc
    intro e he
    obtain ⟨k, -, hk⟩ := mapM_seqKey_mem hks e.name (List.mem_map_of_mem he)
    simp [hk]
  -- Two entries of the section with one name, or at one offset, are one.
  have hTname : ∀ q ∈ T, ∀ q' ∈ T, q.1.name = q'.1.name → q = q' := by
    have hnT : (T.map (fun q => q.1.name)).Nodup := by
      rw [show (fun q : AverCert.WasmSlice.ExportEntry × Nat => q.1.name) =
        (·.name) ∘ (·.1) from rfl, ← List.map_map, hTfst]; exact hnE
    exact fun q hq q' hq' h => inj_of_nodup_map hnT hq hq' h
  have hToff : ∀ q ∈ T, ∀ q' ∈ T, q.2 = q'.2 → q = q' := by
    have hnT : (T.map (·.2)).Nodup := by rw [hTsnd]; exact offsAt_nodup e0 ls hpos
    exact fun q hq q' hq' h => inj_of_nodup_map hnT hq hq' h
  have hTge : ∀ q ∈ T, e0 ≤ q.2 := fun q hq =>
    offsAt_ge e0 ls q.2 (hTsnd ▸ List.mem_map_of_mem hq)
  -- The declared names are the entries outside the sites' bits, in order.
  have hD : declaredUncertifiedNames m =
      (T.filter (fun q => !cert.testBit (q.2 - e0))).map (fun q => q.1.name) := by
    unfold declaredUncertifiedNames
    exact map_of_map_some (fun a c h => stringBytes_of_ascii h) _ _ hd
  -- The sites.
  obtain ⟨hlenx, hsnd, hsge, hbits⟩ := certBitsOf_spec hC
  have hsite := sites_of_plansFrom fns ds rs xs hP hnames
  have hat : ∀ q ∈ (fns.zip xs).filter (·.1.exported), ∃ t ∈ T, t.2 = q.2.1 ∧
      t.1 = ({ name := stringBytes q.1.name, kind := 0, idx := q.1.funcIdx } :
        AverCert.WasmSlice.ExportEntry) := by
    intro q hq
    have hq' := List.mem_filter.mp hq
    obtain ⟨p, hp, hoff, hEp⟩ := exportSite_at hn hfit hH hcut hB (hsite q hq'.1 hq'.2)
    have hpE : p < E.length := by omega
    refine ⟨(E[p], (offsAt e0 ls)[p]'(by rw [offsAt_length]; exact hp)), ?_, ?_, ?_⟩
    · rw [hT]
      exact List.mem_iff_getElem.mpr ⟨p, by simp [offsAt_length, hlenE, hp], by simp⟩
    · have := offsAt_get e0 ls p hp
      rw [List.getElem?_eq_getElem (by rw [offsAt_length]; exact hp)] at this
      simp only [Option.some.injEq] at this
      simp only [this, hoff]
    · rw [List.getElem?_eq_getElem hpE] at hEp
      exact Option.some.inj hEp
  have hCeq := certifiedExportEntries_of_plans hO
  rw [← filter_zip_fst (fun e : FnEntry => e.exported)
      (fun e : FnEntry => (⟨stringBytes e.name, 0, e.funcIdx⟩ : AverCert.WasmSlice.ExportEntry))
      fns xs hlenx] at hCeq
  rw [hCeq, hD]
  refine exportsAccountedOf_of_props (decodeRawExports_of_cut hcut) hkE hnE ?_ ?_ ?_ ?_ ?_ ?_
  · -- The certified names are distinct: their sites are.
    rw [List.map_map]
    refine nodup_map_of_inj ?_ (nodup_of_map hsnd)
    intro a ha b hb hab
    obtain ⟨ta, hta, hta2, hta1⟩ := hat a ha
    obtain ⟨tb, htb, htb2, htb1⟩ := hat b hb
    have : ta = tb := hTname ta hta tb htb (by simp only [Function.comp_apply] at hab; rw [hta1, htb1]; exact hab)
    exact inj_of_nodup_map hsnd ha hb (by rw [← hta2, ← htb2, this])
  · -- The declared names are distinct: they are names of the section.
    have hsub : ((T.filter (fun q => !cert.testBit (q.2 - e0))).map (fun q => q.1.name)).Sublist
        (T.map (fun q => q.1.name)) := List.Sublist.map _ List.filter_sublist
    refine List.Nodup.sublist hsub ?_
    rw [show (fun q : AverCert.WasmSlice.ExportEntry × Nat => q.1.name) =
      (·.name) ∘ (·.1) from rfl, ← List.map_map, hTfst]; exact hnE
  · -- No certified name is declared: a site's entry has its bit set.
    intro c hc hcd
    obtain ⟨a, ha, rfl⟩ := List.mem_map.mp hc
    obtain ⟨ta, hta, hta2, hta1⟩ := hat a ha
    obtain ⟨q, hq, hqn⟩ := List.mem_map.mp hcd
    have hq' := List.mem_filter.mp hq
    have : q = ta := hTname q hq'.1 ta hta (by rw [hqn, hta1])
    subst this
    have hbit : cert.testBit (q.2 - e0) = true := (hbits _).mpr (Or.inr ⟨a, ha, by rw [hta2]⟩)
    simp [hbit] at hq'
  · -- Every entry is certified or declared.
    intro e he
    have heT : ∃ o, (e, o) ∈ T := by
      rw [← hTfst] at he
      obtain ⟨q, hq, rfl⟩ := List.mem_map.mp he
      exact ⟨q.2, hq⟩
    obtain ⟨o, heo⟩ := heT
    cases hb : cert.testBit (o - e0)
    · right
      exact List.mem_map.mpr ⟨(e, o), List.mem_filter.mpr ⟨heo, by simp [hb]⟩, rfl⟩
    · left
      rcases (hbits _).mp hb with h0 | ⟨a, ha, hao⟩
      · simp at h0
      · obtain ⟨ta, hta, hta2, hta1⟩ := hat a ha
        have hge := hTge _ heo
        have hge' := (hsge a ha).1
        have heq : (e, o) = ta := hToff _ heo ta hta (by simp only; omega)
        have he' : e = ta.1 := by rw [← heq]
        rw [he', hta1]
        exact List.mem_map.mpr ⟨a, ha, rfl⟩
  · -- Every certified entry is in the section.
    intro c hc
    obtain ⟨a, ha, rfl⟩ := List.mem_map.mp hc
    obtain ⟨ta, hta, -, hta1⟩ := hat a ha
    rw [← hTfst, ← hta1]
    exact List.mem_map_of_mem hta
  · -- Every declared name is a name of the section.
    intro d hd
    obtain ⟨q, hq, rfl⟩ := List.mem_map.mp hd
    rw [← hTfst, List.map_map]
    exact List.mem_map_of_mem (List.mem_filter.mp hq).1

end AverCert.ScaleExports

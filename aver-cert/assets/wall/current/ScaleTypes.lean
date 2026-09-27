-- The type section in blocks, and the String helper roles read from them.
import ScaleExports

set_option linter.unusedSimpArgs false

namespace AverCert.ScaleTypes
open CertDecode CertDecode.StringHost AverCert.ByteWindow AverCert.ScaleBytes AverCert.ScaleLayout
open AverCert.DeclaredLayout AverCert.DeclaredLayout.StringFast AverCert.ScaleExports

/-! ### The type walk

`walkTypes` reads the type section one top-level entry at a time from a
declared offset, each on its own chunk window (a rec group whole, its subtypes
in order), and checks three producer declarations against every type it
reads, by its index: `SB`, the String byte-array types (the types
`StringHost.stringBytesIndex` picks, in order); `S`, the shape bits (bit `t`
set exactly when type `t`'s signature has the shape of an eq or concat helper,
`StringFast.candShape`); and `sigs`, the signature of every type whose bit is
set. A block of the section is one declaration (`walkTypes_append`). The walk
gives the section's cut (`typesCutOk_of_walk`), so the type section is read in
blocks, and it gives the String helper roles with the functions classified in
blocks (`roleTable_of_blocks`): a function reads its type's shape bit, and only
a function of a helper's shape reads its signature and its code entry. -/

/-- The declared signature of type `u`. -/
def sigAt : List (Nat × Sig) → Nat → Option Sig
  | [], _ => none
  | (t, s) :: rest, u => if t == u then some s else sigAt rest u

/-- Type `t` against the declarations; `sb` is the part of `SB` not yet met. -/
def entryOk (SB : List Nat) (S : Nat) (sigs : List (Nat × Sig)) (t : Nat) (e : TypeEntry)
    (sb : List Nat) : Option (List Nat) :=
  if S.testBit t == candShape SB (decodeTypeSig e) &&
      (!S.testBit t || decide (sigAt sigs t = decodeTypeSig e)) then
    match stringBytesIndex t e with
    | some _ =>
        match sb with
        | x :: sb' => if x == t then some sb' else none
        | [] => none
    | none => some sb
  else none

/-- Types `t, t + 1, …` against the declarations. -/
def entriesOk (SB : List Nat) (S : Nat) (sigs : List (Nat × Sig)) :
    Nat → List TypeEntry → List Nat → Option (Nat × List Nat)
  | t, [], sb => some (t, sb)
  | t, e :: es, sb =>
      match entryOk SB S sigs t e sb with
      | some sb' => entriesOk SB S sigs (t + 1) es sb'
      | none => none

/-- The top-level type entries of the given lengths from `off`, their types
    numbered from `t`: the next type index, the offset after the last entry,
    and the part of `SB` not yet met. -/
def walkTypes (cs : List Nat) (SB : List Nat) (S : Nat) (sigs : List (Nat × Sig)) :
    Nat → Nat → List Nat → List Nat → Option (Nat × Nat × List Nat)
  | t, off, [], sb => some (t, off, sb)
  | t, off, l :: ls, sb =>
      match whole readRecEntry (window 1024 cs off l, l) with
      | none => none
      | some g =>
          match entriesOk SB S sigs t g sb with
          | some (t', sb') => walkTypes cs SB S sigs t' (off + l) ls sb'
          | none => none

theorem walkTypes_append {cs SB : List Nat} {S : Nat} {sigs : List (Nat × Sig)} :
    ∀ {l1 : List Nat} {t off : Nat} {sb : List Nat} {t' off' : Nat} {sb' : List Nat}
      (l2 : List Nat),
      walkTypes cs SB S sigs t off l1 sb = some (t', off', sb') →
      walkTypes cs SB S sigs t off (l1 ++ l2) sb = walkTypes cs SB S sigs t' off' l2 sb'
  | [], t, off, sb, t', off', sb', l2, h => by
      simp only [walkTypes, Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl, rfl⟩ := h
      rfl
  | l :: ls, t, off, sb, t', off', sb', l2, h => by
      rw [List.cons_append, walkTypes.eq_2]
      rw [walkTypes.eq_2] at h
      cases hg : whole readRecEntry (window 1024 cs off l, l) with
      | none => simp [hg] at h
      | some g =>
          simp only [hg] at h ⊢
          cases he : entriesOk SB S sigs t g sb with
          | none => simp [he] at h
          | some p =>
              obtain ⟨t1, sb1⟩ := p
              simp only [he] at h ⊢
              exact walkTypes_append l2 h

/-- A block of top-level entries, and a walk over the rest. -/
theorem walkTypes_cons {cs SB : List Nat} {S : Nat} {sigs : List (Nat × Sig)} {l1 l2 : List Nat}
    {t off t' off' : Nat} {sb sb' : List Nat} {r : Option (Nat × Nat × List Nat)}
    (h1 : walkTypes cs SB S sigs t off l1 sb = some (t', off', sb'))
    (h2 : walkTypes cs SB S sigs t' off' l2 sb' = r) :
    walkTypes cs SB S sigs t off (l1 ++ l2) sb = r :=
  (walkTypes_append l2 h1).trans h2

theorem entriesOk_append {SB : List Nat} {S : Nat} {sigs : List (Nat × Sig)} :
    ∀ {es1 : List TypeEntry} {t : Nat} {sb : List Nat} {t' : Nat} {sb' : List Nat}
      (es2 : List TypeEntry),
      entriesOk SB S sigs t es1 sb = some (t', sb') →
      entriesOk SB S sigs t (es1 ++ es2) sb = entriesOk SB S sigs t' es2 sb'
  | [], t, sb, t', sb', es2, h => by
      simp only [entriesOk, Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl⟩ := h
      rfl
  | e :: es, t, sb, t', sb', es2, h => by
      rw [List.cons_append, entriesOk.eq_2]
      rw [entriesOk.eq_2] at h
      cases he : entryOk SB S sigs t e sb with
      | none => simp [he] at h
      | some sb1 =>
          simp only [he] at h ⊢
          exact entriesOk_append es2 h

/-- What a walk reads: the windows' groups, and the declarations checked
    against all of their types. -/
theorem walkTypes_spec {cs SB : List Nat} {S : Nat} {sigs : List (Nat × Sig)} :
    ∀ (ls : List Nat) (t off : Nat) (sb : List Nat) (t' off' : Nat) (sb' : List Nat),
      walkTypes cs SB S sigs t off ls sb = some (t', off', sb') →
      ∃ gs : List (List TypeEntry),
        (winsAt cs off ls).mapM (whole readRecEntry) = some gs ∧
        entriesOk SB S sigs t gs.flatten sb = some (t', sb')
  | [], t, off, sb, t', off', sb', h => by
      simp only [walkTypes, Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl, rfl⟩ := h
      exact ⟨[], rfl, rfl⟩
  | l :: ls, t, off, sb, t', off', sb', h => by
      simp only [walkTypes] at h
      split at h
      · cases h
      · rename_i g hg
        split at h
        · rename_i t1 sb1 he
          obtain ⟨gs, hgs, hrest⟩ := walkTypes_spec ls t1 (off + l) sb1 t' off' sb' h
          refine ⟨g :: gs, ?_, ?_⟩
          · simp only [winsAt, List.mapM_cons, hg, hgs, Option.bind_eq_bind, Option.bind_some,
              Option.pure_def]
          · simp only [List.flatten_cons]
            rw [entriesOk_append _ he]
            exact hrest
        · cases h

/-! ### The type section's head -/

/-- The type section's count and first entry, against the declared cut. -/
def typesHead (cs : List Nat) (len : Nat) (hs : List Nat) (t0 : Nat) (ls : List Nat) : Bool :=
  framingOk cs len hs &&
  match headersAt cs len 8 hs with
  | some S =>
      match S.find? (fun e => e.1 == 1) with
      | some (_, start, size) =>
          match readU (window 1024 cs start (min 5 size)) (min 5 size) with
          | some (cnt, _, rest) =>
              cnt == ls.length && t0 == start + (min 5 size - rest) &&
                ls.sum == size - (min 5 size - rest)
          | none => false
      | none => false
  | none => false

theorem typesHead_spec {cs : List Nat} {n len : Nat} {hs : List Nat} {t0 : Nat} {ls : List Nat}
    (hn : join 1024 cs = n) (hfit : chunksFit 1024 cs = true)
    (h : typesHead cs len hs t0 ls = true) :
    ∃ start size k, modulePayload 1 n len = some (slice n start size, size) ∧
      readU (slice n start size) size =
        some (ls.length, slice n t0 (size - k), size - k) ∧ ls.sum = size - k := by
  unfold typesHead at h
  simp only [Bool.and_eq_true] at h
  obtain ⟨hfr, hm⟩ := h
  split at hm
  · rename_i S hS
    split at hm
    · rename_i id start size hfind
      split at hm
      · rename_i cnt w' rest hU
        simp only [Bool.and_eq_true, beq_iff_eq] at hm
        obtain ⟨⟨rfl, rfl⟩, hsum⟩ := hm
        have hpay := modulePayload_of_framing hn hfit hfr hS 1
        rw [hfind] at hpay
        rw [window_eq (by decide) hfit, hn] at hU
        obtain ⟨-, hread⟩ := readU_window_slice (S := size) (by omega) hU
        exact ⟨start, size, min 5 size - rest, hpay, hread, hsum⟩
      · cases hm
    · cases hm
  · cases hm

/-- A walk over the whole type section decodes it on its declared cut. -/
theorem typesCut_of_walk {cs : List Nat}
    {n len : Nat} {hs : List Nat} {t0 : Nat} {ls : List Nat} {gs : List (List TypeEntry)}
    (hn : join 1024 cs = n) (hfit : chunksFit 1024 cs = true)
    (hH : typesHead cs len hs t0 ls = true)
    (hG : (winsAt cs t0 ls).mapM (whole readRecEntry) = some gs) :
    decodeTypesCut n len ls = some (typeInfoOf gs.flatten) := by
  obtain ⟨start, size, k, hpay, hread, hsum⟩ := typesHead_spec hn hfit hH
  unfold decodeTypesCut
  rw [hpay]
  simp only [hread, BEq.rfl, hsum, Bool.and_self, ↓reduceIte]
  rw [cutWin_eq, ← winsAt_eq hn hfit ls t0 (size - k) (by omega), hG]
  rfl

theorem typesCutOk_of_walk {cs SB : List Nat} {S : Nat} {sigs : List (Nat × Sig)}
    {n len : Nat} {hs : List Nat} {t0 : Nat} {ls : List Nat} {t' off' : Nat} {sb' : List Nat}
    (hn : join 1024 cs = n) (hfit : chunksFit 1024 cs = true)
    (hH : typesHead cs len hs t0 ls = true)
    (hW : walkTypes cs SB S sigs 0 t0 ls SB = some (t', off', sb')) :
    (decodeTypesCut n len ls).isSome = true := by
  obtain ⟨gs, hG, -⟩ := walkTypes_spec ls 0 t0 SB t' off' sb' hW
  rw [typesCut_of_walk hn hfit hH hG]
  rfl

/-! ### What the declarations give -/

theorem decodeTypeEntries_fst : ∀ (t : Nat) (es : List TypeEntry),
    (decodeTypeEntries t es).1 = es.map decodeTypeSig
  | _, [] => rfl
  | t, e :: es => by simp [decodeTypeEntries, decodeTypeEntries_fst (t + 1) es]

/-- The walk over the types from `t` ends `es.length` types later, meets the
    part of `sb` the section's String byte arrays are, and every type's shape
    bit and declared signature are its own. -/
theorem entriesOk_spec {SB : List Nat} {S : Nat} {sigs : List (Nat × Sig)} :
    ∀ (es : List TypeEntry) (t : Nat) (sb : List Nat) (t' : Nat) (sb' : List Nat),
      entriesOk SB S sigs t es sb = some (t', sb') →
      t' = t + es.length ∧ sb = (decodeTypeEntries t es).2 ++ sb' ∧
      ∀ i (hi : i < es.length), S.testBit (t + i) = candShape SB (decodeTypeSig es[i]) ∧
        (S.testBit (t + i) = true → sigAt sigs (t + i) = decodeTypeSig es[i])
  | [], t, sb, t', sb', h => by
      simp only [entriesOk, Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl⟩ := h
      exact ⟨rfl, rfl, fun i hi => by simp at hi⟩
  | e :: es, t, sb, t', sb', h => by
      simp only [entriesOk] at h
      split at h
      · rename_i sb1 he
        obtain ⟨ht, hsb, hall⟩ := entriesOk_spec es (t + 1) sb1 t' sb' h
        unfold entryOk at he
        split at he
        · rename_i hc
          simp only [Bool.and_eq_true, beq_iff_eq, Bool.or_eq_true, Bool.not_eq_true',
            decide_eq_true_eq] at hc
          obtain ⟨hbit, hsig⟩ := hc
          refine ⟨by rw [ht, List.length_cons]; omega, ?_, ?_⟩
          · simp only [decodeTypeEntries, consOpt]
            split at he
            · rename_i x hx
              split at he
              · rename_i y sb2
                split at he
                · rename_i hy
                  simp only [beq_iff_eq] at hy
                  simp only [Option.some.injEq] at he
                  subst he
                  have hx' : stringBytesIndex t e = some t := by
                    unfold stringBytesIndex at hx ⊢
                    split at hx
                    · split at hx
                      · simp_all
                      · cases hx
                    · cases hx
                  rw [hx', hsb, hy]
                  rfl
                · cases he
              · cases he
            · rename_i hx
              simp only [Option.some.injEq] at he
              subst he
              rw [hx, hsb]
          · intro i hi
            cases i with
            | zero =>
                simp only [Nat.add_zero, List.getElem_cons_zero]
                refine ⟨hbit, fun hb => ?_⟩
                rcases hsig with h0 | h0
                · rw [hb] at h0; cases h0
                · exact h0
            | succ i =>
                simp only [List.length_cons] at hi
                have := hall i (by omega)
                simp only [List.getElem_cons_succ]
                rw [show t + (i + 1) = t + 1 + i by omega]
                exact this
        · cases he
      · cases h

/-- The signature lookup a function classification reads: a type's
    declared signature when its shape bit is set, none otherwise. -/
def lookS (S : Nat) (sigs : List (Nat × Sig)) (ty : Nat) : Option Sig :=
  if S.testBit ty then sigAt sigs ty else none

theorem testBit_of_lt {S T i : Nat} (hS : S < 2 ^ T) (hi : T ≤ i) : S.testBit i = false :=
  Nat.testBit_lt_two_pow (Nat.lt_of_lt_of_le hS (Nat.pow_le_pow_right (by decide) hi))

/-- Reading a type's signature through its shape bit classifies a function as
    reading it from the decoded section does. -/
theorem classifyOne_lookS {SB : List Nat} {S : Nat} {sigs : List (Nat × Sig)}
    {es : List TypeEntry} {sb' : List Nat}
    (hW : entriesOk SB S sigs 0 es SB = some (es.length, sb'))
    (hS : S < 2 ^ es.length) (ty nloc bodyN bodyLen : Nat) :
    classifyOne SB (lookS S sigs ty) nloc bodyN bodyLen =
      classifyOne SB ((((decodeTypeEntries 0 es).1.toArray)[ty]?).getD none) nloc bodyN bodyLen := by
  obtain ⟨-, -, hall⟩ := entriesOk_spec es 0 SB es.length sb' hW
  rw [List.getElem?_toArray, decodeTypeEntries_fst, List.getElem?_map]
  unfold lookS
  by_cases hty : ty < es.length
  · obtain ⟨hbit, hsig⟩ := hall ty hty
    simp only [Nat.zero_add] at hbit hsig
    rw [List.getElem?_eq_getElem hty]
    simp only [Option.map_some, Option.getD_some]
    cases hb : S.testBit ty
    · rw [hb] at hbit
      simp only [Bool.false_eq_true, ↓reduceIte]
      rw [classifyOne_of_not_shape SB none rfl, classifyOne_of_not_shape SB _ hbit.symm]
    · simp only [↓reduceIte, hsig hb]
  · have hle : es.length ≤ ty := Nat.le_of_not_lt hty
    rw [List.getElem?_eq_none hle, testBit_of_lt hS hle]
    rfl

/-! ### Functions in blocks -/

theorem classifyBy_append (nimp : Nat) (sbat : List Nat) (look : Nat → Option Sig) :
    ∀ (d : Nat) (tys1 tys2 : List Nat) (locs1 locs2 : List (Nat × Nat × Nat)),
      tys1.length = locs1.length →
      classifyBy nimp sbat look d (tys1 ++ tys2) (locs1 ++ locs2) =
        classifyBy nimp sbat look d tys1 locs1 ++
          classifyBy nimp sbat look (d + tys1.length) tys2 locs2
  | _, [], _, [], _, _ => by simp [classifyBy]
  | d, ty :: tys, tys2, (nloc, bodyN, bodyLen) :: locs, locs2, h => by
      simp only [List.length_cons, Nat.add_right_cancel_iff] at h
      simp only [List.cons_append, classifyBy, List.length_cons]
      rw [classifyBy_append nimp sbat look (d + 1) tys tys2 locs locs2 h,
        show d + 1 + tys.length = d + (tys.length + 1) by omega]
      split <;> simp
  | _, [], _, _ :: _, _, h => by simp at h
  | _, _ :: _, _, [], _, h => by simp at h

/-- A function's code entry as the classification reads it. -/
def tripleOf (loc : CodeLoc) : Nat × Nat × Nat := (loc.nlocals, loc.bodyN, loc.bodyLen)

/-- The types and code entries of functions `k, …, k + m - 1`. -/
def fnTys (L : Layout) (k m : Nat) : List Nat := (List.range' k m).map L.ty

def fnLocs (cs : List Nat) (L : Layout) (k m : Nat) : List (Nat × Nat × Nat) :=
  (List.range' k m).map fun j => tripleOf (locOf (window 1024 cs (L.off j) (L.len j), L.len j))

/-- Functions `k, …, k + m - 1` classified by their shape bits. -/
def classifyBlock (cs : List Nat) (L : Layout) (SB : List Nat) (S : Nat)
    (sigs : List (Nat × Sig)) (k m : Nat) : List (Nat × Role) :=
  classifyBy L.imports SB (lookS S sigs) k (fnTys L k m) (fnLocs cs L k m)

/-- Two consecutive blocks of functions. -/
theorem classifyBlock_join {cs : List Nat} {L : Layout} {SB : List Nat} {S : Nat}
    {sigs : List (Nat × Sig)} {k a b : Nat} {r1 r2 : List (Nat × Role)}
    (h1 : classifyBlock cs L SB S sigs k a = r1) (h2 : classifyBlock cs L SB S sigs (k + a) b = r2) :
    classifyBlock cs L SB S sigs k (a + b) = r1 ++ r2 := by
  unfold classifyBlock fnTys fnLocs at *
  rw [← List.range'_append_1, List.map_append, List.map_append,
    classifyBy_append _ _ _ _ _ _ _ _ (by simp), ← h1, ← h2]
  simp

/-! ### The String helper roles -/

/-- The String helper roles, from a walk over the whole type section, the
    shape bits bounded by the type count, the declared layout and the
    functions classified in blocks. -/
theorem roleTable_of_blocks {cs SB : List Nat} {S : Nat} {sigs : List (Nat × Sig)}
    {n len : Nat} {hs : List Nat} {t0 : Nat} {ls : List Nat} {T off' : Nat} {L : Layout}
    {R : List (Nat × Role)}
    (hn : join 1024 cs = n) (hfit : chunksFit 1024 cs = true)
    (hH : typesHead cs len hs t0 ls = true)
    (hW : walkTypes cs SB S sigs 0 t0 ls SB = some (T, off', []))
    (hS : S < 2 ^ T)
    (hL : codeLocs n len = some (codeLocsL cs L)) (hF : funcsOk n len L = true)
    (hC : classifyBlock cs L SB S sigs 0 L.count = R) :
    roleTable n len = some R := by
  obtain ⟨gs, hG, hE⟩ := walkTypes_spec ls 0 t0 SB T off' [] hW
  have hcut := typesCut_of_walk hn hfit hH hG
  have htypes := decodeTypes_of_cut hcut
  obtain ⟨hT, hsb, -⟩ := entriesOk_spec gs.flatten 0 SB T [] hE
  simp only [Nat.zero_add] at hT
  subst hT
  rw [List.append_nil] at hsb
  unfold roleTable
  rw [decodeTypeSigs, htypes, funcImportBase_of_funcsOk hF, decodeFuncTypes_of_funcsOk hF]
  unfold bodyLocs
  rw [hL]
  simp only [Option.map_some, typeInfoOf]
  rw [← hC, ← hsb, classify_eq_by]
  unfold classifyBlock fnTys fnLocs layoutTys codeLocsL
  simp only [typeInfoOf, List.range_eq_range', List.toList_toArray, List.map_map]
  congr 1
  apply classifyBy_congr
  intro ty nloc bodyN bodyLen
  rw [hsb]
  exact (classifyOne_lookS (sb' := []) (by rw [← hsb]; exact hE) hS ty nloc bodyN bodyLen).symm

end AverCert.ScaleTypes

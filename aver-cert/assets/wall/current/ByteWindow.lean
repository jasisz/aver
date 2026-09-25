-- Section cuts: a section decoded one declared entry at a time.
import CertDecode

set_option linter.unusedSimpArgs false

namespace AverCert.ByteWindow
open CertDecode

/-! ### Windows

The decoders read a section as a little-endian numeral `n` and a length, one
byte at a time: each byte read shifts the remaining numeral, so decoding a
section of `S` bytes builds `S` numerals of average size `S / 2`, and the
kernel keeps every one of them until the declaration is checked. A window is
the numeral of one entry, `w < 2 ^ (8 * l)`, followed in the section by the
rest `R`: the section is `w + 2 ^ (8 * l) * R`. The lemmas below show that a
decoder run on a window alone gives what it gives inside the section, so a
section can be cut into its entries and each entry decoded alone. -/

theorem pow_eight (l : Nat) : 2 ^ (8 * (l + 1)) = 256 * 2 ^ (8 * l) := by
  rw [Nat.mul_succ, Nat.pow_add, Nat.mul_comm]

theorem pow_split {k l : Nat} (h : k ≤ l) : 2 ^ (8 * l) = 2 ^ (8 * k) * 2 ^ (8 * (l - k)) := by
  rw [← Nat.pow_add, ← Nat.mul_add, Nat.add_sub_cancel' h]

theorem land_ff (x : Nat) : x &&& 0xff = x % 256 :=
  Nat.and_two_pow_sub_one_eq_mod x 8

theorem byte_ext {w l R : Nat} (hl : l ≠ 0) : (w + 2 ^ (8 * l) * R) &&& 0xff = w &&& 0xff := by
  obtain ⟨l, rfl⟩ := Nat.exists_eq_succ_of_ne_zero hl
  rw [land_ff, land_ff, pow_eight, Nat.mul_assoc, Nat.add_mul_mod_self_left]

theorem shr_ext {w l R : Nat} (k : Nat) (hk : k ≤ l) :
    (w + 2 ^ (8 * l) * R) >>> (8 * k) = (w >>> (8 * k)) + 2 ^ (8 * (l - k)) * R := by
  rw [Nat.shiftRight_eq_div_pow, Nat.shiftRight_eq_div_pow, pow_split hk, Nat.mul_assoc,
    Nat.add_mul_div_left _ _ (Nat.two_pow_pos _)]

theorem shr_lt {w l : Nat} (k : Nat) (hk : k ≤ l) (hw : w < 2 ^ (8 * l)) :
    w >>> (8 * k) < 2 ^ (8 * (l - k)) := by
  rw [Nat.shiftRight_eq_div_pow, Nat.div_lt_iff_lt_mul (Nat.two_pow_pos _), Nat.mul_comm,
    ← pow_split hk]
  exact hw

theorem shr8_ext {w l R : Nat} (hl : l ≠ 0) :
    (w + 2 ^ (8 * l) * R) >>> 8 = (w >>> 8) + 2 ^ (8 * (l - 1)) * R := by
  have := shr_ext (w := w) (R := R) 1 (Nat.one_le_iff_ne_zero.mpr hl)
  simpa using this

theorem shr8_lt {w l : Nat} (hl : l ≠ 0) (hw : w < 2 ^ (8 * l)) : w >>> 8 < 2 ^ (8 * (l - 1)) := by
  have := shr_lt (w := w) 1 (Nat.one_le_iff_ne_zero.mpr hl) hw
  simpa using this

theorem takeBytes_ext : ∀ {k w l R : Nat}, k ≤ l → w < 2 ^ (8 * l) →
    takeBytes k (w + 2 ^ (8 * l) * R) = takeBytes k w
  | 0, _, _, _, _, _ => rfl
  | k + 1, w, l, R, hk, hw => by
      have hl : l ≠ 0 := by omega
      simp only [takeBytes]
      rw [byte_ext hl, shr8_ext hl, takeBytes_ext (by omega) (shr8_lt hl hw)]

theorem isolateBytes_eq (n k : Nat) : isolateBytes n k = n % 2 ^ (8 * k) := by
  unfold isolateBytes
  rw [Nat.shiftLeft_eq, Nat.one_mul, Nat.and_two_pow_sub_one_eq_mod]

theorem isolateBytes_ext {w l R k : Nat} (hk : k ≤ l) :
    isolateBytes (w + 2 ^ (8 * l) * R) k = isolateBytes w k := by
  rw [isolateBytes_eq, isolateBytes_eq, pow_split hk, Nat.mul_assoc, Nat.add_mul_mod_self_left]

theorem isolateBytes_lt (n k : Nat) : isolateBytes n k < 2 ^ (8 * k) := by
  rw [isolateBytes_eq]; exact Nat.mod_lt _ (Nat.two_pow_pos _)

/-- A reader's result on a window is its result inside the section: the same
    value, and the section's rest after the window's rest. -/
def Ext {α : Type} (r : Nat → Nat → Option (α × Nat × Nat)) : Prop :=
  ∀ {w l x w' l'}, w < 2 ^ (8 * l) → r w l = some (x, w', l') →
    w' < 2 ^ (8 * l') ∧ l' ≤ l ∧
      ∀ R L, r (w + 2 ^ (8 * l) * R) (l + L) = some (x, w' + 2 ^ (8 * l') * R, l' + L)

theorem uleb_ext : ∀ (fuel acc sh : Nat), Ext (uleb fuel acc sh)
  | 0, _, _, _, _, _, _, _, _, h => by simp [uleb] at h
  | fuel + 1, acc, sh, w, l, x, w', l', hw, h => by
      unfold uleb at h
      by_cases hl : l = 0
      · simp [hl] at h
      simp only [hl, beq_iff_eq, ite_false] at h
      split at h
      · rename_i hb
        split at h
        · cases h
        · simp only [Option.some.injEq, Prod.mk.injEq] at h
          obtain ⟨rfl, rfl, rfl⟩ := h
          refine ⟨shr8_lt hl hw, by omega, fun R L => ?_⟩
          have hlL : (l + L == 0) = false := by simp; omega
          unfold uleb
          simp only [hlL, Bool.false_eq_true, ite_false, byte_ext hl, shr8_ext hl, hb, ite_true]
          rename_i hc
          simp only [hc, Bool.false_eq_true, ite_false]
          congr 3
          omega
      · rename_i hb
        obtain ⟨hw', hle, hext⟩ := uleb_ext fuel _ _ (shr8_lt hl hw) h
        refine ⟨hw', by omega, fun R L => ?_⟩
        have hlL : (l + L == 0) = false := by simp; omega
        unfold uleb
        simp only [hlL, Bool.false_eq_true, ite_false, byte_ext hl, shr8_ext hl, hb, ite_false]
        have := hext R L
        rw [show l + L - 1 = l - 1 + L by omega]
        exact this

theorem readU_ext : Ext readU := uleb_ext 5 0 0

theorem readName_ext : Ext readName := by
  intro w l x w' l' hw h
  unfold readName at h
  split at h
  · cases h
  · rename_i nameLen b bl hU
    obtain ⟨hb, hbl, hext⟩ := readU_ext hw hU
    split at h
    · rename_i hle
      simp only [Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl, rfl⟩ := h
      refine ⟨shr_lt nameLen hle hb, by omega, fun R L => ?_⟩
      unfold readName
      rw [hext R L]
      simp only [show nameLen ≤ bl + L by omega, ↓reduceIte, takeBytes_ext hle hb,
        shr_ext nameLen hle, Option.some.injEq, Prod.mk.injEq, true_and]
      omega
    · cases h

theorem readExportEntry_ext : Ext readExportEntry := by
  intro w l x w' l' hw h
  unfold readExportEntry at h
  split at h
  · cases h
  · rename_i name n1 l1 hN
    obtain ⟨hn1, hl1, hext⟩ := readName_ext hw hN
    by_cases h0 : l1 = 0
    · simp [h0] at h
    simp only [h0, beq_iff_eq, ↓reduceIte] at h
    split at h
    · cases h
    · rename_i hk
      split at h
      · cases h
      · rename_i idx n2 l2 hU
        obtain ⟨hn2, hl2, hext2⟩ := readU_ext (shr8_lt h0 hn1) hU
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl, rfl⟩ := h
        refine ⟨hn2, by omega, fun R L => ?_⟩
        unfold readExportEntry
        rw [hext R L]
        have hL : (l1 + L == 0) = false := by simp; omega
        simp only [hL, Bool.false_eq_true, ↓reduceIte, byte_ext h0, hk, shr8_ext h0,
          show l1 + L - 1 = l1 - 1 + L by omega, hext2 R L]

/-- A vector of entries read one after another. -/
def vec {α : Type} (r : Nat → Nat → Option (α × Nat × Nat)) : Nat → Nat → Nat → Option (List α × Nat × Nat)
  | 0, n, len => some ([], n, len)
  | k + 1, n, len =>
      match r n len with
      | none => none
      | some (x, n1, len1) =>
          match vec r k n1 len1 with
          | none => none
          | some (rest, n2, len2) => some (x :: rest, n2, len2)

theorem decRawExportVec_eq_vec : ∀ k n len, decRawExportVec k n len = vec readExportEntry k n len
  | 0, _, _ => rfl
  | k + 1, n, len => by
      simp only [decRawExportVec, vec, decRawExportVec_eq_vec k]
      rcases readExportEntry n len with _ | ⟨e, n1, l1⟩
      · rfl
      · simp only []
        rcases vec readExportEntry k n1 l1 with _ | ⟨xs, n2, l2⟩ <;> rfl

theorem vec_ext {α : Type} {r : Nat → Nat → Option (α × Nat × Nat)} (hr : Ext r) (k : Nat) :
    Ext (vec r k) := by
  induction k with
  | zero =>
      intro w l x w' l' hw h
      simp only [vec, Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl, rfl⟩ := h
      exact ⟨hw, Nat.le_refl _, fun _ _ => rfl⟩
  | succ k ih =>
      intro w l x w' l' hw h
      simp only [vec] at h
      split at h
      · cases h
      · rename_i y w1 l1 h1
        split at h
        · cases h
        · rename_i ys w2 l2 h2
          simp only [Option.some.injEq, Prod.mk.injEq] at h
          obtain ⟨rfl, rfl, rfl⟩ := h
          obtain ⟨hw1, hl1, hx1⟩ := hr hw h1
          obtain ⟨hw2, hl2, hx2⟩ := ih hw1 h2
          refine ⟨hw2, by omega, fun R L => ?_⟩
          simp only [vec, hx1 R L, hx2 R L]

/-- A byte-at-a-time step: the byte is the window's, and the rest is the
    window's rest followed by the section's. -/
theorem step_ext {w l R L : Nat} (hl : l ≠ 0) :
    (l + L == 0) = false ∧ (w + 2 ^ (8 * l) * R) &&& 0xff = w &&& 0xff ∧
      (w + 2 ^ (8 * l) * R) >>> 8 = (w >>> 8) + 2 ^ (8 * (l - 1)) * R ∧ l + L - 1 = l - 1 + L := by
  refine ⟨by simp; omega, byte_ext hl, shr8_ext hl, by omega⟩

theorem sleb_ext : ∀ (fuel : Nat) (acc : Int) (sh : Nat), Ext (sleb fuel acc sh)
  | 0, _, _, _, _, _, _, _, _, h => by simp [sleb] at h
  | fuel + 1, acc, sh, w, l, x, w', l', hw, h => by
      unfold sleb at h
      by_cases hl : l = 0
      · simp [hl] at h
      simp only [hl, beq_iff_eq, ite_false] at h
      obtain ⟨h0, hb, hs, hm⟩ := step_ext (R := 0) (L := 0) (w := w) hl
      split at h
      · rename_i hlt
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl, rfl⟩ := h
        refine ⟨shr8_lt hl hw, by omega, fun R L => ?_⟩
        obtain ⟨h0, hb, hs, hm⟩ := step_ext (R := R) (L := L) (w := w) hl
        unfold sleb
        simp only [h0, Bool.false_eq_true, ite_false, hb, hs, hm, hlt, ite_true]
      · rename_i hlt
        obtain ⟨hw', hle, hext⟩ := sleb_ext fuel _ _ (shr8_lt hl hw) h
        refine ⟨hw', by omega, fun R L => ?_⟩
        obtain ⟨h0, hb, hs, hm⟩ := step_ext (R := R) (L := L) (w := w) hl
        unfold sleb
        simp only [h0, Bool.false_eq_true, ite_false, hb, hs, hm, hlt]
        exact hext R L

theorem readS_ext : Ext readS := sleb_ext 10 0 0

theorem readS33_ext : Ext readS33 := by
  intro w l x w' l' hw h
  unfold readS33 at h
  split at h
  · rename_i v w1 l1 h1
    split at h
    · rename_i hr
      simp only [Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl, rfl⟩ := h
      obtain ⟨hw1, hl1, hx⟩ := sleb_ext 5 0 0 hw h1
      refine ⟨hw1, hl1, fun R L => ?_⟩
      unfold readS33
      simp only [hx R L, hr, and_self, ite_true]
    · cases h
  · cases h

theorem readValType_ext : Ext readValType := by
  intro w l x w' l' hw h
  unfold readValType at h
  by_cases hl : l = 0
  · simp [hl] at h
  simp only [hl, beq_iff_eq, ite_false] at h
  split at h
  · rename_i ht
    simp only [Option.some.injEq, Prod.mk.injEq] at h
    obtain ⟨rfl, rfl, rfl⟩ := h
    refine ⟨shr8_lt hl hw, by omega, fun R L => ?_⟩
    obtain ⟨h0, hb, hs, hm⟩ := step_ext (R := R) (L := L) (w := w) hl
    unfold readValType
    simp only [h0, Bool.false_eq_true, ite_false, hb, hs, hm, ht, ite_true]
  · rename_i ht
    split at h
    · rename_i ht2
      split at h
      · rename_i heap w2 l2 h2
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl, rfl⟩ := h
        obtain ⟨hw2, hl2, hx⟩ := readS33_ext (shr8_lt hl hw) h2
        refine ⟨hw2, by omega, fun R L => ?_⟩
        obtain ⟨h0, hb, hs, hm⟩ := step_ext (R := R) (L := L) (w := w) hl
        unfold readValType
        simp only [h0, Bool.false_eq_true, ite_false, hb, hs, hm, ht, ht2, ite_true, hx R L]
      · cases h
    · rename_i ht2
      split at h
      · rename_i ht3
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl, rfl⟩ := h
        refine ⟨shr8_lt hl hw, by omega, fun R L => ?_⟩
        obtain ⟨h0, hb, hs, hm⟩ := step_ext (R := R) (L := L) (w := w) hl
        unfold readValType
        simp only [h0, Bool.false_eq_true, ite_false, hb, hs, hm, ht, ht2, ht3, ite_true]
      · cases h

theorem readValTypes_eq_vec : ∀ k n len, readValTypes k n len = vec readValType k n len
  | 0, _, _ => rfl
  | k + 1, n, len => by
      simp only [readValTypes, vec, readValTypes_eq_vec k]
      rcases readValType n len with _ | ⟨e, n1, l1⟩
      · rfl
      · simp only []
        rcases vec readValType k n1 l1 with _ | ⟨xs, n2, l2⟩ <;> rfl

theorem readValTypes_ext (k : Nat) : Ext (readValTypes k) := by
  intro w l x w' l' hw h
  rw [readValTypes_eq_vec] at h
  obtain ⟨a, b, c⟩ := vec_ext readValType_ext k hw h
  exact ⟨a, b, fun R L => by rw [readValTypes_eq_vec]; exact c R L⟩

theorem readStorageType_ext : Ext readStorageType := by
  intro w l x w' l' hw h
  unfold readStorageType at h
  by_cases hl : l = 0
  · simp [hl] at h
  simp only [hl, beq_iff_eq, ite_false] at h
  split at h
  · rename_i ht
    simp only [Option.some.injEq, Prod.mk.injEq] at h
    obtain ⟨rfl, rfl, rfl⟩ := h
    refine ⟨shr8_lt hl hw, by omega, fun R L => ?_⟩
    obtain ⟨h0, hb, hs, hm⟩ := step_ext (R := R) (L := L) (w := w) hl
    unfold readStorageType
    simp only [h0, Bool.false_eq_true, ite_false, hb, hs, hm, ht, ite_true]
  · rename_i ht
    cases h1 : readValType w l with
    | none => simp [h1] at h
    | some p =>
        obtain ⟨v, w1, l1⟩ := p
        simp only [h1, Option.map_some, Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl, rfl⟩ := h
        obtain ⟨hw1, hl1, hx⟩ := readValType_ext hw h1
        refine ⟨hw1, hl1, fun R L => ?_⟩
        obtain ⟨h0, hb, hs, hm⟩ := step_ext (R := R) (L := L) (w := w) hl
        unfold readStorageType
        simp only [h0, Bool.false_eq_true, ite_false, hb, ht, hx R L, Option.map_some]

theorem readField_ext : Ext readField := by
  intro w l x w' l' hw h
  unfold readField at h
  split at h
  · cases h
  · rename_i st w1 l1 h1
    obtain ⟨hw1, hl1, hx⟩ := readStorageType_ext hw h1
    by_cases hz : l1 = 0
    · simp [hz] at h
    simp only [hz, beq_iff_eq, ite_false] at h
    split at h
    · rename_i hm
      simp only [Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl, rfl⟩ := h
      refine ⟨shr8_lt hz hw1, by omega, fun R L => ?_⟩
      obtain ⟨h0, hb, hs, hm'⟩ := step_ext (R := R) (L := L) (w := w1) hz
      unfold readField
      simp only [hx R L, h0, Bool.false_eq_true, ite_false, hb, hs, hm', hm, ite_true]
    · cases h

theorem readFields_eq_vec : ∀ k n len, readFields k n len = vec readField k n len
  | 0, _, _ => rfl
  | k + 1, n, len => by
      simp only [readFields, vec, readFields_eq_vec k]
      rcases readField n len with _ | ⟨e, n1, l1⟩
      · rfl
      · simp only []
        rcases vec readField k n1 l1 with _ | ⟨xs, n2, l2⟩ <;> rfl

theorem readFields_ext (k : Nat) : Ext (readFields k) := by
  intro w l x w' l' hw h
  rw [readFields_eq_vec] at h
  obtain ⟨a, b, c⟩ := vec_ext readField_ext k hw h
  exact ⟨a, b, fun R L => by rw [readFields_eq_vec]; exact c R L⟩

theorem readUlebs_eq_vec : ∀ k n len, readUlebs k n len = vec readU k n len
  | 0, _, _ => rfl
  | k + 1, n, len => by
      simp only [readUlebs, vec, readUlebs_eq_vec k]
      rcases readU n len with _ | ⟨e, n1, l1⟩
      · rfl
      · simp only []
        rcases vec readU k n1 l1 with _ | ⟨xs, n2, l2⟩ <;> rfl

theorem readUlebs_ext (k : Nat) : Ext (readUlebs k) := by
  intro w l x w' l' hw h
  rw [readUlebs_eq_vec] at h
  obtain ⟨a, b, c⟩ := vec_ext readU_ext k hw h
  exact ⟨a, b, fun R L => by rw [readUlebs_eq_vec]; exact c R L⟩

theorem readCompositeType_ext : Ext readCompositeType := by
  intro w l x w' l' hw h
  unfold readCompositeType at h
  by_cases hl : l = 0
  · simp [hl] at h
  simp only [hl, beq_iff_eq, ite_false] at h
  have hw0 := shr8_lt hl hw
  split at h
  · rename_i ht
    split at h
    · cases h
    · rename_i np w2 l2 h2
      obtain ⟨hw2, hl2, hx2⟩ := readU_ext hw0 h2
      split at h
      · cases h
      · rename_i ps w3 l3 h3
        obtain ⟨hw3, hl3, hx3⟩ := readValTypes_ext np hw2 h3
        split at h
        · cases h
        · rename_i nr w4 l4 h4
          obtain ⟨hw4, hl4, hx4⟩ := readU_ext hw3 h4
          split at h
          · rename_i rs w5 l5 h5
            obtain ⟨hw5, hl5, hx5⟩ := readValTypes_ext nr hw4 h5
            simp only [Option.some.injEq, Prod.mk.injEq] at h
            obtain ⟨rfl, rfl, rfl⟩ := h
            refine ⟨hw5, by omega, fun R L => ?_⟩
            obtain ⟨h0, hb, hs, hm⟩ := step_ext (R := R) (L := L) (w := w) hl
            unfold readCompositeType
            simp only [h0, Bool.false_eq_true, ite_false, hb, hs, hm, ht, ite_true, beq_self_eq_true, ↓reduceIte, Nat.reduceBEq, hx2 R L,
              hx3 R L, hx4 R L, hx5 R L]
          · cases h
  · rename_i ht
    split at h
    · rename_i ht2
      split at h
      · cases h
      · rename_i nf w2 l2 h2
        obtain ⟨hw2, hl2, hx2⟩ := readU_ext hw0 h2
        split at h
        · rename_i fs w3 l3 h3
          obtain ⟨hw3, hl3, hx3⟩ := readFields_ext nf hw2 h3
          simp only [Option.some.injEq, Prod.mk.injEq] at h
          obtain ⟨rfl, rfl, rfl⟩ := h
          refine ⟨hw3, by omega, fun R L => ?_⟩
          obtain ⟨h0, hb, hs, hm⟩ := step_ext (R := R) (L := L) (w := w) hl
          unfold readCompositeType
          simp only [h0, Bool.false_eq_true, ite_false, hb, hs, hm, ht, ht2, ite_true, beq_self_eq_true, ↓reduceIte, Nat.reduceBEq, hx2 R L,
            hx3 R L]
        · cases h
    · rename_i ht2
      split at h
      · rename_i ht3
        split at h
        · rename_i fd w2 l2 h2
          obtain ⟨hw2, hl2, hx2⟩ := readField_ext hw0 h2
          simp only [Option.some.injEq, Prod.mk.injEq] at h
          obtain ⟨rfl, rfl, rfl⟩ := h
          refine ⟨hw2, by omega, fun R L => ?_⟩
          obtain ⟨h0, hb, hs, hm⟩ := step_ext (R := R) (L := L) (w := w) hl
          unfold readCompositeType
          simp only [h0, Bool.false_eq_true, ite_false, hb, hs, hm, ht, ht2, ht3, ite_true, beq_self_eq_true, ↓reduceIte, Nat.reduceBEq,
            hx2 R L]
        · cases h
      · cases h

theorem readSubtypeForm_ext : Ext readSubtypeForm := by
  intro w l x w' l' hw h
  unfold readSubtypeForm at h
  by_cases hl : l = 0
  · simp [hl] at h
  simp only [hl, beq_iff_eq, ite_false] at h
  split at h
  · rename_i ht
    split at h
    · cases h
    · rename_i cnt w1 l1 h1
      obtain ⟨hw1, hl1, hx1⟩ := readU_ext (shr8_lt hl hw) h1
      split at h
      · cases h
      · rename_i sup w2 l2 h2
        obtain ⟨hw2, hl2, hx2⟩ := readUlebs_ext cnt hw1 h2
        have key : ∀ R L, readSubtypeForm (w + 2 ^ (8 * l) * R) (l + L) =
            if (w &&& 0xff) == 0x50 then some (.sub sup, w2 + 2 ^ (8 * l2) * R, l2 + L)
            else some (.subFinal sup, w2 + 2 ^ (8 * l2) * R, l2 + L) := by
          intro R L
          obtain ⟨h0, hb, hs, hm⟩ := step_ext (R := R) (L := L) (w := w) hl
          unfold readSubtypeForm
          simp only [h0, Bool.false_eq_true, ite_false, hb, hs, hm, ht, ite_true, hx1 R L, hx2 R L]
        split at h
        · rename_i h50
          simp only [Option.some.injEq, Prod.mk.injEq] at h
          obtain ⟨rfl, rfl, rfl⟩ := h
          exact ⟨hw2, by omega, fun R L => by rw [key R L]; simp [h50]⟩
        · rename_i h50
          simp only [Option.some.injEq, Prod.mk.injEq] at h
          obtain ⟨rfl, rfl, rfl⟩ := h
          exact ⟨hw2, by omega, fun R L => by rw [key R L]; simp [h50]⟩
  · rename_i ht
    simp only [Option.some.injEq, Prod.mk.injEq] at h
    obtain ⟨rfl, rfl, rfl⟩ := h
    refine ⟨hw, Nat.le_refl _, fun R L => ?_⟩
    obtain ⟨h0, hb, hs, hm⟩ := step_ext (R := R) (L := L) (w := w) hl
    unfold readSubtypeForm
    simp only [h0, Bool.false_eq_true, ite_false, hb, ht]

theorem readTypeEntry_ext : Ext readTypeEntry := by
  intro w l x w' l' hw h
  unfold readTypeEntry at h
  split at h
  · cases h
  · rename_i form w1 l1 h1
    obtain ⟨hw1, hl1, hx1⟩ := readSubtypeForm_ext hw h1
    split at h
    · cases h
    · rename_i comp w2 l2 h2
      obtain ⟨hw2, hl2, hx2⟩ := readCompositeType_ext hw1 h2
      simp only [Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl, rfl⟩ := h
      refine ⟨hw2, by omega, fun R L => ?_⟩
      unfold readTypeEntry
      simp only [hx1 R L, hx2 R L]

theorem readTypeEntries_eq_vec : ∀ k n len, readTypeEntries k n len = vec readTypeEntry k n len
  | 0, _, _ => rfl
  | k + 1, n, len => by
      simp only [readTypeEntries, vec, readTypeEntries_eq_vec k]
      rcases readTypeEntry n len with _ | ⟨e, n1, l1⟩
      · rfl
      · simp only []
        rcases vec readTypeEntry k n1 l1 with _ | ⟨xs, n2, l2⟩ <;> rfl

theorem readTypeEntries_ext (k : Nat) : Ext (readTypeEntries k) := by
  intro w l x w' l' hw h
  rw [readTypeEntries_eq_vec] at h
  obtain ⟨a, b, c⟩ := vec_ext readTypeEntry_ext k hw h
  exact ⟨a, b, fun R L => by rw [readTypeEntries_eq_vec]; exact c R L⟩

/-- One entry of the type section: a rec group, or a single subtype. -/
def readRecEntry (n len : Nat) : Option (List TypeEntry × Nat × Nat) :=
  if len == 0 then none else
    if (n &&& 0xff) == 0x4e then
      match readU (n >>> 8) (len - 1) with
      | none => none
      | some (count, n1, len1) => readTypeEntries count n1 len1
    else
      (readTypeEntry n len).map (fun p => ([p.1], p.2.1, p.2.2))

theorem readRecEntry_ext : Ext readRecEntry := by
  intro w l x w' l' hw h
  unfold readRecEntry at h
  by_cases hl : l = 0
  · simp [hl] at h
  simp only [hl, beq_iff_eq, ite_false] at h
  split at h
  · rename_i ht
    split at h
    · cases h
    · rename_i cnt w1 l1 h1
      obtain ⟨hw1, hl1, hx1⟩ := readU_ext (shr8_lt hl hw) h1
      obtain ⟨hw2, hl2, hx2⟩ := readTypeEntries_ext cnt hw1 h
      refine ⟨hw2, by omega, fun R L => ?_⟩
      obtain ⟨h0, hb, hs, hm⟩ := step_ext (R := R) (L := L) (w := w) hl
      unfold readRecEntry
      simp only [h0, Bool.false_eq_true, ite_false, hb, hs, hm, ht, ite_true, beq_self_eq_true, ↓reduceIte, hx1 R L, hx2 R L]
  · rename_i ht
    cases h1 : readTypeEntry w l with
    | none => simp [h1] at h
    | some p =>
        obtain ⟨e, w1, l1⟩ := p
        simp only [h1, Option.map_some, Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl, rfl⟩ := h
        obtain ⟨hw1, hl1, hx⟩ := readTypeEntry_ext hw h1
        refine ⟨hw1, hl1, fun R L => ?_⟩
        obtain ⟨h0, hb, hs, hm⟩ := step_ext (R := R) (L := L) (w := w) hl
        unfold readRecEntry
        simp only [h0, Bool.false_eq_true, ite_false, hb, beq_iff_eq, ht, ↓reduceIte, hx R L, Option.map_some]

/-! ### Cutting a section at declared entry lengths -/

/-- The windows of the entries of the given lengths, one after another. -/
def seqWin (P : Nat) : List Nat → List (Nat × Nat)
  | [] => []
  | l :: ls => (P % 2 ^ (8 * l), l) :: seqWin (P >>> (8 * l)) ls

/-- A window decoded alone, consuming it exactly. -/
def whole {α : Type} (r : Nat → Nat → Option (α × Nat × Nat)) (win : Nat × Nat) : Option α :=
  match r win.1 win.2 with
  | some (x, _, 0) => some x
  | _ => none

theorem split_pow (P l : Nat) : P % 2 ^ (8 * l) + 2 ^ (8 * l) * (P >>> (8 * l)) = P := by
  rw [Nat.shiftRight_eq_div_pow]; exact Nat.mod_add_div P _

/-- Each window decoded alone and consumed exactly: the vector reads the same
    entries inside the section, and stops after the last window. -/
theorem vec_of_windows {α : Type} {r : Nat → Nat → Option (α × Nat × Nat)} (hr : Ext r) :
    ∀ (ls : List Nat) (P S : Nat) (xs : List α), ls.sum ≤ S →
      (seqWin P ls).mapM (whole r) = some xs →
      vec r ls.length P S = some (xs, P >>> (8 * ls.sum), S - ls.sum)
  | [], P, S, xs, _, h => by
      simp only [seqWin, List.mapM_nil, Option.pure_def, Option.some.injEq] at h
      subst h; simp [vec]
  | l :: ls, P, S, xs, hS, h => by
      simp only [seqWin, List.mapM_cons, Option.bind_eq_bind, Option.pure_def] at h
      cases hx : whole r (P % 2 ^ (8 * l), l) with
      | none => simp [hx] at h
      | some x =>
          cases hxs : (seqWin (P >>> (8 * l)) ls).mapM (whole r) with
          | none => simp [hx, hxs] at h
          | some ys =>
              simp only [hx, hxs, Option.bind_some, Option.some.injEq] at h
              subst h
              simp only [List.sum_cons] at hS
              unfold whole at hx
              split at hx
              · rename_i x' w' hw
                simp only [Option.some.injEq] at hx
                subst hx
                obtain ⟨hw', _, hext⟩ := hr (Nat.mod_lt _ (Nat.two_pow_pos _)) hw
                have hw0 : w' = 0 := by simpa using hw'
                subst hw0
                have hstep := hext (P >>> (8 * l)) (S - l)
                rw [split_pow, show l + (S - l) = S by omega] at hstep
                simp only [Nat.mul_zero, Nat.pow_zero, Nat.one_mul, Nat.zero_add] at hstep
                have hrest := vec_of_windows hr ls (P >>> (8 * l)) (S - l) ys (by omega) hxs
                simp only [List.length_cons, vec, hstep, hrest, List.sum_cons,
                  Nat.shiftRight_add, Option.some.injEq, Prod.mk.injEq, true_and, Nat.mul_add]
                omega
              · cases hx

theorem mod_mod_pow {P l a : Nat} (h : l ≤ a) : P % 2 ^ (8 * a) % 2 ^ (8 * l) = P % 2 ^ (8 * l) :=
  Nat.mod_mod_of_dvd _ (Nat.pow_dvd_pow 2 (Nat.mul_le_mul_left 8 h))

theorem mod_shr_pow {P l a : Nat} (h : l ≤ a) :
    (P % 2 ^ (8 * a)) >>> (8 * l) = (P >>> (8 * l)) % 2 ^ (8 * (a - l)) := by
  rw [Nat.shiftRight_eq_div_pow, Nat.shiftRight_eq_div_pow, pow_split h, Nat.mod_mul_right_div_self]

theorem seqWin_mod : ∀ (ls : List Nat) (P a : Nat), ls.sum ≤ a → seqWin (P % 2 ^ (8 * a)) ls = seqWin P ls
  | [], _, _, _ => rfl
  | l :: ls, P, a, h => by
      simp only [List.sum_cons] at h
      simp only [seqWin, mod_mod_pow (show l ≤ a by omega), mod_shr_pow (show l ≤ a by omega),
        seqWin_mod ls (P >>> (8 * l)) (a - l) (by omega)]

theorem seqWin_append : ∀ (xs ys : List Nat) (P : Nat),
    seqWin P (xs ++ ys) = seqWin (P % 2 ^ (8 * xs.sum)) xs ++ seqWin (P >>> (8 * xs.sum)) ys
  | [], ys, P => by simp [seqWin]
  | x :: xs, ys, P => by
      simp only [List.cons_append, seqWin, List.sum_cons, seqWin_append xs ys, List.cons.injEq]
      refine ⟨by rw [mod_mod_pow (by omega)], ?_⟩
      rw [mod_shr_pow (show x ≤ x + xs.sum by omega), Nat.add_sub_cancel_left,
        Nat.mul_add, Nat.shiftRight_add]

/-- `seqWin`, a group of entries at a time: the kernel shifts the rest of the
    section once per group, and each entry's window out of its group's. -/
def cutWin : Nat → Nat → List Nat → List (Nat × Nat)
  | 0, P, ls => seqWin P ls
  | f + 1, P, ls =>
      match ls with
      | [] => []
      | _ :: _ =>
          seqWin (P % 2 ^ (8 * (ls.take 64).sum)) (ls.take 64) ++
            cutWin f (P >>> (8 * (ls.take 64).sum)) (ls.drop 64)

theorem cutWin_eq : ∀ (f P : Nat) (ls : List Nat), cutWin f P ls = seqWin P ls
  | 0, _, _ => rfl
  | f + 1, P, [] => rfl
  | f + 1, P, l :: ls => by
      unfold cutWin
      simp only
      rw [cutWin_eq f, ← seqWin_append, List.take_append_drop]


/-! ### The export section, cut -/

/-- The export entries, read one declared window at a time. -/
def decodeRawExportsCut (n len : Nat) (ls : List Nat) : Option (List ExportEntry) :=
  match modulePayload 7 n len with
  | none => none
  | some (eN, eLen) =>
      match readU eN eLen with
      | none => none
      | some (cnt, n1, len1) =>
          if cnt == ls.length && ls.sum == len1 then
            (cutWin ls.length n1 ls).mapM (whole readExportEntry)
          else none

theorem decodeRawExports_of_cut {n len : Nat} {ls : List Nat} {E : List ExportEntry}
    (h : decodeRawExportsCut n len ls = some E) : decodeRawExports n len = some E := by
  unfold decodeRawExportsCut at h
  unfold decodeRawExports
  split at h
  · cases h
  · rename_i eN eLen hP
    rw [hP]
    split at h
    · cases h
    · rename_i cnt n1 len1 hU
      simp only [hU]
      split at h
      · rename_i hc
        simp only [Bool.and_eq_true, beq_iff_eq] at hc
        obtain ⟨rfl, hsum⟩ := hc
        rw [cutWin_eq] at h
        have hv := vec_of_windows readExportEntry_ext ls n1 len1 E (by omega) h
        rw [decRawExportVec_eq_vec, hv, hsum, Nat.sub_self]
        rfl
      · cases h

theorem decodeRawExports_eq_cut {n len : Nat} {ls : List Nat}
    (h : (decodeRawExportsCut n len ls).isSome = true) :
    decodeRawExports n len = decodeRawExportsCut n len ls := by
  obtain ⟨E, hE⟩ := Option.isSome_iff_exists.mp h
  rw [hE, decodeRawExports_of_cut hE]

/-! ### The type section, cut -/

theorem decRecVec_eq_vec : ∀ k n len,
    decRecVec k n len = (vec readRecEntry k n len).map (fun p => (p.1.flatten, p.2.1, p.2.2))
  | 0, _, _ => rfl
  | k + 1, n, len => by
      unfold decRecVec
      simp only [vec, readRecEntry, decRecVec_eq_vec k]
      by_cases hl : len = 0
      · simp [hl]
      simp only [hl, beq_iff_eq, ↓reduceIte]
      split
      · rcases readU (n >>> 8) (len - 1) with _ | ⟨c, n1, l1⟩
        · rfl
        · simp only []
          rcases readTypeEntries c n1 l1 with _ | ⟨g, n2, l2⟩
          · rfl
          · simp only []
            rcases vec readRecEntry k n2 l2 with _ | ⟨gs, n3, l3⟩ <;> rfl
      · rcases readTypeEntry n len with _ | ⟨e, n1, l1⟩
        · rfl
        · simp only [Option.map_some]
          rcases vec readRecEntry k n1 l1 with _ | ⟨gs, n3, l3⟩ <;> rfl

/-- The type information of a flat entry list, built as `decodeTypes` builds it. -/
def typeInfoOf (entries : List TypeEntry) : TypeInfo :=
  { nfields := entries.map TypeEntry.fieldCount
  , arityIndex := (entries.map TypeEntry.arity).toArray
  , nfieldIndex := (entries.map TypeEntry.fieldCount).toArray
  , carrier := firstCarrier 0 entries
  , entries := entries
  , entryIndex := entries.toArray }

/-- The type section, read one declared window (rec group or subtype) at a time. -/
def decodeTypesCut (n len : Nat) (ls : List Nat) : Option TypeInfo :=
  match modulePayload 1 n len with
  | none => none
  | some (tN, tLen) =>
      match readU tN tLen with
      | none => none
      | some (count, n1, len1) =>
          if count == ls.length && ls.sum == len1 then
            ((cutWin ls.length n1 ls).mapM (whole readRecEntry)).map
              (fun groups => typeInfoOf groups.flatten)
          else none

theorem decodeTypes_of_cut {n len : Nat} {ls : List Nat} {T : TypeInfo}
    (h : decodeTypesCut n len ls = some T) : decodeTypes n len = some T := by
  unfold decodeTypesCut at h
  unfold decodeTypes
  split at h
  · cases h
  · rename_i tN tLen hP
    rw [hP]
    split at h
    · cases h
    · rename_i cnt n1 len1 hU
      simp only [hU]
      split at h
      · rename_i hc
        simp only [Bool.and_eq_true, beq_iff_eq] at hc
        obtain ⟨rfl, hsum⟩ := hc
        rw [cutWin_eq] at h
        cases hg : (seqWin n1 ls).mapM (whole readRecEntry) with
        | none => simp [hg] at h
        | some gs =>
            simp only [hg, Option.map_some, Option.some.injEq] at h
            subst h
            have hv := vec_of_windows readRecEntry_ext ls n1 len1 gs (by omega) hg
            rw [decRecVec_eq_vec, hv, hsum, Nat.sub_self]
            rfl
      · cases h

theorem decodeTypes_eq_cut {n len : Nat} {ls : List Nat}
    (h : (decodeTypesCut n len ls).isSome = true) :
    decodeTypes n len = decodeTypesCut n len ls := by
  obtain ⟨T, hT⟩ := Option.isSome_iff_exists.mp h
  rw [hT, decodeTypes_of_cut hT]

/-! ### The code section, cut -/

/-- One code entry, as `decCodeLocs` reads it. -/
def readCodeEntry (n len : Nat) : Option (CodeLoc × Nat × Nat) :=
  match readU n len with
  | none => none
  | some (esz, bN, bLen) =>
      if esz ≤ bLen then
        match readU (isolateBytes bN esz) esz with
        | none => none
        | some (ng, gN, gLen) =>
            match decLocals ng gN gLen with
            | none => none
            | some (nloc, bodyN, bodyLen) =>
                some (⟨nloc, isolateBytes bodyN bodyLen, bodyLen,
                  isolateBytes n (len - bLen + esz), len - bLen + esz⟩, bN >>> (8 * esz), bLen - esz)
      else none

theorem decCodeLocs_eq_vec : ∀ k n len,
    decCodeLocs k n len = (vec readCodeEntry k n len).bind
      (fun p => if p.2.2 == 0 then some p.1 else none)
  | 0, _, _ => rfl
  | k + 1, n, len => by
      unfold decCodeLocs
      simp only [vec, readCodeEntry, decCodeLocs_eq_vec k]
      rcases readU n len with _ | ⟨esz, bN, bLen⟩
      · rfl
      · simp only []
        split
        · rcases readU (isolateBytes bN esz) esz with _ | ⟨ng, gN, gLen⟩
          · rfl
          · simp only []
            rcases decLocals ng gN gLen with _ | ⟨nloc, bodyN, bodyLen⟩
            · rfl
            · simp only []
              rcases vec readCodeEntry k (bN >>> (8 * esz)) (bLen - esz) with _ | ⟨ls, n3, l3⟩
              · rfl
              · simp only [Option.bind_some]
                by_cases h3 : l3 = 0 <;> simp [h3]
        · rfl

theorem readCodeEntry_ext : Ext readCodeEntry := by
  intro w l x w' l' hw h
  unfold readCodeEntry at h
  split at h
  · cases h
  · rename_i esz bN bLen hU
    obtain ⟨hb, hbl, hx⟩ := readU_ext hw hU
    have hl : l ≠ 0 := by
      intro h0; subst h0; simp [readU, uleb] at hU
    have hbl' : bLen < l := by
      have := hU
      unfold readU uleb at this
      simp only [hl, beq_iff_eq, ite_false] at this
      split at this
      · split at this
        · cases this
        · simp only [Option.some.injEq, Prod.mk.injEq] at this; omega
      · have := (uleb_ext 4 _ 7 (shr8_lt hl hw) this).2.1; omega
    split at h
    · rename_i hle
      split at h
      · cases h
      · rename_i ng gN gLen hG
        split at h
        · cases h
        · rename_i nloc bodyN bodyLen hL
          simp only [Option.some.injEq, Prod.mk.injEq] at h
          obtain ⟨rfl, rfl, rfl⟩ := h
          refine ⟨shr_lt esz hle hb, by omega, fun R L => ?_⟩
          unfold readCodeEntry
          rw [hx R L]
          simp only [show esz ≤ bLen + L by omega, ↓reduceIte, isolateBytes_ext hle, hG, hL,
            Option.some.injEq, Prod.mk.injEq, shr_ext esz hle]
          refine ⟨?_, ?_, by omega⟩
          · rw [show l + L - (bLen + L) + esz = l - bLen + esz by omega,
              isolateBytes_ext (by omega)]
          · trivial
    · cases h

/-- The code section, read one declared entry at a time. -/
def codeLocsCut (n len : Nat) (ls : List Nat) : Option (Array CodeLoc) :=
  match modulePayload 10 n len with
  | none => none
  | some (codeN, codeLen) =>
      match readU codeN codeLen with
      | none => none
      | some (nf, r0, l0) =>
          if nf == ls.length && ls.sum == l0 then
            ((cutWin ls.length r0 ls).mapM (whole readCodeEntry)).map List.toArray
          else none

theorem codeLocs_of_cut {n len : Nat} {ls : List Nat} {C : Array CodeLoc}
    (h : codeLocsCut n len ls = some C) : codeLocs n len = some C := by
  unfold codeLocsCut at h
  unfold codeLocs
  split at h
  · cases h
  · rename_i cN cLen hP
    rw [hP]
    split at h
    · cases h
    · rename_i nf r0 l0 hU
      simp only [hU]
      split at h
      · rename_i hc
        simp only [Bool.and_eq_true, beq_iff_eq] at hc
        obtain ⟨rfl, hsum⟩ := hc
        rw [cutWin_eq] at h
        cases hg : (seqWin r0 ls).mapM (whole readCodeEntry) with
        | none => simp [hg] at h
        | some locs =>
            simp only [hg, Option.map_some, Option.some.injEq] at h
            subst h
            have hv := vec_of_windows readCodeEntry_ext ls r0 l0 locs (by omega) hg
            rw [decCodeLocs_eq_vec, hv, hsum, Nat.sub_self]
            rfl
      · cases h

theorem codeLocs_eq_cut {n len : Nat} {ls : List Nat}
    (h : (codeLocsCut n len ls).isSome = true) :
    codeLocs n len = codeLocsCut n len ls := by
  obtain ⟨C, hC⟩ := Option.isSome_iff_exists.mp h
  rw [hC, codeLocs_of_cut hC]

/-! ### Reading a confirmed cut lazily

Once one declaration has confirmed a cut (every window decodes alone and is
consumed exactly), the section is the list of its windows' entries. The lazy
forms below build that list without re-checking any window: the kernel
decodes a window only when a check reads its entry, so a check that reads a
few entries of a section decodes only those. -/

theorem seqWin_bound : ∀ (P : Nat) (ls : List Nat) (w : Nat × Nat), w ∈ seqWin P ls →
    w.1 < 2 ^ (8 * w.2)
  | _, [], _, h => by cases h
  | P, l :: ls, w, h => by
      simp only [seqWin, List.mem_cons] at h
      rcases h with rfl | h
      · exact Nat.mod_lt _ (Nat.two_pow_pos _)
      · exact seqWin_bound _ ls w h

/-- A checked `mapM` over windows is the plain `map` of any reading that agrees
    with it on every window it accepts. -/
theorem mapM_eq_map {α : Type} {g : Nat × Nat → Option α} {f : Nat × Nat → α} :
    ∀ (ws : List (Nat × Nat)) (xs : List α),
      (∀ w ∈ ws, ∀ x, g w = some x → f w = x) → ws.mapM g = some xs → ws.map f = xs
  | [], xs, _, h => by simpa using h
  | w :: ws, xs, hf, h => by
      simp only [List.mapM_cons, Option.bind_eq_bind, Option.pure_def] at h
      cases hw : g w with
      | none => simp [hw] at h
      | some x =>
          cases hws : ws.mapM g with
          | none => simp [hw, hws] at h
          | some ys =>
              simp only [hw, hws, Option.bind_some, Option.some.injEq] at h
              subst h
              simp only [List.map_cons, List.cons.injEq]
              exact ⟨hf w (List.mem_cons_self) x hw,
                mapM_eq_map ws ys (fun v hv => hf v (List.mem_cons_of_mem _ hv)) hws⟩

/-- A window's entry, read without checking the window. -/
def entryOf {α : Type} (r : Nat → Nat → Option (α × Nat × Nat)) (d : α) (w : Nat × Nat) : α :=
  (whole r w).getD d

theorem entryOf_eq {α : Type} {r : Nat → Nat → Option (α × Nat × Nat)} {d : α} {w : Nat × Nat}
    {x : α} (h : whole r w = some x) : entryOf r d w = x := by
  simp [entryOf, h]

def noExport : ExportEntry := ⟨[], 0, 0⟩

/-- The export entries of a cut, read lazily. -/
def exportsLazy (n len : Nat) (ls : List Nat) : Option (List ExportEntry) :=
  match modulePayload 7 n len with
  | none => none
  | some (eN, eLen) =>
      match readU eN eLen with
      | none => none
      | some (cnt, n1, len1) =>
          if cnt == ls.length && ls.sum == len1 then
            some ((cutWin ls.length n1 ls).map (entryOf readExportEntry noExport))
          else none

theorem exportsLazy_of_cut {n len : Nat} {ls : List Nat} {E : List ExportEntry}
    (h : decodeRawExportsCut n len ls = some E) : exportsLazy n len ls = some E := by
  unfold decodeRawExportsCut at h
  unfold exportsLazy
  split at h
  · cases h
  · rename_i eN eLen hP
    try rw [hP]
    split at h
    · cases h
    · rename_i cnt n1 len1 hU
      try simp only [hU]
      split at h
      · rename_i hc
        simp only [hc, ↓reduceIte, Option.some.injEq]
        exact mapM_eq_map _ E (fun w _ x hx => entryOf_eq hx) h
      · cases h

/-- A confirmed export cut: the export section is its lazily read windows. -/
theorem decodeRawExports_eq_lazy {n len : Nat} {ls : List Nat}
    (h : (decodeRawExportsCut n len ls).isSome = true) :
    decodeRawExports n len = exportsLazy n len ls := by
  obtain ⟨E, hE⟩ := Option.isSome_iff_exists.mp h
  rw [decodeRawExports_of_cut hE, exportsLazy_of_cut hE]

def noTypeEntry : TypeEntry := ⟨.plain, .structType []⟩

/-- A window of the type section read lazily: a rec group in full, a single
    subtype as its one entry. -/
def groupOf (w : Nat × Nat) : List TypeEntry :=
  if (w.1 &&& 0xff) == 0x4e then (whole readRecEntry w).getD []
  else [entryOf readTypeEntry noTypeEntry w]

theorem groupOf_eq {w : Nat × Nat} {g : List TypeEntry} (h : whole readRecEntry w = some g) :
    groupOf w = g := by
  unfold groupOf
  split
  · simp [h]
  · rename_i hb
    unfold whole readRecEntry at h
    by_cases hl : w.2 = 0
    · simp [hl] at h
    simp only [hl, beq_iff_eq, ↓reduceIte] at h
    simp only [beq_iff_eq] at hb
    simp only [hb, ↓reduceIte] at h
    cases he : readTypeEntry w.1 w.2 with
    | none => simp [he] at h
    | some p =>
        obtain ⟨e, w1, l1⟩ := p
        simp only [he, Option.map_some] at h
        split at h
        · rename_i x w' heq
          simp only [Option.some.injEq, Prod.mk.injEq] at heq h
          obtain ⟨rfl, -, rfl⟩ := heq
          subst h
          have : whole readTypeEntry w = some e := by simp [whole, he]
          simp [entryOf_eq this]
        · cases h

/-- The type section of a cut, read lazily. -/
def typesLazy (n len : Nat) (ls : List Nat) : Option TypeInfo :=
  match modulePayload 1 n len with
  | none => none
  | some (tN, tLen) =>
      match readU tN tLen with
      | none => none
      | some (count, n1, len1) =>
          if count == ls.length && ls.sum == len1 then
            some (typeInfoOf ((cutWin ls.length n1 ls).map groupOf).flatten)
          else none

theorem typesLazy_of_cut {n len : Nat} {ls : List Nat} {T : TypeInfo}
    (h : decodeTypesCut n len ls = some T) : typesLazy n len ls = some T := by
  unfold decodeTypesCut at h
  unfold typesLazy
  split at h
  · cases h
  · rename_i tN tLen hP
    try rw [hP]
    split at h
    · cases h
    · rename_i cnt n1 len1 hU
      try simp only [hU]
      split at h
      · rename_i hc
        simp only [hc, ↓reduceIte]
        cases hg : (cutWin ls.length n1 ls).mapM (whole readRecEntry) with
        | none => simp [hg] at h
        | some gs =>
            simp only [hg, Option.map_some, Option.some.injEq] at h
            rw [mapM_eq_map _ gs (fun w _ g hx => groupOf_eq hx) hg, h]
      · cases h

/-- A confirmed type cut: the type section is its lazily read windows. -/
theorem decodeTypes_eq_lazy {n len : Nat} {ls : List Nat}
    (h : (decodeTypesCut n len ls).isSome = true) :
    decodeTypes n len = typesLazy n len ls := by
  obtain ⟨T, hT⟩ := Option.isSome_iff_exists.mp h
  rw [decodeTypes_of_cut hT, typesLazy_of_cut hT]

def noCodeLoc : CodeLoc := ⟨0, 0, 0, 0, 0⟩

/-- A code entry read lazily: the window is the whole entry, and its locals
    and body are decoded only when a check reads them. -/
def locOf (w : Nat × Nat) : CodeLoc :=
  let loc := entryOf readCodeEntry noCodeLoc w
  ⟨loc.nlocals, loc.bodyN, loc.bodyLen, w.1, w.2⟩

theorem locOf_eq {w : Nat × Nat} {loc : CodeLoc} (hw : w.1 < 2 ^ (8 * w.2))
    (h : whole readCodeEntry w = some loc) : locOf w = loc := by
  have he := entryOf_eq (d := noCodeLoc) h
  unfold locOf
  rw [he]
  unfold whole readCodeEntry at h
  split at h
  · rename_i x w' heq
    split at heq
    · cases heq
    · rename_i esz bN bLen hU
      split at heq
      · rename_i hle
        split at heq
        · cases heq
        · split at heq
          · cases heq
          · simp only [Option.some.injEq, Prod.mk.injEq] at heq h
            obtain ⟨rfl, -, hz⟩ := heq
            subst h
            have hlen : w.2 - bLen + esz = w.2 := by
              have := (readU_ext hw hU).2.1; omega
            simp only [hlen, isolateBytes_eq, Nat.mod_eq_of_lt hw]
      · cases heq
  · cases h

/-- The code section of a cut, read lazily. -/
def codeLazy (n len : Nat) (ls : List Nat) : Option (Array CodeLoc) :=
  match modulePayload 10 n len with
  | none => none
  | some (codeN, codeLen) =>
      match readU codeN codeLen with
      | none => none
      | some (nf, r0, l0) =>
          if nf == ls.length && ls.sum == l0 then
            some ((cutWin ls.length r0 ls).map locOf).toArray
          else none

theorem codeLazy_of_cut {n len : Nat} {ls : List Nat} {C : Array CodeLoc}
    (h : codeLocsCut n len ls = some C) : codeLazy n len ls = some C := by
  unfold codeLocsCut at h
  unfold codeLazy
  split at h
  · cases h
  · rename_i cN cLen hP
    try rw [hP]
    split at h
    · cases h
    · rename_i nf r0 l0 hU
      try simp only [hU]
      split at h
      · rename_i hc
        simp only [hc, ↓reduceIte]
        cases hg : (cutWin ls.length r0 ls).mapM (whole readCodeEntry) with
        | none => simp [hg] at h
        | some locs =>
            simp only [hg, Option.map_some, Option.some.injEq] at h
            have hb : ∀ w ∈ cutWin ls.length r0 ls, w.1 < 2 ^ (8 * w.2) := by
              rw [cutWin_eq]; exact seqWin_bound r0 ls
            rw [mapM_eq_map _ locs (fun w hw loc hx => locOf_eq (hb w hw) hx) hg, h]
      · cases h

/-- A confirmed code cut: the code section is its lazily read windows. -/
theorem codeLocs_eq_lazy {n len : Nat} {ls : List Nat}
    (h : (codeLocsCut n len ls).isSome = true) :
    codeLocs n len = codeLazy n len ls := by
  obtain ⟨C, hC⟩ := Option.isSome_iff_exists.mp h
  rw [codeLocs_of_cut hC, codeLazy_of_cut hC]

end AverCert.ByteWindow

import AverCommon

/-!
Candidate lemma kit for Aver's own Lean prelude. Core Lean 4.34 only: no
Mathlib, every lemma proved here, no `sorry`, no extra axioms. Each lemma is
stated over the shape of a claim, not over a user function, so the exporter
can cite it whenever that shape turns up.
-/

set_option autoImplicit false

namespace AverKit

/-! ## Signs of products (nonlinear `Int`) -/

/-- A square is never negative. -/
theorem sq_nonneg (a : Int) : 0 ≤ a * a := by
  rcases Int.le_total 0 a with h | h
  · exact Int.mul_nonneg h h
  · exact Int.mul_nonneg_of_nonpos_of_nonpos h h

/-- A square of a nonzero number is positive. -/
theorem sq_pos_of_ne {a : Int} (h : a ≠ 0) : 0 < a * a := by
  rcases Int.lt_or_gt_of_ne h with h' | h'
  · exact Int.mul_pos_of_neg_of_neg h' h'
  · exact Int.mul_pos h' h'

/-- The sign of `a * b` from the signs of `a` and `b`, as one hypothesis that
`omega` and `grind` read with `a * b` as an atom. -/
theorem mul_sign (a b : Int) :
    (0 ≤ a → 0 ≤ b → 0 ≤ a * b) ∧ (a ≤ 0 → b ≤ 0 → 0 ≤ a * b) ∧
    (0 ≤ a → b ≤ 0 → a * b ≤ 0) ∧ (a ≤ 0 → 0 ≤ b → a * b ≤ 0) ∧
    (0 < a → 0 < b → 0 < a * b) ∧ (a < 0 → b < 0 → 0 < a * b) ∧
    (0 < a → b < 0 → a * b < 0) ∧ (a < 0 → 0 < b → a * b < 0) :=
  ⟨Int.mul_nonneg, Int.mul_nonneg_of_nonpos_of_nonpos, Int.mul_nonpos_of_nonneg_of_nonpos,
   Int.mul_nonpos_of_nonpos_of_nonneg, Int.mul_pos, Int.mul_pos_of_neg_of_neg,
   Int.mul_neg_of_pos_of_neg, Int.mul_neg_of_neg_of_pos⟩

/-- Transitivity of the cross-multiplied order `a1/a2 < b1/b2` in the form
`a1*a2*(b2*b2) < b1*b2*(a2*a2)` (the order that is sign-correct for any
nonzero denominators). -/
theorem cross_lt_trans {a1 a2 b1 b2 c1 c2 : Int}
    (h1 : a1 * a2 * (b2 * b2) < b1 * b2 * (a2 * a2))
    (h2 : b1 * b2 * (c2 * c2) < c1 * c2 * (b2 * b2)) :
    a1 * a2 * (c2 * c2) < c1 * c2 * (a2 * a2) := by
  have hb : b2 ≠ 0 := by intro h; subst h; simp at h1
  have ha : a2 ≠ 0 := by intro h; subst h; simp at h1
  have hc : c2 ≠ 0 := by intro h; subst h; simp at h2
  have sa := sq_pos_of_ne ha
  have sb := sq_pos_of_ne hb
  have sc := sq_pos_of_ne hc
  have e1 := Int.mul_lt_mul_of_pos_right h1 sc
  have e2 := Int.mul_lt_mul_of_pos_right h2 sa
  have e3 : a1 * a2 * (c2 * c2) * (b2 * b2) < c1 * c2 * (a2 * a2) * (b2 * b2) := by
    have r1 : a1 * a2 * (c2 * c2) * (b2 * b2) = a1 * a2 * (b2 * b2) * (c2 * c2) := by
      first | ac_rfl | grind
    have r2 : b1 * b2 * (a2 * a2) * (c2 * c2) = b1 * b2 * (c2 * c2) * (a2 * a2) := by
      first | ac_rfl | grind
    have r3 : c1 * c2 * (a2 * a2) * (b2 * b2) = c1 * c2 * (b2 * b2) * (a2 * a2) := by
      first | ac_rfl | grind
    omega
  exact Int.lt_of_mul_lt_mul_right e3 (Int.le_of_lt sb)

/-! ## Bit masks: `AverBits.and` against a nonnegative mask, read through `Nat` -/

/-- A nonnegative number masked by a nonnegative mask is the `Nat` conjunction. -/
theorem and_of_nonneg (a m : Int) (M : Nat) (hM : m = M) (ha : 0 ≤ a) :
    AverBits.and a m = ((a.toNat &&& M : Nat) : Int) := by
  subst hM
  have hm : ¬ ((M : Int) < 0) := by omega
  have hna : ¬ (a < 0) := by omega
  first
  | (simp only [AverBits.and, AverBits.mag, hm, hna, ite_false, ↓reduceIte, Int.toNat_natCast,
      Nat.land_eq]; done)
  | (unfold AverBits.and AverBits.mag; simp [hm, hna]; done)
  | (simp [AverBits.and, AverBits.mag, hm, hna])

/-- A negative number masked by a nonnegative mask: the mask minus the mask's
bits that the complement `-a - 1` carries. -/
theorem and_of_neg (a m : Int) (M : Nat) (hM : m = M) (ha : a < 0) :
    AverBits.and a m = ((M - ((-a - 1).toNat &&& M) : Nat) : Int) := by
  subst hM
  have hm : ¬ ((M : Int) < 0) := by omega
  first
  | (simp only [AverBits.and, AverBits.mag, hm, ha, ite_true, ite_false, ↓reduceIte,
      Int.toNat_natCast, Nat.land_eq]; done)
  | (unfold AverBits.and AverBits.mag; simp [hm, ha]; done)
  | (simp [AverBits.and, AverBits.mag, hm, ha])

/-- A low mask `2^k - 1` is a remainder. -/
theorem nat_land_low (x m M k : Nat) (hM : m + 1 = M) (hk : M = 2 ^ k) : x &&& m = x % M := by
  subst hk
  have e : m = 2 ^ k - 1 := by omega
  rw [e, Nat.and_two_pow_sub_one_eq_mod]

/-- A single-bit mask `2^k` is that bit of the quotient. -/
theorem nat_land_bit (x m k : Nat) (hm : m = 2 ^ k) : x &&& m = m * (x / m % 2) := by
  subst hm
  apply Nat.eq_of_testBit_eq
  intro i
  rw [Nat.testBit_and, Nat.testBit_two_pow]
  rcases Nat.mod_two_eq_zero_or_one (x / 2 ^ k) with h | h
  · rw [h, Nat.mul_zero, Nat.zero_testBit]
    by_cases hk : k = i
    · subst hk
      rw [Nat.testBit_eq_decide_div_mod_eq, h]
      simp
    · simp [hk]
  · rw [h, Nat.mul_one, Nat.testBit_two_pow]
    by_cases hk : k = i
    · subst hk
      rw [Nat.testBit_eq_decide_div_mod_eq, h]
      simp
    · simp [hk]

/-- A mask `P * hi + lo` with `P = 2^j` and `lo < P` splits at bit `j`: the
high part masks the quotient, the low part the remainder. Applied repeatedly
it takes any literal mask apart into single bits and low runs. -/
theorem nat_land_split (x m P j hi lo : Nat) (hP : P = 2 ^ j) (hm : m = P * hi + lo)
    (hlo : lo < P) : x &&& m = P * ((x / P) &&& hi) + ((x % P) &&& lo) := by
  subst hP
  subst hm
  have hlt : (x % 2 ^ j) &&& lo < 2 ^ j := Nat.and_lt_two_pow _ hlo
  apply Nat.eq_of_testBit_eq
  intro i
  rw [Nat.testBit_and, Nat.testBit_two_pow_mul_add _ hlo, Nat.testBit_two_pow_mul_add _ hlt]
  by_cases hij : i < j
  · first
    | (simp [hij, Nat.testBit_and, Nat.testBit_mod_two_pow]; done)
    | grind
  · first
    | (simp [hij, Nat.testBit_and, Nat.testBit_div_two_pow,
        Nat.sub_add_cancel (Nat.le_of_not_lt hij)]; done)
    | grind

end AverKit

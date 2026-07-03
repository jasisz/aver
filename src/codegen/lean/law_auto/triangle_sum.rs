//! Lean renderer for the rational triangle-sum rung — the strict
//! magnitude bound on an `absFraction`-of-a-three-term-sum over the
//! exact-rational order (`Domain.Rational`), the rounded Newton-Raphson
//! reciprocal step family of `projects/k5_fdiv`.
//!
//! This is a BESPOKE K5 figure — the rounded Newton-Raphson reciprocal
//! step-error bound (`recip.av` `nrRoundedStepError` / `oneStepBound`), not a
//! general mechanism. The reusable part is the triangle-inequality KIT it
//! emits (below); the ASSEMBLY is a fixed composition pinned to this one
//! lemma. It is name-blind on the law and figure names (keyed only on the
//! rational order primitives `lessThan` / `absFraction` / `minus` / `plus` /
//! `times` / `oneFraction` / `sameValue` and the rounding primitives
//! `awayFrac` / `truncFrac` / `compFrac` / `fracExpo` / `pow2Signed`). The
//! recognized shape is a conditional `holds` law whose claim is
//!
//! ```text
//! lessThan (absFraction E) B
//! ```
//!
//! where the error `E` is established `sameValue` a three-term sum
//! `X + Y + Z` (the ring identity discharged by `grind` over the opaque
//! rational atoms), `X` is a squared prior error bounded by `tri_sq_strict`,
//! and `Y`, `Z` are each a cited rounding bound times a monotone magnitude
//! factor bounded by `tri_abs_times_le`. The keystone composer `tri_sum3`
//! assembles the three term bounds into the bound on `|E|`.
//!
//! The renderer emits a prelude-style kit of GENERIC, name-blind helper
//! lemmas (the `Int.natAbs` abs algebra, the rational triangle / abs
//! sign-split / scaled-difference monotonicities, the squared and
//! times-bounded term lemmas, and the `tri_sum3` sum-of-bounds composer) —
//! each provable on its own, reusable for ANY such law — then the fixed
//! assembly. The assembly CITES the two rounding-error bound universals from
//! the dependency module (`awayFracErrorBound` / `truncFracErrorBound`,
//! Lemma 7.2.2 / 7.2.3) rather than re-deriving them: those laws are admitted
//! into the build through the cross-file law pool. The whole assembly sits
//! under a `first | (…) | sorry` floor, so a law whose shape the recognizer
//! admits but whose closing slips falls to an honest caught `sorry`; credit
//! stays fail-closed behind the `#print axioms` whitelist.

use super::AutoProof;
use super::aver_name_to_lean;
use super::shared::{
    self, expr_dotted_name, find_fn_def, find_fn_def_by_call_name, law_simp_source_names,
};
use crate::ast::{Expr, Spanned, Stmt, VerifyBlock, VerifyKind, VerifyLaw};
use crate::codegen::CodegenContext;

/// The generic, name-blind helper kit (the `Int.natAbs` abs algebra, the
/// rational triangle / monotonicity lemmas, and the `tri_sum3` composer).
/// Provable on its own — none of it mentions the law. `aver_sq_nonneg` is
/// defined here (the kit uses it); the `aver_int_order` macro is NOT emitted
/// (the kit does not use it), so the body never trips the prelude's
/// nonlinear-helper trigger and never collides with it.
const TRIANGLE_KIT: &str = r#"theorem aver_sq_nonneg (t : Int) : 0 ≤ t * t := by
  rcases Int.le_total 0 t with h | h
  · exact Int.mul_nonneg h h
  · have h2 : 0 ≤ -t := by omega
    have := Int.mul_nonneg h2 h2
    rwa [Int.neg_mul_neg] at this

theorem tri_absInt_eq (m : Int) : Domain.Rational.absInt m = (m.natAbs : Int) := by
  simp only [Domain.Rational.absInt]; by_cases h : m < 0 <;> simp [h] <;> omega

theorem tri_absInt_nonneg (m : Int) : 0 ≤ Domain.Rational.absInt m := by
  rw [tri_absInt_eq]; omega

theorem tri_absInt_ne_zero (m : Int) (h : m ≠ 0) : Domain.Rational.absInt m ≠ 0 := by
  rw [tri_absInt_eq]; omega

theorem tri_absInt_mul (m n : Int) :
    Domain.Rational.absInt (m * n) = Domain.Rational.absInt m * Domain.Rational.absInt n := by
  simp only [tri_absInt_eq, Int.natAbs_mul, Int.natCast_mul]

theorem tri_absInt_triangle (m n : Int) :
    Domain.Rational.absInt (m + n) ≤ Domain.Rational.absInt m + Domain.Rational.absInt n := by
  simp only [tri_absInt_eq]; omega

theorem tri_int_sq_strict (x y : Int) (hx : 0 ≤ x) (hxy : x < y) : x * x < y * y := by
  have h := Int.mul_pos (show 0 < y - x by omega) (show 0 < y + x by omega)
  have key : (y - x) * (y + x) = y * y - x * x := by grind
  rw [key] at h; omega

theorem tri_int_sq_pos (x : Int) (h : x ≠ 0) : 0 < x * x := by
  rcases (by omega : x < 0 ∨ 0 < x) with hx | hx
  · have := Int.mul_pos (show 0 < -x by omega) (show 0 < -x by omega)
    rwa [Int.neg_mul_neg] at this
  · exact Int.mul_pos hx hx

theorem tri_abs_add (a b : Fraction) :
    Domain.Rational.isNonNeg (Domain.Rational.minus
      (Domain.Rational.plus (Domain.Rational.absFraction a) (Domain.Rational.absFraction b))
      (Domain.Rational.absFraction (Domain.Rational.plus a b))) = true := by
  simp only [Domain.Rational.isNonNeg, Domain.Rational.minus, Domain.Rational.plus,
    Domain.Rational.absFraction, decide_eq_true_eq, ge_iff_le]
  rw [tri_absInt_mul a.bottom b.bottom]
  have hb1 := tri_absInt_nonneg a.bottom
  have hb2 := tri_absInt_nonneg b.bottom
  have hPQ : Domain.Rational.absInt (a.top * b.bottom + b.top * a.bottom)
           ≤ Domain.Rational.absInt a.top * Domain.Rational.absInt b.bottom
           + Domain.Rational.absInt b.top * Domain.Rational.absInt a.bottom := by
    have h := tri_absInt_triangle (a.top * b.bottom) (b.top * a.bottom)
    rw [tri_absInt_mul a.top b.bottom, tri_absInt_mul b.top a.bottom] at h
    exact h
  have key :
      ((Domain.Rational.absInt a.top * Domain.Rational.absInt b.bottom + Domain.Rational.absInt b.top * Domain.Rational.absInt a.bottom)
          * (Domain.Rational.absInt a.bottom * Domain.Rational.absInt b.bottom)
        - Domain.Rational.absInt (a.top * b.bottom + b.top * a.bottom)
          * (Domain.Rational.absInt a.bottom * Domain.Rational.absInt b.bottom))
        * ((Domain.Rational.absInt a.bottom * Domain.Rational.absInt b.bottom)
          * (Domain.Rational.absInt a.bottom * Domain.Rational.absInt b.bottom))
      = ((Domain.Rational.absInt a.top * Domain.Rational.absInt b.bottom + Domain.Rational.absInt b.top * Domain.Rational.absInt a.bottom)
          - Domain.Rational.absInt (a.top * b.bottom + b.top * a.bottom))
        * ((Domain.Rational.absInt a.bottom * Domain.Rational.absInt b.bottom)
          * ((Domain.Rational.absInt a.bottom * Domain.Rational.absInt b.bottom)
            * (Domain.Rational.absInt a.bottom * Domain.Rational.absInt b.bottom))) := by grind
  rw [key]
  have hD : 0 ≤ Domain.Rational.absInt a.bottom * Domain.Rational.absInt b.bottom := Int.mul_nonneg hb1 hb2
  exact Int.mul_nonneg (by omega) (Int.mul_nonneg hD (Int.mul_nonneg hD hD))

theorem tri_abs_samevalue_le (a b : Fraction) (h : Domain.Rational.sameValue a b = true) :
    Domain.Rational.isNonNeg (Domain.Rational.minus (Domain.Rational.absFraction b) (Domain.Rational.absFraction a)) = true := by
  simp only [Domain.Rational.sameValue, beq_iff_eq] at h
  simp only [Domain.Rational.isNonNeg, Domain.Rational.minus, Domain.Rational.absFraction, decide_eq_true_eq, ge_iff_le]
  have hz : Domain.Rational.absInt b.top * Domain.Rational.absInt a.bottom
          - Domain.Rational.absInt a.top * Domain.Rational.absInt b.bottom = 0 := by
    have h2 : Domain.Rational.absInt (a.top * b.bottom) = Domain.Rational.absInt (b.top * a.bottom) := by rw [h]
    rw [tri_absInt_mul, tri_absInt_mul] at h2; omega
  have key : (Domain.Rational.absInt b.top * Domain.Rational.absInt a.bottom
          - Domain.Rational.absInt a.top * Domain.Rational.absInt b.bottom)
        * (Domain.Rational.absInt b.bottom * Domain.Rational.absInt a.bottom) = 0 := by
    rw [hz, Int.zero_mul]
  rw [key]
  omega

theorem tri_le_lt_trans (a b c : Fraction)
    (hab : Domain.Rational.isNonNeg (Domain.Rational.minus b a) = true)
    (hbc : Domain.Rational.lessThan b c = true)
    (ha : a.bottom ≠ 0) (hb : b.bottom ≠ 0) : Domain.Rational.lessThan a c = true := by
  simp only [Domain.Rational.isNonNeg, Domain.Rational.minus, Domain.Rational.lessThan, decide_eq_true_eq, ge_iff_le] at hab hbc ⊢
  have hq2 : 0 < a.bottom * a.bottom := tri_int_sq_pos _ ha
  have hs2 : 0 < b.bottom * b.bottom := tri_int_sq_pos _ hb
  have hu2 : 0 ≤ c.bottom * c.bottom := aver_sq_nonneg _
  have step1 : (b.top * b.bottom * (c.bottom * c.bottom)) * (a.bottom * a.bottom)
             < (c.top * c.bottom * (b.bottom * b.bottom)) * (a.bottom * a.bottom) :=
    Int.mul_lt_mul_of_pos_right hbc hq2
  have hab' : a.top * (b.bottom * b.bottom) * a.bottom
            ≤ b.top * b.bottom * (a.bottom * a.bottom) := by grind
  have step2 : (a.top * (b.bottom * b.bottom) * a.bottom) * (c.bottom * c.bottom)
             ≤ (b.top * b.bottom * (a.bottom * a.bottom)) * (c.bottom * c.bottom) :=
    Int.mul_le_mul_of_nonneg_right hab' hu2
  have hmul : (a.top * a.bottom * (c.bottom * c.bottom)) * (b.bottom * b.bottom)
            < (c.top * c.bottom * (a.bottom * a.bottom)) * (b.bottom * b.bottom) := by grind
  exact Int.lt_of_mul_lt_mul_right hmul (by omega)

theorem tri_le_trans (a b c : Fraction)
    (hab : Domain.Rational.isNonNeg (Domain.Rational.minus b a) = true)
    (hbc : Domain.Rational.isNonNeg (Domain.Rational.minus c b) = true)
    (hb : b.bottom ≠ 0) : Domain.Rational.isNonNeg (Domain.Rational.minus c a) = true := by
  simp only [Domain.Rational.isNonNeg, Domain.Rational.minus, decide_eq_true_eq, ge_iff_le] at hab hbc ⊢
  have hs2 : 0 < b.bottom * b.bottom := tri_int_sq_pos _ hb
  have m1 : 0 ≤ ((b.top * a.bottom - a.top * b.bottom) * (b.bottom * a.bottom)) * (c.bottom * c.bottom) :=
    Int.mul_nonneg hab (aver_sq_nonneg _)
  have m2 : 0 ≤ ((c.top * b.bottom - b.top * c.bottom) * (c.bottom * b.bottom)) * (a.bottom * a.bottom) :=
    Int.mul_nonneg hbc (aver_sq_nonneg _)
  have key : ((c.top * a.bottom - a.top * c.bottom) * (c.bottom * a.bottom)) * (b.bottom * b.bottom)
      = ((b.top * a.bottom - a.top * b.bottom) * (b.bottom * a.bottom)) * (c.bottom * c.bottom)
      + ((c.top * b.bottom - b.top * c.bottom) * (c.bottom * b.bottom)) * (a.bottom * a.bottom) := by grind
  have hfin : 0 * (b.bottom * b.bottom)
            ≤ ((c.top * a.bottom - a.top * c.bottom) * (c.bottom * a.bottom)) * (b.bottom * b.bottom) := by
    rw [Int.zero_mul, key]; exact Int.add_nonneg m1 m2
  exact Int.le_of_mul_le_mul_right hfin hs2

theorem tri_le_add (a b c d : Fraction)
    (hab : Domain.Rational.isNonNeg (Domain.Rational.minus b a) = true)
    (hcd : Domain.Rational.isNonNeg (Domain.Rational.minus d c) = true) :
    Domain.Rational.isNonNeg (Domain.Rational.minus (Domain.Rational.plus b d) (Domain.Rational.plus a c)) = true := by
  simp only [Domain.Rational.isNonNeg, Domain.Rational.minus, Domain.Rational.plus, decide_eq_true_eq, ge_iff_le] at hab hcd ⊢
  have key :
      ((b.top * d.bottom + d.top * b.bottom) * (a.bottom * c.bottom)
        - (a.top * c.bottom + c.top * a.bottom) * (b.bottom * d.bottom))
        * (b.bottom * d.bottom * (a.bottom * c.bottom))
      = ((b.top * a.bottom - a.top * b.bottom) * (b.bottom * a.bottom)) * ((c.bottom * d.bottom) * (c.bottom * d.bottom))
      + ((d.top * c.bottom - c.top * d.bottom) * (d.bottom * c.bottom)) * ((a.bottom * b.bottom) * (a.bottom * b.bottom)) := by grind
  rw [key]
  exact Int.add_nonneg (Int.mul_nonneg hab (aver_sq_nonneg _)) (Int.mul_nonneg hcd (aver_sq_nonneg _))

theorem tri_lt_le_add (a b c d : Fraction)
    (hab : Domain.Rational.lessThan a b = true)
    (hcd : Domain.Rational.isNonNeg (Domain.Rational.minus d c) = true)
    (hc : c.bottom ≠ 0) (hd : d.bottom ≠ 0) :
    Domain.Rational.lessThan (Domain.Rational.plus a c) (Domain.Rational.plus b d) = true := by
  simp only [Domain.Rational.lessThan, Domain.Rational.isNonNeg, Domain.Rational.minus, Domain.Rational.plus, decide_eq_true_eq, ge_iff_le] at hab hcd ⊢
  have hab' : 0 < (b.top * a.bottom - a.top * b.bottom) * (a.bottom * b.bottom) := by
    have key2 : (b.top * a.bottom - a.top * b.bottom) * (a.bottom * b.bottom)
              = b.top * b.bottom * (a.bottom * a.bottom) - a.top * a.bottom * (b.bottom * b.bottom) := by grind
    rw [key2]; omega
  have hcdpos : 0 < (c.bottom * d.bottom) * (c.bottom * d.bottom) :=
    tri_int_sq_pos _ (Int.mul_ne_zero hc hd)
  have t1 : 0 < ((b.top * a.bottom - a.top * b.bottom) * (a.bottom * b.bottom)) * ((c.bottom * d.bottom) * (c.bottom * d.bottom)) :=
    Int.mul_pos hab' hcdpos
  have t2 : 0 ≤ ((d.top * c.bottom - c.top * d.bottom) * (d.bottom * c.bottom)) * ((a.bottom * b.bottom) * (a.bottom * b.bottom)) :=
    Int.mul_nonneg hcd (aver_sq_nonneg _)
  have keyG :
      (b.top * d.bottom + d.top * b.bottom) * (b.bottom * d.bottom) * ((a.bottom * c.bottom) * (a.bottom * c.bottom))
        - (a.top * c.bottom + c.top * a.bottom) * (a.bottom * c.bottom) * ((b.bottom * d.bottom) * (b.bottom * d.bottom))
      = ((b.top * a.bottom - a.top * b.bottom) * (a.bottom * b.bottom)) * ((c.bottom * d.bottom) * (c.bottom * d.bottom))
      + ((d.top * c.bottom - c.top * d.bottom) * (d.bottom * c.bottom)) * ((a.bottom * b.bottom) * (a.bottom * b.bottom)) := by grind
  omega

theorem tri_sq_strict (p e : Fraction)
    (h : Domain.Rational.lessThan (Domain.Rational.absFraction p) e = true) :
    Domain.Rational.lessThan (Domain.Rational.absFraction (Domain.Rational.times p p)) (Domain.Rational.times e e) = true := by
  simp only [Domain.Rational.lessThan, Domain.Rational.absFraction, Domain.Rational.times, decide_eq_true_eq] at h ⊢
  rw [tri_absInt_mul p.top p.top, tri_absInt_mul p.bottom p.bottom]
  have hx0 : 0 ≤ Domain.Rational.absInt p.top * Domain.Rational.absInt p.bottom * (e.bottom * e.bottom) :=
    Int.mul_nonneg (Int.mul_nonneg (tri_absInt_nonneg _) (tri_absInt_nonneg _)) (aver_sq_nonneg _)
  have hsq := tri_int_sq_strict _ _ hx0 h
  have eL : Domain.Rational.absInt p.top * Domain.Rational.absInt p.top * (Domain.Rational.absInt p.bottom * Domain.Rational.absInt p.bottom) * (e.bottom * e.bottom * (e.bottom * e.bottom))
          = (Domain.Rational.absInt p.top * Domain.Rational.absInt p.bottom * (e.bottom * e.bottom)) * (Domain.Rational.absInt p.top * Domain.Rational.absInt p.bottom * (e.bottom * e.bottom)) := by grind
  have eR : e.top * e.top * (e.bottom * e.bottom) * (Domain.Rational.absInt p.bottom * Domain.Rational.absInt p.bottom * (Domain.Rational.absInt p.bottom * Domain.Rational.absInt p.bottom))
          = (e.top * e.bottom * (Domain.Rational.absInt p.bottom * Domain.Rational.absInt p.bottom)) * (e.top * e.bottom * (Domain.Rational.absInt p.bottom * Domain.Rational.absInt p.bottom)) := by grind
  rw [eL, eR]; exact hsq

theorem tri_abs_times_le (x y u : Fraction)
    (h : Domain.Rational.lessThan (Domain.Rational.absFraction y) u = true) :
    Domain.Rational.isNonNeg (Domain.Rational.minus (Domain.Rational.times (Domain.Rational.absFraction x) u) (Domain.Rational.absFraction (Domain.Rational.times x y))) = true := by
  simp only [Domain.Rational.lessThan, Domain.Rational.isNonNeg, Domain.Rational.minus, Domain.Rational.times, Domain.Rational.absFraction, decide_eq_true_eq, ge_iff_le] at h ⊢
  rw [tri_absInt_mul x.top y.top, tri_absInt_mul x.bottom y.bottom]
  have hW : 0 ≤ (u.top * Domain.Rational.absInt y.bottom - Domain.Rational.absInt y.top * u.bottom) * (Domain.Rational.absInt y.bottom * u.bottom) := by
    have keyW : (u.top * Domain.Rational.absInt y.bottom - Domain.Rational.absInt y.top * u.bottom) * (Domain.Rational.absInt y.bottom * u.bottom)
              = u.top * u.bottom * (Domain.Rational.absInt y.bottom * Domain.Rational.absInt y.bottom) - Domain.Rational.absInt y.top * Domain.Rational.absInt y.bottom * (u.bottom * u.bottom) := by grind
    rw [keyW]; omega
  have hXn : 0 ≤ Domain.Rational.absInt x.top * (Domain.Rational.absInt x.bottom * (Domain.Rational.absInt x.bottom * Domain.Rational.absInt x.bottom)) := by
    have hxt := tri_absInt_nonneg x.top
    have hxb := tri_absInt_nonneg x.bottom
    exact Int.mul_nonneg hxt (Int.mul_nonneg hxb (Int.mul_nonneg hxb hxb))
  have key : (Domain.Rational.absInt x.top * u.top * (Domain.Rational.absInt x.bottom * Domain.Rational.absInt y.bottom)
            - Domain.Rational.absInt x.top * Domain.Rational.absInt y.top * (Domain.Rational.absInt x.bottom * u.bottom))
            * (Domain.Rational.absInt x.bottom * u.bottom * (Domain.Rational.absInt x.bottom * Domain.Rational.absInt y.bottom))
          = ((u.top * Domain.Rational.absInt y.bottom - Domain.Rational.absInt y.top * u.bottom) * (Domain.Rational.absInt y.bottom * u.bottom))
            * (Domain.Rational.absInt x.top * (Domain.Rational.absInt x.bottom * (Domain.Rational.absInt x.bottom * Domain.Rational.absInt x.bottom))) := by grind
  rw [key]
  exact Int.mul_nonneg hW hXn

theorem tri_sum3 (E X Y Z bx byy bz : Fraction)
    (hsv : Domain.Rational.sameValue E (Domain.Rational.plus X (Domain.Rational.plus Y Z)) = true)
    (hX : Domain.Rational.lessThan (Domain.Rational.absFraction X) bx = true)
    (hY : Domain.Rational.isNonNeg (Domain.Rational.minus byy (Domain.Rational.absFraction Y)) = true)
    (hZ : Domain.Rational.isNonNeg (Domain.Rational.minus bz (Domain.Rational.absFraction Z)) = true)
    (hEb : E.bottom ≠ 0) (hXb : X.bottom ≠ 0) (hYb : Y.bottom ≠ 0) (hZb : Z.bottom ≠ 0)
    (hbyb : byy.bottom ≠ 0) (hbzb : bz.bottom ≠ 0) :
    Domain.Rational.lessThan (Domain.Rational.absFraction E) (Domain.Rational.plus bx (Domain.Rational.plus byy bz)) = true := by
  have hYZtri := tri_abs_add Y Z
  have hYZle := tri_le_add (Domain.Rational.absFraction Y) byy (Domain.Rational.absFraction Z) bz hY hZ
  have hmidb : (Domain.Rational.plus (Domain.Rational.absFraction Y) (Domain.Rational.absFraction Z)).bottom ≠ 0 := by
    simp only [Domain.Rational.plus, Domain.Rational.absFraction]
    exact Int.mul_ne_zero (tri_absInt_ne_zero _ hYb) (tri_absInt_ne_zero _ hZb)
  have hYZle2 : Domain.Rational.isNonNeg (Domain.Rational.minus (Domain.Rational.plus byy bz) (Domain.Rational.absFraction (Domain.Rational.plus Y Z))) = true :=
    tri_le_trans (Domain.Rational.absFraction (Domain.Rational.plus Y Z)) (Domain.Rational.plus (Domain.Rational.absFraction Y) (Domain.Rational.absFraction Z)) (Domain.Rational.plus byy bz) hYZtri hYZle hmidb
  have hYZb : (Domain.Rational.absFraction (Domain.Rational.plus Y Z)).bottom ≠ 0 := by
    simp only [Domain.Rational.absFraction, Domain.Rational.plus]
    exact tri_absInt_ne_zero _ (Int.mul_ne_zero hYb hZb)
  have hbyzb : (Domain.Rational.plus byy bz).bottom ≠ 0 := by
    simp only [Domain.Rational.plus]; exact Int.mul_ne_zero hbyb hbzb
  have hsum_lt : Domain.Rational.lessThan (Domain.Rational.plus (Domain.Rational.absFraction X) (Domain.Rational.absFraction (Domain.Rational.plus Y Z))) (Domain.Rational.plus bx (Domain.Rational.plus byy bz)) = true :=
    tri_lt_le_add (Domain.Rational.absFraction X) bx (Domain.Rational.absFraction (Domain.Rational.plus Y Z)) (Domain.Rational.plus byy bz) hX hYZle2 hYZb hbyzb
  have hXtri := tri_abs_add X (Domain.Rational.plus Y Z)
  have hSb : (Domain.Rational.absFraction (Domain.Rational.plus X (Domain.Rational.plus Y Z))).bottom ≠ 0 := by
    simp only [Domain.Rational.absFraction, Domain.Rational.plus]
    exact tri_absInt_ne_zero _ (Int.mul_ne_zero hXb (Int.mul_ne_zero hYb hZb))
  have hsumb : (Domain.Rational.plus (Domain.Rational.absFraction X) (Domain.Rational.absFraction (Domain.Rational.plus Y Z))).bottom ≠ 0 := by
    simp only [Domain.Rational.plus, Domain.Rational.absFraction]
    exact Int.mul_ne_zero (tri_absInt_ne_zero _ hXb) (tri_absInt_ne_zero _ (Int.mul_ne_zero hYb hZb))
  have hS_lt : Domain.Rational.lessThan (Domain.Rational.absFraction (Domain.Rational.plus X (Domain.Rational.plus Y Z))) (Domain.Rational.plus bx (Domain.Rational.plus byy bz)) = true :=
    tri_le_lt_trans (Domain.Rational.absFraction (Domain.Rational.plus X (Domain.Rational.plus Y Z))) (Domain.Rational.plus (Domain.Rational.absFraction X) (Domain.Rational.absFraction (Domain.Rational.plus Y Z))) (Domain.Rational.plus bx (Domain.Rational.plus byy bz)) hXtri hsum_lt hSb hsumb
  have hEle := tri_abs_samevalue_le E (Domain.Rational.plus X (Domain.Rational.plus Y Z)) hsv
  exact tri_le_lt_trans (Domain.Rational.absFraction E) (Domain.Rational.absFraction (Domain.Rational.plus X (Domain.Rational.plus Y Z))) (Domain.Rational.plus bx (Domain.Rational.plus byy bz)) hEle hS_lt (by simp only [Domain.Rational.absFraction]; exact tri_absInt_ne_zero _ hEb) hSb

theorem tri_denoms (x b : Fraction) (h : Domain.Rational.lessThan x b = true) :
    x.bottom ≠ 0 ∧ b.bottom ≠ 0 := by
  simp only [Domain.Rational.lessThan, decide_eq_true_eq] at h
  constructor <;> intro hc <;> rw [hc] at h <;> simp at h"#;

/// The per-law ring identity `E = X + Y + Z`, discharged by `grind` over the
/// opaque rational atoms — generic over the four atoms, parameterised only by
/// the rounding precision.
const VALUE_IDENTITY_TEMPLATE: &str = r#"theorem nr_value_identity (d sd sdd snew : Fraction) :
    Domain.Rational.sameValue (Domain.Rational.minus (Domain.Rational.oneFraction) (Domain.Rational.times (d) (snew)))
      (Domain.Rational.plus (Domain.Rational.times (Domain.Rational.minus (Domain.Rational.oneFraction) (Domain.Rational.times (d) (sd))) (Domain.Rational.minus (Domain.Rational.oneFraction) (Domain.Rational.times (d) (sd)))) (Domain.Rational.plus (Domain.Rational.times (Domain.Rational.times (d) (sd)) (Domain.Rational.minus (sdd) (Domain.Rational.times (d) (sd)))) (Domain.Rational.times (d) (Domain.Rational.minus (Domain.Rational.times (sd) (Domain.Kernel.compFrac sdd 32)) (snew))))) = true := by
  simp only [Domain.Kernel.compFrac, Domain.Rational.sameValue, Domain.Rational.minus,
    Domain.Rational.plus, Domain.Rational.times, Domain.Rational.oneFraction, beq_iff_eq]
  grind"#;

/// Render the fixed assembly: cite the two rounding bounds, bound `X` by
/// `tri_sq_strict`, bound `Y`/`Z` by `tri_abs_times_le`, and compose with
/// `tri_sum3`. Every figure/law/dependency NAME is a discovered parameter
/// (`base` theorem name, `subj`/`prior` fns, the `unfold` cone list, the two
/// cited bound theorems `away_thm`/`trunc_thm` and the dependency unfold fns
/// `away_subj_fn`/`away_err_fn`/`trunc_subj_fn`/`trunc_err_fn`, the per-law
/// value-identity `vid`) — there is NO figure-name literal in the source. The
/// quantified variables `d`/`sd`/`eps` are GENERIC LOCAL binder names
/// (alpha-equivalent to the law's three givens whatever they are named); the
/// rounding precision `32` is substituted by the caller. The `tri_*` /
/// `aver_sq_nonneg` helper references are left bare here and prefixed per-law
/// by the caller so two such laws in one module never collide.
#[allow(clippy::too_many_arguments)]
fn render_assembly(
    base: &str,
    subj: &str,
    prior: &str,
    unfold: &str,
    away_thm: &str,
    away_subj_fn: &str,
    away_err_fn: &str,
    trunc_thm: &str,
    trunc_subj_fn: &str,
    trunc_err_fn: &str,
    vid: &str,
) -> String {
    format!(
        r#"theorem {base} : ∀ (d sd eps : Fraction),
    (((((d.top != 0) && (d.bottom != 0)) && (sd.top != 0)) && (sd.bottom != 0)) && Domain.Rational.lessThan (Domain.Rational.absFraction ({prior} d sd)) eps) = true →
    {subj} d sd eps = true := by
  intro d sd eps hwhen
  simp only [Bool.and_eq_true, bne_iff_ne, ne_eq] at hwhen
  obtain ⟨⟨⟨⟨hdt, hdb⟩, hst⟩, hsb⟩, hPrior⟩ := hwhen
  simp only [{prior}] at hPrior
  simp only [{unfold}]
  have hqt : (Domain.Rational.times d sd).top ≠ 0 := Int.mul_ne_zero hdt hst
  have hqb : (Domain.Rational.times d sd).bottom ≠ 0 := Int.mul_ne_zero hdb hsb
  have hAway : Domain.Rational.lessThan (Domain.Rational.absFraction (Domain.Rational.minus (Domain.Kernel.awayFrac (Domain.Rational.times d sd) 32) (Domain.Rational.times d sd))) (Domain.Fprep.pow2Signed (Domain.Kernel.fracExpo (Domain.Rational.times d sd) - 32 + 1)) = true := by
    have hL := {away_thm} (Domain.Rational.times d sd) 32 (by simp only [Bool.and_eq_true, bne_iff_ne]; exact ⟨hqt, hqb⟩)
    simp only [{away_subj_fn}, {away_err_fn}] at hL
    exact hL
  obtain ⟨hminAwayb, huAb⟩ := tri_denoms _ _ hAway
  have hsddb : (Domain.Kernel.awayFrac (Domain.Rational.times d sd) 32).bottom ≠ 0 := by
    simp only [Domain.Rational.absFraction, Domain.Rational.minus] at hminAwayb
    intro hc; apply hminAwayb; rw [hc]; simp [Domain.Rational.absInt]
  have hprodb : (Domain.Rational.times sd (Domain.Kernel.compFrac (Domain.Kernel.awayFrac (Domain.Rational.times d sd) 32) 32)).bottom ≠ 0 := Int.mul_ne_zero hsb (Int.mul_ne_zero (by decide) hsddb)
  have hTrunc : Domain.Rational.lessThan (Domain.Rational.absFraction (Domain.Rational.minus (Domain.Rational.times sd (Domain.Kernel.compFrac (Domain.Kernel.awayFrac (Domain.Rational.times d sd) 32) 32)) (Domain.Kernel.truncFrac (Domain.Rational.times sd (Domain.Kernel.compFrac (Domain.Kernel.awayFrac (Domain.Rational.times d sd) 32) 32)) 32))) (Domain.Fprep.pow2Signed (Domain.Kernel.fracExpo (Domain.Rational.times sd (Domain.Kernel.compFrac (Domain.Kernel.awayFrac (Domain.Rational.times d sd) 32) 32)) - 32 + 1)) = true := by
    by_cases hp0 : (Domain.Rational.times sd (Domain.Kernel.compFrac (Domain.Kernel.awayFrac (Domain.Rational.times d sd) 32) 32)).top = 0
    · have hsnew0 : (Domain.Kernel.truncFrac (Domain.Rational.times sd (Domain.Kernel.compFrac (Domain.Kernel.awayFrac (Domain.Rational.times d sd) 32) 32)) 32) = Domain.Rational.zeroFraction := by
        simp only [Domain.Kernel.truncFrac, hp0, beq_self_eq_true, if_true]
      rw [hsnew0]
      have hutT := {trunc_thm}__frb__sgnt_pos (Domain.Kernel.fracExpo (Domain.Rational.times sd (Domain.Kernel.compFrac (Domain.Kernel.awayFrac (Domain.Rational.times d sd) 32) 32)) - 32 + 1)
      have hutB := {trunc_thm}__frb__sgnb_pos (Domain.Kernel.fracExpo (Domain.Rational.times sd (Domain.Kernel.compFrac (Domain.Kernel.awayFrac (Domain.Rational.times d sd) 32) 32)) - 32 + 1)
      simp only [Domain.Rational.lessThan, Domain.Rational.absFraction, Domain.Rational.minus, Domain.Rational.zeroFraction, decide_eq_true_eq]
      have htop0 : Domain.Rational.absInt ((Domain.Rational.times sd (Domain.Kernel.compFrac (Domain.Kernel.awayFrac (Domain.Rational.times d sd) 32) 32)).top * 1 - 0 * (Domain.Rational.times sd (Domain.Kernel.compFrac (Domain.Kernel.awayFrac (Domain.Rational.times d sd) 32) 32)).bottom) = 0 := by
        rw [hp0]; simp [Domain.Rational.absInt]
      rw [htop0]
      have hbne : Domain.Rational.absInt ((Domain.Rational.times sd (Domain.Kernel.compFrac (Domain.Kernel.awayFrac (Domain.Rational.times d sd) 32) 32)).bottom * 1) ≠ 0 := tri_absInt_ne_zero _ (by simpa using hprodb)
      have hbnn := tri_absInt_nonneg ((Domain.Rational.times sd (Domain.Kernel.compFrac (Domain.Kernel.awayFrac (Domain.Rational.times d sd) 32) 32)).bottom * 1)
      have hbpos : 0 < Domain.Rational.absInt ((Domain.Rational.times sd (Domain.Kernel.compFrac (Domain.Kernel.awayFrac (Domain.Rational.times d sd) 32) 32)).bottom * 1) := by omega
      have hpos : 0 < (Domain.Fprep.pow2Signed (Domain.Kernel.fracExpo (Domain.Rational.times sd (Domain.Kernel.compFrac (Domain.Kernel.awayFrac (Domain.Rational.times d sd) 32) 32)) - 32 + 1)).top * (Domain.Fprep.pow2Signed (Domain.Kernel.fracExpo (Domain.Rational.times sd (Domain.Kernel.compFrac (Domain.Kernel.awayFrac (Domain.Rational.times d sd) 32) 32)) - 32 + 1)).bottom * (Domain.Rational.absInt ((Domain.Rational.times sd (Domain.Kernel.compFrac (Domain.Kernel.awayFrac (Domain.Rational.times d sd) 32) 32)).bottom * 1) * Domain.Rational.absInt ((Domain.Rational.times sd (Domain.Kernel.compFrac (Domain.Kernel.awayFrac (Domain.Rational.times d sd) 32) 32)).bottom * 1)) :=
        Int.mul_pos (Int.mul_pos hutT hutB) (Int.mul_pos hbpos hbpos)
      simpa using hpos
    · have hL := {trunc_thm} (Domain.Rational.times sd (Domain.Kernel.compFrac (Domain.Kernel.awayFrac (Domain.Rational.times d sd) 32) 32)) 32 (by simp only [Bool.and_eq_true, bne_iff_ne]; exact ⟨hp0, hprodb⟩)
      simp only [{trunc_subj_fn}, {trunc_err_fn}] at hL
      exact hL
  obtain ⟨hminTruncb, huTb⟩ := tri_denoms _ _ hTrunc
  have hsnewb : (Domain.Kernel.truncFrac (Domain.Rational.times sd (Domain.Kernel.compFrac (Domain.Kernel.awayFrac (Domain.Rational.times d sd) 32) 32)) 32).bottom ≠ 0 := by
    simp only [Domain.Rational.absFraction, Domain.Rational.minus] at hminTruncb
    intro hc; apply hminTruncb; rw [hc]; simp [Domain.Rational.absInt]
  have hX := tri_sq_strict (Domain.Rational.minus (Domain.Rational.oneFraction) (Domain.Rational.times d sd)) eps hPrior
  have hY := tri_abs_times_le (Domain.Rational.times d sd) (Domain.Rational.minus (Domain.Kernel.awayFrac (Domain.Rational.times d sd) 32) (Domain.Rational.times d sd)) (Domain.Fprep.pow2Signed (Domain.Kernel.fracExpo (Domain.Rational.times d sd) - 32 + 1)) hAway
  have hZ := tri_abs_times_le d (Domain.Rational.minus (Domain.Rational.times sd (Domain.Kernel.compFrac (Domain.Kernel.awayFrac (Domain.Rational.times d sd) 32) 32)) (Domain.Kernel.truncFrac (Domain.Rational.times sd (Domain.Kernel.compFrac (Domain.Kernel.awayFrac (Domain.Rational.times d sd) 32) 32)) 32)) (Domain.Fprep.pow2Signed (Domain.Kernel.fracExpo (Domain.Rational.times sd (Domain.Kernel.compFrac (Domain.Kernel.awayFrac (Domain.Rational.times d sd) 32) 32)) - 32 + 1)) hTrunc
  have hsv := {vid} d sd (Domain.Kernel.awayFrac (Domain.Rational.times d sd) 32) (Domain.Kernel.truncFrac (Domain.Rational.times sd (Domain.Kernel.compFrac (Domain.Kernel.awayFrac (Domain.Rational.times d sd) 32) 32)) 32)
  have hEb : (Domain.Rational.minus (Domain.Rational.oneFraction) (Domain.Rational.times (d) (Domain.Kernel.truncFrac (Domain.Rational.times sd (Domain.Kernel.compFrac (Domain.Kernel.awayFrac (Domain.Rational.times d sd) 32) 32)) 32))).bottom ≠ 0 := Int.mul_ne_zero (by decide) (Int.mul_ne_zero hdb hsnewb)
  have hXb : (Domain.Rational.times (Domain.Rational.minus (Domain.Rational.oneFraction) (Domain.Rational.times d sd)) (Domain.Rational.minus (Domain.Rational.oneFraction) (Domain.Rational.times d sd))).bottom ≠ 0 := Int.mul_ne_zero (Int.mul_ne_zero (by decide) hqb) (Int.mul_ne_zero (by decide) hqb)
  have hYb : (Domain.Rational.times (Domain.Rational.times d sd) (Domain.Rational.minus (Domain.Kernel.awayFrac (Domain.Rational.times d sd) 32) (Domain.Rational.times d sd))).bottom ≠ 0 := Int.mul_ne_zero hqb (Int.mul_ne_zero hsddb hqb)
  have hZb : (Domain.Rational.times (d) (Domain.Rational.minus (Domain.Rational.times sd (Domain.Kernel.compFrac (Domain.Kernel.awayFrac (Domain.Rational.times d sd) 32) 32)) (Domain.Kernel.truncFrac (Domain.Rational.times sd (Domain.Kernel.compFrac (Domain.Kernel.awayFrac (Domain.Rational.times d sd) 32) 32)) 32))).bottom ≠ 0 := Int.mul_ne_zero hdb (Int.mul_ne_zero hprodb hsnewb)
  have hbyb : (Domain.Rational.times (Domain.Rational.absFraction (Domain.Rational.times d sd)) (Domain.Fprep.pow2Signed (Domain.Kernel.fracExpo (Domain.Rational.times d sd) - 32 + 1))).bottom ≠ 0 := Int.mul_ne_zero (tri_absInt_ne_zero _ hqb) huAb
  have hbzb : (Domain.Rational.times (Domain.Rational.absFraction (d)) (Domain.Fprep.pow2Signed (Domain.Kernel.fracExpo (Domain.Rational.times sd (Domain.Kernel.compFrac (Domain.Kernel.awayFrac (Domain.Rational.times d sd) 32) 32)) - 32 + 1))).bottom ≠ 0 := Int.mul_ne_zero (tri_absInt_ne_zero _ hdb) huTb
  exact tri_sum3 _ _ _ _ _ _ _ hsv hX hY hZ hEb hXb hYb hZb hbyb hbzb"#
    )
}

/// The recognised roles of a triangle-sum law. Every field is INFERRED from
/// the law structure / the dependency cone — nothing is keyed on the literal
/// names a particular figure happens to give its givens. The three given
/// roles (`d`/`sd`/`eps`) are discovered structurally in
/// [`recognize_triangle_sum_shape`] (claim arguments + prior-error premise +
/// guard set) and used ONLY there to validate the law; the emitted proof
/// quantifies over generic local binder names (alpha-equivalent to the law's
/// givens whatever they are), so no given name appears in this struct.
pub(super) struct TriangleSum {
    /// The magnitude-bound subject fn (Lean name) — the claim is
    /// `subj <g0> <g1> <g2> = true`.
    subj: String,
    /// The prior-error fn (Lean name) — the `when` premise bounds
    /// `absFraction (prior <g0> <g1>)`.
    prior: String,
    /// Goal unfold list: the entry-module wrapper fns in the law cone (minus
    /// `prior`), so `simp only [..]` exposes the primitive sum the kit matches.
    unfold_csv: String,
    /// Rounding precision (the `awayFrac`/`truncFrac` second argument).
    prec: i64,
    /// The cited away-rounding bound theorem (Lean name, resolved via `open`).
    away_thm: String,
    /// The cited trunc-rounding bound theorem (Lean name, resolved via `open`).
    trunc_thm: String,
    /// The away-bound dependency's subject fn and inner error fn, qualified —
    /// the `simp only [..]` unfold that exposes the cited away bound's
    /// conclusion. Discovered from the dependency law, not hardcoded.
    away_subj_fn: String,
    away_err_fn: String,
    /// The trunc-bound dependency's subject fn and inner error fn, qualified.
    trunc_subj_fn: String,
    trunc_err_fn: String,
    /// The two cited bound laws as `(module_prefix, theorem_base)` — the
    /// cross-file admission keys on these so the dep theorems are emitted.
    cited_deps: Vec<(String, String)>,
}

/// `<var>.<field> != 0` ⇒ `(var, field)`.
fn nonzero_guard(expr: &Spanned<Expr>) -> Option<(String, String)> {
    let Expr::BinOp(crate::ast::BinOp::Neq, l, r) = &expr.node else {
        return None;
    };
    if !matches!(r.node, Expr::Literal(crate::ast::Literal::Int(0))) {
        return None;
    }
    let Expr::Attr(base, field) = &l.node else {
        return None;
    };
    Some((shared::ident_name(base)?.to_string(), field.clone()))
}

/// Whether `fn_name`'s def is a rounding-error bound law subject: a `Bool`
/// fn whose body is `lessThan (absFraction (E ..)) (pow2Signed (..))` whose
/// inner error fn `E` calls `awayFrac` (returns `Some((true, E))`) or
/// `truncFrac` (`Some((false, E))`). Name-blind — keyed on the rounding
/// primitive used. The second tuple element is the inner error fn's short
/// name, so the citation can unfold the dependency's definition structurally.
fn bound_law_kind(ctx: &CodegenContext, fn_name: &str) -> Option<(bool, String)> {
    let fd = find_fn_def_by_call_name(ctx, fn_name)?;
    if fd.return_type != "Bool" {
        return None;
    }
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return None;
    };
    let lt = shared::call_named(body, "lessThan", 2)?;
    let abs = shared::call_named(&lt[0], "absFraction", 1)?;
    // RHS must be a pow2Signed ulp.
    shared::call_named(&lt[1], "pow2Signed", 1)?;
    let (efn_short, _) = shared::short_call_name_args(&abs[0])?;
    let efd = find_fn_def_by_call_name(ctx, &efn_short)?;
    let [Stmt::Expr(ebody)] = efd.body.stmts() else {
        return None;
    };
    let mut callees = std::collections::BTreeSet::new();
    shared::collect_short_callees(ebody, &mut callees);
    if callees.contains("awayFrac") {
        Some((true, efn_short))
    } else if callees.contains("truncFrac") {
        Some((false, efn_short))
    } else {
        None
    }
}

/// A dependency-module rounding bound law, all names DISCOVERED from the
/// dependency (never hardcoded). `theorem_base` keys the cross-file
/// admission; `theorem_lean` is the bare citation name (resolved via `open
/// M`); `subj_fn` / `err_fn` are the qualified dependency subject / inner
/// error fns the citation `simp only [..]` unfolds.
struct RoundingBoundLaw {
    prefix: String,
    theorem_base: String,
    theorem_lean: String,
    subj_fn: String,
    err_fn: String,
}

/// Find the dependency-module rounding bound law of the requested kind
/// (`want_away`).
fn find_rounding_bound_law(ctx: &CodegenContext, want_away: bool) -> Option<RoundingBoundLaw> {
    for module in &ctx.modules {
        for vb in &module.verify_laws {
            let VerifyKind::Law(law) = &vb.kind else {
                continue;
            };
            let Some((kind, err_short)) = bound_law_kind(ctx, &vb.fn_name) else {
                continue;
            };
            if kind != want_away {
                continue;
            }
            let base = format!(
                "{}_law_{}",
                aver_name_to_lean(&vb.fn_name),
                aver_name_to_lean(&law.name)
            );
            return Some(RoundingBoundLaw {
                prefix: module.prefix.clone(),
                theorem_base: base.clone(),
                theorem_lean: base,
                subj_fn: format!("{}.{}", module.prefix, aver_name_to_lean(&vb.fn_name)),
                err_fn: format!("{}.{}", module.prefix, aver_name_to_lean(&err_short)),
            });
        }
    }
    None
}

/// Extract the rounding precision: the second-argument int literal of an
/// `awayFrac` / `truncFrac` / `compFrac` call anywhere in the cone fns.
fn extract_precision(
    ctx: &CodegenContext,
    cone: &std::collections::BTreeSet<String>,
) -> Option<i64> {
    fn walk(expr: &Spanned<Expr>) -> Option<i64> {
        shared::find_map_short_call_tree(expr, &mut |short, args| {
            if matches!(short, "awayFrac" | "truncFrac" | "compFrac")
                && args.len() == 2
                && let Expr::Literal(crate::ast::Literal::Int(k)) = &args[1].node
            {
                Some(*k)
            } else {
                None
            }
        })
    }
    for name in cone {
        if let Some(fd) = find_fn_def(ctx, name) {
            for stmt in fd.body.stmts() {
                if let Stmt::Expr(e) | Stmt::Binding(_, _, e) = stmt
                    && let Some(p) = walk(e)
                {
                    return Some(p);
                }
            }
        }
    }
    None
}

/// Recognise the triangle-sum shape. Pure / name-blind — declines (so the law
/// keeps its bounded sampled fallback) unless EVERY structural gate holds.
pub(super) fn recognize_triangle_sum_shape(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Option<TriangleSum> {
    // Conditional `subject(d, sd, eps) holds`.
    law.when.as_ref()?;
    if !matches!(law.rhs.node, Expr::Literal(crate::ast::Literal::Bool(true))) {
        return None;
    }
    if law.givens.len() != 3 || !law.givens.iter().all(|g| g.type_name.trim() == "Fraction") {
        return None;
    }
    let Expr::FnCall(callee, claim_args) = &law.lhs.node else {
        return None;
    };
    if claim_args.len() != 3 {
        return None;
    }
    // STRUCTURAL ROLES — discovered from the claim, never matched against any
    // literal name. `d`-role = first subject argument (the value the squared
    // prior error is built from), `sd`-role = second, `eps`-role = third (the
    // error bound). The `when` analysis below confirms the prior-error premise
    // bounds `absFraction (prior d sd)` by `eps`, pinning these roles to the
    // law structure rather than to the names a figure happens to choose.
    let d = shared::ident_name(&claim_args[0])?.to_string();
    let sd = shared::ident_name(&claim_args[1])?.to_string();
    let eps = shared::ident_name(&claim_args[2])?.to_string();
    // The three roles must be three DISTINCT givens (so the discharged
    // statement quantifies over exactly the law's givens, name-blind).
    let roles: std::collections::BTreeSet<&str> = [d.as_str(), sd.as_str(), eps.as_str()]
        .into_iter()
        .collect();
    let given_names: std::collections::BTreeSet<&str> =
        law.givens.iter().map(|g| g.name.as_str()).collect();
    if roles.len() != 3 || roles != given_names {
        return None;
    }
    let subject_src = expr_dotted_name(callee)?;

    // Subject body: lessThan (absFraction (E p0 p1)) (B ..).
    let subj_fd = find_fn_def_by_call_name(ctx, &subject_src)?;
    if subj_fd.return_type != "Bool" || subj_fd.params.len() != 3 || !subj_fd.effects.is_empty() {
        return None;
    }
    let [Stmt::Expr(sbody)] = subj_fd.body.stmts() else {
        return None;
    };
    let lt = shared::call_named(sbody, "lessThan", 2)?;
    shared::call_named(&lt[0], "absFraction", 1)?;

    // `when`: four `<d|sd>.<top|bottom> != 0` guards + one prior-error bound.
    let when = law.when.as_ref()?;
    let conj = shared::collect_when_clauses(when);
    if conj.len() != 5 {
        return None;
    }
    let mut guards: std::collections::BTreeSet<(String, String)> =
        std::collections::BTreeSet::new();
    let mut prior: Option<String> = None;
    for c in &conj {
        if let Some(g) = nonzero_guard(c) {
            guards.insert(g);
            continue;
        }
        // The prior-error bound: `lessThan (absFraction (prior d sd)) eps`.
        // The prior call's two arguments must be the `d`/`sd` roles in order
        // and its bound the `eps` role — this is what pins the discovered
        // roles to the law structure (name-blind).
        if let Some(pa) = shared::call_named(c, "lessThan", 2)
            && let Some(abs) = shared::call_named(&pa[0], "absFraction", 1)
            && let Some((pfn, pargs)) = shared::short_call_name_args(&abs[0])
            && pargs.len() == 2
            && shared::ident_name(&pargs[0]) == Some(d.as_str())
            && shared::ident_name(&pargs[1]) == Some(sd.as_str())
            && shared::ident_name(&pa[1]) == Some(eps.as_str())
        {
            prior = Some(pfn);
            continue;
        }
        return None;
    }
    // The four nonzero guards must be exactly `top`/`bottom` of the two VALUE
    // roles (`d`, `sd`) — built from the discovered role names, not a literal
    // `(name, field)` list. The `eps` role carries no guard (it is only a
    // bound), matching the assembled statement.
    let expect: std::collections::BTreeSet<(String, String)> = [
        (d.clone(), "top".to_string()),
        (d.clone(), "bottom".to_string()),
        (sd.clone(), "top".to_string()),
        (sd.clone(), "bottom".to_string()),
    ]
    .into_iter()
    .collect();
    if guards != expect {
        return None;
    }
    let prior_src = prior?;

    // Cone: the rounded-NR-step round primitives must all be present.
    let cone = law_simp_source_names(ctx, vb, law);
    for prim in [
        "awayFrac",
        "truncFrac",
        "compFrac",
        "pow2Signed",
        "fracExpo",
    ] {
        // syntax-discovery-only: cone entries are source call spellings —
        // bare in the defining module, dep-qualified elsewhere. Presence
        // gates the attempt; the kernel judges it.
        if !cone.iter().any(|n| n.rsplit('.').next() == Some(prim)) {
            return None;
        }
    }

    // The two cited rounding bound universals (dep modules) — every name
    // discovered from the dependency law, nothing hardcoded.
    let away = find_rounding_bound_law(ctx, true)?;
    let trunc = find_rounding_bound_law(ctx, false)?;

    let prec = extract_precision(ctx, &cone)?;

    // Goal unfold list: entry-module wrapper fns in the cone, minus `prior`.
    let prior_lean = aver_name_to_lean(&prior_src);
    let mut unfold: Vec<String> = cone
        .iter()
        .filter(|n| find_fn_def(ctx, n).is_some() && ctx.fn_defs.iter().any(|fd| &fd.name == *n))
        .map(|n| aver_name_to_lean(n))
        .filter(|n| n != &prior_lean)
        .collect();
    unfold.sort();
    unfold.dedup();
    if unfold.is_empty() {
        return None;
    }

    Some(TriangleSum {
        subj: aver_name_to_lean(&subject_src),
        prior: prior_lean,
        unfold_csv: unfold.join(", "),
        prec,
        away_thm: away.theorem_lean,
        trunc_thm: trunc.theorem_lean,
        away_subj_fn: away.subj_fn,
        away_err_fn: away.err_fn,
        trunc_subj_fn: trunc.subj_fn,
        trunc_err_fn: trunc.err_fn,
        cited_deps: vec![
            (away.prefix, away.theorem_base),
            (trunc.prefix, trunc.theorem_base),
        ],
    })
}

/// Statement-builder hook (mirror of the emit): whether the triangle-sum arm
/// will close this law universally, so the caller drops the sampled domain.
pub(in crate::codegen::lean) fn recognize_triangle_sum(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> bool {
    recognize_triangle_sum_shape(vb, law, ctx).is_some()
}

/// The `(module_prefix, theorem_base)` rounding bound laws the triangle-sum
/// arm cites for this law — unioned into the cross-file admission set so the
/// dep theorems are actually emitted.
pub(in crate::codegen::lean) fn triangle_sum_cited_deps(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Vec<(String, String)> {
    recognize_triangle_sum_shape(vb, law, ctx)
        .map(|t| t.cited_deps)
        .unwrap_or_default()
}

/// Close a triangle-sum law. Emits the generic helper-lemma kit plus the
/// per-law value identity and the fixed assembly as a self-contained
/// TRUE-universal theorem (`replaces_theorem`), wrapped `first | (…) | sorry`.
pub(super) fn emit_triangle_sum_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    theorem_base: &str,
) -> Option<AutoProof> {
    let t = recognize_triangle_sum_shape(vb, law, ctx)?;
    let prec = t.prec.to_string();
    // The per-law value-identity name carries NO `tri_`, so the kit-prefix
    // pass below leaves it untouched.
    let vid_thm = format!("{theorem_base}__value_identity");
    // Per-law prefix for the generic helper kit, so two triangle-sum laws in
    // one module never collide on a duplicate declaration.
    let p = format!("{theorem_base}_ts_");

    let vid = VALUE_IDENTITY_TEMPLATE
        .replace("nr_value_identity", &vid_thm)
        .replace("32", &prec);

    let assembly = render_assembly(
        theorem_base,
        &t.subj,
        &t.prior,
        &t.unfold_csv,
        &t.away_thm,
        &t.away_subj_fn,
        &t.away_err_fn,
        &t.trunc_thm,
        &t.trunc_subj_fn,
        &t.trunc_err_fn,
        &vid_thm,
    )
    .replace("32", &prec);

    // Prefix every generic helper (`tri_*` and the `aver_sq_nonneg` base
    // case) and every reference to them — in BOTH the kit definitions and the
    // assembly's citations — with the per-law prefix `p`. `tri_` is replaced
    // before `aver_sq_nonneg` so the prefix inserted for the latter is never
    // re-scanned.
    let text = format!("{TRIANGLE_KIT}\n{vid}\n{assembly}")
        .replace("tri_", &format!("{p}tri_"))
        .replace("aver_sq_nonneg", &format!("{p}aver_sq_nonneg"));
    Some(AutoProof {
        support_lines: text.lines().map(|l| l.to_string()).collect(),
        body: crate::codegen::lean::tactic_ir::Tactic::raw(Vec::new()),
        replaces_theorem: true,
    })
}

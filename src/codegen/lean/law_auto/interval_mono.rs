//! Lean renderer for the interval-monotonicity rung — a magnitude bound
//! that is affine in an interval variable over the exact-rational order
//! (`Domain.Rational`), the table-bucket family of `projects/k5_fdiv`.
//!
//! The rung closes ONE generic shape, name-blind on the law and given
//! names (keyed only on the Rational order primitives `lessThan` /
//! `absFraction` / `minus` / `times` / `oneFraction` / `isNonNeg` /
//! `negate`): a conditional `holds` law
//!
//! ```text
//! when lo ≤ d, d ≤ hi, 0 ≤ v, d.bottom ≠ 0,
//!      |lo·v − 1| < e, |hi·v − 1| < e   ⟹   |d·v − 1| < e
//! ```
//!
//! where the magnitude `|d·v − 1| = absFraction (minus (times d v) one)`
//! is affine in the interval variable `d` and `v`, `e` do not mention it.
//! The proof is the rational interval-monotonicity argument: `d·v − 1`
//! lies between the two endpoint values `lo·v − 1` and `hi·v − 1`, both of
//! which are within `e` of zero, so the midpoint value is too.
//!
//! The renderer emits a prelude-style kit of GENERIC, name-blind helper
//! lemmas (rational `lessThan` transitivity through a `≤`/`<` chain, the
//! 4-way abs sign-split extract/intro, the scaled-difference
//! monotonicities, the denominator non-vacuity reader) — each provable on
//! its own, reusable for ANY such law — then a fixed ~25-line assembly.
//! The whole assembly sits under a `first | (…) | sorry` floor, so a law
//! whose shape the recognizer admits but whose closing slips falls to an
//! honest caught `sorry`; credit stays fail-closed behind the `#print
//! axioms` whitelist. The generic helper lemmas always typecheck (they do
//! not mention the law), so only the assembly can fall through.
//!
//! Soundness: the `d.bottom ≠ 0` guard is load-bearing — at a zero
//! denominator the interval premises degenerate to `0 ≥ 0` (true) while
//! the goal degenerates to `0 < 0` (false), so the law is FALSE without
//! it. The recognizer requires the guard; the non-vacuous endpoint bounds
//! supply the remaining nonzero denominators the transitivity needs.

use super::AutoProof;
use super::aver_name_to_lean;
use super::shared::{expr_dotted_name, find_fn_def_by_call_name};
use crate::ast::{Expr, Spanned, Stmt, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;

/// The five role-assigned given names (source) plus the magnitude-bound
/// subject fn — everything the fixed assembly template needs.
pub(super) struct IntervalMono {
    /// Interval variable (`d`): appears inside the `times` of the claim and
    /// of both endpoint premises, and in the `d.bottom ≠ 0` guard.
    d: String,
    /// Lower endpoint (`lo`): `isNonNeg (minus d lo)` ⇒ `lo ≤ d`.
    lo: String,
    /// Upper endpoint (`hi`): `isNonNeg (minus hi d)` ⇒ `d ≤ hi`.
    hi: String,
    /// The scaled value (`v`): the second `times` factor; `isNonNeg v`.
    v: String,
    /// The bound (`e`): the `lessThan` right-hand side.
    e: String,
    /// The magnitude-bound subject fn (source name).
    subject: String,
}

/// `(short_callee_name, args)` when `expr` is a function call, else `None`.
/// The short name is the last dotted component, so a qualified
/// `Domain.Rational.minus` call matches the primitive `minus` — the rung
/// keys on the rational ALGEBRA primitive, never on the law/figure name.
fn as_call(expr: &Spanned<Expr>) -> Option<(String, &[Spanned<Expr>])> {
    let Expr::FnCall(callee, args) = &expr.node else {
        return None;
    };
    let dotted = expr_dotted_name(callee)?;
    let short = dotted.rsplit('.').next().unwrap_or(&dotted).to_string();
    Some((short, args.as_slice()))
}

/// A call to the named primitive with exactly `n` args.
fn call_named<'a>(expr: &'a Spanned<Expr>, name: &str, n: usize) -> Option<&'a [Spanned<Expr>]> {
    let (short, args) = as_call(expr)?;
    (short == name && args.len() == n).then_some(args)
}

/// The bare identifier this expr names (an ident / resolved binding), else
/// `None`. Used to pin a sub-expression to a given variable.
fn ident_of(expr: &Spanned<Expr>) -> Option<String> {
    match &expr.node {
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => Some(n.clone()),
        _ => None,
    }
}

/// `absFraction (minus (times <X> <Y>) (oneFraction))` ⇒ `(X_ident, Y_ident)`.
/// The shared magnitude shape of the claim and both endpoint premises.
fn match_abs_magnitude(expr: &Spanned<Expr>) -> Option<(String, String)> {
    let abs_args = call_named(expr, "absFraction", 1)?;
    let minus_args = call_named(&abs_args[0], "minus", 2)?;
    let times_args = call_named(&minus_args[0], "times", 2)?;
    // second minus operand must be `oneFraction()` (0-arg call)
    call_named(&minus_args[1], "oneFraction", 0)?;
    let x = ident_of(&times_args[0])?;
    let y = ident_of(&times_args[1])?;
    Some((x, y))
}

/// Flatten a left-nested `Bool.and(Bool.and(…), w)` `when` tree into its
/// atomic conjuncts.
fn flatten_and<'a>(expr: &'a Spanned<Expr>, out: &mut Vec<&'a Spanned<Expr>>) {
    if let Some(args) = call_named(expr, "and", 2) {
        flatten_and(&args[0], out);
        flatten_and(&args[1], out);
    } else {
        out.push(expr);
    }
}

/// Recognize the interval-monotonicity shape. Pure / name-blind on the law
/// — see [`IntervalMono`]. Declines (so the law keeps its bounded sampled
/// fallback / other rung) unless EVERY structural gate holds.
pub(super) fn recognize_interval_monotonicity_shape(
    _vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Option<IntervalMono> {
    // Five givens, all `Fraction`.
    if law.givens.len() != 5 {
        return None;
    }
    if !law.givens.iter().all(|g| g.type_name.trim() == "Fraction") {
        return None;
    }
    let given_names: std::collections::BTreeSet<String> =
        law.givens.iter().map(|g| g.name.clone()).collect();

    // Conclusion `subject(d, v, e) holds` ⇒ `subject(d, v, e) = true`.
    if !matches!(law.rhs.node, Expr::Literal(crate::ast::Literal::Bool(true))) {
        return None;
    }
    let Expr::FnCall(callee, claim_args) = &law.lhs.node else {
        return None;
    };
    if claim_args.len() != 3 {
        return None;
    }
    let subject_src = expr_dotted_name(callee)?;
    let subject_short = subject_src
        .rsplit('.')
        .next()
        .unwrap_or(&subject_src)
        .to_string();
    let d = ident_of(&claim_args[0])?;
    let v = ident_of(&claim_args[1])?;
    let e = ident_of(&claim_args[2])?;

    // Subject body must be the magnitude bound `lessThan (absFraction
    // (minus (times p0 p1) oneFraction)) p2` over its three params — this
    // is the shape gate that makes the rung an honest interval-magnitude
    // closer and not a blanket conditional-Fraction stealer.
    let subj_fd = find_fn_def_by_call_name(ctx, &subject_src)?;
    if subj_fd.return_type != "Bool" || subj_fd.params.len() != 3 || !subj_fd.effects.is_empty() {
        return None;
    }
    let [Stmt::Expr(body)] = subj_fd.body.stmts() else {
        return None;
    };
    let lt_args = call_named(body, "lessThan", 2)?;
    let (mx, my) = match_abs_magnitude(&lt_args[0])?;
    let p0 = &subj_fd.params[0].0;
    let p1 = &subj_fd.params[1].0;
    let p2 = &subj_fd.params[2].0;
    if &mx != p0 || &my != p1 || ident_of(&lt_args[1]).as_deref() != Some(p2.as_str()) {
        return None;
    }

    // Six `when` conjuncts, matched to the six expected shapes.
    let when = law.when.as_ref()?;
    let mut conj: Vec<&Spanned<Expr>> = Vec::new();
    flatten_and(when, &mut conj);
    if conj.len() != 6 {
        return None;
    }

    // A `subject(arg0, v, e)` endpoint premise ⇒ arg0 ident.
    let subject_endpoint = |c: &Spanned<Expr>| -> Option<String> {
        let (short, args) = as_call(c)?;
        if short != subject_short || args.len() != 3 {
            return None;
        }
        if ident_of(&args[1]).as_deref() != Some(v.as_str())
            || ident_of(&args[2]).as_deref() != Some(e.as_str())
        {
            return None;
        }
        ident_of(&args[0])
    };

    // Pass 1: bind lo/hi from the `isNonNeg(minus …)` pins and check the
    // `isNonNeg v` and `d.bottom ≠ 0` premises. Every conjunct must be one
    // of the six recognized shapes (an endpoint premise is left for pass 2).
    let mut lo: Option<String> = None;
    let mut hi: Option<String> = None;
    let mut saw_v_nonneg = false;
    let mut saw_guard = false;
    for c in &conj {
        if let Some(args) = call_named(c, "isNonNeg", 1) {
            if let Some(margs) = call_named(&args[0], "minus", 2) {
                let a0 = ident_of(&margs[0]);
                let a1 = ident_of(&margs[1]);
                if a0.as_deref() == Some(d.as_str()) {
                    lo = a1.or(lo); // minus(d, lo) ⇒ lo ≤ d
                } else if a1.as_deref() == Some(d.as_str()) {
                    hi = a0.or(hi); // minus(hi, d) ⇒ d ≤ hi
                } else {
                    return None;
                }
                continue;
            }
            if ident_of(&args[0]).as_deref() == Some(v.as_str()) {
                saw_v_nonneg = true;
                continue;
            }
            return None;
        }
        if let Expr::BinOp(crate::ast::BinOp::Neq, l, r) = &c.node {
            if expr_dotted_name(l).as_deref() == Some(format!("{d}.bottom").as_str())
                && matches!(r.node, Expr::Literal(crate::ast::Literal::Int(0)))
            {
                saw_guard = true;
                continue;
            }
            return None;
        }
        // Otherwise it must be a subject endpoint premise (validated in pass 2).
        subject_endpoint(c)?;
    }

    let lo = lo?;
    let hi = hi?;
    // Pass 2: both endpoint premises present now that lo/hi are bound.
    let mut saw_lo_endpoint = false;
    let mut saw_hi_endpoint = false;
    for c in &conj {
        if let Some(arg0) = subject_endpoint(c) {
            if arg0 == lo {
                saw_lo_endpoint = true;
            } else if arg0 == hi {
                saw_hi_endpoint = true;
            }
        }
    }
    if !(saw_v_nonneg && saw_guard && saw_lo_endpoint && saw_hi_endpoint) {
        return None;
    }
    // Roles must be exactly the five distinct givens.
    let roles: std::collections::BTreeSet<String> = [&d, &lo, &hi, &v, &e]
        .iter()
        .map(|s| (*s).clone())
        .collect();
    if roles.len() != 5 || roles != given_names {
        return None;
    }

    Some(IntervalMono {
        d,
        lo,
        hi,
        v,
        e,
        subject: subject_src,
    })
}

/// Statement-builder hook: whether the interval-monotonicity emit will
/// close this law universally (so the caller drops the sampled domain and
/// classes it `universal`, keeping statement and proof in lockstep).
pub(in crate::codegen::lean) fn recognize_interval_monotonicity(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> bool {
    recognize_interval_monotonicity_shape(vb, law, ctx).is_some()
}

/// Close an interval-monotonicity law. Emits the generic helper-lemma kit
/// plus the fixed assembly as a self-contained TRUE-universal theorem
/// (`replaces_theorem`), wrapped `first | (…) | sorry`.
pub(super) fn emit_interval_monotonicity_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    theorem_base: &str,
) -> Option<AutoProof> {
    let IntervalMono {
        d,
        lo,
        hi,
        v,
        e,
        subject,
    } = recognize_interval_monotonicity_shape(vb, law, ctx)?;
    let d = aver_name_to_lean(&d);
    let lo = aver_name_to_lean(&lo);
    let hi = aver_name_to_lean(&hi);
    let v = aver_name_to_lean(&v);
    let e = aver_name_to_lean(&e);
    let subj = aver_name_to_lean(&subject);
    // Per-law prefix for the generic helper lemma names, so two such laws
    // in one module never collide on a duplicate declaration.
    let p = format!("{theorem_base}_im_");

    let text = render_interval_mono(theorem_base, &p, &subj, &d, &lo, &hi, &v, &e);
    Some(AutoProof {
        support_lines: text.lines().map(|l| l.to_string()).collect(),
        body: crate::codegen::lean::tactic_ir::Tactic::raw(Vec::new()),
        replaces_theorem: true,
    })
}

#[allow(clippy::too_many_arguments)]
fn render_interval_mono(
    base: &str,
    p: &str,
    subj: &str,
    d: &str,
    lo: &str,
    hi: &str,
    v: &str,
    e: &str,
) -> String {
    // The generic helper kit (name-blind; provable on its own). Names are
    // prefixed with `{p}` for per-law uniqueness. `aver_sq_nonneg` /
    // `aver_int_order` come from AverCommon (triggered by the literal
    // `aver_int_order` appearing in this body).
    let helpers = format!(
        r#"set_option maxHeartbeats 1000000 in
theorem {p}absMulSelf (n : Int) : absInt n * absInt n = n * n := by
  by_cases h : n < 0 <;> simp only [absInt, h, if_true, if_false] <;> grind
set_option maxHeartbeats 1000000 in
theorem {p}leAbsMul (m n : Int) : m * n ≤ absInt m * absInt n := by
  by_cases h1 : m < 0 <;> by_cases h2 : n < 0 <;>
    simp only [absInt, h1, h2, if_true, if_false] <;>
    first
      | grind
      | (apply Int.mul_le_mul_of_nonneg_right <;> omega)
      | (apply Int.mul_le_mul_of_nonneg_left <;> omega)
set_option maxHeartbeats 1000000 in
theorem {p}absExtract (x b : Fraction)
    (h : lessThan (absFraction x) b = true) : lessThan x b = true := by
  simp only [lessThan, absFraction, decide_eq_true_eq] at h ⊢
  rw [{p}absMulSelf] at h
  have key := Int.mul_le_mul_of_nonneg_right ({p}leAbsMul x.top x.bottom)
    (aver_sq_nonneg b.bottom)
  omega
set_option maxHeartbeats 1000000 in
theorem {p}absIntro (x b : Fraction)
    (h1 : lessThan x b = true) (h2 : lessThan (negate x) b = true) :
    lessThan (absFraction x) b = true := by
  simp only [lessThan, absFraction, negate, decide_eq_true_eq] at h1 h2 ⊢
  by_cases ht : x.top < 0 <;> by_cases hb2 : x.bottom < 0 <;>
    simp only [absInt, ht, hb2, if_true, if_false] <;> grind
set_option maxHeartbeats 1000000 in
theorem {p}absNeg (x : Fraction) : absFraction (negate x) = absFraction x := by
  simp only [absFraction, negate]
  by_cases h : x.top < 0 <;> simp only [absInt, h, if_true, if_false] <;>
    (congr 1 <;> grind)
set_option maxHeartbeats 1000000 in
theorem {p}leLtTrans (a b c : Fraction)
    (hab : isNonNeg (minus b a) = true) (hbc : lessThan b c = true)
    (ha : a.bottom ≠ 0) (hb : b.bottom ≠ 0) : lessThan a c = true := by
  simp only [isNonNeg, minus, lessThan, decide_eq_true_eq, ge_iff_le] at hab hbc ⊢
  have hq2 : 0 < a.bottom * a.bottom := by
    rcases (by omega : a.bottom < 0 ∨ 0 < a.bottom) with h | h
    · have : 0 < (-a.bottom) * (-a.bottom) := Int.mul_pos (by omega) (by omega)
      rwa [Int.neg_mul_neg] at this
    · exact Int.mul_pos h h
  have hs2 : 0 < b.bottom * b.bottom := by
    rcases (by omega : b.bottom < 0 ∨ 0 < b.bottom) with h | h
    · have : 0 < (-b.bottom) * (-b.bottom) := Int.mul_pos (by omega) (by omega)
      rwa [Int.neg_mul_neg] at this
    · exact Int.mul_pos h h
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
set_option maxHeartbeats 1000000 in
theorem {p}monoR (a b w : Fraction)
    (hab : isNonNeg (minus b a) = true) (hw : isNonNeg w = true) :
    isNonNeg (minus (minus (times b w) oneFraction) (minus (times a w) oneFraction)) = true := by
  simp only [isNonNeg, minus, times, oneFraction, decide_eq_true_eq, ge_iff_le] at hab hw ⊢
  have hp : (0:Int) ≤ (b.top * a.bottom - a.top * b.bottom) * (b.bottom * a.bottom)
                      * ((w.top * w.bottom) * (w.bottom * w.bottom)) := by aver_int_order
  grind
set_option maxHeartbeats 1000000 in
theorem {p}monoRneg (a b w : Fraction)
    (hab : isNonNeg (minus b a) = true) (hw : isNonNeg w = true) :
    isNonNeg (minus (negate (minus (times a w) oneFraction))
                    (negate (minus (times b w) oneFraction))) = true := by
  simp only [isNonNeg, minus, times, negate, oneFraction, decide_eq_true_eq, ge_iff_le] at hab hw ⊢
  have hp : (0:Int) ≤ (b.top * a.bottom - a.top * b.bottom) * (b.bottom * a.bottom)
                      * ((w.top * w.bottom) * (w.bottom * w.bottom)) := by aver_int_order
  grind
set_option maxHeartbeats 1000000 in
theorem {p}denoms (x b : Fraction) (h : lessThan x b = true) :
    x.bottom ≠ 0 ∧ b.bottom ≠ 0 := by
  simp only [lessThan, decide_eq_true_eq] at h
  constructor <;> intro hc <;> rw [hc] at h <;> simp at h"#
    );

    let assembly = format!(
        r#"set_option maxHeartbeats 1000000 in
theorem {base} : ∀ ({d} {lo} {hi} {v} {e} : Fraction),
    isNonNeg (minus {d} {lo}) = true →
    isNonNeg (minus {hi} {d}) = true →
    isNonNeg {v} = true →
    {d}.bottom ≠ 0 →
    {subj} {lo} {v} {e} = true →
    {subj} {hi} {v} {e} = true →
    {subj} {d} {v} {e} = true := by
  intro {d} {lo} {hi} {v} {e} h1 h2 h3 h4 h5 h6
  first
  | (simp only [{subj}] at h5 h6 ⊢
     obtain ⟨hPlo_b, _⟩ := {p}denoms _ _ h5
     obtain ⟨hPhi_b, _⟩ := {p}denoms _ _ h6
     have hPhi_raw : (minus (times {hi} {v}) oneFraction).bottom ≠ 0 := by
       simp only [absFraction] at hPhi_b
       intro hc; apply hPhi_b; rw [hc]; simp [absInt]
     have hPlo_raw : (minus (times {lo} {v}) oneFraction).bottom ≠ 0 := by
       simp only [absFraction] at hPlo_b
       intro hc; apply hPlo_b; rw [hc]; simp [absInt]
     have hvb : {v}.bottom ≠ 0 := by
       simp only [minus, times, oneFraction] at hPhi_raw
       intro hc; apply hPhi_raw; rw [hc]; simp
     have hP_raw : (minus (times {d} {v}) oneFraction).bottom ≠ 0 := by
       simp only [minus, times, oneFraction]
       exact Int.mul_ne_zero (Int.mul_ne_zero h4 hvb) (by decide)
     apply {p}absIntro
     · have hPhi_lt : lessThan (minus (times {hi} {v}) oneFraction) {e} = true :=
         {p}absExtract _ _ h6
       have hmono : isNonNeg (minus (minus (times {hi} {v}) oneFraction)
                                    (minus (times {d} {v}) oneFraction)) = true :=
         {p}monoR {d} {hi} {v} h2 h3
       exact {p}leLtTrans _ _ _ hmono hPhi_lt hP_raw hPhi_raw
     · have h5' : lessThan (absFraction (negate (minus (times {lo} {v}) oneFraction))) {e} = true := by
         rw [{p}absNeg]; exact h5
       have hnegPlo_lt : lessThan (negate (minus (times {lo} {v}) oneFraction)) {e} = true :=
         {p}absExtract _ _ h5'
       have hmono2 : isNonNeg (minus (negate (minus (times {lo} {v}) oneFraction))
                                     (negate (minus (times {d} {v}) oneFraction))) = true :=
         {p}monoRneg {lo} {d} {v} h1 h3
       have hnegP_b : (negate (minus (times {d} {v}) oneFraction)).bottom ≠ 0 := by
         simp only [negate]; exact hP_raw
       have hnegPlo_b : (negate (minus (times {lo} {v}) oneFraction)).bottom ≠ 0 := by
         simp only [negate]; exact hPlo_raw
       exact {p}leLtTrans _ _ _ hmono2 hnegPlo_lt hnegP_b hnegPlo_b)
  | sorry"#
    );

    format!("{helpers}\n{assembly}")
}

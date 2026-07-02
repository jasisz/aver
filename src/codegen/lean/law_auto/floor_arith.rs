//! CONTENT-BLIND Euclidean-floor arithmetic rungs — two shape-only recognizers,
//! siblings of the nested-floor collapse, closing in pure core (NO Mathlib, NO
//! `native_decide`).
//!
//! ```text
//!   floor (a * c) (d * c) = floor a d          (0 < d, 0 < c)      [cancelCommonFactor]
//!   floor (d * q + r) d   = q                  (0 < d, 0 <= r, r < d) [absorbRemainder]
//! ```
//!
//! `floor` is a Euclidean floor-division fn (body `withDefault (Int.div a d) 0`,
//! recognized by SHAPE via [`is_euclidean_floor_fn`], never by name). Every atom
//! is captured STRUCTURALLY from the law's AST — no `pow2` / `trunc` / `sticky` /
//! K5 literal anywhere in the recognizer or the emitted proof, so the same rungs
//! fire on the K5 laws AND on a plain-integer floor-division fn in any other
//! module (the cross-domain second witness that proves they are shape-only).
//!
//! Both recognizers are ORIENTATION-TOLERANT: the shared factor may sit on
//! either side of each product, and the divisor multiplicand on either side of
//! `d * q`, independently. The emitted proof normalizes the written orientation
//! to the core-lemma's canonical form with an explicit `Int.mul_comm` step, then
//! closes by a CORE lemma: `Int.mul_ediv_mul_of_pos_left` cancels the shared
//! positive factor, and `Int.add_mul_ediv_left` + `Int.ediv_eq_zero_of_lt`
//! absorb the bounded remainder. The positivity / bound facts are read off the
//! law's `when` guard.

use super::AutoProof;
use super::aver_name_to_lean;
use super::shared::{
    clause_gives_nonneg, clause_gives_pos, clause_is_lt, expr_dotted_name, flatten_and, floor_call,
    is_euclidean_floor_fn, render,
};
use crate::ast::{BinOp, Expr, Spanned, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;

/// The shared `floor a d = a / d` peel lemma text (positive divisor), keyed to a
/// per-law unique base so two floor rungs in the same module never collide.
fn floordiv_eq_lemma(base: &str, floor: &str) -> String {
    format!(
        r#"theorem {base}__floordiv_eq (a d : Int) (hd : 0 < d) : {floor} a d = a / d := by
  have hne : ¬((d == 0) = true) := by simp only [beq_iff_eq]; omega
  simp only [{floor}]
  rw [if_neg hne]
  simp only [Except.withDefault]"#
    )
}

fn intro_names(law: &VerifyLaw) -> String {
    law.givens
        .iter()
        .map(|g| aver_name_to_lean(&g.name))
        .collect::<Vec<_>>()
        .join(" ")
}

/// In a two-operand product whose children render to `l_r` / `r_r`, find the one
/// that equals `target`; return the OTHER child's render and whether `target`
/// sat on the LEFT. Content-blind: keyed only on structural equality of renders.
fn split_shared(l_r: &str, r_r: &str, target: &str) -> Option<(String, bool)> {
    if l_r == target {
        Some((r_r.to_string(), true))
    } else if r_r == target {
        Some((l_r.to_string(), false))
    } else {
        None
    }
}

// ---------------------------------------------------------------------------
// cancelCommonFactor: floor (a * c) (d * c) = floor a d   (0 < d, 0 < c)
// ---------------------------------------------------------------------------

struct CancelShape {
    floor_lean: String,
    a: String,
    d: String,
    c: String,
    /// The dividend / divisor products as WRITTEN (either operand order).
    dividend: String,
    divisor: String,
    /// `true` when the shared factor `c` sits on the LEFT of that product
    /// (`c * a` / `c * d`); the emitted proof commutes it to the right before
    /// citing the core cancel lemma.
    c_left_in_dividend: bool,
    c_left_in_divisor: bool,
}

/// Recognize `floor (a * c) (d * c) = floor a d`, capturing the floor fn and the
/// atoms `a` / `d` / the shared factor `c` structurally. ORIENTATION-TOLERANT:
/// the shared factor may sit on either side of each product independently; `a`
/// and `d` are pinned by the reduced rhs. A genuine non-match (no shared factor)
/// declines.
fn recognize_cancel(law: &VerifyLaw, ctx: &CodegenContext) -> Option<CancelShape> {
    // rhs = floor(a, d)
    let Expr::FnCall(callee, args) = &law.rhs.node else {
        return None;
    };
    let floor_src = expr_dotted_name(callee)?;
    if args.len() != 2 {
        return None;
    }
    let a_render = render(&args[0], ctx);
    let d_render = render(&args[1], ctx);

    // lhs = floor(<product>, <product>), same floor fn
    let (prod_a, prod_d) = floor_call(&law.lhs, &floor_src)?;
    let Expr::BinOp(BinOp::Mul, a_l, c_a) = &prod_a.node else {
        return None;
    };
    let Expr::BinOp(BinOp::Mul, d_l, c_d) = &prod_d.node else {
        return None;
    };

    // In the dividend, one side is `a` and the other is the shared factor `c`;
    // in the divisor, one side is `d` and the other must be the SAME `c`.
    let (c_from_a, a_left) = split_shared(&render(a_l, ctx), &render(c_a, ctx), &a_render)?;
    let (c_from_d, d_left) = split_shared(&render(d_l, ctx), &render(c_d, ctx), &d_render)?;
    if c_from_a != c_from_d {
        return None;
    }
    let c_render = c_from_a;

    if !is_euclidean_floor_fn(&floor_src, ctx) {
        return None;
    }

    // `when` must guarantee both the divisor and the shared factor positive.
    let when = law.when.as_ref()?;
    let mut clauses = Vec::new();
    flatten_and(when, &mut clauses);
    let pos_d = clauses
        .iter()
        .any(|cl| clause_gives_pos(cl, &d_render, ctx));
    let pos_c = clauses
        .iter()
        .any(|cl| clause_gives_pos(cl, &c_render, ctx));
    if !pos_d || !pos_c {
        return None;
    }

    Some(CancelShape {
        floor_lean: aver_name_to_lean(&floor_src),
        a: a_render,
        d: d_render,
        c: c_render,
        dividend: render(prod_a, ctx),
        divisor: render(prod_d, ctx),
        // shared factor is on the left iff `a` (resp. `d`) is on the right.
        c_left_in_dividend: !a_left,
        c_left_in_divisor: !d_left,
    })
}

pub(in crate::codegen::lean) fn recognize_cancel_common_factor(
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> bool {
    recognize_cancel(law, ctx).is_some()
}

pub(super) fn emit_cancel_common_factor_law(
    _vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    theorem_base: &str,
    quant_params: &str,
) -> Option<AutoProof> {
    let CancelShape {
        floor_lean: floor,
        a,
        d,
        c,
        dividend,
        divisor,
        c_left_in_dividend,
        c_left_in_divisor,
    } = recognize_cancel(law, ctx)?;
    let when = render(law.when.as_ref()?, ctx);
    let lhs = render(&law.lhs, ctx);
    let rhs = render(&law.rhs, ctx);

    // `0 < divisor` proof follows the WRITTEN factor order of the divisor.
    let hdc = if c_left_in_divisor {
        "Int.mul_pos hc hd" // 0 < c * d
    } else {
        "Int.mul_pos hd hc" // 0 < d * c
    };
    // Commute the shared factor to the RIGHT (core lemma's canonical form).
    let mut normalize = String::new();
    if c_left_in_dividend {
        normalize.push_str(&format!("\n  rw [Int.mul_comm {c} {a}]"));
    }
    if c_left_in_divisor {
        normalize.push_str(&format!("\n  rw [Int.mul_comm {c} {d}]"));
    }

    let text = format!(
        r#"{peel}
theorem {base} : ∀ {quant}, {when} = true -> {lhs} = {rhs} := by
  intro {intro} h_when
  simp only [Bool.and_eq_true, decide_eq_true_eq, ge_iff_le, gt_iff_lt] at h_when
  have hd : 0 < {d} := by omega
  have hc : 0 < {c} := by omega
  have hdc : 0 < {divisor} := {hdc}
  rw [{base}__floordiv_eq ({dividend}) ({divisor}) hdc, {base}__floordiv_eq {a} {d} hd]{normalize}
  exact Int.mul_ediv_mul_of_pos_left {a} {d} hc"#,
        peel = floordiv_eq_lemma(theorem_base, &floor),
        base = theorem_base,
        quant = quant_params,
        intro = intro_names(law),
    );

    Some(AutoProof {
        support_lines: text.lines().map(str::to_string).collect(),
        body: crate::codegen::lean::tactic_ir::Tactic::raw(Vec::new()),
        replaces_theorem: true,
    })
}

// ---------------------------------------------------------------------------
// absorbRemainder: floor (d * q + r) d = q   (0 < d, 0 <= r, r < d)
// ---------------------------------------------------------------------------

struct AbsorbShape {
    floor_lean: String,
    d: String,
    q: String,
    r: String,
    /// The dividend sum as WRITTEN.
    dividend: String,
    /// `true` when the divisor sits on the LEFT of the product (`d * q`); the
    /// emitted proof commutes `q * d` to `d * q` first.
    d_left_in_product: bool,
    /// `true` when the remainder is written FIRST (`r + d * q`); otherwise the
    /// product leads (`d * q + r`) and the proof reorders it with `omega`.
    r_first: bool,
}

/// Split a sum into (product, remainder) whichever way it is written: the
/// operand that is a `Mul` matching the divisor / quotient is the product, the
/// other is the remainder.
fn split_sum<'a>(
    add_l: &'a Spanned<Expr>,
    add_r: &'a Spanned<Expr>,
    d_render: &str,
    rhs_render: &str,
    ctx: &CodegenContext,
) -> Option<(bool, &'a Spanned<Expr>, bool)> {
    // Returns (d_left_in_product, remainder, r_first).
    for (prod, rem, r_first) in [(add_l, add_r, false), (add_r, add_l, true)] {
        let Expr::BinOp(BinOp::Mul, x, y) = &prod.node else {
            continue;
        };
        let (xr, yr) = (render(x, ctx), render(y, ctx));
        // one factor is the divisor `d`, the other is the quotient `q` = rhs.
        if xr == d_render && yr == rhs_render {
            return Some((true, rem, r_first));
        }
        if yr == d_render && xr == rhs_render {
            return Some((false, rem, r_first));
        }
    }
    None
}

/// Recognize `floor (d * q + r) d = q`, capturing the floor fn, the divisor `d`,
/// the quotient `q` and the remainder `r` structurally. ORIENTATION-TOLERANT:
/// the divisor may sit on either side of the product (`d * q` or `q * d`) and the
/// remainder on either side of the sum. A genuine non-match declines.
fn recognize_absorb(law: &VerifyLaw, ctx: &CodegenContext) -> Option<AbsorbShape> {
    // lhs = floor(<sum>, d)
    let floor_src = {
        let Expr::FnCall(callee, _) = &law.lhs.node else {
            return None;
        };
        expr_dotted_name(callee)?
    };
    let (dividend, d_l) = floor_call(&law.lhs, &floor_src)?;
    let Expr::BinOp(BinOp::Add, add_l, add_r) = &dividend.node else {
        return None;
    };
    let d_render = render(d_l, ctx);
    let q_render = render(&law.rhs, ctx);

    let (d_left_in_product, rem, r_first) = split_sum(add_l, add_r, &d_render, &q_render, ctx)?;
    let r_render = render(rem, ctx);

    if !is_euclidean_floor_fn(&floor_src, ctx) {
        return None;
    }

    // `when` must guarantee 0 < d, 0 <= r, and r < d.
    let when = law.when.as_ref()?;
    let mut clauses = Vec::new();
    flatten_and(when, &mut clauses);
    let pos_d = clauses
        .iter()
        .any(|cl| clause_gives_pos(cl, &d_render, ctx));
    let nonneg_r = clauses
        .iter()
        .any(|cl| clause_gives_nonneg(cl, &r_render, ctx));
    let r_lt_d = clauses
        .iter()
        .any(|cl| clause_is_lt(cl, &r_render, &d_render, ctx));
    if !pos_d || !nonneg_r || !r_lt_d {
        return None;
    }

    Some(AbsorbShape {
        floor_lean: aver_name_to_lean(&floor_src),
        d: d_render,
        q: q_render,
        r: r_render,
        dividend: render(dividend, ctx),
        d_left_in_product,
        r_first,
    })
}

pub(in crate::codegen::lean) fn recognize_absorb_remainder(
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> bool {
    recognize_absorb(law, ctx).is_some()
}

pub(super) fn emit_absorb_remainder_law(
    _vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    theorem_base: &str,
    quant_params: &str,
) -> Option<AutoProof> {
    let AbsorbShape {
        floor_lean: floor,
        d,
        q,
        r,
        dividend,
        d_left_in_product,
        r_first,
    } = recognize_absorb(law, ctx)?;
    let when = render(law.when.as_ref()?, ctx);
    let lhs = render(&law.lhs, ctx);
    let rhs = render(&law.rhs, ctx);

    // Normalize to the core lemma's canonical dividend `r + d * q`.
    let mut normalize = String::new();
    if !d_left_in_product {
        // commute `q * d` -> `d * q`
        normalize.push_str(&format!("\n  rw [Int.mul_comm {q} {d}]"));
    }
    if !r_first {
        // reorder `d * q + r` -> `r + d * q` (omega abstracts the product atom)
        normalize.push_str(&format!(
            "\n  rw [show {d} * {q} + {r} = {r} + {d} * {q} from by omega]"
        ));
    }

    let text = format!(
        r#"{peel}
theorem {base} : ∀ {quant}, {when} = true -> {lhs} = {rhs} := by
  intro {intro} h_when
  simp only [Bool.and_eq_true, decide_eq_true_eq, ge_iff_le, gt_iff_lt] at h_when
  have hd : 0 < {d} := by omega
  have h0 : 0 <= {r} := by omega
  have hr : {r} < {d} := by omega
  rw [{base}__floordiv_eq ({dividend}) {d} hd]{normalize}
  rw [Int.add_mul_ediv_left {r} {q} (by omega : {d} ≠ 0)]
  rw [Int.ediv_eq_zero_of_lt h0 hr]
  omega"#,
        peel = floordiv_eq_lemma(theorem_base, &floor),
        base = theorem_base,
        quant = quant_params,
        intro = intro_names(law),
    );

    Some(AutoProof {
        support_lines: text.lines().map(str::to_string).collect(),
        body: crate::codegen::lean::tactic_ir::Tactic::raw(Vec::new()),
        replaces_theorem: true,
    })
}

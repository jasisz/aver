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
//! Both peel each `floor a d` to the bare Euclidean quotient `a / d` (the
//! positive-divisor zero-guard peel), then close by a CORE lemma:
//! `Int.mul_ediv_mul_of_pos_left` cancels the shared positive factor, and
//! `Int.add_mul_ediv_left` + `Int.ediv_eq_zero_of_lt` absorb the bounded
//! remainder. The positivity / bound facts are read off the law's `when` guard.

use super::AutoProof;
use super::aver_name_to_lean;
use super::shared::{
    clause_gives_nonneg, clause_gives_pos, clause_is_lt, expr_dotted_name, flatten_and, floor_call,
    is_euclidean_floor_fn, render, same_atom,
};
use crate::ast::{BinOp, Expr, VerifyBlock, VerifyLaw};
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

// ---------------------------------------------------------------------------
// cancelCommonFactor: floor (a * c) (d * c) = floor a d   (0 < d, 0 < c)
// ---------------------------------------------------------------------------

struct CancelShape {
    floor_lean: String,
    a: String,
    d: String,
    c: String,
}

/// Recognize `floor (a * c) (d * c) = floor a d`, capturing the floor fn and the
/// atoms `a` / `d` / the shared RIGHT factor `c` structurally. The factor is
/// keyed on the right of both products (matching the core cancel lemma); a
/// different orientation declines.
fn recognize_cancel(law: &VerifyLaw, ctx: &CodegenContext) -> Option<CancelShape> {
    // rhs = floor(a, d)
    let Expr::FnCall(callee, args) = &law.rhs.node else {
        return None;
    };
    let floor_src = expr_dotted_name(callee)?;
    if args.len() != 2 {
        return None;
    }
    let (a_r, d_r) = (&args[0], &args[1]);

    // lhs = floor(a * c, d * c), same floor fn
    let (prod_a, prod_d) = floor_call(&law.lhs, &floor_src)?;
    let Expr::BinOp(BinOp::Mul, a_l, c_a) = &prod_a.node else {
        return None;
    };
    let Expr::BinOp(BinOp::Mul, d_l, c_d) = &prod_d.node else {
        return None;
    };

    // Shared factor on the RIGHT of both products; the left factors are the
    // dividend / divisor of the reduced rhs.
    if !same_atom(c_a, c_d, ctx) || !same_atom(a_l, a_r, ctx) || !same_atom(d_l, d_r, ctx) {
        return None;
    }

    if !is_euclidean_floor_fn(&floor_src, ctx) {
        return None;
    }

    // `when` must guarantee both the divisor and the shared factor positive.
    let when = law.when.as_ref()?;
    let mut clauses = Vec::new();
    flatten_and(when, &mut clauses);
    let d_render = render(d_r, ctx);
    let c_render = render(c_a, ctx);
    let pos_d = clauses.iter().any(|cl| clause_gives_pos(cl, &d_render, ctx));
    let pos_c = clauses.iter().any(|cl| clause_gives_pos(cl, &c_render, ctx));
    if !pos_d || !pos_c {
        return None;
    }

    Some(CancelShape {
        floor_lean: aver_name_to_lean(&floor_src),
        a: render(a_r, ctx),
        d: d_render,
        c: c_render,
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
    } = recognize_cancel(law, ctx)?;
    let when = render(law.when.as_ref()?, ctx);
    let lhs = render(&law.lhs, ctx);
    let rhs = render(&law.rhs, ctx);

    let text = format!(
        r#"{peel}
theorem {base} : ∀ {quant}, {when} = true -> {lhs} = {rhs} := by
  intro {intro} h_when
  simp only [Bool.and_eq_true, decide_eq_true_eq, ge_iff_le, gt_iff_lt] at h_when
  have hd : 0 < {d} := by omega
  have hc : 0 < {c} := by omega
  have hdc : 0 < {d} * {c} := Int.mul_pos hd hc
  rw [{base}__floordiv_eq ({a} * {c}) ({d} * {c}) hdc, {base}__floordiv_eq {a} {d} hd]
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
}

/// Recognize `floor (d * q + r) d = q`, capturing the floor fn, the divisor `d`,
/// the quotient `q` and the remainder `r` structurally. Keyed on the divisor
/// being the LEFT factor of the `d * q` product (matching the core absorb
/// lemma); a different orientation declines.
fn recognize_absorb(law: &VerifyLaw, ctx: &CodegenContext) -> Option<AbsorbShape> {
    // lhs = floor(d * q + r, d)
    let floor_src = {
        let Expr::FnCall(callee, _) = &law.lhs.node else {
            return None;
        };
        expr_dotted_name(callee)?
    };
    let (dividend, d_l) = floor_call(&law.lhs, &floor_src)?;
    let Expr::BinOp(BinOp::Add, prod, r_e) = &dividend.node else {
        return None;
    };
    let Expr::BinOp(BinOp::Mul, d_m, q_e) = &prod.node else {
        return None;
    };

    // The product's left factor is the divisor; the rhs is the quotient.
    if !same_atom(d_m, d_l, ctx) || !same_atom(q_e, &law.rhs, ctx) {
        return None;
    }

    if !is_euclidean_floor_fn(&floor_src, ctx) {
        return None;
    }

    // `when` must guarantee 0 < d, 0 <= r, and r < d.
    let when = law.when.as_ref()?;
    let mut clauses = Vec::new();
    flatten_and(when, &mut clauses);
    let d_render = render(d_l, ctx);
    let r_render = render(r_e, ctx);
    let pos_d = clauses.iter().any(|cl| clause_gives_pos(cl, &d_render, ctx));
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
        q: render(q_e, ctx),
        r: r_render,
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
    } = recognize_absorb(law, ctx)?;
    let when = render(law.when.as_ref()?, ctx);
    let lhs = render(&law.lhs, ctx);
    let rhs = render(&law.rhs, ctx);

    let text = format!(
        r#"{peel}
theorem {base} : ∀ {quant}, {when} = true -> {lhs} = {rhs} := by
  intro {intro} h_when
  simp only [Bool.and_eq_true, decide_eq_true_eq, ge_iff_le, gt_iff_lt] at h_when
  have hd : 0 < {d} := by omega
  have h0 : 0 <= {r} := by omega
  have hr : {r} < {d} := by omega
  rw [{base}__floordiv_eq ({d} * {q} + {r}) {d} hd]
  rw [show {d} * {q} + {r} = {r} + {d} * {q} from by omega]
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

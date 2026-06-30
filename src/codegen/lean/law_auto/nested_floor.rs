//! CONTENT-BLIND nested-Euclidean-floor rung — ONE recognizer + ONE skeleton
//! that closes ANY law of the nested-floor COLLAPSE shape
//!
//! ```text
//!     floor (floor a d) e = floor a (d * e)      (0 < d, 0 < e)
//! ```
//!
//! where `floor` is a Euclidean floor-division fn (body `withDefault (Int.div a
//! d) 0`, recognized by SHAPE not by name). The three atoms `a` / `d` / `e` are
//! captured STRUCTURALLY from the law's AST — there is no `pow2` / `trunc` /
//! `sticky` / K5 literal anywhere in the recognizer or the emitted proof. The
//! same rung therefore closes the K5 `nestedFloorCollapse` over a power-of-two
//! divisor AND a plain-integer nested floor in any other module — the
//! cross-domain witness that it is shape-only.
//!
//! The skeleton is core Lean 4.31, NO Mathlib: reduce each `floor a d` to the
//! bare Euclidean quotient `a / d` (the `withDefault` / zero-guard peel, valid
//! because the divisor is positive), then close `a / d / e = a / (d * e)` by the
//! CORE lemma `Int.ediv_ediv_of_nonneg` (`0 ≤ d`). The divisor positivity is read
//! off the law's `when` guard. Deterministic (no `sorry` floor): the recognizer's
//! gates guarantee every rewrite closes, so a structural surprise is a loud build
//! error, never silent credit.

use super::AutoProof;
use super::aver_name_to_lean;
use super::shared::{expr_dotted_name, find_fn_def_by_call_name};
use crate::ast::{BinOp, Expr, Literal, Spanned, Stmt, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;

struct NestedFloorShape {
    /// Lean name of the Euclidean floor fn (e.g. `floorDiv`).
    floor_lean: String,
    /// Rendered dividend / inner divisor / outer divisor atoms.
    a: String,
    d: String,
    e: String,
}

fn render(e: &Spanned<Expr>, ctx: &CodegenContext) -> String {
    super::super::expr::emit_expr_legacy(e, ctx, None)
}

fn same_atom(a: &Spanned<Expr>, b: &Spanned<Expr>, ctx: &CodegenContext) -> bool {
    render(a, ctx) == render(b, ctx)
}

/// `floor(x, y)` — a 2-arg call to `floor_src`; returns `(x, y)`.
fn floor_call<'a>(
    e: &'a Spanned<Expr>,
    floor_src: &str,
) -> Option<(&'a Spanned<Expr>, &'a Spanned<Expr>)> {
    let Expr::FnCall(callee, args) = &e.node else {
        return None;
    };
    if expr_dotted_name(callee).as_deref() != Some(floor_src) || args.len() != 2 {
        return None;
    }
    Some((&args[0], &args[1]))
}

/// Whether `fd_src` names a Euclidean floor-division fn: its body is
/// `Result.withDefault(Int.div(a, d), 0)` over the two parameters. This is the
/// definition-shape gate that makes the nested-floor identity TRUE — keyed on
/// the body, never on the fn's name.
fn is_euclidean_floor_fn(floor_src: &str, ctx: &CodegenContext) -> bool {
    let Some(fd) = find_fn_def_by_call_name(ctx, floor_src) else {
        return false;
    };
    if !fd.effects.is_empty() || fd.params.len() != 2 {
        return false;
    }
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return false;
    };
    // withDefault(Int.div(_, _), 0)
    let Expr::FnCall(callee, args) = &body.node else {
        return false;
    };
    if expr_dotted_name(callee).as_deref() != Some("Result.withDefault") || args.len() != 2 {
        return false;
    }
    if !matches!(&args[1].node, Expr::Literal(Literal::Int(0))) {
        return false;
    }
    let Expr::FnCall(div_callee, div_args) = &args[0].node else {
        return false;
    };
    expr_dotted_name(div_callee).as_deref() == Some("Int.div") && div_args.len() == 2
}

/// Flatten a `Bool.and` / `&&` conjunction of `when` clauses.
fn flatten_and<'a>(e: &'a Spanned<Expr>, out: &mut Vec<&'a Spanned<Expr>>) {
    match &e.node {
        Expr::FnCall(callee, args)
            if expr_dotted_name(callee).as_deref() == Some("Bool.and") && args.len() == 2 =>
        {
            flatten_and(&args[0], out);
            flatten_and(&args[1], out);
        }
        _ => out.push(e),
    }
}

/// Whether `clause` guarantees `0 < x` for the atom rendered as `x_render`:
/// `0 < x`, `x > 0`, `1 <= x`, `x >= 1` (any nonneg/≥1 literal bound).
fn clause_gives_pos(clause: &Spanned<Expr>, x_render: &str, ctx: &CodegenContext) -> bool {
    let Expr::BinOp(op, l, r) = &clause.node else {
        return false;
    };
    let int_lit = |e: &Spanned<Expr>| match &e.node {
        Expr::Literal(Literal::Int(n)) => Some(*n),
        _ => None,
    };
    match op {
        // c < x  (c >= 0)  ⟹  0 < x ;  x > c  (c >= 0)
        BinOp::Lt => int_lit(l).is_some_and(|c| c >= 0) && render(r, ctx) == x_render,
        BinOp::Gt => render(l, ctx) == x_render && int_lit(r).is_some_and(|c| c >= 0),
        // c <= x  (c >= 1)  ;  x >= c  (c >= 1)
        BinOp::Lte => int_lit(l).is_some_and(|c| c >= 1) && render(r, ctx) == x_render,
        BinOp::Gte => render(l, ctx) == x_render && int_lit(r).is_some_and(|c| c >= 1),
        _ => false,
    }
}

/// Recognize the nested-floor collapse shape, capturing `floor` / `a` / `d` /
/// `e` from the AST. Declines unless every structural gate holds.
fn recognize(law: &VerifyLaw, ctx: &CodegenContext) -> Option<NestedFloorShape> {
    // rhs = floor(a, d * e)
    let (ra, r_prod) = {
        let Expr::FnCall(callee, args) = &law.rhs.node else {
            return None;
        };
        let floor_src = expr_dotted_name(callee)?;
        if args.len() != 2 {
            return None;
        }
        (floor_src, args)
    };
    let floor_src = ra;
    let a_r = &r_prod[0];
    let Expr::BinOp(BinOp::Mul, d_r, e_r) = &r_prod[1].node else {
        return None;
    };

    // lhs = floor(floor(a, d), e), same floor fn
    let (inner, e_l) = floor_call(&law.lhs, &floor_src)?;
    let (a_l, d_l) = floor_call(inner, &floor_src)?;

    // The three atoms must coincide across the two sides.
    if !same_atom(a_l, a_r, ctx) || !same_atom(d_l, d_r, ctx) || !same_atom(e_l, e_r, ctx) {
        return None;
    }

    // `floor` must really be Euclidean floor division (its identity rests on it).
    if !is_euclidean_floor_fn(&floor_src, ctx) {
        return None;
    }

    // The `when` must guarantee both divisors positive (exactly two clauses).
    let when = law.when.as_ref()?;
    let mut clauses = Vec::new();
    flatten_and(when, &mut clauses);
    if clauses.len() != 2 {
        return None;
    }
    let d_render = render(d_r, ctx);
    let e_render = render(e_r, ctx);
    let pos_d = clauses.iter().any(|c| clause_gives_pos(c, &d_render, ctx));
    let pos_e = clauses.iter().any(|c| clause_gives_pos(c, &e_render, ctx));
    if !pos_d || !pos_e {
        return None;
    }

    Some(NestedFloorShape {
        floor_lean: aver_name_to_lean(&floor_src),
        a: render(a_r, ctx),
        d: d_render,
        e: e_render,
    })
}

/// Statement-builder hook: whether the nested-floor emit will close this law
/// universally (so the caller drops the sampled domain and classes it
/// `universal`, keeping statement and proof in lockstep).
pub(in crate::codegen::lean) fn recognize_nested_floor(
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> bool {
    recognize(law, ctx).is_some()
}

/// Close a nested-floor collapse law with the core `Int.ediv_ediv_of_nonneg`
/// skeleton. Emits its OWN TRUE-universal theorem (`replaces_theorem`).
pub(super) fn emit_nested_floor_law(
    _vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    theorem_base: &str,
    quant_params: &str,
) -> Option<AutoProof> {
    let shape = recognize(law, ctx)?;
    let when = render(law.when.as_ref()?, ctx);
    let lhs = render(&law.lhs, ctx);
    let rhs = render(&law.rhs, ctx);
    let intro: Vec<String> = law
        .givens
        .iter()
        .map(|g| aver_name_to_lean(&g.name))
        .collect();
    let NestedFloorShape {
        floor_lean: floor,
        a,
        d,
        e,
    } = &shape;

    let text = format!(
        r#"theorem {base}__floordiv_eq (a d : Int) (hd : 0 < d) : {floor} a d = a / d := by
  have hne : ¬((d == 0) = true) := by simp only [beq_iff_eq]; omega
  simp only [{floor}]
  rw [if_neg hne]
  simp only [Except.withDefault]
theorem {base} : ∀ {quant_params}, {when} = true -> {lhs} = {rhs} := by
  intro {intro} h_when
  simp only [Bool.and_eq_true, decide_eq_true_eq, ge_iff_le, gt_iff_lt] at h_when
  obtain ⟨hc0, hc1⟩ := h_when
  have hd : 0 < {d} := by omega
  have he : 0 < {e} := by omega
  have hde : 0 < {d} * {e} := Int.mul_pos hd he
  rw [{base}__floordiv_eq {a} {d} hd, {base}__floordiv_eq ({a} / {d}) {e} he,
      {base}__floordiv_eq {a} ({d} * {e}) hde]
  exact Int.ediv_ediv_of_nonneg (Int.le_of_lt hd)"#,
        base = theorem_base,
        intro = intro.join(" "),
    );

    Some(AutoProof {
        support_lines: text.lines().map(|l| l.to_string()).collect(),
        body: crate::codegen::lean::tactic_ir::Tactic::raw(Vec::new()),
        replaces_theorem: true,
    })
}

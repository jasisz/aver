//! Claim-shape arms that close with core Lean alone.
//!
//! Each arm is keyed on the shape of the law's statement and of its
//! non-recursive call cone, never on a particular function, and each is
//! cheap: a fixed rewrite chain, `split`, or one `grind` / `omega` call over
//! facts built from the claim itself. An arm either closes its goal or fails,
//! so the portfolio it joins falls through to the next alternative.
//!
//! * [`mask_arm`]: `Bits.and(x, L)` with a literal mask `L`. Each mask gets a
//!   closed form `∀ y, AverBits.and y L = Σ 2^a * (y / 2^a % 2^n)` (one term
//!   per run of ones in `L`), proved by a sign split, the `AverBits` bit
//!   lemmas and `omega`; the claim is then rewritten with it and closed by
//!   `omega`.
//! * [`INT_LITERAL_MATCH_ARM`]: a cone function that matches an `Int` on
//!   literals, compared with range predicates: `split <;> simp_all <;> omega`
//!   (gated by [`cone_matches_int_literals`]).
//! * [`sign_fact_haves`]: products of two non-constant terms in the claim, its
//!   premise or the cone body the law calls: the core sign lemmas for each
//!   product (and the nonnegativity of each square) as hypotheses, so `grind`
//!   and `omega` can read the product as an atom with a known sign.
//! * [`cone_grind_arms`]: a guarded claim over a cone that branches: `grind`
//!   handed the unfolded cone.

use std::collections::{BTreeSet, HashMap};

use super::super::expr::aver_name_to_lean;
use super::shared;
use crate::ast::{BinOp, Expr, FnDef, Literal, Pattern, Spanned, Stmt, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;

/// The claim's own expressions: both sides and the `when` premise.
fn law_exprs(law: &VerifyLaw) -> Vec<&Spanned<Expr>> {
    let mut out = vec![&law.lhs, &law.rhs];
    if let Some(when) = &law.when {
        out.push(when);
    }
    out
}

/// The pure, non-recursive user functions of the law's call cone.
fn cone_defs<'a>(ctx: &'a CodegenContext, vb: &VerifyBlock, law: &VerifyLaw) -> Vec<&'a FnDef> {
    let recursive = super::recursive_pure_fn_names(ctx);
    shared::law_simp_source_names(ctx, vb, law)
        .iter()
        .filter_map(|name| shared::find_fn_def_by_call_name(ctx, name))
        .filter(|fd| fd.effects.is_empty() && !recursive.contains(&fd.name))
        .collect()
}

fn body_exprs(fd: &FnDef) -> Vec<&Spanned<Expr>> {
    fd.body
        .stmts()
        .iter()
        .map(|stmt| match stmt {
            Stmt::Binding(_, _, e) | Stmt::Expr(e) => e,
        })
        .collect()
}

fn nonneg_literal(expr: &Spanned<Expr>) -> Option<u128> {
    match &expr.node {
        Expr::Literal(Literal::Int(n)) if *n >= 0 => Some(*n as u128),
        Expr::Literal(Literal::BigInt(digits)) => digits.parse().ok(),
        _ => None,
    }
}

/// Literal masks `L >= 1` of every `Bits.and(e, L)` / `Bits.and(L, e)` in the
/// claim and its non-recursive cone.
fn literal_masks(vb: &VerifyBlock, law: &VerifyLaw, ctx: &CodegenContext) -> BTreeSet<u128> {
    let mut masks = BTreeSet::new();
    let mut visit = |expr: &Spanned<Expr>| {
        crate::codegen::expr_walk::walk(expr, &mut |node| {
            if let Expr::FnCall(callee, args) = &node.node
                && shared::expr_dotted_name(callee).as_deref() == Some("Bits.and")
                && let [a, b] = args.as_slice()
            {
                match (nonneg_literal(a), nonneg_literal(b)) {
                    (None, Some(mask)) | (Some(mask), None) if mask > 0 => {
                        masks.insert(mask);
                    }
                    _ => {}
                }
            }
        });
    };
    for expr in law_exprs(law) {
        visit(expr);
    }
    for fd in cone_defs(ctx, vb, law) {
        for expr in body_exprs(fd) {
            visit(expr);
        }
    }
    masks
}

fn pow2_exponent(n: u128) -> Option<u32> {
    (n != 0 && n.is_power_of_two()).then(|| n.trailing_zeros())
}

/// The `rw` steps that take `x &&& mask` apart into single bits and low runs:
/// a low run `2^k - 1` is a remainder, a single bit `2^k` a quotient bit, and
/// anything else splits at the end of its lowest run (or its trailing zeros)
/// and recurses on both halves.
fn mask_rewrites(mask: u128, out: &mut Vec<String>) {
    if mask == 0 {
        out.push("Nat.and_zero".to_string());
        return;
    }
    if let Some(k) = mask.checked_add(1).and_then(pow2_exponent) {
        out.push(format!(
            "AverBits.nat_land_low _ {mask} {} {k} (by decide) (by decide)",
            mask + 1
        ));
        return;
    }
    if let Some(k) = pow2_exponent(mask) {
        out.push(format!("AverBits.nat_land_bit _ {mask} {k} (by decide)"));
        return;
    }
    let j = if mask & 1 == 1 {
        mask.trailing_ones()
    } else {
        mask.trailing_zeros()
    };
    let p = 1u128 << j;
    let hi = mask >> j;
    let lo = mask & (p - 1);
    out.push(format!(
        "AverBits.nat_land_split _ {mask} {p} {j} {hi} {lo} (by decide) (by decide) (by decide)"
    ));
    mask_rewrites(hi, out);
    mask_rewrites(lo, out);
}

/// `Σ 2^a * (y / 2^a % 2^n)` over the runs of ones of `mask`, lowest first.
fn mask_closed_form(mask: u128) -> String {
    let mut terms = Vec::new();
    let mut bit = 0u32;
    while bit < 128 && (mask >> bit) != 0 {
        if (mask >> bit) & 1 == 0 {
            bit += 1;
            continue;
        }
        let run = (mask >> bit).trailing_ones();
        let modulus = 1u128 << run;
        let shift = 1u128 << bit;
        terms.push(if bit == 0 {
            format!("y % {modulus}")
        } else {
            format!("{shift} * (y / {shift} % {modulus})")
        });
        bit += run;
    }
    terms.join(" + ")
}

/// The literal-mask arm (see the module doc), or `None` when the law has no
/// literal mask or carries a `when` premise.
pub(super) fn mask_arm(vb: &VerifyBlock, law: &VerifyLaw, ctx: &CodegenContext) -> Option<String> {
    if law.when.is_some() {
        return None;
    }
    let masks = literal_masks(vb, law, ctx);
    if masks.is_empty() || masks.len() > 4 || masks.iter().any(|m| *m >= 1u128 << 64) {
        return None;
    }
    let mut lines = Vec::new();
    let mut keys = Vec::new();
    for (index, mask) in masks.iter().enumerate() {
        let key = format!("_aver_mask_{index}");
        let mut rewrites = Vec::new();
        mask_rewrites(*mask, &mut rewrites);
        let chain = rewrites.join(", ");
        lines.push(format!(
            "have {key} : ∀ y : Int, AverBits.and y {mask} = {} := by",
            mask_closed_form(*mask)
        ));
        lines.push("  intro y".to_string());
        lines.push("  rcases Int.lt_or_le y 0 with hn | hp".to_string());
        lines.push(format!(
            "  · rw [AverBits.and_of_neg y {mask} {mask} (by decide) hn, {chain}]"
        ));
        lines.push("    omega".to_string());
        lines.push(format!(
            "  · rw [AverBits.and_of_nonneg y {mask} {mask} (by decide) hp, {chain}]"
        ));
        lines.push("    omega".to_string());
        keys.push(key);
    }
    let mut simp_set: Vec<String> = shared::law_simp_defs_blind(ctx, vb, law)
        .into_iter()
        .collect();
    simp_set.extend(keys);
    simp_set.push("Int.emod_emod".to_string());
    lines.push(format!(
        "simp only [{}] <;> first | rfl | omega | (rw [Bool.eq_iff_iff]; simp only [beq_iff_eq, bne_iff_ne]; omega)",
        simp_set.join(", ")
    ));
    Some(lines.join("\n"))
}

/// Whether a function of `unfold_fns` matches on integer literals (two or more
/// literal arms): the claim then compares a literal table with a predicate.
pub(super) fn cone_matches_int_literals(unfold_fns: &[String], ctx: &CodegenContext) -> bool {
    unfold_fns.iter().any(|name| {
        shared::find_fn_def_by_call_name(ctx, name).is_some_and(|fd| {
            body_exprs(fd).into_iter().any(|expr| {
                crate::codegen::expr_walk::any(expr, &mut |node| {
                    matches!(&node.node, Expr::Match { arms, .. }
                        if arms
                            .iter()
                            .filter(|arm| matches!(arm.pattern, Pattern::Literal(Literal::Int(_))))
                            .count()
                            >= 2)
                })
            })
        })
    })
}

/// The arm for an unfolded literal match against range predicates.
pub(super) const INT_LITERAL_MATCH_ARM: &str = "split <;> simp_all <;> omega";

/// Lean text of an `Int` term built from the law's givens, integer literals,
/// `+`, `-`, `*` and negation; `None` for anything else.
fn arithmetic_text(expr: &Spanned<Expr>, givens: &BTreeSet<String>) -> Option<String> {
    match &expr.node {
        Expr::Ident(name) | Expr::Resolved { name, .. } if givens.contains(name) => {
            Some(aver_name_to_lean(name))
        }
        Expr::Literal(Literal::Int(n)) if *n >= 0 => Some(n.to_string()),
        Expr::Neg(inner) => Some(format!("(-{})", arithmetic_text(inner, givens)?)),
        Expr::BinOp(op @ (BinOp::Add | BinOp::Sub | BinOp::Mul), l, r) => {
            let symbol = match op {
                BinOp::Add => "+",
                BinOp::Sub => "-",
                _ => "*",
            };
            Some(format!(
                "({} {symbol} {})",
                arithmetic_text(l, givens)?,
                arithmetic_text(r, givens)?
            ))
        }
        _ => None,
    }
}

fn is_literal(expr: &Spanned<Expr>) -> bool {
    match &expr.node {
        Expr::Literal(Literal::Int(_)) => true,
        Expr::Neg(inner) => is_literal(inner),
        _ => false,
    }
}

/// Collect the products of two non-constant terms under `expr`, with the
/// cone's parameters replaced by the arguments the law (or the caller in the
/// cone) passes, down to three calls deep.
fn collect_products(
    expr: &Spanned<Expr>,
    bindings: &HashMap<String, Spanned<Expr>>,
    ctx: &CodegenContext,
    givens: &BTreeSet<String>,
    depth: usize,
    out: &mut Vec<(String, String)>,
) {
    let substituted = |e: &Spanned<Expr>| {
        let map: HashMap<&str, &Spanned<Expr>> =
            bindings.iter().map(|(k, v)| (k.as_str(), v)).collect();
        shared::substitute_expr(e, &map)
    };
    let recursive = super::recursive_pure_fn_names(ctx);
    crate::codegen::expr_walk::walk(expr, &mut |node| match &node.node {
        Expr::BinOp(BinOp::Mul, l, r) => {
            let (l, r) = (substituted(l), substituted(r));
            if is_literal(&l) || is_literal(&r) {
                return;
            }
            if let (Some(a), Some(b)) = (arithmetic_text(&l, givens), arithmetic_text(&r, givens))
                && !out.contains(&(a.clone(), b.clone()))
            {
                out.push((a, b));
            }
        }
        Expr::FnCall(callee, args) if depth < 3 => {
            let Some(name) = shared::expr_dotted_name(callee) else {
                return;
            };
            let Some(fd) = shared::find_fn_def_by_call_name(ctx, &name) else {
                return;
            };
            if !fd.effects.is_empty()
                || recursive.contains(&fd.name)
                || fd.params.len() != args.len()
            {
                return;
            }
            let mut inner: HashMap<String, Spanned<Expr>> = fd
                .params
                .iter()
                .zip(args)
                .map(|((param, _), arg)| (param.clone(), substituted(arg)))
                .collect();
            for stmt in fd.body.stmts() {
                match stmt {
                    Stmt::Binding(bound, _, e) => {
                        let map: HashMap<&str, &Spanned<Expr>> =
                            inner.iter().map(|(k, v)| (k.as_str(), v)).collect();
                        let value = shared::substitute_expr(e, &map);
                        collect_products(e, &inner, ctx, givens, depth + 1, out);
                        inner.insert(bound.clone(), value);
                    }
                    Stmt::Expr(e) => collect_products(e, &inner, ctx, givens, depth + 1, out),
                }
            }
        }
        _ => {}
    });
}

/// `have` steps stating the sign of each product of two non-constant terms in
/// the claim, its premise or the cone body it calls, from the core `Int.mul_*`
/// lemmas; a square gets its nonnegativity. Empty when there is no such
/// product or there are too many to hand to `grind` (more than three, or more
/// than two that are not squares).
pub(super) fn sign_fact_haves(law: &VerifyLaw, ctx: &CodegenContext) -> Vec<String> {
    let givens: BTreeSet<String> = law.givens.iter().map(|g| g.name.clone()).collect();
    let mut products = Vec::new();
    for expr in law_exprs(law) {
        collect_products(expr, &HashMap::new(), ctx, &givens, 0, &mut products);
    }
    let squares = products.iter().filter(|(a, b)| a == b).count();
    if products.is_empty() || products.len() > 3 || products.len() - squares > 2 {
        return Vec::new();
    }
    let mut haves = Vec::new();
    for (index, (a, b)) in products.iter().enumerate() {
        if a == b {
            haves.push(format!(
                "have _aver_sq_{index} : 0 ≤ {a} * {a} := (Int.le_total 0 {a}).elim \
                 (fun h => Int.mul_nonneg h h) (fun h => Int.mul_nonneg_of_nonpos_of_nonpos h h)"
            ));
            continue;
        }
        let facts = [
            ("0 ≤ {a} → 0 ≤ {b} → 0 ≤ {p}", "Int.mul_nonneg"),
            (
                "{a} ≤ 0 → {b} ≤ 0 → 0 ≤ {p}",
                "Int.mul_nonneg_of_nonpos_of_nonpos",
            ),
            (
                "0 ≤ {a} → {b} ≤ 0 → {p} ≤ 0",
                "Int.mul_nonpos_of_nonneg_of_nonpos",
            ),
            (
                "{a} ≤ 0 → 0 ≤ {b} → {p} ≤ 0",
                "Int.mul_nonpos_of_nonpos_of_nonneg",
            ),
            ("0 < {a} → 0 < {b} → 0 < {p}", "Int.mul_pos"),
            ("{a} < 0 → {b} < 0 → 0 < {p}", "Int.mul_pos_of_neg_of_neg"),
            ("0 < {a} → {b} < 0 → {p} < 0", "Int.mul_neg_of_pos_of_neg"),
            ("{a} < 0 → 0 < {b} → {p} < 0", "Int.mul_neg_of_neg_of_pos"),
        ];
        let product = format!("{a} * {b}");
        for (fact_index, (statement, lemma)) in facts.iter().enumerate() {
            let statement = statement
                .replace("{a}", a)
                .replace("{b}", b)
                .replace("{p}", &product);
            haves.push(format!(
                "have _aver_sgn_{index}_{fact_index} : {statement} := {lemma}"
            ));
        }
    }
    haves
}

/// Whether the law's left side calls its own subject function somewhere below
/// a comparison, a field access or an equation.
pub(super) fn lhs_calls_subject(vb: &VerifyBlock, law: &VerifyLaw) -> bool {
    crate::codegen::expr_walk::any(&law.lhs, &mut |node| {
        matches!(&node.node, Expr::FnCall(callee, _)
            if shared::expr_dotted_name(callee).as_deref() == Some(vb.fn_name.as_str()))
    })
}

/// Arms for a guarded claim over records and helpers that branch: `grind`
/// handed the unfolded cone, first on its own and then after the premise is
/// split into facts and the cone is simplified. Only for a cone that
/// branches (a `match` somewhere in it); well-founded countdowns stay out of
/// the unfold set, whose unconditional equations would loop.
pub(super) fn cone_grind_arms(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Vec<String> {
    let branches = cone_defs(ctx, vb, law).into_iter().any(|fd| {
        body_exprs(fd).into_iter().any(|expr| {
            crate::codegen::expr_walk::any(expr, &mut |node| {
                matches!(node.node, Expr::Match { .. })
            })
        })
    });
    if !branches || law.givens.len() > 4 {
        return Vec::new();
    }
    let defs: Vec<String> = shared::law_simp_defs_blind(ctx, vb, law)
        .into_iter()
        .collect();
    if defs.is_empty() {
        return Vec::new();
    }
    let list = defs.join(", ");
    vec![
        format!("grind [{list}]"),
        format!(
            "simp only [Bool.and_eq_true, decide_eq_true_eq, ge_iff_le] at h_when; \
             simp [{list}] <;> grind"
        ),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mask_decomposition_covers_runs_bits_and_splits() {
        let mut out = Vec::new();
        mask_rewrites(31, &mut out);
        assert_eq!(
            out,
            vec!["AverBits.nat_land_low _ 31 32 5 (by decide) (by decide)"]
        );
        out.clear();
        mask_rewrites(128, &mut out);
        assert_eq!(out, vec!["AverBits.nat_land_bit _ 128 7 (by decide)"]);
        out.clear();
        mask_rewrites(0x0040_FFFF, &mut out);
        assert_eq!(
            out,
            vec![
                "AverBits.nat_land_split _ 4259839 65536 16 64 65535 (by decide) (by decide) (by decide)",
                "AverBits.nat_land_bit _ 64 6 (by decide)",
                "AverBits.nat_land_low _ 65535 65536 16 (by decide) (by decide)",
            ]
        );
        assert_eq!(mask_closed_form(31), "y % 32");
        assert_eq!(mask_closed_form(128), "128 * (y / 128 % 2)");
        assert_eq!(
            mask_closed_form(0x0040_FFFF),
            "y % 65536 + 4194304 * (y / 4194304 % 2)"
        );
        assert_eq!(
            mask_closed_form(0b1010),
            "2 * (y / 2 % 2) + 8 * (y / 8 % 2)"
        );
    }
}

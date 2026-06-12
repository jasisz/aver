//! `FloorDivWindow` strategy detectors — the floor-division window
//! law family (laws over a power-of-two fn, a floor-halving
//! binary-exponent fn, and the scaled-significand / bit-width window
//! predicates built from them).
//!
//! Split from `proof_lower.rs` — see the module docs in [`super`].
//!
//! Every detector here is **syntax-discovery-only** and deliberately
//! NARROW: exactly the hand-validated figures are recognized (the
//! proof templates the backends render were validated end-to-end on
//! the emitted artifacts — Lean 4.15 core kernel-genuine, Dafny
//! fully verified), and everything else declines at zero cost. The
//! termination side is NOT re-derived here: the binary-exponent fn
//! must already carry the guard-validated
//! [`crate::ir::RecursionContract::WellFoundedToNat`] floor-division
//! contract, so a fn whose guards don't justify the halving never
//! reaches a window figure.

use super::*;
use crate::ir::FloorWindowFigure;

/// Top-level detector. Tries the four figures in claim-complexity
/// order; the first match wins. Returns `None` (decline) for
/// anything outside the validated shapes.
pub(super) fn detect_floor_window(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
    fn_contracts: &std::collections::HashMap<crate::ir::FnId, crate::ir::FnContract>,
) -> Option<FloorWindowFigure> {
    if let Some(figure) = detect_sig_window(law, fn_name, inputs, fn_contracts) {
        return Some(figure);
    }
    if let Some(figure) = detect_product_window(law, fn_name, inputs) {
        return Some(figure);
    }
    if let Some(figure) = detect_pow_sum_split(law, fn_name, inputs) {
        return Some(figure);
    }
    if let Some(figure) = detect_pow_positive(law, fn_name, inputs) {
        return Some(figure);
    }
    None
}

/// Resolve `name` to a pure entry/dep fn def, rejecting effectful
/// fns and `main`.
fn resolve_pure_fn<'a>(name: &str, inputs: &ProofLowerInputs<'a>) -> Option<&'a FnDef> {
    let fd = inputs.find_fn_def_by_call_name(name)?;
    if !fd.effects.is_empty() || fd.name == "main" {
        return None;
    }
    Some(fd)
}

/// The power-of-two countdown shape:
/// `match p <= 0 { true -> 1; false -> 2 * self(p - 1) }` over a
/// single Int param, Int return. The literal base 1 and multiplier 2
/// are load-bearing — the window templates' algebra is specific to
/// powers of two.
fn is_pow2_shape(fd: &FnDef) -> bool {
    use crate::ast::{BinOp, Literal, Stmt};
    let [(p, ty)] = fd.params.as_slice() else {
        return false;
    };
    if ty != "Int" || fd.return_type != "Int" {
        return false;
    }
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return false;
    };
    let Expr::Match { subject, arms } = &body.node else {
        return false;
    };
    let Expr::BinOp(BinOp::Lte, sl, sr) = &subject.node else {
        return false;
    };
    if !matches_ident(sl, p) || !is_int_lit(sr, 0) {
        return false;
    }
    let (Some(base), Some(rec)) = (bool_arm(arms, true), bool_arm(arms, false)) else {
        return false;
    };
    if !matches!(&base.node, Expr::Literal(Literal::Int(1))) {
        return false;
    }
    let Expr::BinOp(BinOp::Mul, ml, mr) = &rec.node else {
        return false;
    };
    if !is_int_lit(ml, 2) {
        return false;
    }
    let Expr::FnCall(callee, args) = &mr.node else {
        return false;
    };
    if expr_to_dotted_name(&callee.node).as_deref() != Some(fd.name.as_str()) || args.len() != 1 {
        return false;
    }
    let Expr::BinOp(BinOp::Sub, dl, dr) = &args[0].node else {
        return false;
    };
    matches_ident(dl, p) && is_int_lit(dr, 1)
}

/// The floor-halving binary-exponent shape:
/// ```text
/// match b < 1 { true -> 0;
///   false -> match a < 2 * b { true -> 0;
///     false -> 1 + self(halve(a), b) } }
/// ```
/// over `(a: Int, b: Int) -> Int`, where `halve` is the unary
/// floor-division wrapper the fn's `WellFoundedToNat` contract
/// (divisor 2) already validated. Returns the halve fn's name.
fn is_binade_exp_shape(
    fd: &FnDef,
    inputs: &ProofLowerInputs,
    fn_contracts: &std::collections::HashMap<crate::ir::FnId, crate::ir::FnContract>,
) -> Option<String> {
    use crate::ast::{BinOp, Literal, Stmt};
    let [(a, ta), (b, tb)] = fd.params.as_slice() else {
        return None;
    };
    if ta != "Int" || tb != "Int" || fd.return_type != "Int" {
        return None;
    }
    // Contract gate: guard-validated floor-division countdown with
    // divisor 2 through a wrapper fn. **syntax-discovery-only**
    // (epic #170 Phase 8 guardrail): verify laws are entry-only by
    // parser grammar, so the cone fns a law names resolve in entry
    // scope; a module-owned same-bare-name fn would simply fail the
    // contract/shape gate below and the figure declines.
    let fn_id = inputs
        .symbol_table
        .fn_id_of(&crate::ir::FnKey::entry(&fd.name))?;
    let contract = fn_contracts.get(&fn_id)?;
    let Some(crate::ir::RecursionContract::WellFoundedToNat {
        floor_div: Some(shrink),
        ..
    }) = &contract.recursion
    else {
        return None;
    };
    if shrink.divisor != 2 {
        return None;
    }
    let halve_name = shrink.helper_fn.clone()?;

    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return None;
    };
    let Expr::Match { subject, arms } = &body.node else {
        return None;
    };
    let Expr::BinOp(BinOp::Lt, sl, sr) = &subject.node else {
        return None;
    };
    if !matches_ident(sl, b) || !is_int_lit(sr, 1) {
        return None;
    }
    let (Some(off), Some(inner)) = (bool_arm(arms, true), bool_arm(arms, false)) else {
        return None;
    };
    if !matches!(&off.node, Expr::Literal(Literal::Int(0))) {
        return None;
    }
    let Expr::Match {
        subject: isub,
        arms: iarms,
    } = &inner.node
    else {
        return None;
    };
    let Expr::BinOp(BinOp::Lt, il, ir) = &isub.node else {
        return None;
    };
    if !matches_ident(il, a) {
        return None;
    }
    let Expr::BinOp(BinOp::Mul, ml, mr) = &ir.node else {
        return None;
    };
    if !is_int_lit(ml, 2) || !matches_ident(mr, b) {
        return None;
    }
    let (Some(base), Some(rec)) = (bool_arm(iarms, true), bool_arm(iarms, false)) else {
        return None;
    };
    if !matches!(&base.node, Expr::Literal(Literal::Int(0))) {
        return None;
    }
    let Expr::BinOp(BinOp::Add, pl, pr) = &rec.node else {
        return None;
    };
    if !is_int_lit(pl, 1) {
        return None;
    }
    let Expr::FnCall(callee, args) = &pr.node else {
        return None;
    };
    if expr_to_dotted_name(&callee.node).as_deref() != Some(fd.name.as_str()) || args.len() != 2 {
        return None;
    }
    let Expr::FnCall(hcallee, hargs) = &args[0].node else {
        return None;
    };
    if expr_to_dotted_name(&hcallee.node).as_deref() != Some(halve_name.as_str())
        || hargs.len() != 1
        || !matches_ident(&hargs[0], a)
        || !matches_ident(&args[1], b)
    {
        return None;
    }
    Some(halve_name)
}

/// `pow(m - 1) <= j && j < pow(m)` — the bit-width window predicate
/// over `(j: Int, m: Int) -> Bool`.
fn is_fits_shape(fd: &FnDef, pow_fn: &str) -> bool {
    use crate::ast::Stmt;
    let [(j, tj), (m, tm)] = fd.params.as_slice() else {
        return false;
    };
    if tj != "Int" || tm != "Int" || fd.return_type != "Bool" {
        return false;
    }
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return false;
    };
    is_window_conjunction(
        body,
        pow_fn,
        &WindowBounds {
            lo_exp: ExpForm::SubLit(m, 1),
            mid: MidForm::Ident(j),
            hi_exp: ExpForm::Ident(m),
        },
    )
}

/// `pow(m + n - 2) <= j * k && j * k < pow(m + n)` — the product
/// window claim over `(j, k, m, n)` all Int, Bool return.
fn is_product_claim_shape(fd: &FnDef, pow_fn: &str) -> bool {
    use crate::ast::Stmt;
    let [(j, tj), (k, tk), (m, tm), (n, tn)] = fd.params.as_slice() else {
        return false;
    };
    if tj != "Int" || tk != "Int" || tm != "Int" || tn != "Int" || fd.return_type != "Bool" {
        return false;
    }
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return false;
    };
    is_window_conjunction(
        body,
        pow_fn,
        &WindowBounds {
            lo_exp: ExpForm::AddSubLit(m, n, 2),
            mid: MidForm::Mul(j, k),
            hi_exp: ExpForm::Add(m, n),
        },
    )
}

/// The scaled-significand shape:
/// ```text
/// e = exp(a, b)
/// s = n - 1 - e
/// match s >= 0
///     true -> Result.withDefault(Int.div(a * pow(s), b), 0)
///     false -> Result.withDefault(Int.div(a, b * pow(0 - s)), 0)
/// ```
/// over `(a, b, n)` all Int, Int return.
fn is_sig_shape(fd: &FnDef, exp_fn: &str, pow_fn: &str) -> bool {
    use crate::ast::{BinOp, Stmt};
    let [(a, ta), (b, tb), (n, tn)] = fd.params.as_slice() else {
        return false;
    };
    if ta != "Int" || tb != "Int" || tn != "Int" || fd.return_type != "Int" {
        return false;
    }
    let [
        Stmt::Binding(e_name, _, e_val),
        Stmt::Binding(s_name, _, s_val),
        Stmt::Expr(body),
    ] = fd.body.stmts()
    else {
        return false;
    };
    // e = exp(a, b)
    if !is_call_of(e_val, exp_fn, &[a, b]) {
        return false;
    }
    // s = n - 1 - e
    let Expr::BinOp(BinOp::Sub, outer_l, outer_r) = &s_val.node else {
        return false;
    };
    if !matches_ident(outer_r, e_name) {
        return false;
    }
    let Expr::BinOp(BinOp::Sub, nl, nr) = &outer_l.node else {
        return false;
    };
    if !matches_ident(nl, n) || !is_int_lit(nr, 1) {
        return false;
    }
    // match s >= 0 { true -> ...; false -> ... }
    let Expr::Match { subject, arms } = &body.node else {
        return false;
    };
    let Expr::BinOp(BinOp::Gte, gl, gr) = &subject.node else {
        return false;
    };
    if !matches_ident(gl, s_name) || !is_int_lit(gr, 0) {
        return false;
    }
    let (Some(pos), Some(neg)) = (bool_arm(arms, true), bool_arm(arms, false)) else {
        return false;
    };
    // true arm: withDefault(div(a * pow(s), b), 0)
    let Some((pos_num, pos_den)) = with_default_div_operands(pos) else {
        return false;
    };
    let Expr::BinOp(BinOp::Mul, pnl, pnr) = &pos_num.node else {
        return false;
    };
    if !matches_ident(pnl, a)
        || !is_pow_call_of(pnr, pow_fn, |arg| matches_ident(arg, s_name))
        || !matches_ident(pos_den, b)
    {
        return false;
    }
    // false arm: withDefault(div(a, b * pow(0 - s)), 0)
    let Some((neg_num, neg_den)) = with_default_div_operands(neg) else {
        return false;
    };
    if !matches_ident(neg_num, a) {
        return false;
    }
    let Expr::BinOp(BinOp::Mul, dnl, dnr) = &neg_den.node else {
        return false;
    };
    matches_ident(dnl, b)
        && is_pow_call_of(dnr, pow_fn, |arg| {
            let Expr::BinOp(BinOp::Sub, zl, zr) = &arg.node else {
                return false;
            };
            is_int_lit(zl, 0) && matches_ident(zr, s_name)
        })
}

/// `j = sig(a, b, n); pow(n - 1) <= j && j < pow(n)` — the
/// significand window predicate over `(a, b, n)` all Int, Bool return.
fn is_window_pred_shape(fd: &FnDef, sig_fn: &str, pow_fn: &str) -> bool {
    use crate::ast::Stmt;
    let [(a, ta), (b, tb), (n, tn)] = fd.params.as_slice() else {
        return false;
    };
    if ta != "Int" || tb != "Int" || tn != "Int" || fd.return_type != "Bool" {
        return false;
    }
    let [Stmt::Binding(j_name, _, j_val), Stmt::Expr(body)] = fd.body.stmts() else {
        return false;
    };
    if !is_call_of(j_val, sig_fn, &[a, b, n]) {
        return false;
    }
    is_window_conjunction(
        body,
        pow_fn,
        &WindowBounds {
            lo_exp: ExpForm::SubLit(n, 1),
            mid: MidForm::Ident(j_name),
            hi_exp: ExpForm::Ident(n),
        },
    )
}

/// Exponent-argument forms a window conjunction recognizes.
enum ExpForm<'a> {
    /// `name`
    Ident(&'a str),
    /// `name - lit`
    SubLit(&'a str, i64),
    /// `name1 + name2`
    Add(&'a str, &'a str),
    /// `name1 + name2 - lit`
    AddSubLit(&'a str, &'a str, i64),
}

/// Middle-term forms (the value the window brackets).
enum MidForm<'a> {
    Ident(&'a str),
    /// `name1 * name2`
    Mul(&'a str, &'a str),
}

struct WindowBounds<'a> {
    lo_exp: ExpForm<'a>,
    mid: MidForm<'a>,
    hi_exp: ExpForm<'a>,
}

/// `Bool.and(pow(<lo_exp>) <= <mid>, <mid> < pow(<hi_exp>))`.
fn is_window_conjunction(expr: &Spanned<Expr>, pow_fn: &str, bounds: &WindowBounds) -> bool {
    use crate::ast::BinOp;
    let Expr::FnCall(callee, args) = &expr.node else {
        return false;
    };
    if expr_to_dotted_name(&callee.node).as_deref() != Some("Bool.and") || args.len() != 2 {
        return false;
    }
    let Expr::BinOp(BinOp::Lte, lo_l, lo_r) = &args[0].node else {
        return false;
    };
    let Expr::BinOp(BinOp::Lt, hi_l, hi_r) = &args[1].node else {
        return false;
    };
    is_pow_call_of(lo_l, pow_fn, |arg| matches_exp_form(arg, &bounds.lo_exp))
        && matches_mid_form(lo_r, &bounds.mid)
        && matches_mid_form(hi_l, &bounds.mid)
        && is_pow_call_of(hi_r, pow_fn, |arg| matches_exp_form(arg, &bounds.hi_exp))
}

fn matches_exp_form(expr: &Spanned<Expr>, form: &ExpForm) -> bool {
    use crate::ast::BinOp;
    match form {
        ExpForm::Ident(name) => matches_ident(expr, name),
        ExpForm::SubLit(name, lit) => {
            let Expr::BinOp(BinOp::Sub, l, r) = &expr.node else {
                return false;
            };
            matches_ident(l, name) && is_int_lit(r, *lit)
        }
        ExpForm::Add(n1, n2) => {
            let Expr::BinOp(BinOp::Add, l, r) = &expr.node else {
                return false;
            };
            matches_ident(l, n1) && matches_ident(r, n2)
        }
        ExpForm::AddSubLit(n1, n2, lit) => {
            let Expr::BinOp(BinOp::Sub, l, r) = &expr.node else {
                return false;
            };
            if !is_int_lit(r, *lit) {
                return false;
            }
            let Expr::BinOp(BinOp::Add, al, ar) = &l.node else {
                return false;
            };
            matches_ident(al, n1) && matches_ident(ar, n2)
        }
    }
}

fn matches_mid_form(expr: &Spanned<Expr>, form: &MidForm) -> bool {
    use crate::ast::BinOp;
    match form {
        MidForm::Ident(name) => matches_ident(expr, name),
        MidForm::Mul(n1, n2) => {
            let Expr::BinOp(BinOp::Mul, l, r) = &expr.node else {
                return false;
            };
            matches_ident(l, n1) && matches_ident(r, n2)
        }
    }
}

/// `Result.withDefault(Int.div(<num>, <den>), <int literal>)` —
/// returns the division operands.
fn with_default_div_operands(expr: &Spanned<Expr>) -> Option<(&Spanned<Expr>, &Spanned<Expr>)> {
    use crate::ast::Literal;
    let Expr::FnCall(callee, args) = &expr.node else {
        return None;
    };
    if expr_to_dotted_name(&callee.node).as_deref() != Some("Result.withDefault")
        || args.len() != 2
        || !matches!(&args[1].node, Expr::Literal(Literal::Int(_)))
    {
        return None;
    }
    let Expr::FnCall(div_callee, div_args) = &args[0].node else {
        return None;
    };
    if expr_to_dotted_name(&div_callee.node).as_deref() != Some("Int.div") || div_args.len() != 2 {
        return None;
    }
    Some((&div_args[0], &div_args[1]))
}

/// Body of the `true`/`false` literal arm of a Bool match.
fn bool_arm(arms: &[crate::ast::MatchArm], value: bool) -> Option<&Spanned<Expr>> {
    arms.iter().find_map(|arm| match &arm.pattern {
        crate::ast::Pattern::Literal(crate::ast::Literal::Bool(b)) if *b == value => {
            Some(arm.body.as_ref())
        }
        _ => None,
    })
}

fn matches_ident(expr: &Spanned<Expr>, name: &str) -> bool {
    crate::codegen::recursion::detect::local_name_of(expr).is_some_and(|n| n == name)
}

fn is_int_lit(expr: &Spanned<Expr>, value: i64) -> bool {
    matches!(&expr.node, Expr::Literal(crate::ast::Literal::Int(v)) if *v == value)
}

/// `fn_name(<idents...>)` where the args are exactly the named locals.
fn is_call_of(expr: &Spanned<Expr>, fn_name: &str, arg_names: &[&String]) -> bool {
    let Expr::FnCall(callee, args) = &expr.node else {
        return false;
    };
    expr_to_dotted_name(&callee.node).as_deref() == Some(fn_name)
        && args.len() == arg_names.len()
        && args
            .iter()
            .zip(arg_names)
            .all(|(arg, name)| matches_ident(arg, name))
}

/// `pow_fn(<arg>)` where `<arg>` satisfies the predicate.
fn is_pow_call_of(
    expr: &Spanned<Expr>,
    pow_fn: &str,
    arg_ok: impl Fn(&Spanned<Expr>) -> bool,
) -> bool {
    let Expr::FnCall(callee, args) = &expr.node else {
        return false;
    };
    expr_to_dotted_name(&callee.node).as_deref() == Some(pow_fn)
        && args.len() == 1
        && arg_ok(&args[0])
}

/// Flatten a left-associated `Bool.and` chain into its conjuncts.
fn when_conjuncts(when: &Spanned<Expr>) -> Vec<&Spanned<Expr>> {
    let Expr::FnCall(callee, args) = &when.node else {
        return vec![when];
    };
    if expr_to_dotted_name(&callee.node).as_deref() != Some("Bool.and") || args.len() != 2 {
        return vec![when];
    }
    let mut out = when_conjuncts(&args[0]);
    out.push(&args[1]);
    out
}

/// `<name> >= <lit>` over a given.
fn is_ge_lit(expr: &Spanned<Expr>, name: &str, lit: i64) -> bool {
    let Expr::BinOp(crate::ast::BinOp::Gte, l, r) = &expr.node else {
        return false;
    };
    matches_ident(l, name) && is_int_lit(r, lit)
}

/// `<name1> >= <name2>` over two givens.
fn is_ge_ident(expr: &Spanned<Expr>, n1: &str, n2: &str) -> bool {
    let Expr::BinOp(crate::ast::BinOp::Gte, l, r) = &expr.node else {
        return false;
    };
    matches_ident(l, n1) && matches_ident(r, n2)
}

/// Figure: `pow(n) >= 1 => true`, no premise.
fn detect_pow_positive(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
) -> Option<FloorWindowFigure> {
    if law.when.is_some() {
        return None;
    }
    let [g] = law.givens.as_slice() else {
        return None;
    };
    if g.type_name != "Int" {
        return None;
    }
    let pow_fd = resolve_pure_fn(fn_name, inputs)?;
    if !is_pow2_shape(pow_fd) {
        return None;
    }
    if !matches!(law.rhs.node, Expr::Literal(crate::ast::Literal::Bool(true))) {
        return None;
    }
    let Expr::BinOp(crate::ast::BinOp::Gte, l, r) = &law.lhs.node else {
        return None;
    };
    if !is_int_lit(r, 1) || !is_pow_call_of(l, &pow_fd.name, |arg| matches_ident(arg, &g.name)) {
        return None;
    }
    Some(FloorWindowFigure::PowPositive {
        pow_fn: pow_fd.name.clone(),
    })
}

/// Figure: `when m >= 0; n >= 0 -> pow(m + n) == pow(m) * pow(n)`.
fn detect_pow_sum_split(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
) -> Option<FloorWindowFigure> {
    let when = law.when.as_ref()?;
    let [gm, gn] = law.givens.as_slice() else {
        return None;
    };
    if gm.type_name != "Int" || gn.type_name != "Int" || gm.name == gn.name {
        return None;
    }
    let pow_fd = resolve_pure_fn(fn_name, inputs)?;
    if !is_pow2_shape(pow_fd) {
        return None;
    }
    let conj = when_conjuncts(when);
    let [w1, w2] = conj.as_slice() else {
        return None;
    };
    if !is_ge_lit(w1, &gm.name, 0) || !is_ge_lit(w2, &gn.name, 0) {
        return None;
    }
    // lhs: pow(m + n)
    if !is_pow_call_of(&law.lhs, &pow_fd.name, |arg| {
        matches_exp_form(arg, &ExpForm::Add(&gm.name, &gn.name))
    }) {
        return None;
    }
    // rhs: pow(m) * pow(n)
    let Expr::BinOp(crate::ast::BinOp::Mul, rl, rr) = &law.rhs.node else {
        return None;
    };
    if !is_pow_call_of(rl, &pow_fd.name, |arg| matches_ident(arg, &gm.name))
        || !is_pow_call_of(rr, &pow_fd.name, |arg| matches_ident(arg, &gn.name))
    {
        return None;
    }
    Some(FloorWindowFigure::PowSumSplit {
        pow_fn: pow_fd.name.clone(),
    })
}

/// Figure: `when b >= 1; a >= b; n >= 1 -> window(a, b, n) => true`.
fn detect_sig_window(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
    fn_contracts: &std::collections::HashMap<crate::ir::FnId, crate::ir::FnContract>,
) -> Option<FloorWindowFigure> {
    let when = law.when.as_ref()?;
    if !matches!(law.rhs.node, Expr::Literal(crate::ast::Literal::Bool(true))) {
        return None;
    }
    let [ga, gb, gn] = law.givens.as_slice() else {
        return None;
    };
    if law.givens.iter().any(|g| g.type_name != "Int") {
        return None;
    }
    // lhs: window(a, b, n) — the window predicate applied to the
    // three givens in order.
    let Expr::FnCall(callee, args) = &law.lhs.node else {
        return None;
    };
    let window_name = expr_to_dotted_name(&callee.node)?;
    if args.len() != 3
        || !matches_ident(&args[0], &ga.name)
        || !matches_ident(&args[1], &gb.name)
        || !matches_ident(&args[2], &gn.name)
    {
        return None;
    }
    // when: b >= 1; a >= b; n >= 1.
    let conj = when_conjuncts(when);
    let [w1, w2, w3] = conj.as_slice() else {
        return None;
    };
    if !is_ge_lit(w1, &gb.name, 1)
        || !is_ge_ident(w2, &ga.name, &gb.name)
        || !is_ge_lit(w3, &gn.name, 1)
    {
        return None;
    }
    // The law subject fn is the significand fn; the window predicate
    // wraps it.
    let sig_fd = resolve_pure_fn(fn_name, inputs)?;
    let window_fd = resolve_pure_fn(&window_name, inputs)?;
    // Pull the pow fn name out of the window predicate's conjunction.
    let pow_name = window_pred_pow_name(window_fd, &sig_fd.name)?;
    let pow_fd = resolve_pure_fn(&pow_name, inputs)?;
    if !is_pow2_shape(pow_fd) {
        return None;
    }
    if !is_window_pred_shape(window_fd, &sig_fd.name, &pow_fd.name) {
        return None;
    }
    let exp_name = sig_shape_exp_name(sig_fd)?;
    let exp_fd = resolve_pure_fn(&exp_name, inputs)?;
    if !is_sig_shape(sig_fd, &exp_fd.name, &pow_fd.name) {
        return None;
    }
    let halve_name = is_binade_exp_shape(exp_fd, inputs, fn_contracts)?;
    Some(FloorWindowFigure::SigWindow {
        pow_fn: pow_fd.name.clone(),
        halve_fn: halve_name,
        exp_fn: exp_fd.name.clone(),
        sig_fn: sig_fd.name.clone(),
        window_fn: window_fd.name.clone(),
    })
}

/// Figure: `when fits(j, m); fits(k, n) -> claim(j, k, m, n) => true`.
fn detect_product_window(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
) -> Option<FloorWindowFigure> {
    let when = law.when.as_ref()?;
    if !matches!(law.rhs.node, Expr::Literal(crate::ast::Literal::Bool(true))) {
        return None;
    }
    let [gj, gk, gm, gn] = law.givens.as_slice() else {
        return None;
    };
    if law.givens.iter().any(|g| g.type_name != "Int") {
        return None;
    }
    // lhs: claim(j, k, m, n) over the four givens in order.
    let Expr::FnCall(callee, args) = &law.lhs.node else {
        return None;
    };
    if expr_to_dotted_name(&callee.node).as_deref() != Some(fn_name)
        || args.len() != 4
        || !matches_ident(&args[0], &gj.name)
        || !matches_ident(&args[1], &gk.name)
        || !matches_ident(&args[2], &gm.name)
        || !matches_ident(&args[3], &gn.name)
    {
        return None;
    }
    // when: fits(j, m) && fits(k, n) — both conjuncts call the same
    // window predicate.
    let conj = when_conjuncts(when);
    let [w1, w2] = conj.as_slice() else {
        return None;
    };
    let Expr::FnCall(f1, a1) = &w1.node else {
        return None;
    };
    let fits_name = expr_to_dotted_name(&f1.node)?;
    if a1.len() != 2 || !matches_ident(&a1[0], &gj.name) || !matches_ident(&a1[1], &gm.name) {
        return None;
    }
    let Expr::FnCall(f2, a2) = &w2.node else {
        return None;
    };
    if expr_to_dotted_name(&f2.node).as_deref() != Some(fits_name.as_str())
        || a2.len() != 2
        || !matches_ident(&a2[0], &gk.name)
        || !matches_ident(&a2[1], &gn.name)
    {
        return None;
    }
    let claim_fd = resolve_pure_fn(fn_name, inputs)?;
    let fits_fd = resolve_pure_fn(&fits_name, inputs)?;
    let pow_name = fits_shape_pow_name(fits_fd)?;
    let pow_fd = resolve_pure_fn(&pow_name, inputs)?;
    if !is_pow2_shape(pow_fd)
        || !is_fits_shape(fits_fd, &pow_fd.name)
        || !is_product_claim_shape(claim_fd, &pow_fd.name)
    {
        return None;
    }
    Some(FloorWindowFigure::ProductWindow {
        pow_fn: pow_fd.name.clone(),
        fits_fn: fits_fd.name.clone(),
        claim_fn: claim_fd.name.clone(),
    })
}

/// Pull the exponent fn's name from the significand shape's first
/// binding (`e = exp(a, b)`).
fn sig_shape_exp_name(fd: &FnDef) -> Option<String> {
    use crate::ast::Stmt;
    let [Stmt::Binding(_, _, e_val), ..] = fd.body.stmts() else {
        return None;
    };
    let Expr::FnCall(callee, _) = &e_val.node else {
        return None;
    };
    expr_to_dotted_name(&callee.node)
}

/// Pull the pow fn's name from a window predicate's conjunction
/// (`pow(n - 1) <= j && ...` after the `j = sig(...)` binding).
fn window_pred_pow_name(fd: &FnDef, _sig_fn: &str) -> Option<String> {
    use crate::ast::Stmt;
    let [_, Stmt::Expr(body)] = fd.body.stmts() else {
        return None;
    };
    first_pow_name_in_conjunction(body)
}

/// Pull the pow fn's name from the fits-shape body (a bare window
/// conjunction).
fn fits_shape_pow_name(fd: &FnDef) -> Option<String> {
    use crate::ast::Stmt;
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return None;
    };
    first_pow_name_in_conjunction(body)
}

fn first_pow_name_in_conjunction(body: &Spanned<Expr>) -> Option<String> {
    use crate::ast::BinOp;
    let Expr::FnCall(callee, args) = &body.node else {
        return None;
    };
    if expr_to_dotted_name(&callee.node).as_deref() != Some("Bool.and") || args.len() != 2 {
        return None;
    }
    let Expr::BinOp(BinOp::Lte, lo_l, _) = &args[0].node else {
        return None;
    };
    let Expr::FnCall(pow_callee, _) = &lo_l.node else {
        return None;
    };
    expr_to_dotted_name(&pow_callee.node)
}

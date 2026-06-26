//! `NonlinearNonneg` strategy detector — nonnegativity / order laws over
//! nonlinear Int products (`E >= 0`, or `prod <= prod`), the Newton-Raphson
//! error-bound family of the K5 division proof.
//!
//! This is the inequality sibling of [`super::ring`]: where the ring
//! detector keys on unconditional *equality* identities over Int-record
//! carriers, this one keys on *order* claims — a nonnegativity `E >= 0` or
//! a product-order `L <= R` (both sides products) — over a pure-Int product
//! cone, and, unlike the ring rung, admits a `when` premise (the guards
//! that constrain the factor signs). The closing primitive on the Lean side
//! is a single generic tactic (`aver_int_order`, shipped in the prelude),
//! the nonlinear analog of `omega` for the products-and-squares fragment:
//! it recurses with `Int.mul_nonneg` (nonneg goal) or `Int.mul_le_mul`
//! (product order), bottoms squares out on `aver_sq_nonneg`, and discharges
//! the leaves from the premise — one generic decision step, never a
//! per-figure template. A `prod <= var` transitivity claim is NOT admitted
//! (it needs a witness the step cannot synthesize). Z3 carries these
//! natively, so the Dafny backend treats the pin like `BackendDispatch`.

use super::*;

/// Detector for [`crate::ir::ProofStrategy::NonlinearNonneg`]. Runs just
/// ahead of the `LinearArithmetic` catch-all (which would otherwise claim
/// these all-Int laws and, unable to decide the nonlinear goal with
/// `omega`, render them on the bounded sampled fallback), so a nonneg/order
/// law no cheaper rung closes is lifted to its true universal statement
/// instead of being pinned to its finite `given` sample. Keys on STRUCTURE
/// only — never on identifier names:
///
/// - every given is `Int` (the family is pure scaled-integer algebra;
///   the paper cross-multiplies the rationals into `Int` form), and
///   there is at least one;
/// - the claim is `subject(args…) = true` where `subject` is a pure
///   non-recursive `Bool` fn whose body is a recognized comparison — a
///   nonnegativity `E >= 0` (`0 <= E`) or a product-order `L <= R` / `L >= R`
///   with both sides products — carrying a genuine nonlinear product;
/// - `E`, and the whole transitively-reached call cone, is pure
///   non-recursive Int arithmetic — Int literals, `+`, `-`, `*`, unary
///   negation, and calls to value-returning cone fns of the same shape;
/// - the law MAY carry a `when` premise (the factor-sign guards); it is
///   threaded into the universal statement as a hypothesis and the
///   generic closer reads it. A premise the closer cannot use only makes
///   the proof fall to its honest `sorry` floor — never an unsound
///   credit — so the premise itself is not over-validated here.
///
/// Returns the ordered unfold list (subject first, then the sorted rest)
/// — the cone the Lean emit unfolds before invoking the generic tactic.
pub(super) fn detect_nonlinear_nonneg(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
) -> Option<Vec<String>> {
    use std::collections::BTreeSet;

    // Given alphabet: every given is a plain `Int`, at least one.
    if law.givens.is_empty() {
        return None;
    }
    if !law.givens.iter().all(|g| g.type_name.trim() == "Int") {
        return None;
    }

    let resolve_user_fn = |name: &str| -> Option<&FnDef> {
        let fd = inputs.find_fn_def_by_call_name(name)?;
        if !fd.effects.is_empty() || fd.name == "main" {
            return None;
        }
        Some(fd)
    };
    let subject = resolve_user_fn(fn_name)?;
    if subject.return_type != "Bool" {
        return None;
    }
    let recursive = inputs.recursive_pure_fn_names();

    // Claim shape: lhs is a call to the subject (the `holds` form lowers
    // `subject(args) holds` to `subject(args) = true`), rhs is the literal
    // `true`. (A non-`holds` value claim is the ring/linear rung's job.)
    let lhs_is_subject_call = match &law.lhs.node {
        crate::ast::Expr::FnCall(callee, _args) => expr_to_dotted_name(&callee.node)
            .and_then(|n| inputs.find_fn_def_by_call_name(&n))
            .is_some_and(|fd| fd.name == subject.name),
        _ => false,
    };
    if !lhs_is_subject_call {
        return None;
    }
    if !matches!(
        law.rhs.node,
        crate::ast::Expr::Literal(crate::ast::Literal::Bool(true))
    ) {
        return None;
    }

    // Subject body: exactly one expression, a recognized nonneg/order
    // comparison (`E >= 0`, or `prod <= prod`) over pure-Int arithmetic.
    let [crate::ast::Stmt::Expr(body)] = subject.body.stmts() else {
        return None;
    };
    let operands = recognized_comparison(&body.node)?;

    // Seed the unfold cone from both law sides plus the nonnegativity
    // operand, then expand transitively through cone bodies (mirrors
    // [`super::ring::detect_ring_identity`]). Every reached call must
    // resolve to a pure non-recursive Int fn.
    let mut cone: BTreeSet<String> = BTreeSet::new();
    collect_fn_calls_expr(&law.lhs, &mut cone);
    collect_fn_calls_expr(&law.rhs, &mut cone);
    loop {
        let before = cone.len();
        let snapshot: Vec<String> = cone.iter().cloned().collect();
        for name in snapshot {
            let fd = resolve_user_fn(&name)?;
            if recursive.contains(&fd.name) {
                return None;
            }
            for stmt in fd.body.stmts() {
                match stmt {
                    crate::ast::Stmt::Binding(_, _, e) | crate::ast::Stmt::Expr(e) => {
                        collect_fn_calls_expr(e, &mut cone);
                    }
                }
            }
        }
        if cone.len() == before {
            break;
        }
    }
    if !cone.contains(fn_name) {
        return None;
    }

    // Validate every cone fn: all-Int params, single pure-Int-arithmetic
    // body (a `Bool` comparator fn — the subject — is exactly the
    // nonnegativity shape already validated above).
    for name in &cone {
        let fd = resolve_user_fn(name)?;
        if !fd.params.iter().all(|(_, t)| t.trim() == "Int") {
            return None;
        }
        let [crate::ast::Stmt::Expr(cone_body)] = fd.body.stmts() else {
            return None;
        };
        match fd.return_type.as_str() {
            "Bool" => {
                // Only a recognized nonneg/order comparison is allowed in
                // Bool position; any other Bool body is out of this family.
                recognized_comparison(&cone_body.node)?;
            }
            "Int" => {
                if !is_int_arith_expr(cone_body, inputs, &cone) {
                    return None;
                }
            }
            _ => return None,
        }
    }
    // Every comparison operand must be pure Int arithmetic. (The nonlinearity
    // requirement — that `omega` provably cannot decide the goal, so the pin
    // steals nothing the `LinearArithmetic` rung it runs ahead of closes — is
    // already enforced inside `recognized_comparison`.)
    for operand in &operands {
        if !is_int_arith_expr_node(operand, inputs, &cone) {
            return None;
        }
    }

    // Subject first (the Lean emit unfolds the outermost layer first),
    // then the sorted rest — mirrors the ring rung.
    let mut unfold_fns: Vec<String> = vec![fn_name.to_string()];
    unfold_fns.extend(cone.iter().filter(|n| *n != fn_name).cloned());
    Some(unfold_fns)
}

/// If `expr` is a nonneg/order comparison the generic `aver_int_order` step
/// decides, return its operand expressions (for arithmetic validation);
/// otherwise `None`. Two admitted shapes:
///
///  - NONNEG: `E >= 0` / `0 <= E` where `E` carries a nonlinear product
///    (`Int.mul_nonneg` recursion, sign-split squares);
///  - PRODUCT ORDER: `L <= R` / `L >= R` where BOTH `L` and `R` are
///    top-level products (so `Int.mul_le_mul` unifies) and at least one is
///    nonlinear (`e*e <= b*b`, `(d·x-s)⁴ <= s²`).
///
/// A `prod <= var` order claim (`a*c <= m`, the transitivity figure) is
/// deliberately NOT admitted: closing it needs a `≤`-chain witness this
/// generic step does not synthesize, so the pin would only land it on the
/// honest `sorry` floor — a regression from its sound bounded fallback.
fn recognized_comparison(expr: &crate::ast::Expr) -> Option<Vec<&crate::ast::Expr>> {
    use crate::ast::{BinOp, Expr};
    match expr {
        // Nonnegativity against the literal `0`.
        Expr::BinOp(BinOp::Gte, l, r) if is_int_zero(&r.node) => {
            has_nonlinear_product(&l.node).then(|| vec![&l.node])
        }
        Expr::BinOp(BinOp::Lte, l, r) if is_int_zero(&l.node) => {
            has_nonlinear_product(&r.node).then(|| vec![&r.node])
        }
        // Product-order: both sides top-level products, at least one nonlinear.
        Expr::BinOp(BinOp::Lte | BinOp::Gte, l, r)
            if is_product(&l.node)
                && is_product(&r.node)
                && (has_nonlinear_product(&l.node) || has_nonlinear_product(&r.node)) =>
        {
            Some(vec![&l.node, &r.node])
        }
        _ => None,
    }
}

/// True when `expr` is a top-level multiplication — the shape `Int.mul_le_mul`
/// unifies its `?a * ?c ≤ ?b * ?d` conclusion against.
fn is_product(expr: &crate::ast::Expr) -> bool {
    matches!(expr, crate::ast::Expr::BinOp(crate::ast::BinOp::Mul, _, _))
}

/// True for the integer literal `0` (`Int(0)` — the canonical small-literal
/// form `0` always takes; the arbitrary-precision `BigInt` form is never `0`).
fn is_int_zero(expr: &crate::ast::Expr) -> bool {
    matches!(expr, crate::ast::Expr::Literal(crate::ast::Literal::Int(0)))
}

/// True when `expr` contains a genuinely nonlinear product — a `Mul`
/// whose two operands BOTH mention a variable (so `omega` cannot decide a
/// nonnegativity claim over it). A constant-scaled product (`2 * s`) is
/// linear and does not count; the recursion still descends through it to
/// catch a nested variable×variable factor.
fn has_nonlinear_product(expr: &crate::ast::Expr) -> bool {
    use crate::ast::{BinOp, Expr};
    match expr {
        Expr::BinOp(BinOp::Mul, l, r) => {
            (mentions_variable(&l.node) && mentions_variable(&r.node))
                || has_nonlinear_product(&l.node)
                || has_nonlinear_product(&r.node)
        }
        Expr::BinOp(BinOp::Add | BinOp::Sub, l, r) => {
            has_nonlinear_product(&l.node) || has_nonlinear_product(&r.node)
        }
        Expr::Neg(inner) => has_nonlinear_product(&inner.node),
        _ => false,
    }
}

/// True when `expr` mentions a variable — an ident / resolved binding, or a
/// fn call (whose result is variable-dependent). A pure literal/arithmetic
/// of literals does not.
fn mentions_variable(expr: &crate::ast::Expr) -> bool {
    use crate::ast::{BinOp, Expr};
    match expr {
        Expr::Ident(_) | Expr::Resolved { .. } | Expr::FnCall(..) => true,
        Expr::Neg(inner) => mentions_variable(&inner.node),
        Expr::BinOp(BinOp::Add | BinOp::Sub | BinOp::Mul, l, r) => {
            mentions_variable(&l.node) || mentions_variable(&r.node)
        }
        _ => false,
    }
}

/// Value-level pure Int-arithmetic expression: Int literals (small or
/// arbitrary-precision), idents (law givens / fn params), unary negation,
/// `+` / `-` / `*`, and calls to value-returning cone fns. Mirrors
/// [`super::ring::is_ring_value_expr`] minus the record carriers (this
/// family is over plain `Int` givens).
///
/// **syntax-discovery-only**: pattern match over source shape; cone
/// membership goes through the names the caller resolved against fn defs.
fn is_int_arith_expr(
    expr: &Spanned<crate::ast::Expr>,
    inputs: &ProofLowerInputs,
    cone: &std::collections::BTreeSet<String>,
) -> bool {
    is_int_arith_expr_node(&expr.node, inputs, cone)
}

fn is_int_arith_expr_node(
    expr: &crate::ast::Expr,
    inputs: &ProofLowerInputs,
    cone: &std::collections::BTreeSet<String>,
) -> bool {
    use crate::ast::{BinOp, Expr, Literal};
    match expr {
        Expr::Literal(Literal::Int(_)) | Expr::Literal(Literal::BigInt(_)) => true,
        Expr::Ident(_) | Expr::Resolved { .. } => true,
        Expr::Neg(inner) => is_int_arith_expr(inner, inputs, cone),
        Expr::BinOp(BinOp::Add | BinOp::Sub | BinOp::Mul, l, r) => {
            is_int_arith_expr(l, inputs, cone) && is_int_arith_expr(r, inputs, cone)
        }
        Expr::FnCall(callee, args) => {
            let Some(name) = expr_to_dotted_name(&callee.node) else {
                return false;
            };
            let Some(fd) = inputs.find_fn_def_by_call_name(&name) else {
                return false;
            };
            cone.contains(&fd.name)
                && fd.return_type == "Int"
                && args.iter().all(|a| is_int_arith_expr(a, inputs, cone))
        }
        _ => false,
    }
}

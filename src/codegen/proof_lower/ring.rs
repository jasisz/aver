//! `RingIdentity` strategy detector — unconditional ring identities
//! over Int-component records (exact-rationals-style libraries with
//! cross-multiplication equality).
//!
//! Split from `proof_lower.rs` — see the module docs in [`super`].

use super::*;

/// Detector for [`crate::ir::ProofStrategy::RingIdentity`]. Runs
/// after every algebraic / spec-equivalence / linear-arithmetic rung
/// declined and BEFORE the prelude-simp fallback, which would
/// otherwise claim the shape and park it on a caught sorry (its simp
/// set has no AC-ring normalization). Keys on STRUCTURE only — never
/// on identifier names:
///
/// - no `when` (enforced at the call site, mirroring the sibling
///   fallbacks) and at least one given;
/// - every given is `Int` or an all-Int-field record with ≥ 2 fields
///   (the multi-component carrier shape — single-field records are
///   the refinement-carrier shape owned by the `LinearArithmetic`
///   lifted path), and at least one given IS such a record;
/// - the law's call cone (lhs + rhs, transitively expanded) consists
///   of pure non-recursive fns whose params/returns stay inside the
///   same type alphabet and whose bodies are single pure-arithmetic
///   expressions: record creates / field projections, Int literals,
///   `+`, `-`, `*`, unary negation — `Bool` only as the return of a
///   comparator fn whose body is one Int `==` over two such
///   expressions;
/// - the claim is either `comparator(args…) = true` (cross-
///   multiplication equality) or a direct value equality of two
///   arithmetic expressions.
///
/// Returns the ordered unfold list (law subject first, then sorted
/// rest), the only payload the backends need: the AC-ring lemma
/// package itself is backend vocabulary (Lean names), so it lives
/// next to the Lean emit, not in the IR.
pub(super) fn detect_ring_identity(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
) -> Option<Vec<String>> {
    use std::collections::BTreeSet;

    if law.givens.is_empty() {
        return None;
    }
    // Given alphabet: Int or all-Int multi-field record; ≥ 1 record.
    if !law
        .givens
        .iter()
        .all(|g| g.type_name == "Int" || is_all_int_record(&g.type_name, inputs))
    {
        return None;
    }
    if !law
        .givens
        .iter()
        .any(|g| is_all_int_record(&g.type_name, inputs))
    {
        return None;
    }

    let resolve_user_fn = |name: &str| -> Option<&FnDef> {
        let fd = inputs.find_fn_def_by_call_name(name)?;
        if !fd.effects.is_empty() || fd.name == "main" {
            return None;
        }
        Some(fd)
    };
    resolve_user_fn(fn_name)?;
    let recursive = inputs.recursive_pure_fn_names();

    // Seed the cone from both law sides, then expand transitively
    // through cone bodies. Every reached call must resolve to a pure
    // non-recursive user fn — a builtin or recursive callee anywhere
    // disqualifies the law (the body validator below re-checks calls
    // structurally; this loop just discovers the unfold set).
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

    // Validate every cone fn: type alphabet + single pure-arithmetic
    // body. `Bool` return is the comparator shape — exactly one Int
    // `==` over two value expressions.
    for name in &cone {
        let fd = resolve_user_fn(name)?;
        if !fd
            .params
            .iter()
            .all(|(_, t)| t == "Int" || is_all_int_record(t, inputs))
        {
            return None;
        }
        let [crate::ast::Stmt::Expr(body)] = fd.body.stmts() else {
            return None;
        };
        let ret = fd.return_type.as_str();
        if ret == "Bool" {
            let crate::ast::Expr::BinOp(crate::ast::BinOp::Eq, l, r) = &body.node else {
                return None;
            };
            if !is_ring_value_expr(l, inputs, &cone) || !is_ring_value_expr(r, inputs, &cone) {
                return None;
            }
        } else {
            if ret != "Int" && !is_all_int_record(ret, inputs) {
                return None;
            }
            if !is_ring_value_expr(body, inputs, &cone) {
                return None;
            }
        }
    }

    // Claim shape. Form A: `comparator(args…) = true` — lhs calls a
    // Bool cone fn, rhs is the literal `true`. Form B: direct value
    // equality of two arithmetic expressions (value validator
    // rejects Bool-returning calls in value position).
    let lhs_is_comparator_call = match &law.lhs.node {
        crate::ast::Expr::FnCall(callee, args) => expr_to_dotted_name(&callee.node)
            .and_then(|n| resolve_user_fn(&n))
            .is_some_and(|fd| {
                fd.return_type == "Bool"
                    && cone.contains(&fd.name)
                    && args.iter().all(|a| is_ring_value_expr(a, inputs, &cone))
            }),
        _ => false,
    };
    if lhs_is_comparator_call {
        if !matches!(
            law.rhs.node,
            crate::ast::Expr::Literal(crate::ast::Literal::Bool(true))
        ) {
            return None;
        }
    } else if !is_ring_value_expr(&law.lhs, inputs, &cone)
        || !is_ring_value_expr(&law.rhs, inputs, &cone)
    {
        return None;
    }

    // Subject first (Lean peels the outermost call layer first), then
    // the sorted rest — mirrors `detect_simp_over_prelude_lemmas`.
    let mut unfold_fns: Vec<String> = vec![fn_name.to_string()];
    unfold_fns.extend(cone.iter().filter(|n| *n != fn_name).cloned());
    Some(unfold_fns)
}

/// True when `type_name` resolves to a user record (`TypeDef::Product`)
/// with at least TWO fields, all annotated `Int`. The ≥ 2 gate keeps
/// single-field records out — that is the refinement-carrier shape
/// (`record Natural { value: Int }` + smart constructor), owned by
/// the `LinearArithmetic` lifted path and the Subtype/subset emit.
fn is_all_int_record(type_name: &str, inputs: &ProofLowerInputs) -> bool {
    matches!(
        inputs.find_type_def(type_name),
        Some(crate::ast::TypeDef::Product { fields, .. })
            if fields.len() >= 2 && fields.iter().all(|(_, t)| t.trim() == "Int")
    )
}

/// Value-level pure-arithmetic expression over the ring alphabet:
/// Int literals, idents (law givens / fn params), unary negation,
/// `+` / `-` / `*`, lowercase field projections, all-Int record
/// creates, and calls to VALUE-returning cone fns (a Bool comparator
/// in value position disqualifies — `Bool` only appears at the law
/// root / comparator body, where the callers validate it).
///
/// **syntax-discovery-only** (epic #170 Phase 8 guardrail): pattern
/// match over source shape; the cone-membership check goes through
/// the names the caller already resolved against user fn defs.
fn is_ring_value_expr(
    expr: &Spanned<crate::ast::Expr>,
    inputs: &ProofLowerInputs,
    cone: &std::collections::BTreeSet<String>,
) -> bool {
    use crate::ast::{BinOp, Expr, Literal};
    match &expr.node {
        Expr::Literal(Literal::Int(_)) => true,
        Expr::Ident(_) | Expr::Resolved { .. } => true,
        Expr::Neg(inner) => is_ring_value_expr(inner, inputs, cone),
        // Field projection (`a.num`): lowercase field over a ring
        // value. An uppercase "field" is a fieldless-ctor / namespace
        // handle, not arithmetic — reject.
        Expr::Attr(obj, field) => {
            field.chars().next().is_some_and(|c| c.is_lowercase())
                && is_ring_value_expr(obj, inputs, cone)
        }
        Expr::BinOp(BinOp::Add | BinOp::Sub | BinOp::Mul, l, r) => {
            is_ring_value_expr(l, inputs, cone) && is_ring_value_expr(r, inputs, cone)
        }
        Expr::FnCall(callee, args) => {
            let Some(name) = expr_to_dotted_name(&callee.node) else {
                return false;
            };
            let Some(fd) = inputs.find_fn_def_by_call_name(&name) else {
                return false;
            };
            cone.contains(&fd.name)
                && fd.return_type != "Bool"
                && args.iter().all(|a| is_ring_value_expr(a, inputs, cone))
        }
        Expr::RecordCreate { type_name, fields } => {
            is_all_int_record(type_name, inputs)
                && fields
                    .iter()
                    .all(|(_, e)| is_ring_value_expr(e, inputs, cone))
        }
        _ => false,
    }
}

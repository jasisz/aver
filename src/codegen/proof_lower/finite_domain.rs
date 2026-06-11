//! `FiniteDomainCases` / `EnumConstantFold` strategy detectors.
//!
//! Split from `proof_lower.rs` — see the module docs in [`super`].

use super::*;

/// Closed finite-domain enumeration detector (the last typed fallback
/// before `BackendDispatch`, after `detect_enum_constant_fold`). Fires
/// iff the law has at least one given and EVERY given's type has a
/// closed, small, enumerable domain:
/// - `Bool` (domain size 2), or
/// - a user-declared enum (`TypeDef::Sum`) whose constructors are ALL
///   fieldless (domain size = constructor count),
///
/// with the product of domain sizes ≤ 16. Exhaustive `cases` over the
/// givens then yields one closed ground goal per domain combination,
/// which `rfl` / `decide` compute out — even through fuel wrappers
/// (constant-measure constructor args compute through fuel) — so
/// unlike `detect_enum_constant_fold` there is deliberately NO
/// call-shape inspection, NO return-type gate and NO recursion gate.
/// A leaf the cascade can't close degrades to an honest caught `sorry`
/// in the Lean emit, never a false universal claim. Returns the given
/// names in intro order (the Lean emitter's `cases` targets); `None`
/// otherwise.
pub(super) fn detect_finite_domain_cases(
    law: &crate::ast::VerifyLaw,
    inputs: &ProofLowerInputs,
) -> Option<Vec<String>> {
    if law.givens.is_empty() {
        return None;
    }
    let mut domain_product: usize = 1;
    for given in &law.givens {
        let size = finite_domain_size(&given.type_name, inputs)?;
        domain_product = domain_product.checked_mul(size)?;
        if domain_product > 16 {
            return None;
        }
    }
    Some(law.givens.iter().map(|g| g.name.clone()).collect())
}

/// Size of a type's closed enumerable domain: 2 for `Bool`, the
/// constructor count for a user-declared enum whose constructors are
/// ALL fieldless. `None` for everything else — open domains
/// (`Int` / `String` / collections), payload-carrying ADTs (whose
/// `cases` would introduce fresh free variables the per-leaf
/// `rfl`/`decide` cascade can't compute out), records, and effect
/// oracle types (`Random.int`, …) which `find_type_def` doesn't
/// resolve.
pub(super) fn finite_domain_size(type_name: &str, inputs: &ProofLowerInputs) -> Option<usize> {
    if type_name == "Bool" {
        return Some(2);
    }
    match inputs.find_type_def(type_name)? {
        crate::ast::TypeDef::Sum { variants, .. }
            if variants.iter().all(|v| v.fields.is_empty()) =>
        {
            Some(variants.len())
        }
        _ => None,
    }
}

/// Ground enum/ADT constant-fold detector (new fallback before
/// `BackendDispatch`). Fires when the verified fn has at least one
/// non-Int (ADT/enum) param and the law pins *every* such param to a
/// constructor literal at the call site — so the constructor selects a
/// fixed match arm and the whole non-recursive call tree folds to a
/// closed term. Any scalar `given`s are quantified but irrelevant to
/// the chosen branch. Returns the ordered unfold list (fn first, then
/// transitively-reached callees) on match; `None` otherwise.
///
/// CONSERVATIVE by construction:
/// - rejects when the fn has no non-Int param (LinearArithmetic /
///   omega owns the all-Int shape);
/// - rejects unless *every* non-Int param at *every* call to `fn_name`
///   in lhs/rhs is a constructor literal (an unpinned ADT arg would
///   leave a free variable the `split`/`rfl` cascade can't discharge);
/// - rejects when any fn in the unfold set is self-recursive (a single
///   `simp only [fn]` step leaves a stale recursive call);
/// - rejects when the verified fn calls a builtin-collection method
///   (`List.*` / `Map.* `/ `Vector.*`) — those don't fold to a ground
///   term by `split`/`rfl`/`decide`.
pub(super) fn detect_enum_constant_fold(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
) -> Option<Vec<String>> {
    use std::collections::BTreeSet;

    let outer_fd = inputs.find_fn_def_by_call_name(fn_name)?;
    // Need at least one non-Int (ADT/enum) param. An all-Int fn is the
    // LinearArithmetic/omega shape; this detector must not poach it.
    let non_int_params: Vec<&str> = outer_fd
        .params
        .iter()
        .enumerate()
        .filter(|(_, (_, t))| t != "Int")
        .map(|(i, _)| outer_fd.params[i].1.as_str())
        .collect();
    if non_int_params.is_empty() {
        return None;
    }
    // The result must be a scalar (`Int` / `Bool`) ground equality the
    // `split`/`rfl`/`decide` cascade can discharge. A wrapper return
    // (Result/Option/record) is out of scope for this fallback.
    let ret = outer_fd.return_type.as_str();
    if ret != "Int" && ret != "Bool" {
        return None;
    }

    // Every call to `fn_name` in lhs/rhs must pin each non-Int param
    // position to a constructor literal. The Int positions may carry
    // the quantified scalar givens (they're irrelevant to the branch).
    let mut saw_call = false;
    let mut ok = true;
    check_enum_calls_pinned(&law.lhs, fn_name, outer_fd, &mut saw_call, &mut ok);
    check_enum_calls_pinned(&law.rhs, fn_name, outer_fd, &mut saw_call, &mut ok);
    if !saw_call || !ok {
        return None;
    }

    // Build the transitive unfold set from both law sides + the outer
    // fn, exactly like `detect_simp_omega_unfold`.
    let mut fn_names: BTreeSet<String> = BTreeSet::new();
    collect_fn_calls_expr(&law.lhs, &mut fn_names);
    collect_fn_calls_expr(&law.rhs, &mut fn_names);
    fn_names.insert(fn_name.to_string());
    loop {
        let before = fn_names.len();
        let snapshot: Vec<String> = fn_names.iter().cloned().collect();
        for fd in iter_all_fn_defs(inputs) {
            if !snapshot.contains(&fd.name) {
                continue;
            }
            for stmt in fd.body.stmts() {
                match stmt {
                    crate::ast::Stmt::Binding(_, _, e) | crate::ast::Stmt::Expr(e) => {
                        collect_fn_calls_expr(e, &mut fn_names);
                    }
                }
            }
        }
        if fn_names.len() == before {
            break;
        }
    }

    // Self-recursion rejection — a single unfold step can't close a
    // recursive call. Mirrors the `detect_simp_omega_unfold` guard.
    for fd in iter_all_fn_defs(inputs) {
        if !fn_names.contains(&fd.name) {
            continue;
        }
        let mut self_only: BTreeSet<String> = BTreeSet::new();
        self_only.insert(fd.name.clone());
        if body_calls_any_of_inputs(&fd.body, &self_only) {
            return None;
        }
    }

    // Order: top-level law fn first (Lean's `simp only`/`unfold` peels
    // the outermost call layer first), then the transitively-reached
    // callees.
    let mut ordered: Vec<String> = Vec::new();
    if fn_names.contains(fn_name) {
        ordered.push(fn_name.to_string());
    }
    for n in &fn_names {
        if n != fn_name {
            ordered.push(n.clone());
        }
    }
    Some(ordered)
}

/// Walk an expr looking for calls to `fn_name`; for each, verify every
/// non-Int param position carries a constructor literal. Sets `saw_call`
/// on the first matched call and clears `ok` on any unpinned ADT arg.
pub(super) fn check_enum_calls_pinned(
    expr: &Spanned<crate::ast::Expr>,
    fn_name: &str,
    fd: &FnDef,
    saw_call: &mut bool,
    ok: &mut bool,
) {
    use crate::ast::Expr;
    match &expr.node {
        Expr::FnCall(callee, args) => {
            if let Some(name) = expr_to_dotted_name(&callee.node) {
                let leaf = name.rsplit('.').next().unwrap_or(&name);
                if leaf == fn_name && args.len() == fd.params.len() {
                    *saw_call = true;
                    for (i, (_, ptype)) in fd.params.iter().enumerate() {
                        if ptype != "Int" && !is_constructor_literal(&args[i].node) {
                            *ok = false;
                        }
                    }
                }
            }
            check_enum_calls_pinned(callee, fn_name, fd, saw_call, ok);
            for a in args {
                check_enum_calls_pinned(a, fn_name, fd, saw_call, ok);
            }
        }
        Expr::BinOp(_, l, r) => {
            check_enum_calls_pinned(l, fn_name, fd, saw_call, ok);
            check_enum_calls_pinned(r, fn_name, fd, saw_call, ok);
        }
        Expr::Neg(inner) | Expr::ErrorProp(inner) => {
            check_enum_calls_pinned(inner, fn_name, fd, saw_call, ok);
        }
        Expr::Attr(obj, _) => check_enum_calls_pinned(obj, fn_name, fd, saw_call, ok),
        Expr::List(elems) | Expr::Tuple(elems) => {
            for e in elems {
                check_enum_calls_pinned(e, fn_name, fd, saw_call, ok);
            }
        }
        _ => {}
    }
}

/// True when `expr` is a fixed ADT/enum constructor literal — either a
/// built-in `Expr::Constructor` (`Option.Some`, `Result.Ok`) or a
/// dotted `Type.Variant` access (`CellContent.Empty`, `Color.Black`)
/// where both segments start uppercase (a user enum variant, not a
/// builtin scalar-namespace method like `Int.max`).
pub(super) fn is_constructor_literal(expr: &crate::ast::Expr) -> bool {
    use crate::ast::Expr;
    match expr {
        Expr::Constructor(_, _) => true,
        Expr::Attr(obj, field) => {
            let head_upper = matches!(
                &obj.node,
                Expr::Ident(n) if n.chars().next().is_some_and(|c| c.is_uppercase())
            );
            let field_upper = field.chars().next().is_some_and(|c| c.is_uppercase());
            head_upper && field_upper
        }
        _ => false,
    }
}

/// True when `expr` is a compile-time Int constant: a literal, a negated
/// constant, or constant arithmetic over constants.
pub(super) fn expr_is_const_int(expr: &Spanned<crate::ast::Expr>) -> bool {
    use crate::ast::Expr;
    match &expr.node {
        Expr::Literal(crate::ast::Literal::Int(_)) => true,
        Expr::Neg(inner) => expr_is_const_int(inner),
        Expr::BinOp(_, l, r) => expr_is_const_int(l) && expr_is_const_int(r),
        _ => false,
    }
}

/// True when the expression tree contains a product of two NON-constant
/// operands (`x * y`, `x * x`, `(s*s - d*x) * (s*s - d*x)`) — nonlinear
/// in the law's quantified variables once the cone is unfolded.
/// `literal * var` does NOT count: omega keeps coefficient products in
/// its linear theory, so those laws stay in scope.
pub(super) fn expr_has_var_product(expr: &Spanned<crate::ast::Expr>) -> bool {
    use crate::ast::Expr;
    match &expr.node {
        Expr::BinOp(crate::ast::BinOp::Mul, l, r) => {
            (!expr_is_const_int(l) && !expr_is_const_int(r))
                || expr_has_var_product(l)
                || expr_has_var_product(r)
        }
        Expr::BinOp(_, l, r) => expr_has_var_product(l) || expr_has_var_product(r),
        Expr::Neg(inner) | Expr::Attr(inner, _) | Expr::ErrorProp(inner) => {
            expr_has_var_product(inner)
        }
        Expr::FnCall(callee, args) => {
            expr_has_var_product(callee) || args.iter().any(expr_has_var_product)
        }
        Expr::Match { subject, arms } => {
            expr_has_var_product(subject) || arms.iter().any(|a| expr_has_var_product(&a.body))
        }
        Expr::Constructor(_, Some(inner)) => expr_has_var_product(inner),
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            items.iter().any(expr_has_var_product)
        }
        Expr::MapLiteral(entries) => entries
            .iter()
            .any(|(k, v)| expr_has_var_product(k) || expr_has_var_product(v)),
        Expr::RecordCreate { fields, .. } => fields.iter().any(|(_, e)| expr_has_var_product(e)),
        Expr::RecordUpdate { base, updates, .. } => {
            expr_has_var_product(base) || updates.iter().any(|(_, e)| expr_has_var_product(e))
        }
        Expr::InterpolatedStr(parts) => parts.iter().any(|p| match p {
            crate::ast::StrPart::Parsed(inner) => expr_has_var_product(inner),
            _ => false,
        }),
        Expr::TailCall(boxed) => boxed.args.iter().any(expr_has_var_product),
        _ => false,
    }
}

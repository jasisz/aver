//! Map library-axiom and map-update / tracked-counter law detectors.
//!
//! Split from `proof_lower.rs` — see the module docs in [`super`].

use super::*;

/// Detect a Map library axiom instance:
///   `Map.has(Map.set(m, k, v), k) => true`        → `Map.has_set_self`
///   `Map.get(Map.set(m, k, v), k) => Option.Some(v)` → `Map.get_set_self`
/// Returns `(axiom_name, [m, k, v])` on match, either side
/// orientation. Both axioms use the same `[m, k, v]` arg order.
pub(super) fn detect_map_set_axiom(
    law: &crate::ast::VerifyLaw,
) -> Option<(String, Vec<Spanned<crate::ast::Expr>>)> {
    // `Map.has(Map.set(m, k, v), k) => true`
    let has_side = |side: &Spanned<crate::ast::Expr>,
                    other: &Spanned<crate::ast::Expr>|
     -> Option<(String, Vec<Spanned<crate::ast::Expr>>)> {
        let (m, k, v) = map_has_set_parts(side)?;
        if !is_bool_true(other) {
            return None;
        }
        Some((
            "Map.has_set_self".to_string(),
            vec![m.clone(), k.clone(), v.clone()],
        ))
    };
    if let Some(found) = has_side(&law.lhs, &law.rhs).or_else(|| has_side(&law.rhs, &law.lhs)) {
        return Some(found);
    }

    // `Map.get(Map.set(m, k, v), k) => Option.Some(v)`
    let get_side = |side: &Spanned<crate::ast::Expr>,
                    other: &Spanned<crate::ast::Expr>|
     -> Option<(String, Vec<Spanned<crate::ast::Expr>>)> {
        let (m, k, v) = map_get_set_parts(side)?;
        let some_v = option_some_arg(other)?;
        if some_v.node != v.node {
            return None;
        }
        Some((
            "Map.get_set_self".to_string(),
            vec![m.clone(), k.clone(), v.clone()],
        ))
    };
    get_side(&law.lhs, &law.rhs).or_else(|| get_side(&law.rhs, &law.lhs))
}

/// Internal scratch carrier — mirrors the IR variant but lives in
/// the lowerer so callers can build incrementally.
pub(super) struct MapUpdatePostconditionPlan {
    pub(super) outer_fn: String,
    pub(super) kind: crate::ir::MapUpdatePostconditionKind,
    pub(super) map_arg: Spanned<crate::ast::Expr>,
    pub(super) key_arg: Spanned<crate::ast::Expr>,
    pub(super) extra_unfolds: Vec<String>,
}

/// Detect a post-condition law on an inline map-update fn `outer(m,
/// k)`. Two shapes: `Map.has(outer(m, k), k) == true` (`HasAfter`),
/// or `Map.get(outer(m, k), k) == Option.Some(...)` (`GetAfter`).
/// Both require `outer`'s body to follow the "inspect get, set in
/// every arm" template (see `outer_fn_map_update_shape`).
pub(super) fn detect_map_update_postcondition(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
) -> Option<MapUpdatePostconditionPlan> {
    use crate::ir::MapUpdatePostconditionKind;

    outer_fn_map_update_shape(fn_name, inputs)?;

    let has_side = |side: &Spanned<crate::ast::Expr>,
                    other: &Spanned<crate::ast::Expr>|
     -> Option<MapUpdatePostconditionPlan> {
        if !is_bool_true(other) {
            return None;
        }
        let (map_arg, key_arg) = map_has_after_fn_call(side, fn_name)?;
        Some(MapUpdatePostconditionPlan {
            outer_fn: fn_name.to_string(),
            kind: MapUpdatePostconditionKind::HasAfter,
            map_arg: map_arg.clone(),
            key_arg: key_arg.clone(),
            extra_unfolds: Vec::new(),
        })
    };
    if let Some(plan) = has_side(&law.lhs, &law.rhs).or_else(|| has_side(&law.rhs, &law.lhs)) {
        return Some(plan);
    }

    let get_side = |side: &Spanned<crate::ast::Expr>,
                    other: &Spanned<crate::ast::Expr>|
     -> Option<MapUpdatePostconditionPlan> {
        option_some_arg(other)?;
        let (map_arg, key_arg) = map_get_after_fn_call(side, fn_name)?;
        let extra_unfolds = law_helper_unfolds(law, fn_name, inputs);
        Some(MapUpdatePostconditionPlan {
            outer_fn: fn_name.to_string(),
            kind: MapUpdatePostconditionKind::GetAfter,
            map_arg: map_arg.clone(),
            key_arg: key_arg.clone(),
            extra_unfolds,
        })
    };
    get_side(&law.lhs, &law.rhs).or_else(|| get_side(&law.rhs, &law.lhs))
}

/// Validate the outer fn's body matches the "inspect get, set in
/// every arm" template:
///
/// ```text
/// fn outer(m: Map<K, V>, k: K) -> Map<K, V>
///   [v = Map.get(m, k);]?
///   match (v | Map.get(m, k)) {
///     _ -> Map.set(m, k, _)
///     _ -> Map.set(m, k, _)
///     ...
///   }
/// ```
///
/// Returns `Some(())` on match. The map/key params are positional —
/// position 0 is the map, position 1 is the key.
pub(super) fn outer_fn_map_update_shape(fn_name: &str, inputs: &ProofLowerInputs) -> Option<()> {
    let fd = inputs.find_fn_def_by_call_name(fn_name)?;
    if fd.params.len() != 2 {
        return None;
    }
    let map_param = fd.params[0].0.as_str();
    let key_param = fd.params[1].0.as_str();
    map_update_body_matches(fd.body.stmts(), map_param, key_param).then_some(())
}

pub(super) fn map_update_body_matches(
    stmts: &[crate::ast::Stmt],
    map_param: &str,
    key_param: &str,
) -> bool {
    use crate::ast::Stmt;
    if stmts.len() < 2 {
        // Even the no-let variant requires the match to be the last
        // stmt — but a single-stmt body is too short to be this shape.
        return matches!(stmts.first(), Some(Stmt::Expr(e)) if map_update_match_expr(e, map_param, key_param, None));
    }
    let Some(last) = stmts.last() else {
        return false;
    };
    let mut bound_name: Option<&str> = None;
    for stmt in &stmts[..stmts.len() - 1] {
        match stmt {
            Stmt::Binding(name, _, expr) => {
                if !is_map_get_of_params(expr, map_param, key_param) {
                    return false;
                }
                bound_name = Some(name);
            }
            Stmt::Expr(_) => return false,
        }
    }
    match last {
        Stmt::Expr(expr) => map_update_match_expr(expr, map_param, key_param, bound_name),
        Stmt::Binding(_, _, _) => false,
    }
}

pub(super) fn map_update_match_expr(
    expr: &Spanned<crate::ast::Expr>,
    map_param: &str,
    key_param: &str,
    bound_name: Option<&str>,
) -> bool {
    use crate::ast::Expr;
    let Expr::Match { subject, arms } = &expr.node else {
        return false;
    };
    if arms.len() < 2 {
        return false;
    }
    let subject_ok = match bound_name {
        Some(name) => matches_ident_expr(subject, name),
        None => is_map_get_of_params(subject, map_param, key_param),
    };
    if !subject_ok {
        return false;
    }
    arms.iter()
        .all(|arm| is_map_set_of_params(&arm.body, map_param, key_param))
}

pub(super) fn is_map_get_of_params(
    expr: &Spanned<crate::ast::Expr>,
    map_param: &str,
    key_param: &str,
) -> bool {
    let Some(args) = call_named_args(expr, "Map.get") else {
        return false;
    };
    args.len() == 2
        && matches_ident_expr(&args[0], map_param)
        && matches_ident_expr(&args[1], key_param)
}

pub(super) fn is_map_set_of_params(
    expr: &Spanned<crate::ast::Expr>,
    map_param: &str,
    key_param: &str,
) -> bool {
    let Some(args) = call_named_args(expr, "Map.set") else {
        return false;
    };
    args.len() == 3
        && matches_ident_expr(&args[0], map_param)
        && matches_ident_expr(&args[1], key_param)
}

pub(super) struct MapKeyTrackedIncrementPlan {
    pub(super) outer_fn: String,
    pub(super) map_arg: Spanned<crate::ast::Expr>,
    pub(super) key_arg: Spanned<crate::ast::Expr>,
}

/// Detect the canonical tracked-counter increment law:
/// `Option.withDefault(Map.get(outer(m, k), k), 0) ==
/// Option.withDefault(Map.get(m, k), 0) + 1`, where `outer` follows
/// the increment-by-one body template (see
/// [`outer_fn_map_increment_shape`]).
pub(super) fn detect_map_key_tracked_increment(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
) -> Option<MapKeyTrackedIncrementPlan> {
    use crate::ast::{BinOp, Expr};

    outer_fn_map_increment_shape(fn_name, inputs)?;

    let side = |after: &Spanned<crate::ast::Expr>,
                rhs: &Spanned<crate::ast::Expr>|
     -> Option<MapKeyTrackedIncrementPlan> {
        let (map_arg, key_arg, default_arg) = defaulted_map_get_after_fn_call(after, fn_name)?;
        if !is_int_lit(default_arg, 0) {
            return None;
        }
        let Expr::BinOp(BinOp::Add, base, one) = &rhs.node else {
            return None;
        };
        if !is_int_lit(one, 1) {
            return None;
        }
        let (base_map, base_key, base_default) = defaulted_map_get(base)?;
        if map_arg.node != base_map.node
            || key_arg.node != base_key.node
            || default_arg.node != base_default.node
        {
            return None;
        }
        Some(MapKeyTrackedIncrementPlan {
            outer_fn: fn_name.to_string(),
            map_arg: map_arg.clone(),
            key_arg: key_arg.clone(),
        })
    };
    side(&law.lhs, &law.rhs).or_else(|| side(&law.rhs, &law.lhs))
}

/// Validate `outer(m, k)` follows the canonical tracked-counter
/// increment body:
///
/// ```text
/// let v = Map.get m k
/// match v {
///   Some(n) -> Map.set m k (n + 1)
///   None    -> Map.set m k 1
/// }
/// ```
pub(super) fn outer_fn_map_increment_shape(fn_name: &str, inputs: &ProofLowerInputs) -> Option<()> {
    use crate::ast::{BinOp, Expr, Pattern, Stmt};

    let fd = inputs.find_fn_def_by_call_name(fn_name)?;
    if fd.params.len() != 2 {
        return None;
    }
    let map_param = fd.params[0].0.as_str();
    let key_param = fd.params[1].0.as_str();
    let stmts = fd.body.stmts();
    if stmts.len() != 2 {
        return None;
    }
    let Stmt::Binding(current, _, bound_expr) = &stmts[0] else {
        return None;
    };
    if !is_map_get_of_params(bound_expr, map_param, key_param) {
        return None;
    }
    let Stmt::Expr(last_expr) = &stmts[1] else {
        return None;
    };
    let Expr::Match { subject, arms, .. } = &last_expr.node else {
        return None;
    };
    if !matches_ident_expr(subject, current) || arms.len() != 2 {
        return None;
    }

    let some_arm = arms.iter().find_map(|arm| match &arm.pattern {
        Pattern::Constructor(name, vars) if name == "Option.Some" && vars.len() == 1 => {
            Some((vars[0].as_str(), arm.body.as_ref()))
        }
        _ => None,
    })?;
    let none_arm = arms.iter().find_map(|arm| match &arm.pattern {
        Pattern::Constructor(name, vars) if name == "Option.None" && vars.is_empty() => {
            Some(arm.body.as_ref())
        }
        _ => None,
    })?;

    let (some_bound, some_body) = some_arm;
    let some_set = call_named_args(some_body, "Map.set")?;
    let none_set = call_named_args(none_arm, "Map.set")?;
    if some_set.len() != 3 || none_set.len() != 3 {
        return None;
    }
    if !matches_ident_expr(&some_set[0], map_param)
        || !matches_ident_expr(&some_set[1], key_param)
        || !matches_ident_expr(&none_set[0], map_param)
        || !matches_ident_expr(&none_set[1], key_param)
    {
        return None;
    }
    let Expr::BinOp(BinOp::Add, add_left, add_right) = &some_set[2].node else {
        return None;
    };
    if !matches_ident_expr(add_left, some_bound) || !is_int_lit(add_right, 1) {
        return None;
    }
    if !is_int_lit(&none_set[2], 1) {
        return None;
    }
    Some(())
}

/// `Option.withDefault(Map.get(outer(m, k), k), d)` — extract (m, k,
/// d) when the outer call matches `fn_name` and the lookup uses the
/// same key as the outer call's key arg.
pub(super) fn defaulted_map_get_after_fn_call<'a>(
    expr: &'a Spanned<crate::ast::Expr>,
    fn_name: &str,
) -> Option<(
    &'a Spanned<crate::ast::Expr>,
    &'a Spanned<crate::ast::Expr>,
    &'a Spanned<crate::ast::Expr>,
)> {
    let (inner, default) = option_with_default_args(expr)?;
    let (map_arg, key_arg) = map_get_after_fn_call(inner, fn_name)?;
    Some((map_arg, key_arg, default))
}

/// `Option.withDefault(Map.get(m, k), d)` — extract (m, k, d) for the
/// bare lookup form (no surrounding outer call).
pub(super) fn defaulted_map_get(
    expr: &Spanned<crate::ast::Expr>,
) -> Option<(
    &Spanned<crate::ast::Expr>,
    &Spanned<crate::ast::Expr>,
    &Spanned<crate::ast::Expr>,
)> {
    let (inner, default) = option_with_default_args(expr)?;
    let get_args = call_named_args(inner, "Map.get")?;
    if get_args.len() != 2 {
        return None;
    }
    Some((&get_args[0], &get_args[1], default))
}

pub(super) fn option_with_default_args(
    expr: &Spanned<crate::ast::Expr>,
) -> Option<(&Spanned<crate::ast::Expr>, &Spanned<crate::ast::Expr>)> {
    let args = call_named_args(expr, "Option.withDefault")?;
    (args.len() == 2).then_some((&args[0], &args[1]))
}

pub(super) fn is_int_lit(expr: &Spanned<crate::ast::Expr>, n: i64) -> bool {
    use crate::ast::{Expr, Literal};
    matches!(&expr.node, Expr::Literal(Literal::Int(m)) if *m == n)
}

/// `Map.has(outer(m, k), k)` — pick out the (m, k) from the inner
/// outer-fn call when the two key positions agree. Returns `None`
/// when the outer call doesn't match `fn_name` or shape is off.
pub(super) fn map_has_after_fn_call<'a>(
    expr: &'a Spanned<crate::ast::Expr>,
    fn_name: &str,
) -> Option<(&'a Spanned<crate::ast::Expr>, &'a Spanned<crate::ast::Expr>)> {
    use crate::ast::Expr;
    let has_args = call_named_args(expr, "Map.has")?;
    if has_args.len() != 2 {
        return None;
    }
    let Expr::FnCall(callee, fn_args) = &has_args[0].node else {
        return None;
    };
    if fn_args.len() != 2
        || !callee_matches_name(callee, fn_name)
        || fn_args[1].node != has_args[1].node
    {
        return None;
    }
    Some((&fn_args[0], &fn_args[1]))
}

/// `Map.get(outer(m, k), k)` — pick out the (m, k) from the inner
/// outer-fn call when the two key positions agree.
pub(super) fn map_get_after_fn_call<'a>(
    expr: &'a Spanned<crate::ast::Expr>,
    fn_name: &str,
) -> Option<(&'a Spanned<crate::ast::Expr>, &'a Spanned<crate::ast::Expr>)> {
    use crate::ast::Expr;
    let get_args = call_named_args(expr, "Map.get")?;
    if get_args.len() != 2 {
        return None;
    }
    let Expr::FnCall(callee, fn_args) = &get_args[0].node else {
        return None;
    };
    if fn_args.len() != 2
        || !callee_matches_name(callee, fn_name)
        || fn_args[1].node != get_args[1].node
    {
        return None;
    }
    Some((&fn_args[0], &fn_args[1]))
}

pub(super) fn map_has_set_parts(
    expr: &Spanned<crate::ast::Expr>,
) -> Option<(
    &Spanned<crate::ast::Expr>,
    &Spanned<crate::ast::Expr>,
    &Spanned<crate::ast::Expr>,
)> {
    let has_args = call_named_args(expr, "Map.has")?;
    if has_args.len() != 2 {
        return None;
    }
    let set_args = call_named_args(&has_args[0], "Map.set")?;
    if set_args.len() != 3 {
        return None;
    }
    if set_args[1].node != has_args[1].node {
        return None;
    }
    Some((&set_args[0], &set_args[1], &set_args[2]))
}

pub(super) fn map_get_set_parts(
    expr: &Spanned<crate::ast::Expr>,
) -> Option<(
    &Spanned<crate::ast::Expr>,
    &Spanned<crate::ast::Expr>,
    &Spanned<crate::ast::Expr>,
)> {
    let get_args = call_named_args(expr, "Map.get")?;
    if get_args.len() != 2 {
        return None;
    }
    let set_args = call_named_args(&get_args[0], "Map.set")?;
    if set_args.len() != 3 {
        return None;
    }
    if set_args[1].node != get_args[1].node {
        return None;
    }
    Some((&set_args[0], &set_args[1], &set_args[2]))
}

pub(super) fn option_some_arg(
    expr: &Spanned<crate::ast::Expr>,
) -> Option<&Spanned<crate::ast::Expr>> {
    let args = call_named_args(expr, "Option.Some")?;
    (args.len() == 1).then_some(&args[0])
}

/// **syntax-discovery-only** (epic #170 Phase 7). Recognises a
/// fn-call expression whose callee's dotted source name matches
/// `full_name` exactly. Used by `Map.set` / `Map.get` axiom shape
/// detection where `full_name` is a builtin namespace path
/// (`"Map.get"` etc.) — global identity, no per-scope leak.
pub(super) fn call_named_args<'a>(
    expr: &'a Spanned<crate::ast::Expr>,
    full_name: &str,
) -> Option<&'a [Spanned<crate::ast::Expr>]> {
    use crate::ast::Expr;
    let Expr::FnCall(callee, args) = &expr.node else {
        return None;
    };
    let callee_name = expr_to_dotted_name(&callee.node)?;
    if callee_name == full_name {
        Some(args.as_slice())
    } else {
        None
    }
}

pub(super) fn is_bool_true(expr: &Spanned<crate::ast::Expr>) -> bool {
    use crate::ast::{Expr, Literal};
    matches!(&expr.node, Expr::Literal(Literal::Bool(true)))
}

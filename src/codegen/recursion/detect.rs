//! Proof-mode recursion classifier.
//!
//! Backend-neutral pattern matching over the Aver AST that decides,
//! for each recursive pure fn (or mutual-recursion SCC), which
//! [`RecursionPlan`] variant applies — or emits a [`ProofModeIssue`]
//! when the shape falls outside the supported set.
//!
//! Lean and Dafny consume the same plans through
//! [`crate::codegen::recursion::analyze_plans`]; a couple of helpers
//! that depend on AST queries tied to Lean's `toplevel` (pure-fn
//! predicate, recursive-type-def predicate, type-def name) still live
//! in `crate::codegen::lean` and are re-used here through
//! `pub(crate)` exports. That could move to a neutral AST helper
//! module in a later pass.
use std::collections::{HashMap, HashSet};

use crate::ast::{
    BinOp, Expr, FnBody, FnDef, MatchArm, Pattern, Spanned, Stmt, TailCallData, TypeDef,
};
use crate::call_graph;
use crate::codegen::CodegenContext;
use crate::codegen::lean::{
    find_type_def, pure_fns, recursive_pure_fn_names, recursive_type_names,
    sizeof_measure_param_indices,
};

use super::{ProofModeIssue, RecursionPlan};

pub(crate) fn expr_to_dotted_name(expr: &Spanned<Expr>) -> Option<String> {
    match &expr.node {
        Expr::Ident(name) => Some(name.clone()),
        Expr::Attr(obj, field) => expr_to_dotted_name(obj).map(|p| format!("{}.{}", p, field)),
        _ => None,
    }
}

/// Local-variable name regardless of whether the expression has been
/// resolved. Resolver rewrites `Expr::Ident(name)` for locals into
/// `Expr::Resolved { name, .. }` (slot-numbered for the VM); pre-resolution
/// passes still see `Expr::Ident`. Recursion-shape detectors run AFTER
/// resolution at codegen time, so they must accept both forms — otherwise
/// every list/string-recursive function silently falls outside the proof
/// subset.
pub(crate) fn local_name_of(expr: &Spanned<Expr>) -> Option<&str> {
    match &expr.node {
        Expr::Ident(name) => Some(name.as_str()),
        Expr::Resolved { name, .. } => Some(name.as_str()),
        _ => None,
    }
}

pub(crate) fn call_matches(name: &str, target: &str) -> bool {
    name == target || name.rsplit('.').next() == Some(target)
}

pub(crate) fn call_is_in_set(name: &str, targets: &HashSet<String>) -> bool {
    call_matches_any(name, targets)
}

pub(crate) fn canonical_callee_name(name: &str, targets: &HashSet<String>) -> Option<String> {
    if targets.contains(name) {
        return Some(name.to_string());
    }
    name.rsplit('.')
        .next()
        .filter(|last| targets.contains(*last))
        .map(ToString::to_string)
}

pub(crate) fn call_matches_any(name: &str, targets: &HashSet<String>) -> bool {
    if targets.contains(name) {
        return true;
    }
    match name.rsplit('.').next() {
        Some(last) => targets.contains(last),
        None => false,
    }
}

pub(crate) fn is_int_minus_positive(expr: &Spanned<Expr>, param_name: &str) -> bool {
    match &expr.node {
        Expr::BinOp(BinOp::Sub, left, right) => {
            local_name_of(left).is_some_and(|id| id == param_name)
                && matches!(&right.node, Expr::Literal(crate::ast::Literal::Int(n)) if *n >= 1)
        }
        Expr::FnCall(callee, args) => {
            let Some(name) = expr_to_dotted_name(callee) else {
                return false;
            };
            (name == "Int.sub" || name == "int.sub")
                && args.len() == 2
                && local_name_of(&args[0]).is_some_and(|id| id == param_name)
                && matches!(&args[1].node, Expr::Literal(crate::ast::Literal::Int(n)) if *n >= 1)
        }
        _ => false,
    }
}

pub(crate) fn collect_calls_from_expr<'a>(
    expr: &'a Spanned<Expr>,
    out: &mut Vec<(String, Vec<&'a Spanned<Expr>>)>,
) {
    match &expr.node {
        Expr::FnCall(callee, args) => {
            if let Some(name) = expr_to_dotted_name(callee) {
                out.push((name, args.iter().collect()));
            }
            collect_calls_from_expr(callee, out);
            for arg in args {
                collect_calls_from_expr(arg, out);
            }
        }
        Expr::TailCall(boxed) => {
            let TailCallData {
                target: name, args, ..
            } = boxed.as_ref();
            out.push((name.clone(), args.iter().collect()));
            for arg in args {
                collect_calls_from_expr(arg, out);
            }
        }
        Expr::Attr(obj, _) => collect_calls_from_expr(obj, out),
        Expr::BinOp(_, left, right) => {
            collect_calls_from_expr(left, out);
            collect_calls_from_expr(right, out);
        }
        Expr::Neg(inner) => collect_calls_from_expr(inner, out),
        Expr::Match { subject, arms, .. } => {
            collect_calls_from_expr(subject, out);
            for arm in arms {
                collect_calls_from_expr(&arm.body, out);
            }
        }
        Expr::Constructor(_, inner) => {
            if let Some(inner) = inner {
                collect_calls_from_expr(inner, out);
            }
        }
        Expr::ErrorProp(inner) => collect_calls_from_expr(inner, out),
        Expr::InterpolatedStr(parts) => {
            for p in parts {
                if let crate::ast::StrPart::Parsed(e) = p {
                    collect_calls_from_expr(e, out);
                }
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for item in items {
                collect_calls_from_expr(item, out);
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries {
                collect_calls_from_expr(k, out);
                collect_calls_from_expr(v, out);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, v) in fields {
                collect_calls_from_expr(v, out);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_calls_from_expr(base, out);
            for (_, v) in updates {
                collect_calls_from_expr(v, out);
            }
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } => {}
    }
}

pub(crate) fn collect_calls_from_body(body: &FnBody) -> Vec<(String, Vec<&Spanned<Expr>>)> {
    let mut out = Vec::new();
    for stmt in body.stmts() {
        match stmt {
            Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => collect_calls_from_expr(expr, &mut out),
        }
    }
    out
}

pub(crate) fn collect_list_tail_binders_from_expr(
    expr: &Spanned<Expr>,
    list_param_name: &str,
    tails: &mut HashSet<String>,
) {
    match &expr.node {
        Expr::Match { subject, arms, .. } => {
            if local_name_of(subject).is_some_and(|id| id == list_param_name) {
                for MatchArm { pattern, .. } in arms {
                    if let Pattern::Cons(_, tail) = pattern {
                        tails.insert(tail.clone());
                    }
                }
            }
            for arm in arms {
                collect_list_tail_binders_from_expr(&arm.body, list_param_name, tails);
            }
            collect_list_tail_binders_from_expr(subject, list_param_name, tails);
        }
        Expr::FnCall(callee, args) => {
            collect_list_tail_binders_from_expr(callee, list_param_name, tails);
            for arg in args {
                collect_list_tail_binders_from_expr(arg, list_param_name, tails);
            }
        }
        Expr::TailCall(boxed) => {
            let TailCallData {
                target: _, args, ..
            } = boxed.as_ref();
            for arg in args {
                collect_list_tail_binders_from_expr(arg, list_param_name, tails);
            }
        }
        Expr::Attr(obj, _) => collect_list_tail_binders_from_expr(obj, list_param_name, tails),
        Expr::BinOp(_, left, right) => {
            collect_list_tail_binders_from_expr(left, list_param_name, tails);
            collect_list_tail_binders_from_expr(right, list_param_name, tails);
        }
        Expr::Neg(inner) => collect_list_tail_binders_from_expr(inner, list_param_name, tails),
        Expr::Constructor(_, inner) => {
            if let Some(inner) = inner {
                collect_list_tail_binders_from_expr(inner, list_param_name, tails);
            }
        }
        Expr::ErrorProp(inner) => {
            collect_list_tail_binders_from_expr(inner, list_param_name, tails)
        }
        Expr::InterpolatedStr(parts) => {
            for p in parts {
                if let crate::ast::StrPart::Parsed(e) = p {
                    collect_list_tail_binders_from_expr(e, list_param_name, tails);
                }
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for item in items {
                collect_list_tail_binders_from_expr(item, list_param_name, tails);
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries {
                collect_list_tail_binders_from_expr(k, list_param_name, tails);
                collect_list_tail_binders_from_expr(v, list_param_name, tails);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, v) in fields {
                collect_list_tail_binders_from_expr(v, list_param_name, tails);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_list_tail_binders_from_expr(base, list_param_name, tails);
            for (_, v) in updates {
                collect_list_tail_binders_from_expr(v, list_param_name, tails);
            }
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } => {}
    }
}

pub(crate) fn collect_list_tail_binders(fd: &FnDef, list_param_name: &str) -> HashSet<String> {
    let mut tails = HashSet::new();
    for stmt in fd.body.stmts() {
        match stmt {
            Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => {
                collect_list_tail_binders_from_expr(expr, list_param_name, &mut tails)
            }
        }
    }
    tails
}

pub(crate) fn recursive_constructor_binders(
    td: &TypeDef,
    variant_name: &str,
    binders: &[String],
) -> Vec<String> {
    let variant_short = variant_name.rsplit('.').next().unwrap_or(variant_name);
    match td {
        TypeDef::Sum { name, variants, .. } => variants
            .iter()
            .find(|variant| variant.name == variant_short)
            .map(|variant| {
                variant
                    .fields
                    .iter()
                    .zip(binders.iter())
                    .filter_map(|(field_ty, binder)| {
                        (field_ty.trim() == name).then_some(binder.clone())
                    })
                    .collect()
            })
            .unwrap_or_default(),
        TypeDef::Product { .. } => Vec::new(),
    }
}

pub(crate) fn grow_recursive_subterm_binders_from_expr(
    expr: &Spanned<Expr>,
    tracked: &HashSet<String>,
    td: &TypeDef,
    out: &mut HashSet<String>,
) {
    match &expr.node {
        Expr::Match { subject, arms, .. } => {
            if let Expr::Ident(subject_name) = &subject.node
                && tracked.contains(subject_name)
            {
                for arm in arms {
                    if let Pattern::Constructor(variant_name, binders) = &arm.pattern {
                        out.extend(recursive_constructor_binders(td, variant_name, binders));
                    }
                }
            }
            grow_recursive_subterm_binders_from_expr(subject, tracked, td, out);
            for arm in arms {
                grow_recursive_subterm_binders_from_expr(&arm.body, tracked, td, out);
            }
        }
        Expr::FnCall(callee, args) => {
            grow_recursive_subterm_binders_from_expr(callee, tracked, td, out);
            for arg in args {
                grow_recursive_subterm_binders_from_expr(arg, tracked, td, out);
            }
        }
        Expr::Attr(obj, _) => grow_recursive_subterm_binders_from_expr(obj, tracked, td, out),
        Expr::BinOp(_, left, right) => {
            grow_recursive_subterm_binders_from_expr(left, tracked, td, out);
            grow_recursive_subterm_binders_from_expr(right, tracked, td, out);
        }
        Expr::Neg(inner) => grow_recursive_subterm_binders_from_expr(inner, tracked, td, out),
        Expr::Constructor(_, Some(inner)) | Expr::ErrorProp(inner) => {
            grow_recursive_subterm_binders_from_expr(inner, tracked, td, out)
        }
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let crate::ast::StrPart::Parsed(inner) = part {
                    grow_recursive_subterm_binders_from_expr(inner, tracked, td, out);
                }
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for item in items {
                grow_recursive_subterm_binders_from_expr(item, tracked, td, out);
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries {
                grow_recursive_subterm_binders_from_expr(k, tracked, td, out);
                grow_recursive_subterm_binders_from_expr(v, tracked, td, out);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, v) in fields {
                grow_recursive_subterm_binders_from_expr(v, tracked, td, out);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            grow_recursive_subterm_binders_from_expr(base, tracked, td, out);
            for (_, v) in updates {
                grow_recursive_subterm_binders_from_expr(v, tracked, td, out);
            }
        }
        Expr::TailCall(boxed) => {
            for arg in &boxed.args {
                grow_recursive_subterm_binders_from_expr(arg, tracked, td, out);
            }
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } | Expr::Constructor(_, None) => {}
    }
}

pub(crate) fn collect_recursive_subterm_binders(
    fd: &FnDef,
    param_name: &str,
    param_type: &str,
    ctx: &CodegenContext,
) -> HashSet<String> {
    let Some(td) = find_type_def(ctx, param_type) else {
        return HashSet::new();
    };
    let mut tracked: HashSet<String> = HashSet::from([param_name.to_string()]);
    loop {
        let mut discovered = HashSet::new();
        for stmt in fd.body.stmts() {
            match stmt {
                Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => {
                    grow_recursive_subterm_binders_from_expr(expr, &tracked, td, &mut discovered);
                }
            }
        }
        let before = tracked.len();
        tracked.extend(discovered);
        if tracked.len() == before {
            break;
        }
    }
    tracked.remove(param_name);
    tracked
}

pub(crate) fn single_int_countdown_param_index(fd: &FnDef) -> Option<usize> {
    let recursive_calls: Vec<Vec<&Spanned<Expr>>> = collect_calls_from_body(fd.body.as_ref())
        .into_iter()
        .filter(|(name, _)| call_matches(name, &fd.name))
        .map(|(_, args)| args)
        .collect();
    if recursive_calls.is_empty() {
        return None;
    }

    fd.params
        .iter()
        .enumerate()
        .find_map(|(idx, (param_name, param_ty))| {
            if param_ty != "Int" {
                return None;
            }
            let countdown_ok = recursive_calls.iter().all(|args| {
                args.get(idx)
                    .cloned()
                    .is_some_and(|arg| is_int_minus_positive(arg, param_name))
            });
            if countdown_ok {
                return Some(idx);
            }

            // Negative-guarded ascent (match n < 0) is handled as countdown
            // because the fuel is natAbs(n) which works for both directions.
            let ascent_ok = recursive_calls.iter().all(|args| {
                args.get(idx)
                    .copied()
                    .is_some_and(|arg| is_int_plus_positive(arg, param_name))
            });
            (ascent_ok && has_negative_guarded_ascent(fd, param_name)).then_some(idx)
        })
}

pub(crate) fn has_negative_guarded_ascent(fd: &FnDef, param_name: &str) -> bool {
    let Some(tail) = fd.body.tail_expr() else {
        return false;
    };
    let Expr::Match { subject, arms, .. } = &tail.node else {
        return false;
    };
    let Expr::BinOp(BinOp::Lt, left, right) = &subject.node else {
        return false;
    };
    if !is_ident(left, param_name)
        || !matches!(&right.node, Expr::Literal(crate::ast::Literal::Int(0)))
    {
        return false;
    }

    let mut true_arm = None;
    let mut false_arm = None;
    for arm in arms {
        match arm.pattern {
            Pattern::Literal(crate::ast::Literal::Bool(true)) => true_arm = Some(arm.body.as_ref()),
            Pattern::Literal(crate::ast::Literal::Bool(false)) => {
                false_arm = Some(arm.body.as_ref())
            }
            _ => return false,
        }
    }

    let Some(true_arm) = true_arm else {
        return false;
    };
    let Some(false_arm) = false_arm else {
        return false;
    };

    let mut true_calls = Vec::new();
    collect_calls_from_expr(true_arm, &mut true_calls);
    let mut false_calls = Vec::new();
    collect_calls_from_expr(false_arm, &mut false_calls);

    true_calls
        .iter()
        .any(|(name, _)| call_matches(name, &fd.name))
        && false_calls
            .iter()
            .all(|(name, _)| !call_matches(name, &fd.name))
}

/// Detect ascending-index recursion and extract the bound expression
/// as an Aver AST (`Spanned<Expr>`). Returns `(param_index, bound)`.
pub(crate) fn single_int_ascending_param(fd: &FnDef) -> Option<(usize, Spanned<Expr>)> {
    let recursive_calls: Vec<Vec<&Spanned<Expr>>> = collect_calls_from_body(fd.body.as_ref())
        .into_iter()
        .filter(|(name, _)| call_matches(name, &fd.name))
        .map(|(_, args)| args)
        .collect();
    if recursive_calls.is_empty() {
        return None;
    }

    for (idx, (param_name, param_ty)) in fd.params.iter().enumerate() {
        if param_ty != "Int" {
            continue;
        }
        let ascent_ok = recursive_calls.iter().all(|args| {
            args.get(idx)
                .cloned()
                .is_some_and(|arg| is_int_plus_positive(arg, param_name))
        });
        if !ascent_ok {
            continue;
        }
        if let Some(bound) = extract_equality_bound_expr(fd, param_name) {
            return Some((idx, bound));
        }
    }
    None
}

/// Extract the bound expression from `match param == BOUND` as an
/// Aver AST node. Each backend renders this into its own idiom (Lean
/// via `bound_expr_to_lean`, Dafny via its own `emit_expr` path).
pub(crate) fn extract_equality_bound_expr(fd: &FnDef, param_name: &str) -> Option<Spanned<Expr>> {
    let tail = fd.body.tail_expr()?;
    let Expr::Match { subject, arms, .. } = &tail.node else {
        return None;
    };
    let Expr::BinOp(BinOp::Eq, left, right) = &subject.node else {
        return None;
    };
    if !is_ident(left, param_name) {
        return None;
    }
    // Verify: true arm = base (no self-call), false arm = recursive (has self-call)
    let mut true_has_self = false;
    let mut false_has_self = false;
    for arm in arms {
        match arm.pattern {
            Pattern::Literal(crate::ast::Literal::Bool(true)) => {
                let mut calls = Vec::new();
                collect_calls_from_expr(&arm.body, &mut calls);
                true_has_self = calls.iter().any(|(n, _)| call_matches(n, &fd.name));
            }
            Pattern::Literal(crate::ast::Literal::Bool(false)) => {
                let mut calls = Vec::new();
                collect_calls_from_expr(&arm.body, &mut calls);
                false_has_self = calls.iter().any(|(n, _)| call_matches(n, &fd.name));
            }
            _ => return None,
        }
    }
    if true_has_self || !false_has_self {
        return None;
    }
    Some((**right).clone())
}

pub(crate) fn supports_single_sizeof_structural(fd: &FnDef, ctx: &CodegenContext) -> bool {
    let recursive_calls: Vec<Vec<&Spanned<Expr>>> = collect_calls_from_body(fd.body.as_ref())
        .into_iter()
        .filter(|(name, _)| call_matches(name, &fd.name))
        .map(|(_, args)| args)
        .collect();
    if recursive_calls.is_empty() {
        return false;
    }

    let metric_indices = sizeof_measure_param_indices(fd);
    if metric_indices.is_empty() {
        return false;
    }

    let binder_sets: HashMap<usize, HashSet<String>> = metric_indices
        .iter()
        .filter_map(|idx| {
            let (param_name, param_type) = fd.params.get(*idx)?;
            recursive_type_names(ctx).contains(param_type).then(|| {
                (
                    *idx,
                    collect_recursive_subterm_binders(fd, param_name, param_type, ctx),
                )
            })
        })
        .collect();

    if binder_sets.values().all(HashSet::is_empty) {
        return false;
    }

    recursive_calls.iter().all(|args| {
        let mut strictly_smaller = false;
        for idx in &metric_indices {
            let Some((param_name, _)) = fd.params.get(*idx) else {
                return false;
            };
            let Some(arg) = args.get(*idx).cloned() else {
                return false;
            };
            if is_ident(arg, param_name) {
                continue;
            }
            let Some(binders) = binder_sets.get(idx) else {
                return false;
            };
            if local_name_of(arg).is_some_and(|id| binders.contains(id)) {
                strictly_smaller = true;
                continue;
            }
            return false;
        }
        strictly_smaller
    })
}

pub(crate) fn single_list_structural_param_index(fd: &FnDef) -> Option<usize> {
    fd.params
        .iter()
        .enumerate()
        .find_map(|(param_index, (param_name, param_ty))| {
            if !(param_ty.starts_with("List<") || param_ty == "List") {
                return None;
            }

            let tails = collect_list_tail_binders(fd, param_name);
            if tails.is_empty() {
                return None;
            }

            let recursive_calls: Vec<Option<&Spanned<Expr>>> =
                collect_calls_from_body(fd.body.as_ref())
                    .into_iter()
                    .filter(|(name, _)| call_matches(name, &fd.name))
                    .map(|(_, args)| args.get(param_index).cloned())
                    .collect();
            if recursive_calls.is_empty() {
                return None;
            }

            recursive_calls
                .into_iter()
                .all(|arg| {
                    arg.and_then(local_name_of)
                        .is_some_and(|id| tails.contains(id))
                })
                .then_some(param_index)
        })
}

pub(crate) fn is_ident(expr: &Spanned<Expr>, name: &str) -> bool {
    local_name_of(expr).is_some_and(|id| id == name)
}

pub(crate) fn is_int_plus_positive(expr: &Spanned<Expr>, param_name: &str) -> bool {
    match &expr.node {
        Expr::BinOp(BinOp::Add, left, right) => {
            local_name_of(left).is_some_and(|id| id == param_name)
                && matches!(&right.node, Expr::Literal(crate::ast::Literal::Int(n)) if *n >= 1)
        }
        Expr::FnCall(callee, args) => {
            let Some(name) = expr_to_dotted_name(callee) else {
                return false;
            };
            (name == "Int.add" || name == "int.add")
                && args.len() == 2
                && local_name_of(&args[0]).is_some_and(|id| id == param_name)
                && matches!(&args[1].node, Expr::Literal(crate::ast::Literal::Int(n)) if *n >= 1)
        }
        _ => false,
    }
}

pub(crate) fn is_skip_ws_advance(
    expr: &Spanned<Expr>,
    string_param: &str,
    pos_param: &str,
) -> bool {
    let Expr::FnCall(callee, args) = &expr.node else {
        return false;
    };
    let Some(name) = expr_to_dotted_name(callee) else {
        return false;
    };
    if !call_matches(&name, "skipWs") || args.len() != 2 {
        return false;
    }
    is_ident(&args[0], string_param) && is_int_plus_positive(&args[1], pos_param)
}

pub(crate) fn is_skip_ws_same(expr: &Spanned<Expr>, string_param: &str, pos_param: &str) -> bool {
    let Expr::FnCall(callee, args) = &expr.node else {
        return false;
    };
    let Some(name) = expr_to_dotted_name(callee) else {
        return false;
    };
    if !call_matches(&name, "skipWs") || args.len() != 2 {
        return false;
    }
    is_ident(&args[0], string_param) && is_ident(&args[1], pos_param)
}

pub(crate) fn is_string_pos_advance(
    expr: &Spanned<Expr>,
    string_param: &str,
    pos_param: &str,
) -> bool {
    is_int_plus_positive(expr, pos_param) || is_skip_ws_advance(expr, string_param, pos_param)
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum StringPosEdge {
    Same,
    Advance,
}

pub(crate) fn classify_string_pos_edge(
    expr: &Spanned<Expr>,
    string_param: &str,
    pos_param: &str,
) -> Option<StringPosEdge> {
    if is_ident(expr, pos_param) || is_skip_ws_same(expr, string_param, pos_param) {
        return Some(StringPosEdge::Same);
    }
    if is_string_pos_advance(expr, string_param, pos_param) {
        return Some(StringPosEdge::Advance);
    }
    if let Expr::FnCall(callee, args) = &expr.node {
        let name = expr_to_dotted_name(callee)?;
        if call_matches(&name, "skipWs")
            && args.len() == 2
            && is_ident(&args[0], string_param)
            && local_name_of(&args[1]).is_some_and(|id| id != pos_param)
        {
            return Some(StringPosEdge::Advance);
        }
    }
    if local_name_of(expr).is_some_and(|id| id != pos_param) {
        return Some(StringPosEdge::Advance);
    }
    None
}

pub(crate) fn ranks_from_same_edges(
    names: &HashSet<String>,
    same_edges: &HashMap<String, HashSet<String>>,
) -> Option<HashMap<String, usize>> {
    let mut indegree: HashMap<String, usize> = names.iter().map(|n| (n.clone(), 0)).collect();
    for outs in same_edges.values() {
        for to in outs {
            if let Some(entry) = indegree.get_mut(to) {
                *entry += 1;
            } else {
                return None;
            }
        }
    }

    let mut queue: Vec<String> = indegree
        .iter()
        .filter_map(|(name, &deg)| (deg == 0).then_some(name.clone()))
        .collect();
    queue.sort();
    let mut topo = Vec::new();
    while let Some(node) = queue.pop() {
        topo.push(node.clone());
        let outs = same_edges.get(&node).cloned().unwrap_or_default();
        let mut newly_zero = Vec::new();
        for to in outs {
            if let Some(entry) = indegree.get_mut(&to) {
                *entry -= 1;
                if *entry == 0 {
                    newly_zero.push(to);
                }
            } else {
                return None;
            }
        }
        newly_zero.sort();
        queue.extend(newly_zero);
    }

    if topo.len() != names.len() {
        return None;
    }

    let n = topo.len();
    let mut ranks = HashMap::new();
    for (idx, name) in topo.into_iter().enumerate() {
        ranks.insert(name, n - idx);
    }
    Some(ranks)
}

pub(crate) fn supports_single_string_pos_advance(fd: &FnDef) -> bool {
    let Some((string_param, string_ty)) = fd.params.first() else {
        return false;
    };
    let Some((pos_param, pos_ty)) = fd.params.get(1) else {
        return false;
    };
    if string_ty != "String" || pos_ty != "Int" {
        return false;
    }

    type CallPair<'a> = (Option<&'a Spanned<Expr>>, Option<&'a Spanned<Expr>>);
    let recursive_calls: Vec<CallPair<'_>> = collect_calls_from_body(fd.body.as_ref())
        .into_iter()
        .filter(|(name, _)| call_matches(name, &fd.name))
        .map(|(_, args)| (args.first().cloned(), args.get(1).cloned()))
        .collect();
    if recursive_calls.is_empty() {
        return false;
    }

    recursive_calls.into_iter().all(|(arg0, arg1)| {
        arg0.is_some_and(|e| is_ident(e, string_param))
            && arg1.is_some_and(|e| is_string_pos_advance(e, string_param, pos_param))
    })
}

pub(crate) fn supports_mutual_int_countdown(component: &[&FnDef]) -> bool {
    if component.len() < 2 {
        return false;
    }
    if component
        .iter()
        .any(|fd| !matches!(fd.params.first(), Some((_, t)) if t == "Int"))
    {
        return false;
    }
    let names: HashSet<String> = component.iter().map(|fd| fd.name.clone()).collect();
    let mut any_intra = false;
    for fd in component {
        let param_name = &fd.params[0].0;
        for (callee, args) in collect_calls_from_body(fd.body.as_ref()) {
            if !call_is_in_set(&callee, &names) {
                continue;
            }
            any_intra = true;
            let Some(arg0) = args.first().cloned() else {
                return false;
            };
            if !is_int_minus_positive(arg0, param_name) {
                return false;
            }
        }
    }
    any_intra
}

pub(crate) fn supports_mutual_string_pos_advance(
    component: &[&FnDef],
) -> Option<HashMap<String, usize>> {
    if component.len() < 2 {
        return None;
    }
    if component.iter().any(|fd| {
        !matches!(fd.params.first(), Some((_, t)) if t == "String")
            || !matches!(fd.params.get(1), Some((_, t)) if t == "Int")
    }) {
        return None;
    }

    let names: HashSet<String> = component.iter().map(|fd| fd.name.clone()).collect();
    let mut same_edges: HashMap<String, HashSet<String>> =
        names.iter().map(|n| (n.clone(), HashSet::new())).collect();
    let mut any_intra = false;

    for fd in component {
        let string_param = &fd.params[0].0;
        let pos_param = &fd.params[1].0;
        for (callee_raw, args) in collect_calls_from_body(fd.body.as_ref()) {
            let Some(callee) = canonical_callee_name(&callee_raw, &names) else {
                continue;
            };
            any_intra = true;

            let arg0 = args.first().cloned()?;
            let arg1 = args.get(1).cloned()?;

            if !is_ident(arg0, string_param) {
                return None;
            }

            match classify_string_pos_edge(arg1, string_param, pos_param) {
                Some(StringPosEdge::Same) => {
                    if let Some(edges) = same_edges.get_mut(&fd.name) {
                        edges.insert(callee);
                    } else {
                        return None;
                    }
                }
                Some(StringPosEdge::Advance) => {}
                None => return None,
            }
        }
    }

    if !any_intra {
        return None;
    }

    ranks_from_same_edges(&names, &same_edges)
}

pub(crate) fn is_scalar_like_type(type_name: &str) -> bool {
    matches!(
        type_name,
        "Int" | "Float" | "Bool" | "String" | "Char" | "Byte" | "Unit"
    )
}

pub(crate) fn supports_mutual_sizeof_ranked(
    component: &[&FnDef],
) -> Option<HashMap<String, usize>> {
    if component.len() < 2 {
        return None;
    }
    let names: HashSet<String> = component.iter().map(|fd| fd.name.clone()).collect();
    let metric_indices: HashMap<String, Vec<usize>> = component
        .iter()
        .map(|fd| (fd.name.clone(), sizeof_measure_param_indices(fd)))
        .collect();
    if component.iter().any(|fd| {
        metric_indices
            .get(&fd.name)
            .is_none_or(|indices| indices.is_empty())
    }) {
        return None;
    }

    let mut same_edges: HashMap<String, HashSet<String>> =
        names.iter().map(|n| (n.clone(), HashSet::new())).collect();
    let mut any_intra = false;
    for fd in component {
        let caller_metric_indices = metric_indices.get(&fd.name)?;
        let caller_metric_params: Vec<&str> = caller_metric_indices
            .iter()
            .filter_map(|idx| fd.params.get(*idx).map(|(name, _)| name.as_str()))
            .collect();
        for (callee_raw, args) in collect_calls_from_body(fd.body.as_ref()) {
            let Some(callee) = canonical_callee_name(&callee_raw, &names) else {
                continue;
            };
            any_intra = true;
            let callee_metric_indices = metric_indices.get(&callee)?;
            let is_same_edge = callee_metric_indices.len() == caller_metric_params.len()
                && callee_metric_indices
                    .iter()
                    .enumerate()
                    .all(|(pos, callee_idx)| {
                        let Some(arg) = args.get(*callee_idx).cloned() else {
                            return false;
                        };
                        is_ident(arg, caller_metric_params[pos])
                    });
            if is_same_edge {
                if let Some(edges) = same_edges.get_mut(&fd.name) {
                    edges.insert(callee);
                } else {
                    return None;
                }
            }
        }
    }
    if !any_intra {
        return None;
    }

    let ranks = ranks_from_same_edges(&names, &same_edges)?;
    let mut out = HashMap::new();
    for fd in component {
        let rank = ranks.get(&fd.name).cloned()?;
        out.insert(fd.name.clone(), rank);
    }
    Some(out)
}

/// Classify every recursive pure fn in `ctx`. The returned map assigns
/// each supported function a [`RecursionPlan`]; anything that falls
/// outside the recognised shapes becomes a [`ProofModeIssue`].
pub fn analyze_plans(
    ctx: &CodegenContext,
) -> (HashMap<String, RecursionPlan>, Vec<ProofModeIssue>) {
    let mut plans = HashMap::new();
    let mut issues = Vec::new();

    let all_pure = pure_fns(ctx);
    let recursive_names = recursive_pure_fn_names(ctx);
    let components = call_graph::ordered_fn_components(&all_pure, &ctx.module_prefixes);

    for component in components {
        if component.is_empty() {
            continue;
        }
        let is_recursive_component =
            component.len() > 1 || recursive_names.contains(&component[0].name);
        if !is_recursive_component {
            continue;
        }

        if component.len() > 1 {
            if supports_mutual_int_countdown(&component) {
                for fd in &component {
                    plans.insert(fd.name.clone(), RecursionPlan::MutualIntCountdown);
                }
            } else if let Some(ranks) = supports_mutual_string_pos_advance(&component) {
                for fd in &component {
                    if let Some(rank) = ranks.get(&fd.name).cloned() {
                        plans.insert(
                            fd.name.clone(),
                            RecursionPlan::MutualStringPosAdvance { rank },
                        );
                    }
                }
            } else if let Some(rankings) = supports_mutual_sizeof_ranked(&component) {
                for fd in &component {
                    if let Some(rank) = rankings.get(&fd.name).cloned() {
                        plans.insert(fd.name.clone(), RecursionPlan::MutualSizeOfRanked { rank });
                    }
                }
            } else {
                let names = component
                    .iter()
                    .map(|fd| fd.name.clone())
                    .collect::<Vec<_>>()
                    .join(", ");
                let line = component.iter().map(|fd| fd.line).min().unwrap_or(1);
                issues.push(ProofModeIssue {
                    line,
                    message: format!(
                        "unsupported mutual recursion group (currently supported in proof mode: Int countdown on first param): {}",
                        names
                    ),
                });
            }
            continue;
        }

        let fd = component[0];
        if crate::codegen::lean::recurrence::detect_second_order_int_linear_recurrence(fd).is_some()
        {
            plans.insert(fd.name.clone(), RecursionPlan::LinearRecurrence2);
        } else if let Some((param_index, bound)) = single_int_ascending_param(fd) {
            plans.insert(
                fd.name.clone(),
                RecursionPlan::IntAscending { param_index, bound },
            );
        } else if let Some(param_index) = single_int_countdown_param_index(fd) {
            plans.insert(fd.name.clone(), RecursionPlan::IntCountdown { param_index });
        } else if supports_single_sizeof_structural(fd, ctx) {
            plans.insert(fd.name.clone(), RecursionPlan::SizeOfStructural);
        } else if let Some(param_index) = single_list_structural_param_index(fd) {
            plans.insert(
                fd.name.clone(),
                RecursionPlan::ListStructural { param_index },
            );
        } else if supports_single_string_pos_advance(fd) {
            plans.insert(fd.name.clone(), RecursionPlan::StringPosAdvance);
        } else {
            issues.push(ProofModeIssue {
                line: fd.line,
                message: format!(
                    "recursive function '{}' is outside proof subset (currently supported: Int countdown, second-order affine Int recurrences with pair-state worker, structural recursion on List/recursive ADTs, String+position, mutual Int countdown, mutual String+position, and ranked sizeOf recursion)",
                    fd.name
                ),
            });
        }
    }

    (plans, issues)
}

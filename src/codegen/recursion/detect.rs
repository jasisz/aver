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
    BinOp, Expr, FnBody, FnDef, MatchArm, Pattern, Spanned, Stmt, TailCallData, TopLevel, TypeDef,
};
use crate::call_graph;
use crate::codegen::proof_lower::ProofLowerInputs;

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

/// **syntax-discovery-only** (epic #170 Phase 7). Exact-match
/// recognition of a dotted source name. The previous suffix-match
/// clause was an identity leak — sibling fix to the one in
/// `proof_lower::callee_matches_name` /
/// `law_auto::shared::callee_matches_name`.
pub(crate) fn call_matches(name: &str, target: &str) -> bool {
    name == target
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

/// Names a pattern brings into scope. Needed by the tail-binder scope below:
/// inside an arm, a binder the pattern introduces names whatever the pattern
/// matched, so any tracked subterm of the same name is no longer reachable
/// under that name there.
fn pattern_bound_names(pattern: &Pattern) -> Vec<&str> {
    match pattern {
        Pattern::Wildcard | Pattern::Literal(_) | Pattern::EmptyList => Vec::new(),
        Pattern::Ident(name) => vec![name.as_str()],
        Pattern::Cons(head, tail) => vec![head.as_str(), tail.as_str()],
        Pattern::Constructor(_, binders) => binders.iter().map(String::as_str).collect(),
        Pattern::Tuple(items) => items.iter().flat_map(pattern_bound_names).collect(),
    }
}

/// Record `name` as a tail reached by peeling `depth` cons cells, keeping the
/// SMALLEST depth when the same binder name is reached along several paths —
/// the guaranteed peel, never an optimistic one.
fn record_tail_depth(out: &mut HashMap<String, usize>, name: &str, depth: usize) {
    out.entry(name.to_string())
        .and_modify(|seen| *seen = (*seen).min(depth))
        .or_insert(depth);
}

/// The tracked map an arm's body sees.
///
/// `tracked` maps the names that stand for a subterm of the list parameter,
/// right here, to their peel depth (the parameter itself sits at `0`). Two
/// edits happen at an arm boundary, in this order:
///
/// - every name the pattern binds LEAVES, because inside the arm that name
///   is whatever the pattern matched, not the subterm it named outside;
/// - if the subject is itself tracked at depth `d` and the pattern is
///   `[head, ..tail]`, `tail` ENTERS at depth `d + 1`.
///
/// A tail of a tail is therefore a tail, one cell deeper — which is how a
/// parser consuming a hex PAIR per step (`match chars { [] -> …; [high,
/// ..afterHigh] -> match afterHigh { [] -> …; [low, ..rest] -> self(rest, …)
/// } }`) is recognised: inside the inner arm, `rest` sits at depth 2.
fn arm_scope(
    tracked: &HashMap<String, usize>,
    pattern: &Pattern,
    subject_depth: Option<usize>,
) -> HashMap<String, usize> {
    let mut scoped = tracked.clone();
    for bound in pattern_bound_names(pattern) {
        scoped.remove(bound);
    }
    if let (Some(depth), Pattern::Cons(_, tail)) = (subject_depth, pattern) {
        scoped.insert(tail.clone(), depth + 1);
    }
    scoped
}

/// Walk `expr` recording every binder that names a cons-tail of the parameter
/// SOMEWHERE below, tagged with its peel depth.
///
/// This is a whole-body UNION over scopes and it is deliberately not exposed:
/// see [`collect_list_tail_binders`] for what such a union can and cannot be
/// asked. Deciding a self-call goes through [`self_call_peel`], which keeps
/// the scope.
fn collect_list_tail_binder_depths_from_expr(
    expr: &Spanned<Expr>,
    tracked: &HashMap<String, usize>,
    out: &mut HashMap<String, usize>,
) {
    if let Expr::Match { subject, arms } = &expr.node {
        collect_list_tail_binder_depths_from_expr(subject, tracked, out);
        let subject_depth = local_name_of(subject).and_then(|id| tracked.get(id).copied());
        for MatchArm { pattern, body, .. } in arms {
            let scoped = arm_scope(tracked, pattern, subject_depth);
            if let (Some(depth), Pattern::Cons(_, tail)) = (subject_depth, pattern) {
                record_tail_depth(out, tail, depth + 1);
            }
            collect_list_tail_binder_depths_from_expr(body, &scoped, out);
        }
        return;
    }
    // Every other shape binds nothing, so the descent is the generic one —
    // and going through `expr_walk` means a new `Expr` variant cannot be
    // silently skipped here.
    crate::codegen::expr_walk::for_each_child(expr, &mut |child| {
        collect_list_tail_binder_depths_from_expr(child, tracked, out)
    });
}

/// [`collect_list_tail_binder_depths_from_expr`] over a whole fn body.
fn collect_list_tail_binder_depths(fd: &FnDef, list_param_name: &str) -> HashMap<String, usize> {
    let mut tracked = HashMap::from([(list_param_name.to_string(), 0usize)]);
    let mut out = HashMap::new();
    for stmt in fd.body.stmts() {
        match stmt {
            Stmt::Binding(name, _, expr) => {
                collect_list_tail_binder_depths_from_expr(expr, &tracked, &mut out);
                // A `let` of the same name shadows the subterm from here on.
                tracked.remove(name);
            }
            Stmt::Expr(expr) => collect_list_tail_binder_depths_from_expr(expr, &tracked, &mut out),
        }
    }
    out
}

/// The DEPTH-1 cons-tails only: binders that, SOMEWHERE in the body, name a
/// list obtained by peeling exactly one cell off the list parameter.
///
/// This is a union over the whole body, so read what it says carefully. It
/// answers "does this name stand for a one-cell tail of the parameter at some
/// point in this fn" — a fact about the body. It does NOT answer "is the
/// argument of THIS call a one-cell tail of the parameter": the same name can
/// be a tail of the parameter in one arm and a tail of an unrelated list, or a
/// pattern binder shadowing it, in another. A caller that needs the per-site
/// answer must keep the scope, the way [`self_call_peel`] does.
///
/// The mutual lex-list synthesiser reads this and nothing else, because its
/// offset propagation is arithmetic on the peel: a strict subterm edge is
/// assumed to hand the callee `len - 1`, and it pays for that with `off + 1`
/// so the lex measure's first component stays EQUAL and the rank tie-break
/// carries the decrease. A depth-2 binder would break that equality, so the
/// deeper tails stay out of this view until that synthesiser learns to
/// propagate the peel amount.
pub(crate) fn collect_list_tail_binders(fd: &FnDef, list_param_name: &str) -> HashSet<String> {
    collect_list_tail_binder_depths(fd, list_param_name)
        .into_iter()
        .filter(|(_, depth)| *depth == 1)
        .map(|(name, _)| name)
        .collect()
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
            if let Some(subject_name) = local_name_of(subject)
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
    inputs: &ProofLowerInputs,
) -> HashSet<String> {
    let Some(td) = inputs.find_type_def(param_type) else {
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

/// Body of the `0` arm of `match <countdown_param> { 0 -> BASE; _ -> rec(...) }`,
/// when the fn body has exactly that two-arm shape and no other recursive
/// branches. Returned to the codegen so the native-emit wrapper can use
/// `BASE` for the `p < 0` case (outside the well-formed Aver domain).
///
/// Conservative: requires the top-level expression to be a `match` whose
/// subject is the countdown param itself (after resolution). Inner /
/// nested matches don't qualify — those are handled by the fuel path
/// where well-foundedness is encoded structurally.
pub(crate) fn int_countdown_native_arms(
    fd: &FnDef,
    param_index: usize,
) -> Option<(i64, Spanned<Expr>, Spanned<Expr>)> {
    let (param_name, _) = fd.params.get(param_index)?;
    // Body must be exactly one expression — leading `let` bindings
    // would need to be threaded into both the wrapper and the aux, which
    // the simple string-level emit can't do. Fuel path stays available
    // for fns with let-prefix bodies.
    if fd.body.stmts().len() != 1 {
        return None;
    }
    let tail = fd.body.tail_expr()?;
    let Expr::Match { subject, arms, .. } = &tail.node else {
        return None;
    };
    if !is_ident(subject, param_name) {
        return None;
    }
    if arms.len() != 2 {
        return None;
    }
    let mut literal_arm: Option<(i64, Spanned<Expr>)> = None;
    let mut wildcard_arm_body: Option<Spanned<Expr>> = None;
    for arm in arms {
        match &arm.pattern {
            Pattern::Literal(crate::ast::Literal::Int(n)) => {
                literal_arm = Some((*n, (*arm.body).clone()));
            }
            Pattern::Wildcard | Pattern::Ident(_) => {
                wildcard_arm_body = Some((*arm.body).clone());
            }
            _ => return None,
        }
    }
    let (literal_value, literal_arm_body) = literal_arm?;
    let wildcard_arm_body = wildcard_arm_body?;

    // Lean native emit assumes the default `(h_dom : p ≥ 0)`
    // precondition; under that, the wildcard arm constraint `p ≠ L`
    // combined with `p ≥ 0` must imply `p ≥ 1` so the recursive call
    // `f(p - 1)` lands in-domain. That holds only when `L == 0`. A
    // shape like `match n { 5 -> base; _ -> f(n - 1) }` would let
    // `n = 0` reach the wildcard arm, recurse with `n - 1 = -1`, and
    // break the precondition — `omega` rightly rejects this at lake
    // build. Generalising to arbitrary literals needs a proper
    // preservation check over (precondition ∧ arm-constraint ⊢
    // f-args-in-domain) and a non-default precondition derived from
    // the caller's guards; until that's in, restrict native emit to
    // the literal-zero shape and fall back to fuel otherwise.
    if literal_value != 0 {
        return None;
    }

    let mut lit_calls = Vec::new();
    collect_calls_from_expr(&literal_arm_body, &mut lit_calls);
    if lit_calls.iter().any(|(n, _)| call_matches(n, &fd.name)) {
        return None;
    }
    let mut wc_calls = Vec::new();
    collect_calls_from_expr(&wildcard_arm_body, &mut wc_calls);
    if !wc_calls.iter().any(|(n, _)| call_matches(n, &fd.name)) {
        return None;
    }
    Some((literal_value, literal_arm_body, wildcard_arm_body))
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

/// **syntax-discovery-only**: recognize a floor-division shrink of
/// `param_name` — bare `Int.div(p, k)` (discharged total form) or the
/// legacy `Result.withDefault(Int.div(p, k), <int literal>)` wrapper,
/// with literal `k >= 2`, either inlined or through a unary wrapper
/// fn whose single-expression body is exactly that shape over its own
/// parameter. Returns `(divisor, helper_fn)`.
pub(crate) fn floor_div_shrink_of(
    expr: &Spanned<Expr>,
    param_name: &str,
    inputs: &ProofLowerInputs,
) -> Option<(i64, Option<String>)> {
    if let Some(divisor) = inline_floor_div_shrink(expr, param_name) {
        return Some((divisor, None));
    }
    // Wrapper form: `h(p)` where `h` is a pure unary Int fn whose
    // body is the inlined shape over its own param. The call must be
    // a bare (same-scope) name so both backends can cite it directly.
    let Expr::FnCall(callee, args) = &expr.node else {
        return None;
    };
    let name = expr_to_dotted_name(callee)?;
    if name.contains('.') || args.len() != 1 || !is_ident(&args[0], param_name) {
        return None;
    }
    let fd = inputs.find_fn_def_by_call_name(&name)?;
    if !fd.effects.is_empty() || fd.name != name {
        return None;
    }
    let [(helper_param, helper_ty)] = fd.params.as_slice() else {
        return None;
    };
    if helper_ty != "Int" || fd.return_type != "Int" {
        return None;
    }
    let [crate::ast::Stmt::Expr(body)] = fd.body.stmts() else {
        return None;
    };
    let divisor = inline_floor_div_shrink(body, helper_param)?;
    Some((divisor, Some(fd.name.clone())))
}

/// The inlined floor-division shrink shape: bare `Int.div(p, k)` with
/// literal `k >= 2` (the discharged total form — a nonzero literal divisor
/// types as plain `Int`), or the legacy `Result.withDefault(Int.div(p, k),
/// <int literal>)` wrapper (kept so pre-discharge shapes in unchecked
/// pipelines stay recognized).
fn inline_floor_div_shrink(expr: &Spanned<Expr>, param_name: &str) -> Option<i64> {
    let div_expr = match &expr.node {
        // Legacy wrapper: peel `Result.withDefault(<div>, <int literal>)`.
        Expr::FnCall(callee, args)
            if expr_to_dotted_name(callee).as_deref() == Some("Result.withDefault")
                && args.len() == 2
                && matches!(&args[1].node, Expr::Literal(crate::ast::Literal::Int(_))) =>
        {
            &args[0]
        }
        _ => expr,
    };
    let Expr::FnCall(div_callee, div_args) = &div_expr.node else {
        return None;
    };
    if expr_to_dotted_name(div_callee).as_deref() != Some("Int.div") || div_args.len() != 2 {
        return None;
    }
    if !is_ident(&div_args[0], param_name) {
        return None;
    }
    match &div_args[1].node {
        Expr::Literal(crate::ast::Literal::Int(k)) if *k >= 2 => Some(*k),
        _ => None,
    }
}

/// Collect, for every self-call of `fd`, the chain of enclosing
/// `match <bool> { true/false -> … }` guards in positive form
/// (false-arm guards flipped via `flip_comparison_binop`;
/// unflippable ones dropped — a SOUND under-approximation, since the
/// caller only ever needs "do the collected guards imply the
/// shrink", and fewer facts can only make it decline).
pub(crate) fn collect_self_call_guard_chains(fd: &FnDef) -> Vec<Vec<Spanned<Expr>>> {
    let mut out = Vec::new();
    for stmt in fd.body.stmts() {
        match stmt {
            Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => {
                walk_self_call_guards(expr, &fd.name, &[], &mut out);
            }
        }
    }
    out
}

fn walk_self_call_guards(
    expr: &Spanned<Expr>,
    fn_name: &str,
    enclosing: &[Spanned<Expr>],
    out: &mut Vec<Vec<Spanned<Expr>>>,
) {
    match &expr.node {
        Expr::FnCall(callee, args) => {
            if let Some(name) = expr_to_dotted_name(callee)
                && call_matches(&name, fn_name)
            {
                out.push(enclosing.to_vec());
            }
            walk_self_call_guards(callee, fn_name, enclosing, out);
            for arg in args {
                walk_self_call_guards(arg, fn_name, enclosing, out);
            }
        }
        Expr::TailCall(boxed) => {
            if boxed.target == fn_name {
                out.push(enclosing.to_vec());
            }
            for arg in &boxed.args {
                walk_self_call_guards(arg, fn_name, enclosing, out);
            }
        }
        Expr::Match { subject, arms } => {
            for arm in arms {
                let mut new_guards: Vec<Spanned<Expr>> = enclosing.to_vec();
                match &arm.pattern {
                    Pattern::Literal(crate::ast::Literal::Bool(true)) => {
                        new_guards.push((**subject).clone());
                    }
                    Pattern::Literal(crate::ast::Literal::Bool(false)) => {
                        if let Some(flipped) =
                            crate::codegen::recursion::flip_comparison_binop(subject)
                        {
                            new_guards.push(flipped);
                        }
                    }
                    _ => {}
                }
                walk_self_call_guards(&arm.body, fn_name, &new_guards, out);
            }
            walk_self_call_guards(subject, fn_name, enclosing, out);
        }
        Expr::BinOp(_, l, r) => {
            walk_self_call_guards(l, fn_name, enclosing, out);
            walk_self_call_guards(r, fn_name, enclosing, out);
        }
        Expr::Attr(inner, _) | Expr::Neg(inner) | Expr::ErrorProp(inner) => {
            walk_self_call_guards(inner, fn_name, enclosing, out);
        }
        Expr::Constructor(_, arg) => {
            if let Some(inner) = arg {
                walk_self_call_guards(inner, fn_name, enclosing, out);
            }
        }
        Expr::InterpolatedStr(parts) => {
            for p in parts {
                if let crate::ast::StrPart::Parsed(inner) = p {
                    walk_self_call_guards(inner, fn_name, enclosing, out);
                }
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for item in items {
                walk_self_call_guards(item, fn_name, enclosing, out);
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries {
                walk_self_call_guards(k, fn_name, enclosing, out);
                walk_self_call_guards(v, fn_name, enclosing, out);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, v) in fields {
                walk_self_call_guards(v, fn_name, enclosing, out);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            walk_self_call_guards(base, fn_name, enclosing, out);
            for (_, v) in updates {
                walk_self_call_guards(v, fn_name, enclosing, out);
            }
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } => {}
    }
}

/// Does the (positive-form) guard chain imply `param >= 1`? Two
/// syntactic certificate shapes, both purely linear:
///
/// - DIRECT: a clause bounding the param below by a positive literal
///   (`p >= L` with `L >= 1`, `p > L` with `L >= 0`, or the flipped
///   literal-first spellings).
/// - SCALED: a clause `p >= c * q` (or `p >= q * c` / `p >= q`) with
///   literal `c >= 1`, where the chain also DIRECTLY bounds `q >= 1`
///   — the binary-exponent shape (`¬(b < 1)` and `¬(a < 2 * b)`
///   together give `a >= 2 * b >= 2`).
///
/// Anything else declines — the caller never guesses a measure.
pub(crate) fn guards_imply_param_ge_one(guards: &[Spanned<Expr>], param_name: &str) -> bool {
    if guards
        .iter()
        .any(|g| guard_bounds_ident_ge_one(g, param_name))
    {
        return true;
    }
    guards.iter().any(|g| {
        let Expr::BinOp(BinOp::Gte, left, right) = &g.node else {
            return false;
        };
        if !is_ident(left, param_name) {
            return false;
        }
        let scaled_var: Option<&str> = match &right.node {
            Expr::Ident(q) => Some(q.as_str()),
            Expr::Resolved { name: q, .. } => Some(q.as_str()),
            Expr::BinOp(BinOp::Mul, l, r) => match (&l.node, &r.node) {
                (Expr::Literal(crate::ast::Literal::Int(c)), _) if *c >= 1 => local_name_of(r),
                (_, Expr::Literal(crate::ast::Literal::Int(c))) if *c >= 1 => local_name_of(l),
                _ => None,
            },
            _ => None,
        };
        scaled_var.is_some_and(|q| guards.iter().any(|h| guard_bounds_ident_ge_one(h, q)))
    })
}

/// One clause of the DIRECT certificate: `name >= L` (L >= 1),
/// `name > L` (L >= 0), `L <= name` (L >= 1), `L < name` (L >= 0).
fn guard_bounds_ident_ge_one(guard: &Spanned<Expr>, name: &str) -> bool {
    let Expr::BinOp(op, left, right) = &guard.node else {
        return false;
    };
    let lit = |e: &Spanned<Expr>| -> Option<i64> {
        match &e.node {
            Expr::Literal(crate::ast::Literal::Int(v)) => Some(*v),
            _ => None,
        }
    };
    match op {
        BinOp::Gte => is_ident(left, name) && lit(right).is_some_and(|l| l >= 1),
        BinOp::Gt => is_ident(left, name) && lit(right).is_some_and(|l| l >= 0),
        BinOp::Lte => is_ident(right, name) && lit(left).is_some_and(|l| l >= 1),
        BinOp::Lt => is_ident(right, name) && lit(left).is_some_and(|l| l >= 0),
        _ => false,
    }
}

/// Classifier for [`RecursionPlan::IntFloorDivCountdown`]: a single
/// `Int` param that EVERY self-call shrinks by the same
/// literal-divisor floor division (see [`floor_div_shrink_of`]),
/// with every self-call site's guard chain implying the param is
/// `>= 1` (see [`guards_imply_param_ge_one`]) so the shrink is a
/// genuine strict decrease. Returns
/// `(param_index, divisor, helper_fn)`.
pub(crate) fn single_int_floor_div_countdown(
    fd: &FnDef,
    inputs: &ProofLowerInputs,
) -> Option<(usize, i64, Option<String>)> {
    let recursive_calls: Vec<Vec<&Spanned<Expr>>> = collect_calls_from_body(fd.body.as_ref())
        .into_iter()
        .filter(|(name, _)| call_matches(name, &fd.name))
        .map(|(_, args)| args)
        .collect();
    if recursive_calls.is_empty() {
        return None;
    }

    let (param_index, divisor, helper_fn) =
        fd.params
            .iter()
            .enumerate()
            .find_map(|(idx, (param_name, param_ty))| {
                if param_ty != "Int" {
                    return None;
                }
                let mut shrink: Option<(i64, Option<String>)> = None;
                for args in &recursive_calls {
                    let arg = args.get(idx).copied()?;
                    let this = floor_div_shrink_of(arg, param_name, inputs)?;
                    match &shrink {
                        None => shrink = Some(this),
                        Some(prev) if *prev == this => {}
                        Some(_) => return None,
                    }
                }
                shrink.map(|(divisor, helper)| (idx, divisor, helper))
            })?;

    // Guard validation: every self-call site must sit under a guard
    // chain that implies the shrinking param is >= 1. The chains come
    // back one per self-call in body-walk order; a missing or
    // too-weak chain declines the whole plan — never guess.
    let (param_name, _) = fd.params.get(param_index)?;
    let chains = collect_self_call_guard_chains(fd);
    if chains.len() != recursive_calls.len() || chains.is_empty() {
        return None;
    }
    if !chains
        .iter()
        .all(|chain| guards_imply_param_ge_one(chain, param_name))
    {
        return None;
    }
    Some((param_index, divisor, helper_fn))
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

pub(crate) fn supports_single_sizeof_structural(fd: &FnDef, inputs: &ProofLowerInputs) -> bool {
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
            inputs.recursive_type_names().contains(param_type).then(|| {
                (
                    *idx,
                    collect_recursive_subterm_binders(fd, param_name, param_type, inputs),
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

/// The user-ADT analog of [`single_list_structural_param_index`]: a single
/// recursive-ADT param such that EVERY recursive self-call passes, at that
/// position, a STRICT recursive-subterm binder of it (the `m` of
/// `match n { S(m) -> rec(m, …) }`). Other params are IGNORED — a THREADED
/// accumulator (`triTR`'s `acc`, fed `plus(n, acc)`) does NOT disqualify,
/// exactly as the list path ignores `qrev`'s threaded `acc`. Lean's equation
/// compiler infers structural recursion on the returned param regardless of
/// what the other args do, so the single shrinking param is a sound termination
/// witness even when a sibling param grows.
///
/// This catches the accumulator-fold inner loop (`triTR` / `factTR`) that the
/// multi-param [`supports_single_sizeof_structural`] rejects: that measure
/// requires EVERY non-scalar param to shrink-or-stay, which the reconstructed
/// accumulator violates. Both are ORed in the classifier, so this only ever ADDS
/// the threaded-accumulator ADT shape — a previously-unclassified fn that fell
/// to `partial def` (and bounded sampling) now emits as a structural `def` and
/// becomes induction-provable.
pub(crate) fn single_adt_structural_param_index(
    fd: &FnDef,
    inputs: &ProofLowerInputs,
) -> Option<usize> {
    let recursive_calls: Vec<Vec<&Spanned<Expr>>> = collect_calls_from_body(fd.body.as_ref())
        .into_iter()
        .filter(|(name, _)| call_matches(name, &fd.name))
        .map(|(_, args)| args)
        .collect();
    if recursive_calls.is_empty() {
        return None;
    }
    let recursive_types = inputs.recursive_type_names();
    fd.params
        .iter()
        .enumerate()
        .find_map(|(param_index, (param_name, param_type))| {
            if !recursive_types.contains(param_type) {
                return None;
            }
            let binders = collect_recursive_subterm_binders(fd, param_name, param_type, inputs);
            if binders.is_empty() {
                return None;
            }
            recursive_calls
                .iter()
                .all(|args| {
                    args.get(param_index)
                        .and_then(|a| local_name_of(a))
                        .is_some_and(|id| binders.contains(id))
                })
                .then_some(param_index)
        })
}

/// The arguments of a self-call of `fn_name`, if `expr` is one.
///
/// Both call shapes count: the TCO transform rewrites in-SCC tail calls into
/// [`Expr::TailCall`] before any backend looks, so a self-recursive fn's calls
/// are often not [`Expr::FnCall`] by then.
fn self_call_args<'a>(expr: &'a Spanned<Expr>, fn_name: &str) -> Option<&'a [Spanned<Expr>]> {
    match &expr.node {
        Expr::FnCall(callee, args) => expr_to_dotted_name(callee)
            .is_some_and(|name| call_matches(&name, fn_name))
            .then(|| args.as_slice()),
        Expr::TailCall(tc) => call_matches(&tc.target, fn_name).then(|| tc.args.as_slice()),
        _ => None,
    }
}

/// How every self-call of `fd` moves the argument at `param_index`, when that
/// param is tracked as a list being peeled.
#[derive(Default)]
struct PeelScan {
    /// Smallest peel over the self-calls seen so far; `None` until the first
    /// one, so "no self-call at all" and "peels nothing" stay distinct.
    peel: Option<usize>,
    /// Some self-call passed, at `param_index`, something that is not a
    /// strict cons-tail of the param IN THE SCOPE THAT CALL SITS IN.
    rejected: bool,
}

impl PeelScan {
    /// The guaranteed peel per step, or `None` if any call site failed or
    /// there were no self-calls to judge.
    fn verdict(&self) -> Option<usize> {
        (!self.rejected).then_some(self.peel).flatten()
    }
}

/// Walk `expr` deciding each self-call's `param_index` argument against the
/// tail-binder scope IN FORCE at that call.
///
/// This is the whole difference between "the body mentions a tail of this
/// param somewhere" and "this call passes one". Deciding against a whole-body
/// union classifies `f(xs, ys)` whose `xs = []` arm recurses on a tail of
/// `ys`, and classifies a call whose argument is a binder SHADOWING the tail
/// rather than the tail — both of them recursions that never terminate.
fn self_call_peel(
    expr: &Spanned<Expr>,
    fn_name: &str,
    param_index: usize,
    tracked: &HashMap<String, usize>,
    scan: &mut PeelScan,
) {
    if let Some(args) = self_call_args(expr, fn_name) {
        // Depth `0` is the parameter itself, passed on unpeeled: tracked, but
        // not a decrease. Only a strict subterm counts.
        match args
            .get(param_index)
            .and_then(local_name_of)
            .and_then(|id| tracked.get(id).copied())
            .filter(|depth| *depth >= 1)
        {
            Some(depth) => {
                scan.peel = Some(scan.peel.map_or(depth, |seen: usize| seen.min(depth)));
            }
            None => scan.rejected = true,
        }
    }

    if let Expr::Match { subject, arms } = &expr.node {
        self_call_peel(subject, fn_name, param_index, tracked, scan);
        let subject_depth = local_name_of(subject).and_then(|id| tracked.get(id).copied());
        for MatchArm { pattern, body, .. } in arms {
            let scoped = arm_scope(tracked, pattern, subject_depth);
            self_call_peel(body, fn_name, param_index, &scoped, scan);
        }
        return;
    }
    // A match is the only shape that changes the scope, so everything else
    // descends generically — through `expr_walk`, which has no wildcard arm,
    // so a new `Expr` variant cannot be silently skipped here.
    crate::codegen::expr_walk::for_each_child(expr, &mut |child| {
        self_call_peel(child, fn_name, param_index, tracked, scan)
    });
}

/// The single `List<_>` param every self-call shrinks, plus how many cons
/// cells the shrink peels off it per step.
///
/// The peel is the SMALLEST depth any self-call passes, i.e. what the fn is
/// guaranteed to consume per step: `1` for the classic `[x, ..rest] ->
/// self(rest, …)` walk, `2` for a hex-pair parser. Every peel `>= 1` is a
/// strict structural decrease, so the peel does not change WHETHER the fn is
/// classified — only what a backend can say about the size of the step.
///
/// Each call site is judged against the tails in scope THERE
/// ([`self_call_peel`]), not against a whole-body union of them, so the
/// answer is per-site and the peel is a minimum over sites.
pub(crate) fn single_list_structural_param(fd: &FnDef) -> Option<(usize, usize)> {
    fd.params
        .iter()
        .enumerate()
        .find_map(|(param_index, (param_name, param_ty))| {
            if !(param_ty.starts_with("List<") || param_ty == "List") {
                return None;
            }

            let mut tracked = HashMap::from([(param_name.to_string(), 0usize)]);
            let mut scan = PeelScan::default();
            for stmt in fd.body.stmts() {
                match stmt {
                    Stmt::Binding(name, _, expr) => {
                        self_call_peel(expr, &fd.name, param_index, &tracked, &mut scan);
                        // A `let` of the same name shadows the param from here on.
                        tracked.remove(name);
                    }
                    Stmt::Expr(expr) => {
                        self_call_peel(expr, &fd.name, param_index, &tracked, &mut scan)
                    }
                }
            }
            scan.verdict().map(|peel| (param_index, peel))
        })
}

pub(crate) fn single_list_structural_param_index(fd: &FnDef) -> Option<usize> {
    single_list_structural_param(fd).map(|(param_index, _)| param_index)
}

/// `true` iff every recursive self-call of `fd` passes, at `param_index`, a
/// bare local that is NOT the param itself — i.e. the param is consumed in the
/// recursion (the `z` of `match p { S(z) -> recurse(z, ...) }`). Used to spot a
/// Peano param a list-recursive fn decrements SYNCHRONOUSLY with the list
/// (`take`/`drop`), which must then be GENERALIZED in the induction or the cons
/// IH lands at the wrong predecessor. Conservative: a non-local arg (`p - 1`)
/// yields `false`, so this fires only on the clean succ-binder shape.
pub(crate) fn param_decremented_in_recursion(fd: &FnDef, param_index: usize) -> bool {
    let calls: Vec<(String, Vec<&Spanned<Expr>>)> = collect_calls_from_body(fd.body.as_ref())
        .into_iter()
        .filter(|(name, _)| call_matches(name, &fd.name))
        .collect();
    if calls.is_empty() {
        return false;
    }
    let Some((pname, _)) = fd.params.get(param_index) else {
        return false;
    };
    calls.iter().all(|(_, args)| {
        args.get(param_index)
            .and_then(|a| local_name_of(a))
            .is_some_and(|id| id != pname)
    })
}

/// `true` iff every recursive self-call passes at `param_index` a RECONSTRUCTED
/// expression (not a bare identifier) — a THREADED accumulator (`qrev`'s `acc`
/// gets `List.concat([h], acc)`). The "not a bare local" requirement is what
/// separates a genuine accumulator from a SYNCHRONOUS second list: `zip(xs,
/// ys)` recurses with `ys`'s tail binder `yt` (a bare local), which is a
/// structural decrement to be inducted normally, NOT a threaded accumulator to
/// generalize-without-cases. Such a param must be GENERALIZED (so the IH reads
/// `∀ acc, P xs acc` and applies at the threaded value) but NOT case-split.
pub(crate) fn param_threaded_in_recursion(fd: &FnDef, param_index: usize) -> bool {
    let calls: Vec<(String, Vec<&Spanned<Expr>>)> = collect_calls_from_body(fd.body.as_ref())
        .into_iter()
        .filter(|(name, _)| call_matches(name, &fd.name))
        .collect();
    if calls.is_empty() {
        return false;
    }
    calls.iter().all(|(_, args)| {
        args.get(param_index)
            .is_some_and(|a| local_name_of(a).is_none())
    })
}

/// `true` iff `fd` is a binary fn that recurses by peeling a constructor off
/// BOTH arguments synchronously — the canonical `max`/`min` shape:
/// `match p0 { base -> …; succ(z) -> match p1 { base -> …; succ(w) -> …rec(z, w)… } }`.
/// Exactly one self-call, reached only from inside both succ arms, passing the
/// two inner succ binders (NOT either original param) at positions 0 and 1.
///
/// This is the recursion shape that a commutativity (`f a b = f b a`) or
/// associativity (`f (f a b) c = f a (f b c)`) proof must induct on with
/// `induction a generalizing <rest> with | zero => cases <rest> … | succ k ih =>
/// cases <rest> …`: inducting on one arg alone leaves the other arg's peel
/// stuck. Conservative — both params must share one type, there must be exactly
/// one self-call, and its two args must be the freshly-bound succ predecessors,
/// so it fires only on the genuine both-args-peeling family and never on a
/// single-arg-recursive fn (whose self-call repeats one original param).
pub(crate) fn recurses_decrementing_both_args(fd: &FnDef) -> bool {
    if fd.params.len() != 2 {
        return false;
    }
    let (p0, t0) = &fd.params[0];
    let (p1, t1) = &fd.params[1];
    if t0 != t1 {
        return false;
    }
    // Exactly one self-call, both args bare locals distinct from the originals.
    let calls: Vec<Vec<&Spanned<Expr>>> = collect_calls_from_body(fd.body.as_ref())
        .into_iter()
        .filter(|(name, _)| call_matches(name, &fd.name))
        .map(|(_, args)| args)
        .collect();
    if calls.len() != 1 {
        return false;
    }
    let args = &calls[0];
    let (Some(a0), Some(a1)) = (
        args.first().and_then(|a| local_name_of(a)),
        args.get(1).and_then(|a| local_name_of(a)),
    ) else {
        return false;
    };
    if a0 == p0 || a0 == p1 || a1 == p0 || a1 == p1 || a0 == a1 {
        return false;
    }
    // Outer match on p0 with a succ arm binding `a0`, whose body is an inner
    // match on p1 with a succ arm binding `a1`. Mirrors `peano_outer_split` but
    // stays local to detect.rs (which has no `CodegenContext`/Peano lookup).
    let Some(tail) = fd.body.tail_expr() else {
        return false;
    };
    let Expr::Match { subject, arms, .. } = &tail.node else {
        return false;
    };
    if local_name_of(subject) != Some(p0.as_str()) {
        return false;
    }
    arms.iter().any(|arm| {
        let Pattern::Constructor(_, binders) = &arm.pattern else {
            return false;
        };
        if binders.len() != 1 || binders[0] != a0 {
            return false;
        }
        let Expr::Match {
            subject: inner_subj,
            arms: inner_arms,
            ..
        } = &arm.body.node
        else {
            return false;
        };
        if local_name_of(inner_subj) != Some(p1.as_str()) {
            return false;
        }
        inner_arms.iter().any(|inner| {
            matches!(&inner.pattern,
                Pattern::Constructor(_, b) if b.len() == 1 && b[0] == a1)
        })
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
            let entry = indegree.get_mut(to)?;
            *entry += 1;
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
            let entry = indegree.get_mut(&to)?;
            *entry -= 1;
            if *entry == 0 {
                newly_zero.push(to);
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
                    let edges = same_edges.get_mut(&fd.name)?;
                    edges.insert(callee);
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
        "Int" | "Float" | "Bool" | "String" | "Char" | "Unit"
    )
}

/// SizeOf-measure param indices for a fn — every non-scalar param
/// position contributes a term to the structural measure. Matches
/// the picks made for the Lean `termination_by` clause and the
/// Dafny native `decreases` tuple so the same params drive measure
/// inference on both backends.
pub fn sizeof_measure_param_indices(fd: &FnDef) -> Vec<usize> {
    fd.params
        .iter()
        .enumerate()
        .filter_map(|(idx, (_, type_name))| (!is_scalar_like_type(type_name)).then_some(idx))
        .collect()
}

/// True iff some intra-SCC call passes a non-trivially-shrinking
/// expression into a callee's sizeOf parameter — tail-recursive
/// accumulator patterns like `[x] + acc` / `List.prepend(x, acc)`
/// / `acc + [x]`, plus arbitrary `FnCall(...)` results we can't
/// statically prove shrink. These all fail termination-by-measure
/// even though fuel encoding handles them fine; backends fall back
/// to fuel for those SCCs.
pub fn scc_has_growing_accumulator(fns: &[&FnDef]) -> bool {
    let names: HashSet<String> = fns.iter().map(|fd| fd.name.clone()).collect();
    let mut sizeof_indices: HashMap<String, Vec<usize>> = HashMap::new();
    for fd in fns {
        sizeof_indices.insert(fd.name.clone(), sizeof_measure_param_indices(fd));
    }
    for fd in fns {
        // String interpolation in a body that ends up calling an
        // intra-SCC fn confuses Lean's wf elaboration — the recursive
        // call lives inside the interpolation's anonymous `s!"{...}"`
        // expansion, and the wf check pins anonymous binders the
        // tactic chain can't pin back to the source-named param.
        // Wumpus's `roomListItem` (`_ => s!"{rs}, {roomList rest}"`)
        // is the canonical shape. Fall back to fuel for this case.
        if body_has_intra_scc_call_in_interpolation(fd.body.as_ref(), &names) {
            return true;
        }
        for (callee_raw, args) in collect_calls_from_body(fd.body.as_ref()) {
            let Some(callee_name) = canonical_callee_name(&callee_raw, &names) else {
                continue;
            };
            let Some(callee_indices) = sizeof_indices.get(&callee_name) else {
                continue;
            };
            for callee_idx in callee_indices {
                let Some(arg) = args.get(*callee_idx) else {
                    continue;
                };
                if !arg_is_non_growing(arg) {
                    return true;
                }
            }
        }
    }
    false
}

fn body_has_intra_scc_call_in_interpolation(body: &FnBody, names: &HashSet<String>) -> bool {
    let FnBody::Block(stmts) = body;
    stmts.iter().any(|s| match s {
        Stmt::Binding(_, _, e) | Stmt::Expr(e) => {
            expr_has_intra_scc_call_in_interpolation(e, names)
        }
    })
}

fn expr_has_intra_scc_call_in_interpolation(expr: &Spanned<Expr>, names: &HashSet<String>) -> bool {
    use crate::ast::StrPart;
    match &expr.node {
        Expr::InterpolatedStr(parts) => parts.iter().any(|p| match p {
            StrPart::Parsed(inner) => expr_calls_into_set(inner, names),
            _ => false,
        }),
        Expr::Match { subject, arms, .. } => {
            expr_has_intra_scc_call_in_interpolation(subject, names)
                || arms
                    .iter()
                    .any(|a| expr_has_intra_scc_call_in_interpolation(&a.body, names))
        }
        Expr::FnCall(f, args) => {
            expr_has_intra_scc_call_in_interpolation(f, names)
                || args
                    .iter()
                    .any(|a| expr_has_intra_scc_call_in_interpolation(a, names))
        }
        Expr::TailCall(boxed) => boxed
            .args
            .iter()
            .any(|a| expr_has_intra_scc_call_in_interpolation(a, names)),
        Expr::BinOp(_, l, r) => {
            expr_has_intra_scc_call_in_interpolation(l, names)
                || expr_has_intra_scc_call_in_interpolation(r, names)
        }
        Expr::Constructor(_, Some(inner)) | Expr::Attr(inner, _) | Expr::Neg(inner) => {
            expr_has_intra_scc_call_in_interpolation(inner, names)
        }
        _ => false,
    }
}

fn expr_calls_into_set(expr: &Spanned<Expr>, names: &HashSet<String>) -> bool {
    match &expr.node {
        Expr::FnCall(f, args) => {
            let head_hit = expr_to_dotted_name(f)
                .as_deref()
                .map(|n| canonical_callee_name(n, names).is_some())
                .unwrap_or(false);
            head_hit
                || args.iter().any(|a| expr_calls_into_set(a, names))
                || expr_calls_into_set(f, names)
        }
        Expr::TailCall(boxed) => {
            canonical_callee_name(&boxed.target, names).is_some()
                || boxed.args.iter().any(|a| expr_calls_into_set(a, names))
        }
        Expr::Attr(inner, _) | Expr::Neg(inner) | Expr::Constructor(_, Some(inner)) => {
            expr_calls_into_set(inner, names)
        }
        Expr::BinOp(_, l, r) => expr_calls_into_set(l, names) || expr_calls_into_set(r, names),
        Expr::Match { subject, arms, .. } => {
            expr_calls_into_set(subject, names)
                || arms.iter().any(|a| expr_calls_into_set(&a.body, names))
        }
        _ => false,
    }
}

fn arg_is_non_growing(expr: &Spanned<Expr>) -> bool {
    match &expr.node {
        Expr::Ident(_) | Expr::Resolved { .. } | Expr::Literal(_) | Expr::Attr(..) => true,
        // `Map.entries(x)` lowers to `x` (AverMap is list-backed; `entries` is
        // the identity), so it is size-PRESERVING — non-growing exactly when
        // its argument is. This lets a json-shaped delegation
        // `objectSafe(m) = entriesSafe(Map.entries m)` read as the same-`sizeOf`
        // lex step it actually is (the rank, not the size, decreases). Sound:
        // a wrong size-claim only makes the SCC's native `termination_by` fail
        // to elaborate (fall back to fuel), never a false theorem.
        Expr::FnCall(callee, args) => {
            expr_to_dotted_name(callee).as_deref() == Some("Map.entries")
                && args.len() == 1
                && arg_is_non_growing(&args[0])
        }
        _ => false,
    }
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
                let edges = same_edges.get_mut(&fd.name)?;
                edges.insert(callee);
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

/// True when every callsite of `fn_name` visible inside `ctx` is also
/// inside `ctx` — i.e. no module that uses `ctx` for proof emission can
/// have an external caller we can't analyze. Holds when either:
/// - the fn lives in the entry items and the entry module has no
///   explicit `exposes` clause (single-file program or implicitly-
///   private surface), or has an `exposes` list that omits `fn_name`;
/// - the fn lives in a dep module and that dep module's `exposes`
///   omits `fn_name`. Dep modules without an `exposes` clause are
///   conservative — treated as exposed since downstream consumers may
///   pull them in unscoped; we don't yet thread that distinction so
///   the conservative side is "not closed-world".
///
/// The `ctx.items` carry a `TopLevel::Module(m)` for entry; dep
/// modules currently surface no `Module` block in `ModuleInfo`, so
/// the dep-module branch returns `false` until that plumbing is
/// added. Practically this matches the issue-84 scope: per-file
/// proof targets (fibonacci.av) where the analyzed fn is in the
/// entry module.
pub(crate) fn is_closed_world_pure_fn(fn_name: &str, inputs: &ProofLowerInputs) -> bool {
    let entry_module = inputs.entry_items.iter().find_map(|item| match item {
        TopLevel::Module(m) => Some(m),
        _ => None,
    });
    let entry_fn_names: std::collections::HashSet<&str> = inputs
        .entry_items
        .iter()
        .filter_map(|item| match item {
            TopLevel::FnDef(fd) => Some(fd.name.as_str()),
            _ => None,
        })
        .collect();
    if entry_fn_names.contains(fn_name) {
        match entry_module {
            None => return true,
            Some(m) => {
                if m.exposes_line.is_none() {
                    return true;
                }
                return !m.exposes.iter().any(|e| e == fn_name);
            }
        }
    }
    false
}

/// Find the unique external caller of `target_fn` in `ctx` and return
/// the path-constraint predicates that wrap the callsite — flipped to
/// positive form and substituted into the callee's variable space.
///
/// Returns `None` when:
/// - there are zero or more than one callers (the issue-84 single-caller
///   simplification — same idea as opaque types having one smart
///   constructor);
/// - the caller has multiple callsites with non-identical enclosing
///   guards (no single weakest precondition);
/// - the arg passed at the countdown-param position isn't a plain local
///   name (caller did `worker(n + 5)` or `worker(f(n))` — substitution
///   would need arithmetic the proof emitter doesn't run);
/// - any enclosing guard is `match (non-bool expression) { ... }` —
///   only `match Bool { true/false -> ... }` and `if/then/else` over
///   bool subjects give a flat predicate, anything else is rejected so
///   the fall-through to fuel stays sound.
///
/// On success the returned list's conjunction is the precondition for
/// `target_fn`'s aux: every clause is a positive `Spanned<Expr>` Bool
/// predicate in the callee's variable space, ready to feed straight
/// into `emit_expr` the same way opaque-type predicates do.
pub(crate) fn find_single_external_caller_predicate(
    target_fn: &str,
    target_param_index: usize,
    callee_param_name: &str,
    inputs: &ProofLowerInputs,
) -> Option<Vec<Spanned<Expr>>> {
    let all_callers: Vec<&FnDef> = inputs
        .entry_items
        .iter()
        .filter_map(|item| match item {
            TopLevel::FnDef(fd) if fd.name != target_fn => Some(fd),
            _ => None,
        })
        .chain(
            inputs
                .dep_modules
                .iter()
                .flat_map(|m| m.fn_defs.iter().filter(|fd| fd.name != target_fn)),
        )
        .filter(|fd| {
            collect_calls_from_body(fd.body.as_ref())
                .iter()
                .any(|(n, _)| call_matches(n, target_fn))
        })
        .collect();

    if all_callers.len() != 1 {
        return None;
    }
    let caller = all_callers[0];

    let mut callsites: Vec<(Vec<Spanned<Expr>>, String)> = Vec::new();
    for stmt in caller.body.stmts() {
        match stmt {
            Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => {
                walk_caller_collect_callsite_guards(
                    expr,
                    target_fn,
                    target_param_index,
                    &[],
                    &mut callsites,
                );
            }
        }
    }

    if callsites.is_empty() {
        return None;
    }

    // For multiple callsites: require IDENTICAL guard lists (else the
    // weakest precondition isn't trivial and we punt to fuel).
    let (first_guards, first_arg_name) = &callsites[0];
    for (other_guards, other_arg_name) in &callsites[1..] {
        if other_arg_name != first_arg_name || other_guards != first_guards {
            return None;
        }
    }

    let arg_name = first_arg_name.as_str();
    let predicate = first_guards
        .iter()
        .filter(|g| crate::codegen::recursion::expr_references_ident(g, arg_name))
        .map(|g| {
            crate::codegen::recursion::substitute_ident_in_expr(g, arg_name, callee_param_name)
        })
        .collect::<Vec<_>>();

    Some(predicate)
}

/// Walker that collects all callsites of `target_fn` inside `expr`,
/// pairing each with (a) the chain of enclosing `match Bool` /
/// `if-then-else` guards in **positive form** (false-arm guards
/// flipped via `flip_comparison_binop`) and (b) the caller-side
/// variable name passed at `target_param_index`.
fn walk_caller_collect_callsite_guards(
    expr: &Spanned<Expr>,
    target_fn: &str,
    target_param_index: usize,
    enclosing_guards: &[Spanned<Expr>],
    out: &mut Vec<(Vec<Spanned<Expr>>, String)>,
) {
    match &expr.node {
        Expr::FnCall(callee, args) => {
            if let Some(name) = expr_to_dotted_name(callee)
                && call_matches(&name, target_fn)
                && let Some(arg_at_idx) = args.get(target_param_index)
                && let Some(arg_name) = local_name_of(arg_at_idx)
            {
                out.push((enclosing_guards.to_vec(), arg_name.to_string()));
            }
            walk_caller_collect_callsite_guards(
                callee,
                target_fn,
                target_param_index,
                enclosing_guards,
                out,
            );
            for arg in args {
                walk_caller_collect_callsite_guards(
                    arg,
                    target_fn,
                    target_param_index,
                    enclosing_guards,
                    out,
                );
            }
        }
        Expr::TailCall(boxed) => {
            for arg in &boxed.args {
                walk_caller_collect_callsite_guards(
                    arg,
                    target_fn,
                    target_param_index,
                    enclosing_guards,
                    out,
                );
            }
        }
        Expr::Match { subject, arms } => {
            for arm in arms {
                let mut new_guards: Vec<Spanned<Expr>> = enclosing_guards.to_vec();
                match &arm.pattern {
                    Pattern::Literal(crate::ast::Literal::Bool(true)) => {
                        new_guards.push((**subject).clone());
                    }
                    Pattern::Literal(crate::ast::Literal::Bool(false)) => {
                        if let Some(flipped) =
                            crate::codegen::recursion::flip_comparison_binop(subject)
                        {
                            new_guards.push(flipped);
                        } else {
                            // Subject isn't a comparison BinOp — can't
                            // flip into a positive Aver expression.
                            // Drop the guard so the predicate stays
                            // sound (subset of true constraints).
                        }
                    }
                    _ => {
                        // Non-bool arm (e.g., `0 -> ...`, `[head, ..tail]
                        // -> ...`). Bind constraints aren't part of the
                        // bool-predicate vocabulary; treat the arm as
                        // transparent.
                    }
                }
                walk_caller_collect_callsite_guards(
                    &arm.body,
                    target_fn,
                    target_param_index,
                    &new_guards,
                    out,
                );
            }
            walk_caller_collect_callsite_guards(
                subject,
                target_fn,
                target_param_index,
                enclosing_guards,
                out,
            );
        }
        Expr::BinOp(_, l, r) => {
            walk_caller_collect_callsite_guards(
                l,
                target_fn,
                target_param_index,
                enclosing_guards,
                out,
            );
            walk_caller_collect_callsite_guards(
                r,
                target_fn,
                target_param_index,
                enclosing_guards,
                out,
            );
        }
        Expr::Attr(inner, _) | Expr::Neg(inner) | Expr::ErrorProp(inner) => {
            walk_caller_collect_callsite_guards(
                inner,
                target_fn,
                target_param_index,
                enclosing_guards,
                out,
            );
        }
        Expr::Constructor(_, arg) => {
            if let Some(inner) = arg {
                walk_caller_collect_callsite_guards(
                    inner,
                    target_fn,
                    target_param_index,
                    enclosing_guards,
                    out,
                );
            }
        }
        Expr::InterpolatedStr(parts) => {
            for p in parts {
                if let crate::ast::StrPart::Parsed(inner) = p {
                    walk_caller_collect_callsite_guards(
                        inner,
                        target_fn,
                        target_param_index,
                        enclosing_guards,
                        out,
                    );
                }
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for item in items {
                walk_caller_collect_callsite_guards(
                    item,
                    target_fn,
                    target_param_index,
                    enclosing_guards,
                    out,
                );
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries {
                walk_caller_collect_callsite_guards(
                    k,
                    target_fn,
                    target_param_index,
                    enclosing_guards,
                    out,
                );
                walk_caller_collect_callsite_guards(
                    v,
                    target_fn,
                    target_param_index,
                    enclosing_guards,
                    out,
                );
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, v) in fields {
                walk_caller_collect_callsite_guards(
                    v,
                    target_fn,
                    target_param_index,
                    enclosing_guards,
                    out,
                );
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            walk_caller_collect_callsite_guards(
                base,
                target_fn,
                target_param_index,
                enclosing_guards,
                out,
            );
            for (_, v) in updates {
                walk_caller_collect_callsite_guards(
                    v,
                    target_fn,
                    target_param_index,
                    enclosing_guards,
                    out,
                );
            }
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } => {}
    }
}

/// Classify every recursive pure fn in `inputs`. The returned map
/// assigns each supported function a [`RecursionPlan`]; anything
/// that falls outside the recognised shapes becomes a
/// [`ProofModeIssue`].
///
/// **Consumers should not call this directly.** After Step 18 / 20
/// the only caller is `proof_lower::populate_fn_contracts`, which
/// translates the output into `ProofIR.fn_contracts` and
/// `ProofIR.unclassified_fns`. Backends read those fields instead.
/// The fn stays `pub` only because the diff test
/// (`tests/proof_ir_diff.rs`) cross-checks ProofIR against the
/// legacy classifier output — that test is itself slated for
/// rationalisation once the migration is fully bedded down.
/// Scope-aware classifier. `scope = None` runs against entry only,
/// `Some(prefix)` against one dep module. Aver's module DAG
/// invariant rules out cross-module recursion SCCs, so per-scope
/// classification is the canonical view and avoids two
/// same-bare-name fns from different modules colliding in the
/// SCC analyser's `HashMap<name, _>`. The `global_view` flag keeps
/// the legacy "chain all fns, classify globally" behaviour for
/// `tests/proof_ir_diff.rs` cross-check tests; production callers
/// always pass `global_view = false` and walk scopes themselves.
pub fn analyze_plans_in_scope(
    inputs: &ProofLowerInputs,
    scope: Option<&str>,
    global_view: bool,
) -> (HashMap<String, RecursionPlan>, Vec<ProofModeIssue>) {
    let mut plans = HashMap::new();
    let mut issues = Vec::new();

    let all_pure = if global_view {
        inputs.pure_fns()
    } else {
        inputs.pure_fns_in_scope(scope)
    };
    let recursive_names = if global_view {
        inputs.recursive_pure_fn_names()
    } else {
        inputs.recursive_pure_fn_names_in_scope(scope)
    };
    let components = call_graph::ordered_fn_components(&all_pure, inputs.module_prefixes);

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
            // Try native guarded emission first — when the body has the
            // clean `match p { 0 -> BASE; _ -> rec(p-1, ...) }` shape and
            // the fn is closed-world (no external module can call it
            // with a negative param), Lean can emit a real recursive
            // def with `(h : p ≥ 0)` precondition + termination on
            // `p.natAbs`. Falls back to the fuel-encoded IntCountdown
            // plan when either condition fails.
            if is_closed_world_pure_fn(&fd.name, inputs)
                && let Some((base_arm_literal, base_arm_body, wildcard_arm_body)) =
                    int_countdown_native_arms(fd, param_index)
                && let Some((callee_param_name, _)) = fd.params.get(param_index)
            {
                // Caller-derived precondition first; empty means
                // "no single external caller in this artifact" and the
                // Lean emitter defaults to `(h_dom : p ≥ 0)` for
                // legacy fibTR-shape compatibility.
                let precondition = find_single_external_caller_predicate(
                    &fd.name,
                    param_index,
                    callee_param_name,
                    inputs,
                )
                .unwrap_or_default();
                plans.insert(
                    fd.name.clone(),
                    RecursionPlan::IntCountdownGuarded {
                        param_index,
                        base_arm_literal,
                        base_arm_body,
                        wildcard_arm_body,
                        precondition,
                    },
                );
            } else {
                plans.insert(fd.name.clone(), RecursionPlan::IntCountdown { param_index });
            }
        } else if let Some((param_index, divisor, helper_fn)) =
            single_int_floor_div_countdown(fd, inputs)
        {
            // Floor-division countdown — every self-call shrinks an
            // Int param by `Result.withDefault(Int.div(p, k), d)`
            // (literal k >= 2, possibly through a unary wrapper fn)
            // and the guard chain at every self-call site implies
            // `p >= 1`. Both side-conditions are validated above;
            // backends emit a native well-founded def.
            plans.insert(
                fd.name.clone(),
                RecursionPlan::IntFloorDivCountdown {
                    param_index,
                    divisor,
                    helper_fn,
                },
            );
        } else if supports_single_sizeof_structural(fd, inputs)
            || (single_list_structural_param_index(fd).is_none()
                && single_adt_structural_param_index(fd, inputs).is_some())
        {
            // Single-param ADT structural recursion (the OR's second arm) covers
            // the accumulator-fold inner loop (`triTR` / `factTR`): one recursive-
            // ADT param strictly shrinks every call while a sibling accumulator is
            // threaded. Lean infers structural termination on the shrinking param,
            // so it emits as a plain `def` (no fuel) just like the multi-param
            // measure case. Gated on NOT being list-structural so a fn that also
            // peels a `List` (`drop(n, xs)` shrinks both the `Nat` and the list)
            // keeps its `ListStructural` plan — its laws induct on the list, and
            // stealing it to the ADT driver regressed `drop`-concat decomposition.
            plans.insert(fd.name.clone(), RecursionPlan::SizeOfStructural);
        } else if let Some((param_index, peel)) = single_list_structural_param(fd) {
            plans.insert(
                fd.name.clone(),
                RecursionPlan::ListStructural { param_index, peel },
            );
        } else if supports_single_string_pos_advance(fd) {
            plans.insert(fd.name.clone(), RecursionPlan::StringPosAdvance);
        } else {
            issues.push(ProofModeIssue {
                line: fd.line,
                message: format!(
                    "recursive function '{}' is outside proof subset (currently supported: Int countdown, guard-validated Int floor-division countdown by a literal divisor, second-order affine Int recurrences with pair-state worker, structural recursion on List/recursive ADTs, String+position, mutual Int countdown, mutual String+position, and ranked sizeOf recursion)",
                    fd.name
                ),
            });
        }
    }

    (plans, issues)
}

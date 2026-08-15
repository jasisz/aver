//! Buffer-build sink detection.
//!
//! Identifies user fns that match the canonical functional list-builder
//! shape consumed by `String.join`:
//!
//! ```aver
//! fn build(..., acc: List<T>) -> List<T>
//!     match <cond>
//!         true  -> List.reverse(acc)
//!         false -> build(..., List.prepend(<elem>, acc))
//! ```
//!
//! …and the list-driven spelling of the same loop, which is what most
//! Aver code actually writes:
//!
//! ```aver
//! fn build(xs: List<S>, acc: List<T>) -> List<T>
//!     match xs
//!         [] -> List.reverse(acc)
//!         [head, ..tail] -> build(tail, List.prepend(<elem>, acc))
//! ```
//!
//! See [`BufferBuildKind`] for the full set of recognised idioms.
//!
//! When such a fn is called from `String.join(build(..., []), sep)`, the
//! whole pipeline is semantically equivalent to a single buffer-write
//! loop — Wadler 1990 shortcut fusion / deforestation. This module is
//! Phase 1 of the deforestation work for 0.15 "Traversal": it detects
//! candidate fns. Lowering (rewriting matched fns + their `String.join`
//! call sites) lives in a separate pass.
//!
//! Detection is intentionally local — the analyzer looks only at the fn
//! body, not its call sites. A matched fn may or may not actually be
//! consumed by `String.join`; the lowering pass cross-references call
//! sites separately and only fuses when both ends of the pipeline agree.

use std::collections::HashMap;
use std::sync::Arc;

use crate::ast::{
    Expr, FnBody, FnDef, Literal, MatchArm, Pattern, Spanned, Stmt, StrPart, TailCallData,
};

/// Which builder idiom the sink follows — the cross product of two
/// independent axes: what drives the loop (a `Bool` condition or the
/// input list itself) and where the single `List.reverse` lives (in the
/// sink's own base case, or at the call site).
///
/// `prepend(elem, acc)` builds the accumulator in reverse-of-input
/// order. To get a forward list (the order we'd hand to `String.join`)
/// it has to be reversed exactly once. The three recognised idioms:
///
/// - `InternalReverse`: Bool-driven. The sink has
///   `true -> List.reverse(acc)` in its base case. Caller writes
///   `String.join(<sink>(args, []), sep)`. The classic "count down,
///   reverse on exit" shape.
/// - `ExternalReverse`: list-driven, no reverse in the sink
///   (`[] -> acc`). Caller writes
///   `String.join(List.reverse(<sink>(args, [])), sep)`. Common in the
///   payment_ops / workflow_engine codebases under the `*Into` naming
///   convention (e.g. `serializeEntriesInto`, `filterSubjectInto`).
/// - `ListInternalReverse`: list-driven AND reverse in the sink
///   (`[] -> List.reverse(acc)`), so the caller writes the plain
///   `String.join(<sink>(args, []), sep)` again. This is the most
///   idiomatic Aver list loop and the shape Aver's own
///   `Bytes.hexParts` / `Bytes.toHex` pair uses.
///
/// All three lower to the same buffered variant — appending in
/// processing order (which is forward order of the input) yields the
/// final string in the right order without any explicit reverse. The
/// kind still matters at the call site: it decides whether a
/// `List.reverse(...)` wrapper is REQUIRED there or FORBIDDEN, and a
/// mismatch means the program reverses a different number of times
/// than the rewrite would.
// The shared `Reverse` suffix is the point: where the single reverse
// lives is what the enum is about, and it is what the call site has to
// agree with. Dropping it would leave names that no longer say which
// half of the pipeline reverses.
#[allow(clippy::enum_variant_names)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BufferBuildKind {
    InternalReverse,
    ExternalReverse,
    ListInternalReverse,
}

/// Information about a fn that matches the buffer-build sink shape.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BufferBuildShape {
    /// 0-based index of the `acc: List<T>` parameter in the fn signature.
    /// Identifies which arg in tail-call positions threads the
    /// accumulator and which `Ident` in the base-case arm is returned.
    pub acc_param_idx: usize,
    /// The accumulator parameter's binding name (looked up in tail-call
    /// args and in the base-case return position).
    pub acc_param_name: String,
    /// Which of the two reverse-placement idioms this sink follows;
    /// determines (a) what shape the original base arm has and (b)
    /// whether the call site is `String.join(<sink>(...), sep)` or
    /// `String.join(List.reverse(<sink>(...)), sep)`.
    pub kind: BufferBuildKind,
}

/// What the matched builder feeds into. Different consumers compile
/// to different buffer types and finalizers, but all share the same
/// underlying deforestation: skip the intermediate List, write
/// elements straight to the consumer's storage.
///
/// Phase 2 implements `StringJoin` only — the canonical case from the
/// fractal demo. Future variants land as separate phases:
/// `VectorFromList` (already half-fused via `Vector.set` owned-mutate
/// in 0.14.0; deforestation closes the cons-cell side), and `ListFold`
/// for stream-fusion-style consumer rewrites.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConsumerKind {
    /// `String.join(builder(...), sep)` — write each element + sep
    /// directly into a `Vec<u8>`-shaped buffer in linear memory.
    StringJoin,
}

/// One detected fusion site: a builder call whose result is consumed
/// by a known sink (currently just `String.join`). Lowering rewrites
/// the producer + consumer pair into a single buffer-write loop.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FusionSite {
    /// Name of the enclosing user fn that contains the call.
    pub enclosing_fn: String,
    /// Line of the consumer call.
    pub line: usize,
    /// The matched buffer-build fn being wrapped.
    pub sink_fn: String,
    /// What's consuming the builder's result.
    pub consumer: ConsumerKind,
}

/// Walk all fns in `fns`, return a map from fn name to detected shape
/// for fns that match the buffer-build sink pattern. Fns that don't
/// match are absent from the result.
pub fn compute_buffer_build_sinks(fns: &[&FnDef]) -> HashMap<String, BufferBuildShape> {
    let mut out = HashMap::new();
    for fd in fns {
        if let Some(shape) = match_buffer_build_shape(fd) {
            out.insert(fd.name.clone(), shape);
        }
    }
    out
}

/// Walk every expression in every fn body looking for fusion sites:
/// `String.join(matched_fn(...), sep)` calls where `matched_fn` is a
/// key in `sinks`. Returns one `FusionSite` per call. The lowering
/// pass rewrites each site to call a buffered variant of `matched_fn`
/// directly into a pre-allocated buffer.
pub fn find_fusion_sites(
    fns: &[&FnDef],
    sinks: &HashMap<String, BufferBuildShape>,
) -> Vec<FusionSite> {
    let mut out = Vec::new();
    for fd in fns {
        for stmt in fd.body.stmts() {
            match stmt {
                Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => {
                    walk_expr_for_fusion_sites(&expr.node, expr.line, &fd.name, sinks, &mut out);
                }
            }
        }
    }
    out
}

/// Recursively walk an expression tree, recording any fusion site we
/// find. The fallback `expr_line` is used when a sub-expression has no
/// own line info.
fn walk_expr_for_fusion_sites(
    expr: &Expr,
    expr_line: usize,
    enclosing_fn: &str,
    sinks: &HashMap<String, BufferBuildShape>,
    out: &mut Vec<FusionSite>,
) {
    if let Some(inner_name) = match_string_join_fusion_site(expr, sinks) {
        out.push(FusionSite {
            enclosing_fn: enclosing_fn.to_string(),
            line: expr_line,
            sink_fn: inner_name,
            consumer: ConsumerKind::StringJoin,
        });
    }
    // Recurse into all sub-expressions regardless of whether this node
    // matched (a fusion site can sit inside another fusion site's args
    // — rare but valid; we'd record both and let the lowering decide).
    visit_subexprs(expr, expr_line, enclosing_fn, sinks, out);
}

/// Helper: recurse into the sub-expressions of `expr`. Mirrors the
/// shape coverage of `expr_allocates` in `alloc_info.rs` so we don't
/// miss any node kind.
fn visit_subexprs(
    expr: &Expr,
    fallback_line: usize,
    enclosing_fn: &str,
    sinks: &HashMap<String, BufferBuildShape>,
    out: &mut Vec<FusionSite>,
) {
    let line_of = |s: &crate::ast::Spanned<Expr>| {
        if s.line > 0 { s.line } else { fallback_line }
    };
    match expr {
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } | Expr::Constructor(_, None) => {}
        Expr::Constructor(_, Some(inner)) | Expr::Attr(inner, _) | Expr::ErrorProp(inner) => {
            walk_expr_for_fusion_sites(&inner.node, line_of(inner), enclosing_fn, sinks, out);
        }
        Expr::FnCall(callee, args) => {
            walk_expr_for_fusion_sites(&callee.node, line_of(callee), enclosing_fn, sinks, out);
            for a in args {
                walk_expr_for_fusion_sites(&a.node, line_of(a), enclosing_fn, sinks, out);
            }
        }
        Expr::TailCall(data) => {
            for a in &data.args {
                walk_expr_for_fusion_sites(&a.node, line_of(a), enclosing_fn, sinks, out);
            }
        }
        Expr::BinOp(_, l, r) => {
            walk_expr_for_fusion_sites(&l.node, line_of(l), enclosing_fn, sinks, out);
            walk_expr_for_fusion_sites(&r.node, line_of(r), enclosing_fn, sinks, out);
        }
        Expr::Neg(inner) => {
            walk_expr_for_fusion_sites(&inner.node, line_of(inner), enclosing_fn, sinks, out);
        }
        Expr::Match { subject, arms } => {
            walk_expr_for_fusion_sites(&subject.node, line_of(subject), enclosing_fn, sinks, out);
            for arm in arms {
                walk_expr_for_fusion_sites(
                    &arm.body.node,
                    line_of(&arm.body),
                    enclosing_fn,
                    sinks,
                    out,
                );
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for it in items {
                walk_expr_for_fusion_sites(&it.node, line_of(it), enclosing_fn, sinks, out);
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries {
                walk_expr_for_fusion_sites(&k.node, line_of(k), enclosing_fn, sinks, out);
                walk_expr_for_fusion_sites(&v.node, line_of(v), enclosing_fn, sinks, out);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, v) in fields {
                walk_expr_for_fusion_sites(&v.node, line_of(v), enclosing_fn, sinks, out);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            walk_expr_for_fusion_sites(&base.node, line_of(base), enclosing_fn, sinks, out);
            for (_, v) in updates {
                walk_expr_for_fusion_sites(&v.node, line_of(v), enclosing_fn, sinks, out);
            }
        }
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let crate::ast::StrPart::Parsed(inner) = part {
                    walk_expr_for_fusion_sites(
                        &inner.node,
                        line_of(inner),
                        enclosing_fn,
                        sinks,
                        out,
                    );
                }
            }
        }
    }
}

/// Pattern-match a single fn against the buffer-build shape.
fn match_buffer_build_shape(fd: &FnDef) -> Option<BufferBuildShape> {
    // The accumulator must be a parameter of type `List<...>`. The
    // params vector stores type strings, not parsed `Type` values, so we
    // match the textual form. Aver's surface syntax accepts both
    // `List<T>` and (rarely) `[T]`-like sugar; canonical form is
    // `List<T>`.
    // Take the *rightmost* List<...> parameter as the accumulator. The
    // InternalReverse shape (`fn build(n: Int, acc: List<T>)`) typically
    // has only one list param; the ExternalReverse shape often has two
    // (`fn build(input: List<T>, acc: List<U>)`) and the accumulator is
    // by convention the last argument. Picking the first match would
    // misidentify `input` as the accumulator and the rest of the
    // detection would silently fail.
    let (acc_idx, acc_name) = fd
        .params
        .iter()
        .enumerate()
        .rfind(|(_, (_, ty))| is_list_type_str(ty))
        .map(|(i, (name, _))| (i, name.clone()))?;

    // Body must be a single expression statement holding the match.
    let match_expr = single_match_body(&fd.body)?;
    let (subject_expr, arms) = match match_expr {
        Expr::Match { subject, arms } => (subject, arms),
        _ => return None,
    };

    // The subject rides through the rewrite unchanged onto the buffered
    // variant's own match, which has no accumulator parameter — so a
    // loop that decides when to stop by looking at what it has collected
    // (`match List.len(acc) >= 3`) would leave the name dangling there.
    // Same occurs-check as the recursive arm, only here the budget is
    // zero: nothing in the subject position is ever replaced.
    if count_ident_reads(&subject_expr.node, &acc_name) > 0 {
        return None;
    }

    // An arm that re-binds the accumulator's name makes the reads below
    // mean something else: `[acc, ..tail] -> f(tail, List.prepend(x, acc))`
    // threads the HEAD OF THE INPUT, not the accumulator the loop was
    // handed, and the base case still returns the untouched parameter.
    // The occurs-check would see one read in the prepend tail and be
    // satisfied — it counts names, and here the name lies. The rewrite
    // keeps the pattern and drops the parameter, so the loop it emits is
    // not the loop that was written.
    if arms
        .iter()
        .any(|a| pattern_binds_name(&a.pattern, &acc_name))
    {
        return None;
    }

    // Try the InternalReverse shape first: `match <bool> { true -> List.reverse(acc); false -> recurse(... prepend(_, acc)) }`.
    if let Some((true_body, false_body)) = pair_bool_arms(arms)
        && is_list_reverse_of(true_body, &acc_name)
        && is_self_tail_with_prepend_acc(false_body, &fd.name, acc_idx, &acc_name)
    {
        return Some(BufferBuildShape {
            acc_param_idx: acc_idx,
            acc_param_name: acc_name,
            kind: BufferBuildKind::InternalReverse,
        });
    }

    // Otherwise the loop is list-driven: `match <list> { [] -> <base>;
    // [_, .._] -> recurse(... prepend(_, acc)) }`. The recursive arm is
    // identical in both list-driven idioms; only the base arm differs,
    // and it is exactly what says where the single reverse lives:
    //   `[] -> acc`               → ExternalReverse (caller reverses)
    //   `[] -> List.reverse(acc)` → ListInternalReverse (sink reverses)
    // Anything else in the base arm — reversing some other binding,
    // returning a literal, a nested call — stays unmatched, and an
    // unmatched sink is simply never fused.
    if let Some((nil_body, cons_body)) = pair_nil_cons_arms(arms)
        && is_self_tail_with_prepend_acc(cons_body, &fd.name, acc_idx, &acc_name)
    {
        let kind = if is_ident_named(nil_body, &acc_name) {
            BufferBuildKind::ExternalReverse
        } else if is_list_reverse_of(nil_body, &acc_name) {
            BufferBuildKind::ListInternalReverse
        } else {
            return None;
        };
        return Some(BufferBuildShape {
            acc_param_idx: acc_idx,
            acc_param_name: acc_name,
            kind,
        });
    }

    None
}

/// Match a 2-arm match where one arm is `[]` and the other is
/// `[head, ..tail]`. Returns `(nil_body, cons_body)` (both as `&Expr`,
/// matching `pair_bool_arms`). `None` if the arms don't exactly cover
/// those two patterns.
fn pair_nil_cons_arms(arms: &[MatchArm]) -> Option<(&Expr, &Expr)> {
    if arms.len() != 2 {
        return None;
    }
    let mut nil_body: Option<&Expr> = None;
    let mut cons_body: Option<&Expr> = None;
    for arm in arms {
        match &arm.pattern {
            Pattern::EmptyList => nil_body = Some(&arm.body.node),
            Pattern::Cons(_, _) => cons_body = Some(&arm.body.node),
            _ => return None,
        }
    }
    match (nil_body, cons_body) {
        (Some(n), Some(c)) => Some((n, c)),
        _ => None,
    }
}

/// True if `expr` is just an identifier reference to `name`.
fn is_ident_named(expr: &Expr, name: &str) -> bool {
    matches!(expr, Expr::Ident(n) if n == name)
}

/// Recognise a `String.join` first-arg as either a direct sink call or
/// a `List.reverse(<sink>(args, []))` wrapper. Returns the matched
/// sink fn name when the kind matches the shape we expect:
///   InternalReverse sinks → only the direct call form
///   ExternalReverse sinks → only the `List.reverse(...)` form
/// Returning the name only when the kinds line up keeps us from
/// accidentally fusing a sink against a call site that's missing
/// its required reverse (or has an extraneous one).
/// Single source of truth for "is this expression a rewriteable
/// `String.join(<sink>(args, []), sep)` fusion site?". Returns the
/// matched sink name when **all** preconditions hold:
/// 1. The expression is a `String.join(<inner>, _)` call.
/// 2. `<inner>` is either a direct sink call (for InternalReverse
///    sinks) or `List.reverse(<sink>(...))` (for ExternalReverse).
/// 3. The reverse-placement on the call site matches the sink's kind
///    — mismatch would silently drop or double-reverse.
/// 4. The acc-position arg in the inner call is a literal empty list.
///    Anything else means the user is starting the fold with a
///    non-empty accumulator, and the buffered rewrite would silently
///    drop those initial elements.
///
/// Both `find_fusion_sites` (diagnostics) and `try_rewrite_fusion_site`
/// (the actual AST rewrite) call this so the two stay in lockstep —
/// `aver check` can never report a site that the rewrite then refuses
/// to take.
fn match_string_join_fusion_site(
    expr: &Expr,
    sinks: &HashMap<String, BufferBuildShape>,
) -> Option<String> {
    let Expr::FnCall(callee, args) = expr else {
        return None;
    };
    if !is_dotted_ident(&callee.node, "String", "join") || args.len() != 2 {
        return None;
    }
    let consumer_arg = &args[0].node;

    // Peel an optional `List.reverse(...)` wrapper.
    let (inner_call_expr, saw_external_reverse) = match consumer_arg {
        Expr::FnCall(rev_callee, rev_args)
            if is_dotted_ident(&rev_callee.node, "List", "reverse") && rev_args.len() == 1 =>
        {
            (&rev_args[0].node, true)
        }
        other => (other, false),
    };

    let Expr::FnCall(inner_callee, inner_args) = inner_call_expr else {
        return None;
    };
    let Expr::Ident(name) = &inner_callee.node else {
        return None;
    };
    let shape = sinks.get(name)?;

    let kinds_align = matches!(
        (saw_external_reverse, &shape.kind),
        (
            false,
            BufferBuildKind::InternalReverse | BufferBuildKind::ListInternalReverse
        ) | (true, BufferBuildKind::ExternalReverse)
    );
    if !kinds_align {
        return None;
    }

    let acc_arg = inner_args.get(shape.acc_param_idx)?;
    if !matches!(&acc_arg.node, Expr::List(items) if items.is_empty()) {
        return None;
    }

    Some(name.clone())
}

/// True if a parameter type-string parses as `List<...>`.
fn is_list_type_str(ty: &str) -> bool {
    let t = ty.trim();
    t.starts_with("List<") && t.ends_with('>')
}

/// Extract the single match expression that forms a fn's entire body.
/// Returns `None` if the body is empty, has multiple statements, or its
/// single statement isn't a match expression.
fn single_match_body(body: &FnBody) -> Option<&Expr> {
    let stmts = body.stmts();
    if stmts.len() != 1 {
        return None;
    }
    match &stmts[0] {
        Stmt::Expr(spanned) => match &spanned.node {
            Expr::Match { .. } => Some(&spanned.node),
            _ => None,
        },
        Stmt::Binding(_, _, _) => None,
    }
}

/// If `arms` is exactly two arms with `Bool(true)` / `Bool(false)`
/// patterns, return `(true_body, false_body)` references. Order in
/// source doesn't matter — we sort by pattern.
fn pair_bool_arms(arms: &[MatchArm]) -> Option<(&Expr, &Expr)> {
    if arms.len() != 2 {
        return None;
    }
    let mut t = None;
    let mut f = None;
    for arm in arms {
        match &arm.pattern {
            Pattern::Literal(Literal::Bool(true)) => {
                if t.is_some() {
                    return None;
                }
                t = Some(&arm.body.node);
            }
            Pattern::Literal(Literal::Bool(false)) => {
                if f.is_some() {
                    return None;
                }
                f = Some(&arm.body.node);
            }
            _ => return None,
        }
    }
    Some((t?, f?))
}

/// True if `expr` is `List.reverse(<Ident(acc_name)>)`.
fn is_list_reverse_of(expr: &Expr, acc_name: &str) -> bool {
    let (callee, args) = match expr {
        Expr::FnCall(c, a) => (c, a),
        _ => return false,
    };
    if !is_dotted_ident(&callee.node, "List", "reverse") {
        return false;
    }
    if args.len() != 1 {
        return false;
    }
    matches!(&args[0].node, Expr::Ident(name) if name == acc_name)
}

/// True if `expr` is a tail-call to `self_name` whose `acc_idx`
/// argument is `List.prepend(<elem>, <Ident(acc_name)>)` AND whose
/// arguments mention `acc_name` nowhere else.
///
/// The second half is what makes the rewrite safe. `build_buffered_variant`
/// drops the accumulator from the parameter list and copies `<elem>` and
/// every other argument through verbatim, so a second mention of the
/// accumulator survives into a fn that no longer has one. It becomes a
/// free identifier: either a compile error, or — when the module happens
/// to carry a top-level binding of the same name — a silent read of that
/// binding instead. Both are wrong answers from a pass whose whole claim
/// is that the shape it matched cannot change the answer.
///
/// So the accumulator gets an occurs-check: in the recursive arm it may
/// be read EXACTLY ONCE, as the tail of the recognised prepend. A loop
/// that also inspects what it has collected so far (`List.contains(acc, x)`,
/// `List.len(acc)`) is a different function from the one this pass knows
/// how to buffer, and is declined. Conservative by construction — the
/// check counts identifiers, it does not normalise or reason about what
/// the second read would have meant.
fn is_self_tail_with_prepend_acc(
    expr: &Expr,
    self_name: &str,
    acc_idx: usize,
    acc_name: &str,
) -> bool {
    let data = match expr {
        Expr::TailCall(data) => data,
        _ => return false,
    };
    if data.target != self_name {
        return false;
    }
    // The prepend has to land in the *acc* position specifically — the
    // synthesizer extracts the element expression from `args[acc_idx]`.
    // A loose `any` here would let through fns where some other arg
    // happens to be a prepend, the synth would later return None, but
    // detection had already promised the sink to call-site rewriting:
    // `String.join(<sink>(...))` would be rewritten to call a
    // `<sink>__buffered` that never gets generated. Require the exact
    // shape here so detection and synthesis agree.
    let acc_arg = match data.args.get(acc_idx) {
        Some(a) => a,
        None => return false,
    };
    if !is_list_prepend_to_acc(&acc_arg.node, acc_name) {
        return false;
    }
    // The prepend's own tail is the one permitted read; anything else
    // anywhere in the argument list — inside `<elem>` or in a sibling
    // argument — is a second one.
    let reads: usize = data
        .args
        .iter()
        .map(|a| count_ident_reads(&a.node, acc_name))
        .sum();
    reads == 1
}

/// True when `pattern` introduces a binding called `name`. Exhaustive
/// for the same reason as [`count_ident_reads`]: a pattern form nobody
/// classified would be a shadow nobody noticed.
fn pattern_binds_name(pattern: &Pattern, name: &str) -> bool {
    match pattern {
        Pattern::Wildcard | Pattern::Literal(_) | Pattern::EmptyList => false,
        Pattern::Ident(n) => n == name,
        Pattern::Cons(head, tail) => head == name || tail == name,
        Pattern::Tuple(items) => items.iter().any(|p| pattern_binds_name(p, name)),
        Pattern::Constructor(_, bindings) => bindings.iter().any(|b| b == name),
    }
}

/// How many times `name` is read as a plain identifier anywhere in
/// `expr`, including nested nodes.
///
/// The match is exhaustive on purpose: a missed `Expr` variant would be
/// a missed read, and a missed read is a fuse this pass should have
/// declined. A new variant has to be classified here before it compiles.
///
/// A nested binder that shadows `name` is counted too. That is the safe
/// direction — an unrelated binding that happens to reuse the name costs
/// a fusion we could in principle have kept, never a wrong answer.
fn count_ident_reads(expr: &Expr, name: &str) -> usize {
    let in_each = |exprs: &[Spanned<Expr>]| -> usize {
        exprs.iter().map(|e| count_ident_reads(&e.node, name)).sum()
    };
    match expr {
        Expr::Literal(_) => 0,
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => usize::from(n == name),
        Expr::Attr(base, _) => count_ident_reads(&base.node, name),
        Expr::FnCall(callee, args) => count_ident_reads(&callee.node, name) + in_each(args),
        Expr::BinOp(_, lhs, rhs) => {
            count_ident_reads(&lhs.node, name) + count_ident_reads(&rhs.node, name)
        }
        Expr::Neg(inner) | Expr::ErrorProp(inner) => count_ident_reads(&inner.node, name),
        Expr::Match { subject, arms } => {
            count_ident_reads(&subject.node, name)
                + arms
                    .iter()
                    .map(|a| count_ident_reads(&a.body.node, name))
                    .sum::<usize>()
        }
        Expr::Constructor(_, payload) => payload
            .as_ref()
            .map_or(0, |p| count_ident_reads(&p.node, name)),
        Expr::InterpolatedStr(parts) => parts
            .iter()
            .map(|part| match part {
                StrPart::Literal(_) => 0,
                StrPart::Parsed(e) => count_ident_reads(&e.node, name),
            })
            .sum(),
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            in_each(items)
        }
        Expr::MapLiteral(entries) => entries
            .iter()
            .map(|(k, v)| count_ident_reads(&k.node, name) + count_ident_reads(&v.node, name))
            .sum(),
        Expr::RecordCreate { fields, .. } => fields
            .iter()
            .map(|(_, e)| count_ident_reads(&e.node, name))
            .sum(),
        Expr::RecordUpdate { base, updates, .. } => {
            count_ident_reads(&base.node, name)
                + updates
                    .iter()
                    .map(|(_, e)| count_ident_reads(&e.node, name))
                    .sum::<usize>()
        }
        Expr::TailCall(data) => in_each(&data.args),
    }
}

/// True if `expr` is `List.prepend(<anything>, <Ident(acc_name)>)`.
fn is_list_prepend_to_acc(expr: &Expr, acc_name: &str) -> bool {
    let (callee, args) = match expr {
        Expr::FnCall(c, a) => (c, a),
        _ => return false,
    };
    if !is_dotted_ident(&callee.node, "List", "prepend") {
        return false;
    }
    if args.len() != 2 {
        return false;
    }
    matches!(&args[1].node, Expr::Ident(name) if name == acc_name)
}

/// True if `expr` is `<Module>.<Member>` access (the un-called callee
/// shape of `Module.member(...)`).
fn is_dotted_ident(expr: &Expr, module: &str, member: &str) -> bool {
    let (base, attr) = match expr {
        Expr::Attr(b, a) => (b, a),
        _ => return false,
    };
    if attr != member {
        return false;
    }
    matches!(&base.node, Expr::Ident(name) if name == module)
}

/// Synthesize a `<fn>__buffered` variant for each matched buffer-build
/// sink. The synthesized FnDef walks the same shape as the original but
/// threads a runtime `Buffer` through tail-call args instead of building
/// a `List<T>` of strings:
///
/// Original:
/// ```aver
/// fn build(.., acc: List<T>) -> List<T>
///     match <cond>
///         true  -> List.reverse(acc)
///         false -> build(.., List.prepend(<elem>, acc))
/// ```
///
/// Synthesized:
/// ```aver
/// fn build__buffered(.., __buf: Buffer, __sep: String) -> Buffer
///     match <cond>
///         true  -> __buf
///         false -> build__buffered(..,
///             __buf_append(
///                 __buf_append_sep_unless_first(__buf, __sep),
///                 <elem>
///             ),
///             __sep
///         )
/// ```
///
/// Threading is via expression composition: the inner
/// `__buf_append_sep_unless_first` returns the (possibly grown) buffer,
/// the outer `__buf_append` writes the element and again returns
/// the (possibly grown) buffer, and that final pointer is what the tail
/// call sees as `__buf`. No `_ =` discards anywhere — the C' review
/// explicitly required this to avoid use-after-grow corruption.
///
/// Returns one `FnDef` per matched fn. Caller appends to the user-fn
/// list before WASM emission so both original and buffered variants
/// reach codegen through the same pipeline.
pub fn synthesize_buffered_variants(
    fns: &[&FnDef],
    sinks: &HashMap<String, BufferBuildShape>,
) -> Vec<FnDef> {
    let mut out = Vec::new();
    for fd in fns {
        if let Some(shape) = sinks.get(&fd.name)
            && let Some(buffered) = build_buffered_variant(fd, shape)
        {
            out.push(buffered);
        }
    }
    out
}

/// Wrap an `Expr` as `Spanned<Expr>` carrying the same line as the
/// matched fn (best effort — the synthesized code is internal and
/// won't be source-located by the user, but having a non-zero line
/// keeps downstream visitors happy).
fn sp_at(line: usize, expr: Expr) -> Spanned<Expr> {
    Spanned::new(expr, line)
}

fn sp_at_typed(line: usize, expr: Expr, ty: crate::types::Type) -> Spanned<Expr> {
    let s = Spanned::new(expr, line);
    s.set_ty(ty);
    s
}

/// Build `<intrinsic>(args...)` as a Spanned<Expr>. Intrinsic names
/// are bare identifiers (no module dot) — `__buf_append`,
/// `__buf_append_sep_unless_first`. The WASM emitter recognises them
/// in the builtin dispatch.
///
/// This pass runs *after* the type checker, so synthesized nodes never
/// see `infer_type`. Callers that want the result `Spanned` to carry a
/// type for downstream readers (Rust codegen reads `expr.ty()` to gate
/// owned-mutable `Buffer` hoists) should use [`buffer_intrinsic_call`]
/// or [`finalize_intrinsic_call`].
fn intrinsic_call(line: usize, name: &str, args: Vec<Spanned<Expr>>) -> Spanned<Expr> {
    let callee = sp_at(line, Expr::Ident(name.to_string()));
    sp_at(line, Expr::FnCall(Box::new(callee), args))
}

/// Same as [`intrinsic_call`] but stamps the result with `Buffer` —
/// every `__buf_new` / `__buf_append` / `__buf_append_sep_unless_first`
/// returns the (possibly-grown) owned buffer.
fn buffer_intrinsic_call(line: usize, name: &str, args: Vec<Spanned<Expr>>) -> Spanned<Expr> {
    let call = intrinsic_call(line, name, args);
    call.set_ty(crate::types::Type::named("Buffer"));
    call
}

/// `__buf_finalize` consumes the buffer and yields the final `String`.
fn finalize_intrinsic_call(line: usize, args: Vec<Spanned<Expr>>) -> Spanned<Expr> {
    let call = intrinsic_call(line, "__buf_finalize", args);
    call.set_ty(crate::types::Type::Str);
    call
}

/// Run the full buffer-build deforestation pass on a program: detect
/// sinks, synthesize buffered variants, rewrite fusion sites in place,
/// and APPEND the synthesized FnDefs to the items list as new
/// top-level fns. Caller is responsible for invoking this AFTER
/// `tco::transform_program` (the detector requires `Expr::TailCall`
/// nodes) and BEFORE `resolver::resolve_program` (the detector +
/// rewrite both match on `Expr::Ident` shapes that the resolver
/// rewrites to `Expr::Resolved`).
///
/// Returns a [`BufferBuildPassReport`] describing what fired, for
/// diagnostic / bench reporting and `--explain-passes`.
pub fn run_buffer_build_pass(items: &mut Vec<crate::ast::TopLevel>) -> BufferBuildPassReport {
    let fn_refs: Vec<&FnDef> = items
        .iter()
        .filter_map(|it| match it {
            crate::ast::TopLevel::FnDef(fd) => Some(fd),
            _ => None,
        })
        .collect();
    let all_sinks = compute_buffer_build_sinks(&fn_refs);
    if all_sinks.is_empty() {
        return BufferBuildPassReport::default();
    }
    // Same namespace rule as the list-build pass beside this one: the
    // rewrite emits `__buf_*` intrinsic calls by bare name into caller
    // and variant bodies, and the variant binds `__buf` / `__sep` of
    // its own. A leading `__` is not reserved, so a program can bind
    // any of those names — a binder is exactly what would capture the
    // emitted call (or hand the separator read the loop's element) for
    // the code underneath it. One name anywhere in the program's own
    // binders takes the namespace away from every site at once, and
    // the pass steps aside.
    let taken = crate::ir::chars_fusion::taken_names(items);
    let namespace_taken = taken
        .iter()
        .any(|name| name.starts_with("__buf_") || name == "__buf" || name == "__sep");
    if namespace_taken {
        return BufferBuildPassReport::default();
    }
    let sites = find_fusion_sites(&fn_refs, &all_sinks);

    // Synthesize a buffered variant only for sinks that actually have
    // at least one rewriteable call site. The earlier shape produced
    // a `<sink>__buffered` for every detected sink — bloat in the
    // common case (most detected sinks aren't called via the canonical
    // String.join shape) and a real risk of name-shadowing a user fn
    // named `<sink>__buffered`. Restricting to used sinks keeps the
    // synthetic surface tight.
    let mut used_sinks: HashMap<String, BufferBuildShape> = HashMap::new();
    for site in &sites {
        if let Some(shape) = all_sinks.get(&site.sink_fn) {
            used_sinks.insert(site.sink_fn.clone(), shape.clone());
        }
    }
    let synthesized = synthesize_buffered_variants(&fn_refs, &used_sinks);
    let sinks = used_sinks;
    drop(fn_refs);

    let mut fn_defs_owned: Vec<&mut FnDef> = items
        .iter_mut()
        .filter_map(|it| match it {
            crate::ast::TopLevel::FnDef(fd) => Some(fd),
            _ => None,
        })
        .collect();
    // rewrite_fusion_sites takes &mut [FnDef], so pull a fresh
    // mutable view across owned slots. We can't pass &mut [&mut FnDef]
    // directly — instead, walk and rewrite each fn body individually.
    for fd in fn_defs_owned.iter_mut() {
        rewrite_one_fn(fd, &sinks);
    }

    items.reserve(synthesized.len());
    for fd in synthesized.iter() {
        items.push(crate::ast::TopLevel::FnDef(fd.clone()));
    }

    let mut sink_fns: Vec<String> = sinks.keys().cloned().collect();
    sink_fns.sort();
    let synthesized_fns: Vec<String> = synthesized.iter().map(|fd| fd.name.clone()).collect();

    let mut rewrites_by_sink: std::collections::BTreeMap<String, usize> =
        std::collections::BTreeMap::new();
    for site in &sites {
        *rewrites_by_sink.entry(site.sink_fn.clone()).or_default() += 1;
    }

    BufferBuildPassReport {
        rewrites: sites.len(),
        synthesized: synthesized_fns,
        sink_fns,
        rewrites_by_sink,
    }
}

/// Per-pass report — what buffer_build did during a single pipeline run.
/// Drives `aver compile --explain-passes`; consumed by the bench
/// regression checks (e.g. "fail if buffer_build no longer fires on the
/// canonical shape").
#[derive(Debug, Clone, Default)]
pub struct BufferBuildPassReport {
    /// Number of fusion sites rewritten in place.
    pub rewrites: usize,
    /// Names of synthesized `<sink>__buffered` variants appended to
    /// the items list.
    pub synthesized: Vec<String>,
    /// Sink fns whose buffered variant actually fired (matches one of
    /// `synthesized` minus the `__buffered` suffix). Sorted.
    pub sink_fns: Vec<String>,
    /// Per-sink rewrite counts. Sorted alphabetically by sink fn.
    pub rewrites_by_sink: std::collections::BTreeMap<String, usize>,
}

/// Apply fusion-site rewrite to a single fn body. Internal helper
/// for `run_buffer_build_pass` since `rewrite_fusion_sites` takes a
/// slice and we have an iterator-of-mut-refs here.
fn rewrite_one_fn(fd: &mut FnDef, sinks: &HashMap<String, BufferBuildShape>) {
    let body_arc = std::sync::Arc::make_mut(&mut fd.body);
    let FnBody::Block(stmts) = body_arc;
    for stmt in stmts.iter_mut() {
        match stmt {
            Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => {
                rewrite_expr_in_place(expr, sinks);
            }
        }
    }
}

/// Walk every expression in `fn_defs` and rewrite `String.join`
/// fusion sites in place: `String.join(matched_fn(args, []), sep)` →
/// `__buf_finalize(matched_fn__buffered(args_without_acc, __buf_new(8192), sep))`.
///
/// Conservative trigger per the C' review: only fires when the
/// acc-position arg is a literal `Expr::List([])`. A non-empty
/// initial accumulator would silently lose elements after rewrite,
/// so we skip in that case.
///
/// The rewrite is recursive: nested fusion sites (a fusion site
/// inside another fusion site's args) all get rewritten in one pass.
pub fn rewrite_fusion_sites(fn_defs: &mut [FnDef], sinks: &HashMap<String, BufferBuildShape>) {
    if sinks.is_empty() {
        return;
    }
    for fd in fn_defs.iter_mut() {
        let body_arc = std::sync::Arc::make_mut(&mut fd.body);
        let FnBody::Block(stmts) = body_arc;
        for stmt in stmts.iter_mut() {
            match stmt {
                Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => {
                    rewrite_expr_in_place(expr, sinks);
                }
            }
        }
    }
}

/// Recursive expression-tree walker that rewrites fusion sites in
/// place. Rewrite is "outermost first" — if the whole expression is
/// a fusion site, transform it before descending into the new shape's
/// children, so we don't double-rewrite.
fn rewrite_expr_in_place(expr: &mut Spanned<Expr>, sinks: &HashMap<String, BufferBuildShape>) {
    if let Some(replacement) = try_rewrite_fusion_site(expr, sinks) {
        *expr = replacement;
        // The replacement contains the original elem expressions
        // (possibly themselves containing fusion sites in deep
        // gradient builders). Recurse into the new tree.
        descend_into_subexprs(expr, sinks);
        return;
    }
    descend_into_subexprs(expr, sinks);
}

/// Recurse into the children of an Expr, applying `rewrite_expr_in_place`
/// to each. Mirrors the shape coverage of `walk_expr_for_fusion_sites`
/// in this module so we don't miss any node kind.
fn descend_into_subexprs(expr: &mut Spanned<Expr>, sinks: &HashMap<String, BufferBuildShape>) {
    match &mut expr.node {
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } | Expr::Constructor(_, None) => {}
        Expr::Constructor(_, Some(inner)) | Expr::Attr(inner, _) | Expr::ErrorProp(inner) => {
            rewrite_expr_in_place(inner, sinks);
        }
        Expr::FnCall(callee, args) => {
            rewrite_expr_in_place(callee, sinks);
            for a in args.iter_mut() {
                rewrite_expr_in_place(a, sinks);
            }
        }
        Expr::TailCall(data) => {
            for a in data.args.iter_mut() {
                rewrite_expr_in_place(a, sinks);
            }
        }
        Expr::BinOp(_, l, r) => {
            rewrite_expr_in_place(l, sinks);
            rewrite_expr_in_place(r, sinks);
        }
        Expr::Neg(inner) => rewrite_expr_in_place(inner, sinks),
        Expr::Match { subject, arms } => {
            rewrite_expr_in_place(subject, sinks);
            for arm in arms.iter_mut() {
                rewrite_expr_in_place(&mut arm.body, sinks);
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for it in items.iter_mut() {
                rewrite_expr_in_place(it, sinks);
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries.iter_mut() {
                rewrite_expr_in_place(k, sinks);
                rewrite_expr_in_place(v, sinks);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, v) in fields.iter_mut() {
                rewrite_expr_in_place(v, sinks);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            rewrite_expr_in_place(base, sinks);
            for (_, v) in updates.iter_mut() {
                rewrite_expr_in_place(v, sinks);
            }
        }
        Expr::InterpolatedStr(parts) => {
            for part in parts.iter_mut() {
                if let crate::ast::StrPart::Parsed(inner) = part {
                    rewrite_expr_in_place(inner, sinks);
                }
            }
        }
    }
}

/// If `expr` is a `String.join(matched_fn(args, []), sep)` with
/// matched_fn in `sinks` and acc-position arg a literal empty list,
/// return the rewritten Spanned<Expr>. Else return None.
fn try_rewrite_fusion_site(
    expr: &Spanned<Expr>,
    sinks: &HashMap<String, BufferBuildShape>,
) -> Option<Spanned<Expr>> {
    let line = expr.line;

    // Match the same predicate used by `find_fusion_sites` so the
    // diagnostic count and the rewrite count are guaranteed equal.
    let sink_name = match_string_join_fusion_site(&expr.node, sinks)?;
    let shape = sinks.get(&sink_name)?;

    // Re-extract the inner sink call and its args. The match predicate
    // above already verified the shape; this is just to recover the
    // pieces we need to assemble the rewrite.
    let outer_args = match &expr.node {
        Expr::FnCall(_, a) => a,
        _ => return None,
    };
    let consumer_arg = &outer_args[0].node;
    let inner_call_expr = if let Expr::FnCall(rev_callee, rev_args) = consumer_arg
        && is_dotted_ident(&rev_callee.node, "List", "reverse")
        && rev_args.len() == 1
    {
        &rev_args[0].node
    } else {
        consumer_arg
    };
    let inner_args = match inner_call_expr {
        Expr::FnCall(_, a) => a,
        _ => return None,
    };

    // Build the rewrite:
    //   __buf_finalize(
    //     <fn>__buffered(
    //       <args without acc-pos>,
    //       __buf_new(8192),
    //       <sep>
    //     )
    //   )
    let sep_expr = outer_args[1].clone();
    let buf_new = buffer_intrinsic_call(
        line,
        "__buf_new",
        vec![sp_at_typed(
            line,
            Expr::Literal(Literal::Int(8192)),
            crate::types::Type::Int,
        )],
    );
    let mut buffered_args: Vec<Spanned<Expr>> = inner_args
        .iter()
        .enumerate()
        .filter_map(|(i, a)| (i != shape.acc_param_idx).then_some(a).cloned())
        .collect();
    buffered_args.push(buf_new);
    buffered_args.push(sep_expr);
    // `<sink>__buffered` returns `String` (the buffered variant signature
    // in `synthesize_buffered_fn` types it that way).
    let buffered_call = sp_at_typed(
        line,
        Expr::FnCall(
            Box::new(sp_at(line, Expr::Ident(format!("{}__buffered", sink_name)))),
            buffered_args,
        ),
        crate::types::Type::Str,
    );
    Some(finalize_intrinsic_call(line, vec![buffered_call]))
}

/// Construct the buffered FnDef for a single matched fn. Returns
/// `None` if the original body shape doesn't match what we expect
/// (defensive: detection should have caught this, but if the body
/// changed shape between detection and synthesis, skip).
fn build_buffered_variant(fd: &FnDef, shape: &BufferBuildShape) -> Option<FnDef> {
    // Original body: `match <subject> { <terminating-arm>; <recursive-arm> }`.
    // The BufferBuildKind variants pair different patterns:
    //   InternalReverse:     `true -> List.reverse(acc)`, `false -> recurse(...)`.
    //   ExternalReverse:     `[] -> acc`, `[head, ..rest] -> recurse(...)`.
    //   ListInternalReverse: `[] -> List.reverse(acc)`, `[head, ..rest] -> recurse(...)`.
    // We extract the recursive arm in every case (for the prepend tail
    // call) and rebuild the match with terminating arm `... -> __buf`.
    // The two list-driven kinds are indistinguishable from here on: the
    // base arm they differ in is exactly the arm the rewrite replaces
    // with a bare buffer, and both drop the reverse for the same reason.
    let stmts = fd.body.stmts();
    if stmts.len() != 1 {
        return None;
    }
    let outer_expr = match &stmts[0] {
        Stmt::Expr(spanned) => spanned,
        _ => return None,
    };
    let (subject_orig, arms_orig) = match &outer_expr.node {
        Expr::Match { subject, arms } => (subject, arms),
        _ => return None,
    };
    let recursive_body: &Spanned<Expr> = match shape.kind {
        BufferBuildKind::InternalReverse => arms_orig
            .iter()
            .find(|a| matches!(a.pattern, Pattern::Literal(Literal::Bool(false))))
            .map(|a| a.body.as_ref())?,
        BufferBuildKind::ExternalReverse | BufferBuildKind::ListInternalReverse => arms_orig
            .iter()
            .find(|a| matches!(a.pattern, Pattern::Cons(_, _)))
            .map(|a| a.body.as_ref())?,
    };
    let tail_data = match &recursive_body.node {
        Expr::TailCall(data) => data,
        _ => return None,
    };

    // The acc-position arg in the original tail call is
    // `List.prepend(<elem>, acc)`. Extract the element expression.
    let acc_arg_orig = tail_data.args.get(shape.acc_param_idx)?;
    let elem_expr = match &acc_arg_orig.node {
        Expr::FnCall(callee, args) => {
            if !is_dotted_ident(&callee.node, "List", "prepend") {
                return None;
            }
            if args.len() != 2 {
                return None;
            }
            // args[0] is elem, args[1] is acc ident — verify acc.
            match &args[1].node {
                Expr::Ident(name) if name == &shape.acc_param_name => {}
                _ => return None,
            }
            args[0].clone()
        }
        _ => return None,
    };

    let line = fd.line;
    let buf_name = "__buf";
    let sep_name = "__sep";
    let buffered_target = format!("{}__buffered", fd.name);

    // Synthesized false arm body:
    //   <self>__buffered(<orig args minus acc>, __buf_append(<sep_unless_first>, <elem>), __sep)
    //
    // Build the buffer-threading expression first: the inner intrinsic
    // appends `__sep` if the buffer is non-empty (otherwise no-op),
    // returning the possibly-grown buffer. The outer intrinsic appends
    // the user's element. The result is what gets passed as the
    // buffered variant's `__buf` arg in the recursive call.
    let buffer_ty = crate::types::Type::named("Buffer");
    let buf_ident = || sp_at_typed(line, Expr::Ident(buf_name.to_string()), buffer_ty.clone());
    let sep_ident = || {
        sp_at_typed(
            line,
            Expr::Ident(sep_name.to_string()),
            crate::types::Type::Str,
        )
    };
    let sep_then_buf = buffer_intrinsic_call(
        line,
        "__buf_append_sep_unless_first",
        vec![buf_ident(), sep_ident()],
    );
    let final_buf = buffer_intrinsic_call(line, "__buf_append", vec![sep_then_buf, elem_expr]);

    // Build new tail-call args: original args with acc-pos replaced by
    // the threaded buffer expression, then `__sep` appended at end.
    let mut new_args: Vec<Spanned<Expr>> = tail_data
        .args
        .iter()
        .enumerate()
        .map(|(i, a)| {
            if i == shape.acc_param_idx {
                final_buf.clone()
            } else {
                a.clone()
            }
        })
        .collect();
    new_args.push(sep_ident());

    // The recursive tail-call lands on `<fn>__buffered`, which returns
    // Buffer; stamp the type so the typed WASM emitter doesn't have to
    // re-derive it from the call target.
    let new_recursive_body = sp_at_typed(
        line,
        Expr::TailCall(Box::new(TailCallData {
            target: buffered_target.clone(),
            args: new_args,
        })),
        buffer_ty.clone(),
    );

    // Terminating arm body: just return `__buf` — the buffer IS the result.
    // Pattern depends on which sink shape we matched: `true` for the
    // Bool-driven InternalReverse idiom, `[]` for both list-driven ones.
    let new_arms = match shape.kind {
        BufferBuildKind::InternalReverse => vec![
            MatchArm {
                pattern: Pattern::Literal(Literal::Bool(true)),
                body: Box::new(buf_ident()),
                binding_slots: std::sync::OnceLock::new(),
            },
            MatchArm {
                pattern: Pattern::Literal(Literal::Bool(false)),
                body: Box::new(new_recursive_body),
                binding_slots: std::sync::OnceLock::new(),
            },
        ],
        BufferBuildKind::ExternalReverse | BufferBuildKind::ListInternalReverse => {
            // Re-use the cons binding names from the original arm so
            // any `head` / `rest` references inside the recursive body
            // continue to resolve.
            let cons_pat = arms_orig
                .iter()
                .find_map(|a| match &a.pattern {
                    Pattern::Cons(h, t) => Some(Pattern::Cons(h.clone(), t.clone())),
                    _ => None,
                })
                .unwrap_or(Pattern::Cons("__head".to_string(), "__tail".to_string()));
            vec![
                MatchArm {
                    pattern: Pattern::EmptyList,
                    body: Box::new(buf_ident()),
                    binding_slots: std::sync::OnceLock::new(),
                },
                MatchArm {
                    pattern: cons_pat,
                    body: Box::new(new_recursive_body),
                    binding_slots: std::sync::OnceLock::new(),
                },
            ]
        }
    };

    // Synthesised Match returns Buffer — both arms produce one
    // (terminating arm: bare `__buf` ident; recursive arm: a
    // `<fn>__buffered` tail-call returning Buffer). Stamp it so the
    // Step 2 type-driven WASM emitter can read the type without a
    // fallback.
    let new_match = sp_at_typed(
        line,
        Expr::Match {
            subject: subject_orig.clone(),
            arms: new_arms,
        },
        crate::types::Type::named("Buffer"),
    );

    let new_body = FnBody::Block(vec![Stmt::Expr(new_match)]);

    // Params: original minus acc + (__buf, "Buffer") + (__sep, "String").
    let mut new_params: Vec<(String, String)> = fd
        .params
        .iter()
        .enumerate()
        .filter_map(|(i, p)| (i != shape.acc_param_idx).then_some(p).cloned())
        .collect();
    new_params.push((buf_name.to_string(), "Buffer".to_string()));
    new_params.push((sep_name.to_string(), "String".to_string()));

    Some(FnDef {
        name: buffered_target,
        line,
        params: new_params,
        return_type: "Buffer".to_string(),
        // Synthesized variants inherit effects from the original — if
        // the matched fn calls effectful helpers (like `renderRow`
        // calling `Console.print`), the buffered variant calls them
        // too at the same positions. Conservative.
        effects: fd.effects.clone(),
        desc: Some(format!(
            "Synthesized buffered variant of `{}` for deforestation \
             lowering. Call sites that match `String.join({}(...), sep)` \
             are rewritten to alloc a buffer + call this variant + \
             finalize, skipping the intermediate List.",
            fd.name, fd.name
        )),
        body: Arc::new(new_body),
        resolution: None,
    })
}

/// The same loop, kept as a list — the collecting loop whose
/// accumulator is the answer rather than something `String.join`
/// consumes. Built on the recognisers above, which is why it is a
/// sibling rather than a module of its own.
mod list_build;

#[cfg(test)]
mod tests;

pub use list_build::{
    ListBuildDecline, ListBuildKind, ListBuildPassReport, ListBuildShape, has_list_build_shape,
    run_list_build_pass,
};

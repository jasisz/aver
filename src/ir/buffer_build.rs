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

use crate::ast::{Expr, FnBody, FnDef, Literal, MatchArm, Pattern, Spanned, Stmt, TailCallData};

/// Information about a fn that matches the buffer-build sink shape.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BufferBuildShape {
    /// 0-based index of the `acc: List<T>` parameter in the fn signature.
    /// Identifies which arg in tail-call positions threads the
    /// accumulator and which `Ident` in the `true` arm is the reversed
    /// return value.
    pub acc_param_idx: usize,
    /// The accumulator parameter's binding name (looked up in tail-call
    /// args and in the `List.reverse(<name>)` return).
    pub acc_param_name: String,
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
    if let Expr::FnCall(callee, args) = expr {
        // Is this `String.join(<inner>, _)`?
        if is_dotted_ident(&callee.node, "String", "join") && args.len() == 2 {
            // Is the first argument a call to one of the matched sinks?
            if let Expr::FnCall(inner_callee, _) = &args[0].node {
                if let Expr::Ident(inner_name) = &inner_callee.node {
                    if sinks.contains_key(inner_name) {
                        out.push(FusionSite {
                            enclosing_fn: enclosing_fn.to_string(),
                            line: expr_line,
                            sink_fn: inner_name.clone(),
                            consumer: ConsumerKind::StringJoin,
                        });
                    }
                }
            }
        }
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
        if s.line > 0 {
            s.line
        } else {
            fallback_line
        }
    };
    match expr {
        Expr::Literal(_)
        | Expr::Ident(_)
        | Expr::Resolved { .. }
        | Expr::Constructor(_, None) => {}
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
        Expr::Match { subject, arms } => {
            walk_expr_for_fusion_sites(
                &subject.node,
                line_of(subject),
                enclosing_fn,
                sinks,
                out,
            );
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
    let (acc_idx, acc_name) = fd
        .params
        .iter()
        .enumerate()
        .find(|(_, (_, ty))| is_list_type_str(ty))
        .map(|(i, (name, _))| (i, name.clone()))?;

    // Body must be a single expression statement holding the match.
    let match_expr = single_match_body(&fd.body)?;
    let (subject_expr, arms) = match match_expr {
        Expr::Match { subject, arms } => (subject, arms),
        _ => return None,
    };

    // Subject is some boolean condition; we don't constrain its shape,
    // only that the two arms cover `true` and `false`.
    let _ = subject_expr;
    let (true_body, false_body) = pair_bool_arms(arms)?;

    // True arm: `List.reverse(<acc>)`.
    if !is_list_reverse_of(true_body, &acc_name) {
        return None;
    }

    // False arm: tail-call to self with one arg being
    // `List.prepend(<anything>, <acc>)`.
    if !is_self_tail_with_prepend_acc(false_body, &fd.name, &acc_name) {
        return None;
    }

    Some(BufferBuildShape {
        acc_param_idx: acc_idx,
        acc_param_name: acc_name,
    })
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

/// True if `expr` is a tail-call to `self_name` whose argument list
/// contains `List.prepend(<anything>, <Ident(acc_name)>)` in any
/// position. The position should match the `acc_param_idx` but the
/// caller may have other params before it; we only require the
/// `prepend` to terminate in the expected accumulator binding.
fn is_self_tail_with_prepend_acc(expr: &Expr, self_name: &str, acc_name: &str) -> bool {
    let data = match expr {
        Expr::TailCall(data) => data,
        _ => return false,
    };
    if data.target != self_name {
        return false;
    }
    data.args
        .iter()
        .any(|arg| is_list_prepend_to_acc(&arg.node, acc_name))
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
        if let Some(shape) = sinks.get(&fd.name) {
            if let Some(buffered) = build_buffered_variant(fd, shape) {
                out.push(buffered);
            }
        }
    }
    out
}

/// Wrap an `Expr` as `Spanned<Expr>` carrying the same line as the
/// matched fn (best effort — the synthesized code is internal and
/// won't be source-located by the user, but having a non-zero line
/// keeps downstream visitors happy).
fn sp_at(line: usize, expr: Expr) -> Spanned<Expr> {
    Spanned { node: expr, line }
}

/// Build `<Module>.<member>(args...)` as a Spanned<Expr>.
fn dotted_call(line: usize, module: &str, member: &str, args: Vec<Spanned<Expr>>) -> Spanned<Expr> {
    let callee = sp_at(
        line,
        Expr::Attr(
            Box::new(sp_at(line, Expr::Ident(module.to_string()))),
            member.to_string(),
        ),
    );
    sp_at(line, Expr::FnCall(Box::new(callee), args))
}

/// Build `<intrinsic>(args...)` as a Spanned<Expr>. Intrinsic names
/// are bare identifiers (no module dot) — `__buf_append`,
/// `__buf_append_sep_unless_first`. The WASM emitter recognises them
/// in the builtin dispatch.
fn intrinsic_call(line: usize, name: &str, args: Vec<Spanned<Expr>>) -> Spanned<Expr> {
    let callee = sp_at(line, Expr::Ident(name.to_string()));
    sp_at(line, Expr::FnCall(Box::new(callee), args))
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
/// Returns the count of fusion sites rewritten + buffered variants
/// synthesized for diagnostic / bench reporting.
pub fn run_buffer_build_pass(items: &mut Vec<crate::ast::TopLevel>) -> (usize, usize) {
    let fn_refs: Vec<&FnDef> = items
        .iter()
        .filter_map(|it| match it {
            crate::ast::TopLevel::FnDef(fd) => Some(fd),
            _ => None,
        })
        .collect();
    let sinks = compute_buffer_build_sinks(&fn_refs);
    if sinks.is_empty() {
        return (0, 0);
    }
    let sites = find_fusion_sites(&fn_refs, &sinks);
    let synthesized = synthesize_buffered_variants(&fn_refs, &sinks);
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

    (sites.len(), synthesized.len())
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
pub fn rewrite_fusion_sites(
    fn_defs: &mut [FnDef],
    sinks: &HashMap<String, BufferBuildShape>,
) {
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
fn rewrite_expr_in_place(
    expr: &mut Spanned<Expr>,
    sinks: &HashMap<String, BufferBuildShape>,
) {
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
fn descend_into_subexprs(
    expr: &mut Spanned<Expr>,
    sinks: &HashMap<String, BufferBuildShape>,
) {
    match &mut expr.node {
        Expr::Literal(_)
        | Expr::Ident(_)
        | Expr::Resolved { .. }
        | Expr::Constructor(_, None) => {}
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
    // Outer must be `String.join(_, _)`.
    let (outer_callee, outer_args) = match &expr.node {
        Expr::FnCall(c, a) => (c, a),
        _ => return None,
    };
    if !is_dotted_ident(&outer_callee.node, "String", "join") {
        return None;
    }
    if outer_args.len() != 2 {
        return None;
    }
    // First arg of String.join must be a call to a matched fn.
    let (inner_callee, inner_args) = match &outer_args[0].node {
        Expr::FnCall(c, a) => (c, a),
        _ => return None,
    };
    let sink_name = match &inner_callee.node {
        Expr::Ident(name) => name.clone(),
        _ => return None,
    };
    let shape = sinks.get(&sink_name)?;
    // Acc-position arg must be a literal empty List. Otherwise the
    // initial accumulator carries elements that the buffered variant
    // would drop on the floor.
    let acc_arg = inner_args.get(shape.acc_param_idx)?;
    let is_empty_list = matches!(&acc_arg.node, Expr::List(items) if items.is_empty());
    if !is_empty_list {
        return None;
    }
    // Build the rewrite:
    //   __buf_finalize(
    //     <fn>__buffered(
    //       <args without acc-pos>,
    //       __buf_new(8192),
    //       <sep>
    //     )
    //   )
    let sep_expr = outer_args[1].clone();
    let buf_new = intrinsic_call(
        line,
        "__buf_new",
        vec![sp_at(line, Expr::Literal(Literal::Int(8192)))],
    );
    let mut buffered_args: Vec<Spanned<Expr>> = inner_args
        .iter()
        .enumerate()
        .filter_map(|(i, a)| (i != shape.acc_param_idx).then(|| a.clone()))
        .collect();
    buffered_args.push(buf_new);
    buffered_args.push(sep_expr);
    let buffered_call = sp_at(
        line,
        Expr::FnCall(
            Box::new(sp_at(
                line,
                Expr::Ident(format!("{}__buffered", sink_name)),
            )),
            buffered_args,
        ),
    );
    Some(intrinsic_call(line, "__buf_finalize", vec![buffered_call]))
}

/// Construct the buffered FnDef for a single matched fn. Returns
/// `None` if the original body shape doesn't match what we expect
/// (defensive: detection should have caught this, but if the body
/// changed shape between detection and synthesis, skip).
fn build_buffered_variant(fd: &FnDef, shape: &BufferBuildShape) -> Option<FnDef> {
    // Original body: `match cond { true → List.reverse(acc); false → tail-call }`.
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
    // Find the false arm to extract the prepend element + tail-call args.
    let mut false_body: Option<&Spanned<Expr>> = None;
    for arm in arms_orig {
        if matches!(arm.pattern, Pattern::Literal(Literal::Bool(false))) {
            false_body = Some(&arm.body);
        }
    }
    let false_expr = false_body?;
    let tail_data = match &false_expr.node {
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
    let buf_ident = || sp_at(line, Expr::Ident(buf_name.to_string()));
    let sep_ident = || sp_at(line, Expr::Ident(sep_name.to_string()));
    let sep_then_buf = intrinsic_call(
        line,
        "__buf_append_sep_unless_first",
        vec![buf_ident(), sep_ident()],
    );
    let final_buf = intrinsic_call(line, "__buf_append", vec![sep_then_buf, elem_expr]);

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

    let new_false_body = sp_at(
        line,
        Expr::TailCall(Box::new(TailCallData {
            target: buffered_target.clone(),
            args: new_args,
        })),
    );

    // True arm body: just return `__buf` — the buffer IS the result.
    let new_true_body = buf_ident();

    let new_arms = vec![
        MatchArm {
            pattern: Pattern::Literal(Literal::Bool(true)),
            body: Box::new(new_true_body),
        },
        MatchArm {
            pattern: Pattern::Literal(Literal::Bool(false)),
            body: Box::new(new_false_body),
        },
    ];

    let new_match = sp_at(
        line,
        Expr::Match {
            subject: subject_orig.clone(),
            arms: new_arms,
        },
    );

    let new_body = FnBody::Block(vec![Stmt::Expr(new_match)]);

    // Params: original minus acc + (__buf, "Buffer") + (__sep, "String").
    let mut new_params: Vec<(String, String)> = fd
        .params
        .iter()
        .enumerate()
        .filter_map(|(i, p)| (i != shape.acc_param_idx).then(|| p.clone()))
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinOp, FnBody, FnDef, Literal, Spanned, TailCallData};
    use std::sync::Arc;

    fn sp<T>(value: T) -> Spanned<T> {
        Spanned {
            node: value,
            line: 1,
        }
    }

    fn ident(name: &str) -> Spanned<Expr> {
        sp(Expr::Ident(name.to_string()))
    }

    fn dotted(module: &str, member: &str) -> Spanned<Expr> {
        sp(Expr::Attr(
            Box::new(ident(module)),
            member.to_string(),
        ))
    }

    fn call(callee: Spanned<Expr>, args: Vec<Spanned<Expr>>) -> Spanned<Expr> {
        sp(Expr::FnCall(Box::new(callee), args))
    }

    /// Build a canonical buffer-build fn: takes (col: Int, acc: List<Int>),
    /// matches col >= 10, true → reverse(acc), false → tail-call self
    /// with prepend(col, acc).
    fn canonical_builder(name: &str) -> FnDef {
        let true_body = call(dotted("List", "reverse"), vec![ident("acc")]);
        let prepend = call(
            dotted("List", "prepend"),
            vec![ident("col"), ident("acc")],
        );
        let false_body = sp(Expr::TailCall(Box::new(TailCallData {
            target: name.to_string(),
            args: vec![
                sp(Expr::BinOp(
                    BinOp::Add,
                    Box::new(ident("col")),
                    Box::new(sp(Expr::Literal(Literal::Int(1)))),
                )),
                prepend,
            ],
        })));
        let match_expr = sp(Expr::Match {
            subject: Box::new(sp(Expr::BinOp(
                BinOp::Gte,
                Box::new(ident("col")),
                Box::new(sp(Expr::Literal(Literal::Int(10)))),
            ))),
            arms: vec![
                MatchArm {
                    pattern: Pattern::Literal(Literal::Bool(true)),
                    body: Box::new(true_body),
                },
                MatchArm {
                    pattern: Pattern::Literal(Literal::Bool(false)),
                    body: Box::new(false_body),
                },
            ],
        });
        FnDef {
            name: name.to_string(),
            line: 1,
            params: vec![
                ("col".to_string(), "Int".to_string()),
                ("acc".to_string(), "List<Int>".to_string()),
            ],
            return_type: "List<Int>".to_string(),
            effects: vec![],
            desc: None,
            body: Arc::new(FnBody::Block(vec![Stmt::Expr(match_expr)])),
            resolution: None,
        }
    }

    #[test]
    fn matches_canonical_buffer_build() {
        let fd = canonical_builder("build");
        let info = compute_buffer_build_sinks(&[&fd]);
        let shape = info.get("build").expect("expected match");
        assert_eq!(shape.acc_param_idx, 1);
        assert_eq!(shape.acc_param_name, "acc");
    }

    #[test]
    fn rejects_fn_without_list_param() {
        let mut fd = canonical_builder("build");
        // Strip the List<...> param.
        fd.params = vec![("col".to_string(), "Int".to_string())];
        let info = compute_buffer_build_sinks(&[&fd]);
        assert!(info.is_empty(), "fn without List param should not match");
    }

    #[test]
    fn rejects_when_true_arm_isnt_reverse() {
        let mut fd = canonical_builder("build");
        // Replace true arm body with a different expression.
        if let FnBody::Block(stmts) = Arc::make_mut(&mut fd.body) {
            if let Stmt::Expr(spanned) = &mut stmts[0] {
                if let Expr::Match { arms, .. } = &mut spanned.node {
                    arms[0].body = Box::new(ident("acc"));
                }
            }
        }
        let info = compute_buffer_build_sinks(&[&fd]);
        assert!(
            info.is_empty(),
            "fn returning bare acc instead of reverse should not match"
        );
    }

    #[test]
    fn rejects_when_false_arm_uses_append_not_prepend() {
        let mut fd = canonical_builder("build");
        // Swap List.prepend → List.append in the false arm tail call.
        if let FnBody::Block(stmts) = Arc::make_mut(&mut fd.body) {
            if let Stmt::Expr(spanned) = &mut stmts[0] {
                if let Expr::Match { arms, .. } = &mut spanned.node {
                    let false_body = arms[1].body.as_mut();
                    if let Expr::TailCall(data) = &mut false_body.node {
                        if let Expr::FnCall(callee, _) = &mut data.args[1].node {
                            if let Expr::Attr(_, attr) = &mut callee.node {
                                *attr = "append".to_string();
                            }
                        }
                    }
                }
            }
        }
        let info = compute_buffer_build_sinks(&[&fd]);
        assert!(
            info.is_empty(),
            "fn using List.append instead of prepend should not match"
        );
    }

    #[test]
    fn rejects_tail_call_to_different_fn() {
        let mut fd = canonical_builder("build");
        if let FnBody::Block(stmts) = Arc::make_mut(&mut fd.body) {
            if let Stmt::Expr(spanned) = &mut stmts[0] {
                if let Expr::Match { arms, .. } = &mut spanned.node {
                    let false_body = arms[1].body.as_mut();
                    if let Expr::TailCall(data) = &mut false_body.node {
                        data.target = "someone_else".to_string();
                    }
                }
            }
        }
        let info = compute_buffer_build_sinks(&[&fd]);
        assert!(
            info.is_empty(),
            "fn whose recursive call targets a different name should not match"
        );
    }

    #[test]
    fn rejects_match_with_non_bool_arms() {
        let mut fd = canonical_builder("build");
        if let FnBody::Block(stmts) = Arc::make_mut(&mut fd.body) {
            if let Stmt::Expr(spanned) = &mut stmts[0] {
                if let Expr::Match { arms, .. } = &mut spanned.node {
                    arms[0].pattern = Pattern::Literal(Literal::Int(0));
                }
            }
        }
        let info = compute_buffer_build_sinks(&[&fd]);
        assert!(
            info.is_empty(),
            "match on non-bool patterns should not be detected as buffer-build"
        );
    }

    /// End-to-end: parse a small Aver source, run TCO, then detect.
    /// The TCO transform is what produces `Expr::TailCall` nodes from
    /// raw `Expr::FnCall` self-recursion; detection runs on the post-TCO
    /// AST.
    #[test]
    fn detects_via_parser_after_tco() {
        let src = r#"
fn build(n: Int, acc: List<Int>) -> List<Int>
    match n <= 0
        true  -> List.reverse(acc)
        false -> build(n - 1, List.prepend(n, acc))
"#;
        let mut lexer = crate::lexer::Lexer::new(src);
        let tokens = lexer.tokenize().expect("lex");
        let mut parser = crate::parser::Parser::new(tokens);
        let mut items = parser.parse().expect("parse");
        crate::tco::transform_program(&mut items);
        let fns: Vec<&FnDef> = items
            .iter()
            .filter_map(|it| match it {
                crate::ast::TopLevel::FnDef(fd) => Some(fd),
                _ => None,
            })
            .collect();
        let info = compute_buffer_build_sinks(&fns);
        let shape = info
            .get("build")
            .expect("expected end-to-end shape match for canonical builder");
        assert_eq!(shape.acc_param_idx, 1);
        assert_eq!(shape.acc_param_name, "acc");
    }

    /// End-to-end fusion-site detection: builder + caller `String.join`
    /// site recognised, line recorded, sink name attached.
    #[test]
    fn finds_fusion_site_via_parser() {
        let src = r#"
fn build(n: Int, acc: List<Int>) -> List<Int>
    match n <= 0
        true  -> List.reverse(acc)
        false -> build(n - 1, List.prepend(n, acc))

fn main() -> String
    String.join(build(5, []), ",")
"#;
        let mut lexer = crate::lexer::Lexer::new(src);
        let tokens = lexer.tokenize().expect("lex");
        let mut parser = crate::parser::Parser::new(tokens);
        let mut items = parser.parse().expect("parse");
        crate::tco::transform_program(&mut items);
        let fns: Vec<&FnDef> = items
            .iter()
            .filter_map(|it| match it {
                crate::ast::TopLevel::FnDef(fd) => Some(fd),
                _ => None,
            })
            .collect();
        let sinks = compute_buffer_build_sinks(&fns);
        let sites = find_fusion_sites(&fns, &sinks);
        assert_eq!(sites.len(), 1, "expected one fusion site, got {sites:?}");
        let site = &sites[0];
        assert_eq!(site.enclosing_fn, "main");
        assert_eq!(site.sink_fn, "build");
        assert!(site.line > 0, "expected real line info, got 0");
    }

    /// Caller passes the matched fn's result to a non-`String.join`
    /// destination — should NOT register as a fusion site (no buffer
    /// to write into).
    #[test]
    fn ignores_call_when_not_wrapped_in_string_join() {
        let src = r#"
fn build(n: Int, acc: List<Int>) -> List<Int>
    match n <= 0
        true  -> List.reverse(acc)
        false -> build(n - 1, List.prepend(n, acc))

fn main() -> List<Int>
    build(5, [])
"#;
        let mut lexer = crate::lexer::Lexer::new(src);
        let tokens = lexer.tokenize().expect("lex");
        let mut parser = crate::parser::Parser::new(tokens);
        let mut items = parser.parse().expect("parse");
        crate::tco::transform_program(&mut items);
        let fns: Vec<&FnDef> = items
            .iter()
            .filter_map(|it| match it {
                crate::ast::TopLevel::FnDef(fd) => Some(fd),
                _ => None,
            })
            .collect();
        let sinks = compute_buffer_build_sinks(&fns);
        let sites = find_fusion_sites(&fns, &sinks);
        assert!(
            sites.is_empty(),
            "build called outside String.join must not be a fusion site, got {sites:?}"
        );
    }

    /// Counter-test: a recursive fn that returns `acc` directly (no
    /// reverse) — semantically valid Aver, but its result order is
    /// reversed relative to natural read order, so deforestation can't
    /// safely rewrite to a forward-emit buffer loop without explicit
    /// authorisation. Detector must reject it.
    #[test]
    fn rejects_via_parser_when_true_arm_returns_bare_acc() {
        let src = r#"
fn build(n: Int, acc: List<Int>) -> List<Int>
    match n <= 0
        true  -> acc
        false -> build(n - 1, List.prepend(n, acc))
"#;
        let mut lexer = crate::lexer::Lexer::new(src);
        let tokens = lexer.tokenize().expect("lex");
        let mut parser = crate::parser::Parser::new(tokens);
        let mut items = parser.parse().expect("parse");
        crate::tco::transform_program(&mut items);
        let fns: Vec<&FnDef> = items
            .iter()
            .filter_map(|it| match it {
                crate::ast::TopLevel::FnDef(fd) => Some(fd),
                _ => None,
            })
            .collect();
        let info = compute_buffer_build_sinks(&fns);
        assert!(
            info.is_empty(),
            "fn returning bare acc must not be detected as a deforestation candidate"
        );
    }

    /// End-to-end synthesis: parse a small builder, run TCO, detect
    /// it as a sink, then synthesize the buffered variant. Verify the
    /// shape: name suffix, dropped acc param, added __buf/__sep
    /// params, true arm returns __buf ident, false arm tail-calls
    /// __buffered self with threaded buffer expression.
    #[test]
    fn synthesizes_buffered_variant_from_real_builder() {
        let src = r#"
fn build(n: Int, acc: List<Int>) -> List<Int>
    match n <= 0
        true  -> List.reverse(acc)
        false -> build(n - 1, List.prepend(n, acc))
"#;
        let mut lexer = crate::lexer::Lexer::new(src);
        let tokens = lexer.tokenize().expect("lex");
        let mut parser = crate::parser::Parser::new(tokens);
        let mut items = parser.parse().expect("parse");
        crate::tco::transform_program(&mut items);
        let fns: Vec<&FnDef> = items
            .iter()
            .filter_map(|it| match it {
                crate::ast::TopLevel::FnDef(fd) => Some(fd),
                _ => None,
            })
            .collect();
        let sinks = compute_buffer_build_sinks(&fns);
        assert!(sinks.contains_key("build"));
        let synthesized = synthesize_buffered_variants(&fns, &sinks);
        assert_eq!(synthesized.len(), 1, "expected exactly one synthesized variant");
        let bf = &synthesized[0];

        // Name + signature shape.
        assert_eq!(bf.name, "build__buffered");
        assert_eq!(bf.return_type, "Buffer");
        let param_names: Vec<&str> = bf.params.iter().map(|(n, _)| n.as_str()).collect();
        let param_types: Vec<&str> = bf.params.iter().map(|(_, t)| t.as_str()).collect();
        assert_eq!(param_names, vec!["n", "__buf", "__sep"]);
        assert_eq!(param_types, vec!["Int", "Buffer", "String"]);

        // Body: single Stmt::Expr holding a 2-arm match.
        let stmts = bf.body.stmts();
        assert_eq!(stmts.len(), 1);
        let match_expr = match &stmts[0] {
            Stmt::Expr(s) => match &s.node {
                Expr::Match { subject: _, arms } => arms,
                _ => panic!("body root must be a match"),
            },
            _ => panic!("body root must be Stmt::Expr"),
        };
        assert_eq!(match_expr.len(), 2);

        // True arm: body is `__buf` ident.
        let true_arm = match_expr
            .iter()
            .find(|a| matches!(a.pattern, Pattern::Literal(Literal::Bool(true))))
            .expect("true arm");
        match &true_arm.body.node {
            Expr::Ident(name) => assert_eq!(name, "__buf"),
            other => panic!("true arm should be Ident(__buf), got {other:?}"),
        }

        // False arm: tail-call to build__buffered with threaded buf.
        let false_arm = match_expr
            .iter()
            .find(|a| matches!(a.pattern, Pattern::Literal(Literal::Bool(false))))
            .expect("false arm");
        let tail_data = match &false_arm.body.node {
            Expr::TailCall(d) => d,
            other => panic!("false arm should be TailCall, got {other:?}"),
        };
        assert_eq!(tail_data.target, "build__buffered");
        // Args: [n - 1, threaded-buffer-expr, __sep_ident]. acc-pos
        // (was index 1 in original) is now the threaded buffer; sep
        // appended at end.
        assert_eq!(tail_data.args.len(), 3);
        // Arg 1 is the buffer-threading composition; verify it's
        // `__buf_append(__buf_append_sep_unless_first(__buf, __sep), n)`.
        let outer = match &tail_data.args[1].node {
            Expr::FnCall(callee, args) => {
                match &callee.node {
                    Expr::Ident(name) => assert_eq!(name, "__buf_append"),
                    _ => panic!("expected Ident callee"),
                }
                args
            }
            _ => panic!("expected outer __buf_append FnCall"),
        };
        assert_eq!(outer.len(), 2);
        // First arg of outer = inner sep-then-buf.
        match &outer[0].node {
            Expr::FnCall(callee, _) => match &callee.node {
                Expr::Ident(name) => assert_eq!(name, "__buf_append_sep_unless_first"),
                _ => panic!("expected Ident callee for inner intrinsic"),
            },
            _ => panic!("expected inner __buf_append_sep_unless_first FnCall"),
        }
        // Second arg of outer = original `n` (the prepend's element).
        match &outer[1].node {
            Expr::Ident(name) => assert_eq!(name, "n"),
            _ => panic!("expected `n` ident as elem"),
        }
        // Last tail-call arg = __sep ident.
        match &tail_data.args[2].node {
            Expr::Ident(name) => assert_eq!(name, "__sep"),
            _ => panic!("expected __sep ident as last arg"),
        }
    }

    #[test]
    fn detects_acc_param_at_arbitrary_index() {
        // Builder where the List<T> param is first, not last.
        let mut fd = canonical_builder("build");
        fd.params = vec![
            ("acc".to_string(), "List<Int>".to_string()),
            ("col".to_string(), "Int".to_string()),
        ];
        let info = compute_buffer_build_sinks(&[&fd]);
        let shape = info.get("build").expect("expected match");
        assert_eq!(shape.acc_param_idx, 0);
        assert_eq!(shape.acc_param_name, "acc");
    }
}

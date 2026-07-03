use std::collections::BTreeSet;

use super::super::expr::aver_name_to_lean;
use crate::ast::{BinOp, Expr, FnBody, FnDef, Literal, Spanned, Stmt, VerifyBlock, VerifyLaw};
use crate::ast_rewrite::rewrite_idents_scoped;
use crate::codegen::CodegenContext;

/// Render an Aver expression to its Lean surface form (the same emitter the
/// theorem statements use), so recognizers can compare atoms structurally.
pub(super) fn render(e: &Spanned<Expr>, ctx: &CodegenContext) -> String {
    super::super::expr::emit_expr_legacy(e, ctx, None)
}

/// Whether two Aver expressions render to the same Lean surface form.
pub(super) fn same_atom(a: &Spanned<Expr>, b: &Spanned<Expr>, ctx: &CodegenContext) -> bool {
    render(a, ctx) == render(b, ctx)
}

/// `floor(x, y)` — a 2-arg call to `floor_src`; returns `(x, y)`.
pub(super) fn floor_call<'a>(
    e: &'a Spanned<Expr>,
    floor_src: &str,
) -> Option<(&'a Spanned<Expr>, &'a Spanned<Expr>)> {
    let Expr::FnCall(callee, args) = &e.node else {
        return None;
    };
    if expr_dotted_name(callee).as_deref() != Some(floor_src) || args.len() != 2 {
        return None;
    }
    Some((&args[0], &args[1]))
}

/// Whether `fd_src` names a Euclidean floor-division fn: its body is
/// `Result.withDefault(Int.div(a, d), 0)` over the two parameters. This is the
/// definition-shape gate that makes the floor identities TRUE — keyed on the
/// body, never on the fn's name. Shared by every floor-division rung.
pub(super) fn is_euclidean_floor_fn(floor_src: &str, ctx: &CodegenContext) -> bool {
    let Some(fd) = find_fn_def_by_call_name(ctx, floor_src) else {
        return false;
    };
    if !fd.effects.is_empty() || fd.params.len() != 2 {
        return false;
    }
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return false;
    };
    // withDefault(Int.div(_, _), 0)
    let Expr::FnCall(callee, args) = &body.node else {
        return false;
    };
    if expr_dotted_name(callee).as_deref() != Some("Result.withDefault") || args.len() != 2 {
        return false;
    }
    if !matches!(&args[1].node, Expr::Literal(Literal::Int(0))) {
        return false;
    }
    let Expr::FnCall(div_callee, div_args) = &args[0].node else {
        return false;
    };
    expr_dotted_name(div_callee).as_deref() == Some("Int.div") && div_args.len() == 2
}

/// Flatten a `Bool.and` / `&&` conjunction of `when` clauses, canonicalizing
/// every leaf comparison to one normal direction (`a > b` -> `b < a`,
/// `a >= b` -> `b <= a`; see [`crate::codegen::common::canonicalize_comparison`]).
/// This is the ONE choke point before the recognition arms below see the
/// clauses: because the operands are pre-swapped here, the arms only ever
/// match `Lt`/`Lte`, so they no longer hand-enumerate the `Gt`/`Gte`
/// direction. Recognition-only — the emitters render `law.when` verbatim, so
/// the user's original surface direction is preserved in the emitted proof.
pub(super) fn flatten_and(e: &Spanned<Expr>, out: &mut Vec<Spanned<Expr>>) {
    match &e.node {
        Expr::FnCall(callee, args)
            if expr_dotted_name(callee).as_deref() == Some("Bool.and") && args.len() == 2 =>
        {
            flatten_and(&args[0], out);
            flatten_and(&args[1], out);
        }
        _ => out.push(crate::codegen::common::canonicalize_comparison(e)),
    }
}

pub(super) fn collect_when_clauses(e: &Spanned<Expr>) -> Vec<Spanned<Expr>> {
    let mut out = Vec::new();
    flatten_and(e, &mut out);
    out
}

/// Callee source-name and args of a call, seeing through the TCO rewrite: a
/// tail-position self/peer call is an `Expr::TailCall` after the proof pipeline
/// runs, not an `Expr::FnCall`.
pub(super) fn call_name_args(e: &Spanned<Expr>) -> Option<(String, &[Spanned<Expr>])> {
    match &e.node {
        Expr::FnCall(callee, args) => Some((expr_dotted_name(callee)?, args.as_slice())),
        Expr::TailCall(tc) => Some((tc.target.clone(), tc.args.as_slice())),
        _ => None,
    }
}

pub(super) fn short_call_name_args(e: &Spanned<Expr>) -> Option<(String, &[Spanned<Expr>])> {
    let (dotted, args) = call_name_args(e)?;
    let short = dotted.rsplit('.').next().unwrap_or(&dotted).to_string();
    Some((short, args))
}

pub(super) fn call_named<'a>(
    expr: &'a Spanned<Expr>,
    name: &str,
    n: usize,
) -> Option<&'a [Spanned<Expr>]> {
    let (short, args) = short_call_name_args(expr)?;
    (short == name && args.len() == n).then_some(args)
}

pub(super) fn call_named_with_dotted<'a>(
    expr: &'a Spanned<Expr>,
    name: &str,
    n: usize,
) -> Option<(String, &'a [Spanned<Expr>])> {
    let (dotted, args) = call_name_args(expr)?;
    let short = dotted.rsplit('.').next().unwrap_or(&dotted);
    (short == name && args.len() == n).then_some((dotted, args))
}

pub(super) fn call_qualified<'a>(
    expr: &'a Spanned<Expr>,
    qual: &str,
    n: usize,
) -> Option<&'a [Spanned<Expr>]> {
    let (dotted, args) = call_name_args(expr)?;
    (dotted == qual && args.len() == n).then_some(args)
}

pub(super) fn ident_name(expr: &Spanned<Expr>) -> Option<&str> {
    match &expr.node {
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => Some(n.as_str()),
        _ => None,
    }
}

pub(super) fn is_ident(expr: &Spanned<Expr>, name: &str) -> bool {
    ident_name(expr) == Some(name)
}

/// Immediate sub-expressions used by law-auto source-AST scans. This mirrors
/// the pre-existing pure recognizer walkers while making tail-call args visible.
pub(super) fn child_exprs(e: &Spanned<Expr>) -> Vec<&Spanned<Expr>> {
    match &e.node {
        Expr::FnCall(callee, args) => {
            let mut v = vec![callee.as_ref()];
            v.extend(args.iter());
            v
        }
        Expr::BinOp(_, l, r) => vec![l.as_ref(), r.as_ref()],
        Expr::Neg(inner) | Expr::ErrorProp(inner) => vec![inner.as_ref()],
        Expr::Attr(base, _) => vec![base.as_ref()],
        Expr::Constructor(_, Some(inner)) => vec![inner.as_ref()],
        Expr::Match { subject, arms } => {
            let mut v = vec![subject.as_ref()];
            v.extend(arms.iter().map(|a| a.body.as_ref()));
            v
        }
        Expr::List(items) | Expr::Tuple(items) => items.iter().collect(),
        Expr::TailCall(tc) => tc.args.iter().collect(),
        _ => Vec::new(),
    }
}

/// Collect dotted/source names of call sites using the legacy law-auto call-scan
/// shape, with `TailCall` treated as a call site after TCO.
pub(super) fn collect_fncall_names(e: &Expr, out: &mut Vec<String>) {
    match e {
        Expr::FnCall(callee, args) => {
            if let Some(n) = expr_dotted_name(callee) {
                out.push(n);
            }
            for a in args {
                collect_fncall_names(&a.node, out);
            }
        }
        Expr::TailCall(tc) => {
            out.push(tc.target.clone());
            for a in &tc.args {
                collect_fncall_names(&a.node, out);
            }
        }
        Expr::BinOp(_, a, b) => {
            collect_fncall_names(&a.node, out);
            collect_fncall_names(&b.node, out);
        }
        Expr::Neg(a) => collect_fncall_names(&a.node, out),
        Expr::Attr(b, _) => collect_fncall_names(&b.node, out),
        Expr::RecordCreate { fields, .. } => {
            for (_, v) in fields {
                collect_fncall_names(&v.node, out);
            }
        }
        Expr::Match { subject, arms } => {
            collect_fncall_names(&subject.node, out);
            for arm in arms {
                collect_fncall_names(&arm.body.node, out);
            }
        }
        _ => {}
    }
}

fn short_call_tree_children(e: &Spanned<Expr>) -> Vec<&Spanned<Expr>> {
    match &e.node {
        Expr::FnCall(_, args) => args.iter().collect(),
        Expr::TailCall(tc) => tc.args.iter().collect(),
        Expr::Attr(b, _) | Expr::Neg(b) => vec![b.as_ref()],
        Expr::BinOp(_, l, r) => vec![l.as_ref(), r.as_ref()],
        Expr::Constructor(_, Some(inner)) => vec![inner.as_ref()],
        _ => Vec::new(),
    }
}

/// Collect short callee names through the legacy triangle-style call tree,
/// treating post-TCO `TailCall` nodes as call sites.
pub(super) fn collect_short_callees(expr: &Spanned<Expr>, out: &mut BTreeSet<String>) {
    if let Some((short, _)) = short_call_name_args(expr) {
        out.insert(short);
    }
    for child in short_call_tree_children(expr) {
        collect_short_callees(child, out);
    }
}

/// Search the legacy triangle-style call tree, invoking `f` at each call site.
pub(super) fn find_map_short_call_tree<T, F>(expr: &Spanned<Expr>, f: &mut F) -> Option<T>
where
    F: FnMut(&str, &[Spanned<Expr>]) -> Option<T>,
{
    if let Some((short, args)) = short_call_name_args(expr)
        && let Some(found) = f(&short, args)
    {
        return Some(found);
    }
    short_call_tree_children(expr)
        .into_iter()
        .find_map(|child| find_map_short_call_tree(child, f))
}

/// Deepest user-fn call `h(<field>)` (single arg the given binder) anywhere in
/// `expr`; wrappers around the call are traversed through [`child_exprs`].
pub(super) fn call_on_binder(
    expr: &Spanned<Expr>,
    field: &str,
    ctx: &CodegenContext,
) -> Option<String> {
    let mut found: Option<String> = None;
    fn walk(e: &Spanned<Expr>, field: &str, ctx: &CodegenContext, found: &mut Option<String>) {
        if let Some((name, args)) = call_name_args(e)
            && args.len() == 1
            && is_ident(&args[0], field)
            && find_fn_def(ctx, &name).is_some()
        {
            *found = Some(name);
        }
        for child in child_exprs(e) {
            walk(child, field, ctx, found);
        }
    }
    walk(expr, field, ctx, &mut found);
    found
}

/// Whether `expr` contains a call `fn_src(<ident>)` (single arg, the given
/// binder), seeing through post-TCO `TailCall` nodes.
pub(super) fn calls_fn_on_ident(expr: &Spanned<Expr>, fn_src: &str, ident: &str) -> bool {
    if let Some((name, args)) = call_name_args(expr)
        && args.len() == 1
        && is_ident(&args[0], ident)
        && name == fn_src
    {
        return true;
    }
    child_exprs(expr)
        .into_iter()
        .any(|c| calls_fn_on_ident(c, fn_src, ident))
}

/// Direct callees of `src` that are user fn defs (by source name), treating
/// `TailCall` as a call site so mutual SCC recognition sees post-TCO bodies.
pub(super) fn direct_user_calls(src: &str, ctx: &CodegenContext) -> BTreeSet<String> {
    let mut out = BTreeSet::new();
    let Some(fd) = find_fn_def(ctx, src) else {
        return out;
    };
    fn walk(e: &Spanned<Expr>, ctx: &CodegenContext, out: &mut BTreeSet<String>) {
        if let Some((name, _)) = call_name_args(e)
            && let Some(fd) = find_fn_def(ctx, &name)
        {
            out.insert(fd.name.clone());
        }
        for c in child_exprs(e) {
            walk(c, ctx, out);
        }
    }
    for stmt in fd.body.stmts() {
        match stmt {
            Stmt::Expr(e) | Stmt::Binding(_, _, e) => walk(e, ctx, &mut out),
        }
    }
    out
}

/// Whether `clause` guarantees `0 < x` for the atom rendered as `x_render`:
/// `0 < x`, `x > 0`, `1 <= x`, `x >= 1` (any nonneg/≥1 literal bound).
/// Clauses arrive canonicalized by [`flatten_and`], so `x > 0` / `x >= 1`
/// have already been swapped to `0 < x` / `1 <= x` — only the `Lt`/`Lte`
/// forms need matching.
pub(super) fn clause_gives_pos(
    clause: &Spanned<Expr>,
    x_render: &str,
    ctx: &CodegenContext,
) -> bool {
    let Expr::BinOp(op, l, r) = &clause.node else {
        return false;
    };
    let int_lit = |e: &Spanned<Expr>| match &e.node {
        Expr::Literal(Literal::Int(n)) => Some(*n),
        _ => None,
    };
    match op {
        // c < x  (c >= 0)  ⟹  0 < x   (also matches canonicalized `x > c`)
        BinOp::Lt => int_lit(l).is_some_and(|c| c >= 0) && render(r, ctx) == x_render,
        // c <= x  (c >= 1)          (also matches canonicalized `x >= c`)
        BinOp::Lte => int_lit(l).is_some_and(|c| c >= 1) && render(r, ctx) == x_render,
        _ => false,
    }
}

/// Whether `clause` guarantees `0 <= x` for the atom rendered as `x_render`:
/// `0 <= x`, `x >= 0`, and any strictly-positive bound (which implies `>= 0`).
pub(super) fn clause_gives_nonneg(
    clause: &Spanned<Expr>,
    x_render: &str,
    ctx: &CodegenContext,
) -> bool {
    if clause_gives_pos(clause, x_render, ctx) {
        return true;
    }
    let Expr::BinOp(op, l, r) = &clause.node else {
        return false;
    };
    let int_lit = |e: &Spanned<Expr>| match &e.node {
        Expr::Literal(Literal::Int(n)) => Some(*n),
        _ => None,
    };
    match op {
        // c <= x  (c >= 0)          (also matches canonicalized `x >= c`)
        BinOp::Lte => int_lit(l).is_some_and(|c| c >= 0) && render(r, ctx) == x_render,
        _ => false,
    }
}

/// Whether `clause` is the strict upper bound `x < y` for atoms rendered as
/// `x_render` / `y_render`. Clauses are canonicalized by [`flatten_and`], so a
/// user-written `y > x` arrives here already swapped to `x < y` and matches.
pub(super) fn clause_is_lt(
    clause: &Spanned<Expr>,
    x_render: &str,
    y_render: &str,
    ctx: &CodegenContext,
) -> bool {
    let Expr::BinOp(BinOp::Lt, l, r) = &clause.node else {
        return false;
    };
    render(l, ctx) == x_render && render(r, ctx) == y_render
}

pub(super) fn body_terminal_expr(body: &FnBody) -> Option<&Spanned<Expr>> {
    match body.stmts() {
        [Stmt::Expr(expr)] => Some(expr),
        _ => None,
    }
}

/// Substitute bare `Ident`/`Resolved` occurrences with mapped
/// expressions. Match-arm pattern bindings shadow the outer
/// substitution via [`rewrite_idents_scoped`] — the same traversal the
/// parser uses, so the law-auto sample expansion and verify-trace
/// local bindings handle shadowing identically.
pub(super) fn substitute_expr(
    expr: &Spanned<Expr>,
    bindings: &std::collections::HashMap<&str, &Spanned<Expr>>,
) -> Spanned<Expr> {
    rewrite_idents_scoped(expr, |name| bindings.get(name).map(|v| (*v).clone()))
}

pub(super) fn law_simp_defs(
    ctx: &CodegenContext,
    vb: &VerifyBlock,
    law: &VerifyLaw,
) -> BTreeSet<String> {
    law_simp_source_names(ctx, vb, law)
        .into_iter()
        .map(|name| {
            let rendered = aver_name_to_lean(&name);
            // Entry-module fns are emitted at the Lean root, so a user fn whose
            // name shadows a stdlib symbol (e.g. `insert` vs the `Insert.insert`
            // class method) makes a bare `simp only [insert]` error ("proposition
            // expected") — the equation compiler can't pick the user def.
            // `_root_.insert` always resolves to it and is a no-op otherwise.
            // Dep-module fns are NOT at root: they live under their module
            // namespace (`Lib.qrev`) and reach the consumer via `open Lib`, so a
            // `_root_.` prefix would FAIL to resolve. Leave those bare — `open`
            // resolves them exactly as before this change.
            if is_dep_module_fn(ctx, &name) {
                rendered
            } else {
                format!("_root_.{rendered}")
            }
        })
        .collect()
}

/// A simp-set name belongs to a dependency module iff its `FnDef` lives in
/// one of `ctx.modules` (which holds dep modules only — entry-module fns sit
/// in `ctx.fn_defs`). Mirrors `find_fn_def`'s dep-first resolution: on a
/// bare-name collision between an entry and a dep fn, the dep classification
/// wins, matching which def the bare reference actually resolves to.
fn is_dep_module_fn(ctx: &CodegenContext, source_name: &str) -> bool {
    ctx.modules
        .iter()
        .any(|m| m.fn_defs.iter().any(|fd| fd.name == source_name))
}

pub(super) fn law_simp_source_names(
    ctx: &CodegenContext,
    vb: &VerifyBlock,
    law: &VerifyLaw,
) -> BTreeSet<String> {
    let mut names = BTreeSet::new();
    names.insert(vb.fn_name.clone());
    collect_user_fn_simp_names(&law.lhs, ctx, &vb.fn_name, &mut names);
    collect_user_fn_simp_names(&law.rhs, ctx, &vb.fn_name, &mut names);
    if let Some(when_expr) = &law.when {
        collect_user_fn_simp_names(when_expr, ctx, &vb.fn_name, &mut names);
    }
    expand_pure_fn_simp_names(ctx, &vb.fn_name, &mut names);
    names
}

fn expand_pure_fn_simp_names(ctx: &CodegenContext, skip_fn: &str, out: &mut BTreeSet<String>) {
    loop {
        let before = out.len();
        let current = out.iter().cloned().collect::<Vec<_>>();
        for name in current {
            let Some(fd) = find_fn_def(ctx, &name) else {
                continue;
            };
            if !fd.effects.is_empty() || fd.name == "main" {
                continue;
            }
            for stmt in fd.body.stmts() {
                match stmt {
                    Stmt::Expr(expr) | Stmt::Binding(_, _, expr) => {
                        collect_user_fn_simp_names(expr, ctx, skip_fn, out);
                    }
                }
            }
        }
        if out.len() == before {
            return;
        }
    }
}

fn collect_user_fn_simp_names(
    expr: &Spanned<Expr>,
    ctx: &CodegenContext,
    skip_fn: &str,
    out: &mut BTreeSet<String>,
) {
    match &expr.node {
        Expr::FnCall(callee, args) => {
            if let Some(name) = expr_dotted_name(callee)
                && let Some(fd) = find_fn_def_by_call_name(ctx, &name)
                && fd.effects.is_empty()
                && fd.name != "main"
                && fd.name != skip_fn
            {
                out.insert(fd.name.clone());
            }
            collect_user_fn_simp_names(callee, ctx, skip_fn, out);
            for arg in args {
                collect_user_fn_simp_names(arg, ctx, skip_fn, out);
            }
        }
        Expr::Attr(base, _) => collect_user_fn_simp_names(base, ctx, skip_fn, out),
        Expr::BinOp(_, l, r) => {
            collect_user_fn_simp_names(l, ctx, skip_fn, out);
            collect_user_fn_simp_names(r, ctx, skip_fn, out);
        }
        Expr::Neg(inner) => collect_user_fn_simp_names(inner, ctx, skip_fn, out),
        Expr::Match { subject, arms, .. } => {
            collect_user_fn_simp_names(subject, ctx, skip_fn, out);
            for arm in arms {
                collect_user_fn_simp_names(&arm.body, ctx, skip_fn, out);
            }
        }
        Expr::Constructor(_, inner) => {
            if let Some(inner) = inner {
                collect_user_fn_simp_names(inner, ctx, skip_fn, out);
            }
        }
        Expr::ErrorProp(inner) => collect_user_fn_simp_names(inner, ctx, skip_fn, out),
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let crate::ast::StrPart::Parsed(inner) = part {
                    collect_user_fn_simp_names(inner, ctx, skip_fn, out);
                }
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for item in items {
                collect_user_fn_simp_names(item, ctx, skip_fn, out);
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries {
                collect_user_fn_simp_names(k, ctx, skip_fn, out);
                collect_user_fn_simp_names(v, ctx, skip_fn, out);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, v) in fields {
                collect_user_fn_simp_names(v, ctx, skip_fn, out);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_user_fn_simp_names(base, ctx, skip_fn, out);
            for (_, v) in updates {
                collect_user_fn_simp_names(v, ctx, skip_fn, out);
            }
        }
        Expr::TailCall(call) => {
            if let Some(fd) = find_fn_def_by_call_name(ctx, &call.target)
                && fd.effects.is_empty()
                && fd.name != "main"
                && fd.name != skip_fn
            {
                out.insert(fd.name.clone());
            }
            for arg in &call.args {
                collect_user_fn_simp_names(arg, ctx, skip_fn, out);
            }
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } => {}
    }
}

pub(super) fn find_fn_def<'a>(ctx: &'a CodegenContext, fn_name: &str) -> Option<&'a FnDef> {
    ctx.modules
        .iter()
        .flat_map(|m| m.fn_defs.iter())
        .chain(ctx.fn_defs.iter())
        .find(|fd| fd.name == fn_name)
}

pub(super) fn find_fn_def_by_call_name<'a>(
    ctx: &'a CodegenContext,
    call_name: &str,
) -> Option<&'a FnDef> {
    find_fn_def(ctx, call_name).or_else(|| {
        let short = call_name.rsplit('.').next()?;
        find_fn_def(ctx, short)
    })
}

pub(super) fn expr_dotted_name(expr: &Spanned<Expr>) -> Option<String> {
    match &expr.node {
        Expr::Ident(name) | Expr::Resolved { name, .. } => Some(name.clone()),
        Expr::Attr(base, field) => expr_dotted_name(base).map(|p| format!("{p}.{field}")),
        _ => None,
    }
}

/// **syntax-discovery-only** (epic #170 Phase 7). Exact-match
/// recognition of a callee's dotted source name. The previous
/// suffix-match clause (`name.rsplit('.').next() == Some(target)`)
/// was an identity leak — sibling fix to the one in
/// `proof_lower::callee_matches_name`.
pub(super) fn callee_matches_name(expr: &Spanned<Expr>, target: &str) -> bool {
    let Some(name) = expr_dotted_name(expr) else {
        return false;
    };
    name == target
}

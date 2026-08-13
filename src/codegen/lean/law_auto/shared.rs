//! Shared recognition helpers for the law_auto proof arms.
//!
//! These live in the Lean codegen layer (not `analysis::shape`) because they
//! render through `emit_expr_legacy`/`aver_name_to_lean` and take a
//! `CodegenContext` — they are recognition-plus-rendering, not pre-codegen
//! analysis. Contract: every walker here is TailCall-aware (the TCO pass
//! rewrites peer calls before law_auto runs), so arms built on this module
//! see post-TCO bodies uniformly; a walker that ignored `Expr::TailCall`
//! reintroduces the recognition blindness fixed for the container arm.

use std::collections::BTreeSet;

use super::super::expr::aver_name_to_lean;
use crate::ast::{
    BinOp, Expr, FnBody, FnDef, Literal, Spanned, Stmt, TopLevel, VerifyBlock, VerifyKind,
    VerifyLaw,
};
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

/// Immediate sub-expressions used by law-auto source-AST scans.
///
/// One line, because the knowledge of what an `Expr`'s children are lives in
/// `codegen::expr_walk` and nowhere else. The local copy this replaces had the
/// same `_ => Vec::new()` arm that hid two variants from the map-order gate
/// (#887), and it was missing four more: interpolated segments, independent
/// products, map-literal entries and record fields.
pub(super) fn child_exprs(e: &Spanned<Expr>) -> Vec<&Spanned<Expr>> {
    crate::codegen::expr_walk::child_exprs(e)
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

// ---------------------------------------------------------------------------
// Divisor-shape positivity (`0 < d`) discharge.
//
// The floor-family arms need `0 < d` for every divisor / factor atom. Rather
// than force the author to spell a `when 0 < d` guard, derive the fact from the
// atom's AST SHAPE: a positive `Int` literal (`decide`), a product of positives
// (`Int.mul_pos` over recursively-derived factors), or a fn call `f(a)` with a
// proven pool positivity law `f(_) >= 1` (CITE it — laws-as-lemmas, keyed on the
// claim FORM not the fn's name). The author's guard stays the fallback and is
// checked FIRST, so every pre-L5 guarded law emits byte-identically (`by omega`).
// An atom no route covers yields `None`, and the arm declines exactly as before
// (fail-closed).
// ---------------------------------------------------------------------------

/// How `0 < <atom>` is discharged for a divisor / factor atom, keyed on the
/// atom's AST shape.
pub(super) enum PositivityFact {
    /// A positive `Int` literal: `by decide`.
    Decide,
    /// A product `x * y`: `Int.mul_pos` over the two factors' facts.
    MulPos(Box<PositivityFact>, Box<PositivityFact>),
    /// A fn call `f(a)` with a proven pool positivity law: cite it at `a`, then
    /// `omega`. `citation` is the applied theorem term `<fn>_law_<name> (arg)`.
    CitedLaw { citation: String },
    /// The author's `when` guard already gives `0 < atom`: read it off `h_when`
    /// with `omega` (byte-identical to the pre-L5 emission).
    WhenGuard,
}

impl PositivityFact {
    /// Whether the atom is built PURELY from literals (so `0 < atom` would
    /// default its numerals to `Nat`), meaning the `have` needs an explicit
    /// `(atom : Int)` ascription. A fact carrying any fn call / bound variable
    /// (`CitedLaw`, `WhenGuard`, or a `MulPos` containing one) is already
    /// `Int`-anchored, so it stays un-ascribed and byte-identical to pre-L5.
    pub(super) fn needs_int_ascription(&self) -> bool {
        match self {
            PositivityFact::Decide => true,
            PositivityFact::MulPos(l, r) => l.needs_int_ascription() && r.needs_int_ascription(),
            PositivityFact::CitedLaw { .. } | PositivityFact::WhenGuard => false,
        }
    }

    /// The Lean proof TERM of type `0 < <atom>`. Valid both as the RHS of a
    /// `have h : 0 < atom := <term>` and as a bare argument to `Int.mul_pos`
    /// (whose expected factor types fix any inner `by` goal).
    pub(super) fn lean_term(&self) -> String {
        match self {
            PositivityFact::Decide => "by decide".to_string(),
            PositivityFact::WhenGuard => "by omega".to_string(),
            PositivityFact::CitedLaw { citation } => {
                // `citation : (f arg >= B) = true` is a `Prop` equality
                // (`(B <= f arg) = True` after `ge_iff_le`) that `omega` will not
                // read. Normalize it to the bare `B <= f arg` fact first — the
                // exact `[ge_iff_le, eq_iff_iff, iff_true]` chain
                // `emit_recursive_positive_law` uses to discharge the same
                // statement — THEN `omega` derives `0 < f arg`. Without the
                // normalization `omega` silently ignores the hypothesis and only
                // closes when other premises already imply positivity (the absorb
                // arms), so the cancel arm — whose sole positivity source is this
                // citation — would fail.
                format!(
                    "by have hpos := {citation}; \
                     simp only [ge_iff_le, eq_iff_iff, iff_true] at hpos; omega"
                )
            }
            PositivityFact::MulPos(l, r) => {
                format!("Int.mul_pos ({}) ({})", l.lean_term(), r.lean_term())
            }
        }
    }
}

/// Derive `0 < atom` from the atom's AST shape, or `None` when no route covers
/// it (the arm then declines, fail-closed). `clauses` are the law's
/// canonicalized `when` conjuncts; `before_line` is the citing law's source
/// line so a cited pool law is only accepted when it is emitted EARLIER (Lean
/// forbids forward references).
pub(super) fn divisor_positivity(
    atom: &Spanned<Expr>,
    clauses: &[Spanned<Expr>],
    ctx: &CodegenContext,
    before_line: usize,
) -> Option<PositivityFact> {
    // 1. Author guard FIRST — every pre-L5 guarded law keeps its exact emission.
    let atom_render = render(atom, ctx);
    if clauses
        .iter()
        .any(|cl| clause_gives_pos(cl, &atom_render, ctx))
    {
        return Some(PositivityFact::WhenGuard);
    }
    // 2. Positive Int literal.
    if let Expr::Literal(Literal::Int(n)) = &atom.node {
        return (*n > 0).then_some(PositivityFact::Decide);
    }
    // 3. Product of positives.
    if let Expr::BinOp(BinOp::Mul, x, y) = &atom.node {
        let fx = divisor_positivity(x, clauses, ctx, before_line)?;
        let fy = divisor_positivity(y, clauses, ctx, before_line)?;
        return Some(PositivityFact::MulPos(Box::new(fx), Box::new(fy)));
    }
    // 4. Fn call with a proven pool positivity law.
    cited_positivity_law(atom, ctx, before_line)
        .map(|citation| PositivityFact::CitedLaw { citation })
}

/// Find a pool positivity law about the SAME fn the atom calls, emitted earlier
/// than `before_line` and proven universal; return the applied citation term
/// `<fn>_law_<name> (arg)`. Shape-keyed on the claim form `f(binder) >= B`
/// (`B >= 1`), never on the fn's name — the fn is discovered from the atom.
fn cited_positivity_law(
    atom: &Spanned<Expr>,
    ctx: &CodegenContext,
    before_line: usize,
) -> Option<String> {
    let (f_src, args) = call_name_args(atom)?;
    if args.len() != 1 {
        // The recursive-positivity pool law is unary (`f : Int -> Int`).
        return None;
    }
    let f_short = f_src.rsplit('.').next().unwrap_or(&f_src);
    for vb in citable_pool_blocks(ctx, before_line) {
        let VerifyKind::Law(law) = &vb.kind else {
            continue;
        };
        if vb.fn_name.rsplit('.').next().unwrap_or(&vb.fn_name) != f_short {
            continue;
        }
        if !law_claims_positive_over(law, f_short) {
            continue;
        }
        // Universality gate: the recursive-positivity arm closes this as the
        // universal `∀ binder, (f binder >= B) = true`, so a citation at any
        // argument typechecks and `omega` derives `0 < f arg` from it.
        if !super::recursive_mono::recognize_recursive_positive(vb, law, ctx) {
            continue;
        }
        let base = format!(
            "{}_law_{}",
            aver_name_to_lean(&vb.fn_name),
            aver_name_to_lean(&law.name)
        );
        return Some(format!("{base} ({})", render(&args[0], ctx)));
    }
    None
}

/// Sibling verify-law blocks a law at `before_line` may cite: dependency-module
/// laws (always emitted before the entry module) and EARLIER entry-module laws
/// (source line strictly before the citing law). Ordering is enforced because
/// Lean forbids forward references to the cited theorem.
fn citable_pool_blocks(ctx: &CodegenContext, before_line: usize) -> Vec<&VerifyBlock> {
    let mut out: Vec<&VerifyBlock> = Vec::new();
    for module in &ctx.modules {
        out.extend(module.verify_laws.iter());
    }
    for item in &ctx.items {
        if let TopLevel::Verify(b) = item
            && b.line < before_line
        {
            out.push(b);
        }
    }
    out
}

/// Whether `law`'s EMITTED theorem is EXACTLY `∀ (v : T), (f v >= B) = true`
/// (or the `B <= f v` twin) with `B >= 1` — the plain single-binder positivity
/// form the citation term `f_law_name (arg)` consumes. STATEMENT-level, not
/// merely recognizer-level: the citation applies exactly ONE argument to the
/// theorem and hands the result straight to `omega`, so the theorem must carry
/// no extra premise or binder. The fn is discovered from the caller's atom,
/// never hardcoded. Fail-closed on anything else. Rejects, in particular:
///   * a `when`-guarded law — its theorem gains a `<guard> = true ->` premise,
///     so `f_law_name (arg)` is an implication `omega` cannot read;
///   * a law with more than one given — `f_law_name (arg)` is then a partial
///     application, still a `∀`, not the bare comparison;
///   * a law whose fn argument is not the plain bound binder (`f(v + 1) >= B`) —
///     citing at the caller's atom would yield a fact about the WRONG term.
///
/// `B >= 1` (not `>= 0`) is load-bearing: a `f >= 0` law gives only `0 <= f`,
/// not the strict `0 < f` the divisor needs.
fn law_claims_positive_over(law: &VerifyLaw, f_short: &str) -> bool {
    // A `when` guard becomes a `... = true ->` premise on the emitted theorem.
    if law.when.is_some() {
        return false;
    }
    // The citation applies exactly ONE argument, so the theorem must bind
    // exactly one `∀` variable for it to fully instantiate.
    let [binder] = law.givens.as_slice() else {
        return false;
    };
    if !matches!(law.rhs.node, Expr::Literal(Literal::Bool(true))) {
        return false;
    }
    let int_lit = |e: &Spanned<Expr>| match &e.node {
        Expr::Literal(Literal::Int(n)) => Some(*n),
        _ => None,
    };
    // `f(v)` where `v` is EXACTLY the single bound binder, so substituting the
    // caller's atom argument yields `(f arg >= B) = true` about THAT atom.
    let is_f_of_binder = |e: &Spanned<Expr>| -> bool {
        matches!(call_name_args(e), Some((n, a))
            if n.rsplit('.').next().unwrap_or(&n) == f_short
                && a.len() == 1
                && ident_name(&a[0]) == Some(binder.name.as_str()))
    };
    match &law.lhs.node {
        // f(binder) >= B
        Expr::BinOp(BinOp::Gte, l, r) => is_f_of_binder(l) && int_lit(r).is_some_and(|b| b >= 1),
        // B <= f(binder)
        Expr::BinOp(BinOp::Lte, l, r) => int_lit(l).is_some_and(|b| b >= 1) && is_f_of_binder(r),
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
            // Entry-module fns use their module-qualified name so a user fn
            // shadowing a stdlib symbol (for example `insert`) remains an
            // unambiguous simp target. Dependency fns already live under an
            // opened module namespace and keep their existing bare spelling.
            if is_dep_module_fn(ctx, &name) {
                rendered
            } else {
                entry_qualified_lean_name(ctx, &name)
            }
        })
        .collect()
}

pub(super) fn entry_qualified_lean_name(ctx: &CodegenContext, source_name: &str) -> String {
    format!(
        "{}.{}",
        super::super::lean_project_name(ctx),
        aver_name_to_lean(source_name)
    )
}

pub(super) fn bare_lean_name(name: &str) -> &str {
    name.rsplit('.').next().unwrap_or(name)
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

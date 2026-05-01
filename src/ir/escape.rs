//! Escape analysis pass — scalar-replace fresh-alloc-immediate-consume.
//!
//! Catches the pattern:
//!
//! ```aver
//! distance(Point(x = n, y = n * 2))
//! ```
//!
//! where `distance` only reads `.x` / `.y` fields of its param. Without
//! this pass, every iteration in a hot loop allocates a `Point` struct,
//! threads it through one fn call, then drops it. Engine GC (wasm-gc)
//! pays per-allocation cost; arena allocators (VM, legacy WASM) pay
//! bump-pointer + tag-bit cost. Both are eliminated when the
//! constructor never escapes the local frame.
//!
//! ## Strategy: targeted inlining + Attr-folding
//!
//! Two-step rewrite at the IR level (post-resolve, pre-codegen):
//!
//! 1. **Classify candidate fns** — a fn `f(p: Record) -> T` is
//!    inline-eligible when its body is a single expression that only
//!    accesses `p` through `Attr(p, field)`. Bare `Resolved(p)` (passing
//!    `p` as-is to another fn, returning it, storing it) disqualifies
//!    because it means the value escapes.
//!
//! 2. **Rewrite call sites** — for every `FnCall(f, [RecordCreate{…}])`
//!    where `f` is eligible, splice `f`'s body in place of the call
//!    and substitute each `Attr(p, field)` with the field's value
//!    expression from the `RecordCreate`. The `RecordCreate` itself
//!    becomes orphaned and gets DCE'd by the same logic — never
//!    reaches codegen.
//!
//! ## Why not full inlining?
//!
//! Aver has no general inliner today (deliberately — `wasm-opt -O3`
//! handles the WASM target). This pass is narrower: only inline when
//! it eliminates a fresh allocation. Same payoff as classical SRoA
//! (scalar replacement of aggregates) without the surrounding
//! infrastructure.
//!
//! ## What it doesn't do (yet)
//!
//! - Multi-param record functions — phase 3d/2.
//! - Match-on-variant scalar replacement (would close the
//!   `match_dispatch` regression) — needs separate handling because
//!   the pattern shape is different.
//! - Deep substitution where the field value is itself an expensive
//!   expression that gets duplicated. For now we accept the duplication
//!   — if measurable, a follow-up adds let-bind hoisting.
//!
//! ## Status
//!
//! Shared IR pass. Each backend (VM, WASM legacy, WASM-gc, Rust)
//! benefits because `Expr::RecordCreate` simply disappears from the
//! IR before codegen sees it.

use std::collections::HashMap;
use std::sync::Arc;

use crate::ast::{Expr, FnBody, FnDef, MatchArm, Spanned, Stmt, StrPart, TopLevel};

/// Per-fn classification: eligible-for-inlining + the body to splice
/// in. Built once per `run()` from the top-level item list.
#[derive(Clone)]
struct InlineCandidate {
    /// The single record-typed param's slot index.
    param_slot: u16,
    /// Body expression — cloned at classification time so the
    /// rewriter can substitute freely.
    body: Spanned<Expr>,
}

/// Run the pass on `items` in place. Returns the number of call sites
/// rewritten — `0` means the program had no fresh-alloc-immediate-consume
/// pattern reachable through eligible fns.
pub fn run(items: &mut [TopLevel]) -> usize {
    let candidates = build_candidate_map(items);
    if candidates.is_empty() {
        return 0;
    }
    let mut rewrites = 0usize;
    for item in items.iter_mut() {
        if let TopLevel::FnDef(fd) = item {
            // Don't try to inline a candidate INTO itself — that would
            // be self-substituting recursion. Should never trigger the
            // pattern (a fn that calls `Self.<self>(RecordCreate)` is
            // unusual), but defensive.
            let body_arc = Arc::make_mut(&mut fd.body);
            let FnBody::Block(stmts) = body_arc;
            for stmt in stmts.iter_mut() {
                rewrites += rewrite_in_stmt(stmt, &candidates, &fd.name);
            }
        }
    }
    rewrites
}

fn build_candidate_map(items: &[TopLevel]) -> HashMap<String, InlineCandidate> {
    let mut out = HashMap::new();
    for item in items {
        if let TopLevel::FnDef(fd) = item
            && let Some(c) = classify_fn(fd)
        {
            out.insert(fd.name.clone(), c);
        }
    }
    out
}

fn classify_fn(fd: &FnDef) -> Option<InlineCandidate> {
    // Single param.
    if fd.params.len() != 1 {
        return None;
    }
    // No declared effects — pure body only.
    if !fd.effects.is_empty() {
        return None;
    }
    // Body is a single Stmt::Expr.
    let FnBody::Block(stmts) = fd.body.as_ref();
    if stmts.len() != 1 {
        return None;
    }
    let body = match &stmts[0] {
        Stmt::Expr(spanned) => spanned,
        _ => return None,
    };
    // Param's resolver slot.
    let resolution = fd.resolution.as_ref()?;
    let param_slot = resolution.local_slots.get(&fd.params[0].0).copied()?;
    // Walk body — every Resolved(param_slot) must be the obj of an
    // Attr expression (i.e. param accessed only via field reads).
    if !body_uses_param_only_via_attr(&body.node, param_slot) {
        return None;
    }
    // No tail-call into the body — this pass runs after TCO; if the
    // body is a TailCall back to itself, we'd be inlining recursive
    // reference into a different fn's body, which doesn't make sense.
    if contains_tail_call(&body.node) {
        return None;
    }
    Some(InlineCandidate {
        param_slot,
        body: body.clone(),
    })
}

/// True iff every `Resolved { slot: param_slot }` in `expr` appears
/// as the LHS of an `Attr`. Bare references — passing the param to a
/// fn, returning it, storing in a struct — disqualify.
fn body_uses_param_only_via_attr(expr: &Expr, param_slot: u16) -> bool {
    let mut violation = false;
    walk_expr_with_context(expr, false, &mut |e, in_attr_obj| {
        if let Expr::Resolved { slot, .. } = e
            && *slot == param_slot
            && !in_attr_obj
        {
            violation = true;
        }
    });
    !violation
}

/// Walk an expression tree, calling `visit(e, in_attr_obj)` for every
/// node. `in_attr_obj` is true when `e` is the LHS of an `Attr`.
fn walk_expr_with_context(expr: &Expr, in_attr_obj: bool, visit: &mut dyn FnMut(&Expr, bool)) {
    visit(expr, in_attr_obj);
    match expr {
        Expr::Attr(obj, _) => walk_expr_with_context(&obj.node, true, visit),
        Expr::ErrorProp(obj) => walk_expr_with_context(&obj.node, false, visit),
        Expr::BinOp(_, l, r) => {
            walk_expr_with_context(&l.node, false, visit);
            walk_expr_with_context(&r.node, false, visit);
        }
        Expr::FnCall(callee, args) => {
            walk_expr_with_context(&callee.node, false, visit);
            for a in args {
                walk_expr_with_context(&a.node, false, visit);
            }
        }
        Expr::TailCall(boxed) => {
            for a in &boxed.args {
                walk_expr_with_context(&a.node, false, visit);
            }
        }
        Expr::Match { subject, arms } => {
            walk_expr_with_context(&subject.node, false, visit);
            for arm in arms {
                walk_expr_with_context(&arm.body.node, false, visit);
            }
        }
        Expr::Constructor(_, payload) => {
            if let Some(p) = payload.as_deref() {
                walk_expr_with_context(&p.node, false, visit);
            }
        }
        Expr::List(xs) | Expr::Tuple(xs) | Expr::IndependentProduct(xs, _) => {
            for x in xs {
                walk_expr_with_context(&x.node, false, visit);
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries {
                walk_expr_with_context(&k.node, false, visit);
                walk_expr_with_context(&v.node, false, visit);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, v) in fields {
                walk_expr_with_context(&v.node, false, visit);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            walk_expr_with_context(&base.node, false, visit);
            for (_, v) in updates {
                walk_expr_with_context(&v.node, false, visit);
            }
        }
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let StrPart::Parsed(inner) = part {
                    walk_expr_with_context(&inner.node, false, visit);
                }
            }
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } => {}
    }
}

fn contains_tail_call(expr: &Expr) -> bool {
    let mut found = false;
    walk_expr_with_context(expr, false, &mut |e, _| {
        if matches!(e, Expr::TailCall(_)) {
            found = true;
        }
    });
    found
}

// ── Rewriter ───────────────────────────────────────────────────────

fn rewrite_in_stmt(
    stmt: &mut Stmt,
    candidates: &HashMap<String, InlineCandidate>,
    self_fn: &str,
) -> usize {
    match stmt {
        Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => {
            rewrite_in_expr(&mut expr.node, candidates, self_fn)
        }
    }
}

fn rewrite_in_expr(
    expr: &mut Expr,
    candidates: &HashMap<String, InlineCandidate>,
    self_fn: &str,
) -> usize {
    let mut rewrites = 0;
    // Recurse FIRST so we rewrite inner expressions before checking
    // whether the outer FnCall is itself a rewrite candidate. This
    // ordering matters when nested calls feed into each other.
    rewrites += rewrite_children(expr, candidates, self_fn);

    // Now look at THIS node — is it an FnCall we can splice?
    if let Expr::FnCall(callee, args) = expr {
        let callee_name = match &callee.node {
            Expr::Ident(n) => Some(n.as_str()),
            Expr::Resolved { name, .. } => Some(name.as_str()),
            _ => None,
        };
        if let Some(name) = callee_name
            && name != self_fn
            && let Some(cand) = candidates.get(name)
            && args.len() == 1
            && let Expr::RecordCreate { fields, .. } = &args[0].node
        {
            // Build name → value-expr map from the RecordCreate.
            let field_map: HashMap<String, Spanned<Expr>> =
                fields.iter().map(|(n, v)| (n.clone(), v.clone())).collect();
            let mut new_body = cand.body.node.clone();
            substitute_param_attr(&mut new_body, cand.param_slot, &field_map);
            *expr = new_body;
            rewrites += 1;
        }
    }
    rewrites
}

fn rewrite_children(
    expr: &mut Expr,
    candidates: &HashMap<String, InlineCandidate>,
    self_fn: &str,
) -> usize {
    let mut rewrites = 0;
    match expr {
        Expr::Attr(obj, _) | Expr::ErrorProp(obj) => {
            rewrites += rewrite_in_expr(&mut obj.node, candidates, self_fn);
        }
        Expr::BinOp(_, l, r) => {
            rewrites += rewrite_in_expr(&mut l.node, candidates, self_fn);
            rewrites += rewrite_in_expr(&mut r.node, candidates, self_fn);
        }
        Expr::FnCall(callee, args) => {
            rewrites += rewrite_in_expr(&mut callee.node, candidates, self_fn);
            for a in args {
                rewrites += rewrite_in_expr(&mut a.node, candidates, self_fn);
            }
        }
        Expr::TailCall(boxed) => {
            for a in &mut boxed.args {
                rewrites += rewrite_in_expr(&mut a.node, candidates, self_fn);
            }
        }
        Expr::Match { subject, arms } => {
            rewrites += rewrite_in_expr(&mut subject.node, candidates, self_fn);
            for arm in arms {
                rewrites += rewrite_in_expr(&mut arm.body.node, candidates, self_fn);
            }
        }
        Expr::Constructor(_, payload) => {
            if let Some(p) = payload.as_deref_mut() {
                rewrites += rewrite_in_expr(&mut p.node, candidates, self_fn);
            }
        }
        Expr::List(xs) | Expr::Tuple(xs) | Expr::IndependentProduct(xs, _) => {
            for x in xs {
                rewrites += rewrite_in_expr(&mut x.node, candidates, self_fn);
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries {
                rewrites += rewrite_in_expr(&mut k.node, candidates, self_fn);
                rewrites += rewrite_in_expr(&mut v.node, candidates, self_fn);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, v) in fields {
                rewrites += rewrite_in_expr(&mut v.node, candidates, self_fn);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            rewrites += rewrite_in_expr(&mut base.node, candidates, self_fn);
            for (_, v) in updates {
                rewrites += rewrite_in_expr(&mut v.node, candidates, self_fn);
            }
        }
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let StrPart::Parsed(inner) = part {
                    rewrites += rewrite_in_expr(&mut inner.node, candidates, self_fn);
                }
            }
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } => {}
    }
    rewrites
}

/// In `body`, replace every `Attr(Resolved(param_slot), field)` with
/// the value expression bound to `field` in `field_map`. Bare
/// `Resolved(param_slot)` should have been ruled out at classification
/// time — if one slips through (defensive), leave it alone (codegen
/// will fail loudly).
fn substitute_param_attr(
    body: &mut Expr,
    param_slot: u16,
    field_map: &HashMap<String, Spanned<Expr>>,
) {
    match body {
        Expr::Attr(obj, field) => {
            if let Expr::Resolved { slot, .. } = &obj.node
                && *slot == param_slot
                && let Some(value) = field_map.get(field.as_str())
            {
                *body = value.node.clone();
                return;
            }
            substitute_param_attr(&mut obj.node, param_slot, field_map);
        }
        Expr::ErrorProp(obj) => substitute_param_attr(&mut obj.node, param_slot, field_map),
        Expr::BinOp(_, l, r) => {
            substitute_param_attr(&mut l.node, param_slot, field_map);
            substitute_param_attr(&mut r.node, param_slot, field_map);
        }
        Expr::FnCall(callee, args) => {
            substitute_param_attr(&mut callee.node, param_slot, field_map);
            for a in args {
                substitute_param_attr(&mut a.node, param_slot, field_map);
            }
        }
        Expr::TailCall(boxed) => {
            for a in &mut boxed.args {
                substitute_param_attr(&mut a.node, param_slot, field_map);
            }
        }
        Expr::Match { subject, arms } => {
            substitute_param_attr(&mut subject.node, param_slot, field_map);
            for arm in arms {
                substitute_param_attr(&mut arm.body.node, param_slot, field_map);
            }
        }
        Expr::Constructor(_, payload) => {
            if let Some(p) = payload.as_deref_mut() {
                substitute_param_attr(&mut p.node, param_slot, field_map);
            }
        }
        Expr::List(xs) | Expr::Tuple(xs) | Expr::IndependentProduct(xs, _) => {
            for x in xs {
                substitute_param_attr(&mut x.node, param_slot, field_map);
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries {
                substitute_param_attr(&mut k.node, param_slot, field_map);
                substitute_param_attr(&mut v.node, param_slot, field_map);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, v) in fields {
                substitute_param_attr(&mut v.node, param_slot, field_map);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            substitute_param_attr(&mut base.node, param_slot, field_map);
            for (_, v) in updates {
                substitute_param_attr(&mut v.node, param_slot, field_map);
            }
        }
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let StrPart::Parsed(inner) = part {
                    substitute_param_attr(&mut inner.node, param_slot, field_map);
                }
            }
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } => {}
    }
}

// `MatchArm` is unused at the type level here; suppress the unused
// import warning while keeping the import for future variant-pattern
// extensions.
#[allow(dead_code)]
fn _arm_placeholder(_: &MatchArm) {}

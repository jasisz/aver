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

use crate::ast::{Expr, FnBody, FnDef, Pattern, Spanned, Stmt, StrPart, TopLevel};

/// Per-fn classification: eligible-for-inlining + the body to splice
/// in. Built once per `run()` from the top-level item list.
#[derive(Clone, Debug)]
enum InlineCandidate {
    /// `f(p: Record) -> T` where `p` only appears as `Attr(p, field)`.
    /// Inline by substituting `Attr(p, x)` → field value at the call
    /// site `f(RecordCreate{x = e_x, …})`.
    RecordAccess {
        param_slot: u16,
        body: Spanned<Expr>,
    },
    /// `f(s: Variant) -> T` whose body is `match s { Pat1(b1)->e1; … }`.
    /// At call site `f(Foo.PatN(args))` we splice arm `N`'s body with
    /// the pattern bindings substituted. Each arm's binding slots
    /// resolve via the resolver's `local_slots` map of `f`.
    VariantMatch {
        /// One entry per arm. Arm pattern dotted name → (binding slots,
        /// arm body). Wildcard arms aren't included; if the dispatch
        /// doesn't find a match the call falls through to runtime
        /// dispatch (today: error — but type checker proves
        /// exhaustiveness, so this is unreachable for well-typed
        /// programs).
        arms_by_constructor: HashMap<String, (Vec<u16>, Spanned<Expr>)>,
    },
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
    // No tail-call in body — we'd be inlining recursive reference
    // into a different fn's body if so.
    if contains_tail_call(&body.node) {
        return None;
    }

    // Shape A: body uses param only via Attr(p, field). Inline at
    // call sites where arg is RecordCreate.
    if body_uses_param_only_via_attr(&body.node, param_slot) {
        return Some(InlineCandidate::RecordAccess {
            param_slot,
            body: body.clone(),
        });
    }

    // Shape B: body is `match p { Constructor(b1)->e1; … }`. Inline
    // at call sites where arg is `Type.Constructor(args…)`.
    if let Expr::Match { subject, arms } = &body.node
        && is_param_subject(&subject.node, param_slot)
    {
        let mut arms_by_constructor: HashMap<String, (Vec<u16>, Spanned<Expr>)> = HashMap::new();
        for arm in arms {
            let Pattern::Constructor(name, bindings) = &arm.pattern else {
                // Non-Constructor arm (Wildcard, Literal) — give up.
                return None;
            };
            // Resolve binding slots via the resolver's local_slots map.
            // Each binding name was registered there during resolve.
            let mut binding_slots = Vec::with_capacity(bindings.len());
            for b in bindings {
                if b == "_" {
                    binding_slots.push(u16::MAX); // sentinel — discard
                    continue;
                }
                let slot = resolution.local_slots.get(b).copied()?;
                binding_slots.push(slot);
            }
            // Arm body must not use `s` directly outside Match's
            // dispatch shape (we walk to verify).
            if !arm_body_safe(&arm.body.node, param_slot) {
                return None;
            }
            arms_by_constructor.insert(name.clone(), (binding_slots, (*arm.body).clone()));
        }
        if !arms_by_constructor.is_empty() {
            return Some(InlineCandidate::VariantMatch {
                arms_by_constructor,
            });
        }
    }

    None
}

fn is_param_subject(expr: &Expr, param_slot: u16) -> bool {
    matches!(expr, Expr::Resolved { slot, .. } if *slot == param_slot)
}

/// Arm body is "safe" for inlining if it doesn't reference the
/// match subject's slot directly — it should only use the pattern
/// binding slots. (We accept FnCall and other expressions that
/// reference the binding slots as Resolved.)
fn arm_body_safe(expr: &Expr, param_slot: u16) -> bool {
    let mut violation = false;
    walk_expr_with_context(expr, false, &mut |e, _| {
        if let Expr::Resolved { slot, .. } = e
            && *slot == param_slot
        {
            violation = true;
        }
    });
    !violation
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
        {
            match cand {
                InlineCandidate::RecordAccess { param_slot, body } => {
                    if let Expr::RecordCreate { fields, .. } = &args[0].node {
                        let field_map: HashMap<String, Spanned<Expr>> =
                            fields.iter().map(|(n, v)| (n.clone(), v.clone())).collect();
                        let mut new_body = body.node.clone();
                        substitute_param_attr(&mut new_body, *param_slot, &field_map);
                        *expr = new_body;
                        rewrites += 1;
                    }
                }
                InlineCandidate::VariantMatch {
                    arms_by_constructor,
                    ..
                } => {
                    if let Some((variant_name, payload_args)) =
                        match_variant_constructor_call(&args[0].node)
                        && let Some((binding_slots, arm_body)) =
                            arms_by_constructor.get(&variant_name)
                        && payload_args.len() == binding_slots.len()
                    {
                        let mut binding_map: HashMap<u16, Spanned<Expr>> = HashMap::new();
                        for (slot, payload_expr) in binding_slots.iter().zip(payload_args.iter()) {
                            if *slot != u16::MAX {
                                binding_map.insert(*slot, payload_expr.clone());
                            }
                        }
                        let mut new_body = arm_body.node.clone();
                        substitute_resolved_slots(&mut new_body, &binding_map);
                        *expr = new_body;
                        rewrites += 1;
                    }
                }
            }
        }
    }
    rewrites
}

/// Match a `Type.Constructor(args)` call shape and return
/// `("Type.Constructor", args)`.
fn match_variant_constructor_call(expr: &Expr) -> Option<(String, Vec<Spanned<Expr>>)> {
    match expr {
        Expr::FnCall(callee, args) => {
            if let Expr::Attr(parent, member) = &callee.node {
                let parent_name = match &parent.node {
                    Expr::Ident(n) => n.clone(),
                    Expr::Resolved { name, .. } => name.clone(),
                    _ => return None,
                };
                Some((format!("{parent_name}.{member}"), args.clone()))
            } else {
                None
            }
        }
        Expr::Constructor(name, payload) => {
            if let Some(p) = payload.as_deref() {
                Some((name.clone(), vec![p.clone()]))
            } else {
                Some((name.clone(), Vec::new()))
            }
        }
        _ => None,
    }
}

fn substitute_resolved_slots(body: &mut Expr, binding_map: &HashMap<u16, Spanned<Expr>>) {
    match body {
        Expr::Resolved { slot, .. } => {
            if let Some(value) = binding_map.get(slot) {
                *body = value.node.clone();
            }
        }
        Expr::Attr(obj, _) | Expr::ErrorProp(obj) => {
            substitute_resolved_slots(&mut obj.node, binding_map);
        }
        Expr::BinOp(_, l, r) => {
            substitute_resolved_slots(&mut l.node, binding_map);
            substitute_resolved_slots(&mut r.node, binding_map);
        }
        Expr::FnCall(callee, args) => {
            substitute_resolved_slots(&mut callee.node, binding_map);
            for a in args {
                substitute_resolved_slots(&mut a.node, binding_map);
            }
        }
        Expr::TailCall(boxed) => {
            for a in &mut boxed.args {
                substitute_resolved_slots(&mut a.node, binding_map);
            }
        }
        Expr::Match { subject, arms } => {
            substitute_resolved_slots(&mut subject.node, binding_map);
            for arm in arms {
                substitute_resolved_slots(&mut arm.body.node, binding_map);
            }
        }
        Expr::Constructor(_, payload) => {
            if let Some(p) = payload.as_deref_mut() {
                substitute_resolved_slots(&mut p.node, binding_map);
            }
        }
        Expr::List(xs) | Expr::Tuple(xs) | Expr::IndependentProduct(xs, _) => {
            for x in xs {
                substitute_resolved_slots(&mut x.node, binding_map);
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries {
                substitute_resolved_slots(&mut k.node, binding_map);
                substitute_resolved_slots(&mut v.node, binding_map);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, v) in fields {
                substitute_resolved_slots(&mut v.node, binding_map);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            substitute_resolved_slots(&mut base.node, binding_map);
            for (_, v) in updates {
                substitute_resolved_slots(&mut v.node, binding_map);
            }
        }
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let StrPart::Parsed(inner) = part {
                    substitute_resolved_slots(&mut inner.node, binding_map);
                }
            }
        }
        Expr::Literal(_) | Expr::Ident(_) => {}
    }
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{AnnotBool, FnBody, FnDef, FnResolution, Literal, MatchArm, TopLevel};
    use std::collections::HashMap;
    use std::sync::Arc;

    // ── builders ────────────────────────────────────────────────────

    fn sp<T>(node: T) -> Spanned<T> {
        Spanned::new(node, 1)
    }

    fn lit_int(n: i64) -> Spanned<Expr> {
        sp(Expr::Literal(Literal::Int(n)))
    }

    fn lit_float(f: f64) -> Spanned<Expr> {
        sp(Expr::Literal(Literal::Float(f)))
    }

    fn resolved(name: &str, slot: u16) -> Spanned<Expr> {
        sp(Expr::Resolved {
            slot,
            name: name.to_string(),
            last_use: AnnotBool(false),
        })
    }

    fn ident(name: &str) -> Spanned<Expr> {
        sp(Expr::Ident(name.to_string()))
    }

    fn attr(obj: Spanned<Expr>, field: &str) -> Spanned<Expr> {
        sp(Expr::Attr(Box::new(obj), field.to_string()))
    }

    fn add(l: Spanned<Expr>, r: Spanned<Expr>) -> Spanned<Expr> {
        sp(Expr::BinOp(
            crate::ast::BinOp::Add,
            Box::new(l),
            Box::new(r),
        ))
    }

    fn fn_call(callee: Spanned<Expr>, args: Vec<Spanned<Expr>>) -> Spanned<Expr> {
        sp(Expr::FnCall(Box::new(callee), args))
    }

    fn record_create(type_name: &str, fields: Vec<(&str, Spanned<Expr>)>) -> Spanned<Expr> {
        sp(Expr::RecordCreate {
            type_name: type_name.to_string(),
            fields: fields
                .into_iter()
                .map(|(n, v)| (n.to_string(), v))
                .collect(),
        })
    }

    fn match_expr(subject: Spanned<Expr>, arms: Vec<MatchArm>) -> Spanned<Expr> {
        sp(Expr::Match {
            subject: Box::new(subject),
            arms,
        })
    }

    fn arm(pattern: Pattern, body: Spanned<Expr>) -> MatchArm {
        MatchArm {
            pattern,
            body: Box::new(body),
        }
    }

    /// Build a single-param fn whose body is `body_expr` (one Stmt::Expr).
    /// The single param `p` is registered at slot 0 in `local_slots`.
    fn fn_def_p(
        name: &str,
        param_type: &str,
        return_type: &str,
        body_expr: Spanned<Expr>,
    ) -> FnDef {
        fn_def_p_with_extra(name, param_type, return_type, body_expr, &[])
    }

    /// Same as `fn_def_p` but registers extra binding-name → slot pairs in
    /// `local_slots` (for tests that need pattern bindings resolved).
    fn fn_def_p_with_extra(
        name: &str,
        param_type: &str,
        return_type: &str,
        body_expr: Spanned<Expr>,
        extra_slots: &[(&str, u16)],
    ) -> FnDef {
        let mut slots = HashMap::new();
        slots.insert("p".to_string(), 0u16);
        for (n, s) in extra_slots {
            slots.insert((*n).to_string(), *s);
        }
        FnDef {
            name: name.to_string(),
            line: 1,
            params: vec![("p".to_string(), param_type.to_string())],
            return_type: return_type.to_string(),
            effects: vec![],
            desc: None,
            body: Arc::new(FnBody::Block(vec![Stmt::Expr(body_expr)])),
            resolution: Some(FnResolution {
                local_count: (1 + extra_slots.len()) as u16,
                local_slots: Arc::new(slots),
            }),
        }
    }

    /// Caller fn `main()` whose body is a single Stmt::Expr `expr`. No
    /// params; resolution carries an empty slot map.
    fn caller_main(expr: Spanned<Expr>) -> FnDef {
        FnDef {
            name: "main".to_string(),
            line: 1,
            params: vec![],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: Arc::new(FnBody::Block(vec![Stmt::Expr(expr)])),
            resolution: Some(FnResolution {
                local_count: 0,
                local_slots: Arc::new(HashMap::new()),
            }),
        }
    }

    /// Pull the single Stmt::Expr out of a fn body for assertions.
    fn body_expr(fd: &FnDef) -> Expr {
        let FnBody::Block(stmts) = fd.body.as_ref();
        match &stmts[0] {
            Stmt::Expr(s) => s.node.clone(),
            other => panic!("expected Stmt::Expr, got {other:?}"),
        }
    }

    fn count_record_creates(expr: &Expr) -> usize {
        let mut n = 0;
        walk_expr_with_context(expr, false, &mut |e, _| {
            if matches!(e, Expr::RecordCreate { .. }) {
                n += 1;
            }
        });
        n
    }

    // ── classify_fn — RecordAccess shape ────────────────────────────

    #[test]
    fn classify_record_access_eligible_simple() {
        // fn distance(p: Point) -> Float = p.x
        let fd = fn_def_p("distance", "Point", "Float", attr(resolved("p", 0), "x"));
        match classify_fn(&fd) {
            Some(InlineCandidate::RecordAccess { param_slot, .. }) => {
                assert_eq!(param_slot, 0);
            }
            other => panic!("expected RecordAccess, got {other:?}"),
        }
    }

    #[test]
    fn classify_record_access_eligible_nested_attr() {
        // fn area(p: Point) -> Float = p.x + p.y
        let fd = fn_def_p(
            "area",
            "Point",
            "Float",
            add(attr(resolved("p", 0), "x"), attr(resolved("p", 0), "y")),
        );
        assert!(matches!(
            classify_fn(&fd),
            Some(InlineCandidate::RecordAccess { .. })
        ));
    }

    #[test]
    fn classify_record_access_disqualifies_bare_param_use() {
        // fn passthrough(p: Point) -> Point = p   (bare Resolved escapes)
        let fd = fn_def_p("passthrough", "Point", "Point", resolved("p", 0));
        assert!(classify_fn(&fd).is_none());
    }

    #[test]
    fn classify_record_access_disqualifies_param_passed_to_fn() {
        // fn wrap(p: Point) -> Int = id(p)        (p is passed bare → escapes)
        let fd = fn_def_p(
            "wrap",
            "Point",
            "Int",
            fn_call(ident("id"), vec![resolved("p", 0)]),
        );
        assert!(classify_fn(&fd).is_none());
    }

    #[test]
    fn classify_disqualifies_multi_param() {
        let mut fd = fn_def_p("two", "Point", "Float", attr(resolved("p", 0), "x"));
        fd.params.push(("q".to_string(), "Point".to_string()));
        assert!(classify_fn(&fd).is_none());
    }

    #[test]
    fn classify_disqualifies_with_effects() {
        let mut fd = fn_def_p("noisy", "Point", "Unit", attr(resolved("p", 0), "x"));
        fd.effects = vec![sp("Console.print".to_string())];
        assert!(classify_fn(&fd).is_none());
    }

    #[test]
    fn classify_disqualifies_with_tail_call() {
        // fn loopy(p: Point) -> Float = TailCall("loopy", [p.x])
        let body = sp(Expr::TailCall(Box::new(crate::ast::TailCallData {
            target: "loopy".to_string(),
            args: vec![attr(resolved("p", 0), "x")],
        })));
        let fd = fn_def_p("loopy", "Point", "Float", body);
        assert!(classify_fn(&fd).is_none());
    }

    #[test]
    fn classify_disqualifies_multi_stmt_body() {
        // body has two statements — only single-Stmt::Expr fns inline.
        let mut fd = fn_def_p("twostmt", "Point", "Float", attr(resolved("p", 0), "x"));
        let extra_stmt = Stmt::Binding("z".to_string(), None, attr(resolved("p", 0), "y"));
        let FnBody::Block(stmts) = Arc::make_mut(&mut fd.body);
        stmts.insert(0, extra_stmt);
        assert!(classify_fn(&fd).is_none());
    }

    #[test]
    fn classify_disqualifies_no_resolution() {
        let mut fd = fn_def_p("unresolved", "Point", "Float", attr(resolved("p", 0), "x"));
        fd.resolution = None;
        assert!(classify_fn(&fd).is_none());
    }

    // ── classify_fn — VariantMatch shape ────────────────────────────

    #[test]
    fn classify_variant_match_eligible_two_arms() {
        // fn unwrap(p: Option<Int>) -> Int =
        //   match p { Option.Some(v) -> v ; Option.None -> 0 }
        let arms = vec![
            arm(
                Pattern::Constructor("Option.Some".to_string(), vec!["v".to_string()]),
                resolved("v", 1),
            ),
            arm(
                Pattern::Constructor("Option.None".to_string(), vec![]),
                lit_int(0),
            ),
        ];
        let body = match_expr(resolved("p", 0), arms);
        let fd = fn_def_p_with_extra("unwrap", "Option<Int>", "Int", body, &[("v", 1)]);
        match classify_fn(&fd) {
            Some(InlineCandidate::VariantMatch {
                arms_by_constructor,
            }) => {
                assert!(arms_by_constructor.contains_key("Option.Some"));
                assert!(arms_by_constructor.contains_key("Option.None"));
            }
            other => panic!("expected VariantMatch, got {other:?}"),
        }
    }

    #[test]
    fn classify_variant_match_disqualifies_wildcard_arm() {
        // Wildcard / Literal arms exit early — pass gives up to keep
        // dispatch shape uniform.
        let arms = vec![
            arm(
                Pattern::Constructor("Option.Some".to_string(), vec!["v".to_string()]),
                resolved("v", 1),
            ),
            arm(Pattern::Wildcard, lit_int(0)),
        ];
        let body = match_expr(resolved("p", 0), arms);
        let fd = fn_def_p_with_extra("unwrap", "Option<Int>", "Int", body, &[("v", 1)]);
        assert!(classify_fn(&fd).is_none());
    }

    #[test]
    fn classify_variant_match_disqualifies_arm_uses_subject_directly() {
        // arm body refers to `p` directly (not just `v`) — bare subject
        // reference disqualifies (would mean the subject escapes).
        let arms = vec![
            arm(
                Pattern::Constructor("Option.Some".to_string(), vec!["v".to_string()]),
                resolved("p", 0),
            ),
            arm(
                Pattern::Constructor("Option.None".to_string(), vec![]),
                lit_int(0),
            ),
        ];
        let body = match_expr(resolved("p", 0), arms);
        let fd = fn_def_p_with_extra("leaky", "Option<Int>", "Int", body, &[("v", 1)]);
        assert!(classify_fn(&fd).is_none());
    }

    // ── run() — call-site rewrites ──────────────────────────────────

    #[test]
    fn run_inlines_record_access_call_site() {
        // distance(p: Point) -> Float = p.x + p.y
        // main() -> Float = distance(Point(x = 1.0, y = 2.0))
        // After run(): main body should be (1.0 + 2.0), no RecordCreate.
        let distance = fn_def_p(
            "distance",
            "Point",
            "Float",
            add(attr(resolved("p", 0), "x"), attr(resolved("p", 0), "y")),
        );
        let main_body = fn_call(
            ident("distance"),
            vec![record_create(
                "Point",
                vec![("x", lit_float(1.0)), ("y", lit_float(2.0))],
            )],
        );
        let main = caller_main(main_body);
        let mut items = vec![TopLevel::FnDef(distance), TopLevel::FnDef(main)];

        let rewrites = run(&mut items);
        assert_eq!(rewrites, 1, "expected one inline");

        let TopLevel::FnDef(main_after) = &items[1] else {
            panic!("expected main fn");
        };
        assert_eq!(
            count_record_creates(&body_expr(main_after)),
            0,
            "RecordCreate should be substituted away"
        );
        assert!(matches!(body_expr(main_after), Expr::BinOp(_, _, _)));
    }

    #[test]
    fn run_skips_self_recursion() {
        // f(p: Point) -> Float = f(Point(x = p.x, y = p.y))
        // The pass must NOT inline f INTO itself — that would loop.
        let f = fn_def_p(
            "f",
            "Point",
            "Float",
            fn_call(
                ident("f"),
                vec![record_create(
                    "Point",
                    vec![
                        ("x", attr(resolved("p", 0), "x")),
                        ("y", attr(resolved("p", 0), "y")),
                    ],
                )],
            ),
        );
        let mut items = vec![TopLevel::FnDef(f)];
        // f's body contains `f(RecordCreate)` — without the self_fn guard
        // the pass would splice f's body into itself, which is recursion-
        // explosion; with the guard, zero rewrites.
        let rewrites = run(&mut items);
        assert_eq!(rewrites, 0, "self-recursion must be skipped");
    }

    #[test]
    fn run_no_rewrite_when_arg_is_not_record_create() {
        // distance is eligible, but the arg is bare `p` (already a
        // Resolved local in the caller) — no RecordCreate to splice
        // through, no rewrite.
        let distance = fn_def_p("distance", "Point", "Float", attr(resolved("p", 0), "x"));
        // caller(p: Point) -> Float = distance(p)
        let caller = fn_def_p(
            "caller",
            "Point",
            "Float",
            fn_call(ident("distance"), vec![resolved("p", 0)]),
        );
        let mut items = vec![TopLevel::FnDef(distance), TopLevel::FnDef(caller)];
        assert_eq!(run(&mut items), 0);
    }

    #[test]
    fn run_idempotent_after_first_pass() {
        let distance = fn_def_p(
            "distance",
            "Point",
            "Float",
            add(attr(resolved("p", 0), "x"), attr(resolved("p", 0), "y")),
        );
        let main_body = fn_call(
            ident("distance"),
            vec![record_create(
                "Point",
                vec![("x", lit_float(1.0)), ("y", lit_float(2.0))],
            )],
        );
        let main = caller_main(main_body);
        let mut items = vec![TopLevel::FnDef(distance), TopLevel::FnDef(main)];

        assert_eq!(run(&mut items), 1, "first pass inlines");
        assert_eq!(run(&mut items), 0, "second pass has nothing left to do");
    }

    #[test]
    fn run_zero_when_no_candidates() {
        // No fns are eligible — pass returns 0 immediately.
        let escapes = fn_def_p("escapes", "Point", "Point", resolved("p", 0));
        let mut items = vec![TopLevel::FnDef(escapes)];
        assert_eq!(run(&mut items), 0);
    }

    #[test]
    fn run_inlines_variant_match_with_constructor_call() {
        // unwrap(p: Option<Int>) -> Int = match p { Option.Some(v) -> v; Option.None -> 0 }
        // main() -> Int = unwrap(Option.Some(42))
        // After run(): main body should be `42` (the Some payload), no
        // FnCall, no Constructor wrapper.
        let arms = vec![
            arm(
                Pattern::Constructor("Option.Some".to_string(), vec!["v".to_string()]),
                resolved("v", 1),
            ),
            arm(
                Pattern::Constructor("Option.None".to_string(), vec![]),
                lit_int(0),
            ),
        ];
        let unwrap = fn_def_p_with_extra(
            "unwrap",
            "Option<Int>",
            "Int",
            match_expr(resolved("p", 0), arms),
            &[("v", 1)],
        );
        // The call site uses `Option.Some(42)` as an Attr-style call.
        let some_call = fn_call(attr(ident("Option"), "Some"), vec![lit_int(42)]);
        let main = caller_main(fn_call(ident("unwrap"), vec![some_call]));
        let mut items = vec![TopLevel::FnDef(unwrap), TopLevel::FnDef(main)];

        let rewrites = run(&mut items);
        assert_eq!(rewrites, 1);

        let TopLevel::FnDef(main_after) = &items[1] else {
            panic!("expected main fn");
        };
        // After splice the body is just the literal payload `42`.
        assert!(matches!(
            body_expr(main_after),
            Expr::Literal(Literal::Int(42))
        ));
    }
}

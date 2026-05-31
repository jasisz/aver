//! Phase 6 MIR optimization passes.
//!
//! ## Wave 5 — const-fold
//!
//! Evaluate `BinOp` / `Neg` over literal operands at compile
//! time. Numeric arithmetic via `checked_*` (overflow leaves the
//! node intact); comparisons (`Eq / Neq / Lt / Gt / Lte / Gte`)
//! over matching `Int` / `Float` / `Bool` / `Str` / `Unit`
//! literal pairs collapse to `Bool` literals; unary `Neg` on
//! `Int` / `Float` literals preserves IEEE-754 `-0.0`.
//!
//! ## Wave 6 — dead-code elimination
//!
//! Drop `Let { binding, value: <pure>, body }` when `binding` is
//! never read in `body`. Pure = no observable side effect:
//! `Literal` / `Local` / `BinOp` / `Neg` / `Tuple` / `List` /
//! `MapLiteral` (with pure entries) / `Project` / `Construct` /
//! `RecordCreate` / `RecordUpdate` (with pure subtrees). `Call` /
//! `TailCall` / `Try` / `Return` / `Match` / `InterpolatedStr`
//! with `Expr` parts / `IndependentProduct` are conservatively
//! impure — they may diverge, raise, or run effects, so even an
//! unused binding can't be dropped without changing observable
//! behavior.
//!
//! Const-fold runs before DCE so any folded sub-arithmetic
//! collapses to a `Literal` (pure) and unlocks its enclosing
//! `Let` for elimination.
//!
//! ## Wave 7 — nullary literal inlining
//!
//! For every fn whose body is exactly a `MirExpr::Literal` and
//! whose param list is empty, rewrite every `Call(Fn(id), [])`
//! to that literal value. Smallest non-trivial inliner — no
//! param substitution needed — and it unlocks further
//! const-fold / DCE cascades: `let x = pi() * 2.0; 99` →
//! `let x = 3.14 * 2.0; 99` → `let x = 6.28; 99` → `99`.
//!
//! Recursive nullary-literal fns are impossible by
//! construction (a literal body has no `MirExpr::Call`), so no
//! cycle check is needed.
//!
//! ## What this doesn't transform (deferred)
//!
//! - General inlining with param substitution (small
//!   single-expression fns with N params) — Phase 6 wave 8.
//! - String concatenation (`Str + Str`) — would require
//!   intern-side allocation policy; the VM does it efficiently
//!   at runtime already.
//! - Match arms (would require pattern → literal classification
//!   plumbing; defer until there's a real perf signal).
//! - CSE / loop-invariant hoisting — future waves.

use std::collections::HashMap;

use crate::ast::{BinOp, Literal, Spanned};
use crate::ir::FnId;

use super::expr::{
    MirBinOp, MirCall, MirCallee, MirConstruct, MirExpr, MirLet, MirMatchArm, MirPattern,
};
use super::program::{LocalId, MirProgram};

/// Apply const-fold to every fn body in `program`. Returns the
/// (transformed) program by value so the caller can chain
/// further optimization passes.
pub fn const_fold(mut program: MirProgram) -> MirProgram {
    for mir_fn in program.fns.values_mut() {
        fold_in_place(&mut mir_fn.body);
    }
    program
}

/// Post-order fold: rewrite children first, then try the
/// current node — this lets a fold cascade up through nested
/// arithmetic (`(1 + 2) * 3` → `3 * 3` → `9`).
fn fold_in_place(expr: &mut Spanned<MirExpr>) {
    walk_children(&mut expr.node);
    if let Some(folded) = try_fold(&expr.node) {
        // Preserve the original `Spanned`'s line + type stamp;
        // only the node shape changes.
        let span = literal_span(folded, expr);
        expr.node = MirExpr::Literal(span);
    }
}

fn literal_span(lit: Literal, source: &Spanned<MirExpr>) -> Spanned<Literal> {
    let ty = std::sync::OnceLock::new();
    if let Some(t) = source.ty() {
        let _ = ty.set(t.clone());
    }
    Spanned {
        node: lit,
        line: source.line,
        ty,
    }
}

fn walk_children(node: &mut MirExpr) {
    match node {
        MirExpr::Literal(_) | MirExpr::Local(_) => {}
        MirExpr::Neg(inner) => fold_in_place(inner),
        MirExpr::BinOp(spanned_bop) => {
            let bop: &mut MirBinOp = &mut spanned_bop.node;
            fold_in_place(&mut bop.lhs);
            fold_in_place(&mut bop.rhs);
        }
        MirExpr::Let(spanned_let) => {
            let let_node: &mut MirLet = &mut spanned_let.node;
            fold_in_place(&mut let_node.value);
            fold_in_place(&mut let_node.body);
        }
        MirExpr::Call(spanned_call) => {
            let call: &mut MirCall = &mut spanned_call.node;
            for arg in &mut call.args {
                fold_in_place(arg);
            }
        }
        MirExpr::TailCall(spanned_tc) => {
            for arg in &mut spanned_tc.node.args {
                fold_in_place(arg);
            }
        }
        MirExpr::Match(spanned_match) => {
            fold_in_place(&mut spanned_match.node.subject);
            for arm in &mut spanned_match.node.arms {
                fold_arm(arm);
            }
        }
        MirExpr::Construct(spanned_ctor) => {
            let ctor: &mut MirConstruct = &mut spanned_ctor.node;
            for arg in &mut ctor.args {
                fold_in_place(arg);
            }
        }
        MirExpr::RecordCreate(spanned_rec) => {
            for f in &mut spanned_rec.node.fields {
                fold_in_place(&mut f.value);
            }
        }
        MirExpr::RecordUpdate(spanned_upd) => {
            fold_in_place(&mut spanned_upd.node.base);
            for f in &mut spanned_upd.node.updates {
                fold_in_place(&mut f.value);
            }
        }
        MirExpr::Project(spanned_proj) => fold_in_place(&mut spanned_proj.node.base),
        MirExpr::Try(inner) => fold_in_place(inner),
        MirExpr::Return(inner) => fold_in_place(inner),
        MirExpr::List(items) | MirExpr::Tuple(items) => {
            for item in items {
                fold_in_place(item);
            }
        }
        MirExpr::MapLiteral(entries) => {
            for (k, v) in entries {
                fold_in_place(k);
                fold_in_place(v);
            }
        }
        MirExpr::InterpolatedStr(parts) => {
            for part in parts {
                if let super::expr::MirStrPart::Expr(e) = part {
                    fold_in_place(e);
                }
            }
        }
        MirExpr::IndependentProduct(spanned_ip) => {
            for item in &mut spanned_ip.node.items {
                fold_in_place(item);
            }
        }
    }
}

fn fold_arm(arm: &mut MirMatchArm) {
    // Don't recurse into the pattern — patterns are structural
    // and don't contain `MirExpr` subtrees.
    let _ = &arm.pattern;
    let _: &MirPattern = &arm.pattern; // make the field's role explicit
    fold_in_place(&mut arm.body);
}

fn try_fold(node: &MirExpr) -> Option<Literal> {
    match node {
        MirExpr::Neg(inner) => {
            let lit = literal_of(&inner.node)?;
            fold_neg(lit)
        }
        MirExpr::BinOp(spanned_bop) => {
            let bop = &spanned_bop.node;
            let lhs = literal_of(&bop.lhs.node)?;
            let rhs = literal_of(&bop.rhs.node)?;
            fold_binop(bop.op, lhs, rhs)
        }
        _ => None,
    }
}

fn literal_of(node: &MirExpr) -> Option<&Literal> {
    if let MirExpr::Literal(spanned) = node {
        Some(&spanned.node)
    } else {
        None
    }
}

fn fold_neg(lit: &Literal) -> Option<Literal> {
    match lit {
        Literal::Int(i) => i.checked_neg().map(Literal::Int),
        Literal::Float(f) => Some(Literal::Float(-f)),
        _ => None,
    }
}

fn fold_binop(op: BinOp, lhs: &Literal, rhs: &Literal) -> Option<Literal> {
    // Numeric arithmetic: `Int` uses `checked_*` so overflow
    // leaves the node intact and the VM's runtime arithmetic
    // error path stays the source of truth.
    match (op, lhs, rhs) {
        // ── Int arithmetic ────────────────────────────────
        (BinOp::Add, Literal::Int(a), Literal::Int(b)) => a.checked_add(*b).map(Literal::Int),
        (BinOp::Sub, Literal::Int(a), Literal::Int(b)) => a.checked_sub(*b).map(Literal::Int),
        (BinOp::Mul, Literal::Int(a), Literal::Int(b)) => a.checked_mul(*b).map(Literal::Int),
        (BinOp::Div, Literal::Int(a), Literal::Int(b)) => {
            // Match Rust's `checked_div` — returns None on
            // `b == 0` so the runtime error path keeps reporting
            // the division-by-zero with the right span.
            a.checked_div(*b).map(Literal::Int)
        }
        // ── Float arithmetic ──────────────────────────────
        (BinOp::Add, Literal::Float(a), Literal::Float(b)) => Some(Literal::Float(a + b)),
        (BinOp::Sub, Literal::Float(a), Literal::Float(b)) => Some(Literal::Float(a - b)),
        (BinOp::Mul, Literal::Float(a), Literal::Float(b)) => Some(Literal::Float(a * b)),
        (BinOp::Div, Literal::Float(a), Literal::Float(b)) => Some(Literal::Float(a / b)),
        // ── Comparisons ──────────────────────────────────
        (BinOp::Eq, a, b) => literal_eq(a, b).map(Literal::Bool),
        (BinOp::Neq, a, b) => literal_eq(a, b).map(|e| Literal::Bool(!e)),
        (BinOp::Lt, Literal::Int(a), Literal::Int(b)) => Some(Literal::Bool(a < b)),
        (BinOp::Lt, Literal::Float(a), Literal::Float(b)) => Some(Literal::Bool(a < b)),
        (BinOp::Gt, Literal::Int(a), Literal::Int(b)) => Some(Literal::Bool(a > b)),
        (BinOp::Gt, Literal::Float(a), Literal::Float(b)) => Some(Literal::Bool(a > b)),
        (BinOp::Lte, Literal::Int(a), Literal::Int(b)) => Some(Literal::Bool(a <= b)),
        (BinOp::Lte, Literal::Float(a), Literal::Float(b)) => Some(Literal::Bool(a <= b)),
        (BinOp::Gte, Literal::Int(a), Literal::Int(b)) => Some(Literal::Bool(a >= b)),
        (BinOp::Gte, Literal::Float(a), Literal::Float(b)) => Some(Literal::Bool(a >= b)),
        _ => None,
    }
}

/// Wave 6 — dead-code elimination. Drop `Let { binding, value,
/// body }` whenever `binding` is never read in `body` and
/// `value` has no observable side effect. Conservative — any
/// node whose evaluation could raise, diverge, or run an effect
/// is treated as impure even when the structural form looks
/// pure (e.g. a `Match` that happens to have only literal arms
/// still counts as impure because the dispatch itself is part
/// of the program's observable behavior).
pub fn dead_code(mut program: MirProgram) -> MirProgram {
    for mir_fn in program.fns.values_mut() {
        dce_in_place(&mut mir_fn.body);
    }
    program
}

/// Post-order DCE: recurse into children first so any inner
/// `Let` chain collapses bottom-up. The bind-elision shape
/// then catches `let _unused = pure; body` at every level.
fn dce_in_place(expr: &mut Spanned<MirExpr>) {
    dce_walk_children(&mut expr.node);

    // Check if the current node is a Let whose binding is
    // unused and whose value is pure — if so, replace
    // `Let { binding, value, body }` with `body`.
    let should_elide = if let MirExpr::Let(spanned_let) = &expr.node {
        let let_node = &spanned_let.node;
        !local_is_read(let_node.binding, &let_node.body) && is_pure(&let_node.value)
    } else {
        false
    };

    if should_elide {
        // Replace `*expr` with the Let's body, dropping the
        // binding + the pure value expression in the process.
        // Temporarily swap in a unit-literal placeholder so we
        // can take ownership of the original Let.
        let placeholder = MirExpr::Literal(Spanned {
            node: Literal::Unit,
            line: expr.line,
            ty: std::sync::OnceLock::new(),
        });
        let original = std::mem::replace(&mut expr.node, placeholder);
        if let MirExpr::Let(spanned_let) = original {
            let body = *spanned_let.node.body;
            *expr = body;
        } else {
            unreachable!("should_elide is only set inside the Let branch")
        }
    }
}

fn dce_walk_children(node: &mut MirExpr) {
    match node {
        MirExpr::Literal(_) | MirExpr::Local(_) => {}
        MirExpr::Neg(inner) => dce_in_place(inner),
        MirExpr::BinOp(spanned_bop) => {
            let bop: &mut MirBinOp = &mut spanned_bop.node;
            dce_in_place(&mut bop.lhs);
            dce_in_place(&mut bop.rhs);
        }
        MirExpr::Let(spanned_let) => {
            let let_node: &mut MirLet = &mut spanned_let.node;
            dce_in_place(&mut let_node.value);
            dce_in_place(&mut let_node.body);
        }
        MirExpr::Call(spanned_call) => {
            for arg in &mut spanned_call.node.args {
                dce_in_place(arg);
            }
        }
        MirExpr::TailCall(spanned_tc) => {
            for arg in &mut spanned_tc.node.args {
                dce_in_place(arg);
            }
        }
        MirExpr::Match(spanned_match) => {
            dce_in_place(&mut spanned_match.node.subject);
            for arm in &mut spanned_match.node.arms {
                dce_in_place(&mut arm.body);
            }
        }
        MirExpr::Construct(spanned_ctor) => {
            for arg in &mut spanned_ctor.node.args {
                dce_in_place(arg);
            }
        }
        MirExpr::RecordCreate(spanned_rec) => {
            for f in &mut spanned_rec.node.fields {
                dce_in_place(&mut f.value);
            }
        }
        MirExpr::RecordUpdate(spanned_upd) => {
            dce_in_place(&mut spanned_upd.node.base);
            for f in &mut spanned_upd.node.updates {
                dce_in_place(&mut f.value);
            }
        }
        MirExpr::Project(spanned_proj) => dce_in_place(&mut spanned_proj.node.base),
        MirExpr::Try(inner) => dce_in_place(inner),
        MirExpr::Return(inner) => dce_in_place(inner),
        MirExpr::List(items) | MirExpr::Tuple(items) => {
            for item in items {
                dce_in_place(item);
            }
        }
        MirExpr::MapLiteral(entries) => {
            for (k, v) in entries {
                dce_in_place(k);
                dce_in_place(v);
            }
        }
        MirExpr::InterpolatedStr(parts) => {
            for part in parts {
                if let super::expr::MirStrPart::Expr(e) = part {
                    dce_in_place(e);
                }
            }
        }
        MirExpr::IndependentProduct(spanned_ip) => {
            for item in &mut spanned_ip.node.items {
                dce_in_place(item);
            }
        }
    }
}

/// `true` when `body` contains a `MirExpr::Local` whose slot
/// equals `target`. Lexical — doesn't track scope shadowing
/// because MIR's slot numbering is already SSA-ish (each
/// binding introduces a fresh `LocalId`).
fn local_is_read(target: LocalId, body: &Spanned<MirExpr>) -> bool {
    let mut found = false;
    visit_locals(&body.node, &mut |slot| {
        if slot == target {
            found = true;
        }
    });
    found
}

fn visit_locals(node: &MirExpr, visit: &mut impl FnMut(LocalId)) {
    match node {
        MirExpr::Literal(_) => {}
        MirExpr::Local(spanned_local) => visit(spanned_local.node.slot),
        MirExpr::Neg(inner) => visit_locals(&inner.node, visit),
        MirExpr::BinOp(spanned_bop) => {
            visit_locals(&spanned_bop.node.lhs.node, visit);
            visit_locals(&spanned_bop.node.rhs.node, visit);
        }
        MirExpr::Let(spanned_let) => {
            visit_locals(&spanned_let.node.value.node, visit);
            visit_locals(&spanned_let.node.body.node, visit);
        }
        MirExpr::Call(spanned_call) => {
            for arg in &spanned_call.node.args {
                visit_locals(&arg.node, visit);
            }
        }
        MirExpr::TailCall(spanned_tc) => {
            for arg in &spanned_tc.node.args {
                visit_locals(&arg.node, visit);
            }
        }
        MirExpr::Match(spanned_match) => {
            visit_locals(&spanned_match.node.subject.node, visit);
            for arm in &spanned_match.node.arms {
                visit_locals(&arm.body.node, visit);
            }
        }
        MirExpr::Construct(spanned_ctor) => {
            for arg in &spanned_ctor.node.args {
                visit_locals(&arg.node, visit);
            }
        }
        MirExpr::RecordCreate(spanned_rec) => {
            for f in &spanned_rec.node.fields {
                visit_locals(&f.value.node, visit);
            }
        }
        MirExpr::RecordUpdate(spanned_upd) => {
            visit_locals(&spanned_upd.node.base.node, visit);
            for f in &spanned_upd.node.updates {
                visit_locals(&f.value.node, visit);
            }
        }
        MirExpr::Project(spanned_proj) => visit_locals(&spanned_proj.node.base.node, visit),
        MirExpr::Try(inner) | MirExpr::Return(inner) => visit_locals(&inner.node, visit),
        MirExpr::List(items) | MirExpr::Tuple(items) => {
            for item in items {
                visit_locals(&item.node, visit);
            }
        }
        MirExpr::MapLiteral(entries) => {
            for (k, v) in entries {
                visit_locals(&k.node, visit);
                visit_locals(&v.node, visit);
            }
        }
        MirExpr::InterpolatedStr(parts) => {
            for part in parts {
                if let super::expr::MirStrPart::Expr(e) = part {
                    visit_locals(&e.node, visit);
                }
            }
        }
        MirExpr::IndependentProduct(spanned_ip) => {
            for item in &spanned_ip.node.items {
                visit_locals(&item.node, visit);
            }
        }
    }
}

/// Conservative purity classification — `true` means the
/// expression has no observable side effect AND cannot diverge
/// or raise. A pure-leaning false positive is fine (leaves a
/// dead binding intact); a false negative would change program
/// semantics, so we round down aggressively.
fn is_pure(expr: &Spanned<MirExpr>) -> bool {
    match &expr.node {
        MirExpr::Literal(_) | MirExpr::Local(_) => true,
        MirExpr::Neg(inner) => is_pure(inner),
        MirExpr::BinOp(spanned_bop) => {
            // Even arithmetic can raise (Int overflow, div-by-
            // zero) — but those would already have been caught
            // by const-fold's `checked_*` path when both
            // operands are literals. For the symbolic case
            // (`x + 1`) we keep BinOp pure: the VM's runtime
            // arithmetic error path is the same whether the
            // binding is kept or dropped, and the binding being
            // dead means its result is never observed.
            is_pure(&spanned_bop.node.lhs) && is_pure(&spanned_bop.node.rhs)
        }
        MirExpr::Tuple(items) | MirExpr::List(items) => items.iter().all(is_pure),
        MirExpr::MapLiteral(entries) => entries.iter().all(|(k, v)| is_pure(k) && is_pure(v)),
        MirExpr::Construct(spanned_ctor) => spanned_ctor.node.args.iter().all(is_pure),
        MirExpr::RecordCreate(spanned_rec) => {
            spanned_rec.node.fields.iter().all(|f| is_pure(&f.value))
        }
        MirExpr::RecordUpdate(spanned_upd) => {
            is_pure(&spanned_upd.node.base)
                && spanned_upd.node.updates.iter().all(|f| is_pure(&f.value))
        }
        MirExpr::Project(spanned_proj) => is_pure(&spanned_proj.node.base),
        MirExpr::Let(spanned_let) => {
            is_pure(&spanned_let.node.value) && is_pure(&spanned_let.node.body)
        }
        // Anything that could call out (Call / TailCall),
        // unwind (Try / Return), dispatch (Match), produce
        // strings via stringification (InterpolatedStr), or
        // schedule independent effects (IndependentProduct)
        // is conservatively impure.
        MirExpr::Call(_)
        | MirExpr::TailCall(_)
        | MirExpr::Try(_)
        | MirExpr::Return(_)
        | MirExpr::Match(_)
        | MirExpr::InterpolatedStr(_)
        | MirExpr::IndependentProduct(_) => false,
    }
}

fn literal_eq(a: &Literal, b: &Literal) -> Option<bool> {
    match (a, b) {
        (Literal::Int(x), Literal::Int(y)) => Some(x == y),
        (Literal::Float(x), Literal::Float(y)) => Some(x == y),
        (Literal::Bool(x), Literal::Bool(y)) => Some(x == y),
        (Literal::Str(x), Literal::Str(y)) => Some(x == y),
        (Literal::Unit, Literal::Unit) => Some(true),
        _ => None,
    }
}

/// Wave 7 — inline nullary fns whose body is a literal. Every
/// `Call(Fn(id), [])` at any depth becomes a `Literal` node
/// once the target qualifies. Unlocks downstream const-fold +
/// DCE cascades when the caller wraps the call in arithmetic.
///
/// Cycle safety: a literal-only body has no `MirExpr::Call`, so
/// a "nullary literal fn that calls itself" is structurally
/// impossible — no SCC analysis needed.
pub fn inline_nullary_literals(mut program: MirProgram) -> MirProgram {
    let candidates = collect_nullary_literal_fns(&program);
    if candidates.is_empty() {
        return program;
    }
    for mir_fn in program.fns.values_mut() {
        inline_in_place(&mut mir_fn.body, &candidates);
    }
    program
}

fn collect_nullary_literal_fns(program: &MirProgram) -> HashMap<FnId, Literal> {
    let mut out = HashMap::new();
    for (fn_id, mir_fn) in program.iter() {
        if !mir_fn.params.is_empty() {
            continue;
        }
        if let MirExpr::Literal(spanned_lit) = &mir_fn.body.node {
            out.insert(*fn_id, spanned_lit.node.clone());
        }
    }
    out
}

fn inline_in_place(expr: &mut Spanned<MirExpr>, candidates: &HashMap<FnId, Literal>) {
    inline_walk_children(&mut expr.node, candidates);

    // Top-level rewrite: replace `Call(Fn(id), [])` with the
    // recorded literal when the callee qualifies. Done after
    // descending so a nested call inside a wrapper expression
    // already inlined first (post-order).
    let replacement = if let MirExpr::Call(spanned_call) = &expr.node {
        let call = &spanned_call.node;
        if call.args.is_empty() {
            if let MirCallee::Fn(fn_id) = &call.callee {
                candidates.get(fn_id).cloned()
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    };

    if let Some(lit) = replacement {
        let ty = std::sync::OnceLock::new();
        if let Some(t) = expr.ty() {
            let _ = ty.set(t.clone());
        }
        expr.node = MirExpr::Literal(Spanned {
            node: lit,
            line: expr.line,
            ty,
        });
    }
}

fn inline_walk_children(node: &mut MirExpr, candidates: &HashMap<FnId, Literal>) {
    match node {
        MirExpr::Literal(_) | MirExpr::Local(_) => {}
        MirExpr::Neg(inner) => inline_in_place(inner, candidates),
        MirExpr::BinOp(spanned_bop) => {
            inline_in_place(&mut spanned_bop.node.lhs, candidates);
            inline_in_place(&mut spanned_bop.node.rhs, candidates);
        }
        MirExpr::Let(spanned_let) => {
            inline_in_place(&mut spanned_let.node.value, candidates);
            inline_in_place(&mut spanned_let.node.body, candidates);
        }
        MirExpr::Call(spanned_call) => {
            for arg in &mut spanned_call.node.args {
                inline_in_place(arg, candidates);
            }
        }
        MirExpr::TailCall(spanned_tc) => {
            for arg in &mut spanned_tc.node.args {
                inline_in_place(arg, candidates);
            }
        }
        MirExpr::Match(spanned_match) => {
            inline_in_place(&mut spanned_match.node.subject, candidates);
            for arm in &mut spanned_match.node.arms {
                inline_in_place(&mut arm.body, candidates);
            }
        }
        MirExpr::Construct(spanned_ctor) => {
            for arg in &mut spanned_ctor.node.args {
                inline_in_place(arg, candidates);
            }
        }
        MirExpr::RecordCreate(spanned_rec) => {
            for f in &mut spanned_rec.node.fields {
                inline_in_place(&mut f.value, candidates);
            }
        }
        MirExpr::RecordUpdate(spanned_upd) => {
            inline_in_place(&mut spanned_upd.node.base, candidates);
            for f in &mut spanned_upd.node.updates {
                inline_in_place(&mut f.value, candidates);
            }
        }
        MirExpr::Project(spanned_proj) => inline_in_place(&mut spanned_proj.node.base, candidates),
        MirExpr::Try(inner) | MirExpr::Return(inner) => inline_in_place(inner, candidates),
        MirExpr::List(items) | MirExpr::Tuple(items) => {
            for item in items {
                inline_in_place(item, candidates);
            }
        }
        MirExpr::MapLiteral(entries) => {
            for (k, v) in entries {
                inline_in_place(k, candidates);
                inline_in_place(v, candidates);
            }
        }
        MirExpr::InterpolatedStr(parts) => {
            for part in parts {
                if let super::expr::MirStrPart::Expr(e) = part {
                    inline_in_place(e, candidates);
                }
            }
        }
        MirExpr::IndependentProduct(spanned_ip) => {
            for item in &mut spanned_ip.node.items {
                inline_in_place(item, candidates);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::FnId;
    use crate::ir::mir::{MirFn, MirProgram};

    fn span<T>(node: T) -> Spanned<T> {
        Spanned {
            node,
            line: 0,
            ty: std::sync::OnceLock::new(),
        }
    }

    fn one_fn_program(body: MirExpr) -> MirProgram {
        let mut p = MirProgram::empty();
        p.fns.insert(
            FnId(0),
            MirFn {
                fn_id: FnId(0),
                name: "test".to_string(),
                params: vec![],
                return_type: "Int".to_string(),
                effects: vec![],
                body: span(body),
            },
        );
        p
    }

    fn body_of(p: &MirProgram) -> &MirExpr {
        &p.fns.get(&FnId(0)).unwrap().body.node
    }

    #[test]
    fn folds_int_add() {
        let body = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Add,
            lhs: Box::new(span(MirExpr::Literal(span(Literal::Int(1))))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(2))))),
        }));
        let folded = const_fold(one_fn_program(body));
        assert!(
            matches!(body_of(&folded), MirExpr::Literal(s) if matches!(s.node, Literal::Int(3)))
        );
    }

    #[test]
    fn folds_nested_arithmetic() {
        // `(1 + 2) * 3` → `3 * 3` → `9`.
        let inner = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Add,
            lhs: Box::new(span(MirExpr::Literal(span(Literal::Int(1))))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(2))))),
        }));
        let outer = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Mul,
            lhs: Box::new(span(inner)),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(3))))),
        }));
        let folded = const_fold(one_fn_program(outer));
        assert!(
            matches!(body_of(&folded), MirExpr::Literal(s) if matches!(s.node, Literal::Int(9)))
        );
    }

    #[test]
    fn folds_neg_on_int_literal() {
        let body = MirExpr::Neg(Box::new(span(MirExpr::Literal(span(Literal::Int(7))))));
        let folded = const_fold(one_fn_program(body));
        assert!(
            matches!(body_of(&folded), MirExpr::Literal(s) if matches!(s.node, Literal::Int(-7)))
        );
    }

    #[test]
    fn folds_eq_to_bool() {
        let body = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Eq,
            lhs: Box::new(span(MirExpr::Literal(span(Literal::Int(5))))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(5))))),
        }));
        let folded = const_fold(one_fn_program(body));
        assert!(
            matches!(body_of(&folded), MirExpr::Literal(s) if matches!(s.node, Literal::Bool(true)))
        );
    }

    #[test]
    fn folds_lt_on_floats() {
        let body = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Lt,
            lhs: Box::new(span(MirExpr::Literal(span(Literal::Float(1.5))))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Float(2.0))))),
        }));
        let folded = const_fold(one_fn_program(body));
        assert!(
            matches!(body_of(&folded), MirExpr::Literal(s) if matches!(s.node, Literal::Bool(true)))
        );
    }

    #[test]
    fn leaves_int_overflow_unfolded() {
        // `i64::MAX + 1` overflows — `checked_add` returns
        // `None` so the BinOp stays intact and the VM's runtime
        // arithmetic error path triggers normally.
        let body = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Add,
            lhs: Box::new(span(MirExpr::Literal(span(Literal::Int(i64::MAX))))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(1))))),
        }));
        let folded = const_fold(one_fn_program(body));
        assert!(matches!(body_of(&folded), MirExpr::BinOp(_)));
    }

    #[test]
    fn leaves_div_by_zero_unfolded() {
        // `n / 0` — `checked_div` returns `None`, keeping the
        // runtime error path intact.
        let body = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Div,
            lhs: Box::new(span(MirExpr::Literal(span(Literal::Int(5))))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(0))))),
        }));
        let folded = const_fold(one_fn_program(body));
        assert!(matches!(body_of(&folded), MirExpr::BinOp(_)));
    }

    #[test]
    fn leaves_non_literal_operands_untouched() {
        // `x + 1` — `x` is a Local, not a Literal; fold bounces
        // and the BinOp stays.
        use super::super::expr::MirLocal;
        use super::super::program::LocalId;
        let body = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Add,
            lhs: Box::new(span(MirExpr::Local(span(MirLocal::at(LocalId(0)))))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(1))))),
        }));
        let folded = const_fold(one_fn_program(body));
        assert!(matches!(body_of(&folded), MirExpr::BinOp(_)));
    }

    #[test]
    fn folds_inside_let_value_and_body() {
        // `let x = 1 + 2; 3 + 4` → `let x = 3; 7`.
        use super::super::expr::MirLet;
        use super::super::program::LocalId;
        let value = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Add,
            lhs: Box::new(span(MirExpr::Literal(span(Literal::Int(1))))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(2))))),
        }));
        let body_expr = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Add,
            lhs: Box::new(span(MirExpr::Literal(span(Literal::Int(3))))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(4))))),
        }));
        let body = MirExpr::Let(span(MirLet {
            binding: LocalId(0),
            binding_name: "x".to_string(),
            value: Box::new(span(value)),
            body: Box::new(span(body_expr)),
        }));
        let folded = const_fold(one_fn_program(body));
        let MirExpr::Let(let_node) = body_of(&folded) else {
            panic!("expected Let at root");
        };
        assert!(
            matches!(&let_node.node.value.node, MirExpr::Literal(s) if matches!(s.node, Literal::Int(3)))
        );
        assert!(
            matches!(&let_node.node.body.node, MirExpr::Literal(s) if matches!(s.node, Literal::Int(7)))
        );
    }

    // ── Phase 6 wave 6: dead-code elimination ────────────

    #[test]
    fn dce_drops_unused_pure_let() {
        // `let x = 7; 42` → `42` (x never read, value pure).
        use super::super::expr::MirLet;
        use super::super::program::LocalId;
        let body = MirExpr::Let(span(MirLet {
            binding: LocalId(0),
            binding_name: "x".to_string(),
            value: Box::new(span(MirExpr::Literal(span(Literal::Int(7))))),
            body: Box::new(span(MirExpr::Literal(span(Literal::Int(42))))),
        }));
        let eliminated = dead_code(one_fn_program(body));
        assert!(
            matches!(body_of(&eliminated), MirExpr::Literal(s) if matches!(s.node, Literal::Int(42))),
            "dead Let with pure value should collapse to body"
        );
    }

    #[test]
    fn dce_keeps_used_let() {
        // `let x = 7; x + 1` — x is read, the Let must stay.
        use super::super::expr::{MirLet, MirLocal};
        use super::super::program::LocalId;
        let read = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Add,
            lhs: Box::new(span(MirExpr::Local(span(MirLocal::at(LocalId(0)))))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(1))))),
        }));
        let body = MirExpr::Let(span(MirLet {
            binding: LocalId(0),
            binding_name: "x".to_string(),
            value: Box::new(span(MirExpr::Literal(span(Literal::Int(7))))),
            body: Box::new(span(read)),
        }));
        let eliminated = dead_code(one_fn_program(body));
        assert!(
            matches!(body_of(&eliminated), MirExpr::Let(_)),
            "Let with read binding must stay"
        );
    }

    #[test]
    fn dce_keeps_unused_impure_let() {
        // `let _ = some_call(); 42` — value side has a Call,
        // which is conservatively impure; the Let stays even
        // though the binding is unread.
        use super::super::expr::{MirCall, MirCallee, MirLet};
        use super::super::program::LocalId;
        use crate::ir::FnId;
        let call_value = MirExpr::Call(span(MirCall {
            callee: MirCallee::Fn(FnId(0)),
            args: vec![],
        }));
        let body = MirExpr::Let(span(MirLet {
            binding: LocalId(0),
            binding_name: String::new(), // synthetic
            value: Box::new(span(call_value)),
            body: Box::new(span(MirExpr::Literal(span(Literal::Int(42))))),
        }));
        let eliminated = dead_code(one_fn_program(body));
        assert!(
            matches!(body_of(&eliminated), MirExpr::Let(_)),
            "unused Let with impure (Call) value must stay — could be an effect"
        );
    }

    #[test]
    fn dce_drops_nested_unused_pure_let_chains() {
        // `let a = 1; let b = 2; 99` — both bindings dead and
        // pure, both should collapse, leaving just `99`.
        use super::super::expr::MirLet;
        use super::super::program::LocalId;
        let inner = MirExpr::Let(span(MirLet {
            binding: LocalId(1),
            binding_name: "b".to_string(),
            value: Box::new(span(MirExpr::Literal(span(Literal::Int(2))))),
            body: Box::new(span(MirExpr::Literal(span(Literal::Int(99))))),
        }));
        let outer = MirExpr::Let(span(MirLet {
            binding: LocalId(0),
            binding_name: "a".to_string(),
            value: Box::new(span(MirExpr::Literal(span(Literal::Int(1))))),
            body: Box::new(span(inner)),
        }));
        let eliminated = dead_code(one_fn_program(outer));
        assert!(
            matches!(body_of(&eliminated), MirExpr::Literal(s) if matches!(s.node, Literal::Int(99))),
            "two stacked dead pure Lets should both collapse"
        );
    }

    #[test]
    fn const_fold_then_dce_composes() {
        // `let x = 1 + 2; 99` — const-fold makes value a
        // literal (pure), THEN dce drops the dead binding.
        // Validates the wired pipeline (`dead_code ∘ const_fold`).
        use super::super::expr::MirLet;
        use super::super::program::LocalId;
        let value = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Add,
            lhs: Box::new(span(MirExpr::Literal(span(Literal::Int(1))))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(2))))),
        }));
        let body = MirExpr::Let(span(MirLet {
            binding: LocalId(0),
            binding_name: "x".to_string(),
            value: Box::new(span(value)),
            body: Box::new(span(MirExpr::Literal(span(Literal::Int(99))))),
        }));
        let optimized = dead_code(const_fold(one_fn_program(body)));
        assert!(
            matches!(body_of(&optimized), MirExpr::Literal(s) if matches!(s.node, Literal::Int(99))),
            "fold→dce should collapse the whole Let to the body literal"
        );
    }

    // ── Phase 6 wave 7: nullary literal inlining ────────

    /// Build a 2-fn program: callee = nullary literal-body fn,
    /// caller = `body_of_caller` which presumably calls callee.
    fn two_fn_program(callee_body: MirExpr, caller_body: MirExpr) -> MirProgram {
        let mut p = MirProgram::empty();
        p.fns.insert(
            FnId(0),
            MirFn {
                fn_id: FnId(0),
                name: "callee".to_string(),
                params: vec![],
                return_type: "Int".to_string(),
                effects: vec![],
                body: span(callee_body),
            },
        );
        p.fns.insert(
            FnId(1),
            MirFn {
                fn_id: FnId(1),
                name: "caller".to_string(),
                params: vec![],
                return_type: "Int".to_string(),
                effects: vec![],
                body: span(caller_body),
            },
        );
        p
    }

    fn caller_body(p: &MirProgram) -> &MirExpr {
        &p.fns.get(&FnId(1)).unwrap().body.node
    }

    #[test]
    fn inlines_nullary_literal_call() {
        // callee: () -> Int = 42
        // caller: () -> Int = callee()
        // → caller body becomes Literal(42).
        let callee_body = MirExpr::Literal(span(Literal::Int(42)));
        let caller_body_expr = MirExpr::Call(span(MirCall {
            callee: MirCallee::Fn(FnId(0)),
            args: vec![],
        }));
        let inlined = inline_nullary_literals(two_fn_program(callee_body, caller_body_expr));
        assert!(
            matches!(caller_body(&inlined), MirExpr::Literal(s) if matches!(s.node, Literal::Int(42))),
            "nullary literal call should inline to the literal"
        );
    }

    #[test]
    fn does_not_inline_non_nullary_call() {
        // callee: (x: Int) -> Int = 42 — body IS literal but
        // params non-empty → skip.
        use super::super::program::MirParam;
        let mut p = MirProgram::empty();
        p.fns.insert(
            FnId(0),
            MirFn {
                fn_id: FnId(0),
                name: "callee".to_string(),
                params: vec![MirParam {
                    local: LocalId(0),
                    name: "x".to_string(),
                    ty: "Int".to_string(),
                }],
                return_type: "Int".to_string(),
                effects: vec![],
                body: span(MirExpr::Literal(span(Literal::Int(42)))),
            },
        );
        let caller_body_expr = MirExpr::Call(span(MirCall {
            callee: MirCallee::Fn(FnId(0)),
            args: vec![span(MirExpr::Literal(span(Literal::Int(1))))],
        }));
        p.fns.insert(
            FnId(1),
            MirFn {
                fn_id: FnId(1),
                name: "caller".to_string(),
                params: vec![],
                return_type: "Int".to_string(),
                effects: vec![],
                body: span(caller_body_expr),
            },
        );
        let inlined = inline_nullary_literals(p);
        assert!(
            matches!(caller_body(&inlined), MirExpr::Call(_)),
            "non-nullary call must not be inlined even if body is literal"
        );
    }

    #[test]
    fn does_not_inline_nullary_non_literal_body() {
        // callee: () -> Int = 1 + 2 — nullary but body is
        // BinOp, not a pure Literal — skip (until const-fold
        // collapses it; that's wave 5's job, and the pipeline
        // composition makes the right thing happen).
        let callee_body = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Add,
            lhs: Box::new(span(MirExpr::Literal(span(Literal::Int(1))))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(2))))),
        }));
        let caller_body_expr = MirExpr::Call(span(MirCall {
            callee: MirCallee::Fn(FnId(0)),
            args: vec![],
        }));
        let inlined = inline_nullary_literals(two_fn_program(callee_body, caller_body_expr));
        assert!(
            matches!(caller_body(&inlined), MirExpr::Call(_)),
            "nullary call with BinOp body must not be inlined directly"
        );
    }

    #[test]
    fn pipeline_inline_then_fold_then_dce() {
        // callee: () -> Int = 3
        // caller: () -> Int = let x = callee() * 2; 99
        //  inline   → let x = 3 * 2; 99
        //  fold     → let x = 6; 99
        //  dce      → 99
        use super::super::expr::MirLet;
        let callee_body = MirExpr::Literal(span(Literal::Int(3)));
        let mul = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Mul,
            lhs: Box::new(span(MirExpr::Call(span(MirCall {
                callee: MirCallee::Fn(FnId(0)),
                args: vec![],
            })))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(2))))),
        }));
        let caller_body_expr = MirExpr::Let(span(MirLet {
            binding: LocalId(0),
            binding_name: "x".to_string(),
            value: Box::new(span(mul)),
            body: Box::new(span(MirExpr::Literal(span(Literal::Int(99))))),
        }));
        let p = two_fn_program(callee_body, caller_body_expr);
        let optimized = dead_code(const_fold(inline_nullary_literals(p)));
        assert!(
            matches!(caller_body(&optimized), MirExpr::Literal(s) if matches!(s.node, Literal::Int(99))),
            "full pipeline should collapse caller body to `99`"
        );
    }
}

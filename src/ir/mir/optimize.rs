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
//! ## Wave 10 — branch collapse
//!
//! After `bool_match_to_if` (wave 9) lifts qualifying matches
//! into `MirExpr::IfThenElse`, this pass collapses any
//! `IfThenElse` whose `cond` is a literal `Bool` directly to
//! the surviving branch. Trivially correct: when the condition
//! is `Literal(Bool(true))` only the `then_branch` ever
//! evaluates; same logic for `false` and the `else_branch`.
//!
//! Composes naturally with const-fold: a folded comparison
//! (`Literal(5) == Literal(5)` → `Literal(true)`) feeds into
//! the surrounding `IfThenElse` and lets this pass drop the
//! dead branch on the next sweep.
//!
//! ## Wave 8 — algebraic identities
//!
//! Rewrite `BinOp` / `Neg` shapes whose result is determined by
//! an algebraic identity over `Int`:
//!
//! - `x + 0` / `0 + x` / `x - 0` → `x`
//! - `x * 1` / `1 * x` / `x / 1` → `x`
//! - `x * 0` / `0 * x` → `0` (when `x` is pure — otherwise the
//!   multiplication's side effects must run)
//! - `Neg(Neg(x))` → `x`
//!
//! Float is deliberately *not* simplified — `x + 0.0` differs
//! from `x` for `x = -0.0` (IEEE-754 signed-zero), `x * 0.0`
//! differs for `x = NaN` / ±∞, and so on. Skipping float keeps
//! the simplifier sound without a per-shape proof of safety.
//!
//! No string / list / map rewrites; those would need
//! reference-equality reasoning the optimizer doesn't carry.
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
        MirExpr::IfThenElse(spanned_ite) => {
            fold_in_place(&mut spanned_ite.node.cond);
            fold_in_place(&mut spanned_ite.node.then_branch);
            fold_in_place(&mut spanned_ite.node.else_branch);
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
        MirExpr::IfThenElse(spanned_ite) => {
            dce_in_place(&mut spanned_ite.node.cond);
            dce_in_place(&mut spanned_ite.node.then_branch);
            dce_in_place(&mut spanned_ite.node.else_branch);
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
        MirExpr::IfThenElse(spanned_ite) => {
            visit_locals(&spanned_ite.node.cond.node, visit);
            visit_locals(&spanned_ite.node.then_branch.node, visit);
            visit_locals(&spanned_ite.node.else_branch.node, visit);
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
        // IfThenElse is pure iff all three subtrees are pure —
        // direct conditional, no dispatch overhead unlike Match.
        MirExpr::IfThenElse(spanned_ite) => {
            is_pure(&spanned_ite.node.cond)
                && is_pure(&spanned_ite.node.then_branch)
                && is_pure(&spanned_ite.node.else_branch)
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

/// Wave 8 — algebraic identity simplification. Rewrite
/// `BinOp` / `Neg` shapes whose result is determined by a
/// purely algebraic identity over `Int`. Float is skipped
/// (signed-zero / NaN), other types untouched.
pub fn algebraic_simplify(mut program: MirProgram) -> MirProgram {
    for mir_fn in program.fns.values_mut() {
        algebraic_in_place(&mut mir_fn.body);
    }
    program
}

fn algebraic_in_place(expr: &mut Spanned<MirExpr>) {
    algebraic_walk_children(&mut expr.node);

    // Try to simplify the current node post-order.
    if let Some(replacement) = try_algebraic(&expr.node) {
        match replacement {
            AlgReplace::Identity(side) => {
                // Replace the BinOp with whichever operand
                // survives the identity (lhs or rhs).
                if let MirExpr::BinOp(spanned_bop) = std::mem::replace(
                    &mut expr.node,
                    MirExpr::Literal(Spanned {
                        node: Literal::Unit,
                        line: expr.line,
                        ty: std::sync::OnceLock::new(),
                    }),
                ) {
                    let bop = spanned_bop.node;
                    let surviving = match side {
                        Side::Lhs => *bop.lhs,
                        Side::Rhs => *bop.rhs,
                    };
                    *expr = surviving;
                } else {
                    unreachable!("AlgReplace::Identity only set inside BinOp branch")
                }
            }
            AlgReplace::Literal(lit) => {
                expr.node = MirExpr::Literal(Spanned {
                    node: lit,
                    line: expr.line,
                    ty: std::sync::OnceLock::new(),
                });
            }
            AlgReplace::UnwrapNeg => {
                // `Neg(Neg(inner))` → `inner`
                if let MirExpr::Neg(outer) = std::mem::replace(
                    &mut expr.node,
                    MirExpr::Literal(Spanned {
                        node: Literal::Unit,
                        line: expr.line,
                        ty: std::sync::OnceLock::new(),
                    }),
                ) {
                    if let MirExpr::Neg(inner) = outer.node {
                        *expr = *inner;
                    } else {
                        unreachable!("UnwrapNeg only set when outer is Neg(Neg)")
                    }
                } else {
                    unreachable!("UnwrapNeg only set inside Neg branch")
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Side {
    Lhs,
    Rhs,
}

enum AlgReplace {
    /// Keep one operand from the BinOp.
    Identity(Side),
    /// Replace the whole node with a literal.
    Literal(Literal),
    /// Unwrap a `Neg(Neg(x))` to `x`.
    UnwrapNeg,
}

fn try_algebraic(node: &MirExpr) -> Option<AlgReplace> {
    match node {
        MirExpr::Neg(inner) => {
            if matches!(&inner.node, MirExpr::Neg(_)) {
                Some(AlgReplace::UnwrapNeg)
            } else {
                None
            }
        }
        MirExpr::BinOp(spanned_bop) => {
            let bop = &spanned_bop.node;
            try_algebraic_binop(bop.op, &bop.lhs, &bop.rhs)
        }
        _ => None,
    }
}

fn try_algebraic_binop(
    op: BinOp,
    lhs: &Spanned<MirExpr>,
    rhs: &Spanned<MirExpr>,
) -> Option<AlgReplace> {
    let lhs_int = int_literal(&lhs.node);
    let rhs_int = int_literal(&rhs.node);
    match op {
        BinOp::Add => {
            // x + 0 → x
            if rhs_int == Some(0) {
                return Some(AlgReplace::Identity(Side::Lhs));
            }
            // 0 + x → x
            if lhs_int == Some(0) {
                return Some(AlgReplace::Identity(Side::Rhs));
            }
            None
        }
        BinOp::Sub => {
            // x - 0 → x. `0 - x` is NOT `-x` here because we'd
            // produce a Neg node and that's a different shape;
            // const-fold / Neg-Neg already covers the literal
            // cases, and a symbolic `-x` rewrite is a separate
            // peephole that doesn't belong here.
            if rhs_int == Some(0) {
                return Some(AlgReplace::Identity(Side::Lhs));
            }
            None
        }
        BinOp::Mul => {
            // x * 1 → x
            if rhs_int == Some(1) {
                return Some(AlgReplace::Identity(Side::Lhs));
            }
            // 1 * x → x
            if lhs_int == Some(1) {
                return Some(AlgReplace::Identity(Side::Rhs));
            }
            // x * 0 → 0 / 0 * x → 0, but ONLY when the
            // surviving operand is pure — otherwise dropping it
            // would skip its side effect.
            if rhs_int == Some(0) && is_pure(lhs) {
                return Some(AlgReplace::Literal(Literal::Int(0)));
            }
            if lhs_int == Some(0) && is_pure(rhs) {
                return Some(AlgReplace::Literal(Literal::Int(0)));
            }
            None
        }
        BinOp::Div => {
            // x / 1 → x. `0 / x` stays as-is (div-by-zero
            // diagnostic must still fire when `x = 0`).
            if rhs_int == Some(1) {
                return Some(AlgReplace::Identity(Side::Lhs));
            }
            None
        }
        // Comparisons / Eq / Neq don't get algebraic
        // rewrites here — those need structural equality
        // (`x == x` → `true`), which is a separate analysis.
        _ => None,
    }
}

fn int_literal(node: &MirExpr) -> Option<i64> {
    if let MirExpr::Literal(spanned) = node
        && let Literal::Int(i) = spanned.node
    {
        return Some(i);
    }
    None
}

fn algebraic_walk_children(node: &mut MirExpr) {
    match node {
        MirExpr::Literal(_) | MirExpr::Local(_) => {}
        MirExpr::Neg(inner) => algebraic_in_place(inner),
        MirExpr::BinOp(spanned_bop) => {
            algebraic_in_place(&mut spanned_bop.node.lhs);
            algebraic_in_place(&mut spanned_bop.node.rhs);
        }
        MirExpr::Let(spanned_let) => {
            algebraic_in_place(&mut spanned_let.node.value);
            algebraic_in_place(&mut spanned_let.node.body);
        }
        MirExpr::Call(spanned_call) => {
            for arg in &mut spanned_call.node.args {
                algebraic_in_place(arg);
            }
        }
        MirExpr::TailCall(spanned_tc) => {
            for arg in &mut spanned_tc.node.args {
                algebraic_in_place(arg);
            }
        }
        MirExpr::Match(spanned_match) => {
            algebraic_in_place(&mut spanned_match.node.subject);
            for arm in &mut spanned_match.node.arms {
                algebraic_in_place(&mut arm.body);
            }
        }
        MirExpr::IfThenElse(spanned_ite) => {
            algebraic_in_place(&mut spanned_ite.node.cond);
            algebraic_in_place(&mut spanned_ite.node.then_branch);
            algebraic_in_place(&mut spanned_ite.node.else_branch);
        }
        MirExpr::Construct(spanned_ctor) => {
            for arg in &mut spanned_ctor.node.args {
                algebraic_in_place(arg);
            }
        }
        MirExpr::RecordCreate(spanned_rec) => {
            for f in &mut spanned_rec.node.fields {
                algebraic_in_place(&mut f.value);
            }
        }
        MirExpr::RecordUpdate(spanned_upd) => {
            algebraic_in_place(&mut spanned_upd.node.base);
            for f in &mut spanned_upd.node.updates {
                algebraic_in_place(&mut f.value);
            }
        }
        MirExpr::Project(spanned_proj) => algebraic_in_place(&mut spanned_proj.node.base),
        MirExpr::Try(inner) | MirExpr::Return(inner) => algebraic_in_place(inner),
        MirExpr::List(items) | MirExpr::Tuple(items) => {
            for item in items {
                algebraic_in_place(item);
            }
        }
        MirExpr::MapLiteral(entries) => {
            for (k, v) in entries {
                algebraic_in_place(k);
                algebraic_in_place(v);
            }
        }
        MirExpr::InterpolatedStr(parts) => {
            for part in parts {
                if let super::expr::MirStrPart::Expr(e) = part {
                    algebraic_in_place(e);
                }
            }
        }
        MirExpr::IndependentProduct(spanned_ip) => {
            for item in &mut spanned_ip.node.items {
                algebraic_in_place(item);
            }
        }
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
        MirExpr::IfThenElse(spanned_ite) => {
            inline_in_place(&mut spanned_ite.node.cond, candidates);
            inline_in_place(&mut spanned_ite.node.then_branch, candidates);
            inline_in_place(&mut spanned_ite.node.else_branch, candidates);
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

/// Wave 9 — rewrite qualifying two-arm `Bool` match expressions
/// into `IfThenElse`. Recognition shape (mirror of HIR's
/// `try_emit_bool_if_else`):
///
/// - Match has exactly 2 arms
/// - One arm's pattern is `Literal(Bool(true))`, the other is
///   `Literal(Bool(false))` or `Wildcard` (catch-all default)
/// - Bindings are empty (no captured locals)
///
/// When matched, replace with `IfThenElse { cond: subject,
/// then_branch: <true-arm body>, else_branch: <false-arm body> }`.
/// Backends consume only the rewritten form — no per-backend
/// recognition logic.
pub fn bool_match_to_if(mut program: MirProgram) -> MirProgram {
    for mir_fn in program.fns.values_mut() {
        bool_match_in_place(&mut mir_fn.body);
    }
    program
}

fn bool_match_in_place(expr: &mut Spanned<MirExpr>) {
    bool_match_walk_children(&mut expr.node);

    let replacement = if let MirExpr::Match(spanned_match) = &expr.node {
        let m = &spanned_match.node;
        try_bool_match_branches(&m.arms)
    } else {
        None
    };

    if let Some(branch_indices) = replacement {
        let placeholder = MirExpr::Literal(Spanned {
            node: Literal::Unit,
            line: expr.line,
            ty: std::sync::OnceLock::new(),
        });
        let original = std::mem::replace(&mut expr.node, placeholder);
        if let MirExpr::Match(spanned_match) = original {
            let m = spanned_match.node;
            let subject = m.subject;
            let mut arms_iter = m.arms.into_iter();
            // Collect arms in source order so we can index.
            let arms_vec: Vec<MirMatchArm> = arms_iter.by_ref().collect();
            let then_branch = Box::new(arms_vec[branch_indices.true_idx].body.clone());
            let else_branch = Box::new(arms_vec[branch_indices.false_idx].body.clone());
            let ite = super::expr::MirIfThenElse {
                cond: subject,
                then_branch,
                else_branch,
            };
            expr.node = MirExpr::IfThenElse(Spanned {
                node: ite,
                line: expr.line,
                ty: std::sync::OnceLock::new(),
            });
        } else {
            unreachable!("replacement only set inside the Match branch")
        }
    }
}

struct BoolBranchIndices {
    true_idx: usize,
    false_idx: usize,
}

/// Recognize the bool match shape over a 2-arm slice. Returns
/// `Some` with the arm index for each branch on hit, `None`
/// when the shape doesn't qualify.
fn try_bool_match_branches(arms: &[MirMatchArm]) -> Option<BoolBranchIndices> {
    if arms.len() != 2 {
        return None;
    }
    let p0 = &arms[0].pattern;
    let p1 = &arms[1].pattern;
    let p0_bool = bool_pattern(p0);
    let p1_bool = bool_pattern(p1);
    match (p0_bool, p1_bool) {
        // `true → A; false → B` or `true → A; _ → B`
        (Some(BoolPat::True), Some(BoolPat::False))
        | (Some(BoolPat::True), Some(BoolPat::Wildcard)) => Some(BoolBranchIndices {
            true_idx: 0,
            false_idx: 1,
        }),
        // `false → B; true → A` or `_ → B; true → A`
        (Some(BoolPat::False), Some(BoolPat::True))
        | (Some(BoolPat::Wildcard), Some(BoolPat::True)) => Some(BoolBranchIndices {
            true_idx: 1,
            false_idx: 0,
        }),
        _ => None,
    }
}

enum BoolPat {
    True,
    False,
    Wildcard,
}

fn bool_pattern(p: &MirPattern) -> Option<BoolPat> {
    match p {
        MirPattern::Literal(Literal::Bool(true)) => Some(BoolPat::True),
        MirPattern::Literal(Literal::Bool(false)) => Some(BoolPat::False),
        MirPattern::Wildcard => Some(BoolPat::Wildcard),
        _ => None,
    }
}

fn bool_match_walk_children(node: &mut MirExpr) {
    match node {
        MirExpr::Literal(_) | MirExpr::Local(_) => {}
        MirExpr::Neg(inner) => bool_match_in_place(inner),
        MirExpr::BinOp(spanned_bop) => {
            bool_match_in_place(&mut spanned_bop.node.lhs);
            bool_match_in_place(&mut spanned_bop.node.rhs);
        }
        MirExpr::Let(spanned_let) => {
            bool_match_in_place(&mut spanned_let.node.value);
            bool_match_in_place(&mut spanned_let.node.body);
        }
        MirExpr::Call(spanned_call) => {
            for arg in &mut spanned_call.node.args {
                bool_match_in_place(arg);
            }
        }
        MirExpr::TailCall(spanned_tc) => {
            for arg in &mut spanned_tc.node.args {
                bool_match_in_place(arg);
            }
        }
        MirExpr::Match(spanned_match) => {
            bool_match_in_place(&mut spanned_match.node.subject);
            for arm in &mut spanned_match.node.arms {
                bool_match_in_place(&mut arm.body);
            }
        }
        MirExpr::IfThenElse(spanned_ite) => {
            bool_match_in_place(&mut spanned_ite.node.cond);
            bool_match_in_place(&mut spanned_ite.node.then_branch);
            bool_match_in_place(&mut spanned_ite.node.else_branch);
        }
        MirExpr::Construct(spanned_ctor) => {
            for arg in &mut spanned_ctor.node.args {
                bool_match_in_place(arg);
            }
        }
        MirExpr::RecordCreate(spanned_rec) => {
            for f in &mut spanned_rec.node.fields {
                bool_match_in_place(&mut f.value);
            }
        }
        MirExpr::RecordUpdate(spanned_upd) => {
            bool_match_in_place(&mut spanned_upd.node.base);
            for f in &mut spanned_upd.node.updates {
                bool_match_in_place(&mut f.value);
            }
        }
        MirExpr::Project(spanned_proj) => bool_match_in_place(&mut spanned_proj.node.base),
        MirExpr::Try(inner) | MirExpr::Return(inner) => bool_match_in_place(inner),
        MirExpr::List(items) | MirExpr::Tuple(items) => {
            for item in items {
                bool_match_in_place(item);
            }
        }
        MirExpr::MapLiteral(entries) => {
            for (k, v) in entries {
                bool_match_in_place(k);
                bool_match_in_place(v);
            }
        }
        MirExpr::InterpolatedStr(parts) => {
            for part in parts {
                if let super::expr::MirStrPart::Expr(e) = part {
                    bool_match_in_place(e);
                }
            }
        }
        MirExpr::IndependentProduct(spanned_ip) => {
            for item in &mut spanned_ip.node.items {
                bool_match_in_place(item);
            }
        }
    }
}

/// Wave 10 — collapse `IfThenElse` whose `cond` is a literal
/// `Bool` directly to the surviving branch. Composes with
/// const-fold: a folded comparison feeds into the surrounding
/// `IfThenElse` and lets this pass drop the dead branch.
pub fn branch_collapse(mut program: MirProgram) -> MirProgram {
    for mir_fn in program.fns.values_mut() {
        branch_collapse_in_place(&mut mir_fn.body);
    }
    program
}

fn branch_collapse_in_place(expr: &mut Spanned<MirExpr>) {
    branch_collapse_walk_children(&mut expr.node);

    let collapse = if let MirExpr::IfThenElse(spanned_ite) = &expr.node {
        let ite = &spanned_ite.node;
        if let MirExpr::Literal(spanned_lit) = &ite.cond.node {
            match &spanned_lit.node {
                Literal::Bool(true) => Some(BranchSide::Then),
                Literal::Bool(false) => Some(BranchSide::Else),
                _ => None,
            }
        } else {
            None
        }
    } else {
        None
    };

    if let Some(side) = collapse {
        let placeholder = MirExpr::Literal(Spanned {
            node: Literal::Unit,
            line: expr.line,
            ty: std::sync::OnceLock::new(),
        });
        let original = std::mem::replace(&mut expr.node, placeholder);
        if let MirExpr::IfThenElse(spanned_ite) = original {
            let ite = spanned_ite.node;
            let surviving = match side {
                BranchSide::Then => *ite.then_branch,
                BranchSide::Else => *ite.else_branch,
            };
            *expr = surviving;
        } else {
            unreachable!("collapse only set inside the IfThenElse branch")
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum BranchSide {
    Then,
    Else,
}

fn branch_collapse_walk_children(node: &mut MirExpr) {
    match node {
        MirExpr::Literal(_) | MirExpr::Local(_) => {}
        MirExpr::Neg(inner) => branch_collapse_in_place(inner),
        MirExpr::BinOp(spanned_bop) => {
            branch_collapse_in_place(&mut spanned_bop.node.lhs);
            branch_collapse_in_place(&mut spanned_bop.node.rhs);
        }
        MirExpr::Let(spanned_let) => {
            branch_collapse_in_place(&mut spanned_let.node.value);
            branch_collapse_in_place(&mut spanned_let.node.body);
        }
        MirExpr::Call(spanned_call) => {
            for arg in &mut spanned_call.node.args {
                branch_collapse_in_place(arg);
            }
        }
        MirExpr::TailCall(spanned_tc) => {
            for arg in &mut spanned_tc.node.args {
                branch_collapse_in_place(arg);
            }
        }
        MirExpr::Match(spanned_match) => {
            branch_collapse_in_place(&mut spanned_match.node.subject);
            for arm in &mut spanned_match.node.arms {
                branch_collapse_in_place(&mut arm.body);
            }
        }
        MirExpr::IfThenElse(spanned_ite) => {
            branch_collapse_in_place(&mut spanned_ite.node.cond);
            branch_collapse_in_place(&mut spanned_ite.node.then_branch);
            branch_collapse_in_place(&mut spanned_ite.node.else_branch);
        }
        MirExpr::Construct(spanned_ctor) => {
            for arg in &mut spanned_ctor.node.args {
                branch_collapse_in_place(arg);
            }
        }
        MirExpr::RecordCreate(spanned_rec) => {
            for f in &mut spanned_rec.node.fields {
                branch_collapse_in_place(&mut f.value);
            }
        }
        MirExpr::RecordUpdate(spanned_upd) => {
            branch_collapse_in_place(&mut spanned_upd.node.base);
            for f in &mut spanned_upd.node.updates {
                branch_collapse_in_place(&mut f.value);
            }
        }
        MirExpr::Project(spanned_proj) => branch_collapse_in_place(&mut spanned_proj.node.base),
        MirExpr::Try(inner) | MirExpr::Return(inner) => branch_collapse_in_place(inner),
        MirExpr::List(items) | MirExpr::Tuple(items) => {
            for item in items {
                branch_collapse_in_place(item);
            }
        }
        MirExpr::MapLiteral(entries) => {
            for (k, v) in entries {
                branch_collapse_in_place(k);
                branch_collapse_in_place(v);
            }
        }
        MirExpr::InterpolatedStr(parts) => {
            for part in parts {
                if let super::expr::MirStrPart::Expr(e) = part {
                    branch_collapse_in_place(e);
                }
            }
        }
        MirExpr::IndependentProduct(spanned_ip) => {
            for item in &mut spanned_ip.node.items {
                branch_collapse_in_place(item);
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

    // ── Phase 6 wave 8: algebraic simplification ────────

    fn local_at(slot: u32) -> MirExpr {
        use super::super::expr::MirLocal;
        use super::super::program::LocalId;
        MirExpr::Local(span(MirLocal::at(LocalId(slot))))
    }

    #[test]
    fn algebraic_x_plus_zero_drops_to_x() {
        let body = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Add,
            lhs: Box::new(span(local_at(0))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(0))))),
        }));
        let simplified = algebraic_simplify(one_fn_program(body));
        assert!(
            matches!(body_of(&simplified), MirExpr::Local(_)),
            "x + 0 should collapse to x"
        );
    }

    #[test]
    fn algebraic_zero_plus_x_drops_to_x() {
        let body = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Add,
            lhs: Box::new(span(MirExpr::Literal(span(Literal::Int(0))))),
            rhs: Box::new(span(local_at(0))),
        }));
        let simplified = algebraic_simplify(one_fn_program(body));
        assert!(matches!(body_of(&simplified), MirExpr::Local(_)));
    }

    #[test]
    fn algebraic_x_times_one_drops_to_x() {
        let body = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Mul,
            lhs: Box::new(span(local_at(0))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(1))))),
        }));
        let simplified = algebraic_simplify(one_fn_program(body));
        assert!(matches!(body_of(&simplified), MirExpr::Local(_)));
    }

    #[test]
    fn algebraic_x_times_zero_collapses_when_pure() {
        // `x * 0` → `0` ONLY when `x` is pure. Local read is pure.
        let body = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Mul,
            lhs: Box::new(span(local_at(0))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(0))))),
        }));
        let simplified = algebraic_simplify(one_fn_program(body));
        assert!(
            matches!(body_of(&simplified), MirExpr::Literal(s) if matches!(s.node, Literal::Int(0))),
            "x * 0 with pure x should collapse to literal 0"
        );
    }

    #[test]
    fn algebraic_x_times_zero_keeps_when_impure() {
        // `some_call() * 0` — Call is impure, dropping it would
        // skip a possible effect.
        use super::super::expr::{MirCall, MirCallee};
        use crate::ir::FnId;
        let impure_call = MirExpr::Call(span(MirCall {
            callee: MirCallee::Fn(FnId(0)),
            args: vec![],
        }));
        let body = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Mul,
            lhs: Box::new(span(impure_call)),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(0))))),
        }));
        let simplified = algebraic_simplify(one_fn_program(body));
        assert!(
            matches!(body_of(&simplified), MirExpr::BinOp(_)),
            "impure x * 0 must stay a BinOp so the side effect runs"
        );
    }

    #[test]
    fn algebraic_x_div_one_drops_to_x() {
        let body = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Div,
            lhs: Box::new(span(local_at(0))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(1))))),
        }));
        let simplified = algebraic_simplify(one_fn_program(body));
        assert!(matches!(body_of(&simplified), MirExpr::Local(_)));
    }

    #[test]
    fn algebraic_double_neg_unwraps() {
        // Neg(Neg(x)) → x
        let body = MirExpr::Neg(Box::new(span(MirExpr::Neg(Box::new(span(local_at(0)))))));
        let simplified = algebraic_simplify(one_fn_program(body));
        assert!(matches!(body_of(&simplified), MirExpr::Local(_)));
    }

    #[test]
    fn algebraic_does_not_simplify_floats() {
        // `x + 0.0` should stay — signed-zero / NaN edge cases
        // mean float identities are unsafe without a per-shape
        // proof. Walker skips floats by structure (literal value
        // matcher reads `Int`-only).
        let body = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Add,
            lhs: Box::new(span(local_at(0))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Float(0.0))))),
        }));
        let simplified = algebraic_simplify(one_fn_program(body));
        assert!(
            matches!(body_of(&simplified), MirExpr::BinOp(_)),
            "float identity must NOT be simplified (signed-zero, NaN)"
        );
    }

    #[test]
    fn pipeline_compose_const_fold_algebraic_dce() {
        // `let x = 1 + 0; x + 0` →
        //  const-fold → `let x = 1; x + 0` (1+0 folds to 1, x+0 symbolic stays)
        //  algebraic  → `let x = 1; x`
        //  dce        → `1` (x used? yes, so dce keeps the let)
        // Final: `let x = 1; x` (binding read, so kept).
        use super::super::expr::MirLet;
        use super::super::program::LocalId;
        let value = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Add,
            lhs: Box::new(span(MirExpr::Literal(span(Literal::Int(1))))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(0))))),
        }));
        let body_expr = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Add,
            lhs: Box::new(span(local_at(0))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(0))))),
        }));
        let body = MirExpr::Let(span(MirLet {
            binding: LocalId(0),
            binding_name: "x".to_string(),
            value: Box::new(span(value)),
            body: Box::new(span(body_expr)),
        }));
        let optimized = dead_code(algebraic_simplify(const_fold(one_fn_program(body))));
        let MirExpr::Let(let_node) = body_of(&optimized) else {
            panic!("expected Let at root, got: {:?}", body_of(&optimized));
        };
        assert!(
            matches!(&let_node.node.value.node, MirExpr::Literal(s) if matches!(s.node, Literal::Int(1))),
            "let value should fold to 1"
        );
        assert!(
            matches!(&let_node.node.body.node, MirExpr::Local(_)),
            "let body's `x + 0` should simplify to `x` (Local)"
        );
    }

    // ── Phase 6 wave 9: bool match → IfThenElse ─────────

    fn bool_match_program(arms: Vec<MirMatchArm>) -> MirProgram {
        use super::super::expr::MirMatch;
        use super::super::program::LocalId;
        let subject = MirExpr::Local(span(super::super::expr::MirLocal::at(LocalId(0))));
        let m = MirExpr::Match(span(MirMatch {
            subject: Box::new(span(subject)),
            arms,
        }));
        one_fn_program(m)
    }

    #[test]
    fn bool_match_rewrites_true_first_then_false() {
        // match cond { true → 1; false → 2 } → if cond { 1 } else { 2 }
        let arms = vec![
            MirMatchArm {
                pattern: MirPattern::Literal(Literal::Bool(true)),
                body: span(MirExpr::Literal(span(Literal::Int(1)))),
            },
            MirMatchArm {
                pattern: MirPattern::Literal(Literal::Bool(false)),
                body: span(MirExpr::Literal(span(Literal::Int(2)))),
            },
        ];
        let rewritten = bool_match_to_if(bool_match_program(arms));
        let MirExpr::IfThenElse(ite) = body_of(&rewritten) else {
            panic!("expected IfThenElse, got: {:?}", body_of(&rewritten));
        };
        assert!(
            matches!(&ite.node.then_branch.node, MirExpr::Literal(s) if matches!(s.node, Literal::Int(1))),
            "then branch should be 1"
        );
        assert!(
            matches!(&ite.node.else_branch.node, MirExpr::Literal(s) if matches!(s.node, Literal::Int(2))),
            "else branch should be 2"
        );
    }

    #[test]
    fn bool_match_rewrites_false_first_then_true() {
        // match cond { false → 2; true → 1 } → if cond { 1 } else { 2 }
        let arms = vec![
            MirMatchArm {
                pattern: MirPattern::Literal(Literal::Bool(false)),
                body: span(MirExpr::Literal(span(Literal::Int(2)))),
            },
            MirMatchArm {
                pattern: MirPattern::Literal(Literal::Bool(true)),
                body: span(MirExpr::Literal(span(Literal::Int(1)))),
            },
        ];
        let rewritten = bool_match_to_if(bool_match_program(arms));
        let MirExpr::IfThenElse(ite) = body_of(&rewritten) else {
            panic!("expected IfThenElse")
        };
        assert!(
            matches!(&ite.node.then_branch.node, MirExpr::Literal(s) if matches!(s.node, Literal::Int(1))),
            "then branch should still be 1 even though true-arm was second in source"
        );
    }

    #[test]
    fn bool_match_rewrites_true_with_wildcard_default() {
        // match cond { true → 1; _ → 2 } → if cond { 1 } else { 2 }
        let arms = vec![
            MirMatchArm {
                pattern: MirPattern::Literal(Literal::Bool(true)),
                body: span(MirExpr::Literal(span(Literal::Int(1)))),
            },
            MirMatchArm {
                pattern: MirPattern::Wildcard,
                body: span(MirExpr::Literal(span(Literal::Int(2)))),
            },
        ];
        let rewritten = bool_match_to_if(bool_match_program(arms));
        assert!(matches!(body_of(&rewritten), MirExpr::IfThenElse(_)));
    }

    #[test]
    fn bool_match_leaves_three_arm_match_intact() {
        // match cond { true → 1; false → 2; _ → 3 } stays Match —
        // wave 9 only handles two-arm shape, three+ wait for
        // future passes.
        let arms = vec![
            MirMatchArm {
                pattern: MirPattern::Literal(Literal::Bool(true)),
                body: span(MirExpr::Literal(span(Literal::Int(1)))),
            },
            MirMatchArm {
                pattern: MirPattern::Literal(Literal::Bool(false)),
                body: span(MirExpr::Literal(span(Literal::Int(2)))),
            },
            MirMatchArm {
                pattern: MirPattern::Wildcard,
                body: span(MirExpr::Literal(span(Literal::Int(3)))),
            },
        ];
        let rewritten = bool_match_to_if(bool_match_program(arms));
        assert!(
            matches!(body_of(&rewritten), MirExpr::Match(_)),
            "three-arm match should stay a Match node"
        );
    }

    #[test]
    fn bool_match_leaves_non_bool_match_intact() {
        // match xs { [] → 0; _ → 1 } — EmptyList isn't a Bool
        // literal, so the pass doesn't fire.
        let arms = vec![
            MirMatchArm {
                pattern: MirPattern::EmptyList,
                body: span(MirExpr::Literal(span(Literal::Int(0)))),
            },
            MirMatchArm {
                pattern: MirPattern::Wildcard,
                body: span(MirExpr::Literal(span(Literal::Int(1)))),
            },
        ];
        let rewritten = bool_match_to_if(bool_match_program(arms));
        assert!(
            matches!(body_of(&rewritten), MirExpr::Match(_)),
            "non-Bool literal pattern should not trigger the rewrite"
        );
    }

    // ── Phase 6 wave 10: branch collapse ──────────────────

    fn ite_program(cond: MirExpr, then_b: MirExpr, else_b: MirExpr) -> MirProgram {
        let ite = super::super::expr::MirIfThenElse {
            cond: Box::new(span(cond)),
            then_branch: Box::new(span(then_b)),
            else_branch: Box::new(span(else_b)),
        };
        one_fn_program(MirExpr::IfThenElse(span(ite)))
    }

    #[test]
    fn branch_collapse_keeps_then_when_cond_is_true() {
        let collapsed = branch_collapse(ite_program(
            MirExpr::Literal(span(Literal::Bool(true))),
            MirExpr::Literal(span(Literal::Int(1))),
            MirExpr::Literal(span(Literal::Int(2))),
        ));
        assert!(
            matches!(body_of(&collapsed), MirExpr::Literal(s) if matches!(s.node, Literal::Int(1))),
            "true cond should collapse to then branch"
        );
    }

    #[test]
    fn branch_collapse_keeps_else_when_cond_is_false() {
        let collapsed = branch_collapse(ite_program(
            MirExpr::Literal(span(Literal::Bool(false))),
            MirExpr::Literal(span(Literal::Int(1))),
            MirExpr::Literal(span(Literal::Int(2))),
        ));
        assert!(
            matches!(body_of(&collapsed), MirExpr::Literal(s) if matches!(s.node, Literal::Int(2))),
            "false cond should collapse to else branch"
        );
    }

    #[test]
    fn branch_collapse_leaves_symbolic_cond_intact() {
        use super::super::expr::MirLocal;
        use super::super::program::LocalId;
        let collapsed = branch_collapse(ite_program(
            MirExpr::Local(span(MirLocal::at(LocalId(0)))),
            MirExpr::Literal(span(Literal::Int(1))),
            MirExpr::Literal(span(Literal::Int(2))),
        ));
        assert!(
            matches!(body_of(&collapsed), MirExpr::IfThenElse(_)),
            "symbolic cond must stay an IfThenElse"
        );
    }

    #[test]
    fn pipeline_const_fold_then_branch_collapse() {
        // `if (5 == 5) { 1 } else { 2 }`
        //  const-fold → `if true { 1 } else { 2 }`
        //  branch-collapse → `1`
        let cond = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Eq,
            lhs: Box::new(span(MirExpr::Literal(span(Literal::Int(5))))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(5))))),
        }));
        let p = ite_program(
            cond,
            MirExpr::Literal(span(Literal::Int(1))),
            MirExpr::Literal(span(Literal::Int(2))),
        );
        let optimized = branch_collapse(const_fold(p));
        assert!(
            matches!(body_of(&optimized), MirExpr::Literal(s) if matches!(s.node, Literal::Int(1))),
            "fold→collapse should reduce the whole IfThenElse to literal 1"
        );
    }

    #[test]
    fn pipeline_bool_match_to_if_then_branch_collapse() {
        // match true { true → 1; false → 2 }
        //  bool_match_to_if → if true { 1 } else { 2 }
        //  branch_collapse  → 1
        use super::super::expr::MirMatch;
        let arms = vec![
            MirMatchArm {
                pattern: MirPattern::Literal(Literal::Bool(true)),
                body: span(MirExpr::Literal(span(Literal::Int(1)))),
            },
            MirMatchArm {
                pattern: MirPattern::Literal(Literal::Bool(false)),
                body: span(MirExpr::Literal(span(Literal::Int(2)))),
            },
        ];
        let m = MirExpr::Match(span(MirMatch {
            subject: Box::new(span(MirExpr::Literal(span(Literal::Bool(true))))),
            arms,
        }));
        let optimized = branch_collapse(bool_match_to_if(one_fn_program(m)));
        assert!(
            matches!(body_of(&optimized), MirExpr::Literal(s) if matches!(s.node, Literal::Int(1))),
            "match true (true=>1, false=>2) should collapse to 1 through the full chain"
        );
    }
}

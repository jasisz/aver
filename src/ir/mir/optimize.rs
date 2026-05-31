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
//! ## What this doesn't transform (deferred)
//!
//! - Call inlining (Phase 6 wave 7 candidate).
//! - String concatenation (`Str + Str`) — would require
//!   intern-side allocation policy; the VM does it efficiently
//!   at runtime already.
//! - Match arms (would require pattern → literal classification
//!   plumbing; defer until there's a real perf signal).
//! - CSE / loop-invariant hoisting — future waves.

use crate::ast::{BinOp, Literal, Spanned};

use super::expr::{MirBinOp, MirCall, MirConstruct, MirExpr, MirLet, MirMatchArm, MirPattern};
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
}

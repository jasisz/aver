//! Phase 6 wave 5 — MIR optimization passes.
//!
//! Smallest non-trivial transformation on the MIR substrate:
//! evaluate `BinOp` / `Neg` over literal operands at compile
//! time. Validates that the optimize pipeline composes cleanly
//! with `lower_program` → backend compile.
//!
//! ## Scope
//!
//! - Numeric: `Int + Int`, `Float * Float`, … — uses
//!   `checked_*` arithmetic for `Int` so overflow leaves the
//!   node intact (runtime semantics: VM produces an arithmetic
//!   error). Float follows IEEE-754; we don't try to constant-
//!   fold operations that depend on rounding mode.
//! - Boolean: `Eq / Neq / Lt / Gt / Lte / Gte` over matching
//!   `Int` / `Float` / `Bool` / `Str` literal pairs.
//! - Unary: `Neg(Literal::Int)` / `Neg(Literal::Float)`. The
//!   `MirExpr::Neg` variant exists specifically so IEEE-754
//!   `-0.0` semantics on `Float` survive — we fold only when
//!   the result preserves that contract.
//!
//! ## What this doesn't fold
//!
//! - Calls (no inlining yet — Phase 6 wave 6 follow-up).
//! - String concatenation (`Str + Str`) — would require
//!   intern-side allocation policy; the VM does it efficiently
//!   at runtime already.
//! - Match arms (would require pattern → literal classification
//!   plumbing; defer until there's a real perf signal).

use crate::ast::{BinOp, Literal, Spanned};

use super::expr::{MirBinOp, MirCall, MirConstruct, MirExpr, MirLet, MirMatchArm, MirPattern};
use super::program::MirProgram;

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
}

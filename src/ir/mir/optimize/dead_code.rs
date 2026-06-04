//! Phase 6 wave 6 — dead-code elimination on MIR.
//!
//! Drop `Let { binding, value: <pure>, body }` when `binding` is
//! never read in `body`. Pure = no observable side effect:
//! `Literal` / `Local` / `BinOp` / `Neg` / `Tuple` / `List` /
//! `MapLiteral` (with pure entries) / `Project` / `Construct` /
//! `RecordCreate` / `RecordUpdate` (with pure subtrees). `Call` /
//! `TailCall` / `Try` / `Return` / `Match` / `InterpolatedStr` /
//! `IndependentProduct` are conservatively impure.
//!
//! Const-fold runs before DCE so any folded sub-arithmetic
//! collapses to a `Literal` (pure) and unlocks its enclosing
//! `Let` for elimination.

use crate::ast::{Literal, Spanned};

use super::super::expr::{MirBinOp, MirExpr, MirLet};
use super::super::program::{LocalId, MirProgram};

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

    let should_elide = if let MirExpr::Let(spanned_let) = &expr.node {
        let let_node = &spanned_let.node;
        !local_is_read(let_node.binding, &let_node.body) && is_pure(&let_node.value)
    } else {
        false
    };

    if should_elide {
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
        MirExpr::Literal(_) | MirExpr::Local(_) | MirExpr::FnValue(_) => {}
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
                if let super::super::expr::MirStrPart::Expr(e) = part {
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
/// because MIR's slot numbering is already SSA-ish.
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
        MirExpr::Literal(_) | MirExpr::FnValue(_) => {}
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
                if let super::super::expr::MirStrPart::Expr(e) = part {
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

/// Whether a division divisor is provably non-zero, so the division
/// cannot trap and the enclosing `BinOp::Div` may count as pure. Only a
/// literal we can inspect qualifies: a non-zero integer, or any float
/// (Float `/` is total). A variable or computed divisor could be zero,
/// so it is conservatively treated as possibly-trapping.
fn divisor_proven_nonzero(rhs: &Spanned<MirExpr>) -> bool {
    match &rhs.node {
        MirExpr::Literal(spanned) => match spanned.node {
            Literal::Int(n) => n != 0,
            Literal::Float(_) => true,
            _ => false,
        },
        _ => false,
    }
}

/// Conservative purity classification — `true` means the
/// expression has no observable side effect AND cannot diverge
/// or raise. Exported `pub(super)` so the algebraic pass can
/// reuse it for `x * 0` (only collapse when the surviving
/// operand is pure).
pub(super) fn is_pure(expr: &Spanned<MirExpr>) -> bool {
    match &expr.node {
        MirExpr::Literal(_) | MirExpr::Local(_) | MirExpr::FnValue(_) => true,
        MirExpr::Neg(inner) => is_pure(inner),
        MirExpr::BinOp(spanned_bop) => {
            let bop = &spanned_bop.node;
            // Integer `/` by a possibly-zero divisor traps at runtime
            // ("division by zero"). Classifying it pure would let DCE drop
            // a dead `5 / 0`, or the `x * 0` collapse fold `(5 / 0) * 0`
            // to `0` — silently turning a trapping program into a
            // non-trapping one. Only a divisor we can prove non-zero is
            // safe to elide. (Float `/` is total — `x / 0.0` is Infinity,
            // never traps — and `%`/modulo is a Result-returning builtin,
            // not a `BinOp`, so neither applies here.)
            if matches!(bop.op, crate::ast::BinOp::Div) && !divisor_proven_nonzero(&bop.rhs) {
                return false;
            }
            is_pure(&bop.lhs) && is_pure(&bop.rhs)
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
        MirExpr::IfThenElse(spanned_ite) => {
            is_pure(&spanned_ite.node.cond)
                && is_pure(&spanned_ite.node.then_branch)
                && is_pure(&spanned_ite.node.else_branch)
        }
        MirExpr::Call(_)
        | MirExpr::TailCall(_)
        | MirExpr::Try(_)
        | MirExpr::Return(_)
        | MirExpr::Match(_)
        | MirExpr::InterpolatedStr(_)
        | MirExpr::IndependentProduct(_) => false,
    }
}

#[cfg(test)]
mod tests {
    use super::super::super::expr::{MirBinOp, MirCall, MirCallee, MirLet};
    use super::super::super::program::LocalId;
    use super::super::const_fold::const_fold;
    use super::super::test_helpers::{body_of, one_fn_program, span};
    use super::*;
    use crate::ast::BinOp;
    use crate::ir::FnId;

    #[test]
    fn dce_drops_unused_pure_let() {
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
        use super::super::super::expr::MirLocal;
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
        let call_value = MirExpr::Call(span(MirCall {
            callee: MirCallee::Fn(FnId(0)),
            args: vec![],
        }));
        let body = MirExpr::Let(span(MirLet {
            binding: LocalId(0),
            binding_name: String::new(),
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
    fn div_purity_classification_respects_trapping_divisor() {
        use super::super::super::expr::MirLocal;
        let div = |l: MirExpr, r: MirExpr| {
            span(MirExpr::BinOp(span(MirBinOp {
                op: BinOp::Div,
                lhs: Box::new(span(l)),
                rhs: Box::new(span(r)),
            })))
        };
        let int = |n| MirExpr::Literal(span(Literal::Int(n)));
        let flt = |f| MirExpr::Literal(span(Literal::Float(f)));

        // Integer `/` by a zero literal traps at runtime → must be impure.
        assert!(!is_pure(&div(int(5), int(0))), "5 / 0 traps → impure");
        // Integer `/` by a proven non-zero literal cannot trap → pure.
        assert!(is_pure(&div(int(10), int(2))), "10 / 2 cannot trap → pure");
        // Float `/` is total (`x / 0.0` is Infinity) → pure.
        assert!(
            is_pure(&div(flt(1.0), flt(0.0))),
            "float div is total → pure"
        );
        // A variable divisor could be zero at runtime → conservatively impure.
        assert!(
            !is_pure(&div(int(5), MirExpr::Local(span(MirLocal::at(LocalId(0)))))),
            "variable divisor could be zero → impure"
        );
    }

    #[test]
    fn dce_keeps_unused_integer_div_by_zero() {
        // Regression: a dead `5 / 0` binding must NOT be eliminated — the
        // division traps, and dropping it would silently turn a trapping
        // program into a non-trapping one.
        let value = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Div,
            lhs: Box::new(span(MirExpr::Literal(span(Literal::Int(5))))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(0))))),
        }));
        let body = MirExpr::Let(span(MirLet {
            binding: LocalId(0),
            binding_name: String::new(),
            value: Box::new(span(value)),
            body: Box::new(span(MirExpr::Literal(span(Literal::Int(42))))),
        }));
        let eliminated = dead_code(one_fn_program(body));
        assert!(
            matches!(body_of(&eliminated), MirExpr::Let(_)),
            "unused `5 / 0` Let must stay — eliding it would drop a runtime trap"
        );
    }

    #[test]
    fn dce_drops_nested_unused_pure_let_chains() {
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

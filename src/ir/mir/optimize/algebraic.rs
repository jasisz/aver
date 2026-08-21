//! Phase 6 wave 8 — algebraic identity simplification.
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
//! Float is deliberately *not* simplified (signed-zero / NaN /
//! ±∞ edge cases). No string / list / map rewrites.

use crate::ast::{BinOp, Literal, Spanned};

use super::super::expr::{MirExpr, walk_children_mut};
use super::super::program::MirProgram;
use super::dead_code::is_pure;

pub fn algebraic_simplify(mut program: MirProgram) -> MirProgram {
    for mir_fn in program.fns.values_mut() {
        algebraic_in_place(&mut mir_fn.body);
    }
    program
}

fn algebraic_in_place(expr: &mut Spanned<MirExpr>) {
    algebraic_walk_children(&mut expr.node);

    if let Some(replacement) = try_algebraic(&expr.node) {
        match replacement {
            AlgReplace::Identity(side) => {
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
    Identity(Side),
    Literal(Literal),
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
            if rhs_int == Some(0) {
                return Some(AlgReplace::Identity(Side::Lhs));
            }
            if lhs_int == Some(0) {
                return Some(AlgReplace::Identity(Side::Rhs));
            }
            None
        }
        BinOp::Sub => {
            if rhs_int == Some(0) {
                return Some(AlgReplace::Identity(Side::Lhs));
            }
            None
        }
        BinOp::Mul => {
            if rhs_int == Some(1) {
                return Some(AlgReplace::Identity(Side::Lhs));
            }
            if lhs_int == Some(1) {
                return Some(AlgReplace::Identity(Side::Rhs));
            }
            if rhs_int == Some(0) && is_pure(lhs) {
                return Some(AlgReplace::Literal(Literal::Int(0)));
            }
            if lhs_int == Some(0) && is_pure(rhs) {
                return Some(AlgReplace::Literal(Literal::Int(0)));
            }
            None
        }
        BinOp::Div => {
            if rhs_int == Some(1) {
                return Some(AlgReplace::Identity(Side::Lhs));
            }
            None
        }
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
    walk_children_mut(node, &mut |child| algebraic_in_place(child));
}

#[cfg(test)]
mod tests {
    use super::super::super::expr::{MirBinOp, MirCall, MirCallee, MirLet, MirLocal};
    use super::super::super::program::LocalId;
    use super::super::const_fold::const_fold;
    use super::super::dead_code::dead_code;
    use super::super::test_helpers::{body_of, one_fn_program, span};
    use super::*;
    use crate::ast::BinOp;
    use crate::ir::FnId;

    fn local_at(slot: u32) -> MirExpr {
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
    fn algebraic_mul_zero_keeps_trapping_div_operand() {
        // Regression: `(5 / 0) * 0` must NOT collapse to `0`. The
        // `x * 0 → 0` rewrite only fires when the dropped operand is pure,
        // and `5 / 0` traps at runtime — folding it away would silently
        // turn a trapping program into a non-trapping one.
        let div = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Div,
            lhs: Box::new(span(MirExpr::Literal(span(Literal::Int(5))))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(0))))),
        }));
        let body = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Mul,
            lhs: Box::new(span(div)),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(0))))),
        }));
        let simplified = algebraic_simplify(one_fn_program(body));
        assert!(
            matches!(body_of(&simplified), MirExpr::BinOp(b) if matches!(b.node.op, BinOp::Mul)),
            "(5 / 0) * 0 must stay a Mul — collapsing to 0 would drop a trap"
        );
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
        let body = MirExpr::Neg(Box::new(span(MirExpr::Neg(Box::new(span(local_at(0)))))));
        let simplified = algebraic_simplify(one_fn_program(body));
        assert!(matches!(body_of(&simplified), MirExpr::Local(_)));
    }

    #[test]
    fn algebraic_does_not_simplify_floats() {
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
            panic!("expected Let at root");
        };
        assert!(matches!(
            &let_node.node.value.node,
            MirExpr::Literal(s) if matches!(s.node, Literal::Int(1))
        ));
        assert!(matches!(&let_node.node.body.node, MirExpr::Local(_)));
    }
}

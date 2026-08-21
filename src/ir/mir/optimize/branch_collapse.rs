//! Phase 6 wave 10 — branch collapse.
//!
//! Collapse `IfThenElse` whose `cond` is a literal `Bool` to
//! the surviving branch. Composes with const-fold: a folded
//! comparison feeds into the surrounding `IfThenElse` and lets
//! this pass drop the dead branch.
//!
//! The pass deliberately does NOT consult `is_pure` on the
//! dropped branch — when the literal `cond` is `true`, the
//! `else_branch` never evaluates at runtime, so dropping it
//! can't change observable behavior even when it contains an
//! effectful `Call`. Mirror of the language-level guarantee:
//! `if true { A } else { B }` is `A`, regardless of what `B`
//! would do.

use crate::ast::{Literal, Spanned};

use super::super::expr::{MirExpr, walk_children_mut};
use super::super::program::MirProgram;

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
    walk_children_mut(node, &mut |child| branch_collapse_in_place(child));
}

#[cfg(test)]
mod tests {
    use super::super::super::expr::{
        MirBinOp, MirCall, MirCallee, MirIfThenElse, MirLocal, MirMatch, MirMatchArm, MirPattern,
    };
    use super::super::super::program::LocalId;
    use super::super::bool_match::bool_match_to_if;
    use super::super::const_fold::const_fold;
    use super::super::test_helpers::{body_of, one_fn_program, span};
    use super::*;
    use crate::ast::BinOp;
    use crate::ir::FnId;

    fn ite_program(cond: MirExpr, then_b: MirExpr, else_b: MirExpr) -> MirProgram {
        let ite = MirIfThenElse {
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
            matches!(body_of(&collapsed), MirExpr::Literal(s) if matches!(s.node, Literal::Int(1)))
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
            matches!(body_of(&collapsed), MirExpr::Literal(s) if matches!(s.node, Literal::Int(2)))
        );
    }

    #[test]
    fn branch_collapse_leaves_symbolic_cond_intact() {
        let collapsed = branch_collapse(ite_program(
            MirExpr::Local(span(MirLocal::at(LocalId(0)))),
            MirExpr::Literal(span(Literal::Int(1))),
            MirExpr::Literal(span(Literal::Int(2))),
        ));
        assert!(matches!(body_of(&collapsed), MirExpr::IfThenElse(_)));
    }

    #[test]
    fn branch_collapse_drops_dead_branch_with_side_effect() {
        // `if true { 1 } else { Call(FnId(99)) }` → `1` —
        // pins the deliberate semantic that branch selection
        // gates the side effect, not purity.
        let dead_call = MirExpr::Call(span(MirCall {
            callee: MirCallee::Fn(FnId(99)),
            args: vec![],
        }));
        let collapsed = branch_collapse(ite_program(
            MirExpr::Literal(span(Literal::Bool(true))),
            MirExpr::Literal(span(Literal::Int(1))),
            dead_call,
        ));
        assert!(
            matches!(body_of(&collapsed), MirExpr::Literal(s) if matches!(s.node, Literal::Int(1))),
            "true cond should drop the dead effectful branch and yield `1`"
        );
    }

    #[test]
    fn pipeline_const_fold_then_branch_collapse() {
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
            matches!(body_of(&optimized), MirExpr::Literal(s) if matches!(s.node, Literal::Int(1)))
        );
    }

    #[test]
    fn pipeline_bool_match_to_if_then_branch_collapse() {
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
            "full chain should collapse to 1"
        );
    }
}

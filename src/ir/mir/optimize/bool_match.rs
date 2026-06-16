//! Phase 6 wave 9 — bool match → IfThenElse.
//!
//! Rewrite qualifying two-arm `Bool` match expressions into
//! `IfThenElse`. Recognition shape (mirror of HIR's
//! `try_emit_bool_if_else`):
//!
//! - Match has exactly 2 arms
//! - One arm's pattern is `Literal(Bool(true))`, the other is
//!   `Literal(Bool(false))` or `Wildcard` (catch-all default)
//! - Bindings are empty (no captured locals)
//!
//! Backends consume only the rewritten `IfThenElse` form — no
//! per-backend recognition logic.

use crate::ast::{Literal, Spanned};

use super::super::expr::{MirExpr, MirIfThenElse, MirMatchArm, MirPattern};
use super::super::program::MirProgram;

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
            let arms_vec: Vec<MirMatchArm> = m.arms.into_iter().collect();
            let then_branch = Box::new(arms_vec[branch_indices.true_idx].body.clone());
            let else_branch = Box::new(arms_vec[branch_indices.false_idx].body.clone());
            let ite = MirIfThenElse {
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

fn try_bool_match_branches(arms: &[MirMatchArm]) -> Option<BoolBranchIndices> {
    if arms.len() != 2 {
        return None;
    }
    let p0 = &arms[0].pattern;
    let p1 = &arms[1].pattern;
    let p0_bool = bool_pattern(p0);
    let p1_bool = bool_pattern(p1);
    match (p0_bool, p1_bool) {
        (Some(BoolPat::True), Some(BoolPat::False))
        | (Some(BoolPat::True), Some(BoolPat::Wildcard)) => Some(BoolBranchIndices {
            true_idx: 0,
            false_idx: 1,
        }),
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
        MirExpr::Literal(_) | MirExpr::Local(_) | MirExpr::FnValue(_) => {}
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
        MirExpr::Try(inner)
        | MirExpr::Return(inner)
        | MirExpr::Box(inner)
        | MirExpr::Unbox(inner) => bool_match_in_place(inner),
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
                if let super::super::expr::MirStrPart::Expr(e) = part {
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

#[cfg(test)]
mod tests {
    use super::super::super::expr::{MirLocal, MirMatch};
    use super::super::super::program::LocalId;
    use super::super::test_helpers::{body_of, one_fn_program, span};
    use super::*;

    fn bool_match_program(arms: Vec<MirMatchArm>) -> MirProgram {
        let subject = MirExpr::Local(span(MirLocal::at(LocalId(0))));
        let m = MirExpr::Match(span(MirMatch {
            subject: Box::new(span(subject)),
            arms,
        }));
        one_fn_program(m)
    }

    #[test]
    fn bool_match_rewrites_true_first_then_false() {
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
        assert!(matches!(
            &ite.node.then_branch.node,
            MirExpr::Literal(s) if matches!(s.node, Literal::Int(1))
        ));
        assert!(matches!(
            &ite.node.else_branch.node,
            MirExpr::Literal(s) if matches!(s.node, Literal::Int(2))
        ));
    }

    #[test]
    fn bool_match_rewrites_false_first_then_true() {
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
        assert!(matches!(
            &ite.node.then_branch.node,
            MirExpr::Literal(s) if matches!(s.node, Literal::Int(1))
        ));
    }

    #[test]
    fn bool_match_rewrites_true_with_wildcard_default() {
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
        assert!(matches!(body_of(&rewritten), MirExpr::Match(_)));
    }

    #[test]
    fn bool_match_leaves_non_bool_match_intact() {
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
        assert!(matches!(body_of(&rewritten), MirExpr::Match(_)));
    }
}

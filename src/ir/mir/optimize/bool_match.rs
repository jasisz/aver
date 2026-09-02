//! Phase 6 wave 9 — bool match → IfThenElse.
//!
//! Rewrite qualifying two-arm `Bool` match expressions into
//! `IfThenElse`. Recognition shape (mirror of HIR's
//! `try_emit_bool_if_else`):
//!
//! - Match has exactly 2 arms
//! - One arm's pattern is `Literal(Bool(true))`, the other is
//!   `Literal(Bool(false))`, `Wildcard`, or a NAMED binder
//!   (`other -> …`) — all three are the catch-all default
//! - The named-binder case keeps its binding: that arm is reached
//!   only when the subject was `false`, so the binder is that
//!   constant as a `Literal(Bool(false))`. The subject is never
//!   re-evaluated (which would duplicate any effect it performs),
//!   and DCE drops the `Let` when the body never reads the name —
//!   the usual case, since the name exists only to spell "otherwise"
//!
//! Backends consume only the rewritten `IfThenElse` form — no
//! per-backend recognition logic.

use crate::ast::{Literal, Spanned};

use super::super::expr::{
    MirExpr, MirIfThenElse, MirLet, MirMatchArm, MirPattern, walk_children_mut,
};
use super::super::program::{LocalId, MirProgram};

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
            let then_branch = Box::new(bind_branch_body(
                arms_vec[branch_indices.true_idx].body.clone(),
                branch_indices.binder_arm(branch_indices.true_idx),
                true,
            ));
            let else_branch = Box::new(bind_branch_body(
                arms_vec[branch_indices.false_idx].body.clone(),
                branch_indices.binder_arm(branch_indices.false_idx),
                false,
            ));
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
    /// The catch-all arm's binder, when it names the matched value
    /// (`other -> …`), paired with that arm's index.
    binder: Option<(usize, LocalId, String)>,
}

impl BoolBranchIndices {
    /// The binder to introduce at the top of branch `idx`, if that
    /// branch is the one whose pattern named the matched value.
    fn binder_arm(&self, idx: usize) -> Option<(LocalId, &str)> {
        match &self.binder {
            Some((binder_idx, slot, name)) if *binder_idx == idx => Some((*slot, name.as_str())),
            _ => None,
        }
    }
}

/// Wrap one rewritten branch in the `Let` that reinstates the arm's
/// binder.
///
/// A named catch-all (`other -> …`) is reached only when the subject
/// held the truth value the other arm did not claim, so the binder is
/// that constant — not a second read of the subject, which would
/// duplicate any effect the subject performs. DCE removes the `Let`
/// again whenever the body never reads the name.
fn bind_branch_body(
    body: Spanned<MirExpr>,
    binder: Option<(LocalId, &str)>,
    branch_value: bool,
) -> Spanned<MirExpr> {
    let Some((slot, name)) = binder else {
        return body;
    };
    let line = body.line;
    let value = Spanned {
        node: MirExpr::Literal(Spanned {
            node: Literal::Bool(branch_value),
            line,
            ty: std::sync::OnceLock::new(),
        }),
        line,
        ty: std::sync::OnceLock::new(),
    };
    Spanned {
        node: MirExpr::Let(Spanned {
            node: MirLet {
                binding: slot,
                binding_name: name.to_string(),
                value: Box::new(value),
                body: Box::new(body),
            },
            line,
            ty: std::sync::OnceLock::new(),
        }),
        line,
        ty: std::sync::OnceLock::new(),
    }
}

fn try_bool_match_branches(arms: &[MirMatchArm]) -> Option<BoolBranchIndices> {
    if arms.len() != 2 {
        return None;
    }
    let p0 = bool_pattern(&arms[0].pattern);
    let p1 = bool_pattern(&arms[1].pattern);
    match (&p0, &p1) {
        // `true` first, then the default arm. A named binder sits
        // exactly where `false` / `_` already sit — the arm the
        // subject reaches only by being `false`.
        (Some(BoolPat::True), Some(BoolPat::False | BoolPat::Wildcard)) => {
            Some(BoolBranchIndices {
                true_idx: 0,
                false_idx: 1,
                binder: None,
            })
        }
        (Some(BoolPat::True), Some(BoolPat::Bind(slot, name))) => Some(BoolBranchIndices {
            true_idx: 0,
            false_idx: 1,
            binder: Some((1, *slot, name.clone())),
        }),
        (Some(BoolPat::False), Some(BoolPat::True))
        | (Some(BoolPat::Wildcard), Some(BoolPat::True)) => Some(BoolBranchIndices {
            true_idx: 1,
            false_idx: 0,
            binder: None,
        }),
        _ => None,
    }
}

enum BoolPat {
    True,
    False,
    Wildcard,
    /// `other -> …` — a catch-all that also names the matched value.
    Bind(LocalId, String),
}

fn bool_pattern(p: &MirPattern) -> Option<BoolPat> {
    match p {
        MirPattern::Literal(Literal::Bool(true)) => Some(BoolPat::True),
        MirPattern::Literal(Literal::Bool(false)) => Some(BoolPat::False),
        MirPattern::Wildcard => Some(BoolPat::Wildcard),
        MirPattern::Bind(slot, name) => Some(BoolPat::Bind(*slot, name.clone())),
        _ => None,
    }
}

fn bool_match_walk_children(node: &mut MirExpr) {
    walk_children_mut(node, &mut |child| bool_match_in_place(child));
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

    /// `match b { true -> …, other -> … }` — the default arm names the
    /// matched value. It becomes an `IfThenElse` like `_` does, but the
    /// else branch first reinstates the name.
    ///
    /// The value it reinstates is the CONSTANT `false`, not a second
    /// read of the subject: the subject may be a call, and re-emitting
    /// it under the else branch would run its effects twice. The arm is
    /// reachable only when the subject was `false`, so the constant is
    /// the whole truth about it.
    #[test]
    fn bool_match_rewrites_true_with_named_default_and_keeps_the_binding() {
        let arms = vec![
            MirMatchArm {
                pattern: MirPattern::Literal(Literal::Bool(true)),
                body: span(MirExpr::Literal(span(Literal::Int(1)))),
            },
            MirMatchArm {
                pattern: MirPattern::Bind(LocalId(4), "other".to_string()),
                body: span(MirExpr::Local(span(MirLocal::at(LocalId(4))))),
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
        let MirExpr::Let(binding) = &ite.node.else_branch.node else {
            panic!(
                "expected the else branch to reinstate the binder, got: {:?}",
                ite.node.else_branch.node
            );
        };
        assert_eq!(binding.node.binding, LocalId(4));
        assert_eq!(binding.node.binding_name, "other");
        assert!(
            matches!(
                &binding.node.value.node,
                MirExpr::Literal(s) if matches!(s.node, Literal::Bool(false))
            ),
            "the named default binds the constant `false`, got: {:?}",
            binding.node.value.node
        );
        assert!(matches!(&binding.node.body.node, MirExpr::Local(_)));
    }

    /// The true arm never gets a binding: only the default arm can
    /// carry a binder, and its constant belongs on its own branch.
    #[test]
    fn bool_match_named_default_leaves_the_true_branch_unwrapped() {
        let arms = vec![
            MirMatchArm {
                pattern: MirPattern::Literal(Literal::Bool(true)),
                body: span(MirExpr::Literal(span(Literal::Int(1)))),
            },
            MirMatchArm {
                pattern: MirPattern::Bind(LocalId(4), "other".to_string()),
                body: span(MirExpr::Literal(span(Literal::Int(2)))),
            },
        ];
        let rewritten = bool_match_to_if(bool_match_program(arms));
        let MirExpr::IfThenElse(ite) = body_of(&rewritten) else {
            panic!("expected IfThenElse")
        };
        assert!(
            matches!(&ite.node.then_branch.node, MirExpr::Literal(_)),
            "then branch should be untouched, got: {:?}",
            ite.node.then_branch.node
        );
    }

    /// A binder in the FIRST arm is not a default: it is irrefutable, so
    /// it always wins and the second arm is unreachable. Rewriting it as
    /// a condition would invert the program.
    #[test]
    fn bool_match_leaves_leading_binder_intact() {
        let arms = vec![
            MirMatchArm {
                pattern: MirPattern::Bind(LocalId(4), "other".to_string()),
                body: span(MirExpr::Literal(span(Literal::Int(2)))),
            },
            MirMatchArm {
                pattern: MirPattern::Literal(Literal::Bool(true)),
                body: span(MirExpr::Literal(span(Literal::Int(1)))),
            },
        ];
        let rewritten = bool_match_to_if(bool_match_program(arms));
        assert!(matches!(body_of(&rewritten), MirExpr::Match(_)));
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

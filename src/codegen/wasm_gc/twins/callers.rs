//! Which target calling conventions the program's call sites use.

use std::collections::{HashMap, HashSet};

use crate::ir::FnId;
use crate::ir::mir::{MirCallee, MirExpr};

use super::{RecordFields, TwinKind, TwinPlan, has_word};

/// How an expression's value is used: as a machine word (arithmetic, a
/// comparison, a builtin's arg), as a match subject, as its fn's result, or
/// otherwise.
#[derive(Clone, Copy, PartialEq)]
pub(super) enum Use {
    Word,
    Subject,
    Tail,
    Other,
    Fields,
}

/// A fn's body read for the twins its calls would use (`plan`).
pub(super) struct Caller<'a> {
    pub(super) at: usize,
    pub(super) plans: &'a [Option<TwinPlan>],
    pub(super) kept: &'a HashSet<usize>,
    pub(super) pos: &'a HashMap<FnId, usize>,
    pub(super) params: &'a [crate::ir::mir::MirParam],
}

impl Caller<'_> {
    fn plan(&self, id: FnId) -> Option<(usize, &TwinPlan)> {
        let j = *self.pos.get(&id)?;
        Some((j, self.plans[j].as_ref()?))
    }

    /// Whether the kept twin of `id` takes its param `k` as a machine word.
    fn takes_word(&self, id: FnId, k: usize) -> bool {
        self.plan(id)
            .is_some_and(|(j, p)| self.kept.contains(&j) && p.unpacked.get(k) == Some(&true))
    }

    /// Whether `arg` reaches a call as a machine word.
    fn word(&self, arg: &MirExpr) -> bool {
        match arg {
            MirExpr::Local(l) => {
                self.kept.contains(&self.at)
                    && self.params.iter().enumerate().any(|(k, p)| {
                        p.local == l.node.slot
                            && self.plans[self.at].as_ref().is_some_and(|p| p.unpacked[k])
                    })
            }
            e => has_word(e, &|id| {
                self.plan(id)
                    .is_some_and(|(j, p)| self.kept.contains(&j) && p.kind == TwinKind::Int)
            }),
        }
    }

    fn record(&self, arg: &MirExpr, layout: &RecordFields) -> bool {
        match arg {
            MirExpr::RecordCreate(_) => true,
            MirExpr::Call(c) => matches!(c.node.callee, MirCallee::Fn(id)
                if self.plan(id).is_some_and(|(_, p)| matches!(p.kind,
                    TwinKind::Fields { struct_idx, .. } if struct_idx == layout.struct_idx))),
            MirExpr::Local(l) if self.kept.contains(&self.at) => {
                self.params.iter().enumerate().any(|(k, param)| {
                    param.local == l.node.slot
                        && self.plans[self.at]
                            .as_ref()
                            .is_some_and(|p| p.records[k].is_some())
                })
            }
            _ => false,
        }
    }

    pub(super) fn visit(&self, e: &MirExpr, used: Use, out: &mut Vec<usize>) {
        let words = |args: &[crate::ast::Spanned<MirExpr>], p: &TwinPlan| {
            args.iter()
                .zip(&p.unpacked)
                .any(|(a, u)| *u && self.word(&a.node))
        };
        let records = |args: &[crate::ast::Spanned<MirExpr>], p: &TwinPlan| {
            args.iter()
                .zip(&p.records)
                .any(|(a, r)| r.as_ref().is_some_and(|r| self.record(&a.node, r)))
        };
        let own = self.plans[self.at]
            .as_ref()
            .filter(|_| self.kept.contains(&self.at));
        match e {
            MirExpr::Call(c) => {
                if let MirCallee::Fn(id) = c.node.callee
                    && let Some((j, p)) = self.plan(id)
                {
                    let result = match &p.kind {
                        TwinKind::Plain => false,
                        TwinKind::Int => used == Use::Word,
                        TwinKind::Fields { .. } => used == Use::Subject || used == Use::Fields,
                    } || (used == Use::Tail && own.is_some_and(|o| o.kind == p.kind));
                    if result || words(&c.node.args, p) || records(&c.node.args, p) {
                        out.push(j);
                    }
                }
                for (k, a) in c.node.args.iter().enumerate() {
                    let fields = match c.node.callee {
                        MirCallee::Fn(id) => self
                            .plan(id)
                            .is_some_and(|(_, p)| p.records.get(k).is_some_and(Option::is_some)),
                        _ => false,
                    };
                    let word = match c.node.callee {
                        MirCallee::Fn(id) => self.takes_word(id, k),
                        _ => true,
                    };
                    self.visit(
                        &a.node,
                        if fields {
                            Use::Fields
                        } else if word {
                            Use::Word
                        } else {
                            Use::Other
                        },
                        out,
                    );
                }
            }
            MirExpr::TailCall(tc) => {
                // A twin tail-calls twins only; a fn without one, its own fn.
                if let Some((j, _)) = self.plan(tc.node.target)
                    && own.is_some()
                {
                    out.push(j);
                }
                for (k, a) in tc.node.args.iter().enumerate() {
                    let word = own.is_some() && self.takes_word(tc.node.target, k);
                    self.visit(&a.node, if word { Use::Word } else { Use::Other }, out);
                }
            }
            MirExpr::BinOp(b) => {
                for x in [&b.node.lhs, &b.node.rhs] {
                    self.visit(&x.node, Use::Word, out);
                }
            }
            MirExpr::Neg(x) | MirExpr::Unbox(x) => self.visit(&x.node, Use::Word, out),
            MirExpr::Project(p) => self.visit(&p.node.base.node, Use::Fields, out),
            MirExpr::Try(x) => self.visit(&x.node, Use::Subject, out),
            MirExpr::Let(l) => {
                self.visit(&l.node.value.node, Use::Other, out);
                self.visit(&l.node.body.node, used, out);
            }
            MirExpr::Match(m) => {
                self.visit(&m.node.subject.node, Use::Subject, out);
                for arm in &m.node.arms {
                    self.visit(&arm.body.node, used, out);
                }
            }
            MirExpr::IfThenElse(ite) => {
                self.visit(&ite.node.cond.node, Use::Other, out);
                self.visit(&ite.node.then_branch.node, used, out);
                self.visit(&ite.node.else_branch.node, used, out);
            }
            other => {
                crate::ir::mir::expr::walk_children(other, &mut |c| self.visit(c, Use::Other, out))
            }
        }
    }
}

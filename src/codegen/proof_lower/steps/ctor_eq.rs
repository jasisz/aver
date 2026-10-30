//! `==` between two applications of one constructor, at a type whose `==`
//! is equality: `C(x1, …, xn) == C(y1, …, yn)` is
//! `x1 == y1 && … && xn == yn`.
//!
//! Nothing new is trusted: the equation is shown from the wall rules by a
//! split on the conjunction. Where it is true, each `xi == yi` is (the
//! `Bool.and` eliminations), so each `xi` is `yi` (`int.eq.of_beq`) and the
//! left side is `C(ys) == C(ys)`, true. Where it is false, a split on the
//! left side: false agrees; true makes `C(xs)` and `C(ys)` one value
//! (`int.eq.of_beq`), so the `match` that reads the fields of a `C` and
//! compares them with the `ys` gives one answer at both, the conjunction
//! (false) at `C(xs)` and true at `C(ys)`, which cannot be.

use crate::ast::{BinOp, Spanned, Type};
use crate::ir::hir::{ResolvedCtor, ResolvedExpr, ResolvedMatchArm, ResolvedPattern};
use crate::ir::proof_steps::term::{self, Term, canon};
use crate::ir::proof_steps::{Proof, WallRule};

/// The names the fields of a constructor are bound to in the `match` that
/// reads them; a `__` name no program can write, so no `ys` holds one.
const FIELD: &str = "__steps_field";

/// `x1 == y1 && (x2 == y2 && …)`, the last comparison alone.
fn conjunction(xs: &[Term], ys: &[Term]) -> Term {
    let mut parts: Vec<Term> = xs
        .iter()
        .zip(ys)
        .map(|(x, y)| canon(&term::binop(BinOp::Eq, x.clone(), y.clone())))
        .collect();
    let mut acc = parts.pop().expect("a constructor with fields");
    while let Some(p) = parts.pop() {
        acc = term::bool_and(p, acc);
    }
    acc
}

/// `conjunction(ys, ys) = true`: each comparison by `bool.beq.refl`, each
/// `true && …` by `bool.and.true_l`.
fn all_reflexive(ys: &[Term]) -> Proof {
    let (y, rest) = ys.split_first().expect("a constructor with fields");
    let refl = Proof::Rule {
        rule: WallRule::BeqRefl,
        subst: vec![("a".into(), y.clone())],
        premises: Vec::new(),
    };
    if rest.is_empty() {
        return refl;
    }
    let tail = conjunction(rest, rest);
    Proof::Trans {
        terms: vec![
            conjunction(ys, ys),
            term::bool_and(term::boolean(true), tail.clone()),
            tail.clone(),
            term::boolean(true),
        ],
        steps: vec![
            Proof::Congr {
                ctx: term::bool_and(term::hole(), tail.clone()),
                inner: Box::new(refl),
            },
            Proof::Rule {
                rule: WallRule::AndTrueL,
                subst: vec![("b".into(), tail)],
                premises: Vec::new(),
            },
            all_reflexive(rest),
        ],
    }
}

/// `xi == yi = true` for each field, from `holds : conjunction = true`.
fn each_true(xs: &[Term], ys: &[Term], holds: Proof) -> Vec<Proof> {
    if xs.len() == 1 {
        return vec![holds];
    }
    let head = canon(&term::binop(BinOp::Eq, xs[0].clone(), ys[0].clone()));
    let tail = conjunction(&xs[1..], &ys[1..]);
    let elim = |rule| Proof::Rule {
        rule,
        subst: vec![("a".into(), head.clone()), ("b".into(), tail.clone())],
        premises: vec![holds.clone()],
    };
    let mut out = vec![elim(WallRule::AndElimL)];
    out.extend(each_true(&xs[1..], &ys[1..], elim(WallRule::AndElimR)));
    out
}

fn eq_of_beq(a: &Term, b: &Term, premise: Proof) -> Proof {
    Proof::Rule {
        rule: WallRule::EqOfBeq,
        subst: vec![("a".into(), a.clone()), ("b".into(), b.clone())],
        premises: vec![premise],
    }
}

/// `C(args)` keeping the type of `like`.
fn rebuilt(like: &Term, ctor: &ResolvedCtor, args: Vec<Term>) -> Term {
    let t = Spanned::bare(ResolvedExpr::Ctor(
        ctor.clone(),
        crate::ast::shared::vec(args),
    ));
    if let Some(ty) = like.ty() {
        t.set_ty(ty.clone());
    }
    t
}

impl super::env::Env<'_> {
    /// `cur = conjunction` for `cur` a `==` between two applications of one
    /// constructor with fields, at a type whose `==` is equality.
    pub(super) fn ctor_fields_equal(&mut self, cur: &Term) -> Option<(Proof, Term)> {
        let ResolvedExpr::BinOp(BinOp::Eq, a, b) = &cur.node else {
            return None;
        };
        let (a, b) = (canon(a), canon(b));
        let (ResolvedExpr::Ctor(c, xs), ResolvedExpr::Ctor(d, ys)) = (&a.node, &b.node) else {
            return None;
        };
        let same = match (c, d) {
            (ResolvedCtor::User { ctor_id: p, .. }, ResolvedCtor::User { ctor_id: q, .. }) => {
                p == q
            }
            (ResolvedCtor::Builtin(p), ResolvedCtor::Builtin(q)) => p == q,
            _ => false,
        };
        if !same || xs.is_empty() || xs.len() != ys.len() || !self.beq_is_equality(&a) {
            return None;
        }
        let others = match c {
            ResolvedCtor::User { type_id, .. } => {
                self.inputs.symbol_table.type_entry(*type_id).variants.len() > 1
            }
            _ => true,
        };
        let (xs, ys): (Vec<Term>, Vec<Term>) = (xs.to_vec(), ys.to_vec());
        let lhs = canon(cur);
        let conj = conjunction(&xs, &ys);

        // The conjunction true: every field equal, so the left side is
        // `C(ys) == C(ys)`.
        let on_conj = self.fresh_hyp();
        let mut terms = vec![lhs.clone()];
        let mut steps = Vec::new();
        let mut now = xs.clone();
        for (i, holds) in each_true(&xs, &ys, Proof::Hyp(on_conj.clone()))
            .into_iter()
            .enumerate()
        {
            if xs[i] == ys[i] {
                continue;
            }
            let mut ctx_args = now.clone();
            ctx_args[i] = term::hole();
            steps.push(Proof::Congr {
                ctx: term::binop(BinOp::Eq, rebuilt(&a, c, ctx_args), b.clone()),
                inner: Box::new(eq_of_beq(&xs[i], &ys[i], holds)),
            });
            now[i] = ys[i].clone();
            terms.push(canon(&term::binop(
                BinOp::Eq,
                rebuilt(&a, c, now.clone()),
                b.clone(),
            )));
        }
        steps.push(Proof::Rule {
            rule: WallRule::BeqRefl,
            subst: vec![("a".into(), b.clone())],
            premises: Vec::new(),
        });
        terms.push(term::boolean(true));
        steps.push(Proof::Symm(Box::new(Proof::Hyp(on_conj.clone()))));
        terms.push(conj.clone());
        let if_true = Proof::Trans { terms, steps };

        // The conjunction false: the left side false, or true and then the
        // fields read off both sides agree.
        let on_lhs = self.fresh_hyp();
        let names: Vec<String> = (1..=xs.len()).map(|i| format!("{FIELD}{i}")).collect();
        let bound: Vec<Term> = names
            .iter()
            .zip(&xs)
            .map(|(n, x)| {
                let v = term::var(n);
                if let Some(ty) = x.ty() {
                    v.set_ty(ty.clone());
                }
                v
            })
            .collect();
        let mut arms = vec![ResolvedMatchArm {
            pattern: ResolvedPattern::Ctor(c.clone(), names),
            body: crate::ast::Shared::new(conjunction(&bound, &ys)),
            binding_slots: std::sync::OnceLock::new(),
        }];
        if others {
            arms.push(ResolvedMatchArm {
                pattern: ResolvedPattern::Wildcard,
                body: crate::ast::Shared::new(term::boolean(true)),
                binding_slots: std::sync::OnceLock::new(),
            });
        }
        let reads = |s: Term| {
            let t = Spanned::bare(ResolvedExpr::Match {
                subject: crate::ast::Shared::new(s),
                arms: crate::ast::Shared::new(arms.clone()),
            });
            t.set_ty(Type::Bool);
            t
        };
        let arm = |s: &Term, binders: &[Term]| Proof::Arm {
            term: reads(s.clone()),
            arm: 1,
            binders: binders.to_vec(),
            premise: Box::new(Proof::Refl(s.clone())),
        };
        let contradiction = Proof::Trans {
            terms: vec![
                term::boolean(false),
                conj.clone(),
                reads(a.clone()),
                reads(b.clone()),
                conjunction(&ys, &ys),
                term::boolean(true),
            ],
            steps: vec![
                Proof::Symm(Box::new(Proof::Hyp(on_conj.clone()))),
                Proof::Symm(Box::new(arm(&a, &xs))),
                Proof::Congr {
                    ctx: reads(term::hole()),
                    inner: Box::new(eq_of_beq(&a, &b, Proof::Hyp(on_lhs.clone()))),
                },
                arm(&b, &ys),
                all_reflexive(&ys),
            ],
        };
        let if_false = Proof::Cases {
            on: lhs.clone(),
            hyp: on_lhs.clone(),
            if_true: Box::new(Proof::Absurd {
                contradiction: Box::new(contradiction),
                lhs: lhs.clone(),
                rhs: conj.clone(),
            }),
            if_false: Box::new(Proof::Trans {
                terms: vec![lhs.clone(), term::boolean(false), conj.clone()],
                steps: vec![
                    Proof::Hyp(on_lhs),
                    Proof::Symm(Box::new(Proof::Hyp(on_conj.clone()))),
                ],
            }),
        };
        Some((
            Proof::Cases {
                on: conj.clone(),
                hyp: on_conj,
                if_true: Box::new(if_true),
                if_false: Box::new(if_false),
            },
            conj,
        ))
    }
}

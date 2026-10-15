//! A case its hypotheses rule out. After a case split the hypotheses in
//! scope are stated about terms as they stood before it; read again, they
//! may contradict each other, and then the case cannot happen and closes by
//! [`Proof::Absurd`]. Every reading is a computation the kernel checks:
//!
//! - **lengths**: a hypothesis between two lists (`List.reverse(x) = []`,
//!   `x = List.prepend(h, t)`, which a split on a list states) gives the
//!   equality of their lengths, written in normal form with the builtin
//!   list facts and the constructor equations of `List.len`; linear
//!   arithmetic then reads those lengths with the comparisons in scope
//!   (`List.len(x) > 0` against `List.len(x) == 0`);
//! - **normal forms**: each hypothesis with a Bool value is evaluated to its
//!   normal form, its parts too, under the others (a stated `x == 3`
//!   substitutes, a record built and read back gives its field, a predicate
//!   opens): one that evaluates to the other Bool closes the case, and so do
//!   two whose normal forms are the same term with different values.
//!
//! Nothing is searched: each hypothesis is read once, by evaluation, within
//! a budget of its own ([`REFUTE_FUEL`]) that neither the claim nor the
//! normal forms of [`Env::normal_fact`] draw on. A case with the same
//! hypotheses is read once per attempt.
//!
//! Where all of that leaves a case open, its hypotheses are evaluated the
//! way its claim was ([`super::evaluated`]).

use crate::ast::{BinOp, Type};
use crate::ir::hir::{ResolvedCallee, ResolvedExpr};
use crate::ir::proof_steps::term::{self, Term, canon};
use crate::ir::proof_steps::{Eqn, Proof, WallRule};

use super::env::Env;
use super::rewrite::Equation;

/// Steps reading the hypotheses again may take in one attempt, apart from
/// the claim's fuel and the normal forms' budget.
pub(crate) const REFUTE_FUEL: usize = 1000;

/// Whether `t` is a list, by its type or its shape.
fn is_list(t: &Term) -> bool {
    matches!(t.ty(), Some(Type::List(_)))
        || matches!(t.node, ResolvedExpr::List(_))
        || matches!(&t.node, ResolvedExpr::Call(ResolvedCallee::Builtin(b), _)
            if matches!(b.as_str(), "List.prepend" | "List.concat" | "List.reverse" | "List.take" | "List.drop"))
}

fn len(l: Term) -> Term {
    term::builtin("List.len", vec![l], Some(Type::Int))
}

impl Env<'_> {
    /// `lhs = rhs` where the hypotheses in scope contradict each other, read
    /// as the module comment says; `None` when they do not, or when this
    /// attempt already read the same hypotheses.
    pub(crate) fn refute_case(&mut self, lhs: &Term, rhs: &Term) -> Option<Proof> {
        let seen = super::memo::debug_hash(&[&self.hyps]);
        if self.refuted.contains(&seen) {
            return None;
        }
        self.refuted.push(seen);
        let wide = self.wide_linear;
        self.wide_linear = true;
        let found = self.refute_reading(lhs, rhs);
        self.wide_linear = wide;
        found
    }

    fn refute_reading(&mut self, lhs: &Term, rhs: &Term) -> Option<Proof> {
        let absurd = |contradiction: Proof| Proof::Absurd {
            contradiction: Box::new(contradiction),
            lhs: canon(lhs),
            rhs: canon(rhs),
        };
        if let Some(proof) = self.refute_by_lengths(&absurd) {
            return Some(proof);
        }
        self.refute_by_normal_forms().map(absurd)
    }

    /// Run `f` on the refutation budget instead of the claim's fuel.
    fn on_refute_fuel<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let (fuel, normal) = (self.fuel, self.normal_fuel);
        self.fuel = self.refute_fuel;
        self.normal_fuel = self.refute_fuel;
        let out = f(self);
        // Whatever drew on either, the refutation budget pays.
        let spent = (self.refute_fuel - self.fuel) + (self.refute_fuel - self.normal_fuel);
        self.refute_fuel = self.refute_fuel.saturating_sub(spent);
        self.fuel = fuel;
        self.normal_fuel = normal;
        out
    }

    /// The lengths of the two sides of each list hypothesis, equal, in
    /// normal form, cut in; a contradiction linear arithmetic finds with
    /// them closes the case.
    fn refute_by_lengths(&mut self, absurd: &dyn Fn(Proof) -> Proof) -> Option<Proof> {
        let lists: Vec<(String, Eqn)> = self
            .hyps
            .iter()
            .filter(|(_, e)| term::bool_value(&e.rhs).is_none())
            .filter(|(_, e)| is_list(&e.lhs) || is_list(&e.rhs))
            .cloned()
            .collect();
        if lists.is_empty() {
            return None;
        }
        let marks = (self.laws.len(), self.used.len(), self.used_consts.len());
        let mut eqs: Vec<Equation> = crate::ir::proof_steps::facts::normalizing()
            .iter()
            .map(|f| Equation::Law(Box::new(f.law_ref())))
            .collect();
        eqs.push(Equation::Closed(Box::new((
            canon(&len(term::nil())),
            term::int(&0.into()),
        ))));
        eqs.push(Equation::Wall(WallRule::LenCons));
        let mut cuts: Vec<(String, Term, Proof)> = Vec::new();
        for (name, e) in lists {
            let (l, r) = (canon(&e.lhs), canon(&e.rhs));
            let stated = canon(&term::binop(BinOp::Eq, len(l), len(r.clone())));
            // `(len l == len r) = (len r == len r) = true`.
            let same = canon(&term::binop(BinOp::Eq, len(r.clone()), len(r.clone())));
            let holds = Proof::Trans {
                terms: vec![stated.clone(), same, term::boolean(true)],
                steps: vec![
                    Proof::Congr {
                        ctx: term::binop(BinOp::Eq, len(term::hole()), len(r.clone())),
                        inner: Box::new(Proof::Hyp(name)),
                    },
                    Proof::Rule {
                        rule: WallRule::BeqRefl,
                        subst: vec![("a".into(), canon(&len(r)))],
                        premises: Vec::new(),
                    },
                ],
            };
            let Ok(chain) = self.on_refute_fuel(|env| env.rewrite(&stated, &eqs)) else {
                continue;
            };
            let (fact, proof) = if chain.is_empty() {
                (stated, holds)
            } else {
                let (to, bridge) = chain.finish();
                let to = canon(&to);
                (
                    to.clone(),
                    Proof::Trans {
                        terms: vec![to, stated, term::boolean(true)],
                        steps: vec![Proof::Symm(Box::new(bridge)), holds],
                    },
                )
            };
            if self.hyp_for(&fact).is_some() || cuts.iter().any(|(_, f, _)| *f == fact) {
                continue;
            }
            // Named apart from every hypothesis in scope; no fresh name is
            // given out unless the case closes.
            let name = format!("h_len{}", self.hyps.len() + cuts.len());
            cuts.push((name, fact, proof));
        }
        let base = self.hyps.len();
        for (name, fact, _) in &cuts {
            self.hyps
                .push((name.clone(), Eqn::new(fact.clone(), term::boolean(true))));
        }
        let found = self.on_refute_fuel(|env| {
            env.refute_linearly()
                .or_else(|| env.refute_an_equality(&cuts))
        });
        self.hyps.truncate(base);
        let Some(contradiction) = found else {
            self.laws.truncate(marks.0);
            self.used.truncate(marks.1);
            self.used_consts.truncate(marks.2);
            return None;
        };
        let mut proof = absurd(contradiction);
        for (name, fact, from) in cuts.into_iter().rev() {
            if super::uses_hyp(&proof, &name) {
                proof = Proof::Have {
                    name,
                    fact,
                    proof: Box::new(from),
                    body: Box::new(proof),
                };
            }
        }
        Some(proof)
    }

    /// `false = true` from one of the length equalities `cuts` stated true,
    /// which linear arithmetic over the other hypotheses decides false
    /// (`List.len(x) == 1 + List.len(t)` against `List.len(x) == 0`).
    fn refute_an_equality(&mut self, cuts: &[(String, Term, Proof)]) -> Option<Proof> {
        for (name, fact, _) in cuts {
            if !matches!(fact.node, ResolvedExpr::BinOp(BinOp::Eq, ..)) {
                continue;
            }
            let at = self.hyps.iter().position(|(n, _)| n == name)?;
            let held = self.hyps.remove(at);
            let decided = self.decide_linearly(fact);
            self.hyps.insert(at, held);
            if let Some((proof, value)) = decided
                && term::bool_value(&value) == Some(false)
            {
                return Some(Proof::Trans {
                    terms: vec![term::boolean(false), fact.clone(), term::boolean(true)],
                    steps: vec![Proof::Symm(Box::new(proof)), Proof::Hyp(name.clone())],
                });
            }
        }
        None
    }

    /// The Bool a builtin fact whose right side is one states `t` to be,
    /// applied at `t` itself, with its proof and the fact
    /// (`List.prepend.notEmpty`): one of the facts the producer rewrites
    /// with, or one the law cites.
    fn decided_by_a_fact(
        &mut self,
        t: &Term,
    ) -> Option<(Term, Proof, crate::ir::proof_steps::LawRef)> {
        if !matches!(t.node, ResolvedExpr::BinOp(BinOp::Eq, ..)) {
            return None;
        }
        let deciding: Vec<crate::ir::proof_steps::LawRef> = self
            .fact_rules
            .iter()
            .chain(self.cited_all.iter())
            .filter(|l| l.fact.is_some() && term::bool_value(&l.rhs).is_some())
            .cloned()
            .collect();
        for law in deciding {
            let eq = Equation::Law(Box::new(law.clone()));
            if let Some((proof, to)) = self.try_equation(&eq, t)
                && term::bool_value(&to).is_some()
            {
                return Some((to, proof, law));
            }
        }
        None
    }

    /// The Bool a builtin fact states the equality `t` to be once each of
    /// its sides is in normal form, with the proof that `t` is it and what
    /// that proof needs in the script: the normal forms and the fact.
    #[allow(clippy::type_complexity)]
    fn decided_by_parts(
        &mut self,
        t: &Term,
    ) -> Option<(
        Term,
        Proof,
        Vec<super::eval::NormalFact>,
        crate::ir::proof_steps::LawRef,
    )> {
        let ResolvedExpr::BinOp(BinOp::Eq, l, r) = &t.node else {
            return None;
        };
        let (l, r) = (canon(l), canon(r));
        // Only where a deciding fact states this side as it stands, a
        // closed term (`[]`), so no other equality pays for the normal
        // forms.
        let closed = |t: &Term| {
            let mut fv = Vec::new();
            term::free_vars(t, &mut fv);
            fv.is_empty()
        };
        let fits = self
            .fact_rules
            .iter()
            .chain(self.cited_all.iter())
            .filter(|f| f.fact.is_some() && term::bool_value(&f.rhs).is_some())
            .any(|f| match &f.lhs.node {
                ResolvedExpr::BinOp(BinOp::Eq, a, b) => {
                    let same = |p: &Term, t: &Term| {
                        closed(p) && super::rewrite::matches(p, t, &[], &mut Vec::new())
                    };
                    same(a, &l) || same(b, &r)
                }
                _ => false,
            });
        if !fits {
            return None;
        }
        // Without the hypotheses stating `t`, which would substitute one
        // side for the other.
        let saved = self.hyps.clone();
        self.hyps.retain(|(_, h)| *term::canon_ref(&h.lhs) != *t);
        let nl = self.on_refute_fuel(|env| env.normal_fact(&l));
        let nr = self.on_refute_fuel(|env| env.normal_fact(&r));
        self.hyps = saved;
        let l2 = nl.as_ref().map_or(l.clone(), |n| n.to.clone());
        let r2 = nr.as_ref().map_or(r.clone(), |n| n.to.clone());
        let read = canon(&term::binop(BinOp::Eq, l2.clone(), r2.clone()));
        let (value, decided, fact) = self.decided_by_a_fact(&read)?;
        let mut terms = vec![t.clone()];
        let mut steps = Vec::new();
        if let Some(n) = &nl {
            steps.push(Proof::Congr {
                ctx: term::binop(BinOp::Eq, term::hole(), r.clone()),
                inner: Box::new(n.proof.clone()),
            });
            terms.push(canon(&term::binop(BinOp::Eq, l2.clone(), r.clone())));
        }
        if let Some(n) = &nr {
            steps.push(Proof::Congr {
                ctx: term::binop(BinOp::Eq, l2.clone(), term::hole()),
                inner: Box::new(n.proof.clone()),
            });
            terms.push(read.clone());
        }
        steps.push(decided);
        terms.push(value.clone());
        let needs = [nl, nr].into_iter().flatten().collect();
        Some((value, Proof::Trans { terms, steps }, needs, fact))
    }

    /// A proof of `b = !b` from a hypothesis whose normal form is the other
    /// Bool, or from two whose normal forms are one term stated with
    /// different values.
    fn refute_by_normal_forms(&mut self) -> Option<Proof> {
        // Each form with the proof that it equals the value stated, and
        // what that proof needs in the script.
        let mut forms: Vec<(Term, Proof, bool, Option<super::eval::NormalFact>)> = Vec::new();
        for (name, e) in self.hyps.clone() {
            let Some(said) = term::bool_value(&e.rhs) else {
                continue;
            };
            let fact = canon(&e.lhs);
            if term::is_literal(&fact) {
                continue;
            }
            let found = self.on_refute_fuel(|env| env.normal_fact(&fact));
            let (to, from) = match &found {
                Some(n) => (
                    n.to.clone(),
                    Proof::Trans {
                        terms: vec![n.to.clone(), fact.clone(), term::boolean(said)],
                        steps: vec![
                            Proof::Symm(Box::new(n.proof.clone())),
                            Proof::Hyp(name.clone()),
                        ],
                    },
                ),
                None => (fact.clone(), Proof::Hyp(name.clone())),
            };
            if term::bool_value(&to) == Some(!said) {
                if let Some(n) = &found {
                    self.keep_needs(n);
                }
                return Some(from);
            }
            // An equality a builtin fact decides once its sides are read
            // in normal form, the other Bool than stated:
            // `List.drop(List.prepend(h, t), n) == []` under `n <= 0` is
            // `List.prepend(h, t) == []`, which is `false`. The sides are
            // read on their own, as the whole may read back the hypothesis.
            if let Some((value, decided, needs, law)) = self.decided_by_parts(&fact)
                && term::bool_value(&value) == Some(!said)
            {
                for n in &needs {
                    self.keep_needs(n);
                }
                self.use_law(law);
                return Some(Proof::Trans {
                    terms: vec![value, fact.clone(), term::boolean(said)],
                    steps: vec![Proof::Symm(Box::new(decided)), Proof::Hyp(name)],
                });
            }
            if let Some((_, other, _, needs)) =
                forms.iter().find(|(f, _, v, _)| *f == to && *v != said)
            {
                let other = other.clone();
                for n in [needs.clone(), found.clone()].into_iter().flatten() {
                    self.keep_needs(&n);
                }
                // `!said = to = said`.
                return Some(Proof::Trans {
                    terms: vec![term::boolean(!said), to, term::boolean(said)],
                    steps: vec![Proof::Symm(Box::new(other)), from],
                });
            }
            forms.push((to, from, said, found));
        }
        None
    }
}

//! A case its hypotheses rule out. After a case split the hypotheses in
//! scope are stated about terms as they stood before it; read again, they
//! may contradict each other, and then the case cannot happen and closes by
//! [`Proof::Absurd`]. The reading here is by lengths, a computation the
//! kernel checks: a hypothesis between two lists (`List.reverse(x) = []`,
//! `x = List.prepend(h, t)`, which a split on a list states) gives the
//! equality of their lengths, written in normal form with the builtin list
//! facts and the constructor equations of `List.len`; linear arithmetic then
//! reads those lengths with the comparisons in scope (`List.len(x) > 0`
//! against `List.len(x) == 0`).
//!
//! Nothing is searched: each hypothesis is read once, within a budget of its
//! own ([`REFUTE_FUEL`]) that neither the claim nor the normal forms of
//! [`Env::normal_fact`] draw on. A case with the same hypotheses is read once
//! per attempt.
//!
//! Where that leaves a case open, its hypotheses are evaluated the way its
//! claim was ([`super::evaluated`]): a hypothesis whose form is the other
//! Bool, or two whose forms are one term with different values, close the
//! case there.

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
        let absurd = |contradiction: Proof| Proof::Absurd {
            contradiction: Box::new(contradiction),
            lhs: canon(lhs),
            rhs: canon(rhs),
        };
        let found = self.refute_by_lengths(&absurd);
        self.wide_linear = wide;
        found
    }

    /// Run `f` on the refutation budget instead of the claim's fuel.
    fn on_refute_fuel<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let (fuel, normal) = (self.fuel, self.normal_fuel);
        self.fuel = self.refute_fuel;
        self.normal_fuel = self.refute_fuel;
        let out = f(self);
        // Whatever drew on either, the refutation budget pays.
        // A refutation inside another leaves less budget than it started
        // with.
        let spent = self.refute_fuel.saturating_sub(self.fuel)
            + self.refute_fuel.saturating_sub(self.normal_fuel);
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
}

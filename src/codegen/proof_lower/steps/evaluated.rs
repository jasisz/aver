//! The hypotheses of a case, evaluated the way its claim is.
//!
//! A claim is evaluated to the end; a hypothesis used to be read as it was
//! stated, or evaluated under every other hypothesis in scope. But a case
//! split restates each hypothesis about the terms it split, and a `when` of
//! several lines is in scope whole and line by line, so under all the others
//! a hypothesis is answered by its own restatement and never evaluates.
//!
//! Where everything else leaves a case open
//! ([`Env::prove_with_evaluated_hypotheses`]), each hypothesis is evaluated
//! under the equations in scope alone, so no restatement of it answers for
//! it, split at its `Bool.and`s, then once more under the other forms. A
//! form that is the other Bool, or two forms that are one term with
//! different values, close the case; otherwise, where a form no hypothesis
//! states yet stands in what the claim stopped at, the new forms are cut in
//! and the claim is evaluated again under them: one may be the claim itself
//! or decide the Bool the claim stopped at.
//!
//! An equation in scope whose left side is not a variable is read the same
//! way, as `left == right` with both sides evaluated; evaluation then
//! rewrites the evaluated left side to the right one where it meets it
//! (`Env::equal_by_hypothesis`, which takes a right side only when no left
//! side of a stated equality occurs in it, so one rewrite never feeds
//! another). An equation whose left side evaluates to a value is left out:
//! a value is never rewritten to another.
//!
//! Nothing is searched. The reading comes last, so a case that closed
//! without it keeps its proof; it runs once per case within budgets of its
//! own ([`READ_FUEL`] for the forms, [`READ_CLAIM_FUEL`] for the claim under
//! them), and every form is a [`Proof::Have`] or [`Proof::Cases`] step the
//! kernel checks.

use crate::ast::{BinOp, Type};
use crate::ir::hir::{ResolvedCallee, ResolvedExpr};
use crate::ir::proof_steps::term::{self, Term, canon};
use crate::ir::proof_steps::{Eqn, Proof, WallRule};

use super::env::Env;

/// Steps evaluating the hypotheses the way the claim was evaluated may take
/// in one attempt ([`Env::prove_with_evaluated_hypotheses`]), apart from
/// every other budget.
pub(crate) const READ_FUEL: usize = 800;

/// Steps evaluating one hypothesis may take: one that takes longer is a
/// computation, not a shape to read.
const READ_FACT_FUEL: usize = 150;

/// Steps the evaluations of a claim under its evaluated hypotheses may take
/// in one attempt, all of them together. One that closes the claim also
/// pays them from the claim's fuel; one that fails leaves that fuel as it
/// was.
pub(crate) const READ_CLAIM_FUEL: usize = 500;

/// A budget of this module as an attempt gets it: `limit`, or a smaller
/// percentage of it named by `AVER_STEPS_READ_BUDGET_PERCENT`.
///
/// The variable is an internal switch for one test, which sets 50 to check
/// that the laws these readings close need less than half of each budget,
/// so a change that eats the room shows there before a law reopens. It can
/// only shrink a budget: a value above 100, or one that is not a number, is
/// read as 100, so no environment makes a source close more than another.
pub(crate) fn budget(limit: usize) -> usize {
    static PERCENT: std::sync::OnceLock<usize> = std::sync::OnceLock::new();
    let percent = *PERCENT.get_or_init(|| {
        std::env::var("AVER_STEPS_READ_BUDGET_PERCENT")
            .ok()
            .and_then(|v| v.parse::<usize>().ok())
            .map_or(100, |p| p.min(100))
    });
    limit * percent / 100
}

/// How many evaluations of the hypotheses may be in progress, one inside
/// the claim another went on to.
const MAX_READINGS: usize = 2;

/// One hypothesis in the form evaluation gives it.
struct Evaluated {
    /// The name the proof reads it by: the hypothesis that states it
    /// already, or the name it is cut in under.
    name: String,
    fact: Term,
    said: bool,
    /// `fact = said`; `None` for a form a hypothesis in scope states.
    proof: Option<Proof>,
    /// What the evaluations behind `proof` need in the script.
    needs: Vec<super::eval::NormalFact>,
}

/// Whether `t` is a value at its head: a literal, a constructor, a record,
/// a tuple, a list literal or a list cell.
fn is_value(t: &Term) -> bool {
    term::is_literal(t)
        || matches!(
            t.node,
            ResolvedExpr::List(_)
                | ResolvedExpr::Ctor(..)
                | ResolvedExpr::RecordCreate { .. }
                | ResolvedExpr::Tuple(_)
        )
        || matches!(&t.node, ResolvedExpr::Call(ResolvedCallee::Builtin(b), _) if b == "List.prepend")
}

/// Whether `part` stands in `t` where evaluation reads it: `t` itself or a
/// part of it, not inside the arms of a `match`.
fn stands_in(part: &Term, t: &Term) -> bool {
    *term::canon_ref(t) == *part || term::children(t).into_iter().any(|c| stands_in(part, c))
}

/// What a try that failed may have written in an attempt, as it stood
/// before the try ([`Env::mark`]).
pub(super) struct Mark {
    fuel: usize,
    named: usize,
    hints: usize,
    open_premises: usize,
    open_whens: usize,
    opened: Vec<Term>,
    barren: Vec<Term>,
    ran_dry: bool,
    laws: usize,
    used: usize,
    used_consts: usize,
}

impl Env<'_> {
    /// The attempt as it stands, for [`Self::rewind`].
    pub(super) fn mark(&self) -> Mark {
        Mark {
            fuel: self.fuel,
            named: self.next_hyp_count(),
            hints: self.hints.len(),
            open_premises: self.open_premises.len(),
            open_whens: self.open_whens.len(),
            opened: self.opened.clone(),
            barren: self.barren.clone(),
            ran_dry: self.ran_dry,
            laws: self.laws.len(),
            used: self.used.len(),
            used_consts: self.used_consts.len(),
        }
    }

    /// Take back what a failed try wrote since `mark`: its fuel, the names
    /// it gave out, its notes and hints, and the definitions and laws it
    /// put in the script. The attempt goes on as if the try never ran, so a
    /// proof found without it is the proof found before.
    pub(super) fn rewind(&mut self, mark: Mark) {
        self.fuel = mark.fuel;
        self.rewind_hyp_count(mark.named);
        self.hints.truncate(mark.hints);
        self.open_premises.truncate(mark.open_premises);
        self.open_whens.truncate(mark.open_whens);
        self.opened = mark.opened;
        self.barren = mark.barren;
        self.ran_dry = mark.ran_dry;
        self.laws.truncate(mark.laws);
        self.used.truncate(mark.used);
        self.used_consts.truncate(mark.used_consts);
    }

    /// `fact` in its normal form with only `hyps` in scope, within
    /// [`READ_FACT_FUEL`] steps of the budget for these evaluations.
    fn evaluated_under(
        &mut self,
        fact: &Term,
        hyps: Vec<(String, Eqn)>,
    ) -> Option<super::eval::NormalFact> {
        let budget = self.read_fuel.min(budget(READ_FACT_FUEL));
        let saved = std::mem::replace(&mut self.hyps, hyps);
        // `normal_fact` draws on `normal_fuel` alone.
        let normal = std::mem::replace(&mut self.normal_fuel, budget);
        let found = self.normal_fact(fact);
        self.read_fuel -= budget - self.normal_fuel;
        self.normal_fuel = normal;
        self.hyps = saved;
        found
    }

    /// Whether the kernel splits `t` into its true and false cases: a
    /// comparison, a Bool connective, or a call of a function that returns
    /// a Bool.
    fn splits_as_bool(&self, t: &Term) -> bool {
        match &t.node {
            ResolvedExpr::BinOp(
                BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Lte | BinOp::Gt | BinOp::Gte,
                ..,
            ) => true,
            ResolvedExpr::Call(ResolvedCallee::Builtin(b), _) => {
                matches!(b.as_str(), "Bool.and" | "Bool.or" | "Bool.not")
            }
            ResolvedExpr::Call(ResolvedCallee::Fn(_), _) => matches!(t.ty(), Some(Type::Bool)),
            _ => false,
        }
    }

    /// The parts of `to`, which `from` proves is `said`, taken into `forms`:
    /// each `Bool.and` line of a true one, a false one whole. `Some` with a
    /// proof that `true` and `false` are equal when a part is the other
    /// Bool, or a form already met with the other value.
    fn take_forms(
        &mut self,
        forms: &mut Vec<Evaluated>,
        to: &Term,
        said: bool,
        from: Proof,
        needs: &[super::eval::NormalFact],
    ) -> Option<Proof> {
        let mut parts = Vec::new();
        if said {
            super::split_and(to, from, &mut parts);
        } else {
            parts.push((canon(to), from));
        }
        for (part, from) in parts {
            match term::bool_value(&part) {
                Some(v) if v != said => return Some(from),
                Some(_) => continue,
                None => {}
            }
            if let Some(other) = forms.iter().find(|f| f.fact == part) {
                if other.said == said {
                    continue;
                }
                // By its own proof: it may not be cut in yet.
                let theirs = other
                    .proof
                    .clone()
                    .unwrap_or_else(|| Proof::Hyp(other.name.clone()));
                for n in other.needs.clone() {
                    self.keep_needs(&n);
                }
                // `!said = part = said`.
                return Some(Proof::Trans {
                    terms: vec![term::boolean(!said), part, term::boolean(said)],
                    steps: vec![Proof::Symm(Box::new(theirs)), from],
                });
            }
            let stated = self.hyps.iter().rev().find(|(_, e)| {
                *term::canon_ref(&e.lhs) == part && term::bool_value(&e.rhs) == Some(said)
            });
            let (name, proof) = match stated {
                Some((name, _)) => (name.clone(), None),
                // Named apart from every form of this attempt, also those of
                // a reading still in progress around this one.
                None => {
                    self.next_read += 1;
                    (format!("h_read{}", self.next_read), Some(from))
                }
            };
            forms.push(Evaluated {
                name,
                fact: part,
                said,
                proof,
                needs: needs.to_vec(),
            });
        }
        None
    }

    /// Whether the proof may read `f` by its name: a hypothesis states it,
    /// or it is cut in, which a form stated false is only where the kernel
    /// can split on it (any other one only closes a case).
    fn in_scope(&self, f: &Evaluated) -> bool {
        f.proof.is_none() || f.said || self.splits_as_bool(&f.fact)
    }

    /// `lhs = rhs` with the hypotheses in scope evaluated the way a claim is
    /// (see the module comment), `stopped` being the terms its claim
    /// stopped at; `None` when that neither closes the case nor gives a form
    /// that stands in those, when the claim stays open under the new forms,
    /// or when this attempt already read this case.
    ///
    /// Every hypothesis with a Bool value is read, and every equation whose
    /// left side is not a variable, as `(left == right) = true`, which its
    /// own hypothesis proves and evaluation then reads as that equality
    /// between the evaluated sides.
    pub(crate) fn prove_with_evaluated_hypotheses(
        &mut self,
        lhs: &Term,
        rhs: &Term,
        stopped: &[&Term],
        depth: usize,
    ) -> Option<Proof> {
        if self.hypotheses_as_stated || self.read_fuel == 0 || self.reading >= MAX_READINGS {
            return None;
        }
        let seen = super::memo::debug_hash(&[&self.hyps, &canon(lhs), &canon(rhs)]);
        if self.read_seen.contains(&seen) {
            return None;
        }
        self.read_seen.push(seen);

        let all = self.hyps.clone();
        let equations: Vec<(String, Eqn)> = all
            .iter()
            .filter(|(_, e)| term::bool_value(&e.rhs).is_none())
            .cloned()
            .collect();
        // What is read: the fact, its value, its proof, and the equation it
        // restates, which is left out of scope while it is evaluated.
        let mut stated: Vec<(Term, bool, Proof, Option<String>)> = Vec::new();
        for (name, e) in &all {
            let (l, r) = (canon(&e.lhs), canon(&e.rhs));
            if let Some(said) = term::bool_value(&r) {
                if !term::is_literal(&l) {
                    stated.push((l, said, Proof::Hyp(name.clone()), None));
                }
                continue;
            }
            let reflects = |t: &Term| {
                t.ty()
                    .is_some_and(|ty| self.inputs.symbol_table.beq_is_equality(ty))
            };
            if matches!(l.node, ResolvedExpr::Ident(_)) || !(reflects(&l) || reflects(&r)) {
                continue;
            }
            // `(l == r) = (r == r) = true`.
            let fact = canon(&term::binop(BinOp::Eq, l, r.clone()));
            let same = canon(&term::binop(BinOp::Eq, r.clone(), r.clone()));
            let proof = Proof::Trans {
                terms: vec![fact.clone(), same, term::boolean(true)],
                steps: vec![
                    Proof::Congr {
                        ctx: term::binop(BinOp::Eq, term::hole(), r.clone()),
                        inner: Box::new(Proof::Hyp(name.clone())),
                    },
                    Proof::Rule {
                        rule: WallRule::BeqRefl,
                        subst: vec![("a".into(), r)],
                        premises: Vec::new(),
                    },
                ],
            };
            stated.push((fact, true, proof, Some(name.clone())));
        }
        if stated.is_empty() {
            return None;
        }

        let mut forms: Vec<Evaluated> = Vec::new();
        // A split on a variable restates each hypothesis that names it with
        // the constructor in its place; evaluated, the two are one form, so
        // of the hypotheses that are the same once every such variable is
        // read as its constructor only the first is evaluated.
        let reads: Vec<(String, Term)> = equations
            .iter()
            .rev()
            .filter_map(|(_, e)| match &canon(&e.lhs).node {
                ResolvedExpr::Ident(n) => Some((n.clone(), canon(&e.rhs))),
                _ => None,
            })
            .collect();
        let as_read = |t: &Term| {
            let mut cur = t.clone();
            for _ in 0..reads.len().min(8) {
                match term::subst(&cur, &reads).map(|next| canon(&next)) {
                    Ok(next) if next != cur => cur = next,
                    _ => break,
                }
            }
            cur
        };
        let mut met: Vec<(Term, bool)> = Vec::new();
        // Each one under the equations alone.
        for (fact, said, proof, restates) in stated {
            if restates.is_none() && !reads.is_empty() {
                let read = (as_read(&fact), said);
                if met.contains(&read) {
                    continue;
                }
                met.push(read);
            }
            let scope: Vec<(String, Eqn)> = equations
                .iter()
                .filter(|(n, _)| Some(n) != restates.as_ref())
                .cloned()
                .collect();
            let (to, from, needs) = match self.evaluated_under(&fact, scope) {
                Some(n) => (
                    n.to.clone(),
                    Proof::Trans {
                        terms: vec![n.to.clone(), fact.clone(), term::boolean(said)],
                        steps: vec![Proof::Symm(Box::new(n.proof.clone())), proof],
                    },
                    vec![n],
                ),
                // An equation that evaluates no further says nothing new.
                None if restates.is_some() => continue,
                None => (fact, proof, Vec::new()),
            };
            // An equation rewrites its left side to its right one where
            // evaluation meets it, so one whose left side evaluated to a
            // value (a literal, a constructor, a list cell) is left out: a
            // value is never rewritten to another.
            if restates.is_some()
                && let ResolvedExpr::BinOp(BinOp::Eq, a, _) = &to.node
                && is_value(a)
            {
                continue;
            }
            if let Some(contradiction) = self.take_forms(&mut forms, &to, said, from, &needs) {
                for n in &needs {
                    self.keep_needs(n);
                }
                return Some(Proof::Absurd {
                    contradiction: Box::new(contradiction),
                    lhs: canon(lhs),
                    rhs: canon(rhs),
                });
            }
        }
        // Then each form under the equations and the other forms.
        let mut absurd: Option<Proof> = None;
        for k in 0..forms.len() {
            if !self.in_scope(&forms[k]) {
                continue;
            }
            let mut scope = equations.clone();
            for (j, f) in forms.iter().enumerate() {
                if j != k && self.in_scope(f) {
                    scope.push((
                        f.name.clone(),
                        Eqn::new(f.fact.clone(), term::boolean(f.said)),
                    ));
                }
            }
            let (fact, said, name) = (forms[k].fact.clone(), forms[k].said, forms[k].name.clone());
            let Some(n) = self.evaluated_under(&fact, scope) else {
                continue;
            };
            let from = Proof::Trans {
                terms: vec![n.to.clone(), fact, term::boolean(said)],
                steps: vec![Proof::Symm(Box::new(n.proof.clone())), Proof::Hyp(name)],
            };
            let to = n.to.clone();
            if let Some(contradiction) =
                self.take_forms(&mut forms, &to, said, from, std::slice::from_ref(&n))
            {
                self.keep_needs(&n);
                absurd = Some(Proof::Absurd {
                    contradiction: Box::new(contradiction),
                    lhs: canon(lhs),
                    rhs: canon(rhs),
                });
                break;
            }
        }

        let cuts: Vec<Evaluated> = forms
            .into_iter()
            .filter(|f| f.proof.is_some() && self.in_scope(f))
            .collect();
        let body = match absurd {
            Some(proof) => proof,
            None => {
                // The claim is evaluated again only where a new form can
                // change what it stopped at: the form stands there, or it is
                // an equality whose left side does.
                let meets = |f: &Evaluated| {
                    let reads = match &f.fact.node {
                        ResolvedExpr::BinOp(BinOp::Eq, a, _) if f.said => Some(canon(a)),
                        _ => None,
                    };
                    stopped.iter().any(|t| {
                        stands_in(&f.fact, t) || reads.as_ref().is_some_and(|a| stands_in(a, t))
                    })
                };
                if !cuts.iter().any(meets) || self.read_claim_fuel == 0 {
                    return None;
                }
                // The claim again, under the forms; a failure leaves the
                // attempt as it found it.
                let base = self.hyps.len();
                let mark = self.mark();
                for f in &cuts {
                    self.hyps.push((
                        f.name.clone(),
                        Eqn::new(f.fact.clone(), term::boolean(f.said)),
                    ));
                }
                let cap = self.fuel.min(self.read_claim_fuel);
                self.fuel = cap;
                self.reading += 1;
                let body = self.prove_by_evaluation(lhs, rhs, depth.saturating_sub(1));
                self.reading -= 1;
                self.hyps.truncate(base);
                let spent = cap - self.fuel;
                // An evaluation inside this one has paid its part already.
                self.read_claim_fuel = self.read_claim_fuel.saturating_sub(spent);
                match body {
                    Ok(proof) => {
                        self.fuel = mark.fuel - spent;
                        proof
                    }
                    Err(_) => {
                        self.rewind(mark);
                        return None;
                    }
                }
            }
        };
        // Each form the proof reads, cut in: the later ones may read the
        // earlier, so the earlier go outermost.
        let mut proof = body;
        for f in cuts.into_iter().rev() {
            if !super::uses_hyp(&proof, &f.name) {
                continue;
            }
            for n in &f.needs {
                self.keep_needs(n);
            }
            let from = f.proof.expect("a cut has its proof");
            proof = if f.said {
                Proof::Have {
                    name: f.name,
                    fact: f.fact,
                    proof: Box::new(from),
                    body: Box::new(proof),
                }
            } else {
                // Where the form is true, `true = form = false`.
                let contradiction = Proof::Trans {
                    terms: vec![term::boolean(true), f.fact.clone(), term::boolean(false)],
                    steps: vec![Proof::Symm(Box::new(Proof::Hyp(f.name.clone()))), from],
                };
                Proof::Cases {
                    on: f.fact,
                    hyp: f.name,
                    if_true: Box::new(Proof::Absurd {
                        contradiction: Box::new(contradiction),
                        lhs: canon(lhs),
                        rhs: canon(rhs),
                    }),
                    if_false: Box::new(proof),
                }
            };
        }
        Some(proof)
    }
}

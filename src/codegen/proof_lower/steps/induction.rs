//! Induction along the recursion of the function a law is about.
//!
//! `verify f law …` names `f`; when `f` passes the termination gate, the
//! claim applies it to a given at the place it recurses on, and evaluation
//! alone does not close the law, the producer writes one case per arm of
//! `f` with one hypothesis per recursive call (see
//! [`crate::ir::proof_steps::induct`]) and closes each case by evaluation,
//! where those hypotheses and the cited laws rewrite what evaluation stops
//! at. It never picks another function to follow: when the law's own
//! function does not recurse, it says so and names the ones that do.

use crate::ir::hir::{ResolvedCallee, ResolvedExpr};
use crate::ir::identity::FnId;
use crate::ir::proof_steps::induct;
use crate::ir::proof_steps::term::{self, Term, canon};
use crate::ir::proof_steps::{Eqn, IhAt, InductCase, Obligation, Proof};

use super::env::Env;

/// Every call of `f` in `t`, outermost first.
fn calls_of(t: &Term, f: FnId, out: &mut Vec<Vec<Term>>) {
    if let ResolvedExpr::Call(ResolvedCallee::Fn(g), args) = &t.node
        && *g == f
    {
        out.push(args.iter().map(canon).collect());
    }
    for c in term::children(t) {
        calls_of(c, f, out);
    }
}

/// Names of the recursive functions the claim calls, for a refusal.
fn recursive_in_claim(env: &mut Env, ob: &Obligation) -> Vec<String> {
    use crate::ir::proof_steps::sexpr::Names;
    let mut ids: Vec<FnId> = Vec::new();
    fn collect(t: &Term, out: &mut Vec<FnId>) {
        if let ResolvedExpr::Call(ResolvedCallee::Fn(g), _) = &t.node
            && !out.contains(g)
        {
            out.push(*g);
        }
        for c in term::children(t) {
            collect(c, out);
        }
    }
    collect(&ob.lhs, &mut ids);
    collect(&ob.rhs, &mut ids);
    let recursive: Vec<FnId> = ids
        .into_iter()
        .filter(|id| {
            env.def(*id)
                .is_some_and(|d| matches!(induct::recursion(&d), Ok(Some(_))))
        })
        .collect();
    recursive
        .into_iter()
        .map(|id| env.inputs.symbol_table.fn_name(id))
        .collect()
}

fn fresh(base: &str, taken: &[String]) -> String {
    if !taken.iter().any(|t| t == base) {
        return base.to_string();
    }
    (1..)
        .map(|n| format!("{base}{n}"))
        .find(|c| !taken.contains(c))
        .expect("an unused name")
}

impl Env<'_> {
    /// Prove `ob` by induction along `f`, the function the law is about.
    pub(crate) fn prove_by_induction(
        &mut self,
        f: FnId,
        ob: &Obligation,
        depth: usize,
    ) -> Result<Proof, String> {
        use crate::ir::proof_steps::sexpr::Names;
        let f_name = self.inputs.symbol_table.fn_name(f);
        let def = self.def(f);
        let rec = match def.as_ref().map(induct::recursion) {
            Some(Ok(Some(rec))) => rec,
            Some(Err(why)) => return Err(format!("induction: {why}")),
            _ => {
                let others = recursive_in_claim(self, ob);
                return Err(if others.is_empty() {
                    format!("induction: the law is about {f_name}, which does not recurse")
                } else {
                    format!(
                        "induction follows the recursion of the function the law is about; {f_name} does not recurse but {} does: state the law about it or name it in `because`",
                        others.join(", ")
                    )
                });
            }
        };
        let def = def.expect("matched above");
        let j = rec.at;
        let mut calls = Vec::new();
        calls_of(&ob.lhs, f, &mut calls);
        calls_of(&ob.rhs, f, &mut calls);
        // The calls whose argument at the recursive place is a given: they
        // must all name the same one, or which to follow would be a guess.
        let candidates: Vec<&Vec<Term>> = calls
            .iter()
            .filter(|c| induct::varied(c, j, &ob.givens).is_ok())
            .collect();
        let Some(first) = candidates.first() else {
            return Err(format!(
                "induction: the claim never passes a given where {f_name} recurses"
            ));
        };
        let (v, _) = induct::varied(first, j, &ob.givens)?;
        if let Some(other) = candidates
            .iter()
            .map(|c| induct::varied(c, j, &ob.givens).expect("filtered").0)
            .find(|o| *o != v)
        {
            return Err(format!(
                "induction: {f_name} recurses on {v} in one call and on {other} in another; which to follow is a choice the law has to make"
            ));
        }
        // Along a count toward zero the given is an Int; each case holds the
        // comparison's value, and a recursive call's hypothesis is the claim
        // at the Int it passes, closer to zero.
        if rec.guard.is_some() && !ob.ints.iter().any(|g| *g == v) {
            return Err(format!(
                "induction along {f_name}: {v} is not a given of type Int"
            ));
        }
        // The scheme follows the first of them, outermost first on the left.
        let args = (*first).clone();
        let (_, general) = induct::varied(&args, j, &ob.givens)?;
        let varying: Vec<&String> = std::iter::once(&v)
            .chain(general.iter().map(|(_, g)| g))
            .collect();
        let mut taken = Vec::new();
        term::free_vars(&ob.lhs, &mut taken);
        term::free_vars(&ob.rhs, &mut taken);
        let mentions = |e: &Eqn| {
            let mut fv = Vec::new();
            term::free_vars(&e.lhs, &mut fv);
            term::free_vars(&e.rhs, &mut fv);
            fv.iter().any(|n| varying.contains(&n))
        };
        // The hypotheses that mention what the induction varies are carried:
        // each holds at a case's pattern, and a recursive call's hypothesis
        // is taken where they are proved at the call's arguments. Each name
        // once, as it stands innermost.
        let mut carried: Vec<(String, Eqn)> = Vec::new();
        for (i, (name, e)) in self.hyps.iter().enumerate() {
            term::free_vars(&e.lhs, &mut taken);
            term::free_vars(&e.rhs, &mut taken);
            let shadowed = self.hyps[i + 1..].iter().any(|(n, _)| n == name);
            if !shadowed && mentions(e) {
                carried.push((name.clone(), e.clone()));
            }
        }
        let kept: Vec<(String, Eqn)> = self
            .hyps
            .iter()
            .filter(|(_, e)| !mentions(e))
            .cloned()
            .collect();
        taken.extend(ob.givens.iter().cloned());
        let ResolvedExpr::Match { arms, .. } = &def.body.node else {
            return Err("induction: the body is not a match".into());
        };
        self.mark_used(f);
        let saved = std::mem::take(&mut self.hyps);
        let result = (|| -> Result<Vec<InductCase>, String> {
            let mut cases = Vec::new();
            for (i, arm) in arms.iter().enumerate() {
                let in_case = |m: String| format!("induction along {f_name}, case {}: {m}", i + 1);
                let mut binders = Vec::new();
                if rec.guard.is_some() {
                    binders.push(self.fresh_hyp());
                } else {
                    for name in term::pattern_binders(&arm.pattern) {
                        let b = fresh(&name, &taken);
                        taken.push(b.clone());
                        binders.push(b);
                    }
                }
                let n = induct::self_calls(&arm.body, f).len();
                let names: Vec<String> = (0..n)
                    .map(|_| {
                        self.next_ih += 1;
                        format!("ih{}", self.next_ih)
                    })
                    .collect();
                let case = induct::case(
                    &def, &rec, &args, &v, &general, arm, &binders, &names, &ob.lhs, &ob.rhs,
                )
                .map_err(in_case)?;
                let here = [(v.clone(), case.value.clone())];
                self.hyps = kept.clone();
                self.hyps.extend(case.guard.clone());
                for (name, e) in &carried {
                    let at = Eqn::new(term::subst(&e.lhs, &here)?, term::subst(&e.rhs, &here)?);
                    self.hyps.push((name.clone(), at));
                }
                // A call's hypothesis is taken where every carried one is
                // proved at its arguments; the case may do without it.
                let mut ihs = Vec::new();
                let mut carry = Vec::new();
                let mut taken_ihs = Vec::new();
                for ((ih, e), tau) in case.ihs.iter().zip(&case.at) {
                    let mut proofs = Vec::new();
                    for (_, c) in &carried {
                        let at = (term::subst(&c.lhs, tau)?, term::subst(&c.rhs, tau)?);
                        match self.prove_by_evaluation(&at.0, &at.1, depth) {
                            Ok(p) => proofs.push(p),
                            Err(_) => break,
                        }
                    }
                    if proofs.len() == carried.len() {
                        ihs.push(ih.clone());
                        carry.push(proofs);
                        taken_ihs.push((ih.clone(), e.clone()));
                    } else {
                        ihs.push("_".to_string());
                        carry.push(Vec::new());
                    }
                }
                self.hyps.extend(taken_ihs);
                let scope = self.hyps.clone();
                // Where the case stops at a call of `f` on the part a
                // recursive call recurses on, with other values of the
                // varied givens, the claim there is a hypothesis too: the
                // induction holds it for every value of them.
                let mut more: Vec<(usize, IhAt)> = Vec::new();
                let mut stated: Vec<(String, Eqn)> = Vec::new();
                let mut rounds = 0;
                let proof = loop {
                    self.hyps = scope.clone();
                    self.hyps.extend(stated.iter().cloned());
                    let stop = match self.prove_by_evaluation(&case.goal.lhs, &case.goal.rhs, depth)
                    {
                        Ok(p) => break p,
                        Err(m) => m,
                    };
                    rounds += 1;
                    if general.is_empty() || rounds > 3 {
                        return Err(in_case(stop));
                    }
                    let mut found = Vec::new();
                    for side in [&case.goal.lhs, &case.goal.rhs] {
                        if let Ok(chain) = self.normalize(side, 8) {
                            calls_of(chain.cur(), f, &mut found);
                        }
                    }
                    let before = more.len();
                    for call in found {
                        let Some(k) = case
                            .at
                            .iter()
                            .position(|tau| canon(&tau[0].1) == canon(&call[j]))
                        else {
                            continue;
                        };
                        let at: Vec<Term> = general.iter().map(|(p, _)| canon(&call[*p])).collect();
                        let own: Vec<Term> =
                            case.at[k][1..].iter().map(|(_, t)| canon(t)).collect();
                        if at == own || more.iter().any(|(m, ih)| *m == k && ih.at == at) {
                            continue;
                        }
                        let mut inst = vec![(v.clone(), case.at[k][0].1.clone())];
                        inst.extend(
                            general
                                .iter()
                                .map(|(_, g)| g.clone())
                                .zip(at.iter().cloned()),
                        );
                        let e =
                            Eqn::new(term::subst(&ob.lhs, &inst)?, term::subst(&ob.rhs, &inst)?);
                        self.hyps = scope.clone();
                        if self.rewrites_forever(&e) {
                            continue;
                        }
                        let mut proofs = Vec::new();
                        for (_, c) in &carried {
                            let at = (term::subst(&c.lhs, &inst)?, term::subst(&c.rhs, &inst)?);
                            match self.prove_by_evaluation(&at.0, &at.1, depth) {
                                Ok(p) => proofs.push(p),
                                Err(_) => break,
                            }
                        }
                        if proofs.len() != carried.len() {
                            continue;
                        }
                        self.next_ih += 1;
                        let name = format!("ih{}", self.next_ih);
                        stated.push((name.clone(), e));
                        more.push((
                            k,
                            IhAt {
                                name,
                                at,
                                carry: proofs,
                            },
                        ));
                    }
                    if more.len() == before {
                        return Err(in_case(stop));
                    }
                };
                cases.push(InductCase {
                    binders,
                    ihs,
                    carry,
                    more,
                    proof,
                });
            }
            Ok(cases)
        })();
        self.hyps = saved;
        let cases = result?;
        Ok(Proof::Induct {
            fn_id: f,
            args,
            lhs: canon(&ob.lhs),
            rhs: canon(&ob.rhs),
            carried: carried.into_iter().map(|(n, _)| n).collect(),
            cases,
        })
    }
}

/// Whether `part` occurs in `t`.
fn holds_term(t: &Term, part: &Term) -> bool {
    canon(t) == *part || term::children(t).into_iter().any(|c| holds_term(c, part))
}

impl Env<'_> {
    /// Whether the hypothesis `e`, read left to right, would rewrite
    /// forever: its right side holds its left one, as written or once
    /// evaluated under the hypotheses in scope.
    fn rewrites_forever(&mut self, e: &Eqn) -> bool {
        let lhs = canon(&e.lhs);
        if holds_term(&e.rhs, &lhs) {
            return true;
        }
        let fuel = self.fuel;
        let looped = match self.normalize(&e.rhs, 8) {
            Ok(chain) => holds_term(chain.cur(), &lhs),
            Err(_) => true,
        };
        self.fuel = fuel;
        looped
    }
}

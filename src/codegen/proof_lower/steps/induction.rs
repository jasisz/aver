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
use crate::ir::proof_steps::{Eqn, InductCase, Obligation, Proof};

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
                .is_some_and(|d| matches!(induct::structural_param(&d), Ok(Some(_))))
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
        let j = match def.as_ref().map(induct::structural_param) {
            Some(Ok(Some(j))) => j,
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
        // A countdown recurses on an Int given: induction down to zero on
        // that given, the hypotheses about it carried along.
        if induct::countdown(&def).is_some() {
            self.mark_used(f);
            return self.prove_by_int_induction(&f_name, &v, ob, depth);
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
                for name in term::pattern_binders(&arm.pattern) {
                    let b = fresh(&name, &taken);
                    taken.push(b.clone());
                    binders.push(b);
                }
                let n = induct::self_calls(&arm.body, f).len();
                let names: Vec<String> = (0..n)
                    .map(|_| {
                        self.next_ih += 1;
                        format!("ih{}", self.next_ih)
                    })
                    .collect();
                let case = induct::case(
                    &def, &args, j, &v, &general, arm, &binders, &names, &ob.lhs, &ob.rhs,
                )
                .map_err(in_case)?;
                let here = [(v.clone(), case.value.clone())];
                self.hyps = kept.clone();
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
                let proof = self
                    .prove_by_evaluation(&case.goal.lhs, &case.goal.rhs, depth)
                    .map_err(in_case)?;
                cases.push(InductCase {
                    binders,
                    ihs,
                    carry,
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

    /// Prove `ob` by induction on the Int given `v` down to zero, as the
    /// countdown `f_name` recurses: the case `v <= 0`, then the case
    /// `v > 0` with the claim at `v - 1`. Every hypothesis that mentions
    /// `v` is carried: it stays in scope, and in the step it is first
    /// proved at `v - 1` by evaluation, so the claim there holds.
    fn prove_by_int_induction(
        &mut self,
        f_name: &str,
        v: &str,
        ob: &Obligation,
        depth: usize,
    ) -> Result<Proof, String> {
        use crate::ast::BinOp;
        if !ob.ints.iter().any(|g| g == v) {
            return Err(format!(
                "induction along {f_name}: {v} is not a given of type Int"
            ));
        }
        let mentions = |e: &crate::ir::proof_steps::Eqn| {
            let mut fv = Vec::new();
            term::free_vars(&e.lhs, &mut fv);
            term::free_vars(&e.rhs, &mut fv);
            fv.iter().any(|n| n == v)
        };
        // Each name once, as it stands innermost, in scope order.
        let mut carried: Vec<(String, crate::ir::proof_steps::Eqn)> = Vec::new();
        for (i, (name, e)) in self.hyps.iter().enumerate() {
            let shadowed = self.hyps[i + 1..].iter().any(|(n, _)| n == name);
            if !shadowed && mentions(e) {
                carried.push((name.clone(), e.clone()));
            }
        }
        let kept: Vec<(String, crate::ir::proof_steps::Eqn)> = self
            .hyps
            .iter()
            .filter(|(_, e)| !mentions(e))
            .cloned()
            .collect();
        let guard = self.fresh_hyp();
        self.next_ih += 1;
        let ih = format!("ih{}", self.next_ih);
        let at = term::binop(BinOp::Lte, term::var(v), term::int(&0.into()));
        let down = [(
            v.to_string(),
            term::binop(BinOp::Sub, term::var(v), term::int(&1.into())),
        )];
        let saved = std::mem::take(&mut self.hyps);
        let scope = |value: bool| {
            let mut h = kept.clone();
            h.push((
                guard.clone(),
                crate::ir::proof_steps::Eqn::new(at.clone(), term::boolean(value)),
            ));
            h.extend(carried.iter().cloned());
            h
        };
        let in_case = |env: &mut Self,
                       value: bool,
                       extra: Option<(String, crate::ir::proof_steps::Eqn)>,
                       lhs: &Term,
                       rhs: &Term| {
            env.hyps = scope(value);
            env.hyps.extend(extra);
            env.prove_by_evaluation(lhs, rhs, depth)
        };
        let result = (|| -> Result<Proof, String> {
            let base = in_case(self, true, None, &ob.lhs, &ob.rhs)
                .map_err(|m| format!("induction on {v} along {f_name}, case {v} <= 0: {m}"))?;
            let mut proved = Vec::new();
            for (name, e) in &carried {
                let at_less = (term::subst(&e.lhs, &down)?, term::subst(&e.rhs, &down)?);
                let p = in_case(self, false, None, &at_less.0, &at_less.1).map_err(|m| {
                    format!("induction on {v} along {f_name}: `{name}` at {v} - 1: {m}")
                })?;
                proved.push((name.clone(), p));
            }
            let at_ih = crate::ir::proof_steps::Eqn::new(
                term::subst(&ob.lhs, &down)?,
                term::subst(&ob.rhs, &down)?,
            );
            let step = in_case(self, false, Some((ih.clone(), at_ih)), &ob.lhs, &ob.rhs)
                .map_err(|m| format!("induction on {v} along {f_name}, case {v} > 0: {m}"))?;
            Ok(Proof::InductInt {
                var: v.to_string(),
                lhs: canon(&ob.lhs),
                rhs: canon(&ob.rhs),
                guard: guard.clone(),
                base: Box::new(base),
                carried: proved,
                ih: ih.clone(),
                step: Box::new(step),
            })
        })();
        self.hyps = saved;
        result
    }
}

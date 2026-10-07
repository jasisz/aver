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
        // A definition that divides an Int down to zero is followed by an
        // induction on that Int, dividing it the same way.
        let halving = def.as_ref().and_then(induct::halving_divisor);
        let j = match def.as_ref().map(induct::structural_param) {
            Some(Ok(Some(j))) => j,
            _ if halving.is_some() => halving.as_ref().expect("matched").0,
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
        let divisor = halving.map(|(_, k)| k);
        if induct::countdown(&def).is_some() || divisor.is_some() {
            self.mark_used(f);
            let fuel = self.fuel;
            let fixed = self.prove_by_int_induction(&f_name, &v, divisor.as_ref(), ob, depth);
            if fixed.is_ok() {
                return fixed;
            }
            // The claim for every value of the givens the recursive call
            // changes, as the function's own recursion passes them on.
            let general = changed_givens(&def, first, j, &ob.givens)?;
            if general.is_empty() {
                return fixed;
            }
            self.fuel = fuel;
            return self.prove_by_general_int_induction(
                f,
                &f_name,
                &def,
                first,
                j,
                &v,
                divisor.as_ref(),
                &general,
                ob,
                depth,
            );
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

    /// Prove `ob` by induction on the Int given `v` down to zero, as the
    /// countdown `f_name` recurses: the case `v <= 0`, then the case
    /// `v > 0` with the claim at `v - 1`. Every hypothesis that mentions
    /// `v` is carried: it stays in scope, and in the step it is first
    /// proved at `v - 1` by evaluation, so the claim there holds.
    fn prove_by_int_induction(
        &mut self,
        f_name: &str,
        v: &str,
        divisor: Option<&num_bigint::BigInt>,
        ob: &Obligation,
        depth: usize,
    ) -> Result<Proof, String> {
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
        let (at, at_base, smaller) = induct::int_descent(v, divisor);
        let names = case_names(v, divisor);
        let down = [(v.to_string(), smaller)];
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
            let base = in_case(self, at_base, None, &ob.lhs, &ob.rhs)
                .map_err(|m| format!("induction on {v} along {f_name}, case {}: {m}", names.0))?;
            let mut proved = Vec::new();
            for (name, e) in &carried {
                let at_less = (term::subst(&e.lhs, &down)?, term::subst(&e.rhs, &down)?);
                let p = in_case(self, !at_base, None, &at_less.0, &at_less.1).map_err(|m| {
                    format!(
                        "induction on {v} along {f_name}: `{name}` at {}: {m}",
                        names.2
                    )
                })?;
                proved.push((name.clone(), p));
            }
            let at_ih = crate::ir::proof_steps::Eqn::new(
                term::subst(&ob.lhs, &down)?,
                term::subst(&ob.rhs, &down)?,
            );
            let step = in_case(self, !at_base, Some((ih.clone(), at_ih)), &ob.lhs, &ob.rhs)
                .map_err(|m| format!("induction on {v} along {f_name}, case {}: {m}", names.1))?;
            Ok(Proof::InductInt {
                var: v.to_string(),
                divisor: divisor.cloned(),
                lhs: canon(&ob.lhs),
                rhs: canon(&ob.rhs),
                guard: guard.clone(),
                base: Box::new(base),
                carried: proved.iter().map(|(n, _)| n.clone()).collect(),
                general: Vec::new(),
                ihs: vec![IhAt {
                    name: ih.clone(),
                    at: Vec::new(),
                    carry: proved.into_iter().map(|(_, p)| p).collect(),
                }],
                step: Box::new(step),
            })
        })();
        self.hyps = saved;
        result
    }
}

/// How the cases of an Int induction read in a refusal: the base case,
/// the step case, and the smaller value (`n <= 0`, `n > 0`, `n - 1`).
fn case_names(v: &str, divisor: Option<&num_bigint::BigInt>) -> (String, String, String) {
    let smaller = match divisor {
        None => format!("{v} - 1"),
        Some(k) => format!("{v} / {k}"),
    };
    (format!("{v} <= 0"), format!("{v} > 0"), smaller)
}

/// The givens the claim passes to the countdown `def` at `args` at a place
/// other than `j` whose parameter a recursive call changes: what an
/// induction along it holds for every value of, with each place.
fn changed_givens(
    def: &crate::ir::proof_steps::Def,
    args: &[Term],
    j: usize,
    givens: &[String],
) -> Result<Vec<(usize, String)>, String> {
    use crate::ir::hir::ResolvedExpr;
    let (_, general) = induct::varied(args, j, givens)?;
    let calls = induct::self_calls(&def.body, def.fn_id);
    Ok(general
        .into_iter()
        .filter(|(k, _)| {
            calls.iter().any(|(call, _)| {
                !matches!(&call[*k].node, ResolvedExpr::Ident(n) if *n == def.params[*k])
            })
        })
        .collect())
}

impl Env<'_> {
    /// Prove `ob` by induction on the Int given `v` down to zero along the
    /// countdown `f`, for every value of the givens in `general`: in the
    /// step, the claim at `v - 1` holds wherever the givens in `general`
    /// take the values of `f`'s own recursive call, and at the values of
    /// any call of `f` at `v - 1` the two sides evaluate to. Every
    /// hypothesis that mentions `v` or a generalised given is carried and
    /// proved at each of those values.
    #[allow(clippy::too_many_arguments)]
    fn prove_by_general_int_induction(
        &mut self,
        f: FnId,
        f_name: &str,
        def: &crate::ir::proof_steps::Def,
        args: &[Term],
        j: usize,
        v: &str,
        divisor: Option<&num_bigint::BigInt>,
        general: &[(usize, String)],
        ob: &Obligation,
        depth: usize,
    ) -> Result<Proof, String> {
        let names: Vec<String> = general.iter().map(|(_, g)| g.clone()).collect();
        let mentions = |e: &Eqn| {
            let mut fv = Vec::new();
            term::free_vars(&e.lhs, &mut fv);
            term::free_vars(&e.rhs, &mut fv);
            fv.iter().any(|n| n == v || names.contains(n))
        };
        let mut carried: Vec<(String, Eqn)> = Vec::new();
        for (i, (name, e)) in self.hyps.iter().enumerate() {
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
        let guard = self.fresh_hyp();
        let (at, at_base, less) = induct::int_descent(v, divisor);
        let shown = case_names(v, divisor);
        let scope = |value: bool| {
            let mut h = kept.clone();
            h.push((guard.clone(), Eqn::new(at.clone(), term::boolean(value))));
            h.extend(carried.iter().cloned());
            h
        };
        let instance = |values: &[Term]| -> Result<Vec<(String, Term)>, String> {
            let mut down = vec![(v.to_string(), less.clone())];
            down.extend(names.iter().cloned().zip(values.iter().cloned()));
            Ok(down)
        };
        let claim_at = |values: &[Term]| -> Result<Eqn, String> {
            let down = instance(values)?;
            Ok(Eqn::new(
                term::subst(&ob.lhs, &down)?,
                term::subst(&ob.rhs, &down)?,
            ))
        };
        // The values the function's own recursive calls pass.
        let outer = def.outer(args)?;
        let mut values: Vec<Vec<Term>> = Vec::new();
        for (call, _) in induct::self_calls(&def.body, f) {
            let at: Vec<Term> = general
                .iter()
                .map(|(k, _)| term::subst(&call[*k], &outer).map(|t| canon(&t)))
                .collect::<Result<_, _>>()?;
            if !values.contains(&at) {
                values.push(at);
            }
        }
        let saved = std::mem::take(&mut self.hyps);
        let result = (|| -> Result<Proof, String> {
            self.hyps = scope(at_base);
            let base = self
                .prove_by_evaluation(&ob.lhs, &ob.rhs, depth)
                .map_err(|m| format!("induction on {v} along {f_name}, case {}: {m}", shown.0))?;
            let mut last = String::new();
            // Each round adds the values of the calls at `v - 1` the sides
            // evaluate to under the hypotheses so far; a few rounds suffice
            // for a claim that applies the function on each side.
            for _ in 0..3 {
                let mut ihs = Vec::new();
                let mut stated = Vec::new();
                for at in &values {
                    let down = instance(at)?;
                    self.hyps = scope(!at_base);
                    let mut carry = Vec::new();
                    for (_, c) in &carried {
                        let lhs = term::subst(&c.lhs, &down)?;
                        let rhs = term::subst(&c.rhs, &down)?;
                        match self.prove_by_evaluation(&lhs, &rhs, depth) {
                            Ok(p) => carry.push(p),
                            Err(_) => break,
                        }
                    }
                    if carry.len() != carried.len() {
                        continue;
                    }
                    self.next_ih += 1;
                    let name = format!("ih{}", self.next_ih);
                    stated.push((name.clone(), claim_at(at)?));
                    ihs.push(IhAt {
                        name,
                        at: at.clone(),
                        carry,
                    });
                }
                self.hyps = scope(!at_base);
                self.hyps.extend(stated);
                match self.prove_by_evaluation(&ob.lhs, &ob.rhs, depth) {
                    Ok(step) => {
                        return Ok(Proof::InductInt {
                            var: v.to_string(),
                            divisor: divisor.cloned(),
                            lhs: canon(&ob.lhs),
                            rhs: canon(&ob.rhs),
                            guard: guard.clone(),
                            base: Box::new(base),
                            carried: carried.iter().map(|(n, _)| n.clone()).collect(),
                            general: names.clone(),
                            ihs,
                            step: Box::new(step),
                        });
                    }
                    Err(m) => last = m,
                }
                // The calls of `f` at `v - 1` the sides stop at.
                let mut found = Vec::new();
                for side in [&ob.lhs, &ob.rhs] {
                    if let Ok(chain) = self.normalize(side, 8) {
                        calls_of(chain.cur(), f, &mut found);
                    }
                }
                let before = values.len();
                for call in found {
                    if canon(&call[j]) != canon(&less) {
                        continue;
                    }
                    let at: Vec<Term> = general.iter().map(|(k, _)| canon(&call[*k])).collect();
                    let e = claim_at(&at)?;
                    self.hyps = scope(!at_base);
                    if values.contains(&at) || self.rewrites_forever(&e) {
                        continue;
                    }
                    values.push(at);
                }
                if values.len() == before {
                    break;
                }
            }
            Err(format!(
                "induction on {v} along {f_name}, for every {}, case {}: {last}",
                names.join(", "),
                shown.1
            ))
        })();
        self.hyps = saved;
        result
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

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
use crate::ir::proof_steps::{InductCase, Obligation, Proof};

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
        // The scheme follows the first of them, outermost first on the left.
        let args = (*first).clone();
        let (_, general) = induct::varied(&args, j, &ob.givens)?;
        let varying: Vec<&String> = std::iter::once(&v)
            .chain(general.iter().map(|(_, g)| g))
            .collect();
        let mut taken = Vec::new();
        term::free_vars(&ob.lhs, &mut taken);
        term::free_vars(&ob.rhs, &mut taken);
        for (_, e) in &self.hyps {
            let mut fv = Vec::new();
            term::free_vars(&e.lhs, &mut fv);
            term::free_vars(&e.rhs, &mut fv);
            if fv.iter().any(|n| varying.contains(&n)) {
                return Err(format!(
                    "induction along {f_name}: the `when` mentions a given the induction varies, which steps do not induct under yet"
                ));
            }
            taken.extend(fv);
        }
        taken.extend(ob.givens.iter().cloned());
        let ResolvedExpr::Match { arms, .. } = &def.body.node else {
            return Err("induction: the body is not a match".into());
        };
        self.mark_used(f);
        let mut cases = Vec::new();
        for (i, arm) in arms.iter().enumerate() {
            let mut binders = Vec::new();
            for name in term::pattern_binders(&arm.pattern) {
                let b = fresh(&name, &taken);
                taken.push(b.clone());
                binders.push(b);
            }
            let n = induct::self_calls(&arm.body, f).len();
            let ihs: Vec<String> = (0..n)
                .map(|_| {
                    self.next_ih += 1;
                    format!("ih{}", self.next_ih)
                })
                .collect();
            let (goal, hyps) = induct::case(
                &def, &args, j, &v, &general, arm, &binders, &ihs, &ob.lhs, &ob.rhs,
            )
            .map_err(|m| format!("induction along {f_name}, case {}: {m}", i + 1))?;
            let saved = self.hyps.len();
            self.hyps.extend(hyps);
            let proof = self.prove_by_evaluation(&goal.lhs, &goal.rhs, depth);
            self.hyps.truncate(saved);
            let proof =
                proof.map_err(|m| format!("induction along {f_name}, case {}: {m}", i + 1))?;
            cases.push(InductCase {
                binders,
                ihs,
                proof,
            });
        }
        Ok(Proof::Induct {
            fn_id: f,
            args,
            lhs: canon(&ob.lhs),
            rhs: canon(&ob.rhs),
            cases,
        })
    }
}

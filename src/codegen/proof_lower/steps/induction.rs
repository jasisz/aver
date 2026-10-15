//! Induction along the recursion of the function a law is about.
//!
//! `verify f law …` names `f`; when `f` passes the termination gate, the
//! claim applies it to a given at the place it recurses on, and evaluation
//! alone does not close the law, the producer writes one case per arm of
//! `f` with one hypothesis per recursive call (see
//! [`crate::ir::proof_steps::induct`]) and closes each case by evaluation,
//! where those hypotheses and the cited laws rewrite what evaluation stops
//! at. Only once that has not closed does it follow another function the
//! claim passes a given where it recurses, at most two of them, and it
//! hints the `induction` line that goes straight to the one that closed.

use crate::ast::Type;
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
            let ds = env.reachable_defs(*id);
            env.def(*id)
                .is_some_and(|d| matches!(induct::recursion(&d, &ds), Ok(Some(_))))
        })
        .collect();
    recursive
        .into_iter()
        .map(|id| env.inputs.symbol_table.fn_name(id))
        .collect()
}

/// Most functions other than the law's own an induction follows, after
/// the one the law's own leads to has not closed
/// ([`Env::prove_by_induction`]).
const MAX_OTHER_FUNCTIONS: usize = 2;

/// Most sets of values of the generalised givens an induction on a list
/// states the claim at the tail at ([`Env::claims_at_tail`]).
const MAX_GENERAL_VALUES: usize = 3;

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
    /// Prove `ob` by induction: along `f`, the function the law is about,
    /// or, on a given of list type that `f` does not recurse on, on the
    /// list itself. `named` is the given the law's `induction` line names:
    /// along `f` only the calls that pass it where `f` recurses are
    /// followed, and a list given no such call passes is inducted on as a
    /// list. Without a line, induction along `f` comes first; when it does
    /// not close, each list given `f` does not recurse on, and some
    /// recursion walks ([`Self::walked_by_recursion`]), is tried in turn,
    /// and the one that closes is hinted as the line to add.
    pub(crate) fn prove_by_induction(
        &mut self,
        f: FnId,
        named: Option<&str>,
        ob: &Obligation,
        depth: usize,
    ) -> Result<Proof, String> {
        self.prove_by_induction_of(f, named, ob, depth)
    }

    /// Induction along each other function the claim passes a given (the
    /// named one, under a line) where it recurses
    /// ([`Self::others_to_follow`]): the last attempt on a law about `f`,
    /// once every other has not closed it. One that closes is hinted as the
    /// `induction` line that goes straight to it.
    pub(crate) fn prove_along_others(
        &mut self,
        f: FnId,
        named: Option<&str>,
        ob: &Obligation,
        depth: usize,
    ) -> Result<Proof, String> {
        let (hyps, open_premises, fuel) =
            (self.hyps.clone(), self.open_premises.clone(), self.fuel);
        let before = self.hints.len();
        let mut tried = Vec::new();
        for g in self.others_to_follow(f, named, ob) {
            match self.induction_along(g, named, ob, depth) {
                Ok(proof) => {
                    self.hints.truncate(before);
                    self.open_premises = open_premises;
                    if named.is_none()
                        && let Proof::Induct { args, .. } = &proof
                        && let Some(v) = self.varied_given(g, args, ob)
                        && !self.recurses_on(f, &v, ob)
                    {
                        use crate::ir::proof_steps::sexpr::Names;
                        let g_name = self.inputs.symbol_table.fn_name(g);
                        self.advice.push(format!(
                            "steps closed it by following the recursion of {g_name}; add `induction {v}` to go straight to it"
                        ));
                    }
                    return Ok(proof);
                }
                Err(m) => tried.push(m),
            }
            self.hyps = hyps.clone();
            self.fuel = fuel;
        }
        self.open_premises = open_premises;
        Err(tried.join("; "))
    }

    /// The recursive functions other than `f` the claim passes a given
    /// where they recurse (the given `named` names, if any), in the order
    /// they stand, at most [`MAX_OTHER_FUNCTIONS`]: once the induction
    /// `f` leads to has not closed, an induction along one of them may.
    fn others_to_follow(&mut self, f: FnId, named: Option<&str>, ob: &Obligation) -> Vec<FnId> {
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
        ids.into_iter()
            .filter(|g| *g != f)
            .filter(|g| match named {
                Some(n) => self.recurses_on(*g, n, ob),
                None => ob.givens.iter().any(|n| self.recurses_on(*g, n, ob)),
            })
            .take(MAX_OTHER_FUNCTIONS)
            .collect()
    }

    /// The given an induction along `g` at `args` follows.
    fn varied_given(&mut self, g: FnId, args: &[Term], ob: &Obligation) -> Option<String> {
        let ds = self.reachable_defs(g);
        let Some(Ok(Some(rec))) = self.def(g).map(|d| induct::recursion(&d, &ds)) else {
            return None;
        };
        induct::varied(args, rec.at, &ob.givens)
            .ok()
            .map(|(v, _)| v)
    }

    /// Induction along `f` or on a list given (see
    /// [`Self::prove_by_induction`]).
    fn prove_by_induction_of(
        &mut self,
        f: FnId,
        named: Option<&str>,
        ob: &Obligation,
        depth: usize,
    ) -> Result<Proof, String> {
        if let Some(n) = named {
            if !self.recurses_on(f, n, ob) && ob.lists.iter().any(|l| l == n) {
                return self
                    .prove_by_list_induction(n, ob, depth)
                    .map_err(|why| format!("induction on {n}: {why}"));
            }
            return self.induction_along(f, named, ob, depth);
        }
        let along = self.induction_along(f, None, ob, depth);
        let Err(why) = along else {
            return along;
        };
        let lists: Vec<String> = ob
            .lists
            .iter()
            .filter(|v| !self.recurses_on(f, v, ob) && self.walked_by_recursion(v, ob))
            .cloned()
            .collect();
        let (hyps, fuel, before) = (self.hyps.clone(), self.fuel, self.hints.len());
        let mut tried = Vec::new();
        for v in lists {
            match self.prove_by_list_induction(&v, ob, depth) {
                Ok(proof) => {
                    self.hints.truncate(before);
                    self.advice.push(format!(
                        "steps closed it by trying each given in turn; add `induction {v}` to go straight to it"
                    ));
                    return Ok(proof);
                }
                Err(m) => tried.push(format!("induction on {v}: {m}")),
            }
            self.hyps = hyps.clone();
            self.fuel = fuel;
        }
        Err(std::iter::once(why)
            .chain(tried)
            .collect::<Vec<_>>()
            .join("; "))
    }

    /// Whether a call of `f` in the claim passes the given `v` where `f`
    /// recurses.
    fn recurses_on(&mut self, f: FnId, v: &str, ob: &Obligation) -> bool {
        let ds = self.reachable_defs(f);
        let Some(Ok(Some(rec))) = self.def(f).map(|d| induct::recursion(&d, &ds)) else {
            return false;
        };
        let mut calls = Vec::new();
        calls_of(&ob.lhs, f, &mut calls);
        calls_of(&ob.rhs, f, &mut calls);
        calls.iter().any(
            |c| matches!(c.get(rec.at).map(|a| &a.node), Some(ResolvedExpr::Ident(n)) if n == v),
        )
    }

    /// Whether a call of a function that recurses holds the given `v` in
    /// the argument it recurses on: in the claim as written, or behind one
    /// call of a function that does not recurse (a law is often about a
    /// name for its claim). Only such a call brings the claim at a cell of
    /// `v` down to the claim at its tail. With none, the hypothesis of an
    /// induction on `v` has nowhere to apply and each case is the claim
    /// evaluated over again, so steps induct on `v` only under the law's
    /// own `induction` line.
    fn walked_by_recursion(&self, v: &str, ob: &Obligation) -> bool {
        fn calls(t: &Term, out: &mut Vec<(FnId, Vec<Term>)>) {
            if let ResolvedExpr::Call(ResolvedCallee::Fn(g), args) = &t.node {
                out.push((*g, args.clone()));
            }
            for c in term::children(t) {
                calls(c, out);
            }
        }
        // The calls the claim writes, then those one opening shows.
        let mut at = Vec::new();
        calls(&ob.lhs, &mut at);
        calls(&ob.rhs, &mut at);
        for written in [true, false] {
            let mut behind = Vec::new();
            for (g, args) in &at {
                let Some(def) = self.raw_def(*g) else {
                    continue;
                };
                match induct::recursion(&def, &self.reachable_defs(*g)) {
                    Ok(Some(rec)) => {
                        let mut held = Vec::new();
                        if let Some(a) = args.get(rec.at) {
                            term::free_vars(a, &mut held);
                        }
                        if held.iter().any(|n| n == v) {
                            return true;
                        }
                    }
                    Ok(None) if written => {
                        if let Ok(outer) = def.outer(args)
                            && let Ok(body) = term::subst(&def.body, &outer)
                        {
                            calls(&body, &mut behind);
                        }
                    }
                    _ => {}
                }
            }
            at = behind;
        }
        false
    }

    /// Structural induction on the list given `v`, apart from any
    /// function's recursion: the claim at `[]`, and at a cell under the
    /// claim at its tail as a hypothesis, each case closed by evaluation.
    /// The hypotheses that mention `v` are carried: each holds at the
    /// case's value of `v`, and the cell case takes the hypothesis where
    /// evaluation proves every carried one at the tail. When the cell case
    /// does not close so, and evaluating it shows the claim at the tail
    /// with other values of other givens ([`Self::claims_at_tail`]), it is
    /// tried again with those givens generalised: the induction proves the
    /// claim for every value of them, so the cell case takes the claim at
    /// the tail at each value found, the hypotheses that mention them
    /// carried as well.
    fn prove_by_list_induction(
        &mut self,
        v: &str,
        ob: &Obligation,
        depth: usize,
    ) -> Result<Proof, String> {
        let plain = self.list_induction_over(v, &[], &[], ob, depth);
        let Err(why) = plain else {
            return plain;
        };
        let (general, at) = self.claims_at_tail(v, ob);
        if general.is_empty() {
            return Err(why);
        }
        let (hyps, fuel) = (self.hyps.clone(), self.fuel);
        match self.list_induction_over(v, &general, &at, ob, depth) {
            Ok(proof) => Ok(proof),
            Err(m) => {
                self.hyps = hyps;
                self.fuel = fuel;
                Err(format!("{why}; for every {}: {m}", general.join(", ")))
            }
        }
    }

    /// The claim at the tail of a cell of `v` with other values of other
    /// givens, where evaluating the cell case shows it: the givens those
    /// values are for, and each set of values, at most
    /// [`MAX_GENERAL_VALUES`]. A value mentions only the givens and the
    /// cell's head and tail; evaluation splits on at most two Bools it
    /// stops at, so a value the claim reaches under a comparison is found
    /// too.
    fn claims_at_tail(&mut self, v: &str, ob: &Obligation) -> (Vec<String>, Vec<Vec<Term>>) {
        let others: Vec<String> = {
            let mut fv = Vec::new();
            term::free_vars(&ob.lhs, &mut fv);
            term::free_vars(&ob.rhs, &mut fv);
            ob.givens
                .iter()
                .filter(|g| *g != v && fv.contains(g))
                .cloned()
                .collect()
        };
        if others.is_empty() {
            return (Vec::new(), Vec::new());
        }
        let mut taken = ob.givens.clone();
        term::free_vars(&ob.lhs, &mut taken);
        term::free_vars(&ob.rhs, &mut taken);
        let h = fresh(&format!("{v}Head"), &taken);
        taken.push(h.clone());
        let t = fresh(&format!("{v}Tail"), &taken);
        let cell = term::builtin("List.prepend", vec![term::var(&h), term::var(&t)], None);
        let at_cell = [(v.to_string(), cell)];
        let at_tail = [(v.to_string(), term::var(&t))];
        let (Ok(l), Ok(r), Ok(pl), Ok(pr)) = (
            term::subst(&ob.lhs, &at_cell),
            term::subst(&ob.rhs, &at_cell),
            term::subst(&ob.lhs, &at_tail),
            term::subst(&ob.rhs, &at_tail),
        ) else {
            return (Vec::new(), Vec::new());
        };
        let (hyps, fuel, dry) = (self.hyps.clone(), self.fuel, self.ran_dry);
        let mut forms = Vec::new();
        self.hyps.retain(|(_, e)| {
            let mut fv = Vec::new();
            term::free_vars(&e.lhs, &mut fv);
            term::free_vars(&e.rhs, &mut fv);
            !fv.iter().any(|n| n == v)
        });
        for side in [&l, &r] {
            self.forms_under_splits(side, 2, &mut forms);
        }
        self.hyps = hyps;
        self.fuel = fuel;
        self.ran_dry = dry;
        let mut scope: Vec<String> = ob.givens.clone();
        scope.push(h);
        scope.push(t.clone());
        let mut found: Vec<Vec<(String, Term)>> = Vec::new();
        fn parts<'t>(t: &'t Term, out: &mut Vec<&'t Term>) {
            out.push(t);
            for c in term::children(t) {
                parts(c, out);
            }
        }
        // A side of the claim the tail stands in: a bare given would match
        // anything.
        let pats: Vec<&Term> = [&pl, &pr]
            .into_iter()
            .filter(|p| {
                let mut fv = Vec::new();
                term::free_vars(p, &mut fv);
                fv.contains(&t) && !matches!(&p.node, ResolvedExpr::Ident(_))
            })
            .collect();
        // A value of the given's type, with no `match` in it.
        let fits = |g: &str, x: &Term| {
            let typed = match (ob.types.iter().find(|(n, _)| n == g), x.ty()) {
                (Some((_, want)), Some(have)) => want == have,
                (_, None) => term::int_value(x).is_none() || ob.ints.iter().any(|n| n == g),
                _ => true,
            };
            typed && !holds_match(x)
        };
        for form in &forms {
            let mut all = Vec::new();
            parts(form, &mut all);
            for sub in all {
                for pat in &pats {
                    let mut m = Vec::new();
                    if !super::rewrite::matches(pat, sub, &others, &mut m) {
                        continue;
                    }
                    let mut fv = Vec::new();
                    for (_, x) in &m {
                        term::free_vars(x, &mut fv);
                    }
                    let own = m
                        .iter()
                        .all(|(g, x)| matches!(&x.node, ResolvedExpr::Ident(n) if n == g));
                    if own
                        || fv.iter().any(|n| !scope.contains(n))
                        || !m.iter().all(|(g, x)| fits(g, x))
                        || found.contains(&m)
                    {
                        continue;
                    }
                    found.push(m);
                }
            }
        }
        found.truncate(MAX_GENERAL_VALUES);
        let general: Vec<String> = others
            .into_iter()
            .filter(|g| {
                found.iter().any(|m| {
                    m.iter().any(|(n, x)| {
                        n == g && !matches!(&x.node, ResolvedExpr::Ident(k) if k == g)
                    })
                })
            })
            .collect();
        let at = found
            .iter()
            .map(|m| {
                general
                    .iter()
                    .map(|g| {
                        m.iter()
                            .find(|(n, _)| n == g)
                            .map_or_else(|| term::var(g), |(_, x)| canon(x))
                    })
                    .collect()
            })
            .collect();
        (general, at)
    }

    /// What `t` evaluates to under the hypotheses in scope, and, where it
    /// stops at an undecided Bool, what it evaluates to under each value of
    /// it, at most `splits` deep.
    fn forms_under_splits(&mut self, t: &Term, splits: usize, out: &mut Vec<Term>) {
        let Ok(ev) = self.whnf(t) else {
            return;
        };
        let blocked = ev.blocked.clone();
        if let Ok(chain) = self.normalize_below(ev.chain, 8) {
            out.push(canon(chain.cur()));
        }
        let Some(g) = blocked.map(|g| canon(&g)) else {
            return;
        };
        if splits == 0 || term::bool_value(&g).is_some() || self.hyp_for(&g).is_some() {
            return;
        }
        for b in [true, false] {
            let name = self.fresh_hyp();
            self.hyps
                .push((name, Eqn::new(g.clone(), term::boolean(b))));
            self.forms_under_splits(t, splits - 1, out);
            self.hyps.pop();
        }
    }

    /// Induction on the list given `v` for every value of the givens in
    /// `general`, the cell case under the claim at the tail at those
    /// givens as they stand and at each set of values in `at`.
    fn list_induction_over(
        &mut self,
        v: &str,
        general: &[String],
        at: &[Vec<Term>],
        ob: &Obligation,
        depth: usize,
    ) -> Result<Proof, String> {
        let varying: Vec<&str> = std::iter::once(v)
            .chain(general.iter().map(String::as_str))
            .collect();
        let mentions_v = |e: &Eqn| {
            let mut fv = Vec::new();
            term::free_vars(&e.lhs, &mut fv);
            term::free_vars(&e.rhs, &mut fv);
            fv.iter().any(|n| varying.contains(&n.as_str()))
        };
        let mut taken = ob.givens.clone();
        term::free_vars(&ob.lhs, &mut taken);
        term::free_vars(&ob.rhs, &mut taken);
        // Each name once, as it stands innermost.
        let mut carried: Vec<(String, Eqn)> = Vec::new();
        for (i, (n, e)) in self.hyps.iter().enumerate() {
            taken.push(n.clone());
            term::free_vars(&e.lhs, &mut taken);
            term::free_vars(&e.rhs, &mut taken);
            let shadowed = self.hyps[i + 1..].iter().any(|(m, _)| m == n);
            if !shadowed && mentions_v(e) {
                carried.push((n.clone(), e.clone()));
            }
        }
        let kept: Vec<(String, Eqn)> = self
            .hyps
            .iter()
            .filter(|(_, e)| !mentions_v(e))
            .cloned()
            .collect();
        let h = fresh(&format!("{v}Head"), &taken);
        taken.push(h.clone());
        let t = fresh(&format!("{v}Tail"), &taken);
        let at_eqn = |e: &Eqn, m: &[(String, Term)]| -> Result<Eqn, String> {
            Ok(Eqn::new(term::subst(&e.lhs, m)?, term::subst(&e.rhs, m)?))
        };
        let claim = Eqn::new(ob.lhs.clone(), ob.rhs.clone());
        let in_case = |x: &Term| -> Result<Vec<(String, Eqn)>, String> {
            let mut hs = kept.clone();
            for (n, e) in &carried {
                hs.push((n.clone(), at_eqn(e, &[(v.to_string(), x.clone())])?));
            }
            Ok(hs)
        };
        let nil_term = canon(&crate::ast::Spanned::bare(ResolvedExpr::List(Vec::new())));
        // The parts of the cell have the types the list's own gives them.
        let list_ty = ob
            .types
            .iter()
            .find(|(n, _)| n == v)
            .map(|(_, ty)| ty.clone())
            .filter(|ty| matches!(ty, Type::List(_)));
        let head = term::var(&h);
        let tail = term::var(&t);
        if let Some(Type::List(elem)) = &list_ty {
            head.set_ty((**elem).clone());
            tail.set_ty(Type::List(elem.clone()));
        }
        let cell = term::builtin("List.prepend", vec![head, tail.clone()], list_ty);
        // The claim at the tail at the givens as they stand, then at each
        // set of values found.
        let own: Vec<Term> = general.iter().map(|g| term::var(g)).collect();
        let values: Vec<&Vec<Term>> = std::iter::once(&own).chain(at).collect();
        let saved = self.hyps.clone();
        let result = (|| -> Result<(Proof, Vec<IhAt>, Proof), String> {
            let nil_goal = at_eqn(&claim, &[(v.to_string(), nil_term.clone())])?;
            self.hyps = in_case(&nil_term)?;
            let nil = self.prove_by_evaluation(&nil_goal.lhs, &nil_goal.rhs, depth)?;
            self.hyps = in_case(&cell)?;
            let scope = self.hyps.clone();
            // Each hypothesis is taken where every carried one is proved
            // there; the case may do without it.
            let mut ihs = Vec::new();
            let mut stated = Vec::new();
            for xs in values {
                let mut m = vec![(v.to_string(), tail.clone())];
                m.extend(general.iter().cloned().zip(xs.iter().cloned()));
                let ih_eq = at_eqn(&claim, &m)?;
                self.hyps = scope.clone();
                if self.rewrites_forever(&ih_eq) {
                    continue;
                }
                let mut proofs = Vec::new();
                for (_, c) in &carried {
                    let at = at_eqn(c, &m)?;
                    match self.prove_by_evaluation(&at.lhs, &at.rhs, depth) {
                        Ok(p) => proofs.push(p),
                        Err(_) => break,
                    }
                }
                if proofs.len() == carried.len() {
                    self.next_ih += 1;
                    let ih = format!("ih{}", self.next_ih);
                    stated.push((ih.clone(), ih_eq));
                    ihs.push(IhAt {
                        name: ih,
                        at: if general.is_empty() {
                            Vec::new()
                        } else {
                            xs.clone()
                        },
                        carry: proofs,
                    });
                }
            }
            self.hyps = scope;
            self.hyps.extend(stated);
            let goal = at_eqn(&claim, &[(v.to_string(), cell.clone())])?;
            let cons = self.prove_by_evaluation(&goal.lhs, &goal.rhs, depth)?;
            Ok((nil, ihs, cons))
        })();
        self.hyps = saved;
        let (nil, ihs, cons) = result?;
        Ok(Proof::InductList {
            var: v.to_string(),
            lhs: canon(&ob.lhs),
            rhs: canon(&ob.rhs),
            carried: carried.into_iter().map(|(n, _)| n).collect(),
            nil: Box::new(nil),
            head: h,
            tail: t,
            general: general.to_vec(),
            ihs,
            cons: Box::new(cons),
        })
    }

    /// Induction along `f` (see [`Self::prove_by_induction`]).
    fn induction_along(
        &mut self,
        f: FnId,
        named: Option<&str>,
        ob: &Obligation,
        depth: usize,
    ) -> Result<Proof, String> {
        use crate::ir::proof_steps::sexpr::Names;
        let f_name = self.inputs.symbol_table.fn_name(f);
        let def = self.def(f);
        let ds = self.reachable_defs(f);
        // A definition the gate keeps closed says why.
        if def.is_none()
            && let Some(Err(why)) = self.raw_def(f).map(|d| induct::recursion(&d, &ds))
        {
            return Err(format!("induction: {why}"));
        }
        let rec = match def.as_ref().map(|d| induct::recursion(d, &ds)) {
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
        // must all name the same one, or which to follow would be a guess,
        // unless the law names the one to follow.
        let candidates: Vec<&Vec<Term>> = calls
            .iter()
            .filter(|c| {
                induct::varied(c, j, &ob.givens).is_ok_and(|(v, _)| named.is_none_or(|n| v == n))
            })
            .collect();
        let Some(first) = candidates.first() else {
            return Err(match named {
                Some(n) => format!(
                    "induction: the law names {n}, but no call of {f_name} passes {n} where {f_name} recurses"
                ),
                None => {
                    format!("induction: the claim never passes a given where {f_name} recurses")
                }
            });
        };
        let (v, _) = induct::varied(first, j, &ob.givens)?;
        let mut others: Vec<String> = Vec::new();
        for c in &candidates {
            let o = induct::varied(c, j, &ob.givens).expect("filtered").0;
            if o != v && !others.contains(&o) {
                others.push(o);
            }
        }
        // Several givens to follow (a law that names one has only it):
        // each is tried in turn, and the first line under which steps
        // close every case is kept; the kernel checks the proof all the
        // same.
        if let Some(other) = others.first() {
            let lines: Vec<String> = std::iter::once(v.clone()).chain(others.clone()).collect();
            if let Some((n, proof)) = self.induction_along_each(f, ob, depth, &lines) {
                self.advice.push(format!(
                    "steps closed it by trying each given in turn; add `induction {n}` to go straight to it"
                ));
                return Ok(proof);
            }
            return Err(format!(
                "induction: {f_name} recurses on {v} in one call and on {other} in another, and steps close every case along neither"
            ));
        }
        // Along a count toward zero the given is an Int; each case holds the
        // comparison's value, and a recursive call's hypothesis is the claim
        // at the Int it passes, closer to zero.
        if rec.guard.is_some() && !ob.ints.contains(&v) {
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
        // The helpers the recursion goes through, which the kernel reads it
        // through as well.
        for g in induct::helpers(f, &ds) {
            self.mark_used(g);
        }
        let saved = std::mem::take(&mut self.hyps);
        self.inducting.push(f);
        let result = (|| -> Result<Vec<InductCase>, String> {
            let mut cases = Vec::new();
            for (i, arm) in arms.iter().enumerate() {
                let in_case = |m: String| format!("induction along {f_name}, case {}: {m}", i + 1);
                let mut binders = Vec::new();
                if rec.guard.is_some() {
                    binders.push(self.fresh_hyp());
                } else {
                    // One name per part of the pattern, a wildcard part
                    // as well.
                    for part in term::pattern_parts(&arm.pattern) {
                        let name = if part == "_" { "x".to_string() } else { part };
                        let b = fresh(&name, &taken);
                        taken.push(b.clone());
                        binders.push(b);
                    }
                }
                let n = induct::self_calls(&arm.body, f, &ds)?.len();
                let names: Vec<String> = (0..n)
                    .map(|_| {
                        self.next_ih += 1;
                        format!("ih{}", self.next_ih)
                    })
                    .collect();
                let case = induct::case(
                    &def, &ds, &rec, &args, &v, &general, arm, &binders, &names, &ob.lhs, &ob.rhs,
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
                // While the claim at other values of the varied givens may
                // still be stated, the case is tried as its hypotheses stand;
                // they are evaluated the way the claim is
                // ([`Env::prove_with_evaluated_hypotheses`]) once no such
                // hypothesis is left to add.
                let evaluated = |env: &mut Self, stated: &[(String, Eqn)], stop: String| {
                    env.hyps = scope.clone();
                    env.hyps.extend(stated.iter().cloned());
                    let mark = env.mark();
                    env.prove_by_evaluation(&case.goal.lhs, &case.goal.rhs, depth)
                        .map_err(|_| {
                            env.rewind(mark);
                            in_case(stop)
                        })
                };
                let proof = loop {
                    self.hyps = scope.clone();
                    self.hyps.extend(stated.iter().cloned());
                    self.hypotheses_as_stated = !general.is_empty();
                    let tried = self.prove_by_evaluation(&case.goal.lhs, &case.goal.rhs, depth);
                    self.hypotheses_as_stated = false;
                    let stop = match tried {
                        Ok(p) => break p,
                        Err(m) => m,
                    };
                    rounds += 1;
                    if general.is_empty() {
                        return Err(in_case(stop));
                    }
                    if rounds > 3 {
                        break evaluated(self, &stated, stop)?;
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
                        break evaluated(self, &stated, stop)?;
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
        self.inducting.pop();
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

impl Env<'_> {
    /// Induction along each of `lines` in turn, the givens `f` recurses
    /// on: the first under which steps close every case, with its proof.
    /// Where none does, the facts that would rewrite where its cases
    /// stopped stay as hints.
    fn induction_along_each(
        &mut self,
        f: FnId,
        ob: &Obligation,
        depth: usize,
        lines: &[String],
    ) -> Option<(String, Proof)> {
        let (hyps, open_premises, fuel) =
            (self.hyps.clone(), self.open_premises.clone(), self.fuel);
        let before = self.hints.len();
        for n in lines {
            if let Ok(proof) = self.induction_along(f, Some(n), ob, depth) {
                self.hints.truncate(before);
                self.open_premises = open_premises;
                return Some((n.clone(), proof));
            }
            self.hyps = hyps.clone();
            self.fuel = fuel;
        }
        self.open_premises = open_premises;
        None
    }
}

/// Whether `t` holds a `match`.
fn holds_match(t: &Term) -> bool {
    matches!(t.node, ResolvedExpr::Match { .. }) || term::children(t).into_iter().any(holds_match)
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

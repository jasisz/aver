//! The equation each step proves, computed from the step's own data.
//!
//! Producers call [`conclusion`] on what they built before handing it on,
//! and the Lean renderer uses it to annotate every intermediate term. It
//! applies the same rules as the Aver replayer, but it is not the
//! independent check: that is the replayer's job (and the Lean kernel's).

use crate::ir::hir::{ResolvedExpr, ResolvedPattern};

use super::term::{self, Term, canon};
use super::{Eqn, Proof, Script};

/// A hypothesis in scope: its name and the equation it states.
pub type Hyps = Vec<(String, Eqn)>;

fn same(a: &Term, b: &Term) -> bool {
    canon(a) == canon(b)
}

fn same_eqn(a: &Eqn, b: &Eqn) -> bool {
    same(&a.lhs, &b.lhs) && same(&a.rhs, &b.rhs)
}

/// The term a pattern denotes once its variables are bound to `binders`.
/// Wildcards and catch-all names have no such term.
pub fn pattern_term(p: &ResolvedPattern, binders: &[Term]) -> Result<Term, String> {
    use crate::ir::hir::ResolvedCtor;
    let need = |n: usize| {
        if binders.len() == n {
            Ok(())
        } else {
            Err(format!("pattern binds {n} names, {} given", binders.len()))
        }
    };
    match p {
        ResolvedPattern::Literal(l) => {
            need(0)?;
            Ok(Spanned::bare(ResolvedExpr::Literal(l.clone())))
        }
        ResolvedPattern::EmptyList => {
            need(0)?;
            Ok(Spanned::bare(ResolvedExpr::List(Vec::new())))
        }
        ResolvedPattern::Cons(h, t) if h != "_" && t != "_" => {
            need(2)?;
            Ok(term::builtin(
                "List.prepend",
                vec![binders[0].clone(), binders[1].clone()],
                None,
            ))
        }
        ResolvedPattern::Ctor(c, names) if names.iter().all(|n| n != "_") => {
            need(names.len())?;
            if matches!(c, ResolvedCtor::Unresolved { .. }) {
                return Err("unresolved constructor".into());
            }
            Ok(Spanned::bare(ResolvedExpr::Ctor(
                c.clone(),
                binders.to_vec(),
            )))
        }
        _ => Err("this pattern has no term to equate the subject with".into()),
    }
}

use crate::ast::Spanned;

/// Equation `arm` (1-based) of a `match`, given the substitution for the
/// enclosing variables: `(subject = pattern, match = body)`.
pub fn arm_equation(
    subject: &Term,
    arms: &[crate::ir::hir::ResolvedMatchArm],
    arm: u32,
    outer: &[(String, Term)],
    binders: &[Term],
) -> Result<(Eqn, Term), String> {
    let k = arm as usize;
    if k == 0 || k > arms.len() {
        return Err(format!("no arm {arm}"));
    }
    let chosen = &arms[k - 1];
    if is_catch_all(&chosen.pattern) {
        // A catch-all arm is selected for one value: the premise names
        // it, every earlier arm must exclude it, and a named catch-all
        // binds it.
        let [value] = binders else {
            return Err("a catch-all arm takes the value it is chosen for".into());
        };
        for earlier in &arms[..k - 1] {
            if !excludes(&earlier.pattern, value) {
                return Err(format!("an earlier arm can also match arm {arm}"));
            }
        }
        let names = term::pattern_binders(&chosen.pattern);
        let mut map: Vec<(String, Term)> = outer
            .iter()
            .filter(|(n, _)| !names.contains(n))
            .cloned()
            .collect();
        map.extend(names.iter().map(|n| (n.clone(), value.clone())));
        let body = term::subst(&chosen.body, &map)?;
        let subject = term::subst(subject, outer)?;
        return Ok((Eqn::new(subject, canon(value)), body));
    }
    // First match wins: every earlier arm must exclude this pattern's head.
    for earlier in &arms[..k - 1] {
        if !heads_differ(&earlier.pattern, &chosen.pattern) {
            return Err(format!("an earlier arm can also match arm {arm}"));
        }
    }
    let pat = pattern_term(&chosen.pattern, binders)?;
    let names = term::pattern_binders(&chosen.pattern);
    let mut map: Vec<(String, Term)> = outer
        .iter()
        .filter(|(n, _)| !names.contains(n))
        .cloned()
        .collect();
    map.extend(names.iter().cloned().zip(binders.iter().cloned()));
    let body = term::subst(&chosen.body, &map)?;
    let subject = term::subst(subject, outer)?;
    Ok((Eqn::new(subject, pat), body))
}

/// A pattern every value matches: `_` or a bare name.
pub fn is_catch_all(p: &ResolvedPattern) -> bool {
    matches!(p, ResolvedPattern::Wildcard | ResolvedPattern::Ident(_))
}

/// Whether pattern `p` certainly does not match the value `v`: a
/// different literal, a different constructor, or the other list shape.
pub fn excludes(p: &ResolvedPattern, v: &Term) -> bool {
    use crate::ir::hir::ResolvedCallee;
    match (p, &v.node) {
        (ResolvedPattern::Literal(l), _) => {
            let lit = Spanned::bare(ResolvedExpr::Literal(l.clone()));
            match (term::int_value(&lit), term::int_value(v)) {
                (Some(a), Some(b)) => a != b,
                (None, None) => match &v.node {
                    ResolvedExpr::Literal(x) => {
                        std::mem::discriminant(x) == std::mem::discriminant(l) && x != l
                    }
                    _ => false,
                },
                _ => false,
            }
        }
        (ResolvedPattern::Ctor(c, _), ResolvedExpr::Ctor(d, _)) => {
            c != d
                && !matches!(c, crate::ir::hir::ResolvedCtor::Unresolved { .. })
                && !matches!(d, crate::ir::hir::ResolvedCtor::Unresolved { .. })
        }
        (ResolvedPattern::EmptyList, ResolvedExpr::Call(ResolvedCallee::Builtin(b), args)) => {
            b == "List.prepend" && args.len() == 2
        }
        (ResolvedPattern::Cons(..), ResolvedExpr::List(xs)) => xs.is_empty(),
        _ => false,
    }
}

fn heads_differ(a: &ResolvedPattern, b: &ResolvedPattern) -> bool {
    use ResolvedPattern as P;
    match (a, b) {
        (P::Literal(x), P::Literal(y)) => x != y,
        (P::EmptyList, P::Cons(..)) | (P::Cons(..), P::EmptyList) => true,
        (P::Ctor(x, _), P::Ctor(y, _)) => x != y,
        _ => false,
    }
}

/// The equation `p` proves, or why it proves none.
pub fn conclusion(p: &Proof, script: &Script, hyps: &Hyps) -> Result<Eqn, String> {
    match p {
        Proof::Refl(t) => Ok(Eqn::new(canon(t), canon(t))),
        Proof::Symm(inner) => {
            let e = conclusion(inner, script, hyps)?;
            Ok(Eqn::new(e.rhs, e.lhs))
        }
        Proof::Trans { terms, steps } => {
            if terms.len() != steps.len() + 1 || steps.is_empty() {
                return Err("trans: n steps need n+1 terms".into());
            }
            for (i, step) in steps.iter().enumerate() {
                let e = conclusion(step, script, hyps).map_err(|m| format!("trans.{i}: {m}"))?;
                if !same_eqn(&e, &Eqn::new(terms[i].clone(), terms[i + 1].clone())) {
                    return Err(format!("trans.{i}: step does not join the written terms"));
                }
            }
            Ok(Eqn::new(canon(&terms[0]), canon(&terms[terms.len() - 1])))
        }
        Proof::Congr { ctx, inner } => {
            if term::hole_count(ctx) != 1 {
                return Err("congr: context must hold exactly one hole".into());
            }
            let e = conclusion(inner, script, hyps)?;
            Ok(Eqn::new(term::plug(ctx, &e.lhs), term::plug(ctx, &e.rhs)))
        }
        Proof::Unfold {
            fn_id,
            arm,
            args,
            binders,
            premise,
        } => {
            let def = script
                .def(*fn_id)
                .ok_or_else(|| "unfold: no such definition".to_string())?;
            super::induct::structural_param(def).map_err(|m| format!("unfold: {m}"))?;
            let outer = def.outer(args).map_err(|m| format!("unfold: {m}"))?;
            let lhs = Spanned::bare(ResolvedExpr::Call(
                crate::ir::hir::ResolvedCallee::Fn(*fn_id),
                args.iter().map(canon).collect(),
            ));
            if let Some(ty) = def.body.ty() {
                lhs.set_ty(ty.clone());
            }
            if *arm == 0 {
                if premise.is_some() || !binders.is_empty() {
                    return Err("unfold: arm 0 takes no premise".into());
                }
                return Ok(Eqn::new(lhs, term::subst(&def.body, &outer)?));
            }
            let ResolvedExpr::Match { subject, arms } = &def.body.node else {
                return Err("unfold: the body is not a match".into());
            };
            let (needed, body) = arm_equation(subject, arms, *arm, &outer, binders)?;
            let premise = premise
                .as_ref()
                .ok_or_else(|| "unfold: an arm needs its premise".to_string())?;
            let got = conclusion(premise, script, hyps)?;
            if !same_eqn(&got, &needed) {
                return Err("unfold: the premise does not select this arm".into());
            }
            Ok(Eqn::new(lhs, body))
        }
        Proof::UnfoldConst { name } => {
            let c = script
                .constant(name)
                .ok_or_else(|| format!("const: no binding {name}"))?;
            let value = canon(&c.value);
            let lhs = term::var(name);
            if let Some(ty) = value.ty() {
                lhs.set_ty(ty.clone());
            }
            Ok(Eqn::new(lhs, value))
        }
        Proof::Arm {
            term: t,
            arm,
            binders,
            premise,
        } => {
            let ResolvedExpr::Match { subject, arms } = &t.node else {
                return Err("arm: not a match".into());
            };
            let (needed, body) = arm_equation(subject, arms, *arm, &[], binders)?;
            let got = conclusion(premise, script, hyps)?;
            if !same_eqn(&got, &needed) {
                return Err("arm: the premise does not select this arm".into());
            }
            Ok(Eqn::new(canon(t), body))
        }
        Proof::Proj { term: t } => {
            let ResolvedExpr::Attr(obj, field) = &t.node else {
                return Err("proj: not a field access".into());
            };
            let ResolvedExpr::RecordCreate { fields, .. } = &obj.node else {
                return Err("proj: not a record literal".into());
            };
            let value = fields
                .iter()
                .find(|(n, _)| n == field)
                .ok_or_else(|| "proj: no such field".to_string())?;
            Ok(Eqn::new(canon(t), canon(&value.1)))
        }
        Proof::Hyp(name) => hyps
            .iter()
            .rev()
            .find(|(n, _)| n == name)
            .map(|(_, e)| e.clone())
            .ok_or_else(|| format!("hyp: {name} is not in scope")),
        Proof::Rule {
            rule,
            subst,
            premises,
        } => {
            let (needed, concl) = rule
                .instantiate(subst)
                .ok_or_else(|| format!("rule {}: wrong substitution", rule.id()))?;
            if needed.len() != premises.len() {
                return Err(format!("rule {}: wrong number of premises", rule.id()));
            }
            for (i, (want, p)) in needed.iter().zip(premises).enumerate() {
                let got = conclusion(p, script, hyps)?;
                if !same_eqn(&got, want) {
                    return Err(format!("rule {}: premise {i} does not match", rule.id()));
                }
            }
            Ok(Eqn::new(canon(&concl.lhs), canon(&concl.rhs)))
        }
        Proof::Law {
            law,
            subst,
            premise,
        } => {
            let l = script
                .law(law)
                .ok_or_else(|| format!("law {law} is not cited"))?;
            if l.givens.len() != subst.len() || l.givens.iter().zip(subst).any(|(g, (k, _))| g != k)
            {
                return Err(format!("law {law}: wrong substitution"));
            }
            match (&l.premise, premise) {
                (None, None) => {}
                (Some(when), Some(p)) => {
                    let want = Eqn::new(term::subst(when, subst)?, term::boolean(true));
                    let got = conclusion(p, script, hyps)?;
                    if !same_eqn(&got, &want) {
                        return Err(format!("law {law}: premise does not match"));
                    }
                }
                _ => return Err(format!("law {law}: premise mismatch")),
            }
            Ok(Eqn::new(
                term::subst(&l.lhs, subst)?,
                term::subst(&l.rhs, subst)?,
            ))
        }
        Proof::Compute { lhs, rhs } => {
            let l =
                term::eval_closed(lhs).ok_or_else(|| "compute: lhs is not closed".to_string())?;
            let r = if term::is_literal(rhs) {
                rhs.clone()
            } else {
                term::eval_closed(rhs).ok_or_else(|| "compute: rhs is not closed".to_string())?
            };
            if term::int_value(&l) != term::int_value(&r)
                || term::bool_value(&l) != term::bool_value(&r)
            {
                return Err("compute: the sides evaluate differently".into());
            }
            Ok(Eqn::new(canon(lhs), canon(rhs)))
        }
        Proof::Cases {
            on,
            hyp,
            if_true,
            if_false,
        } => {
            let with = |v: bool| {
                let mut h = hyps.clone();
                h.push((hyp.clone(), Eqn::new(canon(on), term::boolean(v))));
                h
            };
            let t =
                conclusion(if_true, script, &with(true)).map_err(|m| format!("cases.true: {m}"))?;
            let f = conclusion(if_false, script, &with(false))
                .map_err(|m| format!("cases.false: {m}"))?;
            if !same_eqn(&t, &f) {
                return Err("cases: the two arms prove different equations".into());
            }
            Ok(t)
        }
        Proof::Induct {
            fn_id,
            args,
            lhs,
            rhs,
            cases,
        } => induct_conclusion(*fn_id, args, lhs, rhs, cases, script, hyps)
            .map_err(|m| format!("induct: {m}")),
        Proof::Linear {
            goal,
            value,
            hyps: names,
            weights,
        } => {
            let mut atoms = Vec::new();
            let mut facts = vec![
                super::linear::as_nonneg(goal, !value, &mut atoms)
                    .ok_or("linear: the goal is not an Int comparison")?,
            ];
            for n in names {
                let (_, e) = hyps
                    .iter()
                    .rev()
                    .find(|(h, _)| h == n)
                    .ok_or_else(|| format!("linear: hypothesis {n} is not in scope"))?;
                let v = term::bool_value(&e.rhs)
                    .ok_or_else(|| format!("linear: hypothesis {n} is not a decided comparison"))?;
                facts.push(
                    super::linear::as_nonneg(&e.lhs, v, &mut atoms).ok_or_else(|| {
                        format!("linear: hypothesis {n} is not an Int comparison")
                    })?,
                );
            }
            match super::linear::combine(&facts, weights) {
                Some(sum) if super::linear::contradicts(&sum) => {
                    Ok(Eqn::new(canon(goal), term::boolean(*value)))
                }
                _ => Err("linear: the weights do not add up to a contradiction".into()),
            }
        }
        Proof::Ring { lhs, rhs } => {
            if super::ring::same_polynomial(lhs, rhs) {
                Ok(Eqn::new(canon(lhs), canon(rhs)))
            } else {
                Err("ring: the two sides are different polynomials".into())
            }
        }
        Proof::Absurd {
            contradiction,
            lhs,
            rhs,
        } => {
            let e = conclusion(contradiction, script, hyps)?;
            match (term::bool_value(&e.lhs), term::bool_value(&e.rhs)) {
                (Some(a), Some(b)) if a != b => Ok(Eqn::new(canon(lhs), canon(rhs))),
                _ => Err("absurd: the step does not equate true with false".into()),
            }
        }
        Proof::Enum {
            var,
            lhs,
            rhs,
            cases,
        } => {
            let (_, ty) = script
                .obligation
                .finite
                .iter()
                .find(|(n, _)| n == var)
                .ok_or_else(|| format!("enum: {var} is not a given of finite type"))?;
            let values = ty.values();
            if values.len() != cases.len() {
                return Err(format!(
                    "enum: {var} has {} values, {} cases given",
                    values.len(),
                    cases.len()
                ));
            }
            for (i, (v, case)) in values.iter().zip(cases).enumerate() {
                let at = [(var.clone(), v.clone())];
                let scoped: Hyps = hyps
                    .iter()
                    .map(|(n, e)| {
                        Ok((
                            n.clone(),
                            Eqn::new(term::subst(&e.lhs, &at)?, term::subst(&e.rhs, &at)?),
                        ))
                    })
                    .collect::<Result<_, String>>()?;
                let got =
                    conclusion(case, script, &scoped).map_err(|m| format!("enum.{i}: {m}"))?;
                let want = Eqn::new(term::subst(lhs, &at)?, term::subst(rhs, &at)?);
                if !same_eqn(&got, &want) {
                    return Err(format!("enum.{i}: the case proves a different equation"));
                }
            }
            Ok(Eqn::new(canon(lhs), canon(rhs)))
        }
    }
}

/// Check a whole script: the proof must prove the obligation.
pub fn check_script(script: &Script) -> Result<(), String> {
    super::induct::refuse_mutual_recursion(&script.defs)?;
    let mut hyps = Hyps::new();
    if let Some(p) = &script.obligation.premise {
        hyps.push(("when".to_string(), Eqn::new(canon(p), term::boolean(true))));
    }
    let e = conclusion(&script.proof, script, &hyps)?;
    let want = Eqn::new(script.obligation.lhs.clone(), script.obligation.rhs.clone());
    if same_eqn(&e, &want) {
        Ok(())
    } else {
        Err("the proof ends at a different equation than the claim".into())
    }
}

/// The claim an [`Proof::Induct`] step proves, once every case is checked.
fn induct_conclusion(
    fn_id: crate::ir::identity::FnId,
    args: &[Term],
    lhs: &Term,
    rhs: &Term,
    cases: &[super::InductCase],
    script: &Script,
    hyps: &Hyps,
) -> Result<Eqn, String> {
    let def = script.def(fn_id).ok_or("no such definition")?;
    let j = super::induct::structural_param(def)?.ok_or("the function does not recurse")?;
    let ResolvedExpr::Match { arms, .. } = &def.body.node else {
        return Err("the body is not a match".into());
    };
    if args.len() != def.params.len() {
        return Err("wrong number of arguments".into());
    }
    let givens = &script.obligation.givens;
    let (v, general) = super::induct::varied(args, j, givens)?;
    let mut taken = Vec::new();
    term::free_vars(lhs, &mut taken);
    term::free_vars(rhs, &mut taken);
    for (_, e) in hyps {
        let mut fv = Vec::new();
        term::free_vars(&e.lhs, &mut fv);
        term::free_vars(&e.rhs, &mut fv);
        if fv
            .iter()
            .any(|n| *n == v || general.iter().any(|(_, g)| g == n))
        {
            return Err("a hypothesis in scope mentions a variable the induction varies".into());
        }
        taken.extend(fv);
    }
    if arms.len() != cases.len() {
        return Err(format!("{} arms, {} cases", arms.len(), cases.len()));
    }
    let has = |p: fn(&ResolvedPattern) -> bool| arms.iter().any(|a| p(&a.pattern));
    if (has(|p| matches!(p, ResolvedPattern::EmptyList))
        != has(|p| matches!(p, ResolvedPattern::Cons(..))))
        || has(|p| {
            !matches!(
                p,
                ResolvedPattern::EmptyList | ResolvedPattern::Cons(..) | ResolvedPattern::Ctor(..)
            )
        })
    {
        return Err("the arms are not one per constructor".into());
    }
    for (i, (arm, case)) in arms.iter().zip(cases).enumerate() {
        for (k, b) in case.binders.iter().enumerate() {
            if taken.contains(b)
                || givens.contains(b)
                || script.constant(b).is_some()
                || case.binders[..k].contains(b)
            {
                return Err(format!("case {i}: {b} is not a fresh name"));
            }
        }
        let (goal, ihs) = super::induct::case(
            def,
            args,
            j,
            &v,
            &general,
            arm,
            &case.binders,
            &case.ihs,
            lhs,
            rhs,
        )
        .map_err(|m| format!("case {i}: {m}"))?;
        let mut scope = hyps.clone();
        scope.extend(ihs);
        let got = conclusion(&case.proof, script, &scope).map_err(|m| format!("case {i}: {m}"))?;
        if !same_eqn(&got, &goal) {
            return Err(format!("case {i}: the case proves a different equation"));
        }
    }
    Ok(Eqn::new(canon(lhs), canon(rhs)))
}

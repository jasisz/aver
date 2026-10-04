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
            if def.params.len() != args.len() {
                return Err("unfold: wrong number of arguments".into());
            }
            let outer: Vec<(String, Term)> = def
                .params
                .iter()
                .cloned()
                .zip(args.iter().cloned())
                .collect();
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
    }
}

/// Check a whole script: the proof must prove the obligation.
pub fn check_script(script: &Script) -> Result<(), String> {
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

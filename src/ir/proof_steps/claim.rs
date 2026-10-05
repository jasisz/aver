//! The equation each step claims, read off the step's own data.
//!
//! Producers call [`claim`] to learn the term a step leads to, and the
//! Lean renderer uses it to annotate every intermediate term. Nothing here
//! judges a step: whether a step proves what it claims is the job of the
//! kernel written in Aver (`crate::proof_kernel`) and of the Lean kernel.
//! A step that claims something it does not prove is refused there, so
//! this file only computes, over the compiler's typed terms.

use crate::ast::Spanned;
use crate::ir::hir::{ResolvedExpr, ResolvedPattern};

use super::term::{self, Term, canon};
use super::{Eqn, Proof, Script};

/// A hypothesis in scope: its name and the equation it states.
pub type Hyps = Vec<(String, Eqn)>;

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

/// Equation `arm` (1-based) of a `match`, given the substitution for the
/// enclosing variables: `(subject = pattern, match = body)`. A catch-all
/// arm takes the one value it is chosen for as its only binder.
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
    let names = term::pattern_binders(&chosen.pattern);
    let mut map: Vec<(String, Term)> = outer
        .iter()
        .filter(|(n, _)| !names.contains(n))
        .cloned()
        .collect();
    let subject = term::subst(subject, outer)?;
    if is_catch_all(&chosen.pattern) {
        let [value] = binders else {
            return Err("a catch-all arm takes the value it is chosen for".into());
        };
        map.extend(names.iter().map(|n| (n.clone(), value.clone())));
        let body = term::subst(&chosen.body, &map)?;
        return Ok((Eqn::new(subject, canon(value)), body));
    }
    let pat = pattern_term(&chosen.pattern, binders)?;
    map.extend(names.iter().cloned().zip(binders.iter().cloned()));
    let body = term::subst(&chosen.body, &map)?;
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

/// The equation `p` claims, or why its data names none (a definition,
/// field, hypothesis or law that is not there).
pub fn claim(p: &Proof, script: &Script, hyps: &Hyps) -> Result<Eqn, String> {
    match p {
        Proof::Refl(t) => Ok(Eqn::new(canon(t), canon(t))),
        Proof::Symm(inner) => {
            let e = claim(inner, script, hyps)?;
            Ok(Eqn::new(e.rhs, e.lhs))
        }
        Proof::Trans { terms, .. } => match (terms.first(), terms.last()) {
            (Some(a), Some(b)) => Ok(Eqn::new(canon(a), canon(b))),
            _ => Err("trans: no terms".into()),
        },
        Proof::Congr { ctx, inner } => {
            let e = claim(inner, script, hyps)?;
            Ok(Eqn::new(term::plug(ctx, &e.lhs), term::plug(ctx, &e.rhs)))
        }
        Proof::Unfold {
            fn_id,
            arm,
            args,
            binders,
            ..
        } => {
            let def = script
                .def(*fn_id)
                .ok_or_else(|| "unfold: no such definition".to_string())?;
            let outer = def.outer(args).map_err(|m| format!("unfold: {m}"))?;
            let lhs = Spanned::bare(ResolvedExpr::Call(
                crate::ir::hir::ResolvedCallee::Fn(*fn_id),
                args.iter().map(canon).collect(),
            ));
            if let Some(ty) = def.body.ty() {
                lhs.set_ty(ty.clone());
            }
            if *arm == 0 {
                return Ok(Eqn::new(lhs, term::subst(&def.body, &outer)?));
            }
            let ResolvedExpr::Match { subject, arms } = &def.body.node else {
                return Err("unfold: the body is not a match".into());
            };
            let (_, body) = arm_equation(subject, arms, *arm, &outer, binders)?;
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
            ..
        } => {
            let ResolvedExpr::Match { subject, arms } = &t.node else {
                return Err("arm: not a match".into());
            };
            let (_, body) = arm_equation(subject, arms, *arm, &[], binders)?;
            Ok(Eqn::new(canon(t), body))
        }
        Proof::Cell { list } => {
            let cell = term::cell_of(list).ok_or("cell: not a list literal with an element")?;
            Ok(Eqn::new(canon(list), canon(&cell)))
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
        Proof::Rule { rule, subst, .. } => {
            let (_, concl) = rule
                .instantiate(subst)
                .ok_or_else(|| format!("rule {}: wrong substitution", rule.id()))?;
            Ok(Eqn::new(canon(&concl.lhs), canon(&concl.rhs)))
        }
        Proof::Law { law, subst, .. } => {
            let l = script
                .law(law)
                .ok_or_else(|| format!("law {law} is not cited"))?;
            Ok(Eqn::new(
                term::subst(&l.lhs, subst)?,
                term::subst(&l.rhs, subst)?,
            ))
        }
        Proof::Cases {
            on, hyp, if_true, ..
        } => {
            let mut h = hyps.clone();
            h.push((hyp.clone(), Eqn::new(canon(on), term::boolean(true))));
            claim(if_true, script, &h)
        }
        Proof::Have {
            name, fact, body, ..
        } => {
            let mut h = hyps.clone();
            h.push((name.clone(), Eqn::new(canon(fact), term::boolean(true))));
            claim(body, script, &h)
        }
        Proof::Compute { lhs, rhs }
        | Proof::Ring { lhs, rhs }
        | Proof::Absurd { lhs, rhs, .. }
        | Proof::Enum { lhs, rhs, .. }
        | Proof::Induct { lhs, rhs, .. }
        | Proof::InductList { lhs, rhs, .. }
        | Proof::InductInt { lhs, rhs, .. } => Ok(Eqn::new(canon(lhs), canon(rhs))),
        Proof::Linear { goal, value, .. } => Ok(Eqn::new(canon(goal), term::boolean(*value))),
    }
}

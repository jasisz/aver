//! Induction following a function's own recursion, and the termination
//! gate every recursive definition a step opens must pass.
//!
//! A definition may call itself only when its body is a `match` on one
//! parameter and every recursive call passes, at that parameter's place, a
//! name the enclosing arm's pattern binds: a strict part of the value
//! matched, so the recursion stops. That is the gate for both opening a
//! definition ([`super::Proof::Unfold`]) and inducting along it
//! ([`super::Proof::Induct`]). Definitions that call each other are
//! refused outright.
//!
//! An induction step names its leading function `f` and the arguments the
//! claim applies it to. The argument at the matched place must be a given;
//! the other arguments that are givens are generalised. Each arm of `f` is
//! one case: the given becomes the arm's pattern over fresh names, and each
//! recursive call in the arm gives one induction hypothesis, the claim at
//! that call's arguments.

use crate::ir::hir::{ResolvedCallee, ResolvedExpr, ResolvedMatchArm};
use crate::ir::identity::FnId;

use super::term::{self, Term};
use super::{Def, Eqn};

/// The self-calls in `t`, in pre-order (a call before the calls inside
/// its arguments; a match's subject before its arms, arms in order), each
/// with the names that inner match arms bind around it.
pub fn self_calls(t: &Term, f: FnId) -> Vec<(Vec<Term>, Vec<String>)> {
    let mut out = Vec::new();
    walk(t, f, &mut Vec::new(), &mut out);
    out
}

fn walk(t: &Term, f: FnId, bound: &mut Vec<String>, out: &mut Vec<(Vec<Term>, Vec<String>)>) {
    match &t.node {
        ResolvedExpr::Call(ResolvedCallee::Fn(g), args) if *g == f => {
            out.push((args.clone(), bound.clone()));
            for a in args {
                walk(a, f, bound, out);
            }
        }
        ResolvedExpr::TailCall { target, args } if *target == f => {
            out.push((args.clone(), bound.clone()));
            for a in args {
                walk(a, f, bound, out);
            }
        }
        ResolvedExpr::Match { subject, arms } => {
            walk(subject, f, bound, out);
            for arm in arms {
                walk_arm(arm, f, bound, out);
            }
        }
        _ => {
            for c in term::children(t) {
                walk(c, f, bound, out);
            }
        }
    }
}

fn walk_arm(
    arm: &ResolvedMatchArm,
    f: FnId,
    bound: &mut Vec<String>,
    out: &mut Vec<(Vec<Term>, Vec<String>)>,
) {
    let names = term::pattern_binders(&arm.pattern);
    let before = bound.len();
    bound.extend(names);
    walk(&arm.body, f, bound, out);
    bound.truncate(before);
}

/// `None` when `def` does not call itself; the place of the parameter it
/// recurses on when it passes the gate; why not otherwise.
pub fn structural_param(def: &Def) -> Result<Option<usize>, String> {
    let f = def.fn_id;
    let in_lets: usize = def.lets.iter().map(|(_, v)| self_calls(v, f).len()).sum();
    let in_body = self_calls(&def.body, f);
    if in_lets == 0 && in_body.is_empty() {
        return Ok(None);
    }
    let refuse = |why: &str| Err(format!("{} recurses {why}", def.name));
    if in_lets > 0 || !def.lets.is_empty() {
        return refuse("from a local binding");
    }
    let ResolvedExpr::Match { subject, arms } = &def.body.node else {
        return refuse("outside a match on a parameter");
    };
    let ResolvedExpr::Ident(p) = &subject.node else {
        return refuse("on a match whose subject is not a parameter");
    };
    let Some(j) = def.params.iter().position(|n| n == p) else {
        return refuse("on a match whose subject is not a parameter");
    };
    for arm in arms {
        let parts = term::pattern_binders(&arm.pattern);
        for (args, inner) in self_calls(&arm.body, f) {
            let smaller = match args.get(j).map(|a| &a.node) {
                Some(ResolvedExpr::Ident(b)) => parts.contains(b) && !inner.contains(b),
                _ => false,
            };
            if args.len() != def.params.len() || !smaller {
                return refuse("on something that is not a part of the value it matches");
            }
        }
    }
    Ok(Some(j))
}

/// The given at the matched place `j`, and the other givens among `args`
/// that vary with the recursion, with their places. A given that appears
/// twice varies at its first place only.
pub fn varied(
    args: &[Term],
    j: usize,
    givens: &[String],
) -> Result<(String, Vec<(usize, String)>), String> {
    let v = match args.get(j).map(|a| &a.node) {
        Some(ResolvedExpr::Ident(n)) if givens.contains(n) => n.clone(),
        _ => return Err("the argument at the matched place must be a given".into()),
    };
    let mut general: Vec<(usize, String)> = Vec::new();
    for (k, a) in args.iter().enumerate() {
        if let ResolvedExpr::Ident(n) = &a.node
            && k != j
            && *n != v
            && givens.contains(n)
            && !general.iter().any(|(_, m)| m == n)
        {
            general.push((k, n.clone()));
        }
    }
    Ok((v, general))
}

/// Case `arm` of an induction along `def` at `args`, with `binders` for the
/// arm's pattern variables: the claim `lhs = rhs` at the arm's pattern, and
/// one hypothesis per recursive call in the arm, named by `ihs`, the claim
/// at that call's arguments. `v` and `general` are what [`varied`] found.
#[allow(clippy::too_many_arguments)]
pub fn case(
    def: &Def,
    args: &[Term],
    j: usize,
    v: &str,
    general: &[(usize, String)],
    arm: &ResolvedMatchArm,
    binders: &[String],
    ihs: &[String],
    lhs: &Term,
    rhs: &Term,
) -> Result<(Eqn, Vec<(String, Eqn)>), String> {
    let names = term::pattern_binders(&arm.pattern);
    if binders.len() != names.len() {
        return Err("wrong number of names".into());
    }
    let ys: Vec<Term> = binders.iter().map(|b| term::var(b)).collect();
    let value = super::claim::pattern_term(&arm.pattern, &ys)?;
    let at = [(v.to_string(), value)];
    let goal = Eqn::new(term::subst(lhs, &at)?, term::subst(rhs, &at)?);
    let calls = self_calls(&arm.body, def.fn_id);
    if calls.len() != ihs.len() {
        return Err(format!(
            "{} recursive calls, {} hypotheses",
            calls.len(),
            ihs.len()
        ));
    }
    let mut map: Vec<(String, Term)> = def
        .outer(args)?
        .into_iter()
        .filter(|(n, _)| !names.contains(n))
        .collect();
    map.extend(names.iter().cloned().zip(ys));
    let mut out = Vec::new();
    for ((call, inner), ih) in calls.iter().zip(ihs) {
        let mut fv = Vec::new();
        for a in call {
            term::free_vars(a, &mut fv);
        }
        if fv.iter().any(|n| inner.contains(n)) {
            return Err("a recursive call reads a name an inner match binds".into());
        }
        let c: Vec<Term> = call
            .iter()
            .map(|a| term::subst(a, &map))
            .collect::<Result<_, _>>()?;
        let mut tau = vec![(v.to_string(), c[j].clone())];
        tau.extend(general.iter().map(|(k, g)| (g.clone(), c[*k].clone())));
        out.push((
            ih.clone(),
            Eqn::new(term::subst(lhs, &tau)?, term::subst(rhs, &tau)?),
        ));
    }
    Ok((goal, out))
}

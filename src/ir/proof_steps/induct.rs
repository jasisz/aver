//! Induction following a function's own recursion, and the termination
//! gate every recursive definition a step opens must pass.
//!
//! A definition may call itself only when its body is a `match` on one
//! parameter and every recursive call passes, at that parameter's place, a
//! name the enclosing arm's pattern binds: a strict part of the value
//! matched, so the recursion stops. Or it counts an Int parameter `p`
//! toward zero: a `match` on `p <= 0` or `p > 0`, the arm where `p` is at
//! most 0 without a recursive call, and every call in the other passing
//! `p - 1` or `p / k` (a literal `k` of at least 2) in `p`'s place, which
//! for `p > 0` is at least 0 and below `p`; it stops for every Int, a
//! negative one included. The gate returns what it checked
//! ([`recursion`]), and both opening a definition ([`super::Proof::Unfold`])
//! and inducting along it ([`super::Proof::Induct`]) read that.
//! Definitions that call each other are refused outright.
//!
//! An induction step names its leading function `f` and the arguments the
//! claim applies it to. The argument at the matched place must be a given;
//! the other arguments that are givens are generalised. Each arm of `f` is
//! one case. On a match on the value, the given becomes the arm's pattern
//! over fresh names; on an Int counted toward zero, the given stays and the
//! case's one name is the hypothesis that the comparison has the arm's
//! value. Each recursive call in the arm gives one induction hypothesis,
//! the claim at that call's arguments: a part of the value, or an Int
//! closer to zero where the comparison lets the call happen.

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

/// What the termination gate checked about a definition that calls
/// itself, which opening it and an induction along it both read.
#[derive(Debug, Clone, PartialEq)]
pub struct Recursion {
    /// The place of the parameter it recurses on.
    pub at: usize,
    /// For a definition that counts that Int toward zero, the comparison
    /// its match splits on; `None` for a match on the value itself, whose
    /// every recursive call passes a part of it.
    pub guard: Option<Guard>,
}

/// The comparison a definition that counts an Int toward zero matches on:
/// `p <= 0` (it stops where it is `true`) or `p > 0` (where it is `false`).
#[derive(Debug, Clone, PartialEq)]
pub struct Guard {
    /// The comparison, over the definition's parameters.
    pub subject: Term,
    /// Its value in the arm without a recursive call.
    pub stop: bool,
}

/// How a recursive call of a definition that counts an Int `p` toward
/// zero descends at `p`'s place: `p - 1`, or `p / k` for a literal `k` of
/// at least 2. Where `p > 0` each is at least 0 and below `p`.
#[derive(Debug, Clone, PartialEq)]
pub enum Descent {
    Less,
    Divide(num_bigint::BigInt),
}

/// `None` when `def` does not call itself; what the gate checked when it
/// passes ([`Recursion`]); why not otherwise. The gate: no local
/// bindings, and a body that is a `match` either on a parameter, every
/// recursive call passing at its place a name the arm's pattern binds and
/// no inner arm rebinds, or on `p <= 0` / `p > 0` for a parameter `p`, one
/// arm per truth value, the arm where `p` is at most 0 without a recursive
/// call and every call in the other descending ([`descent`]) at `p`'s
/// place, `p` not rebound around it.
pub fn recursion(def: &Def) -> Result<Option<Recursion>, String> {
    use crate::ast::{BinOp, Literal};
    use crate::ir::hir::ResolvedPattern;
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
    let place = |p: &str| def.params.iter().position(|n| n == p);
    if let ResolvedExpr::BinOp(op, p, zero) = &subject.node
        && let ResolvedExpr::Ident(p) = &p.node
        && term::int_value(zero) == Some(0.into())
    {
        let Some(j) = place(p) else {
            return refuse("on a match whose subject is not a parameter");
        };
        let stop = match op {
            BinOp::Lte => true,
            BinOp::Gt => false,
            _ => return Err(format!("{} does not count {p} down to zero", def.name)),
        };
        let value = |arm: &ResolvedMatchArm| match arm.pattern {
            ResolvedPattern::Literal(Literal::Bool(b)) => Some(b),
            _ => None,
        };
        let counts = match arms.as_slice() {
            [a, b] if value(a).is_some() && value(b).is_some() && value(a) != value(b) => {
                arms.iter().all(|arm| {
                    let calls = self_calls(&arm.body, f);
                    if value(arm) == Some(stop) {
                        calls.is_empty()
                    } else {
                        calls.iter().all(|(args, inner)| {
                            args.len() == def.params.len()
                                && descent(&args[j], p).is_some()
                                && !inner.contains(p)
                        })
                    }
                })
            }
            _ => false,
        };
        if !counts {
            return Err(format!("{} does not count {p} down to zero", def.name));
        }
        return Ok(Some(Recursion {
            at: j,
            guard: Some(Guard {
                subject: (**subject).clone(),
                stop,
            }),
        }));
    }
    if let ResolvedExpr::BinOp(..) = &subject.node {
        return refuse("outside a match on a parameter");
    }
    let ResolvedExpr::Ident(p) = &subject.node else {
        return refuse("on a match whose subject is not a parameter");
    };
    let Some(j) = place(p) else {
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
    Ok(Some(Recursion { at: j, guard: None }))
}

/// How `t`, a recursive call's argument at the place of the Int `p`,
/// descends: `p - 1`, or `p / k` for a literal `k` of at least 2.
pub fn descent(t: &Term, p: &str) -> Option<Descent> {
    use crate::ast::BinOp;
    use crate::ir::hir::BuiltinIntrinsic;
    let is_p = |x: &Term| matches!(&x.node, ResolvedExpr::Ident(n) if n == p);
    match &t.node {
        ResolvedExpr::BinOp(BinOp::Sub, x, one)
            if is_p(x) && term::int_value(one) == Some(1.into()) =>
        {
            Some(Descent::Less)
        }
        ResolvedExpr::Call(ResolvedCallee::Intrinsic(BuiltinIntrinsic::IntDivEuclid), xs)
            if xs.len() == 2 && is_p(&xs[0]) =>
        {
            term::int_value(&xs[1])
                .filter(|k| *k >= 2.into())
                .map(Descent::Divide)
        }
        _ => None,
    }
}

/// Whether `def` counts an Int toward zero with some recursive call that
/// divides it, which opens only where its guard is decided.
pub fn divides_down(def: &Def) -> bool {
    let Ok(Some(Recursion { at, guard: Some(_) })) = recursion(def) else {
        return false;
    };
    self_calls(&def.body, def.fn_id).iter().any(|(args, _)| {
        matches!(
            descent(&args[at], &def.params[at]),
            Some(Descent::Divide(_))
        )
    })
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

/// Case `arm` of an induction along `def` at `args`, as `rec` describes
/// its recursion, with `binders` for the names the case introduces: the
/// claim `lhs = rhs` in the case (at the arm's pattern, on a match on the
/// value), on an Int counted toward zero the hypothesis that the
/// comparison has the arm's value, and one hypothesis per recursive call
/// in the arm, named by `ihs`, the claim at that call's arguments, with
/// the substitution that puts it there. `v` and `general` are what
/// [`varied`] found.
#[allow(clippy::too_many_arguments)]
pub fn case(
    def: &Def,
    rec: &Recursion,
    args: &[Term],
    v: &str,
    general: &[(usize, String)],
    arm: &ResolvedMatchArm,
    binders: &[String],
    ihs: &[String],
    lhs: &Term,
    rhs: &Term,
) -> Result<Case, String> {
    let j = rec.at;
    let names = term::pattern_binders(&arm.pattern);
    let wanted = if rec.guard.is_some() { 1 } else { names.len() };
    if binders.len() != wanted {
        return Err("wrong number of names".into());
    }
    let ys: Vec<Term> = binders.iter().map(|b| term::var(b)).collect();
    let mut map: Vec<(String, Term)> = def
        .outer(args)?
        .into_iter()
        .filter(|(n, _)| !names.contains(n))
        .collect();
    let (value, guard) = match &rec.guard {
        None => {
            map.extend(names.iter().cloned().zip(ys.iter().cloned()));
            (super::claim::pattern_term(&arm.pattern, &ys)?, None)
        }
        Some(g) => {
            let crate::ir::hir::ResolvedPattern::Literal(crate::ast::Literal::Bool(b)) =
                arm.pattern
            else {
                return Err("the arm is not one value of the comparison".into());
            };
            let at = Eqn::new(term::subst(&g.subject, &map)?, term::boolean(b));
            // The given stays as the claim writes it, its type kept.
            (args[j].clone(), Some((binders[0].clone(), at)))
        }
    };
    let here = [(v.to_string(), value.clone())];
    let goal = Eqn::new(term::subst(lhs, &here)?, term::subst(rhs, &here)?);
    let calls = self_calls(&arm.body, def.fn_id);
    if calls.len() != ihs.len() {
        return Err(format!(
            "{} recursive calls, {} hypotheses",
            calls.len(),
            ihs.len()
        ));
    }
    let mut out = Vec::new();
    let mut at = Vec::new();
    for ((call, inner), ih) in calls.iter().zip(ihs) {
        // Only the argument at the recursive place and those at the varied
        // places enter the hypothesis; a name an inner match binds may sit
        // anywhere else.
        let mut fv = Vec::new();
        for (k, a) in call.iter().enumerate() {
            if k == j || general.iter().any(|(g, _)| *g == k) {
                term::free_vars(a, &mut fv);
            }
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
        at.push(tau);
    }
    Ok(Case {
        value,
        goal,
        guard,
        ihs: out,
        at,
    })
}

/// One case of an induction, as [`case`] states it.
pub struct Case {
    /// Where the case puts the given inducted on: the arm's pattern over
    /// the case's names, or the given itself on an Int counted toward zero.
    pub value: Term,
    /// The claim there.
    pub goal: Eqn,
    /// On an Int counted toward zero, the case's hypothesis: the comparison
    /// at the claim's arguments has the arm's value.
    pub guard: Option<(String, Eqn)>,
    /// One hypothesis per recursive call: the claim at its arguments.
    pub ihs: Vec<(String, Eqn)>,
    /// For each, the substitution that puts the claim there.
    pub at: Vec<Vec<(String, Term)>>,
}

/// For each recursive call in `arm`, in the order of [`case`]'s
/// hypotheses: the place, among the arm's pattern variables, of the part
/// it recurses on, and its arguments at the varied places, over `binders`.
pub fn ih_sources(
    def: &Def,
    args: &[Term],
    j: usize,
    general: &[(usize, String)],
    arm: &ResolvedMatchArm,
    binders: &[String],
) -> Result<Vec<(usize, Vec<Term>)>, String> {
    let names = term::pattern_binders(&arm.pattern);
    let ys: Vec<Term> = binders.iter().map(|b| term::var(b)).collect();
    let mut map: Vec<(String, Term)> = def
        .outer(args)?
        .into_iter()
        .filter(|(n, _)| !names.contains(n))
        .collect();
    map.extend(names.iter().cloned().zip(ys));
    self_calls(&arm.body, def.fn_id)
        .iter()
        .map(|(call, _)| {
            let part = match call.get(j).map(|a| &a.node) {
                Some(ResolvedExpr::Ident(b)) => names.iter().position(|n| n == b),
                _ => None,
            }
            .ok_or("a recursive call does not pass a part of the matched value")?;
            let at = general
                .iter()
                .map(|(k, _)| term::subst(&call[*k], &map))
                .collect::<Result<_, _>>()?;
            Ok((part, at))
        })
        .collect()
}

/// Whether an arm of `def` holds a recursive call inside a further case
/// split (a `match` in the arm), which Lean's own induction principle for
/// `def` splits into more cases than the arms.
pub fn nested_split(def: &Def) -> bool {
    let ResolvedExpr::Match { arms, .. } = &def.body.node else {
        return false;
    };
    arms.iter()
        .any(|arm| split_holds_self_call(&arm.body, def.fn_id))
}

fn split_holds_self_call(t: &Term, f: FnId) -> bool {
    if let ResolvedExpr::Match { subject, arms } = &t.node {
        return split_holds_self_call(subject, f)
            || arms.iter().any(|a| !self_calls(&a.body, f).is_empty());
    }
    term::children(t)
        .into_iter()
        .any(|c| split_holds_self_call(c, f))
}

//! Induction following a function's own recursion, and the termination
//! gate every recursive definition a step opens must pass.
//!
//! A definition may call itself only when its body is a `match` on one
//! parameter and every recursive call passes, at that parameter's place, a
//! name the enclosing arm's pattern binds, or `List.drop` of one: a value
//! smaller than the one matched, so the recursion stops. Or it counts an
//! Int parameter `p` toward zero: a `match` on `p <= 0` or `p > 0`, the
//! arm where `p` is at
//! most 0 without a recursive call, and every call in the other passing
//! `p - 1` or `p / k` (a literal `k` of at least 2) in `p`'s place, which
//! for `p > 0` is at least 0 and below `p`; it stops for every Int, a
//! negative one included. The gate returns what it checked
//! ([`recursion`]), and both opening a definition ([`super::Proof::Unfold`])
//! and inducting along it ([`super::Proof::Induct`]) read that.
//! A call of another definition that calls back into the one checked
//! counts as the recursive calls its body makes, its local bindings
//! substituted, read at the call's arguments ([`self_calls`]): a recursion
//! through a helper is checked as if the helper's body stood in its place. A helper that reaches itself
//! without passing the definition checked is refused, and so is any
//! other cycle, unless it passes a definition whose recursion read this
//! way passes the gate ([`rooted`]).
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

/// A recursive call: its arguments, and the names inner match arms bind
/// around it.
pub type Call = (Vec<Term>, Vec<String>);

/// The self-calls in `t`, in pre-order (a call before the calls inside
/// its arguments; a match's subject before its arms, arms in order), each
/// with the names that inner match arms bind around it. A call of another
/// definition of `ds` that calls back into `f` stands for the calls of `f`
/// its body makes, read at the call's arguments, before the calls in its
/// arguments: a recursion through a helper is checked as if the helper's
/// body stood in its place. The kernel's `selfCalls` is the judge; this
/// mirrors it so the producer writes what it will accept.
///
/// A helper that reaches itself without passing `f` is refused before
/// any body is read, and the helper bodies read are capped
/// ([`HELPER_READS`]): each path through a group of helpers is read on
/// its own, so the reading grows with the number of paths, not of
/// helpers.
pub fn self_calls(t: &Term, f: FnId, ds: &[Def]) -> Result<Vec<Call>, String> {
    let mut called = Vec::new();
    callees(t, &mut called);
    // Without a call of another definition of `ds` no helper is read.
    let calls_back = if called
        .iter()
        .any(|g| *g != f && ds.iter().any(|d| d.fn_id == *g))
    {
        calling_back(f, ds)
    } else {
        Vec::new()
    };
    if let Some(h) = helper_cycle(&called, ds, &calls_back) {
        return Err(cycle_refusal(f, h, ds));
    }
    let mut read = Reading {
        f,
        ds,
        calls_back,
        left: HELPER_READS,
    };
    let mut out = Vec::new();
    walk(t, &mut read, ds.len(), &mut Vec::new(), &mut out)?;
    Ok(out)
}

/// The most helper bodies one [`self_calls`] reads through. A recursion
/// whose calls through its helpers need more is refused with that reason.
pub const HELPER_READS: usize = 1024;

/// What one [`self_calls`] reads by: the definition whose calls it lists,
/// the definitions of `ds` that call back into it, and how many more
/// helper bodies it may read.
struct Reading<'a> {
    f: FnId,
    ds: &'a [Def],
    calls_back: Vec<FnId>,
    left: usize,
}

/// The definitions of `ds` other than `f` from which `f` is reachable:
/// the helpers whose calls [`self_calls`] reads through.
fn calling_back(f: FnId, ds: &[Def]) -> Vec<FnId> {
    if !ds.iter().any(|d| d.fn_id == f) {
        return Vec::new();
    }
    let callees: Vec<(FnId, Vec<FnId>)> = ds.iter().map(|d| (d.fn_id, def_callees(d))).collect();
    let mut found = vec![f];
    let mut todo = vec![f];
    while let Some(at) = todo.pop() {
        for (g, called) in &callees {
            if *g != at && called.contains(&at) && !found.contains(g) {
                found.push(*g);
                todo.push(*g);
            }
        }
    }
    found.retain(|g| *g != f);
    found
}

/// A helper reached from `called`, the functions a term calls, that lies
/// on a cycle of helpers calling back into the definition checked without
/// passing it: reading through it goes round that cycle until the fuel
/// runs out, which refuses the recursion, so it is refused at once.
fn helper_cycle(called: &[FnId], ds: &[Def], calls_back: &[FnId]) -> Option<FnId> {
    let edges = |g: FnId| -> Vec<FnId> {
        ds.iter()
            .find(|d| d.fn_id == g)
            .map(def_callees)
            .unwrap_or_default()
            .into_iter()
            .filter(|h| calls_back.contains(h))
            .collect()
    };
    // `false` while a helper's callees are being followed, `true` once done.
    let mut done: std::collections::HashMap<FnId, bool> = std::collections::HashMap::new();
    for &s in called.iter().filter(|g| calls_back.contains(g)) {
        if done.contains_key(&s) {
            continue;
        }
        done.insert(s, false);
        let mut stack: Vec<(FnId, Vec<FnId>)> = vec![(s, edges(s))];
        while let Some((at, next)) = stack.last_mut() {
            let at = *at;
            match next.pop() {
                Some(g) => match done.get(&g) {
                    Some(false) => return Some(g),
                    Some(true) => {}
                    None => {
                        done.insert(g, false);
                        let e = edges(g);
                        stack.push((g, e));
                    }
                },
                None => {
                    done.insert(at, true);
                    stack.pop();
                }
            }
        }
    }
    None
}

/// Why a recursion through `h`, a helper that reaches itself without
/// `f`, is refused.
fn cycle_refusal(f: FnId, h: FnId, ds: &[Def]) -> String {
    let name = |g: FnId| {
        ds.iter()
            .find(|d| d.fn_id == g)
            .map_or("the function", |d| d.name.as_str())
    };
    let f_name = name(f);
    format!(
        "{f_name} recurses through {}, which reaches itself without {f_name}",
        name(h)
    )
}

fn walk(
    t: &Term,
    read: &mut Reading,
    fuel: usize,
    bound: &mut Vec<String>,
    out: &mut Vec<Call>,
) -> Result<(), String> {
    let f = read.f;
    let called = match &t.node {
        ResolvedExpr::Call(ResolvedCallee::Fn(g), args) => Some((*g, args)),
        ResolvedExpr::TailCall { target, args } => Some((*target, args)),
        _ => None,
    };
    match (&t.node, called) {
        (_, Some((g, args))) => {
            if g == f {
                out.push((args.to_vec(), bound.clone()));
            } else {
                helper_calls(g, args, read, fuel, bound, out)?;
            }
            for a in args {
                walk(a, read, fuel, bound, out)?;
            }
        }
        (ResolvedExpr::Match { subject, arms }, _) => {
            walk(subject, read, fuel, bound, out)?;
            for arm in arms {
                let names = term::pattern_binders(&arm.pattern);
                let before = bound.len();
                bound.extend(names);
                walk(&arm.body, read, fuel, bound, out)?;
                bound.truncate(before);
            }
        }
        _ => {
            for c in term::children(t) {
                walk(c, read, fuel, bound, out)?;
            }
        }
    }
    Ok(())
}

/// The calls of `f` a call of `g` at `args` makes: none unless `g` is
/// another definition of `ds` that calls back into `f`; then those in its
/// body, each with `g`'s parameters replaced where no arm of `g` rebinds
/// them (refusing a capture), reading nothing else of `g`'s scope.
fn helper_calls(
    g: FnId,
    args: &[Term],
    read: &mut Reading,
    fuel: usize,
    bound: &[String],
    out: &mut Vec<Call>,
) -> Result<(), String> {
    let (f, ds) = (read.f, read.ds);
    let Some(h) = ds.iter().find(|d| d.fn_id == g) else {
        return Ok(());
    };
    if !read.calls_back.contains(&g) {
        return Ok(());
    }
    let f_name = ds
        .iter()
        .find(|d| d.fn_id == f)
        .map_or("the function", |d| d.name.as_str());
    if fuel == 0 {
        return Err(cycle_refusal(f, g, ds));
    }
    if read.left == 0 {
        return Err(format!(
            "{f_name} recurses through more than {HELPER_READS} readings of helper bodies"
        ));
    }
    read.left -= 1;
    if h.params.len() != args.len() {
        return Err(format!("{} takes {} arguments", h.name, h.params.len()));
    }
    // A call of `f` in a local binding would run whatever the body does
    // with it; without one, the bindings are substituted into the body.
    let mut in_lets = Vec::new();
    for (_, v) in &h.lets {
        walk(v, read, fuel - 1, &mut Vec::new(), &mut in_lets)?;
    }
    if !in_lets.is_empty() {
        return Err(format!(
            "{f_name} recurses from a local binding of {}",
            h.name
        ));
    }
    let mut lets: Vec<(String, Term)> = Vec::new();
    for (name, value) in &h.lets {
        let v = term::subst(value, &lets)?;
        lets.insert(0, (name.clone(), v));
    }
    let body = term::subst(&h.body, &lets)?;
    let mut inside = Vec::new();
    walk(&body, read, fuel - 1, &mut Vec::new(), &mut inside)?;
    for (call, inner) in inside {
        let mut used = Vec::new();
        for a in &call {
            term::free_vars(a, &mut used);
        }
        if let Some(n) = used
            .iter()
            .find(|n| !h.params.contains(n) && !inner.contains(n))
        {
            return Err(format!(
                "{} passes {n}, which it does not bind, to a recursive call",
                h.name
            ));
        }
        let map: Vec<(String, Term)> = h
            .params
            .iter()
            .cloned()
            .zip(args.iter().cloned())
            .filter(|(p, _)| !inner.contains(p))
            .collect();
        for (p, v) in &map {
            if !used.contains(p) {
                continue;
            }
            let mut fv = Vec::new();
            term::free_vars(v, &mut fv);
            if let Some(c) = fv.iter().find(|n| inner.contains(n)) {
                return Err(format!("substituting {p} would capture {c}"));
            }
        }
        let call = call
            .iter()
            .map(|a| term::subst(a, &map))
            .collect::<Result<_, _>>()?;
        let mut names = bound.to_vec();
        names.extend(inner);
        out.push((call, names));
    }
    Ok(())
}

/// The functions `t` calls, match arms included.
fn callees(t: &Term, out: &mut Vec<FnId>) {
    match &t.node {
        ResolvedExpr::Call(ResolvedCallee::Fn(g), _) | ResolvedExpr::TailCall { target: g, .. } => {
            if !out.contains(g) {
                out.push(*g);
            }
        }
        ResolvedExpr::Match { arms, .. } => {
            for arm in arms {
                callees(&arm.body, out);
            }
        }
        _ => {}
    }
    for c in term::children(t) {
        callees(c, out);
    }
}

/// The functions `d` calls, in its local bindings and its body.
pub fn def_callees(d: &Def) -> Vec<FnId> {
    let mut out = Vec::new();
    for (_, v) in &d.lets {
        callees(v, &mut out);
    }
    callees(&d.body, &mut out);
    out
}

/// Whether `to` is reachable from `from`, another definition of `ds`,
/// through the definitions of `ds`.
fn reaches(from: FnId, to: FnId, ds: &[Def]) -> bool {
    if from == to {
        return false;
    }
    reachable(from, to, ds)
}

/// Whether `to` is `from` or reachable from it through the definitions of
/// `ds`.
fn reachable(from: FnId, to: FnId, ds: &[Def]) -> bool {
    let mut seen = vec![from];
    let mut todo = vec![from];
    while let Some(at) = todo.pop() {
        if at == to {
            return true;
        }
        let Some(d) = ds.iter().find(|d| d.fn_id == at) else {
            continue;
        };
        for g in def_callees(d) {
            if g != at && !seen.contains(&g) && ds.iter().any(|d| d.fn_id == g) {
                seen.push(g);
                todo.push(g);
            }
        }
    }
    false
}

/// The definitions of `ds` on a cycle with `f`, other than `f`: the
/// helpers its recursion goes through, which a script opening `f` carries.
pub fn helpers(f: FnId, ds: &[Def]) -> Vec<FnId> {
    ds.iter()
        .map(|d| d.fn_id)
        .filter(|g| *g != f && reachable(f, *g, ds) && reachable(*g, f, ds))
        .collect()
}

/// Whether every cycle through `def` passes a definition of `ds` whose
/// recursion, read through the others, passes the gate: `def` then stops,
/// and steps may open it though its own recursion is not one they follow.
pub fn rooted(def: &Def, ds: &[Def]) -> bool {
    rooted_by(def, ds, |r| matches!(recursion(r, ds), Ok(Some(_))))
}

/// [`rooted`], with whether a definition on a cycle with `def` passes the
/// gate read by `passes` (one that remembers what it read).
pub fn rooted_by(def: &Def, ds: &[Def], passes: impl Fn(&Def) -> bool) -> bool {
    let back = calling_back(def.fn_id, ds);
    let ahead = reached(def.fn_id, ds);
    ds.iter().any(|r| {
        (r.fn_id == def.fn_id || back.contains(&r.fn_id)) && ahead.contains(&r.fn_id) && passes(r)
    })
}

/// `from` and every definition of `ds` reachable from it ([`reachable`]).
fn reached(from: FnId, ds: &[Def]) -> Vec<FnId> {
    let mut seen = vec![from];
    let mut todo = vec![from];
    while let Some(at) = todo.pop() {
        let Some(d) = ds.iter().find(|d| d.fn_id == at) else {
            continue;
        };
        for g in def_callees(d) {
            if g != at && !seen.contains(&g) && ds.iter().any(|d| d.fn_id == g) {
                seen.push(g);
                todo.push(g);
            }
        }
    }
    seen
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
/// place, `p` not rebound around it. The recursive calls are those
/// [`self_calls`] lists, a recursion through other definitions of `ds`
/// included.
pub fn recursion(def: &Def, ds: &[Def]) -> Result<Option<Recursion>, String> {
    use crate::ast::{BinOp, Literal};
    use crate::ir::hir::ResolvedPattern;
    let f = def.fn_id;
    let mut in_lets = 0;
    for (_, v) in &def.lets {
        in_lets += self_calls(v, f, ds)?.len();
    }
    let in_body = self_calls(&def.body, f, ds)?;
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
        let mut counts = matches!(arms.as_slice(),
            [a, b] if value(a).is_some() && value(b).is_some() && value(a) != value(b));
        for arm in arms {
            let calls = self_calls(&arm.body, f, ds)?;
            counts &= if value(arm) == Some(stop) {
                calls.is_empty()
            } else {
                calls.iter().all(|(args, inner)| {
                    args.len() == def.params.len()
                        && descent(&args[j], p).is_some()
                        && !inner.contains(p)
                })
            };
        }
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
        for (args, inner) in self_calls(&arm.body, f, ds)? {
            let smaller = args.get(j).is_some_and(|a| smaller(a, &parts, &inner));
            if args.len() != def.params.len() || !smaller {
                return refuse("on something that is not a part of the value it matches");
            }
        }
    }
    Ok(Some(Recursion { at: j, guard: None }))
}

/// Whether steps may open `def` though its recursion is not one they
/// follow: it calls itself only directly, never back through another
/// definition of `ds`, and at one place every call, its local bindings
/// substituted, passes a list shorter than the one there. A shorter list
/// is the tail of a cell of the parameter (while no arm rebinds it) or of
/// a shorter list, a name a catch-all arm binds to a shorter list, or
/// `List.drop` of one. The kernel's `shortens` is the judge.
pub fn shortens(def: &Def, ds: &[Def]) -> bool {
    shortened_place(def, ds).is_some()
}

/// The place [`shortens`] finds, the first of them.
pub fn shortened_place(def: &Def, ds: &[Def]) -> Option<usize> {
    let f = def.fn_id;
    if def_callees(def)
        .iter()
        .any(|g| *g != f && reaches(*g, f, ds))
    {
        return None;
    }
    let mut lets: Vec<(String, Term)> = Vec::new();
    for (name, value) in &def.lets {
        let v = term::subst(value, &lets).ok()?;
        lets.insert(0, (name.clone(), v));
    }
    let body = term::subst(&def.body, &lets).ok()?;
    (0..def.params.len()).find(|j| shorter(&body, f, def, *j, true, &[]))
}

/// Every call of `f` in `t` passes at place `j` one of `parts` or
/// `List.drop` of one ([`shortens`]); `live` while the parameter at `j`
/// is not rebound.
fn shorter(t: &Term, f: FnId, def: &Def, j: usize, live: bool, parts: &[String]) -> bool {
    use crate::ir::hir::ResolvedPattern;
    let p = &def.params[j];
    match &t.node {
        ResolvedExpr::Call(ResolvedCallee::Fn(g), args)
        | ResolvedExpr::TailCall {
            target: g, args, ..
        } if *g == f => {
            args.len() == def.params.len()
                && smaller(&args[j], parts, &[])
                && args.iter().all(|a| shorter(a, f, def, j, live, parts))
        }
        ResolvedExpr::Match { subject, arms } => {
            shorter(subject, f, def, j, live, parts)
                && arms.iter().all(|arm| {
                    let bound = term::pattern_binders(&arm.pattern);
                    let mut inner: Vec<String> = parts
                        .iter()
                        .filter(|n| !bound.contains(n))
                        .cloned()
                        .collect();
                    let at_most = (live
                        && matches!(&subject.node, ResolvedExpr::Ident(n) if n == p))
                        || smaller(subject, parts, &[]);
                    match &arm.pattern {
                        ResolvedPattern::Cons(_, tail) if at_most => inner.push(tail.clone()),
                        ResolvedPattern::Ident(x) if smaller(subject, parts, &[]) => {
                            inner.push(x.clone())
                        }
                        _ => {}
                    }
                    shorter(&arm.body, f, def, j, live && !bound.contains(p), &inner)
                })
        }
        _ => term::children(t)
            .into_iter()
            .all(|c| shorter(c, f, def, j, live, parts)),
    }
}

/// Whether `t`, a recursive call's argument at the matched place, is
/// smaller than the value matched: a name the arm's pattern binds that no
/// inner arm rebinds, or `List.drop` of something smaller, which is never
/// longer than the list it drops from, whatever the count.
fn smaller(t: &Term, parts: &[String], inner: &[String]) -> bool {
    match &t.node {
        ResolvedExpr::Ident(b) => parts.contains(b) && !inner.contains(b),
        ResolvedExpr::Call(ResolvedCallee::Builtin(name), xs)
            if name == "List.drop" && xs.len() == 2 =>
        {
            smaller(&xs[0], parts, inner)
        }
        _ => false,
    }
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
pub fn divides_down(def: &Def, ds: &[Def]) -> bool {
    let Ok(Some(Recursion { at, guard: Some(_) })) = recursion(def, ds) else {
        return false;
    };
    let calls = self_calls(&def.body, def.fn_id, ds).unwrap_or_default();
    calls.iter().any(|(args, _)| {
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
/// [`varied`] found; `ds` the definitions a recursion through others
/// reads ([`self_calls`]).
#[allow(clippy::too_many_arguments)]
pub fn case(
    def: &Def,
    ds: &[Def],
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
    // One name per part of the pattern, a wildcard part as well.
    let parts = term::pattern_parts(&arm.pattern);
    let wanted = if rec.guard.is_some() { 1 } else { parts.len() };
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
            map.extend(
                parts
                    .iter()
                    .cloned()
                    .zip(ys.iter().cloned())
                    .filter(|(n, _)| n != "_"),
            );
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
    let calls = self_calls(&arm.body, def.fn_id, ds)?;
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
    ds: &[Def],
    args: &[Term],
    j: usize,
    general: &[(usize, String)],
    arm: &ResolvedMatchArm,
    binders: &[String],
) -> Result<Vec<(usize, Vec<Term>)>, String> {
    let names = term::pattern_binders(&arm.pattern);
    let parts = term::pattern_parts(&arm.pattern);
    let ys: Vec<Term> = binders.iter().map(|b| term::var(b)).collect();
    let mut map: Vec<(String, Term)> = def
        .outer(args)?
        .into_iter()
        .filter(|(n, _)| !names.contains(n))
        .collect();
    map.extend(parts.iter().cloned().zip(ys).filter(|(n, _)| n != "_"));
    self_calls(&arm.body, def.fn_id, ds)?
        .iter()
        .map(|(call, _)| {
            let part = match call.get(j).map(|a| &a.node) {
                Some(ResolvedExpr::Ident(b)) if b != "_" => parts.iter().position(|n| n == b),
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

/// Whether some recursive call of `def`, a match on the value, passes at
/// the matched place `List.drop` of a part rather than a part: an
/// induction along it then goes by the list's length, not by its cells.
pub fn drops(def: &Def, ds: &[Def]) -> bool {
    let Ok(Some(Recursion { at, guard: None })) = recursion(def, ds) else {
        return false;
    };
    self_calls(&def.body, def.fn_id, ds)
        .unwrap_or_default()
        .iter()
        .any(|(args, _)| !matches!(args.get(at).map(|a| &a.node), Some(ResolvedExpr::Ident(_))))
}

/// Whether an arm of `def` holds a recursive call inside a further case
/// split (a `match` in the arm), which Lean's own induction principle for
/// `def` splits into more cases than the arms; or a recursion through
/// other definitions of `ds`, which that principle does not follow.
pub fn nested_split(def: &Def, ds: &[Def]) -> bool {
    let ResolvedExpr::Match { arms, .. } = &def.body.node else {
        return false;
    };
    !helpers(def.fn_id, ds).is_empty()
        || arms
            .iter()
            .any(|arm| split_holds_self_call(&arm.body, def.fn_id, ds))
}

fn split_holds_self_call(t: &Term, f: FnId, ds: &[Def]) -> bool {
    if let ResolvedExpr::Match { subject, arms } = &t.node {
        return split_holds_self_call(subject, f, ds)
            || arms
                .iter()
                .any(|a| !self_calls(&a.body, f, ds).unwrap_or_default().is_empty());
    }
    term::children(t)
        .into_iter()
        .any(|c| split_holds_self_call(c, f, ds))
}

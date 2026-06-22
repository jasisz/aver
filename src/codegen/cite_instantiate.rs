//! Engine B — generic cited-lemma INSTANTIATION in the Dafny inductive step.
//!
//! The keystone (`earlier_law_citations`) hoists each eligible earlier law as a
//! `forall`-fact at the body top. That suffices when Z3 can instantiate the
//! universal itself — true for the builtin `List.concat` (Dafny `seq` `+`, whose
//! associativity Z3 knows natively), but NOT for a USER-defined `append`: Z3
//! never materialises the nested term `append(append(rev y, rev t), [h])`, so it
//! cannot fire the cited associativity there. Lean closes the same goal because
//! `simp_all` applies the pool laws as directed rewrites.
//!
//! This module recovers that power on Dafny by emitting EXPLICIT instantiation
//! calls of the cited lemmas at exactly the arguments the inductive step needs —
//! the same thing the retired `rev` synthesizer hand-coded, but derived
//! generically: substitute the induction variable by its `cons(head, tail)`,
//! unfold the recursive cone fns one step, apply the induction hypothesis, then
//! first-order-match each cited lemma's LHS against the resulting subterms.
//!
//! Fail-closed: every cited lemma is universal-form, so any type-correct
//! instantiation is a valid lemma call Dafny re-proves — a redundant one is
//! harmless, and structural matching against type-correct subterms keeps the
//! emitted calls well-typed. The engine need not be complete, only useful.

use crate::ast::{Expr, FnDef, Pattern, Spanned, VerifyLaw};
use crate::codegen::CodegenContext;
use crate::codegen::common::expr_to_dotted_name;
use std::collections::{BTreeMap, BTreeSet};

/// Placeholders for the cons head/tail of the induction variable in the computed
/// instantiation arguments. Backends map these to their own syntax (Dafny
/// `xs[0]` / `xs[1..]`, Lean's `head` / `tail` binders).
pub(crate) const HEAD: &str = "__cite_h";
pub(crate) const TAIL: &str = "__cite_t";
const UNFOLD_FUEL: usize = 8;

/// One computed instantiation of a cited law: which cited law (index into the
/// `cited` slice) and the argument expressions to apply it at, in the cited
/// law's given order. Arguments contain [`HEAD`] / [`TAIL`] placeholders that the
/// backend renders.
pub(crate) struct Instantiation {
    pub law_index: usize,
    pub args: Vec<Spanned<Expr>>,
}

/// Backend-neutral core of Engine B: for the list-induction step of `law` on
/// `ind_param` (the AVER given name), derive the exact arguments at which each
/// cited law must be applied so the step closes. Substitute the induction
/// variable by `cons(head, tail)`, unfold the recursive cone fns one step, apply
/// the induction hypothesis, then first-order-match each cited law's LHS against
/// the resulting subterms. Backends (Dafny lemma calls, Lean `have`-facts) render
/// the returned arguments.
pub(crate) fn compute_instantiations(
    law: &VerifyLaw,
    ind_param: &str,
    cited: &[&VerifyLaw],
    ctx: &CodegenContext,
) -> Vec<Instantiation> {
    if cited.is_empty() {
        return Vec::new();
    }
    let list_param = ind_param;
    let mod_scope = ctx.active_module_scope();
    // Recursive cone fns reachable from the law, keyed by call name, for unfolding.
    let mut cone: BTreeMap<String, &FnDef> = BTreeMap::new();
    let mut seed = BTreeSet::new();
    collect_calls(&law.lhs, &mut seed);
    collect_calls(&law.rhs, &mut seed);
    let mut frontier: Vec<String> = seed.into_iter().collect();
    while let Some(name) = frontier.pop() {
        if cone.contains_key(&name) {
            continue;
        }
        if let Some(fd) = ctx.fn_def_by_name(&name, mod_scope.as_deref()) {
            cone.insert(name.clone(), fd);
            if let Some(body) = fd.body.tail_expr() {
                let mut inner = BTreeSet::new();
                collect_calls(body, &mut inner);
                frontier.extend(inner);
            }
        }
    }

    let head = ident(HEAD);
    let tail = ident(TAIL);
    let consed = cons(head.clone(), tail.clone());

    // Step goal sides: substitute the induction var by `cons(head, tail)`, then
    // unfold the recursive cone fns one step (repeatedly) where the first arg is
    // a concrete cons.
    let lhs0 = unfold_fix(&subst(&law.lhs, list_param, &consed), &cone);
    let rhs0 = unfold_fix(&subst(&law.rhs, list_param, &consed), &cone);

    // Rewrite rules: the consuming law's own IH at `tail`, plus every cited law at
    // `tail`, applied as `lhs -> rhs` over the OTHER givens as wildcards. These
    // expose the reassociated terms the cited lemmas must match.
    let mut rules: Vec<RewriteRule> = Vec::new();
    rules.push(rule_at_tail(law, list_param));
    // Every cited law is also a rewrite (at all its givens) — applying it can
    // expose the reassociated subterms another cited lemma needs to match.
    for c in cited {
        rules.push(rewrite_rule(c));
    }

    // Build the candidate term pool: the unfolded sides, plus every rewrite-closure.
    let mut pool: Vec<Spanned<Expr>> = vec![lhs0.clone(), rhs0.clone()];
    for base in [&lhs0, &rhs0] {
        for r in &rules {
            let rewritten = apply_rule_all(base, r);
            if !expr_eq(&rewritten, base) {
                pool.push(rewritten);
            }
        }
    }

    // Collect every subterm of every pooled term.
    let mut subterms: Vec<Spanned<Expr>> = Vec::new();
    for t in &pool {
        collect_subterms(t, &mut subterms);
    }

    // Match each cited lemma's LHS against the subterms; record one instantiation
    // per distinct (law, args) match.
    let mut emitted: BTreeSet<String> = BTreeSet::new();
    let mut out: Vec<Instantiation> = Vec::new();
    for (law_index, c) in cited.iter().enumerate() {
        let wildcards: BTreeSet<String> = c.givens.iter().map(|g| g.name.clone()).collect();
        for sub in &subterms {
            let mut binds: BTreeMap<String, Spanned<Expr>> = BTreeMap::new();
            if match_expr(&c.lhs, sub, &wildcards, &mut binds) {
                // Build the arg list in the cited law's given order.
                let mut args: Vec<Spanned<Expr>> = Vec::new();
                let mut ok = true;
                for g in &c.givens {
                    match binds.get(&g.name) {
                        Some(e) => args.push(e.clone()),
                        None => {
                            ok = false;
                            break;
                        }
                    }
                }
                if !ok {
                    continue;
                }
                // Structural dedup key (placeholder names are stable).
                let key = format!(
                    "{law_index}:{:?}",
                    args.iter().map(|a| &a.node).collect::<Vec<_>>()
                );
                if emitted.insert(key) {
                    out.push(Instantiation { law_index, args });
                }
            }
        }
    }
    out
}

// ---- AST helpers ----------------------------------------------------------

fn ident(name: &str) -> Spanned<Expr> {
    Spanned::bare(Expr::Ident(name.to_string()))
}

/// A variable reference, whether source-form `Ident` or resolved (fn bodies are
/// lowered before the proof backend sees them, so both forms appear in the mix
/// of law sides + unfolded bodies).
pub(crate) fn ident_name(e: &Spanned<Expr>) -> Option<&str> {
    match &e.node {
        Expr::Ident(n) => Some(n),
        Expr::Resolved { name, .. } => Some(name),
        _ => None,
    }
}

fn cons(head: Spanned<Expr>, tail: Spanned<Expr>) -> Spanned<Expr> {
    Spanned::bare(Expr::FnCall(
        Box::new(ident("List.concat")),
        vec![Spanned::bare(Expr::List(vec![head])), tail],
    ))
}

/// Recognise `List.concat([h], t)` as `cons(h, t)`.
fn as_cons(e: &Spanned<Expr>) -> Option<(&Spanned<Expr>, &Spanned<Expr>)> {
    if let Expr::FnCall(callee, args) = &e.node
        && expr_to_dotted_name(&callee.node).as_deref() == Some("List.concat")
        && args.len() == 2
        && let Expr::List(items) = &args[0].node
        && items.len() == 1
    {
        return Some((&items[0], &args[1]));
    }
    None
}

fn collect_calls(e: &Spanned<Expr>, out: &mut BTreeSet<String>) {
    if let Expr::FnCall(callee, _) = &e.node
        && let Some(n) = expr_to_dotted_name(&callee.node)
    {
        out.insert(n);
    }
    for c in children(e) {
        collect_calls(c, out);
    }
}

/// Direct child expressions (for the variants the engine reasons about).
fn children(e: &Spanned<Expr>) -> Vec<&Spanned<Expr>> {
    match &e.node {
        Expr::FnCall(callee, args) => {
            let mut v = vec![callee.as_ref()];
            v.extend(args.iter());
            v
        }
        Expr::List(items) | Expr::Tuple(items) => items.iter().collect(),
        Expr::Constructor(_, Some(inner)) => vec![inner.as_ref()],
        Expr::Attr(inner, _) => vec![inner.as_ref()],
        Expr::BinOp(_, l, r) => vec![l.as_ref(), r.as_ref()],
        Expr::Neg(inner) | Expr::ErrorProp(inner) => vec![inner.as_ref()],
        _ => Vec::new(),
    }
}

/// Rebuild `e` with each direct child replaced by `f(child)`.
fn map_children(
    e: &Spanned<Expr>,
    f: &mut impl FnMut(&Spanned<Expr>) -> Spanned<Expr>,
) -> Spanned<Expr> {
    let node = match &e.node {
        Expr::FnCall(callee, args) => {
            Expr::FnCall(Box::new(f(callee)), args.iter().map(&mut *f).collect())
        }
        Expr::List(items) => Expr::List(items.iter().map(&mut *f).collect()),
        Expr::Tuple(items) => Expr::Tuple(items.iter().map(&mut *f).collect()),
        Expr::Constructor(name, Some(inner)) => {
            Expr::Constructor(name.clone(), Some(Box::new(f(inner))))
        }
        Expr::Attr(inner, field) => Expr::Attr(Box::new(f(inner)), field.clone()),
        Expr::BinOp(op, l, r) => Expr::BinOp(*op, Box::new(f(l)), Box::new(f(r))),
        Expr::Neg(inner) => Expr::Neg(Box::new(f(inner))),
        Expr::ErrorProp(inner) => Expr::ErrorProp(Box::new(f(inner))),
        other => other.clone(),
    };
    Spanned::bare(node)
}

fn collect_subterms(e: &Spanned<Expr>, out: &mut Vec<Spanned<Expr>>) {
    out.push(e.clone());
    for c in children(e) {
        collect_subterms(c, out);
    }
}

/// Substitute every variable reference named `name` (source or resolved) by `replacement`.
fn subst(e: &Spanned<Expr>, name: &str, replacement: &Spanned<Expr>) -> Spanned<Expr> {
    if ident_name(e) == Some(name) {
        return replacement.clone();
    }
    let mut f = |c: &Spanned<Expr>| subst(c, name, replacement);
    map_children(e, &mut f)
}

/// Simultaneous substitution — every mapped variable is replaced in ONE pass, so
/// a replacement expression's own free variables are not captured by a later
/// mapping (e.g. unfolding `append` where the tail carries `y` and the 2nd param
/// is also `y` bound to a different arg).
fn subst_many(e: &Spanned<Expr>, map: &BTreeMap<String, Spanned<Expr>>) -> Spanned<Expr> {
    if let Some(n) = ident_name(e)
        && let Some(rep) = map.get(n)
    {
        return rep.clone();
    }
    let mut f = |c: &Spanned<Expr>| subst_many(c, map);
    map_children(e, &mut f)
}

// ---- unfolding ------------------------------------------------------------

/// Unfold recursive cone-fn applications whose first arg is a concrete `cons`,
/// to a fixpoint (bounded by fuel).
fn unfold_fix(e: &Spanned<Expr>, cone: &BTreeMap<String, &FnDef>) -> Spanned<Expr> {
    let mut cur = e.clone();
    for _ in 0..UNFOLD_FUEL {
        let (next, changed) = unfold_once(&cur, cone);
        cur = next;
        if !changed {
            break;
        }
    }
    cur
}

fn unfold_once(e: &Spanned<Expr>, cone: &BTreeMap<String, &FnDef>) -> (Spanned<Expr>, bool) {
    if let Expr::FnCall(callee, args) = &e.node
        && let Some(name) = expr_to_dotted_name(&callee.node)
        && let Some(fd) = cone.get(&name)
        && !args.is_empty()
        && let Some((h, t)) = as_cons(&args[0])
        && let Some(unfolded) = unfold_cons_arm(fd, h, t, &args[1..])
    {
        return (unfolded, true);
    }
    let mut changed = false;
    let mut f = |c: &Spanned<Expr>| {
        let (next, ch) = unfold_once(c, cone);
        changed |= ch;
        next
    };
    let mapped = map_children(e, &mut f);
    (mapped, changed)
}

/// Instantiate `fd`'s `[head, ..tail]` match arm: bind the arm's head/tail names
/// to `h`/`t`, and the fn's remaining params to `rest`.
fn unfold_cons_arm(
    fd: &FnDef,
    h: &Spanned<Expr>,
    t: &Spanned<Expr>,
    rest: &[Spanned<Expr>],
) -> Option<Spanned<Expr>> {
    let body = fd.body.tail_expr()?;
    let Expr::Match { subject, arms } = &body.node else {
        return None;
    };
    // The match must dispatch on the first parameter.
    let first_param = fd.params.first()?.0.clone();
    if ident_name(subject) != Some(first_param.as_str()) {
        return None;
    }
    let arm = arms
        .iter()
        .find(|a| matches!(a.pattern, Pattern::Cons(_, _)))?;
    let Pattern::Cons(hname, tname) = &arm.pattern else {
        return None;
    };
    // Bind the cons head/tail AND the fn's remaining params simultaneously, so the
    // tail expression's free vars are not captured by a param substitution.
    let mut map: BTreeMap<String, Spanned<Expr>> = BTreeMap::new();
    map.insert(hname.clone(), h.clone());
    map.insert(tname.clone(), t.clone());
    for (param, arg) in fd.params.iter().skip(1).zip(rest.iter()) {
        map.insert(param.0.clone(), arg.clone());
    }
    Some(subst_many(&arm.body, &map))
}

// ---- rewrite rules (IH + cited laws) -------------------------------------

struct RewriteRule {
    lhs: Spanned<Expr>,
    rhs: Spanned<Expr>,
    wildcards: BTreeSet<String>,
}

/// The consuming law's IH at `tail`: `law.lhs[p->tail] -> law.rhs[p->tail]`, with
/// the OTHER givens left as wildcards.
fn rule_at_tail(law: &VerifyLaw, list_param: &str) -> RewriteRule {
    let tail = ident(TAIL);
    let lhs = subst(&law.lhs, list_param, &tail);
    let rhs = subst(&law.rhs, list_param, &tail);
    let wildcards = law
        .givens
        .iter()
        .map(|g| g.name.clone())
        .filter(|n| n != list_param)
        .collect();
    RewriteRule {
        lhs,
        rhs,
        wildcards,
    }
}

/// A cited law as a universal rewrite `lhs -> rhs` over all its givens.
fn rewrite_rule(law: &VerifyLaw) -> RewriteRule {
    RewriteRule {
        lhs: law.lhs.clone(),
        rhs: law.rhs.clone(),
        wildcards: law.givens.iter().map(|g| g.name.clone()).collect(),
    }
}

/// Rewrite every subterm matching `rule.lhs` to the instantiated `rule.rhs`.
fn apply_rule_all(e: &Spanned<Expr>, rule: &RewriteRule) -> Spanned<Expr> {
    let mut binds: BTreeMap<String, Spanned<Expr>> = BTreeMap::new();
    if match_expr(&rule.lhs, e, &rule.wildcards, &mut binds) {
        return instantiate(&rule.rhs, &binds);
    }
    let mut f = |c: &Spanned<Expr>| apply_rule_all(c, rule);
    map_children(e, &mut f)
}

/// Substitute the matched wildcard bindings into a rule RHS.
fn instantiate(e: &Spanned<Expr>, binds: &BTreeMap<String, Spanned<Expr>>) -> Spanned<Expr> {
    if let Some(n) = ident_name(e)
        && let Some(rep) = binds.get(n)
    {
        return rep.clone();
    }
    let mut f = |c: &Spanned<Expr>| instantiate(c, binds);
    map_children(e, &mut f)
}

// ---- first-order matching -------------------------------------------------

/// Match `pattern` against `target`; idents in `wildcards` bind to subterms
/// (consistently). Non-wildcard structure must match exactly.
fn match_expr(
    pattern: &Spanned<Expr>,
    target: &Spanned<Expr>,
    wildcards: &BTreeSet<String>,
    binds: &mut BTreeMap<String, Spanned<Expr>>,
) -> bool {
    if let Some(pn) = ident_name(pattern) {
        if wildcards.contains(pn) {
            return match binds.get(pn) {
                Some(prev) => expr_eq(prev, target),
                None => {
                    binds.insert(pn.to_string(), target.clone());
                    true
                }
            };
        }
        // Non-wildcard variable: the target must be the same-named variable.
        return ident_name(target) == Some(pn);
    }
    match (&pattern.node, &target.node) {
        (Expr::Literal(a), Expr::Literal(b)) => a == b,
        (Expr::FnCall(pc, pa), Expr::FnCall(tc, ta)) => {
            expr_to_dotted_name(&pc.node) == expr_to_dotted_name(&tc.node)
                && pa.len() == ta.len()
                && pa
                    .iter()
                    .zip(ta.iter())
                    .all(|(p, t)| match_expr(p, t, wildcards, binds))
        }
        (Expr::List(pa), Expr::List(ta)) => {
            pa.len() == ta.len()
                && pa
                    .iter()
                    .zip(ta.iter())
                    .all(|(p, t)| match_expr(p, t, wildcards, binds))
        }
        (Expr::Tuple(pa), Expr::Tuple(ta)) => {
            pa.len() == ta.len()
                && pa
                    .iter()
                    .zip(ta.iter())
                    .all(|(p, t)| match_expr(p, t, wildcards, binds))
        }
        (Expr::Constructor(pn, pi), Expr::Constructor(tn, ti)) => {
            pn == tn
                && match (pi, ti) {
                    (None, None) => true,
                    (Some(p), Some(t)) => match_expr(p, t, wildcards, binds),
                    _ => false,
                }
        }
        (Expr::Attr(pi, pf), Expr::Attr(ti, tf)) => {
            pf == tf && match_expr(pi, ti, wildcards, binds)
        }
        (Expr::BinOp(po, pl, pr), Expr::BinOp(to, tl, tr)) => {
            po == to && match_expr(pl, tl, wildcards, binds) && match_expr(pr, tr, wildcards, binds)
        }
        _ => false,
    }
}

fn expr_eq(a: &Spanned<Expr>, b: &Spanned<Expr>) -> bool {
    let empty = BTreeSet::new();
    let mut binds = BTreeMap::new();
    // Structural equality = match with no wildcards.
    match_expr(a, b, &empty, &mut binds)
}

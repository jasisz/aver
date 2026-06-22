//! Engine B — generic cited-lemma INSTANTIATION in the list-induction step,
//! shared by both proof backends.
//!
//! The keystone (each backend's `forall`-citation / simp-pool hoist) makes every
//! eligible earlier law available as a universal fact. That suffices when the
//! backend can instantiate the universal itself — Z3 for the builtin
//! `List.concat` (Dafny `seq` `+`, whose associativity it knows natively), or
//! Lean's `simp_all` applying the pool laws as directed rewrites. It does NOT
//! suffice for a USER-defined `append` on Dafny: Z3 never materialises the nested
//! term `append(append(rev y, rev t), [h])`, so it cannot fire the cited
//! associativity there.
//!
//! This module recovers that power generically by computing EXPLICIT
//! instantiations of the cited lemmas at exactly the arguments the inductive step
//! needs — the same thing the retired `rev` synthesizer hand-coded, but derived
//! from the law itself: substitute the induction variable by its
//! `cons(head, tail)`, unfold the recursive cone fns one step, apply the
//! induction hypothesis, then first-order-match each cited lemma's LHS against
//! the resulting subterms. Each backend renders the returned argument terms — the
//! Dafny consumer as explicit lemma CALLS (`rev_revDist(rev(x[1..]), [x[0]])`),
//! the Lean consumer as `have`-facts driving a tight `simp` (`have key :=
//! rev_law_revDist (rev tail) [head]; simp […]`).
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
/// Rounds of rewrite-closure when building the candidate term pool. Each round
/// applies every rule to the previous round's new terms and re-unfolds, so a term
/// needing N nested rewrites (a doubly-distributed `rev (rev t)`) surfaces by
/// round N. Bounded to keep the dedup'd closure from growing without limit.
const POOL_CLOSURE_FUEL: usize = 4;

/// Whether using `law` LEFT-TO-RIGHT as a rewrite rule would loop: its LHS
/// pattern (givens as wildcards) matches its own RHS, so applying `lhs -> rhs`
/// yields a term the same rule still fires on (a commutation `plus x y = plus y
/// x` is the canonical case). Such a law is fine for Dafny (Z3 instantiates the
/// universal, it does not rewrite) but a non-terminating simp rule on Lean — the
/// Lean cite selector drops it, leaning on `omega` for the commutativity instead.
pub(crate) fn law_rewrites_to_self(law: &VerifyLaw) -> bool {
    let wildcards: BTreeSet<String> = law.givens.iter().map(|g| g.name.clone()).collect();
    if wildcards.is_empty() {
        return false;
    }
    let mut binds: BTreeMap<String, Spanned<Expr>> = BTreeMap::new();
    !expr_eq(&law.lhs, &law.rhs) && match_expr(&law.lhs, &law.rhs, &wildcards, &mut binds)
}

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

    // Build the candidate term pool: the unfolded sides, then the rewrite-closure
    // taken to a (fuel-bounded) FIXPOINT, re-unfolding after each round. One
    // rewrite often exposes a term a SECOND rewrite must fire on — `rev (rev t)`
    // (which a cited involution then matches) only appears after the distribution
    // law has fired twice, and a deeper `qrev t [h]` only after the accumulator
    // unfolds another step. Applying each rule once (the old behaviour) missed
    // those, so the engine produced no instantiation and the law fell to the
    // ladder. Dedup by structural equality keeps the closure from exploding.
    let mut pool: Vec<Spanned<Expr>> = vec![lhs0.clone(), rhs0.clone()];
    let mut frontier: Vec<Spanned<Expr>> = pool.clone();
    for _ in 0..POOL_CLOSURE_FUEL {
        let mut next: Vec<Spanned<Expr>> = Vec::new();
        for base in &frontier {
            for r in &rules {
                let rewritten = unfold_fix(&apply_rule_all(base, r), &cone);
                if !expr_eq(&rewritten, base)
                    && !pool.iter().any(|p| expr_eq(p, &rewritten))
                    && !next.iter().any(|p| expr_eq(p, &rewritten))
                {
                    next.push(rewritten);
                }
            }
        }
        if next.is_empty() {
            break;
        }
        pool.extend(next.iter().cloned());
        frontier = next;
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
    // A tail-recursive fn body lowers its self/peer call to `TailCall` — the cone
    // collector must see that target too, or the accumulator fn never joins the
    // cone and never unfolds.
    if let Expr::TailCall(data) = &e.node {
        out.insert(data.target.clone());
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
        Expr::TailCall(data) => data.args.iter().collect(),
        Expr::List(items) | Expr::Tuple(items) => items.iter().collect(),
        Expr::Constructor(_, Some(inner)) => vec![inner.as_ref()],
        Expr::Attr(inner, _) => vec![inner.as_ref()],
        Expr::BinOp(_, l, r) => vec![l.as_ref(), r.as_ref()],
        Expr::Neg(inner) | Expr::ErrorProp(inner) => vec![inner.as_ref()],
        _ => Vec::new(),
    }
}

/// Rebuild `e` with each direct child replaced by `f(child)`. A `TailCall` is
/// rebuilt as the equivalent `FnCall` (a tail call IS a call) — that both lets
/// `f` substitute into its arguments (`map_children` is otherwise opaque to the
/// `TailCall` payload, leaving the lowered `xs`/`acc` slots un-substituted) and
/// normalises it so the unfolder and matcher see a uniform `FnCall` shape.
fn map_children(
    e: &Spanned<Expr>,
    f: &mut impl FnMut(&Spanned<Expr>) -> Spanned<Expr>,
) -> Spanned<Expr> {
    let node = match &e.node {
        Expr::FnCall(callee, args) => {
            Expr::FnCall(Box::new(f(callee)), args.iter().map(&mut *f).collect())
        }
        Expr::TailCall(data) => Expr::FnCall(
            Box::new(ident(&data.target)),
            data.args.iter().map(&mut *f).collect(),
        ),
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
/// to a fixpoint (bounded by fuel), simplifying builtin nil-concatenations after
/// each round.
fn unfold_fix(e: &Spanned<Expr>, cone: &BTreeMap<String, &FnDef>) -> Spanned<Expr> {
    let mut cur = simplify_concat_nil(e);
    for _ in 0..UNFOLD_FUEL {
        let (next, changed) = unfold_once(&cur, cone);
        cur = simplify_concat_nil(&next);
        if !changed {
            break;
        }
    }
    cur
}

/// Simplify builtin nil-concatenations to a fixpoint: `List.concat(a, [])` → `a`
/// and `List.concat([], a)` → `a`. Unfolding an accumulator recursion threads
/// `List.concat([head], acc)` into the accumulator, so a step that started from
/// `[]` leaves junk like `List.concat([head], [])`; without this the engine
/// matches a cited law at the UN-normalised `List.concat([head], [])` instead of
/// `[head]`, producing a useless (and simp-looping) instantiation. Builtin concat
/// has `[]` as a two-sided identity, so this is sound; it touches only the
/// builtin, never a user `append` (whose nil law is a cited theorem, not a
/// rewrite the engine may assume).
fn simplify_concat_nil(e: &Spanned<Expr>) -> Spanned<Expr> {
    let mut f = |c: &Spanned<Expr>| simplify_concat_nil(c);
    let mapped = map_children(e, &mut f);
    if let Expr::FnCall(callee, args) = &mapped.node
        && expr_to_dotted_name(&callee.node).as_deref() == Some("List.concat")
        && args.len() == 2
    {
        let is_nil = |x: &Spanned<Expr>| matches!(&x.node, Expr::List(items) if items.is_empty());
        if is_nil(&args[0]) {
            return args[1].clone();
        }
        if is_nil(&args[1]) {
            return args[0].clone();
        }
    }
    mapped
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

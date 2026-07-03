//! Lemma-Calc: mechanically CALCULATE the forced auxiliary lemma from a stuck
//! `--explain` residual goal ([`UntranslatedGoal`]). A pure function of the goal
//! and the induction hypothesis — no enumeration, no search, no solver. Every
//! step is deterministic; anything that is not forced is an HONEST DECLINE
//! ([`CalcVerdict::Decline`]) with a named reason. The result feeds the same VM
//! sample-check + candidate renderer as the raw residual (`--explain` in
//! `src/main/commands.rs`); on decline the caller falls back to the raw
//! candidate, so the calculator only ever ADDS a stronger lemma, never removes
//! information.
//!
//! Operations (in order), from the procedure trace in the G2b design:
//!   1. `subst` — apply the induction hypothesis ONCE, left-to-right, with an
//!      anti-loop guard. An IH `l = r` whose `l` is an exact subtree of the claim
//!      rewrites every `l` to `r` and is consumed (the claim becomes IH-free).
//!   2. `lift (a)` — anti-unify the claim against a surviving IH premise: lift
//!      each maximal common subterm to a fresh variable, allowing the claim side
//!      to carry ONE extra constructor (`len ys` vs `Succ (len ys)`). Every diff
//!      point must be a common subterm or such a constructor-wrap, else decline
//!      (a lemma that needs a term outside the goal and IH is not forced).
//!   3. `lift (b)` — lift each maximal user-function-application subterm that
//!      occurs on BOTH sides of the claim to a fresh variable (typed by the
//!      function's declared return type). Correspondence-preserving: a subterm on
//!      only one side is left literal (the calculator stops at the literal
//!      remainder; further generalization is The Method's job).
//!
//! Identity note (identity_guardrails): a pure `ast::Expr` transform. It keys on
//! the claim's SHAPE (constructor-wrap, function-application head), never on
//! function or domain names; the constructor / function names it reads are DATA
//! from the program, threaded through [`CalcEnv`]. Holds no `FnId`/`TypeId` table.

use std::collections::{BTreeMap, HashSet};

use crate::ast::{BinOp, Expr, Spanned, TopLevel};

use super::untranslate::UntranslatedGoal;

/// The calculator's verdict: a forced lemma (a rewritten goal ready for the
/// candidate builder), or an honest decline naming why the lemma is not forced.
#[derive(Debug, Clone)]
pub enum CalcVerdict {
    Lemma(Box<UntranslatedGoal>),
    Decline(String),
}

/// Program facts the calculator reads as DATA: the constructor names (so a
/// constructor-wrap is distinguished from a function application) and each
/// function's declared return type (to type a lifted variable). Shape-keyed
/// machinery, name-blind: these are looked up, never matched against.
pub struct CalcEnv {
    ctors: HashSet<String>,
    fn_ret: BTreeMap<String, String>,
}

impl CalcEnv {
    pub fn from_items(items: &[TopLevel]) -> Self {
        let mut ctors = HashSet::new();
        let mut fn_ret = BTreeMap::new();
        for it in items {
            match it {
                TopLevel::TypeDef(crate::ast::TypeDef::Sum { name, variants, .. }) => {
                    for v in variants {
                        ctors.insert(v.name.clone());
                        ctors.insert(format!("{name}.{}", v.name));
                    }
                }
                TopLevel::FnDef(f) => {
                    fn_ret.insert(f.name.clone(), f.return_type.clone());
                }
                _ => {}
            }
        }
        CalcEnv { ctors, fn_ret }
    }

    fn is_ctor(&self, name: &str) -> bool {
        self.ctors.contains(name)
            || name
                .rsplit_once('.')
                .is_some_and(|(_, s)| self.ctors.contains(s))
    }

    /// The declared return type of `name`, if it is a program function.
    fn fn_return(&self, name: &str) -> Option<&str> {
        self.fn_ret.get(name).map(String::as_str)
    }
}

/// Calculate the forced lemma for `goal`, or decline. Mutates a clone; the input
/// is untouched (the caller keeps the raw residual for the decline fallback).
/// `reserved` names are kept off the fresh-variable stream in addition to the
/// goal's own binders: the candidate builder resolves givens by name against the
/// PARENT law, so a lifted variable that collides with a parent given would clone
/// the wrong sample domain (name capture). The caller passes the parent law's
/// given names.
pub fn calculate(
    goal: &UntranslatedGoal,
    env: &CalcEnv,
    reserved: &HashSet<String>,
) -> CalcVerdict {
    // Aver `when` is a single Bool expression; a residual with several surviving
    // premises is not a single forced lemma (the candidate builder declines it
    // too, but naming it here keeps the verdict on the calculator's surface).
    if goal.premises.len() > 1 {
        return CalcVerdict::Decline(format!(
            "{} surviving premises (a `when` is one Bool expression)",
            goal.premises.len()
        ));
    }
    let mut g = goal.clone();

    // 1. subst: apply the IH once, L->R.
    let substituted = subst_ih_once(&mut g);

    // 2a. lift (a): anti-unify the claim against a surviving IH premise.
    if let Some(prem) = g.premises.first().cloned() {
        match lift_antiunify(&mut g, &prem, env, reserved) {
            Ok(()) => {}
            Err(reason) => return CalcVerdict::Decline(reason),
        }
    }

    // 2b. lift (b): generalize maximal both-sides user-function subterms.
    let lifted_b = lift_opaque_subterms(&mut g, env, reserved);

    rebuild_givens(&mut g, goal);

    // A residual that neither substituted, anti-unified, nor lifted is the goal
    // itself — an executor gap in the engine, not a missing lemma.
    if !substituted && !lifted_b && g.claim == goal.claim && g.premises == goal.premises {
        return CalcVerdict::Decline(
            "residual is the parent claim itself (no decomposition — an executor gap)".to_string(),
        );
    }
    CalcVerdict::Lemma(Box::new(g))
}

// ---------------------------------------------------------------- subst -------

/// Apply a single IH premise `l = r` left-to-right if `l` is an exact subtree of
/// the claim and `r` does not contain `l` (anti-loop). Rewrites every `l` to `r`
/// and consumes the premise. Returns whether it applied.
fn subst_ih_once(g: &mut UntranslatedGoal) -> bool {
    let Some(prem) = g.premises.first() else {
        return false;
    };
    let Expr::BinOp(BinOp::Eq, l, r) = &prem.node else {
        return false;
    };
    let (l, r) = (l.node.clone(), r.node.clone());
    let in_claim = contains_subtree(&g.claim.0.node, &l) || contains_subtree(&g.claim.1.node, &l);
    if !in_claim || contains_subtree(&r, &l) {
        return false;
    }
    substitute(&mut g.claim.0.node, &l, &r);
    substitute(&mut g.claim.1.node, &l, &r);
    g.premises.clear();
    true
}

// -------------------------------------------------------------- lift (a) ------

/// Anti-unify the claim against a surviving IH premise (both `<expr> = <rhs>`
/// shapes), lifting maximal common subterms to fresh variables and allowing the
/// claim to carry one extra unary constructor. On success the generalization is
/// applied to BOTH the claim and the premise; a diff point that is neither a
/// common subterm nor a constructor-wrap declines.
fn lift_antiunify(
    g: &mut UntranslatedGoal,
    prem: &Spanned<Expr>,
    env: &CalcEnv,
    reserved: &HashSet<String>,
) -> Result<(), String> {
    // The premise is `<pl> = <pr>` (an IH equality untranslated to `pl == pr`).
    let Expr::BinOp(BinOp::Eq, pl, pr) = &prem.node else {
        return Ok(()); // not an equality premise — nothing to anti-unify
    };
    let mut existing: HashSet<String> = g.givens.iter().map(|(n, _)| n.clone()).collect();
    existing.extend(reserved.iter().cloned());
    let mut fresh = FreshVars::new(existing);
    let mut subst: Vec<(Expr, String)> = Vec::new();

    // Anti-unify the two equality sides pairwise. Both must align structurally
    // (same claim/premise shape); a mismatch that is not a constructor-wrap is a
    // diff requiring an invented term — decline.
    let new_lhs = antiunify(&g.claim.0.node, &pl.node, env, &mut subst, &mut fresh)?;
    let new_rhs = antiunify(&g.claim.1.node, &pr.node, env, &mut subst, &mut fresh)?;
    if subst.is_empty() {
        return Ok(()); // no common structure lifted — leave as-is
    }
    g.claim.0.node = new_lhs;
    g.claim.1.node = new_rhs;
    // Generalize the premise consistently: replace each lifted concrete subterm.
    let mut new_prem = prem.node.clone();
    for (concrete, var) in &subst {
        substitute(&mut new_prem, concrete, &Expr::Ident(var.clone()));
    }
    g.premises = vec![Spanned::new(new_prem, 0)];
    // Record the fresh givens (type = the head function's return type).
    for (concrete, var) in &subst {
        let ty = lifted_type(concrete, env)
            .ok_or_else(|| format!("cannot type the lifted variable `{var}`"))?;
        g.givens.push((var.clone(), ty));
    }
    Ok(())
}

/// The anti-unifier of a claim node and a premise node. Equal subtrees lift to a
/// shared fresh variable; a claim `Ctor(inner)` whose `inner` anti-unifies with
/// the premise carries the constructor over the lifted result; same head + arity
/// recurses; anything else is a diff that needs an invented term (`Err`).
fn antiunify(
    claim: &Expr,
    prem: &Expr,
    env: &CalcEnv,
    subst: &mut Vec<(Expr, String)>,
    fresh: &mut FreshVars,
) -> Result<Expr, String> {
    if claim == prem {
        // Only lift a compound common subterm — a bare literal / variable shared
        // by both sides is not a generalization target (it would rename a
        // constant), so keep it literal.
        if is_liftable_atom_or_call(claim, env) {
            return Ok(Expr::Ident(var_for(subst, claim, fresh)));
        }
        return Ok(claim.clone());
    }
    // Constructor-wrap: the claim has one extra unary constructor the IH lacks
    // (`Succ (len ys)` vs `len ys`). Peel it, anti-unify the inner, rewrap.
    if let Some((ctor, inner)) = unary_ctor_app(claim, env) {
        let g_inner = antiunify(inner, prem, env, subst, fresh)?;
        return Ok(rewrap_ctor(ctor, g_inner));
    }
    // Same application head and arity → recurse pairwise.
    if let (Some((h1, a1)), Some((h2, a2))) = (as_call(claim), as_call(prem))
        && h1 == h2
        && a1.len() == a2.len()
    {
        let mut args = Vec::with_capacity(a1.len());
        for (c, p) in a1.iter().zip(a2.iter()) {
            args.push(Spanned::new(
                antiunify(&c.node, &p.node, env, subst, fresh)?,
                0,
            ));
        }
        return Ok(Expr::FnCall(
            Box::new(Spanned::new(Expr::Ident(h1.to_string()), 0)),
            args,
        ));
    }
    // Same binary operator → recurse on both operands.
    if let (Expr::BinOp(o1, l1, r1), Expr::BinOp(o2, l2, r2)) = (claim, prem)
        && o1 == o2
    {
        let l = antiunify(&l1.node, &l2.node, env, subst, fresh)?;
        let r = antiunify(&r1.node, &r2.node, env, subst, fresh)?;
        return Ok(Expr::BinOp(
            *o1,
            Box::new(Spanned::new(l, 0)),
            Box::new(Spanned::new(r, 0)),
        ));
    }
    Err("anti-unification diff needs a term outside the goal and IH".to_string())
}

// -------------------------------------------------------------- lift (b) ------

/// Lift each maximal user-function-application subterm that occurs on BOTH sides
/// of the claim to a fresh variable (correspondence-preserving generalization).
/// A subterm on only one side is left literal. Returns whether anything lifted.
fn lift_opaque_subterms(
    g: &mut UntranslatedGoal,
    env: &CalcEnv,
    reserved: &HashSet<String>,
) -> bool {
    // Candidate fn-app subterms on each side (excluding the whole side itself:
    // lifting `f(x) = y` to `v = y` is not a decomposition).
    let mut lhs: Vec<Expr> = Vec::new();
    collect_fn_apps(&g.claim.0.node, env, true, &mut lhs);
    let mut rhs: Vec<Expr> = Vec::new();
    collect_fn_apps(&g.claim.1.node, env, true, &mut rhs);

    // Subterms occurring on BOTH sides (deduped), then the MAXIMAL of those
    // (drop any that is contained in another both-sides subterm).
    let mut both: Vec<Expr> = Vec::new();
    for e in &lhs {
        if rhs.iter().any(|r| r == e) && !both.iter().any(|b| b == e) {
            both.push(e.clone());
        }
    }
    let maximal: Vec<Expr> = both
        .iter()
        .filter(|e| {
            !both
                .iter()
                .any(|other| other != *e && contains_subtree(other, e))
        })
        .cloned()
        .collect();

    let mut existing: HashSet<String> = g.givens.iter().map(|(n, _)| n.clone()).collect();
    existing.extend(reserved.iter().cloned());
    let mut fresh = FreshVars::new(existing);
    let mut lifted = false;
    for sub in maximal {
        let Some(ty) = lifted_type(&sub, env) else {
            continue; // untypable head → leave literal
        };
        let var = fresh.next();
        substitute(&mut g.claim.0.node, &sub, &Expr::Ident(var.clone()));
        substitute(&mut g.claim.1.node, &sub, &Expr::Ident(var.clone()));
        for p in &mut g.premises {
            substitute(&mut p.node, &sub, &Expr::Ident(var.clone()));
        }
        g.givens.push((var, ty));
        lifted = true;
    }
    lifted
}

/// Collect EVERY user-function-application subterm of `e` (nested included, so a
/// deeper subterm shared across sides is found even when it sits inside a bigger
/// fn-app on one side). When `top` is true the whole `e` is skipped (only proper
/// subterms), so a claim side that is itself `f(...)` is not lifted wholesale.
/// Maximality is decided later, over the both-sides intersection.
fn collect_fn_apps(e: &Expr, env: &CalcEnv, top: bool, out: &mut Vec<Expr>) {
    if !top && is_user_fn_call(e, env) {
        out.push(e.clone());
    }
    for c in children(e) {
        collect_fn_apps(&c.node, env, false, out);
    }
}

// ------------------------------------------------------------- helpers --------

/// A monotonic fresh-variable source (`a`, `b`, …, `a1`, `b1`, …) skipping any
/// name already bound in the goal.
struct FreshVars {
    used: HashSet<String>,
    idx: usize,
}

impl FreshVars {
    fn new(used: HashSet<String>) -> Self {
        FreshVars { used, idx: 0 }
    }

    fn next(&mut self) -> String {
        loop {
            let letter = (b'a' + (self.idx % 26) as u8) as char;
            let round = self.idx / 26;
            let name = if round == 0 {
                letter.to_string()
            } else {
                format!("{letter}{round}")
            };
            self.idx += 1;
            if !self.used.contains(&name) {
                self.used.insert(name.clone());
                return name;
            }
        }
    }
}

/// The fresh variable already assigned to `subterm`, or a new one.
fn var_for(subst: &mut Vec<(Expr, String)>, subterm: &Expr, fresh: &mut FreshVars) -> String {
    if let Some((_, v)) = subst.iter().find(|(e, _)| e == subterm) {
        return v.clone();
    }
    let v = fresh.next();
    subst.push((subterm.clone(), v.clone()));
    v
}

/// `FnCall(Ident(name), [arg])` where `name` is a UNARY constructor → `(name, arg)`.
fn unary_ctor_app<'a>(e: &'a Expr, env: &CalcEnv) -> Option<(&'a str, &'a Expr)> {
    if let Expr::FnCall(head, args) = e
        && args.len() == 1
        && let Expr::Ident(name) = &head.node
        && env.is_ctor(name)
    {
        return Some((name.as_str(), &args[0].node));
    }
    None
}

fn rewrap_ctor(ctor: &str, inner: Expr) -> Expr {
    Expr::FnCall(
        Box::new(Spanned::new(Expr::Ident(ctor.to_string()), 0)),
        vec![Spanned::new(inner, 0)],
    )
}

/// `FnCall(Ident(name), args)` → `(name, args)` regardless of what `name` is.
fn as_call(e: &Expr) -> Option<(&str, &[Spanned<Expr>])> {
    if let Expr::FnCall(head, args) = e
        && let Expr::Ident(name) = &head.node
    {
        return Some((name.as_str(), args.as_slice()));
    }
    None
}

/// A `FnCall` whose head is a PROGRAM function (not a constructor) — the lift(b)
/// target. `List.concat` and other builtins are not program functions, so they
/// stay structural.
fn is_user_fn_call(e: &Expr, env: &CalcEnv) -> bool {
    matches!(as_call(e), Some((name, _)) if !env.is_ctor(name) && lifted_head(name, env).is_some())
}

/// A compound term worth lifting as a common subterm in anti-unification: a
/// call (fn or ctor) — not a bare literal or variable (renaming a constant is
/// not a generalization).
fn is_liftable_atom_or_call(e: &Expr, env: &CalcEnv) -> bool {
    match e {
        Expr::FnCall(..) => lifted_type(e, env).is_some(),
        _ => false,
    }
}

/// The Aver type of a lifted subterm: the return type of its head function.
fn lifted_type(e: &Expr, env: &CalcEnv) -> Option<String> {
    let (name, _) = as_call(e)?;
    lifted_head(name, env).map(str::to_string)
}

fn lifted_head<'a>(name: &str, env: &'a CalcEnv) -> Option<&'a str> {
    // A module-qualified name spells its final segment as the fn.
    env.fn_return(name)
        .or_else(|| name.rsplit_once('.').and_then(|(_, s)| env.fn_return(s)))
}

/// The direct sub-expressions of `e` (for structural recursion over the
/// un-translator's Expr image).
fn children(e: &Expr) -> Vec<&Spanned<Expr>> {
    match e {
        Expr::FnCall(head, args) => {
            let mut v = vec![head.as_ref()];
            v.extend(args.iter());
            v
        }
        Expr::BinOp(_, a, b) => vec![a.as_ref(), b.as_ref()],
        Expr::Neg(a) => vec![a.as_ref()],
        Expr::List(xs) | Expr::Tuple(xs) => xs.iter().collect(),
        Expr::Attr(b, _) => vec![b.as_ref()],
        _ => vec![],
    }
}

/// True iff `needle` is `hay` or an exact subtree of it (span-agnostic — the
/// `Spanned` `PartialEq` compares nodes only).
fn contains_subtree(hay: &Expr, needle: &Expr) -> bool {
    hay == needle
        || children(hay)
            .iter()
            .any(|c| contains_subtree(&c.node, needle))
}

/// Replace every subtree structurally equal to `from` with `to`.
fn substitute(e: &mut Expr, from: &Expr, to: &Expr) {
    if e == from {
        *e = to.clone();
        return;
    }
    match e {
        Expr::FnCall(head, args) => {
            substitute(&mut head.node, from, to);
            for a in args {
                substitute(&mut a.node, from, to);
            }
        }
        Expr::BinOp(_, a, b) => {
            substitute(&mut a.node, from, to);
            substitute(&mut b.node, from, to);
        }
        Expr::Neg(a) => substitute(&mut a.node, from, to),
        Expr::List(xs) | Expr::Tuple(xs) => {
            for x in xs {
                substitute(&mut x.node, from, to);
            }
        }
        Expr::Attr(b, _) => substitute(&mut b.node, from, to),
        _ => {}
    }
}

/// Drop givens no longer referenced after lifting; keep survivors in their
/// original order, then the fresh lifted variables (already appended in order).
fn rebuild_givens(g: &mut UntranslatedGoal, original: &UntranslatedGoal) {
    let mut used = HashSet::new();
    collect_idents(&g.claim.0.node, &mut used);
    collect_idents(&g.claim.1.node, &mut used);
    for p in &g.premises {
        collect_idents(&p.node, &mut used);
    }
    let orig_names: HashSet<&String> = original.givens.iter().map(|(n, _)| n).collect();
    g.givens.retain(|(n, _)| used.contains(n));
    // Preserve declaration order: originals first (as they were), then lifted.
    g.givens.sort_by_key(|(n, _)| !orig_names.contains(n));
}

fn collect_idents(e: &Expr, out: &mut HashSet<String>) {
    if let Expr::Ident(n) = e {
        out.insert(n.clone());
    }
    for c in children(e) {
        collect_idents(&c.node, out);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::unparse;
    use crate::codegen::lean::untranslate::{PeanoCtx, UntranslateCtx, untranslate_goal_ctx};

    fn render(g: &UntranslatedGoal) -> String {
        let mut out = String::new();
        for (n, t) in &g.givens {
            out.push_str(&format!("given {n}: {t}; "));
        }
        for p in &g.premises {
            let mut buf = String::new();
            unparse::write_expr_public(&mut buf, p, 0).unwrap();
            out.push_str(&format!("when {buf}; "));
        }
        let mut l = String::new();
        let mut r = String::new();
        unparse::write_expr_public(&mut l, &g.claim.0, 0).unwrap();
        unparse::write_expr_public(&mut r, &g.claim.1, 0).unwrap();
        out.push_str(&format!("{l} => {r}"));
        out
    }

    fn nat_env() -> CalcEnv {
        // `type Nat { Z; S(Nat) }` + `fn len(...) -> Nat`, `fn le(...) -> Bool`,
        // `fn filterZ(...) -> List<Nat>`, `fn rev(...) -> List<Nat>`.
        let src = "type Nat\n    Z\n    S(Nat)\n\n\
                   fn len(xs: List<Nat>) -> Nat\n    Nat.Z\n\n\
                   fn le(x: Nat, y: Nat) -> Bool\n    true\n\n\
                   fn filterZ(xs: List<Nat>) -> List<Nat>\n    xs\n\n\
                   fn rev(xs: List<Nat>) -> List<Nat>\n    xs\n";
        CalcEnv::from_items(&crate::source::parse_source(src).expect("parses"))
    }

    fn peano_nat() -> UntranslateCtx {
        UntranslateCtx {
            peano: Some(PeanoCtx {
                type_name: "Nat".to_string(),
                zero_ctor: "Z".to_string(),
                succ_ctor: "S".to_string(),
            }),
        }
    }

    const DUMP_P66_1: &str = include_str!("testdata/lemma_calc_krok0/p66_1.json");

    #[test]
    fn antiunify_forces_the_prop_66_successor_lemma() {
        // The distinctive G2b case: constructor-on-a-variable. p66_1 un-translates
        // to `when le(len(filterZ tail), len tail); le(len(filterZ tail),
        // S(len tail)) => true`; the IH does not subst (its `le` is not a subtree
        // of the claim's `le`), so anti-unification lifts `len(filterZ tail) -> a`,
        // `len tail -> b`, allowing the claim's extra `S`.
        let goal = untranslate_goal_ctx(DUMP_P66_1, &peano_nat()).expect("in grammar");
        let CalcVerdict::Lemma(l) = calculate(&goal, &nat_env(), &HashSet::new()) else {
            panic!("expected a forced lemma");
        };
        assert_eq!(
            render(&l),
            "given a: Nat; given b: Nat; when (le(a, b) == true); le(a, Nat.S(b)) => true"
        );
    }

    #[test]
    fn fresh_vars_skip_parent_law_given_names() {
        // NAME-CAPTURE guard: the fresh lifted variables must dedup against the
        // PARENT law's given names, not only the residual's own binders. The
        // candidate builder resolves givens by name against the parent law, so a
        // lifted `a` colliding with a parent given `a` would clone the wrong
        // sample domain. Reserving `a` forces the lifted variables to `b`, `c`.
        let goal = untranslate_goal_ctx(DUMP_P66_1, &peano_nat()).expect("in grammar");
        let reserved: HashSet<String> = ["a".to_string()].into_iter().collect();
        let CalcVerdict::Lemma(l) = calculate(&goal, &nat_env(), &reserved) else {
            panic!("expected a forced lemma");
        };
        assert_eq!(
            render(&l),
            "given b: Nat; given c: Nat; when (le(b, c) == true); le(b, Nat.S(c)) => true"
        );
    }

    #[test]
    fn subst_consumes_ih_and_lifts_snoc_general_lemma() {
        // prop_73 cons branch (hand-built to the shape the real probe emits): IH
        // `rev(filterZ tail) = filterZ(rev tail)` substs into the claim
        // `rev(filterZ tail) ++ [Z] = filterZ(rev tail ++ [Z])`, then `rev tail`
        // (both sides) lifts to a fresh `xs`, giving the general snoc lemma.
        let concat = |a: &str, b: &str| {
            format!(r#"{{"app":{{"fn":{{"const":"List.append"}},"args":[{a},{b}]}}}}"#)
        };
        let rev = |a: &str| format!(r#"{{"app":{{"fn":{{"const":"rev"}},"args":[{a}]}}}}"#);
        let filterz = |a: &str| format!(r#"{{"app":{{"fn":{{"const":"filterZ"}},"args":[{a}]}}}}"#);
        let tail = r#"{"var":"tail"}"#;
        let zlist = r#"{"app":{"fn":{"const":"List.cons"},"args":[{"const":"Nat"},{"app":{"fn":{"const":"OfNat.ofNat"},"args":[{"const":"Nat"},{"nat":"0"},{"opaque":"i"}]}},{"app":{"fn":{"const":"List.nil"},"args":[{"const":"Nat"}]}}]}}"#;
        let list_nat = r#"{"app":{"fn":{"const":"List"},"args":[{"const":"Nat"}]}}"#;
        let ih = format!(
            r#"{{"app":{{"fn":{{"const":"Eq"}},"args":[{list_nat},{},{}]}}}}"#,
            rev(&filterz(tail)),
            filterz(&rev(tail))
        );
        let claim = format!(
            r#"{{"app":{{"fn":{{"const":"Eq"}},"args":[{list_nat},{},{}]}}}}"#,
            concat(&rev(&filterz(tail)), zlist),
            filterz(&concat(&rev(tail), zlist))
        );
        let json = format!(
            r#"{{"forall":{{"name":"tail","ty":{list_nat},"body":{{"forall":{{"name":"ih","ty":{ih},"body":{claim}}}}}}}}}"#
        );
        let goal = untranslate_goal_ctx(&json, &peano_nat()).expect("in grammar");
        let CalcVerdict::Lemma(l) = calculate(&goal, &nat_env(), &HashSet::new()) else {
            panic!("expected a forced lemma");
        };
        // IH consumed (no `when`), `rev tail` generalized to `a`.
        let r = render(&l);
        assert!(!r.contains("when"), "IH should be substituted away: {r}");
        assert!(r.contains("given a: List<Nat>"), "{r}");
        assert_eq!(
            r,
            "given a: List<Nat>; List.concat(filterZ(a), List.concat([Nat.Z], [])) \
             => filterZ(List.concat(a, List.concat([Nat.Z], [])))"
        );
    }

    #[test]
    fn declines_when_diff_is_not_common_or_ctor_wrap() {
        // A claim/IH pair whose difference is a genuinely different term (not a
        // shared subterm, not a constructor-wrap) is NOT forced — decline.
        let f = |a: &str| format!(r#"{{"app":{{"fn":{{"const":"le"}},"args":[{a},{a}]}}}}"#);
        let ih = format!(
            r#"{{"app":{{"fn":{{"const":"Eq"}},"args":[{{"const":"Bool"}},{},{{"const":"Bool.true"}}]}}}}"#,
            f(r#"{"var":"x"}"#)
        );
        let claim = format!(
            r#"{{"app":{{"fn":{{"const":"Eq"}},"args":[{{"const":"Bool"}},{},{{"const":"Bool.true"}}]}}}}"#,
            f(r#"{"var":"y"}"#)
        );
        let list_nat = r#"{"app":{"fn":{"const":"List"},"args":[{"const":"Nat"}]}}"#;
        let json = format!(
            r#"{{"forall":{{"name":"x","ty":{{"const":"Nat"}},"body":{{"forall":{{"name":"ih","ty":{ih},"body":{claim}}}}}}}}}"#
        );
        let _ = list_nat;
        let goal = untranslate_goal_ctx(&json, &peano_nat()).expect("in grammar");
        match calculate(&goal, &nat_env(), &HashSet::new()) {
            CalcVerdict::Decline(reason) => {
                assert!(
                    reason.contains("outside") || reason.contains("term"),
                    "{reason}"
                );
            }
            CalcVerdict::Lemma(l) => panic!("should decline, got {}", render(&l)),
        }
    }
}

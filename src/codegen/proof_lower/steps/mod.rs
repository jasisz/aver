//! Step producers: the rungs that recognised a law's shape write its proof
//! as data (`crate::ir::proof_steps`) instead of leaving a backend to search.
//!
//! Producers, tried in order; a script is kept only once the kernel
//! written in Aver ([`crate::proof_kernel::verdict`]) accepts it:
//! - **algebra**: a pinned `Commutative` / `Associative` / `IdentityElement`
//!   law: open the wrapper on both sides, close with one ring rule;
//! - **citations**: a law with an explicit `using` list: rewrite with the
//!   cited laws and the Euclidean recomposition rule to a normal form; where
//!   a cited law matched with one line of its `when` open, and no other
//!   producer closes the law, split on that line once
//!   ([`split_on_cited_when`]);
//! - **evaluation**: evaluate both sides, splitting on each undecided Bool
//!   guard, until both sides are the same term, or until the hypotheses of
//!   a case rule it out ([`hypotheses`]); where a case stays open, its
//!   hypotheses are evaluated the way the claim was and the claim is tried
//!   again under them ([`evaluated`]);
//! - **finite domains**: when evaluation alone does not close the law,
//!   split every given of finite type into all of its values and evaluate
//!   each case;
//! - **induction**: along the recursion of the function the law is about,
//!   each case closed by evaluation with its hypotheses and cited laws.
//!
//! Evaluation may rewrite with the laws a `using` list cites, left to
//! right, where it stops; a cited law stating an `Int` comparison is also a
//! fact for linear arithmetic at the calls it matches, also under a `when`
//! proved at that instance.
//!
//! A law with a `by Module.plan` line is proved by that project plan
//! instead ([`by_plan`]); none of the producers above runs for it. Under
//! `aver proof`, a law they leave open (with no `induction` or `because`
//! lines) is tried with each plan of the plans modules other `by` lines
//! name, only for a hint naming the `by` line that would close it.
//!
//! A law left open is also proved again with the facts and earlier laws
//! that match where it stopped added to its `using` line, only for a hint
//! naming the line it closes with ([`using_hint`]). When no hint closes it,
//! the lemmas stated from where the steps stopped that close it are hinted
//! instead ([`lemma`]).
//!
//! A law no producer handles keeps `steps: None` and its tactic portfolio.

mod accumulator;
mod by_plan;
mod chain;
mod decls;
mod divisor;
mod egraph;
mod env;
mod eval;
mod evaluated;
mod finite;
mod hypotheses;
mod induction;
mod lemma;
mod memo;
mod nonlinear;
mod rewrite;
mod split;

use crate::codegen::proof_lower::ProofLowerInputs;
use crate::ir::hir::{ResolvedCallee, ResolvedExpr};
use crate::ir::proof_steps::term::{Term, canon};
use crate::ir::proof_steps::{Eqn, LawRef, Obligation, Proof, Script, WallRule};
use crate::ir::{LawTheorem, ProofIR, ProofStrategy};

use env::Env;
use rewrite::Equation;

/// Split depth of the evaluation producer: at most 2^6 leaves.
const SPLIT_DEPTH: usize = 6;
/// Split depth of the attempt made again where the first ran out of fuel.
const SHALLOW_DEPTH: usize = 2;
/// Largest proof the producers hand to a backend.
const MAX_PROOF_NODES: usize = 3000;

fn law_key(inputs: &ProofLowerInputs, t: &LawTheorem) -> String {
    let key = &inputs.symbol_table.fn_entry(t.fn_id).key;
    match key.scope_str() {
        Some(scope) => format!("{scope}.{}.{}", key.name, t.law_name),
        None => format!("{}.{}", key.name, t.law_name),
    }
}

/// Whether `text` is [`law_key`] of `t`, without writing the key out.
fn is_law_key(inputs: &ProofLowerInputs, t: &LawTheorem, text: &str) -> bool {
    let key = &inputs.symbol_table.fn_entry(t.fn_id).key;
    match key.scope_str() {
        Some(scope) => parts_eq(&[scope, ".", &key.name, ".", &t.law_name], text),
        None => parts_eq(&[&key.name, ".", &t.law_name], text),
    }
}

/// Whether the text `parts` written one after the other is `text`.
fn parts_eq(parts: &[&str], text: &str) -> bool {
    let mut rest = text;
    for part in parts {
        let Some(after) = rest.strip_prefix(part) else {
            return false;
        };
        rest = after;
    }
    rest.is_empty()
}

fn law_ref(inputs: &ProofLowerInputs, ir: &ProofIR, t: &LawTheorem) -> Result<LawRef, String> {
    let (premise, refined) = assumed(inputs, ir, t)?;
    Ok(LawRef {
        key: law_key(inputs, t),
        givens: t.quantifiers.iter().map(|q| q.name.clone()).collect(),
        types: finite::given_types(inputs, t),
        premise,
        refined,
        lhs: law_term(inputs, t, &t.claim_lhs),
        rhs: law_term(inputs, t, &t.claim_rhs),
        fact: None,
    })
}

/// A term of law `t`, with its module's binding reads spelled as every
/// step term spells them (`env::qualify_bindings`). A given hides a
/// module-level binding of the same name.
fn law_term(inputs: &ProofLowerInputs, t: &LawTheorem, term: &Term) -> Term {
    let scope = inputs.symbol_table.fn_entry(t.fn_id).key.scope_str();
    let givens: Vec<String> = t.quantifiers.iter().map(|q| q.name.clone()).collect();
    env::qualify_bindings(&canon(term), inputs, scope, &givens)
}

fn premise_of(inputs: &ProofLowerInputs, t: &LawTheorem) -> Result<Option<Term>, String> {
    match t.premises.as_slice() {
        [] => Ok(None),
        [p] => Ok(Some(law_term(inputs, t, &p.expr))),
        _ => Err("more than one premise".into()),
    }
}

/// What the refined type of each given of law `t` holds of it, in the
/// order of the givens: the check of the type's smart constructor, read at
/// the given's field (`n.value >= 0` for `n: Natural`). The source is the
/// one the Lean export states the type from (`ProofIR::refined_types`), so
/// a given has a line here exactly when Lean gives it a type that carries
/// the proof. A given that only holds a refined value somewhere inside (in
/// a list, an `Option`, a field of another record) has no line.
fn refinements(
    inputs: &ProofLowerInputs,
    ir: &ProofIR,
    t: &LawTheorem,
) -> Result<Vec<Term>, String> {
    use crate::ir::proof_steps::term;
    let mut out = Vec::new();
    for (given, ty) in finite::given_types(inputs, t) {
        let crate::ast::Type::Named { id: Some(id), .. } = &ty else {
            continue;
        };
        let Some(decl) = ir.refined_types.get(id) else {
            continue;
        };
        let scope = inputs.symbol_table.type_entry(*id).key.scope_str();
        let holds = env::qualify_bindings(
            &canon(&decl.invariant.expr),
            inputs,
            scope,
            std::slice::from_ref(&decl.predicate_param),
        );
        let of = term::var(&given);
        of.set_ty(ty.clone());
        let read = crate::ast::Spanned::bare(ResolvedExpr::Attr(
            crate::ast::Shared::new(of),
            decl.carrier_field.clone(),
        ));
        read.set_ty(finite::qualify(
            inputs,
            &crate::types::parse_type_str(&decl.carrier_type),
            scope,
        ));
        let line = term::subst(&holds, &[(decl.predicate_param.clone(), read)])
            .map_err(|why| format!("what {} holds of `{given}`: {why}", decl.name))?;
        out.push(canon(&line));
    }
    Ok(out)
}

/// What law `t` assumes, as its scripts state it: a line per given of a
/// refined type ([`refinements`]), then its `when`, joined with `Bool.and`;
/// and those leading lines.
fn assumed(
    inputs: &ProofLowerInputs,
    ir: &ProofIR,
    t: &LawTheorem,
) -> Result<(Option<Term>, Vec<Term>), String> {
    let refined = refinements(inputs, ir, t)?;
    let mut items = refined.clone();
    items.extend(premise_of(inputs, t)?);
    Ok((conjoin(&items).map(|(whole, _)| whole), refined))
}

/// The laws a `using` list names, resolved against the citing law's
/// module. `using` is a set: the laws come back in the order they are
/// defined, whatever order the list names them in.
fn cited(
    inputs: &ProofLowerInputs,
    ir: &ProofIR,
    t: &LawTheorem,
    names: &[String],
) -> Option<Vec<LawRef>> {
    use crate::ir::proof_steps::facts;
    // Builtin facts first, in the order the facts are listed.
    let mut out: Vec<LawRef> = facts::all()
        .iter()
        .filter(|f| names.iter().any(|n| n == f.key))
        .map(facts::Fact::law_ref)
        .collect();
    if names.iter().any(|n| {
        if facts::is_fact_name(n) {
            facts::named(n).is_none()
        } else {
            !ir.law_theorems.iter().any(|c| names_law(inputs, t, c, n))
        }
    }) {
        return None;
    }
    out.extend(
        ir.law_theorems
            .iter()
            .filter(|c| names.iter().any(|n| names_law(inputs, t, c, n)))
            .map(|c| law_ref(inputs, ir, c))
            .collect::<Result<Vec<_>, _>>()
            .ok()?,
    );
    Some(out)
}

/// The keys of the laws `c`'s `using` list names (builtin facts left
/// out), resolved against `c`'s module.
fn cited_keys(inputs: &ProofLowerInputs, ir: &ProofIR, c: &LawTheorem) -> Vec<String> {
    let mut out = Vec::new();
    for name in c.using.iter().flatten() {
        for d in &ir.law_theorems {
            if names_law(inputs, c, d, name) {
                out.push(law_key(inputs, d));
            }
        }
    }
    out
}

/// Whether `name`, in the `using` list of law `t`, names law `c`: a law of
/// `t`'s own module by `fn.law` (or its full key), a law of another module
/// by `Module.fn.law`. A law of the entry is never named from a module: no
/// module depends on the entry, and its `fn.law` key is spelled like a
/// module's own law, so matching it would let a module's law cite an entry
/// law that cites it back.
fn names_law(inputs: &ProofLowerInputs, t: &LawTheorem, c: &LawTheorem, name: &str) -> bool {
    let own = &inputs.symbol_table.fn_entry(t.fn_id).key.scope;
    let key = &inputs.symbol_table.fn_entry(c.fn_id).key;
    if key.scope == *own {
        parts_eq(&[&key.name, ".", &c.law_name], name) || is_law_key(inputs, c, name)
    } else {
        key.scope.is_some() && is_law_key(inputs, c, name)
    }
}

/// Whether law `t` may cite law `c` in its `using` list, so a hint may
/// name it: `c` is of `t`'s own module, or of a module `t`'s module
/// depends on that exposes `c`'s function; and `c` does not already cite
/// `t`, directly or through other laws, which would make the citation a
/// cycle.
fn citable_from(inputs: &ProofLowerInputs, ir: &ProofIR, t: &LawTheorem, c: &LawTheorem) -> bool {
    let own = &inputs.symbol_table.fn_entry(t.fn_id).key;
    let theirs = &inputs.symbol_table.fn_entry(c.fn_id).key;
    if theirs.scope != own.scope {
        let Some(module) = theirs.scope.as_deref() else {
            return false;
        };
        let depends: Option<&[String]> = match own.scope.as_deref() {
            None => {
                crate::visibility::module_decl(inputs.entry_items).map(|m| m.depends.as_slice())
            }
            Some(prefix) => inputs
                .dep_modules
                .iter()
                .find(|m| m.prefix == prefix)
                .map(|m| m.depends.as_slice()),
        };
        if !depends.is_some_and(|d| d.iter().any(|m| m == module)) {
            return false;
        }
        let Some(info) = inputs.dep_modules.iter().find(|m| m.prefix == module) else {
            return false;
        };
        let exposes = (!info.exposes.is_empty()).then_some(info.exposes.as_slice());
        if !crate::visibility::is_exposed(&theirs.name, exposes) {
            return false;
        }
    }
    let target = law_key(inputs, t);
    let mut pending = cited_keys(inputs, ir, c);
    let mut seen: Vec<String> = Vec::new();
    while let Some(next) = pending.pop() {
        if next == target {
            return false;
        }
        if seen.contains(&next) {
            continue;
        }
        if let Some(d) = ir
            .law_theorems
            .iter()
            .find(|d| is_law_key(inputs, d, &next))
        {
            pending.extend(cited_keys(inputs, ir, d));
        }
        seen.push(next);
    }
    true
}

fn obligation(
    inputs: &ProofLowerInputs,
    ir: &ProofIR,
    t: &LawTheorem,
) -> Result<Obligation, String> {
    let (premise, refined) = assumed(inputs, ir, t)?;
    Ok(Obligation {
        key: law_key(inputs, t),
        givens: t.quantifiers.iter().map(|q| q.name.clone()).collect(),
        types: finite::given_types(inputs, t),
        finite: finite::finite_givens(inputs, t),
        lists: finite::list_givens(t),
        ints: finite::int_givens(t),
        premise,
        refined,
        lhs: law_term(inputs, t, &t.claim_lhs),
        rhs: law_term(inputs, t, &t.claim_rhs),
    })
}

/// A commutative, associative or identity-element law of an Int
/// operation: both sides unfolded once, then one `ring` step between them.
fn algebra(env: &mut Env, t: &LawTheorem, ob: &Obligation) -> Result<Proof, String> {
    if !matches!(
        t.strategy,
        ProofStrategy::Commutative { .. }
            | ProofStrategy::Associative { .. }
            | ProofStrategy::IdentityElement { .. }
    ) {
        return Err("not an algebraic strategy".into());
    }
    let unfold = [Equation::Unfold(t.fn_id)];
    let mut left = env.rewrite(&ob.lhs, &unfold)?;
    let right = env.rewrite(&ob.rhs, &unfold)?;
    if left.cur() == right.cur() {
        return Ok(chain::meet(left, right));
    }
    let int = |x: &Term| matches!(x.ty(), Some(crate::ast::Type::Int));
    let (l, r) = (canon(left.cur()), canon(right.cur()));
    if !(int(&l) && int(&r) && crate::ir::proof_steps::ring::same_polynomial(&l, &r)) {
        return Err("the unfolded sides are not the same Int polynomial".into());
    }
    left.push(
        Proof::Ring {
            lhs: l,
            rhs: r.clone(),
        },
        r,
    );
    Ok(chain::meet(left, right))
}

fn citations(env: &mut Env, laws: Vec<LawRef>, ob: &Obligation) -> Result<Proof, String> {
    // A law that would rewrite its own result is left out, with its reason
    // kept for the refusal.
    let (looping, usable): (Vec<_>, Vec<_>) =
        laws.into_iter().partition(|l| rewrite::loops(l).is_some());
    let reasons: Vec<String> = looping.iter().filter_map(rewrite::loops).collect();
    let mut eqs: Vec<Equation> = usable
        .iter()
        .cloned()
        .map(|l| Equation::Law(Box::new(l)))
        .collect();
    eqs.push(Equation::Wall(WallRule::DivModRecompose));
    eqs.extend(
        env.fact_rules
            .iter()
            .cloned()
            .map(|l| Equation::Law(Box::new(l))),
    );
    env.laws = usable;
    let with_reasons = |why: String| {
        if reasons.is_empty() {
            why
        } else {
            format!("{why} ({})", reasons.join("; "))
        }
    };
    let left = env.rewrite(&ob.lhs, &eqs).map_err(with_reasons)?;
    let right = env.rewrite(&ob.rhs, &eqs).map_err(with_reasons)?;
    if left.cur() == right.cur() {
        Ok(chain::meet(left, right))
    } else {
        let names = env.inputs.symbol_table;
        Err(env.with_open_premises(with_reasons(format!(
            "the cited laws rewrite the sides to `{}` and `{}`",
            crate::ir::proof_steps::show::term(left.cur(), names),
            crate::ir::proof_steps::show::term(right.cur(), names)
        ))))
    }
}

/// Most open `when` lines of cited laws one attempt splits on, each tried
/// alone, in the order they were met.
const MAX_WHEN_SPLITS: usize = 2;

/// `ob` by cases on `on`, the one line of a cited law's `when` that stayed
/// open where the law matched: in each case the sides are rewritten with
/// the cited laws again (with `on` true the law now applies; with `on`
/// false another may), a side that is an Int comparison is decided by
/// linear arithmetic where it can be (the cited comparisons count, at the
/// calls as the rewriting left them), and what is left is evaluated as
/// [`Env::prove_by_evaluation`] does. One level only: the cases do not
/// split on a cited `when` again, so an attempt makes at most
/// [`MAX_WHEN_SPLITS`] such splits, each into two cases, and the split is
/// on a line of the law's own `when`, never on a term searched for.
fn split_on_cited_when(
    env: &mut Env,
    laws: Vec<LawRef>,
    ob: &Obligation,
    on: &Term,
) -> Result<Proof, String> {
    let usable: Vec<LawRef> = laws
        .into_iter()
        .filter(|l| rewrite::loops(l).is_none())
        .collect();
    let mut eqs: Vec<Equation> = usable
        .into_iter()
        .map(|l| Equation::Law(Box::new(l)))
        .collect();
    eqs.push(Equation::Wall(WallRule::DivModRecompose));
    eqs.extend(
        env.fact_rules
            .iter()
            .cloned()
            .map(|l| Equation::Law(Box::new(l))),
    );
    let hyp = env.fresh_hyp();
    env.wide_linear = true;
    let case = |env: &mut Env, value: bool| -> Result<Proof, String> {
        env.hyps.push((
            hyp.clone(),
            crate::ir::proof_steps::Eqn::new(
                canon(on),
                crate::ir::proof_steps::term::boolean(value),
            ),
        ));
        let out = (|| {
            let mut left = env.rewrite(&ob.lhs, &eqs)?;
            let mut right = env.rewrite(&ob.rhs, &eqs)?;
            for side in [&mut left, &mut right] {
                if matches!(side.cur().node, crate::ir::hir::ResolvedExpr::BinOp(..))
                    && let Some((proof, value)) = env.decide_linearly(&side.cur().clone())
                {
                    side.push(proof, value);
                }
            }
            if left.cur() != right.cur() {
                let (l, r) = (left.cur().clone(), right.cur().clone());
                let proof = env.prove_by_evaluation(&l, &r, SPLIT_DEPTH - 1)?;
                left.push(proof, r);
            }
            Ok(chain::meet(left, right))
        })();
        env.hyps.pop();
        out.map_err(|why: String| format!("where `{}` is {value}: {why}", show_term(env, on)))
    };
    let if_true = case(env, true)?;
    let if_false = case(env, false)?;
    Ok(Proof::Cases {
        on: canon(on),
        hyp,
        if_true: Box::new(if_true),
        if_false: Box::new(if_false),
    })
}

fn show_term(env: &Env, t: &Term) -> String {
    crate::ir::proof_steps::show::term(t, env.inputs.symbol_table)
}

/// An environment for one attempt: the law's `when` in scope, each of its
/// lines and each reason proved so far as a hypothesis of its own, and
/// its cited laws, minus any that would loop, as rewrite rules; the
/// earlier laws it does not cite only as hints.
fn fresh_env<'a>(
    inputs: &'a ProofLowerInputs<'a>,
    ob: &Obligation,
    known: &[(String, Eqn)],
    using: &Option<Vec<LawRef>>,
    earlier: &[LawRef],
    auto: &[LawRef],
) -> Env<'a> {
    let mut env = Env::new(inputs);
    // `Module.fn.law`, with `.because<k>` or `.implication` for an
    // obligation of a `because` chain.
    let mut parts: Vec<&str> = ob.key.split('.').collect();
    if parts.last().is_some_and(|p| {
        *p == "implication"
            || p.strip_prefix("because")
                .is_some_and(|k| k.parse::<u32>().is_ok())
    }) {
        parts.pop();
    }
    env.law_scope = (parts.len() > 2).then(|| parts[..parts.len() - 2].join("."));
    env.givens = ob.givens.clone();
    env.finite = ob.finite.iter().map(|(n, _)| n.clone()).collect();
    if let Some(p) = &ob.premise {
        env.hyps.push(("when".into(), chain::eqn_true(p)));
    }
    for (name, eqn) in known {
        env.hyps.push((name.clone(), eqn.clone()));
    }
    env.rewrite_laws = using
        .iter()
        .flatten()
        .filter(|l| rewrite::loops(l).is_none())
        .cloned()
        .collect();
    env.cited_all = using.iter().flatten().cloned().collect();
    env.fact_rules = crate::ir::proof_steps::facts::normalizing()
        .iter()
        .filter(|f| !env.cited_all.iter().any(|c| c.key == f.key))
        .map(crate::ir::proof_steps::facts::Fact::law_ref)
        .collect();
    env.hint_laws = earlier.to_vec();
    env.auto_laws = auto.to_vec();
    env
}

/// The lines of a `when` written over several lines (which the parser
/// joins with `Bool.and`), each with its proof from hypothesis `when`.
/// A `when` of one line has none.
fn when_lines(premise: &Term) -> Vec<(Term, Proof)> {
    let mut out = Vec::new();
    split_and(premise, Proof::Hyp("when".into()), &mut out);
    if out.len() < 2 {
        return Vec::new();
    }
    out
}

/// The leaves of a `Bool.and` (nested) that `proof` proves true, each with
/// its proof by the eliminations.
pub(crate) fn split_and(t: &Term, proof: Proof, out: &mut Vec<(Term, Proof)>) {
    use crate::ir::hir::{ResolvedCallee, ResolvedExpr};
    if let ResolvedExpr::Call(ResolvedCallee::Builtin(b), args) = &t.node
        && b == "Bool.and"
        && args.len() == 2
    {
        let elim = |rule: WallRule| Proof::Rule {
            rule,
            subst: vec![("a".into(), canon(&args[0])), ("b".into(), canon(&args[1]))],
            premises: vec![proof.clone()],
        };
        split_and(&args[0], elim(WallRule::AndElimL), out);
        split_and(&args[1], elim(WallRule::AndElimR), out);
    } else {
        out.push((canon(t), proof));
    }
}

/// Whether `p` cuts in a `when` line stated with the values of its
/// constants.
fn cuts_used(p: &Proof) -> bool {
    (1..=64).any(|k| uses_hyp(p, &format!("when_value{k}")))
}

/// Calls `f` on each proof directly inside `p`.
fn each_part<'a>(p: &'a Proof, f: &mut dyn FnMut(&'a Proof)) {
    match p {
        Proof::Symm(q) | Proof::Congr { inner: q, .. } => f(q),
        Proof::Absurd { contradiction, .. } => f(contradiction),
        Proof::Arm { premise, .. } => f(premise),
        Proof::Unfold { premise, .. } | Proof::Law { premise, .. } => {
            if let Some(q) = premise {
                f(q);
            }
        }
        Proof::Trans { steps: all, .. }
        | Proof::Rule { premises: all, .. }
        | Proof::Enum { cases: all, .. } => all.iter().for_each(f),
        Proof::Cases {
            if_true, if_false, ..
        } => {
            f(if_true);
            f(if_false);
        }
        Proof::Split { cases, .. } => cases.iter().for_each(|c| f(&c.proof)),
        Proof::Have { proof, body, .. } => {
            f(proof);
            f(body);
        }
        Proof::Induct { cases, .. } => cases.iter().for_each(|c| {
            f(&c.proof);
            c.carry.iter().flatten().for_each(&mut *f);
        }),
        Proof::InductList { nil, cons, .. } | Proof::ListCases { nil, cons, .. } => {
            f(nil);
            f(cons);
        }
        Proof::Refl(_)
        | Proof::Hyp(_)
        | Proof::Linear { .. }
        | Proof::UnfoldConst { .. }
        | Proof::Proj { .. }
        | Proof::Cell { .. }
        | Proof::Compute { .. }
        | Proof::Ring { .. } => {}
    }
}

/// The user functions whose calls `p` splits into the true and false cases.
fn split_calls(p: &Proof, out: &mut Vec<crate::ir::identity::FnId>) {
    if let Proof::Cases { on, .. } = p
        && let ResolvedExpr::Call(ResolvedCallee::Fn(id), _) = &on.node
    {
        out.push(*id);
    }
    each_part(p, &mut |q| split_calls(q, out));
}

/// Whether `p` may read hypothesis `name` (an over-approximation: a
/// hypothesis of the same name bound inside counts too).
pub(super) fn uses_hyp(p: &Proof, name: &str) -> bool {
    let mut found = match p {
        Proof::Hyp(n) => n == name,
        Proof::Linear { hyps, .. } => hyps.iter().any(|n| n == name),
        Proof::Induct { carried, .. } => carried.iter().any(|n| n == name),
        _ => false,
    };
    each_part(p, &mut |q| found = found || uses_hyp(q, name));
    found
}

/// The keys of the laws `p` applies, each once.
fn laws_applied(p: &Proof, out: &mut Vec<String>) {
    if let Proof::Law { law, .. } = p
        && !out.contains(law)
    {
        out.push(law.clone());
    }
    each_part(p, &mut |q| laws_applied(q, out));
}

/// What failed attempts found, for the hints of a law left open: the keys
/// of the facts and earlier laws whose left side matched where an attempt
/// stopped, and where evaluation stopped ([`env::Stop`]).
#[derive(Default)]
pub(crate) struct Found {
    pub laws: Vec<String>,
    pub stops: Vec<env::Stop>,
}

/// What one obligation's proof needs from its environment.
struct Part {
    proof: Proof,
    /// Advice for the law once closed (see `Env::advice`).
    advice: Vec<String>,
    defs: Vec<crate::ir::proof_steps::Def>,
    consts: Vec<crate::ir::proof_steps::Const>,
    laws: Vec<LawRef>,
}

/// The weight of a proof: its steps and the parts of every term it writes,
/// what checking it reads.
fn weight(p: &Proof) -> usize {
    fn parts(t: &Term) -> usize {
        1 + crate::ir::proof_steps::term::children(t)
            .into_iter()
            .map(parts)
            .sum::<usize>()
    }
    let mut n = p.size();
    p.each_term(&mut |t| n += parts(t));
    n
}

/// Where a [`search`] starts (its stage, see there) and what the stages
/// before it found.
#[derive(Default)]
struct Resume {
    stage: usize,
    met_halving: bool,
    pending_whens: Vec<Term>,
}

/// A proof a [`search`] found, the environment it was found in, the stage
/// that found it, and whether a table of equal terms closed a part of it.
struct Closed<'a> {
    proof: Proof,
    env: Env<'a>,
    stage: usize,
    by_table: bool,
}

/// What a [`search`] found, or why it found nothing; and what a search
/// resumed at a later stage needs from the stages before.
struct Searched<'a> {
    found: Option<Closed<'a>>,
    refusal: String,
    met_halving: bool,
    pending_whens: Vec<Term>,
}

/// The attempts of [`prove_part`] in order, from stage `from.stage`:
/// stages 0 to 3 are the cited laws, evaluation, the cases of the givens
/// of finite type and induction; 4 to 7 the same with a call that divides
/// an Int down to zero kept whole, when one was met; 8 a table of equal
/// terms over the claim as stated; 9 a split on an open line of a cited
/// `when`; 10 an induction along another function of the claim.
fn search<'a>(
    fresh: &dyn Fn() -> Env<'a>,
    using: &Option<Vec<LawRef>>,
    ob: &Obligation,
    induct_on: Option<(crate::ir::identity::FnId, Option<&str>)>,
    hints: &mut Found,
    from: Resume,
) -> Searched<'a> {
    let Resume {
        stage: from,
        mut met_halving,
        mut pending_whens,
    } = from;
    let tables_closed = || egraph::stats().closed;
    let mut env = fresh();
    let mut refusals: Vec<String> = Vec::new();
    // What found the proof: the stage, and whether a table closed a part.
    let mut found: Option<(Proof, usize, bool)> = None;
    // The first pass's refusals, kept once the second pass runs.
    let mut opened: Option<Vec<String>> = None;
    for stage in from..8 {
        if stage == 4 {
            if !met_halving {
                break;
            }
            opened = Some(std::mem::take(&mut refusals));
        }
        env.open_halving = stage < 4;
        let before = tables_closed();
        let outcome = match stage % 4 {
            0 => match using {
                // One instance of a cited law, either way round, first.
                Some(laws) => match env.direct_equal(&ob.lhs, &ob.rhs) {
                    Some(proof) => Ok(proof),
                    None => {
                        let out = citations(&mut env, laws.clone(), ob);
                        // A cited law matched where a line of its `when`
                        // stayed open: split on that line once every
                        // other attempt has failed.
                        if out.is_err() && pending_whens.is_empty() {
                            pending_whens = env
                                .open_whens
                                .iter()
                                .take(MAX_WHEN_SPLITS)
                                .cloned()
                                .collect();
                        }
                        out
                    }
                },
                None => continue,
            },
            1 => {
                let out = env.prove_by_evaluation(&ob.lhs, &ob.rhs, SPLIT_DEPTH);
                // Where the splits spent all the fuel, fewer of them: a
                // shallow case analysis may close what a deep one never
                // reaches.
                if out.is_err() && env.ran_dry {
                    env = fresh();
                    env.open_halving = stage < 4;
                    env.prove_by_evaluation(&ob.lhs, &ob.rhs, SHALLOW_DEPTH)
                } else {
                    out
                }
            }
            2 if ob.finite.is_empty() => continue,
            2 => env.prove_by_cases(&ob.finite, &ob.lhs, &ob.rhs, SPLIT_DEPTH),
            _ => match induct_on {
                Some((f, named)) => env.prove_by_induction(f, named, ob, SPLIT_DEPTH),
                None => continue,
            },
        };
        match outcome {
            Ok(proof) => {
                found = Some((proof, stage, tables_closed() > before));
                break;
            }
            Err(why) => {
                refusals.push(why);
                for h in env.hints.drain(..) {
                    if !hints.laws.contains(&h) {
                        hints.laws.push(h);
                    }
                }
                for stop in env.stops.drain(..) {
                    if hints.stops.len() < env::MAX_STOPS {
                        hints.stops.push(stop);
                    }
                }
                met_halving |= env.met_halving.get();
                env = fresh();
            }
        }
    }
    // A table of equal terms over the claim as stated, its hypotheses, the
    // cited laws and the facts (see [`egraph`]): where evaluation ran out
    // of fuel before it stopped anywhere, the table never saw the claim.
    if found.is_none() && from <= 8 {
        let mut table_env = fresh();
        if let Some(proof) = table_env.close_by_table(&canon(&ob.lhs), &canon(&ob.rhs), &[]) {
            env = table_env;
            found = Some((proof, 8, true));
        }
    }
    let whens = pending_whens.clone();
    // Last, a split on an open line of a cited law's `when`; the splits
    // share half of one attempt's fuel.
    let mut tried = Vec::new();
    if found.is_none()
        && from <= 9
        && let Some(laws) = using
    {
        let mut budget = fresh().fuel / 2;
        for on in std::mem::take(&mut pending_whens) {
            if budget == 0 {
                break;
            }
            let mut split_env = fresh();
            split_env.fuel = budget;
            let before = tables_closed();
            let split = split_on_cited_when(&mut split_env, laws.clone(), ob, &on);
            budget = split_env.fuel;
            match split {
                Ok(proof) => {
                    env = split_env;
                    found = Some((proof, 9, tables_closed() > before));
                    break;
                }
                Err(e) => tried.push(e),
            }
        }
    }
    let mut tried = if tried.is_empty() {
        String::new()
    } else {
        format!("; split on a cited `when`: {}", tried.join("; "))
    };
    // Last, an induction along another function of the claim, on half of
    // one attempt's fuel.
    if found.is_none()
        && let Some((f, named)) = induct_on
    {
        let mut other_env = fresh();
        other_env.fuel /= 2;
        let before = tables_closed();
        match other_env.prove_along_others(f, named, ob, SPLIT_DEPTH) {
            Ok(proof) => {
                env = other_env;
                found = Some((proof, 10, tables_closed() > before));
            }
            Err(why) if !why.is_empty() => {
                for h in other_env.hints.drain(..) {
                    if !hints.laws.contains(&h) {
                        hints.laws.push(h);
                    }
                }
                tried.push_str("; ");
                tried.push_str(&why);
            }
            Err(_) => {}
        }
    }
    let refusal = match &opened {
        Some(first) => format!(
            "{}; with the call that divides down to zero kept whole: {}{tried}",
            first.join("; "),
            refusals.join("; ")
        ),
        None => format!("{}{tried}", refusals.join("; ")),
    };
    Searched {
        found: found.map(|(proof, stage, by_table)| Closed {
            proof,
            env,
            stage,
            by_table,
        }),
        refusal,
        met_halving,
        pending_whens: whens,
    }
}

/// Prove one obligation of the law: `ob` under the hypotheses `known`
/// besides the `when`.
/// `induct_on` is the function induction may follow, with the given the
/// law's `induction` line names for it, if any; `algebra` is set for
/// the claim of a law with an algebraic strategy; `fuel` is what each
/// attempt may spend ([`env::FUEL`], or less for a hint's first try).
#[allow(clippy::too_many_arguments)]
fn prove_part(
    inputs: &ProofLowerInputs,
    t: &LawTheorem,
    ob: &Obligation,
    known: &[(String, Eqn)],
    using: &Option<Vec<LawRef>>,
    earlier: &[LawRef],
    auto: &[LawRef],
    induct_on: Option<(crate::ir::identity::FnId, Option<&str>)>,
    algebraic: bool,
    fuel: usize,
    hints: &mut Found,
) -> Result<Part, String> {
    // Each attempt starts from a fresh environment: a failed one may have
    // opened definitions and bound hypothesis names. The attempts share
    // what evaluation found ([`memo`]), which replays it exactly.
    let mode = memo::mode();
    let shared = (mode != memo::Mode::Off).then(std::rc::Rc::default);
    let fresh = || {
        let mut env = fresh_env(inputs, ob, known, using, earlier, auto);
        env.memo = shared.clone();
        env.memo_mode = mode;
        env.fuel = fuel;
        env
    };
    let (proof, mut env) = if algebraic {
        let mut env = fresh();
        (algebra(&mut env, t, ob)?, env)
    } else {
        let first = search(&fresh, using, ob, induct_on, hints, Resume::default());
        match first.found {
            // A table may close a part where the attempt that met it, or a
            // later one, closes the whole without a table, by a smaller
            // proof: those attempts run again without tables, and the
            // smaller of the two proofs is kept.
            Some(found) if found.by_table => {
                let again = Resume {
                    stage: found.stage,
                    met_halving: first.met_halving,
                    pending_whens: first.pending_whens,
                };
                let mut ignored = Found::default();
                let plain = egraph::without_tables(|| {
                    search(&fresh, using, ob, induct_on, &mut ignored, again)
                });
                match plain.found {
                    Some(p) if weight(&p.proof) <= weight(&found.proof) => (p.proof, p.env),
                    _ => (found.proof, found.env),
                }
            }
            Some(found) => (found.proof, found.env),
            None => return Err(first.refusal),
        }
    };
    // The kernel types a call it splits on by its definition, so a
    // function split on comes with it.
    let mut split = Vec::new();
    split_calls(&proof, &mut split);
    for id in split {
        env.mark_used(id);
    }
    Ok(Part {
        proof,
        advice: std::mem::take(&mut env.advice),
        defs: env.used_defs(),
        consts: env.used_consts(),
        laws: env.laws.clone(),
    })
}

/// One cut in scope order: a `when` line, or reason `k` (from 1) with its
/// proof when its obligation closed.
struct Cut {
    name: String,
    fact: Term,
    proof: Option<Proof>,
    reason: Option<usize>,
}

/// What [`produce`] found: the law's own script (every obligation closed)
/// or why not, and, for a law with `because` lines, each obligation's.
struct Produced {
    law: Result<Script, String>,
    /// Advice for the law when it closed, shown with it.
    advice: Vec<String>,
    obligations: Vec<crate::ir::ObligationSteps>,
}

/// `items` joined right to left with `Bool.and`, and the proof of each
/// item from hypothesis `when` stating the whole.
fn conjoin(items: &[Term]) -> Option<(Term, Vec<Proof>)> {
    use crate::ir::proof_steps::term;
    let (last, rest) = items.split_last()?;
    let mut whole = last.clone();
    for item in rest.iter().rev() {
        whole = canon(&term::bool_and(item.clone(), whole));
    }
    let mut proofs = Vec::new();
    let mut at = whole.clone();
    let mut from = Proof::Hyp("when".into());
    for _ in rest {
        let crate::ir::hir::ResolvedExpr::Call(_, args) = &at.node else {
            unreachable!("built as a conjunction");
        };
        let (a, b) = (canon(&args[0]), canon(&args[1]));
        let elim = |rule: WallRule| Proof::Rule {
            rule,
            subst: vec![("a".into(), a.clone()), ("b".into(), b.clone())],
            premises: vec![from.clone()],
        };
        proofs.push(elim(WallRule::AndElimL));
        from = elim(WallRule::AndElimR);
        at = b;
    }
    proofs.push(from);
    Some((whole, proofs))
}

/// `body`, proved under the named hypotheses `assumes`, as the proof of a
/// script whose premise states them all as one term (`when`): each is cut
/// out of that premise first, under its own name, so `when` names the
/// law's own `when` again. A line about a given of a refined type
/// (`refined<k>`) is cut out only where `body` reads it. A premise that is
/// the `when` alone needs no cut.
fn assuming(assumes: &[(String, Term)], body: Proof) -> (Option<Term>, Proof) {
    let items: Vec<Term> = assumes.iter().map(|(_, fact)| fact.clone()).collect();
    let Some((whole, from)) = conjoin(&items) else {
        return (None, body);
    };
    let mut proof = body;
    if !matches!(assumes, [(name, _)] if name == "when") {
        for ((name, fact), p) in assumes.iter().zip(from).rev() {
            if name.starts_with("refined") && !uses_hyp(&proof, name) {
                continue;
            }
            proof = Proof::Have {
                name: name.clone(),
                fact: fact.clone(),
                proof: Box::new(p),
                body: Box::new(proof),
            };
        }
    }
    (Some(whole), proof)
}

fn merge_into(script: &mut Script, part: &Part) {
    for d in &part.defs {
        if !script.defs.iter().any(|x| x.fn_id == d.fn_id) {
            script.defs.push(d.clone());
        }
    }
    for c in &part.consts {
        if !script.consts.iter().any(|x| x.name == c.name) {
            script.consts.push(c.clone());
        }
    }
    for l in &part.laws {
        if !script.laws.iter().any(|x| x.key == l.key) {
            script.laws.push(l.clone());
        }
    }
}

/// `body` under the cuts in `cuts`, innermost last: a reason is read from
/// hypothesis `h_reason<k-1>` (`with_proofs` false) or proved by its own
/// proof; any other cut only when something after it reads it.
fn under_cuts(cuts: &[Cut], body: Proof, with_proofs: bool) -> Proof {
    let mut proof = body;
    for c in cuts.iter().rev() {
        let from = match (c.reason, with_proofs) {
            (Some(k), false) => Proof::Hyp(format!("h_reason{}", k - 1)),
            _ => c.proof.clone().expect("a cut in scope has its proof"),
        };
        if c.reason.is_some() || uses_hyp(&proof, &c.name) {
            proof = Proof::Have {
                name: c.name.clone(),
                fact: c.fact.clone(),
                proof: Box::new(from),
                body: Box::new(proof),
            };
        }
    }
    proof
}

/// Prove law `i` by steps; `hints` collects the keys of the facts and
/// earlier laws whose left side matches where a failed attempt stopped.
///
/// A law with `because` lines is proved the way the user argued it: with
/// guard `H` and reasons `R1 … Rn`, each `Ri` under `H` and the reasons
/// before it, then the claim under all of them. Each obligation gets a
/// script of its own, checked apart, whose premise states what it assumes
/// (the earlier reasons, then `H`); a later obligation is tried even when
/// an earlier one did not close. When all close, the law's own script
/// proves it by one cut ([`Proof::Have`]) per reason, so the kernel
/// checks the composition too.
fn produce(
    inputs: &ProofLowerInputs,
    ir: &ProofIR,
    i: usize,
    auto: &[LawRef],
    hints: &mut Found,
) -> Produced {
    // A table of equal terms ([`egraph`]) is built here, where a law is
    // closed, and not for the hints a law left open is tried again for;
    // nor for a law the compiler generated (a name in the `__` namespace),
    // whose functions the Lean export writes in a shape a table's steps
    // do not elaborate against.
    let t = &ir.law_theorems[i];
    if inputs
        .symbol_table
        .fn_entry(t.fn_id)
        .key
        .name
        .starts_with("__")
    {
        return produce_as(inputs, ir, i, t, auto, env::FUEL, hints);
    }
    let before = egraph::stats();
    let out = egraph::with_tables(|| produce_as(inputs, ir, i, t, auto, env::FUEL, hints));
    if std::env::var_os("AVER_STEPS_DEBUG").is_some() {
        let after = egraph::stats();
        eprintln!(
            "tables: {}: {} built, {} closed, {:.1} ms in open ones, {:.1} ms in closed ones",
            law_key(inputs, t),
            after.built - before.built,
            after.closed - before.closed,
            (after.open_time - before.open_time).as_secs_f64() * 1000.0,
            (after.closed_time - before.closed_time).as_secs_f64() * 1000.0
        );
    }
    out
}

/// [`produce`] for `t` standing where law `i` stands: law `i` itself, or
/// law `i` with another `using` line; each attempt may spend `fuel`.
fn produce_as(
    inputs: &ProofLowerInputs,
    ir: &ProofIR,
    i: usize,
    t: &LawTheorem,
    auto: &[LawRef],
    fuel: usize,
    hints: &mut Found,
) -> Produced {
    let refuse = |why: String| Produced {
        law: Err(why),
        advice: Vec::new(),
        obligations: Vec::new(),
    };
    // What the law states: its claim under everything it assumes.
    let stated = match obligation(inputs, ir, t) {
        Ok(ob) => ob,
        Err(why) => return refuse(why),
    };
    // What its givens' refined types hold, each a hypothesis of its own,
    // and the claim under the law's own `when`: what the producers prove.
    let refined: Vec<(String, Term)> = stated
        .refined
        .iter()
        .enumerate()
        .map(|(k, line)| (format!("refined{}", k + 1), line.clone()))
        .collect();
    let ob = Obligation {
        premise: stated.when().cloned(),
        refined: Vec::new(),
        ..stated.clone()
    };
    let using = match &t.using {
        Some(names) if !names.is_empty() => match cited(inputs, ir, t, names) {
            Some(laws) => Some(laws),
            None => return refuse("a cited law has no theorem".into()),
        },
        _ => None,
    };
    let earlier = earlier_laws(inputs, ir, i);
    let algebraic = matches!(
        t.strategy,
        ProofStrategy::Commutative { .. }
            | ProofStrategy::Associative { .. }
            | ProofStrategy::IdentityElement { .. }
    );
    let mut cuts = when_cuts(&ob);
    // A `when` line that names a constant (`capBytes()`) is also stated
    // with its value, so the comparisons read it as arithmetic.
    let mut constant_defs = Vec::new();
    if let Some(p) = &ob.premise {
        let mut lines = when_lines(p);
        if lines.is_empty() {
            lines.push((canon(p), Proof::Hyp("when".into())));
        }
        let mut scratch = Env::new(inputs);
        for (k, (fact, proof)) in lines.into_iter().enumerate() {
            if let Some((bridge, folded)) = scratch.fold_constants(&fact) {
                cuts.push(Cut {
                    name: format!("when_value{}", k + 1),
                    fact: folded.clone(),
                    proof: Some(Proof::Trans {
                        terms: vec![folded, fact, crate::ir::proof_steps::term::boolean(true)],
                        steps: vec![Proof::Symm(Box::new(bridge)), proof],
                    }),
                    reason: None,
                });
            }
        }
        constant_defs = scratch.used_defs();
    }
    let with_constants = |script: &mut Script| {
        if cuts_used(&script.proof) {
            for d in &constant_defs {
                if !script.defs.iter().any(|x| x.fn_id == d.fn_id) {
                    script.defs.push(d.clone());
                }
            }
        }
    };
    let known_of = |cuts: &[Cut]| -> Vec<(String, Eqn)> {
        refined
            .iter()
            .map(|(name, line)| (name.clone(), chain::eqn_true(line)))
            .chain(
                cuts.iter()
                    .map(|c| (c.name.clone(), chain::eqn_true(&c.fact))),
            )
            .collect()
    };
    let reasons: Vec<Term> = t.reasons.iter().map(|r| law_term(inputs, t, r)).collect();
    let n = reasons.len();
    let key = law_key(inputs, t);
    // Each obligation: its part, or why not, with the cuts in its scope.
    let mut results: Vec<(Result<Part, String>, usize)> = Vec::new();
    for (i, reason) in reasons.iter().enumerate() {
        let part_ob = Obligation {
            key: format!("{key}.because{}", i + 1),
            lhs: reason.clone(),
            rhs: crate::ir::proof_steps::term::boolean(true),
            ..ob.clone()
        };
        // A reason that recurses with a checked plan may be proved by
        // induction along its own function, as the claim is along the law's.
        let induct_on = match (&t.reason_inductions.get(i), &reason.node) {
            (
                Some(Some(_)),
                crate::ir::hir::ResolvedExpr::Call(crate::ir::hir::ResolvedCallee::Fn(f), _),
            ) => Some((*f, None)),
            _ => None,
        };
        let known = known_of(&cuts);
        let part = prove_part(
            inputs, t, &part_ob, &known, &using, &earlier, auto, induct_on, false, fuel, hints,
        )
        .map_err(|why| format!("reason {} of {n}: {why}", i + 1));
        let proof = part.as_ref().ok().map(|p| p.proof.clone());
        results.push((part, cuts.len()));
        cuts.push(Cut {
            name: format!("because{}", i + 1),
            fact: reason.clone(),
            proof,
            reason: Some(i + 1),
        });
    }
    let known = known_of(&cuts);
    let last_ob = Obligation {
        key: if n == 0 {
            key.clone()
        } else {
            format!("{key}.implication")
        },
        ..ob.clone()
    };
    let last = prove_part(
        inputs,
        t,
        &last_ob,
        &known,
        &using,
        &earlier,
        auto,
        Some((t.fn_id, t.induction_given.as_deref())),
        algebraic && n == 0,
        fuel,
        hints,
    )
    .map_err(|why| {
        if n == 0 {
            why
        } else {
            format!("the claim after its {n} reasons: {why}")
        }
    });
    results.push((last, cuts.len()));

    let check = |script: Script| -> Result<Script, String> {
        if script.proof.size() > MAX_PROOF_NODES {
            return Err(format!(
                "{} steps is more than a backend should elaborate",
                script.proof.size()
            ));
        }
        // The kernel judges what the producers built; a script it refuses,
        // or cannot read back, is refused here by the kernel's reason.
        let mut script = script;
        decls::declare_types(inputs, &mut script);
        let text = crate::ir::proof_steps::sexpr::script(&script, inputs.symbol_table)?;
        crate::proof_kernel::verdict(&text).inspect_err(|why| {
            if std::env::var_os("AVER_STEPS_DEBUG").is_some() {
                eprintln!(
                    "steps: {}: the kernel refuses: {why}",
                    script.obligation.key
                );
            }
        })?;
        Ok(script)
    };

    // One script per obligation of a `because` chain.
    let mut obligations = Vec::new();
    if n > 0 {
        for (i, (part, scope)) in results.iter().enumerate() {
            let ob_key = if i < n {
                format!("{key}.because{}", i + 1)
            } else {
                format!("{key}.implication")
            };
            let script = part.as_ref().map_err(Clone::clone).and_then(|part| {
                // What it assumes: what the givens' refined types hold,
                // then, in the order its Lean theorem introduces them, the
                // earlier reasons and the guard.
                let mut assumes: Vec<(String, Term)> = refined.clone();
                assumes.extend(
                    reasons[..i.min(n)]
                        .iter()
                        .enumerate()
                        .map(|(k, r)| (format!("h_reason{k}"), r.clone())),
                );
                if let Some(h) = &ob.premise {
                    assumes.push(("when".into(), h.clone()));
                }
                let view = under_cuts(&cuts[..*scope], part.proof.clone(), false);
                let (premise, proof) = assuming(&assumes, view);
                let (lhs, rhs) = if i < n {
                    (
                        reasons[i].clone(),
                        crate::ir::proof_steps::term::boolean(true),
                    )
                } else {
                    (ob.lhs.clone(), ob.rhs.clone())
                };
                let mut script = Script {
                    obligation: Obligation {
                        key: ob_key.clone(),
                        premise,
                        refined: stated.refined.clone(),
                        lhs,
                        rhs,
                        ..ob.clone()
                    },
                    defs: Vec::new(),
                    consts: Vec::new(),
                    laws: Vec::new(),
                    decls: Default::default(),
                    sigs: Vec::new(),
                    plan: None,
                    proof,
                };
                merge_into(&mut script, part);
                with_constants(&mut script);
                check(script)
            });
            obligations.push(crate::ir::ObligationSteps {
                key: ob_key,
                script: script.as_ref().ok().cloned(),
                refusal: script.err(),
            });
        }
    }

    // The law itself, once every obligation closed.
    if let Some(why) = results.iter().find_map(|(p, _)| p.as_ref().err()) {
        return Produced {
            law: Err(why.clone()),
            advice: Vec::new(),
            obligations,
        };
    }
    let parts: Vec<&Part> = results
        .iter()
        .map(|(p, _)| p.as_ref().expect("all closed"))
        .collect();
    let mut assumes = refined.clone();
    if let Some(h) = &ob.premise {
        assumes.push(("when".into(), h.clone()));
    }
    let (_, proof) = assuming(&assumes, under_cuts(&cuts, parts[n].proof.clone(), true));
    let advice = parts[n].advice.clone();
    let mut script = Script {
        obligation: stated,
        defs: Vec::new(),
        consts: Vec::new(),
        laws: Vec::new(),
        decls: Default::default(),
        sigs: Vec::new(),
        plan: None,
        proof,
    };
    for part in parts {
        merge_into(&mut script, part);
    }
    with_constants(&mut script);
    Produced {
        law: check(script),
        advice,
        obligations,
    }
}

/// The laws before law `i`, as a `using` list may name them: hints only,
/// where one would rewrite a stuck term.
fn earlier_laws(inputs: &ProofLowerInputs, ir: &ProofIR, i: usize) -> Vec<LawRef> {
    let t = &ir.law_theorems[i];
    ir.law_theorems[..i]
        .iter()
        .filter(|c| citable_from(inputs, ir, t, c))
        .filter_map(|c| law_ref(inputs, ir, c).ok())
        .filter(|l| rewrite::loops(l).is_none())
        .collect()
}

/// Most earlier laws one proof of the automatic pass may apply.
const MAX_AUTO_LAWS: usize = 8;

/// The earlier laws a law without a `using` line may be proved with on
/// the automatic pass, in the order the laws are proved (the citation
/// order of the export, not the order of the text): those a `using` line of law `i`
/// could name ([`citable_from`]) that the steps closed in this run (so a
/// law closed only by Lean's tactics is not one), minus any that would
/// loop as a rewrite rule, and minus both laws of a pair where each
/// rewrites what the other produces, which together would undo each
/// other's work.
fn auto_pool(inputs: &ProofLowerInputs, ir: &ProofIR, i: usize) -> Vec<LawRef> {
    let t = &ir.law_theorems[i];
    let pool: Vec<LawRef> = ir.law_theorems[..i]
        .iter()
        .filter(|c| c.steps.is_some() && citable_from(inputs, ir, t, c))
        .filter_map(|c| law_ref(inputs, ir, c).ok())
        .filter(|l| rewrite::loops(l).is_none())
        .collect();
    let undoes = |a: &LawRef, b: &LawRef| {
        rewrite::matches(&a.lhs, &b.rhs, &a.givens, &mut Vec::new())
            && rewrite::matches(&b.lhs, &a.rhs, &b.givens, &mut Vec::new())
    };
    pool.iter()
        .filter(|a| !pool.iter().any(|b| b.key != a.key && undoes(a, b)))
        .cloned()
        .collect()
}

/// Each line of `ob`'s `when`, when written over several lines, as a
/// hypothesis of its own, cut from `when` by the `Bool.and` eliminations.
fn when_cuts(ob: &Obligation) -> Vec<Cut> {
    ob.premise
        .as_ref()
        .map(when_lines)
        .unwrap_or_default()
        .into_iter()
        .enumerate()
        .map(|(i, (fact, proof))| Cut {
            name: format!("when{}", i + 1),
            fact,
            proof: Some(proof),
            reason: None,
        })
        .collect()
}

/// The automatic steps on one goal of a proof plan's script (an `Auto`
/// node of `Kernel.Std.Tactic`): law `i`'s claim as the script has split it
/// (`ob`), under the hypotheses the script brought into scope (`hyps`).
/// The attempts are those for the law's own claim, with its cited laws and
/// the earlier laws as hints; each line of a `when` written over several
/// lines is a hypothesis of its own, as it is for the law.
fn auto_at(
    inputs: &ProofLowerInputs,
    ir: &ProofIR,
    i: usize,
    ob: &Obligation,
    hyps: &[(String, Eqn)],
) -> Result<Part, String> {
    let t = &ir.law_theorems[i];
    let using = match &t.using {
        Some(names) if !names.is_empty() => {
            Some(cited(inputs, ir, t, names).ok_or("a cited law has no theorem")?)
        }
        _ => None,
    };
    let earlier = earlier_laws(inputs, ir, i);
    let cuts = when_cuts(ob);
    let mut known = hyps.to_vec();
    known.extend(
        cuts.iter()
            .map(|c| (c.name.clone(), chain::eqn_true(&c.fact))),
    );
    let mut hints = Found::default();
    let mut part = prove_part(
        inputs,
        t,
        ob,
        &known,
        &using,
        &earlier,
        &[],
        Some((t.fn_id, None)),
        false,
        env::FUEL,
        &mut hints,
    )?;
    part.proof = under_cuts(&cuts, part.proof, true);
    Ok(part)
}

/// Most times the laws of one hint are tried: a law added to the `using`
/// line moves where the steps stop, and what matches there is added for
/// the next try.
const MAX_HINT_ROUNDS: usize = 3;

/// What each attempt of a hint's first try may spend: an eighth of
/// [`env::FUEL`], so a line that does not close the law costs a part of
/// what the law itself did. A line that closes the law only on more than
/// this is not hinted.
const HINT_FUEL: usize = env::FUEL / 8;

/// The hint for law `i`, which the steps left open with its `because`
/// lines: remove them, when the steps close the same law without them.
/// A `because` line only says how to argue the claim, never what it
/// states, so the law without it is the same law. Its reasons can lead the
/// steps where they stop (an induction along a reason's own function that
/// runs out of fuel) while the claim goes by its own induction. The law
/// without them is tried as it would be once its text says so: on a short
/// budget first ([`BECAUSE_HINT_FUEL`]), then on the law's own, with the
/// earlier laws of the automatic pass when it has no `using` line. It is
/// tried only when a reason is open, and hinted only when every law its
/// proof cites is closed too.
fn because_hint(inputs: &ProofLowerInputs, ir: &ProofIR, i: usize) -> Option<String> {
    let t = &ir.law_theorems[i];
    if t.reasons.is_empty() || t.by_plan.is_some() {
        return None;
    }
    // When every reason closed, the claim stopped with all of them in
    // scope: without them it has less to go on, not more.
    let reason_open = t
        .obligation_steps
        .iter()
        .any(|o| o.script.is_none() && !o.key.ends_with(".implication"));
    if !reason_open {
        return None;
    }
    let bare = LawTheorem {
        reasons: Vec::new(),
        reason_inductions: Vec::new(),
        ..t.clone()
    };
    let pool = if bare.using.is_none() {
        auto_pool(inputs, ir, i)
    } else {
        Vec::new()
    };
    // Closed, and every law the proof cites closed too: a law citing an
    // open law stays open.
    let earlier = &ir.law_theorems[..i];
    let closed = |script: Script| {
        script
            .laws
            .iter()
            .filter(|l| l.fact.is_none())
            .all(|l| closed_by_steps(inputs, earlier, &l.key))
    };
    let closes = |fuel: usize| {
        produce_as(inputs, ir, i, &bare, &[], fuel, &mut Found::default())
            .law
            .is_ok_and(closed)
            || (!pool.is_empty()
                && produce_as(inputs, ir, i, &bare, &pool, fuel, &mut Found::default())
                    .law
                    .is_ok_and(closed))
    };
    if !closes(BECAUSE_HINT_FUEL) || !closes(env::FUEL) {
        return None;
    }
    Some(if t.reasons.len() == 1 {
        "steps close this law without its `because` line; remove it".to_string()
    } else {
        "steps close this law without its `because` lines; remove them".to_string()
    })
}

/// What the first try of a law without its `because` lines may spend: a
/// quarter of [`env::FUEL`]. The claim's own induction is a whole proof,
/// not one rewrite, and an eighth leaves some that close unhinted.
const BECAUSE_HINT_FUEL: usize = env::FUEL / 4;

/// Most laws one hint adds to a `using` line.
const MAX_HINT_LAWS: usize = 8;

/// Whether the steps closed the law `key` among `laws`, and every law its
/// script cites: a law that cites an open law is open too.
fn closed_by_steps(inputs: &ProofLowerInputs, laws: &[LawTheorem], key: &str) -> bool {
    let mut pending = vec![key.to_string()];
    let mut seen: Vec<String> = Vec::new();
    while let Some(next) = pending.pop() {
        if seen.contains(&next) {
            continue;
        }
        let Some(script) = laws
            .iter()
            .find(|c| is_law_key(inputs, c, &next))
            .and_then(|c| c.steps.as_ref())
        else {
            return false;
        };
        let cited = script.laws.iter().filter(|l| l.fact.is_none());
        pending.extend(cited.map(|l| l.key.clone()));
        seen.push(next);
    }
    true
}

/// The hint for law `i`, which the steps left open: the `using` line they
/// close it with, or `None`. `found` are the facts and earlier laws whose
/// left side matched where an attempt stopped. Those the steps closed are
/// added to the law's line and the law is tried again; where it still
/// stops, what matches there is added too, [`MAX_HINT_ROUNDS`] times at
/// most. A line is hinted only when the law closed with it, every
/// obligation of a `because` chain included, so a law whose `when` cannot
/// be proved where it matched is never hinted, and neither is an open law,
/// which would leave the citing law open. The line names only the laws the
/// proof applies when the law closes with those alone.
fn using_hint(
    inputs: &ProofLowerInputs,
    ir: &ProofIR,
    i: usize,
    mut found: Vec<String>,
) -> Option<String> {
    use crate::ir::proof_steps::facts;
    let t = &ir.law_theorems[i];
    if found.is_empty() || t.by_plan.is_some() {
        return None;
    }
    let earlier = &ir.law_theorems[..i];
    let closed = |key: &str| facts::named(key).is_some() || closed_by_steps(inputs, earlier, key);
    let base = t.using.clone().unwrap_or_default();
    // A line that cites an open law leaves the law open whatever is added.
    let cited_closed = |l: &LawRef| l.fact.is_some() || closed(&l.key);
    if !cited(inputs, ir, t, &base)?.iter().all(cited_closed) {
        return None;
    }
    // A law of the law's own module is named as its text names it.
    let own = &inputs.symbol_table.fn_entry(t.fn_id).key.scope;
    let name = |key: &String| -> String {
        earlier
            .iter()
            .find(|c| is_law_key(inputs, c, key))
            .map(|c| (&inputs.symbol_table.fn_entry(c.fn_id).key, &c.law_name))
            .filter(|(k, _)| k.scope == *own)
            .map_or_else(|| key.clone(), |(k, law)| format!("{}.{law}", k.name))
    };
    let line = |added: &[String]| -> Vec<String> {
        base.iter().cloned().chain(added.iter().map(name)).collect()
    };
    let closes = |added: &[String], fuel: usize, found: &mut Vec<String>| {
        let with = LawTheorem {
            using: Some(line(added)),
            ..t.clone()
        };
        let mut more = Found::default();
        let out = produce_as(inputs, ir, i, &with, &[], fuel, &mut more)
            .law
            .ok();
        found.extend(more.laws);
        out
    };
    let mut added: Vec<String> = Vec::new();
    for _ in 0..MAX_HINT_ROUNDS {
        let before = added.len();
        for key in std::mem::take(&mut found) {
            if added.len() < MAX_HINT_LAWS && !added.contains(&key) && closed(&key) {
                added.push(key);
            }
        }
        if added.len() == before {
            return None;
        }
        // First on a short budget: a line that closes the law seldom needs
        // more, and a line that does not would spend all there is. Then as
        // the law will be proved once the line is in its text.
        if closes(&added, HINT_FUEL, &mut found).is_none() {
            continue;
        }
        let Some(script) = closes(&added, env::FUEL, &mut Vec::new()) else {
            continue;
        };
        let mut applied = Vec::new();
        laws_applied(&script.proof, &mut applied);
        let fewer: Vec<String> = added
            .iter()
            .filter(|k| applied.contains(k))
            .cloned()
            .collect();
        if !fewer.is_empty()
            && fewer.len() < added.len()
            && closes(&fewer, env::FUEL, &mut Vec::new()).is_some()
        {
            added = fewer;
        }
        return Some(format!(
            "steps close this law with `using [{}]`",
            line(&added).join(", ")
        ));
    }
    None
}

/// Fill `LawTheorem::steps` for every law a producer can prove.
/// The first try of law `i`, made apart from the walk over the laws: what
/// it produced, what its failed attempts found, the time it took and the
/// kernel's share of it.
struct FirstTry {
    produced: Produced,
    hints: Found,
    spent: std::time::Duration,
    kernel: (std::time::Duration, u64),
}

/// The first try of every law that names no plan. Each reads the program
/// and the statements of the laws, never what another law's try found, so
/// they run side by side, on as many threads as there are cores, each on
/// the stack the proof code runs on ([`crate::proof_stack`]); what each
/// finds is what it finds when the laws are tried one after another.
#[cfg(not(target_family = "wasm"))]
fn first_tries(inputs: &ProofLowerInputs, ir: &ProofIR) -> Vec<Option<FirstTry>> {
    use std::sync::atomic::{AtomicUsize, Ordering};
    let laws = ir.law_theorems.len();
    let workers = std::thread::available_parallelism()
        .map_or(1, |n| n.get())
        .min(laws);
    if workers <= 1 {
        return (0..laws).map(|_| None).collect();
    }
    let next = AtomicUsize::new(0);
    let tries: Vec<std::sync::Mutex<Option<FirstTry>>> =
        (0..laws).map(|_| std::sync::Mutex::new(None)).collect();
    std::thread::scope(|scope| {
        for _ in 0..workers {
            let (next, tries) = (&next, &tries);
            let started = std::thread::Builder::new()
                .name("aver-steps".to_string())
                .stack_size(crate::proof_stack::STACK_BYTES)
                .spawn_scoped(scope, move || {
                    loop {
                        let i = next.fetch_add(1, Ordering::Relaxed);
                        if i >= laws {
                            break;
                        }
                        if ir.law_theorems[i].by_plan.is_some() {
                            continue;
                        }
                        let mut hints = Found::default();
                        let started = std::time::Instant::now();
                        let kernel_before = crate::proof_kernel::spent();
                        let produced = produce(inputs, ir, i, &[], &mut hints);
                        let spent = started.elapsed();
                        let kernel_after = crate::proof_kernel::spent();
                        let kernel = (
                            kernel_after.0 - kernel_before.0,
                            kernel_after.1 - kernel_before.1,
                        );
                        if let Ok(mut slot) = tries[i].lock() {
                            *slot = Some(FirstTry {
                                produced,
                                hints,
                                spent,
                                kernel,
                            });
                        }
                    }
                });
            // A thread that cannot start leaves its laws to the walk.
            drop(started);
        }
    });
    tries
        .into_iter()
        .map(|slot| slot.into_inner().ok().flatten())
        .collect()
}

/// A wasm build has no threads to start: every law is tried in the walk.
#[cfg(target_family = "wasm")]
fn first_tries(_inputs: &ProofLowerInputs, ir: &ProofIR) -> Vec<Option<FirstTry>> {
    ir.law_theorems.iter().map(|_| None).collect()
}

/// Prove every law by steps. The walk runs on the stack the proof code
/// runs on ([`crate::proof_stack`]), as the first tries do, so how deep an
/// evaluation may nest does not depend on the caller's thread.
pub(crate) fn populate_law_steps(inputs: &ProofLowerInputs, ir: &mut ProofIR) {
    if let Err(why) = crate::proof_stack::run("the proof steps", || walk_laws(inputs, ir)) {
        panic!("{why}");
    }
}

fn walk_laws(inputs: &ProofLowerInputs, ir: &mut ProofIR) {
    let debug = std::env::var_os("AVER_STEPS_DEBUG").is_some();
    let mut plans = by_plan::Plans::default();
    // The plans of the modules `by` lines name, found when a law first
    // stays open.
    let mut named_plans = None;
    let started = std::time::Instant::now();
    let mut first = first_tries(inputs, ir).into_iter();
    for i in 0..ir.law_theorems.len() {
        let tried = first.next().flatten();
        // The laws that match where an attempt stopped, by key, and where
        // the attempts stopped.
        let mut hints = Found::default();
        let mut timing = crate::ir::StepsTiming::default();
        let law_started = std::time::Instant::now();
        let kernel_before = crate::proof_kernel::spent();
        // The kernel's time in this law's first try, where another thread
        // made it.
        let mut kernel_elsewhere = (std::time::Duration::ZERO, 0);
        let mut tried_for = None;
        // A law that names its plan is proved by that plan alone.
        let mut produced = if ir.law_theorems[i].by_plan.is_some() {
            Produced {
                law: by_plan::produce(inputs, ir, i, &mut plans),
                advice: Vec::new(),
                obligations: Vec::new(),
            }
        } else if let Some(tried) = tried {
            hints = tried.hints;
            kernel_elsewhere = tried.kernel;
            tried_for = Some(tried.spent);
            tried.produced
        } else {
            produce(inputs, ir, i, &[], &mut hints)
        };
        timing.produce = tried_for.unwrap_or_else(|| law_started.elapsed());
        // A law without a `using` line that stays open is tried once more
        // with the earlier laws the steps closed, where evaluation stops
        // (`using []` turns this off). What closes without them keeps its
        // proof as it was.
        let mut automatic = Vec::new();
        if produced.law.is_err()
            && ir.law_theorems[i].using.is_none()
            && ir.law_theorems[i].by_plan.is_none()
        {
            let started = std::time::Instant::now();
            let pool = auto_pool(inputs, ir, i);
            if !pool.is_empty() {
                let mut more = Found::default();
                let again = produce(inputs, ir, i, &pool, &mut more);
                // Where the pass with the earlier laws stopped comes first:
                // it got further.
                more.stops.append(&mut hints.stops);
                more.stops.truncate(env::MAX_STOPS);
                hints.stops = more.stops;
                if let Ok(script) = &again.law {
                    let used: Vec<String> = pool
                        .iter()
                        .filter(|l| script.laws.iter().any(|s| s.key == l.key))
                        .map(|l| l.key.clone())
                        .collect();
                    if debug {
                        let key = law_key(inputs, &ir.law_theorems[i]);
                        eprintln!("steps: {key}: automatic pass uses {used:?}");
                    }
                    if used.len() <= MAX_AUTO_LAWS {
                        automatic = used;
                        produced = again;
                    }
                }
            }
            timing.automatic = started.elapsed();
        }
        ir.law_theorems[i].obligation_steps = produced.obligations;
        let mut produced_advice = produced.advice;
        if !automatic.is_empty() {
            produced_advice.push(format!(
                "steps closed it with earlier laws they chose; add `using [{}]` to go straight to it",
                automatic.join(", ")
            ));
        }
        ir.law_theorems[i].steps_auto_using = automatic;
        let result = produced.law;
        if debug {
            let key = law_key(inputs, &ir.law_theorems[i]);
            match &result {
                Ok(s) => eprintln!("steps: {key}: {} nodes", s.proof.size()),
                Err(why) => eprintln!("steps: {key}: none ({why})"),
            }
        }
        match result {
            Ok(script) => {
                ir.law_theorems[i].steps = Some(script);
                ir.law_theorems[i].steps_refusal = None;
                ir.law_theorems[i].steps_hints = produced_advice;
            }
            Err(why) => {
                // A plan that closes the law comes first:
                // it is the one hint that closes the law outright.
                let started = std::time::Instant::now();
                let mut from_plans = by_plan::hints(inputs, ir, i, &mut plans, &mut named_plans);
                timing
                    .hints
                    .push(("by_plan", started.elapsed(), from_plans.len()));
                // Then, for a wrapper that starts an accumulator at a
                // constant, the law about the recursion that closes it.
                let started = std::time::Instant::now();
                let accumulated = accumulator::hint(inputs, ir, i);
                timing.hints.push((
                    "accumulator",
                    started.elapsed(),
                    usize::from(accumulated.is_some()),
                ));
                from_plans.extend(accumulated);
                // Then, for a law with `because` lines, the same law
                // without them, when the steps close that.
                let started = std::time::Instant::now();
                let without = because_hint(inputs, ir, i);
                timing
                    .hints
                    .push(("because", started.elapsed(), usize::from(without.is_some())));
                if debug && !ir.law_theorems[i].reasons.is_empty() {
                    let key = law_key(inputs, &ir.law_theorems[i]);
                    let ms = started.elapsed().as_millis();
                    eprintln!("steps: {key}: because hint in {ms} ms: {without:?}");
                }
                from_plans.extend(without);
                // Then the `using` line the steps close it with.
                let started = std::time::Instant::now();
                let line = using_hint(inputs, ir, i, hints.laws);
                timing
                    .hints
                    .push(("using", started.elapsed(), usize::from(line.is_some())));
                if debug {
                    let key = law_key(inputs, &ir.law_theorems[i]);
                    let ms = started.elapsed().as_millis();
                    eprintln!("steps: {key}: using hint in {ms} ms: {line:?}");
                }
                from_plans.extend(line);
                // Last, when nothing above closes it, a lemma stated from
                // where the steps stopped, which the steps close and with
                // which they close the law.
                if from_plans.is_empty() && lemma::enabled(inputs) {
                    let started = std::time::Instant::now();
                    let lemma = lemma::hint(inputs, ir, i, &hints.stops);
                    timing
                        .hints
                        .push(("lemma", started.elapsed(), usize::from(lemma.is_some())));
                    if debug {
                        let key = law_key(inputs, &ir.law_theorems[i]);
                        let ms = started.elapsed().as_millis();
                        eprintln!("steps: {key}: lemma hint in {ms} ms: {lemma:?}");
                    }
                    from_plans.extend(lemma);
                }
                let hints = from_plans;
                ir.law_theorems[i].steps = None;
                ir.law_theorems[i].steps_refusal = Some(why);
                ir.law_theorems[i].steps_hints = hints;
            }
        }
        let (kernel, calls) = crate::proof_kernel::spent();
        timing.kernel = kernel - kernel_before.0 + kernel_elsewhere.0;
        timing.kernel_calls = calls - kernel_before.1 + kernel_elsewhere.1;
        ir.law_theorems[i].steps_timing = timing;
    }
    ir.steps_elapsed = started.elapsed();
}

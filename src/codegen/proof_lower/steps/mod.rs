//! Step producers: the rungs that recognised a law's shape write its proof
//! as data (`crate::ir::proof_steps`) instead of leaving a backend to search.
//!
//! Producers, tried in order; a script is kept only once the kernel
//! written in Aver ([`crate::proof_kernel::verdict`]) accepts it:
//! - **algebra**: a pinned `Commutative` / `Associative` / `IdentityElement`
//!   law: open the wrapper on both sides, close with one ring rule;
//! - **citations**: a law with an explicit `using` list: rewrite with the
//!   cited laws and the Euclidean recomposition rule to a normal form;
//! - **evaluation**: evaluate both sides, splitting on each undecided Bool
//!   guard, until both sides are the same term;
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
//! A law no producer handles keeps `steps: None` and its tactic portfolio.

mod chain;
mod env;
mod eval;
mod finite;
mod induction;
mod rewrite;
mod split;

use crate::codegen::proof_lower::ProofLowerInputs;
use crate::ir::hir::{ResolvedCallee, ResolvedExpr};
use crate::ir::proof_steps::term::{Term, canon};
use crate::ir::proof_steps::{LawRef, Obligation, Proof, Script, WallRule};
use crate::ir::{LawTheorem, ProofIR, ProofStrategy};

use env::Env;
use rewrite::Equation;

/// Split depth of the evaluation producer: at most 2^6 leaves.
const SPLIT_DEPTH: usize = 6;
/// Largest proof the producers hand to a backend.
const MAX_PROOF_NODES: usize = 3000;

fn law_key(inputs: &ProofLowerInputs, t: &LawTheorem) -> String {
    let key = &inputs.symbol_table.fn_entry(t.fn_id).key;
    match key.scope_str() {
        Some(scope) => format!("{scope}.{}.{}", key.name, t.law_name),
        None => format!("{}.{}", key.name, t.law_name),
    }
}

fn law_ref(inputs: &ProofLowerInputs, t: &LawTheorem) -> LawRef {
    LawRef {
        key: law_key(inputs, t),
        givens: t.quantifiers.iter().map(|q| q.name.clone()).collect(),
        premise: premise_of(inputs, t),
        lhs: law_term(inputs, t, &t.claim_lhs),
        rhs: law_term(inputs, t, &t.claim_rhs),
        fact: None,
    }
}

/// A term of law `t`, with its module's binding reads spelled as every
/// step term spells them (`env::qualify_bindings`).
fn law_term(inputs: &ProofLowerInputs, t: &LawTheorem, term: &Term) -> Term {
    let scope = inputs.symbol_table.fn_entry(t.fn_id).key.scope_str();
    env::qualify_bindings(&canon(term), inputs, scope)
}

fn premise_of(inputs: &ProofLowerInputs, t: &LawTheorem) -> Option<Term> {
    match t.premises.as_slice() {
        [] => None,
        [p] => Some(law_term(inputs, t, &p.expr)),
        _ => None,
    }
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
    let own_scope = inputs.symbol_table.fn_entry(t.fn_id).key.scope.clone();
    let names_law = |c: &LawTheorem, name: &String| {
        let key = &inputs.symbol_table.fn_entry(c.fn_id).key;
        let local = format!("{}.{}", key.name, c.law_name);
        (key.scope == own_scope && *name == local) || *name == law_key(inputs, c)
    };
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
            !ir.law_theorems.iter().any(|c| names_law(c, n))
        }
    }) {
        return None;
    }
    out.extend(
        ir.law_theorems
            .iter()
            .filter(|c| names.iter().any(|n| names_law(c, n)))
            .map(|c| law_ref(inputs, c)),
    );
    Some(out)
}

fn obligation(inputs: &ProofLowerInputs, t: &LawTheorem) -> Obligation {
    Obligation {
        key: law_key(inputs, t),
        givens: t.quantifiers.iter().map(|q| q.name.clone()).collect(),
        finite: finite::finite_givens(inputs, t),
        lists: finite::list_givens(t),
        ints: finite::int_givens(t),
        premise: premise_of(inputs, t),
        lhs: law_term(inputs, t, &t.claim_lhs),
        rhs: law_term(inputs, t, &t.claim_rhs),
    }
}

fn algebra(env: &mut Env, t: &LawTheorem, ob: &Obligation) -> Result<Proof, String> {
    let (op, closers): (_, &[WallRule]) = match &t.strategy {
        ProofStrategy::Commutative { op } => (*op, &[WallRule::AddComm, WallRule::MulComm]),
        ProofStrategy::Associative { op } => (*op, &[WallRule::AddAssoc, WallRule::MulAssoc]),
        ProofStrategy::IdentityElement { op } => (
            *op,
            &[
                WallRule::AddZero,
                WallRule::ZeroAdd,
                WallRule::MulOne,
                WallRule::OneMul,
                WallRule::SubZero,
            ],
        ),
        _ => return Err("not an algebraic strategy".into()),
    };
    let _ = op;
    let unfold = [Equation::Unfold(t.fn_id)];
    let left = env.rewrite(&ob.lhs, &unfold)?;
    let right = env.rewrite(&ob.rhs, &unfold)?;
    if left.cur() == right.cur() {
        return Ok(chain::meet(left, right));
    }
    let closers: Vec<Equation> = closers.iter().copied().map(Equation::Wall).collect();
    for (from, to, flip) in [(&left, &right, false), (&right, &left, true)] {
        let mut extended = from.clone();
        let Some((step, next)) = env.apply_at_root(&closers, from.cur()) else {
            continue;
        };
        extended.push(step, next);
        if canon(extended.cur()) == canon(to.cur()) {
            return Ok(if flip {
                chain::meet(to.clone(), extended)
            } else {
                chain::meet(extended, to.clone())
            });
        }
    }
    Err("no ring rule closes the unfolded sides".into())
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

/// An environment for one attempt: the law's `when` in scope, each of its
/// lines and each reason proved so far as a hypothesis of its own, and
/// its cited laws, minus any that would loop, as rewrite rules.
fn fresh_env<'a>(
    inputs: &'a ProofLowerInputs<'a>,
    ob: &Obligation,
    known: &[(String, Term)],
    using: &Option<Vec<LawRef>>,
) -> Env<'a> {
    let mut env = Env::new(inputs);
    env.givens = ob.givens.clone();
    env.finite = ob.finite.iter().map(|(n, _)| n.clone()).collect();
    if let Some(p) = &ob.premise {
        env.hyps.push(("when".into(), chain::eqn_true(p)));
    }
    for (name, fact) in known {
        env.hyps.push((name.clone(), chain::eqn_true(fact)));
    }
    env.rewrite_laws = using
        .iter()
        .flatten()
        .filter(|l| rewrite::loops(l).is_none())
        .cloned()
        .collect();
    env.cited_all = using.iter().flatten().cloned().collect();
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

/// The user functions whose calls `p` splits into the true and false cases.
fn split_calls(p: &Proof, out: &mut Vec<crate::ir::identity::FnId>) {
    let mut all = |ps: &[Proof]| ps.iter().for_each(|q| split_calls(q, out));
    match p {
        Proof::Cases {
            on,
            if_true,
            if_false,
            ..
        } => {
            if let ResolvedExpr::Call(ResolvedCallee::Fn(id), _) = &on.node {
                out.push(*id);
            }
            split_calls(if_true, out);
            split_calls(if_false, out);
        }
        Proof::Symm(q) | Proof::Congr { inner: q, .. } => split_calls(q, out),
        Proof::Absurd { contradiction, .. } => split_calls(contradiction, out),
        Proof::Arm { premise, .. } => split_calls(premise, out),
        Proof::Unfold { premise, .. } | Proof::Law { premise, .. } => {
            if let Some(q) = premise {
                split_calls(q, out);
            }
        }
        Proof::Trans { steps, .. } => all(steps),
        Proof::Rule { premises, .. } => all(premises),
        Proof::Enum { cases, .. } => all(cases),
        Proof::Split { cases, .. } => cases.iter().for_each(|c| split_calls(&c.proof, out)),
        Proof::Have { proof, body, .. } => {
            split_calls(proof, out);
            split_calls(body, out);
        }
        Proof::Induct { cases, .. } => cases.iter().for_each(|c| {
            split_calls(&c.proof, out);
            c.carry.iter().flatten().for_each(|q| split_calls(q, out));
        }),
        Proof::InductList { nil, cons, .. } => {
            split_calls(nil, out);
            split_calls(cons, out);
        }
        Proof::InductInt {
            base, ihs, step, ..
        } => {
            split_calls(base, out);
            split_calls(step, out);
            ihs.iter()
                .flat_map(|i| &i.carry)
                .for_each(|q| split_calls(q, out));
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

/// Whether `p` may read hypothesis `name` (an over-approximation: a
/// hypothesis of the same name bound inside counts too).
fn uses_hyp(p: &Proof, name: &str) -> bool {
    let any = |ps: &[Proof]| ps.iter().any(|q| uses_hyp(q, name));
    match p {
        Proof::Hyp(n) => n == name,
        Proof::Linear { hyps, .. } => hyps.iter().any(|n| n == name),
        Proof::Symm(q) | Proof::Congr { inner: q, .. } => uses_hyp(q, name),
        Proof::Absurd { contradiction, .. } => uses_hyp(contradiction, name),
        Proof::Arm { premise, .. } => uses_hyp(premise, name),
        Proof::Unfold { premise, .. } | Proof::Law { premise, .. } => {
            premise.as_ref().is_some_and(|q| uses_hyp(q, name))
        }
        Proof::Trans { steps, .. } => any(steps),
        Proof::Rule { premises, .. } => any(premises),
        Proof::Enum { cases, .. } => any(cases),
        Proof::Cases {
            if_true, if_false, ..
        } => uses_hyp(if_true, name) || uses_hyp(if_false, name),
        Proof::Split { cases, .. } => cases.iter().any(|c| uses_hyp(&c.proof, name)),
        Proof::Have { proof, body, .. } => uses_hyp(proof, name) || uses_hyp(body, name),
        Proof::Induct { carried, cases, .. } => {
            carried.iter().any(|n| n == name)
                || cases.iter().any(|c| {
                    uses_hyp(&c.proof, name) || c.carry.iter().flatten().any(|q| uses_hyp(q, name))
                })
        }
        Proof::InductList { nil, cons, .. } => uses_hyp(nil, name) || uses_hyp(cons, name),
        Proof::InductInt {
            base,
            carried,
            ihs,
            step,
            ..
        } => {
            uses_hyp(base, name)
                || uses_hyp(step, name)
                || carried.iter().any(|n| n == name)
                || ihs.iter().flat_map(|i| &i.carry).any(|p| uses_hyp(p, name))
        }
        Proof::Refl(_)
        | Proof::UnfoldConst { .. }
        | Proof::Proj { .. }
        | Proof::Cell { .. }
        | Proof::Compute { .. }
        | Proof::Ring { .. } => false,
    }
}

/// What one obligation's proof needs from its environment.
struct Part {
    proof: Proof,
    defs: Vec<crate::ir::proof_steps::Def>,
    consts: Vec<crate::ir::proof_steps::Const>,
    laws: Vec<LawRef>,
}

/// Prove one obligation of the law: `ob` under `known` besides the `when`.
/// `induct_on` is the function induction may follow; `algebra` is set for
/// the claim of a law with an algebraic strategy.
#[allow(clippy::too_many_arguments)]
fn prove_part(
    inputs: &ProofLowerInputs,
    t: &LawTheorem,
    ob: &Obligation,
    known: &[(String, Term)],
    using: &Option<Vec<LawRef>>,
    induct_on: Option<crate::ir::identity::FnId>,
    algebraic: bool,
    hints: &mut Vec<String>,
) -> Result<Part, String> {
    // Each attempt starts from a fresh environment: a failed one may have
    // opened definitions and bound hypothesis names.
    let mut env = fresh_env(inputs, ob, known, using);
    let proof = if algebraic {
        algebra(&mut env, t, ob)?
    } else {
        let mut refusals: Vec<String> = Vec::new();
        let mut found = None;
        // The attempts run again with a call that divides an Int down to
        // zero kept whole, when opening one did not close the part.
        let mut met_halving = false;
        for attempt in 0..8 {
            if attempt == 4 {
                if !met_halving {
                    break;
                }
                refusals.clear();
            }
            env.open_halving = attempt < 4;
            let attempt = attempt % 4;
            let outcome = match attempt {
                0 => match using {
                    // One instance of a cited law, either way round, first.
                    Some(laws) => match env.direct_equal(&ob.lhs, &ob.rhs) {
                        Some(proof) => Ok(proof),
                        None => citations(&mut env, laws.clone(), ob),
                    },
                    None => continue,
                },
                1 => env.prove_by_evaluation(&ob.lhs, &ob.rhs, SPLIT_DEPTH),
                2 if ob.finite.is_empty() => continue,
                2 => env.prove_by_cases(&ob.finite, &ob.lhs, &ob.rhs, SPLIT_DEPTH),
                _ => match induct_on {
                    Some(f) => env.prove_by_induction(f, ob, SPLIT_DEPTH),
                    None => continue,
                },
            };
            match outcome {
                Ok(proof) => {
                    found = Some(proof);
                    break;
                }
                Err(why) => {
                    refusals.push(why);
                    for h in env.hints.drain(..) {
                        if !hints.contains(&h) {
                            hints.push(h);
                        }
                    }
                    met_halving |= env.met_halving.get();
                    env = fresh_env(inputs, ob, known, using);
                }
            }
        }
        found.ok_or_else(|| refusals.join("; "))?
    };
    // The kernel splits on a call only when its definition says the call
    // is a Bool, so a function split on comes with its definition.
    let mut split = Vec::new();
    split_calls(&proof, &mut split);
    for id in split {
        env.mark_used(id);
    }
    Ok(Part {
        proof,
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

/// Prove `t` by steps; `hints` collects the facts that would rewrite where
/// a failed attempt stopped.
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
    t: &LawTheorem,
    hints: &mut Vec<String>,
) -> Produced {
    let refuse = |why: String| Produced {
        law: Err(why),
        obligations: Vec::new(),
    };
    if t.premises.len() > 1 {
        return refuse("more than one premise".into());
    }
    let ob = obligation(inputs, t);
    let using = match &t.using {
        Some(names) if !names.is_empty() => match cited(inputs, ir, t, names) {
            Some(laws) => Some(laws),
            None => return refuse("a cited law has no theorem".into()),
        },
        _ => None,
    };
    let algebraic = matches!(
        t.strategy,
        ProofStrategy::Commutative { .. }
            | ProofStrategy::Associative { .. }
            | ProofStrategy::IdentityElement { .. }
    );
    // Each line of a `when` written over several lines is a hypothesis of
    // its own, cut from `when` by the `Bool.and` eliminations.
    let mut cuts: Vec<Cut> = ob
        .premise
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
        .collect();
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
    let known_of = |cuts: &[Cut]| -> Vec<(String, Term)> {
        cuts.iter()
            .map(|c| (c.name.clone(), c.fact.clone()))
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
            ) => Some(*f),
            _ => None,
        };
        let known = known_of(&cuts);
        let part = prove_part(inputs, t, &part_ob, &known, &using, induct_on, false, hints)
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
        Some(t.fn_id),
        algebraic && n == 0,
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
        let text = crate::ir::proof_steps::sexpr::script(&script, inputs.symbol_table)?;
        crate::proof_kernel::verdict(&text)?;
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
                // What it assumes, in the order its Lean theorem introduces
                // them: the earlier reasons, then the guard.
                let mut names: Vec<String> =
                    (0..i.min(n)).map(|k| format!("h_reason{k}")).collect();
                let mut items: Vec<Term> = reasons[..i.min(n)].to_vec();
                if let Some(h) = &ob.premise {
                    names.push("when".into());
                    items.push(h.clone());
                }
                let view = under_cuts(&cuts[..*scope], part.proof.clone(), false);
                let (premise, proof) = match conjoin(&items) {
                    None => (None, view),
                    Some((whole, from)) => {
                        let mut proof = view;
                        if !(names.len() == 1 && names[0] == "when") {
                            for (name, (fact, p)) in names.iter().zip(items.iter().zip(from)).rev()
                            {
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
                };
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
                        lhs,
                        rhs,
                        ..ob.clone()
                    },
                    defs: Vec::new(),
                    consts: Vec::new(),
                    laws: Vec::new(),
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
            obligations,
        };
    }
    let parts: Vec<&Part> = results
        .iter()
        .map(|(p, _)| p.as_ref().expect("all closed"))
        .collect();
    let proof = under_cuts(&cuts, parts[n].proof.clone(), true);
    let mut script = Script {
        obligation: ob,
        defs: Vec::new(),
        consts: Vec::new(),
        laws: Vec::new(),
        proof,
    };
    for part in parts {
        merge_into(&mut script, part);
    }
    with_constants(&mut script);
    Produced {
        law: check(script),
        obligations,
    }
}

/// Fill `LawTheorem::steps` for every law a producer can prove.
pub(crate) fn populate_law_steps(inputs: &ProofLowerInputs, ir: &mut ProofIR) {
    let debug = std::env::var_os("AVER_STEPS_DEBUG").is_some();
    for i in 0..ir.law_theorems.len() {
        let mut hints = Vec::new();
        let produced = produce(inputs, ir, &ir.law_theorems[i], &mut hints);
        ir.law_theorems[i].obligation_steps = produced.obligations;
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
                ir.law_theorems[i].steps_hints = Vec::new();
            }
            Err(why) => {
                ir.law_theorems[i].steps = None;
                ir.law_theorems[i].steps_refusal = Some(why);
                ir.law_theorems[i].steps_hints = hints;
            }
        }
    }
}

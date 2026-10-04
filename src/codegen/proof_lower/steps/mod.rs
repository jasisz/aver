//! Step producers: the rungs that recognised a law's shape write its proof
//! as data (`crate::ir::proof_steps`) instead of leaving a backend to search.
//!
//! Three producers, tried in order and each checked by
//! [`crate::ir::proof_steps::check::check_script`] before it is kept:
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
//! right, where it stops.
//!
//! A law no producer handles keeps `steps: None` and its tactic portfolio.

mod chain;
mod env;
mod eval;
mod finite;
mod induction;
mod rewrite;

use crate::codegen::proof_lower::ProofLowerInputs;
use crate::ir::proof_steps::check::check_script;
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
        Err(with_reasons(format!(
            "the cited laws rewrite the sides to `{}` and `{}`",
            crate::ir::proof_steps::show::term(left.cur(), names),
            crate::ir::proof_steps::show::term(right.cur(), names)
        )))
    }
}

/// An environment for one attempt: the law's `when` in scope and its
/// cited laws, minus any that would loop, as rewrite rules.
fn fresh_env<'a>(
    inputs: &'a ProofLowerInputs<'a>,
    ob: &Obligation,
    using: &Option<Vec<LawRef>>,
) -> Env<'a> {
    let mut env = Env::new(inputs);
    if let Some(p) = &ob.premise {
        env.hyps.push(("when".into(), chain::eqn_true(p)));
    }
    env.rewrite_laws = using
        .iter()
        .flatten()
        .filter(|l| rewrite::loops(l).is_none())
        .cloned()
        .collect();
    env
}

fn produce(inputs: &ProofLowerInputs, ir: &ProofIR, t: &LawTheorem) -> Result<Script, String> {
    if !t.reason_inductions.is_empty() {
        return Err("a law with `because` steps".into());
    }
    if t.premises.len() > 1 {
        return Err("more than one premise".into());
    }
    let ob = obligation(inputs, t);
    let using = match &t.using {
        Some(names) if !names.is_empty() => {
            Some(cited(inputs, ir, t, names).ok_or("a cited law has no theorem")?)
        }
        _ => None,
    };
    // Each attempt starts from a fresh environment: a failed one may have
    // opened definitions and bound hypothesis names.
    let mut env = fresh_env(inputs, &ob, &using);
    let proof = match &t.strategy {
        ProofStrategy::Commutative { .. }
        | ProofStrategy::Associative { .. }
        | ProofStrategy::IdentityElement { .. } => algebra(&mut env, t, &ob)?,
        _ => {
            let mut refusals: Vec<String> = Vec::new();
            let mut found = None;
            for attempt in 0..4 {
                let outcome = match attempt {
                    0 => match &using {
                        Some(laws) => citations(&mut env, laws.clone(), &ob),
                        None => continue,
                    },
                    1 => env.prove_by_evaluation(&ob.lhs, &ob.rhs, SPLIT_DEPTH),
                    2 if ob.finite.is_empty() => continue,
                    2 => env.prove_by_cases(&ob.finite, &ob.lhs, &ob.rhs, SPLIT_DEPTH),
                    _ => env.prove_by_induction(t.fn_id, &ob, SPLIT_DEPTH),
                };
                match outcome {
                    Ok(proof) => {
                        found = Some(proof);
                        break;
                    }
                    Err(why) => {
                        refusals.push(why);
                        env = fresh_env(inputs, &ob, &using);
                    }
                }
            }
            found.ok_or_else(|| refusals.join("; "))?
        }
    };
    if proof.size() > MAX_PROOF_NODES {
        return Err(format!(
            "{} steps is more than a backend should elaborate",
            proof.size()
        ));
    }
    let script = Script {
        obligation: ob,
        defs: env.used_defs(),
        consts: env.used_consts(),
        laws: env.laws.clone(),
        proof,
    };
    check_script(&script)?;
    // A script the kernel could not read back would be a producer error
    // later; refuse it here, by name.
    crate::ir::proof_steps::sexpr::script(&script, inputs.symbol_table)?;
    Ok(script)
}

/// Fill `LawTheorem::steps` for every law a producer can prove.
pub(crate) fn populate_law_steps(inputs: &ProofLowerInputs, ir: &mut ProofIR) {
    let debug = std::env::var_os("AVER_STEPS_DEBUG").is_some();
    for i in 0..ir.law_theorems.len() {
        let result = produce(inputs, ir, &ir.law_theorems[i]);
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
            }
            Err(why) => {
                ir.law_theorems[i].steps = None;
                ir.law_theorems[i].steps_refusal = Some(why);
            }
        }
    }
}

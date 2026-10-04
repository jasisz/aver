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
//!   guard, until both sides are the same term.
//!
//! A law no producer handles keeps `steps: None` and its tactic portfolio.

mod chain;
mod env;
mod eval;
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

/// The laws a `using` list names, resolved against the citing law's module.
fn cited(
    inputs: &ProofLowerInputs,
    ir: &ProofIR,
    t: &LawTheorem,
    names: &[String],
) -> Option<Vec<LawRef>> {
    let own_scope = inputs.symbol_table.fn_entry(t.fn_id).key.scope.clone();
    let mut out = Vec::new();
    for name in names {
        let found = ir.law_theorems.iter().find(|c| {
            let key = &inputs.symbol_table.fn_entry(c.fn_id).key;
            let local = format!("{}.{}", key.name, c.law_name);
            let qualified = law_key(inputs, c);
            (key.scope == own_scope && *name == local) || *name == qualified
        })?;
        out.push(law_ref(inputs, found));
    }
    Some(out)
}

fn obligation(inputs: &ProofLowerInputs, t: &LawTheorem) -> Obligation {
    Obligation {
        key: law_key(inputs, t),
        givens: t.quantifiers.iter().map(|q| q.name.clone()).collect(),
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
    let mut eqs: Vec<Equation> = laws
        .iter()
        .cloned()
        .map(|l| Equation::Law(Box::new(l)))
        .collect();
    eqs.push(Equation::Wall(WallRule::DivModRecompose));
    env.laws = laws;
    let left = env.rewrite(&ob.lhs, &eqs)?;
    let right = env.rewrite(&ob.rhs, &eqs)?;
    if left.cur() == right.cur() {
        Ok(chain::meet(left, right))
    } else {
        Err("the cited laws do not rewrite the sides to one term".into())
    }
}

fn produce(inputs: &ProofLowerInputs, ir: &ProofIR, t: &LawTheorem) -> Result<Script, String> {
    if !t.reason_inductions.is_empty() {
        return Err("a law with `because` steps".into());
    }
    if t.premises.len() > 1 {
        return Err("more than one premise".into());
    }
    let ob = obligation(inputs, t);
    let mut env = Env::new(inputs);
    if let Some(p) = &ob.premise {
        env.hyps.push(("when".into(), chain::eqn_true(p)));
    }
    let proof = match (&t.strategy, &t.using) {
        (
            ProofStrategy::Commutative { .. }
            | ProofStrategy::Associative { .. }
            | ProofStrategy::IdentityElement { .. },
            _,
        ) => algebra(&mut env, t, &ob)?,
        (_, Some(names)) if !names.is_empty() => {
            let laws = cited(inputs, ir, t, names).ok_or("a cited law has no theorem")?;
            citations(&mut env, laws, &ob)?
        }
        _ => env.prove_by_evaluation(&ob.lhs, &ob.rhs, SPLIT_DEPTH)?,
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
        ir.law_theorems[i].steps = result.ok();
    }
}

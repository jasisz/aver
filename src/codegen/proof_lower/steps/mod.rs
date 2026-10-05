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
fn split_and(t: &Term, proof: Proof, out: &mut Vec<(Term, Proof)>) {
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

/// A hypothesis `name : fact = true` whose fact calls a predicate that does
/// not recurse, opened once: its body as hypothesis `<name>_open`, from the
/// definition's whole-body equation and the hypothesis, then the body's
/// `Bool.and` lines as `<name>_open1`, `<name>_open2`, … like a `when`'s.
/// Nothing for any other fact, or for a body that is a `match`.
fn open_hypothesis(
    inputs: &ProofLowerInputs,
    name: &str,
    fact: &Term,
    opened: &mut Vec<crate::ir::proof_steps::Def>,
) -> Vec<(String, Term, Option<Proof>)> {
    use crate::ir::hir::{ResolvedCallee, ResolvedExpr};
    use crate::ir::proof_steps::term;
    let fact = canon(fact);
    let ResolvedExpr::Call(ResolvedCallee::Fn(id), args) = &fact.node else {
        return Vec::new();
    };
    if inputs.recursive_fns.contains(id) {
        return Vec::new();
    }
    let Some(def) = Env::new(inputs).def(*id) else {
        return Vec::new();
    };
    if matches!(def.body.node, ResolvedExpr::Match { .. }) {
        return Vec::new();
    }
    let args: Vec<Term> = args.iter().map(canon).collect();
    let Ok(outer) = def.outer(&args) else {
        return Vec::new();
    };
    let Ok(body) = term::subst(&def.body, &outer) else {
        return Vec::new();
    };
    let body = canon(&body);
    let open = format!("{name}_open");
    let from = Proof::Trans {
        terms: vec![body.clone(), fact.clone(), term::boolean(true)],
        steps: vec![
            Proof::Symm(Box::new(Proof::Unfold {
                fn_id: *id,
                arm: 0,
                args,
                binders: Vec::new(),
                premise: None,
            })),
            Proof::Hyp(name.into()),
        ],
    };
    if !opened.iter().any(|d| d.fn_id == *id) {
        opened.push(def);
    }
    let mut out = vec![(open.clone(), body.clone(), Some(from))];
    let mut lines = Vec::new();
    split_and(&body, Proof::Hyp(open.clone()), &mut lines);
    if lines.len() > 1 {
        for (i, (line, proof)) in lines.into_iter().enumerate() {
            out.push((format!("{open}{}", i + 1), line, Some(proof)));
        }
    }
    out
}

/// Whether `p` opens definition `f` anywhere.
fn uses_unfold(p: &Proof, f: crate::ir::identity::FnId) -> bool {
    let any = |ps: &[Proof]| ps.iter().any(|q| uses_unfold(q, f));
    match p {
        Proof::Unfold { fn_id, premise, .. } => {
            *fn_id == f || premise.as_ref().is_some_and(|q| uses_unfold(q, f))
        }
        Proof::Induct { fn_id, cases, .. } => {
            *fn_id == f || cases.iter().any(|c| uses_unfold(&c.proof, f))
        }
        Proof::Symm(q) | Proof::Congr { inner: q, .. } => uses_unfold(q, f),
        Proof::Absurd { contradiction, .. } => uses_unfold(contradiction, f),
        Proof::Arm { premise, .. } => uses_unfold(premise, f),
        Proof::Law { premise, .. } => premise.as_ref().is_some_and(|q| uses_unfold(q, f)),
        Proof::Trans { steps, .. } => any(steps),
        Proof::Rule { premises, .. } => any(premises),
        Proof::Enum { cases, .. } => any(cases),
        Proof::Cases {
            if_true, if_false, ..
        } => uses_unfold(if_true, f) || uses_unfold(if_false, f),
        Proof::Have { proof, body, .. } => uses_unfold(proof, f) || uses_unfold(body, f),
        Proof::InductList { nil, cons, .. } => uses_unfold(nil, f) || uses_unfold(cons, f),
        Proof::Refl(_)
        | Proof::Hyp(_)
        | Proof::Linear { .. }
        | Proof::UnfoldConst { .. }
        | Proof::Proj { .. }
        | Proof::Cell { .. }
        | Proof::Compute { .. }
        | Proof::Ring { .. } => false,
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
        Proof::Have { proof, body, .. } => uses_hyp(proof, name) || uses_hyp(body, name),
        Proof::Induct { cases, .. } => cases.iter().any(|c| uses_hyp(&c.proof, name)),
        Proof::InductList { nil, cons, .. } => uses_hyp(nil, name) || uses_hyp(cons, name),
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
        for attempt in 0..4 {
            let outcome = match attempt {
                0 => match using {
                    Some(laws) => citations(&mut env, laws.clone(), ob),
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
                    env = fresh_env(inputs, ob, known, using);
                }
            }
        }
        found.ok_or_else(|| refusals.join("; "))?
    };
    Ok(Part {
        proof,
        defs: env.used_defs(),
        consts: env.used_consts(),
        laws: env.laws.clone(),
    })
}

/// Prove `t` by steps; `hints` collects the facts that would rewrite where
/// a failed attempt stopped.
///
/// A law with `because` lines is proved the way the user argued it: with
/// guard `H` and reasons `R1 … Rn`, each `Ri` under `H` and the reasons
/// before it, then the claim under all of them. The script states the law
/// itself and proves it by one cut ([`Proof::Have`]) per reason, so the
/// kernel checks the composition too.
fn produce(
    inputs: &ProofLowerInputs,
    ir: &ProofIR,
    t: &LawTheorem,
    hints: &mut Vec<String>,
) -> Result<Script, String> {
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
    let algebraic = matches!(
        t.strategy,
        ProofStrategy::Commutative { .. }
            | ProofStrategy::Associative { .. }
            | ProofStrategy::IdentityElement { .. }
    );
    // Each line of a `when` written over several lines is a hypothesis of
    // its own, cut from `when` by the `Bool.and` eliminations; a hypothesis
    // that calls a predicate is opened once to its body, split the same way.
    // The cuts, in scope order: `None` marks reason i, proved in its part.
    let mut cuts: Vec<(String, Term, Option<Proof>)> = Vec::new();
    let mut opened: Vec<crate::ir::proof_steps::Def> = Vec::new();
    let lines = ob.premise.as_ref().map(when_lines).unwrap_or_default();
    if lines.is_empty() {
        if let Some(p) = &ob.premise {
            cuts.extend(open_hypothesis(inputs, "when", p, &mut opened));
        }
    } else {
        for (i, (fact, from_when)) in lines.iter().enumerate() {
            let name = format!("when{}", i + 1);
            cuts.push((name.clone(), fact.clone(), Some(from_when.clone())));
            cuts.extend(open_hypothesis(inputs, &name, fact, &mut opened));
        }
    }
    let known_of = |cuts: &[(String, Term, Option<Proof>)]| -> Vec<(String, Term)> {
        cuts.iter()
            .map(|(n, f, _)| (n.clone(), f.clone()))
            .collect()
    };
    let reasons: Vec<Term> = t.reasons.iter().map(|r| law_term(inputs, t, r)).collect();
    let n = reasons.len();
    let mut parts: Vec<Part> = Vec::new();
    for (i, reason) in reasons.iter().enumerate() {
        let part_ob = Obligation {
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
            .map_err(|why| format!("reason {} of {n}: {why}", i + 1))?;
        let name = format!("because{}", i + 1);
        cuts.push((name.clone(), reason.clone(), Some(part.proof.clone())));
        parts.push(part);
        cuts.extend(open_hypothesis(inputs, &name, reason, &mut opened));
    }
    let known = known_of(&cuts);
    let last = prove_part(
        inputs,
        t,
        &ob,
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
    })?;
    // A reason is always cut; a `when` line or an opened hypothesis only
    // when something after it reads it.
    let mut proof = last.proof.clone();
    for (name, fact, from) in cuts.into_iter().rev() {
        let reason = name.starts_with("because") && name[7..].bytes().all(|b| b.is_ascii_digit());
        if reason || uses_hyp(&proof, &name) {
            proof = Proof::Have {
                name,
                fact,
                proof: Box::new(from.expect("every cut has its proof")),
                body: Box::new(proof),
            };
        }
    }
    if proof.size() > MAX_PROOF_NODES {
        return Err(format!(
            "{} steps is more than a backend should elaborate",
            proof.size()
        ));
    }
    let mut script = Script {
        obligation: ob,
        defs: opened
            .into_iter()
            .filter(|d| uses_unfold(&proof, d.fn_id))
            .collect(),
        consts: Vec::new(),
        laws: Vec::new(),
        proof,
    };
    for part in parts.iter().chain(std::iter::once(&last)) {
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
    // The kernel judges what the producers built; a script it refuses,
    // or cannot read back, is refused here by the kernel's reason.
    let text = crate::ir::proof_steps::sexpr::script(&script, inputs.symbol_table)?;
    crate::proof_kernel::verdict(&text)?;
    Ok(script)
}

/// Fill `LawTheorem::steps` for every law a producer can prove.
pub(crate) fn populate_law_steps(inputs: &ProofLowerInputs, ir: &mut ProofIR) {
    let debug = std::env::var_os("AVER_STEPS_DEBUG").is_some();
    for i in 0..ir.law_theorems.len() {
        let mut hints = Vec::new();
        let result = produce(inputs, ir, &ir.law_theorems[i], &mut hints);
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

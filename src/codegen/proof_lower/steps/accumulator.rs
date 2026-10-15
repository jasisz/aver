//! A hint for a law about a wrapper that starts an accumulator at a
//! constant: `sum(xs) => sumDirect(xs)` where `sum(xs)` is `sumTR(xs, 0)`.
//!
//! No induction closes such a law as stated: the recursive call moves the
//! accumulator (`sumTR(t, 0 + h)`), and the claim speaks of it only at its
//! start. The law that does close states the recursion with the accumulator
//! as a given (`sumTR(xs, acc) => acc + sumDirect(xs)`), and the wrapper's
//! law follows from it at the start. Which law that is cannot be read off
//! the program, so the steps do not prove one behind the author's back:
//! they try a short, fixed list of shapes and, when one closes and the
//! wrapper's law closes citing it, offer its text as a hint. The law stays
//! open until its author adds what the hint says.
//!
//! The shape is read off the terms, never off names:
//! - the claim's left side, evaluated, is one call of a function that
//!   recurses, with a given where it recurses;
//! - exactly one other argument is a constant at a place the function's
//!   recursive calls change: the accumulator and its start;
//! - the start is neutral for an operation of the accumulator's type: `0`
//!   for `+`, `1` for `*`, `[]` for `List.concat`.
//!
//! With `R` the law's right side and `acc` a fresh given in the start's
//! place, the candidates are `acc + R` and `R + acc`, `acc * R` and
//! `R * acc`, or `List.concat(R, acc)` and `List.concat(acc, R)`: each is
//! `R` at the start, by the neutral element. A candidate is first checked
//! on samples (the law's own, and a few values of `acc`), then proved by
//! the steps like any law; the first that closes, and under which the
//! wrapper's law closes too, is the hint.

use crate::ast::{BinOp, TopLevel, Type, VerifyGivenDomain, VerifyKind, VerifyLaw};
use crate::codegen::proof_lower::ProofLowerInputs;
use crate::ir::hir::{ResolvedCallee, ResolvedExpr};
use crate::ir::identity::FnId;
use crate::ir::proof_steps::term::{self, Term, canon};
use crate::ir::proof_steps::{induct, show};
use crate::ir::{LawTheorem, ProofIR, Quantifier, QuantifierType};

use super::env::Env;

/// The name the hinted law takes.
const LAW_NAME: &str = "accGeneralizes";

/// Most samples of the law's givens a candidate is checked on.
const MAX_SAMPLES: usize = 4;

/// The operation an accumulator's start is neutral for.
#[derive(Clone, Copy, PartialEq)]
enum Join {
    Add,
    Mul,
    Concat,
}

/// The law `name` about function `fn_name` as the entry file states it.
fn source_law<'a>(
    inputs: &'a ProofLowerInputs,
    fn_name: &str,
    name: &str,
) -> Option<&'a VerifyLaw> {
    inputs.entry_items.iter().find_map(|item| match item {
        TopLevel::Verify(vb) if vb.fn_name == fn_name => match &vb.kind {
            VerifyKind::Law(law) if law.name == name => Some(&**law),
            _ => None,
        },
        _ => None,
    })
}

/// Where the entry file defines function `name`, and its parameters' types.
fn source_fn<'a>(
    inputs: &'a ProofLowerInputs,
    name: &str,
) -> Option<(usize, &'a [(String, String)])> {
    inputs
        .entry_items
        .iter()
        .enumerate()
        .find_map(|(at, item)| match item {
            TopLevel::FnDef(f) if f.name == name => Some((at, f.params.as_slice())),
            _ => None,
        })
}

/// `given name: Type = domain`, as the law wrote it.
fn given_line(g: &crate::ast::VerifyGiven) -> String {
    let domain = match &g.domain {
        VerifyGivenDomain::IntRange { start, end } => format!("{start}..{end}"),
        VerifyGivenDomain::Explicit(values) => format!(
            "[{}]",
            values
                .iter()
                .map(crate::checker::expr_to_str)
                .collect::<Vec<_>>()
                .join(", ")
        ),
    };
    format!("given {}: {} = {domain}", g.name, g.type_name)
}

/// A sample value written in an entry law, as a term: no given is in scope
/// there, so a name in it reads a module-level binding.
fn sample_term(inputs: &ProofLowerInputs, e: &crate::ast::Spanned<crate::ast::Expr>) -> Term {
    super::env::qualify_bindings(&canon(&inputs.resolve_expr(e, None)), inputs, None, &[])
}

/// The sample values of a given, as terms.
fn samples(inputs: &ProofLowerInputs, g: &crate::ast::VerifyGiven) -> Vec<Term> {
    match &g.domain {
        VerifyGivenDomain::IntRange { start, end } => (*start..=*end)
            .take(MAX_SAMPLES)
            .map(|v| term::int(&v.into()))
            .collect(),
        VerifyGivenDomain::Explicit(values) => values
            .iter()
            .take(MAX_SAMPLES)
            .map(|e| sample_term(inputs, e))
            .collect(),
    }
}

/// What the hint is built from: the recursion's call with the accumulator
/// as a given, the candidates for the other side, and the samples of the
/// accumulator as the hinted law writes them and as terms.
struct Shape {
    /// The function that recurses.
    worker: FnId,
    worker_name: String,
    acc: String,
    acc_type: String,
    lhs: Term,
    candidates: Vec<Term>,
    acc_samples: Vec<(String, Term)>,
}

/// The shape of the module comment in law `t`, if it has it.
fn shape(
    inputs: &ProofLowerInputs,
    ir: &ProofIR,
    t: &LawTheorem,
    law: &VerifyLaw,
) -> Option<Shape> {
    let symbols = inputs.symbol_table;
    let mut env = Env::new(inputs);
    let lhs = super::law_term(inputs, t, &t.claim_lhs);
    let rhs = super::law_term(inputs, t, &t.claim_rhs);
    let stopped = canon(env.normalize(&lhs, 8).ok()?.cur());
    let ResolvedExpr::Call(ResolvedCallee::Fn(worker), args) = &stopped.node else {
        return None;
    };
    let worker = *worker;
    let key = &symbols.fn_entry(worker).key;
    if key.scope.is_some() {
        return None;
    }
    let def = env.raw_def(worker)?;
    let ds = env.reachable_defs(worker);
    let rec = induct::recursion(&def, &ds).ok()??;
    let givens: Vec<String> = t.quantifiers.iter().map(|q| q.name.clone()).collect();
    if !matches!(args.get(rec.at).map(|a| &a.node), Some(ResolvedExpr::Ident(n)) if givens.contains(n))
    {
        return None;
    }
    // The places the recursive calls change.
    let ResolvedExpr::Match { arms, .. } = &def.body.node else {
        return None;
    };
    let mut changed = vec![false; def.params.len()];
    for arm in arms {
        for (call, _) in induct::self_calls(&arm.body, worker, &ds).ok()? {
            for (k, a) in call.iter().enumerate() {
                let same = matches!(&canon(a).node, ResolvedExpr::Ident(n) if *n == def.params[k]);
                if k != rec.at && !same {
                    changed[k] = true;
                }
            }
        }
    }
    // Exactly one of them holds a constant: the accumulator at its start.
    let constant = |a: &Term| {
        term::is_literal(a) || matches!(&a.node, ResolvedExpr::List(xs) if xs.is_empty())
    };
    let starts: Vec<usize> = (0..args.len())
        .filter(|k| changed.get(*k).copied().unwrap_or(false) && constant(&args[*k]))
        .collect();
    let [at] = starts.as_slice() else {
        return None;
    };
    let at = *at;
    let start = &args[at];
    let (worker_at, params) = source_fn(inputs, &key.name)?;
    let acc_type = params.get(at)?.1.clone();
    let join = match (
        crate::types::parse_type_str(&acc_type),
        term::int_value(start),
    ) {
        (Type::Int, Some(v)) if v == 0.into() => Join::Add,
        (Type::Int, Some(v)) if v == 1.into() => Join::Mul,
        (Type::List(_), None) => Join::Concat,
        _ => return None,
    };
    // The hinted law is stated about the worker and cited by this one, so
    // the worker is defined first; and it is not there already.
    let own = &symbols.fn_entry(t.fn_id).key;
    if own.scope.is_some() || source_fn(inputs, &own.name)?.0 < worker_at {
        return None;
    }
    if ir
        .law_theorems
        .iter()
        .any(|c| c.fn_id == worker && c.law_name == LAW_NAME)
    {
        return None;
    }
    let acc = (0..)
        .map(|n| match n {
            0 => "acc".to_string(),
            n => format!("acc{n}"),
        })
        .find(|n| !givens.contains(n))?;
    let var = term::var(&acc);
    var.set_ty(crate::types::parse_type_str(&acc_type));
    let mut with_acc = args.clone();
    with_acc[at] = var.clone();
    let call = crate::ast::Spanned::bare(ResolvedExpr::Call(ResolvedCallee::Fn(worker), with_acc));
    if let Some(ty) = stopped.ty() {
        call.set_ty(ty.clone());
    }
    let typed = |t: Term, ty: Type| {
        t.set_ty(ty);
        t
    };
    let candidates = match join {
        Join::Add => vec![
            typed(term::binop(BinOp::Add, var.clone(), rhs.clone()), Type::Int),
            typed(term::binop(BinOp::Add, rhs.clone(), var.clone()), Type::Int),
        ],
        Join::Mul => vec![
            typed(term::binop(BinOp::Mul, var.clone(), rhs.clone()), Type::Int),
            typed(term::binop(BinOp::Mul, rhs.clone(), var.clone()), Type::Int),
        ],
        Join::Concat => {
            let ty = crate::types::parse_type_str(&acc_type);
            vec![
                term::builtin(
                    "List.concat",
                    vec![rhs.clone(), var.clone()],
                    Some(ty.clone()),
                ),
                term::builtin("List.concat", vec![var.clone(), rhs.clone()], Some(ty)),
            ]
        }
    };
    // The start first, then values that tell the candidates apart.
    let acc_samples: Vec<(String, Term)> = match join {
        Join::Add => [0, 1, 5]
            .iter()
            .map(|v| (v.to_string(), term::int(&(*v).into())))
            .collect(),
        Join::Mul => [1, 2, 3]
            .iter()
            .map(|v| (v.to_string(), term::int(&(*v).into())))
            .collect(),
        Join::Concat => {
            let mut out = vec![("[]".to_string(), term::nil())];
            for g in law.givens.iter().filter(|g| g.type_name == acc_type) {
                let VerifyGivenDomain::Explicit(values) = &g.domain else {
                    continue;
                };
                for e in values {
                    let text = crate::checker::expr_to_str(e);
                    if out.len() < 3 && !out.iter().any(|(t, _)| *t == text) {
                        out.push((text, sample_term(inputs, e)));
                    }
                }
            }
            out
        }
    };
    Some(Shape {
        worker,
        worker_name: key.name.clone(),
        acc,
        acc_type,
        lhs: canon(&call),
        candidates,
        acc_samples,
    })
}

/// Whether `lhs = rhs` holds on the law's samples and each sample of the
/// accumulator, by evaluation.
fn holds_on_samples(inputs: &ProofLowerInputs, law: &VerifyLaw, shape: &Shape, rhs: &Term) -> bool {
    let per_given: Vec<(String, Vec<Term>)> = law
        .givens
        .iter()
        .map(|g| (g.name.clone(), samples(inputs, g)))
        .collect();
    if per_given.iter().any(|(_, vs)| vs.is_empty()) {
        return false;
    }
    let rounds = per_given
        .iter()
        .map(|(_, vs)| vs.len())
        .max()
        .unwrap_or(1)
        .min(MAX_SAMPLES);
    for round in 0..rounds {
        for (_, acc) in &shape.acc_samples {
            let mut at: Vec<(String, Term)> = per_given
                .iter()
                .map(|(n, vs)| (n.clone(), vs[round % vs.len()].clone()))
                .collect();
            at.push((shape.acc.clone(), acc.clone()));
            let (Ok(l), Ok(r)) = (term::subst(&shape.lhs, &at), term::subst(rhs, &at)) else {
                return false;
            };
            if Env::new(inputs).prove_by_evaluation(&l, &r, 2).is_err() {
                return false;
            }
        }
    }
    true
}

/// The hint for law `i`, which the steps left open: the text of a law about
/// the recursion with its accumulator as a given, which the steps close,
/// and the `using` line under which they then close law `i`. `None` when
/// the law does not have the shape, or no candidate does both.
pub(super) fn hint(inputs: &ProofLowerInputs, ir: &ProofIR, i: usize) -> Option<String> {
    let t = &ir.law_theorems[i];
    if !t.premises.is_empty() || !t.reasons.is_empty() || t.by_plan.is_some() {
        return None;
    }
    let symbols = inputs.symbol_table;
    let own = &symbols.fn_entry(t.fn_id).key;
    let law = source_law(inputs, &own.name, &t.law_name)?;
    let shape = shape(inputs, ir, t, law)?;
    let cited = format!("{}.{LAW_NAME}", shape.worker_name);
    for rhs in &shape.candidates {
        if !holds_on_samples(inputs, law, &shape, rhs) {
            continue;
        }
        // The givens the hinted law speaks of, in the law's order, then the
        // accumulator.
        let mut free = Vec::new();
        term::free_vars(&shape.lhs, &mut free);
        term::free_vars(rhs, &mut free);
        let mut quantifiers: Vec<Quantifier> = t
            .quantifiers
            .iter()
            .filter(|q| free.contains(&q.name))
            .cloned()
            .collect();
        quantifiers.push(Quantifier {
            name: shape.acc.clone(),
            binder_type: QuantifierType::Plain(shape.acc_type.clone()),
        });
        let general = LawTheorem {
            fn_id: shape.worker,
            law_name: LAW_NAME.to_string(),
            quantifiers,
            claim_lhs: shape.lhs.clone(),
            claim_rhs: rhs.clone(),
            using: None,
            induction_given: None,
            steps: None,
            steps_refusal: None,
            steps_hints: Vec::new(),
            steps_auto_using: Vec::new(),
            obligation_steps: Vec::new(),
            steps_timing: Default::default(),
            ..t.clone()
        };
        // The candidate as the steps would prove it standing before law
        // `i`: without earlier laws first, then with those they closed.
        let mut probe = ir.clone();
        probe.law_theorems.truncate(i);
        probe.law_theorems.push(general);
        let mut pinned: Vec<String> = Vec::new();
        let mut closed = super::produce(inputs, &probe, i, &[], &mut super::Found::default()).law;
        if closed.is_err() {
            let pool = super::auto_pool(inputs, &probe, i);
            if !pool.is_empty() {
                closed = super::produce(inputs, &probe, i, &pool, &mut super::Found::default()).law;
                if let Ok(script) = &closed {
                    pinned = pool
                        .iter()
                        .filter(|l| script.laws.iter().any(|s| s.key == l.key))
                        .map(|l| l.key.clone())
                        .collect();
                }
            }
        }
        let Ok(script) = closed else {
            continue;
        };
        // Then law `i` citing it.
        probe.law_theorems[i].steps = Some(script);
        let mut using = t.using.clone().unwrap_or_default();
        using.push(cited.clone());
        probe.law_theorems.push(LawTheorem {
            using: Some(using.clone()),
            ..t.clone()
        });
        if super::produce(inputs, &probe, i + 1, &[], &mut super::Found::default())
            .law
            .is_err()
        {
            continue;
        }
        let mut text = format!(
            "steps close this law once the accumulator of `{}` is a given; add this law before it:\n\n",
            shape.worker_name
        );
        text.push_str(&format!(
            "    verify {} law {LAW_NAME}\n",
            shape.worker_name
        ));
        for g in law.givens.iter().filter(|g| free.contains(&g.name)) {
            text.push_str(&format!("        {}\n", given_line(g)));
        }
        text.push_str(&format!(
            "        given {}: {} = [{}]\n",
            shape.acc,
            shape.acc_type,
            shape
                .acc_samples
                .iter()
                .map(|(s, _)| s.as_str())
                .collect::<Vec<_>>()
                .join(", ")
        ));
        if !pinned.is_empty() {
            text.push_str(&format!("        using [{}]\n", pinned.join(", ")));
        }
        text.push_str(&format!(
            "        {} => {}\n\n",
            show::term(&shape.lhs, symbols),
            show::term(rhs, symbols)
        ));
        text.push_str(&format!(
            "    and this line to `{}.{}`:\n\n        using [{}]",
            own.name,
            t.law_name,
            using.join(", ")
        ));
        return Some(text);
    }
    None
}

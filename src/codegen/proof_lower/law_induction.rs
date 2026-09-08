//! Law induction follows a checked source input and the actual recursive
//! arguments. Both targets receive resolved expressions and canonical IDs.
//! The emitted recursive lemma calls remain obligations of the target checker.

use std::collections::{BTreeMap, BTreeSet};

use crate::ast::{BinOp, Expr, FnDef, Literal, Pattern, Spanned, Stmt, VerifyLaw};
use crate::ir::proof_ir::{
    FuelMetric, LawInduction, LawInductionCall, LawInductionListCase, LawInductionMeasure, ProofIR,
    RecursionContract,
};

use super::ProofLowerInputs;

fn ident(expr: &Spanned<Expr>) -> Option<&str> {
    match &expr.node {
        Expr::Ident(name) | Expr::Resolved { name, .. } => Some(name),
        _ => None,
    }
}

fn var(name: &str) -> Spanned<Expr> {
    Spanned::new(Expr::Ident(name.to_string()), 0)
}

fn int(value: i64) -> Spanned<Expr> {
    Spanned::new(Expr::Literal(Literal::Int(value)), 0)
}

fn call(name: &str, args: Vec<Spanned<Expr>>) -> Spanned<Expr> {
    Spanned::new(Expr::FnCall(Box::new(var(name)), args), 0)
}

/// Simultaneous value substitution; callees and fields are not value names.
/// Binder-bearing expressions are omitted until their scopes are represented
/// here. Preserving types matters for discharged division and empty containers.
fn substitute(
    expr: &Spanned<Expr>,
    bindings: &BTreeMap<String, Spanned<Expr>>,
) -> Option<Spanned<Expr>> {
    let rec = |e| substitute(e, bindings);
    let node = match &expr.node {
        Expr::Ident(name) | Expr::Resolved { name, .. } => {
            let replacement = bindings.get(name)?;
            return Some(if ident(replacement) == Some(name.as_str()) {
                expr.clone()
            } else {
                replacement.clone()
            });
        }
        Expr::Literal(_) => return Some(expr.clone()),
        Expr::BinOp(op, a, b) => Expr::BinOp(*op, Box::new(rec(a)?), Box::new(rec(b)?)),
        Expr::Neg(e) => Expr::Neg(Box::new(rec(e)?)),
        Expr::List(xs) => Expr::List(xs.iter().map(rec).collect::<Option<_>>()?),
        Expr::Tuple(xs) => Expr::Tuple(xs.iter().map(rec).collect::<Option<_>>()?),
        Expr::Attr(base, field) => Expr::Attr(Box::new(rec(base)?), field.clone()),
        Expr::FnCall(callee, args) => {
            // A parameter used as a callable needs a callback substitution
            // contract; do not mistake it for a static source declaration.
            if ident(callee).is_some_and(|name| bindings.contains_key(name)) {
                return None;
            }
            Expr::FnCall(callee.clone(), args.iter().map(rec).collect::<Option<_>>()?)
        }
        _ => return None,
    };
    let result = Spanned::new(node, expr.line);
    if let Some(ty) = expr.ty() {
        result.set_ty(ty.clone());
    }
    Some(result)
}

fn self_args<'a>(
    expr: &'a Spanned<Expr>,
    id: crate::ir::FnId,
    inputs: &ProofLowerInputs,
    scope: Option<&str>,
) -> Option<&'a [Spanned<Expr>]> {
    let (name, args) = match &expr.node {
        Expr::FnCall(callee, args) => (crate::checker::expr_to_str(callee), args.as_slice()),
        Expr::TailCall(tc) => (tc.target.clone(), tc.args.as_slice()),
        _ => return None,
    };
    (inputs.symbol_table.resolve_fn_id_in(&name, scope) == Some(id)).then_some(args)
}

struct SourceBranch<'a> {
    guard: Spanned<Expr>,
    body: &'a Spanned<Expr>,
    list_bindings: Option<(String, String)>,
}

/// A single outer branch exposes an unambiguous source guard and substitution.
/// Nested matches/local bindings stay with existing strategies, never guessed.
fn recursive_branch<'a>(
    fd: &'a FnDef,
    driver: &str,
    measure: LawInductionMeasure,
    id: crate::ir::FnId,
    inputs: &ProofLowerInputs,
    scope: Option<&str>,
) -> Option<SourceBranch<'a>> {
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return None;
    };
    let Expr::Match { subject, arms } = &body.node else {
        return None;
    };
    if arms.len() != 2 {
        return None;
    }
    match measure {
        LawInductionMeasure::SequenceLength => {
            if ident(subject)? != driver {
                return None;
            }
            arms.iter()
                .find(|arm| matches!(arm.pattern, Pattern::EmptyList))?;
            let arm = arms
                .iter()
                .find(|arm| matches!(arm.pattern, Pattern::Cons(_, _)))?;
            let Pattern::Cons(head, tail) = &arm.pattern else {
                return None;
            };
            let guard = Spanned::new(
                Expr::BinOp(
                    BinOp::Gt,
                    Box::new(call("List.len", vec![var(driver)])),
                    Box::new(int(0)),
                ),
                body.line,
            );
            Some(SourceBranch {
                guard,
                body: &arm.body,
                list_bindings: Some((head.clone(), tail.clone())),
            })
        }
        LawInductionMeasure::NonnegativeInt => {
            // The function contract has checked positivity at every call.
            // Keep the actual branch test, including its orientation.
            let recursive = arms.iter().find(|arm| {
                crate::codegen::expr_walk::any(&arm.body, &mut |e| {
                    self_args(e, id, inputs, scope).is_some()
                })
            })?;
            let guard = match recursive.pattern {
                Pattern::Literal(Literal::Bool(true)) => (**subject).clone(),
                Pattern::Literal(Literal::Bool(false)) => {
                    call("Bool.not", vec![(**subject).clone()])
                }
                _ => return None,
            };
            Some(SourceBranch {
                guard,
                body: &recursive.body,
                list_bindings: None,
            })
        }
    }
}

pub(super) fn plan(
    law: &VerifyLaw,
    id: crate::ir::FnId,
    inputs: &ProofLowerInputs,
    ir: &ProofIR,
    scope: Option<&str>,
) -> Option<LawInduction> {
    let (param, measure) = match ir.fn_contracts.get(&id)?.recursion.as_ref()? {
        RecursionContract::WellFoundedToNat { param, .. } => {
            (param, LawInductionMeasure::NonnegativeInt)
        }
        RecursionContract::Fuel {
            fuel_metric: FuelMetric::SeqLenPlusOne { param },
        } => (param, LawInductionMeasure::SequenceLength),
        _ => return None,
    };
    let name = &inputs.symbol_table.fn_entry(id).key.name;
    let fd = inputs
        .pure_fns_in_scope(scope)
        .into_iter()
        .find(|fd| fd.name == *name)?;
    let driver_index = fd.params.iter().position(|(p, _)| p == param)?;
    let mut occurrences = Vec::new();
    for expr in [&law.lhs, &law.rhs] {
        crate::codegen::expr_walk::walk(expr, &mut |e| {
            if self_args(e, id, inputs, scope).is_some() {
                occurrences.push(e);
            }
        });
    }
    // A fixed seed is not a quantified induction parameter. Recurse on the
    // varying givens while retaining that seed in the theorem statement; the
    // backend must prove the resulting equation (often using an accumulator
    // decomposition law). Never infer a bound from concrete sample values.
    fn fixed(expr: &Spanned<Expr>) -> bool {
        match &expr.node {
            Expr::Literal(_) => true,
            Expr::List(xs) | Expr::Tuple(xs) => xs.iter().all(fixed),
            _ => false,
        }
    }
    let full_anchor = occurrences.iter().copied().find(|e| {
        let args = self_args(e, id, inputs, scope).unwrap();
        let mut names = BTreeSet::new();
        args.len() == fd.params.len()
            && args.iter().all(|arg| {
                ident(arg).is_some_and(|name| {
                    law.givens.iter().any(|g| g.name == name) && names.insert(name)
                })
            })
    });
    let anchor = full_anchor.or_else(|| {
        occurrences.iter().copied().find(|e| {
            let args = self_args(e, id, inputs, scope).unwrap();
            if args.len() != fd.params.len() || ident(&args[driver_index]).is_none() {
                return false;
            }
            let mut names = BTreeSet::new();
            args.iter().all(|arg| match ident(arg) {
                Some(name) => law.givens.iter().any(|g| g.name == name) && names.insert(name),
                None => fixed(arg),
            })
        })
    })?;
    let anchor_args = self_args(anchor, id, inputs, scope)?;
    let driver = ident(&anchor_args[driver_index])?.to_string();
    let SourceBranch {
        guard,
        body: branch,
        list_bindings: pattern,
    } = recursive_branch(fd, param, measure, id, inputs, scope)?;
    if crate::codegen::expr_walk::any(branch, &mut |e| matches!(e.node, Expr::Match { .. })) {
        return None;
    }
    let mut recursive_calls = Vec::new();
    crate::codegen::expr_walk::walk(branch, &mut |e| {
        if let Some(args) = self_args(e, id, inputs, scope) {
            recursive_calls.push(args);
        }
    });
    if recursive_calls.len() != 1 {
        return None;
    }
    let recursive_args = recursive_calls[0];
    if recursive_args.len() != fd.params.len() {
        return None;
    }
    // A plain structural homomorphism already has its own decomposition
    // strategy. This plan is needed when a list fold changes another input;
    // selecting an arbitrary occurrence in e.g. f(x ++ y) = f(y) ++ f(x)
    // would choose an unrelated driver and discard useful algebraic support.
    if matches!(measure, LawInductionMeasure::SequenceLength)
        && fd
            .params
            .iter()
            .zip(recursive_args)
            .enumerate()
            .all(|(index, ((name, _), arg))| index == driver_index || ident(arg) == Some(name))
    {
        return None;
    }
    let mut occupied: BTreeSet<String> = law
        .givens
        .iter()
        .map(|g| g.name.clone())
        .chain(fd.params.iter().map(|(n, _)| n.clone()))
        .chain(
            inputs
                .pure_fns_in_scope(scope)
                .iter()
                .map(|f| f.name.clone()),
        )
        .collect();
    for expr in [&law.lhs, &law.rhs, branch] {
        crate::codegen::expr_walk::walk(expr, &mut |e| {
            if let Some(name) = ident(e) {
                occupied.insert(name.to_string());
            }
        });
    }
    let mut fresh = || {
        for index in 0.. {
            let name = format!("averInductionPart{index}");
            if occupied.insert(name.clone()) {
                return name;
            }
        }
        unreachable!()
    };
    let mut calls = Vec::new();
    for occurrence in occurrences {
        let args = self_args(occurrence, id, inputs, scope)?;
        if args.len() != fd.params.len() || ident(&args[driver_index]) != Some(driver.as_str()) {
            continue;
        }
        let mut bindings: BTreeMap<_, _> = fd
            .params
            .iter()
            .zip(args)
            .map(|((name, _), arg)| (name.clone(), arg.clone()))
            .collect();
        let guard = substitute(
            &guard,
            &fd.params
                .iter()
                .zip(args)
                .map(|((n, _), a)| (n.clone(), a.clone()))
                .collect(),
        )?;
        let list_case = pattern.as_ref().map(|(head, tail)| {
            let head_name = (head != "_").then(&mut fresh);
            let tail_name = (tail != "_").then(&mut fresh);
            // Pattern binders shadow function formals. Fresh IR projections
            // are substituted simultaneously, so law names cannot be captured.
            if let Some(name) = &head_name {
                bindings.insert(head.clone(), var(name));
            }
            if let Some(name) = &tail_name {
                bindings.insert(tail.clone(), var(name));
            }
            LawInductionListCase {
                list: inputs.resolve_expr(&args[driver_index], scope),
                head: head_name,
                tail: tail_name,
            }
        });
        let mut given_args: BTreeMap<_, _> = law
            .givens
            .iter()
            .map(|g| (g.name.clone(), var(&g.name)))
            .collect();
        for (anchor_arg, recursive_arg) in anchor_args.iter().zip(recursive_args) {
            if let Some(name) = ident(anchor_arg) {
                given_args.insert(name.to_string(), substitute(recursive_arg, &bindings)?);
            }
        }
        // A conditional theorem supplies an IH only where its recursive
        // premise holds. Other branches remain independent proof obligations.
        let premise = match &law.when {
            Some(premise) => Some(inputs.resolve_expr(&substitute(premise, &given_args)?, scope)),
            None => None,
        };
        calls.push(LawInductionCall {
            guard: inputs.resolve_expr(&guard, scope),
            premise,
            list_case,
            arguments: law
                .givens
                .iter()
                .map(|g| inputs.resolve_expr(&given_args[&g.name], scope))
                .collect(),
        });
    }
    (!calls.is_empty()).then(|| LawInduction {
        driver,
        measure,
        calls,
        source_call: inputs.resolve_expr(anchor, scope),
    })
}

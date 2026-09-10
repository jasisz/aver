//! Small literal countdowns suggest bounded unfolding of an obligation's cone.
//! This is a search hint, not a proof of a runtime bound. In particular, no
//! `given` samples or `when` bounds participate in choosing the budget.

use std::collections::{BTreeMap, BTreeSet};

use crate::ast::{Expr, FnDef, Literal, Spanned, Stmt, Type, VerifyLaw};
use crate::ir::FnId;
use crate::ir::hir::{ResolvedCallee, ResolvedExpr};
use crate::ir::proof_ir::{ProofIR, RecursionContract};

use crate::codegen::proof_lower::ProofLowerInputs;

use super::UnfoldingHint;

fn definition<'a>(inputs: &ProofLowerInputs<'a>, id: FnId) -> Option<&'a FnDef> {
    let key = &inputs.symbol_table.fn_entry(id).key;
    inputs
        .pure_fns_in_scope(key.scope_str())
        .into_iter()
        .find(|f| f.name == key.name)
}

fn literal(e: &Spanned<Expr>, bindings: &BTreeMap<String, Option<i64>>) -> Option<i64> {
    match &e.node {
        Expr::Literal(Literal::Int(n)) => Some(*n),
        Expr::Ident(name) | Expr::Resolved { name, .. } => bindings.get(name).copied().flatten(),
        _ => None,
    }
}

/// Follow only direct forwarding wrappers. Branches, local bindings and
/// callbacks need richer substitution; they keep the existing search strategy.
fn countdown(
    e: &Spanned<Expr>,
    bindings: &BTreeMap<String, Option<i64>>,
    scope: Option<&str>,
    inputs: &ProofLowerInputs,
    ir: &ProofIR,
    remaining: u32,
) -> Option<u32> {
    if remaining == 0 {
        return None;
    }
    let Expr::FnCall(callee, args) = &e.node else {
        return None;
    };
    let name = crate::checker::expr_to_str(callee);
    if bindings.contains_key(&name) {
        return None;
    }
    let id = inputs.symbol_table.resolve_fn_id_in(&name, scope)?;
    let fd = definition(inputs, id)?;
    if fd.params.len() != args.len() {
        return None;
    }
    let actuals: BTreeMap<_, _> = fd
        .params
        .iter()
        .zip(args)
        .map(|(p, a)| (p.0.clone(), literal(a, bindings)))
        .collect();
    if let Some(RecursionContract::WellFoundedToNat {
        param,
        floor_div: None,
    }) = ir.fn_contracts.get(&id).and_then(|c| c.recursion.as_ref())
    {
        let n = actuals.get(param).copied().flatten()?;
        return (0..=16).contains(&n).then_some(n as u32);
    }
    if inputs.recursive_fns.contains(&id) {
        return None;
    }
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return None;
    };
    countdown(
        body,
        &actuals,
        inputs.symbol_table.fn_entry(id).key.scope_str(),
        inputs,
        ir,
        remaining - 1,
    )
}

/// Until named-type substitution is represented here, only closed structural
/// types can name a helper instance without carrying an owning module scope.
fn closed_type(ty: &Type) -> bool {
    match ty {
        Type::Int | Type::Bool | Type::Str => true,
        Type::List(t) => closed_type(t),
        Type::Tuple(ts) => ts.iter().all(closed_type),
        _ => false,
    }
}

pub(super) fn plan(
    law: &VerifyLaw,
    expressions: &[&Spanned<Expr>],
    inputs: &ProofLowerInputs,
    ir: &ProofIR,
    scope: Option<&str>,
) -> Option<UnfoldingHint> {
    // A fixed countdown does not bound an independently quantified sequence.
    // Keep citation/induction search for those obligations: deeper unfolding
    // alone can lose proofs about an arbitrary suffix or accumulator.
    if !law.givens.iter().all(|g| {
        matches!(
            crate::types::parse_type_str(&g.type_name),
            Type::Int | Type::Bool
        )
    }) {
        return None;
    }
    let mut width = None;
    for e in expressions {
        crate::codegen::expr_walk::walk(e, &mut |node| {
            if let Some(n) = countdown(node, &BTreeMap::new(), scope, inputs, ir, 8) {
                width = Some(width.unwrap_or(0).max(n));
            }
        });
    }
    let depth = width? + 4;
    let mut pending = Vec::new();
    let mut seen = BTreeSet::new();
    let mut functions = Vec::new();
    let mut reverse_elements = Vec::new();
    let mut supported = true;
    let mut collect = |e: &Spanned<Expr>,
                       owner: Option<&str>,
                       parameters: &[(String, String)],
                       pending: &mut Vec<FnId>| {
        crate::codegen::expr_walk::walk(
            e,
            &mut |node| match inputs.resolve_expr(node, owner).node {
                ResolvedExpr::Call(ResolvedCallee::Fn(id), _)
                | ResolvedExpr::TailCall { target: id, .. } => pending.push(id),
                ResolvedExpr::Call(ResolvedCallee::Builtin(name), args)
                    if name == "List.reverse" =>
                {
                    // Imported source views may lack expression stamps. A
                    // directly named parameter still has its checked signature.
                    let parameter_type = args.first().and_then(|arg| match &arg.node {
                        ResolvedExpr::Ident(name) | ResolvedExpr::Resolved { name, .. } => {
                            parameters
                                .iter()
                                .find(|(p, _)| p == name)
                                .map(|(_, ty)| crate::types::parse_type_str(ty))
                        }
                        _ => None,
                    });
                    if let Some(Type::List(element)) = node.ty().or(parameter_type.as_ref()) {
                        if closed_type(element) {
                            if !reverse_elements.contains(element.as_ref()) {
                                reverse_elements.push((**element).clone());
                            }
                        } else {
                            supported = false;
                        }
                    } else {
                        supported = false;
                    }
                }
                _ => {}
            },
        );
    };
    for e in expressions {
        collect(e, scope, &[], &mut pending);
    }
    while let Some(id) = pending.pop() {
        if !seen.insert(id) {
            continue;
        }
        if seen.len() > 128 {
            return None;
        }
        let fd = definition(inputs, id)?;
        if inputs.recursive_fns.contains(&id) {
            functions.push(id);
        }
        let owner = inputs.symbol_table.fn_entry(id).key.scope_str();
        for stmt in fd.body.stmts() {
            let e = match stmt {
                Stmt::Expr(e) | Stmt::Binding(_, _, e) => e,
            };
            collect(e, owner, &fd.params, &mut pending);
        }
    }
    functions.sort();
    supported.then_some(UnfoldingHint {
        depth,
        functions,
        reverse_elements,
    })
}

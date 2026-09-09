//! Canonical source dependency walks shared by the proof backends. These
//! collect equations for search; neither walk proves a claim or bounds inputs.

use std::collections::HashSet;

use crate::ast::{Expr, Spanned, Stmt, VerifyLaw};
use crate::ir::FnId;
use crate::ir::hir::{ResolvedCallee, ResolvedExpr};

use super::ProofLowerInputs;

fn visit_fn(
    id: FnId,
    inputs: &ProofLowerInputs,
    seen: &mut HashSet<FnId>,
    out: &mut Vec<FnId>,
    builtins: &mut Vec<String>,
    calls_static: &mut bool,
) {
    if !seen.insert(id) {
        return;
    }
    let key = &inputs.symbol_table.fn_entry(id).key;
    if let Some(fd) = inputs
        .pure_fns_in_scope(key.scope_str())
        .into_iter()
        .find(|fd| fd.name == key.name)
    {
        out.push(id);
        for stmt in fd.body.stmts() {
            let (Stmt::Expr(body) | Stmt::Binding(_, _, body)) = stmt;
            visit(
                body,
                key.scope_str(),
                inputs,
                seen,
                out,
                builtins,
                calls_static,
            );
        }
    } else {
        *calls_static = false;
    }
}

fn visit(
    expr: &Spanned<Expr>,
    scope: Option<&str>,
    inputs: &ProofLowerInputs,
    seen: &mut HashSet<FnId>,
    out: &mut Vec<FnId>,
    builtins: &mut Vec<String>,
    calls_static: &mut bool,
) {
    match inputs.resolve_expr(expr, scope).node {
        ResolvedExpr::Call(ResolvedCallee::Fn(id), _)
        | ResolvedExpr::TailCall { target: id, .. } => {
            visit_fn(id, inputs, seen, out, builtins, calls_static)
        }
        ResolvedExpr::Call(ResolvedCallee::Builtin(name), _) => {
            if !builtins.contains(&name) {
                builtins.push(name);
            }
        }
        ResolvedExpr::Call(
            ResolvedCallee::LocalSlot { .. } | ResolvedCallee::Unresolved { .. },
            _,
        ) => *calls_static = false,
        _ => {}
    }
    crate::codegen::expr_walk::for_each_child(expr, &mut |child| {
        visit(child, scope, inputs, seen, out, builtins, calls_static)
    });
}

pub(super) fn collect(
    law: &VerifyLaw,
    inputs: &ProofLowerInputs,
    scope: Option<&str>,
) -> Vec<FnId> {
    let mut out = Vec::new();
    let mut seen = HashSet::new();
    for expr in law
        .because
        .iter()
        .chain([&law.lhs, &law.rhs])
        .chain(law.when.iter())
    {
        visit(
            expr,
            scope,
            inputs,
            &mut seen,
            &mut out,
            &mut Vec::new(),
            &mut true,
        );
    }
    out
}

/// A law can belong to a nonrecursive constructor while also mentioning a
/// recursive observer. Keep the constructor's own dependencies separate.
pub(super) fn target(id: FnId, inputs: &ProofLowerInputs) -> (Vec<FnId>, Vec<String>, bool) {
    let mut out = Vec::new();
    let mut builtins = Vec::new();
    let mut calls_static = true;
    visit_fn(
        id,
        inputs,
        &mut HashSet::new(),
        &mut out,
        &mut builtins,
        &mut calls_static,
    );
    (out, builtins, calls_static)
}

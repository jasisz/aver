//! One canonical source dependency walk for both proof backends. This collects
//! available equations; it neither proves a claim nor bounds its input domain.

use std::collections::HashSet;

use crate::ast::{Expr, Spanned, Stmt, VerifyLaw};
use crate::ir::FnId;
use crate::ir::hir::{ResolvedCallee, ResolvedExpr};

use super::ProofLowerInputs;

pub(super) fn collect(
    law: &VerifyLaw,
    inputs: &ProofLowerInputs,
    scope: Option<&str>,
) -> Vec<FnId> {
    fn visit(
        expr: &Spanned<Expr>,
        scope: Option<&str>,
        inputs: &ProofLowerInputs,
        seen: &mut HashSet<FnId>,
        out: &mut Vec<FnId>,
    ) {
        let id = match inputs.resolve_expr(expr, scope).node {
            ResolvedExpr::Call(ResolvedCallee::Fn(id), _)
            | ResolvedExpr::TailCall { target: id, .. } => Some(id),
            _ => None,
        };
        if let Some(id) = id
            && seen.insert(id)
        {
            let key = &inputs.symbol_table.fn_entry(id).key;
            if let Some(fd) = inputs
                .pure_fns_in_scope(key.scope_str())
                .into_iter()
                .find(|fd| fd.name == key.name)
            {
                out.push(id);
                for stmt in fd.body.stmts() {
                    let (Stmt::Expr(body) | Stmt::Binding(_, _, body)) = stmt;
                    visit(body, key.scope_str(), inputs, seen, out);
                }
            }
        }
        crate::codegen::expr_walk::for_each_child(expr, &mut |child| {
            visit(child, scope, inputs, seen, out)
        });
    }
    let mut out = Vec::new();
    let mut seen = HashSet::new();
    for expr in law
        .because
        .iter()
        .chain([&law.lhs, &law.rhs])
        .chain(law.when.iter())
    {
        visit(expr, scope, inputs, &mut seen, &mut out);
    }
    out
}

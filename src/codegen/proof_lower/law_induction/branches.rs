//! Enumerate actual recursive calls under their explicit Boolean path guards.
//! Binder-bearing nested matches remain outside this planner's scope contract.

use super::{ProofLowerInputs, call, self_args};
use crate::ast::{Expr, Literal, Pattern, Spanned};

pub(super) struct Site<'a> {
    pub arguments: &'a [Spanned<Expr>],
    pub guard: Option<Spanned<Expr>>,
}

pub(super) fn recursive_sites<'a>(
    body: &'a Spanned<Expr>,
    id: crate::ir::FnId,
    inputs: &ProofLowerInputs,
    scope: Option<&str>,
) -> Option<Vec<Site<'a>>> {
    fn visit<'a>(
        body: &'a Spanned<Expr>,
        guard: Option<Spanned<Expr>>,
        id: crate::ir::FnId,
        inputs: &ProofLowerInputs,
        scope: Option<&str>,
        out: &mut Vec<Site<'a>>,
    ) -> Option<()> {
        // A base branch needs no recursive instance. Its internal bindings are
        // irrelevant to this plan and its result is still checked by the backend.
        if !crate::codegen::expr_walk::any(body, &mut |e| self_args(e, id, inputs, scope).is_some())
        {
            return Some(());
        }
        if let Expr::Match { subject, arms } = &body.node {
            if arms.len() != 2
                || ![true, false].iter().all(|v| {
                    arms.iter().any(
                        |arm| matches!(arm.pattern, Pattern::Literal(Literal::Bool(b)) if b == *v),
                    )
                })
                || crate::codegen::expr_walk::any(subject, &mut |e| {
                    self_args(e, id, inputs, scope).is_some()
                })
            {
                return None;
            }
            for arm in arms {
                let condition = if matches!(arm.pattern, Pattern::Literal(Literal::Bool(true))) {
                    (**subject).clone()
                } else {
                    call("Bool.not", vec![(**subject).clone()])
                };
                let path = Some(match &guard {
                    Some(previous) => call("Bool.and", vec![previous.clone(), condition]),
                    None => condition,
                });
                visit(&arm.body, path, id, inputs, scope, out)?;
            }
        } else {
            if crate::codegen::expr_walk::any(body, &mut |e| matches!(e.node, Expr::Match { .. })) {
                return None;
            }
            crate::codegen::expr_walk::walk(body, &mut |e| {
                if let Some(arguments) = self_args(e, id, inputs, scope) {
                    out.push(Site {
                        arguments,
                        guard: guard.clone(),
                    });
                }
            });
        }
        Some(())
    }
    let mut sites = Vec::new();
    visit(body, None, id, inputs, scope, &mut sites)?;
    (!sites.is_empty()).then_some(sites)
}

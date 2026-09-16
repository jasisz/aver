//! Compare two computations by exposing one checked recursive equation at a
//! time on the right. Functional induction on a left-hand list recursion
//! supplies the exact recursive calls, including changing accumulator values.
//! This is a proof search candidate, never a recognition rule granting credit.
use super::induction::{self, Definitions};
use crate::ast::{BinOp, Expr, FnBody, Spanned, Stmt, VerifyLaw};
use crate::codegen::CodegenContext;
use crate::codegen::lean::expr::{emit_expr, resolve_rewrite_output};

pub(super) fn candidate(
    law: &VerifyLaw,
    ctx: &CodegenContext,
    definitions: &Definitions,
    fact_count: usize,
) -> Option<String> {
    // An incidental list helper in a scalar/record operation (e.g. a hash
    // encoder) is not the law's induction input. Keep its established solver.
    if definitions.list_steps.is_empty()
        || !law
            .givens
            .iter()
            .any(|given| given.type_name.starts_with("List<"))
    {
        return None;
    }
    let (left, right) = match &law.lhs.node {
        Expr::BinOp(BinOp::Eq, left, right) => (left.as_ref(), right.as_ref()),
        _ => (&law.lhs, &law.rhs),
    };
    if !matches!(left.node, Expr::FnCall(_, _)) || !matches!(right.node, Expr::FnCall(_, _)) {
        return None;
    }
    let scope = ctx.active_module_scope();
    let mut call = left.clone();
    let mut visited = std::collections::HashSet::new();
    let mut induction_call = None;
    let mut wrappers = Vec::new();
    while let Some(fd) = induction::callee(&call, ctx, scope.as_deref()) {
        if !visited.insert(fd.name.clone()) || !fd.effects.is_empty() {
            break;
        }
        if let Some(measure) = induction::list_measure(fd, ctx) {
            let Expr::FnCall(_, args) = &call.node else {
                return None;
            };
            let index = fd.params.iter().position(|(name, _)| name == measure)?;
            // A visible cons/drop expression is an equation-rewrite problem;
            // induction would hide the ordinary one-step solver for that law.
            if !matches!(
                args.get(index)?.node,
                Expr::Ident(_) | Expr::Resolved { .. }
            ) {
                return None;
            }
            induction_call = Some(emit_expr(&resolve_rewrite_output(&call, ctx, None), ctx));
            break;
        }
        // A composition can wrap a checked recursive result (for example a
        // helper trace passed to a continuation). Induct on that call before
        // splitting its result projections; the kernel supplies the same IH.
        if let Expr::FnCall(_, args) = &call.node {
            induction_call = args.iter().find_map(|arg| {
                let nested = induction::callee(arg, ctx, scope.as_deref())?;
                let measure = induction::list_measure(nested, ctx)?;
                let Expr::FnCall(_, values) = &arg.node else {
                    return None;
                };
                let index = nested.params.iter().position(|(name, _)| name == measure)?;
                if !matches!(
                    values.get(index)?.node,
                    Expr::Ident(_) | Expr::Resolved { .. }
                ) {
                    return None;
                }
                Some(emit_expr(&resolve_rewrite_output(arg, ctx, None), ctx))
            });
            if induction_call.is_some() {
                break;
            }
        }
        // Peel only a direct wrapper. Source bindings, conditions and recursive
        // bodies are not substituted by a compiler-side proof heuristic.
        let body: &Spanned<Expr> = match fd.body.as_ref() {
            FnBody::Block(stmts) => match stmts.as_slice() {
                [Stmt::Expr(expr)] => expr,
                _ => break,
            },
        };
        if !matches!(body.node, Expr::FnCall(_, _)) {
            break;
        }
        let Expr::FnCall(_, args) = &call.node else {
            break;
        };
        if fd.params.len() != args.len() {
            break;
        }
        let bindings = fd
            .params
            .iter()
            .zip(args)
            .map(|((name, _), arg)| (name.as_str(), arg))
            .collect();
        wrappers.push(induction::lean_name(fd, ctx));
        call = super::super::shared::substitute_expr(body, &bindings);
    }
    let start = match induction_call {
        Some(call) => format!("fun_induction {call}; "),
        None => String::new(),
    };
    let simp = [definitions.simp.clone(), definitions.list_maps.clone()]
        .into_iter()
        .filter(|s| !s.is_empty())
        .chain((0..fact_count).map(|i| format!("-_fact{i}")))
        .collect::<Vec<_>>()
        .join(", ");
    let steps = definitions
        .list_steps
        .split(", ")
        .map(|name| format!(" | (conv => rhs; rw [{name}.eq_def])"))
        .collect::<String>();
    if let Some(fd) = induction::callee(right, ctx, scope.as_deref())
        && induction::list_measure(fd, ctx).is_none()
    {
        wrappers.push(induction::lean_name(fd, ctx));
    }
    let heads = wrappers
        .into_iter()
        .chain([definitions.heads.clone()])
        .filter(|s| !s.is_empty())
        .collect::<Vec<_>>()
        .join(", ");
    let heads = if heads.is_empty() {
        String::new()
    } else {
        format!(", {heads}")
    };
    let solve = format!(
        "simp only [beq_iff_eq{heads}]; {start}all_goals (repeat' first | assumption | rfl | (simp_all [{simp}]) | split{steps} | (solve | grind)); done"
    );
    // Finite helper chains often return records containing a mapped remainder.
    // Split only a constructor prefix before substitution duplicates those
    // projections. The tail remains universally quantified, not enumerated.
    let cases = if start.is_empty() && !definitions.list_maps.is_empty() {
        law.givens.iter().find(|g| g.type_name.starts_with("List<")).map(|g| {
            let input = crate::codegen::lean::expr::aver_name_to_lean(&g.name);
            format!(" | (have __aver_sub_cursor (a b : Int) : a + b - a = b := (by omega); cases {input}; all_goals (try (rename_i __aver_head __aver_tail; cases __aver_head <;> cases __aver_tail)); all_goals (try (rename_i __aver_head __aver_tail; cases __aver_head)); all_goals (simp_all [beq_iff_eq, __aver_sub_cursor, {simp}, {}]); all_goals (try grind); done)", definitions.list_steps)
        }).unwrap_or_default()
    } else {
        String::new()
    };
    let roots = [left, right]
        .iter()
        .filter_map(|call| induction::callee(call, ctx, scope.as_deref()))
        .map(|fd| induction::lean_name(fd, ctx))
        .collect::<Vec<_>>()
        .join(", ");
    let compose = if fact_count > 0 {
        format!(" | (simp only [beq_iff_eq] at *; simp only [{roots}]; with_reducible grind only)")
    } else {
        String::new()
    };
    Some(format!("(first{compose}{cases} | ({solve}))"))
}

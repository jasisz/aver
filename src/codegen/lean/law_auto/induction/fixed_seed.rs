//! Compare a structural accumulator worker with a wrapper of another worker.
//! Unfold the wrapper before functional induction so the shared initial seed
//! appears on both sides and the recursive IH follows each updated accumulator.

use crate::ast::{Expr, Literal, Spanned, Stmt, VerifyLaw};
use crate::codegen::CodegenContext;
use crate::codegen::recursion::detect::{
    param_threaded_in_recursion, single_list_structural_param_index,
};

fn call(expr: &Spanned<Expr>) -> Option<(String, &[Spanned<Expr>])> {
    let Expr::FnCall(callee, args) = &expr.node else {
        return None;
    };
    Some((super::super::shared::expr_dotted_name(callee)?, args))
}

fn ident(expr: &Spanned<Expr>) -> Option<&str> {
    match &expr.node {
        Expr::Ident(name) | Expr::Resolved { name, .. } => Some(name),
        _ => None,
    }
}

fn accumulator_worker(fd: &crate::ast::FnDef) -> bool {
    fd.effects.is_empty()
        && fd.params.len() == 2
        && fd.params[1].1 == "Int"
        && fd.return_type == "Int"
        && single_list_structural_param_index(fd) == Some(0)
        && param_threaded_in_recursion(fd, 1)
}

pub(super) fn rung(
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
    simp_defs: &str,
) -> Option<Vec<String>> {
    let scope = ctx.active_module_scope();
    let try_side = |left: &Spanned<Expr>, right: &Spanned<Expr>| -> Option<Vec<String>> {
        let (worker_name, args) = call(left)?;
        let [list, seed] = args else { return None };
        if !matches!(seed.node, Expr::Literal(Literal::Int(_))) {
            return None;
        }
        let given = ident(list)?;
        let given_index = law.givens.iter().position(|g| g.name == given)?;
        let worker = ctx.fn_def_by_callee(&worker_name, scope.as_deref())?;
        if !accumulator_worker(worker) {
            return None;
        }

        let (wrapper_name, wrapper_args) = call(right)?;
        let [wrapper_list] = wrapper_args else {
            return None;
        };
        if ident(wrapper_list)? != given {
            return None;
        }
        let wrapper = ctx.fn_def_by_callee(&wrapper_name, scope.as_deref())?;
        if !wrapper.effects.is_empty() || wrapper.params.len() != 1 {
            return None;
        }
        let [Stmt::Expr(body)] = wrapper.body.stmts() else {
            return None;
        };
        let (other_name, other_args) = call(body)?;
        let [other_list, other_seed] = other_args else {
            return None;
        };
        if ident(other_list)? != wrapper.params[0].0 || other_seed.node != seed.node {
            return None;
        }
        // Private helper names resolve in the wrapper's owning module.
        let wrapper_id = crate::codegen::common::fn_id_for_decl(ctx, wrapper)?;
        let wrapper_scope = ctx.symbol_table.fn_entry(wrapper_id).key.scope_str();
        let other = ctx.fn_def_by_callee(&other_name, wrapper_scope)?;
        if !accumulator_worker(other) || worker.params[0].1 != other.params[0].1 {
            return None;
        }

        let worker_lean = super::super::shared::simp_def_name(ctx, &worker_name);
        let wrapper_lean = super::super::shared::simp_def_name(ctx, &wrapper_name);
        let driver = intro_names.get(given_index)?;
        let seed_lean = crate::codegen::lean::expr::emit_int_anchored(
            &ctx.resolve_expr(seed, scope.as_deref()),
            ctx,
        );
        Some(vec![format!(
            "  | (simp only [{wrapper_lean}]; fun_induction {worker_lean} {driver} {seed_lean} <;> (simp_all [{simp_defs}]; done))"
        )])
    };
    try_side(&law.lhs, &law.rhs).or_else(|| try_side(&law.rhs, &law.lhs))
}

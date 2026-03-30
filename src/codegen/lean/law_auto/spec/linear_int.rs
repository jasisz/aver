use std::collections::{HashMap, HashSet};

use crate::ast::{BinOp, Expr, Spanned, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;
use crate::verify_law::canonical_spec_ref;

use super::super::super::expr::emit_expr;
use super::super::shared::{body_terminal_expr, callee_matches_name, find_fn_def, substitute_expr};
use super::super::{AutoProof, intro_then};

fn is_linear_int_expr(expr: &Spanned<Expr>, allowed_idents: &HashSet<&str>) -> bool {
    match &expr.node {
        Expr::Literal(crate::ast::Literal::Int(_)) => true,
        Expr::Ident(name) => allowed_idents.contains(name.as_str()),
        Expr::BinOp(BinOp::Add | BinOp::Sub, left, right) => {
            is_linear_int_expr(left, allowed_idents) && is_linear_int_expr(right, allowed_idents)
        }
        _ => false,
    }
}

pub(super) fn emit_linear_int_omega_spec_equivalence_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
) -> Option<AutoProof> {
    if law.givens.is_empty() || !law.givens.iter().all(|given| given.type_name == "Int") {
        return None;
    }

    let spec_ref = canonical_spec_ref(&vb.fn_name, law, &ctx.fn_sigs)?;
    let impl_fd = find_fn_def(ctx, &vb.fn_name)?;
    let spec_fd = find_fn_def(ctx, &spec_ref.spec_fn_name)?;
    if impl_fd.return_type != "Int" || spec_fd.return_type != "Int" {
        return None;
    }

    let impl_body = body_terminal_expr(impl_fd.body.as_ref())?;
    let spec_body = body_terminal_expr(spec_fd.body.as_ref())?;
    let allowed_idents: HashSet<&str> =
        law.givens.iter().map(|given| given.name.as_str()).collect();

    let try_side = |impl_side: &Spanned<Expr>, spec_side: &Spanned<Expr>| -> Option<AutoProof> {
        let Expr::FnCall(impl_callee, impl_args) = &impl_side.node else {
            return None;
        };
        let Expr::FnCall(spec_callee, spec_args) = &spec_side.node else {
            return None;
        };
        if !callee_matches_name(impl_callee, &vb.fn_name)
            || !callee_matches_name(spec_callee, &spec_ref.spec_fn_name)
            || impl_args != spec_args
            || impl_args.len() != impl_fd.params.len()
            || spec_args.len() != spec_fd.params.len()
        {
            return None;
        }

        let impl_bindings: HashMap<&str, &Spanned<Expr>> = impl_fd
            .params
            .iter()
            .zip(impl_args.iter())
            .map(|((name, _), arg)| (name.as_str(), arg))
            .collect();
        let spec_bindings: HashMap<&str, &Spanned<Expr>> = spec_fd
            .params
            .iter()
            .zip(spec_args.iter())
            .map(|((name, _), arg)| (name.as_str(), arg))
            .collect();

        let unfolded_impl = substitute_expr(impl_body, &impl_bindings);
        let unfolded_spec = substitute_expr(spec_body, &spec_bindings);
        if !is_linear_int_expr(&unfolded_impl, &allowed_idents)
            || !is_linear_int_expr(&unfolded_spec, &allowed_idents)
        {
            return None;
        }

        Some(AutoProof {
            support_lines: Vec::new(),
            proof_lines: intro_then(
                intro_names,
                vec![
                    format!(
                        "change {} = {}",
                        emit_expr(&unfolded_impl, ctx),
                        emit_expr(&unfolded_spec, ctx)
                    ),
                    "omega".to_string(),
                ],
            ),
            replaces_theorem: false,
        })
    };

    try_side(&law.lhs, &law.rhs).or_else(|| try_side(&law.rhs, &law.lhs))
}

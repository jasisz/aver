use std::collections::{HashMap, HashSet};

use crate::ast::{BinOp, Expr, MatchArm, StrPart, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;
use crate::verify_law::canonical_spec_ref;

use super::super::super::expr::emit_expr;
use super::super::shared::{body_terminal_expr, callee_matches_name, find_fn_def};
use super::super::{AutoProof, intro_then};

fn substitute_expr(expr: &Expr, bindings: &HashMap<&str, &Expr>) -> Expr {
    match expr {
        Expr::Literal(lit) => Expr::Literal(lit.clone()),
        Expr::Ident(name) => bindings
            .get(name.as_str())
            .map_or_else(|| Expr::Ident(name.clone()), |bound| (*bound).clone()),
        Expr::Attr(base, field) => {
            Expr::Attr(Box::new(substitute_expr(base, bindings)), field.clone())
        }
        Expr::FnCall(callee, args) => Expr::FnCall(
            Box::new(substitute_expr(callee, bindings)),
            args.iter()
                .map(|arg| substitute_expr(arg, bindings))
                .collect(),
        ),
        Expr::BinOp(op, left, right) => Expr::BinOp(
            op.clone(),
            Box::new(substitute_expr(left, bindings)),
            Box::new(substitute_expr(right, bindings)),
        ),
        Expr::Match {
            subject,
            arms,
            line,
        } => Expr::Match {
            subject: Box::new(substitute_expr(subject, bindings)),
            arms: arms
                .iter()
                .map(|arm| MatchArm {
                    pattern: arm.pattern.clone(),
                    body: Box::new(substitute_expr(&arm.body, bindings)),
                })
                .collect(),
            line: *line,
        },
        Expr::Constructor(name, inner) => Expr::Constructor(
            name.clone(),
            inner
                .as_ref()
                .map(|expr| Box::new(substitute_expr(expr, bindings))),
        ),
        Expr::ErrorProp(inner) => Expr::ErrorProp(Box::new(substitute_expr(inner, bindings))),
        Expr::InterpolatedStr(parts) => Expr::InterpolatedStr(
            parts
                .iter()
                .map(|part| match part {
                    StrPart::Literal(s) => StrPart::Literal(s.clone()),
                    StrPart::Parsed(expr) => {
                        StrPart::Parsed(Box::new(substitute_expr(expr, bindings)))
                    }
                })
                .collect(),
        ),
        Expr::List(items) => Expr::List(
            items
                .iter()
                .map(|item| substitute_expr(item, bindings))
                .collect(),
        ),
        Expr::Tuple(items) => Expr::Tuple(
            items
                .iter()
                .map(|item| substitute_expr(item, bindings))
                .collect(),
        ),
        Expr::MapLiteral(entries) => Expr::MapLiteral(
            entries
                .iter()
                .map(|(key, value)| {
                    (
                        substitute_expr(key, bindings),
                        substitute_expr(value, bindings),
                    )
                })
                .collect(),
        ),
        Expr::RecordCreate { type_name, fields } => Expr::RecordCreate {
            type_name: type_name.clone(),
            fields: fields
                .iter()
                .map(|(name, value)| (name.clone(), substitute_expr(value, bindings)))
                .collect(),
        },
        Expr::RecordUpdate {
            type_name,
            base,
            updates,
        } => Expr::RecordUpdate {
            type_name: type_name.clone(),
            base: Box::new(substitute_expr(base, bindings)),
            updates: updates
                .iter()
                .map(|(name, value)| (name.clone(), substitute_expr(value, bindings)))
                .collect(),
        },
        Expr::TailCall(call) => Expr::TailCall(Box::new((
            call.0.clone(),
            call.1
                .iter()
                .map(|arg| substitute_expr(arg, bindings))
                .collect(),
        ))),
        Expr::Resolved(slot) => Expr::Resolved(*slot),
    }
}

fn is_linear_int_expr(expr: &Expr, allowed_idents: &HashSet<&str>) -> bool {
    match expr {
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

    let try_side = |impl_side: &Expr, spec_side: &Expr| -> Option<AutoProof> {
        let Expr::FnCall(impl_callee, impl_args) = impl_side else {
            return None;
        };
        let Expr::FnCall(spec_callee, spec_args) = spec_side else {
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

        let impl_bindings: HashMap<&str, &Expr> = impl_fd
            .params
            .iter()
            .zip(impl_args.iter())
            .map(|((name, _), arg)| (name.as_str(), arg))
            .collect();
        let spec_bindings: HashMap<&str, &Expr> = spec_fd
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

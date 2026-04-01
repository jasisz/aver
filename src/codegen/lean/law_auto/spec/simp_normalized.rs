use std::collections::HashMap;

use crate::ast::{BinOp, Expr, Literal, Spanned, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;
use crate::verify_law::canonical_spec_ref;

use super::super::shared::{
    body_terminal_expr, callee_matches_name, find_fn_def, law_simp_defs, substitute_expr,
};
use super::super::{AutoProof, intro_then};

fn int_lit(expr: &Expr) -> Option<i64> {
    match expr {
        Expr::Literal(Literal::Int(n)) => Some(*n),
        _ => None,
    }
}

fn simplify_identity_expr(expr: &Spanned<Expr>) -> Spanned<Expr> {
    let line = expr.line;
    let new_node = match &expr.node {
        Expr::BinOp(op, left, right) => {
            let left = simplify_identity_expr(left);
            let right = simplify_identity_expr(right);
            match op {
                BinOp::Add => {
                    if int_lit(&left.node) == Some(0) {
                        return right;
                    } else if int_lit(&right.node) == Some(0) {
                        return left;
                    } else {
                        Expr::BinOp(*op, Box::new(left), Box::new(right))
                    }
                }
                BinOp::Sub => {
                    if int_lit(&right.node) == Some(0) {
                        return left;
                    } else {
                        Expr::BinOp(*op, Box::new(left), Box::new(right))
                    }
                }
                BinOp::Mul => {
                    if int_lit(&left.node) == Some(0) || int_lit(&right.node) == Some(0) {
                        Expr::Literal(Literal::Int(0))
                    } else if int_lit(&left.node) == Some(1) {
                        return right;
                    } else if int_lit(&right.node) == Some(1) {
                        return left;
                    } else {
                        Expr::BinOp(*op, Box::new(left), Box::new(right))
                    }
                }
                _ => Expr::BinOp(*op, Box::new(left), Box::new(right)),
            }
        }
        Expr::Attr(base, field) => {
            Expr::Attr(Box::new(simplify_identity_expr(base)), field.clone())
        }
        Expr::FnCall(callee, args) => Expr::FnCall(
            Box::new(simplify_identity_expr(callee)),
            args.iter().map(simplify_identity_expr).collect(),
        ),
        Expr::Match { subject, arms } => Expr::Match {
            subject: Box::new(simplify_identity_expr(subject)),
            arms: arms
                .iter()
                .map(|arm| crate::ast::MatchArm {
                    pattern: arm.pattern.clone(),
                    body: Box::new(simplify_identity_expr(&arm.body)),
                })
                .collect(),
        },
        Expr::Constructor(name, inner) => Expr::Constructor(
            name.clone(),
            inner
                .as_ref()
                .map(|inner| Box::new(simplify_identity_expr(inner))),
        ),
        Expr::ErrorProp(inner) => Expr::ErrorProp(Box::new(simplify_identity_expr(inner))),
        Expr::InterpolatedStr(parts) => Expr::InterpolatedStr(
            parts
                .iter()
                .map(|part| match part {
                    crate::ast::StrPart::Literal(s) => crate::ast::StrPart::Literal(s.clone()),
                    crate::ast::StrPart::Parsed(inner) => {
                        crate::ast::StrPart::Parsed(Box::new(simplify_identity_expr(inner)))
                    }
                })
                .collect(),
        ),
        Expr::List(items) => Expr::List(items.iter().map(simplify_identity_expr).collect()),
        Expr::Tuple(items) => Expr::Tuple(items.iter().map(simplify_identity_expr).collect()),
        Expr::EffectTuple(items, flag) => {
            Expr::EffectTuple(items.iter().map(simplify_identity_expr).collect(), *flag)
        }
        Expr::MapLiteral(entries) => Expr::MapLiteral(
            entries
                .iter()
                .map(|(key, value)| (simplify_identity_expr(key), simplify_identity_expr(value)))
                .collect(),
        ),
        Expr::RecordCreate { type_name, fields } => Expr::RecordCreate {
            type_name: type_name.clone(),
            fields: fields
                .iter()
                .map(|(name, value)| (name.clone(), simplify_identity_expr(value)))
                .collect(),
        },
        Expr::RecordUpdate {
            type_name,
            base,
            updates,
        } => Expr::RecordUpdate {
            type_name: type_name.clone(),
            base: Box::new(simplify_identity_expr(base)),
            updates: updates
                .iter()
                .map(|(name, value)| (name.clone(), simplify_identity_expr(value)))
                .collect(),
        },
        Expr::TailCall(call) => Expr::TailCall(Box::new((
            call.0.clone(),
            call.1.iter().map(simplify_identity_expr).collect(),
        ))),
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved(_) => return expr.clone(),
    };
    Spanned::new(new_node, line)
}

pub(super) fn emit_simp_normalized_spec_equivalence_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
) -> Option<AutoProof> {
    let spec_ref = canonical_spec_ref(&vb.fn_name, law, &ctx.fn_sigs)?;
    let impl_fd = find_fn_def(ctx, &vb.fn_name)?;
    let spec_fd = find_fn_def(ctx, &spec_ref.spec_fn_name)?;
    let impl_body = body_terminal_expr(impl_fd.body.as_ref())?;
    let spec_body = body_terminal_expr(spec_fd.body.as_ref())?;

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
        if simplify_identity_expr(&unfolded_impl) != simplify_identity_expr(&unfolded_spec) {
            return None;
        }

        let simp_defs = law_simp_defs(ctx, vb, law).into_iter().collect::<Vec<_>>();
        Some(AutoProof {
            support_lines: Vec::new(),
            proof_lines: intro_then(
                intro_names,
                vec![format!("simp [{}]", simp_defs.join(", "))],
            ),
            replaces_theorem: false,
        })
    };

    try_side(&law.lhs, &law.rhs).or_else(|| try_side(&law.rhs, &law.lhs))
}

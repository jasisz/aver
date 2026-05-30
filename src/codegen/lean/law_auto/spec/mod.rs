mod linear_int;
mod linear_recurrence2;
mod result_pipeline_chain;
mod simp_normalized;
mod wrapper_over_recursion;

pub(super) use linear_recurrence2::emit_second_order_linear_recurrence_spec_equivalence_law;
pub(super) use result_pipeline_chain::emit_result_pipeline_chain_law;
pub(super) use wrapper_over_recursion::emit_wrapper_over_recursion_law;

use crate::ast::{Expr, Spanned, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;
use crate::verify_law::canonical_spec_ref;

use super::super::expr::emit_expr_legacy;
use super::shared::{body_terminal_expr, callee_matches_name, find_fn_def, law_simp_defs};
use super::{AutoProof, intro_then};

/// Oracle v1: auto-prove effectful laws of the shape
///
///   verify pickOne law consistent
///       given rnd: Random.int = [stubConst]
///       pickOne() => pickOneSpec(BranchPath.root(), rnd)
///
/// After the law-body rewrite both sides are direct calls with
/// identical arguments (`pickOne(root, rnd) == pickOneSpec(root, rnd)`);
/// unfolding both definitions gives the same `rnd(root, 0, …)` term
/// and `simpa [pickOne, pickOneSpec]` closes the goal.
fn emit_effectful_spec_equivalence_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
) -> Option<AutoProof> {
    let impl_fd = find_fn_def(ctx, &vb.fn_name)?;
    if impl_fd.effects.is_empty() {
        return None;
    }
    if !impl_fd
        .effects
        .iter()
        .all(|e| crate::types::checker::effect_classification::is_classified(&e.node))
    {
        return None;
    }

    // Try the full `impl(args) == spec(args)` shape first — both
    // sides fn calls, identical args, distinct fns. Closes via
    // `simp [impl, spec]` since both unfold to the same oracle call.
    if let Some((impl_name, _impl_args, spec_name)) = match_impl_spec_call_shape(vb, law) {
        let simp_defs = [
            aver_name_to_lean_ident(&impl_name),
            aver_name_to_lean_ident(&spec_name),
        ];
        return Some(AutoProof {
            support_lines: Vec::new(),
            proof_lines: intro_then(
                intro_names,
                // `simp` (no-a variant) avoids the `unnecessarySimpa`
                // linter warning when no goal is left to close after
                // unfolding — a bare def-equality match doesn't need
                // `simpa`'s cleanup step.
                vec![format!("simp [{}]", simp_defs.join(", "))],
            ),
            replaces_theorem: false,
        });
    }

    // Fallback: one side is the impl call, the other is any
    // expression — may inline the oracle directly
    // (`rnd root 0 1 6 + rnd root 1 1 6`) OR call pure helper fns
    // like `bothSpec args env`. `simp [impl]` alone unfolds impl but
    // leaves helper calls in the goal, and Lean can't close.
    // `law_simp_defs` already walks lhs + rhs transitively to gather
    // every pure user fn called — include them all so the whole RHS
    // reduces.
    let _impl_name = match_impl_call_one_side(vb, law)?;
    let simp_defs: Vec<String> = law_simp_defs(ctx, vb, law).into_iter().collect();
    Some(AutoProof {
        support_lines: Vec::new(),
        proof_lines: intro_then(
            intro_names,
            vec![format!("simp [{}]", simp_defs.join(", "))],
        ),
        replaces_theorem: false,
    })
}

/// Return the impl fn name if one side of the law is a direct fn
/// call to `vb.fn_name`. The other side can be any expression —
/// typically an inline spec using the oracle ident directly.
fn match_impl_call_one_side(vb: &VerifyBlock, law: &VerifyLaw) -> Option<String> {
    let side_calls_vb = |e: &Spanned<Expr>| -> bool {
        let Expr::FnCall(callee, _) = &e.node else {
            return false;
        };
        expr_fn_name(callee).as_deref() == Some(vb.fn_name.as_str())
    };
    if side_calls_vb(&law.lhs) || side_calls_vb(&law.rhs) {
        Some(vb.fn_name.clone())
    } else {
        None
    }
}

/// Return `(impl_fn_name, impl_args, spec_fn_name)` if the law body
/// matches the `impl(args…) == spec(args…)` shape with identical arg
/// lists and distinct fn names. Accepts either ordering (LHS/RHS).
fn match_impl_spec_call_shape(
    vb: &VerifyBlock,
    law: &VerifyLaw,
) -> Option<(String, Vec<Spanned<Expr>>, String)> {
    let try_side = |lhs: &Spanned<Expr>,
                    rhs: &Spanned<Expr>|
     -> Option<(String, Vec<Spanned<Expr>>, String)> {
        let Expr::FnCall(l_callee, l_args) = &lhs.node else {
            return None;
        };
        let Expr::FnCall(r_callee, r_args) = &rhs.node else {
            return None;
        };
        if l_args != r_args {
            return None;
        }
        let l_name = expr_fn_name(l_callee)?;
        let r_name = expr_fn_name(r_callee)?;
        if l_name == r_name {
            return None;
        }
        if l_name != vb.fn_name {
            return None;
        }
        Some((l_name, l_args.clone(), r_name))
    };
    try_side(&law.lhs, &law.rhs).or_else(|| try_side(&law.rhs, &law.lhs))
}

fn expr_fn_name(expr: &Spanned<Expr>) -> Option<String> {
    match &expr.node {
        Expr::Ident(name) | Expr::Resolved { name, .. } => Some(name.clone()),
        _ => None,
    }
}

fn aver_name_to_lean_ident(name: &str) -> String {
    super::super::expr::aver_name_to_lean(name)
}

pub(super) fn emit_spec_function_equivalence_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
) -> Option<AutoProof> {
    // Oracle v1: effectful laws rarely follow the "law name == spec fn
    // name" naming convention that `canonical_spec_ref` enforces — they
    // typically live as `verify pickOne law consistent` with a sibling
    // `pickOneSpec` on the RHS. Match on the rewritten law's structural
    // shape instead: both sides are direct calls with matching args to
    // two different fns, impl has classified effects. After the lift
    // the bodies reduce to the same term and `simpa` closes the goal.
    if let Some(proof) = emit_effectful_spec_equivalence_law(vb, law, ctx, intro_names) {
        return Some(proof);
    }

    let spec_ref = canonical_spec_ref(&vb.fn_name, law, ctx)?;
    let impl_fd = find_fn_def(ctx, &vb.fn_name)?;
    let spec_fd = find_fn_def(ctx, &spec_ref.spec_fn_name)?;
    let impl_body = body_terminal_expr(impl_fd.body.as_ref())?;
    let spec_body = body_terminal_expr(spec_fd.body.as_ref())?;
    let bodies_match_exactly =
        emit_expr_legacy(impl_body, ctx, None) == emit_expr_legacy(spec_body, ctx, None);

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
        {
            return None;
        }

        let simp_defs = law_simp_defs(ctx, vb, law).into_iter().collect::<Vec<_>>();
        Some(AutoProof {
            support_lines: Vec::new(),
            proof_lines: intro_then(
                intro_names,
                vec![format!("simpa [{}]", simp_defs.join(", "))],
            ),
            replaces_theorem: false,
        })
    };

    if bodies_match_exactly {
        return try_side(&law.lhs, &law.rhs).or_else(|| try_side(&law.rhs, &law.lhs));
    }

    simp_normalized::emit_simp_normalized_spec_equivalence_law(vb, law, ctx, intro_names)
        .or_else(|| {
            linear_recurrence2::emit_second_order_linear_recurrence_spec_equivalence_law(
                vb,
                law,
                ctx,
                intro_names,
            )
        })
        .or_else(|| {
            linear_int::emit_linear_int_omega_spec_equivalence_law(vb, law, ctx, intro_names)
        })
}

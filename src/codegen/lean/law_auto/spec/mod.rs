mod linear_int;
mod linear_recurrence2;

use crate::ast::{Expr, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;
use crate::verify_law::canonical_spec_ref;

use super::super::expr::emit_expr;
use super::shared::{body_terminal_expr, callee_matches_name, find_fn_def, law_simp_defs};
use super::{AutoProof, intro_then};

pub(super) fn emit_spec_function_equivalence_law(
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
    let bodies_match_exactly = emit_expr(impl_body, ctx) == emit_expr(spec_body, ctx);

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

    linear_recurrence2::emit_second_order_linear_recurrence_spec_equivalence_law(
        vb,
        law,
        ctx,
        intro_names,
    )
    .or_else(|| linear_int::emit_linear_int_omega_spec_equivalence_law(vb, law, ctx, intro_names))
}

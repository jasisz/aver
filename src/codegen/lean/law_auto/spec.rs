use crate::ast::{Expr, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;
use crate::verify_law::canonical_spec_ref;

use super::intro_then;
use super::shared::{callee_matches_name, law_simp_defs};

pub(super) fn emit_spec_function_equivalence_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
) -> Option<Vec<String>> {
    let spec_ref = canonical_spec_ref(&vb.fn_name, law, &ctx.fn_sigs)?;

    let try_side = |impl_side: &Expr, spec_side: &Expr| -> Option<Vec<String>> {
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
        Some(intro_then(
            intro_names,
            vec![format!("simpa [{}]", simp_defs.join(", "))],
        ))
    };

    try_side(&law.lhs, &law.rhs).or_else(|| try_side(&law.rhs, &law.lhs))
}

//! Render an obligation-local search budget from shared ProofIR. Keeping
//! universal citation lifts out of this context prevents their quantified
//! calls from multiplying the cost of deeper recursive unfolding.

use crate::ast::{VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;

pub(super) fn attributes(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    index: usize,
    ctx: &CodegenContext,
) -> Option<String> {
    let id = ctx.law_target_fn_id(&vb.fn_name)?;
    let plan = ctx
        .proof_ir
        .law_theorems
        .iter()
        .find(|t| t.fn_id == id && t.law_name == law.name)?
        .unfolding
        .get(index)?
        .as_ref()?;
    let mut targets: Vec<_> = plan
        .functions
        .iter()
        .map(|id| super::super::expr::function_name(*id, ctx))
        .collect();
    targets.extend(plan.reverse_elements.iter().map(|t| {
        format!(
            "ListReverse<{}>",
            super::super::toplevel::type_to_dafny_in_scope(t, None)
        )
    }));
    Some(
        targets
            .iter()
            .map(|name| format!("{{:fuel {name}, {}}}", plan.depth))
            .collect::<Vec<_>>()
            .join(" "),
    )
}

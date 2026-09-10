//! Dafny-only unfolding policy. These budgets never enter ProofIR: they do
//! not change a source claim, its premises, or its checked induction arguments.
mod search;
#[cfg(test)]
mod tests;

use crate::ast::{Type, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;
use crate::codegen::proof_lower::ProofLowerInputs;
use crate::ir::FnId;

/// Optional solver settings, not a derivation or a bound on quantified inputs.
struct UnfoldingHint {
    depth: u32,
    functions: Vec<FnId>,
    reverse_elements: Vec<Type>,
}

pub(super) fn attributes(
    _vb: &VerifyBlock,
    law: &VerifyLaw,
    index: usize,
    ctx: &CodegenContext,
) -> Option<String> {
    let expressions = if index == law.because.len() {
        vec![&law.lhs, &law.rhs]
    } else {
        vec![law.because.get(index)?]
    };
    let scope = ctx.active_module_scope();
    let inputs = ProofLowerInputs::from_ctx(ctx);
    let hint = search::plan(law, &expressions, &inputs, &ctx.proof_ir, scope.as_deref())?;
    let mut targets: Vec<_> = hint
        .functions
        .iter()
        .map(|id| super::super::expr::function_name(*id, ctx))
        .collect();
    targets.extend(hint.reverse_elements.iter().map(|t| {
        format!(
            "ListReverse<{}>",
            super::super::toplevel::type_to_dafny_in_scope(t, None)
        )
    }));
    Some(
        targets
            .iter()
            .map(|name| format!("{{:fuel {name}, {}}}", hint.depth))
            .collect::<Vec<_>>()
            .join(" "),
    )
}

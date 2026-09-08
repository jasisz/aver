//! Target search over the shared source cone. Budgets affect automation only;
//! admitting an ordinary universal still requires Dafny to check its full claim.

use crate::ast::{Type, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;
use crate::ir::FnId;
use std::collections::HashSet;

fn cone<'a>(vb: &VerifyBlock, law: &VerifyLaw, ctx: &'a CodegenContext) -> &'a [FnId] {
    ctx.law_target_fn_id(&vb.fn_name)
        .and_then(|id| {
            ctx.proof_ir
                .law_theorems
                .iter()
                .find(|t| t.fn_id == id && t.law_name == law.name)
        })
        .map(|t| t.function_cone.as_slice())
        .unwrap_or(&[])
}

/// A list binder makes this disjoint from the legacy finite-Int lane: the
/// universal never dispatches back to samples which call it. Keep oracle,
/// refinement and specialized-signature exclusions in the shared seed gate.
pub(super) fn native_sequence_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    opaque: &HashSet<FnId>,
    native: &HashSet<FnId>,
) -> bool {
    law.because.is_empty()
        && law.using.is_none()
        && law
            .givens
            .iter()
            .any(|g| matches!(crate::types::parse_type_str(&g.type_name), Type::List(_)))
        && super::toplevel::sample_seed_lemma_available(vb, law, ctx)
        && cone(vb, law, ctx).iter().any(|id| native.contains(id))
        && !cone(vb, law, ctx).iter().any(|id| opaque.contains(id))
}

pub(super) fn attributes(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    native_sequence: bool,
    native: &HashSet<FnId>,
) -> String {
    cone(vb, law, ctx)
        .iter()
        .map(|id| {
            // Moderate unfolding of every checked cycle member, including peers
            // hidden behind wrappers. This is a solver budget, never an input bound.
            let depth = if native_sequence && native.contains(id) && ctx.recursive_fns.contains(id)
            {
                12
            } else {
                5
            };
            format!(
                "{{:fuel {}, {depth}}}",
                super::expr::function_name(*id, ctx)
            )
        })
        .collect::<Vec<_>>()
        .join(" ")
}

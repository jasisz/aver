//! Target search over the shared source cone. Budgets affect automation only;
//! admitting an ordinary universal still requires Dafny to check its full claim.

use crate::ast::{Type, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;
use crate::ir::FnId;
use std::collections::HashSet;

/// The ordinary emitter's default universal has exactly the source givens
/// and `when`. A guided citation may call it, but must not promote a sampled
/// contract, an opaque dependency or a specialized support-stack signature.
/// The caller has already checked the entire source citation with `subset`.
/// Ordinary automatic citations only point to earlier ordinary laws in their
/// declaring module, so reusing one cannot create a cycle through guidance.
pub(super) fn reusable_ordinary_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    recursion: &super::toplevel::LawRecursion<'_>,
) -> bool {
    let Some(theorem) = ctx.law_target_fn_id(&vb.fn_name).and_then(|id| {
        ctx.proof_ir
            .law_theorems
            .iter()
            .find(|t| t.fn_id == id && t.law_name == law.name)
    }) else {
        return false;
    };
    super::toplevel::sample_seed_lemma_available(vb, law, ctx)
        && !crate::codegen::common::law_lhs_has_trace_projection(&law.lhs)
        && crate::codegen::common::law_map_order_refusal(vb, law, ctx).is_none()
        && !matches!(
            theorem.strategy,
            crate::ir::ProofStrategy::TailRecFixedBaseFold { .. }
                | crate::ir::ProofStrategy::FloorDivWindow { .. }
        )
        && !theorem.function_cone.iter().any(|id| {
            recursion.opaque_fns.contains(id) || recursion.termination_opaque.contains(id)
        })
        && (!theorem
            .function_cone
            .iter()
            .any(|id| recursion.native_callers.contains(id))
            || native_sequence_law(vb, law, ctx, recursion.opaque_fns, recursion.native_callers))
}

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

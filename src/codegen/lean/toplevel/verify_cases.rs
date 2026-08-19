//! Oracle arguments for cases-form `verify` claims.

use crate::ast::{Expr, Spanned, VerifyBlock, VerifyGiven, VerifyGivenDomain, VerifyLaw};
use crate::codegen::CodegenContext;

/// Rewrite a cases-form assertion against an Oracle-lifted function.
///
/// Explicit `given` stubs remain concrete sample arguments. Any classified
/// input effect without a `given` gets a theorem-local symbolic oracle. A
/// passing plain verify case cannot have dispatched such an effect (the VM's
/// reached-effect guard rejects it), so quantifying the unused oracle states
/// the useful stronger fact: this concrete branch has the expected result for
/// every implementation of the capability operation.
pub(super) fn rewrite_plain_case_oracles(
    vb: &VerifyBlock,
    case_index: usize,
    left: &Spanned<Expr>,
    right: &Spanned<Expr>,
    ctx: &CodegenContext,
) -> (Spanned<Expr>, Spanned<Expr>, Vec<(String, String)>) {
    use crate::types::checker::effect_classification::{EffectDimension, classify_with_registry};

    let scope = ctx.active_module_scope();
    let Some(fd) = ctx.fn_def_by_name(&vb.fn_name, scope.as_deref()) else {
        return (left.clone(), right.clone(), Vec::new());
    };
    if fd.effects.is_empty()
        || !fd
            .effects
            .iter()
            .all(|effect| classify_with_registry(&ctx.capabilities, &effect.node).is_some())
    {
        return (left.clone(), right.clone(), Vec::new());
    }

    let target_effects: std::collections::HashSet<&str> = fd
        .effects
        .iter()
        .map(|effect| effect.node.as_str())
        .collect();
    let mut givens: Vec<VerifyGiven> = vb
        .cases_givens
        .iter()
        .filter(|given| target_effects.contains(given.type_name.as_str()))
        .cloned()
        .collect();
    let explicitly_bound: std::collections::HashSet<String> =
        givens.iter().map(|given| given.type_name.clone()).collect();

    let Ok(oracle_params) =
        crate::types::checker::effect_lifting::oracle_params_for_effects_with_registry(
            &fd.effects,
            &ctx.capabilities,
        )
    else {
        return (left.clone(), right.clone(), Vec::new());
    };
    // `oracle_params_for_effects_with_registry` preserves first declaration
    // order. Rebuild the same deduplicated effect stream rather than relying on
    // HashSet iteration, so each generated name keeps its operation.
    let mut seen = std::collections::HashSet::new();
    let ordered_input_effects: Vec<&str> = fd
        .effects
        .iter()
        .filter_map(|effect| {
            let classification = classify_with_registry(&ctx.capabilities, &effect.node)?;
            if matches!(classification.dimension, EffectDimension::Output)
                || !seen.insert(effect.node.as_str())
            {
                return None;
            }
            Some(effect.node.as_str())
        })
        .collect();
    let mut theorem_params = Vec::new();
    let mut symbolic_bindings = Vec::new();
    for (effect, (name, type_annotation)) in ordered_input_effects.into_iter().zip(oracle_params) {
        if explicitly_bound.contains(effect) {
            continue;
        }
        givens.push(VerifyGiven {
            name: name.clone(),
            type_name: effect.to_string(),
            // Lemma/sample rewriting only needs the operation/name mapping;
            // the symbolic value is supplied below as a theorem parameter.
            domain: VerifyGivenDomain::Explicit(Vec::new()),
        });
        symbolic_bindings.push((
            name.clone(),
            Spanned::new(Expr::Ident(name.clone()), left.line),
        ));
        theorem_params.push((name, type_annotation));
    }

    let synthetic_law = VerifyLaw {
        name: String::new(),
        givens,
        when: None,
        lhs: left.clone(),
        rhs: right.clone(),
        sample_guards: Vec::new(),
    };
    let mut case_bindings = vb.case_givens.get(case_index).cloned().unwrap_or_default();
    case_bindings.extend(symbolic_bindings);
    let mode = crate::codegen::common::OracleInjectionMode::SampleCaseBinding(&case_bindings);
    let left = crate::codegen::common::rewrite_effectful_calls_in_law_with_registry(
        left,
        &synthetic_law,
        |name| ctx.fn_def_by_name(name, scope.as_deref()),
        mode.clone(),
        &ctx.capabilities,
    );
    let right = crate::codegen::common::rewrite_effectful_calls_in_law_with_registry(
        right,
        &synthetic_law,
        |name| ctx.fn_def_by_name(name, scope.as_deref()),
        mode,
        &ctx.capabilities,
    );

    theorem_params.extend(
        crate::codegen::common::law_fresh_resource_params(&synthetic_law, &ctx.capabilities)
            .into_iter()
            .map(|(_, name, type_annotation)| (name, type_annotation)),
    );
    (left, right, theorem_params)
}

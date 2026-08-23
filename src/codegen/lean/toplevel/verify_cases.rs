//! Oracle arguments for cases-form `verify` claims.

use crate::ast::{Expr, Spanned, VerifyBlock, VerifyGiven, VerifyGivenDomain, VerifyLaw};
use crate::codegen::CodegenContext;
use crate::codegen::lean::decl_order;

/// Fully-qualified user functions in a plain case's transitive call cone.
/// Symbolic-oracle cases must simplify the concrete branch far enough to
/// eliminate the otherwise free oracle before `native_decide`; unfolding only
/// the subject function is insufficient when branch selection happens through
/// a helper argument such as `get(fixture(...))`.
pub(super) fn plain_case_unfold_names(expr: &Spanned<Expr>, ctx: &CodegenContext) -> Vec<String> {
    let scope = ctx.active_module_scope();
    let resolved = ctx.resolve_expr(expr, scope.as_deref());
    let mut direct = std::collections::HashSet::new();
    decl_order::collect_resolved_fn_refs(&resolved, &mut direct);
    let mut pending: Vec<_> = direct.into_iter().collect();
    let mut seen = std::collections::HashSet::new();
    let mut names = std::collections::BTreeSet::new();
    while let Some(fn_id) = pending.pop() {
        if !seen.insert(fn_id) {
            continue;
        }
        let Some(rfd) = ctx.resolved_program.fn_by_id(fn_id) else {
            continue;
        };
        names.insert(ctx.symbol_table.fn_entry(fn_id).key.canonical());
        for stmt in rfd.body.stmts() {
            let expr = match stmt {
                crate::ir::hir::ResolvedStmt::Expr(expr) => expr,
                crate::ir::hir::ResolvedStmt::Binding { value, .. } => value,
            };
            let mut callees = std::collections::HashSet::new();
            decl_order::collect_resolved_fn_refs(expr, &mut callees);
            pending.extend(callees);
        }
    }
    names.into_iter().collect()
}

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

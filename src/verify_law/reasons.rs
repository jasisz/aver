//! Shared source semantics for law explanations. Explanations are obligations,
//! never guards: every sample checks them under only the original `when`.

use crate::ast::{Expr, Literal, Spanned, TopLevel, VerifyBlock, VerifyKind};

/// Materialize explanation samples through the same expansion used by hostile
/// verification. Keeping the synthetic law template also lets hostile mode
/// regenerate the checks at new values, rather than retaining declared samples.
pub fn sample_blocks(block: &VerifyBlock) -> Vec<VerifyBlock> {
    let VerifyKind::Law(law) = &block.kind else {
        return Vec::new();
    };
    law.because
        .iter()
        .enumerate()
        .map(|(index, reason)| {
            let mut result = block.clone();
            result.line = reason.line;
            result.case_spans = block
                .cases
                .iter()
                .map(|_| crate::ast::SourceSpan {
                    line: reason.line,
                    col: 5,
                    end_line: reason.line,
                    end_col: 12,
                })
                .collect();
            let mut explanation = (**law).clone();
            explanation.name = format!("{}.because{}", law.name, index + 1);
            explanation.lhs = reason.clone();
            explanation.rhs = Spanned::bare(Expr::Literal(Literal::Bool(true)));
            explanation.because.clear();
            explanation.using = None;
            let samples = super::expand::expand_law_cases(
                &explanation,
                super::expand::ExpansionMode::Declared,
            );
            result.cases = samples
                .iter()
                .map(|s| (s.lhs.clone(), s.rhs.clone()))
                .collect();
            result.case_givens = samples.iter().map(|s| s.bindings.clone()).collect();
            explanation.sample_guards = samples.into_iter().filter_map(|s| s.guard).collect();
            result.kind = VerifyKind::Law(Box::new(explanation));
            result
        })
        .collect()
}

/// Explicit dependency names are source identities, not Lean declaration text.
/// Imported laws follow the existing exposed-subject visibility rule.
pub fn dependency_errors<'a>(
    items: &[TopLevel],
    loaded: impl IntoIterator<Item = &'a crate::source::LoadedModule>,
) -> Vec<(usize, String)> {
    use std::collections::{BTreeMap, BTreeSet};
    let laws: BTreeMap<String, &VerifyBlock> = items
        .iter()
        .filter_map(|item| {
            let TopLevel::Verify(block) = item else {
                return None;
            };
            let VerifyKind::Law(law) = &block.kind else {
                return None;
            };
            Some((format!("{}.{}", block.fn_name, law.name), block))
        })
        .collect();
    let mut available: BTreeSet<String> = laws.keys().cloned().collect();
    for module in loaded {
        for block in crate::codegen::collect_verify_laws(&module.items) {
            if let VerifyKind::Law(law) = &block.kind {
                available.insert(format!(
                    "{}.{}.{}",
                    module.dep_name, block.fn_name, law.name
                ));
            }
        }
    }
    let mut errors = Vec::new();
    for (name, block) in &laws {
        let VerifyKind::Law(law) = &block.kind else {
            unreachable!()
        };
        for dependency in law.using.iter().flatten() {
            if !available.contains(dependency) {
                errors.push((
                    block.line,
                    format!("Law '{name}' uses unknown or unexposed law '{dependency}'"),
                ));
            }
            let mut pending = vec![dependency.as_str()];
            let mut seen = BTreeSet::new();
            while let Some(next) = pending.pop() {
                if next == name {
                    errors.push((
                        block.line,
                        format!(
                            "Law '{name}' has a cyclic 'using' dependency through '{dependency}'"
                        ),
                    ));
                    break;
                }
                if seen.insert(next)
                    && let Some(target) = laws.get(next)
                    && let VerifyKind::Law(other) = &target.kind
                {
                    pending.extend(other.using.iter().flatten().map(String::as_str));
                }
            }
        }
    }
    errors
}

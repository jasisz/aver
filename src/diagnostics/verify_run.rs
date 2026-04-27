//! Execute verify blocks via the VM and surface results as canonical
//! [`Diagnostic`]s. Shared by CLI (`aver verify`), LSP, and the playground
//! Verify panel so every consumer sees identical outcomes for identical
//! source.
//!
//! Delegates the actual run to [`super::vm_verify::run_verify_for_items_vm`]
//! / `_with_loaded` — the Oracle-aware runner that handles `given` stubs,
//! trace-projection stripping, and `.trace.*` post-processing. This module
//! converts the runner's `VerifyResult`s into failing-case diagnostics plus
//! a per-block scorecard.
//!
//! Requires the `runtime` feature (needs the VM).

use crate::ast::TopLevel;
use crate::checker::{VerifyCaseOutcome, VerifyResult};

use super::factories::{
    verify_mismatch_diagnostic, verify_runtime_error_diagnostic, verify_unexpected_err_diagnostic,
};
use super::model::{Diagnostic, VerifyBlockResult, VerifySummary};
use super::vm_verify;
use crate::verify_law::expand::ExpansionMode;

/// Run every verify block found in `items` and return failing-case
/// diagnostics plus a per-block scorecard.
///
/// Passes emit no diagnostic. Guards that evaluate to `false` count as
/// skipped (same semantics as CLI `aver verify`).
pub fn run_verify_blocks(
    items: Vec<TopLevel>,
    base_dir: Option<&str>,
    file_label: &str,
    source: &str,
) -> (Vec<Diagnostic>, VerifySummary) {
    run_verify_blocks_with_mode(items, base_dir, file_label, source, ExpansionMode::Declared)
}

/// Like [`run_verify_blocks`] but lets the caller request hostile mode
/// (`aver audit --hostile`, playground "Hostile" toggle). Hostile expands
/// each `verify ... law` block's `given` domains with the per-type boundary
/// set and multiplies cases by every applicable adversarial effect profile.
pub fn run_verify_blocks_with_mode(
    items: Vec<TopLevel>,
    base_dir: Option<&str>,
    file_label: &str,
    source: &str,
    mode: ExpansionMode,
) -> (Vec<Diagnostic>, VerifySummary) {
    let config = base_dir.and_then(load_project_config);
    let results = match vm_verify::run_verify_for_items_vm_with_mode(
        items,
        config,
        base_dir,
        file_label,
        mode,
    ) {
        Ok(r) => r,
        Err(_) => {
            return (Vec::new(), VerifySummary { blocks: Vec::new() });
        }
    };
    map_results_to_diagnostics(results, file_label, source)
}

/// Variant that accepts pre-loaded dependency modules (e.g. from the
/// playground's virtual fs) instead of a disk module root.
pub fn run_verify_blocks_with_loaded(
    items: Vec<TopLevel>,
    loaded: Vec<crate::source::LoadedModule>,
    file_label: &str,
    source: &str,
) -> (Vec<Diagnostic>, VerifySummary) {
    run_verify_blocks_with_loaded_and_mode(
        items,
        loaded,
        file_label,
        source,
        ExpansionMode::Declared,
    )
}

pub fn run_verify_blocks_with_loaded_and_mode(
    items: Vec<TopLevel>,
    loaded: Vec<crate::source::LoadedModule>,
    file_label: &str,
    source: &str,
    mode: ExpansionMode,
) -> (Vec<Diagnostic>, VerifySummary) {
    let results = match vm_verify::run_verify_for_items_vm_with_loaded_and_mode(
        items,
        loaded,
        None,
        file_label,
        mode,
    ) {
        Ok(r) => r,
        Err(_) => {
            return (Vec::new(), VerifySummary { blocks: Vec::new() });
        }
    };
    map_results_to_diagnostics(results, file_label, source)
}

fn load_project_config(base_dir: &str) -> Option<crate::config::ProjectConfig> {
    crate::config::ProjectConfig::load_from_dir(std::path::Path::new(base_dir))
        .ok()
        .flatten()
}

fn map_results_to_diagnostics(
    results: Vec<VerifyResult>,
    file_label: &str,
    source: &str,
) -> (Vec<Diagnostic>, VerifySummary) {
    let mut diagnostics = Vec::new();
    let mut blocks = Vec::with_capacity(results.len());

    for result in results {
        let is_law = result.block_label.contains(" spec ");
        let (declared_passed, declared_failed, hostile_passed, hostile_failed) =
            split_hostile_counts(&result.case_results);
        blocks.push(VerifyBlockResult {
            name: result.fn_name.clone(),
            passed: result.passed,
            failed: result.failed,
            skipped: result.skipped,
            total: result.passed + result.failed + result.skipped,
            declared_passed,
            declared_failed,
            hostile_passed,
            hostile_failed,
        });

        // Group `Mismatch` outcomes that share the same (case_expr,
        // span line) — under `--hostile` a single broken case
        // typically fails across multiple adversarial profiles, and
        // emitting one diagnostic per (case × profile) drowns the
        // user in near-identical entries with the same repair text.
        // Other outcomes (RuntimeError / UnexpectedErr) stay
        // per-case; they're already rare.
        use std::collections::HashMap;
        let mut mismatch_groups: HashMap<(String, usize), Vec<usize>> = HashMap::new();
        let mut mismatch_order: Vec<(String, usize)> = Vec::new();
        for (idx, case) in result.case_results.iter().enumerate() {
            if matches!(case.outcome, VerifyCaseOutcome::Mismatch { .. }) {
                let line = case.span.as_ref().map(|s| s.line).unwrap_or(1);
                let key = (case.case_expr.clone(), line);
                if !mismatch_groups.contains_key(&key) {
                    mismatch_order.push(key.clone());
                }
                mismatch_groups.entry(key).or_default().push(idx);
            }
        }

        // Emit one diagnostic per mismatch group.
        for key in &mismatch_order {
            let group = &mismatch_groups[key];
            let primary_case = &result.case_results[group[0]];
            let (line, col) = primary_case
                .span
                .as_ref()
                .map(|s| (s.line, s.col))
                .unwrap_or((1, 1));
            let (expected, actual) = match &primary_case.outcome {
                VerifyCaseOutcome::Mismatch { expected, actual } => {
                    (expected.clone(), actual.clone())
                }
                _ => unreachable!("filtered above"),
            };
            let mut diag = verify_mismatch_diagnostic(
                file_label,
                source,
                &result.fn_name,
                &primary_case.case_expr,
                &expected,
                &actual,
                line,
                col,
                is_law,
                None,
                primary_case.from_hostile,
                primary_case.hostile_profile.as_deref(),
            );
            // Append every other origin in the group as an extra
            // `("origin", ...)` field so the renderer can list each
            // world the same case broke under. Skip duplicates.
            for &other_idx in &group[1..] {
                let other = &result.case_results[other_idx];
                let origin = match (other.from_hostile, other.hostile_profile.as_deref()) {
                    (true, Some(profile)) => {
                        format!("hostile effect profile: {}", profile)
                    }
                    (true, None) => "hostile boundary expansion".to_string(),
                    (false, _) => continue,
                };
                if !diag
                    .fields
                    .iter()
                    .any(|(k, v)| *k == "origin" && v == &origin)
                {
                    diag.fields.push(("origin", origin));
                }
            }
            diagnostics.push(diag);
        }

        // Non-mismatch outcomes — emit per case as before.
        for case in &result.case_results {
            let (line, col) = case
                .span
                .as_ref()
                .map(|s| (s.line, s.col))
                .unwrap_or((1, 1));
            match &case.outcome {
                VerifyCaseOutcome::Pass
                | VerifyCaseOutcome::Skipped
                | VerifyCaseOutcome::Mismatch { .. } => {}
                VerifyCaseOutcome::UnexpectedErr { err_repr } => {
                    diagnostics.push(verify_unexpected_err_diagnostic(
                        file_label,
                        source,
                        &result.fn_name,
                        &case.case_expr,
                        err_repr,
                        line,
                        col,
                    ));
                }
                VerifyCaseOutcome::RuntimeError { error } => {
                    diagnostics.push(verify_runtime_error_diagnostic(
                        file_label,
                        source,
                        &result.fn_name,
                        &case.case_expr,
                        error,
                        line,
                        col,
                    ));
                }
            }
        }
    }

    (diagnostics, VerifySummary { blocks })
}

/// Bucket case outcomes by `from_hostile` so the per-block summary can
/// report declared-vs-hostile pass/fail breakdown. Skipped cases (when
/// guard returned false) are not counted in either bucket — they're in
/// `result.skipped`.
fn split_hostile_counts(
    cases: &[crate::checker::VerifyCaseResult],
) -> (usize, usize, usize, usize) {
    use crate::checker::VerifyCaseOutcome;

    let mut declared_passed = 0usize;
    let mut declared_failed = 0usize;
    let mut hostile_passed = 0usize;
    let mut hostile_failed = 0usize;
    for case in cases {
        let passed = matches!(case.outcome, VerifyCaseOutcome::Pass);
        let skipped = matches!(case.outcome, VerifyCaseOutcome::Skipped);
        if skipped {
            continue;
        }
        match (case.from_hostile, passed) {
            (false, true) => declared_passed += 1,
            (false, false) => declared_failed += 1,
            (true, true) => hostile_passed += 1,
            (true, false) => hostile_failed += 1,
        }
    }
    (
        declared_passed,
        declared_failed,
        hostile_passed,
        hostile_failed,
    )
}

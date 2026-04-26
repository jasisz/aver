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
    let config = base_dir.and_then(load_project_config);
    let results = match vm_verify::run_verify_for_items_vm(items, config, base_dir, file_label) {
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
    let results =
        match vm_verify::run_verify_for_items_vm_with_loaded(items, loaded, None, file_label) {
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

        for case in &result.case_results {
            let (line, col) = case
                .span
                .as_ref()
                .map(|s| (s.line, s.col))
                .unwrap_or((1, 1));
            match &case.outcome {
                VerifyCaseOutcome::Pass | VerifyCaseOutcome::Skipped => {}
                VerifyCaseOutcome::Mismatch { expected, actual } => {
                    diagnostics.push(verify_mismatch_diagnostic(
                        file_label,
                        source,
                        &result.fn_name,
                        &case.case_expr,
                        expected,
                        actual,
                        line,
                        col,
                        is_law,
                        None,
                        case.from_hostile,
                        case.hostile_profile.as_deref(),
                    ));
                }
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

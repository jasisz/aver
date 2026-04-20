//! Single-file analysis pipeline.
//!
//! `analyze_source` is the canonical entry for going from source text to
//! diagnostics. Runtime-neutral: no file IO, no config, no VM. Multi-file
//! concerns (unused exposes, config suppression, dependency resolution)
//! stay in CLI / LSP callers.

use super::factories::{from_check_finding, from_type_error, unused_binding_diagnostic};
use super::model::{AnalysisReport, Diagnostic, Severity, Span};
use crate::checker::{
    CheckFinding, check_module_intent_with_sigs_in, collect_cse_warnings_in,
    collect_independence_warnings_in, collect_perf_warnings_in,
    collect_verify_coverage_warnings_in,
};
#[cfg(feature = "runtime")]
use crate::checker::{FindingSpan, collect_verify_law_dependency_warnings_in};
use crate::source::parse_source;
#[cfg(feature = "runtime")]
use crate::tail_check::collect_non_tail_recursion_warnings_with_sigs;
use crate::tco;
use crate::types::checker::run_type_check_full;

/// Options for `analyze_source`. Defaults enable every available collector.
#[derive(Clone, Debug)]
pub struct AnalyzeOptions {
    pub file_label: String,
    pub module_base_dir: Option<String>,
    pub include_intent_warnings: bool,
    pub include_coverage_warnings: bool,
    pub include_law_dependency_warnings: bool,
    pub include_cse_warnings: bool,
    pub include_perf_warnings: bool,
    pub include_independence_warnings: bool,
    pub include_non_tail_warnings: bool,
    pub include_unused_bindings: bool,
    /// When `true` **and** the `runtime` feature is enabled, execute every
    /// verify block found in the source and emit a diagnostic per failing
    /// case. Off by default: analysis should stay pure static checks;
    /// callers opt in explicitly.
    pub include_verify_run: bool,
}

impl Default for AnalyzeOptions {
    fn default() -> Self {
        Self {
            file_label: "<input>".to_string(),
            module_base_dir: None,
            include_intent_warnings: true,
            include_coverage_warnings: true,
            include_law_dependency_warnings: true,
            include_cse_warnings: true,
            include_perf_warnings: true,
            include_independence_warnings: true,
            include_non_tail_warnings: true,
            include_unused_bindings: true,
            include_verify_run: false,
        }
    }
}

impl AnalyzeOptions {
    pub fn new(file_label: impl Into<String>) -> Self {
        Self {
            file_label: file_label.into(),
            ..Default::default()
        }
    }

    pub fn with_module_base_dir(mut self, dir: impl Into<String>) -> Self {
        self.module_base_dir = Some(dir.into());
        self
    }
}

/// Run the single-file analysis pipeline.
///
/// Pipeline: parse → TCO → typecheck → collectors → canonical diagnostics.
/// Returns all diagnostics encountered; does not stop at first error.
pub fn analyze_source(source: &str, options: &AnalyzeOptions) -> AnalysisReport {
    let items = match parse_source(source) {
        Ok(items) => items,
        Err(e) => {
            return AnalysisReport::with_diagnostics(
                options.file_label.clone(),
                vec![parse_error_diagnostic(&e, source, &options.file_label)],
            );
        }
    };

    let mut transformed = items.clone();
    tco::transform_program(&mut transformed);

    let tc_result = run_type_check_full(&items, options.module_base_dir.as_deref());

    let mut diagnostics: Vec<Diagnostic> = Vec::new();

    for te in &tc_result.errors {
        diagnostics.push(from_type_error(te, source, &options.file_label));
    }

    let findings = if options.include_intent_warnings {
        Some(check_module_intent_with_sigs_in(
            &items,
            Some(&tc_result.fn_sigs),
            None,
        ))
    } else {
        None
    };

    if let Some(ref findings) = findings {
        for e in &findings.errors {
            diagnostics.push(from_check_finding(Severity::Error, e, source, &options.file_label));
        }
    }

    if options.include_unused_bindings {
        for (binding, fn_name, line) in &tc_result.unused_bindings {
            diagnostics.push(unused_binding_diagnostic(
                binding,
                fn_name,
                *line,
                source,
                &options.file_label,
            ));
        }
    }

    if let Some(ref findings) = findings {
        for w in &findings.warnings {
            diagnostics.push(from_check_finding(
                Severity::Warning,
                w,
                source,
                &options.file_label,
            ));
        }
    }

    if options.include_coverage_warnings {
        for w in collect_verify_coverage_warnings_in(&items, None) {
            diagnostics.push(from_check_finding(
                Severity::Warning,
                &w,
                source,
                &options.file_label,
            ));
        }
    }

    #[cfg(feature = "runtime")]
    if options.include_law_dependency_warnings {
        for w in
            collect_verify_law_dependency_warnings_in(&items, &tc_result.fn_sigs, None)
        {
            diagnostics.push(from_check_finding(
                Severity::Warning,
                &w,
                source,
                &options.file_label,
            ));
        }
    }

    if options.include_cse_warnings {
        for w in collect_cse_warnings_in(&transformed, None) {
            diagnostics.push(from_check_finding(
                Severity::Warning,
                &w,
                source,
                &options.file_label,
            ));
        }
    }

    if options.include_perf_warnings {
        for w in collect_perf_warnings_in(&transformed, None) {
            diagnostics.push(from_check_finding(
                Severity::Warning,
                &w,
                source,
                &options.file_label,
            ));
        }
    }

    if options.include_independence_warnings {
        for w in collect_independence_warnings_in(&transformed, &tc_result.fn_sigs, None) {
            diagnostics.push(from_check_finding(
                Severity::Warning,
                &w,
                source,
                &options.file_label,
            ));
        }
    }

    #[cfg(feature = "runtime")]
    if options.include_verify_run && tc_result.errors.is_empty() {
        // Verify execution only runs when typecheck is clean — otherwise
        // the compiled VM would crash on missing symbols.
        let runnable_items = items.clone();
        for diag in super::verify_run::run_verify_blocks(
            runnable_items,
            options.module_base_dir.as_deref(),
            &options.file_label,
            source,
        ) {
            diagnostics.push(diag);
        }
    }

    #[cfg(feature = "runtime")]
    if options.include_non_tail_warnings {
        let non_tail =
            collect_non_tail_recursion_warnings_with_sigs(&transformed, &tc_result.fn_sigs);
        for w in &non_tail {
            let mut line_counts: Vec<(usize, usize)> = Vec::new();
            for &ln in &w.callsite_lines {
                if let Some(entry) = line_counts.iter_mut().find(|(l, _)| *l == ln) {
                    entry.1 += 1;
                } else {
                    line_counts.push((ln, 1));
                }
            }
            let max_shown = 3;
            let extra_spans: Vec<FindingSpan> = line_counts
                .iter()
                .take(max_shown)
                .map(|&(ln, count)| {
                    let label = if count > 1 {
                        format!("{} non-tail calls", count)
                    } else {
                        "non-tail call".to_string()
                    };
                    FindingSpan {
                        line: ln,
                        col: 0,
                        len: 0,
                        label,
                    }
                })
                .collect();
            let finding = CheckFinding {
                line: w.line,
                module: None,
                file: None,
                fn_name: Some(w.fn_name.clone()),
                message: w.message.clone(),
                extra_spans,
            };
            diagnostics.push(from_check_finding(
                Severity::Warning,
                &finding,
                source,
                &options.file_label,
            ));
        }
    }

    AnalysisReport::with_diagnostics(options.file_label.clone(), diagnostics)
}

/// Build a minimal `Diagnostic` for a parser error. Errors surface as a
/// single line-1 diagnostic with the parser message as summary.
fn parse_error_diagnostic(msg: &str, _source: &str, file: &str) -> Diagnostic {
    use super::model::Repair;
    Diagnostic {
        severity: Severity::Error,
        slug: "parse-error",
        summary: msg.to_string(),
        span: Span {
            file: file.to_string(),
            line: 1,
            col: 1,
        },
        fn_name: None,
        intent: None,
        fields: Vec::new(),
        conflict: None,
        repair: Repair::default(),
        regions: Vec::new(),
        related: Vec::new(),
    }
}

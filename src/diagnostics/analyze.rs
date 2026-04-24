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
    collect_independence_warnings_in, collect_naming_warnings_in, collect_perf_warnings_in,
    collect_plain_cases_effectful_warnings_in, collect_verify_coverage_warnings_in,
};
#[cfg(feature = "runtime")]
use crate::checker::{FindingSpan, collect_verify_law_dependency_warnings_in};
use crate::source::{LoadedModule, parse_source};
#[cfg(feature = "runtime")]
use crate::tail_check::collect_non_tail_recursion_warnings_with_sigs;
use crate::tco;
use crate::types::checker::{run_type_check_full, run_type_check_with_loaded};

/// Options for `analyze_source`. Defaults enable every available collector.
#[derive(Clone, Debug)]
pub struct AnalyzeOptions {
    pub file_label: String,
    pub module_base_dir: Option<String>,
    /// Pre-resolved dependency modules (e.g. from a virtual filesystem
    /// in the playground). When set, takes precedence over
    /// `module_base_dir` — the type checker integrates these directly
    /// instead of loading from disk.
    pub loaded_modules: Option<Vec<LoadedModule>>,
    pub include_intent_warnings: bool,
    pub include_coverage_warnings: bool,
    pub include_law_dependency_warnings: bool,
    pub include_cse_warnings: bool,
    pub include_perf_warnings: bool,
    pub include_independence_warnings: bool,
    pub include_naming_warnings: bool,
    pub include_non_tail_warnings: bool,
    pub include_unused_bindings: bool,
    /// Plain cases-form `verify fn` on an effectful fn whose effect list
    /// includes at least one generative effect. The test runs with real
    /// effects and the RHS is compared against a non-deterministic value.
    pub include_verify_effectful_warnings: bool,
    /// When `true` **and** the `runtime` feature is enabled, execute every
    /// verify block found in the source and emit a diagnostic per failing
    /// case. Off by default: analysis should stay pure static checks;
    /// callers opt in explicitly.
    pub include_verify_run: bool,
    /// When `true`, populate `AnalysisReport::why_summary` with
    /// per-function justification data. Off by default.
    pub include_why_summary: bool,
    /// When `true`, populate `AnalysisReport::context_summary` with
    /// module shape / function / type / decision summary.
    pub include_context_summary: bool,
}

impl Default for AnalyzeOptions {
    fn default() -> Self {
        Self {
            file_label: "<input>".to_string(),
            module_base_dir: None,
            loaded_modules: None,
            include_intent_warnings: true,
            include_coverage_warnings: true,
            include_law_dependency_warnings: true,
            include_cse_warnings: true,
            include_perf_warnings: true,
            include_independence_warnings: true,
            include_naming_warnings: true,
            include_non_tail_warnings: true,
            include_unused_bindings: true,
            include_verify_effectful_warnings: true,
            include_verify_run: false,
            include_why_summary: false,
            include_context_summary: false,
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

    pub fn with_loaded_modules(mut self, loaded: Vec<LoadedModule>) -> Self {
        self.loaded_modules = Some(loaded);
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

    let tc_result = if let Some(loaded) = options.loaded_modules.as_deref() {
        run_type_check_with_loaded(&items, loaded)
    } else {
        run_type_check_full(&items, options.module_base_dir.as_deref())
    };

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
            diagnostics.push(from_check_finding(
                Severity::Error,
                e,
                source,
                &options.file_label,
            ));
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
        for w in collect_verify_law_dependency_warnings_in(&items, &tc_result.fn_sigs, None) {
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

    if options.include_verify_effectful_warnings {
        for w in
            collect_plain_cases_effectful_warnings_in(&transformed, &tc_result.fn_sigs, None)
        {
            diagnostics.push(from_check_finding(
                Severity::Warning,
                &w,
                source,
                &options.file_label,
            ));
        }
    }

    if options.include_naming_warnings {
        for w in collect_naming_warnings_in(&items, None) {
            diagnostics.push(from_check_finding(
                Severity::Warning,
                &w,
                source,
                &options.file_label,
            ));
        }
    }

    #[cfg(feature = "runtime")]
    let verify_summary_opt = if options.include_verify_run && tc_result.errors.is_empty() {
        // Verify execution only runs when typecheck is clean — otherwise
        // the compiled VM would crash on missing symbols. Multi-file
        // now works through the same VM path via loaded_modules →
        // compile_program_with_loaded_modules.
        let runnable_items = items.clone();
        let (verify_diags, verify_summary) = if let Some(loaded) = options.loaded_modules.clone() {
            super::verify_run::run_verify_blocks_with_loaded(
                runnable_items,
                loaded,
                &options.file_label,
                source,
            )
        } else {
            super::verify_run::run_verify_blocks(
                runnable_items,
                options.module_base_dir.as_deref(),
                &options.file_label,
                source,
            )
        };
        for diag in verify_diags {
            diagnostics.push(diag);
        }
        Some(verify_summary)
    } else {
        None
    };
    #[cfg(not(feature = "runtime"))]
    let verify_summary_opt: Option<super::model::VerifySummary> = None;

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

    let mut report = AnalysisReport::with_diagnostics(options.file_label.clone(), diagnostics);
    report.verify_summary = verify_summary_opt;

    if options.include_why_summary {
        report.why_summary = Some(super::why::summarize(
            &items,
            source,
            options.file_label.clone(),
        ));
    }

    if options.include_context_summary {
        let ctx = super::context::build_context_for_items(
            &items,
            source,
            options.file_label.clone(),
            options.module_base_dir.as_deref(),
        );
        report.context_summary = Some(super::context::summarize(&ctx));
    }

    report
}

/// Build a `Diagnostic` for a parser error.
///
/// Parser emits its message as `error[LINE:COL]: <body>` (see
/// `ParseError::Display`). We strip the prefix to rebuild the real
/// span, add a source region anchored on that line, and map common
/// patterns to a repair hint — otherwise the CLI / playground showed
/// parse errors pointing at line 1:1 with no fix suggestion.
fn parse_error_diagnostic(msg: &str, source: &str, file: &str) -> Diagnostic {
    use super::classify::{estimate_span_len, extract_source_lines_range};
    use super::model::{AnnotatedRegion, Underline};
    let (line, col, body) = strip_parse_error_prefix(msg);
    let regions = if line > 0 {
        // Include one line of pre-context so the reader sees the
        // surrounding code, but stop at the target line so the
        // underline renders directly beneath it (tty_render draws
        // the caret after the last line of the region).
        let start = line.saturating_sub(1).max(1);
        let source_lines = extract_source_lines_range(source, start, line);
        if source_lines.is_empty() {
            Vec::new()
        } else {
            // Underline the offending token. Parser emits col =
            // line_len + 1 for errors that fire at the newline
            // (e.g. Unterminated string literal); clamp to the last
            // real char so the caret doesn't float off the end of
            // the line.
            let underline = source.lines().nth(line.saturating_sub(1)).map(|l| {
                let line_chars = l.chars().count();
                let anchor = if col > line_chars && line_chars > 0 {
                    line_chars
                } else {
                    col.max(1)
                };
                Underline {
                    col: anchor,
                    len: estimate_span_len(l, anchor),
                    label: String::new(),
                }
            });
            vec![AnnotatedRegion {
                source_lines,
                underline,
            }]
        }
    } else {
        Vec::new()
    };
    Diagnostic {
        severity: Severity::Error,
        slug: "parse-error",
        summary: body.to_string(),
        span: Span {
            file: file.to_string(),
            line: line.max(1),
            col: col.max(1),
        },
        fn_name: None,
        intent: None,
        fields: Vec::new(),
        conflict: None,
        repair: parse_error_repair(body),
        regions,
        related: Vec::new(),
    }
}

fn strip_parse_error_prefix(msg: &str) -> (usize, usize, &str) {
    // `error[LINE:COL]: body` — the parser's Display impl (see
    // src/parser/mod.rs). Lexer errors may share the shape.
    let Some(rest) = msg.strip_prefix("error[") else {
        return (0, 0, msg);
    };
    let Some(close) = rest.find("]: ") else {
        return (0, 0, msg);
    };
    let (coord, tail) = rest.split_at(close);
    let body = &tail[3..];
    let Some((line_s, col_s)) = coord.split_once(':') else {
        return (0, 0, body);
    };
    let line = line_s.parse::<usize>().unwrap_or(0);
    let col = col_s.parse::<usize>().unwrap_or(0);
    (line, col, body)
}

fn parse_error_repair(body: &str) -> super::model::Repair {
    // Map common parser messages to a concrete nudge. The parser
    // emits short human strings (`Expected X, found Y`, `Expected '['
    // after '!'`, ...); we pattern-match on the shape so the repair
    // points at a likely fix instead of leaving the user staring at
    // "found EOF".
    use super::model::Repair;
    let hint = if body.contains("after '?'") {
        Some("Description needs a string literal: `? \"what this does\"`")
    } else if body.contains("after 'intent ='") {
        Some(
            "Module intent is a string or an indented block of strings: `intent = \"one line\"` or `intent =\\n    \"line one\"\\n    \"line two\"`",
        )
    } else if body.contains("Expected '[' after '!'") {
        Some("Effects are a bracketed list: `! [Console.print, Random.int]`")
    } else if body.contains("Expected '=>' between key and value in map literal") {
        Some("Map literal uses `=>`: `{\"k\" => 1, \"other\" => 2}`")
    } else if body.contains("Tuple type must have at least 2 elements") {
        Some("Single-element tuples aren't allowed — use the bare type, or add a second element.")
    } else if body.contains("Constructor patterns must be qualified") {
        Some(
            "Qualify variant patterns with the type name: `Shape.Circle(r) ->` not `Circle(r) ->`.",
        )
    } else if body.contains("bind the whole value with a lower-case name") {
        Some(
            "Record patterns don't take positional args — bind the whole record: `match user ... u -> u.name`.",
        )
    } else if body.starts_with("Expected ") && body.contains(", found ") {
        Some(
            "Replace the unexpected token with the expected form; check for a missing keyword, bracket, or separator above.",
        )
    } else if body.contains("must place `module <Name>`") {
        Some("Move `module <Name>` so it's the very first top-level item in the file.")
    } else if body.contains("must declare `module <Name>`") {
        Some("Add `module <Name>` as the first line of the file.")
    } else if body.contains("must contain exactly one module declaration") {
        Some("Keep one `module` per file — split multi-module files into one file each.")
    } else {
        None
    };
    Repair {
        primary: hint.map(String::from),
        ..Repair::default()
    }
}

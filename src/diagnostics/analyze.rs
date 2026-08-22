//! Single-file analysis pipeline.
//!
//! `analyze_source` is the canonical entry for going from source text to
//! diagnostics. Runtime-neutral: no VM, and no file IO beyond what naming a
//! project implies — a caller that sets both `module_base_dir` and
//! `source_path` is pointing at a real project, and the parse reads its
//! `aver.toml` for the verify-case ceiling. Multi-file concerns (unused
//! exposes, config suppression, dependency resolution) stay in CLI / LSP
//! callers.

#[cfg(feature = "runtime")]
use super::factories::verify_provider_setup_diagnostic;
use super::factories::{from_check_finding, from_type_error, unused_binding_diagnostic};
use super::model::{AnalysisReport, Diagnostic, Severity, Span};
use crate::checker::{
    CheckFinding, check_module_intent_with_sigs_in, collect_cse_warnings_in,
    collect_independence_warnings_in, collect_module_effects_warnings_in,
    collect_naming_warnings_in, collect_perf_warnings_in, collect_traversal_warnings_in,
    collect_verify_coverage_warnings_in,
};
#[cfg(feature = "runtime")]
use crate::checker::{FindingSpan, collect_verify_law_dependency_warnings_in};
use crate::source::{LoadedModule, parse_source_with_verify_ceiling};
#[cfg(feature = "runtime")]
use crate::tail_check::collect_non_tail_recursion_warnings_with_sigs;

/// Options for `analyze_source`. Defaults enable every available collector.
#[derive(Clone, Debug)]
pub struct AnalyzeOptions {
    pub file_label: String,
    pub module_base_dir: Option<String>,
    /// Where this source lives on disk, when it lives anywhere. Set together
    /// with `module_base_dir` it names a file of a real project, and the
    /// parse honours the verify-case ceiling that project declared for it —
    /// the same ceiling `aver verify`'s loader applies. Left `None` by the
    /// playground and by editor scratch buffers, which have no `aver.toml`
    /// to ask, so those parse under the built-in default.
    pub source_path: Option<String>,
    /// Pre-resolved dependency modules (e.g. from a virtual filesystem
    /// in the playground). When set, takes precedence over
    /// `module_base_dir` — the type checker integrates these directly
    /// instead of loading from disk.
    pub loaded_modules: Option<Vec<LoadedModule>>,
    /// `(module_name, ignored_file)` pairs for `depends` entries that
    /// resolve to an embedded standard module while a same-named project
    /// file exists (which module resolution silently ignores). Precomputed
    /// by callers — `crate::source::collect_stdlib_shadowed` (disk) or
    /// `collect_stdlib_shadowed_in_map` (playground) — because detection
    /// needs the caller's file universe; each entry becomes a warning.
    pub stdlib_shadowed: Vec<(String, String)>,
    pub include_intent_warnings: bool,
    pub include_coverage_warnings: bool,
    pub include_law_dependency_warnings: bool,
    pub include_cse_warnings: bool,
    pub include_perf_warnings: bool,
    /// 0.15 Traversal antipattern lints — surfaces uses of recursive
    /// list builders feeding `Vector.fromList`, `Map.fromList`, or a
    /// standalone `List.reverse`, where Aver has a more direct primitive.
    /// Companion to the buffer-build deforestation pass: what we don't
    /// fuse, we warn about.
    pub include_traversal_warnings: bool,
    pub include_independence_warnings: bool,
    pub include_naming_warnings: bool,
    pub include_non_tail_warnings: bool,
    pub include_unused_bindings: bool,
    /// When `true` **and** the `runtime` feature is enabled, execute every
    /// verify block found in the source and emit a diagnostic per failing
    /// case. Off by default: analysis should stay pure static checks;
    /// callers opt in explicitly.
    pub include_verify_run: bool,
    /// When `true` and `include_verify_run` is also `true`, run verify
    /// blocks under `--hostile` mode: typed `given` domains are expanded
    /// with the per-type boundary set and each case is multiplied by the
    /// adversarial effect-profile cartesian. Failures that surface only
    /// here are flagged with `from_hostile = true` so the renderer can
    /// suggest weakening the law (`when`) or pinning the effect (`given`).
    pub verify_run_hostile: bool,
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
            source_path: None,
            loaded_modules: None,
            stdlib_shadowed: Vec::new(),
            include_intent_warnings: true,
            include_coverage_warnings: true,
            include_law_dependency_warnings: true,
            include_cse_warnings: true,
            include_perf_warnings: true,
            include_traversal_warnings: true,
            include_independence_warnings: true,
            include_naming_warnings: true,
            include_non_tail_warnings: true,
            include_unused_bindings: true,
            include_verify_run: false,
            verify_run_hostile: false,
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
#[cfg(feature = "runtime")]
pub fn analyze_source(source: &str, options: &AnalyzeOptions) -> AnalysisReport {
    analyze_source_impl(source, options, &[])
}

#[cfg(not(feature = "runtime"))]
pub fn analyze_source(source: &str, options: &AnalyzeOptions) -> AnalysisReport {
    analyze_source_impl(source, options)
}

/// Analyze source while installing explicit process-level bindings for the
/// optional verify execution. Static collectors remain provider-neutral.
#[cfg(feature = "runtime")]
pub fn analyze_source_with_verify_provider_bindings(
    source: &str,
    options: &AnalyzeOptions,
    provider_bindings: &[crate::provider::ProviderBinding],
) -> AnalysisReport {
    analyze_source_impl(source, options, provider_bindings)
}

fn analyze_source_impl(
    source: &str,
    options: &AnalyzeOptions,
    #[cfg(feature = "runtime")] provider_bindings: &[crate::provider::ProviderBinding],
) -> AnalysisReport {
    // The ceiling on verify-case expansion is project policy, so a file of a
    // real project parses under the number that project wrote down. Resolved
    // through the same helper the source loader uses, because two rules about
    // how many cases a file may declare is exactly the disagreement the
    // setting exists to prevent.
    let ceiling = crate::source::project_verify_ceiling_or_default(
        options.module_base_dir.as_deref(),
        options.source_path.as_deref(),
    );
    let items = match parse_source_with_verify_ceiling(source, ceiling) {
        Ok(items) => items,
        Err(e) => {
            return AnalysisReport::with_diagnostics(
                options.file_label.clone(),
                vec![parse_error_diagnostic(&e, source, &options.file_label)],
            );
        }
    };

    let mut transformed = items.clone();
    crate::ir::pipeline::tco(&mut transformed);

    let mode = if let Some(loaded) = options.loaded_modules.as_deref() {
        crate::ir::TypecheckMode::WithLoaded(loaded)
    } else {
        crate::ir::TypecheckMode::Full {
            base_dir: options.module_base_dir.as_deref(),
        }
    };
    // The same gate `pipeline::run` and both verify doors go through —
    // type errors and the shadowing ban (#954) in one channel. Nothing
    // is appended to `items` here, so the ban's scope is the whole
    // program.
    let tc_result = crate::ir::pipeline::typecheck_gate(&items, &mode, &items);

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

    // Stdlib-shadowing: a `depends` entry resolved to an embedded standard
    // module while a same-named project file exists — that file is silently
    // ignored, so the program runs with stdlib semantics regardless of what
    // the project file says. Anchored on the module declaration line (the
    // `depends [...]` list lives in its indented block).
    if !options.stdlib_shadowed.is_empty() {
        let (module_name, module_line) = crate::visibility::module_decl(&items)
            .map(|m| (Some(m.name.clone()), m.line))
            .unwrap_or((None, 1));
        for (dep_name, shadowed_path) in &options.stdlib_shadowed {
            let finding = CheckFinding {
                line: module_line,
                module: module_name.clone(),
                file: None,
                fn_name: None,
                message: crate::source::stdlib_shadow_message(dep_name, shadowed_path),
                extra_spans: vec![],
            };
            diagnostics.push(from_check_finding(
                Severity::Warning,
                &finding,
                source,
                &options.file_label,
            ));
        }
    }

    // Module-level `effects [...]` boundary diagnostics. Underdeclared
    // (a fn uses an effect outside the boundary) is a hard type error,
    // surfaced via `tc_result.errors`. Overdeclared (boundary lists
    // effects no fn uses) is a softer hint — still worth surfacing so
    // the module header documents what the code actually does.
    for w in collect_module_effects_warnings_in(&items, None) {
        diagnostics.push(from_check_finding(
            Severity::Warning,
            &w,
            source,
            &options.file_label,
        ));
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

    if options.include_traversal_warnings {
        for w in collect_traversal_warnings_in(&transformed, None) {
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
        let mode = if options.verify_run_hostile {
            crate::verify_law::expand::ExpansionMode::Hostile
        } else {
            crate::verify_law::expand::ExpansionMode::Declared
        };
        let verify_result = if let Some(loaded) = options.loaded_modules.clone() {
            super::verify_run::try_run_verify_blocks_with_loaded_and_mode_and_bindings(
                runnable_items,
                loaded,
                &options.file_label,
                source,
                mode,
                provider_bindings,
            )
        } else {
            super::verify_run::try_run_verify_blocks_with_mode_and_bindings(
                runnable_items,
                options.module_base_dir.as_deref(),
                &options.file_label,
                source,
                mode,
                provider_bindings,
            )
        };
        match verify_result {
            Ok((verify_diags, verify_summary)) => {
                diagnostics.extend(verify_diags);
                Some(verify_summary)
            }
            Err(error) => {
                if crate::provider::is_provider_setup_error(&error) {
                    diagnostics.push(verify_provider_setup_diagnostic(
                        &options.file_label,
                        &error,
                    ));
                }
                Some(super::model::VerifySummary { blocks: Vec::new() })
            }
        }
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
        // A rejected operator is not a syntax slip — it is a redirection to
        // the named function that replaces it, and tooling (and the reader)
        // should be able to tell the two apart.
        slug: if body.contains("operator does not exist in Aver") {
            "rejected-operator"
        } else {
            "parse-error"
        },
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
        from_hostile: false,
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
    // Every operator Aver rejects names its replacement in the message
    // itself (`lexer::rejected_operator_hint` and the shift arm in
    // `parser::parse_comparison`). The repair adds the shape of the call —
    // and, for the bit-level family, the one fact a reader coming from
    // another language most needs: `Bits` is a namespace, not a type.
    let hint = if body.contains("operator does not exist in Aver") {
        if body.contains("Bits.") {
            Some(
                "Bit-level operations live in the `Bits` namespace: Bits.and / Bits.or / Bits.xor / Bits.not are Int -> Int, and Bits.shiftLeft / Bits.shiftRight / Bits.low return Result<Int, String> (plain Int when the count is a non-negative literal). `Bits` is a namespace, not a type — its arguments and results are ordinary mathematical Int values",
            )
        } else if body.contains("Int.mod") {
            Some(
                "Use Int.mod(a, b) : Result<Int, String>; handle the failure with `match` or `Result.withDefault(Int.mod(a, b), fallback)`. With a nonzero literal divisor, Int.mod(a, k) is total and returns plain Int",
            )
        } else {
            Some(
                "Use the named function, or a nested `match` — Aver has no short-circuit operators because effects are eager",
            )
        }
    } else if body.contains("after '?'") {
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

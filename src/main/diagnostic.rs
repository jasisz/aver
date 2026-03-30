/// Structured diagnostic model and adaptive renderer for Aver.
///
/// One canonical model (`Diagnostic`) — always rich.
/// Adaptive rendering: errors get full treatment, warnings are compact.
///
/// Field order: severity, slug, summary, at, in-fn, intent,
///   contract.*, observed.*, conflict, repair.*, verify.*, source
use colored::Colorize;
use std::fmt::Write;

// ---------------------------------------------------------------------------
// Core types
// ---------------------------------------------------------------------------

/// Severity level.
pub(super) enum Severity {
    Error,
    Warning,
    Fail,
}

/// Source location.
pub(super) struct Span {
    pub file: String,
    pub line: usize,
    pub col: usize,
}

/// A single source line for display.
pub(super) struct SourceLine {
    pub line_num: usize,
    pub text: String,
}

/// Underline annotation beneath a source line.
pub(super) struct Underline {
    pub col: usize,
    pub len: usize,
    pub label: String,
}

/// The canonical diagnostic record.
pub(super) struct Diagnostic {
    pub severity: Severity,
    pub slug: &'static str,
    pub summary: String,
    pub span: Span,
    pub fn_name: Option<String>,
    pub intent: Option<String>,
    pub fields: Vec<(&'static str, String)>,
    pub conflict: Option<String>,
    pub repair_primary: Option<String>,
    pub repair_alternatives: Vec<String>,
    pub repair_example: Option<String>,
    pub source_lines: Vec<SourceLine>,
    pub underline: Option<Underline>,
}

// ---------------------------------------------------------------------------
// Rendering
// ---------------------------------------------------------------------------

impl Diagnostic {
    /// Render for terminal output.
    ///
    /// Errors get full treatment (fields + source).
    /// Warnings get compact treatment (fewer fields, source only if verbose).
    pub fn render(&self, verbose: bool) -> String {
        let mut out = String::new();

        // --- header ---
        let tag = match self.severity {
            Severity::Error => "error",
            Severity::Warning => "warning",
            Severity::Fail => "fail",
        };
        let header_text = format!("{}[{}]: {}", tag, self.slug, self.summary);
        let header = match self.severity {
            Severity::Error | Severity::Fail => header_text.red().bold().to_string(),
            Severity::Warning => header_text.yellow().bold().to_string(),
        };
        let _ = writeln!(out, "{}", header);

        // --- at ---
        let at_label = "at:".blue().to_string();
        let _ = writeln!(
            out,
            "  {} {}:{}:{}",
            at_label, self.span.file, self.span.line, self.span.col
        );

        // --- in-fn ---
        if let Some(ref fn_name) = self.fn_name {
            let key = "in-fn:".blue().to_string();
            let _ = writeln!(out, "  {} {}", key, fn_name);
        }

        // --- intent (verbose only, or error+verbose) ---
        if verbose && let Some(ref intent) = self.intent {
            let key = "intent:".blue().to_string();
            let _ = writeln!(out, "  {} {}", key, intent.dimmed());
        }

        let is_error = matches!(self.severity, Severity::Error | Severity::Fail);

        // --- conflict (errors) ---
        if is_error && let Some(ref conflict) = self.conflict {
            let key = "conflict:".blue().to_string();
            let _ = writeln!(out, "  {} {}", key, conflict);
        }

        // --- fields ---
        let field_limit = if verbose {
            self.fields.len() // all
        } else if is_error {
            4
        } else {
            2
        };
        for (key, value) in self.fields.iter().take(field_limit) {
            let colored_key = format!("{}:", key).blue().to_string();
            let _ = writeln!(out, "  {} {}", colored_key, value);
        }

        // --- repair.primary ---
        if let Some(ref repair) = self.repair_primary {
            let key = "repair:".blue().to_string();
            let _ = writeln!(out, "  {} {}", key, repair.cyan());
        }

        // --- repair.alternatives (verbose only) ---
        if verbose {
            for alt in &self.repair_alternatives {
                let key = "repair.alt:".blue().to_string();
                let _ = writeln!(out, "  {} {}", key, alt.cyan());
            }
        }

        // --- repair.example (verbose only) ---
        if verbose && let Some(ref example) = self.repair_example {
            let key = "repair.example:".blue().to_string();
            let _ = writeln!(out, "  {} {}", key, example.cyan());
        }

        // --- source snippet ---
        // Errors: always show. Warnings: only if verbose.
        let show_source = is_error || verbose;
        if show_source && !self.source_lines.is_empty() {
            let max_num = self
                .source_lines
                .iter()
                .map(|sl| sl.line_num)
                .max()
                .unwrap_or(0);
            let gutter_width = format!("{}", max_num).len();

            // empty gutter
            let gutter_pad: String = " ".repeat(gutter_width);
            let _ = writeln!(out, "  {} {}", gutter_pad, "|".blue());

            for sl in &self.source_lines {
                let num_str = format!("{:>width$}", sl.line_num, width = gutter_width);
                let _ = writeln!(out, "  {} {} {}", num_str.dimmed(), "|".blue(), sl.text);
            }

            // underline
            if let Some(ref ul) = self.underline {
                let pad: String = " ".repeat(ul.col.saturating_sub(1));
                let carets: String = "^".repeat(ul.len.max(1));
                let colored_carets = match self.severity {
                    Severity::Error | Severity::Fail => carets.red().to_string(),
                    Severity::Warning => carets.yellow().to_string(),
                };
                let _ = writeln!(
                    out,
                    "  {} {} {}{}  {}",
                    gutter_pad,
                    "|".blue(),
                    pad,
                    colored_carets,
                    ul.label.dimmed()
                );
            }
        }

        out
    }

    /// Render as JSON for tooling.
    pub fn render_json(&self) -> String {
        let severity_str = match self.severity {
            Severity::Error => "error",
            Severity::Warning => "warning",
            Severity::Fail => "fail",
        };
        let mut parts: Vec<String> = Vec::new();
        parts.push("\"schema_version\":1".to_string());
        parts.push("\"kind\":\"diagnostic\"".to_string());
        parts.push(format!("\"severity\":\"{}\"", severity_str));
        parts.push(format!("\"slug\":\"{}\"", self.slug));
        parts.push(format!("\"summary\":{}", json_escape(&self.summary)));
        parts.push(format!("\"file\":{}", json_escape(&self.span.file)));
        parts.push(format!("\"line\":{}", self.span.line));
        parts.push(format!("\"col\":{}", self.span.col));
        if let Some(ref fn_name) = self.fn_name {
            parts.push(format!("\"fn\":{}", json_escape(fn_name)));
        }
        if let Some(ref intent) = self.intent {
            parts.push(format!("\"intent\":{}", json_escape(intent)));
        }
        if !self.fields.is_empty() {
            let field_strs: Vec<String> = self
                .fields
                .iter()
                .map(|(k, v)| format!("{}:{}", json_escape(k), json_escape(v)))
                .collect();
            parts.push(format!("\"fields\":{{{}}}", field_strs.join(",")));
        }
        if let Some(ref conflict) = self.conflict {
            parts.push(format!("\"conflict\":{}", json_escape(conflict)));
        }
        if let Some(ref repair) = self.repair_primary {
            parts.push(format!("\"repair\":{}", json_escape(repair)));
        }
        format!("{{{}}}", parts.join(","))
    }
}

/// Minimal JSON string escaping (no external deps).
pub(super) fn json_escape(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for ch in s.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

// ---------------------------------------------------------------------------
// Source line extraction
// ---------------------------------------------------------------------------

/// Extract source lines around `line` (1-based) with `context` lines of
/// surrounding context on each side.
fn extract_source_lines(source: &str, line: usize, context: usize) -> Vec<SourceLine> {
    let lines: Vec<&str> = source.lines().collect();
    let start = line.saturating_sub(context + 1); // 0-based
    let end = (line + context).min(lines.len()); // exclusive, 0-based
    (start..end)
        .map(|i| SourceLine {
            line_num: i + 1,
            text: lines[i].to_string(),
        })
        .collect()
}

/// Estimate how many characters to underline starting at `col` (1-based).
fn estimate_span_len(line: &str, col: usize) -> usize {
    let start = col.saturating_sub(1);
    if start >= line.len() {
        return 1;
    }
    let rest = &line[start..];
    let len = rest
        .chars()
        .take_while(|c| !c.is_whitespace() && !matches!(c, '(' | ')' | '[' | ']' | ',' | ':'))
        .count();
    if len == 0 { 1 } else { len }
}

// ---------------------------------------------------------------------------
// Factory functions — convert existing error types to Diagnostic
// ---------------------------------------------------------------------------

/// Build a `Diagnostic` from a `TypeError` (from the typechecker).
pub(super) fn from_type_error(
    msg: &str,
    line: usize,
    col: usize,
    source: &str,
    file: &str,
) -> Diagnostic {
    // Try to extract structured information from the message.
    let (slug, conflict, fields, repair) = classify_type_error(msg);

    let source_line_text = source
        .lines()
        .nth(line.saturating_sub(1))
        .unwrap_or_default();
    let span_len = if col > 0 {
        estimate_span_len(source_line_text, col)
    } else {
        1
    };

    Diagnostic {
        severity: Severity::Error,
        slug,
        summary: msg.to_string(),
        span: Span {
            file: file.to_string(),
            line,
            col,
        },
        fn_name: None,
        intent: None,
        fields,
        conflict,
        repair_primary: repair,
        repair_alternatives: Vec::new(),
        repair_example: None,
        source_lines: extract_source_lines(source, line, 1),
        underline: if col > 0 {
            Some(Underline {
                col,
                len: span_len,
                label: String::new(),
            })
        } else {
            None
        },
    }
}

/// Build a `Diagnostic` for an unused binding warning.
pub(super) fn unused_binding_diagnostic(
    binding: &str,
    fn_name: &str,
    line: usize,
    source: &str,
    file: &str,
) -> Diagnostic {
    Diagnostic {
        severity: Severity::Warning,
        slug: "unused-binding",
        summary: format!("Unused binding '{}' in function '{}'", binding, fn_name),
        span: Span {
            file: file.to_string(),
            line,
            col: 0,
        },
        fn_name: Some(fn_name.to_string()),
        intent: None,
        fields: vec![("binding", binding.to_string())],
        conflict: None,
        repair_primary: Some(format!("Remove the binding or prefix with _: _{}", binding)),
        repair_alternatives: Vec::new(),
        repair_example: None,
        source_lines: extract_source_lines(source, line, 0),
        underline: None,
    }
}

/// Build a `Diagnostic` for a missing verify warning.
pub(super) fn missing_verify_diagnostic(
    fn_name: &str,
    line: usize,
    _intent: Option<&str>,
    source: &str,
    file: &str,
) -> Diagnostic {
    Diagnostic {
        severity: Severity::Warning,
        slug: "missing-verify",
        summary: format!("Function '{}' has no verify block", fn_name),
        span: Span {
            file: file.to_string(),
            line,
            col: 0,
        },
        fn_name: Some(fn_name.to_string()),
        intent: _intent.map(|s| s.to_string()),
        fields: Vec::new(),
        conflict: None,
        repair_primary: Some(format!("Add a verify block: verify {}:", fn_name)),
        repair_alternatives: Vec::new(),
        repair_example: None,
        source_lines: extract_source_lines(source, line, 0),
        underline: None,
    }
}

/// Build a `Diagnostic` for an effect violation. (Stub for V1.)
pub(super) fn effect_violation_diagnostic(
    msg: &str,
    line: usize,
    col: usize,
    source: &str,
    file: &str,
) -> Diagnostic {
    Diagnostic {
        severity: Severity::Error,
        slug: "effect-violation",
        summary: msg.to_string(),
        span: Span {
            file: file.to_string(),
            line,
            col,
        },
        fn_name: None,
        intent: None,
        fields: Vec::new(),
        conflict: Some(msg.to_string()),
        repair_primary: Some("Declare missing effects with ! [Effect] on the function".to_string()),
        repair_alternatives: Vec::new(),
        repair_example: None,
        source_lines: extract_source_lines(source, line, 1),
        underline: if col > 0 {
            let source_line_text = source
                .lines()
                .nth(line.saturating_sub(1))
                .unwrap_or_default();
            Some(Underline {
                col,
                len: estimate_span_len(source_line_text, col),
                label: String::new(),
            })
        } else {
            None
        },
    }
}

/// Build a `Diagnostic` from a `CheckFinding` (intent/verify/coverage warning or error).
pub(super) fn from_check_finding(
    severity: Severity,
    finding: &aver::checker::CheckFinding,
    source: &str,
    file: &str,
) -> Diagnostic {
    let (slug, repair) = classify_finding(&finding.message);
    Diagnostic {
        severity,
        slug,
        summary: finding.message.clone(),
        span: Span {
            file: file.to_string(),
            line: finding.line,
            col: 0,
        },
        fn_name: extract_fn_name_from_finding(&finding.message),
        intent: None,
        fields: Vec::new(),
        conflict: None,
        repair_primary: repair,
        repair_alternatives: Vec::new(),
        repair_example: None,
        source_lines: extract_source_lines(source, finding.line, 0),
        underline: None,
    }
}

fn classify_finding(msg: &str) -> (&'static str, Option<String>) {
    if msg.contains("no verify block") {
        (
            "missing-verify",
            Some("Add a verify block with representative test cases".to_string()),
        )
    } else if msg.contains("no description") {
        (
            "missing-description",
            Some("Add a ? \"description\" line after the function signature".to_string()),
        )
    } else if msg.contains("non-tail recursive") {
        (
            "non-tail-recursion",
            Some("Convert to accumulator style for tail-call optimization".to_string()),
        )
    } else if msg.contains("unused expose") || msg.contains("not used by") {
        ("unused-expose", None)
    } else if msg.contains("verify coverage") || msg.contains("verify case") {
        ("verify-coverage", None)
    } else if msg.contains("verify law") {
        ("verify-law", None)
    } else {
        ("check", None)
    }
}

fn extract_fn_name_from_finding(msg: &str) -> Option<String> {
    // "Function 'foo' has no verify block" → "foo"
    if let Some(start) = msg.find('\'')
        && let Some(end) = msg[start + 1..].find('\'')
    {
        return Some(msg[start + 1..start + 1 + end].to_string());
    }
    None
}

// ---------------------------------------------------------------------------
// Internal: classify a type error message into slug/conflict/fields/repair
// ---------------------------------------------------------------------------

/// (slug, conflict, fields, repair)
type Classification = (
    &'static str,
    Option<String>,
    Vec<(&'static str, String)>,
    Option<String>,
);

fn classify_type_error(msg: &str) -> Classification {
    // Type mismatch pattern: "Type mismatch: expected X, got Y"
    if let Some(rest) = msg.strip_prefix("Type mismatch:") {
        let rest = rest.trim();
        let mut fields = Vec::new();
        let mut expected = String::new();
        let mut got = String::new();
        if let Some((exp, g)) = rest.split_once(", got ") {
            expected = exp
                .strip_prefix("expected ")
                .unwrap_or(exp)
                .trim()
                .to_string();
            got = g.trim().to_string();
            fields.push(("contract.expected", expected.clone()));
            fields.push(("observed.actual", got.clone()));
        }
        let repair = if !expected.is_empty() && !got.is_empty() {
            Some(format!("Change the expression to produce {}", expected))
        } else {
            None
        };
        return ("type-mismatch", Some(msg.to_string()), fields, repair);
    }

    // Unknown identifier
    if msg.starts_with("Unknown identifier") || msg.starts_with("Unknown function") {
        return (
            "unknown-ident",
            None,
            Vec::new(),
            Some("Check the spelling or add the missing import".to_string()),
        );
    }

    // Arity mismatch
    if msg.contains("expects") && msg.contains("argument") {
        return (
            "arity-mismatch",
            Some(msg.to_string()),
            Vec::new(),
            Some("Adjust the number of arguments".to_string()),
        );
    }

    // Effect violation
    if msg.contains("effect") && (msg.contains("not declared") || msg.contains("not allowed")) {
        return (
            "effect-violation",
            Some(msg.to_string()),
            Vec::new(),
            Some("Add the missing effect to the function's ! [...] declaration".to_string()),
        );
    }

    // Fallback
    ("type-error", None, Vec::new(), None)
}

// -- Verify failure diagnostics -----------------------------------------------

#[allow(clippy::too_many_arguments)]
pub(super) fn verify_mismatch_diagnostic(
    file: &str,
    source: &str,
    block_name: &str,
    case_expr: &str,
    expected: &str,
    actual: &str,
    line: usize,
    col: usize,
    is_law: bool,
    law_context: Option<&aver::checker::VerifyLawContext>,
) -> Diagnostic {
    let summary = if is_law {
        "law violated"
    } else {
        "assertion failed"
    };
    let mut fields: Vec<(&'static str, String)> = vec![
        ("block", block_name.to_string()),
        ("case", case_expr.to_string()),
        ("expected", expected.to_string()),
        ("actual", actual.to_string()),
    ];
    if let Some(lctx) = law_context {
        for (name, val) in &lctx.givens {
            fields.push(("given", format!("{} = {}", name, val)));
        }
        fields.push(("law", lctx.law_expr.clone()));
    }
    Diagnostic {
        severity: Severity::Fail,
        slug: "verify-mismatch",
        summary: summary.to_string(),
        span: Span {
            file: file.to_string(),
            line,
            col,
        },
        fn_name: None,
        intent: None,
        fields,
        conflict: None,
        repair_primary: None,
        repair_alternatives: Vec::new(),
        repair_example: None,
        source_lines: extract_source_lines(source, line, 0),
        underline: Some(Underline {
            col,
            len: source
                .lines()
                .nth(line.saturating_sub(1))
                .map(|l| l.trim().len())
                .unwrap_or(1)
                .max(1),
            label: "verify-mismatch".to_string(),
        }),
    }
}

pub(super) fn verify_runtime_error_diagnostic(
    file: &str,
    source: &str,
    block_name: &str,
    case_expr: &str,
    error: &str,
    line: usize,
    col: usize,
) -> Diagnostic {
    let fields: Vec<(&'static str, String)> = vec![
        ("block", block_name.to_string()),
        ("case", case_expr.to_string()),
        ("error", error.to_string()),
    ];
    Diagnostic {
        severity: Severity::Fail,
        slug: "verify-runtime-error",
        summary: "case aborted".to_string(),
        span: Span {
            file: file.to_string(),
            line,
            col,
        },
        fn_name: None,
        intent: None,
        fields,
        conflict: None,
        repair_primary: None,
        repair_alternatives: Vec::new(),
        repair_example: None,
        source_lines: extract_source_lines(source, line, 0),
        underline: Some(Underline {
            col,
            len: source
                .lines()
                .nth(line.saturating_sub(1))
                .map(|l| l.trim().len())
                .unwrap_or(1)
                .max(1),
            label: "verify-runtime-error".to_string(),
        }),
    }
}

pub(super) fn verify_unexpected_err_diagnostic(
    file: &str,
    source: &str,
    block_name: &str,
    case_expr: &str,
    err_repr: &str,
    line: usize,
    col: usize,
) -> Diagnostic {
    let fields: Vec<(&'static str, String)> = vec![
        ("block", block_name.to_string()),
        ("case", case_expr.to_string()),
        ("error", err_repr.to_string()),
    ];
    Diagnostic {
        severity: Severity::Fail,
        slug: "verify-unexpected-err",
        summary: "error propagated from ?".to_string(),
        span: Span {
            file: file.to_string(),
            line,
            col,
        },
        fn_name: None,
        intent: None,
        fields,
        conflict: None,
        repair_primary: None,
        repair_alternatives: Vec::new(),
        repair_example: None,
        source_lines: extract_source_lines(source, line, 0),
        underline: Some(Underline {
            col,
            len: source
                .lines()
                .nth(line.saturating_sub(1))
                .map(|l| l.trim().len())
                .unwrap_or(1)
                .max(1),
            label: "verify-unexpected-err".to_string(),
        }),
    }
}

// -- Replay failure diagnostics -----------------------------------------------

pub(super) fn replay_output_mismatch_diagnostic(
    recording_path: &str,
    expected: &str,
    actual: &str,
    diff_path: Option<&str>,
) -> Diagnostic {
    let mut fields: Vec<(&'static str, String)> = vec![
        ("expected", expected.to_string()),
        ("actual", actual.to_string()),
    ];
    if let Some(dp) = diff_path {
        fields.push(("diff", dp.to_string()));
    }
    Diagnostic {
        severity: Severity::Fail,
        slug: "replay-output-mismatch",
        summary: "recorded output differs".to_string(),
        span: Span {
            file: recording_path.to_string(),
            line: 0,
            col: 0,
        },
        fn_name: None,
        intent: None,
        fields,
        conflict: None,
        repair_primary: None,
        repair_alternatives: Vec::new(),
        repair_example: None,
        source_lines: vec![],
        underline: None,
    }
}

pub(super) fn replay_effect_error_diagnostic(recording_path: &str, error: &str) -> Diagnostic {
    let fields: Vec<(&'static str, String)> = vec![("error", error.to_string())];
    Diagnostic {
        severity: Severity::Fail,
        slug: "replay-error",
        summary: "replay failed".to_string(),
        span: Span {
            file: recording_path.to_string(),
            line: 0,
            col: 0,
        },
        fn_name: None,
        intent: None,
        fields,
        conflict: None,
        repair_primary: None,
        repair_alternatives: Vec::new(),
        repair_example: None,
        source_lines: vec![],
        underline: None,
    }
}

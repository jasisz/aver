use tower_lsp_server::ls_types::{
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, Location, NumberOrString,
    Position, Range, Uri,
};

use aver::ast::{TopLevel, VerifyKind};
use aver::checker::merge_verify_blocks;
use aver::diagnostics::{
    AnalyzeOptions, Diagnostic as CanonicalDiagnostic, Severity as CanonicalSeverity,
    analyze_source,
};
use aver::lexer::{Lexer, LexerError};
use aver::parser::Parser;

/// Run the Aver analysis pipeline and return LSP diagnostics.
///
/// Stages:
///   1. Lex / parse — if either fails, emit a single error diagnostic and stop.
///   2. Canonical analysis via [`aver::diagnostics::analyze_source`] —
///      type errors, intent/coverage/perf/cse/etc. warnings, unused bindings.
///   3. Verify hygiene hints (case/law balance) — LSP-local for now; will
///      migrate to canonical in a later commit when the Why pipeline does.
///   4. Verify block execution — LSP-local; unified with CLI in a later
///      commit alongside the playground Verify panel.
pub fn diagnose(source: &str, base_dir: Option<&str>, uri: Option<&Uri>) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    // A buffer that belongs to a project is parsed under that project's
    // verify-case ceiling, so the editor agrees with `aver check` about how
    // many cases a `given` domain may declare. A scratch buffer with no file
    // and no root keeps the built-in default.
    let source_path = uri.and_then(crate::modules::uri_to_path);
    let ceiling = aver::source::project_verify_ceiling_or_default(base_dir, source_path.as_deref());

    // Phase 1: lex — own step to get precise lexer error locations.
    let mut lexer = Lexer::new(source);
    let tokens = match lexer.tokenize() {
        Ok(tokens) => tokens,
        Err(LexerError::Error { msg, line, col }) => {
            diagnostics.push(point_diagnostic(line, col, DiagnosticSeverity::ERROR, msg));
            return diagnostics;
        }
    };

    // Phase 2: parse — same rationale.
    let mut parser = Parser::new(tokens);
    parser.set_verify_ceiling(ceiling);
    let items = match parser.parse() {
        Ok(items) => items,
        Err(aver::parser::ParseError::Error { msg, line, col }) => {
            diagnostics.push(point_diagnostic(line, col, DiagnosticSeverity::ERROR, msg));
            return diagnostics;
        }
    };

    // Phase 3: canonical analysis. Re-serializes source as it's cheap
    // compared to going through parse again, and analyze_source's shape
    // is the shared contract with CLI / playground.
    let file_label = uri
        .map(|u| u.to_string())
        .unwrap_or_else(|| "<lsp>".to_string());
    let mut opts = AnalyzeOptions::new(file_label);
    if let Some(dir) = base_dir {
        opts.module_base_dir = Some(dir.to_string());
    }
    opts.source_path = source_path;
    // Verify execution is now part of the canonical analysis pipeline.
    opts.include_verify_run = true;
    let report = analyze_source(source, &opts);
    for diag in &report.diagnostics {
        diagnostics.push(to_lsp_diagnostic(diag, uri));
    }

    // Phase 4: verify hygiene hints (LSP-local — not yet in canonical).
    diagnostics.extend(verify_hygiene_diagnostics(&items));

    diagnostics
}

// ─── Canonical → LSP mapping ─────────────────────────────────────────────────

fn to_lsp_diagnostic(d: &CanonicalDiagnostic, uri: Option<&Uri>) -> Diagnostic {
    let severity = match d.severity {
        CanonicalSeverity::Error | CanonicalSeverity::Fail => DiagnosticSeverity::ERROR,
        CanonicalSeverity::Warning => DiagnosticSeverity::WARNING,
        CanonicalSeverity::Hint => DiagnosticSeverity::HINT,
    };

    // Prefer the first region's underline for precise range; fall back to
    // the primary span.
    let line = d.span.line.saturating_sub(1) as u32;
    let (start_char, end_char) = d
        .regions
        .first()
        .and_then(|r| r.underline.as_ref())
        .map(|ul| (ul.col.saturating_sub(1), ul.col + ul.len.max(1) - 1))
        .unwrap_or_else(|| {
            let start = d.span.col.saturating_sub(1);
            (start, start.max(start + 1))
        });

    let related = if d.related.is_empty() {
        None
    } else {
        uri.map(|u| {
            d.related
                .iter()
                .map(|rel| DiagnosticRelatedInformation {
                    location: Location {
                        uri: u.clone(),
                        range: Range {
                            start: Position {
                                line: rel.span.line.saturating_sub(1) as u32,
                                character: rel.span.col.saturating_sub(1) as u32,
                            },
                            end: Position {
                                line: rel.span.line.saturating_sub(1) as u32,
                                character: (rel.span.col + 1) as u32,
                            },
                        },
                    },
                    message: rel.label.clone(),
                })
                .collect()
        })
    };

    // Compose the human message: summary, structured fields (expected,
    // actual, etc.), then repair. Fields carry the bits that make
    // `assertion failed` actionable in an IDE hover.
    let mut message = d.summary.clone();
    for (key, value) in &d.fields {
        message.push_str(&format!("\n  {}: {}", key, value));
    }
    if let Some(repair) = d.repair.primary.as_deref() {
        message.push_str(&format!("\n\nrepair: {}", repair));
    }

    Diagnostic {
        range: Range {
            start: Position {
                line,
                character: start_char as u32,
            },
            end: Position {
                line,
                character: end_char as u32,
            },
        },
        severity: Some(severity),
        code: Some(NumberOrString::String(d.slug.to_string())),
        source: Some("aver".to_string()),
        message,
        related_information: related,
        ..Default::default()
    }
}

fn point_diagnostic(
    line: usize,
    col: usize,
    severity: DiagnosticSeverity,
    message: String,
) -> Diagnostic {
    Diagnostic {
        range: Range {
            start: Position {
                line: line.saturating_sub(1) as u32,
                character: col.saturating_sub(1) as u32,
            },
            end: Position {
                line: line.saturating_sub(1) as u32,
                character: col as u32,
            },
        },
        severity: Some(severity),
        source: Some("aver".to_string()),
        message,
        ..Default::default()
    }
}

// ─── LSP-local: verify hygiene hints ─────────────────────────────────────────
// Moves to canonical in a later commit.

fn verify_hygiene_diagnostics(items: &[TopLevel]) -> Vec<Diagnostic> {
    let verify_by_fn = merge_verify_blocks(items).into_iter().fold(
        std::collections::HashMap::new(),
        |mut acc, vb| {
            acc.entry(vb.fn_name.clone())
                .or_insert_with(Vec::new)
                .push(vb);
            acc
        },
    );

    let mut diagnostics = Vec::new();
    for item in items {
        let TopLevel::FnDef(fd) = item else {
            continue;
        };
        let Some(blocks) = verify_by_fn.get(&fd.name) else {
            continue;
        };

        let case_count: usize = blocks
            .iter()
            .filter(|vb| matches!(vb.kind, VerifyKind::Cases))
            .map(|vb| vb.cases.len())
            .sum();
        let law_count = blocks
            .iter()
            .filter(|vb| matches!(vb.kind, VerifyKind::Law(_)))
            .count();

        if case_count > 0 && law_count == 0 {
            diagnostics.push(hint_at_line(
                fd.line,
                format!(
                    "Function '{}' has verify examples but no law; add one invariant to lock behavior",
                    fd.name
                ),
            ));
        }
        if case_count == 0 && law_count > 0 {
            diagnostics.push(hint_at_line(
                fd.line,
                format!(
                    "Function '{}' has verify laws but no concrete examples; add a few examples for readability",
                    fd.name
                ),
            ));
        }
    }

    diagnostics
}

fn hint_at_line(line: usize, message: String) -> Diagnostic {
    let line = line.saturating_sub(1) as u32;
    Diagnostic {
        range: Range {
            start: Position { line, character: 0 },
            end: Position { line, character: 0 },
        },
        severity: Some(DiagnosticSeverity::HINT),
        source: Some("aver-lsp".to_string()),
        message,
        ..Default::default()
    }
}

#[cfg(test)]
mod tests {
    use tower_lsp_server::ls_types::DiagnosticSeverity;

    use super::diagnose;

    #[test]
    fn diagnostics_show_verify_mismatch() {
        let source = r#"module Demo
    intent = "demo"

fn add(a: Int, b: Int) -> Int
    ? "adds"
    a + b

verify add
    add(1, 2) => 3
    add(2, 3) => 999
"#;
        let diagnostics = diagnose(source, None, None);
        assert!(
            diagnostics.iter().any(|diag| {
                diag.severity == Some(DiagnosticSeverity::ERROR)
                    && matches!(
                        &diag.code,
                        Some(tower_lsp_server::ls_types::NumberOrString::String(s))
                            if s == "verify-mismatch"
                    )
                    && diag.message.contains("999")
                    && diag.message.contains("5")
            }),
            "expected verify-mismatch diagnostic, got: {:?}",
            diagnostics
                .iter()
                .map(|d| (d.code.clone(), d.message.clone()))
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn diagnostics_no_verify_error_when_all_pass() {
        let source = r#"module Demo
    intent = "demo"

fn add(a: Int, b: Int) -> Int
    ? "adds"
    a + b

verify add
    add(1, 2) => 3
    add(2, 3) => 5
"#;
        let diagnostics = diagnose(source, None, None);
        assert!(
            !diagnostics
                .iter()
                .any(|diag| diag.message.contains("verify mismatch")),
            "should not have verify mismatch, got: {:?}",
            diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>()
        );
    }

    #[test]
    fn diagnostics_warn_when_verify_has_cases_but_no_law() {
        let source = r#"module Demo
    intent =
        "demo"

fn add1(x: Int) -> Int
    x + 1

verify add1
    add1(1) => 2
"#;

        let diagnostics = diagnose(source, None, None);
        assert!(diagnostics.iter().any(|diag| {
            diag.severity == Some(DiagnosticSeverity::HINT)
                && diag.message.contains("verify examples but no law")
        }));
    }

    #[test]
    fn diagnostics_warn_when_verify_has_law_but_no_examples() {
        let source = r#"module Demo
    intent =
        "demo"

fn add1(x: Int) -> Int
    x + 1

fn add1Spec(x: Int) -> Int
    x + 1

verify add1 law add1Spec
    given x: Int = 0..1
    add1(x) => add1Spec(x)
"#;

        let diagnostics = diagnose(source, None, None);
        assert!(diagnostics.iter().any(|diag| {
            diag.severity == Some(DiagnosticSeverity::HINT)
                && diag
                    .message
                    .contains("verify laws but no concrete examples")
        }));
    }

    #[test]
    fn diagnostics_carry_slug_as_code() {
        let source = r#"module Demo
fn bad() -> Int
    ? "type mismatch"
    "string"
"#;
        let diagnostics = diagnose(source, None, None);
        let has_type_error_with_code = diagnostics.iter().any(|d| {
            d.severity == Some(DiagnosticSeverity::ERROR)
                && matches!(
                    &d.code,
                    Some(tower_lsp_server::ls_types::NumberOrString::String(slug))
                        if slug.starts_with("type-")
                )
        });
        assert!(
            has_type_error_with_code,
            "expected a type-* diagnostic carrying its slug in the `code` field; got: {:?}",
            diagnostics
                .iter()
                .map(|d| (d.severity, d.code.clone(), d.message.clone()))
                .collect::<Vec<_>>()
        );
    }
}

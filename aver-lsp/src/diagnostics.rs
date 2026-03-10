use tower_lsp_server::ls_types::{Diagnostic, DiagnosticSeverity, Position, Range};

use aver::ast::{TopLevel, VerifyKind};
use aver::checker::{
    check_module_intent_with_sigs, collect_verify_coverage_warnings, merge_verify_blocks,
};
use aver::lexer::{Lexer, LexerError};
use aver::parser::Parser;
use aver::tco;
use aver::types::checker::{TypeError, run_type_check_full};

/// Run the full Aver analysis pipeline on source text and return LSP diagnostics.
pub fn diagnose(source: &str, base_dir: Option<&str>) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    // Phase 1: Lexing
    let mut lexer = Lexer::new(source);
    let tokens = match lexer.tokenize() {
        Ok(tokens) => tokens,
        Err(e) => {
            let (msg, line, col) = match e {
                LexerError::Error { msg, line, col } => (msg, line, col),
            };
            diagnostics.push(Diagnostic {
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
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("aver".to_string()),
                message: msg,
                ..Default::default()
            });
            return diagnostics;
        }
    };

    // Phase 2: Parsing
    let mut parser = Parser::new(tokens);
    let mut items = match parser.parse() {
        Ok(items) => items,
        Err(e) => {
            let (msg, line, col) = match e {
                aver::parser::ParseError::Error { msg, line, col } => (msg, line, col),
            };
            diagnostics.push(Diagnostic {
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
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("aver".to_string()),
                message: msg,
                ..Default::default()
            });
            return diagnostics;
        }
    };

    // Phase 3: TCO transform (required before type checking)
    tco::transform_program(&mut items);

    // Phase 4: Type checking (with module resolution from base_dir)
    let tc_result = run_type_check_full(&items, base_dir);
    for te in &tc_result.errors {
        diagnostics.push(type_error_to_diagnostic(te));
    }

    // Phase 5: Contract-level findings (missing intent, descriptions, verify blocks)
    let findings = check_module_intent_with_sigs(&items, Some(&tc_result.fn_sigs));
    for warning in &findings.warnings {
        diagnostics.push(check_finding_to_diagnostic(
            warning,
            DiagnosticSeverity::WARNING,
        ));
    }
    for error in &findings.errors {
        diagnostics.push(check_finding_to_diagnostic(
            error,
            DiagnosticSeverity::ERROR,
        ));
    }
    for warning in &collect_verify_coverage_warnings(&items) {
        diagnostics.push(check_finding_to_diagnostic(
            warning,
            DiagnosticSeverity::WARNING,
        ));
    }
    diagnostics.extend(verify_hygiene_diagnostics(&items));

    diagnostics
}

/// Convert a checker finding to an LSP diagnostic.
fn check_finding_to_diagnostic(
    finding: &aver::checker::CheckFinding,
    severity: DiagnosticSeverity,
) -> Diagnostic {
    let line = finding.line.saturating_sub(1) as u32;
    Diagnostic {
        range: Range {
            start: Position { line, character: 0 },
            end: Position { line, character: 0 },
        },
        severity: Some(severity),
        source: Some("aver".to_string()),
        message: finding.message.clone(),
        ..Default::default()
    }
}

fn type_error_to_diagnostic(te: &TypeError) -> Diagnostic {
    Diagnostic {
        range: Range {
            start: Position {
                line: te.line.saturating_sub(1) as u32,
                character: te.col as u32,
            },
            end: Position {
                line: te.line.saturating_sub(1) as u32,
                character: (te.col + 1) as u32,
            },
        },
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some("aver".to_string()),
        message: te.message.clone(),
        ..Default::default()
    }
}

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
    fn diagnostics_warn_when_verify_has_cases_but_no_law() {
        let source = r#"module Demo
    intent =
        "demo"

fn add1(x: Int) -> Int
    x + 1

verify add1
    add1(1) => 2
"#;

        let diagnostics = diagnose(source, None);
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

        let diagnostics = diagnose(source, None);
        assert!(diagnostics.iter().any(|diag| {
            diag.severity == Some(DiagnosticSeverity::HINT)
                && diag
                    .message
                    .contains("verify laws but no concrete examples")
        }));
    }
}

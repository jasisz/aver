use tower_lsp_server::ls_types::{Diagnostic, DiagnosticSeverity, Position, Range};

use aver::ast::TopLevel;
use aver::ast::VerifyKind;
use aver::checker::{
    VerifyCaseOutcome, check_module_intent_with_sigs, collect_verify_coverage_warnings,
    merge_verify_blocks, run_verify,
};
use aver::interpreter::Interpreter;
use aver::lexer::{Lexer, LexerError};
use aver::parser::Parser;
use aver::resolver;
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

    // Phase 6: Run verify blocks (only when no type errors)
    if tc_result.errors.is_empty() {
        diagnostics.extend(verify_run_diagnostics(&mut items, base_dir));
    }

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
    let end_char = if te.col == 0 { 200 } else { te.col + 1 };
    Diagnostic {
        range: Range {
            start: Position {
                line: te.line.saturating_sub(1) as u32,
                character: te.col as u32,
            },
            end: Position {
                line: te.line.saturating_sub(1) as u32,
                character: end_char as u32,
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

fn verify_run_diagnostics(items: &mut [TopLevel], base_dir: Option<&str>) -> Vec<Diagnostic> {
    // Resolve variables for interpreter
    resolver::resolve_program(items);

    let mut interp = Interpreter::new();

    // Register type definitions
    for item in items.iter() {
        if let TopLevel::TypeDef(td) = item {
            interp.register_type_def(td);
        }
    }

    // Register all functions
    for item in items.iter() {
        if let TopLevel::FnDef(fd) = item
            && interp.exec_fn_def(fd).is_err()
        {
            return vec![];
        }
    }

    // Load dependency modules if base_dir available
    if let Some(base) = base_dir {
        let mut loading = Vec::new();
        let mut loading_set = std::collections::HashSet::new();
        if let Some(TopLevel::Module(m)) = items.iter().find(|i| matches!(i, TopLevel::Module(_))) {
            for dep_name in &m.depends {
                if let Ok(ns) = interp.load_module(dep_name, base, &mut loading, &mut loading_set) {
                    let _ = interp.define_module_path(dep_name, ns);
                }
            }
        }
    }

    let verify_blocks = merge_verify_blocks(items);
    let mut diagnostics = Vec::new();

    for vb in &verify_blocks {
        let result = run_verify(vb, &mut interp);
        for cr in &result.case_results {
            let (line, col) = cr
                .span
                .as_ref()
                .map(|s| (s.line, s.col))
                .unwrap_or((vb.line, 0));

            let (severity, message) = match &cr.outcome {
                VerifyCaseOutcome::Pass | VerifyCaseOutcome::Skipped => continue,
                VerifyCaseOutcome::Mismatch { expected, actual } => (
                    DiagnosticSeverity::ERROR,
                    format!(
                        "verify mismatch: {} — expected {}, got {}",
                        cr.case_expr, expected, actual
                    ),
                ),
                VerifyCaseOutcome::RuntimeError { error } => (
                    DiagnosticSeverity::ERROR,
                    format!("verify runtime error: {} — {}", cr.case_expr, error),
                ),
                VerifyCaseOutcome::UnexpectedErr { err_repr } => (
                    DiagnosticSeverity::ERROR,
                    format!(
                        "verify error propagation: {} — ? hit {}",
                        cr.case_expr, err_repr
                    ),
                ),
            };

            diagnostics.push(Diagnostic {
                range: Range {
                    start: Position {
                        line: line.saturating_sub(1) as u32,
                        character: col.saturating_sub(1) as u32,
                    },
                    end: Position {
                        line: line.saturating_sub(1) as u32,
                        character: (col + cr.case_expr.len()).min(200) as u32,
                    },
                },
                severity: Some(severity),
                source: Some("aver-verify".to_string()),
                message,
                ..Default::default()
            });
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
        let diagnostics = diagnose(source, None);
        assert!(
            diagnostics.iter().any(|diag| {
                diag.severity == Some(DiagnosticSeverity::ERROR)
                    && diag.message.contains("verify mismatch")
                    && diag.message.contains("999")
                    && diag.message.contains("5")
            }),
            "expected verify mismatch diagnostic, got: {:?}",
            diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>()
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
        let diagnostics = diagnose(source, None);
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

use tower_lsp_server::ls_types::{Diagnostic, DiagnosticSeverity, Position, Range};

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

    // Phase 5: Check warnings (missing intent, descriptions, verify blocks)
    let warnings = aver::checker::check_module_intent(&items);
    for warning in &warnings {
        diagnostics.push(warning_to_diagnostic(warning, &items, source));
    }

    diagnostics
}

/// Convert a check warning to an LSP diagnostic, placing it at the relevant line.
fn warning_to_diagnostic(
    warning: &str,
    items: &[aver::ast::TopLevel],
    _source: &str,
) -> Diagnostic {
    use aver::ast::TopLevel;

    let mut line = 0u32;

    // Try to locate the warning at the relevant function or module
    if let Some(fn_name) = warning
        .strip_prefix("Function '")
        .and_then(|s| s.split('\'').next())
    {
        for item in items {
            if let TopLevel::FnDef(fd) = item {
                if fd.name == fn_name {
                    line = fd.line.saturating_sub(1) as u32;
                    break;
                }
            }
        }
    } else if let Some(mod_name) = warning
        .strip_prefix("Module '")
        .and_then(|s| s.split('\'').next())
    {
        for item in items {
            if let TopLevel::Module(m) = item {
                if m.name == mod_name {
                    // Module block doesn't have a line field; use line 0
                    break;
                }
            }
        }
    }

    Diagnostic {
        range: Range {
            start: Position { line, character: 0 },
            end: Position { line, character: 0 },
        },
        severity: Some(DiagnosticSeverity::WARNING),
        source: Some("aver".to_string()),
        message: warning.to_string(),
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

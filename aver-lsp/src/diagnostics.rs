use tower_lsp_server::ls_types::{
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, Location, Position, Range, Uri,
};

use std::sync::Arc as Rc;

use aver::ast::{Expr, FnBody, FnDef, Spanned, TopLevel};
use aver::ast::VerifyKind;
use aver::checker::{
    VerifyCaseOutcome, VerifyCaseResult, VerifyLawContext, VerifyResult,
    check_module_intent_with_sigs, collect_verify_coverage_warnings, expr_to_str,
    merge_verify_blocks,
};
use aver::lexer::{Lexer, LexerError};
use aver::nan_value::Arena;
use aver::parser::Parser;
use aver::resolver;
use aver::tco;
use aver::types::checker::{TypeError, run_type_check_full};
use aver::value::{Value, aver_repr};
use aver::vm;

/// Run the full Aver analysis pipeline on source text and return LSP diagnostics.
pub fn diagnose(source: &str, base_dir: Option<&str>, uri: Option<&Uri>) -> Vec<Diagnostic> {
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
        diagnostics.push(type_error_to_diagnostic(te, source, uri));
    }

    // Phase 5: Contract-level findings (missing intent, descriptions, verify blocks)
    let findings = check_module_intent_with_sigs(&items, Some(&tc_result.fn_sigs));
    for warning in &findings.warnings {
        diagnostics.push(check_finding_to_diagnostic(
            warning,
            DiagnosticSeverity::WARNING,
            source,
        ));
    }
    for error in &findings.errors {
        diagnostics.push(check_finding_to_diagnostic(
            error,
            DiagnosticSeverity::ERROR,
            source,
        ));
    }
    for warning in &collect_verify_coverage_warnings(&items) {
        diagnostics.push(check_finding_to_diagnostic(
            warning,
            DiagnosticSeverity::WARNING,
            source,
        ));
    }
    diagnostics.extend(verify_hygiene_diagnostics(&items));

    // Phase 6: Run verify blocks (only when no type errors)
    if tc_result.errors.is_empty() {
        diagnostics.extend(verify_run_diagnostics(items, base_dir));
    }

    diagnostics
}

/// Convert a checker finding to an LSP diagnostic.
fn check_finding_to_diagnostic(
    finding: &aver::checker::CheckFinding,
    severity: DiagnosticSeverity,
    source: &str,
) -> Diagnostic {
    let line = finding.line.saturating_sub(1) as u32;
    let source_line = source.lines().nth(line as usize).unwrap_or("");
    let (start_char, end_char) =
        find_precise_span(source_line, &finding.message).unwrap_or_else(|| {
            let indent = source_line.len() - source_line.trim_start().len();
            (indent, indent + source_line.trim().len())
        });
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
        source: Some("aver".to_string()),
        message: finding.message.clone(),
        ..Default::default()
    }
}

/// Find precise (start, end) byte offsets for the first backtick/quote-delimited
/// fragment in `summary` within `source_line`. Returns 0-based offsets.
fn find_precise_span(source_line: &str, summary: &str) -> Option<(usize, usize)> {
    let search_after_arrow = summary.contains("right side") || summary.contains("=>");
    for quote in ['`', '\''] {
        if let Some(start_offset) = summary.find(quote) {
            let start = start_offset + 1;
            if let Some(end_offset) = summary[start..].find(quote) {
                let needle = &summary[start..start + end_offset];
                if !needle.is_empty() {
                    let search_region = if search_after_arrow {
                        source_line.find("=>").map(|p| p + 2).unwrap_or(0)
                    } else {
                        0
                    };
                    if let Some(pos) = source_line[search_region..].find(needle) {
                        let col = search_region + pos;
                        return Some((col, col + needle.len()));
                    }
                }
            }
        }
    }
    None
}

fn type_error_to_diagnostic(te: &TypeError, source: &str, uri: Option<&Uri>) -> Diagnostic {
    // When secondary span is present, narrow primary range to the return type after `->`.
    let source_line = source.lines().nth(te.line.saturating_sub(1)).unwrap_or("");
    let (start_char, end_char) = if te.secondary.is_some() {
        if let Some(arrow_pos) = source_line.find("-> ") {
            let after = &source_line[arrow_pos + 3..];
            let len = after
                .chars()
                .take_while(|c| c.is_alphanumeric() || matches!(c, '<' | '>' | ',' | ' ' | '.'))
                .count();
            let len = after[..len].trim_end().len();
            (arrow_pos + 3, arrow_pos + 3 + len.max(1))
        } else if te.col > 0 {
            (te.col, te.col + 1)
        } else {
            (0, 200)
        }
    } else if te.col > 0 {
        (te.col, te.col + 1)
    } else {
        (0, 200)
    };

    let related = te.secondary.as_ref().and_then(|sec| {
        let u = uri?;
        Some(vec![DiagnosticRelatedInformation {
            location: Location {
                uri: u.clone(),
                range: Range {
                    start: Position {
                        line: sec.line.saturating_sub(1) as u32,
                        character: 0,
                    },
                    end: Position {
                        line: sec.line.saturating_sub(1) as u32,
                        character: 200,
                    },
                },
            },
            message: sec.label.clone(),
        }])
    });

    Diagnostic {
        range: Range {
            start: Position {
                line: te.line.saturating_sub(1) as u32,
                character: start_char as u32,
            },
            end: Position {
                line: te.line.saturating_sub(1) as u32,
                character: end_char as u32,
            },
        },
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some("aver".to_string()),
        message: te.message.clone(),
        related_information: related,
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

fn make_verify_vm_helper(
    name: String,
    line: usize,
    body_expr: Spanned<Expr>,
    wrap_ok: bool,
) -> TopLevel {
    let body_expr = if wrap_ok {
        Spanned {
            node: Expr::Constructor("Result.Ok".to_string(), Some(Box::new(body_expr.clone()))),
            line: body_expr.line,
            col: body_expr.col,
        }
    } else {
        body_expr
    };
    TopLevel::FnDef(FnDef {
        name,
        params: vec![],
        line,
        return_type: "Unit".to_string(),
        effects: vec![],
        desc: None,
        body: Rc::new(FnBody::from_expr(body_expr)),
        resolution: None,
    })
}

fn verify_run_diagnostics(mut items: Vec<TopLevel>, base_dir: Option<&str>) -> Vec<Diagnostic> {
    tco::transform_program(&mut items);

    let verify_blocks = merge_verify_blocks(&items);
    if verify_blocks.is_empty() {
        return vec![];
    }

    // Build verify plans — add __verify_* helper fns to items
    struct CaseFns {
        left: String,
        right: String,
        guard: Option<String>,
    }
    struct Plan {
        block: aver::ast::VerifyBlock,
        cases: Vec<CaseFns>,
    }

    let mut plans = Vec::with_capacity(verify_blocks.len());
    for (block_idx, block) in verify_blocks.iter().enumerate() {
        let mut case_plans = Vec::with_capacity(block.cases.len());
        let sample_guards = match &block.kind {
            VerifyKind::Law(law) => Some(&law.sample_guards),
            VerifyKind::Cases => None,
        };
        for (case_idx, (left_expr, right_expr)) in block.cases.iter().cloned().enumerate() {
            let prefix = format!("__verify_{}_{}_{}", block.fn_name, block_idx, case_idx);
            let left_name = format!("{}_left", prefix);
            let right_name = format!("{}_right", prefix);
            items.push(make_verify_vm_helper(left_name.clone(), block.line, left_expr, true));
            items.push(make_verify_vm_helper(right_name.clone(), block.line, right_expr, true));
            let guard_name = sample_guards
                .and_then(|guards| guards.get(case_idx))
                .cloned()
                .map(|guard_expr| {
                    let name = format!("{}_guard", prefix);
                    items.push(make_verify_vm_helper(name.clone(), block.line, guard_expr, false));
                    name
                });
            case_plans.push(CaseFns { left: left_name, right: right_name, guard: guard_name });
        }
        plans.push(Plan { block: block.clone(), cases: case_plans });
    }

    resolver::resolve_program(&mut items);

    let source_file = "";
    let mut arena = Arena::new();
    vm::register_service_types(&mut arena);
    let (code, globals) =
        match vm::compile_program_with_modules(&items, &mut arena, base_dir, source_file) {
            Ok(v) => v,
            Err(_) => return vec![],
        };
    let mut machine = vm::VM::new(code, globals, arena);
    machine.set_silent_console(true);

    let mut diagnostics = Vec::new();

    for plan in &plans {
        let block = &plan.block;
        let case_total = block.cases.len();
        let is_law = matches!(block.kind, VerifyKind::Law(_));
        let law_context_template = if let VerifyKind::Law(law) = &block.kind {
            Some(format!("{} == {}", expr_to_str(&law.lhs), expr_to_str(&law.rhs)))
        } else {
            None
        };

        for (idx, ((left_expr, right_expr), case_fns)) in
            block.cases.iter().zip(&plan.cases).enumerate()
        {
            let case_str = format!("{} == {}", expr_to_str(left_expr), expr_to_str(right_expr));
            let span = block.case_spans.get(idx).cloned();
            let (line, col) = span
                .as_ref()
                .map(|s| (s.line, s.col))
                .unwrap_or((block.line, 0));

            let law_context = if let VerifyKind::Law(_) = &block.kind {
                let givens: Vec<(String, String)> = block
                    .case_givens
                    .get(idx)
                    .map(|gs| gs.iter().map(|(name, expr)| (name.clone(), expr_to_str(expr))).collect())
                    .unwrap_or_default();
                Some(VerifyLawContext {
                    givens,
                    law_expr: law_context_template.clone().unwrap_or_default(),
                })
            } else {
                None
            };

            // Guard check
            if let Some(guard_name) = &case_fns.guard {
                match machine.run_named_function(guard_name, &[]) {
                    Ok(v) => {
                        let val = v.to_value(&machine.arena);
                        if val == Value::Bool(false) {
                            continue; // skipped
                        }
                    }
                    Err(_) => continue,
                }
            }

            let left_result = machine
                .run_named_function(&case_fns.left, &[])
                .map(|v| v.to_value(&machine.arena));
            let right_result = machine
                .run_named_function(&case_fns.right, &[])
                .map(|v| v.to_value(&machine.arena));

            let outcome = match (left_result, right_result) {
                (Ok(Value::Ok(left_val)), Ok(Value::Ok(right_val))) => {
                    if *left_val == *right_val {
                        continue; // pass
                    }
                    format!(
                        "verify mismatch: {} — expected {}, got {}",
                        case_str,
                        aver_repr(&right_val),
                        aver_repr(&left_val)
                    )
                }
                (Ok(Value::Err(err_val)), _) | (_, Ok(Value::Err(err_val))) => {
                    format!(
                        "verify error propagation: {} — ? hit Result.Err({})",
                        case_str,
                        aver_repr(&err_val)
                    )
                }
                (Err(e), _) | (_, Err(e)) => {
                    format!("verify runtime error: {} — {}", case_str, e)
                }
                _ => {
                    format!("verify runtime error: {} — unexpected shape", case_str)
                }
            };

            diagnostics.push(Diagnostic {
                range: Range {
                    start: Position {
                        line: line.saturating_sub(1) as u32,
                        character: col.saturating_sub(1) as u32,
                    },
                    end: Position {
                        line: line.saturating_sub(1) as u32,
                        character: (col + case_str.len()).min(200) as u32,
                    },
                },
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("aver-verify".to_string()),
                message: outcome,
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
        let diagnostics = diagnose(source, None, None);
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
}

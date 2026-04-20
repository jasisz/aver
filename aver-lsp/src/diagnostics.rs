use tower_lsp_server::ls_types::{
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, Location, NumberOrString,
    Position, Range, Uri,
};

use std::sync::Arc as Rc;

use aver::ast::{Expr, FnBody, FnDef, Spanned, TopLevel, VerifyKind};
use aver::checker::{expr_to_str, merge_verify_blocks};
use aver::diagnostics::{
    AnalyzeOptions, Diagnostic as CanonicalDiagnostic, Severity as CanonicalSeverity,
    analyze_source,
};
use aver::lexer::{Lexer, LexerError};
use aver::nan_value::{Arena, NanValueConvert};
use aver::parser::Parser;
use aver::resolver;
use aver::tco;
use aver::value::{Value, aver_repr};
use aver::vm;

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
    let report = analyze_source(source, &opts);
    for diag in &report.diagnostics {
        diagnostics.push(to_lsp_diagnostic(diag, uri));
    }

    // Phase 4: verify hygiene hints (LSP-local).
    diagnostics.extend(verify_hygiene_diagnostics(&items));

    // Phase 5: verify block execution (LSP-local).
    let tc_had_errors = report
        .diagnostics
        .iter()
        .any(|d| matches!(d.severity, CanonicalSeverity::Error) && d.slug.starts_with("type-"));
    if !tc_had_errors {
        diagnostics.extend(verify_run_diagnostics(items, base_dir));
    }

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

    // Compose the human message: summary first, then repair if present.
    let message = match d.repair.primary.as_deref() {
        Some(repair) => format!("{}\n\nrepair: {}", d.summary, repair),
        None => d.summary.clone(),
    };

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

// ─── LSP-local: verify block execution ───────────────────────────────────────
// Unified with CLI `aver verify` in a later commit.

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
            items.push(make_verify_vm_helper(
                left_name.clone(),
                block.line,
                left_expr,
                true,
            ));
            items.push(make_verify_vm_helper(
                right_name.clone(),
                block.line,
                right_expr,
                true,
            ));
            let guard_name = sample_guards
                .and_then(|guards| guards.get(case_idx))
                .cloned()
                .map(|guard_expr| {
                    let name = format!("{}_guard", prefix);
                    items.push(make_verify_vm_helper(
                        name.clone(),
                        block.line,
                        guard_expr,
                        false,
                    ));
                    name
                });
            case_plans.push(CaseFns {
                left: left_name,
                right: right_name,
                guard: guard_name,
            });
        }
        plans.push(Plan {
            block: block.clone(),
            cases: case_plans,
        });
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

        for (idx, ((left_expr, right_expr), case_fns)) in
            block.cases.iter().zip(&plan.cases).enumerate()
        {
            let case_str = format!("{} == {}", expr_to_str(left_expr), expr_to_str(right_expr));
            let line = block
                .case_spans
                .get(idx)
                .map(|s| s.line)
                .unwrap_or(block.line);
            let col = 0usize;

            if let Some(guard_name) = &case_fns.guard {
                match machine.run_named_function(guard_name, &[]) {
                    Ok(v) => {
                        let val = v.to_value(&machine.arena);
                        if val == Value::Bool(false) {
                            continue;
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
                        continue;
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
                code: Some(NumberOrString::String("verify-mismatch".to_string())),
                source: Some("aver-verify".to_string()),
                message: outcome,
                ..Default::default()
            });
        }
    }

    diagnostics
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

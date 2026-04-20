//! Execute verify blocks via the VM and surface results as canonical
//! [`Diagnostic`]s. Shared by CLI (`aver verify`), LSP, and the playground
//! Verify panel so every consumer sees identical outcomes for identical
//! source.
//!
//! Requires the `runtime` feature (needs the VM). On wasm, the VM must
//! be configured with `IndependenceMode::Sequential` — done automatically
//! by the playground bindgen.

use std::sync::Arc as Rc;

use crate::ast::{Expr, FnBody, FnDef, Spanned, TopLevel, VerifyBlock, VerifyKind};
use crate::checker::{expr_to_str, merge_verify_blocks};
use crate::nan_value::{Arena, NanValueConvert};
use crate::resolver;
use crate::tco;
use crate::value::{Value, aver_repr};
use crate::vm;

use super::factories::{
    verify_mismatch_diagnostic, verify_runtime_error_diagnostic,
    verify_unexpected_err_diagnostic,
};
use super::model::Diagnostic;

/// Run every verify block found in `items` and return a diagnostic per
/// failing case (mismatches, runtime errors, unexpected errors from `?`).
///
/// Passes emit no diagnostic. Guards that evaluate to `false` skip the
/// case silently (same as CLI `aver verify`).
pub fn run_verify_blocks(
    mut items: Vec<TopLevel>,
    base_dir: Option<&str>,
    file_label: &str,
    source: &str,
) -> Vec<Diagnostic> {
    tco::transform_program(&mut items);

    let verify_blocks = merge_verify_blocks(&items);
    if verify_blocks.is_empty() {
        return Vec::new();
    }

    let plans = synthesize_verify_helpers(&mut items, &verify_blocks);

    resolver::resolve_program(&mut items);

    let mut arena = Arena::new();
    vm::register_service_types(&mut arena);
    let (code, globals) =
        match vm::compile_program_with_modules(&items, &mut arena, base_dir, "") {
            Ok(v) => v,
            Err(_) => return Vec::new(),
        };
    let mut machine = vm::VM::new(code, globals, arena);
    machine.set_silent_console(true);

    let mut diagnostics = Vec::new();

    for plan in &plans {
        let block = &plan.block;
        for (idx, ((left_expr, right_expr), case_fns)) in
            block.cases.iter().zip(&plan.cases).enumerate()
        {
            let case_expr = format!(
                "{} == {}",
                expr_to_str(left_expr),
                expr_to_str(right_expr)
            );
            let line = block
                .case_spans
                .get(idx)
                .map(|s| s.line)
                .unwrap_or(block.line);
            let col = block.case_spans.get(idx).map(|s| s.col).unwrap_or(1);

            // Guard: skip case if evaluator returns false.
            if let Some(guard_name) = &case_fns.guard {
                match machine.run_named_function(guard_name, &[]) {
                    Ok(v) => {
                        if v.to_value(&machine.arena) == Value::Bool(false) {
                            continue;
                        }
                    }
                    Err(_) => continue,
                }
            }

            let left = machine
                .run_named_function(&case_fns.left, &[])
                .map(|v| v.to_value(&machine.arena));
            let right = machine
                .run_named_function(&case_fns.right, &[])
                .map(|v| v.to_value(&machine.arena));

            let is_law = matches!(block.kind, VerifyKind::Law(_));

            match (left, right) {
                (Ok(Value::Ok(left_val)), Ok(Value::Ok(right_val))) => {
                    if *left_val == *right_val {
                        continue;
                    }
                    diagnostics.push(verify_mismatch_diagnostic(
                        file_label,
                        source,
                        &block.fn_name,
                        &case_expr,
                        &aver_repr(&right_val),
                        &aver_repr(&left_val),
                        line,
                        col,
                        is_law,
                        None,
                    ));
                }
                (Ok(Value::Err(err_val)), _) | (_, Ok(Value::Err(err_val))) => {
                    diagnostics.push(verify_unexpected_err_diagnostic(
                        file_label,
                        source,
                        &block.fn_name,
                        &case_expr,
                        &aver_repr(&err_val),
                        line,
                        col,
                    ));
                }
                (Err(e), _) | (_, Err(e)) => {
                    diagnostics.push(verify_runtime_error_diagnostic(
                        file_label,
                        source,
                        &block.fn_name,
                        &case_expr,
                        &e.to_string(),
                        line,
                        col,
                    ));
                }
                _ => {
                    diagnostics.push(verify_runtime_error_diagnostic(
                        file_label,
                        source,
                        &block.fn_name,
                        &case_expr,
                        "unexpected shape (left or right did not return Result)",
                        line,
                        col,
                    ));
                }
            }
        }
    }

    diagnostics
}

// ─── Helper synthesis ─────────────────────────────────────────────────────────

struct CaseFns {
    left: String,
    right: String,
    guard: Option<String>,
}

struct Plan {
    block: VerifyBlock,
    cases: Vec<CaseFns>,
}

fn synthesize_verify_helpers(
    items: &mut Vec<TopLevel>,
    verify_blocks: &[VerifyBlock],
) -> Vec<Plan> {
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
            items.push(make_vm_helper(left_name.clone(), block.line, left_expr, true));
            items.push(make_vm_helper(
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
                    items.push(make_vm_helper(name.clone(), block.line, guard_expr, false));
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
    plans
}

fn make_vm_helper(
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

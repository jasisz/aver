use std::io::{self, BufRead, Write};
use std::sync::Arc as Rc;

use colored::Colorize;

use aver::ast::{Expr, FnBody, FnDef, Spanned, Stmt, TopLevel, TypeDef};
use aver::checker::{expr_to_str, merge_verify_blocks};
use aver::nan_value::{Arena, NanValueConvert};
use aver::source::parse_source;
use aver::types::checker::run_type_check_with_base;
use aver::value::{Value, aver_repr};
use aver::vm;

use crate::shared::print_type_errors;

pub(super) fn is_incomplete(source: &str) -> bool {
    let lines: Vec<&str> = source.lines().collect();
    if lines.is_empty() {
        return false;
    }

    if let Some(last) = lines.iter().rev().find(|l| !l.trim().is_empty())
        && (last.starts_with("    ") || last.starts_with('\t'))
    {
        return true;
    }

    let first = lines[0].trim();
    let needs_body = first.starts_with("fn ")
        || first.starts_with("type ")
        || first.starts_with("record ")
        || first.starts_with("verify ")
        || first.starts_with("module ");

    needs_body && lines.len() == 1
}

pub(super) fn repl_help() {
    println!("Commands:");
    println!("  :help / :h   Show this help");
    println!("  :quit / :q   Exit the REPL");
    println!("  :clear / :c  Clear all definitions and restart");
    println!("  :env         Show all defined names");
    println!();
    println!("Multi-line input: fn/type/record/verify/module start a block.");
    println!("Press Enter on an empty line to finish a block.");
}

pub(super) fn repl_env(accumulated: &[TopLevel]) {
    let mut found = false;
    for item in accumulated {
        match item {
            TopLevel::FnDef(fd) => {
                println!("  fn {}", fd.name);
                found = true;
            }
            TopLevel::TypeDef(td) => {
                let name = match td {
                    TypeDef::Sum { name, .. } | TypeDef::Product { name, .. } => name,
                };
                println!("  type {}", name);
                found = true;
            }
            TopLevel::Stmt(Stmt::Binding(name, _, _)) => {
                println!("  {}", name);
                found = true;
            }
            _ => {}
        }
    }
    if !found {
        println!("  (empty)");
    }
}

// ---------------------------------------------------------------------------
// VM verify plans — same approach as commands.rs::build_verify_vm_plans
// ---------------------------------------------------------------------------

struct VerifyCaseFns {
    left: String,
    right: String,
}

struct VerifyPlan {
    block: aver::ast::VerifyBlock,
    cases: Vec<VerifyCaseFns>,
}

fn make_verify_helper(
    name: String,
    line: usize,
    body_expr: Spanned<Expr>,
    effects: Vec<Spanned<String>>,
) -> TopLevel {
    TopLevel::FnDef(FnDef {
        name,
        params: vec![],
        line,
        return_type: String::new(),
        effects,
        desc: None,
        body: Rc::new(FnBody::from_expr(Spanned {
            node: Expr::Constructor("Result.Ok".to_string(), Some(Box::new(body_expr))),
            line,
        })),
        resolution: None,
    })
}

/// Inject __verify_* helper functions into `program` and return plans for execution.
fn build_verify_plans(
    program: &mut Vec<TopLevel>,
    verify_blocks: &[aver::ast::VerifyBlock],
    effects: &[Spanned<String>],
) -> Vec<VerifyPlan> {
    let mut plans = Vec::with_capacity(verify_blocks.len());

    for (block_idx, block) in verify_blocks.iter().enumerate() {
        let mut case_plans = Vec::with_capacity(block.cases.len());
        for (case_idx, (left_expr, right_expr)) in block.cases.iter().cloned().enumerate() {
            let prefix = format!("__verify_{}_{}_{}", block.fn_name, block_idx, case_idx);
            let left_name = format!("{}_left", prefix);
            let right_name = format!("{}_right", prefix);
            program.push(make_verify_helper(
                left_name.clone(),
                block.line,
                left_expr,
                effects.to_vec(),
            ));
            program.push(make_verify_helper(
                right_name.clone(),
                block.line,
                right_expr,
                effects.to_vec(),
            ));
            case_plans.push(VerifyCaseFns {
                left: left_name,
                right: right_name,
            });
        }
        plans.push(VerifyPlan {
            block: block.clone(),
            cases: case_plans,
        });
    }

    plans
}

fn run_verify_plans(machine: &mut vm::VM, plans: &[VerifyPlan]) {
    for plan in plans {
        let block = &plan.block;
        let mut passed = 0usize;
        let mut failed = 0usize;
        let case_total = block.cases.len();

        for ((left_expr, right_expr), case_fns) in block.cases.iter().zip(&plan.cases) {
            let case_str = format!("{} == {}", expr_to_str(left_expr), expr_to_str(right_expr));

            let left_result = machine
                .run_named_function(&case_fns.left, &[])
                .map(|v| v.to_value(&machine.arena));
            let right_result = machine
                .run_named_function(&case_fns.right, &[])
                .map(|v| v.to_value(&machine.arena));

            match (left_result, right_result) {
                (Ok(Value::Ok(l)), Ok(Value::Ok(r))) if *l == *r => {
                    passed += 1;
                }
                (Ok(Value::Ok(l)), Ok(Value::Ok(r))) => {
                    failed += 1;
                    eprintln!(
                        "  {} {} — expected {}, got {}",
                        "✗".red(),
                        case_str,
                        aver_repr(&r),
                        aver_repr(&l)
                    );
                }
                (Ok(Value::Err(e)), _) | (_, Ok(Value::Err(e))) => {
                    failed += 1;
                    eprintln!(
                        "  {} {} — ? hit Result.Err({})",
                        "✗".red(),
                        case_str,
                        aver_repr(&e)
                    );
                }
                (Err(e), _) | (_, Err(e)) => {
                    failed += 1;
                    eprintln!("  {} {} — {}", "✗".red(), case_str, e);
                }
                _ => {
                    failed += 1;
                    eprintln!("  {} {} — unexpected result shape", "✗".red(), case_str);
                }
            }
        }

        if failed == 0 {
            println!(
                "  {} {}      {}/{}",
                "✓".green(),
                block.fn_name,
                passed,
                case_total
            );
        }
    }
}

// ---------------------------------------------------------------------------
// Core REPL execution
// ---------------------------------------------------------------------------

/// Compile accumulated + new_items together, execute top-level, then call
/// getter functions for display.  All new items see each other.
fn repl_execute(accumulated: &[TopLevel], new_items: &[TopLevel]) -> Result<(), String> {
    // Build program = accumulated + new items.
    // Bindings stay as top-level statements (executed by run_top_level).
    // Bare expressions become wrapper functions (avoids double evaluation).
    let mut program: Vec<TopLevel> = accumulated.to_vec();

    // Grant all known service effects to REPL wrappers.  The type checker
    // already validated effect usage on the original expressions before we
    // get here, so this cannot open a hole — it only prevents the VM from
    // rejecting a wrapper whose body the type checker already approved.
    let all_effects: Vec<Spanned<String>> = aver::services::all_effect_names()
        .into_iter()
        .map(|s| Spanned {
            node: s.to_string(),
            line: 0,
        })
        .collect();

    enum Action {
        Defined(String),
        DefinedType(String),
        ShowBinding { getter: String, name: String },
        ShowExpr { getter: String },
    }
    let mut actions: Vec<Action> = Vec::new();

    for (idx, item) in new_items.iter().enumerate() {
        match item {
            TopLevel::FnDef(fd) => {
                program.push(item.clone());
                actions.push(Action::Defined(fd.name.clone()));
            }
            TopLevel::TypeDef(td) => {
                program.push(item.clone());
                let name = match td {
                    TypeDef::Sum { name, .. } | TypeDef::Product { name, .. } => name.clone(),
                };
                actions.push(Action::DefinedType(name));
            }
            TopLevel::Stmt(Stmt::Binding(name, _, _)) => {
                program.push(item.clone());
                let getter = format!("__repl_get_{}", idx);
                let getter_items = parse_source(&format!("fn {}()\n    {}", getter, name))
                    .map_err(|e| e.to_string())?;
                program.extend(getter_items);
                actions.push(Action::ShowBinding {
                    getter,
                    name: name.clone(),
                });
            }
            TopLevel::Stmt(Stmt::Expr(expr)) => {
                let getter = format!("__repl_eval_{}", idx);
                program.push(TopLevel::FnDef(FnDef {
                    name: getter.clone(),
                    params: vec![],
                    line: expr.line,
                    return_type: String::new(),
                    effects: all_effects.clone(),
                    desc: None,
                    body: Rc::new(FnBody::from_expr(expr.clone())),
                    resolution: None,
                }));
                actions.push(Action::ShowExpr { getter });
            }
            TopLevel::Verify(_) => {
                program.push(item.clone());
            }
            _ => {
                program.push(item.clone());
            }
        }
    }

    // Build verify plans BEFORE compilation — injects __verify_* helper fns.
    let verify_blocks = merge_verify_blocks(&program);
    let verify_plans = build_verify_plans(&mut program, &verify_blocks, &all_effects);

    // Compile and run once. REPL only needs the resolver — TCO + traversal
    // would be wasted on the throwaway snippet AST.
    aver::ir::pipeline::resolve(&mut program);
    let mut arena = Arena::new();
    vm::register_service_types(&mut arena);
    let (code, globals) = vm::compile_program_with_modules(&program, &mut arena, None, "<repl>")
        .map_err(|e| format!("VM compile error: {}", e))?;
    let mut machine = vm::VM::new(code, globals, arena);
    machine.run_top_level().map_err(|e| e.to_string())?;

    // Display results — use machine.arena for all NanValue rendering.
    for action in &actions {
        match action {
            Action::Defined(name) => {
                println!("{}", format!("defined: {}", name).cyan());
            }
            Action::DefinedType(name) => {
                println!("{}", format!("defined type: {}", name).cyan());
            }
            Action::ShowBinding { getter, name } => {
                let nv = machine
                    .run_named_function(getter, &[])
                    .map_err(|e| e.to_string())?;
                println!("{} = {}", name, nv.repr(&machine.arena));
            }
            Action::ShowExpr { getter } => {
                let nv = machine
                    .run_named_function(getter, &[])
                    .map_err(|e| e.to_string())?;
                if let Some(display) = nv.display(&machine.arena) {
                    println!("{}", display);
                }
            }
        }
    }

    // Execute verify plans.
    if !verify_plans.is_empty() {
        run_verify_plans(&mut machine, &verify_plans);
    }

    Ok(())
}

pub(super) fn cmd_repl() {
    let mut accumulated: Vec<TopLevel> = Vec::new();
    let mut buffer: Vec<String> = Vec::new();

    println!("Aver REPL — :help for commands, :quit to exit");

    let stdin = io::stdin();

    loop {
        let prompt = if buffer.is_empty() {
            "aver> "
        } else {
            "...   "
        };
        print!("{}", prompt);
        io::stdout().flush().ok();

        let mut line = String::new();
        match stdin.lock().read_line(&mut line) {
            Ok(0) => {
                println!();
                break;
            }
            Ok(_) => {}
            Err(_) => break,
        }
        let line = line
            .trim_end_matches('\n')
            .trim_end_matches('\r')
            .to_string();

        if buffer.is_empty() && line.trim().starts_with(':') {
            match line.trim() {
                ":quit" | ":q" => {
                    println!("Bye.");
                    break;
                }
                ":help" | ":h" => {
                    repl_help();
                    continue;
                }
                ":clear" | ":c" => {
                    accumulated.clear();
                    println!("Cleared.");
                    continue;
                }
                ":env" => {
                    repl_env(&accumulated);
                    continue;
                }
                cmd => {
                    println!("Unknown command: {}. Type :help.", cmd);
                    continue;
                }
            }
        }

        buffer.push(line.clone());
        let source = buffer.join("\n");

        if is_incomplete(&source) && !line.trim().is_empty() {
            continue;
        }

        if source.trim().is_empty() {
            buffer.clear();
            continue;
        }

        let new_items = match parse_source(&source) {
            Ok(items) => items,
            Err(e) => {
                eprintln!("{} {}", "Error:".red(), e);
                buffer.clear();
                continue;
            }
        };

        if new_items.is_empty() {
            buffer.clear();
            continue;
        }

        // Typecheck accumulated + new items together.
        let all: Vec<TopLevel> = accumulated
            .iter()
            .chain(new_items.iter())
            .cloned()
            .collect();
        let type_errors = run_type_check_with_base(&all, None);
        if !type_errors.is_empty() {
            print_type_errors(&type_errors);
            buffer.clear();
            continue;
        }

        match repl_execute(&accumulated, &new_items) {
            Ok(()) => {
                accumulated.extend(new_items);
            }
            Err(e) => {
                eprintln!("{} {}", "Error:".red(), e);
            }
        }

        buffer.clear();
    }
}

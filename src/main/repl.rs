use std::io::{self, BufRead, Write};
use std::sync::Arc as Rc;

use colored::Colorize;

use aver::ast::{Expr, FnBody, FnDef, Spanned, Stmt, TopLevel, TypeDef};
use aver::checker::{expr_to_str, merge_verify_blocks};
use aver::nan_value::{Arena, NanValue, NanValueConvert};
use aver::resolver;
use aver::source::parse_source;
use aver::types::checker::run_type_check_with_base;
use aver::value::Value;
use aver::vm;

use crate::shared::print_type_errors;

pub(super) fn is_incomplete(source: &str) -> bool {
    let lines: Vec<&str> = source.lines().collect();
    if lines.is_empty() {
        return false;
    }

    // Inside a block: last non-empty line is indented
    if let Some(last) = lines.iter().rev().find(|l| !l.trim().is_empty())
        && (last.starts_with("    ") || last.starts_with('\t'))
    {
        return true;
    }

    // Block header without a body (only 1 line so far)
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

/// Build a VM from accumulated items and run it. Returns the arena for display.
/// Compile accumulated items + a wrapper that evaluates `expr` and calls it via VM.
fn repl_eval_expr(
    accumulated: &[TopLevel],
    expr_item: &TopLevel,
) -> Result<(Value, Arena), String> {
    // Wrap the expression in a __repl_eval function so the VM can call it
    let expr = match expr_item {
        TopLevel::Stmt(Stmt::Expr(e)) => e.clone(),
        TopLevel::Stmt(Stmt::Binding(name, ty, e)) => {
            // For bindings, we need to evaluate the expression and return it
            return repl_eval_binding(accumulated, name, ty.as_deref(), e);
        }
        _ => return Err("not an expression".to_string()),
    };

    let wrapper = TopLevel::FnDef(FnDef {
        name: "__repl_eval".to_string(),
        params: vec![],
        line: 0,
        return_type: "Unit".to_string(),
        effects: vec![],
        desc: None,
        body: Rc::new(FnBody::from_expr(expr)),
        resolution: None,
    });

    let mut all: Vec<TopLevel> = accumulated.to_vec();
    all.push(wrapper);
    resolver::resolve_program(&mut all);

    let mut arena = Arena::new();
    vm::register_service_types(&mut arena);
    let (code, globals) =
        vm::compile_program_with_modules(&all, &mut arena, None, "<repl>")
            .map_err(|e| format!("VM compile error: {}", e))?;
    let mut machine = vm::VM::new(code, globals, arena);
    machine.run_top_level().map_err(|e| format!("{}", e))?;
    let result = machine
        .run_named_function("__repl_eval", &[])
        .map_err(|e| format!("{}", e))?;
    let val = result.to_value(&machine.arena);
    let arena = machine.arena;
    Ok((val, arena))
}

fn repl_eval_binding(
    accumulated: &[TopLevel],
    _name: &str,
    _ty: Option<&str>,
    expr: &Spanned<Expr>,
) -> Result<(aver::value::Value, Arena), String> {
    let wrapper = TopLevel::FnDef(FnDef {
        name: "__repl_eval".to_string(),
        params: vec![],
        line: 0,
        return_type: "Unit".to_string(),
        effects: vec![],
        desc: None,
        body: Rc::new(FnBody::from_expr(expr.clone())),
        resolution: None,
    });

    let mut all: Vec<TopLevel> = accumulated.to_vec();
    all.push(wrapper);
    resolver::resolve_program(&mut all);

    let mut arena = Arena::new();
    vm::register_service_types(&mut arena);
    let (code, globals) =
        vm::compile_program_with_modules(&all, &mut arena, None, "<repl>")
            .map_err(|e| format!("VM compile error: {}", e))?;
    let mut machine = vm::VM::new(code, globals, arena);
    machine.run_top_level().map_err(|e| format!("{}", e))?;
    let result = machine
        .run_named_function("__repl_eval", &[])
        .map_err(|e| format!("{}", e))?;
    let val = result.to_value(&machine.arena);
    let arena = machine.arena;
    Ok((val, arena))
}

fn repl_run_verify(accumulated: &[TopLevel]) {
    let verify_blocks = merge_verify_blocks(accumulated);
    if verify_blocks.is_empty() {
        return;
    }

    // Build VM verify plans
    let mut items: Vec<TopLevel> = accumulated.to_vec();
    struct CaseFns {
        left: String,
        right: String,
    }
    struct Plan {
        block: aver::ast::VerifyBlock,
        cases: Vec<CaseFns>,
    }

    let mut plans = Vec::new();
    for (block_idx, block) in verify_blocks.iter().enumerate() {
        let mut case_plans = Vec::new();
        for (case_idx, (left_expr, right_expr)) in block.cases.iter().cloned().enumerate() {
            let prefix = format!("__verify_{}_{}_{}", block.fn_name, block_idx, case_idx);
            let left_name = format!("{}_left", prefix);
            let right_name = format!("{}_right", prefix);
            items.push(TopLevel::FnDef(FnDef {
                name: left_name.clone(),
                params: vec![],
                line: block.line,
                return_type: "Unit".to_string(),
                effects: vec![],
                desc: None,
                body: Rc::new(FnBody::from_expr(Spanned {
                    node: Expr::Constructor(
                        "Result.Ok".to_string(),
                        Some(Box::new(left_expr)),
                    ),
                    line: block.line,
                })),
                resolution: None,
            }));
            items.push(TopLevel::FnDef(FnDef {
                name: right_name.clone(),
                params: vec![],
                line: block.line,
                return_type: "Unit".to_string(),
                effects: vec![],
                desc: None,
                body: Rc::new(FnBody::from_expr(Spanned {
                    node: Expr::Constructor(
                        "Result.Ok".to_string(),
                        Some(Box::new(right_expr)),
                    ),
                    line: block.line,
                })),
                resolution: None,
            }));
            case_plans.push(CaseFns {
                left: left_name,
                right: right_name,
            });
        }
        plans.push(Plan {
            block: block.clone(),
            cases: case_plans,
        });
    }

    resolver::resolve_program(&mut items);
    let mut arena = Arena::new();
    vm::register_service_types(&mut arena);
    let (code, globals) = match vm::compile_program_with_modules(&items, &mut arena, None, "<repl>")
    {
        Ok(v) => v,
        Err(e) => {
            eprintln!("{} {}", "Error:".red(), e);
            return;
        }
    };
    let mut machine = vm::VM::new(code, globals, arena);

    for plan in &plans {
        let block = &plan.block;
        let mut passed = 0usize;
        let mut failed = 0usize;

        for ((left_expr, right_expr), case_fns) in
            block.cases.iter().zip(&plan.cases)
        {
            let case_str = format!("{} == {}", expr_to_str(left_expr), expr_to_str(right_expr));

            let left_result = machine
                .run_named_function(&case_fns.left, &[])
                .map(|v| v.to_value(&machine.arena));
            let right_result = machine
                .run_named_function(&case_fns.right, &[])
                .map(|v| v.to_value(&machine.arena));

            match (left_result, right_result) {
                (Ok(aver::value::Value::Ok(l)), Ok(aver::value::Value::Ok(r))) if *l == *r => {
                    passed += 1;
                }
                (Ok(aver::value::Value::Ok(l)), Ok(aver::value::Value::Ok(r))) => {
                    failed += 1;
                    eprintln!(
                        "  {} {} — expected {}, got {}",
                        "✗".red(),
                        case_str,
                        aver::value::aver_repr(&r),
                        aver::value::aver_repr(&l)
                    );
                }
                (Err(e), _) | (_, Err(e)) => {
                    failed += 1;
                    eprintln!("  {} {} — {}", "✗".red(), case_str, e);
                }
                _ => {
                    failed += 1;
                    eprintln!("  {} {} — unexpected error", "✗".red(), case_str);
                }
            }
        }

        if failed == 0 {
            println!(
                "  {} {}  {}/{}",
                "✓".green(),
                block.fn_name,
                passed,
                passed + failed
            );
        }
    }
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
            } // EOF (Ctrl+D)
            Ok(_) => {}
            Err(_) => break,
        }
        let line = line
            .trim_end_matches('\n')
            .trim_end_matches('\r')
            .to_string();

        // REPL commands — only when buffer is empty
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

        // Incomplete input and not an empty line → prompt for more
        if is_incomplete(&source) && !line.trim().is_empty() {
            continue;
        }

        // Empty input → ignore
        if source.trim().is_empty() {
            buffer.clear();
            continue;
        }

        // Parse
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

        // Typecheck (accumulated + new items)
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

        // Execute new items via VM
        let mut ok = true;
        for item in &new_items {
            match item {
                TopLevel::FnDef(fd) => {
                    println!("{}", format!("defined: {}", fd.name).cyan());
                }
                TopLevel::TypeDef(td) => {
                    let name = match td {
                        TypeDef::Sum { name, .. } | TypeDef::Product { name, .. } => name,
                    };
                    println!("{}", format!("defined type: {}", name).cyan());
                }
                TopLevel::Stmt(s) => {
                    match repl_eval_expr(&accumulated, item) {
                        Ok((val, arena)) => match s {
                            Stmt::Binding(name, _, _) => {
                                let nv = NanValue::from_value(&val, &mut Arena::new());
                                println!("{} = {}", name, nv.repr(&arena));
                            }
                            Stmt::Expr(_) => {
                                let nv = NanValue::from_value(&val, &mut Arena::new());
                                if let Some(display) = nv.display(&arena) {
                                    println!("{}", display);
                                }
                            }
                        },
                        Err(e) => {
                            eprintln!("{} {}", "Error:".red(), e);
                            ok = false;
                            break;
                        }
                    }
                }
                TopLevel::Verify(_) => {
                    // Run verify blocks with all accumulated items + new
                    let tentative: Vec<TopLevel> = accumulated
                        .iter()
                        .chain(new_items.iter())
                        .cloned()
                        .collect();
                    repl_run_verify(&tentative);
                }
                _ => {}
            }
        }

        if ok {
            accumulated.extend(new_items);
        }
        buffer.clear();
    }
}

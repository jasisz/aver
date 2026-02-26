use std::collections::HashMap;
use std::io::{self, BufRead, Write};
use std::rc::Rc;

use colored::Colorize;

use aver::ast::{Stmt, TopLevel, TypeDef};
use aver::checker::run_verify;
use aver::interpreter::{aver_display, aver_repr, EnvFrame, Interpreter, Value};
use aver::source::parse_source;
use aver::types::checker::run_type_check_with_base;

use crate::shared::print_type_errors;

pub(super) fn is_incomplete(source: &str) -> bool {
    let lines: Vec<&str> = source.lines().collect();
    if lines.is_empty() {
        return false;
    }

    // Inside a block: last non-empty line is indented
    if let Some(last) = lines.iter().rev().find(|l| !l.trim().is_empty()) {
        if last.starts_with("    ") || last.starts_with('\t') {
            return true;
        }
    }

    // Block header without a body (only 1 line so far)
    let first = lines[0].trim();
    let needs_body = (first.starts_with("fn ") && !first.contains(" = "))
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

pub(super) fn repl_env(interp: &Interpreter) {
    let builtins = [
        "print",
        "str",
        "int",
        "abs",
        "len",
        "map",
        "filter",
        "fold",
        "get",
        "push",
        "head",
        "tail",
        "Ok",
        "Err",
        "Some",
        "None",
        "Console",
        "Http",
        "Disk",
        "Tcp",
        "HttpServer",
    ];
    let mut found = false;
    for frame in &interp.env {
        let scope: Option<&HashMap<String, Rc<Value>>> = match frame {
            EnvFrame::Owned(scope) => Some(scope),
            EnvFrame::Shared(scope) => Some(scope.as_ref()),
            EnvFrame::Slots(_) => None,
        };
        let Some(scope) = scope else { continue };
        for (name, val) in scope {
            if builtins.contains(&name.as_str()) {
                continue;
            }
            if name.starts_with("__") {
                continue;
            }
            println!("  {} = {}", name, aver_repr(val.as_ref()));
            found = true;
        }
    }
    if !found {
        println!("  (empty)");
    }
}

pub(super) fn cmd_repl() {
    let mut interp = Interpreter::new();
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
                    interp = Interpreter::new();
                    println!("Cleared.");
                    continue;
                }
                ":env" => {
                    repl_env(&interp);
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

        // Execute new items
        let mut ok = true;
        for item in &new_items {
            match item {
                TopLevel::FnDef(fd) => match interp.exec_fn_def(fd) {
                    Ok(_) => println!("{}", format!("defined: {}", fd.name).cyan()),
                    Err(e) => {
                        eprintln!("{} {}", "Error:".red(), e);
                        ok = false;
                        break;
                    }
                },
                TopLevel::TypeDef(td) => {
                    interp.register_type_def(td);
                    let name = match td {
                        TypeDef::Sum { name, .. } | TypeDef::Product { name, .. } => name,
                    };
                    println!("{}", format!("defined type: {}", name).cyan());
                }
                TopLevel::EffectSet { name, effects } => {
                    interp.register_effect_set(name.clone(), effects.clone());
                    println!("{}", format!("defined effects: {}", name).cyan());
                }
                TopLevel::Stmt(s) => match interp.exec_stmt(s) {
                    Ok(val) => match s {
                        Stmt::Binding(name, _, _) => {
                            if let Ok(v) = interp.lookup(name) {
                                println!("{} = {}", name, aver_repr(&v));
                            }
                        }
                        Stmt::Expr(_) => {
                            if let Some(display) = aver_display(&val) {
                                println!("{}", display);
                            }
                        }
                    },
                    Err(e) => {
                        eprintln!("{} {}", "Error:".red(), e);
                        ok = false;
                        break;
                    }
                },
                TopLevel::Verify(vb) => {
                    let result = run_verify(vb, &mut interp);
                    if result.failed > 0 {
                        ok = false;
                    }
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

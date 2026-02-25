use std::fs;
use std::io::{self, BufRead, Write};
use std::path::Path;
use std::process;

use clap::{Parser as ClapParser, Subcommand};
use colored::Colorize;

use aver::ast::{DecisionBlock, FnDef, Stmt, TopLevel, TypeDef, VerifyBlock};
use aver::checker::{check_module_intent, expr_to_str, index_decisions, run_verify};
use aver::interpreter::{aver_display, aver_repr, Interpreter};
use aver::source::{find_module_file, parse_source};
use aver::typechecker::run_type_check_with_base;

#[derive(ClapParser)]
#[command(name = "aver", about = "The Aver language interpreter")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Run an Aver file
    Run {
        file: String,
        /// Also run verify blocks after execution
        #[arg(long)]
        verify: bool,
    },
    /// Static analysis (intent presence, module size)
    Check {
        file: String,
        /// Treat all warnings as errors (exit 1 if any warning)
        #[arg(long)]
        strict: bool,
    },
    /// Run all verify blocks
    Verify { file: String },
    /// List all decision blocks
    Decisions { file: String },
    /// Interactive REPL
    Repl,
    /// Export project context for LLM consumption
    Context {
        file: String,
        /// Write output to file instead of stdout
        #[arg(short = 'o', long)]
        output: Option<String>,
    },
}

fn read_file(path: &str) -> Result<String, String> {
    fs::read_to_string(path).map_err(|e| format!("Cannot open file '{}': {}", path, e))
}

fn parse_file(source: &str) -> Result<Vec<TopLevel>, String> {
    parse_source(source)
}

fn base_dir_for(file: &str) -> String {
    Path::new(file)
        .parent()
        .and_then(|p| p.to_str())
        .unwrap_or(".")
        .to_string()
}

fn load_dep_modules(
    interp: &mut Interpreter,
    items: &[TopLevel],
    base_dir: &str,
) -> Result<(), String> {
    let mut loading = Vec::new();
    if let Some(module) = items.iter().find_map(|i| {
        if let TopLevel::Module(m) = i {
            Some(m)
        } else {
            None
        }
    }) {
        for dep_name in &module.depends {
            let ns = interp
                .load_module(dep_name, base_dir, &mut loading)
                .map_err(|e| e.to_string())?;
            interp
                .define_module_path(dep_name, ns)
                .map_err(|e| e.to_string())?;
        }
    }
    Ok(())
}

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Run { file, verify } => {
            cmd_run(file, *verify);
        }
        Commands::Check { file, strict } => {
            cmd_check(file, *strict);
        }
        Commands::Verify { file } => {
            cmd_verify(file);
        }
        Commands::Decisions { file } => {
            cmd_decisions(file);
        }
        Commands::Repl => {
            cmd_repl();
        }
        Commands::Context { file, output } => {
            cmd_context(file, output.as_deref());
        }
    }
}

fn cmd_run(file: &str, run_verify_blocks: bool) {
    let base_dir = base_dir_for(file);
    let source = match read_file(file) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("{}", e.red());
            process::exit(1);
        }
    };

    let items = match parse_file(&source) {
        Ok(i) => i,
        Err(e) => {
            eprintln!("{}", e.red());
            process::exit(1);
        }
    };

    // Static type check — block execution on any error
    let type_errors = run_type_check_with_base(&items, Some(&base_dir));
    if !type_errors.is_empty() {
        for te in &type_errors {
            eprintln!("{} {}", "Error:".red(), te.message);
        }
        process::exit(1);
    }

    let mut interp = Interpreter::new();
    if let Err(e) = load_dep_modules(&mut interp, &items, &base_dir) {
        eprintln!("{}", e.red());
        process::exit(1);
    }

    // Register effect sets first (needed before FnDef expansion)
    for item in &items {
        if let TopLevel::EffectSet { name, effects } = item {
            interp.register_effect_set(name.clone(), effects.clone());
        }
    }

    // Register type definitions (constructors)
    for item in &items {
        if let TopLevel::TypeDef(td) = item {
            interp.register_type_def(td);
        }
    }

    // Register all function definitions
    for item in &items {
        if let TopLevel::FnDef(fd) = item {
            if let Err(e) = interp.exec_fn_def(fd) {
                eprintln!("{}", e.to_string().red());
                process::exit(1);
            }
        }
    }

    // Run top-level statements that appear before main
    for item in &items {
        if let TopLevel::Stmt(s) = item {
            if let Err(e) = interp.exec_stmt(s) {
                eprintln!("{}", e.to_string().red());
                process::exit(1);
            }
        }
    }

    // Run main() if it exists
    match interp.lookup("main") {
        Ok(main_fn) => {
            let allowed = Interpreter::callable_declared_effects(&main_fn);
            if let Err(e) = interp.call_value_with_effects_pub(main_fn, vec![], "<main>", allowed) {
                eprintln!("{}", e.to_string().red());
                process::exit(1);
            }
        }
        Err(_) => {
            // No main() — that's fine, top-level statements already ran
        }
    }

    // Optionally run verify blocks
    if run_verify_blocks {
        println!();
        let mut total_passed = 0;
        let mut total_failed = 0;

        for item in &items {
            if let TopLevel::Verify(vb) = item {
                let result = run_verify(vb, &mut interp);
                total_passed += result.passed;
                total_failed += result.failed;
                println!();
            }
        }

        if total_failed > 0 {
            process::exit(1);
        }
        let _ = (total_passed, total_failed);
    }
}

fn cmd_check(file: &str, strict: bool) {
    let base_dir = base_dir_for(file);
    let source = match read_file(file) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("{}", e.red());
            process::exit(1);
        }
    };

    let line_count = source.lines().count();

    let items = match parse_file(&source) {
        Ok(i) => i,
        Err(e) => {
            eprintln!("{}", e.red());
            process::exit(1);
        }
    };

    println!("Check: {}", file.cyan());

    // --- Type errors (hard errors) ---
    let type_errors = run_type_check_with_base(&items, Some(&base_dir));
    let has_errors = !type_errors.is_empty();
    for te in &type_errors {
        println!("  {} {}", "Error:".red(), te.message);
    }

    // Check line count
    if line_count > 150 {
        println!(
            "  {} File has {} lines (recommended max: 150)",
            "WARNING:".yellow(),
            line_count
        );
    } else {
        println!("  {} Size OK ({} lines)", "✓".green(), line_count);
    }

    // Check intents, descriptions, and verify coverage
    let warnings = check_module_intent(&items);
    if warnings.is_empty() {
        println!("  {} All intent/desc/verify present", "✓".green());
    } else {
        let label = if strict {
            "Error:".red()
        } else {
            "Warning:".yellow()
        };
        for w in &warnings {
            println!("  {} {}", label, w);
        }
    }

    // Count decisions
    let decisions = index_decisions(&items);
    if !decisions.is_empty() {
        println!(
            "  {} Found {} decision block(s)",
            "✓".green(),
            decisions.len()
        );
    }

    let has_warnings = !warnings.is_empty();
    if has_errors || (strict && has_warnings) {
        process::exit(1);
    } else {
        println!("  {} Type check passed", "✓".green());
    }
}

fn cmd_verify(file: &str) {
    let base_dir = base_dir_for(file);
    let source = match read_file(file) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("{}", e.red());
            process::exit(1);
        }
    };

    let items = match parse_file(&source) {
        Ok(i) => i,
        Err(e) => {
            eprintln!("{}", e.red());
            process::exit(1);
        }
    };

    // Static type check — verify should use the same soundness gate as run/check
    let type_errors = run_type_check_with_base(&items, Some(&base_dir));
    if !type_errors.is_empty() {
        for te in &type_errors {
            eprintln!("{} {}", "Error:".red(), te.message);
        }
        process::exit(1);
    }

    let mut interp = Interpreter::new();
    if let Err(e) = load_dep_modules(&mut interp, &items, &base_dir) {
        eprintln!("{}", e.red());
        process::exit(1);
    }

    // Register effect sets first (needed before FnDef expansion)
    for item in &items {
        if let TopLevel::EffectSet { name, effects } = item {
            interp.register_effect_set(name.clone(), effects.clone());
        }
    }

    // Register type definitions (constructors)
    for item in &items {
        if let TopLevel::TypeDef(td) = item {
            interp.register_type_def(td);
        }
    }

    // Register all functions
    for item in &items {
        if let TopLevel::FnDef(fd) = item {
            if let Err(e) = interp.exec_fn_def(fd) {
                eprintln!("{}", e.to_string().red());
                process::exit(1);
            }
        }
    }

    let verify_blocks: Vec<_> = items
        .iter()
        .filter_map(|item| {
            if let TopLevel::Verify(vb) = item {
                Some(vb)
            } else {
                None
            }
        })
        .collect();

    if verify_blocks.is_empty() {
        println!(
            "{}",
            format!("No verify blocks found in {}.", file).yellow()
        );
        return;
    }

    let mut total_passed = 0;
    let mut total_failed = 0;

    for vb in verify_blocks {
        let result = run_verify(vb, &mut interp);
        total_passed += result.passed;
        total_failed += result.failed;
        println!();
    }

    let total = total_passed + total_failed;
    if total_failed == 0 {
        println!(
            "{}",
            format!("Total: {}/{} passed", total_passed, total).green()
        );
    } else {
        println!(
            "{}",
            format!("Total: {}/{} passed", total_passed, total).red()
        );
        process::exit(1);
    }
}

// ─── aver context ────────────────────────────────────────────────────────────

struct FileContext {
    source_file: String,
    module_name: Option<String>,
    intent: Option<String>,
    exposes: Vec<String>,
    fn_defs: Vec<FnDef>,
    type_defs: Vec<TypeDef>,
    effect_sets: Vec<(String, Vec<String>)>,
    verify_blocks: Vec<VerifyBlock>,
    decisions: Vec<DecisionBlock>,
}

fn collect_contexts(
    file: &str,
    visited: &mut std::collections::HashSet<String>,
) -> Vec<FileContext> {
    let canonical = std::fs::canonicalize(file)
        .unwrap_or_else(|_| std::path::PathBuf::from(file))
        .to_string_lossy()
        .to_string();

    if visited.contains(&canonical) {
        return vec![];
    }
    visited.insert(canonical);

    let source = match fs::read_to_string(file) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Cannot read '{}': {}", file, e);
            return vec![];
        }
    };

    let items = match parse_source(&source) {
        Ok(i) => i,
        Err(e) => {
            eprintln!("Parse error in '{}': {}", file, e);
            return vec![];
        }
    };

    let base_dir = Path::new(file)
        .parent()
        .and_then(|p| p.to_str())
        .unwrap_or(".")
        .to_string();

    let mut ctx = FileContext {
        source_file: file.to_string(),
        module_name: None,
        intent: None,
        exposes: vec![],
        fn_defs: vec![],
        type_defs: vec![],
        effect_sets: vec![],
        verify_blocks: vec![],
        decisions: vec![],
    };

    let mut dep_names: Vec<String> = vec![];

    for item in &items {
        match item {
            TopLevel::Module(m) => {
                ctx.module_name = Some(m.name.clone());
                ctx.intent = if m.intent.is_empty() {
                    None
                } else {
                    Some(m.intent.clone())
                };
                ctx.exposes = m.exposes.clone();
                dep_names = m.depends.clone();
            }
            TopLevel::FnDef(fd) => ctx.fn_defs.push(fd.clone()),
            TopLevel::TypeDef(td) => ctx.type_defs.push(td.clone()),
            TopLevel::EffectSet { name, effects } => {
                ctx.effect_sets.push((name.clone(), effects.clone()))
            }
            TopLevel::Verify(vb) => ctx.verify_blocks.push(vb.clone()),
            TopLevel::Decision(db) => ctx.decisions.push(db.clone()),
            _ => {}
        }
    }

    // Filter functions by exposes if the list is non-empty
    if !ctx.exposes.is_empty() {
        let exposes = ctx.exposes.clone();
        ctx.fn_defs.retain(|fd| exposes.contains(&fd.name));
    }

    let mut result = vec![ctx];

    // Recurse into dependencies
    for dep_name in dep_names {
        if let Some(dep_path) = find_module_file(&dep_name, &base_dir) {
            let dep_file = dep_path.to_string_lossy().to_string();
            let mut sub = collect_contexts(&dep_file, visited);
            result.append(&mut sub);
        }
    }

    result
}

fn format_context_md(contexts: &[FileContext], entry_file: &str) -> String {
    let mut out = String::new();
    out.push_str(&format!("# Aver Context — {}\n\n", entry_file));
    out.push_str("_Generated by `aver context`_\n\n");

    let all_decisions: Vec<&DecisionBlock> =
        contexts.iter().flat_map(|c| c.decisions.iter()).collect();

    for ctx in contexts {
        let has_content = !ctx.fn_defs.is_empty()
            || !ctx.type_defs.is_empty()
            || !ctx.effect_sets.is_empty();

        if !has_content && ctx.module_name.is_none() {
            continue;
        }

        out.push_str("---\n\n");

        if let Some(name) = &ctx.module_name {
            out.push_str(&format!("## Module: {}\n\n", name));
        } else {
            out.push_str(&format!("## {}\n\n", ctx.source_file));
        }

        if let Some(intent) = &ctx.intent {
            out.push_str(&format!("> {}\n\n", intent));
        }

        // Effect set aliases
        for (name, effects) in &ctx.effect_sets {
            out.push_str(&format!(
                "**effects** `{}` = `[{}]`\n\n",
                name,
                effects.join(", ")
            ));
        }

        // Types
        for td in &ctx.type_defs {
            match td {
                TypeDef::Sum { name, variants } => {
                    out.push_str(&format!("### type {}\n", name));
                    let vars: Vec<String> = variants
                        .iter()
                        .map(|v| {
                            if v.fields.is_empty() {
                                v.name.clone()
                            } else {
                                format!("{}({})", v.name, v.fields.join(", "))
                            }
                        })
                        .collect();
                    out.push_str(&format!("`{}`\n\n", vars.join("` | `")));
                }
                TypeDef::Product { name, fields } => {
                    out.push_str(&format!("### record {}\n", name));
                    let flds: Vec<String> = fields
                        .iter()
                        .map(|(fname, ftype)| format!("{}: {}", fname, ftype))
                        .collect();
                    out.push_str(&format!("`{}`\n\n", flds.join("`, `")));
                }
            }
        }

        // Functions
        for fd in &ctx.fn_defs {
            if fd.name == "main" {
                continue;
            }

            let params: Vec<String> = fd
                .params
                .iter()
                .map(|(pname, ptype)| format!("{}: {}", pname, ptype))
                .collect();
            out.push_str(&format!(
                "### `{}({}) -> {}`\n",
                fd.name,
                params.join(", "),
                fd.return_type
            ));

            if !fd.effects.is_empty() {
                out.push_str(&format!("effects: `[{}]`  \n", fd.effects.join(", ")));
            }

            if let Some(desc) = &fd.desc {
                out.push_str(&format!("> {}\n", desc));
            }

            // Verify cases for this function (max 3)
            for vb in ctx.verify_blocks.iter().filter(|vb| vb.fn_name == fd.name) {
                if vb.cases.is_empty() {
                    continue;
                }
                let max = 3usize;
                let shown: Vec<String> = vb
                    .cases
                    .iter()
                    .take(max)
                    .map(|(lhs, rhs)| format!("`{}` → `{}`", expr_to_str(lhs), expr_to_str(rhs)))
                    .collect();
                let extra = if vb.cases.len() > max {
                    format!(" _(+{} more)_", vb.cases.len() - max)
                } else {
                    String::new()
                };
                out.push_str(&format!("verify: {}{}\n", shown.join(", "), extra));
            }

            out.push('\n');
        }
    }

    // Decisions collected from all files
    if !all_decisions.is_empty() {
        out.push_str("---\n\n## Decisions\n\n");
        for dec in all_decisions {
            out.push_str(&format!("### {} ({})\n", dec.name, dec.date));
            out.push_str(&format!("**Chosen:** {}", dec.chosen));
            if !dec.rejected.is_empty() {
                out.push_str(&format!(" — **Rejected:** {}", dec.rejected.join(", ")));
            }
            out.push('\n');
            if !dec.reason.is_empty() {
                let reason = if dec.reason.len() > 160 {
                    format!("{}…", dec.reason[..160].trim_end())
                } else {
                    dec.reason.clone()
                };
                out.push_str(&format!("> {}\n", reason));
            }
            if !dec.impacts.is_empty() {
                out.push_str(&format!("impacts: `{}`\n", dec.impacts.join("`, `")));
            }
            out.push('\n');
        }
    }

    out
}

fn cmd_context(file: &str, output: Option<&str>) {
    let mut visited = std::collections::HashSet::new();
    let contexts = collect_contexts(file, &mut visited);

    if contexts.is_empty() {
        eprintln!("{}", "No content found.".yellow());
        process::exit(1);
    }

    let markdown = format_context_md(&contexts, file);

    match output {
        None => print!("{}", markdown),
        Some(out_path) => {
            if let Err(e) = fs::write(out_path, &markdown) {
                eprintln!("{} Cannot write to '{}': {}", "Error:".red(), out_path, e);
                process::exit(1);
            }
            println!("{}", format!("Context written to {}", out_path).green());
        }
    }
}

// ─── REPL ────────────────────────────────────────────────────────────────────

fn is_incomplete(source: &str) -> bool {
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

fn repl_help() {
    println!("Commands:");
    println!("  :help / :h   Show this help");
    println!("  :quit / :q   Exit the REPL");
    println!("  :clear / :c  Clear all definitions and restart");
    println!("  :env         Show all defined names");
    println!();
    println!("Multi-line input: fn/type/record/verify/module start a block.");
    println!("Press Enter on an empty line to finish a block.");
}

fn repl_env(interp: &Interpreter) {
    let builtins = [
        "print", "str", "int", "abs", "len", "map", "filter", "fold", "get", "push", "head",
        "tail", "Ok", "Err", "Some", "None", "Console",
    ];
    let mut found = false;
    for scope in &interp.env {
        for (name, val) in scope {
            if builtins.contains(&name.as_str()) {
                continue;
            }
            if name.starts_with("__") {
                continue;
            }
            println!("  {} = {}", name, aver_repr(val));
            found = true;
        }
    }
    if !found {
        println!("  (empty)");
    }
}

fn cmd_repl() {
    let mut interp = Interpreter::new();
    let mut accumulated: Vec<TopLevel> = Vec::new();
    let mut buffer: Vec<String> = Vec::new();

    println!("Aver REPL — :help for commands, :quit to exit");

    let stdin = io::stdin();

    loop {
        let prompt = if buffer.is_empty() { "aver> " } else { "...   " };
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
            for te in &type_errors {
                eprintln!("{} {}", "Error:".red(), te.message);
            }
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
                        Stmt::Val(name, _) | Stmt::Var(name, _, _) => {
                            if let Ok(v) = interp.lookup(name) {
                                println!("{} = {}", name, aver_repr(&v));
                            }
                        }
                        Stmt::Assign(name, _) => {
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

fn cmd_decisions(file: &str) {
    let source = match read_file(file) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("{}", e.red());
            process::exit(1);
        }
    };

    let items = match parse_file(&source) {
        Ok(i) => i,
        Err(e) => {
            eprintln!("{}", e.red());
            process::exit(1);
        }
    };

    let decisions = index_decisions(&items);

    if decisions.is_empty() {
        println!("{}", format!("No decision blocks in {}", file).yellow());
        return;
    }

    println!("Decisions in {}:", file.cyan());
    println!();

    for dec in decisions {
        println!("  {} ({})", dec.name.cyan(), dec.date.yellow());
        println!("    Chosen:   {}", dec.chosen);
        if !dec.rejected.is_empty() {
            println!("    Rejected: {}", dec.rejected.join(", "));
        }
        if !dec.impacts.is_empty() {
            println!("    Impacts:  {}", dec.impacts.join(", "));
        }
        if !dec.reason.is_empty() {
            let prefix = "    Reason:   ";
            let continuation = "              ";
            let max_width = 80usize;
            let mut current = prefix.to_string();
            for word in dec.reason.split_whitespace() {
                if current == prefix || current == continuation {
                    current.push_str(word);
                } else if current.len() + 1 + word.len() <= max_width {
                    current.push(' ');
                    current.push_str(word);
                } else {
                    println!("{}", current);
                    current = format!("{}{}", continuation, word);
                }
            }
            println!("{}", current);
        }
        if let Some(author) = &dec.author {
            println!("    Author:   {}", author);
        }
        println!();
    }
}

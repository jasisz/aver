use std::fs;
use std::path::Path;
use std::process;

use clap::{Parser as ClapParser, Subcommand};
use colored::Colorize;

use aver::ast::TopLevel;
use aver::checker::{check_module_intent, index_decisions, run_verify};
use aver::interpreter::Interpreter;
use aver::source::parse_source;
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

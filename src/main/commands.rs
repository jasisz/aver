use std::fs;
use std::path::{Path, PathBuf};
use std::process;
use std::time::{SystemTime, UNIX_EPOCH};

use colored::Colorize;

use aver::ast::TopLevel;
use aver::checker::{check_module_intent, index_decisions, run_verify};
use aver::interpreter::{aver_repr, Interpreter, Value};
use aver::replay::{
    session_recording_to_string_pretty, value_to_json, JsonValue, RecordedOutcome, SessionRecording,
};
use aver::resolver;
use aver::tco;
use aver::types::checker::{run_type_check_full, run_type_check_with_base};

use crate::shared::{
    compile_program_for_exec, compute_memo_fns, load_dep_modules, parse_file, print_type_errors,
    read_file, resolve_module_root, run_entry_function, run_top_level_statements,
};

pub(super) fn generate_request_id() -> String {
    let millis = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_millis())
        .unwrap_or(0);
    format!("rec-{}", millis)
}

pub(super) fn generate_timestamp() -> String {
    let secs = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0);
    format!("unix-{}", secs)
}

pub(super) fn write_session_recording(
    dir: &str,
    recording: &SessionRecording,
) -> Result<PathBuf, String> {
    let dir_path = Path::new(dir);
    fs::create_dir_all(dir_path)
        .map_err(|e| format!("Cannot create recording dir '{}': {}", dir, e))?;
    let out_path = dir_path.join(format!("{}.json", recording.request_id));
    let json = session_recording_to_string_pretty(recording);
    fs::write(&out_path, json)
        .map_err(|e| format!("Cannot write recording '{}': {}", out_path.display(), e))?;
    Ok(out_path)
}

pub(super) fn cmd_run(
    file: &str,
    module_root_override: Option<&str>,
    run_verify_blocks: bool,
    record_dir: Option<&str>,
) {
    if run_verify_blocks && record_dir.is_some() {
        eprintln!(
            "{}",
            "Cannot combine --verify and --record in one run; record should capture only main flow."
                .red()
        );
        process::exit(1);
    }

    let (mut interp, items, module_root) =
        match compile_program_for_exec(file, module_root_override) {
            Ok(v) => v,
            Err(e) => {
                eprintln!("{}", e.red());
                process::exit(1);
            }
        };

    if record_dir.is_some() {
        interp.start_recording();
    }

    let mut runtime_failure: Option<String> = run_top_level_statements(&mut interp, &items).err();

    let mut main_result: Option<Result<Value, String>> = None;
    if runtime_failure.is_none() {
        if interp.lookup("main").is_ok() {
            let result = run_entry_function(&mut interp, "main", vec![]);
            if let Ok(Value::Err(err)) = &result {
                runtime_failure = Some(format!("Main returned error: {}", aver_repr(err)));
            } else if let Err(e) = &result {
                runtime_failure = Some(e.clone());
            }
            main_result = Some(result);
        }
    }

    if let Some(dir) = record_dir {
        let output = if let Some(msg) = &runtime_failure {
            RecordedOutcome::RuntimeError(msg.clone())
        } else {
            match &main_result {
                Some(Ok(v)) => match value_to_json(v) {
                    Ok(json) => RecordedOutcome::Value(json),
                    Err(e) => RecordedOutcome::RuntimeError(e),
                },
                Some(Err(e)) => RecordedOutcome::RuntimeError(e.clone()),
                None => RecordedOutcome::Value(JsonValue::Null),
            }
        };

        let recording = SessionRecording {
            schema_version: 1,
            request_id: generate_request_id(),
            timestamp: generate_timestamp(),
            program_file: file.to_string(),
            module_root: module_root.clone(),
            entry_fn: "main".to_string(),
            input: JsonValue::Null,
            effects: interp.take_recorded_effects(),
            output,
        };

        match write_session_recording(dir, &recording) {
            Ok(path) => println!("Recording saved: {}", path.display()),
            Err(e) => {
                eprintln!("{}", e.red());
                process::exit(1);
            }
        }
    }

    if let Some(msg) = runtime_failure {
        eprintln!("{}", msg.red());
        process::exit(1);
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

pub(super) fn cmd_check(file: &str, module_root_override: Option<&str>, strict: bool) {
    let module_root = resolve_module_root(module_root_override);
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
    let type_errors = run_type_check_with_base(&items, Some(&module_root));
    let has_errors = !type_errors.is_empty();
    for te in &type_errors {
        match te.line {
            Some(line) => println!("  {}", format!("error[{}]: {}", line, te.message).red()),
            None => println!("  {}", format!("error: {}", te.message).red()),
        }
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

pub(super) fn cmd_verify(file: &str, module_root_override: Option<&str>) {
    let module_root = resolve_module_root(module_root_override);
    let source = match read_file(file) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("{}", e.red());
            process::exit(1);
        }
    };

    let mut items = match parse_file(&source) {
        Ok(i) => i,
        Err(e) => {
            eprintln!("{}", e.red());
            process::exit(1);
        }
    };

    // TCO transform — rewrite tail-position calls in recursive SCCs
    tco::transform_program(&mut items);

    // Static type check — verify should use the same soundness gate as run/check
    let tc_result = run_type_check_full(&items, Some(&module_root));
    if !tc_result.errors.is_empty() {
        print_type_errors(&tc_result.errors);
        process::exit(1);
    }

    // Compile-time variable resolution
    resolver::resolve_program(&mut items);

    // Auto-memoization
    let memo_fns = compute_memo_fns(&items, &tc_result);

    let mut interp = Interpreter::new();
    interp.enable_memo(memo_fns);

    if let Err(e) = load_dep_modules(&mut interp, &items, &module_root) {
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

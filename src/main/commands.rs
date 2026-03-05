use std::fs;
use std::path::{Path, PathBuf};
use std::process;
use std::time::{SystemTime, UNIX_EPOCH};

use colored::Colorize;

use aver::ast::TopLevel;
use aver::checker::{
    CheckFinding, check_module_intent_with_sigs_in, index_decisions, merge_verify_blocks,
    run_verify,
};
use aver::codegen;
use aver::codegen::ModuleInfo;
use aver::codegen::lean as lean_codegen;
use aver::codegen::rust as rust_codegen;
use aver::interpreter::{Interpreter, RecordingConfig, Value, aver_repr};
use aver::replay::{JsonValue, RecordedOutcome, value_to_json};
use aver::resolver;
use aver::source::{find_module_file, require_module_declaration};
use aver::tco;
use aver::types::checker::run_type_check_full;

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

pub(super) fn prepare_recording_path(dir: &str, request_id: &str) -> Result<PathBuf, String> {
    let dir_path = Path::new(dir);
    fs::create_dir_all(dir_path)
        .map_err(|e| format!("Cannot create recording dir '{}': {}", dir, e))?;
    Ok(dir_path.join(format!("{}.json", request_id)))
}

fn path_to_string(path: &Path) -> String {
    path.to_string_lossy().into_owned()
}

fn relativize_to(base: &Path, path: &Path) -> Option<String> {
    let rel = path.strip_prefix(base).ok()?;
    if rel.as_os_str().is_empty() {
        Some(".".to_string())
    } else {
        Some(path_to_string(rel))
    }
}

fn relativize_to_canonical(base: &Path, path: &Path) -> Option<String> {
    let base_canon = std::fs::canonicalize(base).ok()?;
    let path_canon = std::fs::canonicalize(path).ok()?;
    relativize_to(&base_canon, &path_canon)
}

fn recording_paths(file: &str, module_root: &str) -> (String, String) {
    let cwd = std::env::current_dir().ok();
    let module_root_path = Path::new(module_root);
    let file_path = Path::new(file);

    let rec_module_root = if module_root_path.is_absolute() {
        match cwd.as_ref().and_then(|cwd_path| {
            relativize_to(cwd_path, module_root_path)
                .or_else(|| relativize_to_canonical(cwd_path, module_root_path))
        }) {
            Some(rel) => rel,
            None => module_root.to_string(),
        }
    } else {
        module_root.to_string()
    };

    let rec_program_file = if file_path.is_absolute() {
        if let Some(rel) = relativize_to(module_root_path, file_path) {
            rel
        } else if let Some(rel) = relativize_to_canonical(module_root_path, file_path) {
            rel
        } else if let Some(rel) = cwd.as_ref().and_then(|cwd_path| {
            relativize_to(cwd_path, file_path)
                .or_else(|| relativize_to_canonical(cwd_path, file_path))
        }) {
            rel
        } else {
            file.to_string()
        }
    } else {
        file.to_string()
    };

    (rec_program_file, rec_module_root)
}

fn module_name(items: &[TopLevel]) -> Option<String> {
    items.iter().find_map(|item| {
        if let TopLevel::Module(m) = item {
            Some(m.name.clone())
        } else {
            None
        }
    })
}

fn collect_check_units(
    file: &str,
    module_root: &str,
    include_deps: bool,
) -> Result<Vec<(String, String, Vec<TopLevel>)>, String> {
    let mut out = Vec::new();
    let mut stack = vec![PathBuf::from(file)];
    let mut visited = std::collections::HashSet::new();

    while let Some(path) = stack.pop() {
        let canonical = std::fs::canonicalize(&path).unwrap_or_else(|_| path.clone());
        let key = canonical.to_string_lossy().to_string();
        if !visited.insert(key) {
            continue;
        }

        let path_str = path.to_string_lossy().to_string();
        let source = read_file(&path_str)?;
        let items = parse_file(&source)?;
        require_module_declaration(&items, &path_str)?;

        if include_deps {
            if let Some(m) = items.iter().find_map(|item| {
                if let TopLevel::Module(m) = item {
                    Some(m)
                } else {
                    None
                }
            }) {
                for dep in m.depends.iter().rev() {
                    let dep_path = find_module_file(dep, module_root).ok_or_else(|| {
                        format!(
                            "Module '{}' not found in '{}' (required by '{}')",
                            dep, module_root, path_str
                        )
                    })?;
                    stack.push(dep_path);
                }
            }
        }

        out.push((path_str, source, items));
    }

    Ok(out)
}

fn finding_location(f: &CheckFinding, entry_module: Option<&str>) -> String {
    match (&f.module, entry_module) {
        (Some(module), Some(entry)) if module == entry => f.line.to_string(),
        (Some(module), _) => format!("{}:{}", module, f.line),
        (None, _) => f.line.to_string(),
    }
}

fn display_check_path(path: &str, module_root: &str) -> String {
    let p = Path::new(path);
    let root = Path::new(module_root);

    if p.is_absolute() {
        if let Some(rel) = relativize_to(root, p).or_else(|| relativize_to_canonical(root, p)) {
            return rel;
        }
        if let Some(cwd) = std::env::current_dir().ok() {
            if let Some(rel) = relativize_to(&cwd, p).or_else(|| relativize_to_canonical(&cwd, p)) {
                return rel;
            }
        }
    }

    path.to_string()
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

    let recording_target = if let Some(dir) = record_dir {
        let request_id = generate_request_id();
        let timestamp = generate_timestamp();
        let (record_program_file, record_module_root) = recording_paths(file, &module_root);
        let out_path = match prepare_recording_path(dir, &request_id) {
            Ok(path) => path,
            Err(e) => {
                eprintln!("{}", e.red());
                process::exit(1);
            }
        };
        interp.configure_recording_sink(RecordingConfig {
            path: out_path.clone(),
            request_id: request_id.clone(),
            timestamp: timestamp.clone(),
            program_file: record_program_file,
            module_root: record_module_root,
            entry_fn: "main".to_string(),
            input: JsonValue::Null,
        });
        interp.start_recording();
        if let Err(e) = interp.persist_recording_snapshot(RecordedOutcome::Value(JsonValue::Null)) {
            eprintln!("{}", e.to_string().red());
            process::exit(1);
        }
        Some(out_path)
    } else {
        None
    };

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

    if recording_target.is_some() {
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

        if let Err(e) = interp.persist_recording_snapshot(output) {
            eprintln!("{}", e.to_string().red());
            process::exit(1);
        }
        if let Some(path) = interp.recording_sink_path() {
            println!("Recording saved: {}", path.display());
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

        let verify_blocks = merge_verify_blocks(&items);
        for vb in &verify_blocks {
            let result = run_verify(vb, &mut interp);
            total_passed += result.passed;
            total_failed += result.failed;
            println!();
        }

        if total_failed > 0 {
            process::exit(1);
        }
        let _ = (total_passed, total_failed);
    }
}

pub(super) fn cmd_check(file: &str, module_root_override: Option<&str>, deps: bool) {
    let module_root = resolve_module_root(module_root_override);
    let units = match collect_check_units(file, &module_root, deps) {
        Ok(units) => units,
        Err(e) => {
            eprintln!("{}", e.red());
            process::exit(1);
        }
    };

    let entry_module = units.first().and_then(|(_, _, items)| module_name(items));
    let mut has_any_error = false;

    for (idx, (path, source, items)) in units.iter().enumerate() {
        if idx > 0 {
            println!();
        }
        let shown_path = display_check_path(path, &module_root);
        println!("Check: {}", shown_path.cyan());
        let line_count = source.lines().count();

        // --- Type errors (hard errors) ---
        let tc_result = run_type_check_full(items, Some(&module_root));
        let has_errors = !tc_result.errors.is_empty();
        for te in &tc_result.errors {
            println!("  {}", format!("error[{}]: {}", te.line, te.message).red());
        }

        // Check line count
        if line_count > 500 {
            println!(
                "  {} File has {} lines (recommended max: 500)",
                "WARNING:".yellow(),
                line_count
            );
        } else {
            println!("  {} Size OK ({} lines)", "✓".green(), line_count);
        }

        // Check intents, descriptions, and verify coverage
        let findings =
            check_module_intent_with_sigs_in(items, Some(&tc_result.fn_sigs), Some(path));
        if findings.errors.is_empty() && findings.warnings.is_empty() {
            println!("  {} All intent/desc/verify present", "✓".green());
        } else {
            for e in &findings.errors {
                let loc = finding_location(e, entry_module.as_deref());
                println!("  {}", format!("error[{}]: {}", loc, e.message).red());
            }
            for w in &findings.warnings {
                let loc = finding_location(w, entry_module.as_deref());
                println!("  {}", format!("error[{}]: {}", loc, w.message).red());
            }
        }

        // Count decisions
        let decisions = index_decisions(items);
        if !decisions.is_empty() {
            println!(
                "  {} Found {} decision block(s)",
                "✓".green(),
                decisions.len()
            );
        }

        let has_warnings = !findings.warnings.is_empty();
        let has_contract_errors = !findings.errors.is_empty();
        if has_errors || has_contract_errors || has_warnings {
            has_any_error = true;
        } else {
            println!("  {} Type check passed", "✓".green());
        }
    }

    if has_any_error {
        process::exit(1);
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
    if let Err(e) = require_module_declaration(&items, file) {
        eprintln!("{}", e.red());
        process::exit(1);
    }

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

    let verify_blocks = merge_verify_blocks(&items);

    if verify_blocks.is_empty() {
        println!(
            "{}",
            format!("No verify blocks found in {}.", file).yellow()
        );
        return;
    }

    let mut total_passed = 0;
    let mut total_failed = 0;

    for vb in &verify_blocks {
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

pub(super) fn cmd_compile(
    file: &str,
    output_dir: &str,
    target: &super::cli::Target,
    project_name: Option<&str>,
    module_root_override: Option<&str>,
    lean_verify: &super::cli::LeanVerifyMode,
    lean_proof_mode: bool,
) {
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
    if let Err(e) = require_module_declaration(&items, file) {
        eprintln!("{}", e.red());
        process::exit(1);
    }

    // TCO transform
    tco::transform_program(&mut items);

    // Static type check
    let tc_result = run_type_check_full(&items, Some(&module_root));
    if !tc_result.errors.is_empty() {
        print_type_errors(&tc_result.errors);
        process::exit(1);
    }

    // Compute memo-eligible functions
    let memo_fns = compute_memo_fns(&items, &tc_result);

    // Derive project name from file if not specified
    let name = project_name.map(|s| s.to_string()).unwrap_or_else(|| {
        Path::new(file)
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("aver_program")
            .to_string()
    });

    // Load dependent modules for codegen
    let modules = load_compile_deps(&items, &module_root);

    // Build codegen context
    let ctx = codegen::build_context(items, &tc_result, memo_fns, name, modules);

    // Transpile to the selected target
    let (output, build_hint) = match target {
        super::cli::Target::Rust => {
            let out = rust_codegen::transpile(&ctx);
            let hint = format!("cd {} && cargo build && cargo run", output_dir);
            (out, hint)
        }
        super::cli::Target::Lean => {
            if lean_proof_mode
                && matches!(
                    lean_verify,
                    super::cli::LeanVerifyMode::Sorry | super::cli::LeanVerifyMode::TheoremSkeleton
                )
            {
                eprintln!(
                    "{}",
                    "Lean proof mode requires --lean-verify auto (not sorry/theorem-skeleton)."
                        .red()
                );
                process::exit(1);
            }

            if lean_proof_mode {
                let proof_issues = lean_codegen::proof_mode_issues(&ctx);
                if !proof_issues.is_empty() {
                    eprintln!("{}", "Lean proof mode blocked compilation:".red());
                    for issue in proof_issues {
                        eprintln!("  - {}", issue);
                    }
                    process::exit(1);
                }
            }

            let verify_mode = match lean_verify {
                super::cli::LeanVerifyMode::Auto => lean_codegen::VerifyEmitMode::NativeDecide,
                super::cli::LeanVerifyMode::Sorry => lean_codegen::VerifyEmitMode::Sorry,
                super::cli::LeanVerifyMode::TheoremSkeleton => {
                    lean_codegen::VerifyEmitMode::TheoremSkeleton
                }
            };
            let out = if lean_proof_mode {
                lean_codegen::transpile_for_proof_mode(&ctx, verify_mode)
            } else {
                lean_codegen::transpile_with_verify_mode(&ctx, verify_mode)
            };
            let hint = format!("cd {} && lake build", output_dir);
            (out, hint)
        }
    };

    // Write output files
    let out_path = Path::new(output_dir);
    for (rel_path, content) in &output.files {
        let full_path = out_path.join(rel_path);
        if let Some(parent) = full_path.parent() {
            if let Err(e) = fs::create_dir_all(parent) {
                eprintln!(
                    "{}",
                    format!("Cannot create dir '{}': {}", parent.display(), e).red()
                );
                process::exit(1);
            }
        }
        if let Err(e) = fs::write(&full_path, content) {
            eprintln!(
                "{}",
                format!("Cannot write '{}': {}", full_path.display(), e).red()
            );
            process::exit(1);
        }
    }

    let target_label = match target {
        super::cli::Target::Rust => "Rust",
        super::cli::Target::Lean => "Lean 4",
    };
    println!(
        "{}",
        format!("Compiled {} → {}/ [{}]", file, output_dir, target_label).green()
    );
    println!("  {}", build_hint.cyan());
}

/// Load dependent modules for codegen (recursive, with circular import detection).
fn load_compile_deps(items: &[TopLevel], module_root: &str) -> Vec<ModuleInfo> {
    let module = items.iter().find_map(|i| {
        if let TopLevel::Module(m) = i {
            Some(m)
        } else {
            None
        }
    });
    let Some(module) = module else {
        return vec![];
    };

    let mut result = Vec::new();
    let mut loaded = std::collections::HashSet::new();

    for dep_name in &module.depends {
        load_module_recursive(dep_name, module_root, &mut result, &mut loaded);
    }

    result
}

fn load_module_recursive(
    name: &str,
    module_root: &str,
    result: &mut Vec<ModuleInfo>,
    loaded: &mut std::collections::HashSet<String>,
) {
    if !loaded.insert(name.to_string()) {
        return; // already loaded or circular
    }

    let path = match find_module_file(name, module_root) {
        Some(p) => p,
        None => {
            eprintln!(
                "{}",
                format!(
                    "Cannot find module '{}' in module root '{}'",
                    name, module_root
                )
                .red()
            );
            process::exit(1);
        }
    };

    let source = match read_file(path.to_str().unwrap_or("")) {
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
    if let Err(e) = require_module_declaration(&items, path.to_str().unwrap_or(name)) {
        eprintln!("{}", e.red());
        process::exit(1);
    }

    tco::transform_program(&mut items);

    // Recursively load transitive dependencies
    if let Some(mod_block) = items.iter().find_map(|i| {
        if let TopLevel::Module(m) = i {
            Some(m)
        } else {
            None
        }
    }) {
        for dep in &mod_block.depends {
            load_module_recursive(dep, module_root, result, loaded);
        }
    }

    let type_defs: Vec<_> = items
        .iter()
        .filter_map(|i| {
            if let TopLevel::TypeDef(td) = i {
                Some(td.clone())
            } else {
                None
            }
        })
        .collect();

    let fn_defs: Vec<_> = items
        .iter()
        .filter_map(|i| {
            if let TopLevel::FnDef(fd) = i {
                if fd.name != "main" {
                    Some(fd.clone())
                } else {
                    None
                }
            } else {
                None
            }
        })
        .collect();

    result.push(ModuleInfo {
        prefix: name.to_string(),
        type_defs,
        fn_defs,
    });
}

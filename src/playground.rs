//! Browser-facing entry points for the Aver playground.

use std::collections::{HashMap, HashSet};

use crate::ast::TopLevel;
use crate::codegen;
use crate::diagnostics::{AnalyzeOptions, analyze_source};
use crate::resolver;
use crate::source::{LoadedModule, load_module_tree_from_map, parse_source};
use crate::tco;
use crate::types::checker::{run_type_check_full, run_type_check_with_loaded};
#[cfg(feature = "runtime")]
use crate::{nan_value::Arena, vm};

/// Compile Aver source text to WASM bytes.
pub fn compile_to_wasm(source: &str) -> Result<Vec<u8>, String> {
    let mut items = parse_source(source)?;
    tco::transform_program(&mut items);

    let tc_result = run_type_check_full(&items, None);
    if !tc_result.errors.is_empty() {
        return Err(format_tc_errors(&tc_result.errors));
    }

    let ctx = codegen::build_context(
        items,
        &tc_result,
        HashSet::new(),
        "playground".to_string(),
        vec![],
    );
    codegen::wasm::emit_wasm(&ctx)
}

/// Compile a multi-file Aver project from an in-memory file map.
/// `files` maps `path -> source` (matching what `find_module_file`
/// expects: e.g. `"types.av"`, `"rogue/combat.av"`). `entry` is the
/// key of the file holding `module Main` (the `fn main` live point).
///
/// Mirrors the CLI's multi-file build, minus disk IO — the same
/// type checker, resolver, and codegen are reused verbatim so the
/// browser sees identical semantics.
pub fn compile_project_to_wasm(
    files: &HashMap<String, String>,
    entry: &str,
) -> Result<Vec<u8>, String> {
    let entry_source = files
        .get(entry)
        .ok_or_else(|| format!("Entry '{}' not present in file map", entry))?;

    let mut entry_items = parse_source(entry_source)?;
    tco::transform_program(&mut entry_items);

    let root_depends = module_depends(&entry_items);
    let loaded = load_module_tree_from_map(&root_depends, files)?;

    let tc_result = run_type_check_with_loaded(&entry_items, &loaded);
    if !tc_result.errors.is_empty() {
        return Err(format_tc_errors(&tc_result.errors));
    }

    resolver::resolve_program(&mut entry_items);

    let modules: Vec<codegen::ModuleInfo> = loaded.into_iter().map(loaded_to_module_info).collect();

    let ctx = codegen::build_context(
        entry_items,
        &tc_result,
        HashSet::new(),
        "playground".to_string(),
        modules,
    );
    codegen::wasm::emit_wasm(&ctx)
}

// ── Proof export & Rust compile entry points ────────────────────────
//
// Single-file source → backend project files (path → content map).
// JS receives `{ "<path>": "<content>", ... }` as JSON and zips it
// in the browser via `buildZip`. Same as what `aver proof --backend
// {lean,dafny}` and `aver compile --target rust` produce on disk.

#[cfg(feature = "runtime")]
fn build_ctx(source: &str) -> Result<codegen::CodegenContext, String> {
    let mut items = parse_source(source)?;
    tco::transform_program(&mut items);
    let tc_result = run_type_check_full(&items, None);
    if !tc_result.errors.is_empty() {
        return Err(format_tc_errors(&tc_result.errors));
    }
    Ok(codegen::build_context(
        items,
        &tc_result,
        HashSet::new(),
        "playground".to_string(),
        vec![],
    ))
}

/// Single-file Aver source → Lean 4 project files.
#[cfg(feature = "runtime")]
pub fn proof_lean_files(source: &str) -> Result<HashMap<String, String>, String> {
    let ctx = build_ctx(source)?;
    let output = codegen::lean::transpile(&ctx);
    Ok(output.files.into_iter().collect())
}

/// Single-file Aver source → Dafny project files.
#[cfg(feature = "runtime")]
pub fn proof_dafny_files(source: &str) -> Result<HashMap<String, String>, String> {
    let ctx = build_ctx(source)?;
    let output = codegen::dafny::transpile(&ctx);
    Ok(output.files.into_iter().collect())
}

/// Single-file Aver source → Rust/Cargo project files.
#[cfg(feature = "runtime")]
pub fn compile_rust_files(source: &str) -> Result<HashMap<String, String>, String> {
    let mut ctx = build_ctx(source)?;
    let output = codegen::rust::transpile(&mut ctx);
    Ok(output.files.into_iter().collect())
}

/// Build a `CodegenContext` for a multi-file project — same semantics
/// as `compile_project_to_wasm`, refactored so the proof and Rust
/// exports can reuse it.
#[cfg(feature = "runtime")]
fn build_project_ctx(
    files: &HashMap<String, String>,
    entry: &str,
) -> Result<codegen::CodegenContext, String> {
    let entry_source = files
        .get(entry)
        .ok_or_else(|| format!("Entry '{}' not present in file map", entry))?;
    let mut entry_items = parse_source(entry_source)?;
    tco::transform_program(&mut entry_items);

    let root_depends = module_depends(&entry_items);
    let loaded = load_module_tree_from_map(&root_depends, files)?;

    let tc_result = run_type_check_with_loaded(&entry_items, &loaded);
    if !tc_result.errors.is_empty() {
        return Err(format_tc_errors(&tc_result.errors));
    }

    resolver::resolve_program(&mut entry_items);
    let modules: Vec<codegen::ModuleInfo> = loaded.into_iter().map(loaded_to_module_info).collect();

    Ok(codegen::build_context(
        entry_items,
        &tc_result,
        HashSet::new(),
        "playground".to_string(),
        modules,
    ))
}

/// Multi-file Aver project → Lean 4 project files.
#[cfg(feature = "runtime")]
pub fn proof_lean_files_project(
    files: &HashMap<String, String>,
    entry: &str,
) -> Result<HashMap<String, String>, String> {
    let ctx = build_project_ctx(files, entry)?;
    let output = codegen::lean::transpile(&ctx);
    Ok(output.files.into_iter().collect())
}

/// Multi-file Aver project → Dafny project files.
#[cfg(feature = "runtime")]
pub fn proof_dafny_files_project(
    files: &HashMap<String, String>,
    entry: &str,
) -> Result<HashMap<String, String>, String> {
    let ctx = build_project_ctx(files, entry)?;
    let output = codegen::dafny::transpile(&ctx);
    Ok(output.files.into_iter().collect())
}

/// Multi-file Aver project → Rust/Cargo project files.
#[cfg(feature = "runtime")]
pub fn compile_rust_files_project(
    files: &HashMap<String, String>,
    entry: &str,
) -> Result<HashMap<String, String>, String> {
    let mut ctx = build_project_ctx(files, entry)?;
    let output = codegen::rust::transpile(&mut ctx);
    Ok(output.files.into_iter().collect())
}

fn module_depends(items: &[TopLevel]) -> Vec<String> {
    items
        .iter()
        .find_map(|i| match i {
            TopLevel::Module(m) => Some(m.depends.clone()),
            _ => None,
        })
        .unwrap_or_default()
}

fn loaded_to_module_info(m: LoadedModule) -> codegen::ModuleInfo {
    let mut items = m.items;
    tco::transform_program(&mut items);
    resolver::resolve_program(&mut items);

    let depends = module_depends(&items);
    let type_defs = items
        .iter()
        .filter_map(|i| match i {
            TopLevel::TypeDef(td) => Some(td.clone()),
            _ => None,
        })
        .collect();
    let fn_defs = items
        .iter()
        .filter_map(|i| match i {
            TopLevel::FnDef(fd) if fd.name != "main" => Some(fd.clone()),
            _ => None,
        })
        .collect();

    codegen::ModuleInfo {
        prefix: m.dep_name,
        depends,
        type_defs,
        fn_defs,
    }
}

fn format_tc_errors(errors: &[crate::types::checker::TypeError]) -> String {
    errors
        .iter()
        .map(|e| format!("error[{}:{}]: {}", e.line, e.col, e.message))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Run the single-file analysis pipeline and return the canonical
/// [`AnalysisReport`](crate::diagnostics::AnalysisReport) as JSON. Consumers
/// should parse the `diagnostics` array; an empty array means the file
/// passed every enabled check.
pub fn check_source(source: &str) -> String {
    let opts = AnalyzeOptions::new("playground");
    analyze_source(source, &opts).to_json()
}

/// Multi-file variant: builds an `AnalyzeOptions` with dependency
/// modules pre-loaded from the provided virtual fs map, so the type
/// checker sees every `depends [...]` entry without disk IO.
/// Verify execution is skipped for multi-file projects (VM module
/// loader is disk-only today).
fn analyze_project(
    files: &HashMap<String, String>,
    entry: &str,
    make_opts: impl FnOnce(AnalyzeOptions) -> AnalyzeOptions,
) -> String {
    let entry_source = match files.get(entry) {
        Some(s) => s.clone(),
        None => {
            return crate::diagnostics::AnalysisReport::new("playground").to_json();
        }
    };
    let mut opts = AnalyzeOptions::new("playground");
    // Parse once to extract depends; errors are surfaced again inside
    // analyze_source with proper diagnostic formatting.
    if let Ok(items) = parse_source(&entry_source) {
        let depends = module_depends(&items);
        if let Ok(loaded) = crate::source::load_module_tree_from_map(&depends, files) {
            opts = opts.with_loaded_modules(loaded);
        }
    }
    opts = make_opts(opts);
    analyze_source(&entry_source, &opts).to_json()
}

pub fn check_project(files: &HashMap<String, String>, entry: &str) -> String {
    analyze_project(files, entry, |o| o)
}

/// Run analysis plus verify block execution and return the canonical
/// [`AnalysisReport`](crate::diagnostics::AnalysisReport) as JSON. Verify
/// runs only when the source is typecheck-clean; callers see the same
/// mismatch/runtime-error diagnostics as `aver verify`.
pub fn verify_source(source: &str) -> String {
    let mut opts = AnalyzeOptions::new("playground");
    opts.include_verify_run = true;
    analyze_source(source, &opts).to_json()
}

pub fn verify_project(files: &HashMap<String, String>, entry: &str) -> String {
    analyze_project(files, entry, |mut o| {
        o.include_verify_run = true;
        o
    })
}

/// Run verify under `--hostile` mode: typed `given` domains are expanded
/// with the per-type boundary set and each case is multiplied by the
/// adversarial effect-profile cartesian. Diagnostics flagged
/// `from_hostile = true` indicate failures the law would not catch under
/// declared values alone — a missing `when` precondition or an unpinned
/// effect.
pub fn verify_source_hostile(source: &str) -> String {
    let mut opts = AnalyzeOptions::new("playground");
    opts.include_verify_run = true;
    opts.verify_run_hostile = true;
    analyze_source(source, &opts).to_json()
}

pub fn verify_project_hostile(files: &HashMap<String, String>, entry: &str) -> String {
    analyze_project(files, entry, |mut o| {
        o.include_verify_run = true;
        o.verify_run_hostile = true;
        o
    })
}

/// Run analysis plus the file-local "why" summary (per-function
/// justification signals) and return the canonical report as JSON.
pub fn why_source(source: &str) -> String {
    let mut opts = AnalyzeOptions::new("playground");
    opts.include_why_summary = true;
    analyze_source(source, &opts).to_json()
}

pub fn why_project(files: &HashMap<String, String>, entry: &str) -> String {
    analyze_project(files, entry, |mut o| {
        o.include_why_summary = true;
        o
    })
}

/// Run analysis plus the file-local context summary (module shape,
/// functions, types, decisions) and return the canonical report as
/// JSON. Dependency bodies are not expanded — the playground sees the
/// entry file only; `depends` carries names for UI.
pub fn context_source(source: &str) -> String {
    let mut opts = AnalyzeOptions::new("playground");
    opts.include_context_summary = true;
    analyze_source(source, &opts).to_json()
}

pub fn context_project(files: &HashMap<String, String>, entry: &str) -> String {
    analyze_project(files, entry, |mut o| {
        o.include_context_summary = true;
        o
    })
}

/// Render the context as markdown (same shape as CLI
/// `aver context --md`). Source → ContextSummary → markdown, no
/// intermediate serialization.
pub fn context_md_source(source: &str) -> String {
    let mut opts = AnalyzeOptions::new("playground");
    opts.include_context_summary = true;
    let report = analyze_source(source, &opts);
    match report.context_summary {
        Some(summary) => crate::diagnostics::context::render_context_md(&summary),
        None => {
            "# Aver Context\n\n_No context available (parse or typecheck failed)._\n".to_string()
        }
    }
}

pub fn context_md_project(files: &HashMap<String, String>, entry: &str) -> String {
    let Some(entry_source) = files.get(entry).cloned() else {
        return format!(
            "# Aver Context\n\n_Entry '{}' not found in project._\n",
            entry
        );
    };
    let mut opts = AnalyzeOptions::new("playground");
    opts.include_context_summary = true;
    if let Ok(items) = parse_source(&entry_source) {
        let deps = module_depends(&items);
        if let Ok(loaded) = crate::source::load_module_tree_from_map(&deps, files) {
            opts = opts.with_loaded_modules(loaded);
        }
    }
    let report = analyze_source(&entry_source, &opts);
    match report.context_summary {
        Some(summary) => crate::diagnostics::context::render_context_md(&summary),
        None => {
            "# Aver Context\n\n_No context available (parse or typecheck failed)._\n".to_string()
        }
    }
}

/// Audit: three-axis health check — static analysis (every enabled
/// collector), verify block execution, and format-check. Equivalent of
/// the CLI `aver audit` but single-file. Returns a canonical
/// [`AnalysisReport`](crate::diagnostics::AnalysisReport) bundle with
/// diagnostics + verify_summary.
#[cfg(feature = "runtime")]
pub fn audit_source(source: &str) -> String {
    audit_build_report(source, None, None, None, false).to_json()
}

/// Audit under `--hostile` mode — see `verify_source_hostile` for
/// what the hostile expansion adds. The audit panel surfaces the same
/// dual-run pass/fail breakdown as `aver audit --hostile` from the CLI.
#[cfg(feature = "runtime")]
pub fn audit_source_hostile(source: &str) -> String {
    audit_build_report(source, None, None, None, true).to_json()
}

#[cfg(feature = "runtime")]
pub fn audit_project(files: &HashMap<String, String>, entry: &str) -> String {
    let Some(entry_source) = files.get(entry) else {
        return crate::diagnostics::AnalysisReport::new("playground").to_json();
    };
    let loaded = parse_source(entry_source)
        .ok()
        .map(|items| module_depends(&items))
        .and_then(|deps| crate::source::load_module_tree_from_map(&deps, files).ok());
    audit_build_report(entry_source, loaded, Some(files), Some(entry), false).to_json()
}

#[cfg(feature = "runtime")]
pub fn audit_project_hostile(files: &HashMap<String, String>, entry: &str) -> String {
    let Some(entry_source) = files.get(entry) else {
        return crate::diagnostics::AnalysisReport::new("playground").to_json();
    };
    let loaded = parse_source(entry_source)
        .ok()
        .map(|items| module_depends(&items))
        .and_then(|deps| crate::source::load_module_tree_from_map(&deps, files).ok());
    audit_build_report(entry_source, loaded, Some(files), Some(entry), true).to_json()
}

#[cfg(feature = "runtime")]
fn audit_build_report(
    source: &str,
    loaded: Option<Vec<LoadedModule>>,
    all_files: Option<&HashMap<String, String>>,
    entry: Option<&str>,
    hostile: bool,
) -> crate::diagnostics::AnalysisReport {
    use crate::diagnostics::needs_format_diagnostic;

    let mut opts = AnalyzeOptions::new("playground");
    opts.include_verify_run = true;
    opts.verify_run_hostile = hostile;
    if let Some(loaded) = loaded {
        opts = opts.with_loaded_modules(loaded);
    }
    let mut report = analyze_source(source, &opts);

    // Format-check for the entry source (parity with CLI audit).
    #[cfg(feature = "tty-render")]
    if let Ok((formatted, violations)) = crate::format::try_format_source(source)
        && formatted != source
    {
        report
            .diagnostics
            .push(needs_format_diagnostic("playground", &violations, source));
    }

    // Extra pass: format-check every non-entry file in the virtual fs
    // too, so the audit panel's Format section covers the whole
    // project, not just main.av.
    #[cfg(feature = "tty-render")]
    if let (Some(files), Some(entry)) = (all_files, entry) {
        for (path, src) in files {
            if path == entry {
                continue;
            }
            if let Ok((formatted, violations)) = crate::format::try_format_source(src)
                && formatted != *src
            {
                report
                    .diagnostics
                    .push(needs_format_diagnostic(path, &violations, src));
            }
        }
    }

    report
}

/// Format the source and return the rewritten text. Non-mutating by
/// itself — caller (JS) replaces editor contents. Returns the original
/// source unchanged on parse error.
#[cfg(feature = "tty-render")]
pub fn format_source(source: &str) -> String {
    crate::format::try_format_source(source)
        .map(|(text, _violations)| text)
        .unwrap_or_else(|_| source.to_string())
}

// ── Record / replay ────────────────────────────────────────────────
// Runs `fn main` through the in-browser VM, captures every effect
// call as a SessionRecording, and returns the recording as JSON.
// Replay loads such a recording back and checks that the program
// reproduces the same effect trace — byte-for-byte parity with the
// CLI's `aver run --record` / `aver replay`.

#[cfg(feature = "runtime")]
#[derive(serde::Serialize)]
struct RunRecordResult {
    ok: bool,
    recording: Option<String>,
    error: Option<String>,
    effect_count: u32,
    runtime_error: Option<String>,
}

#[cfg(feature = "runtime")]
pub fn run_record_project(files: &HashMap<String, String>, entry: &str) -> String {
    run_record_project_with_entry(files, entry, None)
}

#[cfg(feature = "runtime")]
pub fn run_record_project_with_entry(
    files: &HashMap<String, String>,
    entry: &str,
    entry_expr: Option<&str>,
) -> String {
    let Some(entry_source) = files.get(entry).cloned() else {
        return err_json(format!("Entry '{}' not in virtual fs", entry));
    };
    match run_record_inner(&entry_source, files, entry, entry_expr) {
        Ok(res) => serde_json::to_string(&res).unwrap_or_else(|_| "{}".to_string()),
        Err(e) => err_json(e),
    }
}

#[cfg(feature = "runtime")]
fn run_record_inner(
    entry_source: &str,
    files: &HashMap<String, String>,
    entry: &str,
    entry_expr: Option<&str>,
) -> Result<RunRecordResult, String> {
    use crate::replay::json::JsonValue;
    use crate::replay::session::{RecordedOutcome, SessionRecording};
    use crate::replay::{
        encode_entry_args, parse_entry_call, session_recording_to_string_pretty,
        value_to_json_lossy,
    };

    let (mut items, loaded) = parse_and_load(entry_source, files, entry)?;
    tco::transform_program(&mut items);

    // Type-check first so we surface nice errors instead of a VM
    // panic on missing symbols.
    let tc_result = run_type_check_with_loaded(&items, &loaded);
    if !tc_result.errors.is_empty() {
        return Err(format_tc_errors(&tc_result.errors));
    }

    resolver::resolve_program(&mut items);

    let mut arena = Arena::new();
    vm::register_service_types(&mut arena);
    let (code, globals) = vm::compile_program_with_loaded_modules(&items, &mut arena, loaded, "")
        .map_err(|e| format!("Compile error: {}", e.msg))?;

    let mut machine = vm::VM::new(code, globals, arena);
    machine.set_silent_console(true);
    // Safety net — a game with no quit path (Terminal.readKey always
    // returning None under the stubs) would otherwise loop forever on
    // the wasm main thread. 10k effects is way above any sensible
    // real program and short-circuits to a clear error fast.
    machine.set_record_cap(Some(10_000));
    machine.start_recording();

    // Resolve the entry: either a user-supplied call expression or `main`.
    let entry_info: Option<(String, Vec<crate::value::Value>)> = match entry_expr {
        Some(src) => Some(parse_entry_call(src)?),
        None => None,
    };

    let mut runtime_error: Option<String> = None;
    let run_result = if let Some((fn_name, args)) = &entry_info {
        machine.run_top_level().and_then(|_| {
            use crate::nan_value::{NanValue, NanValueConvert};
            let nv_args: Vec<NanValue> = args
                .iter()
                .map(|v| NanValue::from_value(v, &mut machine.arena))
                .collect();
            machine.run_named_function(fn_name, &nv_args)
        })
    } else {
        machine.run()
    };

    let output = match run_result {
        Ok(val) => {
            // Recorded outcome is only used by replay-validation; the
            // browser usually drives Unit out of main, so a lossy
            // representation is fine. If a program returns a rich
            // value we'll fall back to its `aver_repr`.
            use crate::nan_value::NanValueConvert;
            let value = val.to_value(&machine.arena);
            RecordedOutcome::Value(value_to_json_lossy(&value))
        }
        Err(e) => {
            let msg = e.to_string();
            runtime_error = Some(msg.clone());
            RecordedOutcome::RuntimeError(msg)
        }
    };

    let (entry_fn, input_json) = match &entry_info {
        Some((name, args)) => (
            name.clone(),
            encode_entry_args(args).unwrap_or(JsonValue::Null),
        ),
        None => ("main".to_string(), JsonValue::Null),
    };

    let recording = SessionRecording {
        schema_version: 1,
        request_id: "playground".to_string(),
        timestamp: String::new(),
        program_file: entry.to_string(),
        module_root: "<virtual-fs>".to_string(),
        entry_fn,
        input: input_json,
        effects: machine.recorded_effects().to_vec(),
        output,
    };

    Ok(RunRecordResult {
        ok: true,
        effect_count: recording.effects.len() as u32,
        recording: Some(session_recording_to_string_pretty(&recording)),
        error: None,
        runtime_error,
    })
}

#[cfg(feature = "runtime")]
#[derive(serde::Serialize)]
struct ReplayResult {
    ok: bool,
    matched: bool,
    replayed: u32,
    total: u32,
    error: Option<String>,
}

#[cfg(feature = "runtime")]
pub fn replay_run_project(
    files: &HashMap<String, String>,
    entry: &str,
    recording_json: &str,
) -> String {
    match replay_run_inner(files, entry, recording_json) {
        Ok(r) => serde_json::to_string(&r).unwrap_or_else(|_| "{}".to_string()),
        Err(e) => serde_json::to_string(&ReplayResult {
            ok: false,
            matched: false,
            replayed: 0,
            total: 0,
            error: Some(e),
        })
        .unwrap_or_else(|_| "{}".to_string()),
    }
}

#[cfg(feature = "runtime")]
fn replay_run_inner(
    files: &HashMap<String, String>,
    entry: &str,
    recording_json: &str,
) -> Result<ReplayResult, String> {
    use crate::nan_value::{NanValue, NanValueConvert};
    use crate::replay::json::JsonValue;
    use crate::replay::json_to_value;
    use crate::replay::session::parse_session_recording;
    use crate::value::{Value, list_to_vec};

    let Some(entry_source) = files.get(entry).cloned() else {
        return Err(format!("Entry '{}' not in virtual fs", entry));
    };
    let recording = parse_session_recording(recording_json)?;
    let total = recording.effects.len() as u32;

    let (mut items, loaded) = parse_and_load(&entry_source, files, entry)?;
    tco::transform_program(&mut items);

    let tc_result = run_type_check_with_loaded(&items, &loaded);
    if !tc_result.errors.is_empty() {
        return Err(format_tc_errors(&tc_result.errors));
    }

    resolver::resolve_program(&mut items);

    let mut arena = Arena::new();
    vm::register_service_types(&mut arena);
    let (code, globals) = vm::compile_program_with_loaded_modules(&items, &mut arena, loaded, "")
        .map_err(|e| format!("Compile error: {}", e.msg))?;

    let mut machine = vm::VM::new(code, globals, arena);
    machine.set_silent_console(true);
    machine.start_replay(recording.effects, true);

    // Replay respects the recording's own entry_fn and input, not just main —
    // otherwise playground replay would miscompare recordings made with a
    // custom entry point.
    let run_err = if recording.entry_fn == "main" && matches!(recording.input, JsonValue::Null) {
        machine.run().err().map(|e| e.to_string())
    } else {
        let top_err = machine.run_top_level().err().map(|e| e.to_string());
        if let Some(err) = top_err {
            Some(err)
        } else {
            let args: Vec<Value> = match json_to_value(&recording.input) {
                Ok(Value::Unit) => vec![],
                Ok(v) => list_to_vec(&v).unwrap_or_else(|| vec![v]),
                Err(e) => return Err(e),
            };
            let nv_args: Vec<NanValue> = args
                .iter()
                .map(|v| NanValue::from_value(v, &mut machine.arena))
                .collect();
            machine
                .run_named_function(&recording.entry_fn, &nv_args)
                .err()
                .map(|e| e.to_string())
        }
    };
    let consumed = machine.ensure_replay_consumed();
    let (replayed, _remaining) = machine.replay_progress();

    let error = run_err.or_else(|| consumed.err().map(|e| e.to_string()));

    Ok(ReplayResult {
        ok: true,
        matched: error.is_none() && replayed as u32 == total,
        replayed: replayed as u32,
        total,
        error,
    })
}

#[cfg(feature = "runtime")]
fn parse_and_load(
    entry_source: &str,
    files: &HashMap<String, String>,
    _entry: &str,
) -> Result<(Vec<TopLevel>, Vec<LoadedModule>), String> {
    let items = parse_source(entry_source)?;
    let depends = module_depends(&items);
    let loaded = load_module_tree_from_map(&depends, files)?;
    Ok((items, loaded))
}

#[cfg(feature = "runtime")]
fn err_json(msg: String) -> String {
    serde_json::to_string(&RunRecordResult {
        ok: false,
        recording: None,
        error: Some(msg),
        effect_count: 0,
        runtime_error: None,
    })
    .unwrap_or_else(|_| "{}".to_string())
}

#[cfg(feature = "playground")]
mod bindgen {
    use wasm_bindgen::prelude::*;

    // Route Rust panics to console.error via a one-shot hook so the
    // browser console shows the real message instead of "unreachable
    // executed". Installed lazily at the first binding entry; cheap
    // if called repeatedly (Once guard).
    #[wasm_bindgen]
    extern "C" {
        #[wasm_bindgen(js_namespace = console, js_name = error)]
        fn console_error(s: &str);
    }

    // Called automatically by wasm-bindgen when the module boots
    // (`await mod.default(...)` in JS). Routes Rust panics to the
    // browser's console.error so wasm traps ("unreachable executed")
    // carry the real panic message instead of a generic stub.
    #[wasm_bindgen(start)]
    pub fn init_playground() {
        std::panic::set_hook(Box::new(|info| {
            console_error(&format!("Aver playground panic: {}", info));
        }));
    }

    #[wasm_bindgen]
    pub fn aver_compile(source: &str) -> Result<Vec<u8>, JsError> {
        super::compile_to_wasm(source).map_err(|e| JsError::new(&e))
    }

    /// Compile a multi-file project. `files_json` is a JSON object
    /// mapping path -> source (e.g. `{"types.av": "...", "main.av":
    /// "..."}`). `entry` is the key of the entry file.
    #[wasm_bindgen]
    pub fn aver_compile_project(files_json: &str, entry: &str) -> Result<Vec<u8>, JsError> {
        let files: std::collections::HashMap<String, String> =
            serde_json::from_str(files_json).map_err(|e| JsError::new(&e.to_string()))?;
        super::compile_project_to_wasm(&files, entry).map_err(|e| JsError::new(&e))
    }

    #[wasm_bindgen]
    pub fn aver_check(source: &str) -> String {
        super::check_source(source)
    }

    #[wasm_bindgen]
    pub fn aver_verify(source: &str) -> String {
        super::verify_source(source)
    }

    /// Verify under `--hostile` mode: per-type boundary set substituted
    /// into typed `given` clauses, plus per-classified-effect adversarial
    /// profile cartesian. The returned report carries `from_hostile = true`
    /// on cases that would not surface under `aver_verify`.
    #[wasm_bindgen]
    pub fn aver_verify_hostile(source: &str) -> String {
        super::verify_source_hostile(source)
    }

    #[wasm_bindgen]
    pub fn aver_why(source: &str) -> String {
        super::why_source(source)
    }

    #[wasm_bindgen]
    pub fn aver_context(source: &str) -> String {
        super::context_source(source)
    }

    #[wasm_bindgen]
    pub fn aver_audit(source: &str) -> String {
        super::audit_source(source)
    }

    #[wasm_bindgen]
    pub fn aver_audit_hostile(source: &str) -> String {
        super::audit_source_hostile(source)
    }

    #[wasm_bindgen]
    pub fn aver_format(source: &str) -> String {
        super::format_source(source)
    }

    /// Aver source → Lean 4 project files (JSON `{path: content}`).
    /// JS zips the result in the browser. Maps to `aver proof
    /// --backend lean` on the CLI.
    #[wasm_bindgen]
    pub fn aver_proof_lean(source: &str) -> Result<String, JsError> {
        let files = super::proof_lean_files(source).map_err(|e| JsError::new(&e))?;
        serde_json::to_string(&files).map_err(|e| JsError::new(&e.to_string()))
    }

    /// Aver source → Dafny project files (JSON `{path: content}`).
    /// Maps to `aver proof --backend dafny` on the CLI.
    #[wasm_bindgen]
    pub fn aver_proof_dafny(source: &str) -> Result<String, JsError> {
        let files = super::proof_dafny_files(source).map_err(|e| JsError::new(&e))?;
        serde_json::to_string(&files).map_err(|e| JsError::new(&e.to_string()))
    }

    /// Aver source → Rust/Cargo project files (JSON `{path: content}`).
    /// Maps to `aver compile --target rust` on the CLI.
    #[wasm_bindgen]
    pub fn aver_compile_rust(source: &str) -> Result<String, JsError> {
        let files = super::compile_rust_files(source).map_err(|e| JsError::new(&e))?;
        serde_json::to_string(&files).map_err(|e| JsError::new(&e.to_string()))
    }

    /// Multi-file Aver project → Lean 4 project files (JSON).
    #[wasm_bindgen]
    pub fn aver_proof_lean_project(files_json: &str, entry: &str) -> Result<String, JsError> {
        let files: std::collections::HashMap<String, String> =
            serde_json::from_str(files_json).map_err(|e| JsError::new(&e.to_string()))?;
        let out = super::proof_lean_files_project(&files, entry).map_err(|e| JsError::new(&e))?;
        serde_json::to_string(&out).map_err(|e| JsError::new(&e.to_string()))
    }

    /// Multi-file Aver project → Dafny project files (JSON).
    #[wasm_bindgen]
    pub fn aver_proof_dafny_project(files_json: &str, entry: &str) -> Result<String, JsError> {
        let files: std::collections::HashMap<String, String> =
            serde_json::from_str(files_json).map_err(|e| JsError::new(&e.to_string()))?;
        let out = super::proof_dafny_files_project(&files, entry).map_err(|e| JsError::new(&e))?;
        serde_json::to_string(&out).map_err(|e| JsError::new(&e.to_string()))
    }

    /// Multi-file Aver project → Rust/Cargo project files (JSON).
    #[wasm_bindgen]
    pub fn aver_compile_rust_project(files_json: &str, entry: &str) -> Result<String, JsError> {
        let files: std::collections::HashMap<String, String> =
            serde_json::from_str(files_json).map_err(|e| JsError::new(&e.to_string()))?;
        let out = super::compile_rust_files_project(&files, entry).map_err(|e| JsError::new(&e))?;
        serde_json::to_string(&out).map_err(|e| JsError::new(&e.to_string()))
    }

    // ── Project (multi-file) analysis bindings ─────────────────────
    // Same semantics as the single-file siblings above, but deps
    // referenced via `depends [...]` resolve against the supplied
    // virtual fs (JSON path → source map) instead of failing with
    // "Unknown identifier".

    fn parse_files(files_json: &str) -> Result<std::collections::HashMap<String, String>, JsError> {
        serde_json::from_str(files_json).map_err(|e| JsError::new(&e.to_string()))
    }

    #[wasm_bindgen]
    pub fn aver_check_project(files_json: &str, entry: &str) -> Result<String, JsError> {
        let files = parse_files(files_json)?;
        Ok(super::check_project(&files, entry))
    }

    #[wasm_bindgen]
    pub fn aver_verify_project(files_json: &str, entry: &str) -> Result<String, JsError> {
        let files = parse_files(files_json)?;
        Ok(super::verify_project(&files, entry))
    }

    #[wasm_bindgen]
    pub fn aver_verify_hostile_project(
        files_json: &str,
        entry: &str,
    ) -> Result<String, JsError> {
        let files = parse_files(files_json)?;
        Ok(super::verify_project_hostile(&files, entry))
    }

    #[wasm_bindgen]
    pub fn aver_why_project(files_json: &str, entry: &str) -> Result<String, JsError> {
        let files = parse_files(files_json)?;
        Ok(super::why_project(&files, entry))
    }

    #[wasm_bindgen]
    pub fn aver_context_project(files_json: &str, entry: &str) -> Result<String, JsError> {
        let files = parse_files(files_json)?;
        Ok(super::context_project(&files, entry))
    }

    #[wasm_bindgen]
    pub fn aver_context_md(source: &str) -> String {
        super::context_md_source(source)
    }

    #[wasm_bindgen]
    pub fn aver_context_md_project(files_json: &str, entry: &str) -> Result<String, JsError> {
        let files = parse_files(files_json)?;
        Ok(super::context_md_project(&files, entry))
    }

    #[wasm_bindgen]
    pub fn aver_audit_project(files_json: &str, entry: &str) -> Result<String, JsError> {
        let files = parse_files(files_json)?;
        Ok(super::audit_project(&files, entry))
    }

    #[wasm_bindgen]
    pub fn aver_audit_hostile_project(
        files_json: &str,
        entry: &str,
    ) -> Result<String, JsError> {
        let files = parse_files(files_json)?;
        Ok(super::audit_project_hostile(&files, entry))
    }

    // ── Record / replay bindings ───────────────────────────────────
    // Project-shaped on purpose: single-file callers pass
    // `{"playground.av": source}` with entry "playground.av".

    #[wasm_bindgen]
    pub fn aver_run_record(files_json: &str, entry: &str) -> Result<String, JsError> {
        let files = parse_files(files_json)?;
        Ok(super::run_record_project(&files, entry))
    }

    /// Record a run starting from an arbitrary call expression instead of
    /// `main`. `entry_expr` must be a function call with literal arguments
    /// (String / Int / Float / Bool / Unit) — same constraints as `aver run
    /// --expr` on the CLI. The resulting recording has `entry_fn` and
    /// `input` populated accordingly and can be replayed unchanged.
    #[wasm_bindgen]
    pub fn aver_run_record_entry(
        files_json: &str,
        entry: &str,
        entry_expr: &str,
    ) -> Result<String, JsError> {
        let files = parse_files(files_json)?;
        Ok(super::run_record_project_with_entry(
            &files,
            entry,
            Some(entry_expr),
        ))
    }

    #[wasm_bindgen]
    pub fn aver_replay_run(
        files_json: &str,
        entry: &str,
        recording_json: &str,
    ) -> Result<String, JsError> {
        let files = parse_files(files_json)?;
        Ok(super::replay_run_project(&files, entry, recording_json))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    fn read(path: &str) -> String {
        std::fs::read_to_string(path).unwrap_or_else(|_| panic!("missing {}", path))
    }

    fn load_rogue_files() -> HashMap<String, String> {
        let root = "tools/website/playground/sources/examples/games/rogue";
        let mut files: HashMap<String, String> = HashMap::new();
        for f in [
            "types",
            "map",
            "fov",
            "pathfinding",
            "combat",
            "render",
            "main",
        ] {
            files.insert(format!("{}.av", f), read(&format!("{}/{}.av", root, f)));
        }
        files
    }

    #[test]
    fn proof_lean_emits_files_for_simple_source() {
        let src = "module M\n    intent = \"t\"\n\n\
                   fn add(a: Int, b: Int) -> Int\n    a + b\n\n\
                   verify add\n    add(2, 3) => 5\n";
        let files = proof_lean_files(src).expect("lean files");
        assert!(!files.is_empty(), "Lean export should produce files");
        let any_lean_with_add = files.iter().any(|(k, v)| {
            k.ends_with(".lean")
                && k != "lakefile.lean"
                && (v.contains("def add") || v.contains("add ("))
        });
        assert!(
            any_lean_with_add,
            "generated Lean should mention `add` somewhere; files: {:?}",
            files.keys().collect::<Vec<_>>()
        );
    }

    #[test]
    fn proof_dafny_emits_files_for_simple_source() {
        let src = "module M\n    intent = \"t\"\n\n\
                   fn add(a: Int, b: Int) -> Int\n    a + b\n\n\
                   verify add\n    add(2, 3) => 5\n";
        let files = proof_dafny_files(src).expect("dafny files");
        assert!(!files.is_empty(), "Dafny export should produce files");
        assert!(
            files.iter().any(|(k, _)| k.ends_with(".dfy")),
            "should include a .dfy"
        );
    }

    #[test]
    fn compile_rust_emits_cargo_project() {
        let src = "module M\n    intent = \"t\"\n\n\
                   fn add(a: Int, b: Int) -> Int\n    a + b\n\n\
                   fn main() -> Unit\n    ! [Console.print]\n    Console.print(\"ok\")\n";
        let files = compile_rust_files(src).expect("rust files");
        assert!(
            files.contains_key("Cargo.toml"),
            "Rust export should include Cargo.toml"
        );
        assert!(
            files
                .iter()
                .any(|(k, _)| k.starts_with("src/") && k.ends_with(".rs")),
            "Rust export should include at least one src/*.rs file"
        );
    }

    #[test]
    fn proof_lean_project_handles_multi_file() {
        let files = load_rogue_files();
        let out = proof_lean_files_project(&files, "main.av")
            .expect("multi-file Lean export should succeed");
        assert!(!out.is_empty(), "Lean project export should produce files");
        assert!(out.iter().any(|(k, _)| k.ends_with(".lean")));
    }

    #[test]
    fn proof_dafny_project_handles_multi_file() {
        let files = load_rogue_files();
        let out = proof_dafny_files_project(&files, "main.av")
            .expect("multi-file Dafny export should succeed");
        assert!(!out.is_empty(), "Dafny project export should produce files");
        assert!(out.iter().any(|(k, _)| k.ends_with(".dfy")));
    }

    #[test]
    fn compile_rust_project_handles_multi_file() {
        let files = load_rogue_files();
        let out = compile_rust_files_project(&files, "main.av")
            .expect("multi-file Rust export should succeed");
        assert!(out.contains_key("Cargo.toml"));
    }

    #[test]
    fn compiles_multi_file_rogue_from_virtual_fs() {
        let files = load_rogue_files();
        let bytes = compile_project_to_wasm(&files, "main.av")
            .expect("rogue project should compile from virtual fs");
        assert!(
            bytes.len() > 1000,
            "emitted wasm looks too small: {}",
            bytes.len()
        );
    }

    // Only meaningful when `terminal` feature is off — otherwise the
    // real crossterm impl drives the effects and can fail outside a
    // TTY (common in CI). The playground (wasm32-unknown-unknown)
    // always ships without `terminal`, which is exactly this path.
    #[cfg(not(feature = "terminal"))]
    #[test]
    fn records_terminal_effects_in_playground_build() {
        // Snake uses Terminal.* extensively. In the playground build
        // (no `terminal` feature → crossterm unavailable) the stubs in
        // vm/builtin.rs should let the VM record each call with a Unit
        // outcome instead of surfacing "not available in this build".
        let mut files = HashMap::new();
        files.insert(
            "playground.av".to_string(),
            [
                "module Main",
                "    intent = \"terminal smoke\"",
                "",
                "fn main() -> Unit",
                "    ! [Terminal.enableRawMode, Terminal.clear, Terminal.disableRawMode]",
                "    Terminal.enableRawMode()",
                "    Terminal.clear()",
                "    Terminal.disableRawMode()",
                "",
            ]
            .join("\n"),
        );
        let record: serde_json::Value =
            serde_json::from_str(&run_record_project(&files, "playground.av")).unwrap();
        assert_eq!(
            record["ok"], true,
            "should record terminal stubs: {}",
            record
        );
        assert_eq!(record["effect_count"], 3, "three terminal calls");
        assert!(
            record["runtime_error"].is_null(),
            "terminal stubs shouldn't raise: {}",
            record["runtime_error"]
        );

        let replay: serde_json::Value = serde_json::from_str(&replay_run_project(
            &files,
            "playground.av",
            record["recording"].as_str().unwrap(),
        ))
        .unwrap();
        assert_eq!(replay["matched"], true, "replay should match: {}", replay);
    }

    #[test]
    fn run_record_captures_effects_then_replays_clean() {
        let mut files = HashMap::new();
        files.insert(
            "playground.av".to_string(),
            [
                "module Main",
                "    intent = \"record/replay smoke\"",
                "",
                "fn main() -> Unit",
                "    ! [Console.print]",
                "    Console.print(\"hello\")",
                "    Console.print(\"world\")",
                "",
            ]
            .join("\n"),
        );

        let record_json = run_record_project(&files, "playground.av");
        let record: serde_json::Value = serde_json::from_str(&record_json).unwrap();
        assert_eq!(record["ok"], true, "record should succeed: {}", record_json);
        assert_eq!(record["effect_count"], 2, "two Console.print calls");
        let recording_str = record["recording"].as_str().expect("recording string");

        let replay_json = replay_run_project(&files, "playground.av", recording_str);
        let replay: serde_json::Value = serde_json::from_str(&replay_json).unwrap();
        assert_eq!(replay["ok"], true);
        assert_eq!(
            replay["matched"], true,
            "replay should match captured effects: {}",
            replay_json
        );
        assert_eq!(replay["replayed"], 2);
        assert_eq!(replay["total"], 2);
    }

    #[test]
    fn multi_file_check_has_no_unknown_ident_noise() {
        let files = load_rogue_files();
        let report: serde_json::Value =
            serde_json::from_str(&check_project(&files, "main.av")).unwrap();
        let diagnostics = report["diagnostics"]
            .as_array()
            .cloned()
            .unwrap_or_default();
        let unknown_ident_on_deps: Vec<_> = diagnostics
            .iter()
            .filter(|d| d["slug"] == "unknown-ident")
            .filter(|d| {
                let s = d["summary"].as_str().unwrap_or("");
                ["Types", "Map", "Fov", "Combat", "Render", "Pathfinding"]
                    .iter()
                    .any(|name| s.contains(&format!("'{}'", name)))
            })
            .collect();
        assert!(
            unknown_ident_on_deps.is_empty(),
            "multi-file check still reports unknown-ident for declared deps: {:?}",
            unknown_ident_on_deps
        );
    }

    #[test]
    fn reports_missing_dep_clearly() {
        let mut files = HashMap::new();
        files.insert(
            "main.av".to_string(),
            [
                "module Main",
                "    intent = \"demo\"",
                "    depends [Missing]",
                "",
                "fn main() -> Unit",
                "    ! [Console.print]",
                "    Console.print(\"hi\")",
                "",
            ]
            .join("\n"),
        );
        let err = compile_project_to_wasm(&files, "main.av").unwrap_err();
        assert!(
            err.contains("Missing") || err.contains("not found"),
            "expected missing-module error, got: {}",
            err
        );
    }
}

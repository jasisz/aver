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

    let modules: Vec<codegen::ModuleInfo> = loaded
        .into_iter()
        .map(|m| loaded_to_module_info(m))
        .collect();

    let ctx = codegen::build_context(
        entry_items,
        &tc_result,
        HashSet::new(),
        "playground".to_string(),
        modules,
    );
    codegen::wasm::emit_wasm(&ctx)
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

/// Audit: three-axis health check — static analysis (every enabled
/// collector), verify block execution, and format-check. Equivalent of
/// the CLI `aver audit` but single-file. Returns a canonical
/// [`AnalysisReport`](crate::diagnostics::AnalysisReport) bundle with
/// diagnostics + verify_summary.
#[cfg(feature = "runtime")]
pub fn audit_source(source: &str) -> String {
    audit_build_report(source, None, None, None).to_json()
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
    audit_build_report(entry_source, loaded, Some(files), Some(entry)).to_json()
}

#[cfg(feature = "runtime")]
fn audit_build_report(
    source: &str,
    loaded: Option<Vec<LoadedModule>>,
    all_files: Option<&HashMap<String, String>>,
    entry: Option<&str>,
) -> crate::diagnostics::AnalysisReport {
    use crate::diagnostics::needs_format_diagnostic;

    let mut opts = AnalyzeOptions::new("playground");
    opts.include_verify_run = true;
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
    let Some(entry_source) = files.get(entry).cloned() else {
        return err_json(format!("Entry '{}' not in virtual fs", entry));
    };
    match run_record_inner(&entry_source, files, entry) {
        Ok(res) => serde_json::to_string(&res).unwrap_or_else(|_| "{}".to_string()),
        Err(e) => err_json(e),
    }
}

#[cfg(feature = "runtime")]
fn run_record_inner(
    entry_source: &str,
    files: &HashMap<String, String>,
    entry: &str,
) -> Result<RunRecordResult, String> {
    use crate::replay::json::JsonValue;
    use crate::replay::session::{RecordedOutcome, SessionRecording};
    use crate::replay::{session_recording_to_string_pretty, value_to_json_lossy};

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

    let mut runtime_error: Option<String> = None;
    let output = match machine.run() {
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

    let recording = SessionRecording {
        schema_version: 1,
        request_id: "playground".to_string(),
        timestamp: String::new(),
        program_file: entry.to_string(),
        module_root: "<virtual-fs>".to_string(),
        entry_fn: "main".to_string(),
        input: JsonValue::Null,
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
    use crate::replay::session::parse_session_recording;

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

    let run_err = machine.run().err().map(|e| e.to_string());
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
    pub fn aver_format(source: &str) -> String {
        super::format_source(source)
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
    pub fn aver_audit_project(files_json: &str, entry: &str) -> Result<String, JsError> {
        let files = parse_files(files_json)?;
        Ok(super::audit_project(&files, entry))
    }

    // ── Record / replay bindings ───────────────────────────────────
    // Project-shaped on purpose: single-file callers pass
    // `{"playground.av": source}` with entry "playground.av".

    #[wasm_bindgen]
    pub fn aver_run_record(files_json: &str, entry: &str) -> Result<String, JsError> {
        let files = parse_files(files_json)?;
        Ok(super::run_record_project(&files, entry))
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

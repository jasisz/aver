//! Browser-facing entry points for the Aver playground.

use std::collections::{HashMap, HashSet};

use crate::ast::TopLevel;
use crate::codegen;
use crate::diagnostics::{AnalyzeOptions, analyze_source};
use crate::resolver;
use crate::source::{LoadedModule, load_module_tree_from_map, parse_source};
use crate::tco;
use crate::types::checker::{run_type_check_full, run_type_check_with_loaded};

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
        report.diagnostics.push(needs_format_diagnostic(
            "playground",
            &violations,
            source,
        ));
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

#[cfg(feature = "playground")]
mod bindgen {
    use wasm_bindgen::prelude::*;

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
        for f in ["types", "map", "fov", "pathfinding", "combat", "render", "main"] {
            files.insert(format!("{}.av", f), read(&format!("{}/{}.av", root, f)));
        }
        files
    }

    #[test]
    fn compiles_multi_file_rogue_from_virtual_fs() {
        let files = load_rogue_files();
        let bytes = compile_project_to_wasm(&files, "main.av")
            .expect("rogue project should compile from virtual fs");
        assert!(bytes.len() > 1000, "emitted wasm looks too small: {}", bytes.len());
    }

    #[test]
    fn multi_file_check_has_no_unknown_ident_noise() {
        let files = load_rogue_files();
        let report: serde_json::Value =
            serde_json::from_str(&check_project(&files, "main.av")).unwrap();
        let diagnostics = report["diagnostics"].as_array().cloned().unwrap_or_default();
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
            ].join("\n"),
        );
        let err = compile_project_to_wasm(&files, "main.av").unwrap_err();
        assert!(
            err.contains("Missing") || err.contains("not found"),
            "expected missing-module error, got: {}",
            err
        );
    }
}

use std::fs;
use std::io::Read;

use colored::Colorize;

use aver::ast::TopLevel;
use aver::source::parse_source;
use aver::types::checker::TypeError;
use aver::value::Value;
use aver::vm;

pub(super) fn read_file(path: &str) -> Result<String, String> {
    fs::read_to_string(path).map_err(|e| format!("Cannot open file '{}': {}", path, e))
}

pub(super) fn parse_file(source: &str) -> Result<Vec<TopLevel>, String> {
    parse_source(source)
}

pub(super) fn resolve_module_root(module_root: Option<&str>) -> String {
    if let Some(root) = module_root {
        return root.to_string();
    }
    std::env::current_dir()
        .ok()
        .and_then(|p| p.into_os_string().into_string().ok())
        .unwrap_or_else(|| ".".to_string())
}

/// Resolve `module_root` for a given entry file. Honours an explicit
/// override; otherwise walks up from the entry file's parent directory
/// until every dependency declared by the entry file's `depends [...]`
/// list resolves on disk. Falls back to the entry file's parent dir
/// when no `depends` block is present (single-module files), and
/// finally to the cwd if every walked-up candidate misses.
pub(super) fn resolve_module_root_for_entry(file: &str, override_: Option<&str>) -> String {
    if let Some(root) = override_ {
        return root.to_string();
    }
    let entry = std::path::Path::new(file);
    let parent = entry
        .parent()
        .and_then(|p| if p.as_os_str().is_empty() { None } else { Some(p) })
        .unwrap_or_else(|| std::path::Path::new("."));

    // Read & parse the entry to learn its declared deps. Skip if the
    // file can't be read; the caller's regular error path will surface
    // the same issue with a clearer message.
    let deps: Vec<String> = match std::fs::read_to_string(file) {
        Ok(src) => match parse_source(&src) {
            Ok(items) => collect_entry_deps(&items),
            Err(_) => Vec::new(),
        },
        Err(_) => Vec::new(),
    };

    if deps.is_empty() {
        return parent.to_string_lossy().into_owned();
    }

    // Walk parent dirs from the file outward, stopping at the first
    // directory where every declared dep resolves to an .av file.
    let mut candidate = Some(parent);
    while let Some(dir) = candidate {
        let dir_str = dir.to_string_lossy();
        let all_ok = deps.iter().all(|dep| {
            aver::source::find_module_file(dep, dir_str.as_ref()).is_some()
        });
        if all_ok {
            return dir_str.into_owned();
        }
        candidate = dir.parent();
    }
    parent.to_string_lossy().into_owned()
}

fn collect_entry_deps(items: &[TopLevel]) -> Vec<String> {
    use aver::ast::TopLevel as TL;
    let mut out = Vec::new();
    for item in items {
        if let TL::Module(decl) = item {
            for dep in &decl.depends {
                out.push(dep.clone());
            }
        }
    }
    out
}

pub(super) fn load_runtime_policy(
    module_root: &str,
) -> Result<Option<aver::config::ProjectConfig>, String> {
    aver::config::ProjectConfig::load_from_dir(std::path::Path::new(module_root))
        .map_err(|e| format!("aver.toml: {}", e))
}

pub(super) fn apply_runtime_policy_to_vm(
    machine: &mut vm::VM,
    module_root: &str,
) -> Result<(), String> {
    if let Some(config) = load_runtime_policy(module_root)? {
        machine.set_runtime_policy(config);
    }
    Ok(())
}

pub(super) fn print_type_errors(errors: &[TypeError]) {
    for te in errors {
        eprintln!(
            "{}",
            format!("error[{}:{}]: {}", te.line, te.col, te.message).red()
        );
    }
}

// `compute_memo_fns` and `is_memo_safe_type` moved to
// `aver::diagnostics::context` so the playground/LSP share one
// implementation. Re-exported for the rest of `src/main/` that still
// imports them from `super::shared`.
pub(super) use aver::diagnostics::context::compute_memo_fns;

pub(super) fn format_type_errors(errors: &[TypeError]) -> String {
    let mut out = Vec::new();
    for te in errors {
        out.push(format!("error[{}:{}]: {}", te.line, te.col, te.message));
    }
    out.join("\n")
}

/// Collect entry expressions from `--expr` flags or `--input-file`.
/// Input file path `-` reads from stdin.
/// Returns empty list if neither is provided (caller runs `main`).
pub(super) fn collect_entry_expressions(
    exprs: &[String],
    input_file: Option<&str>,
) -> Result<Vec<String>, String> {
    if let Some(path) = input_file {
        let content = if path == "-" {
            let mut buf = String::new();
            std::io::stdin()
                .read_to_string(&mut buf)
                .map_err(|e| format!("Cannot read stdin: {}", e))?;
            buf
        } else {
            fs::read_to_string(path)
                .map_err(|e| format!("Cannot read input file '{}': {}", path, e))?
        };
        let trimmed = content.trim().to_string();
        if trimmed.is_empty() {
            return Err(format!("Input file '{}' is empty", path));
        }
        return Ok(vec![trimmed]);
    }
    Ok(exprs.to_vec())
}

/// Parse a CLI `--expr` argument. Delegates to `aver::replay::parse_entry_call`.
pub(super) fn parse_call_expression(src: &str) -> Result<(String, Vec<Value>), String> {
    aver::replay::parse_entry_call(src)
}

/// Serialise entry-call arguments into the replay schema's `input` field.
pub(super) fn encode_entry_args_json(args: &[Value]) -> Result<aver::replay::JsonValue, String> {
    aver::replay::encode_entry_args(args)
}

/// Derive a readable filename stem from an entry call.
pub(super) fn entry_recording_stem(fn_name: &str, args: &[Value]) -> String {
    aver::replay::recording_stem(fn_name, args)
}

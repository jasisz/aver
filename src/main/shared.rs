use std::collections::HashSet;
use std::fs;

use colored::Colorize;

use aver::ast::TopLevel;
use aver::call_graph::{find_recursive_fns, recursive_callsite_counts};
use aver::source::parse_source;
use aver::types;
use aver::types::checker::{TypeCheckResult, TypeError};
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

/// Determine which functions qualify for auto-memoization:
/// pure (no effects), recursive, branchy recursion (>1 recursive callsite),
/// where callsites are counted syntactically within the caller's recursive SCC,
/// and all parameters are memo-safe types.
pub(super) fn compute_memo_fns(items: &[TopLevel], tc_result: &TypeCheckResult) -> HashSet<String> {
    let recursive = find_recursive_fns(items);
    let recursive_calls = recursive_callsite_counts(items);
    let mut memo = HashSet::new();

    for fn_name in &recursive {
        if let Some((params, _ret, effects)) = tc_result.fn_sigs.get(fn_name) {
            // Must be pure (no effects)
            if !effects.is_empty() {
                continue;
            }
            // Must have branching recursion where memoization can collapse overlap.
            if recursive_calls.get(fn_name).copied().unwrap_or(0) < 2 {
                continue;
            }
            // All params must be memo-safe
            let all_safe = params
                .iter()
                .all(|ty| is_memo_safe_type(ty, &tc_result.memo_safe_types));
            if all_safe {
                memo.insert(fn_name.clone());
            }
        }
    }
    memo
}

pub(super) fn is_memo_safe_type(ty: &types::Type, safe_named: &HashSet<String>) -> bool {
    use aver::types::Type;
    match ty {
        // String stays excluded for now: memo keys hash String content,
        // so string-heavy recursion can degrade to O(n) keying work.
        Type::Int | Type::Float | Type::Bool | Type::Unit => true,
        Type::Str => false,
        Type::Tuple(items) => items.iter().all(|item| is_memo_safe_type(item, safe_named)),
        Type::List(_) | Type::Vector(_) | Type::Map(_, _) | Type::Fn(_, _, _) | Type::Unknown => {
            false
        }
        Type::Result(_, _) | Type::Option(_) => false,
        Type::Named(name) => safe_named.contains(name),
    }
}

pub(super) fn format_type_errors(errors: &[TypeError]) -> String {
    let mut out = Vec::new();
    for te in errors {
        out.push(format!("error[{}:{}]: {}", te.line, te.col, te.message));
    }
    out.join("\n")
}

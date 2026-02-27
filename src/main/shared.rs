use std::collections::HashSet;
use std::fs;

use colored::Colorize;

use aver::ast::TopLevel;
use aver::call_graph::{find_recursive_fns, recursive_callsite_counts};
use aver::interpreter::{Interpreter, Value};
use aver::resolver;
use aver::source::parse_source;
use aver::tco;
use aver::types;
use aver::types::checker::{run_type_check_full, TypeCheckResult, TypeError};

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

pub(super) fn load_dep_modules(
    interp: &mut Interpreter,
    items: &[TopLevel],
    module_root: &str,
) -> Result<(), String> {
    let mut loading = Vec::new();
    let mut loading_set = std::collections::HashSet::new();
    if let Some(module) = items.iter().find_map(|i| {
        if let TopLevel::Module(m) = i {
            Some(m)
        } else {
            None
        }
    }) {
        for dep_name in &module.depends {
            let ns = interp
                .load_module(dep_name, module_root, &mut loading, &mut loading_set)
                .map_err(|e| e.to_string())?;
            interp
                .define_module_path(dep_name, ns)
                .map_err(|e| e.to_string())?;
        }
    }
    Ok(())
}

pub(super) fn print_type_errors(errors: &[TypeError]) {
    for te in errors {
        match te.line {
            Some(line) => eprintln!("{}", format!("error[{}]: {}", line, te.message).red()),
            None => eprintln!("{}", format!("error: {}", te.message).red()),
        }
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
        Type::List(_) | Type::Map(_, _) | Type::Fn(_, _, _) | Type::Unknown => false,
        Type::Result(_, _) | Type::Option(_) => false,
        Type::Named(name) => safe_named.contains(name),
    }
}

pub(super) fn format_type_errors(errors: &[TypeError]) -> String {
    let mut out = Vec::new();
    for te in errors {
        match te.line {
            Some(line) => out.push(format!("error[{}]: {}", line, te.message)),
            None => out.push(format!("error: {}", te.message)),
        }
    }
    out.join("\n")
}

pub(super) fn compile_program_for_exec(
    file: &str,
    module_root_override: Option<&str>,
) -> Result<(Interpreter, Vec<TopLevel>, String), String> {
    let module_root = resolve_module_root(module_root_override);
    let source = read_file(file)?;
    let mut items = parse_file(&source)?;

    // TCO transform — rewrite tail-position calls in recursive SCCs
    tco::transform_program(&mut items);

    // Static type check — block execution on any error
    let tc_result = run_type_check_full(&items, Some(&module_root));
    if !tc_result.errors.is_empty() {
        return Err(format_type_errors(&tc_result.errors));
    }

    // Compile-time variable resolution
    resolver::resolve_program(&mut items);

    // Auto-memoization: find pure recursive fns with memo-safe params
    let memo_fns = compute_memo_fns(&items, &tc_result);

    let mut interp = Interpreter::new();
    interp.enable_memo(memo_fns);

    load_dep_modules(&mut interp, &items, &module_root)?;

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
            interp.exec_fn_def(fd).map_err(|e| e.to_string())?;
        }
    }

    Ok((interp, items, module_root))
}

pub(super) fn run_top_level_statements(
    interp: &mut Interpreter,
    items: &[TopLevel],
) -> Result<(), String> {
    for item in items {
        if let TopLevel::Stmt(stmt) = item {
            interp.exec_stmt(stmt).map_err(|e| e.to_string())?;
        }
    }
    Ok(())
}

pub(super) fn run_entry_function(
    interp: &mut Interpreter,
    entry_fn: &str,
    args: Vec<Value>,
) -> Result<Value, String> {
    let fn_val = interp
        .lookup(entry_fn)
        .map_err(|_| format!("Entry function '{}' not found", entry_fn))?;
    let allowed = Interpreter::callable_declared_effects(&fn_val);
    interp
        .call_value_with_effects_pub(fn_val, args, &format!("<{}>", entry_fn), allowed)
        .map_err(|e| e.to_string())
}

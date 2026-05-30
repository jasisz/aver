use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::process;
use std::time::{SystemTime, UNIX_EPOCH};

use colored::Colorize;

use aver::ast::{Expr, FnDef, Pattern, Spanned, Stmt, TopLevel, TypeDef, VerifyKind};
use aver::checker::{CheckFinding, VerifyResult, index_decisions};
use aver::codegen;
use aver::codegen::ModuleInfo;
use aver::codegen::lean as lean_codegen;
use aver::codegen::rust as rust_codegen;
use aver::nan_value::{Arena, NanValueConvert};
use aver::source::{find_module_file, require_module_declaration};
use aver::types::{Type, parse_type_str};
use aver::verify_law::{
    collect_contextual_helper_law_hints, collect_missing_helper_law_hints,
    contextual_helper_law_message, missing_helper_law_message,
};
use aver::vm;

use super::diagnostic;
use aver::tty_render::render_tty;

use crate::shared::{
    apply_runtime_policy_to_vm, compute_memo_fns, format_type_errors, load_runtime_policy,
    parse_file, print_type_errors, read_file, resolve_module_root,
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

#[cfg(feature = "wasm")]
fn format_byte_size(bytes: u64) -> String {
    if bytes >= 1024 * 1024 {
        format!("{:.1} MiB", bytes as f64 / (1024.0 * 1024.0))
    } else if bytes >= 1024 {
        format!("{:.1} KiB", bytes as f64 / 1024.0)
    } else {
        format!("{} B", bytes)
    }
}

fn is_av_file(path: &Path) -> bool {
    path.extension().and_then(|ext| ext.to_str()) == Some("av")
}

fn collect_av_input_files(path: &Path, out: &mut Vec<PathBuf>) -> Result<(), String> {
    if !path.exists() {
        return Err(format!("Path '{}' does not exist", path.display()));
    }

    if path.is_file() {
        if is_av_file(path) {
            out.push(path.to_path_buf());
            return Ok(());
        }
        return Err(format!("'{}' is not an .av file", path.display()));
    }

    let entries = fs::read_dir(path)
        .map_err(|e| format!("Cannot read directory '{}': {}", path.display(), e))?;
    for entry in entries {
        let entry = entry
            .map_err(|e| format!("Cannot read directory entry in '{}': {}", path.display(), e))?;
        let child = entry.path();
        if child.is_dir() {
            collect_av_input_files(&child, out)?;
        } else if is_av_file(&child) {
            out.push(child);
        }
    }

    Ok(())
}

pub(super) fn resolve_av_inputs(path: &str) -> Result<Vec<String>, String> {
    let root = Path::new(path);
    let mut files = Vec::new();
    collect_av_input_files(root, &mut files)?;
    files.sort();

    if files.is_empty() {
        return Err(format!("No .av files found under '{}'", root.display()));
    }

    Ok(files
        .into_iter()
        .map(|path| path_to_string(&path))
        .collect())
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

pub(super) fn recording_paths(file: &str, module_root: &str) -> (String, String) {
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

fn materialize_codegen_output(
    output_dir: &Path,
    output: &codegen::ProjectOutput,
) -> Result<(), String> {
    for (rel_path, content) in &output.files {
        let full_path = output_dir.join(rel_path);
        if let Some(parent) = full_path.parent() {
            fs::create_dir_all(parent)
                .map_err(|e| format!("Cannot create dir '{}': {}", parent.display(), e))?;
        }
        fs::write(&full_path, content)
            .map_err(|e| format!("Cannot write '{}': {}", full_path.display(), e))?;
    }
    Ok(())
}

fn with_local_runtime_override<T>(run: impl FnOnce() -> T) -> T {
    let key = "AVER_RUNTIME_PATH";
    let previous = std::env::var_os(key);
    let local_runtime = Path::new(env!("CARGO_MANIFEST_DIR")).join("aver-rt");
    let use_local = local_runtime.exists();

    if use_local {
        // CLI is single-threaded here; we scope the override tightly around one transpile call.
        unsafe {
            std::env::set_var(key, &local_runtime);
        }
    }

    let result = run();

    match previous {
        Some(value) => unsafe {
            std::env::set_var(key, value);
        },
        None => unsafe {
            std::env::remove_var(key);
        },
    }

    result
}

/// Find the pre-compiled self-host binary next to the current executable.
/// The binary is built as a `[[bin]]` target in the same Cargo package,
/// so `cargo build` / `cargo install` places it alongside `aver`.
pub(super) fn find_self_host_binary() -> Result<PathBuf, String> {
    let self_exe =
        std::env::current_exe().map_err(|e| format!("cannot determine executable path: {e}"))?;
    let dir = self_exe
        .parent()
        .ok_or_else(|| "cannot determine executable directory".to_string())?;
    let name = format!("aver_self_host_cli{}", std::env::consts::EXE_SUFFIX);
    let binary = dir.join(&name);
    if binary.exists() {
        Ok(binary)
    } else {
        Err(format!(
            "self-host binary not found at {}. Rebuild with: cargo build --features runtime",
            binary.display()
        ))
    }
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

        // Parse failure shouldn't abort the whole check — let
        // analyze_source turn it into a canonical parse-error
        // diagnostic (with line/col + repair hint) the same way
        // every other diagnostic flows. We still have the source so
        // the downstream render can snippet the error line.
        let items = parse_file(&source).unwrap_or_default();

        if !items.is_empty() {
            let _ = require_module_declaration(&items, &path_str);
        }

        if include_deps
            && let Some(m) = items.iter().find_map(|item| {
                if let TopLevel::Module(m) = item {
                    Some(m)
                } else {
                    None
                }
            })
        {
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

        out.push((path_str, source, items));
    }

    Ok(out)
}

fn canonical_path_key(path: &str) -> String {
    std::fs::canonicalize(path)
        .unwrap_or_else(|_| PathBuf::from(path))
        .to_string_lossy()
        .to_string()
}

#[derive(Debug, Clone)]
struct ExposedModuleInfo {
    canonical_path: String,
    file: String,
    module_name: String,
    exposes_line: usize,
    exposed_names: Vec<String>,
    exposed_name_set: HashSet<String>,
    exposed_type_names: HashSet<String>,
    is_entry: bool,
}

#[derive(Debug, Clone)]
struct ImportTarget {
    dep_path_parts: Vec<String>,
    info: ExposedModuleInfo,
}

fn local_type_names(items: &[TopLevel]) -> HashSet<String> {
    items
        .iter()
        .filter_map(|item| match item {
            TopLevel::TypeDef(TypeDef::Sum { name, .. })
            | TopLevel::TypeDef(TypeDef::Product { name, .. }) => Some(name.clone()),
            _ => None,
        })
        .collect()
}

fn mark_used_export(
    export_name: &str,
    target_path: &str,
    used_by_target: &mut HashMap<String, HashSet<String>>,
) {
    used_by_target
        .entry(target_path.to_string())
        .or_default()
        .insert(export_name.to_string());
}

fn mark_path_use(
    parts: &[String],
    dep_targets: &[ImportTarget],
    unique_type_owner: &HashMap<String, String>,
    used_by_target: &mut HashMap<String, HashSet<String>>,
) {
    for target in dep_targets {
        if parts.len() <= target.dep_path_parts.len() {
            continue;
        }
        if parts.starts_with(&target.dep_path_parts) {
            let export_name = &parts[target.dep_path_parts.len()];
            if target.info.exposed_name_set.contains(export_name) {
                mark_used_export(export_name, &target.info.canonical_path, used_by_target);
            }
        }
    }

    if let Some(owner) = unique_type_owner.get(&parts[0]) {
        mark_used_export(&parts[0], owner, used_by_target);
    }
}

fn expr_path_parts(expr: &Spanned<Expr>) -> Option<Vec<String>> {
    match &expr.node {
        Expr::Attr(inner, field) => {
            let mut parts = match &inner.node {
                Expr::Ident(name) => vec![name.clone()],
                _ => expr_path_parts(inner)?,
            };
            parts.push(field.clone());
            Some(parts)
        }
        Expr::Ident(_) => None,
        Expr::Constructor(name, _) => Some(name.split('.').map(|part| part.to_string()).collect()),
        _ => None,
    }
}

fn expr_self_host_runtime_name(expr: &Spanned<Expr>) -> Option<String> {
    match &expr.node {
        Expr::Ident(name) => Some(name.clone()),
        Expr::Attr(_, _) => expr_path_parts(expr).map(|parts| parts.join(".")),
        Expr::Constructor(name, _) => Some(name.clone()),
        _ => None,
    }
}

fn expr_uses_self_host_runtime(expr: &Spanned<Expr>) -> bool {
    if expr_self_host_runtime_name(expr).is_some_and(|name| name.starts_with("SelfHostRuntime.")) {
        return true;
    }

    match &expr.node {
        Expr::Attr(inner, _) | Expr::Constructor(_, Some(inner)) | Expr::ErrorProp(inner) => {
            expr_uses_self_host_runtime(inner)
        }
        Expr::FnCall(callee, args) => {
            expr_uses_self_host_runtime(callee) || args.iter().any(expr_uses_self_host_runtime)
        }
        Expr::BinOp(_, left, right) => {
            expr_uses_self_host_runtime(left) || expr_uses_self_host_runtime(right)
        }
        Expr::Neg(inner) => expr_uses_self_host_runtime(inner),
        Expr::Match { subject, arms, .. } => {
            expr_uses_self_host_runtime(subject)
                || arms
                    .iter()
                    .any(|arm| expr_uses_self_host_runtime(&arm.body))
        }
        Expr::InterpolatedStr(parts) => parts.iter().any(|part| match part {
            aver::ast::StrPart::Literal(_) => false,
            aver::ast::StrPart::Parsed(inner) => expr_uses_self_host_runtime(inner),
        }),
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            items.iter().any(expr_uses_self_host_runtime)
        }
        Expr::MapLiteral(entries) => entries.iter().any(|(key, value)| {
            expr_uses_self_host_runtime(key) || expr_uses_self_host_runtime(value)
        }),
        Expr::RecordCreate { fields, .. } => fields
            .iter()
            .any(|(_, value)| expr_uses_self_host_runtime(value)),
        Expr::RecordUpdate { base, updates, .. } => {
            expr_uses_self_host_runtime(base)
                || updates
                    .iter()
                    .any(|(_, value)| expr_uses_self_host_runtime(value))
        }
        Expr::TailCall(inner) => inner.args.iter().any(expr_uses_self_host_runtime),
        Expr::Literal(_) | Expr::Ident(_) | Expr::Constructor(_, None) | Expr::Resolved { .. } => {
            false
        }
    }
}

fn stmt_uses_self_host_runtime(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => expr_uses_self_host_runtime(expr),
    }
}

fn fn_uses_self_host_runtime(fd: &FnDef) -> bool {
    fd.body.stmts().iter().any(stmt_uses_self_host_runtime)
}

fn item_uses_self_host_runtime(item: &TopLevel) -> bool {
    match item {
        TopLevel::FnDef(fd) => fn_uses_self_host_runtime(fd),
        TopLevel::Stmt(stmt) => stmt_uses_self_host_runtime(stmt),
        _ => false,
    }
}

fn codegen_uses_self_host_runtime(ctx: &codegen::CodegenContext) -> bool {
    ctx.items.iter().any(item_uses_self_host_runtime)
        || ctx
            .modules
            .iter()
            .any(|module| module.fn_defs.iter().any(fn_uses_self_host_runtime))
}

fn validate_self_host_guest_entry_contract(ctx: &codegen::CodegenContext) -> Result<(), String> {
    if !ctx.emit_self_host_support {
        return Ok(());
    }

    let entry_name = ctx
        .guest_entry
        .as_deref()
        .ok_or_else(|| "--with-self-host-support requires --guest-entry".to_string())?;
    let fd = ctx
        .fn_defs
        .iter()
        .find(|fd| fd.name == entry_name)
        .ok_or_else(|| format!("guest entry '{entry_name}' was not found"))?;

    let has_prog = fd.params.iter().any(|(name, type_ann)| {
        name == "prog" && parse_type_str(type_ann) == Type::named("Program")
    });
    let has_module_fns = fd.params.iter().any(|(name, type_ann)| {
        name == "moduleFns"
            && parse_type_str(type_ann) == Type::List(Box::new(Type::named("FnDef")))
    });

    if has_prog && has_module_fns {
        Ok(())
    } else {
        Err(format!(
            "--with-self-host-support requires guest entry '{}' to declare `prog: Program` and `moduleFns: List<FnDef>`",
            entry_name
        ))
    }
}

fn mark_type_uses(
    ty: &Type,
    dep_targets: &[ImportTarget],
    unique_type_owner: &HashMap<String, String>,
    used_by_target: &mut HashMap<String, HashSet<String>>,
) {
    match ty {
        Type::Named { name, .. } => {
            let parts = name
                .split('.')
                .map(|part| part.to_string())
                .collect::<Vec<_>>();
            mark_path_use(&parts, dep_targets, unique_type_owner, used_by_target);
        }
        Type::Result(ok, err) => {
            mark_type_uses(ok, dep_targets, unique_type_owner, used_by_target);
            mark_type_uses(err, dep_targets, unique_type_owner, used_by_target);
        }
        Type::Option(inner) | Type::List(inner) | Type::Vector(inner) => {
            mark_type_uses(inner, dep_targets, unique_type_owner, used_by_target);
        }
        Type::Tuple(items) => {
            for item in items {
                mark_type_uses(item, dep_targets, unique_type_owner, used_by_target);
            }
        }
        Type::Map(key, value) => {
            mark_type_uses(key, dep_targets, unique_type_owner, used_by_target);
            mark_type_uses(value, dep_targets, unique_type_owner, used_by_target);
        }
        Type::Fn(params, ret, _) => {
            for param in params {
                mark_type_uses(param, dep_targets, unique_type_owner, used_by_target);
            }
            mark_type_uses(ret, dep_targets, unique_type_owner, used_by_target);
        }
        Type::Int
        | Type::Float
        | Type::Str
        | Type::Bool
        | Type::Unit
        | Type::Invalid
        | Type::Var(_) => {}
    }
}

fn mark_type_annotation(
    type_str: &str,
    dep_targets: &[ImportTarget],
    unique_type_owner: &HashMap<String, String>,
    used_by_target: &mut HashMap<String, HashSet<String>>,
) {
    let ty = parse_type_str(type_str);
    mark_type_uses(&ty, dep_targets, unique_type_owner, used_by_target);
}

fn walk_pattern_for_exposes(
    pattern: &Pattern,
    dep_targets: &[ImportTarget],
    unique_type_owner: &HashMap<String, String>,
    used_by_target: &mut HashMap<String, HashSet<String>>,
) {
    match pattern {
        Pattern::Constructor(path, _) => {
            let parts = path
                .split('.')
                .map(|part| part.to_string())
                .collect::<Vec<_>>();
            mark_path_use(&parts, dep_targets, unique_type_owner, used_by_target);
        }
        Pattern::Tuple(items) => {
            for item in items {
                walk_pattern_for_exposes(item, dep_targets, unique_type_owner, used_by_target);
            }
        }
        Pattern::Wildcard
        | Pattern::Literal(_)
        | Pattern::Ident(_)
        | Pattern::EmptyList
        | Pattern::Cons(_, _) => {}
    }
}

fn walk_expr_for_exposes(
    expr: &Spanned<Expr>,
    dep_targets: &[ImportTarget],
    unique_type_owner: &HashMap<String, String>,
    used_by_target: &mut HashMap<String, HashSet<String>>,
) {
    if let Some(parts) = expr_path_parts(expr) {
        mark_path_use(&parts, dep_targets, unique_type_owner, used_by_target);
    }

    match &expr.node {
        Expr::Attr(inner, _) => {
            walk_expr_for_exposes(inner, dep_targets, unique_type_owner, used_by_target);
        }
        Expr::FnCall(callee, args) => {
            walk_expr_for_exposes(callee, dep_targets, unique_type_owner, used_by_target);
            for arg in args {
                walk_expr_for_exposes(arg, dep_targets, unique_type_owner, used_by_target);
            }
        }
        Expr::BinOp(_, left, right) => {
            walk_expr_for_exposes(left, dep_targets, unique_type_owner, used_by_target);
            walk_expr_for_exposes(right, dep_targets, unique_type_owner, used_by_target);
        }
        Expr::Neg(inner) => {
            walk_expr_for_exposes(inner, dep_targets, unique_type_owner, used_by_target);
        }
        Expr::Match { subject, arms, .. } => {
            walk_expr_for_exposes(subject, dep_targets, unique_type_owner, used_by_target);
            for arm in arms {
                walk_pattern_for_exposes(
                    &arm.pattern,
                    dep_targets,
                    unique_type_owner,
                    used_by_target,
                );
                walk_expr_for_exposes(&arm.body, dep_targets, unique_type_owner, used_by_target);
            }
        }
        Expr::Constructor(_, Some(inner)) | Expr::ErrorProp(inner) => {
            walk_expr_for_exposes(inner, dep_targets, unique_type_owner, used_by_target);
        }
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let aver::ast::StrPart::Parsed(inner) = part {
                    walk_expr_for_exposes(inner, dep_targets, unique_type_owner, used_by_target);
                }
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for item in items {
                walk_expr_for_exposes(item, dep_targets, unique_type_owner, used_by_target);
            }
        }
        Expr::MapLiteral(entries) => {
            for (key, value) in entries {
                walk_expr_for_exposes(key, dep_targets, unique_type_owner, used_by_target);
                walk_expr_for_exposes(value, dep_targets, unique_type_owner, used_by_target);
            }
        }
        Expr::RecordCreate { type_name, fields } => {
            let parts = type_name
                .split('.')
                .map(|part| part.to_string())
                .collect::<Vec<_>>();
            mark_path_use(&parts, dep_targets, unique_type_owner, used_by_target);
            for (_, value) in fields {
                walk_expr_for_exposes(value, dep_targets, unique_type_owner, used_by_target);
            }
        }
        Expr::RecordUpdate {
            type_name,
            base,
            updates,
        } => {
            let parts = type_name
                .split('.')
                .map(|part| part.to_string())
                .collect::<Vec<_>>();
            mark_path_use(&parts, dep_targets, unique_type_owner, used_by_target);
            walk_expr_for_exposes(base, dep_targets, unique_type_owner, used_by_target);
            for (_, value) in updates {
                walk_expr_for_exposes(value, dep_targets, unique_type_owner, used_by_target);
            }
        }
        Expr::TailCall(inner) => {
            for arg in &inner.args {
                walk_expr_for_exposes(arg, dep_targets, unique_type_owner, used_by_target);
            }
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Constructor(_, None) | Expr::Resolved { .. } => {}
    }
}

fn walk_stmt_for_exposes(
    stmt: &Stmt,
    dep_targets: &[ImportTarget],
    unique_type_owner: &HashMap<String, String>,
    used_by_target: &mut HashMap<String, HashSet<String>>,
) {
    match stmt {
        Stmt::Binding(_, Some(type_name), expr) => {
            mark_type_annotation(type_name, dep_targets, unique_type_owner, used_by_target);
            walk_expr_for_exposes(expr, dep_targets, unique_type_owner, used_by_target);
        }
        Stmt::Binding(_, None, expr) | Stmt::Expr(expr) => {
            walk_expr_for_exposes(expr, dep_targets, unique_type_owner, used_by_target);
        }
    }
}

fn collect_used_exposes_for_importer(
    items: &[TopLevel],
    dep_targets: &[ImportTarget],
) -> HashMap<String, HashSet<String>> {
    let local_types = local_type_names(items);
    let mut type_providers: HashMap<String, Vec<String>> = HashMap::new();
    for target in dep_targets {
        for type_name in &target.info.exposed_type_names {
            type_providers
                .entry(type_name.clone())
                .or_default()
                .push(target.info.canonical_path.clone());
        }
    }

    let unique_type_owner = type_providers
        .into_iter()
        .filter_map(|(type_name, owners)| {
            if owners.len() == 1 && !local_types.contains(&type_name) {
                Some((type_name, owners[0].clone()))
            } else {
                None
            }
        })
        .collect::<HashMap<_, _>>();

    let mut used_by_target = HashMap::new();

    for item in items {
        match item {
            TopLevel::Module(_) | TopLevel::Decision(_) => {}
            TopLevel::FnDef(fd) => {
                for (_, type_name) in &fd.params {
                    mark_type_annotation(
                        type_name,
                        dep_targets,
                        &unique_type_owner,
                        &mut used_by_target,
                    );
                }
                mark_type_annotation(
                    &fd.return_type,
                    dep_targets,
                    &unique_type_owner,
                    &mut used_by_target,
                );
                for stmt in fd.body.stmts() {
                    walk_stmt_for_exposes(
                        stmt,
                        dep_targets,
                        &unique_type_owner,
                        &mut used_by_target,
                    );
                }
            }
            TopLevel::Verify(vb) => {
                for (lhs, rhs) in &vb.cases {
                    walk_expr_for_exposes(
                        lhs,
                        dep_targets,
                        &unique_type_owner,
                        &mut used_by_target,
                    );
                    walk_expr_for_exposes(
                        rhs,
                        dep_targets,
                        &unique_type_owner,
                        &mut used_by_target,
                    );
                }
                if let VerifyKind::Law(law) = &vb.kind {
                    for given in &law.givens {
                        mark_type_annotation(
                            &given.type_name,
                            dep_targets,
                            &unique_type_owner,
                            &mut used_by_target,
                        );
                    }
                    if let Some(when) = &law.when {
                        walk_expr_for_exposes(
                            when,
                            dep_targets,
                            &unique_type_owner,
                            &mut used_by_target,
                        );
                    }
                    walk_expr_for_exposes(
                        &law.lhs,
                        dep_targets,
                        &unique_type_owner,
                        &mut used_by_target,
                    );
                    walk_expr_for_exposes(
                        &law.rhs,
                        dep_targets,
                        &unique_type_owner,
                        &mut used_by_target,
                    );
                    for guard in &law.sample_guards {
                        walk_expr_for_exposes(
                            guard,
                            dep_targets,
                            &unique_type_owner,
                            &mut used_by_target,
                        );
                    }
                }
            }
            TopLevel::Stmt(stmt) => {
                walk_stmt_for_exposes(stmt, dep_targets, &unique_type_owner, &mut used_by_target);
            }
            TopLevel::TypeDef(TypeDef::Sum { variants, .. }) => {
                for variant in variants {
                    for field_type in &variant.fields {
                        mark_type_annotation(
                            field_type,
                            dep_targets,
                            &unique_type_owner,
                            &mut used_by_target,
                        );
                    }
                }
            }
            TopLevel::TypeDef(TypeDef::Product { fields, .. }) => {
                for (_, field_type) in fields {
                    mark_type_annotation(
                        field_type,
                        dep_targets,
                        &unique_type_owner,
                        &mut used_by_target,
                    );
                }
            }
        }
    }

    used_by_target
}

fn collect_unused_exposes_findings(
    units: &[(String, String, Vec<TopLevel>)],
    entry_file: &str,
    module_root: &str,
) -> Vec<CheckFinding> {
    let entry_canonical = canonical_path_key(entry_file);
    let mut module_info_by_path = HashMap::new();

    for (path, _source, items) in units {
        let canonical = canonical_path_key(path);
        let Some(module) = items.iter().find_map(|item| {
            if let TopLevel::Module(module) = item {
                Some(module)
            } else {
                None
            }
        }) else {
            continue;
        };

        if module.exposes.is_empty() && module.exposes_opaque.is_empty() {
            continue;
        }

        let exposed_name_set = module.exposes.iter().cloned().collect::<HashSet<_>>();
        let opaque_name_set: HashSet<String> = module.exposes_opaque.iter().cloned().collect();
        let exposed_type_names = items
            .iter()
            .filter_map(|item| match item {
                TopLevel::TypeDef(TypeDef::Sum { name, .. })
                | TopLevel::TypeDef(TypeDef::Product { name, .. })
                    if exposed_name_set.contains(name) || opaque_name_set.contains(name) =>
                {
                    Some(name.clone())
                }
                _ => None,
            })
            .collect::<HashSet<_>>();

        module_info_by_path.insert(
            canonical.clone(),
            ExposedModuleInfo {
                canonical_path: canonical,
                file: path.clone(),
                module_name: module.name.clone(),
                exposes_line: module.exposes_line.unwrap_or(module.line),
                exposed_names: module.exposes.clone(),
                exposed_name_set,
                exposed_type_names,
                is_entry: canonical_path_key(path) == entry_canonical,
            },
        );
    }

    let mut used_by_target: HashMap<String, HashSet<String>> = HashMap::new();

    for (_path, _source, items) in units {
        let Some(module) = items.iter().find_map(|item| {
            if let TopLevel::Module(module) = item {
                Some(module)
            } else {
                None
            }
        }) else {
            continue;
        };

        let dep_targets = module
            .depends
            .iter()
            .filter_map(|dep| {
                let dep_path = find_module_file(dep, module_root)?;
                let dep_key = canonical_path_key(&dep_path.to_string_lossy());
                let info = module_info_by_path.get(&dep_key)?.clone();
                Some(ImportTarget {
                    dep_path_parts: dep.split('.').map(|part| part.to_string()).collect(),
                    info,
                })
            })
            .collect::<Vec<_>>();

        if dep_targets.is_empty() {
            continue;
        }

        let importer_usage = collect_used_exposes_for_importer(items, &dep_targets);
        for (target_path, names) in importer_usage {
            used_by_target.entry(target_path).or_default().extend(names);
        }
    }

    let mut findings = Vec::new();
    let mut modules = module_info_by_path.into_values().collect::<Vec<_>>();
    modules.sort_by(|left, right| left.file.cmp(&right.file));

    for info in modules {
        if info.is_entry {
            continue;
        }

        let used = used_by_target
            .get(&info.canonical_path)
            .cloned()
            .unwrap_or_default();
        let unused = info
            .exposed_names
            .iter()
            .filter(|name| !used.contains(name.as_str()))
            .cloned()
            .collect::<Vec<_>>();
        if unused.is_empty() {
            continue;
        }

        findings.push(CheckFinding {
            line: info.exposes_line,
            module: Some(info.module_name),
            file: Some(info.file),
            fn_name: None,
            message: format!("Unused exposes: {}", unused.join(", ")),
            extra_spans: vec![],
        });
    }

    findings
}

#[allow(dead_code)]
fn finding_location(f: &CheckFinding, entry_module: Option<&str>) -> String {
    match (&f.module, entry_module) {
        (Some(module), Some(entry)) if module == entry => f.line.to_string(),
        (Some(module), _) => format!("{}:{}", module, f.line),
        (None, _) => f.line.to_string(),
    }
}

pub(super) fn display_check_path(path: &str, module_root: &str) -> String {
    let p = Path::new(path);
    let root = Path::new(module_root);

    if p.is_absolute() {
        if let Some(rel) = relativize_to(root, p).or_else(|| relativize_to_canonical(root, p)) {
            return rel;
        }
        if let Ok(cwd) = std::env::current_dir()
            && let Some(rel) = relativize_to(&cwd, p).or_else(|| relativize_to_canonical(&cwd, p))
        {
            return rel;
        }
    }

    path.to_string()
}

pub(super) fn cmd_run_vm(
    file: &str,
    module_root_override: Option<&str>,
    record_dir: Option<&str>,
    program_args: Vec<String>,
    profile: bool,
    entry_expression: Option<&str>,
) {
    use aver::replay::{
        JsonValue, session::RecordedOutcome, session::SessionRecording,
        session_recording_to_string_pretty,
    };

    let module_root = super::shared::resolve_module_root(module_root_override);
    let source = match super::shared::read_file(file) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("{}", e.red());
            process::exit(1);
        }
    };
    let mut items = match super::shared::parse_file(&source) {
        Ok(items) => items,
        Err(e) => {
            eprintln!("{}", e.red());
            process::exit(1);
        }
    };

    // Compiler pipeline: tco → typecheck → interp_lower → buffer_build → resolve.
    // Single source of truth lives in `aver::ir::pipeline`; see that module
    // for ordering invariants between stages.
    //
    // Pre-load dep modules so the entry pipeline's `SymbolTable` (and
    // the resolved HIR derived from it) knows about every cross-module
    // call. Without this the resolver classified `Module.fn(...)` as
    // `ResolvedCallee::Unresolved`, leaning on the VM's
    // `resolve_dotted_call_target` fallback — same dispatch outcome
    // but a leaky `resolved_items` contract. PR 7.2 of #147 closes
    // the gap by mirroring what `cmd_compile_aver` already does.
    let dep_modules = load_compile_deps(&items, &module_root, false, false, false);
    let pipeline_result = aver::ir::pipeline::run(
        &mut items,
        aver::ir::PipelineConfig {
            typecheck: Some(aver::ir::TypecheckMode::Full {
                base_dir: Some(&module_root),
            }),
            dep_modules: &dep_modules,
            ..Default::default()
        },
    );
    let tc_result = pipeline_result.typecheck.expect("typecheck was requested");
    if !tc_result.errors.is_empty() {
        eprintln!(
            "{}",
            super::shared::format_type_errors(&tc_result.errors).red()
        );
        process::exit(1);
    }

    // Compile to bytecode. The analysis result from the pipeline carries
    // per-fn `FnAnalysis.allocates` flags so the VM compiler doesn't
    // recompute `compute_alloc_info` on the same items.
    let mut arena = Arena::new();
    vm::register_service_types(&mut arena);
    let (code, globals) = match vm::compile_program_with_modules(
        &pipeline_result.resolved_items,
        &pipeline_result.symbol_table,
        &mut arena,
        Some(&module_root),
        file,
        pipeline_result.analysis.as_ref(),
    ) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("{}", format!("VM compile error: {}", e).red());
            process::exit(1);
        }
    };

    // Execute
    let mut machine = vm::VM::new(code, globals, arena);
    if let Err(e) = apply_runtime_policy_to_vm(&mut machine, &module_root) {
        eprintln!("{}", e.red());
        process::exit(1);
    }

    machine.set_cli_args(program_args);

    if profile {
        machine.start_profiling();
    }

    if record_dir.is_some() {
        machine.start_recording();
    }

    // Resolve entry: either a user-supplied call expression or the default `main`.
    let entry_info: Option<(String, Vec<aver::value::Value>)> = if let Some(src) = entry_expression
    {
        match super::shared::parse_call_expression(src) {
            Ok(info) => Some(info),
            Err(e) => {
                eprintln!("{}", format!("--expr: {}", e).red());
                process::exit(1);
            }
        }
    } else {
        None
    };

    let entry_fn_label: String = entry_info
        .as_ref()
        .map(|(n, _)| n.clone())
        .unwrap_or_else(|| "main".to_string());

    let run_result = if let Some((fn_name, args)) = &entry_info {
        // Initialise top-level globals, then invoke the requested function.
        if let Err(e) = machine.run_top_level() {
            eprintln!("{}", format!("{}", e).red());
            process::exit(1);
        }
        let nv_args: Vec<aver::nan_value::NanValue> = args
            .iter()
            .map(|v| {
                <aver::nan_value::NanValue as aver::nan_value::NanValueConvert>::from_value(
                    v,
                    &mut machine.arena,
                )
            })
            .collect();
        machine.run_named_function(fn_name, &nv_args)
    } else {
        machine.run()
    };

    // Persist recording if requested.
    if let Some(dir) = record_dir {
        let request_id = generate_request_id();
        let timestamp = generate_timestamp();
        let (record_program_file, record_module_root) = recording_paths(file, &module_root);

        // For --expr runs, use a readable stem derived from fn + args; fall back
        // to the timestamped request_id otherwise.
        let file_stem = match &entry_info {
            Some((fn_name, args)) => super::shared::entry_recording_stem(fn_name, args),
            None => request_id.clone(),
        };
        let out_path = match prepare_recording_path(dir, &file_stem) {
            Ok(path) => path,
            Err(e) => {
                eprintln!("{}", e.red());
                process::exit(1);
            }
        };

        let output = match &run_result {
            Ok(result) => {
                let val = result.to_value(&machine.arena);
                match aver::replay::value_to_json(&val) {
                    Ok(json) => RecordedOutcome::Value(json),
                    Err(e) => RecordedOutcome::RuntimeError(e),
                }
            }
            Err(e) => RecordedOutcome::RuntimeError(format!("{}", e)),
        };

        // `input` is null for the default main entry; for --expr we serialise
        // the supplied arguments as a JSON list (or a single value if there is
        // exactly one) so `aver replay` can feed them back into
        // `run_named_function` via the existing `decode_entry_args` path.
        let input = match &entry_info {
            None => JsonValue::Null,
            Some((_, args)) => match super::shared::encode_entry_args_json(args) {
                Ok(v) => v,
                Err(e) => {
                    eprintln!(
                        "{}",
                        format!("Failed to serialise --expr arguments: {}", e).red()
                    );
                    process::exit(1);
                }
            },
        };

        let recording = SessionRecording {
            schema_version: 1,
            request_id,
            timestamp,
            program_file: record_program_file,
            module_root: record_module_root,
            entry_fn: entry_fn_label.clone(),
            input,
            effects: machine.recorded_effects().to_vec(),
            output,
        };

        let json_str = session_recording_to_string_pretty(&recording);
        if let Err(e) = std::fs::write(&out_path, json_str) {
            eprintln!("{}", format!("Failed to write recording: {}", e).red());
            process::exit(1);
        }
        println!("Recording saved: {}", out_path.display());
    }

    if profile && let Some(report) = machine.profile_report() {
        eprintln!("\n── VM Profile ──────────────────────────────────");
        eprintln!("Total opcodes: {}", report.total_opcodes);
        eprintln!("\nTop opcodes:");
        let mut sorted = report.opcodes.clone();
        sorted.sort_by_key(|b| std::cmp::Reverse(b.count));
        for op in sorted.iter().take(20).filter(|o| o.count > 0) {
            let pct = op.count as f64 / report.total_opcodes as f64 * 100.0;
            eprintln!("  {:>22} {:>12}  ({:.1}%)", op.name, op.count, pct);
        }
        eprintln!("\nTop functions (by entries):");
        let mut fns = report.functions.clone();
        fns.sort_by_key(|b| std::cmp::Reverse(b.entries));
        for f in fns.iter().take(15).filter(|f| f.entries > 0) {
            let flags = format!(
                "{}{}",
                if f.thin { "T" } else { "" },
                if f.parent_thin { "P" } else { "" }
            );
            eprintln!(
                "  {:>22} {:>10} entries  fast:{} slow:{} {}",
                f.name, f.entries, f.fast_returns, f.slow_returns, flags
            );
        }
        if !report.builtins.is_empty() {
            eprintln!("\nTop builtins:");
            for b in report.builtins.iter().take(10) {
                eprintln!("  {:>22} {:>12}", b.name, b.count);
            }
        }
        let bigrams = machine.profile_top_bigrams(15);
        if !bigrams.is_empty() {
            eprintln!("\nTop opcode pairs:");
            for ((a, b), count) in &bigrams {
                let pct = *count as f64 / report.total_opcodes as f64 * 100.0;
                eprintln!(
                    "  {:>14} → {:<14} {:>12}  ({:.1}%)",
                    aver::vm::opcode::opcode_name(*a),
                    aver::vm::opcode::opcode_name(*b),
                    count,
                    pct
                );
            }
        }
        eprintln!("\nReturn stats:");
        let r = &report.returns;
        eprintln!(
            "  total:{} thin:{} parent-thin:{}",
            r.total_entries, r.thin_entries, r.parent_thin_entries
        );
        eprintln!(
            "  fast:{} young-trunc:{} slow:{}",
            r.thin_fast_returns + r.parent_thin_fast_returns,
            r.young_truncate_fast_returns,
            r.thin_slow_returns + r.parent_thin_slow_returns + r.regular_slow_returns
        );
        eprintln!("────────────────────────────────────────────────\n");
    }

    match run_result {
        Ok(result) => {
            if result.is_err() {
                let inner = result.wrapper_inner(&machine.arena);
                let msg = inner.repr(&machine.arena);
                eprintln!(
                    "{}",
                    format!("{} returned error: {}", entry_fn_label, msg).red()
                );
                process::exit(1);
            }
        }
        Err(e) => {
            eprintln!("{}", format!("{}", e).red());
            process::exit(1);
        }
    }
}

pub(super) fn cmd_run_self_hosted(
    file: &str,
    module_root_override: Option<&str>,
    record_dir: Option<&str>,
    program_args: Vec<String>,
) {
    // Keep CLI parity with host `aver run` until the self-host carries its own
    // full front-end pipeline (type checker + TCO + module diagnostics).
    {
        let mr = resolve_module_root(module_root_override);
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
        // Self-host preflight only needs TCO + typecheck — codegen runs
        // in the spawned binary, not here.
        let pipeline_result = aver::ir::pipeline::run(
            &mut items,
            aver::ir::PipelineConfig {
                typecheck: Some(aver::ir::TypecheckMode::Full {
                    base_dir: Some(&mr),
                }),
                run_interp_lower: false,
                run_buffer_build: false,
                run_resolve: false,
                ..Default::default()
            },
        );
        let tc = pipeline_result.typecheck.expect("typecheck was requested");
        if !tc.errors.is_empty() {
            eprintln!("{}", format_type_errors(&tc.errors).red());
            process::exit(1);
        }
    }

    let module_root = resolve_module_root(module_root_override);
    let binary_path = match find_self_host_binary() {
        Ok(path) => path,
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
        Some((
            out_path,
            request_id,
            timestamp,
            record_program_file,
            record_module_root,
        ))
    } else {
        None
    };

    let mut command = process::Command::new(&binary_path);
    command.arg(file).arg(&module_root).args(&program_args);
    command.env("AVER_REPLAY_ENTRY_FN", "main");
    command.env("AVER_REPLAY_MODULE_ROOT", &module_root);

    if let Some((path, request_id, timestamp, program_file, record_module_root)) = &recording_target
    {
        command.env("AVER_REPLAY_RECORD", path);
        command.env("AVER_REPLAY_REQUEST_ID", request_id);
        command.env("AVER_REPLAY_TIMESTAMP", timestamp);
        command.env("AVER_REPLAY_PROGRAM_FILE", program_file);
        command.env("AVER_REPLAY_MODULE_ROOT", record_module_root);
    }

    let status = match command.status() {
        Ok(status) => status,
        Err(e) => {
            eprintln!(
                "{}",
                format!(
                    "Failed to launch cached self-host binary '{}': {}",
                    binary_path.display(),
                    e
                )
                .red()
            );
            process::exit(1);
        }
    };

    if let Some((path, ..)) = &recording_target
        && path.exists()
    {
        println!("Recording saved: {}", path.display());
    }

    if !status.success() {
        process::exit(status.code().unwrap_or(1));
    }
}

fn run_check_for_file(
    file: &str,
    module_root: &str,
    config: Option<&aver::config::ProjectConfig>,
    deps: bool,
    verbose: bool,
    json: bool,
) -> Result<bool, String> {
    let units = collect_check_units(file, module_root, deps)?;
    let _entry_module = units.first().and_then(|(_, _, items)| module_name(items));
    let mut unused_exposes_by_file: HashMap<String, Vec<CheckFinding>> = HashMap::new();
    if deps {
        for finding in collect_unused_exposes_findings(&units, file, module_root) {
            if let Some(path) = &finding.file {
                unused_exposes_by_file
                    .entry(canonical_path_key(path))
                    .or_default()
                    .push(finding);
            }
        }
    }
    let mut has_any_error = false;

    for (idx, (path, source, items)) in units.iter().enumerate() {
        let shown_path = display_check_path(path, module_root);
        if !json {
            if idx > 0 {
                println!();
            }
            println!("Check: {}", shown_path.cyan());
        }
        let line_count = source.lines().count();

        // --- Canonical analysis pipeline ---
        let opts = diagnostic::AnalyzeOptions {
            file_label: shown_path.clone(),
            module_base_dir: Some(module_root.to_string()),
            ..Default::default()
        };
        let report = diagnostic::analyze_source(source, &opts);
        let has_errors = report.diagnostics.iter().any(|d| d.is_error());
        let mut diagnostics = report.diagnostics;

        // --- Multi-file concerns: append unused-expose warnings computed
        //     across the whole check unit (not visible to single-file analyze)
        let unused_exposes_warnings = unused_exposes_by_file
            .get(&canonical_path_key(path))
            .cloned()
            .unwrap_or_default();
        for w in &unused_exposes_warnings {
            diagnostics.push(diagnostic::from_check_finding(
                diagnostic::Severity::Warning,
                w,
                source,
                &shown_path,
            ));
        }

        // --- Filter suppressed warnings ---
        let total_before = diagnostics.len();
        if let Some(cfg) = config {
            diagnostics.retain(|diag| {
                !diag.is_warning() || !cfg.is_check_suppressed(diag.slug, &shown_path)
            });
        }
        let suppressed_count = total_before - diagnostics.len();

        // --- Emit ---
        if json {
            let bundle = diagnostic::AnalysisReport::with_diagnostics(
                shown_path.clone(),
                diagnostics.clone(),
            );
            println!("{}", bundle.to_json());
        } else {
            for (i, diag) in diagnostics.iter().enumerate() {
                if i > 0 {
                    println!();
                }
                print!("{}", render_tty(diag, verbose));
            }
        }
        if !diagnostics.is_empty() && !json {
            println!();
        }
        if !json {
            let decisions = index_decisions(items);
            let mut summary_parts = Vec::new();
            if !has_errors {
                summary_parts.push(format!("{} types", "✓".green()));
            }
            if line_count <= 500 {
                summary_parts.push(format!("{} lines", line_count));
            } else {
                summary_parts.push(format!("{} {} lines (max 500)", "!".yellow(), line_count));
            }
            if !decisions.is_empty() {
                summary_parts.push(format!("{} decision(s)", decisions.len()));
            }
            if suppressed_count > 0 {
                summary_parts.push(format!(
                    "{} warning(s) suppressed by aver.toml",
                    suppressed_count
                ));
            }
            // Buffer-build sink/fusion summary used to print here. As of
            // 0.15.2 the same data (sinks, fusion sites, synthesized
            // variants, per-sink rewrite counts) is surfaced through
            // `aver compile --explain-passes` — keeping it out of the
            // default `aver check` summary so the line stays focused on
            // diagnostics.
            println!("  {}", summary_parts.join(" | "));
        }

        if has_errors {
            has_any_error = true;
        }
    }

    Ok(has_any_error)
}

/// Composite: static check + verify execution + format-check in one
/// pass. JSON mode emits one AnalysisReport bundle per file (diagnostics
/// include check issues + verify failures + needs-format), trailing
/// summary aggregates the three axes.
pub(super) fn cmd_audit(path: &str, module_root_override: Option<&str>, json: bool, hostile: bool) {
    use super::format_cmd::try_format_source;
    use aver::diagnostics::{AnalyzeOptions, analyze_source, needs_format_diagnostic};

    let module_root = crate::shared::resolve_module_root(module_root_override);
    let inputs = match resolve_av_inputs(path) {
        Ok(v) => v,
        Err(e) => {
            if json {
                println!(
                    "{{\"schema_version\":1,\"kind\":\"file-error\",\"error\":{}}}",
                    aver::diagnostics::json_escape(&e)
                );
            } else {
                eprintln!("{}", e.red());
            }
            process::exit(1);
        }
    };

    let mut total_check_errors = 0usize;
    let mut total_verify_failures = 0usize;
    let mut total_format_needed = 0usize;

    for file in &inputs {
        let shown_path = display_check_path(file, &module_root);
        let source = match crate::shared::read_file(file) {
            Ok(s) => s,
            Err(e) => {
                if json {
                    println!(
                        "{{\"schema_version\":1,\"kind\":\"file-error\",\"file\":{},\"error\":{}}}",
                        aver::diagnostics::json_escape(&shown_path),
                        aver::diagnostics::json_escape(&e)
                    );
                } else {
                    eprintln!("{}: {}", shown_path.red(), e);
                }
                continue;
            }
        };

        let mut opts = AnalyzeOptions::new(shown_path.clone());
        opts.module_base_dir = Some(module_root.clone());
        opts.include_verify_run = true;
        opts.verify_run_hostile = hostile;
        let mut report = analyze_source(&source, &opts);

        // Format check: append needs-format diagnostic with structured
        // per-rule violations (capped at the factory's MAX_VIOLATION_REGIONS).
        let (format_changed, format_violations) = match try_format_source(&source) {
            Ok((formatted, violations)) if formatted != source => (true, violations),
            _ => (false, Vec::new()),
        };
        let needs_format = format_changed;
        if needs_format {
            report.diagnostics.push(needs_format_diagnostic(
                &shown_path,
                &format_violations,
                &source,
            ));
            total_format_needed += 1;
        }

        let file_check_errors = report
            .diagnostics
            .iter()
            .filter(|d| {
                d.is_error() && d.slug != "verify-mismatch" && !d.slug.starts_with("verify-")
            })
            .count();
        let file_verify_failures = report
            .verify_summary
            .as_ref()
            .map(|vs| vs.blocks.iter().map(|b| b.failed).sum::<usize>())
            .unwrap_or(0);
        total_check_errors += file_check_errors;
        total_verify_failures += file_verify_failures;

        if json {
            println!("{}", report.to_json());
        } else {
            render_audit_tty(&shown_path, &report, needs_format);
        }
    }

    if json {
        println!(
            "{{\"schema_version\":1,\"kind\":\"summary\",\"files\":{},\"audit\":{{\"check_errors\":{},\"verify_failures\":{},\"format_needed\":{}}}}}",
            inputs.len(),
            total_check_errors,
            total_verify_failures,
            total_format_needed
        );
    } else {
        println!();
        println!("{}", "─".repeat(50).dimmed());
        println!(
            "{} {} files | {} check errors | {} verify failures | {} format",
            "Audit:".bold(),
            inputs.len(),
            total_check_errors,
            total_verify_failures,
            total_format_needed
        );
    }

    if total_check_errors > 0 || total_verify_failures > 0 || total_format_needed > 0 {
        process::exit(1);
    }
}

fn render_audit_tty(
    shown_path: &str,
    report: &aver::diagnostics::AnalysisReport,
    needs_format: bool,
) {
    println!();
    println!("{}", format!("Audit: {}", shown_path).cyan());
    for diag in &report.diagnostics {
        println!("  {}[{}]: {}", severity_tag(diag), diag.slug, diag.summary);
    }
    if let Some(vs) = &report.verify_summary {
        for block in &vs.blocks {
            if block.failed == 0 && block.skipped == 0 {
                println!(
                    "  {} verify {}  {}/{}",
                    "✓".green(),
                    block.name,
                    block.passed,
                    block.total
                );
            } else if block.failed == 0 {
                // Skipped cases aren't failures — typically law-form
                // guards (`when`) that didn't hit on some samples, or
                // proof obligations that the auto-prover deferred.
                // Marking them `✗` red made the block look broken at
                // a glance even with 0 failures. Use a neutral circle
                // in yellow to signal "partial / some obligations
                // skipped, nothing failed".
                println!(
                    "  {} verify {}  {}/{} passed, {} skipped",
                    "○".yellow(),
                    block.name,
                    block.passed,
                    block.total,
                    block.skipped
                );
            } else {
                println!(
                    "  {} verify {}  {}/{} passed, {} failed, {} skipped",
                    "✗".red(),
                    block.name,
                    block.passed,
                    block.total,
                    block.failed,
                    block.skipped
                );
            }
        }
    }
    if needs_format {
        println!("  {} needs format", "!".yellow());
    }
}

fn severity_tag(diag: &aver::diagnostics::Diagnostic) -> colored::ColoredString {
    use aver::diagnostics::Severity;
    match diag.severity {
        Severity::Error => "error".red(),
        Severity::Fail => "fail".red(),
        Severity::Warning => "warning".yellow(),
        Severity::Hint => "hint".cyan(),
    }
}

pub(super) fn cmd_check(
    path: &str,
    module_root_override: Option<&str>,
    deps: bool,
    verbose: bool,
    json: bool,
) {
    let module_root = resolve_module_root(module_root_override);
    let config = match aver::config::ProjectConfig::load_from_dir(Path::new(&module_root)) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("{}", e.red());
            process::exit(1);
        }
    };
    let inputs = match resolve_av_inputs(path) {
        Ok(inputs) => inputs,
        Err(e) => {
            eprintln!("{}", e.red());
            process::exit(1);
        }
    };

    let batch = Path::new(path).is_dir();
    let mut failed_files = Vec::new();

    for (idx, file) in inputs.iter().enumerate() {
        if !json && batch && idx > 0 {
            println!();
        }

        if !json && batch {
            println!("Input: {}", display_check_path(file, &module_root).cyan());
        }

        match run_check_for_file(file, &module_root, config.as_ref(), deps, verbose, json) {
            Ok(has_errors) => {
                if has_errors {
                    failed_files.push(file.clone());
                }
            }
            Err(e) => {
                eprintln!("{}", e.red());
                failed_files.push(file.clone());
            }
        }
    }

    if json {
        let passed = inputs.len().saturating_sub(failed_files.len());
        println!(
            "{{\"schema_version\":1,\"kind\":\"summary\",\"files\":{},\"passed\":{},\"failed\":{}}}",
            inputs.len(),
            passed,
            failed_files.len()
        );
    } else if batch {
        println!();
        let passed = inputs.len().saturating_sub(failed_files.len());
        if failed_files.is_empty() {
            println!(
                "{}",
                format!("Checked {} file(s): {} passed", inputs.len(), passed).green()
            );
        } else {
            println!(
                "{}",
                format!(
                    "Checked {} file(s): {} passed, {} failed",
                    inputs.len(),
                    passed,
                    failed_files.len()
                )
                .red()
            );
            for file in &failed_files {
                println!("  {}", display_check_path(file, &module_root));
            }
            if failed_files.len() > 3 {
                println!(
                    "{}",
                    "hint: if these files use modules, pass --module-root <dir>".dimmed()
                );
            }
        }
    }

    if !failed_files.is_empty() {
        process::exit(1);
    }
}

struct VerifyFileResult {
    path: String,
    source: String,
    blocks: Vec<VerifyResult>,
}

fn run_verify_for_file(
    file: &str,
    module_root: &str,
    deps: bool,
    hostile: bool,
    wasm_gc: bool,
) -> Result<Vec<VerifyFileResult>, String> {
    use aver::verify_law::expand::ExpansionMode;

    let units = collect_check_units(file, module_root, deps)?;
    let mut file_results = Vec::new();

    let config = load_runtime_policy(module_root)?;
    let mode = if hostile {
        ExpansionMode::Hostile
    } else {
        ExpansionMode::Declared
    };
    for (path, source, items) in units {
        let blocks = if wasm_gc {
            #[cfg(feature = "wasm")]
            {
                aver::diagnostics::wasm_gc_verify::run_verify_for_items_wasm_gc_with_mode(
                    items,
                    config.clone(),
                    Some(module_root),
                    &path,
                    mode,
                )?
            }
            #[cfg(not(feature = "wasm"))]
            {
                let _ = (items, &path);
                return Err("verify --wasm-gc requires building with --features wasm".to_string());
            }
        } else {
            aver::diagnostics::vm_verify::run_verify_for_items_vm_with_mode(
                items,
                config.clone(),
                Some(module_root),
                &path,
                mode,
            )?
        };
        file_results.push(VerifyFileResult {
            path,
            source,
            blocks,
        });
    }

    Ok(file_results)
}

/// Bucket case outcomes by `from_hostile` for the per-block summary
/// (declared vs hostile pass/fail). Skipped cases are dropped — they
/// already live in `result.skipped`.
/// Detect "vacuous-under-hostile" blocks: at least one hostile-profile
/// case was generated, and every single one ended in `Skipped` (its
/// `when` predicate returned false). Means the user's `when` is so
/// strict that no adversarial profile satisfies it — the law's hostile
/// run reduces to nothing. The renderer flags this as a warning so the
/// user doesn't read "0 hostile failures" as a clean bill.
fn vacuous_under_hostile(cases: &[aver::checker::VerifyCaseResult]) -> bool {
    use aver::checker::VerifyCaseOutcome;
    let mut had_hostile = false;
    let mut all_skipped = true;
    for case in cases {
        // `from_hostile` covers both axes: value-side boundary
        // expansion (typed `given` widened with i64::MIN/MAX,
        // ±Inf/NaN, NUL-embedded strings, …) and effect-side
        // adversarial profiles (frozen clock, always-min random,
        // network-down, …). Either is enough to drive the
        // vacuous warning when `when` rejects them all.
        if !case.from_hostile {
            continue;
        }
        // SkippedAfterBaseFail isn't a `when`-driven skip; it's a
        // VM-level optimization (base case already failed, so we
        // didn't bother running the profile permutation). Treat
        // those cases as if they didn't exist — vacuous-under-hostile
        // means "every adversarial profile rejected by `when`", not
        // "every adversarial profile pre-empted because the base
        // already broke".
        if matches!(case.outcome, VerifyCaseOutcome::SkippedAfterBaseFail) {
            continue;
        }
        had_hostile = true;
        if !matches!(case.outcome, VerifyCaseOutcome::Skipped) {
            all_skipped = false;
        }
    }
    had_hostile && all_skipped
}

fn bucket_hostile(cases: &[aver::checker::VerifyCaseResult]) -> (usize, usize, usize, usize) {
    use aver::checker::VerifyCaseOutcome;

    let mut declared_passed = 0usize;
    let mut declared_failed = 0usize;
    let mut hostile_passed = 0usize;
    let mut hostile_failed = 0usize;
    for case in cases {
        let passed = matches!(case.outcome, VerifyCaseOutcome::Pass);
        let skipped = matches!(
            case.outcome,
            VerifyCaseOutcome::Skipped | VerifyCaseOutcome::SkippedAfterBaseFail
        );
        if skipped {
            continue;
        }
        match (case.from_hostile, passed) {
            (false, true) => declared_passed += 1,
            (false, false) => declared_failed += 1,
            (true, true) => hostile_passed += 1,
            (true, false) => hostile_failed += 1,
        }
    }
    (
        declared_passed,
        declared_failed,
        hostile_passed,
        hostile_failed,
    )
}

fn render_verify_output(
    file_results: &[VerifyFileResult],
    module_root: &str,
    verbose: bool,
    json: bool,
) {
    use super::diagnostic::{
        verify_mismatch_diagnostic, verify_runtime_error_diagnostic,
        verify_unexpected_err_diagnostic,
    };
    use aver::checker::VerifyCaseOutcome;

    for (idx, fr) in file_results.iter().enumerate() {
        if fr.blocks.is_empty() {
            continue;
        }
        let display_path = display_check_path(&fr.path, module_root);

        if json {
            // One AnalysisReport bundle per file: failing-case diagnostics
            // + per-block scorecard. Same shape the playground and LSP see.
            let mut diagnostics: Vec<diagnostic::Diagnostic> = Vec::new();
            let mut block_results: Vec<aver::diagnostics::model::VerifyBlockResult> =
                Vec::with_capacity(fr.blocks.len());
            for block in &fr.blocks {
                for cr in &block.case_results {
                    let (line, col) = cr.span.as_ref().map(|s| (s.line, s.col)).unwrap_or((1, 1));
                    let diag = match &cr.outcome {
                        VerifyCaseOutcome::Mismatch { expected, actual } => {
                            Some(verify_mismatch_diagnostic(
                                &display_path,
                                &fr.source,
                                &block.block_label,
                                &cr.case_expr,
                                expected,
                                actual,
                                line,
                                col,
                                cr.law_context.is_some(),
                                cr.law_context.as_ref(),
                                cr.from_hostile,
                                cr.hostile_profile.as_deref(),
                            ))
                        }
                        VerifyCaseOutcome::RuntimeError { error } => {
                            Some(verify_runtime_error_diagnostic(
                                &display_path,
                                &fr.source,
                                &block.block_label,
                                &cr.case_expr,
                                error,
                                line,
                                col,
                            ))
                        }
                        VerifyCaseOutcome::UnexpectedErr { err_repr } => {
                            Some(verify_unexpected_err_diagnostic(
                                &display_path,
                                &fr.source,
                                &block.block_label,
                                &cr.case_expr,
                                err_repr,
                                line,
                                col,
                            ))
                        }
                        _ => None,
                    };
                    if let Some(d) = diag {
                        diagnostics.push(d);
                    }
                }
                let (declared_passed, declared_failed, hostile_passed, hostile_failed) =
                    bucket_hostile(&block.case_results);
                let skipped_by_when = block
                    .case_results
                    .iter()
                    .filter(|c| matches!(c.outcome, VerifyCaseOutcome::Skipped))
                    .count();
                let skipped_after_base_fail = block
                    .case_results
                    .iter()
                    .filter(|c| matches!(c.outcome, VerifyCaseOutcome::SkippedAfterBaseFail))
                    .count();
                block_results.push(aver::diagnostics::model::VerifyBlockResult {
                    name: block.block_label.clone(),
                    passed: block.passed,
                    failed: block.failed,
                    skipped: block.skipped,
                    total: block.passed + block.failed + block.skipped,
                    declared_passed,
                    declared_failed,
                    hostile_passed,
                    hostile_failed,
                    skipped_by_when,
                    skipped_after_base_fail,
                });
            }
            let mut report =
                diagnostic::AnalysisReport::with_diagnostics(display_path.clone(), diagnostics);
            report.verify_summary = Some(aver::diagnostics::model::VerifySummary {
                blocks: block_results,
            });
            println!("{}", report.to_json());
        } else {
            // Terminal mode
            if idx > 0 {
                println!();
            }
            println!("{}", format!("Verify: {}", display_path).cyan());

            for block in &fr.blocks {
                let total = block.passed + block.failed + block.skipped;
                if block.failed == 0 {
                    println!(
                        "  {} {}      {}/{}",
                        "✓".green(),
                        block.block_label,
                        block.passed,
                        total
                    );
                } else {
                    // Bracket reports either declared/hostile pass-ratios
                    // (when --hostile produced extra cases — the per-
                    // bucket split already implies the failure count) or
                    // a typed-failure breakdown (mismatch / runtime err /
                    // unexpected err — declared-only runs need this since
                    // the bucket split is just `1/1 ✗`). Mixing both is
                    // redundant: 24 mismatch + 11/35 hostile says "24
                    // failed" twice.
                    let (declared_passed, declared_failed, hostile_passed, hostile_failed) =
                        bucket_hostile(&block.case_results);
                    let has_hostile = hostile_passed + hostile_failed > 0;
                    let breakdown = if has_hostile {
                        let declared_total = declared_passed + declared_failed;
                        let hostile_total = hostile_passed + hostile_failed;
                        let skipped_when = block
                            .case_results
                            .iter()
                            .filter(|c| matches!(c.outcome, VerifyCaseOutcome::Skipped))
                            .count();
                        let skipped_base = block
                            .case_results
                            .iter()
                            .filter(|c| {
                                matches!(c.outcome, VerifyCaseOutcome::SkippedAfterBaseFail)
                            })
                            .count();
                        let mut tail = String::new();
                        if skipped_when > 0 {
                            tail.push_str(&format!(", {} skipped by `when`", skipped_when));
                        }
                        if skipped_base > 0 {
                            tail.push_str(&format!(
                                ", {} skipped (base case already failed)",
                                skipped_base
                            ));
                        }
                        format!(
                            " ({}/{} declared, {}/{} hostile{})",
                            declared_passed, declared_total, hostile_passed, hostile_total, tail
                        )
                    } else {
                        let mut mismatch = 0usize;
                        let mut runtime_err = 0usize;
                        let mut unexpected_err = 0usize;
                        for cr in &block.case_results {
                            match &cr.outcome {
                                VerifyCaseOutcome::Mismatch { .. } => mismatch += 1,
                                VerifyCaseOutcome::RuntimeError { .. } => runtime_err += 1,
                                VerifyCaseOutcome::UnexpectedErr { .. } => unexpected_err += 1,
                                _ => {}
                            }
                        }
                        let mut parts = Vec::new();
                        if mismatch > 0 {
                            parts.push(format!("{} mismatch", mismatch));
                        }
                        if runtime_err > 0 {
                            parts.push(format!("{} runtime error", runtime_err));
                        }
                        if unexpected_err > 0 {
                            parts.push(format!("{} unexpected err", unexpected_err));
                        }
                        if parts.is_empty() {
                            String::new()
                        } else {
                            format!(" ({})", parts.join(", "))
                        }
                    };
                    println!(
                        "  {} {}      {}/{} passed{}",
                        "✗".red(),
                        block.block_label,
                        block.passed,
                        total,
                        breakdown
                    );
                }

                // Vacuous-truth warning. If every hostile-profile case
                // was skipped by `when`, the law was effectively NOT
                // exercised under hostile mode — the user's assumption
                // is so strict that no adversarial profile satisfies
                // it. Without this hint, the user reads "passed under
                // hostile" and gets a false sense of safety.
                if vacuous_under_hostile(&block.case_results) {
                    println!(
                        "    {} every hostile profile was skipped by `when` — \
                         this law was not exercised under --hostile. Consider \
                         loosening the assumption.",
                        "warning:".yellow()
                    );
                }

                // Group `Mismatch` outcomes by (case_expr, line).
                // Profile-after-base-fail skipping happens at the VM
                // layer (`SkippedAfterBaseFail` outcome), so by the
                // time we get here every `Mismatch` is one we
                // actually want to show.
                use std::collections::HashMap;
                let mut mismatch_groups: HashMap<(String, usize), Vec<usize>> = HashMap::new();
                let mut mismatch_order: Vec<(String, usize)> = Vec::new();
                for (idx, cr) in block.case_results.iter().enumerate() {
                    if matches!(cr.outcome, VerifyCaseOutcome::Mismatch { .. }) {
                        let line = cr.span.as_ref().map(|s| s.line).unwrap_or(1);
                        let key = (cr.case_expr.clone(), line);
                        if !mismatch_groups.contains_key(&key) {
                            mismatch_order.push(key.clone());
                        }
                        mismatch_groups.entry(key).or_default().push(idx);
                    }
                }
                let max_diags = if verbose { usize::MAX } else { 3 };
                let mut diag_count = 0usize;

                for key in &mismatch_order {
                    let group = &mismatch_groups[key];
                    let primary = &block.case_results[group[0]];
                    let (line, col) = primary
                        .span
                        .as_ref()
                        .map(|s| (s.line, s.col))
                        .unwrap_or((1, 1));
                    let (expected, actual) = match &primary.outcome {
                        VerifyCaseOutcome::Mismatch { expected, actual } => {
                            (expected.clone(), actual.clone())
                        }
                        _ => unreachable!(),
                    };
                    let mut d = verify_mismatch_diagnostic(
                        &display_path,
                        &fr.source,
                        &block.block_label,
                        &primary.case_expr,
                        &expected,
                        &actual,
                        line,
                        col,
                        primary.law_context.is_some(),
                        primary.law_context.as_ref(),
                        primary.from_hostile,
                        primary.hostile_profile.as_deref(),
                    );
                    for &other_idx in &group[1..] {
                        let other = &block.case_results[other_idx];
                        let origin = match (other.from_hostile, other.hostile_profile.as_deref()) {
                            (true, Some(profile)) => {
                                format!("effect profile: {}", profile)
                            }
                            (true, None) => "value boundary substitution".to_string(),
                            (false, _) => continue,
                        };
                        if !d.fields.iter().any(|(k, v)| *k == "origin" && v == &origin) {
                            d.fields.push(("origin", origin));
                        }
                    }
                    if diag_count < max_diags {
                        println!();
                        print!("{}", render_tty(&d, verbose));
                    }
                    diag_count += 1;
                }

                // Other outcomes — per-case, not grouped (rare).
                for cr in &block.case_results {
                    let (line, col) = cr.span.as_ref().map(|s| (s.line, s.col)).unwrap_or((1, 1));
                    let diag = match &cr.outcome {
                        VerifyCaseOutcome::RuntimeError { error } => {
                            Some(verify_runtime_error_diagnostic(
                                &display_path,
                                &fr.source,
                                &block.block_label,
                                &cr.case_expr,
                                error,
                                line,
                                col,
                            ))
                        }
                        VerifyCaseOutcome::UnexpectedErr { err_repr } => {
                            Some(verify_unexpected_err_diagnostic(
                                &display_path,
                                &fr.source,
                                &block.block_label,
                                &cr.case_expr,
                                err_repr,
                                line,
                                col,
                            ))
                        }
                        _ => None,
                    };
                    if let Some(d) = diag {
                        if diag_count < max_diags {
                            println!();
                            print!("{}", render_tty(&d, verbose));
                        }
                        diag_count += 1;
                    }
                }
                if diag_count > max_diags {
                    println!(
                        "\n  {}",
                        format!(
                            "... and {} more (use --verbose to see all)",
                            diag_count - max_diags
                        )
                        .dimmed()
                    );
                }
            }
        }
    }
}

pub(super) fn cmd_verify(
    path: &str,
    module_root_override: Option<&str>,
    deps: bool,
    verbose: bool,
    json: bool,
    hostile: bool,
    wasm_gc: bool,
) {
    // 0.13 Limit: --hostile reruns each `verify ... law` against an adversarial
    // world. Domain side (this commit) injects boundary values per typed
    // `given`; effect side (next commit) responds with worst-case classified-
    // effect oracles; differential reporting is layered on top of both.
    let module_root = resolve_module_root(module_root_override);
    let inputs = match resolve_av_inputs(path) {
        Ok(inputs) => inputs,
        Err(e) => {
            eprintln!("{}", e.red());
            process::exit(1);
        }
    };

    let mut all_file_results: Vec<VerifyFileResult> = Vec::new();
    let mut failed_files = Vec::new();
    let mut skipped_typecheck: Vec<String> = Vec::new();
    let mut printed_any = false;

    for file in &inputs {
        match run_verify_for_file(file, &module_root, deps, hostile, wasm_gc) {
            Ok(file_results) => {
                // Render immediately — streaming output
                let has_blocks = file_results.iter().any(|fr| !fr.blocks.is_empty());
                if has_blocks && printed_any && !json {
                    println!();
                }
                render_verify_output(&file_results, &module_root, verbose, json);
                if has_blocks {
                    printed_any = true;
                }
                for fr in &file_results {
                    if fr.blocks.iter().any(|b| b.failed > 0) {
                        failed_files.push(fr.path.clone());
                    }
                }
                all_file_results.extend(file_results);
            }
            Err(e) => {
                eprintln!("{}: {}", display_check_path(file, &module_root).red(), e);
                skipped_typecheck.push(display_check_path(file, &module_root));
                failed_files.push(file.clone());
            }
        }
    }

    if !skipped_typecheck.is_empty() && !json {
        println!();
        println!(
            "{}",
            format!(
                "{} file(s) skipped — type errors (run aver check for details):",
                skipped_typecheck.len()
            )
            .yellow()
        );
        for f in &skipped_typecheck {
            println!("  {}", f.dimmed());
        }
        println!(
            "{}",
            "hint: if these files use modules, pass --module-root <dir>".dimmed()
        );
    }

    // Summary
    let total_blocks: usize = all_file_results.iter().map(|fr| fr.blocks.len()).sum();
    let total_passed: usize = all_file_results
        .iter()
        .flat_map(|fr| &fr.blocks)
        .map(|b| b.passed)
        .sum();
    let total_failed: usize = all_file_results
        .iter()
        .flat_map(|fr| &fr.blocks)
        .map(|b| b.failed)
        .sum();
    let total_skipped: usize = all_file_results
        .iter()
        .flat_map(|fr| &fr.blocks)
        .map(|b| b.skipped)
        .sum();
    let total_cases = total_passed + total_failed + total_skipped;
    let total_files = all_file_results
        .iter()
        .filter(|fr| !fr.blocks.is_empty())
        .count();

    if total_blocks == 0 {
        let scope = if deps {
            format!("{} or its transitive dependencies", path)
        } else {
            path.to_string()
        };
        if json {
            println!(
                "{{\"schema_version\":1,\"kind\":\"summary\",\"files\":0,\"blocks\":0,\"cases_passed\":0,\"cases_failed\":0}}"
            );
        } else {
            println!(
                "{}",
                format!("No verify blocks found in {}.", scope).yellow()
            );
        }
    } else if json {
        println!(
            "{{\"schema_version\":1,\"kind\":\"summary\",\"files\":{},\"blocks\":{},\"cases_passed\":{},\"cases_failed\":{}}}",
            total_files, total_blocks, total_passed, total_failed
        );
    } else {
        println!();
        // Split skipped into when-driven and base-failure-driven —
        // they have different meanings (`when` filtered the case
        // out vs Aver pre-empted a redundant profile permutation).
        use aver::checker::VerifyCaseOutcome;
        let mut skipped_when = 0usize;
        let mut skipped_base = 0usize;
        for fr in &all_file_results {
            for b in &fr.blocks {
                for cr in &b.case_results {
                    match cr.outcome {
                        VerifyCaseOutcome::Skipped => skipped_when += 1,
                        VerifyCaseOutcome::SkippedAfterBaseFail => skipped_base += 1,
                        _ => {}
                    }
                }
            }
        }
        let mut skipped_part = String::new();
        if skipped_when > 0 {
            skipped_part.push_str(&format!(" | {} skipped by `when`", skipped_when));
        }
        if skipped_base > 0 {
            skipped_part.push_str(&format!(
                " | {} skipped (base case already failed)",
                skipped_base
            ));
        }
        let summary = format!(
            "Summary: {} file{} | {} block{} | {}/{} cases passed | {} failed{}",
            total_files,
            if total_files == 1 { "" } else { "s" },
            total_blocks,
            if total_blocks == 1 { "" } else { "s" },
            total_passed,
            total_cases,
            total_failed,
            skipped_part,
        );
        if total_failed == 0 {
            println!("{}", summary.green());
        } else {
            println!("{}", summary.red());
        }
    }

    if !failed_files.is_empty() || total_failed > 0 {
        process::exit(1);
    }
}

#[allow(clippy::too_many_arguments)]
fn build_codegen_context(
    file: &str,
    project_name: Option<&str>,
    module_root_override: Option<&str>,
    with_replay: bool,
    policy_mode: &super::cli::CompilePolicyMode,
    guest_entry: Option<&str>,
    with_self_host_support: bool,
    apply_traversal_lowering: bool,
    run_refinement_lower: bool,
    run_contract_lower: bool,
    run_law_lower: bool,
) -> (codegen::CodegenContext, String) {
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

    // Compiler pipeline. The `apply_traversal_lowering` parameter at the
    // command-level API is the proof-export distinction — Lean/Dafny
    // exporters want source-level IR (interp_lower + buffer_build off),
    // runtime backends (VM/WASM/Rust) want the deforested form. See
    // `aver::ir::pipeline` for the canonical stage order and invariants.
    //
    // `with_self_host_support` flips the typecheck variant to
    // `FullSelfHost`, which bypasses opaque-type checks so
    // `domain/builtins.av` can round-trip `Tcp.Connection` (and any
    // future opaque host type) through the replay `Val` shape. User
    // code outside the self-host always goes through the regular
    // `Full` variant.
    let typecheck_mode = if with_self_host_support {
        aver::ir::TypecheckMode::FullSelfHost {
            base_dir: Some(&module_root),
        }
    } else {
        aver::ir::TypecheckMode::Full {
            base_dir: Some(&module_root),
        }
    };
    // Load dep modules BEFORE the entry pipeline runs — needed because
    // the proof-lower pipeline stage walks both entry items and dep
    // module type/fn defs in one sweep (cross-module refinement records,
    // module-spanning call graphs). load_compile_deps only reads
    // `TopLevel::Module(m).depends`, which TCO never touches, so it's
    // safe to run pre-pipeline.
    let modules = load_compile_deps(
        &items,
        &module_root,
        apply_traversal_lowering, // run_interp_lower
        apply_traversal_lowering, // run_buffer_build
        with_self_host_support,   // self_host_mode → bypass opaque in dep modules
    );

    let pipeline_result = aver::ir::pipeline::run(
        &mut items,
        aver::ir::PipelineConfig {
            typecheck: Some(typecheck_mode),
            run_interp_lower: apply_traversal_lowering,
            run_buffer_build: apply_traversal_lowering,
            run_refinement_lower,
            run_contract_lower,
            run_law_lower,
            // #138 phase E wire-up — build the symbol table for
            // every codegen context. Cheap, no consumers yet but
            // downstream PRs migrate proof IR maps to `FnId` and
            // need the table populated. Production callers always
            // get it; ad-hoc `pipeline::run` callers (tests,
            // playground) opt in via `PipelineConfig`.
            run_build_symbols: true,
            dep_modules: &modules,
            ..Default::default()
        },
    );
    let tc_result = pipeline_result.typecheck.expect("typecheck was requested");
    if !tc_result.errors.is_empty() {
        print_type_errors(&tc_result.errors);
        process::exit(1);
    }

    // Memo eligibility reads from `PipelineResult.analysis` — the analyze
    // stage already collected `recursive_fns` and `recursive_call_count`
    // facts; `compute_memo_fns` just filters by typecheck signature
    // (effects, memo-safe param types).
    let memo_fns = compute_memo_fns(&items, &tc_result, pipeline_result.analysis.as_ref());

    // Derive project name from file if not specified
    let name = project_name.map(|s| s.to_string()).unwrap_or_else(|| {
        Path::new(file)
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("aver_program")
            .to_string()
    });

    let use_runtime_policy = matches!(policy_mode, super::cli::CompilePolicyMode::Runtime);
    let use_scoped_runtime = with_replay || use_runtime_policy;

    // Runtime policy mode loads aver.toml lazily at execution time so one
    // artifact can serve multiple guest module roots.
    let policy = if use_runtime_policy {
        None
    } else {
        match load_runtime_policy(&module_root) {
            Ok(policy) => policy,
            Err(e) => {
                eprintln!("{}", e.red());
                process::exit(1);
            }
        }
    };

    // Build codegen context. `entry_analysis` carries `mutual_tco_members`,
    // `recursive_fns`, and per-fn `FnAnalysis` from the analyze stage; codegen
    // unions these with each `module.analysis` to build a global view.
    // ProofIR (when proof stages ran) comes pre-computed from the
    // pipeline; pull it across before assembly so build_context doesn't
    // redundantly recompute it.
    let prebuilt_proof_ir = pipeline_result.proof_ir;
    let mut ctx = codegen::build_context(
        items,
        &tc_result,
        pipeline_result.analysis.as_ref(),
        memo_fns,
        name,
        modules,
        pipeline_result.symbol_table,
        pipeline_result.resolved_items,
    );
    #[cfg(feature = "runtime")]
    if let Some(ir) = prebuilt_proof_ir {
        ctx.proof_ir = ir;
    }
    #[cfg(not(feature = "runtime"))]
    let _ = prebuilt_proof_ir;
    ctx.policy = policy;
    ctx.emit_replay_runtime = use_scoped_runtime;
    ctx.runtime_policy_from_env = use_runtime_policy;
    ctx.guest_entry = guest_entry.map(str::to_string);
    ctx.emit_self_host_support = with_self_host_support;
    if let Some(entry) = guest_entry
        && !ctx.fn_defs.iter().any(|fd| fd.name == entry)
    {
        eprintln!("{}", format!("Guest entry '{}' not found", entry).red());
        process::exit(1);
    }
    (ctx, module_root)
}

fn write_codegen_output(
    file: &str,
    output_dir: &str,
    target_label: &str,
    build_hint: &str,
    output: &codegen::ProjectOutput,
) {
    let out_path = Path::new(output_dir);
    if let Err(e) = materialize_codegen_output(out_path, output) {
        eprintln!("{}", e.red());
        process::exit(1);
    }

    println!(
        "{}",
        format!("Compiled {} → {}/ [{}]", file, output_dir, target_label).green()
    );
    println!("  {}", build_hint.cyan());
}

pub(super) struct BenchOptions<'a> {
    pub scenario_path: &'a str,
    pub target: &'a str,
    pub iterations: Option<usize>,
    pub warmup: Option<usize>,
    pub json: bool,
    pub save_baseline: Option<&'a str>,
    pub compare: Option<&'a str>,
    pub baseline_dir: Option<&'a str>,
    pub fail_on_regression: bool,
}

/// Pick the baseline file for the current host out of `dir`. Naming:
/// `<host.os>-<host.arch>-<backend.name>.json`. Returns `None` when no
/// match exists — the caller treats that as "skip the gate" so a single
/// CI workflow can run on multiple hosts and only gate where a baseline
/// is actually pinned.
fn pick_host_baseline(dir: &Path, target: aver::bench::BenchTarget) -> Option<std::path::PathBuf> {
    let host = aver::bench::report::HostInfo::capture();
    let filename = format!("{}-{}-{}.json", host.os, host.arch, target.name());
    let candidate = dir.join(&filename);
    if candidate.is_file() {
        Some(candidate)
    } else {
        None
    }
}

/// `aver bench (SCENARIO.toml | SCENARIO_DIR) [flags]`
///
/// Single-manifest mode runs one scenario; directory mode globs every
/// `*.toml` inside, sorts alphabetically, runs each in turn. Single-mode
/// supports `--save-baseline` / `--compare` / `--fail-on-regression`.
/// Directory mode emits NDJSON (one report per line) when `--json` is
/// passed; without `--json` it prints the human form for each scenario
/// separated by blank lines. `--compare` is single-scenario only —
/// directory-mode comparison is the 0.15.2 baseline-snapshot workflow.
pub(super) fn cmd_bench(opts: BenchOptions<'_>) {
    let target = match aver::bench::BenchTarget::parse(opts.target) {
        Ok(t) => t,
        Err(msg) => {
            eprintln!("{}", msg.red());
            process::exit(1);
        }
    };

    let scenario_path = Path::new(opts.scenario_path);
    if scenario_path.is_dir() {
        run_bench_dir(scenario_path, target, &opts);
        return;
    }

    // Two single-file shapes: `.toml` manifest (full per-scenario
    // tolerances + expected shape) or `.av` source directly (ad-hoc
    // synthesized manifest with `--iterations` / `--warmup` overrides
    // on top of defaults). Anything else falls through to manifest
    // load and surfaces whatever parse error TOML throws.
    let is_av = scenario_path
        .extension()
        .and_then(|s| s.to_str())
        .is_some_and(|ext| ext.eq_ignore_ascii_case("av"));
    let manifest = if is_av {
        if opts.compare.is_some() || opts.save_baseline.is_some() {
            eprintln!(
                "{}",
                "ad-hoc `.av` mode: --compare / --save-baseline need a `.toml` manifest with per-scenario tolerances".red()
            );
            process::exit(1);
        }
        synth_manifest_for_av(scenario_path, opts.iterations, opts.warmup)
    } else {
        match aver::bench::Manifest::load(scenario_path) {
            Ok(m) => m,
            Err(e) => {
                eprintln!("{}", format!("scenario load: {}", e).red());
                process::exit(1);
            }
        }
    };

    let report = match aver::bench::run_scenario(&manifest, target) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("{}", format!("bench run: {}", e).red());
            process::exit(1);
        }
    };

    if let Some(path) = opts.save_baseline {
        match serde_json::to_string_pretty(&report) {
            Ok(text) => {
                if let Err(e) = std::fs::write(path, format!("{}\n", text)) {
                    eprintln!("{}", format!("save-baseline write '{}': {}", path, e).red());
                    process::exit(1);
                }
                eprintln!("{}", format!("Saved baseline → {}", path).cyan());
            }
            Err(e) => {
                eprintln!("{}", format!("save-baseline JSON encode: {}", e).red());
                process::exit(1);
            }
        }
    }

    if opts.json {
        match serde_json::to_string_pretty(&report) {
            Ok(text) => println!("{}", text),
            Err(e) => {
                eprintln!("{}", format!("bench JSON encode: {}", e).red());
                process::exit(1);
            }
        }
    } else {
        print!("{}", aver::bench::format_human(&report));
    }

    let baseline_pick: Option<std::path::PathBuf> = match (opts.compare, opts.baseline_dir) {
        (Some(p), _) => Some(std::path::PathBuf::from(p)),
        (None, Some(dir)) => pick_host_baseline(Path::new(dir), target),
        _ => None,
    };
    if let Some(baseline_path) = baseline_pick {
        compare_against_baseline(&baseline_path, &report, manifest.tolerance, &opts);
    }
}

fn compare_against_baseline(
    baseline_path: &Path,
    report: &aver::bench::BenchReport,
    tolerance: aver::bench::Tolerance,
    opts: &BenchOptions<'_>,
) {
    let baseline_text = match std::fs::read_to_string(baseline_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!(
                "{}",
                format!(
                    "compare: cannot read baseline '{}': {}",
                    baseline_path.display(),
                    e
                )
                .red()
            );
            process::exit(1);
        }
    };
    // Two file shapes: a pretty-printed single `BenchReport` (the
    // `--save-baseline` output for single-scenario mode) or an NDJSON
    // file (one report per line, the dir-mode shape we use for
    // committed CI baselines). Try single first; on failure, parse
    // NDJSON and pick the entry matching the current scenario name.
    let baseline: aver::bench::BenchReport = match serde_json::from_str(&baseline_text) {
        Ok(b) => b,
        Err(_) => {
            let mut found: Option<aver::bench::BenchReport> = None;
            for line in baseline_text.lines() {
                let trimmed = line.trim();
                if trimmed.is_empty() {
                    continue;
                }
                match serde_json::from_str::<aver::bench::BenchReport>(trimmed) {
                    Ok(r) if r.scenario.name == report.scenario.name => {
                        found = Some(r);
                        break;
                    }
                    Ok(_) => continue,
                    Err(e) => {
                        eprintln!(
                            "{}",
                            format!(
                                "compare: cannot parse baseline '{}': {}",
                                baseline_path.display(),
                                e
                            )
                            .red()
                        );
                        process::exit(1);
                    }
                }
            }
            match found {
                Some(b) => b,
                None => {
                    eprintln!(
                        "{}",
                        format!(
                            "compare: baseline '{}' has no entry for scenario '{}'",
                            baseline_path.display(),
                            report.scenario.name
                        )
                        .red()
                    );
                    return;
                }
            }
        }
    };
    let diff = aver::bench::diff(report, &baseline, tolerance);
    if !opts.json {
        println!();
        print!("{}", aver::bench::format_diff(&diff));
    }
    if diff.regressed && opts.fail_on_regression {
        process::exit(1);
    }
}

/// Build an in-memory `Manifest` for the ad-hoc `.av` form. CLI flags
/// override the defaults; `[expected]` and `[tolerance]` stay at their
/// defaults — those need a real TOML manifest to opt into.
fn synth_manifest_for_av(
    av_path: &Path,
    iterations: Option<usize>,
    warmup: Option<usize>,
) -> aver::bench::Manifest {
    let name = av_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("scenario")
        .to_string();
    aver::bench::Manifest {
        name,
        entry: av_path.to_path_buf(),
        iterations: iterations.unwrap_or(30),
        warmup: warmup.unwrap_or(3),
        args: Vec::new(),
        expected: aver::bench::manifest::ExpectedShape::default(),
        tolerance: aver::bench::Tolerance::default(),
    }
}

/// Directory mode: run every `*.toml` in `dir` (alphabetical), emit one
/// report per scenario. NDJSON when `--json` is set, human-readable
/// blocks separated by blank lines otherwise. `--compare` is single-
/// scenario only (rejected here with a clear error). `--save-baseline`
/// in dir mode writes NDJSON of every report to that path — same shape
/// as `--json` output, suitable for committing as a CI baseline.
/// `--baseline-dir` loads `<DIR>/<host.os>-<host.arch>-<backend.name>.json`
/// (NDJSON) and compares each current scenario against its same-named
/// counterpart in the baseline.
fn run_bench_dir(dir: &Path, target: aver::bench::BenchTarget, opts: &BenchOptions<'_>) {
    if opts.compare.is_some() {
        eprintln!(
            "{}",
            "directory mode: --compare needs a single scenario; use --baseline-dir DIR for batch gating"
                .red()
        );
        process::exit(1);
    }

    let mut manifest_paths: Vec<std::path::PathBuf> = Vec::new();
    match std::fs::read_dir(dir) {
        Ok(entries) => {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.extension().and_then(|s| s.to_str()) == Some("toml") {
                    manifest_paths.push(path);
                }
            }
        }
        Err(e) => {
            eprintln!(
                "{}",
                format!("scenarios dir '{}': {}", dir.display(), e).red()
            );
            process::exit(1);
        }
    }
    manifest_paths.sort();

    if manifest_paths.is_empty() {
        eprintln!(
            "{}",
            format!("scenarios dir '{}' has no *.toml manifests", dir.display()).red()
        );
        process::exit(1);
    }

    let baseline_index: Option<std::collections::HashMap<String, aver::bench::BenchReport>> =
        opts.baseline_dir.and_then(|baseline_dir| {
            let baseline_path = pick_host_baseline(Path::new(baseline_dir), target)?;
            let text = match std::fs::read_to_string(&baseline_path) {
                Ok(s) => s,
                Err(e) => {
                    eprintln!(
                        "{}",
                        format!(
                            "baseline-dir: cannot read '{}': {}",
                            baseline_path.display(),
                            e
                        )
                        .red()
                    );
                    process::exit(1);
                }
            };
            let mut index: std::collections::HashMap<String, aver::bench::BenchReport> =
                std::collections::HashMap::new();
            for (lineno, line) in text.lines().enumerate() {
                let trimmed = line.trim();
                if trimmed.is_empty() {
                    continue;
                }
                match serde_json::from_str::<aver::bench::BenchReport>(trimmed) {
                    Ok(r) => {
                        index.insert(r.scenario.name.clone(), r);
                    }
                    Err(e) => {
                        eprintln!(
                            "{}",
                            format!(
                                "baseline-dir: parse error '{}' line {}: {}",
                                baseline_path.display(),
                                lineno + 1,
                                e
                            )
                            .red()
                        );
                        process::exit(1);
                    }
                }
            }
            Some(index)
        });

    let mut save_buffer: Vec<String> = Vec::new();
    let mut any_regression = false;
    let mut first = true;
    let mut diff_blocks: Vec<String> = Vec::new();
    for manifest_path in &manifest_paths {
        let manifest = match aver::bench::Manifest::load(manifest_path) {
            Ok(m) => m,
            Err(e) => {
                eprintln!("{}", format!("scenario load: {}", e).red());
                process::exit(1);
            }
        };
        let report = match aver::bench::run_scenario(&manifest, target) {
            Ok(r) => r,
            Err(e) => {
                eprintln!("{}", format!("bench run ({}): {}", manifest.name, e).red());
                process::exit(1);
            }
        };

        if let Some(baseline_idx) = baseline_index.as_ref() {
            if let Some(baseline_report) = baseline_idx.get(&manifest.name) {
                let diff = aver::bench::diff(&report, baseline_report, manifest.tolerance);
                if diff.regressed {
                    any_regression = true;
                }
                if !opts.json {
                    diff_blocks.push(aver::bench::format_diff(&diff));
                } else {
                    // In JSON mode, emit the diff as an extra NDJSON line
                    // tagged so consumers can `jq -c 'select(.kind == "diff")'`.
                    let regressed = diff.regressed;
                    let scenario = diff.scenario.clone();
                    let p50 = diff.p50;
                    let p95 = diff.p95;
                    let notes_arr: String = diff
                        .notes
                        .iter()
                        .map(|n| serde_json::to_string(n).unwrap_or_else(|_| "\"\"".to_string()))
                        .collect::<Vec<_>>()
                        .join(",");
                    println!(
                        "{{\"kind\":\"diff\",\"scenario\":{},\"regressed\":{},\"p50\":{{\"baseline_ms\":{},\"current_ms\":{},\"delta_pct\":{},\"tolerance_pct\":{},\"regressed\":{}}},\"p95\":{{\"baseline_ms\":{},\"current_ms\":{},\"delta_pct\":{},\"tolerance_pct\":{},\"regressed\":{}}},\"notes\":[{}]}}",
                        serde_json::to_string(&scenario).unwrap_or_else(|_| "\"\"".to_string()),
                        regressed,
                        p50.baseline,
                        p50.current,
                        p50.delta_pct,
                        p50.tolerance_pct,
                        p50.regressed,
                        p95.baseline,
                        p95.current,
                        p95.delta_pct,
                        p95.tolerance_pct,
                        p95.regressed,
                        notes_arr,
                    );
                }
            } else if !opts.json {
                diff_blocks.push(format!("{}: no baseline entry — skipped\n", manifest.name));
            }
        }

        if opts.save_baseline.is_some() {
            match serde_json::to_string(&report) {
                Ok(text) => save_buffer.push(text),
                Err(e) => {
                    eprintln!("{}", format!("save-baseline JSON encode: {}", e).red());
                    process::exit(1);
                }
            }
        }

        if opts.json {
            // NDJSON: one compact report per line, no surrounding array.
            // Streams trivially through `jq -c .iterations.p50_ms` etc.
            match serde_json::to_string(&report) {
                Ok(text) => println!("{}", text),
                Err(e) => {
                    eprintln!("{}", format!("bench JSON encode: {}", e).red());
                    process::exit(1);
                }
            }
        } else {
            if !first {
                println!();
            }
            print!("{}", aver::bench::format_human(&report));
        }
        first = false;
    }

    if let Some(save_path) = opts.save_baseline {
        let body = save_buffer.join("\n");
        let with_trailing = if body.is_empty() {
            String::new()
        } else {
            format!("{}\n", body)
        };
        if let Err(e) = std::fs::write(save_path, with_trailing) {
            eprintln!(
                "{}",
                format!("save-baseline write '{}': {}", save_path, e).red()
            );
            process::exit(1);
        }
        eprintln!(
            "{}",
            format!(
                "Saved baseline → {} ({} scenario(s))",
                save_path,
                save_buffer.len()
            )
            .cyan()
        );
    }

    if !diff_blocks.is_empty() && !opts.json {
        println!();
        for block in &diff_blocks {
            print!("{}", block);
        }
    }
    if any_regression && opts.fail_on_regression {
        process::exit(1);
    }
}

/// `aver compile FILE --emit-ir-after=PASS` — runs the canonical pipeline
/// (full traversal lowering, runtime shape) and prints the IR after the
/// requested stage to stdout, then exits without invoking codegen.
///
/// Stage names match `aver::ir::PipelineStage::name()` plus `parse` for
/// the pre-pipeline AST. Anything else is rejected with an error listing
/// the legal stage names.
pub(super) fn cmd_emit_ir_after(file: &str, module_root_override: Option<&str>, stage_name: &str) {
    use aver::ir::{PipelineConfig, PipelineStage, TypecheckMode, dump};

    let target_stage = match stage_name {
        "parse" => None, // pre-pipeline snapshot
        "tco" => Some(PipelineStage::Tco),
        "typecheck" => Some(PipelineStage::Typecheck),
        "interp_lower" => Some(PipelineStage::InterpLower),
        "buffer_build" => Some(PipelineStage::BufferBuild),
        "resolve" => Some(PipelineStage::Resolve),
        "last_use" => Some(PipelineStage::LastUse),
        "analyze" => Some(PipelineStage::Analyze),
        "escape" => Some(PipelineStage::Escape),
        "build_symbols" => Some(PipelineStage::BuildSymbols),
        "name_resolve" => Some(PipelineStage::NameResolve),
        "refinement_lower" => Some(PipelineStage::RefinementLower),
        "contract_lower" => Some(PipelineStage::ContractLower),
        "law_lower" => Some(PipelineStage::LawLower),
        other => {
            eprintln!(
                "{}",
                format!(
                    "unknown --emit-ir-after stage '{}'; expected one of: \
                     parse, tco, typecheck, interp_lower, buffer_build, resolve, last_use, analyze, escape, build_symbols, name_resolve, refinement_lower, contract_lower, law_lower",
                    other
                )
                .red()
            );
            process::exit(1);
        }
    };

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

    if target_stage.is_none() {
        // `--emit-ir-after=parse` — no pipeline runs, no analysis available.
        print!("{}", dump::dump_items(&items, None));
        return;
    }

    // Snapshot the IR at the requested stage. Per-fn analysis facts are
    // only attached when the snapshot was taken at or after the analyze
    // stage — earlier snapshots get rendered without facts (the FnDef
    // header collapses to its plain `fn name(...) -> T` form).
    let captured = std::cell::RefCell::new(None::<Vec<aver::ast::TopLevel>>);
    let target = target_stage.unwrap();
    let neutral_policy = aver::ir::NeutralAllocPolicy;
    // Law lowering reads `proof_ir.refined_types` to detect
    // refinement-lifted shapes — RefinementLower is a transitive
    // dependency when targeting LawLower. Without it the lifted
    // detection always returns false and the diagnostic
    // misrepresents the law's actual classification.
    let run_refinement_lower = matches!(
        target,
        PipelineStage::RefinementLower | PipelineStage::LawLower
    );
    let run_contract_lower = target == PipelineStage::ContractLower;
    let run_law_lower = target == PipelineStage::LawLower;
    let proof_target = run_refinement_lower || run_contract_lower || run_law_lower;
    // Preload deps for every diagnostic target. After Phase E PR 7.2
    // the resolved HIR contract is "well-typed ⇒ zero unresolved",
    // and that only holds when the entry pipeline has seen the
    // dep modules. Skipping deps here would make
    // `--emit-ir-after=name_resolve` show stale `<unresolved:…>`
    // markers that the production run path never emits.
    let _ = proof_target; // kept for future per-target diagnostic shape switches
    let dep_modules = load_compile_deps(&items, &module_root, false, false, false);
    let pipeline_result = aver::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full {
                base_dir: Some(&module_root),
            }),
            // `--emit-ir` is a diagnostic, so attach the neutral policy
            // — the dump's `[no_alloc]` annotation matches the shared
            // VM/WASM baseline. Codegen pipelines should pass their
            // backend-specific policy when consuming the analysis.
            alloc_policy: Some(&neutral_policy),
            run_refinement_lower,
            run_contract_lower,
            run_law_lower,
            dep_modules: &dep_modules,
            on_after_pass: Some(Box::new(|stage, items_after| {
                if stage == target {
                    *captured.borrow_mut() = Some(items_after.to_vec());
                }
            })),
            ..Default::default()
        },
    );
    if let Some(tc) = &pipeline_result.typecheck
        && !tc.errors.is_empty()
    {
        eprintln!("{}", super::shared::format_type_errors(&tc.errors).red());
        process::exit(1);
    }

    // Proof stages don't transform items — they produce a side
    // artifact. Render the lowered ProofIR (whichever fields the
    // selected stage populated) instead of the (unchanged) items list.
    if proof_target {
        match pipeline_result.proof_ir {
            Some(ir) => print!(
                "{}",
                render_proof_ir_dump(&ir, &pipeline_result.symbol_table)
            ),
            None => {
                eprintln!(
                    "{}",
                    format!(
                        "stage '{}' did not run (likely skipped after typecheck errors)",
                        stage_name
                    )
                    .red(),
                );
                process::exit(1);
            }
        }
        return;
    }

    // `build_symbols` is the same shape — side-artifact (the symbol
    // table), not item rewrite. Dump the resolved-identity layer so
    // users can inspect FnKey → FnId / TypeKey → TypeId mappings.
    if target == PipelineStage::BuildSymbols {
        print!(
            "{}",
            render_symbol_table_dump(&pipeline_result.symbol_table)
        );
        return;
    }

    // `name_resolve` — render the resolved HIR (Phase E PR 5 of #147).
    // The resolved AST lives in `pipeline_result.resolved_items`, not
    // in the item slice, because the pass is a producer rather than
    // an in-place rewrite. `dump_resolved_program` surfaces opaque
    // `FnId` / `CtorId` / `TypeId` markers so the migration is
    // visually verifiable from the CLI.
    if target == PipelineStage::NameResolve {
        print!(
            "{}",
            aver::ir::hir::dump_resolved_program(&pipeline_result.resolved_items)
        );
        return;
    }

    match captured.into_inner() {
        Some(snapshot) => {
            let analysis_for_dump = if target == PipelineStage::Analyze {
                pipeline_result.analysis.as_ref()
            } else {
                None
            };
            print!("{}", dump::dump_items(&snapshot, analysis_for_dump));
        }
        None => {
            eprintln!(
                "{}",
                format!(
                    "stage '{}' did not run (likely disabled or skipped after typecheck errors)",
                    stage_name
                )
                .red()
            );
            process::exit(1);
        }
    }
}

/// Backend-neutral textual dump of a lowered `ProofIR`. Drives
/// `aver compile FILE --emit-ir-after=proof_lower` — same lens any
/// other pipeline stage gets, scoped to the proof artifact a proof
/// exporter (Lean / Dafny) would consume. Useful for debugging
/// "why did this fn get Fuel vs Native?", "what precondition did
/// the lowerer derive?", "did this type lift to a subtype?".
fn render_proof_ir_dump(ir: &aver::ir::ProofIR, symbols: &aver::ir::SymbolTable) -> String {
    use aver::ir::{Measure, RecursionContract};
    use std::fmt::Write as _;
    let mut out = String::new();
    writeln!(out, "# ProofIR").unwrap();
    writeln!(out).unwrap();
    writeln!(out, "## refined_types ({})", ir.refined_types.len()).unwrap();
    // After phase E2 the map is keyed by opaque `TypeId`; render the
    // canonical `Module.Name` form via the symbol table so two
    // module-owned `Natural`s disambiguate in the dump.
    let type_label =
        |type_id: aver::ir::TypeId| -> String { symbols.type_entry(type_id).key.canonical() };
    let mut refined: Vec<(aver::ir::TypeId, &aver::ir::proof_ir::RefinedTypeDecl)> = ir
        .refined_types
        .iter()
        .map(|(id, decl)| (*id, decl))
        .collect();
    refined.sort_by_key(|(id, _)| type_label(*id));
    for (type_id, decl) in refined {
        let witness = decl.witness.as_deref().unwrap_or("<none>");
        writeln!(
            out,
            "- {} : {{ {} : {} // <predicate> }} witness {}",
            type_label(type_id),
            decl.predicate_param,
            decl.carrier_type,
            witness,
        )
        .unwrap();
        writeln!(
            out,
            "    carrier_field: {}    predicate: {:?}",
            decl.carrier_field, decl.invariant.expr.node,
        )
        .unwrap();
    }
    writeln!(out).unwrap();
    writeln!(out, "## fn_contracts ({})", ir.fn_contracts.len()).unwrap();
    let mut contracts: Vec<_> = ir.fn_contracts.values().collect();
    contracts.sort_by(|a, b| a.source_name.cmp(&b.source_name));
    for contract in contracts {
        write!(out, "- {} ", contract.source_name).unwrap();
        match &contract.recursion {
            None => writeln!(out, "(non-recursive)").unwrap(),
            Some(RecursionContract::Fuel { fuel_metric }) => {
                writeln!(out, "Fuel {{ {:?} }}", fuel_metric).unwrap();
            }
            Some(RecursionContract::LinearRecurrence2) => {
                writeln!(out, "LinearRecurrence2 (pair-state Nat worker)").unwrap();
            }
            Some(RecursionContract::Native {
                precondition,
                measure,
                preservation,
                decrease,
                body,
            }) => {
                let Measure::NatAbsInt { param } = measure else {
                    writeln!(out, "Native {{ measure: {:?} }}", measure).unwrap();
                    continue;
                };
                writeln!(
                    out,
                    "Native {{ measure: natAbs({}), preservation: {:?}, decrease: {:?} }}",
                    param, preservation, decrease,
                )
                .unwrap();
                if precondition.is_empty() {
                    writeln!(out, "    precondition: <none — default p ≥ 0>").unwrap();
                } else {
                    writeln!(out, "    precondition ({} clauses):", precondition.len()).unwrap();
                    for (i, clause) in precondition.iter().enumerate() {
                        writeln!(out, "      [{i}] {:?}", clause.expr.node).unwrap();
                    }
                }
                writeln!(out, "    body.base_arm_literal: {}", body.base_arm_literal).unwrap();
            }
        }
    }
    writeln!(out).unwrap();
    writeln!(out, "## law_theorems ({})", ir.law_theorems.len()).unwrap();
    let mut laws: Vec<_> = ir.law_theorems.iter().collect();
    // Render the fn identity through the symbol table so the dump
    // stays human-readable after the FnKey → FnId migration.
    let fn_label = |fn_id: aver::ir::FnId| -> String { symbols.fn_entry(fn_id).key.canonical() };
    laws.sort_by(|a, b| (fn_label(a.fn_id), &a.law_name).cmp(&(fn_label(b.fn_id), &b.law_name)));
    for theorem in laws {
        writeln!(
            out,
            "- {}::{} ({:?}, {} quantifier(s), {} premise(s))",
            fn_label(theorem.fn_id),
            theorem.law_name,
            theorem.strategy,
            theorem.quantifiers.len(),
            theorem.premises.len(),
        )
        .unwrap();
    }
    out
}

/// Backend-neutral textual dump of the resolved-identity layer.
/// Drives `aver compile FILE --emit-ir-after=build_symbols` — same
/// shape as `render_proof_ir_dump`, just for the SymbolTable
/// side-artifact instead of ProofIR. Lists every module / fn /
/// type / ctor with its opaque ID so debug sessions can verify
/// "what FnId did `Module.foo` resolve to?" without grep'ing.
fn render_symbol_table_dump(symbols: &aver::ir::SymbolTable) -> String {
    use std::fmt::Write as _;
    let mut out = String::new();
    writeln!(out, "# SymbolTable").unwrap();
    writeln!(out).unwrap();
    writeln!(out, "## modules ({})", symbols.modules.len()).unwrap();
    for (idx, m) in symbols.modules.iter().enumerate() {
        let prefix = m.prefix.as_deref().unwrap_or("<entry>");
        writeln!(out, "- ModuleId({}) = {}", idx, prefix).unwrap();
    }
    writeln!(out).unwrap();
    writeln!(out, "## fns ({})", symbols.fns.len()).unwrap();
    for (idx, fe) in symbols.fns.iter().enumerate() {
        writeln!(
            out,
            "- FnId({}) = {} (in ModuleId({}), source index {})",
            idx,
            fe.key.canonical(),
            fe.module.0,
            fe.index_in_module,
        )
        .unwrap();
    }
    writeln!(out).unwrap();
    writeln!(out, "## types ({})", symbols.types.len()).unwrap();
    for (idx, te) in symbols.types.iter().enumerate() {
        let shape = if te.is_product { "record" } else { "sum" };
        writeln!(
            out,
            "- TypeId({}) = {} ({}, {} ctor(s), in ModuleId({}))",
            idx,
            te.key.canonical(),
            shape,
            te.variants.len().max(if te.is_product { 1 } else { 0 }),
            te.module.0,
        )
        .unwrap();
    }
    writeln!(out).unwrap();
    writeln!(out, "## ctors ({})", symbols.ctors.len()).unwrap();
    for (idx, ce) in symbols.ctors.iter().enumerate() {
        let owning = &symbols.types[ce.owning_type.0 as usize];
        writeln!(
            out,
            "- CtorId({}) = {}.{} (of TypeId({}))",
            idx,
            owning.key.canonical(),
            ce.name,
            ce.owning_type.0,
        )
        .unwrap();
    }
    out
}

/// `aver compile FILE --explain-passes` — runs the canonical pipeline
/// (no codegen) and prints a per-pass diagnostic report describing what
/// each stage actually did. Defaults to a human-readable report; `json`
/// switches to a stable machine-readable shape (`schema_version: 1`)
/// consumable by CI scripts and the failable-invariant gates ("fail if
/// buffer_build no longer fires on the canonical shape", "fail if hot
/// fn loses no-alloc status").
pub(super) fn cmd_explain_passes(file: &str, module_root_override: Option<&str>, json: bool) {
    use aver::ir::{PipelineConfig, TypecheckMode};

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

    let neutral_policy = aver::ir::NeutralAllocPolicy;
    // `--explain-passes` is a diagnostic over the FULL pipeline shape,
    // including stages a runtime backend would skip (proof_lower).
    // Pre-load dep modules so proof_lower has the data to walk; without
    // them the stage would only see entry-file refinement records.
    let dep_modules = load_compile_deps(&items, &module_root, false, false, false);
    let result = aver::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full {
                base_dir: Some(&module_root),
            }),
            alloc_policy: Some(&neutral_policy),
            run_refinement_lower: true,
            run_contract_lower: true,
            run_law_lower: true,
            dep_modules: &dep_modules,
            ..Default::default()
        },
    );
    if let Some(tc) = &result.typecheck
        && !tc.errors.is_empty()
    {
        eprintln!("{}", super::shared::format_type_errors(&tc.errors).red());
        process::exit(1);
    }

    if json {
        print!("{}", render_pass_diagnostics_json(&result.pass_diagnostics));
    } else {
        print!("{}", render_pass_diagnostics(&result.pass_diagnostics));
    }
}

fn render_pass_diagnostics(diags: &[aver::ir::pipeline::PassDiagnostic]) -> String {
    use aver::ir::pipeline::PassReport;
    let mut out = String::new();
    out.push_str("compiler pipeline — per-pass report\n");
    out.push_str("====================================\n\n");
    for diag in diags {
        let label = format!("[{}]", diag.stage.name());
        match &diag.report {
            PassReport::Tco {
                tail_calls_added,
                fns_changed,
                non_tail_recursive,
            } => {
                if *tail_calls_added == 0 {
                    out.push_str(&format!("{label} no calls converted to tail calls\n"));
                } else {
                    out.push_str(&format!(
                        "{label} {tail_calls_added} callsite(s) converted to tail calls\n"
                    ));
                }
                for c in fns_changed {
                    out.push_str(&format!(
                        "  • {}: {} → {} tail call(s)\n",
                        c.name, c.before, c.after
                    ));
                }
                if !non_tail_recursive.is_empty() {
                    let total_calls: usize =
                        non_tail_recursive.iter().map(|w| w.recursive_calls).sum();
                    out.push_str(&format!(
                        "  • {} non-tail recursive callsite(s) remain in {} fn(s)\n",
                        total_calls,
                        non_tail_recursive.len()
                    ));
                }
            }
            PassReport::Typecheck {
                items_checked,
                errors,
                error_messages,
            } => {
                if *errors == 0 {
                    out.push_str(&format!(
                        "{label} {items_checked} top-level item(s) checked, no errors\n"
                    ));
                } else {
                    out.push_str(&format!("{label} {errors} type error(s)\n"));
                    for msg in error_messages {
                        out.push_str(&format!("  • {msg}\n"));
                    }
                }
            }
            PassReport::InterpLower {
                interpolations_lowered,
                fns_changed,
            } => {
                if *interpolations_lowered == 0 {
                    out.push_str(&format!("{label} no interpolations to lower\n"));
                } else {
                    out.push_str(&format!(
                        "{label} {interpolations_lowered} interpolation literal(s) lowered to buffer pipeline\n"
                    ));
                }
                for c in fns_changed {
                    out.push_str(&format!(
                        "  • {}: {} → {} interpolation(s)\n",
                        c.name, c.before, c.after
                    ));
                }
            }
            PassReport::BufferBuild(r) => {
                if r.rewrites == 0 {
                    out.push_str(&format!(
                        "{label} no fusion sites detected on canonical String.join shape\n"
                    ));
                } else {
                    out.push_str(&format!(
                        "{label} {} fusion site(s) rewritten, {} buffered variant(s) synthesized\n",
                        r.rewrites,
                        r.synthesized.len()
                    ));
                    for (sink, count) in &r.rewrites_by_sink {
                        out.push_str(&format!("  • sink {sink}: {count} rewrite(s)\n"));
                    }
                    for fn_name in &r.synthesized {
                        out.push_str(&format!("  • synthesized {fn_name}\n"));
                    }
                }
            }
            PassReport::Resolve {
                slots_resolved,
                fns_with_slots,
                slot_types_total,
                slot_types_invalid,
            } => {
                out.push_str(&format!(
                    "{label} {slots_resolved} ident(s) resolved to slot lookups across {fns_with_slots} fn(s); {slot_types_total} typed slot(s) ({slot_types_invalid} invalid)\n"
                ));
            }
            PassReport::LastUse {
                last_use_marked,
                total_resolved,
            } => {
                out.push_str(&format!(
                    "{label} {last_use_marked} of {total_resolved} resolved slot(s) marked last-use (move-eligible)\n"
                ));
            }
            PassReport::Analyze {
                total_fns,
                no_alloc_fns,
                recursive_fns,
                mutual_tco_members,
                unknown_alloc,
            } => {
                out.push_str(&format!(
                    "{label} {total_fns} fn(s) analyzed: {no_alloc_fns} no-alloc, {recursive_fns} recursive, {mutual_tco_members} mutual-TCO member(s)\n"
                ));
                if *unknown_alloc > 0 {
                    out.push_str(&format!(
                        "  • {unknown_alloc} fn(s) skipped alloc classification (no policy supplied)\n"
                    ));
                }
            }
            PassReport::Escape { rewrites } => {
                if *rewrites == 0 {
                    out.push_str(&format!(
                        "{label} no fresh-alloc-immediate-consume sites detected\n"
                    ));
                } else {
                    out.push_str(&format!(
                        "{label} {rewrites} call site(s) rewritten — record/variant alloc eliminated\n"
                    ));
                }
            }
            PassReport::RefinementLower { refined_types } => {
                out.push_str(&format!(
                    "{label} {refined_types} refined type(s) lifted to subtype/subset\n"
                ));
            }
            PassReport::ContractLower { fn_contracts } => {
                out.push_str(&format!("{label} {fn_contracts} fn contract(s) decided\n"));
            }
            PassReport::LawLower { law_theorems } => {
                out.push_str(&format!(
                    "{label} {law_theorems} verify-law theorem(s) lowered\n"
                ));
            }
            PassReport::BuildSymbols {
                fns,
                types,
                ctors,
                modules,
                fn_name_collisions,
                type_name_collisions,
            } => {
                out.push_str(&format!(
                    "{label} symbol table: {} module(s), {fns} fn(s), \
                     {types} type(s), {ctors} ctor(s)\n",
                    modules.len()
                ));
                if *fn_name_collisions > 0 || *type_name_collisions > 0 {
                    out.push_str(&format!(
                        "  • bare-name collisions resolved by opaque ID: \
                         {fn_name_collisions} fn name(s), {type_name_collisions} type name(s)\n"
                    ));
                }
                for m in modules {
                    let scope = if m.prefix.is_empty() {
                        "<entry>"
                    } else {
                        m.prefix.as_str()
                    };
                    out.push_str(&format!(
                        "  • {scope}: {} fn(s), {} type(s), {} ctor(s)\n",
                        m.fns, m.types, m.ctors
                    ));
                }
            }
            PassReport::NameResolve {
                promoted_fns,
                passthrough_items,
                unresolved_count,
            } => {
                out.push_str(&format!(
                    "{label} resolved HIR: {promoted_fns} fn(s) promoted, \
                     {passthrough_items} item(s) passthrough, \
                     {unresolved_count} unresolved\n"
                ));
            }
        }
        out.push('\n');
    }
    out
}

fn render_pass_diagnostics_json(diags: &[aver::ir::pipeline::PassDiagnostic]) -> String {
    use aver::diagnostics::json_escape;
    use aver::ir::pipeline::PassReport;

    fn json_str(s: &str) -> String {
        json_escape(s)
    }
    fn json_str_array(items: &[String]) -> String {
        let mut out = String::from("[");
        for (i, s) in items.iter().enumerate() {
            if i > 0 {
                out.push(',');
            }
            out.push_str(&json_str(s));
        }
        out.push(']');
        out
    }
    fn json_fn_change(c: &aver::ir::pipeline::FnCountChange) -> String {
        format!(
            "{{\"name\":{},\"before\":{},\"after\":{}}}",
            json_str(&c.name),
            c.before,
            c.after
        )
    }
    fn json_fn_changes(cs: &[aver::ir::pipeline::FnCountChange]) -> String {
        let mut out = String::from("[");
        for (i, c) in cs.iter().enumerate() {
            if i > 0 {
                out.push(',');
            }
            out.push_str(&json_fn_change(c));
        }
        out.push(']');
        out
    }

    let mut out = String::new();
    out.push_str("{\"schema_version\":1,\"passes\":[");
    for (i, d) in diags.iter().enumerate() {
        if i > 0 {
            out.push(',');
        }
        out.push_str(&format!(
            "{{\"stage\":{},\"data\":",
            json_str(d.stage.name())
        ));
        match &d.report {
            PassReport::Tco {
                tail_calls_added,
                fns_changed,
                non_tail_recursive,
            } => {
                let mut nontail = String::from("[");
                for (j, w) in non_tail_recursive.iter().enumerate() {
                    if j > 0 {
                        nontail.push(',');
                    }
                    nontail.push_str(&format!(
                        "{{\"fn\":{},\"recursive_calls\":{},\"line\":{}}}",
                        json_str(&w.fn_name),
                        w.recursive_calls,
                        w.line
                    ));
                }
                nontail.push(']');
                out.push_str(&format!(
                    "{{\"tail_calls_added\":{},\"fns_changed\":{},\"non_tail_recursive\":{}}}",
                    tail_calls_added,
                    json_fn_changes(fns_changed),
                    nontail
                ));
            }
            PassReport::Typecheck {
                items_checked,
                errors,
                error_messages,
            } => {
                out.push_str(&format!(
                    "{{\"items_checked\":{},\"errors\":{},\"error_messages\":{}}}",
                    items_checked,
                    errors,
                    json_str_array(error_messages)
                ));
            }
            PassReport::InterpLower {
                interpolations_lowered,
                fns_changed,
            } => {
                out.push_str(&format!(
                    "{{\"interpolations_lowered\":{},\"fns_changed\":{}}}",
                    interpolations_lowered,
                    json_fn_changes(fns_changed)
                ));
            }
            PassReport::BufferBuild(r) => {
                let mut by_sink = String::from("{");
                for (j, (k, v)) in r.rewrites_by_sink.iter().enumerate() {
                    if j > 0 {
                        by_sink.push(',');
                    }
                    by_sink.push_str(&format!("{}:{}", json_str(k), v));
                }
                by_sink.push('}');
                out.push_str(&format!(
                    "{{\"rewrites\":{},\"synthesized\":{},\"sinks\":{},\"rewrites_by_sink\":{}}}",
                    r.rewrites,
                    json_str_array(&r.synthesized),
                    json_str_array(&r.sink_fns),
                    by_sink
                ));
            }
            PassReport::Resolve {
                slots_resolved,
                fns_with_slots,
                slot_types_total,
                slot_types_invalid,
            } => {
                out.push_str(&format!(
                    "{{\"slots_resolved\":{},\"fns_with_slots\":{},\"slot_types_total\":{},\"slot_types_invalid\":{}}}",
                    slots_resolved, fns_with_slots, slot_types_total, slot_types_invalid
                ));
            }
            PassReport::LastUse {
                last_use_marked,
                total_resolved,
            } => {
                out.push_str(&format!(
                    "{{\"last_use_marked\":{},\"total_resolved\":{}}}",
                    last_use_marked, total_resolved
                ));
            }
            PassReport::Analyze {
                total_fns,
                no_alloc_fns,
                recursive_fns,
                mutual_tco_members,
                unknown_alloc,
            } => {
                out.push_str(&format!(
                    "{{\"total_fns\":{},\"no_alloc_fns\":{},\"recursive_fns\":{},\"mutual_tco_members\":{},\"unknown_alloc\":{}}}",
                    total_fns, no_alloc_fns, recursive_fns, mutual_tco_members, unknown_alloc
                ));
            }
            PassReport::Escape { rewrites } => {
                out.push_str(&format!("{{\"rewrites\":{}}}", rewrites));
            }
            PassReport::RefinementLower { refined_types } => {
                out.push_str(&format!("{{\"refined_types\":{}}}", refined_types));
            }
            PassReport::ContractLower { fn_contracts } => {
                out.push_str(&format!("{{\"fn_contracts\":{}}}", fn_contracts));
            }
            PassReport::LawLower { law_theorems } => {
                out.push_str(&format!("{{\"law_theorems\":{}}}", law_theorems));
            }
            PassReport::BuildSymbols {
                fns,
                types,
                ctors,
                modules,
                fn_name_collisions,
                type_name_collisions,
            } => {
                out.push_str(&format!(
                    "{{\"fns\":{fns},\"types\":{types},\"ctors\":{ctors},\
                     \"fn_name_collisions\":{fn_name_collisions},\
                     \"type_name_collisions\":{type_name_collisions},\
                     \"modules\":["
                ));
                let mut first = true;
                for m in modules {
                    if !first {
                        out.push(',');
                    }
                    first = false;
                    out.push_str(&format!(
                        "{{\"prefix\":{},\"fns\":{},\"types\":{},\"ctors\":{}}}",
                        json_escape(&m.prefix),
                        m.fns,
                        m.types,
                        m.ctors
                    ));
                }
                out.push_str("]}");
            }
            PassReport::NameResolve {
                promoted_fns,
                passthrough_items,
                unresolved_count,
            } => {
                out.push_str(&format!(
                    "{{\"promoted_fns\":{promoted_fns},\
                     \"passthrough_items\":{passthrough_items},\
                     \"unresolved_count\":{unresolved_count}}}"
                ));
            }
        }
        out.push('}');
    }
    out.push_str("]}\n");
    out
}

pub(super) fn cmd_compile(opts: CompileOptions<'_>) {
    let CompileOptions {
        file,
        output_dir,
        project_name,
        module_root_override,
        target,
        with_replay,
        policy_mode,
        guest_entry,
        with_self_host_support,
        pack,
        handler,
        world,
        optimize,
    } = opts;

    // `--target wasip2` follows its own pipeline: wasm-gc lowering
    // gives the core module, then `wit-component` wraps it as a
    // Component. Lives in `src/codegen/wasip2/` so the wasm-gc path
    // stays untouched. Phase 1 of 0.18 "Span" — see `docs/wasip2.md`
    // for the contract.
    if matches!(target, super::cli::CompileTarget::Wasip2) {
        cmd_compile_wasip2(
            file,
            output_dir,
            project_name,
            module_root_override,
            world,
            optimize,
            handler,
        );
        return;
    }

    // `--target wasm-gc`: native engine GC + tail calls, no custom
    // runtime. Replay / policy / guest-entry plumbing not wired here
    // — Rust target remains the primary host for those concerns.
    if matches!(target, super::cli::CompileTarget::WasmGc) {
        #[cfg(feature = "wasm")]
        {
            cmd_compile_wasm_gc(
                file,
                output_dir,
                project_name,
                module_root_override,
                handler,
                optimize,
                pack,
            );
            return;
        }
        #[cfg(not(feature = "wasm"))]
        {
            let _ = (handler, optimize, pack);
            eprintln!(
                "{}",
                "WASM target requires --features wasm (rebuild with: \
                 cargo build --features wasm)"
                    .red()
            );
            process::exit(1);
        }
    }

    if guest_entry.is_some()
        && !with_replay
        && !matches!(policy_mode, super::cli::CompilePolicyMode::Runtime)
    {
        eprintln!(
            "{}",
            "--guest-entry requires either --with-replay or --policy runtime".red()
        );
        process::exit(1);
    }

    if with_self_host_support && guest_entry.is_none() {
        eprintln!(
            "{}",
            "--with-self-host-support requires --guest-entry".red()
        );
        process::exit(1);
    }

    if with_self_host_support
        && !with_replay
        && !matches!(policy_mode, super::cli::CompilePolicyMode::Runtime)
    {
        eprintln!(
            "{}",
            "--with-self-host-support requires either --with-replay or --policy runtime".red()
        );
        process::exit(1);
    }

    let (mut ctx, _module_root) = build_codegen_context(
        file,
        project_name,
        module_root_override,
        with_replay,
        policy_mode,
        guest_entry,
        with_self_host_support,
        true,  // apply_traversal_lowering — Rust target wants the optimized form
        false, // run_refinement_lower — runtime backend, doesn't need ProofIR
        false, // run_contract_lower — same
        false, // run_law_lower — same
    );
    if let Err(err) = validate_self_host_guest_entry_contract(&ctx) {
        eprintln!("{}", err.red());
        process::exit(1);
    }
    if codegen_uses_self_host_runtime(&ctx) && !with_self_host_support {
        eprintln!(
            "{}",
            "This program uses SelfHostRuntime.* builtins; re-run with --with-self-host-support"
                .red()
        );
        process::exit(1);
    }
    let output = with_local_runtime_override(|| rust_codegen::transpile(&mut ctx));
    let build_hint = format!("cd {} && cargo build && cargo run", output_dir);
    write_codegen_output(file, output_dir, "Rust", &build_hint, &output);
}

/// `aver compile FILE --target=wasm-gc` — 0.16 probe backend.
/// Type-direct lowering, no custom runtime, native tail calls. Phase-1:
/// only `fn main() -> Int <int_literal>` compiles; everything else
/// surfaces an `Unimplemented` error pointing at the relevant phase
/// in the README.
#[cfg(feature = "wasm")]
fn cmd_compile_wasm_gc(
    file: &str,
    output_dir: &str,
    project_name: Option<&str>,
    module_root_override: Option<&str>,
    handler: Option<&str>,
    optimize: Option<super::cli::WasmOptMode>,
    pack: Option<super::cli::DeployPack>,
) {
    use aver::codegen::wasm_gc;

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

    use aver::ir::{PipelineConfig, TypecheckMode};
    let neutral_policy = aver::ir::NeutralAllocPolicy;
    let result = aver::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full {
                base_dir: Some(&module_root),
            }),
            alloc_policy: Some(&neutral_policy),
            // wasm-gc backend lowers `Expr::InterpolatedStr` natively
            // to a `String.concat` chain (immutable arrays match the
            // engine's GC primitives). The `__buf_*` pipeline that
            // `interp_lower` produces targets bump-allocator backends
            // (legacy wasm, VM); for wasm-gc it would force us to
            // emulate a mutable buffer over `(struct len array)` with
            // grow-on-append, when `array.copy` x2 is the idiomatic
            // shape. Keep the source InterpolatedStr in the IR.
            run_interp_lower: false,
            run_buffer_build: false,
            ..Default::default()
        },
    );
    if let Some(tc) = &result.typecheck
        && !tc.errors.is_empty()
    {
        eprintln!("{}", super::shared::format_type_errors(&tc.errors).red());
        process::exit(1);
    }

    // Multi-module: load `depends [...]` modules and flatten them
    // into a single item list with module-prefixed fn names. Single-
    // binary linking — every reachable fn from every module ends up
    // in the same wasm module, so cross-module calls are plain
    // `call $fn` after rewriting `Attr(Ident("Fractal"), "render")`
    // call sites to `Ident("Fractal_render")`. Component Model is a
    // future separate mode (see `project_wasm_gc_multimodule.md`).
    let dep_modules = load_compile_deps(
        &items,
        &module_root,
        false, /* run_interp_lower */
        false, /* run_buffer_build */
        false, /* self_host_mode — wasm-gc compile path doesn't use self-host */
    );
    flatten_multimodule(&mut items, &dep_modules);
    // Re-run resolver after flatten so dep fns get a FnResolution
    // (slot_types). Entry items already had one from `pipeline::run`
    // above; this picks up the newly appended dep FnDefs.
    aver::ir::pipeline::resolve(&mut items);

    let bytes =
        match wasm_gc::compile_to_wasm_gc_with_handler(&items, result.analysis.as_ref(), handler) {
            Ok(b) => b,
            Err(e) => {
                eprintln!("{}", format!("{e}").red());
                process::exit(1);
            }
        };

    let out_path = Path::new(output_dir);
    if let Err(e) = std::fs::create_dir_all(out_path) {
        eprintln!(
            "{}",
            format!("Failed to create output directory: {}", e).red()
        );
        process::exit(1);
    }
    let wasm_name = project_name.map(|s| s.to_string()).unwrap_or_else(|| {
        Path::new(file)
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("program")
            .to_string()
    });
    let wasm_file = out_path.join(format!("{}.wasm", wasm_name));
    if let Err(e) = std::fs::write(&wasm_file, &bytes) {
        eprintln!("{}", format!("Failed to write WASM file: {}", e).red());
        process::exit(1);
    }
    // Optional `--optimize` post-pass — see `run_optimize_pipeline`.
    let (final_size, opt_suffix) = finalize_wasm_artifact(&wasm_file, optimize);
    println!(
        "{} wasm-gc → {} ({} bytes{})",
        "•".cyan(),
        wasm_file.display().to_string().cyan(),
        final_size,
        opt_suffix
    );
    // Deployment pack — drops platform-specific bootstrap files
    // next to the wasm-gc artifact. Same call site as the legacy
    // backend; the worker.js template is wasm-gc-aware (LM string
    // transport + `aver_http_handle` synth wrapper).
    if let Some(super::cli::DeployPack::Cloudflare) = pack {
        emit_cloudflare_pack(out_path, &wasm_name, &wasm_file);
    }
}

/// `--target wasip2` compile entry — 0.18 "Span".
///
/// The wasm-gc backend produces a core module that already imports
/// canonical-ABI WIT functions (e.g. `wasi:cli/stdout@0.2.4`,
/// `wasi:filesystem/preopens@0.2.4`, `wasi:io/streams@0.2.4`).
/// `wit-component` wraps it as a Component bound to the chosen WIT
/// world — no preview-1 adapter, no shim layer; effects lower
/// directly. Outputs:
///
/// - `out/<name>.component.wasm` — the component bytes
/// - `out/<name>.wit` — the component contract in WIT, per
///   `docs/wasip2.md` point 5
///
/// Effect surface today: `Console.print/error/warn`,
/// `Console.readLine`, `Time.unixMs/now/sleep`, `Random.int/float`,
/// `Args.get`, `Env.get`, all `Disk.*`. Effects that the wasip2
/// pipeline cannot lower (`Terminal.*`, `Http.*`, `HttpServer.*`,
/// `Tcp.*`) are rejected at this command's entry — see
/// `docs/wasip2.md` "Why X is rejected, not stubbed" for the
/// dynamic-host vs static-target axis.
fn cmd_compile_wasip2(
    file: &str,
    output_dir: &str,
    project_name: Option<&str>,
    module_root_override: Option<&str>,
    world: super::cli::Wasip2World,
    optimize: Option<super::cli::WasmOptMode>,
    handler: Option<&str>,
) {
    #[cfg(not(feature = "wasip2"))]
    {
        let _ = (
            file,
            output_dir,
            project_name,
            module_root_override,
            world,
            optimize,
            handler,
        );
        eprintln!(
            "{}",
            "--target wasip2 requires --features wasip2 \
             (rebuild with: cargo build --features wasip2)"
                .red()
        );
        process::exit(1);
    }

    #[cfg(feature = "wasip2")]
    {
        use aver::codegen::{wasip2 as wasip2_codegen, wasm_gc};

        // `--optimize` runs `wasm-opt` against a core module, which
        // doesn't yet handle wasm-gc + Component Model bytes cleanly
        // upstream. Rather than silently drop the flag, reject it
        // with a clear diagnostic — the wasm-gc target accepts it
        // for the legacy core flow.
        if optimize.is_some() {
            eprintln!(
                "{}",
                "--optimize is not supported on `--target wasip2`: wasm-opt does not yet \
                 handle wasm-gc + Component Model output. Use `--target wasm-gc` if you \
                 need post-pass size/speed optimization."
                    .red()
            );
            process::exit(1);
        }

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

        use aver::ir::{PipelineConfig, TypecheckMode};
        let neutral_policy = aver::ir::NeutralAllocPolicy;
        let result = aver::ir::pipeline::run(
            &mut items,
            PipelineConfig {
                typecheck: Some(TypecheckMode::Full {
                    base_dir: Some(&module_root),
                }),
                alloc_policy: Some(&neutral_policy),
                run_interp_lower: false,
                run_buffer_build: false,
                ..Default::default()
            },
        );
        if let Some(tc) = &result.typecheck
            && !tc.errors.is_empty()
        {
            eprintln!("{}", super::shared::format_type_errors(&tc.errors).red());
            process::exit(1);
        }

        let dep_modules = load_compile_deps(&items, &module_root, false, false, false);
        // Bypass the `flatten_multimodule` shim in this file (gated on
        // the `wasm` feature) and call the wasm-gc library function
        // directly — `wasip2` enables `wasm-compile` (which exposes
        // it) but does not pull `wasm`.
        aver::codegen::wasm_gc::flatten_multimodule(&mut items, &dep_modules);
        aver::ir::pipeline::resolve(&mut items);

        // Phase 1.6 — static effect-set check. Catches every Aver
        // effect that `--target wasip2` cannot lower today, BEFORE
        // wasm-gc emits anything. Three categories: permanent (WASI
        // 0.2 cannot satisfy by design), out-of-release (Phase 2/3
        // / 0.19+), and pending-phase (planned but not yet wired in
        // 0.18). All surfaced as `target-effect-unsupported` so the
        // user sees one consistent error class. See
        // docs/wasip2.md "Why X is rejected, not stubbed".
        if let Err(unsupported) = wasip2_codegen::check_supported_effects(&items) {
            eprintln!(
                "{}",
                format!(
                    "error[target-effect-unsupported]: \
                     {} effect site(s) cannot be lowered by `--target wasip2`",
                    unsupported.len()
                )
                .red()
            );
            eprintln!("{}", wasip2_codegen::render_errors(&unsupported).yellow());
            eprintln!(
                "{}",
                "  See docs/wasip2.md (\"Why X is rejected, not stubbed\") \
                 for the static-target vs dynamic-host axis."
                    .yellow()
            );
            process::exit(1);
        }

        // Phase 1.2b1 — wasip2 path goes through its own wasm-gc
        // entry. At commit 1.2b1.1 this is plumbing only (delegates
        // to the same `emit_module_with` body as `--target wasm-gc`),
        // but every later commit in this phase changes the bytes
        // produced under `TargetMode::Wasip2`.
        //
        // `--world wasi:http/proxy` (0.19 Phase 3) splits off here:
        // the `--handler X` flag (same flag the wasm-gc + Cloudflare
        // path uses) names the user fn that becomes the proxy
        // handler. No magic detection from `main` — the source can
        // still have `HttpServer.listen(port, handler)` in main for
        // `aver run` local execution (VM honours it; codegen lowers
        // it to a no-op on wasip2 proxy), but the codegen path
        // reads the handler identity from the flag alone.
        let core_bytes = if matches!(world, super::cli::Wasip2World::HttpProxy) {
            let handler_name = handler.unwrap_or_else(|| {
                eprintln!(
                    "{}",
                    "--world wasi:http/proxy requires --handler <fn> naming the user fn \
                     with signature Fn(HttpRequest) -> HttpResponse. Same flag the wasm-gc + \
                     Cloudflare path uses; pick whatever fn is your request handler."
                        .red()
                );
                process::exit(1);
            });
            match wasm_gc::compile_to_wasm_gc_for_wasip2_with_handler(
                &items,
                result.analysis.as_ref(),
                handler_name,
            ) {
                Ok(b) => b,
                Err(e) => {
                    eprintln!("{}", format!("{e}").red());
                    process::exit(1);
                }
            }
        } else {
            if handler.is_some() {
                eprintln!(
                    "{}",
                    "--handler is only meaningful with --world wasi:http/proxy on \
                     `--target wasip2` (the proxy world's `incoming-handler.handle` \
                     export needs a handler fn name). Drop the flag for the default \
                     `wasi:cli/command` world."
                        .red()
                );
                process::exit(1);
            }
            match wasm_gc::compile_to_wasm_gc_for_wasip2(&items, result.analysis.as_ref()) {
                Ok(b) => b,
                Err(e) => {
                    eprintln!("{}", format!("{e}").red());
                    process::exit(1);
                }
            }
        };

        let world_codegen = match world {
            super::cli::Wasip2World::CliCommand => wasip2_codegen::Wasip2World::CliCommand,
            super::cli::Wasip2World::HttpProxy => wasip2_codegen::Wasip2World::HttpProxy,
        };

        let (component_bytes, wit_source) =
            match wasip2_codegen::compile_to_component(&core_bytes, world_codegen) {
                Ok(p) => p,
                Err(e) => {
                    eprintln!("{}", format!("{e}").red());
                    eprintln!(
                        "{}",
                        "  hint: Phase 1.6 already rejects every effect that \
                         `--target wasip2` cannot lower today, so this failure \
                         points at a wasm-gc emit shape the component model did \
                         not expect (rare). Report with the program that \
                         triggered it."
                            .yellow()
                    );
                    process::exit(1);
                }
            };

        let out_path = Path::new(output_dir);
        if let Err(e) = std::fs::create_dir_all(out_path) {
            eprintln!(
                "{}",
                format!("Failed to create output directory: {}", e).red()
            );
            process::exit(1);
        }
        let stem = project_name.map(|s| s.to_string()).unwrap_or_else(|| {
            Path::new(file)
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("program")
                .to_string()
        });
        let component_file = out_path.join(format!("{}.component.wasm", stem));
        let wit_file = out_path.join(format!("{}.wit", stem));

        if let Err(e) = std::fs::write(&component_file, &component_bytes) {
            eprintln!("{}", format!("Failed to write component file: {}", e).red());
            process::exit(1);
        }
        if let Err(e) = std::fs::write(&wit_file, &wit_source) {
            eprintln!("{}", format!("Failed to write WIT file: {}", e).red());
            process::exit(1);
        }

        println!(
            "{} wasip2 → {} ({} bytes, world {})",
            "•".cyan(),
            component_file.display().to_string().cyan(),
            component_bytes.len(),
            world_codegen.wit_name(),
        );
        println!(
            "{}        {}",
            "•".cyan(),
            wit_file.display().to_string().cyan(),
        );
    }
}

/// `worker.js` template for the Cloudflare Workers pack. Lives as a
/// real `.js` file under `src/main/templates/cloudflare/` so editor
/// tooling (syntax highlighting, ESLint, prettier) treats it like
/// JavaScript instead of a Rust-side `format!` literal. The single
/// `__WASM_NAME__` placeholder is the only thing we substitute at
/// pack time — everything else is identical across packs.
#[cfg(feature = "wasm")]
const CLOUDFLARE_WORKER_JS: &str = include_str!("templates/cloudflare/worker.js");

/// `wrangler.toml` template for the Cloudflare Workers pack — same
/// rationale as `CLOUDFLARE_WORKER_JS`. `__WASM_NAME__` is the only
/// substitution.
#[cfg(feature = "wasm")]
const CLOUDFLARE_WRANGLER_TOML: &str = include_str!("templates/cloudflare/wrangler.toml");

/// Drop a Cloudflare Workers deployment pack next to the compiled
/// `user.wasm`: a `worker.js` bootstrap that loads the wasm and wires
/// `aver/*` host imports against JS APIs (`console.log`, `Date.now()`,
/// `crypto.getRandomValues`), plus a `wrangler.toml` template the
/// user can `wrangler deploy` directly. Pack is intentionally minimal
/// — only `Console.*`, `Time.unixMs`, and `Random.*` are wired today;
/// HTTP request handling lands in a follow-up.
#[cfg(feature = "wasm")]
fn emit_cloudflare_pack(out_path: &Path, wasm_name: &str, wasm_file: &Path) {
    let worker_path = out_path.join("worker.js");
    let wrangler_path = out_path.join("wrangler.toml");

    let worker_js = CLOUDFLARE_WORKER_JS.replace("__WASM_NAME__", wasm_name);
    let wrangler_toml = CLOUDFLARE_WRANGLER_TOML.replace("__WASM_NAME__", wasm_name);

    // worker.js is the host-bridge between user.wasm and the
    // Workers JS environment — it tracks the compiler's `aver/*`
    // import surface, so we always regenerate it. User edits to
    // worker.js between regens are not the supported path.
    if let Err(e) = std::fs::write(&worker_path, worker_js) {
        eprintln!(
            "{}",
            format!("Failed to write {}: {}", worker_path.display(), e).red()
        );
        return;
    }

    // wrangler.toml is *user-customisable* deployment config:
    // worker name, custom domain routes, observability toggles,
    // KV/D1 bindings, secrets, etc. Once written, never overwrite —
    // the regen path is "compiler refreshes app.wasm and worker.js,
    // user keeps their wrangler.toml". A first run drops the
    // template; subsequent runs leave it alone.
    let wrangler_existed = wrangler_path.exists();
    if !wrangler_existed && let Err(e) = std::fs::write(&wrangler_path, wrangler_toml) {
        eprintln!(
            "{}",
            format!("Failed to write {}: {}", wrangler_path.display(), e).red()
        );
        return;
    }

    let wrangler_note = if wrangler_existed { " (preserved)" } else { "" };
    println!(
        "{} {} + {}{} ({})",
        "  Pack".green().bold(),
        worker_path.display().to_string().cyan(),
        wrangler_path.display().to_string().cyan(),
        wrangler_note.dimmed(),
        format!("Cloudflare Workers, paired with {}", wasm_file.display()).dimmed()
    );
}

/// Run the post-codegen WASM tail: optionally run wasm-opt. Returns
/// (final_size, suffix) for the existing `Compiled X → Y (size, suffix)`
/// print line.
///
/// WAT companion output is intentionally not provided — the name section
/// emitted by codegen makes the binary readable through standard tooling
/// (`wasm-tools print program.wasm`). For pre-opt builds, names survive;
/// for post-opt, `wasm-opt -Oz` strips the section by design.
#[cfg(feature = "wasm")]
fn finalize_wasm_artifact(
    wasm_file: &Path,
    optimize: Option<super::cli::WasmOptMode>,
) -> (u64, String) {
    let mut final_size = std::fs::metadata(wasm_file).map(|m| m.len()).unwrap_or(0);
    let mut compile_suffix = String::new();
    if let Some(mode) = optimize {
        final_size = run_optimize_pipeline(wasm_file, mode).unwrap_or_else(|err| {
            eprintln!("{}", err.red());
            process::exit(1);
        });
        compile_suffix = format!(", optimized for {}", optimize_label(mode));
    }
    (final_size, compile_suffix)
}

#[cfg(feature = "wasm")]
fn optimize_label(mode: super::cli::WasmOptMode) -> &'static str {
    match mode {
        super::cli::WasmOptMode::O3 => "speed",
        super::cli::WasmOptMode::Oz => "size",
    }
}

/// `--optimize` post-pass for wasm-gc artifacts. Skips wasm-metadce —
/// factory exports + `__rt_*` LM transport helpers are host-callable
/// roots and a metadce graph would have to enumerate every conditional
/// export by hand. `wasm-opt -Oz` keeps the export surface and converges
/// on a smaller body — that's where the per-instantiation helpers (Map
/// probes, List ops, eq helpers) shrink when unreachable.
#[cfg(feature = "wasm")]
fn run_optimize_pipeline(wasm_file: &Path, mode: super::cli::WasmOptMode) -> Result<u64, String> {
    let input_size = std::fs::metadata(wasm_file)
        .map(|meta| meta.len())
        .map_err(|e| format!("Failed to stat {}: {}", wasm_file.display(), e))?;
    let stage1_file = wasm_file.with_extension("dce.wasm");
    let optimized_file = wasm_file.with_extension("opt.wasm");
    let opt_flag = match mode {
        super::cli::WasmOptMode::O3 => "-O3",
        super::cli::WasmOptMode::Oz => "-Oz",
    };

    std::fs::copy(wasm_file, &stage1_file)
        .map_err(|e| format!("Failed to stage wasm for opt: {}", e))?;

    // Aggressive optimization with --converge (run passes to
    // fixed point) and metadata strip. -Oz already drops the name
    // section; --strip-producers and --strip-target-features remove
    // sections that survive otherwise and bloat merged artifacts.
    let output = std::process::Command::new("wasm-opt")
        .arg(opt_flag)
        .arg("--converge")
        .arg("--strip-producers")
        .arg("--strip-target-features")
        .arg("--enable-bulk-memory")
        .arg("--enable-multivalue")
        .arg("--enable-tail-call")
        .arg("--enable-gc")
        .arg("--enable-reference-types")
        .arg(&stage1_file)
        .arg("-o")
        .arg(&optimized_file)
        .output()
        .map_err(|e| {
            let _ = std::fs::remove_file(&stage1_file);
            format!(
                "Failed to run wasm-opt {} for {}: {}. Install binaryen or compile without --optimize.",
                opt_flag,
                wasm_file.display(),
                e
            )
        })?;

    let _ = std::fs::remove_file(&stage1_file);

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let _ = std::fs::remove_file(&optimized_file);
        return Err(format!(
            "wasm-opt {} failed for {}: {}",
            opt_flag,
            wasm_file.display(),
            stderr.trim()
        ));
    }

    std::fs::rename(&optimized_file, wasm_file).map_err(|e| {
        format!(
            "Failed to replace {} with wasm-opt output: {}",
            wasm_file.display(),
            e
        )
    })?;

    let output_size = std::fs::metadata(wasm_file)
        .map(|meta| meta.len())
        .map_err(|e| format!("Failed to stat optimized {}: {}", wasm_file.display(), e))?;
    let size_delta = if input_size == output_size {
        "(no size change)".to_string()
    } else {
        format!("from {}", format_byte_size(input_size))
    };
    let opt_summary = format!("for {} {}", optimize_label(mode), size_delta);
    println!(
        "{} {} → {} ({})",
        "Optimized".green().bold(),
        wasm_file.display(),
        format_byte_size(output_size),
        opt_summary
    );

    Ok(output_size)
}

pub(super) struct CompileOptions<'a> {
    pub(super) file: &'a str,
    pub(super) output_dir: &'a str,
    pub(super) project_name: Option<&'a str>,
    pub(super) module_root_override: Option<&'a str>,
    pub(super) target: super::cli::CompileTarget,
    pub(super) with_replay: bool,
    pub(super) policy_mode: &'a super::cli::CompilePolicyMode,
    pub(super) guest_entry: Option<&'a str>,
    pub(super) with_self_host_support: bool,
    pub(super) pack: Option<super::cli::DeployPack>,
    pub(super) handler: Option<&'a str>,
    pub(super) world: super::cli::Wasip2World,
    pub(super) optimize: Option<super::cli::WasmOptMode>,
}

#[allow(clippy::too_many_arguments)]
pub(super) fn cmd_proof(
    file: &str,
    output_dir: &str,
    project_name: Option<&str>,
    module_root_override: Option<&str>,
    backend: &super::cli::ProofBackend,
    verify_mode: &super::cli::ProofVerifyMode,
    check: bool,
    error_budget: Option<usize>,
    sorry_budget: Option<usize>,
    check_json: bool,
) {
    let (mut ctx, _module_root) = build_codegen_context(
        file,
        project_name,
        module_root_override,
        false,
        &super::cli::CompilePolicyMode::Embed,
        None,
        false,
        false, // apply_traversal_lowering — proof export wants source-level IR
        true,  // run_refinement_lower — proof backends need ProofIR
        true,  // run_contract_lower — same
        true,  // run_law_lower — same
    );

    // Oracle v1: aver proof only models `?!` in complete mode. If the
    // project's aver.toml selects cancel or sequential, fail loudly —
    // proofs emitted under a different runtime mode wouldn't transfer.
    #[cfg(feature = "runtime")]
    if let Some(policy) = &ctx.policy {
        match policy.independence_mode {
            aver::config::IndependenceMode::Complete => {}
            aver::config::IndependenceMode::Cancel => {
                eprintln!(
                    "{}",
                    "error: aver.toml has [independence] mode = \"cancel\", but aver proof \
	                     only models `?!` in complete mode. Exported proofs would describe \
	                     complete-mode semantics that do not hold under cancel at runtime. \
	                     Set [independence] mode = \"complete\" in aver.toml for proof export."
                        .red()
                );
                std::process::exit(1);
            }
            aver::config::IndependenceMode::Sequential => {
                eprintln!(
                    "{}",
                    "error: aver.toml has [independence] mode = \"sequential\", but aver proof \
                     requires complete mode. Sequential execution is a legal schedule under \
                     complete-mode semantics, but generating proofs under `mode = sequential` \
                     would emit artifacts that do not describe the runtime policy consistently. \
                     Set [independence] mode = \"complete\" in aver.toml for proof export."
                        .red()
                );
                std::process::exit(1);
            }
        }
    }

    match backend {
        super::cli::ProofBackend::Lean => {
            cmd_proof_lean(file, output_dir, &mut ctx, verify_mode);
        }
        super::cli::ProofBackend::Dafny => {
            cmd_proof_dafny(file, output_dir, &ctx);
        }
    }

    if check {
        run_proof_check(output_dir, backend, error_budget, sorry_budget, check_json);
    }
}

/// `aver proof --check` harness: invoke the backend's verifier inside
/// `output_dir`, count errors (Dafny) or residual `sorry`s (Lean),
/// compare against the optional budget, and exit accordingly:
/// - exit 0: count ≤ budget (budget defaults to 0 when unset)
/// - exit 1: count > budget
/// - exit 2: harness failure (verifier not on PATH, missing .dfy entry,
///   verifier output didn't parse)
///
/// With `--check-json`, prints a structured summary to stdout
/// instead of streaming verifier output verbatim — same exit codes,
/// for CI consumption.
fn run_proof_check(
    output_dir: &str,
    backend: &super::cli::ProofBackend,
    error_budget: Option<usize>,
    sorry_budget: Option<usize>,
    check_json: bool,
) {
    use std::process::Command;

    let (cmd, args, label, backend_tag): (&str, Vec<String>, &str, &str) = match backend {
        super::cli::ProofBackend::Lean => {
            ("lake", vec!["build".to_string()], "Lean / lake", "lean")
        }
        super::cli::ProofBackend::Dafny => {
            let entry = match std::fs::read_dir(output_dir) {
                Ok(rd) => rd
                    .filter_map(|e| e.ok())
                    .map(|e| e.file_name().to_string_lossy().into_owned())
                    .find(|n| n.ends_with(".dfy") && n != "common.dfy"),
                Err(e) => {
                    eprintln!(
                        "{}",
                        format!("--check: read_dir({}) failed: {}", output_dir, e).red()
                    );
                    std::process::exit(2);
                }
            };
            let Some(entry) = entry else {
                eprintln!(
                    "{}",
                    format!(
                        "--check: no .dfy entry file found in {} (besides common.dfy)",
                        output_dir
                    )
                    .red()
                );
                std::process::exit(2);
            };
            (
                "dafny",
                vec!["verify".to_string(), entry],
                "Dafny / Z3",
                "dafny",
            )
        }
    };

    if !check_json {
        println!("{}", format!("--check: running {} verifier…", label).blue());
    }
    let output = match Command::new(cmd)
        .args(&args)
        .current_dir(output_dir)
        .output()
    {
        Ok(o) => o,
        Err(e) => {
            eprintln!(
                "{}",
                format!(
                    "--check: failed to spawn `{}`: {} — is the verifier installed and on PATH?",
                    cmd, e
                )
                .red()
            );
            std::process::exit(2);
        }
    };

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    let (errors, sorries, budget, count) = match backend {
        super::cli::ProofBackend::Dafny => {
            let errors = match parse_dafny_error_count(&stdout) {
                Some(n) => n,
                None => {
                    eprintln!(
                        "{}",
                        "--check: could not parse Dafny verifier output (missing \"finished with X verified, Y errors\" line)".red()
                    );
                    if !check_json {
                        eprint!("{}", stderr);
                        print!("{}", stdout);
                    }
                    std::process::exit(2);
                }
            };
            (Some(errors), None, error_budget.unwrap_or(0), errors)
        }
        super::cli::ProofBackend::Lean => {
            let sorries = count_lean_sorries(&stderr) + count_lean_sorries(&stdout);
            (None, Some(sorries), sorry_budget.unwrap_or(0), sorries)
        }
    };

    let passed = count <= budget;

    if check_json {
        let mut obj = serde_json::Map::new();
        obj.insert("backend".into(), backend_tag.into());
        if let Some(e) = errors {
            obj.insert("errors".into(), e.into());
        }
        if let Some(s) = sorries {
            obj.insert("sorries".into(), s.into());
        }
        obj.insert("budget".into(), budget.into());
        obj.insert("passed".into(), passed.into());
        println!(
            "{}",
            serde_json::to_string(&serde_json::Value::Object(obj))
                .unwrap_or_else(|_| "{}".to_string())
        );
    } else {
        // Stream the verifier's own output so the user sees the
        // diagnostics; we already parsed counts above.
        print!("{}", stdout);
        eprint!("{}", stderr);
        let metric = match backend {
            super::cli::ProofBackend::Dafny => format!("{count} errors"),
            super::cli::ProofBackend::Lean => format!("{count} sorries"),
        };
        if passed {
            let suffix = if budget > 0 {
                format!(" (within budget {budget})")
            } else {
                String::new()
            };
            println!("{}", format!("--check: {label} — {metric}{suffix}").green());
        } else {
            eprintln!(
                "{}",
                format!("--check: {label} — {metric} over budget {budget}").red()
            );
        }
    }

    if !passed {
        std::process::exit(1);
    }
}

/// Parse `Dafny program verifier finished with N verified, M errors`
/// out of the verifier's stdout. Returns `Some(M)` on a match,
/// `None` when the line isn't present.
fn parse_dafny_error_count(stdout: &str) -> Option<usize> {
    for line in stdout.lines() {
        let line = line.trim();
        if let Some(rest) = line.strip_prefix("Dafny program verifier finished with ") {
            // Shape: "<N> verified, <M> errors"
            if let Some((_, after_comma)) = rest.split_once(", ")
                && let Some(m) = after_comma.split_whitespace().next()
                && let Ok(n) = m.parse::<usize>()
            {
                return Some(n);
            }
        }
    }
    None
}

/// Count Lean's `declaration uses 'sorry'` warnings in build output.
/// Lake emits one such warning per `sorry` in the residual program;
/// counting them matches the budget the proof_spec gating tests use.
fn count_lean_sorries(s: &str) -> usize {
    s.lines()
        .filter(|l| l.contains("declaration uses 'sorry'"))
        .count()
}

fn cmd_proof_lean(
    file: &str,
    output_dir: &str,
    ctx: &mut codegen::CodegenContext,
    verify_mode: &super::cli::ProofVerifyMode,
) {
    let proof_issues = lean_codegen::proof_mode_findings(ctx);
    for issue in proof_issues {
        eprintln!(
            "{}",
            format!("warning[{}:1]: {}", issue.line, issue.message).yellow()
        );
    }
    let missing_helper_hints = collect_missing_helper_law_hints(&ctx.items, ctx);
    for hint in missing_helper_hints {
        eprintln!(
            "{}",
            format!(
                "warning[{}:1]: {}",
                hint.line,
                missing_helper_law_message(&hint)
            )
            .yellow()
        );
    }
    let contextual_helper_hints = collect_contextual_helper_law_hints(&ctx.items, ctx);
    for hint in contextual_helper_hints {
        eprintln!(
            "{}",
            format!(
                "warning[{}:1]: {}",
                hint.line,
                contextual_helper_law_message(&hint)
            )
            .yellow()
        );
    }

    let verify_mode = match verify_mode {
        super::cli::ProofVerifyMode::Auto => lean_codegen::VerifyEmitMode::NativeDecide,
        super::cli::ProofVerifyMode::Sorry => lean_codegen::VerifyEmitMode::Sorry,
        super::cli::ProofVerifyMode::TheoremSkeleton => {
            lean_codegen::VerifyEmitMode::TheoremSkeleton
        }
    };

    let output = lean_codegen::transpile_for_proof_mode(ctx, verify_mode);
    let build_hint = format!("cd {} && lake build", output_dir);
    write_codegen_output(file, output_dir, "Lean 4", &build_hint, &output);
}

fn cmd_proof_dafny(file: &str, output_dir: &str, ctx: &codegen::CodegenContext) {
    use aver::codegen::dafny as dafny_codegen;

    let output = dafny_codegen::transpile(ctx);
    let build_hint = format!(
        "cd {} && dafny verify {}.dfy",
        output_dir,
        aver::codegen::common::entry_basename(ctx)
    );
    write_codegen_output(file, output_dir, "Dafny", &build_hint, &output);
}

/// CLI shim around the library-level wasm-gc multi-module flattener.
#[cfg(feature = "wasm")]
pub(super) fn flatten_multimodule(items: &mut Vec<TopLevel>, dep_modules: &[ModuleInfo]) {
    aver::codegen::wasm_gc::flatten_multimodule(items, dep_modules);
}

/// Load dependent modules for codegen (recursive, with circular import detection).
///
/// `run_interp_lower` and `run_buffer_build` mirror the entry-module decision —
/// proof exporters (Lean/Dafny) pass `false` for both so dep modules also
/// stay source-level; runtime backends (VM/WASM/Rust) pass `true` for both
/// so the buffer-build pass fires on sinks living in dep modules too. Split
/// per-stage rather than a bundled flag so this matches the pipeline gates
/// 1-to-1 with no magic translation in between.
pub(super) fn load_compile_deps(
    items: &[TopLevel],
    module_root: &str,
    run_interp_lower: bool,
    run_buffer_build: bool,
    self_host_mode: bool,
) -> Vec<ModuleInfo> {
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
        load_module_recursive(
            dep_name,
            module_root,
            run_interp_lower,
            run_buffer_build,
            self_host_mode,
            &mut result,
            &mut loaded,
        );
    }

    result
}

fn load_module_recursive(
    name: &str,
    module_root: &str,
    run_interp_lower: bool,
    run_buffer_build: bool,
    self_host_mode: bool,
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

    // Dep modules go through the same pipeline shape as the entry, AND
    // they typecheck against the same on-disk module tree. Typecheck
    // here populates `Spanned::ty()` on this module's expressions so
    // type-driven codegen (legacy WASM Step 2, Rust Step 1) can read
    // them without per-backend ad-hoc inference. The entry-level
    // typecheck only validates cross-module references — it visits the
    // entry's items, not dep bodies, because the items it sees are a
    // separate parse from the ones flowing into ModuleInfo. Errors are
    // surfaced as fatal — a dep module that fails to typecheck means
    // the program is incoherent and codegen would emit broken output.
    let neutral_policy = aver::ir::NeutralAllocPolicy;
    let dep_typecheck_mode = if self_host_mode {
        aver::ir::TypecheckMode::FullSelfHost {
            base_dir: Some(module_root),
        }
    } else {
        aver::ir::TypecheckMode::Full {
            base_dir: Some(module_root),
        }
    };
    let pipeline_result = aver::ir::pipeline::run(
        &mut items,
        aver::ir::PipelineConfig {
            typecheck: Some(dep_typecheck_mode),
            run_interp_lower,
            run_buffer_build,
            alloc_policy: Some(&neutral_policy),
            ..Default::default()
        },
    );
    if let Some(tc) = pipeline_result.typecheck.as_ref()
        && !tc.errors.is_empty()
    {
        eprintln!(
            "{}",
            format!(
                "Type errors in dependency module '{}':\n{}",
                name,
                tc.errors
                    .iter()
                    .map(|e| format!("  {}:{}: {}", e.line, e.col, e.message))
                    .collect::<Vec<_>>()
                    .join("\n")
            )
            .red()
        );
        process::exit(1);
    }

    let depends = items
        .iter()
        .find_map(|i| {
            if let TopLevel::Module(m) = i {
                Some(m.depends.clone())
            } else {
                None
            }
        })
        .unwrap_or_default();

    // Recursively load transitive dependencies
    if let Some(mod_block) = items.iter().find_map(|i| {
        if let TopLevel::Module(m) = i {
            Some(m)
        } else {
            None
        }
    }) {
        for dep in &mod_block.depends {
            load_module_recursive(
                dep,
                module_root,
                run_interp_lower,
                run_buffer_build,
                self_host_mode,
                result,
                loaded,
            );
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
        depends,
        type_defs,
        fn_defs,
        analysis: pipeline_result.analysis,
    });
}

#[cfg(test)]
mod tests {
    use super::{
        codegen_uses_self_host_runtime, resolve_av_inputs, validate_self_host_guest_entry_contract,
    };
    use aver::ast::{Expr, FnBody, FnDef, Literal, Spanned, Stmt, TopLevel};
    use aver::codegen::CodegenContext;
    use std::collections::{HashMap, HashSet};
    use std::fs;
    use std::path::PathBuf;
    use std::sync::Arc as Rc;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn temp_case_dir(tag: &str) -> PathBuf {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0);
        std::env::temp_dir().join(format!("aver_commands_{tag}_{nanos}"))
    }

    fn empty_codegen_ctx() -> CodegenContext {
        CodegenContext {
            items: vec![],
            memo_fns: HashSet::new(),
            memo_safe_types: HashSet::new(),
            type_defs: vec![],
            fn_defs: vec![],
            project_name: "test".to_string(),
            modules: vec![],
            module_prefixes: HashSet::new(),
            policy: None,
            emit_replay_runtime: false,
            runtime_policy_from_env: false,
            guest_entry: None,
            emit_self_host_support: false,
            extra_fn_defs: Vec::new(),
            mutual_tco_members: HashSet::new(),
            recursive_fns: HashSet::new(),
            buffer_build_sinks: HashMap::new(),
            buffer_fusion_sites: Vec::new(),
            synthesized_buffered_fns: Vec::new(),
            proof_ir: aver::ir::ProofIR::default(),
            symbol_table: aver::ir::SymbolTable::default(),
            resolved_fn_defs: Vec::new(),
            resolved_module_fn_defs: Vec::new(),
            current_module_scope: std::cell::RefCell::new(None),
            resolved_program: aver::codegen::program_view::ResolvedProgramView::default(),
            program_shape: None,
        }
    }

    fn test_fn(name: &str, params: Vec<(String, String)>) -> FnDef {
        FnDef {
            name: name.to_string(),
            line: 1,
            params,
            return_type: "Unit".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::from_expr(Spanned::bare(Expr::Literal(
                Literal::Unit,
            )))),
            resolution: None,
        }
    }

    #[test]
    fn resolve_av_inputs_collects_and_sorts_directories() {
        let dir = temp_case_dir("collect");
        let nested = dir.join("nested");
        fs::create_dir_all(&nested).expect("create nested dir");
        fs::write(dir.join("b.av"), "module B\n").expect("write b.av");
        fs::write(dir.join("ignore.txt"), "nope").expect("write ignore.txt");
        fs::write(nested.join("a.av"), "module A\n").expect("write a.av");

        let inputs = resolve_av_inputs(dir.to_str().expect("utf8 path")).expect("collect inputs");
        assert_eq!(
            inputs,
            vec![
                dir.join("b.av").to_string_lossy().to_string(),
                nested.join("a.av").to_string_lossy().to_string(),
            ]
        );

        fs::remove_dir_all(&dir).expect("cleanup temp dir");
    }

    #[test]
    fn resolve_av_inputs_rejects_non_av_files() {
        let dir = temp_case_dir("reject");
        fs::create_dir_all(&dir).expect("create dir");
        let file = dir.join("note.txt");
        fs::write(&file, "nope").expect("write file");

        let err = resolve_av_inputs(file.to_str().expect("utf8 path")).expect_err("expected error");
        assert!(
            err.contains("is not an .av file"),
            "unexpected error: {err}"
        );

        fs::remove_dir_all(&dir).expect("cleanup temp dir");
    }

    #[test]
    fn detects_self_host_runtime_in_top_level_statement() {
        let mut ctx = empty_codegen_ctx();
        ctx.items = vec![TopLevel::Stmt(Stmt::Expr(Spanned::bare(Expr::FnCall(
            Box::new(Spanned::bare(Expr::Attr(
                Box::new(Spanned::bare(Expr::Ident("SelfHostRuntime".to_string()))),
                "httpServerListen".to_string(),
            ))),
            vec![
                Spanned::bare(Expr::Literal(Literal::Int(3000))),
                Spanned::bare(Expr::Ident("handler".to_string())),
            ],
        ))))];

        assert!(codegen_uses_self_host_runtime(&ctx));
    }

    #[test]
    fn self_host_support_requires_explicit_guest_entry_contract() {
        let mut ctx = empty_codegen_ctx();
        ctx.emit_self_host_support = true;
        ctx.guest_entry = Some("runGuestCliProgram".to_string());
        ctx.fn_defs = vec![test_fn(
            "runGuestCliProgram",
            vec![
                ("program".to_string(), "Program".to_string()),
                ("moduleFns".to_string(), "List<FnDef>".to_string()),
            ],
        )];

        let err =
            validate_self_host_guest_entry_contract(&ctx).expect_err("expected contract error");
        assert!(err.contains("prog: Program"), "unexpected error: {err}");
    }
}

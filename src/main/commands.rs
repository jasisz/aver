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
    apply_runtime_policy_to_vm, format_type_errors, load_runtime_policy, parse_file,
    print_type_errors, read_file, resolve_module_root,
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

        // Check errors = static-analysis errors (`Severity::Error`:
        // parse errors, type errors, intent-checker errors such as
        // `verify-rhs`). Verify EXECUTION failures carry
        // `Severity::Fail` and are counted through `verify_summary`
        // below, so excluding them here avoids double counting — but
        // exclusion must key on the severity, not the slug. The old
        // `!slug.starts_with("verify-")` filter also swallowed
        // `error[verify-rhs]` (a static check error that happens to
        // share the prefix), so a file whose only problems were
        // verify-rhs errors audited as "0 check errors" with exit 0.
        let file_check_errors = report
            .diagnostics
            .iter()
            .filter(|d| matches!(d.severity, aver::diagnostics::Severity::Error))
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
    let mut skipped_wasm_gc_backend: Vec<String> = Vec::new();
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
                // Bucket honestly: a wasm-gc backend error (compile /
                // codegen failure, preflight reject, wasmtime setup) is
                // NOT a source type error — `aver check` passes on such
                // files, so pointing the user there would be a dead end.
                // Backend errors carry a `wasm-gc` / `verify --wasm-gc`
                // prefix (see `diagnostics::wasm_gc_verify`); everything
                // else stays in the type-error bucket.
                if e.starts_with("wasm-gc") || e.starts_with("verify --wasm-gc") {
                    skipped_wasm_gc_backend.push(display_check_path(file, &module_root));
                } else {
                    skipped_typecheck.push(display_check_path(file, &module_root));
                }
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

    if !skipped_wasm_gc_backend.is_empty() && !json {
        println!();
        println!(
            "{}",
            format!(
                "{} file(s) skipped — wasm-gc backend error (the source type-checks; see the message above):",
                skipped_wasm_gc_backend.len()
            )
            .yellow()
        );
        for f in &skipped_wasm_gc_backend {
            println!("  {}", f.dimmed());
        }
        println!(
            "{}",
            "hint: `aver verify` (VM) runs these blocks without the wasm-gc backend".dimmed()
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

    // MIR isn't a pipeline stage — it's lowered from the resolved HIR
    // after the pipeline (the same `lower_program` + optimize the VM
    // backend runs). Recognise it here, run the pipeline to completion,
    // and dump the `MirProgram` below.
    let want_mir = stage_name == "mir";
    let target_stage = match stage_name {
        "parse" => None, // pre-pipeline snapshot
        // Run the full pipeline so the resolved HIR is available to lower.
        "mir" => Some(PipelineStage::NameResolve),
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
                     parse, tco, typecheck, interp_lower, buffer_build, resolve, last_use, analyze, escape, build_symbols, name_resolve, refinement_lower, contract_lower, law_lower, mir",
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

    // `--emit-ir-after=mir` — lower the resolved HIR to MIR and run the
    // optimize pipeline (the exact lowering the VM backend consumes),
    // then print the textual `MirProgram` dump.
    if want_mir {
        use aver::ir::mir;
        let program = mir::optimize(mir::lower_program(&pipeline_result.resolved_items));
        print!("{program}");
        return;
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
            Some(RecursionContract::WellFoundedToNat { param, floor_div }) => match floor_div {
                Some(shrink) => writeln!(
                    out,
                    "WellFoundedToNat {{ measure: toNat({}), floor_div: /{}{} }}",
                    param,
                    shrink.divisor,
                    shrink
                        .helper_fn
                        .as_ref()
                        .map(|h| format!(" via {}", h))
                        .unwrap_or_default(),
                )
                .unwrap(),
                None => writeln!(
                    out,
                    "WellFoundedToNat {{ measure: toNat({}), guarded countdown }}",
                    param,
                )
                .unwrap(),
            },
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
            run_interval_analyze: true,
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

/// `aver compile FILE --explain-mir-coverage` — lowers the resolved
/// program to MIR and reports how much of it the MIR pipeline accepts vs.
/// drops to the HIR fallback, broken down by the shape that blocked each
/// drop. MIR is the default VM path (`compile_program_with_modules`);
/// this is the lowering-level coverage — the upper bound on how many fns
/// the VM walker can take off the HIR path, and the roadmap for which
/// blocking shapes to lower next.
pub(super) fn cmd_explain_mir_coverage(
    file: &str,
    module_root_override: Option<&str>,
    json: bool,
    target: super::cli::CompileTarget,
) {
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

    let dep_modules = load_compile_deps(&items, &module_root, false, false, false);
    let result = aver::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full {
                base_dir: Some(&module_root),
            }),
            // Rust coverage needs the symbol table to resolve `FnId` /
            // `TypeId` / `CtorId` through `MirEmitCtx`. Cheap to build;
            // the VM/wasm-gc paths read it too once a ctx is assembled.
            run_build_symbols: true,
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

    let mir = aver::ir::mir::lower_program(&result.resolved_items);

    // `--target wasm-gc` reports the wasm-gc body emitter's reach over
    // the lowered MIR (Phase 5 #340 — how many fns ride the MIR body
    // walk vs. fall back to the `ResolvedExpr` emitter). `--target rust`
    // reports the Rust `from_mir` body emitter's reach (Wave 0 of the
    // rust-on-MIR port — how many fns the MIR-to-Rust walker emits
    // standalone vs fall back to the HIR walker), with a first-blocker
    // histogram so each wave's reach is measurable. Other targets report
    // the VM lowering-level coverage (how many fns lowered to MIR at
    // all).
    if matches!(target, super::cli::CompileTarget::WasmGc) {
        explain_wasm_gc_mir_coverage(&mir, json);
        return;
    }
    if matches!(target, super::cli::CompileTarget::Rust) {
        explain_rust_mir_coverage(&mir, &result.symbol_table, &dep_modules, json);
        return;
    }

    if json {
        print!("{}", render_mir_coverage_json(&mir.stats));
    } else {
        print!("{}", render_mir_coverage(&mir.stats));
    }
}

/// Rust backend coverage over a lowered MIR program — the body-emit
/// level reach of the `from_mir` walker, which is the SOLE Rust
/// runtime codegen path (the HIR walker was deleted in rust-on-MIR
/// W6/Stage-3). Reports total fns, MIR-emitted vs hard-error counts,
/// and a first-blocker histogram (which `MirExpr` variant / `MirCallee`
/// kind caused the first `None` in each fn that can't render) so the
/// residual (verify-only Oracle/trace shapes) is measurable. The walker
/// resolves `FnId`/`TypeId`/`CtorId` through the pipeline `SymbolTable`
/// and module-scoped names through the dep module prefixes — the same
/// inputs the production rust transpile path builds via `build_context`.
fn explain_rust_mir_coverage(
    mir: &aver::ir::mir::MirProgram,
    symbol_table: &aver::ir::SymbolTable,
    dep_modules: &[ModuleInfo],
    json: bool,
) {
    // Measure structural reach over the OPTIMIZED MIR — the same form
    // the production path emits (`build_context` stores
    // `optimize(lower_program(...))` on the ctx). The shared `mir` the
    // caller passed is the un-optimized lowering (the VM / wasm-gc
    // coverage paths want that), so optimize a copy here.
    let opt_mir = aver::ir::mir::optimize(mir.clone());
    let module_prefixes: HashSet<String> = dep_modules.iter().map(|m| m.prefix.clone()).collect();
    let emit_ctx = rust_codegen::MirEmitCtx::for_test(symbol_table, &module_prefixes);
    let (report, blockers) = rust_codegen::coverage_report_with_blockers(&opt_mir, &emit_ctx);

    // rust-on-MIR W6/Stage-3: the HIR walker is gone, so MIR is the
    // unconditional production path. The structural `coverage_report`
    // is the whole story now — every covered fn is emitted from MIR;
    // the residual `hir_fallback` count is the verify-only Oracle/trace
    // shapes that hard-error (they never built on the Rust backend).
    // There is no graduated/considered parity metric to report anymore.

    // Sort blockers by count descending (dominant first), then by label
    // for stable ties — so the report reads as a porting worklist.
    let mut sorted: Vec<(&&str, &usize)> = blockers.iter().collect();
    sorted.sort_by(|a, b| b.1.cmp(a.1).then(a.0.cmp(b.0)));

    if json {
        let blocker_json: Vec<String> = sorted
            .iter()
            .map(|(label, count)| {
                format!(
                    "{{\"shape\":\"{}\",\"count\":{count}}}",
                    label.replace('"', "\\\"")
                )
            })
            .collect();
        println!(
            "{{\"schema_version\":1,\"backend\":\"rust\",\"total\":{total},\"mir_lowered\":{covered},\"hir_fallback\":{fallback},\"coverage_ratio\":{ratio:.4},\"always_mir\":true,\"blocked_by_shape\":[{blockers}]}}",
            total = report.total,
            covered = report.mir_covered,
            fallback = report.hir_fallback,
            ratio = report.ratio(),
            blockers = blocker_json.join(","),
        );
    } else {
        let mut out = String::new();
        out.push_str("MIR coverage (rust backend) — body-emit level\n");
        out.push_str("=============================================\n\n");
        out.push_str(&format!("MIR fns:       {}\n", report.total));
        out.push_str(&format!(
            "MIR-emitted:   {}  ({:.1}%)\n",
            report.mir_covered,
            report.ratio() * 100.0
        ));
        out.push_str(&format!("HIR fallback:  {}\n", report.hir_fallback));
        // rust-on-MIR W6/Stage-3: MIR is the sole codegen path. Every
        // MIR-emitted fn is emitted from MIR unconditionally (no
        // byte-parity gate); the `HIR fallback` count is the verify-only
        // Oracle/trace residual that hard-errors at the call site.
        out.push_str("codegen path:  MIR (sole path; HIR walker deleted)\n");
        if !sorted.is_empty() {
            out.push_str("\nfirst blocker per fallback fn (dominant first):\n");
            for (label, count) in sorted {
                out.push_str(&format!("  {count:>4}  {label}\n"));
            }
        }
        print!("{out}");
    }
}

/// wasm-gc backend coverage over a lowered MIR program. Gated on
/// `wasm-compile` (the feature that exposes the wasm-gc emitter); a
/// default `aver` build without it prints a build-hint instead.
#[cfg(feature = "wasm-compile")]
fn explain_wasm_gc_mir_coverage(mir: &aver::ir::mir::MirProgram, json: bool) {
    let report = aver::codegen::wasm_gc::coverage_report(mir);
    if json {
        println!(
            "{{\"schema_version\":1,\"backend\":\"wasm-gc\",\"total\":{total},\"mir_covered\":{covered},\"trap_stub\":{fallback},\"coverage_ratio\":{ratio:.4}}}",
            total = report.total,
            covered = report.mir_covered,
            fallback = report.trap_stub,
            ratio = report.ratio(),
        );
    } else {
        let mut out = String::new();
        out.push_str("MIR coverage (wasm-gc backend) — body-emit level\n");
        out.push_str("================================================\n\n");
        out.push_str(&format!("MIR fns:       {}\n", report.total));
        out.push_str(&format!(
            "MIR-emitted:   {}  ({:.1}%)\n",
            report.mir_covered,
            report.ratio() * 100.0
        ));
        out.push_str(&format!("trap stub:     {}\n", report.trap_stub));
        print!("{out}");
    }
}

#[cfg(not(feature = "wasm-compile"))]
fn explain_wasm_gc_mir_coverage(_mir: &aver::ir::mir::MirProgram, _json: bool) {
    eprintln!(
        "{}",
        "--explain-mir-coverage --target wasm-gc requires a wasm-enabled build \
         (cargo build --features wasm)"
            .red()
    );
    process::exit(1);
}

/// Sort skip reasons by count descending (dominant blocker first), then
/// by stable variant order for ties — so the report reads as a roadmap.
fn mir_coverage_blockers(
    stats: &aver::ir::mir::LowerStats,
) -> Vec<(aver::ir::mir::SkipReason, u32)> {
    let mut blockers = stats.skipped_sorted();
    blockers.sort_by(|a, b| b.1.cmp(&a.1).then((a.0 as u8).cmp(&(b.0 as u8))));
    blockers
}

fn render_mir_coverage(stats: &aver::ir::mir::LowerStats) -> String {
    let total = stats.total();
    let lowered = stats.lowered;
    let fallback = total - lowered;
    let pct = stats.coverage_ratio() * 100.0;
    let mut out = String::new();
    out.push_str("MIR coverage (VM backend) — lowering level\n");
    out.push_str("==========================================\n\n");
    out.push_str(&format!("fns total:     {total}\n"));
    out.push_str(&format!("MIR-lowered:   {lowered}  ({pct:.1}%)\n"));
    out.push_str(&format!("HIR fallback:  {fallback}\n"));
    let blockers = mir_coverage_blockers(stats);
    if !blockers.is_empty() {
        out.push_str("\nblocked by shape (dominant first):\n");
        for (reason, count) in blockers {
            out.push_str(&format!("  {count:>4}  {}\n", reason.label()));
        }
    }
    out
}

fn render_mir_coverage_json(stats: &aver::ir::mir::LowerStats) -> String {
    let total = stats.total();
    let blockers: Vec<String> = mir_coverage_blockers(stats)
        .into_iter()
        .map(|(reason, count)| {
            format!(
                "{{\"shape\":\"{}\",\"count\":{count}}}",
                reason.label().replace('"', "\\\"")
            )
        })
        .collect();
    format!(
        "{{\"schema_version\":1,\"total\":{total},\"mir_lowered\":{lowered},\"hir_fallback\":{fallback},\"coverage_ratio\":{ratio:.4},\"blocked_by_shape\":[{blockers}]}}\n",
        lowered = stats.lowered,
        fallback = total - stats.lowered,
        ratio = stats.coverage_ratio(),
        blockers = blockers.join(","),
    )
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
            PassReport::IntervalAnalyze {
                types_analyzed,
                two_sided_bounded,
                ops_overflow_free,
                ops_needs_wider,
                ops_unbounded,
                raw_i64_eligible,
            } => {
                out.push_str(&format!(
                    "{label} {types_analyzed} refined type(s) analyzed: \
                     {two_sided_bounded} two-sided bounded; ops {ops_overflow_free} overflow-free, \
                     {ops_needs_wider} needs-wider-scratch, {ops_unbounded} unbounded; \
                     raw_i64_eligible: {raw_i64_eligible}\n"
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
            PassReport::IntervalAnalyze {
                types_analyzed,
                two_sided_bounded,
                ops_overflow_free,
                ops_needs_wider,
                ops_unbounded,
                raw_i64_eligible,
            } => {
                out.push_str(&format!(
                    "{{\"types_analyzed\":{},\"two_sided_bounded\":{},\
                     \"ops_overflow_free\":{},\"ops_needs_wider\":{},\
                     \"ops_unbounded\":{},\"raw_i64_eligible\":{}}}",
                    types_analyzed,
                    two_sided_bounded,
                    ops_overflow_free,
                    ops_needs_wider,
                    ops_unbounded,
                    raw_i64_eligible
                ));
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
    gate: Option<&str>,
    write_baseline: Option<&str>,
    discover: bool,
    emit_laws: bool,
    emit_laws_to: Option<&str>,
) {
    let (mut ctx, module_root) = build_codegen_context(
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

    // `--discover`: the explicit, expensive, cached lemma-discovery step
    // (charter: `prompts/lemma-discovery.md`); normal `aver proof` never enters
    // here. Discover-once / replay-after: the proved lemmas are committed as a
    // reviewable `<output>/DiscoveredLemmas.lean`, keyed by a discovery-surface
    // hash. On re-run with an unchanged surface we REPLAY — re-verify the
    // committed lemmas (the soundness guard) WITHOUT re-enumerating.
    if discover {
        // `--emit-laws`: born-as-Aver discovery. Render each VM-survivor
        // `Conjecture` as a legal `verify <fn> law`, keep the ones that pass the
        // STRICT forward check, and write them to a sidecar `<file>.discovered.av`
        // — so a discovered law flows through the same pipeline as a user law.
        // Additive: the Lean `DiscoveredLemmas.lean` path is left untouched; this
        // is a distinct output and returns early.
        if emit_laws {
            cmd_proof_emit_laws(&ctx, file, &module_root, emit_laws_to);
            return;
        }

        let lemmas_path = std::path::Path::new(output_dir).join("DiscoveredLemmas.lean");
        let surface_hash = {
            let inputs = aver::codegen::proof_lower::ProofLowerInputs::from_ctx(&ctx);
            aver::codegen::lemma_discovery::discovery_surface_hash(&inputs)
        };
        let hash_tag = format!("-- cone-hash: {surface_hash}");

        // REPLAY: committed lemmas present + surface unchanged → re-verify only.
        if let Ok(existing) = std::fs::read_to_string(&lemmas_path)
            && existing.contains(&hash_tag)
        {
            let count = existing
                .lines()
                .filter(|l| l.trim_start().starts_with("theorem "))
                .count();
            if lake_reverify_appended(&mut ctx, verify_mode, &existing) {
                println!(
                    "lemma discovery: replayed {count} committed lemma(s), re-verified (no rediscovery)\n  {}",
                    lemmas_path.display()
                );
                return;
            }
            eprintln!(
                "{}",
                "warning: committed lemmas failed re-verification (surface stale?) — rediscovering"
                    .yellow()
            );
        }

        // DISCOVER: enumerate + VM-filter + collect structure-directed guarded
        // lemma groups (immutable borrow of ctx, scoped so it ends before the
        // prove step needs `&mut ctx`).
        let (mut reports, structural_groups) = {
            let inputs = aver::codegen::proof_lower::ProofLowerInputs::from_ctx(&ctx);
            let mut reports = aver::codegen::lemma_discovery::run_discovery(&inputs);
            aver::codegen::lemma_discovery::vm_filter(&mut reports, &inputs);
            let groups = aver::codegen::lemma_discovery::structural_lemma_groups(&inputs);
            (reports, groups)
        };
        let proved_lean =
            prove_discovered_lemmas_lean(&mut reports, &structural_groups, &mut ctx, verify_mode);
        print!(
            "{}",
            aver::codegen::lemma_discovery::render_report(&reports)
        );

        // Persist proved lemmas as a reviewable Lean file (the committed
        // artifact), tagged with the surface hash for replay.
        if !proved_lean.is_empty() {
            let _ = std::fs::create_dir_all(output_dir);
            let mut content = format!(
                "-- Discovered lemmas for {file} — `aver proof --discover`\n\
                 {hash_tag}\n\
                 -- Each theorem below was discovered (by enumeration or structure)\n\
                 -- and kernel-proved. Re-verified (not rediscovered) on replay while\n\
                 -- the cone-hash holds.\n\n"
            );
            for thm in &proved_lean {
                content.push_str(thm);
                content.push('\n');
            }
            if std::fs::write(&lemmas_path, &content).is_ok() {
                println!(
                    "\ncommitted {} proved lemma(s) → {}",
                    proved_lean.len(),
                    lemmas_path.display()
                );
            }
        }
        return;
    }

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

    // Discovery feedback loop (the `ProofStrategy::SimpOverLemmas` hook):
    // when a prior `--discover` run committed kernel-proved lemmas for THIS
    // surface into `<output>/DiscoveredLemmas.lean`, a normal Lean proof run
    // re-pins each in-scope `Induction` law so the backend embeds the lemmas
    // (re-proving them in the same `lake build` — the soundness guard) and
    // `simp`s over them. The cone-hash gate is staleness-only: a stale file
    // is IGNORED (behaves exactly like no discovery ran), never trusted.
    // Lean-only — discovery commits no Dafny artifact today.
    if matches!(backend, super::cli::ProofBackend::Lean) {
        let lemmas_path = std::path::Path::new(output_dir).join("DiscoveredLemmas.lean");
        // The cone-hash HEADER (any hash) is what identifies the file as the
        // discovery artifact at all — a previously-emitted entry root from a
        // user module that happens to be named `DiscoveredLemmas` has no such
        // header and is silently ignored, not warned about as "stale".
        if let Ok(content) = std::fs::read_to_string(&lemmas_path)
            && content.contains("-- cone-hash:")
        {
            let lemmas = {
                let inputs = aver::codegen::proof_lower::ProofLowerInputs::from_ctx(&ctx);
                let hash_tag = format!(
                    "-- cone-hash: {}",
                    aver::codegen::lemma_discovery::discovery_surface_hash(&inputs)
                );
                if content.contains(&hash_tag) {
                    let parsed = aver::codegen::lemma_discovery::parse_committed_lemmas(&content);
                    // Soundness gate: the texts are embedded VERBATIM into the
                    // generated Lean, so a block carrying any top-level
                    // declaration keyword (`axiom`, `set_option`, …) beyond
                    // its own theorem would join the kernel environment.
                    // Discovery never emits those — reject the whole artifact
                    // loudly instead of trusting hand-edited content. (The
                    // axiom whitelist in `--check`'s universal metric is the
                    // backstop if anything ever slips through.)
                    match parsed.iter().find_map(|l| {
                        aver::codegen::lemma_discovery::forbidden_token_in_lemma(&l.text)
                            .map(|tok| (l.name.clone(), tok))
                    }) {
                        Some((name, tok)) => {
                            eprintln!(
                                "{}",
                                format!(
                                    "warning: committed discovered lemma `{name}` contains a \
                                     forbidden declaration token `{tok}` — the artifact was NOT \
                                     emitted by `aver proof --discover`; ignoring it entirely. \
                                     Delete {} and re-run `aver proof --discover`.",
                                    lemmas_path.display()
                                )
                                .yellow()
                            );
                            Vec::new()
                        }
                        None => parsed,
                    }
                } else {
                    eprintln!(
                        "{}",
                        "warning: committed discovered lemmas are stale (surface changed) — \
                         ignored; re-run `aver proof --discover` to refresh"
                            .yellow()
                    );
                    Vec::new()
                }
            };
            if !lemmas.is_empty() {
                let plan = {
                    let inputs = aver::codegen::proof_lower::ProofLowerInputs::from_ctx(&ctx);
                    aver::codegen::lemma_discovery::plan_simp_over_lemma_pins(
                        &inputs,
                        &ctx.proof_ir,
                        &lemmas,
                    )
                };
                if !plan.is_empty() {
                    aver::codegen::lemma_discovery::apply_simp_over_lemma_pins(
                        &mut ctx.proof_ir,
                        &plan,
                    );
                    println!(
                        "lemma feedback: {} committed lemma(s) joined {} law(s) (simp-over-lemmas)",
                        lemmas.len(),
                        plan.len()
                    );
                }
                ctx.discovered_lemmas = lemmas;
            }
        }
    }

    match backend {
        super::cli::ProofBackend::Lean => {
            // Ground-truth literalization for bounded checks: run the same
            // Declared-mode VM verify pass `aver verify` runs (the proof flow
            // does NOT otherwise run it) and collect each passing case's
            // expected-side value. The Lean emitter pins the expected side of
            // `verify` examples and law samples to these literals — model vs
            // program result instead of model vs model — so a model that
            // diverges from the program (fuel exhaustion included) fails the
            // build instead of kernel-certifying a vacuous equation. A failed
            // or impossible verify run yields an empty table → unchanged
            // emission. Lean-only: Dafny doesn't evaluate concrete examples
            // (it proves laws symbolically — Z3 either discharges a sample
            // lemma or reports an error; there is no panic-returns-default
            // evaluation path to go vacuous through).
            ctx.sample_expected = collect_verify_ground_truth(file, &module_root);
            cmd_proof_lean(file, output_dir, &mut ctx, verify_mode);
        }
        super::cli::ProofBackend::Dafny => {
            cmd_proof_dafny(file, output_dir, &ctx);
        }
    }

    // `--gate` / `--write-baseline` imply a verifier run (they recompute the
    // current manifest), so run the check harness when EITHER is set even
    // without an explicit `--check`.
    if check || gate.is_some() || write_baseline.is_some() {
        // For Dafny, hand the harness the real entry module filename so it
        // verifies the file that carries the verify-law lemmas (not an
        // arbitrary dependency module).
        let dafny_entry = match backend {
            super::cli::ProofBackend::Dafny => Some(format!(
                "{}.dfy",
                aver::codegen::common::entry_basename(&ctx)
            )),
            super::cli::ProofBackend::Lean => None,
        };
        // Source-level duplicate `fn.law` identities. Detected here (where the
        // parsed items are in hand) and handed to the ratchet so it can fail
        // CLOSED rather than collapse two distinct law blocks into one manifest
        // entry — see `duplicate_law_identities`.
        let duplicate_laws = duplicate_law_identities(&ctx.items);
        run_proof_check(
            output_dir,
            backend,
            error_budget,
            sorry_budget,
            check_json,
            dafny_entry,
            gate,
            write_baseline,
            &duplicate_laws,
        );
    }
}

/// `--discover --emit-laws`: born-as-Aver discovery. Enumerate + VM-filter the
/// `Conjecture` family, render each survivor as a legal `verify <fn> law`,
/// keep only the laws that pass the STRICT forward check (`aver verify`
/// semantics — stricter than the 6-round sampler), and write them to a sidecar
/// `<file>.discovered.av`. The user's source file is NEVER mutated.
///
/// The sidecar is the source verbatim + a provenance header + the rendered
/// laws, so it is self-contained (cone fns / ADTs / module all in scope) and
/// runs straight through `aver check` / `aver verify` / `aver proof … --gate`.
fn cmd_proof_emit_laws(
    ctx: &codegen::CodegenContext,
    file: &str,
    module_root: &str,
    emit_laws_to: Option<&str>,
) {
    use std::collections::BTreeSet;

    // Re-parse the source fresh: the strict-check runner wants un-resolved
    // items (it runs its own tco/typecheck/resolve), and the sidecar wants the
    // verbatim source text. `ctx.items` are post-pipeline (lowered), so we don't
    // reuse them for either.
    let source_text = match read_file(file) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("{}", e.red());
            process::exit(1);
        }
    };
    let source_items = match parse_source_items(&source_text) {
        Ok(items) => items,
        Err(e) => {
            eprintln!("{}", format!("error: {e}").red());
            process::exit(1);
        }
    };

    // The user's own law names — dedup target so a discovered law never
    // collides with (or cross-wires the SimpOverLemmas pool against) one.
    let existing_law_names: BTreeSet<String> = source_items
        .iter()
        .filter_map(|item| match item {
            TopLevel::Verify(vb) => match &vb.kind {
                aver::ast::VerifyKind::Law(law) => Some(law.name.clone()),
                aver::ast::VerifyKind::Cases => None,
            },
            _ => None,
        })
        .collect();

    // Enumerate + VM-filter the Conjecture family (the 100%-expressible slice).
    let inputs = aver::codegen::proof_lower::ProofLowerInputs::from_ctx(ctx);
    let mut reports = aver::codegen::lemma_discovery::run_discovery(&inputs);
    aver::codegen::lemma_discovery::vm_filter(&mut reports, &inputs);

    let result = aver::codegen::lemma_discovery::emit_laws_for_reports(
        &reports,
        &inputs,
        &source_items,
        &existing_law_names,
        Some(module_root),
        file,
    );

    let sidecar_path = emit_laws_to
        .map(std::path::PathBuf::from)
        .unwrap_or_else(|| std::path::PathBuf::from(format!("{file}.discovered.av")));

    // SOURCE-PROTECTION (the "never mutates source" contract MUST hold for the
    // `--emit-laws-to` override path too): refuse to write if the target IS — or
    // aliases (symlink / hardlink / `.`/`..` / case-variant, via `canonicalize`)
    // — the entry source OR any input `.av` (its loaded dep modules). Fail
    // CLOSED with a clear error; never clobber a hand-written program.
    if let Some(conflict) = sidecar_alias_conflict(&sidecar_path, file, module_root, &source_items)
    {
        eprintln!(
            "{}",
            format!(
                "error: --emit-laws-to {} would overwrite the input source `{}` \
                 (the sidecar must be a NEW file; the source is never mutated)",
                sidecar_path.display(),
                conflict.display()
            )
            .red()
        );
        process::exit(1);
    }

    let content = aver::codegen::lemma_discovery::render_sidecar(
        &source_text,
        &result.emitted,
        file,
        &sidecar_path.display().to_string(),
        env!("CARGO_PKG_VERSION"),
    );

    println!(
        "born-as-Aver discovery: {} law(s) cleared the hostile-widened forward check, {} dropped",
        result.emitted.len(),
        result.dropped.len()
    );
    for law in &result.emitted {
        println!("  ✓ verify {} law {}", law.subject_fn, law.name);
    }
    // Honesty: name every dropped candidate + reason (e.g. the spurious
    // `flushAcc` commutativity the hostile check refutes, or an inconclusive
    // Named-guard shape hostile can't breach), never silently.
    for (cand, reason) in &result.dropped {
        println!("  ✗ {cand}  [{reason}]");
    }

    // Atomic-ish write: write to a temp sibling, then rename over the target, so
    // a concurrent run can't observe (or clobber) a half-written reviewed
    // sidecar. Rename is atomic on the same filesystem.
    if let Err(e) = write_sidecar_atomic(&sidecar_path, &content) {
        eprintln!(
            "{}",
            format!(
                "error: could not write sidecar {}: {e}",
                sidecar_path.display()
            )
            .red()
        );
        process::exit(1);
    }
    println!(
        "\nwrote sidecar → {0}\n  next: aver proof {0} --check   (add --gate <baseline> to ratchet)",
        sidecar_path.display()
    );
}

/// Return `Some(input_path)` iff `target` is, or aliases, the entry source
/// `file` or any of its loaded dependency `.av` files — so the caller can refuse
/// to overwrite a human-written program. Uses `canonicalize` (resolves symlinks,
/// `.`/`..`, case-folding) for existing files and falls back to a lexical
/// normalization for a target that does not exist yet (the common case: the
/// sidecar is brand new).
fn sidecar_alias_conflict(
    target: &std::path::Path,
    file: &str,
    module_root: &str,
    source_items: &[TopLevel],
) -> Option<std::path::PathBuf> {
    // The set of input files we must never overwrite: the entry source + every
    // dep module `.av` resolved (transitively) from the same root.
    let mut inputs: Vec<std::path::PathBuf> = vec![std::path::PathBuf::from(file)];
    let mut seen = std::collections::HashSet::new();
    let mut stack: Vec<String> = source_items
        .iter()
        .find_map(|i| match i {
            TopLevel::Module(m) => Some(m.depends.clone()),
            _ => None,
        })
        .unwrap_or_default();
    while let Some(dep) = stack.pop() {
        if !seen.insert(dep.clone()) {
            continue;
        }
        if let Some(path) = aver::source::find_module_file(&dep, module_root) {
            // Pull this dep's own `depends` so the protection is transitive.
            if let Ok(text) = std::fs::read_to_string(&path)
                && let Ok(items) = parse_source_items(&text)
            {
                for i in &items {
                    if let TopLevel::Module(m) = i {
                        stack.extend(m.depends.iter().cloned());
                    }
                }
            }
            inputs.push(path);
        }
    }
    let target_key = canonical_key(target);
    inputs
        .into_iter()
        .find(|input| canonical_key(input) == target_key)
}

/// A comparison key for path aliasing: the canonical path when the file exists
/// (resolves symlinks / hardlink targets share an inode but distinct paths, so
/// we ALSO compare canonical parent + file name), else a lexical normalization.
/// Two paths that name the same on-disk file produce the same key.
fn canonical_key(p: &std::path::Path) -> std::path::PathBuf {
    if let Ok(c) = p.canonicalize() {
        return c;
    }
    // File doesn't exist yet: canonicalize the PARENT (which usually does) and
    // re-attach the file name, so `dir/../dir/x.av` and `dir/x.av` collapse.
    match (p.parent(), p.file_name()) {
        (Some(parent), Some(name)) => {
            let base = parent
                .canonicalize()
                .unwrap_or_else(|_| parent.to_path_buf());
            base.join(name)
        }
        _ => p.to_path_buf(),
    }
}

/// Write `content` to `path` atomically: write a temp sibling first, then
/// `rename` it over the target (atomic on the same filesystem). Falls back to a
/// direct write only if a temp sibling can't be created.
fn write_sidecar_atomic(path: &std::path::Path, content: &str) -> std::io::Result<()> {
    let tmp = match path.file_name().and_then(|n| n.to_str()) {
        Some(name) => path.with_file_name(format!(".{name}.tmp-{}", std::process::id())),
        None => return std::fs::write(path, content),
    };
    std::fs::write(&tmp, content)?;
    match std::fs::rename(&tmp, path) {
        Ok(()) => Ok(()),
        Err(e) => {
            let _ = std::fs::remove_file(&tmp);
            Err(e)
        }
    }
}

/// Lex + parse a source string into its `TopLevel` items (no pipeline lowering).
fn parse_source_items(source: &str) -> Result<Vec<TopLevel>, String> {
    let mut lexer = aver::lexer::Lexer::new(source);
    let tokens = lexer.tokenize().map_err(|e| e.to_string())?;
    aver::parser::Parser::new(tokens)
        .parse()
        .map_err(|e| e.to_string())
}

/// Phase 2d of lemma discovery: attempt to kernel-prove the top-ranked
/// surviving candidates by appending each as a standalone Lean theorem to the
/// program's generated Lean project and running `lake build`.
///
/// A candidate proves iff the build still succeeds: the program's own law
/// theorems carry `sorry` fallbacks (warnings → lake exit 0), while a
/// discovered theorem has NO `sorry`, so it can only flip the exit code by
/// failing (unsolved goals → exit 1). Records proved equations into
/// `report.proved`. Skips silently if `lake` is unavailable (discovery
/// degrades to survivors-only). Bounded by a small build budget — discovery is
/// the expensive cached step, but `--discover` should still return promptly.
fn prove_discovered_lemmas_lean(
    reports: &mut [aver::codegen::lemma_discovery::LawDiscovery],
    structural_groups: &[Vec<(String, String)>],
    ctx: &mut codegen::CodegenContext,
    verify_mode: &super::cli::ProofVerifyMode,
) -> Vec<String> {
    use std::process::Command;

    let mut proved_lean: Vec<String> = Vec::new();

    // `lake` must be on PATH; otherwise leave proving to a later run.
    let lake_ok = Command::new("lake")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false);
    if !lake_ok {
        return proved_lean;
    }

    let emit_mode = match verify_mode {
        super::cli::ProofVerifyMode::Auto => aver::codegen::lean::VerifyEmitMode::NativeDecide,
        super::cli::ProofVerifyMode::Sorry => aver::codegen::lean::VerifyEmitMode::Sorry,
        super::cli::ProofVerifyMode::TheoremSkeleton => {
            aver::codegen::lean::VerifyEmitMode::TheoremSkeleton
        }
    };
    let project = aver::codegen::lean::transpile_for_proof_mode(ctx, emit_mode);
    let entry = format!("{}.lean", aver::codegen::common::entry_basename(ctx));

    let Ok(dir) = tempfile::tempdir() else {
        return proved_lean;
    };
    let mut entry_orig: Option<String> = None;
    for (rel, content) in &project.files {
        let path = dir.path().join(rel);
        if let Some(parent) = path.parent() {
            let _ = std::fs::create_dir_all(parent);
        }
        if *rel == entry {
            entry_orig = Some(content.clone());
        }
        if std::fs::write(&path, content).is_err() {
            return proved_lean;
        }
    }
    let Some(entry_orig) = entry_orig else {
        return proved_lean;
    };
    let entry_path = dir.path().join(&entry);

    // Helper: append `addition` to the entry file and `lake build`; true on
    // success (the appended theorems kernel-check, given the baseline builds).
    let build_with = |addition: &str| -> bool {
        let body = format!("{entry_orig}\n\n{addition}\n");
        if std::fs::write(&entry_path, body).is_err() {
            return false;
        }
        Command::new("lake")
            .arg("build")
            .current_dir(dir.path())
            .output()
            .map(|o| o.status.success())
            .unwrap_or(false)
    };

    // Layer-3 structure-directed groups first (targeted guarded lemmas, e.g.
    // the counted-repeat advance). Each group's co-dependent theorems are
    // appended and built together, in dependency order.
    for group in structural_groups {
        let addition = group
            .iter()
            .map(|(_, text)| text.as_str())
            .collect::<Vec<_>>()
            .join("\n");
        if build_with(&addition) {
            for (_, text) in group {
                proved_lean.push(text.clone());
            }
        }
    }

    // Total `lake build` attempts for enumerated candidates across all laws.
    let mut budget = 8usize;
    let mut counter = 0usize;
    for report in reports.iter_mut() {
        for idx in aver::codegen::lemma_discovery::rank_candidate_indices(report) {
            if budget == 0 {
                break;
            }
            let candidate = &report.conjectures[idx];
            let name = format!("aver_discovered_lemma_{counter}");
            counter += 1;
            let Some(theorem) = aver::codegen::lemma_discovery::lean_lemma_theorem(
                candidate,
                &report.binders,
                &name,
            ) else {
                // No list-typed free variable → the list-induction template
                // doesn't apply; don't spend build budget on it.
                continue;
            };
            budget -= 1;
            if build_with(&theorem) {
                report.proved.push(candidate.render(&report.binders));
                proved_lean.push(theorem);
            }
        }
    }
    proved_lean
}

/// Re-verify already-discovered lemmas (replay path): regenerate the program's
/// Lean project, append the committed lemma source verbatim, and `lake build`.
/// Returns whether the build still succeeds (the committed proofs still
/// kernel-check). This — not the cone hash — is the soundness guard: a code
/// change that staled a lemma fails loudly here instead of being trusted.
fn lake_reverify_appended(
    ctx: &mut codegen::CodegenContext,
    verify_mode: &super::cli::ProofVerifyMode,
    appended: &str,
) -> bool {
    use std::process::Command;

    if Command::new("lake")
        .arg("--version")
        .output()
        .map(|o| !o.status.success())
        .unwrap_or(true)
    {
        return false;
    }
    let emit_mode = match verify_mode {
        super::cli::ProofVerifyMode::Auto => aver::codegen::lean::VerifyEmitMode::NativeDecide,
        super::cli::ProofVerifyMode::Sorry => aver::codegen::lean::VerifyEmitMode::Sorry,
        super::cli::ProofVerifyMode::TheoremSkeleton => {
            aver::codegen::lean::VerifyEmitMode::TheoremSkeleton
        }
    };
    let project = aver::codegen::lean::transpile_for_proof_mode(ctx, emit_mode);
    let entry = format!("{}.lean", aver::codegen::common::entry_basename(ctx));
    let Ok(dir) = tempfile::tempdir() else {
        return false;
    };
    for (rel, content) in &project.files {
        let path = dir.path().join(rel);
        if let Some(parent) = path.parent() {
            let _ = std::fs::create_dir_all(parent);
        }
        let body = if *rel == entry {
            format!("{content}\n\n{appended}\n")
        } else {
            content.clone()
        };
        if std::fs::write(&path, body).is_err() {
            return false;
        }
    }
    Command::new("lake")
        .arg("build")
        .current_dir(dir.path())
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Run the Declared-mode VM verify pass over `file`'s entry items and build
/// the ground-truth table for `CodegenContext::sample_expected`: for every
/// case that PASSES, the VM-computed expected (right-side) value, rendered
/// with `aver_repr_literal`, keyed by
/// `(verify_block_counter_key, global_case_index)`.
///
/// The index space mirrors the Lean emitter exactly: per-key running
/// counters over the merged blocks (plain `verify <fn>` blocks coalesce per
/// fn in source order — `merge_verify_blocks` — matching the emitter's
/// per-key counter continuation over unmerged items; law blocks each start
/// at their own running offset, which also keeps duplicate same-named law
/// blocks from cross-associating values).
///
/// Skips, by design:
/// - trace blocks (runtime-only projections; the emitter doesn't literalize
///   them either);
/// - Float-carrying values — decimal `aver_repr` round-trip is not bit-exact,
///   so a literalized Float could fail a CORRECT model; those cases keep the
///   source RHS and rely on the `--check` fuel-panic gate;
/// - values whose strings contain characters the lexer would misread when
///   parsed back (`"`, `\`, interpolation braces, control chars).
///
/// Any failure (unreadable file, parse/typecheck error, VM error) returns an
/// empty table — emission then behaves exactly as before this feature.
fn collect_verify_ground_truth(file: &str, module_root: &str) -> HashMap<(String, usize), String> {
    use aver::checker::{VerifyCaseOutcome, merge_verify_blocks};

    let mut out = HashMap::new();
    let Ok(source) = read_file(file) else {
        return out;
    };
    let Ok(items) = parse_file(&source) else {
        return out;
    };
    let merged = merge_verify_blocks(&items);
    if merged.is_empty() {
        return out;
    }
    let config = match load_runtime_policy(module_root) {
        Ok(c) => c,
        Err(_) => return out,
    };
    let results = match aver::diagnostics::vm_verify::run_verify_for_items_vm(
        items,
        config,
        Some(module_root),
        file,
    ) {
        Ok(r) => r,
        Err(_) => return out,
    };
    // One result per merged block, in order — the runner builds its plans
    // from the same `merge_verify_blocks` output. Anything else means the
    // pairing below would be guesswork; return empty (fall back to source).
    if results.len() != merged.len() {
        return out;
    }

    let mut counters: HashMap<String, usize> = HashMap::new();
    for (block, result) in merged.iter().zip(&results) {
        let key = aver::codegen::common::verify_block_counter_key(block);
        let base = *counters.get(&key).unwrap_or(&0);
        counters.insert(key.clone(), base + block.cases.len());
        if block.trace {
            continue;
        }
        for cr in &result.case_results {
            if !matches!(cr.outcome, VerifyCaseOutcome::Pass) {
                continue;
            }
            let Some(value) = &cr.expected_value else {
                continue;
            };
            if value_contains_float(value)
                || value_contains_map(value)
                || !value_strings_are_literal_safe(value)
            {
                continue;
            }
            out.insert(
                (key.clone(), base + cr.case_index),
                aver::value::aver_repr_literal(value),
            );
        }
    }
    out
}

/// Structural Float scan for ground-truth literalization: any embedded
/// `Value::Float` disqualifies the value (its decimal repr does not
/// round-trip bit-exactly through parse + Lean emission, so the literalized
/// equation could fail on a CORRECT model).
fn value_contains_float(value: &aver::value::Value) -> bool {
    use aver::value::Value;
    match value {
        Value::Float(_) => true,
        Value::Ok(v) | Value::Err(v) | Value::Some(v) => value_contains_float(v),
        Value::List(items) => items.iter().any(value_contains_float),
        Value::Tuple(items) => items.iter().any(value_contains_float),
        Value::Vector(items) => items.iter().any(value_contains_float),
        Value::Map(entries) => entries
            .iter()
            .any(|(k, v)| value_contains_float(k) || value_contains_float(v)),
        Value::Variant { fields, .. } => fields.iter().any(value_contains_float),
        Value::Record { fields, .. } => fields.iter().any(|(_, v)| value_contains_float(v)),
        _ => false,
    }
}

/// Structural Map scan for ground-truth literalization: any embedded
/// `Value::Map` disqualifies the value. The repr renders maps sorted, but
/// the emitted Lean `AverMap` carries entries in APPEND order — a
/// literalized map equation would compare structurally and could fail on a
/// CORRECT model whose insertion order differs from sort order. (Today the
/// repr's `{k: v}` spelling also fails the parser — which expects
/// `{k => v}` — so the fallback fires anyway; this gate makes the skip
/// deliberate instead of accidental.)
fn value_contains_map(value: &aver::value::Value) -> bool {
    use aver::value::Value;
    match value {
        Value::Map(_) => true,
        Value::Ok(v) | Value::Err(v) | Value::Some(v) => value_contains_map(v),
        Value::List(items) => items.iter().any(value_contains_map),
        Value::Tuple(items) => items.iter().any(value_contains_map),
        Value::Vector(items) => items.iter().any(value_contains_map),
        Value::Variant { fields, .. } => fields.iter().any(value_contains_map),
        Value::Record { fields, .. } => fields.iter().any(|(_, v)| value_contains_map(v)),
        _ => false,
    }
}

/// Every embedded string must survive the repr → parse round-trip verbatim:
/// `aver_repr_literal` does not escape-render, so quotes/backslashes would
/// break or alter the literal and `{`/`}` would be lexed as interpolation.
/// Non-ASCII is fine; control characters are not.
fn value_strings_are_literal_safe(value: &aver::value::Value) -> bool {
    use aver::value::Value;
    match value {
        Value::Str(s) => s
            .chars()
            .all(|c| !matches!(c, '"' | '\\' | '{' | '}') && !c.is_control()),
        Value::Ok(v) | Value::Err(v) | Value::Some(v) => value_strings_are_literal_safe(v),
        Value::List(items) => items.iter().all(value_strings_are_literal_safe),
        Value::Tuple(items) => items.iter().all(value_strings_are_literal_safe),
        Value::Vector(items) => items.iter().all(value_strings_are_literal_safe),
        Value::Map(entries) => entries
            .iter()
            .all(|(k, v)| value_strings_are_literal_safe(k) && value_strings_are_literal_safe(v)),
        Value::Variant { fields, .. } => fields.iter().all(value_strings_are_literal_safe),
        Value::Record { fields, .. } => fields
            .iter()
            .all(|(_, v)| value_strings_are_literal_safe(v)),
        _ => true,
    }
}

/// `aver proof --check` harness: invoke the backend's verifier inside
/// `output_dir`, require the verifier to exit cleanly, count errors +
/// `assume {:axiom}` trust-escapes (Dafny) or residual `sorry`s (Lean),
/// compare against the optional budget(s), and exit accordingly:
/// - exit 0: count ≤ budget (budget defaults to 0 when unset)
/// - exit 1: count > budget
/// - exit 2: harness failure (verifier not on PATH, missing .dfy entry,
///   verifier output didn't parse)
///
/// With `--check-json`, prints a structured summary to stdout
/// instead of streaming verifier output verbatim — same exit codes,
/// for CI consumption.
#[allow(clippy::too_many_arguments)]
fn run_proof_check(
    output_dir: &str,
    backend: &super::cli::ProofBackend,
    error_budget: Option<usize>,
    sorry_budget: Option<usize>,
    check_json: bool,
    dafny_entry: Option<String>,
    // The ratchet. `gate`: compare the freshly recomputed manifest against
    // this committed baseline and FAIL on any regression (a baseline law that
    // is MISSING, DEMOTED in tier, whose recorded axiom set grew — any axiom
    // not already in that law's own baseline record, whitelisted or not — or
    // whose backend changed). `write_baseline`: regenerate the baseline at
    // this path (the human-ack path for a legitimate removal — the change
    // becomes a reviewable git diff) and exit 0. The baseline is a committed,
    // code-reviewed file; CI runs `--gate` against it, never
    // `--write-baseline`. Both are Lean-only; on Dafny they are no-ops (no
    // per-law manifest is produced — see `run_proof_check`'s manifest block).
    gate: Option<&str>,
    write_baseline: Option<&str>,
    // Source-level `fn.law` identities declared by more than one `verify ...
    // law` block (see `duplicate_law_identities`). The ratchet fails CLOSED
    // (exit 2) on any duplicate before it can collapse two distinct law blocks
    // into one manifest entry.
    duplicate_laws: &[String],
) {
    use std::process::Command;

    let (cmd, args, label, backend_tag): (&str, Vec<String>, &str, &str) = match backend {
        super::cli::ProofBackend::Lean => {
            ("lake", vec!["build".to_string()], "Lean / lake", "lean")
        }
        super::cli::ProofBackend::Dafny => {
            // Verify the ACTUAL entry module, not whatever `read_dir`
            // happens to yield first. The entry file holds the verify-law
            // lemmas; in a multi-module project a dependency module picked
            // by chance does NOT include the entry, so the entry's laws
            // would go unverified and the check would false-green.
            // `dafny_entry` is the entry basename derived from the codegen
            // context (the same source the build hint prints); fall back to
            // a directory scan only if that file is somehow absent.
            let entry = dafny_entry
                .filter(|e| std::path::Path::new(output_dir).join(e).is_file())
                .or_else(|| match std::fs::read_dir(output_dir) {
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
                });
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
                // `--verify-included-files`: by default `dafny verify` trusts
                // every `include`d file, so a dependency module's termination
                // obligations (native `decreases` / fuel lemmas) would go
                // unchecked — a non-decreasing measure there would be silently
                // accepted. Verifying includes closes that trust gap; the
                // runtime prelude (`common.dfy`) verifies clean, so this adds
                // coverage without introducing spurious errors.
                vec![
                    "verify".to_string(),
                    "--verify-included-files".to_string(),
                    entry,
                ],
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

    // Per-backend metric + pass decision.
    //
    // Lean: the build itself must SUCCEED (lake exit 0), not merely stay
    // within the sorry budget. `count_lean_sorries` only sees the
    // non-fatal `declaration uses 'sorry'` warning and is blind to hard
    // errors like `unsolved goals` (lake exit 1, zero sorry-warnings).
    // Gating on exit status closes that false-green.
    //
    // Dafny is the SYMMETRIC case and needs the same discipline on two
    // fronts. (1) `dafny verify` exits non-zero not only on verification
    // errors but on out-of-resource / timeout / inconclusive (exit 4) —
    // none of which the parsed "M errors" field reflects — so we gate on
    // exit status too. (2) The emitter discharges laws it cannot prove
    // with `assume {:axiom} lhs == rhs;` (its own "sorry-style fallback",
    // toplevel.rs:1167/1826), which Dafny TRUSTS: 0 errors, exit 0. Those
    // axioms are the Dafny analog of Lean's `sorry`; count them and charge
    // against the sorry budget so a trusted-but-unproven law cannot pass.
    // (Opaque `function {:axiom}` declarations are NOT counted — like
    // Lean's `partial def` they trust a fn's definition, not a specific
    // law, and neither backend charges that fn-level trust to the budget.)
    let error_budget_v = error_budget.unwrap_or(0);
    let sorry_budget_v = sorry_budget.unwrap_or(0);
    // Lean only: model panic lines in the captured build output. The emitted
    // exports panic only at compiler-generated sites (fuel-wrapper
    // exhaustion, partial prelude builtins like `Char.toCode` on an empty
    // string), and Lean's `panic!` RETURNS the type's `default` instead of
    // aborting — under `native_decide` both sides of a model-vs-model sample
    // equation then reduce to `default` and the kernel certifies a vacuous
    // (possibly false) equality with lake exit 0 and zero sorries. The panic
    // line is the only trace, so ANY hit is a hard check failure (see
    // `count_model_panic_lines`).
    let mut model_panic_hits = 0usize;
    let (errors, sorries, axioms, omitted, budget, passed) = match backend {
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
            // An unproven law obligation has TWO shapes on Dafny, both of
            // which keep `errors == 0` / exit 0 and would false-green an
            // errors-only check: (1) `assume {:axiom} lhs == rhs;` (the law is
            // TRUSTED), and (2) the universal lemma is DROPPED entirely and
            // only concrete samples remain ("sample-only (universal lemma
            // omitted)" — the law's ∀-claim is never stated, so Dafny has
            // nothing to fail on). Both mean "this law was not proven
            // universally"; charge BOTH against the sorry budget so a degraded
            // law cannot pass. (Trace-projection "runtime-only" laws are a
            // deliberate non-Dafny gate, not a coverage claim, and are excluded.)
            let axioms = count_dafny_axioms(output_dir);
            let omitted = count_dafny_omitted_universals(output_dir);
            let unproven = axioms + omitted;
            let passed =
                output.status.success() && errors <= error_budget_v && unproven <= sorry_budget_v;
            (
                Some(errors),
                None,
                Some(axioms),
                Some(omitted),
                error_budget_v,
                passed,
            )
        }
        super::cli::ProofBackend::Lean => {
            let sorries = count_lean_sorries(&stderr) + count_lean_sorries(&stdout);
            model_panic_hits = lean_codegen::count_model_panic_lines(&stdout)
                + lean_codegen::count_model_panic_lines(&stderr);
            let passed =
                output.status.success() && sorries <= sorry_budget_v && model_panic_hits == 0;
            (None, Some(sorries), None, None, sorry_budget_v, passed)
        }
    };

    if model_panic_hits > 0 {
        eprintln!(
            "{}",
            format!(
                "--check: the Lean model panicked while evaluating a bounded sample \
                 ({} \"{}\" line(s) in the build output) — the exported model may \
                 disagree with the program, so its sample equations prove nothing; this is \
                 an Aver bug, please report it",
                model_panic_hits,
                lean_codegen::LEAN_PANIC_LINE_MARKER.trim_end()
            )
            .red()
        );
    }

    // Honest-coverage signal (Lean only): did the proof establish the law's
    // UNIVERSAL `∀`-claim by genuine kernel reasoning, or only by bounded
    // `native_decide` enumeration over the finite sample domain? `passed`
    // stays deliberately lenient — a bounded verify-on-domain is a
    // legitimate (if weaker) check the corpus must not regress on (e.g.
    // `examples/refinement/email`) — so the proof-corpus runner keys on this
    // `universal` field instead for an honest "what Aver kernel-proves"
    // count. (Dafny already folds the analogous "universal lemma omitted"
    // degradation into its own `passed`, so it needs no separate field.)
    let lean_law_audit: Option<LeanLawAudit> = match backend {
        super::cli::ProofBackend::Lean => {
            // Same short-circuit the bool always had: a failed counted
            // build (or a model panic) earns no audit run at all — the
            // probe would otherwise `#print axioms` against a stale or
            // partial environment.
            if output.status.success() && model_panic_hits == 0 {
                Some(lean_universal_audit(output_dir, sorries.unwrap_or(0)))
            } else {
                Some(LeanLawAudit::FAIL_CLOSED)
            }
        }
        super::cli::ProofBackend::Dafny => None,
    };
    let universal: Option<bool> = lean_law_audit.as_ref().map(|a| a.universal);

    // When-universal quarantine lane (Lean only): run the SEPARATE,
    // failure-tolerated per-law lane builds and the per-declaration
    // `#print axioms` crediting probes. Strictly ADDITIVE — it runs
    // after every counted metric above is already computed, and
    // nothing it does (including hard lane build failures) can touch
    // `sorries`/`passed`/`universal` or the process exit decision.
    let when_universal: Option<(usize, usize, Vec<ManifestLaw>)> = match backend {
        super::cli::ProofBackend::Lean => Some(run_when_universal_lane(output_dir)),
        super::cli::ProofBackend::Dafny => None,
    };

    // Proof manifest (Lean only): compose the file-level audit's per-law
    // records with the when-universal lane records (strongest tier wins per
    // `fn.law` identity) into one byte-reproducible per-law table, written to
    // `<out>/proof_manifest.json`. This is the artifact `--gate` diffs against
    // a committed baseline; it reuses the SAME class markers + `#print axioms`
    // verdicts already computed above (no extra lake invocation).
    let manifest: Option<ProofManifest> = match (&lean_law_audit, &when_universal) {
        (Some(audit), Some((_, _, lane_laws))) => {
            let m = build_proof_manifest(&audit.laws, lane_laws);
            write_proof_manifest(output_dir, &m);
            Some(m)
        }
        _ => None,
    };

    if check_json {
        let mut obj = serde_json::Map::new();
        obj.insert("backend".into(), backend_tag.into());
        if let Some(e) = errors {
            obj.insert("errors".into(), e.into());
        }
        if let Some(s) = sorries {
            obj.insert("sorries".into(), s.into());
        }
        if let Some(a) = axioms {
            obj.insert("axioms".into(), a.into());
            obj.insert("axiom_budget".into(), sorry_budget_v.into());
        }
        if let Some(o) = omitted {
            obj.insert("omitted".into(), o.into());
        }
        if let Some(u) = universal {
            obj.insert("universal".into(), u.into());
        }
        if let Some(audit) = &lean_law_audit {
            // ADDITIVE law-count fields, sourced from the same class
            // markers and `#print axioms` audit the `universal` bool
            // keys on (computed in the counted build, BEFORE the
            // when-universal lane runs — lane state cannot move them).
            obj.insert("universal_laws".into(), audit.universal_laws.into());
            obj.insert("bounded_laws".into(), audit.bounded_laws.into());
        }
        if let Some((credited, _, _)) = &when_universal {
            // ADDITIVE field: count of `when`-laws whose quarantine-lane
            // twin earned per-declaration universal credit. The file-level
            // `universal` flag above keeps its counted-build semantics.
            obj.insert("when_universal".into(), (*credited).into());
        }
        if matches!(backend, super::cli::ProofBackend::Lean) {
            // Renamed from the short-lived `fuel_exhausted` (0.25.0-unreleased
            // only; no consumer outside this repo's tests reads it —
            // proof-corpus/run.sh and the proof_spec gating tests key on
            // passed/universal/sorries) now that the gate scans for ANY model
            // panic line, not just the fuel-exhaustion marker. `true` means
            // the check FAILED with the compiler-model bug above regardless
            // of budgets.
            obj.insert("model_panicked".into(), (model_panic_hits > 0).into());
        }
        if manifest.is_some() {
            // ADDITIVE path pointer: the per-law table lives in its own file
            // (the gate's diff target), NOT inline in this summary line, so
            // existing substring consumers of check-json are untouched.
            obj.insert("manifest".into(), PROOF_MANIFEST_FILE.into());
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
        if let Some((credited, total, _)) = &when_universal
            && *total > 0
        {
            println!(
                "{}",
                format!(
                    "--check: when-universal lane — {credited}/{total} conditional law(s) \
                     proven universally (see when_universal_laws.json)"
                )
                .blue()
            );
        }
        let (metric, budget_desc) = match backend {
            super::cli::ProofBackend::Dafny => (
                format!(
                    "{} errors, {} axioms, {} omitted",
                    errors.unwrap_or(0),
                    axioms.unwrap_or(0),
                    omitted.unwrap_or(0)
                ),
                format!("errors ≤ {error_budget_v}, axioms+omitted ≤ {sorry_budget_v}"),
            ),
            super::cli::ProofBackend::Lean => (
                format!(
                    "{} sorries, universal: {}",
                    sorries.unwrap_or(0),
                    if universal == Some(true) { "yes" } else { "no" }
                ),
                format!("sorries ≤ {sorry_budget_v}"),
            ),
        };
        if passed {
            let suffix = if error_budget_v > 0 || sorry_budget_v > 0 {
                format!(" (within budget: {budget_desc})")
            } else {
                String::new()
            };
            println!("{}", format!("--check: {label} — {metric}{suffix}").green());
        } else {
            eprintln!(
                "{}",
                format!("--check: {label} — {metric} (budget: {budget_desc})").red()
            );
        }
    }

    // The ratchet: `--write-baseline` and `--gate`. Both need the per-law
    // manifest, which only the Lean lane produces (Dafny has no per-law
    // identity — scout Risk 3). Fold the gate verdict into ONE final exit so
    // `--check` and `--gate` compose without a double-exit.
    if write_baseline.is_some() || gate.is_some() {
        // Fail CLOSED on duplicate source law identity. Two distinct `verify
        // ... law` blocks sharing one `fn.law` key would collapse to a single
        // manifest entry (strongest-tier-wins), hiding a weakened duplicate or
        // reading a colliding rename as a benign merge — exactly the silent
        // weakening the ratchet exists to catch. A collision is a harness error
        // (exit 2), never a skip; the human must rename one of the laws.
        if !duplicate_laws.is_empty() {
            eprintln!(
                "{}",
                format!(
                    "--gate: duplicate law identity {{{}}} — two `verify ... law` blocks share \
                     one `fn.law` name. Rename one so each proven law has a distinct identity \
                     in the manifest.",
                    duplicate_laws.join(", ")
                )
                .red()
            );
            std::process::exit(2);
        }
        let Some(manifest) = &manifest else {
            eprintln!(
                "{}",
                "--gate: no per-law manifest available (gate is Lean-only; Dafny emits no \
                 per-law identity). Use --backend lean."
                    .red()
            );
            std::process::exit(2);
        };
        // Ack path first: regenerate the baseline and exit 0. A
        // legitimate removal/weakening becomes a reviewable git diff.
        if let Some(path) = write_baseline {
            match std::fs::write(path, proof_manifest_to_json(manifest)) {
                Ok(()) => {
                    println!(
                        "{}",
                        format!(
                            "--write-baseline: wrote {} law(s) to {path}",
                            manifest.laws.len()
                        )
                        .green()
                    );
                }
                Err(e) => {
                    eprintln!(
                        "{}",
                        format!("--write-baseline: write {path} failed: {e}").red()
                    );
                    std::process::exit(2);
                }
            }
            // `--write-baseline` always exits 0 after writing — it is the ack,
            // not a check. Honor any pre-existing `--check` failure first so a
            // broken proof can't be silently baselined.
            std::process::exit(if passed { 0 } else { 1 });
        }
        if let Some(baseline_path) = gate {
            let raw = match std::fs::read_to_string(baseline_path) {
                Ok(r) => r,
                Err(e) => {
                    eprintln!(
                        "{}",
                        format!("--gate: cannot read baseline {baseline_path}: {e}").red()
                    );
                    std::process::exit(2);
                }
            };
            let baseline = match parse_proof_manifest(&raw) {
                Ok(b) => b,
                Err(e) => {
                    eprintln!(
                        "{}",
                        format!(
                            "--gate: baseline {baseline_path} is not a valid proof manifest: {e}"
                        )
                        .red()
                    );
                    std::process::exit(2);
                }
            };
            let report = gate_manifest(&baseline, manifest);
            for line in &report.lines {
                if report.regressions == 0 {
                    println!("{}", line.blue());
                } else {
                    eprintln!("{}", line.red());
                }
            }
            if report.regressions > 0 {
                std::process::exit(1);
            }
            // Gate clean — but a within-gate proof can still have failed the
            // count `--check` (e.g. a NEW law sorry'd). Honor that.
            std::process::exit(if passed { 0 } else { 1 });
        }
    }

    if !passed {
        std::process::exit(1);
    }
}

/// Manifest filename written to the proof output dir by `run_proof_check`.
/// The committed baseline a `--gate` run diffs against is a copy of this
/// file at a known path.
const PROOF_MANIFEST_FILE: &str = "proof_manifest.json";

/// Collect `fn.law` identities that are declared by MORE THAN ONE source
/// `verify ... law` block. The manifest keys every law on this `fn.law`
/// identity; two distinct source law blocks sharing one identity would
/// otherwise collapse to a single manifest entry (strongest-tier-wins), which
/// silently hides a weakened duplicate or reads a colliding rename as a benign
/// merge. The ratchet must fail CLOSED on that ambiguity, so we detect it at
/// the SOURCE level (two distinct law blocks) — NOT at the manifest merge,
/// where the file-level audit and the when-universal lane legitimately emit one
/// record EACH for the SAME law and are meant to merge. Returns the colliding
/// identities sorted, so the harness-error message is deterministic.
fn duplicate_law_identities(items: &[TopLevel]) -> Vec<String> {
    let mut seen: HashSet<String> = HashSet::new();
    let mut dups: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    for item in items {
        if let TopLevel::Verify(vb) = item
            && let VerifyKind::Law(law) = &vb.kind
        {
            let identity = format!("{}.{}", vb.fn_name, law.name);
            if !seen.insert(identity.clone()) {
                dups.insert(identity);
            }
        }
    }
    dups.into_iter().collect()
}

/// The whole per-law proof manifest: a backend tag and the per-law records,
/// sorted by `fn.law` identity. Serialized deterministically (sorted keys,
/// sorted axiom arrays) so the committed baseline is a clean, byte-reproducible
/// git artifact.
#[derive(Debug)]
struct ProofManifest {
    backend: String,
    laws: Vec<ManifestLaw>,
}

/// Compose the file-level audit records with the when-universal lane records
/// into one per-law manifest. Keyed on the `fn.law` identity; on a collision
/// (a `when`-law that is BOTH file-level `bounded` and lane `universal`) the
/// STRONGER tier wins, so a credited conditional law is recorded `universal`.
/// Sorted by identity for byte-reproducibility.
fn build_proof_manifest(file_laws: &[ManifestLaw], lane_laws: &[ManifestLaw]) -> ProofManifest {
    let mut by_label: std::collections::BTreeMap<String, ManifestLaw> =
        std::collections::BTreeMap::new();
    for record in file_laws.iter().chain(lane_laws.iter()) {
        manifest_keep_stronger(&mut by_label, record.clone());
    }
    ProofManifest {
        backend: "lean".to_string(),
        laws: by_label.into_values().collect(),
    }
}

/// Serialize the manifest deterministically. `serde_json::Map` is a `BTreeMap`
/// (no `preserve_order` feature) so object keys are already alphabetical;
/// `laws` is sorted by identity HERE (defensively — `build_proof_manifest`
/// already sorts, but a manifest built any other way still serializes
/// byte-reproducibly) and each `axioms` array is sorted+deduped at
/// construction, so the output is byte-reproducible across runs.
fn proof_manifest_to_json(manifest: &ProofManifest) -> String {
    let mut sorted: Vec<&ManifestLaw> = manifest.laws.iter().collect();
    sorted.sort_by(|a, b| a.law.cmp(&b.law));
    let laws: Vec<serde_json::Value> = sorted
        .iter()
        .map(|l| {
            serde_json::json!({
                "law": l.law,
                "backend": l.backend,
                "tier": l.tier.as_str(),
                "axioms": l.axioms,
                "theorem": l.theorem,
            })
        })
        .collect();
    serde_json::to_string_pretty(&serde_json::json!({
        "version": 1,
        "backend": manifest.backend,
        "laws": laws,
    }))
    .unwrap_or_else(|_| "{}".to_string())
}

/// Write the manifest to `<dir>/proof_manifest.json`.
fn write_proof_manifest(dir: &str, manifest: &ProofManifest) {
    let path = std::path::Path::new(dir).join(PROOF_MANIFEST_FILE);
    let _ = std::fs::write(path, proof_manifest_to_json(manifest));
}

/// Parse a committed baseline manifest. FAILS CLOSED: a malformed per-law
/// record (missing `law`, missing/unknown `tier`) is a harness error
/// (`Err`), NOT a silently dropped record. A corrupt or truncated baseline
/// must never quietly un-ratchet the law it elided — the gate iterates the
/// baseline law set, so a skipped record would silently stop enforcing that
/// law. The informational `theorem` field staying absent is still tolerated.
/// `Err(msg)` → the caller exits 2 (harness failure); `Ok` only on a fully
/// well-formed manifest.
fn parse_proof_manifest(raw: &str) -> Result<ProofManifest, String> {
    let value: serde_json::Value =
        serde_json::from_str(raw).map_err(|e| format!("not valid JSON: {e}"))?;
    let arr = value["laws"]
        .as_array()
        .ok_or_else(|| "top-level `laws` is not an array".to_string())?;
    let mut laws = Vec::new();
    for (i, item) in arr.iter().enumerate() {
        let law = item["law"]
            .as_str()
            .ok_or_else(|| format!("law record #{i} is missing a string `law` field"))?;
        let tier_s = item["tier"]
            .as_str()
            .ok_or_else(|| format!("law `{law}` is missing a string `tier` field"))?;
        let tier = LawTier::from_str(tier_s)
            .ok_or_else(|| format!("law `{law}` has unknown tier `{tier_s}`"))?;
        let axioms: Vec<String> = item["axioms"]
            .as_array()
            .map(|a| {
                a.iter()
                    .filter_map(|v| v.as_str().map(str::to_string))
                    .collect()
            })
            .unwrap_or_default();
        laws.push(ManifestLaw {
            law: law.to_string(),
            backend: item["backend"].as_str().unwrap_or("lean").to_string(),
            tier,
            axioms,
            theorem: item["theorem"].as_str().unwrap_or("").to_string(),
        });
    }
    Ok(ProofManifest {
        backend: value["backend"].as_str().unwrap_or("lean").to_string(),
        laws,
    })
}

/// Result of comparing a current manifest against a committed baseline.
struct GateReport {
    /// Number of regressions found (missing / demoted / axiom-grew).
    regressions: usize,
    /// Human-readable report lines (one per regression, named, plus a
    /// summary). On a clean gate the lines are an INFO summary only.
    lines: Vec<String>,
}

/// THE RATCHET comparator (pure — unit-tested on recorded fixtures, no lake).
///
/// Iterate the BASELINE law set (Risk 5: keying on the baseline is the core
/// invariant — a law that vanished from `current` MUST still be inspected, so
/// a deletion is caught). For each baseline law, FAIL when one of:
///
/// - MISSING: absent from `current`.
/// - DEMOTED: `current` tier rank is below the baseline tier rank (the order
///   is universal, then bounded, then sampled, then failed). This is the
///   subtle case the ratchet exists for — a silent slide from a universal to a
///   bounded proof.
/// - AXIOM SET GREW: `current.axioms` is NOT a subset of `baseline.axioms` for
///   that law. EVERY axiom present now but absent from the law's OWN baseline
///   record is a regression — whitelisted or not. The whitelist governs
///   whether a law is *credited* universal in the first place (that decision
///   already happened upstream in the audit / lane); the ratchet here compares
///   each law against ITS OWN recorded axiom set, so a law that moves from
///   `propext`-only to ALSO leaning on `Classical.choice` (or `Quot.sound`,
///   `Lean.ofReduceBool`, `sorryAx`, …) is flagged even at an unchanged tier.
///   A SHRINKING axiom set (a strict subset) is fine — that is strengthening.
/// - BACKEND CHANGED: a baseline `lean` law that now records a different
///   backend is a regression (the proof no longer holds under the backend it
///   was certified by).
///
/// New laws in `current` but not `baseline` are OK (additions allowed),
/// reported as INFO.
fn gate_manifest(baseline: &ProofManifest, current: &ProofManifest) -> GateReport {
    use std::collections::BTreeMap;
    let current_by: BTreeMap<&str, &ManifestLaw> =
        current.laws.iter().map(|l| (l.law.as_str(), l)).collect();
    let baseline_by: BTreeMap<&str, &ManifestLaw> =
        baseline.laws.iter().map(|l| (l.law.as_str(), l)).collect();

    let mut lines = Vec::new();
    let mut regressions = 0usize;

    for bl in &baseline.laws {
        match current_by.get(bl.law.as_str()) {
            None => {
                regressions += 1;
                lines.push(format!(
                    "--gate: REGRESSION {}: MISSING (was {})",
                    bl.law,
                    bl.tier.as_str()
                ));
            }
            Some(cur) => {
                if cur.tier.rank() < bl.tier.rank() {
                    regressions += 1;
                    lines.push(format!(
                        "--gate: REGRESSION {}: tier {} -> {}",
                        bl.law,
                        bl.tier.as_str(),
                        cur.tier.as_str()
                    ));
                }
                // Backend must match: a law certified under one backend that
                // now only records another backend is a regression (the
                // certificate the baseline trusts is gone). Future-proofs the
                // manifest for a second backend (Dafny) without re-opening the
                // hole that a stored-but-unchecked `backend` field leaves.
                if cur.backend != bl.backend {
                    regressions += 1;
                    lines.push(format!(
                        "--gate: REGRESSION {}: backend {} -> {}",
                        bl.law, bl.backend, cur.backend
                    ));
                }
                // Axiom-set GROWTH = current axioms NOT a subset of the law's
                // OWN baseline axioms. ANY axiom present now but not in the
                // baseline record is a new trust dependency for THIS law —
                // whitelisted or not. A new `Classical.choice`/`Quot.sound`/
                // `propext` at an unchanged tier is still a regression here; the
                // whitelist only decided whether the law was credited universal
                // upstream, it does NOT excuse a law from growing its own
                // recorded axiom set.
                let grown: Vec<&String> = cur
                    .axioms
                    .iter()
                    .filter(|a| !bl.axioms.contains(*a))
                    .collect();
                if !grown.is_empty() {
                    regressions += 1;
                    lines.push(format!(
                        "--gate: REGRESSION {}: axioms grew {{{}}} -> {{{}}}",
                        bl.law,
                        bl.axioms.join(","),
                        cur.axioms.join(",")
                    ));
                }
            }
        }
    }

    let new_laws: Vec<&str> = current
        .laws
        .iter()
        .filter(|l| !baseline_by.contains_key(l.law.as_str()))
        .map(|l| l.law.as_str())
        .collect();

    let new_desc = if new_laws.is_empty() {
        "<none>".to_string()
    } else {
        new_laws.join(", ")
    };
    lines.push(format!(
        "--gate: {} regression(s) vs baseline ({} baseline laws, {} current). New laws OK: {}",
        regressions,
        baseline.laws.len(),
        current.laws.len(),
        new_desc
    ));

    GateReport { regressions, lines }
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

/// Count `assume {:axiom}` obligation trust-escapes across all emitted
/// `.dfy` files in `dir`. These are the Dafny analog of Lean's `sorry`:
/// when the emitter cannot prove a law (open-domain opaque recursion,
/// past the fuel cliff) it discharges the obligation with
/// `assume {:axiom} lhs == rhs;`, which Dafny TRUSTS — the proof verifies
/// with 0 errors and exit 0. Counting them (and charging against the
/// sorry budget) keeps `--check` honest and symmetric with
/// `count_lean_sorries`. Scans every `.dfy` so axioms in dependency
/// modules count too. (Opaque `function {:axiom}` declarations are not
/// counted; see the note at the pass-decision site.)
fn count_dafny_axioms(dir: &str) -> usize {
    let mut total = 0;
    if let Ok(rd) = std::fs::read_dir(dir) {
        for entry in rd.flatten() {
            let name = entry.file_name();
            if name.to_string_lossy().ends_with(".dfy")
                && let Ok(contents) = std::fs::read_to_string(entry.path())
            {
                total += contents.matches("assume {:axiom}").count();
            }
        }
    }
    total
}

/// Count laws whose universal `∀` lemma was DROPPED to sample-only across
/// the emitted `.dfy` files in `dir`. When the emitter cannot express a
/// law's universal claim (e.g. it calls a recursive fn still outside the
/// proof subset) it emits concrete sample assertions plus a
/// `…, sample-only (universal lemma omitted)` comment and NO `∀`-lemma.
/// Dafny then verifies with 0 errors / exit 0 because the universal claim
/// was never stated — a false-green the errors-only and axiom-only checks
/// both miss. These are unproven law obligations exactly like
/// `assume {:axiom}`, so `--check` charges them against the sorry budget
/// too. (The deliberate trace-projection `runtime-only` gate is NOT a
/// coverage claim and carries a different marker, so it is not counted.)
fn count_dafny_omitted_universals(dir: &str) -> usize {
    let mut total = 0;
    if let Ok(rd) = std::fs::read_dir(dir) {
        for entry in rd.flatten() {
            let name = entry.file_name();
            if name.to_string_lossy().ends_with(".dfy")
                && let Ok(contents) = std::fs::read_to_string(entry.path())
            {
                total += contents.matches("(universal lemma omitted)").count();
            }
        }
    }
    total
}

/// Count Lean's `declaration uses 'sorry'` warnings in build output.
/// Lake emits one such warning per `sorry` in the residual program;
/// counting them matches the budget the proof_spec gating tests use.
///
/// The quote glyph around `sorry` is toolchain-dependent: Lean ≤4.15
/// (the pinned toolchain) prints straight quotes `'sorry'`, but ≥4.17
/// switched to backticks `` `sorry` ``. A `sorry` is only a non-fatal
/// warning, so `lake` exits 0 and the exit-status gate does NOT catch
/// it — the count is the sole signal. Matching the literal `'sorry'`
/// alone would silently return 0 on a backtick-era toolchain, turning
/// a proof full of `sorry`s into a false-green. So match `declaration
/// uses` + `sorry` regardless of the surrounding glyph.
fn count_lean_sorries(s: &str) -> usize {
    s.lines()
        .filter(|l| l.contains("declaration uses") && l.contains("sorry"))
        .count()
}

/// Tier of a single proven law, strongest → weakest. The proof manifest
/// records this per law and the gate (`aver proof --gate`) FAILS when a
/// baseline law's tier drops in this order (universal > bounded > sampled >
/// failed). The numeric ranks are the comparison order; `Missing` (rank 0)
/// is not a marker the emitter writes — it is the synthetic verdict the gate
/// assigns to a baseline law that has vanished from the current manifest.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum LawTier {
    Universal,
    Bounded,
    Sampled,
    Failed,
    Missing,
}

impl LawTier {
    /// Comparison rank: higher is stronger. A drop in rank is a regression.
    fn rank(self) -> u8 {
        match self {
            LawTier::Universal => 4,
            LawTier::Bounded => 3,
            LawTier::Sampled => 2,
            LawTier::Failed => 1,
            LawTier::Missing => 0,
        }
    }

    /// Stable manifest spelling. `Missing` has no on-disk spelling (the gate
    /// never writes it), but it round-trips for symmetry.
    fn as_str(self) -> &'static str {
        match self {
            LawTier::Universal => "universal",
            LawTier::Bounded => "bounded",
            LawTier::Sampled => "sampled",
            LawTier::Failed => "failed",
            LawTier::Missing => "missing",
        }
    }

    fn from_str(s: &str) -> Option<LawTier> {
        match s {
            "universal" => Some(LawTier::Universal),
            "bounded" => Some(LawTier::Bounded),
            "sampled" => Some(LawTier::Sampled),
            "failed" => Some(LawTier::Failed),
            "missing" => Some(LawTier::Missing),
            _ => None,
        }
    }
}

/// One per-law record in the proof manifest: stable `fn.law` identity, the
/// backend that produced it, its verdict tier, and the sorted/deduped axiom
/// set its `#print axioms` probe reported. `theorem` is informational (the
/// emitted theorem name) and is NOT the identity.
#[derive(Clone, Debug)]
struct ManifestLaw {
    /// `fn.law` identity (the same key the when-universal lane emits).
    law: String,
    backend: String,
    tier: LawTier,
    /// Sorted, deduped kernel axioms (empty = `does not depend on any axioms`).
    axioms: Vec<String>,
    /// Emitted theorem name — informational, never compared.
    theorem: String,
}

/// Result of the Lean law-theorem audit (`lean_universal_audit`): the
/// file-level `universal` verdict plus the two additive law-count fields
/// surfaced in `--check-json`, and the per-law records the proof manifest
/// is built from.
struct LeanLawAudit {
    /// EXACTLY the old `lean_universal_proof` bool: every law theorem in
    /// the crediting set is kernel-clean, at least one is explicitly
    /// classed universal, and the file has no sorries.
    universal: bool,
    /// Law theorems classed `universal` whose own `#print axioms` line
    /// passed the kernel-axiom whitelist.
    universal_laws: usize,
    /// Law theorems classed `bounded-domain` by the emitter's statement-
    /// class marker.
    bounded_laws: usize,
    /// Per-law manifest records (file-level audit half), keyed on the
    /// `fn.law` identity read from the class marker. Composed with the
    /// when-universal lane detail in `run_proof_check` to form the manifest.
    laws: Vec<ManifestLaw>,
}

impl LeanLawAudit {
    /// Zero-credit verdict for paths where no classification evidence is
    /// readable at all (no lakefile roots, no law theorems, failed counted
    /// build) — the same fail-closed bias the bool always had.
    const FAIL_CLOSED: LeanLawAudit = LeanLawAudit {
        universal: false,
        universal_laws: 0,
        bounded_laws: 0,
        laws: Vec::new(),
    };
}

/// Honest-coverage distinguisher for the Lean backend: did the emitted
/// proof establish the law's UNIVERSAL `∀`-claim by genuine kernel
/// reasoning, or only by bounded `native_decide` enumeration over the
/// finite sample domain?
///
/// `passed` stays deliberately lenient — a bounded verify-on-domain is a
/// legitimate (if weaker) check the corpus must not regress on (e.g.
/// `examples/refinement/email`). But the proof-corpus coverage runner wants
/// the HONEST count: only laws whose `∀`-theorem is kernel-clean.
///
/// The ground-truth signal is `#print axioms`: a genuine proof depends only
/// on logical axioms (`propext` / `Classical.choice` / `Quot.sound`), while
/// `native_decide` injects `Lean.ofReduceBool` — the kernel trusting the
/// compiler's reduction of a `Bool` over the concrete domain, NOT the
/// universal claim. We collect the main law theorems (`*_law_*` / `*_eq_*`,
/// excluding the `_checked_domain` and `_sample_N` bounded cross-checks
/// built off the same base) and run `#print axioms` on each against the
/// freshly built environment. The law is universal iff EVERY participating
/// theorem is `ofReduceBool`-free — and at least one exists, and there are
/// no sorries.
///
/// Axiom-cleanliness alone is NOT enough: the theorem's STATEMENT must also
/// be the law's genuine `∀`-claim. A `when`-law over non-refinement-lifted
/// givens is emitted with sampled-domain disjunction premises prepended
/// (`a = 0 ∨ a = 1 ∨ … ->`), so even a real-tactic, axiom-clean proof of it
/// only establishes the law on the finite sample domain. Whether those
/// premises were prepended is knowable only at the statement-construction
/// site, so the emitter records a per-theorem class marker in the `.lean`
/// source (`-- aver:law-class <name> universal|bounded-domain`, see
/// `lean::LAW_CLASS_MARKER_PREFIX`) and this checker consumes it — it never
/// re-derives the class from names or by parsing statements:
///   - `bounded-domain`-classed theorems are EXCLUDED from universal
///     crediting (their axiom profile no longer matters here; sorries still
///     count file-wide via the `sorries` gate above);
///   - universal credit requires at least one emitted law theorem
///     explicitly classed `universal` — a dir with law theorems but no
///     markers (stale/foreign export not produced by this emitter) FAILS
///     CLOSED to `false` instead of reverting to name heuristics. The
///     `--check` flow always runs on a fresh emission (`cmd_proof` emits,
///     then calls `run_proof_check`, the only caller), so the channel is
///     present in practice;
///   - a name-matched theorem WITHOUT a marker (auxiliary `*_eq_*`-shaped
///     helper lemmas some strategies emit) stays in the axiom check —
///     conservative: it can only withhold credit, never grant it.
///
/// A task whose universal theorem was dropped entirely (`skip_universal`:
/// const-RHS singleton or a fuel-bounded recursive callee — the Lean analog
/// of Dafny's "universal lemma omitted") emits no `*_law_*` theorem, so the
/// empty set correctly reports `false`.
///
/// Conservative on any failure (missing `lake`, import error, non-zero
/// exit): returns `false`. A false "not-universal" only lowers the coverage
/// number, never inflates it — the right bias for an honest metric.
///
/// Besides the file-level `universal` bool the audit returns two ADDITIVE
/// counts sourced from the SAME class markers and the SAME `#print axioms`
/// probe output (no new trust surface):
///   - `universal_laws`: law theorems classed `universal` whose own probe
///     line stays within the kernel-axiom whitelist;
///   - `bounded_laws`: law theorems classed `bounded-domain` (a pure
///     classification count — no certificate involved).
///
/// The bool's semantics are untouched: `universal` remains the
/// all-or-nothing verdict over the whole crediting set (universal-classed
/// AND unmarked theorems), computed from the exact same expression as
/// before the counts existed.
fn lean_universal_audit(dir: &str, sorries: usize) -> LeanLawAudit {
    use std::process::Command;
    // Import every root from the lakefile so unqualified law-theorem names
    // resolve. The law theorems live at top level in the entry root;
    // importing all roots is robust without identifying which is the entry.
    let roots = lean_lakefile_roots(dir);
    if roots.is_empty() {
        return LeanLawAudit::FAIL_CLOSED;
    }
    // Collect the main universal law theorems across the emitted sources,
    // plus the emitter's per-theorem statement-class markers.
    let mut law_thms: Vec<String> = Vec::new();
    let mut classes: std::collections::HashMap<String, String> = std::collections::HashMap::new();
    // `theorem -> fn.law` identity, read off the marker's third field. The
    // label is the SAME identity the when-universal lane emits, so the proof
    // manifest keys file-level and lane laws on one stable key.
    let mut labels: std::collections::HashMap<String, String> = std::collections::HashMap::new();
    if let Ok(rd) = std::fs::read_dir(dir) {
        for entry in rd.flatten() {
            let name = entry.file_name().to_string_lossy().into_owned();
            if !name.ends_with(".lean") || name == "lakefile.lean" {
                continue;
            }
            if let Ok(contents) = std::fs::read_to_string(entry.path()) {
                // `DiscoveredLemmas.lean` WITH a cone-hash header is the
                // committed `--discover` ARTIFACT, not an emitted proof file:
                // lake doesn't build it (it's not a lakefile root), so
                // theorems scanned from it wouldn't resolve in the
                // axiom-checker environment — the lemmas that actually joined
                // a proof are embedded in the entry root and scanned there.
                // The header check keeps an entry MODULE legitimately named
                // `DiscoveredLemmas` (whose emitted root has no such header)
                // in the scan instead of silently zeroing its universal
                // metric.
                if name == "DiscoveredLemmas.lean" && contents.contains("-- cone-hash:") {
                    continue;
                }
                // Track `namespace … end` nesting so dep-module law theorems
                // (emitted INSIDE `namespace M` by the cross-file law pool) are
                // skipped here: the entry's own law theorems live at top level,
                // and a consumer law's `#print axioms` already inherits the dep
                // theorem's axiom footprint transitively. Probing the dep
                // theorem directly by its bare (unqualified) name would fail to
                // resolve and wrongly zero the metric. Single-file files have no
                // namespaces, so this is a no-op there (byte-identical metric).
                let mut namespace_depth = 0usize;
                for line in contents.lines() {
                    let trimmed = line.trim_start();
                    if trimmed.starts_with("namespace ") {
                        namespace_depth += 1;
                    } else if trimmed == "end" || trimmed.starts_with("end ") {
                        namespace_depth = namespace_depth.saturating_sub(1);
                    }
                    if let Some(rest) = line.strip_prefix(lean_codegen::LAW_CLASS_MARKER_PREFIX) {
                        let mut parts = rest.split_whitespace();
                        if let (Some(thm), Some(class)) = (parts.next(), parts.next()) {
                            classes.insert(thm.to_string(), class.to_string());
                            // Third field (optional on older emissions): the
                            // `fn.law` identity label for the manifest.
                            if let Some(label) = parts.next() {
                                labels.insert(thm.to_string(), label.to_string());
                            }
                        }
                        continue;
                    }
                    if namespace_depth > 0 {
                        continue;
                    }
                    if let Some(rest) = line.strip_prefix("theorem ") {
                        let thm = rest
                            .split_whitespace()
                            .next()
                            .unwrap_or("")
                            .trim_end_matches(':');
                        if is_main_law_theorem(thm) {
                            law_thms.push(thm.to_string());
                        }
                    }
                }
            }
        }
    }
    if law_thms.is_empty() {
        return LeanLawAudit::FAIL_CLOSED;
    }
    law_thms.sort();
    law_thms.dedup();
    // Class COUNTS, read off the same markers the crediting set below
    // keys on. `bounded_laws` is a pure classification count (the
    // emitter decided at statement-construction time that the theorem
    // only states the law on the finite sample domain) — it needs no
    // kernel certificate, so it survives every downstream gate.
    let bounded_law_keys = law_thms
        .iter()
        .filter_map(|t| {
            let class = law_class_for_theorem(t, &classes)?;
            (class == lean_codegen::LAW_CLASS_BOUNDED_DOMAIN)
                .then(|| law_dedup_key(t, &classes).to_string())
        })
        .collect::<std::collections::HashSet<_>>();
    let bounded_laws = bounded_law_keys.len();
    // Per-law manifest records for the `bounded-domain`-classed laws: deduped
    // by `fn.law` identity, tier `bounded`. These carry no kernel certificate
    // (they're native_decide'd over the finite sample domain), so their axiom
    // set is empty in the manifest. Computed BEFORE the `retain` below drops
    // bounded theorems from `law_thms`, and reused (cloned) on every return
    // path — the bounded set is the same regardless of how the universal half
    // resolves.
    let bounded_records: Vec<ManifestLaw> = {
        let mut by_label: std::collections::BTreeMap<String, ManifestLaw> =
            std::collections::BTreeMap::new();
        for thm in &law_thms {
            if !matches!(
                law_class_for_theorem(thm, &classes),
                Some(lean_codegen::LAW_CLASS_BOUNDED_DOMAIN)
            ) {
                continue;
            }
            let key = law_dedup_key(thm, &classes);
            let label = manifest_label_for(key, &labels);
            by_label
                .entry(label.clone())
                .or_insert_with(|| ManifestLaw {
                    law: label,
                    backend: "lean".to_string(),
                    tier: LawTier::Bounded,
                    axioms: Vec::new(),
                    theorem: thm.clone(),
                });
        }
        by_label.into_values().collect()
    };
    if sorries > 0 {
        return LeanLawAudit {
            universal: false,
            universal_laws: 0,
            bounded_laws,
            laws: bounded_records,
        };
    }
    // Consume the statement-class channel (see the doc comment above):
    // bounded-domain theorems leave the crediting set; at least one
    // explicitly universal-classed theorem must remain or the dir earns no
    // universal credit (fail-closed when the channel is absent).
    let mut universal_class_present = false;
    let mut universal_classed: Vec<String> = Vec::new();
    law_thms.retain(|thm| match law_class_for_theorem(thm, &classes) {
        Some(lean_codegen::LAW_CLASS_BOUNDED_DOMAIN) => false,
        Some(lean_codegen::LAW_CLASS_UNIVERSAL) => {
            universal_class_present = true;
            universal_classed.push(thm.clone());
            true
        }
        // Unmarked (or unknown class tag): keep it in the axiom check —
        // conservative — but it cannot by itself earn universal credit.
        _ => true,
    });
    if !universal_class_present {
        return LeanLawAudit {
            universal: false,
            universal_laws: 0,
            bounded_laws,
            laws: bounded_records,
        };
    }
    // Throwaway checker: print each main law theorem's axiom dependency
    // against the already-built environment.
    let mut src = String::new();
    for r in &roots {
        src.push_str("import ");
        src.push_str(r);
        src.push('\n');
    }
    for t in &law_thms {
        src.push_str("#print axioms ");
        src.push_str(t);
        src.push('\n');
    }
    let checker = std::path::Path::new(dir).join("_aver_axcheck.lean");
    if std::fs::write(&checker, &src).is_err() {
        return LeanLawAudit {
            universal: false,
            universal_laws: 0,
            bounded_laws,
            laws: bounded_records,
        };
    }
    let out = Command::new("lake")
        .args(["env", "lean", "_aver_axcheck.lean"])
        .current_dir(dir)
        .output();
    let _ = std::fs::remove_file(&checker);
    match out {
        Ok(o) => {
            let combined = format!(
                "{}{}",
                String::from_utf8_lossy(&o.stdout),
                String::from_utf8_lossy(&o.stderr)
            );
            // WHITELIST: every axiom a law theorem depends on must be one of
            // the three core logical axioms. A blacklist (no `ofReduceBool` =
            // native_decide, no `sorryAx`) was equivalent while every line of
            // emitted Lean came from typed IR — but the discovery feedback
            // loop embeds COMMITTED `DiscoveredLemmas.lean` text verbatim, so
            // an artifact smuggling e.g. a top-level `axiom` declaration must
            // flip the metric to false, not slide past a name blacklist. The
            // output format is `'name' depends on axioms: [a, b]` (or `does
            // not depend on any axioms`); the two blacklist probes stay as a
            // belt-and-suspenders floor against output-format drift.
            let universal = o.status.success()
                && lean_axiom_lines_whitelisted(&combined)
                && !combined.contains("Lean.ofReduceBool")
                && !combined.contains("sorryAx");
            // Per-theorem attribution over the SAME probe output: a
            // universal-classed theorem is counted iff its own `#print
            // axioms` line stays within the kernel whitelist (the same
            // per-declaration parser the when-universal lane credits
            // with). When `universal` is true above, every line passed,
            // so `universal_laws` equals the universal-classed count —
            // the file-level bool keeps EXACTLY its all-or-nothing
            // semantics, the count just shows how many theorems the
            // certificate covers (and, on a degraded file, how many
            // survived).
            let universal_laws = if o.status.success() {
                universal_classed
                    .iter()
                    .filter(|t| lane_credit_from_probe(&combined, t))
                    .count()
            } else {
                0
            };
            // Per-law manifest records for the universal-classed laws, from
            // the SAME probe output: tier `universal` iff the theorem's own
            // `#print axioms` line stays within the kernel whitelist (the
            // exact `lane_credit_from_probe` decision `universal_laws` counts),
            // else `failed`. Axioms are the parsed, sorted set the gate diffs.
            // Deduped by `fn.law` identity so chunked `_part<N>` theorems
            // collapse onto one law (strongest-wins if they disagree).
            let mut universal_records: std::collections::BTreeMap<String, ManifestLaw> =
                std::collections::BTreeMap::new();
            for thm in &universal_classed {
                let key = law_dedup_key(thm, &classes);
                let label = manifest_label_for(key, &labels);
                let credited = o.status.success() && lane_credit_from_probe(&combined, thm);
                let tier = if credited {
                    LawTier::Universal
                } else {
                    LawTier::Failed
                };
                let axioms = axioms_for_theorem(&combined, thm).unwrap_or_default();
                let record = ManifestLaw {
                    law: label.clone(),
                    backend: "lean".to_string(),
                    tier,
                    axioms,
                    theorem: thm.clone(),
                };
                manifest_keep_stronger(&mut universal_records, record);
            }
            let mut laws = bounded_records;
            laws.extend(universal_records.into_values());
            laws.sort_by(|a, b| a.law.cmp(&b.law));
            LeanLawAudit {
                universal,
                universal_laws,
                bounded_laws,
                laws,
            }
        }
        Err(_) => LeanLawAudit {
            universal: false,
            universal_laws: 0,
            bounded_laws,
            laws: bounded_records,
        },
    }
}

/// Resolve the `fn.law` manifest identity for a law theorem. Prefers the
/// label recorded in the class marker's third field (the SAME `fn.law` the
/// when-universal lane emits); falls back to the theorem name itself only on
/// an older emission that didn't carry the label (so the law still appears in
/// the manifest under a stable-per-emission key rather than vanishing).
fn manifest_label_for(theorem: &str, labels: &std::collections::HashMap<String, String>) -> String {
    labels
        .get(theorem)
        .cloned()
        .unwrap_or_else(|| theorem.to_string())
}

/// Insert `record` into `by_label`, keeping the STRONGER tier on a collision
/// (two theorems mapping to one `fn.law` identity — e.g. chunked `_part<N>`
/// pieces). Axioms follow the kept tier's record. A demotion can only be
/// recorded by the gate, never hidden by a sibling chunk, so strongest-wins is
/// the right merge for the per-law manifest.
fn manifest_keep_stronger(
    by_label: &mut std::collections::BTreeMap<String, ManifestLaw>,
    record: ManifestLaw,
) {
    match by_label.get(&record.law) {
        Some(existing) if existing.tier.rank() >= record.tier.rank() => {}
        _ => {
            by_label.insert(record.law.clone(), record);
        }
    }
}

/// Detail artifact for the when-universal quarantine lane, written to
/// the proof output dir by `run_when_universal_lane`.
const WHEN_UNIVERSAL_DETAIL_FILE: &str = "when_universal_laws.json";

/// Run the when-universal quarantine lane checks (see
/// `lean::universal_lane`): for every law listed in the emitted lane
/// index (`_aver_universal_lane.json`), (1) `lake build <lane-lib>` as
/// a SEPARATE, failure-TOLERATED invocation — a hard failure
/// (elaboration error, maxHeartbeats, anything) is absorbed at the
/// process boundary and means only "this law stays bounded"; (2) a
/// per-declaration crediting probe: `import <module>` + `#print
/// axioms <theorem>` against the freshly built lane module. Credit
/// requires the declaration to exist, the import to elaborate, and
/// the axiom set to stay within {propext, Classical.choice,
/// Quot.sound} — NEVER an invocation exit code alone.
///
/// Returns `(credited, total, manifest_laws)` and writes the per-law
/// detail artifact (`when_universal_laws.json`). No lane index →
/// `(0, 0, vec![])` and any stale detail artifact is removed. Counted
/// metrics are computed BEFORE this runs and are mathematically out of
/// its reach. The returned `manifest_laws` carry the per-law `fn.law`
/// identity + tier (universal when credited, else failed) + parsed axiom
/// set; `run_proof_check` merges them with the file-level audit records
/// (strongest-wins) to form the proof manifest, so a credited `when`-law
/// is recorded `universal` even though the file-level audit classed it
/// `bounded`.
fn run_when_universal_lane(dir: &str) -> (usize, usize, Vec<ManifestLaw>) {
    use std::process::Command;
    let manifest_path =
        std::path::Path::new(dir).join(lean_codegen::universal_lane::LANE_MANIFEST_FILE);
    let detail_path = std::path::Path::new(dir).join(WHEN_UNIVERSAL_DETAIL_FILE);
    let Ok(raw) = std::fs::read_to_string(&manifest_path) else {
        let _ = std::fs::remove_file(&detail_path);
        return (0, 0, Vec::new());
    };
    let Ok(manifest) = serde_json::from_str::<serde_json::Value>(&raw) else {
        let _ = std::fs::remove_file(&detail_path);
        return (0, 0, Vec::new());
    };
    let laws = manifest["laws"].as_array().cloned().unwrap_or_default();
    // Laws the collision guard honestly omitted: surfaced verbatim into
    // the detail artifact so a withheld law is a visible note, never a
    // silent gap (an unguarded clash would fail a tolerated build and
    // strip a neighbor's credit instead).
    let omitted = manifest["omitted"].as_array().cloned().unwrap_or_default();
    let mut details: Vec<serde_json::Value> = Vec::new();
    let mut manifest_laws: Vec<ManifestLaw> = Vec::new();
    let mut credited = 0usize;
    let mut total = 0usize;
    for law in &laws {
        let (Some(label), Some(theorem), Some(module)) = (
            law["law"].as_str(),
            law["theorem"].as_str(),
            law["module"].as_str(),
        ) else {
            continue;
        };
        total += 1;
        // (1) Failure-tolerated build of this law's own non-default lib
        // (lib name == module name). Its exit code is deliberately NOT
        // a crediting signal; a failed build just leaves the module
        // un-importable, which the probe below reports as no-credit.
        let _ = Command::new("lake")
            .args(["build", module])
            .current_dir(dir)
            .output();
        // (2) Per-declaration evidence.
        let (universal, evidence) = lane_probe_declaration(dir, module, theorem);
        if universal {
            credited += 1;
        }
        // Manifest record for this lane law. A credited universal twin is
        // tier `universal` with its parsed axiom set; an un-credited one is
        // `failed` HERE — the merge in `run_proof_check` lets the file-level
        // `bounded` classification (which this law also has) win, so the law
        // is recorded at its true strongest tier `bounded`, never lost.
        let tier = if universal {
            LawTier::Universal
        } else {
            LawTier::Failed
        };
        let axioms = axioms_for_theorem(&evidence, theorem).unwrap_or_default();
        manifest_laws.push(ManifestLaw {
            law: label.to_string(),
            backend: "lean".to_string(),
            tier,
            axioms,
            theorem: theorem.to_string(),
        });
        details.push(serde_json::json!({
            "law": label,
            "theorem": theorem,
            "module": module,
            "universal": universal,
            "evidence": evidence,
        }));
    }
    let _ = std::fs::write(
        &detail_path,
        serde_json::to_string_pretty(&serde_json::json!({
            "when_universal": credited,
            "laws": details,
            "omitted": omitted,
        }))
        .unwrap_or_else(|_| "{}".to_string()),
    );
    (credited, total, manifest_laws)
}

/// Per-declaration crediting probe for one lane law: `import
/// <module>` + `#print axioms <theorem>` via `lake env lean`. Returns
/// `(credited, evidence)` where evidence is the `#print axioms`
/// output line (quoted verbatim into the detail artifact). Fail-closed
/// on every error path — a missing module, missing declaration, or
/// unparseable output earns no credit.
fn lane_probe_declaration(dir: &str, module: &str, theorem: &str) -> (bool, String) {
    use std::process::Command;
    let src = format!("import {module}\n#print axioms {theorem}\n");
    let probe = std::path::Path::new(dir).join("_aver_lane_axcheck.lean");
    if std::fs::write(&probe, &src).is_err() {
        return (false, "probe write failed".to_string());
    }
    let out = Command::new("lake")
        .args(["env", "lean", "_aver_lane_axcheck.lean"])
        .current_dir(dir)
        .output();
    let _ = std::fs::remove_file(&probe);
    match out {
        Ok(o) => {
            let combined = format!(
                "{}{}",
                String::from_utf8_lossy(&o.stdout),
                String::from_utf8_lossy(&o.stderr)
            );
            let credited = o.status.success() && lane_credit_from_probe(&combined, theorem);
            let evidence = combined
                .lines()
                .find(|l| l.contains(&format!("'{theorem}'")))
                .map(str::to_string)
                .unwrap_or_else(|| {
                    let tail: Vec<&str> = combined.lines().take(3).collect();
                    format!(
                        "no #print axioms line for '{theorem}': {}",
                        tail.join(" | ")
                    )
                });
            (credited, evidence)
        }
        Err(e) => (false, format!("probe spawn failed: {e}")),
    }
}

/// Parse the crediting decision out of a `#print axioms` probe output
/// for `theorem`. The declaration must be PRESENT (its own result
/// line exists) and its axiom set must stay within the whitelist —
/// `sorryAx` (a sorry-floored proof) and `Lean.ofReduceBool`
/// (`native_decide`) are rejected explicitly on top of the whitelist.
/// Anything else — error output, missing line, unknown constant —
/// earns no credit. Pure parser, unit-tested directly.
fn lane_credit_from_probe(output: &str, theorem: &str) -> bool {
    let needle = format!("'{theorem}'");
    for line in output.lines() {
        if !line.contains(&needle) {
            continue;
        }
        if line.contains("does not depend on any axioms") {
            return true;
        }
        if line.contains("depends on axioms:") {
            return lean_axiom_lines_whitelisted(line)
                && !line.contains("Lean.ofReduceBool")
                && !line.contains("sorryAx");
        }
    }
    false
}

/// Parse the SORTED, DEDUPED axiom set a theorem depends on out of a
/// `#print axioms` probe output. `Some(vec![])` = the declaration is present
/// and `does not depend on any axioms`; `Some([a, b, …])` = it depends on the
/// listed axioms; `None` = no result line for the theorem (missing / error).
/// The manifest records this set per law so the gate can flag axiom-set GROWTH
/// (a proof that newly leans on `Lean.ofReduceBool` / `sorryAx` / any axiom
/// outside the recorded baseline set).
fn axioms_for_theorem(output: &str, theorem: &str) -> Option<Vec<String>> {
    let needle = format!("'{theorem}'");
    for line in output.lines() {
        if !line.contains(&needle) {
            continue;
        }
        if line.contains("does not depend on any axioms") {
            return Some(Vec::new());
        }
        if let Some(idx) = line.find("depends on axioms:") {
            let list = line[idx + "depends on axioms:".len()..]
                .trim()
                .trim_start_matches('[')
                .trim_end_matches(']');
            let mut axioms: Vec<String> = list
                .split(',')
                .map(str::trim)
                .filter(|a| !a.is_empty())
                .map(str::to_string)
                .collect();
            axioms.sort();
            axioms.dedup();
            return Some(axioms);
        }
    }
    None
}

/// `true` iff every `#print axioms` result line in `output` reports only the
/// core logical axioms (`propext`, `Classical.choice`, `Quot.sound`). Lines
/// not matching the `depends on axioms: […]` shape are ignored — the caller's
/// blacklist probes remain the floor for those.
fn lean_axiom_lines_whitelisted(output: &str) -> bool {
    const ALLOWED: [&str; 3] = ["propext", "Classical.choice", "Quot.sound"];
    for line in output.lines() {
        let Some(idx) = line.find("depends on axioms:") else {
            continue;
        };
        let list = line[idx + "depends on axioms:".len()..]
            .trim()
            .trim_start_matches('[')
            .trim_end_matches(']');
        for axiom in list.split(',') {
            let axiom = axiom.trim();
            if !axiom.is_empty() && !ALLOWED.contains(&axiom) {
                return false;
            }
        }
    }
    true
}

/// True for the main universal law theorem names emitted by the Lean
/// backend (`<fn>_law_<law>` or `<fn>_eq_<spec>`), excluding the bounded
/// cross-checks built off the same base (`_checked_domain`, `_sample_N`)
/// which are proved by `native_decide` by design.
fn is_main_law_theorem(name: &str) -> bool {
    if !(name.contains("_law_") || name.contains("_eq_")) {
        return false;
    }
    if name.ends_with("_checked_domain") {
        return false;
    }
    // Large given-domain products emit the checked-domain conjunction
    // chunked into `_checked_domain_part<N>` theorems (one elaborator-
    // sized piece each) — same bounded cross-check, same exclusion.
    if let Some(idx) = name.rfind("_checked_domain_part") {
        let tail = &name[idx + "_checked_domain_part".len()..];
        if !tail.is_empty() && tail.bytes().all(|b| b.is_ascii_digit()) {
            return false;
        }
    }
    if let Some(idx) = name.rfind("_sample_") {
        let tail = &name[idx + "_sample_".len()..];
        if !tail.is_empty() && tail.bytes().all(|b| b.is_ascii_digit()) {
            return false;
        }
    }
    true
}

fn law_class_for_theorem<'a>(
    theorem: &str,
    classes: &'a std::collections::HashMap<String, String>,
) -> Option<&'a str> {
    classes.get(theorem).map(String::as_str).or_else(|| {
        law_class_base_name(theorem).and_then(|base| classes.get(base).map(String::as_str))
    })
}

/// Counting key that collapses `<base>_part<N>` chunk declarations onto their
/// base law, mirroring `law_class_for_theorem`'s direct-lookup-first logic: the
/// base name is used ONLY when the part carries no class marker of its own (so
/// its class was resolved via the base-name fallback). A theorem with its OWN
/// `-- aver:law-class` marker is a distinct law even if it happens to be named
/// `part1`/`part2`, so it keeps its own name as the key.
fn law_dedup_key<'a>(
    theorem: &'a str,
    classes: &std::collections::HashMap<String, String>,
) -> &'a str {
    if classes.contains_key(theorem) {
        return theorem;
    }
    law_class_base_name(theorem).unwrap_or(theorem)
}

fn law_class_base_name(theorem: &str) -> Option<&str> {
    let idx = theorem.rfind("_part")?;
    let tail = &theorem[idx + "_part".len()..];
    (!tail.is_empty() && tail.bytes().all(|b| b.is_ascii_digit())).then_some(&theorem[..idx])
}

/// Parse the root modules out of a generated `lakefile.lean`
/// (`roots := #[`A, `B]`).
fn lean_lakefile_roots(dir: &str) -> Vec<String> {
    let path = std::path::Path::new(dir).join("lakefile.lean");
    let Ok(contents) = std::fs::read_to_string(path) else {
        return Vec::new();
    };
    for line in contents.lines() {
        let line = line.trim();
        if let Some(rest) = line.strip_prefix("roots :=") {
            return rest
                .split(',')
                .filter_map(|tok| {
                    let t = tok
                        .trim()
                        .trim_start_matches("#[")
                        .trim_end_matches(']')
                        .trim()
                        .trim_start_matches('`')
                        .trim();
                    (!t.is_empty()).then(|| t.to_string())
                })
                .collect();
        }
    }
    Vec::new()
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

    let mut output = lean_codegen::transpile_for_proof_mode(ctx, verify_mode);

    // When-universal quarantine lane (see `lean::universal_lane`):
    // ADDITIVE twin theorems for recognized scalar-sign `when`-laws,
    // hosted in non-default per-law `lean_lib`s under
    // `universal_lane/`. The counted default build, every budget pin
    // and the manifest emission are untouched by construction — the
    // lane only appends files and trailing lakefile entries.
    // `AVER_PROOF_NO_UNIVERSAL_LANE` (test hook) disables the lane
    // entirely; `AVER_PROOF_LANE_SABOTAGE=<label-substring>` (test
    // hook) injects a failing tactic into the matching law's module —
    // the executable iron-guard check.
    let lane = if matches!(verify_mode, lean_codegen::VerifyEmitMode::NativeDecide)
        && std::env::var("AVER_PROOF_NO_UNIVERSAL_LANE").is_err()
    {
        let entry_name = format!("{}.lean", aver::codegen::common::entry_basename(ctx));
        let entry_content = output
            .files
            .iter()
            .find(|(name, _)| *name == entry_name)
            .map(|(_, content)| content.clone())
            .unwrap_or_default();
        let sabotage = std::env::var("AVER_PROOF_LANE_SABOTAGE").ok();
        let chain = std::env::var("AVER_PROOF_LANE_CHAIN").is_ok();
        lean_codegen::universal_lane::generate(ctx, &entry_content, sabotage.as_deref(), chain)
    } else {
        lean_codegen::universal_lane::LaneOutput {
            files: Vec::new(),
            omitted: Vec::new(),
        }
    };
    if lane.files.is_empty() && lane.omitted.is_empty() {
        // Lane disabled or nothing recognized (and nothing omitted):
        // retire any stale lane index from a previous emission so
        // `--check` can never credit outdated modules.
        let _ = std::fs::remove_file(
            Path::new(output_dir).join(lean_codegen::universal_lane::LANE_MANIFEST_FILE),
        );
    } else {
        for (name, content) in &mut output.files {
            if name == "lakefile.lean" {
                *content =
                    lean_codegen::universal_lane::lakefile_with_lane_libs(content, &lane.files);
            }
        }
        output.files.push((
            lean_codegen::universal_lane::LANE_MANIFEST_FILE.to_string(),
            lean_codegen::universal_lane::lane_manifest_json(&lane),
        ));
        for law in &lane.files {
            output.files.push((
                format!(
                    "{}/{}.lean",
                    lean_codegen::universal_lane::LANE_SUBDIR,
                    law.module
                ),
                law.content.clone(),
            ));
        }
    }

    let build_hint = format!("cd {} && lake build", output_dir);
    write_codegen_output(file, output_dir, "Lean 4", &build_hint, &output);
}

fn cmd_proof_dafny(file: &str, output_dir: &str, ctx: &codegen::CodegenContext) {
    use aver::codegen::dafny as dafny_codegen;

    // Example-based `verify f` (concrete `f(x) => y` cases) are an
    // EVALUATION check: Lean verifies them with `native_decide` (which
    // runs the function) and `aver verify` samples them at runtime. The
    // Dafny backend is a PROVER, not an evaluator — it cannot reduce a
    // concrete recursive / List / String case (attempting it floods
    // spurious errors and can hang the verifier), so it does not check
    // case-form verify and only proves law-form `verify`. Warn so the
    // silence isn't mistaken for a Dafny-verified pass (law blocks ARE
    // proven; the Lean backend and `aver verify` cover the examples).
    let unchecked_case_blocks = ctx
        .items
        .iter()
        .filter(|i| matches!(i, TopLevel::Verify(vb) if matches!(vb.kind, VerifyKind::Cases)))
        .count();
    if unchecked_case_blocks > 0 {
        eprintln!(
            "{}",
            format!(
                "warning: {unchecked_case_blocks} example-based `verify` block(s) are NOT \
                 checked by the Dafny backend (Dafny proves laws, not concrete examples) — \
                 they are verified by `aver proof --backend lean` and `aver verify`"
            )
            .yellow()
        );
    }

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

    // `verify … law` blocks in a dependency module ARE carried now (the
    // cross-file law pool): each proven dep law is emitted as a theorem
    // in the dep's `.lean` and admitted into a consumer law's lemma pool
    // under the same cone ∪ subject gate as in-file siblings. A dep law
    // that does not itself prove emits only samples (no universal
    // theorem), so it can never launder credit to a consumer. Plain
    // example-style `verify` blocks in a dep are still NOT checked
    // (module-scoped sampling is a separate, larger feature) — warn only
    // for those so an unchecked dependency sample isn't mistaken for a
    // proven one.
    let dep_example_verify_count = items
        .iter()
        .filter(|i| matches!(i, TopLevel::Verify(vb) if !matches!(vb.kind, aver::ast::VerifyKind::Law(_))))
        .count();
    if dep_example_verify_count > 0 {
        eprintln!(
            "{}",
            format!(
                "warning: {dep_example_verify_count} non-law verify block(s) in dependency \
                 module '{name}' are NOT checked (module-scoped sampling is not yet supported) — \
                 move them to the entry module to sample them"
            )
            .yellow()
        );
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
        verify_laws: aver::codegen::collect_verify_laws(&items),
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

    #[test]
    fn lean_axiom_whitelist_rejects_foreign_axioms() {
        // The universal metric must be a WHITELIST over `#print axioms`
        // output: only the three core logical axioms pass. A custom axiom
        // (e.g. smuggled through a tampered DiscoveredLemmas.lean and
        // referenced transitively by a law's proof) is neither
        // `Lean.ofReduceBool` nor `sorryAx`, so the old blacklist would have
        // reported universal:true for a false law.
        let clean = "'f_law_x' depends on axioms: [propext, Classical.choice, Quot.sound]";
        let none = "'f_law_x' does not depend on any axioms";
        let foreign = "'f_law_x' depends on axioms: [propext, cheat]";
        let native = "'f_law_x' depends on axioms: [Lean.ofReduceBool]";
        assert!(super::lean_axiom_lines_whitelisted(clean));
        assert!(super::lean_axiom_lines_whitelisted(none));
        assert!(!super::lean_axiom_lines_whitelisted(foreign));
        assert!(!super::lean_axiom_lines_whitelisted(native));
    }

    // ---- THE RATCHET: pure comparator + parser, fixture-driven (no lake) ----

    fn law(name: &str, tier: super::LawTier, axioms: &[&str]) -> super::ManifestLaw {
        super::ManifestLaw {
            law: name.to_string(),
            backend: "lean".to_string(),
            tier,
            axioms: axioms.iter().map(|a| a.to_string()).collect(),
            theorem: format!("{}_thm", name.replace('.', "_")),
        }
    }

    fn manifest(laws: Vec<super::ManifestLaw>) -> super::ProofManifest {
        super::ProofManifest {
            backend: "lean".to_string(),
            laws,
        }
    }

    #[test]
    fn law_tier_rank_strict_order() {
        use super::LawTier::*;
        // The demote detector depends on this strict order; pin it.
        assert!(Universal.rank() > Bounded.rank());
        assert!(Bounded.rank() > Sampled.rank());
        assert!(Sampled.rank() > Failed.rank());
        assert!(Failed.rank() > Missing.rank());
    }

    #[test]
    fn gate_deleted_law_fails_named_missing() {
        // The founding-claim falsifier #1: a proven law removed entirely. The
        // gate iterates the BASELINE set, so a vanished law is still inspected
        // and reported MISSING (scout Risk 5 — the core invariant).
        let base = manifest(vec![
            law(
                "floorQ.cellFloorStable",
                super::LawTier::Universal,
                &["Quot.sound", "propext"],
            ),
            law(
                "coarseFloorEq.sharedCellFloor",
                super::LawTier::Universal,
                &["Quot.sound", "propext"],
            ),
        ]);
        let cur = manifest(vec![law(
            "floorQ.cellFloorStable",
            super::LawTier::Universal,
            &["Quot.sound", "propext"],
        )]);
        let report = super::gate_manifest(&base, &cur);
        assert_eq!(report.regressions, 1, "missing law must be a regression");
        assert!(
            report
                .lines
                .iter()
                .any(|l| l.contains("coarseFloorEq.sharedCellFloor")
                    && l.contains("MISSING")
                    && l.contains("was universal")),
            "must name the missing law with its old tier: {:?}",
            report.lines
        );
    }

    #[test]
    fn gate_demoted_law_fails_with_tiers() {
        // The SUBTLE soundness case the ratchet exists for: a law silently
        // slides universal -> bounded. The count-based gate stays green; the
        // ratchet must FAIL and name the tier change.
        let base = manifest(vec![law(
            "floorQ.cellFloorStable",
            super::LawTier::Universal,
            &["Quot.sound", "propext"],
        )]);
        let cur = manifest(vec![law(
            "floorQ.cellFloorStable",
            super::LawTier::Bounded,
            &[],
        )]);
        let report = super::gate_manifest(&base, &cur);
        assert_eq!(report.regressions, 1, "demotion must be a regression");
        assert!(
            report
                .lines
                .iter()
                .any(|l| l.contains("floorQ.cellFloorStable")
                    && l.contains("tier universal -> bounded")),
            "must name the demoted law with before/after tiers: {:?}",
            report.lines
        );
    }

    #[test]
    fn gate_axiom_set_growth_fails() {
        // A proof that newly leans on an axiom outside the recorded set (e.g.
        // native_decide's `Lean.ofReduceBool`) is a trust regression even if
        // the tier is unchanged.
        let base = manifest(vec![law(
            "f.law",
            super::LawTier::Universal,
            &["propext", "Quot.sound"],
        )]);
        let cur = manifest(vec![law(
            "f.law",
            super::LawTier::Universal,
            &["Lean.ofReduceBool", "propext", "Quot.sound"],
        )]);
        let report = super::gate_manifest(&base, &cur);
        assert_eq!(report.regressions, 1, "axiom growth must be a regression");
        assert!(
            report.lines.iter().any(|l| l.contains("f.law")
                && l.contains("axioms grew")
                && l.contains("Lean.ofReduceBool")),
            "must name the law and the grown axiom: {:?}",
            report.lines
        );
    }

    #[test]
    fn gate_axiom_set_shrink_is_clean() {
        // Dropping an axiom (strengthening) is NOT a regression.
        let base = manifest(vec![law(
            "f.law",
            super::LawTier::Universal,
            &["propext", "Quot.sound"],
        )]);
        let cur = manifest(vec![law("f.law", super::LawTier::Universal, &["propext"])]);
        let report = super::gate_manifest(&base, &cur);
        assert_eq!(report.regressions, 0, "a smaller axiom set is clean");
    }

    #[test]
    fn gate_clean_when_baseline_preserved() {
        // Identical manifest -> green, no regressions.
        let base = manifest(vec![
            law("a.one", super::LawTier::Universal, &["propext"]),
            law("b.two", super::LawTier::Bounded, &[]),
        ]);
        let report = super::gate_manifest(&base, &base);
        assert_eq!(report.regressions, 0);
    }

    #[test]
    fn gate_new_law_is_allowed_and_reported() {
        // Additions are OK (reported INFO, not a regression).
        let base = manifest(vec![law("a.one", super::LawTier::Universal, &["propext"])]);
        let cur = manifest(vec![
            law("a.one", super::LawTier::Universal, &["propext"]),
            law("c.three", super::LawTier::Universal, &["propext"]),
        ]);
        let report = super::gate_manifest(&base, &cur);
        assert_eq!(report.regressions, 0, "a new law is not a regression");
        assert!(
            report
                .lines
                .iter()
                .any(|l| l.contains("New laws OK: c.three")),
            "new law must be reported as INFO: {:?}",
            report.lines
        );
    }

    #[test]
    fn gate_promotion_with_no_new_axioms_is_clean() {
        // A pure tier promotion (bounded -> universal) that introduces NO new
        // axiom is a strengthening, never a regression. The axiom set is a
        // subset (here: equal, both empty), so the strict-subset axiom check
        // passes and the higher tier is welcome.
        let base = manifest(vec![law("f.law", super::LawTier::Bounded, &[])]);
        let cur = manifest(vec![law("f.law", super::LawTier::Universal, &[])]);
        let report = super::gate_manifest(&base, &cur);
        assert_eq!(report.regressions, 0);
    }

    #[test]
    fn gate_promotion_that_adds_axioms_fails_until_rebaselined() {
        // A promotion that GAINS axioms is NOT silently clean: the ratchet
        // compares each law against its OWN baseline axiom set, so a bounded
        // law recorded with no axioms that newly leans on `propext`/`Quot.sound`
        // grows its set and FAILS — even though the tier strengthened. The
        // honest path is to re-baseline (`--write-baseline`), which makes the
        // new axioms a reviewable diff. This is the inverse of the old behavior,
        // which locked whitelisted additions as clean and let new trust axioms
        // slip in unnoticed.
        let base = manifest(vec![law("f.law", super::LawTier::Bounded, &[])]);
        let cur = manifest(vec![law(
            "f.law",
            super::LawTier::Universal,
            &["propext", "Quot.sound"],
        )]);
        let report = super::gate_manifest(&base, &cur);
        assert_eq!(
            report.regressions, 1,
            "a promotion that adds axioms must FAIL until re-baselined"
        );
        assert!(
            report
                .lines
                .iter()
                .any(|l| l.contains("f.law") && l.contains("axioms grew")),
            "must name the law and its grown axiom set: {:?}",
            report.lines
        );
    }

    #[test]
    fn gate_new_whitelisted_axiom_at_same_tier_fails() {
        // BLOCKER fix: a law that gains a NEW WHITELISTED axiom
        // (`Classical.choice`) at the SAME tier is a regression. The axiom check
        // is a TRUE SUBSET check against the law's own baseline record — the
        // whitelist does not excuse a law from growing its recorded axiom set.
        // Under the old whitelist-exempt logic this slipped through clean.
        let base = manifest(vec![law(
            "f.law",
            super::LawTier::Universal,
            &["propext", "Quot.sound"],
        )]);
        let cur = manifest(vec![law(
            "f.law",
            super::LawTier::Universal,
            &["Classical.choice", "propext", "Quot.sound"],
        )]);
        let report = super::gate_manifest(&base, &cur);
        assert_eq!(
            report.regressions, 1,
            "a new whitelisted axiom at the same tier must be a regression"
        );
        assert!(
            report.lines.iter().any(|l| l.contains("f.law")
                && l.contains("axioms grew")
                && l.contains("Classical.choice")),
            "must name the law and the new whitelisted axiom: {:?}",
            report.lines
        );
    }

    #[test]
    fn gate_backend_change_fails() {
        // MAJOR 2: a baseline law certified under one backend that now records a
        // different backend is a regression (the certificate the baseline trusts
        // is gone). The stored per-law `backend` field is ratcheted, not just
        // tier + axioms.
        let base = manifest(vec![law("f.law", super::LawTier::Universal, &["propext"])]);
        let mut cur_law = law("f.law", super::LawTier::Universal, &["propext"]);
        cur_law.backend = "dafny".to_string();
        let cur = manifest(vec![cur_law]);
        let report = super::gate_manifest(&base, &cur);
        assert_eq!(
            report.regressions, 1,
            "a backend change must be a regression"
        );
        assert!(
            report
                .lines
                .iter()
                .any(|l| l.contains("f.law") && l.contains("backend lean -> dafny")),
            "must name the law and the backend change: {:?}",
            report.lines
        );
    }

    #[test]
    fn proof_manifest_json_roundtrips() {
        // Serialize -> parse -> the per-law identity/tier/axioms survive, and
        // the serialization is sorted (byte-reproducible baseline).
        let m = manifest(vec![
            law("b.two", super::LawTier::Bounded, &[]),
            law(
                "a.one",
                super::LawTier::Universal,
                &["Quot.sound", "propext"],
            ),
        ]);
        let json = super::proof_manifest_to_json(&m);
        // `laws` array is sorted by identity at construction; check `a.one`
        // appears before `b.two` in the serialized text.
        let a_pos = json.find("a.one").expect("a.one present");
        let b_pos = json.find("b.two").expect("b.two present");
        assert!(a_pos < b_pos, "laws must serialize sorted by identity");
        let parsed = super::parse_proof_manifest(&json).expect("parses back");
        assert_eq!(parsed.laws.len(), 2);
        let a = parsed.laws.iter().find(|l| l.law == "a.one").unwrap();
        assert_eq!(a.tier, super::LawTier::Universal);
        assert_eq!(
            a.axioms,
            vec!["Quot.sound".to_string(), "propext".to_string()]
        );
    }

    #[test]
    fn parse_manifest_unknown_tier_fails_closed() {
        // MAJOR 1 fix: a per-law record with an unknown tier is a harness error
        // (the caller exits 2), NOT a silently dropped record. A corrupt or
        // truncated baseline must never quietly un-ratchet the law it elided —
        // the gate iterates the baseline law set, so a skipped record would
        // silently stop enforcing that law. (Old behavior dropped it and still
        // returned a partial manifest.)
        let raw = r#"{"version":1,"backend":"lean","laws":[
            {"law":"a.one","tier":"universal","axioms":["propext"]},
            {"law":"b.bad","tier":"quantum","axioms":[]}
        ]}"#;
        let err = super::parse_proof_manifest(raw)
            .expect_err("an unknown tier must fail the parse, not be skipped");
        assert!(
            err.contains("b.bad") && err.contains("quantum"),
            "the error must name the offending law and its bad tier: {err}"
        );
    }

    #[test]
    fn parse_manifest_missing_law_field_fails_closed() {
        // A record missing its identity `law` field is also a harness error,
        // never a silent skip.
        let raw = r#"{"version":1,"backend":"lean","laws":[
            {"tier":"universal","axioms":["propext"]}
        ]}"#;
        assert!(
            super::parse_proof_manifest(raw).is_err(),
            "a record missing `law` must fail the parse"
        );
    }

    #[test]
    fn parse_manifest_rejects_non_object() {
        assert!(super::parse_proof_manifest("[]").is_err());
        assert!(super::parse_proof_manifest("not json").is_err());
    }

    #[test]
    fn axioms_for_theorem_parses_sorted_deduped() {
        let out = "'f_law_x' depends on axioms: [propext, Quot.sound, propext]";
        let got = super::axioms_for_theorem(out, "f_law_x").expect("present");
        assert_eq!(got, vec!["Quot.sound".to_string(), "propext".to_string()]);
        let none = "'f_law_x' does not depend on any axioms";
        assert_eq!(super::axioms_for_theorem(none, "f_law_x"), Some(Vec::new()));
        assert_eq!(super::axioms_for_theorem("unrelated", "f_law_x"), None);
    }

    /// Build a minimal `verify <fn> law <name>` top-level item for the
    /// duplicate-identity detector.
    fn law_block(fn_name: &str, law_name: &str) -> super::TopLevel {
        use aver::ast::{Expr, Literal, Spanned, VerifyBlock, VerifyKind, VerifyLaw};
        let t = || Spanned::bare(Expr::Literal(Literal::Bool(true)));
        let vk = VerifyKind::Law(Box::new(VerifyLaw {
            name: law_name.to_string(),
            givens: vec![],
            when: None,
            lhs: t(),
            rhs: t(),
            sample_guards: vec![],
        }));
        super::TopLevel::Verify(VerifyBlock::new_unspanned(
            fn_name.to_string(),
            0,
            vec![],
            vk,
        ))
    }

    #[test]
    fn duplicate_law_identities_detected() {
        // MAJOR 3: two distinct `verify ... law` blocks sharing one `fn.law`
        // identity is a collision the ratchet must catch — the manifest keys on
        // `fn.law`, so they would otherwise collapse to one entry and hide a
        // weakened duplicate. The detector reports the collision; the caller
        // fails CLOSED (exit 2).
        let items = vec![
            law_block("floorQ", "cellFloorStable"),
            law_block("floorQ", "cellFloorStable"),
            law_block("coarseFloorEq", "sharedCellFloor"),
        ];
        let dups = super::duplicate_law_identities(&items);
        assert_eq!(
            dups,
            vec!["floorQ.cellFloorStable".to_string()],
            "the one collision must be reported once, named by `fn.law` identity"
        );
    }

    #[test]
    fn distinct_law_identities_have_no_duplicates() {
        // Distinct `fn.law` identities (including the same law name under a
        // different fn) are not collisions.
        let items = vec![
            law_block("floorQ", "cellFloorStable"),
            law_block("coarseFloorEq", "cellFloorStable"),
            law_block("coarseFloorEq", "sharedCellFloor"),
        ];
        assert!(
            super::duplicate_law_identities(&items).is_empty(),
            "distinct identities must not be flagged"
        );
    }

    #[test]
    fn lane_credit_keys_on_per_declaration_evidence() {
        // When-universal quarantine lane crediting: the decision is a
        // pure parse over the `#print axioms` probe output, keyed to
        // ONE declaration — never an invocation exit code. Each
        // negative below is a distinct no-credit class.
        let thm = "foo_law_bar_universal";
        // Credited: declaration present + whitelisted axioms.
        let clean =
            "'foo_law_bar_universal' depends on axioms: [propext, Classical.choice, Quot.sound]";
        assert!(super::lane_credit_from_probe(clean, thm));
        let axiom_free = "'foo_law_bar_universal' does not depend on any axioms";
        assert!(super::lane_credit_from_probe(axiom_free, thm));
        // No credit: missing declaration (probe errored — there is no
        // result line for the theorem at all).
        let missing = "error: unknown constant 'someOtherName'";
        assert!(!super::lane_credit_from_probe(missing, thm));
        // No credit: sorry-floored proof (sorryAx in the dependency set).
        let sorried = "'foo_law_bar_universal' depends on axioms: [propext, sorryAx]";
        assert!(!super::lane_credit_from_probe(sorried, thm));
        // No credit: native_decide (Lean.ofReduceBool).
        let native = "'foo_law_bar_universal' depends on axioms: [propext, Lean.ofReduceBool]";
        assert!(!super::lane_credit_from_probe(native, thm));
        // No credit: any axiom outside the whitelist.
        let extra = "'foo_law_bar_universal' depends on axioms: [propext, smuggledAxiom]";
        assert!(!super::lane_credit_from_probe(extra, thm));
        // No credit: a DIFFERENT declaration's clean line cannot pay
        // for ours (per-declaration, not per-invocation).
        let other = "'other_thm' depends on axioms: [propext]";
        assert!(!super::lane_credit_from_probe(other, thm));
    }

    #[test]
    fn count_lean_sorries_matches_both_quote_glyphs() {
        // Lean ≤4.15 prints straight quotes, ≥4.17 prints backticks.
        // `sorry` is a non-fatal warning (lake exits 0), so the count is
        // the only signal — it must survive the glyph switch or every
        // `sorry` silently passes as a false-green.
        let straight = "warning: Foo.lean:1:8: declaration uses 'sorry'";
        let backtick = "warning: Foo.lean:1:8: declaration uses `sorry`";
        assert_eq!(super::count_lean_sorries(straight), 1, "straight quotes");
        assert_eq!(super::count_lean_sorries(backtick), 1, "backticks");
        assert_eq!(
            super::count_lean_sorries(&format!("{straight}\n{backtick}\nunrelated line")),
            2,
            "both glyphs counted, unrelated lines ignored"
        );
        assert_eq!(super::count_lean_sorries("Build completed successfully"), 0);
    }

    fn empty_codegen_ctx() -> CodegenContext {
        CodegenContext {
            items: vec![],
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
            mir_program: None,
            bare_i64: Default::default(),
            discovered_lemmas: Vec::new(),
            sample_expected: std::collections::HashMap::new(),
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

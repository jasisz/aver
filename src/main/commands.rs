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
use aver::resolver;
use aver::source::{find_module_file, require_module_declaration};
use aver::tco;
use aver::types::checker::run_type_check_full;
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
        name == "prog" && parse_type_str(type_ann) == Type::Named("Program".to_string())
    });
    let has_module_fns = fd.params.iter().any(|(name, type_ann)| {
        name == "moduleFns"
            && parse_type_str(type_ann) == Type::List(Box::new(Type::Named("FnDef".to_string())))
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
        Type::Named(name) => {
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
        Type::Int | Type::Float | Type::Str | Type::Bool | Type::Unit | Type::Unknown => {}
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
    run_verify_blocks: bool,
    record_dir: Option<&str>,
    program_args: Vec<String>,
    profile: bool,
    entry_expression: Option<&str>,
) {
    use aver::replay::{
        JsonValue, session::RecordedOutcome, session::SessionRecording,
        session_recording_to_string_pretty,
    };

    if run_verify_blocks && record_dir.is_some() {
        eprintln!(
            "{}",
            "Cannot combine --verify and --record in one run; record should capture only main flow."
                .red()
        );
        process::exit(1);
    }

    if run_verify_blocks && entry_expression.is_some() {
        eprintln!(
            "{}",
            "Cannot combine --verify with --expr / --input-file.".red()
        );
        process::exit(1);
    }

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

    // TCO transform
    tco::transform_program(&mut items);

    // Type check
    let tc_result = run_type_check_full(&items, Some(&module_root));
    if !tc_result.errors.is_empty() {
        eprintln!(
            "{}",
            super::shared::format_type_errors(&tc_result.errors).red()
        );
        process::exit(1);
    }

    // Resolver
    resolver::resolve_program(&mut items);

    // Compile to bytecode
    let mut arena = Arena::new();
    vm::register_service_types(&mut arena);
    let (code, globals) =
        match vm::compile_program_with_modules(&items, &mut arena, Some(&module_root), file) {
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

    if run_verify_blocks {
        println!();
        let cfg = load_runtime_policy(&module_root).unwrap_or_else(|e| {
            eprintln!("{}", e.red());
            process::exit(1);
        });
        match aver::diagnostics::vm_verify::run_verify_for_items_vm(
            items,
            cfg,
            Some(&module_root),
            file,
        ) {
            Ok(results) => {
                let failed: usize = results.iter().map(|r| r.failed).sum();
                let file_results = vec![VerifyFileResult {
                    path: file.to_string(),
                    source: source.clone(),
                    blocks: results,
                }];
                render_verify_output(&file_results, &module_root, false, false);
                if failed > 0 {
                    process::exit(1);
                }
            }
            Err(e) => {
                eprintln!("{}", e.red());
                process::exit(1);
            }
        }
    }
}

/// Compile to WASM and execute with built-in host.
/// Uses aver/* import ABI — host provides capabilities natively.
pub(super) fn cmd_run_wasm(
    file: &str,
    module_root_override: Option<&str>,
    program_args: Vec<String>,
) {
    #[cfg(not(feature = "wasm"))]
    {
        let _ = (file, module_root_override, program_args);
        eprintln!("{}", "WASM requires --features wasm".red());
        process::exit(1);
    }

    #[cfg(feature = "wasm")]
    {
        #[cfg(feature = "terminal")]
        let _terminal_guard = aver_rt::TerminalGuard::new();

        use aver::codegen;

        let (ctx, _module_root) = build_codegen_context(
            file,
            None, // project_name
            module_root_override,
            false,
            &super::cli::CompilePolicyMode::Embed,
            None,
            false,
        );

        // Compile to WASM with aver/* ABI
        let wasm_bytes = match codegen::wasm::emit_wasm(&ctx) {
            Ok(bytes) => bytes,
            Err(e) => {
                eprintln!("{}", format!("WASM compilation error: {}", e).red());
                process::exit(1);
            }
        };
        if let Ok(path) = std::env::var("AVER_DEBUG_DUMP_WASM") {
            let _ = std::fs::write(path, &wasm_bytes);
        }

        // Run with wasmtime host
        match run_wasm_with_host(&wasm_bytes, &program_args) {
            Ok(()) => {}
            Err(e) => {
                eprintln!("{}", format!("WASM execution error: {}", e).red());
                process::exit(1);
            }
        }
    }
}

#[cfg(feature = "wasm")]
thread_local! {
    static VARIANT_NAMES: std::cell::RefCell<std::collections::HashMap<u32, String>> =
        std::cell::RefCell::new(std::collections::HashMap::new());
}

#[cfg(feature = "wasm")]
fn load_variant_names_from_instance(
    instance: &wasmtime::Instance,
    store: &mut wasmtime::Store<()>,
) {
    let ptr_global = instance.get_global(&mut *store, "$variant_names_ptr");
    let len_global = instance.get_global(&mut *store, "$variant_names_len");
    if let (Some(pg), Some(lg)) = (ptr_global, len_global) {
        let ptr = pg.get(&mut *store).i32().unwrap_or(0) as usize;
        let len = lg.get(&mut *store).i32().unwrap_or(0) as usize;
        if len > 0 {
            let mem = instance
                .get_memory(&mut *store, "memory")
                .expect("memory export");
            let data = mem.data(&*store);
            if ptr + len <= data.len() {
                let text = String::from_utf8_lossy(&data[ptr..ptr + len]).to_string();
                let mut map = std::collections::HashMap::new();
                for entry in text.split('|') {
                    if let Some(colon) = entry.find(':')
                        && let Ok(tag) = entry[..colon].parse::<u32>()
                    {
                        map.insert(tag, entry[colon + 1..].to_string());
                    }
                }
                VARIANT_NAMES.with(|names| *names.borrow_mut() = map);
            }
        }
    }
}

#[cfg(feature = "wasm")]
fn variant_name(tag: u64) -> String {
    VARIANT_NAMES.with(|names| {
        names
            .borrow()
            .get(&(tag as u32))
            .cloned()
            .unwrap_or_else(|| format!("Variant#{}", tag))
    })
}

#[cfg(feature = "wasm")]
/// Format a WASM value (i64) by reading heap structures from memory.
fn format_wasm_value(val: i64, mem: &[u8]) -> String {
    let ptr = val as u32 as usize;
    let io_scratch = 128; // IO_SCRATCH_SIZE

    // Check if it looks like a heap pointer
    if ptr >= io_scratch && ptr + 8 <= mem.len() {
        let header = u64::from_le_bytes(mem[ptr..ptr + 8].try_into().unwrap_or([0; 8]));
        let kind = (header >> 56) & 0xFF;
        let field_count = header & 0xFFFFFFFF;

        if kind > 11 {
            // Not a valid heap object kind — treat as integer
            return format!("{}", val);
        }

        match kind {
            0 => {
                // OBJ_STRING — nested strings get quotes (aver_display_inner)
                let len = field_count as usize;
                if ptr + 8 + len <= mem.len() {
                    let bytes = &mem[ptr + 8..ptr + 8 + len];
                    let s = String::from_utf8_lossy(bytes);
                    return format!("\"{}\"", s);
                }
            }
            11 => {
                // OBJ_MAP_ENTRY — format as {"key": value, ...}
                // Dedup: first occurrence wins (matches Map.get behavior)
                let mut seen_keys = std::collections::HashSet::new();
                let mut entries = Vec::new();
                let mut cur = ptr;
                while cur != 0 && cur + 24 <= mem.len() {
                    let h = u64::from_le_bytes(mem[cur..cur + 8].try_into().unwrap_or([0; 8]));
                    if (h >> 56) & 0xFF != 11 {
                        break;
                    }
                    let head =
                        u64::from_le_bytes(mem[cur + 8..cur + 16].try_into().unwrap_or([0; 8]));
                    let tuple_ptr = head as u32 as usize;
                    if tuple_ptr + 24 <= mem.len() {
                        let key_i64 = u64::from_le_bytes(
                            mem[tuple_ptr + 8..tuple_ptr + 16]
                                .try_into()
                                .unwrap_or([0; 8]),
                        );
                        let val_i64 = u64::from_le_bytes(
                            mem[tuple_ptr + 16..tuple_ptr + 24]
                                .try_into()
                                .unwrap_or([0; 8]),
                        );
                        let key_str = format_wasm_value(key_i64 as i64, mem);
                        if seen_keys.insert(key_str.clone()) {
                            let val_str = format_wasm_value(val_i64 as i64, mem);
                            entries.push(format!("{}: {}", key_str, val_str));
                        }
                    }
                    let tail =
                        u64::from_le_bytes(mem[cur + 16..cur + 24].try_into().unwrap_or([0; 8]));
                    cur = tail as u32 as usize;
                }
                return format!("{{{}}}", entries.join(", "));
            }
            4 | 9 => {
                // OBJ_LIST_CONS / OBJ_LIST_CONS_F64
                let is_f64 = kind == 9;
                let mut items = Vec::new();
                let mut cur = ptr;
                while cur != 0 && cur + 24 <= mem.len() {
                    let h = u64::from_le_bytes(mem[cur..cur + 8].try_into().unwrap_or([0; 8]));
                    if (h >> 56) & 0xFF != kind {
                        break;
                    }
                    let head =
                        u64::from_le_bytes(mem[cur + 8..cur + 16].try_into().unwrap_or([0; 8]));
                    if is_f64 {
                        items.push(format!("{}", f64::from_bits(head)));
                    } else {
                        items.push(format_wasm_value(head as i64, mem));
                    }
                    let tail =
                        u64::from_le_bytes(mem[cur + 16..cur + 24].try_into().unwrap_or([0; 8]));
                    cur = tail as u32 as usize;
                }
                return format!("[{}]", items.join(", "));
            }
            5 => {
                // OBJ_TUPLE
                let count = field_count as usize;
                let mut items = Vec::new();
                for i in 0..count {
                    if ptr + 8 + (i + 1) * 8 <= mem.len() {
                        let field = u64::from_le_bytes(
                            mem[ptr + 8 + i * 8..ptr + 8 + (i + 1) * 8]
                                .try_into()
                                .unwrap_or([0; 8]),
                        );
                        items.push(format_wasm_value(field as i64, mem));
                    }
                }
                return format!("({})", items.join(", "));
            }
            3 | 7 | 8 => {
                // OBJ_WRAPPER / OBJ_WRAPPER_F64 / OBJ_WRAPPER_I32
                let tag = (header >> 48) & 0xFF;
                let prefix = match tag {
                    0 => "Result.Ok",
                    1 => "Result.Err",
                    2 => "Option.Some",
                    _ => "Wrapper",
                };
                if ptr + 16 <= mem.len() {
                    let inner =
                        u64::from_le_bytes(mem[ptr + 8..ptr + 16].try_into().unwrap_or([0; 8]));
                    let inner_str = if kind == 7 {
                        format!("{}", f64::from_bits(inner))
                    } else if kind == 8 {
                        let inner_ptr = inner as u32 as usize;
                        if inner_ptr >= io_scratch {
                            // format_wasm_value already adds quotes for strings
                            format_wasm_value(inner as i64, mem)
                        } else {
                            format!("{}", inner)
                        }
                    } else {
                        format_wasm_value(inner as i64, mem)
                    };
                    return format!("{}({})", prefix, inner_str);
                }
            }
            2 => {
                // OBJ_VARIANT
                let tag = (header >> 48) & 0xFF;
                let count = field_count as usize;
                let mut fields = Vec::new();
                for i in 0..count {
                    if ptr + 8 + (i + 1) * 8 <= mem.len() {
                        let field = u64::from_le_bytes(
                            mem[ptr + 8 + i * 8..ptr + 8 + (i + 1) * 8]
                                .try_into()
                                .unwrap_or([0; 8]),
                        );
                        fields.push(format_wasm_value(field as i64, mem));
                    }
                }
                let name = variant_name(tag);
                if count == 0 {
                    return name;
                }
                return format!("{}({})", name, fields.join(", "));
            }
            1 => {
                // OBJ_RECORD
                let count = field_count as usize;
                let mut fields = Vec::new();
                for i in 0..count {
                    if ptr + 8 + (i + 1) * 8 <= mem.len() {
                        let field = u64::from_le_bytes(
                            mem[ptr + 8 + i * 8..ptr + 8 + (i + 1) * 8]
                                .try_into()
                                .unwrap_or([0; 8]),
                        );
                        fields.push(format_wasm_value(field as i64, mem));
                    }
                }
                return format!("Record({})", fields.join(", "));
            }
            _ => {}
        }
    }

    // Default: print as integer
    format!("{}", val)
}

#[cfg(feature = "wasm")]
/// Format a tagged value to string.
/// tag: 0=Int, 1=Float(bits), 2=Bool, 3=String(ptr), 4=Heap(ptr), 5=Unit
fn format_tagged_value(tag: i32, val: i64, mem: &[u8]) -> String {
    match tag {
        0 => format!("{}", val),                        // Int
        1 => format!("{}", f64::from_bits(val as u64)), // Float
        2 => {
            if val != 0 {
                "true".to_string()
            } else {
                "false".to_string()
            }
        } // Bool
        3 => {
            // String pointer
            let ptr = val as u32 as usize;
            if ptr + 8 <= mem.len() {
                let header = u64::from_le_bytes(mem[ptr..ptr + 8].try_into().unwrap_or([0; 8]));
                let len = (header & 0xFFFFFFFF) as usize;
                if ptr + 8 + len <= mem.len() {
                    return String::from_utf8_lossy(&mem[ptr + 8..ptr + 8 + len]).to_string();
                }
            }
            String::new()
        }
        4 => {
            // Heap pointer — check sentinels first
            if val == 0 {
                return "[]".to_string();
            }
            if val == -1 {
                return "Option.None".to_string();
            }
            format_wasm_value(val, mem)
        }
        5 => String::new(), // Unit
        _ => format!("{}", val),
    }
}

#[cfg(feature = "wasm")]
fn wasm_guest_bytes(caller: &mut wasmtime::Caller<'_, ()>, ptr: i32, len: i32) -> Vec<u8> {
    if ptr < 0 || len < 0 {
        return Vec::new();
    }
    let mem = caller.get_export("memory").unwrap().into_memory().unwrap();
    let data = mem.data(&*caller);
    let start = ptr as usize;
    let end = start.saturating_add(len as usize);
    if end > data.len() {
        return Vec::new();
    }
    data[start..end].to_vec()
}

#[cfg(feature = "wasm")]
fn wasm_guest_string(caller: &mut wasmtime::Caller<'_, ()>, ptr: i32, len: i32) -> String {
    String::from_utf8_lossy(&wasm_guest_bytes(caller, ptr, len)).to_string()
}

#[cfg(feature = "wasm")]
fn wasm_write_guest_bytes(caller: &mut wasmtime::Caller<'_, ()>, bytes: &[u8]) -> (i32, i32) {
    let mem = caller.get_export("memory").unwrap().into_memory().unwrap();
    // Short strings: IO_SCRATCH tail (bytes 96-127).
    const SCRATCH_BASE: usize = 96;
    const SCRATCH_CAP: usize = 32;
    if bytes.len() <= SCRATCH_CAP {
        mem.data_mut(caller)[SCRATCH_BASE..SCRATCH_BASE + bytes.len()].copy_from_slice(bytes);
        return (SCRATCH_BASE as i32, bytes.len() as i32);
    }
    // Longer strings: use exported $alloc to avoid heap collision.
    if let Some(alloc) = caller.get_export("alloc").and_then(|e| e.into_func()) {
        let mut result = [wasmtime::Val::I32(0)];
        if alloc
            .call(
                &mut *caller,
                &[wasmtime::Val::I32(bytes.len() as i32)],
                &mut result,
            )
            .is_ok()
        {
            let ptr = result[0].i32().unwrap_or(0);
            let start = ptr as usize;
            let mem = caller.get_export("memory").unwrap().into_memory().unwrap();
            mem.data_mut(caller)[start..start + bytes.len()].copy_from_slice(bytes);
            return (ptr, bytes.len() as i32);
        }
    }
    // Fallback: end of memory.
    let mem_size = mem.data_size(&*caller);
    let reserve = bytes.len().saturating_add(64);
    let ptr = mem_size.saturating_sub(reserve) as i32;
    let start = ptr as usize;
    let end = start.saturating_add(bytes.len());
    if end <= mem_size {
        mem.data_mut(caller)[start..end].copy_from_slice(bytes);
    }
    (ptr, bytes.len() as i32)
}

#[cfg(feature = "wasm")]
fn wasm_write_guest_string(caller: &mut wasmtime::Caller<'_, ()>, text: &str) -> (i32, i32) {
    wasm_write_guest_bytes(caller, text.as_bytes())
}

#[cfg(feature = "wasm")]
fn run_wasm_with_host(wasm_bytes: &[u8], program_args: &[String]) -> Result<(), String> {
    use wasmtime::*;

    let engine = Engine::default();

    // Step 1 of the WAT runtime migration: aver_runtime is a separate
    // wasm module that owns memory + the bump allocator. Instantiate it
    // first; user.wasm imports memory/heap_ptr/rt_alloc from it.
    let runtime_bytes = aver::codegen::wasm::build_runtime_wasm()
        .map_err(|e| format!("Runtime build error: {e}"))?;
    let runtime_module =
        Module::new(&engine, &runtime_bytes).map_err(|e| format!("Runtime module error: {e:#}"))?;

    let module = Module::new(&engine, wasm_bytes).map_err(|e| format!("Module error: {e:#}"))?;
    let mut store = Store::new(&engine, ());
    let mut linker = Linker::new(&engine);

    let runtime_instance = linker
        .instantiate(&mut store, &runtime_module)
        .map_err(|e| format!("Runtime instantiation error: {e:#}"))?;
    linker
        .instance(&mut store, "aver_runtime", runtime_instance)
        .map_err(|e| format!("Runtime link error: {e:#}"))?;

    // Wire aver/* capabilities to native Rust implementations

    // aver/console_print(ptr: i32, len: i32)
    linker
        .func_wrap("aver", "args_len", {
            let program_args = program_args.to_vec();
            move || -> i32 { program_args.len() as i32 }
        })
        .map_err(|e| format!("Link error: {}", e))?;

    linker
        .func_wrap("aver", "args_get", {
            let program_args = program_args.to_vec();
            move |mut caller: Caller<'_, ()>, index: i32| -> (i32, i32) {
                let arg = program_args
                    .get(index.max(0) as usize)
                    .map(|s| s.as_str())
                    .unwrap_or("");
                wasm_write_guest_string(&mut caller, arg)
            }
        })
        .map_err(|e| format!("Link error: {}", e))?;

    linker
        .func_wrap(
            "aver",
            "console_print",
            |mut caller: Caller<'_, ()>, ptr: i32, len: i32| {
                use std::io::Write;
                let data = wasm_guest_bytes(&mut caller, ptr, len);
                std::io::stdout().write_all(&data).unwrap();
            },
        )
        .map_err(|e| format!("Link error: {}", e))?;

    // aver/console_error(ptr: i32, len: i32)
    linker
        .func_wrap(
            "aver",
            "console_error",
            |mut caller: Caller<'_, ()>, ptr: i32, len: i32| {
                use std::io::Write;
                let data = wasm_guest_bytes(&mut caller, ptr, len);
                std::io::stderr().write_all(&data).unwrap();
            },
        )
        .map_err(|e| format!("Link error: {}", e))?;

    // aver/random_int(min: i64, max: i64) -> i64
    linker
        .func_wrap("aver", "random_int", |min: i64, max: i64| -> i64 {
            use std::collections::hash_map::RandomState;
            use std::hash::{BuildHasher, Hasher};
            // Simple random using HashMap hasher (no extra dependency)
            let s = RandomState::new();
            let mut h = s.build_hasher();
            h.write_u64(min as u64 ^ max as u64);
            let range = (max - min + 1) as u64;
            if range == 0 {
                return min;
            }
            min + (h.finish() % range) as i64
        })
        .map_err(|e| format!("Link error: {}", e))?;

    // aver/random_float() -> f64 in [0.0, 1.0).
    // The WASI bridge does this via `random_get`; this `aver run --wasm`
    // path doesn't go through the bridge yet (TODO 0.15: use
    // wasmtime-wasi + bridge instead of one func_wrap per effect), so
    // we duplicate the impl in native Rust.
    linker
        .func_wrap("aver", "random_float", || -> f64 {
            use std::collections::hash_map::RandomState;
            use std::hash::{BuildHasher, Hasher};
            let s = RandomState::new();
            let mut h = s.build_hasher();
            h.write_u8(0xA5);
            // Take entropy as 53-bit mantissa, divide by 2^53 → [0.0, 1.0).
            let bits = h.finish() >> 11;
            (bits as f64) / ((1u64 << 53) as f64)
        })
        .map_err(|e| format!("Link error: {}", e))?;

    // aver/time_now() -> (i32, i32)  — returns ISO timestamp string in WASM memory
    linker
        .func_wrap(
            "aver",
            "time_now",
            |mut caller: Caller<'_, ()>| -> (i32, i32) {
                use std::time::{SystemTime, UNIX_EPOCH};
                let millis = SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .unwrap_or_default()
                    .as_millis() as u64;
                let secs = millis / 1000;
                let ms = millis % 1000;
                // Simple ISO-8601 formatting from unix timestamp
                let days = secs / 86400;
                let time_of_day = secs % 86400;
                let hours = time_of_day / 3600;
                let minutes = (time_of_day % 3600) / 60;
                let seconds = time_of_day % 60;
                // Days since epoch to Y-M-D (simplified)
                let mut y = 1970i64;
                let mut d = days as i64;
                loop {
                    let days_in_year = if y % 4 == 0 && (y % 100 != 0 || y % 400 == 0) {
                        366
                    } else {
                        365
                    };
                    if d < days_in_year {
                        break;
                    }
                    d -= days_in_year;
                    y += 1;
                }
                let leap = y % 4 == 0 && (y % 100 != 0 || y % 400 == 0);
                let month_days = [
                    31,
                    if leap { 29 } else { 28 },
                    31,
                    30,
                    31,
                    30,
                    31,
                    31,
                    30,
                    31,
                    30,
                    31,
                ];
                let mut m = 0usize;
                while m < 12 && d >= month_days[m] {
                    d -= month_days[m];
                    m += 1;
                }
                let now = format!(
                    "{:04}-{:02}-{:02}T{:02}:{:02}:{:02}.{:03}Z",
                    y,
                    m + 1,
                    d + 1,
                    hours,
                    minutes,
                    seconds,
                    ms
                );
                wasm_write_guest_string(&mut caller, &now)
            },
        )
        .map_err(|e| format!("Link error: {}", e))?;

    // aver/time_unixMs() -> i64
    linker
        .func_wrap("aver", "time_unixMs", || -> i64 {
            use std::time::{SystemTime, UNIX_EPOCH};
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap_or_default()
                .as_millis() as i64
        })
        .map_err(|e| format!("Link error: {}", e))?;

    // aver/time_sleep(millis: i64)
    linker
        .func_wrap("aver", "time_sleep", |millis: i64| {
            std::thread::sleep(std::time::Duration::from_millis(millis as u64));
        })
        .map_err(|e| format!("Link error: {}", e))?;

    // aver/print_value(tag: i32, val: i64) — format and print any value
    // tag: 0=Int, 1=Float(bits), 2=Bool, 3=String(ptr), 4=Heap(ptr), 5=Unit
    linker
        .func_wrap(
            "aver",
            "print_value",
            |mut caller: Caller<'_, ()>, tag: i32, val: i64| {
                let mem = caller.get_export("memory").unwrap().into_memory().unwrap();
                let formatted = format_tagged_value(tag, val, mem.data(&caller));
                use std::io::Write;
                std::io::stdout().write_all(formatted.as_bytes()).unwrap();
            },
        )
        .map_err(|e| format!("Link error: {}", e))?;

    // aver/format_value(tag: i32, val: i64) -> (i32, i32) — format to string in memory
    linker
        .func_wrap(
            "aver",
            "format_value",
            |mut caller: Caller<'_, ()>, tag: i32, val: i64| -> (i32, i32) {
                let mem = caller.get_export("memory").unwrap().into_memory().unwrap();
                let formatted = format_tagged_value(tag, val, mem.data(&caller));
                wasm_write_guest_string(&mut caller, &formatted)
            },
        )
        .map_err(|e| format!("Link error: {}", e))?;

    // Math (no native WASM ops)
    linker
        .func_wrap("aver", "math_sin", |x: f64| -> f64 { x.sin() })
        .map_err(|e| format!("Link error: {}", e))?;
    linker
        .func_wrap("aver", "math_cos", |x: f64| -> f64 { x.cos() })
        .map_err(|e| format!("Link error: {}", e))?;
    linker
        .func_wrap("aver", "math_atan2", |y: f64, x: f64| -> f64 { y.atan2(x) })
        .map_err(|e| format!("Link error: {}", e))?;
    linker
        .func_wrap("aver", "math_pow", |base: f64, exp: f64| -> f64 {
            base.powf(exp)
        })
        .map_err(|e| format!("Link error: {}", e))?;

    // aver/console_readLine() -> (i32, i32)
    // Reads a line from stdin, allocates in WASM memory, returns (ptr, len)
    linker
        .func_wrap(
            "aver",
            "console_readLine",
            |mut caller: Caller<'_, ()>| -> (i32, i32) {
                let mut input = String::new();
                std::io::stdin().read_line(&mut input).unwrap_or(0);
                let trimmed = input.trim_end_matches('\n').trim_end_matches('\r');
                wasm_write_guest_string(&mut caller, trimmed)
            },
        )
        .map_err(|e| format!("Link error: {}", e))?;

    #[cfg(feature = "terminal")]
    linker
        .func_wrap("aver", "terminal_enableRawMode", || {
            aver_rt::terminal_enable_raw_mode().unwrap();
        })
        .map_err(|e| format!("Link error: {}", e))?;

    #[cfg(feature = "terminal")]
    linker
        .func_wrap("aver", "terminal_disableRawMode", || {
            aver_rt::terminal_disable_raw_mode().unwrap();
        })
        .map_err(|e| format!("Link error: {}", e))?;

    #[cfg(feature = "terminal")]
    linker
        .func_wrap("aver", "terminal_clear", || {
            aver_rt::terminal_clear().unwrap();
        })
        .map_err(|e| format!("Link error: {}", e))?;

    #[cfg(feature = "terminal")]
    linker
        .func_wrap("aver", "terminal_moveTo", |x: i32, y: i32| {
            aver_rt::terminal_move_to(x as i64, y as i64).unwrap();
        })
        .map_err(|e| format!("Link error: {}", e))?;

    #[cfg(feature = "terminal")]
    linker
        .func_wrap(
            "aver",
            "terminal_print",
            |mut caller: Caller<'_, ()>, ptr: i32, len: i32| {
                let text = wasm_guest_string(&mut caller, ptr, len);
                aver_rt::terminal_print(&text).unwrap();
            },
        )
        .map_err(|e| format!("Link error: {}", e))?;

    #[cfg(feature = "terminal")]
    linker
        .func_wrap(
            "aver",
            "terminal_setColor",
            |mut caller: Caller<'_, ()>, ptr: i32, len: i32| {
                let color = wasm_guest_string(&mut caller, ptr, len);
                aver_rt::terminal_set_color(&color).unwrap();
            },
        )
        .map_err(|e| format!("Link error: {}", e))?;

    #[cfg(feature = "terminal")]
    linker
        .func_wrap("aver", "terminal_resetColor", || {
            aver_rt::terminal_reset_color().unwrap();
        })
        .map_err(|e| format!("Link error: {}", e))?;

    #[cfg(feature = "terminal")]
    linker
        .func_wrap(
            "aver",
            "terminal_readKey",
            |mut caller: Caller<'_, ()>| -> (i32, i32) {
                match aver_rt::terminal_read_key() {
                    Some(key) => wasm_write_guest_string(&mut caller, &key),
                    None => (-1, 0),
                }
            },
        )
        .map_err(|e| format!("Link error: {}", e))?;

    #[cfg(feature = "terminal")]
    linker
        .func_wrap("aver", "terminal_size", || -> (i32, i32) {
            let (width, height) = aver_rt::terminal_size().unwrap();
            (width as i32, height as i32)
        })
        .map_err(|e| format!("Link error: {}", e))?;

    #[cfg(feature = "terminal")]
    linker
        .func_wrap("aver", "terminal_hideCursor", || {
            aver_rt::terminal_hide_cursor().unwrap();
        })
        .map_err(|e| format!("Link error: {}", e))?;

    #[cfg(feature = "terminal")]
    linker
        .func_wrap("aver", "terminal_showCursor", || {
            aver_rt::terminal_show_cursor().unwrap();
        })
        .map_err(|e| format!("Link error: {}", e))?;

    #[cfg(feature = "terminal")]
    linker
        .func_wrap("aver", "terminal_flush", || {
            aver_rt::terminal_flush().unwrap();
        })
        .map_err(|e| format!("Link error: {}", e))?;

    let instance = linker
        .instantiate(&mut store, &module)
        .map_err(|e| format!("Instantiation error: {e:#}"))?;

    // Load variant name table from globals before execution starts.
    load_variant_names_from_instance(&instance, &mut store);

    // Try _start — check its actual return type and provide matching results buffer
    if let Some(start) = instance.get_func(&mut store, "_start") {
        let ty = start.ty(&store);
        let num_results = ty.results().len();
        let mut results: Vec<Val> = (0..num_results).map(|_| Val::I32(0)).collect();
        start
            .call(&mut store, &[], &mut results)
            .map_err(|e| format!("Execution error: {e:#}"))?;
    }

    Ok(())
}

pub(super) fn cmd_run_self_hosted(
    file: &str,
    module_root_override: Option<&str>,
    run_verify_blocks: bool,
    record_dir: Option<&str>,
    program_args: Vec<String>,
) {
    if run_verify_blocks && record_dir.is_some() {
        eprintln!(
            "{}",
            "Cannot combine --verify and --record in one run; record should capture only main flow."
                .red()
        );
        process::exit(1);
    }

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
        tco::transform_program(&mut items);
        let tc = run_type_check_full(&items, Some(&mr));
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

    if run_verify_blocks {
        println!();
        cmd_verify(file, module_root_override, false, false, false, false);
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
        let blocks = aver::diagnostics::vm_verify::run_verify_for_items_vm_with_mode(
            items,
            config.clone(),
            Some(module_root),
            &path,
            mode,
        )?;
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
        match run_verify_for_file(file, &module_root, deps, hostile) {
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
            Err(_e) => {
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

    // TCO transform
    tco::transform_program(&mut items);

    // Static type check (runs before resolution — works on Ident nodes)
    let tc_result = run_type_check_full(&items, Some(&module_root));
    if !tc_result.errors.is_empty() {
        print_type_errors(&tc_result.errors);
        process::exit(1);
    }

    // Resolve locals + annotate last-use (unified across all backends)
    resolver::resolve_program(&mut items);

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

    // Build codegen context
    let mut ctx = codegen::build_context(items, &tc_result, memo_fns, name, modules);
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
        bridge,
        pack,
        handler,
        optimize,
    } = opts;

    // WASM-side targets (wasm / wat / wasm+wat): simplified pipeline,
    // no replay/policy/guest-entry support yet
    if target.needs_wasm_pipeline() {
        cmd_compile_wasm(
            file,
            output_dir,
            project_name,
            module_root_override,
            bridge,
            pack,
            handler,
            optimize,
            target,
        );
        return;
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

#[allow(clippy::too_many_arguments)]
fn cmd_compile_wasm(
    file: &str,
    output_dir: &str,
    project_name: Option<&str>,
    module_root_override: Option<&str>,
    bridge: Option<super::cli::WasmBridge>,
    pack: Option<super::cli::DeployPack>,
    handler: Option<&str>,
    optimize: Option<super::cli::WasmOptMode>,
    target: super::cli::CompileTarget,
) {
    #[cfg(not(feature = "wasm"))]
    {
        let _ = (
            file,
            output_dir,
            project_name,
            module_root_override,
            bridge,
            pack,
            handler,
            optimize,
            target,
        );
        eprintln!(
            "{}",
            "WASM target requires --features wasm (rebuild with: cargo build --features wasm)"
                .red()
        );
        process::exit(1);
    }

    #[cfg(feature = "wasm")]
    {
        let (ctx, _module_root) = build_codegen_context(
            file,
            project_name,
            module_root_override,
            false,
            &super::cli::CompilePolicyMode::Embed,
            None,
            false,
        );

        // user.wasm bytes are identical regardless of bridge — the
        // adapter swap happens at bundle time. We still pass the
        // bridge through to emit so it can shape the WASI _start
        // wrapper and any future deployment-side hints.
        let wasm_adapter = match bridge {
            Some(super::cli::WasmBridge::Wasip1) => codegen::wasm::WasmAdapter::Wasi,
            Some(super::cli::WasmBridge::Fetch) => codegen::wasm::WasmAdapter::Fetch,
            _ => codegen::wasm::WasmAdapter::Aver,
        };
        match codegen::wasm::emit_wasm_with_adapter(&ctx, wasm_adapter, handler) {
            Ok(wasm_bytes) => {
                if let Err(err) = validate_wasm_bytes(&wasm_bytes) {
                    eprintln!(
                        "{}",
                        format!("WASM emit produced invalid bytecode: {}", err).red()
                    );
                    process::exit(1);
                }
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
                if let Err(e) = std::fs::write(&wasm_file, &wasm_bytes) {
                    eprintln!("{}", format!("Failed to write WASM file: {}", e).red());
                    process::exit(1);
                }

                // Two output shapes:
                //   - EdgeWasm: thin user.wasm with aver_runtime.* + aver/*
                //     all unresolved. Consumer (CDN-cached runtime, browser
                //     playground) wires every import at instantiate time.
                //   - Wasm: bundled artifact via wasm-merge. aver_runtime
                //     is always merged in. Bridge controls the rest:
                //       --bridge none  → only aver/* unresolved (host
                //                         supplies effects)
                //       --bridge wasip1 → also merge the aver→wasi shim;
                //                         result is a standalone WASI
                //                         binary that runs under
                //                         `wasmtime program.wasm` with
                //                         only wasi_snapshot_preview1 as
                //                         the open import.
                let is_edge = matches!(target, super::cli::CompileTarget::EdgeWasm);
                let bridge_mode = bridge.unwrap_or(super::cli::WasmBridge::None);
                let uses_aver_effects = wasm_imports_module(&wasm_bytes, "aver");
                let uses_wasi = wasm_imports_module(&wasm_bytes, "wasi_snapshot_preview1");

                if is_edge {
                    let file_display = file.cyan();
                    let (final_size, compile_suffix) = finalize_wasm_artifact(&wasm_file, optimize);
                    let wasm_display = wasm_file.display().to_string().cyan();
                    let imports_note = if uses_aver_effects {
                        ", imports aver_runtime.* + aver/* (effects)"
                    } else if uses_wasi {
                        ", imports aver_runtime.* + wasi_snapshot_preview1.*"
                    } else {
                        ", imports aver_runtime.*"
                    };
                    println!(
                        "{} {} → {} ({}{}{})",
                        "Compiled".green().bold(),
                        file_display,
                        wasm_display,
                        format_byte_size(final_size),
                        compile_suffix,
                        imports_note
                    );
                    // Deployment pack: drop platform-specific bootstrap
                    // files next to user.wasm so the build is one
                    // platform-CLI command from running. Pack is
                    // independent of target/bridge — adds files,
                    // doesn't change the .wasm.
                    if let Some(super::cli::DeployPack::Cloudflare) = pack {
                        emit_cloudflare_pack(out_path, &wasm_name, &wasm_file);
                    }
                } else {
                    // Bundle aver_runtime + (optional bridge) + user.
                    let runtime_bytes = match aver::codegen::wasm::build_runtime_wasm() {
                        Ok(b) => b,
                        Err(e) => {
                            eprintln!("{}", format!("Runtime build error: {}", e).red());
                            process::exit(1);
                        }
                    };
                    let runtime_file = out_path.join(format!("{}_runtime.wasm", wasm_name));
                    if let Err(e) = std::fs::write(&runtime_file, &runtime_bytes) {
                        eprintln!(
                            "{}",
                            format!("Failed to write runtime WASM file: {}", e).red()
                        );
                        process::exit(1);
                    }

                    // Optional bridge module (today only `wasi`).
                    let bridge_file = if matches!(bridge_mode, super::cli::WasmBridge::Wasip1) {
                        let bytes = match aver::codegen::wasm::build_aver_to_wasi_wasm() {
                            Ok(b) => b,
                            Err(e) => {
                                eprintln!(
                                    "{}",
                                    format!("aver_to_wasi bridge build error: {}", e).red()
                                );
                                process::exit(1);
                            }
                        };
                        let path = out_path.join(format!("{}_aver_to_wasi.wasm", wasm_name));
                        if let Err(e) = std::fs::write(&path, &bytes) {
                            eprintln!(
                                "{}",
                                format!("Failed to write aver_to_wasi shim: {}", e).red()
                            );
                            process::exit(1);
                        }
                        Some(path)
                    } else {
                        None
                    };

                    let merged_file = out_path.join(format!("{}_merged.wasm", wasm_name));
                    let mut merge = std::process::Command::new("wasm-merge");
                    merge.arg(&runtime_file).arg("aver_runtime");
                    if let Some(bridge_path) = &bridge_file {
                        merge.arg(bridge_path).arg("aver");
                    }
                    merge.arg(&wasm_file).arg("program");
                    merge
                        .arg("--rename-export-conflicts")
                        .arg("--enable-bulk-memory")
                        // Host imports like format_value return (i32, i32);
                        // emitter uses tail calls in TCO trampolines.
                        .arg("--enable-multivalue")
                        .arg("--enable-tail-call")
                        .arg("-o")
                        .arg(&merged_file);
                    let merge_result = merge.output();

                    let _ = std::fs::remove_file(&runtime_file);
                    if let Some(bridge_path) = &bridge_file {
                        let _ = std::fs::remove_file(bridge_path);
                    }

                    match merge_result {
                        Ok(out) if out.status.success() => {
                            let _ = std::fs::rename(&merged_file, &wasm_file);
                            let file_display = file.cyan();
                            let (final_size, compile_suffix) =
                                finalize_wasm_artifact(&wasm_file, optimize);
                            let wasm_display = wasm_file.display().to_string().cyan();
                            let imports_note = match bridge_mode {
                                super::cli::WasmBridge::Wasip1 => {
                                    ", with runtime + aver→wasi bridge"
                                }
                                super::cli::WasmBridge::Fetch => {
                                    ", with runtime, imports aver/* (JS host)"
                                }
                                super::cli::WasmBridge::None => {
                                    if uses_aver_effects {
                                        ", with runtime, imports aver/* (effects)"
                                    } else if uses_wasi {
                                        ", with runtime, imports wasi_snapshot_preview1.*"
                                    } else {
                                        ", with runtime"
                                    }
                                }
                            };
                            println!(
                                "{} {} → {} ({}{}{})",
                                "Compiled".green().bold(),
                                file_display,
                                wasm_display,
                                format_byte_size(final_size),
                                compile_suffix,
                                imports_note
                            );
                            // Deployment pack — drops platform-specific
                            // bootstrap files next to the bundled wasm.
                            // Cloudflare Workers reject runtime-fetched
                            // wasm bytes, so the only viable shape on
                            // CF is `--target wasm` (single bundled),
                            // and the pack lives here too.
                            if let Some(super::cli::DeployPack::Cloudflare) = pack {
                                emit_cloudflare_pack(out_path, &wasm_name, &wasm_file);
                            }
                        }
                        Ok(out) => {
                            let stderr = String::from_utf8_lossy(&out.stderr);
                            eprintln!("{}", format!("wasm-merge failed: {}", stderr.trim()).red());
                            let _ = std::fs::remove_file(&merged_file);
                            process::exit(1);
                        }
                        Err(_) => {
                            eprintln!(
                                "{}",
                                "wasm-merge not found. Install binaryen (`brew install binaryen`) or use --target edge-wasm."
                                    .red()
                            );
                            process::exit(1);
                        }
                    }
                }
            }
            Err(e) => {
                eprintln!("{}", format!("WASM codegen error: {}", e).red());
                process::exit(1);
            }
        }
    }
}

/// True if `bytes` declares any import whose module name is exactly
/// `module`. Used to detect effect imports without false positives from
/// data sections that happen to spell `aver_rt`.
#[cfg(feature = "wasm")]
fn wasm_imports_module(bytes: &[u8], module: &str) -> bool {
    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        if let Ok(wasmparser::Payload::ImportSection(reader)) = payload {
            for import in reader {
                if let Ok(import) = import
                    && import.module == module
                {
                    return true;
                }
            }
        }
    }
    false
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
    if !wrangler_existed
        && let Err(e) = std::fs::write(&wrangler_path, wrangler_toml)
    {
        eprintln!(
            "{}",
            format!("Failed to write {}: {}", wrangler_path.display(), e).red()
        );
        return;
    }

    let wrangler_note = if wrangler_existed {
        " (preserved)"
    } else {
        ""
    };
    println!(
        "{} {} + {}{} ({})",
        "  Pack".green().bold(),
        worker_path.display().to_string().cyan(),
        wrangler_path.display().to_string().cyan(),
        wrangler_note.dimmed(),
        format!("Cloudflare Workers, paired with {}", wasm_file.display()).dimmed()
    );
}

/// Validate WASM bytes structurally before they reach disk or wasmtime.
/// Catches emit-time bugs like Map<Int,V>'s `expected i32, found i64`
/// where the type checker accepted the program but codegen produced
/// invalid bytecode.
#[cfg(feature = "wasm")]
fn validate_wasm_bytes(bytes: &[u8]) -> Result<(), String> {
    let mut validator = wasmparser::Validator::new();
    validator
        .validate_all(bytes)
        .map(|_| ())
        .map_err(|e| e.to_string())
}

/// Emit the standalone aver_runtime / aver_to_wasi artifact. Internal
/// release tooling — used by `tools/release/build_runtime_artifacts.py`
/// to publish per-version runtime modules to averlang.dev.
#[cfg(feature = "wasm")]
pub fn cmd_wasm_runtime(
    output: &str,
    artifact: super::cli::WasmRuntimeArtifact,
    optimize: Option<super::cli::WasmOptMode>,
    wat: bool,
) {
    let bytes = match artifact {
        super::cli::WasmRuntimeArtifact::Runtime => aver::codegen::wasm::build_runtime_wasm(),
        super::cli::WasmRuntimeArtifact::WasiBridge => {
            aver::codegen::wasm::build_aver_to_wasi_wasm()
        }
    };
    let bytes = match bytes {
        Ok(b) => b,
        Err(e) => {
            eprintln!("{}", format!("Runtime build error: {}", e).red());
            process::exit(1);
        }
    };
    if let Err(e) = validate_wasm_bytes(&bytes) {
        eprintln!(
            "{}",
            format!("Runtime artifact failed validation: {}", e).red()
        );
        process::exit(1);
    }

    let output_path = Path::new(output);
    if let Some(parent) = output_path.parent()
        && !parent.as_os_str().is_empty()
        && let Err(e) = std::fs::create_dir_all(parent)
    {
        eprintln!(
            "{}",
            format!(
                "Failed to create output directory {}: {}",
                parent.display(),
                e
            )
            .red()
        );
        process::exit(1);
    }
    if let Err(e) = std::fs::write(output_path, &bytes) {
        eprintln!(
            "{}",
            format!("Failed to write {}: {}", output_path.display(), e).red()
        );
        process::exit(1);
    }

    let raw_size = bytes.len() as u64;
    let final_size = if let Some(mode) = optimize {
        match run_optimize_pipeline_library(output_path, mode) {
            Ok(size) => size,
            Err(e) => {
                eprintln!("{}", e.red());
                process::exit(1);
            }
        }
    } else {
        raw_size
    };

    let label = match artifact {
        super::cli::WasmRuntimeArtifact::Runtime => "aver_runtime",
        super::cli::WasmRuntimeArtifact::WasiBridge => "aver_to_wasi",
    };
    let opt_note = match optimize {
        Some(super::cli::WasmOptMode::Oz) => " (optimized for size)",
        Some(super::cli::WasmOptMode::O3) => " (optimized for speed)",
        None => " (raw)",
    };
    println!(
        "{} {} → {} ({}{})",
        "Built".green().bold(),
        label,
        output_path.display().to_string().cyan(),
        format_byte_size(final_size),
        opt_note
    );

    if wat {
        let wat_path = output_path.with_extension("wat");
        let result = std::process::Command::new("wasm-tools")
            .arg("print")
            .arg(output_path)
            .output();
        match result {
            Ok(out) if out.status.success() => {
                if let Err(e) = std::fs::write(&wat_path, &out.stdout) {
                    eprintln!(
                        "{}",
                        format!(
                            "Failed to write WAT companion {}: {}",
                            wat_path.display(),
                            e
                        )
                        .red()
                    );
                    process::exit(1);
                }
                println!(
                    "        WAT companion → {}",
                    wat_path.display().to_string().cyan()
                );
            }
            Ok(out) => {
                let stderr = String::from_utf8_lossy(&out.stderr);
                eprintln!(
                    "{}",
                    format!("wasm-tools print failed: {}", stderr.trim()).red()
                );
                process::exit(1);
            }
            Err(_) => {
                eprintln!(
                    "{}",
                    "wasm-tools not found on PATH — install wasm-tools or omit --wat.".red()
                );
                process::exit(1);
            }
        }
    }
}

#[cfg(not(feature = "wasm"))]
pub fn cmd_wasm_runtime(
    _output: &str,
    _artifact: super::cli::WasmRuntimeArtifact,
    _optimize: Option<super::cli::WasmOptMode>,
    _wat: bool,
) {
    eprintln!(
        "{}",
        "`aver wasm-runtime` requires building aver with `--features wasm`.".red()
    );
    process::exit(1);
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

#[cfg(feature = "wasm")]
fn run_optimize_pipeline(wasm_file: &Path, mode: super::cli::WasmOptMode) -> Result<u64, String> {
    run_optimize_pipeline_inner(wasm_file, mode, /* library = */ false)
}

/// Library-mode optimize: skip wasm-metadce (every export is a real
/// public API root, not an artifact of bundling), apply only
/// `-Oz --converge --strip-*`. Used by `aver wasm-runtime --optimize`.
#[cfg(feature = "wasm")]
fn run_optimize_pipeline_library(
    wasm_file: &Path,
    mode: super::cli::WasmOptMode,
) -> Result<u64, String> {
    run_optimize_pipeline_inner(wasm_file, mode, /* library = */ true)
}

#[cfg(feature = "wasm")]
fn run_optimize_pipeline_inner(
    wasm_file: &Path,
    mode: super::cli::WasmOptMode,
    library_mode: bool,
) -> Result<u64, String> {
    let input_size = std::fs::metadata(wasm_file)
        .map(|meta| meta.len())
        .map_err(|e| format!("Failed to stat {}: {}", wasm_file.display(), e))?;
    let stage1_file = wasm_file.with_extension("dce.wasm");
    let metadce_graph = wasm_file.with_extension("metadce.json");
    let optimized_file = wasm_file.with_extension("opt.wasm");
    let opt_flag = match mode {
        super::cli::WasmOptMode::O3 => "-O3",
        super::cli::WasmOptMode::Oz => "-Oz",
    };

    // Stage 1: meta-DCE on the cross-module reachability graph. After
    // wasm-merge bundles aver_runtime + (optional bridge) + user, every
    // runtime function (~70) is still exported — `-Oz` treats exports
    // as DCE roots, so they survive even though bundled artifacts have
    // no external caller. wasm-metadce takes a JSON describing what
    // *outside* the module reaches, marks `_start`/`main` as the only
    // entry points, and prunes everything else (including exports).
    // No-op for `--target edge-wasm` artifacts (they already export
    // only `_start`/`main`). We ship a minimal graph and let metadce
    // discover the internal call tree.
    //
    // Library mode (`aver wasm-runtime`): every export is a real public
    // API consumed by some external user.wasm — we cannot prune any of
    // them. Skip the metadce stage entirely; copy input → stage1.
    if library_mode {
        std::fs::copy(wasm_file, &stage1_file)
            .map_err(|e| format!("Failed to stage runtime artifact for opt: {}", e))?;
    } else {
        // The `memory` export is reachable from outside even though
        // _start / main don't reference it directly: WASI host calls
        // (fd_write, random_get, clock_time_get, …) read and write
        // through it. Without this root, metadce strips the export
        // and wasmtime rejects the module with "missing required
        // memory export".
        let graph_json = "[\n  { \"name\": \"outside\", \"root\": true, \"reaches\": [\"main_export\", \"start_export\", \"memory_export\"] },\n  { \"name\": \"main_export\", \"export\": \"main\" },\n  { \"name\": \"start_export\", \"export\": \"_start\" },\n  { \"name\": \"memory_export\", \"export\": \"memory\" }\n]\n";
        if let Err(e) = std::fs::write(&metadce_graph, graph_json) {
            return Err(format!(
                "Failed to write wasm-metadce graph for {}: {}",
                wasm_file.display(),
                e
            ));
        }
        let dce_output = std::process::Command::new("wasm-metadce")
            .arg(format!("--graph-file={}", metadce_graph.display()))
            .arg("--enable-bulk-memory")
            .arg("--enable-multivalue")
            .arg("--enable-tail-call")
            .arg(wasm_file)
            .arg("-o")
            .arg(&stage1_file)
            .output()
            .map_err(|e| {
                let _ = std::fs::remove_file(&metadce_graph);
                format!(
                    "Failed to run wasm-metadce for {}: {}. Install binaryen or compile without --optimize.",
                    wasm_file.display(),
                    e
                )
            })?;
        let _ = std::fs::remove_file(&metadce_graph);

        if !dce_output.status.success() {
            let stderr = String::from_utf8_lossy(&dce_output.stderr);
            let _ = std::fs::remove_file(&stage1_file);
            return Err(format!(
                "wasm-metadce failed for {}: {}",
                wasm_file.display(),
                stderr.trim()
            ));
        }
    }

    // Stage 2: aggressive optimization with --converge (run passes to
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
    pub(super) bridge: Option<super::cli::WasmBridge>,
    pub(super) pack: Option<super::cli::DeployPack>,
    pub(super) handler: Option<&'a str>,
    pub(super) optimize: Option<super::cli::WasmOptMode>,
}

pub(super) fn cmd_proof(
    file: &str,
    output_dir: &str,
    project_name: Option<&str>,
    module_root_override: Option<&str>,
    backend: &super::cli::ProofBackend,
    verify_mode: &super::cli::ProofVerifyMode,
) {
    let (ctx, _module_root) = build_codegen_context(
        file,
        project_name,
        module_root_override,
        false,
        &super::cli::CompilePolicyMode::Embed,
        None,
        false,
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
            cmd_proof_lean(file, output_dir, &ctx, verify_mode);
        }
        super::cli::ProofBackend::Dafny => {
            cmd_proof_dafny(file, output_dir, &ctx);
        }
    }
}

fn cmd_proof_lean(
    file: &str,
    output_dir: &str,
    ctx: &codegen::CodegenContext,
    verify_mode: &super::cli::ProofVerifyMode,
) {
    let proof_issues = lean_codegen::proof_mode_findings(ctx);
    for issue in proof_issues {
        eprintln!(
            "{}",
            format!("warning[{}:1]: {}", issue.line, issue.message).yellow()
        );
    }
    let missing_helper_hints = collect_missing_helper_law_hints(&ctx.items, &ctx.fn_sigs);
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
    let contextual_helper_hints = collect_contextual_helper_law_hints(&ctx.items, &ctx.fn_sigs);
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
    resolver::resolve_program(&mut items);

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
        depends,
        type_defs,
        fn_defs,
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
            fn_sigs: HashMap::new(),
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

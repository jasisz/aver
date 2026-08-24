use std::collections::{BTreeMap, HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::process;
use std::time::{SystemTime, UNIX_EPOCH};

use colored::Colorize;

use aver::ast::{
    CapabilityItem, Expr, FnDef, Pattern, Spanned, Stmt, TopLevel, TypeDef, VerifyKind,
};
use aver::checker::{CheckFinding, VerifyResult, index_decisions};
use aver::codegen;
use aver::codegen::ModuleInfo;
use aver::codegen::lean as lean_codegen;
use aver::codegen::rust as rust_codegen;
use aver::nan_value::{Arena, NanValueConvert};
use aver::source::{LoadError, LoadMode, require_module_declaration, resolve_module_source};
use aver::types::{Type, parse_type_str};
use aver::verify_law::{
    collect_contextual_helper_law_hints, collect_missing_helper_law_hints,
    contextual_helper_law_message, missing_helper_law_message,
};
use aver::vm;

use super::diagnostic;
use aver::tty_render::render_tty;

use crate::cli_entry::shared::{
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

/// One module of a report: its path as the loader spelled it, its source,
/// and its parsed items (empty when the file failed to parse).
pub(super) type ReportUnit = (String, String, Vec<TopLevel>);

/// The modules `check`, `verify` and `audit` report for the program named
/// by `file`:
/// every project module reachable through `depends [...]`, leaves-first,
/// the entry last. Embedded standard modules are not units. Modules whose
/// canonical path is already in `reported` are skipped and the rest are
/// added to it, so a directory input reports each module once.
pub(super) fn collect_program_units(
    file: &str,
    module_root: &str,
    reported: &mut HashSet<PathBuf>,
) -> Result<Vec<ReportUnit>, String> {
    let mut cache = aver::source::ProgramLoadCache::default();
    collect_program_units_with_cache(file, module_root, reported, &mut cache)
}

fn collect_program_units_with_cache(
    file: &str,
    module_root: &str,
    reported: &mut HashSet<PathBuf>,
    cache: &mut aver::source::ProgramLoadCache,
) -> Result<Vec<ReportUnit>, String> {
    let program = load_report_program_with_cache(file, module_root, cache)?;
    Ok(program
        .report_units()
        .filter(|module| reported.insert(aver::source::canonicalize_path(&module.path)))
        .map(|module| {
            let path = if module.is_entry {
                file.to_string()
            } else {
                module.path.to_string_lossy().to_string()
            };
            (path, module.source.clone(), module.items.clone())
        })
        .collect())
}

pub(super) fn load_report_program_with_cache(
    file: &str,
    module_root: &str,
    cache: &mut aver::source::ProgramLoadCache,
) -> Result<aver::source::Program, String> {
    let source = read_file(file)?;
    // Parse failure shouldn't abort `check`: its analysis pass owns the
    // canonical line/column diagnostic. `verify` re-parses an empty item set
    // and still fails loudly, preserving its existing behavior.
    //
    // The entry file is one of the user's own project files, so it parses
    // under the ceiling the project declared for it; `load_program` below
    // reads the same `aver.toml` for every dependency it walks.
    let items = aver::source::parse_project_source(&source, module_root, file).unwrap_or_default();

    // The tolerant walk keeps a dependency that fails to parse or lacks its
    // declaration as a unit of its own, so each file reports its diagnostics
    // in place; only an unresolvable dependency stops the walk.
    aver::source::load_program_with_cache(
        Path::new(file),
        &source,
        &items,
        module_root,
        LoadMode::Tolerant,
        cache,
    )
    .map_err(|error| match error {
        LoadError::Missing {
            name,
            root,
            required_by,
        } => format!(
            "Module '{}' not found in '{}' (required by '{}')",
            name,
            root,
            required_by.unwrap_or_default().display()
        ),
        other => other.to_string(),
    })
}

/// Canonical key of the file a diagnostic span names, resolved against the
/// module root when the span carries a relative path.
fn span_file_key(file: &str, module_root: &str) -> String {
    if file.is_empty() {
        return String::new();
    }
    let path = Path::new(file);
    if path.is_absolute() {
        canonical_path_key(file)
    } else {
        canonical_path_key(&Path::new(module_root).join(path).to_string_lossy())
    }
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
            // An operation's parameter and return annotations name real
            // types, and those types can come from a dependency. An
            // ignore arm here would report a genuinely used expose as
            // unused.
            TopLevel::Capability(cap) => {
                if let CapabilityItem::Operation(op) = cap {
                    for (_, type_name) in &op.params {
                        mark_type_annotation(
                            type_name,
                            dep_targets,
                            &unique_type_owner,
                            &mut used_by_target,
                        );
                    }
                    mark_type_annotation(
                        &op.return_type,
                        dep_targets,
                        &unique_type_owner,
                        &mut used_by_target,
                    );
                }
            }
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

/// Exposed names nobody imports, judged over `units` as one program: a
/// name counts as used when any unit imports it. A module no unit imports
/// (an entry, or a leaf pointed at directly) is not judged, since its
/// importers are not in view. The finding names that scope: a sibling
/// program outside the checked inputs is not consulted.
fn collect_unused_exposes_findings(units: &[&ReportUnit], module_root: &str) -> Vec<CheckFinding> {
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
            },
        );
    }

    let mut used_by_target: HashMap<String, HashSet<String>> = HashMap::new();
    let mut imported: HashSet<String> = HashSet::new();

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
                let resolved = resolve_module_source(dep, module_root).ok().flatten()?;
                let dep_key = canonical_path_key(&resolved.path.to_string_lossy());
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
        imported.extend(
            dep_targets
                .iter()
                .map(|target| target.info.canonical_path.clone()),
        );

        let importer_usage = collect_used_exposes_for_importer(items, &dep_targets);
        for (target_path, names) in importer_usage {
            used_by_target.entry(target_path).or_default().extend(names);
        }
    }

    let mut findings = Vec::new();
    let mut modules = module_info_by_path.into_values().collect::<Vec<_>>();
    modules.sort_by(|left, right| left.file.cmp(&right.file));

    for info in modules {
        if !imported.contains(&info.canonical_path) {
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
            message: format!(
                "exposes not used by the checked program(s): {}",
                unused.join(", ")
            ),
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

/// The path key `[[check.suppress]]` globs are matched against: the file's
/// location relative to the module root, with no leading `./`.
///
/// Deliberately not `display_check_path`: that one keeps whatever spelling
/// the command line used, which is what the user should read back, but it
/// makes the same file match a waiver under one spelling and miss it under
/// another. Suppression must not depend on how the path was typed.
pub(super) fn suppression_path(path: &str, module_root: &str) -> String {
    let p = Path::new(path);
    let root = Path::new(module_root);

    if let Some(rel) = relativize_to_canonical(root, p).or_else(|| relativize_to(root, p)) {
        return rel;
    }

    // The file may not exist on disk (or may sit outside the module root),
    // so fall back to a purely lexical cleanup.
    let mut lexical = PathBuf::new();
    for component in p.components() {
        if matches!(component, std::path::Component::CurDir) {
            continue;
        }
        lexical.push(component.as_os_str());
    }
    if lexical.as_os_str().is_empty() {
        path.to_string()
    } else {
        path_to_string(&lexical)
    }
}

/// Per-rule bookkeeping for `[[check.suppress]]`, so a run can tell the
/// user which waivers did nothing.
struct SuppressionTracker {
    /// Some checked file fell inside the rule's file globs.
    covered: Vec<bool>,
    /// The rule actually removed at least one diagnostic.
    fired: Vec<bool>,
}

impl SuppressionTracker {
    fn new(config: Option<&aver::config::ProjectConfig>) -> Self {
        let len = config.map_or(0, |cfg| cfg.check_suppressions.len());
        SuppressionTracker {
            covered: vec![false; len],
            fired: vec![false; len],
        }
    }

    fn note_file(&mut self, config: Option<&aver::config::ProjectConfig>, key: &str) {
        let Some(cfg) = config else {
            return;
        };
        for (idx, covered) in self.covered.iter_mut().enumerate() {
            *covered |= cfg.suppression_covers_file(idx, key);
        }
    }
}

/// Drop the warnings waived by `[[check.suppress]]` and return how many went.
/// Shared by `aver check` and `aver audit` so the two commands cannot drift.
fn apply_check_suppressions(
    diagnostics: &mut Vec<diagnostic::Diagnostic>,
    config: Option<&aver::config::ProjectConfig>,
    key: &str,
    tracker: &mut SuppressionTracker,
) -> usize {
    let Some(cfg) = config else {
        return 0;
    };
    let before = diagnostics.len();
    diagnostics.retain(|diag| {
        // Warnings only. Keying on anything wider would let a waiver hide an
        // error or a verify failure, and with it the command's exit code.
        if !diag.is_warning() {
            return true;
        }
        // Every matching rule is credited, not just the first, so an
        // overlapping waiver is never reported as dead.
        let mut suppressed = false;
        for idx in 0..cfg.check_suppressions.len() {
            if cfg.check_suppression_applies_to(idx, diag.slug, key, diag.fn_name.as_deref()) {
                suppressed = true;
                if let Some(fired) = tracker.fired.get_mut(idx) {
                    *fired = true;
                }
            }
        }
        !suppressed
    });
    before - diagnostics.len()
}

/// Report `[[check.suppress]]` rules that removed nothing. Only meaningful
/// when the run walked a whole directory: checking a single file legitimately
/// leaves waivers for other paths untouched.
///
/// Written to stderr so `--json` stdout stays a clean stream, and never
/// changes the exit code.
fn report_dead_suppressions(
    config: Option<&aver::config::ProjectConfig>,
    tracker: &SuppressionTracker,
    whole_tree: bool,
) {
    if !whole_tree {
        return;
    }
    let Some(cfg) = config else {
        return;
    };
    for (idx, rule) in cfg.check_suppressions.iter().enumerate() {
        if tracker.fired.get(idx).copied().unwrap_or(false) {
            continue;
        }
        let scope = if rule.files.is_empty() {
            "every file".to_string()
        } else {
            rule.files.join(", ")
        };
        let detail = if tracker.covered.get(idx).copied().unwrap_or(false) {
            "matched files in this run but suppressed nothing — the warning it waives no longer fires"
        } else {
            "matched no checked file — the path may be stale"
        };
        let fn_scope = rule
            .fn_name
            .as_deref()
            .map_or(String::new(), |name| format!(", fn: {name}"));
        eprintln!(
            "{} aver.toml [[check.suppress]] slug = \"{}\" (files: {}{}) {}",
            "warning:".yellow(),
            rule.slug,
            scope,
            fn_scope,
            detail
        );
    }
}

pub(super) fn cmd_run_vm(
    file: &str,
    module_root_override: Option<&str>,
    record_dir: Option<&str>,
    program_args: Vec<String>,
    profile: bool,
    entry_expression: Option<&str>,
    provider_bindings: &[aver::provider::ProviderBinding],
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
    let mut items = match super::shared::parse_file(&source, &module_root, file) {
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
    //
    // `run_buffer_build: true` mirrors the entry module, which gets the
    // pass from `Default::default()` below. Loading deps without it made
    // `aver run` execute the unfused spelling of the very code
    // `aver compile` deforests — a `String.join` builder living in a
    // dependency was fused for the Rust target and left alone for the
    // VM. The synthesized `<dep>.<fn>__buffered` names have to be in the
    // symbol table this pipeline builds, because the VM compiler
    // re-parses each dep and re-runs the pass against exactly these
    // symbols (`adopt_buffer_build_if_symbols_agree`).
    //
    // `run_interp_lower` stays off here: it is a separate stage with its
    // own history and no reported inconsistency.
    let dep_modules =
        load_compile_deps(&items, &module_root, DepLowering::deforesting(true, false));
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
    let providers = match aver::provider::ProviderRegistry::for_program_with_bindings(
        tc_result.capabilities.clone(),
        provider_bindings.iter().cloned(),
    ) {
        Ok(providers) => std::sync::Arc::new(providers),
        Err(error) => {
            eprintln!("{}", error.red());
            process::exit(1);
        }
    };
    let mut machine = vm::VM::new(code, globals, arena);
    machine.set_provider_registry(providers);
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
            capabilities: machine.provider_registry().provenance(),
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
        let slots = &report.slot_uniqueness;
        eprintln!("\nCollection targets (Map.set and Vector.set):");
        eprintln!(
            "  owned by the compiler:{} of which not uniquely held at run time:{}",
            slots.owned_grants, slots.owned_grants_without_unique_slot
        );
        eprintln!(
            "  not owned by the compiler, uniquely held at run time:{} still held:{}",
            slots.unique_slot_without_owned_grant, slots.declined_with_slot_still_held
        );
        let owned = &report.runtime_ownership;
        eprintln!("\nMap writes the compiler declined, decided at run time:");
        eprintln!(
            "  taken in place:{} refused, a stack cell holds it:{}",
            owned.grants, owned.refused_stack_holder
        );
        eprintln!(
            "  refused, something off the stack holds it:{} not examined, walk dearer than the copy:{}",
            owned.refused_off_stack_holder, owned.unexamined_walk_too_costly
        );
        let fence = &report.vector_ownership;
        eprintln!("\nVector writes the compiler granted, confirmed at run time:");
        eprintln!(
            "  kept in place:{} revoked, a stack cell holds it:{}",
            fence.grants, fence.refused_stack_holder
        );
        eprintln!(
            "  revoked, something off the stack holds it:{} revoked unexamined, walk dearer than the copy:{}",
            fence.refused_off_stack_holder, fence.unexamined_walk_too_costly
        );
        // The mirror that re-derives every grant from scratch is budgeted, and a
        // spent budget is a smaller claim, not a clean one — so it says so. Only
        // a build with debug assertions runs the mirror at all, which is why a
        // silent line here is the normal case rather than a missing one.
        let unaudited = vm::grants_the_mirror_could_not_afford();
        if unaudited > 0 {
            eprintln!(
                "  taken in place without the full cross-check, its budget spent:{unaudited}"
            );
        }
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
            let error = e.to_string();
            eprintln!("{}", error.red());
            if let Some(repair) =
                super::provider_host_cmd::missing_provider_repair(&error, &module_root, "run", file)
            {
                eprintln!("\n{}", repair);
            }
            process::exit(1);
        }
    }
}

/// Refuse a program the self-host pipeline would run with different
/// semantics than the host.
///
/// The literal smart-constructor discharge is a HOST rule: the host
/// typechecker types `Dep.fromList([1, 2, 3])` as the refined type and
/// the host resolver lowers it to the carrier construction. The
/// Aver-in-Aver resolver (`self_hosted/domain/resolver/calls.av`) has no
/// refinement recognizer — it has no type defs, no dependency-module
/// ASTs and no interval derivation — so it keeps building a guest
/// `Result`. Mirroring the rule there is not the three syntactic
/// predicates the literal-divisor rule needed; it is the whole
/// recognizer. Until that lands, the boundary is a LOUD error: staying
/// silent means the guest returns `Result.Ok(v)` where the host-checked
/// source expects `v`, and the program dies far from the cause (or, in
/// the worst case, does not).
pub(super) fn reject_literal_refinement_discharge(
    items: &[TopLevel],
    module_root: Option<&str>,
) -> Result<(), String> {
    use aver::analysis::literal_refinement::discharge_sites;

    let loaded = module_root
        .and_then(|base| {
            items
                .iter()
                .find_map(|item| match item {
                    TopLevel::Module(m) => Some(m.depends.clone()),
                    _ => None,
                })
                .and_then(|depends| aver::source::load_module_tree(&depends, base).ok())
        })
        .unwrap_or_default();
    let dep_modules: Vec<aver::codegen::ModuleInfo> = loaded
        .iter()
        .map(|m| {
            let (capability_items, capability_semantics) =
                aver::codegen::capability_metadata(&m.items);
            let decl = aver::visibility::module_decl(&m.items);
            aver::codegen::ModuleInfo {
                prefix: m.dep_name.clone(),
                depends: decl.map(|d| d.depends.clone()).unwrap_or_default(),
                exposes: decl.map(|d| d.exposes.clone()).unwrap_or_default(),
                exposes_opaque: decl.map(|d| d.exposes_opaque.clone()).unwrap_or_default(),
                type_defs: m
                    .items
                    .iter()
                    .filter_map(|i| match i {
                        TopLevel::TypeDef(td) => Some(td.clone()),
                        _ => None,
                    })
                    .collect(),
                fn_defs: m
                    .items
                    .iter()
                    .filter_map(|i| match i {
                        TopLevel::FnDef(fd) => Some(fd.clone()),
                        _ => None,
                    })
                    .collect(),
                capability_items,
                capability_semantics,
                verify_blocks: Vec::new(),
                verify_laws: Vec::new(),
                analysis: None,
            }
        })
        .collect();
    // `SymbolTable::build` already derives the refinement table (and the
    // scan resolves callee identities against this same table anyway), so
    // there is nothing left to recompute here.
    let symbols = aver::ir::SymbolTable::build(items, &dep_modules);

    // Entry items resolve under their DECLARED module name (that is what
    // `resolve_program` sets), dep items under the prefix the symbol table
    // indexed them by. Same context as the rewrite, same answers.
    let entry_scope = items.iter().find_map(|item| match item {
        TopLevel::Module(m) => Some(m.name.clone()),
        _ => None,
    });
    let mut sites = discharge_sites(&symbols, entry_scope.as_deref(), items);
    for module in &loaded {
        sites.extend(discharge_sites(
            &symbols,
            Some(&module.dep_name),
            &module.items,
        ));
    }
    if sites.is_empty() {
        return Ok(());
    }
    let listed: Vec<String> = sites
        .iter()
        .map(|(line, callee)| format!("  line {line}: {callee}(…)"))
        .collect();
    Err(format!(
        "The self-host pipeline does not support the literal smart-constructor discharge.\n\
         These calls type as the refined type on the host but would build a Result in the \
         self-hosted interpreter:\n{}\n\
         Pass the list through a binding or a non-literal expression to keep the Result path, \
         or run without --self-host.",
        listed.join("\n")
    ))
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
        let mut items = match parse_file(&source, &mr, file) {
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
                run_chars_fusion: false,
                run_string_index: false,
                run_list_build: false,
                run_resolve: false,
                ..Default::default()
            },
        );
        let tc = pipeline_result.typecheck.expect("typecheck was requested");
        if !tc.errors.is_empty() {
            eprintln!("{}", format_type_errors(&tc.errors).red());
            process::exit(1);
        }
        if let Err(e) = reject_literal_refinement_discharge(&items, Some(&mr)) {
            eprintln!("{}", e.red());
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

/// Check and report `units` in order. `unused_exposes` carries the
/// cross-module findings, keyed by canonical path, judged over every unit
/// the command was pointed at. Returns `(path, has_errors)` per unit.
fn check_units(
    units: &[ReportUnit],
    module_root: &str,
    config: Option<&aver::config::ProjectConfig>,
    verbose: bool,
    json: bool,
    tracker: &mut SuppressionTracker,
    unused_exposes: &HashMap<String, Vec<CheckFinding>>,
) -> Vec<(String, bool)> {
    let mut outcomes = Vec::with_capacity(units.len());
    // A diagnostic belongs to the module whose file it points at. When that
    // module is itself a unit of this program it reports the diagnostic, so
    // another unit (typically the entry, whose typecheck surfaces dependency
    // errors) must not repeat it.
    let unit_keys: std::collections::HashSet<String> = units
        .iter()
        .map(|(path, _, _)| canonical_path_key(path))
        .collect();

    for (idx, (path, source, items)) in units.iter().enumerate() {
        let shown_path = display_check_path(path, module_root);
        let own_key = canonical_path_key(path);
        let suppress_key = suppression_path(path, module_root);
        tracker.note_file(config, &suppress_key);
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
            source_path: Some(path.to_string()),
            stdlib_shadowed: aver::source::collect_stdlib_shadowed(items, module_root),
            ..Default::default()
        };
        let report = diagnostic::analyze_source(source, &opts);
        let mut diagnostics = report.diagnostics;
        diagnostics.retain(|d| {
            let key = span_file_key(&d.span.file, module_root);
            key == own_key || !unit_keys.contains(&key)
        });
        let has_errors = diagnostics.iter().any(|d| d.is_error());

        // --- Multi-file concerns: append unused-expose warnings computed
        //     across the whole check unit (not visible to single-file analyze)
        let unused_exposes_warnings = unused_exposes
            .get(&canonical_path_key(path))
            .map(Vec::as_slice)
            .unwrap_or_default();
        for w in unused_exposes_warnings {
            diagnostics.push(diagnostic::from_check_finding(
                diagnostic::Severity::Warning,
                w,
                source,
                &shown_path,
            ));
        }

        // --- Filter suppressed warnings ---
        let suppressed_count =
            apply_check_suppressions(&mut diagnostics, config, &suppress_key, tracker);

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
            let registry = aver::stdlib::standard_capability_registry();
            let standard_dependencies = aver::stdlib::implicit_stdlib_deps(items)
                .into_iter()
                .filter(|dependency| registry.contract(dependency).is_some())
                .collect::<std::collections::BTreeSet<_>>();
            if !standard_dependencies.is_empty() {
                let required =
                    aver::provider::required_capability_operations(items, &[], &registry);
                let manifest =
                    aver::provider::CapabilityTargetManifest::build(&registry, &required)
                        .expect("standard capability calls belong to the standard registry");
                for capability in standard_dependencies {
                    let contract = registry
                        .contract(&capability)
                        .expect("standard capability contract");
                    let provided = manifest
                        .rows()
                        .iter()
                        .filter(|row| row.capability == capability)
                        .filter_map(|row| match &row.status {
                            aver::provider::TargetBindingStatus::Provided(provider) => {
                                Some(format!("{}:{}", row.target, provider.identity))
                            }
                            aver::provider::TargetBindingStatus::HostBound { .. }
                            | aver::provider::TargetBindingStatus::Unsupported { .. } => None,
                        })
                        .collect::<Vec<_>>()
                        .join(", ");
                    println!(
                        "  capability {}: contract_hash={} | model_hash={} | provided=[{}]",
                        capability, contract.contract_hash, contract.model_hash, provided
                    );
                }
            }
        }

        outcomes.push((path.clone(), has_errors));
    }

    outcomes
}

/// Composite: static check + verify execution + format-check in one
/// pass over every module of the program — the entry plus everything it
/// reaches through `depends [...]`, leaves-first — and, for a directory,
/// over the union of the programs rooted at its files, each module audited
/// once. JSON mode emits one AnalysisReport bundle per module (diagnostics
/// include check issues + verify failures + needs-format), trailing
/// summary aggregates the three axes.
pub(super) fn cmd_audit(
    path: &str,
    module_root_override: Option<&str>,
    json: bool,
    hostile: bool,
    provider_bindings: &[aver::provider::ProviderBinding],
) {
    let module_root = crate::cli_entry::shared::resolve_module_root(module_root_override);
    let config = match aver::config::ProjectConfig::load_from_dir(Path::new(&module_root)) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("{}", e.red());
            process::exit(1);
        }
    };
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

    let batch = Path::new(path).is_dir();
    // Every module of every input's program, each collected once: a module
    // reached from an earlier input is not a unit of a later one.
    let mut reported = HashSet::new();
    let mut load_cache = aver::source::ProgramLoadCache::default();
    let programs = inputs
        .iter()
        .map(|file| {
            (
                file,
                collect_program_units_with_cache(
                    file,
                    &module_root,
                    &mut reported,
                    &mut load_cache,
                ),
            )
        })
        .collect::<Vec<_>>();
    // A diagnostic belongs to the module whose file it points at. When that
    // module is itself audited, the unit whose typecheck surfaced the
    // diagnostic (typically an entry seeing a dependency error) must not
    // repeat it.
    let unit_keys = programs
        .iter()
        .filter_map(|(_, units)| units.as_ref().ok())
        .flatten()
        .map(|(path, _, _)| canonical_path_key(path))
        .collect::<HashSet<_>>();
    let context = AuditContext {
        module_root: &module_root,
        config: config.as_ref(),
        unit_keys: &unit_keys,
        json,
        hostile,
        provider_bindings,
    };

    let mut tracker = SuppressionTracker::new(config.as_ref());
    let mut audited_modules = 0usize;
    let mut totals = AuditTotals::default();
    let mut printed_any = false;

    for (file, units) in programs {
        // An input whose every module was audited under an earlier input
        // has no section of its own.
        if matches!(&units, Ok(units) if units.is_empty()) {
            continue;
        }
        if !json && batch {
            if printed_any {
                println!();
            }
            println!("Input: {}", display_check_path(file, &module_root).cyan());
        }
        printed_any = true;

        let units = match units {
            Ok(units) => units,
            Err(e) => {
                // A program that cannot be loaded fails the audit: none of
                // its modules was checked.
                let shown_path = display_check_path(file, &module_root);
                if json {
                    println!(
                        "{{\"schema_version\":1,\"kind\":\"file-error\",\"file\":{},\"error\":{}}}",
                        aver::diagnostics::json_escape(&shown_path),
                        aver::diagnostics::json_escape(&e)
                    );
                } else {
                    eprintln!("{}: {}", shown_path.red(), e);
                }
                if reported.insert(aver::source::canonicalize_path(Path::new(file))) {
                    audited_modules += 1;
                }
                totals.check_errors += 1;
                continue;
            }
        };
        for (idx, (unit_path, source, _items)) in units.iter().enumerate() {
            if !json && idx > 0 {
                println!();
            }
            totals.add(audit_unit(unit_path, source, &context, &mut tracker));
            audited_modules += 1;
        }
    }

    report_dead_suppressions(config.as_ref(), &tracker, batch);

    if json {
        // The declined count appears only when something was declined, so an
        // audit with nothing to decline emits the bytes it always did.
        let declined_key = if totals.verify_declined > 0 {
            format!(",\"verify_declined\":{}", totals.verify_declined)
        } else {
            String::new()
        };
        println!(
            "{{\"schema_version\":1,\"kind\":\"summary\",\"files\":{},\"modules\":{},\"audit\":{{\"check_errors\":{},\"verify_failures\":{}{},\"format_needed\":{}}}}}",
            inputs.len(),
            audited_modules,
            totals.check_errors,
            totals.verify_failures,
            declined_key,
            totals.format_needed
        );
    } else {
        println!();
        println!("{}", "─".repeat(50).dimmed());
        let declined_part = if totals.verify_declined > 0 {
            format!(" | {} verify not answered", totals.verify_declined)
        } else {
            String::new()
        };
        println!(
            "{} {} {} | {} check errors | {} verify failures{} | {} format",
            "Audit:".bold(),
            audited_modules,
            if audited_modules == 1 {
                "module"
            } else {
                "modules"
            },
            totals.check_errors,
            totals.verify_failures,
            declined_part,
            totals.format_needed
        );
    }

    if totals.any() {
        process::exit(1);
    }
}

/// What every module of an audit shares.
struct AuditContext<'a> {
    module_root: &'a str,
    config: Option<&'a aver::config::ProjectConfig>,
    /// Canonical paths of every audited module, across all inputs.
    unit_keys: &'a HashSet<String>,
    json: bool,
    hostile: bool,
    provider_bindings: &'a [aver::provider::ProviderBinding],
}

/// The three axes of an audit, counted per module and summed per run.
#[derive(Default)]
struct AuditTotals {
    check_errors: usize,
    verify_failures: usize,
    /// Verify cases that ran out of their step budget. Its own count, never
    /// folded into `verify_failures`: a decline is not a counter-example. It
    /// still fails the audit, because a case nobody answered must not leave
    /// the gate green.
    verify_declined: usize,
    format_needed: usize,
}

impl AuditTotals {
    fn add(&mut self, other: AuditTotals) {
        self.check_errors += other.check_errors;
        self.verify_failures += other.verify_failures;
        self.verify_declined += other.verify_declined;
        self.format_needed += other.format_needed;
    }

    fn any(&self) -> bool {
        self.check_errors > 0
            || self.verify_failures > 0
            || self.verify_declined > 0
            || self.format_needed > 0
    }
}

/// Audit one module: its static diagnostics (minus those another audited
/// module owns), its verify run, and its format check, rendered in place.
fn audit_unit(
    path: &str,
    source: &str,
    context: &AuditContext<'_>,
    tracker: &mut SuppressionTracker,
) -> AuditTotals {
    use super::format_cmd::try_format_project_source;
    use aver::diagnostics::{
        AnalyzeOptions, analyze_source_with_verify_provider_bindings, needs_format_diagnostic,
    };

    let module_root = context.module_root;
    let shown_path = display_check_path(path, module_root);
    let own_key = canonical_path_key(path);

    let mut opts = AnalyzeOptions::new(shown_path.clone());
    opts.module_base_dir = Some(module_root.to_string());
    opts.source_path = Some(path.to_string());
    opts.include_verify_run = true;
    opts.verify_run_hostile = context.hostile;
    let mut report =
        analyze_source_with_verify_provider_bindings(source, &opts, context.provider_bindings);
    report.diagnostics.retain(|d| {
        let key = span_file_key(&d.span.file, module_root);
        key == own_key || !context.unit_keys.contains(&key)
    });

    // Suppression runs before the format check on purpose: `needs-format`
    // still counts toward `format_needed` and still forces exit 1, so
    // letting a waiver delete the diagnostic would report a clean module
    // that fails anyway.
    let suppress_key = suppression_path(path, module_root);
    tracker.note_file(context.config, &suppress_key);
    let suppressed_count = apply_check_suppressions(
        &mut report.diagnostics,
        context.config,
        &suppress_key,
        tracker,
    );

    // Format check: append needs-format diagnostic with structured
    // per-rule violations (capped at the factory's MAX_VIOLATION_REGIONS).
    let (needs_format, format_violations) =
        match try_format_project_source(source, module_root, path) {
            Ok((formatted, violations)) if formatted != source => (true, violations),
            _ => (false, Vec::new()),
        };
    if needs_format {
        report.diagnostics.push(needs_format_diagnostic(
            &shown_path,
            &format_violations,
            source,
        ));
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
    let check_errors = report
        .diagnostics
        .iter()
        .filter(|d| matches!(d.severity, aver::diagnostics::Severity::Error))
        .count();
    let verify_setup_failures = report
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.slug == "verify-provider-setup")
        .count();
    let verify_failures = verify_setup_failures
        + report
            .verify_summary
            .as_ref()
            .map(|vs| vs.blocks.iter().map(|b| b.failed).sum::<usize>())
            .unwrap_or(0);
    let verify_declined = report
        .verify_summary
        .as_ref()
        .map(|vs| vs.blocks.iter().map(|b| b.declined).sum::<usize>())
        .unwrap_or(0);

    if context.json {
        println!("{}", report.to_json());
    } else {
        render_audit_tty(&shown_path, &report, needs_format, suppressed_count);
    }

    AuditTotals {
        check_errors,
        verify_failures,
        verify_declined,
        format_needed: usize::from(needs_format),
    }
}

fn render_audit_tty(
    shown_path: &str,
    report: &aver::diagnostics::AnalysisReport,
    needs_format: bool,
    suppressed_count: usize,
) {
    println!("{}", format!("Audit: {}", shown_path).cyan());
    for diag in &report.diagnostics {
        println!("  {}[{}]: {}", severity_tag(diag), diag.slug, diag.summary);
    }
    if let Some(vs) = &report.verify_summary {
        for block in &vs.blocks {
            if block.failed == 0 && block.skipped == 0 && block.declined == 0 {
                println!(
                    "  {} verify {}  {}/{}",
                    "✓".green(),
                    block.name,
                    block.passed,
                    block.total
                );
            } else if block.declined > 0 {
                // Not answered is neither passed nor failed; say so first,
                // because it is the one outcome a reader must not skim past.
                println!(
                    "  {} verify {}  {}/{} passed, {} not answered{}",
                    "?".yellow(),
                    block.name,
                    block.passed,
                    block.total,
                    block.declined,
                    if block.failed > 0 {
                        format!(", {} failed", block.failed)
                    } else {
                        String::new()
                    }
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
    if suppressed_count > 0 {
        println!("  {} warning(s) suppressed by aver.toml", suppressed_count);
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

pub(super) fn cmd_check(path: &str, module_root_override: Option<&str>, verbose: bool, json: bool) {
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
    // Every module of every input's program, each collected once: a module
    // reached from an earlier input is not a unit of a later one.
    let mut reported = HashSet::new();
    let mut load_cache = aver::source::ProgramLoadCache::default();
    let programs = inputs
        .iter()
        .map(|file| {
            (
                file,
                collect_program_units_with_cache(
                    file,
                    &module_root,
                    &mut reported,
                    &mut load_cache,
                ),
            )
        })
        .collect::<Vec<_>>();

    // Unused exposes are judged over the union of those programs, so a name
    // one input's program exports for another input's program counts as
    // used.
    let unused_exposes = {
        let union = programs
            .iter()
            .filter_map(|(_, units)| units.as_ref().ok())
            .flatten()
            .collect::<Vec<_>>();
        let mut by_file: HashMap<String, Vec<CheckFinding>> = HashMap::new();
        for finding in collect_unused_exposes_findings(&union, &module_root) {
            if let Some(path) = &finding.file {
                by_file
                    .entry(canonical_path_key(path))
                    .or_default()
                    .push(finding);
            }
        }
        by_file
    };

    let mut checked_modules = 0usize;
    let mut failed_modules = Vec::new();
    let mut tracker = SuppressionTracker::new(config.as_ref());
    let mut printed_any = false;

    for (file, units) in programs {
        // An input whose every module was reported under an earlier input
        // has no section of its own.
        if matches!(&units, Ok(units) if units.is_empty()) {
            continue;
        }
        if !json && batch {
            if printed_any {
                println!();
            }
            println!("Input: {}", display_check_path(file, &module_root).cyan());
        }
        printed_any = true;

        match units {
            Ok(units) => {
                let outcomes = check_units(
                    &units,
                    &module_root,
                    config.as_ref(),
                    verbose,
                    json,
                    &mut tracker,
                    &unused_exposes,
                );
                checked_modules += outcomes.len();
                failed_modules.extend(
                    outcomes
                        .into_iter()
                        .filter_map(|(path, has_errors)| has_errors.then_some(path)),
                );
            }
            Err(e) => {
                eprintln!("{}", e.red());
                if reported.insert(aver::source::canonicalize_path(Path::new(file))) {
                    checked_modules += 1;
                }
                failed_modules.push(file.clone());
            }
        }
    }

    report_dead_suppressions(config.as_ref(), &tracker, batch);

    let passed = checked_modules.saturating_sub(failed_modules.len());
    if json {
        println!(
            "{{\"schema_version\":1,\"kind\":\"summary\",\"files\":{},\"modules\":{},\"passed\":{},\"failed\":{}}}",
            inputs.len(),
            checked_modules,
            passed,
            failed_modules.len()
        );
    } else if batch || checked_modules > 1 {
        println!();
        if failed_modules.is_empty() {
            println!(
                "{}",
                format!("Checked {} module(s): {} passed", checked_modules, passed).green()
            );
        } else {
            println!(
                "{}",
                format!(
                    "Checked {} module(s): {} passed, {} failed",
                    checked_modules,
                    passed,
                    failed_modules.len()
                )
                .red()
            );
            for file in &failed_modules {
                println!("  {}", display_check_path(file, &module_root));
            }
            if failed_modules.len() > 3 {
                println!(
                    "{}",
                    "hint: if these files use modules, pass --module-root <dir>".dimmed()
                );
            }
        }
    }

    if !failed_modules.is_empty() {
        process::exit(1);
    }
}

/// Report `[[verify.costly]]` entries that found nothing to raise.
///
/// Same hygiene `[[check.suppress]]` gets, and for the same reason: a waiver
/// that points at a fn or a path this run never saw is a claim about the
/// project that has quietly stopped being true. Written to stderr so `--json`
/// stdout stays a clean stream, and never changes the exit code.
///
/// Whether an entry raises at all is settled against the project default,
/// once, at load: an entry not above it is a config error. So the only
/// question left here is whether the entry found a block, and it is never
/// which of two matching entries won — an entry out-granted by another still
/// matched a live block, and losing a tie-break says nothing about whether
/// the declaration is still true of the project.
fn report_stale_verify_costly(
    config: Option<&aver::config::ProjectConfig>,
    file_results: &[VerifyFileResult],
    module_root: &str,
) {
    let Some(cfg) = config else {
        return;
    };
    if cfg.verify.costly.is_empty() {
        return;
    }
    let mut covered = vec![false; cfg.verify.costly.len()];
    let mut matched = vec![false; cfg.verify.costly.len()];
    for fr in file_results {
        let key = aver::diagnostics::vm_verify::costly_glob_key(&fr.path, Some(module_root));
        for idx in 0..cfg.verify.costly.len() {
            if cfg.verify_costly_covers_file(idx, &key) {
                covered[idx] = true;
            }
            if fr
                .blocks
                .iter()
                .any(|block| cfg.verify_costly_applies(idx, &block.fn_name, &key))
            {
                matched[idx] = true;
            }
        }
    }
    for (idx, entry) in cfg.verify.costly.iter().enumerate() {
        if matched[idx] {
            continue;
        }
        let scope = if entry.files.is_empty() {
            "every file".to_string()
        } else {
            entry.files.join(", ")
        };
        let detail = if covered[idx] {
            "matched files in this run but no verify block of that fn — the fn may be stale"
        } else {
            "matched no verified file — the path may be stale"
        };
        eprintln!(
            "{} aver.toml [[verify.costly]] fn = \"{}\" (files: {}) {}",
            "warning:".yellow(),
            entry.fn_name,
            scope,
            detail
        );
    }
}

/// Render a step count the way a reader compares budgets: `8.2M`, `50M`,
/// `812k`, `431`. Exact below a thousand, one decimal above.
fn format_step_count(steps: u64) -> String {
    const K: u64 = 1_000;
    const M: u64 = 1_000_000;
    const G: u64 = 1_000_000_000;
    let (scaled, unit) = if steps >= G {
        (steps as f64 / G as f64, "G")
    } else if steps >= M {
        (steps as f64 / M as f64, "M")
    } else if steps >= K {
        (steps as f64 / K as f64, "k")
    } else {
        return steps.to_string();
    };
    if (scaled - scaled.round()).abs() < 0.05 {
        format!("{}{}", scaled.round() as u64, unit)
    } else {
        format!("{:.1}{}", scaled, unit)
    }
}

struct VerifyFileResult {
    path: String,
    source: String,
    blocks: Vec<VerifyResult>,
}

/// A module `verify` did not check, and the number of verify blocks that went
/// unchecked with it.
struct VerifyUncheckedFile {
    path: String,
    blocks: usize,
}

/// Why a module went unchecked. Each reason owns its repair line, so a fault
/// `aver check` cannot see never sends the user there.
#[derive(Clone, Copy, PartialEq, Eq)]
enum VerifySkipReason {
    /// A source error — `aver check` prints it in full.
    Source,
    /// The wasm-gc backend refused the program; the source type-checks.
    WasmGcBackend,
    /// Provider composition or setup failed.
    ProviderSetup,
    /// Verify itself could not run. Not a source error, so `aver check`
    /// passes on the file and has nothing to add.
    Engine,
}

/// One module could not be checked, so it and every module of the same
/// program behind it went unchecked.
struct VerifyStop {
    /// The module that carries the fault — not necessarily the entry.
    at: String,
    reason: VerifySkipReason,
    message: String,
    unchecked: Vec<VerifyUncheckedFile>,
}

/// What one input produced: the modules that were verified, plus the stop, if
/// any. Verified modules are kept even when a later module of the same program
/// could not be checked — their results are real and the user asked for them.
struct VerifyRun {
    results: Vec<VerifyFileResult>,
    stop: Option<VerifyStop>,
}

struct PlannedVerifyInput {
    file: String,
    units: Result<Vec<VerifyReportUnit>, String>,
}

struct VerifyReportUnit {
    path: String,
    source: String,
    items: Vec<TopLevel>,
    loaded: Vec<aver::source::LoadedModule>,
    project_dependencies: Vec<(PathBuf, String)>,
    fault: Option<String>,
    prepared: Option<Result<aver::diagnostics::vm_verify::PreparedVmVerify, String>>,
}

/// Bucket a message `verify` could not get past. `Engine` is the bucket that
/// makes a claim — that `aver check` passes on the file and has nothing to
/// add — so it is only ever chosen for a message verify itself is known to
/// have written. Everything else falls back to `Source`, where the worst case
/// is a pointer at `aver check` that turns out to have nothing to say. Source
/// faults reach here in several dialects (`error[...]` from the type checker,
/// `Type errors in dependency`, and the plain-prose parse / module-resolution
/// messages `load_compile_deps` returns), and no list of those would stay
/// complete.
fn classify_verify_stop(message: &str) -> VerifySkipReason {
    if message.starts_with("wasm-gc") || message.starts_with("verify --wasm-gc") {
        VerifySkipReason::WasmGcBackend
    } else if aver::provider::is_provider_setup_error(message) {
        VerifySkipReason::ProviderSetup
    } else if
    // The `--hostile` expansion cap.
    message.starts_with("verify '")
        // The bytecode compiler refused a program the checker accepted.
        || message.starts_with("VM compile error:")
        // Misuse of the trace vocabulary, which only exists inside a law.
        || message.starts_with("Trace.")
    {
        VerifySkipReason::Engine
    } else {
        VerifySkipReason::Source
    }
}

/// The verify blocks the entry itself declares, for the one case where no
/// program units could be collected at all. Reporting `0` for a file full of
/// `verify` blocks understates the gap by exactly the number that matters.
fn verify_entry_block_count(file: &str, module_root: &str) -> usize {
    read_file(file)
        .ok()
        // Parsed the way the project declares it: `[verify] max-cases` decides
        // whether a block parses at all, so counting under the compiled
        // default would understate a project that raised its own ceiling.
        .and_then(|source| parse_file(&source, module_root, file).ok())
        .map(|items| aver::checker::merge_verify_blocks(&items).len())
        .unwrap_or(0)
}

fn verify_stopped_before_any_module(
    file: &str,
    module_root: &str,
    reason: VerifySkipReason,
    message: String,
) -> VerifyRun {
    VerifyRun {
        results: Vec::new(),
        stop: Some(VerifyStop {
            at: file.to_string(),
            reason,
            message,
            unchecked: vec![VerifyUncheckedFile {
                path: file.to_string(),
                blocks: verify_entry_block_count(file, module_root),
            }],
        }),
    }
}

/// Verify every not-yet-reported module of the program named by `file`,
/// leaves-first.
struct VerifyRunOptions<'a> {
    module_root: &'a str,
    hostile: bool,
    wasm_gc: bool,
    parallel_cases: bool,
    provider_bindings: &'a [aver::provider::ProviderBinding],
    config: &'a Result<Option<aver::config::ProjectConfig>, String>,
}

fn run_verify_for_units(
    file: &str,
    units: Vec<VerifyReportUnit>,
    options: VerifyRunOptions<'_>,
) -> VerifyRun {
    use aver::verify_law::expand::ExpansionMode;
    let VerifyRunOptions {
        module_root,
        hostile,
        wasm_gc,
        parallel_cases,
        provider_bindings,
        config,
    } = options;
    // Counted before anything runs — and before anything else can fail: once a
    // module stops the walk, the modules from that one on carry blocks nobody
    // checked, and the summary has to say how many. A stop that happens here,
    // rather than inside the loop, still leaves every one of these modules
    // unchecked.
    let mut unchecked: Vec<VerifyUncheckedFile> = units
        .iter()
        .map(|unit| VerifyUncheckedFile {
            path: unit.path.clone(),
            blocks: aver::checker::merge_verify_blocks(&unit.items).len(),
        })
        .collect();

    // Every module of this program was already reported under an earlier input
    // of the same directory walk. There is nothing left for this entry to run
    // and nothing left for it to skip, so it must not open a stop of its own:
    // that would print `0 file(s) not checked` above an empty list, and count
    // modules an earlier entry already counted.
    if unchecked.is_empty() {
        return VerifyRun {
            results: Vec::new(),
            stop: None,
        };
    }

    // Loaded before the loop, because the project's `[verify] max-cases` is
    // what the loader parsed these units under, and the re-parse below has to
    // agree with it. An unreadable `aver.toml` is a project error like any
    // other, so it stops the walk and leaves every module above unchecked.
    let config = match config {
        Ok(config) => config.clone(),
        Err(e) => {
            return VerifyRun {
                results: Vec::new(),
                stop: Some(VerifyStop {
                    at: file.to_string(),
                    reason: VerifySkipReason::Source,
                    message: e.clone(),
                    unchecked,
                }),
            };
        }
    };

    let mut results = Vec::new();
    let mode = if hostile {
        ExpansionMode::Hostile
    } else {
        ExpansionMode::Declared
    };
    for (index, unit) in units.into_iter().enumerate() {
        let VerifyReportUnit {
            path,
            source,
            items,
            loaded: _,
            project_dependencies: _,
            fault: _,
            prepared,
        } = unit;
        // `collect_program_units` swallows a parse error into empty `items` so
        // that `aver check` can surface it as a canonical line/col diagnostic
        // via its own analysis pass. `verify` has no such pass, so an
        // unparseable file would silently report "no verify blocks" and exit 0
        // — hiding both the parse error and any real blocks behind it.
        // Re-parse ONLY when there are no items (cheap and rare — an
        // empty/comment-only file parses to no items with no error, and is
        // left alone) and surface the real parse error so `verify` fails
        // loudly instead of passing green.
        if items.is_empty()
            && let Err(e) = aver::source::parse_source_with_verify_ceiling(
                &source,
                match &config {
                    Some(cfg) => aver::source::verify_ceiling_for(cfg, module_root, &path),
                    None => aver::config::VerifyCaseCeiling::compiled_default(),
                },
            )
        {
            unchecked.drain(..index);
            return VerifyRun {
                results,
                stop: Some(VerifyStop {
                    at: path,
                    reason: VerifySkipReason::Source,
                    message: e,
                    unchecked,
                }),
            };
        }
        let outcome = if wasm_gc {
            #[cfg(feature = "wasm")]
            {
                aver::diagnostics::wasm_gc_verify::run_verify_for_items_wasm_gc_with_mode(
                    items,
                    config.clone(),
                    Some(module_root),
                    &path,
                    mode,
                )
            }
            #[cfg(not(feature = "wasm"))]
            {
                let _ = (items, &path);
                Err("verify --wasm-gc requires building with --features wasm".to_string())
            }
        } else {
            if let Some(prepared) = prepared {
                prepared.and_then(|prepared| {
                    aver::diagnostics::vm_verify::run_prepared_verify_vm_with_bindings(
                        prepared,
                        config.clone(),
                        Some(module_root),
                        &path,
                        provider_bindings,
                        parallel_cases,
                    )
                })
            } else if parallel_cases {
                aver::diagnostics::vm_verify::run_verify_for_items_vm_parallel_with_mode_and_bindings(
                    items,
                    config.clone(),
                    Some(module_root),
                    &path,
                    mode,
                    provider_bindings,
                )
            } else {
                aver::diagnostics::vm_verify::run_verify_for_items_vm_with_mode_and_bindings(
                    items,
                    config.clone(),
                    Some(module_root),
                    &path,
                    mode,
                    provider_bindings,
                )
            }
        };
        match outcome {
            Ok(blocks) => results.push(VerifyFileResult {
                path,
                source,
                blocks,
            }),
            Err(e) => {
                let reason = classify_verify_stop(&e);
                unchecked.drain(..index);
                return VerifyRun {
                    results,
                    stop: Some(VerifyStop {
                        at: path,
                        reason,
                        message: e,
                        unchecked,
                    }),
                };
            }
        }
    }

    VerifyRun {
        results,
        stop: None,
    }
}

fn collect_verify_program_units_with_cache(
    file: &str,
    module_root: &str,
    reported: &mut HashSet<PathBuf>,
    cache: &mut aver::source::ProgramLoadCache,
) -> Result<Vec<VerifyReportUnit>, String> {
    let program = load_report_program_with_cache(file, module_root, cache)?;
    Ok(program
        .report_units()
        .filter(|module| reported.insert(aver::source::canonicalize_path(&module.path)))
        .map(|module| {
            let (loaded, graph_fault) = match program.loaded_dependencies_for(module) {
                Ok(loaded) => (loaded, None),
                Err(error) => (Vec::new(), Some(error.to_string())),
            };
            let project_dependencies = loaded
                .iter()
                .filter(|dependency| !dependency.path.starts_with("<aver-stdlib>"))
                .map(|dependency| (dependency.path.clone(), dependency.dep_name.clone()))
                .collect();
            VerifyReportUnit {
                path: if module.is_entry {
                    file.to_string()
                } else {
                    module.path.to_string_lossy().to_string()
                },
                source: module.source.clone(),
                items: module.items.clone(),
                loaded,
                project_dependencies,
                fault: module
                    .fault
                    .as_ref()
                    .map(ToString::to_string)
                    .or(graph_fault),
                prepared: None,
            }
        })
        .collect())
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
        // Exhaustive on purpose: a decline is neither bucket. Folding it into
        // `failed` would report a counter-example Aver never saw.
        let passed = match &case.outcome {
            VerifyCaseOutcome::Pass => true,
            VerifyCaseOutcome::Mismatch { .. }
            | VerifyCaseOutcome::RuntimeError { .. }
            | VerifyCaseOutcome::UnexpectedErr { .. } => false,
            VerifyCaseOutcome::Skipped
            | VerifyCaseOutcome::SkippedAfterBaseFail
            | VerifyCaseOutcome::Declined { .. } => continue,
        };
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
        verify_declined_diagnostic, verify_mismatch_diagnostic, verify_runtime_error_diagnostic,
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
                        VerifyCaseOutcome::Declined {
                            reason,
                            steps,
                            limit,
                            raised_by,
                        } => Some(verify_declined_diagnostic(
                            &display_path,
                            &fr.source,
                            &block.block_label,
                            &cr.case_expr,
                            reason,
                            *steps,
                            *limit,
                            raised_by.as_deref(),
                            line,
                            col,
                        )),
                        VerifyCaseOutcome::Pass
                        | VerifyCaseOutcome::Skipped
                        | VerifyCaseOutcome::SkippedAfterBaseFail => None,
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
                    declined: block.declined,
                    total: block.passed + block.failed + block.skipped + block.declined,
                    declared_passed,
                    declared_failed,
                    hostile_passed,
                    hostile_failed,
                    skipped_by_when,
                    skipped_after_base_fail,
                    costly_cases: aver::diagnostics::verify_run::costly_cases_of(block),
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
                let total = block.passed + block.failed + block.skipped + block.declined;
                if block.failed == 0 && block.declined == 0 {
                    println!(
                        "  {} {}      {}/{}",
                        "✓".green(),
                        block.block_label,
                        block.passed,
                        total
                    );
                } else if block.failed == 0 {
                    // Nothing disagreed; some cases were not answered. The
                    // mark is neither ✓ nor ✗ — the block was not checked.
                    println!(
                        "  {} {}      {}/{} passed ({} not answered)",
                        "?".yellow(),
                        block.block_label,
                        block.passed,
                        total,
                        block.declined
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
                                // Declines are counted from `block.declined`
                                // just below, alongside the failure kinds.
                                VerifyCaseOutcome::Declined { .. }
                                | VerifyCaseOutcome::Pass
                                | VerifyCaseOutcome::Skipped
                                | VerifyCaseOutcome::SkippedAfterBaseFail => {}
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
                        if block.declined > 0 {
                            parts.push(format!("{} not answered", block.declined));
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

                // What the raised budget bought. Named explicitly so a
                // `[[verify.costly]]` entry is never a silent licence: the
                // reader sees which cases needed more than the project
                // default and by how much.
                for costly in aver::diagnostics::verify_run::costly_cases_of(block) {
                    println!(
                        "    {}",
                        format!(
                            "{} case {}: {} steps (limit {}, aver.toml [[verify.costly]] fn = \"{}\")",
                            block.fn_name,
                            costly.case_index + 1,
                            format_step_count(costly.steps),
                            format_step_count(costly.limit),
                            costly.raised_by
                        )
                        .dimmed()
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
                        VerifyCaseOutcome::Declined {
                            reason,
                            steps,
                            limit,
                            raised_by,
                        } => Some(verify_declined_diagnostic(
                            &display_path,
                            &fr.source,
                            &block.block_label,
                            &cr.case_expr,
                            reason,
                            *steps,
                            *limit,
                            raised_by.as_deref(),
                            line,
                            col,
                        )),
                        VerifyCaseOutcome::Pass
                        | VerifyCaseOutcome::Skipped
                        | VerifyCaseOutcome::SkippedAfterBaseFail
                        | VerifyCaseOutcome::Mismatch { .. } => None,
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

#[allow(clippy::too_many_arguments)]
pub(super) fn cmd_verify(
    path: &str,
    module_root_override: Option<&str>,
    verbose: bool,
    json: bool,
    hostile: bool,
    wasm_gc: bool,
    jobs: Option<usize>,
    provider_bindings: &[aver::provider::ProviderBinding],
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
    let input_count = inputs.len();
    let jobs = jobs.unwrap_or_else(|| {
        std::thread::available_parallelism()
            .map(std::num::NonZeroUsize::get)
            .unwrap_or(1)
    });

    // Ownership is decided once, in sorted input order, before any worker
    // starts. The shared loader cache makes overlapping dependency cones pay
    // for filesystem IO and parsing once while preserving each walk's own
    // cycle detection and leaves-first order.
    let mut reported = HashSet::new();
    let mut load_cache = aver::source::ProgramLoadCache::default();
    let mut plans: Vec<PlannedVerifyInput> = inputs
        .into_iter()
        .map(|file| PlannedVerifyInput {
            units: collect_verify_program_units_with_cache(
                &file,
                &module_root,
                &mut reported,
                &mut load_cache,
            ),
            file,
        })
        .collect();
    let config = load_runtime_policy(&module_root);
    let pool = if jobs > 1 {
        match rayon::ThreadPoolBuilder::new().num_threads(jobs).build() {
            Ok(pool) => Some(pool),
            Err(error) => {
                eprintln!("{}", format!("Cannot start verify workers: {error}").red());
                process::exit(1);
            }
        }
    } else {
        None
    };

    // Declared VM verification prepares each unique project module once in
    // the same bounded pool used by its cases. Every job checks only its own
    // body against immutable surfaces from the one graph walk above. Once all
    // jobs join, dependency errors are propagated through the transitive
    // closure before any report runs, retaining leaves-first fail-closed
    // semantics without serializing otherwise independent preparation.
    if !hostile && !wasm_gc {
        let prepare_unit = |unit: &mut VerifyReportUnit| {
            let prepared = if let Some(fault) = &unit.fault {
                Err(fault.clone())
            } else {
                aver::diagnostics::vm_verify::prepare_verify_for_items_vm_with_checked_loaded(
                    unit.items.clone(),
                    std::mem::take(&mut unit.loaded),
                    &unit.path,
                )
            };
            unit.prepared = Some(prepared);
        };
        if let Some(pool) = &pool {
            use rayon::prelude::*;
            pool.install(|| {
                plans.par_iter_mut().for_each(|plan| {
                    if let Ok(units) = &mut plan.units {
                        units.par_iter_mut().for_each(prepare_unit);
                    }
                })
            });
        } else {
            for plan in &mut plans {
                if let Ok(units) = &mut plan.units {
                    units.iter_mut().for_each(&prepare_unit);
                }
            }
        }

        let own_errors = plans
            .iter()
            .filter_map(|plan| plan.units.as_ref().ok())
            .flatten()
            .filter_map(|unit| {
                unit.prepared
                    .as_ref()
                    .and_then(|prepared| prepared.as_ref().err())
                    .map(|error| {
                        (
                            aver::source::canonicalize_path(Path::new(&unit.path)),
                            error.clone(),
                        )
                    })
            })
            .collect::<HashMap<_, _>>();
        for unit in plans
            .iter_mut()
            .filter_map(|plan| plan.units.as_mut().ok())
            .flatten()
        {
            let failed_dependency =
                unit.project_dependencies
                    .iter()
                    .find_map(|(dependency_path, dependency_name)| {
                        own_errors
                            .get(&aver::source::canonicalize_path(dependency_path))
                            .map(|error| (dependency_name, error))
                    });
            if let Some((dependency, error)) = failed_dependency {
                unit.prepared = Some(Err(format!(
                    "Type errors in dependency module '{dependency}':\n{error}"
                )));
            }
        }
    }

    let execute = |plan: PlannedVerifyInput, parallel_cases: bool| {
        let file = plan.file;
        let run = match plan.units {
            Ok(units) => run_verify_for_units(
                &file,
                units,
                VerifyRunOptions {
                    module_root: &module_root,
                    hostile,
                    wasm_gc,
                    parallel_cases,
                    provider_bindings,
                    config: &config,
                },
            ),
            Err(error) => verify_stopped_before_any_module(
                &file,
                &module_root,
                VerifySkipReason::Source,
                error,
            ),
        };
        (file, run)
    };

    let runs: Vec<(String, VerifyRun)> = if let Some(pool) = &pool {
        use rayon::prelude::*;
        pool.install(|| {
            plans
                .into_par_iter()
                .map(|plan| execute(plan, !wasm_gc))
                .collect()
        })
    } else {
        plans.into_iter().map(|plan| execute(plan, false)).collect()
    };

    let mut all_file_results: Vec<VerifyFileResult> = Vec::new();
    let mut failed_files = Vec::new();
    let mut stops: Vec<VerifyStop> = Vec::new();
    let mut printed_any = false;
    for (file, run) in runs {
        // Rayon preserves indexed-iterator collection order. Rendering only
        // here makes `-jN` byte-identical to `-j1` even when a later input
        // finishes first. Modules completed before a per-program stop remain
        // reportable: their cases really ran.
        let has_blocks = run.results.iter().any(|fr| !fr.blocks.is_empty());
        if has_blocks && printed_any && !json {
            println!();
        }
        render_verify_output(&run.results, &module_root, verbose, json);
        if has_blocks {
            printed_any = true;
        }
        for fr in &run.results {
            if fr.blocks.iter().any(|b| b.failed > 0) {
                failed_files.push(fr.path.clone());
            }
        }
        all_file_results.extend(run.results);
        if let Some(stop) = run.stop {
            // Name the module that carries the fault, not the entry that
            // happens to depend on it.
            eprintln!(
                "{}: {}",
                display_check_path(&stop.at, &module_root).red(),
                stop.message
            );
            failed_files.push(file);
            stops.push(stop);
        }
    }

    let files_not_checked: usize = stops.iter().map(|stop| stop.unchecked.len()).sum();
    let blocks_unchecked: usize = stops
        .iter()
        .flat_map(|stop| &stop.unchecked)
        .map(|file| file.blocks)
        .sum();

    if !json {
        // Bucket honestly. A wasm-gc backend error (compile / codegen
        // failure, preflight reject, wasmtime setup) is NOT a source error —
        // `aver check` passes on such files, so pointing the user there would
        // be a dead end. The same holds for provider setup and for verify's
        // own refusals, and each bucket says so in its own repair line.
        for (reason, headline, hint) in [
            (
                VerifySkipReason::Source,
                "type errors (run aver check for details)",
                Some("hint: if these files use modules, pass --module-root <dir>"),
            ),
            (
                VerifySkipReason::WasmGcBackend,
                "wasm-gc backend error (the source type-checks; see the message above)",
                Some("hint: `aver verify` (VM) runs these blocks without the wasm-gc backend"),
            ),
            (
                VerifySkipReason::ProviderSetup,
                "provider composition error (the source type-checks; see the message above)",
                None,
            ),
            (
                VerifySkipReason::Engine,
                "verify could not run (not a source error; see the message above)",
                Some("hint: `aver check` will not show this — it passes on these files"),
            ),
        ] {
            let bucket: Vec<&VerifyStop> =
                stops.iter().filter(|stop| stop.reason == reason).collect();
            if bucket.is_empty() {
                continue;
            }
            // The module-root hint is about resolving `depends [...]`. When
            // every stop in this bucket is a project file that does not parse,
            // it has nothing to do with the fault.
            let hint = match reason {
                VerifySkipReason::Source
                    if bucket
                        .iter()
                        .all(|stop| stop.message.starts_with("aver.toml:")) =>
                {
                    None
                }
                _ => hint,
            };
            let count: usize = bucket.iter().map(|stop| stop.unchecked.len()).sum();
            println!();
            println!(
                "{}",
                format!("{} file(s) not checked — {}:", count, headline).yellow()
            );
            for stop in bucket {
                for unchecked in &stop.unchecked {
                    let name = display_check_path(&unchecked.path, &module_root);
                    let line = if unchecked.blocks == 0 {
                        name
                    } else {
                        format!(
                            "{} ({} verify block{} unchecked)",
                            name,
                            unchecked.blocks,
                            if unchecked.blocks == 1 { "" } else { "s" }
                        )
                    };
                    println!("  {}", line.dimmed());
                }
            }
            if let Some(hint) = hint {
                println!("{}", hint.dimmed());
            }
        }
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
    let total_declined: usize = all_file_results
        .iter()
        .flat_map(|fr| &fr.blocks)
        .map(|b| b.declined)
        .sum();
    let total_cases = total_passed + total_failed + total_skipped + total_declined;
    // Modules that declared at least one block; `files` stays what the
    // command was pointed at.
    let total_modules = all_file_results
        .iter()
        .filter(|fr| !fr.blocks.is_empty())
        .count();

    if json {
        // `cases_declined` appears only when something was declined, so a
        // project with nothing to decline emits the bytes it always did.
        // `files_skipped` / `blocks_unchecked` are unconditional: a run that
        // checked everything has to say so, or "nothing to verify" and
        // "nothing was verified" stay indistinguishable.
        let declined_key = if total_declined > 0 {
            format!(",\"cases_declined\":{}", total_declined)
        } else {
            String::new()
        };
        println!(
            "{{\"schema_version\":1,\"kind\":\"summary\",\"files\":{},\"modules\":{},\"blocks\":{},\"cases_passed\":{},\"cases_failed\":{}{},\"files_skipped\":{},\"blocks_unchecked\":{}}}",
            input_count,
            total_modules,
            total_blocks,
            total_passed,
            total_failed,
            declined_key,
            files_not_checked,
            blocks_unchecked
        );
    } else if total_blocks == 0 && files_not_checked == 0 {
        println!(
            "{}",
            format!("No verify blocks found in {}.", path).yellow()
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
                        // Declines have their own total; they are not a
                        // kind of skip — a skip was ruled out by `when`,
                        // a decline was never answered.
                        VerifyCaseOutcome::Declined { .. }
                        | VerifyCaseOutcome::Pass
                        | VerifyCaseOutcome::Mismatch { .. }
                        | VerifyCaseOutcome::RuntimeError { .. }
                        | VerifyCaseOutcome::UnexpectedErr { .. } => {}
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
        // A case Aver never answered is part of the answer too, and it is
        // neither a pass nor a failure.
        let declined_part = if total_declined > 0 {
            format!(" | {} not answered", total_declined)
        } else {
            String::new()
        };
        // A file nobody could check is part of the answer, not a footnote:
        // without this member a run that checked one of three files reads
        // exactly like a run that checked all three.
        let mut not_checked_part = String::new();
        if files_not_checked > 0 {
            not_checked_part = format!(
                " | {} file{} not checked",
                files_not_checked,
                if files_not_checked == 1 { "" } else { "s" }
            );
        }
        let summary = format!(
            "Summary: {} module{} | {} block{} | {}/{} cases passed | {} failed{}{}{}",
            total_modules,
            if total_modules == 1 { "" } else { "s" },
            total_blocks,
            if total_blocks == 1 { "" } else { "s" },
            total_passed,
            total_cases,
            total_failed,
            declined_part,
            skipped_part,
            not_checked_part,
        );
        if total_failed > 0 {
            println!("{}", summary.red());
        } else if total_declined > 0 || files_not_checked > 0 {
            // Never green while something went unanswered or unchecked.
            println!("{}", summary.yellow());
        } else {
            println!("{}", summary.green());
        }
    }

    if let Ok(config) = load_runtime_policy(&module_root) {
        report_stale_verify_costly(config.as_ref(), &all_file_results, &module_root);
    }

    if !json
        && let Some(error) = all_file_results
            .iter()
            .flat_map(|file| &file.blocks)
            .flat_map(|block| &block.case_results)
            .find_map(|case| match &case.outcome {
                aver::checker::VerifyCaseOutcome::RuntimeError { error }
                    if error.contains("capability-provider-missing") =>
                {
                    Some(error.as_str())
                }
                _ => None,
            })
        && let Some(repair) =
            super::provider_host_cmd::missing_provider_repair(error, &module_root, "verify", path)
    {
        eprintln!("\n{repair}");
    }

    // A decline fails the run. "We did not check this" must never be read as
    // "this checks out", and the exit code is the only thing most CI reads.
    if !failed_files.is_empty() || total_failed > 0 || total_declined > 0 {
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

    let mut items = match parse_file(&source, &module_root, file) {
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
    // A context that carries ProofIR is one the proof exporters read,
    // so its dep modules keep the fabricating passes off whatever the
    // caller asked for at `apply_traversal_lowering`. The entry
    // module's own line is structural (`pipeline::run` snapshots the
    // AST for the proof stages and completes the copy without those
    // passes); this is the same line one level down, where the dep
    // module's post-pass items are what `ModuleInfo.fn_defs` carries
    // into the export. Everything above the line — `escape`, the
    // ownership annotations — runs on a dep exactly as it does on the
    // entry module and on the certified artifact's own dep load
    // (`cmd_compile_wasm_gc` passes the same two `false`s), so the two
    // sides of a certificate agree on dep bodies as well.
    let proof_facing = run_refinement_lower || run_contract_lower || run_law_lower;
    let dep_lowering = apply_traversal_lowering && !proof_facing;
    // Load dep modules BEFORE the entry pipeline runs — needed because
    // the proof-lower pipeline stage walks both entry items and dep
    // module type/fn defs in one sweep (cross-module refinement records,
    // module-spanning call graphs). load_compile_deps only reads
    // `TopLevel::Module(m).depends`, which TCO never touches, so it's
    // safe to run pre-pipeline.
    let modules = load_compile_deps(
        &items,
        &module_root,
        DepLowering::fully_lowered(dep_lowering, with_self_host_support),
    );

    let mut pipeline_result = aver::ir::pipeline::run(
        &mut items,
        aver::ir::PipelineConfig {
            typecheck: Some(typecheck_mode),
            run_interp_lower: apply_traversal_lowering,
            run_buffer_build: apply_traversal_lowering,
            run_chars_fusion: apply_traversal_lowering,
            run_string_index: !proof_facing,
            run_list_build: apply_traversal_lowering,
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
    let tc_result = pipeline_result
        .typecheck
        .take()
        .expect("typecheck was requested");
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
    let prebuilt_proof_ir = pipeline_result.proof_ir.take();
    // A proof-facing context is assembled from the pipeline's proof
    // view — the AST as it stood before the first optimising pass, plus
    // the facts derived from THAT AST — so the exporters describe the
    // program the user wrote no matter which passes this build ran.
    // Runtime backends keep the post-pipeline items, deforestation and
    // all. `codegen_view` is where that choice is made.
    let view = pipeline_result.codegen_view(items);
    let mut ctx = codegen::build_context(
        view.items,
        &tc_result,
        view.analysis.as_ref(),
        name,
        modules,
        view.symbol_table,
        view.resolved_items,
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

    // A code generator that writes a deliberate compile error must not
    // report success. The Rust emitter substitutes `compile_error!` for a
    // construct it cannot render, which used to leave `compile` exiting 0
    // with a crate that only fails once the user reaches `cargo build`.
    // The files are written first so the message is readable in place.
    //
    // What counts is what the emitter recorded when it made the
    // substitution — not what a scan of the output finds. A program may put
    // `compile_error!` in a string of its own, and that is not a backend
    // refusal.
    let unrenderable = output.generated_compile_errors();
    if !unrenderable.is_empty() {
        eprintln!(
            "{}",
            format!(
                "Compiled {} → {}/ [{}], but the generated code contains a compile error the backend could not avoid:",
                file, output_dir, target_label
            )
            .red()
        );
        for message in unrenderable {
            eprintln!("  {}", message.dimmed());
        }
        eprintln!(
            "{}",
            "the message is in the file too, at the construct that could not be rendered".dimmed()
        );
        process::exit(1);
    }

    println!(
        "{}",
        format!("Compiled {} → {}/ [{}]", file, output_dir, target_label).green()
    );
    // A verify case the backend could not render is left out of the
    // generated test module. That is not a build failure, but it is a case
    // the crate does not run, so it is named here rather than vanishing.
    if !output.omitted_verify_cases.is_empty() {
        println!(
            "  {}",
            "verify cases not carried into the generated tests:".yellow()
        );
        for note in &output.omitted_verify_cases {
            println!("    {}", note.dimmed());
        }
    }
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
        module_root: av_path
            .parent()
            .unwrap_or_else(|| Path::new("."))
            .to_path_buf(),
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
        "chars_fusion" => Some(PipelineStage::CharsFusion),
        "string_index" => Some(PipelineStage::StringIndex),
        "list_build" => Some(PipelineStage::ListBuild),
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
                     parse, tco, typecheck, interp_lower, buffer_build, chars_fusion, string_index, list_build, resolve, last_use, analyze, escape, build_symbols, name_resolve, refinement_lower, contract_lower, law_lower, mir",
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
    let mut items = match parse_file(&source, &module_root, file) {
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
    let dep_modules = load_compile_deps(&items, &module_root, DepLowering::PRISTINE);
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
    // Its `FnId`s are keyed through the symbol table of the AST the
    // stages read, which is the proof view's, not the runtime one's —
    // these defaults leave fusion on, and a fused run's table carries
    // a `__buffered` entry the proof view has no name for.
    if proof_target {
        let symbols = match pipeline_result.proof_view.as_ref() {
            Some(view) => &view.symbol_table,
            None => &pipeline_result.symbol_table,
        };
        match pipeline_result.proof_ir.as_ref() {
            Some(ir) => print!("{}", render_proof_ir_dump(ir, symbols)),
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
    for te in &symbols.types {
        let shape = if te.is_product { "record" } else { "sum" };
        let type_id = symbols
            .type_id_of(&te.key)
            .expect("listed type must have an identity");
        writeln!(
            out,
            "- TypeId({}) = {} ({}, {} ctor(s), in ModuleId({}))",
            type_id.0,
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
        let owning = symbols.type_entry(ce.owning_type);
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
    let mut items = match parse_file(&source, &module_root, file) {
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
    let dep_modules = load_compile_deps(&items, &module_root, DepLowering::PRISTINE);
    let dep_fusion = dep_fusion_reports(&items, &module_root);
    let mut result = aver::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full {
                base_dir: Some(&module_root),
            }),
            alloc_policy: Some(&neutral_policy),
            dep_modules: &dep_modules,
            run_refinement_lower: true,
            run_interval_analyze: true,
            run_contract_lower: true,
            run_law_lower: true,
            ..Default::default()
        },
    );
    if let Some(tc) = &result.typecheck
        && !tc.errors.is_empty()
    {
        eprintln!("{}", super::shared::format_type_errors(&tc.errors).red());
        process::exit(1);
    }
    merge_dep_buffer_build(&mut result.pass_diagnostics, &dep_fusion);
    merge_dep_chars_fusion(&mut result.pass_diagnostics, &dep_fusion);
    merge_dep_string_index(&mut result.pass_diagnostics, &dep_fusion);
    merge_dep_list_build(&mut result.pass_diagnostics, &dep_fusion);

    if json {
        print!("{}", render_pass_diagnostics_json(&result.pass_diagnostics));
    } else {
        print!("{}", render_pass_diagnostics(&result.pass_diagnostics));
    }
}

/// Which compile targets actually carry the fusion the `buffer_build`
/// report counts.
///
/// The pass is half of the traversal-lowering toggle: `aver run` and the
/// default Rust codegen run it over the entry module and every
/// dependency, while `--target wasm-gc` and `--target wasip2` build with
/// it off (their lowering has no representation for the buffer it
/// introduces) — as do the proof exporters. `--explain-passes` runs one
/// pipeline regardless of `--target`, so a report of N rewritten sites
/// is a statement about the rust / VM artifact and about no other. Say
/// so rather than let the reader assume it describes the wasm they
/// asked for.
const DEFORESTING_TARGETS_JSON: &str = "[\"rust\",\"vm\"]";
const DEFORESTING_TARGETS_NOTE: &str = "counted for the rust and VM pipelines — --target wasm-gc and --target wasip2 build without this pass, so their artifacts carry none of these rewrites";
const STRING_INDEX_TARGETS_JSON: &str = "[\"rust\",\"vm\",\"wasm-gc\",\"wasip2\"]";
const STRING_INDEX_TARGETS_NOTE: &str =
    "counted for every runtime pipeline — rust, VM, wasm-gc, and wasip2";

/// Per-dependency deforestation reports, for `--explain-passes`.
///
/// The pipeline this command runs sees the ENTRY file only, so a program
/// whose dependency fuses — Aver's own `Bytes.toHex` is one — was
/// reported as having no fusion sites at all. That is the diagnostic
/// lying about the artifact: the compile path runs the pass over every
/// dep module too.
///
/// The dep `ModuleInfo`s cannot answer this. They come back resolved,
/// and the recogniser matches `Expr::Ident` shapes that `resolve` has
/// already rewritten to `Expr::Resolved`; re-detecting on them finds
/// nothing. So re-run the pre-resolve prefix of the pipeline on a fresh
/// parse of each dep and keep only its report. Typecheck is skipped —
/// `load_compile_deps` has already surfaced any dep type errors, and
/// detection is purely syntactic.
///
/// All dependency-side fabricating passes are asked at once because the
/// runtime compile paths run their supported subset over every module;
/// each would otherwise be reported as having done nothing.
fn dep_fusion_reports(items: &[TopLevel], module_root: &str) -> Vec<DepFusion> {
    let Some(module) = items.iter().find_map(|i| match i {
        TopLevel::Module(m) => Some(m),
        _ => None,
    }) else {
        return vec![];
    };
    let mut roots = module.depends.clone();
    roots.extend(aver::stdlib::implicit_stdlib_deps(items));
    let Ok(loaded) = aver::source::load_module_tree(&roots, module_root) else {
        // Unresolvable deps are already a hard error on the loader path
        // above; a diagnostic must not exit twice on the same fault.
        return vec![];
    };

    let mut out = Vec::new();
    for m in loaded {
        let mut dep_items = m.items;
        let result = aver::ir::pipeline::run(
            &mut dep_items,
            aver::ir::PipelineConfig {
                typecheck: None,
                run_resolve: false,
                run_last_use: false,
                run_analyze: false,
                run_escape: false,
                ..Default::default()
            },
        );
        let buffer_build = result.buffer_build.unwrap_or_default();
        let chars_fusion = result.chars_fusion.unwrap_or_default();
        let string_index = result.string_index.unwrap_or_default();
        let list_build = result.list_build.unwrap_or_default();
        if buffer_build.rewrites > 0
            || chars_fusion.fired()
            || !chars_fusion.declined.is_empty()
            || string_index.fired()
            || !string_index.declined.is_empty()
            || list_build.rewrites > 0
            || !list_build.declined.is_empty()
            || !list_build.pair_declined.is_empty()
        {
            out.push(DepFusion {
                prefix: m.dep_name,
                buffer_build,
                chars_fusion,
                string_index,
                list_build,
            });
        }
    }
    out
}

/// One dependency's share of the fabricating passes.
struct DepFusion {
    prefix: String,
    buffer_build: aver::ir::BufferBuildPassReport,
    chars_fusion: aver::ir::CharsFusionPassReport,
    string_index: aver::ir::StringIndexPassReport,
    list_build: aver::ir::ListBuildPassReport,
}

/// Fold per-dependency fusion into the entry's `buffer_build` pass
/// report so both renderers show one honest total. Dep-side names are
/// module-qualified (`Bytes.hexParts`) — the entry's own stay bare, so
/// the reader can tell which file a site lives in.
fn merge_dep_buffer_build(diagnostics: &mut [aver::ir::PassDiagnostic], dep_reports: &[DepFusion]) {
    if dep_reports.is_empty() {
        return;
    }
    for diagnostic in diagnostics.iter_mut() {
        let aver::ir::PassReport::BufferBuild(entry) = &mut diagnostic.report else {
            continue;
        };
        for DepFusion {
            prefix,
            buffer_build: dep,
            ..
        } in dep_reports
        {
            entry.rewrites += dep.rewrites;
            entry
                .synthesized
                .extend(dep.synthesized.iter().map(|n| format!("{prefix}.{n}")));
            entry
                .sink_fns
                .extend(dep.sink_fns.iter().map(|n| format!("{prefix}.{n}")));
            for (sink, count) in &dep.rewrites_by_sink {
                *entry
                    .rewrites_by_sink
                    .entry(format!("{prefix}.{sink}"))
                    .or_default() += count;
            }
        }
        entry.sink_fns.sort();
    }
}

/// The chars-fusion half of the same fold: a cursor synthesized in a
/// dependency is a rewrite the artifact carries, and a loop a dependency
/// DECLINED is the fact this diagnostic exists to surface.
fn merge_dep_chars_fusion(diagnostics: &mut [aver::ir::PassDiagnostic], dep_reports: &[DepFusion]) {
    if dep_reports.is_empty() {
        return;
    }
    for diagnostic in diagnostics.iter_mut() {
        let aver::ir::PassReport::CharsFusion(entry) = &mut diagnostic.report else {
            continue;
        };
        for DepFusion {
            prefix,
            chars_fusion: dep,
            ..
        } in dep_reports
        {
            entry.cursor_rewrites += dep.cursor_rewrites;
            entry.codepoint_matches += dep.codepoint_matches;
            entry.codepoint_calls += dep.codepoint_calls;
            entry
                .synthesized
                .extend(dep.synthesized.iter().map(|n| format!("{prefix}.{n}")));
            entry
                .loop_fns
                .extend(dep.loop_fns.iter().map(|n| format!("{prefix}.{n}")));
            for (fn_name, count) in &dep.codepoint_matches_by_fn {
                *entry
                    .codepoint_matches_by_fn
                    .entry(format!("{prefix}.{fn_name}"))
                    .or_default() += count;
            }
            for (fn_name, reason) in &dep.declined {
                entry.declined.insert(format!("{prefix}.{fn_name}"), reason);
            }
        }
        entry.synthesized.sort();
        entry.loop_fns.sort();
    }
}

/// Fold indexed String workers synthesized in dependencies into the
/// entry report. Component labels can contain several function names,
/// so qualify the complete stable label rather than pretending it is a
/// single symbol.
fn merge_dep_string_index(diagnostics: &mut [aver::ir::PassDiagnostic], dep_reports: &[DepFusion]) {
    if dep_reports.is_empty() {
        return;
    }
    for diagnostic in diagnostics.iter_mut() {
        let aver::ir::PassReport::StringIndex(entry) = &mut diagnostic.report else {
            continue;
        };
        for DepFusion {
            prefix,
            string_index: dep,
            ..
        } in dep_reports
        {
            entry.components += dep.components;
            entry.indexed_accesses += dep.indexed_accesses;
            entry.indexed_fns.extend(
                dep.indexed_fns
                    .iter()
                    .map(|name| format!("{prefix}.{name}")),
            );
            entry.synthesized.extend(
                dep.synthesized
                    .iter()
                    .map(|name| format!("{prefix}.{name}")),
            );
            for (component, reason) in &dep.declined {
                entry
                    .declined
                    .insert(format!("{prefix}.({component})"), reason);
            }
        }
        entry.indexed_fns.sort();
        entry.synthesized.sort();
    }
}

/// The list-build half of the same fold: a collecting loop fused inside
/// a dependency is a rewrite the artifact carries, and one a dependency
/// DECLINED is the fact this diagnostic exists to surface.
fn merge_dep_list_build(diagnostics: &mut [aver::ir::PassDiagnostic], dep_reports: &[DepFusion]) {
    if dep_reports.is_empty() {
        return;
    }
    for diagnostic in diagnostics.iter_mut() {
        let aver::ir::PassReport::ListBuild(entry) = &mut diagnostic.report else {
            continue;
        };
        for DepFusion {
            prefix,
            list_build: dep,
            ..
        } in dep_reports
        {
            entry.rewrites += dep.rewrites;
            entry.byte_retargets += dep.byte_retargets;
            entry
                .synthesized
                .extend(dep.synthesized.iter().map(|n| format!("{prefix}.{n}")));
            entry
                .builder_fns
                .extend(dep.builder_fns.iter().map(|n| format!("{prefix}.{n}")));
            entry
                .byte_fns
                .extend(dep.byte_fns.iter().map(|n| format!("{prefix}.{n}")));
            for (fn_name, count) in &dep.rewrites_by_fn {
                *entry
                    .rewrites_by_fn
                    .entry(format!("{prefix}.{fn_name}"))
                    .or_default() += count;
            }
            for (fn_name, reason) in &dep.declined {
                entry.declined.insert(format!("{prefix}.{fn_name}"), reason);
            }
            for (driver, steps) in &dep.pair_inlined_by_fn {
                entry.pair_inlined_by_fn.insert(
                    format!("{prefix}.{driver}"),
                    steps.iter().map(|s| format!("{prefix}.{s}")).collect(),
                );
            }
            for (fn_name, reason) in &dep.pair_declined {
                entry
                    .pair_declined
                    .insert(format!("{prefix}.{fn_name}"), reason);
            }
            for (fn_name, reason) in &dep.byte_declined {
                entry
                    .byte_declined
                    .insert(format!("{prefix}.{fn_name}"), reason);
            }
        }
        entry.synthesized.sort();
        entry.builder_fns.sort();
        entry.byte_fns.sort();
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
    let mut items = match parse_file(&source, &module_root, file) {
        Ok(i) => i,
        Err(e) => {
            eprintln!("{}", e.red());
            process::exit(1);
        }
    };

    let dep_modules = load_compile_deps(&items, &module_root, DepLowering::PRISTINE);
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
                    out.push_str(&format!("  • {DEFORESTING_TARGETS_NOTE}\n"));
                }
            }
            PassReport::CharsFusion(r) => {
                if !r.fired() {
                    out.push_str(&format!(
                        "{label} no String.chars loop or single-character match rewritten\n"
                    ));
                } else {
                    out.push_str(&format!(
                        "{label} {} chars traversal(s) rewritten to a cursor, \
                         {} single-character match(es) rewritten to codepoints\n",
                        r.cursor_rewrites, r.codepoint_matches
                    ));
                    for fn_name in &r.synthesized {
                        out.push_str(&format!("  • synthesized {fn_name}\n"));
                    }
                    for (fn_name, count) in &r.codepoint_matches_by_fn {
                        out.push_str(&format!("  • {fn_name}: {count} codepoint match(es)\n"));
                    }
                    if r.codepoint_calls > 0 {
                        out.push_str(&format!(
                            "  • {} classifier call(s) receive the codepoint instead of a \
                             one-character string\n",
                            r.codepoint_calls
                        ));
                    }
                    out.push_str(&format!("  • {DEFORESTING_TARGETS_NOTE}\n"));
                }
                // Declines are reported whether or not anything fired:
                // a loop the recogniser stopped seeing is exactly the
                // regression this diagnostic exists to surface.
                for (fn_name, reason) in &r.declined {
                    out.push_str(&format!("  • declined {fn_name}: {reason}\n"));
                }
            }
            PassReport::StringIndex(r) => {
                if !r.fired() {
                    out.push_str(&format!(
                        "{label} no recursive String.charAt/String.slice component indexed\n"
                    ));
                } else {
                    out.push_str(&format!(
                        "{label} {} recursive String component(s) indexed, {} access site(s) rewritten\n",
                        r.components, r.indexed_accesses
                    ));
                    for fn_name in &r.synthesized {
                        out.push_str(&format!("  • synthesized {fn_name}\n"));
                    }
                    out.push_str(&format!("  • {STRING_INDEX_TARGETS_NOTE}\n"));
                }
                for (component, reason) in &r.declined {
                    out.push_str(&format!("  • declined {component}: {reason}\n"));
                }
            }
            PassReport::ListBuild(r) => {
                if r.rewrites == 0 {
                    out.push_str(&format!(
                        "{label} no collecting loop rewritten to a list builder\n"
                    ));
                } else {
                    out.push_str(&format!(
                        "{label} {} call site(s) moved onto a list builder, \
                         {} collected variant(s) synthesized\n",
                        r.rewrites,
                        r.synthesized.len()
                    ));
                    for (fn_name, count) in &r.rewrites_by_fn {
                        out.push_str(&format!("  • loop {fn_name}: {count} call site(s)\n"));
                    }
                    for fn_name in &r.synthesized {
                        out.push_str(&format!("  • synthesized {fn_name}\n"));
                    }
                    for fn_name in &r.byte_fns {
                        out.push_str(&format!(
                            "  • {fn_name} collects bytes; its fromList call is gone\n"
                        ));
                    }
                    out.push_str(&format!("  • {DEFORESTING_TARGETS_NOTE}\n"));
                }
                for (driver, steps) in &r.pair_inlined_by_fn {
                    out.push_str(&format!(
                        "  • {driver} absorbed its step fn(s) {} before candidacy\n",
                        steps.join(", ")
                    ));
                }
                // Same reason the chars-fusion declines are unconditional:
                // a loop the recogniser stopped seeing is what this
                // diagnostic exists to surface.
                for (fn_name, reason) in &r.declined {
                    out.push_str(&format!("  • declined {fn_name}: {reason}\n"));
                }
                for (fn_name, reason) in &r.pair_declined {
                    out.push_str(&format!(
                        "  • declined the step inline for {fn_name}: {reason}\n"
                    ));
                }
                for (fn_name, reason) in &r.byte_declined {
                    out.push_str(&format!(
                        "  • declined the byte retarget of {fn_name}: {reason}\n"
                    ));
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
                    "{{\"rewrites\":{},\"synthesized\":{},\"sinks\":{},\"rewrites_by_sink\":{},\"targets\":{}}}",
                    r.rewrites,
                    json_str_array(&r.synthesized),
                    json_str_array(&r.sink_fns),
                    by_sink,
                    DEFORESTING_TARGETS_JSON
                ));
            }
            PassReport::CharsFusion(r) => {
                let mut by_fn = String::from("{");
                for (j, (k, v)) in r.codepoint_matches_by_fn.iter().enumerate() {
                    if j > 0 {
                        by_fn.push(',');
                    }
                    by_fn.push_str(&format!("{}:{}", json_str(k), v));
                }
                by_fn.push('}');
                let mut declined = String::from("{");
                for (j, (k, v)) in r.declined.iter().enumerate() {
                    if j > 0 {
                        declined.push(',');
                    }
                    declined.push_str(&format!("{}:{}", json_str(k), json_str(v)));
                }
                declined.push('}');
                out.push_str(&format!(
                    "{{\"cursor_rewrites\":{},\"synthesized\":{},\"loop_fns\":{},\
                     \"codepoint_matches\":{},\"codepoint_matches_by_fn\":{},\
                     \"codepoint_calls\":{},\"declined\":{},\"targets\":{}}}",
                    r.cursor_rewrites,
                    json_str_array(&r.synthesized),
                    json_str_array(&r.loop_fns),
                    r.codepoint_matches,
                    by_fn,
                    r.codepoint_calls,
                    declined,
                    DEFORESTING_TARGETS_JSON
                ));
            }
            PassReport::StringIndex(r) => {
                let mut declined = String::from("{");
                for (j, (k, v)) in r.declined.iter().enumerate() {
                    if j > 0 {
                        declined.push(',');
                    }
                    declined.push_str(&format!("{}:{}", json_str(k), json_str(v)));
                }
                declined.push('}');
                out.push_str(&format!(
                    "{{\"components\":{},\"indexed_accesses\":{},\"indexed_fns\":{},\
                     \"synthesized\":{},\"declined\":{},\"targets\":{}}}",
                    r.components,
                    r.indexed_accesses,
                    json_str_array(&r.indexed_fns),
                    json_str_array(&r.synthesized),
                    declined,
                    STRING_INDEX_TARGETS_JSON
                ));
            }
            PassReport::ListBuild(r) => {
                let mut by_fn = String::from("{");
                for (j, (k, v)) in r.rewrites_by_fn.iter().enumerate() {
                    if j > 0 {
                        by_fn.push(',');
                    }
                    by_fn.push_str(&format!("{}:{}", json_str(k), v));
                }
                by_fn.push('}');
                let mut declined = String::from("{");
                for (j, (k, v)) in r.declined.iter().enumerate() {
                    if j > 0 {
                        declined.push(',');
                    }
                    declined.push_str(&format!("{}:{}", json_str(k), json_str(v)));
                }
                declined.push('}');
                let mut byte_declined = String::from("{");
                for (j, (k, v)) in r.byte_declined.iter().enumerate() {
                    if j > 0 {
                        byte_declined.push(',');
                    }
                    byte_declined.push_str(&format!("{}:{}", json_str(k), json_str(v)));
                }
                byte_declined.push('}');
                let mut pair_inlined = String::from("{");
                for (j, (k, v)) in r.pair_inlined_by_fn.iter().enumerate() {
                    if j > 0 {
                        pair_inlined.push(',');
                    }
                    pair_inlined.push_str(&format!("{}:{}", json_str(k), json_str_array(v)));
                }
                pair_inlined.push('}');
                let mut pair_declined = String::from("{");
                for (j, (k, v)) in r.pair_declined.iter().enumerate() {
                    if j > 0 {
                        pair_declined.push(',');
                    }
                    pair_declined.push_str(&format!("{}:{}", json_str(k), json_str(v)));
                }
                pair_declined.push('}');
                out.push_str(&format!(
                    "{{\"rewrites\":{},\"synthesized\":{},\"loop_fns\":{},\
                     \"rewrites_by_fn\":{},\"declined\":{},\
                     \"pair_inlined_by_fn\":{},\"pair_declined\":{},\
                     \"byte_retargets\":{},\
                     \"byte_fns\":{},\"byte_declined\":{},\"targets\":{}}}",
                    r.rewrites,
                    json_str_array(&r.synthesized),
                    json_str_array(&r.builder_fns),
                    by_fn,
                    declined,
                    pair_inlined,
                    pair_declined,
                    r.byte_retargets,
                    json_str_array(&r.byte_fns),
                    byte_declined,
                    DEFORESTING_TARGETS_JSON
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

/// Flag-level gate for `--certify`: `Some(error)` when this binary was built
/// without the certificate engine (feature `certify`, implied by `--features
/// wasm`), `None` when the flag can proceed (or was not passed). Uses `cfg!`
/// rather than an `#[cfg]`-gated block so the decision compiles — and is
/// unit-testable — under every feature combination, including the default
/// certify-less build.
fn certify_flag_rejection(certify: bool) -> Option<&'static str> {
    if certify && !cfg!(feature = "certify") {
        Some(
            "--certify: this build of aver was compiled without certificate support \
             (feature `certify`; rebuild with: cargo build --features wasm)",
        )
    } else {
        None
    }
}

pub(super) fn capability_target_rejection(
    items: &[TopLevel],
    modules: &[ModuleInfo],
    registry: &aver::capability::CapabilityRegistry,
    target: aver::provider::CapabilityTarget,
) -> Option<String> {
    let required = aver::provider::required_capability_operations(items, modules, registry);
    let manifest = aver::provider::CapabilityTargetManifest::build(registry, &required)
        .expect("required operations came from the capability registry");
    let errors = manifest
        .required_unsupported(target)
        .map(|row| {
            let aver::provider::TargetBindingStatus::Unsupported { reason } = &row.status else {
                unreachable!("required_unsupported returns only unsupported rows")
            };
            format!(
                "error[capability-target-unsupported]: target `{}` cannot bind capability `{}`\n  reason[{}]: {}\n  required operations: {}\n  contract_hash: {}\n  model_hash: {}",
                target,
                row.capability,
                reason.code(),
                reason.description(),
                row.required_operations
                    .iter()
                    .cloned()
                    .collect::<Vec<_>>()
                    .join(", "),
                row.contract_hash,
                row.model_hash,
            )
        })
        .collect::<Vec<_>>();
    (!errors.is_empty()).then(|| errors.join("\n\n"))
}

fn print_capability_target_accounting(
    items: &[TopLevel],
    modules: &[ModuleInfo],
    registry: &aver::capability::CapabilityRegistry,
    target: aver::provider::CapabilityTarget,
) {
    let required = aver::provider::required_capability_operations(items, modules, registry);
    let manifest = aver::provider::CapabilityTargetManifest::build(registry, &required)
        .expect("required operations came from the capability registry");
    for row in manifest.for_target(target) {
        if !row.is_required() {
            continue;
        }
        match &row.status {
            aver::provider::TargetBindingStatus::Provided(provider) => println!(
                "  capability {}: provided by {}@{} | contract_hash={} | model_hash={}",
                row.capability,
                provider.identity,
                provider.fingerprint,
                row.contract_hash,
                row.model_hash
            ),
            aver::provider::TargetBindingStatus::HostBound { reason } => println!(
                "  capability {}: host-bound[{}] | contract_hash={} | model_hash={}",
                row.capability,
                reason.code(),
                row.contract_hash,
                row.model_hash
            ),
            aver::provider::TargetBindingStatus::Unsupported { reason } => println!(
                "  capability {}: unsupported[{}] | contract_hash={} | model_hash={}",
                row.capability,
                reason.code(),
                row.contract_hash,
                row.model_hash
            ),
        }
    }
}

fn reject_unsupported_capability_targets(
    items: &[TopLevel],
    modules: &[ModuleInfo],
    registry: &aver::capability::CapabilityRegistry,
    target: aver::provider::CapabilityTarget,
) {
    if let Some(error) = capability_target_rejection(items, modules, registry, target) {
        eprintln!("{}", error.red());
        process::exit(1);
    }
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
        certify,
        packed_sequences_enabled,
    } = opts;

    // `--certify` needs the certificate engine (feature `certify`, part of
    // `--features wasm`). Reject the flag before any target dispatch, so a
    // binary built without the engine (default, `wasip2`-only, playground-
    // style builds) fails with a clean flag-level error instead of a
    // target-level one — or, worse, half a certificate.
    if let Some(error) = certify_flag_rejection(certify) {
        eprintln!("{}", error.red());
        process::exit(1);
    }

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
                certify,
                packed_sequences_enabled,
            );
            return;
        }
        #[cfg(not(feature = "wasm"))]
        {
            let _ = (handler, optimize, pack, certify, packed_sequences_enabled);
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

    let (mut ctx, module_root) = build_codegen_context(
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
    reject_unsupported_capability_targets(
        &ctx.items,
        &ctx.modules,
        &ctx.capabilities,
        aver::provider::CapabilityTarget::Rust,
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
    let project_config = match aver::config::ProjectConfig::load_from_dir(Path::new(&module_root)) {
        Ok(config) => config,
        Err(error) => {
            eprintln!("{}", error.red());
            process::exit(1);
        }
    };
    let provider_manifest = project_config
        .as_ref()
        .and_then(|config| config.provider_manifest.as_ref());
    let known_provider_capabilities = super::provider_host_cmd::known_project_capabilities(
        &module_root,
        &ctx.capabilities,
        provider_manifest,
    );
    let output = match rust_codegen::transpile_with_provider_manifest_for_project(
        &mut ctx,
        provider_manifest,
        &known_provider_capabilities,
    ) {
        Ok(output) => output,
        Err(error) => {
            eprintln!("{}", error.red());
            process::exit(1);
        }
    };
    let build_hint = format!("cd {} && cargo build && cargo run", output_dir);
    write_codegen_output(file, output_dir, "Rust", &build_hint, &output);
    print_capability_target_accounting(
        &ctx.items,
        &ctx.modules,
        &ctx.capabilities,
        aver::provider::CapabilityTarget::Rust,
    );
}

/// `aver compile FILE --target=wasm-gc` — 0.16 probe backend.
/// Type-direct lowering, no custom runtime, native tail calls. Phase-1:
/// only `fn main() -> Int <int_literal>` compiles; everything else
/// surfaces an `Unimplemented` error pointing at the relevant phase
/// in the README.
#[cfg(feature = "wasm")]
#[allow(clippy::too_many_arguments)]
fn cmd_compile_wasm_gc(
    file: &str,
    output_dir: &str,
    project_name: Option<&str>,
    module_root_override: Option<&str>,
    handler: Option<&str>,
    optimize: Option<super::cli::WasmOptMode>,
    pack: Option<super::cli::DeployPack>,
    certify: bool,
    packed_sequences_enabled: bool,
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
    let mut items = match parse_file(&source, &module_root, file) {
        Ok(i) => i,
        Err(e) => {
            eprintln!("{}", e.red());
            process::exit(1);
        }
    };

    // Standard capabilities such as Time are source modules even though their
    // calls remain implicitly visible. Preload them before symbol/MIR building
    // so the target sees a real capability callee rather than an unresolved
    // call that lowers to a trap.
    let dep_modules = load_compile_deps(&items, &module_root, DepLowering::STRING_INDEX_ONLY);
    use aver::ir::{PipelineConfig, TypecheckMode};
    let neutral_policy = aver::ir::NeutralAllocPolicy;
    let result = aver::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full {
                base_dir: Some(&module_root),
            }),
            alloc_policy: Some(&neutral_policy),
            dep_modules: &dep_modules,
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
            run_chars_fusion: false,
            run_string_index: true,
            run_list_build: false,
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
    // The wasm-gc compile path lowers the immutable String index, but
    // neither the buffers nor the cursors/builders introduced by the
    // older passes. It does not use the self-host typecheck driver.
    reject_unsupported_capability_targets(
        &items,
        &dep_modules,
        &result
            .typecheck
            .as_ref()
            .expect("wasm-gc pipeline requested typechecking")
            .capabilities,
        aver::provider::CapabilityTarget::WasmGc,
    );
    let type_aliases = flatten_multimodule(
        &mut items,
        &dep_modules,
        &result
            .typecheck
            .as_ref()
            .expect("wasm-gc pipeline requested typechecking")
            .capabilities,
    );
    // Re-run resolver after flatten so dep fns get a FnResolution
    // (slot_types). Entry items already had one from `pipeline::run`
    // above; this picks up the newly appended dep FnDefs. The
    // `_and_reannotate` half is load-bearing: a bare re-resolve wipes
    // `aliased_slots` and the wasm-gc in-place fast path then mutates
    // container-held collections through extracted locals (#950).
    aver::ir::pipeline::resolve_and_reannotate(&mut items);

    let wasm_gc_output = match wasm_gc::compile_to_wasm_gc_flattened_with_options(
        &items,
        result.analysis.as_ref(),
        handler,
        wasm_gc::TargetMode::AverBridge,
        &type_aliases,
        packed_sequences_enabled,
    ) {
        Ok(output) => output,
        Err(e) => {
            eprintln!("{}", format!("{e}").red());
            process::exit(1);
        }
    };
    let bytes = wasm_gc_output.bytes;

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
    print_capability_target_accounting(
        &items,
        &dep_modules,
        &result
            .typecheck
            .as_ref()
            .expect("wasm-gc pipeline requested typechecking")
            .capabilities,
        aver::provider::CapabilityTarget::WasmGc,
    );
    // `--certify`: emit the artifact-certificate `cert/` project next to
    // the module. Binds THESE bytes (pre-optimize; `--certify` conflicts
    // with `--optimize`) via sha256, classifies each user function, and
    // emits kernel-clean Lean theorems for the certified ones. The model
    // definitions are the reused `aver proof` Lean emission.
    #[cfg(feature = "certify")]
    if certify
        && let Err(error) = emit_artifact_certificate(
            file,
            project_name,
            module_root_override,
            out_path,
            &wasm_name,
            &bytes,
            &wasm_gc_output.fragment_plans,
        )
    {
        eprintln!("{}", format!("certificate: {error}").red());
        process::exit(1);
    }
    // Deployment pack — drops platform-specific bootstrap files
    // next to the wasm-gc artifact. Same call site as the legacy
    // backend; the worker.js template is wasm-gc-aware (LM string
    // transport + `aver_http_handle` synth wrapper).
    if let Some(super::cli::DeployPack::Cloudflare) = pack {
        emit_cloudflare_pack(out_path, &wasm_name, &wasm_file);
    }
}

/// Emit the Stage-B artifact certificate: classify the emitted module,
/// reuse the `aver proof` Lean model emission, and write `cert/`.
///
/// Gated on `certify` (the aver-cert producer engine + `codegen::cert`) AND
/// `wasm` (the sole caller, `cmd_compile_wasm_gc`), so a bare
/// `--features certify` build compiles without an uncallable function.
#[cfg(all(feature = "certify", feature = "wasm"))]
fn emit_artifact_certificate(
    file: &str,
    project_name: Option<&str>,
    module_root_override: Option<&str>,
    out_path: &Path,
    wasm_name: &str,
    bytes: &[u8],
    fragment_plans: &[aver::codegen::cert::FragmentPlanArtifact],
) -> Result<(), String> {
    use aver::codegen::cert;

    // Reuse the `aver proof` Lean model emission for the model definitions.
    // Built before `analyze` so the recursion classifier can read the combinator
    // operator (`+`/`*`) from the model.
    let (mut mctx, _mroot) = build_codegen_context(
        file,
        project_name,
        module_root_override,
        false,
        &super::cli::CompilePolicyMode::Embed,
        None,
        false,
        false, // apply_traversal_lowering — model wants source-level IR
        true,  // run_refinement_lower
        true,  // run_contract_lower
        true,  // run_law_lower
    );
    let model_out = lean_codegen::transpile_for_cert_model(&mut mctx);

    let analysis = cert::analyze_with_fragment_plans(bytes, &model_out.files, fragment_plans)?;

    cert::write_project(out_path, wasm_name, bytes, &analysis, &model_out.files)?;

    let cert_dir = out_path.join("cert");
    let certified = analysis.certified_names();
    println!(
        "{} certificate → {}/ ({} certified, {} source-level-only)",
        "•".cyan(),
        cert_dir.display().to_string().cyan(),
        certified.len(),
        analysis.declined().len(),
    );
    if !certified.is_empty() {
        println!("    certified: {}", certified.join(", "));
    }
    println!(
        "    verify: aver cert verify {} {}",
        out_path.join(format!("{wasm_name}.wasm")).display(),
        cert_dir.display()
    );
    Ok(())
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
        let mut items = match parse_file(&source, &module_root, file) {
            Ok(i) => i,
            Err(e) => {
                eprintln!("{}", e.red());
                process::exit(1);
            }
        };

        let dep_modules = load_compile_deps(&items, &module_root, DepLowering::STRING_INDEX_ONLY);
        use aver::ir::{PipelineConfig, TypecheckMode};
        let neutral_policy = aver::ir::NeutralAllocPolicy;
        let result = aver::ir::pipeline::run(
            &mut items,
            PipelineConfig {
                typecheck: Some(TypecheckMode::Full {
                    base_dir: Some(&module_root),
                }),
                alloc_policy: Some(&neutral_policy),
                dep_modules: &dep_modules,
                run_interp_lower: false,
                run_buffer_build: false,
                run_chars_fusion: false,
                run_string_index: true,
                run_list_build: false,
                ..Default::default()
            },
        );
        if let Some(tc) = &result.typecheck
            && !tc.errors.is_empty()
        {
            eprintln!("{}", super::shared::format_type_errors(&tc.errors).red());
            process::exit(1);
        }

        reject_unsupported_capability_targets(
            &items,
            &dep_modules,
            &result
                .typecheck
                .as_ref()
                .expect("wasip2 pipeline requested typechecking")
                .capabilities,
            aver::provider::CapabilityTarget::Wasip2,
        );
        let capabilities = &result
            .typecheck
            .as_ref()
            .expect("wasip2 pipeline requested typechecking")
            .capabilities;
        let required =
            aver::provider::required_capability_operations(&items, &dep_modules, capabilities);
        let project_config = match load_runtime_policy(&module_root) {
            Ok(config) => config,
            Err(error) => {
                eprintln!("{}", error.red());
                process::exit(1);
            }
        };
        if let Some(warning) = crate::cli_entry::shared::wasip2_tcp_timeout_warning(
            "--target wasip2",
            &required,
            project_config.as_ref(),
        ) {
            eprintln!("{}", warning.yellow());
        }
        let capability_wit_plan =
            aver::codegen::wasip2::CapabilityWitPlan::build(capabilities, &required)
                .unwrap_or_else(|unsupported| {
                    eprintln!(
                        "{}",
                        format!(
                            "error[wit-boundary-type-unsupported]: {}",
                            unsupported.description()
                        )
                        .red()
                    );
                    process::exit(1);
                });
        // Bypass the `flatten_multimodule` shim in this file (gated on
        // the `wasm` feature) and call the wasm-gc library function
        // directly — `wasip2` enables `wasm-compile` (which exposes
        // it) but does not pull `wasm`.
        let type_aliases = aver::codegen::wasm_gc::flatten_multimodule(
            &mut items,
            &dep_modules,
            capabilities,
            aver::codegen::wasm_gc::CapabilityFunctionSurface::Runtime,
        );
        // `_and_reannotate`: same #950 wipe-guard as the wasm-gc
        // compile path — a bare re-resolve wipes `aliased_slots`.
        aver::ir::pipeline::resolve_and_reannotate(&mut items);

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
            match wasm_gc::compile_to_wasm_gc_flattened_with_capabilities(
                &items,
                result.analysis.as_ref(),
                Some(handler_name),
                &type_aliases,
                &capability_wit_plan,
            ) {
                Ok(out) => out.bytes,
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
            match wasm_gc::compile_to_wasm_gc_flattened_with_capabilities(
                &items,
                result.analysis.as_ref(),
                None,
                &type_aliases,
                &capability_wit_plan,
            ) {
                Ok(out) => out.bytes,
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
            match wasip2_codegen::compile_to_component_with_capabilities(
                &core_bytes,
                world_codegen,
                &capability_wit_plan,
            ) {
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
        print_capability_target_accounting(
            &items,
            &dep_modules,
            &result
                .typecheck
                .as_ref()
                .expect("wasip2 pipeline requested typechecking")
                .capabilities,
            aver::provider::CapabilityTarget::Wasip2,
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
///
/// One of those names is load-bearing, not decoration: the map insert
/// helpers carry a resize-bug backstop trap, and its name is the only
/// diagnostic a wasm trap can carry (a trap has no message of its own). The
/// optimize pass costs the program that name twice over — it drops the
/// section, and `-Oz` / `-O3` inline a sole-call-site helper into its
/// caller, so `-g` would only hand back an empty name map. Nothing here
/// can preserve it, so the build says out loud what the artifact lost.
#[cfg(feature = "wasm")]
fn finalize_wasm_artifact(
    wasm_file: &Path,
    optimize: Option<super::cli::WasmOptMode>,
) -> (u64, String) {
    let mut final_size = std::fs::metadata(wasm_file).map(|m| m.len()).unwrap_or(0);
    let mut compile_suffix = String::new();
    if let Some(mode) = optimize {
        warn_optimize_drops_capacity_names(wasm_file);
        final_size = run_optimize_pipeline(wasm_file, mode).unwrap_or_else(|err| {
            eprintln!("{}", err.red());
            process::exit(1);
        });
        compile_suffix = format!(", optimized for {}", optimize_label(mode));
    }
    (final_size, compile_suffix)
}

/// Say on stderr that the artifact about to be optimized carries the map
/// insert-helper names, and will not carry them afterwards. Silent for
/// a program that instantiates no map — the emitter writes the names
/// only for those, so their presence is the exact test.
#[cfg(feature = "wasm")]
fn warn_optimize_drops_capacity_names(wasm_file: &Path) {
    let Ok(bytes) = std::fs::read(wasm_file) else {
        return;
    };
    if !aver::codegen::wasm_gc::carries_capacity_helper_names(&bytes) {
        return;
    }
    eprintln!(
        "{} this program uses a Map. Its wasm-gc table grows on demand, \
         so an insert has no size limit to hit — but if one ever stops, \
         the helper's name in an un-optimized build says which map it \
         was. `--optimize` drops the name section and inlines the \
         helper, so the same stop prints `<wasm function N>` here. \
         Build without `--optimize` to read the backtrace.",
        "note:".yellow()
    );
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
    // section — `warn_optimize_drops_capacity_names` says so when that
    // costs the map capacity trap its name; --strip-producers and
    // --strip-target-features remove sections that survive otherwise
    // and bloat merged artifacts.
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
        // The bignum f64->Int helper emits `i64.trunc_sat_f64_u` (exact integer
        // over the full f64 range); `--strip-target-features` above drops the
        // feature section, so wasm-opt must be told this proposal is allowed or
        // it rejects the input. Every wasm-gc target engine (Chrome 119+ / FF
        // 120+ / Safari 18.2+ / wasmtime / Node 22+) supports it.
        .arg("--enable-nontrapping-float-to-int")
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
    pub(super) certify: bool,
    pub(super) packed_sequences_enabled: bool,
}

/// Load hand-proof SIDECARS for `file`'s project and `backend` into the
/// `(fn, law) -> body` map the codegen splices. The sidecar dir is found by
/// ascending from the `.av` file's directory to the FIRST ancestor that has a
/// `proofs/<lean|dafny>/` subdir (so it works whether the entry is a project's
/// `main.av` or a `domain/<mod>.av`); the ascent is bounded so it never wanders
/// past the project. Each `<fn>__<law>.{lean,dfy}` file's contents become the
/// proof body for `(fn, law)`. No dir / no match => empty map => the auto path,
/// byte-identical to before. Keeping the `.av` sources pure spec, the persistent
/// hand proofs live ONLY here and are re-spliced + kernel-re-checked every run.
fn load_hand_proofs(
    file: &str,
    backend: &super::cli::ProofBackend,
) -> std::collections::HashMap<(String, String), String> {
    use std::path::Path;
    let (subdir, ext) = match backend {
        super::cli::ProofBackend::Lean => ("lean", ".lean"),
        super::cli::ProofBackend::Dafny => ("dafny", ".dfy"),
    };
    let mut out = std::collections::HashMap::new();
    let mut cur = Path::new(file).parent();
    let mut hops = 0;
    while let Some(dir) = cur {
        let proofs = dir.join("proofs").join(subdir);
        if proofs.is_dir() {
            if let Ok(rd) = std::fs::read_dir(&proofs) {
                for entry in rd.flatten() {
                    let name = entry.file_name().to_string_lossy().into_owned();
                    let Some(stem) = name.strip_suffix(ext) else {
                        continue;
                    };
                    let Some((fn_name, law_name)) = stem.split_once("__") else {
                        continue;
                    };
                    if let Ok(body) = std::fs::read_to_string(entry.path()) {
                        out.insert((fn_name.to_string(), law_name.to_string()), body);
                    }
                }
            }
            break; // first proofs/ dir up the chain wins
        }
        cur = dir.parent();
        hops += 1;
        if hops > 8 {
            break;
        }
    }
    out
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
    declined_budget: Option<usize>,
    check_json: bool,
    explain: bool,
    // Reserved for the `--minimize` proof-output pass (collapse each `first |
    // … | sorry` portfolio to its winning branch on the Tactic IR). Plumbed
    // through now; consumed once the IR migration lands.
    minimize: bool,
    // `--allow-mathlib` (Lean-only, opt-in): permit the generic Mathlib break-
    // glass arm on walling entry-module `when`-laws and wire the cached Mathlib
    // into the generated lake project. OFF → byte-identical to today.
    allow_mathlib: bool,
    gate: Option<&str>,
    write_baseline: Option<&str>,
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

    // `--allow-mathlib` is Lean-only. On Dafny it is a no-op (Z3 already carries
    // the nonlinear-floor lemmas natively, so there is no break-glass tier) —
    // warn and proceed with the unchanged Dafny path.
    if allow_mathlib && matches!(backend, super::cli::ProofBackend::Dafny) {
        eprintln!(
            "{}",
            "--allow-mathlib applies to the Lean backend only; ignored for Dafny".yellow()
        );
    }
    let allow_mathlib = allow_mathlib && matches!(backend, super::cli::ProofBackend::Lean);
    ctx.allow_mathlib = allow_mathlib;

    // Hand-proof SIDECAR tier (both backends): load any source-controlled
    // `proofs/<lean|dafny>/<fn>__<law>.{lean,dfy}` proof bodies for this file's
    // project + backend into the codegen's splice map. When a law has a sidecar
    // the codegen emits its theorem/lemma with that body and the kernel
    // (lake / dafny verify) re-checks it; absent a sidecar the auto path is
    // byte-identical. The `.av` source stays pure spec — proofs live only here.
    ctx.hand_proofs = load_hand_proofs(file, backend);

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
            let ground_truth = collect_verify_ground_truth(file, &module_root);
            ctx.sample_expected = ground_truth.expected;
            ctx.declined_cases = ground_truth.declined;
            cmd_proof_lean(file, output_dir, &mut ctx, verify_mode);
            // Under `--allow-mathlib` the speculative/minimize re-emit passes are
            // SKIPPED: they run their own `lake build` probes that would choke on
            // the not-yet-wired `aver_mathlib` macro (the Mathlib import + macro
            // are injected by `setup_mathlib_for_project` AFTER the final emit, and
            // a re-emit would clobber them). The break-glass arm already promotes a
            // walling `when`-law to its true-universal form directly, so neither
            // pass is needed on the opt-in tier.
            if !allow_mathlib {
                // Speculative-universal: a SINGLE-LIST conditional law cannot be
                // statically classified as universal-closeable, so try each
                // universally in one probe build and re-emit with the ones that
                // CLOSED stated universally and the rest on their bounded fallback
                // (try-universal, fall-back-to-sampled — analog of `--minimize` for
                // the statement form). No-op when the file has no such candidate.
                run_lean_speculative(file, output_dir, &mut ctx, verify_mode);
                // `--minimize`: learn each portfolio's winning branch from one
                // instrumented build, then re-emit collapsed (fail-safe — restores
                // the normal proof if the collapsed project does not build).
                if minimize {
                    run_lean_minimize(file, output_dir, &mut ctx, verify_mode);
                }
            }
            // `--allow-mathlib`: wire the prebuilt Mathlib cache into the generated
            // lake project — add `require mathlib` + reuse the cached packages, and
            // inject `import Mathlib` + the `aver_mathlib` macro into the entry
            // file(s) that actually use the break-glass arm. Must run AFTER the
            // final emit (no re-emit follows). Exits non-zero on a misconfigured
            // cache so the opt-in failure is loud, never a silent core fallback.
            if allow_mathlib {
                setup_mathlib_for_project(output_dir);
            }
        }
        super::cli::ProofBackend::Dafny => {
            if minimize {
                eprintln!(
                    "{}",
                    "--minimize applies to the Lean backend only; ignored for Dafny".yellow()
                );
            }
            cmd_proof_dafny(file, output_dir, &ctx);
        }
    }

    // Claims the exporter would not state, collected during emission. Read out
    // of the context BEFORE the check harness runs, so both the console report
    // and the charge below see the same list.
    let declined: Vec<aver::codegen::DeclinedClaim> =
        ctx.declined_claims.borrow().values().cloned().collect();
    // Report on stdout whether or not `--check` was asked for. `aver proof`
    // already tells the user what it compiled; a claim it silently dropped on
    // the floor belongs in the same breath — that is the guarantee the
    // CHANGELOG advertises ("refused … with a message saying why"), and until
    // now the message existed only inside a generated file nobody opens.
    // Suppressed under `--check-json`, whose contract is that stdout is one
    // JSON object; the same list travels in the `declined_claims` key there.
    if !declined.is_empty() && !check_json {
        println!(
            "{}",
            format!(
                "  {} claim(s) declined — not exported, so nothing was proved about them:",
                declined.len()
            )
            .yellow()
        );
        for d in &declined {
            println!(
                "{}",
                format!("    {} {} — {}", d.kind.as_str(), d.claim, d.reason).yellow()
            );
        }
    }

    // `--check-json` is the machine-readable form of `--check` and IMPLIES it;
    // `--gate` / `--write-baseline` also imply a verifier run (they recompute
    // the current manifest). So run the check harness when ANY of these is set
    // even without an explicit `--check`.
    if check || check_json || gate.is_some() || write_baseline.is_some() {
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
        let duplicate_laws = duplicate_program_law_identities(&ctx);
        // `fn.law` identities that had a hand-proof sidecar spliced for this
        // backend — the credit channel (a spliced law that reaches Universal
        // tier is credited `hand`, else `open`; fail-closed). Derived from the
        // loaded sidecar map keys, so empty when no sidecar exists.
        let hand_laws: std::collections::HashSet<String> = ctx
            .hand_proofs
            .keys()
            .map(|(f, l)| format!("{f}.{l}"))
            .collect();
        run_proof_check(
            output_dir,
            backend,
            error_budget,
            sorry_budget,
            declined_budget,
            check_json,
            explain,
            allow_mathlib,
            dafny_entry,
            gate,
            write_baseline,
            &duplicate_laws,
            &hand_laws,
            &declined,
            &ctx.items,
            file,
            &module_root,
        );
    }
}

/// Run the Declared-mode VM verify pass over every project module reached by
/// `file` and build
/// the ground-truth table for `CodegenContext::sample_expected`: for every
/// case that PASSES, the VM-computed expected (right-side) value, rendered
/// with `aver_repr_literal`, keyed by
/// `(module_scope, verify_block_counter_key, module_local_case_index)`.
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
/// A module that cannot run contributes no entries; successfully verified
/// modules still keep their ground truth. Emission falls back to the source
/// RHS for every miss, as before.
fn collect_verify_ground_truth(file: &str, module_root: &str) -> VerifyGroundTruth {
    use aver::checker::{VerifyCaseOutcome, merge_verify_blocks};

    let mut out = VerifyGroundTruth::default();
    let mut reported = HashSet::new();
    let Ok(units) = collect_program_units(file, module_root, &mut reported) else {
        return out;
    };
    let config = match load_runtime_policy(module_root) {
        Ok(c) => c,
        Err(_) => return out,
    };
    let unit_count = units.len();
    for (unit_index, (path, _source, items)) in units.into_iter().enumerate() {
        let merged = merge_verify_blocks(&items);
        if merged.is_empty() {
            continue;
        }
        let scope = if unit_index + 1 == unit_count {
            None
        } else {
            aver::visibility::module_decl(&items).map(|module| module.name.clone())
        };
        let results = match aver::diagnostics::vm_verify::run_verify_for_items_vm(
            items,
            config.clone(),
            Some(module_root),
            &path,
        ) {
            Ok(results) if results.len() == merged.len() => results,
            _ => continue,
        };

        let mut counters: HashMap<String, usize> = HashMap::new();
        for (block, result) in merged.iter().zip(&results) {
            let block_key = aver::codegen::common::verify_block_counter_key(block);
            let base = *counters.get(&block_key).unwrap_or(&0);
            counters.insert(block_key.clone(), base + block.cases.len());
            if block.trace {
                continue;
            }
            for cr in &result.case_results {
                let key = (scope.clone(), block_key.clone(), base + cr.case_index);
                // Exhaustive on purpose. A decline is not an absent value: the
                // emitter must refuse the theorem rather than fall back to the
                // author's expected expression.
                match &cr.outcome {
                    VerifyCaseOutcome::Pass => {}
                    VerifyCaseOutcome::Declined { reason, .. } => {
                        out.declined.insert(key, reason.clone());
                        continue;
                    }
                    VerifyCaseOutcome::Skipped
                    | VerifyCaseOutcome::SkippedAfterBaseFail
                    | VerifyCaseOutcome::Mismatch { .. }
                    | VerifyCaseOutcome::RuntimeError { .. }
                    | VerifyCaseOutcome::UnexpectedErr { .. } => continue,
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
                out.expected
                    .insert(key, aver::value::aver_repr_literal(value));
            }
        }
    }
    out
}

/// What a Declared-mode verify pass tells the Lean emitter about each case:
/// the value it observed, or the reason it observed nothing.
#[derive(Default)]
struct VerifyGroundTruth {
    /// Cases that passed, with their VM-computed expected value.
    expected: std::collections::HashMap<aver::codegen::VerifyCaseKey, String>,
    /// Cases that were declined, with the reason.
    declined: std::collections::HashMap<aver::codegen::VerifyCaseKey, String>,
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
    // `--declined-budget`: how many claims the exporter may refuse to state
    // before the check fails. Its own pot, never folded into `sorry_budget` —
    // a budget granted for an open induction must not silently license a
    // refusal. Defaults to 0.
    declined_budget: Option<usize>,
    check_json: bool,
    // `--explain` (Lean-only): after the counted build + audit succeed, run an
    // ISOLATED, fail-soft residual probe per OPEN law and populate each
    // `ManifestLaw.open_goal` with the law's `unsolved goals` text (and, with
    // `--check-json`, surface them inline as a top-level `open_goals` object).
    // Off by default; never touches `passed` / exit code / the counted build.
    explain: bool,
    // `--allow-mathlib` (Lean-only): after the manifest is built, tag each law's
    // `credit` field (`core` / `mathlib` / `open`) from the build-log
    // `AVER_MATHLIB:fn.law` trace markers + the law's tier. Off → no `credit`
    // key is written, so the manifest stays byte-identical.
    allow_mathlib: bool,
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
    // `fn.law` identities that had a hand-proof sidecar spliced for this
    // backend. After the manifest is built, each such law is tagged `credit:
    // hand` if it reached Universal tier (the spliced proof kernel-verified) or
    // `credit: open` if not (a wrong/stale sidecar failed the build — fail-
    // closed, never `hand`). Empty => no `credit` key written => byte-identical.
    hand_laws: &std::collections::HashSet<String>,
    // Claims codegen REFUSED to state (see `CodegenContext::declined_claims`).
    // Charged against `declined_budget` below and recorded in the manifest, so
    // a claim that moves from proved to declined is a demotion the ratchet
    // sees rather than a law that quietly ceases to exist.
    declined: &[aver::codegen::DeclinedClaim],
    // `--explain` candidate-law renderer inputs (Lean-only, console): the parsed
    // source items + file + module root, used to un-translate each open law's
    // residual back into an Aver candidate and gate it through the VM sample-
    // check. Read ONLY on the `explain && !check_json` console path; never
    // touches manifest/json bytes, tiers, `passed`, or the exit code.
    source_items: &[aver::ast::TopLevel],
    source_file: &str,
    source_module_root: &str,
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
    // A DECLINED claim is a third failure mode, and the one no other counter
    // can see: the claim never reached the backend, so there is no error to
    // count and no `sorry` to catch. Charge it on BOTH backends.
    //
    // Charging is what makes it safe to widen a refusal. Widening moves a
    // claim out of `build_errors` / `sorries` and into `declined`; if
    // `declined` is free, the same widening turns a RED check GREEN and the
    // regression signal vanishes exactly when it is needed. Charged, the exit
    // code cannot improve. Printing alone does not close this — CI reads the
    // exit code, not stdout.
    //
    // Dafny already had this discipline for its own refusal ("universal lemma
    // omitted", counted and charged below); Lean simply had no slot for it.
    let declined_budget_v = declined_budget.unwrap_or(0);
    let declined_count = declined.len();
    let declined_within_budget = declined_count <= declined_budget_v;
    // Lean only: model panic lines in the captured build output. The emitted
    // exports panic only at compiler-generated sites (fuel-wrapper
    // exhaustion or partial prelude builtins
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
            let passed = output.status.success()
                && errors <= error_budget_v
                && unproven <= sorry_budget_v
                && declined_within_budget;
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
            let passed = output.status.success()
                && sorries <= sorry_budget_v
                && model_panic_hits == 0
                && declined_within_budget;
            (None, Some(sorries), None, None, sorry_budget_v, passed)
        }
    };
    if !declined_within_budget {
        // Headline only: the per-claim list already went to stdout above (and
        // under `--check-json` it travels in `declined_claims`), so repeating
        // it here would print the same four sentences twice.
        eprintln!(
            "{}",
            format!(
                "--check: {} claim(s) were declined, not exported and therefore never proved \
                 (budget {}). Fix the claim, or acknowledge the refusal with --declined-budget {}.",
                declined_count, declined_budget_v, declined_count,
            )
            .red()
        );
    }

    // Additive check-json telemetry, per backend — informational only, NEVER
    // folded into `passed` or the exit code (the floor still degrades to a
    // caught `sorry` / the budgets still gate). `build_errors` surfaces a hard
    // Lean error that `sorries` hides; `timeouts` surfaces Dafny timeouts the
    // `errors` count is blind to.
    let (lean_build_errors, dafny_timeouts): (Option<usize>, Option<usize>) = match backend {
        super::cli::ProofBackend::Lean => (
            Some(count_lean_build_errors(&stderr) + count_lean_build_errors(&stdout)),
            None,
        ),
        super::cli::ProofBackend::Dafny => (None, Some(count_dafny_timeouts(&stdout))),
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

    // Which law(s) actually FAILED in the gate build (Lean only): map each
    // `declaration uses 'sorry'` warning back to the `fn.law` identity of the
    // theorem that carries it, via the emitted `-- aver:law-class` markers. This
    // is the machine-readable answer to "which law failed?" — previously the user
    // had to `lake build` the generated project by hand and grep the sorry
    // warning's line number against the emitted theorems. Empty for a clean build
    // or on Dafny. Also anchors `--explain`'s residual attribution below.
    let sorry_laws: Vec<String> = match backend {
        super::cli::ProofBackend::Lean if sorries.unwrap_or(0) > 0 => {
            lean_sorry_laws(output_dir, &format!("{stdout}{stderr}"))
        }
        _ => Vec::new(),
    };

    // Proof manifest (Lean only): the file-level audit's per-law records as one
    // byte-reproducible per-law table, written to `<out>/proof_manifest.json`.
    // This is the artifact `--gate` diffs against a committed baseline; it
    // reuses the SAME class markers + `#print axioms` verdicts already computed
    // above (no extra lake invocation).
    //
    // `--explain` (Lean only, opt-in, fail-soft): BEFORE writing the manifest,
    // run an isolated residual probe over the laws that did NOT close
    // universally (tier Failed / Bounded) and merge each law's `unsolved goals`
    // text onto its `open_goal` by `fn.law` identity. Strictly additive: a law
    // with no residual found stays `open_goal: None`, and with `--explain`
    // absent the probe never runs, so the written bytes are unchanged. The probe
    // is gated on the COUNTED build having succeeded (no audit otherwise), and
    // its own outcome can never touch `passed` / `universal` / the exit code.
    let mut manifest: Option<ProofManifest> = lean_law_audit
        .as_ref()
        .map(|audit| build_proof_manifest(&audit.laws, declined));
    let mut open_goals: std::collections::BTreeMap<String, String> =
        std::collections::BTreeMap::new();
    // `--explain` residuals borrowed from HEALTHY (proven) laws — see the
    // attribution split below. Kept separate from `open_goals` so a proven law's
    // probe artifact is never mistaken for the failing law.
    let mut probe_of: std::collections::BTreeMap<String, String> =
        std::collections::BTreeMap::new();
    if explain
        && matches!(backend, super::cli::ProofBackend::Lean)
        && output.status.success()
        && model_panic_hits == 0
        && let Some(m) = manifest.as_mut()
    {
        // The OPEN laws to probe = every emitted MAIN law theorem (with its
        // `fn.law` identity from the class marker) MINUS the ones the manifest
        // already records as tier Universal (a genuinely-closed law has no
        // residual). Reading the open set from the emitted markers — not just
        // `m.laws` — is essential: a sorry-floored, universal-CLASSED law earns
        // NO manifest record at all (the audit returns early on `sorries > 0`),
        // so it would otherwise be invisible to a `m.laws`-only scan. A Bounded
        // record IS probed (its native_decide twin closes, but the law's own
        // universal `∀`-statement is the residual-bearing shape).
        let closed_universal: std::collections::HashSet<&str> = m
            .laws
            .iter()
            .filter(|l| matches!(l.tier, LawTier::Universal))
            .map(|l| l.law.as_str())
            .collect();
        let open: Vec<(String, String)> = emitted_main_law_theorems(output_dir)
            .into_iter()
            .filter(|(label, _)| !closed_universal.contains(label.as_str()))
            .collect();
        open_goals = lean_residual_goals(output_dir, &open);
        // Attribute residuals to the law that ACTUALLY failed. When the gate
        // build has residual sorries the audit bailed on `sorries > 0`, so
        // `closed_universal` above is empty and the coarse normalization-only
        // probe runs over EVERY emitted law — including healthy, kernel-proven
        // ones (a proven law still yields an "unsolved goals" residual once its
        // closing tactics are stripped). Keying that borrowed residual as the
        // failure is what sent the P4 cold-start report to "fix" a law that was
        // already proven. So split by `sorry_laws` (the laws whose theorem truly
        // carries the gate-build sorry): sorry-bearers are the genuine
        // `open_goals`; a residual from a non-sorry law is probe context
        // (`probe_of`), never presented as the failure. With no sorries the probe
        // only ran over bounded laws, so keep every residual as `open_goals`
        // (unchanged behavior — no `probe_of`).
        if !sorry_laws.is_empty() {
            let sorry_set: std::collections::HashSet<&str> =
                sorry_laws.iter().map(String::as_str).collect();
            let mut genuine: std::collections::BTreeMap<String, String> =
                std::collections::BTreeMap::new();
            for (law, goal) in std::mem::take(&mut open_goals) {
                if sorry_set.contains(law.as_str()) {
                    genuine.insert(law, goal);
                } else {
                    probe_of.insert(law, goal);
                }
            }
            open_goals = genuine;
        }
        for l in m.laws.iter_mut() {
            if let Some(goal) = open_goals.get(&l.law) {
                l.open_goal = Some(goal.clone());
            }
        }
        // `--explain` stage 2 + Aver-space rendering (console only). The raw Lean
        // residual above stays internal (manifest `open_goal` / json
        // `open_goals`); here the driver re-runs the probe with `aver_dump_goal`
        // to read a STRUCTURED goal, un-translates it to Aver, and prints either
        // a sample-checked candidate law or an honest engine-form-gap verdict.
        // Skipped under `--check-json` so the JSON bytes are unaffected.
        if !check_json {
            let goal_json = lean_goal_json(output_dir, &open);
            render_explain_candidates(
                &open,
                &goal_json,
                source_items,
                source_file,
                source_module_root,
            );
        }
    }
    // `--allow-mathlib` per-law credit (Lean only): tag each law `core` /
    // `mathlib` / `open`, ORTHOGONAL to its axiom-clean `tier`. The break-glass
    // arm emits a `trace "AVER_MATHLIB:fn.law"` as its first step, so a law whose
    // marker surfaces in the build log was emitted with the Mathlib arm (the
    // break-glass theorem's ONLY real closer — there are no core arms in it). A
    // Universal-tier law with the marker therefore closed via Mathlib (`mathlib`);
    // a Universal law without a marker closed in core (`core`); anything not
    // Universal is `open`. Set ONLY under the flag, so the default manifest is
    // byte-identical.
    if allow_mathlib
        && matches!(backend, super::cli::ProofBackend::Lean)
        && let Some(m) = manifest.as_mut()
    {
        let combined_build = format!("{stdout}{stderr}");
        let break_glass: std::collections::HashSet<&str> = combined_build
            .lines()
            .filter_map(|l| l.split("AVER_MATHLIB:").nth(1))
            .map(|rest| rest.split_whitespace().next().unwrap_or("").trim())
            .filter(|s| !s.is_empty())
            .collect();
        for l in m.laws.iter_mut() {
            let credit = if !matches!(l.tier, LawTier::Universal) {
                "open"
            } else if break_glass.contains(l.law.as_str()) {
                "mathlib"
            } else {
                "core"
            };
            l.credit = Some(credit.to_string());
        }
    }
    // Hand-proof SIDECAR per-law credit (BOTH backends): a law whose `fn.law`
    // identity had a sidecar spliced for this backend is credited `hand` IFF it
    // reached Universal tier (the spliced proof kernel/Z3-verified, axiom-clean
    // on Lean), else `open` — a wrong/stale sidecar that failed the build never
    // earns `hand`. Set ONLY for laws WITH a sidecar (orthogonal to `tier`, like
    // `--allow-mathlib`'s credit), so a law with no sidecar keeps `credit: None`
    // and the serialized manifest stays byte-identical. Runs last, so a hand
    // sidecar overrides any Mathlib classification for the same law.
    if !hand_laws.is_empty()
        && let Some(m) = manifest.as_mut()
    {
        for l in m.laws.iter_mut() {
            if hand_laws.contains(l.law.as_str()) {
                l.credit = Some(
                    if matches!(l.tier, LawTier::Universal) {
                        "hand"
                    } else {
                        "open"
                    }
                    .to_string(),
                );
            }
        }
    }
    // Law PROVENANCE (Lean only): a self-declared `// aver:provenance <value>
    // [k=v …]` comment directly above a `verify … law` block travels into the
    // manifest as maintenance metadata. Recorded ONLY for a law that PROVES
    // (any real tier — not Failed/Missing) AND only when the marker is present,
    // so an unmarked law stays provenance-free (authored by default, no noise)
    // and a manifest with no marked law is byte-identical to before. The marker
    // is UNVERIFIED (see `PROVENANCE_MARKER_PREFIX`): a hand-written law may
    // claim `calculated`; it is recorded as claimed — the harmless direction.
    if let Some(m) = manifest.as_mut() {
        let src = std::fs::read_to_string(source_file).unwrap_or_default();
        for l in m.laws.iter_mut() {
            if matches!(l.tier, LawTier::Failed | LawTier::Missing) {
                continue; // did not prove — not an established law yet
            }
            let Some((fn_name, law_name)) = l.law.rsplit_once('.') else {
                continue;
            };
            if let Some(line) = verify_law_source_line(source_items, fn_name, law_name)
                && let Some(p) = provenance_marker_above(&src, line)
            {
                l.provenance = Some(p);
            }
        }
    }
    // Write the manifest AFTER residuals are merged so the sidecar carries them.
    if let Some(m) = &manifest {
        write_proof_manifest(output_dir, m);
    }

    if check_json {
        let mut obj = serde_json::Map::new();
        obj.insert("backend".into(), backend_tag.into());
        if let Some(e) = errors {
            obj.insert("errors".into(), e.into());
        }
        if let Some(t) = dafny_timeouts {
            obj.insert("timeouts".into(), t.into());
        }
        if let Some(s) = sorries {
            obj.insert("sorries".into(), s.into());
        }
        if let Some(be) = lean_build_errors {
            obj.insert("build_errors".into(), be.into());
        }
        if let Some(a) = axioms {
            obj.insert("axioms".into(), a.into());
            obj.insert("axiom_budget".into(), sorry_budget_v.into());
        }
        if let Some(o) = omitted {
            obj.insert("omitted".into(), o.into());
        }
        // Claims the exporter refused to state. Emitted only when there ARE
        // any, so a clean run's bytes are unchanged and every existing
        // substring consumer is untouched. `declined_claims` carries the
        // identity and the reason, so a consumer never has to grep the
        // generated file for a sentence that gets reworded.
        if declined_count > 0 {
            obj.insert("declined".into(), declined_count.into());
            obj.insert("declined_budget".into(), declined_budget_v.into());
            obj.insert(
                "declined_claims".into(),
                serde_json::Value::Array(
                    declined
                        .iter()
                        .map(|d| {
                            let mut e = serde_json::Map::new();
                            e.insert("claim".into(), d.claim.clone().into());
                            e.insert("kind".into(), d.kind.as_str().into());
                            e.insert("reason".into(), d.reason.clone().into());
                            serde_json::Value::Object(e)
                        })
                        .collect(),
                ),
            );
        }
        if let Some(u) = universal {
            obj.insert("universal".into(), u.into());
        }
        if let Some(audit) = &lean_law_audit {
            // ADDITIVE law-count fields, sourced from the same class
            // markers and `#print axioms` audit the `universal` bool
            // keys on (computed in the counted build).
            obj.insert("universal_laws".into(), audit.universal_laws.into());
            obj.insert("bounded_laws".into(), audit.bounded_laws.into());
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
        // Which law(s) failed (Lean): the `fn.law` identities whose theorem
        // carries the gate-build `sorry`. Answers "which law?" without a manual
        // `lake build` + grep. Emitted only when non-empty (a clean build / Dafny
        // adds no key), so the check-json bytes are unchanged on a pass.
        if !sorry_laws.is_empty() {
            obj.insert(
                "sorry_laws".into(),
                serde_json::Value::Array(
                    sorry_laws
                        .iter()
                        .map(|l| serde_json::Value::String(l.clone()))
                        .collect(),
                ),
            );
        }
        // `--explain` ONLY: surface the per-law residuals inline so an agent
        // consumer reads them WITHOUT opening the sidecar. Keyed by `fn.law`
        // identity. Emitted only when `--explain` produced at least one residual
        // — with the flag absent (or no open law bearing a residual) the map is
        // empty and NO key is added, so the check-json bytes are unchanged.
        if !open_goals.is_empty() {
            let mut goals = serde_json::Map::new();
            for (law, goal) in &open_goals {
                goals.insert(law.clone(), goal.clone().into());
            }
            obj.insert("open_goals".into(), serde_json::Value::Object(goals));
        }
        // `--explain` ONLY: residuals the coarse probe surfaced from HEALTHY
        // (proven) laws while at least one OTHER law failed — probe context, kept
        // out of `open_goals` so a proven law is never mistaken for the failure.
        // Empty (no key) unless the split above moved a borrowed residual here.
        if !probe_of.is_empty() {
            let mut probes = serde_json::Map::new();
            for (law, goal) in &probe_of {
                probes.insert(law.clone(), goal.clone().into());
            }
            obj.insert("probe_of".into(), serde_json::Value::Object(probes));
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
        // A declined claim is invisible in `metric` — it produced no sorry and
        // no error — so name it explicitly, or the last line a user reads on a
        // failed run is "0 sorries" sitting next to a non-zero exit code.
        let metric = if declined_count > 0 {
            format!("{metric}, {declined_count} declined")
        } else {
            metric
        };
        let budget_desc = if declined_count > 0 {
            format!("{budget_desc}, declined ≤ {declined_budget_v}")
        } else {
            budget_desc
        };
        if passed {
            let suffix = if error_budget_v > 0 || sorry_budget_v > 0 || declined_budget_v > 0 {
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

/// Structured source-comment marker carrying a law's PROVENANCE — the tool (or
/// hand) that produced the `verify … law` block directly below it:
///
/// ```text
/// // aver:provenance <value> [k=v …]
/// verify <fn> law <name>
/// ```
///
/// It is an ordinary Aver `//` line comment (so it pastes into `.av` source and
/// the lexer skips it — the emitter's `-- aver:law-class` markers use Lean's
/// `--` because they live in generated Lean, not Aver source). `<value>` is an
/// OPEN-ENDED lowercase token (`calculated`, `conjectured`, …); several
/// producers mint it (the `--explain` calculator today, The Method conjecturer,
/// future tools), so it is deliberately NOT a closed enum. Optional `k=v` keys
/// carry maintenance context (`from=<parent law>`, `tool=explain`). It is
/// MAINTENANCE METADATA, not a proof input: when a law later breaks it says
/// whether to recompute, re-conjecture, or ask the author.
///
/// SELF-DECLARED, NOT VERIFIED: nothing checks that a law tagged `calculated`
/// was actually calculated — a hand-written law may claim any value and it is
/// recorded as claimed (the harmless direction; a wrong claim never grants
/// proof credit, which still comes only from the kernel/manifest tier).
const PROVENANCE_MARKER_PREFIX: &str = "// aver:provenance ";

/// Scan raw source `src` for a [`PROVENANCE_MARKER_PREFIX`] marker on the
/// comment line(s) IMMEDIATELY preceding the `verify` keyword at 1-indexed
/// `verify_line`. Walks upward over the contiguous run of blank / `--` comment
/// lines directly above the block; the first line that is neither blank nor a
/// comment ends the search. Returns the marker payload (value + any `k=v`
/// metadata) verbatim and trimmed, or `None` when no marker precedes the block
/// (or the payload is empty). Self-declared — see the const's doc.
fn provenance_marker_above(src: &str, verify_line: usize) -> Option<String> {
    let lines: Vec<&str> = src.lines().collect();
    let mut idx = verify_line.checked_sub(1)?; // 0-indexed `verify` line
    while idx > 0 {
        idx -= 1;
        let t = lines.get(idx)?.trim_start();
        if let Some(rest) = t.strip_prefix(PROVENANCE_MARKER_PREFIX) {
            let payload = rest.trim();
            return (!payload.is_empty()).then(|| payload.to_string());
        }
        if t.is_empty() || t.starts_with("//") {
            continue; // blank or ordinary comment — keep walking up
        }
        break; // first real line above the block: no marker
    }
    None
}

/// Source line (1-indexed, at the `verify` keyword) of the `verify <fn> law
/// <name>` block matching `fn_name` / `law_name`, from the parsed items — the
/// anchor `provenance_marker_above` scans from. `None` when no such block is in
/// these items (e.g. the law lives in a different, imported file).
fn verify_law_source_line(
    items: &[aver::ast::TopLevel],
    fn_name: &str,
    law_name: &str,
) -> Option<usize> {
    items.iter().find_map(|it| match it {
        aver::ast::TopLevel::Verify(vb)
            if vb.fn_name == fn_name
                && matches!(&vb.kind, aver::ast::VerifyKind::Law(l) if l.name == law_name) =>
        {
            Some(vb.line)
        }
        _ => None,
    })
}

/// Collect `fn.law` identities that are declared by MORE THAN ONE source
/// `verify ... law` block. The manifest keys every law on this `fn.law`
/// identity; two distinct source law blocks sharing one identity would
/// otherwise collapse to a single manifest entry (strongest-tier-wins), which
/// silently hides a weakened duplicate or reads a colliding rename as a benign
/// merge. The ratchet must fail CLOSED on that ambiguity, so we detect it at
/// the SOURCE level (two distinct law blocks), where the ambiguity actually
/// originates, rather than after the manifest has already deduped by identity.
/// Returns the colliding identities sorted, so the harness-error message is
/// deterministic.
#[cfg(test)]
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

/// Whole-program form of [`duplicate_law_identities`]. Dependency identities
/// are module-qualified, so `A.f.refl` and `B.f.refl` remain distinct while
/// two declarations inside `A` still fail closed.
fn duplicate_program_law_identities(ctx: &codegen::CodegenContext) -> Vec<String> {
    let mut seen: HashSet<String> = HashSet::new();
    let mut dups: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    let mut collect = |scope: Option<&str>, blocks: &[aver::ast::VerifyBlock]| {
        for vb in blocks {
            let VerifyKind::Law(law) = &vb.kind else {
                continue;
            };
            let bare = format!("{}.{}", vb.fn_name, law.name);
            let identity = scope.map_or(bare.clone(), |prefix| format!("{prefix}.{bare}"));
            if !seen.insert(identity.clone()) {
                dups.insert(identity);
            }
        }
    };
    let entry_blocks: Vec<_> = ctx
        .items
        .iter()
        .filter_map(|item| match item {
            TopLevel::Verify(vb) => Some(vb.clone()),
            _ => None,
        })
        .collect();
    collect(None, &entry_blocks);
    for module in &ctx.modules {
        collect(Some(&module.prefix), &module.verify_blocks);
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
    /// Claims the exporter REFUSED to state, so no law record exists for
    /// them. Recorded here because the ratchet's baseline is the artifact a
    /// reviewer reads: without this, a refused claim leaves the manifest with
    /// no trace at all and a freshly written baseline ratchets against a world
    /// in which it never existed. Serialized only when non-empty, so an
    /// unaffected corpus's manifest stays byte-for-byte identical.
    declined: Vec<aver::codegen::DeclinedClaim>,
}

/// The file-level audit records as one per-law manifest, keyed on the `fn.law`
/// identity and sorted by it for byte-reproducibility.
fn build_proof_manifest(
    file_laws: &[ManifestLaw],
    declined: &[aver::codegen::DeclinedClaim],
) -> ProofManifest {
    let mut by_label: std::collections::BTreeMap<String, ManifestLaw> =
        std::collections::BTreeMap::new();
    for record in file_laws.iter() {
        manifest_keep_stronger(&mut by_label, record.clone());
    }
    ProofManifest {
        backend: "lean".to_string(),
        laws: by_label.into_values().collect(),
        declined: declined.to_vec(),
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
            // Build per-law records as a `Map` (a `BTreeMap`, so keys stay
            // alphabetical / byte-reproducible) so `open_goal` is inserted ONLY
            // when present: an absent key (not `null`) for a closed law / a
            // manifest written without `--explain` keeps the byte-for-byte
            // baseline diff clean and existing substring consumers untouched.
            let mut obj = serde_json::Map::new();
            obj.insert("law".into(), l.law.clone().into());
            obj.insert("backend".into(), l.backend.clone().into());
            obj.insert("tier".into(), l.tier.as_str().into());
            obj.insert("axioms".into(), l.axioms.clone().into());
            obj.insert("theorem".into(), l.theorem.clone().into());
            if let Some(g) = &l.open_goal {
                obj.insert("open_goal".into(), g.clone().into());
            }
            // `--allow-mathlib` per-law credit: inserted ONLY when present, so a
            // manifest written without the flag stays byte-for-byte identical.
            if let Some(c) = &l.credit {
                obj.insert("credit".into(), c.clone().into());
            }
            // Self-declared law provenance: inserted ONLY when a marker was
            // present, so an unmarked corpus stays byte-for-byte identical.
            if let Some(p) = &l.provenance {
                obj.insert("provenance".into(), p.clone().into());
            }
            serde_json::Value::Object(obj)
        })
        .collect();
    let mut root = serde_json::Map::new();
    root.insert("version".into(), 1.into());
    root.insert("backend".into(), manifest.backend.clone().into());
    root.insert("laws".into(), serde_json::Value::Array(laws));
    // Declined claims, sorted by identity for byte-reproducibility. Written
    // ONLY when there are any, so a corpus with no refusal keeps the exact
    // bytes it had before this field existed.
    if !manifest.declined.is_empty() {
        let mut sorted: Vec<&aver::codegen::DeclinedClaim> = manifest.declined.iter().collect();
        sorted.sort_by(|a, b| (a.kind, &a.claim).cmp(&(b.kind, &b.claim)));
        root.insert(
            "declined".into(),
            serde_json::Value::Array(
                sorted
                    .iter()
                    .map(|d| {
                        let mut o = serde_json::Map::new();
                        o.insert("claim".into(), d.claim.clone().into());
                        o.insert("kind".into(), d.kind.as_str().into());
                        o.insert("reason".into(), d.reason.clone().into());
                        serde_json::Value::Object(o)
                    })
                    .collect(),
            ),
        );
    }
    serde_json::to_string_pretty(&serde_json::Value::Object(root))
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
            // `--explain` residual, tolerantly read (informational, like
            // `theorem`): absent on a manifest written without `--explain` /
            // for a closed law, so it stays `None` and never gates the ratchet.
            open_goal: item["open_goal"].as_str().map(str::to_string),
            // `--allow-mathlib` per-law credit, tolerantly read (informational):
            // absent on a manifest written without the flag, so it stays `None`.
            credit: item["credit"].as_str().map(str::to_string),
            // Self-declared provenance, tolerantly read (informational): absent
            // on a manifest whose law carried no marker, so it stays `None`.
            provenance: item["provenance"].as_str().map(str::to_string),
        });
    }
    Ok(ProofManifest {
        backend: value["backend"].as_str().unwrap_or("lean").to_string(),
        laws,
        // Informational in a BASELINE: the ratchet compares law records, and a
        // law that moved from proved to declined is already caught as MISSING
        // (the comparator iterates the baseline law set). Read as empty rather
        // than parsed, so an older baseline stays loadable.
        declined: Vec::new(),
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
    count_marker_in_generated_files(std::path::Path::new(dir), "dfy", "assume {:axiom}")
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
    count_marker_in_generated_files(
        std::path::Path::new(dir),
        "dfy",
        "(universal lemma omitted)",
    )
}

/// Count a marker in generated files recursively. Dotted Aver module names are
/// emitted under matching directories (`Infra.Store` -> `Infra/Store.dfy`), so
/// a top-level `read_dir` would miss exactly the dependency obligations these
/// gates are meant to charge.
fn count_marker_in_generated_files(dir: &std::path::Path, extension: &str, marker: &str) -> usize {
    let mut total = 0;
    if let Ok(rd) = std::fs::read_dir(dir) {
        for entry in rd.flatten() {
            let path = entry.path();
            if path.is_dir() {
                total += count_marker_in_generated_files(&path, extension, marker);
            } else if path.extension().and_then(|e| e.to_str()) == Some(extension)
                && let Ok(contents) = std::fs::read_to_string(path)
            {
                total += contents.matches(marker).count();
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

/// Count HARD Lean/lake build errors in captured verifier output — the
/// source-located `error: <file>.lean:L:C: …` diagnostics that abort the
/// build, DISTINCT from `sorry` (a non-fatal warning counted by
/// `count_lean_sorries`). A degraded proof arm should always fall to a caught
/// `sorry`; a hard error here means a tactic escaped the `first | … | sorry`
/// floor (measured: `assumption` on a metavariable conjunction goal, a
/// deterministic `whnf` heartbeat timeout), which `sorries` alone hides. Lake's
/// cascade lines (`error: Lean exited with code 1`, `error: build failed`)
/// carry no `.lean:` source location and are not counted, so one failing
/// theorem reads as one hard error, not three. Purely telemetry: never gates
/// `passed` or the exit code.
fn count_lean_build_errors(s: &str) -> usize {
    s.lines()
        .filter(|l| {
            let l = l.trim_start();
            l.starts_with("error:") && l.contains(".lean:")
        })
        .count()
}

/// Count Dafny per-lemma verification timeouts in captured verifier output.
/// The `errors` count from `parse_dafny_error_count` mirrors only Dafny's
/// "N errors" total and is BLIND to timeouts (measured on k5_fdiv round.av:
/// 2 errors reported while 12 additional laws timed out), so a consumer
/// reading `errors` alone under-counts failing laws. Each timed-out lemma
/// prints one `Verification of '…' timed out after N seconds` line; the
/// summary "N time outs" line uses "time outs" (with a space) and is not
/// matched, so it is not double-counted. Purely telemetry: never gates
/// `passed` or the exit code.
fn count_dafny_timeouts(stdout: &str) -> usize {
    stdout
        .lines()
        .filter(|l| l.contains("timed out after"))
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
    /// `fn.law` identity.
    law: String,
    backend: String,
    tier: LawTier,
    /// Sorted, deduped kernel axioms (empty = `does not depend on any axioms`).
    axioms: Vec<String>,
    /// Emitted theorem name — informational, never compared.
    theorem: String,
    /// `aver proof --explain` ONLY: the law's UNSOLVED GOAL ("residual") text,
    /// for laws that do not close universally. `None` everywhere unless
    /// `--explain` populated it (and unset for closed laws even then), so the
    /// serialized manifest stays byte-identical to before when absent. Purely
    /// informational, like `theorem` — NEVER gates the ratchet.
    open_goal: Option<String>,
    /// `aver proof --allow-mathlib` ONLY: which tier closed the law, ORTHOGONAL
    /// to `tier`/axioms — `core` (a core arm closed it, no Mathlib import
    /// needed), `mathlib` (only the generic Mathlib break-glass arm closed it,
    /// determined from the build-log `AVER_MATHLIB:fn.law` trace marker), or
    /// `open` (the law did not close universally). `None` unless `--allow-mathlib`
    /// set it, so the serialized manifest is byte-identical to before when absent.
    /// Informational like `theorem` — NEVER gates the ratchet.
    credit: Option<String>,
    /// The law's SELF-DECLARED provenance: the payload of a
    /// `-- aver:provenance <value> [k=v …]` source comment directly above the
    /// `verify … law` block (value + any `k=v` metadata, verbatim), recorded
    /// only when the marker is present AND the law proves. `None` for an
    /// unmarked law, so the serialized manifest is byte-identical to before
    /// when absent. UNVERIFIED — see `PROVENANCE_MARKER_PREFIX`: a hand-written
    /// law may claim any value; it is recorded as claimed, never proof credit.
    /// Informational like `theorem` — NEVER gates the ratchet.
    provenance: Option<String>,
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
    /// `fn.law` identity read from the class marker, forming the manifest.
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
    // Every lakefile root is part of the program proof. The first is the entry;
    // later roots include dependency modules (plus shared support roots, which
    // simply carry no law markers). The audit keys declarations by their full
    // root-qualified name so same-bare-name laws in two modules cannot collide.
    let roots = lean_lakefile_roots(dir);
    if roots.is_empty() {
        return LeanLawAudit::FAIL_CLOSED;
    }
    // Collect the main universal law theorems across the emitted sources,
    // plus the emitter's per-theorem statement-class markers.
    let mut law_thms: Vec<String> = Vec::new();
    let mut classes: std::collections::HashMap<String, String> = std::collections::HashMap::new();
    // `theorem -> fn.law` identity, read off the marker's third field — the
    // stable key the proof manifest is keyed on.
    let mut labels: std::collections::HashMap<String, String> = std::collections::HashMap::new();
    for root in &roots {
        let relative = format!("{}.lean", root.replace('.', "/"));
        let path = std::path::Path::new(dir).join(&relative);
        if let Ok(contents) = std::fs::read_to_string(path) {
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
            if relative == "DiscoveredLemmas.lean" && contents.contains("-- cone-hash:") {
                continue;
            }
            // User declarations live one level below their file namespace.
            // Ignore any deeper helper namespace while retaining those laws.
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
                        let qualified = format!("{root}.{thm}");
                        classes.insert(qualified.clone(), class.to_string());
                        // Third field (optional on older emissions): the
                        // `fn.law` identity label for the manifest.
                        if let Some(label) = parts.next() {
                            labels.insert(qualified, label.to_string());
                        }
                    }
                    continue;
                }
                if namespace_depth > 1 {
                    continue;
                }
                if let Some(rest) = line.strip_prefix("theorem ") {
                    let thm = rest
                        .split_whitespace()
                        .next()
                        .unwrap_or("")
                        .trim_end_matches(':');
                    if is_main_law_theorem(thm) {
                        law_thms.push(format!("{root}.{thm}"));
                    }
                }
            }
        }
    }
    // Every law theorem emitted by this compiler has a class marker (chunked
    // parts inherit their base marker). Roots such as `AverCommon` also contain
    // helper theorems whose ordinary names happen to include `_eq_`; importing
    // those roots into the whole-program audit must not promote support lemmas
    // into user law obligations.
    law_thms.retain(|thm| law_class_for_theorem(thm, &classes).is_some());
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
                    open_goal: None,
                    credit: None,
                    provenance: None,
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
    for theorem in &law_thms {
        src.push_str("#print axioms ");
        src.push_str(theorem);
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
            // per-declaration parser `theorem_credit_from_axioms` uses).
            // When `universal` is true above, every line passed,
            // so `universal_laws` equals the universal-classed count —
            // the file-level bool keeps EXACTLY its all-or-nothing
            // semantics, the count just shows how many theorems the
            // certificate covers (and, on a degraded file, how many
            // survived).
            let universal_laws = if o.status.success() {
                universal_classed
                    .iter()
                    .filter(|theorem| theorem_credit_from_axioms(&combined, theorem))
                    .count()
            } else {
                0
            };
            // Per-law manifest records for the universal-classed laws, from
            // the SAME probe output: tier `universal` iff the theorem's own
            // `#print axioms` line stays within the kernel whitelist (the
            // exact `theorem_credit_from_axioms` decision `universal_laws` counts),
            // else `failed`. Axioms are the parsed, sorted set the gate diffs.
            // Deduped by `fn.law` identity so chunked `_part<N>` theorems
            // collapse onto one law (strongest-wins if they disagree).
            let mut universal_records: std::collections::BTreeMap<String, ManifestLaw> =
                std::collections::BTreeMap::new();
            for thm in &universal_classed {
                let key = law_dedup_key(thm, &classes);
                let label = manifest_label_for(key, &labels);
                let credited = o.status.success() && theorem_credit_from_axioms(&combined, thm);
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
                    open_goal: None,
                    credit: None,
                    provenance: None,
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

/// Parse `<path>.lean:<line>` out of a Lean/lake diagnostic line, returning the
/// normalized project-relative file path and the 1-based line number. Tolerant
/// of a leading `warning:` and repeated `./` prefixes (`lake build` and
/// `lake env lean` render paths differently). Keeping subdirectories is
/// identity-relevant: `A/Foo.lean` and `B/Foo.lean` are different modules.
/// `None` if the line carries no `.lean:<digits>`.
fn parse_lean_decl_location(line: &str) -> Option<(String, usize)> {
    let idx = line.find(".lean:")?;
    let before = &line[..idx];
    let start = before
        .rfind(char::is_whitespace)
        .map(|p| p + 1)
        .unwrap_or(0);
    let stem = before[start..].trim_start_matches("./");
    if stem.is_empty() {
        return None;
    }
    let after = &line[idx + ".lean:".len()..];
    let digits: String = after.chars().take_while(|c| c.is_ascii_digit()).collect();
    let ln = digits.parse::<usize>().ok()?;
    Some((format!("{stem}.lean"), ln))
}

/// Map each `declaration uses 'sorry'` warning in a Lean `lake build` log to the
/// `fn.law` identity of the enclosing law theorem, via the emitted
/// `-- aver:law-class` markers. Returns the SORTED, DEDUPED set of law identities
/// whose theorem carries a residual `sorry` in the GATE build — the answer to
/// "which law failed?" that otherwise required a manual `lake build` + grep. The
/// gate build's floor is a bare `sorry` (not the probe's `AVERSPEC_SORRY` trace),
/// so the warning's `file:line` is the only signal: it points at the theorem's
/// declaration line, so the enclosing theorem is the nearest one whose line is
/// `<= the warning line`. Lean-only; empty when the build has no sorries or the
/// emitted sources are unreadable.
fn lean_sorry_laws(dir: &str, build_output: &str) -> Vec<String> {
    let locations: Vec<(String, usize)> = build_output
        .lines()
        .filter(|l| l.contains("declaration uses") && l.contains("sorry"))
        .filter_map(parse_lean_decl_location)
        .collect();
    if locations.is_empty() {
        return Vec::new();
    }
    let roots = lean_lakefile_roots(dir);
    if roots.is_empty() {
        return Vec::new();
    }
    // In every program root: its `theorem <name>` declarations as
    // `(1-based line, root-qualified name)`, plus the global qualified-theorem
    // → `fn.law` label map read off the class markers.
    let mut labels: std::collections::HashMap<String, String> = std::collections::HashMap::new();
    let mut file_thms: std::collections::HashMap<String, Vec<(usize, String)>> =
        std::collections::HashMap::new();
    for root in &roots {
        let relative = format!("{}.lean", root.replace('.', "/"));
        let path = std::path::Path::new(dir).join(&relative);
        if let Ok(contents) = std::fs::read_to_string(path) {
            let mut thms: Vec<(usize, String)> = Vec::new();
            for (idx, line) in contents.lines().enumerate() {
                if let Some(rest) = line.strip_prefix(lean_codegen::LAW_CLASS_MARKER_PREFIX) {
                    let mut parts = rest.split_whitespace();
                    if let (Some(thm), Some(_class)) = (parts.next(), parts.next())
                        && let Some(label) = parts.next()
                    {
                        labels.insert(format!("{root}.{thm}"), label.to_string());
                    }
                    continue;
                }
                if let Some(rest) = line.strip_prefix("theorem ") {
                    let thm = rest
                        .split_whitespace()
                        .next()
                        .unwrap_or("")
                        .trim_end_matches(':');
                    if !thm.is_empty() {
                        thms.push((idx + 1, format!("{root}.{thm}"))); // Lean lines are 1-based
                    }
                }
            }
            file_thms.insert(relative, thms);
        }
    }
    let mut out: Vec<String> = Vec::new();
    for (file, warn_line) in &locations {
        let Some(thms) = file_thms.get(file) else {
            continue;
        };
        // Enclosing theorem = the nearest declaration at or above the warning
        // line (the sorry warning is located at the theorem's own declaration).
        if let Some((_, thm)) = thms
            .iter()
            .filter(|(tl, _)| tl <= warn_line)
            .max_by_key(|(tl, _)| *tl)
        {
            out.push(manifest_label_for(thm, &labels));
        }
    }
    out.sort();
    out.dedup();
    out
}

/// Scan the emitted `.lean` sources for every MAIN law theorem (the universal
/// `∀`-claim — `is_main_law_theorem`, excluding `_checked_domain`/`_sample_N`
/// bounded cross-checks) paired with its `fn.law` identity from the class
/// marker's third field (falling back to the theorem name on an older emission
/// without the label, matching `manifest_label_for`). Used by `--explain` to
/// build the open-law set DIRECTLY from the markers — robust to the audit
/// returning early (no manifest record) for a sorry-floored universal-classed
/// law. Returns `(fn.law, theorem)` pairs, deduped by theorem name. Only the
/// entry module's namespace is scanned; dependency law pools remain transitive
/// inputs to those theorems.
fn emitted_main_law_theorems(dir: &str) -> Vec<(String, String)> {
    let roots = lean_lakefile_roots(dir);
    let Some(entry_root) = roots.first() else {
        return Vec::new();
    };
    let entry_file_name = format!("{entry_root}.lean");
    let mut labels: std::collections::HashMap<String, String> = std::collections::HashMap::new();
    let mut thms: Vec<String> = Vec::new();
    let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
    if let Ok(rd) = std::fs::read_dir(dir) {
        for entry in rd.flatten() {
            let name = entry.file_name().to_string_lossy().into_owned();
            if name != entry_file_name {
                continue;
            }
            let Ok(contents) = std::fs::read_to_string(entry.path()) else {
                continue;
            };
            if name == "DiscoveredLemmas.lean" && contents.contains("-- cone-hash:") {
                continue;
            }
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
                    if let (Some(thm), Some(_class)) = (parts.next(), parts.next())
                        && let Some(label) = parts.next()
                    {
                        labels.insert(thm.to_string(), label.to_string());
                    }
                    continue;
                }
                if namespace_depth > 1 {
                    continue;
                }
                if let Some(rest) = line.strip_prefix("theorem ") {
                    let thm = rest
                        .split_whitespace()
                        .next()
                        .unwrap_or("")
                        .trim_end_matches(':');
                    if is_main_law_theorem(thm) && seen.insert(thm.to_string()) {
                        thms.push(thm.to_string());
                    }
                }
            }
        }
    }
    thms.into_iter()
        .map(|thm| {
            let label = manifest_label_for(&thm, &labels);
            (label, thm)
        })
        .collect()
}

/// `aver proof --explain` residual probe (Lean only, fail-soft). For each OPEN
/// law in `open_laws` (`(fn.law identity, emitted theorem name)`, tier
/// Failed/Bounded), re-emit ONLY that theorem's body as a NORMALIZATION-ONLY twin
/// (def-unfold + `List.cons_append` cons-peel, no `done`/`omega`/`split`/
/// `simp_all`/`| sorry`) so Lean's elaborator reports the law's residual
/// (`unsolved goals`) with the IH in canonical recursive form — exactly what a
/// Lemma-Calculation agent applies the IH against. Returns `fn.law -> residual`.
///
/// Runs `lake env lean <probe>` (NOT `lake build`: build reformats the diagnostic
/// as `error: ././File.lean:L:C: …` with build-failed noise and a different
/// path-before-`error:` shape; `lake env lean` prints the clean
/// `<file>:L:C: error: unsolved goals` the parser keys on). The COUNTED build
/// (`lake build` in `run_proof_check`) is left completely untouched. Every
/// failure path is
/// absorbed: a missing root, an unprobeable shape, or a non-`unsolved goals`
/// diagnostic just leaves that law without a residual (the caller keeps it
/// `open_goal: None`).
fn lean_residual_goals(
    dir: &str,
    open_laws: &[(String, String)],
) -> std::collections::BTreeMap<String, String> {
    use std::process::Command;
    let mut out: std::collections::BTreeMap<String, String> = std::collections::BTreeMap::new();
    if open_laws.is_empty() {
        return out;
    }
    let roots = lean_lakefile_roots(dir);
    if roots.is_empty() {
        return out;
    }
    let entry_root = &roots[0];
    let entry_file_name = format!("{entry_root}.lean");
    // Map the requested theorem names to their `fn.law` identity (the caller
    // already pairs them, but we key the scan on theorem name to find each
    // theorem's emitted source block). `theorem -> fn.law`.
    let want_label: std::collections::HashMap<&str, &str> = open_laws
        .iter()
        .map(|(label, thm)| (thm.as_str(), label.as_str()))
        .collect();

    // Scan the emitted `.lean` sources for each wanted theorem's full source
    // block: from its `theorem <name>` line through the line before the next
    // top-level `theorem`/`--`-marker/EOF. Re-emitting the body verbatim (minus
    // the closing cascade) keeps the probe statement byte-identical to what the
    // counted build elaborated.
    let mut blocks: std::collections::HashMap<String, Vec<String>> =
        std::collections::HashMap::new();
    if let Ok(rd) = std::fs::read_dir(dir) {
        for entry in rd.flatten() {
            let name = entry.file_name().to_string_lossy().into_owned();
            if name != entry_file_name {
                continue;
            }
            let Ok(contents) = std::fs::read_to_string(entry.path()) else {
                continue;
            };
            if name == "DiscoveredLemmas.lean" && contents.contains("-- cone-hash:") {
                continue;
            }
            let lines: Vec<&str> = contents.lines().collect();
            let mut i = 0;
            while i < lines.len() {
                let t = lines[i].trim_start();
                if let Some(rest) = t.strip_prefix("theorem ") {
                    let thm = rest
                        .split_whitespace()
                        .next()
                        .unwrap_or("")
                        .trim_end_matches(':');
                    if want_label.contains_key(thm) {
                        // Capture this theorem's source up to the next top-level
                        // `theorem` / marker comment / EOF.
                        let mut block = vec![lines[i].to_string()];
                        let mut j = i + 1;
                        while j < lines.len() {
                            let tj = lines[j].trim_start();
                            if tj.starts_with("theorem ")
                                || tj.starts_with(lean_codegen::LAW_CLASS_MARKER_PREFIX.trim())
                                || tj.starts_with("-- verify law ")
                                || tj == format!("end {entry_root}")
                            {
                                break;
                            }
                            block.push(lines[j].to_string());
                            j += 1;
                        }
                        blocks.insert(thm.to_string(), block);
                        i = j;
                        continue;
                    }
                }
                i += 1;
            }
        }
    }

    // Build the combined probe file: import every lakefile root, then one
    // normalization-only probe theorem per open law whose source block was found
    // AND whose shape is probeable (`residual_probe_body` returns `Some`). Track
    // the probe theorem name -> `fn.law` so a parsed residual attributes back to
    // the stable identity. Each probe is named `_aver_residual_<N>` so several
    // coexist without clashing with each other or the imported originals.
    let mut src = String::new();
    for r in &roots {
        src.push_str("import ");
        src.push_str(r);
        src.push('\n');
    }
    src.push_str(&lean_entry_scope_header(dir, entry_root));
    // `probe theorem name -> fn.law`, and the emit order so we can map a parsed
    // error line back to the enclosing probe theorem.
    let mut probe_to_law: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    let mut n = 0usize;
    for (label, thm) in open_laws {
        let Some(block) = blocks.get(thm) else {
            continue;
        };
        let block_refs: Vec<&str> = block.iter().map(String::as_str).collect();
        let probe_name = format!("_aver_residual_{n}");
        let Some(body) = aver::codegen::lean::residual_probe_body(&block_refs, &probe_name) else {
            continue;
        };
        src.push_str(&body);
        src.push('\n');
        probe_to_law.insert(probe_name, label.clone());
        n += 1;
    }
    if probe_to_law.is_empty() {
        return out;
    }
    src.push_str(&format!("end {entry_root}\n"));

    let probe_file = std::path::Path::new(dir).join("_aver_residual_probe.lean");
    if std::fs::write(&probe_file, &src).is_err() {
        return out;
    }
    let res = Command::new("lake")
        .args(["env", "lean", "_aver_residual_probe.lean"])
        .current_dir(dir)
        .output();
    let _ = std::fs::remove_file(&probe_file);
    let Ok(o) = res else { return out };
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&o.stdout),
        String::from_utf8_lossy(&o.stderr)
    );

    // Index probe theorem name -> the source LINE it was emitted on, so a parsed
    // error line attributes to the nearest preceding probe theorem.
    let probe_lines: Vec<&str> = src.lines().collect();
    let mut thm_at_line: Vec<(usize, &str)> = Vec::new();
    for (idx, line) in probe_lines.iter().enumerate() {
        if let Some(rest) = line.trim_start().strip_prefix("theorem ") {
            let nm = rest.split_whitespace().next().unwrap_or("");
            if probe_to_law.contains_key(nm) {
                thm_at_line.push((idx + 1, nm)); // lean is 1-based
            }
        }
    }

    // Parse the `lake env lean` diagnostics. Each block starts at a header
    // `<file>:<line>:<col>: (error|warning): <msg>` and runs to the next header
    // / EOF. Keep blocks whose msg == "unsolved goals", attribute the error line
    // to the nearest preceding probe theorem, and key the residual on its
    // `fn.law` identity.
    let diag_lines: Vec<&str> = combined.lines().collect();
    let mut k = 0;
    while k < diag_lines.len() {
        if let Some((line_no, msg)) = parse_lean_diag_header(diag_lines[k]) {
            // Slice this block's body (lines after the header up to the next
            // header / EOF).
            let mut body = Vec::new();
            let mut m = k + 1;
            while m < diag_lines.len() && parse_lean_diag_header(diag_lines[m]).is_none() {
                body.push(diag_lines[m]);
                m += 1;
            }
            if msg == "unsolved goals" {
                // Nearest probe theorem whose emit line is <= the error line
                // (`thm_at_line` is in ascending line order, so the LAST match is
                // the nearest preceding theorem).
                let owner = thm_at_line
                    .iter()
                    .rfind(|(tl, _)| *tl <= line_no)
                    .map(|(_, nm)| *nm);
                if let Some(nm) = owner
                    && let Some(label) = probe_to_law.get(nm)
                {
                    let residual = body.join("\n").trim_end().to_string();
                    if !residual.is_empty() {
                        // First residual per law wins (nil arm precedes cons);
                        // both arms describe the same law's open goal, and the
                        // cons arm carries the IH — prefer the LAST (cons) one,
                        // so overwrite.
                        out.insert(label.clone(), residual);
                    }
                }
            }
            k = m;
        } else {
            k += 1;
        }
    }
    out
}

/// `aver proof --explain` SECOND stage (Lean only, fail-soft): re-run the
/// residual probe with the `aver_dump_goal` meta-tactic appended so each open
/// law's residual goal is serialised to OUR JSON via the info log. Returns
/// `fn.law -> goal JSON` for the laws whose residual dump succeeded — the input
/// the candidate-law renderer un-translates back into Aver-space. Structurally
/// independent of [`lean_residual_goals`] (which still owns the pretty-text
/// `open_goal`): a total failure here just yields an empty map, so the pretty
/// path and `open_goal` are never disturbed.
/// One open law's dumped residual goal (our JSON), plus whether MORE THAN ONE
/// proof arm produced a dump for that law. When multiple arms are open, the
/// cons-arm candidate we render may not close the law on its own — the renderer
/// flags it.
struct GoalDump {
    /// Every residual goal dumped for this law, in info-log order. The dump probe
    /// splits a blocked conditional and dumps ALL branch goals, so one law yields
    /// several residuals (deduped); the calculator lifts each and dedups again.
    jsons: Vec<String>,
    multi_arm: bool,
}

fn lean_goal_json(
    dir: &str,
    open_laws: &[(String, String)],
) -> std::collections::BTreeMap<String, GoalDump> {
    use aver::codegen::lean::untranslate::{AVER_DUMP_GOAL_ELAB, GOAL_JSON_MARKER};
    use std::process::Command;
    let mut out: std::collections::BTreeMap<String, GoalDump> = std::collections::BTreeMap::new();
    if open_laws.is_empty() {
        return out;
    }
    let roots = lean_lakefile_roots(dir);
    if roots.is_empty() {
        return out;
    }
    let entry_root = &roots[0];
    let entry_file_name = format!("{entry_root}.lean");
    let want: std::collections::HashSet<&str> =
        open_laws.iter().map(|(_, thm)| thm.as_str()).collect();

    // Scan the emitted `.lean` sources for each wanted theorem's source block
    // (same shape as `lean_residual_goals`: `theorem <name>` through the line
    // before the next top-level theorem / marker / EOF).
    let mut blocks: std::collections::HashMap<String, Vec<String>> =
        std::collections::HashMap::new();
    if let Ok(rd) = std::fs::read_dir(dir) {
        for entry in rd.flatten() {
            let name = entry.file_name().to_string_lossy().into_owned();
            if name != entry_file_name {
                continue;
            }
            let Ok(contents) = std::fs::read_to_string(entry.path()) else {
                continue;
            };
            if name == "DiscoveredLemmas.lean" && contents.contains("-- cone-hash:") {
                continue;
            }
            let lines: Vec<&str> = contents.lines().collect();
            let mut i = 0;
            while i < lines.len() {
                let t = lines[i].trim_start();
                if let Some(rest) = t.strip_prefix("theorem ") {
                    let thm = rest
                        .split_whitespace()
                        .next()
                        .unwrap_or("")
                        .trim_end_matches(':');
                    if want.contains(thm) {
                        let mut block = vec![lines[i].to_string()];
                        let mut j = i + 1;
                        while j < lines.len() {
                            let tj = lines[j].trim_start();
                            if tj.starts_with("theorem ")
                                || tj.starts_with(lean_codegen::LAW_CLASS_MARKER_PREFIX.trim())
                                || tj.starts_with("-- verify law ")
                                || tj == format!("end {entry_root}")
                            {
                                break;
                            }
                            block.push(lines[j].to_string());
                            j += 1;
                        }
                        blocks.insert(thm.to_string(), block);
                        i = j;
                        continue;
                    }
                }
                i += 1;
            }
        }
    }

    // Build the probe file: the meta-tactic (carries its own `import Lean`),
    // preceded by the lakefile-root imports, then one dump probe per open law.
    let mut import_src = String::new();
    for r in &roots {
        import_src.push_str("import ");
        import_src.push_str(r);
        import_src.push('\n');
    }
    let mut body_src = String::new();
    let mut n = 0usize;
    for (label, thm) in open_laws {
        let Some(block) = blocks.get(thm) else {
            continue;
        };
        let block_refs: Vec<&str> = block.iter().map(String::as_str).collect();
        let probe_name = format!("_aver_goal_json_{n}");
        let Some(body) = aver::codegen::lean::residual_probe_body_dump(
            &block_refs,
            &probe_name,
            Some(label.as_str()),
        ) else {
            continue;
        };
        body_src.push_str(&body);
        body_src.push('\n');
        n += 1;
    }
    if n == 0 {
        return out;
    }
    // All imports (roots + the meta-tactic's `import Lean`) must precede any
    // declaration, so the root imports lead, then the elaborator, then probes.
    let scope_header = lean_entry_scope_header(dir, entry_root);
    let src =
        format!("{import_src}{AVER_DUMP_GOAL_ELAB}\n{scope_header}{body_src}end {entry_root}\n");

    let probe_file = std::path::Path::new(dir).join("_aver_goal_json_probe.lean");
    if std::fs::write(&probe_file, &src).is_err() {
        return out;
    }
    let res = Command::new("lake")
        .args(["env", "lean", "_aver_goal_json_probe.lean"])
        .current_dir(dir)
        .output();
    let _ = std::fs::remove_file(&probe_file);
    let Ok(o) = res else { return out };
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&o.stdout),
        String::from_utf8_lossy(&o.stderr)
    );
    // Each `AVER_GOAL_JSON:<fn.law>:<json>` info line carries one law's residual.
    // Locate the marker with `find` (not `strip_prefix`) so a position-prefixed
    // channel (`<file>:<line>:<col>: info: AVER_GOAL_JSON:…`) still matches — the
    // same defensive slice the AVERMIN trace parser uses.
    for line in combined.lines() {
        let Some(pos) = line.find(GOAL_JSON_MARKER) else {
            continue;
        };
        let rest = &line[pos + GOAL_JSON_MARKER.len()..];
        let Some((label, json)) = rest.split_once(':') else {
            continue;
        };
        let json = json.trim();
        if json.is_empty() {
            continue;
        }
        // Collect every distinct residual (one arm's split yields several); the
        // `multi_arm` flag records that more than one residual is open, so the
        // renderer can note the candidates may not each close the law alone.
        out.entry(label.to_string())
            .and_modify(|d| {
                if !d.jsons.iter().any(|j| j == json) {
                    d.jsons.push(json.to_string());
                    d.multi_arm = true;
                }
            })
            .or_insert_with(|| GoalDump {
                jsons: vec![json.to_string()],
                multi_arm: false,
            });
    }
    out
}

/// The VM sample-check verdict for a candidate `--explain` law.
enum SampleVerdict {
    /// Every sampled case held → a real candidate to add and cite.
    Pass,
    /// A sampled case broke the claim → the law is false as stated.
    Fail { counterexample: String },
    /// The candidate could not be machine-checked (did not parse / type-check as
    /// Aver, or the premise never held on the sample domain) — an engine-form
    /// gap, honestly declined rather than shown as a candidate.
    Gap(String),
}

/// `aver proof --explain` Aver-space renderer (Lean-only, console). For each
/// open law with a dumped residual goal: un-translate it back to Aver, and print
/// EITHER a candidate `law` skeleton gated through the VM sample-check (passed →
/// add-and-cite; failed → counterexample) OR an honest engine-form-gap verdict.
/// The agent/user surface is AVER-ONLY here — the raw Lean residual stays in the
/// internal `open_goal` channel. Pure console output: never affects tiers /
/// credit / `passed` / the exit code.
fn render_explain_candidates(
    open_laws: &[(String, String)],
    goal_json: &std::collections::BTreeMap<String, GoalDump>,
    items: &[aver::ast::TopLevel],
    file: &str,
    module_root: &str,
) {
    use aver::codegen::lean::lemma_calc::{self, CalcVerdict};
    use aver::codegen::lean::untranslate::{peano_ctx_for_law, untranslate_goal_ctx};
    use colored::Colorize;
    if open_laws.is_empty() {
        return;
    }
    // The lemma calculator reads program facts (constructor names, fn return
    // types) as data; build it once for the whole render.
    let calc_env = lemma_calc::CalcEnv::from_items(items);
    println!();
    println!("{}", "--explain: candidate Aver laws for open goals".bold());
    // Every OPEN law gets at least one verdict — a law whose residual could not be
    // extracted (no arm dumped a goal) is reported as an engine-form gap, never
    // silently skipped. A law's split residual yields several branch goals: each
    // becomes a candidate, deduped by source; a branch outside the grammar records
    // its decline reason so a law with no in-grammar branch still gets one honest
    // gap verdict.
    for (label, _thm) in open_laws {
        let Some(dump) = goal_json.get(label) else {
            println!(
                "  {label}: {}",
                "residual not extractable (engine-form gap)".yellow()
            );
            continue;
        };
        // `fn.law`: the law name is the final segment, the fn everything before.
        let (fn_name, law_name) = match label.rsplit_once('.') {
            Some((f, l)) => (f, l),
            None => ("", label.as_str()),
        };
        // Thread a Peano context so a law over a canonical-Peano ADT inverts the
        // transpiler's `Nat`-lift (`Succ x → x + 1`) back to the ADT's
        // constructors; a non-Peano law gets the default (pre-V2) behavior.
        let ctx = peano_ctx_for_law(items, fn_name, law_name);
        // A calculated lemma is named `_calc` and rendered "calculated law"; a raw
        // #630 residual fallback keeps `_residual` and "candidate law". The names
        // must agree with the block the sample-check keys its verdict on.
        let calc_law_name = format!("{law_name}_calc");
        let residual_law_name = format!("{law_name}_residual");
        // The parent law's given names: the fresh lifted variables must dedup
        // against them (the candidate builder resolves givens by name against the
        // parent, so a colliding lift clones the wrong domain — name capture).
        let mut parent_givens: std::collections::HashSet<String> = std::collections::HashSet::new();
        for it in items {
            if let aver::ast::TopLevel::Verify(vb) = it
                && vb.fn_name == fn_name
                && let aver::ast::VerifyKind::Law(l) = &vb.kind
                && l.name == law_name
            {
                for g in &l.givens {
                    parent_givens.insert(g.name.clone());
                }
            }
        }
        // A split residual yields several branch goals. Each becomes a candidate,
        // deduped by source; the sample-check partitions them. We surface every
        // branch that PASSES (a forced lemma — prop_73 legitimately yields one per
        // branch), and fall back to a single honest negative (counterexample, then
        // gap) only when no branch passes. A branch outside the grammar records its
        // decline reason so a law with no in-grammar branch still gets a verdict.
        let mut seen: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
        let mut passed: Vec<(String, bool)> = Vec::new();
        let mut first_fail: Option<(String, String)> = None;
        let mut first_gap: Option<String> = None;
        let mut decline: Option<String> = None;
        for json in &dump.jsons {
            let goal = match untranslate_goal_ctx(json, &ctx) {
                Ok(g) => g,
                Err(gap) => {
                    first_gap.get_or_insert(gap.reason);
                    continue;
                }
            };
            // Prefer the calculator's forced lemma when it sample-checks; else
            // fall back to the raw residual candidate. The calculator only ever
            // ADDS a stronger lemma — it never downgrades a raw candidate that
            // would have passed, so a Lemma that fails the VM defers to the raw.
            // A `Decline` is an honest verdict of its own: record its reason so it
            // is surfaced before the fallback, never silently dropped.
            let mut chosen: Option<(String, SampleVerdict, bool)> = None;
            match lemma_calc::calculate(&goal, &calc_env, &parent_givens) {
                CalcVerdict::Lemma(g) => {
                    if let Ok(src) = build_candidate_law(
                        fn_name,
                        law_name,
                        &calc_law_name,
                        *g,
                        items,
                        ctx.peano.as_ref(),
                    ) {
                        let verdict = sample_check_candidate(
                            &src,
                            fn_name,
                            &calc_law_name,
                            file,
                            module_root,
                        );
                        if matches!(verdict, SampleVerdict::Pass) {
                            chosen = Some((src, verdict, true));
                        }
                    }
                }
                CalcVerdict::Decline(reason) => {
                    decline.get_or_insert(reason);
                }
            }
            let (src, verdict, calculated) = match chosen {
                Some(c) => c,
                None => {
                    let src = match build_candidate_law(
                        fn_name,
                        law_name,
                        &residual_law_name,
                        goal,
                        items,
                        ctx.peano.as_ref(),
                    ) {
                        Ok(s) => s,
                        Err(reason) => {
                            first_gap.get_or_insert(reason);
                            continue;
                        }
                    };
                    let verdict = sample_check_candidate(
                        &src,
                        fn_name,
                        &residual_law_name,
                        file,
                        module_root,
                    );
                    (src, verdict, false)
                }
            };
            if !seen.insert(src.clone()) {
                continue; // alpha-equivalent branch already accounted for
            }
            match verdict {
                SampleVerdict::Pass => passed.push((src, calculated)),
                SampleVerdict::Fail { counterexample } => {
                    first_fail.get_or_insert((counterexample, src));
                }
                SampleVerdict::Gap(reason) => {
                    first_gap.get_or_insert(reason);
                }
            }
        }
        // The fourth verdict form: an honest calculator decline. Surface its reason
        // BEFORE the raw fallback candidate it deferred to, so the reason the lemma
        // was not forced is never silently discarded.
        if let Some(reason) = &decline {
            println!(
                "  {label}: {}",
                format!("not a forced lemma — {reason}").yellow()
            );
        }
        if !passed.is_empty() {
            for (src, calculated) in &passed {
                let head = if *calculated {
                    "calculated law — sample-check passed, add it and cite:"
                } else {
                    "candidate law — sample-check passed, add it and cite:"
                };
                println!("  {label}: {}", head.green());
                // A calculated block is machine-produced, so stamp its
                // provenance directly above the pasteable `verify …`: when the
                // user pastes it and it proves, `--check` records the value in
                // the new law's manifest entry (see `PROVENANCE_MARKER_PREFIX`).
                // `from=` points back at the stuck law this was calculated from.
                if *calculated {
                    println!(
                        "      {PROVENANCE_MARKER_PREFIX}calculated from={label} tool=explain"
                    );
                }
                for line in src.lines() {
                    println!("      {line}");
                }
            }
        } else if let Some((counterexample, src)) = first_fail {
            println!(
                "  {label}: {}",
                "law false as stated — sample-check counterexample:".red()
            );
            println!("      {counterexample}");
            for line in src.lines() {
                println!("      {line}");
            }
        } else {
            let reason = first_gap.unwrap_or_else(|| "no residual branch in grammar".to_string());
            println!(
                "  {label}: {}",
                format!("engine-form gap — {reason}").yellow()
            );
        }
        // The multi-arm caveat applies to any verdict — a single-branch candidate
        // may not close a many-branch law whether it passed, failed, or gapped.
        if dump.multi_arm {
            println!(
                "      {}",
                "(the law has more than one open branch — a candidate may not close it alone)"
                    .yellow()
            );
        }
    }
}

/// Build a candidate Aver `law` source from an un-translated residual goal:
/// givens from the data binders (domains reused from the original law where a
/// binder name matches, else synthesized per type), `when` from a surviving
/// premise, claim from the goal equality. Declines (engine-form gap) on a binder
/// whose type cannot be sampled or when more than one premise survives (Aver
/// `when` is a single Bool expression). `cand_name` is the emitted law's name
/// (`<law>_calc` for a calculated lemma, `<law>_residual` for a raw candidate);
/// `law_name` still keys the parent-law given lookup.
fn build_candidate_law(
    fn_name: &str,
    law_name: &str,
    cand_name: &str,
    goal: aver::codegen::lean::untranslate::UntranslatedGoal,
    items: &[aver::ast::TopLevel],
    peano: Option<&aver::codegen::lean::untranslate::PeanoCtx>,
) -> Result<String, String> {
    use aver::ast::{TopLevel, VerifyBlock, VerifyGiven, VerifyKind, VerifyLaw};
    // Original givens (by name) so a residual binder that IS the law's own
    // universally-quantified variable samples over the same domain the author
    // chose, not a synthesized guess.
    let mut orig: std::collections::HashMap<String, VerifyGiven> = std::collections::HashMap::new();
    for item in items {
        if let TopLevel::Verify(vb) = item
            && vb.fn_name == fn_name
            && let VerifyKind::Law(law) = &vb.kind
            && law.name == law_name
        {
            for g in &law.givens {
                orig.insert(g.name.clone(), clone_given(g));
            }
        }
    }
    let mut givens: Vec<VerifyGiven> = Vec::new();
    for (name, ty) in &goal.givens {
        if let Some(g) = orig.get(name) {
            givens.push(clone_given(g));
        } else {
            let domain = synth_domain(ty, peano)
                .ok_or_else(|| format!("cannot sample binder `{name}: {ty}`"))?;
            givens.push(VerifyGiven {
                name: name.clone(),
                type_name: ty.clone(),
                domain,
            });
        }
    }
    let when = match goal.premises.len() {
        0 => None,
        1 => Some(clone_expr(&goal.premises[0])),
        n => {
            return Err(format!(
                "{n} surviving premises (Aver `when` is one Bool expression)"
            ));
        }
    };
    let law = VerifyLaw {
        name: cand_name.to_string(),
        givens,
        when,
        lhs: clone_expr(&goal.claim.0),
        rhs: clone_expr(&goal.claim.1),
        sample_guards: vec![],
    };
    let block = VerifyBlock {
        fn_name: fn_name.to_string(),
        line: 0,
        cases: vec![],
        case_spans: vec![],
        case_givens: vec![],
        case_hostile_origins: vec![],
        case_hostile_profiles: vec![],
        case_reverse_order: vec![],
        kind: VerifyKind::Law(Box::new(law)),
        trace: false,
        cases_givens: vec![],
    };
    aver::ast::unparse::unparse(&[TopLevel::Verify(block)])
        .map_err(|e| format!("could not render candidate: {e}"))
        .map(|s| s.trim_end().to_string())
}

/// Synthesize a small sample domain for a residual binder whose name is not one
/// of the original law's givens. Only the types `--explain` can sample directly.
/// A canonical-Peano ADT `T` (and `List<T>`) is sampled with constructor terms
/// `Zero, Succ(Zero), Succ(Succ(Zero))` — depth 2, and both the base and a
/// successor so a predicate like `isZ` is exercised on each branch (`peano`
/// carries the ADT's actual constructor names, threaded from the render site).
fn synth_domain(
    ty: &str,
    peano: Option<&aver::codegen::lean::untranslate::PeanoCtx>,
) -> Option<aver::ast::VerifyGivenDomain> {
    use aver::ast::{Expr, Literal, Spanned, VerifyGivenDomain};
    let int = |n: i64| Spanned::new(Expr::Literal(Literal::Int(n)), 0);
    match ty {
        "Int" => return Some(VerifyGivenDomain::IntRange { start: -3, end: 5 }),
        "Bool" => {
            return Some(VerifyGivenDomain::Explicit(vec![
                Spanned::new(Expr::Literal(Literal::Bool(true)), 0),
                Spanned::new(Expr::Literal(Literal::Bool(false)), 0),
            ]));
        }
        "List<Int>" => {
            return Some(VerifyGivenDomain::Explicit(vec![
                Spanned::new(Expr::List(vec![]), 0),
                Spanned::new(Expr::List(vec![int(1)]), 0),
                Spanned::new(Expr::List(vec![int(3), int(1), int(2)]), 0),
            ]));
        }
        _ => {}
    }
    let p = peano?;
    let zero = || Spanned::new(Expr::Ident(format!("{}.{}", p.type_name, p.zero_ctor)), 0);
    let succ = |x: Spanned<Expr>| {
        Spanned::new(
            Expr::FnCall(
                Box::new(Spanned::new(
                    Expr::Ident(format!("{}.{}", p.type_name, p.succ_ctor)),
                    0,
                )),
                vec![x],
            ),
            0,
        )
    };
    if ty == p.type_name {
        return Some(VerifyGivenDomain::Explicit(vec![
            zero(),
            succ(zero()),
            succ(succ(zero())),
        ]));
    }
    if ty == format!("List<{}>", p.type_name) {
        return Some(VerifyGivenDomain::Explicit(vec![
            Spanned::new(Expr::List(vec![]), 0),
            Spanned::new(Expr::List(vec![zero()]), 0),
            Spanned::new(
                Expr::List(vec![succ(zero()), zero(), succ(succ(zero()))]),
                0,
            ),
        ]));
    }
    None
}

/// Gate a candidate law source through the VM sample-check: append it to the
/// original source, re-parse, run the Declared-mode VM verify, and classify the
/// candidate block's result. Any parse / type-check failure (a residual that is
/// not a well-typed Aver law) is an honest engine-form gap, not a false
/// counterexample.
fn sample_check_candidate(
    candidate_src: &str,
    fn_name: &str,
    candidate_law_name: &str,
    file: &str,
    module_root: &str,
) -> SampleVerdict {
    use aver::checker::VerifyCaseOutcome;
    let Ok(orig) = std::fs::read_to_string(file) else {
        return SampleVerdict::Gap(format!("cannot read source `{file}`"));
    };
    let combined = format!("{orig}\n{candidate_src}\n");
    let base_dir = if module_root.is_empty() {
        None
    } else {
        Some(module_root)
    };
    // The candidate is appended to the user's own file, so the whole thing
    // parses under that file's project ceiling — otherwise a project with a
    // wide `given` domain could never sample-check a candidate at all.
    let ceiling = aver::source::project_verify_ceiling_or_default(base_dir, Some(file));
    let items = match aver::source::parse_source_with_verify_ceiling(&combined, ceiling) {
        // A candidate that does not even parse is syntactically outside the
        // Aver surface — report the kind, never the raw error (it can quote
        // Lean-only idents that leaked through as text).
        Err(_) => return SampleVerdict::Gap("candidate not machine-checkable".to_string()),
        Ok(i) => i,
    };
    let results =
        match aver::diagnostics::vm_verify::run_verify_for_items_vm(items, None, base_dir, file) {
            Ok(r) => r,
            // A type-check failure means the candidate references something the
            // translator never emits (an out-of-image ident). Sanitize: name the
            // construct kind, never echo the type error (it quotes the offending
            // Lean-only ident verbatim).
            Err(_) => {
                return SampleVerdict::Gap("construct outside the translator image".to_string());
            }
        };
    // Key the verdict on the EXACT synthesized block — `<fn> law <cand_name>`
    // (`<law>_calc` or `<law>_residual`) — not a substring: a pre-existing law
    // whose name merely CONTAINS the candidate name must not steal the verdict.
    // The candidate is appended last, so scan from the end to prefer our block
    // over any pre-existing same-named law.
    let want_label = format!("{fn_name} law {candidate_law_name}");
    let Some(res) = results
        .iter()
        .rev()
        .find(|r| r.is_law && r.block_label == want_label)
    else {
        return SampleVerdict::Gap("candidate law produced no verify result".to_string());
    };
    // Only a genuine value Mismatch means the law is false as stated. A
    // RuntimeError / unexpected error is the candidate failing to RUN — an
    // engine-form gap, not a counterexample — so it must not be rendered as
    // "false".
    let mismatch = res.case_results.iter().find_map(|c| match &c.outcome {
        VerifyCaseOutcome::Mismatch { expected, actual } => {
            let binds = c
                .law_context
                .as_ref()
                .map(|lc| {
                    lc.givens
                        .iter()
                        .map(|(n, v)| format!("{n} = {v}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                })
                .unwrap_or_default();
            Some(format!("{binds} :: expected {expected}, got {actual}"))
        }
        _ => None,
    });
    if let Some(counterexample) = mismatch {
        return SampleVerdict::Fail { counterexample };
    }
    if res.declined > 0 {
        // Not a counterexample and not a pass: the candidate's sampled case
        // ran out of budget, so nothing was observed about it.
        return SampleVerdict::Gap(
            "candidate not machine-checkable (a sampled case exceeded its step budget)".to_string(),
        );
    }
    if res.failed > 0 {
        return SampleVerdict::Gap(
            "candidate not machine-checkable (a sampled case errored)".to_string(),
        );
    }
    if res.passed == 0 {
        return SampleVerdict::Gap(
            "sample-check vacuous — the premise never held on the sample domain".to_string(),
        );
    }
    SampleVerdict::Pass
}

fn clone_given(g: &aver::ast::VerifyGiven) -> aver::ast::VerifyGiven {
    aver::ast::VerifyGiven {
        name: g.name.clone(),
        type_name: g.type_name.clone(),
        domain: clone_domain(&g.domain),
    }
}

fn clone_domain(d: &aver::ast::VerifyGivenDomain) -> aver::ast::VerifyGivenDomain {
    use aver::ast::VerifyGivenDomain;
    match d {
        VerifyGivenDomain::IntRange { start, end } => VerifyGivenDomain::IntRange {
            start: *start,
            end: *end,
        },
        VerifyGivenDomain::Explicit(vs) => {
            VerifyGivenDomain::Explicit(vs.iter().map(clone_expr).collect())
        }
    }
}

fn clone_expr(e: &aver::ast::Spanned<aver::ast::Expr>) -> aver::ast::Spanned<aver::ast::Expr> {
    // `Spanned<Expr>` is `Clone`; the OnceLock type slot is not re-derived, which
    // is fine — the candidate is re-parsed and re-checked from source anyway.
    e.clone()
}

/// Parse one `lake env lean` diagnostic header line —
/// `<file>:<line>:<col>: (error|warning): <msg>` (the
/// `^(\S+):(\d+):(\d+): (error|warning): (.*)$` shape) — into
/// `(line_number, message)`. `None` for any non-header line (so the caller can
/// slice a multi-line diagnostic body from header to header). Manual parse: the
/// `regex` crate is not a dependency of this binary.
fn parse_lean_diag_header(line: &str) -> Option<(usize, String)> {
    // No leading whitespace, no internal whitespace before the first colon.
    if line.starts_with(char::is_whitespace) {
        return None;
    }
    // Find `: error: ` or `: warning: ` (the severity marker).
    let (head, msg) = if let Some(idx) = line.find(": error: ") {
        (&line[..idx], line[idx + ": error: ".len()..].to_string())
    } else {
        let idx = line.find(": warning: ")?;
        (&line[..idx], line[idx + ": warning: ".len()..].to_string())
    };
    // `head` == `<file>:<line>:<col>`. The last two colon-separated fields must
    // be decimal `line`/`col`.
    let mut parts = head.rsplitn(3, ':');
    let col = parts.next()?;
    let ln = parts.next()?;
    let file = parts.next()?;
    if file.is_empty() || file.contains(char::is_whitespace) {
        return None;
    }
    if !col.bytes().all(|b| b.is_ascii_digit()) || col.is_empty() {
        return None;
    }
    let line_no: usize = ln.parse().ok()?;
    Some((line_no, msg))
}

/// Resolve the `fn.law` manifest identity for a law theorem. Prefers the
/// label recorded in the class marker's third field; falls back to the
/// theorem name itself only on
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

/// Parse the per-theorem crediting decision out of a `#print axioms` probe
/// output. The declaration must be PRESENT (its own result line exists) and its
/// axiom set must stay within the whitelist — `sorryAx` (a sorry-floored proof)
/// and `Lean.ofReduceBool` (`native_decide`) are rejected explicitly on top of
/// the whitelist. Anything else — error output, missing line, unknown constant
/// — earns no credit. Pure parser, unit-tested directly.
/// Read the complete `[...]` axiom list following `depends on axioms:` in
/// `tail` (the substring starting just after the colon), JOINING wrapped
/// continuation lines. `#print axioms` wraps long lists across physical lines;
/// a per-line parse would miss e.g. a `sorryAx` pushed onto a continuation
/// line — a soundness hole. Reads from the first `[` to its matching `]`
/// (across newlines) and returns the trimmed, non-empty axiom names.
fn parse_axiom_bracket(tail: &str) -> Vec<String> {
    let Some(open) = tail.find('[') else {
        return Vec::new();
    };
    let after = &tail[open + 1..];
    let close = after.find(']').unwrap_or(after.len());
    after[..close]
        .split(',')
        .map(|a| a.trim().to_string())
        .filter(|a| !a.is_empty())
        .collect()
}

fn theorem_credit_from_axioms(output: &str, theorem: &str) -> bool {
    const ALLOWED: [&str; 3] = ["propext", "Classical.choice", "Quot.sound"];
    match axioms_for_theorem(output, theorem) {
        Some(axioms) => axioms.iter().all(|a| ALLOWED.contains(&a.as_str())),
        None => false,
    }
}

/// Parse the SORTED, DEDUPED axiom set a theorem depends on out of a
/// `#print axioms` probe output. `Some(vec![])` = the declaration is present
/// and `does not depend on any axioms`; `Some([a, b, …])` = it depends on the
/// listed axioms; `None` = no result line for the theorem (missing / error).
/// The full `[...]` is read across WRAPPED continuation lines, so a long axiom
/// list cannot hide an axiom (e.g. `sorryAx` / `Lean.ofReduceBool`) on a second
/// physical line. The manifest records this set per law so the gate can flag
/// axiom-set GROWTH outside the recorded baseline set.
fn axioms_for_theorem(output: &str, theorem: &str) -> Option<Vec<String>> {
    let needle = format!("'{theorem}'");
    let pos = output.find(&needle)?;
    let rest = &output[pos + needle.len()..];
    let no_dep = rest.find("does not depend on any axioms");
    let dep = rest.find("depends on axioms:");
    match (no_dep, dep) {
        // The phrase that comes FIRST after the theorem name is this theorem's.
        (Some(n), d) if d.is_none_or(|d| n < d) => Some(Vec::new()),
        (_, Some(d)) => {
            let tail = &rest[d + "depends on axioms:".len()..];
            let mut axioms = parse_axiom_bracket(tail);
            axioms.sort();
            axioms.dedup();
            Some(axioms)
        }
        _ => None,
    }
}

/// `true` iff EVERY `depends on axioms: […]` record in `output` reports only the
/// core logical axioms (`propext`, `Classical.choice`, `Quot.sound`). Each
/// bracket is read in full across wrapped continuation lines, so an axiom on a
/// second physical line cannot slip past the whitelist. Text not matching the
/// `depends on axioms: […]` shape is ignored — the caller's blacklist probes
/// remain the floor for those.
fn lean_axiom_lines_whitelisted(output: &str) -> bool {
    const ALLOWED: [&str; 3] = ["propext", "Classical.choice", "Quot.sound"];
    const MARK: &str = "depends on axioms:";
    let mut search = output;
    while let Some(idx) = search.find(MARK) {
        let tail = &search[idx + MARK.len()..];
        if parse_axiom_bracket(tail)
            .iter()
            .any(|a| !ALLOWED.contains(&a.as_str()))
        {
            return false;
        }
        let advance = tail.find(']').map_or(tail.len(), |c| c + 1);
        search = &tail[advance..];
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

fn lean_entry_scope_header(dir: &str, entry_root: &str) -> String {
    let path = std::path::Path::new(dir).join(format!("{entry_root}.lean"));
    let opens = std::fs::read_to_string(path)
        .ok()
        .into_iter()
        .flat_map(|contents| {
            contents
                .lines()
                .filter(|line| line.starts_with("open "))
                .map(str::to_string)
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let mut header = String::new();
    if !opens.is_empty() {
        header.push_str(&opens.join("\n"));
        header.push('\n');
    }
    header.push_str(&format!("namespace {entry_root}\n\n"));
    header
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

/// Speculative-universal (Lean): the "try-universal, fall-back-to-sampled"
/// statement-form decision for SINGLE-LIST conditional laws (Gap 1). A single
/// list given with a `when` premise is too diverse to classify statically — the
/// generic conditional driver closes some (sortedness, the per-element-fold
/// shape) and `sorry`s others (the json roundtrips, whose conclusion is a
/// String/parse equation the list-induction portfolio cannot peel). So the
/// decision is made EMPIRICALLY, exactly as `--minimize` learns a portfolio's
/// winning branch from one instrumented build:
///
/// 1. PROBE — state EVERY single-list candidate universally (floored with an
///    `AVERSPEC_SORRY:<fn.law>` trace) and run one `lake build`. A candidate
///    whose portfolio fell through to the floor surfaces its id in the log (it
///    did not close). The bounded sample cross-checks still pass, so the probe
///    build always succeeds (a `sorry` is a warning).
/// 2. COMMIT — re-emit with the laws that CLOSED (`probed − failed`) stated
///    universally and the rest reverted to their sound bounded sampled-domain
///    statement.
///
/// No-op when the file has no single-list candidate (the probe emit records
/// none — the byte-identical baseline is restored without a build, so the
/// decomposed corpus pays nothing). Fail-safe: if the committed project does not
/// build, the bounded baseline is restored. `lake`'s content-addressed cache
/// keeps the probe + commit builds cheap.
fn run_lean_speculative(
    file: &str,
    output_dir: &str,
    ctx: &mut codegen::CodegenContext,
    verify_mode: &super::cli::ProofVerifyMode,
) {
    use aver::ast::{TopLevel, VerifyKind};
    use aver::codegen::lean::tactic_ir::speculative;
    use std::process::Command;

    // Cheap necessary-condition pre-filter: the speculative path fires for a
    // `when`-law with ONE or TWO `List<_>` givens (the single-list and two-list
    // conditional-inductive shapes), OR for a `when`-law with NO list givens that
    // FOLLOWS an earlier law block (a possible laws-as-lemmas pool — the keystone
    // `recognize_pool_composition_generic` shape). With no such law the probe
    // would admit nothing, so skip the extra emits + build entirely and leave the
    // baseline untouched. A loose match here is harmless: a file whose probe emit
    // traces no candidate (`probed_ids` empty) short-circuits before any build.
    let mut seen_law = false;
    let candidate_law = |law: &aver::ast::VerifyLaw, seen_law: bool| -> bool {
        let lists = law
            .givens
            .iter()
            .filter(|g| g.type_name.trim().starts_with("List<"))
            .count();
        law.when.is_some() && ((lists == 1 || lists == 2) || (lists == 0 && seen_law))
    };
    let entry_candidate = ctx.items.iter().any(|item| {
        let TopLevel::Verify(vb) = item else {
            return false;
        };
        let VerifyKind::Law(law) = &vb.kind else {
            return false;
        };
        let is_candidate = candidate_law(law, seen_law);
        seen_law = true;
        is_candidate
    });
    // A DEPENDENCY module's `when`-law can be a speculative candidate too — the
    // rounded-step reciprocal bound (`projects/k5_fdiv`) CITES the dependency
    // rounding bounds (`awayFracErrorBound` / `truncFracErrorBound`), which are
    // single-`when` laws the keystone closes universally only through the probe.
    // When the entry file has no candidate of its own the probe would never run,
    // leaving those dep laws stated bounded and the citation unresolved. Trigger
    // on a dep candidate as well; the probe still short-circuits (no build) when
    // the probe emit traces nothing (`probed_ids` empty), so a file with no
    // admitted speculative dep law pays only one extra emit and is byte-identical.
    let dep_candidate = ctx.modules.iter().any(|m| {
        let mut seen = false;
        m.verify_laws.iter().any(|vb| {
            let VerifyKind::Law(law) = &vb.kind else {
                return false;
            };
            let is_candidate = candidate_law(law, seen);
            seen = true;
            is_candidate
        })
    });
    if !entry_candidate && !dep_candidate {
        return;
    }

    let lake_ok = Command::new("lake")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false);
    if !lake_ok {
        // No prover to run the probe, so we cannot learn which candidates close.
        // The baseline directly admits two-list conditionals (the `default`), which
        // would stamp a non-closer `universal` over a `sorry` floor. Commit an
        // EMPTY closed-set instead: every conditional law falls back to its sound
        // bounded statement (honest — nothing was proven). Then re-emit.
        speculative::set_committed(std::collections::HashSet::new());
        cmd_proof_lean(file, output_dir, ctx, verify_mode);
        return;
    }
    let build = |dir: &str| -> (bool, String) {
        match Command::new("lake").arg("build").current_dir(dir).output() {
            Ok(o) => (
                o.status.success(),
                format!(
                    "{}{}",
                    String::from_utf8_lossy(&o.stdout),
                    String::from_utf8_lossy(&o.stderr)
                ),
            ),
            Err(_) => (false, String::new()),
        }
    };

    // 1) PROBE emit — single-list conditionals stated universally with trace floors.
    speculative::begin_probe();
    cmd_proof_lean(file, output_dir, ctx, verify_mode);
    let probed = speculative::probed_ids();
    if probed.is_empty() {
        // No single-list candidate — restore the byte-identical baseline (no
        // build needed; the probe emit only differs on single-list laws).
        speculative::clear();
        cmd_proof_lean(file, output_dir, ctx, verify_mode);
        return;
    }

    let (probe_ok, probe_out) = build(output_dir);
    if !probe_ok {
        // A `sorry` is only a warning, so the probe build succeeds even when
        // every candidate fails to close — a HARD failure means a speculative
        // statement did not elaborate, and the per-law verdict can't be trusted.
        // Commit an empty closed-set so every candidate falls back to bounded
        // (NOT `clear()`, which would re-expose the default-admit baseline and
        // stamp a two-list non-closer `universal` over a `sorry`).
        speculative::set_committed(std::collections::HashSet::new());
        cmd_proof_lean(file, output_dir, ctx, verify_mode);
        return;
    }
    let failed = speculative::parse_failures(&probe_out);
    let closed: std::collections::HashSet<String> = probed.difference(&failed).cloned().collect();

    // 2) COMMIT re-emit — the laws that closed go universal, the rest fall back.
    speculative::set_committed(closed.clone());
    cmd_proof_lean(file, output_dir, ctx, verify_mode);

    // 3) FAIL-SAFE verify — the committed project must still build.
    let (commit_ok, _) = build(output_dir);
    if !commit_ok {
        // Commit an empty closed-set so every conditional law falls back to
        // bounded (NOT `clear()` — the default-admit baseline would stamp a
        // two-list non-closer `universal` over a `sorry`).
        speculative::set_committed(std::collections::HashSet::new());
        cmd_proof_lean(file, output_dir, ctx, verify_mode);
        eprintln!(
            "{}",
            "speculative-universal: committed proof did not build — fell back to bounded".yellow()
        );
    } else if !closed.is_empty() {
        println!(
            "{}",
            format!(
                "speculative-universal: proved {} single-list conditional law(s) universally",
                closed.len()
            )
            .green()
        );
    }
}

/// `--minimize` (Lean): collapse each emitted `first | … | sorry` portfolio to
/// the single branch that actually closed.
///
/// The normal proof project is already on disk. We re-emit it twice:
///
/// 1. INSTRUMENT — prefix every `first` branch with a `trace
///    "AVERMIN:<idx>:<branch>"` marker and run ONE `lake build`. Lean tries a
///    portfolio's branches left-to-right and commits to the first that closes,
///    tracing each it reaches, so the winner of node `idx` is the maximum
///    branch index that surfaces in the build log (failed branches trace too —
///    they are not rolled back).
/// 2. COLLAPSE — re-emit with each portfolio reduced to its proven branch.
///
/// Fail-safe: the collapsed project must still build; if a winner was
/// mis-parsed and it does not, the normal (un-minimized) project is restored.
/// `lake`'s content-addressed cache keeps the second and third builds cheap.
fn run_lean_minimize(
    file: &str,
    output_dir: &str,
    ctx: &mut codegen::CodegenContext,
    verify_mode: &super::cli::ProofVerifyMode,
) {
    use aver::codegen::lean::tactic_ir::minimize;
    use std::process::Command;

    let lake_ok = Command::new("lake")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false);
    if !lake_ok {
        eprintln!(
            "{}",
            "--minimize: `lake` is not on PATH — left the proof un-minimized".yellow()
        );
        return;
    }

    // Run `lake build` in the project dir, returning (success, combined output).
    let build = |dir: &str| -> (bool, String) {
        match Command::new("lake").arg("build").current_dir(dir).output() {
            Ok(o) => (
                o.status.success(),
                format!(
                    "{}{}",
                    String::from_utf8_lossy(&o.stdout),
                    String::from_utf8_lossy(&o.stderr)
                ),
            ),
            Err(_) => (false, String::new()),
        }
    };

    // 1) INSTRUMENT + probe build.
    minimize::begin_instrument();
    cmd_proof_lean(file, output_dir, ctx, verify_mode);
    minimize::end();
    let (_probe_ok, probe_out) = build(output_dir);
    let winners = minimize::parse_winners(&probe_out);

    if winners.is_empty() {
        // No portfolio was exercised (or none collapsible) — restore the normal
        // project so `--check` runs against the unmodified proof.
        cmd_proof_lean(file, output_dir, ctx, verify_mode);
        println!("--minimize: no collapsible proof portfolios found");
        return;
    }

    // 2) COLLAPSE re-emit.
    minimize::begin_collapse(winners.clone());
    cmd_proof_lean(file, output_dir, ctx, verify_mode);
    minimize::end();

    // 3) FAIL-SAFE verify.
    let (collapsed_ok, _) = build(output_dir);
    if collapsed_ok {
        println!(
            "{}",
            format!(
                "--minimize: collapsed {} proof portfolio(s) to their proven branch",
                winners.len()
            )
            .green()
        );
    } else {
        cmd_proof_lean(file, output_dir, ctx, verify_mode);
        eprintln!(
            "{}",
            "--minimize: collapsed proof did not build — restored the normal proof".yellow()
        );
    }
}

/// The generic domain-blind Mathlib break-glass closer, injected into the entry
/// file(s) that use it. A CURATED tactic PORTFOLIO (not a per-figure template):
/// the nested-floor collapse `Int.ediv_ediv_of_nonneg`, the exponent-addition
/// `pow_add` / `pow_succ'`, and `positivity` / `nlinarith` / `norm_num` / `omega`.
/// Every arm is a guaranteed CLOSER (the `; done` floors the simp-style arms) so
/// `first` never commits to a non-closing branch and silently leave the goal open.
const LEAN_AVER_MATHLIB_MACRO: &str = r#"set_option linter.unreachableTactic false
set_option linter.unusedTactic false

/-- Generic Mathlib break-glass closer for the nonlinear-floor / power-of-two
fragment the core `omega` cannot see: a domain-blind tactic portfolio, NOT a
per-figure template. Every arm closes its goal or fails (the `; done` floors the
simp-style arms), so `first` commits only to a genuine closer. -/
syntax "aver_mathlib" : tactic
macro_rules
  | `(tactic| aver_mathlib) => `(tactic|
      first
        | omega
        | (rw [Int.ediv_ediv_of_nonneg (by omega)])
        | (rw [Int.ediv_ediv_of_nonneg (by positivity)])
        | (rw [Int.ediv_ediv_of_nonneg (by omega), ← pow_add])
        | (rw [Int.ediv_ediv_of_nonneg (by positivity), ← pow_add])
        | (rw [pow_succ'])
        | (rw [← pow_add])
        | nlinarith
        | (norm_num; done)
        | (positivity; done))"#;

/// `--allow-mathlib` post-emit wiring. Reuses a PREBUILT Mathlib cache (a lake
/// project with Mathlib already built, pointed to by the `AVER_MATHLIB_CACHE`
/// env var) so the per-check cost is just loading the cached oleans — no git
/// re-fetch. Three edits to the freshly-emitted project, all skipped when no
/// emitted file actually uses the break-glass arm (no `aver_mathlib` text):
/// (1) inject `import Mathlib` + the `aver_mathlib` macro into each entry file
/// that uses it (Mathlib stays out of every other file, so the dep modules' core
/// `simp`/`grind` keep their fast core simp set); (2) rewrite `lakefile.lean` to
/// `require mathlib` from the cached package; (3) reuse the cache's resolved
/// `.lake/packages` + `lake-manifest.json` so `lake build` (in `run_proof_check`)
/// resolves Mathlib from prebuilt oleans.
///
/// Fails LOUD (exit 2) on a misconfigured cache: the opt-in tier must never
/// silently degrade to a core build that then mis-credits a walling law.
fn setup_mathlib_for_project(output_dir: &str) {
    use std::path::Path;

    // Which emitted files actually invoke the break-glass arm? Only those need
    // (and may safely carry) the Mathlib import — importing Mathlib into a file
    // with a bare core `simp_all` blows the simp set up to a heartbeat timeout.
    let dir = Path::new(output_dir);
    let mut files_using: Vec<std::path::PathBuf> = Vec::new();
    if let Ok(rd) = std::fs::read_dir(dir) {
        for entry in rd.flatten() {
            let name = entry.file_name().to_string_lossy().into_owned();
            if !name.ends_with(".lean") || name == "lakefile.lean" {
                continue;
            }
            if let Ok(content) = std::fs::read_to_string(entry.path())
                && content.contains("aver_mathlib")
            {
                files_using.push(entry.path());
            }
        }
    }
    if files_using.is_empty() {
        // No law walled into the break-glass arm — leave the project pure-core
        // (no Mathlib import anywhere), so the build is the unchanged core build.
        return;
    }

    let cache = match std::env::var("AVER_MATHLIB_CACHE") {
        Ok(p) if !p.trim().is_empty() => p,
        _ => {
            eprintln!(
                "{}",
                "--allow-mathlib: set AVER_MATHLIB_CACHE to a lake project that has Mathlib \
                 built (a dir with .lake/packages/mathlib and lake-manifest.json, toolchain \
                 leanprover/lean4:v4.32.2) — the break-glass tier reuses its prebuilt oleans \
                 instead of re-fetching Mathlib per check."
                    .red()
            );
            std::process::exit(2);
        }
    };
    let cache = Path::new(&cache);
    let cache_packages = cache.join(".lake/packages");
    let cache_manifest = cache.join("lake-manifest.json");
    let cache_mathlib = cache_packages.join("mathlib");
    if !cache_mathlib.is_dir() || !cache_manifest.is_file() {
        eprintln!(
            "{}",
            format!(
                "--allow-mathlib: AVER_MATHLIB_CACHE={} is not a built Mathlib lake project \
                 (expected {}/ and {})",
                cache.display(),
                cache_mathlib.display(),
                cache_manifest.display()
            )
            .red()
        );
        std::process::exit(2);
    }

    // 1) Inject `import Mathlib` + the macro into each using file. `import`s must
    // precede every command, so splice `import Mathlib` at the end of the leading
    // import block and the macro right after it.
    for path in &files_using {
        let Ok(content) = std::fs::read_to_string(path) else {
            continue;
        };
        if content.contains("import Mathlib") {
            continue;
        }
        let mut out: Vec<String> = Vec::new();
        let mut macro_emitted = false;
        let mut in_import_block = true;
        for line in content.lines() {
            let is_import = line.trim_start().starts_with("import ");
            if in_import_block && !is_import && !line.trim().is_empty() && !macro_emitted {
                // First non-import, non-blank line: close the import block.
                out.push("import Mathlib".to_string());
                out.push(String::new());
                out.push(LEAN_AVER_MATHLIB_MACRO.to_string());
                out.push(String::new());
                macro_emitted = true;
                in_import_block = false;
            }
            out.push(line.to_string());
        }
        if !macro_emitted {
            // File was nothing but imports (degenerate) — append at the end.
            out.push("import Mathlib".to_string());
            out.push(String::new());
            out.push(LEAN_AVER_MATHLIB_MACRO.to_string());
        }
        let _ = std::fs::write(path, out.join("\n") + "\n");
    }

    // 2) Rewrite the lakefile to require the cached Mathlib (local-path require —
    // reuses the cache's already-built package, no fetch).
    let lakefile = dir.join("lakefile.lean");
    if let Ok(content) = std::fs::read_to_string(&lakefile)
        && !content.contains("require mathlib")
    {
        let require_line = format!("require mathlib from \"{}\"", cache_mathlib.display());
        // Insert the require after the `open Lake DSL` opener so the DSL is in scope.
        let patched = if let Some(idx) = content.find("open Lake DSL") {
            let cut = idx + "open Lake DSL".len();
            let (head, tail) = content.split_at(cut);
            format!("{head}\n\n{require_line}{tail}")
        } else {
            format!("{require_line}\n{content}")
        };
        let _ = std::fs::write(&lakefile, patched);
    }

    // 3) Reuse the cache's resolved packages + manifest so `lake build` finds
    // Mathlib (and its transitive deps) as prebuilt oleans without re-resolving.
    let _ = std::fs::copy(&cache_manifest, dir.join("lake-manifest.json"));
    let dot_lake = dir.join(".lake");
    let _ = std::fs::create_dir_all(&dot_lake);
    let pkg_link = dot_lake.join("packages");
    // Replace any stale link/dir from a prior run, then point at the cache.
    let _ = std::fs::remove_file(&pkg_link);
    let _ = std::fs::remove_dir_all(&pkg_link);
    #[cfg(unix)]
    let _ = std::os::unix::fs::symlink(&cache_packages, &pkg_link);

    println!(
        "{}",
        format!(
            "--allow-mathlib: wired cached Mathlib ({}) into {} break-glass file(s)",
            cache.display(),
            files_using.len()
        )
        .blue()
    );
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
    let entry_case_blocks = ctx
        .items
        .iter()
        .filter(|i| matches!(i, TopLevel::Verify(vb) if matches!(vb.kind, VerifyKind::Cases)))
        .count();
    let dependency_case_blocks: usize = ctx
        .modules
        .iter()
        .map(|module| {
            module
                .verify_blocks
                .iter()
                .filter(|vb| matches!(vb.kind, VerifyKind::Cases))
                .count()
        })
        .sum();
    let unchecked_case_blocks = entry_case_blocks + dependency_case_blocks;
    let unchecked_modules = usize::from(entry_case_blocks > 0)
        + ctx
            .modules
            .iter()
            .filter(|module| {
                module
                    .verify_blocks
                    .iter()
                    .any(|vb| matches!(vb.kind, VerifyKind::Cases))
            })
            .count();
    if unchecked_case_blocks > 0 {
        eprintln!(
            "{}",
            format!(
                "warning: {unchecked_case_blocks} example-based `verify` block(s) across \
                 {unchecked_modules} module(s) are NOT checked by the Dafny backend \
                 (Dafny proves laws, not concrete examples) — they are verified by \
                 `aver proof --backend lean` and `aver verify {file}`"
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
/// Returns the identity-preserving qualified type-name aliases the
/// flattener derived; thread them into the flattened compile entry.
#[cfg(feature = "wasm")]
pub(super) fn flatten_multimodule(
    items: &mut Vec<TopLevel>,
    dep_modules: &[ModuleInfo],
    capabilities: &aver::capability::CapabilityRegistry,
) -> std::collections::HashMap<String, String> {
    aver::codegen::wasm_gc::flatten_multimodule(
        items,
        dep_modules,
        capabilities,
        aver::codegen::wasm_gc::CapabilityFunctionSurface::Runtime,
    )
}

/// Which lowering a dependency module gets, mirroring the entry-module
/// decision the caller already made.
///
/// One field per pipeline gate rather than a bundled "optimise" flag, so
/// this matches the gates 1-to-1 with no magic translation in between:
/// Proof exporters (Lean/Dafny) ask for [`Self::PRISTINE`]. The wasm-gc
/// family asks for [`Self::STRING_INDEX_ONLY`] because it lowers the
/// packed String index but not the older mutable buffers/cursors/builders.
/// VM and Rust turn every supported pass on for dependencies too.
#[derive(Clone, Copy)]
pub(super) struct DepLowering {
    pub interp_lower: bool,
    pub buffer_build: bool,
    pub chars_fusion: bool,
    pub string_index: bool,
    pub list_build: bool,
    /// Self-host typecheck driver — bypasses the opaque-type checks so
    /// `domain/builtins.av` can round-trip host types.
    pub self_host: bool,
}

impl DepLowering {
    /// Source-level dependencies: nothing that invents code.
    pub(super) const PRISTINE: Self = Self {
        interp_lower: false,
        buffer_build: false,
        chars_fusion: false,
        string_index: false,
        list_build: false,
        self_host: false,
    };

    /// Runtime wasm-gc / wasip2 shape: the index is a native i32 array,
    /// while Buffer/cursor/list-builder fabrications remain unsupported.
    #[cfg(any(feature = "wasm", feature = "wasip2"))]
    pub(super) const STRING_INDEX_ONLY: Self = Self {
        string_index: true,
        ..Self::PRISTINE
    };

    /// Every fabricating pass on or off together for VM/Rust callers.
    pub(super) const fn deforesting(on: bool, self_host: bool) -> Self {
        Self {
            interp_lower: false,
            buffer_build: on,
            chars_fusion: on,
            string_index: on,
            list_build: on,
            self_host,
        }
    }

    /// As [`Self::deforesting`], with interpolation lowering too.
    pub(super) const fn fully_lowered(on: bool, self_host: bool) -> Self {
        Self {
            interp_lower: on,
            ..Self::deforesting(on, self_host)
        }
    }
}

/// Load dependent modules for codegen: every module of the program behind
/// `items`, typechecked and lowered with the target's matrix. Any problem is
/// fatal here — printed in red, exit 1 — exactly as it always was.
pub(super) fn load_compile_deps(
    items: &[TopLevel],
    module_root: &str,
    lowering: DepLowering,
) -> Vec<ModuleInfo> {
    fn fail(message: String) -> ! {
        eprintln!("{}", message.red());
        process::exit(1);
    }
    let program = match aver::source::load_program(
        Path::new("<entry>"),
        "",
        items,
        module_root,
        LoadMode::Tolerant,
    ) {
        Ok(program) => program,
        Err(LoadError::Missing { name, root, .. }) => fail(format!(
            "Cannot find module '{name}' in module root '{root}'"
        )),
        Err(error) => fail(error.to_string()),
    };
    let mut lowered = BTreeMap::new();

    // Parent before child, the order the walk met the modules in: the first
    // fault on the way down is the one reported. The returned list stays
    // leaves-first.
    for module in program.dependencies_in_discovery_order() {
        match &module.fault {
            Some(LoadError::Parse { error, .. }) => fail(error.clone()),
            Some(fault) => fail(fault.to_string()),
            None => {}
        }
        let mut module_items = module.items.clone();

        let neutral_policy = aver::ir::NeutralAllocPolicy;
        let dep_typecheck_mode = if lowering.self_host {
            aver::ir::TypecheckMode::FullSelfHost {
                base_dir: Some(module_root),
            }
        } else {
            aver::ir::TypecheckMode::Full {
                base_dir: Some(module_root),
            }
        };
        let pipeline_result = aver::ir::pipeline::run(
            &mut module_items,
            aver::ir::PipelineConfig {
                typecheck: Some(dep_typecheck_mode),
                run_interp_lower: lowering.interp_lower,
                run_buffer_build: lowering.buffer_build,
                run_chars_fusion: lowering.chars_fusion,
                run_string_index: lowering.string_index,
                run_list_build: lowering.list_build,
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
                    module.dep_name,
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
        lowered.insert(
            module.discovery_index,
            ModuleInfo::from_items(
                module.dep_name.clone(),
                &module_items,
                pipeline_result.analysis,
            ),
        );
    }

    program
        .dependencies()
        .iter()
        .filter_map(|module| lowered.remove(&module.discovery_index))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::{
        codegen_uses_self_host_runtime, resolve_av_inputs, suppression_path,
        validate_self_host_guest_entry_contract,
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
    fn verify_setup_errors_distinguish_provider_composition_from_typechecking() {
        for error in [
            "provider binding names unknown capability 'Shapes'",
            "error[capability-provider-mismatch]: wrong contract",
            "reserved standard capability 'Time' has contract_hash wrong",
        ] {
            assert!(
                aver::provider::is_provider_setup_error(error),
                "provider error was mislabeled: {error}"
            );
        }
        assert!(!aver::provider::is_provider_setup_error(
            "error[7:3]: expected Int, got String"
        ));
        assert!(!aver::provider::is_provider_setup_error(
            "wasm-gc compile failed"
        ));
    }

    #[test]
    fn certify_flag_is_rejected_exactly_on_certify_less_builds() {
        // `--certify` dispatch is decided by `certify_flag_rejection`, called
        // before any target dispatch in `cmd_compile`. On a build without the
        // `certify` feature (the default test build) the flag must produce
        // the clean flag-level error; on certify-carrying builds it must pass
        // through. Not passing the flag is never an error.
        let rejection = super::certify_flag_rejection(true);
        if cfg!(feature = "certify") {
            assert!(rejection.is_none());
        } else {
            let error = rejection.expect("--certify must be rejected without the certify feature");
            assert!(error.contains("--certify"));
            assert!(error.contains("certify"));
        }
        assert!(super::certify_flag_rejection(false).is_none());
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

    #[test]
    fn lean_axiom_whitelist_reads_wrapped_lists() {
        // `#print axioms` wraps long lists across physical lines. A `sorryAx`
        // (or any foreign axiom) pushed onto a CONTINUATION line must still be
        // caught — a per-line parse would only see the first physical line and
        // award universal credit to a sorry-bearing proof.
        let wrapped_sorry =
            "'f_law_x' depends on axioms: [propext, Classical.choice,\n  Quot.sound, sorryAx]";
        let wrapped_clean =
            "'f_law_x' depends on axioms: [propext,\n  Classical.choice, Quot.sound]";
        // The whitelist reader reads the full bracket across the wrap:
        assert!(!super::lean_axiom_lines_whitelisted(wrapped_sorry));
        assert!(super::lean_axiom_lines_whitelisted(wrapped_clean));
        // The credit reader (awards hand/core/universal) likewise rejects it:
        assert!(!super::theorem_credit_from_axioms(wrapped_sorry, "f_law_x"));
        assert!(super::theorem_credit_from_axioms(wrapped_clean, "f_law_x"));
        // And the per-law axiom set captures the wrapped tail in full:
        let set = super::axioms_for_theorem(wrapped_sorry, "f_law_x").unwrap();
        assert!(set.contains(&"sorryAx".to_string()));
        assert_eq!(set.len(), 4);
    }

    // ---- THE RATCHET: pure comparator + parser, fixture-driven (no lake) ----

    fn law(name: &str, tier: super::LawTier, axioms: &[&str]) -> super::ManifestLaw {
        super::ManifestLaw {
            law: name.to_string(),
            backend: "lean".to_string(),
            tier,
            axioms: axioms.iter().map(|a| a.to_string()).collect(),
            theorem: format!("{}_thm", name.replace('.', "_")),
            open_goal: None,
            credit: None,
            provenance: None,
        }
    }

    fn manifest(laws: Vec<super::ManifestLaw>) -> super::ProofManifest {
        super::ProofManifest {
            backend: "lean".to_string(),
            laws,
            declined: Vec::new(),
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
    fn manifest_open_goal_absent_when_none_present_when_some() {
        // `--explain` no-op invariant: a closed law (`open_goal: None`)
        // serializes with NO `open_goal` key at all (not `null`) — so a manifest
        // written without `--explain` is byte-identical to before. A law carrying
        // a residual serializes the key and it round-trips through the tolerant
        // parser as informational data.
        let mut closed = law("a.one", super::LawTier::Universal, &["propext"]);
        closed.open_goal = None;
        let mut open = law("b.two", super::LawTier::Bounded, &[]);
        open.open_goal = Some("case cons\nih : P tail\n⊢ Q (head :: tail)".to_string());
        let json = super::proof_manifest_to_json(&manifest(vec![closed, open]));
        // The closed law's record must NOT mention `open_goal`; the open one must.
        // (Only one `open_goal` occurrence in the whole document.)
        assert_eq!(
            json.matches("\"open_goal\"").count(),
            1,
            "exactly one law carries an open_goal key:\n{json}"
        );
        let parsed = super::parse_proof_manifest(&json).expect("parses back");
        let a = parsed.laws.iter().find(|l| l.law == "a.one").unwrap();
        let b = parsed.laws.iter().find(|l| l.law == "b.two").unwrap();
        assert!(a.open_goal.is_none(), "closed law has no residual");
        assert_eq!(
            b.open_goal.as_deref(),
            Some("case cons\nih : P tail\n⊢ Q (head :: tail)")
        );
    }

    #[test]
    fn manifest_all_none_open_goal_byte_identical_to_pre_explain() {
        // The strongest no-op guard: a manifest where every law has
        // `open_goal: None` (the only state reachable without `--explain`)
        // serializes to bytes that contain NO `open_goal` substring — the exact
        // shape pre-`--explain` baselines and substring consumers expect.
        let m = manifest(vec![
            law("a.one", super::LawTier::Universal, &["propext"]),
            law("b.two", super::LawTier::Bounded, &[]),
        ]);
        let json = super::proof_manifest_to_json(&m);
        assert!(
            !json.contains("open_goal"),
            "no law carries a residual → no open_goal key anywhere:\n{json}"
        );
    }

    #[test]
    fn provenance_marker_scan_reads_payload_verbatim() {
        // Directly above `verify`: the whole payload (value + `k=v`) is captured
        // verbatim. `verify` is on 1-indexed line 2 here.
        let src = "// aver:provenance calculated from=f.stuck tool=explain\nverify f law g\n";
        assert_eq!(
            super::provenance_marker_above(src, 2).as_deref(),
            Some("calculated from=f.stuck tool=explain")
        );
        // An UNKNOWN token is recorded verbatim too — the taxonomy is an open
        // enum (multiple producers mint tokens), so the scan never rejects one.
        let unknown = "// aver:provenance frobnicated\nverify f law g\n";
        assert_eq!(
            super::provenance_marker_above(unknown, 2).as_deref(),
            Some("frobnicated")
        );
        // A blank line and an ordinary comment between marker and `verify` are
        // part of the "immediately preceding" run — still found (`verify` = 4).
        let spaced = "// aver:provenance calculated\n// a note\n\nverify f law g\n";
        assert_eq!(
            super::provenance_marker_above(spaced, 4).as_deref(),
            Some("calculated")
        );
        // No marker → None; a real line above the block ends the search.
        let none = "fn f(n: Int) -> Int\n    n\nverify f law g\n";
        assert!(super::provenance_marker_above(none, 3).is_none());
        // Empty payload (`// aver:provenance ` with nothing after) is not a
        // valid marker → None.
        let empty = "// aver:provenance \nverify f law g\n";
        assert!(super::provenance_marker_above(empty, 2).is_none());
    }

    #[test]
    fn manifest_provenance_absent_when_none_present_when_some() {
        // No-op invariant: an unmarked law (`provenance: None`) serializes with
        // NO `provenance` key — byte-identical to a pre-provenance manifest — and
        // a marked law's payload round-trips verbatim through the tolerant parser.
        let unmarked = law("a.one", super::LawTier::Universal, &["propext"]);
        let mut marked = law("b.two", super::LawTier::Universal, &["propext"]);
        marked.provenance = Some("calculated from=c.stuck tool=explain".to_string());
        let json = super::proof_manifest_to_json(&manifest(vec![unmarked, marked]));
        assert_eq!(
            json.matches("\"provenance\"").count(),
            1,
            "exactly one law carries a provenance key:\n{json}"
        );
        let parsed = super::parse_proof_manifest(&json).expect("parses back");
        let a = parsed.laws.iter().find(|l| l.law == "a.one").unwrap();
        let b = parsed.laws.iter().find(|l| l.law == "b.two").unwrap();
        assert!(a.provenance.is_none(), "unmarked law has no provenance");
        assert_eq!(
            b.provenance.as_deref(),
            Some("calculated from=c.stuck tool=explain")
        );
    }

    #[test]
    fn residual_probe_strips_cascade_and_reuses_def_set() {
        // The probe re-emits the theorem statement verbatim (renamed), keeps the
        // intro + top-level `induction … with` skeleton, and replaces each arm's
        // closing cascade with NORMALIZATION-ONLY `(try simp only [<defs>,
        // List.cons_append])` — no `done`/`omega`/`split`/`simp_all`/`| sorry`.
        let thm = [
            "theorem f_law_x : ∀ (xs : List Int), g xs = h xs := by",
            "  intro xs",
            "  induction xs with",
            "  | nil => first | (simp [g, h]; done) | (simp [g, h]; omega) | sorry",
            "  | cons head tail ih => first | (simp_all [g, h]; done) | sorry",
        ];
        let body = aver::codegen::lean::residual_probe_body(&thm, "_probe0")
            .expect("a clean top-level induction shape is probeable");
        // Statement renamed, kept verbatim otherwise.
        assert!(body.contains("theorem _probe0 : ∀ (xs : List Int), g xs = h xs := by"));
        assert!(body.contains("intro xs"));
        assert!(body.contains("induction xs with"));
        // Both arms reduced to the normalization-only strip with the def set
        // harvested from the original arms + `List.cons_append`. The set is a
        // `BTreeSet`, so it renders sorted (`List.cons_append` < `g` < `h`).
        assert!(
            body.contains("| nil => (try simp only [List.cons_append, g, h])"),
            "nil arm not stripped to normalization-only:\n{body}"
        );
        assert!(
            body.contains("| cons head tail ih => (try simp only [List.cons_append, g, h])"),
            "cons arm not stripped to normalization-only:\n{body}"
        );
        // NONE of the closing tactics survive — that is the whole point (a leftover
        // `omega`/`split` would throw `No usable constraints` instead of leaving a
        // clean residual).
        for banned in ["done", "omega", "split", "simp_all", "sorry"] {
            assert!(
                !body.contains(banned),
                "probe must not contain `{banned}`:\n{body}"
            );
        }
    }

    #[test]
    fn residual_probe_dump_splits_and_drops_nat_bridge_lemma() {
        // The dump arm (Some label) splits a blocked conditional (bounded two
        // levels) + re-strips every goal + dumps all, and drops the Nat-comparison
        // bridge lemma (`*_isNatLe`) from the strip (it would rewrite the
        // `le … = true` claim into a builtin `≤` the un-translator declines).
        let thm = [
            "theorem f_law_x : ∀ (xs : List Nat), le (g xs) (h xs) = true := by",
            "  intro xs",
            "  induction xs with",
            "  | nil => first | (simp only [g, h, le, f_x_le_isNatLe]; done) | sorry",
            "  | cons head tail ih => first | (simp_all [g, h, le, f_x_le_isNatLe]; done) | sorry",
        ];
        let body = aver::codegen::lean::residual_probe_body_dump(&thm, "_probe0", Some("f.x"))
            .expect("a clean top-level induction shape is probeable");
        assert!(
            body.contains("<;> (try split) <;> (try split) <;>"),
            "dump arm must split blocked matches:\n{body}"
        );
        assert!(
            body.contains("all_goals (try aver_dump_goal \"f.x\")"),
            "dump arm must dump all resulting goals:\n{body}"
        );
        // Def unfolds + list peel survive (sorted `BTreeSet`); bridge lemma gone.
        assert!(
            body.contains("(try simp only [List.cons_append, g, h, le])"),
            "dump strip missing the def set:\n{body}"
        );
        assert!(
            !body.contains("f_x_le_isNatLe"),
            "nat-bridge lemma must be excluded from the dump strip:\n{body}"
        );
        for banned in ["done", "omega", "simp_all", "sorry"] {
            assert!(
                !body.contains(banned),
                "probe must not contain `{banned}`:\n{body}"
            );
        }
    }

    #[test]
    fn residual_probe_rejects_unprobeable_shapes() {
        // A single-line `native_decide` / `rcases` body has no top-level
        // `induction … with` to strip → `None` (the caller leaves open_goal None
        // rather than emit a misleading empty residual).
        let native = ["theorem f_law_x : P := by native_decide"];
        assert!(aver::codegen::lean::residual_probe_body(&native, "_p").is_none());
        let rcases = [
            "theorem f_law_x : ∀ n, P n := by",
            "  intro n",
            "  rcases h with a | b",
            "  · simp",
        ];
        assert!(aver::codegen::lean::residual_probe_body(&rcases, "_p").is_none());
    }

    #[test]
    fn parse_lean_diag_header_matches_lake_env_lean_form() {
        // `lake env lean` prints `<file>:<line>:<col>: error: <msg>` — the form
        // the residual parser keys on (NOT the `lake build` `error: <path>:…`
        // shape). The header parse yields (line, msg) and rejects non-headers.
        assert_eq!(
            super::parse_lean_diag_header("_aver_residual_probe.lean:5:8: error: unsolved goals"),
            Some((5, "unsolved goals".to_string()))
        );
        assert_eq!(
            super::parse_lean_diag_header("Foo.lean:32:8: warning: declaration uses 'sorry'"),
            Some((32, "declaration uses 'sorry'".to_string()))
        );
        // Body lines (indented goal text) are NOT headers.
        assert!(super::parse_lean_diag_header("case cons").is_none());
        assert!(super::parse_lean_diag_header("  ⊢ g xs = h xs").is_none());
        // No severity marker → not a header.
        assert!(super::parse_lean_diag_header("info: building").is_none());
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
    fn axiom_credit_keys_on_per_declaration_evidence() {
        // Per-theorem universal crediting: the decision is a pure parse
        // over the `#print axioms` probe output, keyed to ONE declaration
        // — never an invocation exit code. Each negative below is a
        // distinct no-credit class.
        let thm = "foo_law_bar_universal";
        // Credited: declaration present + whitelisted axioms.
        let clean =
            "'foo_law_bar_universal' depends on axioms: [propext, Classical.choice, Quot.sound]";
        assert!(super::theorem_credit_from_axioms(clean, thm));
        let axiom_free = "'foo_law_bar_universal' does not depend on any axioms";
        assert!(super::theorem_credit_from_axioms(axiom_free, thm));
        // No credit: missing declaration (probe errored — there is no
        // result line for the theorem at all).
        let missing = "error: unknown constant 'someOtherName'";
        assert!(!super::theorem_credit_from_axioms(missing, thm));
        // No credit: sorry-floored proof (sorryAx in the dependency set).
        let sorried = "'foo_law_bar_universal' depends on axioms: [propext, sorryAx]";
        assert!(!super::theorem_credit_from_axioms(sorried, thm));
        // No credit: native_decide (Lean.ofReduceBool).
        let native = "'foo_law_bar_universal' depends on axioms: [propext, Lean.ofReduceBool]";
        assert!(!super::theorem_credit_from_axioms(native, thm));
        // No credit: any axiom outside the whitelist.
        let extra = "'foo_law_bar_universal' depends on axioms: [propext, smuggledAxiom]";
        assert!(!super::theorem_credit_from_axioms(extra, thm));
        // No credit: a DIFFERENT declaration's clean line cannot pay
        // for ours (per-declaration, not per-invocation).
        let other = "'other_thm' depends on axioms: [propext]";
        assert!(!super::theorem_credit_from_axioms(other, thm));
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

    #[test]
    fn parse_lean_decl_location_extracts_relative_path_and_line() {
        // The gate-build sorry warning `lean_sorry_laws` keys on. Relative path
        // + line must survive a `warning:` prefix, a `././` build path prefix,
        // and the backtick glyph — the line is mapped to the enclosing theorem.
        assert_eq!(
            super::parse_lean_decl_location(
                "warning: Warehouse.lean:105:8: declaration uses `sorry`"
            ),
            Some(("Warehouse.lean".to_string(), 105))
        );
        assert_eq!(
            super::parse_lean_decl_location(
                "warning: ././Warehouse.lean:105:8: declaration uses 'sorry'"
            ),
            Some(("Warehouse.lean".to_string(), 105))
        );
        assert_eq!(
            super::parse_lean_decl_location(
                "warning: ././Infra/Store.lean:27:8: declaration uses 'sorry'"
            ),
            Some(("Infra/Store.lean".to_string(), 27))
        );
        // A line with no `.lean:<digits>` is not a location.
        assert_eq!(super::parse_lean_decl_location("error: build failed"), None);
    }

    #[test]
    fn dafny_trust_escape_counts_recurse_into_module_directories() {
        let root =
            std::env::temp_dir().join(format!("aver-dafny-recursive-count-{}", std::process::id()));
        let nested = root.join("Infra");
        std::fs::create_dir_all(&nested).unwrap();
        std::fs::write(
            root.join("Main.dfy"),
            "assume {:axiom} true;\n// sample-only (universal lemma omitted)\n",
        )
        .unwrap();
        std::fs::write(
            nested.join("Store.dfy"),
            "assume {:axiom} true;\nassume {:axiom} true;\n\
             // sample-only (universal lemma omitted)\n",
        )
        .unwrap();
        std::fs::write(nested.join("Ignored.txt"), "assume {:axiom}").unwrap();

        assert_eq!(super::count_dafny_axioms(root.to_str().unwrap()), 3);
        assert_eq!(
            super::count_dafny_omitted_universals(root.to_str().unwrap()),
            2
        );
        std::fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn count_lean_build_errors_counts_hard_errors_not_cascade_or_sorries() {
        // Verbatim captured pre-fix output of probe_a.av (the DEFECT-1 file):
        // one source-located hard error escaped the sorry floor while the
        // check-json line reported only `sorries:1`. The counter must see the
        // ONE real Lean error, ignore the `sorry` warning, and ignore lake's
        // cascade lines (`Lean exited with code 1`, `build failed`) that carry
        // no `.lean:` location — otherwise one failure would read as three.
        let probe_a = "\
warning: SosProbeA.lean:65:49: This simp argument is unused:
error: SosProbeA.lean:100:113: Tactic `assumption` failed
warning: SosProbeA.lean:175:8: declaration uses `sorry`
error: Lean exited with code 1
Some required targets logged failures:
- SosProbeA
error: build failed";
        assert_eq!(super::count_lean_build_errors(probe_a), 1);
        // The genuine sorry warning is NOT a build error.
        assert_eq!(super::count_lean_sorries(probe_a), 1);
        // A deterministic whnf timeout (probe_b2's DEFECT-2 shape) is also a
        // hard, source-located error.
        let probe_b2 = "error: SosProbeB2.lean:69:0: (deterministic) timeout at 'whnf', maximum number of heartbeats (200000)";
        assert_eq!(super::count_lean_build_errors(probe_b2), 1);
        // A clean build has zero.
        assert_eq!(
            super::count_lean_build_errors("Build completed successfully"),
            0
        );
    }

    #[test]
    fn count_dafny_timeouts_counts_timed_out_lemmas_not_summary() {
        // Verbatim captured lines from the k5_fdiv round.av Dafny run
        // (prompts/probe-artifacts/dafny-parity/.../run2.log): 12 lemmas timed
        // out while `errors` reported only 2. Each timeout is one
        // `timed out after` line; the "N time outs" summary (space) and the two
        // postcondition errors must NOT be counted as timeouts.
        let run2 = "\
Round.dfy(244,26): Error: Verification of 'floorDiv_dividesPow2Multiple' timed out after 30 seconds. (the limit can be increased using --verification-time-limit)
Round.dfy(278,26): Error: Verification of 'floorDiv_nestedFloorCollapse' timed out after 30 seconds. (the limit can be increased using --verification-time-limit)
Round.dfy(340,26): Error: Verification of 'floorDiv_absorbRemainder' timed out after 30 seconds. (the limit can be increased using --verification-time-limit)
Round.dfy(470,74): Error: Verification of 'truncErrorMagnitudeNonneg_nonneg' timed out after 30 seconds. (the limit can be increased using --verification-time-limit)
Round.dfy(492,0): Error: a postcondition could not be proved on this return path
Round.dfy(491,33): Related location: this is the postcondition that could not be proved
Round.dfy(520,81): Error: Verification of 'truncErrorReconstructs_reconstructsValue' timed out after 30 seconds. (the limit can be increased using --verification-time-limit)
Round.dfy(554,0): Error: a postcondition could not be proved on this return path
Round.dfy(553,32): Related location: this is the postcondition that could not be proved
Round.dfy(582,58): Error: Verification of 'truncErrorSameSign_signCondition' timed out after 30 seconds. (the limit can be increased using --verification-time-limit)
Round.dfy(737,50): Error: Verification of 'truncComposes_composesToInner' timed out after 30 seconds. (the limit can be increased using --verification-time-limit)
Round.dfy(768,72): Error: Verification of 'awayTruncComposes_composesToInner' timed out after 30 seconds. (the limit can be increased using --verification-time-limit)
Round.dfy(799,76): Error: Verification of 'truncStickyComposes_composesThroughSticky' timed out after 30 seconds. (the limit can be increased using --verification-time-limit)
Round.dfy(830,78): Error: Verification of 'awayErrorReconstructs_reconstructsValue' timed out after 30 seconds. (the limit can be increased using --verification-time-limit)
Round.dfy(861,53): Error: Verification of 'awayErrorBound_strictBound' timed out after 30 seconds. (the limit can be increased using --verification-time-limit)
Round.dfy(923,57): Error: Verification of 'stickyErrorBound_strictBound' timed out after 30 seconds. (the limit can be increased using --verification-time-limit)

Dafny program verifier finished with 158 verified, 2 errors, 12 time outs";
        assert_eq!(super::count_dafny_timeouts(run2), 12);
        // The `errors` parser still reports 2 on the same capture — the two are
        // orthogonal counts.
        assert_eq!(super::parse_dafny_error_count(run2), Some(2));
        // A clean run has zero timeouts.
        assert_eq!(
            super::count_dafny_timeouts(
                "Dafny program verifier finished with 8 verified, 0 errors"
            ),
            0
        );
    }

    fn empty_codegen_ctx() -> CodegenContext {
        CodegenContext {
            items: vec![],
            type_defs: vec![],
            fn_defs: vec![],
            project_name: "test".to_string(),
            modules: vec![],
            capabilities: Default::default(),
            module_prefixes: HashSet::new(),
            policy: None,
            emit_replay_runtime: false,
            runtime_policy_from_env: false,
            guest_entry: None,
            emit_self_host_support: false,
            mutual_tco_members: HashSet::new(),
            recursive_fns: HashSet::new(),
            buffer_build_sinks: HashMap::new(),
            buffer_fusion_sites: Vec::new(),
            synthesized_buffered_fns: Vec::new(),
            packed_sequence_layouts: HashMap::new(),
            proof_ir: aver::ir::ProofIR::default(),
            symbol_table: aver::ir::SymbolTable::default(),
            current_module_scope: std::cell::RefCell::new(None),
            lean_do_block: std::cell::Cell::new(false),
            declined_claims: std::cell::RefCell::new(std::collections::BTreeMap::new()),
            substituted_compile_errors: std::cell::RefCell::new(Vec::new()),
            omitted_verify_cases: std::cell::RefCell::new(Vec::new()),
            resolved_program: aver::codegen::program_view::ResolvedProgramView::default(),
            program_shape: None,
            mir_program: None,
            bare_i64: Default::default(),
            discovered_lemmas: Vec::new(),
            sample_expected: std::collections::HashMap::new(),
            declined_cases: std::collections::HashMap::new(),
            allow_mathlib: false,
            hand_proofs: Default::default(),
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
    fn suppression_path_strips_dot_slash_and_relativizes() {
        let dir = temp_case_dir("suppress_key");
        let nested = dir.join("domain");
        fs::create_dir_all(&nested).expect("create nested dir");
        fs::write(nested.join("version.av"), "module Version\n").expect("write version.av");
        let root = dir.to_str().expect("utf8 path");

        assert_eq!(
            suppression_path(nested.join("version.av").to_str().expect("utf8 path"), root),
            "domain/version.av"
        );
        assert_eq!(
            suppression_path(
                dir.join("./domain/version.av").to_str().expect("utf8 path"),
                root
            ),
            "domain/version.av"
        );
        // No filesystem answer available: fall back to a lexical cleanup.
        assert_eq!(
            suppression_path("./domain/missing.av", "."),
            "domain/missing.av"
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

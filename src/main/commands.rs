use std::collections::{HashMap, HashSet};
use std::fs;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
use std::process;
use std::sync::Arc as Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use colored::Colorize;

use aver::ast::{
    Expr, FnBody, FnDef, Pattern, Spanned, Stmt, TopLevel, TypeDef, VerifyBlock, VerifyKind,
};
use aver::checker::{
    CheckFinding, VerifyResult, check_module_intent_with_sigs_in, collect_cse_warnings_in,
    collect_independence_warnings_in, collect_perf_warnings_in,
    collect_verify_coverage_warnings_in, collect_verify_law_dependency_warnings_in, expr_to_str,
    index_decisions, merge_verify_blocks,
};
use aver::codegen;
use aver::codegen::ModuleInfo;
use aver::codegen::lean as lean_codegen;
use aver::codegen::rust as rust_codegen;
use aver::nan_value::{Arena, NanValueConvert};
use aver::resolver;
use aver::source::{find_module_file, require_module_declaration};
use aver::tail_check::collect_non_tail_recursion_warnings_with_sigs;
use aver::tco;
use aver::types::checker::run_type_check_full;
use aver::types::{Type, parse_type_str};
use aver::value::{Value, aver_repr};
use aver::verify_law::{
    collect_contextual_helper_law_hints, collect_missing_helper_law_hints,
    contextual_helper_law_message, missing_helper_law_message,
};
use aver::vm;

use super::diagnostic;

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

fn hashed_cache_dir(label: &str, key: &str) -> PathBuf {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    label.hash(&mut hasher);
    key.hash(&mut hasher);
    let hash = hasher.finish();
    std::env::temp_dir().join(format!("{label}-{hash:016x}"))
}

fn self_host_paths() -> (PathBuf, PathBuf) {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("self_hosted");
    let file = root.join("main.av");
    (file, root)
}

fn self_host_binary_path(cache_dir: &Path) -> PathBuf {
    cache_dir.join("target").join("release").join(format!(
        "aver_self_host_cli{}",
        std::env::consts::EXE_SUFFIX
    ))
}

fn self_host_build_fingerprint() -> Result<String, String> {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    env!("CARGO_PKG_VERSION").hash(&mut hasher);

    if let Ok(exe) = std::env::current_exe() {
        exe.to_string_lossy().hash(&mut hasher);
        let meta = fs::metadata(&exe)
            .map_err(|e| format!("Cannot stat current executable '{}': {}", exe.display(), e))?;
        meta.len().hash(&mut hasher);
        if let Ok(modified) = meta.modified()
            && let Ok(delta) = modified.duration_since(UNIX_EPOCH)
        {
            delta.as_secs().hash(&mut hasher);
            delta.subsec_nanos().hash(&mut hasher);
        }
    }

    let (_self_host_file, self_host_root) = self_host_paths();
    let mut sources = Vec::new();
    collect_av_input_files(&self_host_root, &mut sources)?;
    sources.sort();
    for path in sources {
        path.strip_prefix(&self_host_root)
            .unwrap_or(&path)
            .to_string_lossy()
            .hash(&mut hasher);
        fs::read(&path)
            .map_err(|e| format!("Cannot read self-host source '{}': {}", path.display(), e))?
            .hash(&mut hasher);
    }

    Ok(format!("{:016x}", hasher.finish()))
}

pub(super) fn build_self_host_binary(show_progress: bool) -> Result<PathBuf, String> {
    let (self_host_file, self_host_root) = self_host_paths();
    let expected_fingerprint = self_host_build_fingerprint()?;
    let cache_dir = hashed_cache_dir("aver-self-host", &expected_fingerprint);
    let binary_path = self_host_binary_path(&cache_dir);
    let fingerprint_path = cache_dir.join(".fingerprint");

    if binary_path.exists()
        && fs::read_to_string(&fingerprint_path)
            .ok()
            .is_some_and(|stored| stored.trim() == expected_fingerprint)
    {
        return Ok(binary_path);
    }

    let self_host_file_str = path_to_string(&self_host_file);
    let self_host_root_str = path_to_string(&self_host_root);
    if show_progress {
        eprintln!("Self-host: generating cached helper code...");
    }
    let (mut ctx, _) = build_codegen_context(
        &self_host_file_str,
        Some("aver_self_host_cli"),
        Some(&self_host_root_str),
        true,
        &super::cli::CompilePolicyMode::Runtime,
        Some("runGuestCliProgram"),
        true,
    );

    let output = with_local_runtime_override(|| rust_codegen::transpile(&mut ctx));
    if show_progress {
        eprintln!("Self-host: materializing helper project...");
    }
    materialize_codegen_output(&cache_dir, &output)?;

    if show_progress {
        eprintln!("Self-host: building cached helper binary...");
    }
    let build = process::Command::new("cargo")
        .arg("build")
        .arg("--quiet")
        .arg("--release")
        .arg("--offline")
        .current_dir(&cache_dir)
        .output()
        .map_err(|e| {
            format!(
                "Failed to build cached self-host binary in '{}': {}",
                cache_dir.display(),
                e
            )
        })?;
    if !build.status.success() {
        let stdout = String::from_utf8_lossy(&build.stdout).trim().to_string();
        let stderr = String::from_utf8_lossy(&build.stderr).trim().to_string();
        let mut msg = format!(
            "Failed to build cached self-host binary in '{}'",
            cache_dir.display()
        );
        if !stdout.is_empty() {
            msg.push_str(&format!("\nstdout:\n{}", stdout));
        }
        if !stderr.is_empty() {
            msg.push_str(&format!("\nstderr:\n{}", stderr));
        }
        return Err(msg);
    }

    fs::write(&fingerprint_path, format!("{expected_fingerprint}\n")).map_err(|e| {
        format!(
            "Cannot write self-host fingerprint '{}': {}",
            fingerprint_path.display(),
            e
        )
    })?;

    if show_progress {
        eprintln!("Self-host: helper ready.");
    }

    Ok(binary_path)
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
        let items = parse_file(&source)?;
        require_module_declaration(&items, &path_str)?;

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

    let run_result = machine.run();

    // Persist recording if requested.
    if let Some(dir) = record_dir {
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

        let recording = SessionRecording {
            schema_version: 1,
            request_id,
            timestamp,
            program_file: record_program_file,
            module_root: record_module_root,
            entry_fn: "main".to_string(),
            input: JsonValue::Null,
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
        sorted.sort_by(|a, b| b.count.cmp(&a.count));
        for op in sorted.iter().take(20).filter(|o| o.count > 0) {
            let pct = op.count as f64 / report.total_opcodes as f64 * 100.0;
            eprintln!("  {:>22} {:>12}  ({:.1}%)", op.name, op.count, pct);
        }
        eprintln!("\nTop functions (by entries):");
        let mut fns = report.functions.clone();
        fns.sort_by(|a, b| b.entries.cmp(&a.entries));
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
                eprintln!("{}", format!("Main returned error: {}", msg).red());
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
        match run_verify_for_items_vm(items, &module_root, file) {
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
    let module = Module::new(&engine, wasm_bytes).map_err(|e| format!("Module error: {e:#}"))?;
    let mut store = Store::new(&engine, ());
    let mut linker = Linker::new(&engine);

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
    let binary_path = match build_self_host_binary(true) {
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
        cmd_verify(file, module_root_override, false, false, false);
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
        let mut transformed = items.clone();
        tco::transform_program(&mut transformed);

        // --- Collect all diagnostics ---
        let tc_result = run_type_check_full(items, Some(module_root));
        let non_tail_warnings =
            collect_non_tail_recursion_warnings_with_sigs(&transformed, &tc_result.fn_sigs);
        let findings =
            check_module_intent_with_sigs_in(items, Some(&tc_result.fn_sigs), Some(path));
        let coverage_warnings = collect_verify_coverage_warnings_in(items, Some(path));
        let law_dependency_warnings =
            collect_verify_law_dependency_warnings_in(items, &tc_result.fn_sigs, Some(path));
        let cse_warnings = collect_cse_warnings_in(&transformed, Some(path));
        let perf_warnings = collect_perf_warnings_in(&transformed, Some(path));
        let independence_warnings =
            collect_independence_warnings_in(&transformed, &tc_result.fn_sigs, Some(path));
        let unused_exposes_warnings = unused_exposes_by_file
            .get(&canonical_path_key(path))
            .cloned()
            .unwrap_or_default();

        let has_errors = !tc_result.errors.is_empty() || !findings.errors.is_empty();

        // --- Collect all diagnostics (errors first, then warnings) ---
        let mut diagnostics = Vec::new();

        for te in &tc_result.errors {
            diagnostics.push(diagnostic::from_type_error(te, source, &shown_path));
        }
        for e in &findings.errors {
            diagnostics.push(diagnostic::from_check_finding(
                diagnostic::Severity::Error,
                e,
                source,
                &shown_path,
            ));
        }
        for (binding_name, fn_name, line) in &tc_result.unused_bindings {
            diagnostics.push(diagnostic::unused_binding_diagnostic(
                binding_name,
                fn_name,
                *line,
                source,
                &shown_path,
            ));
        }
        for w in findings
            .warnings
            .iter()
            .chain(coverage_warnings.iter())
            .chain(law_dependency_warnings.iter())
            .chain(cse_warnings.iter())
            .chain(perf_warnings.iter())
            .chain(independence_warnings.iter())
            .chain(unused_exposes_warnings.iter())
        {
            diagnostics.push(diagnostic::from_check_finding(
                diagnostic::Severity::Warning,
                w,
                source,
                &shown_path,
            ));
        }
        for warning in &non_tail_warnings {
            // Deduplicate callsite lines: multiple calls on the same line
            // get a single extra_span with a count suffix.
            let mut line_counts: Vec<(usize, usize)> = Vec::new();
            for &ln in &warning.callsite_lines {
                if let Some(entry) = line_counts.iter_mut().find(|(l, _)| *l == ln) {
                    entry.1 += 1;
                } else {
                    line_counts.push((ln, 1));
                }
            }
            let max_shown = 3;
            let extra_spans: Vec<_> = line_counts
                .iter()
                .take(max_shown)
                .map(|&(ln, count)| {
                    let label = if count > 1 {
                        format!("{} non-tail calls", count)
                    } else {
                        "non-tail call".to_string()
                    };
                    aver::checker::FindingSpan {
                        line: ln,
                        col: 0,
                        len: 0,
                        label,
                    }
                })
                .collect();
            let finding = CheckFinding {
                line: warning.line,
                module: None,
                file: Some(path.to_string()),
                fn_name: Some(warning.fn_name.clone()),
                message: warning.message.clone(),
                extra_spans,
            };
            diagnostics.push(diagnostic::from_check_finding(
                diagnostic::Severity::Warning,
                &finding,
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
        for (i, diag) in diagnostics.iter().enumerate() {
            if json {
                println!("{}", diag.render_json().trim());
            } else {
                if i > 0 {
                    println!();
                }
                print!("{}", diag.render(verbose));
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

struct VmVerifyCaseFns {
    left: String,
    right: String,
    guard: Option<String>,
}

struct VmVerifyPlan {
    block: VerifyBlock,
    cases: Vec<VmVerifyCaseFns>,
}

enum VmVerifyEval {
    Value(Value),
    ErrProp(Value),
}

fn make_verify_vm_helper(
    name: String,
    line: usize,
    expr: Spanned<Expr>,
    wrap_result: bool,
) -> TopLevel {
    let body_expr = if wrap_result {
        Spanned::new(
            Expr::Constructor("Result.Ok".to_string(), Some(Box::new(expr))),
            line,
        )
    } else {
        expr
    };

    TopLevel::FnDef(FnDef {
        name,
        line,
        params: vec![],
        return_type: "Unit".to_string(),
        effects: vec![],
        desc: None,
        body: Rc::new(FnBody::from_expr(body_expr)),
        resolution: None,
    })
}

fn build_verify_vm_plans(
    items: &mut Vec<TopLevel>,
    verify_blocks: &[VerifyBlock],
) -> Vec<VmVerifyPlan> {
    let mut plans = Vec::with_capacity(verify_blocks.len());

    for (block_idx, block) in verify_blocks.iter().enumerate() {
        let mut case_plans = Vec::with_capacity(block.cases.len());
        let sample_guards = match &block.kind {
            VerifyKind::Law(law) => Some(&law.sample_guards),
            VerifyKind::Cases => None,
        };

        for (case_idx, (left_expr, right_expr)) in block.cases.iter().cloned().enumerate() {
            let prefix = format!("__verify_{}_{}_{}", block.fn_name, block_idx, case_idx);
            let left_name = format!("{}_left", prefix);
            let right_name = format!("{}_right", prefix);
            items.push(make_verify_vm_helper(
                left_name.clone(),
                block.line,
                left_expr,
                true,
            ));
            items.push(make_verify_vm_helper(
                right_name.clone(),
                block.line,
                right_expr,
                true,
            ));

            let guard_name = sample_guards
                .and_then(|guards| guards.get(case_idx))
                .cloned()
                .map(|guard_expr| {
                    let name = format!("{}_guard", prefix);
                    items.push(make_verify_vm_helper(
                        name.clone(),
                        block.line,
                        guard_expr,
                        false,
                    ));
                    name
                });

            case_plans.push(VmVerifyCaseFns {
                left: left_name,
                right: right_name,
                guard: guard_name,
            });
        }

        plans.push(VmVerifyPlan {
            block: block.clone(),
            cases: case_plans,
        });
    }

    plans
}

fn vm_call_verify_helper(machine: &mut vm::VM, fn_name: &str) -> Result<VmVerifyEval, String> {
    let value = machine
        .run_named_function(fn_name, &[])
        .map_err(|e| e.to_string())?
        .to_value(&machine.arena);

    match value {
        Value::Ok(inner) => Ok(VmVerifyEval::Value(*inner)),
        Value::Err(inner) => Ok(VmVerifyEval::ErrProp(*inner)),
        other => Err(format!(
            "verify helper '{}' returned unexpected shape: {}",
            fn_name,
            aver_repr(&other)
        )),
    }
}

fn vm_call_guard_helper(machine: &mut vm::VM, fn_name: &str) -> Result<Value, String> {
    machine
        .run_named_function(fn_name, &[])
        .map_err(|e| e.to_string())
        .map(|value| value.to_value(&machine.arena))
}

fn run_verify_vm(plan: &VmVerifyPlan, machine: &mut vm::VM) -> VerifyResult {
    use aver::checker::{VerifyCaseOutcome, VerifyCaseResult, VerifyLawContext};

    let block = &plan.block;
    let mut passed = 0;
    let mut failed = 0;
    let mut skipped = 0;
    let mut failures = Vec::new();
    let mut case_results = Vec::new();
    let is_law = matches!(block.kind, VerifyKind::Law(_));
    let case_total = block.cases.len();

    let law_context_template = if let VerifyKind::Law(law) = &block.kind {
        Some(format!(
            "{} == {}",
            expr_to_str(&law.lhs),
            expr_to_str(&law.rhs)
        ))
    } else {
        None
    };

    for (idx, ((left_expr, right_expr), case_fns)) in
        block.cases.iter().zip(&plan.cases).enumerate()
    {
        let case_str = format!("{} == {}", expr_to_str(left_expr), expr_to_str(right_expr));
        let span = block.case_spans.get(idx).cloned();
        let failure_case = if is_law {
            format!("case {}/{} [{}]", idx + 1, case_total, case_str)
        } else {
            case_str.clone()
        };

        let law_context = if let VerifyKind::Law(_) = &block.kind {
            let givens: Vec<(String, String)> = block
                .case_givens
                .get(idx)
                .map(|gs| {
                    gs.iter()
                        .map(|(name, expr)| (name.clone(), expr_to_str(expr)))
                        .collect()
                })
                .unwrap_or_default();
            Some(VerifyLawContext {
                givens,
                law_expr: law_context_template.clone().unwrap_or_default(),
            })
        } else {
            None
        };

        // Check law sample guard
        if let Some(guard_name) = &case_fns.guard {
            match vm_call_guard_helper(machine, guard_name) {
                Ok(Value::Bool(true)) => {}
                Ok(Value::Bool(false)) => {
                    skipped += 1;
                    case_results.push(VerifyCaseResult {
                        outcome: VerifyCaseOutcome::Skipped,
                        span,
                        case_expr: case_str,
                        case_index: idx,
                        case_total,
                        law_context,
                    });
                    continue;
                }
                Ok(Value::Err(err_val)) => {
                    failed += 1;
                    let err_repr = format!("Result.Err({})", aver_repr(&err_val));
                    failures.push((failure_case, String::new(), err_repr.clone()));
                    case_results.push(VerifyCaseResult {
                        outcome: VerifyCaseOutcome::UnexpectedErr { err_repr },
                        span,
                        case_expr: case_str,
                        case_index: idx,
                        case_total,
                        law_context,
                    });
                    continue;
                }
                Ok(other) => {
                    failed += 1;
                    let error = format!("when produced {}, expected Bool", aver_repr(&other));
                    failures.push((failure_case, "Bool".to_string(), error.clone()));
                    case_results.push(VerifyCaseResult {
                        outcome: VerifyCaseOutcome::RuntimeError { error },
                        span,
                        case_expr: case_str,
                        case_index: idx,
                        case_total,
                        law_context,
                    });
                    continue;
                }
                Err(e) => {
                    failed += 1;
                    let error = format!("guard error: {}", e);
                    failures.push((failure_case, String::new(), error.clone()));
                    case_results.push(VerifyCaseResult {
                        outcome: VerifyCaseOutcome::RuntimeError { error },
                        span,
                        case_expr: case_str,
                        case_index: idx,
                        case_total,
                        law_context,
                    });
                    continue;
                }
            }
        }

        let left_result = vm_call_verify_helper(machine, &case_fns.left);
        let right_result = vm_call_verify_helper(machine, &case_fns.right);

        match (left_result, right_result) {
            (Ok(VmVerifyEval::Value(left_val)), Ok(VmVerifyEval::Value(right_val))) => {
                if left_val == right_val {
                    passed += 1;
                    case_results.push(VerifyCaseResult {
                        outcome: VerifyCaseOutcome::Pass,
                        span,
                        case_expr: case_str,
                        case_index: idx,
                        case_total,
                        law_context,
                    });
                } else {
                    failed += 1;
                    let expected = aver_repr(&right_val);
                    let actual = aver_repr(&left_val);
                    failures.push((failure_case, expected.clone(), actual.clone()));
                    case_results.push(VerifyCaseResult {
                        outcome: VerifyCaseOutcome::Mismatch { expected, actual },
                        span,
                        case_expr: case_str,
                        case_index: idx,
                        case_total,
                        law_context,
                    });
                }
            }
            (Ok(VmVerifyEval::ErrProp(err_val)), _) | (_, Ok(VmVerifyEval::ErrProp(err_val))) => {
                failed += 1;
                let err_repr = format!("Result.Err({})", aver_repr(&err_val));
                failures.push((failure_case, String::new(), err_repr.clone()));
                case_results.push(VerifyCaseResult {
                    outcome: VerifyCaseOutcome::UnexpectedErr { err_repr },
                    span,
                    case_expr: case_str,
                    case_index: idx,
                    case_total,
                    law_context,
                });
            }
            (Err(e), _) | (_, Err(e)) => {
                failed += 1;
                let error = e.to_string();
                failures.push((failure_case, String::new(), error.clone()));
                case_results.push(VerifyCaseResult {
                    outcome: VerifyCaseOutcome::RuntimeError { error },
                    span,
                    case_expr: case_str,
                    case_index: idx,
                    case_total,
                    law_context,
                });
            }
        }
    }

    let block_label = match &block.kind {
        VerifyKind::Law(law) => format!("{} spec {}", block.fn_name, law.name),
        VerifyKind::Cases => block.fn_name.clone(),
    };
    VerifyResult {
        fn_name: block.fn_name.clone(),
        block_label,
        passed,
        failed,
        skipped,
        case_results,
        failures,
    }
}

fn run_verify_for_items_vm(
    mut items: Vec<TopLevel>,
    module_root: &str,
    source_file: &str,
) -> Result<Vec<VerifyResult>, String> {
    tco::transform_program(&mut items);

    let tc_result = run_type_check_full(&items, Some(module_root));
    if !tc_result.errors.is_empty() {
        return Err(format_type_errors(&tc_result.errors));
    }

    let verify_blocks = merge_verify_blocks(&items);
    if verify_blocks.is_empty() {
        return Ok(vec![]);
    }

    let plans = build_verify_vm_plans(&mut items, &verify_blocks);
    resolver::resolve_program(&mut items);

    let mut arena = Arena::new();
    vm::register_service_types(&mut arena);
    let (code, globals) =
        vm::compile_program_with_modules(&items, &mut arena, Some(module_root), source_file)
            .map_err(|e| format!("VM compile error: {}", e))?;
    let mut machine = vm::VM::new(code, globals, arena);
    apply_runtime_policy_to_vm(&mut machine, module_root)?;

    let mut results = Vec::new();
    for plan in &plans {
        results.push(run_verify_vm(plan, &mut machine));
    }
    Ok(results)
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
) -> Result<Vec<VerifyFileResult>, String> {
    let units = collect_check_units(file, module_root, deps)?;
    let mut file_results = Vec::new();

    for (path, source, items) in units {
        let blocks = run_verify_for_items_vm(items, module_root, &path)?;
        file_results.push(VerifyFileResult {
            path,
            source,
            blocks,
        });
    }

    Ok(file_results)
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
            // NDJSON mode
            for block in &fr.blocks {
                // block-result event
                let mut failure_counts: std::collections::HashMap<&str, usize> =
                    std::collections::HashMap::new();
                for cr in &block.case_results {
                    match &cr.outcome {
                        VerifyCaseOutcome::Mismatch { .. } => {
                            *failure_counts.entry("verify-mismatch").or_default() += 1
                        }
                        VerifyCaseOutcome::RuntimeError { .. } => {
                            *failure_counts.entry("verify-runtime-error").or_default() += 1
                        }
                        VerifyCaseOutcome::UnexpectedErr { .. } => {
                            *failure_counts.entry("verify-unexpected-err").or_default() += 1
                        }
                        _ => {}
                    }
                }
                let failures_json: Vec<String> = failure_counts
                    .iter()
                    .map(|(k, v)| format!("\"{}\":{}", k, v))
                    .collect();
                println!(
                    "{{\"schema_version\":1,\"kind\":\"block-result\",\"file\":{},\"block\":{},\"passed\":{},\"failed\":{},\"skipped\":{},\"total\":{},\"failures\":{{{}}}}}",
                    diagnostic::json_escape(&display_path),
                    diagnostic::json_escape(&block.block_label),
                    block.passed,
                    block.failed,
                    block.skipped,
                    block.passed + block.failed + block.skipped,
                    failures_json.join(","),
                );

                // diagnostic events for failures
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
                        println!("{}", d.render_json().trim());
                    }
                }
            }
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
                    // Count failure types
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
                    let breakdown = if parts.is_empty() {
                        String::new()
                    } else {
                        format!(" ({})", parts.join(", "))
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

                // Emit diagnostics for failures (capped in normal mode)
                let max_diags = if verbose { usize::MAX } else { 3 };
                let mut diag_count = 0usize;
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
                        if diag_count < max_diags {
                            println!();
                            print!("{}", d.render(verbose));
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
) {
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
        match run_verify_for_file(file, &module_root, deps) {
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
    let total_cases = total_passed + total_failed;
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
        let summary = format!(
            "Summary: {} file{} | {} block{} | {}/{} cases passed | {} failed",
            total_files,
            if total_files == 1 { "" } else { "s" },
            total_blocks,
            if total_blocks == 1 { "" } else { "s" },
            total_passed,
            total_cases,
            total_failed,
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
        adapter,
        wasm_opt,
    } = opts;

    // WASM target: simplified pipeline, no replay/policy/guest-entry support yet
    if matches!(target, super::cli::CompileTarget::Wasm) {
        cmd_compile_wasm(
            file,
            output_dir,
            project_name,
            module_root_override,
            adapter,
            wasm_opt,
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

fn cmd_compile_wasm(
    file: &str,
    output_dir: &str,
    project_name: Option<&str>,
    module_root_override: Option<&str>,
    adapter: Option<super::cli::WasmAdapter>,
    wasm_opt: Option<super::cli::WasmOptMode>,
) {
    #[cfg(not(feature = "wasm"))]
    {
        let _ = (
            file,
            output_dir,
            project_name,
            module_root_override,
            adapter,
            wasm_opt,
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

        let wasm_adapter = match adapter {
            Some(super::cli::WasmAdapter::Wasi) => codegen::wasm::WasmAdapter::Wasi,
            _ => codegen::wasm::WasmAdapter::Aver,
        };
        match codegen::wasm::emit_wasm_with_adapter(&ctx, wasm_adapter) {
            Ok(wasm_bytes) => {
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

                let mut final_size = std::fs::metadata(&wasm_file)
                    .map(|m| m.len())
                    .unwrap_or(wasm_bytes.len() as u64);
                let mut compile_suffix = String::new();

                // If program uses effects (Console.print etc.), merge with runtime
                let needs_runtime =
                    !wasm_bytes.is_empty() && wasm_bytes.windows(7).any(|w| w == b"aver_rt");

                if needs_runtime {
                    // Try to find aver-wasm-rt.wasm
                    let rt_path = find_wasm_runtime();
                    if let Some(rt_path) = rt_path {
                        let merged_file = out_path.join(format!("{}_merged.wasm", wasm_name));
                        let merge_result = std::process::Command::new("wasm-merge")
                            .arg(&rt_path)
                            .arg("aver_rt")
                            .arg(&wasm_file)
                            .arg("program")
                            .arg("--rename-export-conflicts")
                            .arg("--enable-bulk-memory")
                            .arg("-o")
                            .arg(&merged_file)
                            .output();

                        match merge_result {
                            Ok(output) if output.status.success() => {
                                // Replace original with merged
                                let _ = std::fs::rename(&merged_file, &wasm_file);
                                final_size =
                                    std::fs::metadata(&wasm_file).map(|m| m.len()).unwrap_or(0);
                                let file_display = file.cyan();
                                let wasm_display = wasm_file.display().to_string().cyan();
                                if let Some(mode) = wasm_opt {
                                    let optimized_size = run_wasm_opt(&wasm_file, mode)
                                        .unwrap_or_else(|err| {
                                            eprintln!("{}", err.red());
                                            process::exit(1);
                                        });
                                    compile_suffix = format!(", wasm-opt {}", wasm_opt_label(mode));
                                    final_size = optimized_size;
                                }
                                println!(
                                    "{} {} → {} ({}{}, with runtime)",
                                    "Compiled".green().bold(),
                                    file_display,
                                    wasm_display,
                                    format_byte_size(final_size),
                                    compile_suffix
                                );
                            }
                            Ok(output) => {
                                let stderr = String::from_utf8_lossy(&output.stderr);
                                eprintln!(
                                    "{}",
                                    format!(
                                        "wasm-merge failed: {}. Run without runtime: wasmtime {}",
                                        stderr.trim(),
                                        wasm_file.display()
                                    )
                                    .yellow()
                                );
                                // Keep unmerged file
                                let file_display = file.cyan();
                                let wasm_display = wasm_file.display().to_string().cyan();
                                if let Some(mode) = wasm_opt {
                                    let optimized_size = run_wasm_opt(&wasm_file, mode)
                                        .unwrap_or_else(|err| {
                                            eprintln!("{}", err.red());
                                            process::exit(1);
                                        });
                                    compile_suffix = format!(", wasm-opt {}", wasm_opt_label(mode));
                                    final_size = optimized_size;
                                }
                                println!(
                                    "{} {} → {} ({}{}, unmerged)",
                                    "Compiled".green().bold(),
                                    file_display,
                                    wasm_display,
                                    format_byte_size(final_size),
                                    compile_suffix
                                );
                            }
                            Err(_) => {
                                eprintln!(
                                    "{}",
                                    "wasm-merge not found. Install binaryen: brew install binaryen"
                                        .yellow()
                                );
                                let file_display = file.cyan();
                                let wasm_display = wasm_file.display().to_string().cyan();
                                if let Some(mode) = wasm_opt {
                                    let optimized_size = run_wasm_opt(&wasm_file, mode)
                                        .unwrap_or_else(|err| {
                                            eprintln!("{}", err.red());
                                            process::exit(1);
                                        });
                                    compile_suffix = format!(", wasm-opt {}", wasm_opt_label(mode));
                                    final_size = optimized_size;
                                }
                                println!(
                                    "{} {} → {} ({}{}, unmerged)",
                                    "Compiled".green().bold(),
                                    file_display,
                                    wasm_display,
                                    format_byte_size(final_size),
                                    compile_suffix
                                );
                            }
                        }
                    } else {
                        eprintln!(
                            "{}",
                            "aver-wasm-rt.wasm not found. Build: cargo build --release --manifest-path aver-wasm-rt/Cargo.toml --target wasm32-wasip1"
                                .yellow()
                        );
                        let file_display = file.cyan();
                        let wasm_display = wasm_file.display().to_string().cyan();
                        if let Some(mode) = wasm_opt {
                            let optimized_size =
                                run_wasm_opt(&wasm_file, mode).unwrap_or_else(|err| {
                                    eprintln!("{}", err.red());
                                    process::exit(1);
                                });
                            compile_suffix = format!(", wasm-opt {}", wasm_opt_label(mode));
                            final_size = optimized_size;
                        }
                        println!(
                            "{} {} → {} ({}{}, unmerged)",
                            "Compiled".green().bold(),
                            file_display,
                            wasm_display,
                            format_byte_size(final_size),
                            compile_suffix
                        );
                    }
                } else {
                    let file_display = file.cyan();
                    let wasm_display = wasm_file.display().to_string().cyan();
                    if let Some(mode) = wasm_opt {
                        let optimized_size = run_wasm_opt(&wasm_file, mode).unwrap_or_else(|err| {
                            eprintln!("{}", err.red());
                            process::exit(1);
                        });
                        compile_suffix = format!(", wasm-opt {}", wasm_opt_label(mode));
                        final_size = optimized_size;
                    }
                    println!(
                        "{} {} → {} ({}{})",
                        "Compiled".green().bold(),
                        file_display,
                        wasm_display,
                        format_byte_size(final_size),
                        compile_suffix
                    );
                }
            }
            Err(e) => {
                eprintln!("{}", format!("WASM codegen error: {}", e).red());
                process::exit(1);
            }
        }
    }
}

#[cfg(feature = "wasm")]
fn wasm_opt_label(mode: super::cli::WasmOptMode) -> &'static str {
    match mode {
        super::cli::WasmOptMode::O3 => "-O3",
        super::cli::WasmOptMode::Oz => "-Oz",
    }
}

#[cfg(feature = "wasm")]
fn run_wasm_opt(wasm_file: &Path, mode: super::cli::WasmOptMode) -> Result<u64, String> {
    let input_size = std::fs::metadata(wasm_file)
        .map(|meta| meta.len())
        .map_err(|e| format!("Failed to stat {}: {}", wasm_file.display(), e))?;
    let optimized_file = wasm_file.with_extension("opt.wasm");
    let opt_flag = match mode {
        super::cli::WasmOptMode::O3 => "-O3",
        super::cli::WasmOptMode::Oz => "-Oz",
    };

    let output = std::process::Command::new("wasm-opt")
        .arg(opt_flag)
        .arg("--enable-bulk-memory")
        .arg("--enable-multivalue")
        .arg(wasm_file)
        .arg("-o")
        .arg(&optimized_file)
        .output()
        .map_err(|e| {
            format!(
                "Failed to run wasm-opt for {}: {}. Install binaryen or compile without --wasm-opt.",
                wasm_file.display(),
                e
            )
        })?;

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
    let opt_summary = format!("{} {}", wasm_opt_label(mode), size_delta);
    println!(
        "{} {} → {} ({})",
        "Optimized".green().bold(),
        wasm_file.display(),
        format_byte_size(output_size),
        opt_summary
    );

    Ok(output_size)
}

/// Find the pre-built aver-wasm-rt.wasm runtime.
/// Searches: next to aver binary, in aver-wasm-rt/target, AVER_WASM_RT env var.
#[cfg(feature = "wasm")]
fn find_wasm_runtime() -> Option<std::path::PathBuf> {
    // 1. AVER_WASM_RT environment variable
    if let Ok(path) = std::env::var("AVER_WASM_RT") {
        let p = std::path::PathBuf::from(path);
        if p.exists() {
            return Some(p);
        }
    }

    // 2. Next to the aver binary
    if let Ok(exe) = std::env::current_exe() {
        let dir = exe.parent().unwrap_or(Path::new("."));
        let candidate = dir.join("aver_wasm_rt.wasm");
        if candidate.exists() {
            return Some(candidate);
        }
    }

    // 3. In the aver-wasm-rt build directory (development)
    let candidates = [
        "aver-wasm-rt/target/wasm32-wasip1/release/aver_wasm_rt.wasm",
        "aver-wasm-rt/target/wasm32-wasip1/debug/aver_wasm_rt.wasm",
    ];
    for c in &candidates {
        let p = std::path::PathBuf::from(c);
        if p.exists() {
            return Some(p);
        }
    }

    None
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
    pub(super) adapter: Option<super::cli::WasmAdapter>,
    pub(super) wasm_opt: Option<super::cli::WasmOptMode>,
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
    let build_hint = format!("cd {} && dafny verify {}.dfy", output_dir, ctx.project_name);
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

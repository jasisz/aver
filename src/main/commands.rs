use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::process;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use colored::Colorize;

use aver::ast::{Expr, FnBody, FnDef, Pattern, Stmt, TopLevel, TypeDef, VerifyBlock, VerifyKind};
use aver::checker::{
    CheckFinding, VerifyResult, check_module_intent_with_sigs_in,
    collect_verify_coverage_warnings_in, collect_verify_law_dependency_warnings_in, expr_to_str,
    index_decisions, merge_verify_blocks, run_verify,
};
use aver::codegen;
use aver::codegen::ModuleInfo;
use aver::codegen::lean as lean_codegen;
use aver::codegen::rust as rust_codegen;
use aver::interpreter::{Interpreter, RecordingConfig, Value, aver_repr};
use aver::nan_value::Arena;
use aver::replay::{JsonValue, RecordedOutcome, value_to_json};
use aver::resolver;
use aver::source::{find_module_file, require_module_declaration};
use aver::tail_check::collect_non_tail_recursion_warnings_with_sigs;
use aver::tco;
use aver::types::checker::run_type_check_full;
use aver::types::{Type, parse_type_str};
use aver::verify_law::{
    collect_contextual_helper_law_hints, collect_missing_helper_law_hints,
    contextual_helper_law_message, missing_helper_law_message,
};
use aver::vm;

use crate::shared::{
    compile_program_for_exec, compute_memo_fns, format_type_errors, load_dep_modules, parse_file,
    print_type_errors, read_file, resolve_module_root, run_entry_function,
    run_top_level_statements,
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

fn resolve_av_inputs(path: &str) -> Result<Vec<String>, String> {
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

fn expr_path_parts(expr: &Expr) -> Option<Vec<String>> {
    match expr {
        Expr::Attr(inner, field) => {
            let mut parts = match inner.as_ref() {
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
    expr: &Expr,
    dep_targets: &[ImportTarget],
    unique_type_owner: &HashMap<String, String>,
    used_by_target: &mut HashMap<String, HashSet<String>>,
) {
    if let Some(parts) = expr_path_parts(expr) {
        mark_path_use(&parts, dep_targets, unique_type_owner, used_by_target);
    }

    match expr {
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
        Expr::List(items) | Expr::Tuple(items) => {
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
            for arg in &inner.1 {
                walk_expr_for_exposes(arg, dep_targets, unique_type_owner, used_by_target);
            }
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Constructor(_, None) | Expr::Resolved(_) => {}
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
            TopLevel::Module(_) | TopLevel::Decision(_) | TopLevel::EffectSet { .. } => {}
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
            message: format!("Unused exposes: {}", unused.join(", ")),
        });
    }

    findings
}

fn finding_location(f: &CheckFinding, entry_module: Option<&str>) -> String {
    match (&f.module, entry_module) {
        (Some(module), Some(entry)) if module == entry => f.line.to_string(),
        (Some(module), _) => format!("{}:{}", module, f.line),
        (None, _) => f.line.to_string(),
    }
}

fn display_check_path(path: &str, module_root: &str) -> String {
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
        match vm::compile_program_with_modules(&items, &mut arena, Some(&module_root)) {
            Ok(v) => v,
            Err(e) => {
                eprintln!("{}", format!("VM compile error: {}", e).red());
                process::exit(1);
            }
        };

    // Execute
    let mut machine = vm::VM::new(code, globals, arena);

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
        match run_verify_for_items_vm(items, &module_root) {
            Ok((_passed, failed, _had_blocks)) => {
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

pub(super) fn cmd_run(
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

    let (mut interp, items, module_root) =
        match compile_program_for_exec(file, module_root_override) {
            Ok(v) => v,
            Err(e) => {
                eprintln!("{}", e.red());
                process::exit(1);
            }
        };

    interp.set_cli_args(program_args);

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
        interp.configure_recording_sink(RecordingConfig {
            path: out_path.clone(),
            request_id: request_id.clone(),
            timestamp: timestamp.clone(),
            program_file: record_program_file,
            module_root: record_module_root,
            entry_fn: "main".to_string(),
            input: JsonValue::Null,
        });
        interp.start_recording();
        if let Err(e) = interp.persist_recording_snapshot(RecordedOutcome::Value(JsonValue::Null)) {
            eprintln!("{}", e.to_string().red());
            process::exit(1);
        }
        Some(out_path)
    } else {
        None
    };

    // Terminal guard — restores raw mode / cursor / colors on drop (even on panic).
    #[cfg(feature = "terminal")]
    let _terminal_guard = aver_rt::TerminalGuard::new();

    let mut runtime_failure: Option<String> = run_top_level_statements(&mut interp, &items).err();

    let mut main_result: Option<Result<Value, String>> = None;
    if runtime_failure.is_none() && interp.lookup("main").is_ok() {
        let result = run_entry_function(&mut interp, "main", vec![]);
        if let Ok(Value::Err(err)) = &result {
            runtime_failure = Some(format!("Main returned error: {}", aver_repr(err)));
        } else if let Err(e) = &result {
            runtime_failure = Some(e.clone());
        }
        main_result = Some(result);
    }

    if recording_target.is_some() {
        let output = if let Some(msg) = &runtime_failure {
            RecordedOutcome::RuntimeError(msg.clone())
        } else {
            match &main_result {
                Some(Ok(v)) => match value_to_json(v) {
                    Ok(json) => RecordedOutcome::Value(json),
                    Err(e) => RecordedOutcome::RuntimeError(e),
                },
                Some(Err(e)) => RecordedOutcome::RuntimeError(e.clone()),
                None => RecordedOutcome::Value(JsonValue::Null),
            }
        };

        if let Err(e) = interp.persist_recording_snapshot(output) {
            eprintln!("{}", e.to_string().red());
            process::exit(1);
        }
        if let Some(path) = interp.recording_sink_path() {
            println!("Recording saved: {}", path.display());
        }
    }

    if let Some(msg) = runtime_failure {
        eprintln!("{}", msg.red());
        process::exit(1);
    }

    // Optionally run verify blocks
    if run_verify_blocks {
        println!();
        let mut total_passed = 0;
        let mut total_failed = 0;

        let verify_blocks = merge_verify_blocks(&items);
        for vb in &verify_blocks {
            let result = run_verify(vb, &mut interp);
            total_passed += result.passed;
            total_failed += result.failed;
            println!();
        }

        if total_failed > 0 {
            process::exit(1);
        }
        let _ = (total_passed, total_failed);
    }
}

fn run_check_for_file(file: &str, module_root: &str, deps: bool) -> Result<bool, String> {
    let units = collect_check_units(file, module_root, deps)?;
    let entry_module = units.first().and_then(|(_, _, items)| module_name(items));
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
        if idx > 0 {
            println!();
        }
        let shown_path = display_check_path(path, module_root);
        println!("Check: {}", shown_path.cyan());
        let line_count = source.lines().count();
        let mut transformed = items.clone();
        tco::transform_program(&mut transformed);

        // --- Type errors (hard errors) ---
        let tc_result = run_type_check_full(items, Some(module_root));
        let non_tail_warnings =
            collect_non_tail_recursion_warnings_with_sigs(&transformed, &tc_result.fn_sigs);
        let has_errors = !tc_result.errors.is_empty();
        for te in &tc_result.errors {
            println!("  {}", format!("error[{}]: {}", te.line, te.message).red());
        }

        // Check line count
        if line_count > 500 {
            println!(
                "  {} File has {} lines (recommended max: 500)",
                "WARNING:".yellow(),
                line_count
            );
        } else {
            println!("  {} Size OK ({} lines)", "✓".green(), line_count);
        }

        // Check intents, descriptions, and verify coverage
        let findings =
            check_module_intent_with_sigs_in(items, Some(&tc_result.fn_sigs), Some(path));
        let coverage_warnings = collect_verify_coverage_warnings_in(items, Some(path));
        let law_dependency_warnings =
            collect_verify_law_dependency_warnings_in(items, &tc_result.fn_sigs, Some(path));
        let unused_exposes_warnings = unused_exposes_by_file
            .get(&canonical_path_key(path))
            .cloned()
            .unwrap_or_default();
        if findings.errors.is_empty()
            && findings.warnings.is_empty()
            && coverage_warnings.is_empty()
            && law_dependency_warnings.is_empty()
            && unused_exposes_warnings.is_empty()
        {
            println!("  {} All intent/desc/verify present", "✓".green());
        } else {
            for e in &findings.errors {
                let loc = finding_location(e, entry_module.as_deref());
                println!("  {}", format!("error[{}]: {}", loc, e.message).red());
            }
            for w in &findings.warnings {
                let loc = finding_location(w, entry_module.as_deref());
                println!("  {}", format!("warning[{}]: {}", loc, w.message).yellow());
            }
            for w in &coverage_warnings {
                let loc = finding_location(w, entry_module.as_deref());
                println!("  {}", format!("warning[{}]: {}", loc, w.message).yellow());
            }
            for w in &law_dependency_warnings {
                let loc = finding_location(w, entry_module.as_deref());
                println!("  {}", format!("warning[{}]: {}", loc, w.message).yellow());
            }
            for w in &unused_exposes_warnings {
                let loc = finding_location(w, entry_module.as_deref());
                println!("  {}", format!("warning[{}]: {}", loc, w.message).yellow());
            }
            for warning in &non_tail_warnings {
                println!(
                    "  {}",
                    format!("warning[{}:1]: {}", warning.line, warning.message).yellow()
                );
            }
        }

        if findings.errors.is_empty()
            && findings.warnings.is_empty()
            && coverage_warnings.is_empty()
            && law_dependency_warnings.is_empty()
            && unused_exposes_warnings.is_empty()
            && !non_tail_warnings.is_empty()
        {
            for warning in &non_tail_warnings {
                println!(
                    "  {}",
                    format!("warning[{}:1]: {}", warning.line, warning.message).yellow()
                );
            }
        }

        // Count decisions
        let decisions = index_decisions(items);
        if !decisions.is_empty() {
            println!(
                "  {} Found {} decision block(s)",
                "✓".green(),
                decisions.len()
            );
        }

        let has_contract_errors = !findings.errors.is_empty();
        if has_errors || has_contract_errors {
            has_any_error = true;
        } else {
            println!("  {} Type check passed", "✓".green());
        }
    }

    Ok(has_any_error)
}

pub(super) fn cmd_check(path: &str, module_root_override: Option<&str>, deps: bool) {
    let module_root = resolve_module_root(module_root_override);
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
        if batch && idx > 0 {
            println!();
        }

        if batch {
            println!("Input: {}", display_check_path(file, &module_root).cyan());
        }

        match run_check_for_file(file, &module_root, deps) {
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

    if batch {
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

fn make_verify_vm_helper(name: String, line: usize, expr: Expr, wrap_result: bool) -> TopLevel {
    let body_expr = if wrap_result {
        Expr::Constructor("Result.Ok".to_string(), Some(Box::new(expr)))
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
    let block = &plan.block;
    let mut passed = 0;
    let mut failed = 0;
    let mut skipped = 0;
    let mut failures = Vec::new();
    let is_law = matches!(block.kind, VerifyKind::Law(_));

    match &block.kind {
        VerifyKind::Cases => println!("Verify: {}", block.fn_name.cyan()),
        VerifyKind::Law(law) => {
            println!("Verify: {} law {}", block.fn_name.cyan(), law.name.cyan());
            for given in &law.givens {
                let domain = match &given.domain {
                    aver::ast::VerifyGivenDomain::IntRange { start, end } => {
                        format!("{start}..{end}")
                    }
                    aver::ast::VerifyGivenDomain::Explicit(values) => {
                        let parts: Vec<String> = values.iter().map(expr_to_str).collect();
                        format!("[{}]", parts.join(", "))
                    }
                };
                println!(
                    "  {} {}: {} = {}",
                    "given".dimmed(),
                    given.name,
                    given.type_name,
                    domain
                );
            }
            if let Some(when_expr) = &law.when {
                println!("  {} {}", "when".dimmed(), expr_to_str(when_expr));
            }
            println!(
                "  {} {} == {}",
                "law".dimmed(),
                expr_to_str(&law.lhs),
                expr_to_str(&law.rhs)
            );
            println!("  {} {}", "cases".dimmed(), block.cases.len());
        }
    }

    for (idx, ((left_expr, right_expr), case_fns)) in
        block.cases.iter().zip(&plan.cases).enumerate()
    {
        let case_str = format!("{} == {}", expr_to_str(left_expr), expr_to_str(right_expr));
        let case_label = if is_law {
            format!("case {}/{}", idx + 1, block.cases.len())
        } else {
            case_str.clone()
        };
        let failure_case = if is_law {
            format!("{} [{}]", case_label, case_str)
        } else {
            case_str.clone()
        };

        if let Some(guard_name) = &case_fns.guard {
            match vm_call_guard_helper(machine, guard_name) {
                Ok(Value::Bool(true)) => {}
                Ok(Value::Bool(false)) => {
                    skipped += 1;
                    println!("  {} {} (when false)", "·".dimmed(), case_label.dimmed());
                    continue;
                }
                Ok(Value::Err(err_val)) => {
                    failed += 1;
                    println!("  {} {}", "✗".red(), case_label);
                    println!("      when ? hit Result.Err({})", aver_repr(&err_val));
                    failures.push((
                        failure_case,
                        String::new(),
                        format!("when ? hit Result.Err({})", aver_repr(&err_val)),
                    ));
                    continue;
                }
                Ok(other) => {
                    failed += 1;
                    println!("  {} {}", "✗".red(), case_label);
                    println!("      when did not evaluate to Bool: {}", aver_repr(&other));
                    failures.push((
                        failure_case,
                        "Bool".to_string(),
                        format!("when produced {}", aver_repr(&other)),
                    ));
                    continue;
                }
                Err(e) => {
                    failed += 1;
                    println!("  {} {}", "✗".red(), case_label);
                    println!("      when error: {}", e);
                    failures.push((failure_case, String::new(), format!("WHEN ERROR: {}", e)));
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
                    if !is_law {
                        println!("  {} {}", "✓".green(), case_label);
                    }
                } else {
                    failed += 1;
                    println!("  {} {}", "✗".red(), case_label);
                    if is_law {
                        println!("      expanded: {}", case_str);
                    }
                    let expected = aver_repr(&right_val);
                    let actual = aver_repr(&left_val);
                    println!("      expected: {}", expected);
                    println!("      got:      {}", actual);
                    failures.push((failure_case, expected, actual));
                }
            }
            (Ok(VmVerifyEval::ErrProp(err_val)), _) | (_, Ok(VmVerifyEval::ErrProp(err_val))) => {
                failed += 1;
                println!("  {} {}", "✗".red(), case_label);
                if is_law {
                    println!("      expanded: {}", case_str);
                }
                println!("      ? hit Result.Err({})", aver_repr(&err_val));
                failures.push((
                    failure_case,
                    String::new(),
                    format!("? hit Result.Err({})", aver_repr(&err_val)),
                ));
            }
            (Err(e), _) | (_, Err(e)) => {
                failed += 1;
                println!("  {} {}", "✗".red(), case_label);
                if is_law {
                    println!("      expanded: {}", case_str);
                }
                println!("      error: {}", e);
                failures.push((failure_case, String::new(), format!("ERROR: {}", e)));
            }
        }
    }

    let total = passed + failed;
    if is_law && failed == 0 {
        if skipped == 0 {
            println!(
                "  {} all {} generated case(s) passed",
                "✓".green(),
                block.cases.len()
            );
        } else {
            println!(
                "  {} all {} active generated case(s) passed ({} skipped by when)",
                "✓".green(),
                passed,
                skipped
            );
        }
    }
    if failed == 0 && skipped == 0 {
        println!("  {}", format!("{}/{} passed", passed, total).green());
    } else if failed == 0 {
        println!(
            "  {}",
            format!("{}/{} passed, {} skipped", passed, total + skipped, skipped).green()
        );
    } else if skipped == 0 {
        println!("  {}", format!("{}/{} passed", passed, total).red());
    } else {
        println!(
            "  {}",
            format!("{}/{} passed, {} skipped", passed, total + skipped, skipped).red()
        );
    }

    VerifyResult {
        fn_name: block.fn_name.clone(),
        passed,
        failed,
        skipped,
        failures,
    }
}

fn run_verify_for_items(
    mut items: Vec<TopLevel>,
    module_root: &str,
) -> Result<(usize, usize, bool), String> {
    // TCO transform — rewrite tail-position calls in recursive SCCs
    tco::transform_program(&mut items);

    // Static type check — verify should use the same soundness gate as run/check
    let tc_result = run_type_check_full(&items, Some(module_root));
    if !tc_result.errors.is_empty() {
        return Err(format_type_errors(&tc_result.errors));
    }

    // Compile-time variable resolution
    resolver::resolve_program(&mut items);

    // Auto-memoization
    let memo_fns = compute_memo_fns(&items, &tc_result);

    let mut interp = Interpreter::new();
    interp.enable_memo(memo_fns);

    // Load aver.toml runtime policy if present
    match aver::config::ProjectConfig::load_from_dir(std::path::Path::new(&module_root)) {
        Ok(Some(config)) => interp.set_runtime_policy(config),
        Ok(None) => {}
        Err(e) => return Err(format!("aver.toml: {}", e)),
    }

    load_dep_modules(&mut interp, &items, module_root)?;

    // Register type definitions (constructors)
    for item in &items {
        if let TopLevel::TypeDef(td) = item {
            interp.register_type_def(td);
        }
    }

    // Register all functions
    for item in &items {
        if let TopLevel::FnDef(fd) = item
            && let Err(e) = interp.exec_fn_def(fd)
        {
            return Err(e.to_string());
        }
    }

    let verify_blocks = merge_verify_blocks(&items);

    if verify_blocks.is_empty() {
        return Ok((0, 0, false));
    }

    let mut total_passed = 0;
    let mut total_failed = 0;

    for vb in &verify_blocks {
        let result = run_verify(vb, &mut interp);
        total_passed += result.passed;
        total_failed += result.failed;
        println!();
    }

    Ok((total_passed, total_failed, true))
}

fn run_verify_for_items_vm(
    mut items: Vec<TopLevel>,
    module_root: &str,
) -> Result<(usize, usize, bool), String> {
    tco::transform_program(&mut items);

    let tc_result = run_type_check_full(&items, Some(module_root));
    if !tc_result.errors.is_empty() {
        return Err(format_type_errors(&tc_result.errors));
    }

    let verify_blocks = merge_verify_blocks(&items);
    if verify_blocks.is_empty() {
        return Ok((0, 0, false));
    }

    let plans = build_verify_vm_plans(&mut items, &verify_blocks);
    resolver::resolve_program(&mut items);

    let mut arena = Arena::new();
    vm::register_service_types(&mut arena);
    let (code, globals) = vm::compile_program_with_modules(&items, &mut arena, Some(module_root))
        .map_err(|e| format!("VM compile error: {}", e))?;
    let mut machine = vm::VM::new(code, globals, arena);

    let mut total_passed = 0;
    let mut total_failed = 0;

    for plan in &plans {
        let result = run_verify_vm(plan, &mut machine);
        total_passed += result.passed;
        total_failed += result.failed;
        println!();
    }

    Ok((total_passed, total_failed, true))
}

fn run_verify_for_file(
    file: &str,
    module_root: &str,
    deps: bool,
    show_file_headers: bool,
    vm_mode: bool,
) -> Result<(usize, usize, bool), String> {
    let units = collect_check_units(file, module_root, deps)?;
    let mut total_passed = 0;
    let mut total_failed = 0;
    let mut saw_verify_blocks = false;

    for (idx, (path, _source, items)) in units.into_iter().enumerate() {
        if show_file_headers {
            if idx > 0 || deps {
                println!();
            }
            println!(
                "{}",
                format!("Verify file: {}", display_check_path(&path, module_root)).cyan()
            );
        }

        let (passed, failed, had_blocks) = if vm_mode {
            run_verify_for_items_vm(items, module_root)?
        } else {
            run_verify_for_items(items, module_root)?
        };

        if had_blocks {
            saw_verify_blocks = true;
            total_passed += passed;
            total_failed += failed;
        }
    }

    Ok((total_passed, total_failed, saw_verify_blocks))
}

pub(super) fn cmd_verify(
    path: &str,
    module_root_override: Option<&str>,
    deps: bool,
    vm_mode: bool,
) {
    let module_root = resolve_module_root(module_root_override);
    let inputs = match resolve_av_inputs(path) {
        Ok(inputs) => inputs,
        Err(e) => {
            eprintln!("{}", e.red());
            process::exit(1);
        }
    };

    let batch = Path::new(path).is_dir();
    let mut total_passed = 0;
    let mut total_failed = 0;
    let mut saw_verify_blocks = false;
    let mut failed_files = Vec::new();

    for file in &inputs {
        let (passed, failed, had_blocks) =
            match run_verify_for_file(file, &module_root, deps, batch || deps, vm_mode) {
                Ok(counts) => counts,
                Err(e) => {
                    eprintln!("{}", e.red());
                    failed_files.push(file.clone());
                    continue;
                }
            };

        if had_blocks {
            saw_verify_blocks = true;
            total_passed += passed;
            total_failed += failed;
        }
        if failed > 0 {
            failed_files.push(file.clone());
        }
    }

    if !failed_files.is_empty() && batch {
        println!();
        println!(
            "{}",
            format!(
                "Verify run completed with {} failed file(s).",
                failed_files.len()
            )
            .red()
        );
        for file in &failed_files {
            println!("  {}", display_check_path(file, &module_root));
        }
    }

    let total = total_passed + total_failed;
    if !saw_verify_blocks {
        let scope = if deps {
            format!("{} or its transitive dependencies", path)
        } else {
            path.to_string()
        };
        println!(
            "{}",
            format!("No verify blocks found in {}.", scope).yellow()
        );
    } else if total_failed == 0 {
        println!(
            "{}",
            format!("Total: {}/{} passed", total_passed, total).green()
        );
    } else {
        println!(
            "{}",
            format!("Total: {}/{} passed", total_passed, total).red()
        );
    }

    if !failed_files.is_empty() || total_failed > 0 {
        process::exit(1);
    }
}

fn build_codegen_context(
    file: &str,
    project_name: Option<&str>,
    module_root_override: Option<&str>,
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

    // Static type check
    let tc_result = run_type_check_full(&items, Some(&module_root));
    if !tc_result.errors.is_empty() {
        print_type_errors(&tc_result.errors);
        process::exit(1);
    }

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

    // Load aver.toml runtime policy for codegen
    let policy =
        match aver::config::ProjectConfig::load_from_dir(std::path::Path::new(&module_root)) {
            Ok(config) => config,
            Err(e) => {
                eprintln!("{}", format!("aver.toml: {}", e).red());
                process::exit(1);
            }
        };

    // Build codegen context
    let mut ctx = codegen::build_context(items, &tc_result, memo_fns, name, modules);
    ctx.policy = policy;
    (ctx, module_root)
}

fn write_codegen_output(
    file: &str,
    output_dir: &str,
    target_label: &str,
    build_hint: &str,
    output: &codegen::ProjectOutput,
) {
    // Write output files
    let out_path = Path::new(output_dir);
    for (rel_path, content) in &output.files {
        let full_path = out_path.join(rel_path);
        if let Some(parent) = full_path.parent()
            && let Err(e) = fs::create_dir_all(parent)
        {
            eprintln!(
                "{}",
                format!("Cannot create dir '{}': {}", parent.display(), e).red()
            );
            process::exit(1);
        }
        if let Err(e) = fs::write(&full_path, content) {
            eprintln!(
                "{}",
                format!("Cannot write '{}': {}", full_path.display(), e).red()
            );
            process::exit(1);
        }
    }

    println!(
        "{}",
        format!("Compiled {} → {}/ [{}]", file, output_dir, target_label).green()
    );
    println!("  {}", build_hint.cyan());
}

pub(super) fn cmd_compile(
    file: &str,
    output_dir: &str,
    project_name: Option<&str>,
    module_root_override: Option<&str>,
) {
    let (ctx, _module_root) = build_codegen_context(file, project_name, module_root_override);
    let output = rust_codegen::transpile(&ctx);
    let build_hint = format!("cd {} && cargo build && cargo run", output_dir);
    write_codegen_output(file, output_dir, "Rust", &build_hint, &output);
}

pub(super) fn cmd_proof(
    file: &str,
    output_dir: &str,
    project_name: Option<&str>,
    module_root_override: Option<&str>,
    backend: &super::cli::ProofBackend,
    verify_mode: &super::cli::ProofVerifyMode,
) {
    let (ctx, _module_root) = build_codegen_context(file, project_name, module_root_override);

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
    use super::resolve_av_inputs;
    use std::fs;
    use std::path::PathBuf;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn temp_case_dir(tag: &str) -> PathBuf {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0);
        std::env::temp_dir().join(format!("aver_commands_{tag}_{nanos}"))
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
}

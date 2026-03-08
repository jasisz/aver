use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::process;
use std::time::{SystemTime, UNIX_EPOCH};

use colored::Colorize;

use aver::ast::{Expr, Pattern, Stmt, TopLevel, TypeDef, VerifyKind};
use aver::checker::{
    CheckFinding, check_module_intent_with_sigs_in, collect_verify_coverage_warnings_in,
    index_decisions, merge_verify_blocks, run_verify,
};
use aver::codegen;
use aver::codegen::ModuleInfo;
use aver::codegen::lean as lean_codegen;
use aver::codegen::rust as rust_codegen;
use aver::interpreter::{Interpreter, RecordingConfig, Value, aver_repr};
use aver::replay::{JsonValue, RecordedOutcome, value_to_json};
use aver::resolver;
use aver::source::{find_module_file, require_module_declaration};
use aver::tco;
use aver::types::checker::run_type_check_full;
use aver::types::{Type, parse_type_str};

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
        Type::Option(inner) | Type::List(inner) => {
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

        if module.exposes.is_empty() {
            continue;
        }

        let exposed_name_set = module.exposes.iter().cloned().collect::<HashSet<_>>();
        let exposed_type_names = items
            .iter()
            .filter_map(|item| match item {
                TopLevel::TypeDef(TypeDef::Sum { name, .. })
                | TopLevel::TypeDef(TypeDef::Product { name, .. })
                    if exposed_name_set.contains(name) =>
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

pub(super) fn cmd_check(file: &str, module_root_override: Option<&str>, deps: bool) {
    let module_root = resolve_module_root(module_root_override);
    let units = match collect_check_units(file, &module_root, deps) {
        Ok(units) => units,
        Err(e) => {
            eprintln!("{}", e.red());
            process::exit(1);
        }
    };

    let entry_module = units.first().and_then(|(_, _, items)| module_name(items));
    let mut unused_exposes_by_file: HashMap<String, Vec<CheckFinding>> = HashMap::new();
    if deps {
        for finding in collect_unused_exposes_findings(&units, file, &module_root) {
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
        let shown_path = display_check_path(path, &module_root);
        println!("Check: {}", shown_path.cyan());
        let line_count = source.lines().count();

        // --- Type errors (hard errors) ---
        let tc_result = run_type_check_full(items, Some(&module_root));
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
        let unused_exposes_warnings = unused_exposes_by_file
            .get(&canonical_path_key(path))
            .cloned()
            .unwrap_or_default();
        if findings.errors.is_empty()
            && findings.warnings.is_empty()
            && coverage_warnings.is_empty()
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
            for w in &unused_exposes_warnings {
                let loc = finding_location(w, entry_module.as_deref());
                println!("  {}", format!("warning[{}]: {}", loc, w.message).yellow());
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

    if has_any_error {
        process::exit(1);
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

    // Register effect sets first (needed before FnDef expansion)
    for item in &items {
        if let TopLevel::EffectSet { name, effects, .. } = item {
            interp.register_effect_set(name.clone(), effects.clone());
        }
    }

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

pub(super) fn cmd_verify(file: &str, module_root_override: Option<&str>, deps: bool) {
    let module_root = resolve_module_root(module_root_override);
    let units = match collect_check_units(file, &module_root, deps) {
        Ok(units) => units,
        Err(e) => {
            eprintln!("{}", e.red());
            process::exit(1);
        }
    };

    let mut total_passed = 0;
    let mut total_failed = 0;
    let mut saw_verify_blocks = false;

    for (idx, (path, _source, items)) in units.into_iter().enumerate() {
        if deps {
            if idx > 0 {
                println!();
            }
            println!(
                "{}",
                format!("Verify file: {}", display_check_path(&path, &module_root)).cyan()
            );
        }

        let (passed, failed, had_blocks) = match run_verify_for_items(items, &module_root) {
            Ok(counts) => counts,
            Err(e) => {
                eprintln!("{}", e.red());
                process::exit(1);
            }
        };

        if had_blocks {
            saw_verify_blocks = true;
            total_passed += passed;
            total_failed += failed;
        }
    }

    if !saw_verify_blocks {
        let scope = if deps {
            format!("{} or its transitive dependencies", file)
        } else {
            file.to_string()
        };
        println!(
            "{}",
            format!("No verify blocks found in {}.", scope).yellow()
        );
        return;
    }

    let total = total_passed + total_failed;
    if total_failed == 0 {
        println!(
            "{}",
            format!("Total: {}/{} passed", total_passed, total).green()
        );
    } else {
        println!(
            "{}",
            format!("Total: {}/{} passed", total_passed, total).red()
        );
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
    verify_mode: &super::cli::ProofVerifyMode,
) {
    let (ctx, _module_root) = build_codegen_context(file, project_name, module_root_override);

    let proof_issues = lean_codegen::proof_mode_issues(&ctx);
    if !proof_issues.is_empty() {
        eprintln!(
            "{}",
            "Proof-mode warnings (unsupported recursive shapes will fall back to partial defs):"
                .yellow()
        );
        for issue in proof_issues {
            eprintln!("  - {}", issue);
        }
    }

    let verify_mode = match verify_mode {
        super::cli::ProofVerifyMode::Auto => lean_codegen::VerifyEmitMode::NativeDecide,
        super::cli::ProofVerifyMode::Sorry => lean_codegen::VerifyEmitMode::Sorry,
        super::cli::ProofVerifyMode::TheoremSkeleton => {
            lean_codegen::VerifyEmitMode::TheoremSkeleton
        }
    };

    let output = lean_codegen::transpile_for_proof_mode(&ctx, verify_mode);
    let build_hint = format!("cd {} && lake build", output_dir);
    write_codegen_output(file, output_dir, "Lean 4", &build_hint, &output);
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

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use crate::ast::TopLevel;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::visibility;

pub fn parse_source(source: &str) -> Result<Vec<TopLevel>, String> {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize().map_err(|e| e.to_string())?;
    let mut parser = Parser::new(tokens);
    parser.parse().map_err(|e| e.to_string())
}

/// Enforce module contract for file-based programs:
/// exactly one `module` declaration and it must be the first top-level item.
pub fn require_module_declaration(items: &[TopLevel], file: &str) -> Result<(), String> {
    let module_positions: Vec<usize> = items
        .iter()
        .enumerate()
        .filter_map(|(idx, item)| matches!(item, TopLevel::Module(_)).then_some(idx))
        .collect();

    if module_positions.is_empty() {
        return Err(format!(
            "File '{}' must declare `module <Name>` as the first top-level item",
            file
        ));
    }

    if module_positions[0] != 0 {
        return Err(format!(
            "File '{}' must place `module <Name>` as the first top-level item",
            file
        ));
    }

    if module_positions.len() > 1 {
        return Err(format!(
            "File '{}' must contain exactly one module declaration (found {})",
            file,
            module_positions.len()
        ));
    }

    Ok(())
}

pub fn find_module_file(name: &str, module_root: &str) -> Option<PathBuf> {
    let root = Path::new(module_root);
    let parts: Vec<&str> = name.split('.').filter(|s| !s.is_empty()).collect();
    if parts.is_empty() {
        return None;
    }

    let lower_rel = format!(
        "{}.av",
        parts
            .iter()
            .map(|p| p.to_lowercase())
            .collect::<Vec<_>>()
            .join("/")
    );
    let exact_rel = format!("{}.av", parts.join("/"));

    let lower = root.join(&lower_rel);
    if lower.exists() {
        return Some(lower);
    }

    let exact = root.join(&exact_rel);
    if exact.exists() {
        return Some(exact);
    }

    None
}

pub fn canonicalize_path(path: &Path) -> PathBuf {
    std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf())
}

// ---------------------------------------------------------------------------
// Shared module loader — find, read, parse, validate, recurse
// ---------------------------------------------------------------------------

/// A parsed module ready for backend consumption.
#[derive(Clone, Debug)]
pub struct LoadedModule {
    pub dep_name: String,
    pub items: Vec<TopLevel>,
    pub path: PathBuf,
}

/// Sibling of [`load_module_tree`] that resolves dependency modules
/// from an in-memory file map instead of the filesystem. Used by the
/// playground so a browser-side virtual fs can compile a multi-file
/// project without disk IO.
///
/// The map's keys must be file paths matching what
/// [`find_module_file`] would produce (e.g. `"types.av"`,
/// `"rogue/combat.av"`). Both lowercase and exact casings are tried
/// for each requested dep, mirroring the on-disk search order.
pub fn load_module_tree_from_map(
    root_deps: &[String],
    files: &HashMap<String, String>,
) -> Result<Vec<LoadedModule>, String> {
    let mut result = Vec::new();
    let mut loaded: HashSet<String> = HashSet::new();
    let mut loading: Vec<String> = Vec::new();
    for dep in root_deps {
        load_recursive_from_map(dep, files, &mut loaded, &mut loading, &mut result)?;
    }
    Ok(result)
}

fn load_recursive_from_map(
    dep_name: &str,
    files: &HashMap<String, String>,
    loaded: &mut HashSet<String>,
    loading: &mut Vec<String>,
    result: &mut Vec<LoadedModule>,
) -> Result<(), String> {
    let key = find_file_key_in_map(dep_name, files)
        .ok_or_else(|| format!("Module '{}' not found in virtual fs", dep_name))?;

    if loaded.contains(&key) {
        return Ok(());
    }
    if loading.contains(&key) {
        let chain = loading
            .iter()
            .cloned()
            .chain(std::iter::once(key.clone()))
            .collect::<Vec<_>>()
            .join(" -> ");
        return Err(format!("Circular import: {}", chain));
    }
    loading.push(key.clone());

    let source = files.get(&key).unwrap();
    let items =
        parse_source(source).map_err(|e| format!("Parse error in '{}': {}", dep_name, e))?;
    require_module_declaration(&items, &key)?;

    if let Some(module) = visibility::module_decl(&items) {
        let expected = dep_name.rsplit('.').next().unwrap_or(dep_name);
        if module.name != expected {
            return Err(format!(
                "Module name mismatch: expected '{}' (from dep '{}'), found '{}' in '{}'",
                expected, dep_name, module.name, key
            ));
        }
        for sub_dep in &module.depends {
            load_recursive_from_map(sub_dep, files, loaded, loading, result)?;
        }
    }

    loading.pop();
    loaded.insert(key.clone());
    result.push(LoadedModule {
        dep_name: dep_name.to_string(),
        items,
        path: PathBuf::from(&key),
    });
    Ok(())
}

fn find_file_key_in_map(dep_name: &str, files: &HashMap<String, String>) -> Option<String> {
    let parts: Vec<&str> = dep_name.split('.').filter(|s| !s.is_empty()).collect();
    if parts.is_empty() {
        return None;
    }
    let lower_rel = format!(
        "{}.av",
        parts
            .iter()
            .map(|p| p.to_lowercase())
            .collect::<Vec<_>>()
            .join("/")
    );
    let exact_rel = format!("{}.av", parts.join("/"));
    let last = parts.last().copied().unwrap_or(dep_name);
    let last_lower = format!("{}.av", last.to_lowercase());
    let last_exact = format!("{}.av", last);

    for candidate in [&lower_rel, &exact_rel, &last_lower, &last_exact] {
        if files.contains_key(candidate) {
            return Some(candidate.clone());
        }
    }
    // Fallback: case-insensitive scan — browsers let users name files
    // however they like, and dep names are canonical-cased anyway.
    let wanted = last.to_lowercase();
    files
        .keys()
        .find(|k| {
            Path::new(k)
                .file_stem()
                .and_then(|s| s.to_str())
                .is_some_and(|stem| stem.eq_ignore_ascii_case(&wanted))
        })
        .cloned()
}

/// Load a dependency tree starting from `root_deps`.
/// Returns modules in dependency order (leaves first).
/// Validates module declarations and detects circular imports.
pub fn load_module_tree(
    root_deps: &[String],
    module_root: &str,
) -> Result<Vec<LoadedModule>, String> {
    let mut result = Vec::new();
    let mut loaded = HashSet::new();
    let mut loading = Vec::new();
    for dep in root_deps {
        load_recursive(dep, module_root, &mut loaded, &mut loading, &mut result)?;
    }
    Ok(result)
}

fn load_recursive(
    dep_name: &str,
    module_root: &str,
    loaded: &mut HashSet<String>,
    loading: &mut Vec<String>,
    result: &mut Vec<LoadedModule>,
) -> Result<(), String> {
    let path = find_module_file(dep_name, module_root)
        .ok_or_else(|| format!("Module '{}' not found in '{}'", dep_name, module_root))?;
    let canon = canonicalize_path(&path).to_string_lossy().to_string();

    if loaded.contains(&canon) {
        return Ok(());
    }
    if loading.contains(&canon) {
        let chain: Vec<String> = loading
            .iter()
            .map(|k| {
                Path::new(k)
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .unwrap_or(k)
                    .to_string()
            })
            .chain(std::iter::once(
                Path::new(&canon)
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .unwrap_or(&canon)
                    .to_string(),
            ))
            .collect();
        return Err(format!("Circular import: {}", chain.join(" -> ")));
    }
    loading.push(canon.clone());

    let source = std::fs::read_to_string(&path)
        .map_err(|e| format!("Cannot read '{}': {}", path.display(), e))?;
    let items =
        parse_source(&source).map_err(|e| format!("Parse error in '{}': {}", dep_name, e))?;

    require_module_declaration(&items, &path.to_string_lossy())?;

    if let Some(module) = visibility::module_decl(&items) {
        let expected = dep_name.rsplit('.').next().unwrap_or(dep_name);
        if module.name != expected {
            return Err(format!(
                "Module name mismatch: expected '{}' (from '{}'), found '{}' in '{}'",
                expected,
                dep_name,
                module.name,
                path.display()
            ));
        }
        for sub_dep in &module.depends {
            load_recursive(sub_dep, module_root, loaded, loading, result)?;
        }
    }

    loading.pop();
    loaded.insert(canon);
    result.push(LoadedModule {
        dep_name: dep_name.to_string(),
        items,
        path,
    });
    Ok(())
}

/// Load every dep module declared by `items`'s `Module.depends`, plus
/// every transitive dep, into `codegen::ModuleInfo` records ready to
/// hand to `PipelineConfig.dep_modules`.
///
/// Each dep goes through the same canonical pipeline as the entry —
/// `pipeline::run` with `TypecheckMode::Full { base_dir: module_root }`,
/// `run_interp_lower: false`, `run_buffer_build: false`, and the
/// neutral alloc policy. Type errors in any dep surface as `Err`.
///
/// Callers that need the legacy `commands.rs` shape (run_interp_lower /
/// run_buffer_build / self_host_mode flags) still own their local copy;
/// every other call site — `vm_verify`, `wasm_gc_verify`, `bench/runner`,
/// the research test — should go through here so the dep-loading
/// contract stays in one place.
pub fn load_compile_deps(
    items: &[TopLevel],
    module_root: &str,
) -> Result<Vec<crate::codegen::ModuleInfo>, String> {
    let module = items.iter().find_map(|i| match i {
        TopLevel::Module(m) => Some(m),
        _ => None,
    });
    let Some(module) = module else {
        return Ok(vec![]);
    };
    let mut result = Vec::new();
    let mut loaded = HashSet::new();
    for dep_name in &module.depends {
        load_module_recursive_for_compile(dep_name, module_root, &mut result, &mut loaded)?;
    }
    Ok(result)
}

fn load_module_recursive_for_compile(
    name: &str,
    module_root: &str,
    result: &mut Vec<crate::codegen::ModuleInfo>,
    loaded: &mut HashSet<String>,
) -> Result<(), String> {
    if !loaded.insert(name.to_string()) {
        return Ok(());
    }
    let path = find_module_file(name, module_root).ok_or_else(|| {
        format!(
            "Cannot find module '{}' in module root '{}'",
            name, module_root
        )
    })?;
    let source =
        std::fs::read_to_string(&path).map_err(|e| format!("Read '{}': {}", path.display(), e))?;
    let mut items =
        parse_source(&source).map_err(|e| format!("Parse '{}': {}", path.display(), e))?;
    require_module_declaration(&items, path.to_str().unwrap_or(name))?;

    let neutral_policy = crate::ir::NeutralAllocPolicy;
    let pipeline_result = crate::ir::pipeline::run(
        &mut items,
        crate::ir::PipelineConfig {
            typecheck: Some(crate::ir::TypecheckMode::Full {
                base_dir: Some(module_root),
            }),
            run_interp_lower: false,
            run_buffer_build: false,
            alloc_policy: Some(&neutral_policy),
            ..Default::default()
        },
    );
    if let Some(tc) = pipeline_result.typecheck.as_ref()
        && !tc.errors.is_empty()
    {
        return Err(format!(
            "Type errors in dependency module '{}':\n{}",
            name,
            tc.errors
                .iter()
                .map(|e| format!("  {}:{}: {}", e.line, e.col, e.message))
                .collect::<Vec<_>>()
                .join("\n")
        ));
    }

    let transitive: Vec<String> = items
        .iter()
        .find_map(|i| match i {
            TopLevel::Module(m) => Some(m.depends.clone()),
            _ => None,
        })
        .unwrap_or_default();
    for dep in &transitive {
        load_module_recursive_for_compile(dep, module_root, result, loaded)?;
    }

    let depends = transitive;
    let type_defs: Vec<_> = items
        .iter()
        .filter_map(|i| match i {
            TopLevel::TypeDef(td) => Some(td.clone()),
            _ => None,
        })
        .collect();
    let fn_defs: Vec<_> = items
        .iter()
        .filter_map(|i| match i {
            TopLevel::FnDef(fd) if fd.name != "main" => Some(fd.clone()),
            _ => None,
        })
        .collect();
    result.push(crate::codegen::ModuleInfo {
        prefix: name.to_string(),
        depends,
        type_defs,
        fn_defs,
        analysis: pipeline_result.analysis,
    });
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{parse_source, require_module_declaration};

    #[test]
    fn require_module_accepts_single_first_module() {
        let src = "module Demo\n    intent = \"ok\"\nfn x() -> Int\n    1\n";
        let items = parse_source(src).expect("parse");
        require_module_declaration(&items, "demo.av").expect("module declaration should pass");
    }

    #[test]
    fn require_module_rejects_missing_module() {
        let src = "fn x() -> Int\n    1\n";
        let items = parse_source(src).expect("parse");
        let err = require_module_declaration(&items, "demo.av").expect_err("expected error");
        assert!(err.contains("must declare `module <Name>`"));
    }

    #[test]
    fn require_module_rejects_module_not_first() {
        let src = "fn x() -> Int\n    1\nmodule Demo\n";
        let items = parse_source(src).expect("parse");
        let err = require_module_declaration(&items, "demo.av").expect_err("expected error");
        assert!(err.contains("must place `module <Name>` as the first"));
    }

    #[test]
    fn require_module_rejects_multiple_modules() {
        let src = "module A\nmodule B\n";
        let items = parse_source(src).expect("parse");
        let err = require_module_declaration(&items, "demo.av").expect_err("expected error");
        assert!(err.contains("exactly one module declaration"));
    }

    #[test]
    fn parse_rejects_record_positional_pattern() {
        let src = "module Demo\nrecord User\n    name: String\nfn f(u: User) -> String\n    match u\n        User(name) -> name\n";
        let err = parse_source(src).expect_err("record positional patterns should be rejected");
        assert!(err.contains("bind the whole value with a lower-case name"));
    }

    #[test]
    fn parse_rejects_unqualified_constructor_pattern() {
        let src = "module Demo\ntype Shape\n    Circle(Int)\nfn f(s: Shape) -> Int\n    match s\n        Circle(r) -> r\n";
        let err =
            parse_source(src).expect_err("unqualified constructor patterns should be rejected");
        assert!(err.contains("Constructor patterns must be qualified"));
    }
}

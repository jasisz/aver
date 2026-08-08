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

/// Source and stable display path for a resolved Aver module.
///
/// Project modules come from `module_root`; standard modules are ordinary Aver
/// source embedded in the compiler binary.
#[derive(Clone, Debug)]
pub struct ModuleSource {
    pub path: PathBuf,
    pub source: String,
}

/// Resolve an Aver standard module without consulting the filesystem.
///
/// This is public for compiler-adjacent tools such as `aver-lsp`, which keep a
/// filesystem cache for project modules but can consume embedded source
/// directly.
pub fn resolve_standard_module_source(name: &str) -> Option<ModuleSource> {
    crate::stdlib::find(name).map(|module| ModuleSource {
        path: PathBuf::from(module.virtual_path),
        source: module.source.to_string(),
    })
}

/// Project file that [`find_module_file`] would resolve for `name`, present
/// even though the embedded standard library reserves the name. `Some` means
/// module resolution silently ignores the on-disk file.
pub fn stdlib_shadowed_project_file(name: &str, module_root: &str) -> Option<PathBuf> {
    crate::stdlib::find(name)?;
    find_module_file(name, module_root)
}

/// Shared wording for the stdlib-shadowing warning, used by both the
/// load-time stderr warning and the `aver check` finding so the two
/// channels never drift apart.
pub fn stdlib_shadow_message(name: &str, shadowed_path: &str) -> String {
    format!(
        "module '{}' is reserved by the Aver standard library; project file \
         '{}' is NOT loaded — rename the module and its `depends [...]` \
         entries to use the project file",
        name, shadowed_path
    )
}

/// Emit the stdlib-shadowing warning once per process per module name.
/// Resolution runs several times per command (typecheck tree walk, dep
/// compile walk, check units), and repeating the identical warning would
/// drown the signal.
///
/// NOT suppressible, unlike the `stdlib-shadow` finding `aver check`
/// reports — that one goes through the usual `[[check.suppress]]` filter,
/// this one does not. Deliberate asymmetry: the loader runs on every
/// command, has no `aver.toml` in hand at this depth, and what it reports
/// is that the program being built is not the program on disk. See the
/// `stdlib-shadow` entry in `docs/diagnostics-slugs.md`.
fn warn_stdlib_shadow_once(name: &str, shadowed_path: &Path) {
    use std::sync::{Mutex, OnceLock};
    static WARNED: OnceLock<Mutex<HashSet<String>>> = OnceLock::new();
    let mut warned = WARNED
        .get_or_init(Default::default)
        .lock()
        .expect("stdlib shadow warning set poisoned");
    if warned.insert(name.to_string()) {
        eprintln!(
            "warning: {}",
            stdlib_shadow_message(name, &shadowed_path.display().to_string())
        );
    }
}

/// `(module_name, ignored_project_file)` pairs for every `depends` entry of
/// `items` where the embedded standard library wins over a same-named
/// project file in `module_root`. Feed the result to
/// `AnalyzeOptions::stdlib_shadowed` so `aver check` surfaces the shadowing.
pub fn collect_stdlib_shadowed(items: &[TopLevel], module_root: &str) -> Vec<(String, String)> {
    let Some(module) = visibility::module_decl(items) else {
        return Vec::new();
    };
    module
        .depends
        .iter()
        .filter_map(|dep| {
            stdlib_shadowed_project_file(dep, module_root)
                .map(|path| (dep.clone(), path.display().to_string()))
        })
        .collect()
}

/// Virtual-fs sibling of [`collect_stdlib_shadowed`] for the playground:
/// flags `depends` entries whose name the standard library reserves while
/// the in-memory file map also carries a file for that module.
pub fn collect_stdlib_shadowed_in_map(
    items: &[TopLevel],
    files: &HashMap<String, String>,
) -> Vec<(String, String)> {
    let Some(module) = visibility::module_decl(items) else {
        return Vec::new();
    };
    module
        .depends
        .iter()
        .filter_map(|dep| {
            crate::stdlib::find(dep)?;
            find_file_key_in_map(dep, files).map(|key| (dep.clone(), key))
        })
        .collect()
}

/// Resolve and read a project or standard-library module.
///
/// The standard library is checked first so its module names cannot be
/// shadowed by a project-local file. `Ok(None)` means that neither source owns
/// `name`.
pub fn resolve_module_source(
    name: &str,
    module_root: &str,
) -> Result<Option<ModuleSource>, String> {
    if let Some(module) = resolve_standard_module_source(name) {
        // The embedded module wins, but a same-named project file on disk
        // means the user probably expects their own code to load — say so
        // instead of silently changing program meaning.
        if let Some(shadowed) = find_module_file(name, module_root) {
            warn_stdlib_shadow_once(name, &shadowed);
        }
        return Ok(Some(module));
    }

    let Some(path) = find_module_file(name, module_root) else {
        return Ok(None);
    };
    let source = std::fs::read_to_string(&path)
        .map_err(|e| format!("Cannot read '{}': {}", path.display(), e))?;
    Ok(Some(ModuleSource { path, source }))
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
    // The embedded standard library wins over a same-named virtual file,
    // exactly like the filesystem loaders. No warning is emitted here: this
    // loader's only output channel is `Result<_, String>` (hard errors) and
    // browser builds drop stderr, so the playground surfaces shadowing as an
    // `aver check` diagnostic instead (`collect_stdlib_shadowed_in_map`,
    // wired in `playground::analyze_project`).
    let (key, source) = if let Some(module) = crate::stdlib::find(dep_name) {
        (module.virtual_path.to_string(), module.source.to_string())
    } else {
        let key = find_file_key_in_map(dep_name, files)
            .ok_or_else(|| format!("Module '{}' not found in virtual fs", dep_name))?;
        let source = files.get(&key).expect("resolved virtual module").clone();
        (key, source)
    };

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

    let items =
        parse_source(&source).map_err(|e| format!("Parse error in '{}': {}", dep_name, e))?;
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
        // Standard modules implied by source-typed builtins load even when
        // this module's `depends` never names them — same contract as the
        // filesystem loaders (`load_compile_deps` and friends).
        for implied in crate::stdlib::implicit_stdlib_deps(&items) {
            load_recursive_from_map(&implied, files, loaded, loading, result)?;
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
    let resolved = resolve_module_source(dep_name, module_root)?
        .ok_or_else(|| format!("Module '{}' not found in '{}'", dep_name, module_root))?;
    let path = resolved.path;
    let source = resolved.source;
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

/// Convert pre-loaded modules (parsed virtual-fs items from the
/// playground / LSP / audit paths) into `ModuleInfo` records suitable
/// for `PipelineConfig.dep_modules` and `SymbolTable::build`.
///
/// Each dep goes through `pipeline::run` with
/// `TypecheckMode::WithLoaded(&siblings)` so the resulting
/// `AnalysisResult` populates the same `no_alloc` / recursion facts
/// the disk-loader path produces. The entry-level pipeline still
/// handles cross-module typing separately; per-dep analysis here
/// just unlocks the VM compiler's `no_alloc` fast paths on dep
/// functions instead of forcing the conservative "assume allocates"
/// branch.
pub fn loaded_to_module_info(loaded: &[LoadedModule]) -> Vec<crate::codegen::ModuleInfo> {
    let neutral_policy = crate::ir::NeutralAllocPolicy;
    loaded
        .iter()
        .map(|m| {
            let depends = visibility::module_decl(&m.items)
                .map(|md| md.depends.clone())
                .unwrap_or_default();
            let type_defs: Vec<_> = m
                .items
                .iter()
                .filter_map(|i| match i {
                    TopLevel::TypeDef(td) => Some(td.clone()),
                    _ => None,
                })
                .collect();
            let fn_defs: Vec<_> = m
                .items
                .iter()
                .filter_map(|i| match i {
                    TopLevel::FnDef(fd) if fd.name != "main" => Some(fd.clone()),
                    _ => None,
                })
                .collect();
            // Run the canonical pipeline on a clone of this dep's
            // items, type-checking against the other loaded modules
            // as the source of cross-module references. We feed the
            // analysis result alone back into ModuleInfo; the
            // pipeline-mutated items themselves stay local — the
            // entry's pipeline run sees the original `m.items` shape
            // via WithLoaded just like the typechecker did pre-fix.
            let mut dep_items = m.items.clone();
            let pipeline_result = crate::ir::pipeline::run(
                &mut dep_items,
                crate::ir::PipelineConfig {
                    typecheck: Some(crate::ir::TypecheckMode::WithLoaded(loaded)),
                    run_interp_lower: false,
                    run_buffer_build: false,
                    alloc_policy: Some(&neutral_policy),
                    ..Default::default()
                },
            );
            crate::codegen::ModuleInfo {
                prefix: m.dep_name.clone(),
                depends,
                type_defs,
                fn_defs,
                verify_laws: crate::codegen::collect_verify_laws(&m.items),
                analysis: pipeline_result.analysis,
            }
        })
        .collect()
}

/// Load every dep module declared by `items`'s `Module.depends`, plus
/// every transitive dep, into `codegen::ModuleInfo` records ready to
/// hand to `PipelineConfig.dep_modules`. Standard modules implied by
/// source-typed builtins (`crate::stdlib::implicit_stdlib_deps`) load
/// too, even when `depends` never names them.
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
    // Builtins like `Crypto.sha256` cross nominal types owned by embedded
    // standard modules (`Bytes`, `Digest32`). Codegen backends emit those
    // records from the loaded module set, so the owning modules must load
    // even when the entry never names them in `depends` — otherwise the
    // program passes check/verify and fails late inside the generated
    // artifact. `loaded` dedupes the overlap with the explicit list.
    for dep_name in crate::stdlib::implicit_stdlib_deps(items) {
        load_module_recursive_for_compile(&dep_name, module_root, &mut result, &mut loaded)?;
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
    let resolved = resolve_module_source(name, module_root)?.ok_or_else(|| {
        format!(
            "Cannot find module '{}' in module root '{}'",
            name, module_root
        )
    })?;
    let path = resolved.path;
    let source = resolved.source;
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
    // A dependency module can also call a builtin whose signature crosses
    // stdlib-owned nominal types without naming the owning standard module
    // in its own `depends` — load those implied modules too (see
    // `load_compile_deps` for the entry-level counterpart).
    for dep in crate::stdlib::implicit_stdlib_deps(&items) {
        load_module_recursive_for_compile(&dep, module_root, result, loaded)?;
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
        verify_laws: crate::codegen::collect_verify_laws(&items),
        analysis: pipeline_result.analysis,
    });
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{
        collect_stdlib_shadowed, collect_stdlib_shadowed_in_map, load_module_tree,
        load_module_tree_from_map, parse_source, require_module_declaration, resolve_module_source,
        stdlib_shadowed_project_file,
    };

    #[test]
    fn standard_bytes_module_resolves_without_a_filesystem_root() {
        let resolved = resolve_module_source("Bytes", "/path/that/does/not/exist")
            .expect("resolve standard module")
            .expect("Bytes is shipped with Aver");
        assert_eq!(resolved.path.to_string_lossy(), "<aver-stdlib>/bytes.av");
        assert!(resolved.source.starts_with("module Bytes\n"));

        let loaded = load_module_tree(
            &["Crypto.Digest32".to_string()],
            "/path/that/does/not/exist",
        )
        .expect("load standard module tree");
        assert_eq!(loaded.len(), 2);
        assert_eq!(loaded[0].dep_name, "Bytes");
        assert_eq!(loaded[1].dep_name, "Crypto.Digest32");
    }

    #[test]
    fn standard_bytes_module_is_available_to_virtual_filesystems() {
        let loaded =
            load_module_tree_from_map(&["Crypto.Digest32".to_string()], &Default::default())
                .expect("load embedded standard module in playground");
        assert_eq!(loaded.len(), 2);
        assert_eq!(loaded[0].dep_name, "Bytes");
        assert_eq!(loaded[1].dep_name, "Crypto.Digest32");
    }

    #[test]
    fn unknown_module_still_uses_normal_project_resolution() {
        let resolved =
            resolve_module_source("DefinitelyNotARealModule", ".").expect("resolve unknown module");
        assert!(resolved.is_none());
    }

    #[test]
    fn stdlib_shadowed_project_file_flags_reserved_names_only() {
        let dir = tempfile::tempdir().expect("tempdir");
        std::fs::write(dir.path().join("bytes.av"), "module Bytes\n").expect("write bytes.av");
        std::fs::write(dir.path().join("helpers.av"), "module Helpers\n")
            .expect("write helpers.av");
        let root = dir.path().to_str().expect("utf8 root");

        // Reserved name + same-named project file = shadowed.
        let shadowed = stdlib_shadowed_project_file("Bytes", root).expect("bytes.av is shadowed");
        assert!(shadowed.ends_with("bytes.av"));
        // The embedded module still wins resolution.
        let resolved = resolve_module_source("Bytes", root)
            .expect("resolve")
            .expect("Bytes is shipped with Aver");
        assert_eq!(resolved.path.to_string_lossy(), "<aver-stdlib>/bytes.av");
        // Non-reserved names and reserved names without a project file
        // are not shadowed.
        assert!(stdlib_shadowed_project_file("Helpers", root).is_none());
        assert!(stdlib_shadowed_project_file("Crypto.Digest32", root).is_none());
    }

    #[test]
    fn collect_stdlib_shadowed_reports_depends_entries_with_project_files() {
        let items = parse_source("module Main\n    intent = \"t\"\n    depends [Bytes]\n")
            .expect("parse entry");

        let dir = tempfile::tempdir().expect("tempdir");
        std::fs::write(dir.path().join("bytes.av"), "module Bytes\n").expect("write bytes.av");
        let pairs = collect_stdlib_shadowed(&items, dir.path().to_str().expect("utf8 root"));
        assert_eq!(pairs.len(), 1);
        assert_eq!(pairs[0].0, "Bytes");
        assert!(pairs[0].1.ends_with("bytes.av"));

        // Negative: no project file for the reserved name — no finding.
        let empty = tempfile::tempdir().expect("empty tempdir");
        assert!(collect_stdlib_shadowed(&items, empty.path().to_str().expect("utf8")).is_empty());
    }

    #[test]
    fn collect_stdlib_shadowed_in_map_flags_virtual_files() {
        let items = parse_source("module Main\n    intent = \"t\"\n    depends [Bytes]\n")
            .expect("parse entry");

        let mut files = std::collections::HashMap::new();
        files.insert("bytes.av".to_string(), "module Bytes\n".to_string());
        assert_eq!(
            collect_stdlib_shadowed_in_map(&items, &files),
            vec![("Bytes".to_string(), "bytes.av".to_string())]
        );

        // Negative: the virtual fs has no file for the reserved name.
        assert!(collect_stdlib_shadowed_in_map(&items, &Default::default()).is_empty());
    }

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

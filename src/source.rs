use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt;
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
// Program loader — the entry module plus everything reachable from it
// ---------------------------------------------------------------------------

/// A parsed module ready for backend consumption.
#[derive(Clone, Debug)]
pub struct LoadedModule {
    pub dep_name: String,
    pub items: Vec<TopLevel>,
    pub path: PathBuf,
}

/// Why a module could not be loaded as written.
///
/// `Display` renders the wording [`load_module_tree`] has always used. The
/// command wrappers that historically said things differently build their
/// own text from the fields.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LoadError {
    /// A project file exists but could not be read.
    Read(String),
    /// Neither a project file nor an embedded standard module owns `name`.
    Missing {
        name: String,
        root: String,
        /// The file whose `depends` named it, when the walk started from one.
        required_by: Option<PathBuf>,
    },
    Parse {
        name: String,
        path: PathBuf,
        error: String,
    },
    /// The file fails [`require_module_declaration`]; `message` is its verdict.
    Declaration { path: PathBuf, message: String },
    NameMismatch {
        expected: String,
        dep_name: String,
        found: String,
        path: PathBuf,
    },
    /// The modules being loaded, outermost first, closed by the one that was
    /// re-entered.
    Cycle { chain: Vec<PathBuf> },
}

impl fmt::Display for LoadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LoadError::Read(message) | LoadError::Declaration { message, .. } => {
                f.write_str(message)
            }
            LoadError::Missing { name, root, .. } => {
                write!(f, "Module '{name}' not found in '{root}'")
            }
            LoadError::Parse { name, error, .. } => write!(f, "Parse error in '{name}': {error}"),
            LoadError::NameMismatch {
                expected,
                dep_name,
                found,
                path,
            } => write!(
                f,
                "Module name mismatch: expected '{expected}' (from '{dep_name}'), found '{found}' in '{}'",
                path.display()
            ),
            LoadError::Cycle { chain } => {
                let stems = chain
                    .iter()
                    .map(|path| {
                        path.file_stem()
                            .and_then(|stem| stem.to_str())
                            .map(str::to_string)
                            .unwrap_or_else(|| path.to_string_lossy().into_owned())
                    })
                    .collect::<Vec<_>>();
                write!(f, "Circular import: {}", stems.join(" -> "))
            }
        }
    }
}

impl std::error::Error for LoadError {}

impl From<LoadError> for String {
    fn from(error: LoadError) -> Self {
        error.to_string()
    }
}

/// How [`load_program`] treats a module it cannot use as written.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LoadMode {
    /// Every module must resolve, parse, declare `module` under the name it
    /// was imported by, and the graph must be acyclic. Typing needs all of
    /// that, so this is what the typechecker loads with.
    Strict,
    /// A module that fails to parse or misdeclares itself stays in the
    /// program with [`ProgramModule::fault`] set, for the caller to report
    /// in walk order; name mismatches and cycles are left to the per-module
    /// typecheck, which has always reported them. Only a dependency that
    /// cannot be found at all stops the walk. Report walks (`check`,
    /// `verify`) and the codegen dependency loaders use this.
    Tolerant,
}

/// One source module in a loaded Aver program.
#[derive(Clone, Debug)]
pub struct ProgramModule {
    pub dep_name: String,
    pub source: String,
    pub items: Vec<TopLevel>,
    pub path: PathBuf,
    pub is_entry: bool,
    pub is_stdlib: bool,
    /// Position in the dependency walk: the entry is 0, dependencies count
    /// up in first-seen (parent-before-child) order.
    pub discovery_index: usize,
    /// Set only under [`LoadMode::Tolerant`]: why this module could not be
    /// used as written. A module that failed to parse keeps no items.
    pub fault: Option<LoadError>,
}

impl ProgramModule {
    pub fn as_loaded(&self) -> LoadedModule {
        LoadedModule {
            dep_name: self.dep_name.clone(),
            items: self.items.clone(),
            path: self.path.clone(),
        }
    }
}

/// The entry module and every module reachable from it through `depends`
/// and through the standard modules its builtin calls imply.
///
/// Modules are deduplicated by canonical path and stored leaves-first, with
/// the entry last. Embedded standard-library modules participate exactly
/// like project modules; consumers may choose not to report them.
#[derive(Clone, Debug)]
pub struct Program {
    pub modules: Vec<ProgramModule>,
}

impl Program {
    pub fn entry(&self) -> &ProgramModule {
        self.modules
            .last()
            .expect("a loaded program always contains its entry")
    }

    pub fn dependencies(&self) -> &[ProgramModule] {
        &self.modules[..self.modules.len().saturating_sub(1)]
    }

    /// Dependencies parent-before-child, the order the walk met them in.
    pub fn dependencies_in_discovery_order(&self) -> Vec<&ProgramModule> {
        let mut modules = self.dependencies().iter().collect::<Vec<_>>();
        modules.sort_by_key(|module| module.discovery_index);
        modules
    }

    /// The opt-in check/verify walk: follow only written `depends [...]`
    /// edges (not the implicit standard-module edges) parent-before-child,
    /// each file once — the entry included, so a cycle back to it does not
    /// list it twice.
    pub fn explicit_dependencies_in_discovery_order(&self) -> Vec<&ProgramModule> {
        let by_name = self
            .dependencies()
            .iter()
            .map(|module| (module.dep_name.as_str(), module))
            .collect::<HashMap<_, _>>();
        let mut stack = visibility::module_decl(&self.entry().items)
            .map(|module| module.depends.iter().rev().cloned().collect::<Vec<_>>())
            .unwrap_or_default();
        let mut seen = HashSet::from([canonicalize_path(&self.entry().path)]);
        let mut modules = Vec::new();
        while let Some(name) = stack.pop() {
            let Some(module) = by_name.get(name.as_str()).copied() else {
                continue;
            };
            if !seen.insert(canonicalize_path(&module.path)) {
                continue;
            }
            modules.push(module);
            if let Some(declaration) = visibility::module_decl(&module.items) {
                stack.extend(declaration.depends.iter().rev().cloned());
            }
        }
        modules
    }
}

/// Load the program named by an already parsed entry module.
///
/// A file without a `module` declaration names a program of itself: it has
/// no `depends` to follow, and its builtin calls imply nothing either.
pub fn load_program(
    entry_path: &Path,
    entry_source: &str,
    entry_items: &[TopLevel],
    module_root: &str,
    mode: LoadMode,
) -> Result<Program, LoadError> {
    let mut walk = Walk::new(module_root, mode);
    walk.follow_edges(entry_path, entry_items)?;
    let mut modules = walk.modules;
    let entry_name = visibility::module_decl(entry_items)
        .map(|module| module.name.clone())
        .unwrap_or_else(|| {
            entry_path
                .file_stem()
                .and_then(|stem| stem.to_str())
                .unwrap_or("entry")
                .to_string()
        });
    modules.push(ProgramModule {
        dep_name: entry_name,
        source: entry_source.to_string(),
        items: entry_items.to_vec(),
        path: entry_path.to_path_buf(),
        is_entry: true,
        is_stdlib: false,
        discovery_index: 0,
        fault: None,
    });
    Ok(Program { modules })
}

/// Depth-first dependency walk shared by every loader.
struct Walk<'a> {
    module_root: &'a str,
    mode: LoadMode,
    loaded: HashSet<PathBuf>,
    loading: Vec<PathBuf>,
    modules: Vec<ProgramModule>,
    next_discovery_index: usize,
}

impl<'a> Walk<'a> {
    fn new(module_root: &'a str, mode: LoadMode) -> Self {
        Self {
            module_root,
            mode,
            loaded: HashSet::new(),
            loading: Vec::new(),
            modules: Vec::new(),
            next_discovery_index: 1,
        }
    }

    fn resolve(&self, name: &str, required_by: Option<&Path>) -> Result<ModuleSource, LoadError> {
        resolve_module_source(name, self.module_root)
            .map_err(LoadError::Read)?
            .ok_or_else(|| LoadError::Missing {
                name: name.to_string(),
                root: self.module_root.to_string(),
                required_by: required_by.map(Path::to_path_buf),
            })
    }

    /// Follow the edges out of one module: its written `depends`, then the
    /// standard modules its builtin calls imply.
    fn follow_edges(
        &mut self,
        parent_path: &Path,
        parent_items: &[TopLevel],
    ) -> Result<(), LoadError> {
        let Some(declaration) = visibility::module_decl(parent_items) else {
            return Ok(());
        };
        for name in &declaration.depends {
            let resolved = self.resolve(name, Some(parent_path))?;
            self.load(name, resolved)?;
        }
        let parent_key = canonicalize_path(parent_path);
        for name in crate::stdlib::implicit_stdlib_deps(parent_items) {
            if declaration.depends.contains(&name) {
                continue;
            }
            let resolved = self.resolve(&name, Some(parent_path))?;
            // A standard module's own declarations mention its own nominal
            // types. That is ownership, not an import; a written
            // `depends [Self]` above remains a real cycle.
            if canonicalize_path(&resolved.path) == parent_key {
                continue;
            }
            self.load(&name, resolved)?;
        }
        Ok(())
    }

    fn load(&mut self, dep_name: &str, resolved: ModuleSource) -> Result<(), LoadError> {
        let key = canonicalize_path(&resolved.path);
        if self.loaded.contains(&key) {
            return Ok(());
        }
        if self.loading.contains(&key) {
            return match self.mode {
                LoadMode::Strict => {
                    let mut chain = self.loading.clone();
                    chain.push(key);
                    Err(LoadError::Cycle { chain })
                }
                // The re-entered module's own typecheck reports the cycle.
                LoadMode::Tolerant => Ok(()),
            };
        }
        let discovery_index = self.next_discovery_index;
        self.next_discovery_index += 1;
        let ModuleSource { path, source } = resolved;
        let is_stdlib = path.starts_with("<aver-stdlib>");

        let (items, fault) = match parse_source(&source) {
            Ok(items) => match require_module_declaration(&items, &path.to_string_lossy()) {
                Ok(()) => (items, None),
                Err(message) => (
                    items,
                    Some(LoadError::Declaration {
                        path: path.clone(),
                        message,
                    }),
                ),
            },
            Err(error) => (
                Vec::new(),
                Some(LoadError::Parse {
                    name: dep_name.to_string(),
                    path: path.clone(),
                    error,
                }),
            ),
        };
        if self.mode == LoadMode::Strict {
            if let Some(fault) = fault {
                return Err(fault);
            }
            if let Some(module) = visibility::module_decl(&items) {
                let expected = dep_name.rsplit('.').next().unwrap_or(dep_name);
                if module.name != expected {
                    return Err(LoadError::NameMismatch {
                        expected: expected.to_string(),
                        dep_name: dep_name.to_string(),
                        found: module.name.clone(),
                        path,
                    });
                }
            }
        }

        self.loading.push(key.clone());
        self.follow_edges(&path, &items)?;
        self.loading.pop();

        self.loaded.insert(key);
        self.modules.push(ProgramModule {
            dep_name: dep_name.to_string(),
            source,
            items,
            path,
            is_entry: false,
            is_stdlib,
            discovery_index,
            fault,
        });
        Ok(())
    }
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
    let mut walk = Walk::new(module_root, LoadMode::Strict);
    for name in root_deps {
        let resolved = walk.resolve(name, None)?;
        walk.load(name, resolved)?;
    }
    Ok(walk
        .modules
        .into_iter()
        .map(|module| module.as_loaded())
        .collect())
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
                    run_chars_fusion: false,
                    run_string_index: true,
                    run_list_build: false,
                    alloc_policy: Some(&neutral_policy),
                    ..Default::default()
                },
            );
            crate::codegen::ModuleInfo::from_items(
                m.dep_name.clone(),
                &m.items,
                pipeline_result.analysis,
            )
        })
        .collect()
}

/// Load every dependency of the program behind `items` — written
/// `depends`, their transitive closure, and the standard modules implied by
/// builtin calls — into `codegen::ModuleInfo` records ready to hand to
/// `PipelineConfig.dep_modules`.
///
/// Each dependency goes through the same canonical pipeline as the entry —
/// `pipeline::run` with `TypecheckMode::Full { base_dir: module_root }`,
/// the mutable builder/cursor passes disabled, String indexing enabled,
/// and the neutral alloc policy. Type errors in any dependency surface as
/// `Err`, parent before child.
///
/// The CLI's `commands::load_compile_deps` is the sibling that applies a
/// per-target lowering matrix and exits on failure; every other call site —
/// `vm_verify`, `wasm_gc_verify`, `bench/runner`, the research test — goes
/// through here so the dependency contract stays in one place.
pub fn load_compile_deps(
    items: &[TopLevel],
    module_root: &str,
) -> Result<Vec<crate::codegen::ModuleInfo>, String> {
    let program = load_program(
        Path::new("<entry>"),
        "",
        items,
        module_root,
        LoadMode::Tolerant,
    )
    .map_err(|error| match error {
        LoadError::Missing { name, root, .. } => {
            format!("Cannot find module '{name}' in module root '{root}'")
        }
        other => other.to_string(),
    })?;
    let neutral_policy = crate::ir::NeutralAllocPolicy;
    let mut lowered = BTreeMap::new();
    for module in program.dependencies_in_discovery_order() {
        if let Some(fault) = &module.fault {
            return Err(match fault {
                LoadError::Parse { path, error, .. } => {
                    format!("Parse '{}': {}", path.display(), error)
                }
                other => other.to_string(),
            });
        }
        let mut module_items = module.items.clone();
        let pipeline_result = crate::ir::pipeline::run(
            &mut module_items,
            crate::ir::PipelineConfig {
                typecheck: Some(crate::ir::TypecheckMode::Full {
                    base_dir: Some(module_root),
                }),
                run_interp_lower: false,
                run_buffer_build: false,
                run_chars_fusion: false,
                run_string_index: true,
                run_list_build: false,
                alloc_policy: Some(&neutral_policy),
                ..Default::default()
            },
        );
        if let Some(tc) = pipeline_result.typecheck.as_ref()
            && !tc.errors.is_empty()
        {
            return Err(format!(
                "Type errors in dependency module '{}':\n{}",
                module.dep_name,
                tc.errors
                    .iter()
                    .map(|e| format!("  {}:{}: {}", e.line, e.col, e.message))
                    .collect::<Vec<_>>()
                    .join("\n")
            ));
        }
        lowered.insert(
            module.discovery_index,
            crate::codegen::ModuleInfo::from_items(
                module.dep_name.clone(),
                &module_items,
                pipeline_result.analysis,
            ),
        );
    }
    Ok(program
        .dependencies()
        .iter()
        .filter_map(|module| lowered.remove(&module.discovery_index))
        .collect())
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

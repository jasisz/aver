use std::collections::{HashMap, HashSet};
use std::fmt;
use std::path::{Path, PathBuf};

use crate::ast::TopLevel;
use crate::config::VerifyCaseCeiling;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::visibility;

pub fn parse_source(source: &str) -> Result<Vec<TopLevel>, String> {
    parse_source_with_verify_ceiling(source, VerifyCaseCeiling::compiled_default())
}

/// [`parse_source`] with one explicit ceiling for every verify block.
pub fn parse_source_with_verify_max_cases(
    source: &str,
    max_cases: usize,
) -> Result<Vec<TopLevel>, String> {
    parse_source_with_verify_ceiling(source, VerifyCaseCeiling::flat(max_cases))
}

/// [`parse_source`] under a ceiling already resolved for the file's path, so
/// each verify block gets the number its own function was given.
pub fn parse_source_with_verify_ceiling(
    source: &str,
    ceiling: VerifyCaseCeiling,
) -> Result<Vec<TopLevel>, String> {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize().map_err(|e| e.to_string())?;
    let mut parser = Parser::new(tokens);
    parser.set_verify_ceiling(ceiling);
    parser.parse().map_err(|e| e.to_string())
}

/// Parse one of the user's own project files, under the ceiling the project
/// declared for that file.
///
/// Every command that reads a `.av` off disk parses it through here or
/// through [`Walk::new`], which resolves the same ceiling the same way for
/// each dependency it loads. That is the whole rule: a file the project
/// declared legal is legal at every door, and a file over the ceiling is
/// refused at every door with the project's own number in the message.
///
/// Every other parse in the compiler goes through [`parse_source`] or
/// constructs a [`Parser`] directly and keeps the built-in default — which
/// is what those want, because they parse compiler-synthesized source (TCO
/// hoists, effect-lifting wrappers, hostile stubs, coverage and law probes),
/// never a user's `given` domain.
///
/// A missing or malformed `aver.toml` leaves the default in place here; every
/// command loads and reports the file separately, so a broken one is never
/// swallowed.
pub fn parse_project_source(
    source: &str,
    module_root: &str,
    file: &str,
) -> Result<Vec<TopLevel>, String> {
    parse_source_with_verify_ceiling(source, project_verify_ceiling(module_root, file))
}

/// The module root a command works against when the user names none: the
/// working directory, which is where `aver.toml` is looked for.
pub fn working_module_root() -> String {
    std::env::current_dir()
        .ok()
        .and_then(|dir| dir.into_os_string().into_string().ok())
        .unwrap_or_else(|| ".".to_string())
}

/// The verify-case ceiling the project rooted at `module_root` declares for
/// `file`, or the built-in default when there is no readable `aver.toml`.
pub fn project_verify_ceiling(module_root: &str, file: &str) -> VerifyCaseCeiling {
    match crate::config::ProjectConfig::load_from_dir(Path::new(module_root))
        .ok()
        .flatten()
    {
        Some(config) => verify_ceiling_for(&config, module_root, file),
        None => VerifyCaseCeiling::compiled_default(),
    }
}

/// [`project_verify_ceiling`] for a caller that may hold no project at all:
/// an editor scratch buffer, the playground's virtual filesystem, a
/// candidate law checked outside any root. Nothing to ask means the built-in
/// default — which is what those callers had before any of this existed.
pub fn project_verify_ceiling_or_default(
    module_root: Option<&str>,
    file: Option<&str>,
) -> VerifyCaseCeiling {
    match (module_root, file) {
        (Some(root), Some(file)) => project_verify_ceiling(root, file),
        _ => VerifyCaseCeiling::compiled_default(),
    }
}

/// The ceiling `config` declares for `file`, matched against the same
/// anchored path form `[[verify.costly]].files` globs are matched against
/// everywhere else. For callers that already hold the project's config and
/// must not read a second, possibly different one off disk.
pub fn verify_ceiling_for(
    config: &crate::config::ProjectConfig,
    module_root: &str,
    file: &str,
) -> VerifyCaseCeiling {
    config.verify_case_ceiling(&crate::diagnostics::vm_verify::costly_glob_key(
        file,
        Some(module_root),
    ))
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

/// The two relative paths every loader tries for one canonical module name.
/// Keeping this derivation shared makes the browser virtual filesystem obey
/// exactly the same module identity contract as the CLI filesystem loader.
fn module_file_candidates(name: &str) -> Option<(String, String)> {
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
    Some((lower_rel, exact_rel))
}

pub fn find_module_file(name: &str, module_root: &str) -> Option<PathBuf> {
    let root = Path::new(module_root);
    let (lower_rel, exact_rel) = module_file_candidates(name)?;

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

/// The proof kernel's modules that proof plans build on (`Kernel.Term`,
/// `Kernel.Proof`, `Kernel.Lib`, …), embedded in the compiler. They resolve
/// only where the project has no file of that name.
pub fn resolve_kernel_api_source(name: &str) -> Option<ModuleSource> {
    crate::stdlib::find_kernel_api(name).map(|module| ModuleSource {
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

    // `Kernel.*` names belong to the proof kernel the compiler ships (see
    // `crate::stdlib::find_kernel_api`); a project file never takes one.
    // Only the kernel's own source tree reads them from its files.
    if is_kernel_module(name) && !is_kernel_source_tree(module_root) {
        if let Some(path) = find_module_file(name, module_root) {
            return Err(format!(
                "'{name}' is a module name reserved for the proof kernel; the project file '{}' cannot use it",
                path.display()
            ));
        }
        return Ok(resolve_kernel_api_source(name));
    }
    let Some(path) = find_module_file(name, module_root) else {
        return Ok(resolve_kernel_api_source(name));
    };
    let source = std::fs::read_to_string(&path)
        .map_err(|e| format!("Cannot read '{}': {}", path.display(), e))?;
    Ok(Some(ModuleSource { path, source }))
}

/// Whether `name` is in the namespace reserved for the proof kernel.
pub fn is_kernel_module(name: &str) -> bool {
    name.starts_with("Kernel.")
}

/// Whether `module_root` is the proof kernel's own source tree
/// (`tools/proof-kernel` of the compiler's repository), the one place whose
/// files are the `Kernel.*` modules.
pub fn is_kernel_source_tree(module_root: &str) -> bool {
    let own = Path::new(env!("CARGO_MANIFEST_DIR")).join("tools/proof-kernel");
    canonicalize_path(Path::new(module_root)) == canonicalize_path(&own)
}

/// The modules of the proof kernel a plans module may depend on.
pub const PLANS_KERNEL_API: [&str; 3] = ["Kernel.Term", "Kernel.Proof", "Kernel.Lib"];

/// Why `parent` may not depend on the kernel module `dep`, if it may not:
/// only a plans module depends on the kernel, and only on its public
/// modules. The kernel's own modules and its source tree are exempt.
fn kernel_dependency_refusal(
    parent_path: &Path,
    parent_items: &[TopLevel],
    dep: &str,
    module_root: &str,
) -> Option<String> {
    if !is_kernel_module(dep)
        || parent_path.starts_with("<aver-stdlib>")
        || is_kernel_source_tree(module_root)
    {
        return None;
    }
    let decl = visibility::module_decl(parent_items)?;
    if decl.plans.is_none() {
        return Some(format!(
            "module '{}' depends on '{dep}': only a plans module may depend on the proof kernel's modules",
            decl.name
        ));
    }
    (!PLANS_KERNEL_API.contains(&dep)).then(|| {
        format!(
            "plans module '{}' depends on '{dep}': a plans module may depend on {} of the proof kernel, and on nothing else of it",
            decl.name,
            PLANS_KERNEL_API.join(", ")
        )
    })
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
#[derive(Clone, Debug, Default)]
pub struct Program {
    pub modules: Vec<ProgramModule>,
    /// Per-path memo of a process dependency's already-lowered form,
    /// computed the first time [`Self::loaded_dependencies_for`] needs it
    /// (see [`lowering_memo_for`]). A whole-program walk calls that method
    /// once per report unit, and units share most of their dependency cone,
    /// so without this a shared process dependency would be re-typechecked
    /// and re-lowered from scratch by every unit that reaches it instead of
    /// once for the whole run.
    ///
    /// A module that cannot hold a process never enters this map, so a
    /// program without one pays nothing extra to build or consult it, and a
    /// command that loads a program without asking for a unit's
    /// dependencies never builds it at all. A `OnceLock` filled at most once
    /// keeps `Program` `Sync`.
    lowering_memo: std::sync::OnceLock<std::sync::Arc<LoweringMemo>>,
    /// The capabilities this program's manifest answers with a module of it,
    /// carried so every re-lowering of a dependency cuts its `yield`
    /// functions at exactly the calls the whole-program walk cut them at.
    marked: crate::config::MarkedCapabilities,
}

// Report units are prepared with `rayon` across worker threads; a `Program`
// shared by reference into that pool must stay `Sync`. The lowering memo is
// a plain map filled once at construction rather than a `RefCell` mutated
// per lookup, so this holds.
const _: fn() = || {
    fn assert_sync<T: Sync>() {}
    assert_sync::<Program>();
};

impl Program {
    /// The capabilities the manifest answers, with an unnamed default loop
    /// already bound to this program's entry, so a dependency lowered with
    /// them never becomes the loop's home.
    pub fn marked(&self) -> &crate::config::MarkedCapabilities {
        &self.marked
    }

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

    /// The modules a report walks, leaves-first with the entry last: every
    /// project module of the program. Embedded standard modules are typed
    /// and compiled like any other but are not units of a report — their
    /// own `verify` blocks are checked per release, not per program.
    pub fn report_units(&self) -> impl Iterator<Item = &ProgramModule> {
        self.modules.iter().filter(|module| !module.is_stdlib)
    }

    /// Parsed dependency closure of one module already present in this
    /// program, in the program's canonical leaves-first order.
    ///
    /// Whole-program reports use this after their single graph walk so each
    /// module can be type-checked and compiled without asking the filesystem
    /// loader to rediscover its dependency cone. `module` may be the entry or
    /// any project dependency returned by [`Self::report_units`].
    pub fn loaded_dependencies_for(
        &self,
        module: &ProgramModule,
    ) -> Result<Vec<LoadedModule>, LoadError> {
        let by_name = self
            .modules
            .iter()
            .map(|candidate| (candidate.dep_name.as_str(), candidate))
            .collect::<HashMap<_, _>>();
        let target_key = canonicalize_path(&module.path);
        let mut reachable = HashSet::new();
        let mut loading = vec![target_key];
        validate_program_module_name(module, &module.dep_name)?;
        collect_dependency_keys(module, &by_name, &mut reachable, &mut loading)?;
        // A process dependency is lowered once for the whole program, the
        // first time any unit asks: that form is handed out and marked settled,
        // so the pipeline call below neither re-checks nor re-lowers what the
        // program already paid for. A module that never could hold a process
        // is never in the memo, so it is a plain clone the pipeline call
        // skips on its own.
        let mut settled = Vec::new();
        let memo = self
            .lowering_memo
            .get_or_init(|| lowering_memo_for(self.dependencies(), &self.marked));
        let mut modules: Vec<LoadedModule> = self
            .modules
            .iter()
            .filter(|candidate| reachable.contains(&canonicalize_path(&candidate.path)))
            .map(|candidate| match memo.get(&candidate.path) {
                Some(lowered) => {
                    settled.push(true);
                    lowered.clone()
                }
                None => {
                    settled.push(false);
                    candidate.as_loaded()
                }
            })
            .collect();
        // Each module of this program is prepared as a unit of its own,
        // through its own front door: a dependency that fails to lower
        // reports it there, against its own file, so the errors this call
        // hands back would be the same ones twice.
        let _ = crate::ir::pipeline::lower_loaded_process_modules_except(
            &mut modules,
            None,
            &self.marked,
            |index| settled[index],
        );
        Ok(modules)
    }
}

type LoweringMemo = HashMap<PathBuf, LoadedModule>;

/// One run of the process lowering over a leaves-first module list, kept with
/// the inputs it read so a later caller holding the very same inputs can take
/// its result instead of checking and lowering every module again.
struct SharedLowering {
    written: Vec<(LoadedModule, String)>,
    module_root: Option<String>,
    marked: crate::config::MarkedCapabilities,
    lowered: Vec<LoadedModule>,
    errors: Vec<crate::types::checker::TypeError>,
    failed: HashSet<usize>,
}

impl SharedLowering {
    fn read(
        &self,
        written: &[ProgramModule],
        module_root: Option<&str>,
        marked: &crate::config::MarkedCapabilities,
    ) -> bool {
        self.module_root.as_deref() == module_root
            && self.marked == *marked
            && self.written.len() == written.len()
            && self
                .written
                .iter()
                .zip(written)
                .all(|((seen, source), module)| {
                    seen.path == module.path
                        && seen.dep_name == module.dep_name
                        && *source == module.source
                        && seen.items == module.items
                })
    }

    /// The lowered modules as a caller of its own, with function bodies that
    /// no other caller shares: the type checker stamps the bodies it walks,
    /// and a copy handed to one command must not carry stamps another
    /// command's check left behind.
    fn lowered(&self) -> Vec<LoadedModule> {
        self.lowered.iter().map(detached).collect()
    }
}

fn detached(module: &LoadedModule) -> LoadedModule {
    LoadedModule {
        dep_name: module.dep_name.clone(),
        items: module
            .items
            .iter()
            .map(|item| match item {
                TopLevel::FnDef(fd) => TopLevel::FnDef(crate::ast::FnDef {
                    body: std::sync::Arc::new(fd.body.as_ref().clone()),
                    ..fd.clone()
                }),
                other => other.clone(),
            })
            .collect(),
        path: module.path.clone(),
    }
}

/// Lowerings run in this process, newest last. One command reaches the same
/// module list several times: the provider host plans a program and then the
/// command walks it, each load through a cache of its own, and a module's
/// own check reads its dependency tree more than once. The lowering is a
/// function of exactly the inputs [`SharedLowering::read`] compares, so a
/// hit is the result that caller would compute. Bounded, so a long-lived
/// process walking many programs holds only the last few.
static SHARED_LOWERINGS: std::sync::Mutex<Vec<std::sync::Arc<SharedLowering>>> =
    std::sync::Mutex::new(Vec::new());
const SHARED_LOWERINGS_KEPT: usize = 4;

/// [`crate::ir::pipeline::lower_loaded_process_modules`] over `written`, or
/// the result of an earlier call in this process that read the same inputs.
fn lower_shared(
    written: &[ProgramModule],
    module_root: Option<&str>,
    marked: &crate::config::MarkedCapabilities,
) -> std::sync::Arc<SharedLowering> {
    let found = SHARED_LOWERINGS.lock().ok().and_then(|runs| {
        runs.iter()
            .find(|run| run.read(written, module_root, marked))
            .map(std::sync::Arc::clone)
    });
    if let Some(run) = found {
        return run;
    }
    let mut lowered: Vec<LoadedModule> = written.iter().map(ProgramModule::as_loaded).collect();
    let (errors, failed) =
        crate::ir::pipeline::lower_loaded_process_modules(&mut lowered, module_root, marked);
    let run = std::sync::Arc::new(SharedLowering {
        written: written
            .iter()
            .map(|module| (module.as_loaded(), module.source.clone()))
            .collect(),
        module_root: module_root.map(str::to_string),
        marked: marked.clone(),
        lowered,
        errors,
        failed,
    });
    if let Ok(mut runs) = SHARED_LOWERINGS.lock() {
        if runs.len() >= SHARED_LOWERINGS_KEPT {
            runs.remove(0);
        }
        runs.push(std::sync::Arc::clone(&run));
    }
    run
}

/// Lowered form of every dependency that may contain a process, computed
/// once for the whole program. `dependencies` must be leaves-first (the
/// invariant [`Program::modules`] already keeps), so a module later in the
/// slice can see an earlier one's lowered form as its own dependency.
///
/// A program none of whose dependencies could hold a process neither clones
/// nor lowers anything here.
fn lowering_memo_for(
    dependencies: &[ProgramModule],
    marked: &crate::config::MarkedCapabilities,
) -> std::sync::Arc<LoweringMemo> {
    if !dependencies
        .iter()
        .any(|module| lowering_candidate(&module.items))
    {
        return std::sync::Arc::default();
    }
    let run = lower_shared(dependencies, None, marked);
    std::sync::Arc::new(
        run.lowered
            .iter()
            .zip(dependencies)
            .enumerate()
            // A module that failed to lower is left out so the next caller
            // retries it (and hits the same errors) instead of memoizing a
            // broken half-state.
            .filter(|(index, (_, written))| {
                lowering_candidate(&written.items) && !run.failed.contains(index)
            })
            .map(|(_, (lowered, _))| (lowered.path.clone(), detached(lowered)))
            .collect(),
    )
}

/// Whether the lowering ever rewrites a module with these items: one that
/// may hold a process, or one with nested patterns to compile.
fn lowering_candidate(items: &[TopLevel]) -> bool {
    crate::yield_lowering::may_have_processes(items)
        || crate::ir::nested_patterns::has_nested_patterns(items)
}

fn validate_program_module_name(module: &ProgramModule, dep_name: &str) -> Result<(), LoadError> {
    let Some(declaration) = visibility::module_decl(&module.items) else {
        return Ok(());
    };
    let expected = dep_name.rsplit('.').next().unwrap_or(dep_name);
    if declaration.name == expected {
        return Ok(());
    }
    Err(LoadError::NameMismatch {
        expected: expected.to_string(),
        dep_name: dep_name.to_string(),
        found: declaration.name.clone(),
        path: module.path.clone(),
    })
}

fn collect_dependency_keys(
    module: &ProgramModule,
    by_name: &HashMap<&str, &ProgramModule>,
    reachable: &mut HashSet<PathBuf>,
    loading: &mut Vec<PathBuf>,
) -> Result<(), LoadError> {
    let Some(declaration) = visibility::module_decl(&module.items) else {
        return Ok(());
    };
    let explicit = declaration.depends.iter().cloned().collect::<HashSet<_>>();
    let mut names = declaration.depends.clone();
    for implied in crate::stdlib::implicit_stdlib_deps(&module.items) {
        if !explicit.contains(&implied) {
            names.push(implied);
        }
    }

    let module_key = canonicalize_path(&module.path);
    for name in names {
        let Some(dependency) = by_name.get(name.as_str()).copied() else {
            // A tolerant walk still resolves every edge before constructing a
            // Program. A missing node here therefore means the graph itself
            // is inconsistent, rather than another filesystem miss.
            return Err(LoadError::Missing {
                name,
                root: "<loaded program>".to_string(),
                required_by: Some(module.path.clone()),
            });
        };
        let key = canonicalize_path(&dependency.path);
        // Source-typed standard modules may imply themselves through their
        // own builtins. The disk walk excludes that ownership edge too; an
        // explicitly written `depends [Self]` remains a real cycle.
        if key == module_key && !explicit.contains(&name) {
            continue;
        }
        validate_program_module_name(dependency, &name)?;
        if let Some(start) = loading.iter().position(|candidate| candidate == &key) {
            let mut chain = loading[start..].to_vec();
            chain.push(key);
            return Err(LoadError::Cycle { chain });
        }
        if !reachable.insert(key.clone()) {
            continue;
        }
        loading.push(key);
        collect_dependency_keys(dependency, by_name, reachable, loading)?;
        loading.pop();
    }
    Ok(())
}

/// Load the program named by an already parsed entry module.
///
/// A file without a `module` declaration names a program of itself: it has no
/// written `depends`, but standard modules implied by its calls and nominal
/// boundary types are still ordinary dependencies of the compiled program.
pub fn load_program(
    entry_path: &Path,
    entry_source: &str,
    entry_items: &[TopLevel],
    module_root: &str,
    mode: LoadMode,
) -> Result<Program, LoadError> {
    let mut cache = ProgramLoadCache::default();
    load_program_with_cache(
        entry_path,
        entry_source,
        entry_items,
        module_root,
        mode,
        &mut cache,
    )
}

/// Sources and parsed dependency modules shared by several program walks.
///
/// Directory reports often name every project file as an entry. Their
/// dependency cones overlap heavily, so rebuilding each cone from disk turns
/// a linear project walk into repeated IO and parsing. A command creates one
/// cache for one module root and passes it to [`load_program_with_cache`];
/// individual walks still own discovery order, and each whole-program graph
/// view explicitly validates dependency names and cycles before reuse.
#[derive(Default)]
pub struct ProgramLoadCache {
    resolved: HashMap<String, Result<Option<ModuleSource>, String>>,
    parsed: HashMap<PathBuf, CachedProgramModule>,
    verify_config: Option<Option<crate::config::ProjectConfig>>,
    /// Capabilities answered anywhere in a batch of programs a command walks
    /// together, as (capability, module): a library module reached as the
    /// entry of its own walk is lowered against the answer modules of the
    /// programs it belongs to, which its own cone may not reach.
    batch_answers: Vec<(String, String)>,
}

impl ProgramLoadCache {
    /// Answer every later walk against these answered capabilities as well
    /// as against the ones its own modules declare.
    pub fn set_batch_answers(&mut self, answers: Vec<(String, String)>) {
        self.batch_answers = answers;
    }
}

#[derive(Clone)]
struct CachedProgramModule {
    source: String,
    items: Vec<TopLevel>,
    path: PathBuf,
    is_stdlib: bool,
    fault: Option<CachedModuleFault>,
}

#[derive(Clone)]
enum CachedModuleFault {
    Parse(String),
    Declaration(String),
}

/// [`load_program`] with a command-scoped dependency cache.
///
/// The cache is deliberately only the immutable input layer. Each call still
/// performs its own graph walk, while [`Program::loaded_dependencies_for`]
/// validates names and cycles before a report reuses the resulting graph.
/// Sharing the cache therefore cannot make ownership depend on execution
/// order.
pub fn load_program_with_cache(
    entry_path: &Path,
    entry_source: &str,
    entry_items: &[TopLevel],
    module_root: &str,
    mode: LoadMode,
    cache: &mut ProgramLoadCache,
) -> Result<Program, LoadError> {
    let mut walk = Walk::new(module_root, mode, cache);
    let mut entry_items = entry_items.to_vec();
    if let Some(module) = visibility::module_decl(&entry_items) {
        walk.marked = walk.marked.with_run_entry(&module.name);
    }
    walk.follow_edges(entry_path, &entry_items)?;
    // A process is known once the answer modules are: an entry that requests
    // something they answer, or calls `Run.turn`, gets the generated loop's
    // imports, and the walk reads the ones it has not read yet.
    let answered = walk.marked.with_items(
        walk.modules
            .iter()
            .map(|module| (module.dep_name.as_str(), module.items.as_slice()))
            .chain(std::iter::once((
                visibility::module_decl(&entry_items)
                    .map(|module| module.name.as_str())
                    .unwrap_or_default(),
                entry_items.as_slice(),
            ))),
    );
    if answered.may_request(&entry_items) {
        answered.add_loop_dependencies(&mut entry_items);
        walk.follow_edges(entry_path, &entry_items)?;
    }
    // The answer modules say what they answer in their own headers, so the
    // program's answered capabilities are known once its modules are.
    let entry_decl_name = visibility::module_decl(&entry_items)
        .map(|module| module.name.clone())
        .unwrap_or_default();
    let marked = walk
        .marked
        .with_items(
            walk.modules
                .iter()
                .map(|module| (module.dep_name.as_str(), module.items.as_slice()))
                .chain(std::iter::once((
                    entry_decl_name.as_str(),
                    entry_items.as_slice(),
                ))),
        )
        .with_answer_pairs(&walk.cache.batch_answers);
    let mut modules = walk.modules;
    let entry_name = visibility::module_decl(&entry_items)
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
    Ok(Program {
        modules,
        lowering_memo: std::sync::OnceLock::new(),
        marked,
    })
}

/// Depth-first dependency walk shared by every loader.
struct Walk<'a> {
    module_root: &'a str,
    mode: LoadMode,
    /// Read once per walk from the project's `aver.toml`, so every module of
    /// one program expands its verify cases under the same policy — the
    /// ceiling itself is resolved per file, because `[[verify.costly]]`
    /// scopes itself by file glob as well as by function name.
    verify_config: Option<crate::config::ProjectConfig>,
    /// The capabilities this project answers itself, read once per walk from
    /// the project's `aver.toml`: the `yield` lowering cuts a process at every
    /// call to one of their operations.
    marked: crate::config::MarkedCapabilities,
    loaded: HashSet<PathBuf>,
    loading: Vec<PathBuf>,
    modules: Vec<ProgramModule>,
    next_discovery_index: usize,
    cache: &'a mut ProgramLoadCache,
}

impl<'a> Walk<'a> {
    fn new(module_root: &'a str, mode: LoadMode, cache: &'a mut ProgramLoadCache) -> Self {
        let verify_config = cache
            .verify_config
            .get_or_insert_with(|| {
                crate::config::ProjectConfig::load_from_dir(Path::new(module_root))
                    .ok()
                    .flatten()
            })
            .clone();
        let marked = crate::config::MarkedCapabilities::from_config(verify_config.as_ref());
        Self {
            module_root,
            mode,
            verify_config,
            marked,
            loaded: HashSet::new(),
            loading: Vec::new(),
            modules: Vec::new(),
            next_discovery_index: 1,
            cache,
        }
    }

    /// The ceiling this walk's project declares for one of its modules.
    fn verify_ceiling(&self, path: &Path) -> VerifyCaseCeiling {
        match &self.verify_config {
            Some(config) => verify_ceiling_for(config, self.module_root, &path.to_string_lossy()),
            None => VerifyCaseCeiling::compiled_default(),
        }
    }

    fn resolve(
        &mut self,
        name: &str,
        required_by: Option<&Path>,
    ) -> Result<ModuleSource, LoadError> {
        let resolved = self
            .cache
            .resolved
            .entry(name.to_string())
            .or_insert_with(|| resolve_module_source(name, self.module_root))
            .clone();
        resolved
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
        let written_dependencies = visibility::module_decl(parent_items)
            .map(|declaration| declaration.depends.as_slice())
            .unwrap_or_default();
        for name in written_dependencies {
            if let Some(why) =
                kernel_dependency_refusal(parent_path, parent_items, name, self.module_root)
            {
                return Err(LoadError::Read(why));
            }
            let resolved = self.resolve(name, Some(parent_path))?;
            self.load(name, resolved)?;
        }
        let parent_key = canonicalize_path(parent_path);
        for name in crate::stdlib::implicit_stdlib_deps(parent_items) {
            if written_dependencies.contains(&name) {
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
        let ceiling = self.verify_ceiling(&path);
        let cached = self.cache.parsed.entry(key.clone()).or_insert_with(|| {
            let is_stdlib = path.starts_with("<aver-stdlib>");
            let (items, fault) = match parse_source_with_verify_ceiling(&source, ceiling) {
                Ok(items) => match require_module_declaration(&items, &path.to_string_lossy()) {
                    Ok(()) => (items, None),
                    Err(message) => (items, Some(CachedModuleFault::Declaration(message))),
                },
                Err(error) => (Vec::new(), Some(CachedModuleFault::Parse(error))),
            };
            CachedProgramModule {
                source,
                items,
                path,
                is_stdlib,
                fault,
            }
        });
        let source = cached.source.clone();
        let items = cached.items.clone();
        let path = cached.path.clone();
        let is_stdlib = cached.is_stdlib;
        let fault = cached.fault.as_ref().map(|fault| match fault {
            CachedModuleFault::Parse(error) => LoadError::Parse {
                name: dep_name.to_string(),
                path: path.clone(),
                error: error.clone(),
            },
            CachedModuleFault::Declaration(message) => LoadError::Declaration {
                path: path.clone(),
                message: message.clone(),
            },
        });
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
    let marked = marked_capabilities_in_map(files).with_items(
        result
            .iter()
            .map(|module| (module.dep_name.as_str(), module.items.as_slice())),
    );
    // The playground analyses every file of the project separately, so a
    // module that fails to lower reports it under its own name there.
    let _ = crate::ir::pipeline::lower_loaded_yield_modules(&mut result, None, &marked);
    Ok(result)
}

/// The job kinds a virtual project binds, read from the `aver.toml` of its
/// own file map. The capabilities it answers come from its modules.
///
/// A browser project is a project: it has a manifest if the author wrote one,
/// and without this the playground would cut no `yield` function at all and
/// tell its author to edit a file the playground does not have. A map with no
/// manifest, or one that does not parse, marks nothing — the same answer a
/// directory without an `aver.toml` gives.
fn marked_capabilities_in_map(
    files: &HashMap<String, String>,
) -> crate::config::MarkedCapabilities {
    let Some(content) = files.get("aver.toml") else {
        return crate::config::MarkedCapabilities::none();
    };
    let Ok(config) = crate::config::ProjectConfig::parse(content) else {
        return crate::config::MarkedCapabilities::none();
    };
    crate::config::MarkedCapabilities::from_config(Some(&config))
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
    let (lower_rel, exact_rel) = module_file_candidates(dep_name)?;
    for candidate in [&lower_rel, &exact_rel] {
        if files.contains_key(candidate) {
            return Some(candidate.clone());
        }
    }
    None
}

/// Load a dependency tree starting from `root_deps`, with the `yield`
/// functions of every module lowered, and the diagnostics of any module
/// whose lowering FAILED: the importer about to read them reports those,
/// because they say why the protocol it is looking for is not there.
///
/// Returns modules in dependency order (leaves first).
/// Validates module declarations and detects circular imports.
pub fn load_module_tree_with_lowering(
    root_deps: &[String],
    module_root: &str,
) -> Result<(Vec<LoadedModule>, Vec<crate::types::checker::TypeError>), String> {
    let mut cache = ProgramLoadCache::default();
    let mut walk = Walk::new(module_root, LoadMode::Strict, &mut cache);
    for name in root_deps {
        let resolved = walk.resolve(name, None)?;
        walk.load(name, resolved)?;
    }
    let marked = crate::config::MarkedCapabilities::for_project_dir(Some(module_root)).with_items(
        walk.modules
            .iter()
            .map(|module| (module.dep_name.as_str(), module.items.as_slice())),
    );
    if !walk
        .modules
        .iter()
        .any(|module| lowering_candidate(&module.items))
    {
        let modules = walk
            .modules
            .into_iter()
            .map(|module| LoadedModule {
                dep_name: module.dep_name,
                items: module.items,
                path: module.path,
            })
            .collect();
        return Ok((modules, Vec::new()));
    }
    let run = lower_shared(&walk.modules, Some(module_root), &marked);
    Ok((run.lowered(), run.errors.clone()))
}

/// [`load_module_tree_with_lowering`] for the callers that run once a door
/// has already type-checked the program: code generation, the VM compiler,
/// the replay backends, the test harnesses that build a dependency list by
/// hand. A dependency that cannot be lowered has been reported by then,
/// and saying it again here would say it twice.
pub fn load_module_tree(
    root_deps: &[String],
    module_root: &str,
) -> Result<Vec<LoadedModule>, String> {
    load_module_tree_with_lowering(root_deps, module_root).map(|(modules, _)| modules)
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

/// Both views of a dependency graph prepared for one entry pipeline.
/// `modules` is target-lowered codegen input; `loaded` is the already-checked
/// closure the entry uses to rebuild import surfaces without walking
/// dependency bodies again — lowered wherever a module has `yield`
/// functions, unchanged otherwise.
pub struct PreparedCompileDeps {
    pub modules: Vec<crate::codegen::ModuleInfo>,
    pub loaded: Vec<LoadedModule>,
    /// The capabilities this project answers itself, read from its manifest
    /// once. Every door that lowers the entry against these dependencies
    /// passes it on, so the entry's `yield` functions are cut at exactly the
    /// calls the dependencies' were.
    pub marked: crate::config::MarkedCapabilities,
}

/// Prepare a codegen dependency graph once, leaves-first, and retain the
/// loaded closure — lowered for any module with `yield` functions — for the
/// entry module's `WithCheckedLoaded` pass.
///
/// This is the library counterpart of the CLI's target-aware loader. The old
/// implementation selected `Full` separately for every dependency, causing
/// each importer to reload and recheck its complete transitive cone before
/// callers checked the entry in full once more.
pub fn load_compile_deps(
    items: &[TopLevel],
    module_root: &str,
) -> Result<PreparedCompileDeps, String> {
    load_compile_deps_with_runtime_strings(items, module_root, false)
}

/// Prepare dependencies for an ordinary wasm-gc/wasip2 runtime artifact.
///
/// The generic loader above intentionally preserves its historical neutral
/// shape for analysis and proof callers. Runtime wasm verification needs the
/// same closed String builder/cursor and packed-byte contracts as `aver run`,
/// including inside dependencies such as `Bytes.toHex` / `Bytes.fromHex`.
#[cfg(feature = "wasm")]
pub(crate) fn load_compile_deps_for_wasm_runtime(
    items: &[TopLevel],
    module_root: &str,
) -> Result<PreparedCompileDeps, String> {
    load_compile_deps_with_runtime_strings(items, module_root, true)
}

fn load_compile_deps_with_runtime_strings(
    items: &[TopLevel],
    module_root: &str,
    runtime_strings: bool,
) -> Result<PreparedCompileDeps, String> {
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
    for module in program.dependencies_in_discovery_order() {
        if let Some(fault) = &module.fault {
            return Err(match fault {
                LoadError::Parse { path, error, .. } => {
                    format!("Parse '{}': {}", path.display(), error)
                }
                other => other.to_string(),
            });
        }
    }

    let marked = program.marked.clone();
    let neutral_policy = crate::ir::NeutralAllocPolicy;
    let mut modules = Vec::with_capacity(program.dependencies().len());
    for module in program.dependencies() {
        let loaded = program
            .loaded_dependencies_for(module)
            .map_err(|error| error.to_string())?;
        let mut module_items = module.items.clone();
        let pipeline_result = crate::ir::pipeline::run(
            &mut module_items,
            crate::ir::PipelineConfig {
                typecheck: Some(crate::ir::TypecheckMode::WithCheckedLoaded(&loaded)),
                marked: marked.clone(),
                run_interp_lower: false,
                run_buffer_build: runtime_strings,
                run_chars_fusion: runtime_strings,
                run_string_index: true,
                run_list_build: false,
                run_byte_sink: runtime_strings,
                keep_printable_unfused: runtime_strings,
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
        modules.push(crate::codegen::ModuleInfo::from_items(
            module.dep_name.clone(),
            &module_items,
            pipeline_result.analysis,
        ));
    }

    let loaded = program
        .loaded_dependencies_for(program.entry())
        .map_err(|error| error.to_string())?;
    Ok(PreparedCompileDeps {
        modules,
        loaded,
        marked,
    })
}

#[cfg(test)]
mod tests {
    use super::{
        LoadMode, ProgramLoadCache, collect_stdlib_shadowed, collect_stdlib_shadowed_in_map,
        load_compile_deps, load_module_tree, load_module_tree_from_map, load_program_with_cache,
        parse_source, require_module_declaration, resolve_module_source,
        stdlib_shadowed_project_file,
    };

    #[test]
    fn compile_dependency_preparation_checks_bodies_once_leaves_first() {
        let root = tempfile::tempdir().expect("module root");
        std::fs::write(
            root.path().join("B.av"),
            "module B\n    exposes [value]\n\nfn value() -> Int\n    1\n",
        )
        .expect("write B");
        std::fs::write(
            root.path().join("A.av"),
            "module A\n    depends [B]\n    exposes [value]\n\nfn value() -> Int\n    B.value()\n",
        )
        .expect("write A");
        let entry =
            parse_source("module Main\n    depends [A]\n\nfn main() -> Int\n    A.value()\n")
                .expect("parse entry");
        let root_str = root.path().to_string_lossy();

        let mut prepared = load_compile_deps(&entry, &root_str).expect("prepare valid graph");
        assert_eq!(
            prepared
                .modules
                .iter()
                .map(|module| module.prefix.as_str())
                .collect::<Vec<_>>(),
            vec!["B", "A"]
        );

        // The entry seam trusts only a graph returned by the preparation
        // above: changing a dependency body afterwards does not recursively
        // recheck it, but its signature remains visible to the importer.
        let broken_b = parse_source(
            "module B\n    exposes [value]\n\nfn value() -> Int\n    \"not an Int\"\n",
        )
        .expect("parse deliberately ill-typed B");
        prepared
            .loaded
            .iter_mut()
            .find(|module| module.dep_name == "B")
            .expect("loaded B")
            .items = broken_b;
        let entry_check = crate::ir::pipeline::typecheck(
            &entry,
            &crate::ir::TypecheckMode::WithCheckedLoaded(&prepared.loaded),
        );
        assert!(entry_check.errors.is_empty(), "{:?}", entry_check.errors);

        // The trust seam is not public input: preparing that same broken body
        // from source rejects it before an importer can select
        // `WithCheckedLoaded`.
        std::fs::write(
            root.path().join("B.av"),
            "module B\n    exposes [value]\n\nfn value() -> Int\n    \"not an Int\"\n",
        )
        .expect("replace B");
        let error = match load_compile_deps(&entry, &root_str) {
            Ok(_) => panic!("preparation must check B's body"),
            Err(error) => error,
        };
        assert!(
            error.contains("Type errors in dependency module 'B'"),
            "{error}"
        );
    }

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
    fn moduleless_program_loads_its_implicit_standard_capability_types() {
        let source = "fn status(response: Http.Response) -> Int\n    response.status\n";
        let items = parse_source(source).expect("parse moduleless program");
        let mut cache = ProgramLoadCache::default();
        let program = load_program_with_cache(
            std::path::Path::new("probe.av"),
            source,
            &items,
            "/path/that/does/not/exist",
            LoadMode::Strict,
            &mut cache,
        )
        .expect("load moduleless standard dependency");
        assert_eq!(
            program
                .dependencies()
                .iter()
                .map(|module| module.dep_name.as_str())
                .collect::<Vec<_>>(),
            vec!["Http"]
        );
    }

    #[test]
    fn virtual_filesystem_uses_the_same_canonical_path_as_disk() {
        let source = "module User\n    intent = \"test\"\n".to_string();
        let mut leaf_only = std::collections::HashMap::new();
        leaf_only.insert("user.av".to_string(), source.clone());
        let error = load_module_tree_from_map(&["Domain.User".to_string()], &leaf_only)
            .expect_err("a dotted dependency must not fall back to a leaf filename");
        assert!(error.contains("Module 'Domain.User' not found"), "{error}");

        let mut canonical = std::collections::HashMap::new();
        canonical.insert("domain/user.av".to_string(), source);
        let loaded = load_module_tree_from_map(&["Domain.User".to_string()], &canonical)
            .expect("canonical virtual path should load");
        assert_eq!(loaded.len(), 1);
        assert_eq!(loaded[0].dep_name, "Domain.User");
    }

    #[test]
    fn a_virtual_project_reads_its_answer_modules_for_what_it_answers() {
        let mut files = std::collections::HashMap::new();
        files.insert(
            "pool.av".to_string(),
            "module Pool\n    kind = capability\n    semantics = effectful\n    intent = \"test\"\n    exposes [claim]\n\noperation claim(peer: Int) -> Int\n    ? \"The handle for one peer.\"\n    oracle = generative\n    replay = recorded\n".to_string(),
        );
        files.insert(
            "pooled.av".to_string(),
            "module Pooled\n    intent = \"test\"\n    depends [Pool]\n    answers [Pool]\n\nfn fresh() -> Int\n    ? \"Nothing claimed.\"\n    0\n".to_string(),
        );
        files.insert(
            "looper.av".to_string(),
            "module Looper\n    intent = \"test\"\n    depends [Pool, Pooled]\n    effects [Pool.claim]\n    exposes [loop]\n\nfn loop(n: Int) -> Int\n    ? \"Asks the pool once.\"\n    ! [Pool.claim]\n    Pool.claim(n)\n".to_string(),
        );
        let loaded =
            load_module_tree_from_map(&["Looper".to_string(), "Pooled".to_string()], &files)
                .expect("the virtual project loads");
        let looper = loaded
            .iter()
            .find(|module| module.dep_name == "Looper")
            .expect("Looper is loaded");
        let names: Vec<&str> = looper
            .items
            .iter()
            .filter_map(|item| match item {
                crate::ast::TopLevel::FnDef(fd) => Some(fd.name.as_str()),
                _ => None,
            })
            .collect();
        assert!(
            names.contains(&"__loopStart") && !names.contains(&"loop"),
            "the answers header of a module Looper depends on is what says Pool.claim is a request: {names:?}"
        );
        let pool = loaded
            .iter()
            .find(|module| module.dep_name == "Pool")
            .expect("Pool is loaded");
        assert!(
            !pool.items.iter().any(|item| matches!(
                item,
                crate::ast::TopLevel::TypeDef(crate::ast::TypeDef::Sum { name, .. })
                    if name.starts_with("__")
            )),
            "nothing is generated into the answered capability"
        );
    }

    #[test]
    fn unknown_module_still_uses_normal_project_resolution() {
        let resolved =
            resolve_module_source("DefinitelyNotARealModule", ".").expect("resolve unknown module");
        assert!(resolved.is_none());
    }

    #[test]
    fn program_cache_reuses_dependency_source_and_parse_across_entries() {
        let dir = tempfile::tempdir().expect("tempdir");
        let dep_path = dir.path().join("shared.av");
        std::fs::write(
            &dep_path,
            "module Shared\n    intent = \"shared\"\nfn value() -> Int\n    1\n",
        )
        .expect("write dependency");
        let root = dir.path().to_str().expect("utf8 root");
        let first_source = "module First\n    intent = \"first\"\n    depends [Shared]\n";
        let second_source = "module Second\n    intent = \"second\"\n    depends [Shared]\n";
        let first_items = parse_source(first_source).expect("parse first");
        let second_items = parse_source(second_source).expect("parse second");
        let mut cache = ProgramLoadCache::default();

        let first = load_program_with_cache(
            &dir.path().join("first.av"),
            first_source,
            &first_items,
            root,
            LoadMode::Tolerant,
            &mut cache,
        )
        .expect("first program");
        assert_eq!(first.dependencies().len(), 1);

        // A command sees one immutable project snapshot. Changing the file
        // after its first walk must not make a later entry parse it again.
        std::fs::write(&dep_path, "this is no longer Aver").expect("replace dependency");
        let second = load_program_with_cache(
            &dir.path().join("second.av"),
            second_source,
            &second_items,
            root,
            LoadMode::Tolerant,
            &mut cache,
        )
        .expect("second program");
        assert_eq!(second.dependencies().len(), 1);
        assert!(second.dependencies()[0].fault.is_none());
        assert!(second.dependencies()[0].source.contains("fn value"));
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

    #[test]
    fn lowering_memo_holds_only_modules_with_yield_functions() {
        // `yield_cross_module` is Pool (a capability, no `yield`), Looper (a
        // `yield` function), and the entry CrossModule that drives it. The
        // memo the program builds must name only Looper: a program without
        // `yield` anywhere never builds this map at all (see
        // `super::lowering_memo_for`), so this fixture is what actually
        // exercises the "yield modules only" half of that guarantee.
        let root = format!(
            "{}/tests/fixtures/yield_cross_module",
            env!("CARGO_MANIFEST_DIR")
        );
        let main_path = std::path::Path::new(&root).join("main.av");
        let source = std::fs::read_to_string(&main_path).expect("read fixture entry");
        // The entry drives the generated protocol by name, which only a
        // compiler fixture may do, so it is parsed as one.
        let tokens = crate::lexer::Lexer::new(&source)
            .tokenize()
            .expect("lex fixture entry");
        let items = crate::parser::Parser::new_compiler_generated(tokens)
            .parse()
            .expect("parse fixture entry");
        let mut cache = ProgramLoadCache::default();
        let program = load_program_with_cache(
            &main_path,
            &source,
            &items,
            &root,
            LoadMode::Strict,
            &mut cache,
        )
        .expect("load yield_cross_module fixture");
        let memoized = program
            .lowering_memo
            .get_or_init(|| super::lowering_memo_for(program.dependencies(), &program.marked))
            .keys()
            .filter_map(|path| path.file_name())
            .filter_map(|name| name.to_str())
            .collect::<Vec<_>>();
        assert_eq!(memoized, vec!["looper.av"]);
    }

    /// A dependency the program already lowered is handed out settled: it is
    /// not checked or lowered again per unit. That is only sound because
    /// lowering what the memo holds once more changes nothing, so every unit
    /// of every process fixture is held to exactly that, and two loads of
    /// one program share one memo.
    #[test]
    fn settled_dependencies_are_what_lowering_them_again_gives() {
        let fixtures = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures");
        let mut roots = std::fs::read_dir(&fixtures)
            .expect("read fixtures")
            .filter_map(|entry| entry.ok().map(|entry| entry.path()))
            .filter(|path| {
                path.file_name()
                    .and_then(|name| name.to_str())
                    .is_some_and(|name| name.starts_with("run_") || name.starts_with("yield_"))
                    && path.join("main.av").is_file()
            })
            .collect::<Vec<_>>();
        roots.sort();
        let mut settled_seen = 0;
        for root in roots {
            let root_str = root.to_string_lossy().to_string();
            let main_path = root.join("main.av");
            let source = std::fs::read_to_string(&main_path).expect("read fixture entry");
            let Ok(tokens) = crate::lexer::Lexer::new(&source).tokenize() else {
                continue;
            };
            let Ok(items) = crate::parser::Parser::new_compiler_generated(tokens).parse() else {
                continue;
            };
            let load = |cache: &mut ProgramLoadCache| {
                load_program_with_cache(
                    &main_path,
                    &source,
                    &items,
                    &root_str,
                    LoadMode::Tolerant,
                    cache,
                )
            };
            let Ok(program) = load(&mut ProgramLoadCache::default()) else {
                continue;
            };
            for unit in program.report_units() {
                let Ok(loaded) = program.loaded_dependencies_for(unit) else {
                    continue;
                };
                let memo = program
                    .lowering_memo
                    .get()
                    .expect("memo built on first use");
                settled_seen += loaded
                    .iter()
                    .filter(|module| memo.contains_key(&module.path))
                    .count();
                let mut again = loaded.clone();
                let _ = crate::ir::pipeline::lower_loaded_yield_modules(
                    &mut again,
                    None,
                    &program.marked,
                );
                for (settled, lowered) in loaded.iter().zip(&again) {
                    assert!(
                        settled.items == lowered.items,
                        "{}: lowering {} again changed it",
                        root.display(),
                        settled.path.display()
                    );
                }
            }
            // A memo another load handed over is the one this load would
            // have computed itself.
            if program
                .lowering_memo
                .get()
                .is_some_and(|memo| !memo.is_empty())
            {
                let reloaded = load(&mut ProgramLoadCache::default()).expect("load again");
                let shared = super::lowering_memo_for(reloaded.dependencies(), &reloaded.marked);
                let written = reloaded
                    .dependencies()
                    .iter()
                    .map(super::ProgramModule::as_loaded)
                    .collect::<Vec<_>>();
                let mut fresh = written.clone();
                let (_, failed) = crate::ir::pipeline::lower_loaded_process_modules(
                    &mut fresh,
                    None,
                    &reloaded.marked,
                );
                let fresh = fresh
                    .into_iter()
                    .zip(&written)
                    .enumerate()
                    .filter(|(index, (_, written))| {
                        super::lowering_candidate(&written.items) && !failed.contains(index)
                    })
                    .map(|(_, (module, _))| module)
                    .collect::<Vec<_>>();
                assert_eq!(shared.len(), fresh.len(), "{}", root.display());
                for lowered in &fresh {
                    assert!(
                        shared
                            .get(&lowered.path)
                            .is_some_and(|kept| kept.items == lowered.items),
                        "{}: the shared memo differs for {}",
                        root.display(),
                        lowered.path.display()
                    );
                }
            }
        }
        assert!(
            settled_seen > 0,
            "no fixture exercised a settled dependency"
        );
    }
}

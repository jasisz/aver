/// Aver static type checker.
///
/// Two-phase analysis:
///   Phase 1 — build a signature table from all FnDef nodes and builtins.
///   Phase 2 — check top-level statements, then each FnDef for call-site
///              argument types, return type, BinOp compatibility, and effects.
///
/// The checker resolves named generic variables at call sites. Error recovery
/// uses `Type::Invalid`, which matches anything to suppress cascading diagnostics
/// (Iron — A4).
use std::collections::{HashMap, HashSet};

use super::{Type, parse_type_str_strict};
use crate::ast::{
    BinOp, Expr, FnDef, Literal, Module, Pattern, Spanned, Stmt, TailCallData, TopLevel, TypeDef,
};
use crate::ir::{FnId, FnKey, SymbolTable, TypeId, TypeKey};

mod builtins;
mod check;
pub mod effect_classification;
pub mod effect_lifting;
mod exhaustiveness;
mod flow;
pub mod hostile_effects;
pub mod hostile_values;
mod infer;
mod modules;
pub mod oracle_subtypes;
pub mod proof_trust_header;

#[cfg(test)]
mod tests;

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct TypeError {
    pub message: String,
    pub line: usize,
    pub col: usize,
    /// Dependency source when the error does not belong to the entry file.
    pub origin: Option<TypeErrorOrigin>,
    /// Optional secondary span for multi-region diagnostics (e.g. declared type vs actual return).
    pub secondary: Option<TypeErrorSpan>,
}

#[derive(Debug, Clone)]
pub struct TypeErrorOrigin {
    pub file: String,
    /// Exact source when the checker used the filesystem loader. Virtual
    /// callers may provide only the file identity.
    pub source: Option<std::sync::Arc<str>>,
}

#[derive(Debug, Clone)]
pub struct TypeErrorSpan {
    pub line: usize,
    pub col: usize,
    pub label: String,
}

/// Result of type-checking.
#[derive(Debug)]
pub struct TypeCheckResult {
    pub errors: Vec<TypeError>,
    /// For each user-defined fn: (param_types, return_type, effects).
    pub fn_sigs: HashMap<String, (Vec<Type>, Type, Vec<String>)>,
    /// Unused binding warnings: (binding_name, fn_name, line).
    pub unused_bindings: Vec<(String, String, usize)>,
    /// Canonical externally-provided contracts visible to this program.
    pub capabilities: crate::capability::CapabilityRegistry,
}

pub fn run_type_check(items: &[TopLevel]) -> Vec<TypeError> {
    run_type_check_with_base(items, None)
}

pub fn run_type_check_with_base(items: &[TopLevel], base_dir: Option<&str>) -> Vec<TypeError> {
    run_type_check_full(items, base_dir).errors
}

pub fn run_type_check_full(items: &[TopLevel], base_dir: Option<&str>) -> TypeCheckResult {
    let mut checker = TypeChecker::new_with_symbols(build_symbols_for_items(items, base_dir));
    checker.check(items, base_dir);
    finalize_check_result(checker, items)
}

/// Build the `SymbolTable` for a typecheck entry point. Mirrors the
/// dep-resolution path inside [`TypeChecker::check`] so the table
/// covers both entry items and any modules the entry file depends on.
/// Filesystem errors are silently dropped here — the checker rebuilds
/// its own diagnostics during the actual check, so duplicating them at
/// the symbol-table layer would only produce noise.
fn build_symbols_for_items(items: &[TopLevel], base_dir: Option<&str>) -> SymbolTable {
    let mut loaded = base_dir
        .and_then(|base| {
            TypeChecker::module_decl(items).and_then(|m| {
                let mut roots = m.depends.clone();
                roots.extend(crate::stdlib::implicit_stdlib_deps(items));
                roots.sort();
                roots.dedup();
                crate::source::load_module_tree(&roots, base).ok()
            })
        })
        .unwrap_or_default();
    crate::stdlib::append_required_standard_capability_modules(items, &mut loaded);
    let dep_modules = symbols_dep_modules_from_loaded(&loaded);
    SymbolTable::build(items, &dep_modules)
}

/// Pre-loaded variant of [`build_symbols_for_items`] for the
/// `WithLoaded` typecheck driver (playground virtual FS).
fn build_symbols_with_loaded(
    items: &[TopLevel],
    loaded: &[crate::source::LoadedModule],
) -> SymbolTable {
    let mut loaded = loaded.to_vec();
    crate::stdlib::append_required_standard_capability_modules(items, &mut loaded);
    let dep_modules = symbols_dep_modules_from_loaded(&loaded);
    SymbolTable::build(items, &dep_modules)
}

fn symbols_dep_modules_from_loaded(
    loaded: &[crate::source::LoadedModule],
) -> Vec<crate::codegen::ModuleInfo> {
    loaded
        .iter()
        .map(|m| {
            let (capability_items, capability_semantics) =
                crate::codegen::capability_metadata(&m.items);
            let decl = crate::visibility::module_decl(&m.items);
            crate::codegen::ModuleInfo {
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
                // Symbol-table build only — the typechecker never reads
                // verify blocks (they are proof-emit-only fields).
                verify_blocks: Vec::new(),
                verify_laws: Vec::new(),
                analysis: None,
            }
        })
        .collect()
}

/// Variant of [`run_type_check_full`] that uses pre-loaded dependency
/// modules instead of resolving them from disk. The playground feeds
/// this from its in-memory virtual fs so multi-file projects type-
/// check without any filesystem access.
pub fn run_type_check_with_loaded(
    items: &[TopLevel],
    loaded: &[crate::source::LoadedModule],
) -> TypeCheckResult {
    let mut checker = TypeChecker::new_with_symbols(build_symbols_with_loaded(items, loaded));
    checker.check_with_loaded(items, loaded);
    finalize_check_result(checker, items)
}

/// Type-check `items` against dependency modules whose bodies have already
/// passed type checking in a leaves-first command preparation.
///
/// Dependency signatures, capabilities and visibility remain part of this
/// check. Only their bodies are not walked again. This is an internal
/// performance seam for whole-program reporting; ordinary embedders should
/// continue to use [`run_type_check_with_loaded`].
pub(crate) fn run_type_check_with_checked_loaded(
    items: &[TopLevel],
    loaded: &[crate::source::LoadedModule],
) -> TypeCheckResult {
    let mut checker = TypeChecker::new_with_symbols(build_symbols_with_loaded(items, loaded));
    checker.check_with_checked_loaded(items, loaded);
    finalize_check_result(checker, items)
}

/// Self-host variant of [`run_type_check_full`]: bypasses the
/// opaque-type checks (construction, field access, pattern match).
/// Used exclusively by `aver compile --with-self-host-support` so
/// `self_hosted/domain/builtins.av` can round-trip opaque host
/// types (e.g. `Tcp.Connection`) through the replay JSON contract.
/// User code outside the self-host always goes through the regular
/// [`run_type_check_full`] and stays bound by the opaque rules.
pub fn run_type_check_full_self_host(
    items: &[TopLevel],
    base_dir: Option<&str>,
) -> TypeCheckResult {
    let mut checker = TypeChecker::new_with_symbols(build_symbols_for_items(items, base_dir));
    checker.self_host_mode = true;
    checker.check(items, base_dir);
    finalize_check_result(checker, items)
}

/// Self-host variant of [`run_type_check_with_loaded`]. See
/// [`run_type_check_full_self_host`] for the opaque-bypass rationale.
pub fn run_type_check_with_loaded_self_host(
    items: &[TopLevel],
    loaded: &[crate::source::LoadedModule],
) -> TypeCheckResult {
    let mut checker = TypeChecker::new_with_symbols(build_symbols_with_loaded(items, loaded));
    checker.self_host_mode = true;
    checker.check_with_loaded(items, loaded);
    finalize_check_result(checker, items)
}

fn finalize_check_result(mut checker: TypeChecker, items: &[TopLevel]) -> TypeCheckResult {
    // Phase B (post peer-review #148): flatten the internal split
    // (`fn_sigs` keyed by `FnId`, `extra_sigs` keyed by canonical
    // string) into the exported `HashMap<String, _>` external
    // consumers expect. The old bare-alias mirror was last-write-wins
    // across modules: when two distinct dep modules each exposed `foo`,
    // one would silently win the global `"foo"` slot and Rust codegen's
    // `ctx.fn_sigs.get(&fd.name)` could pick up the wrong signature.
    //
    // Now bare aliases land only when unambiguous. Each canonical key
    // is always present; bare-name keys land only when no other fn in
    // the program shares that bare name. Consumers that previously
    // relied on a non-deterministic global "foo" entry will see a miss
    // and surface a clear "qualify the reference" error instead of
    // mismatched parameters.
    let entry_prefix = checker.current_module_prefix.clone();
    let mut fn_sigs: HashMap<String, (Vec<Type>, Type, Vec<String>)> = HashMap::new();

    // Iterate in `FnId` order for deterministic output (HashMap
    // iteration order would otherwise leak non-determinism into the
    // exported map and downstream diagnostics).
    let mut ordered_user: Vec<(FnId, &FnSig)> =
        checker.fn_sigs.iter().map(|(id, sig)| (*id, sig)).collect();
    ordered_user.sort_by_key(|(id, _)| id.0);

    // Tally bare-name owners. Phase B (peer review round 2): an
    // entry-scope fn shadows any dep-module fn sharing the same bare
    // name — source-level `doit()` inside `Main` unambiguously means
    // `Main.doit`, even when a dep also exposes `doit`. We therefore
    // suppress the bare alias only for dep-dep ambiguity (multiple
    // dep modules share a bare name *and* the entry doesn't define
    // one). When the entry owns the bare name, the bare alias points
    // at the entry FnId; dep fns stay reachable only by qualified
    // name.
    let mut bare_entry_owner: HashMap<String, FnId> = HashMap::new();
    let mut bare_dep_owners: HashMap<String, FnId> = HashMap::new();
    let mut bare_dep_ambiguous: HashSet<String> = HashSet::new();
    for (id, _) in &ordered_user {
        let entry = checker.symbol_table.fn_entry(*id);
        let bare = entry.key.name.as_str();
        if entry.module.is_entry() {
            bare_entry_owner.insert(bare.to_string(), *id);
            continue;
        }
        match bare_dep_owners.get(bare) {
            None => {
                bare_dep_owners.insert(bare.to_string(), *id);
            }
            Some(prior) if prior == id => {}
            Some(_) => {
                bare_dep_ambiguous.insert(bare.to_string());
            }
        }
    }

    for (id, sig) in &ordered_user {
        let entry = checker.symbol_table.fn_entry(*id);
        let canonical = if entry.module.is_entry() {
            match entry_prefix.as_deref() {
                Some(prefix) => crate::visibility::qualified_name(prefix, &entry.key.name),
                None => entry.key.name.clone(),
            }
        } else {
            entry.key.canonical()
        };
        let triple = (sig.params.clone(), sig.ret.clone(), sig.effects.clone());
        fn_sigs.insert(canonical.clone(), triple.clone());
        // Bare alias rules:
        //  - entry-scope owner always wins (shadows any dep with the
        //    same bare name);
        //  - dep-scope fn gets the bare alias only when (a) the entry
        //    doesn't own it and (b) no other dep module conflicts.
        if entry.key.name == canonical {
            continue;
        }
        let is_entry_owner = bare_entry_owner.get(&entry.key.name) == Some(id);
        let mut emit_bare = false;
        if entry.module.is_entry() {
            emit_bare = is_entry_owner;
        } else if !bare_entry_owner.contains_key(&entry.key.name)
            && !bare_dep_ambiguous.contains(&entry.key.name)
        {
            emit_bare = true;
        }
        if emit_bare {
            fn_sigs.entry(entry.key.name.clone()).or_insert(triple);
        }
    }
    for (k, sig) in &checker.extra_sigs {
        fn_sigs
            .entry(k.clone())
            .or_insert_with(|| (sig.params.clone(), sig.ret.clone(), sig.effects.clone()));
    }

    check_capability_effect_shorthand(items, &checker.capabilities, &mut checker.errors);
    check_forwarded_effect_marker(items, &mut checker.errors);
    check_module_effect_boundary(items, &mut checker.errors);

    TypeCheckResult {
        errors: checker.errors,
        fn_sigs,
        unused_bindings: checker.unused_warnings,
        capabilities: checker.capabilities,
    }
}

fn check_forwarded_effect_marker(items: &[TopLevel], errors: &mut Vec<TypeError>) {
    let mut reject = |line| {
        errors.push(TypeError {
            message: "Effect '_' is allowed only as the sole effect of a direct callback parameter type (`Fn(...) -> ... ! [_]`); it forwards that named callback's concrete effects at the call site and is not an ambient effect".to_string(),
            line,
            col: 1,
            origin: None,
            secondary: None,
        });
    };
    for item in items {
        match item {
            TopLevel::Module(module) => {
                if let Some(effects) = &module.effects
                    && effects
                        .iter()
                        .any(|effect| effect == crate::effects::FORWARDED_CALLBACK_EFFECT)
                {
                    reject(module.effects_line.unwrap_or(module.line));
                }
            }
            TopLevel::FnDef(fd)
                if fd
                    .effects
                    .iter()
                    .any(|effect| effect.node == crate::effects::FORWARDED_CALLBACK_EFFECT) =>
            {
                reject(
                    fd.effects
                        .iter()
                        .find(|effect| effect.node == crate::effects::FORWARDED_CALLBACK_EFFECT)
                        .map_or(fd.line, |effect| effect.line),
                );
            }
            _ => {}
        }
    }
}

fn check_capability_effect_shorthand(
    items: &[TopLevel],
    capabilities: &crate::capability::CapabilityRegistry,
    errors: &mut Vec<TypeError>,
) {
    let capability_modules: HashSet<&str> = capabilities
        .contracts()
        .map(|contract| contract.module.as_str())
        .collect();
    let mut reject = |effect: &str, line: usize| {
        let is_standard = crate::stdlib::is_standard_capability(effect);
        if capability_modules.contains(effect) && !is_standard {
            errors.push(TypeError {
                message: format!(
                    "Capability effect shorthand '{}' is not allowed; name the exact operation so adding a provider atom cannot silently widen this boundary",
                    effect
                ),
                line,
                col: 1,
                origin: None,
                secondary: None,
            });
        }
    };
    for item in items {
        match item {
            TopLevel::Module(module) => {
                if let Some(effects) = &module.effects {
                    for effect in effects {
                        reject(effect, module.effects_line.unwrap_or(module.line));
                    }
                }
            }
            TopLevel::FnDef(fd) => {
                for effect in &fd.effects {
                    reject(&effect.node, effect.line);
                }
            }
            _ => {}
        }
    }
}

/// Enforce module-level `effects [...]` declaration against per-fn effect
/// usage. The rule:
///
/// - Module without `effects [...]` → legacy/mixed, no enforcement (0.13
///   migration shim; 0.14+ may upgrade to soft warning).
/// - Module with `effects [...]` (including `effects []` for explicit pure)
///   → every function's `! [...]` must be covered by the module's declared
///   surface. A namespace-level entry like `Disk` admits any `Disk.*`
///   method; a method-level entry like `Time.now` admits only that one.
fn check_module_effect_boundary(items: &[TopLevel], errors: &mut Vec<TypeError>) {
    let Some(allowed) = items.iter().find_map(|i| match i {
        TopLevel::Module(m) => m.effects.as_ref().map(|e| (e, m)),
        _ => None,
    }) else {
        return;
    };
    let (allowed_list, module) = allowed;

    let allowed_namespaces: HashSet<&str> = allowed_list
        .iter()
        .filter(|e| !e.contains('.'))
        .map(|e| e.as_str())
        .collect();
    let allowed_methods: HashSet<&str> = allowed_list.iter().map(|e| e.as_str()).collect();

    for item in items {
        let TopLevel::FnDef(fd) = item else { continue };
        for eff in &fd.effects {
            let method = eff.node.as_str();
            if allowed_methods.contains(method) {
                continue;
            }
            if let Some((ns, _)) = method.split_once('.')
                && allowed_namespaces.contains(ns)
            {
                continue;
            }
            errors.push(TypeError {
                message: format!(
                    "module '{}' declared `effects [{}]` but '{}' uses '{}' which is not in the declared boundary",
                    module.name,
                    allowed_list.join(", "),
                    fd.name,
                    method
                ),
                line: eff.line,
                col: 1,
                origin: None,
                secondary: module.effects_line.map(|l| TypeErrorSpan {
                    line: l,
                    col: 1,
                    label: "module effects declared here".to_string(),
                }),
            });
        }
    }
}

// ---------------------------------------------------------------------------
// Internal structures
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
struct FnSig {
    params: Vec<Type>,
    ret: Type,
    effects: Vec<String>,
}

/// Iron — A5: typed key for `record_field_types`. Pre-A5 the map
/// was keyed by `"TypeName.fieldName"` stringifications, which
/// forced every reader to `strip_prefix(format!("{type}."))` and
/// then re-check that the remainder didn't itself contain a dot
/// (because the post-A3 dual-keying mirrored each entry under both
/// the canonical `"Module.Type.field"` form and the bare alias
/// `"Type.field"` — and the canonical form spuriously matched the
/// `"Module."` prefix-strip when the read came from a module
/// looking up its own fields). The struct key separates the two
/// dimensions, so the canonical resolution happens once at
/// insert/lookup (via `sig_aliases`) and iteration filters on
/// `key.type_name == canonical` with no string-shape gymnastics.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct RecordFieldKey {
    pub(crate) type_name: String,
    pub(crate) field_name: String,
}

impl RecordFieldKey {
    pub(crate) fn new(type_name: impl Into<String>, field_name: impl Into<String>) -> Self {
        Self {
            type_name: type_name.into(),
            field_name: field_name.into(),
        }
    }
}

/// Bare-name resolution result. Tracks ambiguity explicitly so
/// `resolve_fn_id` / `resolve_type_id` can refuse the look-up when
/// two distinct identities surface the same source name. The
/// `Ambiguous` variant carries the actual candidate IDs (only those
/// that made it through visibility, since that's the population
/// path) so diagnostics can suggest exactly the names the user can
/// actually pick from — never a private dep type that happens to
/// share the bare name.
#[derive(Debug, Clone)]
enum Resolution<T> {
    Single(T),
    Ambiguous(Vec<T>),
}

impl<T: Copy + PartialEq> Resolution<T> {
    /// Merge another candidate identity into an alias entry. Two
    /// distinct identities for the same bare name produce
    /// `Ambiguous`; a duplicate registration of the same identity is
    /// a no-op (same module re-traversed by another path).
    fn merge(&mut self, candidate: T) {
        match self {
            Resolution::Single(existing) if *existing == candidate => {}
            Resolution::Single(existing) => {
                let prior = *existing;
                *self = Resolution::Ambiguous(vec![prior, candidate]);
            }
            Resolution::Ambiguous(seen) => {
                if !seen.contains(&candidate) {
                    seen.push(candidate);
                }
            }
        }
    }

    fn unambiguous(&self) -> Option<T> {
        match self {
            Resolution::Single(v) => Some(*v),
            Resolution::Ambiguous(_) => None,
        }
    }
}

struct TypeChecker {
    /// Resolved-identity table — phase B (#138). Populated by
    /// [`TypeChecker::new_with_symbols`] from the program's `entry_items
    /// + dep_modules` before any signature registration. Carries opaque
    /// `FnId` / `TypeId` for every user-defined fn and type. The
    /// checker resolves bare-name references through it instead of the
    /// pre-phase-B `sig_aliases` string→string map.
    symbol_table: SymbolTable,
    /// User-defined function signatures, keyed by the opaque `FnId`
    /// from `symbol_table`. Phase B (#138) migrated this away from
    /// `HashMap<String, FnSig>` so that two modules each declaring `foo`
    /// can't silently collide through bare-name keying.
    ///
    /// Built-in fn signatures (namespace methods like `Int.add`,
    /// `Console.print`) and user constructors (variants of sum types,
    /// product type constructors keyed by `"Module.Type.Variant"`)
    /// don't have `FnId`s in the program symbol table — those live in
    /// `extra_sigs` keyed by canonical string. `find_fn_sig` chains
    /// the two for a unified bare-name → signature lookup.
    fn_sigs: HashMap<FnId, FnSig>,
    /// Builtin fn signatures + constructor signatures keyed by
    /// canonical string. The non-`FnId` half of the split — entries
    /// here either come from `register_builtins` (namespace methods)
    /// or `register_type_def_sigs` (sum-type variant constructors,
    /// product type "callable name" entries). Lookups against this
    /// map use the canonical key directly; bare→canonical resolution
    /// goes through `symbol_table.type_id_of` for type-derived keys.
    extra_sigs: HashMap<String, FnSig>,
    /// Bare → `TypeId` aliases for the type-name dimension; consumed by
    /// ambiguity diagnostics and canonical display. Functions have no
    /// equivalent: cross-module calls are always `Module.fn`, and own bare
    /// calls resolve directly through `SymbolTable`.
    bare_type_aliases: HashMap<String, Resolution<TypeId>>,
    /// `FnId`s the current checker may legitimately resolve to.
    /// Populated from `build_signatures` (the current module's own
    /// fns — always visible from within the module) and from
    /// `integrate_registry` (visibility-exposed entries from each
    /// dep module — already filtered against the `exposes` contract
    /// by `crate::visibility::SymbolRegistry::from_modules`).
    ///
    /// `resolve_fn_id` consults this set so a qualified
    /// `C.helper()` reference fails when `helper` isn't exposed even
    /// though the symbol table — which carries every fn from every
    /// dep module regardless of visibility — has its `FnId`.
    visible_fn_ids: HashSet<FnId>,
    /// `TypeId`s the current checker may legitimately resolve to.
    /// Same role as `visible_fn_ids` for the type-name dimension —
    /// closes the qualified-private-import leak peer review round 4
    /// flagged on `fn takes(s: C.Shape)` references against a `C`
    /// whose `exposes` list didn't include `Shape`.
    visible_type_ids: HashSet<TypeId>,
    /// Direct dependency modules visible to this checker. Kept separately
    /// from `visible_type_ids` because an explicit type re-export such as
    /// `High.Item` resolves to the original `Low.Item` identity and therefore
    /// has no `TypeKey::in_module("High", "Item")` of its own.
    visible_module_names: HashSet<String>,
    /// Public type surface for every loaded module, including explicit
    /// re-exports resolved to their original declaration identity. The full
    /// map is resolver context; only entries whose module is in
    /// `visible_module_names` are visible to ordinary importer lookups.
    module_type_exports: crate::visibility::ModuleTypeExports,
    value_members: HashMap<String, Type>,
    /// Field types for record types, keyed by `(type_name, field_name)`.
    /// Populated for both user-defined `record` types and built-in records
    /// (Http.Response, Trace, …). Single entry per (canonical type name, field);
    /// lookup canonicalises `type_name` through `SymbolTable` at read time.
    /// Enables checked dot-access on Named types.
    record_field_types: HashMap<RecordFieldKey, Type>,
    /// Variant names for sum types: "Shape" → ["Circle", "Rect", "Point"].
    /// Pre-populated for Result and Option; extended by user-defined sum types.
    type_variants: HashMap<String, Vec<String>>,
    /// Module prefix of the items currently being checked. `None`
    /// while checking entry-scope items. Per-module sub-checkers
    /// (`check_loaded_module_bodies`) set this to the dep module's
    /// prefix so bare-name resolution finds the local type/fn first.
    current_module_prefix: Option<String>,
    /// Top-level bindings visible from function bodies.
    globals: HashMap<String, Type>,
    /// Local bindings in the current function/scope.
    locals: HashMap<String, Type>,
    errors: Vec<TypeError>,
    /// Return type of the function currently being checked; None at top level.
    current_fn_ret: Option<Type>,
    /// Line number of the function currently being checked; None at top level.
    current_fn_line: Option<usize>,
    /// Type names that are opaque in this module's context (imported via `exposes opaque`).
    opaque_types: HashSet<String>,
    /// Every named type the compiler itself declares — the host
    /// records effect signatures hand back (`Http.Response`,
    /// `Terminal.Size`, …), the Oracle nominals
    /// (`Trace`, `EffectEvent`, `BranchPath`) and the embedded
    /// standard library's refinements (`Bytes`, `Digest32`).
    ///
    /// Derived, never hand-listed: [`Self::record_builtin_type_names`]
    /// harvests it from the signatures `register_builtins` just
    /// registered, so a name stops being writable the moment the
    /// builtin that mentions it goes away, and a new builtin type is
    /// admitted without touching this file. A user may legitimately
    /// write any of these in an annotation even though none of them
    /// has a `type` declaration to resolve against, so
    /// [`Self::named_type_is_declared`] treats them as declared.
    builtin_type_names: HashSet<String>,
    /// When `true`, opaque-type construction + field-access + pattern-match
    /// checks are bypassed. Used only by the self-host compile path
    /// (`aver compile --with-self-host-support`) where
    /// `self_hosted/domain/builtins.av` still round-trips its compatibility
    /// carrier for opaque host types (e.g. `Tcp.Connection`) through the
    /// replay `Val` representation:
    /// it serialises by reading `.id` / `.host` / `.port`, and
    /// reconstructs by `Tcp.Connection(id = …, host = …, port = …)` on
    /// replay deserialise. Both operations are forbidden in user code by
    /// design (Phase 4.7+ fix #11), but the self-host has to read +
    /// write the underlying record shape because that's the contract
    /// with the replay JSON format. The flag is set by
    /// [`run_type_check_full_self_host`] / [`run_type_check_with_loaded_self_host`]
    /// and never user-toggleable from source.
    self_host_mode: bool,
    /// Names referenced during type checking of current function body (for unused detection).
    used_names: HashSet<String>,
    /// Bindings defined in the current function body: (name, line).
    fn_bindings: Vec<(String, usize)>,
    /// Unused binding warnings collected during checking: (binding_name, fn_name, line).
    unused_warnings: Vec<(String, String, usize)>,
    /// Oracle v1: `.result` / `.trace` / `.trace.*` projections are
    /// only meaningful inside `verify <fn> trace` cases. This flag is
    /// set true while checking such a case's LHS / RHS, false
    /// otherwise. Outside verify-trace the projections are rejected at
    /// check time — otherwise user code would type-check then crash
    /// at runtime with "namespace has no member 'trace'".
    in_verify_trace_context: bool,
    /// Program-defined capability contracts, built from the entry and
    /// dependency closure before signatures or verify flow are checked.
    capabilities: crate::capability::CapabilityRegistry,
}

impl TypeChecker {
    fn new_with_symbols(symbol_table: SymbolTable) -> Self {
        let mut type_variants = HashMap::new();
        type_variants.insert(
            "Result".to_string(),
            vec!["Ok".to_string(), "Err".to_string()],
        );
        type_variants.insert(
            "Option".to_string(),
            vec!["Some".to_string(), "None".to_string()],
        );

        let mut tc = TypeChecker {
            symbol_table,
            fn_sigs: HashMap::new(),
            extra_sigs: HashMap::new(),
            bare_type_aliases: HashMap::new(),
            visible_fn_ids: HashSet::new(),
            visible_type_ids: HashSet::new(),
            visible_module_names: HashSet::new(),
            module_type_exports: crate::visibility::ModuleTypeExports::new(),
            value_members: HashMap::new(),
            record_field_types: HashMap::new(),
            type_variants,
            current_module_prefix: None,
            globals: HashMap::new(),
            locals: HashMap::new(),
            errors: Vec::new(),
            current_fn_ret: None,
            current_fn_line: None,
            opaque_types: HashSet::new(),
            builtin_type_names: HashSet::new(),
            self_host_mode: false,
            used_names: HashSet::new(),
            fn_bindings: Vec::new(),
            unused_warnings: Vec::new(),
            in_verify_trace_context: false,
            capabilities: crate::capability::CapabilityRegistry::default(),
        };
        tc.register_builtins();
        tc.record_builtin_type_names();
        tc
    }

    /// Snapshot every named type the just-registered builtin
    /// signatures mention, before a single user declaration lands in
    /// the same maps. Reads four sources: the record schemas
    /// (`Http.Response`, `Trace`, `Terminal.Size`, …) by key and by
    /// field type, the opaque host handles, the namespace-method
    /// signatures (which is where `BranchPath`, `Bytes` and
    /// `Digest32` surface), and the nullary value members.
    ///
    /// Must stay the last step of construction: `record_field_types`
    /// and `extra_sigs` both go on to hold user declarations, and
    /// those must not be mistaken for compiler-declared names.
    fn record_builtin_type_names(&mut self) {
        let mut names: HashSet<String> = HashSet::new();
        for key in self.record_field_types.keys() {
            names.insert(key.type_name.clone());
        }
        let collect = |ty: &Type, into: &mut HashSet<String>| {
            let mut stack = vec![ty.clone()];
            while let Some(t) = stack.pop() {
                match t {
                    Type::Named { name, .. } => {
                        into.insert(name);
                    }
                    Type::Option(inner) | Type::List(inner) | Type::Vector(inner) => {
                        stack.push(*inner)
                    }
                    Type::Result(a, b) | Type::Map(a, b) => {
                        stack.push(*a);
                        stack.push(*b);
                    }
                    Type::Tuple(items) => stack.extend(items),
                    Type::Fn(params, ret, _) => {
                        stack.extend(params);
                        stack.push(*ret);
                    }
                    _ => {}
                }
            }
        };
        for ty in self.record_field_types.values() {
            collect(ty, &mut names);
        }
        for ty in self.value_members.values() {
            collect(ty, &mut names);
        }
        for sig in self.extra_sigs.values() {
            for p in &sig.params {
                collect(p, &mut names);
            }
            collect(&sig.ret, &mut names);
        }
        names.extend(self.opaque_types.iter().cloned());
        self.builtin_type_names = names;
    }

    // -- Identity resolution (phase B) -------------------------------------

    /// Resolve a source-faithful function reference (`"foo"`,
    /// `"Module.foo"`, `"Tcp.send"`) through the symbol table's shared
    /// module visibility relation.
    ///
    /// Misses for builtin namespace methods and constructors — those
    /// don't live in the program symbol table; callers fall back to
    /// the `extra_sigs` string-keyed half.
    pub(crate) fn resolve_fn_id(&self, name: &str) -> Option<FnId> {
        self.symbol_table
            .resolve_fn_id_in(name, self.current_module_prefix.as_deref())
            .filter(|id| self.visible_fn_ids.contains(id))
    }

    /// `true` when the bare alias map has recorded multiple distinct
    /// `TypeId`s for `name` (cross-module same-bare-name import).
    /// Distinct from "name doesn't resolve at all" — used by the
    /// matcher to decide whether a mixed (Some, None) typed/raw
    /// comparison should fall back to name equality (only when the
    /// `None` side is genuinely a builtin / external name, not when
    /// it's ambiguous bare reference whose typed identity we
    /// deliberately suppressed).
    pub(crate) fn type_name_is_ambiguous(&self, name: &str) -> bool {
        matches!(
            self.bare_type_aliases.get(name),
            Some(Resolution::Ambiguous(_))
        )
    }

    /// List the canonical names of every type that the bare alias map
    /// recorded as a candidate for `bare`. The `Resolution::Ambiguous`
    /// variant carries the actual conflicting `TypeId`s populated
    /// through visibility-exposed aliases — never scans the full
    /// `symbol_table.types`, so a private (non-exposed) dep type that
    /// happens to share a bare name never appears in the diagnostic.
    pub(crate) fn ambiguous_type_candidates(&self, bare: &str) -> Vec<String> {
        let Some(Resolution::Ambiguous(ids)) = self.bare_type_aliases.get(bare) else {
            return Vec::new();
        };
        let mut out: Vec<String> = ids
            .iter()
            .map(|id| self.symbol_table.type_entry(*id).key.canonical())
            .collect();
        out.sort();
        out
    }

    /// `true` when something in scope actually declares `name`.
    ///
    /// A capitalised identifier in type position parses into
    /// `Type::Named` whatever it says, so this is the only question
    /// that separates a real type from a typo. Four things count as a
    /// declaration:
    ///
    ///   - a `type` / `record` reachable through the symbol table —
    ///     the current module's own, or one a `depends` module
    ///     exposes;
    ///   - a name the compiler itself declares
    ///     ([`Self::builtin_type_names`]);
    ///   - an opaque type, which has no structure at the use site but
    ///     is nameable in a signature;
    ///   - a name the checker registered a record schema or a variant
    ///     list for, which covers `Result` / `Option` written without
    ///     their parameters and any type whose identity landed in the
    ///     local maps without a symbol-table entry.
    ///
    /// Anything else names nothing, and every position that accepts a
    /// type annotation reports it through
    /// [`Self::report_ambiguous_named`].
    pub(crate) fn named_type_is_declared(&self, name: &str) -> bool {
        self.resolve_type_id(name).is_some()
            || self.builtin_type_names.contains(name)
            || self.opaque_types.contains(name)
            || self.has_record_schema(name)
            || self.has_variants_for(name)
    }

    /// Walk `ty` and emit diagnostics for every distinct unresolved
    /// reason the typechecker deliberately blocked resolution.
    /// Called from the signature-registration boundary
    /// (`build_signatures`, `register_type_def_sigs`, flow's binding
    /// annotations, verify-law `given` binders) so the user gets a
    /// clean explanation instead of downstream `expected X, got X`
    /// cascades. Three reasons are surfaced:
    ///
    ///   - Ambiguous bare reference: `Foo` matches multiple
    ///     visibility-exposed `TypeId`s. Diagnostic suggests the
    ///     qualified forms.
    ///   - Private qualified import: `Module.Foo` exists in the
    ///     symbol table but isn't on `Module`'s `exposes` list.
    ///     Diagnostic names the dep + asks for the export.
    ///   - Undeclared name: nothing anywhere declares `Foo`. Before
    ///     this check a phantom type propagated silently all the way
    ///     into the proof export, and only the wasm-gc backend ever
    ///     refused it (#859).
    pub(super) fn report_ambiguous_named(&mut self, ty: &Type, line: usize, source_ctx: &str) {
        let mut seen_ambig: HashSet<String> = HashSet::new();
        let mut seen_private: HashSet<String> = HashSet::new();
        let mut seen_unknown: HashSet<String> = HashSet::new();
        self.collect_unresolved_into(ty, &mut seen_ambig, &mut seen_private, &mut seen_unknown);
        let mut unknown: Vec<String> = seen_unknown.into_iter().collect();
        unknown.sort();
        for name in unknown {
            self.error_at_line(
                line,
                format!("{source_ctx}: Unknown type '{name}' — no type of that name is declared in this module or exposed by one it depends on"),
            );
        }
        for name in seen_ambig {
            let candidates = self.ambiguous_type_candidates(&name);
            if candidates.is_empty() {
                continue;
            }
            let suggestion = match candidates.as_slice() {
                [a] => a.clone(),
                [a, b] => format!("`{}` or `{}`", a, b),
                more => {
                    let last = more.last().expect("non-empty");
                    let head = &more[..more.len() - 1];
                    let joined = head
                        .iter()
                        .map(|c| format!("`{}`", c))
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("{}, or `{}`", joined, last)
                }
            };
            self.error_at_line(
                line,
                format!(
                    "{source_ctx}: Ambiguous type name '{name}'; use {suggestion} to disambiguate"
                ),
            );
        }
        for qualified in seen_private {
            let (module, type_name) = qualified
                .rsplit_once('.')
                .map(|(m, t)| (m.to_string(), t.to_string()))
                .expect("private qualified name always has a `.`");
            self.error_at_line(
                line,
                format!(
                    "{source_ctx}: Type '{qualified}' is not exposed by module '{module}' — add '{type_name}' to its `exposes` list to import it",
                ),
            );
        }
    }

    fn collect_unresolved_into(
        &self,
        ty: &Type,
        ambig: &mut HashSet<String>,
        private: &mut HashSet<String>,
        unknown: &mut HashSet<String>,
    ) {
        match ty {
            Type::Named { id: None, name } => {
                if self.type_name_is_ambiguous(name) {
                    ambig.insert(name.clone());
                } else if self.type_name_is_private_import(name) {
                    private.insert(name.clone());
                } else if !self.named_type_is_declared(name) {
                    unknown.insert(name.clone());
                }
            }
            Type::Named { .. }
            | Type::Int
            | Type::Float
            | Type::Str
            | Type::Bool
            | Type::Unit
            | Type::Var(_)
            | Type::Invalid => {}
            Type::Option(inner) | Type::List(inner) | Type::Vector(inner) => {
                self.collect_unresolved_into(inner, ambig, private, unknown);
            }
            Type::Result(ok, err) => {
                self.collect_unresolved_into(ok, ambig, private, unknown);
                self.collect_unresolved_into(err, ambig, private, unknown);
            }
            Type::Map(k, v) => {
                self.collect_unresolved_into(k, ambig, private, unknown);
                self.collect_unresolved_into(v, ambig, private, unknown);
            }
            Type::Tuple(items) => {
                for item in items {
                    self.collect_unresolved_into(item, ambig, private, unknown);
                }
            }
            Type::Fn(params, ret, _) => {
                for p in params {
                    self.collect_unresolved_into(p, ambig, private, unknown);
                }
                self.collect_unresolved_into(ret, ambig, private, unknown);
            }
        }
    }

    /// Narrowing: a function TYPE (`Fn(...)`) may appear only as a *direct
    /// function parameter type*. Reject it in every other declared-type
    /// position — return types, record / sum-variant fields, collection /
    /// map-value / tuple elements, binding annotations, and nested inside
    /// another `Fn`. Aver functions are first-class values only in
    /// call-argument position (`HttpServer.listen(port, handler)`); letting a
    /// function value be returned, stored, or otherwise escape would make the
    /// concrete callee — and therefore its effects — runtime-determined,
    /// which is exactly what the static effect / Oracle / verify guarantees
    /// rely on NOT happening.
    ///
    /// `allow_top_level_param` is true only at the parameter site: a parameter
    /// may itself be a `Fn(...)` callback, but a `Fn` nested inside that
    /// callback's own params/return is still rejected. Emits at most one error
    /// per offending position (stops descending past a rejected `Fn`), so a
    /// `-> Fn(A) -> Fn(B) -> C` return yields one diagnostic, not a cascade.
    pub(super) fn reject_fn_in_type(
        &mut self,
        ty: &Type,
        allow_top_level_param: bool,
        line: usize,
        source_ctx: &str,
    ) {
        match ty {
            Type::Fn(params, ret, effects) => {
                if !allow_top_level_param {
                    self.error_at_line(
                        line,
                        format!(
                            "{source_ctx}: a function type `{}` is not allowed here. Aver permits `Fn(...)` only as a direct function parameter type \
                             (e.g. `fn run(step: Fn(Int) -> Int) -> Int`); functions are first-class values only in call-argument position \
                             (`HttpServer.listen(port, handler)`). Return a concrete value and call the function at its use site.",
                            ty.display()
                        ),
                    );
                    return;
                }
                if effects
                    .iter()
                    .any(|effect| effect == crate::effects::FORWARDED_CALLBACK_EFFECT)
                    && !crate::effects::forwards_callback_effects(effects)
                {
                    self.error_at_line(
                        line,
                        format!(
                            "{source_ctx}: callback effect '_' must be the sole effect (`Fn(...) -> ... ! [_]`); it stands for the named callback's complete concrete effect set"
                        ),
                    );
                }
                // A callback parameter may not itself take or return a fn.
                for p in params {
                    self.reject_fn_in_type(p, false, line, source_ctx);
                }
                self.reject_fn_in_type(ret, false, line, source_ctx);
            }
            Type::Option(inner) | Type::List(inner) | Type::Vector(inner) => {
                self.reject_fn_in_type(inner, false, line, source_ctx);
            }
            Type::Result(ok, err) => {
                self.reject_fn_in_type(ok, false, line, source_ctx);
                self.reject_fn_in_type(err, false, line, source_ctx);
            }
            Type::Map(k, v) => {
                if self.type_contains_capability_resource(k) {
                    self.error_at_line(
                        line,
                        format!(
                            "{source_ctx}: capability resource type `{}` cannot be used as a Map key; provider token identity has no equality or hash semantics",
                            k.display()
                        ),
                    );
                } else {
                    self.require_ordered_map_key(k, line, Some(source_ctx));
                }
                self.reject_fn_in_type(k, false, line, source_ctx);
                self.reject_fn_in_type(v, false, line, source_ctx);
            }
            Type::Tuple(items) => {
                for item in items {
                    self.reject_fn_in_type(item, false, line, source_ctx);
                }
            }
            Type::Named { .. }
            | Type::Int
            | Type::Float
            | Type::Str
            | Type::Bool
            | Type::Unit
            | Type::Var(_)
            | Type::Invalid => {}
        }
    }

    /// `true` if `ty` is a `Fn(...)` or structurally contains one (in a
    /// collection element, tuple slot, etc.). Used to reject binding a
    /// function value in any shape (`g = double`, `gs = [double, inc]`).
    pub(super) fn type_contains_fn(&self, ty: &Type) -> bool {
        match ty {
            Type::Fn(..) => true,
            Type::Option(inner) | Type::List(inner) | Type::Vector(inner) => {
                self.type_contains_fn(inner)
            }
            Type::Result(ok, err) => self.type_contains_fn(ok) || self.type_contains_fn(err),
            Type::Map(k, v) => self.type_contains_fn(k) || self.type_contains_fn(v),
            Type::Tuple(items) => items.iter().any(|i| self.type_contains_fn(i)),
            Type::Named { .. }
            | Type::Int
            | Type::Float
            | Type::Str
            | Type::Bool
            | Type::Unit
            | Type::Var(_)
            | Type::Invalid => false,
        }
    }

    /// Whether observing structural equality/hash of `ty` would expose a
    /// provider-minted capability token. The registry precomputes represented
    /// ADTs that transitively contain a resource, while this walk covers
    /// generic containers assembled at the use site.
    /// Refuse a map key that reaches a `Float`.
    ///
    /// `source_ctx` prefixes the sentence where the door knows whose
    /// annotation it is; call sites that only hold an expression pass `None`.
    /// The same key is often decided at two doors at once and the refusal
    /// reads the same at each, so the sentence is reported once.
    pub(super) fn require_ordered_map_key(
        &mut self,
        key: &Type,
        line: usize,
        source_ctx: Option<&str>,
    ) {
        let Some((what, via)) = self.map_key_unorderable_part(key) else {
            return;
        };
        let why = match what.as_str() {
            "Float" => {
                "a NaN has no place in the finite range, and neither the compiled binary nor the proof model can state an order the other agrees with"
            }
            "Map" => {
                "a map has no order of its own — its own entries are already ordered by ITS key"
            }
            _ => "a vector has no order of its own",
        };
        let tail = match via {
            None => format!(
                "a map iterates its entries sorted by key, so the key type has to order — and `{what}` does not: {why}. Key on a value that orders: Int, String, Bool, or a record, variant, list or tuple built out of them"
            ),
            Some(path) => format!(
                "a map iterates its entries sorted by key, so the key type has to order — and this one reaches a `{what}` through `{path}`, which does not: {why}. Key on a value that orders"
            ),
        };
        let msg = match source_ctx {
            Some(ctx) => format!("{ctx}: {tail}"),
            None => tail,
        };
        if self.errors.iter().any(|e| e.message == msg) {
            return;
        }
        self.error_at_line(line, msg);
    }

    /// Refuse every unordered map key anywhere inside `ty`.
    ///
    /// The annotation walk reaches each `Map` on its own way down, so it calls
    /// [`Self::require_ordered_map_key`] directly. Doors that hold a whole
    /// type and nothing that walks it — a `verify` given, a signature
    /// registered from a dependency — use this.
    pub(super) fn require_ordered_map_keys_in(
        &mut self,
        ty: &Type,
        line: usize,
        source_ctx: Option<&str>,
    ) {
        match ty {
            Type::Map(k, v) => {
                self.require_ordered_map_key(k, line, source_ctx);
                self.require_ordered_map_keys_in(k, line, source_ctx);
                self.require_ordered_map_keys_in(v, line, source_ctx);
            }
            Type::Option(inner) | Type::List(inner) | Type::Vector(inner) => {
                self.require_ordered_map_keys_in(inner, line, source_ctx);
            }
            Type::Result(ok, err) => {
                self.require_ordered_map_keys_in(ok, line, source_ctx);
                self.require_ordered_map_keys_in(err, line, source_ctx);
            }
            Type::Tuple(items) => {
                for item in items {
                    self.require_ordered_map_keys_in(item, line, source_ctx);
                }
            }
            Type::Fn(params, ret, _) => {
                for param in params {
                    self.require_ordered_map_keys_in(param, line, source_ctx);
                }
                self.require_ordered_map_keys_in(ret, line, source_ctx);
            }
            Type::Named { .. }
            | Type::Int
            | Type::Float
            | Type::Str
            | Type::Bool
            | Type::Unit
            | Type::Var(_)
            | Type::Invalid => {}
        }
    }

    /// Whether `ty`, used as a map key, reaches a `Float`.
    ///
    /// A map iterates its entries sorted by key, and every backend and the
    /// proof model have to state that order the same way. `Float` is the one
    /// type where they cannot: a NaN has no place in the finite range, the
    /// generated Rust cannot even build the map (`f64` is neither `Eq` nor
    /// `Hash`), and the proof model has no faithful counterpart for the
    /// runtime's total order. Everything else — including a record, a
    /// variant, a list or a tuple — has a canonical order built out of its
    /// parts, so it keys a map perfectly well.
    ///
    /// The walk descends into named types, because a `Float` sitting in a
    /// record's field is exactly as unsupported as one written in key
    /// position.
    pub(crate) fn map_key_unorderable_part(&self, ty: &Type) -> Option<(String, Option<String>)> {
        let mut seen: Vec<String> = Vec::new();
        self.map_key_unorderable_path(ty, &mut seen)
    }

    fn map_key_unorderable_path(
        &self,
        ty: &Type,
        seen: &mut Vec<String>,
    ) -> Option<(String, Option<String>)> {
        match ty {
            Type::Float => Some(("Float".to_string(), None)),
            // A map and a vector have no order of their own — the runtime's
            // key comparator has no arm for either, and the emitted Rust has
            // no `Ord`. A key that reaches one is refused rather than sorted
            // by whatever a fallback happens to do.
            Type::Map(_, _) => Some(("Map".to_string(), None)),
            Type::Vector(_) => Some(("Vector".to_string(), None)),
            Type::Option(inner) | Type::List(inner) => self.map_key_unorderable_path(inner, seen),
            Type::Result(left, right) => self
                .map_key_unorderable_path(left, seen)
                .or_else(|| self.map_key_unorderable_path(right, seen)),
            Type::Tuple(items) => items
                .iter()
                .find_map(|item| self.map_key_unorderable_path(item, seen)),
            Type::Named { name, .. } => {
                let canonical = self.canonical_type_name(name);
                if seen.contains(&canonical) {
                    return None;
                }
                seen.push(canonical.clone());
                let field_hit = self
                    .record_field_types
                    .iter()
                    .filter(|(key, _)| key.type_name == canonical)
                    .find_map(|(key, field_ty)| {
                        self.map_key_unorderable_path(field_ty, seen)
                            .map(|(what, _)| (what, Some(format!("{}.{}", name, key.field_name))))
                    });
                if field_hit.is_some() {
                    return field_hit;
                }
                let variants = self
                    .type_variants
                    .get(&canonical)
                    .cloned()
                    .unwrap_or_default();
                variants.into_iter().find_map(|variant| {
                    let ctor = format!("{canonical}.{variant}");
                    let params = self.find_fn_sig(&ctor).map(|sig| sig.params.clone())?;
                    params
                        .iter()
                        .find_map(|param| self.map_key_unorderable_path(param, seen))
                        .map(|(what, _)| (what, Some(format!("{}.{}", name, variant))))
                })
            }
            Type::Fn(_, _, _)
            | Type::Int
            | Type::Str
            | Type::Bool
            | Type::Unit
            | Type::Var(_)
            | Type::Invalid => None,
        }
    }

    pub(crate) fn type_contains_capability_resource(&self, ty: &Type) -> bool {
        match ty {
            Type::Named { id, name } => {
                let typed = id.map(|id| self.symbol_table.type_entry(id).key.canonical());
                typed
                    .as_deref()
                    .is_some_and(|name| self.capabilities.is_resource_tainted(name))
                    || self.capabilities.is_resource_tainted(name)
                    || self.current_module_prefix.as_deref().is_some_and(|scope| {
                        self.capabilities
                            .is_resource_tainted(&format!("{scope}.{name}"))
                    })
                    || self
                        .capabilities
                        .is_resource_tainted(&self.canonical_type_name(name))
            }
            Type::Option(inner) | Type::List(inner) | Type::Vector(inner) => {
                self.type_contains_capability_resource(inner)
            }
            Type::Result(left, right) | Type::Map(left, right) => {
                self.type_contains_capability_resource(left)
                    || self.type_contains_capability_resource(right)
            }
            Type::Tuple(items) | Type::Fn(items, _, _) => {
                items
                    .iter()
                    .any(|item| self.type_contains_capability_resource(item))
                    || matches!(ty, Type::Fn(_, ret, _) if self.type_contains_capability_resource(ret))
            }
            Type::Int
            | Type::Float
            | Type::Str
            | Type::Bool
            | Type::Unit
            | Type::Var(_)
            | Type::Invalid => false,
        }
    }

    pub(super) fn merge_bare_type_alias(&mut self, alias: String, id: TypeId) {
        self.bare_type_aliases
            .entry(alias)
            .and_modify(|r| r.merge(id))
            .or_insert(Resolution::Single(id));
    }

    /// Type-side equivalent of [`Self::resolve_fn_id`]. Same
    /// visibility gating: `SymbolTable` look-ups are filtered through
    /// `visible_type_ids` so a qualified `C.Shape` reference can't
    /// resolve to a private (non-exposed) type even though the symbol
    /// table holds every dep type unconditionally.
    pub(crate) fn resolve_type_id(&self, name: &str) -> Option<TypeId> {
        self.symbol_table
            .resolve_type_id_in(name, self.current_module_prefix.as_deref())
            .filter(|id| self.visible_type_ids.contains(id))
            .or_else(|| {
                self.bare_type_aliases
                    .get(name)
                    .and_then(Resolution::unambiguous)
            })
    }

    /// Resolve `module.alias` through that module's public type surface. The
    /// returned ID belongs to the original declaration, not necessarily to
    /// `module` itself (explicit re-exports deliberately preserve identity).
    pub(super) fn type_export_id(&self, module: &str, alias: &str) -> Option<TypeId> {
        let target = self.module_type_exports.get(module)?.get(alias)?;
        self.symbol_table
            .type_id_of(&TypeKey::in_module(&target.module, &target.name))
    }

    /// `true` when `name` (qualified `Module.Type` form) resolves to
    /// an existing `TypeId` in the symbol table but the typechecker
    /// hasn't registered that ID as visible to the current scope.
    /// Distinguishes "type doesn't exist anywhere" (silently miss →
    /// downstream "unknown type" error) from "type exists but its
    /// dep module doesn't expose it" (explicit private-import
    /// diagnostic emitted by `report_named_visibility_errors`).
    pub(crate) fn type_name_is_private_import(&self, name: &str) -> bool {
        let Some((prefix, n)) = name.rsplit_once('.') else {
            return false;
        };
        if self.current_module_prefix.as_deref() == Some(prefix) {
            // Self-references like `Main.foo` inside `Main` always
            // resolve through the entry-scope alias; never a privacy
            // failure.
            return false;
        }
        if self.visible_module_names.contains(prefix) {
            return self.type_export_id(prefix, n).is_none()
                && self
                    .symbol_table
                    .type_id_of(&TypeKey::in_module(prefix, n))
                    .is_some();
        }
        self.symbol_table
            .type_id_of(&TypeKey::in_module(prefix, n))
            .is_some()
            || self
                .module_type_exports
                .get(prefix)
                .is_some_and(|exports| exports.contains_key(n))
    }

    /// Canonical name (`"Module.Type"` or bare entry name) for a
    /// source-faithful type reference. Resolves through the symbol
    /// table; falls back to the input string for references the table
    /// doesn't know about (builtins, opaque host types, in-flight
    /// recovery from earlier errors).
    ///
    /// For entry-scope types in a checker that's currently processing
    /// items with a `module X` declaration, the returned name includes
    /// the `X.` prefix even though the symbol table itself stores
    /// entry items without one. This preserves the pre-phase-B
    /// canonical view the typechecker's internal maps
    /// (`type_variants`, `record_field_types`, …) are keyed against.
    pub(crate) fn canonical_type_name(&self, name: &str) -> String {
        match self.resolve_type_id(name) {
            Some(id) => {
                let entry = self.symbol_table.type_entry(id);
                if entry.module.is_entry()
                    && let Some(prefix) = self.current_module_prefix.as_deref()
                {
                    crate::visibility::qualified_name(prefix, &entry.key.name)
                } else {
                    entry.key.canonical()
                }
            }
            None => name.to_string(),
        }
    }

    /// Whether `name` resolves to a representation-less type owned and minted
    /// by a capability provider. Capability resources share the access gates
    /// used by module-level opaque types, but they are a distinct language
    /// concept and diagnostics must not describe them as `exposes opaque`.
    pub(crate) fn is_capability_resource_type(&self, name: &str) -> bool {
        let canonical = self.canonical_type_name(name);
        self.capabilities
            .resource_types()
            .any(|resource| resource == &canonical)
    }

    fn is_capability_resource_type_named(&self, type_id: Option<TypeId>, name: &str) -> bool {
        let canonical = self.canonical_type_name_from_stamp(type_id, name);
        self.capabilities
            .resource_types()
            .any(|resource| resource == &canonical)
    }

    /// Render the two types of a "expected X, got Y" diagnostic.
    ///
    /// Aver lets a module declare a type whose bare name a dependency
    /// also declares; the two are distinct types and the reference
    /// resolves to the local one. Both then print as the same source
    /// spelling, so the plain message reads `expects Thing, got Thing`
    /// and looks like the compiler contradicting itself. When the two
    /// spellings collide but the identities differ, qualify each side
    /// with the module that declares it.
    ///
    /// When the spellings already differ, the pair is returned exactly
    /// as [`Type::display`] renders it — every existing message keeps
    /// its wording.
    pub(crate) fn describe_type_pair(&self, expected: &Type, actual: &Type) -> (String, String) {
        let plain = (expected.display(), actual.display());
        if plain.0 != plain.1 {
            return plain;
        }
        let rename = |id: TypeId| self.declaring_module_name(id);
        let qualified = (expected.display_with(&rename), actual.display_with(&rename));
        if qualified.0 == qualified.1 {
            return plain;
        }
        qualified
    }

    /// `Module.Type` for a resolved type reference, or `None` when no
    /// declaring module can be named (an entry-scope type in a file
    /// with no `module` declaration).
    fn declaring_module_name(&self, id: TypeId) -> Option<String> {
        let entry = self.symbol_table.type_entry_if_present(id)?;
        let scope = entry
            .key
            .scope_str()
            .or(self.current_module_prefix.as_deref())?;
        Some(crate::visibility::qualified_name(scope, &entry.key.name))
    }

    // -- Unified lookups ---------------------------------------------------

    fn find_fn_sig(&self, key: &str) -> Option<&FnSig> {
        self.find_fn_sig_resolved(key).map(|(_, sig)| sig)
    }

    /// [`Self::find_fn_sig`] plus the resolved user-fn identity the
    /// signature came from.
    ///
    /// The `FnId` is `Some` EXACTLY when the returned signature is the one
    /// `fn_sigs[id]` holds — i.e. when the reference resolved to a
    /// program-declared function. Builtin and constructor signatures come
    /// out of `extra_sigs`, which has no identity, so they answer `None`.
    ///
    /// Callers that key a rewrite on "which function is this really" must
    /// use this and not re-resolve the name themselves: the whole point is
    /// that the identity and the signature the call is checked against are
    /// produced by one lookup and therefore cannot disagree.
    pub(crate) fn find_fn_sig_resolved(&self, key: &str) -> Option<(Option<FnId>, &FnSig)> {
        // Phase B: user fns live in `fn_sigs` keyed by `FnId`; everything
        // else (builtins + sum-type variant constructors) stays in
        // `extra_sigs`. Direct hit on `extra_sigs` covers references that
        // came in already-canonicalised; `resolve_fn_id` chains the
        // symbol-table lookups for bare/qualified user-fn references.
        if let Some(id) = self.resolve_fn_id(key)
            && let Some(sig) = self.fn_sigs.get(&id)
        {
            return Some((Some(id), sig));
        }
        if let Some(sig) = self.extra_sigs.get(key) {
            return Some((None, sig));
        }
        // Try canonicalised form for type-derived keys
        // (`"Module.Type.Variant"`).
        let canonical = self.canonical_extra_key(key);
        if canonical != key {
            return self.extra_sigs.get(&canonical).map(|sig| (None, sig));
        }
        None
    }

    /// Take a bare-or-qualified key that may name a constructor or a
    /// per-type member (`"Shape.Circle"`, `"Status.Open"`) and resolve
    /// the leading type segment through `canonical_type_name`. Uses
    /// the typechecker view of canonical names (which include the
    /// entry module's prefix), matching what `register_type_def_sigs`
    /// inserts into `extra_sigs`.
    fn canonical_extra_key(&self, key: &str) -> String {
        if let Some((head, tail)) = key.split_once('.') {
            let canonical_type = self.canonical_type_name(head);
            if canonical_type != head {
                return format!("{}.{}", canonical_type, tail);
            }
        }
        key.to_string()
    }

    fn find_value_member(&self, key: &str) -> Option<&Type> {
        if let Some(v) = self.value_members.get(key) {
            return Some(v);
        }
        let canonical = self.canonical_extra_key(key);
        if canonical != key {
            return self.value_members.get(&canonical);
        }
        None
    }

    fn find_record_field_type(&self, type_name: &str, field_name: &str) -> Option<&Type> {
        let direct = RecordFieldKey::new(type_name, field_name);
        if let Some(ty) = self.record_field_types.get(&direct) {
            return Some(ty);
        }
        let canonical_type = self.canonical_type_name(type_name);
        if canonical_type != type_name {
            let canonical = RecordFieldKey::new(canonical_type, field_name);
            return self.record_field_types.get(&canonical);
        }
        None
    }

    /// Field lookup for a type carried by an already-typed value. The value's
    /// `TypeId` records the declaration that produced it even when the current
    /// module never wrote or directly imported that type name — for example a
    /// public `Rules.policy: Policy` field projected by a consumer that only
    /// depends on `Rules`. Re-resolving bare `Policy` in the consumer's source
    /// scope loses that identity and used to stamp the next projection
    /// `Type::Invalid` without reporting an error.
    fn find_record_field_type_named(
        &self,
        type_id: Option<TypeId>,
        type_name: &str,
        field_name: &str,
    ) -> Option<&Type> {
        let canonical = self.canonical_type_name_from_stamp(type_id, type_name);
        self.record_field_types
            .get(&RecordFieldKey::new(canonical, field_name))
            .or_else(|| self.find_record_field_type(type_name, field_name))
    }

    fn fields_for_type(&self, type_name: &str) -> Vec<(String, Type)> {
        let canonical = self.canonical_type_name(type_name);
        let canonical_ref: &str = canonical.as_str();
        self.record_field_types
            .iter()
            .filter(|(k, _)| k.type_name == canonical_ref || k.type_name == type_name)
            .map(|(k, v)| (k.field_name.clone(), v.clone()))
            .collect()
    }

    fn has_record_schema(&self, type_name: &str) -> bool {
        let canonical = self.canonical_type_name(type_name);
        let canonical_ref: &str = canonical.as_str();
        self.record_field_types
            .keys()
            .any(|k| k.type_name == canonical_ref || k.type_name == type_name)
    }

    fn has_record_schema_named(&self, type_id: Option<TypeId>, type_name: &str) -> bool {
        let canonical = self.canonical_type_name_from_stamp(type_id, type_name);
        self.record_field_types
            .keys()
            .any(|key| key.type_name == canonical)
            || self.has_record_schema(type_name)
    }

    fn canonical_type_name_from_stamp(&self, type_id: Option<TypeId>, name: &str) -> String {
        let Some(entry) = type_id.and_then(|id| self.symbol_table.type_entry_if_present(id)) else {
            return self.canonical_type_name(name);
        };
        if entry.module.is_entry()
            && let Some(prefix) = self.current_module_prefix.as_deref()
        {
            crate::visibility::qualified_name(prefix, &entry.key.name)
        } else {
            entry.key.canonical()
        }
    }

    /// Look up the variant list for a named sum type. Resolves
    /// `name` through `canonical_type_name` so bare references find
    /// the canonical "Module.Type" entry registered by
    /// `register_type_def_sigs` / `integrate_registry`.
    pub(crate) fn variants_for(&self, name: &str) -> Option<&Vec<String>> {
        if let Some(v) = self.type_variants.get(name) {
            return Some(v);
        }
        let canonical = self.canonical_type_name(name);
        if canonical != name {
            return self.type_variants.get(&canonical);
        }
        None
    }

    pub(crate) fn has_variants_for(&self, name: &str) -> bool {
        self.variants_for(name).is_some()
    }

    /// Iterate every fn signature regardless of which storage half
    /// holds it. Used by the namespace-prefix check and by
    /// `finalize_check_result` to flatten for external export.
    fn all_fn_sigs(&self) -> impl Iterator<Item = (String, &FnSig)> + '_ {
        let from_user = self.fn_sigs.iter().map(|(id, sig)| {
            let name = self.symbol_table.fn_entry(*id).key.canonical();
            (name, sig)
        });
        let from_extra = self.extra_sigs.iter().map(|(k, sig)| (k.clone(), sig));
        from_user.chain(from_extra)
    }

    fn fn_sig_contains_canonical(&self, canonical: &str) -> bool {
        if let Some(id) = self.resolve_fn_id(canonical)
            && self.fn_sigs.contains_key(&id)
        {
            return true;
        }
        if self.extra_sigs.contains_key(canonical) {
            return true;
        }
        let canonical_form = self.canonical_extra_key(canonical);
        canonical_form != canonical && self.extra_sigs.contains_key(&canonical_form)
    }

    /// Insert a fn signature under its canonical form. Routes to the
    /// `FnId`-keyed user map when the name resolves through the
    /// symbol table (i.e. it names a user fn declared in `items` or a
    /// dep module); otherwise it lands in the `extra_sigs` half.
    ///
    /// Also marks the `FnId` as visible to the current scope — every
    /// fn the checker's own `build_signatures` / `integrate_registry`
    /// path inserts is by definition reachable from here (own module
    /// items or visibility-exposed dep entries). The visibility
    /// gating in `resolve_fn_id` then refuses look-ups against any
    /// `FnId` that landed in the symbol table but not in this set —
    /// a qualified `C.helper()` reference whose `helper` isn't on
    /// `C`'s `exposes` list never gets inserted here and so never
    /// resolves.
    fn insert_fn_sig(&mut self, canonical: &str, sig: FnSig) {
        match self.fn_id_for_canonical(canonical) {
            Some(id) => {
                self.fn_sigs.insert(id, sig);
                self.visible_fn_ids.insert(id);
            }
            None => {
                self.extra_sigs.insert(canonical.to_string(), sig);
            }
        }
    }

    /// `resolve_fn_id` minus the visibility filter — used by
    /// `insert_fn_sig` (where the very point of the insert is to
    /// register visibility) and by other boundary points that build
    /// the visible set itself. Production look-ups must go through
    /// `resolve_fn_id`.
    fn fn_id_for_canonical(&self, name: &str) -> Option<FnId> {
        if let Some((prefix, n)) = name.rsplit_once('.') {
            if let Some(id) = self.symbol_table.fn_id_of(&FnKey::in_module(prefix, n)) {
                return Some(id);
            }
            if self.current_module_prefix.as_deref() == Some(prefix)
                && let Some(id) = self.symbol_table.fn_id_of(&FnKey::entry(n))
            {
                return Some(id);
            }
        }
        if let Some(prefix) = self.current_module_prefix.as_deref()
            && let Some(id) = self.symbol_table.fn_id_of(&FnKey::in_module(prefix, name))
        {
            return Some(id);
        }
        self.symbol_table.fn_id_of(&FnKey::entry(name))
    }

    /// Type-side equivalent of [`Self::fn_id_for_canonical`].
    fn type_id_for_canonical(&self, name: &str) -> Option<TypeId> {
        if let Some((prefix, n)) = name.rsplit_once('.') {
            if let Some(id) = self.symbol_table.type_id_of(&TypeKey::in_module(prefix, n)) {
                return Some(id);
            }
            if self.current_module_prefix.as_deref() == Some(prefix)
                && let Some(id) = self.symbol_table.type_id_of(&TypeKey::entry(n))
            {
                return Some(id);
            }
        }
        if let Some(prefix) = self.current_module_prefix.as_deref()
            && let Some(id) = self
                .symbol_table
                .type_id_of(&TypeKey::in_module(prefix, name))
        {
            return Some(id);
        }
        self.symbol_table.type_id_of(&TypeKey::entry(name))
    }

    /// Mark a `TypeId` as visible to the current scope. Called from
    /// `register_type_def_sigs` (own module types) and
    /// `integrate_registry` (visibility-exposed dep types).
    fn mark_type_visible(&mut self, id: TypeId) {
        self.visible_type_ids.insert(id);
    }

    // -- Helpers -----------------------------------------------------------

    /// Check whether `required_effect` is satisfied by `caller_effects`.
    fn caller_has_effect(&self, caller_effects: &[String], required_effect: &str) -> bool {
        caller_effects
            .iter()
            .any(|declared| crate::effects::effect_satisfies(declared, required_effect))
    }

    fn error(&mut self, msg: impl Into<String>) {
        let line = self.current_fn_line.unwrap_or(1);
        self.errors.push(TypeError {
            message: msg.into(),
            line,
            col: 0,
            origin: None,
            secondary: None,
        });
    }

    fn error_at_line(&mut self, line: usize, msg: impl Into<String>) {
        self.errors.push(TypeError {
            message: msg.into(),
            line,
            col: 0,
            origin: None,
            secondary: None,
        });
    }

    /// Current length of the diagnostic list, for use with
    /// [`Self::discard_errors_since`].
    pub(super) fn error_mark(&self) -> usize {
        self.errors.len()
    }

    /// Drop every diagnostic recorded since `mark`.
    ///
    /// Only for a recogniser that inferred a subexpression to decide
    /// whether it applies and then declined. Inference is not free of
    /// side effects: it appends diagnostics. The caller returning `None`
    /// hands the same node back to the general path, which re-infers it
    /// and reports for itself, so anything the probe recorded would
    /// otherwise be reported twice.
    pub(super) fn discard_errors_since(&mut self, mark: usize) {
        self.errors.truncate(mark);
    }

    fn insert_sig(&mut self, name: &str, params: &[Type], ret: Type, effects: &[&str]) {
        // Builtins (Int.add, Console.print, …) are not part of the
        // user-program symbol table, so they always land in
        // `extra_sigs`. The `insert_fn_sig` router would normally
        // resolve user fns into `fn_sigs` — but no `register_builtins`
        // caller could ever name a user fn, so we short-circuit here
        // to avoid pointless symbol-table probes.
        self.extra_sigs.insert(
            name.to_string(),
            FnSig {
                params: params.to_vec(),
                ret,
                effects: effects.iter().map(|s| s.to_string()).collect(),
            },
        );
    }

    fn fn_type_from_sig(sig: &FnSig) -> Type {
        Type::Fn(
            sig.params.clone(),
            Box::new(sig.ret.clone()),
            sig.effects.clone(),
        )
    }

    fn sig_from_callable_type(ty: &Type) -> Option<FnSig> {
        match ty {
            Type::Fn(params, ret, effects) => Some(FnSig {
                params: params.clone(),
                ret: *ret.clone(),
                effects: effects.clone(),
            }),
            _ => None,
        }
    }

    fn binding_type(&self, name: &str) -> Option<Type> {
        self.locals
            .get(name)
            .or_else(|| self.globals.get(name))
            .cloned()
    }

    /// Phase B: `&self`-bearing constraint check. Resolves bare named
    /// types against the `SymbolTable` (carried on `self`) so
    /// source-faithful Spanned stamps still match against canonical fn
    /// signatures. Replaces the pre-phase-B `sig_aliases` string→string
    /// alias map with typed `TypeId` resolution.
    pub(super) fn compatible(&self, actual: &Type, expected: &Type) -> bool {
        let mut subst = HashMap::new();
        Self::match_expected_type_inner(actual, expected, &mut subst, Some(self))
    }

    /// Static-form matcher (no symbol-table resolution). Tests use
    /// this directly; production code should reach for `compatible`
    /// instead.
    pub(super) fn match_expected_type(
        actual: &Type,
        expected: &Type,
        subst: &mut HashMap<String, Type>,
    ) -> bool {
        Self::match_expected_type_inner(actual, expected, subst, None)
    }

    /// `&self` matcher that lets the caller carry a substitution
    /// (poly fn arg inference). The pure `compatible` helper above
    /// hides `subst` for the common "no `Type::Var` involved" callers;
    /// this method exposes it for the FnCall arg loop in `infer/expr.rs`.
    pub(super) fn match_with(
        &self,
        actual: &Type,
        expected: &Type,
        subst: &mut HashMap<String, Type>,
    ) -> bool {
        Self::match_expected_type_inner(actual, expected, subst, Some(self))
    }

    fn match_expected_type_inner(
        actual: &Type,
        expected: &Type,
        subst: &mut HashMap<String, Type>,
        checker: Option<&TypeChecker>,
    ) -> bool {
        // Iron — A4: `Type::Invalid` is the checker's "we already
        // reported an error here, don't compound it" sentinel.
        // Returning `false` for it turned every downstream use site
        // into a fresh `expected X, got Invalid` diagnostic — a single
        // unknown-fn call could fan out to N + 1 errors (the unknown
        // fn plus one per downstream consumer). Treat Invalid as a
        // wildcard on either side so the original error stands alone.
        // Per-callsite guards like `!matches!(ty, Type::Invalid)`
        // around `self.compatible(...)` are now redundant but harmless;
        // sweeping them is deliberately out of scope here.
        if matches!(actual, Type::Invalid) || matches!(expected, Type::Invalid) {
            return true;
        }
        match expected {
            Type::Var(name) => Self::bind_expected_var(name, actual, subst),
            Type::Invalid => unreachable!("Type::Invalid handled by the early guard above"),
            Type::Int => matches!(actual, Type::Int),
            Type::Float => matches!(actual, Type::Float),
            Type::Str => matches!(actual, Type::Str),
            Type::Bool => matches!(actual, Type::Bool),
            Type::Unit => matches!(actual, Type::Unit),
            Type::Named {
                id: expected_id,
                name: expected_name,
            } => match actual {
                // Phase B: typed-identity comparison. When both sides
                // carry a `TypeId` (resolved against the symbol table)
                // we compare IDs directly — two unrelated modules
                // declaring `Shape` get distinct `TypeId`s by
                // construction and so stay incompatible. When the
                // checker is available we resolve either side's bare
                // string through `resolve_type_id` to bring it into
                // the typed identity domain; without the checker (or
                // for references that don't resolve, like builtin
                // `Http.Response`) we fall back to canonical-name
                // equality.
                Type::Named {
                    id: actual_id,
                    name: actual_name,
                } => {
                    // Peer review round 6: do NOT auto-resolve an
                    // unresolved side in the matcher's
                    // (importer-context) symbol table. Upstream
                    // signature/binding boundaries
                    // (`canonicalize_named`,
                    // `canonicalize_named_in_module`) are
                    // responsible for stamping `id` in the correct
                    // owner context. If a `Type::Named` reaches the
                    // matcher with `id = None`, that's a deliberate
                    // unresolved state — either a genuine builtin
                    // (Http.Response) or a resolution gap the matcher
                    // must surface, not silently paper over by
                    // re-resolving in the wrong scope.
                    let exp_id = *expected_id;
                    let act_id = *actual_id;
                    // Phase B (peer review round 2): the typed-identity
                    // comparison must reject mixed (Some, None) cases
                    // for user-defined types — otherwise an ambiguous
                    // bare reference (`Shape` when both `A.Shape` and
                    // `B.Shape` are exposed) silently matches against
                    // any specific `A.Shape` / `B.Shape` via the
                    // string fallback below. Distinguish "ambiguous
                    // bare reference, identity deliberately
                    // suppressed" from "builtin name that has no
                    // typed identity by design (`Http.Response`,
                    // `Buffer`, …)" by asking the checker whether the
                    // unresolved side's name is recorded as
                    // ambiguous; reject in that case, allow name
                    // fallback otherwise.
                    match (exp_id, act_id) {
                        (Some(e), Some(a)) => e == a,
                        // Peer review round 6 entry-fallback bug:
                        // a dep module's unresolved bare `Shape` was
                        // silently binding to the entry module's
                        // `Shape` via name equality here. Reject all
                        // mixed (Some, None) cases. Builtins always
                        // exercise (None, None) below; user-source
                        // typed/raw mixes are by definition a
                        // resolution gap and must surface.
                        (Some(_), None) | (None, Some(_)) => false,
                        (None, None) => {
                            let exp = checker
                                .map(|c| c.canonical_type_name(expected_name))
                                .unwrap_or_else(|| expected_name.clone());
                            let act = checker
                                .map(|c| c.canonical_type_name(actual_name))
                                .unwrap_or_else(|| actual_name.clone());
                            exp == act
                        }
                    }
                }
                _ => false,
            },
            Type::Option(expected_inner) => match actual {
                Type::Option(actual_inner) => {
                    Self::match_expected_type_inner(actual_inner, expected_inner, subst, checker)
                }
                _ => false,
            },
            Type::List(expected_inner) => match actual {
                Type::List(actual_inner) => {
                    Self::match_expected_type_inner(actual_inner, expected_inner, subst, checker)
                }
                _ => false,
            },
            Type::Vector(expected_inner) => match actual {
                Type::Vector(actual_inner) => {
                    Self::match_expected_type_inner(actual_inner, expected_inner, subst, checker)
                }
                _ => false,
            },
            Type::Result(expected_ok, expected_err) => match actual {
                Type::Result(actual_ok, actual_err) => {
                    Self::match_expected_type_inner(actual_ok, expected_ok, subst, checker)
                        && Self::match_expected_type_inner(actual_err, expected_err, subst, checker)
                }
                _ => false,
            },
            Type::Map(expected_k, expected_v) => match actual {
                Type::Map(actual_k, actual_v) => {
                    Self::match_expected_type_inner(actual_k, expected_k, subst, checker)
                        && Self::match_expected_type_inner(actual_v, expected_v, subst, checker)
                }
                _ => false,
            },
            Type::Tuple(expected_items) => match actual {
                Type::Tuple(actual_items) if actual_items.len() == expected_items.len() => {
                    actual_items.iter().zip(expected_items.iter()).all(
                        |(actual_item, expected_item)| {
                            Self::match_expected_type_inner(
                                actual_item,
                                expected_item,
                                subst,
                                checker,
                            )
                        },
                    )
                }
                _ => false,
            },
            Type::Fn(expected_params, expected_ret, expected_effects) => match actual {
                Type::Fn(actual_params, actual_ret, actual_effects)
                    if actual_params.len() == expected_params.len() =>
                {
                    actual_params.iter().zip(expected_params.iter()).all(
                        |(actual_param, expected_param)| {
                            Self::match_expected_type_inner(
                                actual_param,
                                expected_param,
                                subst,
                                checker,
                            )
                        },
                    ) && Self::match_expected_type_inner(actual_ret, expected_ret, subst, checker)
                        && (crate::effects::forwards_callback_effects(expected_effects)
                            || actual_effects.iter().all(|actual| {
                                expected_effects.iter().any(|expected| {
                                    crate::effects::effect_satisfies(expected, actual)
                                })
                            }))
                }
                _ => false,
            },
        }
    }

    fn bind_expected_var(name: &str, actual: &Type, subst: &mut HashMap<String, Type>) -> bool {
        match actual {
            Type::Var(actual_name) => return actual_name == name,
            // Iron — A4: matches the wildcard in `match_expected_type_inner`.
            // An already-errored actual binds vacuously instead of
            // refusing the unification and triggering a cascade.
            Type::Invalid => return true,
            _ => {}
        }
        if let Some(bound) = subst.get(name).cloned() {
            return Self::match_expected_type(actual, &bound, subst)
                && Self::match_expected_type(&bound, actual, subst);
            // bind_expected_var is alias-agnostic — Var bindings
            // never compare Named types against `sig_aliases` since
            // the binding rule already accepts whatever concrete
            // type the caller hands in.
        }
        // Occurs check — refuse `T := F<…T…>` style circular bindings.
        // Without this, polymorphic recursion patterns like `fn nest(v:
        // A) -> Unit; nest([v])` would insert `A → List<A>` into `subst`
        // and rely on downstream structural mismatch to terminate
        // matching. The HashMap entry itself is still a cycle that
        // later `instantiate_type` walks would have to skip; rejecting
        // the bind at source keeps the substitution map well-formed
        // and surfaces the constraint failure to the caller as a
        // normal type-incompatibility error.
        if Self::type_contains_var(actual, name) {
            return false;
        }
        subst.insert(name.to_string(), actual.clone());
        true
    }

    /// Structural recursion over `ty` looking for any `Type::Var(name)`.
    /// Used by the occurs check in [`bind_expected_var`]; not exposed
    /// elsewhere because it's a one-step deep walk over a finite Type
    /// AST (no shared subterms, no cycles in the AST itself — the cycle
    /// would only exist in the substitution map, which the bind path
    /// is what guards).
    fn type_contains_var(ty: &Type, name: &str) -> bool {
        match ty {
            Type::Var(other) => other == name,
            Type::Int
            | Type::Float
            | Type::Str
            | Type::Bool
            | Type::Unit
            | Type::Invalid
            | Type::Named { .. } => false,
            Type::Option(inner) | Type::List(inner) | Type::Vector(inner) => {
                Self::type_contains_var(inner, name)
            }
            Type::Result(ok, err) => {
                Self::type_contains_var(ok, name) || Self::type_contains_var(err, name)
            }
            Type::Map(k, v) => Self::type_contains_var(k, name) || Self::type_contains_var(v, name),
            Type::Tuple(items) => items.iter().any(|t| Self::type_contains_var(t, name)),
            Type::Fn(params, ret, _effects) => {
                params.iter().any(|p| Self::type_contains_var(p, name))
                    || Self::type_contains_var(ret, name)
            }
        }
    }

    pub(super) fn instantiate_type(ty: &Type, subst: &HashMap<String, Type>) -> Type {
        match ty {
            Type::Var(name) => subst.get(name).cloned().unwrap_or_else(|| ty.clone()),
            Type::Result(ok, err) => Type::Result(
                Box::new(Self::instantiate_type(ok, subst)),
                Box::new(Self::instantiate_type(err, subst)),
            ),
            Type::Option(inner) => Type::Option(Box::new(Self::instantiate_type(inner, subst))),
            Type::List(inner) => Type::List(Box::new(Self::instantiate_type(inner, subst))),
            Type::Vector(inner) => Type::Vector(Box::new(Self::instantiate_type(inner, subst))),
            Type::Map(k, v) => Type::Map(
                Box::new(Self::instantiate_type(k, subst)),
                Box::new(Self::instantiate_type(v, subst)),
            ),
            Type::Tuple(items) => Type::Tuple(
                items
                    .iter()
                    .map(|item| Self::instantiate_type(item, subst))
                    .collect(),
            ),
            Type::Fn(params, ret, effects) => Type::Fn(
                params
                    .iter()
                    .map(|param| Self::instantiate_type(param, subst))
                    .collect(),
                Box::new(Self::instantiate_type(ret, subst)),
                effects.clone(),
            ),
            Type::Int
            | Type::Float
            | Type::Str
            | Type::Bool
            | Type::Unit
            | Type::Invalid
            | Type::Named { .. } => ty.clone(),
        }
    }
}

/// Aver → target language transpilation.
///
/// The codegen module transforms a type-checked Aver AST into source code
/// for a target language. Current backends: Rust deployment and Lean proof export.
pub(crate) mod builtin_helpers;
pub(crate) mod builtin_records;
pub(crate) mod builtins;
pub mod common;
#[cfg(feature = "runtime")]
pub mod dafny;
#[cfg(feature = "runtime")]
pub mod lean;
#[cfg(feature = "runtime")]
pub mod recursion;
#[cfg(feature = "runtime")]
pub mod rust;
#[cfg(feature = "wasm-compile")]
pub mod wasm;

use std::collections::{HashMap, HashSet};

use crate::ast::{FnDef, TopLevel, TypeDef};
use crate::types::checker::TypeCheckResult;

/// Information about a dependent module loaded for codegen.
pub struct ModuleInfo {
    /// Qualified module path, e.g. "Models.User".
    pub prefix: String,
    /// Direct `depends [...]` entries from the source module.
    pub depends: Vec<String>,
    /// Type definitions from the module.
    pub type_defs: Vec<TypeDef>,
    /// Function definitions from the module (excluding `main`).
    pub fn_defs: Vec<FnDef>,
}

/// Collected context from the Aver program, shared across all backends.
pub struct CodegenContext {
    /// All top-level items (post-TCO transform, post-typecheck).
    pub items: Vec<TopLevel>,
    /// Function signatures: name → (param_types, return_type, effects).
    pub fn_sigs: HashMap<String, (Vec<crate::types::Type>, crate::types::Type, Vec<String>)>,
    /// Functions eligible for auto-memoization.
    pub memo_fns: HashSet<String>,
    /// Set of type names whose values are memo-safe.
    pub memo_safe_types: HashSet<String>,
    /// User-defined type definitions (for struct/enum generation).
    pub type_defs: Vec<TypeDef>,
    /// User-defined function definitions.
    pub fn_defs: Vec<FnDef>,
    /// Project/binary name.
    pub project_name: String,
    /// Dependent modules loaded for inlining.
    pub modules: Vec<ModuleInfo>,
    /// Set of module prefixes for qualified name resolution (e.g. "Models.User").
    pub module_prefixes: HashSet<String>,
    /// Embedded runtime policy from `aver.toml` for generated code.
    #[cfg(feature = "runtime")]
    pub policy: Option<crate::config::ProjectConfig>,
    /// Emit generated scoped runtime support (replay and/or runtime-loaded policy).
    pub emit_replay_runtime: bool,
    /// Load runtime policy from the active module root instead of embedding it.
    pub runtime_policy_from_env: bool,
    /// Explicit guest entry boundary for scoped replay/policy.
    pub guest_entry: Option<String>,
    /// Emit extra generated helpers needed only by the cached self-host helper.
    pub emit_self_host_support: bool,
    /// Extra fn_defs visible during current module emission (not in `fn_defs` or `modules`).
    /// Set temporarily by the Rust backend when emitting a dependent module so that
    /// `find_fn_def_by_name` can resolve same-module calls.
    pub extra_fn_defs: Vec<FnDef>,
    /// Functions that are part of a mutual-TCO SCC group (emitted as trampoline + wrappers).
    /// Functions NOT in this set but with TailCalls are emitted as plain self-TCO loops.
    pub mutual_tco_members: HashSet<String>,
    /// Buffer-build sink fns (`List.prepend`/`reverse` builders consumed
    /// by `String.join`). The Rust backend emits a `<fn>__buffered`
    /// variant alongside each entry; the WASM backend rewrites bodies
    /// to call `rt_buffer_*` helpers. Detection lives in `ir::buffer_build`.
    pub buffer_build_sinks: HashMap<String, crate::ir::BufferBuildShape>,
    /// Fusion sites detected for `String.join(<sink>(...), sep)` calls.
    /// Each entry pairs an enclosing fn + line + sink fn name; the
    /// emitter rewrites these call expressions to use buffered variants
    /// in place of the producer + consumer chain.
    pub buffer_fusion_sites: Vec<crate::ir::FusionSite>,
}

/// Output files from a codegen backend.
pub struct ProjectOutput {
    /// Files to write: (relative_path, content).
    pub files: Vec<(String, String)>,
}

/// Build a CodegenContext from parsed + type-checked items.
pub fn build_context(
    items: Vec<TopLevel>,
    tc_result: &TypeCheckResult,
    memo_fns: HashSet<String>,
    project_name: String,
    modules: Vec<ModuleInfo>,
) -> CodegenContext {
    let type_defs: Vec<TypeDef> = items
        .iter()
        .filter_map(|item| {
            if let TopLevel::TypeDef(td) = item {
                Some(td.clone())
            } else {
                None
            }
        })
        .collect();

    let fn_defs: Vec<FnDef> = items
        .iter()
        .filter_map(|item| {
            if let TopLevel::FnDef(fd) = item {
                Some(fd.clone())
            } else {
                None
            }
        })
        .collect();

    let module_prefixes: HashSet<String> = modules.iter().map(|m| m.prefix.clone()).collect();

    // Compute which functions are in mutual-TCO SCC groups (emitted as trampoline + wrappers).
    let mut mutual_tco_members = HashSet::new();
    {
        // Entry module (non-main)
        let entry_fns: Vec<&FnDef> = fn_defs.iter().filter(|fd| fd.name != "main").collect();
        for group in crate::call_graph::tailcall_scc_components(&entry_fns) {
            for fd in &group {
                mutual_tco_members.insert(fd.name.clone());
            }
        }
        // Dependent modules
        for module in &modules {
            let mod_fns: Vec<&FnDef> = module.fn_defs.iter().collect();
            for group in crate::call_graph::tailcall_scc_components(&mod_fns) {
                for fd in &group {
                    mutual_tco_members.insert(fd.name.clone());
                }
            }
        }
    }

    // Start with checker's fn_sigs (exposed API), then add signatures for
    // ALL module functions (including private helpers) via SymbolRegistry.
    // Codegen emits full module implementations, so it needs signatures for
    // intra-module calls that the checker intentionally omits.
    let mut fn_sigs = tc_result.fn_sigs.clone();
    {
        let pairs: Vec<(String, Vec<TopLevel>)> = modules
            .iter()
            .map(|m| {
                let items: Vec<TopLevel> = m
                    .fn_defs
                    .iter()
                    .map(|fd| TopLevel::FnDef(fd.clone()))
                    .chain(m.type_defs.iter().map(|td| TopLevel::TypeDef(td.clone())))
                    .collect();
                (m.prefix.clone(), items)
            })
            .collect();
        let registry = crate::visibility::SymbolRegistry::from_modules_all(&pairs);
        for entry in &registry.entries {
            if fn_sigs.contains_key(&entry.canonical_name) {
                continue;
            }
            if let crate::visibility::SymbolKind::Function {
                params,
                return_type,
                effects,
                ..
            } = &entry.kind
            {
                let parsed_params: Vec<crate::types::Type> = params
                    .iter()
                    .map(|(_, ty_str)| crate::types::parse_type_str(ty_str))
                    .collect();
                let ret = crate::types::parse_type_str(return_type);
                fn_sigs.insert(
                    entry.canonical_name.clone(),
                    (parsed_params, ret, effects.clone()),
                );
            }
        }
    }

    // Detect buffer-build sinks + their fusion sites once. Both the
    // Rust and WASM backends consume the same analysis; downstream
    // emit logic looks them up by name. Detection requires
    // post-TCO `Expr::TailCall` nodes — `items` here is post-tco per
    // the build_context contract — so no extra transform needed.
    let detect_fns: Vec<&FnDef> = fn_defs.iter().chain(modules.iter().flat_map(|m| m.fn_defs.iter())).collect();
    let buffer_build_sinks = crate::ir::compute_buffer_build_sinks(&detect_fns);
    let buffer_fusion_sites = crate::ir::find_fusion_sites(&detect_fns, &buffer_build_sinks);

    CodegenContext {
        items,
        fn_sigs,
        memo_fns,
        memo_safe_types: tc_result.memo_safe_types.clone(),
        type_defs,
        fn_defs,
        project_name,
        modules,
        module_prefixes,
        #[cfg(feature = "runtime")]
        policy: None,
        emit_replay_runtime: false,
        runtime_policy_from_env: false,
        guest_entry: None,
        emit_self_host_support: false,
        extra_fn_defs: Vec::new(),
        mutual_tco_members,
        buffer_build_sinks,
        buffer_fusion_sites,
    }
}

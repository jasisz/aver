/// Aver → target language transpilation.
///
/// The codegen module transforms a type-checked Aver AST into source code
/// for a target language. Current backends: Rust deployment and Lean proof export.
pub(crate) mod builtins;
pub(crate) mod common;
#[cfg(feature = "runtime")]
pub mod dafny;
#[cfg(feature = "runtime")]
pub mod lean;
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
    // ALL module functions (including private helpers). Codegen emits full
    // module implementations, so it needs signatures for intra-module calls
    // that the checker intentionally omits.
    let mut fn_sigs = tc_result.fn_sigs.clone();
    for module in &modules {
        for fd in &module.fn_defs {
            let qualified = crate::visibility::qualified_name(&module.prefix, &fd.name);
            if fn_sigs.contains_key(&qualified) {
                continue;
            }
            let params: Vec<crate::types::Type> = fd
                .params
                .iter()
                .map(|(_, ty_str)| {
                    crate::types::parse_type_str(ty_str)
                })
                .collect();
            let ret = crate::types::parse_type_str(&fd.return_type);
            let effects: Vec<String> = fd.effects.iter().map(|e| e.node.clone()).collect();
            fn_sigs.insert(qualified, (params, ret, effects));
        }
    }

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
    }
}

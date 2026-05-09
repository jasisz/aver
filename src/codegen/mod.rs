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
#[cfg(feature = "wasip2")]
pub mod wasip2;
#[cfg(feature = "wasm-compile")]
pub mod wasm_gc;

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
    /// IR-level analysis facts produced by the dep module's pipeline run
    /// (`analyze` stage). `None` for modules loaded via paths that skip
    /// the analyze stage (none in production today; left optional for
    /// future ad-hoc loaders). Aver's module DAG invariant makes per-module
    /// analysis sufficient — see `project_aver_module_dag` memory and
    /// `src/ir/analyze.rs` for why cross-module SCCs are impossible.
    pub analysis: Option<crate::ir::AnalysisResult>,
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
    /// Functions that call themselves directly or transitively. Set-form
    /// union of `entry_analysis.recursive_fns` plus each module's
    /// `analysis.recursive_fns`. Used by codegen sites that previously
    /// called `call_graph::find_recursive_fns` ad-hoc (Lean recursion
    /// planning, type checker flow, etc.).
    pub recursive_fns: HashSet<String>,
    /// Per-fn analysis facts unioned from entry + every dep module's
    /// `AnalysisResult.fn_analyses`. WASM emitter / VM compiler /
    /// future inliner read `allocates`, `thin_kind`, `body_shape`,
    /// `local_count`, etc. from here instead of recomputing.
    pub fn_analyses: HashMap<String, crate::ir::FnAnalysis>,
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
    /// Synthesized `<fn>__buffered` variants for every buffer-build
    /// sink, produced by `ir::synthesize_buffered_variants`. These are
    /// real `FnDef`s with proper body AST; backends iterate over them
    /// alongside `fn_defs` so they reach codegen through the same
    /// pipeline (TCO / no-alloc / mutual-recursion all apply
    /// identically). Empty when no sinks are detected.
    pub synthesized_buffered_fns: Vec<FnDef>,
}

/// Output files from a codegen backend.
pub struct ProjectOutput {
    /// Files to write: (relative_path, content).
    pub files: Vec<(String, String)>,
}

/// Build a CodegenContext from parsed + type-checked items.
///
/// `entry_analysis` is the `analyze` stage output for `items` (entry
/// module). When provided, codegen reads `mutual_tco_members`,
/// `recursive_fns`, and per-fn `FnAnalysis` from it instead of recomputing.
/// Each `ModuleInfo` in `modules` carries its own per-module analysis;
/// codegen unions the per-module sets to build a global view (sound
/// under Aver's module DAG invariant — no cross-module SCCs possible,
/// see `src/ir/analyze.rs` doc).
pub fn build_context(
    items: Vec<TopLevel>,
    tc_result: &TypeCheckResult,
    entry_analysis: Option<&crate::ir::AnalysisResult>,
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

    // Mutual-TCO membership unions per-module sets from the analyze stage
    // (entry's `entry_analysis` + each dep module's `module.analysis`).
    // Aver's module DAG invariant guarantees SCCs never span modules, so
    // a per-module union is the correct global view — see
    // `project_aver_module_dag` memory and `src/ir/analyze.rs` doc.
    //
    // Falls back to ad-hoc `tailcall_scc_components` per module when the
    // analysis isn't supplied (callers that haven't migrated to the
    // pipeline). The fallback path will go away once every entry point
    // runs the canonical pipeline.
    let mut mutual_tco_members: HashSet<String> = HashSet::new();
    match entry_analysis {
        Some(a) => mutual_tco_members.extend(a.mutual_tco_members.iter().cloned()),
        None => {
            let entry_fns: Vec<&FnDef> = fn_defs.iter().filter(|fd| fd.name != "main").collect();
            for group in crate::call_graph::tailcall_scc_components(&entry_fns) {
                if group.len() < 2 {
                    continue;
                }
                for fd in group {
                    mutual_tco_members.insert(fd.name.clone());
                }
            }
        }
    }
    for module in &modules {
        match module.analysis.as_ref() {
            Some(a) => mutual_tco_members.extend(a.mutual_tco_members.iter().cloned()),
            None => {
                let mod_fns: Vec<&FnDef> = module.fn_defs.iter().collect();
                for group in crate::call_graph::tailcall_scc_components(&mod_fns) {
                    if group.len() < 2 {
                        continue;
                    }
                    for fd in group {
                        mutual_tco_members.insert(fd.name.clone());
                    }
                }
            }
        }
    }

    // Per-fn analysis dictionary — union of entry's `fn_analyses` plus
    // each dep module's. Codegen reads `allocates`, `thin_kind`, etc.
    // from here instead of recomputing.
    let mut fn_analyses: HashMap<String, crate::ir::FnAnalysis> = HashMap::new();
    if let Some(a) = entry_analysis {
        for (name, fa) in &a.fn_analyses {
            fn_analyses.insert(name.clone(), fa.clone());
        }
    }
    for module in &modules {
        if let Some(a) = module.analysis.as_ref() {
            for (name, fa) in &a.fn_analyses {
                fn_analyses
                    .entry(name.clone())
                    .or_insert_with(|| fa.clone());
            }
        }
    }

    // `recursive_fns` follows the same shape as `mutual_tco_members` —
    // per-module sets unioned (Aver's module DAG keeps cross-module
    // recursion from existing). Falls back to ad-hoc `find_recursive_fns`
    // when a module's analysis is missing.
    let mut recursive_fns: HashSet<String> = HashSet::new();
    match entry_analysis {
        Some(a) => recursive_fns.extend(a.recursive_fns.iter().cloned()),
        None => {
            recursive_fns.extend(crate::call_graph::find_recursive_fns(&items));
        }
    }
    for module in &modules {
        match module.analysis.as_ref() {
            Some(a) => recursive_fns.extend(a.recursive_fns.iter().cloned()),
            None => {
                let mod_items: Vec<TopLevel> = module
                    .fn_defs
                    .iter()
                    .map(|fd| TopLevel::FnDef(fd.clone()))
                    .collect();
                recursive_fns.extend(crate::call_graph::find_recursive_fns(&mod_items));
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

    // Detection layer for buffer-build sinks + fusion sites. The
    // ACTUAL rewrite + synthesis must happen BEFORE the resolver
    // pass (callers run it via `ir::run_buffer_build_pass` between
    // TCO and resolver) — the detector matches on `Expr::Ident`
    // shapes that resolver later rewrites to `Expr::Resolved`. We
    // rerun detection here against the final items so the resulting
    // ctx fields reflect what's actually in the AST. With pre-
    // resolver pass having already run, sinks/sites should be the
    // same set (sinks are fns, not call sites; fusion sites were
    // rewritten away so the post-rewrite count is zero in normal flow).
    let detect_fns: Vec<&FnDef> = fn_defs
        .iter()
        .chain(modules.iter().flat_map(|m| m.fn_defs.iter()))
        .collect();
    let buffer_build_sinks = crate::ir::compute_buffer_build_sinks(&detect_fns);
    let buffer_fusion_sites = crate::ir::find_fusion_sites(&detect_fns, &buffer_build_sinks);
    // The synthesizer already ran in the pre-resolver compile pass
    // (`ir::run_buffer_build_pass`); the resulting `<fn>__buffered`
    // variants live in `items` (or in dep `module.fn_defs`) directly,
    // so we just collect references for the ctx field instead of
    // re-synthesizing — re-running here would duplicate every fn
    // and confuse the WASM emitter's fn_indices table.
    let synthesized_buffered_fns: Vec<FnDef> = fn_defs
        .iter()
        .chain(modules.iter().flat_map(|m| m.fn_defs.iter()))
        .filter(|fd| fd.name.ends_with("__buffered"))
        .cloned()
        .collect();
    // 0.15 Traversal — register signatures for the four buffer-build
    // internal intrinsics. Without these in fn_sigs, downstream
    // `infer_aver_type` on `__buf_append(...)` etc. returns None and
    // `expr_is_heap_ptr` falls through to false — meaning TCO
    // compaction doesn't retain the buffer pointer across GC, the
    // buffer object gets relocated by collect_end, and the next
    // iteration reads through the stale pointer producing
    // `memory access out of bounds` traps. Buffer parses to
    // Type::Named("Buffer") which is_heap_type accepts.
    {
        let buffer_ty = || crate::types::Type::Named("Buffer".to_string());
        let str_ty = || crate::types::Type::Str;
        let int_ty = || crate::types::Type::Int;
        let intrinsic_sigs: &[(&str, Vec<crate::types::Type>, crate::types::Type)] = &[
            ("__buf_new", vec![int_ty()], buffer_ty()),
            ("__buf_append", vec![buffer_ty(), str_ty()], buffer_ty()),
            (
                "__buf_append_sep_unless_first",
                vec![buffer_ty(), str_ty()],
                buffer_ty(),
            ),
            ("__buf_finalize", vec![buffer_ty()], str_ty()),
        ];
        for (name, params, ret) in intrinsic_sigs {
            fn_sigs.insert(name.to_string(), (params.clone(), ret.clone(), vec![]));
        }
    }

    // Inject signatures for synthesized variants into fn_sigs so the
    // WASM emitter's type-section pass produces correct param/return
    // wasm types (the fallback path emits `all-i64` which breaks
    // validation when a body calls intrinsics with i32 buffer ptrs).
    for fd in synthesized_buffered_fns.iter() {
        if fn_sigs.contains_key(&fd.name) {
            continue;
        }
        let param_types: Vec<crate::types::Type> = fd
            .params
            .iter()
            .map(|(_, ty_str)| crate::types::parse_type_str(ty_str))
            .collect();
        let ret = crate::types::parse_type_str(&fd.return_type);
        fn_sigs.insert(
            fd.name.clone(),
            (
                param_types,
                ret,
                fd.effects.iter().map(|e| e.node.clone()).collect(),
            ),
        );
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
        recursive_fns,
        fn_analyses,
        buffer_build_sinks,
        buffer_fusion_sites,
        synthesized_buffered_fns,
    }
}

impl CodegenContext {
    /// Recompute `mutual_tco_members` and `recursive_fns` from current
    /// `items` + `modules`. Used by test helpers that build the context
    /// piecewise (push items in-place, bypass `build_context`) so the
    /// derived sets stay in sync. Idempotent — production callers go
    /// through `build_context`, where these are already populated from
    /// the analyze stage; calling `refresh_facts` again is a no-op for
    /// them (computes the same answer).
    pub fn refresh_facts(&mut self) {
        let entry_fn_refs: Vec<&FnDef> =
            self.fn_defs.iter().filter(|fd| fd.name != "main").collect();

        let mut mutual_tco_members: HashSet<String> = HashSet::new();
        for group in crate::call_graph::tailcall_scc_components(&entry_fn_refs) {
            if group.len() < 2 {
                continue;
            }
            for fd in group {
                mutual_tco_members.insert(fd.name.clone());
            }
        }
        for module in &self.modules {
            let mod_fns: Vec<&FnDef> = module.fn_defs.iter().collect();
            for group in crate::call_graph::tailcall_scc_components(&mod_fns) {
                if group.len() < 2 {
                    continue;
                }
                for fd in group {
                    mutual_tco_members.insert(fd.name.clone());
                }
            }
        }
        self.mutual_tco_members = mutual_tco_members;

        let mut recursive_fns: HashSet<String> = crate::call_graph::find_recursive_fns(&self.items);
        for module in &self.modules {
            let mod_items: Vec<TopLevel> = module
                .fn_defs
                .iter()
                .map(|fd| TopLevel::FnDef(fd.clone()))
                .collect();
            recursive_fns.extend(crate::call_graph::find_recursive_fns(&mod_items));
        }
        self.recursive_fns = recursive_fns;
    }
}

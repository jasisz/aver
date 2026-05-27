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
pub mod proof_lower;
#[cfg(feature = "runtime")]
pub mod recursion;
#[cfg(feature = "runtime")]
pub mod rust;
pub mod scc;
#[cfg(feature = "wasip2")]
pub mod wasip2;
#[cfg(feature = "wasm-compile")]
pub mod wasm_gc;

use std::collections::{HashMap, HashSet};

use crate::ast::{FnDef, TopLevel, TypeDef};
use crate::source::LoadedModule;
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

impl ModuleInfo {
    /// Build a [`ModuleInfo`] from a freshly-parsed [`LoadedModule`].
    /// Skips the analyze stage — callers that need per-dep analysis
    /// facts should run the pipeline themselves (see
    /// `crate::main::commands::load_compile_deps` /
    /// `playground::loaded_to_module_info`). Used by ad-hoc loaders
    /// (`vm_profile`, the eval-spec test helpers) that just need the
    /// dep's symbol layout to feed `SymbolTable::build` /
    /// `pipeline::run`'s `dep_modules` slot.
    pub fn from_loaded(loaded: &LoadedModule) -> Self {
        let depends = loaded
            .items
            .iter()
            .find_map(|i| match i {
                TopLevel::Module(m) => Some(m.depends.clone()),
                _ => None,
            })
            .unwrap_or_default();
        let type_defs = loaded
            .items
            .iter()
            .filter_map(|i| match i {
                TopLevel::TypeDef(td) => Some(td.clone()),
                _ => None,
            })
            .collect();
        let fn_defs = loaded
            .items
            .iter()
            .filter_map(|i| match i {
                TopLevel::FnDef(fd) if fd.name != "main" => Some(fd.clone()),
                _ => None,
            })
            .collect();
        Self {
            prefix: loaded.dep_name.clone(),
            depends,
            type_defs,
            fn_defs,
            analysis: None,
        }
    }
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
    /// Functions that are part of a mutual-TCO SCC group (emitted as
    /// trampoline + wrappers). Functions NOT in this set but with
    /// TailCalls are emitted as plain self-TCO loops. Keyed by opaque
    /// [`crate::ir::FnId`] from the symbol table — entry-module fns
    /// and dep-module fns with the same bare name can't accidentally
    /// merge under bare-name keying.
    pub mutual_tco_members: HashSet<crate::ir::FnId>,
    /// Functions that call themselves directly or transitively. Set-
    /// form union of `entry_analysis.recursive_fns` plus each
    /// module's `analysis.recursive_fns`. Keyed by opaque
    /// [`crate::ir::FnId`] — same disambiguation guarantee as
    /// `mutual_tco_members`. Used by codegen sites that previously
    /// called `call_graph::find_recursive_fns` ad-hoc.
    pub recursive_fns: HashSet<crate::ir::FnId>,
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
    /// Proof-export decision IR populated by `proof_lower::lower`
    /// during `build_context`. Backends (Lean, Dafny) read from
    /// here to decide refinement-record lift, recursion contracts,
    /// law-theorem shape, etc. Single source of truth — both
    /// backends see the same decisions so cross-backend drift
    /// becomes impossible at the shape level. Step 2: only
    /// `refined_types` is populated; backends still consume legacy
    /// `refinement_info_for` for now. Step 3+ migrates backends.
    #[cfg(feature = "runtime")]
    pub proof_ir: crate::ir::ProofIR,
    /// Resolved-identity table (#138 phase E). Always populated:
    /// `pipeline::run` builds it unconditionally and threads it
    /// through `build_context`. Consumers (proof IR lookups,
    /// backend FnId/TypeId resolution) read it directly — no
    /// `Option` wrapper to unwrap at each callsite.
    pub symbol_table: crate::ir::SymbolTable,
    /// Resolved-HIR forms of every entry-scope fn in `fn_defs`,
    /// in the same source order. Lifted by `build_context` via
    /// [`crate::ir::hir::resolve_fn_def_external`] against the
    /// entry's [`ResolveCtx`]; #147 phase E PR 8 consumers (the
    /// Rust backend's expression / pattern emitters) walk these
    /// instead of the source-shape bodies in `fn_defs`. Items
    /// whose name doesn't resolve through the symbol table (only
    /// possible when typecheck rejected the program upstream)
    /// are skipped from this list — codegen falls back to
    /// `fn_defs` in those cases.
    pub resolved_fn_defs: Vec<crate::ir::hir::ResolvedFnDef>,
    /// Per-dep resolved fn defs, parallel to `modules`. Each
    /// entry in `resolved_module_fn_defs[i]` corresponds to
    /// `modules[i].fn_defs` in source order — same skip-on-missing
    /// rule as `resolved_fn_defs`. Lifted against the *entry's*
    /// symbol table (pinned to the dep's prefix as the resolver's
    /// `current_module`) so every scope shares a single `FnId` /
    /// `TypeId` namespace, matching the VM compiler's
    /// `integrate_module` shape.
    pub resolved_module_fn_defs: Vec<Vec<crate::ir::hir::ResolvedFnDef>>,
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
///
/// `symbol_table` is the resolved-identity layer built by the
/// pipeline (`pipeline_result.symbol_table`). Always required:
/// `pipeline::run` builds it unconditionally so every caller has
/// one available. The ad-hoc test helpers that drive a stripped
/// pipeline build their own via `SymbolTable::build(&items,
/// &modules)` and pass it here.
pub fn build_context(
    items: Vec<TopLevel>,
    tc_result: &TypeCheckResult,
    entry_analysis: Option<&crate::ir::AnalysisResult>,
    memo_fns: HashSet<String>,
    project_name: String,
    modules: Vec<ModuleInfo>,
    symbol_table: crate::ir::SymbolTable,
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

    // Mutual-TCO membership unions per-scope sets from the analyze
    // stage (entry's `entry_analysis` + each dep module's
    // `module.analysis`); falls back to recomputing per-scope via
    // `call_graph::tailcall_scc_components` when no analysis ran.
    // Aver's module DAG invariant guarantees SCCs never span
    // modules — per-scope union is the correct global view (see
    // `project_aver_module_dag` memory + `src/ir/analyze.rs`). The
    // FnId resolution happens inside the `scc` wrappers below.
    let mut mutual_tco_members: HashSet<crate::ir::FnId> = HashSet::new();
    match entry_analysis {
        Some(a) => mutual_tco_members.extend(scc::analysis_set_to_fn_ids(
            &a.mutual_tco_members,
            &symbol_table,
            None,
        )),
        None => {
            // No entry analysis: compute the per-scope SCC set inline
            // via `call_graph` and project to FnIds. Same effect as
            // running the analyze stage's mutual-TCO discovery.
            let entry_fns: Vec<&FnDef> = fn_defs.iter().filter(|fd| fd.name != "main").collect();
            for group in crate::call_graph::tailcall_scc_components(&entry_fns) {
                if group.len() < 2 {
                    continue;
                }
                for fd in group {
                    if let Some(id) = symbol_table.fn_id_of(&crate::ir::FnKey::entry(&fd.name)) {
                        mutual_tco_members.insert(id);
                    }
                }
            }
        }
    }
    for module in &modules {
        match module.analysis.as_ref() {
            Some(a) => mutual_tco_members.extend(scc::analysis_set_to_fn_ids(
                &a.mutual_tco_members,
                &symbol_table,
                Some(&module.prefix),
            )),
            None => {
                let mod_fns: Vec<&FnDef> = module.fn_defs.iter().collect();
                for group in crate::call_graph::tailcall_scc_components(&mod_fns) {
                    if group.len() < 2 {
                        continue;
                    }
                    for fd in group {
                        if let Some(id) = symbol_table.fn_id_of(&crate::ir::FnKey::in_module(
                            module.prefix.clone(),
                            &fd.name,
                        )) {
                            mutual_tco_members.insert(id);
                        }
                    }
                }
            }
        }
    }

    // `recursive_fns` follows the same shape — per-scope union with
    // analyze-stage fallback. Keyed by opaque `FnId` so entry +
    // dep-module same-bare-name fns stay distinct.
    let mut recursive_fns: HashSet<crate::ir::FnId> = HashSet::new();
    match entry_analysis {
        Some(a) => recursive_fns.extend(scc::analysis_set_to_fn_ids(
            &a.recursive_fns,
            &symbol_table,
            None,
        )),
        None => recursive_fns.extend(scc::bare_names_to_fn_ids(
            crate::call_graph::find_recursive_fns(&items)
                .iter()
                .map(String::as_str),
            &symbol_table,
            None,
        )),
    }
    for module in &modules {
        match module.analysis.as_ref() {
            Some(a) => recursive_fns.extend(scc::analysis_set_to_fn_ids(
                &a.recursive_fns,
                &symbol_table,
                Some(&module.prefix),
            )),
            None => {
                let mod_items: Vec<TopLevel> = module
                    .fn_defs
                    .iter()
                    .map(|fd| TopLevel::FnDef(fd.clone()))
                    .collect();
                recursive_fns.extend(scc::bare_names_to_fn_ids(
                    crate::call_graph::find_recursive_fns(&mod_items)
                        .iter()
                        .map(String::as_str),
                    &symbol_table,
                    Some(&module.prefix),
                ));
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
        let buffer_ty = || crate::types::Type::named("Buffer");
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

    // Resolved-HIR lift of every fn def (entry + each dep), against
    // the entry's symbol table. The Rust backend (#147 phase E PR 8)
    // consumes these from `ctx.resolved_fn_defs` /
    // `ctx.resolved_module_fn_defs`; tests + helper paths that build
    // a ctx piecewise refresh them via `CodegenContext::refresh_facts`.
    let resolved_fn_defs: Vec<crate::ir::hir::ResolvedFnDef> = {
        let entry_ctx = crate::ir::hir::ResolveCtx::new(&symbol_table);
        fn_defs
            .iter()
            .filter_map(|fd| crate::ir::hir::resolve_fn_def_external(&entry_ctx, fd))
            .collect()
    };
    let resolved_module_fn_defs: Vec<Vec<crate::ir::hir::ResolvedFnDef>> = modules
        .iter()
        .map(|module| {
            let mut module_ctx = crate::ir::hir::ResolveCtx::new(&symbol_table);
            module_ctx.current_module = Some(module.prefix.clone());
            module
                .fn_defs
                .iter()
                .filter_map(|fd| crate::ir::hir::resolve_fn_def_external(&module_ctx, fd))
                .collect()
        })
        .collect();

    let ctx = CodegenContext {
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
        buffer_build_sinks,
        buffer_fusion_sites,
        synthesized_buffered_fns,
        #[cfg(feature = "runtime")]
        proof_ir: crate::ir::ProofIR::default(),
        // Symbol table threaded through from the pipeline (or
        // built locally in fallback). The FnId-keyed `recursive_
        // fns` / `mutual_tco_members` above used it; backends
        // (proof_lower / Lean / Rust / Dafny) read it directly off
        // ctx for opaque-ID lookups.
        symbol_table,
        resolved_fn_defs,
        resolved_module_fn_defs,
    };
    // ProofIR no longer populated here. Pipeline owns the lowerings
    // (`PipelineStage::RefinementLower`, `PipelineStage::ContractLower`);
    // proof backends opt in via `PipelineConfig.run_refinement_lower` /
    // `run_contract_lower` and read `pipeline_result.proof_ir` back.
    // Runtime backends (VM / WASM / Rust) leave both off and skip the
    // work. Tests that bypass the pipeline assemble the ctx by hand
    // and call `refresh_facts()` to populate the field — the field
    // stays `default()` here for those callers until they explicitly
    // refresh.
    ctx
}

impl CodegenContext {
    /// Test-only bridge: recompute every derived fact
    /// (`mutual_tco_members`, `recursive_fns`, `proof_ir`) from the
    /// current `items` and `modules`. Used exclusively by unit tests
    /// that construct a `CodegenContext` piecewise — pushing synthetic
    /// `FnDef`s straight into the items list rather than going through
    /// the parser and pipeline. Production code never needs this:
    /// every derived fact is populated by the pipeline stages
    /// (analyze, proof_lower) and propagated through `build_context`.
    /// Calling `refresh_facts` on a production-built ctx is redundant
    /// work that produces the same answer — leave it off the hot path.
    pub fn refresh_facts(&mut self) {
        // Synthetic-ctx path must own its symbol table too — FnId-
        // keyed sets below resolve through it, same shape as the
        // production `build_context` flow.
        let symbol_table = crate::ir::SymbolTable::build(&self.items, &self.modules);
        let entry_fn_id = |name: &str| -> Option<crate::ir::FnId> {
            symbol_table.fn_id_of(&crate::ir::FnKey::entry(name))
        };
        let module_fn_id = |prefix: &str, name: &str| -> Option<crate::ir::FnId> {
            symbol_table.fn_id_of(&crate::ir::FnKey::in_module(prefix.to_string(), name))
        };

        let entry_fn_refs: Vec<&FnDef> =
            self.fn_defs.iter().filter(|fd| fd.name != "main").collect();

        let mut mutual_tco_members: HashSet<crate::ir::FnId> = HashSet::new();
        for group in crate::call_graph::tailcall_scc_components(&entry_fn_refs) {
            if group.len() < 2 {
                continue;
            }
            for fd in group {
                if let Some(id) = entry_fn_id(&fd.name) {
                    mutual_tco_members.insert(id);
                }
            }
        }
        for module in &self.modules {
            let mod_fns: Vec<&FnDef> = module.fn_defs.iter().collect();
            for group in crate::call_graph::tailcall_scc_components(&mod_fns) {
                if group.len() < 2 {
                    continue;
                }
                for fd in group {
                    if let Some(id) = module_fn_id(&module.prefix, &fd.name) {
                        mutual_tco_members.insert(id);
                    }
                }
            }
        }
        self.mutual_tco_members = mutual_tco_members;

        let mut recursive_fns: HashSet<crate::ir::FnId> = scc::bare_names_to_fn_ids(
            crate::call_graph::find_recursive_fns(&self.items)
                .iter()
                .map(String::as_str),
            &symbol_table,
            None,
        );
        for module in &self.modules {
            let mod_items: Vec<TopLevel> = module
                .fn_defs
                .iter()
                .map(|fd| TopLevel::FnDef(fd.clone()))
                .collect();
            recursive_fns.extend(scc::bare_names_to_fn_ids(
                crate::call_graph::find_recursive_fns(&mod_items)
                    .iter()
                    .map(String::as_str),
                &symbol_table,
                Some(&module.prefix),
            ));
        }
        self.recursive_fns = recursive_fns;

        // Reuse the symbol table built at the top of this function
        // for proof_lower below — it already resolved every FnId we
        // need for `recursive_fns` / `mutual_tco_members`.
        self.symbol_table = symbol_table;

        // Refresh the resolved-HIR mirror of every fn def so the
        // backend (#147 phase E PR 8) sees the same shape
        // `build_context` would have produced from scratch.
        let entry_resolve_ctx = crate::ir::hir::ResolveCtx::new(&self.symbol_table);
        self.resolved_fn_defs = self
            .fn_defs
            .iter()
            .filter_map(|fd| crate::ir::hir::resolve_fn_def_external(&entry_resolve_ctx, fd))
            .collect();
        self.resolved_module_fn_defs = self
            .modules
            .iter()
            .map(|module| {
                let mut module_ctx = crate::ir::hir::ResolveCtx::new(&self.symbol_table);
                module_ctx.current_module = Some(module.prefix.clone());
                module
                    .fn_defs
                    .iter()
                    .filter_map(|fd| crate::ir::hir::resolve_fn_def_external(&module_ctx, fd))
                    .collect()
            })
            .collect();

        // ProofIR's `fn_contracts` / `refined_types` are derived from
        // the just-recomputed item set + the recursion classifier, so
        // they must stay in step with the rest of the facts. Test
        // helpers that build the context piecewise and call
        // `refresh_facts` rely on this to see the same proof decisions
        // the production pipeline would emit.
        let inputs = crate::codegen::proof_lower::ProofLowerInputs::from_ctx(self);
        self.proof_ir = crate::codegen::proof_lower::lower(&inputs);
    }

    /// Look up the resolved-HIR mirror of a source-shape [`FnDef`]
    /// previously stashed in [`resolved_fn_defs`] /
    /// [`resolved_module_fn_defs`]. Falls back to a fresh per-call
    /// resolver lift against the entry's [`crate::ir::SymbolTable`]
    /// when neither path covers `fd` — this happens for synthetic
    /// FnDefs inserted between `build_context` and emit (memo
    /// wrappers, TCO hoist rewrites, test fixtures) which the
    /// resolver hasn't lifted upfront.
    ///
    /// `scope` is the owning module prefix when `fd` came from a
    /// dependency module's `module.fn_defs`, `None` when `fd` is part
    /// of the entry's `ctx.fn_defs`. Lookup keys by
    /// [`crate::ir::FnKey`] through the [`crate::ir::SymbolTable`] so
    /// two modules that share a bare fn name (e.g. `Util.format` and
    /// `Other.format`) resolve to their own [`crate::ir::FnId`]
    /// without bare-name collisions. Pre-PR-9.3a this matched by
    /// `rfd.name == fd.name` against a flat search of every resolved
    /// table — fragile the moment flatten changes (or doesn't run)
    /// and two scopes share a name.
    ///
    /// Phase E shared lookup boundary — Rust codegen (PR 8) already
    /// consumes this through `rust::toplevel::resolved_fn_def_for`;
    /// wasm-gc / Lean / Dafny / self-host backends pick it up in
    /// their follow-up PRs.
    ///
    /// [`resolved_fn_defs`]: Self::resolved_fn_defs
    /// [`resolved_module_fn_defs`]: Self::resolved_module_fn_defs
    pub fn resolve_fn_def<'a>(
        &'a self,
        fd: &'a FnDef,
        scope: Option<&str>,
    ) -> std::borrow::Cow<'a, crate::ir::hir::ResolvedFnDef> {
        use crate::ir::FnKey;
        use crate::ir::hir::{
            ResolveCtx, ResolvedFnBody, ResolvedFnDef, ResolvedStmt, resolve_fn_def_external,
        };
        use std::borrow::Cow;

        // Resolve identity via the symbol table — entry scope vs
        // dependency module scope is the caller's stated context.
        let key = match scope {
            Some(prefix) => FnKey::in_module(prefix.to_string(), fd.name.clone()),
            None => FnKey::entry(fd.name.clone()),
        };
        if let Some(fn_id) = self.symbol_table.fn_id_of(&key) {
            // Resolved tables are dense in the order each scope's
            // `build_context` populated them; the only authoritative
            // way to find the right slot is `fn_id` equality.
            if let Some(rfd) = self.resolved_fn_defs.iter().find(|rfd| rfd.fn_id == fn_id) {
                return Cow::Borrowed(rfd);
            }
            for module in &self.resolved_module_fn_defs {
                if let Some(rfd) = module.iter().find(|rfd| rfd.fn_id == fn_id) {
                    return Cow::Borrowed(rfd);
                }
            }
            // Symbol table knew the key but resolver didn't lift a
            // resolved entry. Falls through to the synthetic-fallback
            // path below; in production this shouldn't happen.
        }

        // Synthetic FnDef path — memo wrappers, TCO hoist rewrites,
        // test fixtures the resolver never saw. Lift on demand
        // against the entry's resolver context.
        let module_name = self.items.iter().find_map(|i| match i {
            TopLevel::Module(m) => Some(m.name.clone()),
            _ => None,
        });
        let mut rctx = ResolveCtx::new(&self.symbol_table);
        rctx.current_module = scope.map(String::from).or(module_name);
        let lifted = resolve_fn_def_external(&rctx, fd).unwrap_or_else(|| {
            let stmts: Vec<ResolvedStmt> = match fd.body.as_ref() {
                crate::ast::FnBody::Block(stmts) => {
                    stmts.iter().map(|s| self.resolve_stmt(s)).collect()
                }
            };
            ResolvedFnDef {
                fn_id: crate::ir::FnId(u32::MAX),
                name: fd.name.clone(),
                line: fd.line,
                params: fd
                    .params
                    .iter()
                    .map(|(n, ann)| (n.clone(), crate::types::parse_type_str(ann)))
                    .collect(),
                return_type: crate::types::parse_type_str(&fd.return_type),
                effects: fd.effects.clone(),
                desc: fd.desc.clone(),
                body: std::sync::Arc::new(ResolvedFnBody::Block(stmts)),
                resolution: fd.resolution.clone(),
            }
        });
        Cow::Owned(lifted)
    }

    /// Resolve a source-shape `Spanned<Expr>` on demand using the
    /// entry's resolver context. Used by emit helpers that still walk
    /// `Expr` (TCO hoisting, mutual TCO, verify blocks, follow-up
    /// backends pre-migration) and need to feed the resolved shape
    /// into the migrated emitter. The returned `Spanned<ResolvedExpr>`
    /// carries the same line + type stamp as the input.
    pub fn resolve_expr(
        &self,
        expr: &crate::ast::Spanned<crate::ast::Expr>,
    ) -> crate::ast::Spanned<crate::ir::hir::ResolvedExpr> {
        use crate::ir::hir::{ResolveCtx, ResolvedStmt};
        let module_name = self.items.iter().find_map(|i| match i {
            TopLevel::Module(m) => Some(m.name.clone()),
            _ => None,
        });
        let mut rctx = ResolveCtx::new(&self.symbol_table);
        rctx.current_module = module_name;
        let stmt = crate::ast::Stmt::Expr(expr.clone());
        match crate::ir::hir::resolve::resolve_stmt_external(&rctx, &stmt) {
            ResolvedStmt::Expr(s) => s,
            ResolvedStmt::Binding { value, .. } => value,
        }
    }

    /// Same as [`Self::resolve_expr`] but for whole statements
    /// (`Binding(name, ty_ann, expr)` or `Expr(expr)`).
    pub fn resolve_stmt(&self, stmt: &crate::ast::Stmt) -> crate::ir::hir::ResolvedStmt {
        use crate::ir::hir::ResolveCtx;
        let module_name = self.items.iter().find_map(|i| match i {
            TopLevel::Module(m) => Some(m.name.clone()),
            _ => None,
        });
        let mut rctx = ResolveCtx::new(&self.symbol_table);
        rctx.current_module = module_name;
        crate::ir::hir::resolve::resolve_stmt_external(&rctx, stmt)
    }

    /// Resolve a source-shape [`crate::ast::Pattern`] to its resolved
    /// HIR form. Wraps the pattern in a synthetic match arm + drops
    /// it through `resolve_stmt_external`, since the resolver doesn't
    /// expose a standalone pattern lifter — same workaround
    /// `rust/toplevel.rs` used pre-PR-9.
    pub fn resolve_pattern(&self, pat: &crate::ast::Pattern) -> crate::ir::hir::ResolvedPattern {
        use crate::ast::{Expr, Literal, MatchArm, Spanned, Stmt};
        use crate::ir::hir::{ResolveCtx, ResolvedExpr, ResolvedStmt};
        let module_name = self.items.iter().find_map(|i| match i {
            TopLevel::Module(m) => Some(m.name.clone()),
            _ => None,
        });
        let mut rctx = ResolveCtx::new(&self.symbol_table);
        rctx.current_module = module_name;
        let synthetic_arm = MatchArm {
            pattern: pat.clone(),
            body: Box::new(Spanned::bare(Expr::Literal(Literal::Unit))),
            binding_slots: std::sync::OnceLock::new(),
        };
        let stmt = Stmt::Expr(Spanned::bare(Expr::Match {
            subject: Box::new(Spanned::bare(Expr::Literal(Literal::Unit))),
            arms: vec![synthetic_arm],
        }));
        let resolved_stmt = crate::ir::hir::resolve::resolve_stmt_external(&rctx, &stmt);
        let ResolvedStmt::Expr(spanned) = resolved_stmt else {
            unreachable!()
        };
        let ResolvedExpr::Match { arms, .. } = spanned.node else {
            unreachable!()
        };
        arms.into_iter().next().unwrap().pattern
    }
}

mod classify;
mod expr;
pub mod mir;
mod resolve_helpers;

use std::collections::HashMap;

use crate::ast::{Stmt, TopLevel, TypeDef};
use crate::ir::SymbolTable;
use crate::ir::hir::{
    ResolveCtx, ResolvedFnDef, ResolvedStmt, ResolvedTopLevel, resolve_top_level,
};
use crate::nan_value::{Arena, NanValue};
use crate::types::{option, result};
use crate::visibility;

use super::builtin::VmBuiltin;
use super::opcode::*;
use super::symbol::{VmSymbolTable, VmVariantCtor};
use super::types::{CodeStore, FnChunk};

/// Compile a resolved program into bytecode.
///
/// `items` is the entry's resolved HIR (the output of the
/// `NameResolve` pipeline stage). `symbols` is the entry's symbol
/// table — every `ResolvedCallee::Fn(FnId)` / `ResolvedCtor::User`
/// in the resolved tree resolves through it to a canonical name
/// that the VM dispatches against.
///
/// `analysis` carries per-fn `FnAnalysis.allocates` from the
/// pipeline's analyze stage; the VM compiler reads `chunk.no_alloc`
/// from it directly.
///
/// Compile with explicit module root for `depends` resolution.
pub fn compile_program_with_modules(
    items: &[ResolvedTopLevel],
    symbols: &SymbolTable,
    arena: &mut Arena,
    module_root: Option<&str>,
    source_file: &str,
    analysis: Option<&crate::ir::AnalysisResult>,
) -> Result<(CodeStore, Vec<NanValue>), CompileError> {
    compile_program_inner(
        items,
        symbols,
        arena,
        source_file,
        ModuleSource::Disk(module_root),
        analysis,
    )
}

/// Compile using dependency modules that were already parsed off-disk
/// (or out of a virtual filesystem). The browser playground uses this
/// to run multi-file programs without any real fs access.
pub fn compile_program_with_loaded_modules(
    items: &[ResolvedTopLevel],
    symbols: &SymbolTable,
    arena: &mut Arena,
    loaded: Vec<crate::source::LoadedModule>,
    source_file: &str,
    analysis: Option<&crate::ir::AnalysisResult>,
) -> Result<(CodeStore, Vec<NanValue>), CompileError> {
    compile_program_inner(
        items,
        symbols,
        arena,
        source_file,
        ModuleSource::Loaded(loaded),
        analysis,
    )
}

/// Phase 4b of #252: compile with MIR-first dispatch + HIR
/// fallback. Per fn: if the fn's body lowers cleanly to MIR
/// *and* MIR-emit produces bytecode, use that chunk; otherwise
/// fall back to the existing HIR walker. The fallback is
/// deliberate — every fn that lands in `MirVmUnsupported`
/// territory (Match / Try / TailCall / Construct / Record* /
/// Project / List / Tuple / Map / InterpolatedStr /
/// IndependentProduct / builtin callees / first-class fn
/// values) keeps the well-tested HIR shape.
///
/// Same I/O contract as [`compile_program`]; the only
/// difference is the per-fn dispatch.
pub fn compile_program_with_mir_fallback(
    items: &[ResolvedTopLevel],
    symbols: &SymbolTable,
    arena: &mut Arena,
    analysis: Option<&crate::ir::AnalysisResult>,
) -> Result<(CodeStore, Vec<NanValue>), CompileError> {
    // Single-module MIR-default entry, kept for the parity tests
    // (`tests/mir_vm_parity.rs`) that exercise the MIR path without a
    // module root. The optimize pipeline + per-fn MIR dispatch now
    // live in `compile_program_inner` under `use_mir`, shared with the
    // module-aware production entry points.
    compile_program_inner(
        items,
        symbols,
        arena,
        "",
        ModuleSource::Disk(None),
        analysis,
    )
}

enum ModuleSource<'a> {
    Disk(Option<&'a str>),
    Loaded(Vec<crate::source::LoadedModule>),
}

/// Lower a resolved item list to MIR and run the Phase 6 optimize
/// pipeline. Order is deliberate: (1) nullary-literal inlining unlocks
/// call-site literals, (2) const-fold collapses literal arithmetic,
/// (3) algebraic-simplify rewrites Int identities, (4) bool-match-to-if
/// rewrites two-arm `Bool` matches into `IfThenElse`, (5) branch-collapse
/// drops the dead branch of a folded `IfThenElse`, (6) DCE drops unread
/// `let _ = <pure>` chains. Shared by the entry compile and per-dep-module
/// MIR builds so both backends see identical lowered+optimized shapes.
fn build_optimized_mir(
    items: &[ResolvedTopLevel],
    external_callers_possible: bool,
) -> crate::ir::mir::MirProgram {
    let mut lowered = crate::ir::mir::lower_program(items);
    // Mark dependency-module fragments so `own_param_refine` bails: the
    // VM compiles each `depends [...]` module separately, so a dep
    // fragment cannot see the entry/sibling call sites that may alias a
    // param. The entry compile (and the flattened wasm-gc / Rust builds)
    // see all callers, so graduation stays enabled there.
    lowered.external_callers_possible = external_callers_possible;
    crate::ir::mir::optimize(lowered)
}

/// Run the buffer-build deforestation pass over a freshly-parsed
/// dependency module, and keep the result only when the entry's symbol
/// table already knows the `<fn>__buffered` variants it synthesized.
///
/// The VM re-parses every dep off disk (`load_module_tree`), so the pass
/// the caller ran over ITS copy of the module — the copy `SymbolTable`
/// was built from — has to run again here or the VM executes the
/// unfused spelling of code the Rust compile path fuses.
///
/// The symbol-table check is what makes running it here safe for every
/// caller at once. A caller that loaded its deps pristine
/// (`run_buffer_build: false` — the proof exporters and the wasm-gc
/// paths, which have no `Buffer` lowering) never registered a
/// `<dep>.<fn>__buffered` name, and compiling a call to a fn the symbol
/// table has never heard of would resolve to nothing. So: fuse when the
/// caller fused, stay pristine when it didn't. Both shapes compute the
/// same result; only the allocation count differs.
fn adopt_buffer_build_if_symbols_agree(
    dep_name: &str,
    pristine: Vec<TopLevel>,
    entry_symbols: &SymbolTable,
) -> Vec<TopLevel> {
    // Detection is read-only, so ask it first: most dep modules hold no
    // builder at all, and this keeps the common case from copying a
    // module's verify blocks just to throw the copy away.
    let candidates: Vec<&crate::ast::FnDef> = pristine
        .iter()
        .filter_map(|it| match it {
            TopLevel::FnDef(fd) => Some(fd),
            _ => None,
        })
        .collect();
    let has_sink = !crate::ir::compute_buffer_build_sinks(&candidates).is_empty();
    drop(candidates);
    if !has_sink {
        return pristine;
    }

    let mut fused = pristine.clone();
    let report = crate::ir::pipeline::buffer_build(&mut fused);
    if report.synthesized.is_empty() {
        return pristine;
    }
    let every_variant_known = report.synthesized.iter().all(|name| {
        entry_symbols
            .fn_id_of(&crate::ir::FnKey::in_module(dep_name, name.clone()))
            .is_some()
    });
    if every_variant_known { fused } else { pristine }
}

fn compile_program_inner(
    items: &[ResolvedTopLevel],
    symbols: &SymbolTable,
    arena: &mut Arena,
    source_file: &str,
    module_source: ModuleSource<'_>,
    analysis: Option<&crate::ir::AnalysisResult>,
) -> Result<(CodeStore, Vec<NanValue>), CompileError> {
    // MIR-default path. Lower the entry items to MIR and run the
    // optimize pipeline; the per-fn loop below dispatches each fn
    // through the MIR walker and falls back to the HIR walker for
    // shapes outside the MIR subset. Built here — not in the caller —
    // so every module-aware entry point shares one pipeline. Order is
    // deliberate: (1) nullary-literal inlining unlocks call-site
    // literals, (2) const-fold collapses literal arithmetic,
    // (3) algebraic-simplify rewrites Int identities, (4) bool-match-
    // to-if rewrites two-arm `Bool` matches into `IfThenElse`,
    // (5) branch-collapse drops the dead branch of a folded
    // `IfThenElse`, (6) DCE drops unread `let _ = <pure>` chains.
    // Dep-module fns build their own MIR in `integrate_module` (same
    // `build_optimized_mir` pipeline); the per-fn loop there dispatches
    // through the MIR walker with the dep's module scope.
    let mir_built = build_optimized_mir(items, false);
    let mir_program = &mir_built;

    let mut compiler = ProgramCompiler::new();
    compiler.source_file = source_file.to_string();
    compiler.sync_record_field_symbols(arena)?;
    // Oracle v1: `BranchPath.Root` is a nullary value constructor
    // (like `Option.None`). The VM symbol table needs it as a
    // constant pointing at a pre-allocated arena record; this
    // happens here because bootstrap_core_symbols runs before the
    // arena is available.
    compiler.install_branch_path_root_constant(arena)?;

    match module_source {
        ModuleSource::Disk(Some(module_root)) => {
            compiler.load_modules(items, module_root, symbols, arena)?;
        }
        ModuleSource::Disk(None) => {}
        ModuleSource::Loaded(loaded) => {
            for m in loaded {
                compiler.integrate_module(&m.dep_name, m.items, symbols, arena)?;
            }
        }
    }

    for item in items {
        if let ResolvedTopLevel::Passthrough(TopLevel::Stmt(Stmt::Binding(name, _, _))) = item {
            compiler.ensure_global(name);
        }
    }

    for item in items {
        match item {
            ResolvedTopLevel::FnDef(rfd) => {
                compiler.ensure_global(&rfd.name);
                let effect_ids: Vec<u32> = rfd
                    .effects
                    .iter()
                    .map(|effect| compiler.symbols.intern_name(&effect.node))
                    .collect();
                let fn_id = compiler.code.add_function(FnChunk {
                    name: rfd.name.clone(),
                    arity: rfd.params.len() as u8,
                    local_count: 0,
                    code: Vec::new(),
                    constants: Vec::new(),
                    effects: effect_ids,
                    thin: false,
                    parent_thin: false,
                    leaf: false,
                    no_alloc: false,
                    source_file: String::new(),
                    line_table: Vec::new(),
                });
                let symbol_id = compiler.symbols.intern_function(
                    &rfd.name,
                    fn_id,
                    &rfd.effects
                        .iter()
                        .map(|e| e.node.clone())
                        .collect::<Vec<_>>(),
                )?;
                let global_idx = compiler.global_names[&rfd.name];
                compiler.globals[global_idx as usize] = VmSymbolTable::symbol_ref(symbol_id);
            }
            ResolvedTopLevel::Passthrough(TopLevel::TypeDef(td)) => {
                // Current module: register in Arena (no qualified alias needed)
                match td {
                    TypeDef::Product { name, fields, .. } => {
                        let field_names: Vec<String> =
                            fields.iter().map(|(n, _)| n.clone()).collect();
                        arena.register_record_type(name, field_names);
                    }
                    TypeDef::Sum { name, variants, .. } => {
                        let variant_names: Vec<String> =
                            variants.iter().map(|v| v.name.clone()).collect();
                        arena.register_sum_type(name, variant_names);
                    }
                }
                // VM-specific: register type symbols
                compiler.register_type_in_symbols(td, None, arena)?;
            }
            _ => {}
        }
    }

    compiler.register_current_module_namespace(items)?;

    for item in items {
        if let ResolvedTopLevel::FnDef(rfd) = item {
            let fn_id = compiler.code.find(&rfd.name).unwrap();
            // Walk this fn's MIR body into bytecode. MIR is the only VM
            // codegen path — there is no HIR fallback. Every well-formed
            // fn lowers (the full corpus + test suite hit zero
            // rejections); a rejection here means an unsupported shape
            // reached codegen on malformed / typecheck-rejected input, so
            // it surfaces as a hard CompileError.
            // Entry fns resolve through `global_names`; no module scope.
            let entry_scope = HashMap::new();
            let mir_fn = mir_program.fn_by_id(rfd.fn_id).ok_or_else(|| CompileError {
                msg: format!(
                    "internal error: fn `{}` did not lower to MIR (an unsupported shape reached the VM backend)",
                    rfd.name
                ),
            })?;
            let chunk = compiler
                .compile_fn_via_mir(rfd, mir_fn, symbols, arena, &entry_scope, mir_program)
                .map_err(|e| e.into_compile_error(&format!("fn `{}`", rfd.name)))?;
            compiler.code.functions[fn_id as usize] = chunk;
        }
    }

    compiler.compile_top_level(items, symbols, arena, mir_program)?;
    compiler.code.symbols = compiler.symbols.clone();
    classify::classify_thin_functions(&mut compiler.code, arena)?;

    // Lowering-level no-alloc analysis driven by the supplied
    // analysis. The pre-Phase-E in-place `compute_alloc_info`
    // fallback assumed access to the original `FnDef` shape;
    // after migration the VM compiler no longer holds those
    // (resolved fn defs carry typed params, not source strings),
    // so the fallback path becomes a conservative "assume yes".
    // Every production caller passes `Some(analysis)` so the
    // optimisation is reached on every real path.
    let allocates = |name: &str| -> bool {
        if let Some(a) = analysis
            && let Some(fa) = a.fn_analyses.get(name)
            && let Some(b) = fa.allocates
        {
            return b;
        }
        true
    };
    for item in items {
        if let ResolvedTopLevel::FnDef(rfd) = item
            && !allocates(&rfd.name)
            && let Some(fn_id) = compiler.code.find(&rfd.name)
        {
            let chunk = &mut compiler.code.functions[fn_id as usize];
            chunk.no_alloc = true;
            // No-alloc bodies always satisfy `can_fast_return`'s
            // runtime length-equality guards, so promote them into
            // the thin fast-return class. The bytecode classifier
            // rejected them for unrelated reasons (mutual TCO call,
            // body size > MAX_PARENT_THIN, etc.) but for return
            // purposes there's nothing left to do.
            chunk.thin = true;
        }
    }

    Ok((compiler.code, compiler.globals))
}

#[derive(Debug)]
pub struct CompileError {
    pub msg: String,
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Compile error: {}", self.msg)
    }
}

/// Iron — B2: lift `SymbolError` into the compiler's diagnostic
/// channel. `VmSymbolTable` used to `panic!` on every kind clash;
/// the conversion lets the compile path surface the same condition
/// as a regular `CompileError` instead of aborting the process.
impl From<crate::vm::symbol::SymbolError> for CompileError {
    fn from(err: crate::vm::symbol::SymbolError) -> Self {
        CompileError {
            msg: err.to_string(),
        }
    }
}

struct ProgramCompiler {
    code: CodeStore,
    symbols: VmSymbolTable,
    globals: Vec<NanValue>,
    global_names: HashMap<String, u16>,
    /// Source file path for the main program (propagated to FnChunks).
    source_file: String,
}

impl ProgramCompiler {
    fn new() -> Self {
        let mut compiler = ProgramCompiler {
            code: CodeStore::new(),
            symbols: VmSymbolTable::default(),
            globals: Vec::new(),
            global_names: HashMap::new(),
            source_file: String::new(),
        };
        // bootstrap into a fresh `VmSymbolTable` populates well-known
        // builtins / wrappers / namespaces; nothing it inserts can
        // clash with prior state, so a `SymbolError` here would be a
        // bug in the bootstrap data, not a user-input failure.
        compiler
            .bootstrap_core_symbols()
            .expect("bootstrap_core_symbols on empty VmSymbolTable cannot fail");
        compiler
    }

    fn sync_record_field_symbols(&mut self, arena: &Arena) -> Result<(), CompileError> {
        for type_id in 0..arena.type_count() {
            let type_name = arena.get_type_name(type_id);
            self.symbols.intern_namespace_path(type_name)?;
            let field_names = arena.get_field_names(type_id);
            if field_names.is_empty() {
                continue;
            }
            let field_symbol_ids: Vec<u32> = field_names
                .iter()
                .map(|field_name| self.symbols.intern_name(field_name))
                .collect();
            self.code.register_record_fields(type_id, &field_symbol_ids);
        }
        Ok(())
    }

    /// Load all modules from `depends [...]` declarations using the shared loader,
    /// then compile each module's functions and register symbols.
    fn load_modules(
        &mut self,
        items: &[ResolvedTopLevel],
        module_root: &str,
        entry_symbols: &SymbolTable,
        arena: &mut Arena,
    ) -> Result<(), CompileError> {
        let module = items.iter().find_map(|i| match i {
            ResolvedTopLevel::Module(m) => Some(m),
            _ => None,
        });
        let module = match module {
            Some(m) => m,
            None => return Ok(()),
        };

        let modules = crate::source::load_module_tree(&module.depends, module_root)
            .map_err(|e| CompileError { msg: e })?;

        for loaded in modules {
            self.integrate_module(&loaded.dep_name, loaded.items, entry_symbols, arena)?;
        }
        Ok(())
    }

    /// Integrate a loaded module into the VM: register types, compile functions,
    /// expose symbols.
    ///
    /// Resolves dep items against the entry's `SymbolTable` so every
    /// scope shares the same `FnId` / `TypeId` namespace — the VM no
    /// longer owns a parallel resolver. Callers ensure the entry
    /// pipeline ran with `dep_modules` populated so `entry_symbols`
    /// knows about every transitive dep before this is invoked
    /// (`cmd_run_vm`, `cmd_compile_aver`, and tests via
    /// `load_compile_deps`).
    fn integrate_module(
        &mut self,
        dep_name: &str,
        mut mod_items: Vec<TopLevel>,
        entry_symbols: &SymbolTable,
        arena: &mut Arena,
    ) -> Result<(), CompileError> {
        // Caller already ran the full canonical pipeline on the entry,
        // including BuildSymbols + NameResolve over `dep_modules`. We
        // still need TCO + slot-resolve on the freshly-parsed dep
        // items so the body shape matches what the entry's resolver
        // saw (TCO rewrites tail calls; the slot resolver allocates
        // local slots both passes rely on).
        crate::ir::pipeline::tco(&mut mod_items);
        mod_items = adopt_buffer_build_if_symbols_agree(dep_name, mod_items, entry_symbols);
        crate::ir::pipeline::resolve(&mut mod_items);

        // Dependency types use qualified canonical keys. Their bare display
        // spellings remain fallback aliases for the Value/JSON boundary.
        for mt in visibility::collect_module_types(&mod_items) {
            let qualified = visibility::qualified_name(dep_name, &mt.bare_name);
            let type_id =
                match &mt.kind {
                    visibility::ModuleTypeKind::Record { field_names } => arena
                        .register_record_type_keyed(&qualified, &mt.bare_name, field_names.clone()),
                    visibility::ModuleTypeKind::Sum { variant_names } => arena
                        .register_sum_type_keyed(&qualified, &mt.bare_name, variant_names.clone()),
                };
            arena.register_type_alias(&mt.bare_name, type_id);
        }
        for item in &mod_items {
            if let TopLevel::TypeDef(td) = item {
                self.register_type_in_symbols(td, Some(dep_name), arena)?;
            }
        }

        // Lift dep items into resolved HIR against the *entry's*
        // symbol table — keeps a single, unified `FnId` / `TypeId`
        // namespace across the whole compile unit. Pin the
        // resolver's `current_module` to the canonical dep_name so
        // intra-dep call shapes (`Foo.bar`, bare `bar`) match the
        // `FnKey::in_module(dep_name, _)` rows the entry pipeline
        // already inserted, regardless of the dep's source-declared
        // leaf name (`module Ast` inside `Domain.Ast.av`).
        let mut ctx = ResolveCtx::new(entry_symbols);
        ctx.current_module = Some(dep_name.to_string());
        let dep_resolved: Vec<ResolvedTopLevel> = mod_items
            .iter()
            .map(|i| resolve_top_level(&ctx, i))
            .collect();

        // Compile ALL functions (not just exposed).
        let mut module_fn_ids: Vec<(String, u32)> = Vec::new();
        for item in &dep_resolved {
            if let ResolvedTopLevel::FnDef(rfd) = item {
                let qualified_name = visibility::qualified_name(dep_name, &rfd.name);
                let effect_ids: Vec<u32> = rfd
                    .effects
                    .iter()
                    .map(|effect| self.symbols.intern_name(&effect.node))
                    .collect();
                let fn_id = self.code.add_function(FnChunk {
                    name: qualified_name.clone(),
                    arity: rfd.params.len() as u8,
                    local_count: 0,
                    code: Vec::new(),
                    constants: Vec::new(),
                    effects: effect_ids,
                    thin: false,
                    parent_thin: false,
                    leaf: false,
                    no_alloc: false,
                    source_file: String::new(),
                    line_table: Vec::new(),
                });
                self.symbols.intern_function(
                    &qualified_name,
                    fn_id,
                    &rfd.effects
                        .iter()
                        .map(|e| e.node.clone())
                        .collect::<Vec<_>>(),
                )?;
                module_fn_ids.push((rfd.name.clone(), fn_id));
            }
        }

        let module_scope: HashMap<String, u32> = module_fn_ids.iter().cloned().collect();
        // Lower the dep module to MIR and walk each fn's MIR body into
        // bytecode with the dep's module scope — the same path the entry
        // module takes. MIR is the only VM codegen path; a rejection
        // surfaces as a hard CompileError (no HIR fallback).
        let dep_mir = build_optimized_mir(&dep_resolved, true);
        let mut fn_idx = 0;
        for item in &dep_resolved {
            if let ResolvedTopLevel::FnDef(rfd) = item {
                let (fn_name, fn_id) = &module_fn_ids[fn_idx];
                let mir_fn = dep_mir.fn_by_id(rfd.fn_id).ok_or_else(|| CompileError {
                    msg: format!(
                        "internal error: dep fn `{}` did not lower to MIR (an unsupported shape reached the VM backend)",
                        rfd.name
                    ),
                })?;
                let mut chunk = self
                    .compile_fn_via_mir(rfd, mir_fn, entry_symbols, arena, &module_scope, &dep_mir)
                    .map_err(|e| e.into_compile_error(&format!("dep fn `{}`", rfd.name)))?;
                chunk.name = visibility::qualified_name(dep_name, fn_name);
                self.code.functions[*fn_id as usize] = chunk;
                fn_idx += 1;
            }
        }

        // Expose exported functions and types via globals and namespace members.
        let exports = visibility::collect_module_exports(&mod_items);

        for fd in &exports.functions {
            let qualified = visibility::qualified_name(dep_name, &fd.name);
            let global_idx = self.ensure_global(&qualified);
            let symbol_id = self.symbols.find(&qualified).ok_or_else(|| CompileError {
                msg: format!("missing VM symbol for exposed function {}", qualified),
            })?;
            self.globals[global_idx as usize] = VmSymbolTable::symbol_ref(symbol_id);
        }

        let module_symbol_id = self.symbols.intern_namespace_path(dep_name)?;
        for et in &exports.types {
            let type_name = match et.def {
                TypeDef::Sum { name, .. } | TypeDef::Product { name, .. } => name,
            };
            let qualified = visibility::qualified_name(dep_name, type_name);
            if let Some(type_symbol_id) = self.symbols.find(&qualified) {
                let member_symbol_id = self.symbols.intern_name(type_name);
                self.symbols.add_namespace_member_by_id(
                    module_symbol_id,
                    member_symbol_id,
                    VmSymbolTable::symbol_ref(type_symbol_id),
                )?;
            }
        }
        for fd in &exports.functions {
            let qualified = visibility::qualified_name(dep_name, &fd.name);
            if let Some(fn_symbol_id) = self.symbols.find(&qualified) {
                let member_symbol_id = self.symbols.intern_name(&fd.name);
                self.symbols.add_namespace_member_by_id(
                    module_symbol_id,
                    member_symbol_id,
                    VmSymbolTable::symbol_ref(fn_symbol_id),
                )?;
            }
        }

        Ok(())
    }

    /// Oracle v1: install `BranchPath.Root` as a nullary constant
    /// member of the `BranchPath` namespace. The record is allocated
    /// once in the arena; the symbol table holds a NanValue
    /// referencing it. Follows the same pattern as `Option.None`
    /// which is installed as an immediate constant in
    /// `bootstrap_core_symbols`.
    fn install_branch_path_root_constant(&mut self, arena: &mut Arena) -> Result<(), CompileError> {
        // Guard: micro-benchmarks and unit tests often build a VM
        // without calling `register_service_types` first. When the
        // BranchPath arena type is absent, there's nothing Oracle-
        // related in the program and skipping the install is safe.
        let Some(type_id) = arena.find_type_id(crate::types::branch_path::TYPE_NAME) else {
            return Ok(());
        };
        let dewey = crate::nan_value::NanValue::new_string_value("", arena);
        let record_idx = arena.push_record(type_id, vec![dewey]);
        let root_value = crate::nan_value::NanValue::new_record(record_idx);
        self.symbols
            .intern_constant("BranchPath.Root", root_value)?;
        let namespace_symbol_id = self.symbols.intern_namespace_path("BranchPath")?;
        let member_symbol_id = self.symbols.intern_name("Root");
        self.symbols.add_namespace_member_by_id(
            namespace_symbol_id,
            member_symbol_id,
            root_value,
        )?;
        Ok(())
    }

    fn ensure_global(&mut self, name: &str) -> u16 {
        if let Some(&idx) = self.global_names.get(name) {
            return idx;
        }
        let idx = self.globals.len() as u16;
        self.global_names.insert(name.to_string(), idx);
        self.globals.push(NanValue::UNIT);
        idx
    }

    /// Register type symbols in VmSymbolTable for namespace resolution.
    /// Arena registration is handled separately via shared `collect_module_types`.
    ///
    /// Symbols are keyed by the type's canonical name — `Module.Type` for
    /// a dependency's declaration, the bare name for the entry's own. Two
    /// modules may each declare a `Colour`; those are distinct types (the
    /// local one shadows the dependency's), so their constructors must not
    /// land in one `Colour.Red` slot. The arena is keyed the same way, so
    /// both registries agree on identity.
    fn register_type_in_symbols(
        &mut self,
        td: &TypeDef,
        scope: Option<&str>,
        arena: &Arena,
    ) -> Result<(), CompileError> {
        let name = match td {
            TypeDef::Product { name, .. } | TypeDef::Sum { name, .. } => name,
        };
        let arena_name = match scope {
            Some(module) => visibility::qualified_name(module, name),
            None => name.to_string(),
        };
        match td {
            TypeDef::Product { fields, .. } => {
                self.symbols.intern_namespace(&arena_name)?;
                let type_id = arena
                    .find_type_id(&arena_name)
                    .unwrap_or_else(|| panic!("type `{arena_name}` already registered in Arena"));
                let field_symbol_ids: Vec<u32> = fields
                    .iter()
                    .map(|(field_name, _)| self.symbols.intern_name(field_name))
                    .collect();
                self.code.register_record_fields(type_id, &field_symbol_ids);
            }
            TypeDef::Sum { variants, .. } => {
                let type_symbol_id = self.symbols.intern_namespace(&arena_name)?;
                let type_id = arena
                    .find_type_id(&arena_name)
                    .unwrap_or_else(|| panic!("type `{arena_name}` already registered in Arena"));
                for (variant_id, variant) in variants.iter().enumerate() {
                    let ctor_id = arena
                        .find_ctor_id(type_id, variant_id as u16)
                        .expect("ctor id");
                    let qualified_name = visibility::member_key(&arena_name, &variant.name);
                    let ctor_symbol_id = self.symbols.intern_variant_ctor(
                        &qualified_name,
                        VmVariantCtor {
                            type_id,
                            variant_id: variant_id as u16,
                            ctor_id,
                            field_count: variant.fields.len() as u8,
                        },
                    )?;
                    let member_symbol_id = self.symbols.intern_name(&variant.name);
                    self.symbols.add_namespace_member_by_id(
                        type_symbol_id,
                        member_symbol_id,
                        VmSymbolTable::symbol_ref(ctor_symbol_id),
                    )?;
                }
            }
        }
        Ok(())
    }

    fn bootstrap_core_symbols(&mut self) -> Result<(), CompileError> {
        for builtin in VmBuiltin::ALL.iter().copied() {
            let builtin_symbol_id = self.symbols.intern_builtin(builtin)?;
            if let Some((namespace, member)) = builtin.name().split_once('.') {
                let namespace_symbol_id = self.symbols.intern_namespace_path(namespace)?;
                let member_symbol_id = self.symbols.intern_name(member);
                self.symbols.add_namespace_member_by_id(
                    namespace_symbol_id,
                    member_symbol_id,
                    VmSymbolTable::symbol_ref(builtin_symbol_id),
                )?;
            }
        }

        let result_symbol_id = self.symbols.intern_namespace_path("Result")?;
        let ok_symbol_id = self.symbols.intern_wrapper("Result.Ok", 0)?;
        let err_symbol_id = self.symbols.intern_wrapper("Result.Err", 1)?;
        let ok_member_symbol_id = self.symbols.intern_name("Ok");
        self.symbols.add_namespace_member_by_id(
            result_symbol_id,
            ok_member_symbol_id,
            VmSymbolTable::symbol_ref(ok_symbol_id),
        )?;
        let err_member_symbol_id = self.symbols.intern_name("Err");
        self.symbols.add_namespace_member_by_id(
            result_symbol_id,
            err_member_symbol_id,
            VmSymbolTable::symbol_ref(err_symbol_id),
        )?;
        for (member, builtin_name) in result::extra_members() {
            if let Some(symbol_id) = self.symbols.find(&builtin_name) {
                let member_symbol_id = self.symbols.intern_name(member);
                self.symbols.add_namespace_member_by_id(
                    result_symbol_id,
                    member_symbol_id,
                    VmSymbolTable::symbol_ref(symbol_id),
                )?;
            }
        }

        let option_symbol_id = self.symbols.intern_namespace_path("Option")?;
        let some_symbol_id = self.symbols.intern_wrapper("Option.Some", 2)?;
        self.symbols
            .intern_constant("Option.None", NanValue::NONE)?;
        let some_member_symbol_id = self.symbols.intern_name("Some");
        self.symbols.add_namespace_member_by_id(
            option_symbol_id,
            some_member_symbol_id,
            VmSymbolTable::symbol_ref(some_symbol_id),
        )?;
        let none_member_symbol_id = self.symbols.intern_name("None");
        self.symbols.add_namespace_member_by_id(
            option_symbol_id,
            none_member_symbol_id,
            NanValue::NONE,
        )?;
        for (member, builtin_name) in option::extra_members() {
            if let Some(symbol_id) = self.symbols.find(&builtin_name) {
                let member_symbol_id = self.symbols.intern_name(member);
                self.symbols.add_namespace_member_by_id(
                    option_symbol_id,
                    member_symbol_id,
                    VmSymbolTable::symbol_ref(symbol_id),
                )?;
            }
        }
        Ok(())
    }

    /// Phase 4b: emit a fn's bytecode by walking the MIR body
    /// instead of the HIR body. Mirrors `compile_fn_with_scope`'s
    /// `FnCompiler` setup exactly — same arity / local_count /
    /// effects / aliased slots — so the resulting `FnChunk` is
    /// drop-in for the HIR-emitted version when the MIR walker
    /// covers the body shape.
    fn compile_fn_via_mir(
        &mut self,
        rfd: &ResolvedFnDef,
        mir_fn: &crate::ir::mir::MirFn,
        symbols: &SymbolTable,
        arena: &mut Arena,
        module_scope: &HashMap<String, u32>,
        mir_program: &crate::ir::mir::MirProgram,
    ) -> Result<FnChunk, mir::MirVmUnsupported> {
        let resolution = rfd.resolution.as_ref();
        // The MIR body may mint synthetic slots past the resolver's
        // `local_count` (opaque-let temps for effectful intermediates),
        // so trust the MIR fn's own count — the resolver's is a lower
        // bound and underruns the frame on `STORE_LOCAL` to a temp.
        let local_count = mir_fn
            .local_count
            .max(resolution.map_or(rfd.params.len() as u32, |r| u32::from(r.local_count)))
            as u16;
        let local_slots: HashMap<String, u16> = resolution
            .map(|r| r.local_slots.as_ref().clone())
            .unwrap_or_else(|| {
                rfd.params
                    .iter()
                    .enumerate()
                    .map(|(i, (name, _))| (name.clone(), i as u16))
                    .collect()
            });

        let mut fc = FnCompiler::new(
            &rfd.name,
            rfd.params.len() as u8,
            local_count,
            rfd.effects
                .iter()
                .map(|effect| self.symbols.intern_name(&effect.node))
                .collect(),
            local_slots,
            &self.global_names,
            module_scope,
            &self.code,
            &mut self.symbols,
            arena,
            symbols,
            Some(mir_program),
        );
        fc.source_file = self.source_file.clone();
        fc.note_line(rfd.line);
        // Alias facts now ride the MIR fn (cloned from the resolver at
        // lowering) rather than the AST `FnResolution` side-channel —
        // the VM reads aliasedness off MIR like every other MIR-sourced
        // fact. Identical bits today; the move lets the analysis
        // re-home into a MIR pass later without touching this site.
        fc.set_aliased_slots(mir_fn.aliased_slots.clone());

        mir::compile_mir_fn_body(&mut fc, mir_fn)?;
        Ok(fc.finish())
    }

    fn compile_top_level(
        &mut self,
        items: &[ResolvedTopLevel],
        symbols: &SymbolTable,
        arena: &mut Arena,
        mir_program: &crate::ir::mir::MirProgram,
    ) -> Result<(), CompileError> {
        let has_stmts = items
            .iter()
            .any(|i| matches!(i, ResolvedTopLevel::Passthrough(TopLevel::Stmt(_))));
        if !has_stmts {
            return Ok(());
        }

        for item in items {
            if let ResolvedTopLevel::Passthrough(TopLevel::Stmt(Stmt::Binding(name, _, _))) = item {
                self.ensure_global(name);
            }
        }

        // Top-level statements never went through the resolver pass
        // (Phase E lifts `FnDef` bodies but leaves `TopLevel::Stmt`
        // as passthrough). Resolve them here against the entry's
        // symbol table.
        let resolver_ctx = crate::ir::hir::ResolveCtx::new(symbols);
        let resolved: Vec<ResolvedStmt> = items
            .iter()
            .filter_map(|i| match i {
                ResolvedTopLevel::Passthrough(TopLevel::Stmt(stmt)) => {
                    Some(resolve_stmt_for_top_level(&resolver_ctx, stmt))
                }
                _ => None,
            })
            .collect();

        // Each statement stores its value to a global (`Binding`) or pops
        // it (`Expr`); compute the store targets up front so the borrow
        // of `self.global_names` doesn't collide with the `FnCompiler`.
        let store_targets: Vec<Option<u16>> = resolved
            .iter()
            .map(|rs| match rs {
                ResolvedStmt::Binding { name, .. } => Some(self.global_names[name.as_str()]),
                ResolvedStmt::Expr(_) => None,
            })
            .collect();

        // Lower every value expression (the builtin / instantiation
        // tables grow on a clone of the entry program so the BuiltinId /
        // FnId references match the walker's program). The walker emits
        // the value; the `STORE_GLOBAL` / `POP` is emitted here, so no
        // MIR-level global-binding node is needed. MIR is the only VM
        // codegen path — a statement outside the lowerable subset (only
        // reachable on malformed / typecheck-rejected input) is a hard
        // CompileError, checked before any bytecode is emitted.
        let mut prog = mir_program.clone();
        let lowered: Vec<crate::ast::Spanned<crate::ir::mir::MirExpr>> = resolved
            .iter()
            .map(|rs| {
                let value = match rs {
                    ResolvedStmt::Binding { value, .. } | ResolvedStmt::Expr(value) => value,
                };
                crate::ir::mir::lower_top_level_value(value, &mut prog).map_err(|reason| {
                    CompileError {
                        msg: format!(
                            "internal error: a top-level statement did not lower to MIR ({reason:?})"
                        ),
                    }
                })
            })
            .collect::<Result<_, _>>()?;
        if let Some(bad) = lowered.iter().find(|low| !mir::mir_expr_compilable(low)) {
            return Err(CompileError {
                msg: format!(
                    "internal error: a top-level statement is outside the VM backend subset: {:?}",
                    bad.node
                ),
            });
        }

        let empty_mod_scope = HashMap::new();
        let mut fc = FnCompiler::new(
            "__top_level__",
            0,
            0,
            Vec::new(),
            HashMap::new(),
            &self.global_names,
            &empty_mod_scope,
            &self.code,
            &mut self.symbols,
            arena,
            symbols,
            Some(&prog),
        );

        for (idx, low) in lowered.iter().enumerate() {
            mir::compile_mir_expr(&mut fc, low)
                .map_err(|e| e.into_compile_error("a top-level statement"))?;
            match store_targets[idx] {
                Some(global_idx) => {
                    fc.emit_op(STORE_GLOBAL);
                    fc.emit_u16(global_idx);
                }
                None => fc.emit_op(POP),
            }
        }

        fc.emit_op(LOAD_UNIT);
        fc.emit_op(RETURN);

        let chunk = fc.finish();
        self.code.add_function(chunk);
        Ok(())
    }

    fn register_current_module_namespace(
        &mut self,
        items: &[ResolvedTopLevel],
    ) -> Result<(), CompileError> {
        let Some(module) = items.iter().find_map(|item| match item {
            ResolvedTopLevel::Module(module) => Some(module),
            _ => None,
        }) else {
            return Ok(());
        };

        let module_symbol_id = self.symbols.intern_namespace_path(&module.name)?;
        let exposes_ref = if module.exposes.is_empty() {
            None
        } else {
            Some(module.exposes.as_slice())
        };

        for item in items {
            match item {
                ResolvedTopLevel::FnDef(rfd) => {
                    if visibility::is_exposed(&rfd.name, exposes_ref)
                        && let Some(symbol_id) = self.symbols.find(&rfd.name)
                    {
                        let member_symbol_id = self.symbols.intern_name(&rfd.name);
                        self.symbols.add_namespace_member_by_id(
                            module_symbol_id,
                            member_symbol_id,
                            VmSymbolTable::symbol_ref(symbol_id),
                        )?;
                    }
                }
                ResolvedTopLevel::Passthrough(TopLevel::TypeDef(
                    TypeDef::Product { name, .. } | TypeDef::Sum { name, .. },
                )) => {
                    if visibility::is_exposed(name, exposes_ref)
                        && let Some(symbol_id) = self.symbols.find(name)
                    {
                        let member_symbol_id = self.symbols.intern_name(name);
                        self.symbols.add_namespace_member_by_id(
                            module_symbol_id,
                            member_symbol_id,
                            VmSymbolTable::symbol_ref(symbol_id),
                        )?;
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }
}

fn resolve_stmt_for_top_level(ctx: &crate::ir::hir::ResolveCtx<'_>, stmt: &Stmt) -> ResolvedStmt {
    crate::ir::hir::resolve::resolve_stmt_external(ctx, stmt)
}

/// What a function expression resolves to at compile time.
pub(super) struct FnCompiler<'a> {
    name: String,
    arity: u8,
    local_count: u16,
    effects: Vec<u32>,
    pub(super) local_slots: HashMap<String, u16>,
    global_names: &'a HashMap<String, u16>,
    /// Module-local function scope: simple_name → fn_id.
    /// Used for intra-module calls (e.g. `placeStairs` inside map.av).
    module_scope: &'a HashMap<String, u32>,
    pub(super) code_store: &'a CodeStore,
    pub(super) symbols: &'a mut VmSymbolTable,
    pub(super) arena: &'a mut Arena,
    /// Resolved-identity table for the current compilation scope. Used
    /// to map [`crate::ir::hir::ResolvedCallee::Fn`] and `ResolvedCtor::User`
    /// references back to their source-level canonical names so the VM
    /// can dispatch through `code_store.find` / arena lookups by name.
    ///
    /// Entry fns use the entry's `SymbolTable`. Dep fns (compiled via
    /// `integrate_module`) use a per-dep `SymbolTable` built off the
    /// dep's own items — keeps each compilation scope's `FnId` space
    /// self-consistent without forcing the caller to pre-merge.
    pub(super) symbol_table: &'a SymbolTable,
    /// Phase 6 wave 11 — the lowered MIR for the current program,
    /// when this `FnCompiler` is running on the MIR walker path
    /// (`compile_program_with_mir_fallback`). Used by the walker
    /// to resolve `MirCallee::Builtin(BuiltinId)` back to the
    /// canonical name the VM builtin table keys on. `None` on the
    /// HIR-only path.
    pub(super) mir_program: Option<&'a crate::ir::mir::MirProgram>,
    code: Vec<u8>,
    constants: Vec<NanValue>,
    /// Byte offset of the last emitted opcode (for superinstruction fusion).
    last_op_pos: usize,
    /// Source file path for this function.
    source_file: String,
    /// Run-length encoded line table being built: (bytecode_offset, source_line).
    line_table: Vec<(u16, u16)>,
    /// Last emitted line (for RLE dedup).
    last_noted_line: u16,
    /// Snapshot of `FnResolution.aliased_slots` for the current fn.
    /// Stamped per slot by the IR `alias` pass; backends consume it
    /// rather than re-deriving the same shape per fn. Empty when the
    /// fn was compiled outside the standard pipeline (REPL with no
    /// last-use phase, partial integrations) — the safe-but-slow
    /// reading is "every slot might be aliased" but the VM defaults
    /// to the legacy "everyone owned" behaviour for backwards
    /// compatibility; the alias pass always runs in real builds.
    aliased_slots: std::sync::Arc<Vec<bool>>,
}

impl<'a> FnCompiler<'a> {
    #[allow(clippy::too_many_arguments)]
    fn new(
        name: &str,
        arity: u8,
        local_count: u16,
        effects: Vec<u32>,
        local_slots: HashMap<String, u16>,
        global_names: &'a HashMap<String, u16>,
        module_scope: &'a HashMap<String, u32>,
        code_store: &'a CodeStore,
        symbols: &'a mut VmSymbolTable,
        arena: &'a mut Arena,
        symbol_table: &'a SymbolTable,
        mir_program: Option<&'a crate::ir::mir::MirProgram>,
    ) -> Self {
        FnCompiler {
            name: name.to_string(),
            arity,
            local_count,
            effects,
            local_slots,
            global_names,
            module_scope,
            code_store,
            symbols,
            arena,
            symbol_table,
            mir_program,
            code: Vec::new(),
            constants: Vec::new(),
            last_op_pos: usize::MAX,
            source_file: String::new(),
            line_table: Vec::new(),
            last_noted_line: 0,
            aliased_slots: std::sync::Arc::new(Vec::new()),
        }
    }

    fn set_aliased_slots(&mut self, aliased: std::sync::Arc<Vec<bool>>) {
        self.aliased_slots = aliased;
    }

    pub(super) fn is_aliased_slot(&self, slot: u16) -> bool {
        self.aliased_slots
            .get(slot as usize)
            .copied()
            .unwrap_or(false)
    }

    pub(super) fn name(&self) -> &str {
        &self.name
    }

    pub(super) fn global_names(&self) -> &HashMap<String, u16> {
        self.global_names
    }

    pub(super) fn module_scope(&self) -> &HashMap<String, u32> {
        self.module_scope
    }

    fn finish(self) -> FnChunk {
        FnChunk {
            name: self.name,
            arity: self.arity,
            local_count: self.local_count,
            code: self.code,
            constants: self.constants,
            effects: self.effects,
            thin: false,
            parent_thin: false,
            leaf: false,
            no_alloc: false,
            source_file: self.source_file,
            line_table: self.line_table,
        }
    }

    /// Record that bytecode emitted from this point forward corresponds to
    /// the given source line. RLE-deduplicated: consecutive calls with the
    /// same line produce only one entry.
    pub(super) fn note_line(&mut self, line: usize) {
        if line == 0 {
            return;
        }
        let line16 = line as u16;
        if line16 == self.last_noted_line {
            return; // RLE dedup
        }
        self.last_noted_line = line16;
        self.line_table.push((self.code.len() as u16, line16));
    }

    pub(super) fn emit_op(&mut self, op: u8) {
        let prev_pos = self.last_op_pos;
        let prev_op = if prev_pos < self.code.len() {
            self.code[prev_pos]
        } else {
            0xFF
        };

        // LOAD_LOCAL + LOAD_LOCAL → LOAD_LOCAL_2
        if op == LOAD_LOCAL && prev_op == LOAD_LOCAL && prev_pos + 2 == self.code.len() {
            self.code[prev_pos] = LOAD_LOCAL_2;
            // slot_a already at prev_pos+1, slot_b emitted next via emit_u8
            return;
        }
        // LOAD_LOCAL + LOAD_CONST → LOAD_LOCAL_CONST
        if op == LOAD_CONST && prev_op == LOAD_LOCAL && prev_pos + 2 == self.code.len() {
            self.code[prev_pos] = LOAD_LOCAL_CONST;
            // slot at prev_pos+1, const_idx (u16) emitted next via emit_u16
            return;
        }
        // VECTOR_GET + LOAD_CONST(hi,lo) + UNWRAP_OR → VECTOR_GET_OR(hi,lo)
        // Before: [..., VECTOR_GET, LOAD_CONST, hi, lo] + about to emit UNWRAP_OR
        // After:  [..., VECTOR_GET_OR, hi, lo]
        if op == UNWRAP_OR && self.code.len() >= 4 {
            let len = self.code.len();
            if self.code[len - 4] == VECTOR_GET && self.code[len - 3] == LOAD_CONST {
                let hi = self.code[len - 2];
                let lo = self.code[len - 1];
                self.code[len - 4] = VECTOR_GET_OR;
                self.code[len - 3] = hi;
                self.code[len - 2] = lo;
                self.code.pop(); // remove extra byte
                self.last_op_pos = len - 4;
                return;
            }
        }
        self.last_op_pos = self.code.len();
        self.code.push(op);
    }

    pub(super) fn emit_u8(&mut self, val: u8) {
        self.code.push(val);
    }

    pub(super) fn emit_u16(&mut self, val: u16) {
        self.code.push((val >> 8) as u8);
        self.code.push((val & 0xFF) as u8);
    }

    pub(super) fn emit_i16(&mut self, val: i16) {
        self.emit_u16(val as u16);
    }

    pub(super) fn emit_u32(&mut self, val: u32) {
        self.code.push((val >> 24) as u8);
        self.code.push(((val >> 16) & 0xFF) as u8);
        self.code.push(((val >> 8) & 0xFF) as u8);
        self.code.push((val & 0xFF) as u8);
    }

    pub(super) fn emit_u64(&mut self, val: u64) {
        self.code.extend_from_slice(&val.to_be_bytes());
    }

    pub(super) fn emit_i64(&mut self, val: i64) {
        self.code.extend_from_slice(&val.to_be_bytes());
    }

    pub(super) fn add_constant(&mut self, val: NanValue) -> u16 {
        for (i, c) in self.constants.iter().enumerate() {
            if c.bits() == val.bits() {
                return i as u16;
            }
        }
        let idx = self.constants.len() as u16;
        self.constants.push(val);
        idx
    }

    pub(super) fn offset(&self) -> usize {
        self.code.len()
    }

    pub(super) fn code_mut(&mut self) -> &mut Vec<u8> {
        &mut self.code
    }

    pub(super) fn emit_jump(&mut self, op: u8) -> usize {
        self.emit_op(op);
        let patch_pos = self.code.len();
        self.emit_i16(0);
        patch_pos
    }

    pub(super) fn patch_jump(&mut self, patch_pos: usize) {
        let target = self.code.len();
        let offset = (target as isize - patch_pos as isize - 2) as i16;
        let bytes = (offset as u16).to_be_bytes();
        self.code[patch_pos] = bytes[0];
        self.code[patch_pos + 1] = bytes[1];
    }

    pub(super) fn patch_jump_to(&mut self, patch_pos: usize, target: usize) {
        let offset = (target as isize - patch_pos as isize - 2) as i16;
        let bytes = (offset as u16).to_be_bytes();
        self.code[patch_pos] = bytes[0];
        self.code[patch_pos + 1] = bytes[1];
    }
}

#[cfg(test)]
mod tests {
    use super::compile_program_with_mir_fallback;
    use crate::ir::SymbolTable;
    use crate::ir::hir::resolve_program;
    use crate::nan_value::Arena;
    use crate::source::parse_source;
    use crate::vm::opcode::{LT, VECTOR_GET_OR, VECTOR_SET_OR_KEEP};

    /// Mirror of the pre-Phase-E test helper: tco + slot-resolve +
    /// resolved-HIR lift, no typecheck. Matches the original
    /// `compile_program` callsites that exercised the bytecode-emit
    /// path in isolation — keeping the "no `LT_INT` because spans
    /// aren't typed" assumption alive so the byte-shape assertions
    /// don't get nudged by typed-opcode promotion.
    fn compile_via_pipeline(source: &str) -> crate::vm::CodeStore {
        let mut items = parse_source(source).expect("source should parse");
        crate::ir::pipeline::tco(&mut items);
        crate::ir::pipeline::resolve(&mut items);
        let symbols = SymbolTable::build(&items, &[]);
        let resolved = resolve_program(&symbols, &items);
        let mut arena = Arena::new();
        let (code, _globals) =
            compile_program_with_mir_fallback(&resolved, &symbols, &mut arena, None)
                .expect("vm compile should pass");
        code
    }

    #[test]
    fn vector_get_with_literal_default_lowers_to_vector_get_or() {
        let source = r#"
module Demo

fn cellAt(grid: Vector<Int>, idx: Int) -> Int
    Option.withDefault(Vector.get(grid, idx), 0)
"#;

        let code = compile_via_pipeline(source);
        let fn_id = code.find("cellAt").expect("cellAt should exist");
        let chunk = code.get(fn_id);

        assert!(
            chunk.code.contains(&VECTOR_GET_OR),
            "expected VECTOR_GET_OR in bytecode, got {:?}",
            chunk.code
        );
    }

    #[test]
    fn vector_set_with_same_default_lowers_to_vector_set_or_keep() {
        let source = r#"
module Demo

fn updateOrKeep(vec: Vector<Int>, idx: Int, value: Int) -> Vector<Int>
    Option.withDefault(Vector.set(vec, idx, value), vec)
"#;

        let code = compile_via_pipeline(source);
        let fn_id = code
            .find("updateOrKeep")
            .expect("updateOrKeep should exist");
        let chunk = code.get(fn_id);

        assert!(
            chunk.code.contains(&VECTOR_SET_OR_KEEP),
            "expected VECTOR_SET_OR_KEEP in bytecode, got {:?}",
            chunk.code
        );
    }

    #[test]
    fn bool_match_on_gte_uses_base_compare_without_not() {
        let source = r#"
module Demo

fn bucket(n: Int) -> Int
    match n >= 10
        true -> 7
        false -> 3
"#;

        let code = compile_via_pipeline(source);
        let fn_id = code.find("bucket").expect("bucket should exist");
        let chunk = code.get(fn_id);

        assert!(
            chunk.code.contains(&LT),
            "expected the base integer compare LT in bytecode, got {:?}",
            chunk.code
        );
    }

    #[test]
    fn self_host_runtime_http_server_aliases_compile_in_vm() {
        let source = r#"
module Demo

fn listen(handler: Int) -> Unit
    SelfHostRuntime.httpServerListen(8080, handler)

fn listenWith(context: Int, handler: Int) -> Unit
    SelfHostRuntime.httpServerListenWith(8081, context, handler)
"#;

        let code = compile_via_pipeline(source);
        assert!(code.find("listen").is_some(), "listen should compile");
        assert!(
            code.find("listenWith").is_some(),
            "listenWith should compile"
        );
    }
}

mod calls;
mod classify;
mod expr;
pub mod mir;
mod patterns;

use std::collections::HashMap;

use crate::ast::{Stmt, TopLevel, TypeDef};
use crate::ir::SymbolTable;
use crate::ir::hir::{
    ResolveCtx, ResolvedFnBody, ResolvedFnDef, ResolvedStmt, ResolvedTopLevel, resolve_top_level,
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
pub fn compile_program(
    items: &[ResolvedTopLevel],
    symbols: &SymbolTable,
    arena: &mut Arena,
    analysis: Option<&crate::ir::AnalysisResult>,
) -> Result<(CodeStore, Vec<NanValue>), CompileError> {
    compile_program_with_modules(items, symbols, arena, None, "", analysis)
}

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
        None,
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
        None,
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
    // Phase 6 wave 5–9: optimize pipeline on the lowered MIR
    // before the VM walker consumes it. Order is deliberate:
    // (1) nullary-literal inlining unlocks call-site literals,
    // (2) const-fold collapses literal arithmetic,
    // (3) algebraic-simplify rewrites Int identities
    //     (`x + 0` / `x * 1` / `Neg(Neg(x))`) that const-fold
    //     leaves symbolic (no two-literal pattern to collapse),
    // (4) bool-match-to-if rewrites qualifying two-arm `Bool`
    //     match expressions into `IfThenElse` so backends get
    //     a uniform conditional shape instead of re-implementing
    //     the recognition (HIR's `try_emit_bool_if_else`),
    // (5) DCE drops `let _ = <pure>; body` chains where the
    //     binding was never read.
    // Each pass is a `MirProgram → MirProgram` pure function;
    // future passes plug in by extending the chain.
    let mir = crate::ir::mir::dead_code(crate::ir::mir::bool_match_to_if(
        crate::ir::mir::algebraic_simplify(crate::ir::mir::const_fold(
            crate::ir::mir::inline_nullary_literals(crate::ir::mir::lower_program(items)),
        )),
    ));
    compile_program_inner(
        items,
        symbols,
        arena,
        "",
        ModuleSource::Disk(None),
        analysis,
        Some(&mir),
    )
}

enum ModuleSource<'a> {
    Disk(Option<&'a str>),
    Loaded(Vec<crate::source::LoadedModule>),
}

fn compile_program_inner(
    items: &[ResolvedTopLevel],
    symbols: &SymbolTable,
    arena: &mut Arena,
    source_file: &str,
    module_source: ModuleSource<'_>,
    analysis: Option<&crate::ir::AnalysisResult>,
    mir_program: Option<&crate::ir::mir::MirProgram>,
) -> Result<(CodeStore, Vec<NanValue>), CompileError> {
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
                compiler.register_type_in_symbols(td, arena)?;
            }
            _ => {}
        }
    }

    compiler.register_current_module_namespace(items)?;

    for item in items {
        if let ResolvedTopLevel::FnDef(rfd) = item {
            let fn_id = compiler.code.find(&rfd.name).unwrap();
            // Phase 4b dispatch: if the caller supplied a
            // `MirProgram` *and* MIR has a body for this fn
            // *and* the MIR walker accepts the body, use the
            // MIR-emitted chunk. Otherwise fall back to the
            // HIR walker — same chunk path every other caller
            // takes.
            let chunk = if let Some(mir) = mir_program
                && let Some(mir_fn) = mir.fn_by_id(rfd.fn_id)
                && let Ok(mir_chunk) = compiler.compile_fn_via_mir(rfd, mir_fn, symbols, arena, mir)
            {
                mir_chunk
            } else {
                compiler.compile_fn(rfd, symbols, arena)?
            };
            compiler.code.functions[fn_id as usize] = chunk;
        }
    }

    compiler.compile_top_level(items, symbols, arena)?;
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
        crate::ir::pipeline::resolve(&mut mod_items);

        // Register types in Arena with qualified aliases.
        for mt in visibility::collect_module_types(&mod_items) {
            let type_id = match &mt.kind {
                visibility::ModuleTypeKind::Record { field_names } => {
                    arena.register_record_type(&mt.bare_name, field_names.clone())
                }
                visibility::ModuleTypeKind::Sum { variant_names } => {
                    arena.register_sum_type(&mt.bare_name, variant_names.clone())
                }
            };
            arena.register_type_alias(
                &visibility::qualified_name(dep_name, &mt.bare_name),
                type_id,
            );
        }
        for item in &mod_items {
            if let TopLevel::TypeDef(td) = item {
                self.register_type_in_symbols(td, arena)?;
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
        let mut fn_idx = 0;
        for item in &dep_resolved {
            if let ResolvedTopLevel::FnDef(rfd) = item {
                let (fn_name, fn_id) = &module_fn_ids[fn_idx];
                let mut chunk =
                    self.compile_fn_with_scope(rfd, entry_symbols, arena, &module_scope)?;
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
            if let Some(type_symbol_id) = self.symbols.find(type_name) {
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
    fn register_type_in_symbols(
        &mut self,
        td: &TypeDef,
        arena: &Arena,
    ) -> Result<(), CompileError> {
        match td {
            TypeDef::Product { name, fields, .. } => {
                self.symbols.intern_namespace_path(name)?;
                let type_id = arena
                    .find_type_id(name)
                    .expect("type already registered in Arena");
                let field_symbol_ids: Vec<u32> = fields
                    .iter()
                    .map(|(field_name, _)| self.symbols.intern_name(field_name))
                    .collect();
                self.code.register_record_fields(type_id, &field_symbol_ids);
            }
            TypeDef::Sum { name, variants, .. } => {
                let type_symbol_id = self.symbols.intern_namespace_path(name)?;
                let type_id = arena
                    .find_type_id(name)
                    .expect("type already registered in Arena");
                for (variant_id, variant) in variants.iter().enumerate() {
                    let ctor_id = arena
                        .find_ctor_id(type_id, variant_id as u16)
                        .expect("ctor id");
                    let qualified_name = visibility::member_key(name, &variant.name);
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

    fn compile_fn(
        &mut self,
        rfd: &ResolvedFnDef,
        symbols: &SymbolTable,
        arena: &mut Arena,
    ) -> Result<FnChunk, CompileError> {
        let empty_scope = HashMap::new();
        self.compile_fn_with_scope(rfd, symbols, arena, &empty_scope)
    }

    fn compile_fn_with_scope(
        &mut self,
        rfd: &ResolvedFnDef,
        symbols: &SymbolTable,
        arena: &mut Arena,
        module_scope: &HashMap<String, u32>,
    ) -> Result<FnChunk, CompileError> {
        let resolution = rfd.resolution.as_ref();
        let local_count = resolution.map_or(rfd.params.len() as u16, |r| r.local_count);
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
            None,
        );
        fc.source_file = self.source_file.clone();
        fc.note_line(rfd.line);
        if let Some(res) = resolution {
            fc.set_aliased_slots(res.aliased_slots.clone());
        }

        match rfd.body.as_ref() {
            ResolvedFnBody::Block(stmts) => fc.compile_body(stmts)?,
        }

        Ok(fc.finish())
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
        mir_program: &crate::ir::mir::MirProgram,
    ) -> Result<FnChunk, mir::MirVmUnsupported> {
        let resolution = rfd.resolution.as_ref();
        let local_count = resolution.map_or(rfd.params.len() as u16, |r| r.local_count);
        let local_slots: HashMap<String, u16> = resolution
            .map(|r| r.local_slots.as_ref().clone())
            .unwrap_or_else(|| {
                rfd.params
                    .iter()
                    .enumerate()
                    .map(|(i, (name, _))| (name.clone(), i as u16))
                    .collect()
            });

        let empty_scope = HashMap::new();
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
            &empty_scope,
            &self.code,
            &mut self.symbols,
            arena,
            symbols,
            Some(mir_program),
        );
        fc.source_file = self.source_file.clone();
        fc.note_line(rfd.line);
        if let Some(res) = resolution {
            fc.set_aliased_slots(res.aliased_slots.clone());
        }

        mir::compile_mir_fn_body(&mut fc, mir_fn)?;
        Ok(fc.finish())
    }

    fn compile_top_level(
        &mut self,
        items: &[ResolvedTopLevel],
        symbols: &SymbolTable,
        arena: &mut Arena,
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
        // symbol table so the bytecode-emit walk operates on the
        // same `ResolvedExpr` shape it does inside fn bodies.
        let resolver_ctx = crate::ir::hir::ResolveCtx::new(symbols);

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
            None,
        );

        for item in items {
            if let ResolvedTopLevel::Passthrough(TopLevel::Stmt(stmt)) = item {
                let resolved_stmt = resolve_stmt_for_top_level(&resolver_ctx, stmt);
                match &resolved_stmt {
                    ResolvedStmt::Binding { name, value, .. } => {
                        fc.compile_expr(value)?;
                        let idx = self.global_names[name.as_str()];
                        fc.emit_op(STORE_GLOBAL);
                        fc.emit_u16(idx);
                    }
                    ResolvedStmt::Expr(value) => {
                        fc.compile_expr(value)?;
                        fc.emit_op(POP);
                    }
                }
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
pub(super) enum CallTarget {
    /// Known function id (local or qualified module function).
    KnownFn(u32),
    /// Result.Ok / Result.Err / Option.Some → WRAP opcode. kind: 0=Ok, 1=Err, 2=Some.
    Wrapper(u8),
    /// Option.None → load constant.
    None_,
    /// User-defined variant constructor: Shape.Circle → VARIANT_NEW (or inline nullary at runtime).
    Variant(u32, u16),
    /// Known VM builtin/service resolved by name and interned into the VM symbol table.
    Builtin(VmBuiltin),
    /// Unknown capitalized dotted path that did not resolve to a function, variant, or builtin.
    UnknownQualified(String),
}

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

    pub(super) fn code(&self) -> &Vec<u8> {
        &self.code
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

    pub(super) fn bind_top_to_local(&mut self, name: &str) {
        if let Some(&slot) = self.local_slots.get(name) {
            self.emit_op(STORE_LOCAL);
            self.emit_u8(slot as u8);
        } else {
            self.emit_op(POP);
        }
    }

    pub(super) fn dup_and_bind_top_to_local(&mut self, name: &str) {
        self.emit_op(DUP);
        self.bind_top_to_local(name);
    }

    /// Override `local_slots` with this arm's per-arm fresh slots so
    /// every `bind_top_to_local(name)` inside the arm writes to the
    /// slot the resolver allocated for *this* arm (not whatever was
    /// last allocated for the same name elsewhere). Returns the saved
    /// prior mapping so the caller can `restore_local_slots` afterward.
    pub(super) fn install_arm_slots(
        &mut self,
        arm: &crate::ir::hir::ResolvedMatchArm,
    ) -> Vec<(String, Option<u16>)> {
        let names = collect_pattern_binding_names(&arm.pattern);
        let slots = arm.binding_slots.get().cloned().unwrap_or_default();
        let mut saved = Vec::new();
        for (i, name) in names.iter().enumerate() {
            if name == "_" {
                continue;
            }
            let Some(&slot) = slots.get(i) else { continue };
            if slot == u16::MAX {
                continue;
            }
            saved.push((name.clone(), self.local_slots.get(name).copied()));
            self.local_slots.insert(name.clone(), slot);
        }
        saved
    }

    pub(super) fn restore_local_slots(&mut self, saved: Vec<(String, Option<u16>)>) {
        for (name, prior) in saved.into_iter().rev() {
            match prior {
                Some(slot) => {
                    self.local_slots.insert(name, slot);
                }
                None => {
                    self.local_slots.remove(&name);
                }
            }
        }
    }
}

/// Pattern-position-ordered binding names — must mirror
/// `resolver::ResolverState::allocate_pattern` exactly so position
/// `i` lines up with `arm.binding_slots[i]`.
fn collect_pattern_binding_names(pattern: &crate::ir::hir::ResolvedPattern) -> Vec<String> {
    use crate::ir::hir::ResolvedPattern;
    match pattern {
        ResolvedPattern::Ident(name) => vec![name.clone()],
        ResolvedPattern::Cons(head, tail) => vec![head.clone(), tail.clone()],
        ResolvedPattern::Ctor(_, bindings) => bindings.clone(),
        ResolvedPattern::Tuple(items) => items
            .iter()
            .flat_map(collect_pattern_binding_names)
            .collect(),
        ResolvedPattern::Wildcard | ResolvedPattern::Literal(_) | ResolvedPattern::EmptyList => {
            Vec::new()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::compile_program;
    use crate::ir::SymbolTable;
    use crate::ir::hir::resolve_program;
    use crate::nan_value::Arena;
    use crate::source::parse_source;
    use crate::vm::opcode::{LT, NOT, VECTOR_GET_OR, VECTOR_SET_OR_KEEP};

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
            compile_program(&resolved, &symbols, &mut arena, None).expect("vm compile should pass");
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
            "expected LT in bytecode, got {:?}",
            chunk.code
        );
        assert!(
            !chunk.code.contains(&NOT),
            "did not expect NOT in normalized bool-match bytecode, got {:?}",
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

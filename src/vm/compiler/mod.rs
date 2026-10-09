mod classify;
mod expr;
mod field_take;
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

/// Compile a single-module program to VM bytecode. Every fn is lowered
/// to MIR and emitted by the MIR walker; a shape the walker cannot emit
/// is a hard error, not a fallback — the HIR walker it used to fall back
/// to is gone.
///
/// Same I/O contract as [`compile_program_with_modules`]; the only
/// difference is where the dependency modules come from.
pub fn compile_program(
    items: &[ResolvedTopLevel],
    symbols: &SymbolTable,
    arena: &mut Arena,
    analysis: Option<&crate::ir::AnalysisResult>,
) -> Result<(CodeStore, Vec<NanValue>), CompileError> {
    // Single-module entry, kept for the parity tests
    // (`tests/mir_vm_parity.rs`) that exercise the compiler without a
    // module root. The optimize pipeline + per-fn MIR emit live in
    // `compile_program_inner`, shared with the module-aware production
    // entry points.
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

/// What to say when a function is absent from the MIR program.
///
/// "Did not lower" is the backend's own vocabulary and names a file the
/// reader did not write in. When the reason is a name the resolver could
/// not classify, the resolved body still holds it, and that is the thing to
/// report: the reference, the function, the file and the line. The internal
/// error stays for the case it was written for — a shape genuinely outside
/// the lowerable subset, where no name is at fault.
fn did_not_lower_message(rfd: &ResolvedFnDef, file: &str) -> String {
    match crate::ir::hir::collect_unresolved_in_fn(rfd).first() {
        Some(unresolved) => {
            unresolved.explain(&format!("fn `{}`", rfd.name), &unresolved.at_file(file))
        }
        None => format!(
            "internal error: fn `{}` did not lower to MIR (an unsupported shape reached the VM backend)",
            rfd.name
        ),
    }
}

/// Lower a resolved item list to MIR and run the Phase 6 optimize
/// pipeline. Order is deliberate: (1) nullary-literal inlining unlocks
/// call-site literals, (2) const-fold collapses literal arithmetic,
/// (3) algebraic-simplify rewrites Int identities, (4) bool-match-to-if
/// rewrites two-arm `Bool` matches into `IfThenElse`, (5) branch-collapse
/// drops the dead branch of a folded `IfThenElse`, (6) DCE drops unread
/// `let _ = <pure>` chains, (7) closed list-refinement provenance removes
/// unreachable validation walks, and (8) ownership refinement runs on the
/// final local graph. Shared by the entry compile and per-dep-module MIR
/// builds so both paths see identical lowered+optimized shapes.
fn build_optimized_mir(
    items: &[ResolvedTopLevel],
    external_callers_possible: bool,
    refinements: &crate::analysis::literal_refinement::LiteralRefinementTable,
) -> crate::ir::mir::MirProgram {
    let mut lowered = crate::ir::mir::lower_program(items);
    // Mark dependency-module fragments so `own_param_refine` bails: the
    // VM compiles each `depends [...]` module separately, so a dep
    // fragment cannot see the entry/sibling call sites that may alias a
    // param. The entry compile (and the flattened wasm-gc / Rust builds)
    // see all callers, so graduation stays enabled there.
    lowered.external_callers_possible = external_callers_possible;
    crate::ir::mir::optimize::optimize_with_list_refinements(lowered, refinements)
}

/// Run the four fabricating traversal passes — buffer-build deforestation,
/// chars fusion, String indexing, and list build — over a freshly-parsed dependency
/// module, and keep the result only when the entry's symbol table
/// already knows every variant they synthesized.
///
/// The VM re-parses every dep off disk (`load_module_tree`), so the
/// passes the caller ran over ITS copy of the module — the copy
/// `SymbolTable` was built from — have to run again here or the VM
/// executes the unfused spelling of code the Rust compile path fuses.
/// They run in the pipeline's own order, because that is the order the
/// caller's copy saw and the two copies have to end up the same
/// program.
///
/// The symbol-table check is what makes running them here safe for every
/// caller at once. A caller only registers workers for passes its target
/// lowers: proofs register none, wasm registers String-index workers,
/// and VM/Rust register the full set. A synthesized name absent from the
/// table means this freshly parsed copy must stay pristine for that pass;
/// otherwise its call resolves to nothing. So: lower exactly when the
/// caller lowered. Both shapes compute the same result; only their cost
/// model differs.
fn adopt_deforestation_if_symbols_agree(
    dep_name: &str,
    pristine: Vec<TopLevel>,
    entry_symbols: &SymbolTable,
) -> Vec<TopLevel> {
    // Detection is read-only, so ask it first: most dep modules hold
    // neither shape, and this keeps the common case from copying a
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
    if !has_sink
        && !crate::ir::has_fusable_shape(&pristine)
        && !crate::ir::has_string_index_shape(&pristine)
        && !crate::ir::has_list_build_shape(&pristine)
    {
        return pristine;
    }

    let every_variant_known = |synthesized: &[String]| {
        synthesized.iter().all(|name| {
            entry_symbols
                .fn_id_of(&crate::ir::FnKey::in_module(dep_name, name.clone()))
                .is_some()
        })
    };
    let mut lowered = pristine;

    // Adopt each stage independently. String indexing is available on wasm
    // while the older mutable builders/cursors are not, so treating all
    // fabricated names as one all-or-nothing set would suppress a supported
    // pass whenever the same dependency also contains an unsupported shape.
    let mut candidate = lowered.clone();
    let report = crate::ir::pipeline::buffer_build(&mut candidate);
    if !report.synthesized.is_empty() && every_variant_known(&report.synthesized) {
        lowered = candidate;
    }

    let mut candidate = lowered.clone();
    let report = crate::ir::pipeline::chars_fusion(&mut candidate);
    if (report.synthesized.is_empty() && report.codepoint_matches > 0)
        || (!report.synthesized.is_empty() && every_variant_known(&report.synthesized))
    {
        lowered = candidate;
    }

    let mut candidate = lowered.clone();
    let report = crate::ir::pipeline::string_index(&mut candidate);
    if !report.synthesized.is_empty() && every_variant_known(&report.synthesized) {
        lowered = candidate;
    }

    let mut candidate = lowered.clone();
    let report = crate::ir::pipeline::list_build(&mut candidate);
    if !report.synthesized.is_empty() && every_variant_known(&report.synthesized) {
        lowered = candidate;
    }

    lowered
}

fn compile_program_inner(
    items: &[ResolvedTopLevel],
    symbols: &SymbolTable,
    arena: &mut Arena,
    source_file: &str,
    module_source: ModuleSource<'_>,
    analysis: Option<&crate::ir::AnalysisResult>,
) -> Result<(CodeStore, Vec<NanValue>), CompileError> {
    // Lower the entry items to MIR and run the optimize pipeline; the
    // per-fn loop below emits each fn through the MIR walker, the only
    // VM codegen path. Built here — not in the caller —
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
    let mir_built = build_optimized_mir(items, false, symbols.literal_refinements());
    let mir_program = &mir_built;

    let mut compiler = ProgramCompiler::new();
    compiler.register_process_verify_drivers(items);
    compiler.source_file = source_file.to_string();
    compiler.sync_record_field_symbols(arena)?;
    compiler.register_capability_symbols(symbols)?;
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
            refuse_failed_dependency(&loaded)?;
            for m in loaded {
                let path = m.path.display().to_string();
                compiler.integrate_module(&m.dep_name, &path, m.items, symbols, arena)?;
            }
        }
    }

    for item in items {
        if let ResolvedTopLevel::Passthrough(TopLevel::Stmt(Stmt::Binding(name, _, _))) = item {
            compiler.ensure_global(name)?;
        }
    }

    for item in items {
        match item {
            ResolvedTopLevel::FnDef(rfd) => {
                // A fn takes no global slot: a fn read as a value
                // resolves through its VM symbol (`compile_ident`), which
                // is the same `symbol_ref` a global would have held. Globals
                // are for module bindings only, so their count does not grow
                // with the number of fns (verify adds two or three per case).
                let arity = operand_u8(
                    rfd.params.len(),
                    &format!(
                        "function `{}` has {} parameters",
                        rfd.name,
                        rfd.params.len()
                    ),
                )?;
                let effect_ids: Vec<u32> = rfd
                    .effects
                    .iter()
                    .map(|effect| compiler.symbols.intern_name(&effect.node))
                    .collect();
                let fn_id = compiler.code.add_function(FnChunk {
                    name: rfd.name.clone(),
                    arity,
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
                compiler.symbols.intern_function(
                    &rfd.name,
                    fn_id,
                    &rfd.effects
                        .iter()
                        .map(|e| e.node.clone())
                        .collect::<Vec<_>>(),
                )?;
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
            let mir_fn = mir_program
                .fn_by_id(rfd.fn_id)
                .ok_or_else(|| CompileError {
                    msg: did_not_lower_message(rfd, &compiler.source_file),
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

    compiler.code.required_capability_operations =
        std::mem::take(&mut compiler.required_capability_operations);
    for operation in &compiler.code.required_capability_operations {
        let module = operation
            .rsplit_once('.')
            .map(|(module, _)| module)
            .unwrap_or(operation);
        if let Some((contract_hash, model_hash)) = symbols.capability_contract_hashes(module) {
            compiler.code.required_capability_contracts.insert(
                module.to_string(),
                (contract_hash.to_string(), model_hash.to_string()),
            );
        }
    }

    Ok((compiler.code, compiler.globals))
}

#[derive(Debug)]
pub struct CompileError {
    pub msg: String,
}

/// Refuse a program one of whose dependencies failed its own check.
///
/// An importer is checked against the surfaces of its dependencies only.
/// Each dependency's body is checked once, by its own front door, when the
/// loader lowers the module list, and the verdict travels on the module
/// (`check_errors`). A module that failed keeps whatever form its check left
/// it in (a nested pattern as written, a name that shadows a function, a
/// type error in a flat body) and must never be resolved or compiled into an
/// importer, whatever it contains. This refuses the program with the first
/// such module's own errors, leaves-first.
fn refuse_failed_dependency(modules: &[crate::source::LoadedModule]) -> Result<(), CompileError> {
    let Some(module) = modules
        .iter()
        .find(|module| !module.check_errors.is_empty())
    else {
        return Ok(());
    };
    let details = module
        .check_errors
        .iter()
        .map(|error| {
            let file = error
                .origin
                .as_ref()
                .map(|origin| origin.file.clone())
                .unwrap_or_else(|| module.path.display().to_string());
            format!("  {}:{}:{}: {}", file, error.line, error.col, error.message)
        })
        .collect::<Vec<_>>();
    Err(CompileError {
        msg: format!(
            "Type errors in dependency module '{}':\n{}",
            module.dep_name,
            details.join("\n")
        ),
    })
}

pub(super) fn operand_u8(value: usize, what: &str) -> Result<u8, CompileError> {
    u8::try_from(value).map_err(|_| CompileError {
        msg: format!("{what}; the VM supports at most {}", u8::MAX),
    })
}

/// A `CALL_KNOWN` / `TAIL_CALL_KNOWN` target. The operand is `u16`, so a
/// call to a function past that id refuses to compile rather than
/// dispatching to whichever function the truncated id names.
pub(super) fn fn_id_operand(fn_id: u32, name: &str) -> Result<u16, CompileError> {
    operand_u16(
        fn_id as usize,
        &format!("call to `{name}` targets function id {fn_id}"),
    )
}

/// An arena type id in a record / variant instruction (`u16` operand).
pub(super) fn type_id_operand(type_id: u32, type_name: &str) -> Result<u16, CompileError> {
    operand_u16(
        type_id as usize,
        &format!("type `{type_name}` has arena type id {type_id}"),
    )
}

/// The 16-bit twin of [`operand_u8`]: every `u16` instruction operand
/// (global, constant, function, type and constructor indices) goes through
/// here, so an index past the field refuses the compile instead of wrapping
/// onto a different entry.
pub(super) fn operand_u16(value: usize, what: &str) -> Result<u16, CompileError> {
    u16::try_from(value).map_err(|_| CompileError {
        msg: format!(
            "{what}; the VM supports at most {} (indices 0..={})",
            usize::from(u16::MAX) + 1,
            u16::MAX
        ),
    })
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
    required_capability_operations: std::collections::BTreeSet<String>,
    process_verify_drivers: std::collections::HashSet<crate::ir::FnId>,
    /// While a dependency's fns compile: the global table they see, which
    /// adds that module's own bindings (`base = 40`) under their bare names,
    /// pointing at the qualified globals (`Lib.base`) that hold them. A
    /// dependency's bindings are not the entry's, so they never enter
    /// `global_names` under a bare name.
    dep_global_view: Option<HashMap<String, u16>>,
}

impl ProgramCompiler {
    fn new() -> Self {
        let mut compiler = ProgramCompiler {
            code: CodeStore::new(),
            symbols: VmSymbolTable::default(),
            globals: Vec::new(),
            global_names: HashMap::new(),
            source_file: String::new(),
            required_capability_operations: std::collections::BTreeSet::new(),
            process_verify_drivers: std::collections::HashSet::new(),
            dep_global_view: None,
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

    /// Register provider-bound operations as callable namespace members. They
    /// deliberately have no function body; runtime dispatch remains fail-closed
    /// unless an oracle/replay/provider supplies the boundary result.
    fn register_capability_symbols(
        &mut self,
        source_symbols: &SymbolTable,
    ) -> Result<(), CompileError> {
        let mut operations: Vec<_> = source_symbols.capability_operations().collect();
        operations.sort_by(|left, right| left.0.cmp(right.0));
        for (canonical_name, info) in operations {
            let symbol_id = self.symbols.intern_capability(canonical_name, info)?;
            let (namespace, member) =
                canonical_name
                    .rsplit_once('.')
                    .ok_or_else(|| CompileError {
                        msg: format!(
                            "capability operation '{}' must have a module-qualified name",
                            canonical_name
                        ),
                    })?;
            let namespace_id = self.symbols.intern_namespace_path(namespace)?;
            let member_id = self.symbols.intern_name(member);
            self.symbols.add_namespace_member_by_id(
                namespace_id,
                member_id,
                VmSymbolTable::symbol_ref(symbol_id),
            )?;
        }
        Ok(())
    }

    /// Load every module already present in the program-wide symbol table,
    /// then compile its functions and register symbols. This includes both
    /// written `depends [...]` edges and implicit standard-library edges owned
    /// by source-typed builtins such as `String.toUtf8`.
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

        let mut root_deps = module.depends.clone();
        for dependency in entry_symbols
            .modules
            .iter()
            .filter_map(|entry| entry.prefix.as_ref())
        {
            if !root_deps.contains(dependency) {
                root_deps.push(dependency.clone());
            }
        }

        let modules = crate::source::load_module_tree(&root_deps, module_root)
            .map_err(|e| CompileError { msg: e })?;
        refuse_failed_dependency(&modules)?;

        for loaded in modules {
            let path = loaded.path.display().to_string();
            self.integrate_module(&loaded.dep_name, &path, loaded.items, entry_symbols, arena)?;
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
    ///
    /// `dep_path` is the file the module was loaded from, carried so a
    /// refusal names a place the reader can open rather than a module name
    /// they then have to go looking for.
    fn integrate_module(
        &mut self,
        dep_name: &str,
        dep_path: &str,
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
        mod_items = adopt_deforestation_if_symbols_agree(dep_name, mod_items, entry_symbols);
        crate::ir::pipeline::resolve_and_reannotate(&mut mod_items);

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
                let arity = operand_u8(
                    rfd.params.len(),
                    &format!(
                        "function `{qualified_name}` has {} parameters",
                        rfd.params.len()
                    ),
                )?;
                let effect_ids: Vec<u32> = rfd
                    .effects
                    .iter()
                    .map(|effect| self.symbols.intern_name(&effect.node))
                    .collect();
                let fn_id = self.code.add_function(FnChunk {
                    name: qualified_name.clone(),
                    arity,
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
        // The module's own bindings (`base = 40`) live in globals keyed by
        // their qualified name, so two modules' `base` stay apart; the
        // module's fns and its top-level chunk read them by the bare name
        // they wrote.
        let binding_stmts: Vec<&Stmt> = mod_items
            .iter()
            .filter_map(|item| match item {
                TopLevel::Stmt(stmt) => Some(stmt),
                _ => None,
            })
            .collect();
        let mut dep_globals = self.global_names.clone();
        for stmt in &binding_stmts {
            if let Stmt::Binding(name, _, _) = stmt {
                let idx = self.ensure_global(&visibility::qualified_name(dep_name, name))?;
                dep_globals.insert(name.clone(), idx);
            }
        }
        self.dep_global_view = Some(dep_globals);
        // Lower the dep module to MIR and walk each fn's MIR body into
        // bytecode with the dep's module scope — the same path the entry
        // module takes. MIR is the only VM codegen path; a rejection
        // surfaces as a hard CompileError (no HIR fallback).
        self.register_process_verify_drivers(&dep_resolved);
        let dep_mir = build_optimized_mir(&dep_resolved, true, entry_symbols.literal_refinements());
        let mut fn_idx = 0;
        for item in &dep_resolved {
            if let ResolvedTopLevel::FnDef(rfd) = item {
                let (fn_name, fn_id) = &module_fn_ids[fn_idx];
                let mir_fn = dep_mir.fn_by_id(rfd.fn_id).ok_or_else(|| CompileError {
                    msg: did_not_lower_message(rfd, dep_path),
                })?;
                let mut chunk = self
                    .compile_fn_via_mir(rfd, mir_fn, entry_symbols, arena, &module_scope, &dep_mir)
                    .map_err(|e| e.into_compile_error(&format!("dep fn `{}`", rfd.name)))?;
                chunk.name = visibility::qualified_name(dep_name, fn_name);
                self.code.functions[*fn_id as usize] = chunk;
                fn_idx += 1;
            }
        }
        let dep_globals = self.dep_global_view.take().unwrap_or_default();
        if !binding_stmts.is_empty() {
            self.compile_top_level_chunk(
                &format!("__top_level__:{dep_name}"),
                &binding_stmts,
                &ctx,
                Some(&dep_globals),
                &module_scope,
                dep_path,
                entry_symbols,
                arena,
                &dep_mir,
            )?;
        }

        // Expose exported functions and types as namespace members. An
        // exported fn read as a value (`Lib.f`) resolves through its VM
        // symbol, so it takes no global slot.
        let exports = visibility::collect_module_exports(&mod_items);

        for fd in &exports.functions {
            let qualified = visibility::qualified_name(dep_name, &fd.name);
            if self.symbols.find(&qualified).is_none() {
                return Err(CompileError {
                    msg: format!("missing VM symbol for exposed function {}", qualified),
                });
            }
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
    ///
    /// This is the one symbol-table value that is a heap reference
    /// rather than an immediate, which is why the table is a root
    /// holder: `VM::collect_stable_roots` and
    /// `VM::build_parallel_base_context` rebase it alongside the
    /// chunk constants and the globals.
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

    /// The global slot holding module binding `name`, allocated on first
    /// use. `LOAD_GLOBAL` / `STORE_GLOBAL` carry a `u16` index, so a program
    /// with more bindings than that refuses to compile rather than letting
    /// two bindings share one slot.
    fn ensure_global(&mut self, name: &str) -> Result<u16, CompileError> {
        if let Some(&idx) = self.global_names.get(name) {
            return Ok(idx);
        }
        let idx = operand_u16(
            self.globals.len(),
            &format!(
                "module binding `{name}` needs global slot {}",
                self.globals.len()
            ),
        )?;
        self.global_names.insert(name.to_string(), idx);
        self.globals.push(NanValue::UNIT);
        Ok(idx)
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
                for (variant_index, variant) in variants.iter().enumerate() {
                    let variant_id = operand_u16(
                        variant_index,
                        &format!("type `{arena_name}` has {} variants", variants.len()),
                    )?;
                    let field_count = operand_u8(
                        variant.fields.len(),
                        &format!(
                            "variant `{}.{}` has {} fields",
                            arena_name,
                            variant.name,
                            variant.fields.len()
                        ),
                    )?;
                    let ctor_id = arena.find_ctor_id(type_id, variant_id).expect("ctor id");
                    let qualified_name = visibility::member_key(&arena_name, &variant.name);
                    let ctor_symbol_id = self.symbols.intern_variant_ctor(
                        &qualified_name,
                        VmVariantCtor {
                            type_id,
                            variant_id,
                            ctor_id,
                            field_count,
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

    /// Resolve compiler-owned verification helpers before recording host needs.
    fn register_process_verify_drivers(&mut self, items: &[ResolvedTopLevel]) {
        let names: std::collections::HashSet<&str> = items
            .iter()
            .filter_map(|item| match item {
                ResolvedTopLevel::Passthrough(TopLevel::Verify(block)) => Some(block),
                _ => None,
            })
            .flat_map(|block| block.process_driver_names())
            .collect();
        self.process_verify_drivers
            .extend(items.iter().filter_map(|item| match item {
                ResolvedTopLevel::FnDef(fd) if names.contains(fd.name.as_str()) => Some(fd.fn_id),
                _ => None,
            }));
    }

    /// Emit a function's bytecode from its MIR body and resolved signature.
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
            .max(resolution.map_or(rfd.params.len() as u32, |r| u32::from(r.local_count)));
        let local_count = u16::try_from(local_count).map_err(|_| CompileError {
            msg: format!(
                "function `{}` has {local_count} local bindings; the VM supports at most {}",
                rfd.name,
                u16::MAX
            ),
        })?;
        // `FnCompiler.local_slots` feeds exactly one consumer: the
        // `MirExpr::FnValue` path (`compile_ident`). A `FnValue` name is
        // an ident the resolver left bare — so when a resolution exists,
        // NO local with that spelling was in scope at the use site, and
        // any name-keyed hit would be an out-of-scope pattern binder
        // that shares the spelling (`FnResolution.local_slots` is
        // last-allocation-wins, issue #948). Loading that slot reads an
        // unrelated — typically uninitialized — local ("cannot call
        // non-function (got Unit)") and hijacks a module-fn reference.
        // Hand the compiler an EMPTY local table so `FnValue` resolves
        // through module fns / globals / builtin symbols only. The
        // params-only fallback stays for bodies compiled WITHOUT a
        // resolution, where idents were never rewritten and a bare
        // param read can legitimately reach `FnValue`.
        let arity = operand_u8(
            rfd.params.len(),
            &format!(
                "function `{}` has {} parameters",
                rfd.name,
                rfd.params.len()
            ),
        )?;
        let local_slots: HashMap<String, u16> = match resolution {
            Some(_) => HashMap::new(),
            None => (0..arity)
                .zip(&rfd.params)
                .map(|(i, (name, _))| (name.clone(), u16::from(i)))
                .collect(),
        };
        // Verify drivers remain callable by the case runner, but their exact
        // stubs do not create live provider requirements for ordinary `run`.
        // Calls in real process segments still accumulate in the main set.
        let mut verify_requirements = std::collections::BTreeSet::new();
        let required_operations = if self.process_verify_drivers.contains(&rfd.fn_id) {
            &mut verify_requirements
        } else {
            &mut self.required_capability_operations
        };
        let mut fc = FnCompiler::new(
            &rfd.name,
            arity,
            local_count,
            rfd.effects
                .iter()
                .map(|effect| self.symbols.intern_name(&effect.node))
                .collect(),
            local_slots,
            self.dep_global_view.as_ref().unwrap_or(&self.global_names),
            module_scope,
            &self.code,
            &mut self.symbols,
            arena,
            symbols,
            Some(mir_program),
            required_operations,
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
        let stmts: Vec<&Stmt> = items
            .iter()
            .filter_map(|i| match i {
                ResolvedTopLevel::Passthrough(TopLevel::Stmt(stmt)) => Some(stmt),
                _ => None,
            })
            .collect();
        if stmts.is_empty() {
            return Ok(());
        }
        for stmt in &stmts {
            if let Stmt::Binding(name, _, _) = stmt {
                self.ensure_global(name)?;
            }
        }
        // Top-level statements never went through the resolver pass
        // (Phase E lifts `FnDef` bodies but leaves `TopLevel::Stmt`
        // as passthrough). Resolve them here against the entry's
        // symbol table.
        let resolver_ctx = crate::ir::hir::ResolveCtx::new(symbols);
        let source_file = self.source_file.clone();
        self.compile_top_level_chunk(
            "__top_level__",
            &stmts,
            &resolver_ctx,
            None,
            &HashMap::new(),
            &source_file,
            symbols,
            arena,
            mir_program,
        )
    }

    /// One chunk that evaluates a module's top-level statements in source
    /// order, storing each binding's value in its global (`Binding`) or
    /// popping it (`Expr`). The entry's chunk is `__top_level__`; a
    /// dependency's is `__top_level__:<Module>`, and `VM::run_top_level`
    /// runs them in the order they were added — dependencies first, the way
    /// the loader hands them over — so a binding whose value calls into a
    /// dependency reads that dependency's bindings already filled.
    ///
    /// `dep_globals` is the global table a dependency's statements see: its
    /// own bindings under their bare names, mapped to the qualified globals
    /// that hold them. `None` for the entry, whose bindings are globals
    /// under their own names. `module_scope` is the dependency's own fns,
    /// for a fn passed as a value (empty for the entry).
    #[allow(clippy::too_many_arguments)]
    fn compile_top_level_chunk(
        &mut self,
        chunk_name: &str,
        stmts: &[&Stmt],
        resolver_ctx: &crate::ir::hir::ResolveCtx<'_>,
        dep_globals: Option<&HashMap<String, u16>>,
        module_scope: &HashMap<String, u32>,
        source_file: &str,
        symbols: &SymbolTable,
        arena: &mut Arena,
        mir_program: &crate::ir::mir::MirProgram,
    ) -> Result<(), CompileError> {
        let resolved: Vec<ResolvedStmt> = stmts
            .iter()
            .map(|stmt| resolve_stmt_for_top_level(resolver_ctx, stmt))
            .collect();

        // Each statement stores its value to a global (`Binding`) or pops
        // it (`Expr`); compute the store targets up front so the borrow
        // of `self.global_names` doesn't collide with the `FnCompiler`.
        let globals = dep_globals.unwrap_or(&self.global_names);
        let store_targets: Vec<Option<u16>> = resolved
            .iter()
            .map(|rs| match rs {
                ResolvedStmt::Binding { name, .. } => Some(globals[name.as_str()]),
                ResolvedStmt::Expr(_) => None,
            })
            .collect();

        // Lower every value expression (the builtin / instantiation
        // tables grow on a clone of the module's program so the BuiltinId /
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
                    // Same rule as `did_not_lower_message`: a name the
                    // resolver could not classify is reportable in the
                    // program's own terms, and the statement carries its
                    // line. The internal error is for everything else.
                    let msg = match crate::ir::hir::collect_unresolved_in_value(value).first() {
                        Some(unresolved) => unresolved
                            .explain("a top-level statement", &unresolved.at_file(source_file)),
                        None => format!(
                            "internal error: a top-level statement did not lower to MIR ({reason:?})"
                        ),
                    };
                    CompileError { msg }
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

        let mut fc = FnCompiler::new(
            chunk_name,
            0,
            0,
            Vec::new(),
            HashMap::new(),
            dep_globals.unwrap_or(&self.global_names),
            module_scope,
            &self.code,
            &mut self.symbols,
            arena,
            symbols,
            Some(&prog),
            &mut self.required_capability_operations,
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
    /// Name → slot table consulted ONLY by the `MirExpr::FnValue` path
    /// (`compile_ident`). Deliberately EMPTY when the fn carries a
    /// resolution: every in-scope local was already rewritten to
    /// `Resolved` by the resolver, so a bare `FnValue` name can never
    /// legitimately be a local, and a name-keyed hit would be an
    /// out-of-scope pattern binder sharing the spelling (issue #948).
    /// Holds the params-only fallback map when the fn has no
    /// resolution (idents never rewritten).
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
    /// (`compile_program`). Used by the walker
    /// to resolve `MirCallee::Builtin(BuiltinId)` back to the
    /// canonical name the VM builtin table keys on. `None` on the
    /// HIR-only path.
    pub(super) mir_program: Option<&'a crate::ir::mir::MirProgram>,
    pub(super) required_capability_operations: &'a mut std::collections::BTreeSet<String>,
    code: Vec<u8>,
    constants: Vec<NanValue>,
    /// Byte offset of the instruction immediately before `last_op_pos`, or
    /// `usize::MAX` when there is no retained predecessor. Peephole recognition
    /// must use instruction boundaries, never bytes that merely look like
    /// opcodes inside another instruction's operands.
    previous_op_pos: usize,
    /// Byte offset of the last emitted opcode (for superinstruction fusion).
    last_op_pos: usize,
    /// Source file path for this function.
    source_file: String,
    /// Run-length encoded line table being built: (bytecode_offset, source_line).
    line_table: Vec<(u32, u32)>,
    /// Last emitted line (for RLE dedup).
    last_noted_line: u32,
    /// Snapshot of `FnResolution.aliased_slots` for the current fn.
    /// Stamped per slot by the IR `alias` pass; backends consume it
    /// rather than re-deriving the same shape per fn. Empty when the
    /// fn was compiled outside the standard pipeline (REPL with no
    /// last-use phase, partial integrations) — the safe-but-slow
    /// reading is "every slot might be aliased" but the VM defaults
    /// to the legacy "everyone owned" behaviour for backwards
    /// compatibility; the alias pass always runs in real builds.
    aliased_slots: std::sync::Arc<Vec<bool>>,
    /// Fields the record literals and updates being compiled may take out of
    /// a local record, innermost last. See `field_take`.
    field_takes: Vec<field_take::FieldTakePlan>,
    /// Field reads of the body being compiled that nothing after them reads
    /// again (`field_moves::movable_projections`). Consulted only for the
    /// target of a matched `Vector.set`; see `field_take`.
    movable_projections: std::collections::HashSet<usize>,
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
        required_capability_operations: &'a mut std::collections::BTreeSet<String>,
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
            required_capability_operations,
            code: Vec::new(),
            constants: Vec::new(),
            previous_op_pos: usize::MAX,
            last_op_pos: usize::MAX,
            source_file: String::new(),
            line_table: Vec::new(),
            last_noted_line: 0,
            aliased_slots: std::sync::Arc::new(Vec::new()),
            field_takes: Vec::new(),
            movable_projections: std::collections::HashSet::new(),
        }
    }

    fn set_aliased_slots(&mut self, aliased: std::sync::Arc<Vec<bool>>) {
        self.aliased_slots = aliased;
    }

    pub(super) fn is_aliased_slot(&self, slot: u32) -> bool {
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
        // Both halves are `u32`: a generated body can pass 64 KiB of
        // bytecode and a generated source can pass 65 535 lines, and a
        // wrapped entry would point an error at an unrelated line.
        let line = u32::try_from(line).unwrap_or(u32::MAX);
        if line == self.last_noted_line {
            return; // RLE dedup
        }
        self.last_noted_line = line;
        let offset = u32::try_from(self.code.len()).unwrap_or(u32::MAX);
        self.line_table.push((offset, line));
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
            if self.previous_op_pos == len - 4
                && self.last_op_pos == len - 3
                && self.code[len - 4] == VECTOR_GET
                && self.code[len - 3] == LOAD_CONST
            {
                let hi = self.code[len - 2];
                let lo = self.code[len - 1];
                self.code[len - 4] = VECTOR_GET_OR;
                self.code[len - 3] = hi;
                self.code[len - 2] = lo;
                self.code.pop(); // remove extra byte
                self.last_op_pos = len - 4;
                // The instruction before the old VECTOR_GET is not tracked;
                // the next normally appended opcode will establish the pair
                // again. No current fusion needs to look farther back.
                self.previous_op_pos = usize::MAX;
                return;
            }
        }
        self.previous_op_pos = self.last_op_pos;
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

    /// Index of `val` in this chunk's constant pool, appended on first
    /// use. `LOAD_CONST` and its fused forms carry a `u16` index, so a
    /// chunk needing more distinct constants than that refuses to compile.
    pub(super) fn add_constant(&mut self, val: NanValue) -> Result<u16, CompileError> {
        if let Some(i) = self.constants.iter().position(|c| c.bits() == val.bits()) {
            // Only indices that passed the check below are ever stored.
            return operand_u16(i, "constant index");
        }
        let idx = operand_u16(
            self.constants.len(),
            &format!(
                "function `{}` needs constant {}",
                self.name,
                self.constants.len()
            ),
        )?;
        self.constants.push(val);
        Ok(idx)
    }

    pub(super) fn offset(&self) -> usize {
        self.code.len()
    }

    pub(super) fn code_mut(&mut self) -> &mut Vec<u8> {
        &mut self.code
    }

    /// A relative jump offset, as every jump and match-fail operand carries
    /// it: a big-endian `i32`. Sixteen bits were once enough and then were
    /// not — a generated function body past 32 KiB of bytecode wrapped a
    /// forward jump into a backward one — so the offset is as wide as any
    /// function body the compiler can emit.
    pub(super) fn emit_i32(&mut self, val: i32) {
        self.emit_u32(val as u32);
    }

    pub(super) fn emit_jump(&mut self, op: u8) -> usize {
        self.emit_op(op);
        let patch_pos = self.code.len();
        self.emit_i32(0);
        patch_pos
    }

    pub(super) fn patch_jump(&mut self, patch_pos: usize) {
        let target = self.code.len();
        self.patch_jump_to(patch_pos, target);
    }

    /// Point the offset at `patch_pos` at `target`. The offset counts from
    /// the byte after the four offset bytes, which is where `ip` stands when
    /// the VM reads it.
    pub(super) fn patch_jump_to(&mut self, patch_pos: usize, target: usize) {
        let offset = jump_offset(patch_pos + 4, target);
        self.code[patch_pos..patch_pos + 4].copy_from_slice(&offset.to_be_bytes());
    }
}

/// The relative offset from `from` to `target`. A function body cannot reach
/// two gigabytes of bytecode, so running out of `i32` is a compiler bug, and
/// it stops here rather than wrapping into a jump somewhere else.
pub(super) fn jump_offset(from: usize, target: usize) -> i32 {
    i32::try_from(target as isize - from as isize)
        .expect("a VM function body is larger than a relative jump can span")
}

#[cfg(test)]
mod tests {
    use super::{FnCompiler, compile_program};
    use crate::ir::SymbolTable;
    use crate::ir::hir::resolve_program;
    use crate::nan_value::Arena;
    use crate::source::parse_source;
    use crate::vm::opcode::{
        LOAD_CONST, LT, RECORD_GET_NAMED, UNWRAP_OR, VECTOR_GET, VECTOR_GET_OR, VECTOR_SET_OR_KEEP,
    };
    use crate::vm::symbol::VmSymbolTable;
    use crate::vm::types::CodeStore;
    use std::collections::{BTreeSet, HashMap};

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

    fn embedded_dependency(source: &str) -> crate::source::LoadedModule {
        crate::source::LoadedModule {
            dep_name: "Kernel.Lib".to_string(),
            items: parse_source(source).expect("source should parse"),
            path: std::path::PathBuf::from("<aver-stdlib>/kernel/lib.av"),
            check_errors: Vec::new(),
        }
    }

    /// A dependency the program does not report as a unit of its own (an
    /// embedded module) whose check fails keeps its nested patterns. The
    /// compiler refuses it with the module's own errors before HIR resolve,
    /// which has no form for them, ever sees it.
    #[test]
    fn dependency_that_failed_its_check_is_refused_with_its_errors() {
        let source = r#"
module Lib
    intent = "t"
    exposes [fresh]

fn taken(xs: List<Int>) -> Int
    ? "t"
    match xs
        [a, b, ..rest] -> a + b
        _ -> 0

fn fresh(taken: List<Int>) -> Int
    ? "t"
    match taken
        [a, ..rest] -> a
        _ -> 0
"#;
        let mut modules = vec![embedded_dependency(source)];
        let (errors, failed) = crate::ir::pipeline::lower_loaded_process_modules(
            &mut modules,
            None,
            &crate::config::MarkedCapabilities::default(),
        );
        assert!(!errors.is_empty() && failed.contains(&0), "{errors:?}");
        let error = super::refuse_failed_dependency(&modules)
            .expect_err("a dependency that failed its check must be refused");
        assert!(error.msg.contains("'Kernel.Lib'"), "{}", error.msg);
        assert!(
            error
                .msg
                .contains("the parameter 'taken' shadows the function 'taken'"),
            "{}",
            error.msg
        );
    }

    #[test]
    fn dependency_whose_nested_patterns_were_compiled_is_accepted() {
        let source = r#"
module Lib
    intent = "t"
    exposes [first]

fn first(xs: List<Int>) -> Int
    ? "t"
    match xs
        [a, b, ..rest] -> a + b
        _ -> 0
"#;
        let mut modules = vec![embedded_dependency(source)];
        let (errors, _) = crate::ir::pipeline::lower_loaded_process_modules(
            &mut modules,
            None,
            &crate::config::MarkedCapabilities::default(),
        );
        assert!(errors.is_empty(), "{errors:?}");
        assert!(super::refuse_failed_dependency(&modules).is_ok());
    }

    /// The verdict, not the module's shape, decides: a dependency with no
    /// nested pattern and no process fails its check just the same, and is
    /// refused just the same, for a shadowing parameter and for a plain type
    /// error.
    #[test]
    fn flat_dependency_that_failed_its_check_is_refused_with_its_errors() {
        for (source, expected) in [
            (
                r#"
module Lib
    intent = "t"
    exposes [fresh]

fn taken(n: Int) -> Int
    ? "t"
    n + 1

fn fresh(taken: Int) -> Int
    ? "t"
    taken + 2
"#,
                "the parameter 'taken' shadows the function 'taken'",
            ),
            (
                r#"
module Lib
    intent = "t"
    exposes [fresh]

fn fresh(n: Int) -> Int
    ? "t"
    n + "one"
"#,
                "lib.av:",
            ),
        ] {
            let mut modules = vec![embedded_dependency(source)];
            let (errors, failed) = crate::ir::pipeline::lower_loaded_process_modules(
                &mut modules,
                None,
                &crate::config::MarkedCapabilities::default(),
            );
            // Nothing to lower, so the lowering itself reports no failure;
            // the module's own verdict still says it failed.
            assert!(errors.is_empty() && failed.is_empty(), "{errors:?}");
            assert!(!modules[0].check_errors.is_empty());
            let error = super::refuse_failed_dependency(&modules)
                .expect_err("a dependency that failed its check must be refused");
            assert!(error.msg.contains("'Kernel.Lib'"), "{}", error.msg);
            assert!(error.msg.contains(expected), "{}", error.msg);
        }
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
    fn unwrap_or_fusion_does_not_match_an_opcode_byte_inside_an_operand() {
        let global_names = HashMap::new();
        let module_scope = HashMap::new();
        let code_store = CodeStore::new();
        let mut vm_symbols = VmSymbolTable::default();
        let mut arena = Arena::new();
        let source_symbols = SymbolTable::default();
        let mut required_capabilities = BTreeSet::new();
        let mut compiler = FnCompiler::new(
            "operand_boundary_probe",
            0,
            0,
            Vec::new(),
            HashMap::new(),
            &global_names,
            &module_scope,
            &code_store,
            &mut vm_symbols,
            &mut arena,
            &source_symbols,
            None,
            &mut required_capabilities,
        );

        // The low byte of this field-symbol operand is deliberately 0x82,
        // the VECTOR_GET opcode. Before #1039, the UNWRAP_OR peephole looked
        // four raw bytes backwards and mistook that operand byte for an
        // instruction, changing the field id to 0x83 and deleting a byte.
        compiler.emit_op(RECORD_GET_NAMED);
        compiler.emit_u32(VECTOR_GET as u32);
        compiler.emit_op(LOAD_CONST);
        compiler.emit_u16(0);
        compiler.emit_op(UNWRAP_OR);

        let chunk = compiler.finish();
        assert_eq!(
            chunk.code,
            vec![
                RECORD_GET_NAMED,
                0,
                0,
                0,
                VECTOR_GET,
                LOAD_CONST,
                0,
                0,
                UNWRAP_OR,
            ]
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

    /// Globals hold module bindings only. A fn takes no slot, so the global
    /// count does not grow with the number of fns — `aver verify` adds two or
    /// three fns per case, and when every fn took a `u16` global slot a large
    /// `given` domain pushed the index past 65 535: it wrapped onto a slot in
    /// use and a fn read as a value (`apply(f, x)`) called a verify helper.
    #[test]
    fn fns_take_no_global_slots_and_fn_values_still_resolve() {
        let mut source = String::from(
            r#"
module Demo

base = 40

fn f(x: Int) -> Int
    base + x

fn apply(g: Fn(Int) -> Int, x: Int) -> Int
    g(x)

fn useF() -> Int
    apply(f, 2)
"#,
        );
        for k in 0..200 {
            source.push_str(&format!("\nfn filler{k}(x: Int) -> Int\n    x + {k}\n"));
        }
        let mut items = parse_source(&source).expect("source should parse");
        crate::ir::pipeline::tco(&mut items);
        crate::ir::pipeline::resolve(&mut items);
        let symbols = SymbolTable::build(&items, &[]);
        let resolved = resolve_program(&symbols, &items);
        let mut arena = Arena::new();
        let (code, globals) =
            compile_program(&resolved, &symbols, &mut arena, None).expect("vm compile should pass");
        assert_eq!(globals.len(), 1, "only `base` takes a global slot");

        let mut vm = crate::vm::VM::new(code, globals, arena);
        vm.run_top_level().expect("top level runs");
        let result = vm.run_named_function("useF", &[]).expect("useF runs");
        assert_eq!(result.as_int(&vm.arena), 42);
    }

    /// Past the last `u16` global index the compiler refuses instead of
    /// handing a binding a slot another binding already holds.
    #[test]
    fn global_slot_past_u16_refuses_to_compile() {
        let mut compiler = super::ProgramCompiler::new();
        compiler
            .globals
            .resize(usize::from(u16::MAX), crate::nan_value::NanValue::UNIT);
        assert_eq!(
            compiler.ensure_global("last").expect("slot 65535 fits"),
            u16::MAX
        );
        assert_eq!(
            compiler
                .ensure_global("last")
                .expect("same name, same slot"),
            u16::MAX
        );
        let err = compiler
            .ensure_global("oneTooMany")
            .expect_err("slot 65536 does not fit a u16 operand");
        assert!(
            err.msg
                .contains("module binding `oneTooMany` needs global slot 65536")
                && err.msg.contains("at most 65536"),
            "{}",
            err.msg
        );
        assert!(!compiler.global_names.contains_key("oneTooMany"));
    }

    /// The constant pool refuses its 65 537th distinct entry rather than
    /// handing out an index that names constant 0.
    #[test]
    fn constant_past_u16_refuses_to_compile() {
        let global_names = HashMap::new();
        let module_scope = HashMap::new();
        let code_store = CodeStore::new();
        let mut vm_symbols = VmSymbolTable::default();
        let mut arena = Arena::new();
        let source_symbols = SymbolTable::default();
        let mut required_capabilities = BTreeSet::new();
        let mut compiler = FnCompiler::new(
            "many_constants",
            0,
            0,
            Vec::new(),
            HashMap::new(),
            &global_names,
            &module_scope,
            &code_store,
            &mut vm_symbols,
            &mut arena,
            &source_symbols,
            None,
            &mut required_capabilities,
        );
        let unit = crate::nan_value::NanValue::UNIT;
        compiler.constants = vec![unit; usize::from(u16::MAX) + 1];
        assert_eq!(compiler.add_constant(unit).expect("existing constant"), 0);
        let err = compiler
            .add_constant(crate::nan_value::NanValue::TRUE)
            .expect_err("constant 65536 does not fit a u16 operand");
        assert!(
            err.msg.contains("`many_constants` needs constant 65536"),
            "{}",
            err.msg
        );
    }

    #[test]
    fn fn_and_type_ids_past_u16_refuse() {
        assert_eq!(super::fn_id_operand(65_535, "f").expect("fits"), u16::MAX);
        let err = super::fn_id_operand(65_536, "helper").expect_err("does not fit");
        assert!(
            err.msg
                .contains("call to `helper` targets function id 65536"),
            "{}",
            err.msg
        );
        let err = super::type_id_operand(70_000, "Big").expect_err("does not fit");
        assert!(
            err.msg.contains("type `Big` has arena type id 70000"),
            "{}",
            err.msg
        );
    }

    /// A field past slot 255 gets no slot instead of a wrapped one that
    /// would read field 0.
    #[test]
    fn record_field_past_u8_slots_gets_no_slot() {
        let mut code = CodeStore::new();
        let fields: Vec<u32> = (1000..1257).collect();
        code.register_record_fields(0, &fields);
        assert_eq!(code.record_field_slots.get(&(0, 1255)), Some(&255));
        assert_eq!(code.record_field_slots.get(&(0, 1256)), None);
        assert_eq!(code.record_field_slots.len(), 256);
    }
}

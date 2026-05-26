//! Ordered compiler pass pipeline — the single source of truth for what
//! happens between `parse_*` and `codegen::*` / `vm::*`.
//!
//! Two layers of API:
//!
//! - **Per-stage entry points** (`pipeline::tco`, `pipeline::typecheck`,
//!   `pipeline::interp_lower`, `pipeline::buffer_build`, `pipeline::resolve`)
//!   — each pass exposed individually. Diagnostic and test sites that only
//!   need one or two passes call these directly. There is no other path
//!   into a pass; `crate::tco::transform_program` etc. are still public
//!   internally but new code should not reach for them.
//!
//! - **Pipeline orchestrator** (`pipeline::run`) — walks all five stages
//!   in fixed order, gating each on a per-stage boolean in
//!   [`PipelineConfig`]. Stages that are off are skipped silently. This
//!   is what `aver run`, `aver compile`, replay, and the playground use.
//!
//! Stages are fixed-order. Buffer-build needs `Expr::TailCall` from TCO,
//! the resolver assumes traversal lowering is done; what is configurable
//! is which stages run, not their ordering. There is **no** bundled
//! "traversal lowering" toggle — `run_interp_lower` and `run_buffer_build`
//! are independent flags so callers can mix them however they need.

use crate::ast::TopLevel;
use crate::ir::buffer_build::BufferBuildPassReport;
use crate::ir::pass_diag::{self, CountsByFn};
use crate::ir::{AllocPolicy, AnalysisResult, CallLowerCtx};
use crate::source::LoadedModule;
use crate::types::checker::{
    TypeCheckResult, run_type_check_full, run_type_check_full_self_host,
    run_type_check_with_loaded, run_type_check_with_loaded_self_host,
};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum PipelineStage {
    Tco,
    Typecheck,
    InterpLower,
    BufferBuild,
    Resolve,
    LastUse,
    Analyze,
    Escape,
    /// Refinement-via-opaque lift — type-level proof-export stage.
    /// Walks user type defs + smart constructors, populates
    /// `ProofIR.refined_types`. Opt-in via `run_refinement_lower`;
    /// proof exporters (Lean → subtype, Dafny → subset type) enable
    /// it, runtime backends leave it off.
    RefinementLower,
    /// Recursion-shape contract derivation — fn-level proof-export
    /// stage. Walks recursion plans, populates `ProofIR.fn_contracts`
    /// with Fuel / Native decisions and proof obligations. Opt-in via
    /// `run_contract_lower`. Independent of `RefinementLower` — a
    /// backend could enable one without the other, even though the
    /// production proof exporters always want both.
    ContractLower,
    /// Verify-law universal theorem lowering — walks every
    /// `VerifyKind::Law(law)` in the source, extracts quantifiers
    /// from `givens`, premises from `when`, and the claim from
    /// `lhs == rhs`, then populates `ProofIR.law_theorems`. Strategy
    /// pinning (Reflexive / Induction / SimpOverLemmas / …) lives
    /// in subsequent migration steps; the initial drop carries
    /// `ProofStrategy::BackendDispatch` and the consumer's ad-hoc
    /// chain still decides.
    LawLower,
    /// Build the resolved-identity table (`SymbolTable`) — opaque
    /// `FnId` / `TypeId` / `CtorId` / `ModuleId` for every named
    /// declaration. Phase E of #138; populates
    /// `PipelineResult.symbol_table`. No consumer reads it from
    /// the pipeline output yet, but downstream migration PRs
    /// (proof IR maps keyed by `FnId`, backend mangling by
    /// `FnId → name`) all depend on this stage running.
    BuildSymbols,
}

impl PipelineStage {
    pub const fn name(self) -> &'static str {
        match self {
            Self::Tco => "tco",
            Self::Typecheck => "typecheck",
            Self::InterpLower => "interp_lower",
            Self::BufferBuild => "buffer_build",
            Self::Resolve => "resolve",
            Self::LastUse => "last_use",
            Self::Analyze => "analyze",
            Self::Escape => "escape",
            Self::RefinementLower => "refinement_lower",
            Self::ContractLower => "contract_lower",
            Self::LawLower => "law_lower",
            Self::BuildSymbols => "build_symbols",
        }
    }
}

/// Hook callback fired after every pipeline stage that ran. Receives the
/// stage label and the (post-mutation) item slice. Drives `--emit-ir-after=PASS`.
pub type AfterPassHook<'a> = Box<dyn FnMut(PipelineStage, &[TopLevel]) + 'a>;

/// Optional typecheck driver.
pub enum TypecheckMode<'a> {
    /// `run_type_check_full(items, base_dir)`.
    Full { base_dir: Option<&'a str> },
    /// `run_type_check_with_loaded(items, loaded)` for in-memory module trees
    /// (playground virtual fs, multi-file ad-hoc compiles).
    WithLoaded(&'a [LoadedModule]),
    /// Self-host variant of [`Full`] — bypasses opaque-type checks
    /// (construct, field access, pattern match). Used exclusively by
    /// `aver compile --with-self-host-support` so `domain/builtins.av`
    /// can round-trip opaque host types through the replay JSON
    /// contract. See [`run_type_check_full_self_host`](crate::types::checker::run_type_check_full_self_host).
    FullSelfHost { base_dir: Option<&'a str> },
    /// Self-host variant of [`WithLoaded`].
    WithLoadedSelfHost(&'a [LoadedModule]),
}

pub struct PipelineConfig<'a> {
    pub run_tco: bool,
    /// `Some(mode)` runs the type checker with that driver; `None` skips it.
    pub typecheck: Option<TypecheckMode<'a>>,
    pub run_interp_lower: bool,
    pub run_buffer_build: bool,
    pub run_resolve: bool,
    /// Whether to run the last-use ownership annotation pass after
    /// `resolve`. Annotates each `Expr::Resolved` slot reference with
    /// `last_use: bool`; backends use it to MOVE instead of COPY
    /// (VM `MOVE_LOCAL`, Rust skips `.clone()`, owned-mutate fast paths).
    /// Independent of `run_resolve`: enabling LastUse without Resolve is
    /// a no-op (no resolved slots to annotate); skipping LastUse keeps
    /// every reference pessimistically marked as "not last".
    pub run_last_use: bool,
    /// Whether to run the IR-level analysis pass after `last_use`. The
    /// pass is read-only — it populates `PipelineResult.analysis` with
    /// per-fn body shape, thin-kind, locals count, and (when an
    /// `alloc_policy` is configured) policy-parametrized alloc info.
    pub run_analyze: bool,
    /// Whether to run the escape-analysis rewriting pass after
    /// `analyze`. Detects `FnCall(f, [RecordCreate{…}])` where `f`
    /// only accesses the record via `Attr` and inlines `f`'s body
    /// with field substitution — eliminates the fresh struct alloc
    /// per call. Backend-agnostic: every backend benefits because
    /// the `RecordCreate` simply disappears from the IR.
    /// Proof exporters (Lean / Dafny) want the source-level shape
    /// preserved and skip this stage.
    pub run_escape: bool,
    /// Whether to run the refinement-lift pass. Walks user type defs
    /// and smart constructors, populating `ProofIR.refined_types`.
    /// Independent of `run_contract_lower` — a backend could opt into
    /// one without the other, though production proof exporters
    /// always enable both. Both stages share the same
    /// `PipelineResult.proof_ir` output sink.
    pub run_refinement_lower: bool,
    /// Whether to run the recursion-contract derivation pass. Walks
    /// recursion plans (Fuel / Native shapes) and populates
    /// `ProofIR.fn_contracts`. Independent of `run_refinement_lower`.
    pub run_contract_lower: bool,
    /// Whether to run the verify-law theorem lowering pass. Walks
    /// `VerifyKind::Law` blocks and populates `ProofIR.law_theorems`.
    /// Independent of the other proof stages; production proof
    /// exporters always enable it alongside refinement_lower and
    /// contract_lower.
    pub run_law_lower: bool,
    /// Whether to run the symbol-table build pass (#138 phase E).
    /// Populates `PipelineResult.symbol_table` with opaque IDs for
    /// every named declaration. Cheap to run unconditionally —
    /// pure traversal of entry items + dep modules, no analysis.
    /// Independent of every other stage; future consumers (proof
    /// IR maps keyed by `FnId`, resolved AST emit) require it on.
    pub run_build_symbols: bool,
    /// Pre-loaded dependent modules. Carried alongside items so the
    /// proof-lower pass can walk both the entry and dep type/fn defs
    /// (refinement records live in either; cross-module recursion is
    /// classified together). Caller pre-loads via
    /// `load_compile_deps` / `loaded_to_module_info` before running
    /// the pipeline. Empty slice when the source is single-file or
    /// when no dep needs proof-side analysis.
    pub dep_modules: &'a [crate::codegen::ModuleInfo],
    /// Allocation policy used by `analyze`. `None` skips the alloc-info
    /// computation; every other analysis fact is still produced.
    /// Backends should pass their own policy (`VmAllocPolicy`,
    /// `WasmAllocPolicy`); diagnostic tools that don't have a backend
    /// in mind can pass `None` or use the dump module's conservative
    /// default.
    pub alloc_policy: Option<&'a dyn AllocPolicy>,
    /// `CallLowerCtx` for the body classifier. `None` uses a conservative
    /// stub that knows nothing about local symbols / module paths — fine
    /// for diagnostic dumps; codegen pipelines should pass a real ctx so
    /// the classifier returns its full set of body shapes.
    pub call_ctx: Option<&'a dyn CallLowerCtx>,
    /// Hook fired after every stage that ran.
    pub on_after_pass: Option<AfterPassHook<'a>>,
}

impl<'a> Default for PipelineConfig<'a> {
    fn default() -> Self {
        Self {
            run_tco: true,
            typecheck: None,
            run_interp_lower: true,
            run_buffer_build: true,
            run_resolve: true,
            run_last_use: true,
            run_analyze: true,
            run_escape: true,
            run_refinement_lower: false,
            run_contract_lower: false,
            run_law_lower: false,
            run_build_symbols: false,
            dep_modules: &[],
            alloc_policy: None,
            call_ctx: None,
            on_after_pass: None,
        }
    }
}

/// Typed per-pass report. Each variant carries the structured facts a
/// CI gate cares about — counts, fn names, error messages — so scripts
/// don't have to regex-parse human-readable summaries. The text and
/// JSON renderers in `cmd_explain_passes` derive their output from
/// these values.
#[derive(Debug, Clone)]
pub enum PassReport {
    Tco {
        /// Total tail-call rewrites this pass made.
        tail_calls_added: usize,
        /// Per-fn before/after, alphabetised by `name`.
        fns_changed: Vec<FnCountChange>,
        /// Recursive callsites that did NOT convert (still call the fn
        /// in non-tail position). Empty when the pass was clean.
        non_tail_recursive: Vec<NonTailEntry>,
    },
    Typecheck {
        items_checked: usize,
        errors: usize,
        /// Up to the first 5 error messages (full list still on
        /// `PipelineResult.typecheck`).
        error_messages: Vec<String>,
    },
    InterpLower {
        interpolations_lowered: usize,
        /// Per-fn before/after counts for fns whose interpolation count
        /// dropped during the pass.
        fns_changed: Vec<FnCountChange>,
    },
    BufferBuild(BufferBuildPassReport),
    Resolve {
        slots_resolved: usize,
        fns_with_slots: usize,
        /// Total slot count across all fns whose resolver populated a
        /// type (one entry per `FnResolution.local_slot_types` element).
        slot_types_total: usize,
        /// Slots whose type came back `Type::Invalid` — typically
        /// wildcards / `_` patterns the resolver counted but never
        /// produced into. Surfaces unhandled pattern shapes (e.g.
        /// future variant kinds the slot-types pass hasn't taught
        /// itself yet) as a non-zero counter.
        slot_types_invalid: usize,
    },
    LastUse {
        last_use_marked: usize,
        total_resolved: usize,
    },
    Analyze {
        total_fns: usize,
        no_alloc_fns: usize,
        recursive_fns: usize,
        mutual_tco_members: usize,
        /// Fns whose `allocates` is `None` because no `alloc_policy` was
        /// configured. Surfaces a misconfigured pipeline run.
        unknown_alloc: usize,
    },
    Escape {
        /// How many `FnCall(callee, [RecordCreate{…}])` sites the
        /// pass rewrote into the inlined-and-substituted body.
        rewrites: usize,
    },
    RefinementLower {
        /// User types lifted into refinement subtypes (records with
        /// a single carrier field + matching smart constructor).
        refined_types: usize,
    },
    ContractLower {
        /// Pure fns with a non-trivial `RecursionContract`. Currently
        /// only covers the IntCountdownGuarded shape; extends as more
        /// `RecursionPlan` variants migrate into ProofIR.
        fn_contracts: usize,
    },
    LawLower {
        /// Verify-law theorems lowered. Each entry pairs `(fn, law)`
        /// with quantifiers + premises + claim.
        law_theorems: usize,
    },
    BuildSymbols {
        /// Modules in the table (always ≥ 1 — entry scope counts).
        modules: usize,
        /// Total fn declarations across entry + dep modules.
        fns: usize,
        /// Total type declarations across entry + dep modules.
        types: usize,
        /// Total constructors (record + sum variants) across all
        /// type declarations.
        ctors: usize,
    },
}

/// Per-fn counter delta — used by `Tco` and `InterpLower` reports to
/// surface which functions a pass actually changed.
#[derive(Debug, Clone)]
pub struct FnCountChange {
    pub name: String,
    pub before: usize,
    pub after: usize,
}

/// One non-tail recursive callsite the TCO pass couldn't rewrite.
#[derive(Debug, Clone)]
pub struct NonTailEntry {
    pub fn_name: String,
    pub recursive_calls: usize,
    pub line: usize,
}

/// Per-pass diagnostic record. `report` carries the typed facts; `stage`
/// labels which pass produced them. Drives `aver compile --explain-passes`
/// and the future failable-invariant CI checks (`fail if buffer_build
/// no longer fires on the canonical shape`, `fail if hot fn loses
/// no-alloc status`, etc.).
#[derive(Debug, Clone)]
pub struct PassDiagnostic {
    pub stage: PipelineStage,
    pub report: PassReport,
}

#[derive(Default)]
pub struct PipelineResult {
    /// Typecheck output, present iff `config.typecheck` was set. Callers
    /// inspect `.errors` and decide what to do — the orchestrator does not
    /// exit on its own.
    pub typecheck: Option<TypeCheckResult>,
    /// Buffer-build pass report — sinks fired, synthesized fns,
    /// per-sink rewrite counts. `None` when the pass was disabled.
    pub buffer_build: Option<BufferBuildPassReport>,
    /// IR-level analysis facts (per-fn body shape, thin kind, alloc info)
    /// when `run_analyze` was on. `None` when the stage was disabled.
    pub analysis: Option<AnalysisResult>,
    /// Backend-neutral proof-export decisions populated by the
    /// `RefinementLower` / `ContractLower` stages. `Some` when at
    /// least one of the two ran; carries whichever fields were
    /// populated by the stages that ran (an opt-in to only
    /// RefinementLower leaves `fn_contracts` empty, vice versa).
    pub proof_ir: Option<crate::ir::ProofIR>,
    /// Resolved-identity table — opaque `FnId` / `TypeId` /
    /// `CtorId` / `ModuleId` for every named declaration in the
    /// program (#138 phase E). `Some` when `run_build_symbols`
    /// was on. No consumer reads it through this field yet;
    /// downstream migration PRs (proof IR maps keyed by `FnId`,
    /// backend mangling) will start once the wire-up is in place.
    pub symbol_table: Option<crate::ir::SymbolTable>,
    /// Per-stage diagnostic records — one per pass that actually ran.
    /// Drives `aver compile --explain-passes`; consumed by the future
    /// CI failable-invariant checks.
    pub pass_diagnostics: Vec<PassDiagnostic>,
}

// ── Per-stage entry points ──────────────────────────────────────────
//
// Three argument shapes, each reflecting what the stage actually does:
//
//   `&[TopLevel]`      — read-only (typecheck)
//   `&mut [TopLevel]`  — mutate in place (tco, interp_lower, resolve)
//   `&mut Vec<TopLevel>` — mutate and append (buffer_build synthesizes
//                          new top-level fn defs)
//
// Looks inconsistent on the surface but the categories are real. Faking
// uniformity by forcing `&mut Vec` everywhere triggers `clippy::ptr_arg`
// for good reason: it lies about what the function does. Callers always
// have a `Vec<TopLevel>` so passing `&mut items` works for every shape.

/// Tail-call rewrite pass.
pub fn tco(items: &mut [TopLevel]) {
    crate::tco::transform_program(items);
}

/// Run the type checker against `items` using the provided driver.
pub fn typecheck(items: &[TopLevel], mode: &TypecheckMode<'_>) -> TypeCheckResult {
    match mode {
        TypecheckMode::Full { base_dir } => run_type_check_full(items, *base_dir),
        TypecheckMode::WithLoaded(loaded) => run_type_check_with_loaded(items, loaded),
        TypecheckMode::FullSelfHost { base_dir } => run_type_check_full_self_host(items, *base_dir),
        TypecheckMode::WithLoadedSelfHost(loaded) => {
            run_type_check_with_loaded_self_host(items, loaded)
        }
    }
}

/// Lower `"a${x}b"` interpolation literals into the buffer pipeline.
/// Skipped by proof exporters (Lean/Dafny) which want the source-level form.
pub fn interp_lower(items: &mut [TopLevel]) {
    crate::ir::lower_interpolation_pass(items);
}

/// Buffer-build deforestation pass — detects `String.join(<builder>(args, []), sep)`
/// shapes, rewrites them to `__buf_finalize(<builder>__buffered(...))`, and
/// appends the synthesized buffered variants to `items`. Returns a
/// [`BufferBuildPassReport`] describing what fired.
pub fn buffer_build(items: &mut Vec<TopLevel>) -> BufferBuildPassReport {
    crate::ir::run_buffer_build_pass(items)
}

/// Resolve local bindings — maps `Expr::Ident(name)` → `Expr::Resolved { slot, .. }`
/// per fn. Does not annotate last-use; that's a separate stage.
pub fn resolve(items: &mut [TopLevel]) {
    crate::resolver::resolve_program(items);
}

/// Last-use ownership annotation. Walks each fn body backwards, sets
/// `last_use: true` on every `Expr::Resolved` whose slot is not
/// referenced again afterwards. Requires `Resolve` to have run; on
/// pre-resolve IR it's a no-op (no resolved slots to annotate).
pub fn last_use(items: &mut [TopLevel]) {
    crate::ir::last_use::annotate_program_last_use(items);
}

// ── Orchestrator ────────────────────────────────────────────────────

/// Run the canonical compiler pipeline on `items`. Each stage is gated
/// on its corresponding `PipelineConfig` flag — disabled stages are
/// skipped without complaint.
///
/// If typecheck runs and surfaces errors, later stages are skipped so
/// callers can render diagnostics without seeing partially-lowered IR.
/// The typecheck result still lands in `PipelineResult::typecheck`.
pub fn run(items: &mut Vec<TopLevel>, mut cfg: PipelineConfig<'_>) -> PipelineResult {
    let mut result = PipelineResult::default();

    if cfg.run_tco {
        let pre = pass_diag::collect(items);
        tco(items);
        let post = pass_diag::collect(items);
        result
            .pass_diagnostics
            .push(diag_for_tco(&pre, &post, items));
        fire(&mut cfg, PipelineStage::Tco, items);
    }

    if let Some(mode) = cfg.typecheck.as_ref() {
        let tc = typecheck(items, mode);
        let has_errors = !tc.errors.is_empty();
        result
            .pass_diagnostics
            .push(diag_for_typecheck(&tc, items.len()));
        result.typecheck = Some(tc);
        fire(&mut cfg, PipelineStage::Typecheck, items);
        if has_errors {
            return result;
        }
    }

    if cfg.run_interp_lower {
        let pre = pass_diag::collect(items);
        interp_lower(items);
        let post = pass_diag::collect(items);
        result
            .pass_diagnostics
            .push(diag_for_interp_lower(&pre, &post));
        fire(&mut cfg, PipelineStage::InterpLower, items);
    }

    if cfg.run_buffer_build {
        let report = buffer_build(items);
        result.pass_diagnostics.push(diag_for_buffer_build(&report));
        result.buffer_build = Some(report);
        fire(&mut cfg, PipelineStage::BufferBuild, items);
    }

    if cfg.run_resolve {
        resolve(items);
        let post = pass_diag::collect(items);
        result.pass_diagnostics.push(diag_for_resolve(&post, items));
        fire(&mut cfg, PipelineStage::Resolve, items);
    }

    if cfg.run_analyze {
        // The body classifier needs a `CallLowerCtx`. When no real ctx is
        // configured we use `StubCallCtx`, which under-classifies `direct`
        // shapes (a body that calls a fn whose name looks like a local
        // gets seen as a generic call). Acceptable for `--emit-ir` dumps;
        // codegen pipelines should plumb a real ctx through `cfg.call_ctx`
        // once the inliner needs accurate body shape data.
        let adapter = CallCtxAdapter(cfg.call_ctx);
        let analysis = crate::ir::analyze(items, cfg.alloc_policy, &adapter);
        result.pass_diagnostics.push(diag_for_analyze(&analysis));
        result.analysis = Some(analysis);
        fire(&mut cfg, PipelineStage::Analyze, items);
    }

    if cfg.run_escape {
        let rewrites = crate::ir::escape::run(items);
        result.pass_diagnostics.push(diag_for_escape(rewrites));
        fire(&mut cfg, PipelineStage::Escape, items);
    }

    // `last_use` runs *after* every rewrite pass — escape duplicates
    // `Attr(p, field)` into each use site, so a "last use" annotation
    // computed pre-inline would become stale on the duplicated reads,
    // and the VM's `MOVE_LOCAL` (emitted on `last_use=true`) would
    // clear the slot before the second read sees it. Annotating once
    // at the end keeps the pass cheap (single forward walk) and means
    // every backend reads markers that match the IR they actually
    // codegen against.
    if cfg.run_last_use {
        last_use(items);
        let post = pass_diag::collect(items);
        result.pass_diagnostics.push(diag_for_last_use(&post));
        fire(&mut cfg, PipelineStage::LastUse, items);
        // Alias-slot annotation rides on the same gate — backends only
        // ever consume `aliased_slots` together with `last_use` (the
        // VM's owned-mask uses both, the wasm-gc clone-on-write skip
        // uses both). No public stage flag yet; the data is opt-in by
        // backends via `FnResolution.aliased_slots`.
        crate::ir::alias::annotate_program_alias_slots(items);
    }

    // Proof-export lowerings come last — they share an output sink
    // (`result.proof_ir`) but populate disjoint fields:
    // RefinementLower writes `refined_types`, ContractLower writes
    // `fn_contracts`, LawLower writes `law_theorems`. Each is
    // independently opt-in.
    // BuildSymbols runs BEFORE proof stages so `populate_*` resolve
    // `FnKey`/`TypeKey` to `FnId`/`TypeId` once at the IR boundary
    // and key `ProofIR.fn_contracts` / `ProofIR.refined_types` /
    // `LawTheorem.fn_id` by the opaque IDs (#138 phase E).
    //
    // Proof stages have a hard dependency on the symbol table —
    // since fn_contracts is keyed by `FnId`, populate cannot run
    // without resolved identity. Auto-enable here so callers can't
    // accidentally turn on `run_contract_lower` / `run_law_lower`
    // without symbols and silently produce an empty proof IR.
    let needs_symbols = cfg.run_build_symbols
        || cfg.run_refinement_lower
        || cfg.run_contract_lower
        || cfg.run_law_lower;
    if needs_symbols {
        let symbol_table = crate::ir::SymbolTable::build(items, cfg.dep_modules);
        result
            .pass_diagnostics
            .push(diag_for_build_symbols(&symbol_table));
        result.symbol_table = Some(symbol_table);
        fire(&mut cfg, PipelineStage::BuildSymbols, items);
    }

    if cfg.run_refinement_lower || cfg.run_contract_lower || cfg.run_law_lower {
        // Round-5: union entry's analyze with each dep module's
        // `analysis.recursive_fns`. Without this, multi-module proof
        // export's `populate_fn_contracts` only sees entry-recursive
        // fns and classifies module fns as "outside subset" — even
        // when each module's analyze already saw their countdown /
        // structural-recursion shape. Aver's module DAG invariant
        // rules out cross-module recursion SCCs, so unioning the
        // per-scope `recursive_fns` sets matches the build-time
        // `ctx.recursive_fns` view in `codegen::build_context`.
        let recursive_fns_owned: std::collections::HashSet<String> = {
            let mut set: std::collections::HashSet<String> = result
                .analysis
                .as_ref()
                .map(|a| a.recursive_fns.iter().cloned().collect())
                .unwrap_or_default();
            for m in cfg.dep_modules {
                if let Some(a) = m.analysis.as_ref() {
                    set.extend(a.recursive_fns.iter().cloned());
                }
            }
            set
        };
        let module_prefixes: std::collections::HashSet<String> =
            cfg.dep_modules.iter().map(|m| m.prefix.clone()).collect();
        let inputs = crate::codegen::proof_lower::ProofLowerInputs {
            entry_items: items,
            dep_modules: cfg.dep_modules,
            module_prefixes: &module_prefixes,
            recursive_fns: &recursive_fns_owned,
            symbol_table: result.symbol_table.as_ref(),
        };
        let mut ir = result.proof_ir.take().unwrap_or_default();
        if cfg.run_refinement_lower {
            crate::codegen::proof_lower::populate_refined_types(&inputs, &mut ir);
            result.pass_diagnostics.push(diag_for_refinement_lower(&ir));
            fire(&mut cfg, PipelineStage::RefinementLower, items);
        }
        if cfg.run_contract_lower {
            crate::codegen::proof_lower::populate_fn_contracts(&inputs, &mut ir);
            result.pass_diagnostics.push(diag_for_contract_lower(&ir));
            fire(&mut cfg, PipelineStage::ContractLower, items);
        }
        if cfg.run_law_lower {
            crate::codegen::proof_lower::populate_law_theorems(&inputs, &mut ir);
            result.pass_diagnostics.push(diag_for_law_lower(&ir));
            fire(&mut cfg, PipelineStage::LawLower, items);
        }
        result.proof_ir = Some(ir);
    }

    result
}

/// Bridges the trait-object `cfg.call_ctx: Option<&dyn CallLowerCtx>`
/// into the generic-impl world that the IR classifiers (`classify_call_plan`,
/// `classify_thin_fn_def`, …) expect (`&impl CallLowerCtx`). When the
/// option is `None` every method returns the conservative answer.
struct CallCtxAdapter<'a>(Option<&'a dyn CallLowerCtx>);

impl<'a> CallLowerCtx for CallCtxAdapter<'a> {
    fn is_local_value(&self, name: &str) -> bool {
        self.0.is_some_and(|c| c.is_local_value(name))
    }
    fn is_user_type(&self, name: &str) -> bool {
        self.0.is_some_and(|c| c.is_user_type(name))
    }
    fn resolve_module_call<'b>(&self, dotted: &'b str) -> Option<(&'b str, &'b str)> {
        self.0.and_then(|c| c.resolve_module_call(dotted))
    }
}

fn fire(cfg: &mut PipelineConfig<'_>, stage: PipelineStage, items: &[TopLevel]) {
    if let Some(cb) = cfg.on_after_pass.as_mut() {
        cb(stage, items);
    }
}

// ── PassDiagnostic builders ─────────────────────────────────────────

fn diag_for_tco(pre: &CountsByFn, post: &CountsByFn, items: &[TopLevel]) -> PassDiagnostic {
    let pre_total = pass_diag::total(pre);
    let post_total = pass_diag::total(post);
    let tail_calls_added = post_total.tail_calls.saturating_sub(pre_total.tail_calls);

    let fns_changed: Vec<FnCountChange> = pass_diag::fns_that_grew(pre, post, |c| c.tail_calls)
        .into_iter()
        .map(|name| {
            let before = pre.get(&name).copied().unwrap_or_default().tail_calls;
            let after = post.get(&name).copied().unwrap_or_default().tail_calls;
            FnCountChange {
                name,
                before,
                after,
            }
        })
        .collect();

    let non_tail_recursive: Vec<NonTailEntry> =
        crate::tail_check::collect_non_tail_recursion_warnings(items)
            .into_iter()
            .map(|w| NonTailEntry {
                fn_name: w.fn_name,
                recursive_calls: w.recursive_calls,
                line: w.line,
            })
            .collect();

    PassDiagnostic {
        stage: PipelineStage::Tco,
        report: PassReport::Tco {
            tail_calls_added,
            fns_changed,
            non_tail_recursive,
        },
    }
}

fn diag_for_typecheck(tc: &TypeCheckResult, item_count: usize) -> PassDiagnostic {
    let error_messages = if tc.errors.is_empty() {
        Vec::new()
    } else {
        tc.errors
            .iter()
            .take(5)
            .map(|e| e.message.clone())
            .collect()
    };
    PassDiagnostic {
        stage: PipelineStage::Typecheck,
        report: PassReport::Typecheck {
            items_checked: item_count,
            errors: tc.errors.len(),
            error_messages,
        },
    }
}

fn diag_for_interp_lower(pre: &CountsByFn, post: &CountsByFn) -> PassDiagnostic {
    let interpolations_lowered = pass_diag::total(pre)
        .interpolations
        .saturating_sub(pass_diag::total(post).interpolations);
    let fns_changed: Vec<FnCountChange> = pass_diag::fns_that_grew(post, pre, |c| c.interpolations)
        .into_iter()
        .map(|name| {
            let before = pre.get(&name).copied().unwrap_or_default().interpolations;
            let after = post.get(&name).copied().unwrap_or_default().interpolations;
            FnCountChange {
                name,
                before,
                after,
            }
        })
        .collect();
    PassDiagnostic {
        stage: PipelineStage::InterpLower,
        report: PassReport::InterpLower {
            interpolations_lowered,
            fns_changed,
        },
    }
}

fn diag_for_buffer_build(report: &BufferBuildPassReport) -> PassDiagnostic {
    PassDiagnostic {
        stage: PipelineStage::BufferBuild,
        report: PassReport::BufferBuild(report.clone()),
    }
}

fn diag_for_resolve(post: &CountsByFn, items: &[TopLevel]) -> PassDiagnostic {
    let slots_resolved = pass_diag::total(post).resolved;
    let fns_with_slots = post.values().filter(|c| c.resolved > 0).count();
    let mut slot_types_total = 0usize;
    let mut slot_types_invalid = 0usize;
    for item in items {
        if let TopLevel::FnDef(fd) = item
            && let Some(res) = fd.resolution.as_ref()
        {
            slot_types_total += res.local_slot_types.len();
            slot_types_invalid += res
                .local_slot_types
                .iter()
                .filter(|t| matches!(t, crate::ast::Type::Invalid))
                .count();
        }
    }
    PassDiagnostic {
        stage: PipelineStage::Resolve,
        report: PassReport::Resolve {
            slots_resolved,
            fns_with_slots,
            slot_types_total,
            slot_types_invalid,
        },
    }
}

fn diag_for_last_use(post: &CountsByFn) -> PassDiagnostic {
    let totals = pass_diag::total(post);
    PassDiagnostic {
        stage: PipelineStage::LastUse,
        report: PassReport::LastUse {
            last_use_marked: totals.last_use_resolved,
            total_resolved: totals.resolved,
        },
    }
}

fn diag_for_escape(rewrites: usize) -> PassDiagnostic {
    PassDiagnostic {
        stage: PipelineStage::Escape,
        report: PassReport::Escape { rewrites },
    }
}

fn diag_for_refinement_lower(ir: &crate::ir::ProofIR) -> PassDiagnostic {
    PassDiagnostic {
        stage: PipelineStage::RefinementLower,
        report: PassReport::RefinementLower {
            refined_types: ir.refined_types.len(),
        },
    }
}

fn diag_for_contract_lower(ir: &crate::ir::ProofIR) -> PassDiagnostic {
    PassDiagnostic {
        stage: PipelineStage::ContractLower,
        report: PassReport::ContractLower {
            fn_contracts: ir.fn_contracts.len(),
        },
    }
}

fn diag_for_law_lower(ir: &crate::ir::ProofIR) -> PassDiagnostic {
    PassDiagnostic {
        stage: PipelineStage::LawLower,
        report: PassReport::LawLower {
            law_theorems: ir.law_theorems.len(),
        },
    }
}

fn diag_for_build_symbols(table: &crate::ir::SymbolTable) -> PassDiagnostic {
    PassDiagnostic {
        stage: PipelineStage::BuildSymbols,
        report: PassReport::BuildSymbols {
            modules: table.modules.len(),
            fns: table.fns.len(),
            types: table.types.len(),
            ctors: table.ctors.len(),
        },
    }
}

fn diag_for_analyze(analysis: &AnalysisResult) -> PassDiagnostic {
    let total_fns = analysis.fn_analyses.len();
    let no_alloc_fns = analysis
        .fn_analyses
        .values()
        .filter(|fa| fa.allocates == Some(false))
        .count();
    let unknown_alloc = analysis
        .fn_analyses
        .values()
        .filter(|fa| fa.allocates.is_none())
        .count();
    PassDiagnostic {
        stage: PipelineStage::Analyze,
        report: PassReport::Analyze {
            total_fns,
            no_alloc_fns,
            recursive_fns: analysis.recursive_fns.len(),
            mutual_tco_members: analysis.mutual_tco_members.len(),
            unknown_alloc,
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source::parse_source;

    fn parse(src: &str) -> Vec<TopLevel> {
        parse_source(src).expect("parse failed")
    }

    #[test]
    fn default_config_fires_every_stage_in_order() {
        let mut items = parse(
            r#"
module M
    intent = "test"
    depends []

fn id(n: Int) -> Int
    n
"#,
        );
        let mut fired: Vec<PipelineStage> = Vec::new();
        run(
            &mut items,
            PipelineConfig {
                typecheck: Some(TypecheckMode::Full { base_dir: None }),
                on_after_pass: Some(Box::new(|stage, _| fired.push(stage))),
                ..Default::default()
            },
        );
        assert_eq!(
            fired,
            vec![
                PipelineStage::Tco,
                PipelineStage::Typecheck,
                PipelineStage::InterpLower,
                PipelineStage::BufferBuild,
                PipelineStage::Resolve,
                PipelineStage::Analyze,
                PipelineStage::Escape,
                PipelineStage::LastUse,
            ]
        );
    }

    #[test]
    fn disabled_stages_dont_fire() {
        let mut items = parse(
            r#"
module M
    intent = "test"
    depends []

fn id(n: Int) -> Int
    n
"#,
        );
        let mut fired: Vec<PipelineStage> = Vec::new();
        run(
            &mut items,
            PipelineConfig {
                typecheck: None,
                run_interp_lower: false,
                run_buffer_build: false,
                run_last_use: false,
                run_analyze: false,
                run_escape: false,
                on_after_pass: Some(Box::new(|stage, _| fired.push(stage))),
                ..Default::default()
            },
        );
        assert_eq!(fired, vec![PipelineStage::Tco, PipelineStage::Resolve]);
    }

    #[test]
    fn typecheck_errors_skip_later_stages() {
        // Reference an undefined identifier so typecheck reports an error.
        let mut items = parse(
            r#"
module M
    intent = "test"
    depends []

fn broken() -> Int
    undefined_thing
"#,
        );
        let mut fired: Vec<PipelineStage> = Vec::new();
        let result = run(
            &mut items,
            PipelineConfig {
                typecheck: Some(TypecheckMode::Full { base_dir: None }),
                on_after_pass: Some(Box::new(|stage, _| fired.push(stage))),
                ..Default::default()
            },
        );
        assert!(
            !result.typecheck.unwrap().errors.is_empty(),
            "typecheck must surface the undefined identifier"
        );
        // Tco fired, typecheck fired, then we bailed out — no later stages.
        assert_eq!(fired, vec![PipelineStage::Tco, PipelineStage::Typecheck]);
    }

    #[test]
    fn analyze_populates_result_when_enabled() {
        let mut items = parse(
            r#"
module M
    intent = "test"
    depends []

fn id(n: Int) -> Int
    n

fn dub(n: Int) -> Int
    n + n
"#,
        );
        let result = run(
            &mut items,
            PipelineConfig {
                typecheck: Some(TypecheckMode::Full { base_dir: None }),
                ..Default::default()
            },
        );
        let analysis = result
            .analysis
            .expect("analyze runs by default and must populate result");
        assert!(
            analysis.fn_analyses.contains_key("id"),
            "every user fn shows up in fn_analyses, got keys: {:?}",
            analysis.fn_analyses.keys().collect::<Vec<_>>()
        );
        assert!(analysis.fn_analyses.contains_key("dub"));
    }

    #[test]
    fn analyze_skipped_when_disabled() {
        let mut items = parse(
            r#"
module M
    intent = "test"
    depends []

fn id(n: Int) -> Int
    n
"#,
        );
        let result = run(
            &mut items,
            PipelineConfig {
                typecheck: Some(TypecheckMode::Full { base_dir: None }),
                run_analyze: false,
                ..Default::default()
            },
        );
        assert!(
            result.analysis.is_none(),
            "run_analyze=false must leave PipelineResult.analysis as None"
        );
    }

    #[test]
    fn alloc_policy_populates_per_fn_allocates() {
        let mut items = parse(
            r#"
module M
    intent = "test"
    depends []

fn pure_one() -> Int
    1

fn allocates_list(n: Int) -> List<Int>
    [n, n, n]
"#,
        );
        let policy = crate::ir::NeutralAllocPolicy;
        let result = run(
            &mut items,
            PipelineConfig {
                typecheck: Some(TypecheckMode::Full { base_dir: None }),
                alloc_policy: Some(&policy),
                ..Default::default()
            },
        );
        let analysis = result.analysis.expect("analyze ran");
        assert_eq!(
            analysis
                .fn_analyses
                .get("pure_one")
                .and_then(|fa| fa.allocates),
            Some(false),
            "pure_one returns a literal — proven not to allocate"
        );
        assert_eq!(
            analysis
                .fn_analyses
                .get("allocates_list")
                .and_then(|fa| fa.allocates),
            Some(true),
            "list literal allocates under the neutral policy"
        );
    }

    #[test]
    fn analyze_without_policy_leaves_allocates_unset() {
        let mut items = parse(
            r#"
module M
    intent = "test"
    depends []

fn id(n: Int) -> Int
    n
"#,
        );
        let result = run(
            &mut items,
            PipelineConfig {
                typecheck: Some(TypecheckMode::Full { base_dir: None }),
                // alloc_policy: None — analyze runs but skips compute_alloc_info
                ..Default::default()
            },
        );
        let analysis = result.analysis.expect("analyze ran");
        let fa = analysis
            .fn_analyses
            .get("id")
            .expect("id is in the analysis");
        assert!(
            fa.allocates.is_none(),
            "without an alloc_policy, allocates stays None (every other field still set)"
        );
    }

    #[test]
    fn last_use_runs_only_after_resolve() {
        // Pipeline ordering invariant: LastUse needs Resolved nodes to
        // annotate. Skipping Resolve while running LastUse is legal but
        // the pass becomes a no-op (no resolved slots in the IR yet).
        // Here we verify it doesn't panic and pipeline returns normally.
        let mut items = parse(
            r#"
module M
    intent = "test"
    depends []

fn id(n: Int) -> Int
    n
"#,
        );
        let mut fired: Vec<PipelineStage> = Vec::new();
        run(
            &mut items,
            PipelineConfig {
                typecheck: Some(TypecheckMode::Full { base_dir: None }),
                run_resolve: false,
                run_analyze: false,
                on_after_pass: Some(Box::new(|stage, _| fired.push(stage))),
                ..Default::default()
            },
        );
        assert_eq!(
            fired,
            vec![
                PipelineStage::Tco,
                PipelineStage::Typecheck,
                PipelineStage::InterpLower,
                PipelineStage::BufferBuild,
                PipelineStage::Escape,
                PipelineStage::LastUse, // fires even without Resolve — a no-op pass
            ]
        );
    }

    #[test]
    fn pass_diagnostics_recorded_for_each_stage_that_ran() {
        let mut items = parse(
            r#"
module M
    intent = "test"
    depends []

fn factorial(n: Int, acc: Int) -> Int
    match n
        0 -> acc
        _ -> factorial(n - 1, acc * n)
"#,
        );
        let result = run(
            &mut items,
            PipelineConfig {
                typecheck: Some(TypecheckMode::Full { base_dir: None }),
                ..Default::default()
            },
        );
        let stages: Vec<PipelineStage> = result.pass_diagnostics.iter().map(|d| d.stage).collect();
        assert_eq!(
            stages,
            vec![
                PipelineStage::Tco,
                PipelineStage::Typecheck,
                PipelineStage::InterpLower,
                PipelineStage::BufferBuild,
                PipelineStage::Resolve,
                PipelineStage::Analyze,
                PipelineStage::Escape,
                PipelineStage::LastUse,
            ]
        );

        let tco_diag = &result.pass_diagnostics[0];
        match &tco_diag.report {
            PassReport::Tco {
                tail_calls_added,
                fns_changed,
                ..
            } => {
                assert!(*tail_calls_added >= 1, "factorial got at least one TCO");
                assert!(
                    fns_changed.iter().any(|c| c.name == "factorial"),
                    "fns_changed must list factorial: {fns_changed:?}"
                );
            }
            other => panic!("expected Tco report, got {other:?}"),
        }

        let bb_diag = result
            .pass_diagnostics
            .iter()
            .find(|d| d.stage == PipelineStage::BufferBuild)
            .unwrap();
        match &bb_diag.report {
            PassReport::BufferBuild(r) => {
                assert_eq!(r.rewrites, 0, "factorial-only program has no fusion sites")
            }
            other => panic!("expected BufferBuild report, got {other:?}"),
        }

        let resolve_diag = result
            .pass_diagnostics
            .iter()
            .find(|d| d.stage == PipelineStage::Resolve)
            .unwrap();
        match &resolve_diag.report {
            PassReport::Resolve { slots_resolved, .. } => assert!(
                *slots_resolved > 0,
                "factorial body resolves at least one ident"
            ),
            other => panic!("expected Resolve report, got {other:?}"),
        }
    }
}

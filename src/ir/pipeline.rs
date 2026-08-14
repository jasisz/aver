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
//!
//! The pipeline carries one more invariant, cutting ACROSS the order:
//! the **proof line**. A pass that puts entities in the AST which the
//! source does not contain (`interp_lower`'s `__buf_*` chain,
//! `buffer_build`'s `<sink>__buffered` sink, the planned chars-fusion)
//! sits below it and must never be visible to Lean / Dafny; every other
//! pass sits above it and the proof exporters read its output. `run`
//! serves both halves from one run: it snapshots the AST before the
//! first fabricating stage and completes the copy afterwards by
//! re-running the above-the-line stages on it — see [`AstView`]. So a
//! new fabricating pass is proof-invisible by construction as long as
//! it is not re-run on the snapshot, and moving a pass across the line
//! is a soundness-grade edit.

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
    /// Per-module interval analysis over refinement-type carriers —
    /// read-only diagnostic. Derives a constant interval for each
    /// refined type's invariant and classifies its arithmetic ops as
    /// overflow-free / needs-wider-scratch / unbounded. Opt-in via
    /// `run_interval_analyze`; runs right after `RefinementLower`
    /// (consumes `ProofIR.refined_types`). No codegen / runtime /
    /// proof-output consumer yet — only `--explain-passes` reads it.
    IntervalAnalyze,
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
    /// Lift the post-typecheck `Expr` AST into
    /// [`crate::ir::hir::ResolvedTopLevel`] — call sites become
    /// `FnId` / `CtorId`-keyed, constructor / record references
    /// carry typed identity, tail calls carry their target `FnId`.
    /// Phase E of #147; populates
    /// `PipelineResult::resolved_items`. Unconditional like
    /// `BuildSymbols` — the resolver is part of the canonical
    /// pipeline output, not an opt-in concern. Backends consume
    /// `resolved_items` in lieu of re-resolving `Expr` themselves.
    NameResolve,
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
            Self::IntervalAnalyze => "interval_analyze",
            Self::ContractLower => "contract_lower",
            Self::LawLower => "law_lower",
            Self::BuildSymbols => "build_symbols",
            Self::NameResolve => "name_resolve",
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
    /// Whether to run the buffer-build deforestation pass. Runtime
    /// backends that own a growable buffer (VM, Rust) turn it on; the
    /// wasm-gc family cannot lower the `Buffer` it introduces and leaves
    /// it off.
    ///
    /// It used to double as the seam that keeps deforestation invisible
    /// to the proof exporters, and that seam was a CONVENTION: every
    /// proof caller had to remember to pass `false` here. It is now
    /// STRUCTURAL — this pass is below the proof line, `run` snapshots
    /// the AST before it and never re-runs it on the copy (see
    /// [`AstView`]), so what you pass here cannot change what gets
    /// proven. A new pass that synthesizes entities the source does not
    /// contain belongs here, below the line, and is proof-invisible for
    /// free.
    pub run_buffer_build: bool,
    pub run_resolve: bool,
    /// Whether to run the last-use ownership annotation pass after
    /// `resolve`. Annotates each `Expr::Resolved` slot reference with
    /// `last_use: bool`; backends use it to MOVE instead of COPY
    /// (VM `MOVE_LOCAL`, Rust skips `.clone()`, owned-mutate fast paths).
    /// Independent of `run_resolve`: enabling LastUse without Resolve is
    /// a no-op (no resolved slots to annotate); skipping LastUse keeps
    /// every reference pessimistically marked as "not last".
    ///
    /// Above the proof line (see [`AstView`]) — it annotates, it does
    /// not synthesize. The proof exporters ignore the annotations, so
    /// running it on the proof view changes no emitted proof; what it
    /// buys is that the view is the artifact's AST exactly, with no
    /// "modulo annotations" left in the claim.
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
    ///
    /// ABOVE the proof line (see [`AstView`]): the pass introduces no
    /// entity the source does not contain, and a certificate's model
    /// must describe the same program its bytes were compiled from —
    /// so the proof view runs it too, on whatever this flag says. Off,
    /// and both halves of the run see the un-inlined program; on, and
    /// both see the scalar-replaced one.
    pub run_escape: bool,
    /// Whether to run the refinement-lift pass. Walks user type defs
    /// and smart constructors, populating `ProofIR.refined_types`.
    /// Independent of `run_contract_lower` — a backend could opt into
    /// one without the other, though production proof exporters
    /// always enable both. Both stages share the same
    /// `PipelineResult.proof_ir` output sink.
    pub run_refinement_lower: bool,
    /// Whether to run the per-module interval analysis (read-only
    /// diagnostic) after `RefinementLower`. Derives a constant
    /// interval for each refined type's invariant and classifies its
    /// arithmetic ops. Requires `run_refinement_lower` to have
    /// populated `ProofIR.refined_types`; a no-op (empty result) when
    /// there are no refined types. Default off — only `--explain-passes`
    /// turns it on.
    pub run_interval_analyze: bool,
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
            run_interval_analyze: false,
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
    IntervalAnalyze {
        /// Refined types the interval analysis saw.
        types_analyzed: usize,
        /// Of those, how many yielded a two-sided constant interval
        /// (both bounds finite) — the carrier-lowering candidates.
        two_sided_bounded: usize,
        /// Arithmetic ops whose intermediate provably fits `i64`
        /// (guard still required — see `OpClass::OverflowFree`).
        ops_overflow_free: usize,
        /// Ops whose intermediate exceeds `i64` but is finite.
        ops_needs_wider: usize,
        /// Ops with no derivable finite bound (one-sided / plain-`Int`
        /// operands).
        ops_unbounded: usize,
        /// Refined types eligible for a raw `i64` carrier — two-sided,
        /// `i64`-fitting interval with every op `OverflowFree`. The
        /// recognizer slice's headline count; a pure diagnostic with no
        /// downstream consumer yet.
        raw_i64_eligible: usize,
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
        /// Total fn declarations across entry + dep modules.
        fns: usize,
        /// Total type declarations across entry + dep modules.
        types: usize,
        /// Total constructors (record + sum variants) across all
        /// type declarations.
        ctors: usize,
        /// Per-module breakdown (entry first, then deps in walk
        /// order). Shows where each fn/type lives — useful for
        /// "is this big project actually multi-module?" sanity
        /// checks at a glance.
        modules: Vec<BuildSymbolsModule>,
        /// Number of bare fn names that occur in 2+ different
        /// scopes (e.g. entry + Module, or Module.A + Module.B).
        /// Under bare-name keying these would have silently
        /// merged; opaque `FnId` keeps them distinct. Zero would
        /// mean phase E migration had no practical effect on
        /// *this* project — non-zero is the proof it did.
        fn_name_collisions: usize,
        /// Same for type names.
        type_name_collisions: usize,
    },
    NameResolve {
        /// Top-level fn definitions promoted to
        /// [`crate::ir::hir::ResolvedFnDef`]. One per `FnDef` that
        /// resolved through the symbol table.
        promoted_fns: usize,
        /// Top-level items kept as
        /// [`crate::ir::hir::ResolvedTopLevel::Passthrough`] (verify
        /// blocks, decisions, type defs, top-level stmts, and any
        /// fn whose name didn't resolve — typechecker error
        /// recovery). The fewer of these, the more the resolved
        /// HIR captured.
        passthrough_items: usize,
        /// Resolver-escape-hatch occurrences in the resolved tree —
        /// `ResolvedCallee::Unresolved` and `ResolvedCtor::Unresolved`
        /// summed across every expression. The contract for backends
        /// that consume `resolved_items` is **zero unresolved for
        /// well-typed programs**; non-zero implies either a
        /// typechecker error already reported (the resolver bailed
        /// to recovery shape) or a resolver gap that needs
        /// patching. CI gates can compare this count against the
        /// typecheck error count to catch silent resolver
        /// regressions.
        unresolved_count: usize,
    },
}

/// Per-module entry in `PassReport::BuildSymbols`. `prefix` is the
/// dotted module name (`"Models.User"`); the entry scope is
/// represented by an empty string so JSON consumers can sort/group
/// uniformly.
#[derive(Debug, Clone)]
pub struct BuildSymbolsModule {
    pub prefix: String,
    pub fns: usize,
    pub types: usize,
    pub ctors: usize,
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
    /// Per-module interval analysis facts (read-only diagnostic) when
    /// `run_interval_analyze` was on alongside `run_refinement_lower`.
    /// `None` when the stage was disabled. The queryable fact for
    /// future opt-in consumers (the carrier-lowering recognizer);
    /// nothing in codegen / runtime / proof emission reads it yet.
    pub interval_analysis: Option<crate::ir::IntervalAnalysisResult>,
    /// Resolved-identity table — opaque `FnId` / `TypeId` /
    /// `CtorId` / `ModuleId` for every named declaration in the
    /// program (#138 phase E). Always populated: the pipeline
    /// builds it unconditionally at the head of `run` (it's the
    /// universal foundation for proof IR + backend identity).
    /// `run_build_symbols` in `PipelineConfig` controls whether
    /// the BuildSymbols stage fires its `on_after_pass` hook for
    /// `--emit-ir-after=build_symbols`; the table itself is built
    /// regardless.
    pub symbol_table: crate::ir::SymbolTable,
    /// Resolved HIR — post-typecheck AST lifted onto opaque
    /// identities (Phase E of #147). Always populated: the
    /// pipeline runs `NameResolve` unconditionally after
    /// `BuildSymbols`, mirroring how the symbol table is always
    /// built. Backends consume this in lieu of re-resolving
    /// `Expr` themselves. The migration of individual backends
    /// (VM / Rust / wasm-gc / Lean / Dafny / self-host) is staged
    /// across follow-up PRs in the Phase E stack.
    pub resolved_items: Vec<crate::ir::hir::ResolvedTopLevel>,
    /// Per-stage diagnostic records — one per pass that actually ran.
    /// Drives `aver compile --explain-passes`; consumed by the future
    /// CI failable-invariant checks.
    pub pass_diagnostics: Vec<PassDiagnostic>,
    /// The one proof-facing view — what `aver proof` exports and what
    /// the artifact-certificate model is built from. `Some` iff a proof
    /// stage ran; see [`AstView`] for why it is a separate view and
    /// not just `items`.
    pub proof_view: Option<AstView>,
}

impl PipelineResult {
    /// The view a [`crate::codegen::CodegenContext`] must be assembled
    /// from: the proof view when the proof stages ran — so Lean and
    /// Dafny describe the program the certified artifact was compiled
    /// from — and the runtime-facing pipeline output otherwise.
    ///
    /// This is the only place the choice is made, which is what keeps
    /// it from being a convention again: a caller cannot forget to
    /// prefer the proof view, because asking for context inputs IS
    /// asking this question. `items` is the caller's post-pipeline
    /// vec (the pipeline mutates it in place and does not own it) and
    /// is used only when no proof stage ran.
    ///
    /// Consumes the result — one context per run, and asking twice is
    /// a compile error rather than a second view that silently differs
    /// from the first. Read anything else you need off the result
    /// (`typecheck`, `proof_ir`, `pass_diagnostics`, …) before calling.
    pub fn codegen_view(self, items: Vec<TopLevel>) -> AstView {
        match self.proof_view {
            Some(view) => view,
            None => AstView {
                items,
                analysis: self.analysis,
                symbol_table: self.symbol_table,
                resolved_items: self.resolved_items,
            },
        }
    }
}

/// An AST plus the facts derived from THAT AST — the bundle a
/// `CodegenContext` is assembled from. Held on
/// [`PipelineResult::proof_view`] as the one view every proof-facing
/// consumer reads.
///
/// Two rules meet in this type, and they pull in opposite directions.
///
/// A proof must not describe a program that does not exist in source.
/// `buffer_build` replaces a `String.join(<builder>(…), sep)` pipeline
/// with a buffer loop and synthesizes a `<sink>__buffered` fn;
/// `interp_lower` replaces an interpolation with a `__buf_*` chain; the
/// planned chars-fusion pass will do the same to traversals. Those
/// FABRICATING passes are below the proof line. Keeping them off a
/// proof used to be a convention — every proof-facing caller passing
/// `run_buffer_build: false` — and one forgotten `true` silently
/// changed what got proven.
///
/// A certificate must not describe a different program from the one it
/// certifies: the artifact-certificate model is a trusted oracle over
/// the emitted bytes (the recursion classifier reads the step operator
/// off the MODEL, because the bytes cannot tell the bignum helpers
/// apart), so a model built from a differently-optimised AST than the
/// bytes attributes one program's facts to another's body. `escape` —
/// the narrow scalar-replace of a fresh record built at a call site and
/// immediately consumed; Aver has no general inliner — rewrites
/// callers, so it is ABOVE the line: small, semantics-preserving, ours,
/// guarded by the differential, and seen by both halves.
///
/// The line therefore cuts by WHAT A PASS DOES, and that is not a point
/// in the stage order: `buffer_build` runs before `escape`. So [`run`]
/// snapshots `items` before the first fabricating stage and completes
/// the copy afterwards, re-running on it — in the pipeline's own order,
/// and mirroring the caller's own flags — every above-the-line stage:
/// slot resolution, `analyze`, `escape`, the `last_use` / alias
/// ownership annotations, then the symbol table and the resolved HIR.
/// The invariant that falls out:
///
/// > the proof view is the program this same run would have compiled
/// > with the fabricating passes turned off.
///
/// So one pipeline run can hand a deforested AST to a runtime backend
/// and the unfabricated one to Lean / Dafny, and a certificate's model
/// and its bytes describe the same program.
///
/// The fields mirror the `PipelineResult` ones a `CodegenContext` is
/// built from — same data, derived from this AST instead of from the
/// fabricated one.
pub struct AstView {
    /// Entry-module items — the unfabricated program when this is the
    /// proof view.
    pub items: Vec<TopLevel>,
    /// `analyze` over [`Self::items`]. `None` when the caller left
    /// `run_analyze` off (the main view is `None` then too).
    pub analysis: Option<AnalysisResult>,
    /// Symbol table built over [`Self::items`].
    pub symbol_table: crate::ir::SymbolTable,
    /// Resolved HIR lifted from [`Self::items`].
    pub resolved_items: Vec<crate::ir::hir::ResolvedTopLevel>,
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

    // Proof stages run at the tail of the pipeline but read an AST from
    // its head — see [`AstView`]. Knowing up front whether any of them
    // is on is what makes the snapshot free for runtime-only pipelines.
    let proof_stages = cfg.run_refinement_lower
        || cfg.run_interval_analyze
        || cfg.run_contract_lower
        || cfg.run_law_lower;

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

    // ── The proof snapshot point ────────────────────────────────────
    //
    // The next two stages are the FABRICATING ones: `interp_lower`
    // replaces an interpolation with a `__buf_*` chain, `buffer_build`
    // replaces a join pipeline with a `<sink>__buffered` loop. Both put
    // entities in the AST that the source does not contain, and a proof
    // about those is a proof about a program the user never wrote — so
    // the AST the proof exporters read is copied here, before either
    // runs. The copy is completed below by re-running the
    // above-the-line stages on it; see [`AstView`] for why the line
    // cannot simply be a point in this stage order.
    let proof_snapshot: Option<Vec<TopLevel>> = proof_stages.then(|| items.clone());

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

    // Resolved-identity table is the universal foundation for proof
    // IR + every backend's name lookup, so it's built unconditionally.
    // `run_build_symbols` in PipelineConfig controls only whether the
    // BuildSymbols stage fires its `on_after_pass` hook (for
    // `--emit-ir-after=build_symbols`); the table itself lands in
    // `result.symbol_table` regardless.
    {
        let symbol_table = crate::ir::SymbolTable::build(items, cfg.dep_modules);
        result
            .pass_diagnostics
            .push(diag_for_build_symbols(&symbol_table));
        result.symbol_table = symbol_table;
        if cfg.run_build_symbols {
            fire(&mut cfg, PipelineStage::BuildSymbols, items);
        }
    }

    // Name-resolve runs after BuildSymbols (needs the symbol
    // table) and after the slot resolver + last_use (so
    // `Expr::Resolved` slots / `last_use` flags are populated and
    // carry through to the resolved AST). Unconditional: like
    // `BuildSymbols`, the resolved HIR is part of the canonical
    // pipeline output — every backend can rely on
    // `PipelineResult::resolved_items` being populated without
    // opt-in setup. Cost is O(n) over the AST (mirror of what
    // typecheck already walks) — small enough to make the
    // architectural simplicity worth it.
    {
        let resolved = crate::ir::hir::resolve_program(&result.symbol_table, items);
        result
            .pass_diagnostics
            .push(diag_for_name_resolve(&resolved));
        result.resolved_items = resolved;
        fire(&mut cfg, PipelineStage::NameResolve, items);
    }

    if proof_stages {
        // Complete the proof view on the snapshot: re-run, on the copy,
        // every above-the-line stage this caller enabled, in this
        // pipeline's own order — and none of the fabricating ones. The
        // result is the program this run would have compiled with
        // `run_interp_lower` / `run_buffer_build` off, which is exactly
        // the AST the certified artifact is compiled from. See
        // [`AstView`].
        let mut proof_items =
            proof_snapshot.expect("snapshot is taken whenever a proof stage is enabled");
        if cfg.run_resolve {
            resolve(&mut proof_items);
        }
        let proof_analysis = cfg.run_analyze.then(|| {
            let adapter = CallCtxAdapter(cfg.call_ctx);
            crate::ir::analyze(&proof_items, cfg.alloc_policy, &adapter)
        });
        if cfg.run_escape {
            crate::ir::escape::run(&mut proof_items);
        }
        if cfg.run_last_use {
            last_use(&mut proof_items);
            crate::ir::alias::annotate_program_alias_slots(&mut proof_items);
        }
        let proof_symbol_table = crate::ir::SymbolTable::build(&proof_items, cfg.dep_modules);
        let proof_resolved_items =
            crate::ir::hir::resolve_program(&proof_symbol_table, &proof_items);

        // Round-5: union entry's analyze with each dep module's
        // `analysis.recursive_fns`. Without this, multi-module proof
        // export's `populate_fn_contracts` only sees entry-recursive
        // fns and classifies module fns as "outside subset" — even
        // when each module's analyze already saw their countdown /
        // structural-recursion shape. Aver's module DAG invariant
        // rules out cross-module recursion SCCs, so unioning the
        // per-scope `recursive_fns` sets matches the build-time
        // `ctx.recursive_fns` view in `codegen::build_context`.
        // Project per-scope bare-name sets to opaque FnIds through the
        // (already-built) symbol table — same flow `build_context`
        // uses to populate `ctx.recursive_fns`. Without a symbol
        // table the producer side of proof_lower can't run anyway
        // (populate panics by design), so default to empty.
        let symbols = &proof_symbol_table;
        let recursive_fns_owned: std::collections::HashSet<crate::ir::FnId> = {
            let mut set = std::collections::HashSet::new();
            if let Some(a) = proof_analysis.as_ref() {
                set.extend(crate::codegen::scc::analysis_set_to_fn_ids(
                    &a.recursive_fns,
                    symbols,
                    None,
                ));
            }
            for m in cfg.dep_modules {
                if let Some(a) = m.analysis.as_ref() {
                    set.extend(crate::codegen::scc::analysis_set_to_fn_ids(
                        &a.recursive_fns,
                        symbols,
                        Some(&m.prefix),
                    ));
                }
            }
            set
        };
        let module_prefixes: std::collections::HashSet<String> =
            cfg.dep_modules.iter().map(|m| m.prefix.clone()).collect();
        // Stage 6b of #232: build ProgramShape once here so
        // refinement_info_for and other proof-lower detectors read
        // from typed patterns instead of rewalking the AST. The
        // module-level patterns (RefinementSmartConstructor) walk
        // dep modules' source-level FnDefs directly; the per-fn
        // archetype facts here only see entry-module resolved fns,
        // which is enough for the current refinement adapter (dep
        // modules don't expose ResolvedFnDef to the pipeline yet).
        let entry_resolved_fns: Vec<&crate::ir::hir::ResolvedFnDef> = proof_resolved_items
            .iter()
            .filter_map(|t| match t {
                crate::ir::hir::ResolvedTopLevel::FnDef(fd) => Some(fd),
                _ => None,
            })
            .collect();
        let program_shape = crate::analysis::shape::analyze_program_with_modules(
            &entry_resolved_fns,
            &proof_items,
            cfg.dep_modules,
        );
        let inputs = crate::codegen::proof_lower::ProofLowerInputs {
            entry_items: &proof_items,
            dep_modules: cfg.dep_modules,
            module_prefixes: &module_prefixes,
            recursive_fns: &recursive_fns_owned,
            symbol_table: symbols,
            program_shape: Some(&program_shape),
        };
        let mut ir = result.proof_ir.take().unwrap_or_default();
        if cfg.run_refinement_lower {
            crate::codegen::proof_lower::populate_refined_types(&inputs, &mut ir);
            result.pass_diagnostics.push(diag_for_refinement_lower(&ir));
            fire(&mut cfg, PipelineStage::RefinementLower, &proof_items);
        }
        if cfg.run_interval_analyze {
            // Read-only: consumes the just-built `ir.refined_types`
            // plus the same `ProofLowerInputs` to look up each type's
            // module-scoped ops. Stores the result on the pipeline
            // output and pushes a diagnostic; no IR mutation.
            let analysis = crate::ir::interval::analyze(&ir.refined_types, &inputs);
            result
                .pass_diagnostics
                .push(diag_for_interval_analyze(&analysis));
            result.interval_analysis = Some(analysis);
            fire(&mut cfg, PipelineStage::IntervalAnalyze, &proof_items);
        }
        if cfg.run_contract_lower {
            crate::codegen::proof_lower::populate_fn_contracts(&inputs, &mut ir);
            result.pass_diagnostics.push(diag_for_contract_lower(&ir));
            fire(&mut cfg, PipelineStage::ContractLower, &proof_items);
        }
        if cfg.run_law_lower {
            crate::codegen::proof_lower::populate_law_theorems(&inputs, &mut ir);
            result.pass_diagnostics.push(diag_for_law_lower(&ir));
            fire(&mut cfg, PipelineStage::LawLower, &proof_items);
        }
        result.proof_ir = Some(ir);
        result.proof_view = Some(AstView {
            items: proof_items,
            analysis: proof_analysis,
            symbol_table: proof_symbol_table,
            resolved_items: proof_resolved_items,
        });
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

fn diag_for_interval_analyze(analysis: &crate::ir::IntervalAnalysisResult) -> PassDiagnostic {
    PassDiagnostic {
        stage: PipelineStage::IntervalAnalyze,
        report: PassReport::IntervalAnalyze {
            types_analyzed: analysis.types_analyzed(),
            two_sided_bounded: analysis.two_sided_bounded(),
            ops_overflow_free: analysis.ops_overflow_free(),
            ops_needs_wider: analysis.ops_needs_wider(),
            ops_unbounded: analysis.ops_unbounded(),
            raw_i64_eligible: analysis.raw_i64_eligible(),
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
    // Per-module breakdown: walk every fn/type/ctor, bucket by its
    // owning module, count. Single pass, O(fns + types + ctors).
    let mut per_module: Vec<BuildSymbolsModule> = table
        .modules
        .iter()
        .map(|m| BuildSymbolsModule {
            prefix: m.prefix.clone().unwrap_or_default(),
            fns: 0,
            types: 0,
            ctors: 0,
        })
        .collect();
    for fe in &table.fns {
        per_module[fe.module.0 as usize].fns += 1;
    }
    for te in &table.types {
        per_module[te.module.0 as usize].types += 1;
    }
    for ce in &table.ctors {
        let owning_module = table.types[ce.owning_type.0 as usize].module;
        per_module[owning_module.0 as usize].ctors += 1;
    }

    // Bare-name collision count: a fn/type with the same `key.name`
    // appearing in 2+ different scopes. Under bare-name keying these
    // would silently merge; under opaque IDs each gets its own slot.
    use std::collections::HashMap;
    let mut fn_buckets: HashMap<&str, usize> = HashMap::new();
    for fe in &table.fns {
        *fn_buckets.entry(fe.key.name.as_str()).or_default() += 1;
    }
    let fn_name_collisions = fn_buckets.values().filter(|c| **c >= 2).count();
    let mut type_buckets: HashMap<&str, usize> = HashMap::new();
    for te in &table.types {
        *type_buckets.entry(te.key.name.as_str()).or_default() += 1;
    }
    let type_name_collisions = type_buckets.values().filter(|c| **c >= 2).count();

    PassDiagnostic {
        stage: PipelineStage::BuildSymbols,
        report: PassReport::BuildSymbols {
            fns: table.fns.len(),
            types: table.types.len(),
            ctors: table.ctors.len(),
            modules: per_module,
            fn_name_collisions,
            type_name_collisions,
        },
    }
}

fn diag_for_name_resolve(items: &[crate::ir::hir::ResolvedTopLevel]) -> PassDiagnostic {
    use crate::ir::hir::ResolvedTopLevel;
    let mut promoted_fns = 0;
    let mut passthrough_items = 0;
    let mut unresolved_count = 0;
    for item in items {
        match item {
            ResolvedTopLevel::FnDef(fd) => {
                promoted_fns += 1;
                unresolved_count += crate::ir::hir::count_unresolved_in_fn(fd);
            }
            ResolvedTopLevel::Passthrough(_) => passthrough_items += 1,
            ResolvedTopLevel::Module(_) => {}
        }
    }
    PassDiagnostic {
        stage: PipelineStage::NameResolve,
        report: PassReport::NameResolve {
            promoted_fns,
            passthrough_items,
            unresolved_count,
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
                // Phase E (#147): NameResolve runs unconditionally
                // after BuildSymbols (which fires only via flag, but
                // the resolved AST is always built).
                PipelineStage::NameResolve,
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
        // NameResolve runs unconditionally after BuildSymbols, even
        // when most other stages are disabled.
        assert_eq!(
            fired,
            vec![
                PipelineStage::Tco,
                PipelineStage::Resolve,
                PipelineStage::NameResolve,
            ]
        );
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
                // NameResolve runs unconditionally after
                // BuildSymbols (Phase E of #147).
                PipelineStage::NameResolve,
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
                // Symbol table is built unconditionally (universal
                // foundation for proof IR + backend identity); the
                // diagnostic always lands in `pass_diagnostics`.
                PipelineStage::BuildSymbols,
                // NameResolve same — unconditional Phase E pass.
                PipelineStage::NameResolve,
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

    #[test]
    fn name_resolve_runs_unconditionally() {
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
                ..Default::default()
            },
        );
        // Phase E (#147): NameResolve runs unconditionally, like
        // `BuildSymbols`. `resolved_items` is always populated —
        // the pipeline is the resolver's caller, and backends
        // consume the result rather than re-resolving themselves.
        assert!(
            !result.resolved_items.is_empty(),
            "NameResolve runs unconditionally; resolved_items must be populated"
        );
        let saw_name_resolve = result
            .pass_diagnostics
            .iter()
            .any(|d| d.stage == PipelineStage::NameResolve);
        assert!(
            saw_name_resolve,
            "NameResolve stage must always fire its diagnostic"
        );
    }

    #[test]
    fn name_resolve_populates_resolved_items() {
        let mut items = parse(
            r#"
module M
    intent = "test"
    depends []

fn helper(n: Int) -> Int
    n + 1

fn main() -> Int
    helper(7)
"#,
        );
        let result = run(
            &mut items,
            PipelineConfig {
                typecheck: Some(TypecheckMode::Full { base_dir: None }),
                ..Default::default()
            },
        );
        let resolved = result.resolved_items;
        // Two FnDefs (helper + main) promoted; the Module item
        // lands as ResolvedTopLevel::Module — neither promoted nor
        // passthrough. So exactly 2 promoted, 0 passthrough.
        use crate::ir::hir::ResolvedTopLevel;
        let promoted = resolved
            .iter()
            .filter(|t| matches!(t, ResolvedTopLevel::FnDef(_)))
            .count();
        let passthrough = resolved
            .iter()
            .filter(|t| matches!(t, ResolvedTopLevel::Passthrough(_)))
            .count();
        assert_eq!(promoted, 2, "helper + main should both promote");
        assert_eq!(passthrough, 0, "no non-fn items in this fixture");

        // Diagnostic mirrors the counts.
        let diag = result
            .pass_diagnostics
            .iter()
            .find(|d| d.stage == PipelineStage::NameResolve)
            .expect("NameResolve diagnostic missing");
        match &diag.report {
            PassReport::NameResolve {
                promoted_fns,
                passthrough_items,
                unresolved_count,
            } => {
                assert_eq!(*promoted_fns, 2);
                assert_eq!(*passthrough_items, 0);
                // Well-typed input — the resolver MUST classify
                // every reference. Non-zero would mean the resolver
                // bailed to a `ResolvedCallee::Unresolved` /
                // `ResolvedCtor::Unresolved` escape hatch.
                assert_eq!(
                    *unresolved_count, 0,
                    "well-typed program must produce zero unresolved nodes"
                );
            }
            other => panic!("expected NameResolve report, got {other:?}"),
        }
    }

    /// Phase E contract gate: for well-typed programs, the resolver
    /// MUST classify every reference. `ResolvedCallee::Unresolved` /
    /// `ResolvedCtor::Unresolved` are recovery escape hatches for
    /// typecheck-error programs; their occurrence in well-typed
    /// input is a resolver gap, not a legitimate state.
    #[test]
    fn name_resolve_invariant_zero_unresolved_for_well_typed_programs() {
        // Battery of well-typed fixtures exercising every shape the
        // resolver classifies: user fn calls (incl. recursion + TCO),
        // builtin namespace methods, user ctors, builtin ctors,
        // record create + update, match patterns, binding
        // annotations.
        let fixtures: &[&str] = &[
            // Recursive fn, builtin call, binding annotation.
            r#"
fn count(n: Int, acc: Int) -> Int
    match n
        0 -> acc
        _ -> count(n - 1, acc + Int.abs(-1))

fn main() -> Int
    x: Int = count(5, 0)
    x
"#,
            // User sum type + variants + match arms.
            r#"
type Shape
    Circle(Float)
    Square(Float)

fn area(s: Shape) -> Float
    match s
        Shape.Circle(r) -> r * r
        Shape.Square(s) -> s * s

fn main() -> Float
    area(Shape.Circle(3.0))
"#,
            // Record create + update + builtin ctor.
            r#"
record Point
    x: Int
    y: Int

fn origin() -> Point
    Point(x = 0, y = 0)

fn shift(p: Point) -> Result<Point, String>
    Result.Ok(Point.update(p, x = p.x + 1))
"#,
            // Interpolation — exercises `interp_lower` synthesizing
            // `__buf_*` / `__to_str` intrinsics. With the post-Phase-E
            // resolver these land as `ResolvedCallee::Intrinsic(_)`,
            // not as `Unresolved`, so the invariant must hold even
            // after lowering passes have run.
            r#"
fn greet(n: Int) -> String
    "hi ${n}!"

fn main() -> String
    greet(7)
"#,
        ];
        for (i, src) in fixtures.iter().enumerate() {
            let mut items = parse(src);
            let result = run(
                &mut items,
                PipelineConfig {
                    typecheck: Some(TypecheckMode::Full { base_dir: None }),
                    ..Default::default()
                },
            );
            // Sanity: input must actually typecheck — otherwise the
            // invariant doesn't apply (recovery escapes are allowed
            // for broken programs).
            if let Some(tc) = result.typecheck.as_ref() {
                assert!(
                    tc.errors.is_empty(),
                    "fixture #{i} did not typecheck: {:?}",
                    tc.errors
                );
            }
            let diag = result
                .pass_diagnostics
                .iter()
                .find(|d| d.stage == PipelineStage::NameResolve)
                .expect("NameResolve diagnostic missing");
            match &diag.report {
                PassReport::NameResolve {
                    unresolved_count, ..
                } => {
                    assert_eq!(
                        *unresolved_count, 0,
                        "fixture #{i}: well-typed program produced {} unresolved nodes",
                        *unresolved_count
                    );
                }
                other => panic!("expected NameResolve report, got {other:?}"),
            }
        }
    }

    #[test]
    fn name_resolve_runs_after_build_symbols() {
        // The resolver needs the symbol table — it must always come
        // after BuildSymbols in the firing order.
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
                run_build_symbols: true,
                on_after_pass: Some(Box::new(|stage, _| fired.push(stage))),
                ..Default::default()
            },
        );
        let build_pos = fired
            .iter()
            .position(|s| *s == PipelineStage::BuildSymbols)
            .expect("BuildSymbols fired");
        let resolve_pos = fired
            .iter()
            .position(|s| *s == PipelineStage::NameResolve)
            .expect("NameResolve fired");
        assert!(
            resolve_pos > build_pos,
            "NameResolve must fire after BuildSymbols (saw {fired:?})"
        );
    }
}

/// Aver → target language transpilation.
///
/// The codegen module transforms a type-checked Aver AST into source code
/// for a target language. Current backends: Rust deployment and Lean proof export.
pub(crate) mod builtin_helpers;
pub(crate) mod builtin_records;
pub(crate) mod builtins;
#[cfg(feature = "wasm-compile")]
pub mod cert;
#[cfg(feature = "runtime")]
pub(crate) mod cite_instantiate;
pub mod common;
#[cfg(feature = "runtime")]
pub mod dafny;
pub mod expr_walk;
#[cfg(feature = "runtime")]
pub mod lean;
#[cfg(feature = "runtime")]
pub mod lemma_discovery;
pub mod program_view;
pub mod proof_lower;
#[cfg(feature = "runtime")]
pub(crate) mod proof_recognize;
#[cfg(feature = "runtime")]
pub mod recursion;
#[cfg(feature = "runtime")]
pub mod rust;
pub mod scc;
pub mod wasip2;
#[cfg(feature = "wasm-compile")]
pub mod wasm_gc;

use std::collections::{HashMap, HashSet};

use crate::ast::{CapabilityItem, FnDef, TopLevel, TypeDef};
use crate::source::LoadedModule;
use crate::types::checker::TypeCheckResult;

/// Information about a dependent module loaded for codegen.
pub struct ModuleInfo {
    /// Qualified module path, e.g. "Models.User".
    pub prefix: String,
    /// Direct `depends [...]` entries from the source module.
    pub depends: Vec<String>,
    /// The module header's `exposes [...]` list, verbatim (empty = the default
    /// rule). Together with `exposes_opaque` and `depends` this is what
    /// [`crate::visibility::collect_type_exports`] needs to say which types the
    /// module hands on to its importers, including ones it only re-exposes.
    pub exposes: Vec<String>,
    /// The module header's `exposes opaque [...]` list, verbatim.
    pub exposes_opaque: Vec<String>,
    /// Type definitions from the module.
    pub type_defs: Vec<TypeDef>,
    /// Function definitions from the module. A dependency's `main` is an
    /// ordinary module-owned function; only the entry module's `main` has
    /// entry-point semantics.
    pub fn_defs: Vec<FnDef>,
    /// Provider-bound declarations retained for name resolution and VM
    /// fail-closed dispatch. They have signatures but no Aver bodies.
    pub capability_items: Vec<CapabilityItem>,
    /// Raw module-header semantics (`pure` / `effectful`) for the retained
    /// capability items. Validation belongs to `capability::CapabilityRegistry`.
    pub capability_semantics: Option<String>,
    /// `verify … law` blocks of this dep module, in source order.
    ///
    /// Carried so the Lean proof backend can (a) emit each proven dep
    /// law as a `<fn>_law_<name>` theorem inside `namespace M`, and
    /// (b) admit it into a consumer law's lemma pool under the same
    /// cone ∪ subject admissibility gate as in-file sibling laws — the
    /// cross-file law pool. Only `VerifyKind::Law` blocks are kept;
    /// plain example-style `verify` blocks in a dep are still dropped
    /// (module-scoped sampling is a separate feature). Read ONLY by the
    /// Lean proof emit / pool; inert for every other backend.
    pub verify_laws: Vec<crate::ast::VerifyBlock>,
    /// IR-level analysis facts produced by the dep module's pipeline run
    /// (`analyze` stage). `None` for modules loaded via paths that skip
    /// the analyze stage (none in production today; left optional for
    /// future ad-hoc loaders). Aver's module DAG invariant makes per-module
    /// analysis sufficient — see `project_aver_module_dag` memory and
    /// `src/ir/analyze.rs` for why cross-module SCCs are impossible.
    pub analysis: Option<crate::ir::AnalysisResult>,
}

impl ModuleInfo {
    /// Build the shared projection from parsed module items. Target-specific
    /// loaders own parsing, typechecking, and lowering; this constructor owns
    /// the stable `ModuleInfo` field selection.
    pub fn from_items(
        prefix: String,
        items: &[TopLevel],
        analysis: Option<crate::ir::AnalysisResult>,
    ) -> Self {
        let decl = crate::visibility::module_decl(items);
        let depends = decl.map(|m| m.depends.clone()).unwrap_or_default();
        let exposes = decl.map(|m| m.exposes.clone()).unwrap_or_default();
        let exposes_opaque = decl.map(|m| m.exposes_opaque.clone()).unwrap_or_default();
        let type_defs = items
            .iter()
            .filter_map(|i| match i {
                TopLevel::TypeDef(td) => Some(td.clone()),
                _ => None,
            })
            .collect();
        let fn_defs = items
            .iter()
            .filter_map(|i| match i {
                TopLevel::FnDef(fd) => Some(fd.clone()),
                _ => None,
            })
            .collect();
        let (capability_items, capability_semantics) = capability_metadata(items);
        Self {
            prefix,
            depends,
            exposes,
            exposes_opaque,
            type_defs,
            fn_defs,
            capability_items,
            capability_semantics,
            verify_laws: collect_verify_laws(items),
            analysis,
        }
    }

    /// Build a [`ModuleInfo`] from a freshly-parsed [`LoadedModule`].
    /// Skips the analyze stage — callers that need per-dep analysis
    /// facts should run the pipeline themselves (see
    /// `crate::main::commands::load_compile_deps` /
    /// `playground::loaded_to_module_info`). Used by ad-hoc loaders
    /// (`vm_profile`, the eval-spec test helpers) that just need the
    /// dep's symbol layout to feed `SymbolTable::build` /
    /// `pipeline::run`'s `dep_modules` slot.
    pub fn from_loaded(loaded: &LoadedModule) -> Self {
        Self::from_items(loaded.dep_name.clone(), &loaded.items, None)
    }
}

/// Capability declarations and their homogeneous module semantics, retained
/// anywhere a parsed dependency is projected into [`ModuleInfo`].
pub fn capability_metadata(items: &[TopLevel]) -> (Vec<CapabilityItem>, Option<String>) {
    let declarations = items
        .iter()
        .filter_map(|item| match item {
            TopLevel::Capability(declaration) => Some(declaration.clone()),
            _ => None,
        })
        .collect();
    let semantics = items.iter().find_map(|item| match item {
        TopLevel::Module(module) => module.semantics.clone(),
        _ => None,
    });
    (declarations, semantics)
}

/// `verify … law` blocks from a module's top-level items, in source
/// order. Plain example-style `verify` blocks (`VerifyKind::Example`)
/// are excluded — only laws are lifted into a dep's [`ModuleInfo`] for
/// the cross-file law pool.
///
/// VISIBILITY gate (cross-file law pool, fail-closed at admission): a
/// law is lifted ONLY when its SUBJECT fn is EXPOSED by the module —
/// the same rule [`crate::visibility::collect_module_exports`] /
/// [`crate::visibility::SymbolRegistry::from_modules`] apply to fns. A
/// law about a private helper (`_`-prefixed, or absent from a non-empty
/// `exposes [...]` list) is a module-internal obligation: it is still
/// proved in the module's OWN export, but it never enters a consumer's
/// pool, is never emitted into a consumer's build, and is never lowered
/// for a consumer. A consumer can only cite what its dependency makes
/// public, exactly as it can only CALL exposed fns.
pub fn collect_verify_laws(items: &[TopLevel]) -> Vec<crate::ast::VerifyBlock> {
    use crate::ast::VerifyKind;
    let module = crate::visibility::module_decl(items);
    let exposes: Option<&[String]> = module.and_then(|m| {
        if m.exposes.is_empty() {
            None
        } else {
            Some(m.exposes.as_slice())
        }
    });
    items
        .iter()
        .filter_map(|i| match i {
            TopLevel::Verify(vb)
                if matches!(vb.kind, VerifyKind::Law(_))
                    && crate::visibility::is_exposed(&vb.fn_name, exposes) =>
            {
                Some(vb.clone())
            }
            _ => None,
        })
        .collect()
}

/// Collected context from the Aver program, shared across all backends.
///
/// # Invariant (epic #170 Phase 2)
///
/// **`resolved_program` is the primary backend input.** Every
/// identity-sensitive decision (call/ctor/type lookup, fn-by-id
/// dispatch, mutual-SCC analysis) belongs to that view; the
/// pipeline produced it once and `build_context` projects it through.
///
/// The legacy AST-shape fields below — `items`, `fn_defs`,
/// `type_defs`, `resolved_fn_defs`, `resolved_module_fn_defs` — are
/// **source metadata / migration caches**, not independent sources
/// of truth:
///
/// - `items`, `fn_defs`, `type_defs` retain source-shape spans and
///   diagnostics; backends mid-migration still walk them. They are
///   NOT the place to add new identity-sensitive logic.
/// - `resolved_fn_defs` / `resolved_module_fn_defs` are projections
///   of `resolved_program` kept for callsites that don't yet route
///   through the `FnId` index. New code should reach
///   `resolved_program.fn_by_id(fn_id)` instead.
///
/// Subsequent epic phases migrate backends (Rust, Lean, Dafny,
/// wasm-gc) to iterate the view directly. New code in backends
/// should default to the view. AST consumption requires a clear
/// category in a code comment: `diagnostic-only`,
/// `syntax-discovery-only`, `backend-link-stage`, `display-only`,
/// or `temporary-migration-bridge`.
/// Whether a declined claim was a `verify … law` block or a plain
/// `verify` block's sampled cases.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum DeclineKind {
    /// `verify <fn> law <name>` — identity is `fn.law`.
    Law,
    /// `verify <fn>` — identity is `fn`.
    Cases,
}

impl DeclineKind {
    pub fn as_str(self) -> &'static str {
        match self {
            DeclineKind::Law => "law",
            DeclineKind::Cases => "cases",
        }
    }
}

/// A claim the proof exporter would not state, and why.
///
/// "Would not state" is stronger than "failed to prove": nothing about the
/// claim reaches the backend, so no verifier can fail on it and no sorry can
/// stand in for it. That makes it invisible to every existing count, which is
/// precisely why it gets its own.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeclinedClaim {
    pub kind: DeclineKind,
    /// `fn.law` for a law block, `fn` for a plain block — the same identity
    /// the proof manifest and `--gate` key on.
    pub claim: String,
    /// One sentence, user-facing: why the exporter would not state it.
    pub reason: String,
}

pub struct CodegenContext {
    /// All top-level items (post-TCO transform, post-typecheck).
    ///
    /// **Source metadata** — kept for span / diagnostic / syntax
    /// discovery access. Backends iterating fn bodies should reach
    /// `resolved_program.entry_fns()` instead.
    pub items: Vec<TopLevel>,
    /// User-defined type definitions (for struct/enum generation).
    ///
    /// **Source metadata.** Type-id-keyed lookups go through
    /// `symbol_table` (see [`Self::symbol_table`]); fn bodies that
    /// need a resolved type reach it via `Type::Named(TypeId, _)`
    /// after the typechecker stamps. This list stays for ergonomic
    /// iteration over user-declared types in syntax-discovery sites
    /// (e.g. cataloguing all `enum` declarations for the proof
    /// pipeline's refinement detection).
    pub type_defs: Vec<TypeDef>,
    /// User-defined function definitions.
    ///
    /// **Source metadata.** Backends mid-migration walk this for
    /// fn-signature shape; new identity-sensitive code reaches
    /// `resolved_program.entry_fns()` / `fn_by_id(fn_id)` instead.
    /// Synthesized FnDefs (TCO hoists) appended after
    /// the pipeline ran live here too; the on-demand resolver
    /// (`Self::resolve_fn_def`) lifts them through the symbol table.
    pub fn_defs: Vec<FnDef>,
    /// Project/binary name.
    pub project_name: String,
    /// Dependent modules loaded for inlining.
    pub modules: Vec<ModuleInfo>,
    /// Canonical capability contracts from the entry plus its transitive
    /// dependency closure. Proof/runtime backends must classify against this
    /// registry rather than the built-in effect table alone.
    pub capabilities: crate::capability::CapabilityRegistry,
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
    /// Proof-derived compact layouts for opaque `List<Int>` refinements in
    /// the unflattened source program. The table is representation-neutral:
    /// backends may consume only the element widths they can preserve
    /// soundly. Ungated construction removes a candidate before it reaches
    /// this table, so an entry certifies both the element interval and the
    /// smart-constructor-only construction discipline.
    pub packed_sequence_layouts: HashMap<String, crate::codegen::proof_lower::PackedSequenceLayout>,
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
    /// in the same source order.
    ///
    /// **Compatibility projection of `resolved_program.entry_fns()`**
    /// (epic #170 Phase 1). Position-aligned with the entry slice of
    /// `resolved_program.entry_items`. New code should prefer
    /// `resolved_program.entry_fns()` / `fn_by_id(fn_id)` so the
    /// `FnId` index is the lookup mechanism. This vec stays for
    /// callsites that haven't yet been migrated to the view; it will
    /// be retired once Phase 3-6 migrate all backends.
    pub resolved_fn_defs: Vec<crate::ir::hir::ResolvedFnDef>,
    /// Module scope currently active for name resolution. Set by a
    /// backend dispatcher before emitting a dep-module's fns so that
    /// legacy resolve-on-demand adapters (e.g. Lean's
    /// `emit_expr_legacy`) thread the right scope into
    /// `resolve_expr` / `resolve_stmt` instead of defaulting to entry.
    /// Empty by default. Set with [`Self::with_module_scope`] in a
    /// scoped manner.
    pub current_module_scope: std::cell::RefCell<Option<String>>,
    /// Whether the Lean emitter is currently rendering inside a `do` block.
    /// This scopes `?` emission: nested error propagation uses a monadic bind
    /// only where Lean has a surrounding action to lift it into. Function
    /// bodies, verify cases and law statements all set it; positions that are
    /// not actions — a `when` premise, a trace projection — do not.
    pub lean_do_block: std::cell::Cell<bool>,
    /// Claims the exporter refused to state, keyed by identity so the
    /// same refusal seen twice counts once.
    ///
    /// A refused claim used to leave one trace: a comment in the generated
    /// file. Nothing was printed, nothing was counted and the exit code did
    /// not move, so a user could read "0 sorries" and believe a law had been
    /// certified when it was never stated. Worse, widening a refusal could
    /// turn a RED `--check` GREEN: the claim that previously failed to build
    /// simply stops being emitted, and `build_errors` drops to zero. The
    /// count has to leave codegen for the driver to report and charge it, and
    /// a structured sink is the only honest channel — the refusal *text* is
    /// UI and has been reworded before, so anything that greps the generated
    /// file for it zeroes silently the next time someone edits a sentence.
    ///
    /// Written by the shared Map-order refusal gates and by proof backends'
    /// own soundness refusals (for example Lean sampled claims whose call cone
    /// reaches a fuel fallback without a statically justified bound); read by
    /// `cmd_proof`. Keyed rather than pushed because each gate can be consulted
    /// several times per claim, and a `Vec` would multiply-count.
    pub declined_claims: std::cell::RefCell<std::collections::BTreeMap<String, DeclinedClaim>>,
    /// Every construct the Rust emitter substituted a `compile_error!` for,
    /// in emit order, as the message it wrote.
    ///
    /// Same reason as `declined_claims` one field up: the emitter knows it
    /// refused, so it says so here. Re-discovering the refusal by scanning
    /// the generated files for the macro name cannot tell the backend's own
    /// output apart from a program's data — a `String` literal quoting
    /// `compile_error!` is ordinary Aver — and it goes quiet the day the
    /// wording changes.
    ///
    /// Written by `rust::toplevel::emit_codegen_error_expr` and by the
    /// mutual-TCO block fallback; read out into
    /// [`ProjectOutput::substituted_compile_errors`] at the end of a Rust
    /// transpile.
    pub substituted_compile_errors: std::cell::RefCell<Vec<String>>,
    /// Verify cases the Rust emitter left out of the generated
    /// `#[cfg(test)]` module because the MIR walker could not render them,
    /// as "`fn` name: reason".
    ///
    /// A dropped case is not a build failure — the verify-only Oracle and
    /// trace shapes are exercised by `aver verify` and were never meant to
    /// run as `cargo test` — but it must not be a secret either: `compile`
    /// reports what it left behind.
    pub omitted_verify_cases: std::cell::RefCell<Vec<String>>,
    /// Per-dep resolved fn defs, parallel to `modules`.
    ///
    /// **Compatibility projection of `resolved_program.modules[i].fn_defs`**
    /// (epic #170 Phase 1). Position-aligned with `modules` for
    /// callsites that index by `modules[i]`. New code should prefer
    /// `resolved_program.module_fns(prefix)` or the global
    /// `fn_by_id(fn_id)` index — that's where cross-module bare-name
    /// disambiguation happens for free. Retired alongside
    /// `resolved_fn_defs` once Phase 3-6 migrate the remaining
    /// backends.
    pub resolved_module_fn_defs: Vec<Vec<crate::ir::hir::ResolvedFnDef>>,
    /// Canonical resolved-program view of the whole codegen input —
    /// entry items (post-pipeline `NameResolve`) + per-dep-module
    /// resolved fn defs + `FnId`-keyed lookup.
    ///
    /// **Epic #170 Phase 1 invariant.** `resolved_program` is the
    /// primary source of truth for backend codegen — `fn_defs`,
    /// `type_defs`, `items`, `resolved_fn_defs`, and
    /// `resolved_module_fn_defs` remain available as projection /
    /// source metadata / migration cache, but consumers should reach
    /// the view first when an `FnId` / `TypeId` is in hand. Subsequent
    /// phases (#170 Phase 3+) migrate backends to iterate the view as
    /// their primary input; this field is the foundation those PRs
    /// build on.
    pub resolved_program: crate::codegen::program_view::ResolvedProgramView,
    /// Whole-program shape facts — typed Archetype labels + call-graph
    /// SCC per `FnId`. Computed once per compilation by
    /// [`analyze_program`](crate::analysis::shape::analyze_program) at
    /// `build_context` time. Stage 5+ of #232 (0.23 "Shape") migrates
    /// ad-hoc fn-shape detectors in proof codegen to read this instead
    /// of rewalking the AST. `None` only for tests that assemble the
    /// ctx by hand without calling `build_context`; downstream callers
    /// should treat that as opt-out (preserve legacy detection path).
    pub program_shape: Option<crate::analysis::shape::ProgramShape>,
    /// Optimized Core MIR for the whole codegen input (entry + dep
    /// module fns), `FnId`-keyed. Built once at `build_context` from
    /// the resolved program — the same lowering + optimizer pass the
    /// VM / wasm-gc / wasip2 backends run. The Rust backend reads
    /// `fn_by_id(fn_id)` here to drive its sole codegen path
    /// (`from_mir::emit_mir_fn_body_routed`): the MIR walker owns all
    /// runtime codegen after the HIR walker's deletion (W6/Stage-3).
    /// `None` for hand-assembled test contexts that skip `build_context`.
    pub mir_program: Option<crate::ir::mir::MirProgram>,
    /// Per-`(FnId, LocalId)` bare-`i64` representation facts for the Int
    /// "unboxing" optimization, computed from `mir_program` by
    /// `bare_i64::analyze`. The Rust backend reads a per-fn slice at
    /// signature + body emit to select native `i64` vs the default
    /// `aver_rt::AverInt` for provably-bounded, non-escaping Int values.
    /// Fail-closed: empty (all-`Boxed`) for hand-assembled test contexts
    /// and for dependency-module fragments (callers unseen).
    pub bare_i64: crate::ir::mir::BareI64Facts,
    /// Kernel-proved lemmas parsed back from a committed
    /// `DiscoveredLemmas.lean` (the `--discover` artifact), set by the CLI
    /// on a normal `aver proof` run when the discovery-surface hash still
    /// matches. The Lean backend embeds each pinned lemma's text before the
    /// first law theorem that uses it (re-proving it in the same build) and
    /// `simp`s over its name (`ProofStrategy::SimpOverLemmas`). Empty unless
    /// the CLI wired it — discovery feedback is strictly opt-in.
    pub discovered_lemmas: Vec<crate::codegen::lemma_discovery::CommittedLemma>,
    /// VM-computed ground-truth values for verify cases, keyed by
    /// `(common::verify_block_counter_key(vb), global_case_index)` →
    /// `aver_repr_literal` rendering of the case's expected (right-side)
    /// value. Set by the CLI on `aver proof --backend lean` from a Declared-
    /// mode `aver verify` run over the entry items; empty everywhere else.
    /// The Lean emitter literalizes the expected side of bounded sample
    /// checks from this table (model-vs-ground-truth) so that fuel
    /// exhaustion — where `panic!` returns `default` and a model-vs-model
    /// equation becomes vacuously true under `native_decide` — cannot
    /// kernel-certify a false equation. Entries exist only for cases that
    /// PASSED `aver verify`; failing/skipped cases keep the source RHS.
    pub sample_expected: std::collections::HashMap<(String, usize), String>,
    /// `(common::verify_block_counter_key(vb), global_case_index)` → the
    /// reason `aver verify` gave for not answering that case.
    ///
    /// The counterpart to [`Self::sample_expected`], and the reason it is a
    /// separate table rather than an absence: a case with no ground truth
    /// falls back to the source RHS, which for a declined case would emit
    /// `impl(sample) = <the author's expected expression>` — a claim nothing
    /// checked, in exactly the shape literalization exists to prevent, and
    /// precisely on the big inputs where the model is likeliest to exhaust
    /// fuel too. A case listed here is declined as a claim instead.
    pub declined_cases: std::collections::HashMap<(String, usize), String>,
    /// `aver proof --allow-mathlib` (Lean only, opt-in): permit a generic
    /// Mathlib break-glass closing arm on laws the core strategies cannot
    /// claim. When `false` (the default) the Lean backend is BYTE-IDENTICAL to
    /// before — no Mathlib import, no break-glass arm, same tiers. When `true`
    /// a walling `when`-law is emitted in true-universal form with a domain-
    /// blind Mathlib tactic portfolio (`aver_mathlib`, keyed on
    /// `Int.ediv_ediv_of_nonneg` / `pow_add` / `positivity` / `nlinarith` / …)
    /// under a `first | (trace "AVER_MATHLIB:fn.law"; …) | sorry` floor. The
    /// post-emit step (`setup_mathlib_for_project`) wires the cached Mathlib
    /// into the generated lake project; the build-log trace marker drives the
    /// per-law `mathlib` credit. Axiom whitelist is UNCHANGED — Mathlib lemmas
    /// are kernel-clean `{propext, Classical.choice, Quot.sound}`.
    pub allow_mathlib: bool,
    /// Hand-proof sidecars, keyed by `(fn_name, law_name)` → the proof BODY
    /// (the tactic text after Lean's `:= by`, or the Dafny lemma body between
    /// `{` and `}`). Loaded by the CLI from a project's source-controlled
    /// `proofs/<lean|dafny>/<fn>__<law>.{lean,dfy}` sidecar dir for the active
    /// backend; empty everywhere else. When an entry exists for a law, the
    /// codegen splices the body into that law's emitted theorem/lemma and lets
    /// the kernel (lake / dafny verify) re-check it — a WRONG body fails the
    /// build loudly and the law is denied universal credit. A law with NO
    /// sidecar is byte-identical to before. The genuinely-hard lemmas the
    /// generic engine cannot find (trunc-sticky composition, sticky-plus) live
    /// here as LABELED, kernel-checked hand proofs (manifest credit `hand`).
    pub hand_proofs: std::collections::HashMap<(String, String), String>,
}

/// Output files from a codegen backend.
pub struct ProjectOutput {
    /// Files to write: (relative_path, content).
    pub files: Vec<(String, String)>,
    /// What the backend substituted a `compile_error!` for while producing
    /// `files`, one message per construct, in emit order. Empty for a
    /// backend that has no such construct (Lean, Dafny).
    pub substituted_compile_errors: Vec<String>,
    /// Verify cases the backend left out of the generated test module, one
    /// line each. Not a failure — the generated crate builds and its
    /// `cargo test` passes — but the user is told which of their cases the
    /// crate does not carry.
    pub omitted_verify_cases: Vec<String>,
}

impl ProjectOutput {
    /// Files from a backend that has no `compile_error!` substitution to
    /// report (Lean, Dafny) — or none this time.
    pub fn of(files: Vec<(String, String)>) -> Self {
        Self {
            files,
            substituted_compile_errors: Vec::new(),
            omitted_verify_cases: Vec::new(),
        }
    }

    /// The constructs the backend could not render and wrote a
    /// `compile_error!` for instead.
    ///
    /// A code generator that writes a deliberate compile error must not
    /// report success: without this the command exited 0 and the failure
    /// surfaced only when the user reached `cargo build`.
    ///
    /// The list is what the emitter RECORDED, not what a scan of the output
    /// found. Scanning cannot work here: `compile_error!` in a generated
    /// file is either the backend's own refusal or a string the program
    /// itself carries — `"rustc says: compile_error!(...)"` is ordinary Aver
    /// data — and no amount of care about the pattern separates the two,
    /// because both are just text in the same file. Reading the fact from
    /// the emitter that produced it cannot be triggered by program data at
    /// all, and cannot go quiet when someone rewords a message.
    pub fn generated_compile_errors(&self) -> &[String] {
        &self.substituted_compile_errors
    }
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
#[allow(clippy::too_many_arguments)]
pub fn build_context(
    items: Vec<TopLevel>,
    tc_result: &TypeCheckResult,
    entry_analysis: Option<&crate::ir::AnalysisResult>,
    project_name: String,
    modules: Vec<ModuleInfo>,
    symbol_table: crate::ir::SymbolTable,
    resolved_items: Vec<crate::ir::hir::ResolvedTopLevel>,
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

    // Build the canonical resolved view before any codegen analysis. Fallback
    // SCC discovery below reads the same FnId-keyed bodies the backends emit.
    let resolved_program = crate::codegen::program_view::ResolvedProgramView::build(
        resolved_items,
        &modules,
        &symbol_table,
    );

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
            let entry_fns: Vec<&crate::ir::hir::ResolvedFnDef> = resolved_program
                .entry_fns()
                .filter(|fd| fd.name != "main")
                .collect();
            for group in crate::call_graph::tailcall_scc_components_resolved(&entry_fns) {
                for fd in group {
                    mutual_tco_members.insert(fd.fn_id);
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
                let mod_fns: Vec<&crate::ir::hir::ResolvedFnDef> =
                    resolved_program.module_fns(&module.prefix).collect();
                for group in crate::call_graph::tailcall_scc_components_resolved(&mod_fns) {
                    for fd in group {
                        mutual_tco_members.insert(fd.fn_id);
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

    // Legacy projection fields remain for consumers still migrating, but
    // both project from the one resolved view built above.
    let resolved_fn_defs: Vec<crate::ir::hir::ResolvedFnDef> =
        resolved_program.entry_fns().cloned().collect();
    let resolved_module_fn_defs: Vec<Vec<crate::ir::hir::ResolvedFnDef>> = resolved_program
        .modules
        .iter()
        .map(|m| m.fn_defs.clone())
        .collect();

    // Compute program shape before moving items / modules into ctx.
    // Once-per-compilation analysis substrate (#232 stage 4+); ad-hoc
    // detectors in codegen (e.g. dafny's `is_directly_recursive`,
    // future stage 6 adapters for `refinement_info_for`) read from
    // this instead of rewalking the AST.
    let program_shape = {
        let mut all_fns: Vec<&crate::ir::hir::ResolvedFnDef> =
            resolved_program.entry_fns().collect();
        for m in &resolved_program.modules {
            for fd in &m.fn_defs {
                all_fns.push(fd);
            }
        }
        Some(crate::analysis::shape::analyze_program_with_modules(
            &all_fns, &items, &modules,
        ))
    };

    // Lower the whole resolved program (entry + dep-module fns) to
    // optimized Core MIR, once, and key it by `FnId`. The Rust
    // backend reads `fn_by_id` here to render every fn body (its sole
    // codegen path); building it here (rather than per-fn) keeps the
    // lowering cost O(program) instead of O(program²). Same
    // `lower_program` → `optimize` pass the other MIR backends run.
    let mir_program = {
        let mut mir_items: Vec<crate::ir::hir::ResolvedTopLevel> = resolved_program
            .entry_fns()
            .cloned()
            .map(crate::ir::hir::ResolvedTopLevel::FnDef)
            .collect();
        for m in &resolved_program.modules {
            for fd in &m.fn_defs {
                mir_items.push(crate::ir::hir::ResolvedTopLevel::FnDef(fd.clone()));
            }
        }
        Some(crate::ir::mir::optimize(crate::ir::mir::lower_program(
            &mir_items,
        )))
    };

    // Int "unboxing": derive the per-(FnId, LocalId) bare-`i64`
    // representation facts from the optimized MIR. Read-only — never
    // mutates the program. Empty (all-`Boxed`) when there is no MIR
    // (defensive) or for fragments the analysis bails on.
    //
    // ETAP-2 SLICE 0+1: this facts path (the VM-side / general read) passes
    // an EMPTY carrier table, so no carrier slot lowers here — byte-identical
    // to the pre-slice behavior. Carrier lowering is opt-in at the two
    // codegen-rewrite entries (`rewrite_for_rust` / `rewrite_for_wasm_gc`),
    // which build the real table from the refinement-via-opaque inputs.
    let empty_carrier = crate::ir::mir::bare_i64::CarrierIntervals::new();
    let bare_i64 = mir_program
        .as_ref()
        .map(|p| crate::ir::mir::bare_i64::analyze(p, &empty_carrier))
        .unwrap_or_default();

    let mut ctx = CodegenContext {
        items,
        type_defs,
        fn_defs,
        project_name,
        modules,
        capabilities: tc_result.capabilities.clone(),
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
        packed_sequence_layouts: HashMap::new(),
        bare_i64,
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
        current_module_scope: std::cell::RefCell::new(None),
        lean_do_block: std::cell::Cell::new(false),
        declined_claims: std::cell::RefCell::new(std::collections::BTreeMap::new()),
        substituted_compile_errors: std::cell::RefCell::new(Vec::new()),
        omitted_verify_cases: std::cell::RefCell::new(Vec::new()),
        program_shape,
        resolved_program,
        mir_program,
        discovered_lemmas: Vec::new(),
        sample_expected: std::collections::HashMap::new(),
        declined_cases: std::collections::HashMap::new(),
        allow_mathlib: false,
        hand_proofs: std::collections::HashMap::new(),
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
    let packed_inputs = crate::codegen::proof_lower::ProofLowerInputs::from_ctx(&ctx);
    let mut packed_layouts =
        crate::codegen::proof_lower::packed_sequence_layout_table(&packed_inputs);
    let candidates = packed_layouts.keys().cloned().collect::<HashSet<_>>();
    let intervals = packed_layouts
        .iter()
        .map(|(name, layout)| (name.clone(), (layout.element_interval, true)))
        .collect();
    let demoted = crate::codegen::proof_lower::carrier_ungated_construction_demotions(
        &packed_inputs,
        &candidates,
        &intervals,
        &HashMap::new(),
    );
    packed_layouts.retain(|name, _| !demoted.contains(name));
    ctx.packed_sequence_layouts = packed_layouts;
    ctx
}

impl CodegenContext {
    /// Set `current_module_scope` for the duration of `f`. Backends
    /// wrap their per-module emit calls with this so legacy
    /// resolve-on-demand adapters see the correct prefix.
    pub fn with_module_scope<R>(&self, scope: Option<&str>, f: impl FnOnce() -> R) -> R {
        let prev = self
            .current_module_scope
            .replace(scope.map(|s| s.to_string()));
        let out = f();
        *self.current_module_scope.borrow_mut() = prev;
        out
    }

    /// Set the Lean `do`-block emission mode for the duration of `f`.
    pub fn with_lean_do_block<R>(&self, on: bool, f: impl FnOnce() -> R) -> R {
        let prev = self.lean_do_block.replace(on);
        let out = f();
        self.lean_do_block.set(prev);
        out
    }

    /// Snapshot of the active module scope. Cloned so callers may
    /// pass `as_deref()` into resolver/emitter APIs without holding
    /// the `RefCell` borrow.
    pub fn active_module_scope(&self) -> Option<String> {
        self.current_module_scope.borrow().clone()
    }

    /// Resolve a verify-law's target fn name to its [`FnId`] under the
    /// active module scope: `FnKey::in_module(scope, name)` when a dep
    /// module is in scope (cross-file law pool — the dep law's
    /// `LawTheorem` is keyed by its dep-scope id), else
    /// `FnKey::entry(name)`. The entry key is tried as a fallback so an
    /// entry law emitted with a stale scope still resolves. Drives the
    /// `proof_ir.law_theorems` strategy lookup so a dep law gets the
    /// SAME auto-proof strategy an entry law of that shape would.
    pub fn law_target_fn_id(&self, fn_name: &str) -> Option<crate::ir::FnId> {
        if let Some(scope) = self.active_module_scope()
            && let Some(id) = self
                .symbol_table
                .fn_id_of(&crate::ir::FnKey::in_module(scope, fn_name))
        {
            return Some(id);
        }
        self.symbol_table
            .fn_id_of(&crate::ir::FnKey::entry(fn_name))
    }

    /// Identity-keyed lookup from a bare fn name + scope to the
    /// matching `&FnDef` in `fn_defs` / `modules[i].fn_defs`. Resolves
    /// the name through the symbol table to an `FnId` first, then
    /// recovers the AST `FnDef` via `fn_id_for_decl` pointer-eq scope
    /// matching — so two same-bare-name fns across modules can't
    /// cross-resolve.
    ///
    /// **Epic #170 Phase 5 helper.** Replaces the
    /// `ctx.fn_defs.iter().find(|fd| fd.name == name)` pattern that
    /// proof-mode law / verify rewriters used pre-migration. Backends
    /// that still need a `&FnDef` (rather than the resolved twin —
    /// e.g. `rewrite_effectful_calls_in_law` consumes AST shape)
    /// reach this method instead of walking by bare name.
    ///
    /// Returns `None` when the symbol table doesn't know the name
    /// under the given scope, or when the resolved `FnId` doesn't
    /// match any `&FnDef` in that scope (synthetic FnDefs added
    /// post-pipeline fall through here — callers can fallback to a
    /// bare-name walk over `extra_fn_defs` etc. when that matters).
    pub fn fn_def_by_name(&self, name: &str, scope: Option<&str>) -> Option<&FnDef> {
        use crate::ir::FnKey;
        let key = match scope {
            Some(prefix) => FnKey::in_module(prefix.to_string(), name),
            None => FnKey::entry(name),
        };
        let fn_id = self.symbol_table.fn_id_of(&key)?;
        let matches = |fd: &&FnDef| crate::codegen::common::fn_id_for_decl(self, fd) == Some(fn_id);
        match scope {
            None => self.fn_defs.iter().find(matches),
            Some(prefix) => self
                .modules
                .iter()
                .find(|m| m.prefix == prefix)?
                .fn_defs
                .iter()
                .find(matches),
        }
    }
}

impl CodegenContext {
    /// Test-only bridge: recompute every derived fact
    /// (`mutual_tco_members`, `recursive_fns`, `proof_ir`,
    /// `resolved_program`) from the current `items` and `modules`.
    /// Used exclusively by unit tests that construct a
    /// `CodegenContext` piecewise — pushing synthetic `FnDef`s
    /// straight into the items list rather than going through the
    /// parser and pipeline. Production code never needs this: every
    /// derived fact is populated by the pipeline stages (analyze,
    /// proof_lower) and propagated through `build_context`. Calling
    /// `refresh_facts` on a production-built ctx is redundant work
    /// that produces the same answer — leave it off the hot path.
    ///
    /// **Single-source-of-truth invariant** (epic #170 Phase 1+2):
    /// rebuilds `resolved_program` once from the freshly-resolved
    /// items, then derives `resolved_fn_defs` /
    /// `resolved_module_fn_defs` as projections of that view. There
    /// is no parallel resolve path here.
    pub fn refresh_facts(&mut self) {
        // Synthetic-ctx path must own its symbol table too — FnId-
        // keyed sets below resolve through it, same shape as the
        // production `build_context` flow.
        let symbol_table = crate::ir::SymbolTable::build(&self.items, &self.modules);
        let entry_resolved_items = crate::ir::hir::resolve_program(&symbol_table, &self.items);
        let resolved_program = crate::codegen::program_view::ResolvedProgramView::build(
            entry_resolved_items,
            &self.modules,
            &symbol_table,
        );
        let entry_fn_refs: Vec<&crate::ir::hir::ResolvedFnDef> = resolved_program
            .entry_fns()
            .filter(|fd| fd.name != "main")
            .collect();

        let mut mutual_tco_members: HashSet<crate::ir::FnId> = HashSet::new();
        for group in crate::call_graph::tailcall_scc_components_resolved(&entry_fn_refs) {
            for fd in group {
                mutual_tco_members.insert(fd.fn_id);
            }
        }
        for module in &self.modules {
            let mod_fns: Vec<&crate::ir::hir::ResolvedFnDef> =
                resolved_program.module_fns(&module.prefix).collect();
            for group in crate::call_graph::tailcall_scc_components_resolved(&mod_fns) {
                for fd in group {
                    mutual_tco_members.insert(fd.fn_id);
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

        // Publish the one view used for SCC discovery above. Projection
        // fields remain for consumers still migrating.
        self.resolved_program = resolved_program;
        self.resolved_fn_defs = self.resolved_program.entry_fns().cloned().collect();
        self.resolved_module_fn_defs = self
            .resolved_program
            .modules
            .iter()
            .map(|m| m.fn_defs.clone())
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
    /// FnDefs inserted between `build_context` and emit (TCO hoist
    /// rewrites, test fixtures) which the resolver hasn't lifted
    /// upfront.
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
            // Canonical lookup goes through the resolved-program view —
            // its `fn_by_id` index is the single FnId-keyed source for
            // the resolved body, replacing the dual-walk over
            // `resolved_fn_defs` + `resolved_module_fn_defs` that
            // predated #170 Phase 1.
            if let Some(rfd) = self.resolved_program.fn_by_id(fn_id) {
                return Cow::Borrowed(rfd);
            }
            // Symbol table knew the key but the view didn't index it.
            // Falls through to the synthetic-fallback path below; in
            // production this shouldn't happen.
        }

        // Synthetic FnDef path — TCO hoist rewrites, test fixtures
        // the resolver never saw. Lift on demand against the entry's
        // resolver context.
        let module_name = self.items.iter().find_map(|i| match i {
            TopLevel::Module(m) => Some(m.name.clone()),
            _ => None,
        });
        let mut rctx = ResolveCtx::new(&self.symbol_table);
        rctx.current_module = scope.map(String::from).or(module_name);
        let lifted = resolve_fn_def_external(&rctx, fd).unwrap_or_else(|| {
            let stmts: Vec<ResolvedStmt> = match fd.body.as_ref() {
                crate::ast::FnBody::Block(stmts) => {
                    stmts.iter().map(|s| self.resolve_stmt(s, scope)).collect()
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

    /// Entry module's name from `items` (the `module X` declaration's
    /// X). `None` for ad-hoc test programs without a module decl.
    pub(crate) fn entry_module_name(&self) -> Option<String> {
        self.items.iter().find_map(|i| match i {
            TopLevel::Module(m) => Some(m.name.clone()),
            _ => None,
        })
    }

    /// Entry module's direct `depends [...]` list in source order — the
    /// same projection [`ModuleInfo::from_items`] keeps for a dependency.
    /// Empty for ad-hoc programs without a module decl; `modules` may
    /// still hold the transitive closure (and implicitly loaded standard
    /// modules), which is deliberately not the same set.
    pub(crate) fn entry_depends(&self) -> Vec<String> {
        self.items
            .iter()
            .find_map(|i| match i {
                TopLevel::Module(m) => Some(m.depends.clone()),
                _ => None,
            })
            .unwrap_or_default()
    }

    /// Resolve a source-shape `Spanned<Expr>` on demand using the
    /// entry's resolver context. Used by emit helpers that still walk
    /// `Expr` (TCO hoisting, mutual TCO, verify blocks, follow-up
    /// backends pre-migration) and need to feed the resolved shape
    /// into the migrated emitter. The returned `Spanned<ResolvedExpr>`
    /// carries the same line + type stamp as the input.
    ///
    /// `scope` is the owning module prefix when the caller knows
    /// which dep module the expression lives in, `None` for entry-
    /// scope code. Required for cross-module name resolution — e.g.,
    /// a call site in module `A` referring to `Val.ValOk` declared
    /// in module `B` only resolves to `ResolvedCtor::User` when the
    /// resolver's `current_module` matches the call site's owning
    /// scope. Pre-PR-9.4 the helper used the *entry* module name
    /// uniformly, which broke cross-module ctor / fn classification
    /// for the legacy emit paths (mutual TCO trampolines, TCO hoist
    /// — they walked dep-module fn bodies but the resolver context
    /// said "you're in the entry module"; the self-host regen
    /// surfaced the gap when same-name shadowing across modules was
    /// no longer an option).
    pub fn resolve_expr(
        &self,
        expr: &crate::ast::Spanned<crate::ast::Expr>,
        scope: Option<&str>,
    ) -> crate::ast::Spanned<crate::ir::hir::ResolvedExpr> {
        use crate::ir::hir::{ResolveCtx, ResolvedStmt};
        let mut rctx = ResolveCtx::new(&self.symbol_table);
        rctx.current_module = scope.map(String::from).or_else(|| self.entry_module_name());
        let stmt = crate::ast::Stmt::Expr(expr.clone());
        match crate::ir::hir::resolve::resolve_stmt_external(&rctx, &stmt) {
            ResolvedStmt::Expr(s) => s,
            ResolvedStmt::Binding { value, .. } => value,
        }
    }

    /// Same as [`Self::resolve_expr`] but for whole statements
    /// (`Binding(name, ty_ann, expr)` or `Expr(expr)`).
    pub fn resolve_stmt(
        &self,
        stmt: &crate::ast::Stmt,
        scope: Option<&str>,
    ) -> crate::ir::hir::ResolvedStmt {
        use crate::ir::hir::ResolveCtx;
        let mut rctx = ResolveCtx::new(&self.symbol_table);
        rctx.current_module = scope.map(String::from).or_else(|| self.entry_module_name());
        crate::ir::hir::resolve::resolve_stmt_external(&rctx, stmt)
    }

    /// Resolve a source-shape [`crate::ast::Pattern`] to its resolved
    /// HIR form. Wraps the pattern in a synthetic match arm + drops
    /// it through `resolve_stmt_external`, since the resolver doesn't
    /// expose a standalone pattern lifter — same workaround
    /// `rust/toplevel.rs` used pre-PR-9.
    pub fn resolve_pattern(
        &self,
        pat: &crate::ast::Pattern,
        scope: Option<&str>,
    ) -> crate::ir::hir::ResolvedPattern {
        use crate::ast::{Expr, Literal, MatchArm, Spanned, Stmt};
        use crate::ir::hir::{ResolveCtx, ResolvedExpr, ResolvedStmt};
        let mut rctx = ResolveCtx::new(&self.symbol_table);
        rctx.current_module = scope.map(String::from).or_else(|| self.entry_module_name());
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

/// Per-key projection of the legacy `fn_sigs` map: routes a source-
/// level name through `resolved_program` first (entry + every dep
/// module's resolved fns), then walks `TypeDef`s for constructor sigs,
/// then handles the synthesised `__buf_*` intrinsics. Lets a
/// `CodegenContext` answer `FnSigOracle::fn_sig` without materialising
/// the whole `FnSigMap` up front — the verify-law helpers query
/// individual names, so per-key resolution is cheaper than per-call
/// rebuild.
fn codegen_ctx_fn_sig(ctx: &CodegenContext, name: &str) -> Option<crate::verify_law::FnSigInfo> {
    use crate::verify_law::FnSigInfo;

    if let Some(fn_id) = crate::codegen::common::fn_id_for_dotted_name(ctx, name)
        && let Some(rfd) = ctx.resolved_program.fn_by_id(fn_id)
    {
        return Some(FnSigInfo {
            return_type: rfd.return_type.clone(),
            is_pure: rfd.effects.is_empty(),
        });
    }

    // Constructor lookup: `Type.Variant` (entry sum), `Module.Type.
    // Variant` (module sum), `Box` (entry product), `Module.Box`
    // (module product). Walks the same `TypeDef` surfaces the
    // legacy fn_sigs population did via SymbolRegistry.
    let walk = |td: &crate::ast::TypeDef, scope: Option<&str>| -> Option<FnSigInfo> {
        match td {
            crate::ast::TypeDef::Sum {
                name: parent,
                variants,
                ..
            } => {
                let parent_full = match scope {
                    Some(prefix) => format!("{prefix}.{parent}"),
                    None => parent.clone(),
                };
                for v in variants {
                    let bare = format!("{parent}.{}", v.name);
                    let full = format!("{parent_full}.{}", v.name);
                    if name == bare || name == full {
                        return Some(FnSigInfo {
                            return_type: crate::types::Type::named(parent_full.clone()),
                            is_pure: true,
                        });
                    }
                }
                None
            }
            crate::ast::TypeDef::Product { name: parent, .. } => {
                let parent_full = match scope {
                    Some(prefix) => format!("{prefix}.{parent}"),
                    None => parent.clone(),
                };
                if name == parent || name == parent_full {
                    return Some(FnSigInfo {
                        return_type: crate::types::Type::named(parent_full),
                        is_pure: true,
                    });
                }
                None
            }
        }
    };
    for item in &ctx.items {
        if let TopLevel::TypeDef(td) = item
            && let Some(info) = walk(td, None)
        {
            return Some(info);
        }
    }
    for m in &ctx.modules {
        for td in &m.type_defs {
            if let Some(info) = walk(td, Some(&m.prefix)) {
                return Some(info);
            }
        }
    }

    // Synthesised `__buf_*` intrinsics — the deforestation pipeline
    // emits these as opaque callables; verify-law walkers may surface
    // a reference if a user's law body sketches the buffer pipeline.
    match name {
        "__buf_new" => Some(FnSigInfo {
            return_type: crate::types::Type::named("Buffer"),
            is_pure: true,
        }),
        "__buf_append" | "__buf_append_sep_unless_first" => Some(FnSigInfo {
            return_type: crate::types::Type::named("Buffer"),
            is_pure: true,
        }),
        "__buf_finalize" | "__str_cursor_head" => Some(FnSigInfo {
            return_type: crate::types::Type::Str,
            is_pure: true,
        }),
        // Chars-fusion cursor / codepoint intrinsics, same story.
        "__str_cursor_end" => Some(FnSigInfo {
            return_type: crate::types::Type::Bool,
            is_pure: true,
        }),
        "__str_cursor_next" | "__str_code1" | "__str_code1_lower" | "__str_code1_upper"
        | "__str_cursor_code" | "__str_fold_lower" | "__str_fold_upper" => Some(FnSigInfo {
            return_type: crate::types::Type::Int,
            is_pure: true,
        }),
        // The byte-builder trio is monomorphic — bytes are bytes — so
        // unlike its list sibling below this oracle can answer for it.
        "__byt_new" | "__byt_push" => Some(FnSigInfo {
            return_type: crate::types::Type::named("ByteBuilder"),
            is_pure: true,
        }),
        "__byt_finalize" => Some(FnSigInfo {
            return_type: crate::types::Type::Result(
                Box::new(crate::types::Type::List(Box::new(crate::types::Type::Int))),
                Box::new(crate::types::Type::Str),
            ),
            is_pure: true,
        }),
        // The list-build builder intrinsics (`__lst_new` / `__lst_push`
        // / `__lst_finalize`) are deliberately absent. Their return type
        // is the accumulator's own `List<T>`, and T is a fact about the
        // call site, not about the name — an answer this oracle cannot
        // give without inventing one. Saying nothing is the truthful
        // answer, and the same one it gives for every other name it does
        // not know.
        _ => None,
    }
}

impl crate::verify_law::FnSigOracle for CodegenContext {
    fn fn_sig(&self, name: &str) -> Option<crate::verify_law::FnSigInfo> {
        codegen_ctx_fn_sig(self, name)
    }
}

#[cfg(test)]
mod project_output_tests {
    use super::ProjectOutput;

    fn output(files: &[(&str, &str)]) -> ProjectOutput {
        ProjectOutput::of(
            files
                .iter()
                .map(|(path, content)| (path.to_string(), content.to_string()))
                .collect(),
        )
    }

    #[test]
    fn a_recorded_substitution_is_reported_with_its_reason() {
        let mut out = output(&[
            ("Cargo.toml", "[package]\nname = \"app\"\n"),
            ("src/main.rs", "fn main() {}\n"),
            (
                "src/aver_generated/domain/user/mod.rs",
                "pub fn added() { compile_error!(\"MIR walker could not render fn `added`\"); }\n",
            ),
        ]);
        out.substituted_compile_errors = vec!["MIR walker could not render fn `added`".to_string()];
        assert_eq!(
            out.generated_compile_errors(),
            ["MIR walker could not render fn `added`".to_string()]
        );
    }

    #[test]
    fn a_clean_crate_reports_nothing() {
        let out = output(&[
            ("Cargo.toml", "[package]\nname = \"app\"\n"),
            ("src/main.rs", "fn main() { println!(\"hi\"); }\n"),
        ]);
        assert!(out.generated_compile_errors().is_empty());
    }

    #[test]
    fn the_macro_in_program_data_is_not_a_backend_refusal() {
        // The user's own string, rendered verbatim into the crate. The
        // backend substituted nothing, so nothing is reported — the crate
        // compiles and printing that string is what the program does.
        let out = output(&[
            ("Cargo.toml", "[package]\nname = \"app\"\n"),
            (
                "src/aver_generated/main/mod.rs",
                "pub fn banner() -> &'static str { \"rustc says: compile_error!(...)\" }\n",
            ),
        ]);
        assert!(out.generated_compile_errors().is_empty());
    }
}

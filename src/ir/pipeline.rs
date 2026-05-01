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
use crate::ir::{AllocPolicy, AnalysisResult, CallLowerCtx};
use crate::source::LoadedModule;
use crate::types::checker::{TypeCheckResult, run_type_check_full, run_type_check_with_loaded};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum PipelineStage {
    Tco,
    Typecheck,
    InterpLower,
    BufferBuild,
    Resolve,
    LastUse,
    Analyze,
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
            alloc_policy: None,
            call_ctx: None,
            on_after_pass: None,
        }
    }
}

#[derive(Default)]
pub struct PipelineResult {
    /// Typecheck output, present iff `config.typecheck` was set. Callers
    /// inspect `.errors` and decide what to do — the orchestrator does not
    /// exit on its own.
    pub typecheck: Option<TypeCheckResult>,
    /// `(rewrites, synthesized)` from the buffer-build pass when it ran.
    pub buffer_build_stats: Option<(usize, usize)>,
    /// IR-level analysis facts (per-fn body shape, thin kind, alloc info)
    /// when `run_analyze` was on. `None` when the stage was disabled.
    pub analysis: Option<AnalysisResult>,
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
    }
}

/// Lower `"a${x}b"` interpolation literals into the buffer pipeline.
/// Skipped by proof exporters (Lean/Dafny) which want the source-level form.
pub fn interp_lower(items: &mut [TopLevel]) {
    crate::ir::lower_interpolation_pass(items);
}

/// Buffer-build deforestation pass — detects `String.join(<builder>(args, []), sep)`
/// shapes, rewrites them to `__buf_finalize(<builder>__buffered(...))`, and
/// appends the synthesized buffered variants to `items`. Returns `(rewrites, synthesized)`.
pub fn buffer_build(items: &mut Vec<TopLevel>) -> (usize, usize) {
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
        tco(items);
        fire(&mut cfg, PipelineStage::Tco, items);
    }

    if let Some(mode) = cfg.typecheck.as_ref() {
        let tc = typecheck(items, mode);
        let has_errors = !tc.errors.is_empty();
        result.typecheck = Some(tc);
        fire(&mut cfg, PipelineStage::Typecheck, items);
        if has_errors {
            return result;
        }
    }

    if cfg.run_interp_lower {
        interp_lower(items);
        fire(&mut cfg, PipelineStage::InterpLower, items);
    }

    if cfg.run_buffer_build {
        result.buffer_build_stats = Some(buffer_build(items));
        fire(&mut cfg, PipelineStage::BufferBuild, items);
    }

    if cfg.run_resolve {
        resolve(items);
        fire(&mut cfg, PipelineStage::Resolve, items);
    }

    if cfg.run_last_use {
        last_use(items);
        fire(&mut cfg, PipelineStage::LastUse, items);
    }

    if cfg.run_analyze {
        // The body classifier needs a `CallLowerCtx`. When no real ctx is
        // configured we use `StubCallCtx`, which under-classifies `direct`
        // shapes (a body that calls a fn whose name looks like a local
        // gets seen as a generic call). Acceptable for `--emit-ir` dumps;
        // codegen pipelines should plumb a real ctx through `cfg.call_ctx`
        // once the inliner needs accurate body shape data.
        let adapter = CallCtxAdapter(cfg.call_ctx);
        result.analysis = Some(crate::ir::analyze(items, cfg.alloc_policy, &adapter));
        fire(&mut cfg, PipelineStage::Analyze, items);
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
                PipelineStage::LastUse,
                PipelineStage::Analyze,
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
                PipelineStage::LastUse, // fires even without Resolve — a no-op pass
            ]
        );
    }
}

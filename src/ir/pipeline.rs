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
use crate::source::LoadedModule;
use crate::types::checker::{TypeCheckResult, run_type_check_full, run_type_check_with_loaded};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum PipelineStage {
    Tco,
    Typecheck,
    InterpLower,
    BufferBuild,
    Resolve,
}

impl PipelineStage {
    pub const fn name(self) -> &'static str {
        match self {
            Self::Tco => "tco",
            Self::Typecheck => "typecheck",
            Self::InterpLower => "interp_lower",
            Self::BufferBuild => "buffer_build",
            Self::Resolve => "resolve",
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
}

// ── Per-stage entry points ──────────────────────────────────────────

/// Tail-call rewrite pass.
pub fn tco(items: &mut [TopLevel]) {
    crate::tco::transform_program(items);
}

/// Run the type checker against `items` using the provided driver.
/// Pure read-only — never mutates the AST.
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

/// Resolve locals + annotate last-use information across the program.
pub fn resolve(items: &mut [TopLevel]) {
    crate::resolver::resolve_program(items);
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

    result
}

fn fire(cfg: &mut PipelineConfig<'_>, stage: PipelineStage, items: &[TopLevel]) {
    if let Some(cb) = cfg.on_after_pass.as_mut() {
        cb(stage, items);
    }
}

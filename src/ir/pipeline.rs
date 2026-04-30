//! Ordered compiler pass pipeline — the single source of truth for what
//! happens between `parse_*` and `codegen::*` / `vm::*`.
//!
//! Before this module the sequence
//!
//! ```text
//! tco → typecheck → interp_lower → buffer_build → resolve
//! ```
//!
//! lived inline in five different call sites (cmd_run_vm,
//! build_codegen_context, load_module_recursive, replay_cmd,
//! playground::*). Adding a new pass meant editing each by hand;
//! adding `--emit-ir-after=PASS` would have meant editing each
//! again. Funnel everything through `pipeline::run` instead.
//!
//! Stages are fixed-order. Buffer-build needs `Expr::TailCall` from
//! TCO; the resolver assumes traversal lowering is done; the proof
//! exporters need to skip traversal because they consume source-level
//! IR. Reordering is not a knob — disabling individual stages is.

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

/// Optional typecheck driver. `None` (the field default) skips typecheck.
pub enum TypecheckMode<'a> {
    /// `run_type_check_full(items, base_dir)`.
    Full { base_dir: Option<&'a str> },
    /// `run_type_check_with_loaded(items, loaded)` for in-memory module trees
    /// (playground virtual fs).
    WithLoaded(&'a [LoadedModule]),
}

pub struct PipelineConfig<'a> {
    /// Run interpolation lowering + buffer-build deforestation. Disabled
    /// for proof exporters (Lean/Dafny) — they want source-level IR.
    pub apply_traversal_lowering: bool,
    /// Run resolver after the traversal passes. Disabled by some test paths
    /// that only need TCO output.
    pub run_resolve: bool,
    /// How to drive the type checker, or `None` to skip it.
    pub typecheck: Option<TypecheckMode<'a>>,
    /// Stop after the named stage. The pipeline returns early without
    /// running later stages even if their flags are set.
    pub stop_after: Option<PipelineStage>,
    /// Hook fired after every stage that ran. Receives the stage label
    /// and the (post-mutation) item slice. Plumbed in for upcoming
    /// `--emit-ir-after=PASS` support; today's callers leave it `None`.
    pub on_after_pass: Option<Box<dyn FnMut(PipelineStage, &[TopLevel]) + 'a>>,
}

impl<'a> Default for PipelineConfig<'a> {
    fn default() -> Self {
        Self {
            apply_traversal_lowering: true,
            run_resolve: true,
            typecheck: None,
            stop_after: None,
            on_after_pass: None,
        }
    }
}

#[derive(Default)]
pub struct PipelineResult {
    /// Typecheck output, present iff `config.typecheck` was set. Callers
    /// inspect `.errors` and decide what to do — pipeline does not exit.
    pub typecheck: Option<TypeCheckResult>,
    /// `(rewrites, synthesized)` from the buffer-build pass when it ran.
    pub buffer_build_stats: Option<(usize, usize)>,
}

/// Run the canonical compiler pipeline on `items`.
///
/// If typecheck is enabled and surfaces errors, later stages are skipped
/// so callers can render the diagnostics without seeing partially-lowered
/// IR. The typecheck result is still in `PipelineResult::typecheck`.
pub fn run(items: &mut Vec<TopLevel>, mut config: PipelineConfig<'_>) -> PipelineResult {
    let mut result = PipelineResult::default();

    crate::tco::transform_program(items);
    fire(&mut config, PipelineStage::Tco, items);
    if config.stop_after == Some(PipelineStage::Tco) {
        return result;
    }

    if let Some(mode) = config.typecheck.as_ref() {
        let tc = match mode {
            TypecheckMode::Full { base_dir } => run_type_check_full(items, *base_dir),
            TypecheckMode::WithLoaded(loaded) => run_type_check_with_loaded(items, loaded),
        };
        let has_errors = !tc.errors.is_empty();
        result.typecheck = Some(tc);
        fire(&mut config, PipelineStage::Typecheck, items);
        if has_errors {
            return result;
        }
    }
    if config.stop_after == Some(PipelineStage::Typecheck) {
        return result;
    }

    if config.apply_traversal_lowering {
        crate::ir::lower_interpolation_pass(items);
    }
    fire(&mut config, PipelineStage::InterpLower, items);
    if config.stop_after == Some(PipelineStage::InterpLower) {
        return result;
    }

    if config.apply_traversal_lowering {
        result.buffer_build_stats = Some(crate::ir::run_buffer_build_pass(items));
    }
    fire(&mut config, PipelineStage::BufferBuild, items);
    if config.stop_after == Some(PipelineStage::BufferBuild) {
        return result;
    }

    if config.run_resolve {
        crate::resolver::resolve_program(items);
    }
    fire(&mut config, PipelineStage::Resolve, items);

    result
}

fn fire(config: &mut PipelineConfig<'_>, stage: PipelineStage, items: &[TopLevel]) {
    if let Some(cb) = config.on_after_pass.as_mut() {
        cb(stage, items);
    }
}

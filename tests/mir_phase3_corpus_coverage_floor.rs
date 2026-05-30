//! Phase 3 → 4 corpus-wide coverage floor.
//!
//! Phase 3's per-wave tests prove each `SkipReason` disappears
//! from `LowerStats.skipped` on small targeted fixtures. This
//! file walks the shipped corpus (`examples/core/`,
//! `examples/data/`) and asserts a minimum lowering ratio,
//! preventing silent corpus-wide regression as new sources land.
//!
//! Phase 4 (VM slice) will tighten this floor — once the VM
//! reads MIR, the floor becomes a hard gate.

use aver::ir::mir::{LowerStats, lower_program};
use aver::ir::pipeline::{self, PipelineConfig, TypecheckMode};
use aver::source::parse_source;
use std::fs;
use std::path::Path;

fn lower_file(path: &Path) -> Option<LowerStats> {
    let source = fs::read_to_string(path).ok()?;
    let mut items = parse_source(&source).ok()?;
    let result = pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full { base_dir: None }),
            ..Default::default()
        },
    );
    let tc = result.typecheck.as_ref()?;
    if !tc.errors.is_empty() {
        return None;
    }
    Some(lower_program(&result.resolved_items).stats)
}

fn aggregate(dir: &Path) -> LowerStats {
    let mut total = LowerStats::default();
    let entries = fs::read_dir(dir).expect("read examples dir");
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) != Some("av") {
            continue;
        }
        if let Some(stats) = lower_file(&path) {
            total.lowered += stats.lowered;
            for (reason, count) in stats.skipped {
                *total.skipped.entry(reason).or_insert(0) += count;
            }
        }
    }
    total
}

#[test]
fn examples_core_meets_coverage_floor() {
    // Per the per-wave tests, the lowerer should cover ~the
    // entirety of user-fn shapes used in `examples/core/`. Some
    // fns will still drop on `UnresolvedIdent` (resolver gap for
    // bare nullary builtin ctors) or `UnsupportedCallee`
    // (closures / intrinsics future work). Floor: 50% — leaves
    // room for Phase 4 to tighten as the VM consumer lands and
    // motivates fixing the remaining drops at their root cause.
    let stats = aggregate(Path::new("examples/core"));
    assert!(
        stats.total() > 0,
        "examples/core should contain lowering candidates: {:?}",
        stats
    );
    let ratio = stats.coverage_ratio();
    eprintln!("examples/core coverage: {ratio:.2} ({:?})", stats);
    assert!(
        ratio >= 0.50,
        "examples/core coverage ratio {ratio} below floor 0.50: {:?}",
        stats
    );
}

#[test]
fn examples_data_meets_coverage_floor() {
    let stats = aggregate(Path::new("examples/data"));
    assert!(
        stats.total() > 0,
        "examples/data should contain lowering candidates: {:?}",
        stats
    );
    let ratio = stats.coverage_ratio();
    eprintln!("examples/data coverage: {ratio:.2} ({:?})", stats);
    assert!(
        ratio >= 0.50,
        "examples/data coverage ratio {ratio} below floor 0.50: {:?}",
        stats
    );
}

#[test]
fn dropped_reasons_are_attributable_not_unsupported_other() {
    // Sanity: corpus drops should bucket into named reasons, not
    // the `UnsupportedOther` catch-all. If `UnsupportedOther`
    // ever fires it means the lowerer is missing a
    // `ResolvedExpr` variant from its match arm — a regression
    // worth catching early.
    let core = aggregate(Path::new("examples/core"));
    let data = aggregate(Path::new("examples/data"));
    let other_total = core
        .skipped
        .get(&aver::ir::mir::SkipReason::UnsupportedOther)
        .copied()
        .unwrap_or(0)
        + data
            .skipped
            .get(&aver::ir::mir::SkipReason::UnsupportedOther)
            .copied()
            .unwrap_or(0);
    assert_eq!(
        other_total, 0,
        "UnsupportedOther should never fire on the shipped corpus — \
         every drop must attribute to a named SkipReason. core: {:?}, data: {:?}",
        core.skipped, data.skipped
    );
}

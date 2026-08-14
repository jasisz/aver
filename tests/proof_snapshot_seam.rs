//! The proof snapshot seam: an optimising pass can never change what
//! gets proven.
//!
//! `buffer_build` (and the AST passes that will follow it) rewrites the
//! surface AST the proof exporters read. Keeping deforestation invisible
//! to Lean / Dafny used to be a CONVENTION — every proof-facing caller
//! had to remember `run_buffer_build: false`, and one forgotten `true`
//! silently changed the theorem statements. `pipeline::run` now
//! snapshots the AST before the first optimising pass and hands the
//! proof stages that snapshot, so the convention is a structure.
//!
//! The test runs ONE pipeline with fusion ON — the scenario the
//! convention could not survive: the same run produces a deforested AST
//! for the runtime backend AND a pristine proof view — and asserts the
//! emitted `.lean` / `.dfy` are byte-identical to a run with every
//! optimising pass off.

use aver::codegen::{CodegenContext, build_context};
use aver::ir::{PipelineConfig, TypecheckMode};
use aver::source::parse_source;

/// Canonical `String.join(<builder>(xs, []), sep)` sink — the shape
/// `buffer_build` fuses.
const FUSABLE: &str = include_str!("fixtures/proof_seam_fusable.av");

struct Exported {
    lean: String,
    dafny: String,
    /// Did the buffer-build pass actually fire in this run? Guards the
    /// vacuous pass where both sides are pristine because nothing fused.
    fused: bool,
    /// Did the post-pipeline (runtime-facing) AST carry the synthesized
    /// buffered variant? Same guard, read off the items a backend gets.
    items_carry_buffered: bool,
}

/// Export the fixture through the proof backends. `optimising` turns on
/// every AST-rewriting pass a runtime backend wants — exactly what a
/// proof-facing caller must NOT have to remember to turn off.
fn export(optimising: bool) -> Exported {
    let mut items = parse_source(FUSABLE).expect("fixture parses");
    let mut pipeline_result = aver::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full { base_dir: None }),
            run_interp_lower: optimising,
            run_buffer_build: optimising,
            run_escape: optimising,
            run_refinement_lower: true,
            run_contract_lower: true,
            run_law_lower: true,
            run_build_symbols: true,
            ..Default::default()
        },
    );
    let tc = pipeline_result
        .typecheck
        .take()
        .expect("typecheck requested");
    assert!(tc.errors.is_empty(), "fixture typechecks: {:?}", tc.errors);

    let fused = pipeline_result
        .buffer_build
        .as_ref()
        .is_some_and(|r| r.rewrites > 0 && !r.synthesized.is_empty());
    let items_carry_buffered = items.iter().any(|item| match item {
        aver::ast::TopLevel::FnDef(fd) => fd.name.contains("__buffered"),
        _ => false,
    });

    let proof_ir = pipeline_result.proof_ir.take();
    // What a proof-facing caller assembles its context from. With the
    // proof stages on, `codegen_view` hands back the AST as it stood
    // before the first optimising pass plus the facts derived from it
    // — never the `items` the runtime backend gets.
    let view = pipeline_result.codegen_view(items);
    let mut ctx: CodegenContext = build_context(
        view.items,
        &tc,
        view.analysis.as_ref(),
        "ProofSeamFusable".to_string(),
        vec![],
        view.symbol_table,
        view.resolved_items,
    );
    if let Some(ir) = proof_ir {
        ctx.proof_ir = ir;
    }

    let lean = aver::codegen::lean::transpile(&mut ctx)
        .files
        .iter()
        .map(|(path, body)| format!("== {path} ==\n{body}"))
        .collect::<Vec<_>>()
        .join("\n");
    let dafny = aver::codegen::dafny::transpile(&ctx)
        .files
        .iter()
        .map(|(path, body)| format!("== {path} ==\n{body}"))
        .collect::<Vec<_>>()
        .join("\n");
    Exported {
        lean,
        dafny,
        fused,
        items_carry_buffered,
    }
}

#[test]
fn fusion_cannot_change_what_gets_proven() {
    let pristine = export(false);
    let fused = export(true);

    // Non-vacuity: the optimising run really did deforest, in the same
    // pipeline run that produced the proof export.
    assert!(
        fused.fused && fused.items_carry_buffered,
        "fixture must fuse with the optimising passes on — otherwise this test proves nothing"
    );
    assert!(
        !pristine.fused,
        "the pristine run must not fuse — otherwise the two sides agree trivially"
    );

    assert_eq!(
        pristine.lean, fused.lean,
        "emitted Lean must not depend on whether the optimising passes ran"
    );
    assert_eq!(
        pristine.dafny, fused.dafny,
        "emitted Dafny must not depend on whether the optimising passes ran"
    );
    assert!(
        !fused.lean.contains("__buffered"),
        "the deforested shape leaked into the emitted Lean"
    );
    assert!(
        !fused.dafny.contains("__buffered"),
        "the deforested shape leaked into the emitted Dafny"
    );
}

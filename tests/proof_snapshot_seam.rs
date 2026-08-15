//! The proof-facing view: ONE program, described by the proof and
//! compiled into the certified artifact.
//!
//! Two rules meet here, and they pull in opposite directions.
//!
//! A proof must not describe a program that does not exist in source.
//! `buffer_build` replaces a `String.join(<builder>(…), sep)` pipeline
//! with a buffer loop and synthesizes a `<sink>__buffered` fn;
//! `interp_lower` replaces an interpolation with a `__buf_*` chain; the
//! planned chars-fusion will do the same to traversals. An exporter
//! reading the post-pass AST states its theorems about entities the
//! user never wrote. Those passes are BELOW the proof line.
//!
//! A certificate must not describe a different program from the one it
//! certifies. The artifact-certificate model is a trusted oracle over
//! the emitted bytes — the recursion classifier reads the step operator
//! off the MODEL because the bytes cannot tell the bignum helpers
//! apart — so a model built from a differently-optimised AST than the
//! bytes would attribute one program's facts to another's body.
//! `escape` — the narrow scalar-replace of a fresh record built at a
//! call site and immediately consumed (Aver has no general inliner) —
//! rewrites callers, so it is ABOVE the proof line: small,
//! semantics-preserving, ours, guarded by the differential, and seen by
//! both halves.
//!
//! The line therefore cuts by WHAT A PASS DOES, not by where it sits in
//! the stage order — and the two are not the same cut, because
//! `buffer_build` runs before `escape`. So `pipeline::run` snapshots the
//! AST before the first fabricating pass and completes the proof view on
//! that copy by re-running the above-the-line stages the caller asked
//! for. The invariant that falls out is the one this file tests:
//!
//! > the proof view is the program this same run would have compiled
//! > with the fabricating passes turned off.
//!
//! Which gives both rules at once: no synthesized entity can reach a
//! proof, and the certificate model is the artifact's own AST.

use aver::ast::TopLevel;
use aver::codegen::{CodegenContext, build_context};
use aver::ir::{PassReport, PipelineConfig, PipelineResult, TypecheckMode};
use aver::source::parse_source;

/// Canonical `String.join(<builder>(xs, []), sep)` sink — the shape
/// `buffer_build` fuses into a `__buffered` loop.
const FUSABLE: &str = include_str!("fixtures/proof_seam_fusable.av");

/// `manhattan(Point(x = n, y = n * 2))` — the shape `escape`
/// scalar-replaces into the caller.
const ESCAPABLE: &str = include_str!("fixtures/proof_seam_escapable.av");

/// `digitSum(String.chars(text), 0)` plus a match over single-character
/// literals — the two shapes `chars_fusion` rewrites.
const CHARS: &str = include_str!("fixtures/proof_seam_chars.av");

/// `doubled(values, [])` — a loop that collects with `List.prepend` and
/// reverses on the way out, the shape `list_build` turns into a builder.
const COLLECT: &str = include_str!("fixtures/proof_seam_collect.av");

/// What a caller asks the pipeline for. Named rather than inlined so a
/// test reads as the claim it makes.
#[derive(Clone, Copy)]
struct Flags {
    /// The passes that introduce entities the source does not contain:
    /// `interp_lower` (`__buf_*`, `__to_str`), `buffer_build`
    /// (`<sink>__buffered`, `Buffer`), `chars_fusion`
    /// (`<loop>__cursor`, `__str_*`) and `list_build`
    /// (`<loop>__collected`, `__lst_*`). Below the proof line.
    fabricating: bool,
    /// The scalar-replace pass. Above the proof line — see the module
    /// doc.
    escape: bool,
    /// Whether the proof-lowering stages run, i.e. whether this run
    /// produces a proof view at all.
    proof_stages: bool,
}

/// One pipeline run, with both of the ASTs it can hand out.
struct Run {
    /// What a runtime backend compiles — post every pass this run enabled.
    runtime_items: Vec<TopLevel>,
    /// What `aver proof` exports and what the artifact-certificate
    /// model is built from. `None` when no proof stage ran.
    proof_items: Option<Vec<TopLevel>>,
    lean: String,
    dafny: String,
    /// `buffer_build` fusion sites rewritten in the runtime half.
    fusion_rewrites: usize,
    /// `chars_fusion` producer sites moved onto a cursor, plus the
    /// single-character matches it turned into codepoint comparisons.
    chars_rewrites: usize,
    /// Did the runtime-facing AST end up carrying a synthesized
    /// `__buffered` sink? Read off the items a backend gets.
    runtime_carries_buffered: bool,
    /// Same question for the synthesized `__cursor` loop.
    runtime_carries_cursor: bool,
    /// `list_build` call sites moved onto a builder in the runtime half.
    list_rewrites: usize,
    /// Same question again for the synthesized `__collected` loop.
    runtime_carries_collected: bool,
    /// `escape` call sites rewritten in the runtime half.
    escape_rewrites: usize,
}

fn run(source: &str, project: &str, flags: Flags) -> Run {
    let mut items = parse_source(source).expect("fixture parses");
    let mut result = aver::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full { base_dir: None }),
            run_interp_lower: flags.fabricating,
            run_buffer_build: flags.fabricating,
            run_chars_fusion: flags.fabricating,
            run_list_build: flags.fabricating,
            run_escape: flags.escape,
            run_refinement_lower: flags.proof_stages,
            run_contract_lower: flags.proof_stages,
            run_law_lower: flags.proof_stages,
            run_build_symbols: true,
            ..Default::default()
        },
    );
    let tc = result.typecheck.take().expect("typecheck requested");
    assert!(tc.errors.is_empty(), "fixture typechecks: {:?}", tc.errors);

    let fusion_rewrites = result.buffer_build.as_ref().map_or(0, |r| r.rewrites);
    let chars_rewrites = result
        .chars_fusion
        .as_ref()
        .map_or(0, |r| r.cursor_rewrites + r.codepoint_matches);
    let carries = |suffix: &str| {
        items.iter().any(|item| match item {
            TopLevel::FnDef(fd) => fd.name.contains(suffix),
            _ => false,
        })
    };
    let list_rewrites = result.list_build.as_ref().map_or(0, |r| r.rewrites);
    let runtime_carries_buffered = carries("__buffered");
    let runtime_carries_cursor = carries("__cursor");
    let runtime_carries_collected = carries("__collected");
    let escape_rewrites = escape_rewrites(&result);
    let proof_ir = result.proof_ir.take();
    let runtime_items = items.clone();

    // Every proof-facing caller assembles its context from the same
    // accessor — `aver proof`, the certificate model, the playground,
    // these tests. There is no second way to reach an AST from here.
    let view = result.codegen_view(items);
    let proof_items = flags.proof_stages.then(|| view.items.clone());
    let mut ctx: CodegenContext = build_context(
        view.items,
        &tc,
        view.analysis.as_ref(),
        project.to_string(),
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
    Run {
        runtime_items,
        proof_items,
        lean,
        dafny,
        fusion_rewrites,
        chars_rewrites,
        runtime_carries_buffered,
        runtime_carries_cursor,
        list_rewrites,
        runtime_carries_collected,
        escape_rewrites,
    }
}

fn escape_rewrites(result: &PipelineResult) -> usize {
    result
        .pass_diagnostics
        .iter()
        .find_map(|d| match d.report {
            PassReport::Escape { rewrites } => Some(rewrites),
            _ => None,
        })
        .unwrap_or(0)
}

/// Every pass exists to serve the program the user wrote, so nothing it
/// invents may reach a proof. This compiler names everything it
/// synthesizes with a leading `__` (`__buffered`, `__buf_new`,
/// `__to_str`), and no Aver source in this repo uses one — so a `__`
/// anywhere in the view's own Debug rendering means a fabricated entity
/// got through, wherever it sits in the AST. Backstop to the equality
/// tests below: those pin the view against a REFERENCE RUN, which a new
/// fabricating pass with a default-on flag would move too.
fn assert_no_synthesized_entity(items: &[TopLevel], what: &str) {
    let rendered = format!("{items:?}");
    assert!(
        !rendered.contains("__"),
        "{what} carries a compiler-synthesized entity — a proof about it \
         would be a proof about a program that does not exist in source"
    );
}

/// THE invariant. `aver proof` and the artifact-certificate model both
/// read this view, and the certified bytes are compiled from an AST
/// that ran the same above-the-line passes — so the two agree by
/// construction rather than by two callers happening to pass matching
/// flags.
///
/// `reference` mirrors what `cmd_compile_wasm_gc` compiles on the
/// `--certify` path (`run_interp_lower: false`, `run_buffer_build:
/// false`, every other pass at its default) and `model` mirrors the
/// `build_codegen_context` call `emit_artifact_certificate` makes for
/// the model, plus — for the fusable fixture — the fabricating passes a
/// VM/Rust build would additionally run.
fn assert_proof_view_is_the_unfabricated_program(source: &str, project: &str, fabricating: bool) {
    let model = run(
        source,
        project,
        Flags {
            fabricating,
            escape: true,
            proof_stages: true,
        },
    );
    let artifact = run(
        source,
        project,
        Flags {
            fabricating: false,
            escape: true,
            proof_stages: false,
        },
    );
    let proof_items = model.proof_items.expect("proof stages ran");
    assert_eq!(
        proof_items, artifact.runtime_items,
        "the proof view must be the program this run would have compiled \
         with the fabricating passes off — otherwise a certificate's model \
         and its bytes describe two different programs"
    );
    assert!(
        !model.lean.is_empty() && !model.dafny.is_empty(),
        "the export must be non-empty — an empty one satisfies every equality here"
    );
    assert_no_synthesized_entity(&proof_items, "the proof view");
}

#[test]
fn proof_view_is_what_the_certified_artifact_was_compiled_from() {
    // The `--certify` shape: the model run and the artifact run pass
    // the same flags, and `escape` rewrites the program under both.
    assert_proof_view_is_the_unfabricated_program(
        ESCAPABLE,
        "ProofSeamEscapable",
        /* fabricating */ false,
    );

    // Non-vacuity: `escape` must actually fire on this fixture, and the
    // view must actually differ from the snapshot the pipeline takes —
    // otherwise the equality above holds for a program nothing rewrote.
    let model = run(
        ESCAPABLE,
        "ProofSeamEscapable",
        Flags {
            fabricating: false,
            escape: true,
            proof_stages: true,
        },
    );
    assert!(
        model.escape_rewrites > 0,
        "fixture must scalar-replace — otherwise this test proves nothing"
    );
    let unescaped = run(
        ESCAPABLE,
        "ProofSeamEscapable",
        Flags {
            fabricating: false,
            escape: false,
            proof_stages: true,
        },
    );
    assert_ne!(
        model.proof_items.expect("proof stages ran"),
        unescaped.proof_items.expect("proof stages ran"),
        "the proof view must be the post-escape program, not the snapshot \
         the pipeline took before it"
    );
    assert_ne!(
        model.lean, unescaped.lean,
        "scalar replacement must be visible in the emitted proof — the \
         certificate's model is a claim about the artifact's own body"
    );
}

#[test]
fn fusion_cannot_change_what_gets_proven() {
    // The two runs differ in the fabricating passes and NOTHING else:
    // every other stage is at its default on both sides, so what this
    // measures is deforestation, not a pile of flags.
    let pristine = run(
        FUSABLE,
        "ProofSeamFusable",
        Flags {
            fabricating: false,
            escape: true,
            proof_stages: true,
        },
    );
    let fused = run(
        FUSABLE,
        "ProofSeamFusable",
        Flags {
            fabricating: true,
            escape: true,
            proof_stages: true,
        },
    );

    // Non-vacuity: the optimising run really did deforest, in the same
    // pipeline run that produced the proof export.
    assert!(
        fused.fusion_rewrites > 0 && fused.runtime_carries_buffered,
        "fixture must fuse with the fabricating passes on — otherwise this test proves nothing"
    );
    assert!(
        pristine.fusion_rewrites == 0 && !pristine.runtime_carries_buffered,
        "the pristine run must not fuse — otherwise the two sides agree trivially"
    );
    assert!(
        !pristine.lean.is_empty() && !pristine.dafny.is_empty(),
        "the export must be non-empty — an empty one satisfies every equality here"
    );

    assert_eq!(
        pristine.lean, fused.lean,
        "emitted Lean must not depend on whether the fabricating passes ran"
    );
    assert_eq!(
        pristine.dafny, fused.dafny,
        "emitted Dafny must not depend on whether the fabricating passes ran"
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

/// The same AST invariant as the certificate half, on the fixture that
/// fuses: with the fabricating passes ON, the proof view is still the
/// program a fabrication-free build compiles. Separate from the emitted
/// -text test above so a regression reports both facts, not the first
/// one to trip.
#[test]
fn proof_view_under_fusion_is_the_unfused_program() {
    assert_proof_view_is_the_unfabricated_program(
        FUSABLE,
        "ProofSeamFusable",
        /* fabricating */ true,
    );
}

/// The chars-fusion half of the same invariant. A cursor loop and a
/// codepoint comparison are entities the source does not contain — a
/// theorem about `parseHexChars__cursor` is a theorem about a function
/// nobody wrote, and `48` is not the character `"0"` the user matched
/// on. Registered below the snapshot, so neither can reach an exporter.
#[test]
fn chars_fusion_cannot_change_what_gets_proven() {
    let pristine = run(
        CHARS,
        "ProofSeamChars",
        Flags {
            fabricating: false,
            escape: true,
            proof_stages: true,
        },
    );
    let fused = run(
        CHARS,
        "ProofSeamChars",
        Flags {
            fabricating: true,
            escape: true,
            proof_stages: true,
        },
    );

    // Non-vacuity: BOTH halves of the pass have to have fired in the
    // same run that produced the proof export.
    assert!(
        fused.chars_rewrites > 1 && fused.runtime_carries_cursor,
        "fixture must fuse its traversal AND its character match with the \
         fabricating passes on — otherwise this test proves nothing"
    );
    assert!(
        pristine.chars_rewrites == 0 && !pristine.runtime_carries_cursor,
        "the pristine run must not fuse — otherwise the two sides agree trivially"
    );
    assert!(
        !pristine.lean.is_empty() && !pristine.dafny.is_empty(),
        "the export must be non-empty — an empty one satisfies every equality here"
    );

    assert_eq!(
        pristine.lean, fused.lean,
        "emitted Lean must not depend on whether the fabricating passes ran"
    );
    assert_eq!(
        pristine.dafny, fused.dafny,
        "emitted Dafny must not depend on whether the fabricating passes ran"
    );
    for (label, emitted) in [("Lean", &fused.lean), ("Dafny", &fused.dafny)] {
        assert!(
            !emitted.contains("__cursor") && !emitted.contains("__str_"),
            "the cursor shape leaked into the emitted {label}"
        );
    }
}

/// The AST invariant on the same fixture, separate from the emitted
/// text so a regression reports both facts rather than the first to
/// trip.
#[test]
fn proof_view_under_chars_fusion_is_the_unfused_program() {
    assert_proof_view_is_the_unfabricated_program(
        CHARS,
        "ProofSeamChars",
        /* fabricating */ true,
    );
}

/// The list-build half of the same invariant. A `<loop>__collected`
/// variant is a function nobody wrote, and `__lst_push(acc, x)` is not
/// the `List.prepend(x, acc)` the user wrote — a theorem about either is
/// a theorem about a program that does not exist in source. Registered
/// below the snapshot, so neither can reach an exporter.
#[test]
fn list_build_cannot_change_what_gets_proven() {
    let pristine = run(
        COLLECT,
        "ProofSeamCollect",
        Flags {
            fabricating: false,
            escape: true,
            proof_stages: true,
        },
    );
    let fused = run(
        COLLECT,
        "ProofSeamCollect",
        Flags {
            fabricating: true,
            escape: true,
            proof_stages: true,
        },
    );

    // Non-vacuity: the pass has to have fired in the same run that
    // produced the proof export.
    assert!(
        fused.list_rewrites > 0 && fused.runtime_carries_collected,
        "fixture must collect into a builder with the fabricating passes on \
         — otherwise this test proves nothing"
    );
    assert!(
        pristine.list_rewrites == 0 && !pristine.runtime_carries_collected,
        "the pristine run must not fuse — otherwise the two sides agree trivially"
    );
    assert!(
        !pristine.lean.is_empty() && !pristine.dafny.is_empty(),
        "the export must be non-empty — an empty one satisfies every equality here"
    );

    assert_eq!(
        pristine.lean, fused.lean,
        "emitted Lean must not depend on whether the fabricating passes ran"
    );
    assert_eq!(
        pristine.dafny, fused.dafny,
        "emitted Dafny must not depend on whether the fabricating passes ran"
    );
    for (label, emitted) in [("Lean", &fused.lean), ("Dafny", &fused.dafny)] {
        assert!(
            !emitted.contains("__collected") && !emitted.contains("__lst_"),
            "the builder shape leaked into the emitted {label}"
        );
    }
}

/// The AST invariant on the same fixture, separate from the emitted
/// text so a regression reports both facts rather than the first to
/// trip.
#[test]
fn proof_view_under_list_build_is_the_unfused_program() {
    assert_proof_view_is_the_unfabricated_program(
        COLLECT,
        "ProofSeamCollect",
        /* fabricating */ true,
    );
}

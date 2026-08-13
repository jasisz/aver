//! Map iteration order is one thing across the toolchain.
//!
//! A map inserted `z`, `a`, `m` iterates key-sorted. `aver verify` runs it on
//! the VM, `aver proof` exports it to Lean, and the two used to disagree: the
//! runtime sorted by key while the exported model kept insertion order, so a
//! claim about `Map.values` that the VM refuted was a theorem the Lean kernel
//! accepted. Both sides read the same order now, and these tests hold them
//! there.
//!
//! Coverage the divergence survived behind: `Map.values` had a single hit in
//! the whole tests tree and none in the cross-backend suites, so nothing ever
//! compared the value sequence across two backends or across two runs.
#![cfg(feature = "runtime")]

use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

const ORDER_FIXTURE: &str = "tests/fixtures/map_iteration_order.av";
const UNMODELLED_FIXTURE: &str = "tests/fixtures/map_order_unmodelled_keys.av";
const NAN_KEY_FIXTURE: &str = "tests/fixtures/map_float_nan_keys.av";
const MODEL_SHAPE_FIXTURE: &str = "tests/fixtures/map_model_shape.av";

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn temp_output_dir(prefix: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    std::env::temp_dir().join(format!("{prefix}-{nanos}"))
}

fn format_output(out: &std::process::Output) -> String {
    format!(
        "status: {}\nstdout:\n{}\nstderr:\n{}",
        out.status,
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
    )
}

fn run_aver(args: &[&str]) -> std::process::Output {
    Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(repo_root())
        .args(args)
        .output()
        .expect("expected the `aver` binary to run")
}

fn lake_available() -> bool {
    Command::new("lake").arg("--version").output().is_ok()
}

/// Generate a Lean project for `fixture` and return the emitted module text.
fn emit_lean(fixture: &str, prefix: &str, module: &str) -> String {
    let out_dir = temp_output_dir(prefix);
    let run = run_aver(&[
        "proof",
        fixture,
        "-o",
        out_dir.to_str().expect("utf-8 temp path"),
    ]);
    assert!(
        run.status.success(),
        "`aver proof {}` failed:\n{}",
        fixture,
        format_output(&run)
    );
    let text = std::fs::read_to_string(out_dir.join(format!("{module}.lean")))
        .expect("expected the generated Lean module to exist");
    let _ = std::fs::remove_dir_all(&out_dir);
    text
}

/// The three-key repro: `aver verify` runs it, and it passes.
#[test]
fn three_key_map_iterates_key_sorted_on_the_vm() {
    let run = run_aver(&["verify", ORDER_FIXTURE]);
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert!(
        run.status.success() && stdout.contains("0 failed"),
        "the map order fixture must verify clean on the VM:\n{}",
        format_output(&run)
    );
}

/// Iterating a float-keyed map holding a NaN returns, on the path `aver run`
/// and `aver verify` take.
///
/// Making the key comparator the shared one routed the NaN-boxed
/// representation through it, and it was not a total order: a NaN compared
/// above `1.0` and below `-1.0` by raw bit pattern while `-1.0 < 1.0`. The
/// standard library detects the cycle on an input this wide and aborts the
/// process — `user-provided comparison function does not correctly implement
/// a total order`, from `sort_keys_nv` — so this is a crash on a live path
/// rather than a wrong answer. `Map.keys`, `Map.values` and `Map.entries` all
/// sort, so all three are read here.
#[test]
fn iterating_a_float_map_holding_nan_completes_on_the_vm() {
    let run = run_aver(&["verify", NAN_KEY_FIXTURE]);
    let stdout = String::from_utf8_lossy(&run.stdout);
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert!(
        !stderr.contains("does not correctly implement a total order"),
        "ordering float keys must be a total order — the sort aborted:\n{}",
        format_output(&run)
    );
    assert!(
        run.status.success() && stdout.contains("0 failed"),
        "reading a float map that holds a NaN must complete and count every key:\n{}",
        format_output(&run)
    );

    // Same map, read twice: the order a map iterates in is a function of its
    // keys, not of which run is reading them.
    let first = run_aver(&["run", NAN_KEY_FIXTURE]);
    let second = run_aver(&["run", NAN_KEY_FIXTURE]);
    assert!(
        first.status.success(),
        "running the float-key fixture must succeed:\n{}",
        format_output(&first)
    );
    assert_eq!(
        String::from_utf8_lossy(&first.stdout),
        String::from_utf8_lossy(&second.stdout),
        "two runs over the same float map must read the same sequence"
    );
}

/// The same three-key repro through `aver proof`: the exported Lean says the
/// same thing the VM does, and the kernel accepts it.
///
/// This is the assertion the bug broke. Before the model was made key-sorted,
/// `lake build` reported `Tactic 'decide' proved that the proposition
/// AverMap.values threeKeys = [2, 3, 1] is false` — the exact claim `aver
/// verify` passes above.
#[test]
fn three_key_map_iterates_key_sorted_in_the_exported_proof() {
    let lean = emit_lean(ORDER_FIXTURE, "aver-map-order-lean", "MapIterationOrder");
    for claim in [
        "AverMap.keys threeKeys = [\"a\", \"m\", \"z\"]",
        "AverMap.values threeKeys = [2, 3, 1]",
        "AverMap.entries threeKeys = [(\"a\", 2), (\"m\", 3), (\"z\", 1)]",
        // A literal written out of key order is emitted in key order, so it is
        // the same value as the map built by three `Map.set` calls.
        "AverMap.values literalOutOfOrder = [2, 3, 1]",
        // Integer keys order numerically, not by printed digits.
        "AverMap.keys numericKeys = [2, 10]",
    ] {
        assert!(
            lean.contains(claim),
            "expected the exported proof to state `{claim}`:\n{lean}"
        );
    }

    if !lake_available() {
        eprintln!("skipping kernel check: `lake` not available");
        return;
    }
    let out_dir = temp_output_dir("aver-map-order-check");
    let run = run_aver(&[
        "proof",
        ORDER_FIXTURE,
        "-o",
        out_dir.to_str().expect("utf-8 temp path"),
        "--check",
        "--check-json",
        "--sorry-budget",
        "0",
    ]);
    let summary = last_json(&run);
    assert_eq!(
        summary["build_errors"].as_u64(),
        Some(0),
        "the exported proof must build: a non-zero error count means the kernel \
         rejected a claim the VM accepts\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(0),
        "the exported proof must close without a `sorry`\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["passed"].as_bool(),
        Some(true),
        "`aver proof --check` must pass on the map order fixture\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&out_dir);
}

fn last_json(run: &std::process::Output) -> serde_json::Value {
    let line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with('{')))
        .unwrap_or_else(|| panic!("no JSON summary line:\n{}", format_output(run)));
    serde_json::from_str(line).expect("summary line parses as JSON")
}

/// A law reading iteration order over a key type the model cannot order the
/// way the runtime does is refused, and the refusal says why.
#[test]
fn iteration_order_law_over_unmodelled_key_types_is_refused() {
    let lean = emit_lean(
        UNMODELLED_FIXTURE,
        "aver-map-order-refuse-lean",
        "MapOrderUnmodelledKeys",
    );
    assert!(
        !lean.contains("theorem floatKeyedValues_law_valuesFollowIterationOrder"),
        "a Float-keyed iteration-order law must NOT be exported as a theorem:\n{lean}"
    );
    assert!(
        lean.contains(
            "-- verify law floatKeyedValues.valuesFollowIterationOrder: map iteration order \
             is not exported — the runtime orders Float keys by IEEE 754 total order, which \
             puts a NaN outside the finite range, and the proof model has no faithful \
             counterpart for that"
        ),
        "the Float refusal must name its reason:\n{lean}"
    );
    assert!(
        !lean.contains("theorem tupleKeyedKeys_law_keysFollowIterationOrder"),
        "a Tuple-keyed iteration-order law must NOT be exported as a theorem:\n{lean}"
    );
    assert!(
        lean.contains(
            "-- verify law tupleKeyedKeys.keysFollowIterationOrder: map iteration order is not \
             exported — the runtime orders Tuple<Int, Int> keys by their printed representation, \
             which the proof model cannot reconstruct from the value"
        ),
        "the non-scalar-key refusal must name its reason:\n{lean}"
    );
}

/// The gate runs on a plain sampled case, not only on the law form.
///
/// `verify plainFloatKeys: plainFloatKeys() => [1.0, 2.0]` is the commonest
/// way to write this down, and it was exported ungated as
/// `example : plainFloatKeys = [2.0, 1.0] := by native_decide` — the literal
/// in written order, which is not what the map iterates. `aver verify` said
/// `1 failed` on the same source that `aver proof --check` reported `passed`.
#[test]
fn a_plain_verify_case_reading_iteration_order_is_refused() {
    let lean = emit_lean(
        UNMODELLED_FIXTURE,
        "aver-map-order-plain-case-lean",
        "MapOrderUnmodelledKeys",
    );
    assert!(
        !lean.contains("example : plainFloatKeys ="),
        "a plain verify case over a Float-keyed map must NOT be exported as an \
         example:\n{lean}"
    );
    assert!(
        lean.contains("-- verify plainFloatKeys: map iteration order is not exported"),
        "the plain-case refusal must say why it declined:\n{lean}"
    );
}

/// One helper is enough to hide the observation, and it must not be.
///
/// The observer test read `law.lhs`, `law.rhs` and `law.when` syntactically and
/// returned before any key type was examined, while key types were already
/// collected across the callee cone. So `firstFloatValue(m) => 3`, whose body
/// calls `hiddenFloatValues` whose body calls `Map.values`, named no observer
/// and exported as a theorem about an order the model does not reproduce.
#[test]
fn an_observer_behind_a_helper_is_still_refused() {
    let lean = emit_lean(
        UNMODELLED_FIXTURE,
        "aver-map-order-hidden-observer-lean",
        "MapOrderUnmodelledKeys",
    );
    assert!(
        !lean.contains("theorem firstFloatValue_law_firstIsLowestKeysValue"),
        "an iteration-order law reaching its observer through a helper must NOT \
         be exported as a theorem:\n{lean}"
    );
    assert!(
        lean.contains(
            "-- verify law firstFloatValue.firstIsLowestKeysValue: map iteration order is \
             not exported"
        ),
        "the refusal must fire on the law that hides its observer:\n{lean}"
    );
}

/// A map whose key type is only visible through a user type still exports.
///
/// Running the observer test over the callee cone made the fail-closed
/// "key type not visible" branch reachable, and it fired on sixteen laws in
/// `examples/data/json.av` — a `String`-keyed map named nowhere in a signature
/// because it sits in a variant payload, `JsonObject(Map<String, Json>)`. Key
/// types are read through user type definitions for that reason, so the
/// refusal stays pointed at key types the model really cannot order.
#[test]
fn a_key_type_reached_through_a_user_type_is_not_refused() {
    let lean = emit_lean("examples/data/json.av", "aver-map-order-json-lean", "Json");
    assert!(
        !lean.contains("map iteration order is not exported"),
        "the JSON example's maps are String-keyed and every one of its laws must \
         still export:\n{}",
        lean.lines()
            .filter(|l| l.contains("map iteration order"))
            .collect::<Vec<_>>()
            .join("\n")
    );
}

/// The refusal is not blanket. Order-blind map laws still export — including
/// over the very key types whose iteration order is refused.
#[test]
fn order_blind_map_laws_still_export() {
    let ordered = emit_lean(
        ORDER_FIXTURE,
        "aver-map-order-blind-lean",
        "MapIterationOrder",
    );
    assert!(
        ordered.contains("theorem setThenLen_law_neverEmptyAfterSet : ∀"),
        "a `Map.len` law must still export as a universal theorem:\n{ordered}"
    );
    assert!(
        ordered.contains("theorem setThenGet_law_readsBackWhatWasWritten : ∀"),
        "a `Map.get` law must still export as a universal theorem:\n{ordered}"
    );
    assert!(
        !ordered.contains("map iteration order is not exported"),
        "no law in the map order fixture reads an unmodelled key type, so nothing \
         in it may be refused:\n{ordered}"
    );

    let unmodelled = emit_lean(
        UNMODELLED_FIXTURE,
        "aver-map-order-blind-float-lean",
        "MapOrderUnmodelledKeys",
    );
    assert!(
        unmodelled.contains("theorem floatKeyedSize_law_neverEmptyAfterSet : ∀"),
        "a `Map.len` law over a Float-keyed map is order-blind and must still \
         export:\n{unmodelled}"
    );
}

/// The Dafny backend refuses the same laws the Lean backend refuses. Dafny's
/// `MapEntries` is declared with no body, so it commits to no iteration order
/// and would let an order claim through by saying nothing about it.
#[test]
fn dafny_refuses_the_same_iteration_order_laws() {
    let out_dir = temp_output_dir("aver-map-order-dafny");
    let run = run_aver(&[
        "proof",
        UNMODELLED_FIXTURE,
        "--backend",
        "dafny",
        "-o",
        out_dir.to_str().expect("utf-8 temp path"),
    ]);
    assert!(
        run.status.success(),
        "`aver proof --backend dafny` failed:\n{}",
        format_output(&run)
    );
    let dfy = std::fs::read_to_string(out_dir.join("MapOrderUnmodelledKeys.dfy"))
        .expect("expected the generated Dafny file to exist");
    let _ = std::fs::remove_dir_all(&out_dir);
    assert!(
        dfy.contains(
            "// Law floatKeyedValues.valuesFollowIterationOrder: map iteration order is not \
             exported"
        ),
        "Dafny must mirror the Lean refusal or the two backends disagree on the \
         same source:\n{dfy}"
    );
    assert!(
        dfy.contains(
            "// Law tupleKeyedKeys.keysFollowIterationOrder: map iteration order is not exported"
        ),
        "Dafny must mirror the Lean refusal for non-scalar keys:\n{dfy}"
    );

    // Plain sampled cases have a gate on the Lean side but nothing to mirror
    // here: `dafny::emit_verify_blocks` only ever walks `VerifyKind::Law`, so a
    // `verify` block without a law contributes the function definition and no
    // claim at all. Pin that, because the day Dafny starts emitting sampled
    // cases it needs the gate the Lean emitter has.
    assert!(
        !dfy.contains("plainFloatKeys()) =="),
        "Dafny emits no claim for a plain verify case today; if that changed, \
         `verify_case_map_order_refusal` has to be wired in here too:\n{dfy}"
    );
    assert!(
        dfy.contains("function plainFloatKeys()"),
        "the plain-case function itself is still emitted — this test would pass \
         vacuously if the fixture stopped reaching Dafny:\n{dfy}"
    );
}

/// The compiled Rust backend emits a key-sorted `Map.values`.
///
/// It used to walk `HashMap::values()` directly while its two neighbours
/// sorted, so the order varied per process and `keys[i]` did not pair with
/// `values[i]`. Running the same program twice through the self-host — which
/// IS compiled Rust, regenerated through the normal path — must give the same
/// answer both times, and the same answer as the VM.
#[test]
fn compiled_rust_map_values_is_key_sorted_and_stable_across_runs() {
    let vm = run_aver(&["run", ORDER_FIXTURE]);
    assert!(
        vm.status.success(),
        "`aver run` failed:\n{}",
        format_output(&vm)
    );
    let vm_out = String::from_utf8_lossy(&vm.stdout).trim().to_string();
    assert_eq!(
        vm_out, "2,3,1",
        "the VM must print the three-key map's values in key order"
    );

    let module_root = repo_root().join("tests/fixtures");
    let first = run_aver(&[
        "run",
        ORDER_FIXTURE,
        "--module-root",
        module_root.to_str().expect("utf-8 module root"),
        "--self-host",
    ]);
    assert!(
        first.status.success(),
        "`aver run --self-host` failed:\n{}",
        format_output(&first)
    );
    let second = run_aver(&[
        "run",
        ORDER_FIXTURE,
        "--module-root",
        module_root.to_str().expect("utf-8 module root"),
        "--self-host",
    ]);
    assert!(
        second.status.success(),
        "`aver run --self-host` failed on the second run:\n{}",
        format_output(&second)
    );
    let first_out = String::from_utf8_lossy(&first.stdout).trim().to_string();
    let second_out = String::from_utf8_lossy(&second.stdout).trim().to_string();
    assert_eq!(
        first_out, second_out,
        "two runs of the same compiled binary must print the same value sequence"
    );
    assert_eq!(
        first_out, vm_out,
        "compiled Rust and the VM must iterate a map the same way"
    );
}

/// The emitted map model is pinned verbatim.
///
/// Making a map iterate one way everywhere moved four separate pieces of
/// emitted proof text: an `AverKeyOrder` class with one instance per modelled
/// key type, `AverMap.set` doing sorted insertion instead of append,
/// `AverMap.fromList` becoming a fold over `set` instead of the identity, and
/// scalar map literals being emitted in key order rather than written order.
/// A regression in any of them changes what the kernel is asked to prove
/// without changing what the source claims, and nothing else in the suite
/// would notice — the laws would still close, about the wrong sequence.
///
/// `tests/fixtures/map_model_shape.av` exercises all four in one file and
/// `aver verify` passes on it, so the pinned text is text the VM agrees with.
/// Refresh with `INSTA_UPDATE=always` (or `cargo insta review`) after an
/// intended model change, and read the diff — a reordered literal or a `set`
/// that stopped sorting is the regression this exists to catch.
#[test]
fn the_exported_map_model_is_pinned() {
    let out_dir = temp_output_dir("aver-map-model-shape");
    let run = run_aver(&[
        "proof",
        MODEL_SHAPE_FIXTURE,
        "-o",
        out_dir.to_str().expect("utf-8 temp path"),
    ]);
    assert!(
        run.status.success(),
        "`aver proof {MODEL_SHAPE_FIXTURE}` failed:\n{}",
        format_output(&run)
    );
    let common =
        std::fs::read_to_string(out_dir.join("AverCommon.lean")).expect("AverCommon.lean exists");
    let module = std::fs::read_to_string(out_dir.join("MapModelShape.lean"))
        .expect("MapModelShape.lean exists");
    let _ = std::fs::remove_dir_all(&out_dir);

    // Only the map section of the shared prelude: the rest of AverCommon moves
    // for reasons that have nothing to do with map order, and a snapshot that
    // churns on every unrelated change stops being read.
    let start = common
        .find("class AverKeyOrder")
        .expect("the key-ordering class must be emitted");
    let end = common[start..]
        .find("\nend AverMap")
        .map(|i| start + i + "\nend AverMap".len())
        .expect("the map namespace must be closed");
    let map_prelude = &common[start..end];

    insta::assert_snapshot!(
        "map_model_emitted_text",
        format!(
            "== AverCommon.lean (map section) ==\n{map_prelude}\n== MapModelShape.lean ==\n{module}"
        )
    );
}

/// Every exported artifact carries the wasm-gc carve-out.
///
/// The trust header used to end "Exported proofs hold uniformly across Aver
/// backends (VM, compiled Rust, WASM)" with no qualification, in a build where
/// wasm-gc returns hash-bucket order for map iteration — so the artifact
/// asserted backend uniformity that the recommended compile target falsifies.
/// The header states the carve-out the way it already stated the `Int`
/// overflow one.
#[test]
fn the_trust_header_carves_out_map_order_on_wasm_gc() {
    let lean = emit_lean(ORDER_FIXTURE, "aver-map-order-header", "MapIterationOrder");
    assert!(
        lean.contains("Map iteration order:"),
        "the trust header must have a map-order section:\n{lean}"
    );
    assert!(
        lean.contains("wasm-gc backend returns hash-bucket"),
        "the carve-out must name the backend that diverges:\n{lean}"
    );
    assert!(
        !lean.contains("Rust, WASM). No concurrency primitives"),
        "the unqualified backend-uniformity sentence must not survive:\n{lean}"
    );
    assert!(
        lean.contains("map iteration order on wasm-gc"),
        "the backend-independence claim must point at the carve-out:\n{lean}"
    );
}

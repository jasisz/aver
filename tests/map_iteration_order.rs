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
             is not exported — the runtime orders Float keys by their raw bit pattern once a \
             NaN is involved, and the proof model has no faithful counterpart for that"
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

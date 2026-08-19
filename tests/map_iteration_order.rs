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
//!
//! What the ordered-key rule changed here. A `Map` key must now have an
//! ordering — `Int`, `String` or `Bool` — so the key types the model could not
//! reproduce (`Float`, tuples, records, lists, handles) can no longer be
//! written down at all, and `tests/ordered_map_key_ban.rs` holds the refusals
//! that replaced them. The exporter's own gate stays as a fail-closed
//! backstop, and two of its branches are still reachable from a program that
//! typechecks:
//!
//! - the key type is NOT VISIBLE — every map in the claim's cone is built
//!   inside a body and no signature it reaches mentions one, so the exporter
//!   cannot see what the maps are keyed on and declines rather than assume;
//! - map equality over an UNRESOLVED key — an empty map literal has not had
//!   its key type decided, so a comparison against one is declined too.
//!
//! Both are conservative: the model reproduces the runtime's order for every
//! key type the language still admits, so a refusal here is a lost export
//! rather than a dodged unsound theorem. The mechanism under test is
//! unchanged — the cone walk that finds the observer, and the name resolution
//! that finds the key type — and the fixtures below drive it through the
//! invisible-key shape.
#![cfg(feature = "runtime")]

use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

const ORDER_FIXTURE: &str = "tests/fixtures/map_iteration_order.av";
const INVISIBLE_KEY_FIXTURE: &str = "tests/fixtures/map_order_invisible_key.av";
const MODEL_SHAPE_FIXTURE: &str = "tests/fixtures/map_model_shape.av";
const HIDDEN_OBSERVER_FIXTURE: &str = "tests/fixtures/map_order_hidden_observer.av";
const EQUALITY_FIXTURE: &str = "tests/fixtures/map_equality_unresolved_key.av";
const ADT_CONTROL_FIXTURE: &str = "tests/fixtures/map_equality_adt_control.av";
const COLLIDING_TYPE_DIR: &str = "tests/fixtures/map_order_colliding_type";
const CROSS_MODULE_DIR: &str = "tests/fixtures/map_order_cross_module";
const NAME_COLLISION_DIR: &str = "tests/fixtures/map_order_name_collision";
const TYPE_MEMO_DIR: &str = "tests/fixtures/map_order_type_memo";
const FIELD_COLLISION_DIR: &str = "tests/fixtures/map_order_field_collision";
const FN_PARAM_FIXTURE: &str = "tests/fixtures/map_order_fn_param.av";
const SHADOWED_GIVEN_FIXTURE: &str = "tests/fixtures/map_order_shadowed_given.av";
const READER_NAMED_GIVEN_FIXTURE: &str = "tests/fixtures/map_order_given_named_after_reader.av";

/// The one sentence every invisible-key refusal ends with. Asserting on the
/// marker alone would pass on a refusal that had changed its mind about why.
const INVISIBLE_KEY_REASON: &str = concat!(
    "the map's key type is not visible from the givens or from any signature ",
    "it reaches, and the proof model only reproduces the runtime's iteration ",
    "order for Int, String and Bool keys"
);

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

/// A law reading iteration order over a map whose key type the exporter cannot
/// see is refused, and the refusal says why.
///
/// The key types the model could not order are gone from the language, so what
/// is left to refuse is the map the exporter cannot READ the key of: every map
/// in this fixture is built inside a body, and no signature the claim reaches
/// mentions one. Both observers are exercised — `Map.values` in one law,
/// `Map.keys` in the other — because the gate is keyed on which builtin the
/// cone calls.
#[test]
fn iteration_order_law_over_an_invisible_key_is_refused() {
    let lean = emit_lean(
        INVISIBLE_KEY_FIXTURE,
        "aver-map-order-refuse-lean",
        "MapOrderInvisibleKey",
    );
    assert!(
        !lean.contains("theorem iteratedValues_law_lowestKeyLeadsTheValues"),
        "a `Map.values` iteration-order law over an unreadable key must NOT be \
         exported as a theorem:\n{lean}"
    );
    assert!(
        lean.contains(&format!(
            "-- verify law iteratedValues.lowestKeyLeadsTheValues: map iteration order is not \
             exported — {INVISIBLE_KEY_REASON}"
        )),
        "the refusal must name its reason:\n{lean}"
    );
    assert!(
        !lean.contains("theorem iteratedKeys_law_lowestKeyLeadsTheKeys"),
        "a `Map.keys` iteration-order law over an unreadable key must NOT be \
         exported as a theorem:\n{lean}"
    );
    assert!(
        lean.contains(&format!(
            "-- verify law iteratedKeys.lowestKeyLeadsTheKeys: map iteration order is not \
             exported — {INVISIBLE_KEY_REASON}"
        )),
        "the `Map.keys` refusal must name the same reason:\n{lean}"
    );
}

/// The gate runs on a plain sampled case, not only on the law form.
///
/// `verify plainEntries: plainEntries() => [("a", 2), ("b", 1)]` is the
/// commonest way to write this down, and it was exported ungated as an
/// `example` stating the literal in written order, which is not what the map
/// iterates. `aver verify` said `1 failed` on the same source that `aver proof
/// --check` reported `passed`.
#[test]
fn a_plain_verify_case_reading_iteration_order_is_refused() {
    let lean = emit_lean(
        INVISIBLE_KEY_FIXTURE,
        "aver-map-order-plain-case-lean",
        "MapOrderInvisibleKey",
    );
    assert!(
        !lean.contains("example : plainEntries ="),
        "a plain verify case reading `Map.entries` must NOT be exported as an \
         example:\n{lean}"
    );
    assert!(
        lean.contains("-- verify plainEntries: map iteration order is not exported"),
        "the plain-case refusal must say why it declined:\n{lean}"
    );
}

/// One helper is enough to hide the observation, and it must not be.
///
/// The observer test read `law.lhs`, `law.rhs` and `law.when` syntactically and
/// returned before any key type was examined, while key types were already
/// collected across the callee cone. So `firstValue() => 3`, whose body calls
/// `hiddenValues` whose body calls `Map.values`, named no observer and exported
/// as a theorem about an order the exporter had not established.
#[test]
fn an_observer_behind_a_helper_is_still_refused() {
    let lean = emit_lean(
        INVISIBLE_KEY_FIXTURE,
        "aver-map-order-hidden-observer-lean",
        "MapOrderInvisibleKey",
    );
    assert!(
        !lean.contains("theorem firstValue_law_firstIsLowestKeysValue"),
        "an iteration-order law reaching its observer through a helper must NOT \
         be exported as a theorem:\n{lean}"
    );
    assert!(
        lean.contains(
            "-- verify law firstValue.firstIsLowestKeysValue: map iteration order is \
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
/// types are read through user type definitions for that reason.
///
/// `toString'` takes a `Json` and its cone reaches `Map.entries`, so it is an
/// iteration-order claim whose key type is reachable ONLY by following that
/// variant payload. Its two laws are named positively here: a walk that stopped
/// at the annotation would find no key, decline both, and leave this test
/// asserting the absence of a string it could no longer produce.
#[test]
fn a_key_type_reached_through_a_user_type_is_exported() {
    let lean = emit_lean("examples/data/json.av", "aver-map-order-json-lean", "Json");
    for law in [
        "theorem toString'_law_parseValueRoundtrip :",
        "theorem toString'_law_parseRoundtrip :",
    ] {
        assert!(
            lean.contains(law),
            "`{law}` reads `Map.entries` over a map named only inside the `Json` \
             variant payload, so it exports exactly when the key type is followed \
             into that declaration:\n{}",
            lean.lines()
                .filter(|l| l.contains("map iteration order"))
                .collect::<Vec<_>>()
                .join("\n")
        );
    }
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
/// from the very file whose iteration-order claims are refused.
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
        "every map in the map order fixture is String- or Int-keyed and says so in \
         a signature, so nothing in it may be refused:\n{ordered}"
    );

    let invisible = emit_lean(
        INVISIBLE_KEY_FIXTURE,
        "aver-map-order-blind-invisible-lean",
        "MapOrderInvisibleKey",
    );
    assert!(
        invisible.contains("theorem sizeAfterSet_law_neverEmptyAfterSet : ∀"),
        "a `Map.len` law is order-blind and must still export, in the same file \
         whose iteration-order claims are declined:\n{invisible}"
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
        INVISIBLE_KEY_FIXTURE,
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
    let dfy = std::fs::read_to_string(out_dir.join("MapOrderInvisibleKey.dfy"))
        .expect("expected the generated Dafny file to exist");
    let _ = std::fs::remove_dir_all(&out_dir);
    assert!(
        dfy.contains(
            "// Law iteratedValues.lowestKeyLeadsTheValues: map iteration order is not exported"
        ),
        "Dafny must mirror the Lean refusal or the two backends disagree on the \
         same source:\n{dfy}"
    );
    assert!(
        dfy.contains(
            "// Law firstValue.firstIsLowestKeysValue: map iteration order is not exported"
        ),
        "Dafny must mirror the Lean refusal for an observer behind a helper:\n{dfy}"
    );

    // Plain sampled cases have a gate on the Lean side but nothing to mirror
    // here: `dafny::emit_verify_blocks` only ever walks `VerifyKind::Law`, so a
    // `verify` block without a law contributes the function definition and no
    // claim at all. Pin that, because the day Dafny starts emitting sampled
    // cases it needs the gate the Lean emitter has.
    assert!(
        !dfy.contains("plainEntries()) =="),
        "Dafny emits no claim for a plain verify case today; if that changed, \
         `verify_case_map_order_refusal` has to be wired in here too:\n{dfy}"
    );
    assert!(
        dfy.contains("function plainEntries()"),
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
/// overflow one. The ordered-key rule does not touch this: wasm-gc returns
/// bucket order for `String` keys exactly as it did for the key types the rule
/// removed.
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

/// A module boundary does not stop the cone walk.
///
/// The callee cone is resolved in the caller's scope, and `Dep.helper` is not
/// in it — so a law over `firstValue(v)` whose body calls
/// `MapKeys.builtValues(v)` one module over would find no `FnDef`, see no
/// observer, and export as a theorem. Dotted names are resolved under the
/// module that defines them for that reason.
#[test]
fn an_observer_in_another_module_is_still_refused() {
    let root = repo_root().join(CROSS_MODULE_DIR);
    let out_dir = temp_output_dir("aver-map-order-cross-module");
    let run = run_aver(&[
        "proof",
        &format!("{CROSS_MODULE_DIR}/main.av"),
        "--module-root",
        CROSS_MODULE_DIR,
        "-o",
        out_dir.to_str().expect("utf-8 temp path"),
    ]);
    assert!(
        run.status.success() && root.is_dir(),
        "`aver proof` over the cross-module fixture failed:\n{}",
        format_output(&run)
    );
    let lean = std::fs::read_to_string(out_dir.join("MapOrderCrossModule.lean"))
        .expect("expected the generated Lean module to exist");
    let _ = std::fs::remove_dir_all(&out_dir);
    assert!(
        !lean.contains("theorem firstValue_law_firstIsLowestKeysValue"),
        "a law reaching its observer across a module boundary must NOT be \
         exported as a theorem:\n{lean}"
    );
    assert!(
        lean.contains(
            "-- verify law firstValue.firstIsLowestKeysValue: map iteration order is \
             not exported"
        ),
        "the refusal must follow the call into the other module:\n{lean}"
    );
}

/// A claim the exporter refuses is COUNTED, NAMED and CHARGED.
///
/// The refusal used to leave exactly one trace: a comment inside the generated
/// Lean. Nothing was printed, no count was reported and the exit code did not
/// move — so `aver proof --check` on this fixture said "0 sorries, universal:
/// yes", exit 0, while four claims had been dropped on the floor. Two things
/// follow from that, and this test pins both.
///
/// A user reads a green check and believes four laws were certified. And —
/// the reason this is load-bearing rather than cosmetic — WIDENING the gate
/// could turn a red check green: a claim that previously failed to build stops
/// being emitted at all, so `build_errors` falls to zero and the regression
/// signal disappears exactly when it is needed. Charging the refusal is what
/// makes the exit code monotone under a widening, and printing alone does not
/// do it: CI reads the exit code, not stdout.
#[test]
fn a_declined_claim_is_counted_and_charged() {
    // The count and the charge are reported by the `--check` harness, which
    // has nothing to report until a verifier has actually run — so this needs
    // a real `lake`. The refusal itself is covered without one by the tests
    // above, and by `a_declined_claim_is_named_on_stdout_without_check`.
    if !lake_available() {
        eprintln!("skipping declined-claim accounting: `lake` not available");
        return;
    }
    let out_dir = temp_output_dir("aver-map-order-declined");
    let run = run_aver(&[
        "proof",
        INVISIBLE_KEY_FIXTURE,
        "-o",
        out_dir.to_str().expect("utf-8 temp path"),
        "--check-json",
    ]);
    let stdout = String::from_utf8_lossy(&run.stdout).to_string();
    let json: serde_json::Value = stdout
        .lines()
        .find_map(|l| serde_json::from_str(l).ok())
        .unwrap_or_else(|| panic!("expected a JSON summary line:\n{}", format_output(&run)));
    let _ = std::fs::remove_dir_all(&out_dir);

    assert_eq!(
        json["declined"].as_u64(),
        Some(4),
        "the four refused claims in the fixture must be counted:\n{stdout}"
    );
    assert_eq!(
        json["passed"].as_bool(),
        Some(false),
        "a refused claim is not a proved claim — the check must not pass:\n{stdout}"
    );
    assert_eq!(
        run.status.code(),
        Some(1),
        "the charge has to reach the EXIT CODE; CI reads that, not stdout:\n{}",
        format_output(&run)
    );
    let claims = json["declined_claims"]
        .as_array()
        .unwrap_or_else(|| panic!("expected declined_claims to be an array:\n{stdout}"));
    assert!(
        claims.iter().any(|c| {
            c["claim"] == "iteratedValues.lowestKeyLeadsTheValues" && c["kind"] == "law"
        }),
        "each declined claim must be named by its `fn.law` identity:\n{stdout}"
    );
    assert!(
        claims
            .iter()
            .any(|c| c["claim"] == "plainEntries" && c["kind"] == "cases"),
        "a declined plain `verify` block is named by its fn:\n{stdout}"
    );
    assert!(
        claims
            .iter()
            .all(|c| c["reason"].as_str().is_some_and(|r| !r.is_empty())),
        "every declined claim carries the reason it was declined:\n{stdout}"
    );
}

/// The same refusal is reported on stdout WITHOUT `--check`, and the reason
/// travels with it.
///
/// `aver proof` is an export command: it writes the artifact and exits 0. But
/// the CHANGELOG advertises that a claim the gate refuses is refused "with a
/// message saying why", and until now that message existed only inside a
/// generated file the user does not open. Exporting silently and calling that
/// a message is the advertised claim being false.
#[test]
fn a_declined_claim_is_named_on_stdout_without_check() {
    let out_dir = temp_output_dir("aver-map-order-declined-plain");
    let run = run_aver(&[
        "proof",
        INVISIBLE_KEY_FIXTURE,
        "-o",
        out_dir.to_str().expect("utf-8 temp path"),
    ]);
    let stdout = String::from_utf8_lossy(&run.stdout).to_string();
    let _ = std::fs::remove_dir_all(&out_dir);
    assert!(
        run.status.success(),
        "plain `aver proof` exports the artifact and exits 0:\n{}",
        format_output(&run)
    );
    assert!(
        stdout.contains("4 claim(s) declined"),
        "the count belongs on stdout, next to what was compiled:\n{stdout}"
    );
    assert!(
        stdout.contains("law iteratedValues.lowestKeyLeadsTheValues"),
        "each declined claim is named:\n{stdout}"
    );
    assert!(
        stdout.contains(INVISIBLE_KEY_REASON),
        "the reason travels with the name — that is the advertised guarantee:\n{stdout}"
    );
}

/// `--declined-budget` is the acknowledgement, and it is per-pot.
///
/// A refusal you have decided to live with is one flag in a CI file,
/// reviewable in a diff — the same shape the Dafny omitted-universal path and
/// `--write-baseline` already have. What it must NOT be is `--sorry-budget`:
/// "we tried and failed" and "we refused to try" are different facts, and a
/// budget granted for an open induction must not quietly license a refusal.
#[test]
fn declined_budget_is_a_separate_pot_from_sorry_budget() {
    if !lake_available() {
        eprintln!("skipping declined-budget accounting: `lake` not available");
        return;
    }
    let out_dir = temp_output_dir("aver-map-order-declined-budget");
    let dir = out_dir.to_str().expect("utf-8 temp path").to_string();

    // A generous sorry budget does NOT pay for a refusal.
    let sorry_only = run_aver(&[
        "proof",
        INVISIBLE_KEY_FIXTURE,
        "-o",
        &dir,
        "--check-json",
        "--sorry-budget",
        "10",
    ]);
    let sorry_stdout = String::from_utf8_lossy(&sorry_only.stdout).to_string();
    assert_eq!(
        sorry_only.status.code(),
        Some(1),
        "--sorry-budget must not license a refusal:\n{}",
        format_output(&sorry_only)
    );
    assert!(
        sorry_stdout.contains("\"passed\":false"),
        "--sorry-budget must not license a refusal:\n{sorry_stdout}"
    );

    // The matching declined budget does.
    let acked = run_aver(&[
        "proof",
        INVISIBLE_KEY_FIXTURE,
        "-o",
        &dir,
        "--check-json",
        "--declined-budget",
        "4",
    ]);
    let acked_stdout = String::from_utf8_lossy(&acked.stdout).to_string();
    let _ = std::fs::remove_dir_all(&out_dir);
    assert!(
        acked_stdout.contains("\"passed\":true"),
        "an acknowledged refusal passes:\n{acked_stdout}"
    );
    assert_eq!(
        acked.status.code(),
        Some(0),
        "an acknowledged refusal exits 0:\n{}",
        format_output(&acked)
    );
    assert!(
        acked_stdout.contains("\"declined\":4"),
        "acknowledging a refusal does not hide it — the count is still reported:\n{acked_stdout}"
    );
}

/// The cone walk follows a TAIL CALL.
///
/// This is the leg nobody knew about, and it is the one that proves the walk
/// has to be exhaustive rather than extended. The TCO transform runs BEFORE
/// typechecking, so by the time any backend looks at a mutual-recursion group
/// every in-group tail call is an `Expr::TailCall` — a variant the cone walk's
/// `_ => {}` arm dropped on the floor. A law over `bounce`, whose entire body
/// is `readValues(v, n)` in tail position, saw a cone containing nothing but
/// itself, found no observer, and exported as a kernel-certified theorem that
/// `aver verify` refutes.
///
/// The control that isolates it: move the same call out of tail position
/// (`List.concat(readValues(v, n), [])`) and the identical cone WAS refused.
/// Same functions, same law, same observer — only the AST variant differed.
#[test]
fn an_observer_behind_a_tail_call_is_still_refused() {
    let lean = emit_lean(
        HIDDEN_OBSERVER_FIXTURE,
        "aver-map-order-tailcall",
        "MapOrderHiddenObserver",
    );
    assert!(
        !lean.contains("theorem bounce_law_bounceReadsIterationOrder"),
        "a law reaching its observer through a tail call must NOT be exported \
         as a theorem:\n{lean}"
    );
    assert!(
        lean.contains(
            "-- verify law bounce.bounceReadsIterationOrder: map iteration order is not exported"
        ),
        "the refusal must follow the tail call into the mutual peer:\n{lean}"
    );
}

/// The cone walk descends into a STRING INTERPOLATION.
///
/// Proof export deliberately runs without interpolation lowering — it wants
/// source-level IR — so `"{firstValue(v)}"` still holds a real call when the
/// gate looks at it. The same `_ => {}` arm skipped `Expr::InterpolatedStr`,
/// so the call inside the braces was invisible: `aver verify` gave 0/2 while
/// `aver proof --check` gave exit 0, 0 sorries, and a certified theorem.
#[test]
fn an_observer_inside_string_interpolation_is_still_refused() {
    let lean = emit_lean(
        HIDDEN_OBSERVER_FIXTURE,
        "aver-map-order-interp",
        "MapOrderHiddenObserver",
    );
    assert!(
        !lean.contains("theorem describe_law_describeReadsIterationOrder"),
        "a law reaching its observer through an interpolated string must NOT \
         be exported as a theorem:\n{lean}"
    );
    assert!(
        lean.contains(
            "-- verify law describe.describeReadsIterationOrder: map iteration order is not \
             exported"
        ),
        "the refusal must descend into the interpolated segment:\n{lean}"
    );
}

/// `aver verify` refutes both of the laws the exporter refuses here.
///
/// The fixture's claims name the value written LAST, and the map yields the
/// value under the LOWEST key first — so the observation the cone walk has to
/// find is the thing that decides the answer. Without this the two tests above
/// could be satisfied by a fixture whose `Map.values` call had quietly stopped
/// mattering: the laws would still be refused, and the refusal would be
/// pinning nothing.
#[test]
fn the_hidden_observer_laws_are_refuted_by_the_vm() {
    let run = run_aver(&["verify", HIDDEN_OBSERVER_FIXTURE]);
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert!(
        stdout.contains("0/4 cases passed"),
        "both hidden-observer laws must FAIL on the VM — otherwise the observer \
         they hide is not what decides their answer:\n{}",
        format_output(&run)
    );
}

/// Map equality decided through `List.contains` is refused.
///
/// This test comes FIRST of the equality set on purpose. Detecting only
/// `BinOp::Eq` / `BinOp::Neq` is precisely what the rejected first attempt at
/// this did, and passing only the `==` test is how it reached review looking
/// finished. `List.contains(xs, v)` lowers to structural `BEq` over the list's
/// element type and decides EXACTLY the same equality without ever being a
/// `BinOp`, so an operator-shaped detector walks straight past it.
#[test]
fn map_equality_decided_by_list_contains_is_refused() {
    let lean = emit_lean(
        EQUALITY_FIXTURE,
        "aver-map-equality-contains",
        "MapEqualityUnresolvedKey",
    );
    assert!(
        !lean.contains("example : seenBefore"),
        "membership decides the same equality an operator does — it must be \
         refused too:\n{lean}"
    );
    assert!(
        lean.contains("-- verify seenBefore: map equality is not exported"),
        "the refusal must name map equality:\n{lean}"
    );
    assert!(
        lean.contains("has no ordering for K keys"),
        "the refusal must say which key type it could not order — here the one \
         the empty map literal never had decided:\n{lean}"
    );
}

/// A map that appears in NO signature is still found.
///
/// The rejected attempt collected key types only from `fd.params`,
/// `fd.return_type` and `law.givens`, and returned early when that set was
/// empty. A map built entirely inside a local binding contributed nothing, so
/// this function — parameter `Int`, return `Bool`, both maps local — exported a
/// kernel-certified example about a map equality the exporter had established
/// nothing about.
///
/// The key type is read off the compared operand's own inferred type instead,
/// which does not care where the map was built.
#[test]
fn a_map_built_only_in_a_local_binding_is_still_refused() {
    let lean = emit_lean(
        EQUALITY_FIXTURE,
        "aver-map-equality-local",
        "MapEqualityUnresolvedKey",
    );
    assert!(
        !lean.contains("example : writtenOrderMatters"),
        "a map never named in a signature is still a map:\n{lean}"
    );
    assert!(
        lean.contains("-- verify writtenOrderMatters: map equality is not exported"),
        "the refusal must fire on a map built in a local binding:\n{lean}"
    );
}

/// A comparison written in the LAW STATEMENT itself is refused.
///
/// A law's `lhs`/`rhs` template is never type-checked — its given-bound names
/// have no binding environment at that point — so an operand there carries no
/// inferred type at all. The expanded per-sample cases DO carry one, which is
/// why the gate reads them: without that, an operand-bound key type would have
/// been unavailable for exactly the shape a law is most naturally written in,
/// and the only fallback left would have been the cone-wide bag this design
/// exists to avoid.
#[test]
fn a_map_comparison_in_the_law_statement_is_refused() {
    let lean = emit_lean(
        EQUALITY_FIXTURE,
        "aver-map-equality-law",
        "MapEqualityUnresolvedKey",
    );
    assert!(
        !lean.contains("theorem rewrite_law_rewriteChangesIdentity"),
        "a comparison in the law statement must be refused like any other:\n{lean}"
    );
    assert!(
        lean.contains("-- verify law rewrite.rewriteChangesIdentity: map equality is not exported"),
        "the refusal must reach a comparison written in the law itself:\n{lean}"
    );
}

/// A comparison over an ordinary record is NOT refused, even when the same
/// call cone touches a map the exporter cannot read the key of.
///
/// This is the regression test for the finding that killed the first attempt.
/// That version treated `Type::Named { .. }` as map-like and blamed a key type
/// drawn from a bag gathered across the WHOLE cone, with nothing binding the
/// operands actually compared to the key type named in the refusal — so an
/// ordinary record comparison was declined with a message about a map key it
/// never touched, and the blast radius grew with the size of the cone.
///
/// Over-refusing is not the safe direction here. A silently dropped provable
/// law and an exported unprovable one fail the same way: `--check` is green and
/// the user believes something that is not true.
///
/// This test PASSES on unmodified code and must keep passing.
#[test]
fn a_comparison_over_a_user_adt_is_not_refused() {
    let lean = emit_lean(
        ADT_CONTROL_FIXTURE,
        "aver-map-equality-adt",
        "MapEqualityAdtControl",
    );
    assert!(
        lean.contains("theorem samePoint_law_reflexive"),
        "comparing two records is not comparing two maps — the law must still \
         be exported:\n{lean}"
    );
    assert!(
        !lean.contains("is not exported"),
        "nothing in this file may be refused; the map is only MEASURED, and the \
         record comparison has no map in it:\n{lean}"
    );

    // And the law is true, so refusing it would be a real loss rather than a
    // dodged bullet.
    let run = run_aver(&["verify", ADT_CONTROL_FIXTURE]);
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert!(
        stdout.contains("0 failed"),
        "the control law must hold on the VM:\n{}",
        format_output(&run)
    );
}

/// Two modules declaring the same bare type name are told apart.
///
/// A key type is read out of a claim's signature by following each annotation
/// into its DECLARATION. Which declaration is not a question a bare name can
/// answer: `Holder.Entry` carries a `Map<String, Int>` and the entry module's
/// own `Entry` carries none, while `Scored` is the same pair the other way
/// round — and a name-keyed lookup that strips the module prefix before
/// matching answers with the entry module's for both.
///
/// It gets exactly one of the two wrong, and the direction is over-refusal:
/// the law whose only word about a key type resolved to the map-free namesake
/// finds no key at all and is declined for a map it could have read. Both laws
/// hold on the VM, so refusing either is a real loss. The stamp's resolved type
/// identity is what separates them.
#[test]
fn two_modules_sharing_a_type_name_are_told_apart() {
    let out_dir = temp_output_dir("aver-map-order-collide");
    let run = run_aver(&[
        "proof",
        &format!("{COLLIDING_TYPE_DIR}/main.av"),
        "--module-root",
        COLLIDING_TYPE_DIR,
        "-o",
        out_dir.to_str().expect("utf-8 temp path"),
    ]);
    assert!(
        run.status.success(),
        "`aver proof` over the colliding-type fixture failed:\n{}",
        format_output(&run)
    );
    let lean = std::fs::read_to_string(out_dir.join("MapOrderCollidingType.lean"))
        .expect("expected the generated Lean module to exist");
    let _ = std::fs::remove_dir_all(&out_dir);

    assert!(
        lean.contains("theorem heldOrder_law_readsBothValues : ∀"),
        "`Holder.Entry` is the map-carrying one of that name — the law that \
         carries it must find a String key and export:\n{lean}"
    );
    assert!(
        lean.contains("theorem ownOrder_law_alsoReadsBothValues : ∀"),
        "this module's own `Scored` is the map-carrying one of THAT name — the \
         law that carries it must export too:\n{lean}"
    );
    assert!(
        !lean.contains("is not exported"),
        "each annotation names a declaration that does hold a map; resolving \
         either to its map-free namesake declines a provable law:\n{lean}"
    );

    // Both laws are true, so either verdict going the wrong way is a loss.
    let verify = run_aver(&[
        "verify",
        &format!("{COLLIDING_TYPE_DIR}/main.av"),
        "--module-root",
        COLLIDING_TYPE_DIR,
    ]);
    let stdout = String::from_utf8_lossy(&verify.stdout);
    assert!(
        stdout.contains("0 failed"),
        "both laws must hold on the VM:\n{}",
        format_output(&verify)
    );
}

/// Generate a Lean project for a fixture that lives in a module directory and
/// return the emitted module text.
fn emit_lean_in_dir(dir: &str, file: &str, prefix: &str, module: &str) -> String {
    let out_dir = temp_output_dir(prefix);
    let run = run_aver(&[
        "proof",
        &format!("{dir}/{file}"),
        "--module-root",
        dir,
        "-o",
        out_dir.to_str().expect("utf-8 temp path"),
    ]);
    assert!(
        run.status.success(),
        "`aver proof {dir}/{file}` failed:\n{}",
        format_output(&run)
    );
    let text = std::fs::read_to_string(out_dir.join(format!("{module}.lean")))
        .expect("expected the generated Lean module to exist");
    let _ = std::fs::remove_dir_all(&out_dir);
    text
}

/// The claim's verdict does not depend on WHICH same-named type the walk
/// reached first.
///
/// `free_first.av` and `map_first.av` are one file written twice, differing
/// only in the order of two names in one signature — same law, same runtime
/// answer, and `aver verify` passes on both. The gate follows each annotation
/// into its own declaration and then remembers "I have already looked at this
/// type" under the DECLARED name, so whichever `Entry` the walk met first
/// claimed the name and the other module's `Entry` was skipped as already-seen.
/// Meeting the map-free `Holder.Entry` first therefore hid the
/// `Map<String, Int>` in the entry module's `Entry`, the claim contributed no
/// key type at all, and a provable law was declined for a key it could have
/// read.
///
/// A test over one ordering would pass on that code. This one pins both.
#[test]
fn a_key_type_is_found_whichever_namesake_the_walk_met_first() {
    let free_first = emit_lean_in_dir(
        TYPE_MEMO_DIR,
        "free_first.av",
        "aver-map-order-free-first",
        "MapOrderTypeMemoFreeFirst",
    );
    let map_first = emit_lean_in_dir(
        TYPE_MEMO_DIR,
        "map_first.av",
        "aver-map-order-map-first",
        "MapOrderTypeMemoMapFirst",
    );
    for (label, lean) in [("map_first.av", &map_first), ("free_first.av", &free_first)] {
        assert!(
            lean.contains("theorem scoreCount_law_countsWhatItIterates : ∀"),
            "{label}: the law reads a String-keyed map that its signature does \
             name, so it must export:\n{lean}"
        );
        assert!(
            !lean.contains("is not exported"),
            "{label}: meeting a map-free type of the same bare name first must \
             not hide the map-carrying one:\n{lean}"
        );
    }

    // And the law holds, so a decline either way is a real loss.
    for file in ["free_first.av", "map_first.av"] {
        let verify = run_aver(&[
            "verify",
            &format!("{TYPE_MEMO_DIR}/{file}"),
            "--module-root",
            TYPE_MEMO_DIR,
        ]);
        let stdout = String::from_utf8_lossy(&verify.stdout);
        assert!(
            stdout.contains("1/1 cases passed"),
            "{file} must hold on the VM, and the case count is part of the claim:\n{}",
            format_output(&verify)
        );
    }
}

/// The claim's verdict does not depend on an unrelated declaration it never
/// names.
///
/// `plain.av` and `shadowed.av` are one file written twice; `shadowed.av`
/// adds a record called `Scored` that carries no map and that the law never
/// mentions. The law carries a `Box`, and `Box.held` is annotated
/// `Holder.Scored` — a piece of TEXT, which the walk resolved by taking the
/// last dotted segment and returning the first declaration of that bare name
/// it could find, entry module first. So the unrelated `Scored` answered for
/// `Holder.Scored`, the `Map<String, Int>` inside the real one was never
/// reached, and adding a record to an unrelated part of the file flipped the
/// law from exported to declined.
#[test]
fn a_field_annotation_resolves_to_the_type_it_names() {
    let plain = emit_lean_in_dir(
        FIELD_COLLISION_DIR,
        "plain.av",
        "aver-map-order-field-plain",
        "MapOrderFieldPlain",
    );
    let shadowed = emit_lean_in_dir(
        FIELD_COLLISION_DIR,
        "shadowed.av",
        "aver-map-order-field-shadowed",
        "MapOrderFieldShadowed",
    );
    for (label, lean) in [("plain.av", &plain), ("shadowed.av", &shadowed)] {
        assert!(
            lean.contains("theorem boxOrder_law_readsBothValues : ∀"),
            "{label}: the box's field names a record that holds a String-keyed \
             map, so the law must export:\n{lean}"
        );
        assert!(
            !lean.contains("is not exported"),
            "{label}: declaring an unrelated type that happens to share a bare \
             name with the field's type must not change the verdict:\n{lean}"
        );
    }

    for file in ["plain.av", "shadowed.av"] {
        let verify = run_aver(&[
            "verify",
            &format!("{FIELD_COLLISION_DIR}/{file}"),
            "--module-root",
            FIELD_COLLISION_DIR,
        ]);
        let stdout = String::from_utf8_lossy(&verify.stdout);
        assert!(
            stdout.contains("2/2 cases passed"),
            "{file} must hold on the VM, and the case count is part of the claim:\n{}",
            format_output(&verify)
        );
    }
}

/// A function handed to a higher-order parameter is part of the cone.
///
/// `viaHof(f, v)` applies whatever it is given, and the claim passes
/// `builtKeys` — whose body reads `Map.keys` — by name. The cone was built
/// from CALLS, and a name in argument position is a leaf: nothing called
/// `builtKeys` syntactically, so the walk never entered it, saw no observer,
/// and exported both the case and the law as certified claims about a
/// key sequence the runtime never produces. `aver verify` refutes both.
#[test]
fn an_observer_passed_in_by_name_is_still_refused() {
    let lean = emit_lean(
        FN_PARAM_FIXTURE,
        "aver-map-order-fn-param",
        "MapOrderFnParam",
    );
    assert!(
        lean.contains("-- verify viaHof: map iteration order is not exported"),
        "the case reads the map's keys through the reader it was handed, so \
         it must be declined:\n{lean}"
    );
    assert!(
        lean.contains("-- verify law viaHof.keysAsWritten: map iteration order is not exported"),
        "the law reads the map's keys through the reader it was handed, so it \
         must be declined:\n{lean}"
    );
    assert!(
        !lean.contains("native_decide"),
        "nothing in this file may be stated as a theorem:\n{lean}"
    );

    let verify = run_aver(&["verify", FN_PARAM_FIXTURE]);
    let stdout = String::from_utf8_lossy(&verify.stdout);
    assert!(
        stdout.contains("0/3 cases passed"),
        "the fixture is meant to be refuted on the VM — that is what makes the \
         reader it is handed the thing that decides its answer:\n{}",
        format_output(&verify)
    );
}

/// A variable that shares a name with a map reader is not a map reader.
///
/// The control on the fix above, and it went red while that fix was being
/// written. Following every argument that resolves to a function pulls an
/// unrelated `fn keys()` into the cone of a law whose given is called `keys`,
/// and the law — true, `2/2` on the VM, with no map anywhere near it — was
/// declined for reading an iteration order it never reads.
///
/// Over-refusing is not the safe direction: a provable law dropped in silence
/// and an unprovable one exported both end with a green `--check` and a user
/// believing something untrue. Which arguments are functions is the
/// typechecker's answer, and the variable's stamp says `List<Float>`.
///
/// The given KEEPS the reader's spelling on purpose: verify-block binders
/// are outside the shadowing ban's v1 scope (issue #954), so this shape
/// stays writable and the cone walk still has to resist it. Only the
/// fixture's fn parameter had to be renamed — the original spelling lives
/// on as the ban's error witness in the test below.
#[test]
fn a_given_named_after_a_map_reader_is_not_refused() {
    let lean = emit_lean(
        READER_NAMED_GIVEN_FIXTURE,
        "aver-map-order-reader-named-given",
        "MapOrderGivenNamedAfterReader",
    );
    assert!(
        lean.contains("theorem howMany_law_countsThem"),
        "counting a list is not reading a map — the law must still be \
         exported:\n{lean}"
    );
    assert!(
        !lean.contains("is not exported"),
        "nothing in this file may be refused; the only map reader in it is \
         never called:\n{lean}"
    );

    // And the law holds, so refusing it would be a real loss.
    let verify = run_aver(&["verify", READER_NAMED_GIVEN_FIXTURE]);
    let stdout = String::from_utf8_lossy(&verify.stdout);
    assert!(
        stdout.contains("0 failed"),
        "the control law must hold on the VM:\n{}",
        format_output(&verify)
    );
}

/// The deliberate shadow fixture is now the ban's error witness: its
/// `howMany` parameter spells the module fn `keys`, and the shadowing ban
/// (issue #954) refuses that at the front door — `aver proof` never
/// reaches the cone walk. The cone-walk control this fixture used to
/// carry moved to `map_order_given_named_after_reader.av` above, given
/// spelling intact.
#[test]
fn the_shadowed_given_fixture_is_rejected_by_the_shadowing_ban() {
    let out_dir = temp_output_dir("aver-map-order-shadowed-reject");
    let run = run_aver(&[
        "proof",
        SHADOWED_GIVEN_FIXTURE,
        "-o",
        out_dir.to_str().expect("utf-8 temp path"),
    ]);
    let _ = std::fs::remove_dir_all(&out_dir);
    assert!(
        !run.status.success(),
        "a parameter shadowing a module fn must be rejected before export:\n{}",
        format_output(&run)
    );
    assert!(
        String::from_utf8_lossy(&run.stderr).contains(
            "the parameter 'keys' shadows the function 'keys' defined at line 6; \
             every name means one thing in its scope — rename one of them"
        ),
        "the refusal must be the standard shadow error:\n{}",
        format_output(&run)
    );
}

/// A bare call to a module's own peer is read in THAT module's scope.
///
/// The walk crosses a module boundary by following a dotted `Peek.describe`,
/// and then reads the names inside that body — but it resolved every one of
/// them in the CLAIM's scope. `Peek.describe` calls its peer as a bare
/// `hiddenValues`, which is a name in `Peek` and nowhere else, so the cone
/// stopped one hop short of `Map.values` and both laws exported as
/// `native_decide` theorems that `aver verify` refutes 0/2.
///
/// With a same-named function in the claim's own file the miss is worse than a
/// stop: the walk resolved `hiddenValues` to the entry file's map-free
/// namesake and reported on a function the claim never calls.
///
/// The call is written inside `"{…}"` on both sides of the boundary, so the
/// descent into an interpolated segment is exercised end to end here as well.
#[test]
fn an_observer_behind_a_bare_peer_call_is_still_refused() {
    let lean = emit_lean_in_dir(
        NAME_COLLISION_DIR,
        "main.av",
        "aver-map-order-name-collision",
        "MapOrderNameCollision",
    );
    assert!(
        lean.contains(
            "-- verify law renderedThenLocal.renderedThenLocalIsWrittenOrder: map iteration \
             order is not exported"
        ),
        "the walk must follow a bare peer call inside the module that wrote \
         it:\n{lean}"
    );
    assert!(
        !lean.contains("native_decide"),
        "neither law may be stated as a theorem — the VM refutes both:\n{lean}"
    );
}

/// The verdict does not depend on WHICH same-named function the walk met
/// first.
///
/// `renderedThenLocal` and `localThenRendered` are one claim written twice,
/// differing only in the order of the two calls inside the interpolated
/// string: the entry file's map-free `hiddenValues` and `Peek.describe`, which
/// reaches the observing `Peek.hiddenValues`. Same runtime answer, and `aver
/// verify` refutes both.
///
/// "Already walked this one" has to be keyed on the declaration the name
/// RESOLVED to, not on the word. Keyed on the word — which is all the queue
/// carried while this was being written — whichever `hiddenValues` the walk
/// popped first claimed the name and the other module's was skipped as
/// already-seen: reaching the map-free one first hid the observer, and that
/// law exported while its mirror image was refused.
///
/// A test over one ordering passes on that code. This one pins both.
#[test]
fn the_map_order_verdict_is_the_same_whichever_namesake_came_first() {
    let lean = emit_lean_in_dir(
        NAME_COLLISION_DIR,
        "main.av",
        "aver-map-order-name-collision-order",
        "MapOrderNameCollision",
    );
    for law in [
        "renderedThenLocal.renderedThenLocalIsWrittenOrder",
        "localThenRendered.localThenRenderedIsWrittenOrder",
    ] {
        assert!(
            lean.contains(&format!(
                "-- verify law {law}: map iteration order is not exported"
            )),
            "{law} reaches the same observer as its mirror image and must get \
             the same verdict:\n{lean}"
        );
    }
}

/// Both name-collision laws are refuted by the VM.
///
/// Each claim names the value written LAST on both sides of the module
/// boundary, and the map yields the value under the LOWEST key first. Without
/// this the two tests above could be satisfied by a fixture whose observer had
/// stopped deciding the answer — the laws would still be refused, and the
/// refusal would be pinning nothing.
#[test]
fn the_name_collision_laws_are_refuted_by_the_vm() {
    let verify = run_aver(&[
        "verify",
        &format!("{NAME_COLLISION_DIR}/main.av"),
        "--module-root",
        NAME_COLLISION_DIR,
    ]);
    let stdout = String::from_utf8_lossy(&verify.stdout);
    assert!(
        stdout.contains("0/2 cases passed"),
        "both laws must FAIL on the VM — otherwise the observer they hide is not \
         what decides their answer:\n{}",
        format_output(&verify)
    );
}

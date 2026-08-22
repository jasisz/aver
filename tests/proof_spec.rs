use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

#[path = "proof_spec/builds.rs"]
mod builds;
#[path = "proof_spec/capability.rs"]
mod capability;
#[path = "proof_spec/capability_opaque.rs"]
mod capability_opaque;
#[path = "proof_spec/check_gates.rs"]
mod check_gates;
#[path = "proof_spec/container_induction.rs"]
mod container_induction;
#[path = "proof_spec/cross_file.rs"]
mod cross_file;
#[path = "proof_spec/dafny_inline.rs"]
mod dafny_inline;
#[path = "proof_spec/export_structure.rs"]
mod export_structure;
#[path = "proof_spec/floor_window.rs"]
mod floor_window;
#[path = "proof_spec/lean_kernel.rs"]
mod lean_kernel;
#[path = "proof_spec/lemmas.rs"]
mod lemmas;
#[path = "proof_spec/literalization.rs"]
mod literalization;
#[path = "proof_spec/opaque_closure.rs"]
mod opaque_closure;
#[path = "proof_spec/oracle_verify.rs"]
mod oracle_verify;
#[path = "proof_spec/panics.rs"]
mod panics;
#[path = "proof_spec/when_lane.rs"]
mod when_lane;

fn temp_output_dir(prefix: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    std::env::temp_dir().join(format!("{prefix}-{nanos}"))
}

fn format_output(output: &std::process::Output) -> String {
    format!(
        "status: {}\nstdout:\n{}\nstderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}

fn assert_proof_builds(example_path: &str, prefix: &str) {
    assert_proof_builds_with_sorry_budget(example_path, prefix, 0);
}

/// `assert_proof_builds`, but tolerate `expected_sorries` occurrences
/// of `sorry` in the generated Lean output. `lake build` accepts
/// `sorry` so it stays green forever once one slips in; gating on the
/// count catches *new* regressions while letting existing budgets
/// (e.g. `json`'s 13 sampled-domain laws) ride until their underlying
/// shape gets a real proof strategy. A drop below the budget fails
/// loudly too — that's the cue to tighten it.
fn assert_proof_builds_with_sorry_budget(
    example_path: &str,
    prefix: &str,
    expected_sorries: usize,
) {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping proof smoke test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let output_dir = temp_output_dir(prefix);
    let aver_bin = env!("CARGO_BIN_EXE_aver");

    // One subprocess: `aver proof --check --check-json` generates the
    // project, runs `lake build`, parses the residual `sorry` count
    // from the build's `declaration uses 'sorry'` warnings, and emits
    // a JSON summary. Exit code is ignored — the test asserts
    // exact-match on the count for regression detection (the CLI's
    // `≤ budget` semantics let CI tolerate noisy examples, but tests
    // want a drift-up-or-down signal both ways).
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg(example_path)
        .arg("--backend")
        .arg("lean")
        .arg("--verify-mode")
        .arg("auto")
        .arg("-o")
        .arg(&output_dir)
        .arg("--check")
        .arg("--check-json")
        // Budget = the expected count, so `passed` means "lake build SUCCEEDED
        // and stayed within this count". Asserting `passed` below (with the
        // count asserted equal) certifies the build actually succeeded — not
        // just that the sorry-warning count matched. Guards the false-green
        // where a tactic leaves unsolved goals (lake exit 1, zero sorry
        // warnings) yet a count-only check would pass.
        .arg("--sorry-budget")
        .arg(expected_sorries.to_string())
        .output()
        .expect("expected `aver proof --check --check-json` to run");

    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| {
            panic!(
                "`aver proof --check --check-json` produced no JSON line:\n{}",
                format_output(&run)
            )
        });
    let summary: serde_json::Value = serde_json::from_str(json_line).unwrap_or_else(|e| {
        panic!(
            "failed to parse `aver proof --check --check-json` output as JSON ({}):\n{}",
            e, json_line
        )
    });
    let actual = summary["sorries"].as_u64().unwrap_or_else(|| {
        panic!(
            "`sorries` field missing from --check-json summary:\n{}",
            json_line
        )
    }) as usize;
    assert_eq!(
        actual,
        expected_sorries,
        "{}: sorry count drift (expected {}, got {}). \
         If the count dropped, lower the budget. If it grew, a new shape regressed — \
         investigate before raising the budget.\n{}",
        example_path,
        expected_sorries,
        actual,
        format_output(&run)
    );

    // Build-success guard (the false-green fix): with --sorry-budget set to the
    // expected count and the count asserted equal above, `passed:false` here
    // means `lake build` itself FAILED (e.g. a tactic left unsolved goals —
    // lake exit 1 with zero `sorry` warnings, which the count-only check is
    // blind to). The generated proof must actually build, not merely match a
    // sorry count.
    let passed = summary["passed"].as_bool().unwrap_or(false);
    assert!(
        passed,
        "{}: generated Lean proof does NOT build (lake reported failure within \
         the sorry budget {}). A count match alone is not enough — the build \
         must succeed.\n{}",
        example_path,
        expected_sorries,
        format_output(&run)
    );

    let _ = std::fs::remove_dir_all(&output_dir);
}

/// `dafny verify` smoke test. Mirrors `assert_proof_builds` but runs
/// the Dafny backend through the full verifier (not just the parser /
/// compile front-end). `lake build` accepts `sorry`-bearing proofs;
/// `dafny verify` actually closes the goal. Several examples verify
/// cleanly and pin the IR-migrated strategy coverage (Steps 24-40 of
/// the proof-IR migration); the remaining flagship examples
/// (`fibonacci`, `rle`, `quicksort`, `date`, `json`) carry
/// pre-IR-migration Dafny gaps tracked in issue #114 and are gated
/// via [`assert_dafny_verifies_with_budgets`].
fn assert_dafny_verifies(example_path: &str, prefix: &str) {
    assert_dafny_verifies_with_budgets(example_path, prefix, 0, 0);
}

/// `assert_dafny_verifies`, but tolerate up to `expected_errors` Dafny
/// verification errors AND exactly `expected_axioms` `assume {:axiom}`
/// trust escapes.
///
/// The error budget is a CEILING (`<=`): the number of undischarged
/// postconditions is platform-sensitive (Z3 build) — quicksort closes 8
/// on macOS, 9 on Linux CI (#342) — so an exact match is fragile. A count
/// ABOVE the ceiling is a real regression (a shape stopped closing). The
/// axiom budget stays EXACT: `assume {:axiom}` is emitted by our codegen,
/// not Z3, so it's deterministic — a drop means a stronger proof (tighten),
/// a rise means a law degraded to a trusted axiom. Parses both counts from
/// the `--check-json` summary.
fn assert_dafny_verifies_with_budgets(
    example_path: &str,
    prefix: &str,
    expected_errors: usize,
    expected_axioms: usize,
) {
    dafny_check_with_budgets(
        example_path,
        prefix,
        expected_errors,
        expected_axioms,
        false,
    );
}

/// `assert_dafny_verifies`, but additionally assert the check's own
/// verdict: `passed == true` in the `--check-json` summary.
///
/// `passed` keys on the `dafny verify` EXIT STATUS — the only place
/// prover TIMEOUTS surface. A timed-out obligation reports as exit 4
/// with `0 errors` in the parsed verifier summary, so an errors/axioms
/// budget assert alone stays green while the file no longer verifies.
/// Use this for anchors whose regression mode is a timeout rather than
/// an error-count drift.
fn assert_dafny_verifies_and_passes(example_path: &str, prefix: &str) {
    dafny_check_with_budgets(example_path, prefix, 0, 0, true);
}

fn dafny_check_with_budgets(
    example_path: &str,
    prefix: &str,
    expected_errors: usize,
    expected_axioms: usize,
    require_passed: bool,
) {
    if Command::new("dafny").arg("--version").output().is_err() {
        eprintln!("skipping dafny verify smoke test: `dafny` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let output_dir = temp_output_dir(prefix);
    let aver_bin = env!("CARGO_BIN_EXE_aver");

    // Same single-subprocess shape as the Lean side: generate, run
    // `dafny verify`, parse the error count out of the verifier
    // summary, emit JSON. Exit code is ignored — tests assert an
    // exact-match on `errors` for regression detection in both
    // directions (drift up = lost a strategy, drift down = budget can
    // be tightened).
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg(example_path)
        .arg("--backend")
        .arg("dafny")
        .arg("-o")
        .arg(&output_dir)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check --check-json` to run");

    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| {
            panic!(
                "`aver proof --check --check-json` produced no JSON line:\n{}",
                format_output(&run)
            )
        });
    let summary: serde_json::Value = serde_json::from_str(json_line).unwrap_or_else(|e| {
        panic!(
            "failed to parse `aver proof --check --check-json` output as JSON ({}):\n{}",
            e, json_line
        )
    });
    let actual = summary["errors"].as_u64().unwrap_or_else(|| {
        panic!(
            "`errors` field missing from --check-json summary:\n{}",
            json_line
        )
    }) as usize;
    // The error budget is a CEILING (`<=`), not an exact count. The same
    // proof can leave a DIFFERENT number of postconditions undischarged
    // across Z3 builds: quicksort closes 8 on macOS but 9 on Linux CI,
    // because Linux's Z3 hits a counterexample-model parse failure (an
    // internal float `0.0`) on one extra assertion and reports it as
    // unproven (#342). An exact `assert_eq` is fragile against that
    // platform jitter; a ceiling tolerates it while still catching a real
    // regression (count ABOVE the ceiling = a new shape stopped closing).
    assert!(
        actual <= expected_errors,
        "{}: dafny error count {} exceeds the budget ceiling {} — a new shape \
         regressed (the budget already tolerates platform-sensitive Z3 jitter \
         below it). Investigate before raising the ceiling.\n{}",
        example_path,
        actual,
        expected_errors,
        format_output(&run)
    );

    // Pin the `assume {:axiom}` count too — the Dafny analog of the Lean
    // sorry budget. An axiom is a TRUSTED (unproven) obligation: a law that
    // silently degrades from a real proof to `assume {:axiom}` keeps
    // `errors == 0` and would slip past an errors-only check (the symmetric
    // twin of the Lean unsolved-goals false-green). Exact-match in both
    // directions: a drop means the proof got stronger (lower the count), a
    // rise means a law regressed to trust (investigate before raising).
    let actual_axioms = summary["axioms"].as_u64().unwrap_or_else(|| {
        panic!(
            "`axioms` field missing from --check-json summary:\n{}",
            json_line
        )
    }) as usize;
    assert_eq!(
        actual_axioms,
        expected_axioms,
        "{}: dafny axiom (assume {{:axiom}}) count drift (expected {}, got {}). \
         These are trusted, NOT proven. A rise means a law regressed to an axiom — \
         investigate before raising the count.\n{}",
        example_path,
        expected_axioms,
        actual_axioms,
        format_output(&run)
    );

    if require_passed {
        assert_eq!(
            summary["passed"].as_bool(),
            Some(true),
            "{}: `--check` reports passed:false even though the error budget \
             holds — `passed` keys on the dafny exit status, where prover \
             timeouts surface (exit 4, 0 parsed errors). The file stopped \
             verifying within the time limit.\n{}",
            example_path,
            format_output(&run)
        );
    }

    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Run `aver proof <file> --backend lean --check --check-json` with
/// optional extra env vars; returns the parsed JSON summary line plus
/// the raw output for diagnostics. Shared by the proof gate tests, which
/// need several runs against the SAME output dir (lake's content-addressed
/// cache keeps re-runs cheap).
fn run_lean_check_json(
    example_path: &str,
    output_dir: &std::path::Path,
    sorry_budget: usize,
    envs: &[(&str, &str)],
) -> (serde_json::Value, std::process::Output) {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let mut cmd = Command::new(aver_bin);
    cmd.current_dir(&repo_root)
        .arg("proof")
        .arg(example_path)
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(output_dir)
        .arg("--check")
        .arg("--check-json")
        .arg("--sorry-budget")
        .arg(sorry_budget.to_string());
    for (k, v) in envs {
        cmd.env(k, v);
    }
    let run = cmd
        .output()
        .expect("expected `aver proof --check --check-json` to run");
    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)))
        .to_string();
    let summary: serde_json::Value =
        serde_json::from_str(&json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}"));
    (summary, run)
}

/// `aver proof --minimize` collapses each `first | … | sorry` portfolio in a
/// generated proof to the branch that actually closed — without changing
/// whether the proof passes. `int_comparison_laws` emits three shape-gated
/// `grind` portfolios (`first | (grind …) | (<body ending in sorry>)`); the
/// probe build reports `grind` as their winner, so the minimized proof keeps
/// just `grind […]; done` and drops the dead body-plus-`sorry` arm while
/// staying universal. Guards the whole instrument → parse → collapse →
/// fail-safe pipeline end-to-end.
#[test]
fn proof_minimize_collapses_grind_portfolios_and_stays_passing() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping --minimize test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let example = "examples/formal/int_comparison_laws.av";
    let output_dir = repo_root.join("target").join("proof-minimize-it");
    let _ = std::fs::remove_dir_all(&output_dir);
    let lean_path = output_dir.join("IntComparisonLaws.lean");

    // Baseline emit (no --minimize): the grind rung is a structured
    // `first | (grind …) | (…)` portfolio.
    let baseline = Command::new(aver_bin)
        .current_dir(&repo_root)
        .args(["proof", example, "--backend", "lean", "-o"])
        .arg(&output_dir)
        .output()
        .expect("baseline `aver proof` to run");
    assert!(
        baseline.status.success(),
        "baseline emit failed:\n{}",
        format_output(&baseline)
    );
    let baseline_src = std::fs::read_to_string(&lean_path).expect("read baseline Lean");
    let portfolios = baseline_src.matches("| (grind [").count();
    assert!(
        portfolios > 0,
        "fixture should emit grind-wrap portfolios; got none:\n{baseline_src}"
    );

    // With --minimize --check: portfolios collapse to their winner; the proof
    // must still pass with zero sorries.
    let min = Command::new(aver_bin)
        .current_dir(&repo_root)
        .args(["proof", example, "--backend", "lean", "-o"])
        .arg(&output_dir)
        .args([
            "--check",
            "--check-json",
            "--minimize",
            "--sorry-budget",
            "0",
        ])
        .output()
        .expect("`aver proof --check --minimize` to run");
    let json_line = min
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&min)))
        .to_string();
    let summary: serde_json::Value =
        serde_json::from_str(&json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}"));
    assert_eq!(
        summary["passed"].as_bool(),
        Some(true),
        "minimized proof must still pass:\n{}",
        format_output(&min)
    );
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(0),
        "minimized proof must keep zero sorries:\n{}",
        format_output(&min)
    );

    let min_src = std::fs::read_to_string(&lean_path).expect("read minimized Lean");
    assert!(
        !min_src.contains("AVERMIN"),
        "instrument markers must not survive into the minimized proof:\n{min_src}"
    );
    assert!(
        min_src.matches("| (grind [").count() < portfolios,
        "grind portfolios should be collapsed (fewer `| (grind [` lines than baseline {portfolios}):\n{min_src}"
    );
    assert!(
        min_src.contains("grind ["),
        "the winning grind branch should remain in the collapsed proof:\n{min_src}"
    );

    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Shared probe source for the fuel-exhaustion soundness tests: a recursive
/// Int countdown whose verify case routes BOTH sides through the model
/// (`stepSum(20) => stepSumAcc(20)`) — the exact shape that goes vacuous when
/// fuel exhaustion collapses both sides to `default` under `native_decide`.
const FUEL_PROBE_AV: &str = "module FuelProbe\n\
    \x20   intent = \"fuel-exhaustion soundness probe\"\n\
    \n\
    fn stepSum(n: Int) -> Int\n\
    \x20   ? \"Sums 1..n by counting down.\"\n\
    \x20   match n <= 0\n\
    \x20       true  -> 0\n\
    \x20       false -> n + stepSum(n - 1)\n\
    \n\
    fn stepSumAcc(n: Int) -> Int\n\
    \x20   ? \"Sums 1..n, other association.\"\n\
    \x20   match n <= 0\n\
    \x20       true  -> 0\n\
    \x20       false -> stepSumAcc(n - 1) + n\n\
    \n\
    verify stepSum\n\
    \x20   stepSum(20) => stepSumAcc(20)\n";

/// A builtin that emits as `receiver.method arg` has to be parenthesised as a
/// whole where an argument is expected.
///
/// The receiver arrives parenthesised, so the application begins with `(` —
/// and the atomicity test used to read "begins with a bracket" as "already
/// atomic" and pass it through unwrapped. Lean then reads
/// `Except.ok (xs).take (Int.toNat 4)` as `Except.ok` applied to a partially
/// applied `List.take` and then to a second argument, and refuses it with
/// `function expected`. Measured on an external project, this one shape
/// accounted for 86% of the claims whose emitted Lean would not elaborate.
///
/// The fixture puts five such builtins behind a compound receiver. With the
/// old test in place it reports twelve build errors.
#[test]
fn a_method_application_in_argument_position_is_emitted_atomically() {
    assert_proof_builds(
        "tests/fixtures/lean_arg_atomicity.av",
        "aver-proof-arg-atomicity",
    );
}

/// A mutual pair's termination measure counts what travels between the peers,
/// not every list the functions happen to take.
///
/// `hereOrDeeper(head, tail, n)` returns `head` and forwards `tail`. Measuring
/// `sizeOf head + sizeOf tail` against the peer's `sizeOf tail` makes the step
/// decrease only if `head` is non-empty — true of a list, but not something
/// `omega` has any reason to believe, so the whole mutual block failed to
/// build. Counting only what is forwarded also lets the ranker see that the
/// step leaves the measure unchanged, so it orders the two peers instead of
/// assuming a strict decrease.
///
/// Mutation-checked: with the measure widened back to every carrier, the
/// fixture reports a build error.
#[test]
fn a_mutual_measure_counts_only_what_is_forwarded() {
    assert_proof_builds(
        "tests/fixtures/mutual_measure_forwarded.av",
        "aver-proof-mutual-measure",
    );
}

/// A recursion cycle of three functions, each descending on a DIFFERENT
/// list — the shape of a byte-wise lexicographic comparison, met on the first
/// external project.
///
/// Measuring each member by one parameter cannot work: on the call that
/// hands a tail on unchanged, the caller's counted parameter is not the
/// callee's, and the termination checker answers with a counterexample
/// (`omega could not prove the goal: a possible counterexample may satisfy
/// the constraints b ≥ 0 a ≥ 0`). The measure now counts every parameter
/// that travels around the cycle, and the one call that leaves the sum
/// unchanged is settled by ordering the members.
///
/// On the binary before this change the fixture reports two build errors.
#[test]
fn a_three_member_cycle_descending_on_different_lists_builds() {
    assert_proof_builds(
        "tests/fixtures/mutual_cycle_three_lists.av",
        "aver-proof-cycle-three-lists",
    );
}

/// A cycle of four, each member consuming one of four lists and handing the
/// other three on unchanged. Every call shrinks exactly one list, so only a
/// measure counting all four decreases on every call. Four build errors on
/// the binary before this change.
#[test]
fn a_four_member_cycle_builds() {
    assert_proof_builds(
        "tests/fixtures/mutual_cycle_four_lists.av",
        "aver-proof-cycle-four-lists",
    );
}

/// A cycle in which the list shrinks on one call and an `Int` budget on
/// another, so neither alone decreases around the cycle.
///
/// The structural parameters alone leave a cycle of unchanged calls, so the
/// budget joins the measure as `Int.toNat budget`, which the guard on the
/// spending call lets the checker see shrink. Before this change the pair
/// had no plan at all and was exported as `partial def`, which proves
/// nothing about its termination.
#[test]
fn a_cycle_mixing_an_int_countdown_with_a_list_descent_builds_with_a_measure() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping proof smoke test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-cycle-countdown");
    let (summary, run) = run_lean_check_json(
        "tests/fixtures/mutual_cycle_countdown_and_list.av",
        &output_dir,
        0,
        &[],
    );
    let lean = std::fs::read_to_string(output_dir.join("MutualCycleCountdownAndList.lean"))
        .expect("read the generated module");
    assert!(
        lean.contains("termination_by (Int.toNat budget + sizeOf queue, 1)")
            && lean.contains("termination_by (sizeOf queue + Int.toNat budget, 2)"),
        "the measure must count the list and the budget:\n{lean}"
    );
    assert!(
        !lean.contains("partial def"),
        "the pair must be a total definition:\n{lean}"
    );
    assert_eq!(
        (
            summary["build_errors"].as_u64(),
            summary["passed"].as_bool()
        ),
        (Some(0), Some(true)),
        "{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// A cycle with no measure: one call hands on a list that GREW, and the
/// `Int` that really bounds the recursion is spent without a guard.
///
/// The group lowers with fuel, which builds, and the claims behind it are
/// declined rather than evaluated under a seed that is not a bound — with
/// the refusal naming the call the exporter could not see shrink. Before
/// this change the refusal named only the functions.
#[test]
fn a_cycle_with_no_measure_is_declined_naming_the_call_that_fails() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping proof smoke test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-cycle-grown-list");
    let (summary, run) = run_lean_check_json(
        "tests/fixtures/mutual_cycle_grown_list.av",
        &output_dir,
        0,
        &[],
    );
    assert_eq!(
        (
            summary["build_errors"].as_u64(),
            summary["declined"].as_u64(),
            summary["sorries"].as_u64(),
        ),
        (Some(0), Some(1), Some(0)),
        "the cycle must build with fuel and decline its claim:\n{}",
        format_output(&run)
    );
    let declined = summary["declined_claims"]
        .as_array()
        .expect("declined_claims array");
    assert_eq!(declined[0]["claim"], "settle");
    let reason = declined[0]["reason"].as_str().unwrap_or_default();
    assert!(
        reason.contains(
            "the call from `pad` to `settle` passes `List.prepend(0, ys)` for `xs`, which is not a parameter of `pad` or a smaller part of one"
        ),
        "the refusal must name the call that fails: {reason}"
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// The measure of a cycle member, as emitted: every `termination_by` line of
/// the generated module, with the build summary.
fn cycle_measures(fixture: &str, module: &str, prefix: &str) -> (Vec<String>, serde_json::Value) {
    let output_dir = temp_output_dir(prefix);
    let (summary, run) = run_lean_check_json(fixture, &output_dir, 0, &[]);
    let lean = std::fs::read_to_string(output_dir.join(format!("{module}.lean")))
        .unwrap_or_else(|e| panic!("read the generated module ({e}):\n{}", format_output(&run)));
    let measures = lean
        .lines()
        .filter(|line| line.trim_start().starts_with("termination_by"))
        .map(|line| line.trim().to_string())
        .collect();
    assert!(
        !lean.contains("partial def"),
        "the group must be a total definition:\n{lean}"
    );
    let _ = std::fs::remove_dir_all(&output_dir);
    (measures, summary)
}

/// One member matches the SAME list twice, nested, and hands both tails on.
///
/// The two tails are the same part under two names, so a measure counting
/// both parameters of the callee does not decrease on that call (`omega
/// could not prove the goal … c - d ≥ 1 where c := sizeOf r1, d := sizeOf
/// a`): one build error on the binary before this change, which took the
/// binders of two patterns for disjoint parts. Parts that first differ at two
/// different patterns overlap, so the callee counts one parameter.
#[test]
fn two_matches_on_one_list_are_not_counted_twice() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping proof smoke test: `lake` not available");
        return;
    }
    let (measures, summary) = cycle_measures(
        "tests/fixtures/mutual_cycle_same_subject_twice.av",
        "MutualCycleSameSubjectTwice",
        "aver-proof-cycle-same-subject-twice",
    );
    assert_eq!(
        measures,
        vec![
            "termination_by (sizeOf ys, 1)",
            "termination_by (sizeOf xs, 2)"
        ]
    );
    assert_eq!(
        (
            summary["build_errors"].as_u64(),
            summary["passed"].as_bool()
        ),
        (Some(0), Some(true)),
        "{summary}"
    );
}

/// The same, two levels deep: the second match on the list binds a tail of a
/// tail, which overlaps the tail the first match bound. One build error on
/// the binary before this change.
#[test]
fn a_tail_of_a_tail_is_not_counted_beside_the_tail() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping proof smoke test: `lake` not available");
        return;
    }
    let (measures, summary) = cycle_measures(
        "tests/fixtures/mutual_cycle_tail_of_tail.av",
        "MutualCycleTailOfTail",
        "aver-proof-cycle-tail-of-tail",
    );
    assert_eq!(
        measures,
        vec![
            "termination_by (sizeOf ys, 1)",
            "termination_by (sizeOf xs, 2)"
        ]
    );
    assert_eq!(
        (
            summary["build_errors"].as_u64(),
            summary["passed"].as_bool()
        ),
        (Some(0), Some(true)),
        "{summary}"
    );
}

/// A cycle an `Int` counts down, on which one call hands on a list grown by
/// concatenation.
///
/// The list is not what bounds the recursion and the measure does not count
/// it; what a call passes for a parameter the measure does not count never
/// appears in a termination goal. On the binary before this change a guard
/// over every list parameter sent the group to fuel before the measure was
/// looked for, and the claims were declined with a reason naming no call.
#[test]
fn a_list_grown_on_a_call_the_measure_does_not_count_is_not_looked_at() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping proof smoke test: `lake` not available");
        return;
    }
    let (measures, summary) = cycle_measures(
        "tests/fixtures/mutual_cycle_grown_concat.av",
        "MutualCycleGrownConcat",
        "aver-proof-cycle-grown-concat",
    );
    assert_eq!(
        measures,
        vec![
            "termination_by (Int.toNat n, 2)",
            "termination_by (Int.toNat n, 1)"
        ]
    );
    assert_eq!(
        (
            summary["build_errors"].as_u64(),
            summary["declined"].as_u64().unwrap_or(0),
            summary["passed"].as_bool()
        ),
        (Some(0), 0, Some(true)),
        "nothing declined, everything proved:\n{summary}"
    );
}

/// A cycle an `Int` counts down, along which two tree parameters travel.
///
/// The measure counts both trees and the countdown; a sum over two
/// recursive types is not stated natively, so the group lowers with fuel —
/// and a fuel seed counting only the trees is no bound on the countdown. The
/// claims are declined, with the reason, rather than evaluated until the
/// fuel runs out: on the binary before this change the true claims reported
/// a panic.
#[test]
fn a_countdown_the_fuel_seed_does_not_count_is_declined_not_run_dry() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping proof smoke test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-cycle-two-trees-countdown");
    let (summary, run) = run_lean_check_json(
        "tests/fixtures/mutual_cycle_two_trees_countdown.av",
        &output_dir,
        0,
        &[],
    );
    assert_eq!(
        (
            summary["build_errors"].as_u64(),
            summary["declined"].as_u64(),
            summary["model_panicked"].as_bool(),
        ),
        (Some(0), Some(1), Some(false)),
        "the group must build with fuel and decline its claims:\n{}",
        format_output(&run)
    );
    let declined = summary["declined_claims"]
        .as_array()
        .expect("declined_claims array");
    assert_eq!(declined[0]["claim"], "rounds");
    let reason = declined[0]["reason"].as_str().unwrap_or_default();
    assert!(
        reason.contains(
            "counts a recursive type together with another parameter, which the Lean export does not state natively"
        ) && reason.contains("counting down, which the fuel seed does not count"),
        "the refusal must say why the seed is no bound: {reason}"
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// A group that lowers with fuel and declines the claims behind it: the
/// module builds, nothing panics, and the one declined claim is `claim`,
/// with a refusal that says each of `reasons`.
fn assert_cycle_declined(fixture: &str, prefix: &str, claim: &str, reasons: &[&str]) {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping proof smoke test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir(prefix);
    let (summary, run) = run_lean_check_json(fixture, &output_dir, 0, &[]);
    assert_eq!(
        (
            summary["build_errors"].as_u64(),
            summary["declined"].as_u64(),
            summary["model_panicked"].as_bool(),
        ),
        (Some(0), Some(1), Some(false)),
        "the group must build with fuel and decline its claims:\n{}",
        format_output(&run)
    );
    let declined = &summary["declined_claims"][0];
    assert_eq!(declined["claim"], claim, "{summary}");
    let reason = declined["reason"].as_str().unwrap_or_default();
    for part in reasons {
        assert!(
            reason.contains(part),
            "the refusal must say {part:?}: {reason}"
        );
    }
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// A cycle the analysis refuses: the list is regrown behind a match on a
/// computed subject, and the `Int` that drives the recursion is guarded by
/// `n == 0`, which rules out zero but not the negatives. The group lowers
/// with fuel, whose seed counts the list — no bound on a recursion as long as
/// `n`.
///
/// On the binary before this change the refusal was not consulted once the
/// group was on fuel: under `native_decide` the seed ran dry on
/// `a([1], 50) => 7` and the build reported a panic. The claims are declined
/// with the call the analysis could not see shrink.
#[test]
fn a_cycle_the_analysis_refuses_is_declined_not_run_dry() {
    assert_cycle_declined(
        "tests/fixtures/mutual_cycle_hidden_regrowth.av",
        "aver-proof-cycle-hidden-regrowth",
        "a",
        &[
            "the call from `b` to `a` passes `u` for `xs`, which is not a parameter of `b` or a smaller part of one",
        ],
    );
}

/// A self-recursive helper receives one tail twice and hands the second on
/// through an alias, so the group makes quadratically many calls on a seed
/// that counts the cells once. The two arguments are the same part under two
/// names, so no measure counts both; on the binary before this change the
/// seed ran dry on the 24-element claim and the build reported a panic.
#[test]
fn a_cycle_through_a_helper_that_takes_one_tail_twice_is_declined_not_run_dry() {
    assert_cycle_declined(
        "tests/fixtures/mutual_cycle_quadratic_helper.av",
        "aver-proof-cycle-quadratic-helper",
        "outer",
        &[
            "the call from `outer` to `helper` passes `t` for `b`, which overlaps the `t` it passes for `a`",
        ],
    );
}

/// Two trees travel a cycle whose measure counts both — a sum the Lean
/// export does not state natively — and one call hands a list grown by
/// concatenation into a position the fuel seed counts. The claims are
/// declined, and the refusal names that call; on the binary before this
/// change it named only the member and the back-off.
#[test]
fn a_decline_for_a_computed_value_on_a_counted_position_names_the_call() {
    assert_cycle_declined(
        "tests/fixtures/mutual_cycle_two_trees_grown_list.av",
        "aver-proof-cycle-two-trees-grown-list",
        "f",
        &[
            "the measure of `f` counts a recursive type together with another parameter, which the Lean export does not state natively",
            "the call from `g` to `f` passes `List.concat(xs, [1])` for `xs`",
        ],
    );
}

/// The Dafny export measures a recursion group by the length of every
/// sequence parameter. The countdown-and-list cycle is planned by a measure
/// that counts the budget too, so the plan's ordering of the members is not
/// an ordering of the calls that leave `|queue|` unchanged: paired with it,
/// Dafny reported `decreases clause might not decrease`. A rank chosen for
/// a measure Dafny does not state is not handed to it; the group lowers with
/// fuel, which verifies. Before the group had a plan at all, the plain
/// functions reported two termination errors.
#[test]
fn dafny_does_not_pair_its_measure_with_a_rank_chosen_for_another() {
    assert_dafny_verifies_and_passes(
        "tests/fixtures/mutual_cycle_countdown_and_list.av",
        "aver-dafny-cycle-countdown",
    );
}

/// A self-recursive helper receives the head and the tail of a list of
/// lists. The head is a smaller part of the list by size, which is what the
/// Lean measure counts, but not by length, which is what Dafny's
/// `decreases |a| + |b|` measures: paired with the ordering chosen for the
/// size measure, Dafny reported `decreases clause might not decrease` on the
/// binary before this change. The analysis run for Dafny takes only a tail
/// for a part, finds no measure, and the group lowers with fuel, which
/// verifies.
#[test]
fn dafny_does_not_take_a_list_head_for_a_shorter_list() {
    assert_dafny_verifies_and_passes(
        "tests/fixtures/mutual_cycle_head_and_tail.av",
        "aver-dafny-cycle-head-and-tail",
    );
}

/// `List.take(List.drop(xs, 1), 32)` and two chains of three list operations,
/// each the next one's receiver.
///
/// `List.drop` emits `receiver.drop n`; as the receiver of `.take` that
/// application went out unwrapped — it begins with `(` — and Lean attached
/// `.take` to its last argument: `Invalid field take: The environment does
/// not contain Nat.take`. Nine build errors on the binary before this change.
#[test]
fn a_method_application_as_a_receiver_is_parenthesised() {
    assert_proof_builds(
        "tests/fixtures/lean_chained_receivers.av",
        "aver-proof-chained-receivers",
    );
}

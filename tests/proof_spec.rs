use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

#[path = "proof_spec/builds.rs"]
mod builds;
#[path = "proof_spec/check_gates.rs"]
mod check_gates;
#[path = "proof_spec/dafny_inline.rs"]
mod dafny_inline;
#[path = "proof_spec/discover.rs"]
mod discover;
#[path = "proof_spec/export_structure.rs"]
mod export_structure;
#[path = "proof_spec/lean_kernel.rs"]
mod lean_kernel;
#[path = "proof_spec/lemmas.rs"]
mod lemmas;
#[path = "proof_spec/literalization.rs"]
mod literalization;
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
/// the raw output for diagnostics. Shared by the when-universal
/// quarantine-lane tests, which need several runs against the SAME
/// output dir (lake's content-addressed cache keeps re-runs cheap).
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

/// The COUNTED summary — everything the budget pins and run.sh
/// consumers key on — with the lane's additive field stripped, for
/// byte-level comparison across lane-on / lane-sabotaged / lane-off
/// runs. The iron guard's observable form: the lane may only ever
/// append, never perturb.
fn counted_summary(summary: &serde_json::Value) -> serde_json::Value {
    let mut obj = summary.as_object().cloned().unwrap_or_default();
    obj.remove("when_universal");
    serde_json::Value::Object(obj)
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

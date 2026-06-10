use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

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

    let _ = std::fs::remove_dir_all(&output_dir);
}

#[test]
fn proof_export_builds_law_auto_when_lake_is_available() {
    assert_proof_builds("examples/formal/law_auto.av", "aver-proof-smoke");
}

#[test]
fn proof_export_builds_fibonacci_when_lake_is_available() {
    assert_proof_builds("examples/data/fibonacci.av", "aver-proof-fibonacci");
}

#[test]
fn proof_dafny_verifies_fibonacci_when_dafny_is_available() {
    // `goldenApprox(n)` divides `Float.fromInt(fib(n + 1))` by
    // `Float.fromInt(fib(n))`. Float `/` lowers via the `FloatDiv`
    // helper which mirrors Aver's IEEE-754 "no crash, b == 0 yields
    // a defined value" semantics, so there's no division-by-zero
    // obligation on the caller — the rest of the proof closes.
    assert_dafny_verifies("examples/data/fibonacci.av", "aver-dafny-fibonacci");
}

#[test]
fn proof_dafny_verifies_sum_acc_when_dafny_is_available() {
    // Stage 8 of #232: `ProofStrategy::WrapperOverRecursion` closes
    // the `sum(xs) == sumDirect(xs)` law on the `sum_acc` example by
    // emitting an accumulator-decomposition aux lemma plus the main
    // universal lemma. Both close in Z3 via list induction —
    // demonstrates the first real consumer of the
    // `analysis::shape::ModulePattern::WrapperOverRecursion` typed
    // pattern. Regression guard: if a future change disables the
    // strategy or breaks the aux template, this lemma falls back to
    // naive induction and Dafny reports 1 error.
    assert_dafny_verifies("examples/data/sum_acc.av", "aver-dafny-sum-acc");
}

#[test]
fn proof_export_builds_sum_acc_when_lake_is_available() {
    // Lean template for `WrapperOverRecursion` emits the aux
    // accumulator-decomposition theorem + main universal lemma; both
    // close in core Lean 4 (`omega`) without Mathlib. Sorry budget
    // 0 — the strategy fully closes the universal proof.
    assert_proof_builds_with_sorry_budget("examples/data/sum_acc.av", "aver-proof-sum-acc", 0);
}

#[test]
fn proof_dafny_verifies_list_length_fold_when_dafny_is_available() {
    // Stage 8c of #232: `ProofStrategy::MatchDispatcherFold` — two
    // structural list folds (`1 + length(t)` vs `length(t) + 1`)
    // closing by induction on `xs`. Dafny's default Induction path
    // already verifies this shape; the explicit strategy makes the
    // recognition observable in proof_ir.
    assert_dafny_verifies(
        "examples/data/list_length_fold.av",
        "aver-dafny-list-length-fold",
    );
}

#[test]
fn proof_export_builds_list_length_fold_when_lake_is_available() {
    // Lean template: `induction xs with | nil => simp | cons => simp;
    // omega`. The omega discharge handles `1 + x = x + 1`.
    assert_proof_builds_with_sorry_budget(
        "examples/data/list_length_fold.av",
        "aver-proof-list-length-fold",
        0,
    );
}

#[test]
fn proof_dafny_verifies_result_chain_when_dafny_is_available() {
    // Stage 8b of #232: `ProofStrategy::ResultPipelineChain` closes
    // `chainQM(n) == chainManual(n)` — `?`-propagating Result chain
    // vs nested `match Result.Err -> Err` chain. Both unfold to the
    // same tree; Z3 closes by structural equality with the right
    // fuel + unfold list. Second real consumer of a typed
    // `ModulePattern` in proof_lower.
    assert_dafny_verifies("examples/core/result_chain.av", "aver-dafny-result-chain");
}

#[test]
fn proof_export_builds_result_chain_when_lake_is_available() {
    // Lean template: `unfold` + `repeat (first | split | rfl) ;
    // all_goals simp_all`. Generic over step count — works for any
    // ResultPipelineChain with arbitrary number of step fns.
    assert_proof_builds_with_sorry_budget(
        "examples/core/result_chain.av",
        "aver-proof-result-chain",
        0,
    );
}

#[test]
fn proof_export_builds_rle_when_lake_is_available() {
    // The encode/decode roundtrip laws are list-given, so #409 attempts Lean
    // list-induction — but `encode` threads an accumulator (`encodeLoop`) so a
    // plain `induction xs` IH does not align; both roundtrips fall to an honest
    // `sorry` (per-arm `first | (simp_all; done) | sorry`, which BUILDS). The
    // earlier #409 revision claimed 1 here, a false green — the tactic left
    // unsolved goals that `lake build` rejects but the sorry-count metric was
    // blind to (fixed in commands.rs to gate on lake's exit status).
    assert_proof_builds_with_sorry_budget("examples/data/rle.av", "aver-proof-rle", 2);
}

#[test]
fn proof_dafny_verifies_rle_when_dafny_is_available() {
    // Three postcondition gaps on the encode/decode roundtrip shape
    // (one universal lemma, one sample assertion, one
    // `decodeString` universal). Z3 can't auto-discharge them
    // without a richer list-induction tactic the lowerer doesn't
    // emit yet. Tracked in issue #114.
    assert_dafny_verifies_with_budgets("examples/data/rle.av", "aver-dafny-rle", 3, 0);
}

#[test]
fn proof_export_builds_quicksort_when_lake_is_available() {
    // `sort` / `sortWithPivot` now emit as a genuine well-founded `mutual`
    // block: the computed-arg partition recursion's termination is discharged
    // by synthesised, kernel-proved `smallerOrEqual_len_le` /
    // `greaterThan_len_le` length-monotonicity lemmas (termination-as-a-law,
    // no fuel / no `partial` / `#print axioms` = [propext, Quot.sound]).
    // The three list-given BEHAVIORAL laws (`sort.resultOrdered` /
    // `sort.lengthPreserved` / `sort.idempotent`) are attempted via #409 Lean
    // list-induction, but `simp_all` cannot reduce the partition recursion, so
    // each still falls to an honest `sorry` that BUILDS (per-arm
    // `first | (simp_all; done) | sorry`). An earlier #409 revision reported 0
    // here — a FALSE GREEN: the tactic left unsolved goals that `lake build`
    // rejects (exit 1), which the `declaration uses 'sorry'` count metric was
    // blind to. Fixed: commands.rs gates pass on lake's exit status, so this
    // budget is now the build-verified count. (Native universal closure for the
    // partition SCC is the #125 native-decreases epic, not reachable here.)
    assert_proof_builds_with_sorry_budget("examples/data/quicksort.av", "aver-proof-quicksort", 3);
}

#[test]
fn proof_dafny_verifies_quicksort_when_dafny_is_available() {
    // Recursive postcondition gaps on `sort.resultOrdered` /
    // `sort.lengthPreserved` / `sort.idempotent`. Sample-domain
    // theorems still hold for ordered/length-preserved; idempotent
    // sample assertions trip the same mutual-recursion / fuel issue
    // tracked in #76 — sort(sort([..])) cannot unfold under Z3's
    // budget without explicit `reveal`. Budget grew from 5 → 8 when
    // `sort.idempotent` landed in #220 (three sample inputs ×
    // one postcondition each). Tracked in issue #114 / #76. The budget
    // is a CEILING: macOS Z3 leaves 8 undischarged, Linux CI 9 (one extra
    // assertion whose counterexample model Z3 can't parse, #342).
    assert_dafny_verifies_with_budgets("examples/data/quicksort.av", "aver-dafny-quicksort", 9, 3);
}

#[test]
fn proof_export_builds_json_when_lake_is_available() {
    // 13 sampled-domain laws (parseString / parseLiteral / escape
    // roundtrips) hit the universal-not-auto-proved fallback in
    // `lean::toplevel` and emit `theorem ... := by sorry`. The
    // per-sample `_sample_N` theorems below them still verify the
    // claim on the declared domain — those are the meaningful
    // coverage. Budget gates regressions: if the count climbs, a
    // new shape lost a strategy and broke the law; if it drops,
    // someone gave one of these a real strategy and the budget
    // should be tightened.
    // Issue #128: dropped from 13 to 9 — the new singleton-given +
    // constant-RHS gate elides 4 universal-with-sorry shapes whose
    // ∀ form was vacuous (or false). Per-sample lemmas still cover
    // the declared domain.
    assert_proof_builds_with_sorry_budget("examples/data/json.av", "aver-proof-json", 8);
}

#[test]
fn proof_dafny_verifies_json_when_dafny_is_available() {
    // Structural shape limits: deeply-nested ADT roundtrip
    // postconditions blow past what Dafny can auto-discharge. The
    // large budget exists so a regression *upward* is still caught;
    // closing this cleanly is probably out of scope for a single
    // fix per issue #114, and would need a different proof
    // strategy entirely.
    assert_dafny_verifies_with_budgets("examples/data/json.av", "aver-dafny-json", 89, 16);
}

#[test]
fn proof_dafny_check_verifies_entry_module_not_arbitrary_dependency() {
    // Regression: `--check` must verify the ENTRY module (which carries the
    // verify-law lemmas), not whatever `.dfy` a directory scan yields first.
    // The dependency module here (`Aaa`) sorts before the entry (`Zzz`) and
    // does NOT include it, so a naive `read_dir().find()` verifies `Aaa.dfy`
    // and never checks `Zzz`'s deliberately-false law → false-green.
    if Command::new("dafny").arg("--version").output().is_err() {
        eprintln!("skipping dafny entry-selection test: `dafny` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-mm-entry-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(
        src.join("aaa.av"),
        "module Aaa\n    depends []\n\nfn ident(n: Int) -> Int\n    ? \"id\"\n    n\n\n\
         verify ident law refl\n    given n: Int = -1..1\n    ident(n) => n\n",
    )
    .expect("write aaa.av");
    std::fs::write(
        src.join("zzz.av"),
        "module Zzz\n    depends [Aaa]\n    effects [Console.print]\n\n\
         fn wrong(n: Int) -> Int\n    ? \"doubles; the law lies\"\n    Aaa.ident(n) + n\n\n\
         verify wrong law falseRefl\n    given n: Int = -1..1\n    wrong(n) => n\n\n\
         fn main() -> Unit\n    ! [Console.print]\n    Console.print(\"mm\")\n",
    )
    .expect("write zzz.av");
    let out = temp_output_dir("aver-mm-entry-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("zzz.av"))
        .arg("--backend")
        .arg("dafny")
        .arg("--module-root")
        .arg(&src)
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check --check-json` to run");
    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)));
    let summary: serde_json::Value =
        serde_json::from_str(json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}"));
    assert_eq!(
        summary["passed"].as_bool(),
        Some(false),
        "entry `Zzz`'s false law `wrong(n) => n` must be caught — `--check` must \
         verify the ENTRY module, not an arbitrary dependency.\n{}",
        format_output(&run)
    );
    assert!(
        summary["errors"].as_u64().unwrap_or(0) >= 1,
        "expected >=1 Dafny error from the false entry law\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

/// Write `source` to a temp `.av`, run `aver proof --backend dafny --check
/// --check-json`, and assert the law's universal closed for real: passed,
/// with no Dafny errors, no trusted axioms, and no dropped (sample-only)
/// universal. Used to pin the Dafny homomorphism strategies.
fn assert_dafny_proves_inline(source: &str, prefix: &str) {
    if Command::new("dafny").arg("--version").output().is_err() {
        eprintln!("skipping dafny proof test ({prefix}): `dafny` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir(&format!("{prefix}-src"));
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(src.join("m.av"), source).expect("write m.av");
    let out = temp_output_dir(&format!("{prefix}-out"));
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("m.av"))
        .arg("--backend")
        .arg("dafny")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check --check-json` to run");
    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)));
    let summary: serde_json::Value =
        serde_json::from_str(json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}"));
    assert_eq!(
        (
            summary["passed"].as_bool(),
            summary["errors"].as_u64(),
            summary["axioms"].as_u64(),
            summary["omitted"].as_u64(),
        ),
        (Some(true), Some(0), Some(0), Some(0)),
        "{prefix}: law must close as a real ∀ proof (passed, 0 errors, 0 \
         axioms, 0 omitted).\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_dafny_proves_concat_fold_homomorphism() {
    // The list-induction emitter supplies cons-decomposition bridge asserts
    // for a fold over `concat(<ind-var>, ys)` (here `count`), which is what
    // lets Z3 close `count(n, xs ++ ys) == plus(count n xs, count n ys)` —
    // a goal it times out on without the head/tail hint. Generic over any
    // left-concat (builtin `List.concat` and user wrappers).
    assert_dafny_proves_inline(
        "module ConcatHom\n    effects []\n\n\
         type Nat\n    Z\n    S(Nat)\n\n\
         fn eqNat(a: Nat, b: Nat) -> Bool\n    match a\n        Nat.Z -> match b\n            Nat.Z -> true\n            Nat.S(w) -> false\n        Nat.S(p) -> match b\n            Nat.Z -> false\n            Nat.S(q) -> eqNat(p, q)\n\n\
         fn count(n: Nat, xs: List<Nat>) -> Nat\n    match xs\n        [] -> Nat.Z\n        [h, ..t] -> match eqNat(n, h)\n            true -> Nat.S(count(n, t))\n            false -> count(n, t)\n\n\
         fn plus(a: Nat, b: Nat) -> Nat\n    match a\n        Nat.Z -> b\n        Nat.S(z) -> Nat.S(plus(z, b))\n\n\
         verify count law countConcat\n    given n: Nat = [Nat.Z]\n    given xs: List<Nat> = [[Nat.Z]]\n    given ys: List<Nat> = [[Nat.Z]]\n    plus(count(n, xs), count(n, ys)) => count(n, List.concat(xs, ys))\n",
        "aver-concat-hom",
    );
}

#[test]
fn proof_dafny_proves_additive_monoid_homomorphism() {
    // When the induction variable lands in an additive op's SECOND argument
    // (`plus(length y, length x)`), the emitter hoists the op's right-identity
    // and succ-shift lemmas to quantified facts so Z3 closes the homomorphism.
    // Generic over any additive op / Peano-shaped codomain; the helper lemmas
    // are proved, not trusted.
    assert_dafny_proves_inline(
        "module AddLift\n    effects []\n\n\
         type Nat\n    Z\n    S(Nat)\n\n\
         fn length(xs: List<Int>) -> Nat\n    match xs\n        [] -> Nat.Z\n        [h, ..t] -> Nat.S(length(t))\n\n\
         fn plus(a: Nat, b: Nat) -> Nat\n    match a\n        Nat.Z -> b\n        Nat.S(z) -> Nat.S(plus(z, b))\n\n\
         fn append(xs: List<Int>, ys: List<Int>) -> List<Int>\n    match xs\n        [] -> ys\n        [h, ..t] -> List.concat([h], append(t, ys))\n\n\
         verify length law lenAppend\n    given x: List<Int> = [[1]]\n    given y: List<Int> = [[2]]\n    length(append(x, y)) => plus(length(y), length(x))\n",
        "aver-add-lift",
    );
}

#[test]
fn proof_dafny_proves_length_snoc_with_evaluable_samples() {
    // Two things in one: (1) the `length-snoc` strategy — for a list-length
    // fold the emitter hoists `length(s ++ [e]) == S(length s)` to a ∀-fact,
    // which directly closes the snoc law; (2) the sample-fuel fix — the
    // concrete samples (`length([1, 2, 3]) == S(length([1, 2]))`) only verify
    // because the sample method now carries the same `{:fuel length, 5}` the
    // universal lemma gets (a `function` with `decreases` does not unfold in a
    // bare `assert` otherwise, so the sample would spuriously fail while the
    // universal proves). `passed && axioms:0 && omitted:0` covers both.
    assert_dafny_proves_inline(
        "module LenSnoc\n    effects []\n\n\
         type Nat\n    Z\n    S(Nat)\n\n\
         fn length(x: List<Int>) -> Nat\n    match x\n        [] -> Nat.Z\n        [y, ..xs] -> Nat.S(length(xs))\n\n\
         verify length law snoc\n    given xs: List<Int> = [[1, 2]]\n    given y: Int = [3]\n    length(List.concat(xs, [y])) => Nat.S(length(xs))\n",
        "aver-len-snoc",
    );
}

#[test]
fn proof_dafny_proves_rev_antihomomorphism() {
    // `rev (rev x) = x` needs the rev anti-homomorphism `rev(a ++ b) =
    // rev b ++ rev a` as an auxiliary lemma. The emitter detects the
    // rev/append fold pair, emits the proved append-nil-right /
    // associativity / rev-distribution lemmas, hoists the distribution to a
    // ∀-fact, and adds the per-step cons bridges. Generic over any
    // reverse-via-left-append fold.
    assert_dafny_proves_inline(
        "module RevHom\n    effects []\n\n\
         fn append(x: List<Int>, y: List<Int>) -> List<Int>\n    match x\n        [] -> y\n        [z, ..xs] -> List.concat([z], append(xs, y))\n\n\
         fn rev(x: List<Int>) -> List<Int>\n    match x\n        [] -> []\n        [y, ..xs] -> append(rev(xs), [y])\n\n\
         verify rev law revRev\n    given x: List<Int> = [[1, 2]]\n    rev(rev(x)) => x\n",
        "aver-rev-antihom",
    );
}

#[test]
fn proof_lean_peano_lift_nat_arith_kernel_clean() {
    // Proof-only Peano representation lift: a canonical `type Nat { Z; S(Nat) }`
    // is emitted as Lean's builtin `Nat` (no `inductive`, `Z`→`0`, `S(x)`→`x+1`,
    // structural recursion not fuel), so `omega`/`simp` close the nat-arithmetic.
    // `minus(n, plus(n, m)) == 0` then kernel-proves as a genuine UNBOUNDED
    // universal — `#print axioms = [propext]`, not the bounded `native_decide`
    // fallback. We pin the lift mechanics (no `inductive Nat`, no `__fuel`) AND
    // a clean pass, which together imply the structural-Nat proof.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lean peano-lift test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-peano-lift-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(
        src.join("m.av"),
        "module PeanoArith\n    effects []\n\n\
         type Nat\n    Z\n    S(Nat)\n\n\
         fn plus(x: Nat, y: Nat) -> Nat\n    match x\n        Nat.Z -> y\n        Nat.S(z) -> Nat.S(plus(z, y))\n\n\
         fn minus(x: Nat, y: Nat) -> Nat\n    match x\n        Nat.Z -> Nat.Z\n        Nat.S(a) -> match y\n            Nat.Z -> x\n            Nat.S(b) -> minus(a, b)\n\n\
         verify minus law cancel\n    given n: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    given m: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    minus(n, plus(n, m)) => Nat.Z\n",
    )
    .expect("write m.av");
    let out = temp_output_dir("aver-peano-lift-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("m.av"))
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check --check-json` to run");
    let lean = std::fs::read_to_string(out.join("PeanoArith.lean")).expect("read PeanoArith.lean");
    assert!(
        !lean.contains("inductive Nat") && !lean.contains("__fuel"),
        "the Peano type must lift to builtin Nat (no `inductive Nat`, no fuel):\n{lean}"
    );
    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)));
    let summary: serde_json::Value =
        serde_json::from_str(json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}"));
    assert_eq!(
        (
            summary["passed"].as_bool(),
            summary["sorries"].as_u64(),
            summary["universal"].as_bool(),
        ),
        (Some(true), Some(0), Some(true)),
        "Peano nat-arithmetic must kernel-prove on Lean via the lift as a GENUINE \
         universal — `--check-json` `universal:true` means `#print axioms` is \
         `ofReduceBool`-free (not a bounded `native_decide`).\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_lean_proves_peano_arith_identity_via_nat_lift_kernel_clean() {
    // Layer-2 of the Peano lift (#3): recognize the canonical `plus` (left-
    // recursive addition) and `minus` (truncated subtraction) and emit a
    // kernel-CHECKED bridge `op a b = a + b` / `a - b` (proved by induction on
    // the lifted builtin `Nat`). Rewriting the user ops to the host builtins
    // hands `(n+m)-n = m` to `omega`, which decides linear Nat arithmetic with
    // truncated subtraction — closing a pure-arithmetic identity that bare
    // structural induction leaves at `sorry`. The bridge is PROVED not trusted:
    // a misrecognized op fails its bridge proof (honest `sorry`), never a false
    // theorem. Result is a GENUINE universal (`universal:true`,
    // `#print axioms`-clean of `ofReduceBool`). (TIP isaplanner prop_07.)
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lean peano-arith test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-peano-arith-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(
        src.join("m.av"),
        "module PeanoArithLift\n    effects []\n\n\
         type Nat\n    Z\n    S(Nat)\n\n\
         fn minus(x: Nat, y: Nat) -> Nat\n    match x\n        Nat.Z -> Nat.Z\n        Nat.S(z) -> match y\n            Nat.Z -> x\n            Nat.S(x2) -> minus(z, x2)\n\n\
         fn plus(x: Nat, y: Nat) -> Nat\n    match x\n        Nat.Z -> y\n        Nat.S(z) -> Nat.S(plus(z, y))\n\n\
         verify minus law plusMinusCancel\n    given n: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    given m: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    minus(plus(n, m), n) => m\n",
    )
    .expect("write m.av");
    let out = temp_output_dir("aver-peano-arith-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("m.av"))
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check --check-json` to run");
    // Both arithmetic bridges must be emitted (the `minus` truncated-subtraction
    // recognizer reaches through the TCO'd tail self-call).
    let lean =
        std::fs::read_to_string(out.join("PeanoArithLift.lean")).expect("read PeanoArithLift.lean");
    assert!(
        lean.contains("_plus_isNatAdd") && lean.contains("_minus_isNatSub"),
        "both the `plus`→`+` and `minus`→`-` bridges must be emitted:\n{lean}"
    );
    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)));
    let summary: serde_json::Value =
        serde_json::from_str(json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}"));
    assert_eq!(
        (
            summary["passed"].as_bool(),
            summary["sorries"].as_u64(),
            summary["universal"].as_bool(),
        ),
        (Some(true), Some(0), Some(true)),
        "`(n+m)-n=m` must kernel-prove as a GENUINE universal via the plus/minus \
         Nat-arithmetic bridges + omega (passed, 0 sorries, universal:true).\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_lean_proves_comparison_lift_le_and_lt_kernel_clean() {
    // Comparison half of the canonical Peano family (#3 completion): `le`/`lt`
    // (Bool-returning `≤`/`<`) lift via a kernel-proved Prop-equality bridge
    // `(op a b = true) = (a R b)`, turning the Bool law goal into a Prop that
    // `omega` closes. `lt` matches its SECOND arg first (the bridge inducts on
    // `b`). Pins the two committed corpus instances that were Lean-open before:
    // prop_69 `n ≤ m+n` and prop_65 `i < S(m+i)`. Both must be GENUINE
    // universals (`universal:true`, `#print axioms` free of ofReduceBool).
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lean comparison-lift test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    for (file, op) in [
        ("proof-corpus/tip/isaplanner/prop_69.av", "le (≤)"),
        ("proof-corpus/tip/isaplanner/prop_65.av", "lt (<)"),
    ] {
        let out = temp_output_dir("aver-cmp-lift-out");
        let run = Command::new(aver_bin)
            .current_dir(&repo_root)
            .arg("proof")
            .arg(file)
            .arg("--backend")
            .arg("lean")
            .arg("-o")
            .arg(&out)
            .arg("--check")
            .arg("--check-json")
            .output()
            .expect("expected `aver proof --check --check-json` to run");
        let json_line = run
            .stdout
            .split(|&b| b == b'\n')
            .rev()
            .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
            .unwrap_or_else(|| panic!("{op}: no JSON line:\n{}", format_output(&run)));
        let summary: serde_json::Value = serde_json::from_str(json_line)
            .unwrap_or_else(|e| panic!("{op}: bad JSON ({e}):\n{json_line}"));
        assert_eq!(
            (summary["passed"].as_bool(), summary["universal"].as_bool()),
            (Some(true), Some(true)),
            "{op} comparison law must kernel-prove as a GENUINE universal via the \
             `(op a b = true) = (a R b)` bridge + omega.\n{}",
            format_output(&run)
        );
        let _ = std::fs::remove_dir_all(&out);
    }
}

#[test]
fn proof_lean_proves_mul_distributivity_via_nat_lift_kernel_clean() {
    // `*` member of the family. `times` lifts to builtin `*` via a kernel-proved
    // bridge `times a b = a * b` (whose succ case uses the `+` bridge). `*` is
    // nonlinear — omega can't and core Lean has no `ring` — so distributivity
    // closes via core `Nat.mul_add` after the bridges rewrite. GENUINE universal.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lean mul-lift test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-mul-lift-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(
        src.join("m.av"),
        "module MulDist\n    effects []\n\n\
         type Nat\n    Z\n    S(Nat)\n\n\
         fn plus(x: Nat, y: Nat) -> Nat\n    match x\n        Nat.Z -> y\n        Nat.S(z) -> Nat.S(plus(z, y))\n\n\
         fn times(x: Nat, y: Nat) -> Nat\n    match x\n        Nat.Z -> Nat.Z\n        Nat.S(z) -> plus(y, times(z, y))\n\n\
         verify times law leftDistrib\n    given a: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    given b: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    given c: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    times(a, plus(b, c)) => plus(times(a, b), times(a, c))\n",
    )
    .expect("write m.av");
    let out = temp_output_dir("aver-mul-lift-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("m.av"))
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check --check-json` to run");
    let lean = std::fs::read_to_string(out.join("MulDist.lean")).expect("read MulDist.lean");
    assert!(
        lean.contains("_times_isNatMul") && lean.contains("_plus_isNatAdd"),
        "the `*` bridge (and its prerequisite `+` bridge) must be emitted:\n{lean}"
    );
    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)));
    let summary: serde_json::Value =
        serde_json::from_str(json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}"));
    assert_eq!(
        (
            summary["passed"].as_bool(),
            summary["sorries"].as_u64(),
            summary["universal"].as_bool(),
        ),
        (Some(true), Some(0), Some(true)),
        "left-distributivity `a*(b+c) = a*b + a*c` must kernel-prove as a GENUINE \
         universal via the times/plus bridges + Nat.mul_add.\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_lean_rejects_noncanonical_peano_ops_no_bridge() {
    // NEGATIVE test (the soundness gate the reviewer flagged as missing): the
    // arithmetic/comparison recognizers key on SHAPE, so a lookalike that is NOT
    // the canonical operation must NOT get a bridge. `addTwo` adds TWO per step
    // (`2a+b`, not `a+b`); `weirdCmp` ignores its second arg (not `≤`/`<`).
    // Neither is a canonical Peano op, so NO `_isNat{Add,Sub,Mul,Le,Lt}` bridge
    // may be emitted — if one were, its kernel proof would be a false claim.
    // (The bridge is also kernel-checked, so even a hypothetical misfire could
    // not mint a theorem; this pins the recognizer's conservativeness directly.)
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lean negative-recognizer test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-noncanon-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(
        src.join("m.av"),
        "module NonCanon\n    effects []\n\n\
         type Nat\n    Z\n    S(Nat)\n\n\
         fn addTwo(x: Nat, y: Nat) -> Nat\n    match x\n        Nat.Z -> y\n        Nat.S(z) -> Nat.S(Nat.S(addTwo(z, y)))\n\n\
         fn weirdCmp(x: Nat, y: Nat) -> Bool\n    match x\n        Nat.Z -> true\n        Nat.S(z) -> weirdCmp(z, y)\n\n\
         verify addTwo law selfEq\n    given a: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    given b: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    addTwo(a, b) => addTwo(a, b)\n",
    )
    .expect("write m.av");
    let out = temp_output_dir("aver-noncanon-out");
    let _ = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("m.av"))
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .output()
        .expect("expected `aver proof` to run");
    let lean = std::fs::read_to_string(out.join("NonCanon.lean")).expect("read NonCanon.lean");
    for marker in [
        "_isNatAdd",
        "_isNatSub",
        "_isNatMul",
        "_isNatLe",
        "_isNatLt",
    ] {
        assert!(
            !lean.contains(marker),
            "a non-canonical op must NOT get the `{marker}` bridge (recognizer must \
             reject lookalike shapes):\n{lean}"
        );
    }
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_lean_proves_count_plus_concat_homomorphism_kernel_clean() {
    // Induction-target selection (the generic fix behind #1): a list-
    // homomorphism `plus (count n xs) (count n ys) = count n (xs ++ ys)` has
    // BOTH a Nat given (`n`) and List givens. Inducting on `n` — which the old
    // "first recursive-typed given" rule did — gets nowhere (`count` recurses
    // on the LIST, not on `n`) and falls to `sorry`. law_auto now routes
    // induction to the variable the VERIFIED fn structurally recurses on, so
    // it inducts on `xs`. The cons arm then needs the inner `match eqNat n
    // head` peeled: the `split`-based ladder branch case-splits the symbolic
    // Bool scrutinee and closes both arms with the IH + `omega`. The result is
    // a GENUINE universal (`#print axioms = [propext]`, `universal:true`), not
    // a bounded `native_decide`. (TIP isaplanner prop_02.)
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lean count-homomorphism test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-count-hom-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(
        src.join("m.av"),
        "module CountHom\n    effects []\n\n\
         type Nat\n    Z\n    S(Nat)\n\n\
         fn eqNat(x: Nat, y: Nat) -> Bool\n    match x\n        Nat.Z -> match y\n            Nat.Z -> true\n            Nat.S(z) -> false\n        Nat.S(x2) -> match y\n            Nat.Z -> false\n            Nat.S(y2) -> eqNat(x2, y2)\n\n\
         fn count(x: Nat, y: List<Nat>) -> Nat\n    match y\n        [] -> Nat.Z\n        [z, ..ys] -> match eqNat(x, z)\n            true -> Nat.S(count(x, ys))\n            false -> count(x, ys)\n\n\
         fn plus(x: Nat, y: Nat) -> Nat\n    match x\n        Nat.Z -> y\n        Nat.S(z) -> Nat.S(plus(z, y))\n\n\
         fn appendNat(xs: List<Nat>, ys: List<Nat>) -> List<Nat>\n    List.concat(xs, ys)\n\n\
         verify count law countPlusConcat\n    given n: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    given xs: List<Nat> = [[], [Nat.Z]]\n    given ys: List<Nat> = [[], [Nat.S(Nat.Z)]]\n    plus(count(n, xs), count(n, ys)) => count(n, appendNat(xs, ys))\n",
    )
    .expect("write m.av");
    let out = temp_output_dir("aver-count-hom-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("m.av"))
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check --check-json` to run");
    // The induction must target the LIST, not the Nat given.
    let lean = std::fs::read_to_string(out.join("CountHom.lean")).expect("read CountHom.lean");
    assert!(
        lean.contains("induction xs with"),
        "count homomorphism must induct on the list given `xs` (the var `count` \
         recurses on), not the Nat given `n`:\n{lean}"
    );
    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)));
    let summary: serde_json::Value =
        serde_json::from_str(json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}"));
    assert_eq!(
        (
            summary["passed"].as_bool(),
            summary["sorries"].as_u64(),
            summary["universal"].as_bool(),
        ),
        (Some(true), Some(0), Some(true)),
        "count/++ homomorphism must kernel-prove as a GENUINE universal via \
         list-induction on `xs` + the inner-match `split` (passed, 0 sorries, \
         universal:true).\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_lean_proves_rev_antihomomorphism_kernel_clean() {
    // SAME backend-neutral `RevOp` recognizer as the Dafny test above, but a
    // Lean renderer: `rev (rev x) = x` on List<Int> kernel-proves because the
    // fold lowers to a clean `def … termination_by` (no fuel / no Nat
    // collision). The renderer prepends the proved append-nil-right /
    // associativity / rev-distribution theorems and adds rev-distribution to
    // the list-induction simp set. `lake build` succeeds with ZERO sorries on
    // the universal, i.e. it is kernel-checked (`#print axioms = [propext]`).
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lean rev kernel test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-rev-lean-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(
        src.join("m.av"),
        "module RevHomLean\n    effects []\n\n\
         fn append(x: List<Int>, y: List<Int>) -> List<Int>\n    match x\n        [] -> y\n        [z, ..xs] -> List.concat([z], append(xs, y))\n\n\
         fn rev(x: List<Int>) -> List<Int>\n    match x\n        [] -> []\n        [y, ..xs] -> append(rev(xs), [y])\n\n\
         verify rev law revRev\n    given x: List<Int> = [[1, 2]]\n    rev(rev(x)) => x\n",
    )
    .expect("write m.av");
    let out = temp_output_dir("aver-rev-lean-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("m.av"))
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check --check-json` to run");
    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)));
    let summary: serde_json::Value =
        serde_json::from_str(json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}"));
    assert_eq!(
        (
            summary["passed"].as_bool(),
            summary["sorries"].as_u64(),
            summary["universal"].as_bool(),
        ),
        (Some(true), Some(0), Some(true)),
        "rev∘rev must kernel-prove on Lean via the shared recognizer as a GENUINE \
         universal (passed, 0 sorries, `universal:true` = `#print axioms` is \
         `ofReduceBool`-free).\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_check_lean_universal_field_distinguishes_bounded_from_genuine() {
    // The honest-coverage gate behind `--check-json` `universal`. Lean's
    // `passed` is deliberately lenient: a law the auto-prover cannot close by
    // genuine induction still emits a finite domain-guarded `∀ … -> …` proved
    // by `native_decide`, which `lake build` accepts (passed:true, 0 sorries) —
    // a legitimate-but-weaker bounded verify-on-domain. That bounded proof
    // depends on `Lean.ofReduceBool` (the kernel trusting the compiler's
    // evaluation over the concrete domain), NOT the universal claim, so
    // `#print axioms` exposes it. `universal` must report `false` there while
    // `passed` stays `true` — the exact split the field exists for. prop_85
    // (zip/rev over a bounded sample domain) is the committed corpus instance.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lean universal-field test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let out = temp_output_dir("aver-universal-bounded-out");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("proof-corpus/tip/isaplanner/prop_85.av")
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check --check-json` to run");
    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)));
    let summary: serde_json::Value =
        serde_json::from_str(json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}"));
    assert_eq!(
        (summary["passed"].as_bool(), summary["universal"].as_bool()),
        (Some(true), Some(false)),
        "a bounded `native_decide` proof must stay lenient on `passed` but report \
         `universal:false` (it depends on `Lean.ofReduceBool`, not the ∀-claim). \
         If `universal` flipped to true, prop_85 now closes genuinely — celebrate \
         and re-baseline this test.\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_check_dafny_rejects_sample_only_universal_as_unproven() {
    // Soundness: when the emitter cannot state a law's universal `∀`-claim it
    // drops it to concrete samples plus a `… (universal lemma omitted)`
    // comment. Dafny then finishes with 0 errors / exit 0 because the
    // universal was never asserted — a false-green the errors-only and
    // axiom-only gates both miss. `--check` must charge an omitted universal
    // against the sorry budget (like `assume {:axiom}`) so it reports
    // `passed:false`. The `fac = qfac · one` accumulator-equivalence is a
    // stable instance: both fns verify cleanly (errors:0) but the universal
    // needs an IH generalization the emitter does not do, so it is omitted.
    if Command::new("dafny").arg("--version").output().is_err() {
        eprintln!("skipping dafny omitted-universal soundness test: `dafny` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-omit-sound-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(
        src.join("omit.av"),
        "module Omit\n    effects []\n\n\
         type Nat\n    Z\n    S(Nat)\n\n\
         fn plus(x: Nat, y: Nat) -> Nat\n    match x\n        Nat.Z -> y\n        Nat.S(z) -> Nat.S(plus(z, y))\n\n\
         fn mult(x: Nat, y: Nat) -> Nat\n    match x\n        Nat.Z -> Nat.Z\n        Nat.S(z) -> plus(y, mult(z, y))\n\n\
         fn fac(x: Nat) -> Nat\n    match x\n        Nat.Z -> Nat.S(Nat.Z)\n        Nat.S(y) -> mult(x, fac(y))\n\n\
         fn qfac(x: Nat, y: Nat) -> Nat\n    match x\n        Nat.Z -> y\n        Nat.S(z) -> qfac(z, mult(x, y))\n\n\
         verify fac law facQfac\n    given x: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    fac(x) => qfac(x, Nat.S(Nat.Z))\n",
    )
    .expect("write omit.av");
    let out = temp_output_dir("aver-omit-sound-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("omit.av"))
        .arg("--backend")
        .arg("dafny")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check --check-json` to run");
    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)));
    let summary: serde_json::Value =
        serde_json::from_str(json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}"));
    // errors:0 confirms the ONLY reason for failure is the dropped universal,
    // so this exercises the omitted-gate specifically.
    assert_eq!(
        summary["errors"].as_u64(),
        Some(0),
        "expected a clean verify (errors:0); the omitted-universal gate, not \
         a Dafny error, must drive the failure.\n{}",
        format_output(&run)
    );
    assert!(
        summary["omitted"].as_u64().unwrap_or(0) >= 1,
        "expected the `facQfac` universal to be dropped to sample-only \
         (omitted >= 1).\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["passed"].as_bool(),
        Some(false),
        "a sample-only law whose universal was omitted must NOT pass --check \
         — dropping the ∀-claim is the Dafny analog of a sorry.\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_warns_when_dependency_module_has_verify_blocks() {
    // A `verify ... law` in a dependency module is silently dropped
    // (module-scoped verify is unsupported), so it would never fail — a
    // vacuous pass. The compiler must warn loudly. Pure codegen, no
    // verifier binary needed.
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-dep-verify-warn-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(
        src.join("dep.av"),
        "module Dep\n    depends []\n\nfn ident(n: Int) -> Int\n    ? \"id\"\n    n\n\n\
         verify ident law refl\n    given n: Int = -1..1\n    ident(n) => n\n",
    )
    .expect("write dep.av");
    std::fs::write(
        src.join("app.av"),
        "module App\n    depends [Dep]\n    effects [Console.print]\n\n\
         fn wrap(n: Int) -> Int\n    ? \"w\"\n    Dep.ident(n)\n\n\
         fn main() -> Unit\n    ! [Console.print]\n    Console.print(\"x\")\n",
    )
    .expect("write app.av");
    let out = temp_output_dir("aver-dep-verify-warn-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("app.av"))
        .arg("--backend")
        .arg("dafny")
        .arg("--module-root")
        .arg(&src)
        .arg("-o")
        .arg(&out)
        .output()
        .expect("expected `aver proof` to run");
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert!(
        stderr.contains("verify block") && stderr.contains("Dep") && stderr.contains("NOT checked"),
        "expected a warning that dependency module `Dep`'s verify blocks are unchecked, got:\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_dafny_warns_example_cases_not_checked() {
    // Dafny proves LAWS, not concrete example-cases — it cannot evaluate
    // a `f(x) => y` case the way Lean's `native_decide` does. It must say
    // so rather than silently pass case-form verify. Pure codegen, no
    // verifier binary needed. `sum_acc.av` carries case-form verify blocks.
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let out = temp_output_dir("aver-dafny-case-warn");
    let run = Command::new(aver_bin)
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .arg("proof")
        .arg("examples/data/sum_acc.av")
        .arg("--backend")
        .arg("dafny")
        .arg("-o")
        .arg(&out)
        .output()
        .expect("expected `aver proof` to run");
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert!(
        stderr.contains("example-based") && stderr.contains("NOT") && stderr.contains("Dafny"),
        "expected a warning that example-based verify is not Dafny-checked, got:\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_lean_vacuous_when_premise_law_builds_and_passes() {
    // A `when` premise that is unsatisfiable (here a nested Bool `match`
    // requiring `n > 0` AND `n < 0`) makes the law vacuously true, so a
    // sound prover must ACCEPT it. The premise lowers to a multi-line
    // `if/then/else`; previously the emit was unparseable Lean (the
    // unparenthesized `if` swallowed the trailing `= true`, and the
    // `-- when` comment leaked its continuation lines), and even parsed
    // `simp only` left the Bool premise opaque so `omega` failed — a
    // valid law wrongly REJECTED (false-RED). Pins parens + single-line
    // comment + `simp_all` so it builds and passes.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lean vacuous-when test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-lean-vacuous-when-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    let av = src.join("vac.av");
    std::fs::write(
        &av,
        "module VacuousLaw\n\nfn dbl(n: Int) -> Int\n    ? \"double\"\n    n + n\n\n\
         verify dbl law vac\n    given n: Int = -2..2\n    when match n > 0\n\
         \x20       true -> match n < 0\n            true -> true\n            false -> false\n\
         \x20       false -> false\n    dbl(n) => n + 999\n",
    )
    .expect("write vac.av");
    let out = temp_output_dir("aver-lean-vacuous-when-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(&av)
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check` to run");
    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)));
    let summary: serde_json::Value =
        serde_json::from_str(json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}"));
    assert_eq!(
        summary["passed"].as_bool(),
        Some(true),
        "vacuously-true `when`-premised law must build and pass on Lean\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_export_builds_grok_s_language_when_lake_is_available() {
    assert_proof_builds("examples/core/grok_s_language.av", "aver-proof-grok");
}

#[test]
fn proof_export_builds_pure_question_bang_when_backends_are_available() {
    let source = "module Prog\n\
        \x20   intent = \"stress pure ?! proof export\"\n\
        \n\
        fn okOne() -> Result<Int, String>\n\
        \x20   ? \"one\"\n\
        \x20   Result.Ok(1)\n\
        \n\
        fn okTwo() -> Result<Int, String>\n\
        \x20   ? \"two\"\n\
        \x20   Result.Ok(2)\n\
        \n\
        fn errLeft() -> Result<Int, String>\n\
        \x20   ? \"left error\"\n\
        \x20   Result.Err(\"left\")\n\
        \n\
        fn errRight() -> Result<Int, String>\n\
        \x20   ? \"right error\"\n\
        \x20   Result.Err(\"right\")\n\
        \n\
        fn pairOk() -> Result<Tuple<Int, Int>, String>\n\
        \x20   ? \"unwrap two successful Result branches\"\n\
        \x20   pair = (okOne(), okTwo())?!\n\
        \x20   match pair\n\
        \x20       (a, b) -> Result.Ok((a, b))\n\
        \n\
        fn pairErr() -> Result<Tuple<Int, Int>, String>\n\
        \x20   ? \"propagate the leftmost Result.Err\"\n\
        \x20   pair = (errLeft(), errRight())?!\n\
        \x20   match pair\n\
        \x20       (a, b) -> Result.Ok((a, b))\n\
        \n\
        verify pairOk\n\
        \x20   pairOk() => Result.Ok((1, 2))\n\
        \n\
        verify pairErr\n\
        \x20   pairErr() => Result.Err(\"left\")\n";

    let dir = temp_output_dir("aver-proof-pure-qbang");
    std::fs::create_dir_all(&dir).expect("create temp dir");
    std::fs::write(
        dir.join("aver.toml"),
        "[independence]\nmode = \"complete\"\n",
    )
    .expect("write aver.toml");
    std::fs::write(dir.join("program.av"), source).expect("write program.av");
    let aver_bin = env!("CARGO_BIN_EXE_aver");

    if Command::new("lake").arg("--version").output().is_ok() {
        let lean_dir = dir.join("lean");
        let proof = Command::new(aver_bin)
            .current_dir(&dir)
            .arg("proof")
            .arg("program.av")
            .arg("--backend")
            .arg("lean")
            .arg("--verify-mode")
            .arg("auto")
            .arg("-o")
            .arg(&lean_dir)
            .output()
            .expect("expected `aver proof --backend lean` to run");
        assert!(
            proof.status.success(),
            "Lean proof export failed:\n{}",
            format_output(&proof)
        );

        let build = Command::new("lake")
            .current_dir(&lean_dir)
            .arg("build")
            .output()
            .expect("expected `lake build` to run");
        assert!(
            build.status.success(),
            "Lean pure ?! proof build failed:\n{}",
            format_output(&build)
        );
    }

    if Command::new("dafny").arg("--version").output().is_ok() {
        let dafny_dir = dir.join("dafny");
        let proof = Command::new(aver_bin)
            .current_dir(&dir)
            .arg("proof")
            .arg("program.av")
            .arg("--backend")
            .arg("dafny")
            .arg("--verify-mode")
            .arg("auto")
            .arg("-o")
            .arg(&dafny_dir)
            .output()
            .expect("expected `aver proof --backend dafny` to run");
        assert!(
            proof.status.success(),
            "Dafny proof export failed:\n{}",
            format_output(&proof)
        );

        let verify = Command::new("dafny")
            .current_dir(&dafny_dir)
            .arg("verify")
            .arg("Prog.dfy")
            .output()
            .expect("expected `dafny verify` to run");
        assert!(
            verify.status.success(),
            "Dafny pure ?! proof verification failed:\n{}",
            format_output(&verify)
        );
    }

    let _ = std::fs::remove_dir_all(&dir);
}

// ---------------------------------------------------------------------------
// Oracle v1 — aver.toml mode rejection in aver proof
// ---------------------------------------------------------------------------

fn run_aver_proof_in_dir(dir: &PathBuf, source: &str, toml: &str) -> std::process::Output {
    std::fs::write(dir.join("aver.toml"), toml).expect("write aver.toml");
    std::fs::write(dir.join("program.av"), source).expect("write program.av");
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output_dir = dir.join("proof_out");
    Command::new(aver_bin)
        .current_dir(dir)
        .arg("proof")
        .arg("program.av")
        .arg("--verify-mode")
        .arg("auto")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("expected `aver proof` to run")
}

#[test]
fn proof_rejects_cancel_independence_mode() {
    let dir = temp_output_dir("aver-proof-cancel");
    std::fs::create_dir_all(&dir).expect("create temp dir");
    let output = run_aver_proof_in_dir(
        &dir,
        "module Prog\n    intent = \"test\"\n\nfn absVal(x: Int) -> Int\n    ? \"abs\"\n    match x < 0\n        true  -> 0 - x\n        false -> x\n",
        "[independence]\nmode = \"cancel\"\n",
    );
    assert!(
        !output.status.success(),
        "aver proof should fail on cancel mode; {}",
        format_output(&output)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("mode = \"cancel\"") || stderr.contains("complete mode"),
        "expected cancel-mode rejection; got: {}",
        stderr
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn proof_rejects_sequential_independence_mode() {
    let dir = temp_output_dir("aver-proof-sequential");
    std::fs::create_dir_all(&dir).expect("create temp dir");
    let output = run_aver_proof_in_dir(
        &dir,
        "module Prog\n    intent = \"test\"\n\nfn absVal(x: Int) -> Int\n    ? \"abs\"\n    match x < 0\n        true  -> 0 - x\n        false -> x\n",
        "[independence]\nmode = \"sequential\"\n",
    );
    assert!(
        !output.status.success(),
        "aver proof should fail on sequential mode; {}",
        format_output(&output)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("sequential") && stderr.contains("complete mode"),
        "expected sequential-mode rejection; got: {}",
        stderr
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn aver_verify_runs_effectful_law_with_oracle_stub() {
    // End-to-end: `aver verify` runs an effectful law whose given clause
    // supplies an oracle stub. LHS evaluation of the effectful impl must
    // see stub values (not real Random.int), so the law's equality check
    // holds deterministically.
    let dir = temp_output_dir("aver-verify-oracle");
    std::fs::create_dir_all(&dir).expect("create temp dir");
    std::fs::write(
        dir.join("aver.toml"),
        "[independence]\nmode = \"complete\"\n",
    )
    .expect("write aver.toml");
    std::fs::write(
        dir.join("program.av"),
        "module Prog\n\
         \x20   intent = \"t\"\n\
         \n\
         fn stubConst(path: BranchPath, n: Int, min: Int, max: Int) -> Int\n\
         \x20   ? \"always returns min\"\n\
         \x20   min\n\
         \n\
         fn pickOne() -> Int\n\
         \x20   ? \"sample one Random.int\"\n\
         \x20   ! [Random.int]\n\
         \x20   Random.int(7, 99)\n\
         \n\
         fn pickOneSpec(path: BranchPath, rnd: Fn(BranchPath, Int, Int, Int) -> Int) -> Int\n\
         \x20   ? \"one draw at the caller's path\"\n\
         \x20   rnd(path, 0, 7, 99)\n\
         \n\
         verify pickOne law consistent\n\
         \x20   given rnd: Random.int = [stubConst]\n\
         \x20   pickOne() => pickOneSpec(BranchPath.Root, rnd)\n",
    )
    .expect("write program.av");

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output = Command::new(aver_bin)
        .current_dir(&dir)
        .arg("verify")
        .arg("program.av")
        .output()
        .expect("expected `aver verify` to run");

    assert!(
        output.status.success(),
        "aver verify failed with effectful-law + oracle stub; {}",
        format_output(&output)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn aver_verify_result_only_law_allows_output_effect_without_stub() {
    // Regression: a result-only law on a fn whose effect list includes
    // both an oracle-stubbed effect (Random.int) and an Output-only
    // effect (Console.print) used to pass typecheck + proof export but
    // fail at runtime verify with "Runtime effect violation: cannot
    // call 'Console.print'". The VM's verify helper declares no
    // effects, so the Output emission was ungated. Fix: the verify
    // runner always enables trace collection (not just for trace
    // blocks) so classified effects without stubs go through the
    // usual suppression path.
    let dir = temp_output_dir("aver-verify-output-without-stub");
    std::fs::create_dir_all(&dir).expect("create temp dir");
    std::fs::write(
        dir.join("aver.toml"),
        "[independence]\nmode = \"complete\"\n",
    )
    .expect("write aver.toml");
    std::fs::write(
        dir.join("program.av"),
        "module Prog\n\
         \x20   intent = \"t\"\n\
         \n\
         fn stubConst(path: BranchPath, n: Int, min: Int, max: Int) -> Int\n\
         \x20   ? \"always min\"\n\
         \x20   min\n\
         \n\
         fn noisyRoll() -> Int\n\
         \x20   ? \"rolls and logs.\"\n\
         \x20   ! [Random.int, Console.print]\n\
         \x20   n = Random.int(1, 6)\n\
         \x20   Console.print(\"rolled\")\n\
         \x20   n\n\
         \n\
         verify noisyRoll law noisyRollSpec\n\
         \x20   given rnd: Random.int = [stubConst]\n\
         \x20   noisyRoll() => rnd(BranchPath.Root, 0, 1, 6)\n",
    )
    .expect("write program.av");

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output = Command::new(aver_bin)
        .current_dir(&dir)
        .arg("verify")
        .arg("program.av")
        .output()
        .expect("expected `aver verify` to run");

    assert!(
        output.status.success(),
        "aver verify must not report an effect violation for an \
         Output effect (Console.print) in a result-only law; {}",
        format_output(&output)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn aver_verify_runs_effectful_bang_group_law() {
    // End-to-end: `aver verify` on an effectful law whose impl uses
    // `(Random.int(1, 6), Random.int(1, 6))!` — a two-branch `!` group.
    // The runtime should thread each branch's BranchPath.child(root, i)
    // and reset the counter to 0 per branch, so the stub returns
    // deterministic values that match the RHS spec.
    let dir = temp_output_dir("aver-verify-bang-group");
    std::fs::create_dir_all(&dir).expect("create temp dir");
    std::fs::write(
        dir.join("aver.toml"),
        "[independence]\nmode = \"complete\"\n",
    )
    .expect("write aver.toml");
    std::fs::write(
        dir.join("program.av"),
        // Stub: returns the branch index of the path (0 or 1) as the
        // random value. This makes each branch's oracle output distinct,
        // so a correct path-threading implementation gives (0, 1) while
        // an incorrect one (root path on both branches) would give (0, 0).
        "module Prog\n\
         \x20   intent = \"t\"\n\
         \n\
         fn stubByBranch(path: BranchPath, n: Int, min: Int, max: Int) -> Int\n\
         \x20   ? \"return counter so each branch's call is distinguishable\"\n\
         \x20   n\n\
         \n\
         fn pickPair() -> Tuple<Int, Int>\n\
         \x20   ? \"two parallel draws\"\n\
         \x20   ! [Random.int]\n\
         \x20   (Random.int(1, 6), Random.int(1, 6))!\n\
         \n\
         fn pickPairSpec(path: BranchPath, rnd: Fn(BranchPath, Int, Int, Int) -> Int) -> Tuple<Int, Int>\n\
         \x20   ? \"two draws, each at its own branch\"\n\
         \x20   (rnd(BranchPath.child(path, 0), 0, 1, 6), rnd(BranchPath.child(path, 1), 0, 1, 6))\n\
         \n\
         verify pickPair law consistent\n\
         \x20   given rnd: Random.int = [stubByBranch]\n\
         \x20   pickPair() => pickPairSpec(BranchPath.Root, rnd)\n",
    )
    .expect("write program.av");

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output = Command::new(aver_bin)
        .current_dir(&dir)
        .arg("verify")
        .arg("program.av")
        .output()
        .expect("expected `aver verify` to run");

    assert!(
        output.status.success(),
        "aver verify failed on `!` group law; {}",
        format_output(&output)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn aver_verify_cases_form_trace_with_result_projection() {
    // End-to-end: trace-aware cases-form verify block with a given-
    // bound oracle and `.result` projection. Closer to the shape a user
    // actually wants to write for simple Oracle v1 laws.
    let dir = temp_output_dir("aver-verify-result-projection");
    std::fs::create_dir_all(&dir).expect("create temp dir");
    std::fs::write(
        dir.join("aver.toml"),
        "[independence]\nmode = \"complete\"\n",
    )
    .expect("write aver.toml");
    std::fs::write(
        dir.join("program.av"),
        "module Prog\n\
         \x20   intent = \"t\"\n\
         \n\
         fn fairDie(path: BranchPath, n: Int, min: Int, max: Int) -> Int\n\
         \x20   ? \"always 4, nicely deterministic\"\n\
         \x20   4\n\
         \n\
         fn pickOne() -> Int\n\
         \x20   ? \"one roll\"\n\
         \x20   ! [Random.int]\n\
         \x20   Random.int(1, 6)\n\
         \n\
         verify pickOne trace\n\
         \x20   given rnd: Random.int = [fairDie]\n\
         \x20   pickOne().result => 4\n",
    )
    .expect("write program.av");

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output = Command::new(aver_bin)
        .current_dir(&dir)
        .arg("verify")
        .arg("program.av")
        .output()
        .expect("expected `aver verify` to run");

    assert!(
        output.status.success(),
        "aver verify failed on cases-form trace with .result projection; {}",
        format_output(&output)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn aver_verify_trace_contains_and_event_and_length_projections() {
    // End-to-end: trace-aware verify exercising all three positional
    // projections — `.trace.length()`, `.trace.event(k)`,
    // `.trace.contains(event_lit)` — alongside `.result`. Matches the
    // shape of the plan's Example 5 + user-requested form.
    let dir = temp_output_dir("aver-verify-trace-projections");
    std::fs::create_dir_all(&dir).expect("create temp dir");
    std::fs::write(
        dir.join("aver.toml"),
        "[independence]\nmode = \"complete\"\n",
    )
    .expect("write aver.toml");
    std::fs::write(
        dir.join("program.av"),
        "module Prog\n\
         \x20   intent = \"t\"\n\
         \n\
         fn fairDie(path: BranchPath, n: Int, min: Int, max: Int) -> Int\n\
         \x20   ? \"always 4\"\n\
         \x20   4\n\
         \n\
         fn hello() -> Int\n\
         \x20   ? \"roll + print\"\n\
         \x20   ! [Random.int, Console.print]\n\
         \x20   x = Random.int(1, 6)\n\
         \x20   Console.print(\"rolled 4\")\n\
         \x20   x\n\
         \n\
         verify hello trace\n\
         \x20   given rnd: Random.int = [fairDie]\n\
         \x20   hello().result => 4\n\
         \x20   hello().trace.length() => 2\n\
         \x20   hello().trace.contains(Console.print(\"rolled 4\")) => true\n",
    )
    .expect("write program.av");

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output = Command::new(aver_bin)
        .current_dir(&dir)
        .arg("verify")
        .arg("program.av")
        .output()
        .expect("expected `aver verify` to run");

    assert!(
        output.status.success(),
        "aver verify failed on trace-projection law; {}",
        format_output(&output)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn aver_verify_trace_count_projection_matches_event_method() {
    // 0.13 Limit nail #3: `.trace.count(M)` returns the number of trace
    // events with method `M`. Argument shape mirrors `.contains` —
    // either an effect-method reference or a call literal. The fn here
    // calls Random.int twice and Console.print once; the count law
    // distinguishes the two methods.
    let dir = temp_output_dir("aver-verify-trace-count");
    std::fs::create_dir_all(&dir).expect("create temp dir");
    std::fs::write(
        dir.join("aver.toml"),
        "[independence]\nmode = \"complete\"\n",
    )
    .expect("write aver.toml");
    std::fs::write(
        dir.join("program.av"),
        "module Prog\n\
         \x20   intent = \"t\"\n\
         \n\
         fn fairDie(path: BranchPath, n: Int, min: Int, max: Int) -> Int\n\
         \x20   ? \"always 4\"\n\
         \x20   4\n\
         \n\
         fn rollPair() -> Int\n\
         \x20   ? \"two rolls + a print\"\n\
         \x20   ! [Random.int, Console.print]\n\
         \x20   a = Random.int(1, 6)\n\
         \x20   b = Random.int(1, 6)\n\
         \x20   Console.print(\"rolled\")\n\
         \x20   a + b\n\
         \n\
         verify rollPair trace\n\
         \x20   given rnd: Random.int = [fairDie]\n\
         \x20   rollPair().trace.count(Random.int) => 2\n\
         \x20   rollPair().trace.count(Console.print) => 1\n",
    )
    .expect("write program.av");

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output = Command::new(aver_bin)
        .current_dir(&dir)
        .arg("verify")
        .arg("program.av")
        .output()
        .expect("expected `aver verify` to run");

    assert!(
        output.status.success(),
        "aver verify failed on trace.count law; {}",
        format_output(&output)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn aver_verify_trace_local_bindings_substitute_into_cases() {
    // Oracle v1: local bindings (`name = expr`) in a verify-trace block
    // are syntactic aliases substituted into each case's LHS / RHS
    // before helper generation. Here `expected = 4` is used as both the
    // `.result` RHS and — via another binding — as a shared event
    // literal referenced by `.contains`. If substitution is wired up,
    // the law must pass exactly like the inlined form would.
    let dir = temp_output_dir("aver-verify-local-bindings");
    std::fs::create_dir_all(&dir).expect("create temp dir");
    std::fs::write(
        dir.join("aver.toml"),
        "[independence]\nmode = \"complete\"\n",
    )
    .expect("write aver.toml");
    std::fs::write(
        dir.join("program.av"),
        "module Prog\n\
         \x20   intent = \"t\"\n\
         \n\
         fn fairDie(path: BranchPath, n: Int, min: Int, max: Int) -> Int\n\
         \x20   ? \"always 4\"\n\
         \x20   4\n\
         \n\
         fn hello() -> Int\n\
         \x20   ? \"roll + print\"\n\
         \x20   ! [Random.int, Console.print]\n\
         \x20   x = Random.int(1, 6)\n\
         \x20   Console.print(\"rolled 4\")\n\
         \x20   x\n\
         \n\
         verify hello trace\n\
         \x20   given rnd: Random.int = [fairDie]\n\
         \x20   expected = 4\n\
         \x20   printed = Console.print(\"rolled 4\")\n\
         \x20   hello().result => expected\n\
         \x20   hello().trace.contains(printed) => true\n",
    )
    .expect("write program.av");

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output = Command::new(aver_bin)
        .current_dir(&dir)
        .arg("verify")
        .arg("program.av")
        .output()
        .expect("expected `aver verify` to run");

    assert!(
        output.status.success(),
        "aver verify failed with local bindings in trace block; {}",
        format_output(&output)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn aver_verify_trace_given_alias_callable_in_case_expressions() {
    // Oracle v1: `given rnd: Random.int = [fairDie]` installs a VM
    // stub that intercepts Random.int inside `hello()`. The same alias
    // is also substituted syntactically into case LHS / RHS, so users
    // can write `rnd(BranchPath.root, 0, 1, 6)` as a value expression
    // — which becomes a direct call to `fairDie` after substitution.
    let dir = temp_output_dir("aver-verify-given-alias");
    std::fs::create_dir_all(&dir).expect("create temp dir");
    std::fs::write(
        dir.join("aver.toml"),
        "[independence]\nmode = \"complete\"\n",
    )
    .expect("write aver.toml");
    std::fs::write(
        dir.join("program.av"),
        "module Prog\n\
         \x20   intent = \"t\"\n\
         \n\
         fn fairDie(path: BranchPath, n: Int, min: Int, max: Int) -> Int\n\
         \x20   ? \"always 4\"\n\
         \x20   4\n\
         \n\
         fn pickOne() -> Int\n\
         \x20   ? \"one roll\"\n\
         \x20   ! [Random.int]\n\
         \x20   Random.int(1, 6)\n\
         \n\
         verify pickOne trace\n\
         \x20   given rnd: Random.int = [fairDie]\n\
         \x20   pickOne().result => rnd(BranchPath.Root, 0, 1, 6)\n",
    )
    .expect("write program.av");

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output = Command::new(aver_bin)
        .current_dir(&dir)
        .arg("verify")
        .arg("program.av")
        .output()
        .expect("expected `aver verify` to run");

    assert!(
        output.status.success(),
        "aver verify failed when using given-alias as callable in case; {}",
        format_output(&output)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn aver_verify_trace_suppresses_output_effects() {
    // Oracle v1: under `verify fn trace`, output-dimension effects
    // (Console.print / .error / .warn) are recorded as trace events
    // but not actually dispatched to the host. Otherwise running
    // `aver verify` on a fn that prints would leak its output into
    // the terminal — noisy and confusing. The trace buffer still
    // lets `.trace.contains(...)` / `.length()` assertions work.
    let dir = temp_output_dir("aver-verify-trace-suppress-output");
    std::fs::create_dir_all(&dir).expect("create temp dir");
    std::fs::write(
        dir.join("aver.toml"),
        "[independence]\nmode = \"complete\"\n",
    )
    .expect("write aver.toml");
    std::fs::write(
        dir.join("program.av"),
        "module Prog\n\
         \x20   intent = \"t\"\n\
         \n\
         fn chatty() -> Int\n\
         \x20   ? \"prints then returns\"\n\
         \x20   ! [Console.print]\n\
         \x20   Console.print(\"SENTINEL-LEAK\")\n\
         \x20   42\n\
         \n\
         verify chatty trace\n\
         \x20   chatty().result => 42\n\
         \x20   chatty().trace.contains(Console.print(\"SENTINEL-LEAK\")) => true\n",
    )
    .expect("write program.av");

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output = Command::new(aver_bin)
        .current_dir(&dir)
        .arg("verify")
        .arg("program.av")
        .output()
        .expect("expected `aver verify` to run");
    assert!(
        output.status.success(),
        "aver verify failed on chatty trace; {}",
        format_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        !stdout.contains("SENTINEL-LEAK"),
        "Console.print should be suppressed under verify trace, but SENTINEL-LEAK \
         appeared in stdout: {}",
        stdout
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn aver_verify_trace_rejects_generative_effect_without_given_stub() {
    // Oracle v1: under `verify fn trace`, every generative / gen+output
    // effect the fn uses must have a `given` stub. Without one, the
    // verify run would dispatch the real effect (e.g. live Random.int)
    // and assertions would compare against non-deterministic output —
    // a confusing failure. The check-time rejection points straight at
    // the fix.
    let dir = temp_output_dir("aver-verify-trace-missing-given");
    std::fs::create_dir_all(&dir).expect("create temp dir");
    std::fs::write(
        dir.join("aver.toml"),
        "[independence]\nmode = \"complete\"\n",
    )
    .expect("write aver.toml");
    std::fs::write(
        dir.join("program.av"),
        "module Prog\n\
         \x20   intent = \"t\"\n\
         \n\
         fn roll() -> Int\n\
         \x20   ? \"rolls\"\n\
         \x20   ! [Random.int]\n\
         \x20   Random.int(1, 6)\n\
         \n\
         verify roll trace\n\
         \x20   roll().result => 4\n",
    )
    .expect("write program.av");

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let check = Command::new(aver_bin)
        .current_dir(&dir)
        .arg("check")
        .arg("program.av")
        .output()
        .expect("expected `aver check` to run");
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&check.stderr),
        String::from_utf8_lossy(&check.stdout)
    );
    assert!(
        combined.contains("needs a `given` stub"),
        "expected missing-given diagnostic, got: {}",
        combined
    );
    assert!(
        combined.contains("Random.int"),
        "expected diagnostic to mention Random.int, got: {}",
        combined
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn aver_verify_trace_failure_shows_recorded_events() {
    // Oracle v1: when a trace-projection assertion fails (e.g.
    // `.trace.contains(X) => true` but X wasn't emitted), the failure
    // message must append the actually-recorded events so the user can
    // see which events fired and fix their assertion.
    let dir = temp_output_dir("aver-verify-trace-failure-tail");
    std::fs::create_dir_all(&dir).expect("create temp dir");
    std::fs::write(
        dir.join("aver.toml"),
        "[independence]\nmode = \"complete\"\n",
    )
    .expect("write aver.toml");
    std::fs::write(
        dir.join("program.av"),
        "module Prog\n\
         \x20   intent = \"t\"\n\
         \n\
         fn fairDie(path: BranchPath, n: Int, min: Int, max: Int) -> Int\n\
         \x20   ? \"always 4\"\n\
         \x20   4\n\
         \n\
         fn hello() -> Int\n\
         \x20   ? \"roll + print\"\n\
         \x20   ! [Random.int, Console.print]\n\
         \x20   x = Random.int(1, 6)\n\
         \x20   Console.print(\"different message\")\n\
         \x20   x\n\
         \n\
         verify hello trace\n\
         \x20   given rnd: Random.int = [fairDie]\n\
         \x20   hello().trace.contains(Console.print(\"rolled 4\")) => true\n",
    )
    .expect("write program.av");

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output = Command::new(aver_bin)
        .current_dir(&dir)
        .arg("verify")
        .arg("program.av")
        .output()
        .expect("expected `aver verify` to run");
    assert!(
        !output.status.success(),
        "expected failure, got success; {}",
        format_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("(trace:"),
        "expected failure output to include recorded trace tail, got: {}",
        stdout
    );
    assert!(
        stdout.contains("different message"),
        "expected failure output to list the actual emitted event, got: {}",
        stdout
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn aver_verify_trace_end_to_end_target_shape() {
    // Oracle v1: the end-to-end shape a user writes for a small
    // trace-aware verification — given binds an oracle alias, a local
    // binding factors out the expected oracle value, `.result` checks
    // the raw return, `.trace.contains(...)` checks an output-only
    // event. This single test guards the whole Oracle v1 UX surface:
    //   - `given rnd: Random.int = [fairDie]` (alias + VM stub)
    //   - `expect = rnd(BranchPath.Root, 0, 1, 6)` (local + alias
    //     substitution)
    //   - `hello().result => expect` (local binding in case RHS)
    //   - `hello().trace.contains(Console.print(...))` (output effect
    //     suppressed at runtime, still present in the trace)
    let dir = temp_output_dir("aver-verify-target-shape");
    std::fs::create_dir_all(&dir).expect("create temp dir");
    std::fs::write(
        dir.join("aver.toml"),
        "[independence]\nmode = \"complete\"\n",
    )
    .expect("write aver.toml");
    std::fs::write(
        dir.join("program.av"),
        "module Prog\n\
         \x20   intent = \"t\"\n\
         \n\
         fn fairDie(path: BranchPath, n: Int, min: Int, max: Int) -> Int\n\
         \x20   ? \"always 4\"\n\
         \x20   4\n\
         \n\
         fn hello() -> Int\n\
         \x20   ? \"roll + print\"\n\
         \x20   ! [Random.int, Console.print]\n\
         \x20   x = Random.int(1, 6)\n\
         \x20   Console.print(\"rolled 4\")\n\
         \x20   x\n\
         \n\
         verify hello trace\n\
         \x20   given rnd: Random.int = [fairDie]\n\
         \x20   expect = rnd(BranchPath.Root, 0, 1, 6)\n\
         \x20   hello().result => expect\n\
         \x20   hello().trace.contains(Console.print(\"rolled 4\")) => true\n",
    )
    .expect("write program.av");

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output = Command::new(aver_bin)
        .current_dir(&dir)
        .arg("verify")
        .arg("program.av")
        .output()
        .expect("expected `aver verify` to run");
    assert!(
        output.status.success(),
        "end-to-end target-shape verify failed; {}",
        format_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("2/2"),
        "expected 2/2 cases passed, got: {}",
        stdout
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn aver_verify_trace_http_get_generative_output_end_to_end() {
    // Oracle v1: Http.get is the first generative+output effect —
    // stubbed response comes from the oracle, request body lands in
    // the trace. This test exercises the full path: given-bound stub
    // returning a non-trivial Result<HttpResponse, String>, with
    // `.trace.contains(Http.get(...))` resolving the request event.
    let dir = temp_output_dir("aver-verify-trace-http");
    std::fs::create_dir_all(&dir).expect("create temp dir");
    std::fs::write(
        dir.join("aver.toml"),
        "[independence]\nmode = \"complete\"\n",
    )
    .expect("write aver.toml");
    std::fs::write(
        dir.join("program.av"),
        "module Prog\n\
         \x20   intent = \"t\"\n\
         \n\
         fn fakeFetch(path: BranchPath, n: Int, url: String) -> Result<HttpResponse, String>\n\
         \x20   ? \"deterministic fake fetch\"\n\
         \x20   Result.Ok(HttpResponse(status = 200, body = \"hello\", headers = {}))\n\
         \n\
         fn fetch() -> Result<HttpResponse, String>\n\
         \x20   ? \"fetches\"\n\
         \x20   ! [Http.get]\n\
         \x20   Http.get(\"https://x.test/y\")\n\
         \n\
         verify fetch trace\n\
         \x20   given stub: Http.get = [fakeFetch]\n\
         \x20   fetch().trace.contains(Http.get(\"https://x.test/y\")) => true\n",
    )
    .expect("write program.av");

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output = Command::new(aver_bin)
        .current_dir(&dir)
        .arg("verify")
        .arg("program.av")
        .output()
        .expect("expected `aver verify` to run");
    assert!(
        output.status.success(),
        "Http.get verify-trace failed end-to-end; {}",
        format_output(&output)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn aver_verify_trace_broader_oracle_effects_end_to_end() {
    // Oracle v1 also covers line input, disk operation/result effects,
    // output-only sleep, and one-shot TCP. This keeps the expanded
    // classification wired through given stubs, trace event literals,
    // and Result propagation.
    let dir = temp_output_dir("aver-verify-trace-broader-effects");
    std::fs::create_dir_all(&dir).expect("create temp dir");
    std::fs::write(
        dir.join("aver.toml"),
        "[independence]\nmode = \"complete\"\n",
    )
    .expect("write aver.toml");
    std::fs::write(
        dir.join("program.av"),
        "module Prog\n\
         \x20   intent = \"t\"\n\
         \n\
         fn fakeLine(path: BranchPath, n: Int) -> Result<String, String>\n\
         \x20   ? \"deterministic console input\"\n\
         \x20   Result.Ok(\"deploy\")\n\
         \n\
         fn fakeWrite(path: BranchPath, n: Int, file: String, content: String) -> Result<Unit, String>\n\
         \x20   ? \"deterministic write\"\n\
         \x20   Result.Ok(Unit)\n\
         \n\
         fn fakeSend(path: BranchPath, n: Int, host: String, port: Int, message: String) -> Result<String, String>\n\
         \x20   ? \"deterministic one-shot tcp\"\n\
         \x20   Result.Ok(\"ACK\")\n\
         \n\
         fn runAll() -> Result<String, String>\n\
         \x20   ? \"uses broader classified effects\"\n\
         \x20   ! [Console.readLine, Disk.writeText, Time.sleep, Tcp.send]\n\
         \x20   cmd = Console.readLine()?\n\
         \x20   _ = Disk.writeText(\"state.txt\", cmd)?\n\
         \x20   Time.sleep(1)\n\
         \x20   ack = Tcp.send(\"127.0.0.1\", 9, cmd)?\n\
         \x20   Result.Ok(ack)\n\
         \n\
         verify runAll trace\n\
         \x20   given line: Console.readLine = [fakeLine]\n\
         \x20   given write: Disk.writeText = [fakeWrite]\n\
         \x20   given send: Tcp.send = [fakeSend]\n\
         \x20   runAll().result => Result.Ok(\"ACK\")\n\
         \x20   runAll().trace.contains(Console.readLine()) => true\n\
         \x20   runAll().trace.contains(Disk.writeText(\"state.txt\", \"deploy\")) => true\n\
         \x20   runAll().trace.contains(Time.sleep(1)) => true\n\
         \x20   runAll().trace.contains(Tcp.send(\"127.0.0.1\", 9, \"deploy\")) => true\n",
    )
    .expect("write program.av");

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output = Command::new(aver_bin)
        .current_dir(&dir)
        .arg("verify")
        .arg("program.av")
        .output()
        .expect("expected `aver verify` to run");
    assert!(
        output.status.success(),
        "broader Oracle effects verify-trace failed end-to-end; {}",
        format_output(&output)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn aver_verify_trace_contains_event_literal_with_interpolated_local() {
    // Oracle v1: `.trace.contains(Console.print("rolled {expect}"))`
    // with local binding `expect = 4` must elaborate to the event
    // literal `Console.print("rolled 4")` after the parse-time ident
    // substitution rewrites `{expect}` to `Literal(Int(4))`. This
    // exercises `literal_expr_to_value`'s Parsed-segment support — the
    // interpolated string resolves to a plain String arg so the event
    // literal round-trips to the recorded `EffectEvent`.
    let dir = temp_output_dir("aver-verify-trace-interp-local");
    std::fs::create_dir_all(&dir).expect("create temp dir");
    std::fs::write(
        dir.join("aver.toml"),
        "[independence]\nmode = \"complete\"\n",
    )
    .expect("write aver.toml");
    std::fs::write(
        dir.join("program.av"),
        "module Prog\n\
         \x20   intent = \"t\"\n\
         \n\
         fn fairDie(path: BranchPath, n: Int, min: Int, max: Int) -> Int\n\
         \x20   ? \"always 4\"\n\
         \x20   4\n\
         \n\
         fn hello() -> Int\n\
         \x20   ? \"roll + print\"\n\
         \x20   ! [Random.int, Console.print]\n\
         \x20   x = Random.int(1, 6)\n\
         \x20   Console.print(\"rolled {x}\")\n\
         \x20   x\n\
         \n\
         verify hello trace\n\
         \x20   given rnd: Random.int = [fairDie]\n\
         \x20   expect = 4\n\
         \x20   hello().result => expect\n\
         \x20   hello().trace.contains(Console.print(\"rolled {expect}\")) => true\n",
    )
    .expect("write program.av");

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output = Command::new(aver_bin)
        .current_dir(&dir)
        .arg("verify")
        .arg("program.av")
        .output()
        .expect("expected `aver verify` to run");
    assert!(
        output.status.success(),
        "interpolated event literal failed; {}",
        format_output(&output)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn aver_verify_trace_snapshot_stub_is_plain_capability_reader() {
    // Oracle v1: snapshot effects (Args.get / Env.get) bind a plain
    // capability reader — the stub signature mirrors the runtime
    // signature with no leading (BranchPath, Int). Only generative /
    // generative+output stubs thread path + counter. This matches the
    // plan's "Snapshot effects → not branch-indexed" rule.
    let dir = temp_output_dir("aver-verify-trace-snapshot");
    std::fs::create_dir_all(&dir).expect("create temp dir");
    std::fs::write(
        dir.join("aver.toml"),
        "[independence]\nmode = \"complete\"\n",
    )
    .expect("write aver.toml");
    std::fs::write(
        dir.join("program.av"),
        "module Prog\n\
         \x20   intent = \"t\"\n\
         \n\
         fn empty() -> List<String>\n\
         \x20   ? \"fixed empty args\"\n\
         \x20   []\n\
         \n\
         fn isEmpty() -> Bool\n\
         \x20   ? \"no args\"\n\
         \x20   ! [Args.get]\n\
         \x20   args = Args.get()\n\
         \x20   match args\n\
         \x20       [] -> true\n\
         \x20       _ -> false\n\
         \n\
         verify isEmpty trace\n\
         \x20   given argv: Args.get = [empty]\n\
         \x20   isEmpty().result => true\n",
    )
    .expect("write program.av");

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output = Command::new(aver_bin)
        .current_dir(&dir)
        .arg("verify")
        .arg("program.av")
        .output()
        .expect("expected `aver verify` to run");
    assert!(
        output.status.success(),
        "snapshot-dim verify failed; {}",
        format_output(&output)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn aver_verify_trace_env_get_snapshot_with_args() {
    // Oracle v1: Env.get is a snapshot effect with a String argument.
    // The stub's runtime signature `String -> Option<String>` matches
    // what the given-bound alias exposes — no BranchPath / counter
    // threading since snapshot effects are deterministic and not
    // branch-indexed.
    let dir = temp_output_dir("aver-verify-trace-env-get");
    std::fs::create_dir_all(&dir).expect("create temp dir");
    std::fs::write(
        dir.join("aver.toml"),
        "[independence]\nmode = \"complete\"\n",
    )
    .expect("write aver.toml");
    std::fs::write(
        dir.join("program.av"),
        "module Prog\n\
         \x20   intent = \"t\"\n\
         \n\
         fn stubEnv(key: String) -> Option<String>\n\
         \x20   ? \"key-aware env stub\"\n\
         \x20   match key\n\
         \x20       \"USER\" -> Option.Some(\"alice\")\n\
         \x20       _ -> Option.None\n\
         \n\
         fn greeting() -> String\n\
         \x20   ? \"greet\"\n\
         \x20   ! [Env.get]\n\
         \x20   match Env.get(\"USER\")\n\
         \x20       Option.Some(u) -> \"hi {u}\"\n\
         \x20       Option.None -> \"hi stranger\"\n\
         \n\
         verify greeting trace\n\
         \x20   given env: Env.get = [stubEnv]\n\
         \x20   greeting().result => \"hi alice\"\n",
    )
    .expect("write program.av");

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output = Command::new(aver_bin)
        .current_dir(&dir)
        .arg("verify")
        .arg("program.av")
        .output()
        .expect("expected `aver verify` to run");
    assert!(
        output.status.success(),
        "Env.get snapshot verify failed; {}",
        format_output(&output)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn aver_verify_trace_rejects_stub_with_wrong_oracle_signature() {
    // Oracle v1: each `given <name>: <Effect.method> = [stub]` must
    // bind a stub whose inferred type matches the oracle signature for
    // that effect. Most common footgun: copy-pasting a (BranchPath,
    // Int)-prefixed generative stub into a snapshot `given` — at
    // runtime those extra params get ignored and the verify produces
    // bogus values. Now caught at check time.
    let dir = temp_output_dir("aver-verify-trace-wrong-stub-sig");
    std::fs::create_dir_all(&dir).expect("create temp dir");
    std::fs::write(
        dir.join("aver.toml"),
        "[independence]\nmode = \"complete\"\n",
    )
    .expect("write aver.toml");
    std::fs::write(
        dir.join("program.av"),
        "module Prog\n\
         \x20   intent = \"t\"\n\
         \n\
         fn wrong(_p: BranchPath, _n: Int) -> List<String>\n\
         \x20   ? \"wrong snapshot stub signature\"\n\
         \x20   []\n\
         \n\
         fn isEmpty() -> Bool\n\
         \x20   ? \"no args\"\n\
         \x20   ! [Args.get]\n\
         \x20   args = Args.get()\n\
         \x20   match args\n\
         \x20       [] -> true\n\
         \x20       _ -> false\n\
         \n\
         verify isEmpty trace\n\
         \x20   given argv: Args.get = [wrong]\n\
         \x20   isEmpty().result => true\n",
    )
    .expect("write program.av");

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let check = Command::new(aver_bin)
        .current_dir(&dir)
        .arg("check")
        .arg("program.av")
        .output()
        .expect("expected `aver check` to run");
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&check.stderr),
        String::from_utf8_lossy(&check.stdout)
    );
    assert!(
        combined.contains("expects a stub of type"),
        "expected oracle-signature mismatch diagnostic, got: {}",
        combined
    );
    assert!(
        combined.contains("Args.get"),
        "expected diagnostic to mention Args.get, got: {}",
        combined
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn aver_verify_trace_event_compares_to_full_effectevent_record() {
    // Oracle v1: the `.trace.event(k)` projection returns an
    // `Option<EffectEvent>`. With EffectEvent and Trace fields now
    // registered at the typechecker level, users can compare against a
    // full record literal `Option.Some(EffectEvent(method = ..., args
    // = [...]))` and the assertion passes on structural equality.
    let dir = temp_output_dir("aver-verify-trace-event-literal");
    std::fs::create_dir_all(&dir).expect("create temp dir");
    std::fs::write(
        dir.join("aver.toml"),
        "[independence]\nmode = \"complete\"\n",
    )
    .expect("write aver.toml");
    std::fs::write(
        dir.join("program.av"),
        "module Prog\n\
         \x20   intent = \"t\"\n\
         \n\
         fn fairDie(path: BranchPath, n: Int, min: Int, max: Int) -> Int\n\
         \x20   ? \"always 4\"\n\
         \x20   4\n\
         \n\
         fn hello() -> Int\n\
         \x20   ? \"roll + print\"\n\
         \x20   ! [Random.int, Console.print]\n\
         \x20   x = Random.int(1, 6)\n\
         \x20   Console.print(\"rolled 4\")\n\
         \x20   x\n\
         \n\
         verify hello trace\n\
         \x20   given rnd: Random.int = [fairDie]\n\
         \x20   hello().trace.event(1) => Option.Some(EffectEvent(method = \"Console.print\", args = [\"rolled 4\"], path = \"\"))\n",
    )
    .expect("write program.av");

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output = Command::new(aver_bin)
        .current_dir(&dir)
        .arg("verify")
        .arg("program.av")
        .output()
        .expect("expected `aver verify` to run");
    assert!(
        output.status.success(),
        "EffectEvent record comparison failed; {}",
        format_output(&output)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn aver_verify_trace_helper_boundary_filter_excludes_nested_emissions() {
    // Oracle v1: helper-boundary filter — only emissions whose
    // immediate caller fn_id matches the verified-fn root land in
    // the trace. Debug prints that come from functions the verified
    // fn calls internally (helpers) stay ghost — neither recorded
    // into the trace nor leaked to the terminal.
    let dir = temp_output_dir("aver-verify-trace-helper-boundary");
    std::fs::create_dir_all(&dir).expect("create temp dir");
    std::fs::write(
        dir.join("aver.toml"),
        "[independence]\nmode = \"complete\"\n",
    )
    .expect("write aver.toml");
    std::fs::write(
        dir.join("program.av"),
        "module Prog\n\
         \x20   intent = \"t\"\n\
         \n\
         fn helper(msg: String) -> Unit\n\
         \x20   ? \"prints from helper\"\n\
         \x20   ! [Console.print]\n\
         \x20   Console.print(msg)\n\
         \n\
         fn top() -> Int\n\
         \x20   ? \"direct + delegated print\"\n\
         \x20   ! [Console.print]\n\
         \x20   Console.print(\"direct\")\n\
         \x20   helper(\"via-helper\")\n\
         \x20   42\n\
         \n\
         verify top trace\n\
         \x20   top().trace.length() => 1\n\
         \x20   top().trace.contains(Console.print(\"direct\")) => true\n\
         \x20   top().trace.contains(Console.print(\"via-helper\")) => false\n",
    )
    .expect("write program.av");

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output = Command::new(aver_bin)
        .current_dir(&dir)
        .arg("verify")
        .arg("program.av")
        .output()
        .expect("expected `aver verify` to run");
    assert!(
        output.status.success(),
        "helper-boundary filter case failed; {}",
        format_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        !stdout.contains("via-helper"),
        "helper Console.print must be suppressed under trace collection, \
         got leaked via-helper in stdout: {}",
        stdout
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn aver_verify_trace_group_navigation_filters_by_source_order() {
    // Oracle v1: `.trace.group(N)` narrows the trace to emissions from
    // the N-th `!`/`?!` group in source order (0-based). Subsequent
    // `.length()` / `.event(k)` / `.contains(_)` operate on the
    // filtered sub-trace. Group ids reset per verify-trace case so
    // `.group(0)` points at the first group in every case.
    let dir = temp_output_dir("aver-verify-trace-group-nav");
    std::fs::create_dir_all(&dir).expect("create temp dir");
    std::fs::write(
        dir.join("aver.toml"),
        "[independence]\nmode = \"complete\"\n",
    )
    .expect("write aver.toml");
    std::fs::write(
        dir.join("program.av"),
        "module Prog\n\
         \x20   intent = \"t\"\n\
         \n\
         fn fairDie(path: BranchPath, n: Int, min: Int, max: Int) -> Int\n\
         \x20   ? \"always 4\"\n\
         \x20   4\n\
         \n\
         fn parallelRolls() -> Int\n\
         \x20   ? \"two rolls in parallel, sum\"\n\
         \x20   ! [Random.int, Console.print]\n\
         \x20   Console.print(\"pre\")\n\
         \x20   pair = (Random.int(1, 6), Random.int(1, 6))!\n\
         \x20   match pair\n\
         \x20       (a, b) -> a + b\n\
         \n\
         verify parallelRolls trace\n\
         \x20   given rnd: Random.int = [fairDie]\n\
         \x20   parallelRolls().trace.length() => 3\n\
         \x20   parallelRolls().trace.group(0).length() => 2\n\
         \x20   parallelRolls().trace.group(0).event(0) => Option.Some(EffectEvent(method = \"Random.int\", args = [1, 6], path = \"0\"))\n",
    )
    .expect("write program.av");

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output = Command::new(aver_bin)
        .current_dir(&dir)
        .arg("verify")
        .arg("program.av")
        .output()
        .expect("expected `aver verify` to run");
    assert!(
        output.status.success(),
        "trace.group(N) navigation failed; {}",
        format_output(&output)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn aver_verify_trace_group_branch_navigation_narrows_to_single_branch() {
    // Oracle v1: `.trace.group(N).branch(idx)` narrows the trace to
    // emissions from branch `idx` of the N-th `!`/`?!` group. Branches
    // are 0-based in both source and runtime; combined with `.group(N)`
    // (also 0-based in source order) users can target any single cell
    // of the branch-witness tree without knowing runtime ids.
    let dir = temp_output_dir("aver-verify-trace-branch-nav");
    std::fs::create_dir_all(&dir).expect("create temp dir");
    std::fs::write(
        dir.join("aver.toml"),
        "[independence]\nmode = \"complete\"\n",
    )
    .expect("write aver.toml");
    std::fs::write(
        dir.join("program.av"),
        "module Prog\n\
         \x20   intent = \"t\"\n\
         \n\
         fn fairDie(path: BranchPath, n: Int, min: Int, max: Int) -> Int\n\
         \x20   ? \"always 4\"\n\
         \x20   4\n\
         \n\
         fn twoBranches() -> Int\n\
         \x20   ? \"two rolls in parallel\"\n\
         \x20   ! [Random.int]\n\
         \x20   pair = (Random.int(1, 6), Random.int(7, 12))!\n\
         \x20   match pair\n\
         \x20       (a, b) -> a + b\n\
         \n\
         verify twoBranches trace\n\
         \x20   given rnd: Random.int = [fairDie]\n\
         \x20   twoBranches().trace.group(0).branch(0).length() => 1\n\
         \x20   twoBranches().trace.group(0).branch(1).length() => 1\n\
         \x20   twoBranches().trace.group(0).branch(0).event(0) => Option.Some(EffectEvent(method = \"Random.int\", args = [1, 6], path = \"0\"))\n\
         \x20   twoBranches().trace.group(0).branch(1).event(0) => Option.Some(EffectEvent(method = \"Random.int\", args = [7, 12], path = \"1\"))\n",
    )
    .expect("write program.av");

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output = Command::new(aver_bin)
        .current_dir(&dir)
        .arg("verify")
        .arg("program.av")
        .output()
        .expect("expected `aver verify` to run");
    assert!(
        output.status.success(),
        "group.branch navigation failed; {}",
        format_output(&output)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn aver_verify_trace_event_path_field_points_at_structural_position() {
    // Oracle v1: `EffectEvent.path` is the dewey-decimal string for
    // the structural position where the event was emitted — empty
    // at the sequential level (canonical `BranchPath.root`) and a
    // dot-separated branch index inside groups. Bridges recording
    // JSON coordinates and spec-side BranchPath without a separate
    // `.path()` accessor method.
    let dir = temp_output_dir("aver-verify-trace-event-path-field");
    std::fs::create_dir_all(&dir).expect("create temp dir");
    std::fs::write(
        dir.join("aver.toml"),
        "[independence]\nmode = \"complete\"\n",
    )
    .expect("write aver.toml");
    std::fs::write(
        dir.join("program.av"),
        "module Prog\n\
         \x20   intent = \"t\"\n\
         \n\
         fn fairDie(path: BranchPath, n: Int, min: Int, max: Int) -> Int\n\
         \x20   ? \"always 4\"\n\
         \x20   4\n\
         \n\
         fn withSeqAndGroup() -> Int\n\
         \x20   ? \"seq roll plus a parallel pair\"\n\
         \x20   ! [Random.int, Console.print]\n\
         \x20   Console.print(\"seq\")\n\
         \x20   pair = (Random.int(1, 6), Random.int(7, 12))!\n\
         \x20   match pair\n\
         \x20       (a, b) -> a + b\n\
         \n\
         verify withSeqAndGroup trace\n\
         \x20   given rnd: Random.int = [fairDie]\n\
         \x20   withSeqAndGroup().trace.event(0) =>\n\
         \x20       Option.Some(EffectEvent(method = \"Console.print\", args = [\"seq\"], path = \"\"))\n\
         \x20   withSeqAndGroup().trace.group(0).branch(0).event(0) =>\n\
         \x20       Option.Some(EffectEvent(method = \"Random.int\", args = [1, 6], path = \"0\"))\n\
         \x20   withSeqAndGroup().trace.group(0).branch(1).event(0) =>\n\
         \x20       Option.Some(EffectEvent(method = \"Random.int\", args = [7, 12], path = \"1\"))\n",
    )
    .expect("write program.av");

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output = Command::new(aver_bin)
        .current_dir(&dir)
        .arg("verify")
        .arg("program.av")
        .output()
        .expect("expected `aver verify` to run");
    assert!(
        output.status.success(),
        "EffectEvent.path field assertion failed; {}",
        format_output(&output)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn aver_verify_trace_case_rhs_wraps_to_indented_next_line() {
    // Oracle v1: verify case RHS may wrap to an indented next line —
    // long event-literal records (Http responses, nested records)
    // don't fit on one line, and cramming them there makes the
    // surface feel hostile. `A =>` followed by an indented
    // expression parses the same as the single-line form.
    let dir = temp_output_dir("aver-verify-trace-rhs-wrap");
    std::fs::create_dir_all(&dir).expect("create temp dir");
    std::fs::write(
        dir.join("aver.toml"),
        "[independence]\nmode = \"complete\"\n",
    )
    .expect("write aver.toml");
    std::fs::write(
        dir.join("program.av"),
        "module Prog\n\
         \x20   intent = \"t\"\n\
         \n\
         fn fakeFetch(path: BranchPath, n: Int, url: String) -> Result<HttpResponse, String>\n\
         \x20   ? \"deterministic\"\n\
         \x20   Result.Ok(HttpResponse(status = 200, body = \"ok\", headers = {}))\n\
         \n\
         fn app() -> Result<HttpResponse, String>\n\
         \x20   ? \"fetch one\"\n\
         \x20   ! [Http.get]\n\
         \x20   Http.get(\"https://example.test/api\")\n\
         \n\
         verify app trace\n\
         \x20   given stub: Http.get = [fakeFetch]\n\
         \x20   app().trace.event(0) =>\n\
         \x20       Option.Some(EffectEvent(method = \"Http.get\", args = [\"https://example.test/api\"], path = \"\"))\n",
    )
    .expect("write program.av");

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output = Command::new(aver_bin)
        .current_dir(&dir)
        .arg("verify")
        .arg("program.av")
        .output()
        .expect("expected `aver verify` to run");
    assert!(
        output.status.success(),
        "multi-line case RHS failed; {}",
        format_output(&output)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn aver_verify_trace_projection_edge_cases_degrade_gracefully() {
    // Oracle v1: out-of-range indices and missing methods return the
    // natural empty / None values instead of erroring. Lets users
    // phrase negative assertions (`.contains(X) => false`,
    // `.event(k) => Option.None`) without special-casing what "X
    // never happened" looks like.
    let dir = temp_output_dir("aver-verify-trace-edge-cases");
    std::fs::create_dir_all(&dir).expect("create temp dir");
    std::fs::write(
        dir.join("aver.toml"),
        "[independence]\nmode = \"complete\"\n",
    )
    .expect("write aver.toml");
    std::fs::write(
        dir.join("program.av"),
        "module Prog\n\
         \x20   intent = \"t\"\n\
         \n\
         fn fairDie(path: BranchPath, n: Int, min: Int, max: Int) -> Int\n\
         \x20   ? \"always 4\"\n\
         \x20   4\n\
         \n\
         fn hello() -> Int\n\
         \x20   ? \"roll + print\"\n\
         \x20   ! [Random.int, Console.print]\n\
         \x20   x = Random.int(1, 6)\n\
         \x20   Console.print(\"rolled 4\")\n\
         \x20   x\n\
         \n\
         verify hello trace\n\
         \x20   given rnd: Random.int = [fairDie]\n\
         \x20   hello().trace.group(99).length() => 0\n\
         \x20   hello().trace.group(99).event(0) => Option.None\n\
         \x20   hello().trace.contains(Console.error) => false\n\
         \x20   hello().trace.event(99) => Option.None\n",
    )
    .expect("write program.av");

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output = Command::new(aver_bin)
        .current_dir(&dir)
        .arg("verify")
        .arg("program.av")
        .output()
        .expect("expected `aver verify` to run");
    assert!(
        output.status.success(),
        "trace projection edge cases failed; {}",
        format_output(&output)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn proof_accepts_complete_independence_mode() {
    let dir = temp_output_dir("aver-proof-complete");
    std::fs::create_dir_all(&dir).expect("create temp dir");
    let output = run_aver_proof_in_dir(
        &dir,
        "module Prog\n    intent = \"test\"\n\nfn absVal(x: Int) -> Int\n    ? \"abs\"\n    match x < 0\n        true  -> 0 - x\n        false -> x\n",
        "[independence]\nmode = \"complete\"\n",
    );
    // Success here means aver proof at least started generating; it may fail
    // later if lake isn't installed or for other reasons, but it must NOT
    // fail with the independence-mode rejection.
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        !stderr.contains("aver.toml has [independence] mode"),
        "complete mode must not trigger the mode rejection; got: {}",
        stderr
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn proof_dafny_verifies_law_auto_when_dafny_is_available() {
    assert_dafny_verifies("examples/formal/law_auto.av", "aver-dafny-law-auto");
}

#[test]
fn proof_dafny_verifies_spec_laws_when_dafny_is_available() {
    assert_dafny_verifies("examples/formal/spec_laws.av", "aver-dafny-spec-laws");
}

#[test]
fn proof_dafny_verifies_oracle_independent_products_when_dafny_is_available() {
    assert_dafny_verifies(
        "examples/formal/oracle_independent_products.av",
        "aver-dafny-oracle-products",
    );
}

#[test]
fn proof_dafny_verifies_map_when_dafny_is_available() {
    assert_dafny_verifies("examples/data/map.av", "aver-dafny-map");
}

// --- expanded coverage (post-IR-migration audit, 0.22.0) ---

#[test]
fn proof_export_builds_clock_as_data_when_lake_is_available() {
    assert_proof_builds("examples/formal/clock_as_data.av", "aver-proof-clock");
}

#[test]
fn proof_dafny_verifies_clock_as_data_when_dafny_is_available() {
    assert_dafny_verifies("examples/formal/clock_as_data.av", "aver-dafny-clock");
}

#[test]
fn proof_export_builds_file_store_pure_core_when_lake_is_available() {
    assert_proof_builds(
        "examples/formal/file_store_pure_core.av",
        "aver-proof-file-store-pure",
    );
}

#[test]
fn proof_dafny_verifies_file_store_pure_core_when_dafny_is_available() {
    assert_dafny_verifies(
        "examples/formal/file_store_pure_core.av",
        "aver-dafny-file-store-pure",
    );
}

#[test]
fn proof_export_builds_oracle_trace_when_lake_is_available() {
    assert_proof_builds("examples/formal/oracle_trace.av", "aver-proof-oracle-trace");
}

#[test]
fn proof_dafny_verifies_oracle_trace_when_dafny_is_available() {
    assert_dafny_verifies("examples/formal/oracle_trace.av", "aver-dafny-oracle-trace");
}

#[test]
fn proof_export_builds_terminal_size_snapshot_when_lake_is_available() {
    assert_proof_builds(
        "examples/formal/terminal_size_snapshot.av",
        "aver-proof-terminal-size",
    );
}

#[test]
fn proof_dafny_verifies_terminal_size_snapshot_when_dafny_is_available() {
    assert_dafny_verifies(
        "examples/formal/terminal_size_snapshot.av",
        "aver-dafny-terminal-size",
    );
}

#[test]
fn proof_export_builds_trust_check_when_lake_is_available() {
    assert_proof_builds("examples/formal/trust_check.av", "aver-proof-trust-check");
}

#[test]
fn proof_dafny_verifies_trust_check_when_dafny_is_available() {
    assert_dafny_verifies("examples/formal/trust_check.av", "aver-dafny-trust-check");
}

#[test]
fn proof_export_builds_date_when_lake_is_available() {
    assert_proof_builds("examples/data/date.av", "aver-proof-date");
}

#[test]
fn proof_dafny_verifies_date_when_dafny_is_available() {
    // `parseIntSlice(s, from, to)` is emitted via the safe
    // `StringSlice` helper instead of raw `s[from..to]`, so the
    // slice carries Aver's clamp-to-empty semantics into Dafny and
    // there's no range obligation to discharge in the caller.
    assert_dafny_verifies("examples/data/date.av", "aver-dafny-date");
}

#[test]
fn proof_export_cross_module_recursive_fns_get_per_module_fn_contracts() {
    // Round-5 audit follow-up: two dep modules each declaring a
    // recursive `countdown(n: Int) -> Int` with the canonical
    // IntCountdown shape used to emit `partial def` in both modules
    // even though the standalone single-module export produced a
    // proper fuel-encoded def. Two coupled gaps:
    //   1. The proof-lower pipeline built `inputs.recursive_fns`
    //      from entry's analyze only — module fns never reached the
    //      IntCountdown classifier (entry has no countdown → empty
    //      entry-recursive set).
    //   2. `populate_fn_contracts` keyed `ir.fn_contracts` by bare
    //      fn name, so even when both modules' contracts were
    //      populated they collided on `"countdown"`.
    // Round-5 plumbs union'd `recursive_fns` through pipeline AND
    // keys `fn_contracts` by canonical `Module.fn`. Lookup-side
    // helpers `find_fn_contract` / `fn_contract_exists` walk back
    // to the canonical slot.
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let root = temp_output_dir("aver-proof-cross-module-fn-contracts");
    std::fs::create_dir_all(&root).expect("create root");

    std::fs::write(
        root.join("CountdownA.av"),
        "module CountdownA\n\
         \x20   exposes [countdown]\n\
         \x20   intent = \"Plain countdown.\"\n\
         \x20   effects []\n\
         \n\
         fn countdown(n: Int) -> Int\n\
         \x20   ? \"Countdown to 0.\"\n\
         \x20   match n <= 0\n\
         \x20       true -> 0\n\
         \x20       false -> countdown(n - 1)\n",
    )
    .expect("write CountdownA.av");
    std::fs::write(
        root.join("CountdownB.av"),
        "module CountdownB\n\
         \x20   exposes [countdown]\n\
         \x20   intent = \"Sum on countdown.\"\n\
         \x20   effects []\n\
         \n\
         fn countdown(n: Int) -> Int\n\
         \x20   ? \"Countdown summing n.\"\n\
         \x20   match n <= 0\n\
         \x20       true -> 0\n\
         \x20       false -> n + countdown(n - 1)\n",
    )
    .expect("write CountdownB.av");
    std::fs::write(
        root.join("entry.av"),
        "module Entry\n\
         \x20   depends [CountdownA, CountdownB]\n\
         \x20   intent = \"Touch both modules so each surfaces in proof IR.\"\n\
         \n\
         fn main() -> Int\n\
         \x20   0\n",
    )
    .expect("write entry.av");

    let out_dir = root.join("out");
    let proof = Command::new(aver_bin)
        .current_dir(&root)
        .arg("proof")
        .arg("entry.av")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver proof");
    assert!(
        proof.status.success(),
        "`aver proof` failed:\n{}",
        format_output(&proof)
    );

    let a_lean =
        std::fs::read_to_string(out_dir.join("CountdownA.lean")).expect("read CountdownA.lean");
    let b_lean =
        std::fs::read_to_string(out_dir.join("CountdownB.lean")).expect("read CountdownB.lean");

    assert!(
        a_lean.contains("def countdown__fuel"),
        "CountdownA.countdown must emit fuel-encoded def, not `partial def`:\n{a_lean}"
    );
    assert!(
        b_lean.contains("def countdown__fuel"),
        "CountdownB.countdown must emit fuel-encoded def, not `partial def`:\n{b_lean}"
    );
    assert!(
        !a_lean.contains("partial def countdown"),
        "CountdownA.lean must not regress to `partial def countdown`:\n{a_lean}"
    );
    assert!(
        !b_lean.contains("partial def countdown"),
        "CountdownB.lean must not regress to `partial def countdown`:\n{b_lean}"
    );

    let _ = std::fs::remove_dir_all(&root);
}

#[test]
fn proof_export_module_owned_native_guarded_resolves_correct_fn_id() {
    // PR 12 Scope A finalization: the Lean native-guarded emit path
    // (`emit_native_guarded_int_countdown_fn`) used to derive the
    // recursive fn's `FnId` via `FnKey::entry(&fd.name)`. For any
    // module-owned native-guarded recursive fn that would either
    // panic on the missing entry slot, or silently target an
    // entry-scope same-bare-name fn. After the followup commit the
    // lookup goes through `fn_id_for_decl(ctx, fd)` — pointer-eq
    // scope, the same path `ProofIR.fn_contracts` keys by.
    //
    // This test exercises the specific bug class: two same-bare
    // `down(n: Int) -> Int` native-guarded fns, one in a dep module
    // and one at entry. Both classify as `IntCountdownGuarded`, both
    // emit `def down__aux`, and the rewriter pins each one's
    // recursive call to its OWN `FnId` rather than crossing wires.
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let root = temp_output_dir("aver-proof-module-native-guarded");
    std::fs::create_dir_all(&root).expect("create root");

    // Worker.av: closed-world (not exposed) `down` countdown with a
    // public `run` calling `down(n)` under a `n >= 0` guard so the
    // classifier accepts it as IntCountdownGuarded.
    std::fs::write(
        root.join("Worker.av"),
        "module Worker\n\
         \x20   exposes [run]\n\
         \x20   intent = \"Closed-world native-guarded countdown.\"\n\
         \x20   effects []\n\
         \n\
         fn down(n: Int) -> Int\n\
         \x20   ? \"Countdown to 0.\"\n\
         \x20   match n\n\
         \x20       0 -> 1\n\
         \x20       _ -> down(n - 1)\n\
         \n\
         fn run(n: Int) -> Int\n\
         \x20   ? \"Public entry; guards n >= 0 before down.\"\n\
         \x20   match n < 0\n\
         \x20       true  -> 0\n\
         \x20       false -> down(n)\n",
    )
    .expect("write Worker.av");
    // Entry: same-bare `down` with the SAME body shape so both
    // classify as IntCountdownGuarded. If the rewriter pinned by
    // bare name the entry's `down__aux` would consume Worker.down's
    // FnId (or vice versa) and the rewritten body would call the
    // wrong target.
    std::fs::write(
        root.join("entry.av"),
        "module Entry\n\
         \x20   depends [Worker]\n\
         \x20   intent = \"Same-bare-name native-guarded countdown alongside Worker.down.\"\n\
         \x20   effects []\n\
         \n\
         fn down(n: Int) -> Int\n\
         \x20   ? \"Entry's own countdown — bare-name twin of Worker.down.\"\n\
         \x20   match n\n\
         \x20       0 -> 2\n\
         \x20       _ -> down(n - 1)\n\
         \n\
         fn launch(n: Int) -> Int\n\
         \x20   ? \"Guards n >= 0 before calling Entry.down.\"\n\
         \x20   match n < 0\n\
         \x20       true  -> 0\n\
         \x20       false -> down(n)\n\
         \n\
         fn main() -> Int\n\
         \x20   launch(3) + Worker.run(5)\n",
    )
    .expect("write entry.av");

    let out_dir = root.join("out");
    let proof = Command::new(aver_bin)
        .current_dir(&root)
        .arg("proof")
        .arg("entry.av")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver proof");
    assert!(
        proof.status.success(),
        "`aver proof` failed (expected to succeed without FnKey::entry panic):\n{}",
        format_output(&proof)
    );

    let worker_lean = std::fs::read_to_string(out_dir.join("Worker.lean"))
        .expect("read Worker.lean (module-owned native-guarded emit must succeed)");
    // Entry file basename is project-name-derived; the proof exporter
    // capitalises the project name to produce a Lean module ident
    // (`entry.av` → `Entry.lean`). macOS APFS is case-insensitive so a
    // lowercase path would silently match locally — on Linux CI it
    // does not, so look up the canonical capitalised form.
    let entry_lean = std::fs::read_to_string(out_dir.join("Entry.lean")).expect("read Entry.lean");

    // Both modules carry their OWN native-guarded aux def. If the
    // rewriter targeted the wrong FnId only one would emit, or both
    // would inline the same body.
    assert!(
        worker_lean.contains("def down__aux"),
        "Worker.lean must contain its own native-guarded aux def:\n{worker_lean}"
    );
    assert!(
        entry_lean.contains("def down__aux"),
        "entry.lean must contain its own native-guarded aux def:\n{entry_lean}"
    );
    // The hard regression assertion: the rewritten body MUST contain
    // the aux call carrying the `(by omega)` OMEGA_PROOF_SENTINEL
    // tail. With the pre-fix bare-name `FnKey::entry("down")` lookup
    // Worker.down's body would walk past every callsite (the entry
    // FnId never matches Worker.down's resolved `ResolvedCallee::Fn`
    // calls), so the recursive `down(n - 1)` stays unchanged and
    // Lean's termination check loses the precondition handle. Pin
    // both files: Worker AND entry produce the rewritten aux call.
    assert!(
        worker_lean.contains("down__aux (n - 1) (by omega)"),
        "Worker.down__aux body must contain the rewritten recursive call \
         `down__aux (n - 1) (by omega)` — the rewriter dropped it:\n{worker_lean}"
    );
    assert!(
        entry_lean.contains("down__aux (n - 1) (by omega)"),
        "entry.down__aux body must contain the rewritten recursive call \
         `down__aux (n - 1) (by omega)` — the rewriter dropped it:\n{entry_lean}"
    );
    // Worker's base arm is `0 -> 1`; entry's is `0 -> 2`. If the
    // rewriter cross-wired the targets the base literal would leak
    // across files.
    let worker_idx = worker_lean
        .find("def down__aux")
        .expect("down__aux present in Worker.lean");
    let worker_aux = &worker_lean[worker_idx..];
    assert!(
        worker_aux.contains("then 1"),
        "Worker.down__aux must keep its OWN base arm literal (1):\n{worker_aux}"
    );
    let entry_idx = entry_lean
        .find("def down__aux")
        .expect("down__aux present in entry.lean");
    let entry_aux = &entry_lean[entry_idx..];
    assert!(
        entry_aux.contains("then 2"),
        "entry.down__aux must keep its OWN base arm literal (2):\n{entry_aux}"
    );

    let _ = std::fs::remove_dir_all(&root);
}

#[test]
fn proof_export_cross_module_differentiated_recursion_shapes_emit_per_module() {
    // Round-6 finding (audit of round 5): the prior test had both
    // modules use the SAME recursion shape (IntCountdown), so even
    // a buggy scope-naive lookup could "accidentally pass" by
    // returning whichever module's identical contract walked first.
    // This test wires module A's `walker(n)` as IntCountdown and
    // module B's `walker(xs)` as ListStructural — different param
    // types AND different fuel metrics. With scope-naive lookup
    // (`find_fn_contract(ctx, "walker")` → first-walked module's
    // contract) the second module's emit would either use the
    // wrong shape or fall back to `partial def`. With pointer-eq
    // scope resolution each module's `walker` lands its OWN
    // classification.
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let root = temp_output_dir("aver-proof-cross-module-shapes");
    std::fs::create_dir_all(&root).expect("create root");

    std::fs::write(
        root.join("WalkerA.av"),
        "module WalkerA\n\
         \x20   exposes [walker]\n\
         \x20   intent = \"Int countdown shape.\"\n\
         \x20   effects []\n\
         \n\
         fn walker(n: Int) -> Int\n\
         \x20   ? \"Countdown to 0.\"\n\
         \x20   match n <= 0\n\
         \x20       true -> 0\n\
         \x20       false -> walker(n - 1)\n",
    )
    .expect("write WalkerA.av");
    std::fs::write(
        root.join("WalkerB.av"),
        "module WalkerB\n\
         \x20   exposes [walker]\n\
         \x20   intent = \"List structural shape.\"\n\
         \x20   effects []\n\
         \n\
         fn walker(xs: List<Int>) -> Int\n\
         \x20   ? \"Sum elements.\"\n\
         \x20   match xs\n\
         \x20       [] -> 0\n\
         \x20       [x, ..rest] -> x + walker(rest)\n",
    )
    .expect("write WalkerB.av");
    std::fs::write(
        root.join("entry.av"),
        "module Entry\n\
         \x20   depends [WalkerA, WalkerB]\n\
         \x20   intent = \"Touch both walker modules.\"\n\
         \n\
         fn main() -> Int\n\
         \x20   0\n",
    )
    .expect("write entry.av");

    let out_dir = root.join("out");
    let proof = Command::new(aver_bin)
        .current_dir(&root)
        .arg("proof")
        .arg("entry.av")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver proof");
    assert!(
        proof.status.success(),
        "`aver proof` failed:\n{}",
        format_output(&proof)
    );

    let a_lean = std::fs::read_to_string(out_dir.join("WalkerA.lean")).expect("read WalkerA.lean");
    let b_lean = std::fs::read_to_string(out_dir.join("WalkerB.lean")).expect("read WalkerB.lean");

    // WalkerA is IntCountdown → fuel-encoded `def walker__fuel
    // (fuel : Nat) (n : Int) : Int`.
    assert!(
        a_lean.contains("def walker__fuel"),
        "WalkerA.walker (IntCountdown) must emit fuel-encoded def:\n{a_lean}"
    );
    assert!(
        a_lean.contains("(n : Int)"),
        "WalkerA.walker fuel sig must carry Int param `n`:\n{a_lean}"
    );

    // WalkerB is ListStructural → backend may emit either a
    // structural-recursion `def walker (xs : List Int)` or a
    // fuel-encoded variant depending on classifier path. Either is
    // fine; the wrong-shape failure mode would be either (a)
    // landing the Int sig from WalkerA, or (b) emitting
    // `partial def walker` because the scope-naive lookup hit the
    // wrong contract and the emit fell through.
    assert!(
        b_lean.contains("walker") && (b_lean.contains("List Int") || b_lean.contains("(xs :")),
        "WalkerB.walker must carry the List<Int> signature, not WalkerA's Int sig:\n{b_lean}"
    );
    assert!(
        !b_lean.contains("partial def walker"),
        "WalkerB.walker must not regress to `partial def` — scope-naive lookup \
         leaked the wrong contract:\n{b_lean}"
    );
    // Defence: WalkerA must not pick up WalkerB's List signature.
    assert!(
        !a_lean.contains("List Int"),
        "WalkerA.walker leaked WalkerB's List signature:\n{a_lean}"
    );

    let _ = std::fs::remove_dir_all(&root);
}

#[test]
fn proof_export_cross_module_refined_types_keep_distinct_predicates() {
    // Review findings 2 + 3 (round 2): two modules each declaring a
    // refined `Natural` (different predicates) must each carry its
    // own predicate into the Lean / Dafny export. Pre-fix
    // `populate_refined_types` keyed `refined_types` by bare name
    // and called the unscoped `refinement_info_for` — both `A` and
    // `B` ended up sharing whichever predicate walked first. The
    // canonical-key + scoped-info path gives each module its own
    // slot; `find_refined_type_scoped` then resolves bare lookups
    // inside each module's emit pass to the local entry.
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let root = temp_output_dir("aver-proof-cross-module-refined");
    std::fs::create_dir_all(&root).expect("create root");

    std::fs::write(
        root.join("AAA.av"),
        "module AAA\n\
         \x20   exposes [fromInt]\n\
         \x20   exposes opaque [Natural]\n\
         \x20   intent = \"Module AAA's Natural — non-negative.\"\n\
         \x20   effects []\n\
         \n\
         record Natural\n\
         \x20   value: Int\n\
         \n\
         fn fromInt(n: Int) -> Result<Natural, String>\n\
         \x20   ? \"Smart constructor — non-negative.\"\n\
         \x20   match n >= 0\n\
         \x20       true  -> Result.Ok(Natural(value = n))\n\
         \x20       false -> Result.Err(\"AAA: must be non-negative\")\n",
    )
    .expect("write AAA.av");

    std::fs::write(
        root.join("BBB.av"),
        "module BBB\n\
         \x20   exposes [fromInt]\n\
         \x20   exposes opaque [Natural]\n\
         \x20   intent = \"Module BBB's Natural — at least 10.\"\n\
         \x20   effects []\n\
         \n\
         record Natural\n\
         \x20   value: Int\n\
         \n\
         fn fromInt(n: Int) -> Result<Natural, String>\n\
         \x20   ? \"Smart constructor — at least 10.\"\n\
         \x20   match n >= 10\n\
         \x20       true  -> Result.Ok(Natural(value = n))\n\
         \x20       false -> Result.Err(\"BBB: must be >= 10\")\n",
    )
    .expect("write BBB.av");

    std::fs::write(
        root.join("entry.av"),
        "module Entry\n\
         \x20   depends [AAA, BBB]\n\
         \x20   intent = \"Touches both Naturals so both surface in the proof IR.\"\n\
         \n\
         fn main() -> Int\n\
         \x20   0\n",
    )
    .expect("write entry.av");

    let out_dir = root.join("out");
    let proof = Command::new(aver_bin)
        .current_dir(&root)
        .arg("proof")
        .arg("entry.av")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver proof");
    assert!(
        proof.status.success(),
        "`aver proof` failed:\n{}",
        format_output(&proof)
    );

    let aaa_lean = std::fs::read_to_string(out_dir.join("AAA.lean")).expect("read AAA.lean");
    let bbb_lean = std::fs::read_to_string(out_dir.join("BBB.lean")).expect("read BBB.lean");

    // AAA's Natural carries `>= 0`; BBB's carries `>= 10`. Each
    // module's emit pass must resolve its own predicate, not the
    // other's. Before the scope fix, populate kept only the first
    // walked predicate under bare key `Natural` and both modules
    // emitted the same subtype.
    assert!(
        aaa_lean.contains("abbrev Natural") && aaa_lean.contains(">= 0"),
        "AAA.lean must abbrev Natural with `>= 0`; got:\n{aaa_lean}"
    );
    assert!(
        bbb_lean.contains("abbrev Natural") && bbb_lean.contains(">= 10"),
        "BBB.lean must abbrev Natural with `>= 10`; got:\n{bbb_lean}"
    );
    // Defense in depth: AAA's emit must not carry BBB's predicate
    // or vice versa.
    assert!(
        !aaa_lean.contains(">= 10"),
        "AAA.lean leaked BBB's predicate; got:\n{aaa_lean}"
    );
    assert!(
        !bbb_lean.contains("n >= 0 "),
        "BBB.lean leaked AAA's predicate; got:\n{bbb_lean}"
    );

    // Round-3 finding 1: the prior round only checked Lean. `pick_
    // witness` was un-scoped and tried only `[0, 1, -1]` candidates,
    // so `BBB.Natural`'s `n >= 10` got `witness = None` and Dafny
    // silently fell back to `witness 0` — which violates the
    // predicate. The scoped picker now (a) scopes the smart-ctor
    // walk to the same module and (b) sweeps higher candidates.
    let dafny_out = root.join("out-dafny");
    let dafny_proof = Command::new(aver_bin)
        .current_dir(&root)
        .arg("proof")
        .arg("entry.av")
        .arg("--backend")
        .arg("dafny")
        .arg("-o")
        .arg(&dafny_out)
        .output()
        .expect("aver proof --backend dafny");
    assert!(
        dafny_proof.status.success(),
        "`aver proof --backend dafny` failed:\n{}",
        format_output(&dafny_proof)
    );

    let aaa_dfy = std::fs::read_to_string(dafny_out.join("AAA.dfy")).expect("read AAA.dfy");
    let bbb_dfy = std::fs::read_to_string(dafny_out.join("BBB.dfy")).expect("read BBB.dfy");

    assert!(
        aaa_dfy.contains("type Natural") && aaa_dfy.contains("n >= 0"),
        "AAA.dfy must declare `type Natural` with `n >= 0`; got:\n{aaa_dfy}"
    );
    assert!(
        bbb_dfy.contains("type Natural") && bbb_dfy.contains("n >= 10"),
        "BBB.dfy must declare `type Natural` with `n >= 10`; got:\n{bbb_dfy}"
    );
    let bbb_witness_line = bbb_dfy
        .lines()
        .find(|l| l.contains("type Natural"))
        .expect("BBB.dfy must declare `type Natural`");
    assert!(
        !bbb_witness_line.contains("witness 0"),
        "BBB.Natural with `n >= 10` must NOT fall back to `witness 0`; \
         got line:\n{bbb_witness_line}"
    );

    // Round-4 finding 3: text checks aren't enough — the witness
    // must actually satisfy the predicate or Dafny rejects the
    // subset type at verify time. Run `dafny verify` on the
    // generated project to catch unsound witnesses going forward.
    // Skipped silently when dafny isn't on PATH (matches the
    // pattern used by `assert_dafny_verifies`).
    if Command::new("dafny").arg("--version").output().is_ok() {
        let verify = Command::new("dafny")
            .current_dir(&dafny_out)
            .arg("verify")
            .arg("Entry.dfy")
            .output()
            .expect("dafny verify");
        assert!(
            verify.status.success(),
            "`dafny verify` rejected the cross-module refinement output \
             — most likely a witness violates its predicate:\n{}",
            format_output(&verify)
        );
    }

    let _ = std::fs::remove_dir_all(&root);
}

#[test]
fn proof_export_lake_builds_red_black_tree_after_singleton_and_fuel_gates() {
    // Issue #128: red_black_tree.av carried 44 lake errors after the
    // #123 path-shadow / `.val` fixes. The diagnosis in the issue
    // text (anonymous `{...}` constructor notation) didn't match the
    // real output — match arms already used qualified positional
    // syntax. The actual failure was two coupled emit shapes:
    //
    //   1. Laws with singleton-domain givens and a RHS that didn't
    //      reference any given (`checkRight L V R = Tree.Black Empty
    //      1 Empty`) emit a `∀ L V R, …` universal that's vacuous or
    //      outright false. The `induction L with …` fallback chose
    //      by the auto-proof matcher then failed to close.
    //   2. Laws calling fuel-bounded fns that the proof-mode
    //      classifier rejected (`size`, `toSorted`) emit
    //      `induction t with …` against `__fuel`-wrapped helpers
    //      whose recursive shape `simp` can't drive.
    //
    // Both gated at the universal emit step; sample / checked_domain
    // lemmas remain (concrete inputs stay decidable). Lake build
    // succeeds; `aver verify` runtime hits every declared case.
    //
    // Sorry budget 1 (was 2): the `detect.rs` resolved-subject fix (which lets
    // Dafny/Z3 prove the Peano-fold homomorphism family) also admits
    // `size` / `toSorted`'s structural recursion into the proof subset, so
    // their two universals EMIT a `∀ … induction t with …` proof rather than
    // gating to sample-only. On Lean those can't close on the ladder — the
    // `__fuel`-wrapped recursion needs a fuel-saturation lemma the auto
    // template lacks.
    //
    // The drop 2→1 is the discovery feedback loop, część A: `toSorted_law_
    // sizePreserved` now closes its TACTIC BLOCK via the fast path `simp only
    // [size_law_equalsSortedLen] <;> omega`, referencing the earlier sibling
    // theorem — so it no longer emits its OWN `sorry`. This is a textual-count
    // drop only, NOT a new genuine universal: `size_law_equalsSortedLen` still
    // `sorry`s, so the consumer inherits `sorryAx` and the `universal` metric
    // correctly stays false for both (verified via `#print axioms`). The
    // honest coverage number is unchanged; only the weaker sorry-count metric
    // moved. Z3 supplies the missing induction automatically, which is why the
    // same laws DO prove on the Dafny backend.
    assert_proof_builds_with_sorry_budget(
        "examples/data/red_black_tree.av",
        "aver-proof-red-black-tree",
        1,
    );
}

#[test]
fn proof_export_gates_trace_projection_law_lhs_as_runtime_only() {
    // Issue #127: a `verify fn trace law` whose LHS projects through
    // `.trace.{event,group,branch}` references the runtime trace
    // buffer, not the lifted fn's return. The lifted Lean / Dafny fn
    // has no `.trace` field — emitting `fn().trace.event 0` as a
    // theorem (universal or sample) produces invalid-field-notation
    // errors. Backends now emit a `runtime-only` comment instead and
    // skip the universal/sample theorem. The `aver verify` runtime
    // path still exercises the law under its stubs.
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");

    let lean_dir = temp_output_dir("aver-proof-issue127-lean");
    let proof = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("examples/formal/hostile_order_axis.av")
        .arg("-o")
        .arg(&lean_dir)
        .output()
        .expect("expected `aver proof` to run");
    assert!(
        proof.status.success(),
        "`aver proof` failed:\n{}",
        format_output(&proof)
    );

    let entry_text = std::fs::read_to_string(lean_dir.join("HostileOrderAxis.lean"))
        .expect("read HostileOrderAxis.lean");

    assert!(
        entry_text.contains(
            "-- verify law rollPair.firstEventOfFirstBranch: \
             trace-projection LHS is runtime-only"
        ),
        "expected runtime-only gate marker for firstEventOfFirstBranch in \
         entry Lean; got:\n{entry_text}"
    );
    assert!(
        entry_text.contains(
            "-- verify law rollPair.firstEventOfSecondBranch: \
             trace-projection LHS is runtime-only"
        ),
        "expected runtime-only gate marker for firstEventOfSecondBranch in \
         entry Lean; got:\n{entry_text}"
    );
    // Defense in depth: the universal/sample theorems must not slip
    // back in — their LHS triggers Lean's invalid-field-notation
    // diagnostic on the bare `(Int × Int)` return.
    assert!(
        !entry_text.contains("rollPair_law_firstEventOfFirstBranch"),
        "universal theorem leaked through the trace-projection gate; \
         got:\n{entry_text}"
    );
    assert!(
        !entry_text.contains(").event 0"),
        "trace projection chain leaked into elaborated Lean; got:\n{entry_text}"
    );
    assert!(
        !entry_text.contains("EffectEvent"),
        "EffectEvent literal leaked into elaborated Lean (gate should \
         keep it out entirely); got:\n{entry_text}"
    );

    let _ = std::fs::remove_dir_all(&lean_dir);

    let dafny_dir = temp_output_dir("aver-proof-issue127-dafny");
    let proof = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("examples/formal/hostile_order_axis.av")
        .arg("--backend")
        .arg("dafny")
        .arg("-o")
        .arg(&dafny_dir)
        .output()
        .expect("expected `aver proof --backend dafny` to run");
    assert!(
        proof.status.success(),
        "`aver proof --backend dafny` failed:\n{}",
        format_output(&proof)
    );

    let dafny_entry = std::fs::read_to_string(dafny_dir.join("HostileOrderAxis.dfy"))
        .expect("read HostileOrderAxis.dfy");

    assert!(
        dafny_entry.contains(
            "// Law rollPair.firstEventOfFirstBranch: trace-projection LHS is runtime-only"
        ),
        "expected Dafny runtime-only gate marker; got:\n{dafny_entry}"
    );
    assert!(
        !dafny_entry.contains("lemma {:fuel rollPair, 5} rollPair_firstEventOfFirstBranch"),
        "universal lemma leaked through the trace-projection gate; got:\n{dafny_entry}"
    );
    assert!(
        !dafny_entry.contains(".trace.group"),
        "trace projection chain leaked into elaborated Dafny; got:\n{dafny_entry}"
    );
    assert!(
        !dafny_entry.contains("EffectEvent"),
        "EffectEvent literal leaked into elaborated Dafny (gate should \
         keep it out entirely); got:\n{dafny_entry}"
    );

    let _ = std::fs::remove_dir_all(&dafny_dir);
}

/// Phase 2/2d acceptance (lemma discovery): `aver proof <rle> --discover`
/// enumerates candidate equations, VM-filters them, and kernel-proves the
/// `decode_append` survivor via `lake build` — end to end, with no
/// RLE-specific recognizer. Skips when `lake` is unavailable.
#[test]
fn discover_kernel_proves_decode_append_when_lake_is_available() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping discovery proof test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    // Fresh `-o` so the run always discovers (no committed-lemma replay).
    let output_dir = temp_output_dir("aver-discover-rle");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("examples/data/rle.av")
        .arg("--discover")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("expected `aver proof --discover` to run");
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert!(
        stdout.contains("PROVED (Lean, kernel-checked)"),
        "no kernel-proved lemma in `--discover` output:\n{}",
        format_output(&run)
    );
    assert!(
        stdout.contains("decode(List.concat(x2, x3)) == List.concat(decode(x2), decode(x3))"),
        "decode_append was not the kernel-proved lemma:\n{}",
        format_output(&run)
    );
    // The proved lemma is persisted as a reviewable committed artifact.
    assert!(
        output_dir.join("DiscoveredLemmas.lean").exists(),
        "DiscoveredLemmas.lean was not written:\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Run `aver proof <path> --discover` and assert the committed
/// `DiscoveredLemmas.lean` contains `lemma_needle` (a kernel-proved lemma).
/// Skips when `lake` is unavailable.
fn assert_discover_proves(example_path: &str, prefix: &str, lemma_needle: &str) {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping discovery proof test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let output_dir = temp_output_dir(prefix);
    let run = Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(&repo_root)
        .arg("proof")
        .arg(example_path)
        .arg("--discover")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("expected `aver proof --discover` to run");
    let committed =
        std::fs::read_to_string(output_dir.join("DiscoveredLemmas.lean")).unwrap_or_default();
    assert!(
        committed.contains(lemma_needle),
        "expected `{lemma_needle}` among kernel-proved lemmas.\n--- stdout ---\n{}\n--- DiscoveredLemmas.lean ---\n{committed}",
        format_output(&run),
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Generalization guard: discovery proves the `flatten` list-homomorphism on a
/// NON-encoder program (no RLE shape anywhere) — evidence the enumeration path
/// is genuinely general, not fitted to `rle.av`.
#[test]
fn discover_proves_flatten_homomorphism_when_lake_is_available() {
    assert_discover_proves(
        "examples/data/flatten.av",
        "aver-discover-flatten",
        "(flatten (x0 ++ x1)) = ((flatten x0) ++ (flatten x1))",
    );
}

/// Generalization guard: the structural counted-repeat conjecturer fires on a
/// differently-named fn (`stars`, not `repeat`) in a non-encoder program —
/// evidence brick 1 keys on shape, not the RLE name.
#[test]
fn discover_proves_stars_repeat_succ_when_lake_is_available() {
    assert_discover_proves(
        "examples/data/stars.av",
        "aver-discover-stars",
        "stars c (n + 1) = stars c n ++ [c]",
    );
}

/// Generalization guard for the (generalized) brick 2: discovery proves the
/// monotone-nonneg accumulator invariant on `tally.av`, whose fold branches on
/// `x > acc.last` (NOT the RLE `count == 0` shape) — evidence the count-
/// invariant conjecturer keys on the field arithmetic, not the RLE step.
#[test]
fn discover_proves_tally_count_invariant_when_lake_is_available() {
    assert_discover_proves(
        "examples/data/tally.av",
        "aver-discover-tally",
        "0 <= (tallyStep acc x).seen",
    );
}

/// Generalization guard on a SHAPE-different second encoder-with-inverse
/// (`sparse.av`: sum-type tokens, branches on `x == 0`). One `--discover` run
/// must kernel-prove BOTH the UNARY counted-repeat advance `repeat0(n+1) =
/// repeat0(n) ++ [0]` (brick 1's arity generalization) AND the monotone-nonneg
/// `pending` invariant — proof that the structural conjecturers generalize
/// across encoders, not just rle.
#[test]
fn discover_generalizes_on_sparse_codec_when_lake_is_available() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping discovery proof test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let output_dir = temp_output_dir("aver-discover-sparse");
    let run = Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(&repo_root)
        .arg("proof")
        .arg("examples/data/sparse.av")
        .arg("--discover")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("expected `aver proof --discover` to run");
    let committed =
        std::fs::read_to_string(output_dir.join("DiscoveredLemmas.lean")).unwrap_or_default();
    for needle in [
        "repeat0 (n + 1) = repeat0 n ++ [0]",
        "0 <= (sparseStep acc x).pending",
    ] {
        assert!(
            committed.contains(needle),
            "expected `{needle}` among kernel-proved lemmas.\n--- stdout ---\n{}\n--- DiscoveredLemmas.lean ---\n{committed}",
            format_output(&run),
        );
    }
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Relational-brick acceptance (the locksmith's last layer): one `--discover`
/// run kernel-proves the FULL roundtrip law `decode (encode xs) = xs` on
/// `rle.av` — the auto-emitted chain (inv_append → counted_one → counted_succ →
/// count_nonneg → flush_fold_step → loop_gen → roundtrip) replaces the retired
/// hardcoded `AccumulatorRoundtrip` recognizer.
#[test]
fn discover_proves_roundtrip_on_rle_when_lake_is_available() {
    assert_discover_proves(
        "examples/data/rle.av",
        "aver-discover-rle-roundtrip",
        "decode (encode xs) = xs",
    );
}

/// DISCIPLINE GUARD (the whole point): the SAME relational emitter must fire +
/// kernel-prove the roundtrip on a SHAPE-different second encoder (`sparse.av`:
/// sum-type tokens, `pending` field, 2-way step guard) — `decodeSparse
/// (encodeSparse xs) = xs`. If this passes only on rle, the chain is the key,
/// not the locksmith; it must prove on BOTH or neither.
#[test]
fn discover_proves_roundtrip_on_sparse_when_lake_is_available() {
    assert_discover_proves(
        "examples/data/sparse.av",
        "aver-discover-sparse-roundtrip",
        "decodeSparse (encodeSparse xs) = xs",
    );
}

/// MONOIDAL flavor of the unified accumulator-generalization schema: the same
/// `--discover` path that proves codec roundtrips also kernel-proves the
/// spec-equivalence `sum xs = sumDirect xs` (sum = sumTR(·, 0), an additive
/// fold) — codec roundtrip and monoidal fold are two flavors of ONE schema.
#[test]
fn discover_proves_monoidal_spec_equivalence_on_sum_acc_when_lake_is_available() {
    assert_discover_proves(
        "examples/data/sum_acc.av",
        "aver-discover-sum-monoidal",
        "sum xs = sumDirect xs",
    );
}

/// Read the committed `DiscoveredLemmas.lean` produced by `--discover` on
/// `example_path` (empty string if none was written). Skips (returns `None`)
/// when `lake` is unavailable.
fn discover_committed(example_path: &str, prefix: &str) -> Option<(String, PathBuf)> {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping discovery proof test: `lake` not available");
        return None;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let output_dir = temp_output_dir(prefix);
    let _ = Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(&repo_root)
        .arg("proof")
        .arg(example_path)
        .arg("--discover")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("expected `aver proof --discover` to run");
    let committed =
        std::fs::read_to_string(output_dir.join("DiscoveredLemmas.lean")).unwrap_or_default();
    Some((committed, output_dir))
}

/// Generalization guard: counted-append with the count parameter FIRST
/// (`pad(n, c)`), the opposite of rle's `repeat(c, n)` — the detector finds the
/// count by role, not position.
#[test]
fn discover_proves_spaces_count_first_when_lake_is_available() {
    assert_discover_proves(
        "examples/data/spaces.av",
        "aver-discover-spaces",
        "pad (n + 1) c = pad n c ++ [c]",
    );
}

/// Generalization guard: monotone-nonneg field with a `+ 2` update (not `+ 1`)
/// — the invariant conjecturer keys on `field + nonneg-literal`, any literal.
#[test]
fn discover_proves_gauge_plus_two_invariant_when_lake_is_available() {
    assert_discover_proves(
        "examples/data/gauge.av",
        "aver-discover-gauge",
        "0 <= (bump acc x).level",
    );
}

/// Completeness guard: a MULTIPLICATIVE nonneg update (`level * 2`, not a `+ k`
/// shift) is still recognized as monotone-nonneg — `0 <= level` is closed under
/// `* 2` and stays linear in the field, so omega proves it.
#[test]
fn discover_proves_scale_multiplicative_nonneg_when_lake_is_available() {
    assert_discover_proves(
        "examples/data/scale.av",
        "aver-discover-scale",
        "0 <= (grow acc x).level",
    );
}

/// Completeness guard: a record with TWO Int fields of different invariant
/// classes — `seen` (non-negative) and `budget` (strictly decreasing) — yields
/// a kernel-proved lemma for EACH, not just the first the conjecturer finds.
#[test]
fn discover_proves_both_invariants_on_two_int_fields_when_lake_is_available() {
    let Some((committed, output_dir)) =
        discover_committed("examples/data/twofield.av", "aver-discover-twofield")
    else {
        return;
    };
    for needle in [
        "0 <= (meterStep acc x).seen",
        "acc.budget - 1 <= (meterStep acc x).budget",
        "(meterStep acc x).budget <= acc.budget - 1",
    ] {
        assert!(
            committed.contains(needle),
            "expected `{needle}` among kernel-proved lemmas.\n--- DiscoveredLemmas.lean ---\n{committed}",
        );
    }
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Generalization guard: the list-homomorphism discovery works over a RECORD
/// element type (`List<Token>`), not just String. The homomorphism theorem
/// names `expandAll` three times (lhs once, rhs twice).
#[test]
fn discover_proves_words_homomorphism_when_lake_is_available() {
    let Some((committed, output_dir)) =
        discover_committed("examples/data/words.av", "aver-discover-words")
    else {
        return;
    };
    assert!(
        committed.matches("expandAll").count() >= 3,
        "expected an `expandAll(a ++ b) = expandAll a ++ expandAll b` homomorphism.\n--- DiscoveredLemmas.lean ---\n{committed}",
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// SOUNDNESS + GENERALIZATION guard: on `drain.av` the accumulator field can
/// DECREASE (`Counter(n = acc.n - 1)`), so `0 <= (tick acc x).n` is FALSE. The
/// engine must NEVER kernel-prove that (proved-or-dropped). But the field DOES
/// move by a bounded delta each step (`+1`/`-1`), so the generalized bounded-
/// step conjecturer must discover and prove the TRUE two-sided bound — proof the
/// engine generalizes past monotone-nonneg without becoming unsound: it picks
/// the right invariant for a decreasing accumulator, not the false one.
#[test]
fn discover_bounds_decreasing_accumulator_on_drain_when_lake_is_available() {
    let Some((committed, output_dir)) =
        discover_committed("examples/data/drain.av", "aver-discover-drain")
    else {
        return;
    };
    // Soundness: the false nonneg invariant is never proved.
    assert!(
        !committed.contains("0 <= (tick acc x).n"),
        "UNSOUND: the false count-invariant `0 <= (tick acc x).n` was kernel-proved.\n--- DiscoveredLemmas.lean ---\n{committed}",
    );
    // Generalization: the true bounded step IS proved (both sides).
    for needle in ["acc.n - 1 <= (tick acc x).n", "(tick acc x).n <= acc.n + 1"] {
        assert!(
            committed.contains(needle),
            "expected the bounded-step bound `{needle}` among kernel-proved lemmas.\n--- DiscoveredLemmas.lean ---\n{committed}",
        );
    }
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// THE FEEDBACK LOOP (`ProofStrategy::SimpOverLemmas`) — the coverage win
/// condition, end to end. The law states `plus(length(ys), length(xs)) =
/// length(append(xs, ys))` — closing it needs the UNSTATED length-into-plus
/// homomorphism (`length (a ++ b) = plus (length a) (length b)`), which the
/// induction ladder alone cannot conjure (the `plus` recursion is stuck on a
/// symbolic first argument, and `omega` sees only opaque atoms). The loop:
///
/// 1. baseline `aver proof --check` — builds, but the universal stays `sorry`
///    (`universal:false`): the honest no-discovery floor;
/// 2. `aver proof --discover` conjectures AND kernel-proves the homomorphism,
///    committing it to `DiscoveredLemmas.lean`;
/// 3. the SAME `aver proof --check` in the SAME output dir now re-pins the
///    law to `SimpOverLemmas`, embeds + re-proves the lemma, and closes the
///    universal for real (`universal:true`, zero sorries).
///
/// This is discovery moving COVERAGE, not just reach.
#[test]
fn discovered_lemmas_close_length_homomorphism_law_when_lake_is_available() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping feedback-loop test: `lake` not available");
        return;
    }
    let source = "module LenHomo\n    intent =\n        \"plus(length ys, length xs) = length(xs ++ ys) — needs the unstated length homomorphism\"\n    effects []\n\n\
         type Nat\n    Z\n    S(Nat)\n\n\
         fn length(xs: List<Int>) -> Nat\n    match xs\n        [] -> Nat.Z\n        [y, ..ys] -> Nat.S(length(ys))\n\n\
         fn plus(x: Nat, y: Nat) -> Nat\n    match x\n        Nat.Z -> y\n        Nat.S(z) -> Nat.S(plus(z, y))\n\n\
         fn append(xs: List<Int>, ys: List<Int>) -> List<Int>\n    List.concat(xs, ys)\n\n\
         verify length law lenAppendSwap\n    given xs: List<Int> = [[], [1], [1, 2]]\n    given ys: List<Int> = [[], [3]]\n    plus(length(ys), length(xs)) => length(append(xs, ys))\n";
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-feedback-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(src.join("m.av"), source).expect("write m.av");
    let out = temp_output_dir("aver-feedback-out");

    let check = |label: &str| -> (bool, String) {
        let run = Command::new(aver_bin)
            .arg("proof")
            .arg(src.join("m.av"))
            .arg("--backend")
            .arg("lean")
            .arg("-o")
            .arg(&out)
            .arg("--check")
            .arg("--check-json")
            .output()
            .unwrap_or_else(|e| panic!("{label}: aver proof failed to run: {e}"));
        let json_line = run
            .stdout
            .split(|&b| b == b'\n')
            .rev()
            .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
            .unwrap_or_else(|| panic!("{label}: no JSON line:\n{}", format_output(&run)))
            .to_string();
        let summary: serde_json::Value = serde_json::from_str(&json_line)
            .unwrap_or_else(|e| panic!("{label}: bad JSON ({e}):\n{json_line}"));
        (
            summary["universal"].as_bool().unwrap_or(false),
            format_output(&run),
        )
    };

    // 1. The honest floor: without discovery the universal must NOT close —
    //    if it ever starts closing, the fixture stopped exercising the loop
    //    (tighten it) rather than the loop having regressed.
    let (universal_before, output_before) = check("baseline");
    assert!(
        !universal_before,
        "fixture closes WITHOUT discovery — it no longer exercises the feedback loop:\n{output_before}"
    );

    // 2. Discover + commit into the same output dir.
    let discover = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("m.av"))
        .arg("--discover")
        .arg("-o")
        .arg(&out)
        .output()
        .expect("expected `aver proof --discover` to run");
    let committed = std::fs::read_to_string(out.join("DiscoveredLemmas.lean")).unwrap_or_default();
    assert!(
        committed.contains("length (") && committed.contains("plus (length"),
        "discovery did not commit the length homomorphism:\n--- DiscoveredLemmas.lean ---\n{committed}\n--- discover output ---\n{}",
        format_output(&discover)
    );

    // 3. The win: the SAME check now closes the universal via the committed
    //    lemma (re-proved in the same build — no trust shortcut).
    let (universal_after, output_after) = check("with-discovery");
    assert!(
        universal_after,
        "committed discovered lemmas did not close the law (feedback loop broken):\n{output_after}"
    );

    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

/// THE FEEDBACK LOOP, część A — an already-proved EARLIER user `verify … law`
/// feeds a later law's proof, with NO `--discover` step at all. The file holds
/// two laws over `length`: `lengthHomo` (the homomorphism `length (append xs
/// ys) = plus (length xs) (length ys)`, provable on its own by induction) and,
/// AFTER it, `lengthAppendSwap` (`length (append xs ys) = plus (length ys)
/// (length xs)`), which needs the homomorphism plus commutativity. The later
/// law must close ONLY because the earlier one is in scope:
///
/// 1. `lengthAppendSwap` ALONE (its own file) must NOT close — it has no
///    helper, so the universal stays `sorry`;
/// 2. the same law AFTER `lengthHomo` in one file must close `universal:true`
///    — the backend references the earlier theorem in the later proof's `simp`
///    set (verified by inspecting the emitted Lean), no discovery artifact
///    involved.
///
/// This is the user-written-decomposition half of the loop: a hard law closes
/// because an earlier proved law is available as a lemma.
#[test]
fn earlier_user_law_feeds_later_law_proof_when_lake_is_available() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping część A test: `lake` not available");
        return;
    }
    let nat_helpers = "type Nat\n    Z\n    S(Nat)\n\n\
         fn length(xs: List<Int>) -> Nat\n    match xs\n        [] -> Nat.Z\n        [y, ..ys] -> Nat.S(length(ys))\n\n\
         fn plus(x: Nat, y: Nat) -> Nat\n    match x\n        Nat.Z -> y\n        Nat.S(z) -> Nat.S(plus(z, y))\n\n\
         fn append(xs: List<Int>, ys: List<Int>) -> List<Int>\n    match xs\n        [] -> ys\n        [z, ..zs] -> List.concat([z], append(zs, ys))\n\n";
    let swap_law = "verify length law lengthAppendSwap\n    given xs: List<Int> = [[], [1], [1, 2]]\n    given ys: List<Int> = [[], [3]]\n    length(append(xs, ys)) => plus(length(ys), length(xs))\n";
    let homo_law = "verify length law lengthHomo\n    given xs: List<Int> = [[], [1], [1, 2]]\n    given ys: List<Int> = [[], [3]]\n    length(append(xs, ys)) => plus(length(xs), length(ys))\n\n";

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let universal_of = |source: &str, prefix: &str| -> (bool, String, String) {
        let src = temp_output_dir(&format!("{prefix}-src"));
        std::fs::create_dir_all(&src).expect("create src dir");
        std::fs::write(src.join("m.av"), source).expect("write m.av");
        let out = temp_output_dir(&format!("{prefix}-out"));
        let run = Command::new(aver_bin)
            .arg("proof")
            .arg(src.join("m.av"))
            .arg("--backend")
            .arg("lean")
            .arg("-o")
            .arg(&out)
            .arg("--check")
            .arg("--check-json")
            .output()
            .expect("aver proof ran");
        let json = run
            .stdout
            .split(|&b| b == b'\n')
            .rev()
            .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
            .unwrap_or_else(|| panic!("{prefix}: no JSON line:\n{}", format_output(&run)))
            .to_string();
        let summary: serde_json::Value =
            serde_json::from_str(&json).unwrap_or_else(|e| panic!("{prefix}: bad JSON ({e})"));
        let emitted = std::fs::read_to_string(out.join("M.lean")).unwrap_or_default();
        let _ = std::fs::remove_dir_all(&src);
        let _ = std::fs::remove_dir_all(&out);
        (
            summary["universal"].as_bool().unwrap_or(false),
            format_output(&run),
            emitted,
        )
    };

    // 1. swap law alone — no helper in scope → must stay open.
    let solo =
        format!("module M\n    intent = \"solo\"\n    effects []\n\n{nat_helpers}{swap_law}");
    let (solo_universal, solo_out, _) = universal_of(&solo, "aver-partA-solo");
    assert!(
        !solo_universal,
        "swap law closed WITHOUT a helper — the fixture no longer exercises część A:\n{solo_out}"
    );

    // 2. helper FIRST, then swap law — must close via the earlier theorem.
    let paired = format!(
        "module M\n    intent = \"paired\"\n    effects []\n\n{nat_helpers}{homo_law}{swap_law}"
    );
    let (paired_universal, paired_out, emitted) = universal_of(&paired, "aver-partA-paired");
    assert!(
        paired_universal,
        "swap law did not close with the earlier homomorphism law in scope (część A broken):\n{paired_out}"
    );
    // The later proof must actually reference the earlier law's theorem — proof
    // that the close came from the sibling lemma, not some incidental tactic.
    assert!(
        emitted.contains("length_law_lengthAppendSwap")
            && emitted.contains("simp only [length_law_lengthHomo"),
        "the swap law's proof does not simp over the earlier homomorphism theorem:\n{emitted}"
    );
}

/// The `proof-corpus/decomposed/` artifacts: OPEN bare-`tip/` TIP tasks that an
/// LLM closed by writing helper `verify ... law` blocks (część A / the loop).
/// Each must stay `universal:true` on its own merits — every law in the file,
/// helpers included, kernel-clean (no `sorry`, axiom set ⊆ {propext,
/// Classical.choice, Quot.sound}). This guards the feedback loop AND the
/// auto-prover's leaf reach from regressing: if a future change stops closing
/// any helper, its file drops to universal:false and this test fails loudly.
/// Distinct from the corpus coverage runner — these are NOT counted in the
/// baseline (run.sh excludes decomposed/); they are the loop-reach record.
#[test]
fn decomposed_tip_tasks_stay_universal_when_lake_is_available() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping decomposed-corpus test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let tasks = [
        "proof-corpus/decomposed/isaplanner/prop_03.av",
        "proof-corpus/decomposed/isaplanner/prop_04.av",
        "proof-corpus/decomposed/isaplanner/prop_20.av",
        "proof-corpus/decomposed/isaplanner/prop_28.av",
        "proof-corpus/decomposed/isaplanner/prop_29.av",
        "proof-corpus/decomposed/isaplanner/prop_30.av",
        "proof-corpus/decomposed/isaplanner/prop_38.av",
        "proof-corpus/decomposed/isaplanner/prop_75.av",
        "proof-corpus/decomposed/prod/prop_03.av",
        "proof-corpus/decomposed/prod/prop_25.av",
        // decomposition-reach chunk #1 (reverse/accumulator family).
        "proof-corpus/decomposed/handwritten/qrev_rev.av",
        "proof-corpus/decomposed/isaplanner/prop_19.av",
        "proof-corpus/decomposed/prod/prop_27.av",
        "proof-corpus/decomposed/prod/prop_29.av",
        "proof-corpus/decomposed/prod/prop_30.av",
        "proof-corpus/decomposed/prod/prop_31.av",
        // decomposition-reach chunk #2 (deterministic spread over fresh open pool).
        "proof-corpus/decomposed/isaplanner/prop_55.av",
        "proof-corpus/decomposed/prod/prop_02.av",
        "proof-corpus/decomposed/prod/prop_19.av",
        "proof-corpus/decomposed/prod/prop_34.av",
    ];
    for task in tasks {
        let out = temp_output_dir(&format!(
            "aver-decomposed-{}",
            task.rsplit('/').next().unwrap().trim_end_matches(".av")
        ));
        let run = Command::new(aver_bin)
            .current_dir(&repo_root)
            .arg("proof")
            .arg(task)
            .arg("--backend")
            .arg("lean")
            .arg("-o")
            .arg(&out)
            .arg("--check")
            .arg("--check-json")
            .output()
            .unwrap_or_else(|e| panic!("{task}: aver proof failed to run: {e}"));
        let json = run
            .stdout
            .split(|&b| b == b'\n')
            .rev()
            .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
            .unwrap_or_else(|| panic!("{task}: no JSON line:\n{}", format_output(&run)));
        let summary: serde_json::Value =
            serde_json::from_str(json).unwrap_or_else(|e| panic!("{task}: bad JSON ({e})"));
        assert_eq!(
            summary["universal"].as_bool(),
            Some(true),
            "{task}: decomposed artifact must stay universal:true (every law, helpers \
             included, kernel-clean). A drop means część A or an auto-prover leaf \
             regressed.\n{}",
            format_output(&run)
        );
        let _ = std::fs::remove_dir_all(&out);
    }
}

/// część C — ARM injection of Forward sibling laws. Some laws need a helper
/// applied INSIDE the induction cons-arm, not just at the top-level fast path:
/// `count n xs = count n (rev xs)` only closes if the count-homomorphism
/// rewrites `count n (rev t ++ [h])` within the arm. część A (fast-path only)
/// leaves these open; część C adds a second ladder whose arms carry the
/// Forward siblings (Reversed stay fast-path-only — loop safety). Two checks:
///
/// 1. count-rev closes when a count-homomorphism helper precedes it (the
///    homo is used in-arm; it also INTRODUCES `plus`, exercising the
///    subject-sharing cone relaxation — `plus` is outside count-rev's cone but
///    the helper shares the subject `count`);
/// 2. length-rev closes via a CHAIN — length-homo, then a length-rev-invariant
///    whose OWN cons-arm needs the length-homo sibling, then the target.
///
/// Both are union-OPEN frontier TIP shapes (isaplanner/prop_52, prod/prop_06)
/// that the manual experiment (#449) could NOT close before część C.
#[test]
fn part_c_arm_injection_closes_in_arm_helpers_when_lake_is_available() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping część C test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let nat = "type Nat\n    Z\n    S(Nat)\n\n";
    let plus = "fn plus(x: Nat, y: Nat) -> Nat\n    match x\n        Nat.Z -> y\n        Nat.S(z) -> Nat.S(plus(z, y))\n\n";

    let count_rev = format!(
        "module M\n    intent = \"count-rev via in-arm homo\"\n    effects []\n\n{nat}\
         fn eqNat(x: Nat, y: Nat) -> Bool\n    match x\n        Nat.Z -> match y\n            Nat.Z -> true\n            Nat.S(z) -> false\n        Nat.S(x2) -> match y\n            Nat.Z -> false\n            Nat.S(y2) -> eqNat(x2, y2)\n\n\
         fn count(x: Nat, y: List<Nat>) -> Nat\n    match y\n        [] -> Nat.Z\n        [z, ..ys] -> match eqNat(x, z)\n            true -> Nat.S(count(x, ys))\n            false -> count(x, ys)\n\n{plus}\
         fn rev(x: List<Nat>) -> List<Nat>\n    match x\n        [] -> []\n        [y, ..xs] -> List.concat(rev(xs), [y])\n\n\
         verify count law countHomo\n    given n: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    given xs: List<Nat> = [[Nat.Z], [Nat.S(Nat.Z), Nat.Z]]\n    given ys: List<Nat> = [[Nat.Z], [Nat.S(Nat.Z)]]\n    count(n, List.concat(xs, ys)) => plus(count(n, xs), count(n, ys))\n\n\
         verify count law countRev\n    given n: Nat = [Nat.Z, Nat.S(Nat.Z), Nat.S(Nat.S(Nat.Z))]\n    given xs: List<Nat> = [[], [Nat.Z], [Nat.Z, Nat.S(Nat.Z), Nat.Z]]\n    count(n, xs) => count(n, rev(xs))\n"
    );

    let length_rev = format!(
        "module M\n    intent = \"length-rev via chain\"\n    effects []\n\n{nat}\
         fn length(xs: List<Int>) -> Nat\n    match xs\n        [] -> Nat.Z\n        [y, ..ys] -> Nat.S(length(ys))\n\n{plus}\
         fn append(x: List<Int>, y: List<Int>) -> List<Int>\n    match x\n        [] -> y\n        [z, ..xs] -> List.concat([z], append(xs, y))\n\n\
         fn rev(x: List<Int>) -> List<Int>\n    match x\n        [] -> []\n        [y, ..ys] -> append(rev(ys), [y])\n\n\
         verify length law lengthHomo\n    given x: List<Int> = [[1], [2, 3]]\n    given y: List<Int> = [[4], [5, 6]]\n    length(append(x, y)) => plus(length(x), length(y))\n\n\
         verify length law lengthRevInv\n    given x: List<Int> = [[], [1], [1, 2, 3]]\n    length(rev(x)) => length(x)\n\n\
         verify length law revAppendLength\n    given x: List<Int> = [[], [1], [1, 2, 3]]\n    given y: List<Int> = [[], [2], [4, 5]]\n    length(rev(append(x, y))) => plus(length(x), length(y))\n"
    );

    let check_universal = |source: &str, label: &str| {
        let src = temp_output_dir(&format!("aver-partc-{label}-src"));
        std::fs::create_dir_all(&src).expect("src dir");
        std::fs::write(src.join("m.av"), source).expect("write");
        let out = temp_output_dir(&format!("aver-partc-{label}-out"));
        let run = Command::new(aver_bin)
            .arg("proof")
            .arg(src.join("m.av"))
            .arg("--backend")
            .arg("lean")
            .arg("-o")
            .arg(&out)
            .arg("--check")
            .arg("--check-json")
            .output()
            .expect("aver proof ran");
        let json = run
            .stdout
            .split(|&b| b == b'\n')
            .rev()
            .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
            .unwrap_or_else(|| panic!("{label}: no JSON:\n{}", format_output(&run)));
        let summary: serde_json::Value =
            serde_json::from_str(json).unwrap_or_else(|e| panic!("{label}: bad JSON ({e})"));
        assert_eq!(
            summary["universal"].as_bool(),
            Some(true),
            "{label}: część C must close this in-arm-helper law universal:true\n{}",
            format_output(&run)
        );
        let _ = std::fs::remove_dir_all(&src);
        let _ = std::fs::remove_dir_all(&out);
    };
    check_universal(&count_rev, "count-rev");
    check_universal(&length_rev, "length-rev");
}

/// Leaf-reach: the Lean backend auto-proves a GENERALIZING-induction law —
/// `take n xs ++ drop n xs = xs`, where `take`/`drop` recurse synchronously on
/// the Nat `n` AND the list `xs`. Single-variable `induction xs` leaves the
/// cons IH at the wrong `n`; the backend now emits `induction xs generalizing
/// n with … cases n` so the IH (`∀ n, P n tail`) applies at the predecessor.
/// No helper law, no decomposition — the bare auto-prover closes it. (TIP
/// isaplanner prop_01, a union-OPEN frontier shape nothing closed before.)
#[test]
fn lean_proves_generalizing_induction_take_drop_when_lake_is_available() {
    assert_proof_builds(
        "proof-corpus/tip/isaplanner/prop_01.av",
        "aver-gen-takedrop",
    );
}

/// Leaf-reach: a user fn named `max` (colliding with Lean 4's Max-typeclass
/// `max`) is emitted as `max'`, so the proof can reference the user's
/// recursion instead of the typeclass form. Before the escape the generated
/// Lean failed to build (ambiguous/typeclass `max`); now it builds clean.
/// max-associativity USED to fall to one honest `sorry` (a 3-var induction the
/// bare prover left open); the both-args-peeling generalizing emit now closes
/// it as a GENUINE universal — `induction a generalizing b c with … cases b
/// <;> cases c <;> simp_all`, `#print axioms = [propext]`. Budget is 0:
/// fully proven. TIP isaplanner prop_22.
#[test]
fn lean_escapes_user_max_min_collision_when_lake_is_available() {
    assert_proof_builds_with_sorry_budget(
        "proof-corpus/tip/isaplanner/prop_22.av",
        "aver-max-escape",
        0,
    );
}

/// Leaf-reach: the Lean backend auto-proves an ACCUMULATOR-generalizing law —
/// `qrev(xs, acc) = rev(xs) ++ acc`, where `qrev` recurses on `xs` while
/// THREADING `acc` (fed `List.concat([h], acc)`). The IH must be `∀ acc,
/// P xs acc`, so the backend emits `induction xs generalizing acc` (no `cases`
/// — `acc` is not a Peano scrutinee, distinct from the take/drop Nat case).
/// No helper, bare — the lemma that previously SORRIED. (The qrev↔rev
/// equivalence `fastRev = rev` then closes by decomposition over it.)
#[test]
fn lean_proves_accumulator_generalizing_qrev_when_lake_is_available() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping accumulator-gen test: `lake` not available");
        return;
    }
    let source = "module QrevAccGen\n    intent = \"qrev acc-generalization\"\n    effects []\n\n\
        fn rev(xs: List<Int>) -> List<Int>\n    match xs\n        [] -> []\n        [h, ..t] -> List.concat(rev(t), [h])\n\n\
        fn qrev(xs: List<Int>, acc: List<Int>) -> List<Int>\n    match xs\n        [] -> acc\n        [h, ..t] -> qrev(t, List.concat([h], acc))\n\n\
        verify qrev law qrevRevAppend\n    given xs: List<Int> = [[1], [1, 2, 3]]\n    given acc: List<Int> = [[9], [8, 7]]\n    qrev(xs, acc) => List.concat(rev(xs), acc)\n";
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-accgen-src");
    std::fs::create_dir_all(&src).expect("src dir");
    std::fs::write(src.join("m.av"), source).expect("write");
    let out = temp_output_dir("aver-accgen-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("m.av"))
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("aver proof ran");
    let json = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON:\n{}", format_output(&run)));
    let summary: serde_json::Value = serde_json::from_str(json).expect("json");
    assert_eq!(
        summary["universal"].as_bool(),
        Some(true),
        "accumulator-generalizing must close qrev(xs,acc)=rev(xs)++acc bare\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

/// LUKA 2: generalizing induction and sibling-lemma injection must COMBINE. The
/// `dropRevLen` law's verified fn (`drop`) threads a Peano param (`n`) while
/// recursing on a list, so it needs `induction xs generalizing n`; AND it needs
/// the forward-homomorphism siblings (`lenAppend`, `lenRev`) rewriting inside
/// the induction arms. Before the fix these were mutually exclusive (the
/// `gen_given` branch was gated on `fast_simp.is_empty()`), so a law needing
/// both fell to plain `induction` and left a `sorry`. The arm simp set carries
/// Forward siblings only (loop-excluded via `simp_entries`) — no `←` unfold
/// rules — so no `maxHeartbeats` simp loop.
#[test]
fn lean_proves_generalizing_induction_with_siblings_when_lake_is_available() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping gen+siblings test: `lake` not available");
        return;
    }
    let source = r#"module GenSiblings
    intent = "generalizing induction + forward-sibling arm injection combined"
    effects []

type Nat
    Z
    S(Nat)

fn len(xs: List<Int>) -> Nat
    match xs
        [] -> Nat.Z
        [y, ..ys] -> Nat.S(len(ys))

fn drop(n: Nat, xs: List<Int>) -> List<Int>
    match n
        Nat.Z -> xs
        Nat.S(z) -> match xs
            [] -> []
            [x2, ..x3] -> drop(z, x3)

fn minus(x: Nat, y: Nat) -> Nat
    match x
        Nat.Z -> Nat.Z
        Nat.S(z) -> match y
            Nat.Z -> x
            Nat.S(x2) -> minus(z, x2)

fn plus(x: Nat, y: Nat) -> Nat
    match x
        Nat.Z -> y
        Nat.S(z) -> Nat.S(plus(z, y))

fn rev(xs: List<Int>) -> List<Int>
    match xs
        [] -> []
        [y, ..ys] -> List.concat(rev(ys), [y])

verify len law lenAppend
    given a: List<Int> = [[1], [1, 2, 3]]
    given b: List<Int> = [[7], [8, 9]]
    len(List.concat(a, b)) => plus(len(a), len(b))

verify len law lenRev
    given xs: List<Int> = [[], [1], [1, 2, 3]]
    len(rev(xs)) => len(xs)

verify drop law dropRevLen
    given n: Nat = [Nat.Z, Nat.S(Nat.Z), Nat.S(Nat.S(Nat.Z))]
    given xs: List<Int> = [[], [1], [1, 2, 3]]
    given ys: List<Int> = [[2], [3, 4]]
    len(List.concat(rev(drop(n, xs)), ys)) => plus(minus(len(xs), n), len(ys))
"#;
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-gensib-src");
    std::fs::create_dir_all(&src).expect("src dir");
    std::fs::write(src.join("m.av"), source).expect("write");
    let out = temp_output_dir("aver-gensib-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("m.av"))
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("aver proof ran");
    let json = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON:\n{}", format_output(&run)));
    let summary: serde_json::Value = serde_json::from_str(json).expect("json");
    assert_eq!(
        summary["universal"].as_bool(),
        Some(true),
        "generalizing induction + forward-sibling arm injection must combine to \
         close dropRevLen (the gen-vs-sibling mutual-exclusion gap)\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

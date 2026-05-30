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

    let proof = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg(example_path)
        .arg("--verify-mode")
        .arg("auto")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("expected `aver proof` to run");
    assert!(
        proof.status.success(),
        "`aver proof` failed:\n{}",
        format_output(&proof)
    );

    // Count `sorry` tokens in the entry Lean file (skip `AverCommon.
    // lean`, which carries prelude lemmas that legitimately use
    // `sorry` for runtime-only obligations). The match is whole-word
    // to avoid counting identifiers like `sorry_substring`.
    let entry_lean = std::fs::read_dir(&output_dir)
        .expect("read output_dir")
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .find(|p| {
            p.extension().is_some_and(|x| x == "lean")
                && p.file_name()
                    .and_then(|n| n.to_str())
                    .is_some_and(|n| n != "AverCommon.lean" && n != "lakefile.lean")
        });
    if let Some(path) = entry_lean {
        let text = std::fs::read_to_string(&path).expect("read entry .lean");
        let actual = text
            .lines()
            .filter(|line| {
                line.split_whitespace().any(|tok| tok == "sorry")
                    && !line.trim_start().starts_with("--")
            })
            .count();
        assert_eq!(
            actual, expected_sorries,
            "{}: sorry count drift (expected {}, got {}). \
             If the count dropped, lower the budget. If it grew, a new shape regressed — \
             investigate before raising the budget.",
            example_path, expected_sorries, actual
        );
    }

    let build = Command::new("lake")
        .current_dir(&output_dir)
        .arg("build")
        .output()
        .expect("expected `lake build` to run");
    assert!(
        build.status.success(),
        "`lake build` failed:\n{}",
        format_output(&build)
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
/// via [`assert_dafny_verifies_with_error_budget`].
fn assert_dafny_verifies(example_path: &str, prefix: &str) {
    assert_dafny_verifies_with_error_budget(example_path, prefix, 0);
}

/// `assert_dafny_verifies`, but tolerate `expected_errors` Dafny
/// verification errors. Mirror of the Lean-side
/// `assert_proof_builds_with_sorry_budget`: a drop below the budget
/// fails (cue to tighten), a climb above it fails (regression — a
/// new shape lost its strategy). Parses Dafny's "Dafny program
/// verifier finished with X verified, Y errors" tail line.
fn assert_dafny_verifies_with_error_budget(
    example_path: &str,
    prefix: &str,
    expected_errors: usize,
) {
    if Command::new("dafny").arg("--version").output().is_err() {
        eprintln!("skipping dafny verify smoke test: `dafny` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let output_dir = temp_output_dir(prefix);
    let aver_bin = env!("CARGO_BIN_EXE_aver");

    let proof = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg(example_path)
        .arg("--backend")
        .arg("dafny")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("expected `aver proof --backend dafny` to run");
    assert!(
        proof.status.success(),
        "`aver proof --backend dafny` failed:\n{}",
        format_output(&proof)
    );

    let dfy = std::fs::read_dir(&output_dir)
        .expect("read output_dir")
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .find(|p| {
            p.extension().is_some_and(|x| x == "dfy")
                && p.file_name()
                    .and_then(|n| n.to_str())
                    .is_some_and(|n| n != "common.dfy")
        })
        .expect("expected a non-`common.dfy` Dafny file in output");
    let verify = Command::new("dafny")
        .current_dir(&output_dir)
        .arg("verify")
        .arg(&dfy)
        .output()
        .expect("expected `dafny verify` to run");

    if expected_errors == 0 {
        assert!(
            verify.status.success(),
            "`dafny verify` failed:\n{}",
            format_output(&verify)
        );
    } else {
        let stdout = String::from_utf8_lossy(&verify.stdout);
        let actual = parse_dafny_error_count(&stdout).unwrap_or_else(|| {
            panic!(
                "could not parse Dafny verifier summary from output:\n{}",
                format_output(&verify)
            )
        });
        assert_eq!(
            actual,
            expected_errors,
            "{}: dafny error count drift (expected {}, got {}). \
             If the count dropped, lower the budget. If it grew, a new shape regressed — \
             investigate before raising the budget.\n{}",
            example_path,
            expected_errors,
            actual,
            format_output(&verify)
        );
    }

    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Extract the trailing error count from a Dafny verifier run.
///
/// Dafny prints `Dafny program verifier finished with N verified, M
/// error(s)` (singular `error` when M == 1). Returns `M` or `None`
/// if the summary line is missing.
fn parse_dafny_error_count(stdout: &str) -> Option<usize> {
    let line = stdout
        .lines()
        .rev()
        .find(|l| l.contains("Dafny program verifier finished with"))?;
    let after = line.split(", ").nth(1)?;
    let n: usize = after.split_whitespace().next()?.parse().ok()?;
    Some(n)
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
    // Two sampled-domain laws (encodeString / decodeString roundtrip
    // shapes) hit the universal-not-auto-proved fallback. Same gate
    // semantics as `json` below — drop the budget when a real
    // strategy lands.
    assert_proof_builds_with_sorry_budget("examples/data/rle.av", "aver-proof-rle", 2);
}

#[test]
fn proof_dafny_verifies_rle_when_dafny_is_available() {
    // Three postcondition gaps on the encode/decode roundtrip shape
    // (one universal lemma, one sample assertion, one
    // `decodeString` universal). Z3 can't auto-discharge them
    // without a richer list-induction tactic the lowerer doesn't
    // emit yet. Tracked in issue #114.
    assert_dafny_verifies_with_error_budget("examples/data/rle.av", "aver-dafny-rle", 3);
}

#[test]
fn proof_export_builds_quicksort_when_lake_is_available() {
    // Three sampled-domain laws (`sort.resultOrdered` /
    // `sort.lengthPreserved` / `sort.idempotent`) emit the sorry
    // fallback — universal closure needs a real induction strategy
    // on the pivot-partition shape. Per-sample `_sample_N` theorems
    // still verify mechanically. (Budget grew from 2 → 3 when
    // `sort.idempotent` landed in #220.)
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
    // one postcondition each). Tracked in issue #114 / #76.
    assert_dafny_verifies_with_error_budget(
        "examples/data/quicksort.av",
        "aver-dafny-quicksort",
        8,
    );
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
    assert_proof_builds_with_sorry_budget("examples/data/json.av", "aver-proof-json", 9);
}

#[test]
fn proof_dafny_verifies_json_when_dafny_is_available() {
    // Structural shape limits: deeply-nested ADT roundtrip
    // postconditions blow past what Dafny can auto-discharge. The
    // large budget exists so a regression *upward* is still caught;
    // closing this cleanly is probably out of scope for a single
    // fix per issue #114, and would need a different proof
    // strategy entirely.
    assert_dafny_verifies_with_error_budget("examples/data/json.av", "aver-dafny-json", 89);
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
    assert_proof_builds(
        "examples/data/red_black_tree.av",
        "aver-proof-red-black-tree",
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

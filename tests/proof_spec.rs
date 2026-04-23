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

#[test]
fn proof_export_builds_law_auto_when_lake_is_available() {
    assert_proof_builds("examples/formal/law_auto.av", "aver-proof-smoke");
}

#[test]
fn proof_export_builds_fibonacci_when_lake_is_available() {
    assert_proof_builds("examples/data/fibonacci.av", "aver-proof-fibonacci");
}

#[test]
fn proof_export_builds_rle_when_lake_is_available() {
    assert_proof_builds("examples/data/rle.av", "aver-proof-rle");
}

#[test]
fn proof_export_builds_quicksort_when_lake_is_available() {
    assert_proof_builds("examples/data/quicksort.av", "aver-proof-quicksort");
}

#[test]
fn proof_export_builds_json_when_lake_is_available() {
    assert_proof_builds("examples/data/json.av", "aver-proof-json");
}

#[test]
fn proof_export_builds_grok_s_language_when_lake_is_available() {
    assert_proof_builds("examples/core/grok_s_language.av", "aver-proof-grok");
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
         \x20   pickOne() => pickOneSpec(BranchPath.root(), rnd)\n",
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
         fn pickPair() -> (Int, Int)\n\
         \x20   ? \"two parallel draws\"\n\
         \x20   ! [Random.int]\n\
         \x20   (Random.int(1, 6), Random.int(1, 6))!\n\
         \n\
         fn pickPairSpec(path: BranchPath, rnd: Fn(BranchPath, Int, Int, Int) -> Int) -> (Int, Int)\n\
         \x20   ? \"two draws, each at its own branch\"\n\
         \x20   (rnd(BranchPath.child(path, 0), 0, 1, 6), rnd(BranchPath.child(path, 1), 0, 1, 6))\n\
         \n\
         verify pickPair law consistent\n\
         \x20   given rnd: Random.int = [stubByBranch]\n\
         \x20   pickPair() => pickPairSpec(BranchPath.root(), rnd)\n",
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

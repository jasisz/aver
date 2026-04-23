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

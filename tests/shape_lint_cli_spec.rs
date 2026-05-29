//! CLI smoke tests for `aver shape --lint`. Confirms exit codes:
//! 0 = no aver.toml expectations / no match / verdict matches.
//! 1 = at least one file under a glob has nearest-layer disagreeing
//!     with the declared layer.

use std::fs;
use std::process::Command;

fn aver_bin() -> &'static str {
    env!("CARGO_BIN_EXE_aver")
}

fn temp_dir(tag: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir().join(format!("aver-shape-lint-{tag}-{}", std::process::id()));
    if dir.exists() {
        fs::remove_dir_all(&dir).ok();
    }
    fs::create_dir_all(&dir).expect("create temp dir");
    dir
}

fn write(path: &std::path::Path, content: &str) {
    fs::write(path, content).unwrap_or_else(|e| panic!("write {}: {e}", path.display()));
}

fn copy(src: &std::path::Path, dst: &std::path::Path) {
    fs::copy(src, dst)
        .unwrap_or_else(|e| panic!("copy {} → {}: {e}", src.display(), dst.display()));
}

#[test]
fn lint_exits_1_on_layer_mismatch() {
    let dir = temp_dir("mismatch");
    let repo_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    copy(
        &repo_root.join("examples/services/redis.av"),
        &dir.join("redis.av"),
    );
    // Force a mismatch: redis.av's histogram lands on Infra (built-in v0),
    // we declare Domain → lint must exit 1.
    write(
        &dir.join("aver.toml"),
        r#"[[shape.expected]]
glob = "**/redis.av"
layer = "Domain"
"#,
    );

    let out = Command::new(aver_bin())
        .current_dir(&dir)
        .args(["shape", "redis.av", "--lint", "--summary"])
        .output()
        .expect("run aver shape --lint");
    assert_eq!(
        out.status.code(),
        Some(1),
        "expected exit 1 on mismatch, got {:?}.\nstdout: {}\nstderr: {}",
        out.status.code(),
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
    );
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("declared as 'Domain'") && stderr.contains("nearest-layer"),
        "expected mismatch message naming declared + nearest layer, got stderr:\n{}",
        stderr
    );

    fs::remove_dir_all(&dir).ok();
}

#[test]
fn lint_exits_0_when_aver_toml_has_no_expectations() {
    let dir = temp_dir("no-expectations");
    let repo_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    copy(
        &repo_root.join("examples/services/redis.av"),
        &dir.join("redis.av"),
    );
    // aver.toml exists but no [[shape.expected]] → --lint is a no-op.
    write(&dir.join("aver.toml"), "");

    let out = Command::new(aver_bin())
        .current_dir(&dir)
        .args(["shape", "redis.av", "--lint", "--summary"])
        .output()
        .expect("run aver shape --lint");
    assert_eq!(
        out.status.code(),
        Some(0),
        "expected exit 0 when [[shape.expected]] is empty, got {:?}.\nstderr: {}",
        out.status.code(),
        String::from_utf8_lossy(&out.stderr),
    );

    fs::remove_dir_all(&dir).ok();
}

#[test]
fn lint_exits_0_when_glob_does_not_match() {
    let dir = temp_dir("no-glob-match");
    let repo_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    copy(
        &repo_root.join("examples/services/redis.av"),
        &dir.join("redis.av"),
    );
    // Glob targets a different file → lint is a silent pass for redis.av.
    write(
        &dir.join("aver.toml"),
        r#"[[shape.expected]]
glob = "**/other.av"
layer = "Domain"
"#,
    );

    let out = Command::new(aver_bin())
        .current_dir(&dir)
        .args(["shape", "redis.av", "--lint", "--summary"])
        .output()
        .expect("run aver shape --lint");
    assert_eq!(
        out.status.code(),
        Some(0),
        "expected exit 0 when glob doesn't match the file, got {:?}.\nstderr: {}",
        out.status.code(),
        String::from_utf8_lossy(&out.stderr),
    );

    fs::remove_dir_all(&dir).ok();
}

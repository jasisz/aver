//! Process-spawning helpers shared by the integration suites: the path to
//! the `aver` binary under test, the repository root, `Output` formatting
//! for assertion messages, a scratch-module writer, and its cleanup.
//!
//! Each of these used to be redeclared verbatim (or with only a variable
//! name or `fs::` vs `std::fs::` spelling changed) in one suite after
//! another; this module is the single copy the identical suites include via
//! `#[path = "support/aver_cmd.rs"]`.
//!
//! Each including suite only calls the subset of helpers it needs, so this
//! module is recompiled (and dead-code-checked) once per test binary against
//! whatever subset that binary actually uses; `dead_code` is silenced here
//! rather than per call site.
#![allow(dead_code)]

use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::time::{SystemTime, UNIX_EPOCH};

/// Path to the `aver` binary built for this test run.
pub fn aver_bin() -> &'static str {
    env!("CARGO_BIN_EXE_aver")
}

/// The repository root, from Cargo's build-time manifest directory.
pub fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

/// Renders a process `Output` for assertion failure messages.
pub fn format_output(out: &Output) -> String {
    format!(
        "status: {}\nstdout:\n{}\nstderr:\n{}",
        out.status,
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
    )
}

/// An `aver` [`Command`] pointed at private, per-process certificate caches,
/// so repeated certify/verify runs in the same test binary reuse the
/// prelude and data stores instead of rebuilding them from scratch.
pub fn aver_command() -> Command {
    let mut command = Command::new(env!("CARGO_BIN_EXE_aver"));
    command.env(
        "AVER_CERT_PRELUDE_CACHE",
        std::env::temp_dir().join("aver-cert-prelude-store"),
    );
    command.env(
        "AVER_CERT_DATA_CACHE",
        std::env::temp_dir().join("aver-cert-data-store"),
    );
    command
}

/// Removes the parent directory of a `temp_module`-created source path.
pub fn cleanup(path: &Path) {
    let _ = std::fs::remove_dir_all(path.parent().expect("temp module has parent"));
}

/// Writes `source` to `main.av` inside a fresh directory named
/// `{prefix}-{nanos}`, returning the source file's path. Callers hold the
/// path until their `Command` completes, then pass it to `cleanup`.
pub fn temp_module(prefix: &str, source: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time before unix epoch")
        .as_nanos();
    let dir = std::env::temp_dir().join(format!("{prefix}-{nanos}"));
    std::fs::create_dir_all(&dir).expect("create temp dir");
    let path = dir.join("main.av");
    std::fs::write(&path, source).expect("write temp module source");
    path
}

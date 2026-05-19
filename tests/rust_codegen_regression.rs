//! Rust codegen regression suite (nightly tier).
//!
//! Per-example: `aver compile <file> --target rust -o <tmp>` then
//! `cargo check --manifest-path <tmp>/Cargo.toml`. Asserts the
//! emitted Rust project parses *and* type-checks under the real
//! Rust compiler — catches the class of bugs where `compile_to_rust`
//! emits syntactically-valid-but-semantically-broken source
//! (missing imports, mistyped fn signatures, wrong trait bounds,
//! generic drift).
//!
//! Why not in `Check & Test`: `cargo check` on the emitted project
//! pulls deps + runs full type inference; ~30-60 s per example, so
//! a 5-example corpus is ~5 min wall — too long for PR-smoke. The
//! workflow file `.github/workflows/rust-codegen.yml` runs this
//! as a nightly tier alongside the proof workflow.
//!
//! Locally you can run it explicitly with
//! `cargo test --test rust_codegen_regression -- --nocapture` if
//! you're poking at the Rust backend — the test discovers the
//! `aver` binary via `env!("CARGO_BIN_EXE_aver")` so no PATH
//! shenanigans.
//!
//! Corpus is hand-picked — only single-file examples that don't
//! depend on `Disk` / `Http` / `Tcp` effects, since cargo's
//! workspace-resolution rules choke on the wider service surface
//! in a fresh tempdir without project-level aver.toml. The
//! `decision/` examples skipped for the same reason. Real
//! coverage growth lands when the multi-module loader moves to
//! lib (same followup blocking the wasm-gc / wasip2 regressions).

#![cfg(feature = "runtime")]

use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

/// Examples to drive through `aver compile --target rust`. Picked
/// for: single-file (no `depends [...]`), no effect dependencies on
/// HTTP / Disk / Tcp, deterministic execution. Order is
/// alphabetical for stable test names.
const RUST_REGRESSION_CORPUS: &[&str] = &[
    "examples/core/calculator.av",
    "examples/core/hello.av",
    "examples/core/lambda.av",
    "examples/core/lists.av",
    "examples/core/shapes.av",
    "examples/core/temperature.av",
];

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

/// Reusable target directory for `cargo check` across all examples.
/// Sharing the target dir lets cargo dedupe dep compile across
/// per-example projects — first example pays the full ~30 s deps
/// build, the rest are seconds. Without this the corpus would
/// linearly multiply nightly time.
fn shared_target_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("target")
        .join("rust-codegen-regression-shared")
}

#[test]
fn rust_codegen_emits_buildable_project_for_every_corpus_example() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let shared_target = shared_target_dir();
    fs::create_dir_all(&shared_target).expect("create shared target dir");

    let mut failures: Vec<String> = Vec::new();
    let mut compiled = 0usize;

    for relative in RUST_REGRESSION_CORPUS {
        let source = repo_root.join(relative);
        if !source.exists() {
            failures.push(format!("{}: corpus file missing", relative));
            continue;
        }
        let workspace = temp_output_dir(&format!("rust-codegen-{}", sanitise(relative)));
        let project_dir = workspace.join("project");
        fs::create_dir_all(&project_dir).expect("create per-example workspace");

        // `aver compile --target rust -o <project_dir>` emits a
        // ready-to-build Cargo project: `Cargo.toml`, `src/`, etc.
        let compile = Command::new(aver_bin)
            .current_dir(&repo_root)
            .arg("compile")
            .arg(&source)
            .arg("--target")
            .arg("rust")
            .arg("-o")
            .arg(&project_dir)
            .output()
            .expect("expected `aver compile --target rust` to spawn");
        if !compile.status.success() {
            failures.push(format!(
                "{}: aver compile --target rust failed\n{}",
                relative,
                format_output(&compile)
            ));
            let _ = fs::remove_dir_all(&workspace);
            continue;
        }

        // `cargo check` against the emitted project. `--manifest-path`
        // + shared `target/` so dep compile cost amortises across
        // the corpus. `CARGO_TARGET_DIR` is the canonical env var
        // for redirecting the build directory.
        let check = Command::new("cargo")
            .arg("check")
            .arg("--manifest-path")
            .arg(project_dir.join("Cargo.toml"))
            .env("CARGO_TARGET_DIR", &shared_target)
            .output()
            .expect("expected `cargo check` to spawn");
        if !check.status.success() {
            failures.push(format!(
                "{}: cargo check failed on emitted project\n{}",
                relative,
                format_output(&check)
            ));
            let _ = fs::remove_dir_all(&workspace);
            continue;
        }
        compiled += 1;
        let _ = fs::remove_dir_all(&workspace);
    }

    if !failures.is_empty() {
        panic!(
            "{} of {} corpus examples failed Rust codegen + cargo check:\n  - {}",
            failures.len(),
            RUST_REGRESSION_CORPUS.len(),
            failures.join("\n  - ")
        );
    }
    eprintln!(
        "rust_codegen_emits_buildable_project_for_every_corpus_example: {} examples compiled + checked",
        compiled
    );
}

fn sanitise(relative: &str) -> String {
    relative
        .chars()
        .map(|c| if c.is_alphanumeric() { c } else { '-' })
        .collect()
}

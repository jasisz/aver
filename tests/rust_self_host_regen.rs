//! Self-host regeneration gates.
//!
//! The Rust backend is the SOLE self-host regeneration path:
//! `tools/release.py` delegates to `tools/regenerate_self_host.py`, which runs
//! `aver compile self_hosted/main.av --target rust ...` to regenerate the
//! Aver-in-Rust compiler under `src/self_host/aver_generated`.
//!
//! The ordinary CI test is a cheap freshness gate: regenerate and normalize in
//! a temporary directory, then compare with the checked-in tree without
//! touching the worktree. The ignored canary separately does a REAL `cargo
//! build` and runs corpus parity, catching codegen that is fresh but invalid.
//!
//! ## Cost / gating
//!
//! The regen emits ~36 files and the build pulls the full runtime dep
//! tree — minutes of wall time on a cold target dir. That expensive half is
//! too heavy for PR smoke, so it is `#[ignore]` + env-gated. Run it explicitly:
//!
//! ```text
//! AVER_SELF_HOST_REGEN=1 cargo test --test rust_self_host_regen -- --ignored --nocapture
//! ```
//!
//! The porting PR MUST run this (it's the regression canary), even
//! though the per-PR smoke job skips it.
//!
//! Gated on `runtime` — needs the `aver` binary + the local `aver-rt`
//! runtime that `aver compile` pins.

#![cfg(feature = "runtime")]

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn aver_bin() -> &'static str {
    env!("CARGO_BIN_EXE_aver")
}

fn temp_dir(prefix: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let dir = std::env::temp_dir().join(format!("aver-self-host-regen-{prefix}-{nanos}"));
    fs::create_dir_all(&dir).expect("create temp dir");
    dir
}

fn format_output(output: &std::process::Output) -> String {
    format!(
        "status: {}\nstdout:\n{}\nstderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}

/// Corpus programs to run through the regenerated CLI, asserting
/// stdout parity with the host VM. Single-file, deterministic,
/// Console-only — exercise records, sum types, match, list ops.
const PARITY_CORPUS: &[&str] = &[
    "examples/core/hello.av",
    "examples/core/calculator.av",
    "examples/core/shapes.av",
];

#[test]
fn checked_in_self_host_matches_release_regeneration() {
    let check = Command::new("python3")
        .current_dir(repo_root())
        .arg("tools/regenerate_self_host.py")
        .arg("--check")
        .arg("--aver-bin")
        .arg(aver_bin())
        .output()
        .expect("run read-only self-host freshness gate");
    assert!(
        check.status.success(),
        "self-host freshness gate failed:\n{}",
        format_output(&check)
    );
}

#[test]
#[ignore = "self-host regen + build is minutes of wall-time; set AVER_SELF_HOST_REGEN=1 and run with --ignored"]
fn self_host_regenerates_and_compiles_and_runs() {
    if std::env::var("AVER_SELF_HOST_REGEN").is_err() {
        eprintln!(
            "skipping self-host regen gate — set AVER_SELF_HOST_REGEN=1 to run \
             (regenerates self-host to a temp dir, cargo-builds it, runs corpus parity)"
        );
        return;
    }
    // The MIR walker is the sole Rust codegen path (the HIR walker was
    // deleted in W6/Stage-3). The emitted self-host is byte-identical to
    // what `release.py` ships into `src/self_host`. This is the densest
    // real-world MIR exercise in the net: a Rust-codegen regression on ANY
    // construct across the ~26k-line self-host compiler surfaces as the
    // regenerated self-host failing to build or miscompiling the corpus.
    run_self_host_regen(&[], "production (MIR-only)");
}

/// Regenerate the self-host compiler to a temp dir, cargo-build it, and
/// run the corpus asserting stdout parity with the host VM. The MIR
/// walker is the sole Rust codegen path; `mir_env` is retained as a
/// generic env hook (callers pass `&[]`). `label` tags the eprintln /
/// panic messages.
fn run_self_host_regen(mir_env: &[(&str, &str)], label: &str) {
    let repo = repo_root();
    let ws = temp_dir("ws");
    let out = ws.join("out");
    let target = ws.join("target");

    // ── (1) Regenerate the self-host compiler to a temp dir ──────────
    // Use the same helper `release.py` invokes, but emit OUTSIDE the repo so
    // this expensive canary never mutates the checked-in source.
    let mut regen_cmd = Command::new("python3");
    regen_cmd
        .current_dir(&repo)
        .arg("tools/regenerate_self_host.py")
        .arg("--output")
        .arg(&out)
        .arg("--aver-bin")
        .arg(aver_bin());
    for (k, v) in mir_env {
        regen_cmd.env(k, v);
    }
    let regen = regen_cmd
        .output()
        .expect("expected `aver compile self_hosted/main.av` to spawn");
    assert!(
        regen.status.success(),
        "self-host regeneration ({label}) failed:\n{}",
        format_output(&regen)
    );

    let generated_files: usize = fs::read_dir(out.join("src"))
        .map(|rd| rd.flatten().count())
        .unwrap_or(0);
    assert!(
        generated_files > 0,
        "self-host regeneration produced no src/ files"
    );

    // ── (2) cargo build the freshly-generated CLI (REAL build) ───────
    let build = Command::new("cargo")
        .arg("build")
        .arg("-q")
        .arg("--offline")
        .arg("--manifest-path")
        .arg(out.join("Cargo.toml"))
        .env("CARGO_TARGET_DIR", &target)
        .output()
        .expect("expected cargo build of regenerated self-host to spawn");
    assert!(
        build.status.success(),
        "regenerated self-host CLI ({label}) failed to compile — a Rust-codegen \
         regression broke the self-host path:\n{}",
        format_output(&build)
    );

    // Binary name is the source file stem ("main").
    let bin = target
        .join("debug")
        .join(format!("main{}", std::env::consts::EXE_SUFFIX));
    assert!(
        bin.exists(),
        "regenerated self-host binary not found at {}",
        bin.display()
    );

    // ── (3) Run the regenerated CLI over corpus programs, asserting
    // stdout parity with the host VM. Catches a regression that
    // compiles but miscompiles (the build alone wouldn't see it). The
    // CLI is invoked the same way `aver run --self-host` drives it:
    // `<bin> <file> <module_root>` with AVER_REPLAY_ENTRY_FN=main.
    let mut failures = Vec::new();
    for relative in PARITY_CORPUS {
        let file = repo.join(relative);
        let module_root = file.parent().expect("corpus file has parent");

        let vm = Command::new(aver_bin())
            .current_dir(&repo)
            .arg("run")
            .arg(&file)
            .output()
            .expect("host VM run");
        if !vm.status.success() {
            failures.push(format!(
                "{relative}: host VM run failed:\n{}",
                format_output(&vm)
            ));
            continue;
        }
        let vm_stdout = String::from_utf8_lossy(&vm.stdout).trim().to_string();

        let sh = run_self_host(&bin, &file, module_root);
        match sh {
            Ok(sh_stdout) if sh_stdout == vm_stdout => {}
            Ok(sh_stdout) => failures.push(format!(
                "{relative}: self-host stdout mismatch\n--- VM ---\n{vm_stdout}\n--- self-host ---\n{sh_stdout}"
            )),
            Err(e) => failures.push(format!("{relative}: {e}")),
        }
    }

    let pass = PARITY_CORPUS.len() - failures.len();
    eprintln!(
        "self-host regen [{label}]: compiled OK ({generated_files} src files), \
         corpus parity {pass}/{}",
        PARITY_CORPUS.len()
    );

    let _ = fs::remove_dir_all(&ws);

    assert!(
        failures.is_empty(),
        "self-host regen [{label}]: {} of {} corpus programs failed parity:\n  - {}",
        failures.len(),
        PARITY_CORPUS.len(),
        failures.join("\n  - ")
    );
}

fn run_self_host(bin: &Path, file: &Path, module_root: &Path) -> Result<String, String> {
    let out = Command::new(bin)
        .arg(file)
        .arg(module_root)
        .env("AVER_REPLAY_ENTRY_FN", "main")
        .env("AVER_REPLAY_MODULE_ROOT", module_root)
        .output()
        .map_err(|e| format!("failed to launch self-host binary: {e}"))?;
    if !out.status.success() {
        return Err(format!("self-host run failed:\n{}", format_output(&out)));
    }
    Ok(String::from_utf8_lossy(&out.stdout).trim().to_string())
}

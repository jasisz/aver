//! Self-host regeneration gate (Wave 0 of the rust-on-MIR port).
//!
//! The Rust backend is the SOLE self-host regeneration path:
//! `tools/release.py` runs `aver compile self_hosted/main.av --target
//! rust ...` to regenerate the ~26k-line Aver-in-Rust compiler under
//! `src/self_host/aver_generated`. Nothing in CI exercises that path
//! today — a Rust-codegen regression would slip through every test and
//! only surface at the next release, when the self-host fails to
//! regenerate or the regenerated compiler miscompiles.
//!
//! This gate is the canary. It regenerates the self-host compiler to a
//! temp dir via the exact `release.py` invocation, then does a REAL
//! `cargo build` of the freshly-generated `aver_self_host_cli`,
//! asserting it COMPILES. On top of that it RUNS the regenerated CLI
//! over a few corpus programs and asserts stdout parity with the host
//! VM — so a codegen regression that compiles-but-miscompiles also
//! fails here.
//!
//! ## Cost / gating
//!
//! The regen emits ~36 files and the build pulls the full runtime dep
//! tree — minutes of wall time on a cold target dir. Too heavy for PR
//! smoke, so it is `#[ignore]` + env-gated. Run it explicitly:
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
#[ignore = "self-host regen + build is minutes of wall-time; set AVER_SELF_HOST_REGEN=1 and run with --ignored"]
fn self_host_regenerates_and_compiles_and_runs() {
    if std::env::var("AVER_SELF_HOST_REGEN").is_err() {
        eprintln!(
            "skipping self-host regen gate — set AVER_SELF_HOST_REGEN=1 to run \
             (regenerates self-host to a temp dir, cargo-builds it, runs corpus parity)"
        );
        return;
    }
    // Production path: no MIR flags. The emitted self-host is byte-
    // identical to what `release.py` ships into `src/self_host`.
    run_self_host_regen(&[], "production");
}

/// All four MIR flags — Stage-1 forces the MIR walker to OWN the entire
/// emit (body, TCO, verify, main + top-stmt) across the ~26k-line
/// self-host compiler. This is the densest real-world MIR exercise in the
/// net: a Rust-codegen regression on ANY construct surfaces as the
/// regenerated self-host failing to build or miscompiling the corpus.
const FORCED_MIR: &[(&str, &str)] = &[
    ("AVER_RUST_MIR_ONLY", "1"),
    ("AVER_RUST_MIR_TCO", "1"),
    ("AVER_RUST_MIR_VERIFY", "1"),
    ("AVER_RUST_MIR_MAIN", "1"),
];

#[test]
#[ignore = "self-host regen + build is minutes of wall-time; set AVER_SELF_HOST_REGEN=1 and run with --ignored"]
fn forced_mir_self_host_regenerates_and_compiles_and_runs() {
    if std::env::var("AVER_SELF_HOST_REGEN").is_err() {
        eprintln!(
            "skipping forced-MIR self-host regen gate — set AVER_SELF_HOST_REGEN=1 to \
             run (regenerates the self-host under ALL FOUR MIR flags, cargo-builds it, \
             runs corpus parity — the densest MIR exercise in the Stage-1 net)"
        );
        return;
    }
    run_self_host_regen(FORCED_MIR, "forced-MIR (all four flags)");
}

/// Regenerate the self-host compiler to a temp dir under the given MIR
/// env (empty = production HIR path), cargo-build it, and run the corpus
/// asserting stdout parity with the host VM. `label` tags the eprintln /
/// panic messages so the production vs forced-MIR runs are distinguishable.
fn run_self_host_regen(mir_env: &[(&str, &str)], label: &str) {
    let repo = repo_root();
    let ws = temp_dir("ws");
    let out = ws.join("out");
    let target = ws.join("target");

    // ── (1) Regenerate the self-host compiler to a temp dir ──────────
    // Exactly the `release.py::regenerate_self_host` invocation. We
    // emit OUTSIDE the repo (temp dir) so the gate never mutates the
    // checked-in `src/self_host` — it's a read-only canary. The MIR env
    // (if any) forces the MIR walker to own the emit.
    let mut regen_cmd = Command::new(aver_bin());
    regen_cmd
        .current_dir(&repo)
        .arg("compile")
        .arg("self_hosted/main.av")
        .arg("--target")
        .arg("rust")
        .arg("--output")
        .arg(&out)
        .arg("--module-root")
        .arg("self_hosted")
        .arg("--with-self-host-support")
        .arg("--guest-entry")
        .arg("runGuestCliProgram")
        .arg("--with-replay")
        .arg("--policy")
        .arg("runtime");
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

    // The release flow deletes the test-only verify.rs (it can't build
    // as a sibling module). Mirror that so the temp project builds the
    // same shape release.py copies into src/self_host.
    let verify_rs = out.join("src").join("verify.rs");
    if verify_rs.exists() {
        // verify.rs is only referenced under #[cfg(test)] in main.rs, so
        // a plain `cargo build` ignores it; removing it keeps parity
        // with the release artifact regardless.
        let _ = fs::remove_file(&verify_rs);
    }

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

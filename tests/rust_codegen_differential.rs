//! Rust backend behavioral parity harness (Wave 0 of the rust-on-MIR port).
//!
//! The pre-existing Rust codegen tests (`rust_codegen_regression.rs`)
//! are all `cargo check`-only: they prove the emitted source *parses
//! and type-checks*, not that it *behaves*. That gap is exactly the
//! class of bug the rust-on-MIR port can introduce — a "covered" fn
//! can emit Rust that fails rustc's borrow checker, or silently drops
//! a policy / replay wrapper while still type-checking and producing
//! identical happy-path stdout. This harness closes the gap by doing
//! a real `cargo build` (the borrow-check) + RUN + behavioral assert.
//!
//! Three behavioral modes — plain stdout parity is NOT enough on its
//! own (a dropped policy or replay wrapper type-checks and produces
//! identical happy-path stdout):
//!
//! - **plain**: `aver compile --target rust` → `cargo build` → run the
//!   binary → assert stdout equals the VM run (`aver run`).
//! - **deny-policy**: compile+run a Disk-write program under a runtime
//!   `aver.toml` that DENIES the write path; assert the built binary
//!   REJECTS the effect at runtime (catches a dropped
//!   `aver_policy::check_*` wrapper).
//! - **record/replay**: `--record` a run then replay it; assert the
//!   recording captures every effect with the right per-effect
//!   arg-json shape, and the replay roundtrips (catches a dropped
//!   `aver_replay::invoke_effect` wrapper).
//!
//! ## Tiers
//!
//! - **fast**: a 3-example plain-parity subset + the two critical
//!   behavioral probes (deny-policy, record/replay). Runs on every
//!   `cargo test` invocation. ~one cargo dep-build then seconds each.
//! - **full**: every single-file example + the multi-module (`depends`)
//!   examples, plain-parity only. Gated behind the `AVER_RUST_DIFF_FULL`
//!   env var (the dep-build + per-example build is minutes of wall
//!   time — too heavy for PR smoke). Run it with
//!   `AVER_RUST_DIFF_FULL=1 cargo test --test rust_codegen_differential -- --ignored --nocapture`.
//!
//! ## Why this is the porting safety net, not theater
//!
//! `rust_codegen_revert.rs` (the sibling self-checking revert-test
//! suite) demonstrates that breaking the HIR emitter — dropping a
//! `.clone()`, dropping the policy wrapper, dropping the replay
//! wrapper — turns each mode RED. A net that passes with AND without
//! the bug proves nothing; the revert evidence is what makes this one
//! trustworthy.
//!
//! Gated on `runtime` (the default feature set) — needs the `aver`
//! binary + the local `aver-rt` runtime that `aver compile` pins.

#![cfg(feature = "runtime")]

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

// ─── Shared infrastructure ──────────────────────────────────────────────

/// Monotonic counter so concurrently-running tests never collide on a
/// temp-dir name even within the same nanosecond.
static UNIQUE: AtomicU64 = AtomicU64::new(0);

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
    let n = UNIQUE.fetch_add(1, Ordering::Relaxed);
    let dir = std::env::temp_dir().join(format!("aver-rust-diff-{prefix}-{nanos}-{n}"));
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

/// Single `cargo build` target dir shared across every example in one
/// process so the (slow) dependency compile amortises — the first
/// example pays it, the rest are seconds.
fn shared_target_dir() -> PathBuf {
    repo_root()
        .join("target")
        .join("rust-codegen-differential-shared")
}

fn binary_name(name: &str) -> String {
    format!("{name}{}", std::env::consts::EXE_SUFFIX)
}

/// `aver run <file>` (VM) — the parity oracle. Returns trimmed stdout.
fn run_vm(file: &Path, module_root: Option<&Path>) -> Result<String, String> {
    let mut cmd = Command::new(aver_bin());
    cmd.current_dir(repo_root()).arg("run").arg(file);
    if let Some(root) = module_root {
        cmd.arg("--module-root").arg(root);
    }
    let out = cmd.output().expect("expected `aver run` (VM) to execute");
    if !out.status.success() {
        return Err(format!("VM run failed:\n{}", format_output(&out)));
    }
    Ok(String::from_utf8_lossy(&out.stdout).trim().to_string())
}

/// Compile `file` to a Rust project at `project_dir`. Extra args are
/// appended verbatim (e.g. `--policy runtime`, `--with-replay`).
fn compile_rust(
    file: &Path,
    project_dir: &Path,
    name: &str,
    module_root: Option<&Path>,
    extra: &[&str],
) -> Result<(), String> {
    compile_rust_env(file, project_dir, name, module_root, extra, &[])
}

/// As [`compile_rust`], but sets extra env vars on the `aver compile`
/// process. Used by the MIR-forced security test to set
/// `AVER_RUST_MIR_ONLY=1` so the parity gate forces the MIR walker to
/// OWN the effectful builtin emission (replay / policy / bare framing)
/// instead of falling back to the byte-equal HIR body.
fn compile_rust_env(
    file: &Path,
    project_dir: &Path,
    name: &str,
    module_root: Option<&Path>,
    extra: &[&str],
    env: &[(&str, &str)],
) -> Result<(), String> {
    let mut cmd = Command::new(aver_bin());
    cmd.current_dir(repo_root())
        .arg("compile")
        .arg(file)
        .arg("--target")
        .arg("rust")
        .arg("--name")
        .arg(name)
        .arg("-o")
        .arg(project_dir);
    if let Some(root) = module_root {
        cmd.arg("--module-root").arg(root);
    }
    cmd.args(extra);
    for (k, v) in env {
        cmd.env(k, v);
    }
    let out = cmd
        .output()
        .expect("expected `aver compile --target rust` to spawn");
    if !out.status.success() {
        return Err(format!(
            "aver compile --target rust failed:\n{}",
            format_output(&out)
        ));
    }
    Ok(())
}

/// `aver compile … --explain-mir-coverage --target rust --json` →
/// parse the `graduated` count. Used by the MIR-forced security test to
/// PROVE the parity gate actually graduated the effectful fn onto the
/// MIR path (so the deny / replay assertions exercise MIR-emitted
/// effects, not an accidental HIR fallback).
fn graduated_count(
    file: &Path,
    module_root: Option<&Path>,
    extra: &[&str],
    env: &[(&str, &str)],
) -> Result<u64, String> {
    let mut cmd = Command::new(aver_bin());
    cmd.current_dir(repo_root())
        .arg("compile")
        .arg(file)
        .arg("--explain-mir-coverage")
        .arg("--target")
        .arg("rust")
        .arg("--json");
    if let Some(root) = module_root {
        cmd.arg("--module-root").arg(root);
    }
    cmd.args(extra);
    for (k, v) in env {
        cmd.env(k, v);
    }
    let out = cmd
        .output()
        .expect("expected `aver compile --explain-mir-coverage` to spawn");
    if !out.status.success() {
        return Err(format!(
            "explain-mir-coverage failed:\n{}",
            format_output(&out)
        ));
    }
    let json = String::from_utf8_lossy(&out.stdout);
    // Tiny field extractor — avoids pulling serde into the test.
    let needle = "\"graduated\":";
    let start = json
        .find(needle)
        .ok_or_else(|| format!("no `graduated` field in coverage JSON:\n{json}"))?
        + needle.len();
    let rest = &json[start..];
    let end = rest
        .find(|c: char| !c.is_ascii_digit())
        .unwrap_or(rest.len());
    rest[..end]
        .parse::<u64>()
        .map_err(|e| format!("bad graduated count {:?}: {e}", &rest[..end]))
}

/// `cargo build` the emitted project against the shared target dir.
/// This is a REAL build (not `cargo check`) so move / borrow / Arc
/// bugs that pass `check` but fail `build` surface here. Returns the
/// path to the produced binary.
fn cargo_build(project_dir: &Path, name: &str) -> Result<PathBuf, String> {
    cargo_build_in(project_dir, name, &shared_target_dir())
}

/// As [`cargo_build`], but builds against an explicit target dir. The
/// forced-MIR full-corpus tier passes a PER-EXAMPLE target dir here so
/// the concurrent generated-project builds never share a target tree —
/// `--offline` + a shared `CARGO_TARGET_DIR` races on the `.rmeta` /
/// proc-macro outputs (observed during the W6 audit), and the per-test
/// isolation is what keeps this standing net from being flaky.
fn cargo_build_in(project_dir: &Path, name: &str, target: &Path) -> Result<PathBuf, String> {
    fs::create_dir_all(target).expect("create cargo target dir");
    let out = Command::new("cargo")
        .arg("build")
        .arg("-q")
        .arg("--offline")
        .arg("--manifest-path")
        .arg(project_dir.join("Cargo.toml"))
        .env("CARGO_TARGET_DIR", target)
        .output()
        .expect("expected `cargo build` to spawn");
    if !out.status.success() {
        return Err(format!(
            "cargo build failed on emitted project:\n{}",
            format_output(&out)
        ));
    }
    Ok(target.join("debug").join(binary_name(name)))
}

// ─── Mode (a): plain stdout parity ──────────────────────────────────────

/// Compile + build + RUN an example, asserting stdout equals the VM.
fn assert_plain_parity(relative: &str, module_root: Option<&str>) -> Result<(), String> {
    let file = repo_root().join(relative);
    if !file.exists() {
        return Err(format!("{relative}: corpus file missing"));
    }
    let root = module_root.map(|r| repo_root().join(r));
    let vm_stdout = run_vm(&file, root.as_deref())?;

    let ws = temp_dir(&sanitise(relative));
    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let name = format!("p_{}", sanitise(relative));

    let result = (|| {
        compile_rust(&file, &project, &name, root.as_deref(), &[])?;
        let bin = cargo_build(&project, &name)?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("failed to run compiled binary: {e}"))?;
        if !out.status.success() {
            return Err(format!(
                "{relative}: compiled binary exited non-zero:\n{}",
                format_output(&out)
            ));
        }
        let rust_stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if rust_stdout != vm_stdout {
            return Err(format!(
                "{relative}: stdout mismatch\n--- VM ---\n{vm_stdout}\n--- Rust ---\n{rust_stdout}"
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result
}

fn sanitise(relative: &str) -> String {
    relative
        .chars()
        .map(|c| if c.is_alphanumeric() { c } else { '_' })
        .collect()
}

// ─── Fast tier ──────────────────────────────────────────────────────────

/// 3-example plain-parity subset for the fast (every-CI) tier. Picked
/// for: single-file, deterministic (no Time / Random / Http), exercises
/// records + sum types + match + list ops + recursion.
const FAST_PLAIN: &[&str] = &[
    "examples/core/calculator.av",
    "examples/core/shapes.av",
    "examples/core/lists.av",
];

#[test]
fn fast_plain_stdout_parity_with_vm() {
    let mut failures = Vec::new();
    for relative in FAST_PLAIN {
        if let Err(e) = assert_plain_parity(relative, None) {
            failures.push(e);
        }
    }
    assert!(
        failures.is_empty(),
        "{} of {} fast plain-parity examples failed:\n  - {}",
        failures.len(),
        FAST_PLAIN.len(),
        failures.join("\n  - ")
    );
}

// ─── Mode (b): deny-policy ──────────────────────────────────────────────

/// A Disk-write program. `__PATH__` is substituted with the real
/// write target at test time. Routed through a helper fn so the
/// effect rides a normal cross-fn call (the same shape the policy
/// wrapper guards).
const DISK_WRITE_PROBE: &str = r#"module DiskProbe
    intent =
        "Writes one file then prints DONE. Probes the policy wrapper:"
        "under a deny policy the write must be rejected at runtime."
    effects [Console, Disk]

fn writeIt(path: String) -> Result<Unit, String>
    ? "Writes a fixed payload to the given path."
    ! [Disk.writeText]
    Disk.writeText(path, "payload")

fn main() -> Result<Unit, String>
    ! [Console.print, Disk.writeText]
    written = writeIt("__PATH__")?
    shown = Console.print("DONE")
    Result.Ok(Unit)
"#;

fn write_runtime_disk_policy(dir: &Path, allowed_path: &str) {
    fs::create_dir_all(dir).expect("create policy dir");
    fs::write(
        dir.join("aver.toml"),
        format!("[effects.Disk]\npaths = [{allowed_path:?}]\n"),
    )
    .expect("write aver.toml");
}

#[test]
fn deny_policy_rejects_denied_disk_write_at_runtime() {
    let ws = temp_dir("deny");
    let out_path = ws.join("out.txt");
    let src = ws.join("disk_probe.av");
    fs::write(
        &src,
        DISK_WRITE_PROBE.replace("__PATH__", &aver_path_literal(&out_path)),
    )
    .expect("write probe source");

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let name = "deny_disk_probe";

    let result = (|| -> Result<(), String> {
        // `--policy runtime` loads aver.toml at run time from
        // AVER_REPLAY_MODULE_ROOT, so one built binary serves both
        // the deny and the allow probe.
        compile_rust(&src, &project, name, None, &["--policy", "runtime"])?;
        let bin = cargo_build(&project, name)?;

        // (1) DENY: allow-list names a DIFFERENT path → the write to
        // out.txt is denied. Binary must exit non-zero and NOT create
        // the file.
        let deny_root = ws.join("deny-policy");
        write_runtime_disk_policy(&deny_root, "/aver/nonexistent/allowed/only");
        let denied = Command::new(&bin)
            .env("AVER_REPLAY_MODULE_ROOT", &deny_root)
            .output()
            .map_err(|e| format!("run denied binary: {e}"))?;
        if denied.status.success() {
            return Err(format!(
                "deny-policy run unexpectedly SUCCEEDED — the policy wrapper \
                 was not enforced:\n{}",
                format_output(&denied)
            ));
        }
        let denied_stderr = String::from_utf8_lossy(&denied.stderr);
        if !denied_stderr.contains("denied by aver.toml policy") {
            return Err(format!(
                "deny-policy run failed for the wrong reason (expected a \
                 policy violation):\n{}",
                format_output(&denied)
            ));
        }
        if out_path.exists() {
            return Err(format!(
                "deny-policy run wrote the file at {} despite the deny policy — \
                 the policy check ran AFTER the effect (or not at all)",
                out_path.display()
            ));
        }

        // (2) ALLOW: allow-list names the real write path → the write
        // is permitted. Proves the deny in (1) was the policy, not an
        // unconditional failure.
        let allow_root = ws.join("allow-policy");
        write_runtime_disk_policy(&allow_root, &out_path.to_string_lossy());
        let allowed = Command::new(&bin)
            .env("AVER_REPLAY_MODULE_ROOT", &allow_root)
            .output()
            .map_err(|e| format!("run allowed binary: {e}"))?;
        if !allowed.status.success() {
            return Err(format!(
                "allow-policy run failed — the probe should succeed when the \
                 write path is permitted:\n{}",
                format_output(&allowed)
            ));
        }
        if !out_path.exists() {
            return Err(format!(
                "allow-policy run did not write {} — the effect was suppressed \
                 even though the policy allowed it",
                out_path.display()
            ));
        }
        let allowed_stdout = String::from_utf8_lossy(&allowed.stdout);
        if !allowed_stdout.contains("DONE") {
            return Err(format!(
                "allow-policy run did not print DONE:\n{}",
                format_output(&allowed)
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

fn aver_path_literal(path: &Path) -> String {
    // Aver string literal — escape backslashes and quotes.
    path.to_string_lossy()
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
}

// ─── Mode (c): record / replay ──────────────────────────────────────────

/// Reads a file, then echoes its contents via Console.print. The read
/// result must be woven into the print arg, so the recorded
/// `Console.print` arg-json proves the `Disk.readText` result flowed
/// through. `__PATH__` is substituted at test time.
const READ_ECHO_PROBE: &str = r#"module RwProbe
    intent =
        "Reads a file and echoes its contents. The record captures the read"
        "result; replay serves it back. Probes the replay wrapper."
    effects [Console, Disk]

fn main() -> Result<Unit, String>
    ! [Console.print, Disk.readText]
    content = Disk.readText("__PATH__")?
    shown = Console.print("READ:{content}")
    Result.Ok(Unit)
"#;

#[test]
fn record_replay_roundtrips_effects_through_invoke_wrapper() {
    let ws = temp_dir("replay");
    let data_path = ws.join("data.txt");
    fs::write(&data_path, "recorded-bytes").expect("write probe data");
    let src = ws.join("rw_probe.av");
    fs::write(
        &src,
        READ_ECHO_PROBE.replace("__PATH__", &aver_path_literal(&data_path)),
    )
    .expect("write probe source");

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let name = "rw_probe";

    let result = (|| -> Result<(), String> {
        compile_rust(&src, &project, name, None, &["--with-replay"])?;
        let bin = cargo_build(&project, name)?;

        // (1) RECORD: run live, capturing the effects into a session.
        let session = ws.join("session.json");
        let recorded = Command::new(&bin)
            .env("AVER_REPLAY_RECORD", &session)
            .output()
            .map_err(|e| format!("run record binary: {e}"))?;
        if !recorded.status.success() {
            return Err(format!("record run failed:\n{}", format_output(&recorded)));
        }
        let recorded_stdout = String::from_utf8_lossy(&recorded.stdout);
        if !recorded_stdout.contains("READ:recorded-bytes") {
            return Err(format!(
                "record run did not echo the read bytes (live read broken):\n{}",
                format_output(&recorded)
            ));
        }
        if !session.exists() {
            return Err("record run did not write the session JSON".to_string());
        }

        // The session must capture BOTH effects through invoke_effect.
        // A dropped replay wrapper makes one (or both) vanish.
        let session_json = fs::read_to_string(&session).expect("read session");
        // Disk.readText recorded with its result.
        if !session_json.contains("\"Disk.readText\"") {
            return Err(format!(
                "session is missing the Disk.readText effect — the replay \
                 wrapper was dropped on the read:\n{session_json}"
            ));
        }
        // Console.print recorded — its arg-json proves the read result
        // flowed through into the printed string (per-effect arg shape).
        if !session_json.contains("\"Console.print\"") {
            return Err(format!(
                "session is missing the Console.print effect — the replay \
                 wrapper was dropped on the print:\n{session_json}"
            ));
        }
        if !session_json.contains("READ:recorded-bytes") {
            return Err(format!(
                "session does not carry the woven read result in the \
                 Console.print arg — per-effect arg-json shape is wrong:\n{session_json}"
            ));
        }

        // (2) REPLAY: mutate the data file so a LIVE read would differ,
        // then replay. Replay must serve the recorded bytes from the
        // session (not re-read the mutated file) and roundtrip the
        // recorded effects without a position mismatch.
        fs::write(&data_path, "MUTATED-ON-DISK").expect("mutate data file");
        let replayed = Command::new(&bin)
            .env("AVER_REPLAY_REPLAY", &session)
            .output()
            .map_err(|e| format!("run replay binary: {e}"))?;
        if !replayed.status.success() {
            return Err(format!(
                "replay run failed — the recorded session did not roundtrip:\n{}",
                format_output(&replayed)
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

// ─── Mode (d): MIR-FORCED effectful security probe (Wave 3b) ─────────────
//
// SECURITY-SENSITIVE. Wave 3b lets the MIR walker OWN effectful builtin
// emission (replay / policy / bare framing). A dropped wrapper there
// silently disables aver.toml DENY enforcement or record/replay capture
// — and it's invisible to rustc, to coverage, and to happy-path stdout.
//
// The byte-parity gate would normally fall an effectful fn back to HIR
// the instant the MIR body diverged, so a dropped MIR wrapper would
// hide behind the HIR fallback. The `AVER_RUST_MIR_ONLY=1` escape hatch
// disables the byte-exact fallback: for any fn the MIR walker renders,
// the MIR body is forced onto production EVEN IF it diverges from HIR.
// That makes the MIR walker provably OWN the effect emission, so a
// dropped MIR wrapper actually reaches the built binary and these
// probes catch it.
//
// `graduated_count(... AVER_RUST_MIR_ONLY=1)` asserts up front that the
// effectful fn really graduated onto the MIR path — without that guard
// a silent HIR fallback would let the probe pass for the wrong reason.

/// Single-fn Disk-write program for the embedded-policy probe. The
/// write rides a helper fn (`writeIt`) that is a single-expr body — the
/// shape the parity gate graduates onto MIR — so the `aver_policy::
/// check_disk` wrapper is emitted by the MIR walker under the hatch.
/// `__PATH__` is substituted at test time.
const MIR_DISK_WRITE_PROBE: &str = r#"module MirDiskProbe
    intent =
        "Writes one file then prints DONE. Probes the MIR-emitted policy"
        "wrapper: under a deny policy the write must be rejected at runtime."
    effects [Console, Disk]

fn writeIt(path: String) -> Result<Unit, String>
    ? "Writes a fixed payload to the given path."
    ! [Disk.writeText]
    Disk.writeText(path, "payload")

fn main() -> Result<Unit, String>
    ! [Console.print, Disk.writeText]
    written = writeIt("__PATH__")?
    shown = Console.print("DONE")
    Result.Ok(Unit)
"#;

fn write_embedded_disk_policy(dir: &Path, allowed_path: &str) {
    fs::create_dir_all(dir).expect("create policy dir");
    fs::write(
        dir.join("aver.toml"),
        format!("[effects.Disk]\npaths = [{allowed_path:?}]\n"),
    )
    .expect("write aver.toml");
}

#[test]
#[ignore = "full tier: cargo build wall-time; set AVER_RUST_DIFF_FULL=1 and run with --include-ignored"]
fn mir_forced_embedded_policy_rejects_denied_disk_write() {
    if std::env::var("AVER_RUST_DIFF_FULL").is_err() {
        eprintln!("skipping MIR-forced policy probe — set AVER_RUST_DIFF_FULL=1");
        return;
    }

    let ws = temp_dir("mir-deny");
    // The embedded aver.toml is read from the module root at compile
    // time, so the source + the deny aver.toml share a dir.
    let proj_root = ws.join("src-root");
    fs::create_dir_all(&proj_root).expect("create src root");
    let out_path = ws.join("out.txt");
    let src = proj_root.join("disk_probe.av");
    fs::write(
        &src,
        MIR_DISK_WRITE_PROBE.replace("__PATH__", &aver_path_literal(&out_path)),
    )
    .expect("write probe source");

    let hatch = [("AVER_RUST_MIR_ONLY", "1")];

    let result = (|| -> Result<(), String> {
        // (0) PROVE the MIR path is actually exercised: the effectful
        // `writeIt` must graduate onto MIR under the hatch. Without
        // this, a silent HIR fallback would make the probe meaningless.
        let grad = graduated_count(&src, Some(&proj_root), &["--policy", "embed"], &hatch)?;
        if grad == 0 {
            return Err(
                "no fn graduated onto the MIR path under AVER_RUST_MIR_ONLY=1 — the \
                 effectful builtin emit is not being exercised by the MIR walker; \
                 the probe would be testing the HIR fallback instead"
                    .to_string(),
            );
        }

        // (1) DENY: embed an aver.toml whose allow-list names a
        // DIFFERENT path → the write to out.txt is denied at compile-
        // baked policy. Compile under the hatch so the MIR walker emits
        // the `aver_policy::check_disk` wrapper, then build + run.
        write_embedded_disk_policy(&proj_root, "/aver/nonexistent/allowed/only");
        let project = ws.join("project-deny");
        fs::create_dir_all(&project).expect("create project dir");
        let name = "mir_deny_disk_probe";
        compile_rust_env(
            &src,
            &project,
            name,
            Some(&proj_root),
            &["--policy", "embed"],
            &hatch,
        )?;
        // Sanity: the emitted source must carry the MIR-emitted policy
        // wrapper. (If a future refactor drops it, the run-time assert
        // below is the real gate; this is a fast structural tripwire.)
        let emitted = fs::read_to_string(
            project
                .join("src")
                .join("aver_generated")
                .join("entry")
                .join("mod.rs"),
        )
        .map_err(|e| format!("read emitted module: {e}"))?;
        if !emitted.contains("aver_policy::check_disk") {
            return Err(format!(
                "emitted Rust is missing the `aver_policy::check_disk` wrapper — \
                 the MIR effectful policy wrap was dropped:\n{emitted}"
            ));
        }

        let bin = cargo_build(&project, name)?;
        let denied = Command::new(&bin)
            .output()
            .map_err(|e| format!("run denied binary: {e}"))?;
        if denied.status.success() {
            return Err(format!(
                "deny run unexpectedly SUCCEEDED — the MIR-emitted policy wrapper \
                 was not enforced:\n{}",
                format_output(&denied)
            ));
        }
        let denied_stderr = String::from_utf8_lossy(&denied.stderr);
        if !denied_stderr.contains("denied by aver.toml policy") {
            return Err(format!(
                "deny run failed for the wrong reason (expected a policy \
                 violation):\n{}",
                format_output(&denied)
            ));
        }
        if out_path.exists() {
            return Err(format!(
                "deny run wrote {} despite the deny policy — the MIR-emitted \
                 check ran AFTER the effect (or not at all)",
                out_path.display()
            ));
        }

        // (2) ALLOW: re-embed an aver.toml whose allow-list names the
        // real write path → the write is permitted. Proves the deny in
        // (1) was the policy, not an unconditional failure.
        write_embedded_disk_policy(&proj_root, &out_path.to_string_lossy());
        let project_allow = ws.join("project-allow");
        fs::create_dir_all(&project_allow).expect("create allow project dir");
        let name_allow = "mir_allow_disk_probe";
        compile_rust_env(
            &src,
            &project_allow,
            name_allow,
            Some(&proj_root),
            &["--policy", "embed"],
            &hatch,
        )?;
        let bin_allow = cargo_build(&project_allow, name_allow)?;
        let allowed = Command::new(&bin_allow)
            .output()
            .map_err(|e| format!("run allowed binary: {e}"))?;
        if !allowed.status.success() {
            return Err(format!(
                "allow run failed — the probe should succeed when the write path \
                 is permitted:\n{}",
                format_output(&allowed)
            ));
        }
        if !out_path.exists() {
            return Err(format!(
                "allow run did not write {} — the effect was suppressed even \
                 though the policy allowed it",
                out_path.display()
            ));
        }
        if !String::from_utf8_lossy(&allowed.stdout).contains("DONE") {
            return Err(format!(
                "allow run did not print DONE:\n{}",
                format_output(&allowed)
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

/// Reads a file, then echoes its contents via Console.print — same
/// shape as `READ_ECHO_PROBE`, but compiled under
/// `AVER_RUST_MIR_ONLY=1` so the MIR walker emits the
/// `aver_replay::invoke_effect` reroute for BOTH effects. `__PATH__` is
/// substituted at test time.
const MIR_READ_ECHO_PROBE: &str = r#"module MirRwProbe
    intent =
        "Reads a file and echoes its contents. The record captures the read"
        "result; replay serves it back. Probes the MIR-emitted replay wrapper."
    effects [Console, Disk]

fn readIt(path: String) -> Result<String, String>
    ? "Reads the file at the given path."
    ! [Disk.readText]
    Disk.readText(path)

fn main() -> Result<Unit, String>
    ! [Console.print, Disk.readText]
    content = readIt("__PATH__")?
    shown = Console.print("READ:{content}")
    Result.Ok(Unit)
"#;

#[test]
#[ignore = "full tier: cargo build wall-time; set AVER_RUST_DIFF_FULL=1 and run with --include-ignored"]
fn mir_forced_record_replay_captures_effects_through_invoke_wrapper() {
    if std::env::var("AVER_RUST_DIFF_FULL").is_err() {
        eprintln!("skipping MIR-forced replay probe — set AVER_RUST_DIFF_FULL=1");
        return;
    }

    let ws = temp_dir("mir-replay");
    let data_path = ws.join("data.txt");
    fs::write(&data_path, "recorded-bytes").expect("write probe data");
    let src = ws.join("rw_probe.av");
    fs::write(
        &src,
        MIR_READ_ECHO_PROBE.replace("__PATH__", &aver_path_literal(&data_path)),
    )
    .expect("write probe source");

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let name = "mir_rw_probe";
    let hatch = [("AVER_RUST_MIR_ONLY", "1")];

    let result = (|| -> Result<(), String> {
        // (0) PROVE the MIR path is exercised: the effectful `readIt`
        // must graduate onto MIR under the hatch.
        let grad = graduated_count(&src, None, &["--with-replay"], &hatch)?;
        if grad == 0 {
            return Err(
                "no fn graduated onto the MIR path under AVER_RUST_MIR_ONLY=1 — the \
                 MIR replay reroute is not being exercised"
                    .to_string(),
            );
        }

        compile_rust_env(&src, &project, name, None, &["--with-replay"], &hatch)?;

        // Structural tripwire: the emitted Rust must carry the MIR-
        // emitted `invoke_effect` reroute for the read.
        let emitted = fs::read_to_string(
            project
                .join("src")
                .join("aver_generated")
                .join("entry")
                .join("mod.rs"),
        )
        .map_err(|e| format!("read emitted module: {e}"))?;
        if !emitted.contains("aver_replay::invoke_effect") {
            return Err(format!(
                "emitted Rust is missing the `aver_replay::invoke_effect` reroute — \
                 the MIR effectful replay wrap was dropped:\n{emitted}"
            ));
        }

        let bin = cargo_build(&project, name)?;

        // (1) RECORD: run live, capturing the effects into a session.
        let session = ws.join("session.json");
        let recorded = Command::new(&bin)
            .env("AVER_REPLAY_RECORD", &session)
            .output()
            .map_err(|e| format!("run record binary: {e}"))?;
        if !recorded.status.success() {
            return Err(format!("record run failed:\n{}", format_output(&recorded)));
        }
        if !String::from_utf8_lossy(&recorded.stdout).contains("READ:recorded-bytes") {
            return Err(format!(
                "record run did not echo the read bytes (live read broken):\n{}",
                format_output(&recorded)
            ));
        }
        if !session.exists() {
            return Err("record run did not write the session JSON".to_string());
        }

        // BOTH effects must be captured through invoke_effect — a
        // dropped MIR replay wrapper makes one (or both) vanish.
        let session_json = fs::read_to_string(&session).expect("read session");
        if !session_json.contains("\"Disk.readText\"") {
            return Err(format!(
                "session is missing the Disk.readText effect — the MIR replay \
                 wrapper was dropped on the read:\n{session_json}"
            ));
        }
        if !session_json.contains("\"Console.print\"") {
            return Err(format!(
                "session is missing the Console.print effect — the MIR replay \
                 wrapper was dropped on the print:\n{session_json}"
            ));
        }
        if !session_json.contains("READ:recorded-bytes") {
            return Err(format!(
                "session does not carry the woven read result in the Console.print \
                 arg — per-effect arg-json shape is wrong:\n{session_json}"
            ));
        }

        // (2) REPLAY: mutate the data file so a LIVE read would differ,
        // then replay. Replay must serve the recorded bytes (not re-read
        // the mutated file) and roundtrip without a position mismatch.
        fs::write(&data_path, "MUTATED-ON-DISK").expect("mutate data file");
        let replayed = Command::new(&bin)
            .env("AVER_REPLAY_REPLAY", &session)
            .output()
            .map_err(|e| format!("run replay binary: {e}"))?;
        if !replayed.status.success() {
            return Err(format!(
                "replay run failed — the recorded session did not roundtrip (a \
                 dropped or mis-ordered MIR invoke_effect reroute trips a position \
                 mismatch here):\n{}",
                format_output(&replayed)
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

// ─── Full tier (env-gated, #[ignore]) ───────────────────────────────────

/// Every single-file example with deterministic, build-and-run-able
/// behavior (Console-only or pure — no Time / Random / Http / Tcp /
/// Terminal, no interactive loop). Plain-parity tier.
const FULL_SINGLE_FILE: &[&str] = &[
    "examples/core/calculator.av",
    "examples/core/hello.av",
    "examples/core/lambda.av",
    "examples/core/lists.av",
    "examples/core/order_total.av",
    "examples/core/result_chain.av",
    "examples/core/result_pipeline.av",
    "examples/core/shapes.av",
    "examples/core/temperature.av",
    "examples/core/user_record.av",
    "examples/data/fibonacci.av",
    "examples/data/list_length_fold.av",
    "examples/data/map.av",
    "examples/data/quicksort.av",
    "examples/data/red_black_tree.av",
    "examples/data/rle.av",
    "examples/data/sum_acc.av",
];

/// Multi-module (`depends`) examples — (entry file, module root).
/// These exercise the cross-module path-mangling the Rust backend
/// emits (the `crate::aver_generated::<dep>::*` references). The games
/// are excluded: they're interactive Terminal loops, not batch
/// programs with deterministic stdout.
const FULL_MULTI_MODULE: &[(&str, &str)] = &[
    ("examples/modules/app.av", "examples"),
    ("examples/modules/pricing_app.av", "examples"),
];

#[test]
#[ignore = "full tier: minutes of build wall-time; set AVER_RUST_DIFF_FULL=1 and run with --ignored"]
fn full_plain_stdout_parity_with_vm() {
    if std::env::var("AVER_RUST_DIFF_FULL").is_err() {
        eprintln!(
            "skipping full tier — set AVER_RUST_DIFF_FULL=1 to run \
             (single-file + multi-module plain parity over the corpus)"
        );
        return;
    }

    let mut failures = Vec::new();
    let mut passed = 0usize;

    for relative in FULL_SINGLE_FILE {
        match assert_plain_parity(relative, None) {
            Ok(()) => passed += 1,
            Err(e) => failures.push(e),
        }
    }
    for (relative, root) in FULL_MULTI_MODULE {
        match assert_plain_parity(relative, Some(root)) {
            Ok(()) => passed += 1,
            Err(e) => failures.push(e),
        }
    }

    let total = FULL_SINGLE_FILE.len() + FULL_MULTI_MODULE.len();
    eprintln!("full_plain_stdout_parity_with_vm: {passed}/{total} passed");
    assert!(
        failures.is_empty(),
        "{} of {} full-tier examples failed plain parity:\n  - {}",
        failures.len(),
        total,
        failures.join("\n  - ")
    );
}

// ─── Mode (e): MIR-synthesized TCO (Wave 5) ──────────────────────────────
//
// Wave 5 synthesizes the self-TCO loop and the mutual-recursion
// trampoline from `MirExpr::TailCall` in the MIR walker, behind the
// `AVER_RUST_MIR_TCO=1` flag. TCO never byte-graduates (the loop /
// trampoline STRUCTURE has no HIR analogue to compare against), so it is
// verified BEHAVIORALLY here: build + RUN the MIR-synthesized binary and
// assert (1) stdout parity with the VM AND (2) the deep self-recursion
// case does NOT stack-overflow — which proves the emitted code is a
// genuine loop, not a recursive call that merely happens to compute the
// right answer at shallow depth.
//
// ## Revert-proof (what makes this a real net, not theater)
//
// Break the synthesized self-loop in
// `src/codegen/rust/from_mir.rs::emit_mir_self_tco_continue` by replacing
// the `continue;` line with a recursive self-call that returns instead of
// looping, e.g.:
//
// ```rust
// // lines.push("            continue;".to_string());
// lines.push(format!(
//     "            return {}({});",
//     "countUp",
//     arg_strs.join(", ")
// ));
// ```
//
// The shallow mutual case still passes (correct answer), but the DEEP
// self-recursion case (10M) overflows the stack → the binary aborts with
// a non-zero exit → `mir_tco_deep_self_and_mutual_recursion_behaves`
// fails on the "compiled binary exited non-zero" branch. Restoring
// `continue;` turns it green again. (Verified during Wave 5 development:
// the test goes RED with the loop broken, GREEN with it intact.)

/// Deep self-TCO (sum to 10M — overflows WITHOUT a real loop) + a
/// 2-cycle and a 3-cycle of mutual recursion, all in one program so a
/// single cargo build amortizes. Console-only, deterministic.
const MIR_TCO_PROBE: &str = r#"module TcoProbe
    intent =
        "Deep self-TCO that would stack-overflow without a real loop, plus"
        "mutual recursion (a 2-cycle and a 3-cycle). Probes the Wave-5 MIR"
        "TCO synthesis: the deep case proves the loop, the cycles the trampoline."
    effects [Console]

fn countUp(n: Int, acc: Int) -> Int
    ? "Tail-recursive sum from n down to 0 — deep enough to overflow without TCO."
    match n == 0
        true -> acc
        false -> countUp(n - 1, acc + n)

fn isEven(n: Int) -> Bool
    ? "True when n is even (mutual recursion with isOdd)."
    match n == 0
        true -> true
        false -> isOdd(n - 1)

fn isOdd(n: Int) -> Bool
    ? "True when n is odd (mutual recursion with isEven)."
    match n == 0
        true -> false
        false -> isEven(n - 1)

fn cycleA(n: Int) -> Int
    ? "Three-cycle member A."
    match n == 0
        true -> 100
        false -> cycleB(n - 1)

fn cycleB(n: Int) -> Int
    ? "Three-cycle member B."
    match n == 0
        true -> 200
        false -> cycleC(n - 1)

fn cycleC(n: Int) -> Int
    ? "Three-cycle member C."
    match n == 0
        true -> 300
        false -> cycleA(n - 1)

fn main() -> Unit
    ! [Console.print]
    total = countUp(10000000, 0)
    e = isEven(5000000)
    o = isOdd(5000000)
    c = cycleA(7)
    Console.print("total={total} even={e} odd={o} cycle={c}")
"#;

#[test]
fn mir_tco_deep_self_and_mutual_recursion_behaves() {
    let ws = temp_dir("mir_tco");
    let src = ws.join("tco_probe.av");
    fs::write(&src, MIR_TCO_PROBE).expect("write TCO probe source");

    // The VM oracle: a deep tail-recursive loop the VM runs via frame
    // reuse, so it produces the answer without overflowing.
    let vm_stdout = run_vm(&src, None).unwrap_or_else(|e| panic!("VM run failed: {e}"));

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let name = "mir_tco_probe";

    let result = (|| -> Result<(), String> {
        // Flag ON: force the Wave-5 MIR TCO synthesis onto the loop +
        // trampoline. (Production default leaves this OFF and uses the
        // proven HIR emitter; this test specifically exercises MIR.)
        compile_rust_env(
            &src,
            &project,
            name,
            None,
            &[],
            &[("AVER_RUST_MIR_TCO", "1")],
        )?;
        let bin = cargo_build(&project, name)?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("failed to run compiled binary: {e}"))?;
        if !out.status.success() {
            // A non-zero exit on the DEEP case is the stack overflow the
            // loop is supposed to prevent — the revert-proof failure mode.
            return Err(format!(
                "MIR-TCO binary exited non-zero — the deep self-recursion likely \
                 stack-overflowed (the synthesized loop is broken):\n{}",
                format_output(&out)
            ));
        }
        let rust_stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if rust_stdout != vm_stdout {
            return Err(format!(
                "MIR-TCO stdout mismatch\n--- VM ---\n{vm_stdout}\n--- Rust (MIR TCO) ---\n{rust_stdout}"
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

// ─── Mode (f): MIR-emitted IndependentProduct (`?!` / `!`) ────────────────
//
// The MIR walker now emits `MirExpr::IndependentProduct` — the Rust
// backend's truly-PARALLEL product (`std::thread::scope` + per-branch
// `spawn`, the cancel-flag machinery for `?!`, the bare tuple fold for
// `!`). The `?!` fns byte-graduate under the normal parity gate; the
// bare `!` fns that carry a literal negation in an argument diverge only
// on a pre-existing `Neg` fold (`(0i64 - 5i64)` vs `(-5i64)`, which is
// semantically equal), orthogonal to the product shape. Under
// `AVER_RUST_MIR_ONLY=1` ALL of them are forced onto the MIR path, so
// this probe proves the MIR walker OWNS the parallel emission and the
// built binary still produces VM-identical output.
//
// `independent_fanout.av` exercises `?!` (flatOk, processStep — recursive
// fan-out), bare `!` (flatFail, bareProduct), and the `Err`-propagation
// path, so a dropped cancel flag, a wrong tuple fold, or a dropped
// unwrap would change stdout here.

#[test]
fn mir_forced_independent_product_builds_and_matches_vm() {
    let relative = "examples/core/independent_fanout.av";
    let file = repo_root().join(relative);
    if !file.exists() {
        panic!("{relative}: corpus file missing");
    }

    let vm_stdout = run_vm(&file, None).unwrap_or_else(|e| panic!("VM run failed: {e}"));

    let ws = temp_dir("mir-ip");
    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let name = "mir_ip_fanout";
    let hatch = [("AVER_RUST_MIR_ONLY", "1")];

    let result = (|| -> Result<(), String> {
        // (0) PROVE the MIR path is exercised: the IndependentProduct fns
        // must graduate onto MIR under the hatch. Without this guard a
        // silent HIR fallback would let the probe pass for the wrong
        // reason.
        let grad = graduated_count(&file, None, &[], &hatch)?;
        if grad == 0 {
            return Err(
                "no fn graduated onto the MIR path under AVER_RUST_MIR_ONLY=1 — the \
                 IndependentProduct emit is not being exercised by the MIR walker"
                    .to_string(),
            );
        }

        // Force the MIR body onto production for every renderable fn, so
        // the parallel IndependentProduct shape is MIR-emitted (not the
        // byte-equal HIR fallback), then build + run.
        compile_rust_env(&file, &project, name, None, &[], &hatch)?;

        // Structural tripwire: the emitted Rust must carry the MIR-
        // emitted parallel product machinery (the cancel-flag branch
        // runner for `?!` and the `thread::scope` fan-out).
        let emitted = fs::read_to_string(
            project
                .join("src")
                .join("aver_generated")
                .join("entry")
                .join("mod.rs"),
        )
        .map_err(|e| format!("read emitted module: {e}"))?;
        if !emitted.contains("run_cancelable_branch") || !emitted.contains("std::thread::scope") {
            return Err(format!(
                "emitted Rust is missing the parallel IndependentProduct machinery \
                 (run_cancelable_branch / thread::scope) — the MIR product emit was \
                 dropped:\n{emitted}"
            ));
        }

        let bin = cargo_build(&project, name)?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("failed to run compiled binary: {e}"))?;
        if !out.status.success() {
            return Err(format!(
                "MIR IndependentProduct binary exited non-zero:\n{}",
                format_output(&out)
            ));
        }
        let rust_stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if rust_stdout != vm_stdout {
            return Err(format!(
                "MIR IndependentProduct stdout mismatch\n--- VM ---\n{vm_stdout}\n--- Rust (MIR) ---\n{rust_stdout}"
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

// ─── Mode (g): MIR-emitted first-class fn values ──────────────────────────
//
// W6/Stage-0 (the final construct gap): the MIR walker now emits
// `MirExpr::FnValue` (a named fn used as a value — `applyIt(dbl, 21)`
// passes `dbl`) and `MirCallee::LocalSlot` (calling a fn-typed param —
// `f(v)` inside `applyIt`). Post-#379 a fn value can only enter through a
// `Fn(..)` param, so the emitted Rust is a plain fn-pointer
// (`fn(i64)->i64`) — no closure / `dyn Fn`, no monomorphization. Rust CAN
// execute these (unlike wasm-gc, which traps), so this is an EMIT path,
// not a trap-stub, and must produce VM-identical output.
//
// The corpus is thin on higher-order (only `pairSpec` in
// `examples/formal/oracle_independent_products.av`, verify-only), so this
// is an inline RUNTIME probe: `dbl` / `inc` are passed into `applyIt`
// (FnValue in arg position), `applyIt` calls them through its slot
// (LocalSlot). Under `AVER_RUST_MIR_ONLY=1` the FnValue / LocalSlot fns
// are FORCED onto the MIR path, so the built binary's `a=42 b=42` proves
// the MIR walker OWNS the first-class-fn emission (a dropped FnValue arg
// or a mis-emitted slot call would change stdout or fail to build).
const MIR_HIGHER_ORDER_PROBE: &str = r#"module HigherOrderProbe
    intent =
        "Probes first-class fn values: a fn passed as a Fn(..) param value"
        "(MirExpr::FnValue) and called through that slot (MirCallee::LocalSlot)."
    effects [Console]

fn dbl(x: Int) -> Int
    ? "Double a number."
    x * 2

fn inc(x: Int) -> Int
    ? "Increment a number."
    x + 1

fn applyIt(f: Fn(Int) -> Int, v: Int) -> Int
    ? "Apply a first-class fn value to v (calls through the slot — LocalSlot)."
    f(v)

fn runDouble(v: Int) -> Int
    ? "Pass dbl as a fn value into applyIt (FnValue in arg position)."
    applyIt(dbl, v)

fn runInc(v: Int) -> Int
    ? "Pass inc as a fn value into applyIt (FnValue in arg position)."
    applyIt(inc, v)

fn main() -> Unit
    ! [Console.print]
    a = runDouble(21)
    b = runInc(41)
    Console.print("a={a} b={b}")
"#;

#[test]
fn mir_first_class_fn_value_builds_and_matches_vm() {
    let ws = temp_dir("mir_ho");
    let src = ws.join("higher_order_probe.av");
    fs::write(&src, MIR_HIGHER_ORDER_PROBE).expect("write higher-order probe source");

    let vm_stdout = run_vm(&src, None).unwrap_or_else(|e| panic!("VM run failed: {e}"));

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let name = "mir_ho_probe";
    let hatch = [("AVER_RUST_MIR_ONLY", "1")];

    let result = (|| -> Result<(), String> {
        // (0) PROVE the MIR path is exercised: the FnValue / LocalSlot fns
        // (`applyIt`, `runDouble`, `runInc`) must graduate onto MIR under
        // the hatch. Without this guard a silent HIR fallback would let
        // the probe pass for the wrong reason (the FnValue / LocalSlot
        // emit would never run). `dbl` / `inc` / the three higher-order
        // fns graduate; only `main` (a `Console.print` interpolation,
        // `Call(Builtin)`) stays on HIR — so ≥ 5 must graduate.
        let grad = graduated_count(&src, None, &[], &hatch)?;
        if grad < 5 {
            return Err(format!(
                "expected ≥ 5 fns to graduate onto MIR under AVER_RUST_MIR_ONLY=1 \
                 (dbl, inc, applyIt, runDouble, runInc) — got {grad}. The FnValue / \
                 LocalSlot emit is not being exercised by the MIR walker."
            ));
        }

        // Force the MIR body onto production for every renderable fn, so
        // the FnValue arg + LocalSlot call are MIR-emitted (not the
        // byte-equal HIR fallback), then build + run.
        compile_rust_env(&src, &project, name, None, &[], &hatch)?;

        // Structural tripwire: the emitted Rust must carry the fn-pointer
        // param (`f: fn(i64) -> i64`), the FnValue passed by bare name
        // (`applyIt(dbl, v)`), and the call-through-slot (`f(v)`). A
        // dropped FnValue or a mis-emitted slot call would erase these.
        let emitted = fs::read_to_string(
            project
                .join("src")
                .join("aver_generated")
                .join("entry")
                .join("mod.rs"),
        )
        .map_err(|e| format!("read emitted module: {e}"))?;
        if !emitted.contains("f: fn(i64) -> i64") {
            return Err(format!(
                "emitted Rust is missing the fn-pointer param `f: fn(i64) -> i64` — \
                 the LocalSlot param lowering was dropped:\n{emitted}"
            ));
        }
        if !emitted.contains("applyIt(dbl, v)") || !emitted.contains("applyIt(inc, v)") {
            return Err(format!(
                "emitted Rust is missing the FnValue arg `applyIt(dbl, v)` / \
                 `applyIt(inc, v)` — the FnValue emit was dropped:\n{emitted}"
            ));
        }
        if !emitted.contains("f(v)") {
            return Err(format!(
                "emitted Rust is missing the call-through-slot `f(v)` — the \
                 LocalSlot call emit was dropped:\n{emitted}"
            ));
        }

        let bin = cargo_build(&project, name)?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("failed to run compiled binary: {e}"))?;
        if !out.status.success() {
            return Err(format!(
                "MIR first-class-fn binary exited non-zero:\n{}",
                format_output(&out)
            ));
        }
        let rust_stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if rust_stdout != vm_stdout {
            return Err(format!(
                "MIR first-class-fn stdout mismatch\n--- VM ---\n{vm_stdout}\n--- Rust (MIR) ---\n{rust_stdout}"
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

// ═══════════════════════════════════════════════════════════════════════
// W6 / Stage-1: the FORCED-MIR behavioral net
// ═══════════════════════════════════════════════════════════════════════
//
// Stage 0 gave every reachable MIR construct a Rust walker arm behind
// per-construct flags + a per-fn byte-parity gate, so production codegen
// is UNCHANGED (flags default-off). Stage 1 (this section) turns the W6
// audit's ad-hoc "forced-MIR builds + runs the corpus + self-host
// correctly" into a STANDING, REPEATABLE gate: with ALL FOUR MIR flags
// set, the MIR walker OWNS ~everything, and we assert the built binaries
// still match the VM.
//
// All four flags are set INTERNALLY by these tests (never in production):
//
//   AVER_RUST_MIR_ONLY=1    — force the MIR body onto production even when
//                             it diverges from HIR (disables the byte-exact
//                             fallback, so the MIR walker provably owns the
//                             emit instead of hiding behind HIR parity).
//   AVER_RUST_MIR_TCO=1     — synthesize the self-TCO loop + mutual-rec
//                             trampoline from MirExpr::TailCall.
//   AVER_RUST_MIR_VERIFY=1  — lower verify-case exprs through MIR.
//   AVER_RUST_MIR_MAIN=1    — render main + top-level-statement values
//                             through MIR.
//
// Two discipline guards make this a real net, not theater:
//
//  - **graduated > 0** (the W3b discipline): every forced-MIR parity
//    assert first checks `graduated_count(... all-four-flags) > 0`, so a
//    test cannot pass via a silent HIR fallback (which would mean the MIR
//    path was never exercised). Pure / Console-only corpus fns graduate;
//    only the `Call(Builtin)` interpolation in `main` stays on HIR.
//  - **per-test isolated target dir** (the W6 audit flakiness fix): the
//    generated-project `cargo build` runs against a UNIQUE target dir per
//    example, so concurrent `--offline` builds never corrupt each other's
//    `.rmeta` / proc-macro outputs.

/// All four MIR flags — set by the forced-MIR net so the MIR walker owns
/// the entire emit (body, TCO, verify, main + top-stmt) for every
/// renderable fn.
const FORCED_MIR: &[(&str, &str)] = &[
    ("AVER_RUST_MIR_ONLY", "1"),
    ("AVER_RUST_MIR_TCO", "1"),
    ("AVER_RUST_MIR_VERIFY", "1"),
    ("AVER_RUST_MIR_MAIN", "1"),
];

/// Per-example isolated `cargo build` target dir. The forced-MIR tier
/// uses one of these per example so concurrent `--offline` builds never
/// race on a shared target tree (the `.rmeta` / proc-macro corruption hit
/// during the W6 audit). Lives next to the temp project so the temp-dir
/// cleanup reclaims it.
fn isolated_target_dir(ws: &Path) -> PathBuf {
    ws.join("cargo-target")
}

/// Compile + build + RUN an example under ALL FOUR MIR flags, asserting
/// stdout parity with the VM. Applies the graduated > 0 guard (the fn
/// must actually take the MIR path under the flags) + a per-example
/// isolated target dir.
fn assert_forced_mir_parity(relative: &str, module_root: Option<&str>) -> Result<(), String> {
    let file = repo_root().join(relative);
    if !file.exists() {
        return Err(format!("{relative}: corpus file missing"));
    }
    let root = module_root.map(|r| repo_root().join(r));
    let vm_stdout = run_vm(&file, root.as_deref())?;

    let ws = temp_dir(&format!("fmir-{}", sanitise(relative)));
    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let name = format!("fm_{}", sanitise(relative));

    let result = (|| -> Result<(), String> {
        // Guard: SOMETHING must graduate onto MIR under the four flags,
        // else the parity assert below would be validating the HIR
        // fallback, not the MIR walker. (`main`'s Console.print
        // interpolation stays on HIR, so any corpus example with a
        // helper fn or a pure body graduates >= 1.)
        let grad = graduated_count(&file, root.as_deref(), &[], FORCED_MIR)?;
        if grad == 0 {
            return Err(format!(
                "{relative}: no fn graduated onto the MIR path under the four MIR \
                 flags — forced-MIR parity here would only be exercising the HIR \
                 fallback, not the MIR walker"
            ));
        }

        compile_rust_env(&file, &project, &name, root.as_deref(), &[], FORCED_MIR)?;
        let bin = cargo_build_in(&project, &name, &isolated_target_dir(&ws))?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("failed to run compiled binary: {e}"))?;
        if !out.status.success() {
            return Err(format!(
                "{relative}: forced-MIR binary exited non-zero:\n{}",
                format_output(&out)
            ));
        }
        let rust_stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if rust_stdout != vm_stdout {
            return Err(format!(
                "{relative}: forced-MIR stdout mismatch\n--- VM ---\n{vm_stdout}\n--- Rust (forced MIR) ---\n{rust_stdout}"
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result
}

// ─── BranchPath exclusion (honest corpus denominator) ────────────────────
//
// Some examples do NOT build in Rust on EITHER walker — a pre-existing
// backend gap, NOT a MIR regression: the Oracle-only `BranchPath` type
// (and the Terminal-only `Terminal.Size` type) are referenced by the
// emitted Rust but never emitted as a Rust type, so rustc rejects with
// E0425 ("cannot find type"). Every `examples/formal/*` program reaches
// the Oracle / trace API, and `services/redis.av` reaches it transitively
// via the Tcp shell, so the whole set is excluded from the run-parity
// corpus. `branch_path_examples_fail_identically_on_both_walkers` proves
// the failure is identical HIR vs forced-MIR (so the exclusion measures a
// real buildable denominator, not a broken baseline). Fixing BranchPath
// is explicitly OUT OF SCOPE for Stage 1.

/// (relative path, optional module root) of the examples that fail the
/// Rust `cargo build` on BOTH walkers because of the BranchPath /
/// Terminal.Size emit gap. Documented + excluded from the run-parity
/// corpus; verified to fail identically by the test below.
// NOTE: only the examples EMPIRICALLY CONFIRMED to fail the `cargo build`
// on BOTH walkers belong here. Several other `formal/*` programs
// (file_store_pure_core, spec_laws, law_auto, trust_check) build cleanly
// in Rust — they are PURE / proof-shaped and never reach the Oracle /
// trace runtime API, so they are NOT exclusions and stay buildable. The
// test below would flag a stale entry that started building on both
// walkers ("move it into the run-parity corpus").
const BRANCH_PATH_EXCLUSIONS: &[(&str, Option<&str>)] = &[
    ("examples/formal/oracle_trace.av", Some("examples")),
    (
        "examples/formal/oracle_independent_products.av",
        Some("examples"),
    ),
    ("examples/formal/hostile_order_axis.av", Some("examples")),
    ("examples/formal/clock_as_data.av", Some("examples")),
    ("examples/formal/randomness_paradox.av", Some("examples")),
    (
        "examples/formal/terminal_size_snapshot.av",
        Some("examples"),
    ),
    ("examples/formal/file_store_shell.av", Some("examples")),
    ("examples/services/redis.av", Some("examples")),
];

/// `cargo build` an example on a given walker, returning Ok(()) on a
/// clean build or Err(stderr) on failure. Used by the exclusion proof to
/// compare the HIR vs forced-MIR failure shapes.
fn try_build_walker(
    relative: &str,
    module_root: Option<&str>,
    env: &[(&str, &str)],
    ws: &Path,
    tag: &str,
) -> Result<Result<(), String>, String> {
    let file = repo_root().join(relative);
    if !file.exists() {
        return Err(format!("{relative}: corpus file missing"));
    }
    let root = module_root.map(|r| repo_root().join(r));
    let project = ws.join(format!("project-{tag}"));
    fs::create_dir_all(&project).expect("create project dir");
    let name = format!("bp_{}_{tag}", sanitise(relative));

    // The compile (Rust source emit) may itself fail under the flags; if
    // so that's the failure to compare. Capture it as an Err.
    if let Err(e) = compile_rust_env(&file, &project, &name, root.as_deref(), &[], env) {
        return Ok(Err(format!("compile: {e}")));
    }
    match cargo_build_in(&project, &name, &ws.join(format!("target-{tag}"))) {
        Ok(_) => Ok(Ok(())),
        Err(e) => Ok(Err(e)),
    }
}

#[test]
#[ignore = "full tier: cargo build wall-time; set AVER_RUST_DIFF_FULL=1 and run with --ignored"]
fn branch_path_examples_fail_identically_on_both_walkers() {
    if std::env::var("AVER_RUST_DIFF_FULL").is_err() {
        eprintln!(
            "skipping BranchPath-exclusion proof — set AVER_RUST_DIFF_FULL=1 \
             (proves the excluded examples fail identically HIR vs forced-MIR)"
        );
        return;
    }

    let mut failures = Vec::new();
    let mut confirmed = 0usize;

    for (relative, root) in BRANCH_PATH_EXCLUSIONS {
        let ws = temp_dir(&format!("bp-{}", sanitise(relative)));
        let hir = try_build_walker(relative, *root, &[], &ws, "hir");
        let mir = try_build_walker(relative, *root, FORCED_MIR, &ws, "mir");
        let _ = fs::remove_dir_all(&ws);

        match (hir, mir) {
            (Ok(Err(hir_err)), Ok(Err(mir_err))) => {
                // Both walkers must fail. The exact rustc message text can
                // differ slightly between walkers (e.g. the first undef
                // name), but the FAILURE CLASS — an undefined-symbol
                // E0425 from the BranchPath / Terminal.Size emit gap —
                // must be present on both. That's what proves it's a
                // pre-existing backend gap, not a MIR regression.
                let hir_e0425 = hir_err.contains("E0425") || hir_err.contains("cannot find");
                let mir_e0425 = mir_err.contains("E0425") || mir_err.contains("cannot find");
                if hir_e0425 && mir_e0425 {
                    confirmed += 1;
                } else {
                    failures.push(format!(
                        "{relative}: both walkers failed but NOT both with the expected \
                         undefined-symbol (BranchPath / Terminal.Size) gap.\n  HIR err:\n{hir_err}\n  MIR err:\n{mir_err}"
                    ));
                }
            }
            (Ok(Ok(())), Ok(Ok(()))) => failures.push(format!(
                "{relative}: BUILT on both walkers — it is no longer a BranchPath \
                 exclusion; move it into the forced-MIR run-parity corpus"
            )),
            (hir_res, mir_res) => failures.push(format!(
                "{relative}: HIR and forced-MIR DISAGREE on buildability — this would \
                 be a MIR regression (or a fix), not a stable pre-existing gap.\n  HIR: {hir_res:?}\n  MIR: {mir_res:?}"
            )),
        }
    }

    eprintln!(
        "branch_path_examples_fail_identically_on_both_walkers: {confirmed}/{} confirmed \
         identical-failure (BranchPath / Terminal.Size gap)",
        BRANCH_PATH_EXCLUSIONS.len()
    );
    assert!(
        failures.is_empty(),
        "{} of {} BranchPath exclusions did not fail identically on both walkers:\n  - {}",
        failures.len(),
        BRANCH_PATH_EXCLUSIONS.len(),
        failures.join("\n  - ")
    );
}

// ─── Forced-MIR full-corpus run-parity tier (env-gated) ──────────────────

/// Single-file examples that build + run deterministically under
/// forced-MIR (Console-only or pure — no live Time / Random / Http / Tcp /
/// Terminal in the run path, no interactive loop). Empirically verified
/// to build + run with VM parity under all four MIR flags. This is the
/// honest buildable denominator: the BranchPath set is excluded above.
const FORCED_MIR_SINGLE_FILE: &[&str] = &[
    "examples/core/calculator.av",
    "examples/core/hello.av",
    "examples/core/lambda.av",
    "examples/core/lists.av",
    "examples/core/order_total.av",
    "examples/core/result_chain.av",
    "examples/core/result_pipeline.av",
    "examples/core/shapes.av",
    "examples/core/temperature.av",
    "examples/core/user_record.av",
    "examples/core/effects_explicit.av",
    // NOTE: examples/core/grok_s_language.av is deliberately EXCLUDED — it
    // is an interactive `Console.readLine` REPL loop, not a batch program;
    // with stdin closed it spins on EOF and the VM oracle itself never
    // terminates. Interactive examples are not run-parity candidates (same
    // reason the games are excluded).
    "examples/data/fibonacci.av",
    "examples/data/list_length_fold.av",
    "examples/data/map.av",
    "examples/data/quicksort.av",
    "examples/data/red_black_tree.av",
    "examples/data/rle.av",
    "examples/data/sum_acc.av",
    "examples/data/json.av",
    // Pure / proof-shaped formal examples that DO build + run in Rust
    // (they never reach the Oracle / trace runtime API, so no BranchPath
    // gap). Empirically VM-parity under forced-MIR (graduated 2/4/6/2).
    "examples/formal/file_store_pure_core.av",
    "examples/formal/spec_laws.av",
    "examples/formal/law_auto.av",
    "examples/formal/trust_check.av",
];

/// Multi-module (`depends`) examples — (entry file, module root). These
/// exercise the cross-module path-mangling the Rust backend emits, with
/// `main` + top-level-statement values routed through MIR
/// (AVER_RUST_MIR_MAIN). The games are excluded: every one is an
/// interactive Terminal loop (Terminal is host territory + no
/// deterministic batch stdout), so they are neither buildable in Rust nor
/// run-parity candidates.
const FORCED_MIR_MULTI_MODULE: &[(&str, &str)] = &[
    ("examples/modules/app.av", "examples"),
    ("examples/modules/app_dot.av", "examples"),
    ("examples/modules/pricing_app.av", "examples"),
];

#[test]
#[ignore = "full tier: minutes of build wall-time; set AVER_RUST_DIFF_FULL=1 and run with --ignored"]
fn forced_mir_full_corpus_parity_with_vm() {
    if std::env::var("AVER_RUST_DIFF_FULL").is_err() {
        eprintln!(
            "skipping forced-MIR full-corpus tier — set AVER_RUST_DIFF_FULL=1 to run \
             (all four MIR flags; single-file + multi-module run-parity over the \
             buildable corpus, with the graduated>0 guard)"
        );
        return;
    }

    let mut failures = Vec::new();
    let mut passed = 0usize;

    for relative in FORCED_MIR_SINGLE_FILE {
        match assert_forced_mir_parity(relative, None) {
            Ok(()) => passed += 1,
            Err(e) => failures.push(e),
        }
    }
    for (relative, root) in FORCED_MIR_MULTI_MODULE {
        match assert_forced_mir_parity(relative, Some(root)) {
            Ok(()) => passed += 1,
            Err(e) => failures.push(e),
        }
    }

    let total = FORCED_MIR_SINGLE_FILE.len() + FORCED_MIR_MULTI_MODULE.len();
    eprintln!(
        "forced_mir_full_corpus_parity_with_vm: {passed}/{total} passed (all four MIR flags)"
    );
    assert!(
        failures.is_empty(),
        "{} of {} forced-MIR corpus examples failed parity:\n  - {}",
        failures.len(),
        total,
        failures.join("\n  - ")
    );
}

// ─── Forced-MIR effect-mode probes (Time / Random / Tcp) ─────────────────
//
// Wave 3b lets the MIR walker emit effectful builtins (the policy /
// replay / bare-framing wrappers). Disk is covered by the deny-policy +
// record/replay probes above. These probes extend that coverage to
// Time / Random (deterministic via record->replay: the live record
// captures the nondeterministic value, replay serves it back) and to a
// Tcp shape (a real loopback listener for the record, then replay serves
// it back so the assertion is listener-independent). Each rides the
// effect on a HELPER fn so it graduates onto MIR (graduated > 0 guard) and
// asserts the MIR-emitted `aver_replay::invoke_effect` reroute captures
// the effect with the right per-effect arg-json shape.

/// Time + Random through helper fns (so they graduate). `Random.int`
/// rides `rollDie`, `Time.unixMs` rides `stamp`.
const MIR_TIME_RANDOM_PROBE: &str = r#"module MirTimeRandomProbe
    intent =
        "Draws a random int and reads a unix-ms timestamp through helper fns,"
        "echoes both. record captures the nondeterministic effects; replay"
        "serves them back. Probes the MIR-emitted Time / Random replay wrap."
    effects [Console, Random, Time]

fn rollDie(lo: Int, hi: Int) -> Int
    ? "Draws a random int in the inclusive range."
    ! [Random.int]
    Random.int(lo, hi)

fn stamp() -> Int
    ? "Reads the current unix-ms timestamp."
    ! [Time.unixMs]
    Time.unixMs()

fn main() -> Unit
    ! [Console.print, Random.int, Time.unixMs]
    r = rollDie(1, 100)
    t = stamp()
    Console.print("r={r} t={t}")
"#;

#[test]
#[ignore = "full tier: cargo build wall-time; set AVER_RUST_DIFF_FULL=1 and run with --ignored"]
fn mir_forced_time_random_record_replay_roundtrips() {
    if std::env::var("AVER_RUST_DIFF_FULL").is_err() {
        eprintln!("skipping MIR-forced Time/Random probe — set AVER_RUST_DIFF_FULL=1");
        return;
    }

    let ws = temp_dir("mir-timerand");
    let src = ws.join("time_random_probe.av");
    fs::write(&src, MIR_TIME_RANDOM_PROBE).expect("write probe source");

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let name = "mir_time_random_probe";
    let hatch = [("AVER_RUST_MIR_ONLY", "1")];

    let result = (|| -> Result<(), String> {
        // (0) PROVE the MIR path is exercised: rollDie + stamp must
        // graduate onto MIR under the hatch.
        let grad = graduated_count(&src, None, &["--with-replay"], &hatch)?;
        if grad == 0 {
            return Err(
                "no fn graduated onto the MIR path under AVER_RUST_MIR_ONLY=1 — the \
                 Time / Random replay reroute is not being exercised"
                    .to_string(),
            );
        }

        compile_rust_env(&src, &project, name, None, &["--with-replay"], &hatch)?;

        // Structural tripwire: the emitted Rust must carry the MIR-emitted
        // invoke_effect reroute for the effectful helpers.
        let emitted = fs::read_to_string(
            project
                .join("src")
                .join("aver_generated")
                .join("entry")
                .join("mod.rs"),
        )
        .map_err(|e| format!("read emitted module: {e}"))?;
        if !emitted.contains("aver_replay::invoke_effect") {
            return Err(format!(
                "emitted Rust is missing the `aver_replay::invoke_effect` reroute — \
                 the MIR effectful replay wrap was dropped:\n{emitted}"
            ));
        }

        let bin = cargo_build_in(&project, name, &isolated_target_dir(&ws))?;

        // (1) RECORD: run live (nondeterministic), capturing into a session.
        let session = ws.join("session.json");
        let recorded = Command::new(&bin)
            .env("AVER_REPLAY_RECORD", &session)
            .output()
            .map_err(|e| format!("run record binary: {e}"))?;
        if !recorded.status.success() {
            return Err(format!("record run failed:\n{}", format_output(&recorded)));
        }
        if !session.exists() {
            return Err("record run did not write the session JSON".to_string());
        }
        // The recorded run prints the live, nondeterministic `r=N t=M`
        // line — capture it so we can prove replay serves THOSE exact
        // values back (not a fresh roll / clock read).
        let recorded_stdout = String::from_utf8_lossy(&recorded.stdout).trim().to_string();
        let recorded_line = recorded_stdout
            .lines()
            .find(|l| l.starts_with("r="))
            .ok_or_else(|| format!("record run had no `r=` line:\n{recorded_stdout}"))?
            .to_string();

        // BOTH nondeterministic effects must be captured through
        // invoke_effect — a dropped MIR replay wrapper makes one vanish.
        let session_json = fs::read_to_string(&session).expect("read session");
        for effect in ["\"Random.int\"", "\"Time.unixMs\"", "\"Console.print\""] {
            if !session_json.contains(effect) {
                return Err(format!(
                    "session is missing the {effect} effect — the MIR replay wrapper \
                     was dropped on it:\n{session_json}"
                ));
            }
        }
        // The recorded Random / Time values flowed through into the
        // interpolated Console.print arg — the session must carry that
        // exact `r=N t=M` string (per-effect arg-json shape). This is the
        // determinism evidence: the captured value is pinned in the
        // session, so replay has the recorded draw to serve back rather
        // than re-rolling.
        if !session_json.contains(&recorded_line) {
            return Err(format!(
                "session does not carry the recorded `{recorded_line}` in the \
                 Console.print arg — the woven Random/Time values were not captured \
                 (per-effect arg-json shape is wrong):\n{session_json}"
            ));
        }

        // (2) REPLAY: roundtrip the recorded session. Replay serves the
        // recorded Random / Time draws back (NOT a fresh roll / clock
        // read); a dropped or mis-ordered MIR invoke_effect reroute trips
        // a position mismatch and the run fails here.
        let replayed = Command::new(&bin)
            .env("AVER_REPLAY_REPLAY", &session)
            .output()
            .map_err(|e| format!("run replay binary: {e}"))?;
        if !replayed.status.success() {
            return Err(format!(
                "replay run failed — the recorded Time/Random session did not \
                 roundtrip (a dropped / mis-ordered MIR invoke_effect reroute trips a \
                 position mismatch here):\n{}",
                format_output(&replayed)
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

/// A Tcp.send shape through a helper fn (so it graduates). `__PORT__` is
/// substituted with a real loopback listener port at test time.
const MIR_TCP_PROBE: &str = r#"module MirTcpProbe
    intent =
        "Sends a line over TCP through a helper fn and echoes the reply. The"
        "helper rides Tcp.send so it graduates onto the MIR path. record"
        "captures the exchange; replay serves it back."
    effects [Console, Tcp]

fn ask(host: String, port: Int, msg: String) -> Result<String, String>
    ? "Open a TCP connection, write msg, read the reply, close."
    ! [Tcp.send]
    Tcp.send(host, port, msg)

fn main() -> Unit
    ! [Console.print, Tcp.send]
    match ask("127.0.0.1", __PORT__, "ping")
        Result.Ok(r) -> Console.print("got:{r}")
        Result.Err(e) -> Console.print("err:{e}")
"#;

/// Spawn a single-threaded loopback TCP server that replies `pong\n` to
/// every connection. Returns the bound port + a join handle that exits
/// once `stop` is set. No external process / Python — pure std, so it is
/// deterministic and dependency-free in CI.
fn spawn_pong_server(
    stop: std::sync::Arc<std::sync::atomic::AtomicBool>,
) -> (u16, std::thread::JoinHandle<()>) {
    use std::io::{Read, Write};
    use std::net::TcpListener;
    let listener = TcpListener::bind("127.0.0.1:0").expect("bind loopback listener");
    let port = listener.local_addr().expect("listener addr").port();
    listener
        .set_nonblocking(true)
        .expect("set listener nonblocking");
    let handle = std::thread::spawn(move || {
        while !stop.load(Ordering::Relaxed) {
            match listener.accept() {
                Ok((mut conn, _)) => {
                    // The accepted stream can inherit the listener's
                    // non-blocking mode on some platforms — force it back
                    // to BLOCKING so the read below actually waits for the
                    // client's bytes instead of returning WouldBlock and
                    // racing the connection closed (the "connection reset
                    // by peer" the first cut hit).
                    let _ = conn.set_nonblocking(false);
                    let _ = conn.set_read_timeout(Some(std::time::Duration::from_secs(2)));
                    // `Tcp.send` writes one request line then half-closes
                    // its write side; read until EOF / the request line so
                    // we don't reply before the request lands.
                    let mut buf = [0u8; 256];
                    let _ = conn.read(&mut buf);
                    let _ = conn.write_all(b"pong\n");
                    let _ = conn.flush();
                    // Give the client time to read the reply before the
                    // stream drops (drop closes the socket).
                    let _ = conn.shutdown(std::net::Shutdown::Write);
                    let mut drain = [0u8; 16];
                    let _ = conn.read(&mut drain);
                }
                Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                    std::thread::sleep(std::time::Duration::from_millis(5));
                }
                Err(_) => break,
            }
        }
    });
    (port, handle)
}

#[test]
#[ignore = "full tier: cargo build wall-time; set AVER_RUST_DIFF_FULL=1 and run with --ignored"]
fn mir_forced_tcp_send_record_replay_roundtrips() {
    if std::env::var("AVER_RUST_DIFF_FULL").is_err() {
        eprintln!("skipping MIR-forced Tcp probe — set AVER_RUST_DIFF_FULL=1");
        return;
    }

    let stop = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
    let (port, server) = spawn_pong_server(stop.clone());

    let ws = temp_dir("mir-tcp");
    let src = ws.join("tcp_probe.av");
    fs::write(&src, MIR_TCP_PROBE.replace("__PORT__", &port.to_string()))
        .expect("write probe source");

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let name = "mir_tcp_probe";
    let hatch = [("AVER_RUST_MIR_ONLY", "1")];

    let result = (|| -> Result<(), String> {
        // (0) PROVE the MIR path is exercised: `ask` must graduate.
        let grad = graduated_count(&src, None, &["--with-replay"], &hatch)?;
        if grad == 0 {
            return Err(
                "no fn graduated onto the MIR path under AVER_RUST_MIR_ONLY=1 — the \
                 Tcp replay reroute is not being exercised"
                    .to_string(),
            );
        }

        compile_rust_env(&src, &project, name, None, &["--with-replay"], &hatch)?;

        let emitted = fs::read_to_string(
            project
                .join("src")
                .join("aver_generated")
                .join("entry")
                .join("mod.rs"),
        )
        .map_err(|e| format!("read emitted module: {e}"))?;
        if !emitted.contains("aver_replay::invoke_effect") {
            return Err(format!(
                "emitted Rust is missing the `aver_replay::invoke_effect` reroute — \
                 the MIR Tcp replay wrap was dropped:\n{emitted}"
            ));
        }

        let bin = cargo_build_in(&project, name, &isolated_target_dir(&ws))?;

        // (1) RECORD: live against the loopback listener.
        let session = ws.join("session.json");
        let recorded = Command::new(&bin)
            .env("AVER_REPLAY_RECORD", &session)
            .output()
            .map_err(|e| format!("run record binary: {e}"))?;
        if !recorded.status.success() {
            return Err(format!("record run failed:\n{}", format_output(&recorded)));
        }
        if !String::from_utf8_lossy(&recorded.stdout).contains("got:pong") {
            return Err(format!(
                "record run did not echo the listener reply (live Tcp.send broken):\n{}",
                format_output(&recorded)
            ));
        }
        if !session.exists() {
            return Err("record run did not write the session JSON".to_string());
        }
        let session_json = fs::read_to_string(&session).expect("read session");
        for effect in ["\"Tcp.send\"", "\"Console.print\""] {
            if !session_json.contains(effect) {
                return Err(format!(
                    "session is missing the {effect} effect — the MIR replay wrapper \
                     was dropped on it:\n{session_json}"
                ));
            }
        }
        if !session_json.contains("pong") {
            return Err(format!(
                "session does not carry the recorded `pong` reply — the per-effect \
                 Tcp arg-json shape is wrong:\n{session_json}"
            ));
        }

        // (2) REPLAY: stop the listener first so a LIVE Tcp.send would
        // FAIL with a connection error — replay must instead serve the
        // recorded reply from the session (listener-independent) and
        // roundtrip without a position mismatch. A dropped MIR Tcp replay
        // reroute would re-attempt a live connection to the now-dead
        // listener and the run would fail here.
        stop.store(true, Ordering::Relaxed);
        let replayed = Command::new(&bin)
            .env("AVER_REPLAY_REPLAY", &session)
            .output()
            .map_err(|e| format!("run replay binary: {e}"))?;
        if !replayed.status.success() {
            return Err(format!(
                "replay run failed — the recorded Tcp session did not roundtrip (the \
                 MIR Tcp replay reroute likely re-tried a live connection to the \
                 stopped listener instead of serving the recorded reply):\n{}",
                format_output(&replayed)
            ));
        }
        Ok(())
    })();

    stop.store(true, Ordering::Relaxed);
    let _ = server.join();
    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

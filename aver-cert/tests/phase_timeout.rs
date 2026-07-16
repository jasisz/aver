//! The verifier must never hang. Every Lean toolchain step on the
//! verify/check path runs under a wall-clock limit; when a step exceeds it,
//! the step's process tree is stopped and the certificate fails closed with
//! a nonzero exit. `AVER_CERT_PHASE_TIMEOUT_SECS` overrides the limit and is
//! itself validated fail-closed.

#![cfg(all(unix, feature = "verify"))]

use sha2::{Digest, Sha256};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

struct Fixture {
    root: PathBuf,
    artifact: PathBuf,
    cert_dir: PathBuf,
}

impl Drop for Fixture {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.root);
    }
}

/// A tiny valid wasm artifact plus a certificate manifest that passes every
/// Rust-side gate, so the check reaches the first Lean toolchain step. Any
/// real `lake build` of the embedded wall takes far longer than the tiny
/// timeout the tests set, so no hostile certificate is needed.
fn minimal_fixture() -> Fixture {
    let nanos = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|duration| duration.as_nanos())
        .unwrap_or(0);
    let root =
        std::env::temp_dir().join(format!("aver-cert-timeout-{}-{nanos}", std::process::id()));
    std::fs::create_dir(&root).expect("fixture root");

    let artifact = root.join("app.wasm");
    let bytes = wat::parse_str("(module)").expect("fixture wasm");
    std::fs::write(&artifact, &bytes).expect("fixture artifact");
    let hash = format!("{:x}", Sha256::digest(&bytes));

    let cert_dir = root.join("cert");
    std::fs::create_dir(&cert_dir).expect("fixture cert dir");
    let manifest = format!(
        r#"{{
  "schema_version": {schema},
  "wasm_sha256": "{hash}",
  "format": {{ "version": {version}, "wall_id": "{wall}" }},
  "artifact_certificate_root": "{root_name}",
  "profile": "wasm-gc",
  "abi": "wasm-gc",
  "certified": [],
  "runtime_contracts": [],
  "declaredUncertified": [],
  "capabilities": [],
  "start": {{ "present": false, "function_index": null }},
  "hostRoleTable": {{ "box": null, "add": null, "mul": null, "sub": null }},
  "stringHostRoles": []
}}
"#,
        schema = aver_cert::format::CERT_SCHEMA_VERSION,
        version = aver_cert::format::FORMAT_VERSION,
        wall = aver_cert::format::CURRENT_WALL_ID,
        root_name = aver_cert::format::ARTIFACT_CERTIFICATE_ROOT,
    );
    std::fs::write(cert_dir.join("cert-manifest.json"), manifest).expect("fixture manifest");

    Fixture {
        root,
        artifact,
        cert_dir,
    }
}

fn verifier_command(artifact: &Path, cert_dir: &Path) -> Command {
    let mut command = Command::new(env!("CARGO_BIN_EXE_aver-cert"));
    command.arg("check").arg(artifact).arg(cert_dir);
    command.env_remove("AVER_CERT_PRELUDE_CACHE");
    command.env_remove("AVER_CERT_DATA_CACHE");
    command
}

/// Probe for the pinned toolchain under a watchdog of its own: `elan` here is
/// whatever the ambient environment provides, and a probe that hangs or
/// starts a long download must skip the gate, not stall the suite.
fn pinned_toolchain_available() -> bool {
    let toolchain = aver_cert::wall::LEAN_TOOLCHAIN.trim();
    let probe = Command::new("elan")
        .args(["run", "--install", toolchain, "lake", "--version"])
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn();
    let Ok(mut probe) = probe else {
        return false;
    };
    let started = Instant::now();
    loop {
        match probe.try_wait() {
            Ok(Some(status)) => return status.success(),
            Ok(None) if started.elapsed() < Duration::from_secs(120) => {
                std::thread::sleep(Duration::from_millis(100));
            }
            _ => {
                let _ = probe.kill();
                let _ = probe.wait();
                return false;
            }
        }
    }
}

/// Wait for the verifier under a watchdog: if the hang guard under test is
/// broken, stop the verifier so the test fails promptly instead of hanging
/// the suite.
fn wait_with_watchdog(child: std::process::Child) -> std::process::Output {
    let verifier_pid = child.id();
    let (done_sender, done_receiver) = std::sync::mpsc::channel::<()>();
    let watchdog = std::thread::spawn(move || {
        if done_receiver
            .recv_timeout(Duration::from_secs(180))
            .is_err()
        {
            let _ = Command::new("kill")
                .args(["-KILL", &verifier_pid.to_string()])
                .status();
        }
    });
    let output = child.wait_with_output().expect("verifier finishes");
    let _ = done_sender.send(());
    watchdog.join().expect("watchdog thread");
    output
}

#[test]
fn check_fails_closed_when_a_lean_step_exceeds_the_time_limit() {
    if !pinned_toolchain_available() {
        assert!(
            std::env::var_os("AVER_CERT_REQUIRE_PINNED_LEAN").is_none(),
            "pinned Elan toolchain is required for this timeout gate"
        );
        eprintln!("skipping phase timeout test: pinned Elan toolchain unavailable");
        return;
    }

    let fixture = minimal_fixture();
    let started = Instant::now();
    let child = verifier_command(&fixture.artifact, &fixture.cert_dir)
        .env("AVER_CERT_PHASE_TIMEOUT_SECS", "2")
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("verifier starts");
    let verifier_pid = child.id();
    let output = wait_with_watchdog(child);
    let elapsed = started.elapsed();

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        elapsed < Duration::from_secs(170),
        "the verifier did not stop the timed-out step promptly ({elapsed:?})"
    );
    assert!(
        !output.status.success(),
        "a timed-out step must exit nonzero:\n{stdout}\n{stderr}"
    );
    assert!(
        !stdout.contains("CHECKED"),
        "green verdict on timeout:\n{stdout}"
    );
    assert!(stderr.contains("CHECK FAILED"), "{stderr}");
    assert!(stderr.contains("certificate proof build"), "{stderr}");
    assert!(stderr.contains("time limit"), "{stderr}");
    assert!(stderr.contains("AVER_CERT_PHASE_TIMEOUT_SECS"), "{stderr}");

    // No process may keep referencing this run's unique checker build dir
    // (lake spawns lean workers with absolute paths under it), and the build
    // dir itself must have been cleaned up.
    let marker = format!("aver-cert-check-{verifier_pid}-");
    let mut survivors = String::new();
    for _ in 0..100 {
        let listed = Command::new("pgrep")
            .args(["-fl", &marker])
            .output()
            .expect("pgrep runs");
        survivors = String::from_utf8_lossy(&listed.stdout).into_owned();
        if survivors.trim().is_empty() {
            break;
        }
        std::thread::sleep(Duration::from_millis(50));
    }
    assert!(
        survivors.trim().is_empty(),
        "lean/lake processes survived the timeout kill:\n{survivors}"
    );
    let leftovers = std::fs::read_dir("/tmp")
        .expect("temp root")
        .filter_map(|entry| entry.ok())
        .map(|entry| entry.file_name().to_string_lossy().into_owned())
        .filter(|name| name.starts_with(&marker))
        .collect::<Vec<_>>();
    assert!(
        leftovers.is_empty(),
        "checker build dirs survived the timeout: {leftovers:?}"
    );
}

/// A cache build that exceeds the per-step limit must degrade LOUDLY to a
/// cache miss: the warning names the step and the fallback, and the mandatory
/// proof build still runs afterwards (and here fails closed under its own
/// limit). No pinned toolchain is needed: `ELAN_HOME` points at a stub `elan`
/// that only sleeps, so both Lean steps deterministically exceed the tiny
/// limit.
#[test]
fn timed_out_cache_build_warns_and_the_mandatory_build_still_decides() {
    let fixture = minimal_fixture();
    let elan_home = fixture.root.join("stub-elan");
    std::fs::create_dir_all(elan_home.join("bin")).expect("stub elan home");
    let elan = elan_home.join("bin").join("elan");
    std::fs::write(&elan, "#!/bin/sh\nexec sleep 600\n").expect("stub elan");
    let mut permissions = std::fs::metadata(&elan)
        .expect("stub elan metadata")
        .permissions();
    std::os::unix::fs::PermissionsExt::set_mode(&mut permissions, 0o755);
    std::fs::set_permissions(&elan, permissions).expect("stub elan mode");

    let cache = fixture.root.join("prelude-cache");
    std::fs::create_dir(&cache).expect("cache store");

    let started = Instant::now();
    let child = verifier_command(&fixture.artifact, &fixture.cert_dir)
        .env("AVER_CERT_PRELUDE_CACHE", &cache)
        .env("ELAN_HOME", &elan_home)
        .env("AVER_CERT_PHASE_TIMEOUT_SECS", "2")
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("verifier starts");
    let output = wait_with_watchdog(child);
    let elapsed = started.elapsed();

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        elapsed < Duration::from_secs(170),
        "the verifier did not stop its timed-out steps promptly ({elapsed:?})"
    );
    assert!(
        stderr.contains(
            "warning: the proof library cache build exceeded its 2-second limit; \
             continuing without the cache"
        ),
        "the cache-build timeout must be loud:\n{stderr}"
    );
    // The warning did not become the verdict: the mandatory proof build ran
    // after the cache fallback and failed closed under its own limit.
    assert!(
        !output.status.success(),
        "a timed-out mandatory step must exit nonzero:\n{stdout}\n{stderr}"
    );
    assert!(
        !stdout.contains("CHECKED"),
        "green verdict on timeout:\n{stdout}"
    );
    assert!(stderr.contains("CHECK FAILED"), "{stderr}");
    assert!(stderr.contains("certificate proof build"), "{stderr}");
    assert!(stderr.contains("time limit"), "{stderr}");
}

#[test]
fn malformed_timeout_override_is_rejected_before_any_lean_step() {
    let fixture = minimal_fixture();
    let output = verifier_command(&fixture.artifact, &fixture.cert_dir)
        .env("AVER_CERT_PHASE_TIMEOUT_SECS", "never")
        .output()
        .expect("verifier finishes");
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(!output.status.success(), "{stderr}");
    assert!(
        stderr.contains("AVER_CERT_PHASE_TIMEOUT_SECS must be a positive whole number"),
        "{stderr}"
    );
}

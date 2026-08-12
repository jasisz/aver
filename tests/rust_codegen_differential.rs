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
/// process. The rust-on-MIR HIR walker was deleted in W6/Stage-3, so MIR
/// is the unconditional codegen path and there are no MIR flags left to
/// set — callers pass `&[]`. The env hook is retained for forward use.
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
/// parse the `mir_lowered` count (how many fns the MIR walker emits).
/// Since the HIR walker was deleted (W6/Stage-3) MIR is the sole codegen
/// path, so this is the "MIR is exercised" guard the probes assert up
/// front — a zero would mean the construct under test never reached the
/// walker (so the build / parity assertions would pass for the wrong
/// reason).
fn mir_lowered_count(
    file: &Path,
    module_root: Option<&Path>,
    extra: &[&str],
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
    let needle = "\"mir_lowered\":";
    let start = json
        .find(needle)
        .ok_or_else(|| format!("no `mir_lowered` field in coverage JSON:\n{json}"))?
        + needle.len();
    let rest = &json[start..];
    let end = rest
        .find(|c: char| !c.is_ascii_digit())
        .unwrap_or(rest.len());
    rest[..end]
        .parse::<u64>()
        .map_err(|e| format!("bad mir_lowered count {:?}: {e}", &rest[..end]))
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

/// `cargo test -q --offline` on the emitted project — builds AND runs the
/// `#[cfg(test)]` module that `emit_verify_blocks` generates from verify
/// blocks, which `cargo build` alone never compiles.
fn cargo_test_in(project_dir: &Path, target: &Path) -> Result<(), String> {
    fs::create_dir_all(target).expect("create cargo target dir");
    let out = Command::new("cargo")
        .arg("test")
        .arg("-q")
        .arg("--offline")
        .arg("--manifest-path")
        .arg(project_dir.join("Cargo.toml"))
        .env("CARGO_TARGET_DIR", target)
        .output()
        .expect("expected `cargo test` to spawn");
    if !out.status.success() {
        return Err(format!(
            "cargo test failed on emitted project:\n{}",
            format_output(&out)
        ));
    }
    Ok(())
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

// ─── own_param ownership: build+run rust vs VM + emitted-shape guard ─────

/// Build+run an inline Aver program through the Rust backend and return
/// trimmed stdout. Shares the fast-tier compile + cargo-build path.
fn build_run_rust_inline(name: &str, source: &str) -> Result<String, String> {
    let ws = temp_dir(name);
    let src = ws.join(format!("{name}.av"));
    fs::write(&src, source).expect("write source");
    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let result = (|| {
        compile_rust(&src, &project, name, None, &[])?;
        let bin = cargo_build(&project, name)?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("failed to run compiled binary: {e}"))?;
        if !out.status.success() {
            return Err(format!("binary exited non-zero:\n{}", format_output(&out)));
        }
        Ok(String::from_utf8_lossy(&out.stdout).trim().to_string())
    })();
    let _ = fs::remove_dir_all(&ws);
    result
}

/// Run an inline Aver program on the VM and return trimmed stdout.
fn run_vm_inline(name: &str, source: &str) -> Result<String, String> {
    let ws = temp_dir(&format!("{name}_vm"));
    let src = ws.join(format!("{name}.av"));
    fs::write(&src, source).expect("write source");
    let out = run_vm(&src, None);
    let _ = fs::remove_dir_all(&ws);
    out
}

/// Literal smart-constructor discharge on the Rust backend. A call whose
/// argument is an all-literal list inside the interval the refinement itself
/// proves types as the refined type and lowers to a direct carrier
/// construction; a computed argument keeps the fallible constructor. Both
/// shapes sit in one program, so a backend that discharged too much or too
/// little diverges from the VM here.
#[test]
fn rust_bits_namespace_matches_vm() {
    // `Bits` is a bit-level VIEW of `Int`, so the Rust backend must answer on
    // the SAME `AverInt` carrier the VM uses — never a raw i64. The cases that
    // would catch a drift are the ones past the 64-bit boundary (a truncating
    // backend gives a different number, not an error) and the negative-count
    // arm (whose `Result.Err` bytes must match the VM verbatim).
    let src = r#"module BitsDifferential
    intent = "Every Bits shape in one program, for cross-backend agreement"
    effects [Console.print]

fn pointwise() -> String
    ? "The four total operations, on operands of both signs."
    "{Bits.and(6, 3)} {Bits.or(6, 3)} {Bits.xor(6, 3)} {Bits.and(-1, 42)} {Bits.or(-1, 42)} {Bits.xor(-6, 3)} {Bits.not(0)} {Bits.not(-1)}"

fn counted() -> String
    ? "Discharged literal counts, including one past the 64-bit cliff."
    "{Bits.shiftLeft(1, 100)} {Bits.shiftRight(-3, 1)} {Bits.low(257, 8)} {Bits.low(-1, 8)} {Bits.low(123, 0)}"

fn large() -> String
    ? "Operands and results on the far side of the 64-bit boundary."
    huge = Bits.shiftLeft(1, 100)
    "{Bits.not(huge)} {Bits.or(huge, 1)} {Bits.xor(huge, huge)} {Bits.and(Bits.not(huge), huge)} {Bits.shiftRight(Bits.not(huge), 100)}"

fn dynamic(count: Int) -> String
    ? "An undischarged count, so the Result survives to runtime."
    match Bits.shiftLeft(1, count)
        Result.Ok(v) -> "ok {v}"
        Result.Err(e) -> "err {e}"

fn main() -> Unit
    ? "Print every shape so a backend that drifts shows a diff, not a pass."
    ! [Console.print]
    Console.print(pointwise())
    Console.print(counted())
    Console.print(large())
    Console.print(dynamic(4))
    Console.print(dynamic(-1))
"#;
    let expected = "2 7 5 42 -1 -7 -1 0\n1267650600228229401496703205376 -2 1 255 0\n-1267650600228229401496703205377 1267650600228229401496703205377 0 0 -2\nok 16\nerr negative shift count";

    let vm = run_vm_inline("bits_namespace", src).expect("vm run");
    let rust =
        build_run_rust_inline("bits_namespace", src).expect("rust compile + cargo build + run");
    assert_eq!(vm, expected, "VM Bits contract changed");
    assert_eq!(rust, expected, "Rust Bits semantics diverged from the VM");
}

#[test]
fn rust_literal_refinement_discharge_matches_vm() {
    let src = r#"module LiteralRefinement
    intent = "Discharged and fallible smart-constructor calls in one program"
    depends [Bytes]
    effects [Console.print]

fn describe(bytes: Bytes) -> String
    ? "Render a validated frame as hex plus its length."
    "{Bytes.toHex(bytes)}/{List.len(Bytes.toList(bytes))}"

fn dynamic(values: List<Int>) -> String
    ? "Validate a computed list through the fallible constructor."
    match Bytes.fromList(values)
        Result.Ok(bytes) -> describe(bytes)
        Result.Err(e) -> e

fn main() -> Unit
    ? "Print the discharged and fallible results side by side."
    ! [Console.print]
    Console.print(describe(Bytes.fromList([249, 190, 180, 217])))
    Console.print(describe(Bytes.fromList([])))
    Console.print(dynamic(List.concat([249, 190], [180, 217])))
    Console.print(dynamic([65, 256]))
"#;
    let expected = "f9beb4d9/4\n/0\nf9beb4d9/4\nbyte 256 at index 1 is outside 0..=255";

    let vm = run_vm_inline("literal_refinement_discharge", src).expect("vm run");
    let rust = build_run_rust_inline("literal_refinement_discharge", src)
        .expect("rust compile + cargo build + run");
    assert_eq!(vm, expected, "VM literal-discharge contract changed");
    assert_eq!(
        rust, expected,
        "Rust literal-discharge contract diverged from VM"
    );
}

/// `aver compile` can succeed while leaving a MIR-walker `compile_error!` in
/// the emitted project, so this regression must drive `Tcp.sendBytes` through
/// a real `cargo build`. Invalid raw lists fail at `Bytes.fromList` before any
/// socket connection, pinning the refinement boundary on both backends.
#[test]
fn rust_tcp_send_bytes_builds_with_the_bytes_refinement_boundary() {
    let src = r#"module TcpSendBytesRange
    intent = "Rust codegen must render Tcp.sendBytes with nominal Bytes"
    depends [Bytes]
    effects [Console, Tcp]

fn report(payload: List<Int>) -> Unit
    ? "Print the result of validating one binary payload."
    ! [Console.print, Tcp.sendBytes]
    match Bytes.fromList(payload)
        Result.Err(e) -> Console.print(e)
        Result.Ok(bytes) -> match Tcp.sendBytes("127.0.0.1", 1, bytes)
            Result.Ok(_) -> Console.print("unexpected-ok")
            Result.Err(e) -> Console.print(e)

fn main() -> Unit
    ! [Console.print, Tcp.sendBytes]
    report([65, 256])
    report([65, 1208925819614629174706176])
"#;
    let expected = "byte 256 at index 1 is outside 0..=255\n\
                    byte 1208925819614629174706176 at index 1 is outside 0..=255";

    let vm = run_vm_inline("tcp_send_bytes_range", src).expect("vm run");
    let rust = build_run_rust_inline("tcp_send_bytes_range", src)
        .expect("rust compile + cargo build + run");
    assert_eq!(vm, expected, "VM Bytes refinement contract changed");
    assert_eq!(
        rust, expected,
        "Rust Bytes refinement contract diverged from VM"
    );
}

#[test]
fn rust_tcp_read_bytes_builds_with_nominal_bytes() {
    let src = r#"module TcpReadBytesBuild
    intent = "Rust codegen must render Tcp.readBytes with nominal Bytes"
    depends [Bytes]
    effects [Console, Tcp]

fn readFrame(conn: Tcp.Connection, count: Int) -> Result<Bytes, String>
    ? "Read one exact binary frame."
    ! [Tcp.readBytes]
    Tcp.readBytes(conn, count)

fn main() -> Unit
    ! [Console.print]
    Console.print("compiled")
"#;

    let vm = run_vm_inline("tcp_read_bytes_build", src).expect("vm run");
    let rust = build_run_rust_inline("tcp_read_bytes_build", src)
        .expect("rust compile + cargo build + run");
    assert_eq!(vm, "compiled");
    assert_eq!(rust, vm, "Rust Tcp.readBytes codegen diverged from VM");
}

#[test]
fn rust_tcp_write_bytes_builds_with_nominal_bytes() {
    let src = r#"module TcpWriteBytesBuild
    intent = "Rust codegen must render Tcp.writeBytes with nominal Bytes"
    depends [Bytes]
    effects [Console, Tcp]

fn writeFrame(conn: Tcp.Connection, payload: Bytes) -> Result<Unit, String>
    ? "Write one exact binary frame."
    ! [Tcp.writeBytes]
    Tcp.writeBytes(conn, payload)

fn main() -> Unit
    ! [Console.print]
    Console.print("compiled")
"#;

    let vm = run_vm_inline("tcp_write_bytes_build", src).expect("vm run");
    let rust = build_run_rust_inline("tcp_write_bytes_build", src)
        .expect("rust compile + cargo build + run");
    assert_eq!(vm, "compiled");
    assert_eq!(rust, vm, "Rust Tcp.writeBytes codegen diverged from VM");
}

/// `Crypto.sha256` produces a `Digest32` even when the program never names
/// `Crypto.Digest32` in `depends`. The emitted Rust references
/// `crate::aver_generated::crypto::digest32::Digest32`, so the owning
/// standard module must join the generated project implicitly — this used to
/// pass `aver compile` and then fail `cargo build` with E0433. The printed
/// tag derives from the digests (structural equality of two hashes), so the
/// sha256 calls stay observable: a future optimizer that deleted an unused
/// pure call would otherwise silently stop exercising the emission path
/// while this test kept passing.
#[test]
fn rust_sha256_builds_without_digest32_in_depends() {
    let src = r#"module Sha256ImplicitDigest
    intent = "Crypto.sha256 must build even when depends omits Crypto.Digest32"
    depends [Bytes]
    effects [Console.print]

fn hashTag(xs: List<Int>, ys: List<Int>) -> Result<String, String>
    ? "Hash both octet lists and report whether the digests agree."
    first = Crypto.sha256(Bytes.fromList(xs)?)
    second = Crypto.sha256(Bytes.fromList(ys)?)
    match first == second
        true -> Result.Ok("digests-agree")
        false -> Result.Ok("digests-differ")

fn main() -> Unit
    ? "Hash byte lists and print digest agreement."
    ! [Console.print]
    match hashTag([1, 2, 3], [1, 2, 3])
        Result.Ok(tag) -> Console.print(tag)
        Result.Err(msg) -> Console.print(msg)
    match hashTag([1, 2, 3], [9, 9, 9])
        Result.Ok(tag) -> Console.print(tag)
        Result.Err(msg) -> Console.print(msg)
"#;

    let vm = run_vm_inline("sha256_implicit_digest32", src).expect("vm run");
    let rust = build_run_rust_inline("sha256_implicit_digest32", src)
        .expect("rust compile + cargo build + run");
    assert_eq!(vm, "digests-agree\ndigests-differ");
    assert_eq!(rust, vm, "Rust Crypto.sha256 codegen diverged from VM");
}

/// A program whose ONLY `Crypto.sha256` call sits in a verify case still
/// generates a project whose `#[cfg(test)]` module references
/// `crate::aver_generated::crypto::digest32::Digest32` — the implicit
/// stdlib-module scan must cover verify blocks, not just fn bodies. This
/// used to emit a project that passed `cargo build` and then failed
/// `cargo test` with E0433. Driving `cargo test` (not just the build) also
/// proves the digest-equality cases hold in the generated code.
///
/// The last verify case uses `Bytes.fromHex(…)?` directly, which doubles
/// as the regression for `?` inside a verify case: the generated test fn
/// returns `Result<(), String>` while generated stdlib fns error with
/// `AverStr`, so the emitter must convert at the `?` boundary (bare `?`
/// used to fail `cargo test` with E0277). It also pins that a discharged
/// literal `Bytes.fromList([1, 2])` and the equivalent value built
/// through the fallible `fromHex` path hash to the same digest.
#[test]
fn rust_sha256_verify_only_generates_testable_project() {
    let src = r#"module Sha256VerifyOnly
    intent = "A verify-only Crypto.sha256 call still generates a buildable test module"
    depends [Bytes]
    effects [Console.print]

fn describe(same: Bool) -> String
    ? "Describe digest agreement."
    match same
        true -> "same"
        false -> "different"

fn main() -> Unit
    ? "Print a fixed label."
    ! [Console.print]
    Console.print(describe(true))

verify describe
    describe(Crypto.sha256(Bytes.fromList([1, 2])) == Crypto.sha256(Bytes.fromList([1, 2]))) => "same"
    describe(Crypto.sha256(Bytes.fromList([1, 2])) == Crypto.sha256(Bytes.fromList([2, 1]))) => "different"
    describe(Crypto.sha256(Bytes.fromHex("0102")?) == Crypto.sha256(Bytes.fromList([1, 2]))) => "same"
"#;

    let name = "sha256_verify_only";
    let ws = temp_dir(name);
    let src_file = ws.join(format!("{name}.av"));
    fs::write(&src_file, src).expect("write source");
    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let result = (|| {
        compile_rust(&src_file, &project, name, None, &[])?;
        cargo_test_in(&project, &shared_target_dir())
    })();
    let _ = fs::remove_dir_all(&ws);
    result.expect("rust compile + cargo test of the generated verify module");
}

/// The #383 corruption class on the RUST backend: a Vector PARAM captured
/// into a record field AND own-mutated, both in the SAME fn on the SAME
/// param. `own_param`'s capture guard must keep the slot flagged so the
/// Rust emit keeps the `.clone()` at the mutation site (refcount-2
/// `Rc::make_mut` deep-copies → the record's snapshot is protected).
/// Build+run the emitted Rust and assert it equals the VM — a wrongly
/// skipped clone would diverge (1998 vs the correct 1006).
#[test]
fn rust_param_captured_and_mutated_in_same_fn_matches_vm() {
    let src = r#"module SameFnCapture
    intent = "Vector param captured into a record AND mutated in the same fn (#383 class)"
    depends []
    effects [Console.print]

record Holder
    snapshot: Vector<Int>

fn captureAndMutate(v: Vector<Int>) -> Int
    ? "store v in a record, then set position 0 to 999 on v; snapshot must read the original"
    h = Holder(snapshot = v)
    mutated = Option.withDefault(Vector.set(v, 0, 999), v)
    snap0 = Option.withDefault(Vector.get(h.snapshot, 0), 0 - 1)
    mut0 = Option.withDefault(Vector.get(mutated, 0), 0 - 1)
    snap0 + mut0

fn run() -> Int
    base = Option.withDefault(Vector.set(Vector.new(3, 0), 0, 7), Vector.new(3, 0))
    captureAndMutate(base)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(run()))
"#;
    let vm = run_vm_inline("samecap", src).expect("vm run");
    let rust = build_run_rust_inline("samecap", src).expect("rust build+run");
    assert_eq!(vm, "1006", "VM must compute the immutable-model value");
    assert_eq!(
        rust, vm,
        "Rust diverged from VM — a captured param's clone was wrongly skipped"
    );
}

/// Multi-tail-call Int-unboxing soundness hole (the C0 bug): a counter `n`
/// with TWO self-tail-call paths — one decrements (`n - 1`), one GROWS
/// (`n + 1_000_000_000_000_000_000`). The pre-fix recurrence recognizer saw
/// only the first (decrement) path, under-approximated `n`'s interval, and
/// marked it bare → the Rust backend emitted native `i64` for `n`, so at
/// runtime the growth path drove `n` past `i64::MAX` and `n + n` silently
/// WRAPPED in release (a panic in dev under `overflow-checks`). The fix
/// boxes `n` (every self-tail-call must be the SAME monotone decrement, else
/// the recurrence is unbounded). Build+run the emitted Rust and assert it
/// equals the VM's arbitrary-precision `10000000000000000016` — a wrongly
/// bare counter would diverge (wrap) or panic.
#[test]
fn rust_multi_tailcall_growing_counter_matches_vm() {
    let src = r#"module MultiTail
    intent = "two self-tail-call paths: one decrements, one grows — counter must box"
    depends []
    effects [Console.print]

fn loopit(n: Int, phase: Int) -> Int
    ? "phase 5000 decrements n; any other non-zero phase grows n past i64 range"
    match n
        0 -> n + n
        _ -> match phase
            0 -> n + n
            5000 -> loopit(n - 1, phase)
            _ -> loopit(n + 1000000000000000000, phase - 1)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(loopit(8, 5)))
"#;
    let vm = run_vm_inline("multitail", src).expect("vm run");
    let rust = build_run_rust_inline("multitail", src).expect("rust build+run");
    assert_eq!(
        vm, "10000000000000000016",
        "VM computes the arbitrary-precision (no-wrap) value"
    );
    assert_eq!(
        rust, vm,
        "Rust diverged from VM — the multi-tail-call counter was wrongly unboxed to i64 and wrapped/panicked"
    );
}

/// Transient intermediate-overflow Int-unboxing hole: a compound
/// `(n + i64::MAX) - i64::MAX` over a bare counter `n`. The whole-tree result
/// narrows back INTO `i64`, but the inner `n + i64::MAX` intermediate leaves
/// `i64`. The pre-fix `bare_expr_interval` checked only the FINAL result
/// interval, so the compound was marked bare and the Rust backend emitted a
/// MIXED tree — the inner `add` boxed-then-`to_i64()` then a raw `i64` outer
/// `.sub` — which does NOT compile (`E0599 no method 'sub' for i64`). This is
/// fail-closed (a compile error, never a silent wrap), but a valid Aver
/// program must still compile: the fix gates EVERY intermediate against `i64`
/// (mirroring the analysis-side `eval_interval` worst-join), so the whole
/// compound boxes. Build+run the emitted Rust and assert it builds and equals
/// the VM's exact `1`.
#[test]
fn rust_transient_intermediate_overflow_compound_matches_vm() {
    let src = r#"module IntermediateOverflow
    intent = "a compound whose intermediate leaves i64 but final narrows back must box"
    depends []
    effects [Console.print]

fn loopit(n: Int) -> Int
    ? "base case adds then subtracts i64::MAX; the intermediate overflows i64"
    match n
        1 -> (n + 9223372036854775807) - 9223372036854775807
        _ -> loopit(n - 1)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(loopit(5)))
"#;
    let vm = run_vm_inline("intermediate_overflow", src).expect("vm run");
    let rust = build_run_rust_inline("intermediate_overflow", src)
        .expect("rust build+run (pre-fix: E0599 — the mixed bare/boxed compound did not compile)");
    assert_eq!(
        vm, "1",
        "VM computes the exact value (the intermediate is ℤ, never wraps)"
    );
    assert_eq!(
        rust, vm,
        "Rust diverged from VM — the transient-overflow compound was wrongly kept bare"
    );
}

/// The own_param perf win on the RUST backend, verified end-to-end: a
/// linearly-threaded Vector param the pass proves uniquely owned must (a)
/// build+run to the same result as the VM, and (b) emit the OWNED-by-value
/// in-place shape — `let __vec = v;` (a MOVE) NOT `let __vec = v.clone();`
/// — so the `Rc::make_mut` runs on a refcount-1 backing (native O(n)),
/// the whole point of wiring own_param into rust. A regression that drops
/// the graduation would re-introduce the `.clone()` (O(n²) COW) and trip
/// the emitted-shape assert even if the result stays correct.
#[test]
fn rust_owned_vector_param_emits_in_place_move_and_matches_vm() {
    let src = r#"module Fill
    intent = "linearly-threaded vector fill+sum — the own_param rust target"
    depends []
    effects [Console.print]

fn fillVector(v: Vector<Int>, n: Int, i: Int) -> Vector<Int>
    ? "tail-recursive fill: write i*i at position i"
    match i == n
        true -> v
        false -> fillVector(Option.withDefault(Vector.set(v, i, i * i), v), n, i + 1)

fn sumVector(v: Vector<Int>, n: Int, i: Int, acc: Int) -> Int
    ? "tail-recursive sum across positions 0..n"
    match i == n
        true -> acc
        false -> sumVector(v, n, i + 1, acc + Option.withDefault(Vector.get(v, i), 0))

fn run() -> Int
    v = fillVector(Vector.new(5, 0), 5, 0)
    sumVector(v, 5, 0, 0)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(run()))
"#;
    // (a) build+run vs VM
    let vm = run_vm_inline("fillmove", src).expect("vm run");
    let rust = build_run_rust_inline("fillmove", src).expect("rust build+run");
    assert_eq!(vm, "30", "VM fill+sum value");
    assert_eq!(rust, vm, "Rust fill+sum diverged from VM");

    // (b) emitted-shape guard: the fusion in `fillVector` must MOVE the
    // owned param (no `.clone()`).
    let ws = temp_dir("fillmove_emit");
    let f = ws.join("fillmove.av");
    fs::write(&f, src).expect("write");
    let project = ws.join("project");
    fs::create_dir_all(&project).expect("project dir");
    compile_rust(&f, &project, "fillmove", None, &[]).expect("compile rust");
    let emitted = fs::read_to_string(project.join("src/aver_generated/entry/mod.rs"))
        .expect("read generated entry module");
    let _ = fs::remove_dir_all(&ws);
    assert!(
        emitted.contains("let __vec = v;"),
        "expected owned-by-value MOVE (`let __vec = v;`) in fillVector, got:\n{emitted}"
    );
    assert!(
        !emitted.contains("let __vec = v.clone();"),
        "fillVector still clones the owned vector param (O(n²) COW regressed):\n{emitted}"
    );
}

/// Nested-tuple Int-literal match on the RUST backend. After the
/// `Int -> AverInt` migration an `AverInt` can't be a Rust `match` pattern,
/// so Int-literal arms lower to an if/else-if equality-guard chain. That
/// lowering originally handled only one level of tuple nesting and bailed on
/// a NESTED tuple, emitting `compile_error!("MIR walker could not render …")`
/// — which builds fine on the VM but fails `cargo build`. This drives a match
/// with a deeply-nested tuple pattern (literal leaves at depth, plus a binding
/// and a wildcard) end-to-end: compile to Rust, `cargo build`, RUN, and assert
/// stdout equals the VM. `aver compile` exits 0 even when it emits the
/// `compile_error!` stub, so only the real build+run catches the regression.
#[test]
fn rust_nested_tuple_int_literal_match_builds_and_matches_vm() {
    let src = r#"module NestedTupleIntMatch
    intent = "Nested-tuple Int-literal match must lower to recursive equality guards"
    depends []
    effects [Console.print]

fn pick(t: Tuple<Int, Tuple<Int, Tuple<Int, Int>>, Int>) -> Int
    ? "match Int literals nested two tuples deep, plus a binding arm"
    match t
        (1, (2, (3, 4)), 5) -> 99
        (1, (x, _), _) -> x
        _ -> 0

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(pick((1, (2, (3, 4)), 5))))
    Console.print(String.fromInt(pick((1, (42, (7, 8)), 9))))
    Console.print(String.fromInt(pick((5, (2, (3, 4)), 5))))
"#;
    // First arm matches (→ 99); second arm binds the nested `x` (→ 42);
    // neither literal arm matches the third call (→ 0).
    let vm = run_vm_inline("nestedtupleint", src).expect("vm run");
    let rust = build_run_rust_inline("nestedtupleint", src).expect("rust build+run");
    assert_eq!(vm, "99\n42\n0", "VM nested-tuple Int match values");
    assert_eq!(
        rust, vm,
        "Rust nested-tuple Int-literal match diverged from VM (or emitted a compile_error! stub)"
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

// ─── Mode (d): effectful security probe ──────────────────────────────────
//
// SECURITY-SENSITIVE. The MIR walker OWNS effectful builtin emission
// (replay / policy / bare framing) — it is the sole codegen path now. A
// dropped wrapper there silently disables aver.toml DENY enforcement or
// record/replay capture — and it's invisible to rustc, to coverage, and
// to happy-path stdout. These probes build + RUN the binary and assert
// the policy is actually enforced / the effect is actually captured, so
// a dropped MIR wrapper reaches the built binary and the probe catches it.
//
// `mir_lowered_count(...)` asserts up front that the effectful fn really
// lowered to MIR — without that guard an empty program would let the
// probe pass for the wrong reason.

/// Single-fn Disk-write program for the embedded-policy probe. The
/// write rides a helper fn (`writeIt`) that is a single-expr body, so the
/// `aver_policy::check_disk` wrapper is emitted by the MIR walker.
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

    let result = (|| -> Result<(), String> {
        // (0) PROVE the MIR path is actually exercised: the effectful
        // `writeIt` must lower to MIR. Without this, an empty program
        // would make the probe meaningless.
        let lowered = mir_lowered_count(&src, Some(&proj_root), &["--policy", "embed"])?;
        if lowered == 0 {
            return Err(
                "no fn lowered to MIR — the effectful builtin emit is not being \
                 exercised by the MIR walker; the probe would be testing nothing"
                    .to_string(),
            );
        }

        // (1) DENY: embed an aver.toml whose allow-list names a
        // DIFFERENT path → the write to out.txt is denied at compile-
        // baked policy. The MIR walker emits the `aver_policy::check_disk`
        // wrapper, then build + run.
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
            &[],
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
            &[],
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
/// shape as `READ_ECHO_PROBE`. The MIR walker (the sole codegen path)
/// emits the `aver_replay::invoke_effect` reroute for BOTH effects.
/// `__PATH__` is substituted at test time.
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

    let result = (|| -> Result<(), String> {
        // (0) PROVE the MIR path is exercised: the effectful `readIt`
        // must lower to MIR.
        let lowered = mir_lowered_count(&src, None, &["--with-replay"])?;
        if lowered == 0 {
            return Err(
                "no fn lowered to MIR — the MIR replay reroute is not being exercised".to_string(),
            );
        }

        compile_rust_env(&src, &project, name, None, &["--with-replay"], &[])?;

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
// The MIR walker synthesizes the self-TCO loop and the mutual-recursion
// trampoline from `MirExpr::TailCall` (the sole codegen path since the
// HIR walker was deleted). TCO is verified BEHAVIORALLY here (there is no
// byte-parity gate): build + RUN the MIR-synthesized binary and
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
        // MIR is the sole codegen path: the self-TCO loop + mutual-rec
        // trampoline are synthesized from `MirExpr::TailCall` by the MIR
        // walker (`emit_mir_tco_fn` / `emit_mir_mutual_tco_block`).
        compile_rust(&src, &project, name, None, &[])?;
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
// The MIR walker emits `MirExpr::IndependentProduct` — the Rust
// backend's truly-PARALLEL product (`std::thread::scope` + per-branch
// `spawn`, the cancel-flag machinery for `?!`, the bare tuple fold for
// `!`) — as the sole codegen path. This probe proves the MIR walker
// OWNS the parallel emission and the built binary still produces
// VM-identical output.
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

    let result = (|| -> Result<(), String> {
        // (0) PROVE the MIR path is exercised: the IndependentProduct fns
        // must lower to MIR. Without this guard an empty program would
        // let the probe pass for the wrong reason.
        let lowered = mir_lowered_count(&file, None, &[])?;
        if lowered == 0 {
            return Err(
                "no fn lowered to MIR — the IndependentProduct emit is not being \
                 exercised by the MIR walker"
                    .to_string(),
            );
        }

        // The parallel IndependentProduct shape is MIR-emitted (the sole
        // codegen path), then build + run.
        compile_rust(&file, &project, name, None, &[])?;

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
// (LocalSlot). The MIR walker (the sole codegen path) emits the
// FnValue / LocalSlot fns, so the built binary's `a=42 b=42` proves the
// MIR walker OWNS the first-class-fn emission (a dropped FnValue arg or a
// mis-emitted slot call would change stdout or fail to build).
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

    let result = (|| -> Result<(), String> {
        // (0) PROVE the MIR path is exercised: the FnValue / LocalSlot fns
        // (`applyIt`, `runDouble`, `runInc`) must lower to MIR. Without
        // this guard an empty program would let the probe pass for the
        // wrong reason (the FnValue / LocalSlot emit would never run).
        // `dbl` / `inc` / the three higher-order fns + `main` all lower —
        // so ≥ 5 must lower.
        let lowered = mir_lowered_count(&src, None, &[])?;
        if lowered < 5 {
            return Err(format!(
                "expected ≥ 5 fns to lower to MIR (dbl, inc, applyIt, runDouble, \
                 runInc) — got {lowered}. The FnValue / LocalSlot emit is not being \
                 exercised by the MIR walker."
            ));
        }

        // The FnValue arg + LocalSlot call are MIR-emitted (the sole
        // codegen path), then build + run.
        compile_rust(&src, &project, name, None, &[])?;

        // Structural tripwire: the emitted Rust must carry the fn-pointer
        // param (`f: fn(aver_rt::AverInt) -> aver_rt::AverInt` — `Int` lowers
        // to `AverInt` now), the FnValue passed by bare name
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
        if !emitted.contains("f: fn(aver_rt::AverInt) -> aver_rt::AverInt") {
            return Err(format!(
                "emitted Rust is missing the fn-pointer param \
                 `f: fn(aver_rt::AverInt) -> aver_rt::AverInt` — \
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

// ─── Mode (h): Int.fromString / Float.fromString Err-message bytes ────────
//
// `Int.fromString` / `Float.fromString` return a `Result<_, String>`.
// On a failed parse the VM (`src/types/int.rs` / `float.rs`) formats the
// Err string as `Cannot parse '{input}' as Int` / `… as Float`. The Rust
// emit used to delegate to rustc's NATIVE `parse` error ("invalid digit
// found in string"), a real cross-backend SEMANTIC divergence: a program
// that reads the `Result.Err(String)` got different bytes on Rust vs the
// VM, and verify cases asserting the message failed under `cargo test`.
//
// This probe reads BOTH parse Results back as strings and prints them, so
// the built binary's stdout carries the exact Err bytes. A regression to
// the native rustc message would change the bytes here and fail parity.
const MIR_FROMSTRING_ERR_PROBE: &str = r#"module FromStringErrProbe
    intent =
        "Probes Int.fromString / Float.fromString Err-message bytes."
        "The Err string must match the VM byte-for-byte, not rustc's native parse error."
    effects [Console]

fn showInt(s: String) -> String
    ? "Render the Int.fromString Result as a string."
    match Int.fromString(s)
        Result.Ok(n) -> "ok:{n}"
        Result.Err(e) -> "err:{e}"

fn showFloat(s: String) -> String
    ? "Render the Float.fromString Result as a string."
    match Float.fromString(s)
        Result.Ok(f) -> "ok:{f}"
        Result.Err(e) -> "err:{e}"

fn main() -> Unit
    ! [Console.print]
    Console.print(showInt("bad"))
    Console.print(showInt("12x"))
    Console.print(showInt(""))
    Console.print(showInt("42"))
    Console.print(showFloat("nope"))
    Console.print(showFloat("3.14"))
"#;

#[test]
fn mir_fromstring_err_message_matches_vm() {
    let ws = temp_dir("mir_fromstring");
    let src = ws.join("fromstring_err_probe.av");
    fs::write(&src, MIR_FROMSTRING_ERR_PROBE).expect("write fromString probe source");

    let vm_stdout = run_vm(&src, None).unwrap_or_else(|e| panic!("VM run failed: {e}"));

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let name = "mir_fromstring_probe";

    let result = (|| -> Result<(), String> {
        // NOTE: no `mir_lowered_count` guard here — every fn in this probe
        // calls a builtin (`Int.fromString` / `Console.print`), and the
        // coverage walk's `--explain-mir-coverage` reports `Call(Builtin)`
        // as a fallback (its `for_test` ctx carries an empty builtin
        // table), so it would report `mir_lowered = 0` even though the
        // production path emits these fns fine. The structural tripwire
        // below (the VM-format message must appear in the emitted Rust)
        // is the real "fromString emit is exercised" guard.
        compile_rust(&src, &project, name, None, &[])?;

        // Structural tripwire: the emitted Rust must format the Aver Err
        // message, NOT delegate to rustc's native parse error.
        let emitted = fs::read_to_string(
            project
                .join("src")
                .join("aver_generated")
                .join("entry")
                .join("mod.rs"),
        )
        .map_err(|e| format!("read emitted module: {e}"))?;
        if !emitted.contains("Cannot parse '{}' as Int")
            || !emitted.contains("Cannot parse '{}' as Float")
        {
            return Err(format!(
                "emitted Rust is missing the VM-format fromString Err message \
                 (`Cannot parse '{{}}' as Int/Float`) — it likely regressed to \
                 rustc's native parse error:\n{emitted}"
            ));
        }

        let bin = cargo_build(&project, name)?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("failed to run compiled binary: {e}"))?;
        if !out.status.success() {
            return Err(format!(
                "MIR fromString binary exited non-zero:\n{}",
                format_output(&out)
            ));
        }
        let rust_stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if rust_stdout != vm_stdout {
            return Err(format!(
                "MIR fromString Err-bytes mismatch (Rust must match VM byte-for-byte)\n\
                 --- VM ---\n{vm_stdout}\n--- Rust (MIR) ---\n{rust_stdout}"
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

// ═══════════════════════════════════════════════════════════════════════
// The MIR behavioral net (MIR is the sole Rust codegen path)
// ═══════════════════════════════════════════════════════════════════════
//
// The HIR `ResolvedExpr` walker was deleted in W6/Stage-3, so the MIR
// walker OWNS all runtime codegen unconditionally — there are no flags
// to set anymore. This section is the STANDING, REPEATABLE gate: build +
// run the corpus and assert the built binaries match the VM. Since every
// compile already takes the MIR path, "forced-MIR" is just "the corpus".
//
// Two discipline guards make this a real net, not theater:
//
//  - **mir_lowered > 0**: every parity assert first checks
//    `mir_lowered_count(...) > 0`, so a test cannot pass while the
//    construct under test never reached the walker (which would mean the
//    build / parity assertion validated nothing relevant).
//  - **per-test isolated target dir** (the W6 audit flakiness fix): the
//    generated-project `cargo build` runs against a UNIQUE target dir per
//    example, so concurrent `--offline` builds never corrupt each other's
//    `.rmeta` / proc-macro outputs.

/// Per-example isolated `cargo build` target dir. The forced-MIR tier
/// uses one of these per example so concurrent `--offline` builds never
/// race on a shared target tree (the `.rmeta` / proc-macro corruption hit
/// during the W6 audit). Lives next to the temp project so the temp-dir
/// cleanup reclaims it.
fn isolated_target_dir(ws: &Path) -> PathBuf {
    ws.join("cargo-target")
}

/// Compile + build + RUN an example through the (sole) MIR codegen path,
/// asserting stdout parity with the VM. Applies the mir_lowered > 0 guard
/// (the program must actually reach the walker) + a per-example isolated
/// target dir.
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
        // Guard: SOMETHING must lower to MIR (so the parity assert below
        // is validating the MIR walker's output, not an empty program).
        let lowered = mir_lowered_count(&file, root.as_deref(), &[])?;
        if lowered == 0 {
            return Err(format!(
                "{relative}: no fn lowered to MIR — the parity assert here would \
                 be validating an empty program"
            ));
        }

        compile_rust_env(&file, &project, &name, root.as_deref(), &[], &[])?;
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

// ─── Oracle-shape full-corpus build (no residual Rust exclusions) ────────
//
// The Oracle-only `BranchPath` type and the Terminal-only `Terminal.Size`
// type are emitted by the Rust backend (a `pub use aver_rt::BranchPath`
// / `Terminal_Size` alias, mirroring `Tcp.Connection`), and the verify
// module skips the verify-only Oracle / trace / given-universal cases, so
// the Oracle programs (`oracle_trace`, `hostile_order_axis`,
// `clock_as_data`, `randomness_paradox`, `terminal_size_snapshot`,
// `file_store_shell`, `services/redis`) `cargo build` + `cargo test`
// cleanly on Rust and run with VM parity.
//
// The last residual was `oracle_independent_products.av`: its higher-order
// spec fn `pairSpec(path, rnd: Fn(BranchPath, …) -> Int)` builds a tuple
// from calls to the `rnd` fn-pointer param over `BranchPath.child(path, n)`
// — the MIR walker bailed because the `BranchPath.child` / `.parse` builtin
// calls and the `BranchPath.Root` nullary value had no emit arm (they fell
// through to `_ => None`, yielding a `compile_error!`). Adding those arms
// (the `aver_rt::BranchPath::{child,parse,root}` constructors) closes the
// gap — the higher-order tuple-of-LocalSlot-calls shape already emitted.
// There is now NO residual Rust-build exclusion in the example corpus.

/// (relative path, optional module root) of the Oracle-shape examples that
/// must `cargo build` cleanly on the (sole) MIR codegen path with zero
/// `compile_error!` stubs. Previously these were the *exclusions*; the
/// BranchPath builtin-emit gap is now closed, so the assertion flipped to
/// a positive build proof. A regression (a dropped `BranchPath.*` arm, an
/// undefined `BranchPath` symbol, or a re-introduced higher-order None)
/// fails the `cargo build` and this test catches it.
const BRANCH_PATH_BUILDS: &[(&str, Option<&str>)] = &[(
    "examples/formal/oracle_independent_products.av",
    Some("examples"),
)];

/// `cargo build` an example through the (sole) MIR codegen path,
/// returning Ok(()) on a clean build or Err(stderr) on failure. Used by
/// the BranchPath build proof to confirm the Oracle-shape examples build
/// on Rust without `compile_error!` stubs.
fn try_build_walker(
    relative: &str,
    module_root: Option<&str>,
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

    // The compile (Rust source emit) may itself fail; if so that's the
    // failure to inspect. Capture it as an Err.
    if let Err(e) = compile_rust(&file, &project, &name, root.as_deref(), &[]) {
        return Ok(Err(format!("compile: {e}")));
    }
    // Guard against a silent `compile_error!` stub slipping through: any
    // such stub fails `cargo build` below, but assert explicitly too so a
    // regression names the exact failure mode.
    let emitted = fs::read_to_string(
        project
            .join("src")
            .join("aver_generated")
            .join("entry")
            .join("mod.rs"),
    )
    .map_err(|e| format!("read emitted module: {e}"))?;
    if emitted.contains("compile_error!") {
        return Ok(Err(
            "emitted Rust still carries a `compile_error!` stub — a fn body \
             the MIR walker could not render"
                .to_string(),
        ));
    }
    match cargo_build_in(&project, &name, &ws.join(format!("target-{tag}"))) {
        Ok(_) => Ok(Ok(())),
        Err(e) => Ok(Err(e)),
    }
}

#[test]
#[ignore = "full tier: cargo build wall-time; set AVER_RUST_DIFF_FULL=1 and run with --ignored"]
fn oracle_shape_examples_build_clean_on_rust() {
    if std::env::var("AVER_RUST_DIFF_FULL").is_err() {
        eprintln!(
            "skipping Oracle-shape build proof — set AVER_RUST_DIFF_FULL=1 \
             (proves the higher-order BranchPath Oracle example builds clean on Rust)"
        );
        return;
    }

    let mut failures = Vec::new();
    let mut confirmed = 0usize;

    for (relative, root) in BRANCH_PATH_BUILDS {
        let ws = temp_dir(&format!("bp-{}", sanitise(relative)));
        let built = try_build_walker(relative, *root, &ws, "mir");
        let _ = fs::remove_dir_all(&ws);

        match built {
            Ok(Ok(())) => confirmed += 1,
            Ok(Err(err)) => failures.push(format!(
                "{relative}: expected a clean Rust build (BranchPath builtin-emit \
                 gap is closed), but it failed:\n  err:\n{err}"
            )),
            Err(e) => failures.push(format!("{relative}: harness error: {e}")),
        }
    }

    eprintln!(
        "oracle_shape_examples_build_clean_on_rust: {confirmed}/{} built clean",
        BRANCH_PATH_BUILDS.len()
    );
    assert!(
        failures.is_empty(),
        "{} of {} Oracle-shape examples did not build clean:\n  - {}",
        failures.len(),
        BRANCH_PATH_BUILDS.len(),
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
/// `main` + top-level-statement values routed through MIR (the sole
/// codegen path). The games are excluded: every one is an interactive
/// Terminal loop (Terminal is host territory + no deterministic batch
/// stdout), so they are neither buildable in Rust nor run-parity
/// candidates.
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

    let result = (|| -> Result<(), String> {
        // (0) PROVE the MIR path is exercised: rollDie + stamp must
        // lower to MIR.
        let lowered = mir_lowered_count(&src, None, &["--with-replay"])?;
        if lowered == 0 {
            return Err(
                "no fn lowered to MIR — the Time / Random replay reroute is not \
                 being exercised"
                    .to_string(),
            );
        }

        compile_rust_env(&src, &project, name, None, &["--with-replay"], &[])?;

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
fn rust_tcp_send_bytes_round_trips_non_utf8() {
    if std::env::var("AVER_RUST_DIFF_FULL").is_err() {
        eprintln!("skipping Rust Tcp.sendBytes probe — set AVER_RUST_DIFF_FULL=1");
        return;
    }

    use std::io::{Read, Write};
    use std::net::TcpListener;

    let listener = TcpListener::bind("127.0.0.1:0").expect("bind loopback listener");
    let port = listener.local_addr().expect("listener addr").port();
    listener
        .set_nonblocking(true)
        .expect("set listener nonblocking");

    let ws = temp_dir("tcp-send-bytes");
    let src = ws.join("tcp_send_bytes.av");
    fs::write(
        &src,
        format!(
            r#"module TcpSendBytesProbe
    intent = "Round-trip non-UTF-8 bytes through the Rust backend"
    depends [Bytes]
    effects [Console, Tcp]

fn exchange() -> Result<Bytes, String>
    ? "Send one binary payload to a loopback echo server."
    ! [Tcp.sendBytes]
    payload = Bytes.fromList([249, 190, 180, 217])
    Tcp.sendBytes("127.0.0.1", {port}, payload)

fn main() -> Unit
    ! [Console.print, Tcp.sendBytes]
    match exchange()
        Result.Ok(response) -> Console.print("{{Bytes.toList(response)}}")
        Result.Err(e) -> Console.print("err:{{e}}")
"#
        ),
    )
    .expect("write probe source");

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let name = "tcp_send_bytes_probe";

    let result = (|| -> Result<(), String> {
        compile_rust(&src, &project, name, None, &[])?;
        let bin = cargo_build_in(&project, name, &isolated_target_dir(&ws))?;

        let server = std::thread::spawn(move || -> Result<Vec<u8>, String> {
            let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
            loop {
                match listener.accept() {
                    Ok((mut stream, _)) => {
                        stream
                            .set_nonblocking(false)
                            .map_err(|e| format!("set stream blocking: {e}"))?;
                        stream
                            .set_read_timeout(Some(std::time::Duration::from_secs(2)))
                            .map_err(|e| format!("set read timeout: {e}"))?;
                        let mut payload = Vec::new();
                        stream
                            .read_to_end(&mut payload)
                            .map_err(|e| format!("read payload: {e}"))?;
                        stream
                            .write_all(&payload)
                            .map_err(|e| format!("echo payload: {e}"))?;
                        return Ok(payload);
                    }
                    Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                        if std::time::Instant::now() >= deadline {
                            return Err(
                                "timed out waiting for Tcp.sendBytes connection".to_string()
                            );
                        }
                        std::thread::sleep(std::time::Duration::from_millis(5));
                    }
                    Err(e) => return Err(format!("accept connection: {e}")),
                }
            }
        });

        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("run generated binary: {e}"))?;
        let received = server
            .join()
            .map_err(|_| "loopback server panicked".to_string())??;
        if !out.status.success() {
            return Err(format!("generated binary failed:\n{}", format_output(&out)));
        }
        if received != [249, 190, 180, 217] {
            return Err(format!(
                "loopback server received wrong payload: {received:?}"
            ));
        }
        let stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if stdout != "[249, 190, 180, 217]" {
            return Err(format!(
                "generated binary returned wrong payload:\n{}",
                format_output(&out)
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

#[test]
#[ignore = "full tier: cargo build wall-time; set AVER_RUST_DIFF_FULL=1 and run with --ignored"]
fn rust_tcp_read_bytes_round_trips_non_utf8() {
    if std::env::var("AVER_RUST_DIFF_FULL").is_err() {
        eprintln!("skipping Rust Tcp.readBytes probe — set AVER_RUST_DIFF_FULL=1");
        return;
    }

    use std::io::Write;
    use std::net::TcpListener;

    let listener = TcpListener::bind("127.0.0.1:0").expect("bind loopback listener");
    let port = listener.local_addr().expect("listener addr").port();
    listener
        .set_nonblocking(true)
        .expect("set listener nonblocking");
    let server = std::thread::spawn(move || -> Result<(), String> {
        // The generated project's first cargo build happens after this thread
        // starts and can take several seconds on a cold CI runner.
        let deadline = std::time::Instant::now() + std::time::Duration::from_secs(60);
        loop {
            match listener.accept() {
                Ok((mut stream, _)) => {
                    return stream
                        .write_all(&[249, 190, 180, 217])
                        .map_err(|e| format!("write binary frame: {e}"));
                }
                Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                    if std::time::Instant::now() >= deadline {
                        return Err("timed out waiting for Tcp.readBytes connection".to_string());
                    }
                    std::thread::sleep(std::time::Duration::from_millis(5));
                }
                Err(e) => return Err(format!("accept Tcp.readBytes: {e}")),
            }
        }
    });

    let ws = temp_dir("tcp-read-bytes");
    let src = ws.join("tcp_read_bytes.av");
    fs::write(
        &src,
        format!(
            r#"module TcpReadBytesProbe
    intent = "Read non-UTF-8 bytes through the Rust backend"
    depends [Bytes]
    effects [Console, Tcp]

fn readFrame(conn: Tcp.Connection) -> Unit
    ! [Tcp.readBytes, Console.print]
    match Tcp.readBytes(conn, 4)
        Result.Ok(frame) -> Console.print("{{Bytes.toList(frame)}}")
        Result.Err(e) -> Console.print("err:{{e}}")

fn main() -> Unit
    ! [Tcp.connect, Tcp.readBytes, Console.print]
    match Tcp.connect("127.0.0.1", {port})
        Result.Ok(conn) -> readFrame(conn)
        Result.Err(e) -> Console.print("connect:{{e}}")
"#
        ),
    )
    .expect("write probe source");

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let name = "tcp_read_bytes_probe";
    let result = (|| -> Result<(), String> {
        compile_rust(&src, &project, name, None, &[])?;
        let bin = cargo_build_in(&project, name, &isolated_target_dir(&ws))?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("run generated binary: {e}"))?;
        server
            .join()
            .map_err(|_| "loopback server panicked".to_string())??;
        if !out.status.success() {
            return Err(format!("generated binary failed:\n{}", format_output(&out)));
        }
        let stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if stdout != "[249, 190, 180, 217]" {
            return Err(format!("unexpected stdout: {stdout:?}"));
        }
        Ok(())
    })();
    let _ = fs::remove_dir_all(&ws);
    result.expect("Rust Tcp.readBytes round-trip");
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

    let result = (|| -> Result<(), String> {
        // (0) PROVE the MIR path is exercised: `ask` must lower to MIR.
        let lowered = mir_lowered_count(&src, None, &["--with-replay"])?;
        if lowered == 0 {
            return Err(
                "no fn lowered to MIR — the Tcp replay reroute is not being exercised".to_string(),
            );
        }

        compile_rust_env(&src, &project, name, None, &["--with-replay"], &[])?;

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

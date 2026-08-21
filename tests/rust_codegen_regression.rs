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
    "examples/core/big_integers.av",
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

#[test]
fn rust_codegen_builds_embedded_bytes_crypto_fixture() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let source = repo_root.join("tests/fixtures/stdlib_bytes_app.av");
    let workspace = temp_output_dir("rust-codegen-stdlib-bytes-crypto");
    let project_dir = workspace.join("project");
    fs::create_dir_all(&project_dir).expect("create crypto codegen workspace");

    let compile = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("compile")
        .arg(&source)
        .arg("--module-root")
        .arg(&repo_root)
        .arg("--target")
        .arg("rust")
        .arg("-o")
        .arg(&project_dir)
        .output()
        .expect("aver compile embedded crypto fixture");
    assert!(
        compile.status.success(),
        "aver compile failed:\n{}",
        format_output(&compile)
    );

    let check = Command::new("cargo")
        .arg("check")
        .arg("--manifest-path")
        .arg(project_dir.join("Cargo.toml"))
        .env("CARGO_TARGET_DIR", shared_target_dir())
        .output()
        .expect("cargo check generated crypto project");
    assert!(
        check.status.success(),
        "generated crypto project failed cargo check:\n{}",
        format_output(&check)
    );

    let bytes_module = fs::read_to_string(project_dir.join("src/aver_generated/bytes/mod.rs"))
        .expect("read generated Bytes module");
    assert!(
        bytes_module.contains("pub values: aver_rt::AverPackedU8"),
        "stdlib Bytes should earn proof-derived U8 storage"
    );
    let entry_module = fs::read_to_string(project_dir.join("src/aver_generated/entry/mod.rs"))
        .expect("read generated crypto entry module");
    assert!(
        entry_module.contains("values.as_slice()"),
        "Crypto.sha256 should consume proof-packed Bytes without materialization"
    );

    let tests = Command::new("cargo")
        .arg("test")
        .arg("--quiet")
        .arg("--manifest-path")
        .arg(project_dir.join("Cargo.toml"))
        .env("CARGO_TARGET_DIR", shared_target_dir())
        .output()
        .expect("cargo test generated crypto project");
    assert!(
        tests.status.success(),
        "generated crypto project failed verify tests:\n{}",
        format_output(&tests)
    );

    let _ = fs::remove_dir_all(workspace);
}

#[test]
fn rust_codegen_uses_proof_derived_u8_storage_for_generic_refinement() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let source = repo_root.join("tests/fixtures/rust_packed_octets.av");
    let workspace = temp_output_dir("rust-codegen-packed-octets");
    let project_dir = workspace.join("project");
    fs::create_dir_all(&project_dir).expect("create packed-octets workspace");

    let compile = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("compile")
        .arg(&source)
        .arg("--target")
        .arg("rust")
        .arg("--name")
        .arg("packed_octets")
        .arg("-o")
        .arg(&project_dir)
        .output()
        .expect("compile generic packed refinement");
    assert!(
        compile.status.success(),
        "aver compile failed:\n{}",
        format_output(&compile)
    );

    let generated = fs::read_to_string(project_dir.join("src/aver_generated/entry/mod.rs"))
        .expect("read generated entry module");
    assert!(
        generated.contains("pub values: aver_rt::AverPackedU8"),
        "the arbitrary Octets refinement should earn packed storage:\n{generated}"
    );
    assert!(
        generated.contains(".to_int_list()"),
        "the semantic List<Int> projection should remain a zero-copy hybrid view"
    );

    let tests = Command::new("cargo")
        .arg("test")
        .arg("--quiet")
        .arg("--manifest-path")
        .arg(project_dir.join("Cargo.toml"))
        .env("CARGO_TARGET_DIR", shared_target_dir())
        .output()
        .expect("test generated packed project");
    assert!(
        tests.status.success(),
        "generated packed project failed verify tests:\n{}",
        format_output(&tests)
    );

    let run = Command::new("cargo")
        .arg("run")
        .arg("--quiet")
        .arg("--manifest-path")
        .arg(project_dir.join("Cargo.toml"))
        .env("CARGO_TARGET_DIR", shared_target_dir())
        .output()
        .expect("run generated packed project");
    assert!(
        run.status.success(),
        "generated packed project failed to run:\n{}",
        format_output(&run)
    );
    assert_eq!(String::from_utf8_lossy(&run.stdout).trim(), "5");

    let _ = fs::remove_dir_all(workspace);
}

fn sanitise(relative: &str) -> String {
    relative
        .chars()
        .map(|c| if c.is_alphanumeric() { c } else { '-' })
        .collect()
}

/// The four BOUNDARY-COMPLETENESS regressions (PR #519): valid Aver
/// programs whose emitted Rust used to fail `rustc` with `E0308` because
/// the bare-i64 unboxing analysis marked a value bare while codegen emitted
/// it (or a callee's bare result) into an `AverInt` position without the
/// `from_i64` boundary conversion. Each `(name, source)` must now emit Rust
/// that type-checks. Inline (not in the file corpus) so the regression is
/// self-documenting and travels with the test.
const UNBOX_BOUNDARY_REGRESSIONS: &[(&str, &str)] = &[
    // Q4: a bare compound `n + 1` as a Call arg to a BOXED param `keep(x)`.
    (
        "q4_call_arg_to_boxed_param",
        r#"module Q4
    intent = "bare compound in Call arg to a boxed param"
    depends []
    effects [Console.print]

fn keep(x: Int) -> Int
    x

fn down(n: Int) -> Int
    match n
        0 -> keep(n + 1)
        _ -> down(n - 1)

fn main() -> Unit
    ! [Console.print]
    Console.print("r={down(2)}")
"#,
    ),
    // Q5: a bare-return fn `g` consumed by a boxed-return fn `h`.
    (
        "q5_bare_return_into_boxed_return",
        r#"module Q5
    intent = "bare return flowing into a boxed return position"
    depends []
    effects [Console.print]

fn g(n: Int) -> Int
    match n
        0 -> 0
        _ -> g(n - 1)

fn h() -> Int
    g(2)

fn main() -> Unit
    ! [Console.print]
    Console.print("r={h()}")
"#,
    ),
    // opus Area 3 (escaping alias): `let x = n - 1; [x, x]` into an Int list.
    (
        "esc_match_let_alias_into_aggregate",
        r#"module EscM
    intent = "bare compound aliased into an Int aggregate"
    depends []
    effects [Console.print]

fn loopit(n: Int) -> List<Int>
    match n
        0 -> match n - 1
            x -> [x, x]
        _ -> loopit(n - 1)

fn main() -> Unit
    ! [Console.print]
    Console.print("r={List.len(loopit(4))}")
"#,
    ),
    // opus Area 3 (subject alias): `match n { y -> y }` returned as Int.
    (
        "subj_ret_match_binding_alias",
        r#"module SubjRet
    intent = "bare subject aliased through a match binding, returned as Int"
    depends []
    effects [Console.print]

fn loopit(n: Int) -> Int
    match n
        0 -> match n
            y -> y
        _ -> loopit(n - 1)

fn main() -> Unit
    ! [Console.print]
    Console.print("r={loopit(3)}")
"#,
    ),
    // marms: a ≥2-literal-arm bounded counter → dispatch-table guard path.
    (
        "marms_multi_literal_arm_dispatch",
        r#"module Marms
    intent = "multi base-case literal arms over a bare counter"
    depends []
    effects [Console.print]

fn loopit(n: Int, acc: Int) -> Int
    match n
        2 -> acc
        0 -> acc
        _ -> loopit(n - 1, acc + 1)

fn main() -> Unit
    ! [Console.print]
    Console.print("r={loopit(5, 0)}")
"#,
    ),
];

#[test]
fn rust_codegen_compiles_unbox_boundary_regressions() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let shared_target = shared_target_dir();
    fs::create_dir_all(&shared_target).expect("create shared target dir");

    let mut failures: Vec<String> = Vec::new();

    for (name, source) in UNBOX_BOUNDARY_REGRESSIONS {
        let workspace = temp_output_dir(&format!("rust-codegen-unbox-{name}"));
        let src_path = workspace.join(format!("{name}.av"));
        let project_dir = workspace.join("project");
        fs::create_dir_all(&workspace).expect("create per-example workspace");
        fs::write(&src_path, source).expect("write repro .av");

        let compile = Command::new(aver_bin)
            .current_dir(&repo_root)
            .arg("compile")
            .arg(&src_path)
            .arg("--target")
            .arg("rust")
            .arg("-o")
            .arg(&project_dir)
            .output()
            .expect("expected `aver compile --target rust` to spawn");
        if !compile.status.success() {
            failures.push(format!(
                "{name}: aver compile --target rust failed\n{}",
                format_output(&compile)
            ));
            let _ = fs::remove_dir_all(&workspace);
            continue;
        }

        // `cargo check` is the load-bearing assertion: `aver compile` exit 0
        // does NOT imply the emitted Rust type-checks — the whole point of
        // these regressions is that it used to emit `i64`-into-`AverInt`
        // E0308 mismatches.
        let check = Command::new("cargo")
            .arg("check")
            .arg("--manifest-path")
            .arg(project_dir.join("Cargo.toml"))
            .env("CARGO_TARGET_DIR", &shared_target)
            .output()
            .expect("expected `cargo check` to spawn");
        if !check.status.success() {
            failures.push(format!(
                "{name}: cargo check failed on emitted project (E0308 boundary regression?)\n{}",
                format_output(&check)
            ));
        }
        let _ = fs::remove_dir_all(&workspace);
    }

    assert!(
        failures.is_empty(),
        "{} of {} unbox-boundary regressions failed Rust codegen + cargo check:\n  - {}",
        failures.len(),
        UNBOX_BOUNDARY_REGRESSIONS.len(),
        failures.join("\n  - ")
    );
}

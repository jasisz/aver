use std::path::PathBuf;
use std::process::Command;
#[cfg(any(feature = "wasm", feature = "wasip2"))]
use std::time::{SystemTime, UNIX_EPOCH};

fn run_aver(args: &[&str]) -> std::process::Output {
    Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .args(args)
        .output()
        .expect("run aver")
}

fn assert_success(label: &str, output: &std::process::Output) {
    assert!(
        output.status.success(),
        "{label} failed:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

#[cfg(any(feature = "wasm", feature = "wasip2"))]
fn temp_output_dir(prefix: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    std::env::temp_dir().join(format!("{prefix}-{nanos}"))
}

#[test]
fn check_warns_when_project_module_is_shadowed_by_the_stdlib() {
    let dir = tempfile::tempdir().expect("tempdir");
    std::fs::write(
        dir.path().join("bytes.av"),
        "module Bytes\n    intent = \"project-local Bytes\"\n    exposes [fromList]\n    effects []\n\nrecord Bytes\n    values: List<Int>\n\nfn fromList(xs: List<Int>) -> Result<Bytes, String>\n    ? \"Accept anything.\"\n    Result.Ok(Bytes(values = xs))\n",
    )
    .expect("write bytes.av");
    let entry = dir.path().join("main.av");
    std::fs::write(
        &entry,
        "module Main\n    intent = \"use Bytes\"\n    depends [Bytes]\n    effects []\n\nfn byteCount(values: List<Int>) -> Result<Int, String>\n    ? \"Validate bytes and count them.\"\n    bytes = Bytes.fromList(values)?\n    Result.Ok(List.len(Bytes.toList(bytes)))\n\nverify byteCount\n    byteCount([1, 2]) => Result.Ok(2)\n    byteCount([300]) => Result.Err(\"byte value outside 0..=255\")\n",
    )
    .expect("write main.av");
    let root = dir.path().to_string_lossy().into_owned();
    let entry_path = entry.to_string_lossy().into_owned();

    let check = run_aver(&["check", &entry_path, "--module-root", &root, "--json"]);
    // Shadowing is a warning, not an error — check must still pass.
    assert_success("aver check (shadowed)", &check);
    let stdout = String::from_utf8_lossy(&check.stdout);
    assert!(stdout.contains("\"slug\":\"stdlib-shadow\""), "{stdout}");
    assert!(
        stdout.contains("reserved by the Aver standard library"),
        "{stdout}"
    );
    // The module loader also warns on stderr at load time.
    let stderr = String::from_utf8_lossy(&check.stderr);
    assert!(stderr.contains("is NOT loaded"), "{stderr}");
    assert!(stderr.contains("bytes.av"), "{stderr}");
}

/// The loader's stderr warning is deduplicated once per process per module
/// name (`source::warn_stdlib_shadow_once`), because module resolution runs
/// several times inside one command — the typecheck tree walk, the dep
/// compile walk, the per-unit check pass. Without the dedup a single
/// `aver check --deps` prints the identical paragraph four times and
/// drowns the signal it exists to carry. Counts the LOADER line only: the
/// structured `warning[stdlib-shadow]:` finding is a separate channel with
/// its own (suppressible) reporting.
#[test]
fn stdlib_shadow_loader_warning_is_printed_once_per_command() {
    let dir = tempfile::tempdir().expect("tempdir");
    std::fs::write(
        dir.path().join("bytes.av"),
        "module Bytes\n    intent = \"project-local Bytes\"\n    exposes [fromList]\n    effects []\n\nrecord Bytes\n    values: List<Int>\n\nfn fromList(xs: List<Int>) -> Result<Bytes, String>\n    ? \"Accept anything.\"\n    Result.Ok(Bytes(values = xs))\n",
    )
    .expect("write bytes.av");
    let entry = dir.path().join("main.av");
    std::fs::write(
        &entry,
        "module Main\n    intent = \"use Bytes\"\n    depends [Bytes]\n    effects []\n\nfn byteCount(values: List<Int>) -> Result<Int, String>\n    ? \"Validate bytes and count them.\"\n    bytes = Bytes.fromList(values)?\n    Result.Ok(List.len(Bytes.toList(bytes)))\n\nverify byteCount\n    byteCount([1, 2]) => Result.Ok(2)\n    byteCount([300]) => Result.Err(\"byte value outside 0..=255\")\n",
    )
    .expect("write main.av");
    let root = dir.path().to_string_lossy().into_owned();
    let entry_path = entry.to_string_lossy().into_owned();

    let check = run_aver(&["check", &entry_path, "--module-root", &root, "--deps"]);
    assert_success("aver check --deps (shadowed)", &check);
    let stderr = String::from_utf8_lossy(&check.stderr);
    let loader_lines = stderr
        .lines()
        .filter(|line| line.starts_with("warning: module 'Bytes' is reserved"))
        .count();
    assert_eq!(
        loader_lines, 1,
        "the loader's shadow warning must be emitted exactly once per process \
         per module name, across every resolution phase of one command\nstderr:\n{stderr}"
    );
}

#[test]
fn check_stays_silent_when_no_project_file_shadows_the_stdlib() {
    let dir = tempfile::tempdir().expect("tempdir");
    let entry = dir.path().join("main.av");
    std::fs::write(
        &entry,
        "module Main\n    intent = \"use Bytes\"\n    depends [Bytes]\n    effects []\n\nfn byteCount(values: List<Int>) -> Result<Int, String>\n    ? \"Validate bytes and count them.\"\n    bytes = Bytes.fromList(values)?\n    Result.Ok(List.len(Bytes.toList(bytes)))\n\nverify byteCount\n    byteCount([1, 2]) => Result.Ok(2)\n    byteCount([300]) => Result.Err(\"byte value outside 0..=255\")\n",
    )
    .expect("write main.av");
    let root = dir.path().to_string_lossy().into_owned();
    let entry_path = entry.to_string_lossy().into_owned();

    let check = run_aver(&["check", &entry_path, "--module-root", &root, "--json"]);
    assert_success("aver check (no shadow)", &check);
    let stdout = String::from_utf8_lossy(&check.stdout);
    assert!(!stdout.contains("stdlib-shadow"), "{stdout}");
    let stderr = String::from_utf8_lossy(&check.stderr);
    assert!(
        !stderr.contains("reserved by the Aver standard library"),
        "{stderr}"
    );
}

#[test]
fn embedded_bytes_module_works_outside_the_project_module_root() {
    let fixture = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/stdlib_bytes_app.av")
        .to_string_lossy()
        .into_owned();
    let missing_root = std::env::temp_dir()
        .join("aver-stdlib-no-project-modules")
        .to_string_lossy()
        .into_owned();

    let check = run_aver(&["check", &fixture, "--module-root", &missing_root, "--deps"]);
    assert_success("aver check", &check);

    let verify = run_aver(&["verify", &fixture, "--module-root", &missing_root]);
    assert_success("aver verify", &verify);

    let context = run_aver(&["context", &fixture, "--module-root", &missing_root]);
    assert_success("aver context", &context);
    let rendered = String::from_utf8_lossy(&context.stdout);
    assert!(rendered.contains("## Module: Bytes"), "{rendered}");
    assert!(rendered.contains("## Module: Digest32"), "{rendered}");
    assert!(rendered.contains("### record Digest32"), "{rendered}");
}

#[cfg(feature = "wasm")]
#[test]
fn embedded_crypto_sha256_matches_fips_vectors_on_wasm_gc() {
    let fixture = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/stdlib_bytes_app.av")
        .to_string_lossy()
        .into_owned();
    let missing_root = temp_output_dir("aver-stdlib-wasm-no-project-modules")
        .to_string_lossy()
        .into_owned();

    let verify = run_aver(&[
        "verify",
        &fixture,
        "--module-root",
        &missing_root,
        "--wasm-gc",
    ]);
    assert_success("aver verify --wasm-gc", &verify);
    let rendered = String::from_utf8_lossy(&verify.stdout);
    assert!(rendered.contains("13/13 cases passed"), "{rendered}");
}

#[cfg(feature = "wasip2")]
#[test]
fn embedded_crypto_sha256_compiles_to_valid_wasip2_component() {
    let fixture = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/stdlib_bytes_app.av")
        .to_string_lossy()
        .into_owned();
    let missing_root = temp_output_dir("aver-stdlib-wasip2-no-project-modules")
        .to_string_lossy()
        .into_owned();
    let output_dir = temp_output_dir("aver-stdlib-wasip2-output");
    let output = output_dir.to_string_lossy().into_owned();

    let compile = run_aver(&[
        "compile",
        &fixture,
        "--module-root",
        &missing_root,
        "--target",
        "wasip2",
        "-o",
        &output,
    ]);
    assert_success("aver compile --target wasip2", &compile);
    let component = output_dir.join("stdlib_bytes_app.component.wasm");
    assert!(
        std::fs::metadata(&component).is_ok_and(|m| m.len() > 0),
        "missing generated component: {}",
        component.display()
    );
    let _ = std::fs::remove_dir_all(output_dir);
}

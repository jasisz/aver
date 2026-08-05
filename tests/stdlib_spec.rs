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

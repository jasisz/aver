use std::path::PathBuf;
use std::process::Command;

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

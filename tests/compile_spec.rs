use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;

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

fn aver_string_literal(path: &Path) -> String {
    path.to_string_lossy()
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
}

fn write_runtime_policy(dir: &Path, mode: &str) {
    fs::create_dir_all(dir).expect("create runtime policy dir");
    fs::write(
        dir.join("aver.toml"),
        format!("[independence]\nmode = \"{mode}\"\n"),
    )
    .expect("write aver.toml");
}

fn marker_count(path: &Path) -> usize {
    match fs::read_to_string(path) {
        Ok(content) => content.lines().count(),
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => 0,
        Err(err) => panic!("failed to read {}: {}", path.display(), err),
    }
}

fn generated_file_mtimes(root: &Path) -> BTreeMap<PathBuf, SystemTime> {
    fn visit(root: &Path, directory: &Path, files: &mut BTreeMap<PathBuf, SystemTime>) {
        for entry in fs::read_dir(directory).expect("read generated directory") {
            let entry = entry.expect("read generated entry");
            let path = entry.path();
            let metadata = entry.metadata().expect("stat generated entry");
            if metadata.is_dir() {
                visit(root, &path, files);
            } else if metadata.is_file() {
                files.insert(
                    path.strip_prefix(root)
                        .expect("generated path below output root")
                        .to_path_buf(),
                    metadata.modified().expect("generated file mtime"),
                );
            }
        }
    }

    let mut files = BTreeMap::new();
    visit(root, root, &mut files);
    files
}

#[cfg(unix)]
fn write_fake_cargo(dir: &Path) -> PathBuf {
    let cargo = dir.join("cargo");
    fs::write(
        &cargo,
        "#!/bin/sh\nprintf '%s\\n' \"$@\" > \"$AVER_TEST_CARGO_ARGS\"\nexit \"$AVER_TEST_CARGO_STATUS\"\n",
    )
    .expect("write fake cargo");
    let mut permissions = fs::metadata(&cargo).expect("stat fake cargo").permissions();
    permissions.set_mode(0o755);
    fs::set_permissions(&cargo, permissions).expect("make fake cargo executable");
    cargo
}

#[cfg(unix)]
fn write_compile_check_probe(workspace: &Path) -> PathBuf {
    fs::create_dir_all(workspace).expect("create compile-check workspace");
    let source = workspace.join("main.av");
    fs::write(
        &source,
        r#"module Main
    intent = "Exercise the Rust compile check boundary."
    exposes [main]

fn main() -> Int
    1
"#,
    )
    .expect("write compile-check source");
    source
}

#[cfg(unix)]
#[test]
fn compile_check_runs_cargo_and_reports_success_only_after_it_passes() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let workspace = temp_output_dir("aver-compile-check-success");
    let fake_bin = workspace.join("bin");
    let output_dir = workspace.join("out");
    let args_log = workspace.join("cargo-args.txt");
    let source = write_compile_check_probe(&workspace);
    fs::create_dir_all(&fake_bin).expect("create fake bin dir");
    write_fake_cargo(&fake_bin);

    let output = Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(&repo_root)
        .arg("compile")
        .arg(&source)
        .arg("--target")
        .arg("rust")
        .arg("--check")
        .arg("-o")
        .arg(&output_dir)
        .env("PATH", &fake_bin)
        .env("AVER_TEST_CARGO_ARGS", &args_log)
        .env("AVER_TEST_CARGO_STATUS", "0")
        .output()
        .expect("run aver compile --check");

    assert!(
        output.status.success(),
        "compile --check should pass when Cargo passes:\n{}",
        format_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("Compiled"),
        "missing compile success:\n{stdout}"
    );
    assert!(
        stdout.contains("cargo check passed"),
        "missing Cargo success:\n{stdout}"
    );
    let cargo_args = fs::read_to_string(&args_log).expect("read fake Cargo args");
    assert_eq!(
        cargo_args.lines().collect::<Vec<_>>(),
        vec![
            "check",
            "--manifest-path",
            output_dir.join("Cargo.toml").to_string_lossy().as_ref()
        ]
    );

    let _ = fs::remove_dir_all(&workspace);
}

#[cfg(unix)]
#[test]
fn compile_check_propagates_cargo_failure_and_keeps_generated_project() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let workspace = temp_output_dir("aver-compile-check-failure");
    let fake_bin = workspace.join("bin");
    let output_dir = workspace.join("out");
    let args_log = workspace.join("cargo-args.txt");
    let source = write_compile_check_probe(&workspace);
    fs::create_dir_all(&fake_bin).expect("create fake bin dir");
    write_fake_cargo(&fake_bin);

    let output = Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(&repo_root)
        .arg("compile")
        .arg(&source)
        .arg("--check")
        .arg("-o")
        .arg(&output_dir)
        .env("PATH", &fake_bin)
        .env("AVER_TEST_CARGO_ARGS", &args_log)
        .env("AVER_TEST_CARGO_STATUS", "37")
        .output()
        .expect("run failing aver compile --check");

    assert_eq!(
        output.status.code(),
        Some(37),
        "compile --check should preserve Cargo's exit code:\n{}",
        format_output(&output)
    );
    assert!(
        output_dir.join("Cargo.toml").is_file(),
        "generated project should remain after Cargo failure"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        !stdout.contains("Compiled"),
        "compile success must wait for Cargo:\n{stdout}"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("`cargo check` failed"),
        "missing Cargo failure context:\n{stderr}"
    );

    let _ = fs::remove_dir_all(&workspace);
}

#[test]
fn compile_check_rejects_non_rust_target_before_codegen() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let output = Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(&repo_root)
        .arg("compile")
        .arg("examples/core/hello.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--check")
        .output()
        .expect("run aver compile --target wasm-gc --check");

    assert_eq!(output.status.code(), Some(1), "{}", format_output(&output));
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("--check requires --target rust"),
        "{}",
        format_output(&output)
    );
}

#[cfg(all(feature = "certify", feature = "wasip2"))]
#[test]
fn compile_wasip2_certify_is_rejected_instead_of_ignored() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let workspace = temp_output_dir("aver-wasip2-certify-reject");
    let output_dir = workspace.join("out");
    let output = Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(&repo_root)
        .arg("compile")
        .arg("examples/core/hello.av")
        .arg("--target")
        .arg("wasip2")
        .arg("--certify")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("run aver compile --target wasip2 --certify");

    assert_eq!(output.status.code(), Some(1), "{}", format_output(&output));
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("--target wasip2 --certify is not available yet"),
        "wrong wasip2 certify rejection:\n{}",
        format_output(&output)
    );
    assert!(
        !output_dir.join("hello.component.wasm").exists(),
        "rejected --certify must not still emit an uncertified component"
    );

    let _ = fs::remove_dir_all(&workspace);
}

#[test]
fn repeated_compile_preserves_byte_identical_generated_file_mtimes() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let workspace = temp_output_dir("aver-compile-noop-mtimes");
    let output_dir = workspace.join("out");
    fs::create_dir_all(&workspace).expect("create compile workspace");

    let compile = || {
        Command::new(env!("CARGO_BIN_EXE_aver"))
            .current_dir(&repo_root)
            .arg("compile")
            .arg("examples/core/hello.av")
            .arg("--target")
            .arg("rust")
            .arg("-o")
            .arg(&output_dir)
            .output()
            .expect("run aver compile")
    };

    let first = compile();
    assert!(
        first.status.success(),
        "initial compile failed:\n{}",
        format_output(&first)
    );
    let before = generated_file_mtimes(&output_dir);
    assert!(!before.is_empty(), "compile emitted no files");

    // Put the second materialisation beyond the coarse one-second timestamp
    // resolution still used by some filesystems. A rewriting implementation
    // must then produce a different snapshot.
    std::thread::sleep(Duration::from_millis(1_100));
    let second = compile();
    assert!(
        second.status.success(),
        "repeated compile failed:\n{}",
        format_output(&second)
    );
    assert_eq!(
        generated_file_mtimes(&output_dir),
        before,
        "byte-identical generated files must retain their mtimes"
    );

    let _ = fs::remove_dir_all(&workspace);
}

#[test]
fn compiled_rust_runtime_policy_cancel_stops_sibling_branch_early() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let append_count = 64usize;
    let workspace = temp_output_dir("aver-compile-cancel");
    let source_file = workspace.join("cancel_probe.av");
    let output_dir = workspace.join("out");
    let generated_target_dir = repo_root.join("target").join("compile-spec-generated");
    let complete_root = workspace.join("runtime-complete");
    let cancel_root = workspace.join("runtime-cancel");
    let marker_file = workspace.join("markers.txt");
    let binary_name = format!("cancel_probe{}", std::env::consts::EXE_SUFFIX);
    let generated_binary = generated_target_dir.join("debug").join(binary_name);
    let marker_literal = aver_string_literal(&marker_file);

    fs::create_dir_all(&workspace).expect("create temp workspace");
    fs::write(
        &source_file,
        format!(
            r#"module CancelProbe
    intent =
        "End-to-end probe for compiled cancel mode."
        "Complete mode must finish sibling work; cancel mode must stop early."

fn failFast() -> Result<Unit, String>
    Result.Err("boom")

fn appendMore(path: String, remaining: Int) -> Result<Unit, String>
    ? "Appends one marker, then continues."
    ! [Disk.appendText]
    _ = Disk.appendText(path, "x\n")?
    appendMany(path, remaining - 1)

fn appendMany(path: String, remaining: Int) -> Result<Unit, String>
    ? "Appends one marker per recursive step."
    ! [Disk.appendText]
    match remaining == 0
        true -> Result.Ok(Unit)
        false -> appendMore(path, remaining)

fn main() -> Result<Unit, String>
    ! [Disk.appendText]
    _ = (failFast(), appendMany("{marker_literal}", {append_count}))?!
    Result.Ok(Unit)
"#
        ),
    )
    .expect("write Aver source");

    let compile = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("compile")
        .arg(&source_file)
        .arg("-o")
        .arg(&output_dir)
        .arg("--name")
        .arg("cancel_probe")
        .arg("--policy")
        .arg("runtime")
        .output()
        .expect("expected `aver compile` to run");
    assert!(
        compile.status.success(),
        "`aver compile` failed:\n{}",
        format_output(&compile)
    );

    let manifest =
        fs::read_to_string(output_dir.join("Cargo.toml")).expect("read generated Cargo.toml");
    assert!(
        manifest.contains("aver-rt = { path = "),
        "generated Cargo.toml should pin to local aver-rt for offline tests:\n{manifest}"
    );

    let build = Command::new("cargo")
        .current_dir(&output_dir)
        .env("CARGO_TARGET_DIR", &generated_target_dir)
        .arg("build")
        .arg("-q")
        .arg("--offline")
        .output()
        .expect("expected generated cargo build to run");
    assert!(
        build.status.success(),
        "generated cargo build failed:\n{}",
        format_output(&build)
    );

    write_runtime_policy(&complete_root, "complete");
    write_runtime_policy(&cancel_root, "cancel");

    let run_complete = Command::new(&generated_binary)
        .env("AVER_REPLAY_MODULE_ROOT", &complete_root)
        .output()
        .expect("run complete-mode binary");
    assert!(
        !run_complete.status.success(),
        "complete-mode run should return the branch error:\n{}",
        format_output(&run_complete)
    );
    let complete_stderr = String::from_utf8_lossy(&run_complete.stderr);
    assert!(
        complete_stderr.contains("boom"),
        "complete-mode stderr should contain branch error, got:\n{}",
        format_output(&run_complete)
    );
    let complete_count = marker_count(&marker_file);
    assert_eq!(
        complete_count, append_count,
        "complete mode should finish all sibling work"
    );

    let _ = fs::remove_file(&marker_file);

    let run_cancel = Command::new(&generated_binary)
        .env("AVER_REPLAY_MODULE_ROOT", &cancel_root)
        .output()
        .expect("run cancel-mode binary");
    assert!(
        !run_cancel.status.success(),
        "cancel-mode run should still return the branch error:\n{}",
        format_output(&run_cancel)
    );
    let cancel_stderr = String::from_utf8_lossy(&run_cancel.stderr);
    assert!(
        cancel_stderr.contains("boom"),
        "cancel-mode stderr should preserve left-to-right error priority, got:\n{}",
        format_output(&run_cancel)
    );
    let cancel_count = marker_count(&marker_file);
    assert!(
        cancel_count <= complete_count,
        "cancel mode should not do MORE work than complete mode, got cancel_count={} vs complete_count={}",
        cancel_count,
        complete_count
    );

    let _ = fs::remove_dir_all(&workspace);
}

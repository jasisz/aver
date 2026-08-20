//! Cross-target canary for the transport-neutral native provider boundary.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

fn aver_bin() -> &'static str {
    env!("CARGO_BIN_EXE_aver")
}

fn fixture_root(name: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join(format!("tests/fixtures/{name}"))
}

fn command_report(output: &Output) -> String {
    format!(
        "stdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}

struct GeneratedProject {
    _temp: tempfile::TempDir,
    root: PathBuf,
    crate_name: String,
}

fn generate_project(fixture_name: &str, with_replay: bool) -> GeneratedProject {
    let fixture = fixture_root(fixture_name);
    let temp = tempfile::tempdir().expect("temporary generated project root");
    let generated = temp.path().join("generated");
    let mut command = Command::new(aver_bin());
    command
        .arg("compile")
        .arg(fixture.join("main.av"))
        .arg("--module-root")
        .arg(&fixture)
        .args(["--target", "rust", "-o"])
        .arg(&generated);
    if with_replay {
        command.arg("--with-replay");
    }
    let compile = command
        .output()
        .expect("compile native-provider fixture to Rust");
    assert!(compile.status.success(), "{}", command_report(&compile));

    let provider_crate = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/native_provider_host")
        .canonicalize()
        .expect("canonical provider fixture path");
    let cargo_path = generated.join("Cargo.toml");
    let cargo = fs::read_to_string(&cargo_path).expect("read generated Cargo.toml");
    let crate_name = cargo
        .lines()
        .find_map(|line| line.strip_prefix("name = \"")?.strip_suffix('"'))
        .expect("generated package name")
        .replace('-', "_");
    let cargo = cargo.replacen(
        "[dependencies]\n",
        &format!(
            "[dependencies]\nnative-provider-fixture = {{ path = {:?} }}\n",
            provider_crate
        ),
        1,
    );
    fs::write(cargo_path, cargo).expect("add the independently compiled provider crate");
    fs::create_dir_all(generated.join("src/bin")).expect("create generated host bin directory");
    GeneratedProject {
        _temp: temp,
        root: generated,
        crate_name,
    }
}

fn run_host(project: &GeneratedProject, bin: &str) -> Output {
    Command::new("cargo")
        .current_dir(&project.root)
        .args(["run", "--quiet", "--bin", bin])
        .env("CARGO_INCREMENTAL", "0")
        .output()
        .expect("build and run generated native host")
}

#[test]
fn generated_rust_accepts_the_exact_provider_crate_used_by_the_vm() {
    let project = generate_project("native_provider_clock", false);
    let generated_crate = &project.crate_name;

    let stock = Command::new("cargo")
        .current_dir(&project.root)
        .args(["run", "--quiet"])
        .env("CARGO_INCREMENTAL", "0")
        .output()
        .expect("run stock generated binary without a custom provider");
    assert!(!stock.status.success(), "{}", command_report(&stock));
    let stock_error = String::from_utf8_lossy(&stock.stderr);
    assert!(stock_error.contains("error[capability-provider-missing]"));
    assert!(stock_error.contains("Clock.now"));

    fs::write(
        project.root.join("src/bin/native_host.rs"),
        format!(
            r#"use std::sync::Arc;
use std::sync::atomic::{{AtomicUsize, Ordering}};
use {generated_crate} as generated;

fn main() {{
    let calls = Arc::new(AtomicUsize::new(0));
    generated::install_provider_bindings(vec![
        native_provider_fixture::clock_binding(calls.clone(), "generated-rust-v1"),
    ])
    .expect("install native Clock provider");
    generated::preflight_required_providers().expect("provider preflight");
    let (direct, (left, right)) = generated::aver_generated::entry::readThree();
    assert_eq!(direct.to_i64(), Some(0));
    assert_eq!(left.to_i64().unwrap() + right.to_i64().unwrap(), 3);
    assert_ne!(left, right);
    println!("{{direct}}:{{}}", calls.load(Ordering::SeqCst));
    assert_eq!(calls.load(Ordering::SeqCst), 3);
}}
"#
        ),
    )
    .expect("write generated Rust host");

    let run = run_host(&project, "native_host");
    assert!(run.status.success(), "{}", command_report(&run));
    assert_eq!(String::from_utf8_lossy(&run.stdout).trim(), "0:3");

    fs::write(
        project.root.join("src/bin/duplicate_host.rs"),
        format!(
            r#"use std::sync::Arc;
use std::sync::atomic::AtomicUsize;
use {generated_crate} as generated;

fn main() {{
    let calls = Arc::new(AtomicUsize::new(0));
    let error = generated::install_provider_bindings(vec![
        native_provider_fixture::clock_binding(calls.clone(), "first"),
        native_provider_fixture::clock_binding(calls, "second"),
    ])
    .expect_err("duplicate host binding must fail");
    assert!(error.contains("error[capability-provider-duplicate]"), "{{error}}");
}}
"#
        ),
    )
    .expect("write duplicate-provider host");
    let duplicate = run_host(&project, "duplicate_host");
    assert!(duplicate.status.success(), "{}", command_report(&duplicate));
}

#[test]
fn generated_rust_time_has_no_legacy_static_bypass() {
    let project = generate_project("native_provider_time", false);
    let generated_crate = &project.crate_name;
    fs::write(
        project.root.join("src/bin/time_without_defaults.rs"),
        format!(
            r#"use {generated_crate} as generated;

fn main() {{
    generated::install_provider_bindings_exact(vec![])
        .expect("install an intentionally empty exact provider set");
    let _ = generated::aver_generated::entry::read();
}}
"#
        ),
    )
    .expect("write generated Time fault-injection host");

    let run = run_host(&project, "time_without_defaults");
    assert!(!run.status.success(), "Time bypassed provider registry");
    let error = String::from_utf8_lossy(&run.stderr);
    assert!(error.contains("error[capability-provider-missing]"));
    assert!(error.contains("Time.now"));
}

#[test]
fn generated_rust_round_trips_result_record_and_deterministic_map() {
    let project = generate_project("native_provider_shapes", false);
    let generated_crate = &project.crate_name;
    fs::write(
        project.root.join("src/bin/shapes_host.rs"),
        format!(
            r#"use {generated_crate} as generated;

fn main() {{
    generated::install_provider_bindings(vec![native_provider_fixture::shapes_binding()])
        .expect("install native Shapes provider");
    generated::preflight_required_providers().expect("provider preflight");
    let bundle = generated::aver_generated::entry::sample().expect("Shapes.echo Result.Ok");
    let a = bundle.index.get(&aver_rt::AverStr::from("a")).expect("a value");
    let z = bundle.index.get(&aver_rt::AverStr::from("z")).expect("z value");
    println!("{{}}:{{}}:{{}}:{{}}", bundle.number, bundle.index.len(), a, z);
}}
"#
        ),
    )
    .expect("write represented-shape host");

    let run = run_host(&project, "shapes_host");
    assert!(run.status.success(), "{}", command_report(&run));
    assert_eq!(String::from_utf8_lossy(&run.stdout).trim(), "7:2:2:9");
}

#[test]
fn generated_rust_reports_wrong_shape_fault_and_panic_at_the_provider_boundary() {
    let project = generate_project("native_provider_shapes", false);
    let generated_crate = &project.crate_name;
    fs::write(
        project.root.join("src/bin/boundary_failure_host.rs"),
        format!(
            r#"use {generated_crate} as generated;

fn main() {{
    let binding = match std::env::var("FAILURE").as_deref() {{
        Ok("wrong-shape") => native_provider_fixture::wrong_shapes_binding(),
        Ok("fault") => native_provider_fixture::fault_shapes_binding(),
        Ok("panic") => native_provider_fixture::panic_shapes_binding(),
        other => panic!("unknown failure mode: {{other:?}}"),
    }};
    generated::install_provider_bindings(vec![binding]).expect("install failure provider");
    generated::preflight_required_providers().expect("provider preflight");
    let _ = generated::aver_generated::entry::sample();
}}
"#
        ),
    )
    .expect("write provider boundary failure host");

    for (mode, code, provider) in [
        (
            "wrong-shape",
            "error[capability-provider-invalid-return]",
            "example.boundary-wrong-shape@1",
        ),
        (
            "fault",
            "error[capability-provider-fault]",
            "example.boundary-fault@1",
        ),
        (
            "panic",
            "error[capability-provider-panic]",
            "example.boundary-panic@1",
        ),
    ] {
        let run = Command::new("cargo")
            .current_dir(&project.root)
            .args(["run", "--quiet", "--bin", "boundary_failure_host"])
            .env("CARGO_INCREMENTAL", "0")
            .env("FAILURE", mode)
            .output()
            .expect("run generated boundary failure host");
        assert!(!run.status.success(), "{mode} unexpectedly succeeded");
        let error = String::from_utf8_lossy(&run.stderr);
        assert!(error.contains(code), "{mode}: {error}");
        assert!(error.contains(provider), "{mode}: {error}");
        assert!(error.contains("Shapes.echo"), "{mode}: {error}");
        if mode == "wrong-shape" {
            assert!(
                error.contains("expected Result<Bundle, String>, received String"),
                "{error}"
            );
            assert!(!error.contains("private wrong value"));
        }
    }
}

#[test]
fn generated_rust_resources_survive_direct_calls_and_parallel_join() {
    let project = generate_project("native_provider_vault", false);
    let generated_crate = &project.crate_name;
    fs::write(
        project.root.join("src/bin/vault_host.rs"),
        format!(
            r#"use aver_rt::AverDisplay as _;
use {generated_crate} as generated;

fn main() {{
    generated::install_provider_bindings(vec![native_provider_fixture::vault_binding()])
        .expect("install native Vault provider");
    generated::preflight_required_providers().expect("provider preflight");
    let direct = generated::aver_generated::entry::openAndRead().expect("direct Vault read");
    let (left, right) = generated::aver_generated::entry::openPair();
    let left_token = left.expect("left token");
    let right_token = right.expect("right token");
    let boxed = generated::aver_generated::entry::boxToken(&left_token);
    let bagged = generated::aver_generated::entry::bagToken(&left_token);
    assert_eq!(boxed, boxed.clone());
    assert_eq!(bagged, bagged.clone());
    assert_eq!(boxed.aver_display(), "TokenBox(token: Vault.Token(<resource>))");
    assert_eq!(bagged.aver_display(), "Stored(Vault.Token(<resource>))");
    let left = generated::aver_generated::entry::consume(&left_token).expect("consume left token");
    let right = generated::aver_generated::entry::consume(&right_token).expect("consume right token");
    println!("{{direct}}:{{left}}:{{right}}");
}}
"#
        ),
    )
    .expect("write capability-resource host");

    let run = run_host(&project, "vault_host");
    assert!(run.status.success(), "{}", command_report(&run));
    assert_eq!(String::from_utf8_lossy(&run.stdout).trim(), "41:41:41");
}

#[test]
fn generated_rust_records_and_replays_capability_resources_without_a_live_provider() {
    let project = generate_project("native_provider_vault", true);
    let generated_crate = &project.crate_name;
    fs::write(
        project.root.join("src/bin/vault_replay_host.rs"),
        format!(
            r#"use {generated_crate} as generated;

fn main() {{
    if std::env::var_os("NO_PROVIDER").is_none() {{
        generated::install_provider_bindings(vec![native_provider_fixture::vault_binding()])
            .expect("install native Vault provider");
        generated::preflight_required_providers().expect("provider preflight");
    }}
    let value = generated::aver_replay::with_guest_scope(
        "vaultReplay",
        serde_json::Value::Null,
        generated::aver_generated::entry::openAndRead,
    )
    .expect("Vault replay result");
    println!("{{value}}");
}}
"#
        ),
    )
    .expect("write capability-resource replay host");

    let recording = project.root.join("vault-replay.json");
    let record = Command::new("cargo")
        .current_dir(&project.root)
        .args(["run", "--quiet", "--bin", "vault_replay_host"])
        .env("CARGO_INCREMENTAL", "0")
        .env("AVER_REPLAY_RECORD", &recording)
        .env("AVER_REPLAY_REQUEST_ID", "native-provider-vault")
        .env("AVER_REPLAY_TIMESTAMP", "2026-08-18T00:00:00Z")
        .output()
        .expect("record generated native provider run");
    assert!(record.status.success(), "{}", command_report(&record));
    assert_eq!(String::from_utf8_lossy(&record.stdout).trim(), "41");
    let json = fs::read_to_string(&recording).expect("read generated replay recording");
    assert!(json.contains("$capabilityResource"));
    assert!(json.contains("\"trace\": \"1\""));
    assert!(json.contains("example.vault@1"));
    assert!(!json.contains("provider-private-secret"));

    let replay = Command::new("cargo")
        .current_dir(&project.root)
        .args(["run", "--quiet", "--bin", "vault_replay_host"])
        .env("CARGO_INCREMENTAL", "0")
        .env("AVER_REPLAY_REPLAY", &recording)
        .env("AVER_REPLAY_CHECK_ARGS", "1")
        .env("NO_PROVIDER", "1")
        .output()
        .expect("replay generated native provider run");
    assert!(replay.status.success(), "{}", command_report(&replay));
    assert_eq!(
        String::from_utf8_lossy(&replay.stdout).trim(),
        "__aver_return__: {\"$ok\":41}\n41"
    );
}

#[test]
fn generated_rust_matches_the_native_provider_replay_matrix() {
    let project = generate_project("native_provider_replay", true);
    let generated_crate = &project.crate_name;
    fs::write(
        project.root.join("src/bin/replay_matrix_host.rs"),
        format!(
            r#"use std::sync::atomic::Ordering;
use {generated_crate} as generated;

fn main() {{
    let fingerprint = if std::env::var_os("CHANGED_FINGERPRINT").is_some() {{
        "replay-matrix-v2"
    }} else {{
        "replay-matrix-v1"
    }};
    let (bindings, counts) =
        native_provider_fixture::replay_bindings_with_fingerprint(fingerprint);
    generated::install_provider_bindings(bindings).expect("install replay-matrix providers");
    generated::preflight_required_providers().expect("provider preflight");
    let (pure, recorded) = generated::aver_replay::with_guest_scope(
        "replayMatrix",
        serde_json::Value::Null,
        generated::aver_generated::entry::exercise,
    );
    println!(
        "{{pure}}:{{recorded}}|{{}}:{{}}:{{}}:{{}}",
        counts.pure.load(Ordering::SeqCst),
        counts.recorded.load(Ordering::SeqCst),
        counts.suppressed.load(Ordering::SeqCst),
        counts.reissued.load(Ordering::SeqCst),
    );
}}
"#
        ),
    )
    .expect("write replay-matrix host");

    let recording = project.root.join("matrix-replay.json");
    let record = Command::new("cargo")
        .current_dir(&project.root)
        .args(["run", "--quiet", "--bin", "replay_matrix_host"])
        .env("CARGO_INCREMENTAL", "0")
        .env("AVER_REPLAY_RECORD", &recording)
        .env("AVER_REPLAY_REQUEST_ID", "native-provider-matrix")
        .env("AVER_REPLAY_TIMESTAMP", "2026-08-18T00:00:00Z")
        .output()
        .expect("record generated provider replay matrix");
    assert!(record.status.success(), "{}", command_report(&record));
    assert_eq!(
        String::from_utf8_lossy(&record.stdout).trim(),
        "10:20|1:1:1:1"
    );

    let replay = Command::new("cargo")
        .current_dir(&project.root)
        .args(["run", "--quiet", "--bin", "replay_matrix_host"])
        .env("CARGO_INCREMENTAL", "0")
        .env("AVER_REPLAY_REPLAY", &recording)
        .env("AVER_REPLAY_CHECK_ARGS", "1")
        .output()
        .expect("replay generated provider matrix");
    assert!(replay.status.success(), "{}", command_report(&replay));
    assert_eq!(
        String::from_utf8_lossy(&replay.stdout).trim(),
        "__aver_return__: {\"$tuple\":[10,20]}\n10:20|1:0:0:1"
    );

    let changed_provider = Command::new("cargo")
        .current_dir(&project.root)
        .args(["run", "--quiet", "--bin", "replay_matrix_host"])
        .env("CARGO_INCREMENTAL", "0")
        .env("AVER_REPLAY_REPLAY", &recording)
        .env("AVER_REPLAY_CHECK_ARGS", "1")
        .env("CHANGED_FINGERPRINT", "1")
        .output()
        .expect("reject changed live-provider provenance");
    assert!(!changed_provider.status.success());
    let changed_error = String::from_utf8_lossy(&changed_provider.stderr);
    assert!(changed_error.contains("Live provider mismatch"));
    assert!(changed_error.contains("replay-matrix-v1"));
    assert!(changed_error.contains("replay-matrix-v2"));

    let mut legacy: serde_json::Value =
        serde_json::from_slice(&fs::read(&recording).expect("read replay matrix recording"))
            .expect("parse replay matrix recording");
    legacy
        .as_object_mut()
        .expect("recording object")
        .remove("capabilities");
    let legacy_recording = project.root.join("matrix-replay-without-provenance.json");
    fs::write(
        &legacy_recording,
        serde_json::to_vec_pretty(&legacy).expect("encode legacy recording"),
    )
    .expect("write recording without custom capability provenance");
    let missing_provenance = Command::new("cargo")
        .current_dir(&project.root)
        .args(["run", "--quiet", "--bin", "replay_matrix_host"])
        .env("CARGO_INCREMENTAL", "0")
        .env("AVER_REPLAY_REPLAY", &legacy_recording)
        .env("AVER_REPLAY_CHECK_ARGS", "1")
        .output()
        .expect("reject custom replay event without provenance");
    assert!(!missing_provenance.status.success());
    let provenance_error = String::from_utf8_lossy(&missing_provenance.stderr);
    assert!(provenance_error.contains("no capability contract/model provenance"));
}

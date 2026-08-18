//! End-to-end contract for the explicit cached provider VM host.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

const SHAPES_SOURCE: &str = include_str!("fixtures/native_provider_composed/Shapes.av");
const MAIN_SOURCE: &str = include_str!("fixtures/native_provider_composed/main.av");
const PROVIDER_SOURCE: &str = include_str!("fixtures/native_provider_host/src/lib.rs");

fn aver_bin() -> &'static str {
    env!("CARGO_BIN_EXE_aver")
}

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn report(output: &Output) -> String {
    format!(
        "stdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}

fn run_aver(cache: &Path, args: &[&str]) -> Output {
    Command::new(aver_bin())
        .args(args)
        .env("AVER_PROVIDER_HOST_CACHE", cache)
        .env("CARGO_NET_OFFLINE", "true")
        // Hermetic tests should not depend on a developer's compiler wrapper
        // socket being reachable from the child provider build.
        .env_remove("RUSTC_WRAPPER")
        .env_remove("RUSTC_WORKSPACE_WRAPPER")
        .output()
        .expect("run aver provider-host command")
}

fn provider_manifest(provider_root: &Path, factory: &str) -> String {
    format!(
        "[providers]\nschema = 1\n\n[[providers.bindings]]\ncapability = \"Shapes\"\ncrate = \"native_provider_fixture\"\npackage = \"native-provider-fixture\"\npath = {:?}\nfactory = \"{factory}\"\n",
        provider_root.to_string_lossy()
    )
}

fn write_app(root: &Path, provider_root: &Path) {
    fs::create_dir_all(root).expect("create app root");
    fs::write(root.join("Shapes.av"), SHAPES_SOURCE).expect("write capability fixture");
    fs::write(root.join("main.av"), MAIN_SOURCE).expect("write entry fixture");
    fs::write(
        root.join("aver.toml"),
        provider_manifest(provider_root, "counted_shapes_binding"),
    )
    .expect("write provider manifest");
}

fn command_args<'a>(command: &'a str, app: &'a Path) -> Vec<String> {
    vec![
        command.to_string(),
        app.join("main.av").to_string_lossy().into_owned(),
        "--module-root".to_string(),
        app.to_string_lossy().into_owned(),
        "--providers".to_string(),
    ]
}

fn run_owned_args(cache: &Path, args: &[String]) -> Output {
    let refs = args.iter().map(String::as_str).collect::<Vec<_>>();
    run_aver(cache, &refs)
}

#[test]
fn configured_packages_run_and_verify_on_one_cached_vm_host() {
    let temp = tempfile::tempdir().expect("provider-host test root");
    let cache = temp.path().join("cache");
    let app = temp.path().join("app");
    let provider = repo_root().join("tests/fixtures/native_provider_host");
    write_app(&app, &provider);

    // Plain VM execution is inert even with a manifest: no Cargo and no
    // provider package execution. The missing-provider error gives the exact
    // explicit opt-in instead.
    let mut plain = command_args("run", &app);
    plain.pop();
    let plain = Command::new(aver_bin())
        .args(&plain)
        .env("CARGO", "/definitely/missing/cargo")
        .output()
        .expect("run plain VM without Cargo");
    assert!(!plain.status.success(), "plain run unexpectedly passed");
    let plain_report = report(&plain);
    assert!(plain_report.contains("capability-provider-missing"));
    assert!(plain_report.contains("aver run"));
    assert!(plain_report.contains("--providers"));
    assert!(!plain_report.contains("failed to start Cargo"));

    let mut plain_verify = command_args("verify", &app);
    plain_verify.pop();
    let plain_verify = Command::new(aver_bin())
        .args(&plain_verify)
        .env("CARGO", "/definitely/missing/cargo")
        .output()
        .expect("run plain VM verify without Cargo");
    assert!(
        !plain_verify.status.success(),
        "plain verify unexpectedly passed"
    );
    let plain_verify_report = report(&plain_verify);
    assert!(plain_verify_report.contains("aver verify"));
    assert!(plain_verify_report.contains("--providers"));
    assert!(plain_verify_report.contains("given echo: Shapes.echo = [stub]"));
    assert!(!plain_verify_report.contains("failed to start Cargo"));

    let first = run_owned_args(&cache, &command_args("run", &app));
    assert!(first.status.success(), "{}", report(&first));
    assert!(report(&first).contains("Building provider host"));

    // The fixture has four blocks: real pure provider, exact local given,
    // real provider again, and two independent branch VMs. Its counted factory
    // faults every call unless it was constructed exactly once in the process.
    let verify = run_owned_args(&cache, &command_args("verify", &app));
    assert!(verify.status.success(), "{}", report(&verify));
    let verify_report = report(&verify);
    assert!(verify_report.contains("4/4 cases passed"));
    assert!(!verify_report.contains("provider host"));

    // CLI options, program args, recording, and provider provenance all cross
    // the host process boundary unchanged.
    let recordings = temp.path().join("recordings");
    let mut recorded_args = command_args("run", &app);
    recorded_args.extend([
        "--record".to_string(),
        recordings.to_string_lossy().into_owned(),
        "--expr".to_string(),
        "expectFirstArg()".to_string(),
        "--".to_string(),
        "host-boundary".to_string(),
    ]);
    let recorded = run_owned_args(&cache, &recorded_args);
    assert!(recorded.status.success(), "{}", report(&recorded));
    assert!(!report(&recorded).contains("provider host"));
    let recording = fs::read_to_string(recordings.join("expectFirstArg.json"))
        .expect("provider-host recording");
    assert!(recording.contains("\"entry_fn\": \"expectFirstArg\""));
    assert!(recording.contains("\"type\": \"Args.get\""));
    assert!(recording.contains("\"host-boundary\""));
    assert!(recording.contains("example.counted-shapes-echo@1"));

    // Program text is deliberately outside the host key and local dependency
    // stamp: edit it and reuse the already-linked host directly.
    fs::write(
        app.join("main.av"),
        format!("{MAIN_SOURCE}\n// host cache ignores Aver source edits\n"),
    )
    .expect("edit Aver program");
    let after_aver_edit = run_owned_args(&cache, &command_args("run", &app));
    assert!(
        after_aver_edit.status.success(),
        "{}",
        report(&after_aver_edit)
    );
    assert!(!report(&after_aver_edit).contains("provider host"));

    // A copied local provider gets its own deterministic host project. Editing
    // its Rust source keeps that project but asks Cargo for an incremental
    // rebuild, rather than silently launching the stale binary.
    let copied_provider = temp.path().join("provider");
    fs::create_dir_all(copied_provider.join("src")).expect("create copied provider");
    fs::write(copied_provider.join("src/lib.rs"), PROVIDER_SOURCE).expect("copy provider source");
    fs::write(
        copied_provider.join("Cargo.toml"),
        format!(
            "[package]\nname = \"native-provider-fixture\"\nversion = \"0.1.0\"\nedition = \"2024\"\npublish = false\n\n[dependencies]\naver-rt = {{ path = {:?} }}\n",
            repo_root().join("aver-rt").to_string_lossy()
        ),
    )
    .expect("write copied provider manifest");
    fs::write(
        app.join("aver.toml"),
        provider_manifest(&copied_provider, "counted_shapes_binding"),
    )
    .expect("point app at copied provider");

    let copied_first = run_owned_args(&cache, &command_args("run", &app));
    assert!(copied_first.status.success(), "{}", report(&copied_first));
    assert!(report(&copied_first).contains("Building provider host"));
    fs::write(
        copied_provider.join("src/lib.rs"),
        format!("{PROVIDER_SOURCE}\n// local provider edit\n"),
    )
    .expect("edit copied provider");
    let copied_rebuild = run_owned_args(&cache, &command_args("run", &app));
    assert!(
        copied_rebuild.status.success(),
        "{}",
        report(&copied_rebuild)
    );
    assert!(report(&copied_rebuild).contains("Rebuilding provider host"));

    // Factory resolution and return typing stay owned by Rust/Cargo, with the
    // provider-host boundary preserving the compiler's concrete diagnostic.
    fs::write(
        app.join("aver.toml"),
        provider_manifest(&copied_provider, "not_a_binding"),
    )
    .expect("select wrong-return factory");
    let wrong_factory = run_owned_args(&cache, &command_args("run", &app));
    assert!(
        !wrong_factory.status.success(),
        "wrong factory unexpectedly ran"
    );
    let wrong_factory_report = report(&wrong_factory);
    assert!(wrong_factory_report.contains("expected `ProviderBinding`, found `usize`"));
    assert!(wrong_factory_report.contains("provider host Cargo build failed"));
}

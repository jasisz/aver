//! End-to-end contract for the cached provider VM host.
//!
//! A project that binds providers under `[providers]` in its `aver.toml` has
//! said what its programs mean: `run`, `verify` and `audit` build the host
//! once and reuse it whenever a program reaches a bound capability, with no
//! flag and no prompt. Programs that reach no bound capability run in
//! process, and backends with no provider host refuse instead of running
//! without the provider.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

const SHAPES_SOURCE: &str = include_str!("fixtures/native_provider_composed/Shapes.av");
const MAIN_SOURCE: &str = include_str!("fixtures/native_provider_composed/main.av");
const INDEPENDENT_SOURCE: &str = include_str!("fixtures/native_provider_composed/independent.av");
const PROVIDER_SOURCE: &str = include_str!("fixtures/native_provider_host/src/lib.rs");
const PROBE_SOURCE: &str = "module Probe\n    intent = \"Exercise an entry program smaller than the project manifest.\"\n\nfn main() -> Unit\n    Unit\n";

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

/// Run with Cargo unreachable: proves the command never tried to build.
fn run_without_cargo(cache: &Path, args: &[&str]) -> Output {
    Command::new(aver_bin())
        .args(args)
        .env("AVER_PROVIDER_HOST_CACHE", cache)
        .env("CARGO", "/definitely/missing/cargo")
        .output()
        .expect("run aver without Cargo")
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
    fs::write(root.join("independent.av"), INDEPENDENT_SOURCE)
        .expect("write independent verify fixture");
    fs::write(root.join("probe.av"), PROBE_SOURCE).expect("write independent run fixture");
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
    ]
}

fn run_owned_args(cache: &Path, args: &[String]) -> Output {
    let refs = args.iter().map(String::as_str).collect::<Vec<_>>();
    run_aver(cache, &refs)
}

#[test]
fn bound_providers_refuse_backends_without_a_host() {
    let temp = tempfile::tempdir().expect("provider-host test root");
    let cache = temp.path().join("cache");
    let app = temp.path().join("app");
    let provider = repo_root().join("tests/fixtures/native_provider_host");
    write_app(&app, &provider);
    let main_path = app.join("main.av").to_string_lossy().into_owned();
    let module_root = app.to_string_lossy().into_owned();

    // The Shapes contract carries `Bundle`, which the WIT boundary cannot
    // lower, so wasip2 refuses the binding before anything is built.
    for (command, backend, flag) in [
        ("verify", "wasm-gc", "--wasm-gc"),
        ("run", "wasm-gc", "--wasm-gc"),
        ("run", "self-host", "--self-host"),
        ("run", "wasip2", "--wasip2"),
    ] {
        let refused = run_without_cargo(
            &cache,
            &[command, &main_path, "--module-root", &module_root, flag],
        );
        assert!(
            !refused.status.success(),
            "{command} {flag} ran with an unhosted provider:\n{}",
            report(&refused)
        );
        let text = report(&refused);
        assert!(
            text.contains("error[capability-provider-unhosted]"),
            "{text}"
        );
        assert!(
            text.contains(&format!(
                "the {backend} backend cannot host a Rust provider"
            )),
            "{text}"
        );
        assert!(
            text.contains("capability 'Shapes' -> package 'native-provider-fixture' from"),
            "{text}"
        );
        assert!(text.contains("native_provider_host"), "{text}");
        assert!(
            text.contains(&format!(
                "`aver {command} {main_path} --module-root {module_root}`"
            )),
            "{text}"
        );
        assert!(text.contains("[providers] in aver.toml"), "{text}");
        assert!(!text.contains("capability-provider-missing"), "{text}");
        assert!(!text.contains("Building provider host"), "{text}");
        assert!(!text.contains("failed to start Cargo"), "{text}");
    }

    // A directory input is repeated as such in the repair command, not
    // replaced by whichever file resolved first.
    let refused = run_without_cargo(
        &cache,
        &[
            "verify",
            &module_root,
            "--module-root",
            &module_root,
            "--wasm-gc",
        ],
    );
    assert!(!refused.status.success(), "{}", report(&refused));
    let text = report(&refused);
    assert!(
        text.contains(&format!(
            "`aver verify {module_root} --module-root {module_root}`"
        )),
        "{text}"
    );

    // A program that reaches no bound capability is free to use any backend.
    let probe_path = app.join("probe.av").to_string_lossy().into_owned();
    let probe = run_without_cargo(
        &cache,
        &[
            "run",
            &probe_path,
            "--module-root",
            &module_root,
            "--self-host",
        ],
    );
    let text = report(&probe);
    assert!(!text.contains("capability-provider-unhosted"), "{text}");
    assert!(!text.contains("failed to start Cargo"), "{text}");
}

#[test]
fn configured_packages_run_and_verify_on_one_cached_vm_host() {
    let temp = tempfile::tempdir().expect("provider-host test root");
    let cache = temp.path().join("cache");
    let app = temp.path().join("app");
    let provider = repo_root().join("tests/fixtures/native_provider_host");
    write_app(&app, &provider);
    let module_root = app.to_string_lossy().into_owned();
    let main_path = app.join("main.av").to_string_lossy().into_owned();

    // Without a `[providers]` table nothing is built or executed: the
    // missing-provider error points at the table, not at a flag.
    fs::write(app.join("aver.toml"), "[project]\nname = \"composed\"\n").expect("drop manifest");
    let plain = run_without_cargo(&cache, &["run", &main_path, "--module-root", &module_root]);
    assert!(!plain.status.success(), "plain run unexpectedly passed");
    let plain_report = report(&plain);
    assert!(plain_report.contains("capability-provider-missing"));
    assert!(plain_report.contains("[[providers.bindings]]"));
    assert!(!plain_report.contains("--providers"));
    assert!(!plain_report.contains("failed to start Cargo"));

    let plain_verify = run_without_cargo(
        &cache,
        &["verify", &main_path, "--module-root", &module_root],
    );
    assert!(
        !plain_verify.status.success(),
        "plain verify unexpectedly passed"
    );
    let plain_verify_report = report(&plain_verify);
    assert!(plain_verify_report.contains("[[providers.bindings]]"));
    assert!(plain_verify_report.contains("given echo: Shapes.echo = [stub]"));
    assert!(!plain_verify_report.contains("--providers"));
    assert!(!plain_verify_report.contains("failed to start Cargo"));
    fs::write(
        app.join("aver.toml"),
        provider_manifest(&provider, "counted_shapes_binding"),
    )
    .expect("restore provider manifest");

    // The `[providers]` table is the consent: the first run builds the host
    // and says which package it builds and where that package lives.
    let first = run_owned_args(&cache, &command_args("run", &app));
    assert!(first.status.success(), "{}", report(&first));
    let first_report = report(&first);
    assert!(
        first_report.contains("Building provider host for Shapes: native-provider-fixture from "),
        "{first_report}"
    );
    assert!(first_report.contains("native_provider_host (cached at "));

    // `aver.toml` describes the project, not only main.av. Shapes is known
    // elsewhere in this module root but inactive for this smaller program,
    // so its binding must not make a valid probe un-runnable — and no host
    // is involved for it at all.
    let probe_path = app.join("probe.av").to_string_lossy().into_owned();
    let probe = run_without_cargo(&cache, &["run", &probe_path, "--module-root", &module_root]);
    assert!(probe.status.success(), "{}", report(&probe));
    assert!(!report(&probe).contains("no capability contract"));
    assert!(!report(&probe).contains("provider host"));

    // Inactivity is not a typo exemption: a manifest capability that cannot
    // resolve to any project contract still fails before Cargo or host code.
    let valid_manifest = provider_manifest(&provider, "counted_shapes_binding");
    let unknown_manifest =
        valid_manifest.replacen("capability = \"Shapes\"", "capability = \"Missing\"", 1);
    fs::write(app.join("aver.toml"), unknown_manifest).expect("select unknown binding");
    let unknown_probe =
        run_without_cargo(&cache, &["run", &probe_path, "--module-root", &module_root]);
    assert!(!unknown_probe.status.success(), "unknown binding passed");
    let unknown_report = report(&unknown_probe);
    assert!(
        unknown_report.contains("capability 'Missing' has no capability contract in this project")
    );
    assert!(!unknown_report.contains("provider host"));
    fs::write(app.join("aver.toml"), valid_manifest).expect("restore valid provider binding");

    // The fixture has four blocks: real pure provider, exact local given,
    // real provider again, and two independent branch VMs. Its counted factory
    // faults every call unless it was constructed exactly once in the process.
    // The program's other module, Shapes, is a capability with no blocks.
    let verify = run_owned_args(&cache, &command_args("verify", &app));
    assert!(verify.status.success(), "{}", report(&verify));
    let verify_report = report(&verify);
    assert!(verify_report.contains("4/4 cases passed"));
    assert!(verify_report.contains("Summary: 1 module"));
    assert!(!verify_report.contains("provider host"));

    // A project-wide provider host carries the union of configured bindings,
    // while each verify module has its own capability registry. A binding
    // used by main.av must be ignored for independent.av, not mislabeled as
    // a type error or used as a reason to skip that module.
    let project_path = app.to_string_lossy().into_owned();
    let project_verify = run_aver(
        &cache,
        &["verify", &project_path, "--module-root", &module_root],
    );
    assert!(
        project_verify.status.success(),
        "{}",
        report(&project_verify)
    );
    let project_verify_report = report(&project_verify);
    assert!(project_verify_report.contains("5/5 cases passed"));
    assert!(!project_verify_report.contains("skipped — type errors"));
    assert!(!project_verify_report.contains("unknown capability"));

    let independent_path = app.join("independent.av").to_string_lossy().into_owned();
    let independent_verify = run_without_cargo(
        &cache,
        &["verify", &independent_path, "--module-root", &module_root],
    );
    assert!(
        independent_verify.status.success(),
        "{}",
        report(&independent_verify)
    );
    let independent_verify_report = report(&independent_verify);
    assert!(independent_verify_report.contains("1/1 cases passed"));
    assert!(!independent_verify_report.contains("unused provider binding"));
    assert!(!independent_verify_report.contains("unknown capability"));
    assert!(!independent_verify_report.contains("provider host"));

    // Audit is the one-command project gate, so its verify phase uses the
    // same bindings.
    let project_audit = run_aver(
        &cache,
        &["audit", &project_path, "--module-root", &module_root],
    );
    assert!(project_audit.status.success(), "{}", report(&project_audit));
    let project_audit_report = report(&project_audit);
    assert!(project_audit_report.contains("Audit: 4 files"));
    assert!(project_audit_report.contains("verify identity"));

    // A real provider setup failure is not a source type error, and audit
    // must not swallow it into an empty verify summary.
    fs::write(
        app.join("aver.toml"),
        provider_manifest(&provider, "mismatched_shapes_binding"),
    )
    .expect("select mismatched provider binding");
    let mismatched_verify = run_aver(
        &cache,
        &["verify", &project_path, "--module-root", &module_root],
    );
    assert!(!mismatched_verify.status.success());
    let mismatched_verify_report = report(&mismatched_verify);
    assert!(mismatched_verify_report.contains("skipped — provider composition error"));
    assert!(!mismatched_verify_report.contains("skipped — type errors"));

    let mismatched_audit = run_aver(
        &cache,
        &["audit", &project_path, "--module-root", &module_root],
    );
    assert!(!mismatched_audit.status.success());
    let mismatched_audit_report = report(&mismatched_audit);
    assert!(mismatched_audit_report.contains("[verify-provider-setup]"));
    assert!(mismatched_audit_report.contains("provider composition could not be installed"));
    assert!(mismatched_audit_report.contains("0 check errors | 1 verify failures"));

    fs::write(
        app.join("aver.toml"),
        provider_manifest(&provider, "counted_shapes_binding"),
    )
    .expect("restore valid provider binding");

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
    // rebuild, rather than silently launching the stale binary. The notice
    // names the package relative to the project.
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
    assert!(
        report(&copied_first).contains(
            "Building provider host for Shapes: native-provider-fixture from ../provider (cached at "
        ),
        "{}",
        report(&copied_first)
    );
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
    assert!(
        report(&copied_rebuild).contains(
            "Rebuilding provider host for Shapes: native-provider-fixture from ../provider"
        ),
        "{}",
        report(&copied_rebuild)
    );

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

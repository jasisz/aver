//! End-to-end contract for explicit native provider package composition.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

const SHAPES_SOURCE: &str = include_str!("fixtures/native_provider_composed/Shapes.av");
const MAIN_SOURCE: &str = include_str!("fixtures/native_provider_composed/main.av");

fn aver_bin() -> &'static str {
    env!("CARGO_BIN_EXE_aver")
}

fn fixture_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/native_provider_composed")
}

fn provider_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/native_provider_host")
        .canonicalize()
        .expect("canonical provider fixture")
}

fn report(output: &Output) -> String {
    format!(
        "stdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}

fn compile(module_root: &Path, output_dir: &Path) -> Output {
    Command::new(aver_bin())
        .arg("compile")
        .arg(module_root.join("main.av"))
        .arg("--module-root")
        .arg(module_root)
        .args(["--target", "rust", "-o"])
        .arg(output_dir)
        .output()
        .expect("compile provider-composed Rust project")
}

fn cargo(project: &Path, target_dir: &Path, action: &str) -> Output {
    Command::new("cargo")
        .current_dir(project)
        .args([action, "--offline", "--quiet"])
        .env("CARGO_INCREMENTAL", "0")
        .env("CARGO_TARGET_DIR", target_dir)
        .output()
        .expect("run Cargo for generated project")
}

fn write_project(root: &Path, main: &str, manifest: &str) {
    fs::create_dir_all(root).expect("create module root");
    fs::write(root.join("Shapes.av"), SHAPES_SOURCE).expect("write Shapes capability");
    fs::write(root.join("main.av"), main).expect("write Aver entry");
    fs::write(root.join("aver.toml"), manifest).expect("write provider manifest");
}

fn local_manifest(capability: &str, factory: &str, path: &Path) -> String {
    format!(
        "[providers]\nschema = 1\n\n[[providers.bindings]]\ncapability = \"{capability}\"\ncrate = \"native_provider_fixture\"\npackage = \"native-provider-fixture\"\npath = \"{}\"\nfactory = \"{factory}\"\n",
        path.display()
    )
}

#[test]
fn local_manifest_is_the_complete_stock_binary_workflow() {
    let temp = tempfile::tempdir().expect("temporary generated project root");
    let generated = temp.path().join("generated");
    let output = compile(&fixture_root(), &generated);
    assert!(output.status.success(), "{}", report(&output));

    let cargo_toml =
        fs::read_to_string(generated.join("Cargo.toml")).expect("generated Cargo.toml");
    let provider_path = provider_root();
    assert!(
        cargo_toml
            .contains("native_provider_fixture = { package = \"native-provider-fixture\", path = ")
    );
    assert!(cargo_toml.contains(&provider_path.to_string_lossy().to_string()));

    let main_rs = fs::read_to_string(generated.join("src/main.rs")).expect("generated main.rs");
    let bootstrap = main_rs
        .split_once("fn bootstrap_provider_bindings()")
        .expect("generated provider bootstrap")
        .1;
    let install = bootstrap
        .find("provider_support::install_provider_bindings")
        .expect("provider installation");
    let factory = bootstrap
        .find("native_provider_fixture::counted_shapes_binding()")
        .expect("typed provider factory call");
    let preflight = bootstrap
        .find("provider_support::preflight_required_providers()")
        .expect("provider preflight");
    assert!(install < factory && factory < preflight);
    assert_eq!(
        main_rs.matches("install_provider_bindings(vec![").count(),
        1
    );

    let run = cargo(&generated, &temp.path().join("cargo-target"), "run");
    assert!(run.status.success(), "{}", report(&run));
}

#[test]
fn registry_version_is_emitted_without_resolving_or_running_cargo() {
    let temp = tempfile::tempdir().expect("temporary registry manifest root");
    let root = temp.path().join("app");
    write_project(
        &root,
        MAIN_SOURCE,
        "[providers]\nschema = 1\n\n[[providers.bindings]]\ncapability = \"Shapes\"\ncrate = \"shapes_provider\"\npackage = \"aver-shapes-provider\"\nversion = \"=999.0.0\"\nfactory = \"binding\"\n",
    );
    let generated = temp.path().join("generated");
    let output = compile(&root, &generated);
    assert!(output.status.success(), "{}", report(&output));
    let cargo_toml =
        fs::read_to_string(generated.join("Cargo.toml")).expect("generated Cargo.toml");
    assert!(cargo_toml.contains(
        "shapes_provider = { package = \"aver-shapes-provider\", version = \"=999.0.0\" }"
    ));
    assert!(
        !generated.join("Cargo.lock").exists(),
        "aver compile must not run Cargo"
    );
}

#[test]
fn compile_rejects_missing_unknown_unused_and_unresolvable_bindings() {
    let cases = [
        (
            MAIN_SOURCE,
            "[providers]\nschema = 1\n",
            "missing required custom capability binding: Shapes",
        ),
        (
            MAIN_SOURCE,
            "[providers]\nschema = 1\n[[providers.bindings]]\ncapability='Missing'\ncrate='missing_provider'\npackage='missing-provider'\nfactory='binding'\nversion='1'\n",
            "capability 'Missing' has no capability contract in this program",
        ),
        (
            "module App\n    depends [Shapes]\n\nfn main() -> Unit\n    Unit\n",
            "[providers]\nschema = 1\n[[providers.bindings]]\ncapability='Shapes'\ncrate='shapes_provider'\npackage='shapes-provider'\nfactory='binding'\nversion='1'\n",
            "capability 'Shapes' is not used by this program",
        ),
        (
            MAIN_SOURCE,
            "[providers]\nschema = 1\n[[providers.bindings]]\ncapability='Shapes'\ncrate='shapes_provider'\npackage='shapes-provider'\nfactory='binding'\npath='missing-provider'\n",
            "local provider path 'missing-provider' does not exist or cannot be resolved",
        ),
    ];

    for (index, (main, manifest, expected)) in cases.into_iter().enumerate() {
        let temp = tempfile::tempdir().expect("temporary invalid manifest root");
        let root = temp.path().join(format!("app-{index}"));
        write_project(&root, main, manifest);
        let output = compile(&root, &temp.path().join("generated"));
        assert!(
            !output.status.success(),
            "case {index} unexpectedly compiled"
        );
        let output = report(&output);
        assert!(output.contains("aver.toml"), "case {index}: {output}");
        assert!(
            output.contains(expected),
            "case {index}: expected {expected:?} in {output}"
        );
    }
}

#[test]
fn rust_compiler_reports_missing_factory_and_wrong_return_type() {
    let temp = tempfile::tempdir().expect("temporary invalid factory root");
    let target = temp.path().join("cargo-target");
    for (index, (factory, expected)) in [
        ("does_not_exist", "does_not_exist"),
        ("not_a_binding", "expected `ProviderBinding`, found `usize`"),
    ]
    .into_iter()
    .enumerate()
    {
        let root = temp.path().join(format!("app-{index}"));
        write_project(
            &root,
            MAIN_SOURCE,
            &local_manifest("Shapes", factory, &provider_root()),
        );
        let generated = temp.path().join(format!("generated-{index}"));
        let output = compile(&root, &generated);
        assert!(output.status.success(), "{}", report(&output));
        let check = cargo(&generated, &target, "check");
        assert!(
            !check.status.success(),
            "factory {factory} unexpectedly compiled"
        );
        assert!(
            report(&check).contains(expected),
            "factory {factory}: {}",
            report(&check)
        );
    }
}

#[test]
fn provider_contract_mismatch_fails_at_stock_binary_bootstrap() {
    let temp = tempfile::tempdir().expect("temporary mismatched provider root");
    let root = temp.path().join("app");
    write_project(
        &root,
        MAIN_SOURCE,
        &local_manifest("Shapes", "mismatched_shapes_binding", &provider_root()),
    );
    let generated = temp.path().join("generated");
    let output = compile(&root, &generated);
    assert!(output.status.success(), "{}", report(&output));
    let run = cargo(&generated, &temp.path().join("cargo-target"), "run");
    assert!(
        !run.status.success(),
        "mismatched provider unexpectedly started"
    );
    let output = report(&run);
    assert!(
        output.contains("error[capability-provider-mismatch]"),
        "{output}"
    );
    assert!(output.contains("Shapes"), "{output}");
}

#[test]
fn explicit_time_package_replaces_the_compiler_default() {
    let temp = tempfile::tempdir().expect("temporary Time override root");
    let root = temp.path().join("app");
    fs::create_dir_all(&root).expect("create Time module root");
    fs::write(
        root.join("main.av"),
        "module TimeOverride\n    depends [Time]\n    effects [Time.now]\n\nfn main() -> Result<Unit, String>\n    ! [Time.now]\n    value = Time.now()\n    match value\n        \"fixed-time\" -> Result.Ok(Unit)\n        _ -> Result.Err(\"configured Time provider was not installed\")\n",
    )
    .expect("write Time override entry");
    fs::write(
        root.join("aver.toml"),
        local_manifest("Time", "fixed_time_binding", &provider_root()),
    )
    .expect("write Time provider manifest");

    let generated = temp.path().join("generated");
    let output = compile(&root, &generated);
    assert!(output.status.success(), "{}", report(&output));
    let run = cargo(&generated, &temp.path().join("cargo-target"), "run");
    assert!(run.status.success(), "{}", report(&run));
}

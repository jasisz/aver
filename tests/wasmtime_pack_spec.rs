//! End-to-end deployment contract for `--pack wasmtime`.
//!
//! This intentionally exercises generated native executables, not only
//! manifest helpers: one standard-provider program and one custom Rust
//! provider program must run with no toolchain reachable on the destination.

#![cfg(feature = "wasm")]

#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use aver_cmd::{aver_bin, repo_root};

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

const SHAPES_SOURCE: &str = include_str!("fixtures/native_provider_composed/Shapes.av");
const CUSTOM_SOURCE: &str = include_str!("fixtures/native_provider_composed/wasm.av");
const STANDARD_SOURCE: &str = r#"module StandardPack
    intent = "Prove a compiler-shipped provider travels in the Wasmtime pack."
    effects [Console.print]

fn main() -> Unit
    ! [Console.print]
    Console.print("standard-pack-ok")
"#;

fn report(output: &Output) -> String {
    format!(
        "stdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}

fn compile_pack(
    cache: &Path,
    source: &Path,
    module_root: &Path,
    output_dir: &Path,
    cargo: Option<&Path>,
) -> Output {
    let mut command = Command::new(aver_bin());
    command
        .arg("compile")
        .arg(source)
        .arg("--module-root")
        .arg(module_root)
        .arg("--target")
        .arg("wasm-gc")
        .arg("--pack")
        .arg("wasmtime")
        .arg("-o")
        .arg(output_dir)
        .env("AVER_PROVIDER_HOST_CACHE", cache)
        .env("CARGO_NET_OFFLINE", "true")
        .env_remove("RUSTC_WRAPPER")
        .env_remove("RUSTC_WORKSPACE_WRAPPER");
    if let Some(cargo) = cargo {
        command.env("CARGO", cargo);
    }
    command.output().expect("compile Wasmtime pack")
}

fn run_host(pack: &Path) -> Output {
    let host = pack.join(format!(
        "aver-wasmtime-host{}",
        std::env::consts::EXE_SUFFIX
    ));
    Command::new(host)
        .current_dir(pack)
        .env_clear()
        .env("PATH", "/definitely/missing")
        .output()
        .expect("run packed host")
}

fn write_custom_project(root: &Path, provider: &Path) -> PathBuf {
    fs::create_dir_all(root).expect("create custom project");
    fs::write(root.join("Shapes.av"), SHAPES_SOURCE).expect("write Shapes capability");
    let source = CUSTOM_SOURCE
        .replace("effects []", "effects [Console.print]")
        .replace(
            "    bundle = Shapes.Bundle",
            "    ! [Console.print]\n    bundle = Shapes.Bundle",
        )
        .replace(
            "    match echoed.flag",
            "    Console.print(\"custom-pack-ok\")\n    match echoed.flag",
        );
    let entry = root.join("main.av");
    fs::write(&entry, source).expect("write custom entry");
    fs::write(
        root.join("aver.toml"),
        format!(
            "[providers]\nschema = 1\n\n[[providers.bindings]]\ncapability = \"Shapes\"\ncrate = \"native_provider_fixture\"\npackage = \"native-provider-fixture\"\npath = {:?}\nfactory = \"counted_shapes_binding\"\n",
            provider.to_string_lossy()
        ),
    )
    .expect("write provider manifest");
    entry
}

#[test]
fn emitted_standard_and_custom_hosts_are_toolchain_free_and_cache_isolated() {
    let temp = tempfile::tempdir().expect("Wasmtime pack test root");
    let cache = temp.path().join("cache");

    let standard_root = temp.path().join("standard");
    fs::create_dir_all(&standard_root).expect("create standard project");
    let standard_entry = standard_root.join("main.av");
    fs::write(&standard_entry, STANDARD_SOURCE).expect("write standard entry");
    let standard_pack = temp.path().join("standard-pack");
    let standard = compile_pack(
        &cache,
        &standard_entry,
        &standard_root,
        &standard_pack,
        None,
    );
    assert!(standard.status.success(), "{}", report(&standard));
    for file in ["main.wasm", "manifest.json"] {
        assert!(standard_pack.join(file).is_file(), "missing {file}");
    }
    let standard_run = run_host(&standard_pack);
    assert!(standard_run.status.success(), "{}", report(&standard_run));
    assert_eq!(
        String::from_utf8_lossy(&standard_run.stdout).trim(),
        "standard-pack-ok"
    );

    // Byte identity is checked before Wasmtime is allowed to compile or
    // instantiate the guest.
    let wasm_path = standard_pack.join("main.wasm");
    let original_wasm = fs::read(&wasm_path).expect("read packed wasm");
    let mut tampered_wasm = original_wasm.clone();
    tampered_wasm.push(0);
    fs::write(&wasm_path, tampered_wasm).expect("tamper packed wasm");
    let tampered = run_host(&standard_pack);
    assert!(!tampered.status.success(), "tampered artifact ran");
    assert!(
        String::from_utf8_lossy(&tampered.stderr).contains("wasmtime-bundle-artifact-mismatch"),
        "{}",
        report(&tampered)
    );
    fs::write(&wasm_path, original_wasm).expect("restore packed wasm");

    let custom_root = temp.path().join("custom");
    let provider = repo_root().join("tests/fixtures/native_provider_host");
    let custom_entry = write_custom_project(&custom_root, &provider);
    let custom_pack = temp.path().join("custom-pack");
    let custom = compile_pack(&cache, &custom_entry, &custom_root, &custom_pack, None);
    assert!(custom.status.success(), "{}", report(&custom));
    let custom_run = run_host(&custom_pack);
    assert!(custom_run.status.success(), "{}", report(&custom_run));
    assert_eq!(
        String::from_utf8_lossy(&custom_run.stdout).trim(),
        "custom-pack-ok"
    );
    let custom_manifest =
        fs::read_to_string(custom_pack.join("manifest.json")).expect("read custom manifest");
    assert!(custom_manifest.contains("example.counted-shapes-echo@1"));
    assert!(custom_manifest.contains("counted-shapes-v1"));

    // Building the custom host must not overwrite the standard host in the
    // shared cache. A cache hit with Cargo made unreachable proves no rebuild
    // repaired the result behind our back.
    let standard_pack_again = temp.path().join("standard-pack-again");
    let cached = compile_pack(
        &cache,
        &standard_entry,
        &standard_root,
        &standard_pack_again,
        Some(Path::new("/definitely/missing/cargo")),
    );
    assert!(cached.status.success(), "{}", report(&cached));
    let cached_run = run_host(&standard_pack_again);
    assert!(cached_run.status.success(), "{}", report(&cached_run));
    assert_eq!(
        String::from_utf8_lossy(&cached_run.stdout).trim(),
        "standard-pack-ok"
    );
}

use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::time::{SystemTime, UNIX_EPOCH};

fn aver_bin() -> &'static str {
    env!("CARGO_BIN_EXE_aver")
}

fn fixture_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/capability_manifest")
}

fn run_capabilities(file: &str, json: bool) -> Output {
    let root = fixture_root();
    let mut command = Command::new(aver_bin());
    command
        .arg("capabilities")
        .arg(root.join(file))
        .arg("--module-root")
        .arg(&root);
    if json {
        command.arg("--json");
    }
    command.output().expect("run aver capabilities")
}

fn temp_output(label: &str) -> PathBuf {
    let nonce = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock after epoch")
        .as_nanos();
    std::env::temp_dir().join(format!(
        "aver-capability-target-{label}-{}-{nonce}",
        std::process::id()
    ))
}

#[test]
fn json_manifest_is_total_ordered_and_explicit() {
    let output = run_capabilities("main.av", true);
    assert!(
        output.status.success(),
        "capabilities failed:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let json: serde_json::Value =
        serde_json::from_slice(&output.stdout).expect("valid manifest JSON");
    assert_eq!(json["schemaVersion"], 1);
    assert_eq!(json["kind"], "capabilityTargetManifest");
    assert_eq!(
        json["targets"],
        serde_json::json!(["vm", "rust", "wasm-gc", "wasip2"])
    );

    let rows = json["rows"].as_array().expect("rows array");
    assert_eq!(rows.len(), 16, "four contracts times four targets");
    let identities = rows
        .iter()
        .map(|row| {
            format!(
                "{}:{}",
                row["capability"].as_str().unwrap(),
                row["target"].as_str().unwrap()
            )
        })
        .collect::<Vec<_>>();
    assert_eq!(
        identities,
        [
            "Clock:vm",
            "Clock:rust",
            "Clock:wasm-gc",
            "Clock:wasip2",
            "Probe:vm",
            "Probe:rust",
            "Probe:wasm-gc",
            "Probe:wasip2",
            "Time:vm",
            "Time:rust",
            "Time:wasm-gc",
            "Time:wasip2",
            "Vault:vm",
            "Vault:rust",
            "Vault:wasm-gc",
            "Vault:wasip2",
        ]
    );

    let row = |capability: &str, target: &str| {
        rows.iter()
            .find(|row| row["capability"] == capability && row["target"] == target)
            .expect("manifest row")
    };
    assert_eq!(row("Clock", "vm")["status"]["kind"], "host-bound");
    assert_eq!(row("Clock", "rust")["status"]["kind"], "host-bound");
    assert_eq!(
        row("Clock", "wasm-gc")["status"]["reason"]["code"],
        "wasm-gc-import-required"
    );
    assert_eq!(
        row("Clock", "wasip2")["status"]["reason"]["code"],
        "component-import-required"
    );
    assert_eq!(row("Time", "wasip2")["status"]["kind"], "provided");
    assert_eq!(
        row("Time", "wasip2")["status"]["provider"],
        "aver.standard.Time/wasip2-wasi"
    );
    assert_eq!(
        row("Clock", "rust")["requiredOperations"],
        serde_json::json!(["Clock.now"])
    );
    assert_eq!(
        row("Clock", "rust")["declaredOperations"],
        serde_json::json!(["Clock.now", "Clock.tick"])
    );
    assert_eq!(row("Probe", "rust")["required"], false);
    assert_eq!(
        row("Probe", "rust")["requiredOperations"],
        serde_json::json!([])
    );
    for capability in ["Clock", "Probe", "Vault"] {
        assert_eq!(row(capability, "vm")["status"]["kind"], "host-bound");
        assert_eq!(
            row(capability, "vm")["status"]["reason"]["code"],
            "runtime-provider-required"
        );
        assert_eq!(row(capability, "rust")["status"]["kind"], "host-bound");
        assert_eq!(
            row(capability, "rust")["status"]["reason"]["code"],
            "runtime-provider-required"
        );
        assert_eq!(
            row(capability, "wasm-gc")["status"]["reason"]["code"],
            "wasm-gc-import-required"
        );
    }
    assert_eq!(row("Clock", "wasip2")["status"]["kind"], "host-bound");
    let probe_reason = &row("Probe", "wasip2")["status"]["reason"];
    assert_eq!(probe_reason["code"], "wit-boundary-type-unsupported");
    assert_eq!(probe_reason["capability"], "Probe");
    assert_eq!(probe_reason["operation"], "Probe.read");
    assert_eq!(probe_reason["position"], "result");
    assert_eq!(probe_reason["averType"], "Int");
    assert!(probe_reason.get("parameterIndex").is_none());

    let vault_reason = &row("Vault", "wasip2")["status"]["reason"];
    assert_eq!(vault_reason["code"], "wit-boundary-type-unsupported");
    assert_eq!(vault_reason["capability"], "Vault");
    assert_eq!(vault_reason["operation"], "Vault.open");
    assert_eq!(vault_reason["position"], "result");
    assert_eq!(vault_reason["averType"], "Result<Token, String>");
}

#[test]
fn json_manifest_is_byte_deterministic() {
    let first = run_capabilities("main.av", true);
    let second = run_capabilities("main.av", true);
    assert!(first.status.success() && second.status.success());
    assert_eq!(first.stdout, second.stdout);
}

#[test]
fn human_manifest_names_every_binding_state() {
    let output = run_capabilities("main.av", false);
    assert!(output.status.success());
    let text = String::from_utf8_lossy(&output.stdout);
    assert!(text.contains("Capability target manifest:"));
    assert!(text.contains("Clock"));
    assert!(text.contains("vm      host-bound[runtime-provider-required]"));
    assert!(text.contains("rust    host-bound[runtime-provider-required]"));
    assert!(text.contains("wasm-gc host-bound[wasm-gc-import-required]"));
    assert!(text.contains("wasip2  host-bound[component-import-required]"));
    assert!(text.contains("Time"));
    assert!(text.contains("provided by aver.standard.Time/wasip2-wasi@"));
    assert!(text.contains("required operations: Clock.now"));
    assert!(text.contains("required operations: <none>"));
}

#[test]
fn process_missing_wasip2_binding_is_explicitly_unsupported() {
    let output = run_capabilities("process_client.av", true);
    assert!(
        output.status.success(),
        "capabilities failed:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let json: serde_json::Value =
        serde_json::from_slice(&output.stdout).expect("valid Process manifest JSON");
    let rows = json["rows"].as_array().expect("rows array");
    let process = |target: &str| {
        rows.iter()
            .find(|row| row["capability"] == "Process" && row["target"] == target)
            .expect("Process target row")
    };

    for target in ["vm", "rust", "wasm-gc"] {
        assert_eq!(process(target)["status"]["kind"], "provided");
    }
    let wasip2 = process("wasip2");
    assert_eq!(wasip2["status"]["kind"], "unsupported");
    assert_eq!(
        wasip2["status"]["reason"]["code"],
        "standard-binding-unavailable"
    );
    let message = wasip2["status"]["reason"]["message"]
        .as_str()
        .expect("reason message");
    assert!(message.contains("WASI 0.2 has no SIGINT/SIGTERM"));
    assert!(message.contains("wasm-gc"));
}

#[test]
#[cfg(feature = "wasip2")]
fn wasip2_compile_rejects_process_with_the_target_matrix_reason() {
    let root = fixture_root();
    let output_dir = temp_output("process-wasip2");
    let output = Command::new(aver_bin())
        .arg("compile")
        .arg(root.join("process_client.av"))
        .arg("--module-root")
        .arg(&root)
        .args(["--target", "wasip2", "-o"])
        .arg(&output_dir)
        .output()
        .expect("compile Process for wasip2");
    assert!(!output.status.success());
    let text = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(text.contains("error[capability-target-unsupported]"));
    assert!(text.contains("WASI 0.2 has no SIGINT/SIGTERM"));
    assert!(text.contains("wasm-gc"));
    assert!(
        !output_dir.exists(),
        "rejected target must not emit an artifact"
    );
}

#[test]
fn rust_compilation_emits_a_host_bound_provider_artifact() {
    let root = fixture_root();
    let output_dir = temp_output("host-bound-rust");
    let output = Command::new(aver_bin())
        .arg("compile")
        .arg(root.join("main.av"))
        .arg("--module-root")
        .arg(&root)
        .args(["--target", "rust", "-o"])
        .arg(&output_dir)
        .output()
        .expect("compile custom capability");
    assert!(output.status.success());
    let text = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(text.contains("capability Clock: host-bound[runtime-provider-required]"));
    assert!(text.contains("contract_hash=sha256:"));
    assert!(text.contains("model_hash=sha256:"));
    assert!(output_dir.join("src/provider_support.rs").is_file());
    std::fs::remove_dir_all(output_dir).expect("remove generated host-bound artifact");
}

#[test]
fn stock_vm_keeps_provider_missing_distinct_from_target_unsupported() {
    let root = fixture_root();
    let output = Command::new(aver_bin())
        .arg("run")
        .arg(root.join("main.av"))
        .arg("--module-root")
        .arg(&root)
        .output()
        .expect("run custom capability");
    assert!(!output.status.success());
    let text = String::from_utf8_lossy(&output.stderr);
    assert!(text.contains("error[capability-provider-missing]"));
    assert!(text.contains("Clock.now"));
    assert!(!text.contains("error[capability-target-unsupported]"));
}

#[test]
fn unused_custom_contracts_do_not_block_rust_compilation() {
    let root = fixture_root();
    let output_dir = temp_output("unused-rust");
    let output = Command::new(aver_bin())
        .arg("compile")
        .arg(root.join("unused.av"))
        .arg("--module-root")
        .arg(&root)
        .args(["--target", "rust", "-o"])
        .arg(&output_dir)
        .output()
        .expect("compile unused custom capabilities");
    let cleanup = std::fs::remove_dir_all(&output_dir);
    assert!(
        output.status.success(),
        "unused contracts blocked compile:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(cleanup.is_ok(), "generated output should be removable");
}

#[cfg(feature = "wasm")]
fn compile_wasm_gc(root: &Path, file: &str, label: &str) -> (PathBuf, Output) {
    let output_dir = temp_output(label);
    let output = Command::new(aver_bin())
        .arg("compile")
        .arg(root.join(file))
        .arg("--module-root")
        .arg(root)
        .args(["--target", "wasm-gc", "-o"])
        .arg(&output_dir)
        .output()
        .expect("compile custom capability for wasm-gc");
    (output_dir, output)
}

#[cfg(feature = "wasm")]
fn wasm_imports(bytes: &[u8]) -> Vec<(String, String)> {
    use wasmparser::{Parser, Payload};

    let mut imports = Vec::new();
    for payload in Parser::new(0).parse_all(bytes) {
        if let Payload::ImportSection(reader) = payload.expect("parse emitted wasm") {
            for group in reader {
                let group = group.expect("read import group");
                for import in group {
                    let (_, import) = import.expect("read import");
                    imports.push((import.module.to_string(), import.name.to_string()));
                }
            }
        }
    }
    imports
}

#[test]
#[cfg(feature = "wasm")]
fn wasm_gc_custom_contract_is_one_hashed_namespace_with_every_operation() {
    let root = fixture_root();
    let (output_dir, output) = compile_wasm_gc(&root, "main.av", "custom-wasm-gc");
    assert!(
        output.status.success(),
        "wasm-gc compile failed:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let report = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(report.contains("capability Clock: host-bound[wasm-gc-import-required]"));

    let manifest = run_capabilities("main.av", true);
    let json: serde_json::Value =
        serde_json::from_slice(&manifest.stdout).expect("valid target manifest");
    let clock = json["rows"]
        .as_array()
        .expect("rows")
        .iter()
        .find(|row| row["capability"] == "Clock" && row["target"] == "wasm-gc")
        .expect("Clock wasm-gc row");
    let hash = clock["contractHash"]
        .as_str()
        .expect("contract hash")
        .strip_prefix("sha256:")
        .expect("sha256 contract hash");
    let namespace = format!("aver:user/cap-n436c6f636b-c{hash}");

    let bytes = std::fs::read(output_dir.join("main.wasm")).expect("read wasm-gc artifact");
    let custom = wasm_imports(&bytes)
        .into_iter()
        .filter(|(module, _)| module.starts_with("aver:user/"))
        .collect::<Vec<_>>();
    assert_eq!(
        custom,
        [
            (namespace.clone(), "op-n6e6f77".to_string()),
            (namespace, "op-n7469636b".to_string()),
        ],
        "using one operation must retain the complete sorted contract"
    );
    std::fs::remove_dir_all(output_dir).expect("remove wasm-gc output");
}

#[test]
#[cfg(feature = "wasm")]
fn wasm_gc_resource_and_complete_provider_value_boundaries_validate() {
    let root = fixture_root();
    let (vault_out, vault) = compile_wasm_gc(&root, "vault_client.av", "vault-wasm-gc");
    assert!(
        vault.status.success(),
        "resource ABI failed:\n{}",
        String::from_utf8_lossy(&vault.stderr)
    );
    let vault_bytes = std::fs::read(vault_out.join("vault_client.wasm")).expect("vault wasm");
    let vault_wat = wasmprinter::print_bytes(&vault_bytes).expect("print vault wasm");
    assert!(vault_wat.contains("aver:user/cap-n5661756c74-c"));
    assert!(vault_wat.contains("externref"));
    assert!(
        vault_wat.contains("__cap_abi_n526573756c743c5661756c742e546f6b656e2c20537472696e673e_ok")
    );
    std::fs::remove_dir_all(vault_out).expect("remove vault output");

    let shapes_root =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/native_provider_shapes");
    let (shapes_out, shapes) = compile_wasm_gc(&shapes_root, "main.av", "shapes-wasm-gc");
    assert!(
        shapes.status.success(),
        "complete ProviderValue ABI failed:\n{}",
        String::from_utf8_lossy(&shapes.stderr)
    );
    let shapes_bytes = std::fs::read(shapes_out.join("main.wasm")).expect("shapes wasm");
    let shapes_wat = wasmprinter::print_bytes(&shapes_bytes).expect("print shapes wasm");
    for helper in [
        "__cap_abi_n496e74_from_decimal",
        "__cap_abi_n496e74_to_decimal",
        "__cap_abi_n5368617065732e42756e646c65_make",
        "__cap_abi_n4c6973743c496e743e_cons",
        "__cap_abi_n4f7074696f6e3c426f6f6c3e_some",
        "__cap_abi_n5374617465_kind",
    ] {
        assert!(shapes_wat.contains(helper), "missing ABI helper {helper}");
    }
    assert!(shapes_wat.contains("__cap_abi_n4d61703c537472696e672c20496e743e_set"));
    std::fs::remove_dir_all(shapes_out).expect("remove shapes output");

    let (units_out, units) = compile_wasm_gc(&root, "unit_shapes_client.av", "unit-shapes-wasm-gc");
    assert!(
        units.status.success(),
        "Unit container ABI failed:\n{}",
        String::from_utf8_lossy(&units.stderr)
    );
    let units_bytes =
        std::fs::read(units_out.join("unit_shapes_client.wasm")).expect("Unit shapes wasm");
    let units_wat = wasmprinter::print_bytes(&units_bytes).expect("print Unit shapes wasm");
    for helper in [
        "__cap_abi_n4f7074696f6e3c556e69743e_some",
        "__cap_abi_n4c6973743c556e69743e_cons",
        "__cap_abi_n566563746f723c556e69743e_get",
        "__cap_abi_n566563746f723c556e69743e_set",
    ] {
        assert!(
            units_wat.contains(helper),
            "missing Unit ABI helper {helper}"
        );
    }
    std::fs::remove_dir_all(units_out).expect("remove Unit shapes output");
}

#[test]
#[cfg(feature = "wasm")]
fn wasm_gc_certificate_keeps_custom_operations_opaque() {
    let root = fixture_root();
    let output_dir = temp_output("custom-capability-cert");
    let output = Command::new(aver_bin())
        .arg("compile")
        .arg(root.join("cert_client.av"))
        .arg("--module-root")
        .arg(&root)
        .args(["--target", "wasm-gc", "--certify", "-o"])
        .arg(&output_dir)
        .output()
        .expect("certify custom capability artifact");
    assert!(
        output.status.success(),
        "custom capability certification failed:\n{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let manifest: serde_json::Value = serde_json::from_slice(
        &std::fs::read(output_dir.join("cert/cert-manifest.json")).expect("certificate manifest"),
    )
    .expect("valid certificate manifest");
    assert!(
        manifest["certified"]
            .as_array()
            .expect("certified exports")
            .iter()
            .any(|entry| entry["name"] == "plusOne")
    );
    let capabilities = manifest["capabilities"]
        .as_array()
        .expect("capability imports");
    assert_eq!(
        capabilities.len(),
        2,
        "the complete Clock contract is opaque"
    );
    assert!(capabilities.iter().all(|pair| {
        pair.as_object().is_some_and(|pair| {
            pair["module"]
                .as_str()
                .is_some_and(|module| module.starts_with("aver:user/cap-n436c6f636b-c"))
                && pair["name"]
                    .as_str()
                    .is_some_and(|field| field.starts_with("op-n"))
        })
    }));
    std::fs::remove_dir_all(output_dir).expect("remove certificate output");
}

#[test]
#[cfg(all(feature = "wasip2", feature = "certify"))]
fn wasip2_certificate_keeps_custom_operations_opaque() {
    let root = fixture_root();
    let output_dir = temp_output("custom-capability-wasip2-cert");
    let output = Command::new(aver_bin())
        .arg("compile")
        .arg(root.join("cert_client.av"))
        .arg("--module-root")
        .arg(&root)
        .args(["--target", "wasip2", "--certify", "-o"])
        .arg(&output_dir)
        .output()
        .expect("certify custom capability component");
    assert!(
        output.status.success(),
        "custom capability component certification failed:\n{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let manifest: serde_json::Value = serde_json::from_slice(
        &std::fs::read(output_dir.join("cert/cert-manifest.json")).expect("certificate manifest"),
    )
    .expect("valid certificate manifest");
    assert_eq!(manifest["target"], "wasip2");
    assert_eq!(manifest["abi"], "aver-wasip2/0");
    assert_eq!(manifest["wasm"], "cert_client.component.wasm");
    assert_eq!(
        manifest["wasip2ComponentEnvelope"]["kind"],
        "prefix-core-suffix/v1"
    );
    let capabilities = manifest["capabilities"]
        .as_array()
        .expect("capability imports");
    assert_eq!(
        capabilities.len(),
        2,
        "the complete Clock contract is opaque"
    );
    assert!(capabilities.iter().all(|pair| {
        pair.as_object().is_some_and(|pair| {
            pair["module"]
                .as_str()
                .is_some_and(|module| module.starts_with("aver:user/cap-n436c6f636b-c"))
                && pair["name"]
                    .as_str()
                    .is_some_and(|field| field.starts_with("op-n"))
        })
    }));
    std::fs::remove_dir_all(output_dir).expect("remove certificate output");
}

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
    assert_eq!(
        row("Clock", "rust")["status"]["reason"]["code"],
        "static-adapter-not-linked"
    );
    assert_eq!(
        row("Clock", "wasm-gc")["status"]["reason"]["code"],
        "host-import-adapter-not-generated"
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
        assert_eq!(
            row(capability, "rust")["status"]["reason"]["code"],
            "static-adapter-not-linked"
        );
        assert_eq!(
            row(capability, "wasm-gc")["status"]["reason"]["code"],
            "host-import-adapter-not-generated"
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
    assert!(text.contains("rust    unsupported[static-adapter-not-linked]"));
    assert!(text.contains("wasm-gc unsupported[host-import-adapter-not-generated]"));
    assert!(text.contains("wasip2  host-bound[component-import-required]"));
    assert!(text.contains("Time"));
    assert!(text.contains("provided by aver.standard.Time/wasip2-wasi@"));
    assert!(text.contains("required operations: Clock.now"));
    assert!(text.contains("required operations: <none>"));
}

#[test]
fn rust_gate_reports_target_unsupported_with_a_stable_reason() {
    let root = fixture_root();
    let output = Command::new(aver_bin())
        .arg("compile")
        .arg(root.join("main.av"))
        .arg("--module-root")
        .arg(&root)
        .args(["--target", "rust", "-o"])
        .arg(temp_output("rejected-rust"))
        .output()
        .expect("compile custom capability");
    assert!(!output.status.success());
    let text = String::from_utf8_lossy(&output.stderr);
    assert!(text.contains("error[capability-target-unsupported]"));
    assert!(text.contains("target `rust` cannot bind capability `Clock`"));
    assert!(text.contains("reason[static-adapter-not-linked]"));
    assert!(text.contains("required operations: Clock.now"));
    assert!(text.contains("contract_hash: sha256:"));
    assert!(text.contains("model_hash: sha256:"));
    assert!(!text.contains("error[capability-provider-missing]"));
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

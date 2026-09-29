//! The literal proof shortcut must still establish exact UTF-8 bytes in Lean.

use super::*;

#[test]
fn certify_bridge_literals_encode_all_utf8_widths_kernel_clean() {
    if !lean_required::lake_available() {
        return;
    }
    let source_dir = temp_dir("certify-bridge-literals-source");
    let source = source_dir.join("literals.av");
    let long = "0123456789abcdef".repeat(40);
    std::fs::write(
        &source,
        format!(
            r#"module BridgeLiterals
    intent = "Certify exact bytes of empty, escaped, Unicode and long literals."
    exposes [empty, escaped, wide, long]

fn empty() -> String
    ""
fn escaped() -> String
    "a'\"\\\n\r\t"
fn wide() -> String
    "é中🦀"
fn long() -> String
    "{long}"
"#
        ),
    )
    .unwrap();
    let (out, manifest) = certify_fixture(source.to_str().unwrap(), &[], "certify-bridge-literals");
    let cert = out.join("cert");
    let literals = std::fs::read_to_string(cert.join("BridgeLits.lean")).unwrap();
    assert_eq!(literals.matches("theorem strLit_").count(), 4);
    assert_eq!(literals.matches("(strBytes_ofList ").count(), 4);
    assert!(literals.contains("[195, 169, 228, 184, 173, 240, 159, 166, 128]"));
    assert_eq!(manifest["sourceBridges"].as_array().unwrap().len(), 4);
    let (ok, report) = check_certificate(&out.join("literals.wasm"), &cert);
    assert!(ok, "{report}");
    assert!(
        report.contains("source-bridges: 4 of 4 credited"),
        "{report}"
    );

    // No fallback is allowed to pass as a successful shortcut. Build and
    // audit each emitted literal proof, in addition to the checker's credit
    // for the bridges that depend on them (escaped and Unicode included).
    materialize_wall(&cert);
    let build = Command::new("lake")
        .current_dir(&cert)
        .args(["build", "BridgeLits"])
        .output()
        .unwrap();
    assert!(
        build.status.success(),
        "{}",
        aver_cmd::format_output(&build)
    );
    let mut probe = String::from("import BridgeLits\n");
    for line in literals.lines() {
        if let Some(rest) = line.strip_prefix("theorem strLit_") {
            let index = rest.split(" :").next().unwrap();
            probe.push_str(&format!("#print axioms AverCert.Bridge.strLit_{index}\n"));
        }
    }
    std::fs::write(cert.join("LiteralAudit.lean"), probe).unwrap();
    let audit = Command::new("lake")
        .current_dir(&cert)
        .args(["env", "lean", "LiteralAudit.lean"])
        .output()
        .unwrap();
    let report = aver_cmd::format_output(&audit);
    assert!(audit.status.success(), "{report}");
    assert_eq!(report.matches("'AverCert.Bridge.strLit_").count(), 4);
    assert!(!report.contains("sorryAx"), "{report}");
    trim_lean_build_tree(&cert);
}

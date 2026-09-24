use super::*;

#[test]
fn proof_export_ignores_legacy_sidecars_in_parent_directories() {
    let root = temp_output_dir("aver-proof-source-only");
    let source_dir = root.join("domain");
    std::fs::create_dir_all(&source_dir).unwrap();
    let source = source_dir.join("double.av");
    std::fs::write(
        &source,
        "module Double\n    exposes [twice]\n    intent = \"Source-owned arithmetic proof.\"\n\nfn twice(x: Int) -> Int\n    x + x\n\nverify twice law doubleIsTwice\n    given x: Int = [-3, 0, 7]\n    twice(x) => 2 * x\n",
    )
    .unwrap();

    let export = |name: &str| {
        let destination = root.join(format!("lean-{name}"));
        let output = Command::new(env!("CARGO_BIN_EXE_aver"))
            .args(["proof", source.to_str().unwrap(), "-o"])
            .arg(&destination)
            .output()
            .unwrap();
        assert!(output.status.success(), "{}", format_output(&output));
        std::fs::read(destination.join("Double.lean")).unwrap()
    };
    let original = export("before");
    let proofs = root.join("proofs").join("lean");
    std::fs::create_dir_all(&proofs).unwrap();
    std::fs::write(
        proofs.join("twice__doubleIsTwice.lean"),
        "THIS IS INVALID PROVER CODE: legacy sidecars must never be injected",
    )
    .unwrap();
    assert_eq!(original, export("after"), "loaded a sidecar");

    if Command::new("lake").arg("--version").output().is_ok() {
        let (summary, output) = run_lean_check_json_with_args(
            source.to_str().unwrap(),
            &root.join("checked"),
            0,
            &[],
            &[],
        );
        assert!(output.status.success(), "{}", format_output(&output));
        assert_eq!(summary["universal_laws"], 1);
        let manifest = std::fs::read_to_string(root.join("checked/proof_manifest.json")).unwrap();
        assert!(!manifest.contains("\"hand\""));
    }
    let _ = std::fs::remove_dir_all(root);
}

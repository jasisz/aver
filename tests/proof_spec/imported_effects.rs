use super::*;

#[test]
fn imported_in_place_effects_have_universal_mapping_and_splice_laws() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let source =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/yield_imported_effects");
    let dir = tempfile::tempdir().unwrap();
    let (summary, run) = run_lean_check_json_with_args(
        source.join("main.av").to_str().unwrap(),
        dir.path(),
        0,
        &[],
        &["--module-root", source.to_str().unwrap()],
    );
    assert!(run.status.success(), "{}", format_output(&run));
    for key in ["bounded_laws", "build_errors", "sorries"] {
        assert_eq!(summary[key], 0, "{summary}");
    }
    let manifest: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(dir.path().join("proof_manifest.json")).unwrap(),
    )
    .unwrap();
    let laws = manifest["laws"].as_array().unwrap();
    assert!(
        laws.len() > 20,
        "missing composition obligations: {manifest}"
    );
    for law in laws {
        assert_eq!(law["tier"], "universal", "{law}");
        for axiom in law["axioms"].as_array().unwrap() {
            assert!(
                ["propext", "Quot.sound", "Classical.choice"].contains(&axiom.as_str().unwrap()),
                "{law}"
            );
        }
    }
}

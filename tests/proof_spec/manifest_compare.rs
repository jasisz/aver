//! `--compare-manifest`: the proof manifest carries a hash per emitted
//! definition and per law script, and a second run can be compared against
//! it so a law that stops proving is attributed to what actually changed.

use super::*;

const BEFORE: &str = r#"module Compare
    intent = "A law proved once, then broken by editing a helper it opens."
    effects []

fn double(n: Int) -> Int
    n + n

fn quad(n: Int) -> Int
    double(double(n))

verify quad law fourfold
    given n: Int = [0]
    quad(n) => n * 4
"#;

#[test]
fn compare_manifest_names_the_changed_definition_under_a_failed_law() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let source = tempfile::tempdir().unwrap();
    let main = source.path().join("compare.av");
    std::fs::write(&main, BEFORE).unwrap();
    let module_root = source.path().to_str().unwrap();

    // Run 1: the law closes. The manifest records a hash for every emitted
    // definition and for the law's script.
    let first = tempfile::tempdir().unwrap();
    let (summary, run) = run_lean_check_json_with_args(
        main.to_str().unwrap(),
        first.path(),
        0,
        &[],
        &["--module-root", module_root],
    );
    assert!(run.status.success(), "{}", format_output(&run));
    assert_eq!(summary["universal_laws"], 1, "{summary}");
    let manifest_path = first.path().join("proof_manifest.json");
    let manifest: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&manifest_path).unwrap()).unwrap();
    for name in ["Compare.double", "Compare.quad"] {
        assert!(
            manifest["definitions"][name].is_string(),
            "no hash for {name}: {manifest}"
        );
    }
    assert!(
        manifest["scripts"]["quad.fourfold"].is_string(),
        "no script hash: {manifest}"
    );

    // Run 2: the helper the law opens changes and the law is now false; its
    // own script is byte for byte the same. The report says so and names the
    // changed definition, not the law's own function.
    let previous = source.path().join("previous.json");
    std::fs::copy(&manifest_path, &previous).unwrap();
    std::fs::write(&main, BEFORE.replace("    n + n\n", "    n + n + n\n")).unwrap();
    let second = tempfile::tempdir().unwrap();
    let (summary, run) = run_lean_check_json_with_args(
        main.to_str().unwrap(),
        second.path(),
        0,
        &[],
        &[
            "--module-root",
            module_root,
            "--compare-manifest",
            previous.to_str().unwrap(),
        ],
    );
    assert!(!run.status.success(), "false law passed: {summary}");
    assert_eq!(summary["build_errors"], 0, "{summary}");
    assert_eq!(
        summary["sorry_laws"],
        serde_json::json!(["quad.fourfold"]),
        "{summary}"
    );
    assert_eq!(
        summary["changed"],
        serde_json::json!({
            "quad.fourfold": {"script": "same", "definitions": ["Compare.double"]}
        }),
        "{summary}"
    );
}

use super::*;

#[test]
fn k5_sticky_composition_has_universal_source_proofs() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-k5-sticky-composition");
    let (summary, output) = run_lean_check_json_with_args(
        "projects/k5_fdiv/domain/round.av",
        &dir,
        0,
        &[],
        &["--module-root", "projects/k5_fdiv"],
    );
    assert!(output.status.success(), "{}", format_output(&output));
    assert_eq!(summary["build_errors"], 0);
    assert_eq!(summary["bounded_laws"], 0);
    let manifest: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(dir.join("proof_manifest.json")).unwrap())
            .unwrap();
    for name in [
        "Domain.StickyScale.coarsen.dropsStickyBit",
        "fpSticky.preservesCoarseTruncation",
        "truncStickyComposes.composesThroughSticky",
    ] {
        let law = manifest["laws"]
            .as_array()
            .unwrap()
            .iter()
            .find(|law| law["law"] == name)
            .unwrap_or_else(|| panic!("missing law {name}"));
        assert_eq!(law["tier"], "universal", "{name}: {law}");
    }
    for obligation in manifest["obligations"].as_array().unwrap() {
        assert_eq!(obligation["tier"], "universal", "{obligation}");
    }
    let _ = std::fs::remove_dir_all(dir);
}

use super::*;

#[test]
fn guarded_floor_citations_compose_without_losing_their_premises() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-law-floor-citations");
    let root = "tests/fixtures/law_reason_floor";
    let (summary, run) = run_lean_check_json_with_args(
        &format!("{root}/main.av"),
        &dir.join("positive"),
        0,
        &[],
        &["--module-root", root],
    );
    assert!(run.status.success(), "{}", format_output(&run));
    assert_eq!(summary["build_errors"], 0);
    assert_eq!(summary["universal_laws"], 5);
    for law in ["scaled.cancelsThenDivides", "discardLowBit.coarsens"] {
        for step in ["because1", "implication"] {
            assert_eq!(summary["obligations"][format!("{law}.{step}")], "universal");
        }
    }

    // The cited absorption law still requires bit < 2. Removing that premise
    // must leave the consumer open even when its retained samples all pass.
    let source = std::fs::read_to_string(format!("{root}/main.av"))
        .unwrap()
        .replace("    when bit < 2\n", "")
        .replace("given bit: Int = [0, 1, 2]", "given bit: Int = [0, 1]");
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(dir.join("main.av"), source).unwrap();
    std::fs::copy(format!("{root}/arithmetic.av"), dir.join("arithmetic.av")).unwrap();
    let (negative, run) = run_lean_check_json_with_args(
        dir.join("main.av").to_str().unwrap(),
        &dir.join("negative"),
        0,
        &[],
        &["--module-root", dir.to_str().unwrap()],
    );
    assert!(
        !run.status.success(),
        "the missing premise must not be assumed"
    );
    assert_eq!(negative["build_errors"], 0, "{}", format_output(&run));
    assert_eq!(
        negative["obligations"]["discardLowBit.coarsens.because1"],
        "failed"
    );
    let _ = std::fs::remove_dir_all(dir);
}

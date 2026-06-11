use super::*;

/// FIX B revert probe: a `verify f` case whose expected side calls another
/// user fn (`stepSum(20) => stepSumAcc(20)`) must be emitted with the
/// VM-computed ground-truth literal on the expected side — `stepSum 20 = 210`
/// — not as the model-vs-model `stepSum 20 = stepSumAcc 20` (which is
/// vacuously provable when fuel exhaustion collapses both sides to
/// `default`). With literalization reverted the emitted Lean contains the
/// `stepSumAcc 20` call again and this test fails. The `--check` half
/// (lake-gated) certifies literalization changed nothing about a correct
/// model passing.
#[test]
fn proof_lean_verify_case_expected_side_is_literalized_from_vm_ground_truth() {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-fixb-literal-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    let av = src.join("probe.av");
    std::fs::write(&av, FUEL_PROBE_AV).expect("write probe.av");

    let out = temp_output_dir("aver-fixb-literal-out");
    let emit = Command::new(aver_bin)
        .arg("proof")
        .arg(&av)
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .output()
        .expect("expected `aver proof` to run");
    assert!(
        emit.status.success(),
        "emit failed:\n{}",
        format_output(&emit)
    );

    let entry = std::fs::read_to_string(out.join("FuelProbe.lean")).expect("read emitted entry");
    assert!(
        entry.contains("example : stepSum 20 = 210"),
        "expected side must be the VM ground-truth literal (210), got:\n{entry}"
    );
    assert!(
        !entry.contains("= stepSumAcc 20"),
        "expected side must NOT remain a model call (vacuous under fuel \
         exhaustion):\n{entry}"
    );

    // Lake-gated half: the literalized export still builds green and the
    // check passes — literalization must not change what a correct model
    // proves.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping literalization build half: `lake` not available");
        let _ = std::fs::remove_dir_all(&src);
        let _ = std::fs::remove_dir_all(&out);
        return;
    }
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(&av)
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check` to run");
    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)));
    let summary: serde_json::Value =
        serde_json::from_str(json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}"));
    assert_eq!(
        summary["passed"].as_bool(),
        Some(true),
        "literalized export must build green and pass --check\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(0),
        "literalized export must be sorry-free\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["model_panicked"].as_bool(),
        Some(false),
        "a healthy build must report no model panic\n{}",
        format_output(&run)
    );

    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

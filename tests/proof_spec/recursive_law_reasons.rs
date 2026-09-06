use super::*;

#[test]
fn sampled_slice_guards_keep_checked_element_types() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-empty-slice-guard");
    let (summary, run) =
        run_lean_check_json("tests/fixtures/law_empty_slice_guard.av", &dir, 0, &[]);
    assert!(run.status.success(), "{}", format_output(&run));
    assert_eq!(summary["build_errors"], 0, "{summary}");
    assert_eq!(summary["universal_laws"], 2, "{summary}");
    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn guided_laws_see_all_nonrecursive_match_alternatives() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-lookup-reasons");
    let (summary, run) = run_lean_check_json("tests/fixtures/law_reason_lookup.av", &dir, 0, &[]);
    assert!(!run.status.success(), "the smaller bound must remain open");
    assert_eq!(summary["build_errors"], 0, "{}", format_output(&run));
    assert_eq!(summary["universal_laws"], 1, "{summary}");
    assert_eq!(
        summary["obligations"]["label.bounded.implication"], "universal",
        "{summary}"
    );
    assert_eq!(
        summary["obligations"]["label.rejectsSmallerBound.implication"], "failed",
        "{summary}"
    );
    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn local_match_reasons_fall_back_to_the_checked_list_measure() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-local-match-reasons");
    let (summary, run) =
        run_lean_check_json("tests/fixtures/law_reason_local_match.av", &dir, 0, &[]);
    assert!(!run.status.success(), "false reasons must remain open");
    assert_eq!(summary["build_errors"], 0, "{}", format_output(&run));
    assert_eq!(summary["universal_laws"], 1, "{summary}");
    for step in ["because1", "because2", "implication"] {
        assert_eq!(
            summary["obligations"][format!("reason.localMatch.{step}")],
            "universal",
            "{summary}"
        );
    }
    for law in ["falseBase.rejectsFalseBase", "guardLoss.rejectsLostPremise"] {
        assert_eq!(
            summary["obligations"][format!("{law}.because1")],
            "failed",
            "{summary}"
        );
        assert_eq!(
            summary["obligations"][format!("{law}.implication")],
            "universal",
            "{summary}"
        );
    }
    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn guided_laws_reuse_native_mutual_list_equations() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-mutual-slice-reasons");
    let (summary, run) =
        run_lean_check_json("tests/fixtures/law_reason_mutual_slices.av", &dir, 0, &[]);
    assert!(!run.status.success(), "the extra skip must remain unproved");
    assert_eq!(summary["build_errors"], 0, "{}", format_output(&run));
    assert_eq!(summary["universal_laws"], 1, "{summary}");
    assert_eq!(
        summary["obligations"]["scan.chunkStep.implication"], "universal",
        "{summary}"
    );
    assert_eq!(
        summary["obligations"]["scan.rejectsExtraSkip.implication"], "failed",
        "{summary}"
    );
    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn slice_reasons_use_integer_counts_without_losing_their_premises() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-slice-reasons");
    let (summary, run) = run_lean_check_json("tests/fixtures/law_reason_slices.av", &dir, 0, &[]);
    assert!(!run.status.success(), "false laws must remain open");
    assert_eq!(summary["build_errors"], 0, "{}", format_output(&run));
    assert_eq!(summary["universal_laws"], 5, "{summary}");
    for law in ["sumInto.step", "rejoined.emptyGivenKeepsElementType"] {
        assert_eq!(
            summary["obligations"][format!("{law}.implication")],
            "universal",
            "{summary}"
        );
    }
    for law in ["above.dropPreservesBound", "above.takePreservesBound"] {
        for step in ["because1", "implication"] {
            assert_eq!(
                summary["obligations"][format!("{law}.{step}")],
                "universal",
                "{summary}"
            );
        }
    }
    for obligation in [
        "above.rejectsMissingPremise.because1",
        "rejoined.rejectsDeletingAnExtraElement.implication",
    ] {
        assert_eq!(summary["obligations"][obligation], "failed", "{summary}");
    }
    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn list_descent_outlives_a_sibling_counter_in_both_proof_models() {
    for (backend, tool) in [("lean", "lake"), ("dafny", "dafny")] {
        if Command::new(tool).arg("--version").output().is_err() {
            continue;
        }
        let dir = temp_output_dir(&format!("aver-list-counter-{backend}"));
        let run = Command::new(env!("CARGO_BIN_EXE_aver"))
            .args([
                "proof",
                "tests/fixtures/list_counter_descent.av",
                "--backend",
                backend,
                "--check-json",
                "-o",
            ])
            .arg(&dir)
            .output()
            .unwrap();
        assert!(run.status.success(), "{backend}: {}", format_output(&run));
        let _ = std::fs::remove_dir_all(dir);
    }
}

#[test]
fn recursive_explanations_use_checked_induction_and_keep_all_premises() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-recursive-law-reasons");
    let (summary, run) =
        run_lean_check_json("tests/fixtures/law_reasons_recursive.av", &dir, 0, &[]);
    assert!(
        !run.status.success(),
        "false reasons must still fail the gate"
    );
    assert_eq!(summary["build_errors"], 0, "{}", format_output(&run));
    assert_eq!(summary["universal_laws"], 3, "{summary}");
    for law in [
        "appendExplained",
        "guardedChangingArgument",
        "guardedStructural",
    ] {
        assert_eq!(
            summary["obligations"][format!("count.{law}.because1")],
            "universal",
            "{summary}"
        );
        assert_eq!(
            summary["obligations"][format!("count.{law}.implication")],
            "universal",
            "{summary}"
        );
    }
    assert_eq!(
        summary["obligations"]["count.guardedStructural.because2"],
        "universal"
    );
    for (law, step) in [
        ("rejectsFalseBase", 1),
        ("rejectsFalseStep", 1),
        ("guardMustReachRecursiveCall", 2),
    ] {
        assert_eq!(
            summary["obligations"][format!("count.{law}.because{step}")],
            "failed",
            "{summary}"
        );
        assert_eq!(
            summary["obligations"][format!("count.{law}.implication")],
            "universal",
            "{summary}"
        );
    }
    let lean = std::fs::read_to_string(dir.join("RecursiveReasons.lean")).unwrap();
    assert!(
        !lean.contains("boundReason__fuel"),
        "list descent must beat sibling countdown fuel"
    );
    let hostile = Command::new(env!("CARGO_BIN_EXE_aver"))
        .args([
            "verify",
            "tests/fixtures/law_reasons_recursive.av",
            "--hostile",
        ])
        .output()
        .unwrap();
    assert!(!hostile.status.success());
    let output = format_output(&hostile);
    for law in [
        "appendExplained",
        "guardedChangingArgument",
        "guardedStructural",
    ] {
        assert!(
            output.contains(&format!("✓ count law {law}.because1")),
            "{output}"
        );
    }
    for law in [
        "rejectsFalseBase",
        "rejectsFalseStep",
        "guardMustReachRecursiveCall",
    ] {
        assert!(
            output.contains(&format!("✗ count law {law}.because")),
            "{output}"
        );
    }
    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn nondecreasing_reason_cannot_turn_passing_samples_into_a_proof() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let file = "tests/fixtures/law_reasons_nondecreasing.av";
    let samples = Command::new(env!("CARGO_BIN_EXE_aver"))
        .args(["verify", file])
        .output()
        .unwrap();
    assert!(samples.status.success(), "{}", format_output(&samples));
    let dir = temp_output_dir("aver-nondecreasing-reason");
    let (summary, run) = run_lean_check_json(file, &dir, 0, &[]);
    assert!(!run.status.success());
    assert_eq!(summary["build_errors"], 0, "{}", format_output(&run));
    assert_eq!(summary["universal_laws"], 0);
    assert_eq!(
        summary["obligations"]["identity.rejectsCircularReason.because1"],
        "failed"
    );
    assert_eq!(
        summary["obligations"]["identity.rejectsCircularReason.implication"],
        "universal"
    );
    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn recursive_reason_resolves_imported_helpers_despite_local_name_collisions() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-recursive-reason-import");
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(dir.join("ListProof.av"), "module ListProof\n    exposes [measure, reason]\n    intent = \"Structural argument.\"\n    effects []\nfn measure(xs: List<Int>) -> Int\n    match xs\n        [] -> 0\n        [x, ..rest] -> 1 + measure(rest)\nfn reason(xs: List<Int>) -> Bool\n    match xs\n        [] -> true\n        [x, ..rest] -> Bool.and(reason(rest), measure(xs) >= 0)\n").unwrap();
    let file = dir.join("Consumer.av");
    std::fs::write(&file, "module Consumer\n    depends [ListProof]\n    intent = \"Names belong to their modules.\"\n    effects []\nfn measure(x: Int) -> Int\n    x\nfn reason(x: Int) -> Bool\n    x == 0\nverify measure law explained\n    given xs: List<Int> = [[], [1, 2]]\n    because ListProof.reason(xs)\n    using []\n    ListProof.measure(xs) >= 0 holds\n").unwrap();
    let (summary, run) = run_lean_check_json_with_args(
        file.to_str().unwrap(),
        &dir.join("lean"),
        0,
        &[],
        &["--module-root", dir.to_str().unwrap()],
    );
    assert!(run.status.success(), "{}", format_output(&run));
    assert_eq!(summary["universal_laws"], 1);
    assert_eq!(
        summary["obligations"]["measure.explained.because1"],
        "universal"
    );
    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn structural_equality_reasons_preserve_float_nonreflexivity() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-reason-equality");
    let (summary, run) = run_lean_check_json("tests/fixtures/law_reason_equality.av", &dir, 0, &[]);
    assert!(!run.status.success(), "NaN reasons must stay open");
    assert_eq!(summary["build_errors"], 0, "{}", format_output(&run));
    assert_eq!(summary["universal_laws"], 2, "{summary}");
    for law in ["identity.recordExplanation", "equalContainers.reflexive"] {
        assert_eq!(
            summary["obligations"][format!("{law}.because1")],
            "universal",
            "{summary}"
        );
    }
    for law in [
        "floatIdentity.noNaNReflexivity",
        "floatIdentity.noNestedNaNReflexivity",
    ] {
        assert_eq!(
            summary["obligations"][format!("{law}.because1")],
            "failed",
            "{summary}"
        );
    }
    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn structural_filter_reason_uses_checked_mutual_equations() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-reason-filter");
    let (summary, run) = run_lean_check_json("tests/fixtures/law_reason_filter.av", &dir, 0, &[]);
    assert!(run.status.success(), "{}", format_output(&run));
    assert_eq!(summary["universal_laws"], 1, "{summary}");
    assert_eq!(
        summary["obligations"]["without.stableFilter.because1"],
        "universal"
    );
    assert_eq!(
        summary["obligations"]["without.stableFilter.implication"],
        "universal"
    );
    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn imported_structural_equality_and_mutual_equations_keep_their_owner() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-imported-filter-reason");
    std::fs::create_dir_all(&dir).unwrap();
    let dependency = include_str!("../fixtures/law_reason_filter.av").replace(
        "module PacketFilter",
        "module PacketFilter\n    exposes [Packet, without, retained, reason]",
    );
    std::fs::write(dir.join("PacketFilter.av"), dependency).unwrap();
    let source = dir.join("Consumer.av");
    std::fs::write(&source, r#"module Consumer
    depends [PacketFilter]
    intent = "Imported proof symbols retain their types and owning scopes."
    effects []
record Packet
    value: Float
fn key(value: Int) -> Int
    value
verify key law importedFilter
    given packets: List<PacketFilter.Packet> = [[], [PacketFilter.Packet.Marker(1)]]
    given target: List<Int> = [[], [1]]
    given acc: List<PacketFilter.Packet> = [[]]
    because PacketFilter.reason(packets, target, acc)
    using []
    PacketFilter.without(packets, target, acc) => List.concat(List.reverse(acc), PacketFilter.retained(packets, target))
"#).unwrap();
    let (summary, run) = run_lean_check_json_with_args(
        source.to_str().unwrap(),
        &dir.join("lean"),
        0,
        &[],
        &["--module-root", dir.to_str().unwrap()],
    );
    assert!(run.status.success(), "{}", format_output(&run));
    assert_eq!(summary["universal_laws"], 2, "{summary}");
    let _ = std::fs::remove_dir_all(dir);
}

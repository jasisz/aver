use super::*;

/// Nonlinear-arithmetic wall (`tests/fixtures/nr_wall.av`):
/// laws whose unfolded cone multiplies two VARIABLES (`x * x`,
/// `(s*s - d*x)^2`) must DEGRADE to honest caught sorries, never to a
/// failing tactic. Pre-fix this file produced three distinct build
/// errors: `by_cases h_h_a : h_a ≥ 0` (case over a premise-HYPOTHESIS
/// name — application type mismatch), `omega` failing on a nonlinear
/// goal, and `_sample_N` theorems FALSE AS STATED (when-guard numerals
/// elaborated as Nat, truncating subtraction). Post-fix the export
/// builds GREEN: exactly 2 sorries (the two unguarded nonlinear
/// universals — sqNonneg, nrNewErrNum≍nrOldErrSq), with every
/// `when`-guarded law closed bounded over its declared domain and
/// every emitted sample theorem true (Int-ascribed guards).
#[test]
fn proof_nonlinear_laws_degrade_to_honest_sorries() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping nonlinear-wall proof test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-nr-wall");
    let (summary, run) = run_lean_check_json("tests/fixtures/nr_wall.av", &output_dir, 2, &[]);
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(2),
        "nonlinear laws must land on exactly 2 honest caught sorries \
         (a build error OR a different count is a regression).\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["passed"].as_bool(),
        Some(true),
        "the nonlinear-wall export must BUILD green — failing tactics \
         (omega on var*var goals, by_cases over hypothesis names, \
         Nat-truncated sample guards) are build errors, not sorries.\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Dafny side of the nonlinear-wall fixture: `when`-law samples come
/// from the UNFILTERED given cartesian product, so premise-violating
/// combinations (square-monotonicity at e=1, b=0) were asserted
/// unguarded and failed verification on a file whose universal lemmas
/// Z3 fully proves. Post-fix the samples are checked under
/// `if <instantiated premise> { … }` (mirroring Lean's `_sample_N`
/// premise-as-hypothesis form) and the whole file verifies: 0 errors,
/// 0 axioms.
///
/// Deliberately budget-only (no `passed` assert): this fixture's
/// genuinely nonlinear universal lemmas have platform-sensitive
/// verification wall-clock — a slower Z3 build can time an obligation
/// out (exit 4) without erroring, which is jitter, not the regression
/// under test. The when-filter regression itself surfaces as ERRORS
/// (reverting the guard yields 2 "assertion might not hold"), so the
/// error budget catches it on every platform.
#[test]
fn proof_dafny_when_filtered_samples() {
    assert_dafny_verifies("tests/fixtures/nr_wall.av", "aver-dafny-nr-wall");
}

/// Rationals fixture (`tests/fixtures/rational_probe.av`):
/// concrete-literal sample asserts over record arguments pushed Z3
/// into symbolic fuel unfolding — 150 s+ timeouts (`dafny verify`
/// exit 4) on a file whose universal lemmas verify in ~1 s, so the
/// exit-status gate failed an otherwise-proven file. Post-fix each
/// sample assert is seeded with the universal lemma instantiated at
/// the sample values and the file verifies end-to-end in seconds.
/// `passed` is asserted explicitly: a timeout leaves the parsed error
/// count at 0 and surfaces ONLY in the exit status, so an errors-only
/// assert cannot catch this regression.
#[test]
fn proof_dafny_rational_samples_no_timeout() {
    assert_dafny_verifies_and_passes("tests/fixtures/rational_probe.av", "aver-dafny-rational");
}

/// Generality pin for the when-universal quarantine lane: a synthetic
/// decimal scanner (fresh names — `tests/fixtures/when_lane_sign.av`)
/// with a `when v > 0` sign law closes universally in the lane
/// (`when_universal >= 1`), proving the recognizer keys on structure,
/// not on the json corpus's identifiers.
#[test]
fn proof_when_universal_lane_closes_synthetic_sign_law() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping when-universal lane test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-when-lane-synth");
    let (summary, run) =
        run_lean_check_json("tests/fixtures/when_lane_sign.av", &output_dir, 0, &[]);
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(0),
        "synthetic lane fixture must stay sorry-free in the counted build.\n{}",
        format_output(&run)
    );
    assert_eq!(summary["passed"].as_bool(), Some(true));
    assert!(
        summary["when_universal"].as_u64().unwrap_or(0) >= 1,
        "the synthetic sign when-law must close universally in the lane \
         (when_universal >= 1).\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// End-to-end pin of the when-universal quarantine lane on the json
/// corpus — the validating cargo of the lane chunk, plus the iron
/// guard made executable:
/// 1. normal run: the five scalar-sign when-laws earn per-declaration
///    universal credit (`when_universal == 5`), the per-law detail
///    artifact lists their axiom evidence, the lane modules carry NO
///    `sorry` token, and the COUNTED summary is byte-identical to
///    today (sorries == 2 exact, passed, file-level universal:false);
/// 2. SABOTAGE run (one lane proof deliberately broken via the
///    `AVER_PROOF_LANE_SABOTAGE` test hook): the counted summary is
///    untouched, the broken law reports bounded (no credit), every
///    neighbor keeps its credit — `when_universal == 4`;
/// 3. REVERT run (`AVER_PROOF_NO_UNIVERSAL_LANE`): the lane vanishes
///    (`when_universal == 0`, no lane index, no stale credit), counted
///    summary again identical.
#[test]
fn proof_when_universal_lane_json_end_to_end() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping when-universal lane test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-when-lane-json");

    // ---- run 1: normal -------------------------------------------------
    let (normal, run) = run_lean_check_json("examples/data/json.av", &output_dir, 2, &[]);
    assert_eq!(
        normal["sorries"].as_u64(),
        Some(2),
        "{}",
        format_output(&run)
    );
    assert_eq!(normal["passed"].as_bool(), Some(true));
    assert_eq!(
        normal["universal"].as_bool(),
        Some(false),
        "file-level `universal` keeps counted-build semantics (json has 2 \
         permanent caught sorries); lane credit is per-law via when_universal"
    );
    assert_eq!(
        normal["when_universal"].as_u64(),
        Some(5),
        "the five scalar-sign when-laws (dispatchNumberOrErr.fromIntRoundtrip, \
         startNumberDigits.fromPositiveIntRoundtrip, parseNumberSign.\
         fromNegativeIntRoundtrip, startSignDigit.negativeDigitRoundtrip, \
         scanIntTail.fromCanonicalIntTail) must all close in the lane.\n{}",
        format_output(&run)
    );
    // Per-law detail artifact: exact set, all credited, evidence quoted.
    let detail: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(output_dir.join("when_universal_laws.json"))
            .expect("when_universal_laws.json must be written"),
    )
    .expect("detail artifact must parse");
    let laws = detail["laws"].as_array().expect("laws array");
    let mut labels: Vec<&str> = laws.iter().filter_map(|l| l["law"].as_str()).collect();
    labels.sort_unstable();
    assert_eq!(
        labels,
        vec![
            "dispatchNumberOrErr.fromIntRoundtrip",
            "parseNumberSign.fromNegativeIntRoundtrip",
            "scanIntTail.fromCanonicalIntTail",
            "startNumberDigits.fromPositiveIntRoundtrip",
            "startSignDigit.negativeDigitRoundtrip",
        ],
        "exact lane law set (exact in both directions, like the budgets)"
    );
    for law in laws {
        assert_eq!(
            law["universal"].as_bool(),
            Some(true),
            "law {} lost lane credit: {}",
            law["law"],
            law["evidence"]
        );
        let evidence = law["evidence"].as_str().unwrap_or("");
        assert!(
            evidence.contains("depends on axioms: [propext, Classical.choice, Quot.sound]"),
            "per-declaration #print axioms evidence must be quoted: {evidence}"
        );
    }
    // L2 of the iron guard: zero sorry tokens anywhere in the lane.
    let lane_index: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(output_dir.join("_aver_universal_lane.json"))
            .expect("lane index must be written"),
    )
    .expect("lane index must parse");
    for law in lane_index["laws"].as_array().expect("laws") {
        let module = law["module"].as_str().expect("module");
        let content = std::fs::read_to_string(
            output_dir
                .join("universal_lane")
                .join(format!("{module}.lean")),
        )
        .expect("lane module file must exist");
        assert!(
            !content.contains("sorry"),
            "no_sorry_token_in_universal_module violated by {module}"
        );
    }

    // ---- run 2: sabotage -------------------------------------------------
    let (sabotaged, run2) = run_lean_check_json(
        "examples/data/json.av",
        &output_dir,
        2,
        &[("AVER_PROOF_LANE_SABOTAGE", "startSignDigit")],
    );
    assert_eq!(
        counted_summary(&sabotaged),
        counted_summary(&normal),
        "a hard lane failure must leave the counted summary byte-identical.\n{}",
        format_output(&run2)
    );
    assert_eq!(
        sabotaged["when_universal"].as_u64(),
        Some(4),
        "exactly the sabotaged law loses credit.\n{}",
        format_output(&run2)
    );
    let detail2: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(output_dir.join("when_universal_laws.json")).expect("artifact"),
    )
    .expect("detail artifact must parse");
    for law in detail2["laws"].as_array().expect("laws") {
        let expected = law["law"].as_str() != Some("startSignDigit.negativeDigitRoundtrip");
        assert_eq!(
            law["universal"].as_bool(),
            Some(expected),
            "sabotage must not leak into neighbors: {} -> {}",
            law["law"],
            law["evidence"]
        );
    }

    // ---- run 3: revert (lane disabled) ------------------------------------
    let (reverted, run3) = run_lean_check_json(
        "examples/data/json.av",
        &output_dir,
        2,
        &[("AVER_PROOF_NO_UNIVERSAL_LANE", "1")],
    );
    assert_eq!(
        counted_summary(&reverted),
        counted_summary(&normal),
        "lane-off counted summary must be byte-identical.\n{}",
        format_output(&run3)
    );
    assert_eq!(
        reverted["when_universal"].as_u64(),
        Some(0),
        "lane disabled -> when_universal drops to 0"
    );
    assert!(
        !output_dir.join("_aver_universal_lane.json").exists(),
        "stale lane index must be retired when the lane is disabled"
    );

    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Bridge-premise family, hard floor: TIP isaplanner prop_85 (zip-rev
/// under the relational premise `natEq(len(xs), len(ys))`) closes
/// GENUINELY through the quarantine lane — `when_universal == 1` keyed
/// on per-declaration `#print axioms` evidence within the kernel
/// whitelist — while the COUNTED summary stays byte-identical to
/// main's (passed, 0 sorries, file-level universal:false). A sabotage
/// run pins the iron guard on this family too.
#[test]
fn proof_when_universal_lane_closes_tip_prop_85() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping when-universal lane test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-when-lane-prop85");
    let (normal, run) = run_lean_check_json(
        "proof-corpus/tip/isaplanner/prop_85.av",
        &output_dir,
        0,
        &[],
    );
    assert_eq!(
        normal["sorries"].as_u64(),
        Some(0),
        "{}",
        format_output(&run)
    );
    assert_eq!(normal["passed"].as_bool(), Some(true));
    assert_eq!(
        normal["universal"].as_bool(),
        Some(false),
        "file-level `universal` keeps counted-build semantics; the lane \
         credit is per-law via when_universal"
    );
    assert_eq!(
        normal["when_universal"].as_u64(),
        Some(1),
        "prop_85's zip.zipRev must close universally in the lane.\n{}",
        format_output(&run)
    );
    // Per-law detail artifact: kernel-genuine evidence, quoted.
    let detail: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(output_dir.join("when_universal_laws.json"))
            .expect("when_universal_laws.json must be written"),
    )
    .expect("detail artifact must parse");
    let laws = detail["laws"].as_array().expect("laws array");
    assert_eq!(laws.len(), 1);
    assert_eq!(laws[0]["law"].as_str(), Some("zip.zipRev"));
    assert_eq!(laws[0]["universal"].as_bool(), Some(true));
    assert_eq!(
        laws[0]["evidence"].as_str(),
        Some("'zip_law_zipRev_universal' depends on axioms: [propext, Quot.sound]"),
        "per-declaration #print axioms evidence must be quoted verbatim"
    );
    // L2 of the iron guard: zero sorry tokens in the lane module — the
    // snoc-distribution aux lemma is rendered from the validated
    // template, never emitted as a hole.
    let lane_index: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(output_dir.join("_aver_universal_lane.json"))
            .expect("lane index must be written"),
    )
    .expect("lane index must parse");
    for law in lane_index["laws"].as_array().expect("laws") {
        let module = law["module"].as_str().expect("module");
        let content = std::fs::read_to_string(
            output_dir
                .join("universal_lane")
                .join(format!("{module}.lean")),
        )
        .expect("lane module file must exist");
        assert!(
            !content.contains("sorry"),
            "no_sorry_token_in_universal_module violated by {module}"
        );
        assert!(
            content.contains("_snoc"),
            "the snoc-distribution aux lemma must be emitted as a lane-local lemma"
        );
    }
    // Sabotage: the broken lane proof costs exactly "prop_85 stays
    // bounded" — counted summary byte-identical, no credit.
    let (sabotaged, run2) = run_lean_check_json(
        "proof-corpus/tip/isaplanner/prop_85.av",
        &output_dir,
        0,
        &[("AVER_PROOF_LANE_SABOTAGE", "zipRev")],
    );
    assert_eq!(
        counted_summary(&sabotaged),
        counted_summary(&normal),
        "a hard lane failure must leave the counted summary byte-identical.\n{}",
        format_output(&run2)
    );
    assert_eq!(sabotaged["when_universal"].as_u64(), Some(0));

    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Generality pin for the bridge-premise family: the fresh-named
/// fixture (`tests/fixtures/when_lane_bridge.av`) closes BOTH its
/// bridge-shaped when-laws end-to-end — the zip-rev figure under
/// `likeNat(bulk(xs), bulk(ys))` and the count-insert figure under the
/// negated `unlikeNat(p, q)` — proving the recognizer keys on the
/// premise's structure, not on the proof-corpus identifiers. The
/// sabotage run additionally pins neighbor isolation on a two-law
/// lane file: the broken law reports bounded, its neighbor keeps
/// credit, the counted summary is untouched.
#[test]
fn proof_when_universal_lane_closes_synthetic_bridge_laws() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping when-universal lane test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-when-lane-bridge");
    let (normal, run) =
        run_lean_check_json("tests/fixtures/when_lane_bridge.av", &output_dir, 0, &[]);
    assert_eq!(
        normal["sorries"].as_u64(),
        Some(0),
        "the bridge fixture must stay sorry-free in the counted build.\n{}",
        format_output(&run)
    );
    assert_eq!(normal["passed"].as_bool(), Some(true));
    assert_eq!(
        normal["when_universal"].as_u64(),
        Some(2),
        "both fresh-named bridge laws must close in the lane.\n{}",
        format_output(&run)
    );
    let detail: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(output_dir.join("when_universal_laws.json")).expect("artifact"),
    )
    .expect("detail artifact must parse");
    let mut labels: Vec<&str> = detail["laws"]
        .as_array()
        .expect("laws")
        .iter()
        .filter_map(|l| l["law"].as_str())
        .collect();
    labels.sort_unstable();
    assert_eq!(
        labels,
        vec!["duoUp.duoFlip", "tallyUp.tallyWedgeNeq"],
        "exact lane law set (exact in both directions, like the budgets)"
    );

    // Sabotage one bridge law: neighbors keep credit, counted untouched.
    let (sabotaged, run2) = run_lean_check_json(
        "tests/fixtures/when_lane_bridge.av",
        &output_dir,
        0,
        &[("AVER_PROOF_LANE_SABOTAGE", "duoFlip")],
    );
    assert_eq!(
        counted_summary(&sabotaged),
        counted_summary(&normal),
        "a hard lane failure must leave the counted summary byte-identical.\n{}",
        format_output(&run2)
    );
    assert_eq!(sabotaged["when_universal"].as_u64(), Some(1));
    let detail2: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(output_dir.join("when_universal_laws.json")).expect("artifact"),
    )
    .expect("detail artifact must parse");
    for law in detail2["laws"].as_array().expect("laws") {
        let expected = law["law"].as_str() != Some("duoUp.duoFlip");
        assert_eq!(
            law["universal"].as_bool(),
            Some(expected),
            "sabotage must not leak into neighbors: {} -> {}",
            law["law"],
            law["evidence"]
        );
    }

    let _ = std::fs::remove_dir_all(&output_dir);
}

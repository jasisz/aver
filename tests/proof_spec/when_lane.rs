use super::*;

/// Tripwire against the silent-absorption proof shape: emitted lane
/// text must never apply a lane-law twin or companion through a
/// `first | exact …` alternative. That shape was measured to succeed
/// against a broken companion and carry its axioms into a green build;
/// lane proofs reference other lane laws ONLY via explicit
/// `have … := <companion> …` applications. Scans every module listed
/// in the lane index of `output_dir`.
fn assert_lane_never_first_exact_lane_theorem(output_dir: &std::path::Path) {
    let Ok(raw) = std::fs::read_to_string(output_dir.join("_aver_universal_lane.json")) else {
        return; // no lane emitted — nothing to scan
    };
    let index: serde_json::Value = serde_json::from_str(&raw).expect("lane index must parse");
    let laws = index["laws"].as_array().cloned().unwrap_or_default();
    let mut lane_names: Vec<String> = Vec::new();
    for law in &laws {
        let theorem = law["theorem"].as_str().expect("theorem name");
        lane_names.push(theorem.to_string());
        if let Some(base) = theorem.strip_suffix("_universal") {
            lane_names.push(format!("{base}_prop"));
        }
    }
    for law in &laws {
        let module = law["module"].as_str().expect("module name");
        let content = std::fs::read_to_string(
            output_dir
                .join("universal_lane")
                .join(format!("{module}.lean")),
        )
        .expect("lane module file must exist");
        for line in content.lines() {
            let in_alternative = line.trim_start().starts_with('|') || line.contains("first");
            if !in_alternative {
                continue;
            }
            for name in &lane_names {
                assert!(
                    !line.contains(&format!("exact {name}")),
                    "lane module {module} applies lane theorem {name} inside a \
                     `first`/`|` alternative — the silent-absorption shape: {line}"
                );
            }
        }
    }
}

/// Nonlinear-arithmetic wall (`tests/fixtures/nr_wall.av`):
/// laws whose unfolded cone multiplies two VARIABLES (`x * x`,
/// `(s*s - d*x)^2`) must DEGRADE to honest caught sorries, never to a
/// failing tactic. Pre-fix this file produced three distinct build
/// errors: `by_cases h_h_a : h_a ≥ 0` (case over a premise-HYPOTHESIS
/// name — application type mismatch), `omega` failing on a nonlinear
/// goal, and `_sample_N` theorems FALSE AS STATED (when-guard numerals
/// elaborated as Nat, truncating subtraction). Post-fix the export
/// builds GREEN: every `when`-guarded law closed bounded over its
/// declared domain and every emitted sample theorem true (Int-ascribed
/// guards).
///
/// The SHAPE-GATED `grind` rung (admitted on flat algebraic/ring goals,
/// skipped on inductive ones) now closes the unconditional nonlinear
/// polynomial ring identity `nrNewErrNum ≍ nrOldErrSq`
/// (`s⁴ - d·(x·(2s² - dx)) = (s² - dx)²`) — grind's `+ring` subsolver
/// carries the multi-variable nonlinear identity the hand-rolled AC-ring
/// simp package stopped normalizing at Lean 4.31. So the wall now lands
/// on exactly 1 honest caught sorry — the genuinely undecidable-by-grind
/// `sqNonneg` (`x·x ≥ 0`, needs `mul_self_nonneg`, off the whitelist
/// here) — down from 2. grind pulls `Classical.choice` on the closure
/// (whitelisted: ⊆ {propext, Classical.choice, Quot.sound}).
#[test]
fn proof_nonlinear_laws_degrade_to_honest_sorries() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping nonlinear-wall proof test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-nr-wall");
    let (summary, run) = run_lean_check_json("tests/fixtures/nr_wall.av", &output_dir, 1, &[]);
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(1),
        "the nonlinear wall must land on exactly 1 honest caught sorry — \
         the shape-gated grind rung closes the nrNewErrNum≍nrOldErrSq ring \
         identity, leaving only sqNonneg (a build error OR a different count \
         is a regression).\n{}",
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

/// Wide single-given domain (`tests/fixtures/wide_domain_law.av`):
/// a conditional law whose one given spans `0..299` makes
/// `law_theorem_prop` prepend a 300-way `a = v0 ∨ … ∨ a = v299`
/// disjunction. Unpartitioned, that statement blows Lean's default
/// `maxRecDepth` during elaboration (the scout bisected the wall at 252
/// values) and the WHOLE file fails to build — every law in it loses its
/// caught-sorry floor. Partitioning the domain into `_partN` theorems
/// keeps each part's disjunction below the wall, so the file builds green
/// and the check passes. Live lake.
#[test]
fn proof_wide_domain_law_partitions_and_builds_green() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping wide-domain proof test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-wide-domain");
    let (summary, run) =
        run_lean_check_json("tests/fixtures/wide_domain_law.av", &output_dir, 0, &[]);
    assert_eq!(
        summary["passed"].as_bool(),
        Some(true),
        "the wide-domain export must BUILD green — without partitioning the \
         300-way disjunction exceeds maxRecDepth and the whole file fails.\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(0),
        "the partitioned bounded law closes its sample/checked-domain checks.\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["bounded_laws"].as_u64(),
        Some(1),
        "the partitioned `_partN` theorems fold to ONE bounded law in the audit.\n{}",
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
    assert_lane_never_first_exact_lane_theorem(&output_dir);
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// End-to-end pin of the when-universal quarantine lane on the json
/// corpus — the validating cargo of the lane chunk, plus the iron
/// guard made executable:
/// 1. normal run: the five scalar-sign when-laws earn per-declaration
///    universal credit (`when_universal == 5`), the per-law detail
///    artifact lists their axiom evidence, the lane modules carry NO
///    `sorry` token, and the COUNTED summary is byte-identical to
///    today (sorries == 0 exact since the escaped-string roundtrip
///    pair closes via `StringEscapeRoundtrip`, passed, file-level
///    universal:true);
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
    let (normal, run) = run_lean_check_json("examples/data/json.av", &output_dir, 0, &[]);
    assert_eq!(
        normal["sorries"].as_u64(),
        Some(0),
        "{}",
        format_output(&run)
    );
    assert_eq!(normal["passed"].as_bool(), Some(true));
    assert_eq!(
        normal["universal"].as_bool(),
        Some(true),
        "file-level `universal` keeps counted-build semantics (json's manifest \
         laws all close kernel-clean since the escaped-string roundtrip pair \
         closes via StringEscapeRoundtrip); lane credit stays per-law via \
         when_universal"
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
    assert_eq!(
        (
            normal["universal_laws"].as_u64(),
            normal["bounded_laws"].as_u64(),
        ),
        (Some(10), Some(19)),
        "explicit law counts from the counted build: ten law theorems \
         certified universal, nineteen classed bounded-domain (the guarded \
         when-law enumerations) — exact in both directions like the \
         budgets.\n{}",
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
    assert_lane_never_first_exact_lane_theorem(&output_dir);

    // ---- run 2: sabotage -------------------------------------------------
    let (sabotaged, run2) = run_lean_check_json(
        "examples/data/json.av",
        &output_dir,
        0,
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
        0,
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
    assert_lane_never_first_exact_lane_theorem(&output_dir);
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

/// Real floor-grid corpus task: the helper law (`floorQ.cellFloorStable`)
/// proves floor stability inside one finer cell, and the later consumer
/// law (`coarseFloorEq.sharedCellFloor`) consumes it at both sides of a
/// coarser-cell equality. Mirrors the prop_85 / floor-fixture template:
/// `when_universal == 2` exactly, the consumer -> helper `imports` edge
/// read from the lane index, and both sabotage legs (helper break drops
/// BOTH to 0 with a byte-identical counted summary; consumer break leaves
/// the helper credited at 1). The pin uses only the JSON summary/detail
/// outputs and never depends on a generated lane module hash.
#[test]
fn proof_when_universal_lane_closes_cell_floor_grid() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping when-universal lane test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-cell-floor-grid");
    let (normal, run) = run_lean_check_json(
        "proof-corpus/handwritten/cell_floor_grid.av",
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
        Some(2),
        "both floor-grid laws must close universally in the lane.\n{}",
        format_output(&run)
    );

    let detail: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(output_dir.join("when_universal_laws.json"))
            .expect("when_universal_laws.json must be written"),
    )
    .expect("detail artifact must parse");
    let laws = detail["laws"].as_array().expect("laws array");
    let labels: Vec<&str> = laws.iter().filter_map(|l| l["law"].as_str()).collect();
    assert_eq!(
        labels,
        vec!["floorQ.cellFloorStable", "coarseFloorEq.sharedCellFloor"],
        "exact floor-grid lane law set"
    );
    for law in laws {
        assert_eq!(
            law["universal"].as_bool(),
            Some(true),
            "law {} lost lane credit: {}",
            law["law"],
            law["evidence"]
        );
        let thm = law["theorem"].as_str().unwrap();
        let evidence = law["evidence"].as_str().unwrap_or("");
        // Per-declaration `#print axioms` must be quoted verbatim and the axiom
        // set must lie within the sound whitelist {propext, Classical.choice,
        // Quot.sound}. The base cell-floor law closes with [propext, Quot.sound];
        // the coarse consumer's `simp` close pulls in Classical.choice on the
        // pinned Lean (whitelisted, still kernel-genuine), so accept either form.
        assert!(
            evidence == format!("'{thm}' depends on axioms: [propext, Quot.sound]")
                || evidence
                    == format!(
                        "'{thm}' depends on axioms: [propext, Classical.choice, Quot.sound]"
                    ),
            "per-declaration #print axioms evidence must be quoted verbatim \
             within the sound whitelist: {evidence}"
        );
    }
    assert_lane_never_first_exact_lane_theorem(&output_dir);

    // The lane index records the consumer -> helper dependency edge. Read
    // the module names from the index; never hardcode a folded hash.
    let lane_index: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(output_dir.join("_aver_universal_lane.json"))
            .expect("lane index must be written"),
    )
    .expect("lane index must parse");
    let lane_laws = lane_index["laws"].as_array().expect("lane laws");
    let helper = &lane_laws[0];
    let consumer = &lane_laws[1];
    assert!(
        helper["imports"].as_array().is_some_and(|a| a.is_empty()),
        "the source-earlier helper imports no lane module"
    );
    assert_eq!(
        consumer["imports"][0].as_str(),
        helper["module"].as_str(),
        "the lane index records the consumer -> helper dependency edge"
    );

    // ---- sabotage the HELPER -> the consumer falls with it -------------
    let (sab_helper, run_h) = run_lean_check_json(
        "proof-corpus/handwritten/cell_floor_grid.av",
        &output_dir,
        0,
        &[("AVER_PROOF_LANE_SABOTAGE", "cellFloorStable")],
    );
    assert_eq!(
        counted_summary(&sab_helper),
        counted_summary(&normal),
        "a broken helper cannot perturb the counted summary.\n{}",
        format_output(&run_h)
    );
    assert_eq!(
        sab_helper["when_universal"].as_u64(),
        Some(0),
        "the consumer must lose credit in the SAME run its helper breaks.\n{}",
        format_output(&run_h)
    );

    // ---- sabotage only the CONSUMER -> helper survives -----------------
    let (sab_consumer, run_c) = run_lean_check_json(
        "proof-corpus/handwritten/cell_floor_grid.av",
        &output_dir,
        0,
        &[("AVER_PROOF_LANE_SABOTAGE", "sharedCellFloor")],
    );
    assert_eq!(
        counted_summary(&sab_consumer),
        counted_summary(&normal),
        "a broken consumer cannot perturb the counted summary.\n{}",
        format_output(&run_c)
    );
    assert_eq!(
        sab_consumer["when_universal"].as_u64(),
        Some(1),
        "the upstream helper keeps its credit when only the consumer breaks.\n{}",
        format_output(&run_c)
    );

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
    assert_lane_never_first_exact_lane_theorem(&output_dir);

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

/// CH-2 two-module chain (lane imports + hash folding), live: with the
/// `AVER_PROOF_LANE_CHAIN` hook the later bridge law (`tallyWedgeNeq`,
/// the CONSUMER) imports the earlier one (`duoFlip`, the HELPER) — a
/// real lane-to-lane dependency graph against live lake.
/// 1. normal chain: BOTH laws credit (`when_universal == 2`); the lane
///    index records the dependency edge (`tallyWedgeNeq.imports` =
///    [duoFlip's module]); the counted summary is byte-identical to the
///    edgeless run.
/// 2. SABOTAGE the HELPER (`duoFlip`): its tolerated build fails AND so
///    does the consumer's — the consumer imports the un-built helper —
///    so BOTH lose credit (`when_universal == 0`); counted summary
///    untouched.
/// 3. SABOTAGE only the CONSUMER (`tallyWedgeNeq`): the helper is
///    upstream of the failure and keeps its credit
///    (`when_universal == 1`) — the edge does not leak the other way.
#[test]
fn proof_when_universal_lane_two_module_chain() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping when-universal lane chain test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-when-lane-chain");

    // ---- run 1: normal chain ------------------------------------------
    let (normal, run) = run_lean_check_json(
        "tests/fixtures/when_lane_bridge.av",
        &output_dir,
        0,
        &[("AVER_PROOF_LANE_CHAIN", "1")],
    );
    assert_eq!(
        normal["sorries"].as_u64(),
        Some(0),
        "{}",
        format_output(&run)
    );
    assert_eq!(normal["passed"].as_bool(), Some(true));
    assert_eq!(
        normal["when_universal"].as_u64(),
        Some(2),
        "both laws of the chain credit (the consumer's import of the helper does not \
         break its own proof).\n{}",
        format_output(&run)
    );
    // The dependency edge is recorded in the lane index.
    let lane_index: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(output_dir.join("_aver_universal_lane.json"))
            .expect("lane index must be written"),
    )
    .expect("lane index must parse");
    let laws = lane_index["laws"].as_array().expect("laws");
    let helper = laws
        .iter()
        .find(|l| l["law"].as_str() == Some("duoUp.duoFlip"))
        .expect("helper law in the lane index");
    let consumer = laws
        .iter()
        .find(|l| l["law"].as_str() == Some("tallyUp.tallyWedgeNeq"))
        .expect("consumer law in the lane index");
    assert!(
        helper["imports"].as_array().is_some_and(|a| a.is_empty()),
        "the source-earlier helper imports nothing"
    );
    assert_eq!(
        consumer["imports"].as_array().map(|a| a.len()),
        Some(1),
        "the consumer records exactly one dependency edge"
    );
    assert_eq!(
        consumer["imports"][0].as_str(),
        helper["module"].as_str(),
        "the edge points at the helper's module"
    );
    // The consumer module file actually imports the helper module.
    let consumer_module = consumer["module"].as_str().expect("consumer module");
    let consumer_src = std::fs::read_to_string(
        output_dir
            .join("universal_lane")
            .join(format!("{consumer_module}.lean")),
    )
    .expect("consumer module file");
    assert!(
        consumer_src.contains(&format!("import {}", helper["module"].as_str().unwrap())),
        "the consumer module imports the helper module"
    );

    // ---- run 2: sabotage the HELPER -> both fail ----------------------
    let (sab_helper, run2) = run_lean_check_json(
        "tests/fixtures/when_lane_bridge.av",
        &output_dir,
        0,
        &[
            ("AVER_PROOF_LANE_CHAIN", "1"),
            ("AVER_PROOF_LANE_SABOTAGE", "duoFlip"),
        ],
    );
    assert_eq!(
        counted_summary(&sab_helper),
        counted_summary(&normal),
        "a helper failure cannot perturb the counted summary.\n{}",
        format_output(&run2)
    );
    assert_eq!(
        sab_helper["when_universal"].as_u64(),
        Some(0),
        "sabotaging the helper fails BOTH the helper and the consumer (which imports \
         it) — no credit survives the chain.\n{}",
        format_output(&run2)
    );

    // ---- run 3: sabotage only the CONSUMER -> helper survives ---------
    let (sab_consumer, run3) = run_lean_check_json(
        "tests/fixtures/when_lane_bridge.av",
        &output_dir,
        0,
        &[
            ("AVER_PROOF_LANE_CHAIN", "1"),
            ("AVER_PROOF_LANE_SABOTAGE", "tallyWedgeNeq"),
        ],
    );
    assert_eq!(
        counted_summary(&sab_consumer),
        counted_summary(&normal),
        "a consumer failure cannot perturb the counted summary.\n{}",
        format_output(&run3)
    );
    assert_eq!(
        sab_consumer["when_universal"].as_u64(),
        Some(1),
        "sabotaging only the consumer leaves the upstream helper credited — the \
         dependency edge does not leak the other way.\n{}",
        format_output(&run3)
    );
    let detail3: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(output_dir.join("when_universal_laws.json")).expect("artifact"),
    )
    .expect("detail artifact must parse");
    for law in detail3["laws"].as_array().expect("laws") {
        let expected = law["law"].as_str() == Some("duoUp.duoFlip");
        assert_eq!(
            law["universal"].as_bool(),
            Some(expected),
            "only the helper keeps credit when the consumer is sabotaged: {} -> {}",
            law["law"],
            law["evidence"]
        );
    }

    let _ = std::fs::remove_dir_all(&output_dir);
}

/// CH-2 stale-`.olean` retirement: editing a helper law in the source
/// changes the helper's lane CONTENT, which folds into the importing
/// consumer's module hash — so the consumer's module NAME changes. No
/// pre-existing `.olean` under the old consumer name can satisfy the
/// probe, closing the masquerade window across the import chain. Pure
/// generation (no lake): asserts on the lane index the emitter writes.
#[test]
fn proof_when_universal_lane_stale_olean_retirement() {
    use std::path::PathBuf;
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let fixture = repo_root.join("tests/fixtures/when_lane_bridge.av");
    let original = std::fs::read_to_string(&fixture).expect("read bridge fixture");

    // Generate the chain once, record the consumer's module name.
    let consumer_module = |src_path: &std::path::Path, out: &std::path::Path| -> String {
        let run = Command::new(aver_bin)
            .current_dir(&repo_root)
            .arg("proof")
            .arg(src_path)
            .arg("--backend")
            .arg("lean")
            .arg("-o")
            .arg(out)
            .env("AVER_PROOF_LANE_CHAIN", "1")
            .output()
            .expect("aver proof must run");
        assert!(run.status.success(), "{}", format_output(&run));
        let index: serde_json::Value = serde_json::from_str(
            &std::fs::read_to_string(out.join("_aver_universal_lane.json"))
                .expect("lane index must be written"),
        )
        .expect("lane index parses");
        index["laws"]
            .as_array()
            .expect("laws")
            .iter()
            .find(|l| l["law"].as_str() == Some("tallyUp.tallyWedgeNeq"))
            .expect("consumer law")["module"]
            .as_str()
            .expect("consumer module")
            .to_string()
    };

    let out_a = temp_output_dir("aver-proof-stale-a");
    let module_before = consumer_module(&fixture, &out_a);

    // Edit the HELPER law in a fresh copy of the source: rename it. The
    // helper's lane module is re-derived (new name + content), and the
    // consumer imports it and folds its content — both feed the
    // consumer's hash, so the consumer's module name must move. (Still a
    // recognized bridge law; only its name changed.)
    let edited = original.replace(
        "verify duoUp law duoFlip\n",
        "verify duoUp law duoFlipEdited\n",
    );
    assert_ne!(edited, original, "the helper-edit substitution must apply");
    let edited_path = temp_output_dir("aver-stale-src").with_extension("av");
    std::fs::write(&edited_path, &edited).expect("write edited fixture");

    let out_b = temp_output_dir("aver-proof-stale-b");
    let module_after = consumer_module(&edited_path, &out_b);

    assert_ne!(
        module_before, module_after,
        "editing the helper law RENAMES the consumer module (hash folding) — a stale \
         `.olean` under the old consumer name can never pay for the changed helper"
    );

    let _ = std::fs::remove_file(&edited_path);
    let _ = std::fs::remove_dir_all(&out_a);
    let _ = std::fs::remove_dir_all(&out_b);
}

/// Lane consumption, pure generation (no lake): on the floor fixture
/// (`tests/fixtures/when_lane_floor.av`) the emitter
/// 1. proves the conditional floor-stability HELPER law in the lane;
/// 2. recognizes the later `sharedCell` law as a CONSUMER — the
///    helper's conclusion shape matches both sides of its proof cone
///    and every helper premise conjunct maps onto one of the
///    consumer's (the premise-subset rule) — and emits a module that
///    imports the helper's and applies its companion via explicit
///    `have` at both instantiations;
/// 3. DECLINES the `sharedCellUnguarded` variant, which is missing the
///    helper's `scale > 0` conjunct: no lane module, no failing build,
///    not even an omission note — recognition simply does not fire.
#[test]
fn proof_when_universal_lane_floor_consumption_emission() {
    use std::path::PathBuf;
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output_dir = temp_output_dir("aver-proof-floor-emit");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("tests/fixtures/when_lane_floor.av")
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("aver proof must run");
    assert!(run.status.success(), "{}", format_output(&run));

    let index: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(output_dir.join("_aver_universal_lane.json"))
            .expect("lane index must be written"),
    )
    .expect("lane index must parse");
    let laws = index["laws"].as_array().expect("laws");
    let labels: Vec<&str> = laws.iter().filter_map(|l| l["law"].as_str()).collect();
    assert_eq!(
        labels,
        vec!["binFloor.cellStable", "coarseAgree.sharedCell"],
        "exactly the helper and the consumer emit; the premise-dropping \
         variant declines (exact in both directions)"
    );
    assert!(
        index["omitted"].as_array().is_some_and(|o| o.is_empty()),
        "a declined consumer is not an omission note — recognition simply does not fire"
    );
    let helper = &laws[0];
    let consumer = &laws[1];
    assert!(
        helper["imports"].as_array().is_some_and(|a| a.is_empty()),
        "the helper imports no lane module"
    );
    assert_eq!(
        consumer["imports"][0].as_str(),
        helper["module"].as_str(),
        "the lane index records the consumer -> helper dependency edge"
    );

    // The consumer module: imports the helper, applies its companion
    // via explicit `have` at BOTH instantiations, and carries no sorry.
    let consumer_src = std::fs::read_to_string(
        output_dir
            .join("universal_lane")
            .join(format!("{}.lean", consumer["module"].as_str().unwrap())),
    )
    .expect("consumer lane module must exist");
    assert!(
        consumer_src.contains(&format!("import {}", helper["module"].as_str().unwrap())),
        "the consumer module imports the helper module"
    );
    assert!(
        consumer_src.contains("have hh0 := binFloor_law_cellStable_prop xn xs cell scale")
            && consumer_src.contains("have hh1 := binFloor_law_cellStable_prop yn ys cell scale"),
        "the helper companion is applied via explicit `have` at both matched \
         instantiations:\n{consumer_src}"
    );
    let helper_src = std::fs::read_to_string(
        output_dir
            .join("universal_lane")
            .join(format!("{}.lean", helper["module"].as_str().unwrap())),
    )
    .expect("helper lane module must exist");
    for (name, content) in [("consumer", &consumer_src), ("helper", &helper_src)] {
        assert!(
            !content.contains("sorry"),
            "no_sorry_token_in_universal_module violated by the {name} module"
        );
    }
    assert_lane_never_first_exact_lane_theorem(&output_dir);

    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Lane consumption end-to-end against live lake — a conditional law's
/// universal proof BUILDS ON an earlier proven conditional law of the
/// same file, both credited kernel-clean by the plain
/// `aver proof --check` pipeline (no test hooks):
/// 1. normal run: helper AND consumer earn per-declaration universal
///    credit (`when_universal == 2`, axiom evidence within the kernel
///    whitelist), while the premise-dropping variant stays bounded;
/// 2. SABOTAGE the helper: its tolerated build fails, and the consumer
///    — which imports the helper's module — fails WITH it in the same
///    run (`when_universal == 0`). No credited declaration can carry a
///    broken helper: the consumer's own `#print axioms` line is the
///    crediting evidence, and an absorbed axiom (sorryAx included)
///    surfaces there by axiom transitivity;
/// 3. SABOTAGE only the consumer: the upstream helper keeps its credit
///    (`when_universal == 1`) — the dependency edge does not leak
///    backwards.
/// The counted summary is byte-identical across all three runs.
#[test]
fn proof_when_universal_lane_floor_consumption_end_to_end() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lane consumption test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-floor-e2e");

    // ---- run 1: normal -------------------------------------------------
    let (normal, run) =
        run_lean_check_json("tests/fixtures/when_lane_floor.av", &output_dir, 0, &[]);
    assert_eq!(
        normal["sorries"].as_u64(),
        Some(0),
        "{}",
        format_output(&run)
    );
    assert_eq!(normal["passed"].as_bool(), Some(true));
    assert_eq!(
        normal["when_universal"].as_u64(),
        Some(2),
        "helper and consumer both close universally in the lane.\n{}",
        format_output(&run)
    );
    let detail: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(output_dir.join("when_universal_laws.json"))
            .expect("when_universal_laws.json must be written"),
    )
    .expect("detail artifact must parse");
    let laws = detail["laws"].as_array().expect("laws array");
    let labels: Vec<&str> = laws.iter().filter_map(|l| l["law"].as_str()).collect();
    assert_eq!(
        labels,
        vec!["binFloor.cellStable", "coarseAgree.sharedCell"]
    );
    for law in laws {
        assert_eq!(
            law["universal"].as_bool(),
            Some(true),
            "law {} lost lane credit: {}",
            law["law"],
            law["evidence"]
        );
        let thm = law["theorem"].as_str().unwrap();
        let evidence = law["evidence"].as_str().unwrap_or("");
        // Per-declaration `#print axioms` quoted verbatim, within the sound
        // whitelist {propext, Classical.choice, Quot.sound}. The base cell law
        // closes [propext, Quot.sound]; the coarse consumer's `simp` close pulls
        // in Classical.choice on the pinned Lean (whitelisted), so accept either.
        assert!(
            evidence == format!("'{thm}' depends on axioms: [propext, Quot.sound]")
                || evidence
                    == format!(
                        "'{thm}' depends on axioms: [propext, Classical.choice, Quot.sound]"
                    ),
            "per-declaration #print axioms evidence must be quoted verbatim \
             within the sound whitelist: {evidence}"
        );
    }
    assert_lane_never_first_exact_lane_theorem(&output_dir);

    // ---- run 2: sabotage the HELPER -> the consumer falls with it -----
    let (sab_helper, run2) = run_lean_check_json(
        "tests/fixtures/when_lane_floor.av",
        &output_dir,
        0,
        &[("AVER_PROOF_LANE_SABOTAGE", "cellStable")],
    );
    assert_eq!(
        counted_summary(&sab_helper),
        counted_summary(&normal),
        "a broken helper cannot perturb the counted summary.\n{}",
        format_output(&run2)
    );
    assert_eq!(
        sab_helper["when_universal"].as_u64(),
        Some(0),
        "a consumer must lose credit in the SAME run its helper breaks.\n{}",
        format_output(&run2)
    );
    let detail2: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(output_dir.join("when_universal_laws.json")).expect("artifact"),
    )
    .expect("detail artifact must parse");
    for law in detail2["laws"].as_array().expect("laws") {
        assert_eq!(
            law["universal"].as_bool(),
            Some(false),
            "no declaration may stay credited above a broken helper: {} -> {}",
            law["law"],
            law["evidence"]
        );
        assert!(
            !law["evidence"]
                .as_str()
                .unwrap_or_default()
                .contains("sorryAx"),
            "the audit must never see sorryAx on a credited declaration: {}",
            law["evidence"]
        );
    }

    // ---- run 3: sabotage only the CONSUMER -> helper survives ----------
    let (sab_consumer, run3) = run_lean_check_json(
        "tests/fixtures/when_lane_floor.av",
        &output_dir,
        0,
        &[("AVER_PROOF_LANE_SABOTAGE", "sharedCell")],
    );
    assert_eq!(
        counted_summary(&sab_consumer),
        counted_summary(&normal),
        "a broken consumer cannot perturb the counted summary.\n{}",
        format_output(&run3)
    );
    assert_eq!(
        sab_consumer["when_universal"].as_u64(),
        Some(1),
        "the upstream helper keeps its credit when only the consumer breaks.\n{}",
        format_output(&run3)
    );

    let _ = std::fs::remove_dir_all(&output_dir);
}

/// CH-2 collision guard, live: a sibling law literally named
/// `duoFlip_universal` emits the counted-build theorem name
/// (`duoUp_law_duoFlip_universal`) that the `duoFlip` lane TWIN would
/// claim. The guard HONESTLY OMITS `duoUp.duoFlip` — no lane module, no
/// credit attempt, an explicit note in the detail artifact — so the
/// clash never fails a tolerated build. The NEIGHBOR
/// (`tallyUp.tallyWedgeNeq`) keeps its credit (`when_universal == 1`);
/// before the guard the clash would fail the colliding law's tolerated
/// build and silently strip its credit with no note. The counted
/// summary is byte-identical to the no-clash bridge fixture's.
#[test]
fn proof_when_universal_lane_collision_guard_omits_clash_keeps_neighbor() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping when-universal lane collision test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-when-lane-collision");
    let (summary, run) =
        run_lean_check_json("tests/fixtures/when_lane_collision.av", &output_dir, 0, &[]);
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(0),
        "{}",
        format_output(&run)
    );
    assert_eq!(summary["passed"].as_bool(), Some(true));
    assert_eq!(
        summary["when_universal"].as_u64(),
        Some(1),
        "the colliding law is omitted; the neighbor keeps its credit — exactly one \
         lane law survives.\n{}",
        format_output(&run)
    );

    let detail: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(output_dir.join("when_universal_laws.json"))
            .expect("when_universal_laws.json must be written"),
    )
    .expect("detail artifact must parse");
    // The neighbor is credited.
    let laws = detail["laws"].as_array().expect("laws array");
    assert_eq!(laws.len(), 1, "exactly the neighbor stays in the lane");
    assert_eq!(laws[0]["law"].as_str(), Some("tallyUp.tallyWedgeNeq"));
    assert_eq!(
        laws[0]["universal"].as_bool(),
        Some(true),
        "the neighbor keeps its credit (the red baseline: it would silently lose it): {}",
        laws[0]["evidence"]
    );
    // The colliding law is an HONEST omission, naming the clash.
    let omitted = detail["omitted"].as_array().expect("omitted array");
    assert_eq!(omitted.len(), 1, "exactly the colliding law is omitted");
    assert_eq!(omitted[0]["law"].as_str(), Some("duoUp.duoFlip"));
    assert_eq!(
        omitted[0]["collides"].as_str(),
        Some("duoUp_law_duoFlip_universal"),
        "the omission names the exact clashing theorem"
    );
    assert!(
        omitted[0]["note"]
            .as_str()
            .is_some_and(|n| n.contains("skipped") && n.contains("clash")),
        "the omission carries an honest, surfaced note"
    );

    let _ = std::fs::remove_dir_all(&output_dir);
}

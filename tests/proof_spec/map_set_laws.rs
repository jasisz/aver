use super::*;

#[test]
fn knowledge_example_has_only_universal_laws_and_clean_axioms() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-knowledge-laws");
    let (summary, run) = run_lean_check_json("examples/formal/knowledge.av", &dir, 0, &[]);
    assert!(run.status.success(), "{}", format_output(&run));
    assert_eq!(summary["universal_laws"], 11, "{summary}");
    assert_eq!(summary["bounded_laws"], 0, "{summary}");
    assert_eq!(summary["sorries"], 0, "{summary}");
    let manifest: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(dir.join("proof_manifest.json")).unwrap())
            .unwrap();
    for law in manifest["laws"].as_array().unwrap() {
        assert_eq!(law["tier"], "universal", "{law}");
        for axiom in law["axioms"].as_array().unwrap() {
            assert!(
                ["propext", "Quot.sound", "Classical.choice"].contains(&axiom.as_str().unwrap()),
                "{law}"
            );
        }
    }
    let cases = Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .args(["verify", "examples/formal/knowledge.av"])
        .output()
        .unwrap();
    assert!(cases.status.success(), "{}", format_output(&cases));
    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn conflicting_sum_updates_cannot_launder_sample_success_into_a_proof() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-map-conflicting-changes");
    std::fs::create_dir_all(&dir).unwrap();
    let source = dir.join("main.av");
    std::fs::write(
        &source,
        r#"module ConflictingChanges
    intent = "The listed distinct-key samples commute; arbitrary changes do not."
    effects []
type Change
    Put(Int, Int)
    Ignore
fn apply(m: Map<Int, Int>, change: Change) -> Map<Int, Int>
    match change
        Change.Put(k, v) -> Map.set(m, k, v)
        Change.Ignore -> m
verify apply law missingAgreement
    given m: Map<Int, Int> = [{}]
    given a: Change = [Change.Put(1, 1)]
    given b: Change = [Change.Put(2, 2)]
    using []
    apply(apply(m, a), b) => apply(apply(m, b), a)
"#,
    )
    .unwrap();
    let (summary, run) = run_lean_check_json(source.to_str().unwrap(), &dir.join("proof"), 0, &[]);
    assert!(!run.status.success(), "{summary}");
    assert_eq!(summary["build_errors"], 0, "{summary}");
    assert_eq!(summary["universal_laws"], 0, "{summary}");
    assert_eq!(
        summary["obligations"]["apply.missingAgreement.implication"], "failed",
        "{summary}"
    );
    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn distinct_key_writes_commute_for_all_supported_scalar_orders() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-map-commutation");
    let (summary, run) = run_lean_check_json("tests/fixtures/map_commutation.av", &dir, 0, &[]);
    assert!(run.status.success(), "{}", format_output(&run));
    assert_eq!(summary["universal_laws"], 3, "{summary}");
    assert_eq!(summary["bounded_laws"], 0, "{summary}");
    assert_eq!(summary["sorries"], 0, "{summary}");
    // The fallback comparator deliberately cannot discharge the new theorem.
    // Check this with Lean's instance search, alongside an arbitrary malformed
    // list (not constructible as an Aver map) covered by the stronger model law.
    std::fs::write(
        dir.join("OrderBoundary.lean"),
        r#"import AverCommon
example : True := by
  fail_if_success have := inferInstanceAs (AverLawfulKeyOrder (Int × Int))
  trivial
example (v w : Int) :
    AverMap.set (AverMap.set [((5 : Int), 0), (1, 4), (5, 3)] 2 v) 4 w =
    AverMap.set (AverMap.set [((5 : Int), 0), (1, 4), (5, 3)] 4 w) 2 v := by
  exact AverMap.set_set_comm _ (2 : Int) 4 v w (by decide)
"#,
    )
    .unwrap();
    let check = Command::new("lake")
        .current_dir(&dir)
        .args(["env", "lean", "OrderBoundary.lean"])
        .output()
        .unwrap();
    assert!(check.status.success(), "{}", format_output(&check));
    let _ = std::fs::remove_dir_all(dir);
}

/// The prelude's facts about `Map.set` reach law proofs once the cone
/// touches a map operation. Three shapes over `Map<String, Int>`:
///
/// - `look.stableUnderOtherKey`: a lookup survives a store under another key
///   (`when look(m, k) == Some(v)` and `k != k2`); a `using []` law, so the
///   final implication goes through the reasons solver, which needs
///   `AverMap.get_set_ne`;
/// - `size.neverShrinks`: `size(apply(m, change)) >= size(m)` over a sum
///   `Change`, closed by the constructor split of the generic rung plus
///   `AverMap.len_set_ge`;
/// - `put.overwrites`: `put(put(m, k, v), k, w) == put(m, k, w)`, closed by
///   the generic rung's simp set plus `AverMap.set_set_self`.
///
/// On `origin/main` (`cbf3a354`) all three stay open: the reasons solver and
/// the generic `simp [cone, Int.add_sub_cancel]` rung never cite the map facts
/// and nothing splits the sum-typed argument. Every law must be `universal`
/// with zero sorries, and its kernel axioms must stay within
/// `propext`, `Quot.sound`, `Classical.choice`.
#[test]
fn map_set_facts_close_the_three_store_laws_universally() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-map-set-laws");
    let (summary, run) = run_lean_check_json("tests/fixtures/map_set_laws.av", &dir, 0, &[]);
    assert!(run.status.success(), "{}", format_output(&run));
    assert_eq!(summary["build_errors"], 0, "{}", format_output(&run));
    assert_eq!(summary["sorries"], 0, "{}", format_output(&run));
    assert_eq!(summary["universal_laws"], 3, "{}", format_output(&run));
    assert_eq!(
        summary["obligations"]["look.stableUnderOtherKey.implication"],
        "universal",
        "{}",
        format_output(&run)
    );
    let manifest: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(dir.join("proof_manifest.json")).unwrap())
            .unwrap();
    let allowed = ["propext", "Quot.sound", "Classical.choice"];
    for law in [
        "look.stableUnderOtherKey",
        "size.neverShrinks",
        "put.overwrites",
    ] {
        let record = manifest["laws"]
            .as_array()
            .unwrap()
            .iter()
            .find(|r| r["law"] == law)
            .unwrap_or_else(|| panic!("{law} missing from the manifest"));
        assert_eq!(record["tier"], "universal", "{law}: {record}");
        let axioms: Vec<&str> = record["axioms"]
            .as_array()
            .unwrap()
            .iter()
            .map(|a| a.as_str().unwrap())
            .collect();
        assert!(
            axioms.iter().all(|a| allowed.contains(a)),
            "{law} depends on {axioms:?}"
        );
    }
    let _ = std::fs::remove_dir_all(dir);
}

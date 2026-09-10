use super::*;

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

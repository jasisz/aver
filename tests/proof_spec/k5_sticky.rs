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

#[test]
fn k5_fraction_exponent_has_a_universal_magnitude_window() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-k5-fraction-exponent");
    let (summary, output) = run_lean_check_json_with_args(
        "projects/k5_fdiv/domain/kernel.av",
        &dir,
        0,
        &[],
        &["--module-root", "projects/k5_fdiv"],
    );
    assert!(output.status.success(), "{}", format_output(&output));
    assert_eq!(summary["build_errors"], 0, "{summary}");
    assert_eq!(summary["sorries"], 0, "{summary}");
    let manifest: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(dir.join("proof_manifest.json")).unwrap())
            .unwrap();
    let laws = manifest["laws"].as_array().unwrap();
    for name in [
        "Domain.Binade.positiveWindow.executableMagnitude",
        "Domain.Binade.negativeWindow.executableMagnitude",
        "Domain.Binade.magnitudeWindow.executableMagnitude",
        "Domain.Binade.significand.normalized",
        "twoPow.roundingScale",
        "fracExpo.executableMagnitude",
        "fracExpo.rationalMagnitude",
        "normalizedValueExponent.executableExponent",
        "Domain.ModelScale.modelWindow.normalizedMagnitude",
        "Domain.ModelScale.uniqueWindow.uniqueExponent",
        "Domain.BinadeOrder.fractionWindow.integerMagnitude",
        "Domain.IntegerOrder.multiplyLe.nonnegativeFactor",
        "Domain.FractionOrder.lt.fromPositiveCrossProduct",
        "binadeSig.significandWindow",
    ] {
        let law = laws
            .iter()
            .find(|law| law["law"] == name)
            .unwrap_or_else(|| panic!("missing law {name}"));
        assert_eq!(law["tier"], "universal", "{name}: {law}");
    }
    // These imported Round laws were already bounded with Kernel as the root.
    // Every other existing or new law must keep universal credit.
    let previously_bounded = [
        "Domain.Round.awayErrorBound.strictBound",
        "Domain.Round.stickyErrorBound.strictBound",
        "Domain.Round.truncErrorBound.strictBound",
        "Domain.Round.truncErrorSameSign.signCondition",
    ];
    for law in laws {
        if previously_bounded.contains(&law["law"].as_str().unwrap()) {
            assert!(
                matches!(law["tier"].as_str(), Some("bounded" | "universal")),
                "{law}"
            );
        } else {
            assert_eq!(law["tier"], "universal", "{law}");
        }
    }
    for obligation in manifest["obligations"].as_array().unwrap() {
        assert_eq!(obligation["tier"], "universal", "{obligation}");
    }
    for claim in laws
        .iter()
        .chain(manifest["obligations"].as_array().unwrap())
    {
        for axiom in claim["axioms"].as_array().unwrap() {
            assert!(
                matches!(
                    axiom.as_str(),
                    Some("propext" | "Classical.choice" | "Quot.sound")
                ),
                "{claim}"
            );
        }
    }
    let binade = std::fs::read_to_string(dir.join("Domain/Binade.lean")).unwrap();
    let kernel = std::fs::read_to_string(dir.join("Kernel.lean")).unwrap();
    assert!(!binade.contains("partial def"), "{binade}");
    assert!(!binade.contains("__fuel"), "{binade}");
    assert!(!kernel.contains("partial def expoBelowOne"), "{kernel}");
    let _ = std::fs::remove_dir_all(dir);
}

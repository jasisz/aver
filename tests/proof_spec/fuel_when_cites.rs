use super::*;

/// The theorem body of one law in the emitted `FuelWhenCites.lean`, cut at
/// the sampled-domain theorems that follow it.
fn law_body(lean: &str, base: &str) -> String {
    let start = lean
        .find(&format!("theorem {base} :"))
        .unwrap_or_else(|| panic!("{base} theorem must be emitted:\n{lean}"));
    let body = &lean[start..];
    let end = body.find("_checked_domain").unwrap_or(body.len());
    body[..end].to_string()
}

/// The ground instances a fuel-induction proof cites: the `have lN := …`
/// lines, trimmed.
fn cite_lines(body: &str) -> Vec<&str> {
    body.lines()
        .map(str::trim)
        .filter(|l| l.contains(":= digits_law_"))
        .collect()
}

/// A ladder of `when`-laws over ONE well-founded countdown
/// (`tests/fixtures/fuel_when_cites.av`): each rung bounds the length of the
/// base-256 digit writer's output and follows from the rung one digit below
/// it, at the shrunk value.
///
/// Export-structure pin (no toolchain needed). Before the fix an earlier
/// `when`-law reached the closer only as the raw implication `<when> = true ->
/// claim` — a conditional simp rewrite whose side condition (`(value / 256 <
/// 256) = true` from `value < 65536`) simp has to prove itself, which is
/// omega's job. Post-fix the cite carries its premise, discharged by the same
/// portfolio the IH instance uses, and the countdown fn appears in NO `by`
/// discharge.
#[test]
fn proof_export_fuel_cites_earlier_when_law_with_its_premise_discharged() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output_dir = temp_output_dir("aver-proof-fuel-when-cites-export");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("tests/fixtures/fuel_when_cites.av")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("aver proof should run");
    assert!(run.status.success(), "{}", format_output(&run));
    let lean = std::fs::read_to_string(output_dir.join("FuelWhenCites.lean"))
        .expect("FuelWhenCites.lean must be emitted");

    let body = law_body(&lean, "digits_law_atMostTwoDigitsBelow65536");
    assert!(
        body.contains("have key : ∀ (k : Nat)"),
        "the rung must be proved by fuel induction:\n{body}"
    );
    // The rung one digit below, cited at the shrunk value WITH its premise —
    // `(value / 256 < 256) = true`, which omega reads off `value < 65536`.
    assert!(
        body.contains(
            "have l1 := digits_law_atMostOneDigitBelow256 (value / 256) (by first | omega"
        ),
        "the rung below must be cited at the shrunk value with a discharged \
         premise:\n{body}"
    );
    // A premise that does NOT hold at the shrunk value must drop its
    // instance, never admit it: the discharge has no `sorry` floor and the
    // whole `have` sits under `try`.
    for cite in cite_lines(&body) {
        assert!(
            cite.starts_with("try (have l") && cite.ends_with("| fail))"),
            "a when-law cite must be droppable, not admitted:\n{cite}"
        );
        assert!(
            !cite.contains("sorry"),
            "a when-law cite must not admit its premise:\n{cite}"
        );
        // Never key the discharge on the countdown fn: no unfold, no simp
        // set that could loop on its unconditional unfold equation.
        assert!(
            !cite.contains("unfold") && !cite.contains("FuelWhenCites.digits"),
            "the countdown fn must stay out of every premise discharge:\n{cite}"
        );
    }
    // The BOTTOM rung has no rung below it to cite; what it needs is the
    // countdown's terminal branch at the shrunk value, so the step arm's
    // closer keeps a second `unfold` as its last alternative before `sorry`.
    let bottom = law_body(&lean, "digits_law_atMostOneDigitBelow256");
    assert!(
        cite_lines(&bottom).is_empty(),
        "the bottom rung has no earlier rung to cite:\n{bottom}"
    );
    let deep = bottom
        .lines()
        .map(str::trim)
        .find(|l| l.contains("| ((unfold FuelWhenCites.digits; repeat' split) <;> simp_all"))
        .unwrap_or_else(|| panic!("the step closer must try a second unfold:\n{bottom}"));
    assert!(
        deep.starts_with("first | (split") && deep.ends_with("| sorry"),
        "the second unfold must sit behind the plain closers and above the \
         `sorry` floor:\n{deep}"
    );
    // Every rung is stated universally.
    for base in [
        "digits_law_atMostOneDigitBelow256",
        "digits_law_atMostTwoDigitsBelow65536",
        "digits_law_atMostThreeDigitsBelow16777216",
        "digits_law_atLeastOneDigitAboveZero",
        "digits_law_atLeastTwoDigitsFrom256",
    ] {
        assert!(
            lean.contains(&format!("-- aver:law-class {base} universal")),
            "{base} must be classed universal:\n{lean}"
        );
    }
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Live Lean gate: the whole ladder closes sorry-free and every rung earns
/// kernel-genuine `universal` credit. Credit here is transitive — a rung
/// proved with an admitted premise would put `sorryAx` in every rung above
/// it — so the count is the real signal.
#[test]
fn proof_fuel_when_cites_ladder_closes_kernel_genuine() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping fuel-when-cites proof test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-fuel-when-cites");
    let (summary, run) =
        run_lean_check_json("tests/fixtures/fuel_when_cites.av", &output_dir, 0, &[]);
    assert_eq!(
        summary["build_errors"].as_u64(),
        Some(0),
        "no discharge arm may become a logged build error.\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(0),
        "every rung must close sorry-free.\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["universal_laws"].as_u64(),
        Some(5),
        "all five rungs of the length ladder must be certified universal.\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["universal"].as_bool(),
        Some(true),
        "{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

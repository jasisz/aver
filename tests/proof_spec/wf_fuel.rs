use super::*;

/// Fuel induction over a well-founded Int countdown
/// (`tests/fixtures/wf_fuel_induction.av`): a base-256 digit writer
/// `digits(value, acc)` that recurses on `Int.div(value, 256)` under a
/// `value < 1` guard (emitted as a native `termination_by value.toNat`
/// def), an accumulator law about it, a structural reader with a snoc
/// law, and a `when m >= 0` round trip through both.
///
/// Export-structure pin (no toolchain needed). Before the fix a law
/// mentioning such a fn hit the generic ladder's blind `simp [digits]`
/// / `fun_induction digits … <;> simp_all [digits]` rungs, whose
/// unconditional unfold equation heartbeat-aborts — a HARD build error
/// `first | … | sorry` cannot catch, which cost the whole module its
/// tier. Post-fix the two `digits` laws carry the fuel-induction
/// skeleton (`have key : ∀ (k : Nat) …`, one `unfold` per fuel step),
/// the countdown fn sits in NO blind simp set and is NO `fun_induction`
/// target, and every law theorem is classed `universal`.
#[test]
fn proof_export_wf_fuel_induction_lean_structure() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output_dir = temp_output_dir("aver-proof-wf-fuel-export");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("tests/fixtures/wf_fuel_induction.av")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("aver proof should run");
    assert!(run.status.success(), "{}", format_output(&run));
    let lean = std::fs::read_to_string(output_dir.join("WfFuel.lean"))
        .expect("WfFuel.lean must be emitted");

    // The recursion class: a native well-founded def, never partial.
    assert!(
        !lean.contains("partial def"),
        "the floor-division countdown must emit as a well-founded def"
    );
    assert!(lean.contains("def digits"));
    assert!(lean.contains("termination_by value.toNat"));

    // Both `digits` laws carry the fuel-induction skeleton.
    for base in [
        "digits_law_accumulatorComesFirst",
        "digits_law_readsBackBigEndian",
    ] {
        let start = lean
            .find(&format!("theorem {base} :"))
            .unwrap_or_else(|| panic!("{base} theorem must be emitted:\n{lean}"));
        let body = &lean[start..];
        let end = body.find("_checked_domain").unwrap_or(body.len());
        let body = &body[..end];
        assert!(
            body.contains("have key : ∀ (k : Nat)"),
            "{base} must be proved by fuel induction:\n{body}"
        );
        assert!(
            body.contains("induction k with") && body.contains("| succ k ih =>"),
            "{base} must induct on the fuel:\n{body}"
        );
        assert!(
            body.contains("unfold WfFuel.digits"),
            "{base} must unfold the countdown fn once per fuel step:\n{body}"
        );
        assert!(
            body.contains("exact key _"),
            "{base} must project the fuel lemma at the countdown's own measure:\n{body}"
        );
        assert!(
            !body.contains("fun_induction digits") && !body.contains("fun_induction WfFuel.digits"),
            "{base} must not drive the countdown fn's .induct blindly:\n{body}"
        );
        assert!(
            !body.contains("simp_all [WfFuel.digits") && !body.contains("simp [WfFuel.digits"),
            "{base} must keep the countdown fn out of every blind simp set:\n{body}"
        );
        assert!(
            lean.contains(&format!("-- aver:law-class {base} universal")),
            "{base} must be classed universal"
        );
    }
    // The when-law is stated in TRUE universal form (no sampled-domain
    // disjunction on the theorem statement).
    assert!(
        lean.contains(
            "theorem digits_law_readsBackBigEndian : ∀ (m : Int), (m >= 0) = true -> bigEndian ((digits m []).reverse) 0 = m := by"
        ),
        "the when-law must drop its sampled domain:\n{lean}"
    );
    // The round trip cites the accumulator law as a GROUND instance (it is a
    // looping rewrite, so it never joins a simp set) and the ground IH at the
    // shrunk value.
    assert!(
        lean.contains("have l1 := digits_law_accumulatorComesFirst (m / 256) ((m % 256) :: [])"),
        "the round trip must cite the accumulator law at the recursive call:\n{lean}"
    );
    assert!(
        lean.contains("have ih1 := ih (m / 256) (by first | omega | sorry)"),
        "the round trip must instantiate the IH at the shrunk value:\n{lean}"
    );
    assert!(
        !lean.contains("simp_all [WfFuel.bigEndian, digits_law_accumulatorComesFirst"),
        "the looping accumulator law must not join a simp set:\n{lean}"
    );
    // The structural reader's snoc law is universal too.
    assert!(lean.contains("-- aver:law-class bigEndian_law_readsTheLastByteLast universal"));
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Documented DECLINE: the countdown position carrying an expression
/// (`digits(magnitudeOf(value), [])`) is out of the fuel strategy's scope
/// (no `generalize`), so the law keeps an honest `sorry` floor — and no
/// blind rung ever unfolds the countdown fn into a hard build error. The
/// export pin only checks the emitted shape; the lake gate below proves the
/// module still builds.
#[test]
fn proof_export_wf_fuel_declines_composite_countdown_arg_honestly() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output_dir = temp_output_dir("aver-proof-wf-fuel-decline");
    let source = std::fs::read_to_string(repo_root.join("tests/fixtures/wf_fuel_induction.av"))
        .expect("fixture source");
    let source = source.replace(
        "verify digits law readsBackBigEndian\n    given m: Int = [0, 1, 127, 128, 255, 256, 65535, 65536, 2147483647, 2147483648]\n    when m >= 0\n    bigEndian(List.reverse(digits(m, [])), 0) => m\n",
        "fn magnitudeOf(value: Int) -> Int\n    ? \"Size without sign.\"\n    match value < 0\n        true -> 0 - value\n        false -> value\n\nverify magnitudeOf\n    magnitudeOf(5) => 5\n    magnitudeOf(0 - 5) => 5\n\nverify digits law readsBackBigEndian\n    given value: Int = [-5, 0, 1, 255, 256, 2147483648]\n    bigEndian(List.reverse(digits(magnitudeOf(value), [])), 0) => magnitudeOf(value)\n",
    );
    assert!(
        source.contains("magnitudeOf(value), [])"),
        "fixture edit must apply"
    );
    let src_dir = temp_output_dir("aver-proof-wf-fuel-decline-src");
    std::fs::create_dir_all(&src_dir).expect("src dir");
    let src_path = src_dir.join("wf_fuel_decline.av");
    std::fs::write(&src_path, source).expect("write source");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg(&src_path)
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("aver proof should run");
    assert!(run.status.success(), "{}", format_output(&run));
    let lean = std::fs::read_to_string(output_dir.join("WfFuel.lean"))
        .expect("WfFuel.lean must be emitted");
    let start = lean
        .find("theorem digits_law_readsBackBigEndian :")
        .expect("round-trip theorem must be emitted");
    let body = &lean[start..];
    let end = body.find("_checked_domain").unwrap_or(body.len());
    let body = &body[..end];
    assert!(
        !body.contains("have key : ∀ (k : Nat)"),
        "a composite countdown arg is out of scope for fuel induction:\n{body}"
    );
    assert!(
        !body.contains("simp [WfFuel.digits")
            && !body.contains("simp_all [WfFuel.digits")
            && !body.contains("fun_induction digits")
            && !body.contains("fun_induction WfFuel.digits"),
        "the declined law must never unfold the countdown fn blindly:\n{body}"
    );
    assert!(
        body.contains("sorry"),
        "the declined law keeps its honest sorry floor:\n{body}"
    );
    let _ = std::fs::remove_dir_all(&output_dir);
    let _ = std::fs::remove_dir_all(&src_dir);
}

/// Live Lean gate: the fixture builds green with ZERO sorries and every law
/// theorem earns kernel-genuine `universal` credit — the fuel-induction
/// proofs close inside the axiom whitelist (`propext`, `Classical.choice`,
/// `Quot.sound`), no `native_decide`, no Mathlib. Before the fix the module
/// reported build errors (heartbeat timeouts) and no tier at all.
#[test]
fn proof_wf_fuel_induction_lean_closes_kernel_genuine() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping wf-fuel proof test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-wf-fuel");
    let (summary, run) =
        run_lean_check_json("tests/fixtures/wf_fuel_induction.av", &output_dir, 0, &[]);
    assert_eq!(
        summary["build_errors"].as_u64(),
        Some(0),
        "no blind rung may heartbeat-abort on the countdown fn.\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(0),
        "wf-fuel laws must close sorry-free.\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["passed"].as_bool(),
        Some(true),
        "{}",
        format_output(&run)
    );
    assert_eq!(
        summary["universal"].as_bool(),
        Some(true),
        "all three law theorems are stated universally and must be \
         kernel-genuine (axioms within the whitelist).\n{}",
        format_output(&run)
    );
    assert_eq!(
        (
            summary["universal_laws"].as_u64(),
            summary["bounded_laws"].as_u64(),
        ),
        (Some(3), Some(0)),
        "explicit law counts: exactly the three universal-classed law \
         theorems certified, none degraded to bounded-domain.\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

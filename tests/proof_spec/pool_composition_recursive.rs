use super::*;

const POOL_LAWS: &str = "grind [bigEndian_law_readsTheLastByteLast, \
    digits_law_accumulatorComesFirst, digits_law_readsBackBigEndian]";

/// Keystone composition as the FLOOR of an unconditional law over a
/// well-founded countdown (`tests/fixtures/pool_composition_recursive.av`):
/// `digits(value, acc)` recurses on `Int.div(value, 256)` (a native
/// `termination_by value.toNat` def), its own laws close by fuel induction,
/// and the top law `readsBackDigits` feeds the countdown an EXPRESSION
/// (`digits(magnitudeOf(value), [])`) — out of the fuel strategy's scope.
/// Such a cone drops the blind simp arm (it would heartbeat-abort), so the
/// generic fallback used to be the bare `first | sorry`. Now that fallback
/// composes the earlier laws about the cone fns exactly as the keystone
/// does: `simp only [<non-recursive cone>] <;> grind [<pool>]`, still under
/// the honest `sorry` floor.
///
/// Export-structure pin (no toolchain needed).
#[test]
fn proof_export_keystone_floor_arm_structure() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output_dir = temp_output_dir("aver-proof-keystone-floor-export");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("tests/fixtures/pool_composition_recursive.av")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("aver proof should run");
    assert!(run.status.success(), "{}", format_output(&run));
    let lean = std::fs::read_to_string(output_dir.join("PoolCompositionRecursive.lean"))
        .expect("PoolCompositionRecursive.lean must be emitted");
    let start = lean
        .find("theorem magnitudeOf_law_readsBackDigits :")
        .unwrap_or_else(|| panic!("the top law theorem must be emitted:\n{lean}"));
    let body = &lean[start..];
    let end = body.find("_checked_domain").unwrap_or(body.len());
    let body = &body[..end];
    assert!(
        body.contains(POOL_LAWS),
        "the floor must compose every earlier law about the cone fns:\n{body}"
    );
    assert!(
        body.contains("(try simp only [PoolCompositionRecursive.magnitudeOf, "),
        "the floor must unfold the non-recursive cone and keep the countdown \
         fn folded:\n{body}"
    );
    assert!(
        body.contains(") | sorry"),
        "the keystone arm stays under the honest sorry floor:\n{body}"
    );
    assert!(
        !body.contains("have key : ∀ (k : Nat)"),
        "a composite countdown arg is out of scope for fuel induction:\n{body}"
    );
    assert!(
        !body.contains("simp [PoolCompositionRecursive.digits")
            && !body.contains("simp_all [PoolCompositionRecursive.digits")
            && !body.contains("fun_induction digits")
            && !body.contains("fun_induction PoolCompositionRecursive.digits"),
        "no arm may unfold the countdown fn blindly:\n{body}"
    );
    for base in [
        "bigEndian_law_readsTheLastByteLast",
        "digits_law_accumulatorComesFirst",
        "digits_law_readsBackBigEndian",
        "magnitudeOf_law_readsBackDigits",
    ] {
        assert!(
            lean.contains(&format!("-- aver:law-class {base} universal")),
            "{base} must be classed universal:\n{lean}"
        );
    }
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Live Lean gate: all four law theorems close sorry-free — the three
/// rungs by fuel induction / structural induction, the top law by the
/// keystone floor arm composing them through `grind`.
#[test]
fn proof_keystone_floor_arm_lean_closes_kernel_genuine() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping keystone-floor proof test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-keystone-floor");
    let (summary, run) = run_lean_check_json(
        "tests/fixtures/pool_composition_recursive.av",
        &output_dir,
        0,
        &[],
    );
    assert_eq!(
        summary["build_errors"].as_u64(),
        Some(0),
        "{}",
        format_output(&run)
    );
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(0),
        "the composed round trip must close sorry-free.\n{}",
        format_output(&run)
    );
    assert_eq!(
        (
            summary["universal"].as_bool(),
            summary["universal_laws"].as_u64(),
            summary["bounded_laws"].as_u64(),
        ),
        (Some(true), Some(4), Some(0)),
        "all four law theorems must be certified universal.\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// A wrapper pins the prelude-simp strategy even though its recursive callee
/// needs earlier laws. Composition must remain available after that attempt;
/// the same pool must not certify a false unguarded statement from its samples.
#[test]
fn proof_prelude_wrapper_composes_without_admitting_a_false_neighbor() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping wrapper composition test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-pool-wrapper");
    let (summary, run) = run_lean_check_json(
        "tests/fixtures/pool_composition_wrapper.av",
        &output_dir,
        1,
        &[],
    );
    assert_eq!(
        (
            summary["build_errors"].as_u64(),
            summary["universal_laws"].as_u64(),
            summary["bounded_laws"].as_u64(),
            summary["sorries"].as_u64(),
        ),
        (Some(0), Some(4), Some(0), Some(1)),
        "the wrapper must compose, while the false law stays open:\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["sorry_laws"],
        serde_json::json!(["normalized.unguardedRequestMustStayOpen"])
    );
    let lean = std::fs::read_to_string(output_dir.join("PoolCompositionWrapper.lean")).unwrap();
    let body = lean
        .split("theorem normalized_law_preservesNonnegativeRequest :")
        .nth(1)
        .unwrap()
        .split("_checked_domain")
        .next()
        .unwrap();
    assert!(
        body.find("simp [").unwrap() < body.find("grind [").unwrap(),
        "composition must follow the original prelude attempt:\n{body}"
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// One-step observations compose earlier laws about the same subject; the
/// leading digit also needs that composition inside its fuel induction.
#[test]
fn proof_equation_composition_closes_observations_and_keeps_false_law_open() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let output_dir = temp_output_dir("aver-proof-equation-composition");
    let (summary, run) = run_lean_check_json(
        "tests/fixtures/equation_composition.av",
        &output_dir,
        1,
        &[],
    );
    assert_eq!(
        (
            summary["build_errors"].as_u64(),
            summary["universal_laws"].as_u64(),
            summary["bounded_laws"].as_u64(),
            summary["sorries"].as_u64(),
        ),
        (Some(0), Some(3), Some(0), Some(1)),
        "true observations must close and the false neighbor must remain open:\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["sorry_laws"],
        serde_json::json!(["digits.falseDropMustStayOpen"])
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

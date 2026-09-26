use super::*;

const FIXTURE: &str = "tests/fixtures/int_literal_nat_leak.av";

/// Issue #1451. A Lean numeral with no expected type is a `Nat`, where
/// `0 - 1 = 0`. The export used to write Aver `Int` literals bare, so a
/// sampled `0 - 1` in `(0 - 1) >= 0` was read as `0 >= 0`, true, while the
/// VM reads `-1 >= 0`, false. Every such literal is now an `Int`.
#[test]
fn proof_export_keeps_int_literals_int() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let output_dir = temp_output_dir("aver-proof-int-literals-export");
    let run = Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(&repo_root)
        .arg("proof")
        .arg(FIXTURE)
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("aver proof should run");
    assert!(run.status.success(), "{}", format_output(&run));
    let lean = std::fs::read_to_string(output_dir.join("IntLiteralNatLeak.lean"))
        .expect("IntLiteralNatLeak.lean must be emitted");
    for needle in [
        // A definition body with no variable to fix the type.
        "def minusOneIsNonNegative  : Bool :=\n  (((0 : Int) - 1) >= 0)",
        // A literal in the claim itself.
        "∀ (x : Int), (positive x || (((0 : Int) - 1) >= 0)) = true",
        // The reported shape: the sampled index on both sides of a law.
        "isSomeInput (inputAt ([] : List Input) ((0 : Int) - 1))",
        "{ present := ((((0 : Int) - 1) >= 0) && (((0 : Int) - 1) < ",
    ] {
        assert!(lean.contains(needle), "missing `{needle}`:\n{lean}");
    }
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Both `positive` laws hold on every sample and are false at `x = 0`. Read
/// over `Nat` they are true (`0 - 1 >= 0`), and the export used to prove them
/// universally without `native_decide`. `vectorSetBelowLength` is false at
/// `-1`, where the export's `Vector.set` used to write element 0. None of the
/// three may count as universal, while every sampled theorem of the reported
/// shape, `0 - 1` included, builds and agrees with the VM.
#[test]
fn proof_int_literal_soundness_laws_are_not_proved() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping Int literal soundness test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-int-literals-lake");
    let (summary, run) = run_lean_check_json(FIXTURE, &output_dir, 16, &[]);
    let names = |key: &str| -> Vec<String> {
        summary[key]
            .as_array()
            .map(|items| {
                items
                    .iter()
                    .filter_map(|item| item.as_str().map(str::to_string))
                    .collect()
            })
            .unwrap_or_default()
    };
    let sorry_laws = names("sorry_laws");
    let false_laws = [
        "positive.literalInClaim",
        "positive.literalInDefinition",
        "setsAt.vectorSetBelowLength",
    ];
    for law in false_laws {
        assert!(
            sorry_laws.iter().any(|name| name == law),
            "`{law}` is false outside its samples and must not be proved:\n{}",
            format_output(&run)
        );
    }
    // A failed universal proof of a false law is the only build error
    // allowed: no sample theorem, `0 - 1` included, may fail.
    let isolated = names("isolated_errors");
    assert!(
        isolated
            .iter()
            .all(|name| false_laws.contains(&name.as_str())),
        "only the false laws may fail to elaborate:\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["build_errors"].as_u64(),
        Some(isolated.len() as u64),
        "every sampled theorem must build and agree with the VM:\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

use super::*;

/// A Boolean equality nested under an Int-valued match reaches Lean as `BEq`.
/// The original simp+omega rung remains first, while a later bridge exposes
/// equality and disequality as propositions before `omega` runs. The final
/// `sorry` alternative is the fail-closed floor for shapes that still do not
/// close.
#[test]
fn proof_export_bool_eq_omega_has_bridge_and_sorry_floor() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output_dir = temp_output_dir("aver-proof-bool-eq-omega-export");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("tests/fixtures/bool_eq_omega.av")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("aver proof should run");
    assert!(run.status.success(), "{}", format_output(&run));

    let lean = std::fs::read_to_string(output_dir.join("BoolEqOmega.lean"))
        .expect("BoolEqOmega.lean must be emitted");
    assert!(
        lean.contains("-- aver:law-class unaryValue_law_notNotIsZeroNotEqual universal"),
        "the true Bool equality law must retain its universal marker:\n{lean}"
    );
    assert!(
        lean.contains(
            "first | (simp only [unaryValue, oneIf] <;> omega) | \
             (simp only [unaryValue, oneIf, Bool.beq_comm, beq_iff_eq, \
             bne_iff_ne, Bool.or_eq_true, Bool.and_eq_true, decide_eq_decide, \
             decide_eq_true_eq, ← decide_not, Bool.not_eq_true', ge_iff_le, \
             gt_iff_lt] <;> (try split) <;> simp_all <;> omega) | sorry"
        ),
        "the old rung, Bool bridge, and fail-closed sorry floor must stay ordered:\n{lean}"
    );

    let _ = std::fs::remove_dir_all(&output_dir);
}

/// The live Lean gate proves that the portfolio closes before its `sorry`
/// floor and earns kernel-genuine universal credit.
#[test]
fn proof_bool_eq_omega_closes_kernel_genuine() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping Bool equality omega proof test: `lake` not available");
        return;
    }

    let output_dir = temp_output_dir("aver-proof-bool-eq-omega-lake");
    let (summary, run) =
        run_lean_check_json("tests/fixtures/bool_eq_omega.av", &output_dir, 0, &[]);
    assert_eq!(
        summary["build_errors"].as_u64(),
        Some(0),
        "{}",
        format_output(&run)
    );
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(0),
        "{}",
        format_output(&run)
    );
    assert_eq!(
        summary["universal"].as_bool(),
        Some(true),
        "{}",
        format_output(&run)
    );
    assert_eq!(
        summary["universal_laws"].as_u64(),
        Some(1),
        "{}",
        format_output(&run)
    );
    assert_eq!(
        summary["passed"].as_bool(),
        Some(true),
        "{}",
        format_output(&run)
    );

    let _ = std::fs::remove_dir_all(&output_dir);
}

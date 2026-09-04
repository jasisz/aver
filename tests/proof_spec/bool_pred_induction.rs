use super::*;

/// The Bool-to-Prop bridge lemmas plus the Bool `&&`/`||` normal-form lemmas,
/// exactly as the induction ladder's closers spell them.
const BOOL_BRIDGE: &str = "Bool.beq_comm, beq_iff_eq, bne_iff_ne, Bool.or_eq_true, \
     Bool.and_eq_true, decide_eq_decide, decide_eq_true_eq, ← decide_not, \
     Bool.not_eq_true', ge_iff_le, gt_iff_lt, Bool.and_assoc, Bool.and_comm, \
     Bool.and_left_comm, Bool.or_assoc, Bool.or_comm, Bool.or_left_comm";

/// A structural law over a Bool predicate leaves each induction arm an equality
/// between two open Bool terms, which no arithmetic rung can touch. Both the
/// `fun_induction` closer and the list-induction arms must carry the bridging
/// alternatives, and they must come AFTER the existing rungs (so a law that
/// closed before still closes on the very same tactic) and BEFORE the
/// fail-closed `sorry` floor.
#[test]
fn proof_export_bool_pred_induction_closers_carry_the_bool_bridge() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output_dir = temp_output_dir("aver-proof-bool-pred-induction-export");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("tests/fixtures/bool_pred_induction.av")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("aver proof should run");
    assert!(run.status.success(), "{}", format_output(&run));

    let lean = std::fs::read_to_string(output_dir.join("BoolPredInduction.lean"))
        .expect("BoolPredInduction.lean must be emitted");

    let defs = "BoolPredInduction.allBytes";
    let bridge_rungs = format!(
        " | (simp_all [{defs}, {BOOL_BRIDGE}]; done) | \
         (simp_all [{defs}, {BOOL_BRIDGE}] <;> (try split) <;> omega)"
    );

    // The `fun_induction` rung: the bridge follows the `repeat' split` rung that
    // used to be last.
    assert!(
        lean.contains(&format!(
            "(simp_all [{defs}] <;> (repeat' split) <;> omega){bridge_rungs})"
        )),
        "the fun_induction closer must end in the Bool bridge:\n{lean}"
    );

    // The list-induction arms: the bridge sits between the `congr 1` rung and
    // the `sorry` floor.
    assert!(
        lean.contains(&format!(
            "(simp_all [{defs}]; congr 1 <;> simp_all [{defs}] <;> omega){bridge_rungs} | sorry"
        )),
        "the cons arm must try the Bool bridge before its sorry floor:\n{lean}"
    );
    assert!(
        lean.contains(&format!(
            "(simp [{defs}]; congr 1 <;> simp_all [{defs}] <;> omega){bridge_rungs} | sorry"
        )),
        "the nil arm must try the Bool bridge before its sorry floor:\n{lean}"
    );

    // The consumer law's arms carry the cited sibling in the same bridged set.
    assert!(
        lean.contains(&format!(
            "simp_all [{defs}, allBytes_law_appendedByteLast, {BOOL_BRIDGE}]"
        )),
        "the citing law's arms must bridge over the cited sibling too:\n{lean}"
    );

    for law in [
        "allBytes_law_appendedByteLast",
        "allBytes_law_reverseKeepsBytes",
    ] {
        assert!(
            lean.contains(&format!("-- aver:law-class {law} universal")),
            "{law} must be exported as a universal claim:\n{lean}"
        );
    }

    let _ = std::fs::remove_dir_all(&output_dir);
}

/// The live Lean gate: both Bool-predicate laws close inside the ladder, so
/// neither reaches its `sorry` floor and both earn kernel-genuine universal
/// credit.
#[test]
fn proof_bool_pred_induction_closes_kernel_genuine() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping Bool predicate induction proof test: `lake` not available");
        return;
    }

    let output_dir = temp_output_dir("aver-proof-bool-pred-induction-lake");
    let (summary, run) =
        run_lean_check_json("tests/fixtures/bool_pred_induction.av", &output_dir, 0, &[]);
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
        Some(2),
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

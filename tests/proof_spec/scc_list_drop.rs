use super::*;

#[test]
fn proof_export_scc_list_drop_and_take_are_native_mutual_groups() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output_dir = temp_output_dir("aver-proof-scc-list-drop-export");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("tests/fixtures/scc_list_drop.av")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("aver proof should run");
    assert!(run.status.success(), "{}", format_output(&run));

    let lean = std::fs::read_to_string(output_dir.join("SccListDrop.lean"))
        .expect("SccListDrop.lean must be emitted");
    assert_eq!(
        lean.matches("\nmutual\n").count(),
        2,
        "drop and take must each emit one genuine mutual block:\n{lean}"
    );
    assert!(
        !lean.contains("__fuel") && !lean.contains("partial def") && !lean.contains("declined"),
        "the recognised groups must not retain a fuel, partial, or declined fallback:\n{lean}"
    );
    for measure in [
        "termination_by (bytes.length, 1)",
        "termination_by (rest.length, 2)",
        "termination_by (rest.length, 3)",
    ] {
        assert_eq!(
            lean.matches(measure).count(),
            2,
            "both builtin variants must use the call-graph-selected length/rank measure {measure}:\n{lean}"
        );
    }
    assert!(
        lean.contains("simp only [List.length_drop]; omega")
            && lean.contains("simp only [List.length_take]; omega"),
        "computed list edges must cite their core non-growing length theorems:\n{lean}"
    );

    let _ = std::fs::remove_dir_all(&output_dir);
}

#[test]
fn proof_scc_list_drop_and_take_build_without_declines() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping SCC List.drop/List.take proof test: `lake` not available");
        return;
    }

    let output_dir = temp_output_dir("aver-proof-scc-list-drop-lake");
    let (summary, run) =
        run_lean_check_json("tests/fixtures/scc_list_drop.av", &output_dir, 0, &[]);
    assert_eq!(
        summary["build_errors"].as_u64(),
        Some(0),
        "{}",
        format_output(&run)
    );
    assert_eq!(
        summary["declined"].as_u64().unwrap_or(0),
        0,
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
        summary["passed"].as_bool(),
        Some(true),
        "{}",
        format_output(&run)
    );

    let _ = std::fs::remove_dir_all(&output_dir);
}

//! #1389: a capability taking a list of tuples must keep its oracle type
//! when a verify case reaches an effectful function through its pure arm.

use super::*;

const FILES: &[(&str, &str)] = &[
    (
        "infra/kv.av",
        include_str!("../fixtures/proof_capability_tuple/infra/kv.av"),
    ),
    (
        "infra/store.av",
        include_str!("../fixtures/proof_capability_tuple/infra/store.av"),
    ),
    (
        "main.av",
        include_str!("../fixtures/proof_capability_tuple/main.av"),
    ),
];

#[test]
fn capability_tuple_verify_exports_from_entry_and_dependency() {
    let root =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/proof_capability_tuple");
    for (entry, lean_file) in [
        ("infra/store.av", "Store.lean"),
        ("main.av", "Infra/Store.lean"),
    ] {
        let out = temp_output_dir("aver-capability-tuple-export");
        let run = Command::new(env!("CARGO_BIN_EXE_aver"))
            .arg("proof")
            .arg(root.join(entry))
            .arg("--module-root")
            .arg(&root)
            .arg("-o")
            .arg(&out)
            .output()
            .expect("run proof export");
        assert!(run.status.success(), "{entry}: {}", format_output(&run));
        let lean = std::fs::read_to_string(out.join(lean_file)).expect("read store export");
        assert!(
            lean.contains("List (Bytes.Bytes × Bytes.Bytes)"),
            "the oracle must retain the batch tuple type:\n{lean}"
        );
        assert!(
            lean.contains("def applied"),
            "the verified function must remain exported"
        );
        assert!(
            lean.lines().any(
                |line| line.starts_with("example ") && line.contains("applied BranchPath.Root")
            ),
            "the pure-branch verify case must remain a proof obligation:\n{lean}"
        );
        let _ = std::fs::remove_dir_all(out);
    }
}

#[test]
fn capability_tuple_verify_builds_from_entry_and_dependency() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping capability tuple proof test: `lake` not available");
        return;
    }
    for entry in ["infra/store.av", "main.av"] {
        let (summary, run, _) = super::cross_file::run_multi(FILES, entry, &[]);
        assert!(run.status.success(), "{entry}: {}", format_output(&run));
        assert_eq!(summary["build_errors"].as_u64(), Some(0), "{summary}");
        assert_eq!(summary["sorries"].as_u64(), Some(0), "{summary}");
        assert_eq!(summary["passed"].as_bool(), Some(true), "{summary}");
    }
}

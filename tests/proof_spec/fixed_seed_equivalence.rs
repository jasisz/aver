//! A fixed seed is shared by both recursive workers, not fixed throughout
//! their execution. Functional induction must see through the spec wrapper.

use super::*;

#[test]
fn recursive_workers_agree_at_a_fixed_seed() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping fixed-seed proof test: `lake` not available");
        return;
    }
    let output_dir = tempfile::tempdir().unwrap();
    let (summary, run) = run_lean_check_json(
        "tests/fixtures/lean_fixed_seed_equivalence.av",
        output_dir.path(),
        0,
        &[],
    );
    assert_eq!(
        summary["universal_laws"],
        1,
        "{summary}\n{}",
        format_output(&run)
    );
    assert_eq!(summary["sorries"], 0, "{summary}");
    assert_eq!(summary["passed"], true, "{summary}");
}

#[test]
fn fixed_seed_equivalence_reaches_a_private_imported_worker() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping imported fixed-seed proof test: `lake` not available");
        return;
    }
    let source_dir = tempfile::tempdir().unwrap();
    std::fs::create_dir(source_dir.path().join("infra")).unwrap();
    std::fs::write(
        source_dir.path().join("infra/reader.av"),
        r#"module Reader
    exposes [numberIn]

fn numberFrom(octets: List<Int>, value: Int) -> Int
    match octets
        [] -> value
        [octet, ..rest] -> numberFrom(rest, value * 256 + octet)

fn numberIn(bytes: List<Int>) -> Int
    numberFrom(bytes, 0)
"#,
    )
    .unwrap();
    let file = source_dir.path().join("main.av");
    std::fs::write(
        &file,
        r#"module Main
    depends [Infra.Reader]

fn decoded(bytes: List<Int>, acc: Int) -> Int
    match bytes
        [] -> acc
        [head, ..tail] -> decoded(tail, acc * 256 + head)

verify decoded law agreesWithNumberIn
    given bytes: List<Int> = [[], [1], [1, 2]]
    decoded(bytes, 0) => Infra.Reader.numberIn(bytes)
"#,
    )
    .unwrap();
    let output_dir = tempfile::tempdir().unwrap();
    let run = Command::new(env!("CARGO_BIN_EXE_aver"))
        .arg("proof")
        .arg(&file)
        .arg("--module-root")
        .arg(source_dir.path())
        .arg("-o")
        .arg(output_dir.path())
        .args(["--check-json", "--sorry-budget", "0"])
        .output()
        .unwrap();
    assert!(run.status.success(), "{}", format_output(&run));
    let summary: serde_json::Value = serde_json::from_str(
        String::from_utf8_lossy(&run.stdout)
            .lines()
            .find(|line| line.starts_with('{'))
            .unwrap(),
    )
    .unwrap();
    assert_eq!(summary["universal_laws"], 1, "{summary}");
    assert_eq!(summary["sorries"], 0, "{summary}");
}

#[test]
fn different_worker_updates_or_seeds_do_not_earn_proof_credit() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping fixed-seed negative controls: `lake` not available");
        return;
    }
    let original = std::fs::read_to_string(
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("tests/fixtures/lean_fixed_seed_equivalence.av"),
    )
    .unwrap();
    for source in [
        original.replace("value * 256 + octet", "value * 257 + octet"),
        original.replace("numberFrom(bytes, 0)", "numberFrom(bytes, 1)"),
    ] {
        let source_dir = tempfile::tempdir().unwrap();
        let file = source_dir.path().join("main.av");
        std::fs::write(&file, source).unwrap();
        let output_dir = tempfile::tempdir().unwrap();
        let (summary, run) =
            run_lean_check_json(file.to_str().unwrap(), output_dir.path(), 1000, &[]);
        assert_eq!(
            summary["passed"],
            false,
            "{summary}\n{}",
            format_output(&run)
        );
        assert_eq!(summary["universal_laws"], 0, "{summary}");
        assert_eq!(summary["model_panicked"], false, "{summary}");
    }
}

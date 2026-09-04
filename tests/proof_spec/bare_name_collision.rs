use super::*;

/// Bare-name resolution under a dependency scope
/// (`tests/fixtures/bare_name_collision/`): two dependencies each define
/// `littleEndian` and `bigEndian` with different shapes — a fixed-width wire
/// encoder and a script-number writer — and the entry depends on the wire
/// module FIRST. The writer's laws used to resolve their bare call names by
/// the program-wide first match, so every strategy saw the wire encoder's
/// cone (`littleEndianInto` in the writer's proofs), the countdown was no
/// longer a countdown, and all three laws lost their tier. A bare name now
/// denotes the fn of the module being emitted; the program-wide search is
/// the last resort. Export pin: no wire fn reaches the writer's file.
#[test]
fn proof_export_resolves_bare_names_in_the_emitted_module_first() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output_dir = temp_output_dir("aver-proof-bare-name-collision-export");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("tests/fixtures/bare_name_collision/main.av")
        .arg("--module-root")
        .arg("tests/fixtures/bare_name_collision")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("aver proof should run");
    assert!(run.status.success(), "{}", format_output(&run));
    let writer = std::fs::read_to_string(output_dir.join("Writer.lean"))
        .expect("Writer.lean must be emitted");
    assert!(
        !writer.contains("littleEndianInto"),
        "the writer's proofs must never unfold the wire encoder's helper:\n{writer}"
    );
    let round_trip = writer
        .find("theorem littleEndian_law_readsBackBigEndian :")
        .expect("round-trip theorem");
    let body = &writer[round_trip..];
    let end = body.find("_checked_domain").unwrap_or(body.len());
    let body = &body[..end];
    assert!(
        body.contains("have key : ∀ (k : Nat)"),
        "the writer's countdown must be recognized as one (fuel induction):\n{body}"
    );
    assert!(
        body.contains("bigEndian_law_readsTheLastByteLast"),
        "the round trip must cite the writer's own snoc law:\n{body}"
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Live Lean gate: the writer's three laws certify with the clashing wire
/// module ahead of it in the dependency list.
#[test]
fn proof_bare_name_collision_lean_closes_kernel_genuine() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping bare-name collision proof test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output_dir = temp_output_dir("aver-proof-bare-name-collision");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("tests/fixtures/bare_name_collision/main.av")
        .arg("--module-root")
        .arg("tests/fixtures/bare_name_collision")
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&output_dir)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("aver proof --check --check-json should run");
    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with('{')))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)))
        .to_string();
    let summary: serde_json::Value =
        serde_json::from_str(&json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}"));
    assert_eq!(
        (
            summary["build_errors"].as_u64(),
            summary["sorries"].as_u64(),
            summary["universal_laws"].as_u64(),
            summary["bounded_laws"].as_u64(),
        ),
        (Some(0), Some(0), Some(3), Some(0)),
        "the writer's laws must certify against the writer's own fns.\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

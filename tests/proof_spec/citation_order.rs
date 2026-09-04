use super::*;

/// Citation order of law theorems (`tests/fixtures/citation_order.av`): the
/// countdown round trip `readsBackBigEndian` on `digits` cites the snoc law
/// on `bigEndian`, and `digits` is defined FIRST, so in source order (which
/// `aver format` enforces block by block) the round trip sits above the law
/// it needs. A law can only cite theorems declared above it, so the export
/// used to lose the lemma and the round trip fell to `sorry` — the same
/// fixture with the two fns swapped closed fine. The export now emits a
/// block about a function before every law whose cone reaches it, so the
/// order the author wrote the fns in no longer decides what a law can cite.
#[test]
fn proof_export_orders_law_theorems_by_citation() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output_dir = temp_output_dir("aver-proof-citation-order-export");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("tests/fixtures/citation_order.av")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("aver proof should run");
    assert!(run.status.success(), "{}", format_output(&run));
    let lean = std::fs::read_to_string(output_dir.join("CitationOrder.lean"))
        .expect("CitationOrder.lean must be emitted");
    let snoc = lean
        .find("theorem bigEndian_law_readsTheLastByteLast :")
        .expect("snoc theorem");
    let round_trip = lean
        .find("theorem digits_law_readsBackBigEndian :")
        .expect("round-trip theorem");
    assert!(
        snoc < round_trip,
        "the cited snoc law must be declared above the round trip that cites it:\n{lean}"
    );
    let body = &lean[round_trip..];
    let end = body.find("_checked_domain").unwrap_or(body.len());
    let body = &body[..end];
    assert!(
        body.contains("bigEndian_law_readsTheLastByteLast"),
        "the round trip must cite the snoc law as a lemma:\n{body}"
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Live Lean gate: all three laws certify, the round trip included.
#[test]
fn proof_citation_order_lean_closes_kernel_genuine() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping citation-order proof test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-citation-order");
    let (summary, run) =
        run_lean_check_json("tests/fixtures/citation_order.av", &output_dir, 0, &[]);
    assert_eq!(
        (
            summary["build_errors"].as_u64(),
            summary["sorries"].as_u64(),
            summary["universal_laws"].as_u64(),
            summary["bounded_laws"].as_u64(),
        ),
        (Some(0), Some(0), Some(3), Some(0)),
        "the round trip must close from the snoc law written after it.\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

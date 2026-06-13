use super::*;

/// Phase 2/2d acceptance (lemma discovery): `aver proof <rle> --discover`
/// enumerates candidate equations, VM-filters them, and kernel-proves the
/// `decode_append` survivor via `lake build` — end to end, with no
/// RLE-specific recognizer. Skips when `lake` is unavailable.
#[test]
fn discover_kernel_proves_decode_append_when_lake_is_available() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping discovery proof test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    // Fresh `-o` so the run always discovers (no committed-lemma replay).
    let output_dir = temp_output_dir("aver-discover-rle");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("examples/data/rle.av")
        .arg("--discover")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("expected `aver proof --discover` to run");
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert!(
        stdout.contains("PROVED (Lean, kernel-checked)"),
        "no kernel-proved lemma in `--discover` output:\n{}",
        format_output(&run)
    );
    assert!(
        stdout.contains("decode(List.concat(x2, x3)) == List.concat(decode(x2), decode(x3))"),
        "decode_append was not the kernel-proved lemma:\n{}",
        format_output(&run)
    );
    // The proved lemma is persisted as a reviewable committed artifact.
    assert!(
        output_dir.join("DiscoveredLemmas.lean").exists(),
        "DiscoveredLemmas.lean was not written:\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Run `aver proof <path> --discover` and assert the committed
/// `DiscoveredLemmas.lean` contains `lemma_needle` (a kernel-proved lemma).
/// Skips when `lake` is unavailable.
fn assert_discover_proves(example_path: &str, prefix: &str, lemma_needle: &str) {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping discovery proof test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let output_dir = temp_output_dir(prefix);
    let run = Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(&repo_root)
        .arg("proof")
        .arg(example_path)
        .arg("--discover")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("expected `aver proof --discover` to run");
    let committed =
        std::fs::read_to_string(output_dir.join("DiscoveredLemmas.lean")).unwrap_or_default();
    assert!(
        committed.contains(lemma_needle),
        "expected `{lemma_needle}` among kernel-proved lemmas.\n--- stdout ---\n{}\n--- DiscoveredLemmas.lean ---\n{committed}",
        format_output(&run),
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Generalization guard: discovery proves the `flatten` list-homomorphism on a
/// NON-encoder program (no RLE shape anywhere) — evidence the enumeration path
/// is genuinely general, not fitted to `rle.av`.
#[test]
fn discover_proves_flatten_homomorphism_when_lake_is_available() {
    assert_discover_proves(
        "examples/data/flatten.av",
        "aver-discover-flatten",
        "(flatten (x0 ++ x1)) = ((flatten x0) ++ (flatten x1))",
    );
}

/// Generalization guard: the structural counted-repeat conjecturer fires on a
/// differently-named fn (`stars`, not `repeat`) in a non-encoder program —
/// evidence brick 1 keys on shape, not the RLE name.
#[test]
fn discover_proves_stars_repeat_succ_when_lake_is_available() {
    assert_discover_proves(
        "examples/data/stars.av",
        "aver-discover-stars",
        "stars c (n + 1) = stars c n ++ [c]",
    );
}

/// Generalization guard for the (generalized) brick 2: discovery proves the
/// monotone-nonneg accumulator invariant on `tally.av`, whose fold branches on
/// `x > acc.last` (NOT the RLE `count == 0` shape) — evidence the count-
/// invariant conjecturer keys on the field arithmetic, not the RLE step.
#[test]
fn discover_proves_tally_count_invariant_when_lake_is_available() {
    assert_discover_proves(
        "examples/data/tally.av",
        "aver-discover-tally",
        "0 <= (tallyStep acc x).seen",
    );
}

/// Generalization guard on a SHAPE-different second encoder-with-inverse
/// (`sparse.av`: sum-type tokens, branches on `x == 0`). One `--discover` run
/// must kernel-prove BOTH the UNARY counted-repeat advance `repeat0(n+1) =
/// repeat0(n) ++ [0]` (brick 1's arity generalization) AND the monotone-nonneg
/// `pending` invariant — proof that the structural conjecturers generalize
/// across encoders, not just rle.
#[test]
fn discover_generalizes_on_sparse_codec_when_lake_is_available() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping discovery proof test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let output_dir = temp_output_dir("aver-discover-sparse");
    let run = Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(&repo_root)
        .arg("proof")
        .arg("examples/data/sparse.av")
        .arg("--discover")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("expected `aver proof --discover` to run");
    let committed =
        std::fs::read_to_string(output_dir.join("DiscoveredLemmas.lean")).unwrap_or_default();
    for needle in [
        "repeat0 (n + 1) = repeat0 n ++ [0]",
        "0 <= (sparseStep acc x).pending",
    ] {
        assert!(
            committed.contains(needle),
            "expected `{needle}` among kernel-proved lemmas.\n--- stdout ---\n{}\n--- DiscoveredLemmas.lean ---\n{committed}",
            format_output(&run),
        );
    }
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Relational-brick acceptance (the locksmith's last layer): one `--discover`
/// run kernel-proves the FULL roundtrip law `decode (encode xs) = xs` on
/// `rle.av` — the auto-emitted chain (inv_append → counted_one → counted_succ →
/// count_nonneg → flush_fold_step → loop_gen → roundtrip) replaces the retired
/// hardcoded `AccumulatorRoundtrip` recognizer.
#[test]
fn discover_proves_roundtrip_on_rle_when_lake_is_available() {
    assert_discover_proves(
        "examples/data/rle.av",
        "aver-discover-rle-roundtrip",
        "decode (encode xs) = xs",
    );
}

/// DISCIPLINE GUARD (the whole point): the SAME relational emitter must fire +
/// kernel-prove the roundtrip on a SHAPE-different second encoder (`sparse.av`:
/// sum-type tokens, `pending` field, 2-way step guard) — `decodeSparse
/// (encodeSparse xs) = xs`. If this passes only on rle, the chain is the key,
/// not the locksmith; it must prove on BOTH or neither.
#[test]
fn discover_proves_roundtrip_on_sparse_when_lake_is_available() {
    assert_discover_proves(
        "examples/data/sparse.av",
        "aver-discover-sparse-roundtrip",
        "decodeSparse (encodeSparse xs) = xs",
    );
}

/// MONOIDAL flavor of the unified accumulator-generalization schema: the same
/// `--discover` path that proves codec roundtrips also kernel-proves the
/// spec-equivalence `sum xs = sumDirect xs` (sum = sumTR(·, 0), an additive
/// fold) — codec roundtrip and monoidal fold are two flavors of ONE schema.
#[test]
fn discover_proves_monoidal_spec_equivalence_on_sum_acc_when_lake_is_available() {
    assert_discover_proves(
        "examples/data/sum_acc.av",
        "aver-discover-sum-monoidal",
        "sum xs = sumDirect xs",
    );
}

/// Read the committed `DiscoveredLemmas.lean` produced by `--discover` on
/// `example_path` (empty string if none was written). Skips (returns `None`)
/// when `lake` is unavailable.
fn discover_committed(example_path: &str, prefix: &str) -> Option<(String, PathBuf)> {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping discovery proof test: `lake` not available");
        return None;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let output_dir = temp_output_dir(prefix);
    let _ = Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(&repo_root)
        .arg("proof")
        .arg(example_path)
        .arg("--discover")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("expected `aver proof --discover` to run");
    let committed =
        std::fs::read_to_string(output_dir.join("DiscoveredLemmas.lean")).unwrap_or_default();
    Some((committed, output_dir))
}

/// Generalization guard: counted-append with the count parameter FIRST
/// (`pad(n, c)`), the opposite of rle's `repeat(c, n)` — the detector finds the
/// count by role, not position.
#[test]
fn discover_proves_spaces_count_first_when_lake_is_available() {
    assert_discover_proves(
        "examples/data/spaces.av",
        "aver-discover-spaces",
        "pad (n + 1) c = pad n c ++ [c]",
    );
}

/// Generalization guard: monotone-nonneg field with a `+ 2` update (not `+ 1`)
/// — the invariant conjecturer keys on `field + nonneg-literal`, any literal.
#[test]
fn discover_proves_gauge_plus_two_invariant_when_lake_is_available() {
    assert_discover_proves(
        "examples/data/gauge.av",
        "aver-discover-gauge",
        "0 <= (bump acc x).level",
    );
}

/// Completeness guard: a MULTIPLICATIVE nonneg update (`level * 2`, not a `+ k`
/// shift) is still recognized as monotone-nonneg — `0 <= level` is closed under
/// `* 2` and stays linear in the field, so omega proves it.
#[test]
fn discover_proves_scale_multiplicative_nonneg_when_lake_is_available() {
    assert_discover_proves(
        "examples/data/scale.av",
        "aver-discover-scale",
        "0 <= (grow acc x).level",
    );
}

/// Completeness guard: a record with TWO Int fields of different invariant
/// classes — `seen` (non-negative) and `budget` (strictly decreasing) — yields
/// a kernel-proved lemma for EACH, not just the first the conjecturer finds.
#[test]
fn discover_proves_both_invariants_on_two_int_fields_when_lake_is_available() {
    let Some((committed, output_dir)) =
        discover_committed("examples/data/twofield.av", "aver-discover-twofield")
    else {
        return;
    };
    for needle in [
        "0 <= (meterStep acc x).seen",
        "acc.budget - 1 <= (meterStep acc x).budget",
        "(meterStep acc x).budget <= acc.budget - 1",
    ] {
        assert!(
            committed.contains(needle),
            "expected `{needle}` among kernel-proved lemmas.\n--- DiscoveredLemmas.lean ---\n{committed}",
        );
    }
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Generalization guard: the list-homomorphism discovery works over a RECORD
/// element type (`List<Token>`), not just String. The homomorphism theorem
/// names `expandAll` three times (lhs once, rhs twice).
#[test]
fn discover_proves_words_homomorphism_when_lake_is_available() {
    let Some((committed, output_dir)) =
        discover_committed("examples/data/words.av", "aver-discover-words")
    else {
        return;
    };
    assert!(
        committed.matches("expandAll").count() >= 3,
        "expected an `expandAll(a ++ b) = expandAll a ++ expandAll b` homomorphism.\n--- DiscoveredLemmas.lean ---\n{committed}",
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// SOUNDNESS + GENERALIZATION guard: on `drain.av` the accumulator field can
/// DECREASE (`Counter(n = acc.n - 1)`), so `0 <= (tick acc x).n` is FALSE. The
/// engine must NEVER kernel-prove that (proved-or-dropped). But the field DOES
/// move by a bounded delta each step (`+1`/`-1`), so the generalized bounded-
/// step conjecturer must discover and prove the TRUE two-sided bound — proof the
/// engine generalizes past monotone-nonneg without becoming unsound: it picks
/// the right invariant for a decreasing accumulator, not the false one.
#[test]
fn discover_bounds_decreasing_accumulator_on_drain_when_lake_is_available() {
    let Some((committed, output_dir)) =
        discover_committed("examples/data/drain.av", "aver-discover-drain")
    else {
        return;
    };
    // Soundness: the false nonneg invariant is never proved.
    assert!(
        !committed.contains("0 <= (tick acc x).n"),
        "UNSOUND: the false count-invariant `0 <= (tick acc x).n` was kernel-proved.\n--- DiscoveredLemmas.lean ---\n{committed}",
    );
    // Generalization: the true bounded step IS proved (both sides).
    for needle in ["acc.n - 1 <= (tick acc x).n", "(tick acc x).n <= acc.n + 1"] {
        assert!(
            committed.contains(needle),
            "expected the bounded-step bound `{needle}` among kernel-proved lemmas.\n--- DiscoveredLemmas.lean ---\n{committed}",
        );
    }
    let _ = std::fs::remove_dir_all(&output_dir);
}

// ===========================================================================
// Born-as-Aver discovery (`--discover --emit-laws`): a discovered law is born
// AS an Aver law in a sidecar `<file>.discovered.av`, so it flows through the
// same pipeline as a user law (verify, the cross-file pool, `aver proof
// --check --gate`). Scope (v1): the `Conjecture` family only.
// ===========================================================================

/// Copy a corpus `.av` into a fresh temp file (and copy any dep modules in its
/// directory alongside it, so the sidecar's `--module-root` still resolves) and
/// return the temp file path + its temp module-root dir.
fn stage_corpus(corpus_rel: &str, prefix: &str) -> (PathBuf, PathBuf) {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let src = repo_root.join(corpus_rel);
    let dir = temp_output_dir(prefix);
    std::fs::create_dir_all(&dir).expect("mk temp dir");
    let file_name = src.file_name().expect("corpus file name");
    let dst = dir.join(file_name);
    std::fs::copy(&src, &dst).expect("copy corpus");
    (dst, dir)
}

/// Run `aver proof <file> --discover --emit-laws` (sidecar written next to
/// `file`). Returns the sidecar path + the tool's stdout.
fn run_emit_laws(file: &PathBuf, module_root: &PathBuf) -> (PathBuf, String) {
    let run = Command::new(env!("CARGO_BIN_EXE_aver"))
        .arg("proof")
        .arg(file)
        .arg("--discover")
        .arg("--emit-laws")
        .arg("--module-root")
        .arg(module_root)
        .output()
        .expect("expected `aver proof --discover --emit-laws` to run");
    let sidecar = PathBuf::from(format!("{}.discovered.av", file.display()));
    (sidecar, String::from_utf8_lossy(&run.stdout).into_owned())
}

/// Run a subcommand and return `(exit_code, stdout)`.
fn run_subcommand(args: &[&str]) -> (Option<i32>, String) {
    let out = Command::new(env!("CARGO_BIN_EXE_aver"))
        .args(args)
        .output()
        .expect("aver subcommand");
    (
        out.status.code(),
        format!(
            "{}{}",
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        ),
    )
}

/// THE unification acceptance: `--emit-laws` on a corpus that yields
/// `Conjecture` survivors produces a sidecar that (a) PARSES + typechecks
/// (`aver check`), (b) VERIFIES (`aver verify`, the strict forward check the
/// laws already cleared), and (c) at least one emitted law, run through `aver
/// proof <sidecar> --check`, CREDITS as a kernel-proved universal — end to end.
/// Lake-gated.
#[test]
fn emit_laws_sidecar_parses_verifies_and_credits_when_lake_is_available() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping emit-laws credit test: `lake` not available");
        return;
    }
    let (file, root) = stage_corpus("examples/data/rle.av", "aver-emitlaws-rle");
    let (sidecar, stdout) = run_emit_laws(&file, &root);
    assert!(
        sidecar.exists(),
        "sidecar not written.\n--- stdout ---\n{stdout}"
    );
    assert!(
        stdout.contains("cleared the hostile-widened forward check"),
        "emit-laws did not report the hostile-widened-check bar.\n{stdout}"
    );

    // (a) PARSES + typechecks.
    let (check_code, check_out) = run_subcommand(&[
        "check",
        sidecar.to_str().unwrap(),
        "--module-root",
        root.to_str().unwrap(),
    ]);
    assert_eq!(
        check_code,
        Some(0),
        "sidecar failed `aver check`:\n{check_out}"
    );

    // (b) the emitted laws VERIFY (strict forward check; the source's own laws
    // are sound on rle, so the whole sidecar is 0 failed).
    let (verify_code, verify_out) = run_subcommand(&[
        "verify",
        sidecar.to_str().unwrap(),
        "--module-root",
        root.to_str().unwrap(),
    ]);
    assert_eq!(
        verify_code,
        Some(0),
        "sidecar failed `aver verify`:\n{verify_out}"
    );
    assert!(
        verify_out.contains("0 failed"),
        "sidecar had verify failures:\n{verify_out}"
    );

    // (c) at least one emitted law credits as a kernel-proved universal.
    // `--check` succeeds (exit 0) within a generous sorry budget (the source's
    // own roundtrip/string_roundtrip laws still sorry); then we confirm a
    // DISCOVERED universal closed with NO `sorry` warning at its line.
    let proof_out = temp_output_dir("aver-emitlaws-rle-proof");
    let (_proof_code, proof_stdout) = run_subcommand(&[
        "proof",
        sidecar.to_str().unwrap(),
        "--module-root",
        root.to_str().unwrap(),
        "-o",
        proof_out.to_str().unwrap(),
        "--check",
        "--check-json",
        "--sorry-budget",
        "200",
    ]);
    assert!(
        proof_stdout.contains("\"passed\":true"),
        "sidecar proof did not build within budget:\n{proof_stdout}"
    );

    // BLOCKER 2: credit is gated on the TRANSITIVE `#print axioms`, NOT a
    // sorry-at-own-line heuristic. Every credited universal's axioms must be
    // whitelist-only (zero sorryAx, zero ofReduceBool) — a law citing a sorry'd
    // FALSE lemma as a rewrite rule surfaces `sorryAx` here and is NOT counted.
    let axioms = probe_discovered_axioms(&proof_out);
    let credited = axioms.values().filter(|clean| **clean).count();
    assert!(
        credited > 0,
        "no discovered universal law kernel-credits (whitelist-only #print axioms).\n{proof_stdout}"
    );
    // No theorem we count as credited may carry sorryAx — that is the laundering
    // the panel caught (109 claimed, 74 transitively sorryAx). The `clean` flag
    // already encodes this; the count being > 0 with the gate in place proves
    // the honest credit path works end to end.
    assert_eq!(
        count_credited_discovered_universals(&proof_out),
        credited,
        "credit count diverged from the #print-axioms probe"
    );

    let _ = std::fs::remove_dir_all(&root);
    let _ = std::fs::remove_dir_all(&proof_out);
}

/// BLOCKER 2 (the laundering guard): every discovered universal the sidecar's
/// proof labels kernel-credited has a TRANSITIVE `#print axioms` set ⊆
/// {propext, Classical.choice, Quot.sound} — ZERO `sorryAx`, zero
/// `Lean.ofReduceBool`. This is the gate the cherry-picked test missed: a law
/// can be sorry-free at its OWN line yet cite a sorry'd FALSE lemma as a rewrite
/// rule, so its transitive axioms contain `sorryAx`. We probe `#print axioms`
/// (transitive by construction) on EVERY discovered universal and assert NONE
/// that the credit gate counts depends on a non-whitelist axiom. Lake-gated.
#[test]
fn emit_laws_credited_universals_have_no_sorry_ax_when_lake_is_available() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping #print-axioms credit test: `lake` not available");
        return;
    }
    let (file, root) = stage_corpus("examples/data/rle.av", "aver-emitlaws-rle-axioms");
    let (sidecar, stdout) = run_emit_laws(&file, &root);
    assert!(sidecar.exists(), "sidecar not written.\n{stdout}");

    let proof_out = temp_output_dir("aver-emitlaws-rle-axioms-proof");
    let (_code, proof_stdout) = run_subcommand(&[
        "proof",
        sidecar.to_str().unwrap(),
        "--module-root",
        root.to_str().unwrap(),
        "-o",
        proof_out.to_str().unwrap(),
        "--check",
        "--check-json",
        "--sorry-budget",
        "200",
    ]);
    assert!(
        proof_stdout.contains("\"passed\":true"),
        "sidecar proof did not build within budget:\n{proof_stdout}"
    );

    // Probe transitive axioms for EVERY discovered universal.
    let axioms = probe_discovered_axioms(&proof_out);
    assert!(
        !axioms.is_empty(),
        "no discovered universal theorems found to probe.\n{proof_stdout}"
    );
    let credited: Vec<&String> = axioms
        .iter()
        .filter_map(|(t, clean)| clean.then_some(t))
        .collect();
    assert!(
        !credited.is_empty(),
        "no discovered universal kernel-credits cleanly (whitelist-only axioms).\n{proof_stdout}"
    );
    // Re-probe the credited set DIRECTLY and assert the raw `#print axioms`
    // output contains neither `sorryAx` nor `Lean.ofReduceBool` for any of them.
    let raw = raw_print_axioms(&proof_out, &credited);
    assert!(
        !raw.contains("sorryAx"),
        "a kernel-credited discovered universal transitively depends on sorryAx (laundering):\n{raw}"
    );
    assert!(
        !raw.contains("Lean.ofReduceBool"),
        "a kernel-credited discovered universal depends on native_decide (ofReduceBool):\n{raw}"
    );

    let _ = std::fs::remove_dir_all(&root);
    let _ = std::fs::remove_dir_all(&proof_out);
}

/// Run `#print axioms <thm>` for each named theorem against the built env and
/// return the combined raw output (for direct sorryAx/ofReduceBool assertions).
fn raw_print_axioms(proof_out: &PathBuf, thms: &[&String]) -> String {
    let (lean_path, _all) = discovered_universal_theorems(proof_out);
    let root = lean_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("Rle")
        .to_string();
    let mut src = format!("import {root}\n");
    for t in thms {
        src.push_str(&format!("#print axioms {t}\n"));
    }
    let checker = proof_out.join("_aver_emit_axcheck_raw.lean");
    if std::fs::write(&checker, &src).is_err() {
        return String::new();
    }
    let probe = Command::new("lake")
        .current_dir(proof_out)
        .args(["env", "lean", "_aver_emit_axcheck_raw.lean"])
        .output()
        .expect("lake env lean for raw #print axioms");
    let _ = std::fs::remove_file(&checker);
    format!(
        "{}{}",
        String::from_utf8_lossy(&probe.stdout),
        String::from_utf8_lossy(&probe.stderr)
    )
}

/// The kernel-axiom whitelist a genuinely-universal proof may depend on.
/// `sorryAx` (a sorry-floored proof) and `Lean.ofReduceBool` (`native_decide`,
/// bounded enumeration) are NOT here — a law citing either is not credited.
const KERNEL_AXIOM_WHITELIST: [&str; 3] = ["propext", "Classical.choice", "Quot.sound"];

/// Names of the discovered universal law theorems (`*_aver_discovered_*`, the
/// `∀`-claim — not the `_checked_domain` / `_sample_N` bounded cross-checks) in
/// the generated Lean. These are the candidates for kernel credit.
fn discovered_universal_theorems(proof_out: &PathBuf) -> (PathBuf, Vec<String>) {
    let lean_path = proof_out.join("Rle.lean");
    let Ok(src) = std::fs::read_to_string(&lean_path) else {
        return (lean_path, Vec::new());
    };
    let thms = src
        .lines()
        .filter_map(|line| {
            let t = line.trim_start();
            if !t.starts_with("theorem ") {
                return None;
            }
            let name = t["theorem ".len()..]
                .split_whitespace()
                .next()
                .unwrap_or("")
                .trim_end_matches(':');
            if name.contains("aver_discovered_")
                && line.contains(": ∀")
                && !name.contains("_checked_domain")
                && !name.contains("_sample_")
            {
                Some(name.to_string())
            } else {
                None
            }
        })
        .collect();
    (lean_path, thms)
}

/// `true` iff a `#print axioms <thm>` block in `combined` for `thm` reports
/// either "does not depend on any axioms" or an axiom list ⊆ the whitelist —
/// and, belt-and-suspenders, names neither `sorryAx` nor `Lean.ofReduceBool`.
/// This is the SAME transitive `#print axioms` gate the product's
/// `lean_universal_audit` applies (Lean's `#print axioms` is transitive: it
/// reports the axioms the whole proof term — across every cited lemma —
/// ultimately rests on, so a law citing a sorry'd FALSE lemma surfaces
/// `sorryAx` here even with no literal `sorry` at its own line).
fn theorem_axioms_clean(combined: &str, thm: &str) -> bool {
    let needle = format!("'{thm}' ");
    let Some(start) = combined.find(&needle) else {
        // No probe output for this theorem (failed to resolve) — not credited.
        return false;
    };
    // The block runs to the next `'<name>'` probe header or end of output.
    let rest = &combined[start + needle.len()..];
    let block_end = rest.find("\n'").unwrap_or(rest.len());
    let block = &rest[..block_end];
    if block.contains("sorryAx") || block.contains("Lean.ofReduceBool") {
        return false;
    }
    if block.contains("does not depend on any axioms") {
        return true;
    }
    // `depends on axioms: [a, b, c]` — every listed axiom must be whitelisted.
    let Some(open) = block.find('[') else {
        // Has an axioms header but no list we can parse → fail closed.
        return false;
    };
    let Some(close) = block[open..].find(']') else {
        return false;
    };
    let list = &block[open + 1..open + close];
    list.split(',')
        .map(|a| a.trim())
        .filter(|a| !a.is_empty())
        .all(|a| KERNEL_AXIOM_WHITELIST.contains(&a))
}

/// Count discovered universal theorems (`*_aver_discovered_*`) that genuinely
/// KERNEL-CREDIT: their transitive `#print axioms` set is whitelist-only (zero
/// `sorryAx`, zero `Lean.ofReduceBool`). Emits a throwaway `#print axioms`
/// checker against the built environment — the SAME mechanism the product
/// `--check` credit gate uses — NOT a sorry-at-own-line heuristic (which a law
/// citing a sorry'd false lemma defeats).
fn count_credited_discovered_universals(proof_out: &PathBuf) -> usize {
    let axioms = probe_discovered_axioms(proof_out);
    axioms.values().filter(|clean| **clean).count()
}

/// Probe every discovered universal theorem's transitive `#print axioms`.
/// Returns `theorem -> is-kernel-clean`. Empty if the Lean env can't be built
/// or there are no discovered universals.
fn probe_discovered_axioms(proof_out: &PathBuf) -> std::collections::HashMap<String, bool> {
    let (lean_path, thms) = discovered_universal_theorems(proof_out);
    let mut out = std::collections::HashMap::new();
    if thms.is_empty() {
        return out;
    }
    // Import the entry root, then `#print axioms` each discovered theorem.
    let root = lean_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("Rle")
        .to_string();
    let mut src = format!("import {root}\n");
    for t in &thms {
        src.push_str(&format!("#print axioms {t}\n"));
    }
    let checker = proof_out.join("_aver_emit_axcheck.lean");
    if std::fs::write(&checker, &src).is_err() {
        return out;
    }
    let probe = Command::new("lake")
        .current_dir(proof_out)
        .args(["env", "lean", "_aver_emit_axcheck.lean"])
        .output()
        .expect("lake env lean for #print axioms");
    let _ = std::fs::remove_file(&checker);
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&probe.stdout),
        String::from_utf8_lossy(&probe.stderr)
    );
    for t in &thms {
        out.insert(t.clone(), theorem_axioms_clean(&combined, t));
    }
    out
}

/// Split a `verify` law body line `LHS => RHS` (the indented case line) into its
/// trimmed `(lhs, rhs)`. `None` for non-case lines.
fn law_case_sides(line: &str) -> Option<(&str, &str)> {
    let t = line.trim();
    let (lhs, rhs) = t.split_once(" => ")?;
    Some((lhs.trim(), rhs.trim()))
}

/// `true` iff `lhs => rhs` is a PROVABLY-FALSE concat-commutativity twin:
/// `concat(f(x), g(y)) == concat(g(y), f(x))` where the two swapped operands are
/// DISTINCT single-binder function applications (e.g. `concat(flushAcc x6,
/// flushAcc x7) == concat(flushAcc x7, flushAcc x6)` — the lemma_8 family).
///
/// Crucially this does NOT flag the TRUE swaps the gate legitimately keeps:
/// - associativity `concat(concat(x,x),x) == concat(x,concat(x,x))` (a nested-
///   concat operand, not a single-binder `fn(x)`), and
/// - roundtrip identity `concat(decode(encode(x)), x) == concat(x,
///   decode(encode(x)))` (a BARE-binder operand `x`, which is provably equal to
///   `decode(encode(x))`). Those are genuinely true and pass hostile-verify.
///
/// We require both operands to be `fn(binder)` applications (an `(` then a bare
/// `x<digits>` arg), distinct from each other — the shape that is false unless
/// `fn` is a constant.
fn is_concat_commutativity_twin(lhs: &str, rhs: &str) -> bool {
    let Some(a) = concat_args(lhs) else {
        return false;
    };
    let Some(b) = concat_args(rhs) else {
        return false;
    };
    a.0 == b.1 && a.1 == b.0 && a.0 != a.1 && is_fn_of_binder(&a.0) && is_fn_of_binder(&a.1)
}

/// `true` iff `s` is a single-binder function application `fn(xN)` (the operand
/// shape of a false commutativity twin — not a bare binder, not a nested call).
fn is_fn_of_binder(s: &str) -> bool {
    let s = s.trim();
    let Some(open) = s.find('(') else {
        return false;
    };
    if !s.ends_with(')') || open == 0 {
        return false;
    }
    let inner = s[open + 1..s.len() - 1].trim();
    // Arg is a single binder `x<digits>`, no nested call / comma.
    inner.starts_with('x') && inner.len() > 1 && inner[1..].chars().all(|c| c.is_ascii_digit())
}

/// `true` iff `lhs => rhs` is a FALSE SWAPPED homomorphism:
/// `f(concat(x, y)) == concat(f(y), f(x))` with x ≠ y (the args swapped on the
/// rhs — the true form is `concat(f(x), f(y))`).
fn is_swapped_homomorphism(lhs: &str, rhs: &str) -> bool {
    // lhs: f(List.concat(x, y))
    let lhs = lhs.trim();
    let Some(open) = lhs.find('(') else {
        return false;
    };
    if !lhs.ends_with(')') {
        return false;
    }
    let inner = &lhs[open + 1..lhs.len() - 1];
    let f = &lhs[..open];
    let Some((x, y)) = concat_args(inner) else {
        return false;
    };
    if x == y {
        return false; // f(concat(x,x)) is symmetric — not a false twin.
    }
    // rhs: List.concat(f(y), f(x)) — args swapped.
    let Some((r0, r1)) = concat_args(rhs) else {
        return false;
    };
    r0 == format!("{f}({y})") && r1 == format!("{f}({x})")
}

/// Parse `List.concat(A, B)` → `(A, B)` at top level (depth-aware split on the
/// single top-level comma). `None` if not a `List.concat(..)` call.
fn concat_args(s: &str) -> Option<(String, String)> {
    let s = s.trim();
    let inner = s.strip_prefix("List.concat(")?.strip_suffix(')')?;
    let mut depth = 0i32;
    for (i, ch) in inner.char_indices() {
        match ch {
            '(' | '[' => depth += 1,
            ')' | ']' => depth -= 1,
            ',' if depth == 0 => {
                return Some((
                    inner[..i].trim().to_string(),
                    inner[i + 1..].trim().to_string(),
                ));
            }
            _ => {}
        }
    }
    None
}

/// HONESTY guard (the headline correctness property): NO false law reaches the
/// sidecar. The gate is the HOSTILE-WIDENED forward check, not the sampler's
/// symmetric domain — so a `Conjecture` that is false (concat-commutativity, or
/// a SWAPPED homomorphism) is refuted and dropped, NEVER emitted. This asserts
/// the ACTUAL false shapes are absent (every orientation, scanned structurally),
/// not one cherry-picked string. `rle.av`'s enumerator mints exactly these false
/// twins (`concat(flushAcc x, flushAcc y) == concat(flushAcc y, flushAcc x)`,
/// `decode(concat x y) == concat(decode y, decode x)`) — the lemma_8 / lemma_13 /
/// lemma_175 shapes the panel caught. Does not need lake.
#[test]
fn emit_laws_drops_false_survivor_refuted_by_strict_check() {
    let (file, root) = stage_corpus("examples/data/rle.av", "aver-emitlaws-rle-neg");
    let (sidecar, stdout) = run_emit_laws(&file, &root);

    // The hostile-widened check refutes the false twins it minted…
    assert!(
        stdout.contains("refuted by hostile-widened forward check"),
        "no candidate was refuted by the hostile-widened check (expected the spurious concat commutativity / swapped homomorphism).\n{stdout}"
    );

    // …and NOT ONE false twin survives into the sidecar, in ANY orientation.
    // Scan EVERY emitted law case structurally — both the concat-commutativity
    // shape AND the swapped homomorphism shape (the lemma_8 / lemma_13 /
    // lemma_175 families). The sidecar contains the verbatim source at the top,
    // so restrict the scan to the discovered laws (after the provenance header).
    let sidecar_src = std::fs::read_to_string(&sidecar).expect("read sidecar");
    let discovered = sidecar_src
        .split_once("Machine-proposed laws")
        .map(|(_, after)| after)
        .unwrap_or(&sidecar_src);

    let mut offenders = Vec::new();
    for line in discovered.lines() {
        if let Some((lhs, rhs)) = law_case_sides(line) {
            if is_concat_commutativity_twin(lhs, rhs) {
                offenders.push(format!("CONCAT-COMMUTATIVITY: {lhs} => {rhs}"));
            }
            if is_swapped_homomorphism(lhs, rhs) {
                offenders.push(format!("SWAPPED-HOMOMORPHISM: {lhs} => {rhs}"));
            }
        }
    }
    assert!(
        offenders.is_empty(),
        "false law(s) were emitted into the sidecar (must be refuted by hostile-widened verify):\n{}",
        offenders.join("\n")
    );

    // Also assert the canonical concrete false shapes from the panel are absent,
    // in BOTH orientations (belt-and-suspenders over the structural scan).
    for false_law in [
        // lemma_13 shape: swapped homomorphism (and its mirror).
        "decode(List.concat(x2, x3)) => List.concat(decode(x3), decode(x2))",
        "decode(List.concat(x3, x2)) => List.concat(decode(x2), decode(x3))",
    ] {
        assert!(
            !discovered.contains(false_law),
            "refuted candidate `{false_law}` was emitted into the sidecar"
        );
    }

    // The TRUE homomorphism must still survive (the gate refutes false, keeps
    // true — it is not just dropping everything).
    assert!(
        discovered.contains("decode(List.concat(x2, x3)) => List.concat(decode(x2), decode(x3))"),
        "the TRUE decode homomorphism was dropped — gate is over-conservative.\n{discovered}"
    );

    // GROUND TRUTH (the definitive property): the whole sidecar passes
    // `aver verify --hostile` with ZERO failures — i.e. not one emitted law is
    // false under the adversarial, distinguishing, boundary-widened domain. This
    // is the same gate the emit path applies, re-run independently end to end.
    let (vcode, vout) = run_subcommand(&[
        "verify",
        sidecar.to_str().unwrap(),
        "--module-root",
        root.to_str().unwrap(),
        "--hostile",
    ]);
    assert_eq!(
        vcode,
        Some(0),
        "sidecar failed `aver verify --hostile`:\n{vout}"
    );
    assert!(
        vout.contains("0 failed"),
        "sidecar has a law that FAILS hostile verify (a false law survived):\n{vout}"
    );

    let _ = std::fs::remove_dir_all(&root);
}

/// SOURCE IS NEVER MUTATED: the original `.av` is byte-identical before and
/// after an `--emit-laws` run. The sidecar is a separate file. Does not need
/// lake.
#[test]
fn emit_laws_never_mutates_source() {
    let (file, root) = stage_corpus("examples/data/rle.av", "aver-emitlaws-rle-immut");
    let before = std::fs::read(&file).expect("read source before");
    let (sidecar, _stdout) = run_emit_laws(&file, &root);
    let after = std::fs::read(&file).expect("read source after");
    assert_eq!(
        before, after,
        "`--emit-laws` mutated the user's source file (must only write the sidecar)"
    );
    assert!(sidecar.exists(), "sidecar should still be written");
    let _ = std::fs::remove_dir_all(&root);
}

/// BLOCKER 3: `--emit-laws-to` MUST refuse to overwrite the source (or any input
/// `.av`) — the "never mutates source" contract holds for the override path too.
/// Pointed at the source itself, AND at a `./`-alias, AND at a symlink to the
/// source: each must fail closed (non-zero exit) and leave the source
/// byte-identical. Does not need lake.
#[test]
fn emit_laws_to_refuses_to_overwrite_source() {
    let (file, root) = stage_corpus("examples/data/rle.av", "aver-emitlaws-rle-to-guard");
    let before = std::fs::read(&file).expect("read source before");
    let file_str = file.to_str().unwrap().to_string();
    let root_str = root.to_str().unwrap().to_string();

    // (1) target == source.
    let (code, out) = run_subcommand(&[
        "proof",
        &file_str,
        "--discover",
        "--emit-laws",
        "--emit-laws-to",
        &file_str,
        "--module-root",
        &root_str,
    ]);
    assert_eq!(
        code,
        Some(1),
        "--emit-laws-to <source> must fail closed (exit 1):\n{out}"
    );
    assert!(
        out.contains("would overwrite the input source"),
        "expected a clear refusal message:\n{out}"
    );

    // (2) target is a `./`-alias of the source.
    let alias = format!(
        "{}/./{}",
        root_str,
        file.file_name().unwrap().to_str().unwrap()
    );
    let (code2, _o2) = run_subcommand(&[
        "proof",
        &file_str,
        "--discover",
        "--emit-laws",
        "--emit-laws-to",
        &alias,
        "--module-root",
        &root_str,
    ]);
    assert_eq!(
        code2,
        Some(1),
        "--emit-laws-to <source-via-dot-alias> must fail closed"
    );

    // (3) target is a symlink to the source (alias via canonicalize).
    let link = root.join("rle_link.av");
    if std::os::unix::fs::symlink(&file, &link).is_ok() {
        let (code3, _o3) = run_subcommand(&[
            "proof",
            &file_str,
            "--discover",
            "--emit-laws",
            "--emit-laws-to",
            link.to_str().unwrap(),
            "--module-root",
            &root_str,
        ]);
        assert_eq!(
            code3,
            Some(1),
            "--emit-laws-to <symlink-to-source> must fail closed"
        );
    }

    // Source untouched throughout.
    let after = std::fs::read(&file).expect("read source after");
    assert_eq!(
        before, after,
        "--emit-laws-to overwrote / mutated the source despite the guard"
    );

    // A DISTINCT target still works (the guard isn't over-broad).
    let good = root.join("explicit_sidecar.av");
    let (code4, out4) = run_subcommand(&[
        "proof",
        &file_str,
        "--discover",
        "--emit-laws",
        "--emit-laws-to",
        good.to_str().unwrap(),
        "--module-root",
        &root_str,
    ]);
    assert_eq!(
        code4,
        Some(0),
        "a distinct --emit-laws-to target must succeed:\n{out4}"
    );
    assert!(good.exists(), "explicit sidecar not written");

    let _ = std::fs::remove_dir_all(&root);
}

/// SIDECAR `aver check` CLEAN (the MAJOR fix's acceptance): the written sidecar
/// — including any tuple-typed `given` annotations, which MUST render as
/// `Tuple<...>` not `(A, B)` — parses AND typechecks. Any shape that doesn't
/// render to parse-legal + typecheck-legal Aver is DROPPED, never written. Does
/// not need lake.
#[test]
fn emit_laws_sidecar_checks_clean() {
    let (file, root) = stage_corpus("examples/data/rle.av", "aver-emitlaws-rle-checkclean");
    let (sidecar, stdout) = run_emit_laws(&file, &root);
    assert!(sidecar.exists(), "sidecar not written.\n{stdout}");

    // No paren-tuple annotation leaked into a `given` clause (the MAJOR bug).
    let src = std::fs::read_to_string(&sidecar).expect("read sidecar");
    for line in src.lines() {
        let t = line.trim_start();
        if let Some(rest) = t.strip_prefix("given ")
            && let Some((_, ann)) = rest.split_once(':')
        {
            let ann = ann.split('=').next().unwrap_or(ann).trim();
            assert!(
                !(ann.starts_with('(') && ann.contains(',')),
                "a paren-tuple `given` annotation leaked (must be `Tuple<...>`): `{line}`"
            );
        }
    }

    let (check_code, check_out) = run_subcommand(&[
        "check",
        sidecar.to_str().unwrap(),
        "--module-root",
        root.to_str().unwrap(),
    ]);
    assert_eq!(
        check_code,
        Some(0),
        "sidecar failed `aver check` (must be parse + typecheck clean):\n{check_out}"
    );
    let _ = std::fs::remove_dir_all(&root);
}

/// DETERMINISM: two `--emit-laws` runs over the same source produce a
/// byte-identical sidecar (stable law order, stable `given` formatting). Does
/// not need lake.
#[test]
fn emit_laws_sidecar_is_byte_reproducible() {
    let (file, root) = stage_corpus("examples/data/rle.av", "aver-emitlaws-rle-det");
    let (sidecar, _a) = run_emit_laws(&file, &root);
    let first = std::fs::read(&sidecar).expect("read sidecar 1");
    let (_sidecar2, _b) = run_emit_laws(&file, &root);
    let second = std::fs::read(&sidecar).expect("read sidecar 2");
    assert_eq!(
        first, second,
        "sidecar is not byte-reproducible across reruns"
    );
    let _ = std::fs::remove_dir_all(&root);
}

/// Fast end-to-end (no lake) on the minimal fixture: the sidecar parses,
/// typechecks, verifies, names are reserved-prefixed + deduped, and the source
/// is unchanged.
#[test]
fn emit_laws_fixture_parses_and_verifies() {
    let (file, root) = stage_corpus("tests/fixtures/emit_laws_rle.av", "aver-emitlaws-fixture");
    let (sidecar, stdout) = run_emit_laws(&file, &root);
    assert!(sidecar.exists(), "sidecar not written.\n{stdout}");

    let sidecar_src = std::fs::read_to_string(&sidecar).expect("read sidecar");
    // Provenance header + reserved prefix.
    assert!(
        sidecar_src.contains("Machine-proposed laws"),
        "missing provenance header"
    );
    assert!(
        sidecar_src.contains("verify decode law aver_discovered_"),
        "missing reserved-prefix discovered law"
    );
    // The decode homomorphism is among the emitted laws.
    assert!(
        sidecar_src.contains("decode(List.concat(x2, x3)) => List.concat(decode(x2), decode(x3))"),
        "decode homomorphism not emitted.\n{sidecar_src}"
    );

    let (check_code, check_out) = run_subcommand(&[
        "check",
        sidecar.to_str().unwrap(),
        "--module-root",
        root.to_str().unwrap(),
    ]);
    assert_eq!(
        check_code,
        Some(0),
        "fixture sidecar `aver check`:\n{check_out}"
    );

    let (verify_code, verify_out) = run_subcommand(&[
        "verify",
        sidecar.to_str().unwrap(),
        "--module-root",
        root.to_str().unwrap(),
    ]);
    assert_eq!(
        verify_code,
        Some(0),
        "fixture sidecar `aver verify`:\n{verify_out}"
    );
    assert!(
        verify_out.contains("0 failed"),
        "fixture verify failures:\n{verify_out}"
    );

    let _ = std::fs::remove_dir_all(&root);
}

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

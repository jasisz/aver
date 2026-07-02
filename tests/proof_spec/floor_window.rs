use super::*;

/// Floor-division window family (`tests/fixtures/floor_window.av`):
/// laws over a power-of-two fn, a floor-halving binary-exponent fn
/// (recursion on `Result.withDefault(Int.div(a, 2), 0)` through a
/// unary wrapper), and the scaled-significand / bit-width window
/// predicates built from them.
///
/// Export-structure pin, Lean side (no toolchain needed). Before the
/// floor-division countdown class existed, the binary-exponent fn
/// emitted as a kernel-opaque `partial def` (nothing universal about
/// it was provable as emitted) and the power fn as a fuel helper
/// with a `panic!` exhaustion arm; the law theorems were
/// sampled-domain-bounded (`native_decide` leaves) or a bare sorry.
/// Post-fix:
/// - the halving fn is a well-founded def (`termination_by a.toNat`
///   with a kernel-checked `decreasing_by`), no `partial def` left;
/// - the power fn graduates out of fuel (no `__fuel` helper, no
///   panic) because the window figures need its defining equations;
/// - all four law theorems carry the `universal` statement-class
///   marker and the file contains NO sorry token.
#[test]
fn proof_export_floor_window_lean_structure() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output_dir = temp_output_dir("aver-proof-floor-window-export");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("tests/fixtures/floor_window.av")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("aver proof should run");
    assert!(run.status.success(), "{}", format_output(&run));
    let lean = std::fs::read_to_string(output_dir.join("FloorWindow.lean"))
        .expect("FloorWindow.lean must be emitted");

    // The recursion class: a native well-founded def, never partial.
    assert!(
        !lean.contains("partial def"),
        "floor-halving recursion must emit as a well-founded def"
    );
    assert!(lean.contains("def widthExp"));
    assert!(lean.contains("termination_by a.toNat"));
    assert!(
        lean.contains("all_goals (simp [halve, Except.withDefault] <;> omega)"),
        "the halving wrapper must be unfolded in the decreasing goal"
    );
    // The demand-driven pow graduation: no fuel helper, no panic.
    assert!(
        !lean.contains("twoPow__fuel"),
        "the window figures graduate the power fn out of fuel"
    );
    assert!(lean.contains("termination_by n.toNat"));

    // All four law theorems replace their bounded statements with the
    // TRUE universal form and are classed `universal`.
    for base in [
        "twoPow_law_positive",
        "twoPow_law_sumSplits",
        "windowSig_law_sigWindow",
        "widthsAdd_law_productWindow",
    ] {
        assert!(
            lean.contains(&format!("-- aver:law-class {base} universal")),
            "{base} must be classed universal"
        );
    }
    // The support stack is present and the file holds no sorry and no
    // native_decide on the law theorems' proofs (samples keep theirs).
    assert!(lean.contains("windowSig_law_sigWindow__exp_window"));
    assert!(lean.contains("Int.le_ediv_iff_mul_le"));
    assert!(lean.contains("Int.ediv_lt_iff_lt_mul"));
    // No law theorem REACHES sorry. The `universal` law-class assertions above
    // already guarantee that (a proof that reaches sorry is classed `bounded`).
    // The only `sorry` tokens permitted are the recursive kit's UNREACHED
    // fail-safe floors `first | <proof> | sorry` — standard across the engine and
    // never taken when the proof closes. (The blanket no-`sorry` check this
    // replaced only ever held because a post-check scrub rewrote sorry->fail in
    // every file, masking these honest floors; that scrub was removed.)
    let reached_sorry: Vec<&str> = lean
        .lines()
        .filter(|l| l.contains("sorry") && !l.contains("| sorry"))
        .collect();
    assert!(
        reached_sorry.is_empty(),
        "the floor-window fixture must not REACH sorry (only `first | proof | sorry` fail-safe floors are allowed); offending lines:\n{}",
        reached_sorry.join("\n")
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Second, cross-domain WITNESS for the Euclidean-floor arithmetic rungs
/// (`tests/fixtures/floor_arith_witness.av`). The cancel / absorb rungs are
/// keyed only on the CLAIM SHAPE (`floor (a * c) (d * c) = floor a d`;
/// `floor (d * q + r) d = q`), never on the K5 `floorDiv` name. This fixture
/// states the SAME two shapes over a differently-named floor-division fn
/// (`quotFloor`, over `num`/`den`) in a different module with different given
/// names — if the rungs were name-blind, both laws close `universal` here too.
/// Export-structure pin (no toolchain needed): asserts the `universal`
/// law-class markers and no REACHED sorry.
#[test]
fn proof_export_floor_arith_second_witness_universal() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output_dir = temp_output_dir("aver-proof-floor-arith-witness");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("tests/fixtures/floor_arith_witness.av")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("aver proof should run");
    assert!(run.status.success(), "{}", format_output(&run));
    let lean = std::fs::read_to_string(output_dir.join("FloorArithWitness.lean"))
        .expect("FloorArithWitness.lean must be emitted");

    // Every cross-domain law theorem replaces its bounded statement with the
    // TRUE universal form and is classed `universal` — the rungs fired on a
    // fn with no `floorDiv` / K5 name. The `Commuted` witnesses additionally
    // pin the ORIENTATION-TOLERANT path: shared factor / divisor multiplicand
    // written on the opposite side, an alternative positivity spelling
    // (`k > 0`, `den > 0`) and reordered when-clauses.
    for base in [
        "quotFloor_law_shrinkFactor",
        "quotFloor_law_soakRemainder",
        "quotFloor_law_shrinkFactorCommuted",
        "quotFloor_law_shrinkFactorDivisorLeft",
        "quotFloor_law_soakRemainderCommuted",
    ] {
        assert!(
            lean.contains(&format!("-- aver:law-class {base} universal")),
            "{base} must be classed universal (rung is name- and orientation-blind); got:\n{lean}"
        );
    }
    // The core lemmas the rungs cite are present, not a sampled `native_decide`
    // fallback or a bounded statement.
    assert!(lean.contains("Int.mul_ediv_mul_of_pos_left"));
    assert!(lean.contains("Int.add_mul_ediv_left"));
    // The commuted witnesses drive the `Int.mul_comm` normalization step that
    // rewrites the written product into the core lemma's canonical operand order.
    assert!(
        lean.contains("rw [Int.mul_comm k num]"),
        "commuted cancel witness must normalize the shared factor with Int.mul_comm"
    );
    assert!(
        lean.contains("rw [Int.mul_comm whole den]"),
        "commuted absorb witness must normalize the divisor multiplicand with Int.mul_comm"
    );
    // The fourth cancel corner puts the shared factor on the LEFT of the DIVISOR
    // product; its normalization commutes the divisor multiplicand (not the
    // dividend), a path no other witness exercises.
    assert!(
        lean.contains("rw [Int.mul_comm scale base]"),
        "divisor-left cancel witness must normalize the divisor factor with Int.mul_comm"
    );
    // No law theorem REACHES sorry (only `first | proof | sorry` fail-safe
    // floors are permitted, and these rungs emit none).
    let reached_sorry: Vec<&str> = lean
        .lines()
        .filter(|l| l.contains("sorry") && !l.contains("| sorry"))
        .collect();
    assert!(
        reached_sorry.is_empty(),
        "the second-witness fixture must not REACH sorry; offending lines:\n{}",
        reached_sorry.join("\n")
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Export-structure pin, Dafny side (no toolchain needed). Before the
/// class existed the binary-exponent fn declined to an opaque
/// `{:axiom}` and the significand law was omitted; the other three
/// law lemmas emitted with empty bodies Z3 could not close (measured:
/// 3 errors). Post-fix:
/// - the halving fn emits with the total guarded measure
///   (`decreases if a >= 0 then a else 0`) and NO synthesized
///   `requires`, so total callers stay wellformed;
/// - every law lemma carries a PROVED support stack (division-window
///   prelude derived from the Euclidean identity, power algebra by
///   self-call induction, branch-split significand lemmas) — no
///   `assume {:axiom}`, no omitted universal.
#[test]
fn proof_export_floor_window_dafny_structure() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output_dir = temp_output_dir("aver-proof-floor-window-dafny-export");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("tests/fixtures/floor_window.av")
        .arg("--backend")
        .arg("dafny")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("aver proof should run");
    assert!(run.status.success(), "{}", format_output(&run));
    let dfy = std::fs::read_to_string(output_dir.join("FloorWindow.dfy"))
        .expect("FloorWindow.dfy must be emitted");

    assert!(
        !dfy.contains("{:axiom}"),
        "no opaque axiom decline and no assume {{:axiom}} trust escape"
    );
    assert!(
        dfy.contains("decreases if a >= 0 then a else 0"),
        "floor-halving recursion gets the total guarded measure"
    );
    assert!(
        !dfy.contains("requires a >= 0\n  decreases"),
        "no synthesized requires on the floor-halving fn"
    );
    assert!(
        !dfy.contains("universal lemma omitted"),
        "every law must emit a real universal lemma"
    );
    // The proved support stack.
    for needle in [
        "windowSig_sigWindow__div_lower",
        "windowSig_sigWindow__div_upper",
        "windowSig_sigWindow__div_window",
        "windowSig_sigWindow__exp_window",
        "windowSig_sigWindow__sig_pos",
        "windowSig_sigWindow__sig_neg",
        "widthsAdd_productWindow__pow_add",
    ] {
        assert!(dfy.contains(needle), "missing support lemma {needle}");
    }
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// The soundness boundary of the recursion class: a floor-division
/// self-call whose guard chain does NOT imply the shrinking param is
/// positive must DECLINE (the measure would be wrong at p = 0 —
/// `0 / 2 == 0` does not decrease), keeping the prior honest
/// emissions: Lean `partial def`, Dafny opaque `{:axiom}`, laws
/// omitted. Never guess.
#[test]
fn proof_floor_div_without_positive_guard_declines() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = r#"module FloorLoop
    intent =
        "Floor halving under a guard that does not bound the argument"
        "below: the proof backends must decline the recursion."
    effects []

fn spin(a: Int) -> Int
    ? "Halves until it reaches zero, but the guard admits a == 0 forever."
    match a == 5
        true -> 0
        false -> spin(Result.withDefault(Int.div(a, 2), 0))

verify spin
    spin(5) => 0
    spin(20) => 0
"#;
    let dir = temp_output_dir("aver-proof-floor-window-decline");
    std::fs::create_dir_all(&dir).expect("mkdir");
    let av = dir.join("floor_loop.av");
    std::fs::write(&av, src).expect("write fixture");
    let out_lean = dir.join("lean");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg(&av)
        .arg("-o")
        .arg(&out_lean)
        .output()
        .expect("aver proof should run");
    assert!(run.status.success(), "{}", format_output(&run));
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert!(
        stderr.contains("outside proof subset"),
        "unvalidated guard must decline the floor-division class:\n{stderr}"
    );
    let lean = std::fs::read_to_string(out_lean.join("FloorLoop.lean")).expect("lean out");
    assert!(
        lean.contains("partial def spin"),
        "Lean must keep the honest partial def for the declined shape"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// Live Lean gate: the whole floor-window fixture builds green with
/// ZERO sorries AND earns file-level `universal` credit — every law
/// theorem is stated in true universal form and `#print axioms` stays
/// inside the kernel whitelist (the support stacks use the core ediv
/// bridges + functional induction; no `native_decide`, no Mathlib).
/// Before the fix this file had 1 sorry (`twoPow.positive`) and the
/// when-laws were sampled-domain-bounded — `universal` was false.
#[test]
fn proof_floor_window_lean_closes_kernel_genuine() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping floor-window proof test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-floor-window");
    let (summary, run) = run_lean_check_json("tests/fixtures/floor_window.av", &output_dir, 0, &[]);
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(0),
        "floor-window laws must close sorry-free.\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["passed"].as_bool(),
        Some(true),
        "{}",
        format_output(&run)
    );
    assert_eq!(
        summary["universal"].as_bool(),
        Some(true),
        "all four law theorems are stated universally and must be \
         kernel-genuine (axioms within the whitelist).\n{}",
        format_output(&run)
    );
    assert_eq!(
        (
            summary["universal_laws"].as_u64(),
            summary["bounded_laws"].as_u64(),
        ),
        (Some(4), Some(0)),
        "explicit law counts: exactly the four universal-classed law \
         theorems certified, none degraded to bounded-domain.\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Live Lean gate for the second, cross-domain witness
/// (`tests/fixtures/floor_arith_witness.av`). The export-structure pin
/// (`proof_export_floor_arith_second_witness_universal`) only asserts the
/// `universal` law-class MARKERS, which the Rust classifier stamps when the
/// rung FIRES — not when the Lean kernel accepts the proof. This gate builds
/// the whole fixture with the toolchain and asserts all five witness laws
/// close sorry-free AND earn kernel-genuine `universal` credit (`#print
/// axioms` inside the whitelist, encoded by the `universal` summary flag —
/// same contract as `proof_floor_window_lean_closes_kernel_genuine`). Covers
/// every orientation corner of the cancel / absorb rungs, including the
/// shared factor on the LEFT of the divisor product.
#[test]
fn proof_floor_arith_witness_lean_closes_kernel_genuine() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping floor-arith-witness proof test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-floor-arith-witness-lake");
    let (summary, run) =
        run_lean_check_json("tests/fixtures/floor_arith_witness.av", &output_dir, 0, &[]);
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(0),
        "floor-arith witness laws must close sorry-free.\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["passed"].as_bool(),
        Some(true),
        "{}",
        format_output(&run)
    );
    assert_eq!(
        summary["universal"].as_bool(),
        Some(true),
        "all five witness law theorems are stated universally and must be \
         kernel-genuine (axioms within the whitelist).\n{}",
        format_output(&run)
    );
    assert_eq!(
        (
            summary["universal_laws"].as_u64(),
            summary["bounded_laws"].as_u64(),
        ),
        (Some(5), Some(0)),
        "explicit law counts: exactly the five universal-classed witness \
         theorems certified (every cancel / absorb orientation corner), none \
         degraded to bounded-domain.\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Live Dafny gate: the floor-window fixture verifies end-to-end —
/// 0 errors, 0 `assume {:axiom}` escapes, 0 omitted universals, and
/// `passed` (an exit-status timeout would surface only there).
/// Before the fix: 3 errors (positivity, sum homomorphism and the
/// product window all unprovable from empty bodies) plus the omitted
/// significand law.
/// Deliberately budget-only (no `passed` assert): a genuinely broken
/// support stack surfaces as ERRORS (a false lemma fails its
/// obligation on every platform), which this catches; prover
/// wall-clock/resource use is platform-sensitive — this exact file
/// verifies 156/156 obligations in <= 0.11 s each on macOS while
/// Linux CI's Z3 build times out, with zero errors either way (same
/// policy as the nonlinear-wall fixture and the quicksort ceiling).
/// The platform-independent pin for the feature is the Lean side:
/// kernel-genuine universal credit, asserted in
/// `proof_floor_window_lean_closes_kernel_genuine`.
#[test]
fn proof_floor_window_dafny_verifies() {
    assert_dafny_verifies("tests/fixtures/floor_window.av", "aver-dafny-floor-window");
}

/// The base-10⁹ digit decomposition (`examples/refinement/bigint`)
/// rides the same validated-measure path: its `digitsOf` floor-div
/// recursion now emits WITHOUT the synthesized `requires n >= 0`
/// that poisoned total callers (Dafny) and as a well-founded def
/// instead of a kernel-opaque `partial def` (Lean). Guards the only
/// existing corpus file the new recursion class touches.
///
/// HONEST budget — the `add_commutative__sample_*` family (operands
/// at and above 10⁹ — multi-digit carry chains past Z3's symbolic
/// unfolding appetite) fails IDENTICALLY before and after this change
/// (measured at the same declarations on the baseline export); the
/// floor-division graduation neither adds nor removes those errors.
/// The family is platform-sensitive: 2 fail on macOS, 4 on Linux CI's
/// Z3 build — the ceiling is the Linux count (same rationale as the
/// quicksort budget). A count ABOVE it is a real regression.
#[test]
fn proof_bigint_floor_div_graduation_dafny() {
    assert_dafny_verifies_with_budgets(
        "examples/refinement/bigint/bigint.av",
        "aver-dafny-bigint-floor",
        4,
        0,
    );
}

/// Third, cross-domain WITNESS for the generic Euclidean-floor + power-of-two
/// COMPOSITION rung (`tests/fixtures/floor_compose_witness.av`). The rung that
/// flips the K5 `truncStickyComposes` (truncating a round-to-odd result to a
/// coarser precision) is keyed only on the CLAIM SHAPE — a nested rounding
/// composition whose cone doubles a Euclidean floor (`2 * floor(..)`) — never on
/// the K5 `fpSticky` / `floorDiv` / `pow2` names. This fixture states the SAME
/// shape over differently-named fns and records (`powB`, `qfloor`, a `Ratio`
/// with `sameRatio`, a `Sig` significand, `roundSticky` / `roundTrunc`). If the
/// rung is name-blind, `coarsenComposes` closes `universal` here with ZERO code
/// changes. Export-structure pin (no toolchain): the supporting exact-cancel law
/// is `universal` (the FloorDivWindow figure fired name-blindly on `powB` /
/// `qfloor`) and the composition law block is emitted.
#[test]
fn proof_export_floor_compose_third_witness_structure() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output_dir = temp_output_dir("aver-proof-floor-compose-witness");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("tests/fixtures/floor_compose_witness.av")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("aver proof should run");
    assert!(run.status.success(), "{}", format_output(&run));
    let lean = std::fs::read_to_string(output_dir.join("FloorComposeWitness.lean"))
        .expect("FloorComposeWitness.lean must be emitted");
    // The exact-cancel law pins the power-of-two figure name-blindly on `powB`.
    assert!(
        lean.contains("-- aver:law-class qfloor_law_cancelExact universal"),
        "cancelExact must be classed universal (FloorDivWindow figure is name-blind); got:\n{lean}"
    );
    // The composition law is emitted (its universal flip is speculative, so it is
    // classed here in the non-probe export as bounded — the live gate below is
    // what certifies the flip).
    assert!(
        lean.contains("coarsenComposes_law_coarsens"),
        "the trunc-through-sticky composition law must be emitted"
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Live Lean gate for the third witness: the whole fixture builds with the
/// toolchain and `coarsenComposes` earns kernel-genuine `universal` credit
/// (`#print axioms` inside the whitelist), closed by the SAME generic
/// floor-composition rung as the K5 `truncStickyComposes` — proving the rung is
/// name- and domain-blind (it fires on `powB` / `qfloor` / `roundSticky` with no
/// per-figure change). Sorry-free, both fixture laws universal.
#[test]
fn proof_floor_compose_third_witness_lean_closes_kernel_genuine() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping floor-compose-witness proof test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-floor-compose-witness-lake");
    let (summary, run) = run_lean_check_json(
        "tests/fixtures/floor_compose_witness.av",
        &output_dir,
        0,
        &[],
    );
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(0),
        "witness laws must close sorry-free.\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["passed"].as_bool(),
        Some(true),
        "{}",
        format_output(&run)
    );
    assert_eq!(
        summary["universal"].as_bool(),
        Some(true),
        "the composition law must be kernel-genuine universal (axioms within the whitelist).\n{}",
        format_output(&run)
    );
    assert_eq!(
        (
            summary["universal_laws"].as_u64(),
            summary["bounded_laws"].as_u64(),
        ),
        (Some(2), Some(0)),
        "both witness laws (exact-cancel + trunc-through-sticky composition) certified universal, none bounded.\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

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
        // Comparator-canonicalization witnesses: the strict remainder bound
        // written divisor-first (`den > rest`) and the positivity/nonneg
        // bounds spelled with `>=` must both close universal — the rung
        // canonicalizes the operand order before recognition.
        "quotFloor_law_soakRemainderUpperFlip",
        "quotFloor_law_soakRemainderGeTwin",
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

/// Live Dafny gate for the same cross-domain floor-arithmetic witness as the
/// Lean structure test above. This pins the Dafny templates as shape-keyed:
/// a differently named floor wrapper plus commuted factor/remainder layouts
/// must verify without an axiom escape or sampled-domain fallback.
#[test]
fn proof_floor_arith_witness_dafny_verifies() {
    assert_dafny_verifies(
        "tests/fixtures/floor_arith_witness.av",
        "aver-dafny-floor-arith-witness",
    );
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
/// the whole fixture with the toolchain and asserts all seven witness laws
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
        "all seven witness law theorems are stated universally and must be \
         kernel-genuine (axioms within the whitelist).\n{}",
        format_output(&run)
    );
    assert_eq!(
        (
            summary["universal_laws"].as_u64(),
            summary["bounded_laws"].as_u64(),
        ),
        (Some(7), Some(0)),
        "explicit law counts: exactly the seven universal-classed witness \
         theorems certified (every cancel / absorb orientation corner plus \
         the two comparator-canonicalization spellings), none degraded to \
         bounded-domain.\n{}",
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

/// Divisor-shape positivity (`tests/fixtures/divisor_shape_positivity.av`): the
/// Euclidean-floor arithmetic rungs derive `0 < d` from the DIVISOR'S AST SHAPE
/// instead of a ritual `when 0 < d` guard. Export-structure pin (no toolchain):
/// four floor laws with NO positivity guard close `universal`, each via its own
/// derivation route — a positive literal by `decide`, a pool-fn call by CITING
/// the proven `pow2 law positive`, a product by `Int.mul_pos` over the two, and
/// a CANCEL law whose sole positivity source is that same citation (no remainder
/// bound implies it for free, unlike the absorb arms).
#[test]
fn proof_export_divisor_shape_positivity_universal() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output_dir = temp_output_dir("aver-proof-divisor-shape");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("tests/fixtures/divisor_shape_positivity.av")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("aver proof should run");
    assert!(run.status.success(), "{}", format_output(&run));
    let lean = std::fs::read_to_string(output_dir.join("DivisorShapePositivity.lean"))
        .expect("DivisorShapePositivity.lean must be emitted");

    // Every guard-free floor law closes universal — the positivity was derived
    // from the divisor's shape, not read off a `when` clause.
    for base in [
        "floorDiv_law_absorbLiteral",
        "floorDiv_law_absorbPow2",
        "floorDiv_law_absorbProduct",
        "floorDiv_law_cancelPow2",
    ] {
        assert!(
            lean.contains(&format!("-- aver:law-class {base} universal")),
            "{base} must close universal via shape-derived positivity; got:\n{lean}"
        );
    }
    // The derivation routes each leave their fingerprint in the emitted `0 < d`
    // proof: `decide` for the literal, a citation of the pool positivity theorem
    // (normalized off its Prop `= true` form so `omega` reads it) for the pow2
    // fn, and `Int.mul_pos` over both for the product.
    assert!(
        lean.contains("have hd : 0 < (8 : Int) := by decide"),
        "literal divisor positivity must close by `decide`:\n{lean}"
    );
    assert!(
        lean.contains(
            "have hpos := pow2_law_positive (k); \
             simp only [ge_iff_le, eq_iff_iff, iff_true] at hpos; omega"
        ),
        "pow2 divisor positivity must CITE the pool positivity law and normalize it:\n{lean}"
    );
    assert!(
        lean.contains(
            "Int.mul_pos (by decide) (by have hpos := pow2_law_positive (k); \
             simp only [ge_iff_le, eq_iff_iff, iff_true] at hpos; omega)"
        ),
        "product divisor positivity must be `Int.mul_pos` over the literal and cited factors:\n{lean}"
    );
    // The CANCEL arm's `0 < pow2 k` has NO route but the citation — no remainder
    // bound implies it for free — so this fingerprint is what actually detects a
    // broken citation (the absorb arms would still close from their `when`).
    assert!(
        lean.contains(
            "have hc : 0 < pow2 k := by have hpos := pow2_law_positive (k); \
             simp only [ge_iff_le, eq_iff_iff, iff_true] at hpos; omega"
        ),
        "cancel divisor positivity must be discharged solely by the cited pool law:\n{lean}"
    );
    // No law theorem REACHES sorry (only `first | proof | sorry` fail-safe floors).
    let reached_sorry: Vec<&str> = lean
        .lines()
        .filter(|l| l.contains("sorry") && !l.contains("| sorry"))
        .collect();
    assert!(
        reached_sorry.is_empty(),
        "divisor-shape fixture must not REACH sorry; offending lines:\n{}",
        reached_sorry.join("\n")
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Live Lean gate for the divisor-shape positivity fixture: the pool positivity
/// law plus the four guard-free floor laws (literal / pool-fn / product / cancel
/// divisors) all close sorry-free and earn kernel-genuine `universal` credit
/// (`#print axioms` inside the whitelist). The cancel law is the load-bearing
/// one: its `0 < pow2 k` comes ONLY from the cited pool law (no remainder bound
/// masks a broken citation), so a regression in the citation surfaces here as a
/// lake failure. Deleting the pool law drops the citing laws back to bounded
/// (`proof_divisor_shape_pool_law_is_load_bearing`); here it is present.
#[test]
fn proof_divisor_shape_positivity_lean_closes_kernel_genuine() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping divisor-shape proof test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-divisor-shape-lake");
    let (summary, run) = run_lean_check_json(
        "tests/fixtures/divisor_shape_positivity.av",
        &output_dir,
        0,
        &[],
    );
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(0),
        "divisor-shape laws must close sorry-free.\n{}",
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
        "the pool law and all three shape-derived floor laws must be kernel-genuine \
         universal (axioms within the whitelist).\n{}",
        format_output(&run)
    );
    assert_eq!(
        (
            summary["universal_laws"].as_u64(),
            summary["bounded_laws"].as_u64(),
        ),
        (Some(5), Some(0)),
        "exactly the five universal-classed law theorems certified, none bounded.\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Cross-domain, NAME- and DOMAIN-blind witness
/// (`tests/fixtures/divisor_shape_positivity_witness.av`): a powers-of-THREE
/// recursive fn `blk` and a differently-named floor fn `quot`, with the pool law
/// spelled `1 <= blk(n)`. If the derivation keys on the claim SHAPE and not on
/// `pow2` / `floorDiv` names or the power-of-two domain, the literal and cited
/// floor laws close universal here too. Export-structure pin.
#[test]
fn proof_export_divisor_shape_positivity_witness_universal() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output_dir = temp_output_dir("aver-proof-divisor-shape-witness");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("tests/fixtures/divisor_shape_positivity_witness.av")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("aver proof should run");
    assert!(run.status.success(), "{}", format_output(&run));
    let lean = std::fs::read_to_string(output_dir.join("DivisorShapePositivityWitness.lean"))
        .expect("DivisorShapePositivityWitness.lean must be emitted");

    for base in ["quot_law_soakLiteral", "quot_law_soakBlock"] {
        assert!(
            lean.contains(&format!("-- aver:law-class {base} universal")),
            "{base} must close universal (name- and domain-blind); got:\n{lean}"
        );
    }
    // The cited theorem is the FOREIGN pool law (`blk_law_atLeastOne`), proving
    // the citation is discovered from the claim shape, not a hardcoded `pow2`.
    assert!(
        lean.contains(
            "have hpos := blk_law_atLeastOne (k); \
             simp only [ge_iff_le, eq_iff_iff, iff_true] at hpos; omega"
        ),
        "the foreign pool positivity law must be the cited one:\n{lean}"
    );
    let reached_sorry: Vec<&str> = lean
        .lines()
        .filter(|l| l.contains("sorry") && !l.contains("| sorry"))
        .collect();
    assert!(
        reached_sorry.is_empty(),
        "divisor-shape witness must not REACH sorry; offending lines:\n{}",
        reached_sorry.join("\n")
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Live Lean gate for the cross-domain witness: the foreign `blk` positivity
/// pool law and the two guard-free `quot` floor laws close sorry-free and
/// kernel-genuine universal — a differently named fn, so any `pow2` / `floorDiv`
/// name leak in the derivation would break here.
#[test]
fn proof_divisor_shape_positivity_witness_lean_closes_kernel_genuine() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping divisor-shape witness proof test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-divisor-shape-witness-lake");
    let (summary, run) = run_lean_check_json(
        "tests/fixtures/divisor_shape_positivity_witness.av",
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
        "the foreign pool law and both shape-derived floor laws must be \
         kernel-genuine universal.\n{}",
        format_output(&run)
    );
    assert_eq!(
        (
            summary["universal_laws"].as_u64(),
            summary["bounded_laws"].as_u64(),
        ),
        (Some(3), Some(0)),
        "exactly three universal-classed witness law theorems, none bounded.\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// The fail-closed boundary of divisor-shape positivity: an absorb law whose
/// divisor is a BARE given with no `when 0 < d` guard and no derivable shape
/// (not a literal, product, or pool-fn call) must DECLINE — the rung keeps its
/// sound sampled fallback (`bounded-domain`), not a false universal. The law is
/// genuinely false for `m <= 0`, so bounded is correct. Export pin, no toolchain.
#[test]
fn proof_divisor_shape_underivable_divisor_declines() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = r#"module UnderivableDivisor
    intent =
        "An absorb-remainder floor law whose divisor is an unguarded bare"
        "variable: no shape derives its positivity, so the rung must decline."
    exposes [floorDiv]
    effects []

fn floorDiv(a: Int, d: Int) -> Int
    ? "Euclidean floor division a / d, guarded to 0 at d = 0."
    Result.withDefault(Int.div(a, d), 0)

verify floorDiv
    floorDiv(52, 8) => 6

verify floorDiv law absorbBare
    given m: Int = [1, 2, 3]
    given q: Int = [4, 6, 0]
    given r: Int = [0, 1, 2]
    when 0 <= r
    when r < m
    floorDiv(m * q + r, m) => q
"#;
    let dir = temp_output_dir("aver-proof-divisor-shape-underivable");
    std::fs::create_dir_all(&dir).expect("mkdir");
    let av = dir.join("underivable.av");
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
    let lean = std::fs::read_to_string(out_lean.join("UnderivableDivisor.lean")).expect("lean out");
    // Declines to the sound bounded fallback, NOT a false universal.
    assert!(
        lean.contains("-- aver:law-class floorDiv_law_absorbBare bounded-domain"),
        "an unguarded, shape-underivable divisor must decline to bounded-domain:\n{lean}"
    );
    assert!(
        !lean.contains("-- aver:law-class floorDiv_law_absorbBare universal"),
        "must NOT claim universal for an underivable divisor:\n{lean}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// The cited pool positivity law is LOAD-BEARING for the cancel arm, proven by a
/// two-source revert. Both sources carry the identical guard-free cancel law
/// `floorDiv(a*pow2(k), 8*pow2(k)) = floorDiv(a, 8)`, whose only positivity
/// source is `0 < pow2 k`. WITH `pow2 law positive` in scope the divisor-shape
/// rung derives that by CITING it and the law classes `universal`; DELETE just
/// that one pool law (the fn and the cancel law untouched) and the same law
/// declines to the sound `bounded-domain` sampled fallback. This is the
/// mechanized form of the "delete the pool law" revert — the classification flip
/// is what proves the citation, not a coincidence, carries the universality.
/// Export-structure pin (no toolchain): the class is fixed at emit time.
#[test]
fn proof_divisor_shape_pool_law_is_load_bearing() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");

    // Shared tail: the pow2 fn, a floor fn, and the guard-free cancel law whose
    // shared factor is `pow2(k)`. The `when 0 < k` guard bounds the exponent,
    // NOT the divisor, so `0 < pow2 k` has no route but a citation.
    let tail = r#"
fn pow2(n: Int) -> Int
    match n <= 0
        true -> 1
        false -> 2 * pow2(n - 1)

verify pow2
    pow2(0) => 1
    pow2(3) => 8
POOL_LAW
fn floorDiv(a: Int, d: Int) -> Int
    ? "Euclidean floor division a / d, guarded to 0 at d = 0."
    Result.withDefault(Int.div(a, d), 0)

verify floorDiv
    floorDiv(52, 8) => 6

verify floorDiv law cancelPow2
    given a: Int = [8, 17, 0 - 5]
    given k: Int = [1, 2, 3]
    when 0 < k
    floorDiv(a * pow2(k), 8 * pow2(k)) => floorDiv(a, 8)
"#;
    let header = "module CancelRevert\n    intent = \"cancel revert\"\n    exposes [pow2, floorDiv]\n    effects []\n";
    let pool_law =
        "\nverify pow2 law positive\n    given k: Int = [0, 1, 3]\n    pow2(k) >= 1 holds\n";

    let emit_class = |with_pool: bool| -> String {
        let body = tail.replace("POOL_LAW", if with_pool { pool_law } else { "" });
        let src = format!("{header}{body}");
        let dir = temp_output_dir(if with_pool {
            "aver-proof-cancel-revert-with"
        } else {
            "aver-proof-cancel-revert-without"
        });
        std::fs::create_dir_all(&dir).expect("mkdir");
        let av = dir.join("cancel_revert.av");
        std::fs::write(&av, &src).expect("write fixture");
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
        let lean = std::fs::read_to_string(out_lean.join("CancelRevert.lean")).expect("lean out");
        let _ = std::fs::remove_dir_all(&dir);
        lean.lines()
            .find(|l| l.contains("-- aver:law-class floorDiv_law_cancelPow2 "))
            .unwrap_or_else(|| panic!("no cancel law-class line:\n{lean}"))
            .to_string()
    };

    // WITH the pool law: the citation discharges `0 < pow2 k` → universal.
    let with_pool = emit_class(true);
    assert!(
        with_pool.contains("floorDiv_law_cancelPow2 universal"),
        "with the pool law present the cancel law must class universal; got: {with_pool}"
    );
    // WITHOUT the pool law: nothing derives `0 < pow2 k` → the rung declines to
    // the sound bounded sampled fallback (NOT a false universal).
    let without_pool = emit_class(false);
    assert!(
        without_pool.contains("floorDiv_law_cancelPow2 bounded-domain"),
        "deleting the pool law must drop the cancel law to bounded-domain; got: {without_pool}"
    );
    assert!(
        !without_pool.contains("floorDiv_law_cancelPow2 universal"),
        "deleting the pool law must NOT leave a false universal; got: {without_pool}"
    );
}

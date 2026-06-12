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
    assert!(
        !lean.contains("sorry"),
        "the floor-window fixture must emit sorry-free"
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

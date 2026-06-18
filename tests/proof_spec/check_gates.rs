use super::*;

#[test]
fn proof_dafny_check_verifies_entry_module_not_arbitrary_dependency() {
    // Regression: `--check` must verify the ENTRY module (which carries the
    // verify-law lemmas), not whatever `.dfy` a directory scan yields first.
    // The dependency module here (`Aaa`) sorts before the entry (`Zzz`) and
    // does NOT include it, so a naive `read_dir().find()` verifies `Aaa.dfy`
    // and never checks `Zzz`'s deliberately-false law → false-green.
    if Command::new("dafny").arg("--version").output().is_err() {
        eprintln!("skipping dafny entry-selection test: `dafny` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-mm-entry-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(
        src.join("aaa.av"),
        "module Aaa\n    depends []\n\nfn ident(n: Int) -> Int\n    ? \"id\"\n    n\n\n\
         verify ident law refl\n    given n: Int = -1..1\n    ident(n) => n\n",
    )
    .expect("write aaa.av");
    std::fs::write(
        src.join("zzz.av"),
        "module Zzz\n    depends [Aaa]\n    effects [Console.print]\n\n\
         fn wrong(n: Int) -> Int\n    ? \"doubles; the law lies\"\n    Aaa.ident(n) + n\n\n\
         verify wrong law falseRefl\n    given n: Int = -1..1\n    wrong(n) => n\n\n\
         fn main() -> Unit\n    ! [Console.print]\n    Console.print(\"mm\")\n",
    )
    .expect("write zzz.av");
    let out = temp_output_dir("aver-mm-entry-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("zzz.av"))
        .arg("--backend")
        .arg("dafny")
        .arg("--module-root")
        .arg(&src)
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check --check-json` to run");
    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)));
    let summary: serde_json::Value =
        serde_json::from_str(json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}"));
    assert_eq!(
        summary["passed"].as_bool(),
        Some(false),
        "entry `Zzz`'s false law `wrong(n) => n` must be caught — `--check` must \
         verify the ENTRY module, not an arbitrary dependency.\n{}",
        format_output(&run)
    );
    assert!(
        summary["errors"].as_u64().unwrap_or(0) >= 1,
        "expected >=1 Dafny error from the false entry law\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_check_lean_universal_field_distinguishes_bounded_from_genuine() {
    // The honest-coverage gate behind `--check-json` `universal`. Lean's
    // `passed` is deliberately lenient: a law the auto-prover cannot close by
    // genuine induction still emits a finite domain-guarded `∀ … -> …` proved
    // by `native_decide`, which `lake build` accepts (passed:true, 0 sorries) —
    // a legitimate-but-weaker bounded verify-on-domain. That bounded proof
    // depends on `Lean.ofReduceBool` (the kernel trusting the compiler's
    // evaluation over the concrete domain), NOT the universal claim, so
    // `#print axioms` exposes it. `universal` must report `false` there while
    // `passed` stays `true` — the exact split the field exists for. prop_85
    // (zip/rev over a bounded sample domain) is the committed corpus instance.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lean universal-field test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let out = temp_output_dir("aver-universal-bounded-out");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("proof-corpus/tip/isaplanner/prop_85.av")
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check --check-json` to run");
    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)));
    let summary: serde_json::Value =
        serde_json::from_str(json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}"));
    assert_eq!(
        (summary["passed"].as_bool(), summary["universal"].as_bool()),
        (Some(true), Some(false)),
        "a bounded `native_decide` proof must stay lenient on `passed` but report \
         `universal:false` (it depends on `Lean.ofReduceBool`, not the ∀-claim). \
         If `universal` flipped to true, prop_85 now closes genuinely — celebrate \
         and re-baseline this test.\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_check_dafny_rejects_sample_only_universal_as_unproven() {
    // Soundness: when the emitter cannot state a law's universal `∀`-claim it
    // drops it to concrete samples plus a `… (universal lemma omitted)`
    // comment. Dafny then finishes with 0 errors / exit 0 because the
    // universal was never asserted — a false-green the errors-only and
    // axiom-only gates both miss. `--check` must charge an omitted universal
    // against the sorry budget (like `assume {:axiom}`) so it reports
    // `passed:false`. The `fac = qfac · one` accumulator-equivalence is a
    // stable instance: both fns verify cleanly (errors:0) but the universal
    // needs an IH generalization the emitter does not do, so it is omitted.
    if Command::new("dafny").arg("--version").output().is_err() {
        eprintln!("skipping dafny omitted-universal soundness test: `dafny` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-omit-sound-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(
        src.join("omit.av"),
        "module Omit\n    effects []\n\n\
         type Nat\n    Z\n    S(Nat)\n\n\
         fn plus(x: Nat, y: Nat) -> Nat\n    match x\n        Nat.Z -> y\n        Nat.S(z) -> Nat.S(plus(z, y))\n\n\
         fn mult(x: Nat, y: Nat) -> Nat\n    match x\n        Nat.Z -> Nat.Z\n        Nat.S(z) -> plus(y, mult(z, y))\n\n\
         fn fac(x: Nat) -> Nat\n    match x\n        Nat.Z -> Nat.S(Nat.Z)\n        Nat.S(y) -> mult(x, fac(y))\n\n\
         fn qfac(x: Nat, y: Nat) -> Nat\n    match x\n        Nat.Z -> y\n        Nat.S(z) -> qfac(z, mult(x, y))\n\n\
         verify fac law facQfac\n    given x: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    fac(x) => qfac(x, Nat.S(Nat.Z))\n",
    )
    .expect("write omit.av");
    let out = temp_output_dir("aver-omit-sound-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("omit.av"))
        .arg("--backend")
        .arg("dafny")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check --check-json` to run");
    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)));
    let summary: serde_json::Value =
        serde_json::from_str(json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}"));
    // errors:0 confirms the ONLY reason for failure is the dropped universal,
    // so this exercises the omitted-gate specifically.
    assert_eq!(
        summary["errors"].as_u64(),
        Some(0),
        "expected a clean verify (errors:0); the omitted-universal gate, not \
         a Dafny error, must drive the failure.\n{}",
        format_output(&run)
    );
    assert!(
        summary["omitted"].as_u64().unwrap_or(0) >= 1,
        "expected the `facQfac` universal to be dropped to sample-only \
         (omitted >= 1).\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["passed"].as_bool(),
        Some(false),
        "a sample-only law whose universal was omitted must NOT pass --check \
         — dropping the ∀-claim is the Dafny analog of a sorry.\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_dependency_law_verify_is_carried_not_warned() {
    // A `verify ... law` in a dependency module is now CARRIED by the
    // cross-file law pool (emitted as a theorem in the dep's export, and
    // offered to consumer proofs under the cone ∪ subject gate), so it is
    // no longer dropped — the old "NOT checked" warning must NOT fire for
    // a dependency LAW block. Pure codegen, no verifier binary needed.
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-dep-law-carried-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(
        src.join("dep.av"),
        "module Dep\n    depends []\n\nfn ident(n: Int) -> Int\n    ? \"id\"\n    n\n\n\
         verify ident law refl\n    given n: Int = -1..1\n    ident(n) => n\n",
    )
    .expect("write dep.av");
    std::fs::write(
        src.join("app.av"),
        "module App\n    depends [Dep]\n    effects [Console.print]\n\n\
         fn wrap(n: Int) -> Int\n    ? \"w\"\n    Dep.ident(n)\n\n\
         fn main() -> Unit\n    ! [Console.print]\n    Console.print(\"x\")\n",
    )
    .expect("write app.av");
    let out = temp_output_dir("aver-dep-law-carried-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("app.av"))
        .arg("--backend")
        .arg("dafny")
        .arg("--module-root")
        .arg(&src)
        .arg("-o")
        .arg(&out)
        .output()
        .expect("expected `aver proof` to run");
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert!(
        !stderr.contains("NOT checked"),
        "a dependency `verify ... law` is now carried by the cross-file law \
         pool, not dropped — the old 'NOT checked' warning must not fire:\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_warns_when_dependency_module_has_nonlaw_verify_block() {
    // A NON-law (example / cases-form) `verify` block in a dependency
    // module is still NOT carried — module-scoped sampling is a separate
    // feature — so it would never run. The compiler must still warn loudly
    // for those, so an unchecked dependency sample isn't mistaken for a
    // proven one. Pure codegen, no verifier binary needed.
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-dep-verify-warn-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(
        src.join("dep.av"),
        "module Dep\n    depends []\n\nfn ident(n: Int) -> Int\n    ? \"id\"\n    n\n\n\
         verify ident\n    ident(1) => 1\n",
    )
    .expect("write dep.av");
    std::fs::write(
        src.join("app.av"),
        "module App\n    depends [Dep]\n    effects [Console.print]\n\n\
         fn wrap(n: Int) -> Int\n    ? \"w\"\n    Dep.ident(n)\n\n\
         fn main() -> Unit\n    ! [Console.print]\n    Console.print(\"x\")\n",
    )
    .expect("write app.av");
    let out = temp_output_dir("aver-dep-verify-warn-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("app.av"))
        .arg("--backend")
        .arg("dafny")
        .arg("--module-root")
        .arg(&src)
        .arg("-o")
        .arg(&out)
        .output()
        .expect("expected `aver proof` to run");
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert!(
        stderr.contains("verify block") && stderr.contains("Dep") && stderr.contains("NOT checked"),
        "expected a warning that dependency module `Dep`'s non-law verify block \
         is unchecked, got:\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_dafny_warns_example_cases_not_checked() {
    // Dafny proves LAWS, not concrete example-cases — it cannot evaluate
    // a `f(x) => y` case the way Lean's `native_decide` does. It must say
    // so rather than silently pass case-form verify. Pure codegen, no
    // verifier binary needed. `sum_acc.av` carries case-form verify blocks.
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let out = temp_output_dir("aver-dafny-case-warn");
    let run = Command::new(aver_bin)
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .arg("proof")
        .arg("examples/data/sum_acc.av")
        .arg("--backend")
        .arg("dafny")
        .arg("-o")
        .arg(&out)
        .output()
        .expect("expected `aver proof` to run");
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert!(
        stderr.contains("example-based") && stderr.contains("NOT") && stderr.contains("Dafny"),
        "expected a warning that example-based verify is not Dafny-checked, got:\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_lean_vacuous_when_premise_law_builds_and_passes() {
    // A `when` premise that is unsatisfiable (here a nested Bool `match`
    // requiring `n > 0` AND `n < 0`) makes the law vacuously true, so a
    // sound prover must ACCEPT it. The premise lowers to a multi-line
    // `if/then/else`; previously the emit was unparseable Lean (the
    // unparenthesized `if` swallowed the trailing `= true`, and the
    // `-- when` comment leaked its continuation lines), and even parsed
    // `simp only` left the Bool premise opaque so `omega` failed — a
    // valid law wrongly REJECTED (false-RED). Pins parens + single-line
    // comment + `simp_all` so it builds and passes.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lean vacuous-when test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-lean-vacuous-when-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    let av = src.join("vac.av");
    std::fs::write(
        &av,
        "module VacuousLaw\n\nfn dbl(n: Int) -> Int\n    ? \"double\"\n    n + n\n\n\
         verify dbl law vac\n    given n: Int = -2..2\n    when match n > 0\n\
         \x20       true -> match n < 0\n            true -> true\n            false -> false\n\
         \x20       false -> false\n    dbl(n) => n + 999\n",
    )
    .expect("write vac.av");
    let out = temp_output_dir("aver-lean-vacuous-when-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(&av)
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check` to run");
    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)));
    let summary: serde_json::Value =
        serde_json::from_str(json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}"));
    assert_eq!(
        summary["passed"].as_bool(),
        Some(true),
        "vacuously-true `when`-premised law must build and pass on Lean\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_lean_bounded_when_law_proof_is_not_credited_universal() {
    // The false-credit probe for the `universal` metric. A `when`-law over a
    // non-refinement-lifted Int given is emitted with sampled-domain
    // disjunction premises prepended (`a = 0 ∨ a = 1 ∨ … ->`), so its
    // theorem is BOUNDED — it claims the law only on the finite sample
    // domain. This exact shape is one the LinearArithmetic `h_when` path
    // proves with real tactics (`intro a h_a h_when; simp_all`-style), so the
    // proof is axiom-clean: before the statement-class channel
    // (`-- aver:law-class`, emitted by `law_theorem_prop`'s caller and
    // consumed by `lean_universal_proof`), the file FALSELY flipped
    // `universal: true`. The honest summary is: passed (the bounded claim IS
    // proven), zero sorries, but NO universal credit. Reverting only the
    // classification change (emitter marker + checker consumption) makes
    // this test fail with `universal: true` — the false credit.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lean bounded-when universal-credit test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-lean-bounded-when-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    let av = src.join("clamp.av");
    std::fs::write(
        &av,
        "module ClampFloor\n\n\
         fn clampFloor(a: Int) -> Int\n    ? \"Clamps negative values to zero.\"\n\
         \x20   match a >= 0\n        true -> a\n        false -> 0\n\n\
         verify clampFloor law identityOnNonNegative\n\
         \x20   given a: Int = [0, 1, 7, 42]\n\
         \x20   when a >= 0\n\
         \x20   clampFloor(a) => a\n",
    )
    .expect("write clamp.av");
    let out = temp_output_dir("aver-lean-bounded-when-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(&av)
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check` to run");
    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)));
    let summary: serde_json::Value =
        serde_json::from_str(json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}"));
    assert_eq!(
        summary["passed"].as_bool(),
        Some(true),
        "the bounded `when`-law claim itself must still prove and pass\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(0),
        "the bounded `when`-law proof must be sorry-free\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["universal"].as_bool(),
        Some(false),
        "a bounded-statement law proof must NOT be credited universal\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_check_dafny_declines_unrecognized_recursion_instead_of_guessing() {
    // Recursion outside every recognized `decreases` pattern — the
    // doubling/halving exponent walk on a rational num/den pair
    // (tests/fixtures/expo_outside_subset.av). The emitter used to
    // GUESS `decreases num` + synthesize `requires num >= 0` on the
    // first Int param: two "decreases clause might not decrease"
    // errors on a correct function plus a "function precondition could
    // not be proved" error at every total caller. The honest export
    // declines: the fn emits as an opaque `function {:axiom}`, callers
    // stay wellformed, sample asserts are suppressed (nothing about an
    // opaque value is provable), and the law is charged as omitted —
    // 0 errors, 0 axiom-attributed lemmas, 1 omitted universal.
    if Command::new("dafny").arg("--version").output().is_err() {
        eprintln!("skipping dafny unrecognized-recursion decline test: `dafny` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let out = temp_output_dir("aver-expo-outside-subset-out");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("tests/fixtures/expo_outside_subset.av")
        .arg("--backend")
        .arg("dafny")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check --check-json` to run");
    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)));
    let summary: serde_json::Value =
        serde_json::from_str(json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}"));
    assert_eq!(
        (
            summary["errors"].as_u64(),
            summary["axioms"].as_u64(),
            summary["omitted"].as_u64(),
        ),
        (Some(0), Some(0), Some(1)),
        "unrecognized recursion must decline to an omitted law, never error \
         on a guessed measure\n{}",
        format_output(&run)
    );
    let dfy = std::fs::read_to_string(out.join("ExpoOutsideSubset.dfy"))
        .expect("read emitted ExpoOutsideSubset.dfy");
    assert!(
        dfy.contains("function {:axiom} expo("),
        "expo must emit as an opaque axiom declaration; got:\n{dfy}"
    );
    assert!(
        !dfy.contains("requires num >= 0"),
        "no synthesized precondition may poison expo's callers; got:\n{dfy}"
    );
    assert!(
        dfy.contains("// Sample assertions for expo.upperBound omitted"),
        "samples over the opaque fn must be suppressed with a marker; got:\n{dfy}"
    );
    assert!(
        dfy.contains("decreases if j >= 0 then j else 0"),
        "the recognized countdown pattern (pow2) must keep its real decreases; got:\n{dfy}"
    );
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_check_lean_chunked_checked_domain_builds_and_keeps_universal_credit() {
    // A 512-cell given-domain product (tests/fixtures/large_domain_law
    // .av) used to emit ONE 512-conjunct `_checked_domain` theorem:
    // `Decidable`-instance synthesis recurses once per nested ∧, blows
    // the elaborator's `maxRecDepth`, and the whole file fails to build
    // — the caught-sorry floor dies for every law in the file. The
    // emitter now chunks past the 36-conjunct edge into
    // `_checked_domain_part<N>` theorems. Both halves matter here:
    // `passed` proves the file builds again, `universal` proves the
    // part-theorems are excluded from the `#print axioms` audit (they
    // are `native_decide` cross-checks; counting them would strip the
    // genuinely-closed universal of its credit).
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lean chunked-domain test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let out = temp_output_dir("aver-large-domain-out");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("tests/fixtures/large_domain_law.av")
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check --check-json` to run");
    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)));
    let summary: serde_json::Value =
        serde_json::from_str(json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}"));
    assert_eq!(
        (
            summary["passed"].as_bool(),
            summary["sorries"].as_u64(),
            summary["universal"].as_bool(),
        ),
        (Some(true), Some(0), Some(true)),
        "the chunked large-domain export must build with zero sorries and keep \
         universal credit on the main law theorem\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&out);
}

// ---- THE RATCHET: end-to-end `aver proof --gate` (live lake) ----
//
// The pure comparator (`gate_manifest`) is unit-tested on fixtures in
// `src/main/commands.rs`. These tests drive the FULL pipeline — emit Lean,
// `lake build`, recompute the per-law manifest, diff against a committed
// baseline — on the scout's `cell_floor_grid` task. The tier-PRODUCING half
// needs live `lake` (the `#print axioms` probe is the ground truth), so each
// test is guarded by the standard `lake --version` skip.

/// The two-law base task (scout `base.av`): both laws prove UNIVERSALLY via
/// the when-universal floor lane.
const RATCHET_BASE_AV: &str = "module CellFloorGrid\n\
    \x20   intent =\n\
    \x20       \"Rounding-grid proof task.\"\n\
    \x20   effects []\n\n\
    fn floorQ(a: Int, b: Int) -> Int\n\
    \x20   ? \"Floor of a/b for b > 0.\"\n\
    \x20   match Int.div(a, b)\n\
    \x20       Result.Ok(q) -> q\n\
    \x20       Result.Err(msg) -> 0\n\n\
    verify floorQ law cellFloorStable\n\
    \x20   given a: Int = [13, 15]\n\
    \x20   given b: Int = [4]\n\
    \x20   given w: Int = [3]\n\
    \x20   given d: Int = [2]\n\
    \x20   when Bool.and(b > 0, Bool.and(d > 0, Bool.and(w * b < a, a < (w + 1) * b)))\n\
    \x20   floorQ(a, b * d) == floorQ(w, d) => true\n\n\
    fn coarseFloorEq(an: Int, ad: Int, bn: Int, bd: Int, d: Int) -> Bool\n\
    \x20   ? \"Both rationals floored on the coarser grid agree.\"\n\
    \x20   floorQ(an, ad * d) == floorQ(bn, bd * d)\n\n\
    verify coarseFloorEq law sharedCellFloor\n\
    \x20   given an: Int = [13, 15]\n\
    \x20   given ad: Int = [4]\n\
    \x20   given bn: Int = [25, 30]\n\
    \x20   given bd: Int = [8]\n\
    \x20   given w: Int = [3]\n\
    \x20   given d: Int = [2]\n\
    \x20   when Bool.and(ad > 0, Bool.and(bd > 0, Bool.and(d > 0, Bool.and(w * ad < an, Bool.and(an < (w + 1) * ad, Bool.and(w * bd < bn, bn < (w + 1) * bd))))))\n\
    \x20   coarseFloorEq(an, ad, bn, bd, d) => true\n";

/// `base.av` with `sharedCellFloor` removed (scout `deleted.av`).
const RATCHET_DELETED_AV: &str = "module CellFloorGrid\n\
    \x20   intent =\n\
    \x20       \"Rounding-grid proof task.\"\n\
    \x20   effects []\n\n\
    fn floorQ(a: Int, b: Int) -> Int\n\
    \x20   ? \"Floor of a/b for b > 0.\"\n\
    \x20   match Int.div(a, b)\n\
    \x20       Result.Ok(q) -> q\n\
    \x20       Result.Err(msg) -> 0\n\n\
    verify floorQ law cellFloorStable\n\
    \x20   given a: Int = [13, 15]\n\
    \x20   given b: Int = [4]\n\
    \x20   given w: Int = [3]\n\
    \x20   given d: Int = [2]\n\
    \x20   when Bool.and(b > 0, Bool.and(d > 0, Bool.and(w * b < a, a < (w + 1) * b)))\n\
    \x20   floorQ(a, b * d) == floorQ(w, d) => true\n\n\
    fn coarseFloorEq(an: Int, ad: Int, bn: Int, bd: Int, d: Int) -> Bool\n\
    \x20   ? \"Both rationals floored on the coarser grid agree.\"\n\
    \x20   floorQ(an, ad * d) == floorQ(bn, bd * d)\n";

/// `base.av` with `cellFloorStable`'s LHS weakened so it leaves the universal
/// lane — both laws fall out of universal crediting and drop to bounded
/// (scout `weakened.av`). This is the SUBTLE soundness case: a silent
/// universal -> bounded slide the count-based gate keeps green.
const RATCHET_WEAKENED_AV: &str = "module CellFloorGrid\n\
    \x20   intent =\n\
    \x20       \"Rounding-grid proof task.\"\n\
    \x20   effects []\n\n\
    fn floorQ(a: Int, b: Int) -> Int\n\
    \x20   ? \"Floor of a/b for b > 0.\"\n\
    \x20   match Int.div(a, b)\n\
    \x20       Result.Ok(q) -> q\n\
    \x20       Result.Err(msg) -> 0\n\n\
    verify floorQ law cellFloorStable\n\
    \x20   given a: Int = [13, 15]\n\
    \x20   given b: Int = [4]\n\
    \x20   given w: Int = [3]\n\
    \x20   given d: Int = [2]\n\
    \x20   when Bool.and(b > 0, Bool.and(d > 0, Bool.and(w * b < a, a < (w + 1) * b)))\n\
    \x20   floorQ(floorQ(a, b), d) == floorQ(w, d) => true\n\n\
    fn coarseFloorEq(an: Int, ad: Int, bn: Int, bd: Int, d: Int) -> Bool\n\
    \x20   ? \"Both rationals floored on the coarser grid agree.\"\n\
    \x20   floorQ(an, ad * d) == floorQ(bn, bd * d)\n\n\
    verify coarseFloorEq law sharedCellFloor\n\
    \x20   given an: Int = [13, 15]\n\
    \x20   given ad: Int = [4]\n\
    \x20   given bn: Int = [25, 30]\n\
    \x20   given bd: Int = [8]\n\
    \x20   given w: Int = [3]\n\
    \x20   given d: Int = [2]\n\
    \x20   when Bool.and(ad > 0, Bool.and(bd > 0, Bool.and(d > 0, Bool.and(w * ad < an, Bool.and(an < (w + 1) * ad, Bool.and(w * bd < bn, bn < (w + 1) * bd))))))\n\
    \x20   coarseFloorEq(an, ad, bn, bd, d) => true\n";

/// Two distinct `verify floorQ law cellFloorStable` blocks sharing ONE
/// `fn.law` identity — the MAJOR 3 collision. The manifest keys on `fn.law`,
/// so without a guard the second block would collapse onto the first
/// (strongest-tier-wins), hiding the weaker (second) duplicate. The ratchet
/// must fail CLOSED (exit 2). The second block's claim is the weakened one.
const RATCHET_DUP_LAW_AV: &str = "module CellFloorGrid\n\
    \x20   intent =\n\
    \x20       \"Rounding-grid proof task.\"\n\
    \x20   effects []\n\n\
    fn floorQ(a: Int, b: Int) -> Int\n\
    \x20   ? \"Floor of a/b for b > 0.\"\n\
    \x20   match Int.div(a, b)\n\
    \x20       Result.Ok(q) -> q\n\
    \x20       Result.Err(msg) -> 0\n\n\
    verify floorQ law cellFloorStable\n\
    \x20   given a: Int = [13, 15]\n\
    \x20   given b: Int = [4]\n\
    \x20   given w: Int = [3]\n\
    \x20   given d: Int = [2]\n\
    \x20   when Bool.and(b > 0, Bool.and(d > 0, Bool.and(w * b < a, a < (w + 1) * b)))\n\
    \x20   floorQ(a, b * d) == floorQ(w, d) => true\n\n\
    verify floorQ law cellFloorStable\n\
    \x20   given a: Int = [13, 15]\n\
    \x20   given b: Int = [4]\n\
    \x20   given w: Int = [3]\n\
    \x20   given d: Int = [2]\n\
    \x20   when Bool.and(b > 0, Bool.and(d > 0, Bool.and(w * b < a, a < (w + 1) * b)))\n\
    \x20   floorQ(floorQ(a, b), d) == floorQ(w, d) => true\n";

/// Write `src` to a fresh temp `.av`, run `aver proof --backend lean` with the
/// given trailing args, and return `(exit_code, stderr)`. Exit code is read
/// directly off the `Output` status — never through a pipe.
fn ratchet_run(tag: &str, src: &str, extra: &[&std::ffi::OsStr]) -> (i32, String) {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let dir = temp_output_dir(&format!("aver-ratchet-{tag}-src"));
    std::fs::create_dir_all(&dir).expect("create src dir");
    let av = dir.join("grid.av");
    std::fs::write(&av, src).expect("write grid.av");
    let out = temp_output_dir(&format!("aver-ratchet-{tag}-out"));
    let mut cmd = Command::new(aver_bin);
    cmd.arg("proof")
        .arg(&av)
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out);
    cmd.args(extra);
    let run = cmd.output().expect("expected `aver proof` to run");
    let code = run.status.code().unwrap_or(-1);
    let stderr = format_output(&run);
    let _ = std::fs::remove_dir_all(&dir);
    let _ = std::fs::remove_dir_all(&out);
    (code, stderr)
}

#[test]
fn ratchet_gate_catches_deleted_law() {
    // (a) A previously-proven law removed entirely. The count-based gate stays
    // green (the count just drops); the ratchet FAILS and names the law.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping ratchet delete test: `lake` not available");
        return;
    }
    let baseline = temp_output_dir("aver-ratchet-del-baseline").with_extension("json");
    let (code, _) = ratchet_run(
        "del-base",
        RATCHET_BASE_AV,
        &[
            "--check".as_ref(),
            "--write-baseline".as_ref(),
            baseline.as_os_str(),
        ],
    );
    assert_eq!(
        code, 0,
        "writing the baseline from the 2-law base must succeed"
    );

    let (code, stderr) = ratchet_run(
        "del-gate",
        RATCHET_DELETED_AV,
        &["--check".as_ref(), "--gate".as_ref(), baseline.as_os_str()],
    );
    assert_eq!(
        code, 1,
        "deleting a proven law must FAIL the gate (exit 1)\n{stderr}"
    );
    assert!(
        stderr.contains("coarseFloorEq.sharedCellFloor") && stderr.contains("MISSING"),
        "the gate must name the deleted law as MISSING\n{stderr}"
    );
    let _ = std::fs::remove_file(&baseline);
}

#[test]
fn ratchet_gate_catches_demoted_law() {
    // (b) THE SOUNDNESS CASE: a law silently slides universal -> bounded. Both
    // laws fall out of the universal lane; `passed`/`sorries`/exit stay green
    // under the old count gate. The ratchet must FAIL and name the tier change.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping ratchet demote test: `lake` not available");
        return;
    }
    let baseline = temp_output_dir("aver-ratchet-demote-baseline").with_extension("json");
    let (code, _) = ratchet_run(
        "demote-base",
        RATCHET_BASE_AV,
        &[
            "--check".as_ref(),
            "--write-baseline".as_ref(),
            baseline.as_os_str(),
        ],
    );
    assert_eq!(code, 0, "writing the baseline must succeed");

    let (code, stderr) = ratchet_run(
        "demote-gate",
        RATCHET_WEAKENED_AV,
        &["--check".as_ref(), "--gate".as_ref(), baseline.as_os_str()],
    );
    assert_eq!(
        code, 1,
        "a universal -> bounded demotion must FAIL the gate (exit 1)\n{stderr}"
    );
    assert!(
        stderr.contains("floorQ.cellFloorStable") && stderr.contains("tier universal -> "),
        "the gate must name the demoted law with its before/after tier\n{stderr}"
    );
    let _ = std::fs::remove_file(&baseline);
}

#[test]
fn ratchet_regenerated_baseline_is_green() {
    // (c) The ack path: after a legitimate change, `--write-baseline` regenerates
    // the baseline; gating the changed file against the NEW baseline is GREEN.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping ratchet regenerate test: `lake` not available");
        return;
    }
    let baseline = temp_output_dir("aver-ratchet-regen-baseline").with_extension("json");
    // Regenerate the baseline FROM the weakened (changed) file — the human ack.
    let (code, _) = ratchet_run(
        "regen-write",
        RATCHET_WEAKENED_AV,
        &[
            "--check".as_ref(),
            "--write-baseline".as_ref(),
            baseline.as_os_str(),
        ],
    );
    assert_eq!(code, 0, "regenerating the baseline must succeed");

    let (code, stderr) = ratchet_run(
        "regen-gate",
        RATCHET_WEAKENED_AV,
        &["--check".as_ref(), "--gate".as_ref(), baseline.as_os_str()],
    );
    assert_eq!(
        code, 0,
        "gating against a freshly regenerated baseline must be GREEN (exit 0)\n{stderr}"
    );
    let _ = std::fs::remove_file(&baseline);
}

#[test]
fn ratchet_added_law_is_green() {
    // (d) Adding a law is allowed. Baseline from the 1-law deleted file; gating
    // the 2-law base file (which ADDS `sharedCellFloor`) against it is GREEN —
    // the new law is reported INFO, not a regression.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping ratchet add test: `lake` not available");
        return;
    }
    let baseline = temp_output_dir("aver-ratchet-add-baseline").with_extension("json");
    let (code, _) = ratchet_run(
        "add-base",
        RATCHET_DELETED_AV,
        &[
            "--check".as_ref(),
            "--write-baseline".as_ref(),
            baseline.as_os_str(),
        ],
    );
    assert_eq!(code, 0, "writing the 1-law baseline must succeed");

    let (code, stderr) = ratchet_run(
        "add-gate",
        RATCHET_BASE_AV,
        &["--check".as_ref(), "--gate".as_ref(), baseline.as_os_str()],
    );
    assert_eq!(
        code, 0,
        "adding a new proven law must be GREEN (exit 0)\n{stderr}"
    );
    let _ = std::fs::remove_file(&baseline);
}

#[test]
fn ratchet_duplicate_law_identity_fails_closed() {
    // MAJOR 3: two `verify ... law` blocks share one `fn.law` identity. The
    // manifest keys on `fn.law`, so they would otherwise collapse to one entry
    // (strongest-tier-wins) and hide the weakened duplicate. The ratchet must
    // fail CLOSED — a harness error (exit 2), naming the collision — rather
    // than silently merge.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping ratchet duplicate-law test: `lake` not available");
        return;
    }
    let baseline = temp_output_dir("aver-ratchet-dup-baseline").with_extension("json");
    let (code, stderr) = ratchet_run(
        "dup-write",
        RATCHET_DUP_LAW_AV,
        &[
            "--check".as_ref(),
            "--write-baseline".as_ref(),
            baseline.as_os_str(),
        ],
    );
    assert_eq!(
        code, 2,
        "a duplicate law identity must be a harness error (exit 2)\n{stderr}"
    );
    assert!(
        stderr.contains("floorQ.cellFloorStable") && stderr.contains("duplicate law identity"),
        "the harness error must name the colliding `fn.law` identity\n{stderr}"
    );
    let _ = std::fs::remove_file(&baseline);
}

#[test]
fn ratchet_corrupt_baseline_fails_closed() {
    // MAJOR 1: a corrupt/truncated baseline (here a per-law record with an
    // unknown tier) must FAIL CLOSED — a harness error (exit 2), never a silent
    // skip that un-ratchets the elided law. Write a real baseline, then corrupt
    // one record's tier, then gate: the gate must refuse it.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping ratchet corrupt-baseline test: `lake` not available");
        return;
    }
    let baseline = temp_output_dir("aver-ratchet-corrupt-baseline").with_extension("json");
    let (code, _) = ratchet_run(
        "corrupt-write",
        RATCHET_BASE_AV,
        &[
            "--check".as_ref(),
            "--write-baseline".as_ref(),
            baseline.as_os_str(),
        ],
    );
    assert_eq!(code, 0, "writing the baseline must succeed");

    // Corrupt one record's tier to an unknown value.
    let raw = std::fs::read_to_string(&baseline).expect("read baseline");
    let corrupted = raw.replacen("\"universal\"", "\"quantum\"", 1);
    assert_ne!(
        raw, corrupted,
        "the baseline must contain a tier to corrupt"
    );
    std::fs::write(&baseline, &corrupted).expect("write corrupted baseline");

    let (code, stderr) = ratchet_run(
        "corrupt-gate",
        RATCHET_BASE_AV,
        &["--check".as_ref(), "--gate".as_ref(), baseline.as_os_str()],
    );
    assert_eq!(
        code, 2,
        "a corrupt baseline (unknown tier) must be a harness error (exit 2)\n{stderr}"
    );
    assert!(
        stderr.contains("not a valid proof manifest") && stderr.contains("quantum"),
        "the harness error must explain the corruption\n{stderr}"
    );
    let _ = std::fs::remove_file(&baseline);
}

/// Run `aver proof <corpus_av> --backend lean --check --check-json [--explain]`
/// and return the parsed check-json summary object. Lake-gated (skips if absent).
fn run_check_json_for(out_tag: &str, corpus_av: &str, explain: bool) -> Option<serde_json::Value> {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping {out_tag}: `lake` not available");
        return None;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let out = temp_output_dir(out_tag);
    let mut cmd = Command::new(aver_bin);
    cmd.current_dir(&repo_root)
        .arg("proof")
        .arg(corpus_av)
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json");
    if explain {
        cmd.arg("--explain");
    }
    let run = cmd.output().expect("expected `aver proof --check` to run");
    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)))
        .to_string();
    let _ = std::fs::remove_dir_all(&out);
    Some(
        serde_json::from_str(&json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}")),
    )
}

#[test]
fn proof_check_explain_surfaces_open_goal_residual() {
    // `--explain` enabler for an agent proposer / "Lemma Calculation": for an
    // OPEN inductive law (prop_53 count-over-sort, which the auto-prover
    // sorry-floors) the check-json must carry a per-`fn.law` `open_goals` entry whose text is
    // the law's UNSOLVED GOAL with the inductive hypothesis (`ih`) in canonical
    // recursive form — exactly what an agent applies the IH against. The counted
    // verdict is unchanged (still not universal, still a sorry).
    let Some(summary) = run_check_json_for(
        "aver-explain-open-out",
        "proof-corpus/tip/isaplanner/prop_53.av",
        true,
    ) else {
        return;
    };
    let goals = summary
        .get("open_goals")
        .and_then(|g| g.as_object())
        .unwrap_or_else(|| panic!("--explain must emit an `open_goals` object:\n{summary}"));
    let residual = goals
        .get("count.countSort")
        .and_then(|v| v.as_str())
        .unwrap_or_else(|| panic!("open_goals must be keyed by `fn.law` identity:\n{summary}"));
    assert!(
        !residual.is_empty(),
        "the residual text must be non-empty:\n{residual}"
    );
    assert!(
        residual.contains("ih :") && residual.contains("count n (sort tail)"),
        "the residual must carry the IH in canonical recursive form (the Lemma \
         Calculation input):\n{residual}"
    );
}

#[test]
fn proof_check_without_explain_emits_no_open_goals_key() {
    // The no-op invariant: WITHOUT `--explain`, the same OPEN task's check-json
    // must contain NO `open_goals` key at all — byte-shape unchanged for existing
    // substring consumers (proof-corpus/run.sh, the --gate baseline diff).
    let Some(summary) = run_check_json_for(
        "aver-explain-noop-out",
        "proof-corpus/tip/isaplanner/prop_53.av",
        false,
    ) else {
        return;
    };
    assert!(
        summary.get("open_goals").is_none(),
        "without --explain there must be no `open_goals` key:\n{summary}"
    );
    // And every counted field that exists without --explain must match what it
    // reports WITH --explain (only the additive `open_goals` key may appear).
    let Some(with_explain) = run_check_json_for(
        "aver-explain-cmp-out",
        "proof-corpus/tip/isaplanner/prop_53.av",
        true,
    ) else {
        return;
    };
    for (k, v) in summary.as_object().unwrap() {
        assert_eq!(
            with_explain.get(k),
            Some(v),
            "field `{k}` must be identical with/without --explain"
        );
    }
}

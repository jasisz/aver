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
fn proof_warns_when_dependency_module_has_verify_blocks() {
    // A `verify ... law` in a dependency module is silently dropped
    // (module-scoped verify is unsupported), so it would never fail — a
    // vacuous pass. The compiler must warn loudly. Pure codegen, no
    // verifier binary needed.
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-dep-verify-warn-src");
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
        "expected a warning that dependency module `Dep`'s verify blocks are unchecked, got:\n{}",
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

use super::*;

#[test]
fn proof_lean_peano_lift_nat_arith_kernel_clean() {
    // Proof-only Peano representation lift: a canonical `type Nat { Z; S(Nat) }`
    // is emitted as Lean's builtin `Nat` (no `inductive`, `Z`→`0`, `S(x)`→`x+1`,
    // structural recursion not fuel), so `omega`/`simp` close the nat-arithmetic.
    // `minus(n, plus(n, m)) == 0` then kernel-proves as a genuine UNBOUNDED
    // universal — `#print axioms = [propext]`, not the bounded `native_decide`
    // fallback. We pin the lift mechanics (no `inductive Nat`, no `__fuel`) AND
    // a clean pass, which together imply the structural-Nat proof.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lean peano-lift test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-peano-lift-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(
        src.join("m.av"),
        "module PeanoArith\n    effects []\n\n\
         type Nat\n    Z\n    S(Nat)\n\n\
         fn plus(x: Nat, y: Nat) -> Nat\n    match x\n        Nat.Z -> y\n        Nat.S(z) -> Nat.S(plus(z, y))\n\n\
         fn minus(x: Nat, y: Nat) -> Nat\n    match x\n        Nat.Z -> Nat.Z\n        Nat.S(a) -> match y\n            Nat.Z -> x\n            Nat.S(b) -> minus(a, b)\n\n\
         verify minus law cancel\n    given n: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    given m: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    minus(n, plus(n, m)) => Nat.Z\n",
    )
    .expect("write m.av");
    let out = temp_output_dir("aver-peano-lift-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("m.av"))
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check --check-json` to run");
    let lean = std::fs::read_to_string(out.join("PeanoArith.lean")).expect("read PeanoArith.lean");
    assert!(
        !lean.contains("inductive Nat") && !lean.contains("__fuel"),
        "the Peano type must lift to builtin Nat (no `inductive Nat`, no fuel):\n{lean}"
    );
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
        "Peano nat-arithmetic must kernel-prove on Lean via the lift as a GENUINE \
         universal — `--check-json` `universal:true` means `#print axioms` is \
         `ofReduceBool`-free (not a bounded `native_decide`).\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_lean_proves_peano_arith_identity_via_nat_lift_kernel_clean() {
    // Layer-2 of the Peano lift (#3): recognize the canonical `plus` (left-
    // recursive addition) and `minus` (truncated subtraction) and emit a
    // kernel-CHECKED bridge `op a b = a + b` / `a - b` (proved by induction on
    // the lifted builtin `Nat`). Rewriting the user ops to the host builtins
    // hands `(n+m)-n = m` to `omega`, which decides linear Nat arithmetic with
    // truncated subtraction — closing a pure-arithmetic identity that bare
    // structural induction leaves at `sorry`. The bridge is PROVED not trusted:
    // a misrecognized op fails its bridge proof (honest `sorry`), never a false
    // theorem. Result is a GENUINE universal (`universal:true`,
    // `#print axioms`-clean of `ofReduceBool`). (TIP isaplanner prop_07.)
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lean peano-arith test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-peano-arith-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(
        src.join("m.av"),
        "module PeanoArithLift\n    effects []\n\n\
         type Nat\n    Z\n    S(Nat)\n\n\
         fn minus(x: Nat, y: Nat) -> Nat\n    match x\n        Nat.Z -> Nat.Z\n        Nat.S(z) -> match y\n            Nat.Z -> x\n            Nat.S(x2) -> minus(z, x2)\n\n\
         fn plus(x: Nat, y: Nat) -> Nat\n    match x\n        Nat.Z -> y\n        Nat.S(z) -> Nat.S(plus(z, y))\n\n\
         verify minus law plusMinusCancel\n    given n: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    given m: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    minus(plus(n, m), n) => m\n",
    )
    .expect("write m.av");
    let out = temp_output_dir("aver-peano-arith-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("m.av"))
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check --check-json` to run");
    // Both arithmetic bridges must be emitted (the `minus` truncated-subtraction
    // recognizer reaches through the TCO'd tail self-call).
    let lean =
        std::fs::read_to_string(out.join("PeanoArithLift.lean")).expect("read PeanoArithLift.lean");
    assert!(
        lean.contains("_plus_isNatAdd") && lean.contains("_minus_isNatSub"),
        "both the `plus`→`+` and `minus`→`-` bridges must be emitted:\n{lean}"
    );
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
        "`(n+m)-n=m` must kernel-prove as a GENUINE universal via the plus/minus \
         Nat-arithmetic bridges + omega (passed, 0 sorries, universal:true).\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_lean_proves_comparison_lift_le_and_lt_kernel_clean() {
    // Comparison half of the canonical Peano family (#3 completion): `le`/`lt`
    // (Bool-returning `≤`/`<`) lift via a kernel-proved Prop-equality bridge
    // `(op a b = true) = (a R b)`, turning the Bool law goal into a Prop that
    // `omega` closes. `lt` matches its SECOND arg first (the bridge inducts on
    // `b`). Pins the two committed corpus instances that were Lean-open before:
    // prop_69 `n ≤ m+n` and prop_65 `i < S(m+i)`. Both must be GENUINE
    // universals (`universal:true`, `#print axioms` free of ofReduceBool).
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lean comparison-lift test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    for (file, op) in [
        ("proof-corpus/tip/isaplanner/prop_69.av", "le (≤)"),
        ("proof-corpus/tip/isaplanner/prop_65.av", "lt (<)"),
    ] {
        let out = temp_output_dir("aver-cmp-lift-out");
        let run = Command::new(aver_bin)
            .current_dir(&repo_root)
            .arg("proof")
            .arg(file)
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
            .unwrap_or_else(|| panic!("{op}: no JSON line:\n{}", format_output(&run)));
        let summary: serde_json::Value = serde_json::from_str(json_line)
            .unwrap_or_else(|e| panic!("{op}: bad JSON ({e}):\n{json_line}"));
        assert_eq!(
            (summary["passed"].as_bool(), summary["universal"].as_bool()),
            (Some(true), Some(true)),
            "{op} comparison law must kernel-prove as a GENUINE universal via the \
             `(op a b = true) = (a R b)` bridge + omega.\n{}",
            format_output(&run)
        );
        let _ = std::fs::remove_dir_all(&out);
    }
}

#[test]
fn proof_lean_proves_mul_distributivity_via_nat_lift_kernel_clean() {
    // `*` member of the family. `times` lifts to builtin `*` via a kernel-proved
    // bridge `times a b = a * b` (whose succ case uses the `+` bridge). `*` is
    // nonlinear — omega can't and core Lean has no `ring` — so distributivity
    // closes via core `Nat.mul_add` after the bridges rewrite. GENUINE universal.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lean mul-lift test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-mul-lift-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(
        src.join("m.av"),
        "module MulDist\n    effects []\n\n\
         type Nat\n    Z\n    S(Nat)\n\n\
         fn plus(x: Nat, y: Nat) -> Nat\n    match x\n        Nat.Z -> y\n        Nat.S(z) -> Nat.S(plus(z, y))\n\n\
         fn times(x: Nat, y: Nat) -> Nat\n    match x\n        Nat.Z -> Nat.Z\n        Nat.S(z) -> plus(y, times(z, y))\n\n\
         verify times law leftDistrib\n    given a: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    given b: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    given c: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    times(a, plus(b, c)) => plus(times(a, b), times(a, c))\n",
    )
    .expect("write m.av");
    let out = temp_output_dir("aver-mul-lift-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("m.av"))
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check --check-json` to run");
    let lean = std::fs::read_to_string(out.join("MulDist.lean")).expect("read MulDist.lean");
    assert!(
        lean.contains("_times_isNatMul") && lean.contains("_plus_isNatAdd"),
        "the `*` bridge (and its prerequisite `+` bridge) must be emitted:\n{lean}"
    );
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
        "left-distributivity `a*(b+c) = a*b + a*c` must kernel-prove as a GENUINE \
         universal via the times/plus bridges + Nat.mul_add.\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_lean_rejects_noncanonical_peano_ops_no_bridge() {
    // NEGATIVE test (the soundness gate the reviewer flagged as missing): the
    // arithmetic/comparison recognizers key on SHAPE, so a lookalike that is NOT
    // the canonical operation must NOT get a bridge. `addTwo` adds TWO per step
    // (`2a+b`, not `a+b`); `weirdCmp` ignores its second arg (not `≤`/`<`).
    // Neither is a canonical Peano op, so NO `_isNat{Add,Sub,Mul,Le,Lt}` bridge
    // may be emitted — if one were, its kernel proof would be a false claim.
    // (The bridge is also kernel-checked, so even a hypothetical misfire could
    // not mint a theorem; this pins the recognizer's conservativeness directly.)
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lean negative-recognizer test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-noncanon-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(
        src.join("m.av"),
        "module NonCanon\n    effects []\n\n\
         type Nat\n    Z\n    S(Nat)\n\n\
         fn addTwo(x: Nat, y: Nat) -> Nat\n    match x\n        Nat.Z -> y\n        Nat.S(z) -> Nat.S(Nat.S(addTwo(z, y)))\n\n\
         fn weirdCmp(x: Nat, y: Nat) -> Bool\n    match x\n        Nat.Z -> true\n        Nat.S(z) -> weirdCmp(z, y)\n\n\
         verify addTwo law selfEq\n    given a: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    given b: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    addTwo(a, b) => addTwo(a, b)\n",
    )
    .expect("write m.av");
    let out = temp_output_dir("aver-noncanon-out");
    let _ = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("m.av"))
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .output()
        .expect("expected `aver proof` to run");
    let lean = std::fs::read_to_string(out.join("NonCanon.lean")).expect("read NonCanon.lean");
    for marker in [
        "_isNatAdd",
        "_isNatSub",
        "_isNatMul",
        "_isNatLe",
        "_isNatLt",
    ] {
        assert!(
            !lean.contains(marker),
            "a non-canonical op must NOT get the `{marker}` bridge (recognizer must \
             reject lookalike shapes):\n{lean}"
        );
    }
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_lean_proves_count_plus_concat_homomorphism_kernel_clean() {
    // Induction-target selection (the generic fix behind #1): a list-
    // homomorphism `plus (count n xs) (count n ys) = count n (xs ++ ys)` has
    // BOTH a Nat given (`n`) and List givens. Inducting on `n` — which the old
    // "first recursive-typed given" rule did — gets nowhere (`count` recurses
    // on the LIST, not on `n`) and falls to `sorry`. law_auto now routes
    // induction to the variable the VERIFIED fn structurally recurses on, so
    // it inducts on `xs`. The cons arm then needs the inner `match eqNat n
    // head` peeled: the `split`-based ladder branch case-splits the symbolic
    // Bool scrutinee and closes both arms with the IH + `omega`. The result is
    // a GENUINE universal (`#print axioms = [propext]`, `universal:true`), not
    // a bounded `native_decide`. (TIP isaplanner prop_02.)
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lean count-homomorphism test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-count-hom-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(
        src.join("m.av"),
        "module CountHom\n    effects []\n\n\
         type Nat\n    Z\n    S(Nat)\n\n\
         fn eqNat(x: Nat, y: Nat) -> Bool\n    match x\n        Nat.Z -> match y\n            Nat.Z -> true\n            Nat.S(z) -> false\n        Nat.S(x2) -> match y\n            Nat.Z -> false\n            Nat.S(y2) -> eqNat(x2, y2)\n\n\
         fn count(x: Nat, y: List<Nat>) -> Nat\n    match y\n        [] -> Nat.Z\n        [z, ..ys] -> match eqNat(x, z)\n            true -> Nat.S(count(x, ys))\n            false -> count(x, ys)\n\n\
         fn plus(x: Nat, y: Nat) -> Nat\n    match x\n        Nat.Z -> y\n        Nat.S(z) -> Nat.S(plus(z, y))\n\n\
         fn appendNat(xs: List<Nat>, ys: List<Nat>) -> List<Nat>\n    List.concat(xs, ys)\n\n\
         verify count law countPlusConcat\n    given n: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    given xs: List<Nat> = [[], [Nat.Z]]\n    given ys: List<Nat> = [[], [Nat.S(Nat.Z)]]\n    plus(count(n, xs), count(n, ys)) => count(n, appendNat(xs, ys))\n",
    )
    .expect("write m.av");
    let out = temp_output_dir("aver-count-hom-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("m.av"))
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check --check-json` to run");
    // The induction must target the LIST, not the Nat given.
    let lean = std::fs::read_to_string(out.join("CountHom.lean")).expect("read CountHom.lean");
    assert!(
        lean.contains("induction xs with"),
        "count homomorphism must induct on the list given `xs` (the var `count` \
         recurses on), not the Nat given `n`:\n{lean}"
    );
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
        "count/++ homomorphism must kernel-prove as a GENUINE universal via \
         list-induction on `xs` + the inner-match `split` (passed, 0 sorries, \
         universal:true).\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_lean_proves_rev_antihomomorphism_kernel_clean() {
    // SAME backend-neutral `RevOp` recognizer as the Dafny test above, but a
    // Lean renderer: `rev (rev x) = x` on List<Int> kernel-proves because the
    // fold lowers to a clean `def … termination_by` (no fuel / no Nat
    // collision). The renderer prepends the proved append-nil-right /
    // associativity / rev-distribution theorems and adds rev-distribution to
    // the list-induction simp set. `lake build` succeeds with ZERO sorries on
    // the universal, i.e. it is kernel-checked (`#print axioms = [propext]`).
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lean rev kernel test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-rev-lean-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(
        src.join("m.av"),
        "module RevHomLean\n    effects []\n\n\
         fn append(x: List<Int>, y: List<Int>) -> List<Int>\n    match x\n        [] -> y\n        [z, ..xs] -> List.concat([z], append(xs, y))\n\n\
         fn rev(x: List<Int>) -> List<Int>\n    match x\n        [] -> []\n        [y, ..xs] -> append(rev(xs), [y])\n\n\
         verify rev law revRev\n    given x: List<Int> = [[1, 2]]\n    rev(rev(x)) => x\n",
    )
    .expect("write m.av");
    let out = temp_output_dir("aver-rev-lean-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("m.av"))
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
        "rev∘rev must kernel-prove on Lean via the shared recognizer as a GENUINE \
         universal (passed, 0 sorries, `universal:true` = `#print axioms` is \
         `ofReduceBool`-free).\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

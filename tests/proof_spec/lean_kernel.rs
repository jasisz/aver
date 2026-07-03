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
fn proof_lean_mutual_recursive_adt_predicate_uses_native_termination_kernel_clean() {
    // A mutual recursive-ADT predicate SCC (`treeOk : Tree -> Bool` +
    // `treeListOk : List Tree -> Bool`) must emit as a NATIVE
    // `termination_by (sizeOf, rank)` mutual block — NOT a fuel-wrapped
    // `__fuel` def. Native emission keeps the predicate a CLEAN recursive def,
    // so its cons-distribution law
    //   `treeListOk (h :: t) = treeOk h && treeListOk t`
    // closes as a one-step structural proof. Under the old conservative gate the
    // SCC fell to fuel because a member has a recursive-ADT param (`t : Tree`),
    // and the fuel WRAPPER makes the two sides of that law carry DIFFERENT fuel
    // expressions — a gap no `simp`/`grind` bridges without a synthesized
    // fuel-congruence lemma, so the law stayed bounded (`sorry`). Proof side is
    // fail-closed: a wrong measure only makes Lean reject the `termination_by`
    // (SCC stays open), never a false theorem — so trying native here is safe.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping native-termination ADT test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-native-adt-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(
        src.join("m.av"),
        "module TreeSafe\n    effects []\n\n\
         type Tree\n    Leaf(Bool)\n    Node(List<Tree>)\n\n\
         fn treeOk(t: Tree) -> Bool\n    match t\n        Tree.Leaf(b) -> b\n        Tree.Node(kids) -> treeListOk(kids)\n\n\
         fn treeListOk(ts: List<Tree>) -> Bool\n    match ts\n        [] -> true\n        [h, ..rest] -> match treeOk(h)\n            true -> treeListOk(rest)\n            false -> false\n\n\
         verify treeListOk law consDistrib\n    given h: Tree = [Tree.Leaf(true), Tree.Leaf(false), Tree.Node([Tree.Leaf(true)])]\n    given t: List<Tree> = [[], [Tree.Leaf(true)], [Tree.Leaf(false), Tree.Leaf(true)]]\n    treeListOk(List.prepend(h, t)) => Bool.and(treeOk(h), treeListOk(t))\n",
    )
    .expect("write m.av");
    let out = temp_output_dir("aver-native-adt-out");
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
    let lean = std::fs::read_to_string(out.join("TreeSafe.lean")).expect("read TreeSafe.lean");
    assert!(
        !lean.contains("__fuel") && lean.contains("termination_by (sizeOf"),
        "the mutual recursive-ADT predicate must emit NATIVE `termination_by (sizeOf, _)` \
         (no `__fuel` wrapper):\n{lean}"
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
        "the cons-distribution law over the native (fuel-free) predicate must \
         kernel-prove as a GENUINE universal:\n{}",
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
    // `rev (rev x) = x` decomposed into its standard helper law: the user
    // writes `revDist` (the rev anti-homomorphism over `List.concat`), and the
    // earlier-law pool feeds it to the `revRev` proof as a simp rewrite. No
    // synthesized algebra — the helper law IS the decomposition (exactly what
    // The Method conjectures). `lake build` succeeds with ZERO sorries on both
    // universals, i.e. kernel-checked (`#print axioms` free of `ofReduceBool`).
    // The compiler only CITES the earlier law; it does not invent the rev
    // anti-homomorphism. (Before #B this closed via a synthesized `rev` algebra
    // template — that content synthesizer is gone; the decomposition replaces it.)
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
         fn rev(x: List<Int>) -> List<Int>\n    match x\n        [] -> []\n        [y, ..xs] -> List.concat(rev(xs), [y])\n\n\
         verify rev law revDist\n    given x: List<Int> = [[1], [1, 2], [1, 2, 3]]\n    given y: List<Int> = [[4], [4, 5], [4, 5, 6]]\n    rev(List.concat(x, y)) => List.concat(rev(y), rev(x))\n\n\
         verify rev law revRev\n    given x: List<Int> = [[], [1], [1, 2], [1, 2, 3]]\n    rev(rev(x)) => x\n",
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

#[test]
fn proof_lean_proves_string_length_additivity_kernel_clean() {
    // Pure builtin String-length additivity: `String.len(a + b) =
    // String.len(a) + String.len(b)`. The law mentions no user fn — its lhs
    // calls builtins directly — so every cone-anchored rung declines
    // (Induction needs a recursive-ADT given; SimpOverPreludeLemmas anchors
    // its unfold set on the subject fn appearing in the lhs) and the IR
    // strategy stays `BackendDispatch`. Dafny's Z3 already discharges the
    // sequence-length axiom; the Lean side used to fall to a bare `sorry`.
    // The shape-driven `emit_string_length_additive_law` rung now closes it:
    // `String.add_eq_append` (rfl) rewrites the custom `HAdd String` `+` to
    // `++`, the core `@[simp] String.length_append` distributes the length,
    // and `omega` finishes over the `(… : Int)` coercions — a GENUINE
    // universal (`universal:true`), not a bounded `native_decide`. (Revert
    // the emitter and this law degrades to `universal:false` — the rung is
    // load-bearing.)
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lean string-length-additivity test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-strlen-add-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(
        src.join("m.av"),
        "module StrLenAdd\n    effects []\n\n\
         fn assembledLength(prefix: String, body: String) -> Int\n    String.len(prefix + body)\n\n\
         verify assembledLength law reservationIsExact\n    given prefix: String = [\"\", \"12:00 \"]\n    given body: String = [\"\", \"boot ok\"]\n    String.len(prefix + body) => String.len(prefix) + String.len(body)\n",
    )
    .expect("write m.av");
    let out = temp_output_dir("aver-strlen-add-out");
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
    // The closer must be the string-length-additive rung, not a sorry.
    let lean = std::fs::read_to_string(out.join("StrLenAdd.lean")).expect("read StrLenAdd.lean");
    assert!(
        lean.contains("simp only [String.add_eq_append, String.length_append] <;> omega"),
        "the String-length-additive law must emit the `String.add_eq_append` + \
         `String.length_append` + `omega` rung:\n{lean}"
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
        "String.len(a + b) = String.len(a) + String.len(b) must kernel-prove as a \
         GENUINE universal (passed, 0 sorries, universal:true).\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_lean_proves_string_concat_monoid_kernel_clean() {
    // Pure builtin String-concat monoid identities — `s + "" = s`,
    // `"" + s = s`, `(a + b) + c = a + (b + c)`. Same class as the
    // String-length-additivity rung but WITHOUT a `String.len` wrapper, so
    // a separate shape-driven rung (`emit_string_append_monoid_law`) closes
    // them: `String.add_eq_append` rewrites the custom `HAdd String` `+` to
    // `++`, then the Lean-core `String.append_empty`/`empty_append`/
    // `append_assoc` finish. Each currently fell to a bare `sorry`
    // (BackendDispatch; Dafny's Z3 already proves them).
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lean string-concat-monoid test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-strmonoid-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(
        src.join("m.av"),
        "module StrMonoid\n    effects []\n\n\
         fn cat2(a: String, b: String) -> String\n    a + b\n\n\
         verify cat2 law rightId\n    given a: String = [\"\", \"x\"]\n    given b: String = [\"\", \"y\"]\n    a + \"\" => a\n\n\
         verify cat2 law leftId\n    given a: String = [\"\", \"x\"]\n    given b: String = [\"\", \"y\"]\n    \"\" + a => a\n\n\
         verify cat2 law assoc\n    given a: String = [\"\", \"x\"]\n    given b: String = [\"\", \"y\"]\n    (a + b) + a => a + (b + a)\n",
    )
    .expect("write m.av");
    let out = temp_output_dir("aver-strmonoid-out");
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
    let lean = std::fs::read_to_string(out.join("StrMonoid.lean")).expect("read StrMonoid.lean");
    assert!(
        lean.contains(
            "String.add_eq_append, String.append_empty, String.empty_append, String.append_assoc"
        ),
        "the String-concat-monoid laws must emit the append-monoid simp rung:\n{lean}"
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
        "all three String-concat monoid identities must kernel-prove as GENUINE \
         universals (passed, 0 sorries, universal:true).\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_lean_min_associativity_closes_without_build_error() {
    // The `Int.min`/`Int.max` LinearArithmetic rung used to emit a bare
    // `simp only [..., Int.min_def, Int.max_def] <;> (try split) <;>
    // simp_all <;> omega` with NO `first | … | sorry` wrapper. For a 3-way
    // `min` nest that chain left unsolved goals and hard-failed the WHOLE
    // module's Lean build (not just this law). The rung now tries a clean
    // `<;> omega` (omega case-splits the unfolded ifs itself) first, keeps
    // the legacy chain as a non-regressing fallback, and floors on `sorry`.
    // So min-associativity now closes as a universal AND a failing min/max
    // case can never crash the build. (Revert the rung change and this
    // task becomes `universal: no` via a Lean build error.)
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lean min-associativity test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-minassoc-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(
        src.join("m.av"),
        "module MinAssoc\n    effects []\n\n\
         fn minOf(a: Int, b: Int) -> Int\n    Int.min(a, b)\n\n\
         verify minOf law associative\n    given a: Int = [0, 1, -3]\n    given b: Int = [0, 1, -3]\n    given c: Int = [0, 1, -3]\n    minOf(minOf(a, b), c) => minOf(a, minOf(b, c))\n",
    )
    .expect("write m.av");
    let out = temp_output_dir("aver-minassoc-out");
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
        "Int.min 3-way associativity must kernel-prove as a universal (passed, 0 \
         sorries, universal:true) — and crucially must NOT hard-fail the Lean build.\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_lean_int_abs_lowers_int_honest_so_nesting_builds() {
    // `Int.abs : Int -> Int` at the Aver surface now lowers to the Int-honest
    // `((arg : Int).natAbs : Int)`, not the `Nat`-typed bare `arg.natAbs`. A
    // nested `Int.abs(Int.abs x)` used to become `(x.natAbs).natAbs` — `Nat`
    // has no `.natAbs`, an uncatchable Lean build error. The ARGUMENT is
    // ascribed `: Int` (not just the result) because `.natAbs` is dot-notation
    // resolved from the receiver's type, and a bare numeric-literal sample
    // (`Int.abs(-5)`) would otherwise default the receiver to `Nat`. (Revert
    // the lowering and this module fails to build / `native_decide` chokes on
    // `Nat.natAbs`.)
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lean int-abs lowering test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-intabs-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(
        src.join("m.av"),
        "module IntAbsNest\n    effects []\n\n\
         fn doubleAbs(x: Int) -> Int\n    Int.abs(Int.abs(x))\n\n\
         verify doubleAbs\n    doubleAbs(-7) => 7\n    doubleAbs(4) => 4\n    doubleAbs(0) => 0\n",
    )
    .expect("write m.av");
    let out = temp_output_dir("aver-intabs-out");
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
    let lean = std::fs::read_to_string(out.join("IntAbsNest.lean")).expect("read IntAbsNest.lean");
    assert!(
        lean.contains("(x : Int).natAbs : Int)"),
        "Int.abs must lower to the arg-ascribed Int-honest form `((… : Int).natAbs : Int)`:\n{lean}"
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
        (summary["passed"].as_bool(), summary["sorries"].as_u64()),
        (Some(true), Some(0)),
        "a fn with a nested `Int.abs(Int.abs x)` must build clean (passed, 0 sorries) — \
         the Int-honest lowering removes the `Nat.natAbs` build error.\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_lean_proves_empty_map_facts_kernel_clean() {
    // Pure builtin empty-map facts — `Map.get(empty, k) = None`,
    // `Map.has(empty, k) = false`, `Map.len(empty) = 0`. Each is stated
    // through its subject fn (`lookup(k) => None`), so `SimpOverPreludeLemmas`
    // claims it but its minimal `simp [cone, Int.add_sub_cancel]` can't reduce
    // the `AverMap.*` accessor and parks it on a sorry. The empty-map-precise
    // `emit_map_empty_fact_law` rung (before the prelude rung) closes it with
    // a bounded `simp only [cone, AverMap.get/has/len, []-lemmas]`. Dafny's Z3
    // already proves these.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lean empty-map-facts test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-emptymap-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(
        src.join("m.av"),
        "module EmptyMap\n    effects []\n\n\
         fn emptyM() -> Map<String, Int>\n    {}\n\n\
         fn lookup(k: String) -> Option<Int>\n    Map.get(emptyM(), k)\n\n\
         fn contains(k: String) -> Bool\n    Map.has(emptyM(), k)\n\n\
         fn size(ignored: Int) -> Int\n    Map.len(emptyM())\n\n\
         verify lookup law getEmptyNone\n    given k: String = [\"a\", \"b\"]\n    Map.get(emptyM(), k) => Option.None\n\n\
         verify contains law hasEmptyFalse\n    given k: String = [\"a\", \"b\"]\n    Map.has(emptyM(), k) => false\n\n\
         verify size law lenEmptyZero\n    given ignored: Int = [0, 1]\n    Map.len(emptyM()) => 0\n",
    )
    .expect("write m.av");
    let out = temp_output_dir("aver-emptymap-out");
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
    let lean = std::fs::read_to_string(out.join("EmptyMap.lean")).expect("read EmptyMap.lean");
    assert!(
        lean.contains("AverMap.get, AverMap.has, AverMap.len"),
        "the empty-map-fact rung must emit the AverMap accessor simp set:\n{lean}"
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
        "all three empty-map facts (get/has/len) must kernel-prove as GENUINE \
         universals (passed, 0 sorries, universal:true).\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_lean_proves_int_comparison_identities_kernel_clean() {
    // Bool-valued Int comparison identities — equality symmetry
    // (`(a == b) = (b == a)`), `!(a < b) = (a >= b)`, totality of `<=`
    // (`(a <= b) || (b <= a) = true`). These route to the `wrapper_return`
    // arm of `emit_simp_omega_from_ir`, whose sign-split `by_cases <;> simp`
    // left them at "simp made no progress" — and with no `first | … | sorry`
    // wrapper that hard-failed the whole module build. The arm now tries the
    // sign-split first (unchanged for arithmetic laws), then a comparison
    // normaliser (`COMPARISON_NORMALIZERS` + `omega`), then a `sorry` floor.
    // (Revert and these become a Lean build error, not even a caught sorry.)
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lean int-comparison test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-intcmp-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(
        src.join("m.av"),
        "module IntCmp\n    effects []\n\n\
         fn sameKey(a: Int, b: Int) -> Bool\n    a == b\n\n\
         fn atLeast(a: Int, b: Int) -> Bool\n    a >= b\n\n\
         fn notBelow(a: Int, b: Int) -> Bool\n    Bool.not(a < b)\n\n\
         fn comparable(a: Int, b: Int) -> Bool\n    Bool.or(a <= b, b <= a)\n\n\
         verify sameKey law symmetric\n    given a: Int = [0, 1, -2]\n    given b: Int = [0, 1, -2]\n    sameKey(a, b) => sameKey(b, a)\n\n\
         verify notBelow law isAtLeast\n    given a: Int = [0, 1, -2]\n    given b: Int = [0, 1, -2]\n    notBelow(a, b) => atLeast(a, b)\n\n\
         verify comparable law totalOrder\n    given a: Int = [0, 1, -2]\n    given b: Int = [0, 1, -2]\n    comparable(a, b) => true\n",
    )
    .expect("write m.av");
    let out = temp_output_dir("aver-intcmp-out");
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
        "the three Int comparison identities must kernel-prove as GENUINE \
         universals (passed, 0 sorries, universal:true) — and must NOT hard-fail \
         the Lean build.\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_lean_proves_int_abs_identities_kernel_clean() {
    // Pure builtin `Int.abs` identities — idempotence
    // (`Int.abs(Int.abs x) = Int.abs x`), multiplicativity
    // (`Int.abs(x*y) = Int.abs x * Int.abs y`), and non-negativity
    // (`Int.abs x >= 0 = true`). `Int.abs` lowers Int-honestly to
    // `((… : Int).natAbs : Int)`; the `emit_int_abs_identity_law` rung
    // (before the LinearArithmetic rung) closes all three with
    // `Int.natAbs_natCast` / `Int.natAbs_mul` / `Int.natCast_mul`
    // (+ a full-`simp` fallback for the Bool `>= 0` wrapper), `sorry`-floored.
    // Before this rung they hard-failed the build (idempotence/non-neg) or
    // sorried (multiplicativity). Dafny's Z3 proves all three.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lean int-abs-identities test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-absid-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(
        src.join("m.av"),
        "module AbsId\n    effects []\n\n\
         fn magnitude(x: Int) -> Int\n    Int.abs(x)\n\n\
         fn scaledMagnitude(x: Int, y: Int) -> Int\n    Int.abs(x * y)\n\n\
         verify magnitude law idempotent\n    given x: Int = [0, 3, -5]\n    Int.abs(Int.abs(x)) => Int.abs(x)\n\n\
         verify magnitude law nonNegative\n    given x: Int = [0, 3, -5]\n    Int.abs(x) >= 0 => true\n\n\
         verify scaledMagnitude law multiplicative\n    given x: Int = [0, 3, -5]\n    given y: Int = [0, 2, -4]\n    Int.abs(x * y) => Int.abs(x) * Int.abs(y)\n",
    )
    .expect("write m.av");
    let out = temp_output_dir("aver-absid-out");
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
    let lean = std::fs::read_to_string(out.join("AbsId.lean")).expect("read AbsId.lean");
    assert!(
        lean.contains("Int.natAbs_natCast, Int.natAbs_mul, Int.natCast_mul"),
        "the Int.abs identity rung must emit the natAbs/cast lemma set:\n{lean}"
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
        "the three Int.abs identities must kernel-prove as GENUINE universals \
         (passed, 0 sorries, universal:true).\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_lean_proves_map_set_nonempty_kernel_clean() {
    // `Map.len(Map.set(m, k, v)) >= 1 => true` — `set` always yields a
    // non-empty map. Unlike the empty-map facts this needs real induction
    // (`set.go` length is `>= 1`), carried by the hand-proved prelude lemma
    // `AverMap.len_set_ge_one` (stated in the exact lowered goal shape and
    // demand-shipped). The `emit_map_len_set_positive_law` rung discharges the
    // law with `exact AverMap.len_set_ge_one _ _ _`. Was a bare sorry before.
    // Dafny's Z3 proves it.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lean map-set-nonempty test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-mapsetne-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(
        src.join("m.av"),
        "module MapSetNE\n    effects []\n\n\
         fn after(reg: Map<String, Int>, k: String, v: Int) -> Int\n    Map.len(Map.set(reg, k, v))\n\n\
         verify after law nonEmpty\n    given reg: Map<String, Int> = [{}, {\"a\" => 1}]\n    given k: String = [\"x\", \"a\"]\n    given v: Int = [7]\n    Map.len(Map.set(reg, k, v)) >= 1 => true\n",
    )
    .expect("write m.av");
    let out = temp_output_dir("aver-mapsetne-out");
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
    let lean = std::fs::read_to_string(out.join("MapSetNE.lean")).expect("read MapSetNE.lean");
    assert!(
        lean.contains("AverMap.len_set_ge_one"),
        "the map-set-nonempty rung must cite the inductive prelude lemma \
         `AverMap.len_set_ge_one`:\n{lean}"
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
        "Map.len(Map.set m k v) >= 1 must kernel-prove as a GENUINE universal \
         (passed, 0 sorries, universal:true) via the inductive prelude lemma.\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_lean_proves_fact_acc_kernel_clean_universal() {
    // Multiplicative twin of `sum_acc`: the `WrapperOverRecursion` strategy
    // proves a tail-recursive factorial equals its recurrence over a Peano
    // `Nat`. The accumulator threads through the user `mul`, whose nonlinear
    // residual `omega` cannot close, so the emitter bridges `mul`→`Nat.*`
    // (via the proved `isNatMul`/`isNatAdd` bridges) and finishes the
    // decomposition lemma `factTR n acc = mul (factTR n 1) acc` + the main
    // law with the core `Nat.mul_*` lemmas — no Mathlib. Result is a GENUINE
    // universal (`universal:true`, `#print axioms` clean of `sorryAx` /
    // `ofReduceBool`). Regression guard for the whole Peano-Nat path.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lean fact-acc test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let out = temp_output_dir("aver-fact-acc-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg("proof-corpus/handwritten/fact_acc_spec.av")
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check --check-json` to run");
    let lean =
        std::fs::read_to_string(out.join("FactAccSpec.lean")).expect("read FactAccSpec.lean");
    // The strategy must fire: the multiplicative bridge + the accumulator-
    // decomposition lemma are what close the law (not the generic ladder).
    assert!(
        lean.contains("_mul_isNatMul") && lean.contains("theorem factTR_acc"),
        "the `mul`→`Nat.*` bridge and the `factTR_acc` decomposition lemma must \
         be emitted (the WrapperOverRecursion Peano-Nat path):\n{lean}"
    );
    // `factTR` must be a terminating `def`, never an opaque `partial def`
    // (which would make the decomposition lemma unprovable).
    assert!(
        lean.contains("def factTR") && !lean.contains("partial def factTR"),
        "factTR must emit as a terminating `def`, not `partial def`:\n{lean}"
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
        "tail-recursive factorial == its recurrence must kernel-prove as a \
         GENUINE universal (passed, 0 sorries, universal:true).\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_lean_proves_list_prod_kernel_clean_universal() {
    // List + Mul corner of the accumulator-fold grid: a tail-recursive product
    // equals its recurrence. The `List` driver fixes the induction skeleton;
    // the `*` combine fixes the close — the linear `omega` of the additive
    // `sum_acc` twin cannot discharge the nonlinear residual, so the emitter
    // closes with the core `Int.mul_*` associativity/commutativity lemmas (no
    // Mathlib). Regression guard that the close is chosen by the combine op,
    // not hardcoded to the `List` driver's original additive `omega`.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lean list-prod test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let out = temp_output_dir("aver-list-prod-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg("proof-corpus/handwritten/list_prod_spec.av")
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check --check-json` to run");
    let lean = std::fs::read_to_string(out.join("ListProd.lean")).expect("read ListProd.lean");
    assert!(
        lean.contains("theorem prodTR_acc") && lean.contains("Int.mul_assoc"),
        "the `prodTR_acc` decomposition lemma and the `Int.mul_*` multiplicative \
         close must be emitted (the combine op picks the close):\n{lean}"
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
        "tail-recursive product == its recurrence must kernel-prove as a \
         GENUINE universal (passed, 0 sorries, universal:true).\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_lean_speculative_proves_single_list_conditional_universal() {
    // Gap-1 speculative-universal mechanism: a SINGLE-LIST conditional law
    // (`when allZero(xs) -> sumList(xs) = Z`) cannot be statically classified as
    // universal-closeable (the generic conditional driver closes some single-list
    // shapes and `sorry`s others), so the probe states it universally, learns
    // from ONE instrumented build that it CLOSES — the `cases hd` rung splits the
    // per-element premise (`allZero (hd :: tl)`) so the conditional IH applies —
    // and commits it as a genuine universal. Result is `universal:true`, 0
    // sorries, `#print axioms` clean of `sorryAx`. The helper `sumCons` is the
    // homomorphism the conditional law decomposes through. Regression guard for
    // the whole single-list conditional try-universal-fall-back-to-sampled path.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lean speculative-universal test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let out = temp_output_dir("aver-speculative-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg("proof-corpus/decomposed/handwritten/all_zero_sum.av")
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check --check-json` to run");
    let lean = std::fs::read_to_string(out.join("AllZeroSum.lean")).expect("read AllZeroSum.lean");
    // The conditional law must be COMMITTED as a universal-classed theorem (no
    // sampled-domain disjunction premises) — the probe promoted it after the
    // build proved it closes.
    assert!(
        lean.contains("-- aver:law-class sumList_law_sumAllZero universal"),
        "sumAllZero must be committed as a universal-classed conditional:\n{lean}"
    );
    // The committed proof must carry neither the probe's transient trace floor
    // nor a reachable sorry.
    assert!(
        !lean.contains("AVERSPEC_SORRY"),
        "the committed proof must not carry the probe's trace floor:\n{lean}"
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
        "the single-list conditional must kernel-prove as a GENUINE universal \
         (passed, 0 sorries, universal:true).\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_lean_proves_nat_tri_kernel_clean_universal() {
    // Nat + Add corner of the accumulator-fold grid: a tail-recursive
    // triangular-sum equals its recurrence. The Peano-`Nat` driver fixes the
    // induction skeleton; the `+` combine fixes the close — the `plus`→`+`
    // bridge plus `omega` (no `Nat.mul_*` needed, unlike the factorial twin).
    // `triTR` must emit as a terminating `def`. Regression guard that the
    // Peano-`Nat` driver is no longer wired only alongside the multiplicative
    // close.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lean nat-tri test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let out = temp_output_dir("aver-nat-tri-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg("proof-corpus/handwritten/nat_tri_spec.av")
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check --check-json` to run");
    let lean = std::fs::read_to_string(out.join("NatTri.lean")).expect("read NatTri.lean");
    assert!(
        lean.contains("theorem triTR_acc") && lean.contains("_plus_isNatAdd"),
        "the `plus`→`+` bridge and the `triTR_acc` decomposition lemma must be \
         emitted (the additive Peano-Nat path):\n{lean}"
    );
    assert!(
        lean.contains("def triTR") && !lean.contains("partial def triTR"),
        "triTR must emit as a terminating `def`, not `partial def`:\n{lean}"
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
        "tail-recursive triangular-sum == its recurrence must kernel-prove as a \
         GENUINE universal (passed, 0 sorries, universal:true).\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_lean_lifts_aliased_peano_type_to_nat_and_proves_universal() {
    // A Peano natural named other than `Nat` (`Num` with `Zero`/`Succ`) must
    // get the builtin-`Nat` treatment by SHAPE, not by name: its values lift
    // (`Zero`→`0`, `Succ e`→`e + 1`) AND its type annotations lift (`Num`→
    // `Nat`), so the factorial wrapper-over-recursion law closes kernel-clean.
    // Regression guard: before the type-annotation lift, the binder type
    // (`Num`) disagreed with the lifted `Nat` literals and the proof failed.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping lean aliased-peano test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let out = temp_output_dir("aver-peano-aliased-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg("proof-corpus/handwritten/peano_aliased_spec.av")
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check --check-json` to run");
    let lean =
        std::fs::read_to_string(out.join("PeanoAliased.lean")).expect("read PeanoAliased.lean");
    // The `Num` driver must be lifted to builtin `Nat` in TYPE position too:
    // the `def` binders read `: Nat`, never `: Num` (a `: Num` binder would
    // disagree with the lifted `Nat` literals in the bodies).
    assert!(
        lean.contains("def factTR (n : Nat)")
            && lean.contains("def factSpec (n : Nat)")
            && !lean.contains("(n : Num)"),
        "the aliased Peano type `Num` must lift to builtin `Nat` in type \
         annotations (def binders should read `: Nat`, not `: Num`):\n{lean}"
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
        "a non-`Nat`-named Peano factorial must kernel-prove as a GENUINE \
         universal (passed, 0 sorries, universal:true).\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_lean_frac_order_transitivity_chain_foreign_witness_kernel_clean() {
    // NON-K5 witness for the generic rational-order transitivity-chain rung: a
    // foreign spend-ratio domain (`LedgerBounds`, foreign subject/law/constant/
    // given names) whose strict comparison `ledgerWithinCeiling` is proven as a
    // GENUINE universal by chaining the two premise links plus a ground link
    // (`phaseBudget < absoluteCeiling`, closed by `decide`) through the emitted
    // order kit. Nothing keys on a K5 name — the rung recognizes the order
    // ALGEBRA shape alone, so this different domain still closes. A second law
    // `gapWitness` has the SAME conclusion but a DISCONNECTED premise chain (the
    // top premise's lower endpoint does not match the first premise's upper
    // endpoint), so the recognizer must DECLINE it and it reverts cleanly to the
    // bounded sampled proof — proving the rung never flips a shape it cannot
    // close. Two more universal laws pin the shape classes CI would otherwise
    // miss: `singleLinkWitness` has EXACTLY ONE `when` conjunct plus a ground top
    // link (the regression that turned red when the emit always split on `&&`),
    // and `nonstrictChainWitness` is the 8.1.1-class 3-link chain (two premise
    // links — one strict, one non-strict `isNonNeg (minus …)` — plus a closed
    // ground top link). `universal:true` means `#print axioms` is
    // `ofReduceBool`-free.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping frac-order-transitivity witness test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-frac-order-trans-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(
        src.join("ledger.av"),
        r#"module LedgerBounds
    intent = "Foreign witness: a spend-ratio bound closed by rational-order transitivity."
    effects []

record Fraction
    top: Int
    bottom: Int

fn oneRatio() -> Fraction
    Fraction(top = 1, bottom = 1)

fn phaseBudget() -> Fraction
    Fraction(top = 3, bottom = 4)

fn absoluteCeiling() -> Fraction
    Fraction(top = 9, bottom = 8)

fn plus(a: Fraction, b: Fraction) -> Fraction
    Fraction(top = a.top * b.bottom + b.top * a.bottom, bottom = a.bottom * b.bottom)

fn times(a: Fraction, b: Fraction) -> Fraction
    Fraction(top = a.top * b.top, bottom = a.bottom * b.bottom)

fn minus(a: Fraction, b: Fraction) -> Fraction
    Fraction(top = a.top * b.bottom - b.top * a.bottom, bottom = a.bottom * b.bottom)

fn lessThan(a: Fraction, b: Fraction) -> Bool
    a.top * a.bottom * (b.bottom * b.bottom) < b.top * b.bottom * (a.bottom * a.bottom)

fn isNonNeg(a: Fraction) -> Bool
    a.top * a.bottom >= 0

fn ledgerWithinCeiling(usage: Fraction, quota: Fraction) -> Bool
    ? "The spend spread stays under the absolute ceiling."
    lessThan(minus(times(usage, quota), oneRatio()), absoluteCeiling())

verify ledgerWithinCeiling law chainWitness
    given usage: Fraction = [Fraction(top = 1, bottom = 2)]
    given quota: Fraction = [Fraction(top = 1, bottom = 2)]
    given rate: Fraction = [Fraction(top = 1, bottom = 2), Fraction(top = 1, bottom = 3)]
    given buffer: Fraction = [Fraction(top = 1, bottom = 4)]
    when lessThan(minus(times(usage, quota), oneRatio()), plus(times(rate, rate), buffer))
    when lessThan(plus(times(rate, rate), buffer), phaseBudget())
    ledgerWithinCeiling(usage, quota) holds

verify ledgerWithinCeiling law gapWitness
    given usage: Fraction = [Fraction(top = 1, bottom = 2)]
    given quota: Fraction = [Fraction(top = 1, bottom = 2)]
    given rate: Fraction = [Fraction(top = 1, bottom = 2), Fraction(top = 1, bottom = 3)]
    given buffer: Fraction = [Fraction(top = 1, bottom = 4)]
    when lessThan(minus(times(usage, quota), oneRatio()), plus(times(rate, rate), buffer))
    when lessThan(plus(times(buffer, buffer), rate), phaseBudget())
    ledgerWithinCeiling(usage, quota) holds

verify ledgerWithinCeiling law singleLinkWitness
    given usage: Fraction = [Fraction(top = 1, bottom = 2), Fraction(top = 1, bottom = 3)]
    given quota: Fraction = [Fraction(top = 1, bottom = 2)]
    when lessThan(minus(times(usage, quota), oneRatio()), phaseBudget())
    ledgerWithinCeiling(usage, quota) holds

verify ledgerWithinCeiling law nonstrictChainWitness
    given usage: Fraction = [Fraction(top = 1, bottom = 2)]
    given quota: Fraction = [Fraction(top = 1, bottom = 2)]
    given rate: Fraction = [Fraction(top = 1, bottom = 2), Fraction(top = 1, bottom = 3)]
    given buffer: Fraction = [Fraction(top = 1, bottom = 4)]
    when lessThan(minus(times(usage, quota), oneRatio()), plus(times(rate, rate), buffer))
    when isNonNeg(minus(phaseBudget(), plus(times(rate, rate), buffer)))
    ledgerWithinCeiling(usage, quota) holds
"#,
    )
    .expect("write ledger.av");
    let out = temp_output_dir("aver-frac-order-trans-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("ledger.av"))
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check --check-json` to run");
    let lean =
        std::fs::read_to_string(out.join("LedgerBounds.lean")).expect("read LedgerBounds.lean");
    // The rung flipped the connected chain to universal and left the
    // disconnected near-miss bounded — pinned on the law-class markers so both
    // the fire (foreign names) and the clean revert are asserted structurally.
    assert!(
        lean.contains(
            "-- aver:law-class ledgerWithinCeiling_law_chainWitness universal ledgerWithinCeiling.chainWitness"
        ),
        "the connected foreign chain law must be classed universal by the \
         transitivity-chain rung:\n{lean}"
    );
    assert!(
        lean.contains(
            "-- aver:law-class ledgerWithinCeiling_law_gapWitness bounded-domain ledgerWithinCeiling.gapWitness"
        ),
        "the disconnected near-miss law must REVERT to bounded-domain (the rung \
         declines a chain it cannot connect):\n{lean}"
    );
    assert!(
        lean.contains(
            "-- aver:law-class ledgerWithinCeiling_law_singleLinkWitness universal ledgerWithinCeiling.singleLinkWitness"
        ),
        "the single-conjunct (one strict premise + ground top link) law must be \
         classed universal — the emit must not split a non-existent `&&`:\n{lean}"
    );
    assert!(
        lean.contains(
            "-- aver:law-class ledgerWithinCeiling_law_nonstrictChainWitness universal ledgerWithinCeiling.nonstrictChainWitness"
        ),
        "the 8.1.1-class 3-link chain (strict + non-strict premise links + closed \
         ground top link) must be classed universal:\n{lean}"
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
            summary["universal_laws"].as_u64(),
            summary["bounded_laws"].as_u64(),
        ),
        (Some(true), Some(0), Some(true), Some(3), Some(1)),
        "the foreign transitivity-chain witness must kernel-prove the three \
         connected shapes as GENUINE universals (`ofReduceBool`-free) and keep \
         the disconnected near-miss bounded — three universal, one bounded, zero \
         sorries.\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

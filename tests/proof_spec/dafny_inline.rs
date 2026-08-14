use super::*;

/// Write `source` to a temp `.av`, run `aver proof --backend dafny --check
/// --check-json`, and assert the law's universal closed for real: passed,
/// with no Dafny errors, no trusted axioms, and no dropped (sample-only)
/// universal. Used to pin the Dafny homomorphism strategies.
fn assert_dafny_proves_inline(source: &str, prefix: &str) {
    if Command::new("dafny").arg("--version").output().is_err() {
        eprintln!("skipping dafny proof test ({prefix}): `dafny` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir(&format!("{prefix}-src"));
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(src.join("m.av"), source).expect("write m.av");
    let out = temp_output_dir(&format!("{prefix}-out"));
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("m.av"))
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
            summary["passed"].as_bool(),
            summary["errors"].as_u64(),
            summary["axioms"].as_u64(),
            summary["omitted"].as_u64(),
        ),
        (Some(true), Some(0), Some(0), Some(0)),
        "{prefix}: law must close as a real ∀ proof (passed, 0 errors, 0 \
         axioms, 0 omitted).\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_dafny_proves_concat_fold_homomorphism() {
    // The list-induction emitter supplies cons-decomposition bridge asserts
    // for a fold over `concat(<ind-var>, ys)` (here `count`), which is what
    // lets Z3 close `count(n, xs ++ ys) == plus(count n xs, count n ys)` —
    // a goal it times out on without the head/tail hint. Generic over any
    // left-concat (builtin `List.concat` and user wrappers).
    assert_dafny_proves_inline(
        "module ConcatHom\n    effects []\n\n\
         type Nat\n    Z\n    S(Nat)\n\n\
         fn eqNat(a: Nat, b: Nat) -> Bool\n    match a\n        Nat.Z -> match b\n            Nat.Z -> true\n            Nat.S(w) -> false\n        Nat.S(p) -> match b\n            Nat.Z -> false\n            Nat.S(q) -> eqNat(p, q)\n\n\
         fn count(n: Nat, xs: List<Nat>) -> Nat\n    match xs\n        [] -> Nat.Z\n        [h, ..t] -> match eqNat(n, h)\n            true -> Nat.S(count(n, t))\n            false -> count(n, t)\n\n\
         fn plus(a: Nat, b: Nat) -> Nat\n    match a\n        Nat.Z -> b\n        Nat.S(z) -> Nat.S(plus(z, b))\n\n\
         verify count law countConcat\n    given n: Nat = [Nat.Z]\n    given xs: List<Nat> = [[Nat.Z]]\n    given ys: List<Nat> = [[Nat.Z]]\n    plus(count(n, xs), count(n, ys)) => count(n, List.concat(xs, ys))\n",
        "aver-concat-hom",
    );
}

#[test]
fn proof_dafny_proves_additive_monoid_homomorphism() {
    // When the induction variable lands in an additive op's SECOND argument
    // (`plus(length y, length x)`), the emitter hoists the op's right-identity
    // and succ-shift lemmas to quantified facts so Z3 closes the homomorphism.
    // Generic over any additive op / Peano-shaped codomain; the helper lemmas
    // are proved, not trusted.
    assert_dafny_proves_inline(
        "module AddLift\n    effects []\n\n\
         type Nat\n    Z\n    S(Nat)\n\n\
         fn length(xs: List<Int>) -> Nat\n    match xs\n        [] -> Nat.Z\n        [h, ..t] -> Nat.S(length(t))\n\n\
         fn plus(a: Nat, b: Nat) -> Nat\n    match a\n        Nat.Z -> b\n        Nat.S(z) -> Nat.S(plus(z, b))\n\n\
         fn append(xs: List<Int>, ys: List<Int>) -> List<Int>\n    match xs\n        [] -> ys\n        [h, ..t] -> List.concat([h], append(t, ys))\n\n\
         verify length law lenAppend\n    given x: List<Int> = [[1]]\n    given y: List<Int> = [[2]]\n    length(append(x, y)) => plus(length(y), length(x))\n",
        "aver-add-lift",
    );
}

#[test]
fn proof_dafny_proves_length_snoc_with_evaluable_samples() {
    // Two things in one: (1) the `length-snoc` strategy — for a list-length
    // fold the emitter hoists `length(s ++ [e]) == S(length s)` to a ∀-fact,
    // which directly closes the snoc law; (2) the sample-fuel fix — the
    // concrete samples (`length([1, 2, 3]) == S(length([1, 2]))`) only verify
    // because the sample method now carries the same `{:fuel length, 5}` the
    // universal lemma gets (a `function` with `decreases` does not unfold in a
    // bare `assert` otherwise, so the sample would spuriously fail while the
    // universal proves). `passed && axioms:0 && omitted:0` covers both.
    assert_dafny_proves_inline(
        "module LenSnoc\n    effects []\n\n\
         type Nat\n    Z\n    S(Nat)\n\n\
         fn length(x: List<Int>) -> Nat\n    match x\n        [] -> Nat.Z\n        [y, ..xs] -> Nat.S(length(xs))\n\n\
         verify length law snoc\n    given xs: List<Int> = [[1, 2]]\n    given y: Int = [3]\n    length(List.concat(xs, [y])) => Nat.S(length(xs))\n",
        "aver-len-snoc",
    );
}

#[test]
fn proof_dafny_cites_earlier_helper_law() {
    // `rev (rev x) = x` decomposed: the user writes `revDist` (the rev anti-
    // homomorphism over `List.concat`), and the consuming `revRev` proof CITES
    // it — Z3 does not auto-apply lemmas, so the decomposition closes only
    // because `emit_verify_law` injects a `forall`-instantiation of every
    // earlier eligible sibling law into the consumer's body. No synthesized
    // algebra: the helper law IS the decomposition (what The Method conjectures).
    // Over the builtin `List.concat` (Dafny seq `+`, associative natively) the
    // whole chain discharges as a real ∀. (Before #B this closed via a
    // synthesized `rev` algebra template — that content synthesizer is gone.)
    assert_dafny_proves_inline(
        "module RevHom\n    effects []\n\n\
         fn rev(x: List<Int>) -> List<Int>\n    match x\n        [] -> []\n        [y, ..xs] -> List.concat(rev(xs), [y])\n\n\
         verify rev law revDist\n    given x: List<Int> = [[1], [1, 2], [1, 2, 3]]\n    given y: List<Int> = [[4], [4, 5], [4, 5, 6]]\n    rev(List.concat(x, y)) => List.concat(rev(y), rev(x))\n\n\
         verify rev law revRev\n    given x: List<Int> = [[], [1], [1, 2], [1, 2, 3]]\n    rev(rev(x)) => x\n",
        "aver-rev-cite",
    );
}

#[test]
fn proof_dafny_proves_list_accumulator_generalization() {
    // The Dafny counterpart of Lean's `induction ... generalizing acc`: the
    // inductive-hint self-call for a THREADED accumulator must recurse at the
    // value the fold feeds it (`acc + xs[0]`), not the unchanged param. With the
    // self-call at the threaded accumulator, Z3 closes `sumTR(xs, acc) == acc +
    // sumSpec(xs)` as a real universal (and the multiplicative twin over `*`,
    // which Z3's nonlinear arithmetic discharges with the same threaded hint).
    assert_dafny_proves_inline(
        "module SumAccGen\n    effects []\n\n\
         fn sumTR(xs: List<Int>, acc: Int) -> Int\n    match xs\n        [] -> acc\n        [h, ..t] -> sumTR(t, acc + h)\n\n\
         fn sumSpec(xs: List<Int>) -> Int\n    match xs\n        [] -> 0\n        [h, ..t] -> h + sumSpec(t)\n\n\
         verify sumTR law accGeneralizes\n    given xs: List<Int> = [[], [1], [1, 2, 3]]\n    given acc: Int = [0, 5]\n    sumTR(xs, acc) => acc + sumSpec(xs)\n",
        "aver-dafny-sumacc-gen",
    );
}

#[test]
fn proof_dafny_accumulator_hint_handles_mangled_cons_binder() {
    // The threaded-accumulator render substitutes on the DAFNY-rendered binder
    // name, in one pass — so a cons binder whose spelling Dafny mangles (a
    // leading-underscore `_h` becomes `aver_h`) is still re-expressed as
    // `xs[0]`, not left as an out-of-scope identifier. Without the fix the
    // emitted recursive call references `aver_h` and Dafny reports an
    // unresolved identifier.
    assert_dafny_proves_inline(
        "module UHeadAcc\n    effects []\n\n\
         fn sumTR(xs: List<Int>, acc: Int) -> Int\n    match xs\n        [] -> acc\n        [_h, ..t] -> sumTR(t, acc + _h)\n\n\
         fn sumSpec(xs: List<Int>) -> Int\n    match xs\n        [] -> 0\n        [h, ..t] -> h + sumSpec(t)\n\n\
         verify sumTR law accGeneralizes\n    given xs: List<Int> = [[], [1], [1, 2]]\n    given acc: Int = [0, 5]\n    sumTR(xs, acc) => acc + sumSpec(xs)\n",
        "aver-dafny-uhead-acc",
    );
}

#[test]
fn proof_dafny_proves_nat_additive_accumulator_via_algebra_helpers() {
    // User-ADT accumulator-generalization on Dafny (`triTR(n, acc) =>
    // plus(triSpec(n), acc)`): the datatype-induction hint mirrors the fold's
    // `match` on its driver and recurses at the threaded accumulator, while the
    // file's commutativity/associativity helper laws for `plus` (which prove
    // generically) supply the algebra Z3 cannot derive over the opaque ADT. The
    // smart gate only ungates this self-fold because those additive helpers are
    // present.
    assert_dafny_proves_inline(
        "module NatTriAccGen\n    effects []\n\n\
         type Nat\n    Z\n    S(Nat)\n\n\
         fn plus(x: Nat, y: Nat) -> Nat\n    match x\n        Nat.Z -> y\n        Nat.S(z) -> Nat.S(plus(z, y))\n\n\
         fn triTR(n: Nat, acc: Nat) -> Nat\n    match n\n        Nat.Z -> acc\n        Nat.S(m) -> triTR(m, plus(n, acc))\n\n\
         fn triSpec(n: Nat) -> Nat\n    match n\n        Nat.Z -> Nat.Z\n        Nat.S(m) -> plus(n, triSpec(m))\n\n\
         verify plus law plusZeroR\n    given x: Nat = [Nat.Z, Nat.S(Nat.Z), Nat.S(Nat.S(Nat.Z))]\n    plus(x, Nat.Z) => x\n\n\
         verify plus law plusSuccR\n    given x: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    given y: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    plus(x, Nat.S(y)) => Nat.S(plus(x, y))\n\n\
         verify plus law plusComm\n    given a: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    given b: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    plus(a, b) => plus(b, a)\n\n\
         verify plus law plusAssoc\n    given a: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    given b: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    given c: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    plus(plus(a, b), c) => plus(a, plus(b, c))\n\n\
         verify triTR law accGeneralizes\n    given n: Nat = [Nat.Z, Nat.S(Nat.Z), Nat.S(Nat.S(Nat.Z))]\n    given acc: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    triTR(n, acc) => plus(triSpec(n), acc)\n",
        "aver-dafny-nat-tri-accgen",
    );
}

#[test]
fn proof_dafny_nat_accumulator_is_given_order_independent() {
    // The datatype-induction hint reorders the recursive lemma call to the
    // lemma's given order and pins `decreases` to the driver, so the additive
    // accumulator law closes even when the accumulator given is declared BEFORE
    // the driver — not a hard verify error from a wrong-instance / non-
    // terminating recursive call.
    assert_dafny_proves_inline(
        "module NatTriOrder\n    effects []\n\n\
         type Nat\n    Z\n    S(Nat)\n\n\
         fn plus(x: Nat, y: Nat) -> Nat\n    match x\n        Nat.Z -> y\n        Nat.S(z) -> Nat.S(plus(z, y))\n\n\
         fn triTR(n: Nat, acc: Nat) -> Nat\n    match n\n        Nat.Z -> acc\n        Nat.S(m) -> triTR(m, plus(n, acc))\n\n\
         fn triSpec(n: Nat) -> Nat\n    match n\n        Nat.Z -> Nat.Z\n        Nat.S(m) -> plus(n, triSpec(m))\n\n\
         verify plus law plusZeroR\n    given x: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    plus(x, Nat.Z) => x\n\n\
         verify plus law plusSuccR\n    given x: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    given y: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    plus(x, Nat.S(y)) => Nat.S(plus(x, y))\n\n\
         verify plus law plusComm\n    given a: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    given b: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    plus(a, b) => plus(b, a)\n\n\
         verify plus law plusAssoc\n    given a: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    given b: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    given c: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    plus(plus(a, b), c) => plus(a, plus(b, c))\n\n\
         verify triTR law accGen\n    given acc: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    given n: Nat = [Nat.Z, Nat.S(Nat.Z), Nat.S(Nat.S(Nat.Z))]\n    triTR(n, acc) => plus(triSpec(n), acc)\n",
        "aver-dafny-nat-accgen-order",
    );
}

#[test]
fn proof_dafny_nat_accumulator_omits_when_algebra_helpers_not_citable() {
    // The gate ungates a Nat accumulator-generalization only when its
    // commutativity/associativity helpers are CITABLE — earlier in source. With
    // the helpers declared AFTER the accGen law (the citation engine cannot hoist
    // them) the universal must stay sample-only — a clean omission, never a hard
    // verify error from a body missing the algebra it needs.
    if Command::new("dafny").arg("--version").output().is_err() {
        eprintln!("skipping dafny accgen-order omit test: `dafny` not available");
        return;
    }
    let source = "module NatTriAcAfter\n    effects []\n\n\
         type Nat\n    Z\n    S(Nat)\n\n\
         fn plus(x: Nat, y: Nat) -> Nat\n    match x\n        Nat.Z -> y\n        Nat.S(z) -> Nat.S(plus(z, y))\n\n\
         fn triTR(n: Nat, acc: Nat) -> Nat\n    match n\n        Nat.Z -> acc\n        Nat.S(m) -> triTR(m, plus(n, acc))\n\n\
         fn triSpec(n: Nat) -> Nat\n    match n\n        Nat.Z -> Nat.Z\n        Nat.S(m) -> plus(n, triSpec(m))\n\n\
         verify triTR law accGen\n    given n: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    given acc: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    triTR(n, acc) => plus(triSpec(n), acc)\n\n\
         verify plus law plusComm\n    given a: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    given b: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    plus(a, b) => plus(b, a)\n\n\
         verify plus law plusAssoc\n    given a: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    given b: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    given c: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    plus(plus(a, b), c) => plus(a, plus(b, c))\n";
    let summary = crate::lemmas::proof_check_summary(source, "dafny", "aver-dafny-acafter");
    assert_eq!(
        summary["errors"].as_u64(),
        Some(0),
        "a Nat accumulator law whose algebra helpers are declared after it must OMIT, not error\n{summary}"
    );
}

/// A law the Dafny backend DOES export over the map readers has to reach the
/// verifier, which means every name the emitter writes has to resolve.
///
/// `Map.keys` and `Map.values` emit `MapKeys(m)` / `MapValues(m)`, and neither
/// was declared in the Dafny prelude — nor did either name trigger the map
/// helper block, so a program using only these two got no helper block at all
/// and the emitted file failed to resolve before a single obligation was
/// checked. Most of the laws in the fixture read the same sequence on both
/// sides, so what they cannot survive is a missing declaration.
///
/// `aValueForEveryKey` asks for one thing more: a map has as many values as
/// it has keys. That holds because all three readers read one sequence — the
/// prelude defines `MapKeys` and `MapValues` as its two projections — and
/// fails against three unrelated uninterpreted declarations, which would let
/// the verifier work in a world the runtime cannot be in.
///
/// This runs through `--check`, which shells out to `dafny verify`: a
/// resolution failure never prints the verifier summary line, so `--check`
/// exits before emitting JSON and this test fails on the missing JSON line.
#[test]
fn proof_export_of_the_dafny_map_readers_resolves_and_verifies() {
    if Command::new("dafny").arg("--version").output().is_err() {
        eprintln!("skipping: `dafny` not available");
        return;
    }
    let out_dir = temp_output_dir("aver-map-dafny-readers");
    let run = Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(PathBuf::from(env!("CARGO_MANIFEST_DIR")))
        .args([
            "proof",
            "tests/fixtures/map_dafny_readers.av",
            "--backend",
            "dafny",
            "-o",
            out_dir.to_str().expect("utf-8 temp path"),
            "--check",
            "--check-json",
        ])
        .output()
        .expect("expected the `aver` binary to run");
    let stdout = String::from_utf8_lossy(&run.stdout).into_owned();
    let dfy = std::fs::read_to_string(out_dir.join("MapDafnyReaders.dfy")).unwrap_or_default();
    let common = std::fs::read_to_string(out_dir.join("common.dfy")).unwrap_or_default();
    let _ = std::fs::remove_dir_all(&out_dir);

    // The fixture is only a test of the declarations while it actually emits
    // the two calls; if the emitter stopped producing them this would pass
    // saying nothing.
    assert!(
        dfy.contains("MapKeys(m)") && dfy.contains("MapValues(m)"),
        "the fixture must still emit both map readers, or this test is \
         vacuous:\n{dfy}"
    );
    assert!(
        common.contains("function MapKeys<") && common.contains("function MapValues<"),
        "both map readers the emitter writes must be declared in the helper \
         block the file includes:\n{common}"
    );

    let json_line = stdout
        .lines()
        .rev()
        .find(|l| l.starts_with('{'))
        .unwrap_or_else(|| {
            panic!(
                "`aver proof --backend dafny --check --check-json` printed no JSON \
                 summary — Dafny did not get far enough to report one, which is what \
                 an unresolved name looks like:\n{}",
                format_output(&run)
            )
        });
    let summary: serde_json::Value =
        serde_json::from_str(json_line).expect("expected a JSON summary line");
    assert_eq!(
        summary["errors"].as_u64(),
        Some(0),
        "the map reader laws must verify with no errors:\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["passed"].as_bool(),
        Some(true),
        "the map reader laws must verify:\n{}",
        format_output(&run)
    );
}

/// `List.zip` was in the same state the map readers were: the emitter writes
/// `ListZip(xs, ys)`, the list helper block declared no such function, and the
/// name matched none of that block's triggers. `aver proof --backend dafny`
/// reported success and `dafny verify` answered `unresolved identifier:
/// ListZip`.
///
/// Like the map readers test, this runs through `--check`, so a file Dafny
/// cannot resolve never reaches the summary line this reads.
#[test]
fn proof_export_of_list_zip_resolves_and_verifies() {
    if Command::new("dafny").arg("--version").output().is_err() {
        eprintln!("skipping: `dafny` not available");
        return;
    }
    let out_dir = temp_output_dir("aver-list-zip-dafny");
    let run = Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(PathBuf::from(env!("CARGO_MANIFEST_DIR")))
        .args([
            "proof",
            "tests/fixtures/list_zip_dafny.av",
            "--backend",
            "dafny",
            "-o",
            out_dir.to_str().expect("utf-8 temp path"),
            "--check",
            "--check-json",
        ])
        .output()
        .expect("expected the `aver` binary to run");
    let stdout = String::from_utf8_lossy(&run.stdout).into_owned();
    let dfy = std::fs::read_to_string(out_dir.join("ListZipDafny.dfy")).unwrap_or_default();
    let common = std::fs::read_to_string(out_dir.join("common.dfy")).unwrap_or_default();
    let _ = std::fs::remove_dir_all(&out_dir);

    // If the emitter stopped writing the call, the rest of this test would
    // pass saying nothing.
    assert!(
        dfy.contains("ListZip("),
        "the fixture must still emit the zip call, or this test is vacuous:\n{dfy}"
    );
    assert!(
        common.contains("function ListZip<"),
        "the list reader the emitter writes must be declared in the helper block \
         the file includes:\n{common}"
    );

    let json_line = stdout
        .lines()
        .rev()
        .find(|l| l.starts_with('{'))
        .unwrap_or_else(|| {
            panic!(
                "`aver proof --backend dafny --check --check-json` printed no JSON \
                 summary — Dafny did not get far enough to report one, which is what \
                 an unresolved name looks like:\n{}",
                format_output(&run)
            )
        });
    let summary: serde_json::Value =
        serde_json::from_str(json_line).expect("expected a JSON summary line");
    assert_eq!(
        summary["errors"].as_u64(),
        Some(0),
        "the zip laws must verify with no errors:\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["passed"].as_bool(),
        Some(true),
        "the zip laws must verify:\n{}",
        format_output(&run)
    );
}

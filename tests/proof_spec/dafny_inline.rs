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

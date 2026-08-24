use super::*;

#[test]
fn proof_export_builds_law_auto_when_lake_is_available() {
    assert_proof_builds("examples/formal/law_auto.av", "aver-proof-smoke");
}

#[test]
fn proof_export_builds_fibonacci_when_lake_is_available() {
    assert_proof_builds("examples/data/fibonacci.av", "aver-proof-fibonacci");
}

#[test]
fn proof_export_builds_entry_that_shares_function_names_with_dependency() {
    assert_proof_builds(
        "stdlib/crypto/digest32.av",
        "aver-proof-entry-namespace-collision",
    );
}

#[test]
fn proof_export_builds_recursive_process_polling_with_monotonic_oracle() {
    // The hostile profile becomes true only at call index three. If effect
    // lifting restarts the oracle counter at zero on every recursive call,
    // Lean's native evaluator diverges here instead of completing the build.
    assert_proof_builds(
        "examples/formal/process_stop_requested.av",
        "aver-proof-process-stop-requested",
    );
}

#[test]
fn dafny_export_accepts_recursive_process_polling_boundary() {
    // Monotonic does not mean eventually true: stopNever is valid. Dafny must
    // therefore keep the recursive loop opaque (matching Lean's partial def)
    // while still checking the emitted cross-call oracle predicate.
    assert_dafny_verifies_and_passes(
        "examples/formal/process_stop_requested.av",
        "aver-dafny-process-stop-requested",
    );
}

#[test]
fn proof_export_builds_log_line_length_when_lake_is_available() {
    // Pure builtin String-length additivity (`String.len(a + b) =
    // String.len(a) + String.len(b)`) on a life-like log-line byte-budget
    // example. The law mentions no user fn, so it stays `BackendDispatch`;
    // the Lean-only `emit_string_length_additive_law` rung closes it as a
    // universal. Sorry budget 0 — the rung must fully discharge it (revert
    // the rung and this example regresses to a caught sorry).
    assert_proof_builds_with_sorry_budget(
        "examples/formal/log_line_length.av",
        "aver-proof-log-line-length",
        0,
    );
}

#[test]
fn proof_export_builds_string_concat_monoid_when_lake_is_available() {
    // Pure builtin String-concat monoid identities (`s + "" = s`,
    // `"" + s = s`, `(a + b) + c = a + (b + c)`). The shape-driven
    // `emit_string_append_monoid_law` rung closes all three as universals;
    // Dafny's Z3 already proves them. Sorry budget 0 — revert the rung and
    // each law regresses to a bare sorry.
    assert_proof_builds_with_sorry_budget(
        "examples/formal/string_concat_monoid.av",
        "aver-proof-string-concat-monoid",
        0,
    );
}

#[test]
fn proof_export_builds_empty_map_facts_when_lake_is_available() {
    // Pure builtin empty-map facts (`Map.get(empty, k) = None`,
    // `Map.has(empty, k) = false`, `Map.len(empty) = 0`). The empty-map-precise
    // `emit_map_empty_fact_law` rung closes all three as universals; Dafny's Z3
    // already proves them. Sorry budget 0 — revert the rung and each regresses
    // to a caught sorry.
    assert_proof_builds_with_sorry_budget(
        "examples/formal/empty_map_facts.av",
        "aver-proof-empty-map-facts",
        0,
    );
}

#[test]
fn proof_export_builds_int_comparison_laws_when_lake_is_available() {
    // Bool-valued Int comparison identities (equality symmetry, `!(a < b) =
    // (a >= b)`, totality of `<=`). The `wrapper_return` arm's sign-split now
    // falls through to a comparison normaliser + `sorry` floor, closing these
    // as universals; Dafny's Z3 already proves them. Sorry budget 0 — revert
    // and each hard-fails the Lean build.
    assert_proof_builds_with_sorry_budget(
        "examples/formal/int_comparison_laws.av",
        "aver-proof-int-comparison-laws",
        0,
    );
}

#[test]
fn proof_export_builds_int_abs_laws_when_lake_is_available() {
    // Pure builtin `Int.abs` identities (idempotence, multiplicativity,
    // non-negativity). The `emit_int_abs_identity_law` rung closes all three
    // as universals via the natAbs/cast lemmas; Dafny's Z3 already proves
    // them. Sorry budget 0 — revert and idempotence/non-neg hard-fail the
    // build and multiplicativity sorries.
    assert_proof_builds_with_sorry_budget(
        "examples/formal/int_abs_laws.av",
        "aver-proof-int-abs-laws",
        0,
    );
}

#[test]
fn proof_export_builds_bits_laws_when_lake_is_available() {
    // The `Bits` proof model is a DEFINITION, not an assumption: Lean's
    // `AverBits` prelude builds the pointwise operations from `Nat` bitwise
    // ops on the two's-complement magnitude, and the shifts are the
    // specification written out (`x * 2^n`, `x / 2^n`, `x % 2^w`). Sorry
    // budget 0 — if the model ever regressed to something opaque, these three
    // universals would stop closing and land here as sorries rather than
    // quietly passing on the sampled rows alone.
    assert_proof_builds_with_sorry_budget(
        "examples/formal/bits_laws.av",
        "aver-proof-bits-laws",
        0,
    );
}

#[test]
fn proof_dafny_verifies_bits_laws_when_dafny_is_available() {
    // The same three universals through Z3. Dafny has no bitwise operators on
    // `int` at all, so this also pins that the prelude's recursive definitions
    // TERMINATE and that `BitsPow2(n) > 0` discharges the division-by-zero
    // obligation `shiftRight` / `low` would otherwise carry.
    assert_dafny_verifies("examples/formal/bits_laws.av", "aver-dafny-bits-laws");
}

#[test]
fn proof_export_failsoft_sos_probe_a_degrades_to_sorry_when_lake_is_available() {
    // Regression for the fail-soft floor: the guarded nonlinear nonnegativity
    // certificate can leave the fragment, but it must degrade to a counted
    // `sorry`, never a hard Lean build error from inside the tactic portfolio.
    assert_proof_builds_with_sorry_budget(
        "tests/fixtures/failsoft_sos_probe_a.av",
        "aver-proof-failsoft-sos-a",
        2,
    );
}

#[test]
fn proof_export_failsoft_sos_probe_b2_degrades_to_sorry_when_lake_is_available() {
    // Regression for the fail-soft floor: this non-strict product certificate
    // used to hit a deterministic whnf heartbeat timeout in `aver_int_order`.
    // The divergence source was the anonymous `_ ∧ _` conjunction split, now
    // narrowed to the named `h_when` guard, so the `mul_le_mul` arm no longer
    // feeds spurious metavariable products; the goal falls through to a counted
    // `sorry` instead of aborting `lake build`. NOTE: the whnf-timeout class is
    // NOT catchable at tactic level (a tactic-level `maxHeartbeats` cap is
    // inert — see prelude.rs); this test passes because the divergence source
    // is gone, not because the arm is bounded.
    assert_proof_builds_with_sorry_budget(
        "tests/fixtures/failsoft_sos_probe_b2.av",
        "aver-proof-failsoft-sos-b2",
        1,
    );
}

#[test]
fn proof_export_builds_validated_wrapper_when_lake_is_available() {
    // Genericity guard for the validated-wrapper arm: a synthetic, non-K5
    // error-checking division wrapper whose correctness law
    // (`checkedDiv(a, b) => Result.Ok(coreDiv(a, b))` when the divisor is
    // nonzero) closes UNIVERSALLY by the same structural arm that proves the
    // K5 divider's Theorem 2 — unfold only the wrapper, let the premise pick
    // the Ok branch, and `rfl` on the shared `coreDiv a b` (never unfolding
    // `coreDiv`). Sorry budget 0 — revert the `emit_validated_wrapper_law`
    // arm and this regresses to a caught sorry. Proves the arm is keyed on
    // STRUCTURE, not on `divide`/`divideBang`/K5 names.
    assert_proof_builds_with_sorry_budget(
        "examples/formal/validated_wrapper_law.av",
        "aver-proof-validated-wrapper",
        0,
    );
}

#[test]
fn proof_export_builds_affine_wrapper_when_lake_is_available() {
    // Genericity guard for the validated-wrapper arm's AFFINE closer: a
    // synthetic, non-K5 wrapper (`boundedScale(a, lo, hi) =>
    // Result.Ok(scaleCore(a))`) whose dispatch branches on an ARITHMETIC
    // field-bounds check `inWindow(scaleCore(a))`, not a premise-pinned Bool.
    // It closes UNIVERSALLY by the same structural arm that proves K5 Theorem 2
    // — unfold the wrapper and the DERIVED guard `inWindow`, derive the field
    // check by `omega` from the premised value bracket, and `rfl` on the shared
    // `scaleCore a` (never unfolding it). Sorry budget 0 — revert the
    // `emit_validated_wrapper_law` arm's `omega` closer and this regresses to a
    // caught sorry. Proves the affine extension is domain-blind: `omega` only
    // ever closes affine guards, on K5 and non-K5 wrappers alike.
    assert_proof_builds_with_sorry_budget(
        "examples/formal/affine_wrapper_law.av",
        "aver-proof-affine-wrapper",
        0,
    );
}

#[test]
fn proof_export_builds_map_set_nonempty_when_lake_is_available() {
    // `Map.len(Map.set(m, k, v)) >= 1` — set yields a non-empty map. Needs
    // induction (the hand-proved prelude lemma `AverMap.len_set_ge_one`); the
    // `emit_map_len_set_positive_law` rung discharges it. Sorry budget 0 —
    // revert and the law regresses to a bare sorry. Dafny's Z3 proves it.
    assert_proof_builds_with_sorry_budget(
        "examples/formal/map_set_nonempty.av",
        "aver-proof-map-set-nonempty",
        0,
    );
}

#[test]
fn proof_export_builds_frac_monotone_geone_flip_when_lake_is_available() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping frac-monotone ge-one flip proof test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let output_dir = temp_output_dir("aver-proof-frac-geone-flip");
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("tests/fixtures/frac_monotone_geone_flip/domain/fprep.av")
        .arg("--module-root")
        .arg("tests/fixtures/frac_monotone_geone_flip")
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&output_dir)
        .arg("--check")
        .arg("--check-json")
        .arg("--sorry-budget")
        .arg("0")
        .output()
        .expect("expected frac-monotone ge-one flip proof to run");
    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)));
    let summary: serde_json::Value =
        serde_json::from_str(json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}"));
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(0),
        "{}",
        format_output(&run)
    );
    assert_eq!(
        summary["passed"].as_bool(),
        Some(true),
        "{}",
        format_output(&run)
    );
    assert_eq!(
        (
            summary["universal"].as_bool(),
            summary["universal_laws"].as_u64(),
            summary["bounded_laws"].as_u64(),
        ),
        (Some(true), Some(2), Some(0)),
        "recursive positivity plus flipped ge-one should both be universal.\n{}",
        format_output(&run)
    );

    let lean = std::fs::read_to_string(output_dir.join("Fprep.lean"))
        .expect("expected generated Fprep.lean");
    assert!(
        lean.contains("-- aver:law-class pow2SignedAtLeastOne_law_geOneFlip universal"),
        "geOneFlip must be classified universal after canonicalizing `when 0 <= k`.\n{lean}"
    );
    assert!(
        lean.contains("-- when (0 <= k)"),
        "fixture must preserve the flipped source spelling in emitted comments.\n{lean}"
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

#[test]
fn proof_dafny_verifies_fibonacci_when_dafny_is_available() {
    // `goldenApprox(n)` divides `Float.fromInt(fib(n + 1))` by
    // `Float.fromInt(fib(n))`. Float `/` lowers via the `FloatDiv`
    // helper which mirrors Aver's IEEE-754 "no crash, b == 0 yields
    // a defined value" semantics, so there's no division-by-zero
    // obligation on the caller — the rest of the proof closes.
    assert_dafny_verifies("examples/data/fibonacci.av", "aver-dafny-fibonacci");
}

#[test]
fn proof_dafny_verifies_sum_acc_when_dafny_is_available() {
    // Stage 8 of #232: `ProofStrategy::WrapperOverRecursion` closes
    // the `sum(xs) == sumDirect(xs)` law on the `sum_acc` example by
    // emitting an accumulator-decomposition aux lemma plus the main
    // universal lemma. Both close in Z3 via list induction —
    // demonstrates the first real consumer of the
    // `analysis::shape::ModulePattern::WrapperOverRecursion` typed
    // pattern. Regression guard: if a future change disables the
    // strategy or breaks the aux template, this lemma falls back to
    // naive induction and Dafny reports 1 error.
    assert_dafny_verifies("examples/data/sum_acc.av", "aver-dafny-sum-acc");
}

#[test]
fn proof_export_builds_sum_acc_when_lake_is_available() {
    // Lean template for `WrapperOverRecursion` emits the aux
    // accumulator-decomposition theorem + main universal lemma; both
    // close in core Lean 4 (`omega`) without Mathlib. Sorry budget
    // 0 — the strategy fully closes the universal proof.
    assert_proof_builds_with_sorry_budget("examples/data/sum_acc.av", "aver-proof-sum-acc", 0);
}

#[test]
fn proof_dafny_verifies_fact_acc_when_dafny_is_available() {
    // The multiplicative Peano-`Nat` twin of `sum_acc`:
    // `WrapperOverRecursion` proves a tail-recursive factorial equals its
    // recurrence. Z3 has no free associativity/commutativity for the user
    // `mul` (a function over the `Nat` datatype, not `int`), so the support
    // stack supplies the `plus`/`mul` monoid lemmas by induction before the
    // accumulator-decomposition + main lemmas. Regression guard: disable the
    // strategy or break a monoid lemma and `dafny verify` reports errors.
    assert_dafny_verifies(
        "proof-corpus/handwritten/fact_acc_spec.av",
        "aver-dafny-fact-acc",
    );
}

#[test]
fn proof_export_builds_fact_acc_when_lake_is_available() {
    // Lean side of the same multiplicative case: the decomposition lemma
    // `factTR n acc = mul (factTR n 1) acc` + the main law, closing the
    // nonlinear residual with the user-monoid→`Nat.*` bridges and the core
    // `Nat.mul_*` lemmas (no Mathlib). Sorry budget 0.
    assert_proof_builds_with_sorry_budget(
        "proof-corpus/handwritten/fact_acc_spec.av",
        "aver-proof-fact-acc",
        0,
    );
}

#[test]
fn proof_dafny_verifies_list_prod_when_dafny_is_available() {
    // List + Mul corner of the accumulator-fold grid. Z3 knows `int` `*` is
    // associative/commutative for free, so the `seq<int>` support stack closes
    // with no extra monoid lemmas — but the strategy must still fire and emit
    // the decomposition + main lemmas. Regression guard for the `List` driver
    // under the multiplicative combine.
    assert_dafny_verifies(
        "proof-corpus/handwritten/list_prod_spec.av",
        "aver-dafny-list-prod",
    );
}

#[test]
fn proof_dafny_verifies_nat_tri_when_dafny_is_available() {
    // Nat + Add corner. The user `plus` is a function over the `Nat` datatype,
    // so Z3 needs the `plus` monoid lemmas (zero/succ/comm/assoc) supplied by
    // induction before the additive accumulator-decomposition + main lemmas —
    // the additive sibling of the factorial support stack. Regression guard
    // that the Peano-`Nat` support stack covers the additive combine, not just
    // the multiplicative one.
    assert_dafny_verifies(
        "proof-corpus/handwritten/nat_tri_spec.av",
        "aver-dafny-nat-tri",
    );
}

#[test]
fn proof_dafny_verifies_aliased_peano_when_dafny_is_available() {
    // A Peano natural named other than `Nat` (`Num`). Dafny names the datatype
    // directly (no builtin-`Nat` lift needed), so the factorial support stack
    // verifies over `Num` exactly as over `Nat`. Pairs with the Lean
    // `proof_lean_lifts_aliased_peano_type_to_nat_and_proves_universal` guard.
    assert_dafny_verifies(
        "proof-corpus/handwritten/peano_aliased_spec.av",
        "aver-dafny-peano-aliased",
    );
}

#[test]
fn proof_dafny_verifies_list_length_fold_when_dafny_is_available() {
    // Two structural list folds (`1 + length(t)` vs `length(t) + 1`)
    // closing by induction on `xs`. The generic structural-induction
    // path verifies this shape on both backends (the bespoke
    // `MatchDispatcherFold` strategy that used to pin it was removed as
    // redundant — the generic driver subsumes it).
    assert_dafny_verifies(
        "examples/data/list_length_fold.av",
        "aver-dafny-list-length-fold",
    );
}

#[test]
fn proof_export_builds_list_length_fold_when_lake_is_available() {
    // The generic list-induction ladder closes it: `induction xs with
    // | nil => simp | cons => simp_all; omega`, the omega discharge
    // handling `1 + x = x + 1`. Sorry budget 0.
    assert_proof_builds_with_sorry_budget(
        "examples/data/list_length_fold.av",
        "aver-proof-list-length-fold",
        0,
    );
}

#[test]
fn proof_dafny_verifies_result_chain_when_dafny_is_available() {
    // Stage 8b of #232: `ProofStrategy::ResultPipelineChain` closes
    // `chainQM(n) == chainManual(n)` — `?`-propagating Result chain
    // vs nested `match Result.Err -> Err` chain. Both unfold to the
    // same tree; Z3 closes by structural equality with the right
    // fuel + unfold list. Second real consumer of a typed
    // `ModulePattern` in proof_lower.
    assert_dafny_verifies("examples/core/result_chain.av", "aver-dafny-result-chain");
}

#[test]
fn proof_export_builds_result_chain_when_lake_is_available() {
    // Lean template: `unfold` + `repeat (first | split | rfl) ;
    // all_goals simp_all`. Generic over step count — works for any
    // ResultPipelineChain with arbitrary number of step fns.
    assert_proof_builds_with_sorry_budget(
        "examples/core/result_chain.av",
        "aver-proof-result-chain",
        0,
    );
}

#[test]
fn proof_export_builds_rle_when_lake_is_available() {
    // The encode/decode roundtrip laws are list-given, so #409 attempts Lean
    // list-induction — but `encode` threads an accumulator (`encodeLoop`) so a
    // plain `induction xs` IH does not align; both roundtrips fall to an honest
    // `sorry` (per-arm `first | (simp_all; done) | sorry`, which BUILDS). The
    // earlier #409 revision claimed 1 here, a false green — the tactic left
    // unsolved goals that `lake build` rejects but the sorry-count metric was
    // blind to (fixed in commands.rs to gate on lake's exit status).
    assert_proof_builds_with_sorry_budget("examples/data/rle.av", "aver-proof-rle", 2);
}

#[test]
fn proof_dafny_verifies_rle_when_dafny_is_available() {
    // Three postcondition gaps on the encode/decode roundtrip shape
    // (one universal lemma, one sample assertion, one
    // `decodeString` universal). Z3 can't auto-discharge them
    // without a richer list-induction tactic the lowerer doesn't
    // emit yet. Tracked in issue #114.
    assert_dafny_verifies_with_budgets("examples/data/rle.av", "aver-dafny-rle", 3, 0);
}

#[test]
fn proof_export_builds_quicksort_when_lake_is_available() {
    // `sort` / `sortWithPivot` now emit as a genuine well-founded `mutual`
    // block: the computed-arg partition recursion's termination is discharged
    // by synthesised, kernel-proved `smallerOrEqual_len_le` /
    // `greaterThan_len_le` length-monotonicity lemmas (termination-as-a-law,
    // no fuel / no `partial` / `#print axioms` = [propext, Quot.sound]).
    // The three list-given BEHAVIORAL laws (`sort.resultOrdered` /
    // `sort.lengthPreserved` / `sort.idempotent`) are attempted via #409 Lean
    // list-induction, but `simp_all` cannot reduce the partition recursion, so
    // each still falls to an honest `sorry` that BUILDS (per-arm
    // `first | (simp_all; done) | sorry`). An earlier #409 revision reported 0
    // here — a FALSE GREEN: the tactic left unsolved goals that `lake build`
    // rejects (exit 1), which the `declaration uses 'sorry'` count metric was
    // blind to. Fixed: commands.rs gates pass on lake's exit status, so this
    // budget is now the build-verified count. (Native universal closure for the
    // partition SCC is the #125 native-decreases epic, not reachable here.)
    assert_proof_builds_with_sorry_budget("examples/data/quicksort.av", "aver-proof-quicksort", 3);
}

#[test]
fn proof_dafny_verifies_quicksort_when_dafny_is_available() {
    // Recursive postcondition gaps on `sort.resultOrdered` /
    // `sort.lengthPreserved` / `sort.idempotent`. Sample-domain
    // theorems still hold for ordered/length-preserved; idempotent
    // sample assertions trip the same mutual-recursion / fuel issue
    // tracked in #76 — sort(sort([..])) cannot unfold under Z3's
    // budget without explicit `reveal`. Budget grew from 5 → 8 when
    // `sort.idempotent` landed in #220 (three sample inputs ×
    // one postcondition each). Tracked in issue #114 / #76. The budget
    // is a CEILING: macOS Z3 leaves 8 undischarged, Linux CI 9 (one extra
    // assertion whose counterexample model Z3 can't parse, #342).
    assert_dafny_verifies_with_budgets("examples/data/quicksort.av", "aver-dafny-quicksort", 9, 3);
}

#[test]
fn proof_export_builds_json_when_lake_is_available() {
    // 13 sampled-domain laws (parseString / parseLiteral / escape
    // roundtrips) hit the universal-not-auto-proved fallback in
    // `lean::toplevel` and emit `theorem ... := by sorry`. The
    // per-sample `_sample_N` theorems below them still verify the
    // claim on the declared domain — those are the meaningful
    // coverage. Budget gates regressions: if the count climbs, a
    // new shape lost a strategy and broke the law; if it drops,
    // someone gave one of these a real strategy and the budget
    // should be tightened.
    // Issue #128: dropped from 13 to 9 — the new singleton-given +
    // constant-RHS gate elides 4 universal-with-sorry shapes whose
    // ∀ form was vacuous (or false). Per-sample lemmas still cover
    // the declared domain.
    // FiniteDomainCases: 8 → 7 — `parseLiteral.boolRoundtrip` now
    // closes genuinely (`intro b; cases b <;> rfl`-class cascade,
    // #print axioms = [propext, Quot.sound]); the two new EscapeCode
    // laws (`parseEscape.escapeCodeRoundtrip`,
    // `escapeJsonChar.encodesEscapeCode`) close axiom-free via the
    // same strategy and add no sorries.
    // SimpOverPreludeLemmas: 7 → 3 — `finishInt.fromCanonicalInt`,
    // `finishNumber.fromCanonicalIntSlice`,
    // `afterIntChar.terminatedIntRoundtrip` and
    // `finishString.plainSegmentRoundtrip` now close genuinely via
    // the String prelude spec lemmas (`String.add_eq_append`,
    // `String.slice_full` / `String.slice_append_prefix`,
    // `String.intercalate_singleton`) + `Int.fromString_fromInt`
    // (#print axioms ⊆ [propext, Classical.choice, Quot.sound]).
    // IntDecimalRoundtrip: 3 → 2 — `parseNumber.fromIntRoundtrip`
    // now closes genuinely (#print axioms = [propext,
    // Classical.choice, Quot.sound]): the synthesized
    // `scanIntTail__fuel_scan` lemma runs the symbolic all-digit
    // suffix through the fuel barrier, and the fixed sign-split
    // skeleton (serializer `rfl`, `String.mk`-form head-char
    // dispatch, `digitChar` disequalities, `Int.fromString_fromInt`
    // leaf) closes both sign branches.
    // StringEscapeRoundtrip: 2 → 0 — the parser-workhorse pair
    // (`escapeJsonString.parseStringRoundtrip` and
    // `parseStringChunk.escapedStringRoundtrip`) now closes genuinely
    // (#print axioms = [propext, Quot.sound]): the detector validates
    // the escape producer/consumer pair end-to-end and the Lean
    // backend renders the suffix-invariant skeleton ported from the
    // verified hand proof — drop-form suffix cursor, accumulator
    // homomorphism over the escape fold, one step lemma per consumer
    // fuel arm, and a chunk invariant quantifying the carried scanner
    // state (segmentStart, chunks), closed by per-char classification
    // over the pinned escape table. The whole json example now builds
    // sorry-free; every synthesized lemma carries a
    // `first | (…; done) | sorry` floor, so a template regression
    // surfaces HERE as a loud count drift, never a build error.
    assert_proof_builds_with_sorry_budget("examples/data/json.av", "aver-proof-json", 0);
}

#[test]
fn proof_clique_cursor_monotonicity_is_universal_cross_domain() {
    // Second-witness litmus for the clique cursor-monotonicity rung: a
    // mutually-recursive bracket scanner in a DIFFERENT domain than JSON (its own
    // `PResult` result type, `Int` value component, distinct function names, a
    // result-carried semantic edge). The rung is keyed only on the AST shape, so
    // `scanExpr.scanExprAdvances` (`when scanExpr(s, pos) == PResult.POk(v, p) ->
    // p >= pos`) must close UNIVERSALLY through the rank-slotted `induction fuel`
    // conjunction + fuel-wrapper projection — no name/type is matched.
    //
    // The file also witnesses the CITATION half: `scanSeq` is a second clique
    // whose cursor cone reaches OUTSIDE itself — the inner `scanExpr` clique (a
    // sub-parser) and a `skipWs` whitespace helper. Its advance law flips
    // universal ONLY by CITING the pool laws `scanExprAdvances` and
    // `skipWsAdvances`; the emitter discharges the conjunction's external
    // hypotheses from those theorems. `skipWs` is recognized as a monotone-cursor
    // helper because a `>= q` pool law exists about it, not because of its name.
    // All three close universally, kernel-clean (the #print-axioms audit gates
    // `universal == true`, so a `native_decide`/`sorry` sneaking in fails here).
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping clique cursor-monotonicity test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-bracket-scan");
    let (summary, run) = run_lean_check_json("examples/data/bracket_scan.av", &output_dir, 0, &[]);
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(0),
        "the clique conjunction, its projection, and the cited-hypothesis \
         reconstruction must all close.\n{}",
        format_output(&run)
    );
    assert_eq!(summary["passed"].as_bool(), Some(true));
    assert_eq!(
        summary["universal"].as_bool(),
        Some(true),
        "cursor-monotonicity must be kernel-genuine (no native_decide).\n{}",
        format_output(&run)
    );
    assert_eq!(
        (
            summary["universal_laws"].as_u64(),
            summary["bounded_laws"].as_u64(),
        ),
        (Some(3), Some(0)),
        "self-contained clique (scanExpr), monotone-cursor scanner (skipWs), and \
         the CITING clique (scanSeq, which cites both) must all certify \
         universal via the rung.\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

#[test]
fn proof_dafny_verifies_json_when_dafny_is_available() {
    // Structural shape limits: deeply-nested ADT roundtrip
    // postconditions blow past what Dafny can auto-discharge. The
    // large budget exists so a regression *upward* is still caught;
    // closing this cleanly is probably out of scope for a single
    // fix per issue #114, and would need a different proof
    // strategy entirely.
    // 89 → 91 errors / 16 → 17 axioms: pure corpus delta from the
    // EscapeCode swap (the FiniteDomainCases strategy itself is
    // Lean-only; Dafny treats it as BackendDispatch). The 4 deleted
    // mislabeled dummy laws each had ONE failing parseEscape sample
    // lemma (-4); `parseEscape.escapeCodeRoundtrip` adds 5 sample
    // lemmas in the same Z3-can't-compute-parseEscape class (+5) and
    // its universal becomes the standard fuel-bounded
    // `assume {:axiom}` (+1 axiom); `escapeJsonChar.encodesEscapeCode`
    // gets a real universal lemma attempt Z3 can't discharge (+1).
    assert_dafny_verifies_with_budgets("examples/data/json.av", "aver-dafny-json", 91, 17);
}

#[test]
fn proof_export_builds_grok_s_language_when_lake_is_available() {
    assert_proof_builds("examples/core/grok_s_language.av", "aver-proof-grok");
}

#[test]
fn proof_export_builds_pure_question_bang_when_backends_are_available() {
    let source = "module Prog\n\
        \x20   intent = \"stress pure ?! proof export\"\n\
        \n\
        fn okOne() -> Result<Int, String>\n\
        \x20   ? \"one\"\n\
        \x20   Result.Ok(1)\n\
        \n\
        fn okTwo() -> Result<Int, String>\n\
        \x20   ? \"two\"\n\
        \x20   Result.Ok(2)\n\
        \n\
        fn errLeft() -> Result<Int, String>\n\
        \x20   ? \"left error\"\n\
        \x20   Result.Err(\"left\")\n\
        \n\
        fn errRight() -> Result<Int, String>\n\
        \x20   ? \"right error\"\n\
        \x20   Result.Err(\"right\")\n\
        \n\
        fn pairOk() -> Result<Tuple<Int, Int>, String>\n\
        \x20   ? \"unwrap two successful Result branches\"\n\
        \x20   pair = (okOne(), okTwo())?!\n\
        \x20   match pair\n\
        \x20       (a, b) -> Result.Ok((a, b))\n\
        \n\
        fn pairErr() -> Result<Tuple<Int, Int>, String>\n\
        \x20   ? \"propagate the leftmost Result.Err\"\n\
        \x20   pair = (errLeft(), errRight())?!\n\
        \x20   match pair\n\
        \x20       (a, b) -> Result.Ok((a, b))\n\
        \n\
        verify pairOk\n\
        \x20   pairOk() => Result.Ok((1, 2))\n\
        \n\
        verify pairErr\n\
        \x20   pairErr() => Result.Err(\"left\")\n";

    let dir = temp_output_dir("aver-proof-pure-qbang");
    std::fs::create_dir_all(&dir).expect("create temp dir");
    std::fs::write(
        dir.join("aver.toml"),
        "[independence]\nmode = \"complete\"\n",
    )
    .expect("write aver.toml");
    std::fs::write(dir.join("program.av"), source).expect("write program.av");
    let aver_bin = env!("CARGO_BIN_EXE_aver");

    if Command::new("lake").arg("--version").output().is_ok() {
        let lean_dir = dir.join("lean");
        let proof = Command::new(aver_bin)
            .current_dir(&dir)
            .arg("proof")
            .arg("program.av")
            .arg("--backend")
            .arg("lean")
            .arg("--verify-mode")
            .arg("auto")
            .arg("-o")
            .arg(&lean_dir)
            .output()
            .expect("expected `aver proof --backend lean` to run");
        assert!(
            proof.status.success(),
            "Lean proof export failed:\n{}",
            format_output(&proof)
        );

        let build = Command::new("lake")
            .current_dir(&lean_dir)
            .arg("build")
            .output()
            .expect("expected `lake build` to run");
        assert!(
            build.status.success(),
            "Lean pure ?! proof build failed:\n{}",
            format_output(&build)
        );
    }

    if Command::new("dafny").arg("--version").output().is_ok() {
        let dafny_dir = dir.join("dafny");
        let proof = Command::new(aver_bin)
            .current_dir(&dir)
            .arg("proof")
            .arg("program.av")
            .arg("--backend")
            .arg("dafny")
            .arg("--verify-mode")
            .arg("auto")
            .arg("-o")
            .arg(&dafny_dir)
            .output()
            .expect("expected `aver proof --backend dafny` to run");
        assert!(
            proof.status.success(),
            "Dafny proof export failed:\n{}",
            format_output(&proof)
        );

        let verify = Command::new("dafny")
            .current_dir(&dafny_dir)
            .arg("verify")
            .arg("Prog.dfy")
            .output()
            .expect("expected `dafny verify` to run");
        assert!(
            verify.status.success(),
            "Dafny pure ?! proof verification failed:\n{}",
            format_output(&verify)
        );
    }

    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn proof_dafny_verifies_law_auto_when_dafny_is_available() {
    assert_dafny_verifies("examples/formal/law_auto.av", "aver-dafny-law-auto");
}

#[test]
fn proof_dafny_verifies_spec_laws_when_dafny_is_available() {
    assert_dafny_verifies("examples/formal/spec_laws.av", "aver-dafny-spec-laws");
}

#[test]
fn proof_dafny_verifies_oracle_independent_products_when_dafny_is_available() {
    assert_dafny_verifies(
        "examples/formal/oracle_independent_products.av",
        "aver-dafny-oracle-products",
    );
}

#[test]
fn proof_dafny_verifies_map_when_dafny_is_available() {
    assert_dafny_verifies("examples/data/map.av", "aver-dafny-map");
}

// --- expanded coverage (post-IR-migration audit, 0.22.0) ---

#[test]
fn proof_export_builds_clock_as_data_when_lake_is_available() {
    assert_proof_builds("examples/formal/clock_as_data.av", "aver-proof-clock");
}

#[test]
fn proof_dafny_verifies_clock_as_data_when_dafny_is_available() {
    assert_dafny_verifies("examples/formal/clock_as_data.av", "aver-dafny-clock");
}

#[test]
fn proof_export_builds_file_store_pure_core_when_lake_is_available() {
    assert_proof_builds(
        "examples/formal/file_store_pure_core.av",
        "aver-proof-file-store-pure",
    );
}

#[test]
fn proof_dafny_verifies_file_store_pure_core_when_dafny_is_available() {
    assert_dafny_verifies(
        "examples/formal/file_store_pure_core.av",
        "aver-dafny-file-store-pure",
    );
}

#[test]
fn proof_export_builds_oracle_trace_when_lake_is_available() {
    assert_proof_builds("examples/formal/oracle_trace.av", "aver-proof-oracle-trace");
}

#[test]
fn proof_dafny_verifies_oracle_trace_when_dafny_is_available() {
    assert_dafny_verifies("examples/formal/oracle_trace.av", "aver-dafny-oracle-trace");
}

#[test]
fn proof_export_builds_terminal_size_snapshot_when_lake_is_available() {
    assert_proof_builds(
        "examples/formal/terminal_size_snapshot.av",
        "aver-proof-terminal-size",
    );
}

#[test]
fn proof_dafny_verifies_terminal_size_snapshot_when_dafny_is_available() {
    assert_dafny_verifies(
        "examples/formal/terminal_size_snapshot.av",
        "aver-dafny-terminal-size",
    );
}

#[test]
fn proof_export_builds_trust_check_when_lake_is_available() {
    assert_proof_builds("examples/formal/trust_check.av", "aver-proof-trust-check");
}

#[test]
fn proof_dafny_verifies_trust_check_when_dafny_is_available() {
    assert_dafny_verifies("examples/formal/trust_check.av", "aver-dafny-trust-check");
}

#[test]
fn proof_export_builds_date_when_lake_is_available() {
    assert_proof_builds("examples/data/date.av", "aver-proof-date");
}

#[test]
fn proof_dafny_verifies_date_when_dafny_is_available() {
    // `parseIntSlice(s, from, to)` is emitted via the safe
    // `StringSlice` helper instead of raw `s[from..to]`, so the
    // slice carries Aver's clamp-to-empty semantics into Dafny and
    // there's no range obligation to discharge in the caller.
    assert_dafny_verifies("examples/data/date.av", "aver-dafny-date");
}

#[test]
fn proof_dafny_verifies_discharged_div_law_when_dafny_is_available() {
    // Dafny side of the literal-divisor discharge: the discharged
    // `Int.div(a, 2)` / `Int.mod(a, 2)` render as bare Euclidean `/` and
    // `%` (Dafny int division is Euclidean — same rounding as the runtime
    // on every sign), and all four laws verify with no `Result`
    // scaffolding: the doubling identity, the quotient/remainder rebuild
    // (the discharged-`mod` arm), the negative-literal-divisor law, and
    // the mixed discharged + dynamic-divisor law, which is the one that
    // forces Z3 to reason about a bare `/` and a `Result` datatype sitting
    // in the SAME function body.
    assert_dafny_verifies(
        "tests/fixtures/discharged_div_law.av",
        "aver-dafny-discharged-div-law",
    );
}

#[test]
fn proof_export_verifies_discharged_div_law_when_lake_is_available() {
    // Literal-divisor discharge, proof side
    // (tests/fixtures/discharged_div_law.av): `half(a) = Int.div(a, 2)`
    // types as plain Int and the Lean backend renders the discharged call
    // as the bare Euclidean `(a / 2)` (Lean `/` on Int = Int.ediv — same
    // rounding as the runtime on every sign); `Int.mod` likewise renders
    // as bare `%`. All four laws must CERTIFY (budget 0), because each
    // unfold cone is pure user fns plus core ediv/emod terms, squarely
    // inside omega's theory:
    //   - `doubleThenHalf(a) => a`         — the discharged quotient;
    //   - `rebuild(a) => a`                — 2*(a/2) + a%2, the discharged
    //                                        `mod` arm;
    //   - `negThird(a) => 0 - third(a)`    — a NEGATIVE literal divisor;
    //   - `mixedQuotients(a) => a + dynamicQuotient(a, 2)` — the
    //     emission-correctness pin. A discharged divisor and a
    //     dynamic-divisor `Result` call share one body; the law closes
    //     only if the discharged call became a bare Euclidean operator the
    //     arithmetic tactic can crunch WHILE the dynamic call stayed an
    //     opaque `Except` term. Cross-contamination in either direction
    //     (a stray `Result` wrap on the discharged side, or the dynamic
    //     side collapsing to a bare `/`) breaks it.
    // Negative divisors are law-covered on the QUOTIENT only: omega
    // relates `a / -k` to `a / k` but has no rule for `a % -k` (it leaves
    // the term uninterpreted and reports a counterexample), so the
    // negative remainder is pinned by verify examples instead. Those
    // examples also pin the Euclidean rounding for negative operands —
    // they are emitted as `native_decide` evaluations, so they exercise
    // the emitted definitions without contributing to the kernel-genuine
    // law count.
    assert_proof_builds(
        "tests/fixtures/discharged_div_law.av",
        "aver-proof-discharged-div-law",
    );
}

#[test]
fn proof_export_builds_discharged_bytes_law_when_lake_is_available() {
    // Literal smart-constructor discharge, Lean side
    // (tests/fixtures/discharged_bytes_law.av). `Bytes.fromList([0, 10,
    // 255])` types as plain `Bytes`, so the Lean backend emits a `Subtype`
    // construction `⟨[0, 10, 255], by …⟩` whose obligation is
    // `Bytes.allInRange [0, 10, 255] = true` — the discharge gate's own
    // claim, re-established by the kernel instead of assumed.
    //
    // That obligation is why the emitted tactic ladder grew a
    // `simp [<predicate>]` rung. `allInRange` is compiled by well-founded
    // recursion, which `decide` cannot evaluate through the elaborator
    // (see the note in `src/codegen/lean/crypto_model.lean`), so
    // `first | omega | decide | (simp_all; omega) | assumption` closes
    // none of these goals. The rung names the predicate the refinement's
    // own invariant applies, so it is derived, not hardcoded.
    //
    // The empty frame is the vacuous case, and `dynamicOctetCount` keeps
    // the `Result` path: the file only builds if the discharged calls
    // became direct constructions WHILE the computed-argument call stayed
    // fallible.
    //
    // `frameOctets` puts both in ONE body and carries the law
    // `literalFrameIsIndependentOfTheComputedFrame` — a single theorem
    // obligation, universal in the computed value, over a body whose
    // literal branch is a direct construction and whose other branch
    // matches on the fallible constructor. Both must be true of the same
    // body at once for it to close (and, upstream of the prover, for the
    // file to typecheck at all: a `Result` pattern over a non-`Result`
    // subject is an error).
    assert_proof_builds(
        "tests/fixtures/discharged_bytes_law.av",
        "aver-proof-discharged-bytes-law",
    );
}

#[test]
fn proof_export_builds_discharged_bytes_long_literal_when_lake_is_available() {
    // Recursion-depth stress case for the same rung: 32-element literal
    // frames, one all-zero and one spanning the whole proven interval.
    // Closing `Bytes.allInRange [0, 0, …] = true` needs 32 unfoldings of a
    // well-founded recursive predicate, which is exactly the depth at
    // which `decide` and the pre-existing ladder both fail.
    //
    // Lean only, deliberately: Dafny discharges the same fact as a
    // subset-type constraint at the use site, and its default function
    // fuel does not unfold a 32-element sequence. The shapes BOTH backends
    // discharge live in `discharged_bytes_law.av` above.
    assert_proof_builds(
        "tests/fixtures/discharged_bytes_long_literal.av",
        "aver-proof-discharged-bytes-long-literal",
    );
}

#[test]
fn proof_dafny_verifies_discharged_bytes_law_when_dafny_is_available() {
    // Dafny side of the discharge. A refinement construction collapses to
    // the bare carrier there, so `Bytes.fromList([0, 10, 255])` becomes the
    // literal sequence and Dafny must discharge the subset-type constraint
    // `allInRange(xs)` at the use site by unfolding the same recursive
    // predicate the Lean obligation names. Includes the mixed-body law
    // `frameOctets.literalFrameIsIndependentOfTheComputedFrame`, which
    // Dafny closes as a lemma over the same one body that holds a
    // discharged construction and a fallible call side by side.
    assert_dafny_verifies(
        "tests/fixtures/discharged_bytes_law.av",
        "aver-dafny-discharged-bytes-law",
    );
}

#[test]
fn proof_export_builds_result_default_cone_when_lake_is_available() {
    // A law whose unfold cone contains the `Result.withDefault` builtin
    // (tests/fixtures/result_default_cone.av): the linear-arithmetic
    // strategy must DECLINE the cone. Its simp set used to cite the
    // Aver-side name `Result.withDefault`, which does not exist in the
    // emitted Lean (the builtin lowers to `Except.withDefault`) — an
    // `unknown identifier` BUILD ERROR that destroyed the whole file's
    // caught-sorry floor. And even name-mapped, the lowered zero-guarded
    // `Except` match sits outside omega's theory, so the tactic could
    // never close. Declined, the law degrades to the prelude-simp
    // `first | simp | sorry` rung: budget exactly 1 honest caught sorry.
    //
    // The budget of 1 PREDATES the literal-divisor discharge — it landed
    // with the decline behaviour it pins (#482, "decline instead of
    // guessing on shapes the exporters cannot justify"), and is the whole
    // point of the fixture, so it stays. The discharge work only had to
    // make the fixture's divisor a NAMED BINDING: a literal divisor would
    // now discharge `Int.div` to a bare Int and erase the
    // `Result.withDefault` call this cone exists to exercise, silently
    // turning the test into a second copy of the discharged-div law.
    assert_proof_builds_with_sorry_budget(
        "tests/fixtures/result_default_cone.av",
        "aver-proof-result-default-cone",
        1,
    );
}

#[test]
fn proof_export_builds_rational_ring_laws_kernel_genuine_when_lake_is_available() {
    // The exact-rationals algebra family (`examples/data/rational.av`):
    // ten unconditional ring identities over the cross-multiplied
    // Fraction record. Without the `RingIdentity` strategy these land
    // on the prelude-simp rung, which closes only the identity-element
    // family (zero/one neutral, zero absorbs, double negation) via the
    // default simp set and parks the whole multi-variable nonlinear
    // family — both commutativities, both associativities,
    // distributivity, sub-equals-add-neg — on caught sorries (6 of
    // 10). The strategy's fixed core AC-ring lemma package closes all
    // ten kernel-genuine. Budgets exact in both directions:
    // - `sorries == 0` — the revert signal (dropping the strategy
    //   brings the 6 caught sorries back);
    // - `universal == true` — keys on the #print-axioms audit
    //   ([propext, Quot.sound] only), so a `native_decide` sneaking
    //   into a law theorem fails here even at 0 sorries.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping rational ring-laws proof test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-rational-ring");
    let (summary, run) = run_lean_check_json("examples/data/rational.av", &output_dir, 0, &[]);
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(0),
        "all ten ring laws must close (a caught sorry means the AC-ring \
         package stopped normalizing a law).\n{}",
        format_output(&run)
    );
    assert_eq!(summary["passed"].as_bool(), Some(true));
    assert_eq!(
        summary["universal"].as_bool(),
        Some(true),
        "ring laws must be kernel-genuine — #print-axioms within the \
         whitelist, no native_decide, no Mathlib.\n{}",
        format_output(&run)
    );
    assert_eq!(
        (
            summary["universal_laws"].as_u64(),
            summary["bounded_laws"].as_u64(),
        ),
        (Some(10), Some(0)),
        "explicit law counts: all ten ring laws certified universal, no \
         bounded-domain degradation — same markers and #print-axioms audit \
         the `universal` bool keys on.\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

#[test]
fn proof_dafny_verifies_rational_ring_laws_when_dafny_is_available() {
    // Z3 decides all ten nonlinear ring identities push-button — the
    // `RingIdentity` strategy is Lean-only and Dafny treats the pin as
    // `BackendDispatch`, so this pins the family's Dafny floor at
    // 0 errors / 0 axioms. `passed` is asserted explicitly because
    // this family's historical regression mode is a prover TIMEOUT on
    // concrete-literal sample asserts (exit-status-only, 0 parsed
    // errors), which an errors-only budget cannot catch.
    assert_dafny_verifies_and_passes("examples/data/rational.av", "aver-dafny-rational-ring");
}

/// Audit miscount guard: two DISTINCT conditional laws on the same
/// function legally named `part1` and `part2` emit theorems
/// (`scaledProduct_law_part1`, `scaledProduct_law_part2`) whose names
/// collide with the `<base>_part<N>` chunk-partition naming. Each carries
/// its OWN `-- aver:law-class … bounded-domain` marker, so the audit must
/// key its `bounded_laws` dedup on each theorem's own class (direct lookup
/// first) and count TWO — never fold them onto one phantom base by name
/// alone. Both laws stay bounded (nonlinear `a * b`, no lane recognizer),
/// so the file is a clean `bounded_laws == 2` probe.
#[test]
fn proof_bounded_laws_counts_distinct_part_named_laws_separately() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping bounded-laws miscount test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-bounded-part-named");
    let (summary, run) = run_lean_check_json(
        "tests/fixtures/bounded_part_named_laws.av",
        &output_dir,
        0,
        &[],
    );
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(0),
        "{}",
        format_output(&run)
    );
    assert_eq!(summary["passed"].as_bool(), Some(true));
    assert_eq!(
        summary["bounded_laws"].as_u64(),
        Some(2),
        "two distinct `part1`/`part2` laws must count as TWO bounded laws — \
         folding them onto one base by name miscounts legal input.\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

/// Read the per-law `tier` for `law_id` (`fn.law`) from the emitted
/// `proof_manifest.json`. `None` if the manifest or record is absent.
fn manifest_law_tier(output_dir: &std::path::Path, law_id: &str) -> Option<String> {
    let raw = std::fs::read_to_string(output_dir.join("proof_manifest.json")).ok()?;
    let manifest: serde_json::Value = serde_json::from_str(&raw).ok()?;
    manifest["laws"].as_array()?.iter().find_map(|l| {
        if l["law"].as_str()? == law_id {
            Some(l["tier"].as_str()?.to_string())
        } else {
            None
        }
    })
}

#[test]
fn proof_export_flips_transparent_chain_h1_to_universal_when_lake_is_available() {
    // Acceptance (a) for the transparent-arithmetic premise-chain arm: the
    // self-contained adaptation of the g1c-h1 probe (`1 + e(q2+q3) < e(q1)`,
    // discharged by chaining two digit-separation citations and the sum-exponent
    // window). The engine emits this `when`-law BOUNDED without the arm; the arm
    // unfolds the three cited predicate bodies and closes `split at h_when <;>
    // omega`, so the speculative probe commits it UNIVERSAL. Revert the arm (or
    // its probe gate) and the chain law falls back to `bounded` — this asserts
    // the flip AND the axiom whitelist.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping transparent-chain h1 test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-transparent-chain-h1");
    let (summary, run) = run_lean_check_json(
        "tests/fixtures/transparent_chain_h1.av",
        &output_dir,
        0,
        &[],
    );
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(0),
        "{}",
        format_output(&run)
    );
    assert_eq!(
        summary["passed"].as_bool(),
        Some(true),
        "{}",
        format_output(&run)
    );
    assert_eq!(
        manifest_law_tier(&output_dir, "expSeparationInner.innerHypothesis").as_deref(),
        Some("universal"),
        "the h1 premise chain must flip bounded -> universal via the arm.\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["universal"].as_bool(),
        Some(true),
        "the whole file (pool laws + flipped chain) must be kernel-genuine \
         universal — no native_decide in a law theorem.\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

#[test]
fn proof_export_closes_transparent_chain_cross_domain_witness_when_lake_is_available() {
    // D3 litmus: the SAME chain shape as the h1 hypothesis with all-different
    // names and a non-K5 domain (capacity / load / threshold) closes UNIVERSALLY
    // through the same arm with ZERO further engine changes — proving the
    // recognizer keys on claim SHAPE, not on names or domain constants. A
    // name-keyed regression would leave `roomToSpare.capacityChain` bounded here.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping transparent-chain witness test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-transparent-chain-witness");
    let (summary, run) = run_lean_check_json(
        "tests/fixtures/transparent_chain_witness.av",
        &output_dir,
        0,
        &[],
    );
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(0),
        "{}",
        format_output(&run)
    );
    assert_eq!(
        summary["passed"].as_bool(),
        Some(true),
        "{}",
        format_output(&run)
    );
    assert_eq!(
        manifest_law_tier(&output_dir, "roomToSpare.capacityChain").as_deref(),
        Some("universal"),
        "the cross-domain witness chain must close universal via the same arm.\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

#[test]
fn proof_probe_gating_reverts_not_implied_transparent_chain_to_bounded_when_lake_is_available() {
    // Probe-gating safety rail (the green -> red repro turned regression):
    // `tightRoom.capacityChain` has the EXACT shape the arm recognizes, so the
    // recognizer admits it and the probe attempts it universally — but the
    // premises do NOT imply the over-strong conclusion, so `omega` cannot close
    // it. The probe must see the `AVERSPEC_SORRY` floor and REVERT the law to its
    // sound bounded sampled statement, leaving the whole project `passed`. Remove
    // the probe gate (force universal-or-sorry) and this law's universal theorem
    // carries a sorry — `passed` flips false / `sorries` climbs — so this test
    // fails loudly, exactly the red build the gate prevents.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping transparent-chain not-implied test: `lake` not available");
        return;
    }
    let output_dir = temp_output_dir("aver-proof-transparent-chain-not-implied");
    let (summary, run) = run_lean_check_json(
        "tests/fixtures/transparent_chain_not_implied.av",
        &output_dir,
        0,
        &[],
    );
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(0),
        "{}",
        format_output(&run)
    );
    assert_eq!(
        summary["passed"].as_bool(),
        Some(true),
        "an in-shape but not-implied chain must revert to bounded and keep the \
         project passing, never flip it red.\n{}",
        format_output(&run)
    );
    assert_eq!(
        manifest_law_tier(&output_dir, "tightRoom.capacityChain").as_deref(),
        Some("bounded"),
        "the not-implied chain must land bounded, not universal.\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

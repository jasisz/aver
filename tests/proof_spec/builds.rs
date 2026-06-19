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
fn proof_dafny_verifies_list_length_fold_when_dafny_is_available() {
    // Stage 8c of #232: `ProofStrategy::MatchDispatcherFold` — two
    // structural list folds (`1 + length(t)` vs `length(t) + 1`)
    // closing by induction on `xs`. Dafny's default Induction path
    // already verifies this shape; the explicit strategy makes the
    // recognition observable in proof_ir.
    assert_dafny_verifies(
        "examples/data/list_length_fold.av",
        "aver-dafny-list-length-fold",
    );
}

#[test]
fn proof_export_builds_list_length_fold_when_lake_is_available() {
    // Lean template: `induction xs with | nil => simp | cons => simp;
    // omega`. The omega discharge handles `1 + x = x + 1`.
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
    //   into a law theorem fails here even at 0 sorries;
    // - `when_universal == 0` — every law is unconditional (that is
    //   the point of the non-normalizing representation), so the
    //   quarantine lane must stay empty.
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
        summary["when_universal"].as_u64(),
        Some(0),
        "no when-laws in the file; the quarantine lane must stay empty.\n{}",
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
        summary["when_universal"].as_u64(),
        Some(0),
        "both laws stay bounded; no lane recognizer fires.\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["bounded_laws"].as_u64(),
        Some(2),
        "two distinct `part1`/`part2` laws must count as TWO bounded laws — \
         folding them onto one base by name miscounts legal input.\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&output_dir);
}

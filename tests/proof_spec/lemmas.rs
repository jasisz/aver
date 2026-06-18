use super::*;

/// THE FEEDBACK LOOP (`ProofStrategy::SimpOverLemmas`) — the coverage win
/// condition, end to end. The law states `plus(length(ys), length(xs)) =
/// length(append(xs, ys))` — closing it needs the UNSTATED length-into-plus
/// homomorphism (`length (a ++ b) = plus (length a) (length b)`), which the
/// induction ladder alone cannot conjure (the `plus` recursion is stuck on a
/// symbolic first argument, and `omega` sees only opaque atoms). The loop:
///
/// 1. baseline `aver proof --check` — builds, but the universal stays `sorry`
///    (`universal:false`): the honest no-discovery floor;
/// 2. `aver proof --discover` conjectures AND kernel-proves the homomorphism,
///    committing it to `DiscoveredLemmas.lean`;
/// 3. the SAME `aver proof --check` in the SAME output dir now re-pins the
///    law to `SimpOverLemmas`, embeds + re-proves the lemma, and closes the
///    universal for real (`universal:true`, zero sorries).
///
/// This is discovery moving COVERAGE, not just reach.
#[test]
fn discovered_lemmas_close_length_homomorphism_law_when_lake_is_available() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping feedback-loop test: `lake` not available");
        return;
    }
    let source = "module LenHomo\n    intent =\n        \"plus(length ys, length xs) = length(xs ++ ys) — needs the unstated length homomorphism\"\n    effects []\n\n\
         type Nat\n    Z\n    S(Nat)\n\n\
         fn length(xs: List<Int>) -> Nat\n    match xs\n        [] -> Nat.Z\n        [y, ..ys] -> Nat.S(length(ys))\n\n\
         fn plus(x: Nat, y: Nat) -> Nat\n    match x\n        Nat.Z -> y\n        Nat.S(z) -> Nat.S(plus(z, y))\n\n\
         fn append(xs: List<Int>, ys: List<Int>) -> List<Int>\n    List.concat(xs, ys)\n\n\
         verify length law lenAppendSwap\n    given xs: List<Int> = [[], [1], [1, 2]]\n    given ys: List<Int> = [[], [3]]\n    plus(length(ys), length(xs)) => length(append(xs, ys))\n";
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-feedback-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(src.join("m.av"), source).expect("write m.av");
    let out = temp_output_dir("aver-feedback-out");

    let check = |label: &str| -> (bool, String) {
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
            .unwrap_or_else(|e| panic!("{label}: aver proof failed to run: {e}"));
        let json_line = run
            .stdout
            .split(|&b| b == b'\n')
            .rev()
            .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
            .unwrap_or_else(|| panic!("{label}: no JSON line:\n{}", format_output(&run)))
            .to_string();
        let summary: serde_json::Value = serde_json::from_str(&json_line)
            .unwrap_or_else(|e| panic!("{label}: bad JSON ({e}):\n{json_line}"));
        (
            summary["universal"].as_bool().unwrap_or(false),
            format_output(&run),
        )
    };

    // 1. The honest floor: without discovery the universal must NOT close —
    //    if it ever starts closing, the fixture stopped exercising the loop
    //    (tighten it) rather than the loop having regressed.
    let (universal_before, output_before) = check("baseline");
    assert!(
        !universal_before,
        "fixture closes WITHOUT discovery — it no longer exercises the feedback loop:\n{output_before}"
    );

    // 2. Discover + commit into the same output dir.
    let discover = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("m.av"))
        .arg("--discover")
        .arg("-o")
        .arg(&out)
        .output()
        .expect("expected `aver proof --discover` to run");
    let committed = std::fs::read_to_string(out.join("DiscoveredLemmas.lean")).unwrap_or_default();
    assert!(
        committed.contains("length (") && committed.contains("plus (length"),
        "discovery did not commit the length homomorphism:\n--- DiscoveredLemmas.lean ---\n{committed}\n--- discover output ---\n{}",
        format_output(&discover)
    );

    // 3. The win: the SAME check now closes the universal via the committed
    //    lemma (re-proved in the same build — no trust shortcut).
    let (universal_after, output_after) = check("with-discovery");
    assert!(
        universal_after,
        "committed discovered lemmas did not close the law (feedback loop broken):\n{output_after}"
    );

    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

/// DO-NO-HARM (Houdini accept) — the INVERSE of the feedback-loop win above.
/// `rev (rev x) = x` already closes `universal:true` WITHOUT discovery (the rev
/// anti-homomorphism recognizer emits its own aux lemma). But `--discover` over
/// this cone commits a REVERSED unfold rule (`(x ++ y) = append x y`), which the
/// feedback path used to inject into the induction ARMS — where, mixed with
/// `append`'s own def, it simp-loops the working ladder into a `sorry`
/// (`universal:true → false`: the net-negative feedback bug). The fix tries the
/// EXACT pre-discovery proof FIRST (a no-`sorry` plain ladder that throws on an
/// open arm), so a committed lemma can only ever ADD a proof, never demote one.
/// This test pins that property: a law universal before discovery STAYS
/// universal after committing lemmas. (Complements the length-homomorphism test
/// — there discovery MUST move false→true; here it MUST NOT move true→false.)
#[test]
fn discovery_does_not_demote_an_already_universal_law_when_lake_is_available() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping do-no-harm test: `lake` not available");
        return;
    }
    let source = "module RevRev\n    intent =\n        \"rev (rev x) = x — already universal via the rev anti-homomorphism recognizer\"\n    effects []\n\n\
         fn append(x: List<Int>, y: List<Int>) -> List<Int>\n    match x\n        [] -> y\n        [z, ..xs] -> List.concat([z], append(xs, y))\n\n\
         fn rev(x: List<Int>) -> List<Int>\n    match x\n        [] -> []\n        [y, ..xs] -> append(rev(xs), [y])\n\n\
         verify rev law revRev\n    given x: List<Int> = [[], [1], [1, 2], [1, 2, 3]]\n    rev(rev(x)) => x\n";
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-donoharm-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(src.join("m.av"), source).expect("write m.av");
    let out = temp_output_dir("aver-donoharm-out");

    let check = |label: &str| -> (bool, u64, String) {
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
            .unwrap_or_else(|e| panic!("{label}: aver proof failed to run: {e}"));
        let json_line = run
            .stdout
            .split(|&b| b == b'\n')
            .rev()
            .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
            .unwrap_or_else(|| panic!("{label}: no JSON line:\n{}", format_output(&run)))
            .to_string();
        let summary: serde_json::Value = serde_json::from_str(&json_line)
            .unwrap_or_else(|e| panic!("{label}: bad JSON ({e}):\n{json_line}"));
        (
            summary["universal"].as_bool().unwrap_or(false),
            summary["sorries"].as_u64().unwrap_or(u64::MAX),
            format_output(&run),
        )
    };

    // 1. Baseline: the law is universal WITHOUT discovery. If this ever flips,
    //    the fixture stopped exercising do-no-harm (the recognizer changed) —
    //    fix the fixture, not by relaxing the post-discovery assertion.
    let (universal_before, sorries_before, output_before) = check("baseline");
    assert!(
        universal_before && sorries_before == 0,
        "fixture is NOT universal without discovery — it no longer exercises do-no-harm:\n{output_before}"
    );

    // 2. Discover + commit into the same output dir (commits the reversed
    //    `(x ++ y) = append x y` unfold rule that used to poison the arms).
    let discover = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("m.av"))
        .arg("--discover")
        .arg("-o")
        .arg(&out)
        .output()
        .expect("expected `aver proof --discover` to run");
    let committed = std::fs::read_to_string(out.join("DiscoveredLemmas.lean")).unwrap_or_default();
    assert!(
        !committed.trim().is_empty() && committed.contains("theorem"),
        "discovery committed no lemmas — the fixture no longer pins the feedback path:\n--- discover output ---\n{}",
        format_output(&discover)
    );

    // 3. DO-NO-HARM: the SAME check, now replaying the committed lemmas, MUST
    //    stay universal with zero sorries. A regression here is the
    //    net-negative feedback bug (a committed lemma demoting a working proof).
    let (universal_after, sorries_after, output_after) = check("with-discovery");
    assert!(
        universal_after && sorries_after == 0,
        "committed discovered lemmas DEMOTED an already-universal law (do-no-harm violated):\n{output_after}"
    );

    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

/// THE FEEDBACK LOOP, część A — an already-proved EARLIER user `verify … law`
/// feeds a later law's proof, with NO `--discover` step at all. The file holds
/// two laws over `length`: `lengthHomo` (the homomorphism `length (append xs
/// ys) = plus (length xs) (length ys)`, provable on its own by induction) and,
/// AFTER it, `lengthAppendSwap` (`length (append xs ys) = plus (length ys)
/// (length xs)`), which needs the homomorphism plus commutativity. The later
/// law must close ONLY because the earlier one is in scope:
///
/// 1. `lengthAppendSwap` ALONE (its own file) must NOT close — it has no
///    helper, so the universal stays `sorry`;
/// 2. the same law AFTER `lengthHomo` in one file must close `universal:true`
///    — the backend references the earlier theorem in the later proof's `simp`
///    set (verified by inspecting the emitted Lean), no discovery artifact
///    involved.
///
/// This is the user-written-decomposition half of the loop: a hard law closes
/// because an earlier proved law is available as a lemma.
#[test]
fn earlier_user_law_feeds_later_law_proof_when_lake_is_available() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping część A test: `lake` not available");
        return;
    }
    let nat_helpers = "type Nat\n    Z\n    S(Nat)\n\n\
         fn length(xs: List<Int>) -> Nat\n    match xs\n        [] -> Nat.Z\n        [y, ..ys] -> Nat.S(length(ys))\n\n\
         fn plus(x: Nat, y: Nat) -> Nat\n    match x\n        Nat.Z -> y\n        Nat.S(z) -> Nat.S(plus(z, y))\n\n\
         fn append(xs: List<Int>, ys: List<Int>) -> List<Int>\n    match xs\n        [] -> ys\n        [z, ..zs] -> List.concat([z], append(zs, ys))\n\n";
    let swap_law = "verify length law lengthAppendSwap\n    given xs: List<Int> = [[], [1], [1, 2]]\n    given ys: List<Int> = [[], [3]]\n    length(append(xs, ys)) => plus(length(ys), length(xs))\n";
    let homo_law = "verify length law lengthHomo\n    given xs: List<Int> = [[], [1], [1, 2]]\n    given ys: List<Int> = [[], [3]]\n    length(append(xs, ys)) => plus(length(xs), length(ys))\n\n";

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let universal_of = |source: &str, prefix: &str| -> (bool, String, String) {
        let src = temp_output_dir(&format!("{prefix}-src"));
        std::fs::create_dir_all(&src).expect("create src dir");
        std::fs::write(src.join("m.av"), source).expect("write m.av");
        let out = temp_output_dir(&format!("{prefix}-out"));
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
            .expect("aver proof ran");
        let json = run
            .stdout
            .split(|&b| b == b'\n')
            .rev()
            .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
            .unwrap_or_else(|| panic!("{prefix}: no JSON line:\n{}", format_output(&run)))
            .to_string();
        let summary: serde_json::Value =
            serde_json::from_str(&json).unwrap_or_else(|e| panic!("{prefix}: bad JSON ({e})"));
        let emitted = std::fs::read_to_string(out.join("M.lean")).unwrap_or_default();
        let _ = std::fs::remove_dir_all(&src);
        let _ = std::fs::remove_dir_all(&out);
        (
            summary["universal"].as_bool().unwrap_or(false),
            format_output(&run),
            emitted,
        )
    };

    // 1. swap law alone — no helper in scope → must stay open.
    let solo =
        format!("module M\n    intent = \"solo\"\n    effects []\n\n{nat_helpers}{swap_law}");
    let (solo_universal, solo_out, _) = universal_of(&solo, "aver-partA-solo");
    assert!(
        !solo_universal,
        "swap law closed WITHOUT a helper — the fixture no longer exercises część A:\n{solo_out}"
    );

    // 2. helper FIRST, then swap law — must close via the earlier theorem.
    let paired = format!(
        "module M\n    intent = \"paired\"\n    effects []\n\n{nat_helpers}{homo_law}{swap_law}"
    );
    let (paired_universal, paired_out, emitted) = universal_of(&paired, "aver-partA-paired");
    assert!(
        paired_universal,
        "swap law did not close with the earlier homomorphism law in scope (część A broken):\n{paired_out}"
    );
    // The later proof must actually reference the earlier law's theorem — proof
    // that the close came from the sibling lemma, not some incidental tactic.
    assert!(
        emitted.contains("length_law_lengthAppendSwap")
            && emitted.contains("simp only [length_law_lengthHomo"),
        "the swap law's proof does not simp over the earlier homomorphism theorem:\n{emitted}"
    );
}

/// The `proof-corpus/decomposed/` artifacts: OPEN bare-`tip/` TIP tasks that an
/// LLM closed by writing helper `verify ... law` blocks (część A / the loop).
/// Each must stay `universal:true` on its own merits — every law in the file,
/// helpers included, kernel-clean (no `sorry`, axiom set ⊆ {propext,
/// Classical.choice, Quot.sound}). This guards the feedback loop AND the
/// auto-prover's leaf reach from regressing: if a future change stops closing
/// any helper, its file drops to universal:false and this test fails loudly.
/// Distinct from the corpus coverage runner — these are NOT counted in the
/// baseline (run.sh excludes decomposed/); they are the loop-reach record.
#[test]
fn decomposed_tip_tasks_stay_universal_when_lake_is_available() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping decomposed-corpus test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let tasks = [
        "proof-corpus/decomposed/isaplanner/prop_03.av",
        "proof-corpus/decomposed/isaplanner/prop_04.av",
        "proof-corpus/decomposed/isaplanner/prop_20.av",
        "proof-corpus/decomposed/isaplanner/prop_28.av",
        "proof-corpus/decomposed/isaplanner/prop_29.av",
        "proof-corpus/decomposed/isaplanner/prop_30.av",
        "proof-corpus/decomposed/isaplanner/prop_38.av",
        "proof-corpus/decomposed/isaplanner/prop_75.av",
        "proof-corpus/decomposed/prod/prop_03.av",
        "proof-corpus/decomposed/prod/prop_25.av",
        // decomposition-reach chunk #1 (reverse/accumulator family).
        "proof-corpus/decomposed/handwritten/qrev_rev.av",
        "proof-corpus/decomposed/isaplanner/prop_19.av",
        "proof-corpus/decomposed/prod/prop_27.av",
        "proof-corpus/decomposed/prod/prop_29.av",
        "proof-corpus/decomposed/prod/prop_30.av",
        "proof-corpus/decomposed/prod/prop_31.av",
        // decomposition-reach chunk #2 (deterministic spread over fresh open pool).
        "proof-corpus/decomposed/isaplanner/prop_55.av",
        "proof-corpus/decomposed/prod/prop_02.av",
        "proof-corpus/decomposed/prod/prop_19.av",
        "proof-corpus/decomposed/prod/prop_34.av",
        // bridge-free structural rung: the `even`-shift targets whose sibling
        // laws are `S (S)`-shaped and were stranded by the `plus → +` / commute
        // normalizers in the bridged simp set (now closed by a leading
        // structural-only rung).
        "proof-corpus/decomposed/prod/lemma_14.av",
        "proof-corpus/decomposed/prod/lemma_16.av",
    ];
    for task in tasks {
        let out = temp_output_dir(&format!(
            "aver-decomposed-{}",
            task.rsplit('/').next().unwrap().trim_end_matches(".av")
        ));
        let run = Command::new(aver_bin)
            .current_dir(&repo_root)
            .arg("proof")
            .arg(task)
            .arg("--backend")
            .arg("lean")
            .arg("-o")
            .arg(&out)
            .arg("--check")
            .arg("--check-json")
            .output()
            .unwrap_or_else(|e| panic!("{task}: aver proof failed to run: {e}"));
        let json = run
            .stdout
            .split(|&b| b == b'\n')
            .rev()
            .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
            .unwrap_or_else(|| panic!("{task}: no JSON line:\n{}", format_output(&run)));
        let summary: serde_json::Value =
            serde_json::from_str(json).unwrap_or_else(|e| panic!("{task}: bad JSON ({e})"));
        assert_eq!(
            summary["universal"].as_bool(),
            Some(true),
            "{task}: decomposed artifact must stay universal:true (every law, helpers \
             included, kernel-clean). A drop means część A or an auto-prover leaf \
             regressed.\n{}",
            format_output(&run)
        );
        let _ = std::fs::remove_dir_all(&out);
    }
}

/// część C — ARM injection of Forward sibling laws. Some laws need a helper
/// applied INSIDE the induction cons-arm, not just at the top-level fast path:
/// `count n xs = count n (rev xs)` only closes if the count-homomorphism
/// rewrites `count n (rev t ++ [h])` within the arm. część A (fast-path only)
/// leaves these open; część C adds a second ladder whose arms carry the
/// Forward siblings (Reversed stay fast-path-only — loop safety). Two checks:
///
/// 1. count-rev closes when a count-homomorphism helper precedes it (the
///    homo is used in-arm; it also INTRODUCES `plus`, exercising the
///    subject-sharing cone relaxation — `plus` is outside count-rev's cone but
///    the helper shares the subject `count`);
/// 2. length-rev closes via a CHAIN — length-homo, then a length-rev-invariant
///    whose OWN cons-arm needs the length-homo sibling, then the target.
///
/// Both are union-OPEN frontier TIP shapes (isaplanner/prop_52, prod/prop_06)
/// that the manual experiment (#449) could NOT close before część C.
#[test]
fn part_c_arm_injection_closes_in_arm_helpers_when_lake_is_available() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping część C test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let nat = "type Nat\n    Z\n    S(Nat)\n\n";
    let plus = "fn plus(x: Nat, y: Nat) -> Nat\n    match x\n        Nat.Z -> y\n        Nat.S(z) -> Nat.S(plus(z, y))\n\n";

    let count_rev = format!(
        "module M\n    intent = \"count-rev via in-arm homo\"\n    effects []\n\n{nat}\
         fn eqNat(x: Nat, y: Nat) -> Bool\n    match x\n        Nat.Z -> match y\n            Nat.Z -> true\n            Nat.S(z) -> false\n        Nat.S(x2) -> match y\n            Nat.Z -> false\n            Nat.S(y2) -> eqNat(x2, y2)\n\n\
         fn count(x: Nat, y: List<Nat>) -> Nat\n    match y\n        [] -> Nat.Z\n        [z, ..ys] -> match eqNat(x, z)\n            true -> Nat.S(count(x, ys))\n            false -> count(x, ys)\n\n{plus}\
         fn rev(x: List<Nat>) -> List<Nat>\n    match x\n        [] -> []\n        [y, ..xs] -> List.concat(rev(xs), [y])\n\n\
         verify count law countHomo\n    given n: Nat = [Nat.Z, Nat.S(Nat.Z)]\n    given xs: List<Nat> = [[Nat.Z], [Nat.S(Nat.Z), Nat.Z]]\n    given ys: List<Nat> = [[Nat.Z], [Nat.S(Nat.Z)]]\n    count(n, List.concat(xs, ys)) => plus(count(n, xs), count(n, ys))\n\n\
         verify count law countRev\n    given n: Nat = [Nat.Z, Nat.S(Nat.Z), Nat.S(Nat.S(Nat.Z))]\n    given xs: List<Nat> = [[], [Nat.Z], [Nat.Z, Nat.S(Nat.Z), Nat.Z]]\n    count(n, xs) => count(n, rev(xs))\n"
    );

    let length_rev = format!(
        "module M\n    intent = \"length-rev via chain\"\n    effects []\n\n{nat}\
         fn length(xs: List<Int>) -> Nat\n    match xs\n        [] -> Nat.Z\n        [y, ..ys] -> Nat.S(length(ys))\n\n{plus}\
         fn append(x: List<Int>, y: List<Int>) -> List<Int>\n    match x\n        [] -> y\n        [z, ..xs] -> List.concat([z], append(xs, y))\n\n\
         fn rev(x: List<Int>) -> List<Int>\n    match x\n        [] -> []\n        [y, ..ys] -> append(rev(ys), [y])\n\n\
         verify length law lengthHomo\n    given x: List<Int> = [[1], [2, 3]]\n    given y: List<Int> = [[4], [5, 6]]\n    length(append(x, y)) => plus(length(x), length(y))\n\n\
         verify length law lengthRevInv\n    given x: List<Int> = [[], [1], [1, 2, 3]]\n    length(rev(x)) => length(x)\n\n\
         verify length law revAppendLength\n    given x: List<Int> = [[], [1], [1, 2, 3]]\n    given y: List<Int> = [[], [2], [4, 5]]\n    length(rev(append(x, y))) => plus(length(x), length(y))\n"
    );

    let check_universal = |source: &str, label: &str| {
        let src = temp_output_dir(&format!("aver-partc-{label}-src"));
        std::fs::create_dir_all(&src).expect("src dir");
        std::fs::write(src.join("m.av"), source).expect("write");
        let out = temp_output_dir(&format!("aver-partc-{label}-out"));
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
            .expect("aver proof ran");
        let json = run
            .stdout
            .split(|&b| b == b'\n')
            .rev()
            .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
            .unwrap_or_else(|| panic!("{label}: no JSON:\n{}", format_output(&run)));
        let summary: serde_json::Value =
            serde_json::from_str(json).unwrap_or_else(|e| panic!("{label}: bad JSON ({e})"));
        assert_eq!(
            summary["universal"].as_bool(),
            Some(true),
            "{label}: część C must close this in-arm-helper law universal:true\n{}",
            format_output(&run)
        );
        let _ = std::fs::remove_dir_all(&src);
        let _ = std::fs::remove_dir_all(&out);
    };
    check_universal(&count_rev, "count-rev");
    check_universal(&length_rev, "length-rev");
}

/// Leaf-reach: the Lean backend auto-proves a GENERALIZING-induction law —
/// `take n xs ++ drop n xs = xs`, where `take`/`drop` recurse synchronously on
/// the Nat `n` AND the list `xs`. Single-variable `induction xs` leaves the
/// cons IH at the wrong `n`; the backend now emits `induction xs generalizing
/// n with … cases n` so the IH (`∀ n, P n tail`) applies at the predecessor.
/// No helper law, no decomposition — the bare auto-prover closes it. (TIP
/// isaplanner prop_01, a union-OPEN frontier shape nothing closed before.)
#[test]
fn lean_proves_generalizing_induction_take_drop_when_lake_is_available() {
    assert_proof_builds(
        "proof-corpus/tip/isaplanner/prop_01.av",
        "aver-gen-takedrop",
    );
}

/// Leaf-reach: a user fn named `max` (colliding with Lean 4's Max-typeclass
/// `max`) is emitted as `max'`, so the proof can reference the user's
/// recursion instead of the typeclass form. Before the escape the generated
/// Lean failed to build (ambiguous/typeclass `max`); now it builds clean.
/// max-associativity USED to fall to one honest `sorry` (a 3-var induction the
/// bare prover left open); the both-args-peeling generalizing emit now closes
/// it as a GENUINE universal — `induction a generalizing b c with … cases b
/// <;> cases c <;> simp_all`, `#print axioms = [propext]`. Budget is 0:
/// fully proven. TIP isaplanner prop_22.
#[test]
fn lean_escapes_user_max_min_collision_when_lake_is_available() {
    assert_proof_builds_with_sorry_budget(
        "proof-corpus/tip/isaplanner/prop_22.av",
        "aver-max-escape",
        0,
    );
}

/// Leaf-reach: the Lean backend auto-proves an ACCUMULATOR-generalizing law —
/// `qrev(xs, acc) = rev(xs) ++ acc`, where `qrev` recurses on `xs` while
/// THREADING `acc` (fed `List.concat([h], acc)`). The IH must be `∀ acc,
/// P xs acc`, so the backend emits `induction xs generalizing acc` (no `cases`
/// — `acc` is not a Peano scrutinee, distinct from the take/drop Nat case).
/// No helper, bare — the lemma that previously SORRIED. (The qrev↔rev
/// equivalence `fastRev = rev` then closes by decomposition over it.)
#[test]
fn lean_proves_accumulator_generalizing_qrev_when_lake_is_available() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping accumulator-gen test: `lake` not available");
        return;
    }
    let source = "module QrevAccGen\n    intent = \"qrev acc-generalization\"\n    effects []\n\n\
        fn rev(xs: List<Int>) -> List<Int>\n    match xs\n        [] -> []\n        [h, ..t] -> List.concat(rev(t), [h])\n\n\
        fn qrev(xs: List<Int>, acc: List<Int>) -> List<Int>\n    match xs\n        [] -> acc\n        [h, ..t] -> qrev(t, List.concat([h], acc))\n\n\
        verify qrev law qrevRevAppend\n    given xs: List<Int> = [[1], [1, 2, 3]]\n    given acc: List<Int> = [[9], [8, 7]]\n    qrev(xs, acc) => List.concat(rev(xs), acc)\n";
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-accgen-src");
    std::fs::create_dir_all(&src).expect("src dir");
    std::fs::write(src.join("m.av"), source).expect("write");
    let out = temp_output_dir("aver-accgen-out");
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
        .expect("aver proof ran");
    let json = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON:\n{}", format_output(&run)));
    let summary: serde_json::Value = serde_json::from_str(json).expect("json");
    assert_eq!(
        summary["universal"].as_bool(),
        Some(true),
        "accumulator-generalizing must close qrev(xs,acc)=rev(xs)++acc bare\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

/// A law whose verified fn body propagates errors with `?` must still collect
/// the `?`-wrapped calls into its unfold set. `sumSafe`'s body is
/// `x = safe(a)?; y = safe(b)?; Ok(x + y)` — every call hides behind `?`
/// (`Expr::ErrorProp`). Before `collect_fn_calls_expr` handled `ErrorProp`, the
/// unfold set missed `safe`, so the LinearArithmetic tactic stalled with "simp
/// made no progress" and the commutativity law stayed open. With the arm, `safe`
/// unfolds and the law closes kernel-clean. (Found on the real
/// `refinement/natural_app.av` safeSum commutativity — a `?`-pipeline the TIP
/// corpus never exercises.)
#[test]
fn lean_proves_error_prop_pipeline_law_when_lake_is_available() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping ?-pipeline test: `lake` not available");
        return;
    }
    let source = r#"module QPipe
    intent = "a law over a ?-propagating body must unfold the ?-wrapped calls"
    effects []

fn safe(x: Int) -> Result<Int, String>
    ? "non-negative gate"
    match x >= 0
        true -> Result.Ok(x)
        false -> Result.Err("neg")

fn sumSafe(a: Int, b: Int) -> Result<Int, String>
    ? "add two gated ints; ? short-circuits"
    x = safe(a)?
    y = safe(b)?
    Result.Ok(x + y)

verify sumSafe law commutative
    given a: Int = [0, 1, 7]
    given b: Int = [0, 2, 9]
    sumSafe(a, b) => sumSafe(b, a)
"#;
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-qpipe-src");
    std::fs::create_dir_all(&src).expect("src dir");
    std::fs::write(src.join("m.av"), source).expect("write");
    let out = temp_output_dir("aver-qpipe-out");
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
        .expect("aver proof ran");
    let json = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON:\n{}", format_output(&run)));
    let summary: serde_json::Value = serde_json::from_str(json).expect("json");
    assert_eq!(
        summary["universal"].as_bool(),
        Some(true),
        "a law over a ?-propagating body must unfold its ?-wrapped calls and close\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

/// A map-counting homomorphism — `(Map.get (countWords ws) w).getD 0 =
/// countWord ws w` and `Map.has (countWords ws) w = containsWord ws w` — closes
/// kernel-clean. The cons different-key branch needs the general-key Map prelude
/// lemmas `AverMap.get_set_ne` / `AverMap.has_set` (added to AverCommon) injected
/// into the induction arm; AverCommon previously shipped only the self-key
/// lemmas, so these stayed `sorry`. Runs the real `examples/data/map.av`
/// (single-module) — both `countWords` laws must be genuine universals.
#[test]
fn lean_proves_map_fold_homomorphism_when_lake_is_available() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping map-fold test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let out = temp_output_dir("aver-mapfold-out");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("examples/data/map.av")
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("aver proof ran");
    let json = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON:\n{}", format_output(&run)));
    let summary: serde_json::Value = serde_json::from_str(json).expect("json");
    assert_eq!(
        summary["universal"].as_bool(),
        Some(true),
        "map-counting homomorphism laws must close via the general-key Map prelude \
         lemmas (get_set_ne / has_set)\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&out);
}

/// A ground ∀-law over fixed enum/ADT constructors (with unused Int givens) must
/// close via the `EnumConstantFold` strategy. `bonus(Cell.Empty, x) = 0` pins the
/// ADT param to a constructor; LinearArithmetic rejects it (non-Int param) and
/// Induction finds no recursive given, so it used to fall to BackendDispatch and
/// `sorry`. The new strategy emits `intro …; simp only [fn, defs] <;> (split <;>
/// rfl | rfl | decide)`. (Found on real `games/checkers/ai.av` centerBonus /
/// pieceValue laws.)
#[test]
fn lean_proves_enum_constant_fold_law_when_lake_is_available() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping enum-constant-fold test: `lake` not available");
        return;
    }
    let source = r#"module EnumFold
    intent = "a ground law over fixed enum/ADT constructors with unused Int givens"
    effects []

type Cell
    Empty
    Piece(Int)

fn bonus(c: Cell, x: Int) -> Int
    match c
        Cell.Empty -> 0
        Cell.Piece(v) -> v + x

verify bonus law emptyZero
    given x: Int = [0, 5, 9]
    bonus(Cell.Empty, x) => 0
"#;
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-enumfold-src");
    std::fs::create_dir_all(&src).expect("src dir");
    std::fs::write(src.join("m.av"), source).expect("write");
    let out = temp_output_dir("aver-enumfold-out");
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
        .expect("aver proof ran");
    let json = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON:\n{}", format_output(&run)));
    let summary: serde_json::Value = serde_json::from_str(json).expect("json");
    assert_eq!(
        summary["universal"].as_bool(),
        Some(true),
        "a ground enum/ADT constant-fold law must close kernel-clean\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

/// A no-when law whose every given ranges over a closed finite domain (here a
/// fieldless 3-ctor enum) must close via the `FiniteDomainCases` strategy:
/// `intro …; cases <given> <;> … <;> (first | rfl | decide | sorry)`. No earlier
/// detector owns it — the given is a free quantified enum (EnumConstantFold's
/// literal-pinning gate rejects it), the return is String (its scalar-return
/// gate rejects it too), and the enum isn't recursive (no Induction target) —
/// so it used to fall to BackendDispatch + `sorry`. (Found on real
/// `examples/data/json.av`: `parseLiteral.boolRoundtrip` + the `EscapeCode`
/// laws, which close genuinely with `cases … <;> rfl`.)
#[test]
fn lean_proves_finite_domain_cases_law_when_lake_is_available() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping finite-domain-cases test: `lake` not available");
        return;
    }
    let source = r#"module FiniteDomain
    intent = "a no-when law quantified over a closed fieldless-enum domain"
    effects []

type Color
    Red
    Green
    Blue

fn shade(c: Color) -> String
    match c
        Color.Red -> "warm"
        Color.Green -> "cool"
        Color.Blue -> "cool"

verify shade law neverEmpty
    given c: Color = [Color.Red, Color.Green, Color.Blue]
    shade(c) == "" => false
"#;
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-finitedomain-src");
    std::fs::create_dir_all(&src).expect("src dir");
    std::fs::write(src.join("m.av"), source).expect("write");
    let out = temp_output_dir("aver-finitedomain-out");
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
        .expect("aver proof ran");
    let json = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON:\n{}", format_output(&run)));
    let summary: serde_json::Value = serde_json::from_str(json).expect("json");
    assert_eq!(
        summary["universal"].as_bool(),
        Some(true),
        "a closed finite-domain law must close kernel-clean via exhaustive cases\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

/// A no-when builtin-roundtrip law over a non-recursive String fn must close
/// via the `SimpOverPreludeLemmas` strategy: the rung emits `intro …; first |
/// (simp [<cone>, <registry lemmas>, Int.add_sub_cancel]; done) | sorry`, where
/// the registry maps the cone's builtins (`String.slice`, the string-`+`
/// marker) to the prelude spec lemmas (`String.add_eq_append`,
/// `String.slice_full` / `String.slice_append_prefix`) that the demand-driven
/// prelude then ships. No earlier detector owns the shape (LinearArithmetic
/// rejects String params, EnumConstantFold needs a ctor-pinned ADT + scalar
/// return, FiniteDomainCases needs a closed-domain given), so it used to fall
/// to BackendDispatch + bare `sorry`. (Found on real `examples/data/json.av`:
/// `finishString.plainSegmentRoundtrip` and the `finishInt` / `finishNumber` /
/// `afterIntChar` canonical-Int roundtrips.)
#[test]
fn lean_proves_simp_over_prelude_law_when_lake_is_available() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping simp-over-prelude test: `lake` not available");
        return;
    }
    let source = r#"module PreludeSimp
    intent = "a builtin-roundtrip law closing via prelude spec lemmas"
    effects []

fn keepPrefix(s: String, n: Int) -> String
    String.slice(s, 0, n)

verify keepPrefix law appendRoundtrip
    given s: String = ["", "ab", "xyz"]
    keepPrefix(s + "!", String.len(s)) => s
"#;
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-preludesimp-src");
    std::fs::create_dir_all(&src).expect("src dir");
    std::fs::write(src.join("m.av"), source).expect("write");
    let out = temp_output_dir("aver-preludesimp-out");
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
        .expect("aver proof ran");
    let json = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON:\n{}", format_output(&run)));
    let summary: serde_json::Value = serde_json::from_str(json).expect("json");
    assert_eq!(
        summary["universal"].as_bool(),
        Some(true),
        "a builtin-roundtrip law must close kernel-clean via the prelude spec lemmas\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

/// A no-when universal `parse(render(C(n)), 0) = Ok(C(n), len)` law over the
/// canonical decimal parser must close GENUINELY via `IntDecimalRoundtrip`:
/// the Lean backend synthesizes the scanner's `<fn>__fuel_scan` companion
/// lemma at the fuel-def site (the general crack in the fuel-unfolding
/// barrier — an all-digit suffix scan runs to the end of a SYMBOLIC string)
/// and renders the fixed sign-split skeleton from the verified json hand
/// proof (serializer `rfl` through ADT-measure fuel, `String.mk`-form
/// head-char dispatch, `digitChar` `Char.toString` disequalities for the
/// dead arms, `Int.fromString_fromInt` at the leaf). `universal:true` means
/// every theorem closed with axioms ⊆ [propext, Classical.choice,
/// Quot.sound] — no sorry, no native_decide. (Found on real
/// `examples/data/json.av`: `parseNumber.fromIntRoundtrip`.)
#[test]
fn lean_proves_int_decimal_roundtrip_law_when_lake_is_available() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping int-decimal-roundtrip test: `lake` not available");
        return;
    }
    let source = r#"module DecimalRt
    intent = "a decimal int render/parse roundtrip closing via IntDecimalRoundtrip"
    effects []

type Num
    NumInt(Int)

type ParseOut
    Got(Num, Int)
    Bad(String, Int)

fn render(v: Num) -> String
    match v
        Num.NumInt(n) -> String.fromInt(n)

fn isDigit(c: String) -> Bool
    code = Char.toCode(c)
    match code >= 48
        true -> code <= 57
        false -> false

fn finishInt(num: String, pos: Int) -> ParseOut
    match Int.fromString(num)
        Result.Ok(n) -> ParseOut.Got(Num.NumInt(n), pos)
        Result.Err(_) -> ParseOut.Bad("bad int", pos)

fn finishNumber(s: String, start: Int, endPos: Int, asFloat: Bool) -> ParseOut
    num = String.slice(s, start, endPos)
    match asFloat
        true -> ParseOut.Bad("float unsupported", endPos)
        false -> finishInt(num, endPos)

fn scanIntTail(s: String, pos: Int, start: Int, leadingZero: Bool) -> ParseOut
    match String.charAt(s, pos)
        Option.None -> finishNumber(s, start, pos, false)
        Option.Some(c) -> match isDigit(c)
            true -> match leadingZero
                true -> ParseOut.Bad("leading zero", pos)
                false -> scanIntTail(s, pos + 1, start, false)
            false -> ParseOut.Bad("trailing", pos)

fn startDigits(s: String, start: Int, c: String) -> ParseOut
    match isDigit(c)
        true -> scanIntTail(s, start + 1, start, false)
        false -> ParseOut.Bad("expected digit", start)

fn signDigit(s: String, pos: Int, start: Int, c: String) -> ParseOut
    match isDigit(c)
        true -> scanIntTail(s, pos + 1, start, false)
        false -> ParseOut.Bad("expected digit", pos)

fn parseSign(s: String, pos: Int, start: Int) -> ParseOut
    match String.charAt(s, pos)
        Option.None -> ParseOut.Bad("expected digit", pos)
        Option.Some(c) -> match c
            "0" -> scanIntTail(s, pos + 1, start, true)
            _ -> signDigit(s, pos, start, c)

fn parseNum(s: String, start: Int) -> ParseOut
    match String.charAt(s, start)
        Option.None -> ParseOut.Bad("empty", start)
        Option.Some(c) -> match c
            "-" -> parseSign(s, start + 1, start)
            "0" -> scanIntTail(s, start + 1, start, true)
            _ -> startDigits(s, start, c)

verify parseNum law fromIntRoundtrip
    given n: Int = [-7, 0, 42, 2500]
    parseNum(render(Num.NumInt(n)), 0) => ParseOut.Got(Num.NumInt(n), String.len(render(Num.NumInt(n))))
"#;
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-decimalrt-src");
    std::fs::create_dir_all(&src).expect("src dir");
    std::fs::write(src.join("m.av"), source).expect("write");
    let out = temp_output_dir("aver-decimalrt-out");
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
        .expect("aver proof ran");
    let json = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON:\n{}", format_output(&run)));
    let summary: serde_json::Value = serde_json::from_str(json).expect("json");
    assert_eq!(
        summary["universal"].as_bool(),
        Some(true),
        "a canonical decimal roundtrip law must close kernel-clean via the synthesized scan lemma + sign-split skeleton\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

/// A recursive ADT carried through `Tuple<...>`-typed params must get DEEP
/// ADT-measure fuel, not constant fuel. `type_measure_expr` had arms for
/// `List<` / `Option<` / `Map<` / `Result<` and the legacy parenthesized
/// `(A, B)` tuple spelling — but not for the `Tuple<A, B>` spelling Aver
/// type names actually use. Consequence (found on `examples/data/json.av`,
/// kernel-checked): a fn over `Tuple<String, Json>` got CONSTANT fuel and a
/// fn over `List<Tuple<String, Json>>` got the shallow spine measure
/// `AverMeasure.list (fun item => 1)`, so at nesting depth 5 the emitted
/// Lean model exhausted fuel and returned `default` — a WRONG model,
/// invisible to shallow samples. This test mirrors the Json object shape
/// (recursive ADT whose constructor carries `List<Tuple<String, T>>`) with
/// a depth-5 verify sample: with the `Tuple<` measure arm in place the
/// sample's `native_decide` computes the real value (`passed: true`); with
/// the arm reverted the wrapper's constant fuel exhausts mid-recursion and
/// the deep case evaluates to `default` → lake build error → `passed: false`.
#[test]
fn lean_deep_tuple_entries_get_adt_measure_fuel_when_lake_is_available() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping deep-tuple-entries test: `lake` not available");
        return;
    }
    let source = r#"module DeepEntries
    intent = "a recursive ADT carrying List<Tuple<String, T>> probed past constant fuel"
    effects []

type Tree
    TreeLeaf(Int)
    TreeNode(List<Tuple<String, Tree>>)

fn renderTree(t: Tree) -> String
    ? "Render a tree."
    match t
        Tree.TreeLeaf(n) -> String.fromInt(n)
        Tree.TreeNode(entries) -> "(" + renderEntries(entries) + ")"

fn renderEntries(entries: List<Tuple<String, Tree>>) -> String
    ? "Render an entry list."
    match entries
        [] -> ""
        [entry, ..tail] -> renderEntriesTail(entryString(entry), tail)

fn renderEntriesTail(acc: String, rest: List<Tuple<String, Tree>>) -> String
    ? "Render remaining entries onto an accumulator."
    match rest
        [] -> acc
        [entry, ..tail] -> renderEntriesTail(acc + "," + entryString(entry), tail)

fn entryString(entry: Tuple<String, Tree>) -> String
    ? "Render one entry."
    match entry
        (key, value) -> key + ":" + renderTree(value)

verify entryString
    entryString(("a", Tree.TreeLeaf(1))) => "a:1"
    entryString(("a", Tree.TreeNode([("b", Tree.TreeNode([("c", Tree.TreeNode([("d", Tree.TreeNode([("e", Tree.TreeNode([("f", Tree.TreeLeaf(1))]))]))]))]))]))) => "a:(b:(c:(d:(e:(f:1)))))"

verify renderEntries
    renderEntries([]) => ""
    renderEntries([("a", Tree.TreeNode([("b", Tree.TreeNode([("c", Tree.TreeNode([("d", Tree.TreeNode([("e", Tree.TreeLeaf(2))]))]))]))]))]) => "a:(b:(c:(d:(e:2))))"
"#;
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-deepentries-src");
    std::fs::create_dir_all(&src).expect("src dir");
    std::fs::write(src.join("m.av"), source).expect("write");
    let out = temp_output_dir("aver-deepentries-out");
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
        .expect("aver proof ran");
    let json = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON:\n{}", format_output(&run)));
    let summary: serde_json::Value = serde_json::from_str(json).expect("json");
    assert_eq!(
        summary["passed"].as_bool(),
        Some(true),
        "depth-5 entries through Tuple-typed params must compute (deep ADT-measure fuel), not exhaust constant fuel into `default`\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

/// A LinearArithmetic law over a fn using `Int.max`/`Int.min` must close. Two
/// bugs blocked it: (1) `Int.max` (lowercase leaf) leaked into the unfold set as
/// the non-existent constant `Int.max` → `unknown constant` compile error;
/// (2) even removed, `omega` treats `max` as opaque. Fix: filter `Int`/`Float`/
/// `Bool`/`String` scalar-namespace methods from the unfold set, and — gated on
/// the law actually using max/min — emit `simp only […, Int.max_def, Int.min_def]
/// <;> (try split) <;> simp_all <;> omega` (byte-identical otherwise). (Found on
/// the real `games/rogue/types.av` calcDamage = `Int.max(1, atk-def)`.)
#[test]
fn lean_proves_int_max_linear_arith_law_when_lake_is_available() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping Int.max test: `lake` not available");
        return;
    }
    let source = r#"module MaxPos
    intent = "an Int.max LinearArithmetic law must unfold Int.max_def + split"
    effects []

fn dmg(atk: Int, def: Int) -> Int
    Int.max(1, atk - def)

verify dmg law alwaysPositive
    given atk: Int = [0, 5, 10]
    given def: Int = [0, 3, 20]
    dmg(atk, def) > 0 => true
"#;
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-maxpos-src");
    std::fs::create_dir_all(&src).expect("src dir");
    std::fs::write(src.join("m.av"), source).expect("write");
    let out = temp_output_dir("aver-maxpos-out");
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
        .expect("aver proof ran");
    let json = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON:\n{}", format_output(&run)));
    let summary: serde_json::Value = serde_json::from_str(json).expect("json");
    assert_eq!(
        summary["universal"].as_bool(),
        Some(true),
        "an Int.max LinearArithmetic law must close kernel-clean\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

/// LUKA 2: generalizing induction and sibling-lemma injection must COMBINE. The
/// `dropRevLen` law's verified fn (`drop`) threads a Peano param (`n`) while
/// recursing on a list, so it needs `induction xs generalizing n`; AND it needs
/// the forward-homomorphism siblings (`lenAppend`, `lenRev`) rewriting inside
/// the induction arms. Before the fix these were mutually exclusive (the
/// `gen_given` branch was gated on `fast_simp.is_empty()`), so a law needing
/// both fell to plain `induction` and left a `sorry`. The arm simp set carries
/// Forward siblings only (loop-excluded via `simp_entries`) — no `←` unfold
/// rules — so no `maxHeartbeats` simp loop.
#[test]
fn lean_proves_generalizing_induction_with_siblings_when_lake_is_available() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping gen+siblings test: `lake` not available");
        return;
    }
    let source = r#"module GenSiblings
    intent = "generalizing induction + forward-sibling arm injection combined"
    effects []

type Nat
    Z
    S(Nat)

fn len(xs: List<Int>) -> Nat
    match xs
        [] -> Nat.Z
        [y, ..ys] -> Nat.S(len(ys))

fn drop(n: Nat, xs: List<Int>) -> List<Int>
    match n
        Nat.Z -> xs
        Nat.S(z) -> match xs
            [] -> []
            [x2, ..x3] -> drop(z, x3)

fn minus(x: Nat, y: Nat) -> Nat
    match x
        Nat.Z -> Nat.Z
        Nat.S(z) -> match y
            Nat.Z -> x
            Nat.S(x2) -> minus(z, x2)

fn plus(x: Nat, y: Nat) -> Nat
    match x
        Nat.Z -> y
        Nat.S(z) -> Nat.S(plus(z, y))

fn rev(xs: List<Int>) -> List<Int>
    match xs
        [] -> []
        [y, ..ys] -> List.concat(rev(ys), [y])

verify len law lenAppend
    given a: List<Int> = [[1], [1, 2, 3]]
    given b: List<Int> = [[7], [8, 9]]
    len(List.concat(a, b)) => plus(len(a), len(b))

verify len law lenRev
    given xs: List<Int> = [[], [1], [1, 2, 3]]
    len(rev(xs)) => len(xs)

verify drop law dropRevLen
    given n: Nat = [Nat.Z, Nat.S(Nat.Z), Nat.S(Nat.S(Nat.Z))]
    given xs: List<Int> = [[], [1], [1, 2, 3]]
    given ys: List<Int> = [[2], [3, 4]]
    len(List.concat(rev(drop(n, xs)), ys)) => plus(minus(len(xs), n), len(ys))
"#;
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-gensib-src");
    std::fs::create_dir_all(&src).expect("src dir");
    std::fs::write(src.join("m.av"), source).expect("write");
    let out = temp_output_dir("aver-gensib-out");
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
        .expect("aver proof ran");
    let json = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON:\n{}", format_output(&run)));
    let summary: serde_json::Value = serde_json::from_str(json).expect("json");
    assert_eq!(
        summary["universal"].as_bool(),
        Some(true),
        "generalizing induction + forward-sibling arm injection must combine to \
         close dropRevLen (the gen-vs-sibling mutual-exclusion gap)\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

/// Shared fixture for the escaped-string roundtrip tests below: a
/// log-field wire format structurally isomorphic to json's string
/// escape family but with FRESH names everywhere, a different escape
/// table (3 classifier pairs + tab via the control ladder vs json's
/// 5 + 2), different terminator / escape-introducer / hex-letter
/// chars (`;` / `%` / `x` vs `"` / `\` / `u`), an opening delimiter
/// that differs from the terminator (`?`), a consumer-only escape arm
/// the producer never emits (`%q`), and a smaller scanner SCC (6 fuel
/// mutual members vs json's 9 — a different probed rank). Proves the
/// `StringEscapeRoundtrip` recognizer keys on structure, not on the
/// json corpus's identifiers or table.
const FIELD_ESCAPE_FIXTURE: &str = r#"module FieldEsc
    intent = "an escaped log-field render/parse roundtrip closing via StringEscapeRoundtrip"
    effects []

type FieldVal
    FText(String)

type FieldOut
    Done(FieldVal, Int)
    Fail(String, Int)

fn escapeField(s: String) -> String
    ? "Escape a field value for the ;-terminated wire format."
    packField(String.chars(s), "")

verify escapeField law fieldRoundtrip
    given s: String = ["", "ok", "a;b", "x%y", "line\nz", "\t", "😀"]
    unpackField("?" + escapeField(s) + ";", 1) => FieldOut.Done(FieldVal.FText(s), String.len(escapeField(s)) + 2)

fn packField(chars: List<String>, acc: String) -> String
    ? "Escape each char onto an accumulator."
    match chars
        [] -> acc
        [c, ..rest] -> packField(rest, acc + packChar(c))

fn packChar(c: String) -> String
    ? "Two-char escapes for the wire format's reserved chars."
    match c
        ";" -> "%s"
        "%" -> "%p"
        "\n" -> "%n"
        _ -> packLow(c)

fn packLow(c: String) -> String
    ? "Tab gets a two-char escape; other control chars go hex."
    code = Char.toCode(c)
    match code == 9
        true -> "%t"
        false -> match code < 32
            true -> packHex(c, code)
            false -> c

fn packHex(c: String, code: Int) -> String
    ? "Hex escape for a control char."
    match Byte.toHex(code)
        Result.Ok(hex) -> "%x00" + hex
        Result.Err(_) -> c

fn fieldHexVal(c: String) -> Option<Int>
    ? "Hex digit value: 0-9, a-f. None for non-hex."
    code = Char.toCode(c)
    match code >= 48
        true -> match code <= 57
            true -> Option.Some(code - 48)
            false -> fieldHexAlpha(code)
        false -> Option.None

fn fieldHexAlpha(code: Int) -> Option<Int>
    ? "Lowercase hex letter value."
    match code >= 97
        true -> match code <= 102
            true -> Option.Some(code - 87)
            false -> Option.None
        false -> Option.None

fn unpackField(s: String, pos: Int) -> FieldOut
    ? "Parse one ;-terminated field starting at pos."
    unpackChunk(s, pos, pos, [])

fn unpackChunk(s: String, pos: Int, segStart: Int, parts: List<String>) -> FieldOut
    ? "Scan the field, collecting segment chunks."
    match String.charAt(s, pos)
        Option.None -> FieldOut.Fail("unterminated field", pos)
        Option.Some(c) -> match c
            ";" -> sealField(s, pos + 1, segStart, parts)
            "%" -> unpackEscape(s, pos + 1, pos, segStart, parts)
            _ -> guardChar(s, pos, segStart, parts, c)

verify unpackChunk law packedFieldRoundtrip
    given s: String = ["", "ok", "a;b"]
    unpackChunk("?" + escapeField(s) + ";", 1, 1, []) => FieldOut.Done(FieldVal.FText(s), String.len(escapeField(s)) + 2)

fn sealField(s: String, nextPos: Int, segStart: Int, parts: List<String>) -> FieldOut
    ? "Flush the open segment and finish the field."
    segment = String.slice(s, segStart, nextPos - 1)
    all = String.join(List.concat(parts, [segment]), "")
    FieldOut.Done(FieldVal.FText(all), nextPos)

fn guardChar(s: String, pos: Int, segStart: Int, parts: List<String>, c: String) -> FieldOut
    ? "Reject unescaped control chars; pass printable chars."
    match Char.toCode(c) < 32
        true -> FieldOut.Fail("unescaped control char", pos)
        false -> unpackChunk(s, pos + 1, segStart, parts)

fn unpackEscape(s: String, pos: Int, pctPos: Int, segStart: Int, parts: List<String>) -> FieldOut
    ? "Decode one escape letter."
    segment = String.slice(s, segStart, pctPos)
    base = List.concat(parts, [segment])
    match String.charAt(s, pos)
        Option.None -> FieldOut.Fail("unterminated escape", pos)
        Option.Some(c) -> match c
            "s" -> unpackChunk(s, pos + 1, pos + 1, List.concat(base, [";"]))
            "p" -> unpackChunk(s, pos + 1, pos + 1, List.concat(base, ["%"]))
            "n" -> unpackChunk(s, pos + 1, pos + 1, List.concat(base, ["\n"]))
            "t" -> unpackChunk(s, pos + 1, pos + 1, List.concat(base, ["\t"]))
            "q" -> unpackChunk(s, pos + 1, pos + 1, List.concat(base, ["?"]))
            "x" -> unpackHex(s, pos + 1, pos + 1, base)
            _ -> FieldOut.Fail("unknown escape", pos)

fn unpackHex(s: String, pos: Int, escapePos: Int, parts: List<String>) -> FieldOut
    ? "Decode a %x00HH hex escape."
    match readHexQuad(s, pos, 0, 0)
        Option.None -> FieldOut.Fail("bad hex escape", escapePos)
        Option.Some(cp) -> unpackCodePoint(s, pos + 4, escapePos, parts, cp)

fn unpackCodePoint(s: String, pos: Int, escapePos: Int, parts: List<String>, cp: Int) -> FieldOut
    ? "Reject surrogate halves; place anything else."
    match highHalf(cp)
        true -> FieldOut.Fail("surrogate", escapePos)
        false -> match lowHalf(cp)
            true -> FieldOut.Fail("surrogate", escapePos)
            false -> placeCodePoint(s, pos, escapePos, parts, cp)

fn highHalf(cp: Int) -> Bool
    ? "High surrogate range check."
    match cp >= 55296
        true -> cp <= 56319
        false -> false

fn lowHalf(cp: Int) -> Bool
    ? "Low surrogate range check."
    match cp >= 56320
        true -> cp <= 57343
        false -> false

fn placeCodePoint(s: String, pos: Int, escapePos: Int, parts: List<String>, cp: Int) -> FieldOut
    ? "Flush the decoded char as its own chunk."
    match Char.fromCode(cp)
        Option.None -> FieldOut.Fail("bad code point", escapePos)
        Option.Some(ch) -> unpackChunk(s, pos, pos, List.concat(parts, [ch]))

fn readHexQuad(s: String, pos: Int, acc: Int, count: Int) -> Option<Int>
    ? "Read exactly four hex digits."
    match count == 4
        true -> Option.Some(acc)
        false -> match String.charAt(s, pos)
            Option.None -> Option.None
            Option.Some(c) -> match fieldHexVal(c)
                Option.None -> Option.None
                Option.Some(v) -> readHexQuad(s, pos + 1, acc * 16 + v, count + 1)
"#;

/// A no-when universal escaped-string roundtrip
/// (`parse(<open> + escape(s) + <term>, 1) = Ok(StrCtor(s), len + 2)`)
/// over the canonical segment-chunking scanner must close GENUINELY
/// via `StringEscapeRoundtrip`: the detector validates the
/// producer/consumer escape-table alignment end-to-end and the Lean
/// backend renders the suffix-invariant skeleton from the verified
/// json hand proof (drop-form suffix cursor, fold accumulator
/// homomorphism, per-fuel-arm step lemmas, by_cases classification
/// ladder). `universal:true` means every theorem closed with axioms
/// ⊆ [propext, Classical.choice, Quot.sound] — no sorry, no
/// native_decide. The fixture's fresh names, different escape table,
/// distinct delimiters and smaller scanner SCC pin that the
/// recognizer keys on structure, not the json corpus's identifiers.
/// (Found on real `examples/data/json.av`:
/// `escapeJsonString.parseStringRoundtrip` +
/// `parseStringChunk.escapedStringRoundtrip` — the 2 → 0 budget
/// flip.)
#[test]
fn lean_proves_string_escape_roundtrip_law_when_lake_is_available() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping string-escape-roundtrip test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-fieldesc-src");
    std::fs::create_dir_all(&src).expect("src dir");
    std::fs::write(src.join("m.av"), FIELD_ESCAPE_FIXTURE).expect("write");
    let out = temp_output_dir("aver-fieldesc-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("m.av"))
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .arg("--sorry-budget")
        .arg("0")
        .output()
        .expect("aver proof ran");
    let json = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON:\n{}", format_output(&run)));
    let summary: serde_json::Value = serde_json::from_str(json).expect("json");
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(0),
        "a fresh-named escaped-string roundtrip must close sorry-free\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["universal"].as_bool(),
        Some(true),
        "a fresh-named escaped-string roundtrip must close kernel-clean via the \
         synthesized suffix-invariant skeleton (recognizer keys on structure, \
         not json's identifiers)\n{}",
        format_output(&run)
    );
    // The strategy itself must have fired (not some other route): the
    // synthesized chunk invariant is the skeleton's signature lemma.
    let lean = std::fs::read_to_string(out.join("FieldEsc.lean")).expect("FieldEsc.lean");
    assert!(
        lean.contains("_chunk_inv :"),
        "the suffix-invariant skeleton must be the closing proof\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

/// Conservatism gate: a producer/consumer pair whose escape tables do
/// NOT align (the consumer decodes the newline escape letter to a
/// CARRIAGE RETURN) must decline the `StringEscapeRoundtrip` pin —
/// the synthesized proof could never close, and an unprovable
/// synthesized lemma stack is the worst failure mode. The export must
/// contain NO suffix-invariant skeleton and the universal theorem
/// falls back to the prior honest emission. No lake needed —
/// generation-only.
#[test]
fn string_escape_roundtrip_declines_misaligned_escape_table() {
    let misaligned = FIELD_ESCAPE_FIXTURE.replace(
        r#""n" -> unpackChunk(s, pos + 1, pos + 1, List.concat(base, ["\n"]))"#,
        r#""n" -> unpackChunk(s, pos + 1, pos + 1, List.concat(base, ["\r"]))"#,
    );
    assert_ne!(
        misaligned, FIELD_ESCAPE_FIXTURE,
        "fixture mutation must apply"
    );
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-fieldesc-neg-src");
    std::fs::create_dir_all(&src).expect("src dir");
    std::fs::write(src.join("m.av"), misaligned).expect("write");
    let out = temp_output_dir("aver-fieldesc-neg-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("m.av"))
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .output()
        .expect("aver proof ran");
    let lean = std::fs::read_to_string(out.join("FieldEsc.lean")).unwrap_or_else(|e| {
        panic!(
            "FieldEsc.lean must be generated ({e})\n{}",
            format_output(&run)
        )
    });
    assert!(
        !lean.contains("_chunk_inv :"),
        "a misaligned escape table must DECLINE the synthesis (no invariant skeleton)\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

/// The honest floor, observed end-to-end: with the TEST-ONLY
/// `AVER_PROOF_ESCAPE_SABOTAGE` hook breaking the control branch's
/// tactic block in both laws' synthesized stacks, the branch theorems
/// fall to their caught `sorry` floors — the build still SUCCEEDS
/// (`passed:true` within the budget) and the sorry count surfaces the
/// regression loudly (2, one per law). A template regression can
/// degrade the budget pin but can never be a hard `lake build` error.
/// (The revert-test norm: proves the floor catches, not just that the
/// happy path closes.)
#[test]
fn lean_escape_roundtrip_template_regression_degrades_to_caught_sorry_when_lake_is_available() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping escape-sabotage test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-fieldesc-sab-src");
    std::fs::create_dir_all(&src).expect("src dir");
    std::fs::write(src.join("m.av"), FIELD_ESCAPE_FIXTURE).expect("write");
    let out = temp_output_dir("aver-fieldesc-sab-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("m.av"))
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .arg("--sorry-budget")
        .arg("2")
        .env("AVER_PROOF_ESCAPE_SABOTAGE", "1")
        .output()
        .expect("aver proof ran");
    let json = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON:\n{}", format_output(&run)));
    let summary: serde_json::Value = serde_json::from_str(json).expect("json");
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(2),
        "a sabotaged branch template must fall to exactly one caught sorry per law\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["passed"].as_bool(),
        Some(true),
        "a sabotaged branch template must stay a CAUGHT sorry (build success), \
         never a hard lake error\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

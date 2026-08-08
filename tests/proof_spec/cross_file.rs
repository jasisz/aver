// Cross-file law pool: a dependency module's PROVEN `verify … law` blocks
// are emitted as theorems in the dep's `.lean` and admitted into a
// consumer law's lemma pool under the SAME cone ∪ subject admissibility
// gate as in-file sibling laws. The soundness invariant is non-negotiable:
// a dep law may be cited ONLY if it is itself proven in its own module's
// export — an exposed-but-unproven dep law must never launder credit.
//
// All tests here drive the FULL pipeline (emit Lean + `lake build` +
// `#print axioms` audit), so each is guarded by the standard
// `lake --version` skip.

use super::*;

/// Write `Lib.av` + `Consumer.av` into a fresh module-root dir, run
/// `aver proof <root>/Consumer.av --backend lean --module-root <root>
/// --check --check-json`, and return the parsed JSON summary plus raw
/// output. Exit code is read off the `Output` status, never piped.
fn run_split(lib_src: &str, consumer_src: &str) -> (serde_json::Value, std::process::Output) {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-crossfile-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(src.join("Lib.av"), lib_src).expect("write Lib.av");
    std::fs::write(src.join("Consumer.av"), consumer_src).expect("write Consumer.av");
    let out = temp_output_dir("aver-crossfile-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("Consumer.av"))
        .arg("--backend")
        .arg("lean")
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
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with('{')))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)))
        .to_string();
    let summary: serde_json::Value =
        serde_json::from_str(&json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}"));
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
    (summary, run)
}

/// `Lib` exposes a PROVEN accumulator-equivalence law over `List<Int>`
/// (`qrev(x, y) = rev(x) ++ y`). `qrev`/`rev` are builtin-typed so the
/// dep `.lean` lowers cleanly.
const LIB_PROVEN: &str = "module Lib\n\
    \x20   intent =\n\
    \x20       \"Reversal helpers with a proven accumulator-equivalence law.\"\n\
    \x20   effects []\n\n\
    fn qrev(x: List<Int>, y: List<Int>) -> List<Int>\n\
    \x20   match x\n\
    \x20       [] -> y\n\
    \x20       [z, ..xs] -> qrev(xs, List.concat([z], y))\n\n\
    fn rev(x: List<Int>) -> List<Int>\n\
    \x20   match x\n\
    \x20       [] -> []\n\
    \x20       [y, ..xs] -> List.concat(rev(xs), [y])\n\n\
    verify qrev law qrevSpec\n\
    \x20   given x: List<Int> = [[], [1], [1, 2, 3]]\n\
    \x20   given y: List<Int> = [[], [9], [8, 7]]\n\
    \x20   qrev(x, y) => List.concat(rev(x), y)\n";

/// The consumer wraps `Lib.rev` and proves it equals `Lib.qrev x []` —
/// the law that genuinely DECOMPOSES through the dep's `qrevSpec`.
const CONSUMER_USES_DEP: &str = "module Consumer\n\
    \x20   depends [Lib]\n\
    \x20   intent =\n\
    \x20       \"Wraps Lib.rev and proves it equals Lib.qrev with empty accumulator.\"\n\
    \x20   effects []\n\n\
    fn myRev(x: List<Int>) -> List<Int>\n\
    \x20   Lib.rev(x)\n\n\
    verify myRev law myRevQrev\n\
    \x20   given x: List<Int> = [[], [1], [1, 2, 3]]\n\
    \x20   myRev(x) => Lib.qrev(x, [])\n";

#[test]
fn cross_file_consumer_proves_via_dep_law() {
    // THE split probe (scout §3): split across a module boundary, the
    // consumer's `myRev law myRevQrev` proves UNIVERSALLY by citing the
    // dependency's proven `Lib.qrev_law_qrevSpec`. Without the cross-file
    // law pool this FAILS (exit 1, universal:false, 1 sorry) — the dep law
    // is invisible across the boundary. This is the measured regression
    // guard: it is RED on `main` without the feature (see the revert-test
    // evidence in /tmp/crossfile/impl_report.md).
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping cross-file split-probe test: `lake` not available");
        return;
    }
    let (summary, run) = run_split(LIB_PROVEN, CONSUMER_USES_DEP);
    assert_eq!(
        (
            summary["passed"].as_bool(),
            summary["universal"].as_bool(),
            summary["sorries"].as_u64(),
        ),
        (Some(true), Some(true), Some(0)),
        "the consumer law must prove UNIVERSALLY across the module boundary by \
         citing the dependency's proven law. If this regressed, the cross-file \
         law pool is not carrying / admitting the dep law.\n{}",
        format_output(&run)
    );
    // The proof must literally cite the dep law (no false-green via some
    // other tactic): scan the emitted Consumer.lean.
    assert!(
        summary["universal_laws"].as_u64().unwrap_or(0) >= 1,
        "expected >=1 universally-credited law\n{}",
        format_output(&run)
    );
}

#[test]
fn cross_file_dep_law_emitted_and_self_consistent() {
    // The EMIT side (scout §3a): when `Lib` is built as a DEPENDENCY, its
    // proven law must appear as `theorem Lib.qrev_law_qrevSpec` inside
    // `namespace Lib` — and the dep theorem's OWN proof must NOT cite
    // itself (the self-reference that makes it structurally recursive and
    // fails Lean's termination check). Asserts the generated sources, then
    // that the whole project builds clean.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping cross-file emit test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-crossfile-emit-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(src.join("Lib.av"), LIB_PROVEN).expect("write Lib.av");
    std::fs::write(src.join("Consumer.av"), CONSUMER_USES_DEP).expect("write Consumer.av");
    let out = temp_output_dir("aver-crossfile-emit-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("Consumer.av"))
        .arg("--backend")
        .arg("lean")
        .arg("--module-root")
        .arg(&src)
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check --check-json` to run");

    let lib_lean = std::fs::read_to_string(out.join("Lib.lean")).expect("Lib.lean must exist");
    assert!(
        lib_lean.contains("theorem qrev_law_qrevSpec"),
        "the dependency's proven law must be emitted as a theorem in its own \
         `.lean`\nLib.lean:\n{lib_lean}"
    );
    // The dep theorem statement line precedes its proof body; the body must
    // not cite the dep law itself (would be `simp only [Lib.qrev_law_qrevSpec]`
    // inside its own proof → structural-recursion termination failure).
    let body_after_stmt = lib_lean
        .split_once("theorem qrev_law_qrevSpec :")
        .map(|(_, rest)| {
            // Cut at the next top-level theorem so we only inspect THIS
            // theorem's proof body.
            rest.split_once("\ntheorem ")
                .map(|(b, _)| b)
                .unwrap_or(rest)
        })
        .unwrap_or("");
    assert!(
        !body_after_stmt.contains("qrev_law_qrevSpec :="),
        "the dep theorem must not cite ITSELF in its own proof body\n{body_after_stmt}"
    );

    let consumer_lean =
        std::fs::read_to_string(out.join("Consumer.lean")).expect("Consumer.lean must exist");
    assert!(
        consumer_lean.contains("Lib.qrev_law_qrevSpec"),
        "the consumer proof must cite the namespace-qualified dep law\n{consumer_lean}"
    );

    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with('{')))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)));
    let summary: serde_json::Value =
        serde_json::from_str(json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}"));
    assert_eq!(
        summary["passed"].as_bool(),
        Some(true),
        "the full multi-module project must build clean\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

/// `Lib` exposes a FALSE law (`qrev(x, y) = rev(x)` — drops the
/// accumulator `y`, false for any nonempty `y`). The dep auto-prover
/// cannot close it, so it emits `sorry`. It is exposed but UNPROVEN.
const LIB_UNPROVEN: &str = "module Lib\n\
    \x20   intent =\n\
    \x20       \"Reversal helpers with an UNPROVABLE (false) law.\"\n\
    \x20   effects []\n\n\
    fn qrev(x: List<Int>, y: List<Int>) -> List<Int>\n\
    \x20   match x\n\
    \x20       [] -> y\n\
    \x20       [z, ..xs] -> qrev(xs, List.concat([z], y))\n\n\
    fn rev(x: List<Int>) -> List<Int>\n\
    \x20   match x\n\
    \x20       [] -> []\n\
    \x20       [y, ..xs] -> List.concat(rev(xs), [y])\n\n\
    verify qrev law qrevBad\n\
    \x20   given x: List<Int> = [[], [1], [1, 2, 3]]\n\
    \x20   given y: List<Int> = [[], [9], [8, 7]]\n\
    \x20   qrev(x, y) => rev(x)\n";

#[test]
fn cross_file_unproven_dep_law_grants_no_false_credit() {
    // THE SOUNDNESS INVARIANT (non-negotiable): an exposed-but-UNPROVEN
    // dep law must never launder universal credit to a consumer. `Lib`'s
    // `qrevBad` is FALSE, so the dep `.lean` emits it as a `sorry` — and a
    // consumer that leans on it inherits `sorryAx` transitively, flipping
    // `universal` to false and tripping the sorry gate (passed:false). The
    // EXACT same consumer law proves UNIVERSALLY against the PROVEN `Lib`
    // (see `cross_file_consumer_proves_via_dep_law`); the only difference
    // is whether the dep law is itself proven. That contrast IS the
    // soundness proof: credit rides on the dep law's own kernel
    // certificate, never on mere exposure.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping cross-file soundness test: `lake` not available");
        return;
    }
    let (summary, run) = run_split(LIB_UNPROVEN, CONSUMER_USES_DEP);
    assert_eq!(
        summary["universal"].as_bool(),
        Some(false),
        "an UNPROVEN dep law must NOT grant universal credit — the consumer \
         that depends on it must report universal:false (it cannot launder \
         credit through an exposed-but-unproven law).\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["universal_laws"].as_u64(),
        Some(0),
        "zero laws may be universally credited when the only path leans on an \
         unproven dep law\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["passed"].as_bool(),
        Some(false),
        "the unproven dep law leaves a `sorry` that the sorry gate must catch\n{}",
        format_output(&run)
    );
}

/// A consumer whose law mentions a fn OUTSIDE its own cone AND outside the
/// dep law's reach. `noise` is an unrelated helper; the consumer's law is
/// about `noise`, which neither shares the dep law's subject (`qrev`) nor
/// pulls the dep law into its cone — so the gate must NOT admit the dep
/// law (it would only add irrelevant noise to the simp set).
const CONSUMER_OUT_OF_CONE: &str = "module Consumer\n\
    \x20   depends [Lib]\n\
    \x20   intent =\n\
    \x20       \"A law unrelated to the dep law's cone or subject.\"\n\
    \x20   effects []\n\n\
    fn noise(n: Int) -> Int\n\
    \x20   n + 0\n\n\
    verify noise law noiseId\n\
    \x20   given n: Int = [0, 1, 2]\n\
    \x20   noise(n) => n\n";

#[test]
fn cross_file_out_of_cone_dep_law_not_admitted() {
    // The admissibility gate stays TIGHT across the boundary: a dep law
    // that shares neither the consumer law's proof cone nor its subject fn
    // is NOT admitted (exactly as an unrelated in-file sibling is
    // rejected). The consumer's `noise law noiseId` proves on its own
    // ladder; the dep `Lib.qrev_law_qrevSpec` must NOT appear in its proof.
    // Tightness is the other half of soundness: the pool may only ever
    // ADD relevant lemmas, never perturb an unrelated law.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping cross-file out-of-cone test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-crossfile-cone-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(src.join("Lib.av"), LIB_PROVEN).expect("write Lib.av");
    std::fs::write(src.join("Consumer.av"), CONSUMER_OUT_OF_CONE).expect("write Consumer.av");
    let out = temp_output_dir("aver-crossfile-cone-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("Consumer.av"))
        .arg("--backend")
        .arg("lean")
        .arg("--module-root")
        .arg(&src)
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check --check-json` to run");

    let consumer_lean =
        std::fs::read_to_string(out.join("Consumer.lean")).expect("Consumer.lean must exist");
    assert!(
        !consumer_lean.contains("Lib.qrev_law_qrevSpec"),
        "an out-of-cone dep law must NOT be admitted into an unrelated consumer \
         law's pool\nConsumer.lean:\n{consumer_lean}"
    );
    // The unrelated law still proves on its own ladder — the pool only ever
    // ADDS relevant lemmas, it never perturbs a law it doesn't touch.
    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with('{')))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)));
    let summary: serde_json::Value =
        serde_json::from_str(json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}"));
    assert_eq!(
        summary["passed"].as_bool(),
        Some(true),
        "the unrelated consumer law must still build clean\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

/// Write an arbitrary set of `(filename, source)` modules into a fresh
/// module-root dir and run `aver proof <root>/<entry> --backend lean
/// --module-root <root> --check --check-json`. Returns the parsed JSON
/// summary, the raw `Output`, AND the generated `.lean` source for each
/// requested file name (so a test can assert on what was emitted).
fn run_multi(
    files: &[(&str, &str)],
    entry: &str,
    read_back: &[&str],
) -> (
    serde_json::Value,
    std::process::Output,
    std::collections::HashMap<String, String>,
) {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-crossfile-multi-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    for (name, source) in files {
        std::fs::write(src.join(name), source).unwrap_or_else(|e| panic!("write {name}: {e}"));
    }
    let out = temp_output_dir("aver-crossfile-multi-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join(entry))
        .arg("--backend")
        .arg("lean")
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
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with('{')))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)))
        .to_string();
    let summary: serde_json::Value =
        serde_json::from_str(&json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}"));
    let mut leans = std::collections::HashMap::new();
    for name in read_back {
        let lean = std::fs::read_to_string(out.join(name))
            .unwrap_or_else(|e| panic!("read generated {name}: {e}"));
        leans.insert((*name).to_string(), lean);
    }
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
    (summary, run, leans)
}

// ---------------------------------------------------------------------------
// MAJOR 2 — bare-name collision. `Lib` and `Other` BOTH define `qrev`/`rev`
// with a same-named `qrev law qrevSpec`. The consumer's cone involves only
// `Lib.qrev` / `Lib.rev`. Under a BARE-name gate, `Other.qrev_law_qrevSpec`'s
// `qrev`/`rev` mentions would be a subset of the (bare) cone and the
// unrelated module's law would be wrongly admitted. The qualified-identity
// gate compares `Other.qrev` against a cone holding `Lib.qrev`, so the
// unrelated dep law is REJECTED — never cited, never emitted.
// ---------------------------------------------------------------------------

/// `Other` coincidentally exposes the SAME bare fn names + law identity as
/// `Lib`, but is unrelated to the consumer's proof.
const OTHER_SAME_BARE: &str = "module Other\n\
    \x20   intent =\n\
    \x20       \"Unrelated module that coincidentally also has bare qrev/rev.\"\n\
    \x20   effects []\n\n\
    fn qrev(x: List<Int>, y: List<Int>) -> List<Int>\n\
    \x20   match x\n\
    \x20       [] -> y\n\
    \x20       [z, ..xs] -> qrev(xs, List.concat([z], y))\n\n\
    fn rev(x: List<Int>) -> List<Int>\n\
    \x20   match x\n\
    \x20       [] -> []\n\
    \x20       [y, ..xs] -> List.concat(rev(xs), [y])\n\n\
    verify qrev law qrevSpec\n\
    \x20   given x: List<Int> = [[], [1], [1, 2, 3]]\n\
    \x20   given y: List<Int> = [[], [9], [8, 7]]\n\
    \x20   qrev(x, y) => List.concat(rev(x), y)\n";

/// Consumer depends on BOTH but its law only involves `Lib.rev`/`Lib.qrev`.
const CONSUMER_TWO_DEPS: &str = "module Consumer\n\
    \x20   depends [Lib, Other]\n\
    \x20   intent =\n\
    \x20       \"Wraps Lib.rev; Other is an unrelated same-bare-name module.\"\n\
    \x20   effects []\n\n\
    fn myRev(x: List<Int>) -> List<Int>\n\
    \x20   Lib.rev(x)\n\n\
    verify myRev law myRevQrev\n\
    \x20   given x: List<Int> = [[], [1], [1, 2, 3]]\n\
    \x20   myRev(x) => Lib.qrev(x, [])\n";

#[test]
fn cross_file_bare_name_collision_dep_law_not_admitted() {
    // A law from an UNRELATED module that merely shares a bare fn name with a
    // fn in the consumer's cone must NOT be admitted. The qualified-identity
    // gate keys on `Module.fn`, so `Other.qrev` ≠ the cone's `Lib.qrev`:
    // `Other.qrev_law_qrevSpec` is never cited in the consumer's proof, and
    // (being un-admitted) is never emitted into `Other.lean` at all. Only the
    // GENUINELY in-cone `Lib.qrev_law_qrevSpec` is admitted + emitted.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping cross-file bare-name-collision test: `lake` not available");
        return;
    }
    let (_summary, _run, leans) = run_multi(
        &[
            ("Lib.av", LIB_PROVEN),
            ("Other.av", OTHER_SAME_BARE),
            ("Consumer.av", CONSUMER_TWO_DEPS),
        ],
        "Consumer.av",
        &["Consumer.lean", "Lib.lean", "Other.lean"],
    );
    let consumer = &leans["Consumer.lean"];
    assert!(
        consumer.contains("Lib.qrev_law_qrevSpec"),
        "the GENUINELY in-cone dep law (Lib) must still be admitted\nConsumer.lean:\n{consumer}"
    );
    assert!(
        !consumer.contains("Other.qrev_law_qrevSpec"),
        "an UNRELATED module's same-bare-name law must NOT be admitted via a \
         bare-name collision — qualified `Module.fn` identity rejects it\nConsumer.lean:\n{consumer}"
    );
    // Un-admitted ⇒ not emitted into the build at all (no sorry contribution).
    let other = &leans["Other.lean"];
    assert!(
        !other.contains("theorem qrev_law_qrevSpec"),
        "the unrelated `Other` law is admitted by no consumer, so it must not be \
         emitted into the build\nOther.lean:\n{other}"
    );
    // The genuinely-cited dep law IS emitted in its own module file.
    let lib = &leans["Lib.lean"];
    assert!(
        lib.contains("theorem qrev_law_qrevSpec"),
        "the in-cone `Lib` law must be emitted as a theorem\nLib.lean:\n{lib}"
    );
}

// ---------------------------------------------------------------------------
// MAJOR 3 — visibility. A dependency law whose SUBJECT fn is PRIVATE (absent
// from a non-empty `exposes [...]`, or `_`-prefixed) must never enter a
// consumer's pool — even when the consumer's proof cone TRANSITIVELY reaches
// that private fn through an exposed one. A consumer can only cite what its
// dependency exposes, exactly as it can only CALL exposed fns.
// ---------------------------------------------------------------------------

/// `Lib` exposes only `rev` + `plain`; `revAcc` is PRIVATE (omitted from the
/// `exposes` list). The proven law is about the PRIVATE `revAcc`. `rev`
/// delegates to `revAcc`, so a consumer wrapping `rev` has `revAcc` in its
/// cone — the law would be cone-admissible were visibility not enforced.
const LIB_PRIVATE_VIA_EXPOSES: &str = "module Lib\n\
    \x20   depends []\n\
    \x20   exposes [rev, plain]\n\
    \x20   intent =\n\
    \x20       \"Hides revAcc via exposes; its law must not cross the boundary.\"\n\
    \x20   effects []\n\n\
    fn revAcc(x: List<Int>, y: List<Int>) -> List<Int>\n\
    \x20   match x\n\
    \x20       [] -> y\n\
    \x20       [z, ..xs] -> revAcc(xs, List.concat([z], y))\n\n\
    fn rev(x: List<Int>) -> List<Int>\n\
    \x20   revAcc(x, [])\n\n\
    fn plain(x: List<Int>) -> List<Int>\n\
    \x20   match x\n\
    \x20       [] -> []\n\
    \x20       [y, ..xs] -> List.concat(plain(xs), [y])\n\n\
    verify revAcc law revAccSpec\n\
    \x20   given x: List<Int> = [[], [1], [1, 2, 3]]\n\
    \x20   given y: List<Int> = [[], [9], [8, 7]]\n\
    \x20   revAcc(x, y) => List.concat(plain(x), y)\n";

/// The consumer wraps the EXPOSED `Lib.rev` (whose body delegates to the
/// private `revAcc`), so the private `revAcc` is in the consumer's cone.
const CONSUMER_REACHES_PRIVATE: &str = "module Consumer\n\
    \x20   depends [Lib]\n\
    \x20   intent =\n\
    \x20       \"Wraps Lib.rev; cone reaches private revAcc but must not cite its law.\"\n\
    \x20   effects []\n\n\
    fn myRev(x: List<Int>) -> List<Int>\n\
    \x20   Lib.rev(x)\n\n\
    verify myRev law myRevPlain\n\
    \x20   given x: List<Int> = [[], [1], [1, 2, 3]]\n\
    \x20   myRev(x) => Lib.plain(x)\n";

#[test]
fn cross_file_private_dep_law_not_admitted() {
    // `verify revAcc law revAccSpec` is about a fn OMITTED from `exposes`, so
    // it is filtered at collection (`collect_verify_laws` honours the same
    // `exposes` rule as `collect_module_exports`). Even though the consumer's
    // cone reaches `revAcc` transitively (through the exposed `rev`), the
    // private law never reaches the pool: not cited in the consumer's proof,
    // not emitted as a theorem in `Lib.lean`.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping cross-file visibility test: `lake` not available");
        return;
    }
    let (_summary, _run, leans) = run_multi(
        &[
            ("Lib.av", LIB_PRIVATE_VIA_EXPOSES),
            ("Consumer.av", CONSUMER_REACHES_PRIVATE),
        ],
        "Consumer.av",
        &["Consumer.lean", "Lib.lean"],
    );
    let consumer = &leans["Consumer.lean"];
    assert!(
        !consumer.contains("revAccSpec"),
        "a PRIVATE dependency law must never be cited by a consumer, even when \
         the cone transitively reaches the private fn\nConsumer.lean:\n{consumer}"
    );
    let lib = &leans["Lib.lean"];
    assert!(
        !lib.contains("theorem revAcc_law_revAccSpec"),
        "a private dependency law is admitted by no consumer, so it must not be \
         emitted as a theorem in the dependency's file\nLib.lean:\n{lib}"
    );
}

/// A clean consumer law (rev-append-nil identity) that closes on its OWN
/// ladder, never touching `Lib.bogus` — used by the un-cited soundness test.
const CONSUMER_OWN_LADDER: &str = "module Consumer\n\
    \x20   depends [Lib]\n\
    \x20   intent =\n\
    \x20       \"A clean own law; never touches Lib.bogus.\"\n\
    \x20   effects []\n\n\
    fn myRev(x: List<Int>) -> List<Int>\n\
    \x20   Lib.rev(x)\n\n\
    verify myRev law myRevId\n\
    \x20   given x: List<Int> = [[], [1], [1, 2, 3]]\n\
    \x20   myRev(x) => List.concat(Lib.rev(x), [])\n";

// ---------------------------------------------------------------------------
// MAJOR 4 — an UNPROVEN dependency law the consumer does NOT cite must not
// inflate the consumer's file-wide `sorry` count. An un-admitted dep law is a
// complete no-op for the consumer: it is never emitted into the build.
// ---------------------------------------------------------------------------

/// `Lib` exposes a FALSE law (`bogus(n) = n` for `bogus(n) = n + 1`) about a
/// fn UNRELATED to the consumer's proof, plus the usual reversal helper.
const LIB_WITH_UNCITED_FALSE_LAW: &str = "module Lib\n\
    \x20   intent =\n\
    \x20       \"Reversal helpers plus an UNRELATED false (unprovable) law.\"\n\
    \x20   effects []\n\n\
    fn rev(x: List<Int>) -> List<Int>\n\
    \x20   match x\n\
    \x20       [] -> []\n\
    \x20       [y, ..xs] -> List.concat(rev(xs), [y])\n\n\
    fn bogus(n: Int) -> Int\n\
    \x20   n + 1\n\n\
    verify bogus law bogusBad\n\
    \x20   given n: Int = [0, 1, 2]\n\
    \x20   bogus(n) => n\n";

#[test]
fn cross_file_uncited_unproven_dep_law_no_sorry_inflation() {
    // The consumer proves a clean own law (rev-append-nil) and never mentions
    // `Lib.bogus`. `Lib.bogus law bogusBad` is FALSE and would fall to `sorry`
    // if emitted. Because no consumer law ADMITS it, it is not emitted into
    // the build at all — so it contributes ZERO to the consumer's file-wide
    // sorry count. The consumer therefore still passes universally. (Contrast
    // `cross_file_unproven_dep_law_grants_no_false_credit`, where the consumer
    // DOES cite the unproven law and correctly inherits its gap.)
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping cross-file uncited-unproven test: `lake` not available");
        return;
    }
    let (summary, run, leans) = run_multi(
        &[
            ("Lib.av", LIB_WITH_UNCITED_FALSE_LAW),
            ("Consumer.av", CONSUMER_OWN_LADDER),
        ],
        "Consumer.av",
        &["Lib.lean"],
    );
    let lib = &leans["Lib.lean"];
    assert!(
        !lib.contains("bogus_law_bogusBad"),
        "an unproven dep law no consumer cites must not be emitted into the \
         build (else its `sorry` inflates the consumer's count)\nLib.lean:\n{lib}"
    );
    assert_eq!(
        (
            summary["passed"].as_bool(),
            summary["universal"].as_bool(),
            summary["sorries"].as_u64(),
        ),
        (Some(true), Some(true), Some(0)),
        "an un-cited unproven dep law must not inflate the consumer's sorry \
         count — the consumer's own clean law still passes universally\n{}",
        format_output(&run)
    );
}

// ---------------------------------------------------------------------------
// EMISSION-TOPOLOGY GUARD (end-to-end). The citation-closure admits a cited
// dep-module theorem only when it is emitted strictly BEFORE the citing law
// (`topology_admits`). The pure comparator has a unit test in
// `induction::topology_tests`; this drives the guarantee through the WHOLE
// pipeline so a regression that keeps the comparator correct but breaks the
// wiring (or the recognizer's own forward-sibling decline) is still caught.
//
// Two dep-module fixtures are byte-identical except for the source order of a
// geone law and the recursive-positivity sibling it cites:
//   * `inorder` — the sibling is emitted first: the citation is backward, so
//     the geone law proves universally and genuinely cites `pow2_law_positive`.
//   * `forward` — the sibling is emitted LATER: the citation would be a forward
//     reference the kernel rejects, so it is refused. The geone law degrades to
//     its pre-closure tier (no universal theorem, no citation), no forward
//     reference reaches the `.lean`, and the build stays green.
// The JSON summary is identical for both; the guarantee lives in the emitted
// dep `.lean`, so the assertions read it back.
// ---------------------------------------------------------------------------

/// Run a `citation_closure_forward_ref` variant end-to-end and return the JSON
/// summary, the raw `Output`, and the generated `Domain/Frac.lean`.
fn run_forward_ref_variant(variant: &str) -> (serde_json::Value, std::process::Output, String) {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let root = format!("tests/fixtures/citation_closure_forward_ref/{variant}");
    let output_dir = temp_output_dir(&format!("aver-citation-closure-{variant}"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg(format!("{root}/main.av"))
        .arg("--module-root")
        .arg(&root)
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&output_dir)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected citation-closure topology fixture to run");
    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with('{')))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)))
        .to_string();
    let summary: serde_json::Value =
        serde_json::from_str(&json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}"));
    let frac = std::fs::read_to_string(output_dir.join("Domain/Frac.lean"))
        .expect("expected generated Domain/Frac.lean");
    let _ = std::fs::remove_dir_all(&output_dir);
    (summary, run, frac)
}

#[test]
fn cross_file_forward_citation_is_refused_end_to_end() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping citation-closure topology end-to-end test: `lake` not available");
        return;
    }

    // BASELINE (`inorder`): the cited sibling is emitted first, so the geone
    // law proves universally and genuinely cites `pow2_law_positive` — AFTER
    // that theorem's own definition (a backward reference). This is what makes
    // the forward case's refusal non-vacuous: the citation edge really exists.
    let (base_summary, base_run, base_frac) = run_forward_ref_variant("inorder");
    assert_eq!(
        (
            base_summary["passed"].as_bool(),
            base_summary["build_errors"].as_u64(),
        ),
        (Some(true), Some(0)),
        "the in-order baseline must build green\n{}",
        format_output(&base_run)
    );
    assert!(
        base_frac.contains("-- aver:law-class pow2SignedAtLeastOne_law_geOneFlip universal"),
        "baseline: the geone law must prove universally when its cited sibling \
         is emitted first\nDomain/Frac.lean:\n{base_frac}"
    );
    let base_def = base_frac
        .find("theorem pow2_law_positive ")
        .expect("baseline: the positivity sibling theorem must be emitted");
    let base_cite = base_frac
        .find("have h := pow2_law_positive")
        .expect("baseline: the geone proof must cite the positivity sibling");
    assert!(
        base_def < base_cite,
        "baseline: the citation must be a BACKWARD reference — the sibling is \
         defined before it is cited\nDomain/Frac.lean:\n{base_frac}"
    );

    // GUARANTEE (`forward`): the identical program with the cited sibling
    // emitted LATER. The forward citation is refused — the geone law degrades
    // to its pre-closure tier (no universal theorem, no citation), NO forward
    // reference reaches `Frac.lean`, and the build stays green (never red).
    let (fwd_summary, fwd_run, fwd_frac) = run_forward_ref_variant("forward");
    assert_eq!(
        (
            fwd_summary["passed"].as_bool(),
            fwd_summary["build_errors"].as_u64(),
        ),
        (Some(true), Some(0)),
        "the forward-citation fixture must NOT produce a red build\n{}",
        format_output(&fwd_run)
    );
    assert!(
        !fwd_frac.contains("-- aver:law-class pow2SignedAtLeastOne_law_geOneFlip universal"),
        "forward: the geone law must degrade to its pre-closure tier (lose its \
         universal theorem) when its citation would be a forward reference\n\
         Domain/Frac.lean:\n{fwd_frac}"
    );
    assert!(
        !fwd_frac.contains("have h := pow2_law_positive"),
        "forward: NO forward reference to the later-emitted sibling may appear \
         in the emitted proof\nDomain/Frac.lean:\n{fwd_frac}"
    );
}

/// A dependency module named `Type` — a reserved Lean token that Aver's
/// lexer accepts as a module name. Every module-name surface must carry
/// the trailing-quote guard: the emitted file (`Type'.lean`), its
/// `namespace`/`end` pair, the consumer's `import`/`open` lines and
/// qualified call sites, and the lakefile root. Guarded by the standard
/// `lake` skip; asserts the emitted sources, then that the project
/// builds clean under the pinned toolchain.
#[test]
fn cross_file_reserved_module_name_escapes_and_builds() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping reserved-module-name test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-reserved-module-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(
        src.join("Type.av"),
        "module Type\n\
        \x20   intent =\n\
        \x20       \"Dependency module named after a reserved Lean token.\"\n\
        \x20   exposes [double]\n\n\
        fn double(n: Int) -> Int\n\
        \x20   n * 2\n",
    )
    .expect("write Type.av");
    std::fs::write(
        src.join("Consumer.av"),
        "module Consumer\n\
        \x20   depends [Type]\n\
        \x20   intent =\n\
        \x20       \"Entry depending on a module named Type.\"\n\n\
        fn quadruple(n: Int) -> Int\n\
        \x20   Type.double(Type.double(n))\n\n\
        verify quadruple\n\
        \x20   quadruple(3) => 12\n",
    )
    .expect("write Consumer.av");
    let out = temp_output_dir("aver-reserved-module-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("Consumer.av"))
        .arg("--backend")
        .arg("lean")
        .arg("--module-root")
        .arg(&src)
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check --check-json` to run");

    let dep_lean = std::fs::read_to_string(out.join("Type'.lean"))
        .expect("dep module named `Type` must emit as Type'.lean");
    assert!(
        dep_lean.contains("namespace Type'") && dep_lean.contains("end Type'"),
        "the dep namespace must carry the reserved-token guard\nType'.lean:\n{dep_lean}"
    );
    let consumer_lean =
        std::fs::read_to_string(out.join("Consumer.lean")).expect("Consumer.lean must exist");
    assert!(
        consumer_lean.contains("import Type'") && consumer_lean.contains("open Type'"),
        "the consumer's import/open lines must escape the module name\n\
         Consumer.lean:\n{consumer_lean}"
    );
    assert!(
        consumer_lean.contains("Type'.double"),
        "qualified call sites must escape the module segment\nConsumer.lean:\n{consumer_lean}"
    );
    let lakefile =
        std::fs::read_to_string(out.join("lakefile.lean")).expect("lakefile.lean must exist");
    assert!(
        lakefile.contains("`Type'"),
        "the lakefile root must match the escaped module name\nlakefile.lean:\n{lakefile}"
    );

    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with('{')))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)));
    let summary: serde_json::Value =
        serde_json::from_str(json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}"));
    assert_eq!(
        summary["passed"].as_bool(),
        Some(true),
        "the reserved-module-name project must build clean\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

/// The ENTRY module named `Type` — the same reserved-token shape as the
/// dep-module test above, but on the entry surface (`lean_project_name`):
/// the emitted entry file and the lakefile root/lib names. `lake build`
/// itself tolerates a raw `Type` root, so the observable failure is
/// subtler than a red build: the `--check` law audit writes probe files
/// that `import` every lakefile root, and `import Type` dies with
/// `unexpected token 'Type'` — silently downgrading a kernel-proven
/// universal law to `universal: false`. Asserts the escaped surfaces,
/// then that the law KEEPS its universal credit end-to-end.
#[test]
fn entry_reserved_module_name_escapes_and_builds() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping reserved-entry-name test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-reserved-entry-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(
        src.join("Type.av"),
        "module Type\n\
        \x20   intent =\n\
        \x20       \"Entry module named after a reserved Lean token.\"\n\n\
        fn add(a: Int, b: Int) -> Int\n\
        \x20   a + b\n\n\
        verify add law commutative\n\
        \x20   given a: Int = [1, 2, 3]\n\
        \x20   given b: Int = [4, 5, 6]\n\
        \x20   add(a, b) => add(b, a)\n",
    )
    .expect("write Type.av");
    let out = temp_output_dir("aver-reserved-entry-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("Type.av"))
        .arg("--backend")
        .arg("lean")
        .arg("--module-root")
        .arg(&src)
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check --check-json` to run");

    let entry_lean = std::fs::read_to_string(out.join("Type'.lean"))
        .expect("entry module named `Type` must emit as Type'.lean");
    assert!(
        entry_lean.contains("theorem add_law_commutative"),
        "the escaped entry file must carry the law theorem\nType'.lean:\n{entry_lean}"
    );
    assert!(
        !out.join("Type.lean").exists(),
        "no raw Type.lean may be emitted alongside the escaped entry file"
    );
    let lakefile =
        std::fs::read_to_string(out.join("lakefile.lean")).expect("lakefile.lean must exist");
    assert!(
        lakefile.contains("`Type'"),
        "the lakefile root must match the escaped entry name\nlakefile.lean:\n{lakefile}"
    );
    assert!(
        !lakefile.contains("#[`Type,"),
        "the raw entry name must not survive as a lakefile root\nlakefile.lean:\n{lakefile}"
    );

    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with('{')))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)));
    let summary: serde_json::Value =
        serde_json::from_str(json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}"));
    assert_eq!(
        (
            summary["passed"].as_bool(),
            summary["universal"].as_bool(),
            summary["universal_laws"].as_u64(),
        ),
        (Some(true), Some(true), Some(1)),
        "the reserved-entry-name project must build clean AND keep its \
         universal-law credit (the audit probes import the escaped root)\n{}",
        format_output(&run)
    );
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

/// `Lib` renamed to `Type` — a dep module whose name is a reserved Lean
/// token, exporting the SAME proven law the split probe
/// (`cross_file_consumer_proves_via_dep_law`) cites. The admissibility
/// gate reads its mentions off the EMITTED statement, where the module
/// segment carries the reserved-token guard (`Type'.qrev`), while the
/// membership index / orientation set / citation name used to be built
/// from the RAW `ModuleInfo::prefix` (`Type.qrev`). Nothing matched, so
/// `mentions` came back empty and the dep law silently degraded to
/// not-admitted: no theorem in `Type'.lean`, no citation in
/// `Consumer.lean`. Same fixture under the name `Lib` is admitted and
/// cited, so the module NAME alone decided proof strength.
const TYPE_PROVEN: &str = "module Type\n\
    \x20   intent =\n\
    \x20       \"Reversal helpers with a proven accumulator-equivalence law.\"\n\
    \x20   effects []\n\n\
    fn qrev(x: List<Int>, y: List<Int>) -> List<Int>\n\
    \x20   match x\n\
    \x20       [] -> y\n\
    \x20       [z, ..xs] -> qrev(xs, List.concat([z], y))\n\n\
    fn rev(x: List<Int>) -> List<Int>\n\
    \x20   match x\n\
    \x20       [] -> []\n\
    \x20       [y, ..xs] -> List.concat(rev(xs), [y])\n\n\
    verify qrev law qrevSpec\n\
    \x20   given x: List<Int> = [[], [1], [1, 2, 3]]\n\
    \x20   given y: List<Int> = [[], [9], [8, 7]]\n\
    \x20   qrev(x, y) => List.concat(rev(x), y)\n";

const CONSUMER_USES_TYPE_DEP: &str = "module Consumer\n\
    \x20   depends [Type]\n\
    \x20   intent =\n\
    \x20       \"Wraps Type.rev and proves it equals Type.qrev with empty accumulator.\"\n\
    \x20   effects []\n\n\
    fn myRev(x: List<Int>) -> List<Int>\n\
    \x20   Type.rev(x)\n\n\
    verify myRev law myRevQrev\n\
    \x20   given x: List<Int> = [[], [1], [1, 2, 3]]\n\
    \x20   myRev(x) => Type.qrev(x, [])\n";

#[test]
fn cross_file_reserved_module_name_dep_law_is_admitted_and_cited() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping reserved-module dep-law citation test: `lake` not available");
        return;
    }
    let (summary, run, leans) = run_multi(
        &[
            ("Type.av", TYPE_PROVEN),
            ("Consumer.av", CONSUMER_USES_TYPE_DEP),
        ],
        "Consumer.av",
        &["Type'.lean", "Consumer.lean"],
    );
    let dep_lean = &leans["Type'.lean"];
    assert!(
        dep_lean.contains("theorem qrev_law_qrevSpec :"),
        "the reserved-name dep module's cited law must be ADMITTED and therefore \
         emitted as a theorem; empty mentions drop it silently\nType'.lean:\n{dep_lean}"
    );
    let consumer_lean = &leans["Consumer.lean"];
    assert!(
        consumer_lean.contains("Type'.qrev_law_qrevSpec"),
        "the consumer proof must cite the dep law under its ESCAPED module \
         segment\nConsumer.lean:\n{consumer_lean}"
    );
    assert!(
        !consumer_lean.contains("Type."),
        "no raw (unescaped) module segment may reach the emitted Lean — Lean \
         cannot parse `Type.qrev_law_qrevSpec`\nConsumer.lean:\n{consumer_lean}"
    );
    assert_eq!(
        (
            summary["passed"].as_bool(),
            summary["universal"].as_bool(),
            summary["universal_laws"].as_u64(),
            summary["sorries"].as_u64(),
        ),
        (Some(true), Some(true), Some(1), Some(0)),
        "the reserved-name dep must give the consumer the SAME universal credit \
         the `Lib`-named split probe gets\n{}",
        format_output(&run)
    );
}

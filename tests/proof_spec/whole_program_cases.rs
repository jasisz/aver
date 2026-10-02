//! Verify-case shapes from btc-listener's whole-program export (`main.av`),
//! each of which used to fail `lake build` for the entire entry.
//!
//! - A case comparing a `Result` of a big tuple needs a larger instance
//!   budget to find the `DecidableEq` it is decided through.
//! - An Int countdown that stops at a floor other than zero needs fuel
//!   measured from that floor; `natAbs(to) + 1` ran out on a true case.
//! - Closed byte-key computations eliminate the symbolic oracle without a
//!   sorry. A case the VM failed is a counterexample and still fails the build.

use super::*;

fn summary_from(output: &std::process::Output) -> serde_json::Value {
    let line = output
        .stdout
        .split(|byte| *byte == b'\n')
        .rev()
        .find_map(|line| {
            std::str::from_utf8(line)
                .ok()
                .filter(|text| text.starts_with('{'))
        })
        .unwrap_or_else(|| panic!("no JSON summary:\n{}", format_output(output)));
    serde_json::from_str(line).unwrap_or_else(|error| {
        panic!(
            "invalid JSON summary ({error}): {line}\n{}",
            format_output(output)
        )
    })
}

const SHAPES: &str = r#"module Shapes
    intent = "Case shapes from a whole program: a large result type, a countdown to a floor, an oracle behind a byte key."
    exposes [Coin, Undo, Standing, Tally, absorbed, heightsFrom, belowMinusThree, lookup]
    depends [Bytes]
    effects [Disk.readText]

record Coin
    key: Bytes
    value: Int

record Undo
    key: Bytes
    value: Bytes

record Standing
    height: Int
    blockId: String

record Tally
    added: Int
    removed: Int

fn absorbed(created: Map<Bytes, Coin>, spent: Map<Bytes, Bool>, undos: List<Undo>, held: Int, refused: Bool) -> Result<Tuple<Map<Bytes, Coin>, Map<Bytes, Bool>, List<Undo>, Int, Option<Standing>, Tally>, String>
    ? "The window after one block, or why the block does not connect."
    match refused
        true -> Result.Err("cannot connect")
        false -> Result.Ok((created, spent, undos, held + 1, Option.None, Tally(added = 0, removed = 0)))

verify absorbed
    absorbed({}, {}, [], 0, true) => Result.Err("cannot connect")

fn heightsFrom(from: Int, to: Int, acc: List<Int>) -> List<Int>
    ? "The heights in a range, lowest first."
    match to < from
        true -> acc
        false -> heightsFrom(from, to - 1, List.prepend(to, acc))

verify heightsFrom
    heightsFrom(1, 3, []) => [1, 2, 3]
    heightsFrom(0, 0, []) => [0]
    heightsFrom(-2, 0, []) => [-2, -1, 0]

fn belowMinusThree(n: Int, acc: List<Int>) -> List<Int>
    ? "The numbers from n down to -3."
    match n < -3
        true -> acc
        false -> belowMinusThree(n - 1, List.prepend(n, acc))

verify belowMinusThree
    belowMinusThree(-2, []) => [-3, -2]

fn lookup(name: String) -> Result<String, String>
    ? "Reads a long name from disk; a short one needs no read."
    ! [Disk.readText]
    match Bytes.len(String.toUtf8(name)) > 3
        true -> Disk.readText(name)
        false -> Result.Ok("short")

verify lookup
    lookup("ab") => Result.Ok("short")
"#;

#[test]
fn whole_program_case_shapes_build_without_unfinished_cases() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping whole-program case shapes test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let source_dir = temp_output_dir("aver-case-shapes-src");
    std::fs::create_dir_all(&source_dir).expect("create source dir");
    let file = source_dir.join("shapes.av");
    std::fs::write(&file, SHAPES).expect("write source");
    let output_dir = temp_output_dir("aver-case-shapes-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg("--examples")
        .arg(&file)
        .arg("--module-root")
        .arg(&source_dir)
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&output_dir)
        .arg("--check")
        .arg("--check-json")
        .arg("--sorry-budget")
        .arg("0")
        .output()
        .expect("run proof export");
    let summary = summary_from(&run);
    let lean = std::fs::read_to_string(output_dir.join("Shapes.lean"))
        .unwrap_or_else(|error| panic!("read Shapes.lean ({error}):\n{}", format_output(&run)));
    let _ = std::fs::remove_dir_all(&source_dir);
    let _ = std::fs::remove_dir_all(&output_dir);

    // The tuple, countdowns and UTF-8 branch all prove without sorries.
    assert_eq!(
        (
            summary["passed"].as_bool(),
            summary["sorries"].as_u64(),
            summary["build_errors"].as_u64(),
            summary["model_panicked"].as_bool(),
        ),
        (Some(true), Some(0), Some(0), Some(false)),
        "{summary}\n{lean}"
    );
    assert!(
        summary.get("isolated_errors").is_none(),
        "{summary}\n{lean}"
    );
    assert!(summary.get("isolated_cases").is_none(), "{summary}\n{lean}");
    assert!(summary.get("sorry_laws").is_none(), "{summary}\n{lean}");
    assert!(
        lean.contains("set_option synthInstance.maxSize 4096 in\nexample : absorbed "),
        "the big-tuple case gets the instance budget:\n{lean}"
    );
    assert!(
        lean.contains(
            "heightsFrom__fuel (max ((Int.natAbs to) + 1) ((Int.natAbs (to - from')) + 2)) from' to acc"
        ),
        "the countdown's fuel is measured from its floor, never below the zero floor:\n{lean}"
    );
    assert!(
        lean.contains(
            "belowMinusThree__fuel (max ((Int.natAbs n) + 1) ((Int.natAbs (n - (-3 : Int))) + 2)) n acc"
        ),
        "a negative literal floor is read too:\n{lean}"
    );
    assert!(
        lean.contains(&format!(
            "{}\ntheorem __aver_verify_lookup_1 (rnd_Disk_readText",
            aver::codegen::lean::isolate::ISOLATION_GUARD
        )),
        "the symbolic-oracle case sits behind the isolation guard:\n{lean}"
    );
}

/// A case the VM fails is a counterexample, whatever the sorry budget.
///
/// `lookup("ab")` is `Ok("short")`, so the case below is false. It used to be
/// isolated like a case `simp` could not finish, and `--sorry-budget 1` passed
/// with the failure only in `isolated_errors`. The source also declares a
/// function named like the case's old theorem, `lookup_verify_1`: the theorem
/// then failed with "already declared", the guard dropped that error, the
/// check found the function under the name, and the false case passed at
/// budget 0 with no trace at all.
const FALSE_ORACLE_CASE: &str = r#"module FalseCase
    intent = "An oracle case the program does not meet, next to a function named like its theorem."
    exposes [lookup, lookup_verify_1]
    depends [Bytes]
    effects [Disk.readText]

fn lookup(name: String) -> Result<String, String>
    ? "Reads a long name from disk; a short one needs no read."
    ! [Disk.readText]
    match Bytes.len(String.toUtf8(name)) > 3
        true -> Disk.readText(name)
        false -> Result.Ok("short")

verify lookup
    lookup("ab") => Result.Ok("long")

fn lookup_verify_1() -> Int
    ? "A function whose name an exported theorem used to take."
    1
"#;

#[test]
fn a_false_oracle_case_fails_the_check_at_any_sorry_budget() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping false oracle case test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let source_dir = temp_output_dir("aver-false-case-src");
    std::fs::create_dir_all(&source_dir).expect("create source dir");
    let file = source_dir.join("false_case.av");
    std::fs::write(&file, FALSE_ORACLE_CASE).expect("write source");
    let output_dir = temp_output_dir("aver-false-case-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg("--examples")
        .arg(&file)
        .arg("--module-root")
        .arg(&source_dir)
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&output_dir)
        .arg("--check")
        .arg("--check-json")
        .arg("--sorry-budget")
        .arg("1000")
        .output()
        .expect("run proof export");
    let summary = summary_from(&run);
    let lean = std::fs::read_to_string(output_dir.join("FalseCase.lean"))
        .unwrap_or_else(|error| panic!("read FalseCase.lean ({error}):\n{}", format_output(&run)));
    let _ = std::fs::remove_dir_all(&source_dir);
    let _ = std::fs::remove_dir_all(&output_dir);

    assert_eq!(
        summary["passed"].as_bool(),
        Some(false),
        "a false case must fail the check:\n{summary}\n{lean}"
    );
    assert!(
        !lean.contains(aver::codegen::lean::isolate::ISOLATION_GUARD),
        "a case the VM failed must not sit behind the isolation guard:\n{lean}"
    );
    assert!(
        lean.contains("example (rnd_Disk_readText"),
        "a case the VM failed stays a plain example:\n{lean}"
    );
}

/// The members of a mutual group share one fuel, and the floor of one
/// member's own self-calls does not bound the calls to the others: `down`
/// recurses under `n >= lo`, `side` under `n >= 1`, and `down(10, 10)` makes
/// eleven calls in all. Seeded from `down`'s floor it had fuel for two, and
/// the model panicked on a true case.
const MUTUAL_FLOOR: &str = r#"module MutualFloor
    intent = "A mutual countdown whose members stop at different floors."
    exposes [down, side]

fn down(n: Int, lo: Int) -> Int
    ? "Steps down to lo, then hands over to side."
    match n < lo
        true -> side(n - 1, lo)
        false -> down(n - 1, lo)

fn side(n: Int, lo: Int) -> Int
    ? "Steps down to zero through down."
    match n < 1
        true -> 0
        false -> down(n - 1, lo)

verify down
    down(10, 10) => 0
"#;

#[test]
fn a_mutual_countdown_keeps_the_zero_floor_fuel() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping mutual floor test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let source_dir = temp_output_dir("aver-mutual-floor-src");
    std::fs::create_dir_all(&source_dir).expect("create source dir");
    let file = source_dir.join("mutual_floor.av");
    std::fs::write(&file, MUTUAL_FLOOR).expect("write source");
    let output_dir = temp_output_dir("aver-mutual-floor-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg("--examples")
        .arg(&file)
        .arg("--module-root")
        .arg(&source_dir)
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&output_dir)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("run proof export");
    let summary = summary_from(&run);
    let lean =
        std::fs::read_to_string(output_dir.join("MutualFloor.lean")).unwrap_or_else(|error| {
            panic!("read MutualFloor.lean ({error}):\n{}", format_output(&run))
        });
    let _ = std::fs::remove_dir_all(&source_dir);
    let _ = std::fs::remove_dir_all(&output_dir);

    assert_eq!(
        (
            summary["passed"].as_bool(),
            summary["model_panicked"].as_bool()
        ),
        (Some(true), Some(false)),
        "{summary}\n{lean}"
    );
    assert!(
        lean.contains("down__fuel ((Int.natAbs n) + 1) n lo"),
        "a mutual group member keeps the zero-floor seed:\n{lean}"
    );
}

// Pure pick keeps the exported model computable. The unused operation givens
// still separate stub worlds, so A/B/A exercises merging without an unrelated
// noncomputable effect model obscuring whether Lean decides the false equation.
const INTERLEAVED_CASES: &str = r#"module Store
    exposes [pick]
    effects [Random.int]

fn low(path: BranchPath, call: Int, min: Int, max: Int) -> Result<Int, String>
    Result.Ok(min)

fn high(path: BranchPath, call: Int, min: Int, max: Int) -> Result<Int, String>
    Result.Ok(max)

fn pick(go: Bool) -> Int
    1

verify pick
    given rnd: Random.int = [low]
    pick(false) => 1

verify pick
    given rnd: Random.int = [high]
    pick(false) => 2

verify pick
    given rnd: Random.int = [low]
    pick(false) => 1
"#;

fn check_interleaved_case_identity(dependency: bool) {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping interleaved case identity test: `lake` not available");
        return;
    }
    let source_dir = tempfile::tempdir().expect("source directory");
    let file = source_dir.path().join("main.av");
    let (subject, lean_path, pick) = if dependency {
        std::fs::create_dir(source_dir.path().join("infra")).unwrap();
        std::fs::write(&file, "module Main\n    depends [Infra.Store]\n\nfn main() -> Int\n    Infra.Store.pick(false)\n").unwrap();
        (
            source_dir.path().join("infra/store.av"),
            "Infra/Store.lean",
            "Infra.Store.pick",
        )
    } else {
        (file.clone(), "Store.lean", "pick")
    };
    // The all-true control must build, proving the negative check fails on the
    // equation rather than a noncomputable model or another export failure.
    for false_middle in [true, false] {
        let source = if false_middle {
            INTERLEAVED_CASES.to_string()
        } else {
            INTERLEAVED_CASES.replace("=> 2", "=> 1")
        };
        std::fs::write(&subject, source).unwrap();
        let output_dir = tempfile::tempdir().expect("output directory");
        let run = Command::new(env!("CARGO_BIN_EXE_aver"))
            .arg("proof")
            .arg("--examples")
            .arg(&file)
            .arg("--module-root")
            .arg(source_dir.path())
            .arg("-o")
            .arg(output_dir.path())
            .args(["--check", "--check-json", "--sorry-budget", "1000"])
            .output()
            .expect("run proof export");
        let summary = summary_from(&run);
        let lean = std::fs::read_to_string(output_dir.path().join(lean_path)).unwrap();
        let equations: Vec<_> = lean
            .lines()
            .filter(|line| line.starts_with(&format!("example : {pick} false =")))
            .collect();
        assert_eq!(equations.len(), 3, "{lean}");
        let expected = if false_middle { [1, 2, 1] } else { [1, 1, 1] };
        for (equation, value) in equations.iter().zip(expected) {
            assert!(
                equation.contains(&format!("= ({value} : Int) := by")),
                "{lean}"
            );
        }
        assert!(
            !lean.contains(aver::codegen::lean::isolate::ISOLATION_GUARD),
            "{lean}"
        );
        assert_eq!(
            summary["passed"],
            !false_middle,
            "{summary}\n{lean}\n{}",
            format_output(&run)
        );
        assert_eq!(summary["model_panicked"], false, "{summary}");
        assert_eq!(summary["sorries"], 0, "{summary}");
        if false_middle {
            assert!(summary["build_errors"].as_u64().unwrap() > 0, "{summary}");
        } else {
            assert_eq!(summary["build_errors"], 0, "{summary}");
        }
    }
}

#[test]
fn interleaved_case_identity_in_path_named_dependency() {
    check_interleaved_case_identity(true);
}

#[test]
fn interleaved_case_identity_in_entry_module() {
    check_interleaved_case_identity(false);
}

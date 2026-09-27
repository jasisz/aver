//! Verify-case shapes from btc-listener's whole-program export (`main.av`),
//! each of which used to fail `lake build` for the entire entry.
//!
//! - A case comparing a `Result` of a big tuple needs a larger instance
//!   budget to find the `DecidableEq` it is decided through.
//! - An Int countdown that stops at a floor other than zero needs fuel
//!   measured from that floor; `natAbs(to) + 1` ran out on a true case.
//! - A case with a symbolic oracle whose branch `simp` cannot evaluate is
//!   isolated and charged, not a build error, when the VM passed it. A case
//!   the VM failed is a counterexample and still fails the build.

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
fn whole_program_case_shapes_build_and_charge_the_unfinished_case() {
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
        .arg("1")
        .output()
        .expect("run proof export");
    let summary = summary_from(&run);
    let lean = std::fs::read_to_string(output_dir.join("Shapes.lean"))
        .unwrap_or_else(|error| panic!("read Shapes.lean ({error}):\n{}", format_output(&run)));
    let _ = std::fs::remove_dir_all(&source_dir);
    let _ = std::fs::remove_dir_all(&output_dir);

    // The build succeeds: the big-tuple case and the three countdown cases
    // are proved, and the one case simp cannot finish is charged as one
    // isolated sorry instead of failing the module.
    assert_eq!(
        (
            summary["passed"].as_bool(),
            summary["sorries"].as_u64(),
            summary["build_errors"].as_u64(),
            summary["model_panicked"].as_bool(),
        ),
        (Some(true), Some(1), Some(1), Some(false)),
        "{summary}\n{lean}"
    );
    assert_eq!(
        summary["isolated_errors"],
        serde_json::json!(["Shapes.__aver_verify_lookup_1"]),
        "{summary}\n{lean}"
    );
    // The case is reported as a case, not as a law.
    assert_eq!(
        summary["isolated_cases"],
        serde_json::json!(["Shapes.__aver_verify_lookup_1"]),
        "{summary}\n{lean}"
    );
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

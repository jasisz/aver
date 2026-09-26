//! Verify-case shapes from btc-listener's whole-program export (`main.av`),
//! each of which used to fail `lake build` for the entire entry.
//!
//! - A case comparing a `Result` of a big tuple needs a larger instance
//!   budget to find the `DecidableEq` it is decided through.
//! - An Int countdown that stops at a floor other than zero needs fuel
//!   measured from that floor; `natAbs(to) + 1` ran out on a true case.
//! - A case with a symbolic oracle whose branch `simp` cannot evaluate is
//!   isolated and charged, not a build error.

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
    exposes [Coin, Undo, Standing, Tally, absorbed, heightsFrom, lookup]
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
        serde_json::json!(["Shapes.lookup_verify_1"]),
        "{summary}\n{lean}"
    );
    assert!(
        lean.contains("set_option synthInstance.maxSize 4096 in\nexample : absorbed "),
        "the big-tuple case gets the instance budget:\n{lean}"
    );
    assert!(
        lean.contains("heightsFrom__fuel ((Int.natAbs (to - from')) + 2) from' to acc"),
        "the countdown's fuel is measured from its floor:\n{lean}"
    );
    assert!(
        lean.contains(&format!(
            "{}\ntheorem lookup_verify_1 (rnd_Disk_readText",
            aver::codegen::lean::isolate::ISOLATION_GUARD
        )),
        "the symbolic-oracle case sits behind the isolation guard:\n{lean}"
    );
}

//! Regression: arena evacuation/promotion used to recurse down
//! `ArenaList::Prepend` tail spines. A mutually-recursive grid walk
//! (Conway's Life `step` shape) that hoists a per-iteration `let` allocation
//! before a nested `match` tips `finalize_frame_locals_for_tail_call`'s
//! `young_growth > 4` promotion mid-loop; once the carried accumulator reached
//! a few thousand cons cells, the recursive tail scan in
//! `aver-memory/src/memory.rs` overflowed the Rust stack. This program (the
//! reduced life trio, `total = 3444`) crashed before the iterative cons-tail
//! rewrite and runs cleanly after it.
//!
//! NOTE: a *self*-recursive accumulator does NOT reproduce — the deep recursive
//! evacuation only fires through the 3-way mutual cycle with the hoisted let,
//! so the fixture must keep that exact shape.

use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

#[test]
fn vm_evacuates_deep_cons_accumulator_without_stack_overflow() {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time before unix epoch")
        .as_nanos();
    let dir = std::env::temp_dir().join(format!("aver-deep-cons-evac-{nanos}"));
    std::fs::create_dir_all(&dir).expect("create temp dir");
    let src = dir.join("main.av");
    std::fs::write(
        &src,
        r#"module DeepConsEvacuate
    intent = "Reduced Conway Life step trio: mutual recursion + hoisted let, deep cons accumulator."
    exposes [run]

fn evolve(alive: Int, n: Int) -> Int
    match alive
        1 -> match n
            2 -> 1
            3 -> 1
            _ -> 0
        _ -> match n
            3 -> 1
            _ -> 0

fn stepLoop(old: Vector<Int>, s: Int, w: Int, h: Int, total: Int, idx: Int, acc: List<Int>) -> List<Int>
    match idx == total
        true -> acc
        false -> stepOne(old, s, w, h, total, idx, acc)

fn stepOne(old: Vector<Int>, s: Int, w: Int, h: Int, total: Int, idx: Int, acc: List<Int>) -> List<Int>
    row = Result.withDefault(Int.div(idx, s), 0)
    col = Result.withDefault(Int.mod(idx, s), 0)
    nextIdx = idx + 1
    borderAcc = List.prepend(0, acc)
    match row == 0
        true -> stepLoop(old, s, w, h, total, nextIdx, borderAcc)
        false -> match row > h
            true -> stepLoop(old, s, w, h, total, nextIdx, borderAcc)
            false -> match col == 0
                true -> stepLoop(old, s, w, h, total, nextIdx, borderAcc)
                false -> match col > w
                    true -> stepLoop(old, s, w, h, total, nextIdx, borderAcc)
                    false -> stepInterior(old, s, w, h, total, idx, acc)

fn stepInterior(old: Vector<Int>, s: Int, w: Int, h: Int, total: Int, idx: Int, acc: List<Int>) -> List<Int>
    alive = Option.withDefault(Vector.get(old, idx), 0)
    nextIdx = idx + 1
    n = Option.withDefault(Vector.get(old, idx - 1), 0) + Option.withDefault(Vector.get(old, nextIdx), 0)
    next = evolve(alive, n)
    stepLoop(old, s, w, h, total, nextIdx, List.prepend(next, acc))

fn run() -> Int
    ? "One step over an 80x40 padded grid (total=3444) — deep enough to overflow the old recursive evacuation."
    List.len(stepLoop(Vector.new(3444, 0), 82, 80, 40, 3444, 0, []))

fn main() -> Unit
    ! [Console.print]
    Console.print("len={String.fromInt(run())}")
"#,
    )
    .expect("write Aver source");

    let output = Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(PathBuf::from(env!("CARGO_MANIFEST_DIR")))
        .arg("run")
        .arg(&src)
        .output()
        .expect("aver run executes");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let _ = std::fs::remove_dir_all(&dir);

    assert!(
        output.status.success(),
        "deep cons accumulator VM run overflowed (recursive arena evacuation regression)\n\
         stdout:\n{stdout}\nstderr:\n{stderr}"
    );
    assert_eq!(stdout.trim(), "len=3444");
}

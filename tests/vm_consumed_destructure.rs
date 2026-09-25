//! A tuple or `Option`/`Result` box that a match (or `?`) takes apart where
//! nothing reads it afterwards gives up what it holds.
//!
//! A record carried in `Option.Some(state)` or in `(state, reply)` used to stay
//! held by that box or tuple after the match had bound it: the arena counted
//! the box as a holder of `state`, and nothing ever took the count back, so
//! every later `Map.set` on a field of `state` copied the whole Map. The
//! generated loop hands an answer module's state out of the run exactly this
//! way, so every answer copied the module's Maps on the VM.
//!
//! Tuples and boxes now count their holders the way maps, vectors and records
//! do, and a match whose subject is a temporary or a local at its last use
//! releases what the tuple or box holds when nothing else holds the tuple or
//! box itself.
//!
//! Two halves:
//!
//! - the WINS: the per-step update copies nothing, measured as map entries
//!   copied;
//! - the REFUSALS: every way the tuple or box can still be reached keeps it
//!   holding its value, and the program's answer is the one immutable values
//!   give. A wrong release would show up as the other holder reading back a
//!   changed Map.

use std::process::Command;

use aver::ir::pipeline::{PipelineConfig, TypecheckMode};
use aver::nan_value::Arena;
use aver::vm::{self, VM};

fn compiled_vm(src: &str) -> VM {
    let mut items = aver::source::parse_source(src).expect("parse failed");
    let result = aver::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full { base_dir: None }),
            ..Default::default()
        },
    );
    let tc = result.typecheck.as_ref().expect("typecheck requested");
    assert!(tc.errors.is_empty(), "typecheck failed: {:?}", tc.errors);

    let mut arena = Arena::new();
    let (code, globals) = vm::compile_program(
        &result.resolved_items,
        &result.symbol_table,
        &mut arena,
        None,
    )
    .expect("compile failed");
    VM::new(code, globals, arena)
}

/// What the program answered and how many map entries it copied.
fn run(src: &str) -> (i64, u64) {
    let mut machine = compiled_vm(src);
    let result = machine.run().expect("program should run");
    (
        result.as_int(&machine.arena),
        machine.arena.map_entries_copied(),
    )
}

const PRELUDE: &str = r#"module Consumed
    intent = "destructured tuples and boxes"
    effects []

record State
    counts: Map<Int, Int>

record Holder
    held: Option<State>

fn fill(counts: Map<Int, Int>, left: Int) -> Map<Int, Int>
    match left <= 0
        true -> counts
        false -> fill(Map.set(counts, left, 1), left - 1)

fn fresh() -> State
    State(counts = fill({}, 200))

fn get(m: Map<Int, Int>, k: Int) -> Int
    Option.withDefault(Map.get(m, k), 0 - 1)

fn countOf(held: Option<State>, k: Int) -> Int
    match held
        Option.Some(s) -> get(s.counts, k)
        Option.None -> 0 - 2

fn step(held: Option<State>, key: Int) -> Option<State>
    match held
        Option.Some(s) -> Option.Some(State(counts = Map.set(s.counts, key, 5)))
        Option.None -> Option.None

fn pair(both: Tuple<State, Int>, key: Int) -> Tuple<State, Int>
    match both
        (s, n) -> (State(counts = Map.set(s.counts, key, 5)), n + 1)

fn checked(s: State, key: Int) -> Result<State, String>
    Result.Ok(State(counts = Map.set(s.counts, key, 5)))
"#;

fn program(body: &str) -> String {
    format!("{PRELUDE}\n{body}")
}

const STEPS: i64 = 50;

#[test]
fn an_option_unwrapped_at_its_last_use_copies_nothing() {
    let src = program(&format!(
        r#"
fn serve(held: Option<State>, left: Int) -> Option<State>
    match left <= 0
        true -> held
        false -> serve(step(held, left), left - 1)

fn main() -> Int
    done = serve(Option.Some(fresh()), {STEPS})
    countOf(done, 7) * 1000 + countOf(done, 199)
"#
    ));
    let (answer, copied) = run(&src);
    assert_eq!(answer, 5 * 1000 + 1);
    assert_eq!(copied, 0, "a step copied the Map inside the Option");
}

#[test]
fn a_tuple_destructured_at_its_last_use_copies_nothing() {
    let src = program(&format!(
        r#"
fn serve(both: Tuple<State, Int>, left: Int) -> Tuple<State, Int>
    match left <= 0
        true -> both
        false -> serve(pair(both, left), left - 1)

fn main() -> Int
    match serve((fresh(), 0), {STEPS})
        (s, n) -> n * 1000 + get(s.counts, 7)
"#
    ));
    let (answer, copied) = run(&src);
    assert_eq!(answer, STEPS * 1000 + 5);
    assert_eq!(copied, 0, "a step copied the Map inside the tuple");
}

#[test]
fn a_result_unwrapped_by_try_copies_nothing() {
    let src = program(&format!(
        r#"
fn serve(s: State, left: Int) -> Result<State, String>
    match left <= 0
        true -> Result.Ok(s)
        false -> serve(checked(s, left)?, left - 1)

fn main() -> Int
    match serve(fresh(), {STEPS})
        Result.Ok(s) -> get(s.counts, 7)
        Result.Err(_) -> 0 - 3
"#
    ));
    let (answer, copied) = run(&src);
    assert_eq!(answer, 5);
    assert_eq!(copied, 0, "a step copied the Map inside the Result");
}

/// The shape the generated loop has: a box inside a tuple, taken apart in two
/// matches.
#[test]
fn a_box_inside_a_tuple_taken_apart_in_turn_copies_nothing() {
    let src = program(&format!(
        r#"
fn stepBoth(both: Tuple<Option<State>, Int>, key: Int) -> Tuple<Option<State>, Int>
    match both
        (held, n) -> match held
            Option.Some(s) -> (Option.Some(State(counts = Map.set(s.counts, key, 5))), n + 1)
            Option.None -> (Option.None, n)

fn serve(both: Tuple<Option<State>, Int>, left: Int) -> Tuple<Option<State>, Int>
    match left <= 0
        true -> both
        false -> serve(stepBoth(both, left), left - 1)

fn main() -> Int
    match serve((Option.Some(fresh()), 0), {STEPS})
        (held, n) -> n * 1000 + countOf(held, 7)
"#
    ));
    let (answer, copied) = run(&src);
    assert_eq!(answer, STEPS * 1000 + 5);
    assert_eq!(
        copied, 0,
        "a step copied the Map inside the box inside the tuple"
    );
}

/// The caller keeps the Option it passed.
#[test]
fn an_option_the_caller_still_holds_keeps_its_value() {
    let src = program(
        r#"
fn main() -> Int
    before = Option.Some(fresh())
    after = step(before, 7)
    countOf(before, 7) * 100 + countOf(after, 7)
"#,
    );
    let (answer, copied) = run(&src);
    assert_eq!(answer, 100 + 5);
    assert!(copied > 0, "the shared Map was written in place");
}

/// A record holds the box.
#[test]
fn an_option_a_record_holds_keeps_its_value() {
    let src = program(
        r#"
fn main() -> Int
    holder = Holder(held = Option.Some(fresh()))
    after = step(holder.held, 7)
    countOf(holder.held, 7) * 100 + countOf(after, 7)
"#,
    );
    let (answer, copied) = run(&src);
    assert_eq!(answer, 100 + 5);
    assert!(copied > 0, "the shared Map was written in place");
}

/// A list holds the box, and the only other reference is the match subject.
#[test]
fn an_option_a_list_holds_keeps_its_value() {
    let src = program(
        r#"
fn firstStepped(xs: List<Option<State>>) -> Option<State>
    match xs
        [] -> Option.None
        [h, .._] -> step(h, 7)

fn main() -> Int
    xs = [Option.Some(fresh())]
    after = firstStepped(xs)
    match xs
        [] -> 0 - 4
        [h, .._] -> countOf(h, 7) * 100 + countOf(after, 7)
"#,
    );
    let (answer, copied) = run(&src);
    assert_eq!(answer, 100 + 5);
    assert!(copied > 0, "the shared Map was written in place");
}

/// One box in two tuples: taking the first apart releases its hold on the
/// box, and the second still holds it.
#[test]
fn a_box_two_tuples_share_keeps_its_value() {
    let src = program(
        r#"
fn stepFirst(both: Tuple<Option<State>, Int>) -> Option<State>
    match both
        (held, _) -> step(held, 7)

fn main() -> Int
    shared = Option.Some(fresh())
    second = (shared, 2)
    first = (shared, 1)
    after = stepFirst(first)
    match second
        (held, n) -> countOf(held, 7) * 100 + countOf(after, 7) * 10 + n
"#,
    );
    let (answer, copied) = run(&src);
    assert_eq!(answer, 100 + 5 * 10 + 2);
    assert!(copied > 0, "the shared Map was written in place");
}

/// A Map holds the box as a value.
#[test]
fn an_option_a_map_holds_keeps_its_value() {
    let src = program(
        r#"
fn main() -> Int
    table = Map.set({}, 1, Option.Some(fresh()))
    after = match Map.get(table, 1)
        Option.Some(held) -> step(held, 7)
        Option.None -> Option.None
    match Map.get(table, 1)
        Option.Some(held) -> countOf(held, 7) * 100 + countOf(after, 7)
        Option.None -> 0 - 4
"#,
    );
    let (answer, copied) = run(&src);
    assert_eq!(answer, 100 + 5);
    assert!(copied > 0, "the shared Map was written in place");
}

/// The caller keeps the tuple it passed.
#[test]
fn a_tuple_the_caller_still_holds_keeps_its_items() {
    let src = program(
        r#"
fn main() -> Int
    before = (fresh(), 0)
    after = pair(before, 7)
    match before
        (s, _) -> match after
            (t, n) -> get(s.counts, 7) * 100 + get(t.counts, 7) * 10 + n
"#,
    );
    let (answer, copied) = run(&src);
    assert_eq!(answer, 100 + 5 * 10 + 1);
    assert!(copied > 0, "the shared Map was written in place");
}

/// A tuple inside another tuple: the outer one still holds the inner one.
#[test]
fn a_tuple_another_tuple_holds_keeps_its_items() {
    let src = program(
        r#"
fn main() -> Int
    inner = (fresh(), 0)
    outer = (inner, 1)
    after = pair(inner, 7)
    match outer
        (kept, _) -> match kept
            (s, _) -> match after
                (t, _) -> get(s.counts, 7) * 10 + get(t.counts, 7)
"#,
    );
    let (answer, copied) = run(&src);
    assert_eq!(answer, 10 + 5);
    assert!(copied > 0, "the shared Map was written in place");
}

/// The Result a `?` unwraps is kept by the caller.
#[test]
fn a_result_the_caller_still_holds_keeps_its_value() {
    let src = program(
        r#"
fn relay(r: Result<State, String>) -> Result<State, String>
    s = r?
    checked(s, 7)

fn main() -> Int
    before: Result<State, String> = Result.Ok(fresh())
    after = relay(before)
    match before
        Result.Ok(s) -> match after
            Result.Ok(t) -> get(s.counts, 7) * 10 + get(t.counts, 7)
            Result.Err(_) -> 0 - 5
        Result.Err(_) -> 0 - 6
"#,
    );
    let (answer, copied) = run(&src);
    assert_eq!(answer, 10 + 5);
    assert!(copied > 0, "the shared Map was written in place");
}

/// A local read again after the match is not consumed by it.
#[test]
fn an_option_read_after_its_match_keeps_its_value() {
    let src = program(
        r#"
fn twice(held: Option<State>) -> Int
    after = match held
        Option.Some(s) -> Option.Some(State(counts = Map.set(s.counts, 7, 5)))
        Option.None -> Option.None
    countOf(held, 7) * 10 + countOf(after, 7)

fn main() -> Int
    twice(Option.Some(fresh()))
"#,
    );
    let (answer, _) = run(&src);
    assert_eq!(answer, 10 + 5);
}

fn repo_root() -> std::path::PathBuf {
    std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

/// The generated loop hands an answer module's state out of the run in an
/// `Option`, inside a tuple, and takes the answer back in another tuple. Every
/// request updates a thousand-entry Map in that state; only the first may copy
/// it (the state the run starts from is still shared with the seating).
#[test]
fn the_generated_loop_updates_an_answer_modules_state_in_place() {
    let dir = repo_root().join("tests/fixtures/run_owned_answer_state");
    let out = Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(repo_root())
        .arg("run")
        .arg(dir.join("main.av"))
        .arg("--module-root")
        .arg(&dir)
        .arg("--profile")
        .output()
        .expect("run aver");
    assert!(
        out.status.success(),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(stdout.contains("total 400"), "{stdout}");
    let report = String::from_utf8_lossy(&out.stderr);
    let copied: u64 = report
        .lines()
        .find_map(|line| {
            line.trim()
                .strip_prefix("map entries copied by the writes that were not in place:")
        })
        .and_then(|n| n.trim().parse().ok())
        .unwrap_or_else(|| panic!("no copied-entries line in:\n{report}"));
    assert!(
        copied < 2 * 1000,
        "the answers copied {copied} map entries; each request copied the state's Map"
    );
}

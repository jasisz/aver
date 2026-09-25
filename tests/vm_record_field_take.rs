//! A Map in a field of a record that a record update or literal consumes.
//!
//! `State.update(state, counts = Map.set(state.counts, k, v))` used to copy the
//! whole Map on every call on the VM, even with `state` uniquely owned: the base
//! of the update sat on the operand stack, often the local still held the record
//! for a later field read, and the record held the Map, so the write always saw
//! another holder. The compiler now takes such a field out of the record
//! (`vm::compiler::field_take`) and tells the runtime which stack cells it knows
//! hold the record, and a record update whose base nothing else holds moves the
//! fields it keeps instead of leaving the dead base holding them too.
//!
//! Two halves:
//!
//! - the WINS: the per-request update copies nothing, measured as map entries
//!   copied;
//! - the REFUSALS: every way the record, or the Map inside it, can still be
//!   reached by someone else keeps the copy, and the program's answer is the one
//!   Aver's immutable values mandate. A wrong take would show up as a caller
//!   reading back an emptied or changed Map.

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

/// Shared prelude: a record with two Maps and a counter, and a filler.
const PRELUDE: &str = r#"module Fields
    intent = "record field updates"
    effects []

record State
    counts: Map<Int, Int>
    other: Map<Int, Int>
    served: Int

record Holder
    inner: State

fn fill(counts: Map<Int, Int>, left: Int) -> Map<Int, Int>
    match left <= 0
        true -> counts
        false -> fill(Map.set(counts, left, 1), left - 1)

fn fresh() -> State
    State(counts = fill({}, 200), other = fill({}, 200), served = 0)

fn get(m: Map<Int, Int>, k: Int) -> Int
    Option.withDefault(Map.get(m, k), 0 - 1)
"#;

fn program(body: &str) -> String {
    format!("{PRELUDE}\n{body}")
}

const REQUESTS: i64 = 50;

/// The shape the port hit: the update reads the record again after the Map
/// field, so the local still holds the record when `Map.set` runs.
#[test]
fn an_update_that_reads_another_field_after_the_map_copies_nothing() {
    let src = program(&format!(
        r#"
fn bump(state: State, key: Int) -> State
    next = get(state.counts, key) + 1
    State.update(state, counts = Map.set(state.counts, key, next), served = state.served + 1)

fn serve(state: State, left: Int) -> State
    match left <= 0
        true -> state
        false -> serve(bump(state, left), left - 1)

fn main() -> Int
    done = serve(fresh(), {REQUESTS})
    done.served * 10000 + get(done.counts, 7) * 100 + Map.len(done.counts)
"#
    ));
    let (answer, copied) = run(&src);
    assert_eq!(answer, REQUESTS * 10000 + 2 * 100 + 200);
    assert_eq!(copied, 0, "every request copied the Map it updated");
}

/// The Map field is the last read of the local; only the update's base holds
/// the record.
#[test]
fn an_update_whose_map_read_is_the_last_use_copies_nothing() {
    let src = program(&format!(
        r#"
fn bump(state: State, key: Int) -> State
    State.update(state, counts = Map.set(state.counts, key, 9))

fn serve(state: State, left: Int) -> State
    match left <= 0
        true -> state
        false -> serve(bump(state, left), left - 1)

fn main() -> Int
    done = serve(fresh(), {REQUESTS})
    get(done.counts, 7) * 1000 + Map.len(done.counts)
"#
    ));
    let (answer, copied) = run(&src);
    assert_eq!(answer, 9 * 1000 + 200);
    assert_eq!(copied, 0, "every request copied the Map it updated");
}

/// Two Maps updated in turn, one by an update and one by a new record: the
/// dead base of each update must not go on holding the Map it did not touch.
#[test]
fn alternating_updates_of_two_maps_copy_nothing() {
    let src = program(&format!(
        r#"
fn bumpCounts(state: State, key: Int) -> State
    State.update(state, counts = Map.set(state.counts, key, 2), served = state.served + 1)

fn bumpOther(state: State, key: Int) -> State
    State(counts = state.counts, other = Map.set(state.other, key, 3), served = state.served + 1)

fn serve(state: State, left: Int) -> State
    match left <= 0
        true -> state
        false -> match Int.mod(left, 2) == 0
            true -> serve(bumpCounts(state, left), left - 1)
            false -> serve(bumpOther(state, left), left - 1)

fn main() -> Int
    done = serve(fresh(), {REQUESTS})
    done.served * 100 + get(done.counts, 2) * 10 + get(done.other, 1)
"#
    ));
    let (answer, copied) = run(&src);
    assert_eq!(answer, REQUESTS * 100 + 2 * 10 + 3);
    assert_eq!(copied, 0, "an update copied a Map the dead base still held");
}

/// The caller keeps the record it passed: the update must copy, and the
/// caller's Map is unchanged.
#[test]
fn a_record_the_caller_still_holds_is_not_taken() {
    let src = program(
        r#"
fn bump(state: State, key: Int) -> State
    State.update(state, counts = Map.set(state.counts, key, 5), served = state.served + 1)

fn main() -> Int
    before = fresh()
    after = bump(before, 7)
    get(before.counts, 7) * 1000 + get(after.counts, 7) * 100 + Map.len(before.counts)
"#,
    );
    let (answer, copied) = run(&src);
    assert_eq!(answer, 1000 + 5 * 100 + 200);
    assert!(copied > 0, "the shared Map was written in place");
}

/// A second local names the same record inside the function.
#[test]
fn a_record_another_local_still_holds_is_not_taken() {
    let src = program(
        r#"
fn bump(state: State, key: Int) -> Int
    kept = state
    after = State.update(state, counts = Map.set(state.counts, key, 5), served = state.served + 1)
    get(kept.counts, key) * 100 + get(after.counts, key)

fn main() -> Int
    bump(fresh(), 7)
"#,
    );
    let (answer, _) = run(&src);
    assert_eq!(answer, 100 + 5);
}

/// Another record holds the record: the take must see the off-stack holder.
#[test]
fn a_record_held_by_another_record_is_not_taken() {
    let src = program(
        r#"
fn bump(state: State, key: Int) -> State
    State.update(state, counts = Map.set(state.counts, key, 5), served = state.served + 1)

fn main() -> Int
    holder = Holder(inner = fresh())
    after = bump(holder.inner, 7)
    get(holder.inner.counts, 7) * 100 + get(after.counts, 7)
"#,
    );
    let (answer, copied) = run(&src);
    assert_eq!(answer, 100 + 5);
    assert!(copied > 0, "the shared Map was written in place");
}

/// One Map in two records: taking it out of the first record leaves the
/// second one holding it, so the write copies.
#[test]
fn a_map_two_records_share_is_not_written_in_place() {
    let src = program(
        r#"
fn bump(state: State, key: Int) -> State
    State.update(state, counts = Map.set(state.counts, key, 5), served = state.served + 1)

fn main() -> Int
    shared = fill({}, 200)
    first = State(counts = shared, other = {}, served = 0)
    second = State(counts = shared, other = {}, served = 0)
    after = bump(first, 7)
    get(second.counts, 7) * 100 + get(after.counts, 7)
"#,
    );
    let (answer, copied) = run(&src);
    assert_eq!(answer, 100 + 5);
    assert!(copied > 0, "the shared Map was written in place");
}

/// The field is read twice inside the update; the second read must still see
/// the whole Map.
#[test]
fn a_field_read_twice_in_the_update_is_not_taken() {
    let src = program(
        r#"
fn bump(state: State, key: Int) -> State
    State.update(state, counts = Map.set(state.counts, key, Map.len(state.counts)), served = state.served + 1)

fn main() -> Int
    after = bump(fresh(), 7)
    get(after.counts, 7) * 1000 + Map.len(after.counts)
"#,
    );
    let (answer, _) = run(&src);
    assert_eq!(answer, 200 * 1000 + 200);
}

/// The update reads a Map field it does not write: the new record keeps the
/// base's Map, so the field must stay in the base.
#[test]
fn a_field_the_update_does_not_write_is_not_taken() {
    let src = program(
        r#"
fn bump(state: State, key: Int) -> State
    State.update(state, served = Map.len(Map.set(state.counts, key, 5)))

fn main() -> Int
    after = bump(fresh(), 1000)
    after.served * 1000 + get(after.counts, 7) * 100 + Map.len(after.counts)
"#,
    );
    let (answer, _) = run(&src);
    assert_eq!(answer, 201 * 1000 + 100 + 200);
}

/// The local is read whole after the record literal that reads its field:
/// the literal is not its last use, so nothing is taken.
#[test]
fn a_record_read_after_the_literal_is_not_taken() {
    let src = program(
        r#"
fn bump(state: State, key: Int) -> Int
    after = State(counts = Map.set(state.counts, key, 5), other = {}, served = 0)
    get(state.counts, key) * 100 + get(after.counts, key)

fn main() -> Int
    bump(fresh(), 7)
"#,
    );
    let (answer, _) = run(&src);
    assert_eq!(answer, 100 + 5);
}

/// The measured fixture: two thousand requests against a hundred thousand
/// keys copy no entry. Before the take, each request copied all of them.
#[test]
fn the_fixture_updates_its_map_in_place() {
    let path = concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/fixtures/vm_record_field_update/main.av"
    );
    let src = std::fs::read_to_string(path).expect("read the fixture");
    let mut machine = compiled_vm(&src);
    machine.run().expect("the fixture should run");
    assert_eq!(
        machine.arena.map_entries_copied(),
        0,
        "a request copied the Map its state holds"
    );
}

/// The whole record goes into the literal beside its own field: the bare read
/// rules the take out.
#[test]
fn a_record_stored_whole_beside_its_field_is_not_taken() {
    let src = program(
        r#"
record Pair
    left: State
    right: Map<Int, Int>

fn split(state: State, key: Int) -> Pair
    Pair(left = state, right = Map.set(state.counts, key, 5))

fn main() -> Int
    pair = split(fresh(), 7)
    get(pair.left.counts, 7) * 100 + get(pair.right, 7)
"#,
    );
    let (answer, _) = run(&src);
    assert_eq!(answer, 100 + 5);
}

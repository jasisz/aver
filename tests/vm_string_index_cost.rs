//! What an indexed `String.charAt` walk is allowed to cost on the VM.
//!
//! `String.charAt` used to rescan the string from the start, so a loop reading
//! it character by character was quadratic; the fix builds one hidden table of
//! UTF-8 boundaries per loop and threads it through as an extra argument. That
//! made the *reads* constant-time, and the coverage that came with it pins
//! exactly that: which shapes get a table, what the workers are called, and
//! that the four table intrinsics agree across the VM, generated Rust and
//! wasm-gc on four-character inputs.
//!
//! None of it says what carrying the table costs, and on the VM the table is an
//! ordinary arena vector of byte offsets. Whenever a step allocates — which it
//! does as soon as the character is USED rather than discarded, because
//! `Option.Some(<one-character String>)` needs a box — the collector runs, and
//! it read the whole table on every run to decide that byte offsets never move.
//! Linear reads, linear allocations, and a quadratic program.
//!
//! Measured before the fix, on the shape below at 32,768 / 65,536 / 131,072
//! characters: 0.58 / 1.73 / 6.50 s, roughly quadrupling per doubling. The
//! sibling shape that compares the character against a literal never allocates,
//! never collects, and stayed at 0.14 / 0.15 / 0.16 s — which is why the
//! difference was invisible to everything already in the suite. The control
//! that isolates it holds the number of reads fixed and varies only the length
//! of the table: 20,000 reads over 16,384 / 65,536 / 262,144 characters cost
//! 572 / 2,528 / 9,815 ms before and 78 / 88 / 104 ms after.
//!
//! The instrument here is `Arena::vector_elements_scanned`, the vector
//! counterpart of the list and map scan counters the earlier collection-cost
//! guards use. It counts elements the collector READ, so the defect shows up as
//! a number instead of a wall-clock reading: before the fix a walk of n
//! characters scanned on the order of n^2/5 elements, after it scans none,
//! because a table of immediates carries a flag saying it holds nothing the
//! collector could rewrite.
//!
//! Three shapes, because they are the three ways a character escapes its match
//! arm — used in an expression, passed to another function, stored in a
//! collection — and each one independently defeats the "discard the character"
//! spelling that the existing tests all use.
//!
//! What these tests do NOT claim: that the whole program is linear. Each one
//! claims only that carrying the hidden index costs nothing, which is what the
//! counter measures and what changed. The third shape is still super-linear for
//! an unrelated reason — a `List.prepend` accumulator that the list-build
//! fusion declines has its chain re-walked by the collector, which reproduces
//! with no string index in sight — so a wall-clock assertion there would be
//! measuring something else entirely.

use aver::ir::pipeline::{PipelineConfig, TypecheckMode};
use aver::nan_value::Arena;
use aver::vm::{self, VM};

/// Elements the collector read, plus enough to prove the run was the run we
/// meant to measure.
struct Walk {
    /// Vector elements the collector read while promoting live values — the
    /// hidden `String.Index` is what it is reading.
    scanned: u64,
    /// What the program answered. A walk that stopped early would make any
    /// cost assertion pass for the wrong reason.
    answer: i64,
    /// How many times the table's character read ran. Zero would mean the pass
    /// stopped lowering this shape to the allocating intrinsic, which is the
    /// other way the cost assertion could go green without the cost being
    /// fixed.
    char_at_ops: u64,
}

/// Compile through the real pipeline, typecheck included.
///
/// The typecheck is load-bearing: the hidden-index pass runs as part of
/// `ir::pipeline::run`, so a helper that only resolves — the one the VM's own
/// unit tests use — would never build a table at all and would measure nothing.
fn walk(src: &str) -> Walk {
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
    let (code, globals) = vm::compile_program_with_mir_fallback(
        &result.resolved_items,
        &result.symbol_table,
        &mut arena,
        None,
    )
    .expect("compile failed");
    let mut machine = VM::new(code, globals, arena);
    machine.start_profiling();
    let value = machine.run().expect("program should run");
    let report = machine.profile_report().expect("profiling was started");
    let char_at_ops = report
        .opcodes
        .iter()
        .find(|entry| entry.name == "STR_INDEX_CHAR_AT")
        .map(|entry| entry.count)
        .unwrap_or(0);
    Walk {
        scanned: machine.arena.vector_elements_scanned(),
        answer: value.as_int(&machine.arena),
        char_at_ops,
    }
}

/// A mixed-width seed doubled `rounds` times: one ASCII, one two-byte, one
/// four-byte and one ASCII character, so the table's boundaries are genuinely
/// irregular and the walk cannot accidentally be a byte walk.
const SEED_CHARS: i64 = 4;

/// The character is USED — its length goes into the accumulator, so the match
/// arm has to materialise a one-character `String` and box it.
fn character_used_in_an_expression(rounds: i64) -> String {
    format!(
        r#"module Walk
    intent = "read every character and use it"
    depends []
    effects []

fn grow(rounds: Int, text: String) -> String
    ? "double a mixed-width seed"
    match rounds
        0 -> text
        _ -> grow(rounds - 1, text + text)

fn count(text: String, position: Int, total: Int) -> Int
    ? "walk by index, adding each character's length"
    match String.charAt(text, position)
        Option.None -> total
        Option.Some(c) -> count(text, position + 1, total + String.len(c))

fn main() -> Int
    ? "characters visited"
    count(grow({rounds}, "aą😀z"), 0, 0)
"#
    )
}

/// The character is PASSED ON — a mutually recursive pair hands it to the
/// other half, which is the shape a hand-written parser has.
fn character_passed_to_another_function(rounds: i64) -> String {
    format!(
        r#"module Walk
    intent = "read every character and pass it on"
    depends []
    effects []

fn grow(rounds: Int, text: String) -> String
    ? "double a mixed-width seed"
    match rounds
        0 -> text
        _ -> grow(rounds - 1, text + text)

fn walk(text: String, position: Int, total: Int) -> Int
    ? "first half of the pair"
    match String.charAt(text, position)
        Option.None -> total
        Option.Some(hi) -> walkNext(text, position, hi, total)

fn walkNext(text: String, position: Int, seen: String, total: Int) -> Int
    ? "second half of the pair, holding the character it was handed"
    match String.charAt(text, position + 1)
        Option.None -> total + String.len(seen)
        Option.Some(lo) -> walk(text, position + 2, total + String.len(seen) + String.len(lo))

fn main() -> Int
    ? "characters visited"
    walk(grow({rounds}, "aą😀z"), 0, 0)
"#
    )
}

/// The character is STORED — every step prepends into a list, so the step
/// allocates twice and the accumulator grows beside the table.
fn character_stored_in_a_list(rounds: i64) -> String {
    format!(
        r#"module Walk
    intent = "read every character and keep it"
    depends []
    effects []

fn grow(rounds: Int, text: String) -> String
    ? "double a mixed-width seed"
    match rounds
        0 -> text
        _ -> grow(rounds - 1, text + text)

fn walk(text: String, position: Int, acc: List<Int>) -> List<Int>
    ? "walk by index, keeping each character's length"
    match String.charAt(text, position)
        Option.None -> acc
        Option.Some(c) -> walk(text, position + 1, List.prepend(String.len(c), acc))

fn main() -> Int
    ? "characters visited"
    List.len(walk(grow({rounds}, "aą😀z"), 0, []))
"#
    )
}

/// Reads the collector is allowed per character visited.
///
/// A fixed table read once per character would sit at 1. The claim is stronger
/// than that — after the fix all three shapes read nothing at all — so the
/// budget only has to be small enough to be nowhere near a table re-read per
/// collection, which is what n/5 reads of an n-element table looks like.
const SCAN_BUDGET_PER_CHARACTER: u64 = 4;

/// Run one shape at n and 2n characters and hold both to the same three
/// claims: the reads stay inside the per-character budget, doubling the input
/// does not more than triple them, and the program still walked what it said
/// it walked.
fn assert_the_hidden_index_is_not_reread(name: &str, program: &dyn Fn(i64) -> String, rounds: i64) {
    let small_chars = (SEED_CHARS << rounds) as u64;
    let large_chars = small_chars * 2;
    let small = walk(&program(rounds));
    let large = walk(&program(rounds + 1));

    assert_eq!(
        (small.answer, large.answer),
        (small_chars as i64, large_chars as i64),
        "{name}: the walk did not visit every character, so its cost says nothing"
    );
    assert_eq!(
        (small.char_at_ops, large.char_at_ops),
        (small_chars + 1, large_chars + 1),
        "{name}: this shape no longer reads characters through the hidden index, \
         so it is not the shape this guard was written for"
    );

    assert!(
        small.scanned <= small_chars * SCAN_BUDGET_PER_CHARACTER
            && large.scanned <= large_chars * SCAN_BUDGET_PER_CHARACTER,
        "{name}: the collector re-reads the hidden string index. \
         {small_chars} characters scanned {} elements and {large_chars} characters \
         scanned {}, against a budget of {SCAN_BUDGET_PER_CHARACTER} per character",
        small.scanned,
        large.scanned,
    );
    assert!(
        large.scanned <= 3 * small.scanned + large_chars,
        "{name}: doubling the input more than tripled the collector's reads, \
         so carrying the hidden string index is still super-linear. \
         {small_chars} characters scanned {} elements, {large_chars} scanned {}",
        small.scanned,
        large.scanned,
    );
}

#[test]
fn an_indexed_walk_that_uses_the_character_does_not_reread_the_index() {
    assert_the_hidden_index_is_not_reread(
        "character used in an expression",
        &character_used_in_an_expression,
        10,
    );
}

#[test]
fn an_indexed_walk_that_passes_the_character_on_does_not_reread_the_index() {
    assert_the_hidden_index_is_not_reread(
        "character passed to another function",
        &character_passed_to_another_function,
        10,
    );
}

#[test]
fn an_indexed_walk_that_stores_the_character_does_not_reread_the_index() {
    assert_the_hidden_index_is_not_reread(
        "character stored in a list",
        &character_stored_in_a_list,
        10,
    );
}

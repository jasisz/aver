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
//! Four shapes. Three are the three ways a character escapes its match arm —
//! used in an expression, passed to another function, stored in a collection —
//! and each one independently defeats the "discard the character" spelling that
//! the existing tests all use. The fourth reads a fixed-width `String.slice`
//! rather than a character: it is the other read that materialises a new
//! String, it allocates for the same reason, and it followed the same curve.
//! Its own fixed-count control, 20,000 eight-character slices over 16,384 /
//! 65,536 / 262,144 characters, cost 576 / 2,533 / 9,892 ms before the fix and
//! 81 / 85 / 98 ms after.
//!
//! The third shape turned out to be paying twice, for two different reasons,
//! and the second instrument here is the one that sees the other half. Its
//! `List.prepend` accumulator is a chain of cells, one arena entry each, and
//! the promotion followed that chain from the top every time it ran instead of
//! stopping where the chain leaves the region it can move. Same shape of cost
//! as the table — linear reads per step, quadratic program — and the same
//! invisibility: nothing is copied, so no copy counter moves, and the shape has
//! neither a map nor a flat list, so neither scan counter moves either.
//!
//! `Arena::out_of_region_entries_read` counts it: entries a promotion read
//! although it was never allowed to move them. Before the fix this shape read
//! 2,795,520 of them over 4,096 characters and 11,180,715 over 8,192 — the same
//! quadrupling per doubling. After it, 2,730 and 5,460, which is flat per
//! character. Wall clock on the standalone reproducer, walking 16,384 / 32,768
//! / 65,536 characters and keeping each one: 3,236 / 13,079 / 50,169 ms before,
//! 37 / 76 / 147 ms after.
//!
//! Both instruments are applied to all four shapes, so a shape that stops
//! paying one way and starts paying the other cannot go green.
//!
//! What these tests do NOT claim: that any whole program is linear. They claim
//! that a step of an indexed walk pays only for itself, and not for everything
//! the walk has already put behind it — the two ways it used to.

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
    /// How many times the shape's own read through the table ran. Zero would
    /// mean the pass stopped lowering this shape to the allocating read, which
    /// is the other way the cost assertion could go green without the cost
    /// being fixed.
    reads: u64,
    /// Arena entries the collector read that it was never allowed to move —
    /// everything the loop had already put behind it. The hidden table is one
    /// of them; so is every cell of a list the loop is building.
    revisited: u64,
}

/// Which read through the hidden table a shape is supposed to be making, and
/// what a walk of `chars` characters owes for it.
///
/// Both allocating reads get a guard. `__str_index_char_at` materialises a
/// one-character String and `__str_index_slice` materialises a longer one; they
/// allocate for the same reason and were quadratic on the same curve, and the
/// suite named neither.
struct IndexedRead {
    /// The opcode the shape must lower to. Named rather than inferred, because
    /// the whole point is to catch a shape that quietly stops using it.
    opcode: &'static str,
    /// Times that opcode must run for a walk of `chars` characters.
    reads: fn(u64) -> u64,
    /// The answer the program must give for a walk of `chars` characters.
    answer: fn(u64) -> i64,
}

/// Reading one character per position, plus the read that finds the end.
const CHARACTER_AT_EVERY_POSITION: IndexedRead = IndexedRead {
    opcode: "STR_INDEX_CHAR_AT",
    reads: |chars| chars + 1,
    answer: |chars| chars as i64,
};

/// Compile through the real pipeline, typecheck included.
///
/// The typecheck is load-bearing: the hidden-index pass runs as part of
/// `ir::pipeline::run`, so a helper that only resolves — the one the VM's own
/// unit tests use — would never build a table at all and would measure nothing.
fn walk(src: &str, read: &IndexedRead) -> Walk {
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
    let reads = report
        .opcodes
        .iter()
        .find(|entry| entry.name == read.opcode)
        .map(|entry| entry.count)
        .unwrap_or(0);
    Walk {
        scanned: machine.arena.vector_elements_scanned(),
        answer: value.as_int(&machine.arena),
        reads,
        revisited: machine.arena.out_of_region_entries_read(),
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

/// A fixed-width SLICE is taken at every position — the other read that has to
/// build a new String, so the other read that allocates once per step.
///
/// The loop is driven by a counter rather than by the end of the string,
/// because that is what makes the slice the only read through the table.
fn a_slice_taken_at_every_position(rounds: i64) -> String {
    let chars = SEED_CHARS << rounds;
    format!(
        r#"module Walk
    intent = "take a fixed-width slice at every position"
    depends []
    effects []

fn grow(rounds: Int, text: String) -> String
    ? "double a mixed-width seed"
    match rounds
        0 -> text
        _ -> grow(rounds - 1, text + text)

fn walk(text: String, position: Int, remaining: Int, total: Int) -> Int
    ? "walk by index, adding the length of an eight-character slice"
    match remaining
        0 -> total
        _ -> walk(text, position + 1, remaining - 1, total + String.len(String.slice(text, position, position + 8)))

fn main() -> Int
    ? "characters the slices covered"
    walk(grow({rounds}, "aą😀z"), 0, {chars}, 0)
"#
    )
}

/// One slice per position, and no read to find the end — the counter does that.
///
/// Every position but the last seven yields eight characters; the last seven
/// yield seven down to one as the slice runs off the end, which is 28 more.
const SLICE_AT_EVERY_POSITION: IndexedRead = IndexedRead {
    opcode: "STR_INDEX_SLICE",
    reads: |chars| chars,
    answer: |chars| (8 * chars - 28) as i64,
};

/// Reads the collector is allowed per character visited.
///
/// A fixed table read once per character would sit at 1. The claim is stronger
/// than that — after the fix all three shapes read nothing at all — so the
/// budget only has to be small enough to be nowhere near a table re-read per
/// collection, which is what n/5 reads of an n-element table looks like.
const SCAN_BUDGET_PER_CHARACTER: u64 = 4;

/// Settled entries the collector is allowed to read per character visited.
///
/// A collection that touches only what the step itself allocated reads none of
/// them, so the honest budget is zero. It is not zero here because a boundary
/// still reaches the one entry directly below the values it promoted, and a
/// walk of n characters crosses a boundary a bounded number of times per
/// character. Anything proportional to what the loop has ALREADY built —
/// re-reading the table, or walking the accumulator from the top — lands orders
/// of magnitude above this.
const REVISIT_BUDGET_PER_CHARACTER: u64 = 4;

/// Run one shape at n and 2n characters and hold both to the same three
/// claims: the reads stay inside the per-character budget, doubling the input
/// does not more than triple them, and the program still walked what it said
/// it walked.
fn assert_the_hidden_index_is_not_reread(
    name: &str,
    program: &dyn Fn(i64) -> String,
    rounds: i64,
    read: &IndexedRead,
) {
    let small_chars = (SEED_CHARS << rounds) as u64;
    let large_chars = small_chars * 2;
    let small = walk(&program(rounds), read);
    let large = walk(&program(rounds + 1), read);

    assert_eq!(
        (small.answer, large.answer),
        ((read.answer)(small_chars), (read.answer)(large_chars)),
        "{name}: the walk did not visit every character, so its cost says nothing"
    );
    assert_eq!(
        (small.reads, large.reads),
        ((read.reads)(small_chars), (read.reads)(large_chars)),
        "{name}: this shape no longer reads through the hidden index with {}, \
         so it is not the shape this guard was written for",
        read.opcode
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

    assert!(
        small.revisited <= small_chars * REVISIT_BUDGET_PER_CHARACTER
            && large.revisited <= large_chars * REVISIT_BUDGET_PER_CHARACTER,
        "{name}: the collector re-reads memory the loop has already settled. \
         {small_chars} characters read {} settled entries and {large_chars} characters \
         read {}, against a budget of {REVISIT_BUDGET_PER_CHARACTER} per character",
        small.revisited,
        large.revisited,
    );
    assert!(
        large.revisited <= 3 * small.revisited + large_chars,
        "{name}: doubling the input more than tripled the settled entries the \
         collector read, so the walk still pays for everything behind it. \
         {small_chars} characters read {}, {large_chars} read {}",
        small.revisited,
        large.revisited,
    );
}

#[test]
fn an_indexed_walk_that_uses_the_character_does_not_reread_the_index() {
    assert_the_hidden_index_is_not_reread(
        "character used in an expression",
        &character_used_in_an_expression,
        10,
        &CHARACTER_AT_EVERY_POSITION,
    );
}

#[test]
fn an_indexed_walk_that_passes_the_character_on_does_not_reread_the_index() {
    assert_the_hidden_index_is_not_reread(
        "character passed to another function",
        &character_passed_to_another_function,
        10,
        &CHARACTER_AT_EVERY_POSITION,
    );
}

#[test]
fn an_indexed_walk_that_stores_the_character_does_not_reread_the_index() {
    assert_the_hidden_index_is_not_reread(
        "character stored in a list",
        &character_stored_in_a_list,
        10,
        &CHARACTER_AT_EVERY_POSITION,
    );
}

#[test]
fn an_indexed_walk_that_slices_does_not_reread_the_index() {
    assert_the_hidden_index_is_not_reread(
        "fixed-width slice at every position",
        &a_slice_taken_at_every_position,
        10,
        &SLICE_AT_EVERY_POSITION,
    );
}

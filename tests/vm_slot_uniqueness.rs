//! Live references to an arena slot, and the directional cross-check against
//! the compiler's static owned mask.
//!
//! Locals are a window on the operand stack, so "how many live references does
//! this arena slot have?" is answered by counting the operand-stack cells that
//! hold its index. The tests below pin the two things that make it worth having:
//!
//! - every path a cell can leave the stack by really does give it back, asked
//!   WHERE THE ANSWER IS USED — at a collection write reached after the exit,
//!   with the map still in reach of the frame that left. A frame that kept a
//!   cell of its own turns that write from one nobody else holds into one
//!   somebody does, and the tallies move by one in each direction;
//! - the directional comparison against the owned mask holds in the soundness
//!   direction (`granted ⊆ runtime-unique`) while the OTHER direction is
//!   non-empty, which is the point of the exercise: the runtime sees slots as
//!   uniquely held that the static analysis declined;
//! - and the blind spots are where the module says they are — a vector fold
//!   reaches none of this, so both tallies are Map-shaped numbers.
//!
//! The exit-path tests are the ones that needed care. Asking "is the stack
//! empty once the run is over?" is not a question about any exit path in
//! particular: a successful run ends with an empty stack whatever an inner
//! frame did, because the frame above it truncates to its own base on the way
//! out. Every one of them was watched to fail against a deleted truncate, and
//! each deletion reddens exactly one of them:
//!
//! | deleted from `src/vm/execute/dispatch.rs`   | test that goes red |
//! |---|---|
//! | `RETURN`, framed                            | `an_ordinary_return_...` |
//! | `RETURN`, frameless leaf (#917)             | `a_frameless_leaf_return_...` |
//! | `PROPAGATE_ERR`                             | `error_propagation_...` |
//! | `leaf_error_return!` (#917)                 | `a_frameless_leaf_error_exit_...` |
//! | `CALL_PAR`, error branch                    | `an_independent_product_error_...` |
//! | `TAIL_CALL_SELF` / `_KNOWN` / `_SELF_THIN`  | `every_tail_call_window_...` |
//!
//! Where the write after the exit is one the compiler granted, the deletion
//! trips the soundness assertion at the call itself; where it is one the
//! compiler declined, it moves the write between two tallies this file spells
//! out in full. Both are red, and both say which slot.
//!
//! Nothing here decides anything. The count is observed, not consulted.

use aver::ir::pipeline::{PipelineConfig, TypecheckMode};
use aver::nan_value::Arena;
use aver::vm::{self, VM, VmSlotUniquenessStats};

/// Compile through the real pipeline, typecheck included.
///
/// The typecheck is load-bearing rather than incidental: `ir::alias`'s
/// destination half is guarded by `slot_is_collection`, so without types every
/// binding slot stays `Type::Invalid` and the collection rules never fire —
/// which would quietly change what the owned mask says and therefore what this
/// file is comparing against.
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
    let (code, globals) = vm::compile_program_with_mir_fallback(
        &result.resolved_items,
        &result.symbol_table,
        &mut arena,
        None,
    )
    .expect("compile failed");
    let mut machine = VM::new(code, globals, arena);
    // The cross-check only walks the stack when somebody is listening: a debug
    // build, or a run that asked for the profile. Tests take the second door as
    // well as the first, so what they pin does not depend on the profile they
    // were compiled under.
    machine.start_profiling();
    machine
}

/// Run to completion and hand back how the cross-check sorted every collection
/// write the run made.
fn tallies(src: &str) -> VmSlotUniquenessStats {
    let mut machine = compiled_vm(src);
    machine.run().expect("program should run");
    machine.slot_uniqueness_stats()
}

/// The four numbers, spelled out, so a shape pins every bucket rather than the
/// one it is about.
fn sorted_writes(
    owned_grants: u64,
    owned_grants_without_unique_slot: u64,
    unique_slot_without_owned_grant: u64,
    declined_with_slot_still_held: u64,
) -> VmSlotUniquenessStats {
    VmSlotUniquenessStats {
        owned_grants,
        owned_grants_without_unique_slot,
        unique_slot_without_owned_grant,
        declined_with_slot_still_held,
    }
}

// ---------------------------------------------------------------------------
// Every exit path gives back the cell it held
// ---------------------------------------------------------------------------
//
// The shape is the same for the first five: `main` builds a map, hands it to a
// frame that leaves through the path under test WITHOUT ever reading it — so
// the map is still sitting in that frame's window at the moment it goes — and
// then writes into the map twice. The first write happens while `main` itself
// still holds the map, so it is a write the count is SUPPOSED to see a holder
// for; the second is at the map's last use, where the only cell that could
// still hold it is one the departed frame failed to give back.

#[test]
fn an_ordinary_return_hands_back_the_cell_it_held() {
    // `peek` calls a user function, so it gets a frame of its own and leaves
    // through the framed `RETURN`.
    let stats = tallies(
        r#"module SlotOrdinaryReturn
    intent = "an ordinary return hands back the cell it held"
    depends []
    effects []

fn tick() -> Int
    ? "one call, so peek is a frame rather than a leaf chunk"
    1

fn peek(m: Map<String, String>, flag: Int) -> Int
    ? "hold the caller's map across a return down a branch that never reads it"
    n = tick()
    match flag > 0
        true -> Map.len(m) + n
        false -> n

fn main() -> Int
    ? "write into the map on both sides of its own last use"
    m = Map.fromList([("s", "z")])
    k = peek(m, 0)
    held = Map.len(Map.set(m, "a", "1"))
    Map.len(Map.set(m, "b", "2")) + held + k
"#,
    );
    assert_eq!(
        stats,
        sorted_writes(0, 0, 1, 1),
        "the two writes after an ordinary return should be one with a holder \
         (main's own cell) and one without; a returning frame that kept the \
         caller's map moves the second into the first",
    );
}

#[test]
fn a_frameless_leaf_return_hands_back_the_cell_it_held() {
    // Same shape with no user call inside `peek`, so it is a leaf chunk and its
    // return is the frameless one #917 taught to consult `leaf_return`.
    let stats = tallies(
        r#"module SlotLeafReturn
    intent = "a frameless leaf return hands back the cell it held"
    depends []
    effects []

fn peek(m: Map<String, String>, flag: Int) -> Int
    ? "a leaf holding the caller's map across its own return"
    match flag > 0
        true -> Map.len(m)
        false -> 7

fn main() -> Int
    ? "write into the map on both sides of its own last use"
    m = Map.fromList([("s", "z")])
    k = peek(m, 0)
    held = Map.len(Map.set(m, "a", "1"))
    Map.len(Map.set(m, "b", "2")) + held + k
"#,
    );
    assert_eq!(
        stats,
        sorted_writes(0, 0, 1, 1),
        "the frameless leaf return owes the same discharge as the framed one",
    );
}

#[test]
fn error_propagation_hands_back_the_cell_the_frame_held() {
    // `?` on a failed call leaves through `PROPAGATE_ERR`, which pops the frame
    // and truncates itself rather than going through `RETURN`. `store` fails
    // BEFORE it reaches its own write, so the caller's map is still in its
    // parameter cell when it goes.
    let stats = tallies(
        r#"module SlotErrorExit
    intent = "the error exit hands back the cell the frame held"
    depends []
    effects []

fn check(tag: String) -> Result<String, String>
    ? "fail on anything but the one word"
    match tag == "ok"
        true -> Result.Ok(tag)
        false -> Result.Err("bad input: {tag}")

fn store(tag: String, m: Map<String, String>) -> Result<Int, String>
    ? "leave through ? with the caller's map still in a cell"
    word = check(tag)?
    Result.Ok(Map.len(Map.set(m, word, "v")))

fn main() -> Int
    ? "take the error exit, then write on both sides of the map's last use"
    m = Map.fromList([("s", "z")])
    _ = store("no", m)
    held = Map.len(Map.set(m, "a", "1"))
    Map.len(Map.set(m, "b", "2")) + held
"#,
    );
    assert_eq!(
        stats,
        sorted_writes(0, 0, 1, 1),
        "an error exit that kept the caller's map turns the write at its last \
         use into one somebody still holds",
    );
}

#[test]
fn a_frameless_leaf_error_exit_hands_back_the_cell_it_held() {
    // The second exit #917 is about: a body that binds no name and calls
    // nothing the user wrote is a leaf chunk, and `?` inside it leaves through
    // `leaf_error_return!`, which truncates to the leaf's own base. Only a
    // BUILTIN can fail there, since a call to a user function would cost the
    // chunk its leaf status.
    let stats = tallies(
        r#"module SlotLeafErrorExit
    intent = "the frameless leaf error exit hands back the cell it held"
    depends []
    effects []

fn measure(text: String, m: Map<String, String>) -> Result<Int, String>
    ? "a leaf whose only fallible call is a builtin"
    Result.Ok(Int.fromString(text)? + Map.len(m))

fn main() -> Int
    ? "take the leaf error exit, then write on both sides of the map's last use"
    m = Map.fromList([("s", "z")])
    _ = measure("no", m)
    held = Map.len(Map.set(m, "a", "1"))
    Map.len(Map.set(m, "b", "2")) + held
"#,
    );
    assert_eq!(
        stats,
        sorted_writes(1, 0, 0, 1),
        "the leaf error exit kept the caller's map, so the write at its last \
         use — the one the compiler granted — no longer owns what it was given",
    );
}

#[test]
fn an_independent_product_error_hands_back_the_cell_the_frame_held() {
    // `(a, b)?!` runs its branches through child VMs and unwraps the results in
    // the parent; a branch that answers `Result.Err` leaves the enclosing frame
    // through `CALL_PAR`'s own error exit, which pops and truncates like
    // `PROPAGATE_ERR` does. `both` calls a user function, so the frame it
    // leaves is a real one.
    let stats = tallies(
        r#"module SlotIndependentProduct
    intent = "the independent-product error exit hands back the cell it held"
    depends []
    effects []

fn size(m: Map<String, String>) -> Int
    ? "one call, so both is a frame rather than a leaf chunk"
    Map.len(m)

fn both(m: Map<String, String>) -> Result<Int, String>
    ? "two independent branches, the first of which fails"
    pair = (Int.fromString("no"), Int.fromString("1"))?!
    match pair
        (a, b) -> Result.Ok(a + b + size(m))

fn main() -> Int
    ? "take the parallel error exit, then write on both sides of the last use"
    m = Map.fromList([("s", "z")])
    _ = both(m)
    held = Map.len(Map.set(m, "a", "1"))
    Map.len(Map.set(m, "b", "2")) + held
"#,
    );
    assert_eq!(
        stats,
        sorted_writes(1, 0, 0, 1),
        "the parallel error exit kept the caller's map, so the granted write \
         after it no longer owns what it was given",
    );
}

/// A fold that inserts once per step into a linearly threaded accumulator,
/// spelled three ways so all three tail-call windows are exercised.
///
/// The observation is the fold itself: every insert is a write the compiler
/// granted ownership of, so a window that kept the PREVIOUS iteration's
/// accumulator cell makes the very next insert a grant over a slot somebody
/// else holds — the soundness direction, at the instruction where it is read.
const TAIL_CALL_FOLDS: [(&str, &str); 3] = [
    (
        "TAIL_CALL_SELF",
        r#"module SlotTailSelf
    intent = "a self tail call hands back the previous window"
    depends []
    effects []

fn build(n: Int, acc: Map<String, String>) -> Map<String, String>
    ? "insert an interpolated key once per step"
    match n > 0
        true -> build(n - 1, Map.set(acc, "k{n}", "v{n}"))
        false -> acc

fn main() -> Int
    ? "size of what the fold built"
    Map.len(build(8, {}))
"#,
    ),
    (
        "TAIL_CALL_KNOWN",
        r#"module SlotTailKnown
    intent = "a mutual tail call hands back the previous window"
    depends []
    effects []

fn even(n: Int, acc: Map<String, String>) -> Map<String, String>
    ? "hand the accumulator to the other function"
    match n > 0
        true -> odd(n - 1, Map.set(acc, "e{n}", "v"))
        false -> acc

fn odd(n: Int, acc: Map<String, String>) -> Map<String, String>
    ? "hand the accumulator back"
    match n > 0
        true -> even(n - 1, Map.set(acc, "o{n}", "v"))
        false -> acc

fn main() -> Int
    ? "size of what the mutual fold built"
    Map.len(even(8, {}))
"#,
    ),
    (
        "TAIL_CALL_SELF_THIN",
        // No interpolation anywhere, so the chunk stays thin and its self tail
        // call takes the third window.
        r#"module SlotTailThin
    intent = "a thin self tail call hands back the previous window"
    depends []
    effects []

fn build(n: Int, acc: Map<String, String>) -> Map<String, String>
    ? "insert a constant key once per step"
    match n > 0
        true -> build(n - 1, Map.set(acc, "k", "v"))
        false -> acc

fn main() -> Int
    ? "size of what the fold built"
    Map.len(build(8, {}))
"#,
    ),
];

#[test]
fn every_tail_call_window_hands_back_the_previous_iterations_cell() {
    for (window, src) in TAIL_CALL_FOLDS {
        assert_eq!(
            tallies(src),
            sorted_writes(8, 0, 0, 0),
            "{window}: eight steps should take eight owned grants over a slot \
             the previous iteration has let go of",
        );
    }
}

// ---------------------------------------------------------------------------
// What the cross-check never sees
// ---------------------------------------------------------------------------

#[test]
fn a_vector_fold_writes_nothing_the_cross_check_can_see() {
    // The module lists four blind spots; this pins the two on the vector side,
    // because they are the ones that decide how the numbers should be READ. A
    // declined `Vector.set` is lowered to its own `VECTOR_SET` opcode and never
    // becomes a `CALL_BUILTIN` at all, and the fused `Vector.set(v, i, x)`
    // kept-by-default spelling becomes `VECTOR_SET_OR_KEEP` — the VM's only true
    // in-place arena write, and the one grant this predicate could not answer
    // even if it were asked, since the fused shape reads the vector with
    // `LOAD_LOCAL` and leaves the target's own cell live across the write.
    //
    // So `unique_slot_without_owned_grant` is a MAP number, and anyone sizing a
    // P2 decision from it is reading a Map-shaped floor. The day the vector side
    // is brought in, this test is what will say so.
    let stats = tallies(
        r#"module SlotVectorFold
    intent = "a vector threaded through a fold, and a set that keeps a default"
    depends []
    effects []

fn fill(v: Vector<Int>, n: Int, i: Int) -> Vector<Int>
    ? "the fused in-place spelling: set, or keep what was there"
    match i == n
        true -> v
        false -> fill(Option.withDefault(Vector.set(v, i, i * i), v), n, i + 1)

fn overwrite(v: Vector<Int>, i: Int) -> Vector<Int>
    ? "a set whose default is a different vector, so nothing fuses"
    Option.withDefault(Vector.set(v, i, 0), Vector.new(1, 0))

fn main() -> Int
    ? "run both vector spellings"
    filled = fill(Vector.new(8, 0), 8, 0)
    kept = overwrite(filled, 1)
    Option.withDefault(Vector.get(kept, 1), 0)
"#,
    );
    assert_eq!(
        stats,
        sorted_writes(0, 0, 0, 0),
        "a vector fold reached the cross-check, so the blind spots the module \
         enumerates are no longer the ones it has — re-read that list before \
         trusting either tally as a whole-VM number",
    );
}

// ---------------------------------------------------------------------------
// The directional cross-check
// ---------------------------------------------------------------------------

/// The linear map fold: `n` inserts into one accumulator threaded through a
/// tail call, seeded from `seed`.
///
/// Everything but the seed is fixed, so a difference in the tallies is a
/// statement about the seed's spelling alone — the same isolation the map-copy
/// tests in `src/vm/execute/tests.rs` are built on.
fn map_fold(seed: &str) -> String {
    format!(
        r#"module SlotMapFold
    intent = "a map threaded through a tail-recursive fold"
    depends []
    effects []

fn build(n: Int, acc: Map<String, String>) -> Map<String, String>
    ? "insert once per step into a linearly threaded accumulator"
    match n > 0
        true -> build(n - 1, Map.set(acc, "k{{n}}", "v{{n}}"))
        false -> acc

fn main() -> Int
    ? "size of what the fold built"
    Map.len(build(40, {seed}))
"#
    )
}

#[test]
fn every_static_owned_grant_is_a_slot_the_runtime_sees_as_uniquely_held() {
    // The soundness direction. A `CALL_BUILTIN_OWNED` mask claims nothing else
    // can observe the mutation, so it has to be a SUBSET of what the runtime
    // count calls unique. The fold below is the shape the mask was built for, so
    // it exercises the grant hundreds of times in one run; under debug
    // assertions each one is checked at the call itself, and the tally is the
    // same statement in a form a release build can also make.
    let stats = tallies(&map_fold("{}"));

    assert!(
        stats.owned_grants > 0,
        "the fold took no owned grant at all, so this test is checking nothing \
         — the static mask stopped reaching the shape it was built for",
    );
    assert_eq!(
        stats.owned_grants_without_unique_slot, 0,
        "{} of {} static owned grants named a slot other operand-stack cells \
         still held",
        stats.owned_grants_without_unique_slot, stats.owned_grants,
    );
}

/// Declined-but-unique writes left by one evaluation of a `k`-entry map literal
/// seed, and the grants the fold after it took.
fn literal_seed_tallies(entries: &[(&str, &str)]) -> (u64, u64) {
    let spelled = entries
        .iter()
        .map(|(key, value)| format!("\"{key}\" => \"{value}\""))
        .collect::<Vec<_>>()
        .join(", ");
    let stats = tallies(&map_fold(&format!("{{{spelled}}}")));
    assert_eq!(
        stats.owned_grants_without_unique_slot, 0,
        "the soundness direction broke while measuring the other one",
    );
    (stats.unique_slot_without_owned_grant, stats.owned_grants)
}

#[test]
fn the_runtime_sees_unique_slots_the_static_mask_declined() {
    // The other direction, and the reason the axis exists. A non-empty map
    // literal lowers to a `LOAD_CONST` of an empty map plus one PLAIN
    // `CALL_BUILTIN` per entry, never an owned one — the static analysis
    // declines every insert the literal itself makes, and
    // `a_non_empty_map_literal_seed_pays_for_its_own_entries_and_nothing_more`
    // in the VM's own tests measures what that costs: 0+1+..+(k-1) duplicated
    // entries per evaluation. The count sees straight through it: one
    // declined-but-unique write per entry, and the difference between three
    // entries and four is exactly one.
    //
    // One of those writes is NOT a slot a decision could take. The literal's
    // first insert targets the empty map the chunk holds as a CONSTANT, and a
    // constant is exactly the kind of holder this count cannot see — it counts
    // operand-stack cells, and the constant table is not one. That is the number
    // being honest about its own definition rather than a flaw in it: a preview
    // of where to look, not a list of grants, and whoever turns it into a
    // decision owes the other root sets an argument of their own.
    let (three, grants) = literal_seed_tallies(&[("a", "1"), ("b", "2"), ("c", "3")]);
    let (four, _) = literal_seed_tallies(&[("a", "1"), ("b", "2"), ("c", "3"), ("d", "4")]);

    assert_eq!(
        (three, four),
        (3, 4),
        "a map literal should leave one declined-but-unique write per entry: \
         three entries left {three}, four left {four}",
    );
    assert!(
        grants > 0,
        "the fold after the literal took no owned grant, so this run is not the \
         mixed shape the comparison assumes",
    );
}

/// The same fold, seeded from a helper's return value rather than an expression
/// written at the call.
fn helper_seeded_fold(steps: i64) -> String {
    format!(
        r#"module SlotHelperSeed
    intent = "a map threaded through a fold, seeded by a helper"
    depends []
    effects []

fn seedPairs() -> List<Tuple<String, String>>
    ? "one pair, built where no literal map is in reach"
    [("s", "z")]

fn seed() -> Map<String, String>
    ? "hand back a map nothing else holds"
    Map.fromList(seedPairs())

fn build(n: Int, acc: Map<String, String>) -> Map<String, String>
    ? "insert once per step into a linearly threaded accumulator"
    match n > 0
        true -> build(n - 1, Map.set(acc, "k{{n}}", "v{{n}}"))
        false -> acc

fn main() -> Int
    ? "size of what the fold built"
    Map.len(build({steps}, seed()))
"#
    )
}

#[test]
fn a_helper_seeded_fold_declines_every_write_the_runtime_can_see_through() {
    // The shape the axis is for. `own_param::uniquely_owned` reads the ARGUMENT
    // written at the call site, and a user function's result is not on its list —
    // it cannot see what the callee did — so the accumulator parameter never
    // graduates and EVERY insert in the fold is declined. That is the whole of
    // the copying half of #890 in the VM, and it is the one shape where the
    // declined-but-unique tally grows with the work rather than with the source:
    // the map literal above leaves one per entry written, this leaves one per
    // step taken.
    //
    // Nothing here is out of reach of the runtime. The seed is a fresh map, it is
    // threaded linearly, and the count says so at every step.
    let small = tallies(&helper_seeded_fold(40));
    let large = tallies(&helper_seeded_fold(80));

    assert_eq!(
        (
            small.unique_slot_without_owned_grant,
            large.unique_slot_without_owned_grant
        ),
        (40, 80),
        "a helper-seeded fold should leave one declined-but-unique write per \
         step: 40 steps left {}, 80 steps left {}",
        small.unique_slot_without_owned_grant,
        large.unique_slot_without_owned_grant,
    );
    assert_eq!(
        (small.owned_grants, large.owned_grants),
        (0, 0),
        "the static mask granted something here, so the shape is no longer the \
         one whose every write it declines",
    );
}

#[test]
fn a_seed_the_static_mask_understands_leaves_the_other_direction_empty() {
    // The control for the test above. Same fold, same inserts, seeded from `{}`:
    // every insert inside the fold is granted statically, so there is nothing
    // left for a runtime decision to find and the tally is zero. Without this,
    // the number above would be evidence that the counter counts *something*,
    // not that it counts what the static analysis missed.
    let stats = tallies(&map_fold("{}"));

    assert_eq!(
        stats.unique_slot_without_owned_grant, 0,
        "an empty-literal seed left {} declined-but-unique writes, so the \
         three-entry tally is not isolating the literal's own inserts",
        stats.unique_slot_without_owned_grant,
    );
}

//! What the runtime count is allowed to decide about a `Map.set` the compiler
//! declined.
//!
//! The compiler decides ownership from a whitelist of spellings it can prove
//! nothing else reaches. Where that whitelist declines, the interpreter now asks
//! its own question — does anything at all still hold this slot? — and takes the
//! owned path when the answer is no. The whitelist keeps everything it granted;
//! nothing here removes a static grant.
//!
//! Two halves, and they need each other:
//!
//! - the WINS: a fold whose accumulator the compiler cannot follow stops
//!   duplicating the whole table once per step. Measured as entries copied,
//!   which is the number the quadratic was made of;
//! - the REFUSALS: every root set the count cannot see has to turn the grant
//!   down some other way, and each shape below is one root set. A refusal that
//!   stopped working would not fail here as a slow program — it would fail as a
//!   map somebody else still holds coming back empty. The independent-product
//!   shapes are the sixth: the interpreter used to hold the sibling branches'
//!   handles in Rust locals, where neither the count nor its mirror could find
//!   them, and that is the one root set that produced a wrong grant rather than
//!   an undercount.
//!
//! Each refusal was watched to fail against the instrument it belongs to being
//! removed. Counts are over the 76 tests of this file, `vm_slot_uniqueness`,
//! `own_param_soundness`, `own_param_graduation`, `alias_soundness` and
//! `frame_boundary_inplace_write` taken together:
//!
//! | removed                                                  | red |
//! |---|---|
//! | one holder skipped in `VM::slot_is_unheld`                | 19 — 6 here, 5 in `own_param_soundness`, 3 in `alias_soundness`, 5 tallies in `vm_slot_uniqueness` |
//! | the `held_elsewhere` test in `runtime_owns_map_target`    | 26 — all four off-stack shapes here, and 19 across the other four files |
//! | `note_held_elsewhere` at `STORE_GLOBAL`                   | 3 — the global shape here, and nothing anywhere else |
//! | the constant-table loop in `VM::new`                      | 25 — the literal seed and the rename here, and 19 elsewhere |
//! | the static mask forced on for `Map.set`                   | 33 — 11 here, and 22 across the other three files |
//! | `CALL_PAR`'s bundles popped into Rust locals again        | 2 — the sibling shapes here, and nothing anywhere else |
//!
//! In every case except the last the mirror assertion fires first and names the
//! slot, which is what it is for: the wrong answer is a map coming back empty
//! three statements later, and an assertion at the write is the only place it is
//! still legible. The last row is the exception that made it a row: the popped
//! bundles are invisible to the mirror as well as to the decision, so nothing
//! fires and the only signal is the program's answer. Putting them back on the
//! operand stack is what gives both instruments something to see.
//!
//! The static-mask row is a different kind of injection from the other five: it
//! forces a grant the compiler never makes, so it reddens the answers as well as
//! the assertions. It is here because the same mirror covers the STATIC mask's
//! off-stack half, and the record-field shape below is where that half is the
//! only thing that can say no — the extracted local is at its last use, so the
//! walk finds nothing.

use aver::ir::pipeline::{PipelineConfig, TypecheckMode};
use aver::nan_value::Arena;
use aver::vm::{self, VM, VmRuntimeOwnershipStats};

/// Compile through the real pipeline, typecheck included.
///
/// The typecheck is load-bearing rather than incidental: `ir::alias`'s
/// destination half is guarded by `slot_is_collection`, so without types every
/// binding slot stays `Type::Invalid` and the collection rules never fire —
/// which would change what the compiler's owned mask says and therefore which
/// writes reach the runtime decision at all.
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
    VM::new(code, globals, arena)
}

/// What one run produced, cost, and decided.
struct Run {
    answer: i64,
    /// Entries duplicated because a builtin had to preserve the map it was
    /// handed.
    copied: u64,
    decisions: VmRuntimeOwnershipStats,
}

fn run(src: &str) -> Run {
    let mut machine = compiled_vm(src);
    let result = machine.run().expect("program should run");
    Run {
        answer: result.as_int(&machine.arena),
        copied: machine.arena.map_entries_copied(),
        decisions: machine.runtime_ownership_stats(),
    }
}

/// Entries a run duplicated because a builtin had to preserve the map it was
/// handed.
fn entries_copied(src: &str) -> u64 {
    run(src).copied
}

/// What the program answered.
fn answer(src: &str) -> i64 {
    run(src).answer
}

/// The same run with effect recording switched on.
///
/// One shipped way of reaching `CALL_PAR`'s in-parent sequential path, and the
/// cheapest: `aver run --record` turns this on for the whole run
/// (`commands.rs`), the branch scheduler sees `is_effect_tracking` and keeps
/// every branch on the parent VM instead of giving each one a child.
fn run_recording(src: &str) -> Run {
    let mut machine = compiled_vm(src);
    machine.start_recording();
    let result = machine.run().expect("program should run");
    Run {
        answer: result.as_int(&machine.arena),
        copied: machine.arena.map_entries_copied(),
        decisions: machine.runtime_ownership_stats(),
    }
}

// ---------------------------------------------------------------------------
// The wins
// ---------------------------------------------------------------------------

/// The fold both win shapes share: `n` inserts into one accumulator threaded
/// through a tail call, seeded from `seed`.
///
/// Everything but the seed is fixed, so a difference is a statement about the
/// seed's spelling alone.
fn map_fold(steps: i64, prelude: &str, seed: &str) -> String {
    format!(
        r#"module MapRuntimeOwnershipFold
    intent = "a map threaded through a tail-recursive fold"
    depends []
    effects []
{prelude}
fn build(n: Int, acc: Map<String, String>) -> Map<String, String>
    ? "insert once per step into a linearly threaded accumulator"
    match n > 0
        true -> build(n - 1, Map.set(acc, "k{{n}}", "v{{n}}"))
        false -> acc

fn main() -> Int
    ? "size of what the fold built"
    Map.len(build({steps}, {seed}))
"#
    )
}

/// The fold from #890 itself: every insert routed through a helper whose whole
/// body is `Map.set` of its own parameter.
///
/// The reporter's `viaHelper` column, minus the timing harness. Nothing about
/// the accumulator is unusual — it is threaded linearly and read once — but the
/// insert happens one frame down, behind a parameter, and comes back as a return
/// value. `own_param` reads the argument written at the call site and a callee's
/// result is not something it can look inside, so `into` never graduates and
/// every one of the `n` inserts is declined.
fn helper_returning_set_fold(steps: i64) -> String {
    format!(
        r#"module MapRuntimeOwnershipViaHelper
    intent = "the fold from #890: every insert routed through a helper that returns a map"
    depends []
    effects []

fn setOne(key: String, into: Map<String, String>) -> Map<String, String>
    ? "return a map built out of this parameter — the only difference from the inline fold"
    Map.set(into, key, "v")

fn viaHelper(n: Int, into: Map<String, String>) -> Map<String, String>
    ? "insert once per step, through the helper"
    match n > 0
        true -> viaHelper(n - 1, setOne("k{{n}}", into))
        false -> into

fn main() -> Int
    ? "size of what the fold built"
    Map.len(viaHelper({steps}, Map.fromList([])))
"#
    )
}

#[test]
fn the_fold_that_routes_every_insert_through_a_helper_is_linear() {
    // The VM half of #890, which is the whole point of this phase. Doubling the
    // steps used to quadruple the entries duplicated — that is the quadratic the
    // report measured, 3,400x at 40,000 keys — and now there is nothing to
    // double: the map crossing a return boundary is not a second holder, and the
    // runtime can see that where the compiler could not.
    let small = run(&helper_returning_set_fold(200));
    let large = run(&helper_returning_set_fold(400));

    assert_eq!(
        (small.copied, large.copied),
        (0, 0),
        "the #890 fold still duplicates its accumulator: 200 steps copied {} \
         entries, 400 steps copied {}",
        small.copied,
        large.copied,
    );
    // The receipt. Every step's insert reaches the decision and every one is
    // granted, bar the first: `Map.fromList([])` is the empty map, an immediate
    // with no slot to take and nothing to copy, so it is never asked about.
    assert_eq!(
        (small.decisions.grants, large.decisions.grants),
        (199, 399),
        "the helper's inserts did not all reach the decision: 200 steps granted \
         {}, 400 steps granted {}",
        small.decisions.grants,
        large.decisions.grants,
    );
    assert_eq!(
        (
            large.decisions.refused_stack_holder,
            large.decisions.refused_off_stack_holder,
            large.decisions.unexamined_walk_too_costly,
        ),
        (0, 0, 0),
        "the accumulator is threaded linearly, so nothing should refuse: {:?}",
        large.decisions,
    );
    // Non-vacuity: a fold that stopped inserting would copy nothing either.
    assert_eq!((small.answer, large.answer), (200, 400));
}

/// A fold seeded by a helper's return value — the other spelling the compiler
/// cannot follow, and the one #890 does not name.
fn helper_seeded_fold(steps: i64) -> String {
    map_fold(
        steps,
        r#"
fn seedPairs() -> List<Tuple<String, String>>
    ? "one pair, built where no literal map is in reach"
    [("s", "z")]

fn seed() -> Map<String, String>
    ? "hand back a map nothing else holds"
    Map.fromList(seedPairs())
"#,
        "seed()",
    )
}

#[test]
fn a_literal_seeded_fold_stops_paying_for_the_literals_own_entries() {
    // A non-empty map literal lowers to a `LOAD_CONST` of an empty map plus one
    // PLAIN `CALL_BUILTIN` per entry, so the compiler declines every insert the
    // literal itself makes and each one duplicated what was there: three entries
    // cost 0+1+2 copies. The runtime sees straight through all three — the
    // constant the first one targets is an empty table, and the two after it are
    // fresh entries nothing else has been handed.
    let three = entries_copied(&map_fold(
        40,
        "",
        "{\"a\" => \"1\", \"b\" => \"2\", \"c\" => \"3\"}",
    ));
    let four = entries_copied(&map_fold(
        40,
        "",
        "{\"a\" => \"1\", \"b\" => \"2\", \"c\" => \"3\", \"d\" => \"4\"}",
    ));

    assert_eq!(
        (three, four),
        (0, 0),
        "a literal seed still duplicates its own entries: three entries copied \
         {three}, four copied {four}",
    );
}

#[test]
fn a_helper_seeded_fold_is_linear_in_the_steps_it_takes() {
    // The VM half of #890. `own_param` reads the argument written at the call
    // site, and a user function's result is not something it can look inside, so
    // the accumulator parameter never graduates and EVERY insert in the fold was
    // declined — one full duplication of the table per step, which is the
    // quadratic. Nothing about the shape is out of the runtime's reach: the seed
    // is a fresh map and it is threaded linearly.
    let small = run(&helper_seeded_fold(40));
    let large = run(&helper_seeded_fold(80));

    assert_eq!(
        (small.copied, large.copied),
        (0, 0),
        "a helper-seeded fold still duplicates its accumulator: 40 steps copied \
         {} entries, 80 steps copied {}",
        small.copied,
        large.copied,
    );
    // The receipt for where the copies went. One grant per step, growing with
    // the steps taken, and nothing refused: the count of writes the runtime saw
    // through is exactly the count P1 predicted it would.
    assert_eq!(
        (small.decisions.grants, large.decisions.grants),
        (40, 80),
        "the fold's inserts did not all reach the decision: 40 steps granted \
         {}, 80 steps granted {}",
        small.decisions.grants,
        large.decisions.grants,
    );
    assert_eq!(
        (
            large.decisions.refused_stack_holder,
            large.decisions.refused_off_stack_holder,
            large.decisions.unexamined_walk_too_costly,
        ),
        (0, 0, 0),
        "a linearly threaded accumulator should leave no refusal at all: {:?}",
        large.decisions,
    );
}

#[test]
fn the_fold_still_answers_what_it_answered() {
    // Non-vacuity for both wins: a fold that stopped copying because it stopped
    // inserting would pass every assertion above.
    assert_eq!(answer(&map_fold(40, "", "{\"a\" => \"1\"}")), 41);
    assert_eq!(answer(&helper_seeded_fold(40)), 41);
    assert_eq!(answer(&helper_seeded_fold(80)), 81);
}

// ---------------------------------------------------------------------------
// The refusals — one per root set the count cannot see
// ---------------------------------------------------------------------------

/// A second local binding holds the same handle.
///
/// The one shape where the operand-stack walk is the ONLY thing standing
/// between the write and a value somebody else reads afterwards: nothing has
/// left the stack, so no other root set is involved and no static flag is
/// consulted at run time. Delete the walk and this program answers 500.
const RENAMED_SOURCE: &str = r#"module MapRuntimeOwnershipRename
    intent = "a renamed map read after a write to the original"
    depends []
    effects []

fn main() -> Int
    ? "the rename is read after the write, so its cell is live across it"
    base = {1 => 7}
    snap = base
    arg = Map.set(base, 1, 500)
    Option.withDefault(Map.get(snap, 1), 0 - 1) + Map.len(arg)
"#;

/// A record field holds the handle, and the local it was extracted into is at
/// its last use.
///
/// No operand-stack cell holds the map at the write: `inner` was moved into the
/// record, `b` holds the RECORD's slot rather than the map's, and `extracted` is
/// read for the last time by the write itself. The stack walk answers "nobody",
/// and it is right — the holder is an arena entry.
const RECORD_FIELD_HOLDER: &str = r#"module MapRuntimeOwnershipRecordField
    intent = "a map extracted from a record field and written to"
    depends []
    effects []

record Box
    held: Map<Int, Int>

fn main() -> Int
    ? "read the field back after writing through what came out of it"
    inner = {8 => 110}
    b = Box(held = inner)
    extracted = b.held
    arg = Map.set(extracted, 8, 999)
    Option.withDefault(Map.get(b.held, 8), 0 - 1) + Map.len(arg)
"#;

/// A map inside another map, same shape as the record one with a different
/// container.
const MAP_VALUE_HOLDER: &str = r#"module MapRuntimeOwnershipMapValue
    intent = "a map extracted from another map's value and written to"
    depends []
    effects []

fn main() -> Int
    ? "read the outer map back after writing through what came out of it"
    inner = {8 => 110}
    holder = {0 => inner}
    extracted = Option.withDefault(Map.get(holder, 0), {9 => 9})
    arg = Map.set(extracted, 8, 999)
    reread = Option.withDefault(Map.get(holder, 0), {9 => 9})
    Option.withDefault(Map.get(reread, 8), 0 - 1) + Map.len(arg)
"#;

/// A list element holds the handle.
const LIST_ELEMENT_HOLDER: &str = r#"module MapRuntimeOwnershipListElement
    intent = "a map captured in a list and written to afterwards"
    depends []
    effects []

fn main() -> Int
    ? "read the captured element back after writing through the original"
    base = {1 => 7}
    held = [base]
    arg = Map.set(base, 1, 500)
    snap = match held
        [] -> {0 => 0}
        [h, ..t] -> h
    Option.withDefault(Map.get(snap, 1), 0 - 1) + Map.len(arg)
"#;

/// A global holds the handle.
///
/// A top-level binding lives in the globals table, which `LOAD_GLOBAL` copies
/// onto the stack and keeps. The copy is consumed by the write; the global is
/// not.
const GLOBAL_HOLDER: &str = r#"module MapRuntimeOwnershipGlobal
    intent = "a map bound at the top level and written to inside a function"
    depends []
    effects []

base = {8 => 110}

fn main() -> Int
    ? "read the global back after writing through it"
    arg = Map.set(base, 8, 999)
    Option.withDefault(Map.get(base, 8), 0 - 1) + Map.len(arg)
"#;

#[test]
fn a_write_nobody_else_can_see_is_the_only_write_that_moves_in_place() {
    // Every shape answers the value Aver's immutable model mandates plus the
    // size of what the write produced, so a refusal that turned into a grant
    // shows up as a different number rather than as a slower program.
    for (name, src, expected) in [
        ("renamed source", RENAMED_SOURCE, 7 + 1),
        ("record field", RECORD_FIELD_HOLDER, 110 + 1),
        ("map value", MAP_VALUE_HOLDER, 110 + 1),
        ("list element", LIST_ELEMENT_HOLDER, 7 + 1),
        ("global", GLOBAL_HOLDER, 110 + 1),
    ] {
        assert_eq!(
            answer(src),
            expected,
            "{name}: the write reached a map another root still holds",
        );
    }
}

#[test]
fn a_write_somebody_else_can_see_still_duplicates_what_it_was_handed() {
    // The mirror of the test above, and the reason it is not vacuous: each of
    // these writes has to COPY, so a run that copies nothing has stopped
    // refusing rather than started answering correctly by luck.
    for (name, src) in [
        ("renamed source", RENAMED_SOURCE),
        ("record field", RECORD_FIELD_HOLDER),
        ("map value", MAP_VALUE_HOLDER),
        ("list element", LIST_ELEMENT_HOLDER),
        ("global", GLOBAL_HOLDER),
    ] {
        let copied = entries_copied(src);
        assert!(
            copied > 0,
            "{name}: the write duplicated nothing, so it took the owned path \
             over a slot another root holds",
        );
    }
}

/// A one-entry map written from the bottom of a deep NON-tail recursion.
///
/// The shape the walk loses on, and the reason there is a bound at all: every
/// suspended frame's locals are cells the walk would visit, and a one-entry map
/// costs one entry to copy. `depth` frames against one entry is the worst ratio
/// a program can offer.
fn deep_stack_tiny_map(depth: i64) -> String {
    format!(
        r#"module MapRuntimeOwnershipDeepStack
    intent = "a tiny map written from under a deep non-tail recursion"
    depends []
    effects []

fn seedPairs() -> List<Tuple<String, String>>
    ? "one pair, built where no literal map is in reach"
    [("s", "z")]

fn seed() -> Map<String, String>
    ? "hand back a map nothing else holds"
    Map.fromList(seedPairs())

fn descend(n: Int, m: Map<String, String>) -> Int
    ? "recurse first, write at the bottom, so every frame is still suspended"
    match n > 0
        true -> descend(n - 1, m) + 0
        false -> Map.len(Map.set(m, "k", "v"))

fn main() -> Int
    ? "how deep the write happened"
    descend({depth}, seed())
"#
    )
}

#[test]
fn a_walk_dearer_than_the_copy_it_would_save_is_not_taken() {
    // The guard, pinned from both sides of it. At depth 4 the stack is well
    // inside the slack, so the write is examined and granted; at depth 4000 it
    // is not, and the write copies its one entry rather than walking four
    // thousand cells to find out it could have kept it. Neither answer is
    // wrong — the bound only ever costs an optimisation — so what this pins is
    // that the bound EXISTS, which is what stops a deep program paying for a
    // search proportional to how deeply it happens to be nested.
    let shallow = run(&deep_stack_tiny_map(4)).decisions;
    let deep = run(&deep_stack_tiny_map(4000)).decisions;

    assert_eq!(
        (shallow.grants, shallow.unexamined_walk_too_costly),
        (1, 0),
        "a write four frames down was not examined: {shallow:?}",
    );
    assert_eq!(
        (deep.grants, deep.unexamined_walk_too_costly),
        (0, 1),
        "a write four thousand frames down walked the stack anyway: {deep:?}",
    );
}

#[test]
fn each_refusal_is_made_by_the_instrument_that_can_see_that_holder() {
    // Which half said no matters as much as that something did. The rename is
    // the shape where the operand-stack walk is on its own: nothing has left the
    // stack, so `held_elsewhere` is false and a build without the walk would
    // grant it. The other three put the map inside an arena entry or a global,
    // where the walk finds nothing and the flag is the whole answer.
    //
    // The rename's run makes two declined writes, not one, and the second is
    // worth having in the same assertion: `{1 => 7}` lowers to a `LOAD_CONST`
    // of an empty map plus an insert, and the constant is a holder for as long
    // as the program lives. So one refusal comes from the walk and one from the
    // flag, in the same three lines of source.
    let renamed = run(RENAMED_SOURCE).decisions;
    assert_eq!(
        renamed,
        VmRuntimeOwnershipStats {
            grants: 0,
            refused_stack_holder: 1,
            refused_off_stack_holder: 1,
            unexamined_walk_too_costly: 0,
        },
        "the rename's write was not refused by the stack walk, or the literal's \
         constant stopped being treated as a holder",
    );

    for (name, src) in [
        ("record field", RECORD_FIELD_HOLDER),
        ("map value", MAP_VALUE_HOLDER),
        ("list element", LIST_ELEMENT_HOLDER),
        ("global", GLOBAL_HOLDER),
    ] {
        let stats = run(src).decisions;
        assert_eq!(
            stats.grants, 0,
            "{name}: a write was granted over a slot something off the stack \
             holds: {stats:?}",
        );
        assert!(
            stats.refused_off_stack_holder > 0,
            "{name}: nothing was refused for holding the slot off the stack, so \
             the shape stopped exercising the flag: {stats:?}",
        );
    }
}

/// The same map handed to both branches of an independent product.
///
/// The branch bundles are popped off the operand stack before the first branch
/// runs, so while branch 0 writes through its copy of the handle, branch 1's
/// copy of the SAME handle is in flight — a reference no walk of a truncated
/// stack could find. `m`'s own local cell is gone too: `peek(m)` is the
/// textually-last read, so `MOVE_LOCAL` writes `UNIT` back over it before
/// `CALL_PAR` ever runs. Every root set the decision knows about therefore says
/// "nobody", and the answer turns on whether the in-flight bundles are one.
const PARALLEL_BRANCH_SIBLING: &str = r#"module MapRuntimeOwnershipParallelSibling
    intent = "a map handed to two branches of an independent product"
    depends []
    effects []

fn seedPairs() -> List<Tuple<String, String>>
    ? "two pairs, built where no literal map is in reach"
    [("a", "1"), ("b", "2")]

fn seed() -> Map<String, String>
    ? "hand back a map nothing else holds"
    Map.fromList(seedPairs())

fn setOne(into: Map<String, String>) -> Map<String, String>
    ? "write through what this branch was handed"
    Map.set(into, "x", "1")

fn peek(m: Map<String, String>) -> Int
    ? "read the map the sibling branch is writing through"
    Map.len(m)

fn main() -> Int
    ? "what the second branch sees after the first one wrote"
    m = seed()
    both = (setOne(m), peek(m))!
    match both
        (p, q) -> Map.len(p) + q
"#;

/// The same shape with the reader FIRST, so the branch that writes runs last.
///
/// The other side of the same coin: by the time `setOne` runs, `peek` has
/// finished with its handle and nothing anywhere is left to read the map, so the
/// write is the LAST use of it and taking it in place is exactly right. A fix
/// that refused every write inside a product would answer this one correctly by
/// giving up, which is why it is here.
const PARALLEL_BRANCH_SIBLING_REVERSED: &str = r#"module MapRuntimeOwnershipParallelSiblingReversed
    intent = "a map read by the first branch of an independent product and written by the second"
    depends []
    effects []

fn seedPairs() -> List<Tuple<String, String>>
    ? "two pairs, built where no literal map is in reach"
    [("a", "1"), ("b", "2")]

fn seed() -> Map<String, String>
    ? "hand back a map nothing else holds"
    Map.fromList(seedPairs())

fn setOne(into: Map<String, String>) -> Map<String, String>
    ? "write through what this branch was handed"
    Map.set(into, "x", "1")

fn peek(m: Map<String, String>) -> Int
    ? "read the map the sibling branch is writing through"
    Map.len(m)

fn main() -> Int
    ? "what the reader saw, plus the size of what the writer produced"
    m = seed()
    both = (peek(m), setOne(m))!
    match both
        (q, p) -> Map.len(p) + q
"#;

/// The map one level down, inside a record the branches are handed.
///
/// The bundle cell holds the RECORD, not the map, so covering the popped values
/// by marking them one for one would miss this unless the arena's own holder
/// rule covers the rest — which it does, and this is what says so.
const PARALLEL_BRANCH_NESTED: &str = r#"module MapRuntimeOwnershipParallelNested
    intent = "a map inside a record handed to two branches of an independent product"
    depends []
    effects []

record Box
    held: Map<String, String>

fn seedPairs() -> List<Tuple<String, String>>
    ? "two pairs, built where no literal map is in reach"
    [("a", "1"), ("b", "2")]

fn setOne(b: Box) -> Int
    ? "write through the field this branch was handed"
    Map.len(Map.set(b.held, "x", "1"))

fn peek(b: Box) -> Int
    ? "read the field the sibling branch is writing through"
    Map.len(b.held)

fn main() -> Int
    ? "what the second branch sees after the first one wrote"
    box = Box(held = Map.fromList(seedPairs()))
    both = (setOne(box), peek(box))!
    match both
        (p, q) -> p + q
"#;

#[test]
fn a_branch_of_an_independent_product_cannot_take_a_map_its_sibling_holds() {
    // The fifth root set: `CALL_PAR` pops every branch's callable and arguments
    // into Rust locals and truncates the stack, so between the pop and the join
    // the sibling bundles are references nothing the decision looks at can see.
    // Both instruments were blind together here — the walk found no cell and
    // `held_elsewhere` had never been told — so the grant went through and the
    // second branch read a table that had been emptied out from under it.
    //
    // Each program answers the value Aver's immutable model mandates: the
    // writer's result is three entries and the reader's map is still the two it
    // was handed. A grant that came back shows up as a different number.
    // The two shapes where a sibling is still holding the map at the write:
    // its bundle has not been dispatched yet, so refusing is the only right
    // answer and the write has to duplicate what it was handed.
    for (name, src) in [
        ("writer first", PARALLEL_BRANCH_SIBLING),
        ("nested in a record", PARALLEL_BRANCH_NESTED),
    ] {
        let recorded = run_recording(src);
        assert_eq!(
            recorded.answer,
            3 + 2,
            "{name}: a branch took a map in place that a sibling branch still \
             held: {:?}",
            recorded.decisions,
        );
        assert_eq!(
            recorded.decisions.grants, 0,
            "{name}: the write over a slot an in-flight branch bundle holds was \
             granted: {:?}",
            recorded.decisions,
        );
        assert!(
            recorded.copied > 0,
            "{name}: the write duplicated nothing, so it took the owned path \
             over a slot a sibling branch holds",
        );
    }

    // The writer running LAST is the same program with nothing left to hold the
    // map, so the answer is the same and the write is granted. The fence is a
    // record of which handles are still in flight, not a blanket refusal inside
    // a product — a fix that turned every branch write into a copy would pass
    // the loop above and fail here.
    let last = run_recording(PARALLEL_BRANCH_SIBLING_REVERSED);
    assert_eq!(
        last.answer,
        3 + 2,
        "reader first: the product stopped answering: {:?}",
        last.decisions,
    );
    assert_eq!(
        last.decisions.grants, 1,
        "reader first: the last branch's write was refused although its sibling \
         had already given the handle back: {:?}",
        last.decisions,
    );
}

#[test]
fn the_products_own_branches_are_what_the_walk_now_sees() {
    // Which instrument said no, at the shape it was added for. The bundles are
    // put back on the operand stack for the length of the product, so it is the
    // WALK that refuses — `held_elsewhere` is false throughout, because nothing
    // outside the stack ever held this map. A build that popped the bundles into
    // Rust locals again would score zero here and grant instead.
    let stats = run_recording(PARALLEL_BRANCH_SIBLING).decisions;
    assert_eq!(
        (stats.grants, stats.refused_stack_holder),
        (0, 1),
        "the branch write was not refused by the stack walk: {stats:?}",
    );
}

#[test]
fn a_product_whose_branches_share_nothing_still_takes_its_writes_in_place() {
    // The other direction, and what stops the fix from being "refuse inside a
    // product". Each branch here builds and writes its own map, so no bundle
    // holds anybody else's slot and both writes are granted exactly as they
    // would be outside a product.
    let src = r#"module MapRuntimeOwnershipParallelDisjoint
    intent = "two branches of an independent product, each with a map of its own"
    depends []
    effects []

fn seedPairs(a: String) -> List<Tuple<String, String>>
    ? "one pair, built where no literal map is in reach"
    [(a, "1")]

fn grow(a: String) -> Int
    ? "build a map nothing else holds, then write through it"
    Map.len(Map.set(Map.fromList(seedPairs(a)), "x", "1"))

fn main() -> Int
    ? "both halves, each of which owns what it wrote"
    both = (grow("a"), grow("b"))!
    match both
        (p, q) -> p + q
"#;

    let recorded = run_recording(src);
    assert_eq!(
        recorded.answer,
        2 + 2,
        "the disjoint product stopped answering: {:?}",
        recorded.decisions,
    );
    assert_eq!(
        recorded.decisions.grants, 2,
        "a branch that owns its own map outright was refused: {:?}",
        recorded.decisions,
    );
}

// ---------------------------------------------------------------------------
// What the mirror actually covered
// ---------------------------------------------------------------------------

#[test]
fn no_run_in_the_corpus_outspends_the_mirrors_budget() {
    // The mirror's arena half is budgeted, and a spent budget is the difference
    // between "every grant was mirrored against all four root sets" and "every
    // grant until the budget ran out was". The skip is explicit and counted
    // rather than answered as "nobody holds it", so this can be asked: run the
    // heaviest shapes this file has — the two folds and the deep-stack probe,
    // all on one thread, which is the unit the budget is per — and read the
    // tally back.
    //
    // It is a claim about THIS corpus, not about every program: a long run is
    // meant to stop paying for the audit. What it pins is that nothing here is
    // green because the mirror stopped looking.
    let before = vm::grants_the_mirror_could_not_afford();
    let _ = run(&helper_returning_set_fold(200));
    let _ = run(&helper_seeded_fold(200));
    let _ = run(&map_fold(200, "", "{\"a\" => \"1\"}"));
    let _ = run(&deep_stack_tiny_map(4));
    let _ = run_recording(PARALLEL_BRANCH_SIBLING);
    let after = vm::grants_the_mirror_could_not_afford();

    assert_eq!(
        (before, after),
        (0, 0),
        "the mirror skipped its arena half for some grant in this file, so the \
         corpus is only checked against three root sets from that write on",
    );
}

// ---------------------------------------------------------------------------
// The way out
// ---------------------------------------------------------------------------

#[test]
fn the_environment_switch_puts_every_declined_write_back_on_the_copying_path() {
    // The transition's way back, and it has to be exercised through a real
    // process: the switch is read once per run, so a test in this one would be
    // asking a question that was already answered. `--profile` is where the
    // decision's own numbers are printed, so the same run says both what it
    // decided and that it decided anything at all.
    let dir = std::env::temp_dir().join("aver_map_runtime_ownership_switch");
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("create temp dir");
    let file = dir.join("main.av");
    std::fs::write(&file, helper_seeded_fold(40)).expect("write source");

    let grants_line = |disabled: bool| {
        let mut cmd = std::process::Command::new(env!("CARGO_BIN_EXE_aver"));
        cmd.arg("run").arg("--profile").arg(&file);
        if disabled {
            cmd.env("AVER_NO_RUNTIME_MAP_OWNERSHIP", "1");
        }
        let out = cmd.output().expect("spawn aver run");
        assert!(
            out.status.success(),
            "`aver run --profile` failed: {}",
            String::from_utf8_lossy(&out.stderr)
        );
        String::from_utf8_lossy(&out.stderr)
            .lines()
            .find(|line| line.contains("taken in place:"))
            .map(str::trim)
            .map(str::to_owned)
            .expect("the profile should report what the runtime decided")
    };

    let on = grants_line(false);
    let off = grants_line(true);
    let _ = std::fs::remove_dir_all(&dir);

    assert_eq!(
        on, "taken in place:40 refused, a stack cell holds it:0",
        "the fold's forty inserts were not all taken in place by default",
    );
    assert_eq!(
        off, "taken in place:0 refused, a stack cell holds it:0",
        "the switch did not put the fold back on the copying path",
    );
}

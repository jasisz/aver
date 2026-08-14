//! Live references to an arena slot, and the directional cross-check against
//! the compiler's static owned mask.
//!
//! Locals are a window on the operand stack, so "how many live references does
//! this arena slot have?" is answered by counting the operand-stack cells that
//! hold its index. The tests below pin the two things that make it worth having:
//!
//! - every path a cell can leave the stack by really does give it back —
//!   ordinary return, error propagation, the frameless-leaf return of #917,
//!   tail-call window reuse, independent-product branches — so nothing is still
//!   spoken for once a run is over;
//! - the directional comparison against the owned mask holds in the soundness
//!   direction (`granted ⊆ runtime-unique`) while the OTHER direction is
//!   non-empty, which is the point of the exercise: the runtime sees slots as
//!   uniquely held that the static analysis declined.
//!
//! Nothing here decides anything. The count is observed, not consulted.

use aver::ir::pipeline::{PipelineConfig, TypecheckMode};
use aver::nan_value::Arena;
use aver::vm::{self, VM};

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
    VM::new(code, globals, arena)
}

/// Run to completion and report what the operand stack still holds.
///
/// A finished run has an empty operand stack, so a non-zero total is a cell an
/// exit path left standing — the whole-program form of "the count returns to
/// zero at frame exit", asked once for every exit path the program went through
/// rather than once per path.
fn run_and_report_live_refs(src: &str) -> u64 {
    let mut machine = compiled_vm(src);
    machine.run().expect("program should run");
    machine.live_slot_refs()
}

// ---------------------------------------------------------------------------
// The count returns to zero, on every exit path
// ---------------------------------------------------------------------------

#[test]
fn an_ordinary_return_gives_back_every_reference_it_took() {
    let live = run_and_report_live_refs(
        r#"module SlotOrdinaryReturn
    intent = "ordinary returns hand back every slot reference they took"
    depends []
    effects []

fn wrap(name: String) -> List<String>
    ? "build a list the caller keeps"
    [name, String.toUpper(name)]

fn main() -> Int
    ? "count what a helper handed back"
    List.len(wrap("slot"))
"#,
    );
    assert_eq!(
        live, 0,
        "an ordinary return left {live} slot reference(s) spoken for after the \
         stack had emptied",
    );
}

#[test]
fn error_propagation_out_of_an_argument_gives_back_every_reference() {
    // `?` inside an argument leaves through `PROPAGATE_ERR`, and from a body
    // that binds no name and calls nothing the user wrote it leaves through the
    // FRAMELESS return inside it — the two exits #917 taught to consult
    // `leaf_return`. Both truncate the stack themselves rather than going
    // through `RETURN`, so both owe the count the same discharge.
    let live = run_and_report_live_refs(
        r#"module SlotErrorExit
    intent = "the error exits hand back every slot reference they took"
    depends []
    effects []

fn parse(text: String) -> Result<String, String>
    ? "fail on anything but the one word"
    match text == "ok"
        true -> Result.Ok(String.toUpper(text))
        false -> Result.Err("bad input: {text}")

fn relay(text: String) -> Result<String, String>
    ? "a frameless body whose only call can fail"
    Result.Ok(parse(text)?)

fn describe(text: String) -> String
    ? "swallow the error so the run finishes"
    match relay(text)
        Result.Ok(word) -> word
        Result.Err(why) -> why

fn main() -> Int
    ? "take both exits in one run"
    String.len(describe("ok")) + String.len(describe("no"))
"#,
    );
    assert_eq!(
        live, 0,
        "an error exit left {live} slot reference(s) spoken for after the stack \
         had emptied",
    );
}

#[test]
fn tail_call_window_reuse_gives_back_every_reference() {
    // The tail call reuses the frame's window: the boundary finalizer compacts
    // what the frame owns and hands the arguments back at their new indices,
    // then the window is rewritten from those. Nothing of the old iteration may
    // still be standing on the stack afterwards, or the loop would accumulate
    // holders across iterations and every collection write inside one would read
    // as shared.
    let live = run_and_report_live_refs(
        r#"module SlotTailCall
    intent = "a reused tail-call window hands back every slot reference"
    depends []
    effects []

fn build(n: Int, acc: List<String>) -> List<String>
    ? "loop-carried heap survivors across a reused frame"
    match n > 0
        true -> build(n - 1, List.prepend(String.toUpper("item-{n}"), acc))
        false -> acc

fn main() -> Int
    ? "length of what the loop carried"
    List.len(build(40, []))
"#,
    );
    assert_eq!(
        live, 0,
        "a reused tail-call window left {live} slot reference(s) spoken for \
         after the stack had emptied",
    );
}

#[test]
fn independent_product_branches_give_back_every_reference() {
    // `(a, b)!` runs its branches through child VMs with arenas of their own and
    // deep-imports the results back, so the parent's count must be about the
    // parent's slots and nothing else.
    let live = run_and_report_live_refs(
        r#"module SlotIndependentProduct
    intent = "independent-product branches hand back every slot reference"
    depends []
    effects []

fn shout(word: String) -> String
    ? "a branch that allocates"
    String.toUpper("{word}-{word}")

fn main() -> Int
    ? "join two branches and measure"
    pair = (shout("left"), shout("right"))!
    match pair
        (left, right) -> String.len(left) + String.len(right)
"#,
    );
    assert_eq!(
        live, 0,
        "an independent product left {live} slot reference(s) spoken for after \
         the stack had emptied",
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
    let mut machine = compiled_vm(&map_fold("{}"));
    machine.run().expect("map fold should run");
    let stats = machine.slot_uniqueness_stats();

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
    let mut machine = compiled_vm(&map_fold(&format!("{{{spelled}}}")));
    machine.run().expect("map fold should run");
    let stats = machine.slot_uniqueness_stats();
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

#[test]
fn a_seed_the_static_mask_understands_leaves_the_other_direction_empty() {
    // The control for the test above. Same fold, same inserts, seeded from `{}`:
    // every insert inside the fold is granted statically, so there is nothing
    // left for a runtime decision to find and the tally is zero. Without this,
    // the number above would be evidence that the counter counts *something*,
    // not that it counts what the static analysis missed.
    let mut machine = compiled_vm(&map_fold("{}"));
    machine.run().expect("map fold should run");
    let stats = machine.slot_uniqueness_stats();

    assert_eq!(
        stats.unique_slot_without_owned_grant, 0,
        "an empty-literal seed left {} declined-but-unique writes, so the \
         three-entry tally is not isolating the literal's own inserts",
        stats.unique_slot_without_owned_grant,
    );
}

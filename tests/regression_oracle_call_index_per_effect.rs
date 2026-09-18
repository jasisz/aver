//! Regression — the call index a `given` stub receives counts the calls of ITS
//! operation, not every effect the function under test reaches.
//!
//! A scripted peer answers by call number: read 0 is 7, read 1 is 9. Under the
//! old shared counter a clock read between the two peer reads pushed the second
//! read to index 2, so the same stub answered 0 and the packed result dropped
//! from 7009 to 7000. Adding a log line or a clock read to the code under test
//! silently rewrote what every stub of every other operation saw.
//!
//! The runtime half and the proof-export half are pinned together in this one
//! file on purpose. The lifter writes the index into the emitted Lean and Dafny
//! as a literal, so the two interpreters only agree while both use the same
//! numbering. `tests/regression_oracle_counter_order.rs` pins the other
//! invariant these two halves share: arguments are charged before the call that
//! surrounds them.

use std::path::PathBuf;
use std::process::Command;

use aver::diagnostics::vm_verify::run_verify_for_items_vm_with_mode;
use aver::source::parse_source;
use aver::verify_law::expand::ExpansionMode;

/// One function reaching two operations, with a scripted stub for each. The
/// peer answers by call number and the clock is frozen, so the value the law
/// asserts is exactly the numbering the peer was handed.
const MIXED: &str = r#"module MixedCallIndex
    intent = "Each operation numbers its own calls from zero."
    exposes [twoReadsWithClock]
    effects [Random.int, Time.unixMs]

fn twoReadsWithClock() -> Int
    ? "Two peer reads with one clock read between them."
    ! [Random.int, Time.unixMs]
    a = Random.int(0, 100)
    _now = Time.unixMs()
    b = Random.int(0, 100)
    a * 1000 + b

fn peer(path: BranchPath, call: Int, low: Int, high: Int) -> Result<Int, String>
    ? "A scripted peer: says 7 on its first call and 9 on its second."
    match call
        0 -> Result.Ok(7)
        1 -> Result.Ok(9)
        _ -> Result.Ok(0)

fn clock(path: BranchPath, call: Int) -> Int
    ? "A frozen clock, so only the peer's numbering is observable."
    1000

verify twoReadsWithClock law scriptedPeer
    given rnd: Random.int = [peer]
    given now: Time.unixMs = [clock]
    twoReadsWithClock() => 7009
"#;

/// One `match` whose arms both read the same operation, followed by a third
/// read. Exactly one arm runs, so the run charges the operation twice: index 0
/// inside the arm that was taken and index 1 after the match.
const ARMS: &str = r#"module ArmCallIndex
    intent = "Only the arm that runs is charged."
    exposes [pick]
    effects [Random.int]

fn pick(flag: Int) -> Int
    ? "Both arms read the peer, and one more read follows the match."
    ! [Random.int]
    chosen = match flag
        0 -> Random.int(1, 6)
        _ -> Random.int(1, 6)
    after = Random.int(1, 6)
    chosen * 10 + after

fn peer(path: BranchPath, call: Int, low: Int, high: Int) -> Result<Int, String>
    ? "The peer reports the index it was handed."
    Result.Ok(call)

verify pick law armsAreNumberedFromTheMatch
    given rnd: Random.int = [peer]
    pick(1) => 1
"#;

#[test]
fn vm_numbers_each_operation_from_zero() {
    let items = parse_source(MIXED).unwrap_or_else(|e| panic!("parse failed: {e:?}"));
    let results = run_verify_for_items_vm_with_mode(
        items,
        None,
        Some(env!("CARGO_MANIFEST_DIR")),
        "regression_oracle_call_index_per_effect.av",
        ExpansionMode::Declared,
    )
    .expect("verify run");

    assert_eq!(results.len(), 1, "one verify block");
    let result = &results[0];
    assert_eq!(
        (result.passed, result.failed),
        (1, 0),
        "the second Random.int must reach the peer as call 1 even though Time.unixMs ran \
         between the two reads — a reported actual of 7000 means the clock consumed the \
         peer's index and src/vm/runtime.rs take_oracle_coordinates is back to one counter \
         per case"
    );
}

/// The emitted file, and what `aver proof` told the user while emitting it.
struct Export {
    file: String,
    report: String,
}

fn export(program: &str, slug: &str, backend: &str, file: &str) -> Export {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("target")
        .join("test-out")
        .join(format!("oracle-call-index-{slug}-{backend}"));
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(&root).expect("create output dir");
    let source = root.join(format!("{slug}.av"));
    std::fs::write(&source, program).expect("write source");

    let output = Command::new(aver_bin)
        .arg("proof")
        .arg(&source)
        .arg("--backend")
        .arg(backend)
        .arg("-o")
        .arg(&root)
        .output()
        .expect("run aver proof");
    assert!(
        output.status.success(),
        "aver proof --backend {backend} failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    Export {
        file: std::fs::read_to_string(root.join(file))
            .unwrap_or_else(|e| panic!("read {file}: {e}")),
        report: format!(
            "{}{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        ),
    }
}

/// Run the program's `verify` blocks and return how many cases passed and failed.
fn verify(program: &str, name: &str) -> (usize, usize) {
    let items = parse_source(program).unwrap_or_else(|e| panic!("parse failed: {e:?}"));
    let results = run_verify_for_items_vm_with_mode(
        items,
        None,
        Some(env!("CARGO_MANIFEST_DIR")),
        name,
        ExpansionMode::Declared,
    )
    .expect("verify run");
    assert_eq!(results.len(), 1, "one verify block");
    (results[0].passed, results[0].failed)
}

/// Assert that the law is declined rather than exported, with a reason naming
/// `operation` that the user can read, and that no theorem carries its name.
fn assert_law_declined(export: &Export, fn_name: &str, law_name: &str, operation: &str) {
    assert!(
        export.report.contains("declined"),
        "`aver proof` must tell the user the law was not exported; report was:\n{}",
        export.report
    );
    assert!(
        export.report.contains(&format!("{fn_name}.{law_name}")),
        "the declined claim must be named so the user knows which law lost its theorem; \
         report was:\n{}",
        export.report
    );
    assert!(
        export.report.contains(operation),
        "the reason must name the operation whose index cannot be written down; report \
         was:\n{}",
        export.report
    );
    assert!(
        export.file.contains("is not exported"),
        "the emitted file must carry the refusal where a reader of the proof will see it:\n{}",
        export.file
    );
    let theorem = format!("theorem {fn_name}_law_{law_name}");
    assert!(
        !export.file.contains(&theorem),
        "`{theorem}` was emitted for a law whose model numbers a stub differently from the \
         run — that theorem certifies a statement about a function the run never \
         computes:\n{}",
        export.file
    );
}

#[test]
fn exported_lean_numbers_each_operation_from_zero() {
    let lean = export(MIXED, "mixed", "lean", "MixedCallIndex.lean").file;
    for call in [
        "rnd_Random_int path 0 0 100",
        "rnd_Time_unixMs path 0",
        "rnd_Random_int path 1 0 100",
    ] {
        assert!(
            lean.contains(call),
            "the lifted proof must charge each operation its own index, so the clock is \
             index 0 and the second read index 1; `{call}` is missing from:\n{lean}"
        );
    }
    assert!(
        !lean.contains("rnd_Random_int path 2 0 100"),
        "index 2 means the lifter still shares one counter across operations:\n{lean}"
    );
}

#[test]
fn exported_dafny_numbers_each_operation_from_zero() {
    let dafny = export(MIXED, "mixed", "dafny", "MixedCallIndex.dfy").file;
    for call in [
        "rnd_Random_int(path, 0, 0, 100)",
        "rnd_Time_unixMs(path, 0)",
        "rnd_Random_int(path, 1, 0, 100)",
    ] {
        assert!(
            dafny.contains(call),
            "the Dafny export must carry the same numbering as the Lean export and the VM; \
             `{call}` is missing from:\n{dafny}"
        );
    }
    assert!(
        !dafny.contains("rnd_Random_int(path, 2, 0, 100)"),
        "index 2 means the lifter still shares one counter across operations:\n{dafny}"
    );
}

#[test]
fn vm_charges_only_the_arm_that_runs() {
    let items = parse_source(ARMS).unwrap_or_else(|e| panic!("parse failed: {e:?}"));
    let results = run_verify_for_items_vm_with_mode(
        items,
        None,
        Some(env!("CARGO_MANIFEST_DIR")),
        "regression_oracle_call_index_arms.av",
        ExpansionMode::Declared,
    )
    .expect("verify run");

    assert_eq!(results.len(), 1, "one verify block");
    let result = &results[0];
    assert_eq!(
        (result.passed, result.failed),
        (1, 0),
        "the arm that runs reads the peer at index 0 and the read after the match at index 1, \
         because the arm that was not taken made no call at all"
    );
}

#[test]
fn exported_lean_numbers_match_arms_from_the_match() {
    let lean = export(ARMS, "arms", "lean", "ArmCallIndex.lean").file;
    let taken = lean.matches("rnd_Random_int path 0 1 6").count();
    assert_eq!(
        taken, 2,
        "each arm starts from the index the match was reached at, so both arms read index 0; \
         one occurrence means the lifter still carries the first arm's call into the second \
         and the exported law is false wherever the second arm runs:\n{lean}"
    );
    assert!(
        lean.contains("rnd_Random_int path 1 1 6"),
        "the read after the match is index 1 for whichever arm ran, because both arms make one \
         call:\n{lean}"
    );
    assert!(
        !lean.contains("rnd_Random_int path 2 1 6"),
        "index 2 means the arms were summed instead of numbered from the match:\n{lean}"
    );
}

// ---------------------------------------------------------------------------
// The four shapes where one static index cannot follow the run, and the
// export declines the law instead of writing down a number it cannot justify.
//
// Each program below passes `aver verify`: the VM hands the stub exactly the
// indices the law's value depends on. Each one also lifts to a model that
// charges different indices. A law is checked on samples and proved for every
// input, so leaving these exportable lets a law whose samples happen to agree
// certify a statement about a function the run does not compute — a false
// certificate, arrived at with every step passing. `docs/oracle.md` carries
// the same four shapes in prose.
// ---------------------------------------------------------------------------

/// A helper call. `outer` reads the peer and then lets `inner` read it again;
/// the run charges the two reads 0 and 1, and `inner`'s lifted body starts
/// again at 0.
const HELPER: &str = r#"module HelperRestart
    intent = "A read in the caller and a read in the helper are two calls of one operation."
    exposes [outer]
    effects [Random.int]

fn outer() -> Int
    ? "Read the peer once, then let the helper read it again."
    ! [Random.int]
    a = Random.int(0, 100)
    b = inner()
    a * 1000 + b

fn inner() -> Int
    ? "Read the peer once."
    ! [Random.int]
    Random.int(0, 100)

fn peer(path: BranchPath, call: Int, low: Int, high: Int) -> Result<Int, String>
    ? "A scripted peer: 7 on its first call, 9 on its second."
    match call
        0 -> Result.Ok(7)
        1 -> Result.Ok(9)
        _ -> Result.Ok(0)

verify outer law helperKeepsCounting
    given rnd: Random.int = [peer]
    outer() => 7009
"#;

/// Recursion that carries no index. Each turn reads the peer once and the run
/// numbers the turns 0, 1, 2; the lifted body emits one literal for them all.
const DRAIN: &str = r#"module DrainRecursion
    intent = "Each turn of a loop reads the peer once, and the reads are numbered across turns."
    exposes [drain]
    effects [Random.int]

fn drain(left: Int, total: Int) -> Int
    ? "Read the peer once per turn until the counter runs out."
    ! [Random.int]
    match left
        0 -> total
        _ -> drain(left - 1, total + Random.int(0, 100))

fn peerByCall(path: BranchPath, call: Int, low: Int, high: Int) -> Result<Int, String>
    ? "The peer reports the call index it was handed."
    Result.Ok(call)

verify drain law turnsAreNumbered
    given rnd: Random.int = [peerByCall]
    drain(3, 0) => 3
"#;

/// A second operation inside a polled loop. The function threads the polling
/// base, which counts polls; the clock is read twice per poll, so the two
/// rates part company on the second turn.
const THREADED: &str = r#"module ThreadedTwoRates
    intent = "Two clock reads per poll, so the clock advances twice as fast as the poll."
    exposes [follow]
    effects [Process.stopRequested, Time.unixMs]

fn follow(steps: Int) -> Int
    ? "Read the clock twice, then poll, then continue."
    ! [Time.unixMs, Process.stopRequested]
    a = Time.unixMs()
    b = Time.unixMs()
    match Process.stopRequested()
        true -> steps + a + b
        false -> follow(steps + 1)

fn clockByCall(path: BranchPath, call: Int) -> Int
    ? "The clock reports the call index it was handed."
    call

fn stopAfterTwo(path: BranchPath, call: Int) -> Bool
    ? "The third poll asks this branch to stop."
    call >= 2

verify follow law twoRates
    given now: Time.unixMs = [clockByCall]
    given stop: Process.stopRequested = [stopAfterTwo]
    follow(0) => 11
"#;

/// A `match` whose arms read the peer a different number of times, with a read
/// after it. The run charges the arm it took; the export numbers the following
/// read from the busiest arm, so the two agree only on that one arm.
const UNEVEN_ARMS: &str = r#"module UnevenArms
    intent = "One arm reads the peer twice and the other once, and a read follows the match."
    exposes [pickUneven]
    effects [Random.int]

fn pickUneven(flag: Int) -> Int
    ? "The arms charge the peer differently, so the read after the match has no fixed index."
    ! [Random.int]
    chosen = match flag
        0 -> Random.int(1, 6) + Random.int(1, 6)
        _ -> Random.int(1, 6)
    after = Random.int(1, 6)
    chosen * 10 + after

fn peerByCall(path: BranchPath, call: Int, low: Int, high: Int) -> Result<Int, String>
    ? "The peer reports the call index it was handed."
    Result.Ok(call)

verify pickUneven law armsChargeDifferently
    given rnd: Random.int = [peerByCall]
    pickUneven(1) => 1
"#;

#[test]
fn a_law_over_a_function_that_calls_an_effectful_helper_is_declined() {
    assert_eq!(
        verify(HELPER, "oracle_call_index_helper.av"),
        (1, 0),
        "the run hands `inner`'s read index 1, so the packed answer is 7009"
    );
    let lean = export(HELPER, "helper", "lean", "HelperRestart.lean");
    assert!(
        lean.file.contains("rnd_Random_int path 0 0 100"),
        "the lifted helper still starts at index 0 — that is the divergence, and the law \
         is declined rather than the numbering being faked:\n{}",
        lean.file
    );
    assert_law_declined(&lean, "outer", "helperKeepsCounting", "Random.int");
    let dafny = export(HELPER, "helper", "dafny", "HelperRestart.dfy");
    assert_law_declined(&dafny, "outer", "helperKeepsCounting", "Random.int");
}

#[test]
fn a_law_over_a_recursive_effectful_function_is_declined() {
    assert_eq!(
        verify(DRAIN, "oracle_call_index_drain.av"),
        (1, 0),
        "the run numbers the three turns 0, 1 and 2, so the total is 3"
    );
    let lean = export(DRAIN, "drain", "lean", "DrainRecursion.lean");
    assert_law_declined(&lean, "drain", "turnsAreNumbered", "Random.int");
    let dafny = export(DRAIN, "drain", "dafny", "DrainRecursion.dfy");
    assert_law_declined(&dafny, "drain", "turnsAreNumbered", "Random.int");
}

#[test]
fn a_law_over_a_second_operation_in_a_polled_loop_is_declined() {
    assert_eq!(
        verify(THREADED, "oracle_call_index_threaded.av"),
        (1, 0),
        "the run reads the clock at 0..5 over three turns, so the answer is 2 + 4 + 5"
    );
    let lean = export(THREADED, "threaded", "lean", "ThreadedTwoRates.lean");
    assert!(
        lean.report.contains("Process.stopRequested"),
        "the reason must say that the base carried into the call counts polls, which is \
         what makes the clock's index wrong; report was:\n{}",
        lean.report
    );
    assert_law_declined(&lean, "follow", "twoRates", "Time.unixMs");
    let dafny = export(THREADED, "threaded", "dafny", "ThreadedTwoRates.dfy");
    assert_law_declined(&dafny, "follow", "twoRates", "Time.unixMs");
}

#[test]
fn a_law_over_a_call_after_uneven_match_arms_is_declined() {
    assert_eq!(
        verify(UNEVEN_ARMS, "oracle_call_index_uneven_arms.av"),
        (1, 0),
        "the arm that runs reads index 0 and the read after the match reads index 1"
    );
    let lean = export(UNEVEN_ARMS, "uneven", "lean", "UnevenArms.lean");
    assert_law_declined(&lean, "pickUneven", "armsChargeDifferently", "Random.int");
    let dafny = export(UNEVEN_ARMS, "uneven", "dafny", "UnevenArms.dfy");
    assert_law_declined(&dafny, "pickUneven", "armsChargeDifferently", "Random.int");
}

/// The other side of the gate: a `Process.stopRequested` loop is the one shape
/// recursion is exact for, because the polling base is threaded into the
/// recursive call. Declining it too would cost the shipped cooperative-shutdown
/// example its theorem for no soundness gain.
#[test]
fn a_law_over_a_single_operation_polling_loop_still_exports() {
    const POLL: &str = r#"module PollOnly
    intent = "Polling alone carries its own index into the next turn."
    exposes [follow]
    effects [Process.stopRequested]

fn follow(steps: Int) -> Int
    ? "Continue until the process asks this branch to stop."
    ! [Process.stopRequested]
    match Process.stopRequested()
        true -> steps
        false -> follow(steps + 1)

fn stopAfterThree(path: BranchPath, call: Int) -> Bool
    ? "The fourth poll asks this branch to stop."
    call >= 3

verify follow law pollsAreCounted
    given stop: Process.stopRequested = [stopAfterThree]
    follow(0) => 3
"#;
    assert_eq!(
        verify(POLL, "oracle_call_index_poll_only.av"),
        (1, 0),
        "the run polls four times and stops on the fourth"
    );
    let lean = export(POLL, "pollonly", "lean", "PollOnly.lean");
    assert!(
        lean.file.contains("theorem follow_law_pollsAreCounted"),
        "the polling base is threaded into the recursive call, so the exported model \
         charges the same indices the run does and the law must still be stated:\n{}",
        lean.file
    );
    assert!(
        !lean.report.contains("declined"),
        "nothing about this law is approximate; report was:\n{}",
        lean.report
    );
}

// ---------------------------------------------------------------------------
// The fifth shape: the claim itself reaches one operation through more than
// one effectful call. The run numbers an operation across the whole case, so
// the claim's second call is charged from where the first left off, while the
// export numbers every call in the claim from `(BranchPath.Root, 0)`.
// ---------------------------------------------------------------------------

/// Two reads, one on each side of the claim, under a peer that answers with
/// the index it was handed. The run answers 0 on the left and 1 on the right,
/// which is the numbering the export cannot write down.
const TWO_CALLS_INDEXED: &str = r#"module TwoCallsIndexed
    intent = "Both sides of the claim read the peer, and the peer answers by call number."
    exposes [readOne, readOneToo]
    effects [Random.int]

fn readOne() -> Int
    ? "Read the peer once."
    ! [Random.int]
    Random.int(0, 100)

fn readOneToo() -> Int
    ? "Read the peer once, again."
    ! [Random.int]
    Random.int(0, 100)

fn peerByCall(path: BranchPath, call: Int, low: Int, high: Int) -> Result<Int, String>
    ? "The peer reports the call index it was handed."
    Result.Ok(call)

verify readOne law bothSidesRead
    given rnd: Random.int = [peerByCall]
    readOne() => readOneToo()
"#;

/// The same claim under a peer that ignores the index. `aver verify` passes,
/// because nothing the law asserts depends on the numbering, and that is how
/// this shape reaches export.
const TWO_CALLS_BLIND: &str = r#"module TwoCallsBlind
    intent = "Both sides of the claim read the peer, and the peer ignores the call number."
    exposes [readOne, readOneToo]
    effects [Random.int]

fn readOne() -> Int
    ? "Read the peer once."
    ! [Random.int]
    Random.int(0, 100)

fn readOneToo() -> Int
    ? "Read the peer once, again."
    ! [Random.int]
    Random.int(0, 100)

fn blind(path: BranchPath, call: Int, low: Int, high: Int) -> Result<Int, String>
    ? "A peer that answers the same whatever index it is handed."
    Result.Ok(42)

verify readOne law bothSidesRead
    given rnd: Random.int = [blind]
    readOne() => readOneToo()
"#;

#[test]
fn a_law_whose_claim_makes_two_effectful_calls_is_declined() {
    assert_eq!(
        verify(TWO_CALLS_INDEXED, "oracle_call_index_two_calls_indexed.av"),
        (0, 1),
        "the run numbers the claim's two reads 0 and 1, so a peer answering with its index \
         breaks the very law an index-blind peer satisfies — that gap is what the export \
         must not certify"
    );
    assert_eq!(
        verify(TWO_CALLS_BLIND, "oracle_call_index_two_calls_blind.av"),
        (1, 0),
        "under an index-blind peer the same claim passes, which is how this shape reaches \
         export in the first place"
    );
    let lean = export(TWO_CALLS_BLIND, "twocalls", "lean", "TwoCallsBlind.lean");
    assert_law_declined(&lean, "readOne", "bothSidesRead", "Random.int");
    let dafny = export(TWO_CALLS_BLIND, "twocalls", "dafny", "TwoCallsBlind.dfy");
    assert_law_declined(&dafny, "readOne", "bothSidesRead", "Random.int");
}

// ---------------------------------------------------------------------------
// The other side of the gate again: two shapes the export numbers exactly, so
// declining them would cost a law its theorem for no soundness gain.
// ---------------------------------------------------------------------------

/// Polling inside an independent product. The run gives each branch its own
/// slot and starts every operation in it at zero on each entry, so the branch
/// reads are index 0 in every turn of the loop however far the threaded
/// polling base has advanced at the sequential level.
const POLL_BRANCH: &str = r#"module PollBranch
    intent = "A poll loop that also polls inside an independent product."
    exposes [follow]
    effects [Process.stopRequested]

fn tag(pair: Tuple<Bool, Bool>) -> Int
    ? "Pack the two branch answers into one digit each."
    match pair
        (true, true) -> 100
        (true, false) -> 10
        (false, true) -> 1
        (false, false) -> 0

fn follow(steps: Int) -> Int
    ? "Poll twice in a product, then poll at the root, then continue."
    ! [Process.stopRequested]
    pair = (Process.stopRequested(), Process.stopRequested())!
    match Process.stopRequested()
        true -> steps * 1000 + tag(pair)
        false -> follow(steps + 1)

fn stopAfterTwo(path: BranchPath, call: Int) -> Bool
    ? "The third poll of any one branch asks it to stop."
    call >= 2

verify follow law branchPollsStartAtZero
    given stop: Process.stopRequested = [stopAfterTwo]
    follow(0) => 2000
"#;

#[test]
fn a_poll_inside_an_independent_product_is_numbered_from_zero() {
    assert_eq!(
        verify(POLL_BRANCH, "oracle_call_index_poll_branch.av"),
        (1, 0),
        "each branch polls at index 0 in every turn, so both branch answers are false and \
         the third root poll stops the loop on turn 2"
    );
    let lean = export(POLL_BRANCH, "pollbranch", "lean", "PollBranch.lean");
    for call in [
        "rnd_Process_stopRequested (BranchPath.child path 0) 0",
        "rnd_Process_stopRequested (BranchPath.child path 1) 0",
    ] {
        assert!(
            lean.file.contains(call),
            "a branch numbers its own calls from zero the way the run does; `{call}` is \
             missing from:\n{}",
            lean.file
        );
    }
    assert!(
        !lean.file.contains("(BranchPath.child path 0) oracleIndex"),
        "carrying the sequential polling base into a branch numbers the branch at the \
         loop's turn count while the run numbers it from zero, so the exported model \
         computes a different function from the second turn on:\n{}",
        lean.file
    );
    assert!(
        !lean.report.contains("declined"),
        "the branch numbering is exact once the base stays outside it, so the law keeps \
         its theorem; report was:\n{}",
        lean.report
    );
}

/// An uneven inner `match` inside one arm of an outer `match`, a sibling arm
/// that reads once, and nothing after the outer match. Every index in the
/// lifted body is the one the run charges, because the arm that is uneven is
/// never followed by a call whose index would depend on it.
const SIBLING_ARM: &str = r#"module SiblingArm
    intent = "An uneven inner match in one arm, a sibling arm beside it, and nothing after."
    exposes [pick]
    effects [Random.int]

fn pick(a: Int, b: Int) -> Int
    ? "The uneven inner match is the last thing its own arm does."
    ! [Random.int]
    match a
        0 -> match b
            0 -> Random.int(1, 6) * 10 + Random.int(1, 6)
            _ -> Random.int(1, 6)
        _ -> Random.int(1, 6)

fn peerByCall(path: BranchPath, call: Int, low: Int, high: Int) -> Result<Int, String>
    ? "The peer reports the call index it was handed."
    Result.Ok(call)

verify pick law siblingArmIsExact
    given rnd: Random.int = [peerByCall]
    pick(0, 0) => 1
"#;

#[test]
fn an_arm_beside_an_uneven_inner_match_still_exports() {
    assert_eq!(
        verify(SIBLING_ARM, "oracle_call_index_sibling_arm.av"),
        (1, 0),
        "the two reads of the inner arm are indices 0 and 1, so the packed answer is 1"
    );
    let lean = export(SIBLING_ARM, "siblingarm", "lean", "SiblingArm.lean");
    assert!(
        !lean.report.contains("declined"),
        "no call follows the uneven match, so nothing in this body is approximate and the \
         law must keep its theorem; report was:\n{}",
        lean.report
    );
    assert!(
        lean.file.contains("theorem pick_law_siblingArmIsExact"),
        "an exact body exports its law:\n{}",
        lean.file
    );
    assert_eq!(
        lean.file.matches("rnd_Random_int path 0 1 6").count(),
        3,
        "each arm starts from the index its own match was reached at, so three of the four \
         reads are index 0:\n{}",
        lean.file
    );
    assert!(
        !lean.file.contains("rnd_Random_int path 2 1 6"),
        "index 2 means an arm carried a sibling's calls:\n{}",
        lean.file
    );
}

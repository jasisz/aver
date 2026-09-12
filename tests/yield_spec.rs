//! `yield` lowering (jasisz/aver#1329, phase one), end to end: the three
//! fixtures under `tests/fixtures/yield_*` drive a lowered function from a
//! hand-written coordinator through every door — `run`, `verify`, the
//! wasm-gc lanes and the Lean check — and the generated Aver for the first
//! one is pinned verbatim.

#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use aver_cmd::{aver_bin, format_output, repo_root};

use std::path::PathBuf;
use std::process::{Command, Output};

fn fixture(name: &str) -> PathBuf {
    repo_root().join("tests/fixtures").join(name)
}

fn aver(fixture_name: &str, args: &[&str]) -> Output {
    let dir = fixture(fixture_name);
    let mut cmd = Command::new(aver_bin());
    cmd.current_dir(repo_root());
    cmd.arg(args[0]).arg(dir.join("main.av"));
    cmd.arg("--module-root").arg(&dir);
    cmd.args(&args[1..]);
    cmd.output().expect("aver runs")
}

fn assert_runs_and_prints(fixture_name: &str, args: &[&str], expected: &str) {
    let out = aver(fixture_name, args);
    assert!(out.status.success(), "{}", format_output(&out));
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        stdout.contains(expected),
        "expected {expected:?} in:\n{}",
        format_output(&out)
    );
}

fn assert_verify_passes(fixture_name: &str, args: &[&str], cases: &str) {
    let out = aver(fixture_name, args);
    assert!(out.status.success(), "{}", format_output(&out));
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        stdout.contains(&format!("{cases} cases passed | 0 failed")),
        "expected {cases} passing cases in:\n{}",
        format_output(&out)
    );
}

// ── The spike fixture: a Pool.claim loop driven to Done(15) ──────────────

#[test]
fn spike_loop_runs_to_done_15_on_the_vm() {
    assert_runs_and_prints("yield_spike", &["run"], "Done(15) ok");
}

#[test]
fn spike_verify_blocks_pass_on_the_vm() {
    assert_verify_passes("yield_spike", &["verify"], "6/6");
}

// ── Two requests of one kind and a request inside a match arm ───────────

#[test]
fn two_reads_run_and_verify_on_the_vm() {
    assert_runs_and_prints("yield_two_reads", &["run"], "pair = 15");
    assert_verify_passes("yield_two_reads", &["verify"], "8/8");
}

// ── Three request kinds: Claim, Release and Yield ───────────────────────

#[test]
fn three_kinds_run_and_verify_on_the_vm() {
    assert_runs_and_prints("yield_three_kinds", &["run"], "total = 7");
    assert_verify_passes("yield_three_kinds", &["verify"], "8/8");
}

// ── Continuations: a request in a non-tail match arm, a Unit answer, `?` ─

#[test]
fn continuations_run_and_verify_on_the_vm() {
    assert_runs_and_prints("yield_continuations", &["run"], "sum = 8");
    assert_verify_passes("yield_continuations", &["verify"], "6/6");
}

// ── A request in tail position: last expression, and match-arm leaf ─────

#[test]
fn tail_stop_runs_and_verifies_on_the_vm() {
    assert_runs_and_prints("yield_tail_stop", &["run"], "one = 10, pick = 6");
    assert_verify_passes("yield_tail_stop", &["verify"], "13/13");
}

#[test]
fn tail_stop_lean_check_builds_with_zero_errors_and_no_sorry() {
    assert_lean_check_clean(
        "yield_tail_stop",
        "YieldTailStop.lean",
        &["def __oneStart", "def __pickAnswerClaim"],
    );
}

// ── An unmarked effect in place, beside a request ───────────────────────

/// Decision 4: a process may perform an operation nobody answers where it
/// stands. `walk` announces the peer in the same expression that asks the
/// pool for it, so the announcement is written before the request and must
/// happen before it: the lowering hoists both, in the order the program
/// wrote them, instead of moving the stop to the front. Without that the
/// announcement lands in the answer function and the run prints it after
/// the coordinator's own line.
#[test]
fn an_in_place_effect_before_a_request_stays_before_it() {
    let out = aver("yield_in_place", &["run"]);
    assert!(out.status.success(), "{}", format_output(&out));
    let stdout = String::from_utf8_lossy(&out.stdout).to_string();
    let asking = stdout
        .find("asking 1")
        .unwrap_or_else(|| panic!("no announcement in:\n{stdout}"));
    let answering = stdout
        .find("answering 1")
        .unwrap_or_else(|| panic!("no answer line in:\n{stdout}"));
    assert!(
        asking < answering,
        "the announcement is written before the request, so it runs before it:\n{stdout}"
    );
    assert!(
        stdout.contains("total = 2"),
        "expected the handle to reach the sum:\n{stdout}"
    );
}

/// The same fact read off the generated Aver: the segment that runs before
/// the stop holds the call and declares its effect, and the segment that
/// resumes after the answer holds neither.
#[test]
fn the_segment_before_a_request_carries_the_effect_it_performs() {
    let (lowered, generated, _) = lower_fixture("yield_in_place");
    assert_eq!(lowered, vec!["walk".to_string()]);
    let start = generated
        .split_once("fn __walkStart")
        .map(|(_, rest)| rest.split("\n\nfn ").next().unwrap_or(rest).to_string())
        .unwrap_or_else(|| panic!("no __walkStart in:\n{generated}"));
    assert!(
        start.contains("! [Console.print]") && start.contains("announce(id)"),
        "the announcement belongs to the first segment:\n{start}"
    );
    let answer = generated
        .split_once("fn __walkAnswerClaim")
        .map(|(_, rest)| rest.split("\n\nfn ").next().unwrap_or(rest).to_string())
        .unwrap_or_else(|| panic!("no __walkAnswerClaim in:\n{generated}"));
    assert!(
        !answer.contains("announce(") && !answer.contains("! ["),
        "the answer segment performs nothing:\n{answer}"
    );
}

/// A `yield` function may name a whole namespace in its effect list, and an
/// operation of it performed in place is named by the operation, not by the
/// namespace: the generated segment declares `Console.print`. That bare entry
/// is the path the stop predicate used to read and decision 4 repurposed, so
/// one fixture keeps it walked.
#[test]
fn a_namespace_effect_entry_names_the_operation_the_segment_performs() {
    assert_runs_and_prints("yield_namespace_effect", &["run"], "total = 9");
    assert_verify_passes("yield_namespace_effect", &["verify"], "2/2");
    let (lowered, generated, _) = lower_fixture("yield_namespace_effect");
    assert_eq!(lowered, vec!["walk".to_string()]);
    assert!(
        generated.contains("! [Console.print]"),
        "the namespace entry admits the operation and the segment declares it:\n{generated}"
    );
}

// ── A program that answers a capability runs on the VM ──────────────────

/// A marked capability's generated reply types carry `Wait.Wake`, a sum that
/// reaches `Work.Job`, and no backend but the VM has a representation for one.
/// So every door that prepares a non-VM target refuses a program that answers
/// a capability, by name and for that reason, as it already refuses a job
/// kind. The VM runs and verifies all of these, above.
#[cfg(feature = "wasm")]
#[test]
fn an_answered_capability_is_refused_on_wasm_gc() {
    for fixture in [
        "yield_spike",
        "yield_two_reads",
        "yield_three_kinds",
        "yield_continuations",
        "yield_tail_stop",
        "yield_cross_module",
        "yield_tail_into",
        "yield_nested",
        "yield_nested_twice",
    ] {
        let out = aver(fixture, &["run", "--wasm-gc"]);
        let text = format!(
            "{}{}",
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        );
        assert!(
            text.contains("error[work-target]")
                && text.contains("is answered by module")
                && text.contains("no representation for yet"),
            "{fixture}:\n{}",
            format_output(&out)
        );
    }
}

// ── A process split into yield helpers ──────────────────────────────────

/// Decision 1: a tail call between two `yield` functions. `total` claims one
/// handle and hands the rest of the work to `rest`; the run drives one
/// protocol, `total`'s, from start to `Done`.
#[test]
fn a_tail_call_into_another_process_runs_and_verifies_on_the_vm() {
    assert_runs_and_prints("yield_tail_into", &["run"], "total = 20");
    assert_verify_passes("yield_tail_into", &["verify"], "10/10");
}

#[test]
fn tail_into_generates_exactly_the_pinned_protocol() {
    let (lowered, generated, items) = lower_fixture("yield_tail_into");
    assert_eq!(lowered, vec!["total".to_string(), "rest".to_string()]);
    assert_eq!(generated, TAIL_INTO_GENERATED);
    assert_removed(&items, "total");
    assert_removed(&items, "rest");
}

#[test]
fn tail_into_lean_check_builds_with_zero_errors_and_no_sorry() {
    assert_lean_check_clean(
        "yield_tail_into",
        "YieldTailInto.lean",
        &["def __totalInRestAt1", "def __restStart"],
    );
}

/// Decision 2: a non-tail call to a helper, with a request inside the helper
/// and work after the call in the caller.
#[test]
fn a_nested_call_runs_and_verifies_on_the_vm() {
    assert_runs_and_prints("yield_nested", &["run"], "walk = 9");
    assert_verify_passes("yield_nested", &["verify"], "8/8");
}

#[test]
fn nested_generates_exactly_the_pinned_protocol() {
    let (lowered, generated, items) = lower_fixture("yield_nested");
    assert_eq!(lowered, vec!["fetch".to_string(), "walk".to_string()]);
    assert_eq!(generated, NESTED_GENERATED);
    assert_removed(&items, "fetch");
    assert_removed(&items, "walk");
}

#[test]
fn nested_lean_check_builds_with_zero_errors_and_no_sorry() {
    assert_lean_check_clean(
        "yield_nested",
        "YieldNested.lean",
        &["inductive __WalkClaimState", "def __walkInFetchAt1"],
    );
}

/// A helper with two request kinds, entered twice from one caller: every kind
/// of the caller gains one variant per call site, and the live variables the
/// two sites carry differ.
#[test]
fn a_helper_nested_twice_runs_and_verifies_on_the_vm() {
    assert_runs_and_prints("yield_nested_twice", &["run"], "pairUp = 14");
    assert_verify_passes("yield_nested_twice", &["verify"], "11/11");
}

#[test]
fn nested_twice_generates_exactly_the_pinned_protocol() {
    let (lowered, generated, items) = lower_fixture("yield_nested_twice");
    assert_eq!(lowered, vec!["swap".to_string(), "pairUp".to_string()]);
    assert_eq!(generated, NESTED_TWICE_GENERATED);
    assert_removed(&items, "swap");
    assert_removed(&items, "pairUp");
}

// ── What nesting still refuses ──────────────────────────────────────────

fn assert_refused(fixture_name: &str, slug: &str, wording: &[&str]) {
    let out = aver(fixture_name, &["check"]);
    assert!(
        !out.status.success(),
        "the module must not check:\n{}",
        format_output(&out)
    );
    let stdout = String::from_utf8_lossy(&out.stdout).to_string();
    let errors: Vec<&str> = stdout
        .lines()
        .filter(|line| line.contains("error["))
        .collect();
    assert_eq!(errors.len(), 1, "one error only:\n{stdout}");
    assert!(
        errors[0].contains(slug),
        "expected {slug} in:\n{}",
        errors[0]
    );
    for text in wording {
        assert!(
            errors[0].contains(text),
            "expected {text:?} in:\n{}",
            errors[0]
        );
    }
}

/// Decision 2: mutual nesting is refused, not attempted. `ping` calls `pong`
/// and `pong` calls `ping`, so each one's state would have to hold the
/// other's, and the message says where the cycle runs and how to break it.
#[test]
fn mutual_nesting_is_refused_with_the_cycle_it_found() {
    assert_refused(
        "yield_mutual_nesting",
        "error[yield-unsupported]",
        &[
            "Mutual nesting is not supported by yield lowering",
            "ping calls pong calls ping",
            "pass what comes next as data in one of them",
        ],
    );
}

/// The branches of an independent product run independently, and a request
/// leaves a process one at a time, so a call into a helper's protocol inside
/// one is refused exactly as a request written there is.
#[test]
fn a_nested_call_inside_an_independent_product_is_refused() {
    assert_refused(
        "yield_nested_product",
        "error[yield-unsupported]",
        &[
            "a call to a yield helper, inside an independent product",
            "perform them one after another",
        ],
    );
}

// ── Across the module boundary: the importer drives the dependency ──────

/// The loader lowers a dependency before any importer reads it, so what
/// `CrossModule` sees of `Looper` is the protocol `Looper` now exposes:
/// `Looper.__loopStart`, `Looper.__LoopOutcome` and the answer functions.
#[test]
fn cross_module_runs_and_verifies_on_the_vm() {
    assert_runs_and_prints("yield_cross_module", &["run"], "total = 6");
    assert_verify_passes("yield_cross_module", &["verify"], "4/4");
}

/// The check door judges the surface an importer sees. `loop` is not on
/// it — the lowering took it off and put the protocol `CrossModule`
/// drives in its place — so it is not an export nobody uses.
#[test]
fn check_does_not_report_the_lowered_function_as_an_unused_expose() {
    let out = aver("yield_cross_module", &["check"]);
    assert!(out.status.success(), "{}", format_output(&out));
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        !stdout.contains("unused-expose"),
        "nothing in this program is an unused export:\n{}",
        format_output(&out)
    );
}

/// A dependency whose lowering fails has no protocol in it. A directory
/// report tells you why exactly once, in the dependency's own section
/// (against the dependency's own file) — not again in every importer's
/// section. The function it could not lower is still in the module as
/// written, so the importer's own call to it is separately answered with
/// the direct-call recipe rather than with a name that vanished.
#[test]
fn a_dependency_that_cannot_be_lowered_reports_at_the_importers_door() {
    let dir = fixture("yield_broken_dep");
    let out = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("check")
        .arg(&dir)
        .arg("--module-root")
        .arg(&dir)
        .output()
        .expect("aver runs");
    let stdout = String::from_utf8_lossy(&out.stdout).to_string();
    let (dependency, importer) = stdout
        .rsplit_once("Input: ")
        .unwrap_or_else(|| panic!("no per-input sections in:\n{stdout}"));
    assert!(
        importer.starts_with("main.av"),
        "the last section should be the importer's:\n{importer}"
    );
    assert!(
        dependency.contains("The type of 'kept' is not settled")
            && dependency.contains("looper.av:10:1"),
        "the dependency's reason, against the dependency's own section:\n{dependency}"
    );
    assert_eq!(
        importer
            .matches("The type of 'kept' is not settled")
            .count(),
        0,
        "the dependency's reason must not repeat in the importer's section:\n{importer}"
    );
    assert!(
        importer.contains("Function 'main' calls 'Looper.loop' directly"),
        "the function that failed to lower is still in the module:\n{importer}"
    );
}

// ── A live variable whose type the checker never settled ────────────────

/// `seen = {}` is `Map<K, V>` and nothing in the body says what it holds.
/// Writing that into a state variant would declare `K` and `V` nowhere and
/// report three type errors about generated names; the lowering stops
/// instead, with one error at the user's binding about the user's
/// variable, and generates nothing.
#[test]
fn an_unsettled_live_variable_asks_for_an_annotation_at_its_binding() {
    let out = aver("yield_open_type", &["check"]);
    assert!(
        !out.status.success(),
        "the module must not check:\n{}",
        format_output(&out)
    );
    let stdout = String::from_utf8_lossy(&out.stdout);
    let errors: Vec<&str> = stdout
        .lines()
        .filter(|line| line.contains("error["))
        .collect();
    assert_eq!(errors.len(), 1, "one error only:\n{}", format_output(&out));
    assert!(
        errors[0].contains("The type of 'seen' is not settled ('Map<K, V>')")
            && errors[0].contains("'seen: <type> = ...'"),
        "{}",
        format_output(&out)
    );
    // At the binding, and with no generated name anywhere in the output.
    assert!(
        stdout.contains("main.av:10:1"),
        "expected the error at the binding on line 10:\n{}",
        format_output(&out)
    );
    assert!(
        !stdout.contains("__Loop") && !stdout.contains("__loop"),
        "a function that fails to lower generates nothing:\n{}",
        format_output(&out)
    );
}

// ── The generated Aver, verbatim ────────────────────────────────────────

/// The generated items are ordinary types and pure functions of the
/// module (the proof-visible decision): this is exactly what the checker,
/// every backend and both proof exporters see in place of `loop`.
const SPIKE_GENERATED: &str = r#"type __LoopClaimState
    AwaitR(Int, Int)

type __LoopYieldState
    Await2(Int, Int)

type __LoopRequest
    Claim(Int, __LoopClaimState)
    Yield(__LoopYieldState)

type __LoopOutcome
    Done(Int)
    Waiting(__LoopRequest)

fn __loopStart(id: Int, done: Int) -> __LoopOutcome
    ? "Claims handles for id until the pool answers None, summing the handles into done."
    (__LoopOutcome).Waiting((__LoopRequest).Claim(id, (__LoopClaimState).AwaitR(id, done)))

fn __loopAnswerClaim(__state: __LoopClaimState, __answer: Option<Int>) -> __LoopOutcome
    ? "Resumes 'loop' after the coordinator answered its Claim request."
    match __state
        __LoopClaimState.AwaitR(id, done) -> match __answer
            Option.None -> (__LoopOutcome).Done(done)
            Option.Some(h) -> (__LoopOutcome).Waiting((__LoopRequest).Yield((__LoopYieldState).Await2(id, (done + h))))

fn __loopAnswerYield(__state: __LoopYieldState) -> __LoopOutcome
    ? "Re-enters 'loop' at its tail call with the arguments the Yield request carries."
    match __state
        __LoopYieldState.Await2(id, done) -> __loopStart(id, done)
"#;

/// A tail request is cut like any other: `__oneStart` stops at once with
/// an empty state variant and the answer *is* the result, and `pick`'s two
/// match-arm leaves stop with the live variables their continuation reads.
const TAIL_STOP_GENERATED: &str = r#"type __OneClaimState
    Await1

type __OneRequest
    Claim(Int, __OneClaimState)

type __OneOutcome
    Done(Int)
    Waiting(__OneRequest)

fn __oneStart(id: Int) -> __OneOutcome
    ? "Claims the handle of id; the request is the last expression of the body."
    (__OneOutcome).Waiting((__OneRequest).Claim(id, (__OneClaimState).Await1))

fn __oneAnswerClaim(__state: __OneClaimState, __answer: Int) -> __OneOutcome
    ? "Resumes 'one' after the coordinator answered its Claim request."
    match __state
        __OneClaimState.Await1 -> (__OneOutcome).Done(__answer)

type __PickClaimState
    Await1
    Await3(Int, Int)

type __PickYieldState
    Await2(Int, Int)

type __PickRequest
    Claim(Int, __PickClaimState)
    Yield(__PickYieldState)

type __PickOutcome
    Done(Int)
    Waiting(__PickRequest)

fn __pickStart(id: Int, left: Int) -> __PickOutcome
    ? "Adds a claimed handle to id once per round, then claims the handle of the total; both claims sit at the leaf of a match arm."
    match left
        0 -> __pickJoin1(id, left, 0)
        _ -> (__PickOutcome).Waiting((__PickRequest).Claim(id, (__PickClaimState).Await3(id, left)))

fn __pickAnswerClaim(__state: __PickClaimState, __answer: Int) -> __PickOutcome
    ? "Resumes 'pick' after the coordinator answered its Claim request."
    match __state
        __PickClaimState.Await1 -> (__PickOutcome).Done(__answer)
        __PickClaimState.Await3(id, left) -> __pickJoin1(id, left, __answer)

fn __pickAnswerYield(__state: __PickYieldState) -> __PickOutcome
    ? "Re-enters 'pick' at its tail call with the arguments the Yield request carries."
    match __state
        __PickYieldState.Await2(id, left) -> __pickStart(id, left)

fn __pickJoin1(id: Int, left: Int, extra: Int) -> __PickOutcome
    ? "Continues 'pick' after the branch at line 15."
    match left
        0 -> (__PickOutcome).Waiting((__PickRequest).Claim((id + extra), (__PickClaimState).Await1))
        _ -> (__PickOutcome).Waiting((__PickRequest).Yield((__PickYieldState).Await2((id + extra), (left - 1))))
"#;

/// Decision 1: a tail call to another `yield` function enters that
/// function's protocol. `total` stops with a `Yield` request carrying what
/// `rest` is entered with — the caller has nothing left, so nothing of it is
/// kept — and every request `rest` makes on its way leaves as a request of
/// `total` carrying `rest`'s own state inside `__Total<Kind>State`.
const TAIL_INTO_GENERATED: &str = r#"type __TotalClaimState
    AwaitFirst
    InRestAt1(__RestClaimState)

type __TotalYieldState
    Await2(Int, Int)
    InRestAt1(__RestYieldState)

type __TotalRequest
    Claim(Int, __TotalClaimState)
    Yield(__TotalYieldState)

type __TotalOutcome
    Done(Int)
    Waiting(__TotalRequest)

fn __totalStart(id: Int) -> __TotalOutcome
    ? "Claims the handle of id and hands the rest of the work to 'rest' in tail position."
    (__TotalOutcome).Waiting((__TotalRequest).Claim(id, (__TotalClaimState).AwaitFirst))

fn __totalAnswerClaim(__state: __TotalClaimState, __answer: Int) -> __TotalOutcome
    ? "Resumes 'total' after the coordinator answered its Claim request."
    match __state
        __TotalClaimState.AwaitFirst -> (__TotalOutcome).Waiting((__TotalRequest).Yield((__TotalYieldState).Await2(__answer, 0)))
        __TotalClaimState.InRestAt1(__inner) -> __totalInRestAt1(__restAnswerClaim(__inner, __answer))

fn __totalAnswerYield(__state: __TotalYieldState) -> __TotalOutcome
    ? "Re-enters 'total' at its tail call with the arguments the Yield request carries."
    match __state
        __TotalYieldState.Await2(left, seen) -> __totalInRestAt1(__restStart(left, seen))
        __TotalYieldState.InRestAt1(__inner) -> __totalInRestAt1(__restAnswerYield(__inner))

fn __totalInRestAt1(__outcome: __RestOutcome) -> __TotalOutcome
    ? "Routes what 'rest' answered back into 'total': a result continues here, a request of 'rest' leaves as a request of 'total' carrying the nested state."
    match __outcome
        __RestOutcome.Done(__value) -> (__TotalOutcome).Done(__value)
        __RestOutcome.Waiting(__request) -> match __request
            __RestRequest.Claim(__a0, __inner) -> (__TotalOutcome).Waiting((__TotalRequest).Claim(__a0, (__TotalClaimState).InRestAt1(__inner)))
            __RestRequest.Yield(__inner) -> (__TotalOutcome).Waiting((__TotalRequest).Yield((__TotalYieldState).InRestAt1(__inner)))

type __RestClaimState
    AwaitHandle(Int, Int)

type __RestYieldState
    Await2(Int, Int)

type __RestRequest
    Claim(Int, __RestClaimState)
    Yield(__RestYieldState)

type __RestOutcome
    Done(Int)
    Waiting(__RestRequest)

fn __restStart(left: Int, seen: Int) -> __RestOutcome
    ? "Claims one handle per round until left runs down, summing the handles into seen."
    (__RestOutcome).Waiting((__RestRequest).Claim(left, (__RestClaimState).AwaitHandle(left, seen)))

fn __restAnswerClaim(__state: __RestClaimState, __answer: Int) -> __RestOutcome
    ? "Resumes 'rest' after the coordinator answered its Claim request."
    match __state
        __RestClaimState.AwaitHandle(left, seen) -> match left
            0 -> (__RestOutcome).Done((seen + __answer))
            _ -> (__RestOutcome).Waiting((__RestRequest).Yield((__RestYieldState).Await2((left - 1), (seen + __answer))))

fn __restAnswerYield(__state: __RestYieldState) -> __RestOutcome
    ? "Re-enters 'rest' at its tail call with the arguments the Yield request carries."
    match __state
        __RestYieldState.Await2(left, seen) -> __restStart(left, seen)
"#;

/// Decision 2: a non-tail call nests the helper's state inside the caller's.
/// `__WalkClaimState` has one variant for the call site, holding `fetch`'s own
/// `__FetchClaimState` beside the two variables `walk` still reads, and the
/// routing function is where a `Done` continues the caller and a `Waiting`
/// leaves as a request of the caller.
///
/// Decision 3 is visible in `__walkStart`: the call is not a stop by itself,
/// so the segment enters `fetch` in place and only what `fetch` waits on is a
/// request.
const NESTED_GENERATED: &str = r#"type __FetchClaimState
    AwaitHandle(Int)

type __FetchRequest
    Claim(Int, __FetchClaimState)

type __FetchOutcome
    Done(Int)
    Waiting(__FetchRequest)

fn __fetchStart(peer: Int) -> __FetchOutcome
    ? "Claims the handle of peer and counts the peer itself into it."
    (__FetchOutcome).Waiting((__FetchRequest).Claim(peer, (__FetchClaimState).AwaitHandle(peer)))

fn __fetchAnswerClaim(__state: __FetchClaimState, __answer: Int) -> __FetchOutcome
    ? "Resumes 'fetch' after the coordinator answered its Claim request."
    match __state
        __FetchClaimState.AwaitHandle(peer) -> (__FetchOutcome).Done((__answer + peer))

type __WalkClaimState
    InFetchAt1(__FetchClaimState, Int, Int)

type __WalkYieldState
    Await1(Int, Int)

type __WalkRequest
    Claim(Int, __WalkClaimState)
    Yield(__WalkYieldState)

type __WalkOutcome
    Done(Int)
    Waiting(__WalkRequest)

fn __walkStart(id: Int, seen: Int) -> __WalkOutcome
    ? "Fetches one handle per round, adds it to seen and goes round until id runs down."
    __walkInFetchAt1(__fetchStart(id), id, seen)

fn __walkAnswerClaim(__state: __WalkClaimState, __answer: Int) -> __WalkOutcome
    ? "Resumes 'walk' after the coordinator answered its Claim request."
    match __state
        __WalkClaimState.InFetchAt1(__inner, id, seen) -> __walkInFetchAt1(__fetchAnswerClaim(__inner, __answer), id, seen)

fn __walkAnswerYield(__state: __WalkYieldState) -> __WalkOutcome
    ? "Re-enters 'walk' at its tail call with the arguments the Yield request carries."
    match __state
        __WalkYieldState.Await1(id, seen) -> __walkStart(id, seen)

fn __walkJoin1(id: Int, seen: Int, got: Int) -> __WalkOutcome
    ? "Continues 'walk' after the branch at line 16."
    match id
        0 -> (__WalkOutcome).Done((seen + got))
        _ -> (__WalkOutcome).Waiting((__WalkRequest).Yield((__WalkYieldState).Await1((id - 1), (seen + got))))

fn __walkInFetchAt1(__outcome: __FetchOutcome, id: Int, seen: Int) -> __WalkOutcome
    ? "Routes what 'fetch' answered back into 'walk': a result continues here, a request of 'fetch' leaves as a request of 'walk' carrying the nested state."
    match __outcome
        __FetchOutcome.Done(got) -> __walkJoin1(id, seen, got)
        __FetchOutcome.Waiting(__request) -> match __request
            __FetchRequest.Claim(__a0, __inner) -> (__WalkOutcome).Waiting((__WalkRequest).Claim(__a0, (__WalkClaimState).InFetchAt1(__inner, id, seen)))
"#;

/// One helper with two request kinds, nested twice: each of the caller's two
/// state sums gains one variant per call site, the two sites carry different
/// live variables, and both answer functions route by site.
const NESTED_TWICE_GENERATED: &str = r#"type __SwapReleaseState
    Await1(Int)

type __SwapClaimState
    Await2

type __SwapRequest
    Release(Int, __SwapReleaseState)
    Claim(Int, __SwapClaimState)

type __SwapOutcome
    Done(Int)
    Waiting(__SwapRequest)

fn __swapStart(handle: Int) -> __SwapOutcome
    ? "Gives one handle back and, when the pool took it, claims the next one for it."
    (__SwapOutcome).Waiting((__SwapRequest).Release(handle, (__SwapReleaseState).Await1(handle)))

fn __swapAnswerRelease(__state: __SwapReleaseState, __answer: Bool) -> __SwapOutcome
    ? "Resumes 'swap' after the coordinator answered its Release request."
    match __state
        __SwapReleaseState.Await1(handle) -> match __answer
            false -> (__SwapOutcome).Done(0)
            true -> (__SwapOutcome).Waiting((__SwapRequest).Claim(handle, (__SwapClaimState).Await2))

fn __swapAnswerClaim(__state: __SwapClaimState, __answer: Int) -> __SwapOutcome
    ? "Resumes 'swap' after the coordinator answered its Claim request."
    match __state
        __SwapClaimState.Await2 -> (__SwapOutcome).Done(__answer)

type __PairUpReleaseState
    InSwapAt1(__SwapReleaseState, Int)
    InSwapAt2(__SwapReleaseState, Int)

type __PairUpClaimState
    InSwapAt1(__SwapClaimState, Int)
    InSwapAt2(__SwapClaimState, Int)

type __PairUpRequest
    Release(Int, __PairUpReleaseState)
    Claim(Int, __PairUpClaimState)

type __PairUpOutcome
    Done(Int)
    Waiting(__PairUpRequest)

fn __pairUpStart(a: Int, b: Int) -> __PairUpOutcome
    ? "Swaps the handle of a, then the handle of b, and adds what came back."
    __pairUpInSwapAt1(__swapStart(a), b)

fn __pairUpAnswerRelease(__state: __PairUpReleaseState, __answer: Bool) -> __PairUpOutcome
    ? "Resumes 'pairUp' after the coordinator answered its Release request."
    match __state
        __PairUpReleaseState.InSwapAt1(__inner, b) -> __pairUpInSwapAt1(__swapAnswerRelease(__inner, __answer), b)
        __PairUpReleaseState.InSwapAt2(__inner, first) -> __pairUpInSwapAt2(__swapAnswerRelease(__inner, __answer), first)

fn __pairUpAnswerClaim(__state: __PairUpClaimState, __answer: Int) -> __PairUpOutcome
    ? "Resumes 'pairUp' after the coordinator answered its Claim request."
    match __state
        __PairUpClaimState.InSwapAt1(__inner, b) -> __pairUpInSwapAt1(__swapAnswerClaim(__inner, __answer), b)
        __PairUpClaimState.InSwapAt2(__inner, first) -> __pairUpInSwapAt2(__swapAnswerClaim(__inner, __answer), first)

fn __pairUpJoin1(first: Int, second: Int) -> __PairUpOutcome
    ? "Continues 'pairUp' after the branch at line 18."
    (__PairUpOutcome).Done((first + second))

fn __pairUpInSwapAt2(__outcome: __SwapOutcome, first: Int) -> __PairUpOutcome
    ? "Routes what 'swap' answered back into 'pairUp': a result continues here, a request of 'swap' leaves as a request of 'pairUp' carrying the nested state."
    match __outcome
        __SwapOutcome.Done(second) -> __pairUpJoin1(first, second)
        __SwapOutcome.Waiting(__request) -> match __request
            __SwapRequest.Release(__a0, __inner) -> (__PairUpOutcome).Waiting((__PairUpRequest).Release(__a0, (__PairUpReleaseState).InSwapAt2(__inner, first)))
            __SwapRequest.Claim(__a0, __inner) -> (__PairUpOutcome).Waiting((__PairUpRequest).Claim(__a0, (__PairUpClaimState).InSwapAt2(__inner, first)))

fn __pairUpJoin2(b: Int, first: Int) -> __PairUpOutcome
    ? "Continues 'pairUp' after the branch at line 17."
    __pairUpInSwapAt2(__swapStart(b), first)

fn __pairUpInSwapAt1(__outcome: __SwapOutcome, b: Int) -> __PairUpOutcome
    ? "Routes what 'swap' answered back into 'pairUp': a result continues here, a request of 'swap' leaves as a request of 'pairUp' carrying the nested state."
    match __outcome
        __SwapOutcome.Done(first) -> __pairUpJoin2(b, first)
        __SwapOutcome.Waiting(__request) -> match __request
            __SwapRequest.Release(__a0, __inner) -> (__PairUpOutcome).Waiting((__PairUpRequest).Release(__a0, (__PairUpReleaseState).InSwapAt1(__inner, b)))
            __SwapRequest.Claim(__a0, __inner) -> (__PairUpOutcome).Waiting((__PairUpRequest).Claim(__a0, (__PairUpClaimState).InSwapAt1(__inner, b)))
"#;

/// Run a fixture through the front door and return what the lowering did
/// together with the lowered module.
fn lower_fixture(fixture_name: &str) -> (Vec<String>, String, Vec<aver::ast::TopLevel>) {
    let dir = fixture(fixture_name);
    let source = std::fs::read_to_string(dir.join("main.av")).expect("fixture source");
    let mut items = aver::source::parse_source(&source).expect("fixture parses");
    let user_program_len = items.len();
    let base_dir = dir.to_string_lossy().to_string();
    let front = aver::ir::pipeline::front(
        &mut items,
        aver::ir::pipeline::FrontConfig {
            run_tco: true,
            typecheck: Some(&aver::ir::TypecheckMode::Full {
                base_dir: Some(&base_dir),
            }),
            user_program_len,
            marked: &aver::config::MarkedCapabilities::for_project_dir(Some(&base_dir)),
            on_after_pass: None,
        },
    );
    let errors = front.typecheck.expect("gate ran").errors;
    assert!(errors.is_empty(), "{errors:?}");
    let report = front.yield_lowering.expect("the module was lowered");
    (report.lowered.clone(), report.generated_source(), items)
}

fn assert_removed(items: &[aver::ast::TopLevel], fn_name: &str) {
    assert!(
        !items
            .iter()
            .any(|item| matches!(item, aver::ast::TopLevel::FnDef(fd) if fd.name == fn_name)),
        "'{fn_name}' should have been replaced by its protocol"
    );
}

/// `Yield` is the kind of the self tail call, and an operation may not
/// take that name: `Sched.yield` is `SchedYield`, the way any other clash
/// of leaf names is resolved. The two kinds carry different data, so
/// merging them reported that two requests of kind 'Yield' disagree about
/// their argument and answer types — about a name the user never wrote.
#[test]
fn an_operation_named_yield_keeps_off_the_tail_call_kind() {
    let (lowered, generated, _) = lower_fixture("yield_reserved_kind");
    assert_eq!(lowered, vec!["loop".to_string()]);
    for text in [
        "type __LoopSchedYieldState",
        "type __LoopYieldState",
        "    SchedYield(Int, __LoopSchedYieldState)",
        "    Yield(__LoopYieldState)",
        "fn __loopAnswerSchedYield(__state: __LoopSchedYieldState, __answer: Int) -> __LoopOutcome",
        "fn __loopAnswerYield(__state: __LoopYieldState) -> __LoopOutcome",
    ] {
        assert!(
            generated.contains(text),
            "expected {text:?} in:\n{generated}"
        );
    }
}

#[test]
fn spike_generates_exactly_the_pinned_protocol() {
    let (lowered, generated, items) = lower_fixture("yield_spike");
    assert_eq!(lowered, vec!["loop".to_string()]);
    assert_eq!(generated, SPIKE_GENERATED);
    // The original function is gone: its protocol replaced it in place.
    assert_removed(&items, "loop");
}

#[test]
fn tail_stop_generates_exactly_the_pinned_protocol() {
    let (lowered, generated, items) = lower_fixture("yield_tail_stop");
    assert_eq!(lowered, vec!["one".to_string(), "pick".to_string()]);
    assert_eq!(generated, TAIL_STOP_GENERATED);
    assert_removed(&items, "one");
    assert_removed(&items, "pick");
}

/// `aver context` renders the LOWERED module, entry and dependency
/// alike (diagnostics/context.rs::build_context_for_items): the
/// protocol's functions are listed with their generated signatures —
/// which the module as written does not contain at all — and the
/// function they replaced is listed nowhere.
#[test]
fn context_dump_renders_the_lowered_module_for_the_entry_and_its_dependency() {
    // The entry module: `loop` is private there, so its protocol shows
    // up under the coordinator that drives it.
    let out = aver("yield_spike", &["context", "--focus", "drive"]);
    assert!(out.status.success(), "{}", format_output(&out));
    let spike = String::from_utf8_lossy(&out.stdout).to_string();
    for signature in [
        "`__loopStart(id: Int, done: Int) -> __LoopOutcome`",
        "`__loopAnswerClaim(__state: __LoopClaimState, __answer: Option<Int>) -> __LoopOutcome`",
    ] {
        assert!(
            spike.contains(signature),
            "expected {signature} in:\n{spike}"
        );
    }
    assert!(
        !spike.contains("loop(id: Int, done: Int) -> Int"),
        "the removed `loop` should not be listed:\n{spike}"
    );

    // The dependency, as its importer's dump describes it.
    let out = aver("yield_cross_module", &["context"]);
    assert!(out.status.success(), "{}", format_output(&out));
    let dump = String::from_utf8_lossy(&out.stdout).to_string();
    let looper = dump
        .split_once("## Module: Looper")
        .map(|(_, rest)| rest.split("## Module:").next().unwrap_or(rest).to_string())
        .unwrap_or_else(|| panic!("no Looper section in:\n{dump}"));
    assert!(
        looper.contains("`__loopStart(id: Int, seen: Int) -> __LoopOutcome`"),
        "expected Looper.__loopStart in:\n{looper}"
    );
    assert!(
        !looper.contains("loop(id: Int, seen: Int) -> Int"),
        "the removed Looper.loop should not be listed:\n{looper}"
    );
}

// ── The Lean check: generated items are total, no sorry ──────────────────

/// The proof-visible decision at work: `__loopStart`, `__loopAnswerClaim`
/// and `__loopAnswerYield` are exported to Lean like any other pure
/// function and the package builds with zero errors and zero `sorry`.
#[test]
fn spike_lean_check_builds_with_zero_errors_and_no_sorry() {
    assert_lean_check_clean(
        "yield_spike",
        "YieldSpike.lean",
        &[
            "inductive __LoopRequest",
            "def __loopStart",
            "def __loopAnswerClaim",
        ],
    );
}

fn assert_lean_check_clean(fixture_name: &str, lean_file: &str, names: &[&str]) {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping yield Lean check: `lake` not available");
        return;
    }
    // The fixture's own name, because these checks run beside each other and
    // each one removes its directory when it is done.
    let output_dir = std::env::temp_dir().join(format!(
        "aver-yield-lean-{fixture_name}-{}",
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0)
    ));
    let out = aver(
        fixture_name,
        &[
            "proof",
            "--backend",
            "lean",
            "-o",
            output_dir.to_str().expect("utf-8 temp dir"),
            "--check",
            "--check-json",
            "--sorry-budget",
            "0",
        ],
    );
    let json_line = out
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with('{')))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&out)))
        .to_string();
    let summary: serde_json::Value = serde_json::from_str(&json_line).expect("summary JSON");
    assert_eq!(
        summary["build_errors"].as_u64(),
        Some(0),
        "{}",
        format_output(&out)
    );
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(0),
        "{}",
        format_output(&out)
    );
    assert_eq!(
        summary["passed"].as_bool(),
        Some(true),
        "{}",
        format_output(&out)
    );
    let lean = std::fs::read_to_string(output_dir.join(lean_file)).expect("Lean output");
    for name in names {
        assert!(lean.contains(name), "{name} missing from:\n{lean}");
    }
    let _ = std::fs::remove_dir_all(&output_dir);
}

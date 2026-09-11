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

#[cfg(feature = "wasm")]
#[test]
fn spike_loop_runs_to_done_15_on_wasm_gc() {
    assert_runs_and_prints("yield_spike", &["run", "--wasm-gc"], "Done(15) ok");
}

#[cfg(feature = "wasm")]
#[test]
fn spike_verify_blocks_pass_on_wasm_gc() {
    assert_verify_passes("yield_spike", &["verify", "--wasm-gc"], "6/6");
}

// ── Two requests of one kind and a request inside a match arm ───────────

#[test]
fn two_reads_run_and_verify_on_the_vm() {
    assert_runs_and_prints("yield_two_reads", &["run"], "pair = 15");
    assert_verify_passes("yield_two_reads", &["verify"], "8/8");
}

#[cfg(feature = "wasm")]
#[test]
fn two_reads_run_and_verify_on_wasm_gc() {
    assert_runs_and_prints("yield_two_reads", &["run", "--wasm-gc"], "pair = 15");
    assert_verify_passes("yield_two_reads", &["verify", "--wasm-gc"], "8/8");
}

// ── Three request kinds: Claim, Release and Yield ───────────────────────

#[test]
fn three_kinds_run_and_verify_on_the_vm() {
    assert_runs_and_prints("yield_three_kinds", &["run"], "total = 7");
    assert_verify_passes("yield_three_kinds", &["verify"], "8/8");
}

#[cfg(feature = "wasm")]
#[test]
fn three_kinds_run_and_verify_on_wasm_gc() {
    assert_runs_and_prints("yield_three_kinds", &["run", "--wasm-gc"], "total = 7");
    assert_verify_passes("yield_three_kinds", &["verify", "--wasm-gc"], "8/8");
}

// ── Continuations: a request in a non-tail match arm, a Unit answer, `?` ─

#[test]
fn continuations_run_and_verify_on_the_vm() {
    assert_runs_and_prints("yield_continuations", &["run"], "sum = 8");
    assert_verify_passes("yield_continuations", &["verify"], "6/6");
}

#[cfg(feature = "wasm")]
#[test]
fn continuations_run_and_verify_on_wasm_gc() {
    assert_runs_and_prints("yield_continuations", &["run", "--wasm-gc"], "sum = 8");
    assert_verify_passes("yield_continuations", &["verify", "--wasm-gc"], "6/6");
}

// ── A request in tail position: last expression, and match-arm leaf ─────

#[test]
fn tail_stop_runs_and_verifies_on_the_vm() {
    assert_runs_and_prints("yield_tail_stop", &["run"], "one = 10, pick = 6");
    assert_verify_passes("yield_tail_stop", &["verify"], "13/13");
}

#[cfg(feature = "wasm")]
#[test]
fn tail_stop_runs_and_verifies_on_wasm_gc() {
    assert_runs_and_prints(
        "yield_tail_stop",
        &["run", "--wasm-gc"],
        "one = 10, pick = 6",
    );
    assert_verify_passes("yield_tail_stop", &["verify", "--wasm-gc"], "13/13");
}

#[test]
fn tail_stop_lean_check_builds_with_zero_errors_and_no_sorry() {
    assert_lean_check_clean(
        "yield_tail_stop",
        "YieldTailStop.lean",
        &["def __oneStart", "def __pickAnswerClaim"],
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

#[cfg(feature = "wasm")]
#[test]
fn cross_module_runs_and_verifies_on_wasm_gc() {
    assert_runs_and_prints("yield_cross_module", &["run", "--wasm-gc"], "total = 6");
    assert_verify_passes("yield_cross_module", &["verify", "--wasm-gc"], "4/4");
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
    let output_dir = std::env::temp_dir().join(format!(
        "aver-yield-lean-{}",
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

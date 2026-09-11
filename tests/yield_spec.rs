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

#[test]
fn spike_generates_exactly_the_pinned_protocol() {
    let dir = fixture("yield_spike");
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
    let report = front.yield_lowering.expect("loop was lowered");
    assert_eq!(report.lowered, vec!["loop".to_string()]);
    assert_eq!(report.generated_source(), SPIKE_GENERATED);
    // The original function is gone: its protocol replaced it in place.
    assert!(
        !items
            .iter()
            .any(|item| matches!(item, aver::ast::TopLevel::FnDef(fd) if fd.name == "loop"))
    );
}

/// `aver context` goes through the same `front` entry as every other
/// door (diagnostics/context.rs::compute_context_fn_flags), so it sees
/// the lowered module: the generated names appear in its dump and the
/// removed `loop` does not.
#[test]
fn context_dump_shows_the_generated_names_not_the_removed_function() {
    let out = aver("yield_spike", &["context"]);
    assert!(out.status.success(), "{}", format_output(&out));
    let stdout = String::from_utf8_lossy(&out.stdout);
    // `drive`'s signature and verify examples are lowered-module names:
    // `__LoopOutcome` from the parameter type, `__loopStart` from the
    // verify examples it calls.
    for name in ["__LoopOutcome", "__loopStart"] {
        assert!(stdout.contains(name), "expected {name:?} in:\n{stdout}");
    }
    assert!(
        !stdout.contains("fn loop("),
        "the removed `loop` should not appear:\n{stdout}"
    );
}

// ── The Lean check: generated items are total, no sorry ──────────────────

/// The proof-visible decision at work: `__loopStart`, `__loopAnswerClaim`
/// and `__loopAnswerYield` are exported to Lean like any other pure
/// function and the package builds with zero errors and zero `sorry`.
#[test]
fn spike_lean_check_builds_with_zero_errors_and_no_sorry() {
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
        "yield_spike",
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
    let lean = std::fs::read_to_string(output_dir.join("YieldSpike.lean")).expect("Lean output");
    for name in [
        "inductive __LoopRequest",
        "def __loopStart",
        "def __loopAnswerClaim",
    ] {
        assert!(lean.contains(name), "{name} missing from:\n{lean}");
    }
    let _ = std::fs::remove_dir_all(&output_dir);
}

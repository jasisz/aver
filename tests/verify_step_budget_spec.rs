//! A verify case that runs out of budget is not a failing case.
//!
//! `aver verify` caps each case at a per-call opcode budget so a
//! tail-recursive shape without a base case bails instead of pinning the
//! host. Real corpora contain cases that need more than the default and are
//! not runaway at all — a consensus-max Bitcoin script, a transaction whose
//! every input hashes the whole transaction — and the only thing the cap
//! could say about them was `fail[verify-runtime-error]: case aborted`. That
//! reads as "your engine is wrong", so the repair it invites is to delete the
//! case, and what is left behind is prose in a module `intent` recording an
//! answer nothing re-checks.
//!
//! Two things had to be true for those cases to come back:
//!
//! 1. A project can raise the budget for the fn it knows is expensive, in
//!    writing, in `aver.toml` — and only there, because it is project policy
//!    and not program meaning.
//! 2. Exceeding a budget — the default or a raised one — is a third case
//!    outcome. Not a pass, because nothing was observed; not a failure,
//!    because nothing disagreed. It is counted on its own, it fails the run,
//!    and the proof lane refuses to state a claim about it instead of falling
//!    back to the expected expression the author wrote.
#![cfg(feature = "runtime")]

mod support {
    pub mod scratch_dir;
}

use std::path::{Path, PathBuf};
use std::process::Command;

use support::scratch_dir::{ScratchDir, temp_dir};

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn format_output(out: &std::process::Output) -> String {
    format!(
        "status: {}\nstdout:\n{}\nstderr:\n{}",
        out.status,
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
    )
}

/// Stage one fixture module plus an `aver.toml` in a fresh project directory.
/// Each test writes its own config, because the config is the subject.
fn project(prefix: &str, fixture: &str, aver_toml: &str) -> ScratchDir {
    let dir = temp_dir(prefix);
    let source = repo_root()
        .join("tests/fixtures/verify_step_budget")
        .join(fixture);
    std::fs::copy(&source, dir.join("main.av")).expect("stage the fixture module");
    std::fs::write(dir.join("aver.toml"), aver_toml).expect("stage aver.toml");
    dir
}

fn run_aver_in(dir: &Path, args: &[&str]) -> std::process::Output {
    Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(dir)
        .args(args)
        .env("NO_COLOR", "1")
        .output()
        .expect("expected the `aver` binary to run")
}

fn verify(dir: &Path, extra: &[&str]) -> std::process::Output {
    let mut args = vec!["verify", "main.av", "--module-root", "."];
    args.extend_from_slice(extra);
    run_aver_in(dir, &args)
}

fn stdout_of(out: &std::process::Output) -> String {
    String::from_utf8_lossy(&out.stdout).to_string()
}

const RAISED: &str = r#"
[[verify.costly]]
fn         = "countdown"
step-limit = 50000000
reason     = "the case counts down from 400,000; that is the case, not a runaway"
"#;

// ── a. a project with no [verify] section is untouched ──────────────────

#[test]
fn a_project_without_a_verify_section_reports_exactly_as_before() {
    let dir = project("budget-plain", "plain.av", "");
    let out = verify(&dir, &[]);
    let text = stdout_of(&out);

    assert!(
        text.contains("✗ double      1/2 passed (1 mismatch)"),
        "{}",
        format_output(&out)
    );
    assert!(
        text.contains("Summary: 1 module | 1 block | 1/2 cases passed | 1 failed"),
        "{}",
        format_output(&out)
    );
    // No decline anywhere: not in the block line, not in the summary, and no
    // new key in the JSON.
    assert!(!text.contains("not answered"), "{}", format_output(&out));

    let json = verify(&dir, &["--json"]);
    let json_text = stdout_of(&json);
    assert!(
        !json_text.contains("declined"),
        "a project with nothing declined must emit the JSON it always did:\n{}",
        format_output(&json)
    );
    assert!(
        json_text.contains(r#""cases_passed":1,"cases_failed":1}"#),
        "{}",
        format_output(&json)
    );
    assert_eq!(out.status.code(), Some(1), "{}", format_output(&out));
}

// ── b. the budget, default and raised ───────────────────────────────────

#[test]
fn a_case_over_the_default_budget_is_declined() {
    let dir = project("budget-default", "costly.av", "");
    let out = verify(&dir, &[]);
    let text = stdout_of(&out);

    assert!(
        text.contains("? countdown      1/2 passed (1 not answered)"),
        "{}",
        format_output(&out)
    );
    assert!(
        text.contains("fail[verify-declined]: case not answered"),
        "{}",
        format_output(&out)
    );
    assert!(
        text.contains("exceeded its step budget of 1000000 steps"),
        "{}",
        format_output(&out)
    );
    // The repair is a project decision, and the diagnostic says which one.
    assert!(
        text.contains(r#"[[verify.costly]] fn = "countdown""#),
        "{}",
        format_output(&out)
    );
}

#[test]
fn a_costly_entry_lets_the_same_case_run_and_names_what_it_bought() {
    let dir = project("budget-raised", "costly.av", RAISED);
    let out = verify(&dir, &[]);
    let text = stdout_of(&out);

    assert!(
        text.contains("✓ countdown      2/2"),
        "{}",
        format_output(&out)
    );
    assert!(
        text.contains(
            r#"countdown case 2: 3.2M steps (limit 50M, aver.toml [[verify.costly]] fn = "countdown")"#
        ),
        "the report must say what the raised budget bought:\n{}",
        format_output(&out)
    );
    assert_eq!(out.status.code(), Some(0), "{}", format_output(&out));

    // And the same fact travels in the JSON, keyed per block.
    let json = stdout_of(&verify(&dir, &["--json"]));
    assert!(json.contains(r#""costly_cases""#), "{json}");
    assert!(json.contains(r#""raised_by":"countdown""#), "{json}");
    assert!(json.contains(r#""limit":50000000"#), "{json}");
}

#[test]
fn a_raised_budget_that_is_still_too_small_declines_and_says_who_raised_it() {
    let dir = project(
        "budget-too-small",
        "costly.av",
        r#"
[[verify.costly]]
fn         = "countdown"
step-limit = 2000000
reason     = "raised once, and not far enough"
"#,
    );
    let out = verify(&dir, &[]);
    let text = stdout_of(&out);
    assert!(text.contains("1 not answered"), "{}", format_output(&out));
    assert!(
        text.contains(r#"raised by aver.toml [[verify.costly]] fn = "countdown""#),
        "a reader must be able to tell 'raise it' from 'it is already raised':\n{}",
        format_output(&out)
    );
}

// ── c. malformed entries are config errors ──────────────────────────────

#[test]
fn a_costly_entry_without_fn_reason_or_step_limit_is_a_config_error() {
    let cases: [(&str, &str, &str); 4] = [
        (
            "no-fn",
            "[[verify.costly]]\nreason = \"expensive\"\nstep-limit = 50000000\n",
            "requires a string `fn`",
        ),
        (
            "no-reason",
            "[[verify.costly]]\nfn = \"countdown\"\nstep-limit = 50000000\n",
            "say why this case is expected to be expensive",
        ),
        (
            "blank-reason",
            "[[verify.costly]]\nfn = \"countdown\"\nreason = \"  \"\nstep-limit = 50000000\n",
            "`reason` must not be empty",
        ),
        (
            "no-step-limit",
            "[[verify.costly]]\nfn = \"countdown\"\nreason = \"expensive\"\n",
            "requires a positive integer `step-limit`",
        ),
    ];
    for (name, toml, expected) in cases {
        let dir = project(&format!("budget-{name}"), "plain.av", toml);
        let out = verify(&dir, &[]);
        let text = format_output(&out);
        assert!(
            text.contains(expected),
            "[[verify.costly]] {name} must say `{expected}`:\n{text}"
        );
        assert_ne!(out.status.code(), Some(0), "{text}");
    }
}

// ── d. an entry that raised nothing is reported ─────────────────────────

#[test]
fn a_costly_entry_that_raised_nothing_is_reported_as_stale() {
    let dir = project(
        "budget-stale",
        "costly.av",
        r#"
[[verify.costly]]
fn         = "countdown"
step-limit = 50000000
reason     = "the case counts down from 400,000"

[[verify.costly]]
fn         = "checkScript"
files      = ["domain/gone*.av"]
step-limit = 50000000
reason     = "points at a path this project no longer has"

[[verify.costly]]
fn         = "noSuchFunction"
step-limit = 50000000
reason     = "points at a fn this project no longer has"
"#,
    );
    let out = verify(&dir, &[]);
    let stderr = String::from_utf8_lossy(&out.stderr).to_string();

    assert!(
        stderr.contains(r#"fn = "checkScript""#) && stderr.contains("the path may be stale"),
        "{}",
        format_output(&out)
    );
    assert!(
        stderr.contains(r#"fn = "noSuchFunction""#) && stderr.contains("the fn may be stale"),
        "{}",
        format_output(&out)
    );
    // The entry that did its job is not scolded, and staleness never changes
    // the verdict.
    assert!(
        !stderr.contains(r#"fn = "countdown""#),
        "{}",
        format_output(&out)
    );
    assert_eq!(out.status.code(), Some(0), "{}", format_output(&out));
}

// ── e. a decline is its own count, and it fails the run ─────────────────

#[test]
fn a_decline_is_counted_as_neither_a_pass_nor_a_failure() {
    let dir = project("budget-counted", "costly.av", "");
    let out = verify(&dir, &[]);
    let text = stdout_of(&out);

    assert!(
        text.contains("Summary: 1 module | 1 block | 1/2 cases passed | 0 failed | 1 not answered"),
        "{}",
        format_output(&out)
    );
    assert_eq!(
        out.status.code(),
        Some(1),
        "a run that did not check something must not exit zero:\n{}",
        format_output(&out)
    );

    let json = verify(&dir, &["--json"]);
    let json_text = stdout_of(&json);
    assert!(
        json_text.contains(r#""declined":1"#),
        "{}",
        format_output(&json)
    );
    assert!(
        json_text.contains(r#""cases_passed":1,"cases_failed":0,"cases_declined":1"#),
        "{}",
        format_output(&json)
    );
    assert!(
        json_text.contains(r#""slug":"verify-declined""#),
        "{}",
        format_output(&json)
    );
}

/// `aver audit` is the single-shot CI gate, so a decline has to reach it —
/// under its own count, and failing the run.
#[test]
fn audit_reports_a_decline_under_its_own_count_and_fails() {
    let dir = project("budget-audit", "costly.av", "");
    let out = run_aver_in(&dir, &["audit", "main.av", "--module-root", "."]);
    let text = stdout_of(&out);
    assert!(
        text.contains("? verify countdown  1/2 passed, 1 not answered"),
        "{}",
        format_output(&out)
    );
    assert!(
        text.contains("0 check errors | 0 verify failures | 1 verify not answered"),
        "a decline is not a verify failure, and it is not nothing:\n{}",
        format_output(&out)
    );
    assert_eq!(out.status.code(), Some(1), "{}", format_output(&out));

    let json = run_aver_in(&dir, &["audit", "main.av", "--module-root", ".", "--json"]);
    assert!(
        stdout_of(&json).contains(r#""verify_declined":1"#),
        "{}",
        format_output(&json)
    );

    // And an audit with nothing declined still emits the summary it always
    // did, key for key.
    let clean = project("budget-audit-clean", "plain.av", "");
    let clean_json = run_aver_in(
        &clean,
        &["audit", "main.av", "--module-root", ".", "--json"],
    );
    assert!(
        !stdout_of(&clean_json).contains("verify_declined"),
        "{}",
        format_output(&clean_json)
    );
}

// ── f. the proof lane refuses instead of pinning the source RHS ─────────

#[test]
fn the_proof_lane_declines_a_case_verify_did_not_answer() {
    let dir = project("budget-proof", "costly.av", "");
    let out_dir = dir.join("out");
    let out = run_aver_in(
        &dir,
        &[
            "proof",
            "main.av",
            "--module-root",
            ".",
            "-o",
            out_dir.to_str().expect("utf-8 path"),
        ],
    );
    assert!(out.status.success(), "{}", format_output(&out));

    let lean = std::fs::read_to_string(out_dir.join("Costly.lean")).expect("emitted Lean module");
    // The answered case keeps its theorem.
    assert!(lean.contains("example : countdown 10 0 = 10"), "{lean}");
    // The unanswered one gets a refusal, not `impl(sample) = <the author's
    // own expected expression>` — the shape ground-truth literalization
    // exists to prevent, and the one that drops out exactly on big inputs.
    assert!(
        !lean.contains("countdown 400000 0 = 400000"),
        "a declined case must not be exported as a theorem:\n{lean}"
    );
    assert!(
        lean.contains("-- verify countdown case 2:")
            && lean.contains("exceeded its step budget")
            && lean.contains("no theorem emitted"),
        "{lean}"
    );

    // And it is charged, not merely omitted.
    let checked = run_aver_in(
        &dir,
        &[
            "proof",
            "main.av",
            "--module-root",
            ".",
            "-o",
            out_dir.to_str().expect("utf-8 path"),
            "--check-json",
        ],
    );
    let json = stdout_of(&checked);
    assert!(json.contains(r#""declined":1"#), "{json}");
    assert!(
        json.contains(r#""claim":"countdown""#) && json.contains("exceeded its step budget"),
        "{json}"
    );
    assert!(json.contains(r#""passed":false"#), "{json}");
}

#[test]
fn the_proof_lane_pins_the_same_case_once_the_budget_lets_it_run() {
    let dir = project("budget-proof-raised", "costly.av", RAISED);
    let out_dir = dir.join("out");
    let out = run_aver_in(
        &dir,
        &[
            "proof",
            "main.av",
            "--module-root",
            ".",
            "-o",
            out_dir.to_str().expect("utf-8 path"),
        ],
    );
    assert!(out.status.success(), "{}", format_output(&out));
    let lean = std::fs::read_to_string(out_dir.join("Costly.lean")).expect("emitted Lean module");
    assert!(
        lean.contains("countdown 400000 0 = 400000"),
        "with the budget raised the case runs, so it is pinned to the value the VM computed:\n{lean}"
    );
    assert!(!lean.contains("no theorem emitted"), "{lean}");
}

// ── g. the two lanes agree about what is runnable ───────────────────────

/// The wasm-gc lane derives its per-case fuel from the same project budget,
/// through one documented factor, so a case is runnable on both lanes or on
/// neither. Before that, the wasm side carried its own constant — ten times
/// larger, and describing itself as symmetric.
#[cfg(feature = "wasm")]
#[test]
fn the_vm_and_wasm_gc_lanes_agree_about_which_cases_are_runnable() {
    let declined = project("budget-lanes-default", "costly.av", "");
    let vm = verify(&declined, &[]);
    let wasm = verify(&declined, &["--wasm-gc"]);
    assert!(
        stdout_of(&vm).contains("1 not answered"),
        "{}",
        format_output(&vm)
    );
    assert!(
        stdout_of(&wasm).contains("1 not answered"),
        "the wasm-gc lane must decline what the VM lane declines:\n{}",
        format_output(&wasm)
    );

    let raised = project("budget-lanes-raised", "costly.av", RAISED);
    let vm = verify(&raised, &[]);
    let wasm = verify(&raised, &["--wasm-gc"]);
    assert_eq!(vm.status.code(), Some(0), "{}", format_output(&vm));
    assert_eq!(
        wasm.status.code(),
        Some(0),
        "the wasm-gc lane must run what the VM lane runs:\n{}",
        format_output(&wasm)
    );
}

// ── h. the case-count ceiling ───────────────────────────────────────────

#[test]
fn a_given_domain_over_the_ceiling_still_fails_at_parse_time() {
    let dir = project("budget-cases-default", "big_domain.av", "");
    let out = verify(&dir, &[]);
    let text = format_output(&out);
    assert!(
        text.contains("Law verify expands to 12000 cases (max 10000)"),
        "{text}"
    );
    assert_ne!(out.status.code(), Some(0), "{text}");
}

#[test]
fn max_cases_lets_a_project_declare_a_larger_domain() {
    let dir = project(
        "budget-cases-raised",
        "big_domain.av",
        "[verify]\nmax-cases = 40000\n",
    );
    let out = verify(&dir, &[]);
    let text = stdout_of(&out);
    assert!(
        text.contains("✓ identity law fixpoint      12000/12000"),
        "{}",
        format_output(&out)
    );
    assert_eq!(out.status.code(), Some(0), "{}", format_output(&out));
}

#[test]
fn a_domain_over_the_raised_ceiling_fails_naming_the_raised_number() {
    let dir = temp_dir("budget-cases-over");
    std::fs::write(
        dir.join("main.av"),
        "module Over\n    intent =\n        \"A domain one case past the ceiling this project declared.\"\n    exposes [identity]\n    effects []\n\nfn identity(n: Int) -> Int\n    ? \"n itself.\"\n    n\n\nverify identity law fixpoint\n    given n: Int = 1..40001\n    identity(n) => n\n",
    )
    .expect("stage the module");
    std::fs::write(dir.join("aver.toml"), "[verify]\nmax-cases = 40000\n")
        .expect("stage aver.toml");
    let out = verify(&dir, &[]);
    let text = format_output(&out);
    assert!(
        text.contains("Law verify expands to 40001 cases (max 40000)"),
        "the message must stay honest about the count:\n{text}"
    );
}

/// `max-cases` reaches the parse of the user's project files and nothing
/// else. The compiler re-parses synthesized source in a dozen internal
/// places — TCO hoists among them — and every one of those keeps the
/// compiled default, because none of them parses a `given` domain.
#[test]
fn the_ceiling_does_not_leak_into_internal_re_parses() {
    use aver::config::DEFAULT_VERIFY_MAX_CASES;

    let source =
        std::fs::read_to_string(repo_root().join("tests/fixtures/verify_step_budget/costly.av"))
            .expect("read the fixture");

    // The fixture's `countdown` is tail-recursive, so the TCO pass rewrites
    // it and re-parses its own synthesized source through `Parser::new`.
    let mut items =
        aver::source::parse_source_with_verify_max_cases(&source, 40_000).expect("fixture parses");
    aver::ir::pipeline::tco(&mut items);
    assert!(
        !items.is_empty(),
        "the TCO pass must not lose the program it re-parsed"
    );

    // And the plain entry point — the one every internal caller uses — is
    // still the compiled ceiling.
    assert_eq!(DEFAULT_VERIFY_MAX_CASES, 10_000);
    let over = "module M\n    effects []\n\nfn f(x: Int) -> Int\n    ? \"toy\"\n    x\n\nverify f law big\n    given x: Int = 1..10001\n    f(x) => x\n";
    let error = aver::source::parse_source(over).expect_err("the default ceiling still holds");
    assert!(error.contains("max 10000"), "{error}");
}

// ── i. the fuzz guard, asserted ─────────────────────────────────────────

/// The fuzz targets call the runner directly with `config: None` and never
/// read an `aver.toml`, so their budget is whatever no-config resolves to.
/// If that stops being 1M, AFL's hang detector replaces the step limit as
/// the thing that stops a tail-recursive goto-loop.
#[test]
fn the_fuzz_path_runs_at_exactly_one_million_steps() {
    use aver::config::DEFAULT_VERIFY_STEP_LIMIT;
    use aver::diagnostics::vm_verify::CaseBudget;

    assert_eq!(DEFAULT_VERIFY_STEP_LIMIT, 1_000_000);
    let budget = CaseBudget::resolve(None, "countdown", "main.av");
    assert_eq!(budget.limit, 1_000_000);
    assert_eq!(budget.raised_by, None);
}

/// The whole point of the fuzz guard: with no config, the expensive case is
/// declined, whatever any `aver.toml` in any other directory says.
#[test]
fn the_runner_with_no_config_declines_the_expensive_case() {
    use aver::checker::VerifyCaseOutcome;

    let source =
        std::fs::read_to_string(repo_root().join("tests/fixtures/verify_step_budget/costly.av"))
            .expect("read the fixture");
    let items = aver::source::parse_source(&source).expect("fixture parses");
    let results =
        aver::diagnostics::vm_verify::run_verify_for_items_vm(items, None, None, "costly.av")
            .expect("the runner accepts the fixture");

    let declined: Vec<&VerifyCaseOutcome> = results
        .iter()
        .flat_map(|r| &r.case_results)
        .map(|c| &c.outcome)
        .filter(|o| matches!(o, VerifyCaseOutcome::Declined { .. }))
        .collect();
    assert_eq!(declined.len(), 1, "{:?}", declined);
    let VerifyCaseOutcome::Declined { limit, steps, .. } = declined[0] else {
        unreachable!("filtered above")
    };
    assert_eq!(*limit, 1_000_000);
    assert!(*steps >= 1_000_000, "steps = {steps}");
}

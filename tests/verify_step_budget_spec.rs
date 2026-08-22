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
fn a_costly_entry_without_fn_reason_or_a_dial_is_a_config_error() {
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
            "no-dial",
            "[[verify.costly]]\nfn = \"countdown\"\nreason = \"expensive\"\n",
            "raises nothing — set `step-limit`, `max-cases`, or both",
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

// ── h-bis. the case ceiling, per function ───────────────────────────────

/// Both dials live in both places. The ceiling is checked per verify block,
/// a block belongs to exactly one function, and a `[[verify.costly]]` entry
/// already names a function and a set of files — so `max-cases` scopes
/// itself there under exactly the rule `step-limit` already uses.
#[test]
fn a_costly_entry_raises_max_cases_for_the_function_it_names() {
    let dir = project(
        "budget-cases-costly",
        "big_domain.av",
        r#"
[verify]
max-cases = 10000

[[verify.costly]]
fn         = "identity"
files      = ["main.av"]
max-cases  = 40000
reason     = "the fixpoint law is declared over twelve thousand points"
"#,
    );
    let out = verify(&dir, &[]);
    assert!(
        stdout_of(&out).contains("✓ identity law fixpoint      12000/12000"),
        "{}",
        format_output(&out)
    );
    assert_eq!(out.status.code(), Some(0), "{}", format_output(&out));
}

#[test]
fn a_domain_over_the_entrys_ceiling_fails_naming_the_entrys_number() {
    let dir = temp_dir("budget-cases-costly-over");
    std::fs::write(
        dir.join("main.av"),
        "module Over\n    intent =\n        \"A domain one case past the ceiling one entry declared.\"\n    exposes [identity]\n    effects []\n\nfn identity(n: Int) -> Int\n    ? \"n itself.\"\n    n\n\nverify identity law fixpoint\n    given n: Int = 1..40001\n    identity(n) => n\n",
    )
    .expect("stage the module");
    std::fs::write(
        dir.join("aver.toml"),
        "[[verify.costly]]\nfn = \"identity\"\nmax-cases = 40000\nreason = \"the fixpoint law is wide\"\n",
    )
    .expect("stage aver.toml");
    let out = verify(&dir, &[]);
    let text = format_output(&out);
    assert!(
        text.contains("Law verify expands to 40001 cases (max 40000)"),
        "the message must name the ceiling the entry asked for:\n{text}"
    );
    assert_ne!(out.status.code(), Some(0), "{text}");
}

#[test]
fn a_costly_entry_whose_files_do_not_match_leaves_the_project_ceiling() {
    let dir = project(
        "budget-cases-elsewhere",
        "big_domain.av",
        r#"
[[verify.costly]]
fn         = "identity"
files      = ["domain/*.av"]
max-cases  = 40000
reason     = "the wide corpus lives under domain/, and this file is not it"
"#,
    );
    let out = verify(&dir, &[]);
    let text = format_output(&out);
    assert!(
        text.contains("Law verify expands to 12000 cases (max 10000)"),
        "an entry whose globs miss the file must not move its ceiling:\n{text}"
    );
    assert_ne!(out.status.code(), Some(0), "{text}");
}

/// An entry may raise either dial. One that names only `max-cases` is a
/// valid entry and leaves the step budget exactly where it was, so the
/// expensive case is still declined.
#[test]
fn a_costly_entry_may_raise_only_max_cases() {
    let dir = project(
        "budget-cases-only",
        "costly.av",
        r#"
[[verify.costly]]
fn         = "countdown"
max-cases  = 40000
reason     = "this corpus is wide, not slow"
"#,
    );
    let out = verify(&dir, &[]);
    let text = stdout_of(&out);
    assert!(
        text.contains("Summary: 1 module | 1 block | 1/2 cases passed | 0 failed | 1 not answered"),
        "raising the ceiling must not raise the step budget:\n{}",
        format_output(&out)
    );
}

/// Staleness follows the dials: an entry that moved the ceiling of a block
/// this run parsed did its job, and one that names a fn nothing has is
/// still reported.
#[test]
fn an_entry_that_only_raises_max_cases_is_not_reported_stale() {
    let dir = project(
        "budget-cases-stale",
        "big_domain.av",
        r#"
[[verify.costly]]
fn         = "identity"
max-cases  = 40000
reason     = "the fixpoint law is declared over twelve thousand points"

[[verify.costly]]
fn         = "noSuchFunction"
max-cases  = 40000
reason     = "points at a fn this project no longer has"
"#,
    );
    let out = verify(&dir, &[]);
    let stderr = String::from_utf8_lossy(&out.stderr).to_string();

    assert!(
        !stderr.contains(r#"fn = "identity""#),
        "the entry that moved this block's ceiling is not stale:\n{}",
        format_output(&out)
    );
    assert!(
        stderr.contains(r#"fn = "noSuchFunction""#) && stderr.contains("the fn may be stale"),
        "{}",
        format_output(&out)
    );
    assert_eq!(out.status.code(), Some(0), "{}", format_output(&out));
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

// ── h-ter. one tie-break, shared by both dials ──────────────────────────

/// Two entries naming the same fn, the larger ceiling written second. Under
/// first-match-wins the narrow shard would govern a block it was never about,
/// and moving either entry up the file would change the answer — the order of
/// `aver.toml` would become part of the meaning of the project. The most
/// permissive matching entry wins instead, for the case ceiling exactly as
/// for the step budget.
const CEILING_NARROW_FIRST: &str = r#"
[[verify.costly]]
fn         = "identity"
max-cases  = 11000
reason     = "the narrow shard of the fixpoint corpus"

[[verify.costly]]
fn         = "identity"
max-cases  = 40000
reason     = "and the wide shard, which is the corpus this file declares"
"#;

const CEILING_WIDE_FIRST: &str = r#"
[[verify.costly]]
fn         = "identity"
max-cases  = 40000
reason     = "and the wide shard, which is the corpus this file declares"

[[verify.costly]]
fn         = "identity"
max-cases  = 11000
reason     = "the narrow shard of the fixpoint corpus"
"#;

#[test]
fn the_largest_matching_max_cases_wins_even_when_it_is_written_last() {
    let dir = project(
        "budget-cases-tiebreak",
        "big_domain.av",
        CEILING_NARROW_FIRST,
    );
    let out = verify(&dir, &[]);
    assert!(
        stdout_of(&out).contains("✓ identity law fixpoint      12000/12000"),
        "the entry that grants the most room governs the block, wherever it sits in the file:\n{}",
        format_output(&out)
    );
    assert_eq!(out.status.code(), Some(0), "{}", format_output(&out));
}

/// The same pair for `step-limit`, which already resolved this way: it is
/// here to pin the two dials to one another, so a later change to either
/// tie-break has to move both or fail.
#[test]
fn the_largest_matching_step_limit_wins_even_when_it_is_written_last() {
    let dir = project(
        "budget-limit-tiebreak",
        "costly.av",
        r#"
[[verify.costly]]
fn         = "countdown"
step-limit = 2000000
reason     = "the ordinary cases of this fn"

[[verify.costly]]
fn         = "countdown"
step-limit = 50000000
reason     = "and the four-hundred-thousand-step one this file declares"
"#,
    );
    let out = verify(&dir, &[]);
    let text = stdout_of(&out);
    assert!(
        text.contains("✓ countdown      2/2"),
        "{}",
        format_output(&out)
    );
    assert!(
        text.contains("limit 50M"),
        "the budget in force must be the most permissive one, not the first:\n{}",
        format_output(&out)
    );
    assert_eq!(out.status.code(), Some(0), "{}", format_output(&out));
}

/// Order-independence is the point of the rule, so it is asserted directly:
/// the same two entries in the other order produce the same run, byte for
/// byte. Each entry is an independent statement about one function, and a
/// reader who finds one does not have to scan the rest of the list.
#[test]
fn reordering_two_matching_entries_changes_nothing() {
    let narrow_first = project(
        "budget-cases-order-narrow",
        "big_domain.av",
        CEILING_NARROW_FIRST,
    );
    let wide_first = project(
        "budget-cases-order-wide",
        "big_domain.av",
        CEILING_WIDE_FIRST,
    );
    let a = verify(&narrow_first, &[]);
    let b = verify(&wide_first, &[]);

    assert_eq!(
        a.status.code(),
        b.status.code(),
        "the exit code depended on the order of the entries:\nnarrow first:\n{}\nwide first:\n{}",
        format_output(&a),
        format_output(&b)
    );
    assert_eq!(
        stdout_of(&a),
        stdout_of(&b),
        "the report depended on the order of the entries:\nnarrow first:\n{}\nwide first:\n{}",
        format_output(&a),
        format_output(&b)
    );
    assert_eq!(a.status.code(), Some(0), "{}", format_output(&a));
}

/// An entry that matched a live block and was out-granted by another entry
/// is not stale. Staleness asks whether an entry raises anything over the
/// project default and whether it found a block at all — never which of two
/// entries won, because losing a tie-break says nothing about whether the
/// declaration is still true of the project.
#[test]
fn an_entry_out_granted_by_another_is_not_reported_stale() {
    let dir = project(
        "budget-subsumed",
        "costly.av",
        r#"
[[verify.costly]]
fn         = "countdown"
step-limit = 2000000
max-cases  = 11000
reason     = "the ordinary cases of this fn, subsumed by the entry below"

[[verify.costly]]
fn         = "countdown"
step-limit = 50000000
max-cases  = 40000
reason     = "and the four-hundred-thousand-step one this file declares"
"#,
    );
    let out = verify(&dir, &[]);
    let stderr = String::from_utf8_lossy(&out.stderr).to_string();

    assert!(
        !stderr.contains("[[verify.costly]]"),
        "a subsumed entry is a live declaration, not a stale one:\n{}",
        format_output(&out)
    );
    assert_eq!(out.status.code(), Some(0), "{}", format_output(&out));
}

// ── h-quater. an entry only ever raises ─────────────────────────────────

/// `[[verify.costly]]` says "this case is expensive, give it room". A value
/// below the number already in force says the opposite — a ratchet keeping
/// something cheap — which is a different feature under a different name.
/// Under a section called `costly` it is almost always a typo, so it is
/// refused at load, where it costs nothing.
#[test]
fn a_costly_entry_below_the_number_in_force_is_a_config_error() {
    let cases: [(&str, &str, &str); 4] = [
        (
            "limit-under-compiled-default",
            "[[verify.costly]]\nfn = \"countdown\"\nstep-limit = 500000\nreason = \"expensive\"\n",
            "`step-limit` = 500000 raises nothing — the step budget in force is 1000000",
        ),
        (
            "limit-equal-to-the-project-default",
            "[verify]\nstep-limit = 5000000\n\n[[verify.costly]]\nfn = \"countdown\"\nstep-limit = 5000000\nreason = \"expensive\"\n",
            "`step-limit` = 5000000 raises nothing — the step budget in force is 5000000",
        ),
        (
            "cases-under-compiled-default",
            "[[verify.costly]]\nfn = \"identity\"\nmax-cases = 500\nreason = \"wide\"\n",
            "`max-cases` = 500 raises nothing — the case ceiling in force is 10000",
        ),
        (
            "cases-under-the-project-ceiling",
            "[verify]\nmax-cases = 40000\n\n[[verify.costly]]\nfn = \"identity\"\nmax-cases = 20000\nreason = \"wide\"\n",
            "`max-cases` = 20000 raises nothing — the case ceiling in force is 40000",
        ),
    ];
    for (name, toml, expected) in cases {
        let dir = project(&format!("budget-lowering-{name}"), "plain.av", toml);
        let out = verify(&dir, &[]);
        let text = format_output(&out);
        assert!(
            text.contains(expected),
            "[[verify.costly]] {name} must say `{expected}`:\n{text}"
        );
        assert_ne!(out.status.code(), Some(0), "{text}");
    }
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

    // The `--hostile` expander asks the same question about the ceiling, and
    // with no config it gets the compiled-in number too.
    assert_eq!(
        aver::diagnostics::vm_verify::max_cases_for(None, "countdown", "main.av"),
        aver::config::DEFAULT_VERIFY_MAX_CASES
    );
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

// ── j. the ceiling reaches every door, not just `verify` ────────────────

/// A ceiling only one command obeys is not project policy. `[verify]
/// max-cases` reached the loader that `aver verify` walks and nothing else,
/// so a project that raised it got a file `aver verify` accepted and no other
/// command could touch: `check`, `run`, `compile` and `proof` parsed the same
/// source under the compiled-in ten thousand and refused it — quoting ten
/// thousand while the project had written forty.
const RAISED_CEILING: &str = "[verify]\nmax-cases = 40000\n";

/// A module one case past the ceiling this project declares, written inline
/// because the number in the source and the number in `aver.toml` are the
/// subject.
const OVER_THE_PROJECT_CEILING: &str = "module Over\n    intent =\n        \"A domain one case past the ceiling this project declared, in a module\"\n        \"that also has an entry, so every door has something to refuse.\"\n    exposes [identity, main]\n    effects []\n\nfn identity(n: Int) -> Int\n    ? \"n itself.\"\n    n\n\nverify identity law fixpoint\n    given n: Int = 1..40001\n    identity(n) => n\n\nfn main() -> Int\n    ? \"The entry hands back one point of the domain.\"\n    identity(1)\n";

/// A program well under every ceiling: what a project with no `[verify]`
/// section has to keep seeing, byte for byte.
const UNDER_EVERY_CEILING: &str = "module Small\n    intent =\n        \"A law over fifty points and an entry, both far below anything any\"\n        \"budget could refuse.\"\n    exposes [identity, main]\n    effects []\n\nfn identity(n: Int) -> Int\n    ? \"n itself.\"\n    n\n\nverify identity law fixpoint\n    given n: Int = 1..50\n    identity(n) => n\n\nfn main() -> Int\n    ? \"The entry hands back one point of the domain.\"\n    identity(1)\n\nverify main\n    main() => 1\n";

/// The four doors the report named, each run against `main.av` in `dir`.
fn doors(dir: &Path) -> Vec<(&'static str, std::process::Output)> {
    vec![
        (
            "check",
            run_aver_in(dir, &["check", "main.av", "--module-root", "."]),
        ),
        (
            "run",
            run_aver_in(dir, &["run", "main.av", "--module-root", "."]),
        ),
        (
            "compile",
            run_aver_in(
                dir,
                &[
                    "compile",
                    "main.av",
                    "--target",
                    "rust",
                    "-o",
                    "out-rust",
                    "--module-root",
                    ".",
                ],
            ),
        ),
        (
            "proof",
            run_aver_in(
                dir,
                &["proof", "main.av", "-o", "out-proof", "--module-root", "."],
            ),
        ),
    ]
}

#[test]
fn a_raised_ceiling_reaches_check_run_compile_and_proof() {
    let dir = project(
        "budget-doors-raised",
        "wide_domain_program.av",
        RAISED_CEILING,
    );

    for (name, out) in doors(&dir) {
        let text = format_output(&out);
        assert!(
            !text.contains("expands to 12000 cases"),
            "`aver {name}` must parse a domain the project declared legal:\n{text}"
        );
        assert_eq!(
            out.status.code(),
            Some(0),
            "`aver {name}` must accept the file `aver verify` accepts:\n{text}"
        );
    }

    // And the door that was already wired still agrees with the other four.
    let out = verify(&dir, &[]);
    assert_eq!(out.status.code(), Some(0), "{}", format_output(&out));
}

/// Every other command that reads one of the user's `.av` files off disk is
/// the same door. None of them may hold a second opinion about how many
/// cases the file is allowed to declare.
#[test]
fn a_raised_ceiling_reaches_the_remaining_doors() {
    let dir = project(
        "budget-doors-rest",
        "wide_domain_program.av",
        RAISED_CEILING,
    );

    let runs = [
        (
            "audit",
            run_aver_in(&dir, &["audit", "main.av", "--module-root", "."]),
        ),
        ("format", run_aver_in(&dir, &["format", "main.av"])),
        (
            "shape",
            run_aver_in(&dir, &["shape", "main.av", "--module-root", "."]),
        ),
        (
            "context",
            run_aver_in(&dir, &["context", "main.av", "--module-root", "."]),
        ),
        (
            "why",
            run_aver_in(&dir, &["why", "main.av", "--module-root", "."]),
        ),
        (
            "capabilities",
            run_aver_in(&dir, &["capabilities", "main.av", "--module-root", "."]),
        ),
    ];

    for (name, out) in runs {
        let text = format_output(&out);
        assert!(
            !text.contains("expands to 12000 cases"),
            "`aver {name}` must parse a domain the project declared legal:\n{text}"
        );
    }
}

/// The message has to name the ceiling that actually applied. A project that
/// wrote forty thousand and declared forty thousand and one cases is told
/// about forty thousand — hearing "max 10000" back would send it to change a
/// setting that is already right.
#[test]
fn a_domain_over_the_projects_ceiling_names_the_projects_number_at_every_door() {
    let dir = temp_dir("budget-doors-over");
    std::fs::write(dir.join("main.av"), OVER_THE_PROJECT_CEILING).expect("stage the module");
    std::fs::write(dir.join("aver.toml"), RAISED_CEILING).expect("stage aver.toml");

    for (name, out) in doors(&dir) {
        let text = format_output(&out);
        assert!(
            text.contains("expands to 40001 cases (max 40000)"),
            "`aver {name}` must name the ceiling that applied:\n{text}"
        );
        assert!(
            !text.contains("max 10000"),
            "`aver {name}` must not quote the compiled-in number at a project that moved it:\n{text}"
        );
        assert_ne!(
            out.status.code(),
            Some(0),
            "`aver {name}` must still refuse the file:\n{text}"
        );
    }
}

/// The no-op claim, at the doors as well as at `verify`: a project with no
/// `[verify]` section gets byte for byte what a project with no `aver.toml`
/// at all gets — and no `aver.toml` at all is what every project had before
/// any of this existed.
#[test]
fn a_project_with_no_verify_section_is_byte_identical_at_every_door() {
    let bare = temp_dir("budget-doors-bare");
    std::fs::write(bare.join("main.av"), UNDER_EVERY_CEILING).expect("stage the module");

    let empty = temp_dir("budget-doors-empty");
    std::fs::write(empty.join("main.av"), UNDER_EVERY_CEILING).expect("stage the module");
    std::fs::write(empty.join("aver.toml"), "").expect("stage an aver.toml with no [verify]");

    for ((name, without), (_, with)) in doors(&bare).into_iter().zip(doors(&empty)) {
        assert_eq!(
            without.status.code(),
            with.status.code(),
            "`aver {name}` changed its exit code:\nwithout aver.toml:\n{}\nwith an empty one:\n{}",
            format_output(&without),
            format_output(&with)
        );
        assert_eq!(
            without.stdout,
            with.stdout,
            "`aver {name}` changed its stdout:\nwithout aver.toml:\n{}\nwith an empty one:\n{}",
            format_output(&without),
            format_output(&with)
        );
        assert_eq!(
            without.stderr,
            with.stderr,
            "`aver {name}` changed its stderr:\nwithout aver.toml:\n{}\nwith an empty one:\n{}",
            format_output(&without),
            format_output(&with)
        );
    }
}

/// And the compiled-in ceiling is still the answer when nothing raises it,
/// at every door: a project with no `[verify]` section is refused the same
/// twelve-thousand-case domain, quoting the same ten thousand.
#[test]
fn without_a_raise_every_door_still_holds_the_compiled_in_ceiling() {
    let dir = project("budget-doors-default", "wide_domain_program.av", "");

    for (name, out) in doors(&dir) {
        let text = format_output(&out);
        assert!(
            text.contains("expands to 12000 cases (max 10000)"),
            "`aver {name}` must keep the built-in ceiling when nothing moves it:\n{text}"
        );
        assert_ne!(out.status.code(), Some(0), "{text}");
    }
}

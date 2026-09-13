//! The program the agent guides cut their coordinator blocks from.
//!
//! `.claude/commands/aver.md` ("Processes, answer modules and the
//! coordinator") and `.claude/commands/aver-tooling.md` ("Processes, answer
//! modules and jobs in `aver.toml`") promise that every `aver` and `toml`
//! block there is cut, as it stands, from a program that checks, verifies and
//! runs. That program is `tests/fixtures/run_guide_example/`; the one
//! hand-driven block comes from `tests/fixtures/yield_spike/`. This suite
//! holds the promise from both ends: the fixture is checked, verified, run and
//! format-checked with the binary under test, and every fenced block of those
//! two sections is a contiguous substring of one fixture file.

#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use aver_cmd::{aver_bin, format_output, repo_root};

use std::fs;
use std::path::PathBuf;
use std::process::{Command, Output};

const EXAMPLE: &str = "tests/fixtures/run_guide_example";
const HAND_DRIVEN: &str = "tests/fixtures/yield_spike/main.av";
const LANGUAGE_GUIDE: &str = ".claude/commands/aver.md";
const TOOLING_GUIDE: &str = ".claude/commands/aver-tooling.md";
const LANGUAGE_SECTION: &str = "### Processes, answer modules and the coordinator";
const TOOLING_SECTION: &str = "### Processes, answer modules and jobs in `aver.toml`";

fn example_dir() -> PathBuf {
    repo_root().join(EXAMPLE)
}

fn aver(args: &[&str]) -> Output {
    let dir = example_dir();
    let mut command = Command::new(aver_bin());
    command.current_dir(&dir);
    command.args(args);
    command.output().expect("aver runs")
}

fn stdout_of(out: &Output) -> String {
    String::from_utf8_lossy(&out.stdout).to_string()
}

fn stderr_of(out: &Output) -> String {
    String::from_utf8_lossy(&out.stderr).to_string()
}

#[test]
fn the_guide_example_checks_without_a_warning() {
    let out = aver(&["check", "main.av", "--module-root", "."]);
    assert!(out.status.success(), "{}", format_output(&out));
    let text = format!("{}{}", stdout_of(&out), stderr_of(&out));
    assert!(
        text.contains("Checked 4 module(s): 4 passed"),
        "{}",
        format_output(&out)
    );
    assert!(
        !text.contains("warning["),
        "the guide example must check clean, so a reader copying it meets no warning:\n{}",
        format_output(&out)
    );
}

#[test]
fn the_guide_example_verifies_with_the_generated_laws() {
    let out = aver(&["verify", "main.av", "--module-root", "."]);
    assert!(out.status.success(), "{}", format_output(&out));
    let text = stdout_of(&out);
    assert!(text.contains("| 0 failed |"), "{}", format_output(&out));
    assert!(
        text.contains("__consumedScoring law aStartedTaskIsNotAskedAgain"),
        "the generated seam law must run beside the program's own:\n{}",
        format_output(&out)
    );
}

#[test]
fn the_guide_example_runs_to_the_score() {
    let out = aver(&["run", "main.av", "--module-root", "."]);
    assert!(out.status.success(), "{}", format_output(&out));
    assert_eq!(
        stdout_of(&out).trim(),
        "scored 60",
        "{}",
        format_output(&out)
    );
}

#[test]
fn the_guide_example_is_formatted() {
    let out = aver(&["format", ".", "--check"]);
    assert!(out.status.success(), "{}", format_output(&out));
}

/// The fenced `aver` and `toml` blocks of the section that starts at
/// `heading` and ends at the next `### ` heading.
fn blocks_of(guide: &str, heading: &str) -> Vec<String> {
    let text = fs::read_to_string(repo_root().join(guide))
        .unwrap_or_else(|error| panic!("read {guide}: {error}"));
    let start = text
        .find(heading)
        .unwrap_or_else(|| panic!("{guide} has no section {heading:?}"));
    let body = &text[start + heading.len()..];
    let end = body.find("\n### ").unwrap_or(body.len());
    let section = &body[..end];

    let mut blocks = Vec::new();
    let mut rest = section;
    while let Some(open) = rest.find("\n```") {
        let after_fence = &rest[open + 4..];
        let (lang, after_lang) = after_fence.split_once('\n').expect("a fence line ends");
        let close = after_lang
            .find("\n```")
            .unwrap_or_else(|| panic!("{guide}: an unclosed ```{lang} fence"));
        if lang == "aver" || lang == "toml" {
            blocks.push(after_lang[..close].to_string());
        }
        rest = &after_lang[close + 4..];
    }
    assert!(
        !blocks.is_empty(),
        "{guide}: {heading:?} has no aver or toml block"
    );
    blocks
}

fn sources() -> Vec<(String, String)> {
    let mut files: Vec<PathBuf> = fs::read_dir(example_dir())
        .expect("read the guide example directory")
        .map(|entry| entry.expect("directory entry").path())
        .collect();
    files.sort();
    files.push(repo_root().join(HAND_DRIVEN));
    files
        .into_iter()
        .map(|path| {
            let text = fs::read_to_string(&path)
                .unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
            (path.display().to_string(), text)
        })
        .collect()
}

fn assert_every_block_is_a_cut(guide: &str, heading: &str) {
    let sources = sources();
    for block in blocks_of(guide, heading) {
        let cut = sources.iter().any(|(_, text)| text.contains(&block));
        assert!(
            cut,
            "{guide}: this block of {heading:?} is not a contiguous cut of any file in \
             {EXAMPLE} or of {HAND_DRIVEN}; edit the fixture first, then paste from it:\n{block}"
        );
    }
}

#[test]
fn every_language_guide_block_is_cut_from_the_example() {
    assert_every_block_is_a_cut(LANGUAGE_GUIDE, LANGUAGE_SECTION);
}

#[test]
fn every_tooling_guide_block_is_cut_from_the_example() {
    assert_every_block_is_a_cut(TOOLING_GUIDE, TOOLING_SECTION);
}

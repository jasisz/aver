//! A law `given`'s annotation pins the element type of every sample it
//! expands into cases.
//!
//! `given xs: List<Int> = [[], [1], [1, 2]]` states the element type once,
//! for the whole domain. The empty sample carries none of its own, and the
//! parser substitutes it into the case bodies verbatim, so the case
//! `allBytes(List.reverse([]))` used to infer `List<T>` for the argument
//! and the call was rejected against `List<Int>` — while the same law
//! without `[]` in the domain checked and ran. Empty is exactly the sample
//! a list law wants (it is the base case of the induction the proof lane
//! then runs), so the whole law had to be written around the hole.
//!
//! The typechecker suite pins the diagnostic and the stamp; this suite is
//! the end-to-end half: the reproducer from the report checks clean, runs
//! under `aver verify`, and the same law without the empty sample behaves
//! exactly as it did.
#![cfg(feature = "runtime")]

#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use aver_cmd::{aver_bin, cleanup, format_output, temp_module};

use std::process::{Command, Output};

/// The reported law: a byte predicate over a reversed list, whose given
/// domain opens with the empty list.
const WITH_EMPTY_SAMPLE: &str = r#"module ReverseKeepsBytes
    intent = "A list law whose given domain contains the empty list."
    effects []

fn allBytes(xs: List<Int>) -> Bool
    ? "Whether every element is a byte, 0 to 255."
    match xs
        [] -> true
        [head, ..tail] -> Bool.and(Bool.and(head >= 0, head <= 255), allBytes(tail))

verify allBytes law reverseKeepsBytes
    given xs: List<Int> = [[], [1], [1, 2]]
    allBytes(List.reverse(xs)) => allBytes(xs)
"#;

fn run(source: &str, prefix: &str, subcommand: &str) -> Output {
    let path = temp_module(prefix, source);
    let out = Command::new(aver_bin())
        .arg(subcommand)
        .arg(&path)
        .env("NO_COLOR", "1")
        .output()
        .expect("expected the `aver` binary to run");
    cleanup(&path);
    out
}

#[test]
fn law_with_an_empty_list_sample_checks_and_verifies() {
    let check = run(WITH_EMPTY_SAMPLE, "aver-law-empty-sample-check", "check");
    assert!(
        check.status.success(),
        "the given's `List<Int>` must type the empty sample: {}",
        format_output(&check)
    );

    let verify = run(WITH_EMPTY_SAMPLE, "aver-law-empty-sample-verify", "verify");
    let report = format_output(&verify);
    assert!(verify.status.success(), "{report}");
    assert!(
        report.contains("3/3"),
        "all three samples — the empty list included — must run: {report}"
    );
}

#[test]
fn law_without_an_empty_list_sample_is_unchanged() {
    // The half of the domain that always worked must keep working, with
    // one case fewer and nothing else moved.
    let source = WITH_EMPTY_SAMPLE.replace("[[], [1], [1, 2]]", "[[1], [1, 2]]");
    let check = run(&source, "aver-law-no-empty-sample-check", "check");
    assert!(check.status.success(), "{}", format_output(&check));

    let verify = run(&source, "aver-law-no-empty-sample-verify", "verify");
    let report = format_output(&verify);
    assert!(verify.status.success(), "{report}");
    assert!(
        report.contains("2/2"),
        "the two non-empty samples must still run: {report}"
    );
}

/// The same law with one more `List.reverse` around the given. `allBytes`
/// pushes its `List<Int>` into the outer call and no further — the inner
/// call sits in a `List<T>` parameter, which fixes nothing — so the
/// annotation is the only thing that can type the sample this deep in.
const NESTED_EMPTY_SAMPLE: &str = r#"module ReverseTwiceKeepsBytes
    intent = "A list law whose sample sits under two polymorphic builtins."
    effects []

fn allBytes(xs: List<Int>) -> Bool
    ? "Whether every element is a byte, 0 to 255."
    match xs
        [] -> true
        [head, ..tail] -> Bool.and(Bool.and(head >= 0, head <= 255), allBytes(tail))

verify allBytes law reverseTwiceKeepsBytes
    given xs: List<Int> = [[], [1], [1, 2]]
    allBytes(List.reverse(List.reverse(xs))) => allBytes(xs)
"#;

#[test]
fn law_with_a_sample_under_nested_builtins_checks_and_verifies() {
    let check = run(NESTED_EMPTY_SAMPLE, "aver-law-nested-sample-check", "check");
    assert!(
        check.status.success(),
        "no expected type reaches the sample here — only the given's annotation can: {}",
        format_output(&check)
    );

    let verify = run(
        NESTED_EMPTY_SAMPLE,
        "aver-law-nested-sample-verify",
        "verify",
    );
    let report = format_output(&verify);
    assert!(verify.status.success(), "{report}");
    assert!(
        report.contains("3/3"),
        "all three samples — the empty list included — must run: {report}"
    );
}

/// The pin is only as good as the annotation it copies. Under consumers
/// that take `List<T>` nothing downstream ever objects to a `List<String>`
/// sample, so a `List<Int>` given that contradicts its own domain has to
/// be rejected where it is written — otherwise `aver check` passes while
/// the expanded case carries an element type the sample never had.
const CONTRADICTING_SAMPLE: &str = r#"module ContradictingSample
    intent = "A law given whose domain contradicts its annotation."
    effects []

fn allBytes(xs: List<Int>) -> Bool
    ? "Whether every element is a byte, 0 to 255."
    match xs
        [] -> true
        [head, ..tail] -> Bool.and(Bool.and(head >= 0, head <= 255), allBytes(tail))

verify allBytes law reverseKeepsLength
    given xs: List<Int> = [[], ["a"]]
    List.len(List.reverse(xs)) => List.len(xs)
"#;

#[test]
fn law_given_whose_domain_contradicts_its_annotation_is_rejected() {
    let check = run(CONTRADICTING_SAMPLE, "aver-law-bad-sample-check", "check");
    let report = format_output(&check);
    assert!(
        !check.status.success(),
        "a `List<String>` sample under a `List<Int>` given must be reported \
         even when every consumer accepts `List<T>`: {report}"
    );
    assert!(
        report.contains("expected Int, got String"),
        "the report must name the contradiction: {report}"
    );
}

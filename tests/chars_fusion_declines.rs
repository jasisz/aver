//! Chars fusion must decline every loop it cannot prove it is stepping,
//! and every character match it cannot prove it is deciding.
//!
//! A missed fusion costs allocations. A WRONG fusion costs the answer:
//! the cursor variant has no list parameter, so any surviving mention of
//! the list is a free identifier that either fails to compile or — when
//! the module happens to bind that name at top level — quietly reads the
//! binding instead; and a loop whose recursive step is a DIFFERENT list
//! walks a different number of characters once the cursor steps for it.
//!
//! Each program below was correct before this pass could see it. The
//! assertion is the answer it has always given. Cross-backend agreement
//! on the shapes that DO fuse is the differential's job
//! (`rust_codegen_differential`); what is pinned here is that these
//! shapes keep their meaning.

use std::process::Command;

fn run_program(name: &str, source: &str) -> String {
    let dir = tempfile::tempdir().expect("tempdir");
    let entry = dir.path().join(format!("{name}.av"));
    std::fs::write(&entry, source).expect("write entry");
    let output = Command::new(env!("CARGO_BIN_EXE_aver"))
        .arg("run")
        .arg(&entry)
        .output()
        .expect("invoke aver");
    assert!(
        output.status.success(),
        "aver run {name} failed:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8_lossy(&output.stdout).trim().to_string()
}

/// The loop measures the list it was handed to decide when to stop. The
/// cursor variant has no list to measure, and the module binds `chars`
/// at top level — which is what turns a dropped parameter from a compile
/// error into a wrong answer.
#[test]
fn a_loop_that_measures_its_list_keeps_its_unfused_answer() {
    let out = run_program(
        "measure",
        r#"module Measure
    intent =
        "Counts characters while more than three remain, with a top-level binding that shares the parameter's name."
    exposes [count]
    effects [Console.print]

chars = ["x", "y", "z", "w", "v", "u"]

fn walk(chars: List<String>, acc: Int) -> Int
    match chars
        [] -> acc
        [head, ..tail] -> match List.len(chars) > 3
            true -> walk(tail, acc + 1)
            false -> acc

fn count(text: String) -> Int
    walk(String.chars(text), 0)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(count("abcde")))
"#,
    );
    // Five characters: the loop counts while more than three are left,
    // so it stops having counted two. Reading the six-element top-level
    // `chars` instead would never stop early and answer 5.
    assert_eq!(out, "2");
}

/// The loop hands the remaining characters to a helper. A cursor cannot
/// be handed to a `List<String>` parameter, and the top-level binding is
/// again what the dropped name would silently resolve to.
#[test]
fn a_loop_that_passes_its_list_along_keeps_its_unfused_answer() {
    let out = run_program(
        "handoff",
        r#"module Handoff
    intent =
        "Asks a helper how much input is left at every step."
    exposes [count]
    effects [Console.print]

tail = ["x", "y"]

fn remaining(xs: List<String>) -> Int
    List.len(xs)

fn walk(chars: List<String>, acc: Int) -> Int
    match chars
        [] -> acc
        [head, ..tail] -> walk(tail, acc + remaining(tail))

fn count(text: String) -> Int
    walk(String.chars(text), 0)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(count("abcd")))
"#,
    );
    // 3 + 2 + 1 + 0. The two-element top-level `tail` would answer 8.
    assert_eq!(out, "6");
}

/// The recursive step is a DIFFERENT list that happens to carry the
/// tail's name. Stepping the cursor on the strength of the name walks
/// the whole string; the loop as written stops after one character.
#[test]
fn a_shadowed_tail_keeps_its_unfused_answer() {
    let out = run_program(
        "shadow",
        r#"module Shadow
    intent =
        "Recurses on an empty list bound under the tail's own name."
    exposes [count]
    effects [Console.print]

fn nothing() -> List<String>
    []

fn walk(chars: List<String>, acc: Int) -> Int
    match chars
        [] -> acc
        [head, ..tail] -> match nothing()
            tail -> walk(tail, acc + 1)

fn count(text: String) -> Int
    walk(String.chars(text), 0)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(count("abcde")))
"#,
    );
    // One step, then the shadowed (empty) tail ends the loop. A cursor
    // that stepped for the name would answer 5.
    assert_eq!(out, "1");
}

/// The list match is not the `[] / [head, ..tail]` pair the cursor
/// replaces: its second arm binds the whole remaining list. There is no
/// cursor form for that binding, and the module binds the name at top
/// level, so carrying the arm across would read six characters where
/// the program reads the ones it was given.
#[test]
fn a_list_match_that_binds_the_whole_list_keeps_its_unfused_answer() {
    let out = run_program(
        "bind_list",
        r#"module BindList
    intent =
        "Measures the whole remaining list in one arm instead of destructuring it."
    exposes [count]
    effects [Console.print]

rest = ["x", "y", "z", "w", "v", "u"]

fn walk(chars: List<String>, acc: Int) -> Int
    match chars
        [] -> acc
        rest -> acc + List.len(rest)

fn count(text: String) -> Int
    walk(String.chars(text), 0)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(count("abc")))
"#,
    );
    // Three characters. The six-element top-level `rest` would answer 6.
    assert_eq!(out, "3");
}

/// Two `String.chars` producers, two parameters. One cursor variant
/// cannot stand in for both lists, and stepping either one for the other
/// walks the wrong string.
#[test]
fn two_producers_into_one_loop_keep_their_unfused_answer() {
    let out = run_program(
        "two_producers",
        r#"module TwoProducers
    intent =
        "Walks two strings at once, counting positions where they agree."
    exposes [agreements]
    effects [Console.print]

fn both(left: List<String>, right: List<String>, acc: Int) -> Int
    match left
        [] -> acc
        [l, ..leftTail] -> match right
            [] -> acc
            [r, ..rightTail] -> match l == r
                true -> both(leftTail, rightTail, acc + 1)
                false -> both(leftTail, rightTail, acc)

fn agreements(a: String, b: String) -> Int
    both(String.chars(a), String.chars(b), 0)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(agreements("abcd", "abxd")))
"#,
    );
    assert_eq!(out, "3");
}

/// The synthesized name is already a function in this module. Taking it
/// would replace what every existing call site meant.
#[test]
fn an_existing_cursor_name_keeps_its_own_meaning() {
    let out = run_program(
        "name_taken",
        r#"module NameTaken
    intent =
        "Already defines a function under the name the pass would synthesize."
    exposes [report]
    effects [Console.print]

fn walk(chars: List<String>, acc: Int) -> Int
    match chars
        [] -> acc
        [head, ..tail] -> walk(tail, acc + 1)

fn walk__cursor(text: String, start: Int, acc: Int) -> Int
    ? "Nothing to do with walking a cursor, and that is the point."
    acc - 100

fn report(text: String) -> String
    "{walk(String.chars(text), 0)}/{walk__cursor(text, 0, 0)}"

fn main() -> Unit
    ! [Console.print]
    Console.print(report("abc"))
"#,
    );
    assert_eq!(out, "3/-100");
}

/// The default arm BINDS the subject. Rewriting the subject to a
/// codepoint would hand that binding an integer where the program put a
/// string.
#[test]
fn a_binding_default_arm_keeps_the_character_it_was_given() {
    let out = run_program(
        "binding_default",
        r#"module BindingDefault
    intent =
        "Upper-cases one letter and echoes anything else in brackets."
    exposes [render]
    effects [Console.print]

fn shout(character: String) -> String
    match character
        "a" -> "A"
        other -> "[{other}]"

fn render() -> String
    "{shout("a")}{shout("q")}{shout("ab")}"

fn main() -> Unit
    ! [Console.print]
    Console.print(render())
"#,
    );
    assert_eq!(out, "A[q][ab]");
}

/// An arm whose literal is more than one character. Its codepoint does
/// not exist, and a rewrite that dropped the arm to reach the ones that
/// do would answer the default where the program answers the arm.
#[test]
fn a_multi_character_arm_keeps_its_unfused_answer() {
    let out = run_program(
        "multi_char",
        r#"module MultiChar
    intent =
        "Ranks words and characters in the same match."
    exposes [render]
    effects [Console.print]

fn rank(word: String) -> Int
    match word
        "ab" -> 1
        "c" -> 2
        _ -> 0

fn render() -> String
    "{rank("ab")}{rank("c")}{rank("z")}"

fn main() -> Unit
    ! [Console.print]
    Console.print(render())
"#,
    );
    assert_eq!(out, "120");
}

/// An arm whose literal is one character but not ASCII. Exactly the same
/// reasoning would make the rewrite exact here, and the recogniser still
/// declines: the arm set this pass claims is the one the probe checked
/// against the builtin over every scalar.
#[test]
fn a_non_ascii_arm_keeps_its_unfused_answer() {
    let out = run_program(
        "non_ascii",
        r#"module NonAscii
    intent =
        "Ranks an accented character alongside an ASCII one."
    exposes [render]
    effects [Console.print]

fn rank(character: String) -> Int
    match character
        "é" -> 1
        "c" -> 2
        _ -> 0

fn render() -> String
    "{rank("é")}{rank("c")}{rank("z")}"

fn main() -> Unit
    ! [Console.print]
    Console.print(render())
"#,
    );
    assert_eq!(out, "120");
}

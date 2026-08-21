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

/// Run a program that must be REJECTED at the front door (the
/// shadowing ban, issue #954); returns stderr for the message pin.
fn run_rejected(name: &str, source: &str) -> String {
    let dir = tempfile::tempdir().expect("tempdir");
    let entry = dir.path().join(format!("{name}.av"));
    std::fs::write(&entry, source).expect("write entry");
    let output = Command::new(env!("CARGO_BIN_EXE_aver"))
        .arg("run")
        .arg(&entry)
        .output()
        .expect("invoke aver");
    assert!(
        !output.status.success(),
        "aver run {name} was expected to be rejected:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8_lossy(&output.stderr).to_string()
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
/// tail's name. This program is a REFUSAL pin and nothing else: the
/// shadowing ban (issue #954) rejects the spelling at the front door,
/// so what it says about the pass is only that the pass never meets
/// this shape in user code. The decline itself — a step on a list that
/// is not the destructured tail — is covered by
/// `a_step_on_a_different_list_keeps_its_unfused_answer` below, which
/// spells the binder distinctly and asserts the answer.
#[test]
fn a_shadowed_tail_is_rejected() {
    let stderr = run_rejected(
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
    assert!(
        stderr.contains(
            "the pattern binding 'tail' shadows the pattern binding 'tail' defined at \
             line 13; every name means one thing in its scope — rename one of them"
        ),
        "the refusal must be the standard shadow error:\n{stderr}"
    );
}

/// The pass-side half of the shape above, spelled legally. The
/// recursive step is a different list — the empty one the inner match
/// binds — and a cursor stepping on the strength of the recursion alone
/// would walk the input instead. The answer is what says which
/// happened.
#[test]
fn a_step_on_a_different_list_keeps_its_unfused_answer() {
    let out = run_program(
        "other_list",
        r#"module OtherList
    intent =
        "Recurses on an empty list bound under a name of its own."
    exposes [count]
    effects [Console.print]

fn nothing() -> List<String>
    []

fn walk(chars: List<String>, acc: Int) -> Int
    match chars
        [] -> acc
        [head, ..tail] -> match nothing()
            other -> walk(other, acc + 1)

fn count(text: String) -> Int
    walk(String.chars(text), 0)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(count("abcde")))
"#,
    );
    // One step and done: the second call is handed the empty list, not
    // the tail. A cursor stepping the input would answer 5.
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

/// Two `String.chars` producers, two parameters. A cursor variant
/// carries ONE cursor, so it cannot stand in for both lists — this loop
/// keeps both, and its answer.
///
/// Conservatism, not a wrong-fuse guard: fusing whichever list came
/// first would leave the other materialised and still answer correctly.
/// The rule is here because "one cursor variant, one list" is what the
/// rewrite can say it does, and picking a winner by argument order is
/// not.
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

/// The synthesized name is free at top level but BOUND inside the very
/// function whose call site would be rewritten to it. Rewriting there
/// makes the call read the local, not the loop.
#[test]
fn a_caller_that_binds_the_synthesized_name_keeps_its_own_call() {
    let out = run_program(
        "caller_binds",
        r#"module CallerBinds
    intent =
        "Binds the name the pass would synthesize, in the function that calls the loop."
    exposes [count]
    effects [Console.print]

fn walk(chars: List<String>, acc: Int) -> Int
    match chars
        [] -> acc
        [head, ..tail] -> walk(tail, acc + 1)

fn count(text: String) -> Int
    walk__cursor = 42
    walk(String.chars(text), 0) + walk__cursor

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(count("abcde")))
"#,
    );
    assert_eq!(out, "47");
}

/// The control for the three above: the same shapes with ordinary names
/// fuse and answer correctly, so what those tests pin is the name and not
/// the shape.
#[test]
fn the_same_shapes_under_ordinary_names_still_fuse() {
    let out = run_program(
        "ordinary_names",
        r#"module OrdinaryNames
    intent =
        "A statement binding, a pattern binder and a caller local, none of them reserved."
    exposes [count]
    effects [Console.print]

fn walk(chars: List<String>, acc: Int) -> Int
    unrelated = 99
    match acc + 3
        alsoUnrelated -> match chars
            [] -> acc + unrelated - alsoUnrelated + acc
            [head, ..tail] -> walk(tail, acc + 1)

fn count(text: String) -> Int
    spare = 42
    walk(String.chars(text), 0) + spare

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(count("abcde")))
"#,
    );
    // walk answers 5 + 99 - 8 + 5 = 101, plus the caller's 42.
    assert_eq!(out, "143");
}

/// The `String` namespace cannot be shadowed, so the pass is right not to
/// check it: `String.chars(text)` reaches the builtin even where `String`
/// is the name of a parameter in scope. Pinned because the ABSENCE of a
/// rule is as load-bearing as its presence — if this ever stops being
/// true, the pass needs the scope check it deliberately does not have.
#[test]
fn a_parameter_named_string_does_not_shadow_the_builtin_namespace() {
    let out = run_program(
        "string_ns",
        r#"module StringNs
    intent =
        "Takes a parameter called String and still reads characters off the builtin."
    exposes [count]
    effects [Console.print]

fn walk(chars: List<String>, acc: Int) -> Int
    match chars
        [] -> acc
        [head, ..tail] -> walk(tail, acc + 1)

fn count(String: String, text: String) -> Int
    walk(String.chars(text), 0)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(count("ignored", "abcde")))
"#,
    );
    // Five, not the seven characters of the parameter's value.
    assert_eq!(out, "5");
}

/// The call is to a PARAMETER that happens to carry a top-level
/// function's name. A REFUSAL pin and nothing else: the shadowing ban
/// (issue #954) rejects the parameter spelling at the front door, so
/// the pass never meets this shape in user code. That a call through a
/// function-valued parameter is not a fusion site is covered by
/// `a_call_through_a_function_parameter_keeps_its_unfused_answer`
/// below, which names the parameter distinctly and asserts the answer.
#[test]
fn a_call_through_a_parameter_spelling_a_fn_is_rejected() {
    let stderr = run_rejected(
        "callee_shadow",
        r#"module CalleeShadow
    intent =
        "Takes the loop it runs as a parameter, under the name of another loop."
    exposes [report]
    effects [Console.print]

fn walk(chars: List<String>, acc: Int) -> Int
    ? "Counts every character."
    match chars
        [] -> acc
        [head, ..tail] -> walk(tail, acc + 1)

fn half(chars: List<String>, acc: Int) -> Int
    ? "Counts every other character."
    match chars
        [] -> acc
        [first, ..afterFirst] -> match afterFirst
            [] -> acc + 1
            [second, ..rest] -> half(rest, acc + 1)

fn go(walk: Fn(List<String>, Int) -> Int, text: String) -> Int
    ? "Runs whichever loop it was handed."
    walk(String.chars(text), 0)

fn report(text: String) -> String
    "{go(half, text)}/{walk(String.chars(text), 0)}"

fn main() -> Unit
    ! [Console.print]
    Console.print(report("abcde"))
"#,
    );
    assert!(
        stderr.contains(
            "the parameter 'walk' shadows the function 'walk' defined at line 7; \
             every name means one thing in its scope — rename one of them"
        ),
        "the refusal must be the standard shadow error:\n{stderr}"
    );
}

/// The pass-side half of the shape above, spelled legally. The callee
/// is a PARAMETER holding whichever loop it was handed, so the call
/// site does not name the function it runs — a cursor rewrite that
/// keyed on the call site's spelling would fuse the wrong loop, or a
/// loop that is not there. Both loops are fusable on their own, which
/// is what makes the call through the parameter the only thing under
/// test.
#[test]
fn a_call_through_a_function_parameter_keeps_its_unfused_answer() {
    let out = run_program(
        "callee_param",
        r#"module CalleeParam
    intent =
        "Takes the loop it runs as a parameter and reports it beside the direct call."
    exposes [report]
    effects [Console.print]

fn walk(chars: List<String>, acc: Int) -> Int
    ? "Counts every character."
    match chars
        [] -> acc
        [head, ..tail] -> walk(tail, acc + 1)

fn half(chars: List<String>, acc: Int) -> Int
    ? "Counts every other character."
    match chars
        [] -> acc
        [first, ..afterFirst] -> match afterFirst
            [] -> acc + 1
            [second, ..rest] -> half(rest, acc + 1)

fn go(step: Fn(List<String>, Int) -> Int, text: String) -> Int
    ? "Runs whichever loop it was handed."
    step(String.chars(text), 0)

fn report(text: String) -> String
    "{go(half, text)}/{walk(String.chars(text), 0)}"

fn main() -> Unit
    ! [Console.print]
    Console.print(report("abcde"))
"#,
    );
    // `half` counts every other character of five, `walk` counts all
    // five. Fusing the call through `step` into either loop's cursor
    // variant moves the first number.
    assert_eq!(out, "3/5");
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

/// A classifier that reads its parameter OUTSIDE the codepoint match's
/// subject — the wildcard arm falls back to the character itself. The
/// codepoint-call stage must leave it on strings: its `__code` variant
/// would keep a read of a parameter that no longer exists, and this
/// module binds the parameter's name at top level, which is what turns
/// that dropped read from a compile error into a quietly wrong answer.
#[test]
fn a_classifier_that_reads_its_parameter_keeps_its_unfused_answer() {
    let out = run_program(
        "callee_param_read",
        r#"module CalleeParamRead
    intent =
        "Falls back to the character itself, under a top-level binding that shares the parameter's name."
    exposes [run]
    effects [Console.print]

c = "zzz"

fn classify(c: String) -> String
    match c
        "a" -> "letter"
        _ -> c

fn walk(chars: List<String>, acc: String) -> String
    match chars
        [] -> acc
        [head, ..tail] -> walk(tail, acc + classify(head))

fn run(text: String) -> String
    walk(String.chars(text), "")

fn main() -> Unit
    ! [Console.print]
    Console.print(run("aq"))
"#,
    );
    assert_eq!(out, "letterq");
}

/// The same loop with a qualifying classifier, as the control: the
/// codepoint crosses the call, the error text still prints the exact
/// character — multibyte included — and every answer is the one the
/// string version gave.
#[test]
fn a_qualifying_classifier_keeps_every_answer_across_the_code_call() {
    let out = run_program(
        "code_call_control",
        r#"module CodeCallControl
    intent =
        "Decodes characters through a classifier and reports the first bad one."
    exposes [run]
    effects [Console.print]

fn value(digit: String) -> Int
    match String.toLower(digit)
        "0" -> 0
        "9" -> 9
        "a" -> 10
        "f" -> 15
        _ -> 0 - 1

fn walk(chars: List<String>, acc: Int) -> String
    match chars
        [] -> "ok:{acc}"
        [head, ..tail] -> match value(head) < 0
            true -> "bad '{head}' after {acc}"
            false -> walk(tail, acc + value(head))

fn run(text: String) -> String
    walk(String.chars(text), 0)

fn main() -> Unit
    ! [Console.print]
    Console.print("{run("09aF")}|{run("9x")}|{run("é")}|{run("")}")
"#,
    );
    assert_eq!(out, "ok:34|bad 'x' after 9|bad 'é' after 0|ok:0");
}

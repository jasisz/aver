//! Deforestation must decline a loop that reads its own accumulator for
//! anything beyond the prepend the rewrite replaces.
//!
//! The buffered variant has no accumulator parameter. Every surviving
//! mention of it — inside the element expression, inside a sibling
//! tail-call argument, inside the match subject — is left dangling by
//! the rewrite: a free identifier that either fails to compile or, when
//! the module carries a top-level binding of the same name, silently
//! reads that binding instead. Each program below is a loop that was
//! correct before the pass could see it; the assertion is the answer it
//! has always given.

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

/// The list-driven loop that reverses in its own base case, marking
/// values it has already seen. The module also binds `acc` at top level,
/// which is what turns the dropped parameter from a compile error into a
/// wrong answer: fused, every `"a"` came back marked.
#[test]
fn an_element_reading_the_accumulator_keeps_its_unfused_answer() {
    let out = run_program(
        "shadow",
        r#"module Shadow
    intent =
        "Marks duplicates while a top-level binding happens to share the accumulator's name."
    exposes [render]
    effects [Console.print]

acc = ["a"]

fn tag(value: String, seen: Bool) -> String
    match seen
        true -> "dup:{value}"
        false -> value

fn parts(values: List<String>, acc: List<String>) -> List<String>
    match values
        [] -> List.reverse(acc)
        [head, ..tail] -> parts(tail, List.prepend(tag(head, List.contains(acc, head)), acc))

fn render(values: List<String>) -> String
    String.join(parts(values, []), ",")

fn main() -> Unit
    ! [Console.print]
    Console.print(render(["a", "a", "b"]))
"#,
    );
    assert_eq!(
        out, "a,dup:a,b",
        "only the second `a` has been seen before; fusing this loop marked the first one too"
    );
}

/// Same escape in the quadrant that shipped first: base arm hands back a
/// bare accumulator and the caller reverses.
#[test]
fn the_external_reverse_quadrant_keeps_its_unfused_answer() {
    let out = run_program(
        "external",
        r#"module External
    intent =
        "Marks the second and later copies of a value, reversing at the call site."
    exposes [render]
    effects [Console.print]

fn tag(value: String, seen: Bool) -> String
    match seen
        true -> "dup:{value}"
        false -> value

fn markInto(values: List<String>, acc: List<String>) -> List<String>
    match values
        [] -> acc
        [head, ..tail] -> markInto(tail, List.prepend(tag(head, List.contains(acc, head)), acc))

fn render(values: List<String>) -> String
    String.join(List.reverse(markInto(values, [])), ",")

fn main() -> Unit
    ! [Console.print]
    Console.print(render(["a", "a", "b"]))
"#,
    );
    assert_eq!(out, "a,dup:a,b");
}

/// And in the Bool-driven quadrant, where the element expression numbers
/// each row by asking how many the accumulator already holds.
#[test]
fn the_bool_driven_quadrant_keeps_its_unfused_answer() {
    let out = run_program(
        "bool_driven",
        r#"module BoolDriven
    intent =
        "Numbers each row it emits by asking the accumulator how many came before."
    exposes [render]
    effects [Console.print]

fn countdown(n: Int, acc: List<String>) -> List<String>
    match n <= 0
        true -> List.reverse(acc)
        false -> countdown(n - 1, List.prepend(String.fromInt(List.len(acc)), acc))

fn render(n: Int) -> String
    String.join(countdown(n, []), ",")

fn main() -> Unit
    ! [Console.print]
    Console.print(render(3))
"#,
    );
    assert_eq!(out, "0,1,2");
}

/// The match subject is copied onto the buffered variant unchanged, so a
/// loop that decides when to stop by measuring what it has collected has
/// to be declined for the same reason.
#[test]
fn a_loop_that_stops_on_the_accumulator_keeps_its_unfused_answer() {
    let out = run_program(
        "subject",
        r#"module Subject
    intent =
        "Stops the loop by asking how much the accumulator already holds."
    exposes [render]
    effects [Console.print]

fn build(word: String, acc: List<String>) -> List<String>
    match List.len(acc) >= 3
        true -> List.reverse(acc)
        false -> build(word, List.prepend(word, acc))

fn render(word: String) -> String
    String.join(build(word, []), ",")

fn main() -> Unit
    ! [Console.print]
    Console.print(render("x"))
"#,
    );
    assert_eq!(out, "x,x,x");
}

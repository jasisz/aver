//! A collecting loop written as a driver and a step companion must
//! answer exactly what the pair answered before the pass could merge
//! it — whether the pair fuses or declines.
//!
//! The fused half is the hygiene matrix. This family's shipped bugs
//! were binder/naming holes, and that class RECURS, so the matrix is
//! order-controlled over binder POSITIONS rather than a single case:
//! every name the driver already owns, worn by the step's binder in
//! each position the step can put it — a statement binding before the
//! recursion, and a pattern binder around it. Every cell must keep the
//! pair's answer. The report-level facts for the same cells (that each
//! one really fuses) are pinned in the unit tests beside the pass; what
//! this file adds is the running answer.
//!
//! The declined half pins the answers of the pairs the normalization
//! must leave alone: a step with a second call site, a step that reads
//! a name the driver binds (the capture witness — inlined, its helper
//! call would resolve to the driver's binder), and an effectful step
//! (inlined, its trace would change shape).

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

/// One cell of the hygiene matrix: the step's binder wears `binder`,
/// either as a statement binding before the recursion or as a pattern
/// binder around it.
fn matrix_program(binder: &str, binder_in_pattern: bool) -> String {
    let step_body = if binder_in_pattern {
        format!(
            "    match Option.Some(sh * 2)\n        Option.Some({binder}) -> drive(st, List.prepend({binder}, sacc))\n        Option.None -> []"
        )
    } else {
        format!("    {binder} = sh * 2\n    drive(st, List.prepend({binder}, sacc))")
    };
    format!(
        r#"module PairMatrix
    intent =
        "One cell of the driver-and-step hygiene matrix."
    exposes [entry]
    effects [Console.print]

fn drive(xs: List<Int>, acc: List<Int>) -> List<Int>
    ? "Driver: matches and terminates."
    match xs
        [] -> List.reverse(acc)
        [h, ..t] -> step(h, t, acc)

fn step(sh: Int, st: List<Int>, sacc: List<Int>) -> List<Int>
    ? "Step: one doubled element, then back into the driver."
{step_body}

fn entry(xs: List<Int>) -> List<Int>
    ? "Start the loop with an empty accumulator."
    drive(xs, [])

fn render(values: List<Int>) -> String
    ? "Print a list with no accumulator of its own."
    match values
        [] -> ""
        [head, ..tail] -> "{{head}}/{{render(tail)}}"

fn main() -> Unit
    ! [Console.print]
    Console.print("{{render(entry([1, 2, 3]))}}")
"#
    )
}

/// Every cell of the matrix answers what the unfused pair answers:
/// doubles, in traversal order. The binder axis runs through the
/// driver's own names — its params, and the pattern binders that stand
/// BETWEEN the driver's head and the inline point — plus a fresh
/// control; the position axis runs through both places a step can bind.
#[test]
fn the_hygiene_matrix_keeps_the_pairs_answer_in_every_cell() {
    for binder in ["xs", "acc", "h", "t", "v"] {
        for in_pattern in [false, true] {
            let out = run_program(
                &format!("matrix_{binder}_{in_pattern}"),
                &matrix_program(binder, in_pattern),
            );
            assert_eq!(
                out, "2/4/6/",
                "binder {binder:?} (in_pattern: {in_pattern}) changed the answer"
            );
        }
    }
}

/// The capture witness: the step reads the top-level `scale`, and the
/// driver's cons pattern re-binds that name around the call site.
/// Inlined, the step's `scale(h)` would read the driver's binder — an
/// integer — instead of the function. The pair must decline and keep
/// its answer.
#[test]
fn a_step_reading_a_name_the_driver_binds_keeps_the_pairs_answer() {
    let out = run_program(
        "capture_witness",
        r#"module CaptureWitness
    intent =
        "The step reads scale; the driver binds scale around the call."
    exposes [entry]
    effects [Console.print]

fn scale(n: Int) -> Int
    ? "The function the step means when it says scale."
    n * 10

fn drive(xs: List<Int>, acc: List<Int>) -> List<Int>
    ? "Driver whose cons pattern re-binds the helper's name."
    match xs
        [] -> List.reverse(acc)
        [scale, ..t] -> step(scale, t, acc)

fn step(h: Int, t: List<Int>, acc: List<Int>) -> List<Int>
    ? "Step: scale one element, then back into the driver."
    drive(t, List.prepend(scale(h), acc))

fn entry(xs: List<Int>) -> List<Int>
    ? "Start the loop with an empty accumulator."
    drive(xs, [])

fn render(values: List<Int>) -> String
    ? "Print a list with no accumulator of its own."
    match values
        [] -> ""
        [head, ..tail] -> "{head}/{render(tail)}"

fn main() -> Unit
    ! [Console.print]
    Console.print("{render(entry([1, 2]))}")
"#,
    );
    assert_eq!(out, "10/20/");
}

/// A step with a second call site is shared code; both callers must
/// keep their answers, including the one that starts mid-loop.
#[test]
fn a_step_with_a_second_call_site_keeps_both_answers() {
    let out = run_program(
        "shared_step",
        r#"module SharedStep
    intent =
        "The same step called by its driver and by another fn."
    exposes [entry, other]
    effects [Console.print]

fn drive(xs: List<Int>, acc: List<Int>) -> List<Int>
    ? "Driver: matches and terminates."
    match xs
        [] -> List.reverse(acc)
        [h, ..t] -> step(h, t, acc)

fn step(h: Int, t: List<Int>, acc: List<Int>) -> List<Int>
    ? "Step: one doubled element, then back into the driver."
    drive(t, List.prepend(h * 2, acc))

fn other(h: Int) -> List<Int>
    ? "The second caller, entering the loop through the step."
    step(h, [7], [])

fn entry(xs: List<Int>) -> List<Int>
    ? "Start the loop with an empty accumulator."
    drive(xs, [])

fn render(values: List<Int>) -> String
    ? "Print a list with no accumulator of its own."
    match values
        [] -> ""
        [head, ..tail] -> "{head}/{render(tail)}"

fn main() -> Unit
    ! [Console.print]
    Console.print("{render(entry([1, 2]))} {render(other(3))}")
"#,
    );
    assert_eq!(out, "2/4/ 6/14/");
}

/// An effectful step's prints are its trace; the pair must decline and
/// keep printing one line per element, in loop order, before the
/// answer.
#[test]
fn an_effectful_step_keeps_its_print_order() {
    let out = run_program(
        "effectful_step",
        r#"module EffectfulStep
    intent =
        "The step prints each element as it works."
    exposes [entry]
    effects [Console.print]

fn drive(xs: List<Int>, acc: List<Int>) -> List<Int>
    ? "Driver: matches and terminates."
    ! [Console.print]
    match xs
        [] -> List.reverse(acc)
        [h, ..t] -> step(h, t, acc)

fn step(h: Int, t: List<Int>, acc: List<Int>) -> List<Int>
    ? "Step: print, double, back into the driver."
    ! [Console.print]
    Console.print("saw {h}")
    drive(t, List.prepend(h * 2, acc))

fn entry(xs: List<Int>) -> List<Int>
    ? "Start the loop with an empty accumulator."
    ! [Console.print]
    drive(xs, [])

fn render(values: List<Int>) -> String
    ? "Print a list with no accumulator of its own."
    match values
        [] -> ""
        [head, ..tail] -> "{head}/{render(tail)}"

fn main() -> Unit
    ! [Console.print]
    Console.print("{render(entry([1, 2]))}")
"#,
    );
    assert_eq!(out, "saw 1\nsaw 2\n2/4/");
}

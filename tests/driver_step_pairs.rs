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
//! The second axis is ARGUMENT SPELLING: a call-site argument dressed
//! in another step parameter's name — earlier, later, the same one, a
//! step binder — on both the substituted and the bound-args path. The
//! later-param spelling was a shipped silent-wrong answer (sequential
//! substitution re-visited the identifiers it had just inserted), so
//! this axis, too, is a matrix and not a single case.
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

/// One cell of the ARGUMENT-SPELLING matrix: the driver peels two
/// elements per round, so the step has two value parameters and the
/// call site can dress either argument in another parameter's name.
/// `first`/`second` name the driver's element binders — the spellings
/// the call-site arguments wear — and the bound flags wrap an argument
/// in `+ 0`, pushing it off the substituted path onto the bound-args
/// path.
fn argument_matrix_program(
    first: &str,
    second: &str,
    bound_first: bool,
    bound_second: bool,
) -> String {
    let arg1 = if bound_first {
        format!("{first} + 0")
    } else {
        first.to_string()
    };
    let arg2 = if bound_second {
        format!("{second} + 0")
    } else {
        second.to_string()
    };
    format!(
        r#"module ArgumentMatrix
    intent =
        "One cell of the argument-spelling matrix."
    exposes [entry]
    effects [Console.print]

fn drive(xs: List<Int>, acc: List<Int>) -> List<Int>
    ? "Driver: peels two elements, then hands the pair to the step."
    match xs
        [] -> List.reverse(acc)
        [{first}, ..t] -> match t
            [] -> List.reverse(acc)
            [{second}, ..t2] -> step({arg1}, {arg2}, t2, acc)

fn step(sa: Int, sb: Int, st: List<Int>, sacc: List<Int>) -> List<Int>
    ? "Step: combine the pair as sa*10 + sb, then back into the driver."
    v = sa * 10 + sb
    drive(st, List.prepend(v, sacc))

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
    Console.print("{{render(entry([1, 2, 3, 4]))}}")
"#
    )
}

/// The argument/parameter axis of the hygiene matrix: a call-site
/// argument spelled like an EARLIER step parameter, like a LATER one,
/// like the SAME one, and like a STEP BINDER — each on the substituted
/// path (bare identifier) and on the bound-args path (`+ 0` around the
/// same identifier). Substituting one parameter at a time captured the
/// later-param spelling: the identifiers just inserted for the earlier
/// parameter were re-visited by the later parameter's pass, and
/// `[1, 2, 3, 4]` answered `22/44/` instead of `12/34/` — silently, on
/// both backends. Every cell must answer what the unfused pair
/// answers.
#[test]
fn the_argument_spelling_matrix_keeps_the_pairs_answer_in_every_cell() {
    let cells: &[(&str, &str, bool, bool)] = &[
        // Argument spelled like a LATER param: sb is the step's second
        // parameter, worn by the FIRST argument. The silent-wrong cell.
        ("sb", "y", false, false),
        ("sb", "y", true, false),
        // Like an EARLIER param: sa worn by the second argument.
        ("x", "sa", false, false),
        ("x", "sa", false, true),
        // Like the SAME param: sa worn by its own argument.
        ("sa", "y", false, false),
        ("sa", "y", true, false),
        // Like a STEP BINDER: v is the step's own statement binding.
        ("v", "y", false, false),
        ("v", "y", true, false),
    ];
    for (first, second, bound_first, bound_second) in cells {
        let out = run_program(
            &format!("argmatrix_{first}_{second}_{bound_first}_{bound_second}"),
            &argument_matrix_program(first, second, *bound_first, *bound_second),
        );
        assert_eq!(
            out, "12/34/",
            "arguments ({first:?}, {second:?}) (bound: {bound_first}, {bound_second}) \
             changed the answer"
        );
    }
}

/// The silent-wrong shape as a fused-vs-unfused differential: the same
/// program with the step private (fuses) and with the step exposed
/// (declines, so the pair runs as written). Both must answer `12/34/`
/// — the cross-backend differential cannot catch this class, because
/// the VM and the generated Rust agree on the wrong answer.
#[test]
fn a_pairwise_combine_answers_the_same_fused_and_unfused() {
    let fused = argument_matrix_program("sb", "y", false, false);
    let unfused = fused.replace("exposes [entry]", "exposes [entry, step]");
    assert_eq!(
        run_program("pairwise_fused", &fused),
        "12/34/",
        "the fused pair changed the answer"
    );
    assert_eq!(
        run_program("pairwise_unfused", &unfused),
        "12/34/",
        "the declined control changed the answer"
    );
}

/// The typed manifestation of the same capture: the driver's cons
/// binder wears the step's LIST parameter's name and is passed as the
/// first argument. Sequential substitution rewrote the inserted
/// identifier to the list argument, and a well-typed program was
/// rejected with "cannot multiply List and Int" blaming the user's
/// correct line. It must compile and keep the pair's answer.
#[test]
fn a_driver_binder_spelled_like_the_steps_list_param_compiles_and_answers() {
    let out = run_program(
        "typed_manifestation",
        r#"module TypedManifestation
    intent =
        "The driver's cons binder wears the step's List param name."
    exposes [entry]
    effects [Console.print]

fn drive(xs: List<Int>, acc: List<Int>) -> List<Int>
    ? "Driver whose cons binder wears the step's second param name."
    match xs
        [] -> List.reverse(acc)
        [st, ..t] -> step(st, t, acc)

fn step(sh: Int, st: List<Int>, sacc: List<Int>) -> List<Int>
    ? "Step: double the head, back into the driver."
    drive(st, List.prepend(sh * 2, sacc))

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
    Console.print("{render(entry([1, 2, 3]))}")
"#,
    );
    assert_eq!(out, "2/4/6/");
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

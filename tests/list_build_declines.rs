//! The list-build pass must decline every collecting loop whose
//! accumulator it cannot prove it is the only holder of.
//!
//! A missed fusion costs a cons cell per element. A WRONG fusion costs
//! the answer: the builder is appended to IN PLACE, so a loop that reads
//! what it has collected while it is collecting would read a list
//! somebody has already written past — and a loop whose exits disagree
//! about the reverse gets its elements back in the order the other half
//! of the pipeline was not expecting.
//!
//! Each program below was correct before this pass could see it. The
//! assertion is the answer it has always given. Cross-backend agreement
//! on the shapes that DO fuse is the differential's job
//! (`rust_codegen_differential`); what is pinned here is that these
//! shapes keep their meaning.
//!
//! Two of them are pinned in the differential INSTEAD of here, because
//! the virtual machine alone cannot see their class of mistake: a VM
//! builder handed something other than a fresh builder falls back to the
//! cons chain, which prepends and reverses, so a wrong fusion of those
//! shapes answers correctly there by accident. Compiled Rust appends and
//! does not reverse, and says so.

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

/// The loop decides what to append by looking at what it has already
/// appended. A builder read while it is being written to is a builder
/// with two holders, and the in-place append has no answer for the
/// second one.
#[test]
fn a_loop_that_reads_what_it_collected_keeps_its_unfused_answer() {
    let out = run_program(
        "reads_back",
        r#"module ReadsBack
    intent =
        "Collects the run so far, and each element is how much of it there is."
    exposes [widths]
    effects [Console.print]

fn widths(n: Int, acc: List<Int>) -> List<Int>
    match n <= 0
        true -> List.reverse(acc)
        false -> widths(n - 1, List.prepend(List.len(acc), acc))

fn render(values: List<Int>) -> String
    ? "Print a list without collecting anything, so the printer is not the subject."
    "[{renderItems(values)}]"

fn renderItems(values: List<Int>) -> String
    ? "The elements, comma-separated, built without an accumulator."
    match values
        [] -> ""
        [head, ..tail] -> match tail
            [] -> "{head}"
            [next, ..rest] -> "{head}, {renderItems(tail)}"

fn main() -> Unit
    ! [Console.print]
    Console.print("{render(widths(4, []))}")
"#,
    );
    // Four steps, each appending how many were there before it.
    assert_eq!(out, "[0, 1, 2, 3]");
}

/// The loop stops when it has collected enough. The subject runs before
/// every arm, so a read there and the append in the recursive arm are
/// two reads on one pass through the loop.
#[test]
fn a_loop_that_stops_on_what_it_collected_keeps_its_unfused_answer() {
    let out = run_program(
        "stops_on_size",
        r#"module StopsOnSize
    intent =
        "Collects until three have been collected, however far the count has to go."
    exposes [firstThree]
    effects [Console.print]

fn firstThree(n: Int, acc: List<Int>) -> List<Int>
    match List.len(acc) >= 3
        true -> List.reverse(acc)
        false -> firstThree(n + 1, List.prepend(n, acc))

fn render(values: List<Int>) -> String
    ? "Print a list without collecting anything, so the printer is not the subject."
    "[{renderItems(values)}]"

fn renderItems(values: List<Int>) -> String
    ? "The elements, comma-separated, built without an accumulator."
    match values
        [] -> ""
        [head, ..tail] -> match tail
            [] -> "{head}"
            [next, ..rest] -> "{head}, {renderItems(tail)}"

fn main() -> Unit
    ! [Console.print]
    Console.print("{render(firstThree(1, []))}")
"#,
    );
    assert_eq!(out, "[1, 2, 3]");
}

/// One exit reverses the accumulator and another hands it back bare. A
/// call site can wear the caller's reverse or not; it cannot do both,
/// and whichever spelling the rewrite picked would be wrong for half the
/// exits.
#[test]
fn a_loop_whose_exits_disagree_keeps_its_unfused_answer() {
    let out = run_program(
        "mixed_exits",
        r#"module MixedExits
    intent =
        "Collects forwards, except on the value that bails out backwards."
    exposes [collect]
    effects [Console.print]

fn collect(n: Int, acc: List<Int>) -> List<Int>
    match n <= 0
        true -> List.reverse(acc)
        false -> match n == 2
            true -> acc
            false -> collect(n - 1, List.prepend(n, acc))

fn render(values: List<Int>) -> String
    ? "Print a list without collecting anything, so the printer is not the subject."
    "[{renderItems(values)}]"

fn renderItems(values: List<Int>) -> String
    ? "The elements, comma-separated, built without an accumulator."
    match values
        [] -> ""
        [head, ..tail] -> match tail
            [] -> "{head}"
            [next, ..rest] -> "{head}, {renderItems(tail)}"

fn main() -> Unit
    ! [Console.print]
    Console.print("{render(collect(4, []))} {render(collect(1, []))}")
"#,
    );
    // 4 and 3 are collected, then n == 2 bails out with the raw
    // accumulator — which is [3, 4], not [4, 3]. The second call never
    // reaches the bail-out and reverses on the way out.
    assert_eq!(out, "[3, 4] [1]");
}

/// A cons pattern that re-binds the accumulator's name. Every read
/// underneath it is the head of the input, not the accumulator the loop
/// was handed — and the base case returns the untouched parameter, so a
/// rewrite that trusted the name would build the wrong list twice over.
///
/// The shadowing ban (issue #954) refuses that spelling at the front
/// door now, so the pass can no longer meet this shape in user code;
/// its own guard stays as defense for compiler-synthesized shapes, and
/// this pin says the refusal is what the user sees.
#[test]
fn a_pattern_that_shadows_the_accumulator_is_rejected() {
    let stderr = run_rejected(
        "shadowed",
        r#"module Shadowed
    intent =
        "Walks a list of lists with a binder that shares the accumulator's name."
    exposes [walk]
    effects [Console.print]

fn walk(values: List<List<Int>>, acc: List<Int>) -> List<Int>
    match values
        [] -> List.reverse(acc)
        [acc, ..tail] -> walk(tail, List.prepend(7, acc))

fn render(values: List<Int>) -> String
    ? "Print a list without collecting anything, so the printer is not the subject."
    "[{renderItems(values)}]"

fn renderItems(values: List<Int>) -> String
    ? "The elements, comma-separated, built without an accumulator."
    match values
        [] -> ""
        [head, ..tail] -> match tail
            [] -> "{head}"
            [next, ..rest] -> "{head}, {renderItems(tail)}"

fn main() -> Unit
    ! [Console.print]
    Console.print("{render(walk([[1, 2], [3, 4]], []))}")
"#,
    );
    assert!(
        stderr.contains(
            "the pattern binding 'acc' shadows the parameter 'acc' defined at line 7; \
             every name means one thing in its scope — rename one of them"
        ),
        "the refusal must be the standard shadow error:\n{stderr}"
    );
}

/// A second self-call that restarts the fold with a fresh list, sitting
/// where the per-read walk never looks: a subtree that mentions the
/// accumulator nowhere.
///
/// This one is conservatism rather than a wrong answer averted. The
/// restart passes a fresh list to the loop the variant was built from,
/// which is still in the program and still takes a list, so leaving it
/// alone would have answered correctly too — it would only have
/// abandoned the builder it was holding. The rule is here so that "the
/// self-calls the rewrite classified" and "the self-calls the function
/// has" are the same set by construction, rather than by an argument
/// about what an unclassified one would have done.
#[test]
fn a_loop_that_restarts_itself_keeps_its_unfused_answer() {
    let out = run_program(
        "restarts",
        r#"module Restarts
    intent =
        "Collects down from a number, throwing away what it has at one of them."
    exposes [collect]
    effects [Console.print]

fn collect(n: Int, acc: List<Int>) -> List<Int>
    match n <= 0
        true -> List.reverse(acc)
        false -> match n == 3
            true -> match n > 100
                true -> []
                false -> collect(n - 1, [])
            false -> collect(n - 1, List.prepend(n, acc))

fn render(values: List<Int>) -> String
    ? "Print a list without collecting anything, so the printer is not the subject."
    "[{renderItems(values)}]"

fn renderItems(values: List<Int>) -> String
    ? "The elements, comma-separated, built without an accumulator."
    match values
        [] -> ""
        [head, ..tail] -> match tail
            [] -> "{head}"
            [next, ..rest] -> "{head}, {renderItems(tail)}"

fn main() -> Unit
    ! [Console.print]
    Console.print("{render(collect(5, []))}")
"#,
    );
    // 5 and 4 are collected, 3 throws them away, then 2 and 1.
    assert_eq!(out, "[2, 1]");
}

/// The program binds `__lst_push` itself. A leading `__` is not
/// reserved — this parses, type-checks and runs — and a binder is
/// exactly what shadows a name for the code underneath it, so emitting
/// a call to the intrinsic here would call the user's number.
#[test]
fn a_program_that_binds_a_builder_name_keeps_its_unfused_answer() {
    let out = run_program(
        "name_taken",
        r#"module NameTaken
    intent =
        "Collects a run, in a module that happens to bind one of the builder names."
    exposes [collect]
    effects [Console.print]

fn collect(n: Int, acc: List<Int>) -> List<Int>
    __lst_push = 3
    match n <= 0
        true -> List.reverse(acc)
        false -> collect(n - 1, List.prepend(n + __lst_push, acc))

fn render(values: List<Int>) -> String
    ? "Print a list without collecting anything, so the printer is not the subject."
    "[{renderItems(values)}]"

fn renderItems(values: List<Int>) -> String
    ? "The elements, comma-separated, built without an accumulator."
    match values
        [] -> ""
        [head, ..tail] -> match tail
            [] -> "{head}"
            [next, ..rest] -> "{head}, {renderItems(tail)}"

fn main() -> Unit
    ! [Console.print]
    Console.print("{render(collect(5, []))}")
"#,
    );
    // Five steps, each element three more than the count it came from.
    assert_eq!(out, "[8, 7, 6, 5, 4]");
}

/// The loop hands its accumulator back bare and the caller reverses.
/// Only the reversing call site produces the forward list, so a caller
/// that asked for the elements backwards has to keep getting them.
#[test]
fn a_caller_that_wanted_the_elements_backwards_still_gets_them() {
    let out = run_program(
        "both_directions",
        r#"module BothDirections
    intent =
        "The same collecting loop read forwards by one caller and backwards by another."
    exposes [forwards, backwards]
    effects [Console.print]

fn collectInto(values: List<Int>, acc: List<Int>) -> List<Int>
    match values
        [] -> acc
        [head, ..tail] -> collectInto(tail, List.prepend(head * 2, acc))

fn forwards(values: List<Int>) -> List<Int>
    List.reverse(collectInto(values, []))

fn backwards(values: List<Int>) -> List<Int>
    collectInto(values, [])

fn render(values: List<Int>) -> String
    ? "Print a list without collecting anything, so the printer is not the subject."
    "[{renderItems(values)}]"

fn renderItems(values: List<Int>) -> String
    ? "The elements, comma-separated, built without an accumulator."
    match values
        [] -> ""
        [head, ..tail] -> match tail
            [] -> "{head}"
            [next, ..rest] -> "{head}, {renderItems(tail)}"

fn main() -> Unit
    ! [Console.print]
    Console.print("{render(forwards([1, 2, 3]))} {render(backwards([1, 2, 3]))}")
"#,
    );
    assert_eq!(out, "[2, 4, 6] [6, 4, 2]");
}

/// A call site that starts the accumulator with elements already in it.
/// The builder has nowhere to put them, so the call keeps the loop it
/// named.
#[test]
fn a_call_that_starts_with_elements_keeps_them() {
    let out = run_program(
        "seeded",
        r#"module Seeded
    intent =
        "The same collecting loop started empty and started with something in it."
    exposes [collect]
    effects [Console.print]

fn collect(n: Int, acc: List<Int>) -> List<Int>
    match n <= 0
        true -> List.reverse(acc)
        false -> collect(n - 1, List.prepend(n, acc))

fn render(values: List<Int>) -> String
    ? "Print a list without collecting anything, so the printer is not the subject."
    "[{renderItems(values)}]"

fn renderItems(values: List<Int>) -> String
    ? "The elements, comma-separated, built without an accumulator."
    match values
        [] -> ""
        [head, ..tail] -> match tail
            [] -> "{head}"
            [next, ..rest] -> "{head}, {renderItems(tail)}"

fn main() -> Unit
    ! [Console.print]
    Console.print("{render(collect(3, []))} {render(collect(3, [99]))}")
"#,
    );
    // The loop counts down and reverses, so an empty start reads 3, 2, 1
    // — and the seeded start carries its element to the front, where the
    // builder has nowhere to put it.
    assert_eq!(out, "[3, 2, 1] [99, 3, 2, 1]");
}

/// The loop hands its accumulator to another function instead of
/// returning it.
///
/// A reverse of the accumulator can stand anywhere, because the builder
/// finalizes to the very list the reverse produced. A bare accumulator
/// is that list BACKWARDS, and the rewrite pays for it by taking the
/// reverse off the CALL SITE — which it can only do when what the call
/// site receives is the accumulator itself. Handed to `dropFirst`, the
/// same substitution would turn its argument the right way round and
/// leave nobody to turn it back.
#[test]
fn a_loop_that_hands_its_accumulator_to_a_helper_keeps_its_unfused_answer() {
    let out = run_program(
        "handed_on",
        r#"module HandedOn
    intent =
        "Collects a run and lets a helper have the accumulator instead of returning it."
    exposes [collect]
    effects [Console.print]

fn dropFirst(values: List<Int>) -> List<Int>
    ? "Everything after the first element."
    List.drop(values, 1)

fn collect(n: Int, acc: List<Int>) -> List<Int>
    match n <= 0
        true -> dropFirst(acc)
        false -> collect(n - 1, List.prepend(n, acc))

fn render(values: List<Int>) -> String
    ? "Print a list without collecting anything, so the printer is not the subject."
    "[{renderItems(values)}]"

fn renderItems(values: List<Int>) -> String
    ? "The elements, comma-separated, built without an accumulator."
    match values
        [] -> ""
        [head, ..tail] -> match tail
            [] -> "{head}"
            [next, ..rest] -> "{head}, {renderItems(tail)}"

fn main() -> Unit
    ! [Console.print]
    Console.print("{render(List.reverse(collect(3, [])))}")
"#,
    );
    // The accumulator reads 1, 2, 3; `dropFirst` leaves 2, 3; the caller
    // reverses to 3, 2. Finalizing the accumulator instead would hand
    // `dropFirst` the list forwards and answer 2, 1.
    assert_eq!(out, "[3, 2]");
}

/// One exit hands the accumulator back bare — so the caller reverses —
/// and another exit answers with a list the accumulator never reached.
/// Taking the reverse off the call site pays for the bare exit, but the
/// bail-out exit never owed one, and any path that takes it would lose
/// a reversal the program wrote.
#[test]
fn a_loop_with_an_exit_the_accumulator_never_reaches_keeps_its_unfused_answer() {
    let out = run_program(
        "bails_out",
        r#"module BailsOut
    intent =
        "Collects a run, except on the value that answers with a list of its own."
    exposes [collect]
    effects [Console.print]

fn collect(values: List<Int>, acc: List<Int>) -> List<Int>
    match values
        [] -> acc
        [head, ..tail] -> match head == 0
            true -> [7, 8]
            false -> collect(tail, List.prepend(head, acc))

fn render(values: List<Int>) -> String
    ? "Print a list without collecting anything, so the printer is not the subject."
    "[{renderItems(values)}]"

fn renderItems(values: List<Int>) -> String
    ? "The elements, comma-separated, built without an accumulator."
    match values
        [] -> ""
        [head, ..tail] -> match tail
            [] -> "{head}"
            [next, ..rest] -> "{head}, {renderItems(tail)}"

fn main() -> Unit
    ! [Console.print]
    Console.print("{render(List.reverse(collect([1, 0, 2], [])))}")
"#,
    );
    // 1 is collected, then 0 bails out with [7, 8] — dropping the
    // accumulator — and the caller reverses whatever came back.
    assert_eq!(out, "[8, 7]");
}

// === The byte sink =====================================================
//
// The programs below carry a word-for-word copy of the standard
// library's `fromList` family, which is what makes them candidates for
// the byte-sink retarget — the retarget verifies its consumer
// structurally against the embedded module, and an exact copy passes.
// Each one then breaks exactly one of the retarget's guards, and the
// assertion is the answer the unfused pair has always given.

/// The `fromList` family the retarget recognises, verbatim.
const FROM_LIST_FAMILY: &str = r#"record Bytes
    values: List<Int>

fn allInRange(xs: List<Int>) -> Bool
    ? "Return true when every integer in the list is an octet."
    match xs
        [] -> true
        [head, ..tail] -> match Bool.and(head >= 0, head <= 255)
            true -> allInRange(tail)
            false -> false

fn firstOutOfRange(xs: List<Int>) -> Int
    ? "Return the first non-octet value; -1 when every value is an octet."
    match xs
        [] -> -1
        [head, ..tail] -> match Bool.and(head >= 0, head <= 255)
            true -> firstOutOfRange(tail)
            false -> head

fn firstOutOfRangeIndex(xs: List<Int>) -> Int
    ? "Return the index of the first non-octet value; the length when every value is an octet."
    match xs
        [] -> 0
        [head, ..tail] -> match Bool.and(head >= 0, head <= 255)
            true -> 1 + firstOutOfRangeIndex(tail)
            false -> 0

fn fromList(xs: List<Int>) -> Result<Bytes, String>
    ? "Validate raw integers and construct a byte sequence."
    match allInRange(xs)
        true -> Result.Ok(Bytes(values = xs))
        false -> Result.Err("byte {firstOutOfRange(xs)} at index {firstOutOfRangeIndex(xs)} is outside 0..=255")

fn renderItems(values: List<Int>) -> String
    ? "The elements, comma-separated."
    match values
        [] -> ""
        [head, ..tail] -> match tail
            [] -> "{head}"
            [next, ..rest] -> "{head}, {renderItems(tail)}"

fn describe(outcome: Result<Bytes, String>) -> String
    ? "Render either side of a fromList answer."
    match outcome
        Result.Ok(bytes) -> "ok:{renderItems(bytes.values)}"
        Result.Err(message) -> "err:{message}"
"#;

/// The consumer reads the collected result once more before handing it
/// to `fromList` — a length gate, exactly the second reader the
/// occurs-check exists for. Both reads see the same list, so a fusion
/// that missed one would hand the gate a builder it already consumed.
#[test]
fn a_collected_result_read_twice_keeps_its_validation_walk() {
    let out = run_program(
        "byte_second_reader",
        &format!(
            r#"module SecondReader
    intent =
        "A collected result read once more before its fromList."
    effects [Console.print]

{FROM_LIST_FAMILY}
fn collectRange(n: Int, acc: List<Int>) -> Result<List<Int>, String>
    ? "Collect n samples spaced by sixty, counting down."
    match n <= 0
        true -> Result.Ok(List.reverse(acc))
        false -> collectRange(n - 1, List.prepend(n * 60, acc))

fn toBytes(n: Int) -> Result<Bytes, String>
    ? "Refuse long runs before validating, which reads the collected list twice."
    values = collectRange(n, [])?
    match List.len(values) >= 3
        true -> Result.Err("three is plenty")
        false -> fromList(values)

fn main() -> Unit
    ! [Console.print]
    Console.print(describe(toBytes(4)))
    Console.print(describe(toBytes(2)))
"#
        ),
    );
    assert_eq!(out, "err:three is plenty\nok:120, 60");
}

/// The consumer is a `fromList` in name only — its message differs
/// from the standard library's by one word. The retarget bakes in the
/// library's exact words, so this module keeps its own.
#[test]
fn a_from_list_with_its_own_words_keeps_them() {
    let family_with_other_words = FROM_LIST_FAMILY.replace(
        "\"byte {firstOutOfRange(xs)}",
        "\"value {firstOutOfRange(xs)}",
    );
    let out = run_program(
        "byte_modified_words",
        &format!(
            r#"module ModifiedWords
    intent =
        "A fromList whose message differs from the standard library's by one word."
    effects [Console.print]

{family_with_other_words}
fn collect(n: Int, acc: List<Int>) -> List<Int>
    ? "Collect n samples spaced by 150, counting down."
    match n <= 0
        true -> List.reverse(acc)
        false -> collect(n - 1, List.prepend(n * 150, acc))

fn main() -> Unit
    ! [Console.print]
    Console.print(describe(fromList(collect(1, []))))
    Console.print(describe(fromList(collect(3, []))))
"#
        ),
    );
    assert_eq!(out, "ok:150\nerr:value 450 at index 0 is outside 0..=255");
}

/// An exit the accumulator never reaches answers with a list of its
/// own — and `fromList` judges that list too. A retarget would have
/// validated only what the loop pushed and answered `ok:7, 999`.
#[test]
fn an_exit_the_accumulator_never_reaches_still_gets_validated() {
    let out = run_program(
        "byte_bails_into_validation",
        &format!(
            r#"module BailsIntoValidation
    intent =
        "A loop with an exit the accumulator never reaches, whose list still passes through fromList."
    effects [Console.print]

{FROM_LIST_FAMILY}
fn collect(values: List<Int>, acc: List<Int>) -> List<Int>
    ? "Collect until a zero, which answers with a list of its own."
    match values
        [] -> List.reverse(acc)
        [head, ..tail] -> match head == 0
            true -> [7, 999]
            false -> collect(tail, List.prepend(head, acc))

fn main() -> Unit
    ! [Console.print]
    Console.print(describe(fromList(collect([1, 0, 2], []))))
    Console.print(describe(fromList(collect([1, 2], []))))
"#
        ),
    );
    assert_eq!(out, "err:byte 999 at index 1 is outside 0..=255\nok:1, 2");
}

/// A call that seeds the accumulator keeps the loop it named — the
/// family's existing rule — and `fromList` still judges the seeded
/// element. The empty-started call beside it is free to fuse; both
/// answers are the pair's.
#[test]
fn a_seeded_from_list_call_keeps_its_elements_and_its_judgement() {
    let out = run_program(
        "byte_seeded",
        &format!(
            r#"module SeededIntoValidation
    intent =
        "The same collecting loop started empty and started with an element fromList must still judge."
    effects [Console.print]

{FROM_LIST_FAMILY}
fn collect(n: Int, acc: List<Int>) -> List<Int>
    ? "Collect a countdown into the accumulator."
    match n <= 0
        true -> List.reverse(acc)
        false -> collect(n - 1, List.prepend(n, acc))

fn main() -> Unit
    ! [Console.print]
    Console.print(describe(fromList(collect(3, [300]))))
    Console.print(describe(fromList(collect(3, []))))
"#
        ),
    );
    assert_eq!(
        out,
        "err:byte 300 at index 0 is outside 0..=255\nok:3, 2, 1"
    );
}

/// The program binds a name in the `__byt_` namespace. A leading `__`
/// is not reserved — this parses, type-checks and runs — and a binder
/// is exactly what shadows a name for the code underneath it, so the
/// whole pass steps aside, list fusion included.
#[test]
fn a_program_that_binds_a_byte_builder_name_keeps_its_unfused_answer() {
    let out = run_program(
        "byte_name_taken",
        &format!(
            r#"module ByteNameTaken
    intent =
        "A module that binds a name in the byte builder's namespace keeps every list."
    effects [Console.print]

{FROM_LIST_FAMILY}
fn collect(n: Int, acc: List<Int>) -> List<Int>
    ? "Collect a countdown, in a module that binds one of the byte builder's names."
    __byt_probe = 3
    match n <= 0
        true -> List.reverse(acc)
        false -> collect(n - 1, List.prepend(n + __byt_probe, acc))

fn main() -> Unit
    ! [Console.print]
    Console.print(describe(fromList(collect(4, []))))
"#
        ),
    );
    assert_eq!(out, "ok:7, 6, 5, 4");
}

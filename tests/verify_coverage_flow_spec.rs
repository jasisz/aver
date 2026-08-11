/// Spec tests for how `verify-coverage` reads return-shape coverage:
/// `?` applied to the function under verification counts as a `Result.Ok`
/// case, and a private helper with one covered caller inherits that caller's
/// return-shape coverage.
///
/// Every "still warns" test here is load-bearing: the rule must stay a
/// targeted relaxation, not a blanket silencer.
use aver::ast::TopLevel;
use aver::checker::collect_verify_coverage_warnings;
use aver::lexer::Lexer;
use aver::parser::Parser;

fn parse(src: &str) -> Vec<TopLevel> {
    let mut lexer = Lexer::new(src);
    let tokens = lexer.tokenize().expect("lex failed");
    let mut parser = Parser::new(tokens);
    parser.parse().expect("parse failed")
}

fn coverage_messages(src: &str) -> Vec<String> {
    collect_verify_coverage_warnings(&parse(src))
        .into_iter()
        .map(|w| w.message)
        .collect()
}

fn assert_absent(messages: &[String], needle: &str) {
    assert!(
        !messages.iter().any(|m| m.contains(needle)),
        "expected no warning containing {:?}, got {:?}",
        needle,
        messages
    );
}

fn assert_present(messages: &[String], needle: &str) {
    assert!(
        messages.iter().any(|m| m.contains(needle)),
        "expected a warning containing {:?}, got {:?}",
        needle,
        messages
    );
}

// ---------------------------------------------------------------------------
// `?` on the function under verification is an Ok case
// ---------------------------------------------------------------------------

#[test]
fn error_propagation_on_the_target_counts_as_an_ok_case() {
    let messages = coverage_messages(
        r#"
fn readOne(n: Int) -> Result<Int, String>
    match n
        0 -> Result.Err("zero")
        _ -> Result.Ok(n)

verify readOne
    readOne(7)? => 7
    readOne(0) => Result.Err("zero")
"#,
    );
    assert_absent(&messages, "Result.Ok");
    assert_absent(&messages, "Result.Err");
}

#[test]
fn error_propagation_under_a_projection_counts_as_an_ok_case() {
    let messages = coverage_messages(
        r#"
record Read
    value: Int
    rest: List<Int>

fn readOne(bytes: List<Int>) -> Result<Read, String>
    match bytes
        [] -> Result.Err("empty input")
        [head, ..tail] -> Result.Ok(Read(value = head, rest = tail))

verify readOne
    readOne([7, 9])?.value => 7
"#,
    );
    assert_absent(&messages, "Result.Ok");
}

#[test]
fn error_propagation_nested_in_another_call_counts_as_an_ok_case() {
    let messages = coverage_messages(
        r#"
fn readOne(n: Int) -> Result<Int, String>
    match n
        0 -> Result.Err("zero")
        _ -> Result.Ok(n)

verify readOne
    Int.abs(readOne(7)?) => 7
"#,
    );
    assert_absent(&messages, "Result.Ok");
}

#[test]
fn error_propagation_on_a_different_function_is_not_an_ok_case() {
    let messages = coverage_messages(
        r#"
fn source(n: Int) -> Result<Int, String>
    Result.Ok(n)

fn alwaysFail(n: Int) -> Result<Int, String>
    Result.Err("nope")

verify alwaysFail
    alwaysFail(source(1)?) => Result.Err("nope")
"#,
    );
    assert_present(
        &messages,
        "verify examples for alwaysFail do not include any Result.Ok case",
    );
}

// ---------------------------------------------------------------------------
// Coverage flowing from a caller to a private helper
// ---------------------------------------------------------------------------

const PRIVATE_HELPER: &str = r#"
module Decoder
    exposes [decode]
    intent = "Read the first byte of a list."
    effects []

fn decode(bytes: List<Int>) -> Result<Int, String>
    ? "The only public entry."
    readOne(bytes)

verify decode
    decode([7, 9]) => Result.Ok(7)
    decode([]) => Result.Err("empty input")

fn readOne(bytes: List<Int>) -> Result<Int, String>
    ? "Private plumbing."
    match bytes
        [] -> Result.Err("empty input")
        [head, ..tail] -> Result.Ok(head)

verify readOne
    readOne([7, 9])? => 7
"#;

#[test]
fn flow_covers_a_private_single_caller_helper() {
    let messages = coverage_messages(PRIVATE_HELPER);
    assert_absent(&messages, "verify examples for readOne do not include any");
}

#[test]
fn flow_leaves_input_shape_warnings_alone() {
    // The caller's arms say nothing about the helper's argument domain, so
    // the input-shape checks survive the relaxation.
    let messages = coverage_messages(PRIVATE_HELPER);
    assert_present(
        &messages,
        "verify examples for readOne do not cover empty list input for `bytes`",
    );
}

#[test]
fn flow_covers_a_transitive_chain_of_private_helpers() {
    let messages = coverage_messages(
        r#"
module Decoder
    exposes [decode]
    intent = "Read a pair of bytes."
    effects []

fn decode(bytes: List<Int>) -> Result<Int, String>
    readPair(bytes)

verify decode
    decode([7, 9]) => Result.Ok(7)
    decode([]) => Result.Err("empty input")

fn readPair(bytes: List<Int>) -> Result<Int, String>
    readByte(bytes)

verify readPair
    readPair([7, 9]) => Result.Ok(7)

fn readByte(bytes: List<Int>) -> Result<Int, String>
    match bytes
        [] -> Result.Err("empty input")
        [head, ..tail] -> Result.Ok(head)

verify readByte
    readByte([7, 9]) => Result.Ok(7)
"#,
    );
    assert_absent(&messages, "verify examples for readPair do not include any");
    assert_absent(&messages, "verify examples for readByte do not include any");
}

#[test]
fn flow_covers_a_mutual_recursion_group_with_one_external_caller() {
    let messages = coverage_messages(
        r#"
module Reader
    exposes [readAll]
    intent = "Reverse a byte list, rejecting zero bytes."
    effects []

fn readAll(bytes: List<Int>) -> Result<List<Int>, String>
    readInto(bytes, [])

verify readAll
    readAll([1, 2]) => Result.Ok([2, 1])
    readAll([0]) => Result.Err("zero is not allowed")

fn readInto(bytes: List<Int>, acc: List<Int>) -> Result<List<Int>, String>
    match bytes
        [] -> Result.Ok(acc)
        [head, ..tail] -> readOne(head, tail, acc)

verify readInto
    readInto([1], [])? => [1]

fn readOne(head: Int, tail: List<Int>, acc: List<Int>) -> Result<List<Int>, String>
    match head == 0
        true -> Result.Err("zero is not allowed")
        false -> readInto(tail, List.prepend(head, acc))

verify readOne
    readOne(1, [], [])? => [1]
"#,
    );
    assert_absent(&messages, "verify examples for readInto do not include any");
    assert_absent(&messages, "verify examples for readOne do not include any");
}

#[test]
fn flow_covers_a_self_recursive_helper() {
    let messages = coverage_messages(
        r#"
module Reader
    exposes [readAll]
    intent = "Drain a byte list."
    effects []

fn readAll(bytes: List<Int>) -> Result<Int, String>
    drain(bytes, 0)

verify readAll
    readAll([1, 2]) => Result.Ok(2)
    readAll([0]) => Result.Err("zero is not allowed")

fn drain(bytes: List<Int>, seen: Int) -> Result<Int, String>
    match bytes
        [] -> Result.Ok(seen)
        [head, ..tail] -> match head == 0
            true -> Result.Err("zero is not allowed")
            false -> drain(tail, seen + 1)

verify drain
    drain([1, 2], 0) => Result.Ok(2)
"#,
    );
    assert_absent(&messages, "verify examples for drain do not include any");
}

// ---------------------------------------------------------------------------
// Shapes the rule must keep warning about
// ---------------------------------------------------------------------------

#[test]
fn flow_does_not_cover_a_helper_with_two_external_callers() {
    let messages = coverage_messages(
        r#"
module Decoder
    exposes [decode, decodeAgain]
    intent = "Two entry points over one helper."
    effects []

fn decode(bytes: List<Int>) -> Result<Int, String>
    readByte(bytes)

verify decode
    decode([7]) => Result.Ok(7)
    decode([]) => Result.Err("empty input")

fn decodeAgain(bytes: List<Int>) -> Result<Int, String>
    readByte(bytes)

verify decodeAgain
    decodeAgain([7]) => Result.Ok(7)
    decodeAgain([]) => Result.Err("empty input")

fn readByte(bytes: List<Int>) -> Result<Int, String>
    match bytes
        [] -> Result.Err("empty input")
        [head, ..tail] -> Result.Ok(head)

verify readByte
    readByte([7]) => Result.Ok(7)
"#,
    );
    assert_present(
        &messages,
        "verify examples for readByte do not include any Result.Err case",
    );
}

#[test]
fn flow_does_not_cover_an_exposed_helper() {
    let messages = coverage_messages(
        r#"
module Decoder
    exposes [decode, readByte]
    intent = "The helper is part of the public surface."
    effects []

fn decode(bytes: List<Int>) -> Result<Int, String>
    readByte(bytes)

verify decode
    decode([7]) => Result.Ok(7)
    decode([]) => Result.Err("empty input")

fn readByte(bytes: List<Int>) -> Result<Int, String>
    match bytes
        [] -> Result.Err("empty input")
        [head, ..tail] -> Result.Ok(head)

verify readByte
    readByte([7]) => Result.Ok(7)
"#,
    );
    assert_present(
        &messages,
        "verify examples for readByte do not include any Result.Err case",
    );
}

#[test]
fn flow_does_not_cover_when_the_only_caller_lacks_an_arm() {
    let messages = coverage_messages(
        r#"
module Decoder
    exposes [decode]
    intent = "The entry point pins only the happy path."
    effects []

fn decode(bytes: List<Int>) -> Result<Int, String>
    readByte(bytes)

verify decode
    decode([7]) => Result.Ok(7)

fn readByte(bytes: List<Int>) -> Result<Int, String>
    match bytes
        [] -> Result.Err("empty input")
        [head, ..tail] -> Result.Ok(head)

verify readByte
    readByte([7]) => Result.Ok(7)
"#,
    );
    assert_present(
        &messages,
        "verify examples for decode do not include any Result.Err case",
    );
    assert_present(
        &messages,
        "verify examples for readByte do not include any Result.Err case",
    );
}

#[test]
fn flow_does_not_cover_when_the_caller_return_type_has_no_shape() {
    let messages = coverage_messages(
        r#"
module Decoder
    exposes [decode]
    intent = "The entry point returns a plain Int."
    effects []

fn decode(bytes: List<Int>) -> Int
    match readByte(bytes)
        Result.Ok(value) -> value
        Result.Err(msg) -> 0

verify decode
    decode([7]) => 7
    decode([]) => 0

fn readByte(bytes: List<Int>) -> Result<Int, String>
    match bytes
        [] -> Result.Err("empty input")
        [head, ..tail] -> Result.Ok(head)

verify readByte
    readByte([7]) => Result.Ok(7)
"#,
    );
    assert_present(
        &messages,
        "verify examples for readByte do not include any Result.Err case",
    );
}

#[test]
fn flow_does_not_cover_a_helper_called_from_a_top_level_statement() {
    let messages = coverage_messages(
        r#"
module Decoder
    exposes [decode]
    intent = "A top-level statement reaches the helper directly."
    effects []

fn decode(bytes: List<Int>) -> Result<Int, String>
    readByte(bytes)

verify decode
    decode([7]) => Result.Ok(7)
    decode([]) => Result.Err("empty input")

fn readByte(bytes: List<Int>) -> Result<Int, String>
    match bytes
        [] -> Result.Err("empty input")
        [head, ..tail] -> Result.Ok(head)

verify readByte
    readByte([7]) => Result.Ok(7)

probe = readByte([1])
"#,
    );
    assert_present(
        &messages,
        "verify examples for readByte do not include any Result.Err case",
    );
}

#[test]
fn flow_does_not_cover_a_helper_passed_as_a_value() {
    let messages = coverage_messages(
        r#"
module Decoder
    exposes [decode, run]
    intent = "The helper also escapes as a function value."
    effects []

fn decode(bytes: List<Int>) -> Result<Int, String>
    readByte(bytes)

verify decode
    decode([7]) => Result.Ok(7)
    decode([]) => Result.Err("empty input")

fn run(bytes: List<Int>) -> Result<Int, String>
    apply(readByte, bytes)

verify run
    run([7]) => Result.Ok(7)
    run([]) => Result.Err("empty input")

fn apply(f: Fn(List<Int>) -> Result<Int, String>, bytes: List<Int>) -> Result<Int, String>
    f(bytes)

verify apply
    apply(readByte, [7]) => Result.Ok(7)
    apply(readByte, []) => Result.Err("empty input")

fn readByte(bytes: List<Int>) -> Result<Int, String>
    match bytes
        [] -> Result.Err("empty input")
        [head, ..tail] -> Result.Ok(head)

verify readByte
    readByte([7]) => Result.Ok(7)
"#,
    );
    assert_present(
        &messages,
        "verify examples for readByte do not include any Result.Err case",
    );
}

#[test]
fn flow_does_not_fire_without_a_module_declaration() {
    // Without an `exposes` list every name that does not start with `_`
    // counts as exposed, so a single-file script gets no relaxation.
    let messages = coverage_messages(
        r#"
fn decode(bytes: List<Int>) -> Result<Int, String>
    readByte(bytes)

verify decode
    decode([7]) => Result.Ok(7)
    decode([]) => Result.Err("empty input")

fn readByte(bytes: List<Int>) -> Result<Int, String>
    match bytes
        [] -> Result.Err("empty input")
        [head, ..tail] -> Result.Ok(head)

verify readByte
    readByte([7]) => Result.Ok(7)
"#,
    );
    assert_present(
        &messages,
        "verify examples for readByte do not include any Result.Err case",
    );
}

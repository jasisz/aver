#![cfg(feature = "wasm")]

//! Regression — match shapes the wasm-gc body emitter used to leave as a
//! silent `unreachable` stub, or lowered wrongly, while the VM ran them.
//!
//! Before: a fn the emitter could not lower compiled clean and trapped the
//! first time it ran. The shapes here were the ones a scan of the language
//! surface found: `String` / `Float` literals and nested tuples as tuple
//! elements, a `Float` literal match, a match whose first arm is `_` or a
//! binder over a `Bool` / `Float` / record / list / `Option` / tuple /
//! `String` subject, a named catch-all after constructor arms, a
//! module-level binding read from a fn, and a `Unit` body ending in a
//! binding. Two shapes compiled but answered
//! wrongly: a named catch-all in a `String` or sum-type match never stored
//! the subject in its binder, and a computed `Int` subject was evaluated
//! again for every literal arm. A shape that still cannot be lowered is now
//! a compile error naming the fn; `wasm_gc_no_trap_stubs` pins that.
//!
//! The pure shapes run through both the VM and the wasm-gc verify runner
//! with the VM's answers as expectations. The two that need effects or
//! module-level code run as programs on both backends and compare stdout.

use aver::checker::VerifyResult;
use aver::diagnostics::vm_verify::run_verify_for_items_vm;
use aver::diagnostics::wasm_gc_verify::run_verify_for_items_wasm_gc;
use aver::source::parse_source;

const FILE: &str = "wasm_gc_match_shapes_regression.av";

fn assert_all_pass_on_both(source: &str, expected_passed: usize) {
    let items = parse_source(source).unwrap_or_else(|e| {
        panic!("parse failed: {e}\n--- source ---\n{source}");
    });
    let vm = run_verify_for_items_vm(items.clone(), None, None, FILE)
        .unwrap_or_else(|e| panic!("VM verify failed: {e}\n--- source ---\n{source}"));
    let gc = run_verify_for_items_wasm_gc(items, None, None, FILE)
        .unwrap_or_else(|e| panic!("wasm-gc verify failed: {e}\n--- source ---\n{source}"));
    for (backend, results) in [("vm", vm), ("wasm-gc", gc)] {
        let results: Vec<VerifyResult> = results;
        let passed: usize = results.iter().map(|r| r.passed).sum();
        let failed: usize = results.iter().map(|r| r.failed).sum();
        let skipped: usize = results.iter().map(|r| r.skipped).sum();
        let failures: Vec<_> = results.iter().flat_map(|r| r.failures.iter()).collect();
        assert_eq!(
            (passed, failed, skipped),
            (expected_passed, 0, 0),
            "[{backend}] expected {expected_passed}/0/0 passed/failed/skipped, got {passed}/{failed}/{skipped}; failures={failures:?}\n--- source ---\n{source}"
        );
    }
}

/// Run `source` as a program on the VM and on wasm-gc (with every trap
/// stub refused) and return both stdouts.
fn run_both(source: &str) -> (String, String) {
    let dir = tempfile::tempdir().expect("tempdir");
    let entry = dir.path().join("main.av");
    std::fs::write(&entry, source).expect("write entry");
    let run = |wasm: bool| {
        let mut cmd = std::process::Command::new(env!("CARGO_BIN_EXE_aver"));
        cmd.arg("run").arg(&entry);
        if wasm {
            cmd.arg("--wasm-gc").env("AVER_WASMGC_REQUIRE_MIR", "1");
        }
        let out = cmd.output().expect("run aver");
        assert!(
            out.status.success(),
            "{} run failed:\n{}\n--- source ---\n{source}",
            if wasm { "wasm-gc" } else { "VM" },
            String::from_utf8_lossy(&out.stderr)
        );
        String::from_utf8_lossy(&out.stdout).into_owned()
    };
    (run(false), run(true))
}

#[test]
fn tuple_elements_with_string_and_float_literals() {
    assert_all_pass_on_both(
        r#"
fn route(method: String, depth: Int) -> Int
    ? "Pick by a String literal and an Int literal together."
    match (method, depth)
        ("GET", 0) -> 1
        ("POST", d) -> d
        _ -> 0

fn scale(x: Float, n: Int) -> Int
    ? "Pick by a Float literal."
    match (x, n)
        (0.5, 0) -> 1
        (1.5, k) -> k
        _ -> 0

verify route
    route("GET", 0) => 1
    route("POST", 7) => 7
    route("GET", 1) => 0
    route("PUT", 0) => 0

verify scale
    scale(0.5, 0) => 1
    scale(1.5, 9) => 9
    scale(2.5, 0) => 0
"#,
        7,
    );
}

#[test]
fn nested_tuple_patterns() {
    assert_all_pass_on_both(
        r#"
fn pick(p: Tuple<Int, Int>, n: Int) -> Int
    ? "Test inside a nested tuple."
    match (p, n)
        ((0, b), c) -> b + c
        ((a, _), 0) -> a
        _ -> 99

fn flat(p: Tuple<Int, Tuple<String, Int>>) -> Int
    ? "Destructure a nested tuple in one arm."
    match p
        (a, (_, c)) -> a + c

verify pick
    pick((0, 2), 3) => 5
    pick((4, 2), 0) => 4
    pick((4, 2), 1) => 99

verify flat
    flat((1, ("x", 2))) => 3
"#,
        4,
    );
}

#[test]
fn float_literal_match_on_a_slot_and_on_a_computed_subject() {
    assert_all_pass_on_both(
        r#"
fn half(x: Float) -> Int
    ? "Match a Float slot against literals."
    match x
        0.5 -> 1
        1.5 -> 2
        other -> Float.round(other)

fn root(x: Float) -> Int
    ? "Match a computed Float against a literal."
    match Float.sqrt(x)
        2.0 -> 1
        _ -> 0

verify half
    half(0.5) => 1
    half(1.5) => 2
    half(4.2) => 4

verify root
    root(4.0) => 1
    root(9.0) => 0
"#,
        5,
    );
}

#[test]
fn irrefutable_first_arm_on_every_subject_kind() {
    assert_all_pass_on_both(
        r#"
record Point
    x: Int

fn onBool(b: Bool) -> Int
    ? "A wildcard over a Bool."
    match b
        _ -> 1

fn keepBool(b: Bool) -> Bool
    ? "A binder over a Bool."
    match b
        same -> same

fn onFloat(x: Float) -> Int
    ? "A wildcard over a Float."
    match x
        _ -> 2

fn onRecord(p: Point) -> Int
    ? "A wildcard over a record."
    match p
        _ -> 3

fn onList(l: List<Int>) -> Int
    ? "A wildcard over a list."
    match l
        _ -> 4

fn onOption(o: Option<Int>) -> Int
    ? "A wildcard over an Option."
    match o
        _ -> 5

fn onTuple(t: Tuple<Int, Int>) -> Int
    ? "A wildcard over a tuple."
    match t
        _ -> 6

fn onString(s: String) -> Int
    ? "A wildcard over a String."
    match s
        _ -> 7

fn keepString(s: String) -> String
    ? "A binder over a String."
    match s
        same -> same

verify onBool
    onBool(true) => 1

verify keepBool
    keepBool(false) => false

verify onFloat
    onFloat(1.0) => 2

verify onRecord
    onRecord(Point(x = 1)) => 3

verify onList
    onList([1]) => 4

verify onOption
    onOption(Option.None) => 5

verify onTuple
    onTuple((1, 2)) => 6

verify onString
    onString("a") => 7

verify keepString
    keepString("kept") => "kept"
"#,
        9,
    );
}

/// The named catch-all used to leave its binder unset on wasm-gc: the
/// `String` match returned a null string and the sum-type match passed a
/// null on, which then took the wrong arm. Option, Result and List matches
/// with a named catch-all were trap stubs.
#[test]
fn named_catch_all_after_testing_arms_captures_the_subject() {
    assert_all_pass_on_both(
        r#"
type Shape
    Dot
    Box(Int, Int)
    Ring(Int)

fn describe(s: Shape) -> String
    ? "Name every shape."
    match s
        Shape.Box(a, b) -> "box {a} {b}"
        Shape.Ring(r) -> "ring {r}"
        Shape.Dot -> "dot"

fn name(s: Shape) -> String
    ? "Special-case a dot, describe the rest."
    match s
        Shape.Dot -> "just a dot"
        other -> describe(other)

fn shout(s: String) -> String
    ? "Special-case one word, pass the rest on."
    match s
        "a" -> "A"
        other -> other

fn optionOr(o: Option<Int>) -> Int
    ? "Named catch-all after None."
    match o
        Option.None -> 0
        other -> Option.withDefault(other, 5)

fn resultOr(r: Result<Int, String>) -> Int
    ? "Named catch-all after Err."
    match r
        Result.Err(_) -> 0
        other -> Result.withDefault(other, 5)

fn listLen(l: List<Int>) -> Int
    ? "Named catch-all after the empty list."
    match l
        [] -> 0
        other -> List.len(other)

verify name
    name(Shape.Dot) => "just a dot"
    name(Shape.Ring(2)) => "ring 2"
    name(Shape.Box(1, 2)) => "box 1 2"

verify shout
    shout("a") => "A"
    shout("quiet") => "quiet"

verify optionOr
    optionOr(Option.Some(3)) => 3
    optionOr(Option.None) => 0

verify resultOr
    resultOr(Result.Ok(3)) => 3

verify listLen
    listLen([1, 2]) => 2
    listLen([]) => 0
"#,
        10,
    );
}

/// A computed `Int` subject matched against literals was evaluated again
/// for every literal arm on wasm-gc, repeating its effects.
#[test]
fn computed_int_subject_runs_once() {
    let (vm, wasm) = run_both(
        r#"module Once
    intent = "A computed match subject runs once."

fn roll() -> Int
    ? "Say something, then answer."
    ! [Console.print]
    Console.print("rolled")
    3

fn main() -> Unit
    ? "Match the roll against literals."
    ! [Console.print]
    r = match roll()
        1 -> "one"
        2 -> "two"
        n -> "other {n}"
    Console.print(r)
"#,
    );
    assert_eq!(vm, "rolled\nother 3\n", "the VM is the reference");
    assert_eq!(wasm, vm);
}

/// A module-level binding read from `main` and from another fn. The VM
/// keeps it in a global; wasm-gc substitutes its value where it is read.
#[test]
fn module_level_binding_read_from_fns() {
    let (vm, wasm) = run_both(
        r#"module Settings
    intent = "Module-level values read from functions."

base = 40

label = "n{base}"

fn shifted(x: Int) -> Int
    ? "Read a module-level Int."
    x + base

fn main() -> Unit
    ? "Read module-level values."
    ! [Console.print]
    Console.print("{shifted(2)} {label}")
"#,
    );
    assert_eq!(vm, "42 n40\n", "the VM is the reference");
    assert_eq!(wasm, vm);
}

/// A `Unit` fn whose body ends in a binding (`_ = add(2, 3)`) has the type
/// `Unit`, as the checker types it. MIR lowering refused it, so the VM
/// stopped with an internal error and wasm-gc left a trap stub.
#[test]
fn unit_body_ending_in_a_binding() {
    let (vm, wasm) = run_both(
        r#"module Tail
    intent = "A body may end in a binding."

fn add(a: Int, b: Int) -> Int
    ? "Say the sum, then return it."
    ! [Console.print]
    Console.print("{a}+{b}")
    a + b

fn main() -> Unit
    ? "Discard the sum."
    ! [Console.print]
    _ = add(2, 3)
"#,
    );
    assert_eq!(vm, "2+3\n", "the VM is the reference");
    assert_eq!(wasm, vm);
}

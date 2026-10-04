#![cfg(feature = "wasm")]

//! Regression — a multi-arm `match (a, b)` whose tuple elements are user
//! variant patterns (`(Pat.PLit(x), Pat.PLit(y)) -> …; _ -> …`).
//!
//! The wasm-gc tuple cascade admitted only `Result`/`Option` tags, list
//! shapes and `Int`/`Bool` literals as element tests. A user variant fell
//! outside that set, so the MIR emitter gave up on the whole fn and the
//! module carried a silent trap stub in its place: the program compiled
//! clean, ran fine on the VM and in Rust, and hit `unreachable` on
//! wasm-gc the first time the fn was called. The proof-step replayer's
//! `headsDiffer` was the first program to reach it.
//!
//! Every block here runs through both the VM verify runner and the
//! wasm-gc verify runner with the VM's answers as expectations, so a pass
//! on both is a cross-backend parity check for these shapes.

use aver::checker::VerifyResult;
use aver::diagnostics::vm_verify::run_verify_for_items_vm;
use aver::diagnostics::wasm_gc_verify::run_verify_for_items_wasm_gc;
use aver::source::parse_source;

fn run_both_backends(source: &str) -> [(&'static str, Vec<VerifyResult>); 2] {
    let items = parse_source(source).unwrap_or_else(|e| {
        panic!("parse failed: {e}\n--- source ---\n{source}");
    });
    let file = "wasm_gc_tuple_variant_match_regression.av";
    let vm = run_verify_for_items_vm(items.clone(), None, None, file)
        .unwrap_or_else(|e| panic!("VM verify failed: {e}\n--- source ---\n{source}"));
    let gc = run_verify_for_items_wasm_gc(items, None, None, file)
        .unwrap_or_else(|e| panic!("wasm-gc verify failed: {e}\n--- source ---\n{source}"));
    [("vm", vm), ("wasm-gc", gc)]
}

fn assert_all_pass_on_both(source: &str, expected_passed: usize) {
    for (backend, results) in run_both_backends(source) {
        let passed: usize = results.iter().map(|r| r.passed).sum();
        let failed: usize = results.iter().map(|r| r.failed).sum();
        let skipped: usize = results.iter().map(|r| r.skipped).sum();
        let failures: Vec<_> = results
            .iter()
            .flat_map(|result| result.failures.iter())
            .collect();
        assert_eq!(
            (passed, failed, skipped),
            (expected_passed, 0, 0),
            "[{backend}] expected {expected_passed}/0/0 passed/failed/skipped, got {passed}/{failed}/{skipped}; failures={failures:?}\n--- source ---\n{source}"
        );
    }
}

/// The replayer's shape: two patterns of a sum type, each arm testing both
/// elements' variants and binding their payloads, a recursive payload
/// compared with `!=`, and a catch-all.
#[test]
fn tuple_of_user_variants_with_payload_binds() {
    assert_all_pass_on_both(
        r#"
type Term
    TInt(Int)
    TCall(String, List<Term>)

type Pat
    PWild
    PLit(Term)
    PCtor(String, List<String>)

fn headsDiffer(a: Pat, b: Pat) -> Bool
    ? "Two patterns no value can both match."
    match (a, b)
        (Pat.PLit(x), Pat.PLit(y)) -> x != y
        (Pat.PCtor(c, xs), Pat.PCtor(d, ys)) -> c != d
        _ -> false

verify headsDiffer
    headsDiffer(Pat.PLit(Term.TInt(1)), Pat.PLit(Term.TInt(2))) => true
    headsDiffer(Pat.PLit(Term.TCall("f", [Term.TInt(1)])), Pat.PLit(Term.TCall("f", [Term.TInt(1)]))) => false
    headsDiffer(Pat.PCtor("A", []), Pat.PCtor("B", ["x"])) => true
    headsDiffer(Pat.PCtor("A", ["y"]), Pat.PCtor("A", [])) => false
    headsDiffer(Pat.PWild, Pat.PLit(Term.TInt(1))) => false
    headsDiffer(Pat.PLit(Term.TInt(1)), Pat.PCtor("A", [])) => false
"#,
        6,
    );
}

/// User variants next to other element tests: a nullary variant, an
/// `Option` tag and an `Int` literal in the same tuple, with wildcard
/// payload binds.
#[test]
fn tuple_mixing_user_variants_with_other_element_tests() {
    assert_all_pass_on_both(
        r#"
type Shape
    Dot
    Box(Int, Int)

fn classify(s: Shape, o: Option<Int>, n: Int) -> Int
    ? "Pick an arm by the shape, the option and the number."
    match (s, o, n)
        (Shape.Dot, Option.None, 0) -> 1
        (Shape.Box(w, _), Option.Some(v), _) -> w + v
        (Shape.Box(_, h), Option.None, k) -> h * k
        (Shape.Dot, _, k) -> k
        _ -> 0

verify classify
    classify(Shape.Dot, Option.None, 0) => 1
    classify(Shape.Box(3, 4), Option.Some(10), 7) => 13
    classify(Shape.Box(3, 4), Option.None, 5) => 20
    classify(Shape.Dot, Option.Some(1), 9) => 9
    classify(Shape.Dot, Option.None, 2) => 2
"#,
        5,
    );
}

/// A newtype variant is the whole type, so it binds the element without a
/// test.
#[test]
fn tuple_with_newtype_variant_element() {
    assert_all_pass_on_both(
        r#"
type Name
    Name(String)

type Slot
    Empty
    Held(String)

fn fits(n: Name, s: Slot) -> Bool
    ? "A name fits a slot that is empty or holds the same name."
    match (n, s)
        (Name.Name(a), Slot.Held(b)) -> a == b
        (Name.Name(_), Slot.Empty) -> true
        _ -> false

verify fits
    fits(Name.Name("a"), Slot.Held("a")) => true
    fits(Name.Name("a"), Slot.Held("b")) => false
    fits(Name.Name("a"), Slot.Empty) => true
"#,
        3,
    );
}

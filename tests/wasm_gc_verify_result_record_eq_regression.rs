#![cfg(feature = "wasm")]

//! Regression — comparing `Result` values on wasm-gc when one module
//! registers TWO `Result<RecordX, String>` instantiations.
//!
//! The `Result.Ok` / `Result.Err` constructor emitter resolved which
//! `Result<T,E>` struct to build by, in order: the single registered
//! instantiation, the enclosing fn's return type, then the first
//! registered instantiation whose payload-position type matches the
//! payload. Inside a synthesized verify `==` check fn the return type
//! is `Bool`, so an RHS `Result.Err("…")` fell to the payload match —
//! and with two instantiations sharing the error type `String`
//! (`Result<Point, String>` and `Result<Label, String>`) the FIRST one
//! always won. The per-instantiation eq helper for the LHS's actual
//! type then `ref.cast`s the mis-built struct to the other
//! instantiation's heap type and traps at runtime: the second record's
//! `=> Result.Err(…)` case aborted with a wasm trap while the first
//! record's identical shape passed. (`examples/core/order_total.av`:
//! `mkDiscount` green, `mkTaxRate` trapped.)
//!
//! The fix consults the constructor expression's own stamped type
//! first — the type checker already unified the concrete
//! `Result<T,E>` at the comparison site — and only falls back to the
//! positional heuristics when the stamp is not a registered
//! instantiation.

use aver::checker::VerifyResult;
use aver::diagnostics::wasm_gc_verify::run_verify_for_items_wasm_gc;
use aver::source::parse_source;

fn run_verify(source: &str) -> Vec<VerifyResult> {
    let items = parse_source(source).unwrap_or_else(|e| {
        panic!("parse failed: {e}\n--- source ---\n{source}");
    });
    run_verify_for_items_wasm_gc(
        items,
        None,
        None,
        "wasm_gc_verify_result_record_eq_regression.av",
    )
    .unwrap_or_else(|e| panic!("wasm-gc verify failed: {e}\n--- source ---\n{source}"))
}

fn assert_all_cases_pass(source: &str, expected_passed: usize) {
    let results = run_verify(source);
    let passed: usize = results.iter().map(|r| r.passed).sum();
    let failed: usize = results.iter().map(|r| r.failed).sum();
    let skipped: usize = results.iter().map(|r| r.skipped).sum();
    assert_eq!(
        (passed, failed, skipped),
        (expected_passed, 0, 0),
        "expected {expected_passed}/0/0 passed/failed/skipped, got {passed}/{failed}/{skipped}\n--- source ---\n{source}"
    );
}

/// Two records, each wrapped in its own `Result<RecordX, String>`,
/// both verified against `Result.Ok(...)` and `Result.Err("...")`.
/// Before the fix the SECOND block's Err case trapped (the Err
/// constructor built the first instantiation's struct; the eq helper
/// cast it to the second's heap type).
#[test]
fn two_result_record_instantiations_err_cases_pass() {
    assert_all_cases_pass(
        r#"
record Point
    x: Int
    y: Int

record Label
    text: String

fn mkPoint(x: Int, y: Int) -> Result<Point, String>
    ? "Builds a point, rejecting negatives"
    match x < 0
        true  -> Result.Err("negative")
        false -> Result.Ok(Point(x = x, y = y))

fn mkLabel(text: String) -> Result<Label, String>
    ? "Builds a label, rejecting empty text"
    match text == ""
        true  -> Result.Err("empty")
        false -> Result.Ok(Label(text = text))

verify mkPoint
    mkPoint(1, 2) => Result.Ok(Point(x = 1, y = 2))
    mkPoint(0 - 1, 2) => Result.Err("negative")

verify mkLabel
    mkLabel("hi") => Result.Ok(Label(text = "hi"))
    mkLabel("") => Result.Err("empty")
"#,
        4,
    );
}

/// The dual ambiguity: two instantiations sharing the OK type
/// (`Result<Int, …>` twice) so the `Result.Ok` payload match is the
/// ambiguous one. The stamped-type resolution covers both positions.
#[test]
fn two_result_instantiations_sharing_ok_type() {
    assert_all_cases_pass(
        r#"
record ParseFault
    at: Int

fn positive(n: Int) -> Result<Int, String>
    ? "Accepts positive numbers"
    match n > 0
        true  -> Result.Ok(n)
        false -> Result.Err("not positive")

fn parsed(n: Int) -> Result<Int, ParseFault>
    ? "Accepts non-negative numbers"
    match n < 0
        true  -> Result.Err(ParseFault(at = n))
        false -> Result.Ok(n)

verify positive
    positive(4) => Result.Ok(4)
    positive(0) => Result.Err("not positive")

verify parsed
    parsed(5) => Result.Ok(5)
    parsed(0 - 2) => Result.Err(ParseFault(at = 0 - 2))
"#,
        4,
    );
}

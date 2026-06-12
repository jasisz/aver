#![cfg(feature = "wasm")]

//! Regression — `verify ... => Option.None` cases on the wasm-gc backend.
//!
//! A bare `Option.None` carries no payload to fix its `T`, so plain
//! inference stamps it `Option<T>` — and `Spanned::set_ty` is set-once,
//! so the imprecision is permanent. Two stamping sites produced the
//! generic stamp:
//!
//!   1. `check_verify_blocks` inferred each verify case's RHS without
//!      an expected type. The wasm-gc verify runner then clones the
//!      stamped RHS into its synthesized `__verify_X_check() -> Bool`
//!      helper (`lhs == rhs`), and the emitter's `Option.None`
//!      constructor arm fails with "Option constructor: instantiation
//!      `Option<T>` was not registered" — every Option-returning fn
//!      with a `=> Option.None` case was unrunnable on `verify
//!      --wasm-gc` while passing `aver check` and VM verify cleanly.
//!   2. `Expr::BinOp` equality inferred both operands without
//!      cross-propagation, so a user-written `f(x) == Option.None` in
//!      a fn BODY hit the same unregistered-instantiation error.
//!
//! The fix propagates the concrete side's type into the bare-None side
//! at both sites (the LHS calls the verified fn, so its type is exactly
//! the expected type). Shapes pinned here (from the original bug's
//! repro matrix):
//!   * fn returning Option<Int> via match, None + Some arms
//!   * fn returning Option<Bool> via match
//!   * fn whose body is a bare `Option.None` (minimal repro)
//!   * `f(x) == Option.None` written directly in a fn body
//!   * `=> Option.Some(payload)` control (green before the fix too;
//!     the payload fixes `T` without bidirectional help)

use aver::checker::VerifyResult;
use aver::diagnostics::wasm_gc_verify::run_verify_for_items_wasm_gc;
use aver::source::parse_source;

/// Parse + run the full `verify --wasm-gc` path (same entry the CLI
/// uses: raw parsed items in, wasm-gc compile + wasmtime execution
/// inside). Panics on a compile/instantiate error — exactly the
/// failure mode this regression pins.
fn run_verify(source: &str) -> Vec<VerifyResult> {
    let items = parse_source(source).unwrap_or_else(|e| {
        panic!("parse failed: {e}\n--- source ---\n{source}");
    });
    run_verify_for_items_wasm_gc(
        items,
        None,
        None,
        "wasm_gc_verify_option_case_regression.av",
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

/// A fn returning Option<Int> via match with a None arm and a Some
/// arm, verified against both `Option.Some(3)` and `Option.None`. The
/// minimal repro from the bug report.
#[test]
fn option_int_match_with_none_case() {
    assert_all_cases_pass(
        r#"
fn pick(n: Int) -> Option<Int>
    ? "None for zero, some n otherwise."
    match n == 0
        true -> Option.None
        false -> Option.Some(n)

verify pick
    pick(3) => Option.Some(3)
    pick(0) => Option.None
"#,
        2,
    );
}

/// A fn returning Option<Bool>; same shape over a Bool payload.
#[test]
fn option_bool_match_with_none_case() {
    assert_all_cases_pass(
        r#"
fn maybePos(n: Int) -> Option<Bool>
    ? "Some sign for nonzero."
    match n == 0
        true -> Option.None
        false -> Option.Some(n > 0)

verify maybePos
    maybePos(3) => Option.Some(true)
    maybePos(-3) => Option.Some(false)
    maybePos(0) => Option.None
"#,
        3,
    );
}

/// Minimal stamping repro — the fn body is a bare `Option.None` (the
/// fn-body stamp is fine via `current_fn_ret`; the failing stamp was
/// the verify case's RHS `Option.None`).
#[test]
fn bare_none_body_with_none_case() {
    assert_all_cases_pass(
        r#"
fn nothing(n: Int) -> Option<Int>
    ? "Always none."
    Option.None

verify nothing
    nothing(3) => Option.None
"#,
        1,
    );
}

/// User-written `f(x) == Option.None` in a fn BODY — the second
/// stamping site (`Expr::BinOp` equality without cross-propagation).
/// Fails wasm-gc compilation on the unfixed checker even without any
/// `=> Option.None` verify case.
#[test]
fn body_equality_against_bare_none() {
    assert_all_cases_pass(
        r#"
fn pick(n: Int) -> Option<Int>
    ? "None for zero, some n otherwise."
    match n == 0
        true -> Option.None
        false -> Option.Some(n)

fn isNone(n: Int) -> Bool
    ? "Whether pick returns none."
    pick(n) == Option.None

verify isNone
    isNone(0) => true
    isNone(2) => false
"#,
        2,
    );
}

/// Control — `=> Option.Some(payload)` only. The payload fixes `T`
/// without bidirectional help; green before the fix too.
#[test]
fn option_some_only_case_control() {
    assert_all_cases_pass(
        r#"
fn wrap(n: Int) -> Option<Int>
    ? "Always some."
    Option.Some(n)

verify wrap
    wrap(3) => Option.Some(3)
"#,
        1,
    );
}

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

// --- Generic-instantiation shapes (second wave). The four shapes
// below type-check and pass VM verify but failed wasm-gc compilation,
// for two distinct reasons:
//
//   * The Option-constructor emitter treated an Option-typed `Some`
//     payload as the FULL `Option<T>` canonical instead of wrapping
//     it (`Option.Some(Option.None)` under `Option<Option<Int>>`
//     resolved the `Option<Int>` slot and failed wasm validation),
//     and looked instantiations up by the raw `Type::display` string,
//     whose `", "` separator never matches the whitespace-free
//     registry keys (`Option<Tuple<Int, Int>>` vs
//     `Option<Tuple<Int,Int>>`).
//   * `check_verify_blocks` propagated the LHS type into the RHS only
//     for a BARE `Option.None` — a `Option.None` nested inside
//     `Option.Some(…)`, a list literal element, or a `Map.set` value
//     argument still plain-inferred and permanently stamped
//     `Option<T>` (set-once), which no backend can resolve to a slot.

/// Nested Option: `Option.Some(Option.None)` under
/// `Option<Option<Int>>`, in both the fn body (match arms) and the
/// verify RHS. Red without the constructor-canonical fix (fn body) and
/// without the verify-RHS expected-type propagation (verify case).
#[test]
fn nested_option_some_of_none() {
    assert_all_cases_pass(
        r#"
fn wrapInner(n: Int) -> Option<Option<Int>>
    ? "Inner none for zero."
    match n == 0
        true -> Option.Some(Option.None)
        false -> Option.Some(Option.Some(n))

verify wrapInner
    wrapInner(0) => Option.Some(Option.None)
    wrapInner(2) => Option.Some(Option.Some(2))
"#,
        2,
    );
}

/// Fully concrete compound payload: `Option<Tuple<Int, Int>>` with a
/// `=> Option.None` case. Red without whitespace normalisation of the
/// Option canonical — the type was registered (whitespace-free) but
/// the emit-time lookup key came from `Type::display` with `", "`.
#[test]
fn option_tuple_payload_with_none_case() {
    assert_all_cases_pass(
        r#"
fn pair(n: Int) -> Option<Tuple<Int, Int>>
    ? "Pair for positive."
    match n > 0
        true -> Option.Some((n, n))
        false -> Option.None

verify pair
    pair(2) => Option.Some((2, 2))
    pair(0) => Option.None
"#,
        2,
    );
}

/// List literal with an `Option.None` element on the verify RHS. The
/// fn body compiled (tail position gets the return type as expected);
/// the verify RHS plain-inferred and stamped `List<Option<T>>`. Red
/// without the verify-RHS expected-type propagation.
#[test]
fn list_of_option_literal_with_none_element() {
    assert_all_cases_pass(
        r#"
fn firstTwo(n: Int) -> List<Option<Int>>
    ? "List with a none and a some."
    [Option.None, Option.Some(n)]

verify firstTwo
    firstTwo(1) => [Option.None, Option.Some(1)]
"#,
        1,
    );
}

/// Single-element variant — no concrete sibling element to unify with,
/// so the expected type is the only source of `T`.
#[test]
fn list_of_option_literal_only_none() {
    assert_all_cases_pass(
        r#"
fn onlyNone(n: Int) -> List<Option<Int>>
    ? "Singleton none list."
    [Option.None]

verify onlyNone
    onlyNone(1) => [Option.None]
"#,
        1,
    );
}

/// `Option.None` as a `Map.set` VALUE argument on the verify RHS —
/// plain inference stamped the whole RHS `Map<String, Option<T>>` and
/// the backend could not lower `Option<T>`. Red without the verify-RHS
/// expected-type propagation.
#[test]
fn map_set_with_none_value_argument() {
    assert_all_cases_pass(
        r#"
fn stash(m: Map<String, Option<Int>>) -> Map<String, Option<Int>>
    ? "Stores a none value under k."
    Map.set(m, "k", Option.None)

verify stash
    stash({}) => Map.set({}, "k", Option.None)
"#,
        1,
    );
}

/// Tuple element with a bare `Option.None` in a match arm. The declared
/// tuple return type must drive the Option payload type all the way down
/// to the element expression.
#[test]
fn tuple_option_return_with_none_arm() {
    assert_all_cases_pass(
        r#"
fn pack(n: Int) -> Tuple<Option<Int>, Int>
    ? "Pair an option with its source."
    match n > 0
        true -> (Option.Some(n), n)
        false -> (Option.None, n)

fn firstOf(t: Tuple<Option<Int>, Int>) -> Option<Int>
    ? "First element."
    match t
        (a, b) -> a

verify firstOf
    firstOf(pack(3)) => Option.Some(3)
    firstOf(pack(-1)) => Option.None
"#,
        2,
    );
}

/// Tuple-shaped verify RHS with two `Result.Ok(...)` constructors. The
/// LHS tuple type supplies each `Result`'s error type on the expected side.
#[test]
fn tuple_result_expected_side_ok_constructors() {
    assert_all_cases_pass(
        r#"
fn both(a: Int, b: Int) -> Tuple<Result<Int, String>, Result<Int, String>>
    ? "Divide both ways."
    (Int.div(a, b), Int.div(b, a))

verify both
    both(4, 2) => (Result.Ok(2), Result.Ok(0))
"#,
        1,
    );
}

/// Direct tuple-return body with `Option.None` as the first element. This
/// was a checker-level false error before tuple expected-type propagation.
#[test]
fn direct_tuple_return_with_none_element() {
    assert_all_cases_pass(
        r#"
fn packNone(n: Int) -> Tuple<Option<Int>, Int>
    ? "Always pair none with n."
    (Option.None, n)

fn firstOf(t: Tuple<Option<Int>, Int>) -> Option<Int>
    ? "First element."
    match t
        (a, b) -> a

verify firstOf
    firstOf(packNone(5)) => Option.None
"#,
        1,
    );
}

/// Control — tuple element whose payload fixes `T` bottom-up.
#[test]
fn tuple_option_some_only_control() {
    assert_all_cases_pass(
        r#"
fn packSome(n: Int) -> Tuple<Option<Int>, Int>
    ? "Always pair some n with n."
    (Option.Some(n), n)

fn firstOf(t: Tuple<Option<Int>, Int>) -> Option<Int>
    ? "First element."
    match t
        (a, b) -> a

verify firstOf
    firstOf(packSome(5)) => Option.Some(5)
"#,
        1,
    );
}

/// Control — `Result.Err` verify cases plus `!=` against `Option.None`
/// and None-on-left equality in fn bodies. All green before the
/// second-wave fixes; pinned so the generalized verify-RHS propagation
/// cannot regress them.
#[test]
fn result_err_and_none_comparison_controls() {
    assert_all_cases_pass(
        r#"
fn tag(n: Int) -> Result<Int, String>
    ? "Ok for positive, error otherwise."
    match n > 0
        true -> Result.Ok(n)
        false -> Result.Err("neg")

fn pick(n: Int) -> Option<Int>
    ? "None for zero, some n otherwise."
    match n == 0
        true -> Option.None
        false -> Option.Some(n)

fn hasValue(n: Int) -> Bool
    ? "Whether pick returns a value."
    pick(n) != Option.None

fn noneOnLeft(n: Int) -> Bool
    ? "None compared from the left."
    Option.None == pick(n)

verify tag
    tag(3) => Result.Ok(3)
    tag(-1) => Result.Err("neg")

verify hasValue
    hasValue(0) => false
    hasValue(2) => true

verify noneOnLeft
    noneOnLeft(0) => true
    noneOnLeft(2) => false
"#,
        6,
    );
}

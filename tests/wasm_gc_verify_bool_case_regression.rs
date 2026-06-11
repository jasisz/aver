#![cfg(feature = "wasm")]

//! Regression — `verify ... => true` cases on the wasm-gc backend.
//!
//! The verify runner synthesizes `__verify_X_check() -> Bool` helpers
//! whose body is `lhs == rhs`. When the case's LHS type is `Bool`
//! (every `... => true` / `... => false` case), that helper contains a
//! `Bool == Bool` BinOp. `emit_mir_numeric_binop`'s instruction
//! selection had no i32 row, so Bool equality fell into the i64
//! catch-all (`i64.eq` over two i32 operands) and the WHOLE module
//! failed wasm validation with "type mismatch: expected i64, found
//! i32" — every verify file with a Bool-literal case was unrunnable on
//! wasm-gc while passing `aver check` and VM verify cleanly. The same
//! shape in a plain fn body (`fn q() -> Bool` returning a `Bool ==
//! Bool` expression) failed `aver compile --target wasm-gc` too.
//!
//! Shapes pinned here (from the original bug's repro matrix):
//!   * pure ordering-comparison Bool fn (`n > 0` — V16)
//!   * record-field ordering comparison (`r.num < r.den` — V17)
//!   * Bool-returning fn called with a nested user-call argument (V12)
//!   * direct `Bool == Bool` inside a fn body (V11)
//!   * `==`-only record-equality case as a control (V4 — green before
//!     the fix too; pins that the `__eq_<T>` helper path stays green)

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
    run_verify_for_items_wasm_gc(items, None, None, "wasm_gc_verify_bool_case_regression.av")
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

/// V16 — pure Int ordering comparison returned as Bool, verified
/// against a Bool literal. The 5-line minimal repro from the bug.
#[test]
fn bool_case_over_pure_int_ordering_comparison() {
    assert_all_cases_pass(
        r#"
fn pos(n: Int) -> Bool
    ? "Positive."
    n > 0

verify pos
    pos(3) => true
    pos(-3) => false
"#,
        2,
    );
}

/// V17 — ordering comparison over two record fields (no literal).
#[test]
fn bool_case_over_record_field_ordering_comparison() {
    assert_all_cases_pass(
        r#"
record Rat
    num: Int
    den: Int

fn rat(n: Int, d: Int) -> Rat
    ? "Build."
    Rat(num = n, den = d)

fn numLtDen(r: Rat) -> Bool
    ? "num < den."
    r.num < r.den

verify numLtDen
    numLtDen(rat(1, 2)) => true
"#,
        1,
    );
}

/// V12 — Bool-returning fn whose argument is a NESTED user call. The
/// only comparison in the file is `==`; the failing instruction was
/// still the synthesized check helper's `Bool == Bool`.
#[test]
fn bool_case_with_nested_user_call_argument() {
    assert_all_cases_pass(
        r#"
record Rat
    num: Int
    den: Int

fn rat(n: Int, d: Int) -> Rat
    ? "Build."
    Rat(num = n, den = d)

fn ratEq(a: Rat, b: Rat) -> Bool
    ? "Cross-mult equality."
    a.num * b.den == b.num * a.den

fn idRat(r: Rat) -> Rat
    ? "Identity."
    r

verify ratEq
    ratEq(idRat(rat(1, 2)), rat(2, 4)) => true
"#,
        1,
    );
}

/// V11 — the `Bool == Bool` BinOp written directly in a fn BODY (not
/// synthesized by verify): `q()` compares a Bool-returning call against
/// a Bool. Fails `compile --target wasm-gc` on the unfixed emitter.
#[test]
fn bool_eq_bool_inside_fn_body() {
    assert_all_cases_pass(
        r#"
fn pos(n: Int) -> Bool
    ? "Positive."
    n > 0

fn q(a: Int, b: Int) -> Bool
    ? "Both positive — Bool == Bool in the body."
    pos(a) == pos(b)

verify q
    q(1, 2) => true
    q(1, -2) => false
"#,
        2,
    );
}

/// V4 control — `==`-only record equality case. Lowers through the
/// per-type `__eq_<T>` helper, NOT the numeric BinOp tail; green both
/// before and after the i32 fix.
#[test]
fn record_equality_case_control() {
    assert_all_cases_pass(
        r#"
record Rat
    num: Int
    den: Int

fn rat(n: Int, d: Int) -> Rat
    ? "Build."
    Rat(num = n, den = d)

fn mk(n: Int) -> Rat
    ? "Build from one Int."
    rat(n, 1)

verify mk
    mk(4) => rat(4, 1)
"#,
        1,
    );
}

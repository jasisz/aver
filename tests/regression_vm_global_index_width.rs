//! Regression — VM verify over a large `given` domain must keep module
//! bindings and fn values apart.
//!
//! `LOAD_GLOBAL` / `STORE_GLOBAL` carry a `u16` index. Every fn used to take a
//! global slot, and `aver verify` compiles two or three helper fns per case,
//! so a domain past about 32 700 cases pushed the next index past 65 535. The
//! index was truncated without a check and wrapped onto a slot already in use:
//! `apply(f, x)` loaded a verify helper instead of `f` and every such case
//! failed with "CALL_VALUE ... exceeds local_count". Globals now hold module
//! bindings only (a fn read as a value resolves through its VM symbol), and an
//! index past the field refuses to compile.
//!
//! A domain at the old wrap point takes minutes in a debug build, for reasons
//! unrelated to globals, so this file pins the behaviour on a domain of a
//! thousand cases per law; `vm::compiler` unit tests pin that the global count does
//! not grow with the number of fns and that an index past `u16` is refused.

use aver::checker::VerifyResult;
use aver::diagnostics::vm_verify::{
    run_verify_for_items_vm_parallel_with_mode_and_bindings,
    run_verify_for_items_vm_with_mode_and_bindings,
};
use aver::source::parse_source;
use aver::verify_law::expand::ExpansionMode;

const DOMAIN: &str = "1..1000";

fn source(expected_offset: i64) -> String {
    format!(
        r#"module GlobalIndexWidth
    intent = "Verify over a large domain reads bindings and fn values."

base = 40

fn f(x: Int) -> Int
    ? "Adds the module binding."
    base + x

fn apply(g: Fn(Int) -> Int, x: Int) -> Int
    ? "Calls g on x."
    g(x)

verify f law readsBase
    given x: Int = {DOMAIN}
    f(x) => x + {expected_offset}

verify apply law fnValue
    given x: Int = {DOMAIN}
    apply(f, x) => x + {expected_offset}
"#
    )
}

fn verify(source: &str, parallel: bool) -> Vec<VerifyResult> {
    let items = parse_source(source).unwrap_or_else(|e| panic!("parse: {e:?}"));
    let file = "regression_vm_global_index_width.av";
    let mode = ExpansionMode::Declared;
    let result = if parallel {
        run_verify_for_items_vm_parallel_with_mode_and_bindings(items, None, None, file, mode, &[])
    } else {
        run_verify_for_items_vm_with_mode_and_bindings(items, None, None, file, mode, &[])
    };
    result.unwrap_or_else(|e| panic!("verify: {e}"))
}

fn totals(results: &[VerifyResult]) -> (usize, usize) {
    (
        results.iter().map(|r| r.passed).sum(),
        results.iter().map(|r| r.failed).sum(),
    )
}

#[test]
fn large_domain_reads_module_binding_and_fn_value() {
    for parallel in [false, true] {
        let results = verify(&source(40), parallel);
        assert_eq!(totals(&results), (2000, 0), "parallel = {parallel}");
    }
}

#[test]
fn large_domain_false_law_fails_every_case() {
    let results = verify(&source(41), true);
    assert_eq!(totals(&results), (0, 2000));
    for result in &results {
        for (_, expected, actual) in &result.failures {
            let expected: i64 = expected.parse().unwrap_or_else(|_| panic!("{expected}"));
            let actual: i64 = actual.parse().unwrap_or_else(|_| panic!("{actual}"));
            assert_eq!(actual + 1, expected, "f(x) is x + 40, not a helper's value");
        }
    }
}

//! Regression — `aver verify` on the VM must see module-level bindings.
//!
//! A module-level binding (`base = 40` outside any fn) lives in a VM global
//! that only the `__top_level__` chunk fills. `aver run` runs that chunk
//! before `main`; the verify runner compiled the same program but went
//! straight to the cases, so a fn reading `base` saw an empty slot. With
//! `fn f(x: Int) -> Int` returning `base + x`, verify computed `f(2)` as 4
//! while `run` printed 42: every case and every law over such a fn was judged
//! against a wrong value, so verify could reject a true case or accept a
//! false one (`f(2) => 4` passed).
//!
//! Pinned here: sequential, parallel and hostile VM verify agree with
//! `aver run` on the same source, for plain cases and for laws, including a
//! `given` domain and an expected side that read a binding, and a binding
//! defined through a fn call on another binding.

use std::process::Command;

use aver::checker::VerifyResult;
use aver::diagnostics::vm_verify::{
    run_verify_for_items_vm_parallel_with_mode_and_bindings,
    run_verify_for_items_vm_with_mode_and_bindings,
};
use aver::source::parse_source;
use aver::verify_law::expand::ExpansionMode;

const SRC: &str = r#"module ModuleBindings
    intent = "Verify must read module-level bindings the way run does."

base = 40
offset = double(base)

fn double(n: Int) -> Int
    ? "Twice n."
    n + n

fn f(x: Int) -> Int
    ? "Adds base."
    base + x

fn g(x: Int) -> Int
    ? "Adds offset."
    offset + x

verify f
    f(2) => 42
    f(0) => base

verify f law addsBase
    given x: Int = [0, 1, 2]
    f(x) => x + 40

verify g law domainFromBinding
    given x: Int = [base, 1]
    g(x) => x + 80

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(f(2)))
    Console.print(String.fromInt(g(1)))
"#;

/// `f(2)` is 42; before the fix the VM verify computed it as 4 and this
/// false case passed.
const SRC_FALSE_CASE: &str = r#"module ModuleBindingsFalse
    intent = "A false case over a module-level binding must fail."

base = 40

fn f(x: Int) -> Int
    ? "Adds base."
    base + x

verify f
    f(2) => 4
"#;

fn verify(source: &str, mode: ExpansionMode, parallel: bool) -> Vec<VerifyResult> {
    let items = parse_source(source).unwrap_or_else(|e| panic!("parse: {e:?}"));
    let file = "regression_vm_verify_module_bindings.av";
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
fn sequential_vm_verify_reads_module_bindings() {
    let results = verify(SRC, ExpansionMode::Declared, false);
    assert_eq!(totals(&results), (7, 0), "{:#?}", failures(&results));
}

#[test]
fn parallel_vm_verify_reads_module_bindings() {
    let results = verify(SRC, ExpansionMode::Declared, true);
    assert_eq!(totals(&results), (7, 0), "{:#?}", failures(&results));
}

#[test]
fn hostile_vm_verify_reads_module_bindings() {
    let results = verify(SRC, ExpansionMode::Hostile, false);
    let (_, failed) = totals(&results);
    assert_eq!(failed, 0, "{:#?}", failures(&results));
}

#[test]
fn false_case_over_module_binding_fails_with_the_run_value() {
    for parallel in [false, true] {
        let results = verify(SRC_FALSE_CASE, ExpansionMode::Declared, parallel);
        assert_eq!(totals(&results), (0, 1), "parallel = {parallel}");
        let (_, expected, actual) = &results[0].failures[0];
        assert_eq!((expected.as_str(), actual.as_str()), ("4", "42"));
    }
}

#[test]
fn run_agrees_with_verify_on_module_bindings() {
    let dir = tempfile::tempdir().expect("temp dir");
    let path = dir.path().join("module_bindings.av");
    std::fs::write(&path, SRC).expect("write source");
    let output = Command::new(env!("CARGO_BIN_EXE_aver"))
        .arg("run")
        .arg(&path)
        .output()
        .expect("run aver");
    assert!(
        output.status.success(),
        "aver run failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "42\n81\n");
}

fn failures(results: &[VerifyResult]) -> Vec<&(String, String, String)> {
    results.iter().flat_map(|r| &r.failures).collect()
}

/// The third backend. On main the wasm-gc emitter has no lowering for a
/// read of a module-level binding (it traps on `unreachable`); the
/// `fix/wasmgc-kernel-trap` branch inlines the bindings. Drop the `ignore`
/// and add this file to the wasm lane in ci.yml once that lands.
#[cfg(feature = "wasm")]
#[test]
#[ignore = "wasm-gc reads of module-level bindings land with fix/wasmgc-kernel-trap"]
fn wasm_gc_verify_agrees_with_vm_on_module_bindings() {
    let items = parse_source(SRC).unwrap_or_else(|e| panic!("parse: {e:?}"));
    let results = aver::diagnostics::wasm_gc_verify::run_verify_for_items_wasm_gc(
        items,
        None,
        None,
        "regression_vm_verify_module_bindings.av",
    )
    .unwrap_or_else(|e| panic!("wasm-gc verify: {e}"));
    assert_eq!(totals(&results), (7, 0), "{:#?}", failures(&results));
}

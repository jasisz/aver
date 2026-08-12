//! Regression — the VM half of the oracle call-index ordering invariant.
//!
//! The VM evaluates a call's arguments eagerly and only then takes its oracle
//! coordinates, so a generative call nested inside the arguments consumes the
//! LOWER index and the surrounding call the higher one. The lifted proof must
//! charge indices in that same order.
//!
//! Pre-fix the lifter claimed its own counter before lifting its arguments, so
//! `Random.int(1, Random.int(2, 6))` exported as `rnd path 0 1 (rnd path 1 2 6)`
//! while the run answered as if it were `rnd path 1 1 (rnd path 0 2 6)`. The
//! two never agreed, and the disagreement was fail-closed in both directions:
//! the law that held at runtime exported a theorem `native_decide` refutes, and
//! the law matching the export failed `verify`. A correct program was simply
//! unprovable.
//!
//! This test pins the runtime side. The exported side is pinned by
//! `generative_call_nested_in_args_gets_the_higher_oracle_index` in
//! src/codegen/lean/tests.rs — both halves have to move together or the two
//! interpreters diverge again.

use aver::diagnostics::vm_verify::run_verify_for_items_vm_with_mode;
use aver::source::parse_source;
use aver::verify_law::expand::ExpansionMode;

/// `indexStub` returns the oracle call index it was handed, which makes the
/// coordinate observable in the law's value. With arguments evaluated first the
/// inner read is charged 0 and the outer read 1, so `nested()` is 1.
const SRC: &str = r#"module CounterOrder
    intent = "Oracle indices must be charged in evaluation order."
    effects [Random]

fn indexStub(path: BranchPath, n: Int, min: Int, max: Int) -> Int
    ? "Returns the call index so the oracle coordinate is observable."
    n

fn nested() -> Int
    ? "Outer Random.int whose upper bound is itself a Random.int."
    ! [Random.int]
    Random.int(1, Random.int(2, 6))

verify nested law indexOrder
    given rnd: Random.int = [indexStub]
    nested() => 1
"#;

#[test]
fn vm_charges_the_nested_generative_call_the_lower_oracle_index() {
    let items = parse_source(SRC).unwrap_or_else(|e| panic!("parse failed: {e:?}"));
    let results = run_verify_for_items_vm_with_mode(
        items,
        None,
        Some(env!("CARGO_MANIFEST_DIR")),
        "regression_oracle_counter_order.av",
        ExpansionMode::Declared,
    )
    .expect("verify run");

    assert_eq!(results.len(), 1, "one verify block");
    let result = &results[0];
    assert_eq!(
        (result.passed, result.failed),
        (1, 0),
        "the outer generative call must observe index 1 — if this reports actual 0, the VM \
         started taking its oracle coordinates before evaluating arguments and the lifter's \
         ordering (src/types/checker/effect_lifting.rs, Generative arm) has to follow"
    );
}

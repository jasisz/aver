//! Regression — `BranchPath.Root` must survive a forked verify worker.
//!
//! `BranchPath.Root` is the one VM symbol-table value that is an arena
//! record rather than an immediate. The table travels with the compiled
//! program into every forked execution context — verify case workers and
//! independent-product branches both build theirs with
//! `build_parallel_base_context` — but nothing rebased it, so the fork kept
//! an index into the parent's arena and the first `BranchPath.child(path, _)`
//! on it read an entry that context does not have. `aver verify
//! examples/formal/oracle_independent_products.av` panicked with
//! `index out of bounds: the len is 0 but the index is 0`.
//!
//! Forking only happens with more than one case in the module, so the block
//! below carries two: a single-case module takes the sequential path and
//! never saw the bug.
//!
//! The VM-level half of this is
//! `parallel_base_context_rebases_the_branch_path_root_constant` in
//! src/vm/execute/tests.rs.

use std::path::PathBuf;

use aver::checker::VerifyResult;
use aver::diagnostics::vm_verify::run_verify_for_items_vm_parallel_with_mode_and_bindings;
use aver::source::parse_source;
use aver::verify_law::expand::ExpansionMode;

const SRC: &str = r#"module BranchPathRootFork
    intent = "BranchPath.Root must still be a BranchPath inside a verify worker."

fn childOf(path: BranchPath, idx: Int) -> Result<BranchPath, String>
    ? "Extend the given path by one branch."
    BranchPath.child(path, idx)

verify childOf
    childOf(BranchPath.Root, 0) => Result.Ok(BranchPath.child(BranchPath.Root, 0))
    childOf(BranchPath.Root, 1) => Result.Ok(BranchPath.child(BranchPath.Root, 1))
"#;

fn verify_parallel(source: &str, source_file: &str) -> Vec<VerifyResult> {
    let items = parse_source(source).unwrap_or_else(|e| panic!("parse {source_file}: {e:?}"));
    run_verify_for_items_vm_parallel_with_mode_and_bindings(
        items,
        None,
        Some(env!("CARGO_MANIFEST_DIR")),
        source_file,
        ExpansionMode::Declared,
        &[],
    )
    .unwrap_or_else(|e| panic!("verify {source_file}: {e}"))
}

fn totals(results: &[VerifyResult]) -> (usize, usize) {
    (
        results.iter().map(|result| result.passed).sum(),
        results.iter().map(|result| result.failed).sum(),
    )
}

#[test]
fn forked_verify_workers_keep_branch_path_root_readable() {
    let results = verify_parallel(SRC, "regression_branch_path_root_parallel_verify.av");
    assert_eq!(
        totals(&results),
        (2, 0),
        "both cases must read the root path — a panic here means the worker's symbol table \
         still names an arena entry that worker does not have"
    );
}

/// The example that reported this. Nothing else gates it: `ci.yml` runs
/// `aver check` over `examples/core` and `examples/data` only, and the
/// codegen suites that mention this file compile it rather than verify it.
#[test]
fn oracle_independent_products_example_verifies() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("examples/formal/oracle_independent_products.av");
    let source = std::fs::read_to_string(&path).expect("read oracle_independent_products.av");
    let results = verify_parallel(&source, path.to_str().expect("example path must be UTF-8"));
    let (passed, failed) = totals(&results);
    assert_eq!(
        (passed, failed),
        (14, 0),
        "examples/formal/oracle_independent_products.av must verify clean"
    );
}

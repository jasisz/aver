//! Regression for #213: `aver verify` on a multi-module program used
//! to fail with "missing VM symbol for exposed function
//! Refinement.Natural.Natural.fromInt" because `vm_verify` built the
//! entry's `SymbolTable` without dep modules — the resolver then
//! classified cross-module callsites as `Passthrough` and the VM
//! compiler had no interned symbol when wiring exports.
//!
//! Hits the same code path as `aver verify <file>` from the CLI,
//! against the canonical refinement-via-opaque flagship example.

use aver::diagnostics::vm_verify::run_verify_for_items_vm;
use aver::lexer::Lexer;
use aver::parser::Parser;

#[test]
fn vm_verify_multi_module_natural_app_passes_31_cases() {
    let path = "examples/refinement/natural_app.av";
    let source = std::fs::read_to_string(path).expect("read natural_app.av");

    let mut lexer = Lexer::new(&source);
    let tokens = lexer.tokenize().expect("tokenize");
    let mut parser = Parser::new(tokens);
    let items = parser.parse().expect("parse");

    // No pre-pipeline run — vm_verify runs tco/typecheck/resolve on
    // raw items itself (mirrors `aver verify` CLI path), and pre-
    // running buffer_build / interp_lower here would emit intrinsics
    // the verify pipeline then can't resolve.
    let results =
        run_verify_for_items_vm(items, None, Some("examples"), path).expect("verify succeeds");

    // natural_app.av has three verify blocks: safeSum (4 cases),
    // safeSum law commutative (25 cases), render (2 cases) = 31 total.
    let total_passed: usize = results.iter().map(|r| r.passed).sum();
    let total_failed: usize = results.iter().map(|r| r.failed).sum();
    assert!(
        total_passed >= 31,
        "expected at least 31 passing cases, got {total_passed}",
    );
    assert_eq!(total_failed, 0, "no cases should fail");
}

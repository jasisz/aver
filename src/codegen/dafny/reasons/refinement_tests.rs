//! The admission check is bound to the emitted source predicate, including
//! its dependencies. Checking a nominal annotation alone is not sufficient.
use crate::codegen::CodegenContext;
use crate::codegen::dafny::{reasons, tests::ctx_from_source};
use std::collections::HashSet;

const POSITIVE: &str = include_str!("../../../../tests/fixtures/dafny_structure/refinement.av");
const OPAQUE: &str =
    include_str!("../../../../tests/fixtures/dafny_structure/refinement_opaque.av");

fn emit(ctx: &CodegenContext) -> Result<String, String> {
    let blocks = reasons::local_blocks(ctx);
    let block = blocks[0];
    let crate::ast::VerifyKind::Law(law) = &block.kind else {
        panic!("expected law");
    };
    reasons::emit(block, law, ctx, &HashSet::new())
}

#[test]
fn matching_refinement_retains_its_nominal_binder() {
    let ctx = ctx_from_source(POSITIVE, "Refinement");
    assert_eq!(ctx.proof_ir.refined_types.len(), 1);
    let output = emit(&ctx).expect("checked source refinement");
    assert!(output.contains("value: Positive"), "{output}");
}

#[test]
fn a_changed_emitted_predicate_cannot_borrow_source_admission() {
    let mut ctx = ctx_from_source(POSITIVE, "Refinement");
    let decl = ctx
        .proof_ir
        .refined_types
        .values_mut()
        .next()
        .expect("refinement");
    decl.invariant.expr = crate::ast::Spanned::bare(crate::ir::hir::ResolvedExpr::Literal(
        crate::ast::Literal::Bool(true),
    ));
    assert!(
        emit(&ctx)
            .unwrap_err()
            .contains("refinement model disagrees with source")
    );
}

#[test]
fn a_bodyless_predicate_dependency_cannot_become_a_trusted_invariant() {
    let ctx = ctx_from_source(OPAQUE, "Refinement");
    assert_eq!(ctx.proof_ir.refined_types.len(), 1);
    let error = emit(&ctx).unwrap_err();
    assert!(
        error.contains("predicate") && error.contains("String.toLower"),
        "{error}"
    );
}

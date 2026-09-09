//! Render ProofIR's concrete applications in the existing fuel-induction step.
//! Statement admission and transitive axiom auditing stay with the Lean backend.

use crate::ast::{VerifyBlock, VerifyKind, VerifyLaw};
use crate::codegen::CodegenContext;

pub(super) fn ground(vb: &VerifyBlock, law: &VerifyLaw, ctx: &CodegenContext) -> Vec<String> {
    let Some(id) = ctx.law_target_fn_id(&vb.fn_name) else {
        return Vec::new();
    };
    let Some(plan) = ctx
        .proof_ir
        .law_theorems
        .iter()
        .find(|t| t.fn_id == id && t.law_name == law.name)
        .and_then(|t| t.induction.as_ref())
    else {
        return Vec::new();
    };
    let scope = ctx.active_module_scope();
    let mut out = Vec::new();
    // List projections have separate backend binder names. Until that mapping
    // is shared, use only steps whose arguments live in the theorem's scope.
    for step in plan.calls.iter().filter(|c| c.list_case.is_none()) {
        for application in &step.applications {
            for previous in super::shared::same_file_verify_blocks(ctx) {
                if previous.line == vb.line && previous.fn_name == vb.fn_name {
                    break;
                }
                let VerifyKind::Law(supplier) = &previous.kind else {
                    continue;
                };
                if supplier.name != application.law_name
                    || ctx
                        .symbol_table
                        .resolve_fn_id_in(&previous.fn_name, scope.as_deref())
                        != Some(application.fn_id)
                {
                    continue;
                }
                let Some((name, _)) =
                    crate::codegen::lean::toplevel::law_as_lemma_statement(previous, supplier, ctx)
                else {
                    continue;
                };
                let args = application
                    .arguments
                    .iter()
                    .map(|a| format!("({})", super::super::expr::emit_expr(a, ctx)))
                    .collect::<Vec<_>>()
                    .join(" ");
                let term = format!("{name} {args}");
                if !out.contains(&term) {
                    out.push(term);
                }
            }
        }
    }
    out
}

//! Stage 8c of #232: Lean emit for `ProofStrategy::MatchDispatcherFold`.
//!
//! Two `MatchDispatcherFold` fns over a `List<T>` param compute the
//! same result via slightly different structural recursions. The
//! proof closes by induction on `xs`: nil case unfolds both bodies,
//! cons case unfolds, applies the inductive hypothesis, and
//! discharges the arithmetic identity via `omega`.

use crate::ast::{VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;

use super::super::super::expr::aver_name_to_lean;
use super::super::AutoProof;

pub(in super::super) fn emit_match_dispatcher_fold_law(
    _vb: &VerifyBlock,
    _law: &VerifyLaw,
    _ctx: &CodegenContext,
    fold_fn: &str,
    spec_fn: &str,
) -> Option<AutoProof> {
    let fold_l = aver_name_to_lean(fold_fn);
    let spec_l = aver_name_to_lean(spec_fn);

    let proof_lines = vec![
        "  intro xs".to_string(),
        "  induction xs with".to_string(),
        format!("  | nil => simp [{fold_l}, {spec_l}]"),
        format!("  | cons h t ih => simp [{fold_l}, {spec_l}]; omega"),
    ];

    Some(AutoProof {
        support_lines: Vec::new(),
        proof_lines,
        replaces_theorem: false,
    })
}

//! Stage 8b of #232: Lean emit for `ProofStrategy::ResultPipelineChain`.
//!
//! The two chains (`?`-propagating and manual `match Result.Err -> Err`
//! nested) unfold to the same nested `match` tree. The proof closes
//! generically by unfolding every fn in the unfold list and then
//! repeatedly applying `split` to peel off each match layer; what
//! remains is structural equality which `simp_all` discharges.

use crate::ast::{VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;

use super::super::super::expr::aver_name_to_lean;
use super::super::AutoProof;

pub(in super::super) fn emit_result_pipeline_chain_law(
    _vb: &VerifyBlock,
    _law: &VerifyLaw,
    _ctx: &CodegenContext,
    chain_qm_fn: &str,
    chain_manual_fn: &str,
    step_fns: &[String],
) -> Option<AutoProof> {
    let qm_l = aver_name_to_lean(chain_qm_fn);
    let manual_l = aver_name_to_lean(chain_manual_fn);
    let mut unfold_list: Vec<String> = vec![qm_l, manual_l];
    for s in step_fns {
        unfold_list.push(aver_name_to_lean(s));
    }
    let unfolds = unfold_list.join(" ");

    let proof_lines = vec![
        "  intro n".to_string(),
        format!("  unfold {unfolds}"),
        "  repeat (first | split | rfl)".to_string(),
        "  all_goals simp_all".to_string(),
    ];

    Some(AutoProof {
        support_lines: Vec::new(),
        body: crate::codegen::lean::tactic_ir::Tactic::raw(proof_lines),
        replaces_theorem: false,
    })
}

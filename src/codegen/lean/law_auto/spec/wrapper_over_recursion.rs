//! Stage 8 of #232: Lean emit for `ProofStrategy::WrapperOverRecursion`.
//!
//! Mirror of the Dafny support-stack emit in `dafny::toplevel`. The IR
//! pin gives us `(wrapper_fn, inner_fn, other_fn, combine_op)` —
//! enough to render the accumulator-decomposition aux lemma plus the
//! main universal lemma. Both close in core Lean 4 (`omega`) without
//! a Mathlib dependency, matching the existing AverCommon shape.

use crate::ast::{BinOp, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;

use super::super::super::expr::aver_name_to_lean;
use super::super::AutoProof;

/// Render the Lean 4 support stack and main proof body for a
/// `WrapperOverRecursion` law. Returns `None` when the strategy
/// payload doesn't carry the data the template needs (defensive —
/// the lowerer should always provide it).
pub(in super::super) fn emit_wrapper_over_recursion_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    _ctx: &CodegenContext,
    wrapper_fn: &str,
    inner_fn: &str,
    other_fn: &str,
    combine_op: BinOp,
) -> Option<AutoProof> {
    let op = match combine_op {
        BinOp::Add => "+",
        BinOp::Mul => "*",
        BinOp::Sub => "-",
        _ => return None,
    };
    let neutral = match combine_op {
        BinOp::Mul => "1",
        _ => "0",
    };
    let wrapper_l = aver_name_to_lean(wrapper_fn);
    let inner_l = aver_name_to_lean(inner_fn);
    let other_l = aver_name_to_lean(other_fn);
    let acc_thm = format!("{inner_l}_acc");

    let support_lines = vec![
        format!(
            "theorem {acc_thm} (xs : List Int) (a : Int) : {inner_l} xs a = a {op} {inner_l} xs {neutral} := by"
        ),
        "  induction xs generalizing a with".to_string(),
        format!("  | nil => simp [{inner_l}]"),
        format!(
            "  | cons h t ih => simp only [{inner_l}]; rw [ih (a {op} h), ih ({neutral} {op} h)]; omega"
        ),
    ];

    // The toplevel renderer emits `theorem ... := by` and then extends
    // the proof_lines verbatim under it — so every line here needs
    // the two-space indent that puts it inside the `by` block.
    let proof_lines = vec![
        "  intro xs".to_string(),
        "  induction xs with".to_string(),
        format!("  | nil => simp [{wrapper_l}, {inner_l}, {other_l}]"),
        "  | cons h t ih =>".to_string(),
        format!("    simp only [{wrapper_l}, {inner_l}, {other_l}]"),
        format!("    rw [{acc_thm} t ({neutral} {op} h)]"),
        format!("    simp only [{wrapper_l}] at ih"),
        "    omega".to_string(),
    ];

    let _ = (vb, law);
    Some(AutoProof {
        support_lines,
        body: crate::codegen::lean::tactic_ir::Tactic::raw(proof_lines),
        replaces_theorem: false,
    })
}

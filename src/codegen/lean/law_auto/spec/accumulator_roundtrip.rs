//! Lean emit for accumulator-threaded encoder/inverse roundtrips.
//!
//! The lowerer has already checked the RLE-like source shape:
//! wrapper over a self-recursive loop, a step function that either
//! starts/increments/flushes a run accumulator, a finish function, and
//! an inverse that appends expanded runs. The proof here is the
//! mechanical helper stack a human would write: decode distributes
//! over append, repeat advances by one under `n >= 0`, the fold step
//! preserves decoded output, the step preserves `count >= 0`, then the
//! loop theorem generalizes over the accumulator.

use crate::ast::{Spanned, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;
use crate::ir::hir::ResolvedExpr;

use super::super::super::expr::{aver_name_to_lean, emit_expr};
use super::super::super::types::type_annotation_to_lean;
use super::super::AutoProof;

#[allow(clippy::too_many_arguments)]
pub(in super::super) fn emit_accumulator_roundtrip_law(
    _vb: &VerifyBlock,
    _law: &VerifyLaw,
    ctx: &CodegenContext,
    theorem_base: &str,
    wrapper_fn: &str,
    loop_fn: &str,
    step_fn: &str,
    finish_fn: &str,
    inverse_fn: &str,
    expand_fn: &str,
    repeat_fn: &str,
    acc_type: &str,
    run_type: &str,
    item_type: &str,
    current_field: &str,
    count_field: &str,
    initial_acc: &Spanned<ResolvedExpr>,
) -> Option<AutoProof> {
    let wrapper_l = aver_name_to_lean(wrapper_fn);
    let loop_l = aver_name_to_lean(loop_fn);
    let step_l = aver_name_to_lean(step_fn);
    let finish_l = aver_name_to_lean(finish_fn);
    let inverse_l = aver_name_to_lean(inverse_fn);
    let expand_l = aver_name_to_lean(expand_fn);
    let repeat_l = aver_name_to_lean(repeat_fn);
    let repeat_fuel_l = aver_name_to_lean(&crate::codegen::recursion::fuel_helper_name(repeat_fn));
    let acc_ty = type_annotation_to_lean(acc_type);
    let run_ty = type_annotation_to_lean(run_type);
    let item_ty = type_annotation_to_lean(item_type);
    let current_l = aver_name_to_lean(current_field);
    let count_l = aver_name_to_lean(count_field);
    let initial_acc_l = emit_expr(initial_acc, ctx);

    let nat_abs_succ = format!("{theorem_base}_natAbs_succ");
    let repeat_succ = format!("{theorem_base}_repeat_succ");
    let decode_append = format!("{theorem_base}_decode_append");
    let flush_fold_step = format!("{theorem_base}_flush_fold_step");
    let step_count_nonneg = format!("{theorem_base}_{step_l}_count_nonneg");
    let loop_gen = format!("{theorem_base}_{loop_l}_gen");

    let support_lines = vec![
        format!(
            "theorem {nat_abs_succ} (n : Int) (hn : 0 <= n) :\n    Int.natAbs (n + 1) = Int.natAbs n + 1 := by"
        ),
        "  apply Int.ofNat_inj.mp".to_string(),
        "  change (Int.natAbs (n + 1) : Int) = (Int.natAbs n : Int) + 1".to_string(),
        "  rw [Int.natAbs_of_nonneg (by omega), Int.natAbs_of_nonneg hn]".to_string(),
        String::new(),
        format!(
            "theorem {repeat_succ} (c : {item_ty}) (n : Int) (hn : 0 <= n) :\n    {repeat_l} c (n + 1) = {repeat_l} c n ++ [c] := by"
        ),
        format!("  unfold {repeat_l}"),
        format!("  rw [{nat_abs_succ} n hn]"),
        "  have hpos : ¬ n + 1 <= 0 := by omega".to_string(),
        format!("  simp [{repeat_fuel_l}, hpos]"),
        String::new(),
        format!(
            "theorem {decode_append} (a b : List {run_ty}) :\n    {inverse_l} (a ++ b) = {inverse_l} a ++ {inverse_l} b := by"
        ),
        "  induction a with".to_string(),
        "  | nil =>".to_string(),
        format!("      simp [{inverse_l}]"),
        "  | cons run rest ih =>".to_string(),
        format!("      simp [{inverse_l}, ih, List.append_assoc]"),
        String::new(),
        format!(
            "theorem {flush_fold_step} (acc : {acc_ty}) (c : {item_ty}) (hcount : 0 <= acc.{count_l}) :\n    {inverse_l} ({finish_l} ({step_l} acc c)) = {inverse_l} ({finish_l} acc) ++ [c] := by"
        ),
        format!("  by_cases hzero : acc.{count_l} = 0"),
        format!(
            "  · simp [{step_l}, {finish_l}, hzero, {decode_append}, {inverse_l}, {expand_l}, {repeat_l}, {repeat_fuel_l}]"
        ),
        format!("  · by_cases hsame : acc.{current_l} = c"),
        format!("    · have hsucc_ne : acc.{count_l} + 1 ≠ 0 := by omega"),
        format!(
            "      simp [{step_l}, {finish_l}, hzero, hsame, hsucc_ne, {decode_append}, {inverse_l}, {expand_l}, {repeat_succ}, hcount, List.append_assoc]"
        ),
        format!(
            "    · simp [{step_l}, {finish_l}, hzero, hsame, {decode_append}, {inverse_l}, {expand_l}, {repeat_l}, {repeat_fuel_l}, List.append_assoc]"
        ),
        String::new(),
        format!(
            "theorem {step_count_nonneg} (acc : {acc_ty}) (c : {item_ty}) (hcount : 0 <= acc.{count_l}) :\n    0 <= ({step_l} acc c).{count_l} := by"
        ),
        format!("  by_cases hzero : acc.{count_l} = 0"),
        format!("  · simp [{step_l}, hzero]"),
        format!("  · by_cases hsame : acc.{current_l} = c"),
        format!("    · simp [{step_l}, hzero, hsame]"),
        "      omega".to_string(),
        format!("    · simp [{step_l}, hzero, hsame]"),
        String::new(),
        format!(
            "theorem {loop_gen} (xs : List {item_ty}) (acc : {acc_ty}) (hcount : 0 <= acc.{count_l}) :\n    {inverse_l} ({loop_l} xs acc) = {inverse_l} ({finish_l} acc) ++ xs := by"
        ),
        "  induction xs generalizing acc with".to_string(),
        "  | nil =>".to_string(),
        format!("      simp [{loop_l}]"),
        "  | cons char rest ih =>".to_string(),
        format!("      simp [{loop_l}]"),
        format!("      rw [ih ({step_l} acc char) ({step_count_nonneg} acc char hcount)]"),
        format!("      rw [{flush_fold_step} acc char hcount]"),
        "      simp [List.append_assoc]".to_string(),
    ];

    let proof_lines = vec![
        "  intro xs".to_string(),
        format!("  simp [{wrapper_l}]"),
        format!("  simpa [{finish_l}, {inverse_l}] using {loop_gen} xs {initial_acc_l} (by simp)"),
    ];

    Some(AutoProof {
        support_lines,
        proof_lines,
        replaces_theorem: false,
    })
}

pub(in super::super) fn emit_string_roundtrip_via_list_law(
    string_encoder_fn: &str,
    string_decoder_fn: &str,
    list_encoder_fn: &str,
    list_law_name: &str,
) -> AutoProof {
    let string_encoder_l = aver_name_to_lean(string_encoder_fn);
    let string_decoder_l = aver_name_to_lean(string_decoder_fn);
    let list_theorem = format!(
        "{}_law_{}",
        aver_name_to_lean(list_encoder_fn),
        aver_name_to_lean(list_law_name)
    );

    AutoProof {
        support_lines: Vec::new(),
        proof_lines: vec![
            "  intro s".to_string(),
            format!(
                "  simp [{string_decoder_l}, {string_encoder_l}, {list_theorem}, String.intercalate_empty_chars]"
            ),
        ],
        replaces_theorem: false,
    }
}

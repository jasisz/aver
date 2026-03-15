use crate::ast::{VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;
use crate::codegen::lean::recurrence::{
    detect_second_order_int_linear_recurrence, detect_tailrec_int_linear_pair_worker,
    detect_tailrec_int_linear_pair_wrapper, fuel_helper_name, recurrence_nat_helper_name,
    render_affine_pair_expr,
};
use crate::verify_law::canonical_spec_ref;

use super::super::super::expr::{aver_name_to_lean, emit_expr};
use super::super::shared::find_fn_def;
use super::super::{AutoProof, indent_lines};

pub(super) fn emit_second_order_linear_recurrence_spec_equivalence_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    _intro_names: &[String],
) -> Option<AutoProof> {
    let spec_ref = canonical_spec_ref(&vb.fn_name, law, &ctx.fn_sigs)?;
    let impl_fd = find_fn_def(ctx, &vb.fn_name)?;
    let spec_fd = find_fn_def(ctx, &spec_ref.spec_fn_name)?;
    let impl_shape = detect_tailrec_int_linear_pair_wrapper(impl_fd)?;
    let spec_shape = detect_second_order_int_linear_recurrence(spec_fd)?;
    if emit_expr(&impl_shape.negative_branch, ctx) != emit_expr(&spec_shape.negative_branch, ctx)
        || emit_expr(&impl_shape.seed_prev, ctx) != emit_expr(&spec_shape.base0, ctx)
        || emit_expr(&impl_shape.seed_curr, ctx) != emit_expr(&spec_shape.base1, ctx)
    {
        return None;
    }

    let helper_fd = find_fn_def(ctx, &impl_shape.helper_fn_name)?;
    let helper_shape = detect_tailrec_int_linear_pair_worker(helper_fd)?;
    if helper_shape.recurrence != spec_shape.recurrence {
        return None;
    }

    let impl_lean = aver_name_to_lean(&vb.fn_name);
    let spec_lean = aver_name_to_lean(&spec_ref.spec_fn_name);
    let helper_lean = aver_name_to_lean(&impl_shape.helper_fn_name);
    let helper_fuel_lean = fuel_helper_name(&impl_shape.helper_fn_name);
    let spec_nat_lean = recurrence_nat_helper_name(&spec_ref.spec_fn_name);
    let worker_nat_lean = recurrence_nat_helper_name(&impl_shape.helper_fn_name);
    let theorem_base = format!("{impl_lean}_eq_{spec_lean}");
    let shift_theorem = format!("{theorem_base}__worker_nat_shift");
    let helper_nat_theorem = format!("{theorem_base}__helper_nat");
    let helper_seed_theorem = format!("{theorem_base}__helper_seed");
    let base0 = emit_expr(&spec_shape.base0, ctx);
    let base1 = emit_expr(&spec_shape.base1, ctx);
    let worker_step = render_affine_pair_expr(spec_shape.recurrence, "a", "b");

    let mut support_lines = Vec::new();
    support_lines.push(format!(
        "private def {} : Nat -> Int -> Int -> Int",
        worker_nat_lean
    ));
    support_lines.push("  | 0, a, _ => a".to_string());
    support_lines.push(format!(
        "  | n + 1, a, b => {} n b ({})",
        worker_nat_lean, worker_step
    ));
    support_lines.push(String::new());

    support_lines.push(format!(
        "private theorem {} : ∀ (k i : Nat), {} k ({} i) ({} (i + 1)) = {} (i + k) := by",
        shift_theorem, worker_nat_lean, spec_nat_lean, spec_nat_lean, spec_nat_lean
    ));
    support_lines.extend(indent_lines(
        vec![
            "intro k".to_string(),
            "induction k with".to_string(),
            "| zero =>".to_string(),
            "    intro i".to_string(),
            format!("    simp [{}]", worker_nat_lean),
            "| succ k ih =>".to_string(),
            "    intro i".to_string(),
            format!(
                "    simpa [{}, {}, Nat.succ_eq_add_one, Nat.add_assoc, Nat.add_left_comm, Nat.add_comm] using (ih (i + 1))",
                worker_nat_lean, spec_nat_lean
            ),
        ],
        2,
    ));
    support_lines.push(String::new());

    support_lines.push(format!(
        "private theorem {} : ∀ (k : Nat) (a b : Int), {} (Int.ofNat k) a b = {} k a b := by",
        helper_nat_theorem, helper_lean, worker_nat_lean
    ));
    support_lines.extend(indent_lines(
        vec![
            "intro k a b".to_string(),
            "induction k generalizing a b with".to_string(),
            "| zero =>".to_string(),
            format!(
                "    simp [{}, {}, {}]",
                helper_lean, helper_fuel_lean, worker_nat_lean
            ),
            "| succ k ih =>".to_string(),
            "    have hk : Int.ofNat (Nat.succ k) ≠ 0 := by".to_string(),
            "      simp [Nat.succ_eq_add_one]".to_string(),
            "      omega".to_string(),
            "    have hk' : Int.ofNat (Nat.succ k) - 1 = Int.ofNat k := by".to_string(),
            "      simpa [Nat.succ_eq_add_one]".to_string(),
            format!(
                "    simpa [{}, {}, {}, hk, hk'] using (ih b (a + b))",
                helper_lean, helper_fuel_lean, worker_nat_lean
            ),
        ],
        2,
    ));
    support_lines.push(String::new());

    support_lines.push(format!(
        "private theorem {} : ∀ (k : Nat), {} (Int.ofNat k) {} {} = {} k := by",
        helper_seed_theorem, helper_lean, base0, base1, spec_nat_lean
    ));
    support_lines.extend(indent_lines(
        vec![
            "intro k".to_string(),
            format!("rw [{} k {} {}]", helper_nat_theorem, base0, base1),
            format!(
                "simpa [{}, Nat.zero_add] using {} k 0",
                spec_nat_lean, shift_theorem
            ),
        ],
        2,
    ));
    support_lines.push(String::new());

    support_lines.push(format!(
        "theorem {} : ∀ (n : Int), {} n = {} n := by",
        theorem_base, impl_lean, spec_lean
    ));
    support_lines.extend(indent_lines(
        vec![
            "intro n".to_string(),
            "by_cases h_neg : n < 0".to_string(),
            "·".to_string(),
            format!("    simp [{}, {}, h_neg]", impl_lean, spec_lean),
            "·".to_string(),
            "    have h_nonneg : 0 <= n := by omega".to_string(),
            "    have h_cast : Int.ofNat n.toNat = n := by simpa using Int.toNat_of_nonneg h_nonneg"
                .to_string(),
            format!("    have h_seed := {} n.toNat", helper_seed_theorem),
            "    rw [h_cast] at h_seed".to_string(),
            format!("    simpa [{}, {}, h_neg] using h_seed", impl_lean, spec_lean),
        ],
        2,
    ));

    Some(AutoProof {
        support_lines,
        proof_lines: Vec::new(),
        replaces_theorem: true,
    })
}

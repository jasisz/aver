//! Stage 8 of #232: Lean emit for `ProofStrategy::WrapperOverRecursion`.
//!
//! Mirror of the Dafny support-stack emit in `dafny::toplevel`. The IR
//! pin gives us `(wrapper_fn, inner_fn, other_fn, combine_op, driver,
//! combine_fn)` — enough to render the accumulator-decomposition aux
//! lemma plus the main universal lemma.
//!
//! Two drivers:
//!   * `List` — the additive `sum_acc` shape. Closes in core Lean 4
//!     (`omega`) without a Mathlib dependency.
//!   * `PeanoNat` (multiplicative) — the `factTR` countdown. `omega`
//!     cannot discharge the nonlinear residual, so the step bridges the
//!     user monoid fn (`mul`) to `Nat.*` and closes with the core
//!     `Nat.mul_*` lemmas — still no Mathlib.

use crate::ast::{BinOp, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;
use crate::ir::WrapperDriver;

use super::super::super::expr::aver_name_to_lean;
use super::super::AutoProof;

/// Render the Lean 4 support stack and main proof body for a
/// `WrapperOverRecursion` law. Returns `None` when the strategy
/// payload doesn't carry the data the template needs (defensive —
/// the lowerer should always provide it).
#[allow(clippy::too_many_arguments)]
pub(in super::super) fn emit_wrapper_over_recursion_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    wrapper_fn: &str,
    inner_fn: &str,
    other_fn: &str,
    combine_op: BinOp,
    driver: &WrapperDriver,
    combine_fn: Option<&str>,
) -> Option<AutoProof> {
    let wrapper_l = aver_name_to_lean(wrapper_fn);
    let inner_l = aver_name_to_lean(inner_fn);
    let other_l = aver_name_to_lean(other_fn);

    match driver {
        WrapperDriver::List => emit_list_fold(&wrapper_l, &inner_l, &other_l, combine_op),
        WrapperDriver::PeanoNat { value_first, .. } => emit_peano_nat_fold(
            vb,
            law,
            ctx,
            &wrapper_l,
            &inner_l,
            &other_l,
            combine_op,
            combine_fn?,
            *value_first,
        ),
    }
}

/// Original `List<_>` fold (`sum_acc`): the accumulator-decomposition lemma
/// and the main proof both close with `omega` (linear). Hardcoded `List Int`.
fn emit_list_fold(
    wrapper_l: &str,
    inner_l: &str,
    other_l: &str,
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
    let acc_thm = format!("{inner_l}_acc");

    // The decomposition residual is linear for `+`/`-` (omega closes it) but
    // nonlinear for `*` — there the close is core-Lean Int multiplicative
    // associativity/commutativity/identity (no Mathlib). The combine op picks
    // the close independently of the `List` driver skeleton.
    let (acc_close, main_close) = match combine_op {
        BinOp::Mul => (
            "simp [Int.one_mul, Int.mul_one, Int.mul_assoc, Int.mul_comm, Int.mul_left_comm]",
            "simp [ih, Int.one_mul, Int.mul_one, Int.mul_assoc, Int.mul_comm, Int.mul_left_comm]",
        ),
        _ => ("omega", "omega"),
    };

    let support_lines = vec![
        format!(
            "theorem {acc_thm} (xs : List Int) (a : Int) : {inner_l} xs a = a {op} {inner_l} xs {neutral} := by"
        ),
        "  induction xs generalizing a with".to_string(),
        format!("  | nil => simp [{inner_l}]"),
        format!(
            "  | cons h t ih => simp only [{inner_l}]; rw [ih (a {op} h), ih ({neutral} {op} h)]; {acc_close}"
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
        format!("    {main_close}"),
    ];

    Some(AutoProof {
        support_lines,
        body: crate::codegen::lean::tactic_ir::Tactic::raw(proof_lines),
        replaces_theorem: false,
    })
}

/// Peano-`Nat` countdown fold (`factTR` / `triTR`). The decomposition lemma
/// `inner n acc = combine (inner n neutral) acc` is proved by `induction n
/// generalizing acc`; the main law unfolds the wrapper's neutral, peels one
/// step with the lemma, applies the IH, and closes the residual. The driver
/// fixes the `Nat` skeleton; the combine op fixes the close and the neutral:
/// multiplicative bridges the user monoid fn to `Nat.*` and closes with the
/// core `Nat.mul_*` lemmas (neutral `1`), additive bridges to `+` and closes
/// with `omega` (neutral `0`). Other ops decline to the generic ladder.
#[allow(clippy::too_many_arguments)]
fn emit_peano_nat_fold(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    wrapper_l: &str,
    inner_l: &str,
    other_l: &str,
    combine_op: BinOp,
    combine_fn: &str,
    value_first: bool,
) -> Option<AutoProof> {
    if !matches!(combine_op, BinOp::Add | BinOp::Mul) {
        return None;
    }
    let neutral = if combine_op == BinOp::Mul { "1" } else { "0" };
    let combine_l = aver_name_to_lean(combine_fn);
    let acc_thm = format!("{inner_l}_acc");
    let law_uid = format!(
        "{}_{}",
        aver_name_to_lean(&vb.fn_name),
        aver_name_to_lean(&law.name)
    );

    // Bridges (user monoid fn → `Nat` builtin) plus the simp set the nonlinear
    // close needs. The `mul` bridge proof rewrites with the `add` bridge, so
    // seed the scan with the combine fn's own callees (`plus`) — the law scan
    // only reaches one level deep and would miss it.
    let mut extra = std::collections::BTreeSet::new();
    if let Some(cfd) = ctx.fn_def_by_name(combine_fn, ctx.active_module_scope().as_deref()) {
        crate::codegen::proof_recognize::collect_called_fns_in_body(&cfd.body, &mut extra);
    }
    let (bridge_support, simp_extra, _bridged) =
        super::super::induction::lean_nat_lift_support(law, ctx, &law_uid, &extra);
    if simp_extra.is_empty() {
        // No Peano arith op recognized — decline rather than emit a dead proof.
        return None;
    }
    let simp_set = simp_extra.join(", ");

    // The decomposition + main residuals: a multiplicative close needs the core
    // `Nat.mul_*` lemmas on top of the bridges already in `simp_set`; an
    // additive residual is linear once `plus` is bridged to `+`, so `omega`
    // finishes it. The combine op picks the close independently of the driver.
    let (decomp_close, main_close) = match combine_op {
        BinOp::Mul => (
            format!("simp [{simp_set}]"),
            format!("simp [{simp_set}, Nat.mul_comm]"),
        ),
        _ => (
            format!("simp only [{simp_set}]; omega"),
            format!("simp only [{simp_set}]; omega"),
        ),
    };

    // Render the step term in the SAME arg order the source wrote, so the
    // rewrite matches the def's actual `combine(...)` subterm.
    let combine_apply = |value: &str, other: &str| -> String {
        if value_first {
            format!("{combine_l} {value} {other}")
        } else {
            format!("{combine_l} {other} {value}")
        }
    };
    let succ_acc = combine_apply("(m + 1)", "acc");
    let succ_one = combine_apply("(m + 1)", neutral);

    let mut support_lines = bridge_support;
    support_lines.push(format!(
        "theorem {acc_thm} : ∀ (n acc : Nat), {inner_l} n acc = {combine_l} ({inner_l} n {neutral}) acc := by"
    ));
    support_lines.push("  intro n acc".to_string());
    support_lines.push("  induction n generalizing acc with".to_string());
    support_lines.push(format!("  | zero => simp [{inner_l}, {simp_set}]"));
    support_lines.push(format!(
        "  | succ m ih => simp only [{inner_l}]; rw [ih ({succ_acc}), ih ({succ_one})]; {decomp_close}"
    ));

    // Main law: unfold the wrapper's neutral (`S(Z)` renders as `0 + 1`;
    // `Nat.zero_add` normalizes it to `1`), induct on the driver, peel one
    // step with the decomposition lemma, apply the IH, and commute/cancel the
    // nonlinear residual.
    let main_step = combine_apply("(m + 1)", neutral);
    let proof_lines = vec![
        "  intro n".to_string(),
        format!("  simp only [{wrapper_l}, Nat.zero_add]"),
        "  induction n with".to_string(),
        format!("  | zero => simp [{inner_l}, {other_l}]"),
        "  | succ m ih =>".to_string(),
        format!("    simp only [{inner_l}, {other_l}]"),
        format!("    rw [{acc_thm} m ({main_step})]"),
        "    rw [ih]".to_string(),
        format!("    {main_close}"),
    ];

    Some(AutoProof {
        support_lines,
        body: crate::codegen::lean::tactic_ir::Tactic::raw(proof_lines),
        replaces_theorem: false,
    })
}

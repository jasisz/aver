//! Lean renderer for the `NonlinearNonneg` strategy — nonnegativity / order
//! over a nonlinear Int product (`E >= 0`, or `prod <= prod`), the
//! Newton-Raphson error-bound family of `projects/k5_fdiv`.
//!
//! The proof is ONE generic decision step, never a per-figure template:
//! unfold the subject's `Bool` body, bridge the Bool comparison to the
//! corresponding Prop (`0 ≤ E` / `L ≤ R`) and split a conjunctive `when`
//! guard into atomic hypotheses, then hand the goal to the shipped prelude
//! tactic `aver_int_order` — the nonlinear analog of `omega` for the
//! products-and-squares fragment (recurse with `Int.mul_nonneg` /
//! `Int.mul_le_mul`, bottom squares out on `aver_sq_nonneg`, discharge the
//! premise leaves). The whole bridge-and-close sits inside a
//! `first | (…) | sorry` portfolio, so a goal outside the fragment falls to
//! an honest caught `sorry` — the `#print axioms` whitelist keeps credit
//! fail-closed. The bounded `_checked_domain` / sample theorems the caller
//! still emits give the same runtime evidence the sampled fallback did.

use super::AutoProof;
use super::aver_name_to_lean;
use crate::ast::{VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;

pub(super) fn emit_nonlinear_nonneg_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Option<AutoProof> {
    let Some(crate::ir::ProofStrategy::NonlinearNonneg { unfold_fns }) =
        super::law_strategy_for(ctx, &vb.fn_name, &law.name)
    else {
        return None;
    };
    // Unfold cone (subject first) → Lean names. Unfolding the subject's
    // `Bool` body is what exposes the `E >= 0` comparison the bridge then
    // lowers to the Prop `0 ≤ E`.
    let cone: Vec<String> = unfold_fns.iter().map(|f| aver_name_to_lean(f)).collect();

    // Bridge simp set: the cone defs, then the Bool→Prop bridges that lower
    // the comparison to a Prop the generic tactic reasons over —
    // `decide_eq_true_eq` strips the `decide … = true` Bool wrapper a
    // `holds` claim lowers to, `ge_iff_le` rewrites `E ≥ 0` to `0 ≤ E`, and
    // `gt_iff_lt` rewrites a `>` guard (`s > 0`) to `0 < s` so `omega` can
    // read it. For a premised law, `Bool.and_eq_true` first splits the
    // conjunctive guard into the atomic facts the tactic reads off context.
    let mut bridge = cone;
    if law.when.is_some() {
        bridge.push("Bool.and_eq_true".to_string());
    }
    bridge.push("ge_iff_le".to_string());
    bridge.push("gt_iff_lt".to_string());
    bridge.push("decide_eq_true_eq".to_string());
    let simp_set = bridge.join(", ");

    let givens: Vec<String> = law
        .givens
        .iter()
        .map(|g| aver_name_to_lean(&g.name))
        .collect();

    // `intro <givens> [h_when]; first | (simp only [<bridge>] [at h_when] ⊢
    //  <;> aver_int_order) | sorry`. The bridge AND the closer sit inside the
    // `first` arm so an unexpected shape (bridge makes no progress, or a leaf
    // the fragment can't reach) falls to the honest `sorry` floor — never an
    // "unsolved goals" build error. `<;>` (not `;`) keeps a bridge that fully
    // closes the goal from leaving `aver_int_order` running on no goals.
    let (intro_names, branch) = if law.when.is_some() {
        let mut names = givens;
        // Naming contract: the guard MUST be introduced as `h_when`. The
        // prelude's conjunction-split arm in `aver_int_order` reads it by that
        // exact name (`And.left h_when` / `And.right h_when`) and peels one
        // level only; rename it and that arm silently no-ops (see prelude.rs).
        names.push("h_when".to_string());
        (
            names,
            format!("simp only [{simp_set}] at h_when ⊢ <;> aver_int_order"),
        )
    } else {
        (givens, format!("simp only [{simp_set}] <;> aver_int_order"))
    };

    Some(AutoProof {
        support_lines: Vec::new(),
        body: super::intro_then_first(&intro_names, vec![branch]),
        replaces_theorem: false,
    })
}

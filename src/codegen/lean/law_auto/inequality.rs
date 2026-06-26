//! Lean renderer for the `NonlinearNonneg` strategy — nonnegativity over
//! a nonlinear Int product (`E >= 0`), the Newton-Raphson error-bound
//! family of `projects/k5_fdiv`.
//!
//! The proof is ONE generic decision step, never a per-figure template:
//! unfold the subject's `Bool` body, bridge the Bool comparison to the
//! Prop `0 ≤ E` (and split a conjunctive `when` guard into atomic
//! hypotheses), then hand the goal to the shipped prelude tactic
//! `aver_int_nonneg` — the nonlinear analog of `omega` for the
//! products-and-squares fragment (decompose with `Int.mul_nonneg`, bottom
//! squares out on `aver_sq_nonneg`, discharge the premise leaves). The
//! whole bridge-and-close sits inside a `first | (…) | sorry` portfolio,
//! so a goal outside the fragment falls to an honest caught `sorry` — the
//! `#print axioms` whitelist keeps credit fail-closed. The bounded
//! `_checked_domain` / sample theorems the caller still emits give the
//! same runtime evidence the sampled fallback did.

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

    // Bridge simp set: the cone defs, then the Bool→Prop bridges
    // (`ge_iff_le` rewrites `E ≥ 0` to `0 ≤ E`; `decide_eq_true_eq` strips
    // the `decide … = true` Bool wrapper a `holds` claim lowers to). For a
    // premised law, `Bool.and_eq_true` first splits the conjunctive guard
    // (`(a≥0 && b≥0) = true`) into the atomic Prop facts the generic tactic
    // reads off the context.
    let mut bridge = cone;
    if law.when.is_some() {
        bridge.push("Bool.and_eq_true".to_string());
    }
    bridge.push("ge_iff_le".to_string());
    bridge.push("decide_eq_true_eq".to_string());
    let simp_set = bridge.join(", ");

    let givens: Vec<String> = law
        .givens
        .iter()
        .map(|g| aver_name_to_lean(&g.name))
        .collect();

    // `intro <givens> [h_when]; first | (simp only [<bridge>] [at h_when] ⊢
    //  <;> aver_int_nonneg) | sorry`. The bridge AND the closer sit inside the
    // `first` arm so an unexpected shape (bridge makes no progress, or a leaf
    // the fragment can't reach) falls to the honest `sorry` floor — never an
    // "unsolved goals" build error. `<;>` (not `;`) keeps a bridge that fully
    // closes the goal from leaving `aver_int_nonneg` running on no goals.
    let (intro_names, branch) = if law.when.is_some() {
        let mut names = givens;
        names.push("h_when".to_string());
        (
            names,
            format!("simp only [{simp_set}] at h_when ⊢ <;> aver_int_nonneg"),
        )
    } else {
        (givens, format!("simp only [{simp_set}] <;> aver_int_nonneg"))
    };

    Some(AutoProof {
        support_lines: Vec::new(),
        body: super::intro_then_first(&intro_names, vec![branch]),
        replaces_theorem: false,
    })
}

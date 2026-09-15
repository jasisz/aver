//! Compose cited transition laws over arbitrary lists. Every candidate is a
//! kernel-checked proof attempt; neither samples nor function names select a
//! trusted result. Guards and all other inputs remain in the induction motive.

use crate::ast::VerifyLaw;
use crate::codegen::lean::expr::aver_name_to_lean;

pub(super) fn candidates(
    law: &VerifyLaw,
    definitions: &super::induction::Definitions,
    hypotheses: &str,
    fact_count: usize,
) -> Vec<String> {
    if definitions.list_steps.is_empty() || fact_count == 0 {
        return Vec::new();
    }
    let lists = law
        .givens
        .iter()
        .filter(|g| g.type_name.starts_with("List<"));
    let excluded = (0..fact_count)
        .map(|i| format!("-_fact{i}"))
        .collect::<Vec<_>>()
        .join(", ");
    let mut candidates = Vec::new();
    for driver in lists.take(2) {
        for generalize_lists in [false, true] {
            let others = law
                .givens
                .iter()
                .filter(|g| {
                    g.name != driver.name && (generalize_lists || !g.type_name.starts_with("List<"))
                })
                .map(|g| aver_name_to_lean(&g.name))
                .collect::<Vec<_>>()
                .join(" ");
            let generalizing = if others.is_empty() {
                String::new()
            } else {
                format!(" generalizing {others}")
            };
            let revert = if hypotheses.trim().is_empty() {
                String::new()
            } else {
                format!("revert{hypotheses}; ")
            };
            let candidate = format!(
                "({revert}induction {}{generalizing} <;> intros <;> simp only [{}, Bool.and_eq_true, Bool.or_eq_true, beq_iff_eq, decide_eq_true_eq, ge_iff_le, gt_iff_lt, List.contains_cons, List.contains_nil, List.contains_eq_mem, List.mem_cons, List.not_mem_nil, List.length_cons, List.length_nil] at * <;> (try simp_all only [{excluded}]) <;> with_reducible grind only)",
                aver_name_to_lean(&driver.name),
                definitions.list_steps
            );
            if !candidates.contains(&candidate) {
                candidates.push(candidate);
            }
        }
    }
    candidates
}

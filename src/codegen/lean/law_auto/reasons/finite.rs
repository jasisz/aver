//! Compose finite observations around opaque recursive summaries. Imported
//! citations can name wrappers absent from the current expression's call cone;
//! expose those interfaces before asking the solver to apply their equations.
use super::induction::{self, Definitions};
use crate::ast::VerifyLaw;
use crate::codegen::{CodegenContext, common};
use std::collections::BTreeSet;

pub(super) fn candidate(
    law: &VerifyLaw,
    ctx: &CodegenContext,
    definitions: &Definitions,
    fact_count: usize,
) -> Option<String> {
    if fact_count == 0 || definitions.unary_list_maps.is_empty() {
        return None;
    }
    let scope = ctx.active_module_scope();
    let mut finite: BTreeSet<String> = definitions
        .simp
        .split(", ")
        .filter(|name| !name.is_empty())
        .map(str::to_string)
        .collect();
    let mut equations = BTreeSet::new();
    for citation in law.using.as_ref()? {
        let (function, label) = citation.rsplit_once('.')?;
        let id = ctx
            .symbol_table
            .resolve_fn_id_in(function, scope.as_deref())?;
        let theorem = ctx
            .proof_ir
            .law_theorems
            .iter()
            .find(|theorem| theorem.fn_id == id && theorem.law_name == label)?;
        for id in &theorem.function_cone {
            let key = &ctx.symbol_table.fn_entry(*id).key;
            let fd = ctx.fn_def_by_name(&key.name, key.scope_str())?;
            if definitions
                .unary_list_maps
                .contains(&induction::lean_name(fd, ctx))
            {
                if let Some(equation) = induction::map_constructor_equations(fd, ctx) {
                    equations.insert(equation);
                }
            }
            if fd.effects.is_empty()
                && common::fn_id_for_decl(ctx, fd)
                    .is_some_and(|id| !ctx.recursive_fns.contains(&id))
            {
                let name = induction::lean_name(fd, ctx);
                // Small element conversions must also reduce when E-matching
                // instantiates a map on a newly discovered concrete prefix.
                if !fd.return_type.starts_with("List<") && fd.params.len() == 1 {
                    equations.insert(format!("= {name}.eq_def"));
                }
                finite.insert(name);
            }
        }
    }
    let equations = equations.into_iter().collect::<Vec<_>>().join(", ");
    let finite = finite.into_iter().collect::<Vec<_>>().join(", ");
    let excluded = (0..fact_count)
        .map(|index| format!("-_fact{index}"))
        .collect::<Vec<_>>()
        .join(", ");
    let lemmas = induction::checked_map_lemmas(&definitions.unary_list_maps);
    Some(format!(
        "(simp only [Bool.and_eq_true, decide_eq_true_eq, beq_iff_eq, {finite}] at *; all_goals (repeat' first | (simp_all +zetaDelta [{finite}, {}, {excluded}]) | split); all_goals ({lemmas}grind [List.drop_cons, List.drop_drop, List.length_drop, List.append_assoc, {equations}]); done)",
        definitions.list_maps,
    ))
}

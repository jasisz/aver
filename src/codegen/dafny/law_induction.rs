//! Render the shared source-recursion instances. Dafny checks the original law
//! premises at every recursive call and verifies the selected strict decrease.

use crate::ast::{VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;
use crate::ir::proof_ir::{LawInduction, LawInductionMeasure};

use super::expr::{aver_name_to_dafny, emit_expr};

/// Checked sequence identities give the SMT solver ground-independent rewrite
/// facts. In particular, recursive calls with `[x] + []` must match calls with
/// `[x]`; unfolding the worker alone does not reliably expose this equality.
/// A consumer matching the first element also needs append reassociated under
/// that cons. Explicitly checking the identity exposes the suffix term at which
/// a cited payload law applies; it does not assume anything about the payload.
pub(super) fn sequence_identities(law: &VerifyLaw, ctx: &CodegenContext) -> Vec<String> {
    let mut elements = std::collections::BTreeSet::new();
    let scope = ctx.active_module_scope();
    for expr in [&law.lhs, &law.rhs].into_iter().chain(law.because.iter()) {
        crate::codegen::expr_walk::walk(expr, &mut |e| {
            if let Some(crate::ast::Type::List(element)) = e.ty()
                && crate::types::checker::type_is_fully_concrete(element)
            {
                elements.insert(super::toplevel::type_to_dafny_in_scope(
                    element,
                    scope.as_deref(),
                ));
            }
        });
    }
    elements
        .into_iter()
        .flat_map(|element| {
            [
                format!("  forall xs: seq<{element}> ensures xs + [] == xs && [] + xs == xs {{ }}"),
                format!("  forall x: {element} ensures ListReverse([x]) == [x] {{ }}"),
                format!("  forall head: {element}, xs: seq<{element}>, ys: seq<{element}> ensures ([head] + xs) + ys == [head] + (xs + ys) {{ }}"),
            ]
        })
        .collect()
}

pub(super) fn plan<'a>(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &'a CodegenContext,
) -> Option<&'a LawInduction> {
    let scope = ctx.active_module_scope();
    let id = ctx
        .symbol_table
        .resolve_fn_id_in(&vb.fn_name, scope.as_deref())?;
    ctx.proof_ir
        .law_theorems
        .iter()
        .find(|t| t.fn_id == id && t.law_name == law.name)?
        .induction
        .as_ref()
}

pub(super) fn measure(plan: &LawInduction) -> String {
    let name = aver_name_to_dafny(&plan.driver);
    match plan.measure {
        LawInductionMeasure::SequenceLength => format!("|{name}|"),
        LawInductionMeasure::NonnegativeInt => format!("if {name} >= 0 then {name} else 0"),
    }
}

pub(super) fn calls(
    plan: &LawInduction,
    name: &str,
    cites: &[(String, &VerifyLaw)],
    ctx: &CodegenContext,
    recursion: &super::toplevel::LawRecursion<'_>,
) -> Vec<String> {
    let mut lines = Vec::new();
    for call in &plan.calls {
        lines.push(format!("  if {} {{", emit_expr(&call.guard, ctx)));
        if let Some(case) = &call.list_case {
            let list = emit_expr(&case.list, ctx);
            if let Some(head) = &case.head {
                lines.push(format!(
                    "    var {} := ({list})[0];",
                    aver_name_to_dafny(head)
                ));
            }
            if let Some(tail) = &case.tail {
                lines.push(format!(
                    "    var {} := ({list})[1..];",
                    aver_name_to_dafny(tail)
                ));
            }
            // Expose the exact nil/cons equation used by source recursion.
            lines.push(format!(
                "    assert {list} == [({list})[0]] + ({list})[1..];"
            ));
            if let Some(ty) = case.list.ty() {
                let ty = super::toplevel::type_to_dafny_in_scope(
                    ty,
                    ctx.active_module_scope().as_deref(),
                );
                let mut suffix = "averInductionSuffix".to_string();
                while list.contains(&suffix) {
                    suffix.push('_');
                }
                // Expose the cons equation underneath append too. The target
                // checker proves this sequence identity for every suffix.
                lines.push(format!("    forall {suffix}: {ty} ensures {list} + {suffix} == [({list})[0]] + (({list})[1..] + {suffix}) {{ }}"));
            }
        }
        let args = call
            .arguments
            .iter()
            .map(|a| emit_expr(a, ctx))
            .collect::<Vec<_>>()
            .join(", ");
        if let Some(premise) = &call.premise {
            lines.push(format!("    if {} {{", emit_expr(premise, ctx)));
            lines.push(format!("      {name}({args});"));
            lines.push("    }".to_string());
        } else {
            lines.push(format!("    {name}({args});"));
        }
        for application in &call.applications {
            let key = &ctx.symbol_table.fn_entry(application.fn_id).key;
            let lemma = format!(
                "{}_{}",
                super::expr::function_name(application.fn_id, ctx),
                aver_name_to_dafny(&application.law_name)
            );
            // Shared search never decides whether Dafny emitted a universal
            // supplier. Reuse the same admission gate as the forall hoist.
            let imported = key
                .scope_str()
                .filter(|owner| Some(*owner) != ctx.active_module_scope().as_deref())
                .and_then(|owner| ctx.modules.iter().find(|m| m.prefix == owner))
                .is_some_and(|module| {
                    module.verify_laws.iter().any(|vb| {
                        let crate::ast::VerifyKind::Law(law) = &vb.kind else {
                            return false;
                        };
                        law.name == application.law_name
                            && ctx
                                .symbol_table
                                .resolve_fn_id_in(&vb.fn_name, Some(&module.prefix))
                                == Some(application.fn_id)
                            && ctx.with_module_scope(Some(&module.prefix), || {
                                super::law_search::reusable_ordinary_law(vb, law, ctx, recursion)
                            })
                    })
                });
            if cites.iter().any(|(name, _)| name == &lemma) || imported {
                let args = application
                    .arguments
                    .iter()
                    .map(|a| emit_expr(a, ctx))
                    .collect::<Vec<_>>()
                    .join(", ");
                lines.push(format!("    {lemma}({args});"));
            }
        }
        lines.push("  }".to_string());
    }
    lines
}

//! First-order Dafny support for source-authored proof steps. Every step is a
//! checked lemma; only the parent chains them into the original guarded law.
//! This pilot has no assumptions/axiom fallback and makes no kernel-audit claim.

use crate::ast::{Expr, Spanned, TopLevel, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;

use super::expr::{aver_name_to_dafny, emit_expr};
use super::toplevel::{emit_type_in_scope, resolve_rewrite_output};

mod arithmetic;
mod citations;
mod induction;
mod subset;
#[cfg(test)]
mod tests;

fn label(vb: &VerifyBlock, law: &VerifyLaw) -> String {
    format!("{}.{}", vb.fn_name, law.name)
}

fn lemma_name(id: &str) -> String {
    // Hex preserves the complete source identity, including punctuation, and
    // cannot merge f_a.b with f.a_b as underscore concatenation would.
    let encoded: String = id.bytes().map(|byte| format!("{byte:02x}")).collect();
    format!("averGuided_{encoded}")
}

fn local_blocks(ctx: &CodegenContext) -> Vec<&VerifyBlock> {
    match ctx.active_module_scope().as_deref() {
        Some(scope) => ctx
            .modules
            .iter()
            .find(|m| m.prefix == scope)
            .map(|m| m.verify_blocks.iter().collect())
            .unwrap_or_default(),
        None => ctx
            .items
            .iter()
            .filter_map(|item| match item {
                TopLevel::Verify(block) => Some(block),
                _ => None,
            })
            .collect(),
    }
}

fn binders(law: &VerifyLaw, ctx: &CodegenContext) -> String {
    law.givens
        .iter()
        .map(|g| {
            format!(
                "{}: {}",
                aver_name_to_dafny(&g.name),
                emit_type_in_scope(&g.type_name, ctx.active_module_scope().as_deref())
            )
        })
        .collect::<Vec<_>>()
        .join(", ")
}

fn arguments(law: &VerifyLaw) -> String {
    law.givens
        .iter()
        .map(|g| aver_name_to_dafny(&g.name))
        .collect::<Vec<_>>()
        .join(", ")
}

fn expression(expr: &crate::ast::Spanned<crate::ast::Expr>, ctx: &CodegenContext) -> String {
    emit_expr(&resolve_rewrite_output(expr, ctx), ctx)
}

fn conclusion(law: &VerifyLaw, ctx: &CodegenContext) -> String {
    let left = resolve_rewrite_output(&law.lhs, ctx);
    let right = resolve_rewrite_output(&law.rhs, ctx);
    equality(&left, &right, ctx)
}

fn equality(
    left: &crate::ast::Spanned<crate::ir::hir::ResolvedExpr>,
    right: &crate::ast::Spanned<crate::ir::hir::ResolvedExpr>,
    ctx: &CodegenContext,
) -> String {
    let render = |expr, expected| match expected {
        Some(ty) => super::expr::emit_expr_with_expected(expr, ctx, ty),
        None => emit_expr(expr, ctx),
    };
    format!(
        "({}) == ({})",
        render(left, right.ty()),
        render(right, left.ty())
    )
}

/// Choose a source given driving a direct recursive Bool predicate obligation.
/// Equalities between recursive Int values often only need unfolding; adding
/// induction there can create untriggerable hypotheses. Dafny checks every
/// selected induction and its decreases, without adding a source premise.
struct Induction {
    variables: String,
    decreases: String,
}

fn induction_driver(
    expressions: &[&Spanned<Expr>],
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Option<Induction> {
    fn collect(
        expr: &Spanned<Expr>,
        law: &VerifyLaw,
        ctx: &CodegenContext,
        drivers: &mut std::collections::BTreeSet<(String, bool)>,
    ) {
        if let Expr::FnCall(callee, args) = &expr.node {
            let name = crate::checker::expr_to_str(callee);
            let scope = ctx.active_module_scope();
            if let Some(id) = ctx.symbol_table.resolve_fn_id_in(&name, scope.as_deref()) {
                let key = &ctx.symbol_table.fn_entry(id).key;
                if let Some(fd) = ctx.fn_def_by_name(&key.name, key.scope_str())
                    && fd.return_type == "Bool"
                    && let Some((index, list)) = subset::list_parameter(fd, ctx)
                        .map(|index| (index, true))
                        .or_else(|| {
                            subset::countdown_parameter(fd, ctx)
                                .or_else(|| arithmetic::quotient_parameter(fd, ctx))
                                .map(|index| (index, false))
                        })
                    && let Some(arg) = args.get(index)
                    && let Expr::Ident(name) | Expr::Resolved { name, .. } = &arg.node
                    && law.givens.iter().any(|g| {
                        g.name == *name
                            && if list {
                                g.type_name.starts_with("List<")
                            } else {
                                g.type_name == "Int"
                            }
                    })
                {
                    drivers.insert((aver_name_to_dafny(name), list));
                }
            }
        }
    }
    let mut drivers = std::collections::BTreeSet::new();
    for expr in expressions {
        collect(expr, law, ctx, &mut drivers);
    }
    if drivers.len() != 1 {
        return None;
    }
    let (driver, list) = drivers.pop_first().expect("one induction driver");
    Some(if list {
        // A fold may advance its accumulators as the list shrinks. Generalize
        // every given, but decrease only the checked list length. Dafny proves
        // the induction; no recursive hypothesis is added as a precondition.
        Induction {
            variables: arguments(law),
            decreases: format!("|{driver}|"),
        }
    } else {
        Induction {
            variables: arguments(law),
            decreases: format!("if {driver} >= 0 then {driver} else 0"),
        }
    })
}

/// Return an explicit decline reason for every shape outside the pilot. This
/// validates the complete selected dependency closure before emitting anything.
pub(super) fn emit(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    native_members: &std::collections::HashSet<crate::ir::FnId>,
) -> Result<String, String> {
    let blocks = local_blocks(ctx);
    let citations = subset::validate(vb, law, ctx, &blocks, native_members)?;
    let id = label(vb, law);
    let name = lemma_name(&id);
    let source_id = match ctx.active_module_scope() {
        Some(scope) => format!("{scope}.{id}"),
        None => id,
    };
    let params = binders(law, ctx);
    let args = arguments(law);
    let guard = law.when.as_ref().map(|e| expression(e, ctx));
    let reasons: Vec<_> = law.because.iter().map(|e| expression(e, ctx)).collect();
    let goal = conclusion(law, ctx);
    let mut out = Vec::new();
    for &citation in &citations {
        if let Some(supplier) = citations::plain_supplier(citation, &name, law, ctx)? {
            out.push(supplier);
        }
    }
    for index in 0..=reasons.len() {
        let step = if index == reasons.len() {
            "implication".to_string()
        } else {
            format!("because{}", index + 1)
        };
        let step_name = format!("{name}_{step}");
        let step_goal = reasons.get(index).unwrap_or(&goal);
        let source_expressions = match law.because.get(index) {
            Some(reason) => vec![reason],
            None => vec![&law.lhs, &law.rhs],
        };
        let driver = induction_driver(&source_expressions, law, ctx);
        let has_induction = driver.is_some();
        let list_induction = driver
            .as_ref()
            .is_some_and(|driver| driver.decreases.starts_with("|"));
        out.push(format!(
            "// aver:dafny-obligation {step_name} {source_id}.{step}"
        ));
        out.push(format!(
            "lemma {{:induction {}}} {step_name}({params})",
            driver
                .as_ref()
                .map(|driver| driver.variables.as_str())
                .unwrap_or("false")
        ));
        if let Some(guard) = &guard {
            out.push(format!("  requires {guard}"));
        }
        for previous in reasons.iter().take(index) {
            out.push(format!("  requires {previous}"));
        }
        out.push(format!("  ensures {step_goal}"));
        if let Some(driver) = driver {
            out.push(format!("  decreases {}", driver.decreases));
        }
        out.push("{".to_string());
        out.extend(super::law_induction::sequence_identities(law, ctx));
        for &citation in &citations {
            let dependency = citation.law;
            let cited_name = citations::supplier_name(citation, &name, ctx);
            // The forall range retains the supplier's guard. Calling its
            // lemma inside that range must prove every precondition; no
            // required fact is asserted unconditionally in the consumer.
            let range = dependency
                .when
                .as_ref()
                .map(|e| format!(" | {}", citations::expression(e, citation.scope, ctx)))
                .unwrap_or_default();
            out.push(format!(
                "  forall {}{range} ensures {} {{",
                citations::binders(citation, ctx)?,
                citations::conclusion(citation, ctx)
            ));
            out.push(format!("    {cited_name}({});", arguments(dependency)));
            out.push("  }".to_string());
        }
        if list_induction {
            for expr in &source_expressions {
                if let Some(calls) = induction::emit_list_calls(expr, law, &step_name, ctx) {
                    out.extend(calls);
                }
            }
        }
        if has_induction {
            for expr in &source_expressions {
                if let Some(calls) = arithmetic::emit_quotient_calls(expr, law, &step_name, ctx) {
                    out.extend(calls);
                }
            }
        }
        out.push(format!("  assert {step_goal};"));
        out.push("}".to_string());
    }
    out.push(format!("// aver:dafny-law {name} {source_id}"));
    out.push(format!("lemma {{:induction false}} {name}({params})"));
    if let Some(guard) = &guard {
        out.push(format!("  requires {guard}"));
    }
    out.push(format!("  ensures {goal}"));
    out.push("{".to_string());
    for index in 0..reasons.len() {
        out.push(format!("  {name}_because{}({args});", index + 1));
    }
    out.push(format!("  {name}_implication({args});"));
    out.push("}".to_string());
    Ok(out.join("\n"))
}

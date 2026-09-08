//! Bounded Dafny support for source-authored proof steps. Every step is a
//! checked lemma; only the parent chains them into the original guarded law.
//! This pilot has no assumptions/axiom fallback and makes no kernel-audit claim.

use crate::ast::{TopLevel, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;

use super::expr::{aver_name_to_dafny, emit_expr};
use super::toplevel::{emit_type, resolve_rewrite_output};

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

fn binders(law: &VerifyLaw) -> String {
    law.givens
        .iter()
        .map(|g| {
            format!(
                "{}: {}",
                aver_name_to_dafny(&g.name),
                emit_type(&g.type_name)
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
    format!(
        "({}) == ({})",
        expression(&law.lhs, ctx),
        expression(&law.rhs, ctx)
    )
}

/// Return an explicit decline reason for every shape outside the pilot. This
/// validates the complete selected dependency closure before emitting anything.
pub(super) fn emit(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Result<String, String> {
    let blocks = local_blocks(ctx);
    let citations = subset::validate(vb, law, ctx, &blocks)?;
    let id = label(vb, law);
    let name = lemma_name(&id);
    let source_id = match ctx.active_module_scope() {
        Some(scope) => format!("{scope}.{id}"),
        None => id,
    };
    let params = binders(law);
    let args = arguments(law);
    let guard = law.when.as_ref().map(|e| expression(e, ctx));
    let reasons: Vec<_> = law.because.iter().map(|e| expression(e, ctx)).collect();
    let goal = conclusion(law, ctx);
    let mut out = Vec::new();
    for index in 0..=reasons.len() {
        let step = if index == reasons.len() {
            "implication".to_string()
        } else {
            format!("because{}", index + 1)
        };
        let step_name = format!("{name}_{step}");
        let step_goal = reasons.get(index).unwrap_or(&goal);
        out.push(format!(
            "// aver:dafny-obligation {step_name} {source_id}.{step}"
        ));
        out.push(format!("lemma {{:induction false}} {step_name}({params})"));
        if let Some(guard) = &guard {
            out.push(format!("  requires {guard}"));
        }
        for previous in reasons.iter().take(index) {
            out.push(format!("  requires {previous}"));
        }
        out.push(format!("  ensures {step_goal}"));
        out.push("{".to_string());
        for (cited, dependency) in &citations {
            let cited_name = lemma_name(&label(cited, dependency));
            // The forall range retains the supplier's guard. Calling its
            // lemma inside that range must prove every precondition; no
            // required fact is asserted unconditionally in the consumer.
            let range = dependency
                .when
                .as_ref()
                .map(|e| format!(" | {}", expression(e, ctx)))
                .unwrap_or_default();
            out.push(format!(
                "  forall {}{range} ensures {} {{",
                binders(dependency),
                conclusion(dependency, ctx)
            ));
            out.push(format!("    {cited_name}({});", arguments(dependency)));
            out.push("  }".to_string());
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

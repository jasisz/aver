/// Heuristics for auto-proving `verify law` theorems in Lean output.
///
/// This module is intentionally isolated from `toplevel.rs` so all heuristic
/// matching and proof-shape logic lives in one place.
mod arithmetic;
mod induction;
mod maps;
mod sampled;
mod shared;
mod spec;

use super::VerifyEmitMode;
use super::expr::aver_name_to_lean;
use crate::ast::{VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;
use crate::verify_law::{collect_missing_helper_law_hints, missing_helper_law_message};
use sampled::emit_guarded_domain_law;

pub struct AutoProof {
    pub support_lines: Vec<String>,
    pub proof_lines: Vec<String>,
    /// When true, the main theorem statement is already included in `support_lines`
    /// and should not be emitted separately by the caller.
    pub replaces_theorem: bool,
}

pub fn emit_verify_law_forall_auto_proof(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    verify_mode: VerifyEmitMode,
    theorem_base: &str,
    quant_params: &str,
    theorem_prop: &str,
) -> Option<AutoProof> {
    if verify_mode != VerifyEmitMode::NativeDecide {
        return None;
    }

    let intro_names: Vec<String> = law
        .givens
        .iter()
        .map(|g| aver_name_to_lean(&g.name))
        .collect();
    let proof_intro_names = extend_intro_names_with_premises(law, &intro_names);

    // Strategy 1: Structural induction on recursive sum types.
    if let Some(proof) = induction::emit_structural_induction_law(
        vb,
        law,
        ctx,
        &intro_names,
        theorem_base,
        quant_params,
        theorem_prop,
    ) {
        return Some(proof);
    }

    if law.lhs == law.rhs {
        return Some(AutoProof {
            support_lines: Vec::new(),
            proof_lines: intro_then(&proof_intro_names, vec!["rfl".to_string()]),
            replaces_theorem: false,
        });
    }

    arithmetic::emit_binary_wrapper_law(vb, law, ctx, &proof_intro_names)
        .map(|proof_lines| AutoProof {
            support_lines: Vec::new(),
            proof_lines,
            replaces_theorem: false,
        })
        .or_else(|| {
            arithmetic::emit_unary_wrapper_equivalence_law(vb, law, ctx, &proof_intro_names).map(
                |proof_lines| AutoProof {
                    support_lines: Vec::new(),
                    proof_lines,
                    replaces_theorem: false,
                },
            )
        })
        .or_else(|| spec::emit_spec_function_equivalence_law(vb, law, ctx, &proof_intro_names))
        .or_else(|| {
            maps::emit_direct_map_set_law(law, ctx, &proof_intro_names).map(|proof_lines| {
                AutoProof {
                    support_lines: Vec::new(),
                    proof_lines,
                    replaces_theorem: false,
                }
            })
        })
        .or_else(|| {
            maps::emit_map_update_law(vb, law, ctx, &proof_intro_names).map(|proof_lines| {
                AutoProof {
                    support_lines: Vec::new(),
                    proof_lines,
                    replaces_theorem: false,
                }
            })
        })
        .or_else(|| {
            maps::emit_map_increment_tracked_count_law(vb, law, ctx, &proof_intro_names).map(
                |proof_lines| AutoProof {
                    support_lines: Vec::new(),
                    proof_lines,
                    replaces_theorem: false,
                },
            )
        })
        .or_else(|| {
            emit_simp_omega_law(vb, law, ctx, &proof_intro_names).map(|proof_lines| AutoProof {
                support_lines: Vec::new(),
                proof_lines,
                replaces_theorem: false,
            })
        })
        .or_else(|| {
            emit_guarded_domain_law(law).map(|proof_lines| AutoProof {
                support_lines: Vec::new(),
                proof_lines,
                replaces_theorem: false,
            })
        })
}

/// Try `simp [fn_names...] ; omega` for laws on Int-domain functions.
///
/// Works when the function is a non-recursive match on Int args
/// (e.g. `computeScore(0, level) => 0`). `simp` unfolds the function,
/// `omega` closes the linear arithmetic goal.
fn emit_simp_omega_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
) -> Option<Vec<String>> {
    // Only attempt when all givens are Int.
    if law.givens.is_empty() || !law.givens.iter().all(|g| g.type_name == "Int") {
        return None;
    }
    // Collect all user-defined function names referenced in lhs and rhs.
    let mut fn_names = std::collections::BTreeSet::new();
    collect_fn_calls(&law.lhs, &mut fn_names);
    collect_fn_calls(&law.rhs, &mut fn_names);
    fn_names.insert(vb.fn_name.clone());
    // Only proceed if all referenced functions exist in ctx.
    if fn_names.iter().any(|n| !ctx.fn_sigs.contains_key(n)) {
        return None;
    }
    let lean_names: Vec<String> = fn_names.iter().map(|n| aver_name_to_lean(n)).collect();
    let simp_list = lean_names.join(", ");
    Some(intro_then(
        intro_names,
        vec![format!("simp only [{}] <;> omega", simp_list)],
    ))
}

fn collect_fn_calls(expr: &crate::ast::Expr, out: &mut std::collections::BTreeSet<String>) {
    use crate::ast::Expr;
    match expr {
        Expr::FnCall(f, args) => {
            if let Some(name) = crate::codegen::common::expr_to_dotted_name(f)
                && (!name.contains('.')
                    || name.chars().next().is_some_and(|c| c.is_lowercase()))
            {
                out.insert(name);
            }
            for arg in args {
                collect_fn_calls(arg, out);
            }
        }
        Expr::BinOp(_, l, r) => {
            collect_fn_calls(l, out);
            collect_fn_calls(r, out);
        }
        Expr::Attr(obj, _) => collect_fn_calls(obj, out),
        _ => {}
    }
}

pub fn emit_verify_law_support_theorems(
    vb: &VerifyBlock,
    _law: &VerifyLaw,
    ctx: &CodegenContext,
    _theorem_base: &str,
) -> Vec<String> {
    collect_missing_helper_law_hints(&ctx.items, &ctx.fn_sigs)
        .into_iter()
        .find(|hint| hint.line == vb.line && hint.fn_name == vb.fn_name)
        .map(|hint| {
            vec![
                format!("-- hint: {}", missing_helper_law_message(&hint)),
                "-- hint: the main theorem can stay generic, but it still needs those helper laws as intermediate theorems".to_string(),
            ]
        })
        .unwrap_or_default()
}

pub(super) fn intro_then(intro_names: &[String], steps: Vec<String>) -> Vec<String> {
    let mut lines = Vec::new();
    if !intro_names.is_empty() {
        lines.push(format!("intro {}", intro_names.join(" ")));
    }
    lines.extend(steps);
    indent_lines(lines, 2)
}

fn extend_intro_names_with_premises(law: &VerifyLaw, intro_names: &[String]) -> Vec<String> {
    let mut names = intro_names.to_vec();
    if law.when.is_some() {
        names.extend(intro_names.iter().map(|name| format!("h_{name}")));
        names.push("h_when".to_string());
    }
    names
}

pub(super) fn indent_lines(lines: Vec<String>, spaces: usize) -> Vec<String> {
    let pad = " ".repeat(spaces);
    lines
        .into_iter()
        .map(|line| format!("{pad}{line}"))
        .collect()
}

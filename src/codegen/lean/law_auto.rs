/// Heuristics for auto-proving `verify law` theorems in Lean output.
///
/// This module is intentionally isolated from `toplevel.rs` so all heuristic
/// matching and proof-shape logic lives in one place.
mod arithmetic;
mod induction;
mod json;
mod maps;
mod sampled;
mod shared;
mod spec;

use super::VerifyEmitMode;
use super::expr::aver_name_to_lean;
use crate::ast::{VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;
use json::emit_json_roundtrip_support_theorems;
use sampled::emit_guarded_sampled_domain_law;

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

    if let Some(proof_lines) = emit_guarded_sampled_domain_law(law) {
        return Some(AutoProof {
            support_lines: Vec::new(),
            proof_lines,
            replaces_theorem: false,
        });
    }

    // Strategy 1: Structural induction on recursive sum types.
    // Guarded laws already compile to sampled-domain theorems above.
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
            proof_lines: intro_then(&intro_names, vec!["rfl".to_string()]),
            replaces_theorem: false,
        });
    }

    arithmetic::emit_binary_wrapper_law(vb, law, ctx, &intro_names)
        .map(|proof_lines| AutoProof {
            support_lines: Vec::new(),
            proof_lines,
            replaces_theorem: false,
        })
        .or_else(|| {
            arithmetic::emit_unary_wrapper_equivalence_law(vb, law, ctx, &intro_names).map(
                |proof_lines| AutoProof {
                    support_lines: Vec::new(),
                    proof_lines,
                    replaces_theorem: false,
                },
            )
        })
        .or_else(|| spec::emit_spec_function_equivalence_law(vb, law, ctx, &intro_names))
        .or_else(|| {
            maps::emit_direct_map_set_law(law, ctx, &intro_names).map(|proof_lines| AutoProof {
                support_lines: Vec::new(),
                proof_lines,
                replaces_theorem: false,
            })
        })
        .or_else(|| {
            maps::emit_map_update_law(vb, law, ctx, &intro_names).map(|proof_lines| AutoProof {
                support_lines: Vec::new(),
                proof_lines,
                replaces_theorem: false,
            })
        })
        .or_else(|| {
            maps::emit_map_increment_tracked_count_law(vb, law, ctx, &intro_names).map(
                |proof_lines| AutoProof {
                    support_lines: Vec::new(),
                    proof_lines,
                    replaces_theorem: false,
                },
            )
        })
        .or_else(|| {
            maps::emit_recursive_map_presence_law(vb, law, ctx, &intro_names).map(|proof_lines| {
                AutoProof {
                    support_lines: Vec::new(),
                    proof_lines,
                    replaces_theorem: false,
                }
            })
        })
        .or_else(|| {
            maps::emit_recursive_map_tracked_count_law(vb, law, ctx, &intro_names).map(
                |proof_lines| AutoProof {
                    support_lines: Vec::new(),
                    proof_lines,
                    replaces_theorem: false,
                },
            )
        })
}

pub fn emit_verify_law_support_theorems(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    theorem_base: &str,
) -> Vec<String> {
    emit_json_roundtrip_support_theorems(vb, law, ctx, theorem_base).unwrap_or_default()
}

pub(super) fn intro_then(intro_names: &[String], steps: Vec<String>) -> Vec<String> {
    let mut lines = Vec::new();
    if !intro_names.is_empty() {
        lines.push(format!("intro {}", intro_names.join(" ")));
    }
    lines.extend(steps);
    indent_lines(lines, 2)
}

pub(super) fn indent_lines(lines: Vec<String>, spaces: usize) -> Vec<String> {
    let pad = " ".repeat(spaces);
    lines
        .into_iter()
        .map(|line| format!("{pad}{line}"))
        .collect()
}

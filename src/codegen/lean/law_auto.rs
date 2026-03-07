/// Heuristics for auto-proving `verify law` theorems in Lean output.
///
/// This module is intentionally isolated from `toplevel.rs` so all heuristic
/// matching and proof-shape logic lives in one place.
mod arithmetic;
mod maps;
mod shared;
mod spec;

use super::VerifyEmitMode;
use super::expr::aver_name_to_lean;
use crate::ast::{VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;

pub struct AutoProof {
    pub support_lines: Vec<String>,
    pub proof_lines: Vec<String>,
}

pub fn emit_verify_law_forall_auto_proof(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    verify_mode: VerifyEmitMode,
) -> Option<AutoProof> {
    if verify_mode != VerifyEmitMode::NativeDecide {
        return None;
    }

    let intro_names: Vec<String> = law
        .givens
        .iter()
        .map(|g| aver_name_to_lean(&g.name))
        .collect();

    if law.lhs == law.rhs {
        return Some(AutoProof {
            support_lines: Vec::new(),
            proof_lines: intro_then(&intro_names, vec!["rfl".to_string()]),
        });
    }

    arithmetic::emit_binary_wrapper_law(vb, law, ctx, &intro_names)
        .map(|proof_lines| AutoProof {
            support_lines: Vec::new(),
            proof_lines,
        })
        .or_else(|| {
            arithmetic::emit_unary_wrapper_equivalence_law(vb, law, ctx, &intro_names).map(
                |proof_lines| AutoProof {
                    support_lines: Vec::new(),
                    proof_lines,
                },
            )
        })
        .or_else(|| spec::emit_spec_function_equivalence_law(vb, law, ctx, &intro_names))
        .or_else(|| {
            maps::emit_direct_map_set_law(law, ctx, &intro_names).map(|proof_lines| AutoProof {
                support_lines: Vec::new(),
                proof_lines,
            })
        })
        .or_else(|| {
            maps::emit_map_update_law(vb, law, ctx, &intro_names).map(|proof_lines| AutoProof {
                support_lines: Vec::new(),
                proof_lines,
            })
        })
        .or_else(|| {
            maps::emit_map_increment_tracked_count_law(vb, law, ctx, &intro_names).map(
                |proof_lines| AutoProof {
                    support_lines: Vec::new(),
                    proof_lines,
                },
            )
        })
        .or_else(|| {
            maps::emit_recursive_map_presence_law(vb, law, ctx, &intro_names).map(|proof_lines| {
                AutoProof {
                    support_lines: Vec::new(),
                    proof_lines,
                }
            })
        })
        .or_else(|| {
            maps::emit_recursive_map_tracked_count_law(vb, law, ctx, &intro_names).map(
                |proof_lines| AutoProof {
                    support_lines: Vec::new(),
                    proof_lines,
                },
            )
        })
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

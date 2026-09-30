//! Native equations for checked two-phase counter walks. The offset matters
//! at and beyond zero: it lets a directly invoked worker return to the guard
//! without assuming that its caller established the guard's precondition.

use super::fn_def::{emit_fn_body_for, lower_pure_question_bang_for_emit};
use super::render::{emit_fn_params, sanitize_doc};
use super::{expr::aver_name_to_lean, is_pure_fn, types::type_annotation_to_lean};
use crate::{ast::FnDef, codegen::CodegenContext, ir::RecursionContract};

pub(in crate::codegen::lean) fn emit_native_int_phase_group(
    fns: &[&FnDef],
    ctx: &CodegenContext,
) -> Option<String> {
    if fns.len() != 2 || !fns.iter().all(|fd| is_pure_fn(fd)) {
        return None;
    }
    let contracts: Option<Vec<_>> = fns
        .iter()
        .map(|fd| {
            match crate::codegen::common::find_fn_contract_for_fn(ctx, fd)?
                .recursion
                .as_ref()?
            {
                RecursionContract::WellFoundedIntPhase {
                    param,
                    bound,
                    worker,
                } => Some((param.clone(), bound.clone(), *worker)),
                _ => None,
            }
        })
        .collect();
    // Oracle lifting adds parameters and rewrites `?` into matches. Original
    // effectful functions have no pure recursion contract; validate the exact
    // lifted call edges using the same backend-neutral recognizer instead.
    let measures = contracts.or_else(|| {
        let (param, bound, guarded) = crate::codegen::recursion::detect_int_phase(fns)?;
        fns.iter()
            .enumerate()
            .map(|(index, fd)| {
                Some((
                    fd.params.get(param)?.0.clone(),
                    bound.and_then(|i| fd.params.get(i).map(|(n, _)| n.clone())),
                    index != guarded,
                ))
            })
            .collect::<Option<Vec<_>>>()
    })?;
    let mut lines = vec!["mutual".to_string()];
    for (fd, (param, bound, worker)) in fns.iter().zip(measures) {
        let param = aver_name_to_lean(&param);
        let gap = bound.as_ref().map_or_else(
            || param.clone(),
            |bound| format!("{} - {param}", aver_name_to_lean(bound)),
        );
        let (offset, rank) = if worker { (" - 1", 1) } else { ("", 0) };
        if let Some(desc) = &fd.desc {
            lines.push(format!("  /-- {} -/", sanitize_doc(desc)));
        }
        lines.push(format!(
            "  def {} {} : {} :=",
            aver_name_to_lean(&fd.name),
            emit_fn_params(&fd.params),
            if fd.return_type.is_empty() {
                "Unit".to_string()
            } else {
                type_annotation_to_lean(&fd.return_type)
            }
        ));
        let lowered = lower_pure_question_bang_for_emit(fd);
        let body_fn = lowered.as_ref().unwrap_or(fd);
        lines.extend(
            emit_fn_body_for(body_fn, &body_fn.body, ctx)
                .lines()
                .map(|line| format!("  {line}")),
        );
        lines.push(format!("  termination_by (({gap}{offset}).toNat, {rank})"));
        lines.push("  decreasing_by all_goals (simp_wf; omega)".to_string());
        lines.push(String::new());
    }
    lines.push("end".to_string());
    Some(lines.join("\n"))
}

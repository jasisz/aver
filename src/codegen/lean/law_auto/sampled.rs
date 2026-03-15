use super::indent_lines;
use crate::ast::VerifyLaw;

/// Guarded laws with explicit finite `given` domains can be proved generically
/// by splitting the generated domain hypotheses and discharging each concrete
/// branch with `native_decide`.
pub(super) fn emit_guarded_domain_law(law: &VerifyLaw) -> Option<Vec<String>> {
    law.when.as_ref()?;
    if law.givens.is_empty() {
        return None;
    }

    let mut lines = Vec::new();
    let intro_names: Vec<String> = law
        .givens
        .iter()
        .map(|given| super::super::expr::aver_name_to_lean(&given.name))
        .collect();
    if !intro_names.is_empty() {
        lines.push(format!("intro {}", intro_names.join(" ")));
    }

    let domain_hyp_names: Vec<String> = law
        .givens
        .iter()
        .map(|given| format!("h_{}", super::super::expr::aver_name_to_lean(&given.name)))
        .collect();
    if !domain_hyp_names.is_empty() {
        lines.push(format!("intro {}", domain_hyp_names.join(" ")));
    }

    lines.extend(emit_guarded_domain_case_tactic_lines(
        law,
        0,
        &["native_decide".to_string()],
    )?);
    Some(indent_lines(lines, 2))
}

pub(super) fn emit_guarded_domain_case_tactic_lines(
    law: &VerifyLaw,
    indent: usize,
    terminal_lines: &[String],
) -> Option<Vec<String>> {
    law.when.as_ref()?;
    let domain_hyp_names: Vec<String> = law
        .givens
        .iter()
        .map(|given| format!("h_{}", super::super::expr::aver_name_to_lean(&given.name)))
        .collect();
    let domain_sizes: Vec<usize> = law
        .givens
        .iter()
        .map(|given| super::super::toplevel::law_given_domain_values(&given.domain).len())
        .collect();

    let mut lines = Vec::new();
    emit_domain_cases(
        &mut lines,
        &domain_hyp_names,
        &domain_sizes,
        0,
        indent,
        terminal_lines,
    );
    Some(lines)
}

fn emit_domain_cases(
    lines: &mut Vec<String>,
    hyp_names: &[String],
    domain_sizes: &[usize],
    idx: usize,
    indent: usize,
    terminal_lines: &[String],
) {
    let pad = " ".repeat(indent);
    if idx >= hyp_names.len() {
        lines.extend(terminal_lines.iter().map(|line| format!("{pad}{line}")));
        return;
    }

    match domain_sizes[idx] {
        0 => lines.push(format!("{pad}cases {}", hyp_names[idx])),
        1 => {
            lines.push(format!("{pad}cases {}", hyp_names[idx]));
            emit_domain_cases(
                lines,
                hyp_names,
                domain_sizes,
                idx + 1,
                indent,
                terminal_lines,
            );
        }
        _ => emit_disjunction_cases(
            lines,
            hyp_names,
            domain_sizes,
            idx,
            &hyp_names[idx],
            domain_sizes[idx],
            indent,
            terminal_lines,
        ),
    }
}

#[allow(clippy::too_many_arguments)]
fn emit_disjunction_cases(
    lines: &mut Vec<String>,
    hyp_names: &[String],
    domain_sizes: &[usize],
    idx: usize,
    hyp_name: &str,
    remaining_cases: usize,
    indent: usize,
    terminal_lines: &[String],
) {
    let pad = " ".repeat(indent);
    if remaining_cases == 1 {
        lines.push(format!("{pad}cases {hyp_name}"));
        emit_domain_cases(
            lines,
            hyp_names,
            domain_sizes,
            idx + 1,
            indent,
            terminal_lines,
        );
        return;
    }

    let left_name = format!("{hyp_name}_case");
    let rest_name = format!("{hyp_name}_rest");
    lines.push(format!(
        "{pad}rcases {hyp_name} with {left_name} | {rest_name}"
    ));
    lines.push(format!("{pad}· cases {left_name}"));
    emit_domain_cases(
        lines,
        hyp_names,
        domain_sizes,
        idx + 1,
        indent + 2,
        terminal_lines,
    );
    lines.push(format!("{pad}·"));
    emit_disjunction_cases(
        lines,
        hyp_names,
        domain_sizes,
        idx,
        &rest_name,
        remaining_cases - 1,
        indent + 2,
        terminal_lines,
    );
}

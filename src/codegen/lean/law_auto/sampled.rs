use crate::ast::VerifyLaw;

use super::indent_lines;

pub(super) fn emit_guarded_sampled_domain_law(law: &VerifyLaw) -> Option<Vec<String>> {
    law.when.as_ref()?;

    let mut lines = Vec::new();
    let intro_names: Vec<String> = law
        .givens
        .iter()
        .map(|given| super::super::expr::aver_name_to_lean(&given.name))
        .collect();
    if !intro_names.is_empty() {
        lines.push(format!("intro {}", intro_names.join(" ")));
    }

    let hyp_names: Vec<String> = law
        .givens
        .iter()
        .map(|given| format!("h_{}", super::super::expr::aver_name_to_lean(&given.name)))
        .collect();
    if !hyp_names.is_empty() {
        lines.push(format!("intro {}", hyp_names.join(" ")));
    }

    let domain_sizes: Vec<usize> = law
        .givens
        .iter()
        .map(|given| super::super::toplevel::law_given_domain_values(&given.domain).len())
        .collect();

    emit_domain_cases(&mut lines, &hyp_names, &domain_sizes, 0, 0);
    Some(indent_lines(lines, 2))
}

fn emit_domain_cases(
    lines: &mut Vec<String>,
    hyp_names: &[String],
    domain_sizes: &[usize],
    idx: usize,
    indent: usize,
) {
    let pad = " ".repeat(indent);
    if idx >= hyp_names.len() {
        lines.push(format!("{pad}native_decide"));
        return;
    }

    match domain_sizes[idx] {
        0 => lines.push(format!("{pad}cases {}", hyp_names[idx])),
        1 => {
            lines.push(format!("{pad}cases {}", hyp_names[idx]));
            emit_domain_cases(lines, hyp_names, domain_sizes, idx + 1, indent);
        }
        _ => emit_disjunction_cases(
            lines,
            hyp_names,
            domain_sizes,
            idx,
            &hyp_names[idx],
            domain_sizes[idx],
            indent,
        ),
    }
}

fn emit_disjunction_cases(
    lines: &mut Vec<String>,
    hyp_names: &[String],
    domain_sizes: &[usize],
    idx: usize,
    hyp_name: &str,
    remaining_cases: usize,
    indent: usize,
) {
    let pad = " ".repeat(indent);
    if remaining_cases == 1 {
        lines.push(format!("{pad}cases {hyp_name}"));
        emit_domain_cases(lines, hyp_names, domain_sizes, idx + 1, indent);
        return;
    }

    let left_name = format!("{hyp_name}_case");
    let rest_name = format!("{hyp_name}_rest");
    lines.push(format!(
        "{pad}rcases {hyp_name} with {left_name} | {rest_name}"
    ));
    lines.push(format!("{pad}· cases {left_name}"));
    emit_domain_cases(lines, hyp_names, domain_sizes, idx + 1, indent + 2);
    lines.push(format!("{pad}·"));
    emit_disjunction_cases(
        lines,
        hyp_names,
        domain_sizes,
        idx,
        &rest_name,
        remaining_cases - 1,
        indent + 2,
    );
}

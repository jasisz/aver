use std::collections::HashSet;

use crate::{
    ast::{Expr, FnDef, Spanned, Stmt, StrPart, TailCallData},
    checker::CheckFinding,
};

use super::{expr_to_short_str, is_nontrivial_arithmetic, is_nontrivial_pure_fncall};

/// Maximum reachable count for each structural expression.
///
/// Aver expressions are eager, so sibling children compose by addition. Match
/// arms are mutually exclusive, so their counts compose by maximum. Keeping
/// that distinction prevents a suggestion to hoist two arm-local expressions
/// that can never execute together. Different entries may reach their maxima
/// on different paths; each diagnostic is decided independently.
#[derive(Default)]
struct PathCounts<'a> {
    entries: Vec<(&'a Spanned<Expr>, usize)>,
}

impl<'a> PathCounts<'a> {
    fn add_occurrence(&mut self, expression: &'a Spanned<Expr>) {
        if let Some((_, count)) = self
            .entries
            .iter_mut()
            .find(|(seen, _)| seen.node == expression.node)
        {
            *count += 1;
        } else {
            self.entries.push((expression, 1));
        }
    }

    fn add_sequential(&mut self, other: Self) {
        for (expression, count) in other.entries {
            if let Some((first, seen_count)) = self
                .entries
                .iter_mut()
                .find(|(seen, _)| seen.node == expression.node)
            {
                *seen_count += count;
                if expression.line < first.line {
                    *first = expression;
                }
            } else {
                self.entries.push((expression, count));
            }
        }
    }

    fn merge_alternative(&mut self, other: Self) {
        for (expression, count) in other.entries {
            if let Some((representative, max_count)) = self
                .entries
                .iter_mut()
                .find(|(seen, _)| seen.node == expression.node)
            {
                if count > *max_count {
                    *representative = expression;
                    *max_count = count;
                } else if count == *max_count && expression.line < representative.line {
                    *representative = expression;
                }
            } else {
                self.entries.push((expression, count));
            }
        }
    }
}

pub(super) fn check_fn_body_duplicates(
    fd: &FnDef,
    match_warned_messages: &HashSet<String>,
    warnings: &mut Vec<CheckFinding>,
) {
    let mut counts = PathCounts::default();
    for statement in fd.body.stmts() {
        let expression = match statement {
            Stmt::Expr(expression) | Stmt::Binding(_, _, expression) => expression,
        };
        counts.add_sequential(count_expression_paths(expression));
    }

    for (expression, count) in counts.entries {
        if count < 2 {
            continue;
        }

        let expression_string = expr_to_short_str(&expression.node);
        let already_warned = match_warned_messages
            .iter()
            .any(|message| message.contains(&expression_string));
        if already_warned {
            continue;
        }

        warnings.push(CheckFinding {
            line: expression.line,
            module: None,
            file: None,
            fn_name: None,
            message: format!(
                "`{expression_string}` is computed {count} times in this function — consider extracting to a binding"
            ),
            extra_spans: vec![],
        });
    }
}

fn count_expression_paths(expression: &Spanned<Expr>) -> PathCounts<'_> {
    let mut counts = PathCounts::default();
    if is_nontrivial_arithmetic(&expression.node) || is_nontrivial_pure_fncall(&expression.node) {
        counts.add_occurrence(expression);
    }

    match &expression.node {
        Expr::Match { subject, arms } => {
            counts.add_sequential(count_expression_paths(subject));

            let mut alternatives = PathCounts::default();
            for arm in arms {
                alternatives.merge_alternative(count_expression_paths(&arm.body));
            }
            counts.add_sequential(alternatives);
        }
        Expr::BinOp(_, left, right) => {
            counts.add_sequential(count_expression_paths(left));
            counts.add_sequential(count_expression_paths(right));
        }
        Expr::Neg(inner)
        | Expr::Attr(inner, _)
        | Expr::Constructor(_, Some(inner))
        | Expr::ErrorProp(inner) => counts.add_sequential(count_expression_paths(inner)),
        Expr::FnCall(callee, arguments) => {
            counts.add_sequential(count_expression_paths(callee));
            add_all_sequential(&mut counts, arguments);
        }
        Expr::List(elements) | Expr::Tuple(elements) | Expr::IndependentProduct(elements, _) => {
            add_all_sequential(&mut counts, elements)
        }
        Expr::MapLiteral(entries) => {
            for (key, value) in entries {
                counts.add_sequential(count_expression_paths(key));
                counts.add_sequential(count_expression_paths(value));
            }
        }
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let StrPart::Parsed(parsed) = part {
                    counts.add_sequential(count_expression_paths(parsed));
                }
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, value) in fields {
                counts.add_sequential(count_expression_paths(value));
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            counts.add_sequential(count_expression_paths(base));
            for (_, value) in updates {
                counts.add_sequential(count_expression_paths(value));
            }
        }
        Expr::TailCall(call) => {
            let TailCallData { args, .. } = call.as_ref();
            add_all_sequential(&mut counts, args);
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } | Expr::Constructor(_, None) => {}
    }

    counts
}

fn add_all_sequential<'a>(counts: &mut PathCounts<'a>, expressions: &'a [Spanned<Expr>]) {
    for expression in expressions {
        counts.add_sequential(count_expression_paths(expression));
    }
}

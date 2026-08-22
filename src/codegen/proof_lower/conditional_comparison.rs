//! Lower the canonical Peano conditional-comparison law into `ProofIR`.
//!
//! This is syntax discovery, not backend rendering: source AST tells us that
//! the premise and conclusion are single canonical Peano comparisons, while
//! `ProofLowerInputs` resolves every called function/type to its owning symbol
//! before the detector inspects that declaration. Lean and future proof
//! backends consume the resulting `ProofStrategy` pin instead of independently
//! deciding whether the law is universal.

use crate::ast::{Expr, Literal, Spanned, VerifyLaw};
use crate::codegen::common::expr_to_dotted_name;
use crate::ir::proof_ir::{PeanoComparison, PeanoComparisonKind};

use super::ProofLowerInputs;

pub(super) struct Plan {
    pub comparisons: Vec<PeanoComparison>,
    pub negated_premise: Option<PeanoComparison>,
}

fn is_linear_nat_arg(expr: &Spanned<Expr>) -> bool {
    match &expr.node {
        Expr::Ident(_) | Expr::Resolved { .. } => true,
        Expr::Literal(Literal::Int(_)) => true,
        Expr::FnCall(callee, args) => match expr_to_dotted_name(&callee.node) {
            Some(name) => match name.rsplit('.').next().unwrap_or(name.as_str()) {
                "S" => args.len() == 1 && is_linear_nat_arg(&args[0]),
                "Z" => args.is_empty(),
                _ => false,
            },
            None => false,
        },
        _ => false,
    }
}

fn comparison_call_plan(
    expr: &Spanned<Expr>,
    inputs: &ProofLowerInputs<'_>,
    scope: Option<&str>,
) -> Option<PeanoComparison> {
    let Expr::FnCall(callee, args) = &expr.node else {
        return None;
    };
    if args.len() != 2 || !is_linear_nat_arg(&args[0]) || !is_linear_nat_arg(&args[1]) {
        return None;
    }
    let name = expr_to_dotted_name(&callee.node)?;
    let fn_id = inputs.symbol_table.resolve_fn_id_in(&name, scope)?;
    let fd = inputs.find_fn_def_in_scope(&name, scope)?;
    let (_, operand_type) = fd.params.first()?;
    let owner_scope = inputs.fn_owning_scope(fd);
    let type_def = inputs.find_type_def_in_scope(operand_type, owner_scope)?;
    let peano = crate::codegen::proof_recognize::detect_canonical_peano(type_def)?;
    let kind = match crate::codegen::proof_recognize::detect_nat_compare_op_for_peano(fd, &peano)? {
        crate::codegen::proof_recognize::NatCompareKind::Le => PeanoComparisonKind::Le,
        crate::codegen::proof_recognize::NatCompareKind::Lt => PeanoComparisonKind::Lt,
        crate::codegen::proof_recognize::NatCompareKind::Eq => PeanoComparisonKind::Eq,
    };
    Some(PeanoComparison { fn_id, kind })
}

fn negated_premise_inner(expr: &Spanned<Expr>) -> Option<&Spanned<Expr>> {
    let Expr::FnCall(callee, args) = &expr.node else {
        return None;
    };
    (expr_to_dotted_name(&callee.node).as_deref() == Some("Bool.not") && args.len() == 1)
        .then(|| &args[0])
}

/// Detect `when R1(a, b); R2(c, d) => true`, where both relations are
/// canonical Peano comparisons over linear constructor terms. The plan carries
/// the exact relation identities/kinds plus the optional negated-premise
/// relation, so a backend has no declaration shape left to rediscover.
pub(super) fn detect(
    law: &VerifyLaw,
    inputs: &ProofLowerInputs<'_>,
    scope: Option<&str>,
) -> Option<Plan> {
    let when = law.when.as_ref()?;
    if !matches!(&law.rhs.node, Expr::Literal(Literal::Bool(true))) {
        return None;
    }
    let conclusion = comparison_call_plan(&law.lhs, inputs, scope)?;
    let (premise, premise_negated) = match negated_premise_inner(when) {
        Some(inner) => (comparison_call_plan(inner, inputs, scope)?, true),
        None => (comparison_call_plan(when, inputs, scope)?, false),
    };
    let mut comparisons = vec![conclusion];
    if !comparisons
        .iter()
        .any(|comparison| comparison.fn_id == premise.fn_id)
    {
        comparisons.push(premise.clone());
    }
    Some(Plan {
        comparisons,
        negated_premise: premise_negated.then_some(premise),
    })
}

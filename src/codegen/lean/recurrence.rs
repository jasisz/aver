use super::expr::aver_name_to_lean;
use crate::ast::{BinOp, Expr, FnDef, Literal, MatchArm, Pattern, Spanned};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct AffinePairExpr {
    pub prev2_coeff: i64,
    pub prev1_coeff: i64,
    pub constant: i64,
}

impl AffinePairExpr {
    fn prev2() -> Self {
        Self {
            prev2_coeff: 1,
            prev1_coeff: 0,
            constant: 0,
        }
    }

    fn prev1() -> Self {
        Self {
            prev2_coeff: 0,
            prev1_coeff: 1,
            constant: 0,
        }
    }

    fn constant(value: i64) -> Self {
        Self {
            prev2_coeff: 0,
            prev1_coeff: 0,
            constant: value,
        }
    }

    fn add(self, other: Self) -> Self {
        Self {
            prev2_coeff: self.prev2_coeff + other.prev2_coeff,
            prev1_coeff: self.prev1_coeff + other.prev1_coeff,
            constant: self.constant + other.constant,
        }
    }

    fn sub(self, other: Self) -> Self {
        Self {
            prev2_coeff: self.prev2_coeff - other.prev2_coeff,
            prev1_coeff: self.prev1_coeff - other.prev1_coeff,
            constant: self.constant - other.constant,
        }
    }

    fn scale(self, factor: i64) -> Self {
        Self {
            prev2_coeff: self.prev2_coeff * factor,
            prev1_coeff: self.prev1_coeff * factor,
            constant: self.constant * factor,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct SecondOrderIntLinearRecurrenceShape {
    pub param_name: String,
    pub negative_branch: Spanned<Expr>,
    pub base0: Spanned<Expr>,
    pub base1: Spanned<Expr>,
    pub recurrence: AffinePairExpr,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct TailrecIntLinearPairWorkerShape {
    pub count_param_name: String,
    pub prev_param_name: String,
    pub curr_param_name: String,
    pub recurrence: AffinePairExpr,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct TailrecIntLinearPairWrapperShape {
    pub param_name: String,
    pub negative_branch: Spanned<Expr>,
    pub helper_fn_name: String,
    pub seed_prev: Spanned<Expr>,
    pub seed_curr: Spanned<Expr>,
}

pub(crate) fn recurrence_nat_helper_name(fn_name: &str) -> String {
    format!("{}__nat", aver_name_to_lean(fn_name))
}

pub(crate) fn fuel_helper_name(fn_name: &str) -> String {
    format!("{}__fuel", aver_name_to_lean(fn_name))
}

pub(crate) fn render_affine_pair_expr(expr: AffinePairExpr, prev2: &str, prev1: &str) -> String {
    let mut terms = Vec::new();
    push_affine_term(&mut terms, expr.prev2_coeff, prev2);
    push_affine_term(&mut terms, expr.prev1_coeff, prev1);
    if expr.constant != 0 || terms.is_empty() {
        terms.push(expr.constant.to_string());
    }
    terms.join(" + ")
}

fn push_affine_term(terms: &mut Vec<String>, coeff: i64, value: &str) {
    match coeff {
        0 => {}
        1 => terms.push(value.to_string()),
        -1 => terms.push(format!("-({value})")),
        _ => terms.push(format!("({coeff}) * ({value})")),
    }
}

pub(crate) fn detect_second_order_int_linear_recurrence(
    fd: &FnDef,
) -> Option<SecondOrderIntLinearRecurrenceShape> {
    let [(param_name, param_type)] = fd.params.as_slice() else {
        return None;
    };
    if param_type != "Int" || fd.return_type != "Int" {
        return None;
    }

    let body = fd.body.tail_expr()?;
    let (negative_branch, nonnegative_branch) = split_negative_guard(&body.node, param_name)?;
    let Expr::Match {
        subject: inner_subject,
        arms,
    } = nonnegative_branch
    else {
        return None;
    };
    if !matches!(&inner_subject.node, Expr::Ident(name) if name == param_name) {
        return None;
    }

    let base0 = match_arm_body_for_int(arms, 0)?;
    let base1 = match_arm_body_for_int(arms, 1)?;
    let recursive_branch = match_arm_body_for_wildcard(arms)?;
    let recurrence = parse_recurrence_affine(&recursive_branch.node, &fd.name, param_name)?;

    Some(SecondOrderIntLinearRecurrenceShape {
        param_name: param_name.clone(),
        negative_branch: negative_branch.clone(),
        base0: base0.clone(),
        base1: base1.clone(),
        recurrence,
    })
}

pub(crate) fn detect_tailrec_int_linear_pair_worker(
    fd: &FnDef,
) -> Option<TailrecIntLinearPairWorkerShape> {
    let [
        (count_param_name, count_ty),
        (prev_param_name, prev_ty),
        (curr_param_name, curr_ty),
    ] = fd.params.as_slice()
    else {
        return None;
    };
    if count_ty != "Int" || prev_ty != "Int" || curr_ty != "Int" || fd.return_type != "Int" {
        return None;
    }

    let body = fd.body.tail_expr()?;
    let Expr::Match { subject, arms, .. } = &body.node else {
        return None;
    };
    if !matches!(&subject.node, Expr::Ident(name) if name == count_param_name) {
        return None;
    }

    let zero_branch = match_arm_body_for_int(arms, 0)?;
    if !matches!(&zero_branch.node, Expr::Ident(name) if name == prev_param_name) {
        return None;
    }

    let recursive_branch = match_arm_body_for_wildcard(arms)?;
    let args: &[Spanned<Expr>] = match &recursive_branch.node {
        Expr::FnCall(callee, args) if matches!(&callee.node, Expr::Ident(name) if name == &fd.name) => {
            args.as_slice()
        }
        Expr::TailCall(call) if call.0 == fd.name => call.1.as_slice(),
        _ => return None,
    };
    if args.len() != 3 {
        return None;
    }
    if !matches_int_sub_positive(&args[0].node, count_param_name, 1)
        || !matches!(&args[1].node, Expr::Ident(name) if name == curr_param_name.as_str())
    {
        return None;
    }

    let recurrence = parse_affine_expr(&args[2].node, prev_param_name, curr_param_name)?;
    Some(TailrecIntLinearPairWorkerShape {
        count_param_name: count_param_name.clone(),
        prev_param_name: prev_param_name.clone(),
        curr_param_name: curr_param_name.clone(),
        recurrence,
    })
}

pub(crate) fn detect_tailrec_int_linear_pair_wrapper(
    fd: &FnDef,
) -> Option<TailrecIntLinearPairWrapperShape> {
    let [(param_name, param_type)] = fd.params.as_slice() else {
        return None;
    };
    if param_type != "Int" || fd.return_type != "Int" {
        return None;
    }

    let body = fd.body.tail_expr()?;
    let (negative_branch, nonnegative_branch) = split_negative_guard(&body.node, param_name)?;
    let Expr::FnCall(callee, args) = nonnegative_branch else {
        return None;
    };
    let Expr::Ident(helper_fn_name) = &callee.node else {
        return None;
    };
    let [count_arg, seed_prev, seed_curr] = args.as_slice() else {
        return None;
    };
    if !matches!(&count_arg.node, Expr::Ident(name) if name == param_name) {
        return None;
    }

    Some(TailrecIntLinearPairWrapperShape {
        param_name: param_name.clone(),
        negative_branch: negative_branch.clone(),
        helper_fn_name: helper_fn_name.clone(),
        seed_prev: seed_prev.clone(),
        seed_curr: seed_curr.clone(),
    })
}

fn split_negative_guard<'a>(
    expr: &'a Expr,
    param_name: &str,
) -> Option<(&'a Spanned<Expr>, &'a Expr)> {
    let Expr::Match { subject, arms, .. } = expr else {
        return None;
    };
    let Expr::BinOp(BinOp::Lt, left, right) = &subject.node else {
        return None;
    };
    if !matches!(&left.node, Expr::Ident(name) if name == param_name)
        || !matches!(&right.node, Expr::Literal(Literal::Int(0)))
    {
        return None;
    }

    let mut negative_branch = None;
    let mut nonnegative_branch = None;
    for arm in arms {
        match &arm.pattern {
            Pattern::Literal(Literal::Bool(true)) => negative_branch = Some(arm.body.as_ref()),
            Pattern::Literal(Literal::Bool(false)) => nonnegative_branch = Some(arm.body.as_ref()),
            _ => return None,
        }
    }

    Some((negative_branch?, &nonnegative_branch?.node))
}

fn match_arm_body_for_int(arms: &[MatchArm], expected: i64) -> Option<&Spanned<Expr>> {
    arms.iter().find_map(|arm| match &arm.pattern {
        Pattern::Literal(Literal::Int(value)) if *value == expected => Some(arm.body.as_ref()),
        _ => None,
    })
}

fn match_arm_body_for_wildcard(arms: &[MatchArm]) -> Option<&Spanned<Expr>> {
    arms.iter().find_map(|arm| match arm.pattern {
        Pattern::Wildcard => Some(arm.body.as_ref()),
        _ => None,
    })
}

fn parse_recurrence_affine(expr: &Expr, fn_name: &str, param_name: &str) -> Option<AffinePairExpr> {
    match expr {
        Expr::Literal(Literal::Int(value)) => Some(AffinePairExpr::constant(*value)),
        Expr::FnCall(callee, args) => {
            if !matches!(&callee.node, Expr::Ident(name) if name == fn_name) || args.len() != 1 {
                return None;
            }
            let offset = int_sub_positive_offset(&args[0].node, param_name)?;
            match offset {
                1 => Some(AffinePairExpr::prev1()),
                2 => Some(AffinePairExpr::prev2()),
                _ => None,
            }
        }
        Expr::BinOp(BinOp::Add, left, right) => {
            Some(
                parse_recurrence_affine(&left.node, fn_name, param_name)?
                    .add(parse_recurrence_affine(&right.node, fn_name, param_name)?),
            )
        }
        Expr::BinOp(BinOp::Sub, left, right) => {
            Some(
                parse_recurrence_affine(&left.node, fn_name, param_name)?
                    .sub(parse_recurrence_affine(&right.node, fn_name, param_name)?),
            )
        }
        Expr::BinOp(BinOp::Mul, left, right) => {
            if let Some(scale) = int_literal(&left.node) {
                return Some(
                    parse_recurrence_affine(&right.node, fn_name, param_name)?.scale(scale),
                );
            }
            if let Some(scale) = int_literal(&right.node) {
                return Some(
                    parse_recurrence_affine(&left.node, fn_name, param_name)?.scale(scale),
                );
            }
            None
        }
        _ => None,
    }
}

fn parse_affine_expr(expr: &Expr, prev2_name: &str, prev1_name: &str) -> Option<AffinePairExpr> {
    match expr {
        Expr::Literal(Literal::Int(value)) => Some(AffinePairExpr::constant(*value)),
        Expr::Ident(name) if name == prev2_name => Some(AffinePairExpr::prev2()),
        Expr::Ident(name) if name == prev1_name => Some(AffinePairExpr::prev1()),
        Expr::BinOp(BinOp::Add, left, right) => Some(
            parse_affine_expr(&left.node, prev2_name, prev1_name)?.add(parse_affine_expr(
                &right.node,
                prev2_name,
                prev1_name,
            )?),
        ),
        Expr::BinOp(BinOp::Sub, left, right) => Some(
            parse_affine_expr(&left.node, prev2_name, prev1_name)?.sub(parse_affine_expr(
                &right.node,
                prev2_name,
                prev1_name,
            )?),
        ),
        Expr::BinOp(BinOp::Mul, left, right) => {
            if let Some(scale) = int_literal(&left.node) {
                return Some(parse_affine_expr(&right.node, prev2_name, prev1_name)?.scale(scale));
            }
            if let Some(scale) = int_literal(&right.node) {
                return Some(parse_affine_expr(&left.node, prev2_name, prev1_name)?.scale(scale));
            }
            None
        }
        _ => None,
    }
}

fn int_literal(expr: &Expr) -> Option<i64> {
    match expr {
        Expr::Literal(Literal::Int(value)) => Some(*value),
        _ => None,
    }
}

fn matches_int_sub_positive(expr: &Expr, param_name: &str, expected: i64) -> bool {
    int_sub_positive_offset(expr, param_name) == Some(expected)
}

fn int_sub_positive_offset(expr: &Expr, param_name: &str) -> Option<i64> {
    let Expr::BinOp(BinOp::Sub, left, right) = expr else {
        return None;
    };
    if !matches!(&left.node, Expr::Ident(name) if name == param_name) {
        return None;
    }
    let value = int_literal(&right.node)?;
    (value > 0).then_some(value)
}

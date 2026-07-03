//! Content-blind strict-order reflection for unary `Fraction` cones.
//!
//! Recognized shape:
//!
//!   * premise: `lessThan(F(a), F(b))`
//!   * claim: a Boolean subject whose body is `a < b`
//!   * premise: `0 < x.bottom && isNonNeg(minus(x, F(e))) && lessThan(x, F(m))`
//!   * claim: a Boolean subject whose body is either `e <= m - 1`, or any
//!     linear integer comparison implied by that bridge fact.
//!
//! The carrier `F` is captured from the premise AST. The proof never unfolds
//! `F`; it cites earlier sibling pool laws for monotonicity and denominator
//! positivity of that same `F`, then closes the contradiction with a generic
//! integer cross-order kit.

use super::shared::{expr_dotted_name, find_fn_def_by_call_name, substitute_expr};
use super::{AutoProof, aver_name_to_lean};
use crate::ast::{BinOp, Expr, FnDef, Literal, Spanned, Stmt, VerifyBlock, VerifyKind, VerifyLaw};
use crate::codegen::CodegenContext;

// Private copy kept off the shared substrate on purpose: this rung predates
// the shared walkers and its recognition is TailCall-blind by construction
// (measured inert on the corpus). Migrating it is banked for the next
// substrate wave; do not extend this copy — extend shared.rs instead.
fn as_call(expr: &Spanned<Expr>) -> Option<(String, &[Spanned<Expr>])> {
    let Expr::FnCall(callee, args) = &expr.node else {
        return None;
    };
    let dotted = expr_dotted_name(callee)?;
    let short = dotted.rsplit('.').next().unwrap_or(&dotted).to_string();
    Some((short, args.as_slice()))
}

fn call_named<'a>(
    expr: &'a Spanned<Expr>,
    name: &str,
    n: usize,
) -> Option<(String, &'a [Spanned<Expr>])> {
    let (short, args) = as_call(expr)?;
    let Expr::FnCall(callee, _) = &expr.node else {
        return None;
    };
    let dotted = expr_dotted_name(callee)?;
    (short == name && args.len() == n).then_some((dotted, args))
}

/// **syntax-discovery-only**: the return annotation is source spelling —
/// bare `Fraction` inside the defining module, dep-qualified
/// `Domain.Rational.Fraction` elsewhere. This only gates whether the
/// strategy attempts; the kernel judges every attempt.
fn is_unary_fraction_fn(fd: &FnDef) -> bool {
    matches!(fd.params.as_slice(), [(_, ty)] if ty.trim() == "Int")
        && fd.return_type.rsplit('.').next() == Some("Fraction")
        && fd.effects.is_empty()
}

fn subject_body<'a>(fn_name: &str, ctx: &'a CodegenContext) -> Option<&'a Spanned<Expr>> {
    let fd = find_fn_def_by_call_name(ctx, fn_name)?;
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return None;
    };
    Some(body)
}

fn sibling_blocks<'a>(vb: &VerifyBlock, ctx: &'a CodegenContext) -> Vec<&'a VerifyBlock> {
    for module in &ctx.modules {
        if module
            .verify_laws
            .iter()
            .any(|b| b.line == vb.line && b.fn_name == vb.fn_name)
        {
            return module.verify_laws.iter().collect();
        }
    }
    ctx.items
        .iter()
        .filter_map(|it| match it {
            crate::ast::TopLevel::Verify(b) => Some(b),
            _ => None,
        })
        .collect()
}

#[derive(Clone)]
struct PoolCite {
    thm: String,
    subject: String,
}

fn law_theorem(prev: &VerifyBlock, law: &VerifyLaw, ctx: &CodegenContext) -> Option<String> {
    crate::codegen::lean::toplevel::law_as_lemma_statement(prev, law, ctx).map(|(thm, _)| thm)
}

fn same_short(dotted: &str, short: &str) -> bool {
    dotted.rsplit('.').next().unwrap_or(dotted) == short
}

fn same_expr(a: &Spanned<Expr>, b: &Spanned<Expr>, ctx: &CodegenContext) -> bool {
    super::super::expr::emit_expr_legacy(a, ctx, None)
        == super::super::expr::emit_expr_legacy(b, ctx, None)
}

fn is_int_literal(expr: &Spanned<Expr>) -> bool {
    matches!(
        &expr.node,
        Expr::Literal(Literal::Int(_)) | Expr::Literal(Literal::BigInt(_))
    )
}

fn is_linear_int_expr(expr: &Spanned<Expr>, allowed: &std::collections::BTreeSet<String>) -> bool {
    match &expr.node {
        Expr::Literal(Literal::Int(_)) | Expr::Literal(Literal::BigInt(_)) => true,
        Expr::Ident(name) => allowed.contains(name),
        Expr::Resolved { name, .. } => allowed.contains(name),
        Expr::Neg(inner) => is_linear_int_expr(inner, allowed),
        Expr::BinOp(BinOp::Add | BinOp::Sub, l, r) => {
            is_linear_int_expr(l, allowed) && is_linear_int_expr(r, allowed)
        }
        Expr::BinOp(BinOp::Mul, l, r) => {
            (is_int_literal(l) && is_linear_int_expr(r, allowed))
                || (is_int_literal(r) && is_linear_int_expr(l, allowed))
        }
        // An Int-returning function application (e.g. the maxInt / minInt of the
        // Section 9.3 sum-exponent bounds, max(n,m) / min(ex,ey)) is opaque to
        // linear arithmetic: omega atomizes the whole application. Accept it as a
        // linear atom when its arguments are themselves linear over the allowed
        // givens, so a claim like `e <= 1 + maxInt(a, b)` is recognized as the
        // magnitude-bracket class; the emitted bridge proof then discharges it by
        // omega with the call standing as a single atom -- the same atomization
        // omega performs, just admitted one step earlier by the recognizer.
        Expr::FnCall(_, args) => args.iter().all(|a| is_linear_int_expr(a, allowed)),
        _ => false,
    }
}

fn is_linear_int_comparison(
    expr: &Spanned<Expr>,
    allowed: &std::collections::BTreeSet<String>,
) -> bool {
    matches!(
        &expr.node,
        Expr::BinOp(
            BinOp::Eq | BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte,
            l,
            r
        ) if is_linear_int_expr(l, allowed) && is_linear_int_expr(r, allowed)
    )
}

fn flatten_bool_and<'a>(expr: &'a Spanned<Expr>, out: &mut Vec<&'a Spanned<Expr>>) {
    if let Some((dotted, args)) = call_named(expr, "and", 2)
        && dotted == "Bool.and"
    {
        flatten_bool_and(&args[0], out);
        flatten_bool_and(&args[1], out);
        return;
    }
    out.push(expr);
}

fn strict_field_pos_call(expr: &Spanned<Expr>, field: &str) -> Option<(String, Spanned<Expr>)> {
    let Expr::BinOp(op, l, r) = &expr.node else {
        return None;
    };
    let (attr_side, lit_side) = match op {
        BinOp::Gt => (l, r),
        BinOp::Lt => (r, l),
        _ => return None,
    };
    if !matches!(&lit_side.node, Expr::Literal(Literal::Int(0))) {
        return None;
    }
    let Expr::Attr(base, got_field) = &attr_side.node else {
        return None;
    };
    if got_field != field {
        return None;
    }
    let Expr::FnCall(callee, args) = &base.node else {
        return None;
    };
    if args.len() != 1 {
        return None;
    }
    Some((expr_dotted_name(callee)?, args[0].clone()))
}

fn is_joint_positivity_body(body: &Spanned<Expr>, f_short: &str) -> bool {
    let Some((_, args)) = call_named(body, "and", 2) else {
        return false;
    };
    let has = |expr: &Spanned<Expr>, field: &str| {
        strict_field_pos_call(expr, field)
            .map(|(f, _)| same_short(&f, f_short))
            .unwrap_or(false)
    };
    (has(&args[0], "top") && has(&args[1], "bottom"))
        || (has(&args[0], "bottom") && has(&args[1], "top"))
}

fn is_denom_positive_body(body: &Spanned<Expr>, f_short: &str) -> bool {
    strict_field_pos_call(body, "bottom")
        .map(|(f, _)| same_short(&f, f_short))
        .unwrap_or(false)
}

fn find_joint_positivity_law(
    vb: &VerifyBlock,
    ctx: &CodegenContext,
    f_short: &str,
) -> Option<PoolCite> {
    for prev in sibling_blocks(vb, ctx) {
        if prev.line == vb.line && prev.fn_name == vb.fn_name {
            break;
        }
        let VerifyKind::Law(prev_law) = &prev.kind else {
            continue;
        };
        if prev_law.when.is_some()
            || !matches!(&prev_law.rhs.node, Expr::Literal(Literal::Bool(true)))
        {
            continue;
        }
        let Some(body) = subject_body(&prev.fn_name, ctx) else {
            continue;
        };
        if !is_joint_positivity_body(body, f_short) {
            continue;
        }
        return Some(PoolCite {
            thm: law_theorem(prev, prev_law, ctx)?,
            subject: aver_name_to_lean(&prev.fn_name),
        });
    }
    None
}

struct DenomPositive {
    arg: Spanned<Expr>,
    subject: String,
    pos: PoolCite,
}

fn recognize_denom_positive_shape(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Option<DenomPositive> {
    if law.when.is_some() || !matches!(law.rhs.node, Expr::Literal(Literal::Bool(true))) {
        return None;
    }
    let Expr::FnCall(callee, call_args) = &law.lhs.node else {
        return None;
    };
    let subject_src = expr_dotted_name(callee)?;
    let subject_fd = find_fn_def_by_call_name(ctx, &subject_src)?;
    if subject_fd.return_type.trim() != "Bool"
        || subject_fd.params.len() != call_args.len()
        || !subject_fd.effects.is_empty()
    {
        return None;
    }
    let body = subject_body(&subject_src, ctx)?;
    let (f_dotted, body_arg) = strict_field_pos_call(body, "bottom")?;
    let f_fd = find_fn_def_by_call_name(ctx, &f_dotted)?;
    if !is_unary_fraction_fn(f_fd) {
        return None;
    }
    let f_short = f_dotted.rsplit('.').next().unwrap_or(&f_dotted).to_string();
    let mut map = std::collections::HashMap::new();
    for ((pname, _), arg) in subject_fd.params.iter().zip(call_args.iter()) {
        map.insert(pname.as_str(), arg);
    }
    let arg = substitute_expr(&body_arg, &map);
    let pos = find_joint_positivity_law(vb, ctx, &f_short)?;
    Some(DenomPositive {
        arg,
        subject: aver_name_to_lean(&subject_src),
        pos,
    })
}

pub(in crate::codegen::lean) fn recognize_denom_positive(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> bool {
    recognize_denom_positive_shape(vb, law, ctx).is_some()
}

pub(super) fn emit_denom_positive_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    theorem_base: &str,
    quant_params: &str,
) -> Option<AutoProof> {
    let shape = recognize_denom_positive_shape(vb, law, ctx)?;
    let render = |e: &Spanned<Expr>| super::super::expr::emit_expr_legacy(e, ctx, None);
    let lhs = render(&law.lhs);
    let rhs = render(&law.rhs);
    let arg = render(&shape.arg);
    let intros: Vec<String> = law
        .givens
        .iter()
        .map(|g| aver_name_to_lean(&g.name))
        .collect();
    let intro_line = if intros.is_empty() {
        String::new()
    } else {
        format!("  intro {}\n", intros.join(" "))
    };
    let text = format!(
        r#"theorem {base} : ∀ {quant}, {lhs} = {rhs} := by
{intro_line}  first
  | (have hpos := {pos_thm} ({arg})
     simp only [{pos_subject}, Bool.and_eq_true, decide_eq_true_eq, gt_iff_lt] at hpos
     simp only [{subject}, decide_eq_true_eq, gt_iff_lt]
     exact hpos.2)
  | sorry"#,
        base = theorem_base,
        quant = quant_params,
        pos_thm = shape.pos.thm,
        pos_subject = shape.pos.subject,
        subject = shape.subject,
    );
    Some(AutoProof {
        support_lines: text.lines().map(|l| l.to_string()).collect(),
        body: crate::codegen::lean::tactic_ir::Tactic::raw(Vec::new()),
        replaces_theorem: true,
    })
}

struct RatOps {
    less_than: String,
    isnonneg: String,
    minus: String,
}

#[derive(Clone, Copy)]
enum MonoArgRole {
    Small,
    Big,
}

struct MonotoneCite {
    thm: String,
    subject: String,
    arg_roles: Vec<MonoArgRole>,
}

fn rat_ops_from_mono_body(body: &Spanned<Expr>, less_than_dotted: &str) -> Option<RatOps> {
    let (isnonneg, nn_args) = call_named(body, "isNonNeg", 1)?;
    let (minus, _) = call_named(&nn_args[0], "minus", 2)?;
    Some(RatOps {
        less_than: aver_name_to_lean(less_than_dotted),
        isnonneg: aver_name_to_lean(&isnonneg),
        minus: aver_name_to_lean(&minus),
    })
}

fn monotone_law_shape(
    prev: &VerifyBlock,
    prev_law: &VerifyLaw,
    body: &Spanned<Expr>,
    ctx: &CodegenContext,
    f_short: &str,
    less_than_dotted: &str,
) -> Option<(MonotoneCite, RatOps)> {
    let when = prev_law.when.as_ref()?;
    if !matches!(&prev_law.rhs.node, Expr::Literal(Literal::Bool(true))) {
        return None;
    }
    let ops = rat_ops_from_mono_body(body, less_than_dotted)?;
    let (_, nn_args) = call_named(body, "isNonNeg", 1)?;
    let (_, m_args) = call_named(&nn_args[0], "minus", 2)?;
    let (f_big, big_args) = as_call(&m_args[0])?;
    let (f_small, small_args) = as_call(&m_args[1])?;
    if f_big != f_short || f_small != f_short || big_args.len() != 1 || small_args.len() != 1 {
        return None;
    }
    let Expr::FnCall(callee, call_args) = &prev_law.lhs.node else {
        return None;
    };
    let subject_src = expr_dotted_name(callee)?;
    let subject_fd = find_fn_def_by_call_name(ctx, &subject_src)?;
    if subject_fd.return_type.trim() != "Bool"
        || subject_fd.params.len() != call_args.len()
        || !subject_fd.effects.is_empty()
    {
        return None;
    }
    let mut map = std::collections::HashMap::new();
    for ((pname, _), arg) in subject_fd.params.iter().zip(call_args.iter()) {
        map.insert(pname.as_str(), arg);
    }
    let big = substitute_expr(&big_args[0], &map);
    let small = substitute_expr(&small_args[0], &map);
    let render = |e: &Spanned<Expr>| super::super::expr::emit_expr_legacy(e, ctx, None);
    let small_lean = render(&small);
    let big_lean = render(&big);
    let premise_ok = match &when.node {
        Expr::BinOp(BinOp::Lte, l, r) => render(l) == small_lean && render(r) == big_lean,
        Expr::BinOp(BinOp::Gte, l, r) => render(l) == big_lean && render(r) == small_lean,
        _ => false,
    };
    if !premise_ok {
        return None;
    }
    let mut arg_roles = Vec::new();
    for given in &prev_law.givens {
        let given_expr = Spanned::bare(Expr::Ident(given.name.clone()));
        let given_lean = render(&given_expr);
        if given_lean == small_lean {
            arg_roles.push(MonoArgRole::Small);
        } else if given_lean == big_lean {
            arg_roles.push(MonoArgRole::Big);
        } else {
            return None;
        }
    }
    if !arg_roles.iter().any(|r| matches!(r, MonoArgRole::Small))
        || !arg_roles.iter().any(|r| matches!(r, MonoArgRole::Big))
    {
        return None;
    }
    Some((
        MonotoneCite {
            thm: law_theorem(prev, prev_law, ctx)?,
            subject: aver_name_to_lean(&prev.fn_name),
            arg_roles,
        },
        ops,
    ))
}

fn find_monotone_law(
    vb: &VerifyBlock,
    ctx: &CodegenContext,
    f_short: &str,
    less_than_dotted: &str,
) -> Option<(MonotoneCite, RatOps)> {
    for prev in sibling_blocks(vb, ctx) {
        if prev.line == vb.line && prev.fn_name == vb.fn_name {
            break;
        }
        let VerifyKind::Law(prev_law) = &prev.kind else {
            continue;
        };
        let Some(body) = subject_body(&prev.fn_name, ctx) else {
            continue;
        };
        if let Some(found) =
            monotone_law_shape(prev, prev_law, body, ctx, f_short, less_than_dotted)
        {
            return Some(found);
        }
    }
    None
}

fn find_denom_law(vb: &VerifyBlock, ctx: &CodegenContext, f_short: &str) -> Option<PoolCite> {
    for prev in sibling_blocks(vb, ctx) {
        if prev.line == vb.line && prev.fn_name == vb.fn_name {
            break;
        }
        let VerifyKind::Law(prev_law) = &prev.kind else {
            continue;
        };
        if prev_law.when.is_some()
            || !matches!(&prev_law.rhs.node, Expr::Literal(Literal::Bool(true)))
        {
            continue;
        }
        let Some(body) = subject_body(&prev.fn_name, ctx) else {
            continue;
        };
        if !is_denom_positive_body(body, f_short) {
            continue;
        }
        return Some(PoolCite {
            thm: law_theorem(prev, prev_law, ctx)?,
            subject: aver_name_to_lean(&prev.fn_name),
        });
    }
    None
}

struct Reflect {
    f: String,
    left: Spanned<Expr>,
    right: Spanned<Expr>,
    subject: String,
    ops: RatOps,
    mono: MonotoneCite,
    denom: PoolCite,
}

struct BridgeCite {
    thm: String,
    subject: String,
    dep_key: (String, String),
}

enum MagnitudeBracketSource {
    Bridge(BridgeCite),
    Reflect { reflect: PoolCite, denom: PoolCite },
}

struct MagnitudeBracket {
    f: String,
    x: Spanned<Expr>,
    e: Spanned<Expr>,
    m: Spanned<Expr>,
    leaf_count: usize,
    pos_idx: usize,
    lower_idx: usize,
    upper_idx: usize,
    subject: String,
    ops: RatOps,
    source: MagnitudeBracketSource,
}

struct LowerBoundExpr {
    isnonneg: String,
    minus: String,
    x: Spanned<Expr>,
    f: String,
    arg: Spanned<Expr>,
}

struct UpperBoundExpr {
    less_than: String,
    x: Spanned<Expr>,
    f: String,
    arg: Spanned<Expr>,
}

fn positive_bottom_expr(expr: &Spanned<Expr>) -> Option<Spanned<Expr>> {
    let Expr::BinOp(op, l, r) = &expr.node else {
        return None;
    };
    let (attr_side, lit_side) = match op {
        BinOp::Gt => (l, r),
        BinOp::Lt => (r, l),
        _ => return None,
    };
    if !matches!(&lit_side.node, Expr::Literal(Literal::Int(0))) {
        return None;
    }
    let Expr::Attr(base, field) = &attr_side.node else {
        return None;
    };
    (field == "bottom").then(|| (**base).clone())
}

fn minus_lower_bound_expr(expr: &Spanned<Expr>) -> Option<LowerBoundExpr> {
    let (isnonneg, nn_args) = call_named(expr, "isNonNeg", 1)?;
    let (minus, m_args) = call_named(&nn_args[0], "minus", 2)?;
    let x = m_args[0].clone();
    let Expr::FnCall(f_callee, f_args) = &m_args[1].node else {
        return None;
    };
    if f_args.len() != 1 {
        return None;
    }
    let f = expr_dotted_name(f_callee)?;
    Some(LowerBoundExpr {
        isnonneg,
        minus,
        x,
        f,
        arg: f_args[0].clone(),
    })
}

fn strict_upper_bound_expr(expr: &Spanned<Expr>) -> Option<UpperBoundExpr> {
    let (less_than, lt_args) = call_named(expr, "lessThan", 2)?;
    let x = lt_args[0].clone();
    let Expr::FnCall(f_callee, f_args) = &lt_args[1].node else {
        return None;
    };
    if f_args.len() != 1 {
        return None;
    }
    let f = expr_dotted_name(f_callee)?;
    Some(UpperBoundExpr {
        less_than,
        x,
        f,
        arg: f_args[0].clone(),
    })
}

fn reflect_law_shape(
    prev: &VerifyBlock,
    prev_law: &VerifyLaw,
    body: &Spanned<Expr>,
    ctx: &CodegenContext,
    f_short: &str,
    less_than_dotted: &str,
) -> Option<PoolCite> {
    let when = prev_law.when.as_ref()?;
    if !matches!(&prev_law.rhs.node, Expr::Literal(Literal::Bool(true))) {
        return None;
    }
    let Expr::BinOp(BinOp::Lt, body_left, body_right) = &body.node else {
        return None;
    };
    let Expr::FnCall(callee, call_args) = &prev_law.lhs.node else {
        return None;
    };
    let subject_src = expr_dotted_name(callee)?;
    let subject_fd = find_fn_def_by_call_name(ctx, &subject_src)?;
    if subject_fd.return_type.trim() != "Bool"
        || subject_fd.params.len() != call_args.len()
        || !subject_fd.effects.is_empty()
    {
        return None;
    }
    let mut map = std::collections::HashMap::new();
    for ((pname, _), arg) in subject_fd.params.iter().zip(call_args.iter()) {
        map.insert(pname.as_str(), arg);
    }
    let left = substitute_expr(body_left, &map);
    let right = substitute_expr(body_right, &map);
    let (when_less_than, lt_args) = call_named(when, "lessThan", 2)?;
    if when_less_than != less_than_dotted {
        return None;
    }
    let Expr::FnCall(left_callee, left_args) = &lt_args[0].node else {
        return None;
    };
    let Expr::FnCall(right_callee, right_args) = &lt_args[1].node else {
        return None;
    };
    if left_args.len() != 1 || right_args.len() != 1 {
        return None;
    }
    let f_dotted = expr_dotted_name(left_callee)?;
    if expr_dotted_name(right_callee)? != f_dotted || !same_short(&f_dotted, f_short) {
        return None;
    }
    if !same_expr(&left_args[0], &left, ctx) || !same_expr(&right_args[0], &right, ctx) {
        return None;
    }
    Some(PoolCite {
        thm: law_theorem(prev, prev_law, ctx)?,
        subject: aver_name_to_lean(&prev.fn_name),
    })
}

fn find_reflect_law(
    vb: &VerifyBlock,
    ctx: &CodegenContext,
    f_short: &str,
    less_than_dotted: &str,
) -> Option<PoolCite> {
    for prev in sibling_blocks(vb, ctx) {
        if prev.line == vb.line && prev.fn_name == vb.fn_name {
            break;
        }
        let VerifyKind::Law(prev_law) = &prev.kind else {
            continue;
        };
        let Some(body) = subject_body(&prev.fn_name, ctx) else {
            continue;
        };
        if let Some(found) = reflect_law_shape(prev, prev_law, body, ctx, f_short, less_than_dotted)
        {
            return Some(found);
        }
    }
    None
}

fn magnitude_bridge_law_shape(
    prev: &VerifyBlock,
    prev_law: &VerifyLaw,
    body: &Spanned<Expr>,
    ctx: &CodegenContext,
    f_short: &str,
) -> Option<PoolCite> {
    let when = prev_law.when.as_ref()?;
    if !matches!(&prev_law.rhs.node, Expr::Literal(Literal::Bool(true))) {
        return None;
    }
    let Expr::BinOp(BinOp::Lte, body_left, body_right) = &body.node else {
        return None;
    };
    let Expr::BinOp(BinOp::Sub, body_m, one_body) = &body_right.node else {
        return None;
    };
    if !matches!(&one_body.node, Expr::Literal(Literal::Int(1))) {
        return None;
    }
    let Expr::FnCall(callee, call_args) = &prev_law.lhs.node else {
        return None;
    };
    let subject_src = expr_dotted_name(callee)?;
    let subject_fd = find_fn_def_by_call_name(ctx, &subject_src)?;
    if subject_fd.return_type.trim() != "Bool"
        || subject_fd.params.len() != call_args.len()
        || !subject_fd.effects.is_empty()
    {
        return None;
    }
    let mut map = std::collections::HashMap::new();
    for ((pname, _), arg) in subject_fd.params.iter().zip(call_args.iter()) {
        map.insert(pname.as_str(), arg);
    }
    let e = substitute_expr(body_left, &map);
    let m = substitute_expr(body_m, &map);

    let mut conjs = Vec::new();
    flatten_bool_and(when, &mut conjs);
    let mut x_pos: Option<Spanned<Expr>> = None;
    let mut lower: Option<LowerBoundExpr> = None;
    let mut upper: Option<UpperBoundExpr> = None;
    for conj in conjs {
        if x_pos.is_none() {
            x_pos = positive_bottom_expr(conj);
        }
        if lower.is_none() {
            lower = minus_lower_bound_expr(conj);
        }
        if upper.is_none() {
            upper = strict_upper_bound_expr(conj);
        }
    }
    let x_pos = x_pos?;
    let lower = lower?;
    let upper = upper?;
    if !same_short(&lower.f, f_short) || !same_short(&upper.f, f_short) || lower.f != upper.f {
        return None;
    }
    if !same_expr(&x_pos, &lower.x, ctx) || !same_expr(&x_pos, &upper.x, ctx) {
        return None;
    }
    if !same_expr(&lower.arg, &e, ctx) || !same_expr(&upper.arg, &m, ctx) {
        return None;
    }
    Some(PoolCite {
        thm: law_theorem(prev, prev_law, ctx)?,
        subject: aver_name_to_lean(&prev.fn_name),
    })
}

fn dep_module_for_dotted_fn<'a>(
    f_dotted: &str,
    ctx: &'a CodegenContext,
) -> Option<(&'a crate::codegen::ModuleInfo, String)> {
    let (prefix, short) = f_dotted.rsplit_once('.')?;
    ctx.modules
        .iter()
        .find(|module| module.prefix == prefix)
        .map(|module| (module, short.to_string()))
}

fn find_dep_magnitude_bridge_law(
    vb: &VerifyBlock,
    ctx: &CodegenContext,
    f_dotted: &str,
) -> Option<BridgeCite> {
    let (module, f_short) = dep_module_for_dotted_fn(f_dotted, ctx)?;
    ctx.with_module_scope(Some(module.prefix.as_str()), || {
        for prev in &module.verify_laws {
            if prev.line == vb.line && prev.fn_name == vb.fn_name {
                continue;
            }
            let VerifyKind::Law(prev_law) = &prev.kind else {
                continue;
            };
            let Some(body) = subject_body(&prev.fn_name, ctx) else {
                continue;
            };
            let Some(cite) = magnitude_bridge_law_shape(prev, prev_law, body, ctx, &f_short) else {
                continue;
            };
            let base = cite.thm;
            let thm = format!("{}.{}", module.prefix, base);
            return Some(BridgeCite {
                thm,
                subject: format!("{}.{}", module.prefix, cite.subject),
                dep_key: (module.prefix.clone(), base),
            });
        }
        None
    })
}

fn recognize_reflect_shape(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Option<Reflect> {
    let when = law.when.as_ref()?;
    if !matches!(law.rhs.node, Expr::Literal(Literal::Bool(true))) {
        return None;
    }
    let Expr::FnCall(subject_callee, subject_args) = &law.lhs.node else {
        return None;
    };
    let subject_src = expr_dotted_name(subject_callee)?;
    let subject_fd = find_fn_def_by_call_name(ctx, &subject_src)?;
    if subject_fd.return_type.trim() != "Bool"
        || subject_fd.params.len() != subject_args.len()
        || !subject_fd.effects.is_empty()
    {
        return None;
    }
    let subject_body = subject_body(&subject_src, ctx)?;
    let Expr::BinOp(BinOp::Lt, body_left, body_right) = &subject_body.node else {
        return None;
    };
    let mut map = std::collections::HashMap::new();
    for ((pname, _), arg) in subject_fd.params.iter().zip(subject_args.iter()) {
        map.insert(pname.as_str(), arg);
    }
    let left = substitute_expr(body_left, &map);
    let right = substitute_expr(body_right, &map);

    let (less_than_dotted, lt_args) = call_named(when, "lessThan", 2)?;
    let Expr::FnCall(left_callee, left_args) = &lt_args[0].node else {
        return None;
    };
    let Expr::FnCall(right_callee, right_args) = &lt_args[1].node else {
        return None;
    };
    if left_args.len() != 1 || right_args.len() != 1 {
        return None;
    }
    let f_dotted = expr_dotted_name(left_callee)?;
    if expr_dotted_name(right_callee)? != f_dotted {
        return None;
    }
    let f_fd = find_fn_def_by_call_name(ctx, &f_dotted)?;
    if !is_unary_fraction_fn(f_fd) {
        return None;
    }
    let render = |e: &Spanned<Expr>| super::super::expr::emit_expr_legacy(e, ctx, None);
    if render(&left_args[0]) != render(&left) || render(&right_args[0]) != render(&right) {
        return None;
    }
    let f_short = f_dotted.rsplit('.').next().unwrap_or(&f_dotted).to_string();
    let (mono, ops) = find_monotone_law(vb, ctx, &f_short, &less_than_dotted)?;
    let denom = find_denom_law(vb, ctx, &f_short)?;
    Some(Reflect {
        f: aver_name_to_lean(&f_dotted),
        left,
        right,
        subject: aver_name_to_lean(&subject_src),
        ops,
        mono,
        denom,
    })
}

fn recognize_magnitude_bracket_shape(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Option<MagnitudeBracket> {
    let when = law.when.as_ref()?;
    if !matches!(law.rhs.node, Expr::Literal(Literal::Bool(true))) {
        return None;
    }
    let Expr::FnCall(subject_callee, subject_args) = &law.lhs.node else {
        return None;
    };
    let subject_src = expr_dotted_name(subject_callee)?;
    let subject_fd = find_fn_def_by_call_name(ctx, &subject_src)?;
    if subject_fd.return_type.trim() != "Bool"
        || subject_fd.params.len() != subject_args.len()
        || !subject_fd.effects.is_empty()
    {
        return None;
    }
    let mut map = std::collections::HashMap::new();
    for ((pname, _), arg) in subject_fd.params.iter().zip(subject_args.iter()) {
        map.insert(pname.as_str(), arg);
    }
    let subject_body = subject_body(&subject_src, ctx)?;
    let claim_body = substitute_expr(subject_body, &map);
    let allowed_ints: std::collections::BTreeSet<String> = law
        .givens
        .iter()
        .filter(|g| g.type_name.trim() == "Int")
        .map(|g| g.name.clone())
        .collect();
    if !is_linear_int_comparison(&claim_body, &allowed_ints) {
        return None;
    }

    let mut conjs = Vec::new();
    flatten_bool_and(when, &mut conjs);
    let mut x_pos: Option<(usize, Spanned<Expr>)> = None;
    let mut lower: Option<(usize, LowerBoundExpr)> = None;
    let mut upper: Option<(usize, UpperBoundExpr)> = None;
    for (idx, conj) in conjs.iter().enumerate() {
        if x_pos.is_none() {
            x_pos = positive_bottom_expr(conj).map(|x| (idx, x));
        }
        if lower.is_none() {
            lower = minus_lower_bound_expr(conj).map(|bound| (idx, bound));
        }
        if upper.is_none() {
            upper = strict_upper_bound_expr(conj).map(|bound| (idx, bound));
        }
    }
    let (pos_idx, x_pos) = x_pos?;
    let (lower_idx, lower) = lower?;
    let (upper_idx, upper) = upper?;
    if conjs.len() < 3 || pos_idx == lower_idx || pos_idx == upper_idx || lower_idx == upper_idx {
        return None;
    }
    if lower.f != upper.f {
        return None;
    }
    let f_fd = find_fn_def_by_call_name(ctx, &lower.f)?;
    if !is_unary_fraction_fn(f_fd) {
        return None;
    }
    if !same_expr(&x_pos, &lower.x, ctx) || !same_expr(&x_pos, &upper.x, ctx) {
        return None;
    }
    let f_short = lower.f.rsplit('.').next().unwrap_or(&lower.f).to_string();
    let e = lower.arg.clone();
    let m = upper.arg.clone();
    let source = if let Some(bridge) = find_dep_magnitude_bridge_law(vb, ctx, &lower.f) {
        MagnitudeBracketSource::Bridge(bridge)
    } else {
        let denom = find_denom_law(vb, ctx, &f_short)?;
        let reflect = find_reflect_law(vb, ctx, &f_short, &upper.less_than)?;
        MagnitudeBracketSource::Reflect { reflect, denom }
    };
    Some(MagnitudeBracket {
        f: aver_name_to_lean(&lower.f),
        x: x_pos,
        e,
        m,
        leaf_count: conjs.len(),
        pos_idx,
        lower_idx,
        upper_idx,
        subject: aver_name_to_lean(&subject_src),
        ops: RatOps {
            less_than: aver_name_to_lean(&upper.less_than),
            isnonneg: aver_name_to_lean(&lower.isnonneg),
            minus: aver_name_to_lean(&lower.minus),
        },
        source,
    })
}

pub(in crate::codegen::lean) fn recognize_monotone_reflect(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> bool {
    recognize_reflect_shape(vb, law, ctx).is_some()
}

pub(in crate::codegen::lean) fn recognize_magnitude_bracket_reflect(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> bool {
    recognize_magnitude_bracket_shape(vb, law, ctx).is_some()
}

fn render_reflect_kit(base: &str) -> String {
    let p = format!("{base}__");
    format!(
        r#"theorem {p}nonneg_of_mul_nonneg_right (a b : Int) (hb : 0 < b) (h : 0 ≤ a * b) : 0 ≤ a := by
  rcases Int.lt_or_le a 0 with ha | ha
  · exfalso
    have hlt : a * b < 0 * b := Int.mul_lt_mul_of_pos_right ha hb
    simp only [Int.zero_mul] at hlt
    omega
  · exact ha
theorem {p}cross_order_contra (pt pb qt qb : Int) (hpb : 0 < pb) (hqb : 0 < qb)
    (hge : 0 ≤ (pt * qb - qt * pb) * (pb * qb))
    (hlt : (pt * pb) * (qb * qb) < (qt * qb) * (pb * pb)) : False := by
  have hD : 0 < pb * qb := Int.mul_pos hpb hqb
  have hN : 0 ≤ pt * qb - qt * pb := {p}nonneg_of_mul_nonneg_right _ _ hD hge
  have e1 : (pt * pb) * (qb * qb) = (pt * qb) * (pb * qb) := by
    simp only [Int.mul_comm, Int.mul_left_comm]
  have e2 : (qt * qb) * (pb * pb) = (qt * pb) * (pb * qb) := by
    simp only [Int.mul_comm, Int.mul_left_comm]
  rw [e1, e2] at hlt
  have := Int.lt_of_mul_lt_mul_right hlt (Int.le_of_lt hD)
  omega"#
    )
}

fn render_magnitude_kit(base: &str) -> String {
    let p = format!("{base}__");
    format!(
        r#"theorem {p}nonneg_of_mul_nonneg_right (a b : Int) (hb : 0 < b) (h : 0 ≤ a * b) : 0 ≤ a := by
  rcases Int.lt_or_le a 0 with ha | ha
  · exfalso
    have hlt : a * b < 0 * b := Int.mul_lt_mul_of_pos_right ha hb
    simp only [Int.zero_mul] at hlt
    omega
  · exact ha
theorem {p}cross_lt_trans (xt xb et eb mt mb : Int)
    (hxb : 0 < xb) (heb : 0 < eb) (hmb : 0 < mb)
    (hlo : 0 ≤ (xt * eb - et * xb) * (xb * eb))
    (hhi : (xt * xb) * (mb * mb) < (mt * mb) * (xb * xb)) :
    (et * eb) * (mb * mb) < (mt * mb) * (eb * eb) := by
  have hN : 0 ≤ xt * eb - et * xb :=
    {p}nonneg_of_mul_nonneg_right _ _ (Int.mul_pos hxb heb) hlo
  have hN' : et * xb ≤ xt * eb := by omega
  have h1 : (xt * xb) * (mb * mb) = (xt * mb) * (xb * mb) := by
    simp only [Int.mul_comm, Int.mul_left_comm]
  have h2 : (mt * mb) * (xb * xb) = (mt * xb) * (xb * mb) := by
    simp only [Int.mul_comm, Int.mul_left_comm]
  rw [h1, h2] at hhi
  have hH : xt * mb < mt * xb :=
    Int.lt_of_mul_lt_mul_right hhi (Int.le_of_lt (Int.mul_pos hxb hmb))
  have c1 : (et * mb) * xb = (et * xb) * mb := by
    simp only [Int.mul_comm, Int.mul_left_comm]
  have c2 : (et * xb) * mb ≤ (xt * eb) * mb := Int.mul_le_mul_of_nonneg_right hN' (Int.le_of_lt hmb)
  have c3 : (xt * eb) * mb = (xt * mb) * eb := by
    simp only [Int.mul_comm, Int.mul_left_comm]
  have c4 : (xt * mb) * eb < (mt * xb) * eb := Int.mul_lt_mul_of_pos_right hH heb
  have c5 : (mt * xb) * eb = (mt * eb) * xb := by
    simp only [Int.mul_comm, Int.mul_left_comm]
  have hchain : (et * mb) * xb < (mt * eb) * xb := by
    rw [c1]
    calc (et * xb) * mb ≤ (xt * eb) * mb := c2
      _ = (xt * mb) * eb := c3
      _ < (mt * xb) * eb := c4
      _ = (mt * eb) * xb := c5
  have hcancel : et * mb < mt * eb := Int.lt_of_mul_lt_mul_right hchain (Int.le_of_lt hxb)
  have g1 : (et * eb) * (mb * mb) = (et * mb) * (eb * mb) := by
    simp only [Int.mul_comm, Int.mul_left_comm]
  have g2 : (mt * mb) * (eb * eb) = (mt * eb) * (eb * mb) := by
    simp only [Int.mul_comm, Int.mul_left_comm]
  rw [g1, g2]
  exact Int.mul_lt_mul_of_pos_right hcancel (Int.mul_pos heb hmb)"#
    )
}

fn left_fold_conj_pattern(names: &[String]) -> Option<String> {
    let mut it = names.iter();
    let mut pat = it.next()?.clone();
    for name in it {
        pat = format!("⟨{pat}, {name}⟩");
    }
    Some(pat)
}

pub(super) fn emit_monotone_reflect_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    theorem_base: &str,
    quant_params: &str,
) -> Option<AutoProof> {
    let shape = recognize_reflect_shape(vb, law, ctx)?;
    let render = |e: &Spanned<Expr>| super::super::expr::emit_expr_legacy(e, ctx, None);
    let lhs = render(&law.lhs);
    let rhs = render(&law.rhs);
    let when = render(law.when.as_ref()?);
    let left = render(&shape.left);
    let right = render(&shape.right);
    let intros: Vec<String> = law
        .givens
        .iter()
        .map(|g| aver_name_to_lean(&g.name))
        .collect();
    let mono_args: Vec<String> = shape
        .mono
        .arg_roles
        .iter()
        .map(|role| match role {
            MonoArgRole::Small => format!("({right})"),
            MonoArgRole::Big => format!("({left})"),
        })
        .collect();
    let mono_args = mono_args.join(" ");
    let RatOps {
        less_than,
        isnonneg,
        minus,
    } = &shape.ops;
    let p = format!("{theorem_base}__");
    let kit = render_reflect_kit(theorem_base);
    let assembly = format!(
        r#"theorem {base} : ∀ {quant}, {when} = true -> {lhs} = {rhs} := by
  intro {intros} h_less
  first
  | (rcases Int.lt_or_le ({left}) ({right}) with hlt | hge
     · simp only [{subject}, decide_eq_true_eq]
       exact hlt
     · exfalso
       have hmono := {mono_thm} {mono_args} (by simp only [eq_iff_iff, iff_true]; exact hge)
       simp only [{mono_subject}, {isnonneg}, {minus}, decide_eq_true_eq, ge_iff_le] at hmono
       simp only [{less_than}, decide_eq_true_eq] at h_less
       have hden_left := {denom_thm} ({left})
       have hden_right := {denom_thm} ({right})
       simp only [{denom_subject}, decide_eq_true_eq, gt_iff_lt] at hden_left hden_right
       exact {p}cross_order_contra ({f} ({left})).top ({f} ({left})).bottom
         ({f} ({right})).top ({f} ({right})).bottom
         hden_left hden_right hmono h_less)
  | sorry"#,
        base = theorem_base,
        quant = quant_params,
        intros = intros.join(" "),
        subject = shape.subject,
        mono_thm = shape.mono.thm,
        mono_subject = shape.mono.subject,
        denom_thm = shape.denom.thm,
        denom_subject = shape.denom.subject,
        f = shape.f,
    );
    let text = format!("{kit}\n{assembly}");
    Some(AutoProof {
        support_lines: text.lines().map(|l| l.to_string()).collect(),
        body: crate::codegen::lean::tactic_ir::Tactic::raw(Vec::new()),
        replaces_theorem: true,
    })
}

pub(super) fn emit_magnitude_bracket_reflect_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    theorem_base: &str,
    quant_params: &str,
) -> Option<AutoProof> {
    let shape = recognize_magnitude_bracket_shape(vb, law, ctx)?;
    let render = |e: &Spanned<Expr>| super::super::expr::emit_expr_legacy(e, ctx, None);
    let lhs = render(&law.lhs);
    let rhs = render(&law.rhs);
    let when = render(law.when.as_ref()?);
    let x = render(&shape.x);
    let e = render(&shape.e);
    let m = render(&shape.m);
    let intros: Vec<String> = law
        .givens
        .iter()
        .map(|g| aver_name_to_lean(&g.name))
        .collect();
    let mut leaf_names: Vec<String> = (0..shape.leaf_count)
        .map(|i| format!("h_unused{i}"))
        .collect();
    leaf_names[shape.pos_idx] = "hxb".to_string();
    leaf_names[shape.lower_idx] = "hlo".to_string();
    leaf_names[shape.upper_idx] = "hhi".to_string();
    let when_pat = left_fold_conj_pattern(&leaf_names)?;
    let RatOps {
        less_than,
        isnonneg,
        minus,
    } = &shape.ops;
    let text = match &shape.source {
        MagnitudeBracketSource::Bridge(bridge) => format!(
            r#"theorem {base} : ∀ {quant}, {when} = true -> {lhs} = {rhs} := by
  intro {intros} h_when
  first
  | (have hbridge := {bridge_thm} ({x}) ({e}) ({m}) h_when
     have hbound : ({e}) <= ({m}) - 1 := by
       simpa only [{bridge_subject}, decide_eq_true_eq] using hbridge
     simp only [{subject}, decide_eq_true_eq, ge_iff_le]
     omega)
  | sorry"#,
            base = theorem_base,
            quant = quant_params,
            intros = intros.join(" "),
            bridge_thm = bridge.thm,
            bridge_subject = bridge.subject,
            subject = shape.subject,
        ),
        MagnitudeBracketSource::Reflect { reflect, denom } => {
            let p = format!("{theorem_base}__");
            let kit = render_magnitude_kit(theorem_base);
            let assembly = format!(
                r#"theorem {base} : ∀ {quant}, {when} = true -> {lhs} = {rhs} := by
  intro {intros} h_when
  first
  | (simp only [Bool.and_eq_true] at h_when
     obtain {when_pat} := h_when
     simp only [decide_eq_true_eq, gt_iff_lt] at hxb
     simp only [{isnonneg}, {minus}, decide_eq_true_eq, ge_iff_le] at hlo
     simp only [{less_than}, decide_eq_true_eq] at hhi
     have hden_e := {denom_thm} ({e})
     have hden_m := {denom_thm} ({m})
     simp only [{denom_subject}, decide_eq_true_eq, gt_iff_lt] at hden_e hden_m
     have hcross := {p}cross_lt_trans ({x}).top ({x}).bottom
       ({f} ({e})).top ({f} ({e})).bottom ({f} ({m})).top ({f} ({m})).bottom
       hxb hden_e hden_m hlo hhi
     have hlt : {less_than} ({f} ({e})) ({f} ({m})) = true := by
       simp only [{less_than}, decide_eq_true_eq]
       exact hcross
     have hreflect := {reflect_thm} ({e}) ({m}) hlt
     simp only [{reflect_subject}, decide_eq_true_eq] at hreflect
     have hbound : ({e}) <= ({m}) - 1 := by omega
     simp only [{subject}, decide_eq_true_eq, ge_iff_le]
     omega)
  | sorry"#,
                base = theorem_base,
                quant = quant_params,
                intros = intros.join(" "),
                subject = shape.subject,
                denom_thm = denom.thm,
                denom_subject = denom.subject,
                reflect_thm = reflect.thm,
                reflect_subject = reflect.subject,
                f = shape.f,
            );
            format!("{kit}\n{assembly}")
        }
    };
    Some(AutoProof {
        support_lines: text.lines().map(|l| l.to_string()).collect(),
        body: crate::codegen::lean::tactic_ir::Tactic::raw(Vec::new()),
        replaces_theorem: true,
    })
}

fn module_prefix_of(vb: &VerifyBlock, ctx: &CodegenContext) -> Option<String> {
    ctx.modules
        .iter()
        .find(|m| {
            m.verify_laws
                .iter()
                .any(|b| b.line == vb.line && b.fn_name == vb.fn_name)
        })
        .map(|m| m.prefix.clone())
}

pub(in crate::codegen::lean) fn monotone_reflect_cited_deps(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Vec<(String, String)> {
    let prefix = module_prefix_of(vb, ctx);
    let mut out = Vec::new();
    if let Some(shape) = recognize_denom_positive_shape(vb, law, ctx)
        && let Some(prefix) = &prefix
    {
        out.push((prefix.clone(), shape.pos.thm));
    }
    if let Some(shape) = recognize_reflect_shape(vb, law, ctx)
        && let Some(prefix) = &prefix
    {
        out.push((prefix.clone(), shape.mono.thm));
        out.push((prefix.clone(), shape.denom.thm));
    }
    if let Some(shape) = recognize_magnitude_bracket_shape(vb, law, ctx) {
        match shape.source {
            MagnitudeBracketSource::Bridge(bridge) => out.push(bridge.dep_key),
            MagnitudeBracketSource::Reflect { reflect, denom } => {
                if let Some(prefix) = &prefix {
                    out.push((prefix.clone(), reflect.thm));
                    out.push((prefix.clone(), denom.thm));
                }
            }
        }
    }
    out
}

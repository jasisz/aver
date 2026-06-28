//! Content-blind strict-order reflection for unary `Fraction` cones.
//!
//! Recognized shape:
//!
//!   * premise: `lessThan(F(a), F(b))`
//!   * claim: a Boolean subject whose body is `a < b`
//!
//! The carrier `F` is captured from the premise AST. The proof never unfolds
//! `F`; it cites earlier sibling pool laws for monotonicity and denominator
//! positivity of that same `F`, then closes the contradiction with a generic
//! integer cross-order kit.

use super::shared::{expr_dotted_name, find_fn_def_by_call_name, substitute_expr};
use super::{AutoProof, aver_name_to_lean};
use crate::ast::{BinOp, Expr, FnDef, Literal, Spanned, Stmt, VerifyBlock, VerifyKind, VerifyLaw};
use crate::codegen::CodegenContext;

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

pub(in crate::codegen::lean) fn recognize_monotone_reflect(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> bool {
    recognize_reflect_shape(vb, law, ctx).is_some()
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
    let Some(prefix) = module_prefix_of(vb, ctx) else {
        return Vec::new();
    };
    let mut out = Vec::new();
    if let Some(shape) = recognize_denom_positive_shape(vb, law, ctx) {
        out.push((prefix.clone(), shape.pos.thm));
    }
    if let Some(shape) = recognize_reflect_shape(vb, law, ctx) {
        out.push((prefix.clone(), shape.mono.thm));
        out.push((prefix, shape.denom.thm));
    }
    out
}

//! Lean renderer for the exact-rational order facts about a UNARY
//! `Fraction`-valued cone function `F` (the signed power of two in `projects/
//! k5_fdiv`, but the rung never inspects `F`'s body — it is name-blind and treats
//! `F` as opaque). Three sibling shapes, each closed by the laws-as-lemmas
//! composition rather than a per-figure template:
//!
//!  * POSITIVITY (`isPos`-style): a `holds` law whose subject body is
//!    `Bool.and(F(k).top > 0, F(k).bottom > 0)` — both numerator and denominator
//!    of `F(k)` strictly positive for any `k`. Closed by unfolding the
//!    (non-recursive) `F` and CITING the recursive-positivity pool law of the
//!    unique recursive integer fn in `F`'s cone (`1 <= g n` by `g.induct`).
//!
//!  * AT-LEAST-ONE (`>= 1`): a conditional `holds` law `when k >= 0 ->
//!    isNonNeg(minus(F(k), oneFraction))` — `F(k) >= 1` for a nonnegative
//!    exponent. The premise kills the negative arm; closed the same way, citing
//!    the recursive-positivity pool law.
//!
//!  * MONOTONICITY (the headline composition): a conditional `holds` law whose
//!    subject body is `isNonNeg(minus(F(HI), F(LO)))` under a premise
//!    `LO <= HI` — `F(LO) <= F(HI)`. Closed by CITING three earlier sibling pool
//!    laws about the SAME `F` — its homomorphism `sameValue(F(a+b), times(F(a),
//!    F(b)))`, its positivity, and its `>= 1` fact — and chaining them through
//!    the generic exact-rational order kit (`frac_le_mul_pos` scales by the
//!    positive `F(LO)`; `frac_le_samevalue_right` transports along the
//!    homomorphism). `F` is OPAQUE: no sign-split, no `unfold F`, no recursion —
//!    only the three cited laws and the generic Fraction kit. This is the generic
//!    analog of the integer `recursive_mono` monotonicity rung lifted to the
//!    sign-robust rational order, with the recursion fully abstracted behind the
//!    pool laws.
//!
//! Every emission is a self-contained TRUE-universal theorem (`replaces_theorem`)
//! wrapped `first | (…) | sorry`: a shape the recognizer admits but whose closing
//! slips falls to an honest caught `sorry`, so credit stays fail-closed behind
//! the `#print axioms` whitelist. The generic kit lemmas never mention the cited
//! laws, so only the assembly can fall through.

use super::AutoProof;
use super::aver_name_to_lean;
use super::shared::{self, expr_dotted_name, find_fn_def_by_call_name};
use crate::ast::{BinOp, Expr, FnDef, Literal, Spanned, Stmt, VerifyBlock, VerifyKind, VerifyLaw};
use crate::codegen::CodegenContext;

/// Deep-clone `e`, replacing any free `Ident`/`Resolved` named in `map`.
fn substitute(
    e: &Spanned<Expr>,
    map: &std::collections::HashMap<String, Spanned<Expr>>,
) -> Spanned<Expr> {
    let node = match &e.node {
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => {
            if let Some(rep) = map.get(n) {
                return rep.clone();
            }
            e.node.clone()
        }
        Expr::BinOp(op, a, b) => Expr::BinOp(
            *op,
            Box::new(substitute(a, map)),
            Box::new(substitute(b, map)),
        ),
        Expr::Neg(a) => Expr::Neg(Box::new(substitute(a, map))),
        Expr::Attr(b, f) => Expr::Attr(Box::new(substitute(b, map)), f.clone()),
        Expr::FnCall(c, args) => Expr::FnCall(
            Box::new(substitute(c, map)),
            args.iter().map(|x| substitute(x, map)).collect(),
        ),
        other => other.clone(),
    };
    Spanned::bare(node)
}

/// Whether `fd` is a unary `Int -> Fraction` function (the opaque carrier `F`).
/// **syntax-discovery-only**: the return annotation is source spelling — bare
/// `Fraction` inside the defining module, dep-qualified elsewhere. This only
/// gates whether the strategy attempts; the kernel judges every attempt.
fn is_unary_fraction_fn(fd: &FnDef) -> bool {
    matches!(fd.params.as_slice(), [(_, ty)] if ty.trim() == "Int")
        && fd.return_type.rsplit('.').next() == Some("Fraction")
}

/// The verify-law blocks of the SAME module as `vb`, in source order — the
/// pool of earlier siblings the composition cites. When `vb` is the ENTRY's
/// law it lives in `ctx.items`; when `vb` belongs to a DEPENDENCY module (the
/// cross-file export path, e.g. `Fprep` as a dep of `Remainder`) it lives in
/// that module's `verify_laws`. Searching both keeps the recognizer firing
/// identically in the standalone export and the dependency export, so a
/// cross-module citation of the monotonicity law still resolves to a universal
/// theorem.
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

/// The subject fn body (single tail expression) of a verify block's fn.
fn subject_body<'a>(fn_name: &str, ctx: &'a CodegenContext) -> Option<&'a Spanned<Expr>> {
    let fd = find_fn_def_by_call_name(ctx, fn_name)?;
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return None;
    };
    Some(body)
}

/// The Lean names of the four rational order primitives, derived from the
/// `Domain.Rational` prefix read off a qualified `minus`/`isNonNeg` call — never
/// hardcoded.
struct RatOps {
    isnonneg: String,
    minus: String,
    times: String,
    samevalue: String,
    onefraction: String,
}

/// Read the rational-op Lean names from the `isNonNeg`/`minus` heads of a
/// conclusion body `isNonNeg (minus …)`.
fn rat_ops_from_body(body: &Spanned<Expr>) -> Option<RatOps> {
    let (isnonneg, nn_args) = shared::call_named_with_dotted(body, "isNonNeg", 1)?;
    let (minus, _) = shared::call_named_with_dotted(&nn_args[0], "minus", 2)?;
    let prefix = minus.rsplit_once('.').map(|(p, _)| p.to_string())?;
    Some(RatOps {
        isnonneg: aver_name_to_lean(&isnonneg),
        minus: aver_name_to_lean(&minus),
        times: aver_name_to_lean(&format!("{prefix}.times")),
        samevalue: aver_name_to_lean(&format!("{prefix}.sameValue")),
        onefraction: aver_name_to_lean(&format!("{prefix}.oneFraction")),
    })
}

/// A discovered earlier-sibling pool law: its Lean theorem name and the Lean
/// name of its subject fn (to unfold the cited fact).
struct PoolCite {
    thm: String,
    subject: String,
}

/// Iterate the EARLIER sibling laws (source order is emit order) whose subject
/// fn is a single-tail fn, calling `pred` on `(prev, prev_law, subject_body)`;
/// the first `Some` wins. `pred` returns the law theorem name's subject match.
fn find_earlier_law<F>(vb: &VerifyBlock, ctx: &CodegenContext, mut pred: F) -> Option<PoolCite>
where
    F: FnMut(&VerifyBlock, &VerifyLaw, &Spanned<Expr>) -> bool,
{
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
        if !pred(prev, prev_law, body) {
            continue;
        }
        let Some((thm, _)) =
            crate::codegen::lean::toplevel::law_as_lemma_statement(prev, prev_law, ctx)
        else {
            continue;
        };
        return Some(PoolCite {
            thm,
            subject: aver_name_to_lean(&prev.fn_name),
        });
    }
    None
}

/// The unique recursive integer fn in `law`'s cone plus the earlier
/// recursive-positivity pool law about it (`g(x) >= LIT` `holds`). Name-blind:
/// `g` is whatever recursive fn the cone hides, the bound is any integer
/// literal. Returns `(g_lean, recpos_thm_name)`.
fn find_recursive_positivity(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Option<(String, String)> {
    let inputs = crate::codegen::proof_lower::ProofLowerInputs::from_ctx(ctx);
    let cone = crate::codegen::proof_lower::LawProofCone::compute(law, &vb.fn_name, &inputs);
    let cone_fns: std::collections::HashSet<String> =
        cone.pure_fns().iter().map(|fd| fd.name.clone()).collect();
    let recursive = crate::codegen::lean::recursive_pure_fn_names(ctx);

    for prev in sibling_blocks(vb, ctx) {
        if prev.line == vb.line && prev.fn_name == vb.fn_name {
            break;
        }
        let VerifyKind::Law(prev_law) = &prev.kind else {
            continue;
        };
        let short = prev.fn_name.rsplit('.').next().unwrap_or(&prev.fn_name);
        if !recursive.contains(short) || !cone_fns.contains(&prev.fn_name) {
            continue;
        }
        // The law CLAIM must be `g(arg) >= LIT` `holds` over the recursive fn `g`
        // (the claim is a bare comparison, not a `subject(args)` call).
        let Expr::BinOp(BinOp::Gte, l, r) = &prev_law.lhs.node else {
            continue;
        };
        if !matches!(&r.node, Expr::Literal(Literal::Int(_))) {
            continue;
        }
        let Some((callee, _)) = shared::short_call_name_args(l) else {
            continue;
        };
        if callee != short {
            continue;
        }
        if !matches!(&prev_law.rhs.node, Expr::Literal(Literal::Bool(true))) {
            continue;
        }
        let Some((thm, _)) =
            crate::codegen::lean::toplevel::law_as_lemma_statement(prev, prev_law, ctx)
        else {
            continue;
        };
        return Some((aver_name_to_lean(&prev.fn_name), thm));
    }
    None
}

// ===========================================================================
// POSITIVITY  —  Bool.and(F(k).top > 0, F(k).bottom > 0)
// ===========================================================================

/// The recognized positivity shape: the opaque `F`, the recursive inner fn and
/// its positivity pool law.
struct FracPositivity {
    sgn: String,
    g: String,
    recpos: String,
}

/// `0 < F(_).field` / `F(_).field > 0` over the fn `F` (dotted), returning that
/// dotted name. Accepts either operand order.
fn pos_comparison_fn(cmp: &Spanned<Expr>) -> Option<String> {
    let Expr::BinOp(op, l, r) = &cmp.node else {
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
    let Expr::Attr(base, _) = &attr_side.node else {
        return None;
    };
    let Expr::FnCall(callee, args) = &base.node else {
        return None;
    };
    if args.len() != 1 {
        return None;
    }
    expr_dotted_name(callee)
}

fn recognize_frac_positivity_shape(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Option<FracPositivity> {
    if law.when.is_some() {
        return None;
    }
    if !matches!(law.rhs.node, Expr::Literal(Literal::Bool(true))) {
        return None;
    }
    let Expr::FnCall(callee, call_args) = &law.lhs.node else {
        return None;
    };
    let subject_src = expr_dotted_name(callee)?;
    let subj_fd = find_fn_def_by_call_name(ctx, &subject_src)?;
    if subj_fd.return_type.trim() != "Bool"
        || subj_fd.params.len() != call_args.len()
        || !subj_fd.effects.is_empty()
    {
        return None;
    }
    let body = subject_body(&subject_src, ctx)?;
    // Body `Bool.and(C1, C2)` with each a strict positivity of the SAME `F`'s
    // numerator / denominator.
    let (_, and_args) = shared::call_named_with_dotted(body, "and", 2)?;
    let f1 = pos_comparison_fn(&and_args[0])?;
    let f2 = pos_comparison_fn(&and_args[1])?;
    if f1 != f2 {
        return None;
    }
    let sgn_fd = find_fn_def_by_call_name(ctx, &f1)?;
    if !is_unary_fraction_fn(sgn_fd) {
        return None;
    }
    let (g, recpos) = find_recursive_positivity(vb, law, ctx)?;
    Some(FracPositivity {
        sgn: aver_name_to_lean(&f1),
        g,
        recpos,
    })
}

pub(in crate::codegen::lean) fn recognize_frac_positivity(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> bool {
    recognize_frac_positivity_shape(vb, law, ctx).is_some()
}

pub(super) fn emit_frac_positivity_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    theorem_base: &str,
    quant_params: &str,
) -> Option<AutoProof> {
    let shape = recognize_frac_positivity_shape(vb, law, ctx)?;
    let render = |e: &Spanned<Expr>| super::super::expr::emit_expr_legacy(e, ctx, None);
    let lhs = render(&law.lhs);
    let rhs = render(&law.rhs);
    let subject = aver_name_to_lean(&expr_dotted_name(&{
        let Expr::FnCall(callee, _) = &law.lhs.node else {
            return None;
        };
        (**callee).clone()
    })?);
    let intros: Vec<String> = law
        .givens
        .iter()
        .map(|g| aver_name_to_lean(&g.name))
        .collect();
    let FracPositivity { sgn, g, recpos } = &shape;
    let text = format!(
        r#"theorem {base} : ∀ {quant}, {lhs} = {rhs} := by
  intro {intros}
  first
  | (have hpp : ∀ j : Int, 0 < {g} j := by
       intro j
       have h := {recpos} j
       simp only [eq_iff_iff, iff_true, ge_iff_le] at h
       omega
     simp only [{subject}, {sgn}, Bool.and_eq_true, decide_eq_true_eq, gt_iff_lt]
     grind)
  | sorry"#,
        base = theorem_base,
        quant = quant_params,
        intros = intros.join(" "),
    );
    Some(AutoProof {
        support_lines: text.lines().map(|l| l.to_string()).collect(),
        body: crate::codegen::lean::tactic_ir::Tactic::raw(Vec::new()),
        replaces_theorem: true,
    })
}

// ===========================================================================
// AT-LEAST-ONE  —  when k >= 0 -> isNonNeg(minus(F(k), oneFraction))
// ===========================================================================

struct FracGeOne {
    sgn: String,
    ops: RatOps,
    g: String,
    recpos: String,
}

fn recognize_frac_geone_shape(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Option<FracGeOne> {
    law.when.as_ref()?;
    if !matches!(law.rhs.node, Expr::Literal(Literal::Bool(true))) {
        return None;
    }
    let Expr::FnCall(callee, call_args) = &law.lhs.node else {
        return None;
    };
    let subject_src = expr_dotted_name(callee)?;
    let subj_fd = find_fn_def_by_call_name(ctx, &subject_src)?;
    if subj_fd.return_type.trim() != "Bool"
        || subj_fd.params.len() != call_args.len()
        || !subj_fd.effects.is_empty()
    {
        return None;
    }
    let body = subject_body(&subject_src, ctx)?;
    // Body `isNonNeg(minus(F(k), oneFraction()))`.
    let ops = rat_ops_from_body(body)?;
    let (_, nn_args) = shared::call_named_with_dotted(body, "isNonNeg", 1)?;
    let (_, m_args) = shared::call_named_with_dotted(&nn_args[0], "minus", 2)?;
    let (sgn_short, f_args) = shared::short_call_name_args(&m_args[0])?;
    if f_args.len() != 1 || shared::call_named_with_dotted(&m_args[1], "oneFraction", 0).is_none() {
        return None;
    }
    let Expr::FnCall(sgn_callee, _) = &m_args[0].node else {
        return None;
    };
    let sgn_dotted = expr_dotted_name(sgn_callee)?;
    let sgn_fd = find_fn_def_by_call_name(ctx, &sgn_dotted)?;
    if !is_unary_fraction_fn(sgn_fd) {
        return None;
    }
    let _ = sgn_short;
    // Premise must be a single nonnegative-exponent guard. Shared collection
    // canonicalizes `arg >= 0` and `0 <= arg` to the same `0 <= arg` shape.
    let clauses = shared::collect_when_clauses(law.when.as_ref()?);
    let [when] = clauses.as_slice() else {
        return None;
    };
    let Expr::BinOp(BinOp::Lte, l, _) = &when.node else {
        return None;
    };
    if !matches!(&l.node, Expr::Literal(Literal::Int(0))) {
        return None;
    }
    let (g, recpos) = find_recursive_positivity(vb, law, ctx)?;
    Some(FracGeOne {
        sgn: aver_name_to_lean(&sgn_dotted),
        ops,
        g,
        recpos,
    })
}

pub(in crate::codegen::lean) fn recognize_frac_geone(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> bool {
    recognize_frac_geone_shape(vb, law, ctx).is_some()
}

pub(super) fn emit_frac_geone_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    theorem_base: &str,
    quant_params: &str,
) -> Option<AutoProof> {
    let shape = recognize_frac_geone_shape(vb, law, ctx)?;
    let render = |e: &Spanned<Expr>| super::super::expr::emit_expr_legacy(e, ctx, None);
    let lhs = render(&law.lhs);
    let rhs = render(&law.rhs);
    let when = render(law.when.as_ref()?);
    let subject = aver_name_to_lean(&expr_dotted_name(&{
        let Expr::FnCall(callee, _) = &law.lhs.node else {
            return None;
        };
        (**callee).clone()
    })?);
    let intros: Vec<String> = law
        .givens
        .iter()
        .map(|g| aver_name_to_lean(&g.name))
        .collect();
    let FracGeOne {
        sgn,
        ops,
        g,
        recpos,
    } = &shape;
    let RatOps {
        isnonneg,
        minus,
        onefraction,
        ..
    } = ops;
    let text = format!(
        r#"theorem {base} : ∀ {quant}, {when} = true -> {lhs} = {rhs} := by
  intro {intros} h_when
  first
  | (simp only [eq_iff_iff, iff_true] at h_when
     have hpp : ∀ j : Int, 0 < {g} j := by
       intro j
       have h := {recpos} j
       simp only [eq_iff_iff, iff_true, ge_iff_le] at h
       omega
     simp only [{subject}, {sgn}, {isnonneg}, {minus}, {onefraction}, decide_eq_true_eq, eq_iff_iff, iff_true, ge_iff_le]
     grind)
  | sorry"#,
        base = theorem_base,
        quant = quant_params,
        intros = intros.join(" "),
    );
    Some(AutoProof {
        support_lines: text.lines().map(|l| l.to_string()).collect(),
        body: crate::codegen::lean::tactic_ir::Tactic::raw(Vec::new()),
        replaces_theorem: true,
    })
}

// ===========================================================================
// MONOTONICITY  —  when LO <= HI -> isNonNeg(minus(F(HI), F(LO)))
// ===========================================================================

struct FracMonotone {
    sgn: String,
    ops: RatOps,
    hi: Spanned<Expr>,
    lo: Spanned<Expr>,
    hom: PoolCite,
    pos: PoolCite,
    geone: PoolCite,
}

/// Match a homomorphism body `sameValue(F(a + b), times(F(a), F(b)))` over the
/// SAME fn `F` (basename `sgn_short`).
fn is_homomorphism_body(body: &Spanned<Expr>, sgn_short: &str) -> bool {
    let Some((_, sv_args)) = shared::call_named_with_dotted(body, "sameValue", 2) else {
        return false;
    };
    let Some((f0, f0_args)) = shared::short_call_name_args(&sv_args[0]) else {
        return false;
    };
    if f0 != sgn_short || f0_args.len() != 1 {
        return false;
    }
    if !matches!(&f0_args[0].node, Expr::BinOp(BinOp::Add, _, _)) {
        return false;
    }
    let Some((_, t_args)) = shared::call_named_with_dotted(&sv_args[1], "times", 2) else {
        return false;
    };
    matches!(shared::short_call_name_args(&t_args[0]), Some((s, a)) if s == sgn_short && a.len() == 1)
        && matches!(shared::short_call_name_args(&t_args[1]), Some((s, a)) if s == sgn_short && a.len() == 1)
}

/// Match a positivity body `Bool.and(F(_).x > 0, F(_).y > 0)` over `sgn_short`.
fn is_positivity_body(body: &Spanned<Expr>, sgn_short: &str) -> bool {
    let Some((_, and_args)) = shared::call_named_with_dotted(body, "and", 2) else {
        return false;
    };
    let same = |c: &Spanned<Expr>| {
        pos_comparison_fn(c)
            .map(|n| n.rsplit('.').next().unwrap_or(&n).to_string())
            .as_deref()
            == Some(sgn_short)
    };
    same(&and_args[0]) && same(&and_args[1])
}

/// Match a `>= 1` body `isNonNeg(minus(F(_), oneFraction()))` over `sgn_short`.
fn is_geone_body(body: &Spanned<Expr>, sgn_short: &str) -> bool {
    let Some((_, nn_args)) = shared::call_named_with_dotted(body, "isNonNeg", 1) else {
        return false;
    };
    let Some((_, m_args)) = shared::call_named_with_dotted(&nn_args[0], "minus", 2) else {
        return false;
    };
    matches!(shared::short_call_name_args(&m_args[0]), Some((s, a)) if s == sgn_short && a.len() == 1)
        && shared::call_named_with_dotted(&m_args[1], "oneFraction", 0).is_some()
}

fn recognize_frac_monotone_shape(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Option<FracMonotone> {
    law.when.as_ref()?;
    if !matches!(law.rhs.node, Expr::Literal(Literal::Bool(true))) {
        return None;
    }
    let Expr::FnCall(callee, call_args) = &law.lhs.node else {
        return None;
    };
    let subject_src = expr_dotted_name(callee)?;
    let subj_fd = find_fn_def_by_call_name(ctx, &subject_src)?;
    if subj_fd.return_type.trim() != "Bool"
        || subj_fd.params.len() != call_args.len()
        || !subj_fd.effects.is_empty()
    {
        return None;
    }
    let body = subject_body(&subject_src, ctx)?;
    // Body `isNonNeg(minus(F(HI), F(LO)))` over a single `Fraction`-valued `F`.
    let ops = rat_ops_from_body(body)?;
    let (_, nn_args) = shared::call_named_with_dotted(body, "isNonNeg", 1)?;
    let (_, m_args) = shared::call_named_with_dotted(&nn_args[0], "minus", 2)?;
    let (sgn_hi, hi_args) = shared::short_call_name_args(&m_args[0])?;
    let (sgn_lo, lo_args) = shared::short_call_name_args(&m_args[1])?;
    if hi_args.len() != 1 || lo_args.len() != 1 || sgn_hi != sgn_lo {
        return None;
    }
    let Expr::FnCall(hi_callee, _) = &m_args[0].node else {
        return None;
    };
    let Expr::FnCall(lo_callee, _) = &m_args[1].node else {
        return None;
    };
    let sgn_dotted = expr_dotted_name(hi_callee)?;
    if expr_dotted_name(lo_callee)? != sgn_dotted {
        return None;
    }
    let sgn_fd = find_fn_def_by_call_name(ctx, &sgn_dotted)?;
    if !is_unary_fraction_fn(sgn_fd) {
        return None;
    }
    let sgn_short = sgn_dotted
        .rsplit('.')
        .next()
        .unwrap_or(&sgn_dotted)
        .to_string();

    // Express the two exponents in the law's givens.
    let mut map: std::collections::HashMap<String, Spanned<Expr>> =
        std::collections::HashMap::new();
    for ((pname, _), arg) in subj_fd.params.iter().zip(call_args.iter()) {
        map.insert(pname.clone(), arg.clone());
    }
    let hi = substitute(&hi_args[0], &map);
    let lo = substitute(&lo_args[0], &map);

    // Premise must establish `LO <= HI` (small side `LO`, big side `HI`).
    let clauses = shared::collect_when_clauses(law.when.as_ref()?);
    let [when] = clauses.as_slice() else {
        return None;
    };
    let render = |e: &Spanned<Expr>| super::super::expr::emit_expr_legacy(e, ctx, None);
    let (lo_lean, hi_lean) = (render(&lo), render(&hi));
    let premise_ok = match &when.node {
        Expr::BinOp(BinOp::Lte, l, r) => render(l) == lo_lean && render(r) == hi_lean,
        _ => false,
    };
    if !premise_ok {
        return None;
    }

    // Discover the three pool laws about the SAME `F` (homomorphism, positivity,
    // `>= 1`). Each must be an earlier sibling with a citable universal theorem.
    let hom = find_earlier_law(vb, ctx, |_, prev_law, body| {
        matches!(&prev_law.rhs.node, Expr::Literal(Literal::Bool(true)))
            && is_homomorphism_body(body, &sgn_short)
    })?;
    let pos = find_earlier_law(vb, ctx, |_, prev_law, body| {
        matches!(&prev_law.rhs.node, Expr::Literal(Literal::Bool(true)))
            && is_positivity_body(body, &sgn_short)
    })?;
    let geone = find_earlier_law(vb, ctx, |_, prev_law, body| {
        matches!(&prev_law.rhs.node, Expr::Literal(Literal::Bool(true)))
            && is_geone_body(body, &sgn_short)
    })?;

    Some(FracMonotone {
        sgn: aver_name_to_lean(&sgn_dotted),
        ops,
        hi,
        lo,
        hom,
        pos,
        geone,
    })
}

pub(in crate::codegen::lean) fn recognize_frac_monotone_compose(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> bool {
    recognize_frac_monotone_shape(vb, law, ctx).is_some()
}

/// The module prefix `vb` belongs to (a dependency module), or `None` when `vb`
/// is the entry's own law (no namespace).
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

/// The `(module_prefix, theorem_base)` sibling laws a positivity / at-least-one /
/// monotonicity law CITES — emitted into the cross-file admission set so a
/// dependency export carries the WHOLE citation chain (the monotonicity needs its
/// module's homomorphism / positivity / `>= 1` theorems, and each of those needs
/// the recursive-positivity theorem). Their subject fns are NOT in the citing
/// law's call cone, so the structural admission gate never reaches them; this
/// hook admits them directly, keyed on the SAME recognizers the emit uses.
pub(in crate::codegen::lean) fn frac_monotone_compose_cited_deps(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Vec<(String, String)> {
    let Some(prefix) = module_prefix_of(vb, ctx) else {
        return Vec::new();
    };
    let mut out = Vec::new();
    if let Some(shape) = recognize_frac_monotone_shape(vb, law, ctx) {
        out.push((prefix.clone(), shape.hom.thm));
        out.push((prefix.clone(), shape.pos.thm));
        out.push((prefix.clone(), shape.geone.thm));
        if let Some((_, recpos)) = find_recursive_positivity(vb, law, ctx) {
            out.push((prefix.clone(), recpos));
        }
    }
    // A positivity / at-least-one law itself cites the recursive-positivity law.
    if (recognize_frac_positivity_shape(vb, law, ctx).is_some()
        || recognize_frac_geone_shape(vb, law, ctx).is_some())
        && let Some((_, recpos)) = find_recursive_positivity(vb, law, ctx)
    {
        out.push((prefix, recpos));
    }
    out
}

/// The generic exact-rational order kit (verbatim transcription of the
/// kernel-checked rung), scoped to `{base}__`. Every primitive name is a
/// parameter — name-blind, nothing pow2-specific.
fn render_frac_kit(base: &str, ops: &RatOps) -> String {
    let RatOps {
        isnonneg,
        minus,
        times,
        samevalue,
        ..
    } = ops;
    let p = format!("{base}__");
    format!(
        r#"theorem {p}sq_nonneg (x : Int) : 0 ≤ x * x := by
  cases Int.le_total 0 x with
  | inl h => exact Int.mul_nonneg h h
  | inr h => exact Int.mul_nonneg_of_nonpos_of_nonpos h h
theorem {p}sq_pos {{x : Int}} (hx : x ≠ 0) : 0 < x * x := by
  cases Int.lt_or_gt_of_ne hx with
  | inl h => exact Int.mul_pos_of_neg_of_neg h h
  | inr h => exact Int.mul_pos h h
theorem {p}frac_le_mul_pos (kk a b : Fraction) (hkt : 0 < kk.top) (hkb : 0 < kk.bottom)
    (h : {isnonneg} ({minus} b a) = true) :
    {isnonneg} ({minus} ({times} kk b) ({times} kk a)) = true := by
  simp only [{isnonneg}, {minus}, {times}, decide_eq_true_eq, ge_iff_le] at h ⊢
  have hid : ((kk.top*b.top)*(kk.bottom*a.bottom) - (kk.top*a.top)*(kk.bottom*b.bottom))
                * ((kk.bottom*b.bottom)*(kk.bottom*a.bottom))
           = ((kk.top*kk.bottom)*(kk.bottom*kk.bottom))
                * ((b.top*a.bottom - a.top*b.bottom) * (b.bottom*a.bottom)) := by grind
  have hkpos : 0 ≤ (kk.top*kk.bottom)*(kk.bottom*kk.bottom) :=
    Int.mul_nonneg (Int.mul_nonneg (Int.le_of_lt hkt) (Int.le_of_lt hkb)) ({p}sq_nonneg kk.bottom)
  rw [hid]
  exact Int.mul_nonneg hkpos h
theorem {p}frac_le_samevalue_right (a b b' : Fraction) (hb : b.bottom ≠ 0)
    (hbb' : {samevalue} b b' = true) (h : {isnonneg} ({minus} b a) = true) :
    {isnonneg} ({minus} b' a) = true := by
  simp only [{isnonneg}, {minus}, {samevalue}, decide_eq_true_eq, ge_iff_le, beq_iff_eq] at h hbb' ⊢
  have hid : ((b'.top*a.bottom - a.top*b'.bottom)*(b'.bottom*a.bottom)) * (b.bottom*b.bottom)
           = ((b.top*a.bottom - a.top*b.bottom)*(b.bottom*a.bottom)) * (b'.bottom*b'.bottom) := by
    grind
  have hrhs : 0 ≤ ((b.top*a.bottom - a.top*b.bottom)*(b.bottom*a.bottom)) * (b'.bottom*b'.bottom) :=
    Int.mul_nonneg h ({p}sq_nonneg b'.bottom)
  rw [← hid] at hrhs
  exact Int.nonneg_of_mul_nonneg_left hrhs ({p}sq_pos hb)
theorem {p}sameValue_symm (a b : Fraction) (h : {samevalue} a b = true) : {samevalue} b a = true := by
  simp only [{samevalue}, beq_iff_eq] at h ⊢
  omega"#
    )
}

pub(super) fn emit_frac_monotone_compose_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    theorem_base: &str,
    quant_params: &str,
) -> Option<AutoProof> {
    let shape = recognize_frac_monotone_shape(vb, law, ctx)?;
    let render = |e: &Spanned<Expr>| super::super::expr::emit_expr_legacy(e, ctx, None);
    let hi = render(&shape.hi);
    let lo = render(&shape.lo);
    let lhs = render(&law.lhs);
    let rhs = render(&law.rhs);
    let when = render(law.when.as_ref()?);
    let subject = aver_name_to_lean(&expr_dotted_name(&{
        let Expr::FnCall(callee, _) = &law.lhs.node else {
            return None;
        };
        (**callee).clone()
    })?);
    let intros: Vec<String> = law
        .givens
        .iter()
        .map(|g| aver_name_to_lean(&g.name))
        .collect();
    let FracMonotone {
        sgn,
        ops,
        hom,
        pos,
        geone,
        ..
    } = &shape;
    let RatOps {
        isnonneg,
        minus,
        times,
        samevalue,
        onefraction,
    } = ops;
    let kit = render_frac_kit(theorem_base, ops);
    let p = format!("{theorem_base}__");
    let assembly = format!(
        r#"set_option maxHeartbeats 1000000 in
theorem {base} : ∀ {quant}, {when} = true -> {lhs} = {rhs} := by
  intro {intros} h_when
  first
  | (simp only [eq_iff_iff, iff_true] at h_when
     simp only [{subject}]
     have hgeC : {isnonneg} ({minus} ({sgn} (({hi}) - ({lo}))) {onefraction}) = true := by
       have h := {geone_thm} (({hi}) - ({lo})) (by simp only [eq_iff_iff, iff_true]; omega)
       simpa only [{geone_subject}] using h
     have hposm : 0 < ({sgn} ({lo})).top ∧ 0 < ({sgn} ({lo})).bottom := by
       have h := {pos_thm} ({lo})
       simpa only [{pos_subject}, Bool.and_eq_true, decide_eq_true_eq, gt_iff_lt] using h
     have hposc : 0 < ({sgn} (({hi}) - ({lo}))).top ∧ 0 < ({sgn} (({hi}) - ({lo}))).bottom := by
       have h := {pos_thm} (({hi}) - ({lo}))
       simpa only [{pos_subject}, Bool.and_eq_true, decide_eq_true_eq, gt_iff_lt] using h
     have hScaled : {isnonneg} ({minus} ({times} ({sgn} ({lo})) ({sgn} (({hi}) - ({lo})))) ({times} ({sgn} ({lo})) {onefraction})) = true :=
       {p}frac_le_mul_pos ({sgn} ({lo})) {onefraction} ({sgn} (({hi}) - ({lo}))) hposm.1 hposm.2 hgeC
     have hAone : {times} ({sgn} ({lo})) {onefraction} = {sgn} ({lo}) := by
       simp only [{times}, {onefraction}, Int.mul_one]
     rw [hAone] at hScaled
     have hsame : {samevalue} ({sgn} ({hi})) ({times} ({sgn} ({lo})) ({sgn} (({hi}) - ({lo})))) = true := by
       have hh := {hom_thm} ({lo}) (({hi}) - ({lo}))
       simp only [{hom_subject}] at hh
       rwa [show ({lo}) + (({hi}) - ({lo})) = ({hi}) by omega] at hh
     have hsame' : {samevalue} ({times} ({sgn} ({lo})) ({sgn} (({hi}) - ({lo})))) ({sgn} ({hi})) = true :=
       {p}sameValue_symm _ _ hsame
     have hbne : ({times} ({sgn} ({lo})) ({sgn} (({hi}) - ({lo})))).bottom ≠ 0 := by
       simp only [{times}]
       exact Int.ne_of_gt (Int.mul_pos hposm.2 hposc.2)
     exact {p}frac_le_samevalue_right ({sgn} ({lo})) ({times} ({sgn} ({lo})) ({sgn} (({hi}) - ({lo})))) ({sgn} ({hi})) hbne hsame' hScaled)
  | sorry"#,
        base = theorem_base,
        quant = quant_params,
        intros = intros.join(" "),
        hom_thm = hom.thm,
        hom_subject = hom.subject,
        pos_thm = pos.thm,
        pos_subject = pos.subject,
        geone_thm = geone.thm,
        geone_subject = geone.subject,
    );
    let text = format!("{kit}\n{assembly}");
    Some(AutoProof {
        support_lines: text.lines().map(|l| l.to_string()).collect(),
        body: crate::codegen::lean::tactic_ir::Tactic::raw(Vec::new()),
        replaces_theorem: true,
    })
}

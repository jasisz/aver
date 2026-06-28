//! Lean renderer for the signed-power-of-two MONOTONICITY rung — the
//! order-monotonicity sibling of the rational strict-order bound
//! (`floor_bound`'s `lessThan(absFraction(error), pow2Signed(bound))`
//! family). It closes ONE generic shape, name-blind on the law / given /
//! figure names (keyed only on the Rational order primitives `isNonNeg` /
//! `minus` and the signed power-of-two cone fn's STRUCTURE): a conditional
//! `holds` law whose subject body is
//!
//! ```text
//! isNonNeg (minus (pow2Signed E_hi) (pow2Signed E_lo))
//! ```
//!
//! under a premise `E_lo <= E_hi`, for ARBITRARY integer-expression
//! exponents `E_hi` / `E_lo`. In the project's exact-rational order this is
//! exactly `pow2Signed E_lo <= pow2Signed E_hi`, i.e. the signed power of two
//! is monotone for EVERY integer exponent (the integer `pow2` is monotone
//! only on the nonnegative half — it clamps to 1 below zero — but
//! `pow2Signed` denotes `2^k` faithfully for every `k`, so its monotonicity
//! needs only `E_lo <= E_hi`).
//!
//! This module is now a THIN DOMAIN ADAPTER: the recursive monotonicity algebra
//! is NOT pow2-specific and lives in the SHARED [`super::recursive_mono`] kit
//! (`recursive_decrement_arms` + `render_recursive_mono_kit`) — the def
//! equations (`rec_lo` / `rec_hi`), positivity (`rec_pos` = `BASE <= pow n` by
//! `pow.induct`), monotonicity (`rec_mono` = `m <= n -> pow m <= pow n` by
//! `pow.induct`, NO `0 <= m` premise), and the product helper (`rec_one_le_mul`)
//! — the exact same kit that closes the `tri` / `fct` monotonicity laws. This
//! adapter contributes ONLY the domain part: recognizing the Fraction sign-split
//! shape and emitting the four-way `by_cases` on `(E_hi < 0)` / `(E_lo < 0)` that
//! reduces each `pow2Signed` sign branch and discharges the resulting `Int`
//! product nonnegativity from the shared kit. The four-way scaffold is rendered
//! as a `<;> first | ...` portfolio so the same emission closes every sign
//! combination uniformly, and the whole assembly sits under a `first | (...) |
//! sorry` floor: a shape the recognizer admits but whose closing slips falls to
//! an honest caught `sorry`, so credit stays fail-closed behind the `#print
//! axioms` whitelist. The kit lemmas always typecheck (they do not mention the
//! law), so only the assembly can fall through.
//!
//! Soundness: the `E_lo <= E_hi` premise is load-bearing — without it the
//! signed power of two is not monotone, and the recognizer requires the
//! premise to relate exactly the two `pow2Signed` exponents (small side
//! `E_lo`, big side `E_hi`); a reversed or unrelated premise is declined (the
//! law keeps its bounded sampled fallback).

use super::AutoProof;
use super::aver_name_to_lean;
use super::shared::{expr_dotted_name, find_fn_def_by_call_name};
use crate::ast::{Expr, FnDef, Spanned, Stmt, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;

/// The recognized monotonicity shape: the role-assigned exponent expressions
/// plus the Lean names of every fn the rendered proof references — all keyed
/// on term STRUCTURE, never on a law/figure name.
pub(super) struct Pow2SignedMonotone {
    /// `E_hi`: the MINUEND exponent (first `minus` operand's `pow2Signed`
    /// argument), in terms of the law givens.
    hi: Spanned<Expr>,
    /// `E_lo`: the SUBTRAHEND exponent (second `minus` operand's `pow2Signed`
    /// argument), in terms of the law givens.
    lo: Spanned<Expr>,
    /// The monotonicity subject fn (dotted source name) — unfolded in the proof.
    subject: String,
    /// The `isNonNeg` Rational predicate (dotted source name).
    isnonneg: String,
    /// The `minus` Rational op (dotted source name).
    minus: String,
    /// The signed power-of-two cone fn `pow2Signed` (dotted source name).
    sgn: String,
    /// The underlying recursive integer power-of-two `pow` (source name) the
    /// signed fn calls — the helper lemmas reason about `pow.induct`/`pow.eq_def`.
    pow: String,
}

/// `(short_callee_name, args)` when `expr` is a function call, else `None`.
/// The short name is the last dotted component, so a qualified
/// `Domain.Rational.minus` call matches the primitive `minus` — the rung keys
/// on the rational ALGEBRA primitive, never on the law/figure name.
fn as_call(expr: &Spanned<Expr>) -> Option<(String, &[Spanned<Expr>])> {
    let Expr::FnCall(callee, args) = &expr.node else {
        return None;
    };
    let dotted = expr_dotted_name(callee)?;
    let short = dotted.rsplit('.').next().unwrap_or(&dotted).to_string();
    Some((short, args.as_slice()))
}

/// A call to the named primitive (matched on its SHORT name) with exactly `n`
/// args, returning `(dotted_name, args)`.
fn call_named<'a>(
    expr: &'a Spanned<Expr>,
    name: &str,
    n: usize,
) -> Option<(String, &'a [Spanned<Expr>])> {
    let Expr::FnCall(callee, args) = &expr.node else {
        return None;
    };
    let dotted = expr_dotted_name(callee)?;
    let short = dotted.rsplit('.').next().unwrap_or(&dotted);
    (short == name && args.len() == n).then_some((dotted, args.as_slice()))
}

/// Collect the dotted names of every `FnCall` reachable in `e`.
fn collect_fncall_names(e: &Expr, out: &mut Vec<String>) {
    match e {
        Expr::FnCall(callee, args) => {
            if let Some(n) = expr_dotted_name(callee) {
                out.push(n);
            }
            for a in args {
                collect_fncall_names(&a.node, out);
            }
        }
        Expr::BinOp(_, a, b) => {
            collect_fncall_names(&a.node, out);
            collect_fncall_names(&b.node, out);
        }
        Expr::Neg(a) => collect_fncall_names(&a.node, out),
        Expr::Attr(b, _) => collect_fncall_names(&b.node, out),
        Expr::RecordCreate { fields, .. } => {
            for (_, v) in fields {
                collect_fncall_names(&v.node, out);
            }
        }
        Expr::Match { subject, arms } => {
            collect_fncall_names(&subject.node, out);
            for arm in arms {
                collect_fncall_names(&arm.body.node, out);
            }
        }
        _ => {}
    }
}

/// Deep-clone `e`, replacing any free `Ident`/`Resolved` named in `map` by the
/// mapped expression — substitutes the subject fn's parameters by the law
/// call's argument terms, so the recognized exponents are expressed in the
/// law's givens.
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
        Expr::BinOp(op, a, b) => {
            Expr::BinOp(*op, Box::new(substitute(a, map)), Box::new(substitute(b, map)))
        }
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

/// Whether `fd` is a signed-power-of-two cone fn — one `Int` param returning
/// `Fraction`, body a `match k < 0 { … }` two-arm sign split — and, if so, the
/// UNIQUE recursive integer power-of-two `pow` (source name) its arms call.
/// Keyed on structure: a fn that is not this exact shape, or that calls no
/// single recursive pure fn, is declined.
fn signed_pow2_pow(fd: &FnDef, ctx: &CodegenContext) -> Option<String> {
    use crate::ast::{BinOp, Literal};
    let [(p, ty)] = fd.params.as_slice() else {
        return None;
    };
    if ty.trim() != "Int" || fd.return_type.rsplit('.').next() != Some("Fraction") {
        return None;
    }
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return None;
    };
    let Expr::Match { subject, arms } = &body.node else {
        return None;
    };
    // Subject `k < 0` over the lone parameter.
    let Expr::BinOp(BinOp::Lt, sl, sr) = &subject.node else {
        return None;
    };
    if expr_dotted_name(sl).as_deref() != Some(p.as_str())
        || !matches!(&sr.node, Expr::Literal(Literal::Int(0)))
        || arms.len() != 2
    {
        return None;
    }
    // The unique recursive pure fn called from the arms is the power-of-two.
    let recursive = crate::codegen::lean::recursive_pure_fn_names(ctx);
    let mut called: Vec<String> = Vec::new();
    for arm in arms {
        collect_fncall_names(&arm.body.node, &mut called);
    }
    let mut pows: Vec<String> = called
        .into_iter()
        .map(|n| n.rsplit('.').next().unwrap_or(&n).to_string())
        .filter(|n| recursive.contains(n))
        .collect();
    pows.sort();
    pows.dedup();
    match pows.as_slice() {
        [single] => Some(single.clone()),
        _ => None,
    }
}

/// Recognize the signed-power-of-two monotonicity shape. Pure / name-blind —
/// see [`Pow2SignedMonotone`]. Declines (so the law keeps its bounded sampled
/// fallback / another rung) unless every structural gate holds.
pub(super) fn recognize_pow2_signed_monotone_shape(
    _vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Option<Pow2SignedMonotone> {
    // Conditional `subject(args) holds` ⇒ `subject(args) = true`.
    law.when.as_ref()?;
    if !matches!(law.rhs.node, Expr::Literal(crate::ast::Literal::Bool(true))) {
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
    // Subject body must be `isNonNeg (minus (sgn HI) (sgn LO))` — the shape
    // gate that makes this an honest signed-power-of-two ORDER closer.
    let [Stmt::Expr(body)] = subj_fd.body.stmts() else {
        return None;
    };
    let (isnonneg, nn_args) = call_named(body, "isNonNeg", 1)?;
    let (minus, m_args) = call_named(&nn_args[0], "minus", 2)?;
    let (sgn_hi, hi_args) = as_call(&m_args[0])?;
    let (sgn_lo, lo_args) = as_call(&m_args[1])?;
    if hi_args.len() != 1 || lo_args.len() != 1 {
        return None;
    }
    // Both `pow2Signed` calls must head the SAME fn — the signed power of two.
    let Expr::FnCall(hi_callee, _) = &m_args[0].node else {
        return None;
    };
    let Expr::FnCall(lo_callee, _) = &m_args[1].node else {
        return None;
    };
    let sgn_dotted = expr_dotted_name(hi_callee)?;
    if sgn_hi != sgn_lo || expr_dotted_name(lo_callee)? != sgn_dotted {
        return None;
    }
    // The signed fn is a `Fraction`-valued `2^k` sign split; capture its `pow`.
    let sgn_fd = find_fn_def_by_call_name(ctx, &sgn_dotted)?;
    let pow = signed_pow2_pow(sgn_fd, ctx)?;

    // Express the two exponents in the law's givens by substituting the
    // subject's params with the call's arguments.
    let mut map: std::collections::HashMap<String, Spanned<Expr>> =
        std::collections::HashMap::new();
    for ((pname, _), arg) in subj_fd.params.iter().zip(call_args.iter()) {
        map.insert(pname.clone(), arg.clone());
    }
    let hi = substitute(&hi_args[0], &map);
    let lo = substitute(&lo_args[0], &map);

    // The premise must establish `E_lo <= E_hi` (small side `E_lo`, big side
    // `E_hi`) — load-bearing for monotonicity. Compare each side to the
    // recognized exponents through the SAME Lean emitter, so a structurally
    // equal premise matches and a reversed / unrelated one is declined.
    let when = law.when.as_ref()?;
    let render = |e: &Spanned<Expr>| super::super::expr::emit_expr_legacy(e, ctx, None);
    let (lo_lean, hi_lean) = (render(&lo), render(&hi));
    let premise_ok = match &when.node {
        Expr::BinOp(crate::ast::BinOp::Lte, l, r) => {
            render(l) == lo_lean && render(r) == hi_lean
        }
        Expr::BinOp(crate::ast::BinOp::Gte, l, r) => {
            render(l) == hi_lean && render(r) == lo_lean
        }
        _ => false,
    };
    if !premise_ok {
        return None;
    }

    Some(Pow2SignedMonotone {
        hi,
        lo,
        subject: subject_src,
        isnonneg,
        minus,
        sgn: sgn_dotted,
        pow,
    })
}

/// Statement-builder hook: whether the monotonicity emit will close this law
/// universally (so the caller drops the sampled domain and classes it
/// `universal`, keeping statement and proof in lockstep).
pub(in crate::codegen::lean) fn recognize_pow2_signed_monotone(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> bool {
    recognize_pow2_signed_monotone_shape(vb, law, ctx).is_some()
}

/// Close a signed-power-of-two monotonicity law. Emits the SHARED recursive-
/// monotonicity kit for the inner power-of-two `pow` (the `pow.induct`
/// positivity + monotonicity algebra, identical to every other recursive
/// monotonicity) plus the DOMAIN-specific four-way sign scaffold that unwraps
/// the Fraction sign-split — all as a self-contained TRUE-universal theorem
/// (`replaces_theorem`), wrapped `first | (…) | sorry`.
pub(super) fn emit_pow2_signed_monotone_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    theorem_base: &str,
    quant_params: &str,
) -> Option<AutoProof> {
    let shape = recognize_pow2_signed_monotone_shape(vb, law, ctx)?;
    let render = |e: &Spanned<Expr>| super::super::expr::emit_expr_legacy(e, ctx, None);
    let hi = render(&shape.hi);
    let lo = render(&shape.lo);
    let subject = aver_name_to_lean(&shape.subject);
    let isnonneg = aver_name_to_lean(&shape.isnonneg);
    let minus = aver_name_to_lean(&shape.minus);
    let sgn = aver_name_to_lean(&shape.sgn);
    let pow = aver_name_to_lean(&shape.pow);

    // The recursive-monotonicity algebra (positivity + monotonicity of the inner
    // `pow`) is the SHARED kit — no pow2-specific helper code lives here.
    let pow_fd = find_fn_def_by_call_name(ctx, &shape.pow)?;
    let (base_arm, step_arm) = super::recursive_mono::recursive_decrement_arms(pow_fd, ctx)?;
    let kit = super::recursive_mono::render_recursive_mono_kit(
        theorem_base,
        &pow,
        &base_arm,
        &step_arm,
    );

    let lhs = render(&law.lhs);
    let rhs = render(&law.rhs);
    let when = render(law.when.as_ref()?);
    let intros: Vec<String> = law
        .givens
        .iter()
        .map(|g| aver_name_to_lean(&g.name))
        .collect();

    // The ONLY domain-specific code: the four-way sign scaffold that unwraps the
    // `pow2Signed` sign-split and hands each Int product to the shared kit.
    let assembly = format!(
        r#"set_option maxHeartbeats 1000000 in
theorem {base} : ∀ {quant_params}, {when} = true -> {lhs} = {rhs} := by
  intro {intros} h_when
  first
  | (simp only [eq_iff_iff, iff_true] at h_when
     by_cases hn : ({hi}) < 0 <;> by_cases hm : ({lo}) < 0 <;>
       simp only [{subject}, {isnonneg}, {minus}, {sgn}, hn, hm, if_true, if_false,
         decide_eq_true_eq, ge_iff_le] <;>
       first
       | (exfalso; omega)
       | (apply Int.mul_nonneg <;>
          first
          | (have hle := {mono} (0 - ({lo})) (0 - ({hi})) (by omega); omega)
          | (exact Int.mul_nonneg (by have := {pos} (0 - ({hi})); omega)
                                  (by have := {pos} (0 - ({lo})); omega))
          | (have h1 := {one_le_mul} ({pow} ({hi})) ({pow} (0 - ({lo})))
               ({pos} ({hi})) ({pos} (0 - ({lo}))); omega)
          | (have := {pos} (0 - ({lo})); omega)
          | (have hle := {mono} ({hi}) ({lo}) (by omega); omega)
          | omega))
  | sorry"#,
        base = theorem_base,
        intros = intros.join(" "),
        mono = kit.mono,
        pos = kit.pos,
        one_le_mul = kit.one_le_mul,
    );
    let text = format!("{}\n{}", kit.text, assembly);
    Some(AutoProof {
        support_lines: text.lines().map(|l| l.to_string()).collect(),
        body: crate::codegen::lean::tactic_ir::Tactic::raw(Vec::new()),
        replaces_theorem: true,
    })
}

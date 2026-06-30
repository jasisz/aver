//! Lean renderer for the rational-order CHAINING rung — the broad,
//! reusable Fraction `<=` (`isNonNeg (minus C A)`) rung of `Domain.Rational`,
//! and its first consumer, the reciprocal-magnitude composition (Lemma 8.2.4,
//! p.30) of `projects/k5_fdiv`.
//!
//! ## The reusable core (the permanent rational-order rung)
//!
//! The renderer always emits a kit of GENERIC, name-blind helper lemmas about
//! the exact-rational order — provable on their own, independent of K5, keyed
//! only on the order primitives `isNonNeg` / `minus` / `times` / `lessThan` /
//! `sameValue`:
//!
//! - `frac_le_trans` — transitivity of `<=` (the keystone): `a<=b`, `b<=c`,
//!   `b.bottom ≠ 0` ⟹ `a<=c`. The middle denominator side condition is
//!   MANDATORY (transitivity over the sign-robust cross-multiplied order is
//!   false without it). Discharged by a `grind`-found ring identity
//!   `P·c² + Q·a² = b²·R` then sign/cancel.
//! - `frac_lt_imp_le` — `a<b ⟹ a<=b`.
//! - `frac_le_mul_pos` — scaling both sides by a fraction with positive
//!   numerator and denominator preserves `<=`.
//! - `frac_le_samevalue_right` — `<=` respects `sameValue` on the upper bound.
//!
//! plus the `sq_nonneg` / `sq_pos` Int helpers they rest on. This is the de-
//! risked, kernel-checked, core-Lean (no Mathlib / no `native_decide`) rung the
//! de-risk verdict calls "a permanent rational-order rung, independent of K5".
//!
//! ## The recognized shape (the chaining closer)
//!
//! The rung CLOSES a conditional `holds` law whose conclusion is the Fraction
//! order fact `isNonNeg (minus (pow2Signed BIG) A)` — i.e. `A <= 2^BIG` — by
//! chaining the premises and a cited monotonicity law through `frac_le_trans`.
//! Concretely it recognizes the reciprocal-magnitude composition shape (the
//! envelope/placement chain of Lemma 8.2.4): a premise `A <= 2·M` (an
//! `isNonNeg (minus (times 2 M) A)` scaled bound), a premise `M < 2^SMALL` (a
//! `lessThan`), a well-formedness `M.bottom ≠ 0`, and an exponent placement
//! `SMALL+1 <= BIG` (any `<=`/`>=` premise the final monotone citation's
//! `omega` consumes). The chain is
//!
//! ```text
//! A <= 2·M < 2·2^SMALL = 2^(SMALL+1) <= 2^BIG
//! ```
//!
//! the final `2^(SMALL+1) <= 2^BIG` CITING the signed power-of-two monotonicity
//! pool law (discovered name-blind from the dependency module — the load-bearing
//! paper lemma). The doubling `2·2^k = 2^(k+1)` is SELF-EMITTED here as an
//! elementary fact (the same class of pow algebra the pow2 monotonicity rung
//! self-emits): the homomorphism pool law currently degrades to a sorry floor
//! under cross-file dependency admission, so it cannot be cited cleanly yet.
//! The exponent placement premise is LOAD-BEARING: it is exactly what the cited
//! monotonicity needs, and the recognizer requires it (drop it and the chain's
//! final step has no `SMALL+1 <= BIG`).
//!
//! HONEST BREADTH: the helper KIT is broad and reusable (any Fraction-order
//! project can cite `frac_le_trans` & co). The RECOGNIZER is name-blind and
//! shape-keyed on the order primitives, but the closing chain it assembles is
//! the specific 8.2.4 envelope/placement composition (scale-by-two + doubling
//! homomorphism + lt→le + two transitivities + one monotonicity citation), not
//! a general edge-graph search over arbitrary `<=` premises. The doubling
//! constant `2` is the mathematical doubling of the reciprocal envelope, not a
//! figure name.
//!
//! The whole assembly sits under a `first | (…) | sorry` floor: a law whose
//! shape the recognizer admits but whose closing slips falls to an honest
//! caught `sorry`, so credit stays fail-closed behind the `#print axioms`
//! whitelist.

use super::AutoProof;
use super::aver_name_to_lean;
use super::shared::{expr_dotted_name, find_fn_def_by_call_name};
use crate::ast::{Expr, FnDef, Spanned, Stmt, VerifyBlock, VerifyKind, VerifyLaw};
use crate::codegen::CodegenContext;

/// The recognized roles of a reciprocal-magnitude composition law. Every name
/// is DISCOVERED from the law / dependency cone — nothing keyed on a law or
/// given name. The exponent/value roles are rendered Lean expressions in the
/// law's givens.
pub(super) struct FracOrderChain {
    /// `A` — the bounded magnitude (subtrahend of the conclusion `minus`).
    a: String,
    /// `M` — the scaled magnitude (the `times` operand in the `A <= 2·M`
    /// premise and the `lessThan` subject).
    m: String,
    /// `K` — the doubling constant fraction `2/1`, rendered exactly as the
    /// premise writes it so the chain's `times K …` matches the hypothesis.
    k: String,
    /// `SMALL` — the envelope exponent (`M < 2^SMALL`), in the law's givens.
    small: String,
    /// `BIG` — the final bound exponent (the conclusion `pow2Signed` argument),
    /// in the law's givens.
    big: String,
    /// Lean names of the rational order primitives (qualified).
    isnonneg: String,
    minus: String,
    times: String,
    lessthan: String,
    samevalue: String,
    /// The signed power-of-two cone fn `pow2Signed` (Lean name).
    sgn: String,
    /// The underlying recursive integer power-of-two `pow2` (Lean name).
    pow: String,
    /// The signed power-of-two monotonicity def + its cited universal theorem
    /// (the load-bearing order fact, cited through the laws-as-lemmas pool). It
    /// is closed by the DETERMINISTIC self-contained signed-pow2 monotonicity
    /// rung, so it stays kernel-clean when admitted as a dependency law (unlike
    /// the keystone-closed homomorphism, which degrades to `sorry` on
    /// admission — the elementary doubling `2·2^k = 2^(k+1)` is therefore
    /// self-emitted in the kit, the same way the pow2 monotonicity rung
    /// self-emits its pow positivity/monotonicity, not cited).
    mono_def: String,
    mono_thm: String,
    /// The cited monotonicity pool law as `(module_prefix, theorem_base)` — the
    /// cross-file admission keys on it so the dep theorem is emitted.
    cited_deps: Vec<(String, String)>,
}

/// `(short_callee_name, args)` when `expr` is a function call. The short name
/// is the last dotted component, so a qualified `Domain.Rational.minus` call
/// matches the primitive `minus` — keyed on the rational ALGEBRA primitive.
fn as_call(expr: &Spanned<Expr>) -> Option<(String, &[Spanned<Expr>])> {
    let Expr::FnCall(callee, args) = &expr.node else {
        return None;
    };
    let dotted = expr_dotted_name(callee)?;
    let short = dotted.rsplit('.').next().unwrap_or(&dotted).to_string();
    Some((short, args.as_slice()))
}

/// A call to the named primitive (matched on its SHORT name) with exactly `n`
/// args, returning the args.
fn call_named<'a>(expr: &'a Spanned<Expr>, name: &str, n: usize) -> Option<&'a [Spanned<Expr>]> {
    let (short, args) = as_call(expr)?;
    (short == name && args.len() == n).then_some(args)
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
/// call's argument terms.
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

/// Whether `fd` is a signed-power-of-two cone fn — one `Int` param returning
/// `Fraction`, body a `match k < 0 { … }` two-arm sign split — and, if so, the
/// UNIQUE recursive integer power-of-two `pow` (source name) its arms call.
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
    let Expr::BinOp(BinOp::Lt, sl, sr) = &subject.node else {
        return None;
    };
    if expr_dotted_name(sl).as_deref() != Some(p.as_str())
        || !matches!(&sr.node, Expr::Literal(Literal::Int(0)))
        || arms.len() != 2
    {
        return None;
    }
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

/// A discovered dependency-module pool law about the signed power of two
/// `sgn_dotted`: its `(prefix, theorem_base, bare_theorem, def_lean)`.
struct PoolLaw {
    prefix: String,
    theorem_base: String,
    theorem_lean: String,
    def_lean: String,
}

/// Whether the subject fn `subj_src`'s body is the signed power-of-two
/// MONOTONICITY shape `isNonNeg (minus (F n) (F m))` over the SAME signed
/// power-of-two fn `sgn_short` (basename). Name-blind on the primitives.
fn body_matches_monotone(ctx: &CodegenContext, subj_src: &str, sgn_short: &str) -> bool {
    let Some(fd) = find_fn_def_by_call_name(ctx, subj_src) else {
        return false;
    };
    if fd.return_type.trim() != "Bool" {
        return false;
    }
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return false;
    };
    let is_sgn = |e: &Spanned<Expr>| -> bool {
        matches!(as_call(e), Some((s, a)) if s == sgn_short && a.len() == 1)
    };
    let Some(nn) = call_named(body, "isNonNeg", 1) else {
        return false;
    };
    let Some(m) = call_named(&nn[0], "minus", 2) else {
        return false;
    };
    is_sgn(&m[0]) && is_sgn(&m[1])
}

/// Find the dependency-module signed power-of-two MONOTONICITY pool law over
/// `sgn_short`. Every name is DISCOVERED from the dependency law (name-blind).
fn find_monotone_law(ctx: &CodegenContext, sgn_short: &str) -> Option<PoolLaw> {
    for module in &ctx.modules {
        for vb in &module.verify_laws {
            let VerifyKind::Law(law) = &vb.kind else {
                continue;
            };
            if !matches!(law.rhs.node, Expr::Literal(crate::ast::Literal::Bool(true))) {
                continue;
            }
            if !body_matches_monotone(ctx, &vb.fn_name, sgn_short) {
                continue;
            }
            let base = format!(
                "{}_law_{}",
                aver_name_to_lean(&vb.fn_name),
                aver_name_to_lean(&law.name)
            );
            return Some(PoolLaw {
                prefix: module.prefix.clone(),
                theorem_base: base.clone(),
                theorem_lean: base,
                def_lean: format!("{}.{}", module.prefix, aver_name_to_lean(&vb.fn_name)),
            });
        }
    }
    None
}

/// Recognize the reciprocal-magnitude composition shape. Pure / name-blind.
/// Declines (so the law keeps its bounded sampled fallback / another rung)
/// unless every structural gate holds.
pub(super) fn recognize_frac_order_chain_shape(
    _vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Option<FracOrderChain> {
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
    // Subject body must be `isNonNeg (minus (sgn BIG) A)`.
    let [Stmt::Expr(body)] = subj_fd.body.stmts() else {
        return None;
    };
    let nn = call_named(body, "isNonNeg", 1)?;
    // `isNonNeg` / `minus` Lean names (qualified), derived from the conclusion
    // heads — name-blind on the rational order primitives.
    let Expr::FnCall(isnonneg_callee, _) = &body.node else {
        return None;
    };
    let isnonneg = aver_name_to_lean(&expr_dotted_name(isnonneg_callee)?);
    let m = call_named(&nn[0], "minus", 2)?;
    let Expr::FnCall(minus_callee, _) = &nn[0].node else {
        return None;
    };
    let minus = aver_name_to_lean(&expr_dotted_name(minus_callee)?);
    // The rational module prefix (e.g. `Domain.Rational`) the order ops live
    // in, read off the `minus` call — so `times` / `sameValue` are the SAME
    // module's primitives, derived not hardcoded.
    let rat_prefix = minus.rsplit_once('.').map(|(p, _)| p.to_string())?;
    let times = format!("{rat_prefix}.times");
    let samevalue = format!("{rat_prefix}.sameValue");
    let (sgn_short, hi_args) = as_call(&m[0])?;
    if hi_args.len() != 1 {
        return None;
    }
    let Expr::FnCall(sgn_callee, _) = &m[0].node else {
        return None;
    };
    let sgn_dotted = expr_dotted_name(sgn_callee)?;
    // The signed fn is a `Fraction`-valued `2^k` sign split; capture its `pow`.
    let sgn_fd = find_fn_def_by_call_name(ctx, &sgn_dotted)?;
    let pow = signed_pow2_pow(sgn_fd, ctx)?;

    // Express BIG and A in the law's givens by substituting subject params.
    let mut map: std::collections::HashMap<String, Spanned<Expr>> =
        std::collections::HashMap::new();
    for ((pname, _), arg) in subj_fd.params.iter().zip(call_args.iter()) {
        map.insert(pname.clone(), arg.clone());
    }
    let big_e = substitute(&hi_args[0], &map);
    let a_e = substitute(&m[1], &map);

    let render = |e: &Spanned<Expr>| super::super::expr::emit_expr_legacy(e, ctx, None);
    let a_lean = render(&a_e);
    let big_lean = render(&big_e);
    let lessthan = format!("{rat_prefix}.lessThan");

    // Premises: flatten the left-nested `Bool.and` tree.
    let when = law.when.as_ref()?;
    let mut conj: Vec<&Spanned<Expr>> = Vec::new();
    flatten_and(when, &mut conj);
    // h1 (scale-by-2 bound), h2 (envelope lessThan), hsd2 (M.bottom != 0),
    // placement (<=/>=). Exactly four conjuncts.
    if conj.len() != 4 {
        return None;
    }

    // h1 contributes (K, M_h1); h2 contributes (M_h2, SMALL); hsd2 the nonzero
    // guard on M; the placement is any `<=`/`>=`. Collected independently, then
    // cross-checked: the three M-renders (h1 scale, h2 envelope, hsd2 guard)
    // must agree, pinning the chain to a single magnitude.
    let mut from_h1: Option<(String, String)> = None; // (K, M)
    let mut from_h2: Option<(String, String)> = None; // (M, SMALL)
    let mut nonzero_m: Option<String> = None;
    let mut have_placement = false;

    for c in &conj {
        // h1: isNonNeg (minus (times K M) A).
        if let Some(nn1) = call_named(c, "isNonNeg", 1)
            && let Some(m1) = call_named(&nn1[0], "minus", 2)
            && let Some(t1) = call_named(&m1[0], "times", 2)
            && render(&m1[1]) == a_lean
        {
            let k_str = render(&t1[0]);
            // K must be the doubling constant 2/1 (the math of the reciprocal
            // envelope doubling — a constant, not a figure name).
            let kc: String = k_str.chars().filter(|ch| !ch.is_whitespace()).collect();
            if !(kc.contains("top:=2") && kc.contains("bottom:=1")) {
                return None;
            }
            from_h1 = Some((k_str, render(&t1[1])));
            continue;
        }
        // h2: lessThan (M, sgn SMALL).
        if let Some(lt) = call_named(c, "lessThan", 2)
            && let Some((s, sa)) = as_call(&lt[1])
            && s == sgn_short
            && sa.len() == 1
        {
            from_h2 = Some((render(&lt[0]), render(&sa[0])));
            continue;
        }
        // hsd2: M.bottom != 0.
        if let Expr::BinOp(crate::ast::BinOp::Neq, l, r) = &c.node
            && matches!(r.node, Expr::Literal(crate::ast::Literal::Int(0)))
            && let Expr::Attr(base, field) = &l.node
            && field == "bottom"
        {
            nonzero_m = Some(render(base));
            continue;
        }
        // placement: a `<=` / `>=` comparison (load-bearing exponent placement).
        if matches!(
            &c.node,
            Expr::BinOp(crate::ast::BinOp::Lte | crate::ast::BinOp::Gte, _, _)
        ) {
            have_placement = true;
            continue;
        }
        return None;
    }

    let (k_lean, m_h1) = from_h1?;
    let (m_h2, small_lean) = from_h2?;
    let nz = nonzero_m?;
    // The scaled magnitude (h1), the envelope subject (h2), and the nonzero
    // guard (hsd2) must all be the SAME M.
    if m_h1 != m_h2 || m_h1 != nz || !have_placement {
        return None;
    }
    let m_lean = m_h1;

    // Discover the cited monotonicity pool law over `sgn` (load-bearing final
    // step). The elementary doubling is self-emitted, not cited.
    let mono = find_monotone_law(ctx, &sgn_short)?;

    Some(FracOrderChain {
        a: a_lean,
        m: m_lean,
        k: k_lean,
        small: small_lean,
        big: big_lean,
        isnonneg,
        minus,
        times,
        lessthan,
        samevalue,
        sgn: aver_name_to_lean(&sgn_dotted),
        pow: aver_name_to_lean(&pow),
        mono_def: mono.def_lean,
        mono_thm: mono.theorem_lean,
        cited_deps: vec![(mono.prefix, mono.theorem_base)],
    })
}

/// Flatten a left-nested `Bool.and(Bool.and(…), w)` `when` tree into conjuncts.
fn flatten_and<'a>(expr: &'a Spanned<Expr>, out: &mut Vec<&'a Spanned<Expr>>) {
    if let Some(args) = call_named(expr, "and", 2) {
        flatten_and(&args[0], out);
        flatten_and(&args[1], out);
    } else {
        out.push(expr);
    }
}

/// Statement-builder hook: whether the chaining emit will close this law
/// universally (so the caller drops the sampled domain and classes it
/// `universal`, keeping statement and proof in lockstep).
pub(in crate::codegen::lean) fn recognize_frac_order_chain(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> bool {
    recognize_frac_order_chain_shape(vb, law, ctx).is_some()
}

/// The `(module_prefix, theorem_base)` pool laws the chaining arm cites for
/// this law — unioned into the cross-file admission set so the dep theorems
/// are actually emitted.
pub(in crate::codegen::lean) fn frac_order_chain_cited_deps(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Vec<(String, String)> {
    recognize_frac_order_chain_shape(vb, law, ctx)
        .map(|c| c.cited_deps)
        .unwrap_or_default()
}

/// Close a reciprocal-magnitude composition law. Emits the generic rational-
/// order helper kit plus the pow2-specific positivity/doubling support and the
/// fixed chain as a self-contained TRUE-universal theorem (`replaces_theorem`),
/// wrapped `first | (…) | sorry`.
pub(super) fn emit_frac_order_chain_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    theorem_base: &str,
    quant_params: &str,
) -> Option<AutoProof> {
    let c = recognize_frac_order_chain_shape(vb, law, ctx)?;
    let render = |e: &Spanned<Expr>| super::super::expr::emit_expr_legacy(e, ctx, None);
    let when = render(law.when.as_ref()?);
    // The conclusion subject call, exactly as the law writes it (its own args —
    // NOT every quantified given), and the subject fn Lean name (for the
    // `simp only [_root_.<subject>]` unfold).
    let lhs = render(&law.lhs);
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
    let text = render_chain(
        theorem_base,
        quant_params,
        &intros.join(" "),
        &when,
        &lhs,
        &subject,
        &c,
    );
    Some(AutoProof {
        support_lines: text.lines().map(|l| l.to_string()).collect(),
        body: crate::codegen::lean::tactic_ir::Tactic::raw(Vec::new()),
        replaces_theorem: true,
    })
}

/// The generic rational-order helper kit (verbatim transcription of the de-
/// risked kernel-checked rung) plus the pow2-specific support and the fixed
/// chain. Every helper is scoped to `{base}__` so two such laws in one file
/// never collide; every primitive / cited-law name is a parameter — no per-
/// figure literal beyond the mathematical doubling constant `2`.
#[allow(clippy::too_many_arguments)]
fn render_chain(
    base: &str,
    quant_params: &str,
    intros: &str,
    when: &str,
    lhs: &str,
    subject: &str,
    c: &FracOrderChain,
) -> String {
    let FracOrderChain {
        a,
        m,
        k,
        small,
        big,
        isnonneg,
        minus,
        times,
        lessthan,
        samevalue,
        sgn,
        pow,
        mono_def,
        mono_thm,
        ..
    } = c;
    let p = format!("{base}__");
    let kit = format!(
        r#"theorem {p}sq_nonneg (x : Int) : 0 ≤ x * x := by
  cases Int.le_total 0 x with
  | inl h => exact Int.mul_nonneg h h
  | inr h => exact Int.mul_nonneg_of_nonpos_of_nonpos h h
theorem {p}sq_pos {{x : Int}} (hx : x ≠ 0) : 0 < x * x := by
  cases Int.lt_or_gt_of_ne hx with
  | inl h => exact Int.mul_pos_of_neg_of_neg h h
  | inr h => exact Int.mul_pos h h
theorem {p}frac_le_trans (a b cc : Fraction) (hb : b.bottom ≠ 0)
    (hab : {isnonneg} ({minus} b a) = true) (hbc : {isnonneg} ({minus} cc b) = true) :
    {isnonneg} ({minus} cc a) = true := by
  simp only [{isnonneg}, {minus}, decide_eq_true_eq, ge_iff_le] at hab hbc ⊢
  have hP : 0 ≤ ((b.top*a.bottom - a.top*b.bottom) * (b.bottom*a.bottom)) * (cc.bottom*cc.bottom) :=
    Int.mul_nonneg hab ({p}sq_nonneg cc.bottom)
  have hQ : 0 ≤ ((cc.top*b.bottom - b.top*cc.bottom) * (cc.bottom*b.bottom)) * (a.bottom*a.bottom) :=
    Int.mul_nonneg hbc ({p}sq_nonneg a.bottom)
  have hid : ((b.top*a.bottom - a.top*b.bottom) * (b.bottom*a.bottom)) * (cc.bottom*cc.bottom)
              + ((cc.top*b.bottom - b.top*cc.bottom) * (cc.bottom*b.bottom)) * (a.bottom*a.bottom)
            = (b.bottom*b.bottom) * ((cc.top*a.bottom - a.top*cc.bottom) * (cc.bottom*a.bottom)) := by
    grind
  have hsum : 0 ≤ (b.bottom*b.bottom) * ((cc.top*a.bottom - a.top*cc.bottom) * (cc.bottom*a.bottom)) := by
    rw [← hid]; omega
  exact Int.nonneg_of_mul_nonneg_right hsum ({p}sq_pos hb)
theorem {p}frac_lt_imp_le (a b : Fraction) (h : {lessthan} a b = true) :
    {isnonneg} ({minus} b a) = true := by
  simp only [{lessthan}, {isnonneg}, {minus}, decide_eq_true_eq, ge_iff_le] at h ⊢
  have hid : (b.top*a.bottom - a.top*b.bottom) * (b.bottom*a.bottom)
           = (b.top*b.bottom)*(a.bottom*a.bottom) - (a.top*a.bottom)*(b.bottom*b.bottom) := by grind
  omega
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
theorem {p}frac_le_samevalue_right (a b b' : Fraction)
    (hb : b.bottom ≠ 0)
    (hbb' : {samevalue} b b' = true)
    (h : {isnonneg} ({minus} b a) = true) :
    {isnonneg} ({minus} b' a) = true := by
  simp only [{isnonneg}, {minus}, {samevalue}, decide_eq_true_eq, ge_iff_le, beq_iff_eq] at h hbb' ⊢
  have hid : ((b'.top*a.bottom - a.top*b'.bottom)*(b'.bottom*a.bottom)) * (b.bottom*b.bottom)
           = ((b.top*a.bottom - a.top*b.bottom)*(b.bottom*a.bottom)) * (b'.bottom*b'.bottom) := by
    grind
  have hrhs : 0 ≤ ((b.top*a.bottom - a.top*b.bottom)*(b.bottom*a.bottom)) * (b'.bottom*b'.bottom) :=
    Int.mul_nonneg h ({p}sq_nonneg b'.bottom)
  rw [← hid] at hrhs
  exact Int.nonneg_of_mul_nonneg_left hrhs ({p}sq_pos hb)
theorem {p}pow_of_nonpos (n : Int) (h : n <= 0) : {pow} n = 1 := by
  rw [{pow}.eq_def, if_pos h]
theorem {p}pow_of_pos (n : Int) (h : ¬n <= 0) : {pow} n = 2 * {pow} (n - 1) := by
  rw [{pow}.eq_def, if_neg h]
theorem {p}pow_pos : ∀ (n : Int), 1 <= {pow} n := by
  intro n
  induction n using {pow}.induct with
  | case1 n h => rw [{p}pow_of_nonpos n h]; omega
  | case2 n h ih => rw [{p}pow_of_pos n h]; omega
theorem {p}pow2Signed_bottom_pos (kk : Int) : 0 < ({sgn} kk).bottom := by
  unfold {sgn}
  split
  · show 0 < {pow} (0 - kk)
    have := {p}pow_pos (0 - kk); omega
  · show (0 : Int) < 1
    decide
theorem {p}pow2Signed_double (kk : Int) :
    {samevalue} ({times} ({k}) ({sgn} kk)) ({sgn} (kk+1)) = true := by
  rcases (by omega : kk < 0 ∨ 0 <= kk) with h0 | h0
  · rcases (by omega : kk + 1 < 0 ∨ kk = -1) with h1 | h1
    · simp only [{samevalue}, {times}, {sgn}, if_pos h0, if_pos h1, beq_iff_eq]
      have hd := {p}pow_of_pos (0 - kk) (by omega)
      rw [show (0 - kk) - 1 = 0 - (kk + 1) by omega] at hd
      rw [hd]; omega
    · subst h1
      simp only [{samevalue}, {times}, {sgn}, if_pos (show (-1:Int) < 0 by omega),
                 if_neg (show ¬ ((-1:Int) + 1 < 0) by omega), beq_iff_eq]
      have ea : {pow} (0 - -1) = 2 := by
        rw [show (0:Int) - -1 = 1 by omega]
        have h := {p}pow_of_pos 1 (by omega)
        rw [show (1:Int) - 1 = 0 by omega, {p}pow_of_nonpos 0 (by omega)] at h
        omega
      have eb : {pow} (-1 + 1) = 1 := by
        rw [show (-1:Int) + 1 = 0 by omega, {p}pow_of_nonpos 0 (by omega)]
      rw [ea, eb]; omega
  · have h0' : ¬ (kk < 0) := by omega
    have h1 : ¬ (kk + 1 < 0) := by omega
    simp only [{samevalue}, {times}, {sgn}, if_neg h0', if_neg h1, beq_iff_eq]
    have hd := {p}pow_of_pos (kk + 1) (by omega)
    rw [show (kk + 1) - 1 = kk by omega] at hd
    rw [hd]; omega"#
    );

    let assembly = format!(
        r#"set_option maxHeartbeats 4000000 in
theorem {base} : ∀ {quant_params}, {when} = true -> {lhs} = true := by
  intro {intros} h_when
  first
  | (simp only [Bool.and_eq_true, decide_eq_true_eq, bne_iff_ne, ne_eq] at h_when
     obtain ⟨⟨⟨h1, h2⟩, hsd2⟩, h3⟩ := h_when
     simp only [_root_.{subject}]
     have hA := {p}frac_lt_imp_le ({m}) ({sgn} ({small})) h2
     have hScaled := {p}frac_le_mul_pos ({k}) ({m}) ({sgn} ({small})) (by decide) (by decide) hA
     have hDbl := {p}pow2Signed_double ({small})
     have hb_ne : ({times} ({k}) ({sgn} ({small}))).bottom ≠ 0 := by
       simp only [{times}, Int.one_mul]; exact Int.ne_of_gt ({p}pow2Signed_bottom_pos ({small}))
     have link2 := {p}frac_le_samevalue_right ({times} ({k}) ({m})) ({times} ({k}) ({sgn} ({small}))) ({sgn} (({small})+1)) hb_ne hDbl hScaled
     have Lmid_ne : ({times} ({k}) ({m})).bottom ≠ 0 := by
       simp only [{times}, Int.one_mul]; exact hsd2
     have L := {p}frac_le_trans ({a}) ({times} ({k}) ({m})) ({sgn} (({small})+1)) Lmid_ne h1 link2
     have Mmono := {mono_thm} (({small})+1) ({big}) (by first
         | omega
         | (simp only [eq_iff_iff, iff_true]; omega)
         | (simp only [decide_eq_true_eq]; omega))
     simp only [{mono_def}] at Mmono
     have Mmid_ne : ({sgn} (({small})+1)).bottom ≠ 0 := Int.ne_of_gt ({p}pow2Signed_bottom_pos (({small})+1))
     exact {p}frac_le_trans ({a}) ({sgn} (({small})+1)) ({sgn} ({big})) Mmid_ne L Mmono)
  | sorry"#
    );

    format!("{kit}\n{assembly}")
}

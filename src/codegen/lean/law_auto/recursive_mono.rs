//! GENERAL recursive-monotonicity rung — the name-blind, structurally-keyed
//! core of the signed-power-of-two order family. It has two parts:
//!
//! 1. [`render_recursive_mono_kit`] — the SHARED helper kit for ANY pure
//!    recursive `Int -> Int` fn `f` whose body is `if p <= 0 then BASE else
//!    STEP` (BASE an integer literal, STEP the one recursive step). It emits the
//!    def equations (`{base}__rec_lo` / `{base}__rec_hi`), positivity
//!    `BASE <= f n` (`{base}__rec_pos`), monotonicity `m <= n -> f m <= f n`
//!    (`{base}__rec_mono`), and a product helper (`{base}__rec_one_le_mul`),
//!    proved on `f.induct` with a small PORTFOLIO closer that covers the
//!    additive (`n + f(n-1)`), doubling (`2 * f(n-1)`) AND multiplicative
//!    (`n * f(n-1)`) steps: `omega` discharges the linear ones, the
//!    `Int.mul_le_mul_of_nonneg_right` / `one_le_mul` arms the multiplicative
//!    one. Every leaf is floored `first | (proof) | sorry`, so a fn that is NOT
//!    actually monotone/nonneg degrades to a `sorry`-tainted kit the `#print
//!    axioms` whitelist rejects — never a false theorem. The kit is keyed only
//!    on `f` having an auto `.induct` principle (i.e. being recursive); it knows
//!    nothing about pow2 or any specific function.
//!
//! 2. The DIRECT positivity rung ([`emit_recursive_positive_law`]) — a
//!    `holds` law whose claim is `f(ARG) >= BASE` or `BASE <= f(ARG)`, for any
//!    such recursive `f`. It emits the shared kit and closes by citing the
//!    kit's positivity theorem. This is intentionally BASE-blind: the literal
//!    lower bound is captured from the law and must equal the recursive base arm.
//!
//! 3. The DIRECT order-law rung ([`emit_recursive_monotone_law`]) — a
//!    conditional `holds` law whose conclusion is the plain integer order
//!    `f(LO) <= f(HI)` (the subject fn body is `f(LO) <= f(HI)`, or the law
//!    claim is that comparison directly) under a premise `LO <= HI`, for any
//!    such recursive `f`. It emits the shared kit and closes by citing the
//!    kit's monotonicity. This is the rung that fires GENUINELY UNIVERSALLY on a
//!    second, non-pow2 function (e.g. a triangular-number `tri n = n + tri(n-1)`
//!    monotonicity law) with no pow2-specific gating.
//!
//! The Fraction-order monotonicity of a `Fraction`-valued cone fn `F`
//! (`isNonNeg(minus(F HI, F LO))`) is NOT handled here: it is closed generically
//! by `frac_monotone_compose.rs`, which treats `F` as OPAQUE and cites `F`'s
//! homomorphism / positivity / `>= 1` pool laws — the recursion is abstracted
//! behind those laws, not re-derived. This rung owns only the PLAIN integer order
//! `f(LO) <= f(HI)` (the `tri` / `fct` family), where the recursion is exposed.

use super::AutoProof;
use super::aver_name_to_lean;
use super::shared::{expr_dotted_name, find_fn_def_by_call_name, substitute_expr};
use crate::ast::{BinOp, Expr, FnDef, Literal, Pattern, Spanned, Stmt, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;

/// The five helper-theorem names a kit emits, all scoped to `{base}__rec_` so
/// two kits in one file never collide.
pub(super) struct RecKit {
    /// The full helper-kit Lean source (the five theorems).
    pub text: String,
    /// `{base}__rec_pos` — `∀ n, BASE <= f n`.
    pub pos: String,
    /// `{base}__rec_mono` — `∀ n m, m <= n -> f m <= f n`.
    pub mono: String,
}

struct RecArms {
    base_int: i64,
    base_arm: String,
    step_arm: String,
}

/// Whether `fd` is a `p <= 0`-guarded single-step recursion on a lone `Int`
/// param returning `Int` — `fn f(p: Int) -> Int = match p <= 0 { true -> BASE;
/// false -> STEP }` with BASE an integer literal — and, if so, the BASE and STEP
/// arms rendered to Lean with the param canonicalized to `n` (so they slot into
/// the kit's `(n : Int)` binders). Keyed purely on this structure; any other
/// shape is declined.
fn recursive_decrement_arm_info(fd: &FnDef, ctx: &CodegenContext) -> Option<RecArms> {
    let [(p, ty)] = fd.params.as_slice() else {
        return None;
    };
    if ty.trim() != "Int" || fd.return_type.trim() != "Int" {
        return None;
    }
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return None;
    };
    let Expr::Match { subject, arms } = &body.node else {
        return None;
    };
    // Subject must be `p <= 0` so the compiled `if p <= 0 then … else …` lets the
    // kit's `if_pos h` / `if_neg h` (with `h : p <= 0`) reduce each arm.
    let Expr::BinOp(BinOp::Lte, sl, sr) = &subject.node else {
        return None;
    };
    if expr_dotted_name(sl).as_deref() != Some(p.as_str())
        || !matches!(&sr.node, Expr::Literal(Literal::Int(0)))
        || arms.len() != 2
    {
        return None;
    }
    // Pick the `true ->` (BASE) and `false ->` (STEP) arms.
    let arm_for = |b: bool| {
        arms.iter().find_map(|a| {
            matches!(&a.pattern, Pattern::Literal(Literal::Bool(v)) if *v == b).then_some(&a.body)
        })
    };
    let base = arm_for(true)?;
    let step = arm_for(false)?;
    // BASE must be a bare integer literal — the kit treats it as the constant
    // lower bound the step preserves; a param-dependent base is declined.
    let Expr::Literal(Literal::Int(base_int)) = &base.node else {
        return None;
    };
    // Render both arms with the param canonicalized to `n`.
    let n_ident = Spanned::bare(Expr::Ident("n".to_string()));
    let mut map = std::collections::HashMap::new();
    map.insert(p.as_str(), &n_ident);
    let render = |e: &Spanned<Expr>| {
        let subbed = substitute_expr(e, &map);
        super::super::expr::emit_expr(
            &super::super::expr::resolve_rewrite_output(&subbed, ctx, None),
            ctx,
        )
    };
    Some(RecArms {
        base_int: *base_int,
        base_arm: render(base),
        step_arm: render(step),
    })
}

pub(super) fn recursive_decrement_arms(
    fd: &FnDef,
    ctx: &CodegenContext,
) -> Option<(String, String)> {
    let arms = recursive_decrement_arm_info(fd, ctx)?;
    Some((arms.base_arm, arms.step_arm))
}

/// Emit the SHARED recursive-monotonicity helper kit for `f_lean` (Lean name)
/// with the rendered `base_arm` / `step_arm`. Transcription of the de-risked,
/// kernel-checked skeletons (core Lean, no Mathlib): the def equations,
/// positivity, monotonicity (no `0 <= m` premise — clamping below zero keeps `f`
/// non-decreasing), and a product helper, with the additive/doubling/
/// multiplicative PORTFOLIO closer. Every leaf is floored `first | … | sorry`.
pub(super) fn render_recursive_mono_kit(
    base: &str,
    f_lean: &str,
    base_arm: &str,
    step_arm: &str,
) -> RecKit {
    let text = format!(
        r#"theorem {base}__rec_lo (n : Int) (h : n <= 0) : {f} n = {base_arm} := by
  first | (rw [{f}.eq_def, if_pos h]) | sorry
theorem {base}__rec_hi (n : Int) (h : ¬n <= 0) : {f} n = {step_arm} := by
  first | (rw [{f}.eq_def, if_neg h]) | sorry
theorem {base}__rec_one_le_mul (a b : Int) (ha : 1 <= a) (hb : 1 <= b) : 1 <= a * b := by
  calc (1 : Int) = 1 * 1 := by omega
    _ <= a * b := Int.mul_le_mul ha hb (by omega) (by omega)
theorem {base}__rec_pos : ∀ (n : Int), {base_arm} <= {f} n := by
  intro n
  induction n using {f}.induct with
  | case1 n h => rw [{base}__rec_lo n h]; first | omega | (exact {base}__rec_one_le_mul _ _ (by omega) ih) | (apply Int.mul_nonneg <;> first | omega | assumption) | sorry
  | case2 n h ih => rw [{base}__rec_hi n h]; first | omega | (exact {base}__rec_one_le_mul _ _ (by omega) ih) | (apply Int.mul_nonneg <;> first | omega | assumption) | sorry
theorem {base}__rec_mono : ∀ (n m : Int), m <= n -> {f} m <= {f} n := by
  intro n
  induction n using {f}.induct with
  | case1 n h =>
      intro m hm
      first | (rw [{base}__rec_lo n h, {base}__rec_lo m (by omega)]; omega) | sorry
  | case2 n h ih =>
      intro m hm
      rw [{base}__rec_hi n h]
      by_cases hmn : m <= n - 1
      · have hle := ih m hmn
        have hp := {base}__rec_pos (n - 1)
        first
        | omega
        | (calc {f} m <= {f} (n - 1) := hle
             _ = 1 * {f} (n - 1) := by omega
             _ <= n * {f} (n - 1) := Int.mul_le_mul_of_nonneg_right (by omega) (by omega))
        | sorry
      · first
        | (have hmeq : m = n := by omega
           rw [hmeq, {base}__rec_hi n h]; omega)
        | sorry"#,
        f = f_lean,
    );
    RecKit {
        text,
        pos: format!("{base}__rec_pos"),
        mono: format!("{base}__rec_mono"),
    }
}

/// The recognized direct positivity-law shape: `BASE <= f(ARG)`, possibly
/// written as `f(ARG) >= BASE`.
pub(super) struct RecPositive {
    /// The recursive `Int -> Int` fn (source name).
    f_src: String,
    /// The single argument supplied to the recursive fn.
    arg: Spanned<Expr>,
}

fn int_literal(e: &Spanned<Expr>) -> Option<i64> {
    match &e.node {
        Expr::Literal(Literal::Int(n)) => Some(*n),
        _ => None,
    }
}

fn subject_call(e: &Spanned<Expr>) -> Option<(String, Spanned<Expr>)> {
    let Expr::FnCall(callee, args) = &e.node else {
        return None;
    };
    if args.len() != 1 {
        return None;
    }
    Some((expr_dotted_name(callee)?, args[0].clone()))
}

fn as_positive_comparison(cmp: &Spanned<Expr>) -> Option<(String, Spanned<Expr>, i64)> {
    match &cmp.node {
        Expr::BinOp(BinOp::Gte, l, r) => {
            let (f_src, arg) = subject_call(l)?;
            Some((f_src, arg, int_literal(r)?))
        }
        Expr::BinOp(BinOp::Lte, l, r) => {
            let (f_src, arg) = subject_call(r)?;
            Some((f_src, arg, int_literal(l)?))
        }
        _ => None,
    }
}

/// Recognize a direct integer-positivity law: `f(ARG) >= BASE` or
/// `BASE <= f(ARG)`, for a pure recursive `Int -> Int` `f` whose guarded base
/// arm is exactly the literal `BASE`. Shape-only: no power-of-two recognizer,
/// no multiplier/base constants beyond the literal captured from the AST.
pub(super) fn recognize_recursive_positive_shape(
    _vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Option<RecPositive> {
    if !matches!(law.rhs.node, Expr::Literal(Literal::Bool(true))) {
        return None;
    }
    let (f_src, arg, lower_bound) = as_positive_comparison(&law.lhs)?;

    let short = f_src.rsplit('.').next().unwrap_or(&f_src).to_string();
    if !crate::codegen::lean::recursive_pure_fn_names(ctx).contains(&short) {
        return None;
    }
    let f_fd = find_fn_def_by_call_name(ctx, &f_src)?;
    if !f_fd.effects.is_empty() {
        return None;
    }
    let arms = recursive_decrement_arm_info(f_fd, ctx)?;
    if arms.base_int != lower_bound {
        return None;
    }

    Some(RecPositive { f_src, arg })
}

/// Statement-builder hook: whether the direct recursive-positivity emit will
/// close this law universally.
pub(in crate::codegen::lean) fn recognize_recursive_positive(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> bool {
    recognize_recursive_positive_shape(vb, law, ctx).is_some()
}

/// Close a direct integer-positivity law. Emits the shared recursive-mono kit
/// for the inner fn plus a citation of its positivity lemma.
pub(super) fn emit_recursive_positive_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    theorem_base: &str,
    quant_params: &str,
) -> Option<AutoProof> {
    let shape = recognize_recursive_positive_shape(vb, law, ctx)?;
    let f_fd = find_fn_def_by_call_name(ctx, &shape.f_src)?;
    let arms = recursive_decrement_arm_info(f_fd, ctx)?;
    let f_lean = aver_name_to_lean(&shape.f_src);
    let kit_base = aver_name_to_lean(shape.f_src.rsplit('.').next().unwrap_or(&shape.f_src));
    let kit = render_recursive_mono_kit(&kit_base, &f_lean, &arms.base_arm, &arms.step_arm);

    let render = |e: &Spanned<Expr>| {
        super::super::expr::emit_expr(
            &super::super::expr::resolve_rewrite_output(e, ctx, None),
            ctx,
        )
    };
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
    let theorem = if let Some(when) = law.when.as_ref() {
        let when = render(when);
        format!(
            r#"theorem {base} : ∀ {quant}, {when} = true -> {lhs} = {rhs} := by
{intro_line}  intro _h_when
  have hpos := {pos} ({arg})
  simpa only [ge_iff_le, eq_iff_iff, iff_true] using hpos"#,
            base = theorem_base,
            quant = quant_params,
            pos = kit.pos,
        )
    } else {
        format!(
            r#"theorem {base} : ∀ {quant}, {lhs} = {rhs} := by
{intro_line}  have hpos := {pos} ({arg})
  simpa only [ge_iff_le, eq_iff_iff, iff_true] using hpos"#,
            base = theorem_base,
            quant = quant_params,
            pos = kit.pos,
        )
    };

    let text = format!("{}\n{}", kit.text, theorem);
    Some(AutoProof {
        support_lines: text.lines().map(|l| l.to_string()).collect(),
        body: crate::codegen::lean::tactic_ir::Tactic::raw(Vec::new()),
        replaces_theorem: true,
    })
}

/// The recognized direct order-law shape: the inner recursive fn, the two
/// exponent expressions (`lo <= hi` side), and whether the comparison sits
/// inside a Bool subject fn that must be unfolded.
pub(super) struct RecMonotone {
    /// The recursive `Int -> Int` fn (source name).
    f_src: String,
    /// `LO`: the smaller side (first `<=` operand's argument), in law givens.
    lo: Spanned<Expr>,
    /// `HI`: the larger side (second `<=` operand's argument), in law givens.
    hi: Spanned<Expr>,
    /// The Bool subject fn (dotted source name) to unfold, if the comparison is
    /// its body (vs. the law claim being the comparison directly).
    subject: Option<String>,
}

/// `(f, lo_arg, hi_arg)` when `cmp` is `f(LO) <= f(HI)` for a single fn `f`
/// applied to one argument on each side — the plain integer monotonicity
/// conclusion. Name-blind on `f`.
fn as_mono_comparison(cmp: &Spanned<Expr>) -> Option<(String, Spanned<Expr>, Spanned<Expr>)> {
    let Expr::BinOp(BinOp::Lte, l, r) = &cmp.node else {
        return None;
    };
    let Expr::FnCall(lc, la) = &l.node else {
        return None;
    };
    let Expr::FnCall(rc, ra) = &r.node else {
        return None;
    };
    let (lf, rf) = (expr_dotted_name(lc)?, expr_dotted_name(rc)?);
    if lf != rf || la.len() != 1 || ra.len() != 1 {
        return None;
    }
    Some((lf, la[0].clone(), ra[0].clone()))
}

/// Recognize a conditional integer-monotonicity law: `f(LO) <= f(HI)` (either
/// the law claim directly, or the body of a Bool subject fn it calls) under a
/// premise `LO <= HI`, for a pure recursive `Int -> Int` `f` with a `p <= 0`
/// single-step recursion. Pure / name-blind — keyed only on `f` being recursive
/// (so `f.induct` exists) and the order shape. Declines (so the law keeps its
/// bounded sampled fallback / another rung) unless every structural gate holds.
pub(super) fn recognize_recursive_monotone_shape(
    _vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Option<RecMonotone> {
    law.when.as_ref()?;
    if !matches!(law.rhs.node, Expr::Literal(Literal::Bool(true))) {
        return None;
    }
    // The comparison either IS the law claim, or is the body of a Bool subject
    // fn the claim calls (the `…Monotone(args) holds` form, mirroring how the
    // project states monotonicity laws).
    let (f_src, lo, hi, subject) = if let Some((f, lo, hi)) = as_mono_comparison(&law.lhs) {
        (f, lo, hi, None)
    } else {
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
        let [Stmt::Expr(body)] = subj_fd.body.stmts() else {
            return None;
        };
        let (f, a, b) = as_mono_comparison(body)?;
        // Express the two exponents in the law's givens by substituting the
        // subject's params with the call's argument terms.
        let mut map = std::collections::HashMap::new();
        for ((pname, _), arg) in subj_fd.params.iter().zip(call_args.iter()) {
            map.insert(pname.as_str(), arg);
        }
        (
            f,
            substitute_expr(&a, &map),
            substitute_expr(&b, &map),
            Some(subject_src),
        )
    };

    // `f` must be a pure recursive `Int -> Int` single-step recursion (so its
    // `.induct` exists and the kit's def-equation rewrites reduce).
    let short = f_src.rsplit('.').next().unwrap_or(&f_src).to_string();
    if !crate::codegen::lean::recursive_pure_fn_names(ctx).contains(&short) {
        return None;
    }
    let f_fd = find_fn_def_by_call_name(ctx, &f_src)?;
    recursive_decrement_arms(f_fd, ctx)?;

    // The premise must establish `LO <= HI` (small side `LO`, big side `HI`) —
    // load-bearing for monotonicity. Compare each side to the recognized
    // exponents through the SAME emitter, so a structurally equal premise
    // matches and a reversed / unrelated one is declined.
    let when = law.when.as_ref()?;
    let render = |e: &Spanned<Expr>| {
        super::super::expr::emit_expr(
            &super::super::expr::resolve_rewrite_output(e, ctx, None),
            ctx,
        )
    };
    let (lo_lean, hi_lean) = (render(&lo), render(&hi));
    let premise_ok = match &when.node {
        Expr::BinOp(BinOp::Lte, l, r) => render(l) == lo_lean && render(r) == hi_lean,
        Expr::BinOp(BinOp::Gte, l, r) => render(l) == hi_lean && render(r) == lo_lean,
        _ => false,
    };
    if !premise_ok {
        return None;
    }

    Some(RecMonotone {
        f_src,
        lo,
        hi,
        subject,
    })
}

/// Statement-builder hook: whether the direct monotonicity emit will close this
/// law universally (so the caller drops the sampled domain and classes it
/// `universal`, keeping statement and proof in lockstep).
pub(in crate::codegen::lean) fn recognize_recursive_monotone(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> bool {
    recognize_recursive_monotone_shape(vb, law, ctx).is_some()
}

/// Close a direct integer-monotonicity law. Emits the shared recursive-mono kit
/// for the inner fn plus a one-line citation of its monotonicity, as a
/// self-contained TRUE-universal theorem (`replaces_theorem`) under a
/// `first | (…) | sorry` floor.
pub(super) fn emit_recursive_monotone_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    theorem_base: &str,
    quant_params: &str,
) -> Option<AutoProof> {
    let shape = recognize_recursive_monotone_shape(vb, law, ctx)?;
    let f_fd = find_fn_def_by_call_name(ctx, &shape.f_src)?;
    let (base_arm, step_arm) = recursive_decrement_arms(f_fd, ctx)?;
    let f_lean = aver_name_to_lean(&shape.f_src);
    let kit = render_recursive_mono_kit(theorem_base, &f_lean, &base_arm, &step_arm);

    let render = |e: &Spanned<Expr>| {
        super::super::expr::emit_expr(
            &super::super::expr::resolve_rewrite_output(e, ctx, None),
            ctx,
        )
    };
    let lhs = render(&law.lhs);
    let rhs = render(&law.rhs);
    let when = render(law.when.as_ref()?);
    let hi = render(&shape.hi);
    let lo = render(&shape.lo);
    let intros: Vec<String> = law
        .givens
        .iter()
        .map(|g| aver_name_to_lean(&g.name))
        .collect();
    // Unfold the Bool subject fn (if any) before stripping the `decide`.
    let unfold = match &shape.subject {
        Some(s) => format!("{}, ", aver_name_to_lean(s)),
        None => String::new(),
    };

    let assembly = format!(
        r#"theorem {base} : ∀ {quant}, {when} = true -> {lhs} = {rhs} := by
  intro {intros} h_when
  first
  | (simp only [eq_iff_iff, iff_true] at h_when
     simp only [{unfold}decide_eq_true_eq]
     exact {mono} ({hi}) ({lo}) h_when)
  | sorry"#,
        base = theorem_base,
        quant = quant_params,
        intros = intros.join(" "),
        mono = kit.mono,
    );

    let text = format!("{}\n{}", kit.text, assembly);
    Some(AutoProof {
        support_lines: text.lines().map(|l| l.to_string()).collect(),
        body: crate::codegen::lean::tactic_ir::Tactic::raw(Vec::new()),
        replaces_theorem: true,
    })
}

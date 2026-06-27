//! Rational-over-floor sign / magnitude family (Lemma 7.2.2 sign half).
//!
//! Split out of `induction.rs` with no behavior change; see `mod.rs`. The
//! dedicated cite-window + power-of-two positivity + `aver_int_order` / sign-split
//! arm that the keystone emitter delegates to for the rational floor shapes.

use super::super::super::expr::aver_name_to_lean;
use super::super::super::tactic_ir::Tactic;
use super::super::AutoProof;
use super::super::shared::law_simp_defs;
use super::keystone::{
    arith_eq, expr_var_name, inline_fn_call, keystone_pow2_fn, signed_pow2_shape, substitute_idents,
};
use crate::ast::{VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;

// ===========================================================================
// Rational-over-floor sign / magnitude family (Lemma 7.2.2, p.19 sign half).
//
// A `when`-law whose subject reduces to either a comparison-against-zero
// (a nonnegativity / strict-positivity magnitude fact) or a `match` on a Bool
// predicate (the sign split), AND whose call cone reaches a power-of-two fn.
// The keystone arm here is a GENERIC, name-blind skeleton:
//   * it CITES earlier sibling `holds` laws whose subject is a single
//     order comparison or a `Bool.and` of comparisons (the floor window, the
//     magnitude/positivity helper laws) over a prefix of this law's givens,
//     discharging each cited law's premise by projecting this law's `when`;
//   * it supplies power-of-two positivity `have`s (shape-keyed on the cone's
//     `pow2` fn) at the record-field exponents, so the nonlinear closer can
//     discharge the power-of-two factor leaves;
//   * the nonneg/pos shape closes with `aver_int_order` (the same nonlinear
//     primitive the Newton-Raphson bounds use); the sign shape `rcases` the
//     predicate's leading disjunction (the float sign) and `grind`s each arm
//     against the cited magnitude/positivity facts.
// The whole arm sits inside the keystone's `first | … | sorry` floor, so any
// citation/discharge that does not typecheck falls to the honest floor and the
// law degrades to its bounded sampled statement — credit stays fail-closed.
// The algebraic content (the magnitude product, the value-magnitude product,
// the window bounds) lives ENTIRELY in the cited Aver laws; deleting any of
// them breaks the proof.
// ===========================================================================

#[derive(Clone, Copy, PartialEq)]
pub(super) enum RationalFloorShape {
    /// subject body is `E >= 0` / `0 <= E` / `E > 0` / `0 < E`.
    NonnegPos,
    /// subject body is `match <pred-call> { true -> …; false -> … }`.
    Sign,
    /// subject body is the rational-order truncation-error bound
    /// `lessThan(absFraction(minus(fpValue(F), fpValue(G))), pow2Signed(LE))`
    /// (Lemma 7.2.2's strict bound). `G` is the rounded value of `F`; the
    /// bound arm factors the error magnitude, splits the `pow2Signed` sign
    /// branch, CITES the floor window + power-of-two homomorphism pool laws,
    /// and multiplies the cited window remainder bound by the positive product
    /// factor (the generic multiply-by-positive rung).
    Bound,
}

/// The power-of-two fn (Lean name) reachable in this law's cone, if any. Tries
/// the keystone's cited-pool detector first (same-module homomorphism cone),
/// then falls back to the `pow_fn` of ANY `FloorDivWindow`-figured law in the
/// module — the floor-window family pins that figure only on an `is_pow2_shape`
/// fn, so this is name-blind. Guarded to a `pow` that actually appears in this
/// law's cone, so an unrelated module's `pow` never leaks in.
fn rf_pow2_fn(vb: &VerifyBlock, law: &VerifyLaw, ctx: &CodegenContext) -> Option<String> {
    use crate::ast::{TopLevel, VerifyKind};
    use crate::ir::{FloorWindowFigure, ProofStrategy};
    let cone = super::super::shared::law_simp_source_names(ctx, vb, law);
    if let Some(p) = keystone_pow2_fn(vb, law, ctx) {
        return Some(p);
    }
    for item in &ctx.items {
        let TopLevel::Verify(prev) = item else {
            continue;
        };
        let VerifyKind::Law(prev_law) = &prev.kind else {
            continue;
        };
        if let Some(ProofStrategy::FloorDivWindow { figure }) =
            super::super::law_strategy_for(ctx, &prev.fn_name, &prev_law.name)
        {
            let pow = match figure {
                FloorWindowFigure::PowPositive { pow_fn }
                | FloorWindowFigure::PowSumSplit { pow_fn }
                | FloorWindowFigure::SigWindow { pow_fn, .. }
                | FloorWindowFigure::ProductWindow { pow_fn, .. }
                | FloorWindowFigure::FloorPow2Window { pow_fn, .. }
                | FloorWindowFigure::FloorPow2Cancel { pow_fn, .. } => pow_fn,
            };
            let pbare = rf_bare_basename(&pow);
            if cone.iter().any(|c| rf_bare_basename(c) == pbare) {
                return Some(aver_name_to_lean(&pow));
            }
        }
    }
    None
}

/// Recognize the rational-over-floor sign/magnitude family on `(vb, law)`.
/// Name-blind: keyed on the inlined subject body shape plus the presence of a
/// power-of-two fn in the cone (via [`rf_pow2_fn`]). Returns `None` (decline)
/// for every other keystone law, so their emission stays byte-identical.
pub(super) fn rational_floor_shape(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Option<RationalFloorShape> {
    use crate::ast::{BinOp, Expr, Literal};
    rf_pow2_fn(vb, law, ctx)?;
    let inlined = inline_fn_call(&law.lhs, ctx)?;
    let is_zero = |e: &Expr| matches!(e, Expr::Literal(Literal::Int(0)));
    // The rational-order bound `lessThan(absFraction(_), pow2Signed(_))`: a
    // `lessThan` whose left arg is an `absFraction` and right arg a `pow2Signed`.
    // Name-blind (basename match), keyed on the call shape; the `Bound` arm
    // below extracts the float / precision givens and the rounded value from it.
    if let Expr::FnCall(callee, args) = &inlined.node
        && rf_callee_basename(callee).as_deref() == Some("lessThan")
        && args.len() == 2
        && matches!(&args[0].node, Expr::FnCall(c, a)
            if rf_callee_basename(c).as_deref() == Some("absFraction") && a.len() == 1)
        && matches!(&args[1].node, Expr::FnCall(c, a)
            if rf_callee_basename(c).as_deref() == Some("pow2Signed") && a.len() == 1)
    {
        return Some(RationalFloorShape::Bound);
    }
    match &inlined.node {
        Expr::BinOp(BinOp::Gte | BinOp::Gt, _l, r) if is_zero(&r.node) => {
            Some(RationalFloorShape::NonnegPos)
        }
        Expr::BinOp(BinOp::Lte | BinOp::Lt, l, _r) if is_zero(&l.node) => {
            Some(RationalFloorShape::NonnegPos)
        }
        Expr::Match { subject, arms }
            if matches!(&subject.node, Expr::FnCall(..)) && arms.len() == 2 =>
        {
            Some(RationalFloorShape::Sign)
        }
        _ => None,
    }
}

/// Basename of a call's callee (last dotted component), or `None` if the callee
/// is not a name.
fn rf_callee_basename(callee: &crate::ast::Spanned<crate::ast::Expr>) -> Option<String> {
    super::super::shared::expr_dotted_name(callee).map(|n| rf_bare_basename(&n).to_string())
}

/// Flatten a left-folded `Bool.and(a, b)` premise (how the parser composes
/// multiple `when` lines) into its atomic conjuncts.
fn rf_flatten_bool_and(
    e: &crate::ast::Spanned<crate::ast::Expr>,
) -> Vec<&crate::ast::Spanned<crate::ast::Expr>> {
    use crate::ast::Expr;
    if let Expr::FnCall(callee, args) = &e.node
        && super::super::shared::expr_dotted_name(callee).as_deref() == Some("Bool.and")
        && args.len() == 2
    {
        let mut out = rf_flatten_bool_and(&args[0]);
        out.extend(rf_flatten_bool_and(&args[1]));
        return out;
    }
    vec![e]
}

/// A Lean proof term for `<cited_when> = true`, projected out of this law's
/// `h_when` (the citing law's composite premise). `None` when the cited premise
/// is not one of the citing law's conjuncts (then the citation is skipped — the
/// arm keeps its honest floor).
fn rf_premise_proof(
    citing_when: &crate::ast::Spanned<crate::ast::Expr>,
    cited_when: &crate::ast::Spanned<crate::ast::Expr>,
) -> Option<String> {
    let conjs = rf_flatten_bool_and(citing_when);
    let n = conjs.len();
    let idx = conjs
        .iter()
        .position(|c| arith_eq(&c.node, &cited_when.node))?;
    if n == 1 {
        return Some("h_when".to_string());
    }
    // `simp only [Bool.and_eq_true] at h_when` turns `(c0 && … && c_{n-1}) = true`
    // into the LEFT-nested `((c0=true ∧ c1=true) ∧ …) ∧ c_{n-1}=true`. The flat
    // conjunct at `idx` is reached by `.1` down the left spine then `.2`
    // (`c0` is the whole left spine, `.1`×(n-1)). A `by`-block keeps the `at
    // h_when` mutation local to this premise's elaboration. The omega arms are a
    // fallback for a numeric Bool conjunct whose standalone elaboration differs
    // from the composed `decide (…)` form (projection then fails the type-check).
    let path = if idx == 0 {
        ".1".repeat(n - 1)
    } else if idx == n - 1 {
        ".2".to_string()
    } else {
        format!("{}.2", ".1".repeat(n - 1 - idx))
    };
    Some(format!(
        "(by first | (simp only [Bool.and_eq_true] at h_when; exact h_when{path}) | (simp only [Bool.and_eq_true, decide_eq_true_eq] at h_when ⊢ <;> omega) | (simp only [Bool.and_eq_true] at h_when; omega))"
    ))
}

/// Whether an expression's head is a `Bool.or` (directly, or as the first
/// conjunct of a `Bool.and`) — the shape of a sign/format predicate's body
/// (`isFp`: `(sign == 1 || sign == -1) && …`), so its first projection is the
/// sign disjunction to `rcases`.
fn rf_starts_with_or(e: &crate::ast::Expr) -> bool {
    use crate::ast::Expr;
    if let Expr::FnCall(callee, args) = e {
        let name = super::super::shared::expr_dotted_name(callee);
        if name.as_deref() == Some("Bool.or") {
            return true;
        }
        if name.as_deref() == Some("Bool.and")
            && let Some(first) = args.first()
        {
            return rf_starts_with_or(&first.node);
        }
    }
    false
}

/// Resolve a (possibly module-qualified) fn name to its def, searching the entry
/// module and every dependency module by basename — `ctx.fn_def_by_name` does
/// not resolve a dep fn by its qualified call name in the consumer scope.
fn rf_resolve_fn<'a>(ctx: &'a CodegenContext, dotted: &str) -> Option<&'a crate::ast::FnDef> {
    if let Some(fd) = ctx.fn_def_by_name(dotted, ctx.active_module_scope().as_deref()) {
        return Some(fd);
    }
    let bare = dotted.rsplit('.').next().unwrap_or(dotted);
    ctx.fn_defs.iter().find(|fd| fd.name == bare).or_else(|| {
        ctx.modules
            .iter()
            .flat_map(|m| m.fn_defs.iter())
            .find(|fd| fd.name == bare)
    })
}

/// The `when`-conjunct that is the sign/format predicate (a fn call whose body
/// leads with a disjunction), if any — the conjunct `rcases` splits on.
fn rf_sign_conjunct<'a>(
    law: &'a VerifyLaw,
    ctx: &CodegenContext,
) -> Option<&'a crate::ast::Spanned<crate::ast::Expr>> {
    use crate::ast::Expr;
    let when = law.when.as_ref()?;
    rf_flatten_bool_and(when).into_iter().find(|c| {
        let Expr::FnCall(callee, _) = &c.node else {
            return false;
        };
        super::super::shared::expr_dotted_name(callee)
            .and_then(|name| rf_resolve_fn(ctx, &name))
            .and_then(|fd| fd.body.tail_expr())
            .is_some_and(|body| rf_starts_with_or(&body.node))
    })
}

/// Whether the named fn is the Euclidean floor wrapper
/// (`Result.withDefault(Int.div(…), <lit>)`) — excluded from the simp unfold
/// set so the floor term stays an opaque atom shared between the goal and the
/// cited window bound (the consistency the nonlinear closer relies on).
fn rf_is_floordiv_wrapper(ctx: &CodegenContext, bare: &str) -> bool {
    use crate::ast::Expr;
    let Some(fd) = ctx.fn_def_by_name(bare, ctx.active_module_scope().as_deref()) else {
        return false;
    };
    let Some(body) = fd.body.tail_expr() else {
        return false;
    };
    let Expr::FnCall(callee, args) = &body.node else {
        return false;
    };
    super::super::shared::expr_dotted_name(callee).as_deref() == Some("Result.withDefault")
        && args.len() == 2
}

pub(super) fn rf_bare_basename(n: &str) -> &str {
    let n = n.strip_prefix("_root_.").unwrap_or(n);
    n.rsplit('.').next().unwrap_or(n)
}

/// The cone simp set for a law, EXCLUDING the recursive power-of-two fn and the
/// Euclidean floor wrapper (both must stay opaque atoms). Used both for the
/// citing goal and for unfolding each cited hypothesis.
fn rf_filtered_defs(ctx: &CodegenContext, vb: &VerifyBlock, law: &VerifyLaw) -> Vec<String> {
    let recursive: std::collections::HashSet<String> = super::super::recursive_pure_fn_names(ctx)
        .iter()
        .map(|n| rf_bare_basename(&aver_name_to_lean(n)).to_string())
        .collect();
    law_simp_defs(ctx, vb, law)
        .into_iter()
        .filter(|d| {
            let b = rf_bare_basename(d);
            !recursive.contains(b) && !rf_is_floordiv_wrapper(ctx, b)
        })
        .collect()
}

/// Int-typed field names of a (possibly dep-module) record type.
fn rf_record_int_fields(ctx: &CodegenContext, type_name: &str) -> Vec<String> {
    use crate::ast::TypeDef;
    let base = type_name.rsplit('.').next().unwrap_or(type_name).trim();
    let find = |tds: &[TypeDef]| -> Option<Vec<String>> {
        tds.iter().find_map(|td| match td {
            TypeDef::Product { name, fields, .. } if name == base => Some(
                fields
                    .iter()
                    .filter(|(_, ty)| ty.trim() == "Int")
                    .map(|(n, _)| n.clone())
                    .collect(),
            ),
            _ => None,
        })
    };
    if let Some(v) = find(&ctx.type_defs) {
        return v;
    }
    for m in &ctx.modules {
        if let Some(v) = find(&m.type_defs) {
            return v;
        }
    }
    Vec::new()
}

/// One cited earlier law for the rational-floor arm.
struct RfCitation {
    have_name: String,
    apply: String,
    simp_set: String,
}

/// Discover the earlier sibling `holds` laws to cite: subject body is a single
/// order comparison or a `Bool.and` of comparisons, givens are a prefix (same
/// names and types, in order) of this law's givens, and any premise projects
/// out of this law's `when`. Deterministic (static IR + source order).
fn rf_citations(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
) -> Vec<RfCitation> {
    use crate::ast::{BinOp, Expr, TopLevel, VerifyKind};
    let mut out = Vec::new();
    let citing_when = law.when.as_ref();
    for item in &ctx.items {
        let TopLevel::Verify(prev) = item else {
            continue;
        };
        if prev.line == vb.line && prev.fn_name == vb.fn_name {
            break; // only earlier-in-source siblings
        }
        let VerifyKind::Law(prev_law) = &prev.kind else {
            continue;
        };
        if !matches!(
            &prev_law.rhs.node,
            Expr::Literal(crate::ast::Literal::Bool(true))
        ) {
            continue;
        }
        // Givens must be a prefix of the citing law's givens (same name+type).
        if prev_law.givens.len() > law.givens.len() {
            continue;
        }
        if !prev_law
            .givens
            .iter()
            .zip(law.givens.iter())
            .all(|(a, b)| a.name == b.name && a.type_name.trim() == b.type_name.trim())
        {
            continue;
        }
        // Subject body: a single order comparison, or a `Bool.and` of comparisons.
        let Some(inlined) = inline_fn_call(&prev_law.lhs, ctx) else {
            continue;
        };
        let is_cmp = |e: &Expr| {
            matches!(
                e,
                Expr::BinOp(BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte, _, _)
            )
        };
        let is_order_content = match &inlined.node {
            Expr::FnCall(callee, args)
                if super::super::shared::expr_dotted_name(callee).as_deref()
                    == Some("Bool.and")
                    && args.len() == 2 =>
            {
                is_cmp(&args[0].node) && is_cmp(&args[1].node)
            }
            other => is_cmp(other),
        };
        if !is_order_content {
            continue;
        }
        // Premise discharge (if any) must project out of this law's `when`.
        let prem = match &prev_law.when {
            None => String::new(),
            Some(w) => {
                let Some(citing) = citing_when else {
                    continue;
                };
                match rf_premise_proof(citing, w) {
                    Some(p) => format!(" {p}"),
                    None => continue,
                }
            }
        };
        let thm = format!("{}_law_{}", aver_name_to_lean(&prev.fn_name), prev_law.name);
        let args = intro_names[..prev_law.givens.len()].join(" ");
        let mut simp: Vec<String> = rf_filtered_defs(ctx, prev, prev_law);
        simp.extend(
            [
                "Bool.and_eq_true",
                "decide_eq_true_eq",
                "ge_iff_le",
                "gt_iff_lt",
            ]
            .iter()
            .map(|s| s.to_string()),
        );
        let have_name = format!("h_{}_{}", aver_name_to_lean(&prev.fn_name), prev_law.name);
        out.push(RfCitation {
            apply: format!("{thm} {args}{prem}"),
            simp_set: simp.join(", "),
            have_name,
        });
    }
    out
}

/// Emit a Lean string for an inlined sub-expression (used to derive the
/// float-format field accessors and the floor term from the recognized shape —
/// produced from the SAME definitions the goal unfolds, so they parse-match).
fn rf_emit(ctx: &CodegenContext, e: &crate::ast::Spanned<crate::ast::Expr>) -> String {
    super::super::super::expr::emit_expr_legacy(e, ctx, None)
}

/// One-level inline of a fn call, resolving the callee across the entry module
/// AND dependency modules (unlike [`inline_fn_call`], which only resolves entry
/// scope — a cross-module `fpValue` call would not resolve there). Substitutes
/// the call's args for the resolved fn's params in its tail body.
fn rf_inline_fn_call(
    call: &crate::ast::Spanned<crate::ast::Expr>,
    ctx: &CodegenContext,
) -> Option<crate::ast::Spanned<crate::ast::Expr>> {
    use crate::ast::Expr;
    let Expr::FnCall(callee, args) = &call.node else {
        return None;
    };
    let name = super::super::shared::expr_dotted_name(callee)?;
    let fd = rf_resolve_fn(ctx, &name)?;
    if fd.params.len() != args.len() {
        return None;
    }
    let body = fd.body.tail_expr()?;
    let mut map: std::collections::HashMap<String, crate::ast::Spanned<crate::ast::Expr>> =
        std::collections::HashMap::new();
    for ((pname, _), arg) in fd.params.iter().zip(args.iter()) {
        map.insert(pname.clone(), arg.clone());
    }
    Some(substitute_idents(body, &map))
}

/// The four float-format value accessors (`f.sign`, `f.sigBits`, `f.exp`,
/// `f.width` as emitted Lean strings) read off the inlined `fpValue(F)` body
/// `Fraction(top = F.sign * F.sigBits * pow(F.exp), bottom = pow(F.width - 1))`.
/// Name-blind — derived from `fpValue`'s own definition, never hard-coded.
struct RfFpFields {
    sign: String,
    sig: String,
    exp: String,
    width: String,
}

fn rf_fpvalue_fields(
    frac: &crate::ast::Spanned<crate::ast::Expr>,
    ctx: &CodegenContext,
) -> Option<RfFpFields> {
    use crate::ast::{BinOp, Expr};
    let Expr::RecordCreate { fields, .. } = &frac.node else {
        return None;
    };
    let top = fields.iter().find(|(n, _)| n == "top").map(|(_, e)| e)?;
    let bottom = fields.iter().find(|(n, _)| n == "bottom").map(|(_, e)| e)?;
    // bottom = pow(Sub(Attr(F, width), _))
    let Expr::FnCall(_, bargs) = &bottom.node else {
        return None;
    };
    let Expr::BinOp(BinOp::Sub, wbase, _) = &bargs.first()?.node else {
        return None;
    };
    // top = Mul(Mul(Attr(F, sign), Attr(F, sig)), pow(Attr(F, exp)))
    let Expr::BinOp(BinOp::Mul, l, r) = &top.node else {
        return None;
    };
    let Expr::BinOp(BinOp::Mul, sign_b, sig_b) = &l.node else {
        return None;
    };
    let Expr::FnCall(_, eargs) = &r.node else {
        return None;
    };
    let exp_b = eargs.first()?;
    Some(RfFpFields {
        sign: rf_emit(ctx, sign_b),
        sig: rf_emit(ctx, sig_b),
        exp: rf_emit(ctx, exp_b),
        width: rf_emit(ctx, wbase),
    })
}

/// The earlier-in-source sibling FLOOR WINDOW law to cite: a `holds` law whose
/// givens are a prefix of this law's and whose inlined body is a `Bool.and` of
/// two comparisons (the Euclidean window `W*q <= N ∧ N < W*(q+1)`). Returns its
/// Lean theorem name and the unfold target. Name-blind (keyed on the body
/// shape), deterministic (source order).
fn rf_window_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Option<(String, String)> {
    use crate::ast::{BinOp, Expr, TopLevel, VerifyKind};
    let is_cmp = |e: &Expr| {
        matches!(
            e,
            Expr::BinOp(BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte, _, _)
        )
    };
    for item in &ctx.items {
        let TopLevel::Verify(prev) = item else {
            continue;
        };
        if prev.line == vb.line && prev.fn_name == vb.fn_name {
            break; // earlier-in-source only
        }
        let VerifyKind::Law(prev_law) = &prev.kind else {
            continue;
        };
        if !matches!(
            &prev_law.rhs.node,
            Expr::Literal(crate::ast::Literal::Bool(true))
        ) {
            continue;
        }
        if prev_law.givens.len() > law.givens.len()
            || !prev_law
                .givens
                .iter()
                .zip(law.givens.iter())
                .all(|(a, b)| a.name == b.name && a.type_name.trim() == b.type_name.trim())
        {
            continue;
        }
        let Some(inlined) = inline_fn_call(&prev_law.lhs, ctx) else {
            continue;
        };
        let Expr::FnCall(callee, args) = &inlined.node else {
            continue;
        };
        if super::super::shared::expr_dotted_name(callee).as_deref() != Some("Bool.and")
            || args.len() != 2
        {
            continue;
        }
        if !is_cmp(&args[0].node) || !is_cmp(&args[1].node) {
            continue;
        }
        let thm = format!("{}_law_{}", aver_name_to_lean(&prev.fn_name), prev_law.name);
        let unfold = format!("_root_.{}", aver_name_to_lean(&prev.fn_name));
        return Some((thm, unfold));
    }
    None
}

/// The power-of-two HOMOMORPHISM pool law's Lean theorem name (`pow(m+n) =
/// pow(m)*pow(n)`), found by shape over the in-file laws and dep-module law
/// pools. Name-blind: matches any law whose subject is the `pow` fn and whose
/// claim's rhs is a product of two `pow` calls. The dep-module name is qualified
/// with the module prefix (`Domain.Fprep.pow2_law_homomorphism`).
fn rf_homomorphism_name(ctx: &CodegenContext, pow_lean: &str) -> Option<String> {
    use crate::ast::{BinOp, Expr, TopLevel, VerifyKind};
    let pow_base = rf_bare_basename(pow_lean);
    let is_hom = |lw: &VerifyLaw| -> bool {
        // rhs is `pow(_) * pow(_)`.
        if let Expr::BinOp(BinOp::Mul, a, b) = &lw.rhs.node {
            let is_pow_call = |e: &Expr| {
                matches!(e, Expr::FnCall(c, _)
                    if super::super::shared::expr_dotted_name(c)
                        .as_deref()
                        .map(rf_bare_basename) == Some(pow_base))
            };
            return is_pow_call(&a.node) && is_pow_call(&b.node);
        }
        false
    };
    // In-file (entry module) first.
    for item in &ctx.items {
        if let TopLevel::Verify(prev) = item
            && let VerifyKind::Law(prev_law) = &prev.kind
            && rf_bare_basename(&prev.fn_name) == pow_base
            && is_hom(prev_law)
        {
            return Some(format!(
                "{}_law_{}",
                aver_name_to_lean(&prev.fn_name),
                prev_law.name
            ));
        }
    }
    // Dep modules — qualify with the module prefix.
    for m in &ctx.modules {
        for prev in &m.verify_laws {
            if let VerifyKind::Law(prev_law) = &prev.kind
                && rf_bare_basename(&prev.fn_name) == pow_base
                && is_hom(prev_law)
            {
                return Some(format!(
                    "{}.{}_law_{}",
                    m.prefix,
                    aver_name_to_lean(&prev.fn_name),
                    prev_law.name
                ));
            }
        }
    }
    None
}

/// The rational-order truncation-error bound arm (Lemma 7.2.2's strict bound
/// `|epsilon| < 2^(e_x - i + 1)`). Mirrors the de-risked litmus-clean proof: it
/// factors the error magnitude (the sign cancels, the floor remainder is the
/// nonneg core), splits the `pow2Signed` ulp-exponent sign branch, CITES the
/// floor window pool law for the strict remainder bound `r < W` and the
/// power-of-two homomorphism pool law for the ulp/significand exponent link, and
/// MULTIPLIES the window bound by the positive product factor via the generic
/// multiply-by-positive rung in `aver_int_order`. The algebra lives ENTIRELY in
/// the two cited Aver laws; the arm only factors / sign-splits / multiplies /
/// ring-bridges (`grind`). Returns `None` (decline → bounded fallback) on any
/// structural surprise — credit stays fail-closed behind the probe + axiom
/// whitelist.
/// The four general-exponent value accessors read off an inlined
/// `fpValueGeneral(F)` body `times(pow2Signed(F.exp), Fraction(top = F.sign *
/// F.sigBits, bottom = pow(F.width - 1)))`: the signed-power-of-two cone fn (the
/// `times` factor whose call shape is [`signed_pow2_shape`]), its exponent
/// argument, and the sign / significand / width off the inner `Fraction`.
/// Name-blind — derived from the value's own structure, never hard-coded.
struct RfGeneralFields {
    sgn_fn: String,
    exp: String,
    sign: String,
    sig: String,
    width: String,
}

/// Read the general-exponent value fields off an inlined `fpValueGeneral(F)`
/// (`times(SGN(exp), Fraction(top = sign * sig, bottom = pow(width - 1)))`).
/// Returns `None` if the value is not in that signed-power-of-two × fraction
/// form (e.g. the clamped `fpValue`, a bare `Fraction`).
fn rf_general_value_fields(
    val: &crate::ast::Spanned<crate::ast::Expr>,
    ctx: &CodegenContext,
    int_pow: &str,
) -> Option<RfGeneralFields> {
    use crate::ast::{BinOp, Expr};
    // val = times(A, B)
    let Expr::FnCall(tcallee, targs) = &val.node else {
        return None;
    };
    if super::super::shared::expr_dotted_name(tcallee)?
        .rsplit('.')
        .next()
        != Some("times")
        || targs.len() != 2
    {
        return None;
    }
    // A = SGN(exp), with SGN a signed-power-of-two cone fn (shape-recognized).
    let Expr::FnCall(acallee, aargs) = &targs[0].node else {
        return None;
    };
    if aargs.len() != 1 {
        return None;
    }
    let sgn_dotted = super::super::shared::expr_dotted_name(acallee)?;
    let sgn_fd = rf_resolve_fn(ctx, &sgn_dotted)?;
    if !signed_pow2_shape(sgn_fd, int_pow) {
        return None;
    }
    // B = Fraction { top = sign * sig, bottom = pow(width - 1) }.
    let Expr::RecordCreate { fields, .. } = &targs[1].node else {
        return None;
    };
    let top = fields.iter().find(|(n, _)| n == "top").map(|(_, e)| e)?;
    let bottom = fields.iter().find(|(n, _)| n == "bottom").map(|(_, e)| e)?;
    let Expr::BinOp(BinOp::Mul, sign_b, sig_b) = &top.node else {
        return None;
    };
    let Expr::FnCall(_, bargs) = &bottom.node else {
        return None;
    };
    let Expr::BinOp(BinOp::Sub, wbase, _) = &bargs.first()?.node else {
        return None;
    };
    Some(RfGeneralFields {
        sgn_fn: aver_name_to_lean(&sgn_dotted),
        exp: rf_emit(ctx, &aargs[0]),
        sign: rf_emit(ctx, sign_b),
        sig: rf_emit(ctx, sig_b),
        width: rf_emit(ctx, wbase),
    })
}

/// One leaf of a piecewise rounded value: its significand integer `sig` (the
/// `sigBits` field) and its exponent `exp`, as inlined record fields. The bound
/// rung emits one `away_leaf` application per DISTINCT leaf.
struct RfLeaf {
    sig: crate::ast::Spanned<crate::ast::Expr>,
    exp: crate::ast::Spanned<crate::ast::Expr>,
    width: crate::ast::Spanned<crate::ast::Expr>,
}

/// Expand a record `sigBits` expression into the significand integers it can
/// take: if it is a fn call that inlines to a (record-free) `match` — `awaySig`'s
/// exact/round-up split — return one per arm (recursively); otherwise the bare
/// expression. The arms here are integer terms (`floorDiv …`, `floorDiv … + 1`),
/// not records, so the recursion bottoms out immediately for `away`/`sticky`.
fn rf_expand_sig(
    sig: &crate::ast::Spanned<crate::ast::Expr>,
    ctx: &CodegenContext,
) -> Vec<crate::ast::Spanned<crate::ast::Expr>> {
    use crate::ast::Expr;
    // Only expand a significand SELECTOR (`awaySig`) — never the recursive
    // power-of-two or the floor wrapper (both stay opaque atoms).
    if let Expr::FnCall(c, _) = &sig.node
        && let Some(name) = super::super::shared::expr_dotted_name(c)
    {
        let base = rf_bare_basename(&name);
        let recursive: std::collections::HashSet<String> =
            super::super::recursive_pure_fn_names(ctx)
                .iter()
                .map(|n| rf_bare_basename(&aver_name_to_lean(n)).to_string())
                .collect();
        if !recursive.contains(base)
            && !rf_is_floordiv_wrapper(ctx, base)
            && let Some(inl) = rf_inline_fn_call(sig, ctx)
            && let Expr::Match { arms, .. } = &inl.node
        {
            return arms
                .iter()
                .flat_map(|a| rf_expand_sig(&a.body, ctx))
                .collect();
        }
    }
    vec![sig.clone()]
}

/// Recursively inline every non-recursive, non-`floorDiv`-wrapper user-fn call in
/// `e` — mirroring what the rung's `set1` simp does to the goal, so the leaf's
/// significand / floor terms computed here parse-match the unfolded goal. The
/// Euclidean `floorDiv` wrapper and the recursive power-of-two stay opaque (a
/// round-to-odd leaf's `2 * stickyHalf(f, n)` thus becomes `2 * floorDiv(...)`,
/// exposing the half-precision floor the window law is about).
fn rf_inline_wrappers(
    e: &crate::ast::Spanned<crate::ast::Expr>,
    ctx: &CodegenContext,
) -> crate::ast::Spanned<crate::ast::Expr> {
    let recursive: std::collections::HashSet<String> = super::super::recursive_pure_fn_names(ctx)
        .iter()
        .map(|n| rf_bare_basename(&aver_name_to_lean(n)).to_string())
        .collect();
    rf_inline_wrappers_inner(e, ctx, &recursive)
}

fn rf_inline_wrappers_inner(
    e: &crate::ast::Spanned<crate::ast::Expr>,
    ctx: &CodegenContext,
    recursive: &std::collections::HashSet<String>,
) -> crate::ast::Spanned<crate::ast::Expr> {
    use crate::ast::{Expr, Spanned};
    let rec = |x: &crate::ast::Spanned<Expr>| rf_inline_wrappers_inner(x, ctx, recursive);
    let mapped = match &e.node {
        Expr::BinOp(op, a, b) => {
            Spanned::bare(Expr::BinOp(*op, Box::new(rec(a)), Box::new(rec(b))))
        }
        Expr::Neg(a) => Spanned::bare(Expr::Neg(Box::new(rec(a)))),
        Expr::FnCall(c, args) => {
            Spanned::bare(Expr::FnCall(c.clone(), args.iter().map(rec).collect()))
        }
        _ => e.clone(),
    };
    if let Expr::FnCall(c, _) = &mapped.node
        && let Some(name) = super::super::shared::expr_dotted_name(c)
    {
        let base = rf_bare_basename(&aver_name_to_lean(&name)).to_string();
        if !recursive.contains(&base)
            && !rf_is_floordiv_wrapper(ctx, &base)
            && let Some(inl) = rf_inline_fn_call(&mapped, ctx)
        {
            return rf_inline_wrappers_inner(&inl, ctx, recursive);
        }
    }
    mapped
}

/// The per-leaf floor placement of a PIECEWISE rounded value's significand, read
/// off the goal-form (wrapper-inlined) significand expression. A leaf significand
/// is `c * floorDiv(S * pow(E), W) [+ off]` (`c` the precision factor — `1` full,
/// `2` the round-to-odd half-cell — and `off` the round-UP / odd `+ 1`); from it
/// the rung supplies the window floor `m = floorDiv(...)`, its numerator
/// `u = S * pow(E)`, the precision exponent `E` (the window is cited at `E + 1`),
/// and `c`. Returns `None` when the significand has no floor (the carry
/// renormalization `pow(i-1)` or the round-to-odd `n ≤ 1` constant) — those leaves
/// borrow a sibling floor or read the format-normalization window.
struct RfFloorInfo {
    m: String,
    u: String,
    eexp: String,
    c: u8,
}

fn rf_leaf_floor(
    sig: &crate::ast::Spanned<crate::ast::Expr>,
    ctx: &CodegenContext,
) -> Option<RfFloorInfo> {
    use crate::ast::{BinOp, Expr, Literal};
    let is_one = |e: &Expr| matches!(e, Expr::Literal(Literal::Int(1)));
    let is_two = |e: &Expr| matches!(e, Expr::Literal(Literal::Int(2)));
    let is_floordiv = |e: &crate::ast::Spanned<Expr>| -> bool {
        matches!(&e.node, Expr::FnCall(c, _)
            if super::super::shared::expr_dotted_name(c)
                .map(|n| rf_is_floordiv_wrapper(ctx, rf_bare_basename(&aver_name_to_lean(&n))))
                == Some(true))
    };
    // Strip a trailing `+ 1` (round-up / round-to-odd).
    let inner = match &sig.node {
        Expr::BinOp(BinOp::Add, a, b) if is_one(&b.node) => a.as_ref(),
        _ => sig,
    };
    // `c * floorDiv(...)` (c = 2) or a bare `floorDiv(...)` (c = 1).
    let (fd, c): (&crate::ast::Spanned<Expr>, u8) = match &inner.node {
        Expr::BinOp(BinOp::Mul, a, b) if is_two(&a.node) && is_floordiv(b) => (b.as_ref(), 2),
        Expr::BinOp(BinOp::Mul, a, b) if is_two(&b.node) && is_floordiv(a) => (a.as_ref(), 2),
        _ if is_floordiv(inner) => (inner, 1),
        _ => return None,
    };
    let Expr::FnCall(_, fargs) = &fd.node else {
        return None;
    };
    let numer = fargs.first()?;
    // numer = S * pow(E).
    let Expr::BinOp(BinOp::Mul, _s, powcall) = &numer.node else {
        return None;
    };
    let Expr::FnCall(_, pargs) = &powcall.node else {
        return None;
    };
    let eexp = rf_emit(ctx, pargs.first()?);
    Some(RfFloorInfo {
        m: rf_emit(ctx, fd),
        u: rf_emit(ctx, numer),
        eexp,
        c,
    })
}

/// Walk a (possibly nested) record-returning `match` — a PIECEWISE rounded value
/// `fpValueGeneral`'s argument inlines to — and collect its leaf records'
/// `(sigBits, exp)` fields, expanding each record's `sigBits` through
/// [`rf_expand_sig`] (so `awaySig`'s own match contributes its arms). Returns
/// `None` if a leaf is neither a `match` nor a `RecordCreate` (an unrecognized
/// shape — decline rather than emit a wrong split).
fn rf_collect_leaves(
    expr: &crate::ast::Spanned<crate::ast::Expr>,
    ctx: &CodegenContext,
    out: &mut Vec<RfLeaf>,
) -> Option<()> {
    use crate::ast::Expr;
    match &expr.node {
        Expr::Match { arms, .. } => {
            for a in arms {
                rf_collect_leaves(&a.body, ctx, out)?;
            }
            Some(())
        }
        Expr::RecordCreate { fields, .. } => {
            let sig = fields
                .iter()
                .find(|(n, _)| n == "sigBits")
                .map(|(_, e)| e)?;
            let exp = fields.iter().find(|(n, _)| n == "exp").map(|(_, e)| e)?;
            let width = fields.iter().find(|(n, _)| n == "width").map(|(_, e)| e)?;
            for s in rf_expand_sig(sig, ctx) {
                out.push(RfLeaf {
                    sig: s,
                    exp: exp.clone(),
                    width: width.clone(),
                });
            }
            Some(())
        }
        _ => None,
    }
}

/// The all-exponent rational strict-order rung for a PIECEWISE rounded value
/// (Lemma 7.2.3 `away` / 7.2.4 `sticky` strict bounds): the same bound as the
/// single-record arm, but the rounded value `fpValueGeneral(G)` inlines to a
/// RECORD-RETURNING MATCH (the `awaySig` exact/round-up split plus the carry-to-2
/// renormalization, or `sticky`'s round-to-odd split), not one clean record. The
/// rung unfolds the rounding layer, `repeat' split`s the match into its leaf
/// records, and closes EVERY leaf by one application of the generic
/// `away_leaf` support lemma — shape-keyed on the leaf's significand `Q` and its
/// exponent offset relative to `e_x` (`k2 = 1` no carry, `k2 = 2` the carry, whose
/// value-uniformity is discharged by the cited pow2 homomorphism `hdbl`). The
/// per-leaf placement `hQe` is the LINEAR fact that the effective significand sits
/// at the floor or its successor — closed by `omega`. Name-blind, reused
/// unchanged for `away` and `sticky`. Returns `None` (→ single-record arm) when
/// the rounded value is not a record-returning match.
fn emit_rational_floor_bound_matched(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
) -> Option<AutoProof> {
    use crate::ast::Expr;
    let pow = rf_pow2_fn(vb, law, ctx)?;
    let inlined = inline_fn_call(&law.lhs, ctx)?;
    let Expr::FnCall(_lt, lt_args) = &inlined.node else {
        return None;
    };
    if lt_args.len() != 2 {
        return None;
    }
    let Expr::FnCall(_abs, abs_args) = &lt_args[0].node else {
        return None;
    };
    let err_call = abs_args.first()?;
    let Expr::FnCall(_ps, ps_args) = &lt_args[1].node else {
        return None;
    };
    let le_expr = ps_args.first()?;
    let Expr::FnCall(_te, te_args) = &err_call.node else {
        return None;
    };
    if te_args.len() != 2 {
        return None;
    }
    let f_name = expr_var_name(&te_args[0].node)?.to_string();
    let i_name = expr_var_name(&te_args[1].node)?.to_string();
    let minus_inlined = rf_inline_fn_call(err_call, ctx)?;
    let Expr::FnCall(_m, m_args) = &minus_inlined.node else {
        return None;
    };
    if m_args.len() != 2 {
        return None;
    }
    // Identify which `minus` arg is the ORIGINAL value `fpValueGeneral(F)` (a bare
    // given) and which is the ROUNDED value `fpValueGeneral(round(F, i))`. The
    // single-record arm assumes `(orig, rounded)`; `awayError` is `(rounded, orig)`.
    let arg_round_call = |e: &crate::ast::Spanned<Expr>| -> Option<crate::ast::Spanned<Expr>> {
        let Expr::FnCall(_, a) = &e.node else {
            return None;
        };
        let inner = a.first()?;
        matches!(&inner.node, Expr::FnCall(..)).then(|| inner.clone())
    };
    let (orig_value, rounded_call) = match (arg_round_call(&m_args[0]), arg_round_call(&m_args[1]))
    {
        // both are calls (round(...)): the rounded one is whichever has 2 args.
        (Some(a0), Some(a1)) => {
            let two = |c: &crate::ast::Spanned<Expr>| matches!(&c.node, Expr::FnCall(_, ar) if ar.len() == 2);
            if two(&a1) {
                (m_args[0].clone(), a1)
            } else {
                (m_args[1].clone(), a0)
            }
        }
        (Some(a0), None) => (m_args[1].clone(), a0),
        (None, Some(a1)) => (m_args[0].clone(), a1),
        (None, None) => return None,
    };
    let fpv_f = rf_inline_fn_call(&orig_value, ctx)?;
    let gf = rf_general_value_fields(&fpv_f, ctx, &pow)?;
    let value_base = match &orig_value.node {
        Expr::FnCall(c, _) => {
            rf_bare_basename(&super::super::shared::expr_dotted_name(c)?).to_string()
        }
        _ => return None,
    };
    // The rounded value must inline to a record-returning MATCH (else the
    // single-record arm handles it).
    let rounded_rec = rf_inline_fn_call(&rounded_call, ctx)?;
    if !matches!(&rounded_rec.node, Expr::Match { .. }) {
        return None;
    }
    let mut leaves: Vec<RfLeaf> = Vec::new();
    rf_collect_leaves(&rounded_rec, ctx, &mut leaves)?;
    if leaves.is_empty() {
        return None;
    }

    let (window_thm, window_fn) = rf_window_law(vb, law, ctx)?;
    let hom = rf_homomorphism_name(ctx, &pow)?;
    let le = rf_emit(ctx, le_expr);

    // Sub-expression strings (signed power of two kept ABSTRACT).
    let sgnfn = gf.sgn_fn.clone();
    let exp = gf.exp.clone();
    let sign = gf.sign.clone();
    let sig = gf.sig.clone();
    let pet = format!("({sgnfn} ({exp})).top");
    let peb = format!("({sgnfn} ({exp})).bottom");
    let plt = format!("({sgnfn} ({le})).top");
    let plb = format!("({sgnfn} ({le})).bottom");
    let pe1t = format!("({sgnfn} ({exp} + 1)).top");
    let pe1b = format!("({sgnfn} ({exp} + 1)).bottom");
    let pw = format!("{pow} ({} - 1)", gf.width);
    let pp = format!("{pow} ({i_name} - 1)");

    // The PRIMARY floor — the first leaf bearing a floorDiv (its half/full
    // precision window); a carry-renormalization leaf (no floor of its own) borrows
    // it. At least one leaf must bear a floor.
    let base = format!("{}_law_{}__rfb", aver_name_to_lean(&vb.fn_name), law.name);
    let primary = leaves
        .iter()
        .find_map(|l| rf_leaf_floor(&rf_inline_wrappers(&l.sig, ctx), ctx));

    // Classify each leaf's exponent offset (0 → no carry, 1 → carry) and its floor
    // placement. Dedup by (goal-form significand, offset). Each leaf is one apply of
    // the generic two-sided leaf lemma; name-blind, keyed on the significand shape:
    //  * floor-bearing → its own (m, u, c) and a window cited at its precision;
    //  * carry (no floor, exp + 1) → borrows the primary floor, k2 = 2;
    //  * degenerate (no floor, exp + 0 — the round-to-odd n ≤ 1 constant) → m = 1,
    //    u = S, c = 1 and the format-normalization window (1 ≤ s_x < 2).
    let mut alts: Vec<String> = Vec::new();
    let mut seen: std::collections::HashSet<(String, u8)> = std::collections::HashSet::new();
    let mut has_carry = false;
    let mut needs_degenerate = false;
    let mut win_exps: Vec<String> = Vec::new();
    for l in &leaves {
        let goal_sig = rf_inline_wrappers(&l.sig, ctx);
        let q = rf_emit(ctx, &goal_sig);
        let le_exp = rf_emit(ctx, &l.exp);
        let offset: u8 = if le_exp == exp {
            0
        } else if matches!(&l.exp.node, Expr::BinOp(crate::ast::BinOp::Add, a, b)
            if rf_emit(ctx, a) == exp && matches!(&b.node, Expr::Literal(crate::ast::Literal::Int(1))))
        {
            1
        } else {
            return None;
        };
        if !seen.insert((q.clone(), offset)) {
            continue;
        }
        let (av, ab, k2) = if offset == 0 {
            (pet.clone(), peb.clone(), "1")
        } else {
            has_carry = true;
            (pe1t.clone(), pe1b.clone(), "2")
        };
        let width_leaf = rf_emit(ctx, &l.width);
        let p_leaf = format!("{pow} ({width_leaf} - 1)");
        let (m, u, c, ehalf) = match rf_leaf_floor(&goal_sig, ctx) {
            Some(fi) => {
                if !win_exps.contains(&fi.eexp) {
                    win_exps.push(fi.eexp.clone());
                }
                let e = fi.eexp.clone();
                (fi.m, fi.u, fi.c, Some(e))
            }
            None if offset == 1 => {
                let pr = primary.as_ref()?;
                if !win_exps.contains(&pr.eexp) {
                    win_exps.push(pr.eexp.clone());
                }
                (pr.m.clone(), pr.u.clone(), pr.c, Some(pr.eexp.clone()))
            }
            None => {
                needs_degenerate = true;
                (String::from("1"), sig.clone(), 1u8, None)
            }
        };
        // The precision-halving discharge: `S * p = c * u` needs
        // `pow(E+1) = 2 * pow(E)` for the half-cell (c = 2) leaf, whose `pow(i-1)`
        // the `pow_succ_p` grind pattern (keyed on `pow(n+1)`) misses (the index is
        // `i - 1`, not `n + 1`). Cite `pow_succ_p` at the leaf's own exponent E so
        // grind sees the link; the `by omega` side condition (`0 ≤ E`) holds in the
        // leaf's split branch and harmlessly fails (→ falls through) for the
        // degenerate leaf.
        let half = match &ehalf {
            Some(e) => {
                format!(" | (have hps := {base}__pow_succ_p ({e}) (by omega); grind)")
            }
            None => String::new(),
        };
        alts.push(format!(
            "    | (apply {base}__away_leaf ({pet}) ({peb}) ({plt}) ({plb}) ({av}) ({ab}) ({pw}) ({p_leaf}) ({u}) {c} {k2} ({q}) ({sig}) ({sign}) ({m}) <;> first | assumption | omega | grind{half})"
        ));
    }

    // The `when` conjuncts → intro pattern + the `isFp` sign conjunct.
    let when = law.when.as_ref()?;
    let conjs = rf_flatten_bool_and(when);
    let sign_conj = rf_sign_conjunct(law, ctx)?;
    let fmt_idx = conjs
        .iter()
        .position(|c| arith_eq(&c.node, &sign_conj.node))?;
    let isfp_lean = match &sign_conj.node {
        Expr::FnCall(callee, _) => {
            aver_name_to_lean(&super::super::shared::expr_dotted_name(callee)?)
        }
        _ => return None,
    };
    let ncj = conjs.len();
    let mut pat = "h_rfbp0".to_string();
    for k in 1..ncj {
        pat = format!("⟨{pat}, h_rfbp{k}⟩");
    }
    let fmtname = format!("h_rfbp{fmt_idx}");

    let mut support_lines: Vec<String> =
        super::super::floor_window::pow2_signed_pos_support(&base, &pow, &sgnfn)
            .lines()
            .map(|l| l.to_string())
            .collect();
    support_lines.extend(
        super::super::floor_window::matched_leaf_support(&base, &pow)
            .lines()
            .map(|l| l.to_string()),
    );

    // The two unfold sets: the rounding layer (set1, before the split) and the
    // value/rational layer (set2, per leaf after the split). The signed power of
    // two stays abstract throughout; `pow`/floorDiv are already excluded.
    let rational_prims = [
        "absFraction",
        "absInt",
        "lessThan",
        "minus",
        "times",
        "plus",
        "sameValue",
        "negate",
    ];
    let sgn_base = rf_bare_basename(&sgnfn).to_string();
    let filtered = rf_filtered_defs(ctx, vb, law)
        .into_iter()
        .filter(|d| rf_bare_basename(d) != sgn_base)
        .collect::<Vec<_>>();
    let is_set2 = |d: &str| {
        let b = rf_bare_basename(d);
        b == value_base || rational_prims.contains(&b)
    };
    let set1 = filtered
        .iter()
        .filter(|d| !is_set2(d))
        .cloned()
        .collect::<Vec<_>>()
        .join(", ");
    let set2 = filtered
        .iter()
        .filter(|d| is_set2(d))
        .cloned()
        .collect::<Vec<_>>()
        .join(", ");

    let intro = format!("  intro {} h_when", intro_names.join(" "));
    let mut body: Vec<String> = vec![intro, "  first".to_string(), "  | (".to_string()];
    body.push("    simp only [Bool.and_eq_true, decide_eq_true_eq] at h_when".to_string());
    body.push(format!("    obtain {pat} := h_when"));
    body.push(format!("    have hpp : 0 < {pp} := {base}__pow_pos _"));
    body.push(format!("    have hpw : 0 < {pw} := {base}__pow_pos _"));
    body.push(format!("    have hPSeT : 0 < {pet} := {base}__sgnt_pos _"));
    body.push(format!("    have hPSeB : 0 < {peb} := {base}__sgnb_pos _"));
    body.push(format!("    have hPSlT : 0 < {plt} := {base}__sgnt_pos _"));
    body.push(format!("    have hPSlB : 0 < {plb} := {base}__sgnb_pos _"));
    if has_carry {
        body.push(format!(
            "    have hPSe1T : 0 < {pe1t} := {base}__sgnt_pos _"
        ));
        body.push(format!(
            "    have hPSe1B : 0 < {pe1b} := {base}__sgnb_pos _"
        ));
    }
    body.push(format!("    have hsign : {sign} = 1 ∨ {sign} = -1 := by unfold {isfp_lean} at {fmtname}; simp only [Bool.and_eq_true, Bool.or_eq_true, beq_iff_eq, decide_eq_true_eq] at {fmtname}; exact {fmtname}.1"));
    body.push(format!(
        "    have hpowi : {pow} {i_name} = 2 * {pp} := {base}__pow_of_pos {i_name} (by omega)"
    ));
    if has_carry {
        body.push(format!("    have hp1 : {pow} 1 = 2 := by have hx := {base}__pow_of_pos 1 (by omega); rw [show (1:Int) - 1 = 0 by omega, {base}__pow_of_nonpos 0 (by omega)] at hx; omega"));
    }
    // The floor windows, one per distinct precision the leaves use: cite
    // truncFitsWindow at the leaf's own precision E + 1 and normalize the unfolded
    // exponent (E + 1) - 1 back to E so the floor matches the goal's
    // floorDiv(S * pow(E), W). The away/trunc precision is i (E = i-1); the
    // round-to-odd half-cell precision is i-1 (E = i-2).
    for (wi, ee) in win_exps.iter().enumerate() {
        body.push(format!(
            "    have h_win{wi} := {window_thm} {f_name} (({ee}) + 1)"
        ));
        body.push(format!(
            "    simp only [{window_fn}, Bool.and_eq_true, decide_eq_true_eq] at h_win{wi}"
        ));
        body.push(format!(
            "    rw [show (({ee}) + 1) - 1 = ({ee}) by omega] at h_win{wi}"
        ));
        body.push(format!("    obtain ⟨hwlo{wi}, hwhi{wi}⟩ := h_win{wi}"));
    }
    // The format-normalization window for the degenerate (round-to-odd n ≤ 1) leaf:
    // 1 ≤ s_x < 2, i.e. pow(width-1) ≤ S < 2 * pow(width-1), read off isFp.
    if needs_degenerate {
        body.push(format!("    have hnorm := {fmtname}"));
        body.push(format!("    unfold {isfp_lean} at hnorm"));
        body.push("    simp only [Bool.and_eq_true, Bool.or_eq_true, beq_iff_eq, decide_eq_true_eq] at hnorm".to_string());
        body.push(format!(
            "    have hp0 : {pow} (1 - 1) = 1 := {base}__pow_of_nonpos (1 - 1) (by omega)"
        ));
        body.push(format!("    have hwgt : ¬ ({} <= 0) := by", gf.width));
        body.push("      intro hle".to_string());
        body.push("      have hn1 := hnorm.2.1".to_string());
        body.push("      have hn2 := hnorm.2.2".to_string());
        body.push(format!(
            "      rw [{base}__pow_of_nonpos ({} - 1) (by omega)] at hn1",
            gf.width
        ));
        body.push(format!(
            "      rw [{base}__pow_of_nonpos {} (by omega)] at hn2",
            gf.width
        ));
        body.push("      omega".to_string());
        body.push(format!(
            "    have hwsucc : {pow} {} = 2 * {pow} ({} - 1) := {base}__pow_of_pos {} hwgt",
            gf.width, gf.width, gf.width
        ));
        body.push(format!(
            "    have hwlo_deg : {pow} ({} - 1) * 1 <= {sig} := by have := hnorm.2.1; omega",
            gf.width
        ));
        body.push(format!(
            "    have hwhi_deg : {sig} < {pow} ({} - 1) * (1 + 1) := by",
            gf.width
        ));
        body.push(format!(
            "      rw [show {pow} ({} - 1) * (1 + 1) = 2 * {pow} ({} - 1) by omega, ← hwsucc]",
            gf.width, gf.width
        ));
        body.push("      exact hnorm.2.2".to_string());
    }
    // hlink: pet * plb = plt * p * peb (signed-power exponent link).
    body.push(format!(
        "    have hlink : {pet} * {plb} = {plt} * {pp} * {peb} := by"
    ));
    body.push(format!("      by_cases hE : {exp} < 0"));
    body.push(format!("      · have hLE : {le} < 0 := by omega"));
    body.push(format!("        have hh := {hom} ({i_name} - 1) (0 - {exp}) (by simp only [Bool.and_eq_true, ge_iff_le, decide_eq_true_eq]; omega)"));
    body.push(format!(
        "        rw [show ({i_name} - 1) + (0 - {exp}) = 0 - ({le}) by omega] at hh"
    ));
    body.push(format!(
        "        unfold {sgnfn}; rw [if_pos hE, if_pos hLE]; grind"
    ));
    body.push(format!("      · by_cases hLE : {le} < 0"));
    body.push(format!("        · have hh := {hom} {exp} (0 - ({le})) (by simp only [Bool.and_eq_true, ge_iff_le, decide_eq_true_eq]; omega)"));
    body.push(format!(
        "          rw [show {exp} + (0 - ({le})) = {i_name} - 1 by omega] at hh"
    ));
    body.push(format!(
        "          unfold {sgnfn}; rw [if_neg hE, if_pos hLE]; grind"
    ));
    body.push(format!("        · have hh := {hom} ({le}) ({i_name} - 1) (by simp only [Bool.and_eq_true, ge_iff_le, decide_eq_true_eq]; omega)"));
    body.push(format!(
        "          rw [show ({le}) + ({i_name} - 1) = {exp} by omega] at hh"
    ));
    body.push(format!(
        "          unfold {sgnfn}; rw [if_neg hE, if_neg hLE]; grind"
    ));
    if has_carry {
        // hdbl: pe1t * peb = 2 * pet * pe1b (the carry value-uniformity, via the
        // same cited pow2 homomorphism with precision shift 1).
        body.push(format!(
            "    have hdbl : {pe1t} * {peb} = 2 * {pet} * {pe1b} := by"
        ));
        body.push(format!("      by_cases hE : ({exp} + 1) < 0"));
        body.push(format!("      · have hLE : {exp} < 0 := by omega"));
        body.push(format!("        have hh := {hom} 1 (0 - ({exp} + 1)) (by simp only [Bool.and_eq_true, ge_iff_le, decide_eq_true_eq]; omega)"));
        body.push(format!(
            "        rw [show (1 : Int) + (0 - ({exp} + 1)) = 0 - {exp} by omega, hp1] at hh"
        ));
        body.push(format!(
            "        unfold {sgnfn}; rw [if_pos hE, if_pos hLE]; grind"
        ));
        body.push(format!("      · by_cases hLE : {exp} < 0"));
        body.push(format!("        · have hh := {hom} ({exp} + 1) (0 - {exp}) (by simp only [Bool.and_eq_true, ge_iff_le, decide_eq_true_eq]; omega)"));
        body.push(format!(
            "          rw [show ({exp} + 1) + (0 - {exp}) = 1 by omega, hp1] at hh"
        ));
        body.push(format!(
            "          unfold {sgnfn}; rw [if_neg hE, if_pos hLE]; grind"
        ));
        body.push(format!("        · have hh := {hom} {exp} 1 (by simp only [Bool.and_eq_true, ge_iff_le, decide_eq_true_eq]; omega)"));
        body.push("          rw [hp1] at hh".to_string());
        body.push(format!(
            "          unfold {sgnfn}; rw [if_neg hE, if_neg hLE]; grind"
        ));
    }
    body.push(format!("    simp only [{set1}]"));
    body.push("    repeat' split".to_string());
    body.push(format!(
        "    all_goals (simp only [{set2}, decide_eq_true_eq])"
    ));
    body.push("    all_goals (try simp only [beq_iff_eq] at *)".to_string());
    body.push("    all_goals (".to_string());
    body.push("      first".to_string());
    for a in &alts {
        body.push(a.clone());
    }
    body.push("    )".to_string());
    body.push("  )".to_string());

    let floor = if super::super::super::tactic_ir::speculative::probing() {
        let id = format!("{}.{}", vb.fn_name, law.name);
        super::super::super::tactic_ir::speculative::record_probed(&id);
        format!("  | (trace \"AVERSPEC_SORRY:{id}\"; sorry)")
    } else {
        "  | sorry".to_string()
    };
    body.push(floor);

    Some(AutoProof {
        support_lines,
        body: Tactic::raw(body),
        replaces_theorem: false,
    })
}

/// The all-exponent rational strict-order rung for the rounding-error bound
/// `|epsilon| < 2^(e_x - i + 1)` over the GENERAL-exponent value `fpValueGeneral`
/// (Lemma 7.2.2's bound, faithful for the fractional floats `exp < 0` the kernel
/// divider lives in). The error reads `minus(fpValueGeneral(F), fpValueGeneral(G))`
/// (`G` the rounded value), so the SIGNED power of two `pow2Signed(e_x)` is the
/// shared factor of both terms. The rung:
///   * keeps the signed power of two an ABSTRACT atom (supplies its top/bottom
///     positivity from `pow2_signed_pos_support`) — routing AWAY from the pow2
///     normalizer that explodes on the squared denominators;
///   * factors the error magnitude (sign cancels, floor remainder `r` is the
///     nonneg core), resolves the two `absInt` matches;
///   * CROSS-MULTIPLIES into a sign-aware INTEGER inequality and rewrites it into
///     the multiply-by-positive shape `M * r < M * 2^(width-1)`;
///   * derives the exponent link `2^(e_x) = 2^(e_x-i+1) * 2^(i-1)` GENERICALLY
///     (one sign case-split, each arm CITING the power-of-two homomorphism pool
///     law) and lets `grind` discharge the ring rewrite with it;
///   * closes via the cited floor window (`r < 2^(width-1)`) and the
///     multiply-by-positive rung in `aver_int_order`.
///
/// Returns `None` (→ legacy `fpValue` arm / bounded fallback) when the value is
/// not the signed-power-of-two × fraction form.
fn emit_rational_floor_bound_general(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
) -> Option<AutoProof> {
    use crate::ast::Expr;
    let pow = rf_pow2_fn(vb, law, ctx)?;
    let inlined = inline_fn_call(&law.lhs, ctx)?;
    let Expr::FnCall(_lt, lt_args) = &inlined.node else {
        return None;
    };
    if lt_args.len() != 2 {
        return None;
    }
    let Expr::FnCall(_abs, abs_args) = &lt_args[0].node else {
        return None;
    };
    let err_call = abs_args.first()?;
    let Expr::FnCall(_ps, ps_args) = &lt_args[1].node else {
        return None;
    };
    let le_expr = ps_args.first()?;
    let Expr::FnCall(_te, te_args) = &err_call.node else {
        return None;
    };
    if te_args.len() != 2 {
        return None;
    }
    let f_name = expr_var_name(&te_args[0].node)?.to_string();
    let i_name = expr_var_name(&te_args[1].node)?.to_string();
    // truncError ↦ minus(fpValueGeneral(F), fpValueGeneral(G)).
    let minus_inlined = rf_inline_fn_call(err_call, ctx)?;
    let Expr::FnCall(_m, m_args) = &minus_inlined.node else {
        return None;
    };
    if m_args.len() != 2 {
        return None;
    }
    let rounded_call = match &m_args[1].node {
        Expr::FnCall(_, fv_args) => fv_args.first()?,
        _ => return None,
    };
    // fpValueGeneral(F) ↦ times(SGN(exp), Fraction): read the field accessors.
    let fpv_f = rf_inline_fn_call(&m_args[0], ctx)?;
    let gf = rf_general_value_fields(&fpv_f, ctx, &pow)?;
    // rounded(F, I) ↦ Fp record: the rounded significand integer (the floor q).
    let rounded_rec = rf_inline_fn_call(rounded_call, ctx)?;
    let Expr::RecordCreate {
        fields: rf_fields, ..
    } = &rounded_rec.node
    else {
        return None;
    };
    let q_spanned = rf_fields
        .iter()
        .find(|(n, _)| n == "sigBits")
        .map(|(_, e)| e)?;
    let q = rf_emit(ctx, q_spanned);
    let le = rf_emit(ctx, le_expr);

    // Cited pool laws: the floor window (strict remainder bound) and the
    // power-of-two homomorphism (the exponent link, generically sign-split).
    let (window_thm, window_fn) = rf_window_law(vb, law, ctx)?;
    let hom = rf_homomorphism_name(ctx, &pow)?;

    // The format predicate conjunct (its leading disjunction is the float sign).
    let when = law.when.as_ref()?;
    let conjs = rf_flatten_bool_and(when);
    let sign_conj = rf_sign_conjunct(law, ctx)?;
    let fmt_idx = conjs
        .iter()
        .position(|c| arith_eq(&c.node, &sign_conj.node))?;
    let isfp_lean = match &sign_conj.node {
        Expr::FnCall(callee, _) => {
            aver_name_to_lean(&super::super::shared::expr_dotted_name(callee)?)
        }
        _ => return None,
    };
    let n = conjs.len();
    let mut pat = "h_rfbp0".to_string();
    for k in 1..n {
        pat = format!("⟨{pat}, h_rfbp{k}⟩");
    }
    let fmtname = format!("h_rfbp{fmt_idx}");

    // Cone simp set EXCLUDING the signed power of two (kept abstract) — the
    // recursive `pow` and the floor wrapper are already excluded by the filter.
    let sgn_base = rf_bare_basename(&gf.sgn_fn).to_string();
    let defs = rf_filtered_defs(ctx, vb, law)
        .into_iter()
        .filter(|d| rf_bare_basename(d) != sgn_base)
        .collect::<Vec<_>>()
        .join(", ");

    // Power-of-two + signed-power positivity support, scoped to a fresh prefix.
    let base = format!("{}_law_{}__rfb", aver_name_to_lean(&vb.fn_name), law.name);
    let support_lines: Vec<String> =
        super::super::floor_window::pow2_signed_pos_support(&base, &pow, &gf.sgn_fn)
            .lines()
            .map(|l| l.to_string())
            .collect();

    // Derived sub-expressions (all from the cone defs, so they parse-match the
    // unfolded goal). `pe*` are the shared signed power of two at `exp`; `pl*`
    // the bound's signed power of two at `le`.
    let exp = gf.exp.clone();
    let sgn = gf.sgn_fn.clone();
    let pet = format!("({sgn} ({exp})).top");
    let peb = format!("({sgn} ({exp})).bottom");
    let plt = format!("({sgn} ({le})).top");
    let plb = format!("({sgn} ({le})).bottom");
    let pw = format!("{pow} ({} - 1)", gf.width);
    let pp = format!("{pow} ({i_name} - 1)");
    let sg = &gf.sign;
    let s = &gf.sig;
    let r = format!("{s} * {pp} - {pw} * {q}");
    let minustop =
        format!("{pet} * ({sg} * {s}) * ({peb} * {pp}) - {pet} * ({sg} * {q}) * ({peb} * {pw})");
    let t_abs = format!("{pet} * {peb} * ({r})");
    let absbot = format!("{peb} * {pw} * ({peb} * {pp})");
    let m = format!("{pet} * {peb} * ({absbot}) * {plb} * {plb}");

    let intro = format!("  intro {} h_when", intro_names.join(" "));
    let mut body: Vec<String> = vec![intro, "  first".to_string(), "  | (".to_string()];
    body.push("    simp only [Bool.and_eq_true, decide_eq_true_eq] at h_when".to_string());
    body.push(format!("    obtain {pat} := h_when"));
    body.push(format!("    have hpp : 0 < {pp} := {base}__pow_pos _"));
    body.push(format!("    have hpw : 0 < {pw} := {base}__pow_pos _"));
    body.push(format!("    have hPSeT : 0 < {pet} := {base}__sgnt_pos _"));
    body.push(format!("    have hPSeB : 0 < {peb} := {base}__sgnb_pos _"));
    body.push(format!("    have hPSlT : 0 < {plt} := {base}__sgnt_pos _"));
    body.push(format!("    have hPSlB : 0 < {plb} := {base}__sgnb_pos _"));
    body.push(format!("    have hsign : {sg} = 1 ∨ {sg} = -1 := by unfold {isfp_lean} at {fmtname}; simp only [Bool.and_eq_true, Bool.or_eq_true, beq_iff_eq, decide_eq_true_eq] at {fmtname}; exact {fmtname}.1"));
    body.push(format!("    have h_win := {window_thm} {f_name} {i_name}"));
    body.push(format!(
        "    simp only [{window_fn}, Bool.and_eq_true, decide_eq_true_eq] at h_win"
    ));
    body.push(format!(
        "    have hr0 : 0 ≤ {r} := by have := h_win.1; omega"
    ));
    body.push(format!(
        "    have hrW : {r} < {pw} := by have hexp : {pw} * ({q} + 1) = {pw} * {q} + {pw} := (by rw [Int.mul_add, Int.mul_one]); have := h_win.2; omega"
    ));
    // The exponent link 2^exp = 2^le * 2^(i-1), derived GENERICALLY by one
    // exponent-sign case-split, each arm CITING the integer power-of-two
    // homomorphism on nonnegative arguments.
    body.push(format!(
        "    have hlink : {pet} * {plb} = {plt} * {pp} * {peb} := by"
    ));
    body.push(format!("      by_cases hE : {exp} < 0"));
    body.push(format!("      · have hLE : {le} < 0 := by omega"));
    body.push(format!(
        "        have hh := {hom} ({i_name} - 1) (0 - {exp}) (by simp only [Bool.and_eq_true, ge_iff_le, decide_eq_true_eq]; omega)"
    ));
    body.push(format!(
        "        rw [show ({i_name} - 1) + (0 - {exp}) = 0 - ({le}) by omega] at hh"
    ));
    body.push(format!(
        "        unfold {sgn}; rw [if_pos hE, if_pos hLE]; grind"
    ));
    body.push(format!("      · by_cases hLE : {le} < 0"));
    body.push(format!(
        "        · have hh := {hom} {exp} (0 - ({le})) (by simp only [Bool.and_eq_true, ge_iff_le, decide_eq_true_eq]; omega)"
    ));
    body.push(format!(
        "          rw [show {exp} + (0 - ({le})) = {i_name} - 1 by omega] at hh"
    ));
    body.push(format!(
        "          unfold {sgn}; rw [if_neg hE, if_pos hLE]; grind"
    ));
    body.push(format!(
        "        · have hh := {hom} ({le}) ({i_name} - 1) (by simp only [Bool.and_eq_true, ge_iff_le, decide_eq_true_eq]; omega)"
    ));
    body.push(format!(
        "          rw [show ({le}) + ({i_name} - 1) = {exp} by omega] at hh"
    ));
    body.push(format!(
        "          unfold {sgn}; rw [if_neg hE, if_neg hLE]; grind"
    ));
    body.push(format!("    simp only [{defs}, decide_eq_true_eq]"));
    body.push(format!(
        "    have habsN : (if {minustop} < 0 then 0 - ({minustop}) else {minustop}) = {t_abs} := by have hfact : {minustop} = {sg} * ({t_abs}) := (by grind); have hnn : 0 ≤ {t_abs} := Int.mul_nonneg (Int.mul_nonneg (Int.le_of_lt hPSeT) (Int.le_of_lt hPSeB)) hr0; rw [hfact]; rcases hsign with h | h <;> rw [h] <;> split <;> omega"
    ));
    body.push("    rw [habsN]".to_string());
    body.push(format!(
        "    have hBotpos : 0 < {absbot} := by aver_int_order"
    ));
    body.push(format!("    rw [if_neg (show ¬ ({absbot} < 0) by omega)]"));
    body.push(format!(
        "    have hL : {t_abs} * ({absbot}) * ({plb} * {plb}) = ({m}) * ({r}) := by grind"
    ));
    body.push(format!(
        "    have hR : {plt} * {plb} * (({absbot}) * ({absbot})) = ({m}) * {pw} := by grind"
    ));
    body.push("    rw [hL, hR]".to_string());
    body.push("    apply Int.mul_lt_mul_of_pos_left hrW".to_string());
    body.push("    aver_int_order".to_string());
    body.push("  )".to_string());

    let floor = if super::super::super::tactic_ir::speculative::probing() {
        let id = format!("{}.{}", vb.fn_name, law.name);
        super::super::super::tactic_ir::speculative::record_probed(&id);
        format!("  | (trace \"AVERSPEC_SORRY:{id}\"; sorry)")
    } else {
        "  | sorry".to_string()
    };
    body.push(floor);

    Some(AutoProof {
        support_lines,
        body: Tactic::raw(body),
        replaces_theorem: false,
    })
}

/// The all-exponent rational SIGN-condition arm for the rounding-error sign
/// law (Lemma 7.2.2's same-sign half) over the GENERAL-exponent value: the error
/// `epsilon` has the sign of `x` — `match isNonNeg(fpValueGeneral(F)) { true =>
/// isNonNeg(epsilon) ; false => isNonPos(epsilon) }`. The value's signed product
/// `x.top·x.bottom` is `sign · A` with `A = (pow2Signed e_x).top·(pow2Signed
/// e_x).bottom·sigBits·2^(width-1) > 0`, and the error's signed product is `sign ·
/// B` with `B = (… · r · …) ≥ 0` (the same shared signed power of two and the
/// nonneg floor remainder `r`). So whichever way the value's sign falls, the error
/// matches it. The arm supplies the signed-power positivity (so the signed power
/// of two stays abstract), the floor window (`r ≥ 0`) and `isFp`'s `sigBits > 0`,
/// factors both products through `sign`, and closes by the sign case-split + the
/// `if`/`decide` split + `omega`. Returns `None` (→ generic Sign arm) when the
/// value is not the signed-power-of-two × fraction form.
fn emit_rational_floor_sign_general(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
) -> Option<AutoProof> {
    use crate::ast::Expr;
    let pow = rf_pow2_fn(vb, law, ctx)?;
    let inlined = inline_fn_call(&law.lhs, ctx)?;
    let Expr::Match { subject, arms } = &inlined.node else {
        return None;
    };
    if arms.len() != 2 {
        return None;
    }
    // subject = isNonNeg(fpValueGeneral(F)).
    let Expr::FnCall(_nn, nn_args) = &subject.node else {
        return None;
    };
    let value_call = nn_args.first()?;
    let fpv_f = rf_inline_fn_call(value_call, ctx)?;
    let gf = rf_general_value_fields(&fpv_f, ctx, &pow)?;
    // arms[0].body = isNonNeg(truncError(F, I)); get the error call.
    let Expr::FnCall(_, a0_args) = &arms[0].body.node else {
        return None;
    };
    let err_call = a0_args.first()?;
    let Expr::FnCall(_, te_args) = &err_call.node else {
        return None;
    };
    if te_args.len() != 2 {
        return None;
    }
    let f_name = expr_var_name(&te_args[0].node)?.to_string();
    let i_name = expr_var_name(&te_args[1].node)?.to_string();
    // truncError ↦ minus(fpValueGeneral(F), fpValueGeneral(G)); read the floor q.
    let minus_inlined = rf_inline_fn_call(err_call, ctx)?;
    let Expr::FnCall(_m, m_args) = &minus_inlined.node else {
        return None;
    };
    if m_args.len() != 2 {
        return None;
    }
    let rounded_call = match &m_args[1].node {
        Expr::FnCall(_, fv_args) => fv_args.first()?,
        _ => return None,
    };
    let rounded_rec = rf_inline_fn_call(rounded_call, ctx)?;
    let Expr::RecordCreate {
        fields: rf_fields, ..
    } = &rounded_rec.node
    else {
        return None;
    };
    let q_spanned = rf_fields
        .iter()
        .find(|(n, _)| n == "sigBits")
        .map(|(_, e)| e)?;
    let q = rf_emit(ctx, q_spanned);

    let (window_thm, window_fn) = rf_window_law(vb, law, ctx)?;
    let when = law.when.as_ref()?;
    let conjs = rf_flatten_bool_and(when);
    let sign_conj = rf_sign_conjunct(law, ctx)?;
    let fmt_idx = conjs
        .iter()
        .position(|c| arith_eq(&c.node, &sign_conj.node))?;
    let isfp_lean = match &sign_conj.node {
        Expr::FnCall(callee, _) => {
            aver_name_to_lean(&super::super::shared::expr_dotted_name(callee)?)
        }
        _ => return None,
    };
    let n = conjs.len();
    let mut pat = "h_rfbp0".to_string();
    for k in 1..n {
        pat = format!("⟨{pat}, h_rfbp{k}⟩");
    }
    let fmtname = format!("h_rfbp{fmt_idx}");

    let sgn_base = rf_bare_basename(&gf.sgn_fn).to_string();
    let defs = rf_filtered_defs(ctx, vb, law)
        .into_iter()
        .filter(|d| rf_bare_basename(d) != sgn_base)
        .collect::<Vec<_>>()
        .join(", ");
    let base = format!("{}_law_{}__rfs", aver_name_to_lean(&vb.fn_name), law.name);
    let support_lines: Vec<String> =
        super::super::floor_window::pow2_signed_pos_support(&base, &pow, &gf.sgn_fn)
            .lines()
            .map(|l| l.to_string())
            .collect();

    let exp = gf.exp.clone();
    let pet = format!("({} ({exp})).top", gf.sgn_fn);
    let peb = format!("({} ({exp})).bottom", gf.sgn_fn);
    let pw = format!("{pow} ({} - 1)", gf.width);
    let pp = format!("{pow} ({i_name} - 1)");
    let sg = &gf.sign;
    let s = &gf.sig;
    let r = format!("{s} * {pp} - {pw} * {q}");
    let minustop =
        format!("{pet} * ({sg} * {s}) * ({peb} * {pp}) - {pet} * ({sg} * {q}) * ({peb} * {pw})");
    let absbot = format!("{peb} * {pw} * ({peb} * {pp})");
    let vprod = format!("{pet} * ({sg} * {s}) * ({peb} * {pw})");
    let a_pos = format!("{pet} * {peb} * {s} * {pw}");
    let teprod = format!("({minustop}) * ({absbot})");
    let b_nn = format!("{pet} * {peb} * ({r}) * ({absbot})");

    let intro = format!("  intro {} h_when", intro_names.join(" "));
    let mut body: Vec<String> = vec![intro, "  first".to_string(), "  | (".to_string()];
    body.push("    simp only [Bool.and_eq_true, decide_eq_true_eq] at h_when".to_string());
    body.push(format!("    obtain {pat} := h_when"));
    body.push(format!("    have hpp : 0 < {pp} := {base}__pow_pos _"));
    body.push(format!("    have hpw : 0 < {pw} := {base}__pow_pos _"));
    body.push(format!("    have hPSeT : 0 < {pet} := {base}__sgnt_pos _"));
    body.push(format!("    have hPSeB : 0 < {peb} := {base}__sgnb_pos _"));
    body.push(format!("    have hfpu := {fmtname}"));
    body.push(format!("    unfold {isfp_lean} at hfpu"));
    body.push(
        "    simp only [Bool.and_eq_true, Bool.or_eq_true, beq_iff_eq, decide_eq_true_eq] at hfpu"
            .to_string(),
    );
    body.push(format!("    have hsign : {sg} = 1 ∨ {sg} = -1 := hfpu.1"));
    body.push(format!(
        "    have hsig : 0 < {s} := by have := hfpu.2.1; omega"
    ));
    body.push(format!("    have h_win := {window_thm} {f_name} {i_name}"));
    body.push(format!(
        "    simp only [{window_fn}, Bool.and_eq_true, decide_eq_true_eq] at h_win"
    ));
    body.push(format!(
        "    have hr0 : 0 ≤ {r} := by have := h_win.1; omega"
    ));
    body.push(format!("    simp only [{defs}, decide_eq_true_eq]"));
    body.push(format!(
        "    have hVfact : {vprod} = {sg} * ({a_pos}) := by grind"
    ));
    body.push(format!(
        "    have hTfact : {teprod} = {sg} * ({b_nn}) := by grind"
    ));
    body.push(format!("    have hVA : 0 < {a_pos} := by aver_int_order"));
    body.push(format!("    have hTB : 0 ≤ {b_nn} := by aver_int_order"));
    body.push("    simp only [hVfact, hTfact]".to_string());
    body.push("    rcases hsign with h | h <;> simp only [h] <;> split <;> simp only [decide_eq_true_eq] <;> omega".to_string());
    body.push("  )".to_string());
    let floor = if super::super::super::tactic_ir::speculative::probing() {
        let id = format!("{}.{}", vb.fn_name, law.name);
        super::super::super::tactic_ir::speculative::record_probed(&id);
        format!("  | (trace \"AVERSPEC_SORRY:{id}\"; sorry)")
    } else {
        "  | sorry".to_string()
    };
    body.push(floor);

    Some(AutoProof {
        support_lines,
        body: Tactic::raw(body),
        replaces_theorem: false,
    })
}

fn emit_rational_floor_bound(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
) -> Option<AutoProof> {
    use crate::ast::Expr;
    // A PIECEWISE rounded value (record-returning match — `away`/`sticky`) takes
    // the matched-leaf rung; a single clean record (`trunc`) takes the
    // single-record general rung; the legacy clamped value keeps the arm below.
    if let Some(p) = emit_rational_floor_bound_matched(vb, law, ctx, intro_names) {
        return Some(p);
    }
    // The general-exponent value (fpValueGeneral) takes the signed strict-order
    // rung; the legacy clamped value (fpValue) keeps the arm below.
    if let Some(p) = emit_rational_floor_bound_general(vb, law, ctx, intro_names) {
        return Some(p);
    }
    let pow = rf_pow2_fn(vb, law, ctx)?;
    // Walk the recognized shape:
    //   lessThan(absFraction(truncError(F, I)), pow2Signed(LE))
    //   truncError(F, I)  ↦  minus(fpValue(F), fpValue(G)),  G = rounded(F, I)
    let inlined = inline_fn_call(&law.lhs, ctx)?;
    let Expr::FnCall(_lt, lt_args) = &inlined.node else {
        return None;
    };
    if lt_args.len() != 2 {
        return None;
    }
    let Expr::FnCall(_abs, abs_args) = &lt_args[0].node else {
        return None;
    };
    let err_call = abs_args.first()?;
    let Expr::FnCall(_ps, ps_args) = &lt_args[1].node else {
        return None;
    };
    let le_expr = ps_args.first()?;
    // truncError(F, I): grab the float / precision givens directly.
    let Expr::FnCall(_te, te_args) = &err_call.node else {
        return None;
    };
    if te_args.len() != 2 {
        return None;
    }
    let f_name = expr_var_name(&te_args[0].node)?.to_string();
    let i_name = expr_var_name(&te_args[1].node)?.to_string();
    // truncError ↦ minus(fpValue(F), fpValue(G)).
    let minus_inlined = rf_inline_fn_call(err_call, ctx)?;
    let Expr::FnCall(_m, m_args) = &minus_inlined.node else {
        return None;
    };
    if m_args.len() != 2 {
        return None;
    }
    let rounded_call = match &m_args[1].node {
        Expr::FnCall(_, fv_args) => fv_args.first()?,
        _ => return None,
    };
    // fpValue(F) ↦ Fraction record: read the field accessors off it.
    let fpv_f = rf_inline_fn_call(&m_args[0], ctx)?;
    let fields = rf_fpvalue_fields(&fpv_f, ctx)?;
    // rounded(F, I) ↦ Fp record: the rounded significand integer (the floor q).
    let rounded_rec = rf_inline_fn_call(rounded_call, ctx)?;
    let Expr::RecordCreate {
        fields: rf_fields, ..
    } = &rounded_rec.node
    else {
        return None;
    };
    let q_spanned = rf_fields
        .iter()
        .find(|(n, _)| n == "sigBits")
        .map(|(_, e)| e)?;
    let q = rf_emit(ctx, q_spanned);
    let le = rf_emit(ctx, le_expr);

    // The cited pool laws.
    let (window_thm, window_fn) = rf_window_law(vb, law, ctx)?;
    let hom = rf_homomorphism_name(ctx, &pow)?;
    let defs = rf_filtered_defs(ctx, vb, law).join(", ");

    // The format predicate conjunct (its leading disjunction is the float sign).
    let when = law.when.as_ref()?;
    let conjs = rf_flatten_bool_and(when);
    let sign_conj = rf_sign_conjunct(law, ctx)?;
    let fmt_idx = conjs
        .iter()
        .position(|c| arith_eq(&c.node, &sign_conj.node))?;
    let isfp_lean = match &sign_conj.node {
        Expr::FnCall(callee, _) => {
            aver_name_to_lean(&super::super::shared::expr_dotted_name(callee)?)
        }
        _ => return None,
    };
    let n = conjs.len();
    let mut pat = "h_rfbp0".to_string();
    for k in 1..n {
        pat = format!("⟨{pat}, h_rfbp{k}⟩");
    }
    let fmtname = format!("h_rfbp{fmt_idx}");

    // The power-of-two positivity support, scoped to a fresh per-law prefix.
    let base = format!("{}_law_{}__rfb", aver_name_to_lean(&vb.fn_name), law.name);
    let support_lines: Vec<String> = super::super::floor_window::pow_pos_support(&base, &pow)
        .lines()
        .map(|l| l.to_string())
        .collect();

    // Derived sub-expression strings (all from the cone defs, so they parse-match
    // the unfolded goal).
    let pe = format!("{pow} ({})", fields.exp);
    let pw = format!("{pow} ({} - 1)", fields.width);
    let pp = format!("{pow} ({i_name} - 1)");
    let pr = format!("{pow} (0 - ({le}))");
    let ple = format!("{pow} ({le})");
    let sgn = &fields.sign;
    let sig = &fields.sig;
    let exa = &fields.exp;
    let r = format!("{sig} * {pp} - {pw} * {q}");
    let num = format!("{sgn} * {sig} * {pe} * {pp} - {sgn} * {q} * {pe} * {pw}");

    let intro = format!("  intro {} h_when", intro_names.join(" "));
    let mut body: Vec<String> = vec![intro, "  first".to_string(), "  | (".to_string()];
    body.push("    simp only [Bool.and_eq_true, decide_eq_true_eq] at h_when".to_string());
    body.push(format!("    obtain {pat} := h_when"));
    body.push(format!("    have hP : 0 < {pp} := {base}__pow_pos _"));
    body.push(format!("    have hW : 0 < {pw} := {base}__pow_pos _"));
    body.push(format!("    have hpe : 0 < {pe} := {base}__pow_pos _"));
    body.push(format!(
        "    have hWP : 0 < {pw} * {pp} := Int.mul_pos hW hP"
    ));
    body.push(format!("    have h_win := {window_thm} {f_name} {i_name}"));
    body.push(format!(
        "    simp only [{window_fn}, Bool.and_eq_true, decide_eq_true_eq] at h_win"
    ));
    body.push(format!(
        "    have hr0 : 0 ≤ {r} := by have := h_win.1; omega"
    ));
    body.push(format!(
        "    have hrW : {r} < {pw} := by have hexp : {pw} * ({q} + 1) = {pw} * {q} + {pw} := (by rw [Int.mul_add, Int.mul_one]); have := h_win.2; omega"
    ));
    body.push(format!("    have hsign : {sgn} = 1 ∨ {sgn} = -1 := by unfold {isfp_lean} at {fmtname}; simp only [Bool.and_eq_true, Bool.or_eq_true, beq_iff_eq, decide_eq_true_eq] at {fmtname}; exact {fmtname}.1"));
    body.push(format!("    simp only [{defs}, decide_eq_true_eq]"));
    body.push(format!(
        "    have habsN : (if {num} < 0 then 0 - ({num}) else {num}) = {pe} * ({r}) := by have hfact : {num} = {sgn} * ({pe} * ({r})) := (by grind); have hnn : 0 ≤ {pe} * ({r}) := Int.mul_nonneg (by omega) hr0; rw [hfact]; rcases hsign with h | h <;> rw [h] <;> split <;> omega"
    ));
    body.push("    rw [habsN]".to_string());
    body.push(format!(
        "    rw [if_neg (show ¬ ({pw} * {pp} < 0) by omega)]"
    ));
    body.push(format!("    by_cases hk : {le} < 0"));
    // k < 0 branch
    body.push("    · rw [if_pos hk]; dsimp only".to_string());
    body.push(format!("      have hRb : 0 < {pr} := {base}__pow_pos _"));
    body.push(format!(
        "      have hlink : {pe} * {pr} = {pp} := by have h := {hom} ({exa}) (0 - ({le})) (by simp only [Bool.and_eq_true, ge_iff_le, decide_eq_true_eq]; omega); rw [show {exa} + (0 - ({le})) = {i_name} - 1 by omega] at h; omega"
    ));
    body.push(format!(
        "      have lhs_eq : {pe} * ({r}) * ({pw} * {pp}) * ({pr} * {pr}) = {pe} * ({pw} * {pp}) * ({pr} * {pr}) * ({r}) := by grind"
    ));
    body.push(format!(
        "      have rhs_eq : 1 * {pr} * (({pw} * {pp}) * ({pw} * {pp})) = {pe} * ({pw} * {pp}) * ({pr} * {pr}) * {pw} := by rw [← hlink]; grind"
    ));
    body.push("      rw [lhs_eq, rhs_eq]; aver_int_order".to_string());
    // k >= 0 branch
    body.push("    · rw [if_neg hk]; dsimp only".to_string());
    body.push(format!(
        "      have hlink : {ple} * {pp} = {pe} := by have h := {hom} ({le}) ({i_name} - 1) (by simp only [Bool.and_eq_true, ge_iff_le, decide_eq_true_eq]; omega); rw [show ({le}) + ({i_name} - 1) = {exa} by omega] at h; omega"
    ));
    body.push(format!(
        "      have lhs_eq : {pe} * ({r}) * ({pw} * {pp}) * (1 * 1) = {pe} * ({pw} * {pp}) * ({r}) := by grind"
    ));
    body.push(format!(
        "      have rhs_eq : {ple} * 1 * (({pw} * {pp}) * ({pw} * {pp})) = {pe} * ({pw} * {pp}) * {pw} := by rw [← hlink]; grind"
    ));
    body.push("      rw [lhs_eq, rhs_eq]; aver_int_order".to_string());
    body.push("  )".to_string());

    let floor = if super::super::super::tactic_ir::speculative::probing() {
        let id = format!("{}.{}", vb.fn_name, law.name);
        super::super::super::tactic_ir::speculative::record_probed(&id);
        format!("  | (trace \"AVERSPEC_SORRY:{id}\"; sorry)")
    } else {
        "  | sorry".to_string()
    };
    body.push(floor);

    Some(AutoProof {
        support_lines,
        body: Tactic::raw(body),
        replaces_theorem: false,
    })
}

/// The rational-over-floor sign/magnitude keystone arm. Returns the full
/// auto-proof (support stack + body) or `None` to fall through to the generic
/// keystone emission.
pub(super) fn emit_rational_floor_family(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
) -> Option<AutoProof> {
    let shape = rational_floor_shape(vb, law, ctx)?;
    // The rational-order truncation-error bound (Lemma 7.2.2) has its own
    // dedicated arm: factor + sign-split + cite-window + cite-homomorphism +
    // multiply-by-positive, none of which the NonnegPos / Sign skeletons cover.
    if shape == RationalFloorShape::Bound {
        return emit_rational_floor_bound(vb, law, ctx, intro_names);
    }
    // The general-exponent same-sign condition (Lemma 7.2.2 sign half over
    // fpValueGeneral) takes its own signed-power-of-two arm; the legacy clamped
    // value keeps the generic Sign skeleton below.
    if shape == RationalFloorShape::Sign
        && let Some(p) = emit_rational_floor_sign_general(vb, law, ctx, intro_names)
    {
        return Some(p);
    }
    let pow = rf_pow2_fn(vb, law, ctx)?;
    let citations = rf_citations(vb, law, ctx, intro_names);
    let defs = rf_filtered_defs(ctx, vb, law).join(", ");
    let intro = format!("  intro {} h_when", intro_names.join(" "));

    // Citation `have`s (shared by both shapes): bring each cited law's order
    // content into context, unfolded to its Int facts.
    let mut steps: Vec<String> = Vec::new();
    for c in &citations {
        steps.push(format!("have {} := {}", c.have_name, c.apply));
        steps.push(format!("simp only [{}] at {}", c.simp_set, c.have_name));
    }

    let mut support_lines: Vec<String> = Vec::new();
    let arm: String = match shape {
        RationalFloorShape::NonnegPos => {
            // Power-of-two positivity at the record-field exponents.
            let base = format!("{}_law_{}__rfpp", aver_name_to_lean(&vb.fn_name), law.name);
            support_lines = super::super::floor_window::pow_pos_support(&base, &pow)
                .lines()
                .map(|l| l.to_string())
                .collect();
            let mut idx = 0usize;
            let mut push_pow = |steps: &mut Vec<String>, expr: String| {
                steps.push(format!("have h_rfpp{idx} := {base}__pow_pos ({expr})"));
                idx += 1;
            };
            for g in &law.givens {
                let gl = aver_name_to_lean(&g.name);
                if g.type_name.trim() == "Int" {
                    push_pow(&mut steps, gl.clone());
                    push_pow(&mut steps, format!("{gl} - 1"));
                } else {
                    for fld in rf_record_int_fields(ctx, &g.type_name) {
                        push_pow(&mut steps, format!("{gl}.{fld}"));
                        push_pow(&mut steps, format!("{gl}.{fld} - 1"));
                    }
                }
            }
            let close = format!(
                "simp only [{defs}, Bool.and_eq_true, Bool.or_eq_true, ge_iff_le, gt_iff_lt, decide_eq_true_eq, beq_iff_eq] at h_when ⊢ <;> aver_int_order"
            );
            steps.push(close);
            format!("  | ({})", steps.join("; "))
        }
        RationalFloorShape::Sign => {
            // The sign/format predicate conjunct (`isFp`): `rcases` its leading
            // disjunction (the float sign) after unfolding it.
            let sign_conj = rf_sign_conjunct(law, ctx)?;
            let when = law.when.as_ref()?;
            let prem = rf_premise_proof(when, sign_conj)?;
            steps.push(format!("have h_rfsign := {prem}"));
            steps.push(format!(
                "simp only [{defs}, Bool.and_eq_true, Bool.or_eq_true, decide_eq_true_eq, beq_iff_eq] at h_rfsign"
            ));
            steps.push(format!("simp only [{defs}]"));
            steps.push("rcases h_rfsign.1 with hs | hs <;> simp only [hs] <;> grind".to_string());
            format!("  | ({})", steps.join("; "))
        }
        // Handled by the dedicated arm above (early return); never reached here.
        RationalFloorShape::Bound => {
            unreachable!("Bound shape handled by emit_rational_floor_bound")
        }
    };

    let floor = if super::super::super::tactic_ir::speculative::probing() {
        let id = format!("{}.{}", vb.fn_name, law.name);
        super::super::super::tactic_ir::speculative::record_probed(&id);
        format!("  | (trace \"AVERSPEC_SORRY:{id}\"; sorry)")
    } else {
        "  | sorry".to_string()
    };

    Some(AutoProof {
        support_lines,
        body: Tactic::raw(vec![intro, "  first".to_string(), arm, floor]),
        replaces_theorem: false,
    })
}

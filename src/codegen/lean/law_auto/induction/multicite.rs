//! Multi-citation composition arm — the generic "premises supplied by an
//! earlier universal, conclusion supplied by another" close.
//!
//! The keystone ([`super::keystone`]) closes a non-recursive law by a single
//! `grind` over a cited pool. That works when one citation (plus the cone)
//! finishes the goal, but it cannot ORCHESTRATE a composition where the
//! premises of one cited universal are produced by the CONCLUSION of an
//! earlier cited universal: `grind`'s e-matching never fires the supplying
//! universal (its conclusion head — e.g. an `entryEpsilonOk` table predicate —
//! does not appear anywhere in the goal), and it cannot fold the goal's
//! definitional alias back onto the applied universal's trigger.
//!
//! This arm emits that orchestration generically and name-blind:
//!
//!   1. split the `when`-premise conjunction into its atomic hypotheses;
//!   2. for EVERY earlier universal whose own premises are discharged by those
//!      hypotheses (a "supplier"), instantiate it (`apply <thm> <;> assumption`)
//!      and split its conclusion conjunction into context — its conjuncts are
//!      now concrete facts;
//!   3. `apply` the one earlier universal whose conclusion definitionally
//!      ALIASES this law's goal (the "applier"), and discharge each of its
//!      premises uniformly: directly (`assumption`, from a `when` hypothesis or
//!      a supplier conjunct), through the rational nonneg-of-positive pool law,
//!      or after a definitional `simp` that unfolds both the applier's predicate
//!      and the cone so an aliased premise matches a supplier conjunct.
//!
//! Everything is keyed on SHAPE — "an earlier universal whose conclusion
//! matches my goal modulo unfolding" and "an earlier universal whose premises
//! are a subset of mine" — never on a law, figure, or function name. It is
//! reusable for any composition of earlier universals with light glue (the K5
//! Section 8.2 remainder/digit lemmas each cite several earlier universals this
//! way). The whole proof sits under a `first | (…) | sorry` floor with the
//! speculative `AVERSPEC_SORRY` trace, so a law the recognizer admits but whose
//! orchestration does not actually close falls back to its bounded sampled
//! statement — credit stays fail-closed behind the `#print axioms` whitelist.

use super::super::super::expr::aver_name_to_lean;
use super::super::super::expr::emit_expr_legacy;
use super::super::super::tactic_ir::Tactic;
use super::super::AutoProof;
use super::super::shared::law_simp_defs;
use super::keystone::inline_fn_call;
use crate::ast::{Expr, Literal, Spanned, TopLevel, VerifyBlock, VerifyKind, VerifyLaw};
use crate::codegen::CodegenContext;
use std::collections::{HashMap, HashSet};

/// A planned orchestration for one `when`-law.
struct MultiCite {
    /// Pattern that destructures the `when`-conjunction into atomic hyps.
    when_obtain: String,
    /// The earlier universals whose premises this law's `when` supplies; each
    /// is instantiated and its conclusion split into context.
    suppliers: Vec<Supplier>,
    /// The earlier universal whose conclusion aliases this law's goal.
    applier_thm: String,
    /// `simp only` defs for the alias discharge: the law cone plus the
    /// applier's predicate def, so an applied premise unfolds to a supplier
    /// conjunct's normal form.
    alias_defs: Vec<String>,
}

/// One earlier universal whose premises are discharged by this law's `when`.
struct Supplier {
    /// Lean theorem name of the supplier universal.
    thm: String,
    /// The supplier's conclusion predicate (Lean fn name) to unfold.
    subj_lean: String,
    /// The applied conclusion `subj arg0 arg1 …`, parenthesised per arg.
    subj_applied: String,
    /// A unique hypothesis name for the instantiated conclusion.
    hyp: String,
    /// Conjunct destructuring pattern, or `None` when the conclusion is one fact.
    obtain: Option<String>,
}

/// The callee basename (last dotted component) of a function-call expression.
fn callee_basename(e: &Spanned<Expr>) -> Option<String> {
    let Expr::FnCall(callee, _) = &e.node else {
        return None;
    };
    let dotted = super::super::shared::expr_dotted_name(callee)?;
    Some(dotted.rsplit('.').next().unwrap_or(&dotted).to_string())
}

/// The variable a leaf names (source `Ident` or resolved binding), else `None`.
fn var_name(e: &Expr) -> Option<&str> {
    match e {
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => Some(n),
        _ => None,
    }
}

/// Two-argument `Bool.and` call (by basename), else `None`.
fn as_and(e: &Spanned<Expr>) -> Option<(&Spanned<Expr>, &Spanned<Expr>)> {
    let Expr::FnCall(callee, args) = &e.node else {
        return None;
    };
    let dotted = super::super::shared::expr_dotted_name(callee)?;
    if dotted.rsplit('.').next() != Some("and") || args.len() != 2 {
        return None;
    }
    Some((&args[0], &args[1]))
}

/// Build a destructuring pattern + leaf names for the left/right `Bool.and`
/// tree of `expr`, naming leaves `{prefix}{n}` in left-to-right order.
fn build_obtain(
    expr: &Spanned<Expr>,
    prefix: &str,
    counter: &mut usize,
    leaves: &mut Vec<(String, Spanned<Expr>)>,
) -> String {
    if let Some((l, r)) = as_and(expr) {
        let lp = build_obtain(l, prefix, counter, leaves);
        let rp = build_obtain(r, prefix, counter, leaves);
        format!("⟨{lp}, {rp}⟩")
    } else {
        let name = format!("{prefix}{}", *counter);
        *counter += 1;
        leaves.push((name.clone(), expr.clone()));
        name
    }
}

/// First-order match: does pattern `pat` (over `vars`) match ground `term`,
/// extending `subst` consistently? Span- and `Ident`/`Resolved`-insensitive.
fn unify(
    pat: &Expr,
    term: &Expr,
    vars: &HashSet<String>,
    subst: &mut HashMap<String, Expr>,
) -> bool {
    if let Some(pn) = var_name(pat)
        && vars.contains(pn)
    {
        return match subst.get(pn) {
            Some(prev) => same_term(prev, term),
            None => {
                subst.insert(pn.to_string(), term.clone());
                true
            }
        };
    }
    if let (Some(a), Some(b)) = (var_name(pat), var_name(term)) {
        return a == b;
    }
    match (pat, term) {
        (Expr::Literal(x), Expr::Literal(y)) => x == y,
        (Expr::Neg(a), Expr::Neg(b)) => unify(&a.node, &b.node, vars, subst),
        (Expr::BinOp(o1, a1, b1), Expr::BinOp(o2, a2, b2)) => {
            o1 == o2
                && unify(&a1.node, &a2.node, vars, subst)
                && unify(&b1.node, &b2.node, vars, subst)
        }
        (Expr::Attr(b1, f1), Expr::Attr(b2, f2)) => {
            f1 == f2 && unify(&b1.node, &b2.node, vars, subst)
        }
        (Expr::FnCall(c1, a1), Expr::FnCall(c2, a2)) => {
            a1.len() == a2.len()
                && unify(&c1.node, &c2.node, vars, subst)
                && a1
                    .iter()
                    .zip(a2)
                    .all(|(p, q)| unify(&p.node, &q.node, vars, subst))
        }
        _ => false,
    }
}

/// Span- and binding-insensitive structural equality on arithmetic terms.
fn same_term(a: &Expr, b: &Expr) -> bool {
    match (var_name(a), var_name(b)) {
        (Some(na), Some(nb)) => return na == nb,
        (None, None) => {}
        _ => return false,
    }
    match (a, b) {
        (Expr::Literal(x), Expr::Literal(y)) => x == y,
        (Expr::Neg(x), Expr::Neg(y)) => same_term(&x.node, &y.node),
        (Expr::BinOp(o1, a1, b1), Expr::BinOp(o2, a2, b2)) => {
            o1 == o2 && same_term(&a1.node, &a2.node) && same_term(&b1.node, &b2.node)
        }
        (Expr::Attr(b1, f1), Expr::Attr(b2, f2)) => f1 == f2 && same_term(&b1.node, &b2.node),
        (Expr::FnCall(c1, a1), Expr::FnCall(c2, a2)) => {
            a1.len() == a2.len()
                && same_term(&c1.node, &c2.node)
                && a1.iter().zip(a2).all(|(p, q)| same_term(&p.node, &q.node))
        }
        _ => false,
    }
}

/// Inline `e` one definitional level when it is a call to a NON-recursive
/// project function, else `None`. Recursive cone fns are left folded (their
/// symbolic recursion does not reduce).
fn inline_one(
    e: &Spanned<Expr>,
    ctx: &CodegenContext,
    recursive: &HashSet<String>,
) -> Option<Spanned<Expr>> {
    let base = callee_basename(e)?;
    if recursive.contains(&base) {
        return None;
    }
    inline_fn_call(e, ctx)
}

/// Match `pat` against `term` MODULO definitional unfolding: when heads differ,
/// unfold one side (whichever is an inlinable call) and retry. A pattern
/// variable absorbs whatever ground subterm sits at its position (so leaf cone
/// fns like a table lookup stay folded), while the shared algebraic skeleton
/// (the rational primitives, present on both sides) matches head-for-head. This
/// is the "goal aliases the applier's conclusion modulo unfolding" check.
fn match_modulo_unfold(
    pat: &Spanned<Expr>,
    term: &Spanned<Expr>,
    vars: &HashSet<String>,
    ctx: &CodegenContext,
    recursive: &HashSet<String>,
    fuel: usize,
    subst: &mut HashMap<String, Expr>,
) -> bool {
    if fuel == 0 {
        return false;
    }
    if let Some(pn) = var_name(&pat.node)
        && vars.contains(pn)
    {
        return match subst.get(pn) {
            Some(prev) => same_term(prev, &term.node),
            None => {
                subst.insert(pn.to_string(), term.node.clone());
                true
            }
        };
    }
    let heads_match = match (&pat.node, &term.node) {
        (Expr::Literal(x), Expr::Literal(y)) => return x == y,
        (Expr::Neg(a), Expr::Neg(b)) => {
            return match_modulo_unfold(a, b, vars, ctx, recursive, fuel - 1, subst);
        }
        (Expr::BinOp(o1, a1, b1), Expr::BinOp(o2, a2, b2)) if o1 == o2 => {
            return match_modulo_unfold(a1, a2, vars, ctx, recursive, fuel - 1, subst)
                && match_modulo_unfold(b1, b2, vars, ctx, recursive, fuel - 1, subst);
        }
        (Expr::Attr(b1, f1), Expr::Attr(b2, f2)) if f1 == f2 => {
            return match_modulo_unfold(b1, b2, vars, ctx, recursive, fuel - 1, subst);
        }
        (Expr::FnCall(c1, a1), Expr::FnCall(c2, a2)) => {
            let same = a1.len() == a2.len()
                && super::super::shared::expr_dotted_name(c1)
                    == super::super::shared::expr_dotted_name(c2);
            if same {
                return a1.iter().zip(a2).all(|(p, q)| {
                    match_modulo_unfold(p, q, vars, ctx, recursive, fuel - 1, subst)
                });
            }
            false
        }
        _ => false,
    };
    if heads_match {
        return true;
    }
    // Heads differ: unfold whichever side is an inlinable call and retry.
    if let Some(t2) = inline_one(term, ctx, recursive) {
        return match_modulo_unfold(pat, &t2, vars, ctx, recursive, fuel - 1, subst);
    }
    if let Some(p2) = inline_one(pat, ctx, recursive) {
        return match_modulo_unfold(&p2, term, vars, ctx, recursive, fuel - 1, subst);
    }
    false
}

/// Whether `prev`/`prev_law` is closed UNIVERSALLY by a DETERMINISTIC strategy
/// (one whose universal disposition does not depend on the speculative probe),
/// so its `{fn}_law_{name}` theorem is the clean `∀ … = true` form and can be
/// cited directly. Excludes the keystone and this very arm (both probe-gated),
/// which keeps a citation from referencing a theorem that re-emits bounded.
fn deterministically_universal(
    prev: &VerifyBlock,
    prev_law: &VerifyLaw,
    ctx: &CodegenContext,
) -> bool {
    if prev_law.when.is_some() {
        super::super::recognize_finite_int_domain(prev, prev_law, ctx)
            || super::super::recognize_interval_monotonicity(prev, prev_law, ctx)
            || super::recognize_conditional_comparison_bridge(prev_law, ctx)
            || super::recognize_conditional_inductive_generic(prev, prev_law, ctx)
            || matches!(
                super::super::law_strategy_for(ctx, &prev.fn_name, &prev_law.name),
                Some(
                    crate::ir::ProofStrategy::FloorDivWindow { .. }
                        | crate::ir::ProofStrategy::NonlinearNonneg { .. }
                        | crate::ir::ProofStrategy::TailRecFixedBaseFold { .. }
                )
            )
    } else {
        // A non-`when` universal law is exactly one law_as_lemma_statement
        // admits (givens present, not skip_universal, …).
        crate::codegen::lean::toplevel::law_as_lemma_statement(prev, prev_law, ctx).is_some()
    }
}

/// The `{fn}_law_{name}` theorem name for a cited law (the canonical universal
/// theorem name; the spec-`_eq_` variant only applies to equational non-`when`
/// spec laws, which this arm never cites).
fn law_theorem_name(prev: &VerifyBlock, prev_law: &VerifyLaw) -> String {
    format!(
        "{}_law_{}",
        aver_name_to_lean(&prev.fn_name),
        aver_name_to_lean(&prev_law.name)
    )
}

/// Render an applied call `fn (arg0) (arg1) …` for the cited conclusion's type.
fn render_applied(fn_lean: &str, args: &[Spanned<Expr>], ctx: &CodegenContext) -> String {
    let mut s = fn_lean.to_string();
    for a in args {
        let rendered = emit_expr_legacy(a, ctx, None);
        s.push_str(&format!(" ({rendered})"));
    }
    s
}

/// Try to plan `prev` as a SUPPLIER: every one of its `when`-premises matches
/// one of this law's `when` leaves (under one consistent substitution of the
/// supplier's givens), and the substitution determines its conclusion args.
fn plan_supplier(
    prev: &VerifyBlock,
    prev_law: &VerifyLaw,
    when_leaves: &[(String, Spanned<Expr>)],
    hyp: String,
    ctx: &CodegenContext,
) -> Option<Supplier> {
    let prev_when = prev_law.when.as_ref()?;
    // Supplier premises (flattened) as patterns over the supplier's givens.
    let mut prem_leaves = Vec::new();
    let mut c = 0usize;
    build_obtain(prev_when, "p", &mut c, &mut prem_leaves);
    if prem_leaves.is_empty() {
        return None;
    }
    let vars: HashSet<String> = prev_law.givens.iter().map(|g| g.name.clone()).collect();
    let mut subst: HashMap<String, Expr> = HashMap::new();
    for (_, prem) in &prem_leaves {
        let matched = when_leaves.iter().any(|(_, leaf)| {
            let mut trial = subst.clone();
            if unify(&prem.node, &leaf.node, &vars, &mut trial) {
                subst = trial;
                true
            } else {
                false
            }
        });
        if !matched {
            return None;
        }
    }
    // The supplier's conclusion subject call, with its args substituted.
    let Expr::FnCall(callee, sargs) = &prev_law.lhs.node else {
        return None;
    };
    let subj_dotted = super::super::shared::expr_dotted_name(callee)?;
    let subj_short = subj_dotted.rsplit('.').next().unwrap_or(&subj_dotted);
    if subj_short != prev.fn_name {
        return None;
    }
    let subj_lean = aver_name_to_lean(&prev.fn_name);
    let mut applied_args: Vec<Spanned<Expr>> = Vec::new();
    for a in sargs {
        let sub = super::keystone::substitute_idents(
            a,
            &subst
                .iter()
                .map(|(k, v)| (k.clone(), Spanned::bare(v.clone())))
                .collect(),
        );
        // Every given referenced by a conclusion arg must be bound.
        if mentions_unbound(&sub.node, &vars, &subst) {
            return None;
        }
        applied_args.push(sub);
    }
    let subj_applied = render_applied(&subj_lean, &applied_args, ctx);
    // Conjunct destructuring pattern from the supplier's (inlined) conclusion.
    let inlined = inline_fn_call(&prev_law.lhs, ctx)?;
    let mut leaves = Vec::new();
    let mut cc = 0usize;
    let pat = build_obtain(&inlined, "c", &mut cc, &mut leaves);
    let obtain = (leaves.len() > 1).then_some(pat);
    Some(Supplier {
        thm: law_theorem_name(prev, prev_law),
        subj_lean,
        subj_applied,
        hyp,
        obtain,
    })
}

/// Whether `e` references a supplier given that is not in `subst`.
fn mentions_unbound(e: &Expr, vars: &HashSet<String>, subst: &HashMap<String, Expr>) -> bool {
    if let Some(n) = var_name(e) {
        return vars.contains(n) && !subst.contains_key(n);
    }
    match e {
        Expr::BinOp(_, a, b) => {
            mentions_unbound(&a.node, vars, subst) || mentions_unbound(&b.node, vars, subst)
        }
        Expr::Neg(a) => mentions_unbound(&a.node, vars, subst),
        Expr::Attr(b, _) => mentions_unbound(&b.node, vars, subst),
        Expr::FnCall(c, args) => {
            mentions_unbound(&c.node, vars, subst)
                || args.iter().any(|a| mentions_unbound(&a.node, vars, subst))
        }
        _ => false,
    }
}

/// Plan the orchestration, or `None` when the shape does not apply. Liberal by
/// design: the speculative probe is the oracle (a plan whose proof does not
/// close falls back to bounded), but it requires the load-bearing structure —
/// an applier universal aliasing the goal AND at least one supplier universal.
fn plan(vb: &VerifyBlock, law: &VerifyLaw, ctx: &CodegenContext) -> Option<MultiCite> {
    law.when.as_ref()?;
    if !matches!(&law.rhs.node, Expr::Literal(Literal::Bool(true))) {
        return None;
    }
    let Expr::FnCall(_, _) = &law.lhs.node else {
        return None;
    };
    // Fail closed on refinement-lifted givens (mirrors the keystone gate): the
    // lifted statement path takes a different quantifier shape than `intro
    // <givens> h_when`.
    if law.givens.iter().any(|g| {
        crate::codegen::common::refinement_lift_for_given(
            &g.name,
            &g.type_name,
            &law.lhs,
            &law.rhs,
            ctx,
        )
        .is_some()
    }) {
        return None;
    }
    let when = law.when.as_ref()?;
    let mut leaves = Vec::new();
    let mut wc = 0usize;
    let when_obtain = build_obtain(when, "hw", &mut wc, &mut leaves);
    if leaves.len() < 2 {
        return None;
    }

    let recursive: HashSet<String> = crate::codegen::lean::recursive_pure_fn_names(ctx)
        .iter()
        .map(|n| n.rsplit('.').next().unwrap_or(n).to_string())
        .collect();

    // Goal body: the law's subject inlined one level.
    let goal_body = inline_fn_call(&law.lhs, ctx)?;

    // Scan earlier sibling universals. Find the applier (conclusion aliases the
    // goal) and the suppliers (premises ⊆ this law's `when`).
    let mut applier: Option<String> = None;
    let mut applier_subj_lean: Option<String> = None;
    let mut suppliers: Vec<Supplier> = Vec::new();
    let mut supplier_idx = 0usize;
    for item in &ctx.items {
        let TopLevel::Verify(prev) = item else {
            continue;
        };
        // Only EARLIER blocks (source order = emit order ⇒ no cyclic citation).
        if prev.line == vb.line && prev.fn_name == vb.fn_name {
            break;
        }
        let VerifyKind::Law(prev_law) = &prev.kind else {
            continue;
        };
        if !deterministically_universal(prev, prev_law, ctx) {
            continue;
        }
        // Applier candidate: its subject body aliases the goal body.
        if applier.is_none()
            && let Some(prev_subj_fd) =
                super::super::shared::find_fn_def_by_call_name(ctx, &prev.fn_name)
            && let Some(prev_body) = prev_subj_fd.body.tail_expr()
        {
            let vars: HashSet<String> =
                prev_subj_fd.params.iter().map(|(n, _)| n.clone()).collect();
            let mut subst = HashMap::new();
            if match_modulo_unfold(
                prev_body, &goal_body, &vars, ctx, &recursive, 64, &mut subst,
            ) {
                applier = Some(law_theorem_name(prev, prev_law));
                applier_subj_lean = Some(aver_name_to_lean(&prev.fn_name));
                continue;
            }
        }
        // Supplier candidate.
        let hyp = format!("hcite{supplier_idx}");
        if let Some(s) = plan_supplier(prev, prev_law, &leaves, hyp, ctx) {
            suppliers.push(s);
            supplier_idx += 1;
        }
    }

    let applier_thm = applier?;
    if suppliers.is_empty() {
        return None;
    }

    // Alias-discharge defs: the law cone (already `_root_`-qualified) plus the
    // applier's predicate (so an applied premise unfolds to a supplier
    // conjunct's normal form).
    let mut alias_defs: Vec<String> = law_simp_defs(ctx, vb, law).into_iter().collect();
    if let Some(subj) = &applier_subj_lean {
        alias_defs.push(subj.clone());
    }
    alias_defs.sort();
    alias_defs.dedup();

    Some(MultiCite {
        when_obtain,
        suppliers,
        applier_thm,
        alias_defs,
    })
}

/// Statement-builder hook (mirror of the emit): whether this law is closed
/// universally by the multi-citation arm. Probe-gated like the keystone.
pub(in crate::codegen::lean) fn recognize_multicite_composition(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> bool {
    if plan(vb, law, ctx).is_none() {
        return false;
    }
    let id = format!("{}.{}", vb.fn_name, law.name);
    super::super::super::tactic_ir::speculative::admits(&id, false)
}

/// Emit the multi-citation orchestration. Keeps the auto-generated universal
/// statement (`replaces_theorem = false`) and supplies the proof body.
pub(in crate::codegen::lean) fn emit_multicite_composition_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
) -> Option<AutoProof> {
    if !recognize_multicite_composition(vb, law, ctx) {
        return None;
    }
    let MultiCite {
        when_obtain,
        suppliers,
        applier_thm,
        alias_defs,
    } = plan(vb, law, ctx)?;

    let alias_csv = alias_defs.join(", ");

    // Proof body. Everything sits inside the `first` arm, so any slip falls to
    // the caught `sorry` floor.
    let mut arm: Vec<String> = Vec::new();
    arm.push(
        "  | (simp only [Bool.and_eq_true, decide_eq_true_eq, bne_iff_ne] at h_when".to_string(),
    );
    arm.push(format!("     obtain {when_obtain} := h_when"));
    for s in &suppliers {
        arm.push(format!(
            "     have {} : {} = true := by apply {} <;> assumption",
            s.hyp, s.subj_applied, s.thm
        ));
        arm.push(format!(
            "     simp only [{}, Bool.and_eq_true, decide_eq_true_eq, bne_iff_ne] at {}",
            s.subj_lean, s.hyp
        ));
        if let Some(pat) = &s.obtain {
            arm.push(format!("     obtain {pat} := {}", s.hyp));
        }
    }
    arm.push(format!("     apply {applier_thm} <;>"));
    arm.push("       first".to_string());
    arm.push("       | assumption".to_string());
    arm.push(
        "       | (apply Domain.Rational.isNonNeg_law_nonNegOfPositive <;> assumption)".to_string(),
    );
    arm.push(format!(
        "       | (simp only [{alias_csv}] at * <;> assumption))"
    ));

    let floor = if super::super::super::tactic_ir::speculative::probing() {
        let id = format!("{}.{}", vb.fn_name, law.name);
        super::super::super::tactic_ir::speculative::record_probed(&id);
        format!("  | (trace \"AVERSPEC_SORRY:{id}\"; sorry)")
    } else {
        "  | sorry".to_string()
    };

    let mut lines: Vec<String> = Vec::new();
    lines.push(format!("  intro {} h_when", intro_names.join(" ")));
    lines.push("  first".to_string());
    lines.extend(arm);
    lines.push(floor);

    Some(AutoProof {
        support_lines: Vec::new(),
        body: Tactic::raw(lines),
        replaces_theorem: false,
    })
}

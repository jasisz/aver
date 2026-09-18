//! Interface lemmas for a module-owned segment observation.
//!
//! Three shapes, all finite: a cursor predicate applied to one observation, an
//! observation compared with its own prefixed form, and one step of a protocol
//! observer written as an observation followed by the generic continuation.
//! Each is recognised from the shape of the law's statement alone — never from
//! what an observation computes — and each unfolds only the law's own wrappers,
//! the observation it is stated about and, for a step, one equation of the
//! observer being stepped. A lifted imported observation is never opened: the
//! cited cursor law is the only thing said about it.
use super::induction;
use crate::ast::{BinOp, Expr, FnDef, Literal, Spanned, Stmt, VerifyKind, VerifyLaw};
use crate::codegen::{CodegenContext, common};

/// Arithmetic and suffix steps every cursor proof needs, proved inline so the
/// rung adds no axiom and no prelude entry. `l` is the observed tape, `c` the
/// cursor the observation started from and `x` the cursor it reports.
const STEPS: &str = "have _aver_seg_lower : ∀ (c x j : Int), c ≤ x → 0 ≤ j → 0 ≤ x + j - c := (by intro c x j h hj; omega); \
have _aver_seg_upper_step : ∀ {α : Type} (l : List α) (c x : Int) (y : α) (tail : List α), c ≤ x → l.drop (x - c).toNat = y :: tail → x + 1 - c ≤ (l.length : Int) := (by intro α l c x y tail h0 h; have hlen := congrArg List.length h; simp only [List.length_drop, List.length_cons] at hlen; omega); \
have _aver_seg_drop_step : ∀ {α : Type} (l : List α) (c x : Int) (y : α) (tail : List α), c ≤ x → l.drop (x - c).toNat = y :: tail → tail = l.drop (x + 1 - c).toNat := (by intro α l c x y tail h0 h; have hk : (x + 1 - c).toNat = (x - c).toNat + 1 := (by omega); rw [hk, ← List.drop_drop, h]; rfl); \
have _aver_seg_drop_zero : ∀ {α : Type} (c : Int) (l : List α), l = l.drop (c - c).toNat := (by intro α c l; have h : (c - c).toNat = 0 := (by omega); rw [h, List.drop_zero]); \
have _aver_seg_upper_cons : ∀ {α : Type} (c : Int) (y : α) (tail : List α), c + 1 - c ≤ ((y :: tail).length : Int) := (by intro α c y tail; simp only [List.length_cons]; omega); \
have _aver_seg_drop_cons : ∀ {α : Type} (c : Int) (y : α) (tail : List α), tail = (y :: tail).drop (c + 1 - c).toNat := (by intro α c y tail; have h : (c + 1 - c).toNat = 1 := (by omega); rw [h]; rfl); \
have _aver_seg_link : ∀ (a b c : Int), b ≤ c → a ≤ b → a ≤ c := (by intro a b c h1 h2; omega); \
have _aver_seg_chain : ∀ {α : Type} (l : List α) (c x y : Int), y - x ≤ ((l.drop (x - c).toNat).length : Int) → c ≤ x → x - c ≤ (l.length : Int) → y - c ≤ (l.length : Int) := (by intro α l c x y h2 h0 h1; simp only [List.length_drop] at h2; omega); \
have _aver_seg_drop_chain : ∀ {α : Type} (l : List α) (c x y : Int), c ≤ x → x ≤ y → (l.drop (x - c).toNat).drop (y - x).toNat = l.drop (y - c).toNat := (by intro α l c x y h0 h1; have hk : (x - c).toNat + (y - x).toNat = (y - c).toNat := (by omega); rw [List.drop_drop, hk]); ";

/// A helper the rung may unfold: effect free, not recursive, and owned by a
/// scope the law can see.
fn is_finite(fd: &FnDef, ctx: &CodegenContext) -> bool {
    common::fn_id_for_decl(ctx, fd)
        .is_some_and(|id| fd.effects.is_empty() && !ctx.recursive_fns.contains(&id))
}

fn finite<'a>(
    expr: &Spanned<Expr>,
    ctx: &'a CodegenContext,
    scope: Option<&str>,
) -> Option<&'a FnDef> {
    let fd = induction::callee(expr, ctx, scope)?;
    is_finite(fd, ctx).then_some(fd)
}

/// The single result expression of a helper whose body is one expression.
fn sole_expression(fd: &FnDef) -> Option<&Spanned<Expr>> {
    match fd.body.stmts() {
        [Stmt::Expr(expr)] => Some(expr),
        _ => None,
    }
}

fn claims_true(law: &VerifyLaw) -> bool {
    matches!(law.rhs.node, Expr::Literal(Literal::Bool(true)))
}

/// Functions this one calls directly, in call order and without repeats. One
/// level only: the rung reveals a wrapper, never a cone.
fn direct_callees<'a>(fd: &FnDef, ctx: &'a CodegenContext, scope: Option<&str>) -> Vec<&'a FnDef> {
    let mut found: Vec<&FnDef> = Vec::new();
    for stmt in fd.body.stmts() {
        let (Stmt::Binding(_, _, expr) | Stmt::Expr(expr)) = stmt;
        crate::codegen::expr_walk::walk(expr, &mut |expr| {
            if matches!(expr.node, Expr::FnCall(..))
                && let Some(callee) = induction::callee(expr, ctx, scope)
                && !found.iter().any(|seen| seen.name == callee.name)
            {
                found.push(callee);
            }
        });
    }
    found
}

/// A cursor law's three parts: the wrapper the law is stated about, the
/// validity predicate it reads and the observation that predicate observes.
fn cursor_shape<'a>(
    law: &VerifyLaw,
    ctx: &'a CodegenContext,
    scope: Option<&str>,
) -> Option<(&'a FnDef, &'a FnDef, &'a FnDef)> {
    if !claims_true(law) {
        return None;
    }
    let wrapper = finite(&law.lhs, ctx, scope)?;
    let body = sole_expression(wrapper)?;
    let valid = finite(body, ctx, scope)?;
    let Expr::FnCall(_, arguments) = &body.node else {
        return None;
    };
    // The predicate reads the tape, the starting cursor and one observation.
    let [_, _, observed] = arguments.as_slice() else {
        return None;
    };
    Some((wrapper, valid, finite(observed, ctx, scope)?))
}

/// The cursor laws this law cites, in the order the harness numbers its facts,
/// each one decomposed like the law itself. A cited law that is not a cursor
/// law leaves a gap, so the numbering stays aligned with `_fact{i}`.
fn cited_cursors<'a>(
    law: &VerifyLaw,
    ctx: &'a CodegenContext,
    scope: Option<&str>,
) -> Vec<Option<(&'a FnDef, &'a FnDef, &'a FnDef)>> {
    let Some(using) = law.using.as_ref() else {
        return Vec::new();
    };
    // `dependencies` sorts the selection before numbering the facts.
    let mut selected = using.clone();
    selected.sort();
    selected
        .iter()
        .map(|cited| {
            let local = super::super::shared::same_file_verify_blocks(ctx)
                .into_iter()
                .find_map(|block| match &block.kind {
                    VerifyKind::Law(cited_law)
                        if *cited == format!("{}.{}", block.fn_name, cited_law.name) =>
                    {
                        cursor_shape(cited_law, ctx, scope)
                    }
                    _ => None,
                });
            local.or_else(|| {
                ctx.modules.iter().find_map(|module| {
                    module.verify_laws.iter().find_map(|block| {
                        let VerifyKind::Law(cited_law) = &block.kind else {
                            return None;
                        };
                        (*cited
                            == format!("{}.{}.{}", module.prefix, block.fn_name, cited_law.name))
                        .then(|| {
                            ctx.with_module_scope(Some(&module.prefix), || {
                                cursor_shape(cited_law, ctx, Some(&module.prefix))
                            })
                        })
                        .flatten()
                    })
                })
            })
        })
        .collect()
}

/// `{observation}Cursor(args) holds`: the observation leaves the tape at the
/// suffix its own cursor reports.
///
/// The predicate compares a consumed counter against its starting value, so
/// the arithmetic that survives `split` is linear. An observation that reads a
/// lifted imported one is closed from that one's cited cursor law, which is
/// split into its three statements first; the lifted observation itself is
/// never unfolded.
fn cursor(law: &VerifyLaw, ctx: &CodegenContext, scope: Option<&str>) -> Option<String> {
    let (wrapper, valid, observer) = cursor_shape(law, ctx, scope)?;
    let cited = cited_cursors(law, ctx, scope);
    // A cited law whose shape this rung cannot read leaves it with nothing to
    // say about that observation.
    if cited.iter().any(Option::is_none) {
        return None;
    }
    let mut prepare = String::new();
    // One arm list per conjunct. A goal about the lower bound is never offered
    // a suffix lemma and a goal about the suffix is never offered arithmetic:
    // an alternative that cannot apply still costs an attempt at unifying
    // against the observation, and two lifts deep that attempt is expensive.
    let (mut low, mut high, mut suffix) = (String::new(), String::new(), String::new());
    let mut lower = vec!["omega".to_string()];
    let mut upper = vec!["omega".to_string()];
    for (index, shape) in cited.iter().enumerate() {
        let (cited_wrapper, cited_valid, _) = shape.as_ref()?;
        prepare.push_str(&format!(
            "(simp only [{}, {}, Bool.and_eq_true, decide_eq_true_eq, beq_iff_eq, ge_iff_le, Int.sub_nonneg, forall_and] at _fact{index}); (obtain ⟨_aver_seg_ge{index}, _aver_seg_le{index}, _aver_seg_rem{index}⟩ := _fact{index}); ",
            induction::lean_name(cited_wrapper, ctx),
            induction::lean_name(cited_valid, ctx),
        ));
        low.push_str(&format!(
            " | (simp only [Int.sub_nonneg]; with_reducible exact _aver_seg_ge{index} ..)"
        ));
        high.push_str(&format!(" | (with_reducible exact _aver_seg_le{index} ..)"));
        suffix.push_str(&format!(" | (exact _aver_seg_rem{index} ..)"));
        lower.push(format!("with_reducible exact _aver_seg_ge{index} .."));
        upper.push(format!("with_reducible exact _aver_seg_le{index} .."));
    }
    // A lifted observation reads a transported tape; its own cursor law is
    // stated over the caller's, so the two lengths have to meet.
    let transports: Vec<String> = direct_callees(observer, ctx, scope)
        .into_iter()
        .filter(|callee| induction::is_unary_list_map(callee, ctx))
        .map(|callee| induction::lean_name(callee, ctx))
        .collect();
    for index in 0..transports.len() {
        for cited in 0..cited.len() {
            let arm = format!(
                "(rw [← _aver_transport_length_{index}]; with_reducible exact _aver_seg_le{cited} ..)"
            );
            high.push_str(&format!(" | {arm}"));
            upper.push(arm);
        }
    }
    lower.rotate_left(1);
    upper.rotate_left(1);
    // A step is applied, not supplied as a finished term: a hypothesis handed
    // to `exact` as a tactic block is elaborated after the application has been
    // accepted, so its failure is reported instead of backtracking out of this
    // alternative. `apply` leaves the hypotheses as goals, and an alternative
    // that cannot discharge them simply fails.
    let step = |name: &str, closers: &[&str]| {
        format!(
            "(with_reducible apply {name} <;> (first | {}))",
            closers.join(" | ")
        )
    };
    let ge: Vec<&str> = lower.iter().map(String::as_str).collect();
    let le: Vec<&str> = upper.iter().map(String::as_str).collect();
    let mut with_assumption = vec!["assumption"];
    with_assumption.extend(ge.iter().copied());
    let mut both = le.clone();
    both.extend(ge.iter().copied());
    low.push_str(&format!(" | {}", step("_aver_seg_lower", &ge)));
    high.push_str(&format!(
        " | (with_reducible exact _aver_seg_upper_cons ..) | {}",
        step("_aver_seg_upper_step", &with_assumption)
    ));
    suffix.push_str(&format!(
        " | (exact _aver_seg_drop_zero ..) | (exact _aver_seg_drop_cons ..) | {}",
        step("_aver_seg_drop_step", &with_assumption)
    ));
    // Two observations in a row: the second starts where the first stopped, so
    // its own bounds are stated over the suffix the first left. Composing them
    // is one step, and the tape the second reads is the first's `drop`.
    if !cited.is_empty() {
        low.push_str(&format!(
            " | (simp only [Int.sub_nonneg]; {})",
            step("_aver_seg_link", &ge)
        ));
        high.push_str(&format!(" | {}", step("_aver_seg_chain", &both)));
        suffix.push_str(&format!(" | {}", step("_aver_seg_drop_chain", &ge)));
    }
    // Each step is parenthesized: `repeat'` and `all_goals` otherwise absorb
    // the rest of the sequence, and `repeat'` swallowing a later failure would
    // leave the goal open instead of backtracking out of this alternative.
    //
    // A bound is never offered `rfl`. Reflexivity of `≤` is a rewrite rule like
    // any other, so on a bound it tries to unify the two sides of the
    // comparison, and two lifts deep that unification walks the whole
    // observation before it fails.
    let branch = |arms: &str| {
        format!(
            "(focus ((try (refine decide_eq_true ?_)); (repeat' split); (all_goals (try dsimp only)); (all_goals (first{arms}))))"
        )
    };
    Some(format!(
        "({STEPS}{transport}{prepare}(simp only [{wrapper}, {valid}, {observer}, Bool.and_eq_true, decide_eq_true_eq, beq_iff_eq, ge_iff_le]); (refine ⟨?_, ?_, ?_⟩); {}; {}; {}; done)",
        branch(&format!("{low} | omega")),
        branch(&format!("{high} | omega")),
        branch(&format!(" | rfl{suffix}")),
        transport = induction::checked_map_lemmas(&transports, false),
        wrapper = induction::lean_name(wrapper, ctx),
        valid = induction::lean_name(valid, ctx),
        observer = induction::lean_name(observer, ctx),
    ))
}

/// `{observation}(…, events, …) == {observation}Prefixed(events, {observation}(…, [], …))`,
/// the statement that an incoming event history is only a prefix. Both sides
/// mention the same observation, so the branch structure matches after `split`,
/// and a lifted one may be revealed: it passes an empty history inward and
/// prepends outside, which is the whole content of the claim.
fn events_prefix(law: &VerifyLaw, ctx: &CodegenContext, scope: Option<&str>) -> Option<String> {
    if !claims_true(law) {
        return None;
    }
    let Expr::BinOp(BinOp::Eq, left, right) = &law.lhs.node else {
        return None;
    };
    let observer = finite(left, ctx, scope)?;
    let prefixed = finite(right, ctx, scope)?;
    let Expr::FnCall(_, arguments) = &right.node else {
        return None;
    };
    // The prefixed form carries the history and the same observation read with
    // an empty one.
    let [_, observed] = arguments.as_slice() else {
        return None;
    };
    if finite(observed, ctx, scope)?.name != observer.name {
        return None;
    }
    let mut names = vec![
        induction::lean_name(prefixed, ctx),
        induction::lean_name(observer, ctx),
    ];
    names.extend(
        direct_callees(observer, ctx, scope)
            .into_iter()
            .filter(|callee| is_finite(callee, ctx))
            .map(|callee| induction::lean_name(callee, ctx)),
    );
    Some(format!(
        "((simp only [beq_iff_eq, {}]); (repeat' split); (all_goals (first | rfl | (simp_all [List.append_assoc]))); done)",
        names.join(", ")
    ))
}

/// `{observer}(…) == {observer}Step{X}({observation}(…))`: one step of a
/// protocol observer is that observation followed by the generic continuation.
///
/// The right side is the step wrapper applied to exactly one observation. The
/// left side is the observer being stepped: one equation of it when it
/// recurses, otherwise the entry wrapper itself.
fn step(law: &VerifyLaw, ctx: &CodegenContext, scope: Option<&str>) -> Option<String> {
    if !claims_true(law) {
        return None;
    }
    let Expr::BinOp(BinOp::Eq, left, right) = &law.lhs.node else {
        return None;
    };
    let wrapper = finite(right, ctx, scope)?;
    let Expr::FnCall(_, arguments) = &right.node else {
        return None;
    };
    let [observed] = arguments.as_slice() else {
        return None;
    };
    let observer = finite(observed, ctx, scope)?;
    let stepped = induction::callee(left, ctx, scope)?;
    let opening = match finite(left, ctx, scope) {
        // A finite entry is revealed with everything else it needs.
        Some(entry) => format!(
            "(simp only [beq_iff_eq, {}, {}, {}])",
            induction::lean_name(entry, ctx),
            induction::lean_name(wrapper, ctx),
            induction::lean_name(observer, ctx),
        ),
        // A recursive observer is opened by its own equation, exactly once.
        None => format!(
            "(simp only [beq_iff_eq]); (rw [{}.eq_def]); (simp only [{}, {}])",
            induction::lean_name(stepped, ctx),
            induction::lean_name(wrapper, ctx),
            induction::lean_name(observer, ctx),
        ),
    };
    Some(format!(
        "({opening}; (repeat' split); (all_goals (first | rfl | (simp_all; done))); done)"
    ))
}

pub(super) fn candidate(law: &VerifyLaw, ctx: &CodegenContext) -> Option<String> {
    // The segment interface stands on its own definitions and on cited cursor
    // laws; a law that explains itself in steps is a different obligation.
    if !law.because.is_empty() {
        return None;
    }
    let scope = ctx.active_module_scope();
    cursor(law, ctx, scope.as_deref())
        .or_else(|| events_prefix(law, ctx, scope.as_deref()))
        .or_else(|| step(law, ctx, scope.as_deref()))
}

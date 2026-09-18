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

/// Suffix steps a boundary proof needs: composing two drops, and moving a
/// cursor past the one token this step consumed. Proved inline, so the rung
/// adds no axiom and no prelude entry.
const TRANSPORT_STEPS: &str = "have _aver_seg_drop_add : ∀ {α : Type} (l : List α) (m n : Nat), (l.drop m).drop n = l.drop (m + n) := (by intro α l m n; induction m generalizing l with | zero => simp | succ m ih => cases l with | nil => simp | cons x xs => rw [List.drop_succ_cons, ih, show m + 1 + n = m + n + 1 by omega, List.drop_succ_cons]); \
have _aver_seg_drop_shift : ∀ {α : Type} (x : α) (rest : List α) (c c1 : Int), c + 1 ≤ c1 → rest.drop (c1 - (c + 1)).toNat = (x :: rest).drop (c1 - c).toNat := (by intro α x rest c c1 h; rw [show (c1 - c).toNat = (c1 - (c + 1)).toNat + 1 by omega, List.drop_succ_cons]); ";

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

/// Read one cited law in the scope that owns it. The same decomposition runs on
/// a law of this file and on a law of an imported module; only the scope names
/// differ.
fn read_cited<T>(
    law: &VerifyLaw,
    ctx: &CodegenContext,
    scope: Option<&str>,
    read: impl Fn(&VerifyLaw, Option<&str>) -> Option<T>,
) -> Vec<Option<T>> {
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
                        read(cited_law, scope)
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
                                read(cited_law, Some(&module.prefix))
                            })
                        })
                        .flatten()
                    })
                })
            })
        })
        .collect()
}

/// The cursor laws this law cites, in the order the harness numbers its facts,
/// each one decomposed like the law itself. A cited law that is not a cursor
/// law leaves a gap, so the numbering stays aligned with `_fact{i}`.
fn cited_cursors<'a>(
    law: &VerifyLaw,
    ctx: &'a CodegenContext,
    scope: Option<&str>,
) -> Vec<Option<(&'a FnDef, &'a FnDef, &'a FnDef)>> {
    read_cited(law, ctx, scope, |cited, scope| {
        cursor_shape(cited, ctx, scope)
    })
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
/// left side is the recursive observer being stepped, opened by its own
/// equation exactly once.
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
    if finite(left, ctx, scope).is_some() {
        return None;
    }
    Some(format!(
        "((simp only [beq_iff_eq]); (rw [{}.eq_def]); (simp only [{}, {}]); (repeat' split); (all_goals (first | rfl | (simp_all; done))); done)",
        induction::lean_name(stepped, ctx),
        induction::lean_name(wrapper, ctx),
        induction::lean_name(observer, ctx),
    ))
}

/// What one cited law says, read from its statement alone.
enum Cited<'a> {
    /// One observation leaves the tape at the suffix its own cursor reports:
    /// the wrapper the claim is about and the predicate it reads.
    Cursor(&'a FnDef, &'a FnDef),
    /// A whole protocol observer does, for every outcome and tape: the wrapper,
    /// the predicate and the observer itself.
    Bounded(&'a FnDef, &'a FnDef, &'a FnDef),
    /// An observation reads its incoming event history only as a prefix: the
    /// observation and its prefixed form.
    Prefix(&'a FnDef, &'a FnDef),
    /// One step of a protocol observer is an observation plus the generic
    /// continuation: the step wrapper and the observation it carries.
    Step(&'a FnDef, &'a FnDef),
    /// A list map distributes over an append, or answers a single cell.
    Append,
    Singleton,
}

/// Decompose one cited law. Everything is read from the statement's shape: a
/// claim about a validity predicate, an observation compared with its prefixed
/// form, a step wrapper applied to one observation, or a list map over an
/// append or a single cell.
fn classify<'a>(
    law: &VerifyLaw,
    ctx: &'a CodegenContext,
    scope: Option<&str>,
) -> Option<Cited<'a>> {
    if !claims_true(law) {
        return None;
    }
    if let Some(wrapper) = finite(&law.lhs, ctx, scope)
        && let Some(body) = sole_expression(wrapper)
        && let Some(valid) = finite(body, ctx, scope)
        && let Expr::FnCall(_, arguments) = &body.node
        && let [_, _, observed] = arguments.as_slice()
    {
        let called = induction::callee(observed, ctx, scope)?;
        return Some(match finite(observed, ctx, scope) {
            Some(_) => Cited::Cursor(wrapper, valid),
            None => Cited::Bounded(wrapper, valid, called),
        });
    }
    let Expr::BinOp(BinOp::Eq, left, right) = &law.lhs.node else {
        return None;
    };
    let observer = induction::callee(left, ctx, scope)?;
    if induction::is_unary_list_map(observer, ctx) {
        return Some(match right.node {
            Expr::List(_) => Cited::Singleton,
            _ => Cited::Append,
        });
    }
    let wrapper = finite(right, ctx, scope)?;
    let Expr::FnCall(_, arguments) = &right.node else {
        return None;
    };
    match arguments.as_slice() {
        // The prefixed form carries the history and the same observation.
        [_, observed] if finite(observed, ctx, scope)?.name == observer.name => {
            Some(Cited::Prefix(observer, wrapper))
        }
        // The step wrapper carries the observation this step produces.
        [observed] => Some(Cited::Step(wrapper, finite(observed, ctx, scope)?)),
        _ => None,
    }
}

/// Finite helpers reachable from one function, itself included.
fn revealed<'a>(fd: &'a FnDef, ctx: &'a CodegenContext, scope: Option<&str>) -> Vec<&'a FnDef> {
    let mut found = vec![fd];
    let mut index = 0;
    while index < found.len() {
        for callee in direct_callees(found[index], ctx, scope) {
            if is_finite(callee, ctx) && !found.iter().any(|seen| seen.name == callee.name) {
                found.push(callee);
            }
        }
        index += 1;
    }
    found
}

/// `{adapter}Direct(args) == {adapter}Mapped(args)`: driving an imported
/// outcome through the caller's own tape is the caller's lift of driving it in
/// the owning module.
///
/// The two sides walk the same tape, so the proof is one induction on it. Each
/// step rewrites the owning module's protocol step and the prefix of its event
/// history — both cited — and then says nothing more about the observation than
/// its cited cursor. The imported observer is never unfolded.
fn transport(law: &VerifyLaw, ctx: &CodegenContext, scope: Option<&str>) -> Option<String> {
    if !claims_true(law) {
        return None;
    }
    let Expr::BinOp(BinOp::Eq, left, right) = &law.lhs.node else {
        return None;
    };
    // The direct side drives the caller's own tape; the mapped side lifts the
    // owning module's drive of the transported tape.
    let direct = finite(left, ctx, scope)?;
    let mapped = finite(right, ctx, scope)?;
    let import_drive = induction::callee(sole_expression(direct)?, ctx, scope)?;
    let lift = induction::callee(sole_expression(mapped)?, ctx, scope)?;
    let Expr::FnCall(_, arguments) = &sole_expression(mapped)?.node else {
        return None;
    };
    let child_drive = induction::callee(arguments.first()?, ctx, scope)?;
    if induction::list_measure(import_drive, ctx).is_none()
        || induction::list_measure(child_drive, ctx).is_none()
    {
        return None;
    }
    let map = |fd: &FnDef| -> Option<&FnDef> {
        direct_callees(fd, ctx, scope)
            .into_iter()
            .find(|callee| induction::is_unary_list_map(callee, ctx))
    };
    let events = map(direct)?;
    let inputs = map(mapped)?;
    let event = direct_callees(events, ctx, scope).into_iter().next()?;
    let Expr::Match { arms: cells, .. } = &sole_expression(event)?.node else {
        return None;
    };
    // Every cited law has to be one this rung can read, or it has nothing to
    // say about the observation that law is about.
    let cited: Vec<Cited> = read_cited(law, ctx, scope, |cited, scope| classify(cited, ctx, scope))
        .into_iter()
        .collect::<Option<_>>()?;
    let mut prepare = String::new();
    let (mut equations, mut cleared, mut reveal, mut suffix, mut lower) =
        (Vec::new(), Vec::new(), Vec::new(), Vec::new(), Vec::new());
    let (mut bounded, mut steps, mut prefixes) = (None, Vec::new(), Vec::new());
    for (index, shape) in cited.iter().enumerate() {
        match shape {
            Cited::Cursor(wrapper, valid) => {
                prepare.push_str(&format!(
                    "(simp only [{}, {}, Bool.and_eq_true, decide_eq_true_eq, beq_iff_eq, ge_iff_le, Int.sub_nonneg, forall_and] at _fact{index}); (obtain ⟨_aver_seg_ge{index}, _aver_seg_le{index}, _aver_seg_rem{index}⟩ := _fact{index}); ",
                    induction::lean_name(wrapper, ctx),
                    induction::lean_name(valid, ctx),
                ));
                cleared.push(format!("_aver_seg_le{index} _aver_seg_rem{index}"));
                suffix.push(format!("_aver_seg_rem{index}"));
                lower.push(format!("exact _aver_seg_ge{index} .."));
            }
            Cited::Bounded(wrapper, valid, observed) => {
                bounded = Some((
                    index,
                    induction::lean_name(wrapper, ctx),
                    induction::lean_name(valid, ctx),
                    induction::lean_name(observed, ctx),
                ));
                cleared.push(format!("_fact{index}"));
            }
            Cited::Prefix(observer, prefixed) => {
                prefixes.push((index, observer.name.clone()));
                equations.push(index);
                reveal.push(induction::lean_name(prefixed, ctx));
            }
            Cited::Step(template, observer) => {
                steps.push((index, observer.name.clone()));
                equations.push(index);
                reveal.push(induction::lean_name(template, ctx));
            }
            Cited::Append | Cited::Singleton => equations.push(index),
        }
    }
    let (bounded_index, cursor, valid, observed) = bounded?;
    if steps.is_empty() || suffix.is_empty() {
        return None;
    }
    // One alternative per answerable kind: the step this branch takes, then the
    // prefix of the observation it produced.
    let arms = steps
        .iter()
        .map(|(step, observer)| {
            let prefix = prefixes
                .iter()
                .find_map(|(index, name)| (name == observer).then_some(*index))?;
            Some(format!("rw [_fact{step}, _fact{prefix}]"))
        })
        .collect::<Option<Vec<_>>>()?
        .join(" | ");
    for index in &equations {
        cleared.push(format!("_fact{index}"));
    }
    let names = |items: Vec<&FnDef>| {
        items
            .into_iter()
            .map(|fd| induction::lean_name(fd, ctx))
            .collect::<Vec<_>>()
            .join(", ")
    };
    let lift_names = names(revealed(lift, ctx, scope));
    let tape_names = names(vec![inputs, direct_callees(inputs, ctx, scope).first()?]);
    let adapters = names(
        direct_callees(import_drive, ctx, scope)
            .into_iter()
            .filter(|callee| is_finite(callee, ctx))
            .collect(),
    );
    let lower = format!("first | omega | {}", lower.join(" | "));
    let transported = induction::checked_map_lemmas(&[induction::lean_name(inputs, ctx)], true);
    // A step is applied with the tape one token shorter, so the recursion's own
    // measure is the hypothesis this rung has to discharge each time.
    let step_ih = |bound: &str| {
        format!(
            "exact Eq.trans (ih _ (by simp only [List.length_drop, List.length_cons]; omega) _ _ _ _ _) (_aver_seg_lift_shift _ _ _ _ _ _ (by {bound}) (_aver_seg_drive_ge _ _ _ _ _))"
        )
    };
    Some(format!(
        "({prepare}(simp only [beq_iff_eq] at {equations}); \
         {TRANSPORT_STEPS}{transported}\
         (have _aver_seg_lift_shift : ∀ (D : _) (x : _) (rest : _) (evs : _) (cc c1 : Int), cc + 1 ≤ c1 → c1 ≤ D.consumed → {lift} D (rest.drop (Int.toNat (c1 - (cc + 1)))) evs c1 = {lift} D (x :: rest) evs cc := (by intro D x rest evs cc c1 h1 hD; simp only [{lift}, _aver_seg_drop_add]; have hk : Int.toNat (D.consumed - cc) = (Int.toNat (c1 - (cc + 1)) + Int.toNat (D.consumed - c1)) + 1 := (by omega); rw [hk, List.drop_succ_cons])); \
         (have _aver_seg_drive_ge : ∀ (o : _) (i : _) (p : Int) (e : _) (cc : Int), cc ≤ ({observed} o i p e cc).consumed := (by intro o i p e cc; have h := _fact{bounded_index} o i p e cc; simp only [{cursor}, {valid}, Bool.and_eq_true, decide_eq_true_eq, beq_iff_eq, ge_iff_le] at h; omega)); \
         (simp only [beq_iff_eq, {direct}, {mapped}]); \
         (induction inputs using (measure List.length).wf.induction generalizing outcome position events childEvents consumed with | h inputs ih => \
         (dsimp only [WellFoundedRelation.rel, measure, invImage, InvImage, Nat.lt_wfRel] at ih); \
         (rw [{import_drive}.eq_def]); (repeat' split); (all_goals (try dsimp only)); \
         (all_goals (first \
         | (rw [{child_drive}.eq_def]; simp [{lift_names}]; done) \
         | (simp only [{tape_names}]; rw [{child_drive}.eq_def]; simp [{lift_names}]; done) \
         | (simp only [{tape_names}]; (first | {arms}); \
         (simp only [{adapters}, {reveal}, {suffix}, _aver_transport_drop_0]); (repeat' split); \
         (all_goals (simp only [List.append_assoc])); \
         (all_goals (simp only [← List.append_assoc, {reversed}, ← _fact{singleton}, ← _fact{append}])); \
         (all_goals (clear {cleared} _aver_transport_length_0 _aver_transport_drop_0 _aver_seg_drop_add)); \
         (all_goals (first \
         | rfl \
         | ({step_one}) \
         | (simp only [{lift}, {event}]; first | rfl | (rw [_aver_seg_drop_shift _ _ _ _ (by {lower})]; done)) \
         | ({step_two}) \
         | (simp_all only [Option.some.injEq, reduceCtorEq] <;> ({step_one})))))))); done)",
        equations = equations
            .iter()
            .map(|index| format!("_fact{index}"))
            .collect::<Vec<_>>()
            .join(" "),
        lift = induction::lean_name(lift, ctx),
        direct = induction::lean_name(direct, ctx),
        mapped = induction::lean_name(mapped, ctx),
        import_drive = induction::lean_name(import_drive, ctx),
        child_drive = induction::lean_name(child_drive, ctx),
        event = induction::lean_name(event, ctx),
        reveal = reveal.join(", "),
        suffix = suffix.join(", "),
        cleared = cleared.join(" "),
        reversed = (1..=cells.len())
            .map(|arm| format!("← {}.eq_{arm}", induction::lean_name(event, ctx)))
            .collect::<Vec<_>>()
            .join(", "),
        singleton = cited
            .iter()
            .position(|shape| matches!(shape, Cited::Singleton))?,
        append = cited
            .iter()
            .position(|shape| matches!(shape, Cited::Append))?,
        step_one = step_ih(&lower),
        step_two = step_ih(&lower.replace("first | omega | ", "first | ")),
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
        .or_else(|| transport(law, ctx, scope.as_deref()))
}

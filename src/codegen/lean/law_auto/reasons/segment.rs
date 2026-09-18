//! Interface lemmas for a module-owned segment observation, and the cursor
//! contract of a protocol observer that reads them.
//!
//! Three finite shapes: a cursor predicate applied to one observation, an
//! observation compared with its own prefixed form, and one step of a protocol
//! observer written as an observation followed by the generic continuation.
//! One recursive shape: the cursor predicate applied to the whole observer,
//! closed by functional induction from the cited cursor laws of the
//! observations it drives through. Each is recognised from the shape of the
//! law's statement alone — never from what an observation computes — and each
//! unfolds only the law's own wrappers, the observation it is stated about
//! and, for a step, one equation of the observer being stepped. A lifted
//! imported observation is never opened: the cited cursor law is the only
//! thing said about it. Of the owner's, a rung here opens only the wrappers
//! its citations are stated with, and the mapping rung applies the owner's
//! protocol observer's own equation once per token where that owner observes
//! no segment; observations, observers and source traces of another module
//! stay closed (`induction::imported_machinery`).
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
pub(super) fn is_finite(fd: &FnDef, ctx: &CodegenContext) -> bool {
    common::fn_id_for_decl(ctx, fd)
        .is_some_and(|id| fd.effects.is_empty() && !ctx.recursive_fns.contains(&id))
}

pub(super) fn finite<'a>(
    expr: &Spanned<Expr>,
    ctx: &'a CodegenContext,
    scope: Option<&str>,
) -> Option<&'a FnDef> {
    let fd = induction::callee(expr, ctx, scope)?;
    is_finite(fd, ctx).then_some(fd)
}

/// The single result expression of a helper whose body is one expression.
pub(super) fn sole_expression(fd: &FnDef) -> Option<&Spanned<Expr>> {
    match fd.body.stmts() {
        [Stmt::Expr(expr)] => Some(expr),
        _ => None,
    }
}

pub(super) fn claims_true(law: &VerifyLaw) -> bool {
    matches!(law.rhs.node, Expr::Literal(Literal::Bool(true)))
}

/// Functions this one calls directly, in call order and without repeats. One
/// level only: the rung reveals a wrapper, never a cone.
pub(super) fn direct_callees<'a>(
    fd: &FnDef,
    ctx: &'a CodegenContext,
    scope: Option<&str>,
) -> Vec<&'a FnDef> {
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
pub(super) fn read_cited<T>(
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
        .filter(|callee| {
            induction::is_unary_list_map(callee, ctx)
                && !induction::imported_machinery(callee, ctx, scope)
        })
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
            .filter(|callee| {
                is_finite(callee, ctx) && !induction::imported_machinery(callee, ctx, scope)
            })
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
/// left side is the observer being stepped — the recursive protocol observer,
/// or the finite protocol entry whose step is the start segment's — opened by
/// its own equation exactly once.
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
    // A finite left side is a protocol entry, which builds trace records; a
    // scalar wrapper of a scalar is not a step of anything.
    if is_finite(stepped, ctx) && !induction::constructs_result_record(stepped) {
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
pub(super) enum Cited<'a> {
    /// One observation leaves the tape at the suffix its own cursor reports:
    /// the wrapper the claim is about, the predicate it reads and the
    /// observation.
    Cursor(&'a FnDef, &'a FnDef, &'a FnDef),
    /// Driving an imported outcome through the caller's tape is the caller's
    /// lift of driving it in the owning module: the direct side and the
    /// mapped side.
    Mapping(&'a FnDef, &'a FnDef),
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
pub(super) fn classify<'a>(
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
            Some(observation) => Cited::Cursor(wrapper, valid, observation),
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
    // The direct side wraps the caller's own drive; the mapped side is the
    // lift of the owner's drive, reached through the lift's first argument.
    if let Some(direct) = finite(left, ctx, scope)
        && sole_expression(direct)
            .and_then(|body| induction::callee(body, ctx, scope))
            .is_some_and(|drive| !is_finite(drive, ctx))
        && let Some(lifted) = sole_expression(wrapper)
        && finite(lifted, ctx, scope).is_some()
        && let Expr::FnCall(_, inner) = &lifted.node
        && inner
            .first()
            .and_then(|arg| induction::callee(arg, ctx, scope))
            .is_some_and(|drive| !is_finite(drive, ctx))
    {
        return Some(Cited::Mapping(direct, wrapper));
    }
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

/// Finite helpers reachable from one function, itself included; another
/// module's machinery is not followed.
fn revealed<'a>(fd: &'a FnDef, ctx: &'a CodegenContext, scope: Option<&str>) -> Vec<&'a FnDef> {
    let mut found = vec![fd];
    let mut index = 0;
    while index < found.len() {
        for callee in direct_callees(found[index], ctx, scope) {
            if is_finite(callee, ctx)
                && !induction::imported_machinery(callee, ctx, scope)
                && !found.iter().any(|seen| seen.name == callee.name)
            {
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
/// its cited cursor. An owner without observed segments publishes neither: its
/// protocol observer is the generic continuation over pure routers, and the
/// rung steps it by its own equation instead. That equation, applied to the
/// owner's protocol observer alone and once per token, is the only equation of
/// another module any request-trace rung applies; the imported observer is
/// never unfolded.
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
            Cited::Cursor(wrapper, valid, _) => {
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
            // A mapping is what this rung proves, never what it reads.
            Cited::Mapping(..) => {}
        }
    }
    let (bounded_index, cursor, valid, observed) = bounded?;
    // No cited step and no cited cursor: the owner observes no segment, and a
    // token is stepped by the owner's own equation.
    let template = steps.is_empty() && suffix.is_empty();
    if !template && (steps.is_empty() || suffix.is_empty()) {
        return None;
    }
    // One alternative per answerable kind: the step this branch takes, then the
    // prefix of the observation it produced.
    let arms = if template {
        format!("rw [{}.eq_def]", induction::lean_name(child_drive, ctx))
    } else {
        steps
            .iter()
            .map(|(step, observer)| {
                let prefix = prefixes
                    .iter()
                    .find_map(|(index, name)| (name == observer).then_some(*index))?;
                Some(format!("rw [_fact{step}, _fact{prefix}]"))
            })
            .collect::<Option<Vec<_>>>()?
            .join(" | ")
    };
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
            .filter(|callee| {
                is_finite(callee, ctx) && !induction::imported_machinery(callee, ctx, scope)
            })
            .collect(),
    );
    let transported = induction::checked_map_lemmas(&[induction::lean_name(inputs, ctx)], true);
    // A step is applied with the tape one token shorter, so the recursion's own
    // measure is the hypothesis this rung has to discharge each time. After an
    // observation the caller continues on a computed suffix, and the cursor
    // bound is arithmetic or a cited lower bound; after a token the owner's
    // template stepped, on the cons tail itself.
    let step_ih = |bound: &str| {
        format!(
            "(exact Eq.trans (ih _ (by simp only [List.length_drop, List.length_cons]; omega) _ _ _ _ _) (_aver_seg_lift_shift _ _ _ _ _ _ (by {bound}) (_aver_seg_drive_ge _ _ _ _ _)))"
        )
    };
    let cited_lower = lower.join(" | ");
    let bound = if lower.is_empty() {
        "omega".to_string()
    } else {
        format!("first | omega | {cited_lower}")
    };
    let mut closers = vec![
        "rfl".to_string(),
        step_ih(&bound),
        "(exact Eq.trans (ih _ (by simp only [List.length_cons]; omega) _ _ _ _ _) (_aver_seg_lift_cons _ _ _ _ _ (_aver_seg_drive_ge _ _ _ _ _)))".to_string(),
        format!(
            "(simp only [{lift}, {event}]; first | rfl | (rw [_aver_seg_drop_shift _ _ _ _ (by {bound})]; done))",
            lift = induction::lean_name(lift, ctx),
            event = induction::lean_name(event, ctx),
        ),
    ];
    if !lower.is_empty() {
        closers.push(step_ih(&format!("first | {cited_lower}")));
    }
    closers.push(format!(
        "(simp_all only [Option.some.injEq, reduceCtorEq] <;> {})",
        step_ih(&bound)
    ));
    let opened = [adapters, reveal.join(", "), suffix.join(", ")]
        .into_iter()
        .filter(|part| !part.is_empty())
        .chain(["_aver_transport_drop_0".to_string()])
        .collect::<Vec<_>>()
        .join(", ");
    Some(format!(
        "({prepare}(simp only [beq_iff_eq] at {equations}); \
         {TRANSPORT_STEPS}{transported}\
         (have _aver_seg_lift_shift : ∀ (D : _) (x : _) (rest : _) (evs : _) (cc c1 : Int), cc + 1 ≤ c1 → c1 ≤ D.consumed → {lift} D (rest.drop (Int.toNat (c1 - (cc + 1)))) evs c1 = {lift} D (x :: rest) evs cc := (by intro D x rest evs cc c1 h1 hD; simp only [{lift}, _aver_seg_drop_add]; have hk : Int.toNat (D.consumed - cc) = (Int.toNat (c1 - (cc + 1)) + Int.toNat (D.consumed - c1)) + 1 := (by omega); rw [hk, List.drop_succ_cons])); \
         (have _aver_seg_lift_cons : ∀ (D : _) (x : _) (rest : _) (evs : _) (cc : Int), cc + 1 ≤ D.consumed → {lift} D rest evs (cc + 1) = {lift} D (x :: rest) evs cc := (by intro D x rest evs cc h; simp only [{lift}]; rw [_aver_seg_drop_shift x rest cc D.consumed h])); \
         (have _aver_seg_drive_ge : ∀ (o : _) (i : _) (p : Int) (e : _) (cc : Int), cc ≤ ({observed} o i p e cc).consumed := (by intro o i p e cc; have h := _fact{bounded_index} o i p e cc; simp only [{cursor}, {valid}, Bool.and_eq_true, decide_eq_true_eq, beq_iff_eq, ge_iff_le] at h; omega)); \
         (simp only [beq_iff_eq, {direct}, {mapped}]); \
         (induction inputs using (measure List.length).wf.induction generalizing outcome position events childEvents consumed with | h inputs ih => \
         (dsimp only [WellFoundedRelation.rel, measure, invImage, InvImage, Nat.lt_wfRel] at ih); \
         (rw [{import_drive}.eq_def]); (repeat' split); (all_goals (try dsimp only)); \
         (all_goals (first \
         | (rw [{child_drive}.eq_def]; simp [{lift_names}]; done) \
         | (simp only [{tape_names}]; rw [{child_drive}.eq_def]; simp [{lift_names}]; done) \
         | (simp only [{tape_names}]; (first | {arms}); \
         (simp only [{opened}]); (repeat' split); \
         (all_goals (try simp only [List.append_assoc])); \
         (all_goals (try simp only [← List.append_assoc, {reversed}, ← _fact{singleton}, ← _fact{append}])); \
         (all_goals (clear {cleared} _aver_transport_length_0 _aver_transport_drop_0 _aver_seg_drop_add)); \
         (all_goals (first | {closers})))))); done)",
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
        closers = closers.join(" | "),
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
    ))
}

/// `{cursor}(outcome, tape, …) holds` for a whole protocol observer: every
/// outcome, every tape.
///
/// Functional induction on the observer gives one case per arm, each with the
/// observations that arm makes as named lets and the recursive call's bound
/// as its hypothesis. At each observation the rung names it and reads the
/// cited cursor law about it; then it names the recursive result. The
/// induction hypothesis is rewritten through the same names, so the cursor
/// arithmetic that remains is linear in the named consumed counters and the
/// suffix equation is a chain of `drop`s. Observations whose arguments carry
/// another observation are named after it. A token the observer reads from
/// the suffix an observation left (`drop k tape = token :: rest`) is one more
/// step of the same chain: its bound and its suffix are registered from that
/// equation and the tail variable is substituted, one token at a time until
/// none is left. Nothing an observation computes is unfolded.
fn bounded(law: &VerifyLaw, ctx: &CodegenContext, scope: Option<&str>) -> Option<String> {
    if !claims_true(law) {
        return None;
    }
    let wrapper = finite(&law.lhs, ctx, scope)?;
    let body = sole_expression(wrapper)?;
    let valid = finite(body, ctx, scope)?;
    let Expr::FnCall(_, arguments) = &body.node else {
        return None;
    };
    let [_, _, observed] = arguments.as_slice() else {
        return None;
    };
    let observer = induction::callee(observed, ctx, scope)?;
    if is_finite(observer, ctx) {
        return None;
    }
    // The observer is applied to exactly the wrapper's parameters, in order:
    // those are the variables the induction runs over.
    let Expr::FnCall(_, args) = &observed.node else {
        return None;
    };
    let names = args
        .iter()
        .map(|arg| match &arg.node {
            Expr::Ident(name) | Expr::Resolved { name, .. } => Some(name.as_str()),
            _ => None,
        })
        .collect::<Option<Vec<_>>>()?;
    if names.len() != wrapper.params.len()
        || names
            .iter()
            .zip(&wrapper.params)
            .any(|(name, (param, _))| name != param)
    {
        return None;
    }
    let cited = cited_cursors(law, ctx, scope);
    if cited.is_empty() || cited.iter().any(Option::is_none) {
        return None;
    }
    let observations: Vec<(usize, &FnDef, &FnDef, &FnDef)> = cited
        .iter()
        .enumerate()
        .filter_map(|(index, shape)| shape.as_ref().map(|(w, v, o)| (index, *w, *v, *o)))
        .collect();
    let owner = common::fn_owning_scope_for(ctx, observer);
    let cited_names: Vec<String> = observations
        .iter()
        .map(|(_, _, _, o)| o.name.clone())
        .collect();
    // An observation whose arguments in the observer's body carry another
    // cited observation reads that one's result, so it is named after it.
    let carried = |name: &str| -> usize {
        let mut inner: Vec<String> = Vec::new();
        for stmt in observer.body.stmts() {
            let (Stmt::Binding(_, _, expr) | Stmt::Expr(expr)) = stmt;
            crate::codegen::expr_walk::walk(expr, &mut |expr| {
                if let Expr::FnCall(_, call_args) = &expr.node
                    && induction::callee(expr, ctx, owner).is_some_and(|fd| fd.name == name)
                {
                    for arg in call_args {
                        crate::codegen::expr_walk::walk(arg, &mut |sub| {
                            if let Some(fd) = induction::callee(sub, ctx, owner)
                                && fd.name != name
                                && cited_names.contains(&fd.name)
                                && !inner.contains(&fd.name)
                            {
                                inner.push(fd.name.clone());
                            }
                        });
                    }
                }
            });
        }
        inner.len()
    };
    let ranks: Vec<usize> = observations
        .iter()
        .map(|(_, _, _, o)| carried(&o.name))
        .collect();
    let mut order: Vec<usize> = (0..observations.len()).collect();
    order.sort_by_key(|&i| ranks[i]);
    let observations: Vec<_> = order.into_iter().map(|i| observations[i]).collect();
    let holes = |fd: &FnDef| " _".repeat(fd.params.len());
    let mut prepare = String::new();
    let mut name_observations = String::new();
    for (k, (index, cited_wrapper, cited_valid, cited_observer)) in observations.iter().enumerate()
    {
        let k = k + 1;
        prepare.push_str(&format!(
            "(simp only [{}, {}, Bool.and_eq_true, beq_iff_eq, decide_eq_true_eq, ge_iff_le] at _fact{index}); ",
            induction::lean_name(cited_wrapper, ctx),
            induction::lean_name(cited_valid, ctx),
        ));
        name_observations.push_str(&format!(
            "all_goals (try ((conv => pattern {obs}{holes}); (generalize _aver_cur_h{k} : {obs}{holes} = _aver_cur_s{k}); (have _aver_cur_f{k} := _aver_cur_h{k} ▸ _fact{index}{holes}); (obtain ⟨_aver_cur_a{k}, _aver_cur_b{k}, _aver_cur_c{k}⟩ : _ ∧ _ ∧ _ := _aver_cur_f{k}); (try simp only [_aver_cur_h{k}] at _aver_cur_i1); (try simp only [_aver_cur_h{k}] at _aver_cur_i2); (try simp only [_aver_cur_h{k}] at _aver_cur_i3))); ",
            obs = induction::lean_name(cited_observer, ctx),
            holes = holes(cited_observer),
        ));
    }
    let drive = induction::lean_name(observer, ctx);
    // A token read after an observation is found by its shape; the
    // observation in its index is renamed like everywhere else, one name at a
    // time, since a case mentions only the observations it makes.
    let renames = (1..=observations.len())
        .map(|k| format!("(try simp only [_aver_cur_h{k}] at _aver_cur_e); "))
        .collect::<String>();
    Some(format!(
        "({STEPS}{TRANSPORT_STEPS}{prepare}(simp only [{wrapper}, {valid}, Bool.and_eq_true, beq_iff_eq, decide_eq_true_eq, ge_iff_le]); \
(fun_induction {drive} {names}); \
all_goals (try ((rename_i _aver_cur_ih); (obtain ⟨_aver_cur_i1, _aver_cur_i2, _aver_cur_i3⟩ : _ ∧ _ ∧ _ := _aver_cur_ih))); \
all_goals (try simp +zetaDelta only [] at *); \
{name_observations}\
all_goals (try ((conv => pattern {drive}{drive_holes}); (generalize _aver_cur_hr : {drive}{drive_holes} = _aver_cur_r); (try simp only [_aver_cur_hr] at _aver_cur_i1); (try simp only [_aver_cur_hr] at _aver_cur_i2); (try simp only [_aver_cur_hr] at _aver_cur_i3))); \
all_goals (repeat ((repeat clear ‹List.drop _ _ = _ :: List.drop _ _›); (have _aver_cur_e := ‹List.drop _ _ = _ :: _›); {renames}(have _aver_cur_u := _aver_seg_upper_step _ _ _ _ _ (by omega) _aver_cur_e); (have _aver_cur_t := _aver_seg_drop_step _ _ _ _ _ (by omega) _aver_cur_e); (subst _aver_cur_t))); \
all_goals (try simp only [List.length_drop, List.length_cons, List.length_nil, Int.sub_nonneg] at *); \
all_goals (refine ⟨by omega, by omega, ?_⟩); \
all_goals (try simp only [Int.sub_self, Int.toNat_zero, List.drop_zero]); \
all_goals (try rfl); \
all_goals (try rw [_aver_cur_i3]); \
all_goals (try (repeat rw [_aver_seg_drop_chain _ _ _ _ (by omega) (by omega)])); \
all_goals (try (rw [← _aver_seg_drop_shift _ _ _ _ (by omega)])); \
all_goals rfl; done)",
        wrapper = induction::lean_name(wrapper, ctx),
        valid = induction::lean_name(valid, ctx),
        names = names.join(" "),
        drive_holes = holes(observer),
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
        .or_else(|| bounded(law, ctx, scope.as_deref()))
        .or_else(|| events_prefix(law, ctx, scope.as_deref()))
        .or_else(|| step(law, ctx, scope.as_deref()))
        .or_else(|| transport(law, ctx, scope.as_deref()))
}

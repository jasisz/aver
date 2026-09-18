//! A caller's source correspondence across imported call sites, crossed one
//! boundary at a time.
//!
//! The source observer calls an imported observer at each site; the protocol
//! observer drives the imported outcome through the caller's tape and splices
//! the continuation. Both are related through what is already checked: the
//! imported observer's correspondence (its observation is the owner's start
//! segment followed by the caller's drive of the outcome) and, per site, the
//! splice law (splicing a driven outcome is the protocol continuation of that
//! outcome).
//!
//! At each site the rung names the segment observation and the drive result
//! instead of projecting them, so a recursive result is never unfolded and no
//! field of it is copied across the continuation's cases. The protocol side is
//! folded into the splice wrapper by the site's law and the wrapper is then
//! opened, which puts both sides in the same shape: a match on the drive
//! result. Nothing owned by another module is unfolded.
//!
//! The same file closes an import's entry agreement: the caller's entry into
//! the owner's protocol against the lift of the owner's trace from a cursor,
//! read through the owner's cited start step and start cursor. There the only
//! thing of the owner's that opens is the step continuation the citation is
//! stated with.
use super::induction;
use super::segment::{
    self, claims_true, direct_callees, finite, is_finite, read_cited, sole_expression,
};
use crate::ast::{BinOp, Expr, FnDef, Spanned, Stmt, VerifyBlock, VerifyLaw};
use crate::codegen::{CodegenContext, common};

/// What one cited law says, read from its statement alone.
enum Cited<'a> {
    /// `Observer(args) == Entry(args)`: an imported observation is the owner's
    /// segment observation followed by the caller's drive of its outcome.
    Correspondence {
        entry: &'a FnDef,
        segment: &'a FnDef,
        drive: &'a FnDef,
    },
    /// `Observed(args) == Driven(args)`: splicing the drive of an outcome at
    /// one site is the protocol continuation of that outcome.
    Splice {
        observed: &'a FnDef,
        driven: &'a FnDef,
        splice: &'a FnDef,
        drive: &'a FnDef,
    },
}

fn classify<'a>(
    law: &VerifyLaw,
    ctx: &'a CodegenContext,
    scope: Option<&str>,
) -> Option<Cited<'a>> {
    if !claims_true(law) {
        return None;
    }
    let Expr::BinOp(BinOp::Eq, left, right) = &law.lhs.node else {
        return None;
    };
    let observed = finite(left, ctx, scope)?;
    let driven = finite(right, ctx, scope)?;
    if let Some(body) = sole_expression(observed)
        && let Some(splice) = finite(body, ctx, scope)
        && let Expr::FnCall(_, arguments) = &body.node
        && let Some(first) = arguments.first()
        && let Some(drive) = induction::callee(first, ctx, scope)
        && !is_finite(drive, ctx)
    {
        return Some(Cited::Splice {
            observed,
            driven,
            splice,
            drive,
        });
    }
    // The entry observes the owner's segment, a finite record, and drives the
    // outcome it reports.
    let callees = direct_callees(driven, ctx, scope);
    let segment = callees
        .iter()
        .copied()
        .find(|fd| is_finite(fd, ctx) && induction::constructs_result_record(fd))?;
    let drive = callees.iter().copied().find(|fd| !is_finite(fd, ctx))?;
    Some(Cited::Correspondence {
        entry: driven,
        segment,
        drive,
    })
}

pub(super) fn candidate(
    block: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    fact_count: usize,
) -> Option<String> {
    let using = law.using.as_ref()?;
    if fact_count == 0 || !law.because.is_empty() || !claims_true(law) {
        return None;
    }
    let scope = ctx.active_module_scope();
    let scope = scope.as_deref();
    let Expr::BinOp(BinOp::Eq, left, right) = &law.lhs.node else {
        return None;
    };
    let source = finite(left, ctx, scope)?;
    let entry = finite(right, ctx, scope)?;
    let observer = finite(sole_expression(source)?, ctx, scope)?;
    // Facts are numbered in sorted order; sites are crossed in the order the
    // law lists them, which is call order.
    let mut sorted = using.clone();
    sorted.sort();
    let cited = read_cited(law, ctx, scope, |cited, scope| classify(cited, ctx, scope));
    let mut correspondences = Vec::new();
    let mut sites = Vec::new();
    for name in using {
        let index = sorted.iter().position(|s| s == name)?;
        match cited.get(index)?.as_ref()? {
            Cited::Correspondence {
                entry,
                segment,
                drive,
            } => correspondences.push((index, *entry, *segment, *drive)),
            Cited::Splice {
                observed,
                driven,
                splice,
                drive,
            } => sites.push((index, *observed, *driven, *splice, *drive)),
        }
    }
    if correspondences.is_empty() || sites.is_empty() {
        return None;
    }
    // Sites in call order. The splice at one site continues through the
    // observation of every later site, so an earlier site's wrapper reaches
    // more observations than a later one's.
    let observations = |splice: &FnDef| {
        let mut count = 0;
        for stmt in splice.body.stmts() {
            let (Stmt::Binding(_, _, expr) | Stmt::Expr(expr)) = stmt;
            crate::codegen::expr_walk::walk(expr, &mut |expr| {
                if matches!(expr.node, Expr::FnCall(..))
                    && let Some(callee) = induction::callee(expr, ctx, scope)
                    && correspondences
                        .iter()
                        .any(|(_, _, segment, _)| segment.name == callee.name)
                {
                    count += 1;
                }
            });
        }
        count
    };
    sites.sort_by_key(|(_, _, _, splice, _)| std::cmp::Reverse(observations(splice)));
    let heads = [source, entry, observer]
        .iter()
        .map(|fd| induction::lean_name(fd, ctx))
        .collect::<Vec<_>>()
        .join(", ");
    let forward = correspondences
        .iter()
        .map(|(index, ..)| format!("_fact{index}"))
        .collect::<Vec<_>>()
        .join(", ");
    let entries = correspondences
        .iter()
        .map(|(_, entry, ..)| induction::lean_name(entry, ctx))
        .collect::<Vec<_>>()
        .join(", ");
    let holes = |fd: &FnDef| " _".repeat(fd.params.len());
    let mut inner = finish(block, law, ctx, scope, &correspondences, &sites)?;
    for (k, (index, observed, driven, splice, drive)) in sites.iter().enumerate().rev() {
        let (_, _, segment, _) = correspondences
            .iter()
            .find(|(_, _, _, d)| d.name == drive.name)?;
        let k = k + 1;
        inner = format!(
            "(generalize _aver_bnd_seg{k} : {seg}{seg_holes} = _aver_bnd_obs{k}); (cases _aver_bnd_val{k} : _aver_bnd_obs{k}.value); \
all_goals (first | ((simp only [_aver_bnd_val{k}]); done) | ((simp only [_aver_bnd_val{k}]); (simp only [{observed}, {driven}] at _fact{index}); (rw [← _fact{index}]); (simp only [{splice}]); \
(generalize _aver_bnd_drv{k} : {drive}{drive_holes} = _aver_bnd_res{k}); (cases _aver_bnd_rv{k} : _aver_bnd_res{k}.value); \
all_goals (first | ((simp only [_aver_bnd_rv{k}]); done) | ((simp only [_aver_bnd_rv{k}]); {inner}))))",
            seg = induction::lean_name(segment, ctx),
            seg_holes = holes(segment),
            observed = induction::lean_name(observed, ctx),
            driven = induction::lean_name(driven, ctx),
            splice = induction::lean_name(splice, ctx),
            drive = induction::lean_name(drive, ctx),
            drive_holes = holes(drive),
        );
    }
    Some(format!(
        "((simp only [beq_iff_eq] at *); (simp only [{heads}]); (simp only [{forward}]); (simp only [{entries}]); {inner}; done)"
    ))
}

/// The caller's entry into an import agrees with the lift of the owner's
/// trace: `{entry}(args) == {lift}({protocolFrom}(…), …)`, the second
/// explanation of an import's correspondence. The entry observes the owner's
/// start segment through the caller's adapter and drives the outcome; by the
/// cited start step, the owner's trace from a cursor is that same observation
/// followed by the owner's continuation. The observation is named and its
/// cited cursor read on the name; on an outcome the two continuations meet
/// through the cited mapping law, the owner's cited cursor contract and a
/// chain of two `drop`s. The owner's entry, observation and drive are never
/// unfolded; the step wrapper the citation is stated with is.
pub(super) fn entry(
    law: &VerifyLaw,
    reason: &Spanned<Expr>,
    ctx: &CodegenContext,
    fact_count: usize,
) -> Option<String> {
    if fact_count == 0 {
        return None;
    }
    let scope = ctx.active_module_scope();
    let scope = scope.as_deref();
    let agrees = finite(reason, ctx, scope)?;
    // One comparison, after a binding for the empty history.
    let Some(Stmt::Expr(claim)) = agrees.body.stmts().last() else {
        return None;
    };
    let Expr::BinOp(BinOp::Eq, left, right) = &claim.node else {
        return None;
    };
    let start = finite(left, ctx, scope)?;
    let lift = finite(right, ctx, scope)?;
    let Expr::FnCall(_, lifted) = &right.node else {
        return None;
    };
    if !induction::callee(lifted.first()?, ctx, scope)
        .is_some_and(|owner_entry| induction::imported_machinery(owner_entry, ctx, scope))
    {
        return None;
    }
    let callees = direct_callees(start, ctx, scope);
    let adapter = callees
        .iter()
        .copied()
        .find(|fd| is_finite(fd, ctx) && induction::constructs_result_record(fd))?;
    let drive = callees.iter().copied().find(|fd| !is_finite(fd, ctx))?;
    let inner = direct_callees(adapter, ctx, scope);
    let observation = inner
        .iter()
        .copied()
        .find(|fd| induction::imported_machinery(fd, ctx, scope))?;
    let tape = inner
        .iter()
        .copied()
        .find(|fd| induction::is_unary_list_map(fd, ctx))?;
    let cited = read_cited(law, ctx, scope, |cited, scope| {
        segment::classify(cited, ctx, scope)
    });
    let (mut step, mut cursor, mut mapping) = (None, None, None);
    for (index, shape) in cited.iter().enumerate() {
        match shape {
            Some(segment::Cited::Step(wrapper, observer)) if observer.name == observation.name => {
                step = Some((index, *wrapper));
            }
            Some(segment::Cited::Cursor(wrapper, valid, observed))
                if observed.name == observation.name =>
            {
                cursor = Some((index, *wrapper, *valid));
            }
            Some(segment::Cited::Mapping(direct, mapped))
                if sole_expression(direct)
                    .and_then(|body| induction::callee(body, ctx, scope))
                    .is_some_and(|fd| fd.name == drive.name) =>
            {
                mapping = Some((index, *direct, *mapped));
            }
            _ => {}
        }
    }
    let (step_index, step_wrapper) = step?;
    let (cursor_index, cursor_wrapper, cursor_valid) = cursor?;
    let (mapping_index, direct, mapped) = mapping?;
    // The owner's continuation, as named by the step wrapper it was cited with.
    let owner_drive = direct_callees(
        step_wrapper,
        ctx,
        common::fn_owning_scope_for(ctx, step_wrapper),
    )
    .into_iter()
    .find(|fd| !is_finite(fd, ctx))?;
    let (bounded_index, bounded_wrapper, bounded_valid) =
        cited
            .iter()
            .enumerate()
            .find_map(|(index, shape)| match shape {
                Some(segment::Cited::Bounded(wrapper, valid, observer))
                    if observer.name == owner_drive.name =>
                {
                    Some((index, *wrapper, *valid))
                }
                _ => None,
            })?;
    let holes = |fd: &FnDef| " _".repeat(fd.params.len());
    Some(format!(
        "({transport}(have _aver_ent_chain : ∀ {{α : Type}} (l : List α) (c x y : Int), c ≤ x → x ≤ y → (l.drop (x - c).toNat).drop (y - x).toNat = l.drop (y - c).toNat := (by intro α l c x y h0 h1; have hk : (x - c).toNat + (y - x).toNat = (y - c).toNat := (by omega); rw [List.drop_drop, hk])); \
(simp only [beq_iff_eq] at _fact{step_index}); \
(simp only [{cursor_wrapper}, {cursor_valid}, Bool.and_eq_true, beq_iff_eq, decide_eq_true_eq, ge_iff_le] at _fact{cursor_index}); \
(simp only [{bounded_wrapper}, {bounded_valid}, Bool.and_eq_true, beq_iff_eq, decide_eq_true_eq, ge_iff_le] at _fact{bounded_index}); \
(simp only [beq_iff_eq, {direct}, {mapped}] at _fact{mapping_index}); \
(simp only [beq_iff_eq, {agrees}, {start}, {adapter}]); \
(rw [_fact{step_index}]); \
(generalize _aver_ent_h : {observation}{observation_holes} = _aver_ent_o); \
(have _aver_ent_f := _aver_ent_h ▸ _fact{cursor_index}{observation_holes}); \
(obtain ⟨_aver_ent_ge, _aver_ent_le, _aver_ent_rem⟩ : _ ∧ _ ∧ _ := _aver_ent_f); \
(simp only [{step_wrapper}]); \
(cases _aver_ent_v : _aver_ent_o.value); \
all_goals (try simp only [_aver_ent_v]); \
all_goals (first | ((simp only [{lift}]); done) | ((rw [_aver_ent_rem, _aver_transport_drop_0, _fact{mapping_index}]); (generalize _aver_ent_hd : {owner_drive}{drive_holes} = _aver_ent_d); (have _aver_ent_g := _aver_ent_hd ▸ _fact{bounded_index}{drive_holes}); (obtain ⟨_aver_ent_dge, _aver_ent_dle, _aver_ent_drem⟩ : _ ∧ _ ∧ _ := _aver_ent_g); (simp only [{lift}]); (rw [_aver_ent_chain _ _ _ _ (by omega) (by omega)]); done)); done)",
        transport = induction::checked_map_lemmas(&[induction::lean_name(tape, ctx)], true),
        cursor_wrapper = induction::lean_name(cursor_wrapper, ctx),
        cursor_valid = induction::lean_name(cursor_valid, ctx),
        bounded_wrapper = induction::lean_name(bounded_wrapper, ctx),
        bounded_valid = induction::lean_name(bounded_valid, ctx),
        direct = induction::lean_name(direct, ctx),
        mapped = induction::lean_name(mapped, ctx),
        agrees = induction::lean_name(agrees, ctx),
        start = induction::lean_name(start, ctx),
        adapter = induction::lean_name(adapter, ctx),
        observation = induction::lean_name(observation, ctx),
        observation_holes = holes(observation),
        step_wrapper = induction::lean_name(step_wrapper, ctx),
        lift = induction::lean_name(lift, ctx),
        owner_drive = induction::lean_name(owner_drive, ctx),
        drive_holes = holes(owner_drive),
    ))
}

/// Close what remains after the last site: the caller's own continuation on a
/// named drive result. Its routers, joins and answers open, and so does its
/// own protocol drive; nothing that builds a trace record or reaches into
/// another module does, and the drives crossed above stay named. That is
/// stricter than `induction::imported_machinery`: this rung names every
/// foreign term it meets and has no use for another module's routers.
fn finish(
    block: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    scope: Option<&str>,
    correspondences: &[(usize, &FnDef, &FnDef, &FnDef)],
    sites: &[(usize, &FnDef, &FnDef, &FnDef, &FnDef)],
) -> Option<String> {
    let id = ctx.law_target_fn_id(&block.fn_name)?;
    let theorem = ctx
        .proof_ir
        .law_theorems
        .iter()
        .find(|t| t.fn_id == id && t.law_name == law.name)?;
    let crossed: Vec<&str> = correspondences
        .iter()
        .flat_map(|(_, entry, segment, drive)| {
            [
                entry.name.as_str(),
                segment.name.as_str(),
                drive.name.as_str(),
            ]
        })
        .chain(sites.iter().map(|(_, _, _, _, drive)| drive.name.as_str()))
        .collect();
    let mut names = Vec::new();
    for id in &theorem.function_cone {
        let key = &ctx.symbol_table.fn_entry(*id).key;
        let Some(fd) = ctx.fn_def_by_name(&key.name, key.scope_str()) else {
            continue;
        };
        if common::fn_owning_scope_for(ctx, fd) != scope
            || !fd.effects.is_empty()
            || crossed.contains(&fd.name.as_str())
        {
            continue;
        }
        // Routers, joins and answers continue the caller's protocol; an
        // observation or a wrapper that builds a trace record is either named
        // above or reaches into another module.
        if !is_finite(fd, ctx) || !induction::constructs_result_record(fd) {
            let name = induction::lean_name(fd, ctx);
            if !names.contains(&name) {
                names.push(name);
            }
        }
    }
    Some(format!(
        "(repeat' first | rfl | (simp_all +zetaDelta [{}]) | split); done",
        names.join(", ")
    ))
}

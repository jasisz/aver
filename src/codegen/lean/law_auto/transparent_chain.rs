//! Transparent arithmetic premise-chain close.
//!
//! This arm is deliberately syntax-discovery-only: it recognizes a `when` law
//! whose premises are calls to already-citable law predicates, then unfolds only
//! those predicate bodies plus their transparent non-recursive helpers. The
//! body gate is shape-keyed, not name-keyed.
//!
//! Probe-gated exactly like the keystone (`induction::keystone`): both the
//! statement recognizer ([`recognize_transparent_chain`], which feeds the
//! `omit_domain` universal-statement driver) and the proof emit
//! ([`emit_transparent_chain_law`]) key on
//! [`tactic_ir::speculative::admits`], so a chain whose `omega` close the probe
//! cannot certify REVERTS to the sound bounded sampled-domain statement instead
//! of forcing a passing project red. Outside any probe (`admits` default
//! `false`) the law stays bounded — byte-identical to before this arm.
//!
//! Named boundary: the citable-pool check resolves premises against EARLIER
//! sibling laws in the entry module (`enclosing_verify_blocks`) or a dependency
//! module's own law list. Under entry-only speculative probing a dependency
//! module's `when`-law that is itself only probe-committable is not yet a stable
//! citation, so the intended fixtures keep the cited pool laws in the SAME module
//! as the chain law. Widening to freely cross-module pool citations is a
//! separate change and deliberately out of scope here.

use std::collections::BTreeSet;

use super::super::expr::aver_name_to_lean;
use super::{AutoProof, shared};
use crate::ast::{
    BinOp, Expr, FnDef, Literal, Pattern, Spanned, VerifyBlock, VerifyKind, VerifyLaw,
};
use crate::codegen::CodegenContext;
use crate::codegen::lean::tactic_ir::speculative;

struct TransparentChain {
    subject_lean: String,
    premise_unfolds: Vec<String>,
}

struct ResolvedFn<'a> {
    fd: &'a FnDef,
    scope: Option<String>,
    lean_name: String,
}

#[derive(Default)]
struct FragmentStats {
    premise_splits: usize,
}

struct CollectState<'a> {
    visited: &'a mut BTreeSet<String>,
    unfolds: &'a mut Vec<String>,
    stats: &'a mut FragmentStats,
}

pub(in crate::codegen::lean) fn recognize_transparent_chain(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> bool {
    if recognize_transparent_chain_shape(vb, law, ctx).is_none() {
        return false;
    }
    // Probe-gated: the universal statement form (`omit_domain`) and the proof
    // emit stay in lockstep because both consult `admits`. Default `false` keeps
    // a non-probe transpile on the bounded fallback (keystone precedent).
    speculative::admits(&law_id(vb, law), false)
}

pub(in crate::codegen::lean) fn emit_transparent_chain_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
) -> Option<AutoProof> {
    let TransparentChain {
        subject_lean,
        premise_unfolds,
    } = recognize_transparent_chain_shape(vb, law, ctx)?;
    // Same gate as the statement recognizer: a chain the probe did not commit
    // (or a plain transpile with no probe) declines here, so the caller falls
    // through to the bounded guarded-domain fallback and the statement stays
    // bounded.
    let id = law_id(vb, law);
    if !speculative::admits(&id, false) {
        return None;
    }
    let mut intros = intro_names.to_vec();
    intros.push("h_when".to_string());
    let premise_defs = premise_unfolds.join(" ");
    // Fail-closed floor. Under the probe it carries the `AVERSPEC_SORRY:<id>`
    // trace so a non-closing chain surfaces in the build log (and `record_probed`
    // registers the id as one that emitted a floor); the committed re-emit then
    // states only the closers universally. Off-probe it is a bare `sorry`.
    let floor = if speculative::probing() {
        speculative::record_probed(&id);
        format!("(trace \"AVERSPEC_SORRY:{id}\"; sorry)")
    } else {
        "sorry".to_string()
    };
    Some(AutoProof {
        support_lines: Vec::new(),
        body: crate::codegen::lean::tactic_ir::Tactic::raw(super::intro_then(
            &intros,
            vec![format!(
                "first | (unfold {subject_lean}; unfold {premise_defs} at h_when; simp only [Bool.and_eq_true, decide_eq_true_eq, ge_iff_le] at h_when ⊢; split at h_when <;> omega) | {floor}"
            )],
        )),
        replaces_theorem: false,
    })
}

fn law_id(vb: &VerifyBlock, law: &VerifyLaw) -> String {
    format!("{}.{}", vb.fn_name, law.name)
}

fn recognize_transparent_chain_shape(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Option<TransparentChain> {
    law.when.as_ref()?;
    if law.givens.is_empty() || law.givens.iter().any(|g| g.type_name.trim() != "Int") {
        return None;
    }
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
    if !matches!(&law.rhs.node, Expr::Literal(Literal::Bool(true))) {
        return None;
    }

    let Expr::FnCall(subject_callee, subject_args) = &law.lhs.node else {
        return None;
    };
    let subject_call = shared::expr_dotted_name(subject_callee)?;
    if bare_basename(&subject_call) != vb.fn_name {
        return None;
    }
    if subject_args.iter().any(|arg| !plain_term(arg)) {
        return None;
    }
    let subject = resolve_fn_for_call(ctx, &subject_call, ctx.active_module_scope().as_deref())?;
    if subject.fd.name != vb.fn_name || !fn_is_pure_nonrecursive(ctx, &subject) {
        return None;
    }
    let subject_body = shared::body_terminal_expr(&subject.fd.body)?;
    if !plain_bool(subject_body) {
        return None;
    }

    let conjuncts = shared::collect_when_clauses(law.when.as_ref()?);
    if conjuncts.is_empty() {
        return None;
    }

    let mut unfolds = Vec::new();
    let mut visited = BTreeSet::new();
    let mut stats = FragmentStats::default();
    for conjunct in &conjuncts {
        let (call_name, args) = shared::call_name_args(conjunct)?;
        if !has_citable_pool_law_for_call(vb, &call_name, ctx) {
            return None;
        }
        let cited = resolve_fn_for_call(ctx, &call_name, ctx.active_module_scope().as_deref())?;
        if cited.fd.return_type.trim() != "Bool" || !fn_is_pure_nonrecursive(ctx, &cited) {
            return None;
        }
        if args.iter().any(|arg| !plain_term(arg)) {
            return None;
        }
        let mut state = CollectState {
            visited: &mut visited,
            unfolds: &mut unfolds,
            stats: &mut stats,
        };
        collect_transparent_premise_fn(ctx, cited, &mut state)?;
    }

    // The measured core tactic is `split at h_when <;> omega`; decline shapes
    // with no premise-side split or several nested splits instead of guessing a
    // different tactic.
    if stats.premise_splits != 1 || unfolds.is_empty() {
        return None;
    }

    Some(TransparentChain {
        subject_lean: subject.lean_name,
        premise_unfolds: unfolds,
    })
}

fn collect_transparent_premise_fn(
    ctx: &CodegenContext,
    resolved: ResolvedFn<'_>,
    state: &mut CollectState<'_>,
) -> Option<()> {
    let key = scoped_key(resolved.scope.as_deref(), &resolved.fd.name);
    if !state.visited.insert(key) {
        return Some(());
    }
    if !fn_is_pure_nonrecursive(ctx, &resolved) {
        return None;
    }
    let body = shared::body_terminal_expr(&resolved.fd.body)?;
    state.unfolds.push(resolved.lean_name.clone());
    match resolved.fd.return_type.trim() {
        "Bool" => premise_bool(body, ctx, resolved.scope.as_deref(), state).then_some(()),
        "Int" => premise_term(body, ctx, resolved.scope.as_deref(), state).then_some(()),
        _ => None,
    }
}

fn plain_bool(expr: &Spanned<Expr>) -> bool {
    match &expr.node {
        Expr::Literal(Literal::Bool(_)) => true,
        Expr::BinOp(op, l, r) if comparison_op(*op) => plain_term(l) && plain_term(r),
        _ => false,
    }
}

fn plain_term(expr: &Spanned<Expr>) -> bool {
    match &expr.node {
        Expr::Literal(Literal::Int(_)) => true,
        Expr::Ident(_) | Expr::Resolved { .. } => true,
        Expr::Neg(inner) => plain_term(inner),
        Expr::BinOp(BinOp::Add | BinOp::Sub, l, r) => plain_term(l) && plain_term(r),
        Expr::BinOp(BinOp::Mul, l, r) => {
            (int_literal(l) && plain_term(r)) || (int_literal(r) && plain_term(l))
        }
        _ => false,
    }
}

fn premise_bool(
    expr: &Spanned<Expr>,
    ctx: &CodegenContext,
    owner_scope: Option<&str>,
    state: &mut CollectState<'_>,
) -> bool {
    if let Some(args) = shared::call_qualified(expr, "Bool.and", 2) {
        return premise_bool(&args[0], ctx, owner_scope, state)
            && premise_bool(&args[1], ctx, owner_scope, state);
    }
    if let Some((name, args)) = shared::call_name_args(expr) {
        for arg in args {
            if !premise_term(arg, ctx, owner_scope, state) {
                return false;
            }
        }
        let Some(resolved) = resolve_fn_for_call(ctx, &name, owner_scope) else {
            return false;
        };
        if resolved.fd.return_type.trim() != "Bool" {
            return false;
        }
        return collect_transparent_premise_fn(ctx, resolved, state).is_some();
    }
    match &expr.node {
        Expr::Literal(Literal::Bool(_)) => true,
        Expr::BinOp(op, l, r) if comparison_op(*op) => {
            premise_term(l, ctx, owner_scope, state) && premise_term(r, ctx, owner_scope, state)
        }
        Expr::Match { subject, arms } => {
            if !premise_bool(subject, ctx, owner_scope, state) {
                return false;
            }
            let mut saw_true = false;
            let mut saw_false = false;
            for arm in arms {
                match bool_pattern(&arm.pattern) {
                    Some(true) => saw_true = true,
                    Some(false) => saw_false = true,
                    None => return false,
                }
                if !premise_bool(&arm.body, ctx, owner_scope, state) {
                    return false;
                }
            }
            if saw_true && saw_false {
                state.stats.premise_splits += 1;
                true
            } else {
                false
            }
        }
        _ => false,
    }
}

fn premise_term(
    expr: &Spanned<Expr>,
    ctx: &CodegenContext,
    owner_scope: Option<&str>,
    state: &mut CollectState<'_>,
) -> bool {
    if let Some((name, args)) = shared::call_name_args(expr) {
        for arg in args {
            if !premise_term(arg, ctx, owner_scope, state) {
                return false;
            }
        }
        let Some(resolved) = resolve_fn_for_call(ctx, &name, owner_scope) else {
            return false;
        };
        if resolved.fd.return_type.trim() != "Int" {
            return false;
        }
        return collect_transparent_premise_fn(ctx, resolved, state).is_some();
    }
    match &expr.node {
        Expr::Literal(Literal::Int(_)) => true,
        Expr::Ident(_) | Expr::Resolved { .. } => true,
        Expr::Neg(inner) => premise_term(inner, ctx, owner_scope, state),
        Expr::BinOp(BinOp::Add | BinOp::Sub, l, r) => {
            premise_term(l, ctx, owner_scope, state) && premise_term(r, ctx, owner_scope, state)
        }
        Expr::BinOp(BinOp::Mul, l, r) => {
            (int_literal(l) && premise_term(r, ctx, owner_scope, state))
                || (int_literal(r) && premise_term(l, ctx, owner_scope, state))
        }
        Expr::Match { subject, arms } => {
            if !premise_bool(subject, ctx, owner_scope, state) {
                return false;
            }
            let mut saw_true = false;
            let mut saw_false = false;
            for arm in arms {
                match bool_pattern(&arm.pattern) {
                    Some(true) => saw_true = true,
                    Some(false) => saw_false = true,
                    None => return false,
                }
                if !premise_term(&arm.body, ctx, owner_scope, state) {
                    return false;
                }
            }
            if saw_true && saw_false {
                state.stats.premise_splits += 1;
                true
            } else {
                false
            }
        }
        _ => false,
    }
}

fn comparison_op(op: BinOp) -> bool {
    matches!(
        op,
        BinOp::Eq | BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte
    )
}

fn int_literal(expr: &Spanned<Expr>) -> bool {
    matches!(&expr.node, Expr::Literal(Literal::Int(_)))
}

fn bool_pattern(pattern: &Pattern) -> Option<bool> {
    match pattern {
        Pattern::Literal(Literal::Bool(value)) => Some(*value),
        _ => None,
    }
}

fn fn_is_pure_nonrecursive(ctx: &CodegenContext, resolved: &ResolvedFn<'_>) -> bool {
    if !resolved.fd.effects.is_empty() {
        return false;
    }
    let recursive = super::super::recursive_pure_fn_names(ctx);
    !recursive.contains(&resolved.fd.name)
}

fn resolve_fn_for_call<'a>(
    ctx: &'a CodegenContext,
    call_name: &str,
    owner_scope: Option<&str>,
) -> Option<ResolvedFn<'a>> {
    if let Some((prefix, short)) = split_module_call(ctx, call_name) {
        let fd = ctx.fn_def_by_name(short, Some(prefix)).or_else(|| {
            ctx.modules
                .iter()
                .find(|m| m.prefix == prefix)?
                .fn_defs
                .iter()
                .find(|fd| fd.name == short)
        })?;
        return Some(ResolvedFn {
            fd,
            scope: Some(prefix.to_string()),
            lean_name: crate::codegen::lean::syntax::aver_path_to_lean(&format!(
                "{prefix}.{short}"
            )),
        });
    }

    if let Some(scope) = owner_scope
        && let Some(module) = ctx.modules.iter().find(|m| m.prefix == scope)
        && let Some(fd) = ctx
            .fn_def_by_name(call_name, Some(scope))
            .or_else(|| module.fn_defs.iter().find(|fd| fd.name == call_name))
    {
        return Some(ResolvedFn {
            fd,
            scope: Some(scope.to_string()),
            lean_name: crate::codegen::lean::syntax::aver_path_to_lean(&format!(
                "{scope}.{}",
                fd.name
            )),
        });
    }

    let fd = ctx
        .fn_def_by_name(call_name, None)
        .or_else(|| ctx.fn_defs.iter().find(|fd| fd.name == call_name))?;
    Some(ResolvedFn {
        fd,
        scope: None,
        lean_name: aver_name_to_lean(&fd.name),
    })
}

fn split_module_call<'a>(
    ctx: &'a CodegenContext,
    call_name: &'a str,
) -> Option<(&'a str, &'a str)> {
    ctx.modules
        .iter()
        .filter_map(|module| {
            call_name
                .strip_prefix(&format!("{}.", module.prefix))
                .map(|short| (module.prefix.as_str(), short))
        })
        .max_by_key(|(prefix, _)| prefix.len())
}

/// Whether an EARLIER law block establishes the premise predicate `call_name` as
/// a pool member — a `verify <fn> law <name>` whose `law_as_lemma_statement` is
/// well-formed. This is a RECOGNITION gate (the arm only fires on premises that
/// name a law-backed predicate), NOT a soundness dependency: the emitted proof
/// never cites the pool law theorem — it `unfold`s the predicate fn's own body
/// (definitional) and discharges the goal from the introduced premises `h_when`
/// with `omega`, then the `#print axioms` whitelist + kernel recheck certify it.
/// So a pool law that is itself only bounded (or, in principle, unproven) cannot
/// lend false credit here; checking mere statement shape rather than
/// proven-universal tier is therefore truthful for this arm.
fn has_citable_pool_law_for_call(vb: &VerifyBlock, call_name: &str, ctx: &CodegenContext) -> bool {
    use crate::codegen::lean::toplevel::law_as_lemma_statement;
    if let Some((prefix, short)) = split_module_call(ctx, call_name)
        && let Some(module) = ctx.modules.iter().find(|m| m.prefix == prefix)
    {
        return module.verify_laws.iter().any(|prev| {
            if prev.fn_name != short {
                return false;
            }
            let VerifyKind::Law(prev_law) = &prev.kind else {
                return false;
            };
            ctx.with_module_scope(Some(prefix), || {
                law_as_lemma_statement(prev, prev_law, ctx).is_some()
            })
        });
    }

    let short = bare_basename(call_name);
    for prev in enclosing_verify_blocks(vb, ctx) {
        if prev.line == vb.line && prev.fn_name == vb.fn_name {
            break;
        }
        if prev.fn_name != short {
            continue;
        }
        let VerifyKind::Law(prev_law) = &prev.kind else {
            continue;
        };
        if law_as_lemma_statement(prev, prev_law, ctx).is_some() {
            return true;
        }
    }
    false
}

fn enclosing_verify_blocks<'a>(vb: &VerifyBlock, ctx: &'a CodegenContext) -> Vec<&'a VerifyBlock> {
    use crate::ast::TopLevel;
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
        .filter_map(|item| match item {
            TopLevel::Verify(block) => Some(block),
            _ => None,
        })
        .collect()
}

fn scoped_key(scope: Option<&str>, name: &str) -> String {
    match scope {
        Some(scope) => format!("{scope}.{name}"),
        None => name.to_string(),
    }
}

fn bare_basename(name: &str) -> &str {
    super::shared::bare_lean_name(name)
}

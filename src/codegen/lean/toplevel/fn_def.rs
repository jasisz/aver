use std::collections::HashSet;

use super::expr::aver_name_to_lean;
use super::fuel::{
    contract_lex_params_rank, detect_simple_string_pos_skip_literal,
    emit_fuelized_int_ascending_fn, emit_fuelized_int_countdown_fn,
    emit_fuelized_mutual_int_countdown_group, emit_fuelized_mutual_sizeof_group,
    emit_fuelized_mutual_string_pos_group, emit_fuelized_string_pos_fn,
    emit_nat_linear_recurrence_fn, emit_native_guarded_int_countdown_fn,
    emit_native_mutual_sizeof_group,
};
use super::is_pure_fn;
use super::lex_list::emit_native_mutual_lex_list_wf_group;
use super::recurrence::detect_second_order_int_linear_recurrence;
use super::render::{emit_fn_params, sanitize_doc};
use super::types::type_annotation_to_lean;
use crate::ast::*;
use crate::codegen::CodegenContext;

/// Emit a Lean 4 function definition from an Aver FnDef.
/// Returns `None` if the function should be skipped (effectful, main).
pub fn emit_fn_def(
    fd: &FnDef,
    recursive_fns: &HashSet<String>,
    ctx: &CodegenContext,
) -> Option<String> {
    if !is_pure_fn(fd) {
        return None;
    }

    let mut lines = Vec::new();

    // Doc comment from description
    if let Some(desc) = &fd.desc {
        lines.push(format!("/-- {} -/", sanitize_doc(desc)));
    }

    let is_recursive = recursive_fns.contains(&fd.name);
    let fn_name = aver_name_to_lean(&fd.name);

    // Parameters — lifted fn keeps the plain function type for oracle
    // bindings; the subtype constraint is enforced at the lemma level
    // (`∀ rng : RandomIntInBounds, ...`) where it bites the universal
    // claim. Threading the subtype through the operational signature
    // would force every sample binding (`theorem ..._sample_1`) to
    // wrap concrete stubs in `⟨stub, by sorry⟩`, since most user
    // stubs (e.g. `counterStub : fn p n min max := n + min`) only
    // satisfy the bound at specific `(min, max)` pairs and `decide`
    // can't discharge the `∀ min max` quantifier. Sound for the
    // universal lemma, executable for the concrete sample — that's
    // the trade.
    let params = emit_fn_params(&fd.params);

    // Return type
    let ret_type = if fd.return_type.is_empty() {
        "Unit".to_string()
    } else {
        type_annotation_to_lean(&fd.return_type)
    };

    // partial for recursive functions
    let prefix = if is_recursive { "partial " } else { "" };

    lines.push(format!(
        "{}def {} {} : {} :=",
        prefix, fn_name, params, ret_type
    ));
    let lowered = lower_pure_question_bang_for_emit(fd);
    let body = lowered
        .as_ref()
        .map(|lowered_fd| lowered_fd.body.as_ref())
        .unwrap_or(fd.body.as_ref());
    lines.push(emit_fn_body_for(fd, body, ctx));

    Some(lines.join("\n"))
}

/// Proof-mode function emission. Reads the contract decision from
/// `ctx.proof_ir.fn_contracts` and dispatches to the matching emit fn
/// (native guarded, fuel-encoded, pair-state Nat worker, etc.). Falls
/// back to plain `def` emission when no contract is present (non-
/// recursive fn).
pub fn emit_fn_def_proof(fd: &FnDef, ctx: &CodegenContext) -> Option<String> {
    if !is_pure_fn(fd) {
        return None;
    }

    // LinearRecurrence2 — dedicated `RecursionContract::LinearRecurrence2`
    // marker. Backend still calls `detect_second_order_int_linear_
    // recurrence` to extract base cases + coefficients; the contract
    // just signals "this fn lowers as pair-state Nat worker, not fuel".
    if let Some(contract) = crate::codegen::common::find_fn_contract_for_fn(ctx, fd)
        && matches!(
            contract.recursion,
            Some(crate::ir::RecursionContract::LinearRecurrence2)
        )
        && let Some(shape) = detect_second_order_int_linear_recurrence(fd)
    {
        return Some(emit_nat_linear_recurrence_fn(fd, &shape, ctx));
    }

    // IntCountdown now reads through ProofIR's `Fuel { NatAbsPlusOne }`
    // contract. Fuel encoding stays — native `termination_by n.natAbs`
    // would require `(n - 1).natAbs < n.natAbs` which only holds for
    // `n > 0`; Aver bodies don't always clamp to non-negative before
    // recursing (fibTR sans-guard relies on its caller). Fuel
    // sidesteps the issue.
    if let Some(contract) = crate::codegen::common::find_fn_contract_for_fn(ctx, fd)
        && let Some(crate::ir::RecursionContract::Fuel {
            fuel_metric: crate::ir::FuelMetric::NatAbsPlusOne { param },
        }) = contract.recursion.as_ref()
        && let Some(param_index) = fd.params.iter().position(|(n, _)| n == param)
    {
        return Some(emit_fuelized_int_countdown_fn(fd, ctx, param_index));
    }

    // WellFoundedToNat — native well-founded def on `param.toNat`.
    // Two validated sources (see the contract docs): the
    // guard-validated floor-division countdown (`floor_div: Some`)
    // and the guarded subtractive countdown a floor-division window
    // law graduated out of fuel (`floor_div: None`). The kernel
    // re-checks the measure through `decreasing_by`: the branch
    // hypotheses of the emitted if/else chain land in the decreasing
    // goals' context, `simp [<wrapper>, Except.withDefault]` reduces
    // the literal-divisor zero-guard, and `omega` (which understands
    // `Int.toNat` and ediv by literals) closes the strict decrease.
    if let Some(contract) = crate::codegen::common::find_fn_contract_for_fn(ctx, fd)
        && let Some(crate::ir::RecursionContract::WellFoundedToNat { param, floor_div }) =
            contract.recursion.as_ref()
    {
        let mut lines = Vec::new();
        if let Some(desc) = &fd.desc {
            lines.push(format!("/-- {} -/", sanitize_doc(desc)));
        }
        let fn_name = aver_name_to_lean(&fd.name);
        let params = emit_fn_params(&fd.params);
        let ret_type = if fd.return_type.is_empty() {
            "Unit".to_string()
        } else {
            type_annotation_to_lean(&fd.return_type)
        };
        lines.push(format!("def {} {} : {} :=", fn_name, params, ret_type));
        let lowered = lower_pure_question_bang_for_emit(fd);
        let body = lowered
            .as_ref()
            .map(|lowered_fd| lowered_fd.body.as_ref())
            .unwrap_or(fd.body.as_ref());
        lines.push(emit_fn_body_for(fd, body, ctx));
        lines.push(format!("termination_by {}.toNat", aver_name_to_lean(param)));
        lines.push("decreasing_by".to_string());
        match floor_div {
            Some(shrink) => match &shrink.helper_fn {
                Some(helper) => lines.push(format!(
                    "  all_goals (simp [{}, Except.withDefault] <;> omega)",
                    aver_name_to_lean(helper)
                )),
                None => lines.push("  all_goals (simp [Except.withDefault] <;> omega)".to_string()),
            },
            None => lines.push("  all_goals omega".to_string()),
        }
        return Some(lines.join("\n"));
    }

    // IntCountdownGuarded now reads through ProofIR — the lowerer
    // populates `ctx.proof_ir.fn_contracts` with a `Native` contract
    // whose `precondition` + `body` carry everything the emit needs.
    // Other RecursionPlan variants still flow through `recursion_plan`
    // directly; Step 7+ migrates them one shape at a time.
    if let Some(contract) = crate::codegen::common::find_fn_contract_for_fn(ctx, fd)
        && let Some(crate::ir::RecursionContract::Native {
            precondition,
            measure: crate::ir::Measure::NatAbsInt { param },
            body,
            ..
        }) = contract.recursion.as_ref()
    {
        // Measure binds the countdown param by name; map back to the
        // arg-position index the emit fn expects. Falls through if the
        // param somehow vanished (shouldn't happen — populator just
        // pulled it from fd.params).
        if let Some(param_index) = fd.params.iter().position(|(n, _)| n == param) {
            let precondition_clauses: Vec<crate::ast::Spanned<crate::ir::hir::ResolvedExpr>> =
                precondition.iter().map(|p| p.expr.clone()).collect();
            return Some(emit_native_guarded_int_countdown_fn(
                fd,
                ctx,
                param_index,
                body.base_arm_literal,
                &body.base_arm_body,
                &body.wildcard_arm_body,
                &precondition_clauses,
            ));
        }
    }

    // IntAscending reads `Fuel { BoundMinusParamNatAbsPlusOne }`.
    // The bound stays as `Spanned<Expr>` in the contract; backend
    // renders it through `bound_expr_to_lean` here.
    if let Some(contract) = crate::codegen::common::find_fn_contract_for_fn(ctx, fd)
        && let Some(crate::ir::RecursionContract::Fuel {
            fuel_metric: crate::ir::FuelMetric::BoundMinusParamNatAbsPlusOne { param, bound },
        }) = contract.recursion.as_ref()
        && let Some(param_index) = fd.params.iter().position(|(n, _)| n == param)
    {
        let bound_lean = super::bound_expr_to_lean(bound);
        return Some(emit_fuelized_int_ascending_fn(
            fd,
            ctx,
            param_index,
            &bound_lean,
        ));
    }

    // SizeOfStructural — `Fuel { SizeOfPlusOne }`. The classifier only assigns
    // this contract when the recursion strictly shrinks a recursive sub-term
    // binder (`supports_single_sizeof_structural`), i.e. it is genuine
    // structural recursion on the user ADT's immediate sub-fields. Lean's
    // equation compiler accepts exactly that natively, so we emit a plain `def`
    // (fall through below) and let Lean infer structural termination — NO fuel.
    //
    // This is strictly better than the old fuel helper: a plain structural `def`
    // has DEFINITIONAL recursive equations (`height (Node l y r) = …` is `rfl`),
    // whereas a fuel counter destroys that (the fuel arg on a child differs from
    // the child's own measure, so `simp [f]`/`omega` can't unfold it for
    // symbolic/universal proofs — the very reason fuel forced the universal law
    // to be skipped, Issue #128). Empirically (Lean 4.15) naive structural also
    // covers mutual / accumulator / lexicographic recursion; only recursion
    // hidden inside a higher-order container combinator (e.g. `kids.map f` over a
    // nested-recursive field) needs an explicit well-founded measure, and that
    // shape is never classified SizeOfStructural.
    //
    // The Peano-lift case already fell through here for the same reason
    // (`recurses_on_peano` → structural on `Nat.rec`); it now shares the path.

    // StringPosAdvance — `Fuel { StringLenMinusPos { string, pos } }`.
    // Lean's emit reads the params from fd.params directly so the
    // contract just acts as the dispatch signal.
    //
    // GRADUATION: the simple SKIP shape (`match charAtAv s pos { none =>
    // pos; some c => match c { <lit> => self(s, pos+1) …; _ => pos } }`,
    // recognized by `detect_simple_string_pos_skip_literal`) emits a
    // native fuel-free `def` with `termination_by (s.length - pos)` so
    // Lean derives a real `<fn>.induct` for the `fun_induction` law rung.
    // The tight recognizer IS the fail-closed floor: it fires only where
    // every self-call sits under the outer `charAtAv s pos = some`
    // guard, which forces `pos.toNat < s.length` — the exact fact the
    // `decreasing_by` needs. Any other string-position shape (unbounded
    // advance, carried-param scanner, mutual clique) keeps the fuel
    // wrapper, since a native `def` cannot sorry-floor its termination.
    if let Some(contract) = crate::codegen::common::find_fn_contract_for_fn(ctx, fd)
        && matches!(
            contract.recursion,
            Some(crate::ir::RecursionContract::Fuel {
                fuel_metric: crate::ir::FuelMetric::StringLenMinusPos { .. },
            })
        )
    {
        if detect_simple_string_pos_skip_literal(fd).is_some() {
            return Some(emit_native_string_pos_skip_fn(fd, ctx));
        }
        return Some(emit_fuelized_string_pos_fn(fd, ctx));
    }

    let mut lines = Vec::new();
    if let Some(desc) = &fd.desc {
        lines.push(format!("/-- {} -/", sanitize_doc(desc)));
    }

    let fn_name = aver_name_to_lean(&fd.name);
    let params = emit_fn_params(&fd.params);
    let ret_type = if fd.return_type.is_empty() {
        "Unit".to_string()
    } else {
        type_annotation_to_lean(&fd.return_type)
    };
    lines.push(format!("def {} {} : {} :=", fn_name, params, ret_type));
    let lowered = lower_pure_question_bang_for_emit(fd);
    let body = lowered
        .as_ref()
        .map(|lowered_fd| lowered_fd.body.as_ref())
        .unwrap_or(fd.body.as_ref());
    lines.push(emit_fn_body_for(fd, body, ctx));

    // termination_by/decreasing_by suffix for the few contract shapes
    // that need explicit Lean termination hints (rest are no-ops —
    // their emit fns already wrote them, or Lean's elaborator infers).
    if let Some(contract) = crate::codegen::common::find_fn_contract_for_fn(ctx, fd) {
        match contract.recursion.as_ref() {
            Some(crate::ir::RecursionContract::Fuel {
                fuel_metric: crate::ir::FuelMetric::Lex { params, rank: 0 },
            }) if params.len() == 1 => {
                // MutualIntCountdown — every member counts down the
                // shared first-Int param.
                let lean_param = aver_name_to_lean(&params[0]);
                lines.push(format!("termination_by Int.natAbs {}", lean_param));
                lines.push("decreasing_by".to_string());
                lines.push("  omega".to_string());
            }
            Some(crate::ir::RecursionContract::Fuel {
                fuel_metric: crate::ir::FuelMetric::SeqLenPlusOne { param },
            }) => {
                // ListStructural — Lean structural recursion on
                // `<param>.length`. The `+1` framing in the IR is
                // ignored here; Lean's elaborator wants the bare
                // length measure.
                let lean_param = aver_name_to_lean(param);
                lines.push(format!("termination_by {}.length", lean_param));
                lines.push("decreasing_by".to_string());
                lines.push("  decreasing_tactic".to_string());
            }
            _ => {}
        }
    }

    Some(lines.join("\n"))
}

/// Native (fuel-free) emission for a graduated single-fn string-position
/// SKIP scanner (the skipWs shape). Gated upstream by
/// [`detect_simple_string_pos_skip_literal`], so the whole body is
/// `match charAtAv s pos { none => pos; some c => match c { <lit> =>
/// self(s, pos+1) …; _ => pos } }` and every recursive call sits under
/// the outer `charAtAv s pos = some c` guard on the SAME `(s, pos)`.
///
/// Two moves make Lean accept the native def:
///  1. The outer `charAtAv` match is named (`match hc_scan : …`). Lean
///     drops the discriminant equation under a plain `match`, so
///     `decreasing_by` would otherwise not see `charAtAv s pos = some c`.
///     The general match emitter only names Ident/Attr subjects (not the
///     `charAtAv` FnCall), so we inject the binder on the sole outer
///     `match String.charAtAv` line here.
///  2. `decreasing_by` cites the prelude's `String.charAt_some_bounds`
///     to turn that equation into `pos.toNat < s.toList.length`, then
///     `omega` closes the strict decrease of `s.toList.length - pos.toNat`.
///
/// Result: a real `<fn>.induct`, which the `fun_induction` law rung
/// (unavailable to the fuel wrapper's non-recursive `.induct`) uses to
/// close universal progress laws.
fn emit_native_string_pos_skip_fn(fd: &FnDef, ctx: &CodegenContext) -> String {
    let fn_name = aver_name_to_lean(&fd.name);
    let params = emit_fn_params(&fd.params);
    let ret_type = if fd.return_type.is_empty() {
        "Unit".to_string()
    } else {
        type_annotation_to_lean(&fd.return_type)
    };
    let s = aver_name_to_lean(&fd.params[0].0);
    let pos = aver_name_to_lean(&fd.params[1].0);

    let lowered = lower_pure_question_bang_for_emit(fd);
    let body = lowered
        .as_ref()
        .map(|lowered_fd| lowered_fd.body.as_ref())
        .unwrap_or(fd.body.as_ref());
    // Native self-calls (no fuel rewrite); Lean's wf compiler handles the
    // recursion once `termination_by`/`decreasing_by` are supplied.
    let body_str = emit_fn_body_for(fd, body, ctx);
    // Name the sole outer `charAtAv` discriminant so its `= some c`
    // equation reaches `decreasing_by`.
    let body_named = body_str.replacen(
        "match String.charAtAv",
        "match hc_scan : String.charAtAv",
        1,
    );

    let mut lines = Vec::new();
    if let Some(desc) = &fd.desc {
        lines.push(format!("/-- {} -/", sanitize_doc(desc)));
    }
    lines.push(format!("def {} {} : {} :=", fn_name, params, ret_type));
    lines.push(body_named);
    lines.push(format!(
        "termination_by ({}.toList.length - {}.toNat)",
        s, pos
    ));
    lines.push("decreasing_by".to_string());
    lines.push("  all_goals (".to_string());
    lines.push(format!(
        "    obtain ⟨h0, hlt⟩ := String.charAt_some_bounds {} {} _ hc_scan",
        s, pos
    ));
    lines.push("    omega)".to_string());
    lines.join("\n")
}

pub(super) fn lower_pure_question_bang_for_emit(fd: &FnDef) -> Option<FnDef> {
    crate::types::checker::effect_lifting::lower_pure_question_bang_fn(fd)
        .ok()
        .flatten()
}

fn expr_uses_error_prop(expr: &Spanned<Expr>) -> bool {
    match &expr.node {
        Expr::ErrorProp(_) => true,
        Expr::FnCall(callee, args) => {
            expr_uses_error_prop(callee) || args.iter().any(expr_uses_error_prop)
        }
        Expr::Attr(obj, _) => expr_uses_error_prop(obj),
        Expr::BinOp(_, left, right) => expr_uses_error_prop(left) || expr_uses_error_prop(right),
        Expr::Neg(inner) => expr_uses_error_prop(inner),
        Expr::Match { subject, arms, .. } => {
            expr_uses_error_prop(subject) || arms.iter().any(|arm| expr_uses_error_prop(&arm.body))
        }
        Expr::Constructor(_, Some(inner)) => expr_uses_error_prop(inner),
        Expr::InterpolatedStr(parts) => parts.iter().any(|part| match part {
            StrPart::Parsed(expr) => expr_uses_error_prop(expr),
            StrPart::Literal(_) => false,
        }),
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            items.iter().any(expr_uses_error_prop)
        }
        Expr::MapLiteral(entries) => entries
            .iter()
            .any(|(key, value)| expr_uses_error_prop(key) || expr_uses_error_prop(value)),
        Expr::RecordCreate { fields, .. } => {
            fields.iter().any(|(_, value)| expr_uses_error_prop(value))
        }
        Expr::RecordUpdate { base, updates, .. } => {
            expr_uses_error_prop(base)
                || updates.iter().any(|(_, value)| expr_uses_error_prop(value))
        }
        Expr::TailCall(boxed) => boxed.args.iter().any(expr_uses_error_prop),
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } | Expr::Constructor(_, None) => {
            false
        }
    }
}

fn body_uses_error_prop(body: &FnBody) -> bool {
    body.stmts().iter().any(|stmt| match stmt {
        Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => expr_uses_error_prop(expr),
    })
}

/// Typed-HIR query: does this fn return `Result<_, _>`?
///
/// Epic #180 Phase 4 — reads the canonical type stamped on the
/// resolved fn def directly instead of re-parsing the AST return
/// type string. The typechecker has already produced the typed
/// surface; backends just consume it.
fn fn_returns_result_typed(rfd: &crate::ir::hir::ResolvedFnDef) -> bool {
    matches!(rfd.return_type, crate::types::Type::Result(_, _))
}

/// Emit one statement inside a Lean `do` block (used when the fn
/// body must thread `ErrorProp` through Lean's monadic chain).
///
/// **Epic #170 Phase 5 PR E2**: resolves each stmt once at the
/// boundary through `ctx.resolve_stmt` (scope-aware) and routes the
/// inner expression through `emit_expr` (resolved) instead of the
/// `temporary-migration-bridge` `emit_expr_legacy` adapter. Keeps the
/// fn-body emit path off the legacy resolve-on-demand surface in the
/// hot path; the remaining `emit_expr_legacy` callsites in this
/// module are all in proof-mode law/verify rewriters where the
/// upstream rewriter still produces raw AST.
fn emit_do_stmt(stmt: &Stmt, ctx: &CodegenContext, is_last: bool) -> String {
    use crate::ir::hir::ResolvedStmt;
    let scope = ctx.active_module_scope();
    let scope_ref = scope.as_deref();
    // Detect `ErrorProp(inner)` BEFORE resolve so we can route the
    // unwrapped inner through the monadic-bind / direct-emit branches
    // the same way the legacy path did.
    let (is_err_prop, target_for_resolve): (bool, std::borrow::Cow<'_, Spanned<Expr>>) = match stmt
    {
        Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => {
            if let Expr::ErrorProp(inner) = &expr.node {
                (true, std::borrow::Cow::Owned((**inner).clone()))
            } else {
                (false, std::borrow::Cow::Borrowed(expr))
            }
        }
    };
    let resolved_expr = ctx.resolve_expr(target_for_resolve.as_ref(), scope_ref);
    let expr_str = super::expr::emit_expr(&resolved_expr, ctx);
    match (stmt, is_err_prop, is_last) {
        (Stmt::Binding(name, _, _), true, _) => {
            format!("  let {} <- {}", aver_name_to_lean(name), expr_str)
        }
        (Stmt::Binding(name, _, _), false, _) => {
            // Re-resolve the full stmt so the inner expression keeps
            // its proper `ResolvedStmt::Binding` shape (preserves
            // the type annotation if it was present).
            let resolved_stmt = ctx.resolve_stmt(stmt, scope_ref);
            if let ResolvedStmt::Binding { name: n, value, .. } = &resolved_stmt {
                format!(
                    "  let {} := {}",
                    aver_name_to_lean(n),
                    super::expr::emit_expr(value, ctx)
                )
            } else {
                format!("  let {} := {}", aver_name_to_lean(name), expr_str)
            }
        }
        (Stmt::Expr(_), true, true) => format!("  {}", expr_str),
        (Stmt::Expr(_), true, false) => format!("  let _ <- {}", expr_str),
        (Stmt::Expr(_), false, true) => format!("  {}", expr_str),
        (Stmt::Expr(_), false, false) => format!("  let _ := {}", expr_str),
    }
}

/// Emit a Lean fn body (plain — no `do` notation).
///
/// **Epic #170 Phase 5 PR E2**: resolves each top-level stmt once at
/// the boundary instead of calling the legacy adapter per expression.
/// Same migration shape as [`emit_do_stmt`].
fn emit_fn_body(body: &FnBody, ctx: &CodegenContext) -> String {
    use crate::ir::hir::ResolvedStmt;
    let scope = ctx.active_module_scope();
    let scope_ref = scope.as_deref();
    let stmts = body.stmts();
    let mut lines = Vec::new();
    for (i, stmt) in stmts.iter().enumerate() {
        let is_last = i == stmts.len() - 1;
        let resolved_stmt = ctx.resolve_stmt(stmt, scope_ref);
        match &resolved_stmt {
            ResolvedStmt::Binding { name, value, .. } => {
                lines.push(format!(
                    "  let {} := {}",
                    aver_name_to_lean(name),
                    super::expr::emit_expr(value, ctx)
                ));
            }
            ResolvedStmt::Expr(expr) => {
                if is_last {
                    lines.push(format!("  {}", super::expr::emit_expr(expr, ctx)));
                } else {
                    lines.push(format!("  let _ := {}", super::expr::emit_expr(expr, ctx)));
                }
            }
        }
    }
    lines.join("\n")
}

fn emit_fn_body_result_do(body: &FnBody, ctx: &CodegenContext) -> String {
    let stmts = body.stmts();
    let mut lines = vec!["  do".to_string()];
    for (i, stmt) in stmts.iter().enumerate() {
        lines.push(emit_do_stmt(stmt, ctx, i == stmts.len() - 1));
    }
    lines.join("\n")
}

pub(super) fn emit_fn_body_for(fd: &FnDef, body: &FnBody, ctx: &CodegenContext) -> String {
    // Pointer-eq scope (`fn_id_for_decl`) → resolved view by `FnId`
    // so a same-bare-name entry/dep twin never accidentally
    // provides this fn's return type. Synthetic FnDefs (TCO hoists,
    // mid-rewrite fns) the resolver never saw fall through to
    // `ctx.resolve_fn_def`'s on-demand lift.
    let resolved_fd = crate::codegen::common::fn_id_for_decl(ctx, fd)
        .and_then(|id| ctx.resolved_program.fn_by_id(id));
    let resolved_owned = match resolved_fd {
        Some(_) => None,
        None => Some(ctx.resolve_fn_def(fd, None)),
    };
    let rfd: &crate::ir::hir::ResolvedFnDef =
        resolved_fd.unwrap_or_else(|| resolved_owned.as_ref().unwrap().as_ref());
    if fn_returns_result_typed(rfd) && body_uses_error_prop(body) {
        emit_fn_body_result_do(body, ctx)
    } else {
        emit_fn_body(body, ctx)
    }
}

/// Emit mutual recursion group wrapped in `mutual ... end`.
pub fn emit_mutual_group(fns: &[&FnDef], ctx: &CodegenContext) -> String {
    let mut lines = Vec::new();
    lines.push("mutual".to_string());
    for fd in fns {
        if !is_pure_fn(fd) {
            continue;
        }
        if let Some(desc) = &fd.desc {
            lines.push(format!("  /-- {} -/", sanitize_doc(desc)));
        }
        let fn_name = aver_name_to_lean(&fd.name);
        let params = emit_fn_params(&fd.params);
        let ret_type = if fd.return_type.is_empty() {
            "Unit".to_string()
        } else {
            type_annotation_to_lean(&fd.return_type)
        };
        // #17: All functions in mutual blocks need `partial` for termination
        lines.push(format!(
            "  partial def {} {} : {} :=",
            fn_name, params, ret_type
        ));
        let body = emit_fn_body_for(fd, &fd.body, ctx);
        // Indent body by 2 more spaces
        for line in body.lines() {
            lines.push(format!("  {}", line));
        }
        lines.push(String::new());
    }
    lines.push("end".to_string());
    lines.join("\n")
}

/// Proof-mode mutual recursion emission with optional group-level termination.
pub fn emit_mutual_group_proof(fns: &[&FnDef], ctx: &CodegenContext) -> String {
    // Distinguish mutual SCC shapes by the Lex params vector:
    //   `[p]` rank 0  → MutualIntCountdown
    //   `[s, pos]`    → MutualStringPosAdvance
    //   `[]` rank >=1 → MutualSizeOfRanked
    let all_int_countdown = fns.iter().all(|fd| {
        matches!(
            contract_lex_params_rank(ctx, fd),
            Some((params, 0)) if params.len() == 1
        )
    });
    if all_int_countdown {
        return emit_fuelized_mutual_int_countdown_group(fns, ctx);
    }

    let all_string_pos = fns.iter().all(|fd| {
        matches!(
            contract_lex_params_rank(ctx, fd),
            Some((params, _)) if params.len() == 2
        )
    });
    if all_string_pos {
        return emit_fuelized_mutual_string_pos_group(fns, ctx);
    }

    let all_sizeof = fns.iter().all(|fd| {
        matches!(
            contract_lex_params_rank(ctx, fd),
            Some((params, _)) if params.is_empty()
        )
    });
    if all_sizeof {
        if let Some(code) = emit_native_mutual_sizeof_group(fns, ctx) {
            return code;
        }
        // Termination-as-a-law: try a genuine well-founded mutual block whose
        // `decreasing_by` cites synthesised, kernel-proved length lemmas for
        // computed-list-arg recursion (quicksort's `sort`/`sortWithPivot`).
        // Backs off to fuel when the SCC isn't length-monotone-WF.
        if let Some(code) = emit_native_mutual_lex_list_wf_group(fns, ctx) {
            return code;
        }
        return emit_fuelized_mutual_sizeof_group(fns, ctx);
    }

    let mut lines = Vec::new();
    lines.push("mutual".to_string());
    for fd in fns {
        if !is_pure_fn(fd) {
            continue;
        }
        if let Some(desc) = &fd.desc {
            lines.push(format!("  /-- {} -/", sanitize_doc(desc)));
        }
        let fn_name = aver_name_to_lean(&fd.name);
        let params = emit_fn_params(&fd.params);
        let ret_type = if fd.return_type.is_empty() {
            "Unit".to_string()
        } else {
            type_annotation_to_lean(&fd.return_type)
        };
        lines.push(format!("  def {} {} : {} :=", fn_name, params, ret_type));
        let body = emit_fn_body_for(fd, &fd.body, ctx);
        for line in body.lines() {
            lines.push(format!("  {}", line));
        }
        match contract_lex_params_rank(ctx, fd) {
            Some((params, 0)) if params.len() == 1 => {
                // MutualIntCountdown — every member counts down the
                // shared first-Int param. (The IR's param name is
                // canonical; we don't fall back to fd.params here.)
                let lean_first = aver_name_to_lean(&params[0]);
                lines.push(format!("  termination_by Int.natAbs {}", lean_first));
                lines.push("  decreasing_by".to_string());
                lines.push("    omega".to_string());
            }
            Some((params, rank)) if params.len() == 2 => {
                // MutualStringPosAdvance — (s, pos) shape; rank
                // distinguishes SCC members.
                let lean_s = aver_name_to_lean(&params[0]);
                let lean_pos = aver_name_to_lean(&params[1]);
                lines.push(format!(
                    "  termination_by (({}.data.length) - ({}.toNat), {})",
                    lean_s, lean_pos, rank
                ));
                lines.push("  decreasing_by".to_string());
                lines.push("    simp_wf".to_string());
            }
            Some(([], _)) => {
                // MutualSizeOfRanked — handled inside the SCC's
                // dedicated emitter; no termination_by suffix here.
            }
            _ => {}
        }
        lines.push(String::new());
    }

    lines.push("end".to_string());
    lines.join("\n")
}

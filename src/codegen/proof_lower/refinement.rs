//! Refinement-via-opaque lift: carrier discovery, smart-constructor
//! guards, and inhabitation witnesses.
//!
//! Split from `proof_lower.rs` — see the module docs in [`super`].

use super::*;

/// Backend-neutral analogue of `codegen::common::refinement_lift_
/// for_given`. Walks `lhs` / `rhs` looking for a `RecordCreate {
/// type_name, fields: [(_, Ident(given))] }` shape where `type_
/// name` is a refined type whose carrier matches the given's
/// declared type. Returns the refined type name on first match.
///
/// The legacy version (common.rs) takes `&CodegenContext` and
/// borrows the type name from `ctx.items`. The lowerer reads
/// `refined_types` directly off the in-progress `ProofIR`
/// (populated by `populate_refined_types`, which runs before
/// `populate_law_theorems` in `lower(...)`).
pub(super) fn refinement_lift_for_given_ir(
    given_name: &str,
    lhs: &Spanned<crate::ast::Expr>,
    rhs: &Spanned<crate::ast::Expr>,
    refined_types: &std::collections::HashMap<crate::ir::TypeId, crate::ir::RefinedTypeDecl>,
    symbols: &crate::ir::SymbolTable,
    dep_modules: &[crate::codegen::ModuleInfo],
) -> Option<String> {
    let mut result: Option<String> = None;
    walk_for_refinement_carrier(
        lhs,
        given_name,
        refined_types,
        symbols,
        dep_modules,
        &mut result,
    );
    walk_for_refinement_carrier(
        rhs,
        given_name,
        refined_types,
        symbols,
        dep_modules,
        &mut result,
    );
    result
}

/// **syntax-discovery-only** (epic #170 Phase 7). Walks raw AST
/// looking for a `RecordCreate(type_name, [(field, Ident(given))])`
/// pattern that lifts a `given` through a refinement type's smart
/// constructor. The recursion descends into nested record-creates,
/// fn calls, and binops so a deeply-wrapped lift still gets found.
/// Identity is handed off to `resolve_refined_type_in_with_key`,
/// which canonicalises through `SymbolTable` before keying the
/// `refined_types` map — no bare-name keying past discovery.
pub(super) fn walk_for_refinement_carrier(
    expr: &Spanned<crate::ast::Expr>,
    given_name: &str,
    refined_types: &std::collections::HashMap<crate::ir::TypeId, crate::ir::RefinedTypeDecl>,
    symbols: &crate::ir::SymbolTable,
    dep_modules: &[crate::codegen::ModuleInfo],
    result: &mut Option<String>,
) {
    use crate::ast::Expr;
    if result.is_some() {
        return;
    }
    match &expr.node {
        Expr::RecordCreate { type_name, fields } if fields.len() == 1 => {
            let (_, fvalue) = &fields[0];
            let matches_var = matches!(
                &fvalue.node,
                Expr::Ident(n) | Expr::Resolved { name: n, .. } if n == given_name
            );
            if matches_var
                && let Some((type_id, _decl)) =
                    crate::codegen::common::resolve_refined_type_in_with_key(
                        refined_types,
                        symbols,
                        dep_modules,
                        type_name,
                    )
            {
                // Stringify the canonical name via the symbol table's
                // type entry. The only consumer today reads `.is_some()`
                // (see `detect_simp_omega_unfold`), but recovering a
                // human-readable id keeps the diagnostic path honest.
                *result = Some(symbols.type_entry(type_id).key.canonical());
                return;
            }
            // Even non-matching RecordCreate may contain nested
            // refinement carriers (e.g. `Foo(value = Bar(value = a))`).
            for (_, v) in fields {
                walk_for_refinement_carrier(
                    v,
                    given_name,
                    refined_types,
                    symbols,
                    dep_modules,
                    result,
                );
            }
        }
        Expr::FnCall(callee, args) => {
            walk_for_refinement_carrier(
                callee,
                given_name,
                refined_types,
                symbols,
                dep_modules,
                result,
            );
            for a in args {
                walk_for_refinement_carrier(
                    a,
                    given_name,
                    refined_types,
                    symbols,
                    dep_modules,
                    result,
                );
            }
        }
        Expr::BinOp(_, l, r) => {
            walk_for_refinement_carrier(l, given_name, refined_types, symbols, dep_modules, result);
            walk_for_refinement_carrier(r, given_name, refined_types, symbols, dep_modules, result);
        }
        Expr::Match { subject, arms, .. } => {
            walk_for_refinement_carrier(
                subject,
                given_name,
                refined_types,
                symbols,
                dep_modules,
                result,
            );
            for arm in arms {
                walk_for_refinement_carrier(
                    &arm.body,
                    given_name,
                    refined_types,
                    symbols,
                    dep_modules,
                    result,
                );
            }
        }
        Expr::Attr(obj, _) => {
            walk_for_refinement_carrier(
                obj,
                given_name,
                refined_types,
                symbols,
                dep_modules,
                result,
            );
        }
        _ => {}
    }
}

/// Find a single-param smart constructor in the unfold set whose
/// body is the canonical `match <bool-subj> { true → Ok; false →
/// Err }` shape. Returns the param name + bool subject of the
/// first match.
pub(super) fn extract_smart_constructor_guard(
    fn_names: &std::collections::BTreeSet<String>,
    inputs: &ProofLowerInputs,
) -> Option<crate::ir::SmartGuard> {
    use crate::ast::{Expr, MatchArm, Pattern, Stmt};
    for fd in iter_all_fn_defs(inputs) {
        if !fn_names.contains(&fd.name) {
            continue;
        }
        if !fd.return_type.starts_with("Result<") {
            continue;
        }
        if fd.params.len() != 1 {
            continue;
        }
        let (param_name, param_type) = &fd.params[0];
        if param_type != "Int" {
            continue;
        }
        let stmts = fd.body.stmts();
        if stmts.len() != 1 {
            continue;
        }
        let Stmt::Expr(body_expr) = &stmts[0] else {
            continue;
        };
        let Expr::Match { subject, arms } = &body_expr.node else {
            continue;
        };
        if !arms_match_bool_ok_err(arms) {
            continue;
        }
        let scope = inputs.fn_owning_scope(fd);
        return Some(crate::ir::SmartGuard {
            param: param_name.clone(),
            predicate: inputs.resolve_expr(subject, scope),
        });
        // Reference the type to satisfy the MatchArm import.
        #[allow(unreachable_code)]
        {
            let _: Option<&MatchArm> = None;
            let _: Option<&Pattern> = None;
        }
    }
    None
}

pub(super) fn arms_match_bool_ok_err(arms: &[crate::ast::MatchArm]) -> bool {
    use crate::ast::{Expr, Literal, Pattern};
    if arms.len() != 2 {
        return false;
    }
    let starts_with_ctor = |expr: &Spanned<Expr>, name: &str| -> bool {
        match &expr.node {
            Expr::Constructor(n, _) => n == name,
            Expr::FnCall(callee, _) => {
                if let Expr::Attr(obj, field) = &callee.node
                    && let Expr::Ident(ns) = &obj.node
                {
                    format!("{ns}.{field}") == name
                } else {
                    false
                }
            }
            _ => false,
        }
    };
    let mut saw_true_ok = false;
    let mut saw_false_err = false;
    for arm in arms {
        match &arm.pattern {
            Pattern::Literal(Literal::Bool(true)) => {
                if starts_with_ctor(&arm.body, "Result.Ok") {
                    saw_true_ok = true;
                }
            }
            Pattern::Literal(Literal::Bool(false)) => {
                if starts_with_ctor(&arm.body, "Result.Err") {
                    saw_false_err = true;
                }
            }
            _ => return false,
        }
    }
    saw_true_ok && saw_false_err
}

/// Pick an inhabitation witness: a literal value of the carrier type
/// that satisfies the refinement predicate. Backend-neutral output —
/// Dafny consumes it as `witness <W>`, Lean may later use it for a
/// `sample_X` helper. First tries the smart constructor's verify-
/// block samples (entry-module only — `ModuleInfo` doesn't surface
/// verify blocks); falls back to evaluating the predicate against
/// `[0, 1, -1]` and returning the first satisfier.
pub(super) fn pick_witness(
    type_name: &str,
    type_id: crate::ir::TypeId,
    inputs: &ProofLowerInputs,
    predicate: &Spanned<Expr>,
    param_name: &str,
    scope: Option<&str>,
) -> Option<String> {
    // Smart-constructor + verify-block walk, scoped to the same
    // module the type lives in. Refinement-via-opaque keeps record
    // and constructor in the same module (the carrier field is
    // opaque from outside), so this mirrors that constraint. Without
    // the scope a module-B `Natural` with predicate `n >= 10` would
    // pick up entry's `fromInt(0)` verify case, silently breaking
    // the witness invariant.
    let smart_ctor_name: Option<String> = match scope {
        None => inputs.entry_items.iter().find_map(|item| match item {
            TopLevel::FnDef(fd)
                if smart_ctor_matches(fd, type_id, type_name, inputs.symbol_table, scope) =>
            {
                Some(fd.name.clone())
            }
            _ => None,
        }),
        Some(prefix) => inputs
            .dep_modules
            .iter()
            .find(|m| m.prefix == prefix)
            .and_then(|m| {
                m.fn_defs
                    .iter()
                    .find(|fd| {
                        smart_ctor_matches(fd, type_id, type_name, inputs.symbol_table, scope)
                    })
                    .map(|fd| fd.name.clone())
            }),
    };
    if let Some(smart_ctor_name) = smart_ctor_name {
        // Verify blocks live on `inputs.entry_items` only — `ModuleInfo`
        // doesn't surface verify cases. Scoped verify-block walk would
        // need a separate plumb that's not in `ProofLowerInputs` today.
        // Restrict the verify-sample fast path to entry scope; module
        // scopes fall through to the predicate-evaluation fallback,
        // which now sweeps a wider range so non-trivial predicates
        // (`n >= 10`) still get a real witness.
        if scope.is_none() {
            for item in inputs.entry_items {
                let TopLevel::Verify(vb) = item else {
                    continue;
                };
                if vb.fn_name != smart_ctor_name {
                    continue;
                }
                for (lhs, rhs) in &vb.cases {
                    if !is_result_ok(&rhs.node) {
                        continue;
                    }
                    let Expr::FnCall(_, args) = &lhs.node else {
                        continue;
                    };
                    if args.len() != 1 {
                        continue;
                    }
                    if let Some(lit) = literal_int_value(&args[0]) {
                        return Some(lit);
                    }
                }
            }
        }
    }
    // Predicate-evaluation. Two-stage: first sweep candidates
    // *extracted from the predicate AST itself* (any `Literal::Int`
    // reachable through `BinOp` / `FnCall` chains, plus the
    // neighbours `K`, `K-1`, `K+1` so `n == 5` / `n > 5` / `n < 5`
    // all land their witness). Then fall back to a fixed magnitude
    // sweep for shapes that don't mention concrete numbers
    // (`n != 0`, `Bool.and(n >= 0, n <= max())`, …). Returning
    // `None` here is intentional and `populate_refined_types`
    // skips the slot — the alternative was Dafny silently emitting
    // `witness 0` against a predicate the witness didn't satisfy.
    let mut tried = std::collections::HashSet::<i64>::new();
    let mut candidates: Vec<i64> = Vec::new();
    let mut from_ast: Vec<i64> = Vec::new();
    collect_int_literals(predicate, &mut from_ast);
    for k in from_ast {
        for delta in &[0_i64, 1, -1] {
            if let Some(c) = k.checked_add(*delta) {
                candidates.push(c);
            }
        }
    }
    candidates.extend_from_slice(&[
        0, 1, -1, 2, -2, 10, -10, 100, 1_000, 10_000, 100_000, 1_000_000,
    ]);
    for candidate in candidates {
        if !tried.insert(candidate) {
            continue;
        }
        if eval_int_bool_predicate(predicate, param_name, candidate) == Some(true) {
            return Some(candidate.to_string());
        }
    }
    None
}

pub(super) fn collect_int_literals(expr: &Spanned<Expr>, out: &mut Vec<i64>) {
    match &expr.node {
        Expr::Literal(Literal::Int(n)) => out.push(*n),
        Expr::Neg(inner) => {
            if let Expr::Literal(Literal::Int(n)) = &inner.node {
                out.push(-n);
            } else {
                collect_int_literals(inner, out);
            }
        }
        Expr::BinOp(_, l, r) => {
            collect_int_literals(l, out);
            collect_int_literals(r, out);
        }
        Expr::FnCall(callee, args) => {
            collect_int_literals(callee, out);
            for a in args {
                collect_int_literals(a, out);
            }
        }
        Expr::Match { subject, arms } => {
            collect_int_literals(subject, out);
            for arm in arms {
                collect_int_literals(&arm.body, out);
            }
        }
        Expr::Attr(o, _) | Expr::ErrorProp(o) => collect_int_literals(o, out),
        _ => {}
    }
}

/// Does `fd` look like a smart constructor for `type_id` (1-param
/// fn whose return type is `Result<T, _>` where `T` is the
/// refined nominal)?
///
/// Epic #180 Phase 6 follow-up — id-aware comparison. The raw
/// `name`-equality path used to silently accept a smart ctor
/// from module B whose return type was `Result<A.Natural, _>`
/// when looking for module B's own `Natural`, because the
/// inner Named's bare `name` field was just `"Natural"` and
/// both shapes match on string. Resolving the inner Named
/// through the symbol table within the current scope hands us
/// the actual `TypeId` and lets us compare opaque identities.
///
/// `type_name` stays in the signature as the fallback for
/// builtins / unresolved refs the symbol table doesn't index
/// (matches the [`crate::codegen::common::backend_named_type_key`]
/// fallback semantics).
pub(super) fn smart_ctor_matches(
    fd: &FnDef,
    type_id: crate::ir::TypeId,
    type_name: &str,
    symbols: &crate::ir::SymbolTable,
    scope: Option<&str>,
) -> bool {
    if fd.params.len() != 1 {
        return false;
    }
    let parsed = crate::types::parse_type_str(&fd.return_type);
    let crate::types::Type::Result(ok, _) = parsed else {
        return false;
    };
    // syntax-discovery-only: pulls the inner Named's `name` out
    // of the raw `parse_type_str` parse to feed into the
    // symbol-table resolution below. The identity decision is
    // the `TypeId` comparison built from that name plus scope;
    // the bare `name` is just an intermediate handle.
    let crate::types::Type::Named { name: n, .. } = &*ok else {
        return false;
    };
    // Prefer id-direct: resolve the inner Named's name against
    // the symbol table within the current scope, then compare
    // `TypeId`s. Identity-safe across cross-module same-bare-name
    // twins by construction.
    let name_is_qualified = n.contains('.');
    let resolved_id = if name_is_qualified {
        n.rsplit_once('.').and_then(|(prefix, bare)| {
            symbols.type_id_of(&crate::ir::TypeKey::in_module(prefix.to_string(), bare))
        })
    } else if let Some(prefix) = scope {
        symbols
            .type_id_of(&crate::ir::TypeKey::in_module(
                prefix.to_string(),
                n.clone(),
            ))
            .or_else(|| symbols.type_id_of(&crate::ir::TypeKey::entry(n.clone())))
    } else {
        symbols.type_id_of(&crate::ir::TypeKey::entry(n.clone()))
    };
    match resolved_id {
        Some(id) => id == type_id,
        None => n == type_name,
    }
}

pub(super) fn is_result_ok(expr: &Expr) -> bool {
    match expr {
        Expr::Constructor(name, _) => name == "Result.Ok",
        Expr::FnCall(callee, _) => matches!(
            &callee.node,
            Expr::Attr(obj, field)
                if field == "Ok" && matches!(&obj.node, Expr::Ident(n) if n == "Result")
        ),
        _ => false,
    }
}

pub(super) fn literal_int_value(expr: &Spanned<Expr>) -> Option<String> {
    match &expr.node {
        Expr::Literal(Literal::Int(n)) => Some(n.to_string()),
        Expr::Neg(inner) => {
            let inner_str = literal_int_value(inner)?;
            Some(format!("-{inner_str}"))
        }
        _ => None,
    }
}

pub(super) fn eval_int_bool_predicate(
    expr: &Spanned<Expr>,
    param_name: &str,
    value: i64,
) -> Option<bool> {
    match &expr.node {
        Expr::Literal(Literal::Bool(b)) => Some(*b),
        Expr::BinOp(op, l, r) => {
            use crate::ast::BinOp::*;
            let li = eval_int_arith(l, param_name, value)?;
            let ri = eval_int_arith(r, param_name, value)?;
            Some(match op {
                Lt => li < ri,
                Gt => li > ri,
                Lte => li <= ri,
                Gte => li >= ri,
                Eq => li == ri,
                Neq => li != ri,
                _ => return None,
            })
        }
        Expr::FnCall(callee, args) if args.len() == 2 => {
            let name = expr_to_dotted_name(&callee.node)?;
            match name.as_str() {
                "Bool.and" => Some(
                    eval_int_bool_predicate(&args[0], param_name, value)?
                        && eval_int_bool_predicate(&args[1], param_name, value)?,
                ),
                "Bool.or" => Some(
                    eval_int_bool_predicate(&args[0], param_name, value)?
                        || eval_int_bool_predicate(&args[1], param_name, value)?,
                ),
                _ => None,
            }
        }
        _ => None,
    }
}

pub(super) fn eval_int_arith(expr: &Spanned<Expr>, param_name: &str, value: i64) -> Option<i64> {
    match &expr.node {
        Expr::Literal(Literal::Int(n)) => Some(*n),
        Expr::Ident(name) | Expr::Resolved { name, .. } if name == param_name => Some(value),
        Expr::BinOp(op, l, r) => {
            use crate::ast::BinOp::*;
            let li = eval_int_arith(l, param_name, value)?;
            let ri = eval_int_arith(r, param_name, value)?;
            match op {
                Add => Some(li.checked_add(ri)?),
                Sub => Some(li.checked_sub(ri)?),
                Mul => Some(li.checked_mul(ri)?),
                Div => Some(li.checked_div(ri)?),
                _ => None,
            }
        }
        Expr::Neg(inner) => Some(-eval_int_arith(inner, param_name, value)?),
        _ => None,
    }
}

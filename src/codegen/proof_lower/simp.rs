//! `SimpOverPreludeLemmas` / simp+omega-unfold strategy detectors.
//!
//! Split from `proof_lower.rs` — see the module docs in [`super`].

use super::*;

/// Builtin-roundtrip detector for [`ProofStrategy::SimpOverPreludeLemmas`]
/// (the last typed fallback before `BackendDispatch`, after
/// `detect_finite_domain_cases`). Fires for a no-when law with givens
/// whose lhs calls resolve to user fns that are each either:
/// - NON-recursive — these seed the transitive unfold cone (expanded
///   through non-recursive bodies only), or
/// - recursive AND classified (a `FnContract` exists — never a
///   `partial def`, whose missing equation lemmas would make `simp
///   [fn]` a hard build error) AND called with measure-closed args at
///   every lhs call site: each arg is a literal or a constructor whose
///   payload is literals/idents over scalar-only variant fields, so
///   every fuel-metric family (`natAbs`, length, `sizeOf`, string-pos)
///   computes to a `Nat` literal and simp drives the `__fuel`
///   equations through (empirically `toString' (Json.jsonInt n) =
///   String.fromInt n` is plain `rfl` despite fuel).
///
/// Recursive fns reached only TRANSITIVELY (inside cone bodies) do
/// not block and are left out of the unfold set — under the law's
/// pinned literal args their branches are usually dead (simp computes
/// the dispatching match away, e.g. `afterIntChar … "]"` never reaches
/// `scanFracFirst`); a live one just stops simp, which the emit's
/// `first | (simp …; done) | sorry` alternation catches as an honest
/// `sorry`, never a build error.
///
/// The builtin call names observed across the law sides + every cone
/// body (recursive ones included — `String.fromInt` lives inside
/// `toString`'s body) become the registry keys the Lean emitter maps
/// to prelude spec lemma names (`lean::prelude_spec_lemmas_for_builtins`,
/// single source of truth next to the lemmas themselves). String `+`
/// is recorded as the synthetic `String.concat` marker so the
/// `HAdd`-normalising `String.add_eq_append` fires for laws that
/// concatenate without naming any `String.*` builtin.
pub(super) fn detect_simp_over_prelude_lemmas(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
    fn_contracts: &std::collections::HashMap<crate::ir::FnId, crate::ir::FnContract>,
) -> Option<crate::ir::ProofStrategy> {
    use std::collections::BTreeSet;

    if law.givens.is_empty() {
        return None;
    }
    let resolve_user_fn = |name: &str| -> Option<&FnDef> {
        let fd = inputs.find_fn_def_by_call_name(name)?;
        if !fd.effects.is_empty() || fd.name == "main" {
            return None;
        }
        Some(fd)
    };
    // The law must actually exercise its subject fn in the lhs — the
    // unfold list is anchored on it (subject first, mirroring
    // `detect_enum_constant_fold`'s ordering contract).
    resolve_user_fn(fn_name)?;
    let recursive = inputs.recursive_pure_fn_names();

    // Seed from direct lhs calls only (the rhs of roundtrip laws is a
    // constructor expectation; a user fn there would also show up in
    // the lhs cone for every shape this detector targets).
    let mut lhs_calls: BTreeSet<String> = BTreeSet::new();
    collect_fn_calls_expr(&law.lhs, &mut lhs_calls);
    let mut cone: BTreeSet<String> = BTreeSet::new();
    let mut fuel_fns: BTreeSet<String> = BTreeSet::new();
    for name in &lhs_calls {
        let Some(fd) = resolve_user_fn(name) else {
            continue;
        };
        if !recursive.contains(&fd.name) {
            cone.insert(fd.name.clone());
            continue;
        }
        // Recursive direct seed: classified + measure-closed or bust.
        // **syntax-discovery-only** (epic #170 Phase 8 guardrail):
        // `fn_owning_scope` resolves the owning dep module via
        // pointer-eq; its `None` IS the entry scope by construction,
        // so the entry key is the correct typed identity here (same
        // shape as `ProofLowerInputs::recursive_pure_fn_names`).
        let fn_key = match inputs.fn_owning_scope(fd) {
            Some(prefix) => crate::ir::FnKey::in_module(prefix.to_string(), &fd.name),
            None => crate::ir::FnKey::entry(&fd.name),
        };
        let classified = inputs
            .symbol_table
            .fn_id_of(&fn_key)
            .is_some_and(|id| fn_contracts.contains_key(&id));
        if !classified || !calls_have_measure_closed_args(&law.lhs, &fd.name, inputs) {
            return None;
        }
        fuel_fns.insert(fd.name.clone());
    }
    if !cone.contains(fn_name) && !fuel_fns.contains(fn_name) {
        return None;
    }

    // Transitive cone expansion through NON-recursive bodies only.
    loop {
        let before = cone.len();
        let snapshot: Vec<String> = cone.iter().cloned().collect();
        for name in snapshot {
            let Some(fd) = resolve_user_fn(&name) else {
                continue;
            };
            let mut called: BTreeSet<String> = BTreeSet::new();
            for stmt in fd.body.stmts() {
                match stmt {
                    crate::ast::Stmt::Binding(_, _, e) | crate::ast::Stmt::Expr(e) => {
                        collect_fn_calls_expr(e, &mut called);
                    }
                }
            }
            for c in called {
                if let Some(callee) = resolve_user_fn(&c)
                    && !recursive.contains(&callee.name)
                {
                    cone.insert(callee.name.clone());
                }
            }
        }
        if cone.len() == before {
            break;
        }
    }

    // Registry keys: builtin calls across law sides + every cone /
    // fuel fn body (fuel bodies carry the partner builtins —
    // `String.fromInt` lives inside `toString`).
    let mut builtins: BTreeSet<String> = BTreeSet::new();
    collect_builtin_calls_expr(&law.lhs, &mut builtins);
    collect_builtin_calls_expr(&law.rhs, &mut builtins);
    for name in cone.iter().chain(fuel_fns.iter()) {
        if let Some(fd) = resolve_user_fn(name) {
            for stmt in fd.body.stmts() {
                match stmt {
                    crate::ast::Stmt::Binding(_, _, e) | crate::ast::Stmt::Expr(e) => {
                        collect_builtin_calls_expr(e, &mut builtins);
                    }
                }
            }
        }
    }

    // Subject first (Lean peels the outermost call layer first), then
    // the sorted rest — mirrors `detect_enum_constant_fold`.
    let mut unfold_fns: Vec<String> = Vec::new();
    if cone.contains(fn_name) {
        unfold_fns.push(fn_name.to_string());
    }
    unfold_fns.extend(cone.iter().filter(|n| *n != fn_name).cloned());
    Some(crate::ir::ProofStrategy::SimpOverPreludeLemmas {
        unfold_fns,
        fuel_fns: fuel_fns.into_iter().collect(),
        builtins: builtins.into_iter().collect(),
    })
}

/// True when every call to `target` inside `expr` passes only
/// measure-closed args — see [`expr_is_measure_closed`]. Used by the
/// `SimpOverPreludeLemmas` detector to admit a recursive (fuel-
/// emitted) fn as a direct lhs seed: closed args mean the fuel value
/// computes to a `Nat` literal, so simp can drive the `__fuel`
/// equations regardless of which fuel-metric family the classifier
/// picked.
pub(super) fn calls_have_measure_closed_args(
    expr: &Spanned<crate::ast::Expr>,
    target: &str,
    inputs: &ProofLowerInputs,
) -> bool {
    use crate::ast::Expr;
    let self_ok = match &expr.node {
        Expr::FnCall(callee, args) => {
            let is_target = expr_to_dotted_name(&callee.node)
                .is_some_and(|n| n.rsplit('.').next().unwrap_or(&n) == target);
            !is_target || args.iter().all(|a| expr_is_measure_closed(a, inputs))
        }
        _ => true,
    };
    if !self_ok {
        return false;
    }
    match &expr.node {
        Expr::FnCall(callee, args) => {
            calls_have_measure_closed_args(callee, target, inputs)
                && args
                    .iter()
                    .all(|a| calls_have_measure_closed_args(a, target, inputs))
        }
        Expr::BinOp(_, l, r) => {
            calls_have_measure_closed_args(l, target, inputs)
                && calls_have_measure_closed_args(r, target, inputs)
        }
        Expr::Neg(inner) | Expr::ErrorProp(inner) => {
            calls_have_measure_closed_args(inner, target, inputs)
        }
        Expr::Attr(obj, _) => calls_have_measure_closed_args(obj, target, inputs),
        Expr::Constructor(_, Some(inner)) => calls_have_measure_closed_args(inner, target, inputs),
        Expr::List(elems) | Expr::Tuple(elems) => elems
            .iter()
            .all(|e| calls_have_measure_closed_args(e, target, inputs)),
        _ => true,
    }
}

/// Measure-closedness of a call arg fed to a recursive (fuel-emitted)
/// fn: every fuel-metric family (`natAbs n`, list/string length,
/// `sizeOf`/`averMeasure*`) reduces to a `Nat` literal when the arg is
/// - a literal (possibly negated), or
/// - a constructor of a user `Sum` type whose matched variant has only
///   scalar fields (`Int`/`Float`/`String`/`Bool`) and whose payload
///   exprs are literals or idents — the ADT measure is constant on
///   such a constructor regardless of the payload values (e.g.
///   `averMeasureJson (Json.jsonInt n) = 2` for free `n`).
///
/// A bare ident is OPEN (a free `String` given feeds `s.length`-shaped
/// fuel; a free `Int` feeds `natAbs`), as is anything else.
pub(super) fn expr_is_measure_closed(
    expr: &Spanned<crate::ast::Expr>,
    inputs: &ProofLowerInputs,
) -> bool {
    use crate::ast::Expr;
    let payload_atom = |e: &Spanned<Expr>| -> bool {
        matches!(
            e.node,
            Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. }
        ) || matches!(&e.node, Expr::Neg(inner) if matches!(inner.node, Expr::Literal(_)))
    };
    let variant_fields_scalar = |ctor_name: &str| -> bool {
        let (type_name, variant_name) = match ctor_name.rsplit_once('.') {
            Some(pair) => pair,
            None => return false,
        };
        let Some(crate::ast::TypeDef::Sum { variants, .. }) = inputs.find_type_def(type_name)
        else {
            return false;
        };
        variants.iter().any(|v| {
            v.name == variant_name
                && v.fields
                    .iter()
                    .all(|f| matches!(f.trim(), "Int" | "Float" | "String" | "Bool"))
        })
    };
    match &expr.node {
        Expr::Literal(_) => true,
        Expr::Neg(inner) => matches!(inner.node, Expr::Literal(_)),
        Expr::Constructor(name, payload) => {
            let payload_ok = match payload.as_deref() {
                None => true,
                Some(Spanned {
                    node: Expr::Tuple(items),
                    ..
                }) => items.iter().all(payload_atom),
                Some(single) => payload_atom(single),
            };
            payload_ok && variant_fields_scalar(name)
        }
        // `Type.Variant(args…)` parses as a FnCall on a dotted
        // uppercase-leaf name; `Type.Variant` (fieldless) as an Attr.
        Expr::FnCall(callee, args) => expr_to_dotted_name(&callee.node).is_some_and(|n| {
            n.rsplit('.')
                .next()
                .is_some_and(|leaf| leaf.chars().next().is_some_and(|c| c.is_uppercase()))
                && variant_fields_scalar(&n)
                && args.iter().all(payload_atom)
        }),
        Expr::Attr(obj, field) => {
            let head = match &obj.node {
                Expr::Ident(n) => n.clone(),
                _ => return false,
            };
            field.chars().next().is_some_and(|c| c.is_uppercase())
                && variant_fields_scalar(&format!("{head}.{field}"))
        }
        _ => false,
    }
}

/// Collect builtin (`Namespace.method`) call names — the complement of
/// [`collect_fn_calls_expr`], which deliberately FILTERS the builtin
/// scalar namespaces out of the user-fn unfold set. Records every
/// dotted callee whose head segment starts uppercase and whose leaf is
/// lowercase (`String.slice`, `Int.fromString`, `List.concat`, …) plus
/// the synthetic `String.concat` marker for `+` over a string operand
/// (literal or interpolation — the type-blind AST signal for the
/// custom `HAdd String` instance). Drives the registry keys of
/// [`ProofStrategy::SimpOverPreludeLemmas`].
pub(super) fn collect_builtin_calls_expr(
    expr: &Spanned<crate::ast::Expr>,
    out: &mut std::collections::BTreeSet<String>,
) {
    use crate::ast::Expr;
    match &expr.node {
        Expr::FnCall(f, args) => {
            if let Some(name) = expr_to_dotted_name(&f.node)
                && name.contains('.')
            {
                let head = name.split('.').next().unwrap_or(&name);
                let leaf = name.rsplit('.').next().unwrap_or(&name);
                if head.chars().next().is_some_and(|c| c.is_uppercase())
                    && leaf.chars().next().is_some_and(|c| c.is_lowercase())
                {
                    out.insert(name);
                }
            }
            collect_builtin_calls_expr(f, out);
            for arg in args {
                collect_builtin_calls_expr(arg, out);
            }
        }
        Expr::BinOp(op, l, r) => {
            let is_stringy = |e: &Spanned<Expr>| {
                matches!(
                    &e.node,
                    Expr::Literal(crate::ast::Literal::Str(_)) | Expr::InterpolatedStr(_)
                )
            };
            if matches!(op, crate::ast::BinOp::Add) && (is_stringy(l) || is_stringy(r)) {
                out.insert("String.concat".to_string());
            }
            collect_builtin_calls_expr(l, out);
            collect_builtin_calls_expr(r, out);
        }
        Expr::Attr(obj, _) => collect_builtin_calls_expr(obj, out),
        Expr::Match { subject, arms, .. } => {
            collect_builtin_calls_expr(subject, out);
            for arm in arms {
                collect_builtin_calls_expr(&arm.body, out);
            }
        }
        Expr::TailCall(boxed) => {
            for arg in &boxed.args {
                collect_builtin_calls_expr(arg, out);
            }
        }
        Expr::ErrorProp(inner) | Expr::Neg(inner) => collect_builtin_calls_expr(inner, out),
        Expr::Constructor(_, Some(arg)) => collect_builtin_calls_expr(arg, out),
        Expr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                collect_builtin_calls_expr(e, out);
            }
        }
        Expr::List(elems) | Expr::Tuple(elems) | Expr::IndependentProduct(elems, _) => {
            for e in elems {
                collect_builtin_calls_expr(e, out);
            }
        }
        _ => {}
    }
}

/// Internal scratch for the simp+omega detector. Carries the
/// same fields as the IR variant but lives outside the IR enum so
/// callers can build it incrementally before pinning.
pub(super) struct SimpOmegaPlan {
    pub(super) unfold_fns: Vec<String>,
    pub(super) wrapper_return: bool,
    pub(super) smart_guard: Option<crate::ir::SmartGuard>,
    /// `true` when at least one law given is used as a refinement
    /// carrier in the law body (e.g. `given a: Int` used as
    /// `Natural(value = a)`). Subtype/subset lift carries the
    /// invariant in the type, so wrapper case-split is unnecessary.
    pub(super) lifted: bool,
}

pub(super) fn detect_simp_omega_unfold(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
    refined_types: &std::collections::HashMap<crate::ir::TypeId, crate::ir::RefinedTypeDecl>,
) -> Option<SimpOmegaPlan> {
    use std::collections::BTreeSet;

    let outer_fd = inputs.find_fn_def_by_call_name(fn_name)?;
    // All law givens Int.
    if law.givens.is_empty() || law.givens.iter().any(|g| g.type_name != "Int") {
        return None;
    }
    // Detect refinement lifts — when any given is used as a
    // `Refined(value = given)` carrier in the law body, the outer
    // fn may legitimately take the refined type (`fn add(a:
    // Natural, b: Natural)`) and unfold through the smart
    // constructor to Int arithmetic. Skip the outer-Int rejection
    // for lifted laws.
    let symbols = inputs.symbol_table;
    let lifted = law.givens.iter().any(|g| {
        refinement_lift_for_given_ir(
            &g.name,
            &law.lhs,
            &law.rhs,
            refined_types,
            symbols,
            inputs.dep_modules,
        )
        .is_some()
    });
    if !lifted && outer_fd.params.iter().any(|(_, t)| t != "Int") {
        return None;
    }

    // Seed the unfold set from the law's two sides + the outer fn.
    let mut fn_names: BTreeSet<String> = BTreeSet::new();
    collect_fn_calls_expr(&law.lhs, &mut fn_names);
    collect_fn_calls_expr(&law.rhs, &mut fn_names);
    fn_names.insert(fn_name.to_string());

    // Transitive expansion through entry+dep fn bodies. Each round
    // can only add fns reachable from the new set; converges in
    // O(items). Without this, cross-module refinement smart
    // constructors (`Modules.Natural.Natural.fromInt`) wouldn't be
    // in the unfold list and the goal would carry opaque
    // match-on-Result branches simp/omega can't close.
    loop {
        let before = fn_names.len();
        let snapshot: Vec<String> = fn_names.iter().cloned().collect();
        for fd in iter_all_fn_defs(inputs) {
            if !snapshot.contains(&fd.name) {
                continue;
            }
            for stmt in fd.body.stmts() {
                match stmt {
                    crate::ast::Stmt::Binding(_, _, e) | crate::ast::Stmt::Expr(e) => {
                        collect_fn_calls_expr(e, &mut fn_names);
                    }
                }
            }
        }
        if fn_names.len() == before {
            break;
        }
    }

    // Builtin-namespace wall (tests/fixtures/result_default_cone.av):
    // calls like `Result.withDefault` collect exactly like cross-module
    // user calls (uppercase head, lowercase leaf) but resolve to NO
    // user def — the rendered `simp only [...]` would cite a Lean
    // constant that does not exist in the emitted file
    // (`Result.withDefault` is not the prelude's `Except.withDefault`),
    // an `unknown identifier` BUILD ERROR that takes the whole file's
    // caught-sorry floor down with it. And even with the name mapped,
    // the lowered builtin shape (zero-guarded `Except`/`Option`
    // matches) sits outside omega's theory, so the tactic still fails.
    // Decline; the law degrades to the honest fallback paths (the
    // prelude-simp `first | simp | sorry` rung or the sampled sorry).
    if fn_names
        .iter()
        .any(|n| inputs.find_fn_def_by_call_name(n).is_none())
    {
        return None;
    }

    // Self-recursion rejection — `unfold fn` only does one step, so
    // a recursive body leaves a stale `fn` in the goal that simp
    // can't close. Check against the narrow self-only set; calling
    // a peer fn in the unfold list is fine.
    let mut wrapper_return = false;
    for fd in iter_all_fn_defs(inputs) {
        if !fn_names.contains(&fd.name) {
            continue;
        }
        let mut self_only: BTreeSet<String> = BTreeSet::new();
        self_only.insert(fd.name.clone());
        if body_calls_any_of_inputs(&fd.body, &self_only) {
            return None;
        }
        // Int-only check for the outer law fn — but skip when the
        // law is refinement-lifted (outer fn takes the refined
        // type, body unfolds through the smart constructor).
        if fd.name == fn_name && !lifted && fd.params.iter().any(|(_, t)| t != "Int") {
            return None;
        }
        let ret = fd.return_type.as_str();
        if ret != "Int" && ret != "Float" {
            wrapper_return = true;
        }
    }

    // Nonlinear-arithmetic wall (tests/fixtures/nr_wall.av): a product
    // of two non-constant operands anywhere in the law sides, the `when`
    // premise, or an unfold-cone body makes the rendered tactic a
    // guaranteed BUILD ERROR instead of a caught sorry — `omega` has
    // no nonlinear theory (it atomizes `x * y` and fails the goal),
    // and the sign-split `by_cases <;> simp` wrapper chain leaves
    // `0 ≤ x * x`-shaped goals unsolved. Decline so the law falls
    // through to the honest paths (the prelude-simp
    // `first | simp | sorry` rung or the bare sampled sorry).
    // Scope guards, each load-bearing:
    // - `literal * var` stays linear and IN scope (see
    //   `expr_has_var_product`) — nothing currently closing regresses;
    // - refinement-lifted laws keep the pin: their arm renders plain
    //   `simp [Int.add_comm, Int.mul_comm]` (no omega), which closes
    //   the commutativity-shaped product laws the refinement corpus
    //   pins (e.g. Natural.mul commutative);
    // - `when` + wrapper-return laws keep the pin: the Lean emitter
    //   itself declines that combination (`law_auto` — the sign-split
    //   chain cannot consume a `when` premise), routing to the
    //   guarded-domain fallback, so the pin is never rendered as a
    //   failing tactic.
    if !(lifted || (wrapper_return && law.when.is_some())) {
        let mut nonlinear = expr_has_var_product(&law.lhs) || expr_has_var_product(&law.rhs);
        if let Some(when_expr) = &law.when {
            nonlinear = nonlinear || expr_has_var_product(when_expr);
        }
        if !nonlinear {
            for fd in iter_all_fn_defs(inputs) {
                if !fn_names.contains(&fd.name) {
                    continue;
                }
                let body_nonlinear = fd.body.stmts().iter().any(|stmt| match stmt {
                    crate::ast::Stmt::Binding(_, _, e) | crate::ast::Stmt::Expr(e) => {
                        expr_has_var_product(e)
                    }
                });
                if body_nonlinear {
                    nonlinear = true;
                    break;
                }
            }
        }
        if nonlinear {
            return None;
        }
    }

    // Top-level law fn first in the unfold list — Lean needs to see
    // it in the goal before transitively-reached callees, otherwise
    // `unfold` fails outright at the outermost call layer.
    let mut ordered: Vec<String> = Vec::new();
    if fn_names.contains(fn_name) {
        ordered.push(fn_name.to_string());
    }
    for n in &fn_names {
        if n != fn_name {
            ordered.push(n.clone());
        }
    }

    let smart_guard = extract_smart_constructor_guard(&fn_names, inputs);

    Some(SimpOmegaPlan {
        unfold_fns: ordered,
        wrapper_return,
        smart_guard,
        lifted,
    })
}

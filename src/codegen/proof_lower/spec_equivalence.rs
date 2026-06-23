//! Impl-vs-spec functional-equivalence strategy detectors.
//!
//! Split from `proof_lower.rs` — see the module docs in [`super`].

use super::*;

/// Detect functional equivalence of `fn_name` and a same-named spec
/// fn (`spec_fn_name = law.name`). Requires (a) law.name resolves to
/// a pure user fn `spec_fd` in `inputs`; (b) law's lhs/rhs are direct
/// calls — one to `fn_name`, one to `law.name` — with identical
/// argument lists; (c) `impl_fd` and `spec_fd` bodies are single-
/// terminal-expression bodies whose AST nodes match exactly.
///
/// On match, returns the unfold list: impl + spec + any user
/// helpers reachable from the law sides. Sorted for deterministic
/// emit.
pub(super) fn detect_spec_equivalence(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
) -> Option<Vec<String>> {
    use crate::ast::Expr;
    use std::collections::BTreeSet;

    let spec_fn_name = &law.name;
    if spec_fn_name == fn_name {
        return None;
    }
    let spec_fd = inputs.find_fn_def_by_call_name(spec_fn_name)?;
    if !spec_fd.effects.is_empty() || spec_fd.name == "main" {
        return None;
    }
    let impl_fd = inputs.find_fn_def_by_call_name(fn_name)?;

    let direct_call =
        |expr: &Spanned<crate::ast::Expr>| -> Option<(String, Vec<Spanned<crate::ast::Expr>>)> {
            let Expr::FnCall(callee, args) = &expr.node else {
                return None;
            };
            let name = match &callee.node {
                Expr::Ident(n) | Expr::Resolved { name: n, .. } => n.clone(),
                _ => return None,
            };
            Some((name, args.clone()))
        };
    let canonical_shape =
        |lhs: &Spanned<crate::ast::Expr>, rhs: &Spanned<crate::ast::Expr>| -> bool {
            let Some((l_name, l_args)) = direct_call(lhs) else {
                return false;
            };
            let Some((r_name, r_args)) = direct_call(rhs) else {
                return false;
            };
            l_name == fn_name && r_name == *spec_fn_name && l_args == r_args
        };
    if !canonical_shape(&law.lhs, &law.rhs) && !canonical_shape(&law.rhs, &law.lhs) {
        return None;
    }

    let impl_body = body_terminal_expr(impl_fd.body.as_ref())?;
    let spec_body = body_terminal_expr(spec_fd.body.as_ref())?;
    if impl_body.node != spec_body.node {
        return None;
    }

    // Build the unfold set: impl + spec + transitively-reached user
    // helpers from law sides. Mirrors the legacy `law_simp_defs`
    // semantic but uses inputs (not CodegenContext).
    let resolve_user_fn = |name: &str| -> Option<&FnDef> {
        let fd = inputs.find_fn_def_by_call_name(name)?;
        if !fd.effects.is_empty() || fd.name == "main" {
            return None;
        }
        Some(fd)
    };
    let mut names: BTreeSet<String> = BTreeSet::new();
    names.insert(fn_name.to_string());
    names.insert(spec_fn_name.clone());
    let mut seed: BTreeSet<String> = BTreeSet::new();
    collect_fn_calls_expr(&law.lhs, &mut seed);
    collect_fn_calls_expr(&law.rhs, &mut seed);
    if let Some(when_expr) = &law.when {
        collect_fn_calls_expr(when_expr, &mut seed);
    }
    for n in seed {
        if let Some(fd) = resolve_user_fn(&n) {
            names.insert(fd.name.clone());
        }
    }
    loop {
        let before = names.len();
        let snapshot: Vec<String> = names.iter().cloned().collect();
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
                if let Some(callee_fd) = resolve_user_fn(&c) {
                    names.insert(callee_fd.name.clone());
                }
            }
        }
        if names.len() == before {
            break;
        }
    }
    Some(names.into_iter().collect())
}

/// Detect functional equivalence in the broader "simp-normalized"
/// shape: same canonical impl/spec call structure as
/// [`detect_spec_equivalence`], but bodies are equivalent only
/// after arg substitution + arithmetic identity folding (drop
/// `+ 0`, `- 0`, `* 1`, fold `* 0 → 0`). Returns the unfold list
/// on match.
pub(super) fn detect_simp_normalized_spec_equivalence(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
) -> Option<Vec<String>> {
    use crate::ast::Expr;
    use std::collections::BTreeSet;

    let spec_fn_name = &law.name;
    if spec_fn_name == fn_name {
        return None;
    }
    let spec_fd = inputs.find_fn_def_by_call_name(spec_fn_name)?;
    if !spec_fd.effects.is_empty() || spec_fd.name == "main" {
        return None;
    }
    let impl_fd = inputs.find_fn_def_by_call_name(fn_name)?;

    let direct_call =
        |expr: &Spanned<crate::ast::Expr>| -> Option<(String, Vec<Spanned<crate::ast::Expr>>)> {
            let Expr::FnCall(callee, args) = &expr.node else {
                return None;
            };
            let name = match &callee.node {
                Expr::Ident(n) | Expr::Resolved { name: n, .. } => n.clone(),
                _ => return None,
            };
            Some((name, args.clone()))
        };
    let canonical_shape_args = |lhs: &Spanned<crate::ast::Expr>,
                                rhs: &Spanned<crate::ast::Expr>|
     -> Option<Vec<Spanned<crate::ast::Expr>>> {
        let (l_name, l_args) = direct_call(lhs)?;
        let (r_name, r_args) = direct_call(rhs)?;
        if l_name != fn_name || r_name != *spec_fn_name || l_args != r_args {
            return None;
        }
        if l_args.len() != impl_fd.params.len() || r_args.len() != spec_fd.params.len() {
            return None;
        }
        Some(l_args)
    };
    let call_args = canonical_shape_args(&law.lhs, &law.rhs)
        .or_else(|| canonical_shape_args(&law.rhs, &law.lhs))?;

    let impl_body = body_terminal_expr(impl_fd.body.as_ref())?;
    let spec_body = body_terminal_expr(spec_fd.body.as_ref())?;
    // Reject the body-identical case — that's covered by the
    // strict `SpecEquivalence` detector running before this one.
    if impl_body.node == spec_body.node {
        return None;
    }
    let impl_subst: std::collections::HashMap<String, Spanned<crate::ast::Expr>> = impl_fd
        .params
        .iter()
        .zip(call_args.iter())
        .map(|((n, _), arg)| (n.clone(), arg.clone()))
        .collect();
    let spec_subst: std::collections::HashMap<String, Spanned<crate::ast::Expr>> = spec_fd
        .params
        .iter()
        .zip(call_args.iter())
        .map(|((n, _), arg)| (n.clone(), arg.clone()))
        .collect();
    let impl_normalised = simplify_identity_expr(&crate::ast_rewrite::rewrite_idents_scoped(
        impl_body,
        |name| impl_subst.get(name).cloned(),
    ));
    let spec_normalised = simplify_identity_expr(&crate::ast_rewrite::rewrite_idents_scoped(
        spec_body,
        |name| spec_subst.get(name).cloned(),
    ));
    if impl_normalised.node != spec_normalised.node {
        return None;
    }

    // Same unfold-set walk as `detect_spec_equivalence`.
    let resolve_user_fn = |name: &str| -> Option<&FnDef> {
        let fd = inputs.find_fn_def_by_call_name(name)?;
        if !fd.effects.is_empty() || fd.name == "main" {
            return None;
        }
        Some(fd)
    };
    let mut names: BTreeSet<String> = BTreeSet::new();
    names.insert(fn_name.to_string());
    names.insert(spec_fn_name.clone());
    let mut seed: BTreeSet<String> = BTreeSet::new();
    collect_fn_calls_expr(&law.lhs, &mut seed);
    collect_fn_calls_expr(&law.rhs, &mut seed);
    if let Some(when_expr) = &law.when {
        collect_fn_calls_expr(when_expr, &mut seed);
    }
    for n in seed {
        if let Some(fd) = resolve_user_fn(&n) {
            names.insert(fd.name.clone());
        }
    }
    loop {
        let before = names.len();
        let snapshot: Vec<String> = names.iter().cloned().collect();
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
                if let Some(callee_fd) = resolve_user_fn(&c) {
                    names.insert(callee_fd.name.clone());
                }
            }
        }
        if names.len() == before {
            break;
        }
    }
    Some(names.into_iter().collect())
}

/// Detect "linear Int" spec equivalence: same canonical impl/spec
/// call shape as the other spec detectors, all givens are `Int`,
/// both impl and spec return `Int`, and the substituted bodies are
/// purely linear arithmetic expressions over the law-quantified
/// givens (only `Int` literals, given idents, `Add`, `Sub`). On
/// match returns the two substituted bodies — backends rewrite to
/// `change <impl> = <spec>` and close via their linear-arithmetic
/// decision procedure (`omega` on Lean, Z3 LIA on Dafny).
pub(super) fn detect_linear_int_spec_equivalence(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
) -> Option<(Spanned<crate::ast::Expr>, Spanned<crate::ast::Expr>)> {
    use crate::ast::Expr;
    use std::collections::HashSet;

    if law.givens.is_empty() || !law.givens.iter().all(|g| g.type_name == "Int") {
        return None;
    }
    let spec_fn_name = &law.name;
    if spec_fn_name == fn_name {
        return None;
    }
    let spec_fd = inputs.find_fn_def_by_call_name(spec_fn_name)?;
    if !spec_fd.effects.is_empty() || spec_fd.name == "main" {
        return None;
    }
    let impl_fd = inputs.find_fn_def_by_call_name(fn_name)?;
    if impl_fd.return_type != "Int" || spec_fd.return_type != "Int" {
        return None;
    }

    let direct_call =
        |expr: &Spanned<crate::ast::Expr>| -> Option<(String, Vec<Spanned<crate::ast::Expr>>)> {
            let Expr::FnCall(callee, args) = &expr.node else {
                return None;
            };
            let name = match &callee.node {
                Expr::Ident(n) | Expr::Resolved { name: n, .. } => n.clone(),
                _ => return None,
            };
            Some((name, args.clone()))
        };
    let canonical_shape_args = |lhs: &Spanned<crate::ast::Expr>,
                                rhs: &Spanned<crate::ast::Expr>|
     -> Option<Vec<Spanned<crate::ast::Expr>>> {
        let (l_name, l_args) = direct_call(lhs)?;
        let (r_name, r_args) = direct_call(rhs)?;
        if l_name != fn_name || r_name != *spec_fn_name || l_args != r_args {
            return None;
        }
        if l_args.len() != impl_fd.params.len() || r_args.len() != spec_fd.params.len() {
            return None;
        }
        Some(l_args)
    };
    let call_args = canonical_shape_args(&law.lhs, &law.rhs)
        .or_else(|| canonical_shape_args(&law.rhs, &law.lhs))?;

    let impl_body = body_terminal_expr(impl_fd.body.as_ref())?;
    let spec_body = body_terminal_expr(spec_fd.body.as_ref())?;
    let impl_subst: std::collections::HashMap<String, Spanned<crate::ast::Expr>> = impl_fd
        .params
        .iter()
        .zip(call_args.iter())
        .map(|((n, _), arg)| (n.clone(), arg.clone()))
        .collect();
    let spec_subst: std::collections::HashMap<String, Spanned<crate::ast::Expr>> = spec_fd
        .params
        .iter()
        .zip(call_args.iter())
        .map(|((n, _), arg)| (n.clone(), arg.clone()))
        .collect();
    let unfolded_impl =
        crate::ast_rewrite::rewrite_idents_scoped(impl_body, |name| impl_subst.get(name).cloned());
    let unfolded_spec =
        crate::ast_rewrite::rewrite_idents_scoped(spec_body, |name| spec_subst.get(name).cloned());

    let allowed_idents: HashSet<&str> = law.givens.iter().map(|g| g.name.as_str()).collect();
    if !is_linear_int_expr(&unfolded_impl, &allowed_idents)
        || !is_linear_int_expr(&unfolded_spec, &allowed_idents)
    {
        return None;
    }
    Some((unfolded_impl, unfolded_spec))
}

/// Check whether `expr` is purely linear arithmetic over `allowed_
/// idents`: only `Int` literals, allowed idents, and `Add`/`Sub`
/// BinOps. Mirrors legacy `spec::linear_int::is_linear_int_expr`.
pub(super) fn is_linear_int_expr(
    expr: &Spanned<crate::ast::Expr>,
    allowed_idents: &std::collections::HashSet<&str>,
) -> bool {
    use crate::ast::{BinOp, Expr, Literal};
    match &expr.node {
        Expr::Literal(Literal::Int(_)) => true,
        Expr::Ident(name) | Expr::Resolved { name, .. } => allowed_idents.contains(name.as_str()),
        Expr::BinOp(BinOp::Add | BinOp::Sub, left, right) => {
            is_linear_int_expr(left, allowed_idents) && is_linear_int_expr(right, allowed_idents)
        }
        _ => false,
    }
}

/// Detect functional equivalence between an effectful impl fn and
/// a spec fn (different name). Runs Oracle Lift over both sides of
/// the law first — injecting `BranchPath.Root` and oracle givens
/// into every classified effectful call site — then matches the
/// canonical `impl(args) == spec(args)` direct-call shape with
/// identical argument lists on the rewritten form. Returns the
/// spec fn name on match.
///
/// Body-match is not required here (and would fail — impl and spec
/// bodies usually differ syntactically; Oracle Lift normalises them
/// to a common oracle call only after backend-side `simp` unfolds).
pub(super) fn detect_effectful_spec_equivalence(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
) -> Option<String> {
    use crate::ast::Expr;

    let impl_fd = inputs.find_fn_def_by_call_name(fn_name)?;
    if impl_fd.effects.is_empty() {
        return None;
    }
    if !impl_fd
        .effects
        .iter()
        .all(|e| crate::types::checker::effect_classification::is_classified(&e.node))
    {
        return None;
    }

    let find_fn = |name: &str| -> Option<&crate::ast::FnDef> {
        inputs
            .entry_items
            .iter()
            .filter_map(|item| match item {
                TopLevel::FnDef(fd) => Some(fd),
                _ => None,
            })
            .find(|fd| fd.name == name)
    };
    let rewritten_lhs = crate::codegen::common::rewrite_effectful_calls_in_law(
        &law.lhs,
        law,
        find_fn,
        crate::codegen::common::OracleInjectionMode::LemmaBindingProjected,
    );
    let rewritten_rhs = crate::codegen::common::rewrite_effectful_calls_in_law(
        &law.rhs,
        law,
        find_fn,
        crate::codegen::common::OracleInjectionMode::LemmaBindingProjected,
    );

    let direct_call =
        |expr: &Spanned<crate::ast::Expr>| -> Option<(String, Vec<Spanned<crate::ast::Expr>>)> {
            let Expr::FnCall(callee, args) = &expr.node else {
                return None;
            };
            let name = match &callee.node {
                Expr::Ident(n) | Expr::Resolved { name: n, .. } => n.clone(),
                _ => return None,
            };
            Some((name, args.clone()))
        };
    let try_side = |impl_side: &Spanned<crate::ast::Expr>,
                    spec_side: &Spanned<crate::ast::Expr>|
     -> Option<String> {
        let (l_name, l_args) = direct_call(impl_side)?;
        let (r_name, r_args) = direct_call(spec_side)?;
        if l_args != r_args || l_name == r_name || l_name != fn_name {
            return None;
        }
        Some(r_name)
    };
    try_side(&rewritten_lhs, &rewritten_rhs).or_else(|| try_side(&rewritten_rhs, &rewritten_lhs))
}

/// Detect second-order linear recurrence spec equivalence (fib /
/// fibSpec pattern). impl_fn is a tail-rec wrapper dispatching on
/// `n < 0` and calling a 3-arg helper with seed pair; spec_fn is a
/// direct recurrence with `match n { 0 / 1 / _ }` arms. The shared
/// affine recurrence must match between the helper's worker and the
/// spec's `_` arm. Returns `(spec_fn_name, helper_fn_name)` on
/// match. Detection lives behind the `lean::recurrence::detect_*`
/// helpers because their AST patterns were specced there originally;
/// the data they extract is backend-neutral.
pub(super) fn detect_linear_recurrence2_spec_equivalence(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
) -> Option<(String, String)> {
    use crate::codegen::lean::recurrence::{
        detect_second_order_int_linear_recurrence, detect_tailrec_int_linear_pair_worker,
        detect_tailrec_int_linear_pair_wrapper,
    };

    let spec_fn_name = &law.name;
    if spec_fn_name == fn_name {
        return None;
    }
    if !law_references_fn(&law.lhs, spec_fn_name) && !law_references_fn(&law.rhs, spec_fn_name) {
        return None;
    }

    let impl_fd = inputs.find_fn_def_by_call_name(fn_name)?;
    let spec_fd = inputs.find_fn_def_by_call_name(spec_fn_name)?;
    let impl_shape = detect_tailrec_int_linear_pair_wrapper(impl_fd)?;
    let spec_shape = detect_second_order_int_linear_recurrence(spec_fd)?;

    // AST-strict cross-check: negative branch + seed values must
    // match across impl wrapper and spec direct recurrence.
    if impl_shape.negative_branch.node != spec_shape.negative_branch.node
        || impl_shape.seed_prev.node != spec_shape.base0.node
        || impl_shape.seed_curr.node != spec_shape.base1.node
    {
        return None;
    }

    let helper_fd = inputs.find_fn_def_by_call_name(&impl_shape.helper_fn_name)?;
    let helper_shape = detect_tailrec_int_linear_pair_worker(helper_fd)?;
    if helper_shape.recurrence != spec_shape.recurrence {
        return None;
    }

    Some((spec_fn_name.clone(), impl_shape.helper_fn_name))
}

pub(super) fn law_references_fn(expr: &Spanned<crate::ast::Expr>, target: &str) -> bool {
    use crate::ast::Expr;
    match &expr.node {
        Expr::FnCall(callee, args) => {
            let name = match &callee.node {
                Expr::Ident(n) | Expr::Resolved { name: n, .. } => Some(n.as_str()),
                _ => None,
            };
            if name == Some(target) {
                return true;
            }
            args.iter().any(|a| law_references_fn(a, target))
        }
        Expr::BinOp(_, l, r) => law_references_fn(l, target) || law_references_fn(r, target),
        Expr::Attr(base, _) => law_references_fn(base, target),
        Expr::Match { subject, arms } => {
            law_references_fn(subject, target)
                || arms.iter().any(|arm| law_references_fn(&arm.body, target))
        }
        _ => false,
    }
}

/// Detect a `given` that binds a recursive sum-typed ADT — the
/// induction target. Returns the given's source name on first
/// match, or `None` when no given fits.
///
/// "Recursive" means at least one variant references the type
/// itself in its field list (either bare `Tree` or wrapped like
/// `List<Tree>` / `Tree, Tree`). Indirect-via-other-types rec
/// shapes are rejected here — the backend's emit can't handle
/// them and would fail at lake-build time; better to fall through
/// to `BackendDispatch` than pin a bad strategy.
/// Stage 8b of #232: detect `chain_qm(g) == chain_manual(g)` where
/// `chain_qm` is a `ModulePattern::ResultPipelineChain` (the `?`-chain
/// fn) and `chain_manual` is a manual `match Result.Err -> Err`
/// nested chain over the same step fns. Returns a payload carrying
/// both fn names + the ordered step list so backends can emit the
/// unfold list without re-walking the AST.
pub(super) fn detect_result_pipeline_chain_equivalence(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
) -> Option<crate::ir::ProofStrategy> {
    use crate::analysis::shape::ModulePattern;
    use crate::ast::{Expr, Pattern, Stmt};

    fn ident_name(e: &Spanned<Expr>) -> Option<&str> {
        match &e.node {
            Expr::Ident(n) => Some(n.as_str()),
            Expr::Resolved { name, .. } => Some(name.as_str()),
            _ => None,
        }
    }

    let shape = inputs.program_shape?;

    if law.givens.len() != 1 {
        return None;
    }
    let given_name = &law.givens[0].name;

    // Confirm fn_name is a ResultPipelineChain in the shape, and
    // pull the step fn list from there — post-pipeline AST has
    // already desugared `?` into nested matches, so the shape
    // (built from pre-resolver source items) is the authoritative
    // record of the original step list.
    let (chain_qm_fn, step_fns) = shape.patterns.iter().find_map(|p| match p {
        ModulePattern::ResultPipelineChain {
            fn_name: n,
            step_fns,
            ..
        } if n == fn_name => Some((n.clone(), step_fns.clone())),
        _ => None,
    })?;

    // Law shape: `chain_qm(g) == chain_manual(g)` (either side).
    let extract = |expr: &Spanned<Expr>| -> Option<String> {
        let Expr::FnCall(callee, args) = &expr.node else {
            return None;
        };
        let name = ident_name(callee)?;
        if args.len() != 1 {
            return None;
        }
        if ident_name(&args[0])? != given_name {
            return None;
        }
        Some(name.to_string())
    };
    let lhs_call = extract(&law.lhs);
    let rhs_call = extract(&law.rhs);
    let chain_manual_fn = match (lhs_call, rhs_call) {
        (Some(l), Some(r)) if l == chain_qm_fn && r != chain_qm_fn => r,
        (Some(l), Some(r)) if r == chain_qm_fn && l != chain_qm_fn => l,
        _ => return None,
    };

    if step_fns.len() < 2 {
        return None;
    }

    // Verify the manual fn is a nested `match Result.Err -> Err`
    // chain calling the same step fns. Heuristic: walk body, look
    // for `Match { subject: Call(step, _), arms: [Err -> Err, Ok -> ...] }`
    // pattern. Counts how many step calls show up.
    let manual_fd = inputs.find_fn_def_by_call_name(&chain_manual_fn)?;
    let mut manual_steps: Vec<String> = Vec::new();
    fn walk_manual<'a>(
        expr: &'a Spanned<Expr>,
        steps: &mut Vec<String>,
        ident_name: &dyn Fn(&'a Spanned<Expr>) -> Option<&'a str>,
    ) {
        if let Expr::Match { subject, arms } = &expr.node
            && let Expr::FnCall(callee, _) = &subject.node
            && let Some(n) = ident_name(subject).or_else(|| ident_name(callee))
        {
            let has_err_pass = arms.iter().any(|a| {
                let pat_is_err = matches!(
                    &a.pattern,
                    Pattern::Constructor(c, _) if c == "Result.Err" || c.ends_with(".Err")
                );
                // Body shape: either `Expr::Constructor("Result.Err", _)`
                // (pre-resolver) or `Expr::FnCall(Attr(Ident("Result"), "Err"), _)`
                // (post-resolver — `Result.Err(...)` is treated as a
                // method-attr call once names are wired up).
                let body_is_err = match &a.body.node {
                    Expr::Constructor(c, _) => c == "Result.Err" || c.ends_with(".Err"),
                    Expr::FnCall(callee, _) => matches!(
                        &callee.node,
                        Expr::Attr(base, attr)
                            if attr == "Err"
                                && matches!(&base.node, Expr::Ident(b) if b == "Result")
                    ),
                    _ => false,
                };
                pat_is_err && body_is_err
            });
            if has_err_pass {
                steps.push(n.to_string());
            }
            for a in arms {
                walk_manual(&a.body, steps, ident_name);
            }
        }
    }
    let manual_stmts = manual_fd.body.stmts();
    if manual_stmts.len() != 1 {
        return None;
    }
    let Stmt::Expr(manual_root) = &manual_stmts[0] else {
        return None;
    };
    walk_manual(manual_root, &mut manual_steps, &ident_name);
    if manual_steps.len() < 2 {
        return None;
    }

    Some(crate::ir::ProofStrategy::ResultPipelineChain {
        chain_qm_fn,
        chain_manual_fn,
        step_fns,
    })
}

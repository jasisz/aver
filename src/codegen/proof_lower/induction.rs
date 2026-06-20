//! Structural-induction target detection, the `LawProofCone`
//! dependency closure, and the shared fn/call-graph walkers.
//!
//! Split from `proof_lower.rs` — see the module docs in [`super`].

use super::*;

pub(super) fn iter_all_fn_defs<'a>(
    inputs: &'a ProofLowerInputs<'a>,
) -> impl Iterator<Item = &'a FnDef> {
    inputs
        .entry_items
        .iter()
        .filter_map(|item| match item {
            TopLevel::FnDef(fd) => Some(fd),
            _ => None,
        })
        .chain(inputs.dep_modules.iter().flat_map(|m| m.fn_defs.iter()))
}

pub(super) fn body_calls_any_of_inputs(
    body: &crate::ast::FnBody,
    names: &std::collections::BTreeSet<String>,
) -> bool {
    let mut called = std::collections::BTreeSet::new();
    for stmt in body.stmts() {
        match stmt {
            crate::ast::Stmt::Binding(_, _, e) | crate::ast::Stmt::Expr(e) => {
                collect_fn_calls_expr(e, &mut called);
            }
        }
    }
    called.iter().any(|c| names.contains(c))
}

pub(super) fn collect_fn_calls_expr(
    expr: &Spanned<crate::ast::Expr>,
    out: &mut std::collections::BTreeSet<String>,
) {
    use crate::ast::Expr;
    match &expr.node {
        Expr::FnCall(f, args) => {
            if let Some(name) = expr_to_dotted_name(&f.node) {
                // Skip uppercase namespace handles (`List.len`,
                // `Option.Some`) — those are built-in namespaces,
                // not user fns the auto-proof can unfold. The leaf
                // segment's case discriminates user fns from
                // namespace types (cross-module user calls survive
                // because the leaf fn name starts lower-case).
                //
                // Builtin *scalar*-namespace methods (`Int.max`,
                // `Float.abs`, `Bool.and`, `String.len`, …) have a
                // lowercase leaf but lower to **core** Lean ops, not
                // to unfoldable user defs — `Int.max` becomes `max`,
                // there is no constant named `Int.max` in scope. Citing
                // them in `simp only [...]` is a hard `unknown constant`
                // error, so the scalar namespaces are excluded by their
                // HEAD segment. Cross-module USER calls (`MyModule.helper`)
                // keep flowing through — only the four builtin scalar
                // namespaces are filtered.
                let last = name.rsplit('.').next().unwrap_or(&name);
                let head = name.split('.').next().unwrap_or(&name);
                let is_builtin_scalar_ns = matches!(head, "Int" | "Float" | "Bool" | "String");
                if !is_builtin_scalar_ns && last.chars().next().is_some_and(|c| c.is_lowercase()) {
                    out.insert(name);
                }
            }
            for arg in args {
                collect_fn_calls_expr(arg, out);
            }
        }
        Expr::BinOp(_, l, r) => {
            collect_fn_calls_expr(l, out);
            collect_fn_calls_expr(r, out);
        }
        Expr::Attr(obj, _) => collect_fn_calls_expr(obj, out),
        Expr::Match { subject, arms, .. } => {
            collect_fn_calls_expr(subject, out);
            for arm in arms {
                collect_fn_calls_expr(&arm.body, out);
            }
        }
        Expr::TailCall(boxed) => {
            out.insert(boxed.target.clone());
            for arg in &boxed.args {
                collect_fn_calls_expr(arg, out);
            }
        }
        // Value-carrying expressions whose sub-exprs hold fn calls the proof's
        // unfold set needs. Crucially `ErrorProp` (`?`): a refinement-pipeline
        // body like `na = fromInt(a)?; add(na, nb)?` hides every call behind `?`,
        // so without this arm the unfold set misses `fromInt`/`add` and the
        // LinearArithmetic tactic stalls with "simp made no progress". Mirrors
        // `proof_recognize::collect_called_fns`.
        Expr::ErrorProp(inner) | Expr::Neg(inner) => collect_fn_calls_expr(inner, out),
        Expr::Constructor(_, Some(arg)) => collect_fn_calls_expr(arg, out),
        Expr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                collect_fn_calls_expr(e, out);
            }
        }
        Expr::List(elems) | Expr::Tuple(elems) | Expr::IndependentProduct(elems, _) => {
            for e in elems {
                collect_fn_calls_expr(e, out);
            }
        }
        _ => {}
    }
}

/// The scoped pure-function dependency closure of a `verify ... law` — the
/// set of pure user functions a law's proof is allowed to mention.
///
/// The various strategy detectors each re-derive the pure-fn closure ad hoc
/// (`law_helper_unfolds`, the spec-equivalence unfold list, etc.); this is the
/// single reusable computation they share. `law_helper_unfolds` consumes
/// `pure_fns` (transitively reachable helper source names, outer fn excluded,
/// sorted for deterministic emit).
pub(crate) struct LawProofCone<'a> {
    /// Pure user fns transitively reachable from the law's lhs/rhs/when,
    /// in deterministic (sorted-by-name) order, excluding the law's own
    /// subject fn.
    pure_fns: Vec<&'a FnDef>,
}

impl<'a> LawProofCone<'a> {
    /// Build the cone: seed from the law sides (lhs/rhs/when), keep only
    /// pure user fns (no effects, not `main`), transitively expand through
    /// their bodies, then drop the law's own subject (`outer_fn`) — each
    /// backend unfolds that one separately. Finally resolve the ADTs those
    /// fns range over from their signatures (`types`).
    pub(crate) fn compute(
        law: &crate::ast::VerifyLaw,
        outer_fn: &str,
        inputs: &ProofLowerInputs<'a>,
    ) -> Self {
        use std::collections::BTreeSet;

        let resolve_user_fn = |name: &str| -> Option<&'a FnDef> {
            let fd = inputs.find_fn_def_by_call_name(name)?;
            if !fd.effects.is_empty() || fd.name == "main" {
                return None;
            }
            Some(fd)
        };

        // Seed from law sides, immediately filtering to user-fn names.
        let mut raw: BTreeSet<String> = BTreeSet::new();
        collect_fn_calls_expr(&law.lhs, &mut raw);
        collect_fn_calls_expr(&law.rhs, &mut raw);
        if let Some(when_expr) = &law.when {
            collect_fn_calls_expr(when_expr, &mut raw);
        }
        let mut names: BTreeSet<String> = raw
            .into_iter()
            .filter_map(|n| resolve_user_fn(&n).map(|fd| fd.name.clone()))
            .collect();

        // Transitive expansion through pure user fn bodies.
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
        names.remove(outer_fn);

        // Resolve the (sorted) names back to defs — `names` only ever held
        // canonical `fd.name`s of pure user fns, so every entry re-resolves.
        let pure_fns: Vec<&'a FnDef> = names.iter().filter_map(|n| resolve_user_fn(n)).collect();

        Self { pure_fns }
    }

    /// The cone's pure-fn names, deterministic (sorted) order, excluding the
    /// law's subject. Exactly the legacy `law_helper_unfolds` result.
    fn unfold_names(&self) -> Vec<String> {
        self.pure_fns.iter().map(|fd| fd.name.clone()).collect()
    }

    /// The cone's pure-fn vocabulary — the functions the term enumerator
    /// may apply. Deterministic (sorted-by-name) order, law subject excluded.
    pub(crate) fn pure_fns(&self) -> &[&'a FnDef] {
        &self.pure_fns
    }
}

pub(super) fn law_helper_unfolds(
    law: &crate::ast::VerifyLaw,
    outer_fn: &str,
    inputs: &ProofLowerInputs,
) -> Vec<String> {
    LawProofCone::compute(law, outer_fn, inputs).unfold_names()
}

pub(super) fn detect_induction_target(
    law: &crate::ast::VerifyLaw,
    inputs: &ProofLowerInputs,
) -> Option<String> {
    // Stage 7 of #232: read the eligibility set from `ProgramShape`
    // when threaded through `ProofLowerInputs`. The shape pass
    // computes the same direct-rec + not-indirect-rec predicate
    // once per compilation; the inline scan below is the
    // pre-shape fallback so callers without a shape (legacy test
    // fixtures, tools that build `ProofLowerInputs` by hand) keep
    // working unchanged.
    if let Some(shape) = inputs.program_shape {
        for given in &law.givens {
            if shape.inductable_sum_types.contains(&given.type_name) {
                return Some(given.name.clone());
            }
        }
        // Builtin `List<T>` given — structural induction via nil/cons (#409).
        // Reached only AFTER the other strategy detectors in
        // `populate_law_theorems` have declined this law, so a law with a
        // working bespoke strategy (e.g. quicksort's ordering/idempotent) keeps
        // it; only laws nobody else classified fall through to list induction.
        if let Some(given) = list_induction_given(law) {
            return Some(given);
        }
        return None;
    }
    detect_induction_target_legacy(law, inputs)
}

/// A `given` whose declared type is a builtin `List<T>` — the structural
/// induction target name, if any.
pub(super) fn list_induction_given(law: &crate::ast::VerifyLaw) -> Option<String> {
    law.givens
        .iter()
        .find(|g| g.type_name.trim().starts_with("List<"))
        .map(|g| g.name.clone())
}

pub(super) fn detect_induction_target_legacy(
    law: &crate::ast::VerifyLaw,
    inputs: &ProofLowerInputs,
) -> Option<String> {
    use crate::ast::TypeDef;
    for given in &law.givens {
        let Some(TypeDef::Sum {
            name: type_name,
            variants,
            ..
        }) = inputs.find_type_def(&given.type_name)
        else {
            continue;
        };
        let direct_rec = variants.iter().any(|variant| {
            variant.fields.iter().any(|field| {
                let f = field.trim();
                f == type_name
                    || f.contains(&format!("<{}", type_name))
                    || f.contains(&format!("{}>", type_name))
                    || f.contains(&format!(", {}", type_name))
                    || f.contains(&format!("{},", type_name))
            })
        });
        if !direct_rec {
            continue;
        }
        if has_indirect_rec_variants(variants, type_name) {
            continue;
        }
        return Some(given.name.clone());
    }
    list_induction_given(law)
}

/// Mirror of `lean::law_auto::induction::has_indirect_variants` —
/// when a variant's field carries the type wrapped inside another
/// generic in a shape the per-variant emit can't decompose
/// (e.g. `Some(List<Self>)` past the simple direct-rec case),
/// the backend rejects. Replicated here so the lowerer's pin
/// matches what the backend would accept.
pub(super) fn has_indirect_rec_variants(
    variants: &[crate::ast::TypeVariant],
    type_name: &str,
) -> bool {
    for variant in variants {
        for field in &variant.fields {
            let f = field.trim();
            // Direct match — that's the recursion we want, not "indirect".
            if f == type_name {
                continue;
            }
            // Bare `List<Tree>` / `Vec<Tree>` is fine (direct list
            // recursion); deeper nesting we conservatively reject.
            let opens = f.matches('<').count();
            if opens > 1 && f.contains(type_name) {
                return true;
            }
        }
    }
    false
}

use std::collections::HashSet;

use crate::ast::{
    Expr, FnBody, FnDef, Literal, MatchArm, Pattern, Spanned, Stmt, StrPart, TailCallData,
    TopLevel, TypeDef, TypeVariant, VerifyBlock, VerifyGivenDomain, VerifyKind,
};
use crate::codegen::CodegenContext;
use crate::types::Type;

/// A "refinement record" is the canonical `refinement-via-opaque`
/// pattern: a single-field `record X { carrier: T }` paired with a
/// validating smart constructor
///   `fn fromX(p: T) -> Result<X, _>` body = `match <pred-in-p> with`
///   `    true  -> Result.Ok(X(carrier = p))`
///   `    false -> Result.Err("...")`
///
/// Detecting this shape lets backends emit the type as a true
/// dependent / subset type (`def X := { n : T // P n }` in Lean,
/// `type X = n: T | P n` in Dafny) instead of a flat product, which
/// in turn collapses universal-law proofs into one-liners
/// (`rw [Int.add_comm]`) by carrying the invariant inside the type
/// rather than threading it through ad-hoc tactic plumbing.
#[derive(Debug, Clone)]
pub struct RefinementInfo<'a> {
    /// Carrier-type annotation as written in the record field
    /// (`"Int"`, `"Float"`, …). Backends emit this as the
    /// subset's underlying type.
    pub carrier_type: &'a str,
    /// Carrier-field name (e.g. `"value"`). Lean projects through
    /// `.val` on a Subtype, so users of the carrier field have to
    /// rewrite `n.value → n.val` when the host type is refined.
    pub carrier_field: &'a str,
    /// Name of the smart constructor's input parameter (`"n"` in
    /// `fromInt(n: Int) → Result<X, _>`). Used when substituting
    /// the law's quantified variable into the predicate.
    pub param_name: &'a str,
    /// Source-level name of the smart constructor itself (`"fromInt"`).
    /// The interval analysis gates its transparent-wrapper peel on this
    /// name so only the real smart constructor — never an arbitrary
    /// one-arg helper — is treated as identity over its argument.
    pub constructor_fn: &'a str,
    /// AST node for the bool predicate the smart constructor
    /// branches on — the body's `Match { subject = <here>, ... }`.
    pub predicate: &'a Spanned<Expr>,
}

/// Inspect inputs for a refinement-via-opaque record by `type_name`.
/// Returns `Some(info)` iff there's exactly one matching smart
/// constructor and the record has a single carrier field.
///
/// Defaults to "any scope" — walks entry + every dep module's
/// type_defs and fn_defs to find the record + smart constructor.
/// Use [`refinement_info_for_in_scope`] when two modules declare a
/// refined record of the same bare name (`A.Natural` vs `B.Natural`)
/// and you need each scope's *own* predicate; the unscoped form
/// returns whichever record walked first.
pub fn refinement_info_for<'a>(
    type_name: &str,
    inputs: &crate::codegen::proof_lower::ProofLowerInputs<'a>,
) -> Option<RefinementInfo<'a>> {
    refinement_info_for_walk(type_name, inputs, None)
}

/// Module-scoped variant of [`refinement_info_for`]. `scope =
/// None` means "look in entry items only"; `scope = Some(prefix)`
/// means "look in the dep module whose `prefix` matches".
///
/// Refinement-via-opaque is a single-module pattern: the record is
/// declared `exposes opaque [X]` and the smart constructor lives in
/// the same module (the carrier field isn't reachable from outside,
/// so the constructor can't reside elsewhere). Scoping the search
/// to one module gives each scope its own predicate, which is what
/// `populate_refined_types` needs so canonical `A.Natural` and
/// `B.Natural` slots don't share a predicate from whichever module
/// happened to walk first.
pub fn refinement_info_for_in_scope<'a>(
    type_name: &str,
    inputs: &crate::codegen::proof_lower::ProofLowerInputs<'a>,
    scope: Option<&str>,
) -> Option<RefinementInfo<'a>> {
    refinement_info_for_walk(type_name, inputs, Some(scope))
}

fn refinement_info_for_walk<'a>(
    type_name: &str,
    inputs: &crate::codegen::proof_lower::ProofLowerInputs<'a>,
    // Outer Option: None = "any scope" (legacy); Some(inner) =
    // module-scoped. Inner Option: None = entry, Some(prefix) =
    // module by prefix.
    scope_filter: Option<Option<&str>>,
) -> Option<RefinementInfo<'a>> {
    // Stage 6b of #232: when the caller threaded a `ProgramShape`
    // through `ProofLowerInputs.program_shape`, look up the typed
    // pattern directly instead of walking the AST. The
    // `detection_payload_matches_refinement_info_for` test guards
    // that the two paths return the same carrier/predicate, so this
    // adapter is behavior-preserving.
    if let Some(shape) = inputs.program_shape
        && let Some(info) = refinement_info_from_shape(shape, inputs, type_name, scope_filter)
    {
        return Some(info);
    }
    refinement_info_for_walk_legacy(type_name, inputs, scope_filter)
}

/// Stage 6b adapter: pick the typed `RefinementSmartConstructor`
/// pattern from `ProgramShape.patterns` and project it back into the
/// legacy `RefinementInfo<'a>` shape (with `&'a Spanned<Expr>`
/// predicate). The predicate is borrowed from the matching smart
/// constructor's body in `inputs.entry_items` / `inputs.dep_modules` —
/// the pattern's owned clone is only used for the cross-check test.
fn refinement_info_from_shape<'a>(
    shape: &crate::analysis::shape::ProgramShape,
    inputs: &crate::codegen::proof_lower::ProofLowerInputs<'a>,
    type_name: &str,
    scope_filter: Option<Option<&str>>,
) -> Option<RefinementInfo<'a>> {
    use crate::analysis::shape::ModulePattern;

    let pattern_match = shape.patterns.iter().find_map(|p| {
        let ModulePattern::RefinementSmartConstructor {
            scope,
            type_name: ptn,
            constructor_fn,
            ..
        } = p
        else {
            return None;
        };
        if ptn != type_name {
            return None;
        }
        let scope_ok = match (scope_filter, scope.as_deref()) {
            (None, _) => true,
            (Some(None), None) => true,
            (Some(None), Some(_)) => false,
            (Some(Some(want)), Some(have)) => want == have,
            (Some(Some(_)), None) => false,
        };
        if !scope_ok {
            return None;
        }
        Some((scope.clone(), constructor_fn.clone()))
    })?;

    let (scope, constructor_fn) = pattern_match;

    let (carrier_field, carrier_type) = match scope.as_deref() {
        None => inputs.entry_items.iter().find_map(|i| match i {
            TopLevel::TypeDef(TypeDef::Product { name, fields, .. })
                if name == type_name && fields.len() == 1 =>
            {
                let (fname, ftype) = &fields[0];
                Some((fname.as_str(), ftype.as_str()))
            }
            _ => None,
        }),
        Some(prefix) => inputs
            .dep_modules
            .iter()
            .find(|m| m.prefix == prefix)
            .and_then(|m| {
                m.type_defs.iter().find_map(|td| match td {
                    TypeDef::Product { name, fields, .. }
                        if name == type_name && fields.len() == 1 =>
                    {
                        let (fname, ftype) = &fields[0];
                        Some((fname.as_str(), ftype.as_str()))
                    }
                    _ => None,
                })
            }),
    }?;

    let constructor_fd: &'a FnDef = match scope.as_deref() {
        None => inputs.entry_items.iter().find_map(|i| match i {
            TopLevel::FnDef(fd) if fd.name == constructor_fn => Some(fd),
            _ => None,
        }),
        Some(prefix) => inputs
            .dep_modules
            .iter()
            .find(|m| m.prefix == prefix)
            .and_then(|m| m.fn_defs.iter().find(|fd| fd.name == constructor_fn)),
    }?;

    let (param_name, _) = constructor_fd.params.first()?;
    let stmts = constructor_fd.body.stmts();
    let Stmt::Expr(body_expr) = stmts.first()? else {
        return None;
    };
    let Expr::Match { subject, .. } = &body_expr.node else {
        return None;
    };

    Some(RefinementInfo {
        carrier_type,
        carrier_field,
        param_name,
        constructor_fn: constructor_fd.name.as_str(),
        predicate: subject,
    })
}

fn refinement_info_for_walk_legacy<'a>(
    type_name: &str,
    inputs: &crate::codegen::proof_lower::ProofLowerInputs<'a>,
    scope_filter: Option<Option<&str>>,
) -> Option<RefinementInfo<'a>> {
    let entry_only = matches!(scope_filter, Some(None));
    let module_only_prefix = match scope_filter {
        Some(Some(p)) => Some(p),
        _ => None,
    };
    let allow_entry = scope_filter.is_none() || entry_only;
    let entry_typedefs: Vec<&'a TypeDef> = if allow_entry {
        inputs
            .entry_items
            .iter()
            .filter_map(|item| match item {
                TopLevel::TypeDef(td) => Some(td),
                _ => None,
            })
            .collect()
    } else {
        Vec::new()
    };
    let module_typedefs: Vec<&'a TypeDef> = inputs
        .dep_modules
        .iter()
        .filter(|m| match (scope_filter, module_only_prefix) {
            (None, _) => true,
            (Some(None), _) => false,
            (Some(Some(_)), Some(p)) => m.prefix == p,
            _ => false,
        })
        .flat_map(|m| m.type_defs.iter())
        .collect();
    let (carrier_field, carrier_type) = entry_typedefs
        .into_iter()
        .chain(module_typedefs)
        .find_map(|td| match td {
            TypeDef::Product { name, fields, .. } if name == type_name && fields.len() == 1 => {
                let (fname, ftype) = &fields[0];
                Some((fname.as_str(), ftype.as_str()))
            }
            _ => None,
        })?;

    let entry_fns: Vec<&'a FnDef> = if allow_entry {
        inputs
            .entry_items
            .iter()
            .filter_map(|item| match item {
                TopLevel::FnDef(fd) => Some(fd),
                _ => None,
            })
            .collect()
    } else {
        Vec::new()
    };
    let module_fns: Vec<&'a FnDef> = inputs
        .dep_modules
        .iter()
        .filter(|m| match (scope_filter, module_only_prefix) {
            (None, _) => true,
            (Some(None), _) => false,
            (Some(Some(_)), Some(p)) => m.prefix == p,
            _ => false,
        })
        .flat_map(|m| m.fn_defs.iter())
        .collect();
    for fd in entry_fns.into_iter().chain(module_fns) {
        if !fd.return_type.starts_with("Result<") {
            continue;
        }
        if !fd.return_type[7..].starts_with(type_name) {
            continue;
        }
        if fd.params.len() != 1 {
            continue;
        }
        let (param_name, _) = &fd.params[0];
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
        if !is_bool_ok_err_match(arms, type_name, carrier_field, param_name) {
            continue;
        }
        return Some(RefinementInfo {
            carrier_type,
            carrier_field,
            param_name,
            constructor_fn: fd.name.as_str(),
            predicate: subject,
        });
    }
    None
}

/// True iff a two-arm bool match is the canonical refinement shape:
/// `true -> Result.Ok(<TypeName>(<carrier_field> = <param>))` and
/// `false -> Result.Err(_)`. Required so we don't mis-classify a
/// random `match … -> Result.Ok(...) | -> Result.Err(...)` (e.g. an
/// effectful pipeline) as a smart constructor.
pub fn is_refinement_bool_ok_err_match(
    arms: &[MatchArm],
    type_name: &str,
    carrier_field: &str,
    param_name: &str,
) -> bool {
    is_bool_ok_err_match(arms, type_name, carrier_field, param_name)
}

fn is_bool_ok_err_match(
    arms: &[MatchArm],
    type_name: &str,
    carrier_field: &str,
    param_name: &str,
) -> bool {
    if arms.len() != 2 {
        return false;
    }
    let mut true_ok = false;
    let mut false_err = false;
    for arm in arms {
        match &arm.pattern {
            Pattern::Literal(Literal::Bool(true)) => {
                if is_ok_constructor_with_identity(&arm.body, type_name, carrier_field, param_name)
                {
                    true_ok = true;
                }
            }
            Pattern::Literal(Literal::Bool(false)) => {
                if is_err_constructor(&arm.body) {
                    false_err = true;
                }
            }
            _ => return false,
        }
    }
    true_ok && false_err
}

fn is_ok_constructor_with_identity(
    expr: &Spanned<Expr>,
    type_name: &str,
    carrier_field: &str,
    param_name: &str,
) -> bool {
    // Result.Ok(<TypeName>(<carrier_field> = <param>))
    let (ctor_name, ctor_arg_node) = match &expr.node {
        Expr::Constructor(name, Some(arg)) => (name.clone(), &arg.node),
        Expr::FnCall(callee, args) if args.len() == 1 => {
            let Some(name) = expr_to_dotted_name(&callee.node) else {
                return false;
            };
            (name, &args[0].node)
        }
        _ => return false,
    };
    if ctor_name != "Result.Ok" {
        return false;
    }
    let (t, fields) = match ctor_arg_node {
        Expr::RecordCreate {
            type_name: t,
            fields,
        } => (t.as_str(), fields),
        _ => return false,
    };
    if t != type_name || fields.len() != 1 {
        return false;
    }
    let (fname, fvalue) = &fields[0];
    if fname != carrier_field {
        return false;
    }
    // Post-resolver bodies have `Expr::Resolved` instead of
    // `Expr::Ident` for fn-param references; accept both shapes so
    // detection works regardless of which stage of the pipeline we
    // run in.
    match &fvalue.node {
        Expr::Ident(name) | Expr::Resolved { name, .. } => name == param_name,
        _ => false,
    }
}

/// Walk `lhs`/`rhs` looking for `RecordCreate { type_name: X, fields:
/// [(_, Ident(given_name))] }` where `X` is a refinement record whose
/// carrier matches `given_type`. Returns the refined type name when
/// found, so callers can lift `given_name`'s quantifier from the
/// carrier type to the refined type. Without this, theorems would
/// emit `∀ (a : Int), … RecordCreate(a) …` where the smart-
/// constructor predicate has to be discharged from `a`'s `when`
/// clause inside the theorem type — which is exactly what the
/// previous heuristic-laden auto-proof had to work around.
pub fn refinement_lift_for_given(
    given_name: &str,
    given_type: &str,
    lhs: &Spanned<Expr>,
    rhs: &Spanned<Expr>,
    ctx: &CodegenContext,
) -> Option<String> {
    // Float carriers don't get lifted: `Int.add_comm` exists and is
    // universally provable in Lean's `Int` model, but `Float.add_
    // comm` doesn't hold across IEEE 754 — `NaN ≠ NaN` blows up
    // the universal claim. Sample-form assertions (concrete Float
    // values, no NaN in the declared `given` domain) still pass
    // through the older auto-proof shape; we only lift when the
    // underlying arithmetic has a true universal law.
    if given_type == "Float" {
        return None;
    }
    // Round-4 finding 2: return the *canonical key* the IR uses
    // (e.g. `AAA.Natural` for module-owned, bare `Natural` for
    // entry), not the bare `TypeDef.name`. Downstream
    // `strip_refinement_wrappers` / `when_is_redundant` / backend
    // emit all key off this same identifier, so two modules with
    // distinct refined `Natural` records can no longer merge under
    // a single bare name.
    let mut result: Option<String> = None;
    search_refinement_wrapper(lhs, given_name, given_type, ctx, &mut result);
    search_refinement_wrapper(rhs, given_name, given_type, ctx, &mut result);
    result
}

fn search_refinement_wrapper(
    expr: &Spanned<Expr>,
    given_name: &str,
    given_type: &str,
    ctx: &CodegenContext,
    result: &mut Option<String>,
) {
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
                && let Some((canonical_key, decl)) = find_refined_type_with_key(ctx, type_name)
                && decl.carrier_type == given_type
            {
                *result = Some(canonical_key);
                return;
            }
            for (_, v) in fields {
                search_refinement_wrapper(v, given_name, given_type, ctx, result);
            }
        }
        Expr::FnCall(callee, args) => {
            search_refinement_wrapper(callee, given_name, given_type, ctx, result);
            for a in args {
                search_refinement_wrapper(a, given_name, given_type, ctx, result);
            }
        }
        Expr::BinOp(_, l, r) => {
            search_refinement_wrapper(l, given_name, given_type, ctx, result);
            search_refinement_wrapper(r, given_name, given_type, ctx, result);
        }
        Expr::Attr(o, _) => search_refinement_wrapper(o, given_name, given_type, ctx, result),
        Expr::Neg(i) | Expr::ErrorProp(i) => {
            search_refinement_wrapper(i, given_name, given_type, ctx, result);
        }
        Expr::Match { subject, arms } => {
            search_refinement_wrapper(subject, given_name, given_type, ctx, result);
            for arm in arms {
                search_refinement_wrapper(&arm.body, given_name, given_type, ctx, result);
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for it in items {
                search_refinement_wrapper(it, given_name, given_type, ctx, result);
            }
        }
        Expr::Constructor(_, Some(arg)) => {
            search_refinement_wrapper(arg, given_name, given_type, ctx, result);
        }
        _ => {}
    }
}

/// Strip `RecordCreate { type_name: X, fields: [(_, Ident(g))] }` →
/// `Ident(g)` when `g` is in `lifted_vars` and `X` is the refined
/// type those vars were lifted to. Used after `refinement_lift_for_
/// given` decides the lift: theorem body talks about `g : Natural`
/// directly, so the `Natural(value = g)` wrapper that aver source
/// wrote becomes redundant noise.
/// Rewrite bare references to refinement-lifted given variables
/// (`a` → `a.val`) inside scalar / arithmetic contexts so the emitted
/// Lean expression typechecks against the Subtype carrier.
///
/// Background: a `verify ... law` with `given a: Int` whose body
/// wraps `a` in `Natural(value = a)` lifts the quantifier from
/// `∀ (a : Int)` to `∀ (a : Natural)`. The law's `when` clause stays
/// in the user's variable space — `when a >= 10` references the bare
/// `a`, which in Lean is now the Subtype `Natural := { v : Int //
/// v ≥ 0 }`, NOT the underlying Int. `a >= 10` then fails to
/// synthesize an `LE Natural` / `OfNat Natural 10` instance.
///
/// Transform projects bare lifted-var Idents to `.val` ONLY inside
/// `BinOp` comparators and the `Bool.and` / `Bool.or` chains the
/// parser builds for multi-`when` lines. Lifted-var refs in function
/// arguments (e.g. `add(a, b)` over `a, b : Natural`) stay bare —
/// those positions DO expect the Subtype carrier, not the underlying
/// Int. The two cases mirror `strip_refinement_wrappers`'s
/// counterpart (stripping `Natural(value = a)` → `a` in
/// function-arg positions).
pub fn project_lifted_idents_to_val(
    expr: &Spanned<Expr>,
    lifted_vars: &std::collections::HashMap<String, String>,
) -> Spanned<Expr> {
    if lifted_vars.is_empty() {
        return expr.clone();
    }
    let new_node = match &expr.node {
        Expr::BinOp(op, l, r) if is_comparator_binop(*op) => {
            let l_proj = project_lifted_ident_leaf(l, lifted_vars);
            let r_proj = project_lifted_ident_leaf(r, lifted_vars);
            Expr::BinOp(*op, Box::new(l_proj), Box::new(r_proj))
        }
        Expr::FnCall(callee, args) => {
            let name = expr_to_dotted_name(&callee.node);
            if matches!(name.as_deref(), Some("Bool.and") | Some("Bool.or")) {
                Expr::FnCall(
                    callee.clone(),
                    args.iter()
                        .map(|a| project_lifted_idents_to_val(a, lifted_vars))
                        .collect(),
                )
            } else {
                return expr.clone();
            }
        }
        _ => return expr.clone(),
    };
    Spanned::new(new_node, expr.line)
}

fn is_comparator_binop(op: crate::ast::BinOp) -> bool {
    use crate::ast::BinOp::*;
    matches!(op, Lt | Gt | Lte | Gte | Eq | Neq)
}

fn project_lifted_ident_leaf(
    expr: &Spanned<Expr>,
    lifted_vars: &std::collections::HashMap<String, String>,
) -> Spanned<Expr> {
    let target_name = match &expr.node {
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => n,
        _ => return expr.clone(),
    };
    if lifted_vars.contains_key(target_name) {
        Spanned::new(
            Expr::Attr(
                Box::new(Spanned::new(Expr::Ident(target_name.clone()), expr.line)),
                "val".to_string(),
            ),
            expr.line,
        )
    } else {
        expr.clone()
    }
}

pub fn strip_refinement_wrappers(
    expr: &Spanned<Expr>,
    lifted_vars: &std::collections::HashMap<String, String>,
    ctx: &CodegenContext,
) -> Spanned<Expr> {
    let new_node = match &expr.node {
        Expr::RecordCreate { type_name, fields } if fields.len() == 1 => {
            let (_, fvalue) = &fields[0];
            let var_name = match &fvalue.node {
                Expr::Ident(n) | Expr::Resolved { name: n, .. } => Some(n.clone()),
                _ => None,
            };
            // Round-4 finding 2: `lifted_vars[name]` holds the
            // *canonical key* (`AAA.Natural` for module-owned, bare
            // for entry). Canonicalise the AST's `type_name` against
            // the same resolver so a bare `Natural` written inside a
            // dep module strips against `AAA.Natural`, not the
            // unrelated entry-bare slot of the same name.
            let canonical_for_ast = find_refined_type_with_key(ctx, type_name).map(|(k, _)| k);
            if let Some(name) = var_name
                && let Some(refined) = lifted_vars.get(&name)
                && canonical_for_ast.as_deref() == Some(refined.as_str())
            {
                return Spanned::new(Expr::Ident(name), expr.line);
            }
            let new_fields: Vec<(String, Spanned<Expr>)> = fields
                .iter()
                .map(|(n, v)| (n.clone(), strip_refinement_wrappers(v, lifted_vars, ctx)))
                .collect();
            Expr::RecordCreate {
                type_name: type_name.clone(),
                fields: new_fields,
            }
        }
        Expr::FnCall(callee, args) => Expr::FnCall(
            Box::new(strip_refinement_wrappers(callee, lifted_vars, ctx)),
            args.iter()
                .map(|a| strip_refinement_wrappers(a, lifted_vars, ctx))
                .collect(),
        ),
        Expr::BinOp(op, l, r) => Expr::BinOp(
            *op,
            Box::new(strip_refinement_wrappers(l, lifted_vars, ctx)),
            Box::new(strip_refinement_wrappers(r, lifted_vars, ctx)),
        ),
        Expr::Attr(o, f) => Expr::Attr(
            Box::new(strip_refinement_wrappers(o, lifted_vars, ctx)),
            f.clone(),
        ),
        Expr::Neg(i) => Expr::Neg(Box::new(strip_refinement_wrappers(i, lifted_vars, ctx))),
        Expr::ErrorProp(i) => {
            Expr::ErrorProp(Box::new(strip_refinement_wrappers(i, lifted_vars, ctx)))
        }
        _ => expr.node.clone(),
    };
    Spanned::new(new_node, expr.line)
}

/// Swap a comparison BinOp's operands canonically: `a OP b` ≡ `b OP' a`
/// where OP' is the commutator-flipped op (`Lt ↔ Gt`, `Lte ↔ Gte`,
/// `Eq` and `Neq` symmetric). Returns `None` for non-comparator BinOps.
/// Used by `predicate_syntactic_eq` so `0 <= a` matches `a >= 0` for the
/// `when`-vs-refinement-invariant check.
pub fn swap_comparison_operands_op(op: &crate::ast::BinOp) -> Option<crate::ast::BinOp> {
    use crate::ast::BinOp::*;
    match op {
        Lt => Some(Gt),
        Gt => Some(Lt),
        Lte => Some(Gte),
        Gte => Some(Lte),
        Eq => Some(Eq),
        Neq => Some(Neq),
        _ => None,
    }
}

/// Canonicalize a `when`-clause comparison to a single normal direction:
/// rewrite `a > b` to `b < a` and `a >= b` to `b <= a` by swapping the
/// operands. This is a pure operand swap, NEVER a negation — a strict
/// bound stays strict (`Gt -> Lt`) and a nonstrict bound stays nonstrict
/// (`Gte -> Lte`), so it can never turn a semantically-load-bearing `r < d`
/// into `r <= d`. Any expression that is not a `>`/`>=` comparison
/// (already-canonical `<`/`<=`, `==`/`!=`, or a non-comparison predicate)
/// is returned unchanged, so canonical-direction clauses round-trip
/// byte-identically. This is a lowering-privilege normalization internal to
/// proof RECOGNITION only: the emitted theorem/lemma still renders the
/// user's original `when` surface form, so nothing user-visible is flipped.
pub fn canonicalize_comparison(e: &Spanned<Expr>) -> Spanned<Expr> {
    if let Expr::BinOp(op, l, r) = &e.node
        && matches!(op, crate::ast::BinOp::Gt | crate::ast::BinOp::Gte)
        && let Some(swapped) = swap_comparison_operands_op(op)
    {
        return Spanned::new(Expr::BinOp(swapped, r.clone(), l.clone()), e.line);
    }
    e.clone()
}

/// Structural equality on Aver predicate expressions with commutator
/// relaxation: at every `BinOp` comparator node, allow the operands +
/// operator to be swapped. Both `a >= 0` and `0 <= a` compare equal,
/// recursively. Non-comparator BinOps (`Add`, `Sub`, ...) and other
/// `Expr` variants fall through to the derived `PartialEq` on
/// `Spanned<Expr>` (which compares `.node` only — line numbers don't
/// participate). Used by the `when`-vs-refinement-invariant identity
/// check so a redundantly-written user `when` gets recognised even when
/// the operand order doesn't match the smart constructor's predicate
/// verbatim.
pub fn predicate_syntactic_eq(a: &Spanned<Expr>, b: &Spanned<Expr>) -> bool {
    match (&a.node, &b.node) {
        (Expr::BinOp(op_a, la, ra), Expr::BinOp(op_b, lb, rb)) => {
            if op_a == op_b && predicate_syntactic_eq(la, lb) && predicate_syntactic_eq(ra, rb) {
                return true;
            }
            if let Some(swapped) = swap_comparison_operands_op(op_a)
                && &swapped == op_b
                && predicate_syntactic_eq(la, rb)
                && predicate_syntactic_eq(ra, lb)
            {
                return true;
            }
            false
        }
        _ => a.node == b.node,
    }
}

/// Flatten a chain of `Bool.and(a, b)` calls into the flat list of
/// leaf predicates. Aver's `when a >= 0` / `when b >= 0` syntax folds
/// multiple `when` lines into nested `Bool.and(prev, next)` at parse
/// time (see `parser/blocks.rs`'s law-block loop), so the predicate
/// arrives at codegen as `Bool.and(Bool.and(p1, p2), p3)`. Identity
/// checks against per-given refinement invariants need the flat shape.
pub fn flatten_bool_and_conjuncts(expr: &Spanned<Expr>) -> Vec<Spanned<Expr>> {
    if let Expr::FnCall(callee, args) = &expr.node
        && args.len() == 2
        && let Some(name) = expr_to_dotted_name(&callee.node)
        && name == "Bool.and"
    {
        let mut out = flatten_bool_and_conjuncts(&args[0]);
        out.extend(flatten_bool_and_conjuncts(&args[1]));
        return out;
    }
    vec![expr.clone()]
}

/// `ResolvedExpr` mirror of [`flatten_bool_and_conjuncts`]. After the
/// resolver lifts `Bool.and(a, b)` into
/// `ResolvedExpr::Call(ResolvedCallee::Builtin("Bool.and"), args)`,
/// the same recursive split holds.
pub fn flatten_bool_and_conjuncts_resolved(
    expr: &Spanned<crate::ir::hir::ResolvedExpr>,
) -> Vec<Spanned<crate::ir::hir::ResolvedExpr>> {
    use crate::ir::hir::{ResolvedCallee, ResolvedExpr};
    if let ResolvedExpr::Call(ResolvedCallee::Builtin(name), args) = &expr.node
        && name == "Bool.and"
        && args.len() == 2
    {
        let mut out = flatten_bool_and_conjuncts_resolved(&args[0]);
        out.extend(flatten_bool_and_conjuncts_resolved(&args[1]));
        return out;
    }
    vec![expr.clone()]
}

/// `ResolvedExpr` mirror of [`predicate_syntactic_eq`].
pub fn predicate_syntactic_eq_resolved(
    a: &Spanned<crate::ir::hir::ResolvedExpr>,
    b: &Spanned<crate::ir::hir::ResolvedExpr>,
) -> bool {
    use crate::ir::hir::ResolvedExpr;
    match (&a.node, &b.node) {
        (ResolvedExpr::BinOp(op_a, la, ra), ResolvedExpr::BinOp(op_b, lb, rb)) => {
            if op_a == op_b
                && predicate_syntactic_eq_resolved(la, lb)
                && predicate_syntactic_eq_resolved(ra, rb)
            {
                return true;
            }
            if let Some(swapped) = swap_comparison_operands_op(op_a)
                && &swapped == op_b
                && predicate_syntactic_eq_resolved(la, rb)
                && predicate_syntactic_eq_resolved(ra, lb)
            {
                return true;
            }
            false
        }
        _ => a.node == b.node,
    }
}

/// `ResolvedExpr` mirror of [`substitute_ident_in_expr`]. Rewrites
/// every `Ident(from)` / `Resolved { name: from, .. }` leaf to
/// `Ident(to)` — the slot identity (if any) is dropped because the
/// substitution targets a free variable name that doesn't have a
/// slot in the resolver's local table. Used by proof-mode
/// `when`-redundancy check + smart-guard predicate substitution
/// after the IR carries pre-resolved expressions.
pub fn substitute_ident_in_resolved_expr(
    expr: &Spanned<crate::ir::hir::ResolvedExpr>,
    from: &str,
    to: &str,
) -> Spanned<crate::ir::hir::ResolvedExpr> {
    use crate::ir::hir::{ResolvedExpr, ResolvedMatchArm, ResolvedStrPart};
    let line = expr.line;
    let rec = |e: &Spanned<ResolvedExpr>| substitute_ident_in_resolved_expr(e, from, to);
    let new_node = match &expr.node {
        ResolvedExpr::Ident(name) | ResolvedExpr::Resolved { name, .. } if name == from => {
            ResolvedExpr::Ident(to.to_string())
        }
        ResolvedExpr::Literal(_) | ResolvedExpr::Ident(_) | ResolvedExpr::Resolved { .. } => {
            return expr.clone();
        }
        ResolvedExpr::Attr(obj, field) => ResolvedExpr::Attr(Box::new(rec(obj)), field.clone()),
        ResolvedExpr::Call(callee, args) => {
            ResolvedExpr::Call(callee.clone(), args.iter().map(&rec).collect())
        }
        ResolvedExpr::BinOp(op, left, right) => {
            ResolvedExpr::BinOp(*op, Box::new(rec(left)), Box::new(rec(right)))
        }
        ResolvedExpr::Neg(inner) => ResolvedExpr::Neg(Box::new(rec(inner))),
        ResolvedExpr::Match { subject, arms } => ResolvedExpr::Match {
            subject: Box::new(rec(subject)),
            arms: arms
                .iter()
                .map(|arm| ResolvedMatchArm {
                    pattern: arm.pattern.clone(),
                    body: Box::new(rec(&arm.body)),
                    binding_slots: std::sync::OnceLock::new(),
                })
                .collect(),
        },
        ResolvedExpr::Ctor(ctor, args) => {
            ResolvedExpr::Ctor(ctor.clone(), args.iter().map(&rec).collect())
        }
        ResolvedExpr::ErrorProp(inner) => ResolvedExpr::ErrorProp(Box::new(rec(inner))),
        ResolvedExpr::InterpolatedStr(parts) => ResolvedExpr::InterpolatedStr(
            parts
                .iter()
                .map(|p| match p {
                    ResolvedStrPart::Literal(_) => p.clone(),
                    ResolvedStrPart::Parsed(inner) => ResolvedStrPart::Parsed(Box::new(rec(inner))),
                })
                .collect(),
        ),
        ResolvedExpr::List(items) => ResolvedExpr::List(items.iter().map(&rec).collect()),
        ResolvedExpr::Tuple(items) => ResolvedExpr::Tuple(items.iter().map(&rec).collect()),
        ResolvedExpr::IndependentProduct(items, flag) => {
            ResolvedExpr::IndependentProduct(items.iter().map(&rec).collect(), *flag)
        }
        ResolvedExpr::MapLiteral(entries) => {
            ResolvedExpr::MapLiteral(entries.iter().map(|(k, v)| (rec(k), rec(v))).collect())
        }
        ResolvedExpr::RecordCreate {
            type_id,
            type_name,
            fields,
        } => ResolvedExpr::RecordCreate {
            type_id: *type_id,
            type_name: type_name.clone(),
            fields: fields.iter().map(|(n, v)| (n.clone(), rec(v))).collect(),
        },
        ResolvedExpr::RecordUpdate {
            type_id,
            type_name,
            base,
            updates,
        } => ResolvedExpr::RecordUpdate {
            type_id: *type_id,
            type_name: type_name.clone(),
            base: Box::new(rec(base)),
            updates: updates.iter().map(|(n, v)| (n.clone(), rec(v))).collect(),
        },
        ResolvedExpr::TailCall { target, args } => ResolvedExpr::TailCall {
            target: *target,
            args: args.iter().map(&rec).collect(),
        },
    };
    Spanned::new(new_node, line)
}

/// Walk `expr` and rename every `Ident(from)` / `Resolved { name: from
/// }` to `Ident(to)`. Lives here (not in `recursion`) because three
/// proof-mode predicate sources reach for the same substitution:
/// caller-guard extraction translates caller's local-var name to
/// callee's param name; opaque-type `when`-redundancy check translates
/// smart constructor's param name to the law's given name; future
/// callers (verify-law domain translation, etc.) will too. Single
/// definition keeps Lean and Dafny in sync.
pub fn substitute_ident_in_expr(expr: &Spanned<Expr>, from: &str, to: &str) -> Spanned<Expr> {
    use crate::ast::{MatchArm, StrPart, TailCallData};
    let line = expr.line;
    let new_node = match &expr.node {
        Expr::Ident(name) | Expr::Resolved { name, .. } if name == from => {
            Expr::Ident(to.to_string())
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } => return expr.clone(),
        Expr::Attr(obj, field) => Expr::Attr(
            Box::new(substitute_ident_in_expr(obj, from, to)),
            field.clone(),
        ),
        Expr::FnCall(callee, args) => Expr::FnCall(
            Box::new(substitute_ident_in_expr(callee, from, to)),
            args.iter()
                .map(|a| substitute_ident_in_expr(a, from, to))
                .collect(),
        ),
        Expr::BinOp(op, left, right) => Expr::BinOp(
            *op,
            Box::new(substitute_ident_in_expr(left, from, to)),
            Box::new(substitute_ident_in_expr(right, from, to)),
        ),
        Expr::Neg(inner) => Expr::Neg(Box::new(substitute_ident_in_expr(inner, from, to))),
        Expr::Match { subject, arms } => Expr::Match {
            subject: Box::new(substitute_ident_in_expr(subject, from, to)),
            arms: arms
                .iter()
                .map(|arm| MatchArm {
                    pattern: arm.pattern.clone(),
                    body: Box::new(substitute_ident_in_expr(&arm.body, from, to)),
                    binding_slots: std::sync::OnceLock::new(),
                })
                .collect(),
        },
        Expr::Constructor(name, arg) => Expr::Constructor(
            name.clone(),
            arg.as_ref()
                .map(|inner| Box::new(substitute_ident_in_expr(inner, from, to))),
        ),
        Expr::ErrorProp(inner) => {
            Expr::ErrorProp(Box::new(substitute_ident_in_expr(inner, from, to)))
        }
        Expr::InterpolatedStr(parts) => Expr::InterpolatedStr(
            parts
                .iter()
                .map(|part| match part {
                    StrPart::Literal(_) => part.clone(),
                    StrPart::Parsed(inner) => {
                        StrPart::Parsed(Box::new(substitute_ident_in_expr(inner, from, to)))
                    }
                })
                .collect(),
        ),
        Expr::List(items) => Expr::List(
            items
                .iter()
                .map(|item| substitute_ident_in_expr(item, from, to))
                .collect(),
        ),
        Expr::Tuple(items) => Expr::Tuple(
            items
                .iter()
                .map(|item| substitute_ident_in_expr(item, from, to))
                .collect(),
        ),
        Expr::IndependentProduct(items, flag) => Expr::IndependentProduct(
            items
                .iter()
                .map(|item| substitute_ident_in_expr(item, from, to))
                .collect(),
            *flag,
        ),
        Expr::MapLiteral(entries) => Expr::MapLiteral(
            entries
                .iter()
                .map(|(k, v)| {
                    (
                        substitute_ident_in_expr(k, from, to),
                        substitute_ident_in_expr(v, from, to),
                    )
                })
                .collect(),
        ),
        Expr::RecordCreate { type_name, fields } => Expr::RecordCreate {
            type_name: type_name.clone(),
            fields: fields
                .iter()
                .map(|(n, v)| (n.clone(), substitute_ident_in_expr(v, from, to)))
                .collect(),
        },
        Expr::RecordUpdate {
            type_name,
            base,
            updates,
        } => Expr::RecordUpdate {
            type_name: type_name.clone(),
            base: Box::new(substitute_ident_in_expr(base, from, to)),
            updates: updates
                .iter()
                .map(|(n, v)| (n.clone(), substitute_ident_in_expr(v, from, to)))
                .collect(),
        },
        Expr::TailCall(boxed) => Expr::TailCall(Box::new(TailCallData::new(
            boxed.target.clone(),
            boxed
                .args
                .iter()
                .map(|a| substitute_ident_in_expr(a, from, to))
                .collect(),
        ))),
    };
    Spanned::new(new_node, line)
}

/// True iff every refinement-lifted given's invariant is
/// syntactically captured by some clause of `when` (and vice versa —
/// a bijection between conjuncts). Used by both Lean and Dafny law
/// emitters to decide whether `when` is provably redundant with the
/// types of the lifted givens; if yes, drop it from the theorem
/// premise (carrier is now the type's invariant); if no, keep it so
/// the user's stronger / orthogonal predicate stays part of the claim
/// and isn't silently lost.
///
/// Same `Spanned<Expr>`-as-predicate path opaque smart constructors
/// already use — `refinement_info_for` provides the invariant, the
/// substitution maps the smart constructor's param name into the
/// given's variable space, and `predicate_syntactic_eq` does the
/// commutator-relaxed compare. No new representation, no parallel
/// emitter.
pub fn when_is_redundant_with_refinement_lifts(
    when_expr: &Spanned<Expr>,
    lifted_vars: &std::collections::HashMap<String, String>,
    ctx: &CodegenContext,
) -> bool {
    if lifted_vars.is_empty() {
        return false;
    }
    // The `when` clause arrives as raw AST (verify block parses
    // straight from source); the refinement invariants on the other
    // side now ride [`crate::ir::hir::ResolvedExpr`] (Phase E PR 12
    // Scope A). Resolve the `when` once and run the conjunct
    // comparison in resolved space so identity holds modulo the
    // resolver's canonicalisation (e.g. `Bool.and` lifts to
    // `ResolvedCallee::Builtin("Bool.and")` on both sides).
    let resolved_when = ctx.resolve_expr(when_expr, ctx.active_module_scope().as_deref());
    let when_conjuncts = flatten_bool_and_conjuncts_resolved(&resolved_when);
    // Flatten BOTH sides — IntRange-style refinement predicates carry a
    // compound `Bool.and(n >= 0, n <= 100)` invariant; without
    // flattening, a `when Bool.and(a >= 0, a <= 100)` user clause
    // (which the parser also flattens into atoms during conjunct
    // walk) would length-mismatch and keep the now-redundant
    // premise. Same flatten on both sides keeps natural / positive
    // / int_range behavior identical to pre-fix.
    // Round-3 finding 3: same canonical-IR routing as
    // `search_refinement_wrapper`. The legacy `refinement_info_for`
    // walks every module looking for a same-bare-name record, so
    // two modules with distinct predicates would share whichever
    // walked first. `find_refined_type` reads from
    // `proof_ir.refined_types` keyed by canonical `Module.Name`.
    let mut lifted_predicates: Vec<Spanned<crate::ir::hir::ResolvedExpr>> = Vec::new();
    for (given_name, refined_type) in lifted_vars {
        let Some(decl) = find_refined_type(ctx, refined_type) else {
            return false;
        };
        let substituted = substitute_ident_in_resolved_expr(
            &decl.invariant.expr,
            decl.predicate_param.as_str(),
            given_name,
        );
        lifted_predicates.extend(flatten_bool_and_conjuncts_resolved(&substituted));
    }
    if when_conjuncts.len() != lifted_predicates.len() {
        return false;
    }
    let mut matched = vec![false; lifted_predicates.len()];
    for wc in &when_conjuncts {
        let Some(idx) = (0..lifted_predicates.len())
            .find(|&i| !matched[i] && predicate_syntactic_eq_resolved(wc, &lifted_predicates[i]))
        else {
            return false;
        };
        matched[idx] = true;
    }
    true
}

fn is_err_constructor(expr: &Spanned<Expr>) -> bool {
    match &expr.node {
        Expr::Constructor(name, Some(_)) => name == "Result.Err",
        Expr::FnCall(callee, args) if args.len() == 1 => {
            matches!(
                expr_to_dotted_name(&callee.node),
                Some(name) if name == "Result.Err"
            )
        }
        _ => false,
    }
}

// Backend-neutral predicates on AST items — all three codegen backends
// (Lean, Dafny, Rust) want the same view of "is this pure?",
// "self-referencing type?", and "what's the name of this type def?".

/// A function is pure if it declares no effects and isn't `main`.
pub fn is_pure_fn(fd: &FnDef) -> bool {
    fd.effects.is_empty() && fd.name != "main"
}

/// Issue #128: do all of a law's givens carry a singleton domain?
///
/// Resolve a (possibly bare) type name to its [`RefinedTypeDecl`]
/// in `ctx.proof_ir.refined_types`. The IR is keyed by canonical
/// name (bare for entry types, `Module.Name` for module types);
/// callers see whatever the AST node carried (`Expr::RecordCreate
/// { type_name: "Natural" }` is bare, `obj.ty()` on a stamped
/// expression can be either).
///
/// Resolution order:
///   1. Direct lookup (covers exact-canonical hits and bare entry
///      types).
///   2. If no direct hit, walk `ctx.modules` for a type def with
///      this bare name and try `Module.Name` keys until one
///      resolves.
///
/// Returns `None` if no match — consistent with the previous
/// `HashMap::get` semantics. Two modules declaring the same bare
/// name (`A.Natural` + `B.Natural` with distinct predicates) each
/// get their own canonical slot now; a direct-canonical-key lookup
/// disambiguates correctly, and a bare-name lookup resolves to the
/// first module that owns the name (typechecker's
/// `cross_module_same_named_types_do_not_merge` rejects mixed
/// usage upstream, so the order-dependence is observed only when a
/// project carries one such refined type as actually-used and the
/// other as dead-code in deps).
pub fn find_refined_type<'a>(
    ctx: &'a CodegenContext,
    name: &str,
) -> Option<&'a crate::ir::proof_ir::RefinedTypeDecl> {
    find_refined_type_with_key_scoped(ctx, name, None).map(|(_, d)| d)
}

/// Canonical backend key for a nominal `Type::Named` reference.
///
/// Epic #180 Phase 6 — preferred entry point for backend
/// registries (record-field maps, eq-helper registration,
/// type-default chasing, newtype underlying lookups) that
/// previously extracted `name` directly out of
/// `Type::Named { name, .. }`. The raw `name` field is
/// source-faithful (so a bare `Shape` reference inside module
/// `A` stays as `"Shape"` even when the declaration is
/// canonically `A.Shape`). Registry routing keyed on raw `name`
/// silently collides two modules' same-bare-name types under
/// one slot — keying by the canonical form is identity-safe by
/// construction.
///
/// Resolution order:
///
/// 1. `id: Some(type_id)` (the canonical case post-typecheck) →
///    `symbol_table.type_entry(id).key.canonical()`.
/// 2. `id: None` (builtins / unresolved refs that never went
///    through `canonicalise_type`) → fall back to the source-
///    faithful `name`. The resolver stamps `id: Some(_)`
///    whenever the name binds to a declared type, so `id: None`
///    survives only for refs the symbol table doesn't know.
/// 3. Non-`Named` types (compound shapes like `List<X>`,
///    `Result<A, B>`, tuples, maps) → `None`. Backend layout
///    registries key compound types by a canonical compound
///    string (`format!("List<{}>", inner)`) the caller builds
///    itself; this helper covers only the nominal case.
pub fn backend_named_type_key(ctx: &CodegenContext, ty: &crate::types::Type) -> Option<String> {
    let crate::types::Type::Named { id, name } = ty else {
        return None;
    };
    if let Some(type_id) = id {
        return Some(ctx.symbol_table.type_entry(*type_id).key.canonical());
    }
    Some(name.clone())
}

/// Canonical backend key for a [`TypeDef`] declaration.
///
/// Epic #180 Phase 6 — paired with [`backend_named_type_key`] at
/// the registry-storage side. Backend registries insert
/// `TypeDef`s under the canonical key (`"Shape"` for entry,
/// `"A.Shape"` for module-owned) so cross-module same-bare-name
/// types occupy distinct slots and the
/// `backend_named_type_key`-derived lookup at the expression
/// side hits the right declaration.
///
/// Falls back to the bare `type_def_name(td)` when the symbol
/// table doesn't index this `TypeDef` (synthetic / builtin
/// inserts) — registry insertion paths accept both during the
/// migration window.
pub fn backend_type_def_key(ctx: &CodegenContext, td: &crate::ast::TypeDef) -> String {
    let key = type_key_for_decl(ctx, td);
    if ctx.symbol_table.type_id_of(&key).is_some() {
        key.canonical()
    } else {
        type_def_name(td).to_string()
    }
}

/// Direct `TypeId` lookup against `ProofIR.refined_types`.
///
/// Epic #180 Phase 6 — preferred entry point for callers holding a
/// typed `Type::Named { id: Some(_), .. }` reference (the
/// typechecker / resolver canonicalises Named refs with `id` when
/// the name binds to a declared type). Skips the
/// name → `TypeKey` → `TypeId` chain that
/// [`find_refined_type_with_key_scoped`] walks for bare-string
/// callers — and is identity-safe across cross-module same-bare-
/// name twins, since `TypeId` carries the module disambiguation.
///
/// Returns `None` for builtins / unresolved refs whose `id` was
/// never stamped; callers should fall back to the name-based path
/// in that case (see [`find_refined_type_for_named`] for the
/// combined helper).
pub fn find_refined_type_by_id(
    ctx: &CodegenContext,
    type_id: crate::ir::TypeId,
) -> Option<&crate::ir::proof_ir::RefinedTypeDecl> {
    ctx.proof_ir.refined_types.get(&type_id)
}

/// Refined-type lookup for a `Type::Named` ref. Prefers the
/// id-direct path when `id: Some(_)` is stamped, falls back to the
/// name-keyed resolution chain otherwise (builtins, unresolved
/// refs).
///
/// Epic #180 Phase 6 — preferred over hand-rolled
/// `Type::Named { name, .. }` destructure + `find_refined_type`
/// at sites that consume `Spanned<ResolvedExpr>.ty()`.
pub fn find_refined_type_for_named<'a>(
    ctx: &'a CodegenContext,
    named: &crate::types::Type,
) -> Option<&'a crate::ir::proof_ir::RefinedTypeDecl> {
    let crate::types::Type::Named { id, name } = named else {
        return None;
    };
    match id {
        Some(type_id) => find_refined_type_by_id(ctx, *type_id),
        None => find_refined_type(ctx, name),
    }
}

/// Scope-aware fn-contract resolver. `scope = Some(prefix)` directs
/// bare-name lookups to that module's slot before falling back to
/// entry / module-walk. Mirror of `find_refined_type_scoped` for fn
/// contracts — without it, two modules with same-bare-name
/// recursive fns silently merge under whichever module-walk hit
/// first.
///
/// Prefer `find_fn_contract_for_fn` whenever you have a `&FnDef` in
/// hand: it resolves identity by pointer-eq against
/// `ctx.modules[*].fn_defs` and avoids the bare-name footgun this
/// scoped lookup still inherits from string-based call sites.
pub fn find_fn_contract_scoped<'a>(
    ctx: &'a CodegenContext,
    name: &str,
    scope: Option<&str>,
) -> Option<&'a crate::ir::proof_ir::FnContract> {
    // Round-7: `ProofIR.fn_contracts` is keyed by opaque `FnId`
    // after the phase-E migration. Resolve `name` → `FnKey` →
    // `FnId` through the symbol table, then look up the contract.
    // Without a symbol table (legacy callers that haven't enabled
    // `run_build_symbols`) this returns `None`, which the
    // downstream emit path treats the same as "no contract" — the
    // table is part of every production build flow.
    let symbols = &ctx.symbol_table;
    let bare = name.rsplit('.').next().unwrap_or(name);
    let name_is_already_qualified = name.contains('.');
    let try_key = |key: crate::ir::FnKey| -> Option<&'a crate::ir::proof_ir::FnContract> {
        let id = symbols.fn_id_of(&key)?;
        ctx.proof_ir.fn_contracts.get(&id)
    };
    if let Some(prefix) = scope
        && !name_is_already_qualified
        && let Some(c) = try_key(crate::ir::FnKey::in_module(prefix.to_string(), bare))
    {
        return Some(c);
    }
    let direct_key =
        if name_is_already_qualified && let Some((prefix, bare_part)) = name.rsplit_once('.') {
            crate::ir::FnKey::in_module(prefix.to_string(), bare_part)
        } else {
            crate::ir::FnKey::entry(name)
        };
    if let Some(c) = try_key(direct_key) {
        return Some(c);
    }
    for m in &ctx.modules {
        for fd in &m.fn_defs {
            if fd.name == bare
                && let Some(c) = try_key(crate::ir::FnKey::in_module(m.prefix.clone(), bare))
            {
                return Some(c);
            }
        }
    }
    None
}

/// Scoped membership check.
pub fn fn_contract_exists_scoped(ctx: &CodegenContext, name: &str, scope: Option<&str>) -> bool {
    find_fn_contract_scoped(ctx, name, scope).is_some()
}

/// Round-7: build a [`crate::ir::FnKey`] from a borrowed `&FnDef`.
/// The owning module is resolved by pointer-comparison against
/// `ctx.modules[*].fn_defs` (entry items / synthesized variants /
/// `extra_fn_defs` fall through to `FnKey::entry`). This is the
/// single resolver consumers should reach for from emit code with
/// a `&FnDef` in hand — produces the typed key the IR maps store
/// instead of leaving the bare-name collision risk in place.
pub fn fn_key_for_decl(ctx: &CodegenContext, fd: &FnDef) -> crate::ir::FnKey {
    match fn_owning_scope_for(ctx, fd) {
        Some(prefix) => crate::ir::FnKey::in_module(prefix.to_string(), fd.name.clone()),
        None => crate::ir::FnKey::entry(fd.name.clone()),
    }
}

/// Round-7: build a [`crate::ir::TypeKey`] from a borrowed
/// `&TypeDef`. Same shape as [`fn_key_for_decl`] — pointer-eq
/// against `ctx.modules[*].type_defs` resolves the owning module.
pub fn type_key_for_decl(ctx: &CodegenContext, td: &TypeDef) -> crate::ir::TypeKey {
    for m in &ctx.modules {
        for t in &m.type_defs {
            if std::ptr::eq(t, td) {
                return crate::ir::TypeKey::in_module(m.prefix.clone(), type_def_name(td));
            }
        }
    }
    crate::ir::TypeKey::entry(type_def_name(td))
}

/// Round-7: resolve a (possibly bare) type name from the AST into
/// a [`crate::ir::TypeKey`]. `scope` is the emit-time current scope
/// (`Some(prefix)` inside a per-module emit loop, `None` for
/// entry). Bare names prefer the current scope, then entry, then
/// fall back to module-walk first match — mirroring
/// [`find_refined_type_scoped`]'s resolution order.
pub fn type_key_for_name(
    ctx: &CodegenContext,
    name: &str,
    scope: Option<&str>,
) -> crate::ir::TypeKey {
    let bare = name.rsplit('.').next().unwrap_or(name);
    let name_is_qualified = name.contains('.');
    if let Some(prefix) = scope
        && !name_is_qualified
    {
        for m in &ctx.modules {
            if m.prefix == prefix && m.type_defs.iter().any(|td| type_def_name(td) == bare) {
                return crate::ir::TypeKey::in_module(prefix.to_string(), bare);
            }
        }
    }
    // Entry-declared types win the bare lookup when no scope is
    // active (or when scope's module doesn't own the name).
    if !name_is_qualified && ctx.type_defs.iter().any(|td| type_def_name(td) == bare) {
        return crate::ir::TypeKey::entry(bare);
    }
    if name_is_qualified
        && let Some((prefix, bare_part)) = name.rsplit_once('.')
        && ctx.modules.iter().any(|m| m.prefix == prefix)
    {
        return crate::ir::TypeKey::in_module(prefix.to_string(), bare_part);
    }
    for m in &ctx.modules {
        if m.type_defs.iter().any(|td| type_def_name(td) == bare) {
            return crate::ir::TypeKey::in_module(m.prefix.clone(), bare);
        }
    }
    // Unresolved — fall back to entry key with the original text.
    // Callers using this typically end up with a miss in the IR
    // map, which is the same outcome as the pre-FnKey
    // `find_refined_type` returning `None`.
    crate::ir::TypeKey::entry(name)
}

/// Round-6: resolve a `&FnDef`'s owning scope by pointer comparison
/// against `ctx.modules[*].fn_defs`. Pointer-eq sidesteps the
/// bare-name collision — `&CountdownA.fn_defs[0]` and
/// `&CountdownB.fn_defs[0]` are *distinct* addresses even with
/// `fd.name = "countdown"` in both.
///
/// Returns `None` when the `fd` is not in any dep module — entry
/// items, synthesized buffered variants, and `extra_fn_defs` all
/// fall through. Callers treat `None` as "entry scope".
pub fn fn_owning_scope_for<'a>(ctx: &'a CodegenContext, fd: &FnDef) -> Option<&'a str> {
    for m in &ctx.modules {
        for f in &m.fn_defs {
            if std::ptr::eq(f, fd) {
                return Some(m.prefix.as_str());
            }
        }
    }
    None
}

/// Convenience: [`find_fn_contract_scoped`] with the scope resolved
/// by pointer-eq against `ctx.modules`. Use this from emit sites
/// that have `&FnDef` in hand — never the bare-name variant — so a
/// module-owned recursive fn always resolves to its OWN canonical
/// slot, even if another module exports a same-bare-name fn.
pub fn find_fn_contract_for_fn<'a>(
    ctx: &'a CodegenContext,
    fd: &FnDef,
) -> Option<&'a crate::ir::proof_ir::FnContract> {
    // Direct path: resolve the `&FnDef` to a `FnKey` via pointer-eq
    // scope detection, then look up the `FnId` in the symbol table
    // and the contract by ID. Cleaner than the bare-name resolver
    // chain because the source `&FnDef` already identifies its
    // owning module unambiguously.
    let symbols = &ctx.symbol_table;
    let fn_key = fn_key_for_decl(ctx, fd);
    let fn_id = symbols.fn_id_of(&fn_key)?;
    ctx.proof_ir.fn_contracts.get(&fn_id)
}

/// Membership check counterpart of [`find_fn_contract_for_fn`].
pub fn fn_contract_exists_for_fn(ctx: &CodegenContext, fd: &FnDef) -> bool {
    find_fn_contract_for_fn(ctx, fd).is_some()
}

/// Resolve `&FnDef` to the opaque [`crate::ir::FnId`] from the
/// symbol table. Pointer-eq scope detection routes module-owned
/// fns through their canonical `Module.fn` key. Returns `None` when
/// the fn isn't registered (built-ins / synthesized variants the
/// table excludes by design).
pub fn fn_id_for_decl(ctx: &CodegenContext, fd: &FnDef) -> Option<crate::ir::FnId> {
    let fn_key = fn_key_for_decl(ctx, fd);
    ctx.symbol_table.fn_id_of(&fn_key)
}

/// Resolve a dotted source-level name (`fn`, `Module.fn`) to the
/// opaque `FnId`. Used by emit code that walks AST expressions
/// (e.g. detecting calls to opaque-emitted fns inside a law body)
/// — keeps the FnKey resolution in one place instead of letting
/// each walker re-derive identity from bare strings.
pub fn fn_id_for_dotted_name(ctx: &CodegenContext, dotted: &str) -> Option<crate::ir::FnId> {
    let key = if let Some((prefix, bare)) = dotted.rsplit_once('.') {
        crate::ir::FnKey::in_module(prefix.to_string(), bare)
    } else {
        crate::ir::FnKey::entry(dotted)
    };
    ctx.symbol_table.fn_id_of(&key)
}

/// Canonical-key-aware resolver — returns `(canonical_key, decl)`
/// so consumers thread one stable identifier through refinement
/// lift / strip-wrappers / when-redundancy / backend emit instead
/// of recomputing identity from bare AST names.
pub fn find_refined_type_with_key<'a>(
    ctx: &'a CodegenContext,
    name: &str,
) -> Option<(String, &'a crate::ir::proof_ir::RefinedTypeDecl)> {
    find_refined_type_with_key_scoped(ctx, name, None)
}

/// Module-scope-aware variant of [`find_refined_type`]. When the
/// caller knows which module's emit-pass is in flight (`scope =
/// Some(module.prefix)`), the resolver tries `Module.Name` *before*
/// falling back to module-walk ordering. Two modules with same-bare
/// refined records (`A.Natural` + `B.Natural`, distinct predicates)
/// then resolve to the *current scope*'s entry — bare references
/// inside module B's emit pick up `B.Natural`, not whichever module
/// happened to populate first.
///
/// Resolution order (revised after review round 3):
///   1. If `scope = Some(prefix)` AND `name` is bare, try
///      `prefix.bare` FIRST — without this an entry-level
///      refined record with the same bare name would shadow the
///      module's own slot from inside that module's emit pass.
///   2. Direct lookup of `name` as written (covers exact-canonical
///      hits + bare-entry from entry-scope callers).
///   3. Walk modules to find owners and try each canonical key.
pub fn find_refined_type_scoped<'a>(
    ctx: &'a CodegenContext,
    name: &str,
    scope: Option<&str>,
) -> Option<&'a crate::ir::proof_ir::RefinedTypeDecl> {
    find_refined_type_with_key_scoped(ctx, name, scope).map(|(_, d)| d)
}

/// Round-4 central canonical resolver. Both `find_refined_type` and
/// `find_refined_type_scoped` reduce to this. Returns the exact
/// `(canonical_name, &decl)` pair stored in `ProofIR.refined_types`
/// so downstream code can re-key safely (no string heuristics). The
/// canonical name is the string form of the `TypeKey` the IR resolved
/// the decl from (e.g. `AAA.Natural`) — even though the map itself is
/// keyed by opaque `TypeId` after phase E2, consumers expect a
/// human-readable identifier here for diagnostics and `defmt`-style
/// canonical comparison.
///
/// `ctx.symbol_table` is always populated after phase E (pipeline
/// builds it unconditionally); the lookup returns `None` only when
/// the requested type isn't registered (built-ins / non-existent
/// names) or when the type has no `RefinedTypeDecl` slot.
pub fn find_refined_type_with_key_scoped<'a>(
    ctx: &'a CodegenContext,
    name: &str,
    scope: Option<&str>,
) -> Option<(String, &'a crate::ir::proof_ir::RefinedTypeDecl)> {
    let symbols = &ctx.symbol_table;
    let bare = name.rsplit('.').next().unwrap_or(name);
    let name_is_already_qualified = name.contains('.');
    let try_key =
        |key: crate::ir::TypeKey| -> Option<(String, &'a crate::ir::proof_ir::RefinedTypeDecl)> {
            let id = symbols.type_id_of(&key)?;
            let decl = ctx.proof_ir.refined_types.get(&id)?;
            Some((key.canonical(), decl))
        };
    if let Some(prefix) = scope
        && !name_is_already_qualified
        && let Some(hit) = try_key(crate::ir::TypeKey::in_module(prefix.to_string(), bare))
    {
        return Some(hit);
    }
    let direct_key =
        if name_is_already_qualified && let Some((prefix, bare_part)) = name.rsplit_once('.') {
            crate::ir::TypeKey::in_module(prefix.to_string(), bare_part)
        } else {
            crate::ir::TypeKey::entry(name)
        };
    if let Some(hit) = try_key(direct_key) {
        return Some(hit);
    }
    for m in &ctx.modules {
        for td in &m.type_defs {
            if type_def_name(td) == bare
                && let Some(hit) = try_key(crate::ir::TypeKey::in_module(m.prefix.clone(), bare))
            {
                return Some(hit);
            }
        }
    }
    None
}

/// Same resolution shape as [`find_refined_type`] but driven by the
/// in-flight `refined_types` map plus dep-module list — used inside
/// `proof_lower` where there is no `CodegenContext` yet. Resolves
/// `name` → `TypeKey` → opaque `TypeId` through the supplied symbol
/// table, then looks up the decl by id.
pub fn resolve_refined_type_in<'a>(
    refined_types: &'a std::collections::HashMap<
        crate::ir::TypeId,
        crate::ir::proof_ir::RefinedTypeDecl,
    >,
    symbols: &crate::ir::SymbolTable,
    modules: &[crate::codegen::ModuleInfo],
    name: &str,
) -> Option<&'a crate::ir::proof_ir::RefinedTypeDecl> {
    resolve_refined_type_in_with_key(refined_types, symbols, modules, name).map(|(_, d)| d)
}

/// Same as [`resolve_refined_type_in`] but returns the canonical
/// `TypeId` paired with the decl — used by IR-internal callers
/// (the proof-lower side `walk_for_refinement_carrier`) so they
/// thread the opaque identity through downstream comparisons
/// without re-introducing bare-name heuristics.
pub fn resolve_refined_type_in_with_key<'a>(
    refined_types: &'a std::collections::HashMap<
        crate::ir::TypeId,
        crate::ir::proof_ir::RefinedTypeDecl,
    >,
    symbols: &crate::ir::SymbolTable,
    modules: &[crate::codegen::ModuleInfo],
    name: &str,
) -> Option<(crate::ir::TypeId, &'a crate::ir::proof_ir::RefinedTypeDecl)> {
    let bare = name.rsplit('.').next().unwrap_or(name);
    let name_is_already_qualified = name.contains('.');
    let direct_key =
        if name_is_already_qualified && let Some((prefix, bare_part)) = name.rsplit_once('.') {
            crate::ir::TypeKey::in_module(prefix.to_string(), bare_part)
        } else {
            crate::ir::TypeKey::entry(name)
        };
    if let Some(id) = symbols.type_id_of(&direct_key)
        && let Some(decl) = refined_types.get(&id)
    {
        return Some((id, decl));
    }
    for m in modules {
        for td in &m.type_defs {
            if type_def_name(td) == bare {
                let canonical = crate::ir::TypeKey::in_module(m.prefix.clone(), bare);
                if let Some(id) = symbols.type_id_of(&canonical)
                    && let Some(decl) = refined_types.get(&id)
                {
                    return Some((id, decl));
                }
            }
        }
    }
    None
}

/// A `verify fn law … given a: T = [v]` with one value per given binds
/// the law to a single concrete point. Combined with
/// [`law_rhs_is_independent_of_givens`] this is the "sample-only"
/// shape — the universal form is vacuous (RHS is a constant,
/// LHS-with-one-input doesn't span anything). Asociative-style laws
/// also have singleton givens but their RHS *uses* the bound names
/// (`add a (add b c)`), so the universal form there is genuinely
/// the asociativity statement and stays.
pub fn all_givens_are_singletons(law: &crate::ast::VerifyLaw) -> bool {
    !law.givens.is_empty()
        && law.givens.iter().all(|g| match &g.domain {
            VerifyGivenDomain::Explicit(values) => values.len() == 1,
            VerifyGivenDomain::IntRange { start, end } => start == end,
        })
}

/// Issue #128: extract fn names from `proof_ir.unclassified_fns`
/// messages. The diagnostic prose is the source of truth (the
/// `UnclassifiedFn` struct doesn't carry a separate `name` field
/// today); each message starts `recursive function 'NAME' is outside
/// proof subset (...)`. Extract the quoted name so the law gate has
/// a `HashSet<String>` to test fn-call expressions against.
pub fn unclassified_fn_names(ctx: &CodegenContext) -> HashSet<String> {
    ctx.proof_ir
        .unclassified_fns
        .iter()
        .filter_map(|uf| {
            let s = &uf.message;
            let start = s.find('\'')?;
            let rest = &s[start + 1..];
            let end = rest.find('\'')?;
            Some(rest[..end].to_string())
        })
        .collect()
}

/// Issue #128: does the law call any fn the proof-mode classifier
/// rejected as "outside proof subset"?
///
/// `size`, `toSorted`, `blackDepth` and friends compile to
/// fuel-bounded helpers (`size__fuel` / `toSorted__fuel`) that the
/// `induction` tactic can't drive — even when the universal claim is
/// mathematically true (`∀ t, size t = (toSorted t).length`). The
/// auto-proof matcher emits an `induction t with …` chain that
/// leaves the goal under fuel, lake fails. Skip the universal on
/// this shape; the expanded `_sample_N` lemmas remain decidable
/// (concrete inputs unfold fuel finitely).
pub fn law_calls_unclassified_fn(
    law: &crate::ast::VerifyLaw,
    unclassified: &HashSet<String>,
) -> bool {
    if unclassified.is_empty() {
        return false;
    }
    expr_calls_named(&law.lhs, unclassified) || expr_calls_named(&law.rhs, unclassified)
}

fn expr_calls_named(expr: &Spanned<Expr>, names: &HashSet<String>) -> bool {
    match &expr.node {
        Expr::FnCall(callee, args) => {
            // Resolve the callee through the same path other emitters use —
            // covers bare (`size`), dotted (`Dep.toSorted`), and resolved
            // module-qualified shapes. The unclassified set is populated
            // from the recursion classifier's diagnostics, which today
            // emit the local `fd.name` (bare) regardless of which module
            // the fn lives in (see `recursion::detect`'s issue messages).
            // Match BOTH the resolved canonical name and its bare suffix
            // so `Dep.toSorted(xs)` lands on the gate when the set holds
            // just `toSorted` — and so a future migration to qualified
            // diagnostic names keeps working.
            let direct = expr_to_dotted_name(&callee.node)
                .map(|n| {
                    let bare = n.rsplit('.').next().unwrap_or(n.as_str());
                    names.contains(n.as_str()) || names.contains(bare)
                })
                .unwrap_or(false);
            direct
                || expr_calls_named(callee, names)
                || args.iter().any(|a| expr_calls_named(a, names))
        }
        Expr::Attr(inner, _) | Expr::ErrorProp(inner) | Expr::Neg(inner) => {
            expr_calls_named(inner, names)
        }
        Expr::BinOp(_, l, r) => expr_calls_named(l, names) || expr_calls_named(r, names),
        Expr::Match { subject, arms } => {
            expr_calls_named(subject, names)
                || arms.iter().any(|a| expr_calls_named(&a.body, names))
        }
        Expr::Constructor(_, Some(arg)) => expr_calls_named(arg, names),
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            items.iter().any(|i| expr_calls_named(i, names))
        }
        Expr::MapLiteral(entries) => entries
            .iter()
            .any(|(k, v)| expr_calls_named(k, names) || expr_calls_named(v, names)),
        Expr::RecordCreate { fields, .. } => fields.iter().any(|(_, v)| expr_calls_named(v, names)),
        Expr::RecordUpdate { base, updates, .. } => {
            expr_calls_named(base, names) || updates.iter().any(|(_, v)| expr_calls_named(v, names))
        }
        Expr::InterpolatedStr(parts) => parts.iter().any(|p| match p {
            crate::ast::StrPart::Parsed(inner) => expr_calls_named(inner, names),
            crate::ast::StrPart::Literal(_) => false,
        }),
        Expr::TailCall(boxed) => {
            let crate::ast::TailCallData { target, args, .. } = boxed.as_ref();
            names.contains(target) || args.iter().any(|a| expr_calls_named(a, names))
        }
        _ => false,
    }
}

/// The pure recursive fns that THREAD an accumulator over a USER-ADT driver —
/// a param fed a RECONSTRUCTED expression in every self-call
/// (`param_threaded_in_recursion`) while NO param is a structural `List`
/// (`single_list_structural_param_index` is `None`), the `triTR` / `qfac` /
/// `qexp` shape. These are exactly the fns the recursion classifier learned to
/// accept as structural `def`s (`single_adt_structural_param_index`): before, a
/// growing accumulator left them unclassified, so [`law_calls_unclassified_fn`]
/// bounded every law that referenced them; now they are classified and that gate
/// no longer fires. A law verified ON such a fn closes by `induction …
/// generalizing acc`, but a law that merely REFERENCES one (`fac(x) => qfac(x,
/// 1)` verified on `fac`) still can't close by simple induction — it needs the
/// inner fn's accumulator-decomposition lemma. The list-accumulator folds
/// (`qrev`) are DELIBERATELY excluded: they were always `ListStructural`
/// (classified), the fuel gate never bounded laws over them, and a consumer law
/// `myRev(x) => Lib.qrev(x, [])` genuinely closes by citing the dep's proven
/// `qrev` law — re-gating those would regress the cross-file law pool.
pub fn accumulator_fold_fn_names(ctx: &CodegenContext) -> HashSet<String> {
    use crate::codegen::recursion::detect::{
        param_decremented_in_recursion, param_threaded_in_recursion,
        single_list_structural_param_index,
    };
    // Exactly the shape the new classifier arm mints
    // (`single_adt_structural_param_index`): a recursive USER-ADT driver that
    // strictly shrinks every call, a threaded sibling accumulator, and NO
    // structural `List` param. The recursive-ADT-driver requirement is what keeps
    // this OFF fns an EARLIER classifier arm already handled — an Int-countdown /
    // floor-division / ascending fold also "threads" a non-bare-local arg
    // (`acc + n`, `n - 1`), was always classified and never fuel-bounded, and its
    // laws must keep proving universally.
    let threads_over_recursive_adt = |fd: &FnDef| -> bool {
        single_list_structural_param_index(fd).is_none()
            && (0..fd.params.len()).any(|i| param_threaded_in_recursion(fd, i))
            && fd.params.iter().enumerate().any(|(i, (_, ty))| {
                ctx_type_is_recursive_sum(ctx, ty.trim()) && param_decremented_in_recursion(fd, i)
            })
    };
    ctx.fn_defs
        .iter()
        .chain(ctx.modules.iter().flat_map(|m| m.fn_defs.iter()))
        .filter(|fd| threads_over_recursive_adt(fd))
        .map(|fd| fd.name.clone())
        .collect()
}

/// `true` iff `type_name` names a directly-recursive user sum type in `ctx`
/// (some variant field carries the type itself, bare or inside a generic) —
/// the `Nat` of a Peano accumulator fold. Mirrors `is_recursive_sum` /
/// `variants_fields_contain_type` in the Lean induction codegen.
fn ctx_type_is_recursive_sum(ctx: &CodegenContext, type_name: &str) -> bool {
    use crate::ast::TypeDef;
    ctx.type_defs
        .iter()
        .chain(ctx.modules.iter().flat_map(|m| m.type_defs.iter()))
        .any(|td| match td {
            TypeDef::Sum { name, variants, .. } if name == type_name => variants.iter().any(|v| {
                v.fields.iter().any(|f| {
                    let f = f.trim();
                    f == type_name
                        || f.contains(&format!("<{type_name}"))
                        || f.contains(&format!("{type_name}>"))
                        || f.contains(&format!(", {type_name}"))
                        || f.contains(&format!("{type_name},"))
                })
            }),
            _ => false,
        })
}

/// The user fn an accumulator fold COMBINES with — the head of the threaded
/// accumulator argument in its single self-call (`plus` of `triTR(m, plus(n,
/// acc))`, `mul` of `factTR(m, mul(n, acc))`). `None` for a non-fold or a
/// non-`FnCall` accumulator step (a builtin `+`/`*` step lowers to a `BinOp`,
/// not a user fn, and the List corner needs no algebraic helpers).
fn accfold_combine_fn(fd: &FnDef) -> Option<String> {
    use crate::ast::Expr;
    use crate::codegen::recursion::detect::{
        call_matches, collect_calls_from_body, param_threaded_in_recursion,
    };
    let acc_idx = (0..fd.params.len()).find(|&i| param_threaded_in_recursion(fd, i))?;
    let calls: Vec<Vec<&Spanned<Expr>>> = collect_calls_from_body(fd.body.as_ref())
        .into_iter()
        .filter(|(name, _)| call_matches(name, &fd.name))
        .map(|(_, args)| args)
        .collect();
    let acc_arg = calls.first()?.get(acc_idx).copied()?;
    let Expr::FnCall(callee, _) = &acc_arg.node else {
        return None;
    };
    expr_to_dotted_name(&callee.node)
}

/// `e` is `fn(arg0, arg1, …)` with each arg the bare ident named in `arg_names`.
fn call_args_are_idents(e: &Spanned<Expr>, fn_name: &str, arg_names: &[&str]) -> bool {
    let Expr::FnCall(callee, args) = &e.node else {
        return false;
    };
    expr_to_dotted_name(&callee.node).as_deref() == Some(fn_name)
        && args.len() == arg_names.len()
        && args.iter().zip(arg_names).all(
            |(a, n)| matches!(&a.node, Expr::Ident(x) | Expr::Resolved { name: x, .. } if x == n),
        )
}

/// `true` iff `vb` is a commutativity law for `combine`: `combine(a, b) =>
/// combine(b, a)` over two givens, no premise.
fn law_is_commutativity(vb: &VerifyBlock, combine: &str) -> bool {
    let crate::ast::VerifyKind::Law(law) = &vb.kind else {
        return false;
    };
    if law.givens.len() != 2 || law.when.is_some() {
        return false;
    }
    let a = law.givens[0].name.as_str();
    let b = law.givens[1].name.as_str();
    (call_args_are_idents(&law.lhs, combine, &[a, b])
        && call_args_are_idents(&law.rhs, combine, &[b, a]))
        || (call_args_are_idents(&law.lhs, combine, &[b, a])
            && call_args_are_idents(&law.rhs, combine, &[a, b]))
}

/// `true` iff `vb` is an associativity law for `combine`: `combine(combine(a,
/// b), c) => combine(a, combine(b, c))` over three givens, no premise (either
/// nesting may be on the lhs).
fn law_is_associativity(vb: &VerifyBlock, combine: &str) -> bool {
    let crate::ast::VerifyKind::Law(law) = &vb.kind else {
        return false;
    };
    if law.givens.len() != 3 || law.when.is_some() {
        return false;
    }
    let a = law.givens[0].name.as_str();
    let b = law.givens[1].name.as_str();
    let c = law.givens[2].name.as_str();
    // `combine(combine(a, b), c)` — outer call, inner first arg.
    let nested = |e: &Spanned<Expr>| -> bool {
        let Expr::FnCall(callee, args) = &e.node else {
            return false;
        };
        expr_to_dotted_name(&callee.node).as_deref() == Some(combine)
            && args.len() == 2
            && call_args_are_idents(&args[0], combine, &[a, b])
            && matches!(&args[1].node, Expr::Ident(x) | Expr::Resolved { name: x, .. } if x == c)
    };
    // `combine(a, combine(b, c))` — outer call, inner second arg.
    let flat = |e: &Spanned<Expr>| -> bool {
        let Expr::FnCall(callee, args) = &e.node else {
            return false;
        };
        expr_to_dotted_name(&callee.node).as_deref() == Some(combine)
            && args.len() == 2
            && matches!(&args[0].node, Expr::Ident(x) | Expr::Resolved { name: x, .. } if x == a)
            && call_args_are_idents(&args[1], combine, &[b, c])
    };
    (nested(&law.lhs) && flat(&law.rhs)) || (nested(&law.rhs) && flat(&law.lhs))
}

/// `true` iff a Nat accumulator-generalizing law verified ON `verified_fn` can
/// CLOSE its universal on Dafny. Unlike Lean (which bridges the user monoid fn
/// to builtin `Nat` arithmetic that Z3 knows is associative/commutative), Dafny
/// sees the user `plus` / `mul` over the ADT as opaque, so the datatype-induction
/// proof only discharges when the file ALSO provides commutativity AND
/// associativity laws for the fold's combine fn — which the generic driver
/// proves and cites. Absent those helpers the universal would ERROR (Dafny
/// cannot sorry), so it must stay sample-only. The List corner never reaches
/// here (its accumulator combine is a builtin `BinOp`, so `accfold_combine_fn`
/// is `None`, and list folds are excluded from `accumulator_fold_fn_names`).
pub fn nat_accfold_self_closeable(
    ctx: &CodegenContext,
    verified_fn: &str,
    accgen_law_name: &str,
) -> bool {
    if !accumulator_fold_fn_names(ctx).contains(verified_fn) {
        return false;
    }
    let Some(fd) = ctx.fn_def_by_name(verified_fn, ctx.active_module_scope().as_deref()) else {
        return false;
    };
    let Some(combine) = accfold_combine_fn(fd) else {
        return false;
    };
    // Restrict to an ADDITIVE monoid combine (`plus`: base arm returns the 2nd
    // param). Its commutativity / associativity prove generically by structural
    // induction, so comm+assoc helpers are SUFFICIENT to close the accGen. A
    // multiplicative combine (`mul`: base arm returns the zero constructor) also
    // needs distributivity — comm+assoc alone do not even prove there — so it
    // stays sample-only rather than ungating into a Z3 error.
    if !ctx
        .fn_def_by_name(&combine, ctx.active_module_scope().as_deref())
        .is_some_and(combine_is_additive_monoid)
    {
        return false;
    }
    // Only count comm/assoc helpers the citation engine will actually hoist into
    // the proof: laws EARLIER in source than the accGen block in THIS module
    // (`eligible_cites` is earlier-in-source, same-module). A helper declared
    // after the accGen — or in a dependency — is present but un-citable, so
    // ungating on it would emit a body missing the algebra it needs and Dafny
    // would error instead of cleanly omitting.
    let citable: Vec<&VerifyBlock> = ctx
        .items
        .iter()
        .filter_map(|i| match i {
            TopLevel::Verify(vb) => Some(vb),
            _ => None,
        })
        .take_while(|vb| {
            !matches!(&vb.kind, VerifyKind::Law(l)
                if vb.fn_name == verified_fn && l.name == accgen_law_name)
        })
        .collect();
    let has_comm = citable.iter().any(|vb| law_is_commutativity(vb, &combine));
    let has_assoc = citable.iter().any(|vb| law_is_associativity(vb, &combine));
    has_comm && has_assoc
}

/// `true` iff `fd` is a 2-param ADDITIVE-monoid fn — some base (nullary-
/// constructor) match arm returns the second parameter unchanged (`plus`'s
/// `Z -> y`). A multiplicative monoid's base arm returns the zero constructor
/// (`mul`'s `Z -> Z`) instead, so it fails this test.
fn combine_is_additive_monoid(fd: &FnDef) -> bool {
    use crate::ast::{Expr, Pattern, Stmt};
    if fd.params.len() != 2 {
        return false;
    }
    let second = fd.params[1].0.as_str();
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return false;
    };
    let Expr::Match { arms, .. } = &body.node else {
        return false;
    };
    arms.iter().any(|arm| {
        matches!(&arm.pattern, Pattern::Constructor(_, binders) if binders.is_empty())
            && matches!(&arm.body.node, Expr::Ident(n) | Expr::Resolved { name: n, .. } if n == second)
    })
}

/// `true` iff `law`'s lhs/rhs calls a recursive accumulator-threading fn OTHER
/// than `verified_fn` — the foreign-fold hazard (see
/// [`accumulator_fold_fn_names`]). Such a law can't close by simple induction
/// (it needs the inner fn's accumulator-decomposition lemma) and must stay
/// bounded, exactly as it did before the recursion classifier learned the
/// threaded-accumulator shape. The verified fn is excluded so an accumulator-
/// generalizing law verified ON the fold (`triTR(n, acc) => plus(triSpec(n),
/// acc)`) is NOT gated — Lean closes it by `induction … generalizing acc`.
/// Lean uses THIS (it has the generalizing emit); Dafny uses
/// [`law_calls_any_accumulator_fold`] because it has no such emit.
pub fn law_calls_foreign_accumulator_fold(
    ctx: &CodegenContext,
    law: &crate::ast::VerifyLaw,
    verified_fn: &str,
) -> bool {
    let foreign: HashSet<String> = accumulator_fold_fn_names(ctx)
        .into_iter()
        .filter(|n| n != verified_fn)
        .collect();
    law_calls_unclassified_fn(law, &foreign)
}

/// Dafny's bound-vs-attempt decision for a law touching an accumulator fold,
/// now that the Dafny backend HAS a datatype-induction generalizing emit. Bound
/// (sample-only) when:
///   * the law references a FOREIGN fold (needs that fold's own decomposition
///     lemma, never available — the `fac(x) => qfac(x, 1)` shape), OR
///   * the law is verified ON a Nat fold whose universal cannot close here
///     (`nat_accfold_self_closeable` is false — no commutativity / associativity
///     helper laws for the combine fn, which Z3 needs over the opaque ADT).
///
/// A self-fold WITH those helpers is NOT bound: the datatype-induction hint plus
/// the cited algebra discharge it as a real universal. The List corner is never
/// in `accumulator_fold_fn_names`, so its accumulator law always attempts (and
/// closes — builtin `Int` arithmetic needs no helper).
pub fn dafny_should_bound_accumulator_fold(
    ctx: &CodegenContext,
    law: &crate::ast::VerifyLaw,
    verified_fn: &str,
) -> bool {
    law_calls_foreign_accumulator_fold(ctx, law, verified_fn)
        || (accumulator_fold_fn_names(ctx).contains(verified_fn)
            && !nat_accfold_self_closeable(ctx, verified_fn, &law.name))
}

/// Issue #128: is the law's RHS independent of every given identifier?
///
/// `checkRight L V R => Tree.Black Empty 1 Empty` — RHS mentions no
/// given. The `∀ L V R, checkRight L V R = Tree.Black Empty 1 Empty`
/// universal is then either trivially false (`checkRight` is not a
/// constant) or vacuous; either way the per-sample lemma is the only
/// meaningful claim.
/// `add a (add b c) => add (add a b) c` — RHS uses `a`, `b`, `c`,
/// so the universal is the real asociativity theorem and is kept.
pub fn law_rhs_is_independent_of_givens(law: &crate::ast::VerifyLaw) -> bool {
    let given_names: HashSet<&str> = law.givens.iter().map(|g| g.name.as_str()).collect();
    if given_names.is_empty() {
        return true;
    }
    !expr_references_any_ident(&law.rhs, &given_names)
}

fn expr_references_any_ident(expr: &Spanned<Expr>, names: &HashSet<&str>) -> bool {
    match &expr.node {
        Expr::Ident(name) | Expr::Resolved { name, .. } => names.contains(name.as_str()),
        Expr::Attr(inner, _) | Expr::ErrorProp(inner) | Expr::Neg(inner) => {
            expr_references_any_ident(inner, names)
        }
        Expr::FnCall(callee, args) => {
            expr_references_any_ident(callee, names)
                || args.iter().any(|a| expr_references_any_ident(a, names))
        }
        Expr::BinOp(_, l, r) => {
            expr_references_any_ident(l, names) || expr_references_any_ident(r, names)
        }
        Expr::Match { subject, arms } => {
            expr_references_any_ident(subject, names)
                || arms
                    .iter()
                    .any(|a| expr_references_any_ident(&a.body, names))
        }
        Expr::Constructor(_, Some(arg)) => expr_references_any_ident(arg, names),
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            items.iter().any(|i| expr_references_any_ident(i, names))
        }
        Expr::MapLiteral(entries) => entries.iter().any(|(k, v)| {
            expr_references_any_ident(k, names) || expr_references_any_ident(v, names)
        }),
        Expr::RecordCreate { fields, .. } => fields
            .iter()
            .any(|(_, v)| expr_references_any_ident(v, names)),
        Expr::RecordUpdate { base, updates, .. } => {
            expr_references_any_ident(base, names)
                || updates
                    .iter()
                    .any(|(_, v)| expr_references_any_ident(v, names))
        }
        Expr::InterpolatedStr(parts) => parts.iter().any(|p| match p {
            crate::ast::StrPart::Parsed(inner) => expr_references_any_ident(inner, names),
            crate::ast::StrPart::Literal(_) => false,
        }),
        Expr::TailCall(boxed) => {
            let crate::ast::TailCallData { args, .. } = boxed.as_ref();
            args.iter().any(|a| expr_references_any_ident(a, names))
        }
        _ => false,
    }
}

/// Oracle v1: does the LHS of a verify-law project through Oracle's
/// runtime trace buffer?
///
/// Trace-buffer projections (`fn().trace.event(k)`,
/// `.trace.group(N).branch(M).event(K)`, `.trace.length()`,
/// `.trace.contains(...)`) are observable only at runtime — the lifted
/// proof-side fn returns the bare value, with no `.trace` field.
///
/// The detection requires *both* signals:
///   (a) an `Attr(_, "trace")` somewhere in the expression — the
///       projection lands on the trace buffer, and
///   (b) a method call from Oracle's trace API (`.event`, `.group`,
///       `.branch`, `.length`, `.contains`) on the result of (a).
///
/// Requiring both rules out a false positive where a user-defined
/// record happens to have a `trace` field (`record Log { trace:
/// String, ... }`) — `log.trace` matches (a) but not (b), so the
/// universal proof stays in normal emit. The cases-form trace block
/// emitter (`emit_verify_trace_block_proofs`) already commented out
/// the same shape; this is the law-form mirror.
pub fn law_lhs_has_trace_projection(expr: &Spanned<Expr>) -> bool {
    expr_has_trace_field(expr) && expr_has_trace_api_call(expr)
}

fn expr_has_trace_field(expr: &Spanned<Expr>) -> bool {
    match &expr.node {
        Expr::Attr(inner, field) => field == "trace" || expr_has_trace_field(inner),
        Expr::FnCall(callee, args) => {
            expr_has_trace_field(callee) || args.iter().any(expr_has_trace_field)
        }
        Expr::BinOp(_, l, r) => expr_has_trace_field(l) || expr_has_trace_field(r),
        Expr::Match { subject, arms } => {
            expr_has_trace_field(subject) || arms.iter().any(|a| expr_has_trace_field(&a.body))
        }
        Expr::ErrorProp(inner) => expr_has_trace_field(inner),
        Expr::Constructor(_, Some(arg)) => expr_has_trace_field(arg),
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            items.iter().any(expr_has_trace_field)
        }
        _ => false,
    }
}

/// Oracle's trace-buffer API surface. Every method that
/// `infer_trace_method` in the typechecker recognises as a
/// trace-only call. Keep in sync with
/// `src/types/checker/infer/expr.rs::infer_trace_method`.
const TRACE_API_METHODS: &[&str] = &["event", "group", "branch", "length", "contains", "count"];

fn expr_has_trace_api_call(expr: &Spanned<Expr>) -> bool {
    match &expr.node {
        Expr::FnCall(callee, args) => {
            let direct = matches!(
                &callee.node,
                Expr::Attr(_, method) if TRACE_API_METHODS.contains(&method.as_str())
            );
            direct || expr_has_trace_api_call(callee) || args.iter().any(expr_has_trace_api_call)
        }
        Expr::Attr(inner, _) | Expr::ErrorProp(inner) => expr_has_trace_api_call(inner),
        Expr::BinOp(_, l, r) => expr_has_trace_api_call(l) || expr_has_trace_api_call(r),
        Expr::Match { subject, arms } => {
            expr_has_trace_api_call(subject)
                || arms.iter().any(|a| expr_has_trace_api_call(&a.body))
        }
        Expr::Constructor(_, Some(arg)) => expr_has_trace_api_call(arg),
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            items.iter().any(expr_has_trace_api_call)
        }
        _ => false,
    }
}

/// The `Map` calls whose result depends on the order a map iterates in.
/// `Map.get` / `Map.has` / `Map.len` are not here: they read a map as a
/// finite function and are order-blind.
const MAP_ORDER_OBSERVERS: &[&str] = &["Map.keys", "Map.values", "Map.entries"];

/// Key types whose ordering the proof model reproduces exactly. `Int` is
/// numeric on both sides, `String` is codepoint order on both sides (Rust
/// compares UTF-8 bytes, which is the same order), `Bool` is `false` before
/// `true`. See `src/types/map.rs::compare_scalar_keys`.
const MODELLED_MAP_KEY_TYPES: &[&str] = &["Int", "String", "Bool"];

/// Why a law that observes map iteration order cannot be exported, or
/// `None` when it can.
///
/// The runtime iterates a map sorted by key, and the proof model matches
/// that — but only for the key types in [`MODELLED_MAP_KEY_TYPES`]. Two
/// families fall outside:
///
/// - `Float`, which `compare_scalar_keys` orders by raw bit pattern once a
///   `NaN` is involved, and where `-0.0` and `0.0` compare equal while being
///   two distinct keys. Neither has a faithful counterpart in the model.
/// - Anything reaching the comparator's catch-all — tuple, variant, list or
///   mixed keys — which the runtime orders by the *printed* form of the key.
///   Ordering by rendered syntax is not reconstructible from the value.
///
/// A law that observes `Map.keys` / `Map.values` / `Map.entries` over such a
/// map would be exported as a theorem about a sequence the runtime never
/// produces, so it is refused instead. This is deliberately narrower than
/// "mentions a map": a law about `Map.len` or `Map.get` is order-blind and
/// still exports, whatever the key type.
///
/// The refusal belongs here, at the law subject, and not in the term
/// emitter: measure and fuel generation emit `AverMap.entries` for their own
/// reasons, and refusing at the emitter would take recursion measures down
/// with it.
pub fn law_map_order_refusal(
    vb: &VerifyBlock,
    law: &crate::ast::VerifyLaw,
    ctx: &CodegenContext,
) -> Option<String> {
    let observers: HashSet<String> = MAP_ORDER_OBSERVERS.iter().map(|s| s.to_string()).collect();
    let observes = expr_calls_named(&law.lhs, &observers)
        || expr_calls_named(&law.rhs, &observers)
        || law
            .when
            .as_ref()
            .map(|w| expr_calls_named(w, &observers))
            .unwrap_or(false);
    if !observes {
        return None;
    }

    let mut key_types: Vec<String> = Vec::new();
    for given in &law.givens {
        collect_map_key_types(&given.type_name, &mut key_types);
    }
    let scope = ctx.active_module_scope();
    let mut subjects: Vec<String> = vec![vb.fn_name.clone()];
    collect_called_names(&law.lhs, &mut subjects);
    collect_called_names(&law.rhs, &mut subjects);
    if let Some(when) = law.when.as_ref() {
        collect_called_names(when, &mut subjects);
    }
    for name in &subjects {
        let Some(fd) = ctx.fn_def_by_name(name, scope.as_deref()) else {
            continue;
        };
        for (_, param_type) in &fd.params {
            collect_map_key_types(param_type, &mut key_types);
        }
        collect_map_key_types(&fd.return_type, &mut key_types);
    }

    if key_types.is_empty() {
        return Some(
            "the map's key type is not visible from the law's givens or from any signature it \
             reaches, and the proof model only reproduces the runtime's iteration order for Int, \
             String and Bool keys"
                .to_string(),
        );
    }
    let unmodelled = key_types
        .iter()
        .find(|k| !MODELLED_MAP_KEY_TYPES.contains(&k.as_str()))?;
    if unmodelled == "Float" {
        return Some(
            "the runtime orders Float keys by their raw bit pattern once a NaN is involved, and \
             the proof model has no faithful counterpart for that"
                .to_string(),
        );
    }
    Some(format!(
        "the runtime orders {} keys by their printed representation, which the proof model cannot \
         reconstruct from the value",
        unmodelled
    ))
}

/// Append the key type of every `Map<K, V>` written anywhere inside a type
/// annotation — including nested ones, so `List<Map<Int, String>>` yields
/// `Int` and `Map<String, Map<Bool, Int>>` yields both `String` and `Bool`.
fn collect_map_key_types(type_name: &str, out: &mut Vec<String>) {
    let bytes = type_name.as_bytes();
    let mut idx = 0usize;
    while let Some(found) = type_name[idx..].find("Map<") {
        let start = idx + found;
        idx = start + 4;
        // `MyMap<..>` is a different type; only a standalone `Map` counts.
        if start > 0 {
            let prev = bytes[start - 1];
            if prev.is_ascii_alphanumeric() || prev == b'_' || prev == b'.' {
                continue;
            }
        }
        let mut depth = 1usize;
        let mut split = None;
        let mut cursor = idx;
        for (offset, ch) in type_name[idx..].char_indices() {
            match ch {
                '<' => depth += 1,
                '>' => {
                    depth -= 1;
                    if depth == 0 {
                        cursor = idx + offset;
                        break;
                    }
                }
                ',' if depth == 1 && split.is_none() => split = Some(idx + offset),
                _ => {}
            }
        }
        let key_end = split.unwrap_or(cursor);
        let key = type_name[idx..key_end].trim();
        if !key.is_empty() {
            out.push(key.to_string());
        }
    }
}

/// Every dotted or bare callee name appearing in an expression.
fn collect_called_names(expr: &Spanned<Expr>, out: &mut Vec<String>) {
    match &expr.node {
        Expr::FnCall(callee, args) => {
            if let Some(name) = expr_to_dotted_name(&callee.node) {
                out.push(name);
            }
            collect_called_names(callee, out);
            for a in args {
                collect_called_names(a, out);
            }
        }
        Expr::Attr(inner, _) | Expr::ErrorProp(inner) | Expr::Neg(inner) => {
            collect_called_names(inner, out)
        }
        Expr::BinOp(_, l, r) => {
            collect_called_names(l, out);
            collect_called_names(r, out);
        }
        Expr::Match { subject, arms } => {
            collect_called_names(subject, out);
            for a in arms {
                collect_called_names(&a.body, out);
            }
        }
        Expr::Constructor(_, Some(arg)) => collect_called_names(arg, out),
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for i in items {
                collect_called_names(i, out);
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries {
                collect_called_names(k, out);
                collect_called_names(v, out);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, v) in fields {
                collect_called_names(v, out);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_called_names(base, out);
            for (_, v) in updates {
                collect_called_names(v, out);
            }
        }
        _ => {}
    }
}

/// True when the type definition mentions its own name somewhere in a
/// field or variant payload (recursive ADT).
pub fn is_recursive_type_def(td: &TypeDef) -> bool {
    match td {
        TypeDef::Sum { name, variants, .. } => is_recursive_sum(name, variants),
        TypeDef::Product { name, fields, .. } => is_recursive_product(name, fields),
    }
}

/// The declared name of a type definition.
pub fn type_def_name(td: &TypeDef) -> &str {
    match td {
        TypeDef::Sum { name, .. } | TypeDef::Product { name, .. } => name,
    }
}

/// Granular variant of [`is_recursive_type_def`] taking a sum's
/// `(name, variants)` split — some backends already have the parts
/// separated and don't want to rebuild a `TypeDef` just to query.
pub fn is_recursive_sum(name: &str, variants: &[TypeVariant]) -> bool {
    variants
        .iter()
        .any(|v| v.fields.iter().any(|f| type_ref_contains(f, name)))
}

/// Granular variant of [`is_recursive_type_def`] for products.
pub fn is_recursive_product(name: &str, fields: &[(String, String)]) -> bool {
    fields.iter().any(|(_, ty)| type_ref_contains(ty, name))
}

fn type_ref_contains(annotation: &str, type_name: &str) -> bool {
    // Direct match or any generic position: List<Foo>, Option<Foo>,
    // Map<K, Foo>, (Foo, Bar), etc.
    annotation == type_name
        || annotation.contains(&format!("<{}", type_name))
        || annotation.contains(&format!("{}>", type_name))
        || annotation.contains(&format!(", {}", type_name))
        || annotation.contains(&format!("{},", type_name))
}

/// Check if a name is a user-defined type (sum or product), including modules.
pub(crate) fn is_user_type(name: &str, ctx: &CodegenContext) -> bool {
    let check_td = |td: &TypeDef| match td {
        TypeDef::Sum { name: n, .. } => n == name,
        TypeDef::Product { name: n, .. } => n == name,
    };
    ctx.type_defs.iter().any(check_td)
        || ctx.modules.iter().any(|m| m.type_defs.iter().any(check_td))
}

/// Resolve a module-qualified dotted name to `(module_prefix, local_suffix)`.
/// Example: `Models.User.nameById` -> `("Models.User", "nameById")`.
pub(crate) fn resolve_module_call<'a>(
    dotted_name: &'a str,
    ctx: &'a CodegenContext,
) -> Option<(&'a str, &'a str)> {
    let mut best: Option<&str> = None;
    for prefix in &ctx.module_prefixes {
        let dotted_prefix = format!("{}.", prefix);
        if dotted_name.starts_with(&dotted_prefix) && best.is_none_or(|b| prefix.len() > b.len()) {
            best = Some(prefix.as_str());
        }
    }
    best.map(|prefix| (prefix, &dotted_name[prefix.len() + 1..]))
}

pub(crate) fn module_prefix_to_rust_segments(prefix: &str) -> Vec<String> {
    prefix.split('.').map(module_segment_to_rust).collect()
}

/// Translate an Aver module prefix (`Models.User`, `Combat`) into a relative
/// filesystem path stem with `/` separators. Lean's path-as-module convention
/// and Dafny's `include "..."` paths both use this — same shape, no
/// backend-specific escaping.
pub(crate) fn module_prefix_to_filename(prefix: &str) -> String {
    prefix.replace('.', "/")
}

/// Effects declared in fn signatures, preserving the distinction
/// between namespace-level and method-level declarations.
///
/// - `bare_namespaces`: e.g. `! [Console]` ⇒ permits every classified
///   `Console.*` method.
/// - `methods`: e.g. `! [Console.print]` ⇒ permits only that one
///   specific method (not the whole namespace).
///
/// Aver source allows both forms — we keep them separate so a single
/// `! [Random.int]` does not pull every `Random.*` method into the
/// trust header (or any other consumer that maps method-by-method).
pub(crate) struct DeclaredEffects {
    pub bare_namespaces: HashSet<String>,
    pub methods: HashSet<String>,
}

impl DeclaredEffects {
    /// True if `c_method` (e.g. `"Random.int"`) is declared either as
    /// an explicit method or via its bare namespace (`"Random"`).
    pub fn includes(&self, c_method: &str) -> bool {
        if self.methods.contains(c_method) {
            return true;
        }
        if let Some((ns, _)) = c_method.split_once('.') {
            return self.bare_namespaces.contains(ns);
        }
        false
    }
}

/// Collect declared effects across `ctx` (entry + dependent modules).
/// Single source of truth for the proof-side trust header and the
/// runtime-dependency detector in the Rust backend.
pub(crate) fn collect_declared_effects(ctx: &CodegenContext) -> DeclaredEffects {
    let mut bare_namespaces: HashSet<String> = HashSet::new();
    let mut methods: HashSet<String> = HashSet::new();
    let mut record = |effect: &str| {
        if effect.contains('.') {
            methods.insert(effect.to_string());
        } else {
            bare_namespaces.insert(effect.to_string());
        }
    };
    for item in &ctx.items {
        if let TopLevel::FnDef(fd) = item {
            for eff in &fd.effects {
                record(&eff.node);
            }
        }
    }
    for module in &ctx.modules {
        for fd in &module.fn_defs {
            for eff in &fd.effects {
                record(&eff.node);
            }
        }
    }
    DeclaredEffects {
        bare_namespaces,
        methods,
    }
}

/// Basename for the entry file emitted by Lean / Dafny. Prefer the
/// source-declared module name (`module Foo` → `Foo`) so the entry
/// file's name matches what the user wrote; fall back to a capitalised
/// project name when no `module` declaration is present. Lake's
/// path-as-module-name convention forces this for Lean — Dafny doesn't
/// strictly need it but the same basename keeps the two backends
/// aligned (no more `playground.dfy` vs `OracleTrace.lean`).
pub fn entry_basename(ctx: &CodegenContext) -> String {
    ctx.items
        .iter()
        .find_map(|item| match item {
            TopLevel::Module(m) => Some(m.name.clone()),
            _ => None,
        })
        .unwrap_or_else(|| {
            let mut chars = ctx.project_name.chars();
            match chars.next() {
                None => String::new(),
                Some(c) => c.to_uppercase().chain(chars).collect(),
            }
        })
}

/// Stable identity key for a verify block's case-index space, shared by the
/// Lean emitter's per-block case counters and the CLI's VM ground-truth
/// collection (`CodegenContext::sample_expected`).
///
/// Plain `verify <fn>` blocks share one index space per fn — the VM verify
/// runner merges them (`checker::merge_verify_blocks`) and the Lean emitter
/// continues the counter across blocks with the same key — while each
/// `verify <fn> law <name>` block keeps its own space. Both sides MUST derive
/// keys from this one function: index drift between them would associate a
/// case with another case's ground-truth value.
pub fn verify_block_counter_key(vb: &crate::ast::VerifyBlock) -> String {
    match &vb.kind {
        crate::ast::VerifyKind::Cases => format!("fn:{}", vb.fn_name),
        crate::ast::VerifyKind::Law(law) => format!("law:{}::{}", vb.fn_name, law.name),
    }
}

// Round-6 finding 3: the legacy `fn_owning_scope(ctx) ->
// HashMap<String, String>` collided on bare names — two modules
// declaring the same-named fn (`AAA.foo` + `BBB.foo`) lost one
// scope to last-insert-wins. Replaced by [`fn_owning_scope_for`]
// (pointer-eq lookup) at the only remaining callsite. The legacy
// helper is removed; if a future caller needs a bulk pre-computed
// map, build one keyed by `(prefix, name)` or by the canonical
// `Module.fn` string rather than the bare `fd.name`.

pub(crate) fn module_prefix_to_rust_path(prefix: &str) -> String {
    format!(
        "crate::aver_generated::{}",
        module_prefix_to_rust_segments(prefix).join("::")
    )
}

fn module_segment_to_rust(segment: &str) -> String {
    let chars = segment.chars().collect::<Vec<_>>();
    let mut out = String::new();

    for (idx, ch) in chars.iter().enumerate() {
        if ch.is_ascii_alphanumeric() {
            if ch.is_ascii_uppercase() {
                let prev_is_lower_or_digit = idx > 0
                    && (chars[idx - 1].is_ascii_lowercase() || chars[idx - 1].is_ascii_digit());
                let next_is_lower = chars
                    .get(idx + 1)
                    .is_some_and(|next| next.is_ascii_lowercase());
                if idx > 0 && (prev_is_lower_or_digit || next_is_lower) && !out.ends_with('_') {
                    out.push('_');
                }
                out.push(ch.to_ascii_lowercase());
            } else {
                out.push(ch.to_ascii_lowercase());
            }
        } else if !out.ends_with('_') {
            out.push('_');
        }
    }

    let trimmed = out.trim_matches('_');
    let mut normalized = if trimmed.is_empty() {
        "module".to_string()
    } else {
        trimmed.to_string()
    };

    if matches!(
        normalized.as_str(),
        "as" | "break"
            | "const"
            | "continue"
            | "crate"
            | "else"
            | "enum"
            | "extern"
            | "false"
            | "fn"
            | "for"
            | "if"
            | "impl"
            | "in"
            | "let"
            | "loop"
            | "match"
            | "mod"
            | "move"
            | "mut"
            | "pub"
            | "ref"
            | "return"
            | "self"
            | "Self"
            | "static"
            | "struct"
            | "super"
            | "trait"
            | "true"
            | "type"
            | "unsafe"
            | "use"
            | "where"
            | "while"
    ) {
        normalized.push_str("_mod");
    }

    normalized
}

/// Split a type annotation string at top-level delimiters (not inside `<>` or `()`).
///
/// Used by multiple backends to parse Aver type annotation strings like
/// `"Map<String, List<Int>>"` or `"(String, Int)"`.
pub(crate) fn split_type_params(s: &str, delim: char) -> Vec<String> {
    let mut parts = Vec::new();
    let mut depth = 0usize;
    let mut current = String::new();
    for ch in s.chars() {
        match ch {
            '<' | '(' => {
                depth += 1;
                current.push(ch);
            }
            '>' | ')' => {
                depth = depth.saturating_sub(1);
                current.push(ch);
            }
            _ if ch == delim && depth == 0 => {
                parts.push(current.trim().to_string());
                current.clear();
            }
            _ => current.push(ch),
        }
    }
    let rest = current.trim().to_string();
    if !rest.is_empty() {
        parts.push(rest);
    }
    parts
}

/// Escape a string literal for target languages that use C-style escapes.
/// Handles `\\`, `\"`, `\n`, `\r`, `\t`, `\0`,
/// and generic control characters as `\xHH` (Lean/Rust) or `\uHHHH` (Dafny).
///
/// Use `unicode_escapes = true` for Dafny (which needs `\uHHHH`),
/// `false` for Lean/Rust (which accept `\xHH`).
pub(crate) fn escape_string_literal_ext(s: &str, unicode_escapes: bool) -> String {
    let mut out = String::with_capacity(s.len());
    for ch in s.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\0' => out.push_str("\\0"),
            c if c.is_control() => {
                if unicode_escapes {
                    // Dafny 4+ with Unicode chars enabled: \U{HHHHHH}
                    out.push_str(&format!("\\U{{{:06x}}}", c as u32));
                } else {
                    out.push_str(&format!("\\x{:02x}", c as u32));
                }
            }
            c => out.push(c),
        }
    }
    out
}

/// Convenience: escape with `\xHH` for control chars (Lean, Rust).
pub(crate) fn escape_string_literal(s: &str) -> String {
    escape_string_literal_ext(s, false)
}

/// Convenience: escape with `\u{HHHH}` for control chars (Dafny).
pub(crate) fn escape_string_literal_unicode(s: &str) -> String {
    escape_string_literal_ext(s, true)
}

/// Parse an Aver type annotation string into the internal `Type` enum.
///
/// Thin wrapper around `types::parse_type_str` for use in codegen modules.
pub(crate) fn parse_type_annotation(ann: &str) -> Type {
    crate::types::parse_type_str(ann)
}

/// Check if a `Type` represents a set pattern: `Map<T, Unit>`.
///
/// Aver has no dedicated `Set` type — the idiomatic way to express a set
/// is `Map<T, Unit>`. Codegen backends can lower this to the target
/// language's native set type (Dafny `set<T>`, Lean `Finset T`, etc.).
pub(crate) fn is_set_type(ty: &Type) -> bool {
    matches!(ty, Type::Map(_, v) if matches!(v.as_ref(), Type::Unit))
}

/// Check if a type annotation string represents a set (`Map<T, Unit>`).
pub(crate) fn is_set_annotation(ann: &str) -> bool {
    is_set_type(&parse_type_annotation(ann))
}

/// Resolved-form mirror of the historical AST helper. Same Phase-E shape
/// (`ResolvedExpr::Literal(Unit)`); used by migrated backends.
pub(crate) fn is_unit_expr_resolved(expr: &crate::ir::hir::ResolvedExpr) -> bool {
    matches!(
        expr,
        crate::ir::hir::ResolvedExpr::Literal(crate::ast::Literal::Unit)
    )
}

/// Escape an Aver identifier if it collides with a target language reserved word.
///
/// `affix` is appended as a suffix (e.g. `"_"` for Dafny, `"'"` for Lean).
/// For prefix escaping (e.g. Rust `r#`), use [`escape_reserved_word_prefix`].
pub(crate) fn escape_reserved_word(name: &str, reserved: &[&str], suffix: &str) -> String {
    if reserved.contains(&name) {
        format!("{}{}", name, suffix)
    } else {
        name.to_string()
    }
}

/// Like [`escape_reserved_word`] but prepends a prefix instead of appending a suffix.
/// Used for Rust's `r#keyword` raw identifier syntax.
pub(crate) fn escape_reserved_word_prefix(name: &str, reserved: &[&str], prefix: &str) -> String {
    if reserved.contains(&name) {
        format!("{}{}", prefix, name)
    } else {
        name.to_string()
    }
}

/// Convert first character of a string to lowercase.
///
/// Used when converting PascalCase type/variant names to camelCase identifiers.
pub(crate) fn to_lower_first(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(c) => c.to_lowercase().to_string() + chars.as_str(),
    }
}

/// Convert an attribute chain into dotted name.
/// Example: `Console.print` -> `Some("Console.print")`.
pub(crate) fn expr_to_dotted_name(expr: &Expr) -> Option<String> {
    crate::ir::expr_to_dotted_name(expr)
}

/// Oracle v1: how to materialise the oracle argument for an effectful
/// fn call in a law body.
///
/// - `LemmaBinding` — use the lemma-local identifier (`rnd`), matching
///   the `given` name. Correct for the universal lemma body.
/// - `SampleValue` — use the first Explicit domain value (the stub
///   fn's identifier, e.g. `stubConst`). Correct for the concrete
///   sample assertions where there's no lemma binding in scope and a
///   single domain value.
/// - `SampleCaseBinding(case_bindings)` — use the per-case binding
///   value (by `given.name`). Correct for sample theorems when the
///   domain has multiple values and each case substitutes a
///   different one (`given stub: Http.get = [httpDown, httpOk]`).
#[derive(Debug, Clone)]
pub(crate) enum OracleInjectionMode<'a> {
    LemmaBinding,
    /// Like `LemmaBinding` but project through the subtype carrier
    /// for classified `Generative` / `GenerativeOutput` effect-givens
    /// — `g.name` becomes `g.name.val` in the rewritten expression.
    /// Used by the Lean backend where lifted theorems quantify over
    /// the constrained subtype (`RandomIntInBounds`) instead of the
    /// plain function type, so call sites need to peel the carrier.
    /// Dafny stays on `LemmaBinding` (no first-class subtype types
    /// over functions); the bound is enforced via `requires` on the
    /// emitted lemma instead.
    LemmaBindingProjected,
    #[allow(dead_code)]
    SampleValue,
    SampleCaseBinding(&'a [(String, crate::ast::Spanned<Expr>)]),
}

/// Oracle v1: rewrite any call to an effectful fn in a law body so
/// it targets the lifted signature — prepend `BranchPath.root()` (for
/// generative / gen+output effects) plus one argument per classified
/// non-output effect in the callee's signature.
///
/// Backend-agnostic — operates on AST + `CodegenContext`. Both the
/// Dafny and Lean backends call this before emitting the law body so
/// the law statement matches the lifted fn shape emitted alongside.
pub(crate) fn rewrite_effectful_calls_in_law<'fd, F>(
    expr: &crate::ast::Spanned<Expr>,
    law: &crate::ast::VerifyLaw,
    find_fn_def: F,
    mode: OracleInjectionMode,
) -> crate::ast::Spanned<Expr>
where
    F: Fn(&str) -> Option<&'fd crate::ast::FnDef> + Copy,
{
    use crate::ast::{Spanned, VerifyGivenDomain};

    let injection_by_effect: std::collections::HashMap<String, Spanned<Expr>> = law
        .givens
        .iter()
        .filter_map(|g| {
            let arg_expr = match &mode {
                OracleInjectionMode::LemmaBinding => {
                    Spanned::new(Expr::Ident(g.name.clone()), expr.line)
                }
                OracleInjectionMode::LemmaBindingProjected => {
                    // Inject the bare oracle name; the post-rewrite pass
                    // `project_oracle_direct_calls` walks the whole
                    // expression once and lifts every reference to a
                    // subtype-carried oracle (callee, arg, comparison
                    // LHS, ...) through `.val`. Doing the projection
                    // here as well would compound — `Attr(Attr(rng,
                    // val), val)` for refs the injection wraps.
                    Spanned::new(Expr::Ident(g.name.clone()), expr.line)
                }
                OracleInjectionMode::SampleValue => match &g.domain {
                    VerifyGivenDomain::Explicit(vals) => vals.first().cloned()?,
                    _ => return None,
                },
                OracleInjectionMode::SampleCaseBinding(case_bindings) => case_bindings
                    .iter()
                    .find(|(name, _)| name == &g.name)
                    .map(|(_, v)| v.clone())?,
            };
            Some((g.type_name.clone(), arg_expr))
        })
        .collect();
    let rewritten = rewrite_effectful_call(expr, &injection_by_effect, find_fn_def);

    // For `LemmaBindingProjected`, oracle bindings live as subtypes
    // (`RandomIntInBounds` etc.); direct calls `rng(path, n, min, max)`
    // in the law body need to peel `.val` off the carrier. Walk the
    // rewritten expression once more and rewrite direct
    // `FnCall(Ident(<oracle_name>), args)` shapes into
    // `FnCall(Attr(Ident(<oracle_name>), "val"), args)` for every
    // classified Generative-shape given. Other modes leave the body
    // alone.
    if matches!(mode, OracleInjectionMode::LemmaBindingProjected) {
        // Only givens whose effect has a bounded-subtype carrier
        // (`RandomIntInBounds` / `RandomFloatInUnit` /
        // `TimeUnixMsNonneg`) get `.val` projection. `EffectDimension::
        // Generative*` covers more effects than that — e.g.
        // `Disk.writeText` is `GenerativeOutput` but the quant param
        // stays a plain function in both Lean and Dafny because there's
        // no bound to encode. Projecting `.val` on a plain function
        // emits `write.val` on something with no `val` field and the
        // proof rejects it.
        let oracle_names: std::collections::HashSet<String> = law
            .givens
            .iter()
            .filter(|g| crate::types::checker::oracle_subtypes::has_bounded_subtype(&g.type_name))
            .map(|g| g.name.clone())
            .collect();
        if !oracle_names.is_empty() {
            return project_oracle_direct_calls(&rewritten, &oracle_names);
        }
    }
    rewritten
}

/// Rewrite every reference to a subtype-carried oracle so the surrounding
/// expression type-checks against the carrier:
///
/// * Bare ident `rng` → `rng.val` (when `rng` is passed as an argument
///   to a helper, or compared with `=` in a domain-premise / `when`
///   clause).
/// * Direct call `rng(args...)` → `rng.val(args...)` (the underlying
///   function call site).
///
/// Recursive over the whole expression. In nested expressions like
/// `Result.Ok(rng(p, n, 1, 6))` or `pairSpec(BranchPath.Root, rng)`,
/// each oracle reference is projected exactly once.
fn project_oracle_direct_calls(
    expr: &crate::ast::Spanned<Expr>,
    oracle_names: &std::collections::HashSet<String>,
) -> crate::ast::Spanned<Expr> {
    use crate::ast::Spanned;
    let line = expr.line;
    let project_ident = |name: &str, line: usize| -> Spanned<Expr> {
        Spanned::new(
            Expr::Attr(
                Box::new(Spanned::new(Expr::Ident(name.to_string()), line)),
                "val".to_string(),
            ),
            line,
        )
    };
    let new_node = match &expr.node {
        // Bare ident reference to a subtype-carried oracle — project.
        // Catches helper-call args (`pairSpec(root, rng)`) and any
        // other position where the oracle name appears as a value.
        Expr::Ident(name) if oracle_names.contains(name) => {
            return project_ident(name, line);
        }
        Expr::FnCall(callee, args) => {
            let new_args: Vec<Spanned<Expr>> = args
                .iter()
                .map(|a| project_oracle_direct_calls(a, oracle_names))
                .collect();
            // `rng(...)` direct call — project the callee.
            let new_callee = if let Expr::Ident(name) = &callee.node
                && oracle_names.contains(name)
            {
                project_ident(name, callee.line)
            } else {
                project_oracle_direct_calls(callee, oracle_names)
            };
            Expr::FnCall(Box::new(new_callee), new_args)
        }
        Expr::Constructor(name, Some(arg)) => Expr::Constructor(
            name.clone(),
            Some(Box::new(project_oracle_direct_calls(arg, oracle_names))),
        ),
        Expr::Attr(obj, field) => Expr::Attr(
            Box::new(project_oracle_direct_calls(obj, oracle_names)),
            field.clone(),
        ),
        Expr::BinOp(op, l, r) => Expr::BinOp(
            *op,
            Box::new(project_oracle_direct_calls(l, oracle_names)),
            Box::new(project_oracle_direct_calls(r, oracle_names)),
        ),
        other => other.clone(),
    };
    Spanned::new(new_node, line)
}

fn rewrite_effectful_call<'fd, F>(
    expr: &crate::ast::Spanned<Expr>,
    injection_by_effect: &std::collections::HashMap<String, crate::ast::Spanned<Expr>>,
    find_fn_def: F,
) -> crate::ast::Spanned<Expr>
where
    F: Fn(&str) -> Option<&'fd crate::ast::FnDef> + Copy,
{
    use crate::ast::Spanned;
    use crate::types::checker::effect_classification::{EffectDimension, classify};

    match &expr.node {
        Expr::FnCall(callee, args) => {
            let rewritten_args: Vec<Spanned<Expr>> = args
                .iter()
                .map(|a| rewrite_effectful_call(a, injection_by_effect, find_fn_def))
                .collect();
            let rewritten_callee = Box::new(rewrite_effectful_call(
                callee,
                injection_by_effect,
                find_fn_def,
            ));

            let callee_name = match &callee.node {
                Expr::Ident(name) => Some(name.clone()),
                Expr::Resolved { name, .. } => Some(name.clone()),
                _ => None,
            };

            if let Some(name) = callee_name
                && let Some(fd) = find_fn_def(&name)
                && !fd.effects.is_empty()
                && fd
                    .effects
                    .iter()
                    .all(|e| crate::types::checker::effect_classification::is_classified(&e.node))
            {
                let mut injected: Vec<Spanned<Expr>> = Vec::new();
                let needs_path = fd.effects.iter().any(|e| {
                    matches!(
                        classify(&e.node).map(|c| c.dimension),
                        Some(EffectDimension::Generative | EffectDimension::GenerativeOutput)
                    )
                });
                if needs_path {
                    injected.push(Spanned::new(
                        // `BranchPath.Root` — nullary value
                        // constructor (PascalCase, no parens),
                        // symmetric with `Option.None`.
                        Expr::Attr(
                            Box::new(Spanned::new(
                                Expr::Ident("BranchPath".to_string()),
                                expr.line,
                            )),
                            "Root".to_string(),
                        ),
                        expr.line,
                    ));
                }
                let mut seen = std::collections::HashSet::new();
                for e in &fd.effects {
                    if !seen.insert(e.node.clone()) {
                        continue;
                    }
                    let Some(c) = classify(&e.node) else { continue };
                    if matches!(c.dimension, EffectDimension::Output) {
                        continue;
                    }
                    if let Some(inj) = injection_by_effect.get(&e.node) {
                        injected.push(inj.clone());
                    }
                }
                injected.extend(rewritten_args);
                return Spanned::new(Expr::FnCall(rewritten_callee, injected), expr.line);
            }

            Spanned::new(Expr::FnCall(rewritten_callee, rewritten_args), expr.line)
        }
        Expr::BinOp(op, l, r) => Spanned::new(
            Expr::BinOp(
                *op,
                Box::new(rewrite_effectful_call(l, injection_by_effect, find_fn_def)),
                Box::new(rewrite_effectful_call(r, injection_by_effect, find_fn_def)),
            ),
            expr.line,
        ),
        Expr::Tuple(items) => Spanned::new(
            Expr::Tuple(
                items
                    .iter()
                    .map(|i| rewrite_effectful_call(i, injection_by_effect, find_fn_def))
                    .collect(),
            ),
            expr.line,
        ),
        _ => expr.clone(),
    }
}

/// Oracle v1: set of user fn names that are reachable from any verify
/// block — directly (`verify f ...`) or through the call graph (fn
/// body of a reachable fn mentions them). Used by proof backends to
/// skip emission of effectful fns that nobody verifies. Dead code in
/// a proof output isn't just ugly — a non-terminating effectful fn
/// (e.g. a REPL loop) will make Lean reject the whole module because
/// it can't prove termination for a fn with no decreasing argument.
/// If the user never asked for a proof about that fn, don't force
/// the backend to invent one.
pub(crate) fn verify_reachable_fn_names(items: &[TopLevel]) -> HashSet<String> {
    let mut reachable: HashSet<String> = HashSet::new();
    for item in items {
        if let TopLevel::Verify(vb) = item {
            collect_verify_block_refs(vb, &mut reachable);
        }
    }
    // Fixed-point closure through the call graph.
    loop {
        let mut changed = false;
        for item in items {
            if let TopLevel::FnDef(fd) = item
                && reachable.contains(&fd.name)
            {
                let mut called = HashSet::new();
                collect_called_idents_in_body(&fd.body, &mut called);
                for name in called {
                    if reachable.insert(name) {
                        changed = true;
                    }
                }
            }
        }
        if !changed {
            break;
        }
    }
    reachable
}

fn collect_verify_block_refs(vb: &VerifyBlock, out: &mut HashSet<String>) {
    out.insert(vb.fn_name.clone());
    for (lhs, rhs) in &vb.cases {
        collect_called_idents(lhs, out);
        collect_called_idents(rhs, out);
    }
    if let VerifyKind::Law(law) = &vb.kind {
        collect_called_idents(&law.lhs, out);
        collect_called_idents(&law.rhs, out);
        if let Some(when) = &law.when {
            collect_called_idents(when, out);
        }
        for given in &law.givens {
            if let VerifyGivenDomain::Explicit(values) = &given.domain {
                for v in values {
                    collect_called_idents(v, out);
                }
            }
        }
    }
    for given in &vb.cases_givens {
        if let VerifyGivenDomain::Explicit(values) = &given.domain {
            for v in values {
                collect_called_idents(v, out);
            }
        }
    }
}

fn collect_called_idents_in_body(body: &FnBody, out: &mut HashSet<String>) {
    for stmt in body.stmts() {
        match stmt {
            Stmt::Binding(_, _, e) | Stmt::Expr(e) => collect_called_idents(e, out),
        }
    }
}

fn collect_called_idents(expr: &Spanned<Expr>, out: &mut HashSet<String>) {
    match &expr.node {
        Expr::FnCall(callee, args) => {
            if let Expr::Ident(name) | Expr::Resolved { name, .. } = &callee.node {
                out.insert(name.clone());
            } else {
                collect_called_idents(callee, out);
            }
            for a in args {
                collect_called_idents(a, out);
            }
        }
        Expr::TailCall(boxed) => {
            let TailCallData { target, args, .. } = boxed.as_ref();
            out.insert(target.clone());
            for a in args {
                collect_called_idents(a, out);
            }
        }
        Expr::Ident(name) | Expr::Resolved { name, .. } => {
            out.insert(name.clone());
        }
        Expr::BinOp(_, l, r) => {
            collect_called_idents(l, out);
            collect_called_idents(r, out);
        }
        Expr::Neg(inner) => collect_called_idents(inner, out),
        Expr::Match { subject, arms, .. } => {
            collect_called_idents(subject, out);
            for arm in arms {
                collect_called_idents(&arm.body, out);
            }
        }
        Expr::ErrorProp(inner) | Expr::Attr(inner, _) => {
            collect_called_idents(inner, out);
        }
        Expr::Constructor(_, Some(inner)) => {
            collect_called_idents(inner, out);
        }
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let StrPart::Parsed(inner) = part {
                    collect_called_idents(inner, out);
                }
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for i in items {
                collect_called_idents(i, out);
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries {
                collect_called_idents(k, out);
                collect_called_idents(v, out);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, v) in fields {
                collect_called_idents(v, out);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_called_idents(base, out);
            for (_, v) in updates {
                collect_called_idents(v, out);
            }
        }
        Expr::Literal(_) | Expr::Constructor(_, None) => {}
    }
}

/// Sections gathered per emission scope ("" for entry, module prefix
/// otherwise). Each backend appends to the bucket for the scope a fn
/// (or its SCC component) belongs to.
pub(crate) struct PerScopeSections {
    pub by_scope: std::collections::HashMap<String, Vec<String>>,
}

impl PerScopeSections {
    pub(crate) fn take(&mut self, scope: &str) -> Vec<String> {
        self.by_scope.remove(scope).unwrap_or_default()
    }
}

/// Run SCC analysis on each scope's pure fns independently and route the
/// rendered output through the supplied closure. Lean and Dafny share
/// this — each scope (entry or dependent module) is SCC-analyzed in
/// isolation so a `def foo` in one module and an unrelated `def foo` in
/// another module don't get conflated.
///
/// `is_pure` filters which fns participate; `emit` renders one SCC
/// component (>= 1 fn) into the lines to append to that scope's bucket.
pub(crate) fn route_pure_components_per_scope<F, G>(
    ctx: &CodegenContext,
    is_pure: F,
    mut emit: G,
) -> PerScopeSections
where
    F: Fn(&FnDef) -> bool,
    G: FnMut(&[&FnDef], &str) -> Vec<String>,
{
    let mut by_scope: std::collections::HashMap<String, Vec<String>> =
        std::collections::HashMap::new();

    let mut process =
        |fns: Vec<&FnDef>,
         scope: String,
         by_scope: &mut std::collections::HashMap<String, Vec<String>>| {
            let comps = crate::call_graph::ordered_fn_components(&fns, &ctx.module_prefixes);
            let bucket = by_scope.entry(scope.clone()).or_default();
            for comp in comps {
                bucket.extend(emit(&comp, scope.as_str()));
            }
        };

    for module in &ctx.modules {
        let pure: Vec<&FnDef> = module.fn_defs.iter().filter(|fd| is_pure(fd)).collect();
        process(pure, module.prefix.clone(), &mut by_scope);
    }
    let entry_pure: Vec<&FnDef> = ctx.fn_defs.iter().filter(|fd| is_pure(fd)).collect();
    process(entry_pure, String::new(), &mut by_scope);

    PerScopeSections { by_scope }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Literal, VerifyGiven, VerifyGivenDomain, VerifyLaw};

    fn sb(node: Expr) -> Spanned<Expr> {
        Spanned::new(node, 1)
    }

    fn bsb(node: Expr) -> Box<Spanned<Expr>> {
        Box::new(sb(node))
    }

    fn law_with(lhs: Spanned<Expr>, rhs: Spanned<Expr>) -> VerifyLaw {
        VerifyLaw {
            name: "test".to_string(),
            givens: vec![VerifyGiven {
                name: "xs".to_string(),
                type_name: "List<String>".to_string(),
                domain: VerifyGivenDomain::Explicit(vec![sb(Expr::List(vec![]))]),
            }],
            when: None,
            lhs,
            rhs,
            sample_guards: Vec::new(),
        }
    }

    #[test]
    fn canonicalize_comparison_swaps_operands_never_negates() {
        use crate::ast::BinOp;
        let a = || bsb(Expr::Ident("a".to_string()));
        let b = || bsb(Expr::Ident("b".to_string()));
        let binop = |op, l: Box<Spanned<Expr>>, r: Box<Spanned<Expr>>| sb(Expr::BinOp(op, l, r));

        // `a > b` canonicalizes to `b < a` — operands swapped, STILL strict.
        let canon_gt = canonicalize_comparison(&binop(BinOp::Gt, a(), b()));
        assert_eq!(canon_gt.node, binop(BinOp::Lt, b(), a()).node);
        // `a >= b` canonicalizes to `b <= a` — operands swapped, STILL nonstrict.
        let canon_gte = canonicalize_comparison(&binop(BinOp::Gte, a(), b()));
        assert_eq!(canon_gte.node, binop(BinOp::Lte, b(), a()).node);

        // Strictness is NEVER crossed: a strict `>` never becomes a nonstrict
        // `<=`, and a nonstrict `>=` never becomes a strict `<`. (A `r < d`
        // upper bound whose strictness is load-bearing stays strict.)
        assert!(!matches!(&canon_gt.node, Expr::BinOp(BinOp::Lte, ..)));
        assert!(!matches!(&canon_gte.node, Expr::BinOp(BinOp::Lt, ..)));

        // Already-canonical / symmetric / non-comparison clauses pass through
        // BYTE-identically (no swap), so canonical-direction laws are untouched.
        for op in [BinOp::Lt, BinOp::Lte, BinOp::Eq, BinOp::Neq] {
            let clause = binop(op, a(), b());
            assert_eq!(canonicalize_comparison(&clause).node, clause.node);
        }
        let call = sb(Expr::FnCall(a(), vec![sb(Expr::Ident("x".to_string()))]));
        assert_eq!(canonicalize_comparison(&call).node, call.node);
    }

    #[test]
    fn law_calls_unclassified_fn_detects_dotted_callee() {
        // Review finding 1 (round 2): the recursion classifier emits
        // `UnclassifiedFn` messages keyed by bare `fd.name`, so the
        // unclassified set the gate consults holds BARE names even
        // for fns living in a dep module. A law calling
        // `Dep.toSorted(xs)` against a bare-`toSorted` unclassified
        // set has to match on the bare suffix too — otherwise the
        // gate doesn't fire and the backend emits a non-closing
        // `induction t with …`. Also accepts a fully-qualified entry
        // for forward compat with a future classifier that emits
        // canonical names.
        let lhs = sb(Expr::FnCall(
            bsb(Expr::Attr(
                bsb(Expr::Ident("Dep".to_string())),
                "toSorted".to_string(),
            )),
            vec![sb(Expr::Ident("xs".to_string()))],
        ));
        let rhs = sb(Expr::Ident("xs".to_string()));
        let law = law_with(lhs, rhs);

        // Forward-compat: a fully-qualified entry matches a dotted
        // callee directly.
        let mut canonical = HashSet::new();
        canonical.insert("Dep.toSorted".to_string());
        assert!(law_calls_unclassified_fn(&law, &canonical));

        // Real production shape today: classifier emits bare names;
        // the gate must still catch the dotted callsite.
        let mut bare_only = HashSet::new();
        bare_only.insert("toSorted".to_string());
        assert!(
            law_calls_unclassified_fn(&law, &bare_only),
            "bare unclassified name must catch a dotted callsite via suffix match"
        );

        // Negative control: an unrelated bare name in the set does
        // not match the dotted callsite.
        let mut unrelated = HashSet::new();
        unrelated.insert("somethingElse".to_string());
        assert!(!law_calls_unclassified_fn(&law, &unrelated));
    }

    #[test]
    fn map_key_types_are_read_out_of_nested_type_annotations() {
        // The refusal decides on the map's KEY type, so it has to find
        // every `Map<K, V>` written in a signature, including the ones
        // nested inside another container, and it has to stop at the
        // key's own comma rather than the outer one.
        let mut keys = Vec::new();
        collect_map_key_types("Map<String, Int>", &mut keys);
        assert_eq!(keys, vec!["String".to_string()]);

        let mut nested = Vec::new();
        collect_map_key_types("List<Map<Int, List<String>>>", &mut nested);
        assert_eq!(nested, vec!["Int".to_string()]);

        let mut both = Vec::new();
        collect_map_key_types("Map<String, Map<Bool, Int>>", &mut both);
        assert_eq!(both, vec!["String".to_string(), "Bool".to_string()]);

        // A generic key is a whole type argument, commas and all.
        let mut generic = Vec::new();
        collect_map_key_types("Map<Tuple<Int, Int>, Int>", &mut generic);
        assert_eq!(generic, vec!["Tuple<Int, Int>".to_string()]);

        // Negative controls: a type that merely ends in `Map` is a
        // different type, and a map-free annotation yields nothing.
        let mut unrelated = Vec::new();
        collect_map_key_types("List<Int>", &mut unrelated);
        collect_map_key_types("MyMap<Float, Int>", &mut unrelated);
        collect_map_key_types("Dep.Map<Float, Int>", &mut unrelated);
        assert!(
            unrelated.is_empty(),
            "only a standalone `Map<..>` names a key type, got {:?}",
            unrelated
        );
    }

    #[test]
    fn law_lhs_has_trace_projection_skips_user_record_field() {
        // Review finding 4: a user-defined record with a `trace`
        // field (`record Log { trace: String, ... }`) used to trigger
        // the gate as soon as `log.trace` appeared in the LHS. The
        // detection now requires BOTH a `.trace` field projection
        // AND a trace-API method call (`.event`, `.group`, `.branch`,
        // `.length`, `.contains`) — a bare `log.trace` is just a
        // record access and stays in normal emit.
        let user_field_lhs = sb(Expr::Attr(
            bsb(Expr::Ident("log".to_string())),
            "trace".to_string(),
        ));
        assert!(
            !law_lhs_has_trace_projection(&user_field_lhs),
            "bare user-record `.trace` field must not trigger the gate"
        );

        // Real Oracle trace projection: `fn().trace.event(0)`.
        let runtime_trace_lhs = sb(Expr::FnCall(
            bsb(Expr::Attr(
                bsb(Expr::Attr(
                    bsb(Expr::FnCall(bsb(Expr::Ident("fn".to_string())), vec![])),
                    "trace".to_string(),
                )),
                "event".to_string(),
            )),
            vec![sb(Expr::Literal(Literal::Int(0)))],
        ));
        assert!(
            law_lhs_has_trace_projection(&runtime_trace_lhs),
            "Oracle `.trace.event(0)` projection must trigger the gate"
        );
    }

    #[test]
    fn resolve_refined_type_disambiguates_cross_module_same_bare_name() {
        // Review finding 3: two modules each declaring a refined
        // `Natural` (different predicates) used to silently collide
        // under bare keying — `populate_refined_types` skipped the
        // second one via `contains_key`, and the backend lookup
        // returned whichever predicate landed first. With canonical
        // keying (`A.Natural` / `B.Natural`) the slots are separate;
        // a fully-qualified lookup matches exactly, a bare lookup
        // resolves through module-walk ordering.
        use crate::ast::{TypeDef, TypeVariant};
        use crate::codegen::ModuleInfo;
        use crate::ir::proof_ir::{Predicate, QuantifierType, RefinedTypeDecl};
        use std::collections::HashMap;
        let _ = TypeVariant {
            name: String::new(),
            fields: Vec::new(),
        }; // make sure TypeVariant import resolves under all features

        let make_module = |prefix: &str| ModuleInfo {
            prefix: prefix.to_string(),
            depends: Vec::new(),
            type_defs: vec![TypeDef::Product {
                name: "Natural".to_string(),
                fields: vec![("value".to_string(), "Int".to_string())],
                line: 1,
            }],
            fn_defs: Vec::new(),
            verify_laws: Vec::new(),
            analysis: None,
        };
        let modules = vec![make_module("A"), make_module("B")];

        let make_decl = |predicate_param: &str, witness: i64| RefinedTypeDecl {
            name: "Natural".to_string(),
            carrier_type: "Int".to_string(),
            carrier_field: "value".to_string(),
            predicate_param: predicate_param.to_string(),
            invariant: Predicate {
                free_vars: vec![(
                    predicate_param.to_string(),
                    QuantifierType::Plain("Int".to_string()),
                )],
                expr: crate::ast::Spanned::bare(crate::ir::hir::ResolvedExpr::Literal(
                    Literal::Bool(true),
                )),
            },
            witness: Some(witness.to_string()),
            interval: None,
            op_classes: Vec::new(),
        };
        let symbols = crate::ir::SymbolTable::build(&[], &modules);
        let a_id = symbols
            .type_id_of(&crate::ir::TypeKey::in_module("A", "Natural"))
            .expect("A.Natural TypeId");
        let b_id = symbols
            .type_id_of(&crate::ir::TypeKey::in_module("B", "Natural"))
            .expect("B.Natural TypeId");

        let mut refined_types: HashMap<crate::ir::TypeId, RefinedTypeDecl> = HashMap::new();
        refined_types.insert(a_id, make_decl("a", 0));
        refined_types.insert(b_id, make_decl("b", 10));

        // Fully-qualified lookups hit the exact slot.
        let a = resolve_refined_type_in(&refined_types, &symbols, &modules, "A.Natural")
            .expect("A.Natural canonical lookup");
        assert_eq!(a.predicate_param, "a");
        assert_eq!(a.witness.as_deref(), Some("0"));
        let b = resolve_refined_type_in(&refined_types, &symbols, &modules, "B.Natural")
            .expect("B.Natural canonical lookup");
        assert_eq!(b.predicate_param, "b");
        assert_eq!(b.witness.as_deref(), Some("10"));

        // Bare lookup finds *something* (module-walk first match) —
        // the typechecker prevents mixed usage upstream, so the
        // observed ordering is the order modules walk.
        let bare = resolve_refined_type_in(&refined_types, &symbols, &modules, "Natural")
            .expect("bare Natural resolves via module walk");
        assert!(
            bare.predicate_param == "a" || bare.predicate_param == "b",
            "bare Natural must resolve to one of the canonical decls"
        );

        // No-module-owner lookup misses — i.e. a bare name that
        // wasn't declared in any module is None, not "first in map".
        assert!(resolve_refined_type_in(&refined_types, &symbols, &modules, "Unrelated").is_none());
    }

    #[test]
    fn find_refined_type_scoped_prefers_current_module_over_entry_collision() {
        // Review finding 2 (round 3): when entry AND a dep module
        // both declare a refined record of the same bare name, the
        // module's emit pass (passing `scope = Some(prefix)` and a
        // bare reference) must resolve to its OWN slot, not entry's.
        // The pre-fix order tried direct `refined_types.get(name)`
        // first, which matched entry's bare-keyed slot and the
        // scope-prefix lookup never ran.
        use crate::ast::{TopLevel, TypeDef};
        use crate::codegen::{CodegenContext, ModuleInfo};
        use crate::ir::proof_ir::{Predicate, QuantifierType, RefinedTypeDecl};
        use std::collections::{HashMap, HashSet};

        let entry_natural = TypeDef::Product {
            name: "Natural".to_string(),
            fields: vec![("value".to_string(), "Int".to_string())],
            line: 1,
        };
        let module = ModuleInfo {
            prefix: "Mod".to_string(),
            depends: Vec::new(),
            type_defs: vec![TypeDef::Product {
                name: "Natural".to_string(),
                fields: vec![("value".to_string(), "Int".to_string())],
                line: 1,
            }],
            fn_defs: Vec::new(),
            verify_laws: Vec::new(),
            analysis: None,
        };

        let make_decl = |param: &str, witness: &str| RefinedTypeDecl {
            name: "Natural".to_string(),
            carrier_type: "Int".to_string(),
            carrier_field: "value".to_string(),
            predicate_param: param.to_string(),
            invariant: Predicate {
                free_vars: vec![(param.to_string(), QuantifierType::Plain("Int".to_string()))],
                expr: crate::ast::Spanned::bare(crate::ir::hir::ResolvedExpr::Literal(
                    Literal::Bool(true),
                )),
            },
            witness: Some(witness.to_string()),
            interval: None,
            op_classes: Vec::new(),
        };

        let items = vec![TopLevel::TypeDef(entry_natural)];
        let modules = vec![module];
        let symbol_table = crate::ir::SymbolTable::build(&items, &modules);
        let entry_id = symbol_table
            .type_id_of(&crate::ir::TypeKey::entry("Natural"))
            .expect("entry Natural id");
        let mod_id = symbol_table
            .type_id_of(&crate::ir::TypeKey::in_module("Mod", "Natural"))
            .expect("Mod.Natural id");

        let mut ctx = CodegenContext {
            items,
            type_defs: Vec::new(),
            fn_defs: Vec::new(),
            project_name: "scope-test".to_string(),
            modules,
            module_prefixes: HashSet::new(),
            #[cfg(feature = "runtime")]
            policy: None,
            emit_replay_runtime: false,
            runtime_policy_from_env: false,
            guest_entry: None,
            emit_self_host_support: false,
            extra_fn_defs: Vec::new(),
            mutual_tco_members: HashSet::new(),
            recursive_fns: HashSet::new(),
            buffer_build_sinks: HashMap::new(),
            buffer_fusion_sites: Vec::new(),
            synthesized_buffered_fns: Vec::new(),
            proof_ir: crate::ir::ProofIR::default(),
            symbol_table,
            resolved_fn_defs: Vec::new(),
            resolved_module_fn_defs: Vec::new(),
            current_module_scope: std::cell::RefCell::new(None),
            lean_do_block: std::cell::Cell::new(false),
            resolved_program: crate::codegen::program_view::ResolvedProgramView::default(),
            program_shape: None,
            mir_program: None,
            bare_i64: Default::default(),
            discovered_lemmas: Vec::new(),
            sample_expected: std::collections::HashMap::new(),
            allow_mathlib: false,
            hand_proofs: Default::default(),
        };
        ctx.proof_ir
            .refined_types
            .insert(entry_id, make_decl("entry_n", "0"));
        ctx.proof_ir
            .refined_types
            .insert(mod_id, make_decl("mod_n", "10"));

        let from_module = find_refined_type_scoped(&ctx, "Natural", Some("Mod"))
            .expect("Mod-scoped Natural lookup");
        assert_eq!(
            from_module.predicate_param, "mod_n",
            "scope=Some(\"Mod\") + bare `Natural` must resolve to Mod.Natural, \
             not entry's bare-keyed slot"
        );

        // Entry scope still resolves to entry's slot.
        let from_entry =
            find_refined_type_scoped(&ctx, "Natural", None).expect("entry Natural lookup");
        assert_eq!(from_entry.predicate_param, "entry_n");
    }
}

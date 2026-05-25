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
    /// AST node for the bool predicate the smart constructor
    /// branches on — the body's `Match { subject = <here>, ... }`.
    pub predicate: &'a Spanned<Expr>,
}

/// Inspect inputs for a refinement-via-opaque record by `type_name`.
/// Returns `Some(info)` iff there's exactly one matching smart
/// constructor and the record has a single carrier field.
pub fn refinement_info_for<'a>(
    type_name: &str,
    inputs: &crate::codegen::proof_lower::ProofLowerInputs<'a>,
) -> Option<RefinementInfo<'a>> {
    // Refinement records may live in the entry file or in a
    // dependent module. Same for the smart constructor. Walk both
    // so cross-module compilations (`aver proof natural_app.av`
    // depending on a `Natural` module) produce the same lifted
    // shape as the standalone module file.
    let entry_typedefs = inputs.entry_items.iter().filter_map(|item| match item {
        TopLevel::TypeDef(td) => Some(td),
        _ => None,
    });
    let module_typedefs = inputs.dep_modules.iter().flat_map(|m| m.type_defs.iter());
    let (carrier_field, carrier_type) =
        entry_typedefs
            .chain(module_typedefs)
            .find_map(|td| match td {
                TypeDef::Product { name, fields, .. } if name == type_name && fields.len() == 1 => {
                    let (fname, ftype) = &fields[0];
                    Some((fname.as_str(), ftype.as_str()))
                }
                _ => None,
            })?;

    let entry_fns = inputs.entry_items.iter().filter_map(|item| match item {
        TopLevel::FnDef(fd) => Some(fd),
        _ => None,
    });
    let module_fns = inputs.dep_modules.iter().flat_map(|m| m.fn_defs.iter());
    for fd in entry_fns.chain(module_fns) {
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
pub fn refinement_lift_for_given<'a>(
    given_name: &str,
    given_type: &str,
    lhs: &Spanned<Expr>,
    rhs: &Spanned<Expr>,
    ctx: &'a CodegenContext,
) -> Option<&'a str> {
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
    let mut result: Option<&'a str> = None;
    search_refinement_wrapper(lhs, given_name, given_type, ctx, &mut result);
    search_refinement_wrapper(rhs, given_name, given_type, ctx, &mut result);
    result
}

fn search_refinement_wrapper<'a>(
    expr: &Spanned<Expr>,
    given_name: &str,
    given_type: &str,
    ctx: &'a CodegenContext,
    result: &mut Option<&'a str>,
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
            let inputs_local = crate::codegen::proof_lower::ProofLowerInputs::from_ctx(ctx);
            if matches_var
                && let Some(info) = refinement_info_for(type_name, &inputs_local)
                && info.carrier_type == given_type
            {
                // Need a stable reference into ctx for the returned
                // &str. `refinement_info_for` returns refs into ctx
                // already, but we want the *type name* itself; the
                // name may live in `ctx.items` (standalone build) or
                // in a dependent module's `type_defs` (cross-module).
                let entry_tds = ctx.items.iter().filter_map(|i| match i {
                    TopLevel::TypeDef(td) => Some(td),
                    _ => None,
                });
                let module_tds = ctx.modules.iter().flat_map(|m| m.type_defs.iter());
                for td in entry_tds.chain(module_tds) {
                    if let TypeDef::Product { name, .. } = td
                        && name == type_name
                    {
                        *result = Some(name.as_str());
                        return;
                    }
                }
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
) -> Spanned<Expr> {
    let new_node = match &expr.node {
        Expr::RecordCreate { type_name, fields } if fields.len() == 1 => {
            let (_, fvalue) = &fields[0];
            let var_name = match &fvalue.node {
                Expr::Ident(n) | Expr::Resolved { name: n, .. } => Some(n.clone()),
                _ => None,
            };
            if let Some(name) = var_name
                && let Some(refined) = lifted_vars.get(&name)
                && refined == type_name
            {
                return Spanned::new(Expr::Ident(name), expr.line);
            }
            let new_fields: Vec<(String, Spanned<Expr>)> = fields
                .iter()
                .map(|(n, v)| (n.clone(), strip_refinement_wrappers(v, lifted_vars)))
                .collect();
            Expr::RecordCreate {
                type_name: type_name.clone(),
                fields: new_fields,
            }
        }
        Expr::FnCall(callee, args) => Expr::FnCall(
            Box::new(strip_refinement_wrappers(callee, lifted_vars)),
            args.iter()
                .map(|a| strip_refinement_wrappers(a, lifted_vars))
                .collect(),
        ),
        Expr::BinOp(op, l, r) => Expr::BinOp(
            *op,
            Box::new(strip_refinement_wrappers(l, lifted_vars)),
            Box::new(strip_refinement_wrappers(r, lifted_vars)),
        ),
        Expr::Attr(o, f) => Expr::Attr(
            Box::new(strip_refinement_wrappers(o, lifted_vars)),
            f.clone(),
        ),
        Expr::Neg(i) => Expr::Neg(Box::new(strip_refinement_wrappers(i, lifted_vars))),
        Expr::ErrorProp(i) => Expr::ErrorProp(Box::new(strip_refinement_wrappers(i, lifted_vars))),
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
    let when_conjuncts = flatten_bool_and_conjuncts(when_expr);
    // Flatten BOTH sides — IntRange-style refinement predicates carry a
    // compound `Bool.and(n >= 0, n <= 100)` invariant; without
    // flattening, a `when Bool.and(a >= 0, a <= 100)` user clause
    // (which the parser also flattens into atoms during conjunct
    // walk) would length-mismatch and keep the now-redundant
    // premise. Same flatten on both sides keeps natural / positive
    // / int_range behavior identical to pre-fix.
    let inputs = crate::codegen::proof_lower::ProofLowerInputs::from_ctx(ctx);
    let mut lifted_predicates: Vec<Spanned<Expr>> = Vec::new();
    for (given_name, refined_type) in lifted_vars {
        let Some(info) = refinement_info_for(refined_type, &inputs) else {
            return false;
        };
        let substituted = substitute_ident_in_expr(info.predicate, info.param_name, given_name);
        lifted_predicates.extend(flatten_bool_and_conjuncts(&substituted));
    }
    if when_conjuncts.len() != lifted_predicates.len() {
        return false;
    }
    let mut matched = vec![false; lifted_predicates.len()];
    for wc in &when_conjuncts {
        let Some(idx) = (0..lifted_predicates.len())
            .find(|&i| !matched[i] && predicate_syntactic_eq(wc, &lifted_predicates[i]))
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
    resolve_refined_type_in(&ctx.proof_ir.refined_types, &ctx.modules, name)
}

/// Same resolution shape as [`find_refined_type`] but driven by the
/// in-flight `refined_types` map plus dep-module list — used inside
/// `proof_lower` where there is no `CodegenContext` yet.
pub fn resolve_refined_type_in<'a>(
    refined_types: &'a std::collections::HashMap<String, crate::ir::proof_ir::RefinedTypeDecl>,
    modules: &[crate::codegen::ModuleInfo],
    name: &str,
) -> Option<&'a crate::ir::proof_ir::RefinedTypeDecl> {
    if let Some(decl) = refined_types.get(name) {
        return Some(decl);
    }
    let bare = name.rsplit('.').next().unwrap_or(name);
    for m in modules {
        for td in &m.type_defs {
            if type_def_name(td) == bare {
                let canonical = format!("{}.{}", m.prefix, bare);
                if let Some(decl) = refined_types.get(&canonical) {
                    return Some(decl);
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
            // module-qualified (`Models.User.size`) shapes. Without this,
            // a law calling `Dep.toSorted(xs)` slipped past the gate and
            // emitted a non-closing `induction t with …` against a
            // fuel-bounded helper.
            let direct = expr_to_dotted_name(&callee.node)
                .map(|n| names.contains(n.as_str()))
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

const TRACE_API_METHODS: &[&str] = &["event", "group", "branch", "length", "contains"];

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

/// Map every fn name in the program to its owning scope: the dependent
/// module's prefix, or `""` for the entry. Used by the multi-file Lean
/// and Dafny paths to route SCC components and fuel groups to the right
/// per-scope file.
pub(crate) fn fn_owning_scope(ctx: &CodegenContext) -> std::collections::HashMap<String, String> {
    let mut scope = std::collections::HashMap::new();
    for m in &ctx.modules {
        for fd in &m.fn_defs {
            scope.insert(fd.name.clone(), m.prefix.clone());
        }
    }
    for fd in &ctx.fn_defs {
        scope.insert(fd.name.clone(), String::new());
    }
    scope
}

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

/// Check if an expression is a compile-time Unit literal.
pub(crate) fn is_unit_expr(expr: &crate::ast::Expr) -> bool {
    matches!(expr, crate::ast::Expr::Literal(crate::ast::Literal::Unit))
}

/// Check if a spanned expression is a compile-time Unit literal.
pub(crate) fn is_unit_expr_spanned(expr: &crate::ast::Spanned<crate::ast::Expr>) -> bool {
    is_unit_expr(&expr.node)
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
    G: FnMut(&[&FnDef]) -> Vec<String>,
{
    let mut by_scope: std::collections::HashMap<String, Vec<String>> =
        std::collections::HashMap::new();

    let mut process =
        |fns: Vec<&FnDef>,
         scope: String,
         by_scope: &mut std::collections::HashMap<String, Vec<String>>| {
            let comps = crate::call_graph::ordered_fn_components(&fns, &ctx.module_prefixes);
            let bucket = by_scope.entry(scope).or_default();
            for comp in comps {
                bucket.extend(emit(&comp));
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
    fn law_calls_unclassified_fn_detects_dotted_callee() {
        // Review finding 1: a law calling `Dep.toSorted(xs)` (where
        // `toSorted` is on the unclassified list) used to slip past
        // the gate because `expr_calls_named` only matched bare
        // `Expr::Ident` callees. With `expr_to_dotted_name`-based
        // resolution the dotted form is detected too.
        let lhs = sb(Expr::FnCall(
            bsb(Expr::Attr(
                bsb(Expr::Ident("Dep".to_string())),
                "toSorted".to_string(),
            )),
            vec![sb(Expr::Ident("xs".to_string()))],
        ));
        let rhs = sb(Expr::Ident("xs".to_string()));
        let law = law_with(lhs, rhs);

        let mut unclassified = HashSet::new();
        unclassified.insert("Dep.toSorted".to_string());
        assert!(law_calls_unclassified_fn(&law, &unclassified));

        // Same lhs, but the unclassified set names only the bare
        // form — confirms the dotted name resolved through.
        let mut bare_only = HashSet::new();
        bare_only.insert("toSorted".to_string());
        assert!(!law_calls_unclassified_fn(&law, &bare_only));
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
                expr: sb(Expr::Literal(Literal::Bool(true))),
            },
            witness: Some(witness.to_string()),
        };
        let mut refined_types: HashMap<String, RefinedTypeDecl> = HashMap::new();
        refined_types.insert("A.Natural".to_string(), make_decl("a", 0));
        refined_types.insert("B.Natural".to_string(), make_decl("b", 10));

        // Fully-qualified lookups hit the exact slot.
        let a = resolve_refined_type_in(&refined_types, &modules, "A.Natural")
            .expect("A.Natural canonical lookup");
        assert_eq!(a.predicate_param, "a");
        assert_eq!(a.witness.as_deref(), Some("0"));
        let b = resolve_refined_type_in(&refined_types, &modules, "B.Natural")
            .expect("B.Natural canonical lookup");
        assert_eq!(b.predicate_param, "b");
        assert_eq!(b.witness.as_deref(), Some("10"));

        // Bare lookup finds *something* (module-walk first match) —
        // the typechecker prevents mixed usage upstream, so the
        // observed ordering is the order modules walk.
        let bare = resolve_refined_type_in(&refined_types, &modules, "Natural")
            .expect("bare Natural resolves via module walk");
        assert!(
            bare.predicate_param == "a" || bare.predicate_param == "b",
            "bare Natural must resolve to one of the canonical decls"
        );

        // No-module-owner lookup misses — i.e. a bare name that
        // wasn't declared in any module is None, not "first in map".
        assert!(resolve_refined_type_in(&refined_types, &modules, "Unrelated").is_none());
    }
}

//! Explicit quotient induction hints with source-level simultaneous renaming.
//! Calls remain ordinary Dafny lemma calls: their guards, prior reasons and
//! decreasing measure are verification obligations, never new assumptions.

use std::collections::BTreeMap;

use crate::ast::{Expr, Literal, Pattern, Spanned, Stmt, VerifyLaw};
use crate::codegen::CodegenContext;
use crate::codegen::dafny::expr::emit_expr;
use crate::codegen::dafny::toplevel::resolve_rewrite_output;

fn identifier(expr: &Spanned<Expr>) -> Option<&str> {
    match &expr.node {
        Expr::Ident(name) | Expr::Resolved { name, .. } => Some(name),
        _ => None,
    }
}

pub(super) fn emit(
    expr: &Spanned<Expr>,
    law: &VerifyLaw,
    step_name: &str,
    ctx: &CodegenContext,
) -> Option<Vec<String>> {
    let Expr::FnCall(callee, actuals) = &expr.node else {
        return None;
    };
    let scope = ctx.active_module_scope();
    let name = crate::checker::expr_to_str(callee);
    let id = ctx.symbol_table.resolve_fn_id_in(&name, scope.as_deref())?;
    let key = &ctx.symbol_table.fn_entry(id).key;
    if key.scope_str() != scope.as_deref() {
        return None;
    }
    let fd = ctx.fn_def_by_name(&key.name, key.scope_str())?;
    if fd.return_type != "Bool"
        || fd.params.len() != actuals.len()
        || actuals.len() != law.givens.len()
    {
        return None;
    }
    super::quotient_parameter(fd, ctx)?;
    let mut substitutions = BTreeMap::new();
    let mut positions = BTreeMap::new();
    for (index, ((formal, _), actual)) in fd.params.iter().zip(actuals).enumerate() {
        let given = identifier(actual)?;
        if !law.givens.iter().any(|g| g.name == given) || positions.insert(given, index).is_some() {
            return None;
        }
        substitutions.insert(formal.as_str(), given);
    }
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return None;
    };
    let Expr::Match { subject, arms } = &body.node else {
        return None;
    };
    if arms.len() != 2 {
        return None;
    }
    let mut recursive = None;
    let mut saw_true = false;
    let mut saw_false = false;
    for arm in arms {
        let Pattern::Literal(Literal::Bool(positive)) = arm.pattern else {
            return None;
        };
        if positive {
            if saw_true {
                return None;
            }
            saw_true = true;
        } else {
            if saw_false {
                return None;
            }
            saw_false = true;
        }
        if crate::codegen::expr_walk::any(&arm.body, &mut |e| matches!(e.node, Expr::Match { .. }))
        {
            return None;
        }
        let mut calls = Vec::new();
        crate::codegen::recursion::detect::collect_calls_from_expr(&arm.body, &mut calls);
        for (name, args) in calls {
            if ctx.symbol_table.resolve_fn_id_in(&name, scope.as_deref()) == Some(id) {
                if recursive.is_some() {
                    return None;
                }
                recursive = Some((positive, args));
            }
        }
    }
    let (positive, recursive_args) = recursive?;
    if recursive_args.len() != fd.params.len() {
        return None;
    }
    let render = |value| {
        let renamed = rename(value, &substitutions)?;
        Some(emit_expr(&resolve_rewrite_output(&renamed, ctx), ctx))
    };
    let guard = render(subject)?;
    let guard = if positive {
        guard
    } else {
        format!("!({guard})")
    };
    let translated = recursive_args
        .iter()
        .map(|arg| render(arg))
        .collect::<Option<Vec<_>>>()?;
    let ordered = law
        .givens
        .iter()
        .map(|given| {
            translated
                .get(*positions.get(given.name.as_str())?)
                .cloned()
        })
        .collect::<Option<Vec<_>>>()?;
    Some(vec![
        format!("  if {guard} {{"),
        format!("    {step_name}({});", ordered.join(", ")),
        "  }".to_string(),
    ])
}

/// Rename value identifiers before HIR resolution; static function/type/field
/// names and literal strings never undergo textual substitution. Inserted
/// names are not visited again. Expressions introducing binders are omitted.
fn rename(expr: &Spanned<Expr>, substitutions: &BTreeMap<&str, &str>) -> Option<Spanned<Expr>> {
    let recur = |value| rename(value, substitutions);
    let node = match &expr.node {
        Expr::Literal(_) => return Some(expr.clone()),
        Expr::Ident(name) | Expr::Resolved { name, .. } => {
            Expr::Ident(substitutions.get(name.as_str())?.to_string())
        }
        Expr::Neg(inner) => Expr::Neg(Box::new(recur(inner)?)),
        Expr::BinOp(op, left, right) => {
            Expr::BinOp(*op, Box::new(recur(left)?), Box::new(recur(right)?))
        }
        Expr::List(values) => Expr::List(values.iter().map(recur).collect::<Option<_>>()?),
        Expr::Tuple(values) => Expr::Tuple(values.iter().map(recur).collect::<Option<_>>()?),
        Expr::FnCall(callee, args) => {
            let name = crate::codegen::common::expr_to_dotted_name(&callee.node)?;
            if substitutions.contains_key(name.as_str()) {
                return None;
            }
            Expr::FnCall(
                callee.clone(),
                args.iter().map(recur).collect::<Option<_>>()?,
            )
        }
        Expr::Attr(base, field) => Expr::Attr(Box::new(recur(base)?), field.clone()),
        Expr::Constructor(name, arg) => Expr::Constructor(
            name.clone(),
            match arg {
                Some(arg) => Some(Box::new(recur(arg)?)),
                None => None,
            },
        ),
        Expr::RecordCreate { type_name, fields } => Expr::RecordCreate {
            type_name: type_name.clone(),
            fields: fields
                .iter()
                .map(|(name, value)| Some((name.clone(), recur(value)?)))
                .collect::<Option<_>>()?,
        },
        Expr::RecordUpdate {
            type_name,
            base,
            updates,
        } => Expr::RecordUpdate {
            type_name: type_name.clone(),
            base: Box::new(recur(base)?),
            updates: updates
                .iter()
                .map(|(name, value)| Some((name.clone(), recur(value)?)))
                .collect::<Option<_>>()?,
        },
        _ => return None,
    };
    Some(Spanned::new(node, expr.line))
}

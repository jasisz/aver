//! A source-analysis view with redundant local value aliases eliminated.
//! This never rewrites the exported function: each backend still checks the
//! original lets. Only a local read is substituted, so evaluation, calls and
//! effects cannot be duplicated or discarded. Capture keeps the binding.

use crate::ast::{Expr, FnBody, FnDef, Spanned, Stmt};
use std::{borrow::Cow, collections::HashSet, sync::Arc};

fn local(e: &Spanned<Expr>) -> Option<&str> {
    match &e.node {
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => Some(n),
        _ => None,
    }
}

fn replace(e: &mut Spanned<Expr>, alias: &str, value: &Spanned<Expr>, captured: bool) -> bool {
    if local(e) == Some(alias) {
        if captured {
            return false;
        }
        let line = e.line;
        *e = value.clone();
        e.line = line;
        return true;
    }
    if let Expr::Match { subject, arms } = &mut e.node {
        if !replace(subject, alias, value, captured) {
            return false;
        }
        for arm in arms {
            let names = crate::ast_rewrite::pattern_binding_names(&arm.pattern);
            // An inner binder of the alias refers to a different value.
            if names.iter().any(|n| n == alias) {
                continue;
            }
            let captured = captured || names.iter().any(|n| Some(n.as_str()) == local(value));
            if !replace(&mut arm.body, alias, value, captured) {
                return false;
            }
        }
        return true;
    }
    let mut valid = true;
    super::expr_walk::for_each_child_mut(e, &mut |child| {
        valid &= replace(child, alias, value, captured);
    });
    valid
}

pub(crate) fn normalize(fd: &FnDef) -> Cow<'_, FnDef> {
    if !matches!(fd.body.stmts().last(), Some(Stmt::Expr(_)))
        || !fd
            .body
            .stmts()
            .iter()
            .any(|s| matches!(s, Stmt::Binding(_, _, e) if local(e).is_some()))
    {
        return Cow::Borrowed(fd);
    }
    let mut stmts = fd.body.stmts().to_vec();
    let mut locals: HashSet<String> = fd.params.iter().map(|(n, _)| n.clone()).collect();
    let mut changed = false;
    let mut i = 0;
    while i < stmts.len() {
        let Stmt::Binding(alias, _, value) = &stmts[i] else {
            i += 1;
            continue;
        };
        let alias = alias.clone();
        let value = value.clone();
        let eligible = local(&value).is_some_and(|n| locals.contains(n))
            // Callable aliases need resolved call-graph support of their own.
            && !matches!(value.ty(), Some(crate::ast::Type::Fn(..)));
        if !eligible {
            locals.insert(alias);
            i += 1;
            continue;
        }
        let mut suffix = stmts[i + 1..].to_vec();
        let mut captured = false;
        let mut valid = eligible;
        if eligible {
            for stmt in &mut suffix {
                let (binding, expr) = match stmt {
                    Stmt::Binding(name, _, e) => (Some(name.as_str()), e),
                    Stmt::Expr(e) => (None, e),
                };
                valid &= replace(expr, &alias, &value, captured);
                if binding == Some(alias.as_str()) {
                    break;
                }
                captured |= binding == local(&value);
            }
        }
        if valid {
            stmts.truncate(i);
            stmts.extend(suffix);
            changed = true;
        } else {
            locals.insert(alias);
            i += 1;
        }
    }
    if !changed {
        return Cow::Borrowed(fd);
    }
    let mut result = fd.clone();
    result.body = Arc::new(FnBody::Block(stmts));
    // Binding positions have changed; this analysis view must not be emitted
    // with the original statement-to-slot table.
    result.resolution = None;
    Cow::Owned(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn function(body: &str) -> FnDef {
        crate::source::parse_source(&format!("fn sample(value: Int) -> Int\n{body}"))
            .unwrap()
            .into_iter()
            .find_map(|item| match item {
                crate::ast::TopLevel::FnDef(fd) => Some(fd),
                _ => None,
            })
            .unwrap()
    }

    #[test]
    fn capture_keeps_original_binding_instead_of_changing_its_meaning() {
        for body in [
            "    saved = value\n    match [7]\n        [] -> 0\n        [value, ..rest] -> saved\n",
            "    saved = value\n    value = 7\n    saved\n",
        ] {
            let fd = function(body);
            assert!(matches!(normalize(&fd), Cow::Borrowed(_)));
        }
    }

    #[test]
    fn shadowed_alias_is_not_an_outer_reference() {
        let fd = function(
            "    saved = value\n    match [7]\n        [] -> saved\n        [saved, ..rest] -> saved\n",
        );
        let normalized = normalize(&fd);
        assert_eq!(normalized.body.stmts().len(), 1);
        let Stmt::Expr(e) = &normalized.body.stmts()[0] else {
            panic!()
        };
        let Expr::Match { arms, .. } = &e.node else {
            panic!()
        };
        assert_eq!(local(&arms[0].body), Some("value"));
        assert_eq!(local(&arms[1].body), Some("saved"));
    }

    #[test]
    fn computed_bindings_are_evaluated_once_and_type_stamps_survive() {
        let fd = function("    computed = value + 1\n    saved = computed\n    saved + saved\n");
        let Stmt::Expr(e) = &fd.body.stmts()[2] else {
            panic!()
        };
        e.set_ty(crate::ast::Type::Int);
        let normalized = normalize(&fd);
        assert_eq!(normalized.body.stmts().len(), 2);
        let Stmt::Binding(name, _, _) = &normalized.body.stmts()[0] else {
            panic!()
        };
        assert_eq!(name, "computed");
        let Stmt::Expr(e) = &normalized.body.stmts()[1] else {
            panic!()
        };
        assert_eq!(e.ty(), Some(&crate::ast::Type::Int));
        assert!(matches!(e.node, Expr::BinOp(..)));
    }
}

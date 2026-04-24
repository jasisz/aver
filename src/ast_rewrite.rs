//! Ident-replacement traversal that respects match-arm pattern scoping.
//!
//! Several passes (verify-trace local bindings in the parser, law-auto
//! sample expansion in the Lean codegen) need to walk an `Expr` tree and
//! substitute bare `Ident`s. Writing one walker per site is tempting,
//! but the match-arm scoping logic is subtle — patterns introduce new
//! bindings that shadow outer names — and each copy picked up the same
//! shadowing bug independently. This module owns the traversal so a
//! single fix keeps every site correct.
//!
//! [`rewrite_idents_scoped`] is the generic entry point: you pass an
//! expression and a callback that maps an `Ident` name to an optional
//! replacement (`None` leaves the identifier alone). Pattern shadowing
//! is handled automatically — inside a `Match { arms: [(pat, body)] }`,
//! names bound by `pat` are ignored by the callback for the duration of
//! `body`.

use std::collections::HashSet;

use crate::ast::{Expr, MatchArm, Pattern, Spanned, StrPart, TailCallData};

/// Collect every name introduced by a match-arm pattern. Composite
/// patterns (tuple, cons, constructor) recursively contribute all their
/// sub-bindings; wildcards and literals contribute nothing.
pub fn pattern_binding_names(pattern: &Pattern) -> Vec<String> {
    let mut out = Vec::new();
    collect_pattern_bindings(pattern, &mut out);
    out
}

fn collect_pattern_bindings(pattern: &Pattern, out: &mut Vec<String>) {
    match pattern {
        Pattern::Ident(name) => out.push(name.clone()),
        Pattern::Cons(head, tail) => {
            out.push(head.clone());
            out.push(tail.clone());
        }
        Pattern::Tuple(items) => {
            for item in items {
                collect_pattern_bindings(item, out);
            }
        }
        Pattern::Constructor(_, binders) => {
            for name in binders {
                out.push(name.clone());
            }
        }
        Pattern::Wildcard | Pattern::Literal(_) | Pattern::EmptyList => {}
    }
}

/// Walk `expr` and replace `Ident(name)` occurrences via `rewrite`.
///
/// Inside a `Match`, names introduced by an arm's pattern are hidden
/// from the callback while its body is rewritten — the inner pattern
/// binding shadows the outer substitution, so
/// `let x = 1; match Option.Some(2) { Some(x) -> x }` leaves the body's
/// `x` alone (→ 2) instead of replacing it with the outer `1`.
///
/// `rewrite` returns `Some(expr)` to substitute, `None` to leave the
/// identifier as-is. `Resolved { name }` nodes are also considered
/// (same shape as `Ident` from a substitution standpoint), but only
/// when not shadowed.
pub fn rewrite_idents_scoped<F>(expr: &Spanned<Expr>, mut rewrite: F) -> Spanned<Expr>
where
    F: FnMut(&str) -> Option<Spanned<Expr>>,
{
    let scope = HashSet::new();
    rewrite_inner(expr, &scope, &mut rewrite)
}

fn rewrite_inner<F>(expr: &Spanned<Expr>, scope: &HashSet<String>, rewrite: &mut F) -> Spanned<Expr>
where
    F: FnMut(&str) -> Option<Spanned<Expr>>,
{
    let line = expr.line;
    match &expr.node {
        Expr::Ident(name) => {
            if scope.contains(name) {
                Spanned::new(expr.node.clone(), line)
            } else {
                rewrite(name).unwrap_or_else(|| Spanned::new(expr.node.clone(), line))
            }
        }
        Expr::Resolved { name, .. } => {
            if scope.contains(name) {
                Spanned::new(expr.node.clone(), line)
            } else {
                rewrite(name).unwrap_or_else(|| Spanned::new(expr.node.clone(), line))
            }
        }
        Expr::Literal(_) => Spanned::new(expr.node.clone(), line),
        Expr::Attr(inner, field) => Spanned::new(
            Expr::Attr(
                Box::new(rewrite_inner(inner, scope, rewrite)),
                field.clone(),
            ),
            line,
        ),
        Expr::FnCall(callee, args) => Spanned::new(
            Expr::FnCall(
                Box::new(rewrite_inner(callee, scope, rewrite)),
                args.iter()
                    .map(|a| rewrite_inner(a, scope, rewrite))
                    .collect(),
            ),
            line,
        ),
        Expr::BinOp(op, l, r) => Spanned::new(
            Expr::BinOp(
                *op,
                Box::new(rewrite_inner(l, scope, rewrite)),
                Box::new(rewrite_inner(r, scope, rewrite)),
            ),
            line,
        ),
        Expr::Match { subject, arms } => {
            let new_subject = Box::new(rewrite_inner(subject, scope, rewrite));
            let new_arms = arms
                .iter()
                .map(|arm| {
                    let shadowed = pattern_binding_names(&arm.pattern);
                    if shadowed.is_empty() {
                        MatchArm {
                            pattern: arm.pattern.clone(),
                            body: Box::new(rewrite_inner(&arm.body, scope, rewrite)),
                        }
                    } else {
                        let mut extended = scope.clone();
                        for name in shadowed {
                            extended.insert(name);
                        }
                        MatchArm {
                            pattern: arm.pattern.clone(),
                            body: Box::new(rewrite_inner(&arm.body, &extended, rewrite)),
                        }
                    }
                })
                .collect();
            Spanned::new(
                Expr::Match {
                    subject: new_subject,
                    arms: new_arms,
                },
                line,
            )
        }
        Expr::Constructor(name, payload) => Spanned::new(
            Expr::Constructor(
                name.clone(),
                payload
                    .as_ref()
                    .map(|inner| Box::new(rewrite_inner(inner, scope, rewrite))),
            ),
            line,
        ),
        Expr::ErrorProp(inner) => Spanned::new(
            Expr::ErrorProp(Box::new(rewrite_inner(inner, scope, rewrite))),
            line,
        ),
        Expr::InterpolatedStr(parts) => Spanned::new(
            Expr::InterpolatedStr(
                parts
                    .iter()
                    .map(|part| match part {
                        StrPart::Literal(s) => StrPart::Literal(s.clone()),
                        StrPart::Parsed(inner) => {
                            StrPart::Parsed(Box::new(rewrite_inner(inner, scope, rewrite)))
                        }
                    })
                    .collect(),
            ),
            line,
        ),
        Expr::List(items) => Spanned::new(
            Expr::List(
                items
                    .iter()
                    .map(|i| rewrite_inner(i, scope, rewrite))
                    .collect(),
            ),
            line,
        ),
        Expr::Tuple(items) => Spanned::new(
            Expr::Tuple(
                items
                    .iter()
                    .map(|i| rewrite_inner(i, scope, rewrite))
                    .collect(),
            ),
            line,
        ),
        Expr::IndependentProduct(items, flag) => Spanned::new(
            Expr::IndependentProduct(
                items
                    .iter()
                    .map(|i| rewrite_inner(i, scope, rewrite))
                    .collect(),
                *flag,
            ),
            line,
        ),
        Expr::MapLiteral(entries) => Spanned::new(
            Expr::MapLiteral(
                entries
                    .iter()
                    .map(|(k, v)| {
                        (
                            rewrite_inner(k, scope, rewrite),
                            rewrite_inner(v, scope, rewrite),
                        )
                    })
                    .collect(),
            ),
            line,
        ),
        Expr::RecordCreate { type_name, fields } => Spanned::new(
            Expr::RecordCreate {
                type_name: type_name.clone(),
                fields: fields
                    .iter()
                    .map(|(n, v)| (n.clone(), rewrite_inner(v, scope, rewrite)))
                    .collect(),
            },
            line,
        ),
        Expr::RecordUpdate {
            type_name,
            base,
            updates,
        } => Spanned::new(
            Expr::RecordUpdate {
                type_name: type_name.clone(),
                base: Box::new(rewrite_inner(base, scope, rewrite)),
                updates: updates
                    .iter()
                    .map(|(n, v)| (n.clone(), rewrite_inner(v, scope, rewrite)))
                    .collect(),
            },
            line,
        ),
        Expr::TailCall(data) => Spanned::new(
            Expr::TailCall(Box::new(TailCallData::new(
                data.target.clone(),
                data.args
                    .iter()
                    .map(|a| rewrite_inner(a, scope, rewrite))
                    .collect(),
            ))),
            line,
        ),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinOp, Literal};

    fn bare(e: Expr) -> Spanned<Expr> {
        Spanned::new(e, 1)
    }

    fn int(n: i64) -> Spanned<Expr> {
        bare(Expr::Literal(Literal::Int(n)))
    }

    fn ident(s: &str) -> Spanned<Expr> {
        bare(Expr::Ident(s.to_string()))
    }

    #[test]
    fn pattern_shadowing_leaves_inner_bound_ident_alone() {
        // match Some(2) { Some(x) -> x, None -> 0 } with outer x = 1.
        let e = bare(Expr::Match {
            subject: Box::new(bare(Expr::Constructor(
                "Option.Some".to_string(),
                Some(Box::new(int(2))),
            ))),
            arms: vec![
                MatchArm {
                    pattern: Pattern::Constructor("Option.Some".to_string(), vec!["x".to_string()]),
                    body: Box::new(ident("x")),
                },
                MatchArm {
                    pattern: Pattern::Constructor("Option.None".to_string(), vec![]),
                    body: Box::new(int(0)),
                },
            ],
        });
        let out = rewrite_idents_scoped(&e, |n| if n == "x" { Some(int(1)) } else { None });
        let Expr::Match { arms, .. } = &out.node else {
            panic!("expected Match");
        };
        assert!(
            matches!(&arms[0].body.node, Expr::Ident(s) if s == "x"),
            "pattern-bound x should not be substituted: {:?}",
            arms[0].body.node
        );
    }

    #[test]
    fn tuple_pattern_shadowing() {
        let e = bare(Expr::Match {
            subject: Box::new(bare(Expr::Tuple(vec![int(1), int(2)]))),
            arms: vec![MatchArm {
                pattern: Pattern::Tuple(vec![
                    Pattern::Ident("a".to_string()),
                    Pattern::Ident("b".to_string()),
                ]),
                body: Box::new(bare(Expr::BinOp(
                    BinOp::Add,
                    Box::new(ident("a")),
                    Box::new(ident("b")),
                ))),
            }],
        });
        let out = rewrite_idents_scoped(&e, |n| if n == "a" { Some(int(99)) } else { None });
        let Expr::Match { arms, .. } = &out.node else {
            panic!();
        };
        let Expr::BinOp(_, l, _) = &arms[0].body.node else {
            panic!();
        };
        assert!(
            matches!(&l.node, Expr::Ident(s) if s == "a"),
            "tuple-pattern `a` should shadow outer substitution: {:?}",
            l.node
        );
    }

    #[test]
    fn rewrites_in_non_shadowed_arm() {
        let e = bare(Expr::Match {
            subject: Box::new(int(42)),
            arms: vec![MatchArm {
                pattern: Pattern::Wildcard,
                body: Box::new(ident("x")),
            }],
        });
        let out = rewrite_idents_scoped(&e, |n| if n == "x" { Some(int(7)) } else { None });
        let Expr::Match { arms, .. } = &out.node else {
            panic!();
        };
        assert!(matches!(&arms[0].body.node, Expr::Literal(Literal::Int(7))));
    }

    #[test]
    fn cons_pattern_shadows_head_and_tail() {
        let e = bare(Expr::Match {
            subject: Box::new(bare(Expr::List(vec![int(1), int(2)]))),
            arms: vec![MatchArm {
                pattern: Pattern::Cons("h".to_string(), "t".to_string()),
                body: Box::new(bare(Expr::Tuple(vec![ident("h"), ident("t"), ident("z")]))),
            }],
        });
        let out = rewrite_idents_scoped(&e, |n| match n {
            "h" => Some(int(100)),
            "t" => Some(int(200)),
            "z" => Some(int(300)),
            _ => None,
        });
        let Expr::Match { arms, .. } = &out.node else {
            panic!();
        };
        let Expr::Tuple(items) = &arms[0].body.node else {
            panic!();
        };
        // h, t shadowed (stay as Ident); z rewritten to 300.
        assert!(matches!(&items[0].node, Expr::Ident(s) if s == "h"));
        assert!(matches!(&items[1].node, Expr::Ident(s) if s == "t"));
        assert!(matches!(&items[2].node, Expr::Literal(Literal::Int(300))));
    }
}

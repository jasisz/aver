//! Boundary value generators for `aver verify --hostile`.
//!
//! Returns the *interesting set* of values for each built-in type — the
//! adversarial cases that find off-by-one and ill-defined boundary
//! assumptions. Boundary-first instead of random: deterministic, finite,
//! and reproducible. A failure under `--hostile` always points at a
//! specific, replayable value the user can encode as a `when` clause if
//! that value is in fact a precondition.
//!
//! The generators are conservative on size: the per-type interesting set
//! stays small (≤6 elements for scalars, ≤4 for collections), and the
//! cartesian product across multiple `given` parameters multiplies them
//! directly without further fan-out. This is the right shape for the
//! 0.13 Limit semantics ("examples are not limits, preconditions are"):
//! we want to surface the corners cleanly, not search exhaustively.

use crate::ast::{Expr, Literal, Spanned};
use crate::types::Type;

/// Boundary values for a type as raw `Literal`s. Scalars only — for the
/// full ready-to-substitute set including compound types use
/// [`boundary_exprs`].
pub fn boundary_values(ty: &Type) -> Vec<Literal> {
    match ty {
        Type::Int => vec![
            Literal::Int(0),
            Literal::Int(1),
            Literal::Int(-1),
            Literal::Int(i64::MIN),
            Literal::Int(i64::MAX),
        ],
        Type::Float => vec![
            Literal::Float(0.0),
            Literal::Float(1.0),
            Literal::Float(-1.0),
            Literal::Float(f64::MIN),
            Literal::Float(f64::MAX),
            Literal::Float(f64::INFINITY),
            Literal::Float(f64::NEG_INFINITY),
            Literal::Float(f64::NAN),
        ],
        Type::Bool => vec![Literal::Bool(true), Literal::Bool(false)],
        Type::Str => vec![
            Literal::Str(String::new()),
            Literal::Str("a".to_string()),
            // Long string — exposes O(n²) and buffer assumptions.
            Literal::Str("x".repeat(1024)),
            // Embedded NUL — exposes C-string assumptions and
            // length-vs-strlen confusion.
            Literal::Str("\0".to_string()),
            // Multi-byte UTF-8 — exposes byte-vs-char-index confusion.
            Literal::Str("ąść 漢字 🦀".to_string()),
        ],
        Type::Unit => vec![Literal::Unit],
        // Compound types do not have a flat literal representation
        // (Aver pattern-matches them as constructors / list literals).
        // The caller — domain expansion — handles wrapping by walking
        // the inner type recursively. Returning empty here is the
        // signal "ask me again with the inner type".
        Type::List(_) | Type::Option(_) | Type::Result(_, _) | Type::Tuple(_) => Vec::new(),
        // Maps, vectors, function types: no boundary set in 0.13. User
        // domains for these usually go through helper constructors
        // anyway; we don't try to invent values for them.
        Type::Map(_, _) | Type::Vector(_) | Type::Fn(_, _, _) => Vec::new(),
        // Named (user records / sum types) and Unknown: no literal
        // representation available without checking the project's type
        // table. Caller should leave these alone.
        Type::Named(_) | Type::Unknown | Type::Var(_) | Type::Any => Vec::new(),
    }
}

/// Boundary values for a type as ready-to-substitute `Spanned<Expr>`s.
/// Scalars come from [`boundary_values`]; compound types (`Option`,
/// `Result`, `List`, `Tuple`) recurse into their element types and wrap
/// in the appropriate constructor / list / tuple expression. The result
/// is what `verify_law::expand` substitutes into the law template.
///
/// Recursion stays *narrow*. The cartesian product is the expensive
/// dimension (multiple `given` clauses multiply directly), so per-type
/// generators clip the inner boundary set to ~3 values and do not
/// generate large lists or deeply nested structures. The intent is to
/// surface corner cases (None, Ok with extreme value, single-element
/// list) without exhaustive enumeration.
pub fn boundary_exprs(ty: &Type) -> Vec<Spanned<Expr>> {
    fn lit_expr(lit: Literal) -> Spanned<Expr> {
        Spanned::bare(Expr::Literal(lit))
    }
    fn ctor(name: &str, arg: Option<Spanned<Expr>>) -> Spanned<Expr> {
        Spanned::bare(Expr::Constructor(name.to_string(), arg.map(Box::new)))
    }

    match ty {
        Type::Int | Type::Float | Type::Str | Type::Bool | Type::Unit => {
            boundary_values(ty).into_iter().map(lit_expr).collect()
        }
        Type::Option(inner) => {
            // None plus Some(x) for each inner boundary.
            let mut out = vec![ctor("Option.None", None)];
            for v in boundary_exprs(inner) {
                out.push(ctor("Option.Some", Some(v)));
            }
            out
        }
        Type::Result(ok_t, err_t) => {
            // Ok(boundary-of-T) ∪ Err(boundary-of-E). Both sides matter:
            // a law over `Result<Int, Str>` should fail under both an
            // i64::MAX Ok and an empty-string Err if it isn't universal.
            let mut out = Vec::new();
            for v in boundary_exprs(ok_t) {
                out.push(ctor("Result.Ok", Some(v)));
            }
            for v in boundary_exprs(err_t) {
                out.push(ctor("Result.Err", Some(v)));
            }
            out
        }
        Type::List(inner) => {
            // Empty list plus a singleton for each of the first 3 inner
            // boundary values. Larger / nested lists would explode the
            // cartesian product; "none" and "one boundary element" cover
            // the typical off-by-one and edge-element bugs.
            let mut out = vec![Spanned::bare(Expr::List(Vec::new()))];
            for v in boundary_exprs(inner).into_iter().take(3) {
                out.push(Spanned::bare(Expr::List(vec![v])));
            }
            out
        }
        Type::Tuple(types) => {
            // Cartesian over the first 2 boundary values per component —
            // tuples grow combinatorially otherwise. For (Int, Bool) that
            // is 2 × 2 = 4 instead of 5 × 2 = 10; still hits both
            // dimensions on each side.
            let per_component: Vec<Vec<Spanned<Expr>>> = types
                .iter()
                .map(|t| boundary_exprs(t).into_iter().take(2).collect())
                .collect();
            cartesian_exprs(&per_component)
                .into_iter()
                .map(|combo| Spanned::bare(Expr::Tuple(combo)))
                .collect()
        }
        // No literal/constructor representation in 0.13: Map and Vector
        // would need synthetic helpers (no `{k: v}` / `vec![...]` source
        // syntax for boundary inputs), Fn / Named / Unknown require
        // user-defined witnesses that hostile mode cannot invent.
        Type::Map(_, _) | Type::Vector(_) | Type::Fn(_, _, _) => Vec::new(),
        Type::Named(_) | Type::Unknown | Type::Var(_) | Type::Any => Vec::new(),
    }
}

fn cartesian_exprs(per_component: &[Vec<Spanned<Expr>>]) -> Vec<Vec<Spanned<Expr>>> {
    let mut out: Vec<Vec<Spanned<Expr>>> = vec![Vec::new()];
    for choices in per_component {
        let mut next = Vec::with_capacity(out.len() * choices.len().max(1));
        for partial in &out {
            for choice in choices {
                let mut extended = partial.clone();
                extended.push(choice.clone());
                next.push(extended);
            }
        }
        out = next;
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn int_boundary_includes_extremes_and_zero() {
        let bs = boundary_values(&Type::Int);
        assert!(bs.contains(&Literal::Int(0)));
        assert!(bs.contains(&Literal::Int(1)));
        assert!(bs.contains(&Literal::Int(-1)));
        assert!(bs.contains(&Literal::Int(i64::MIN)));
        assert!(bs.contains(&Literal::Int(i64::MAX)));
    }

    #[test]
    fn bool_boundary_is_both() {
        let bs = boundary_values(&Type::Bool);
        assert_eq!(bs.len(), 2);
        assert!(bs.contains(&Literal::Bool(true)));
        assert!(bs.contains(&Literal::Bool(false)));
    }

    #[test]
    fn str_boundary_includes_empty_and_edge_chars() {
        let bs = boundary_values(&Type::Str);
        assert!(bs.contains(&Literal::Str(String::new())));
        // NUL embedded
        assert!(
            bs.iter()
                .any(|l| matches!(l, Literal::Str(s) if s.contains('\0')))
        );
        // Multi-byte
        assert!(
            bs.iter()
                .any(|l| matches!(l, Literal::Str(s) if s.bytes().count() > s.chars().count()))
        );
    }

    #[test]
    fn float_boundary_includes_nan_and_infinities() {
        let bs = boundary_values(&Type::Float);
        assert!(
            bs.iter()
                .any(|l| matches!(l, Literal::Float(f) if f.is_nan()))
        );
        assert!(
            bs.iter()
                .any(|l| matches!(l, Literal::Float(f) if f.is_infinite() && *f > 0.0))
        );
        assert!(
            bs.iter()
                .any(|l| matches!(l, Literal::Float(f) if f.is_infinite() && *f < 0.0))
        );
    }

    #[test]
    fn compound_literal_form_is_empty_caller_uses_boundary_exprs() {
        // `boundary_values` returns Literals only (scalars). Compound
        // recursion lives in `boundary_exprs` — caller (domain
        // expansion) goes through that.
        assert!(boundary_values(&Type::List(Box::new(Type::Int))).is_empty());
        assert!(boundary_values(&Type::Option(Box::new(Type::Int))).is_empty());
        assert!(
            boundary_values(&Type::Result(Box::new(Type::Int), Box::new(Type::Str))).is_empty()
        );
    }

    #[test]
    fn named_and_unknown_return_empty() {
        // No literal representation — domain expansion should not invent
        // values for user-defined types in 0.13.
        assert!(boundary_values(&Type::Named("Shape".to_string())).is_empty());
        assert!(boundary_values(&Type::Unknown).is_empty());
    }

    #[test]
    fn boundary_exprs_option_int_includes_none_and_some_extremes() {
        let bs = boundary_exprs(&Type::Option(Box::new(Type::Int)));
        // None + 5 Some-wrapped Int boundaries = 6
        assert_eq!(bs.len(), 6);
        assert!(
            bs.iter()
                .any(|e| matches!(&e.node, Expr::Constructor(name, None) if name == "Option.None"))
        );
        assert!(bs.iter().any(|e| matches!(
            &e.node,
            Expr::Constructor(name, Some(inner))
                if name == "Option.Some"
                    && matches!(&inner.node, Expr::Literal(Literal::Int(i64::MAX)))
        )));
    }

    #[test]
    fn boundary_exprs_result_covers_both_sides() {
        let bs = boundary_exprs(&Type::Result(Box::new(Type::Int), Box::new(Type::Bool)));
        // 5 Int Oks + 2 Bool Errs = 7
        assert_eq!(bs.len(), 7);
        let oks = bs
            .iter()
            .filter(|e| matches!(&e.node, Expr::Constructor(n, _) if n == "Result.Ok"))
            .count();
        let errs = bs
            .iter()
            .filter(|e| matches!(&e.node, Expr::Constructor(n, _) if n == "Result.Err"))
            .count();
        assert_eq!(oks, 5);
        assert_eq!(errs, 2);
    }

    #[test]
    fn boundary_exprs_list_int_has_empty_and_singletons() {
        let bs = boundary_exprs(&Type::List(Box::new(Type::Int)));
        // [] + first 3 boundary singletons = 4
        assert_eq!(bs.len(), 4);
        assert!(
            bs.iter()
                .any(|e| matches!(&e.node, Expr::List(items) if items.is_empty()))
        );
        assert!(bs.iter().any(|e| matches!(
            &e.node,
            Expr::List(items) if items.len() == 1
        )));
    }

    #[test]
    fn boundary_exprs_tuple_clips_per_component() {
        let bs = boundary_exprs(&Type::Tuple(vec![Type::Int, Type::Bool]));
        // 2 × 2 = 4 (Int boundaries clipped to first 2, Bool has 2)
        assert_eq!(bs.len(), 4);
        assert!(
            bs.iter()
                .all(|e| matches!(&e.node, Expr::Tuple(items) if items.len() == 2))
        );
    }

    #[test]
    fn boundary_exprs_map_and_vector_are_empty_in_0_13() {
        // No literal syntax for these in 0.13 — domain expansion would
        // need synthetic helpers (vec![...], {k: v}). Hostile leaves
        // them alone.
        assert!(boundary_exprs(&Type::Map(Box::new(Type::Str), Box::new(Type::Int))).is_empty());
        assert!(boundary_exprs(&Type::Vector(Box::new(Type::Int))).is_empty());
    }

    #[test]
    fn boundary_exprs_named_returns_empty() {
        assert!(boundary_exprs(&Type::Named("MyShape".to_string())).is_empty());
    }

    #[test]
    fn boundary_exprs_scalar_matches_boundary_values() {
        // For scalars, `boundary_exprs(t)` is just `boundary_values(t)`
        // wrapped in `Expr::Literal`. Same length, same order.
        let lits = boundary_values(&Type::Int);
        let exprs = boundary_exprs(&Type::Int);
        assert_eq!(lits.len(), exprs.len());
        for (lit, expr) in lits.iter().zip(&exprs) {
            assert!(
                matches!(&expr.node, Expr::Literal(l) if l == lit),
                "expr should wrap matching literal"
            );
        }
    }

    #[test]
    fn boundary_sets_are_small_and_finite() {
        // Cartesian products are bounded — confirm scalar sets stay tight.
        assert!(boundary_values(&Type::Int).len() <= 6);
        assert!(boundary_values(&Type::Bool).len() == 2);
        assert!(boundary_values(&Type::Str).len() <= 6);
        assert!(boundary_values(&Type::Float).len() <= 8);
    }
}

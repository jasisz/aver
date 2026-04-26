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

use crate::ast::Literal;
use crate::types::Type;

/// Boundary values for a type. Returns the *interesting set* — values
/// that are most likely to expose off-by-one, sign, overflow, or empty-
/// case assumptions in pure laws. Compound types recurse into their
/// element types.
///
/// Returns expressions as raw `Literal` values; the caller is
/// responsible for wrapping them in the appropriate `Expr` variants
/// (e.g. `Option.None` / `Option.Some(...)` / `Result.Ok(...)` /
/// `Result.Err(...)`) when expanding `given` domains.
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
        Type::Named(_) | Type::Unknown => Vec::new(),
    }
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
    fn compound_types_return_empty_for_caller_to_recurse() {
        // Compound types: caller (domain expansion) must walk the inner
        // structure itself — there's no flat literal for `Some(x)`.
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
    fn boundary_sets_are_small_and_finite() {
        // Cartesian products are bounded — confirm scalar sets stay tight.
        assert!(boundary_values(&Type::Int).len() <= 6);
        assert!(boundary_values(&Type::Bool).len() == 2);
        assert!(boundary_values(&Type::Str).len() <= 6);
        assert!(boundary_values(&Type::Float).len() <= 8);
    }
}

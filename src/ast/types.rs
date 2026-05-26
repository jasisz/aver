//! Aver static type representation.
//!
//! Lives under `crate::ast` so that `Spanned<T>` can carry an optional
//! `Type` annotation without forming a cycle through `crate::types`
//! (which depends on `crate::ast`). The original `crate::types::Type`
//! is re-exported from here for backward compatibility.

use crate::ir::TypeId;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Str,
    Bool,
    Unit,
    Result(Box<Type>, Box<Type>),
    Option(Box<Type>),
    List(Box<Type>),
    Tuple(Vec<Type>),
    Map(Box<Type>, Box<Type>),
    Vector(Box<Type>),
    Fn(Vec<Type>, Box<Type>, Vec<String>),
    Var(String), // named type variable in polymorphic builtin signatures (instantiated at call site)
    Invalid, // checker recovery after an earlier error; matches anything in `.compatible` to suppress cascading diagnostics
    /// User-defined or builtin named type. The `id` is `Some` once the
    /// typechecker has resolved the reference against the program's
    /// [`crate::ir::SymbolTable`] (#138 phase B). The `id` is `None` for
    /// transient parser output (before typecheck has run), for builtin
    /// record types (`HttpResponse`, `Header`, `Tcp.Connection`,
    /// `Buffer`, …) that aren't registered in the user-program symbol
    /// table, and for stamps the checker couldn't resolve.
    ///
    /// Identity:
    /// - two `Named` with both `id = Some` compare by `id` (typed
    ///   identity — cross-module same-bare-name types stay distinct);
    /// - otherwise fall back to source-faithful name comparison, with
    ///   the historical suffix tolerance for `Bare` vs `Module.Bare`.
    Named {
        id: Option<TypeId>,
        name: String,
    },
}

impl Type {
    /// Build a `Type::Named` whose identity has not been resolved
    /// against a `SymbolTable` (parser output, builtins, in-flight
    /// stamps). Equivalent to `Named { id: None, name: name.into() }`.
    pub fn named(name: impl Into<String>) -> Self {
        Self::Named {
            id: None,
            name: name.into(),
        }
    }

    /// Build a `Type::Named` resolved against the program's
    /// `SymbolTable`. Caller is responsible for ensuring `name` is the
    /// canonical form (`symbol_table.type_entry(id).key.canonical()`).
    pub fn named_resolved(id: TypeId, name: impl Into<String>) -> Self {
        Self::Named {
            id: Some(id),
            name: name.into(),
        }
    }

    /// Source-faithful name of a `Named` (`"Shape"` / `"A.Shape"` /
    /// `"HttpResponse"`); `None` for any non-`Named` variant.
    pub fn named_name(&self) -> Option<&str> {
        match self {
            Type::Named { name, .. } => Some(name.as_str()),
            _ => None,
        }
    }

    /// Resolved `TypeId` of a `Named`, if any. `None` for unresolved
    /// references and for any non-`Named` variant.
    pub fn named_id(&self) -> Option<TypeId> {
        match self {
            Type::Named { id, .. } => *id,
            _ => None,
        }
    }

    /// `a.compatible(b)` — can a value of type `self` be used where `other` is expected?
    /// Two concrete types must be equal (structurally) to be compatible. Type variables
    /// are resolved by the type checker at call sites, not by this raw relation.
    ///
    /// Iron — A4: `Type::Invalid` is the checker's "already-errored"
    /// sentinel and matches anything. Without this, a single bad
    /// expression would fan its `Invalid` type out through every
    /// downstream `.compatible(...)` check and produce a chain of
    /// duplicate diagnostics.
    pub fn compatible(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Invalid, _) | (_, Type::Invalid) => true,
            (Type::Int, Type::Int) => true,
            (Type::Float, Type::Float) => true,
            (Type::Str, Type::Str) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::Unit, Type::Unit) => true,
            (Type::Var(a), Type::Var(b)) => a == b,
            (Type::Result(a1, b1), Type::Result(a2, b2)) => a1.compatible(a2) && b1.compatible(b2),
            (Type::Option(a), Type::Option(b)) => a.compatible(b),
            (Type::List(a), Type::List(b)) => a.compatible(b),
            (Type::Tuple(a), Type::Tuple(b)) => {
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| x.compatible(y))
            }
            (Type::Map(k1, v1), Type::Map(k2, v2)) => k1.compatible(k2) && v1.compatible(v2),
            (Type::Vector(a), Type::Vector(b)) => a.compatible(b),
            (Type::Fn(p1, r1, e1), Type::Fn(p2, r2, e2)) => {
                p1.len() == p2.len()
                    && p1.iter().zip(p2.iter()).all(|(a, b)| a.compatible(b))
                    && r1.compatible(r2)
                    && e1.iter().all(|actual| {
                        e2.iter()
                            .any(|expected| crate::effects::effect_satisfies(expected, actual))
                    })
            }
            (
                Type::Named {
                    id: id_a,
                    name: name_a,
                },
                Type::Named {
                    id: id_b,
                    name: name_b,
                },
            ) => match (id_a, id_b) {
                // Both sides resolved against the symbol table: typed
                // identity is the load-bearing comparison. Two distinct
                // `TypeId`s never compare equal even when their source
                // names happen to coincide (cross-module `Shape` —
                // distinct `TypeId`, must stay incompatible).
                (Some(a), Some(b)) => a == b,
                // Exactly one side carries a `TypeId`: the typechecker
                // already classified that side, the other side is a
                // raw stamp (parser output / builtin / in-flight
                // recovery). Phase B post-review tightens this branch:
                // the pre-phase-B suffix fallback would have happily
                // matched `Named { Some(A_Shape), "A.Shape" }` against
                // `Named { None, "Shape" }`, re-introducing the
                // cross-module collapse the typed identity is meant to
                // prevent. Require strict-name equality so a Some-id
                // side never matches a string that could canonicalise
                // to a different ID.
                (Some(_), None) | (None, Some(_)) => name_a == name_b,
                // Both sides unresolved (raw stamps, builtin records,
                // tests). Keep the historical suffix relation as a
                // best-effort match — there's no typed identity to
                // disagree with on either side.
                (None, None) => {
                    name_a == name_b
                        || name_a.ends_with(&format!(".{}", name_b))
                        || name_b.ends_with(&format!(".{}", name_a))
                }
            },
            _ => false,
        }
    }

    pub fn display(&self) -> String {
        match self {
            Type::Int => "Int".to_string(),
            Type::Float => "Float".to_string(),
            Type::Str => "String".to_string(),
            Type::Bool => "Bool".to_string(),
            Type::Unit => "Unit".to_string(),
            Type::Result(ok, err) => format!("Result<{}, {}>", ok.display(), err.display()),
            Type::Option(inner) => format!("Option<{}>", inner.display()),
            Type::List(inner) => format!("List<{}>", inner.display()),
            Type::Tuple(items) => format!(
                "Tuple<{}>",
                items
                    .iter()
                    .map(Type::display)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Type::Map(key, value) => format!("Map<{}, {}>", key.display(), value.display()),
            Type::Vector(inner) => format!("Vector<{}>", inner.display()),
            Type::Fn(params, ret, effects) => {
                let ps: Vec<String> = params.iter().map(|p| p.display()).collect();
                if effects.is_empty() {
                    format!("Fn({}) -> {}", ps.join(", "), ret.display())
                } else {
                    format!(
                        "Fn({}) -> {} ! [{}]",
                        ps.join(", "),
                        ret.display(),
                        effects.join(", ")
                    )
                }
            }
            Type::Var(name) => name.clone(),
            Type::Invalid => "Invalid".to_string(),
            Type::Named { name, .. } => name.clone(),
        }
    }
}

/// Aver static type representation.
///
/// Lives under `crate::ast` so that `Spanned<T>` can carry an optional
/// `Type` annotation without forming a cycle through `crate::types`
/// (which depends on `crate::ast`). The original `crate::types::Type`
/// is re-exported from here for backward compatibility.

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
    Invalid,     // checker recovery after an earlier error; matches anything in `.compatible` to suppress cascading diagnostics
    Named(String), // user-defined type: Shape, User, etc.
}

impl Type {
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
            (Type::Named(a), Type::Named(b)) => {
                a == b || a.ends_with(&format!(".{}", b)) || b.ends_with(&format!(".{}", a))
            }
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
            Type::Named(n) => n.clone(),
        }
    }
}

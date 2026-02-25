/// Aver static type representation.
/// Type annotations in the AST are plain strings; this module converts them
/// to a structured enum and provides the compatibility relation used by
/// the type checker.

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
    Fn(Vec<Type>, Box<Type>),
    Any, // unknown / gradual-typing escape hatch
    Named(String), // user-defined type: Shape, User, etc.
}

impl Type {
    /// `a.compatible(b)` — can a value of type `self` be used where `other` is expected?
    /// `Any` is compatible with everything (gradual typing).
    /// Two concrete types must be equal (structurally) to be compatible.
    pub fn compatible(&self, other: &Type) -> bool {
        if matches!(self, Type::Any) || matches!(other, Type::Any) {
            return true;
        }
        match (self, other) {
            (Type::Int, Type::Int) => true,
            (Type::Float, Type::Float) => true,
            // Allow Int where Float expected (widening)
            (Type::Int, Type::Float) => true,
            (Type::Str, Type::Str) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::Unit, Type::Unit) => true,
            (Type::Result(a1, b1), Type::Result(a2, b2)) => {
                a1.compatible(a2) && b1.compatible(b2)
            }
            (Type::Option(a), Type::Option(b)) => a.compatible(b),
            (Type::List(a), Type::List(b)) => a.compatible(b),
            (Type::Fn(p1, r1), Type::Fn(p2, r2)) => {
                p1.len() == p2.len()
                    && p1.iter().zip(p2.iter()).all(|(a, b)| a.compatible(b))
                    && r1.compatible(r2)
            }
            (Type::Named(a), Type::Named(b)) => a == b,
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
            Type::Fn(params, ret) => {
                let ps: Vec<String> = params.iter().map(|p| p.display()).collect();
                format!("Fn({}) -> {}", ps.join(", "), ret.display())
            }
            Type::Any => "Any".to_string(),
            Type::Named(n) => n.clone(),
        }
    }
}

/// Parse a type annotation string strictly.
/// Returns `Err(unknown_name)` if the string is a non-empty, non-`Any` identifier
/// that does not map to a known type (i.e. a likely typo).
/// Generic forms (`Result<...>`, `Option<...>`, `List<...>`) with valid inner types are accepted.
pub fn parse_type_str_strict(s: &str) -> Result<Type, String> {
    let s = s.trim();
    if s.is_empty() || s == "Any" {
        return Ok(Type::Any);
    }
    match s {
        "Int" => Ok(Type::Int),
        "Float" => Ok(Type::Float),
        "String" | "Str" => Ok(Type::Str),
        "Bool" => Ok(Type::Bool),
        "Unit" => Ok(Type::Unit),
        "Any" => Ok(Type::Any),
        _ => {
            if let Some(inner) = strip_wrapper(s, "Result<", ">") {
                if let Some((ok_s, err_s)) = split_top_level_comma(inner) {
                    let ok_ty = parse_type_str_strict(ok_s)?;
                    let err_ty = parse_type_str_strict(err_s)?;
                    return Ok(Type::Result(Box::new(ok_ty), Box::new(err_ty)));
                }
                return Err(s.to_string());
            }
            if let Some(inner) = strip_wrapper(s, "Option<", ">") {
                let inner_ty = parse_type_str_strict(inner)?;
                return Ok(Type::Option(Box::new(inner_ty)));
            }
            if let Some(inner) = strip_wrapper(s, "List<", ">") {
                let inner_ty = parse_type_str_strict(inner)?;
                return Ok(Type::List(Box::new(inner_ty)));
            }

            // Capitalized identifier with only alphanumeric/_chars = user-defined type name
            if s.chars().next().map_or(false, |c| c.is_uppercase())
                && s.chars().all(|c| c.is_alphanumeric() || c == '_')
            {
                return Ok(Type::Named(s.to_string()));
            }

            Err(s.to_string())
        }
    }
}

/// Parse an Aver type annotation string into a `Type`.
/// Returns `Type::Any` for unknown identifiers (gradual typing fallback).
/// Prefer `parse_type_str_strict` for user-facing type annotations.
pub fn parse_type_str(s: &str) -> Type {
    let s = s.trim();
    match s {
        "Int" => Type::Int,
        "Float" => Type::Float,
        "String" | "Str" => Type::Str,
        "Bool" => Type::Bool,
        "Unit" => Type::Unit,
        "Any" | "" => Type::Any,
        _ => {
            // Try generic forms: Result<A, B>, Option<A>, List<A>
            if let Some(inner) = strip_wrapper(s, "Result<", ">") {
                // Split on the first top-level comma
                if let Some((ok_str, err_str)) = split_top_level_comma(inner) {
                    return Type::Result(
                        Box::new(parse_type_str(ok_str)),
                        Box::new(parse_type_str(err_str)),
                    );
                }
            }
            if let Some(inner) = strip_wrapper(s, "Option<", ">") {
                return Type::Option(Box::new(parse_type_str(inner)));
            }
            if let Some(inner) = strip_wrapper(s, "List<", ">") {
                return Type::List(Box::new(parse_type_str(inner)));
            }
            // Capitalized identifier with only alphanumeric/_chars = user-defined type
            if s.chars().next().map_or(false, |c| c.is_uppercase())
                && s.chars().all(|c| c.is_alphanumeric() || c == '_')
            {
                return Type::Named(s.to_string());
            }
            // Unknown — gradual typing escape hatch
            Type::Any
        }
    }
}

/// If `s` starts with `prefix` and ends with `suffix`, return the middle part.
fn strip_wrapper<'a>(s: &'a str, prefix: &str, suffix: &str) -> Option<&'a str> {
    if s.starts_with(prefix) && s.ends_with(suffix) {
        let inner = &s[prefix.len()..s.len() - suffix.len()];
        Some(inner)
    } else {
        None
    }
}

/// Split a string on the first top-level comma (depth=0), returning the two sides.
fn split_top_level_comma(s: &str) -> Option<(&str, &str)> {
    let mut depth = 0usize;
    for (i, ch) in s.char_indices() {
        match ch {
            '<' => depth += 1,
            '>' => {
                if depth > 0 {
                    depth -= 1;
                }
            }
            ',' if depth == 0 => {
                return Some((&s[..i], &s[i + 1..]));
            }
            _ => {}
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_primitives() {
        assert_eq!(parse_type_str("Int"), Type::Int);
        assert_eq!(parse_type_str("Float"), Type::Float);
        assert_eq!(parse_type_str("String"), Type::Str);
        assert_eq!(parse_type_str("Bool"), Type::Bool);
        assert_eq!(parse_type_str("Unit"), Type::Unit);
    }

    #[test]
    fn test_generics() {
        assert_eq!(
            parse_type_str("Result<Int, String>"),
            Type::Result(Box::new(Type::Int), Box::new(Type::Str))
        );
        assert_eq!(
            parse_type_str("Option<Bool>"),
            Type::Option(Box::new(Type::Bool))
        );
        assert_eq!(
            parse_type_str("List<Int>"),
            Type::List(Box::new(Type::Int))
        );
    }

    #[test]
    fn test_nested() {
        assert_eq!(
            parse_type_str("Result<Float, String>"),
            Type::Result(Box::new(Type::Float), Box::new(Type::Str))
        );
    }

    #[test]
    fn test_unknown() {
        // Capitalized identifiers are now parsed as user-defined Named types
        assert_eq!(parse_type_str("SomeUnknownType"), Type::Named("SomeUnknownType".to_string()));
        // Lowercase non-keyword identifiers and empty strings remain Any
        assert_eq!(parse_type_str(""), Type::Any);
    }

    #[test]
    fn test_compatible() {
        assert!(Type::Int.compatible(&Type::Int));
        assert!(!Type::Int.compatible(&Type::Str));
        assert!(Type::Any.compatible(&Type::Int));
        assert!(Type::Int.compatible(&Type::Any));
        assert!(Type::Int.compatible(&Type::Float)); // widening
        assert!(
            Type::Result(Box::new(Type::Int), Box::new(Type::Str))
                .compatible(&Type::Result(Box::new(Type::Int), Box::new(Type::Str)))
        );
    }

    #[test]
    fn test_strict_parser_accepts_valid_generics() {
        assert_eq!(
            parse_type_str_strict("Result<Int, String>").unwrap(),
            Type::Result(Box::new(Type::Int), Box::new(Type::Str))
        );
        assert_eq!(
            parse_type_str_strict("List<Option<Float>>").unwrap(),
            Type::List(Box::new(Type::Option(Box::new(Type::Float))))
        );
    }

    #[test]
    fn test_strict_parser_accepts_user_defined_types() {
        // Capitalized identifiers are accepted as user-defined Named types
        assert_eq!(
            parse_type_str_strict("Result<MyError, String>").unwrap(),
            Type::Result(Box::new(Type::Named("MyError".to_string())), Box::new(Type::Str))
        );
        assert_eq!(
            parse_type_str_strict("Option<Shape>").unwrap(),
            Type::Option(Box::new(Type::Named("Shape".to_string())))
        );
        assert_eq!(
            parse_type_str_strict("List<User>").unwrap(),
            Type::List(Box::new(Type::Named("User".to_string())))
        );
        // Lowercase unknown types still fail
        assert!(parse_type_str_strict("integ").is_err());
    }

    #[test]
    fn test_strict_parser_rejects_malformed_generics() {
        assert!(parse_type_str_strict("Result<Int>").is_err());
        assert!(parse_type_str_strict("Option<Int, String>").is_err());
    }
}

/// Aver static type representation and built-in type namespaces.
///
/// Type annotations in the AST are plain strings; this module converts them
/// to a structured enum and provides the compatibility relation used by
/// the type checker.
///
/// Sub-modules:
/// - `checker` — static type checker
/// - `int`, `float`, `string`, `list` — pure namespace helpers (no effects)
pub mod checker;
pub mod float;
pub mod int;
pub mod list;
pub mod string;

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
    Fn(Vec<Type>, Box<Type>, Vec<String>),
    Unknown,       // internal fallback when checker cannot infer a precise type
    Named(String), // user-defined type: Shape, User, etc.
}

impl Type {
    /// `a.compatible(b)` — can a value of type `self` be used where `other` is expected?
    /// `Unknown` is compatible with everything (internal fallback).
    /// Two concrete types must be equal (structurally) to be compatible.
    pub fn compatible(&self, other: &Type) -> bool {
        if matches!(self, Type::Unknown) || matches!(other, Type::Unknown) {
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
            (Type::Result(a1, b1), Type::Result(a2, b2)) => a1.compatible(a2) && b1.compatible(b2),
            (Type::Option(a), Type::Option(b)) => a.compatible(b),
            (Type::List(a), Type::List(b)) => a.compatible(b),
            (Type::Fn(p1, r1, e1), Type::Fn(p2, r2, e2)) => {
                p1.len() == p2.len()
                    && p1.iter().zip(p2.iter()).all(|(a, b)| a.compatible(b))
                    && r1.compatible(r2)
                    && e1.iter().all(|eff| e2.contains(eff))
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
            Type::Unknown => "Unknown".to_string(),
            Type::Named(n) => n.clone(),
        }
    }
}

/// Parse a type annotation string strictly.
/// Returns `Err(unknown_name)` if the string is a non-empty identifier
/// that does not map to a known type (i.e. a likely typo).
/// Generic forms (`Result<...>`, `Option<...>`, `List<...>`) with valid inner types are accepted.
pub fn parse_type_str_strict(s: &str) -> Result<Type, String> {
    let s = s.trim();
    if s.is_empty() || s == "Any" {
        return Err(s.to_string());
    }
    if let Some(fn_ty) = parse_fn_type_strict(s)? {
        return Ok(fn_ty);
    }
    match s {
        "Int" => Ok(Type::Int),
        "Float" => Ok(Type::Float),
        "String" | "Str" => Ok(Type::Str),
        "Bool" => Ok(Type::Bool),
        "Unit" => Ok(Type::Unit),
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

            // Capitalized identifier with only alphanumeric/_ and dot chars = user-defined type name
            // Supports dotted names like "Tcp.Connection"
            if s.chars().next().map_or(false, |c| c.is_uppercase())
                && s.chars()
                    .all(|c| c.is_alphanumeric() || c == '_' || c == '.')
            {
                return Ok(Type::Named(s.to_string()));
            }

            Err(s.to_string())
        }
    }
}

/// Parse an Aver type annotation string into a `Type`.
/// Returns `Type::Unknown` for unknown identifiers (internal fallback).
/// Prefer `parse_type_str_strict` for user-facing type annotations.
pub fn parse_type_str(s: &str) -> Type {
    let s = s.trim();
    if s.starts_with("Fn(") {
        if let Ok(Some(fn_ty)) = parse_fn_type_strict(s) {
            return fn_ty;
        }
        return Type::Unknown;
    }
    match s {
        "Int" => Type::Int,
        "Float" => Type::Float,
        "String" | "Str" => Type::Str,
        "Bool" => Type::Bool,
        "Unit" => Type::Unit,
        "" => Type::Unknown,
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
            // Capitalized identifier with only alphanumeric/_ and dot chars = user-defined type
            // Supports dotted names like "Tcp.Connection"
            if s.chars().next().map_or(false, |c| c.is_uppercase())
                && s.chars()
                    .all(|c| c.is_alphanumeric() || c == '_' || c == '.')
                && s != "Any"
            {
                return Type::Named(s.to_string());
            }
            // Unknown — internal fallback
            Type::Unknown
        }
    }
}

fn parse_fn_type_strict(s: &str) -> Result<Option<Type>, String> {
    if !s.starts_with("Fn(") {
        return Ok(None);
    }

    let close_idx = find_matching_paren(s, 2).ok_or_else(|| s.to_string())?;
    let params_src = &s[3..close_idx];

    let after_params = s[close_idx + 1..].trim_start();
    if !after_params.starts_with("->") {
        return Err(s.to_string());
    }
    let ret_and_effects = after_params[2..].trim();
    if ret_and_effects.is_empty() {
        return Err(s.to_string());
    }

    let (ret_src, effects) = split_fn_effects_suffix(ret_and_effects)?;
    let ret_ty = parse_type_str_strict(ret_src)?;
    let params = parse_type_list_strict(params_src)?;
    Ok(Some(Type::Fn(params, Box::new(ret_ty), effects)))
}

fn parse_type_list_strict(src: &str) -> Result<Vec<Type>, String> {
    if src.trim().is_empty() {
        return Ok(vec![]);
    }
    split_top_level(src, ',')?
        .into_iter()
        .map(|part| {
            let part = part.trim();
            if part.is_empty() {
                Err(src.to_string())
            } else {
                parse_type_str_strict(part)
            }
        })
        .collect()
}

fn split_fn_effects_suffix(src: &str) -> Result<(&str, Vec<String>), String> {
    if let Some(bang_idx) = find_top_level_bang(src) {
        let ret_src = src[..bang_idx].trim();
        if ret_src.is_empty() {
            return Err(src.to_string());
        }
        let effects_src = src[bang_idx + 1..].trim();
        if !(effects_src.starts_with('[') && effects_src.ends_with(']')) {
            return Err(src.to_string());
        }
        let inner = &effects_src[1..effects_src.len() - 1];
        let effects = if inner.trim().is_empty() {
            vec![]
        } else {
            split_top_level(inner, ',')?
                .into_iter()
                .map(|part| {
                    let name = part.trim();
                    if name.is_empty() {
                        Err(src.to_string())
                    } else {
                        Ok(name.to_string())
                    }
                })
                .collect::<Result<Vec<_>, _>>()?
        };
        Ok((ret_src, effects))
    } else {
        Ok((src.trim(), vec![]))
    }
}

fn find_matching_paren(s: &str, open_idx: usize) -> Option<usize> {
    if s.as_bytes().get(open_idx).copied() != Some(b'(') {
        return None;
    }
    let mut depth = 1usize;
    for (i, ch) in s.char_indices().skip(open_idx + 1) {
        match ch {
            '(' => depth += 1,
            ')' => {
                depth -= 1;
                if depth == 0 {
                    return Some(i);
                }
            }
            _ => {}
        }
    }
    None
}

fn find_top_level_bang(s: &str) -> Option<usize> {
    let mut angle = 0usize;
    let mut paren = 0usize;
    let mut bracket = 0usize;

    for (i, ch) in s.char_indices() {
        match ch {
            '<' => angle += 1,
            '>' => angle = angle.saturating_sub(1),
            '(' => paren += 1,
            ')' => paren = paren.saturating_sub(1),
            '[' => bracket += 1,
            ']' => bracket = bracket.saturating_sub(1),
            '!' if angle == 0 && paren == 0 && bracket == 0 => return Some(i),
            _ => {}
        }
    }

    None
}

fn split_top_level<'a>(s: &'a str, delimiter: char) -> Result<Vec<&'a str>, String> {
    let mut out = Vec::new();
    let mut start = 0usize;
    let mut angle = 0usize;
    let mut paren = 0usize;
    let mut bracket = 0usize;

    for (i, ch) in s.char_indices() {
        match ch {
            '<' => angle += 1,
            '>' => {
                if angle == 0 {
                    return Err(s.to_string());
                }
                angle -= 1;
            }
            '(' => paren += 1,
            ')' => {
                if paren == 0 {
                    return Err(s.to_string());
                }
                paren -= 1;
            }
            '[' => bracket += 1,
            ']' => {
                if bracket == 0 {
                    return Err(s.to_string());
                }
                bracket -= 1;
            }
            _ if ch == delimiter && angle == 0 && paren == 0 && bracket == 0 => {
                out.push(&s[start..i]);
                start = i + ch.len_utf8();
            }
            _ => {}
        }
    }

    if angle != 0 || paren != 0 || bracket != 0 {
        return Err(s.to_string());
    }
    out.push(&s[start..]);
    Ok(out)
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
    let mut angle = 0usize;
    let mut paren = 0usize;
    let mut bracket = 0usize;
    for (i, ch) in s.char_indices() {
        match ch {
            '<' => angle += 1,
            '>' => angle = angle.saturating_sub(1),
            '(' => paren += 1,
            ')' => paren = paren.saturating_sub(1),
            '[' => bracket += 1,
            ']' => bracket = bracket.saturating_sub(1),
            ',' if angle == 0 && paren == 0 && bracket == 0 => {
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
        assert_eq!(parse_type_str("List<Int>"), Type::List(Box::new(Type::Int)));
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
        assert_eq!(
            parse_type_str("SomeUnknownType"),
            Type::Named("SomeUnknownType".to_string())
        );
        // Lowercase non-keyword identifiers and empty strings become Unknown fallback
        assert_eq!(parse_type_str(""), Type::Unknown);
    }

    #[test]
    fn test_compatible() {
        assert!(Type::Int.compatible(&Type::Int));
        assert!(!Type::Int.compatible(&Type::Str));
        assert!(Type::Unknown.compatible(&Type::Int));
        assert!(Type::Int.compatible(&Type::Unknown));
        assert!(Type::Int.compatible(&Type::Float)); // widening
        assert!(Type::Result(Box::new(Type::Int), Box::new(Type::Str))
            .compatible(&Type::Result(Box::new(Type::Int), Box::new(Type::Str))));
    }

    #[test]
    fn test_function_type_parsing() {
        assert_eq!(
            parse_type_str_strict("Fn(Int, String) -> Bool").unwrap(),
            Type::Fn(vec![Type::Int, Type::Str], Box::new(Type::Bool), vec![])
        );
        assert_eq!(
            parse_type_str_strict("Fn(Int) -> Int ! [Console]").unwrap(),
            Type::Fn(
                vec![Type::Int],
                Box::new(Type::Int),
                vec!["Console".to_string()]
            )
        );
    }

    #[test]
    fn test_function_effect_compatibility_subset() {
        let pure = Type::Fn(vec![Type::Int], Box::new(Type::Int), vec![]);
        let console = Type::Fn(
            vec![Type::Int],
            Box::new(Type::Int),
            vec!["Console".to_string()],
        );

        assert!(pure.compatible(&console));
        assert!(!console.compatible(&pure));
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
            Type::Result(
                Box::new(Type::Named("MyError".to_string())),
                Box::new(Type::Str)
            )
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
    fn test_dotted_named_type() {
        assert_eq!(
            parse_type_str("Tcp.Connection"),
            Type::Named("Tcp.Connection".to_string())
        );
        assert_eq!(
            parse_type_str_strict("Tcp.Connection").unwrap(),
            Type::Named("Tcp.Connection".to_string())
        );
        assert_eq!(
            parse_type_str_strict("Result<Tcp.Connection, String>").unwrap(),
            Type::Result(
                Box::new(Type::Named("Tcp.Connection".to_string())),
                Box::new(Type::Str)
            )
        );
    }

    #[test]
    fn test_strict_parser_rejects_malformed_generics() {
        assert!(parse_type_str_strict("Result<Int>").is_err());
        assert!(parse_type_str_strict("Option<Int, String>").is_err());
        assert!(parse_type_str_strict("Fn(Int) Int").is_err());
        assert!(parse_type_str_strict("Fn(Int) -> ! [Console]").is_err());
    }
}

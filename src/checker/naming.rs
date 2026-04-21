//! Naming-convention warnings.
//!
//! Aver's naming conventions (empirical from `examples/` and the Aver
//! language reference):
//!   - functions / bindings / record fields: camelCase
//!   - types / modules / variants: PascalCase
//!
//! This collector emits check warnings when a declaration breaks the
//! convention. It never auto-fixes — renames would break all call
//! sites, so the repair hint points the user at a manual rename.

use crate::ast::{FnDef, Module, TopLevel, TypeDef};

use super::CheckFinding;

pub fn collect_naming_warnings(items: &[TopLevel]) -> Vec<CheckFinding> {
    collect_naming_warnings_in(items, None)
}

pub fn collect_naming_warnings_in(
    items: &[TopLevel],
    source_file: Option<&str>,
) -> Vec<CheckFinding> {
    let module_name = items.iter().find_map(|item| match item {
        TopLevel::Module(m) => Some(m.name.clone()),
        _ => None,
    });

    let mut out = Vec::new();

    for item in items {
        match item {
            TopLevel::Module(m) => check_module(m, &mut out, &module_name, source_file),
            TopLevel::FnDef(fd) => check_fn(fd, &mut out, &module_name, source_file),
            TopLevel::TypeDef(td) => check_type(td, &mut out, &module_name, source_file),
            _ => {}
        }
    }

    out
}

fn check_module(
    m: &Module,
    out: &mut Vec<CheckFinding>,
    module_name: &Option<String>,
    source_file: Option<&str>,
) {
    if !is_pascal_case(&m.name) {
        out.push(CheckFinding {
            line: m.line,
            module: module_name.clone(),
            file: source_file.map(|s| s.to_string()),
            fn_name: None,
            message: format!(
                "Module '{}' should use PascalCase — e.g. '{}'",
                m.name,
                to_pascal_case(&m.name)
            ),
            extra_spans: vec![],
        });
    }
}

fn check_fn(
    fd: &FnDef,
    out: &mut Vec<CheckFinding>,
    module_name: &Option<String>,
    source_file: Option<&str>,
) {
    // Skip generated / internal helpers (`__verify_*`, `__replay_*`, etc.).
    if fd.name.starts_with("__") {
        return;
    }
    if !is_camel_case(&fd.name) {
        out.push(CheckFinding {
            line: fd.line,
            module: module_name.clone(),
            file: source_file.map(|s| s.to_string()),
            fn_name: Some(fd.name.clone()),
            message: format!(
                "Function '{}' should use camelCase — e.g. '{}'",
                fd.name,
                to_camel_case(&fd.name)
            ),
            extra_spans: vec![],
        });
    }
}

fn check_type(
    td: &TypeDef,
    out: &mut Vec<CheckFinding>,
    module_name: &Option<String>,
    source_file: Option<&str>,
) {
    let (name, line, fields_or_variants): (&str, usize, Vec<(&str, usize)>) = match td {
        TypeDef::Sum { name, line, variants, .. } => (
            name,
            *line,
            variants
                .iter()
                .map(|v| (v.name.as_str(), *line))
                .collect(),
        ),
        TypeDef::Product { name, line, fields, .. } => (
            name,
            *line,
            fields
                .iter()
                .map(|(n, _)| (n.as_str(), *line))
                .collect(),
        ),
    };

    if !is_pascal_case(name) {
        out.push(CheckFinding {
            line,
            module: module_name.clone(),
            file: source_file.map(|s| s.to_string()),
            fn_name: None,
            message: format!(
                "Type '{}' should use PascalCase — e.g. '{}'",
                name,
                to_pascal_case(name)
            ),
            extra_spans: vec![],
        });
    }

    if let TypeDef::Sum { .. } = td {
        for (variant, vline) in fields_or_variants {
            if !is_pascal_case(variant) {
                out.push(CheckFinding {
                    line: vline,
                    module: module_name.clone(),
                    file: source_file.map(|s| s.to_string()),
                    fn_name: None,
                    message: format!(
                        "Variant '{}' of type '{}' should use PascalCase",
                        variant, name
                    ),
                    extra_spans: vec![],
                });
            }
        }
    } else {
        for (field, fline) in fields_or_variants {
            if !is_camel_case(field) {
                out.push(CheckFinding {
                    line: fline,
                    module: module_name.clone(),
                    file: source_file.map(|s| s.to_string()),
                    fn_name: None,
                    message: format!(
                        "Record field '{}' of type '{}' should use camelCase",
                        field, name
                    ),
                    extra_spans: vec![],
                });
            }
        }
    }
}

fn is_camel_case(s: &str) -> bool {
    if s.is_empty() {
        return true;
    }
    // `_foo` is permitted (unused-binding convention); strip a single
    // leading underscore and check the rest.
    let core = s.strip_prefix('_').unwrap_or(s);
    if core.is_empty() {
        return true;
    }
    let core_first = core.chars().next().unwrap();
    core_first.is_ascii_lowercase() && !core.contains('_')
}

fn is_pascal_case(s: &str) -> bool {
    if s.is_empty() {
        return true;
    }
    let first = s.chars().next().unwrap();
    first.is_ascii_uppercase() && !s.contains('_')
}

fn to_camel_case(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut upper_next = false;
    let mut first = true;
    for ch in s.chars() {
        if ch == '_' {
            upper_next = !out.is_empty();
            continue;
        }
        if first {
            out.push(ch.to_ascii_lowercase());
            first = false;
        } else if upper_next {
            out.push(ch.to_ascii_uppercase());
            upper_next = false;
        } else {
            out.push(ch);
        }
    }
    out
}

fn to_pascal_case(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut upper_next = true;
    for ch in s.chars() {
        if ch == '_' {
            upper_next = true;
            continue;
        }
        if upper_next {
            out.push(ch.to_ascii_uppercase());
            upper_next = false;
        } else {
            out.push(ch);
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn camel_case_allows_underscore_prefix() {
        assert!(is_camel_case("foo"));
        assert!(is_camel_case("fooBar"));
        assert!(is_camel_case("_unused"));
        assert!(!is_camel_case("foo_bar"));
        assert!(!is_camel_case("FooBar"));
    }

    #[test]
    fn pascal_case_basic() {
        assert!(is_pascal_case("Foo"));
        assert!(is_pascal_case("FooBar"));
        assert!(!is_pascal_case("foo"));
        assert!(!is_pascal_case("Foo_Bar"));
    }

    #[test]
    fn camel_case_conversion() {
        assert_eq!(to_camel_case("snake_case"), "snakeCase");
        assert_eq!(to_camel_case("already_camelCase"), "alreadyCamelCase");
    }

    #[test]
    fn pascal_case_conversion() {
        assert_eq!(to_pascal_case("snake_case"), "SnakeCase");
        assert_eq!(to_pascal_case("foo"), "Foo");
    }
}

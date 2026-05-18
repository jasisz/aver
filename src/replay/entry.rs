//! Parsing and serialising of user-supplied entry-point expressions
//! for record mode. Shared by the `aver run --expr` CLI path and the
//! playground's custom-entry recording API.

use std::sync::Arc;

use crate::ast::{Expr, Literal, Spanned};
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::replay::{JsonValue, value_to_json};
use crate::value::Value;

/// Parse a CLI `--expr` / playground entry expression.
///
/// Accepts a single function call of the form `name(arg1, arg2, ...)` where
/// each argument is a literal (`String` / `Int` / `Float` / `Bool` / `Unit`).
/// Returns `(function_name, evaluated_args)`.
///
/// Complex argument expressions (arithmetic, record construction, nested
/// calls) are rejected because recordings store entry args in the
/// `input` JSON field which only round-trips values. Users needing richer
/// inputs wrap the call in a helper function and point the entry at that.
pub fn parse_entry_call(src: &str) -> Result<(String, Vec<Value>), String> {
    let mut lexer = Lexer::new(src);
    let tokens = lexer
        .tokenize()
        .map_err(|e| format!("lex error in entry expression: {}", e))?;
    let mut parser = Parser::new(tokens);
    let spanned = parser
        .parse_expr()
        .map_err(|e| format!("parse error in entry expression: {}", e))?;

    let (target, args) = match spanned.node {
        Expr::FnCall(target, args) => (target, args),
        _ => {
            return Err(
                "entry expression must be a function call like 'loadTaxRate(\"PL\")'".to_string(),
            );
        }
    };

    let fn_name = match &target.node {
        Expr::Ident(name) => name.clone(),
        _ => {
            return Err("entry expression target must be a bare function name \
                 (qualified paths not supported yet)"
                .to_string());
        }
    };

    let mut values = Vec::with_capacity(args.len());
    for (idx, arg) in args.iter().enumerate() {
        let val = expr_to_value(&arg.node).map_err(|e| format!("arg #{}: {}", idx + 1, e))?;
        values.push(val);
    }

    Ok((fn_name, values))
}

/// Convert a parsed expression to a runtime `Value` without running the VM.
/// Supports literals, tuples, lists, and ADT constructors (both built-in
/// Result/Option/None and user-defined variants). Rejects arbitrary
/// expressions (function calls, arithmetic, variables, records) so entry
/// arguments stay round-trippable through the replay JSON schema.
fn expr_to_value(expr: &Expr) -> Result<Value, String> {
    match expr {
        Expr::Literal(lit) => Ok(literal_to_value(lit)),
        // Unary minus on a numeric literal: support `-300.0` in --expr args.
        // Anything other than a literal operand is rejected — entry-arg
        // expressions must be round-trippable through the JSON schema.
        Expr::Neg(inner) => match &inner.node {
            Expr::Literal(Literal::Int(n)) => Ok(Value::Int(-*n)),
            Expr::Literal(Literal::Float(f)) => Ok(Value::Float(-*f)),
            _ => Err("unary '-' must be applied to a numeric literal in entry args".to_string()),
        },
        Expr::Ident(name) if is_upper_camel(name) => constructor_value(name, &[]),
        Expr::Attr(_, _) if dotted_upper_path(expr).is_some() => {
            let path = dotted_upper_path(expr).unwrap();
            constructor_value(&path, &[])
        }
        Expr::Constructor(name, arg) => {
            let fields = constructor_arg_fields(arg.as_deref())?;
            constructor_value(name, &fields)
        }
        Expr::FnCall(target, args) if dotted_upper_path(&target.node).is_some() => {
            let path = dotted_upper_path(&target.node).unwrap();
            let mut fields = Vec::with_capacity(args.len());
            for a in args {
                fields.push(expr_to_value(&a.node)?);
            }
            constructor_value(&path, &fields)
        }
        Expr::List(items) => {
            let mut out = Vec::with_capacity(items.len());
            for e in items {
                out.push(expr_to_value(&e.node)?);
            }
            Ok(Value::List(aver_rt::AverList::from_vec(out)))
        }
        Expr::Tuple(items) => {
            let mut out = Vec::with_capacity(items.len());
            for e in items {
                out.push(expr_to_value(&e.node)?);
            }
            Ok(Value::Tuple(out))
        }
        _ => Err(
            "unsupported expression shape (supported: literals, lists, tuples, \
             ADT constructors like Shape.Circle(1.0) / Result.Ok(x) / Option.None)"
                .to_string(),
        ),
    }
}

fn literal_to_value(lit: &Literal) -> Value {
    match lit {
        Literal::Int(i) => Value::Int(*i),
        Literal::Float(f) => Value::Float(*f),
        Literal::Str(s) => Value::Str(s.clone()),
        Literal::Bool(b) => Value::Bool(*b),
        Literal::Unit => Value::Unit,
    }
}

fn is_upper_camel(name: &str) -> bool {
    name.chars().next().is_some_and(|c| c.is_ascii_uppercase())
}

fn dotted_upper_path(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Ident(name) if is_upper_camel(name) => Some(name.clone()),
        Expr::Attr(inner, field) if is_upper_camel(field) => {
            let base = dotted_upper_path(&inner.node)?;
            Some(format!("{}.{}", base, field))
        }
        _ => None,
    }
}

fn constructor_arg_fields(arg: Option<&Spanned<Expr>>) -> Result<Vec<Value>, String> {
    match arg {
        None => Ok(Vec::new()),
        Some(inner) => match &inner.node {
            Expr::Tuple(items) => {
                let mut out = Vec::with_capacity(items.len());
                for e in items {
                    out.push(expr_to_value(&e.node)?);
                }
                Ok(out)
            }
            _ => Ok(vec![expr_to_value(&inner.node)?]),
        },
    }
}

fn constructor_value(path: &str, fields: &[Value]) -> Result<Value, String> {
    // Built-in wrapper constructors: accept both qualified (`Result.Ok`)
    // and bare (`Ok`) forms mirroring what the parser produces.
    match path {
        "Result.Ok" | "Ok" => {
            require_arity(path, fields, 1)?;
            Ok(Value::Ok(Box::new(fields[0].clone())))
        }
        "Result.Err" | "Err" => {
            require_arity(path, fields, 1)?;
            Ok(Value::Err(Box::new(fields[0].clone())))
        }
        "Option.Some" | "Some" => {
            require_arity(path, fields, 1)?;
            Ok(Value::Some(Box::new(fields[0].clone())))
        }
        "Option.None" | "None" => {
            require_arity(path, fields, 0)?;
            Ok(Value::None)
        }
        _ => {
            let mut parts = path.rsplitn(2, '.');
            let variant = parts.next().ok_or("empty constructor path")?.to_string();
            let type_name = parts
                .next()
                .ok_or_else(|| {
                    format!(
                        "constructor '{}' needs a type prefix (e.g. 'Shape.Circle')",
                        path
                    )
                })?
                .to_string();
            Ok(Value::Variant {
                type_name,
                variant,
                fields: Arc::<[Value]>::from(fields.to_vec()),
            })
        }
    }
}

fn require_arity(path: &str, fields: &[Value], expected: usize) -> Result<(), String> {
    if fields.len() != expected {
        return Err(format!(
            "constructor '{}' expects {} argument{}, got {}",
            path,
            expected,
            if expected == 1 { "" } else { "s" },
            fields.len()
        ));
    }
    Ok(())
}

/// Serialise entry-call arguments into the replay schema's `input` field.
///
/// Matches `decode_entry_args` on the replay side:
/// - empty arg list → `JsonValue::Null`
/// - single arg → the single value directly
/// - multiple args → a JSON array
pub fn encode_entry_args(args: &[Value]) -> Result<JsonValue, String> {
    match args.len() {
        0 => Ok(JsonValue::Null),
        1 => value_to_json(&args[0]),
        _ => {
            let jsons: Result<Vec<_>, _> = args.iter().map(value_to_json).collect();
            jsons.map(JsonValue::Array)
        }
    }
}

/// Derive a readable filename stem from an entry call.
/// Simple literal args produce a visible slug (`"fetchUser-alice"`);
/// complex cases fall back to a stable hash-based stem.
pub fn recording_stem(fn_name: &str, args: &[Value]) -> String {
    fn value_slug(v: &Value) -> Option<String> {
        match v {
            Value::Str(s) if is_slug_safe(s) && s.len() <= 32 => Some(s.clone()),
            Value::Int(i) => Some(i.to_string()),
            Value::Float(f) if f.is_finite() => Some(format!("{}", f).replace('.', "_")),
            Value::Bool(b) => Some(if *b { "true".into() } else { "false".into() }),
            _ => None,
        }
    }
    fn is_slug_safe(s: &str) -> bool {
        !s.is_empty()
            && s.chars()
                .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_')
    }

    let slugs: Option<Vec<String>> = args.iter().map(value_slug).collect();
    match slugs {
        Some(parts) if !parts.is_empty() => format!("{}-{}", fn_name, parts.join("-")),
        Some(_) => fn_name.to_string(),
        None => {
            use std::collections::hash_map::DefaultHasher;
            use std::hash::{Hash, Hasher};
            let mut hasher = DefaultHasher::new();
            fn_name.hash(&mut hasher);
            for v in args {
                format!("{:?}", v).hash(&mut hasher);
            }
            let h = hasher.finish();
            format!("{}-{:08x}", fn_name, (h & 0xffff_ffff) as u32)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(src: &str) -> (String, Vec<Value>) {
        parse_entry_call(src).expect("should parse")
    }

    fn parse_err(src: &str) -> String {
        parse_entry_call(src)
            .expect_err("should reject")
            .to_string()
    }

    #[test]
    fn literal_args() {
        let (name, args) = parse(r#"greet("Alice", 42, 3.14, true)"#);
        assert_eq!(name, "greet");
        assert_eq!(args.len(), 4);
        assert!(matches!(args[0], Value::Str(ref s) if s == "Alice"));
        assert!(matches!(args[1], Value::Int(42)));
        let expected = 314.0 / 100.0;
        assert!(matches!(args[2], Value::Float(f) if (f - expected).abs() < 1e-9));
        assert!(matches!(args[3], Value::Bool(true)));
    }

    #[test]
    fn negative_numeric_literals() {
        let (_, args) = parse("loadTempBounds(-300.0, -40)");
        assert!(matches!(args[0], Value::Float(f) if (f + 300.0).abs() < 1e-9));
        assert!(matches!(args[1], Value::Int(-40)));
    }

    #[test]
    fn negative_on_non_literal_is_rejected() {
        let msg = parse_err("foo(-Shape.Circle(1.0))");
        assert!(msg.contains("numeric literal"), "got: {}", msg);
    }

    #[test]
    fn user_variant_single_and_multi_field() {
        let (_, args) = parse("area(Shape.Circle(1.0))");
        let Value::Variant {
            type_name,
            variant,
            fields,
        } = &args[0]
        else {
            panic!("expected Variant, got {:?}", args[0]);
        };
        assert_eq!(type_name, "Shape");
        assert_eq!(variant, "Circle");
        assert_eq!(fields.len(), 1);
        assert!(matches!(fields[0], Value::Float(f) if (f - 1.0).abs() < 1e-9));

        let (_, args) = parse("area(Shape.Rectangle(3.0, 4.0))");
        let Value::Variant { fields, .. } = &args[0] else {
            panic!("expected Variant");
        };
        assert_eq!(fields.len(), 2);
    }

    #[test]
    fn builtin_wrapper_constructors() {
        let (_, args) = parse(r#"handle(Result.Ok(5))"#);
        assert!(matches!(&args[0], Value::Ok(inner) if matches!(**inner, Value::Int(5))));

        let (_, args) = parse(r#"handle(Result.Err("bad"))"#);
        assert!(
            matches!(&args[0], Value::Err(inner) if matches!(**inner, Value::Str(ref s) if s == "bad"))
        );

        let (_, args) = parse("handle(Option.Some(1))");
        assert!(matches!(&args[0], Value::Some(inner) if matches!(**inner, Value::Int(1))));

        let (_, args) = parse("handle(Option.None)");
        assert!(matches!(&args[0], Value::None));
    }

    #[test]
    fn nested_constructors() {
        let (_, args) = parse("handle(Result.Ok(Shape.Circle(2.0)))");
        let Value::Ok(inner) = &args[0] else {
            panic!("expected Ok");
        };
        let Value::Variant {
            type_name, variant, ..
        } = &**inner
        else {
            panic!("expected inner Variant");
        };
        assert_eq!(type_name, "Shape");
        assert_eq!(variant, "Circle");
    }

    #[test]
    fn list_and_tuple_args() {
        let (_, args) = parse("sumAll([1, 2, 3])");
        assert!(matches!(args[0], Value::List(_)));

        let (_, args) = parse(r#"describe((1, "x"))"#);
        assert!(matches!(args[0], Value::Tuple(ref items) if items.len() == 2));
    }

    #[test]
    fn arity_mismatch_on_builtin_wrapper() {
        let msg = parse_err("handle(Result.Ok(1, 2))");
        assert!(msg.contains("Result.Ok"), "got: {}", msg);
    }

    #[test]
    fn zero_arg_call_is_accepted() {
        let (name, args) = parse("tick()");
        assert_eq!(name, "tick");
        assert!(args.is_empty());
    }

    #[test]
    fn top_level_must_be_a_call() {
        let msg = parse_err("42");
        assert!(msg.contains("function call"), "got: {}", msg);
    }

    #[test]
    fn arithmetic_arg_rejected() {
        let msg = parse_err("foo(1 + 2)");
        assert!(msg.contains("arg #1"), "got: {}", msg);
    }

    #[test]
    fn function_call_arg_rejected() {
        // Lowercase `helper` = function ref, not constructor. Rejected.
        let msg = parse_err("foo(helper(5))");
        assert!(msg.contains("arg #1"), "got: {}", msg);
    }

    #[test]
    fn variable_arg_rejected() {
        let msg = parse_err("foo(x)");
        assert!(msg.contains("arg #1"), "got: {}", msg);
    }

    #[test]
    fn qualified_target_rejected() {
        let msg = parse_err("Math.abs(-5)");
        assert!(msg.contains("bare function name"), "got: {}", msg);
    }

    #[test]
    fn encode_entry_args_shape() {
        use crate::replay::JsonValue;

        match encode_entry_args(&[]).unwrap() {
            JsonValue::Null => {}
            other => panic!("expected Null for empty, got {:?}", other),
        }

        let single = encode_entry_args(&[Value::Int(5)]).unwrap();
        assert!(matches!(single, JsonValue::Int(5)), "got: {:?}", single);

        let multi = encode_entry_args(&[Value::Int(1), Value::Str("x".into())]).unwrap();
        assert!(
            matches!(&multi, JsonValue::Array(v) if v.len() == 2),
            "got: {:?}",
            multi
        );
    }

    #[test]
    fn recording_stem_literal_args() {
        assert_eq!(
            recording_stem("loadPort", &[Value::Str("PL".into())]),
            "loadPort-PL"
        );
        assert_eq!(recording_stem("fib", &[Value::Int(10)]), "fib-10");
        assert_eq!(recording_stem("flag", &[Value::Bool(false)]), "flag-false");
    }

    #[test]
    fn recording_stem_complex_args_fall_back_to_hash() {
        let stem = recording_stem(
            "area",
            &[Value::Variant {
                type_name: "Shape".into(),
                variant: "Circle".into(),
                fields: Arc::<[Value]>::from(vec![Value::Float(1.0)]),
            }],
        );
        assert!(stem.starts_with("area-"), "got: {}", stem);
        assert_eq!(stem.len(), "area-".len() + 8, "expected 8-hex suffix");
    }
}

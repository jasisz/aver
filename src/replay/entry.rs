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

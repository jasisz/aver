//! Parsing and serialising of user-supplied entry-point expressions
//! for record mode. Shared by the `aver run --expr` CLI path and the
//! playground's custom-entry recording API.

use crate::ast::{Expr, Literal};
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
    for (idx, arg) in args.into_iter().enumerate() {
        let Expr::Literal(lit) = &arg.node else {
            return Err(format!(
                "entry expression arg #{} must be a literal (String, Int, Float, Bool, Unit). \
                 Complex arguments (records, lists, function calls) not yet supported; \
                 wrap them in a helper function and call that instead.",
                idx + 1
            ));
        };
        let val = match lit {
            Literal::Int(i) => Value::Int(*i),
            Literal::Float(f) => Value::Float(*f),
            Literal::Str(s) => Value::Str(s.clone()),
            Literal::Bool(b) => Value::Bool(*b),
            Literal::Unit => Value::Unit,
        };
        values.push(val);
    }

    Ok((fn_name, values))
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

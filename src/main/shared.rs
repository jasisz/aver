use std::fs;
use std::io::Read;

use colored::Colorize;

use aver::ast::TopLevel;
use aver::source::parse_source;
use aver::types::checker::TypeError;
use aver::value::Value;
use aver::vm;

pub(super) fn read_file(path: &str) -> Result<String, String> {
    fs::read_to_string(path).map_err(|e| format!("Cannot open file '{}': {}", path, e))
}

pub(super) fn parse_file(source: &str) -> Result<Vec<TopLevel>, String> {
    parse_source(source)
}

pub(super) fn resolve_module_root(module_root: Option<&str>) -> String {
    if let Some(root) = module_root {
        return root.to_string();
    }
    std::env::current_dir()
        .ok()
        .and_then(|p| p.into_os_string().into_string().ok())
        .unwrap_or_else(|| ".".to_string())
}

pub(super) fn load_runtime_policy(
    module_root: &str,
) -> Result<Option<aver::config::ProjectConfig>, String> {
    aver::config::ProjectConfig::load_from_dir(std::path::Path::new(module_root))
        .map_err(|e| format!("aver.toml: {}", e))
}

pub(super) fn apply_runtime_policy_to_vm(
    machine: &mut vm::VM,
    module_root: &str,
) -> Result<(), String> {
    if let Some(config) = load_runtime_policy(module_root)? {
        machine.set_runtime_policy(config);
    }
    Ok(())
}

pub(super) fn print_type_errors(errors: &[TypeError]) {
    for te in errors {
        eprintln!(
            "{}",
            format!("error[{}:{}]: {}", te.line, te.col, te.message).red()
        );
    }
}

// `compute_memo_fns` and `is_memo_safe_type` moved to
// `aver::diagnostics::context` so the playground/LSP share one
// implementation. Re-exported for the rest of `src/main/` that still
// imports them from `super::shared`.
pub(super) use aver::diagnostics::context::compute_memo_fns;

pub(super) fn format_type_errors(errors: &[TypeError]) -> String {
    let mut out = Vec::new();
    for te in errors {
        out.push(format!("error[{}:{}]: {}", te.line, te.col, te.message));
    }
    out.join("\n")
}

/// Collect entry expressions from `--expr` flags or `--input-file`.
/// Input file path `-` reads from stdin.
/// Returns empty list if neither is provided (caller runs `main`).
pub(super) fn collect_entry_expressions(
    exprs: &[String],
    input_file: Option<&str>,
) -> Result<Vec<String>, String> {
    if let Some(path) = input_file {
        let content = if path == "-" {
            let mut buf = String::new();
            std::io::stdin()
                .read_to_string(&mut buf)
                .map_err(|e| format!("Cannot read stdin: {}", e))?;
            buf
        } else {
            fs::read_to_string(path)
                .map_err(|e| format!("Cannot read input file '{}': {}", path, e))?
        };
        let trimmed = content.trim().to_string();
        if trimmed.is_empty() {
            return Err(format!("Input file '{}' is empty", path));
        }
        return Ok(vec![trimmed]);
    }
    Ok(exprs.to_vec())
}

/// Parse a CLI `--expr` argument. Requires a simple function call of the form
/// `name(arg1, arg2, ...)` where each arg is a literal (String / Int / Float / Bool / Unit).
/// Returns `(function_name, evaluated_args)`.
///
/// This intentionally rejects arbitrary expressions (arithmetic, record
/// construction, conditionals) because 0.10.1 stores entry args in the
/// replay schema's `input` JSON field, which only round-trips values. Rich
/// argument expressions would require re-compiling the caller at replay time.
pub(super) fn parse_call_expression(src: &str) -> Result<(String, Vec<Value>), String> {
    use aver::ast::{Expr, Literal};
    use aver::lexer::Lexer;
    use aver::parser::Parser;

    let mut lexer = Lexer::new(src);
    let tokens = lexer
        .tokenize()
        .map_err(|e| format!("lex error in --expr: {}", e))?;
    let mut parser = Parser::new(tokens);
    let spanned = parser
        .parse_expr()
        .map_err(|e| format!("parse error in --expr: {}", e))?;

    let (target, args) = match spanned.node {
        Expr::FnCall(target, args) => (target, args),
        _ => {
            return Err("--expr must be a function call like 'loadTaxRate(\"PL\")'".to_string());
        }
    };

    let fn_name = match &target.node {
        Expr::Ident(name) => name.clone(),
        _ => {
            return Err(
                "--expr target must be a bare function name (qualified paths not supported yet)"
                    .to_string(),
            );
        }
    };

    let mut values = Vec::with_capacity(args.len());
    for (idx, arg) in args.into_iter().enumerate() {
        let Expr::Literal(lit) = &arg.node else {
            return Err(format!(
                "--expr arg #{} must be a literal (String, Int, Float, Bool, Unit). \
                 Complex arguments (records, lists, function calls) not yet supported \
                 on the CLI; wrap them in a helper function and call that instead.",
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
/// Matches `decode_entry_args` on the replay side: `Value::Unit` / empty → null,
/// a single arg round-trips as a single JSON value, multiple args as a JSON list.
pub(super) fn encode_entry_args_json(args: &[Value]) -> Result<aver::replay::JsonValue, String> {
    use aver::replay::{JsonValue, value_to_json};
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
/// Simple literal args get a visible slug; complex cases fall back to a hash.
pub(super) fn entry_recording_stem(fn_name: &str, args: &[Value]) -> String {
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

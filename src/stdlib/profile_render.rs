//! Render one parsed capability function back to Aver source.
//!
//! Hostile profiles are ordinary functions inside a capability's `.av`
//! contract, and the verify pipeline injects them into the user's module
//! under a synthetic name. Taking that text out of the contract used to mean
//! searching the file for `fn <name>(` and cutting at the next line starting
//! `fn ` — a boundary the source's shape happens to have today, not one the
//! language promises. A comment between two profiles, a blank-line-free file,
//! or a name that is a prefix of another moves the cut.
//!
//! The renderer takes the profile the parser already produced and prints it
//! under whatever name the injector wants. It is deliberately total over the
//! `Expr` shapes a capability contract may contain and refuses anything else
//! by name, so a profile written in a shape this cannot print fails loudly
//! rather than reaching the user as truncated source.

use crate::ast::{BinOp, Expr, FnBody, FnDef, Literal, MatchArm, Pattern, Spanned, Stmt, StrPart};

const INDENT: usize = 4;

/// Render `function` as a top-level Aver function named `name`.
///
/// The output ends with a single newline and parses on its own.
pub(crate) fn render_fn_def(function: &FnDef, name: &str) -> Result<String, String> {
    let mut out = String::new();
    out.push_str("fn ");
    out.push_str(name);
    out.push('(');
    for (index, (param, type_name)) in function.params.iter().enumerate() {
        if index > 0 {
            out.push_str(", ");
        }
        out.push_str(param);
        out.push_str(": ");
        out.push_str(type_name);
    }
    out.push(')');
    if !function.return_type.is_empty() {
        out.push_str(" -> ");
        out.push_str(&function.return_type);
    }
    out.push('\n');

    if let Some(desc) = &function.desc {
        out.push_str(&pad(INDENT));
        out.push_str("? ");
        out.push_str(&string_literal(desc));
        out.push('\n');
    }
    if !function.effects.is_empty() {
        out.push_str(&pad(INDENT));
        out.push_str("! [");
        for (index, effect) in function.effects.iter().enumerate() {
            if index > 0 {
                out.push_str(", ");
            }
            out.push_str(&effect.node);
        }
        out.push_str("]\n");
    }

    let FnBody::Block(statements) = function.body.as_ref();
    if statements.is_empty() {
        return Err(format!("capability function `{name}` has an empty body"));
    }
    for statement in statements {
        match statement {
            Stmt::Binding(binding, type_name, value) => {
                out.push_str(&pad(INDENT));
                out.push_str(binding);
                if let Some(type_name) = type_name {
                    out.push_str(": ");
                    out.push_str(type_name);
                }
                out.push_str(" = ");
                render_expr(value, INDENT, &mut out)?;
                out.push('\n');
            }
            Stmt::Expr(value) => {
                out.push_str(&pad(INDENT));
                render_expr(value, INDENT, &mut out)?;
                out.push('\n');
            }
        }
    }
    Ok(out)
}

fn pad(width: usize) -> String {
    " ".repeat(width)
}

/// Append `expr` starting at the current end of `out`, which already sits at
/// column `indent`. Only `match` spans lines; its arms are written at
/// `indent + INDENT` and an arm body that is itself a `match` recurses.
fn render_expr(expr: &Spanned<Expr>, indent: usize, out: &mut String) -> Result<(), String> {
    match &expr.node {
        Expr::Match { subject, arms } => {
            out.push_str("match ");
            out.push_str(&inline(subject)?);
            for arm in arms {
                out.push('\n');
                out.push_str(&pad(indent + INDENT));
                render_arm(arm, indent + INDENT, out)?;
            }
            Ok(())
        }
        _ => {
            out.push_str(&inline(expr)?);
            Ok(())
        }
    }
}

fn render_arm(arm: &MatchArm, indent: usize, out: &mut String) -> Result<(), String> {
    out.push_str(&pattern(&arm.pattern)?);
    out.push_str(" -> ");
    render_expr(&arm.body, indent, out)
}

fn pattern(pattern: &Pattern) -> Result<String, String> {
    Ok(match pattern {
        Pattern::Wildcard => "_".to_string(),
        Pattern::Literal(literal) => self::literal(literal),
        Pattern::Ident(name) => name.clone(),
        Pattern::EmptyList => "[]".to_string(),
        Pattern::Cons(head, tail) => format!("[{head}, ..{tail}]"),
        Pattern::Tuple(elements) => {
            let rendered = elements
                .iter()
                .map(self::pattern)
                .collect::<Result<Vec<_>, _>>()?;
            format!("({})", rendered.join(", "))
        }
        Pattern::Constructor(name, bindings) if bindings.is_empty() => name.clone(),
        Pattern::Constructor(name, bindings) => format!("{name}({})", bindings.join(", ")),
    })
}

fn literal(literal: &Literal) -> String {
    match literal {
        Literal::Int(value) => value.to_string(),
        Literal::BigInt(digits) => digits.clone(),
        Literal::Float(value) => {
            // A float that round-trips through `to_string` without a fraction
            // needs its `.0` back to stay a float in source.
            let rendered = value.to_string();
            if rendered.contains(['.', 'e', 'E', 'N', 'i']) {
                rendered
            } else {
                format!("{rendered}.0")
            }
        }
        Literal::Str(text) => string_literal(text),
        Literal::Bool(value) => value.to_string(),
        Literal::Unit => "Unit".to_string(),
    }
}

fn string_literal(text: &str) -> String {
    let mut out = String::with_capacity(text.len() + 2);
    out.push('"');
    out.push_str(&escape(text));
    out.push('"');
    out
}

/// Undo exactly what the lexer's escape table does, so a rendered literal
/// lexes back to the same string.
fn escape(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    for ch in text.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            '\r' => out.push_str("\\r"),
            '\u{0008}' => out.push_str("\\b"),
            '\u{000C}' => out.push_str("\\f"),
            other => out.push(other),
        }
    }
    out
}

fn binary_operator(operator: BinOp) -> &'static str {
    match operator {
        BinOp::Add => "+",
        BinOp::Sub => "-",
        BinOp::Mul => "*",
        BinOp::Div => "/",
        BinOp::Eq => "==",
        BinOp::Neq => "!=",
        BinOp::Lt => "<",
        BinOp::Gt => ">",
        BinOp::Lte => "<=",
        BinOp::Gte => ">=",
    }
}

/// Render an expression that fits on one line.
fn inline(expr: &Spanned<Expr>) -> Result<String, String> {
    Ok(match &expr.node {
        Expr::Literal(value) => literal(value),
        Expr::Ident(name) => name.clone(),
        Expr::Attr(object, field) => format!("{}.{field}", inline(object)?),
        Expr::FnCall(callee, args) => format!("{}({})", inline(callee)?, inline_all(args)?),
        Expr::BinOp(operator, left, right) => format!(
            "{} {} {}",
            inline(left)?,
            binary_operator(*operator),
            inline(right)?
        ),
        Expr::Neg(operand) => format!("-{}", inline(operand)?),
        Expr::Constructor(name, None) => name.clone(),
        Expr::Constructor(name, Some(argument)) => format!("{name}({})", inline(argument)?),
        Expr::ErrorProp(inner) => format!("{}?", inline(inner)?),
        Expr::InterpolatedStr(parts) => {
            let mut out = String::from("\"");
            for part in parts {
                match part {
                    StrPart::Literal(text) => out.push_str(&escape(text)),
                    StrPart::Parsed(inner_expr) => {
                        out.push('{');
                        out.push_str(&inline(inner_expr)?);
                        out.push('}');
                    }
                }
            }
            out.push('"');
            out
        }
        Expr::List(elements) => format!("[{}]", inline_all(elements)?),
        Expr::Tuple(elements) => format!("({})", inline_all(elements)?),
        Expr::IndependentProduct(elements, unwrap) => format!(
            "({}){}",
            inline_all(elements)?,
            if *unwrap { "?!" } else { "!" }
        ),
        Expr::MapLiteral(entries) => {
            let rendered = entries
                .iter()
                .map(|(key, value)| Ok(format!("{} => {}", inline(key)?, inline(value)?)))
                .collect::<Result<Vec<String>, String>>()?;
            format!("{{{}}}", rendered.join(", "))
        }
        Expr::RecordCreate { type_name, fields } => {
            format!("{type_name}({})", inline_fields(fields)?)
        }
        Expr::RecordUpdate {
            type_name,
            base,
            updates,
        } => format!(
            "{type_name}.update({}, {})",
            inline(base)?,
            inline_fields(updates)?
        ),
        // A capability contract is parsed and rendered before any transform
        // pass runs, so these three cannot appear. Naming them keeps the
        // match exhaustive and the refusal specific.
        Expr::Match { .. } => {
            return Err("a `match` inside a one-line expression cannot be rendered".to_string());
        }
        Expr::TailCall(_) => {
            return Err("a tail call has no capability-contract source form".to_string());
        }
        Expr::Resolved { .. } => {
            return Err("a resolved slot has no capability-contract source form".to_string());
        }
    })
}

fn inline_all(items: &[Spanned<Expr>]) -> Result<String, String> {
    Ok(items
        .iter()
        .map(inline)
        .collect::<Result<Vec<_>, _>>()?
        .join(", "))
}

fn inline_fields(fields: &[(String, Spanned<Expr>)]) -> Result<String, String> {
    Ok(fields
        .iter()
        .map(|(name, value)| Ok(format!("{name} = {}", inline(value)?)))
        .collect::<Result<Vec<String>, String>>()?
        .join(", "))
}

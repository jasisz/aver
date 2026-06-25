//! AST → parseable Aver source. See crate-level docs for the
//! guarantees and non-guarantees.
//!
//! Every AST variant the parser produces has a branch here. The
//! pre-fuzz "this should be unreachable for a mutator" variants
//! (`Expr::TailCall` post-TCO, `Expr::Resolved` post-resolver)
//! return `UnparseError::Unsupported` with a tag identifying the
//! variant — the caller maps that to a "skip this mutation"
//! signal on the AFL side.

use std::fmt::Write;

use aver::ast::{
    BinOp, DecisionBlock, DecisionImpact, Expr, FnDef, Literal, Module, Pattern, Spanned, Stmt,
    StrPart, TopLevel, TypeDef, VerifyBlock, VerifyGiven, VerifyGivenDomain, VerifyKind,
};

/// Errors the unparser can surface. Caller (Phase 1 mutator) maps
/// these to "skip this mutation, return 0 to AFL".
#[derive(Debug, Clone, PartialEq)]
pub enum UnparseError {
    /// AST variant the unparser explicitly refuses — typically a
    /// node only produced by a post-parse pass (TCO, resolver)
    /// that the mutator should never see. Includes a static tag
    /// describing the variant so the integration test failure
    /// names the exact gap.
    Unsupported(&'static str),
    /// `std::fmt::Write` failed against the internal String
    /// buffer. Effectively unreachable (String never fails on
    /// `Write`), but `?` propagation needs a variant.
    FmtError,
}

impl std::fmt::Display for UnparseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnparseError::Unsupported(tag) => write!(f, "unsupported AST variant: {tag}"),
            UnparseError::FmtError => write!(f, "internal fmt error"),
        }
    }
}

impl std::error::Error for UnparseError {}

impl From<std::fmt::Error> for UnparseError {
    fn from(_: std::fmt::Error) -> Self {
        UnparseError::FmtError
    }
}

type Result<T> = std::result::Result<T, UnparseError>;

const INDENT: &str = "    ";

/// Entry point. Emits parseable Aver source covering every
/// `TopLevel` in `items`. The output bytes are not normalised
/// against the production formatter — they're whatever the
/// simplest layout that re-parses cleanly is.
pub fn unparse(items: &[TopLevel]) -> Result<String> {
    let mut out = String::with_capacity(items.len() * 64);
    let mut first = true;
    for item in items {
        if !first {
            out.push('\n');
        }
        first = false;
        write_top_level(&mut out, item)?;
        if !out.ends_with('\n') {
            out.push('\n');
        }
    }
    if out.is_empty() {
        // Empty program is legal Aver; emit a trailing newline so
        // the parser's "must end with newline" rule holds.
        out.push('\n');
    }
    Ok(out)
}

fn write_top_level(out: &mut String, item: &TopLevel) -> Result<()> {
    match item {
        TopLevel::Module(m) => write_module(out, m),
        TopLevel::TypeDef(td) => write_typedef(out, td),
        TopLevel::FnDef(fd) => write_fndef(out, fd),
        TopLevel::Verify(vb) => write_verify(out, vb),
        TopLevel::Decision(db) => write_decision(out, db),
        TopLevel::Stmt(stmt) => write_stmt(out, stmt, 0),
    }
}

fn write_module(out: &mut String, m: &Module) -> Result<()> {
    writeln!(out, "module {}", m.name)?;
    if !m.intent.is_empty() {
        // Multi-line intents get rejoined into a single quoted
        // string per line. The parser accepts both shapes.
        writeln!(out, "{INDENT}intent =")?;
        for line in m.intent.split('\n') {
            let trimmed = line.trim();
            if trimmed.is_empty() {
                continue;
            }
            writeln!(out, "{INDENT}{INDENT}{}", quote_string(trimmed))?;
        }
    }
    if !m.depends.is_empty() {
        writeln!(out, "{INDENT}depends [{}]", m.depends.join(", "))?;
    }
    if !m.exposes.is_empty() || !m.exposes_opaque.is_empty() {
        // The parser accepts a single `exposes [...]` list; emit
        // the non-opaque names first, opaque-tagged names suffixed
        // with the `opaque` keyword the parser recognises.
        let names: Vec<String> = m
            .exposes
            .iter()
            .cloned()
            .chain(m.exposes_opaque.iter().map(|n| format!("opaque {n}")))
            .collect();
        writeln!(out, "{INDENT}exposes [{}]", names.join(", "))?;
    }
    if let Some(effects) = &m.effects {
        writeln!(out, "{INDENT}effects [{}]", effects.join(", "))?;
    }
    Ok(())
}

fn write_typedef(out: &mut String, td: &TypeDef) -> Result<()> {
    match td {
        TypeDef::Sum { name, variants, .. } => {
            writeln!(out, "type {name}")?;
            for v in variants {
                if v.fields.is_empty() {
                    writeln!(out, "{INDENT}{}", v.name)?;
                } else {
                    writeln!(out, "{INDENT}{}({})", v.name, v.fields.join(", "))?;
                }
            }
        }
        TypeDef::Product { name, fields, .. } => {
            writeln!(out, "record {name}")?;
            for (fname, fty) in fields {
                writeln!(out, "{INDENT}{fname}: {fty}")?;
            }
        }
    }
    Ok(())
}

fn write_fndef(out: &mut String, fd: &FnDef) -> Result<()> {
    let params: Vec<String> = fd.params.iter().map(|(n, t)| format!("{n}: {t}")).collect();
    // Aver convention: effects + doc live in the body block, not
    // the header. The parser's `! [effects]` lexer rule binds
    // tightly to the start of a statement; chaining it onto the
    // signature line confuses it ("expected expression, found
    // '!'"). Mirror what `examples/core/*.av` and the corpus
    // seeds do.
    writeln!(
        out,
        "fn {}({}) -> {}",
        fd.name,
        params.join(", "),
        fd.return_type
    )?;
    if let Some(desc) = &fd.desc {
        writeln!(out, "{INDENT}? {}", quote_string(desc))?;
    }
    if !fd.effects.is_empty() {
        let names: Vec<String> = fd.effects.iter().map(|e| e.node.clone()).collect();
        writeln!(out, "{INDENT}! [{}]", names.join(", "))?;
    }
    for stmt in fd.body.stmts() {
        write_stmt(out, stmt, 1)?;
    }
    Ok(())
}

fn write_stmt(out: &mut String, stmt: &Stmt, indent: usize) -> Result<()> {
    let pad = INDENT.repeat(indent);
    match stmt {
        Stmt::Binding(name, ty, expr) => {
            let ty_part = ty.as_deref().map(|t| format!(": {t}")).unwrap_or_default();
            write!(out, "{pad}{name}{ty_part} = ")?;
            write_expr(out, expr, indent)?;
            writeln!(out)?;
        }
        Stmt::Expr(expr) => {
            write!(out, "{pad}")?;
            write_expr(out, expr, indent)?;
            writeln!(out)?;
        }
    }
    Ok(())
}

fn write_verify(out: &mut String, vb: &VerifyBlock) -> Result<()> {
    match &vb.kind {
        VerifyKind::Cases => {
            let header = if vb.trace {
                "verify {} trace"
            } else {
                "verify {}"
            };
            let header = header.replace("{}", &vb.fn_name);
            writeln!(out, "{header}")?;
            for given in &vb.cases_givens {
                write_given(out, given, 1)?;
            }
            for (lhs, rhs) in &vb.cases {
                write!(out, "{INDENT}")?;
                write_expr(out, lhs, 1)?;
                write!(out, " => ")?;
                write_expr(out, rhs, 1)?;
                writeln!(out)?;
            }
        }
        VerifyKind::Law(law) => {
            let header = if vb.trace {
                format!("verify {} law {} trace", vb.fn_name, law.name)
            } else {
                format!("verify {} law {}", vb.fn_name, law.name)
            };
            writeln!(out, "{header}")?;
            for given in &law.givens {
                write_given(out, given, 1)?;
            }
            if let Some(when_expr) = &law.when {
                write!(out, "{INDENT}when ")?;
                write_expr(out, when_expr, 1)?;
                writeln!(out)?;
            }
            write!(out, "{INDENT}")?;
            write_expr(out, &law.lhs, 1)?;
            write!(out, " => ")?;
            write_expr(out, &law.rhs, 1)?;
            writeln!(out)?;
        }
    }
    Ok(())
}

fn write_given(out: &mut String, given: &VerifyGiven, indent: usize) -> Result<()> {
    let pad = INDENT.repeat(indent);
    match &given.domain {
        VerifyGivenDomain::IntRange { start, end } => {
            writeln!(
                out,
                "{pad}given {}: {} = {}..{}",
                given.name, given.type_name, start, end
            )?;
        }
        VerifyGivenDomain::Explicit(values) => {
            write!(out, "{pad}given {}: {} = [", given.name, given.type_name)?;
            for (i, v) in values.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ")?;
                }
                write_expr(out, v, indent)?;
            }
            writeln!(out, "]")?;
        }
    }
    Ok(())
}

fn write_decision(out: &mut String, db: &DecisionBlock) -> Result<()> {
    writeln!(out, "decision {}", db.name)?;
    if !db.date.is_empty() {
        writeln!(out, "{INDENT}date = {}", quote_string(&db.date))?;
    }
    if !db.reason.is_empty() {
        // Parser always wants the multi-line shape for `reason`:
        //   reason =
        //       "first line"
        //       "second line"
        // even when the AST holds a single-line string. Inline
        // `reason = "..."` is rejected with "Expected decision
        // field name". Emit a single quoted line under the
        // `reason =` header — round-trips because the parser
        // re-joins the multi-string sequence with ` `.
        writeln!(out, "{INDENT}reason =")?;
        for line in db.reason.split('\n') {
            let trimmed = line.trim();
            if trimmed.is_empty() {
                continue;
            }
            writeln!(out, "{INDENT}{INDENT}{}", quote_string(trimmed))?;
        }
    }
    let chosen_text = decision_impact_text(&db.chosen.node);
    writeln!(out, "{INDENT}chosen = {}", quote_string(chosen_text))?;
    if !db.rejected.is_empty() {
        let parts: Vec<String> = db
            .rejected
            .iter()
            .map(|r| quote_string(decision_impact_text(&r.node)))
            .collect();
        writeln!(out, "{INDENT}rejected = [{}]", parts.join(", "))?;
    }
    if !db.impacts.is_empty() {
        // Impacts can be bare symbols (function names) or
        // semantic strings. Emit symbols as bare identifiers, strings
        // quoted — matches what the parser accepts.
        let parts: Vec<String> = db
            .impacts
            .iter()
            .map(|i| match &i.node {
                DecisionImpact::Symbol(s) => s.clone(),
                DecisionImpact::Semantic(s) => quote_string(s),
            })
            .collect();
        writeln!(out, "{INDENT}impacts = [{}]", parts.join(", "))?;
    }
    if let Some(author) = &db.author {
        writeln!(out, "{INDENT}author = {}", quote_string(author))?;
    }
    Ok(())
}

fn decision_impact_text(impact: &DecisionImpact) -> &str {
    match impact {
        DecisionImpact::Symbol(s) | DecisionImpact::Semantic(s) => s,
    }
}

// ---------------------------------------------------------------------------
// Expressions
// ---------------------------------------------------------------------------

fn write_expr(out: &mut String, expr: &Spanned<Expr>, indent: usize) -> Result<()> {
    match &expr.node {
        Expr::Literal(lit) => write_literal(out, lit),
        Expr::Ident(name) => {
            out.push_str(name);
            Ok(())
        }
        Expr::Attr(inner, field) => {
            // Parens around the receiver for the same reason as
            // every other compound: the unparser is precedence-
            // blind by design.
            out.push('(');
            write_expr(out, inner, indent)?;
            out.push(')');
            out.push('.');
            out.push_str(field);
            Ok(())
        }
        Expr::FnCall(callee, args) => {
            write_expr(out, callee, indent)?;
            out.push('(');
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                write_expr(out, arg, indent)?;
            }
            out.push(')');
            Ok(())
        }
        Expr::BinOp(op, lhs, rhs) => {
            out.push('(');
            write_expr(out, lhs, indent)?;
            write!(out, " {} ", binop_str(*op))?;
            write_expr(out, rhs, indent)?;
            out.push(')');
            Ok(())
        }
        Expr::Neg(inner) => {
            out.push_str("(-");
            write_expr(out, inner, indent)?;
            out.push(')');
            Ok(())
        }
        Expr::Match { subject, arms } => {
            out.push_str("match ");
            write_expr(out, subject, indent)?;
            out.push('\n');
            let pad = INDENT.repeat(indent + 1);
            for arm in arms {
                out.push_str(&pad);
                write_pattern(out, &arm.pattern)?;
                out.push_str(" -> ");
                write_expr(out, &arm.body, indent + 1)?;
                out.push('\n');
            }
            // No trailing newline — caller's `writeln!` adds one.
            // Strip the last one we just pushed so the bare-match
            // expression doesn't end with two newlines when nested.
            if out.ends_with('\n') {
                out.pop();
            }
            Ok(())
        }
        Expr::Constructor(name, arg) => {
            out.push_str(name);
            if let Some(arg) = arg {
                out.push('(');
                write_expr(out, arg, indent)?;
                out.push(')');
            }
            Ok(())
        }
        Expr::ErrorProp(inner) => {
            out.push('(');
            write_expr(out, inner, indent)?;
            out.push_str(")?");
            Ok(())
        }
        Expr::InterpolatedStr(parts) => {
            out.push('"');
            for part in parts {
                match part {
                    StrPart::Literal(s) => out.push_str(&escape_string_inner(s)),
                    StrPart::Parsed(e) => {
                        out.push('{');
                        write_expr(out, e, indent)?;
                        out.push('}');
                    }
                }
            }
            out.push('"');
            Ok(())
        }
        Expr::List(items) => {
            out.push('[');
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                write_expr(out, item, indent)?;
            }
            out.push(']');
            Ok(())
        }
        Expr::Tuple(items) => {
            out.push('(');
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                write_expr(out, item, indent)?;
            }
            out.push(')');
            Ok(())
        }
        Expr::MapLiteral(entries) => {
            out.push('{');
            for (i, (k, v)) in entries.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                write_expr(out, k, indent)?;
                out.push_str(" => ");
                write_expr(out, v, indent)?;
            }
            out.push('}');
            Ok(())
        }
        Expr::RecordCreate { type_name, fields } => {
            out.push_str(type_name);
            out.push('(');
            for (i, (fname, val)) in fields.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                write!(out, "{fname} = ")?;
                write_expr(out, val, indent)?;
            }
            out.push(')');
            Ok(())
        }
        Expr::RecordUpdate {
            type_name,
            base,
            updates,
        } => {
            write!(out, "{type_name}.update(")?;
            write_expr(out, base, indent)?;
            for (fname, val) in updates {
                write!(out, ", {fname} = ")?;
                write_expr(out, val, indent)?;
            }
            out.push(')');
            Ok(())
        }
        Expr::IndependentProduct(items, unwrap) => {
            out.push('(');
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                write_expr(out, item, indent)?;
            }
            out.push(')');
            out.push_str(if *unwrap { "?!" } else { "!" });
            Ok(())
        }
        Expr::TailCall(_) => Err(UnparseError::Unsupported("Expr::TailCall")),
        Expr::Resolved { .. } => Err(UnparseError::Unsupported("Expr::Resolved")),
    }
}

fn binop_str(op: BinOp) -> &'static str {
    match op {
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

fn write_literal(out: &mut String, lit: &Literal) -> Result<()> {
    match lit {
        Literal::Int(i) => write!(out, "{i}")?,
        // A `>i64` literal — emit the validated decimal magnitude verbatim
        // (sign, if any, is the surrounding negation).
        Literal::BigInt(s) => write!(out, "{s}")?,
        Literal::Float(f) => {
            // Emit at full precision and force a decimal point so
            // the result is unambiguously parsed as Float (the
            // parser routes plain `42` to Int even when context
            // expects Float).
            let s = format!("{f}");
            if s.contains('.') || s.contains('e') || s.contains('E') {
                write!(out, "{s}")?;
            } else {
                write!(out, "{s}.0")?;
            }
        }
        Literal::Str(s) => {
            out.push('"');
            out.push_str(&escape_string_inner(s));
            out.push('"');
        }
        Literal::Bool(b) => out.push_str(if *b { "true" } else { "false" }),
        Literal::Unit => out.push_str("Unit"),
    }
    Ok(())
}

fn write_pattern(out: &mut String, pattern: &Pattern) -> Result<()> {
    match pattern {
        Pattern::Wildcard => {
            out.push('_');
            Ok(())
        }
        Pattern::Literal(lit) => write_literal(out, lit),
        Pattern::Ident(name) => {
            out.push_str(name);
            Ok(())
        }
        Pattern::EmptyList => {
            out.push_str("[]");
            Ok(())
        }
        Pattern::Cons(head, tail) => {
            write!(out, "[{head}, ..{tail}]")?;
            Ok(())
        }
        Pattern::Tuple(items) => {
            out.push('(');
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                write_pattern(out, item)?;
            }
            out.push(')');
            Ok(())
        }
        Pattern::Constructor(name, bindings) => {
            out.push_str(name);
            if !bindings.is_empty() {
                out.push('(');
                out.push_str(&bindings.join(", "));
                out.push(')');
            }
            Ok(())
        }
    }
}

// ---------------------------------------------------------------------------
// String escaping
// ---------------------------------------------------------------------------

/// Escape every character that the Aver lexer treats specially
/// inside a `"..."` string. Conservative: when in doubt, emit the
/// `\xNN` form rather than risk a parse failure.
fn escape_string_inner(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            // `{` is an interpolation marker inside `"..."`.
            // Escaping it preserves the literal char without
            // dragging the lexer into interpolation mode.
            '{' => out.push_str("\\{"),
            '}' => out.push_str("\\}"),
            c if (c as u32) < 0x20 => {
                // Control character — escape as \xNN. Aver's
                // lexer accepts \xNN under the standard rule.
                write!(out, "\\x{:02x}", c as u32).ok();
            }
            c => out.push(c),
        }
    }
    out
}

/// Wrap an already-clean string in quotes. Used for top-level
/// metadata (intent, decision fields) where the source had a
/// quoted literal and we want one back.
fn quote_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    out.push_str(&escape_string_inner(s));
    out.push('"');
    out
}

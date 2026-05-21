/// Aver expressions → Lean 4 expression strings.
use super::builtins;
use super::pattern::emit_pattern;
use super::shared::to_lower_first;
use crate::ast::*;
use crate::codegen::CodegenContext;
use crate::codegen::common::{expr_to_dotted_name, is_user_type, resolve_module_call};

/// Emit a Lean 4 expression from an Aver Expr.
pub fn emit_expr(expr: &Spanned<Expr>, ctx: &CodegenContext) -> String {
    match &expr.node {
        Expr::Literal(lit) => emit_literal(lit),
        Expr::Ident(name) | Expr::Resolved { name, .. } => aver_name_to_lean(name),
        Expr::Attr(obj, field) => {
            if let Expr::Ident(type_name) = &obj.node {
                // Option.None → none
                if type_name == "Option" && field == "None" {
                    return "none".to_string();
                }
                // Oracle v1: `BranchPath.Root` is a nullary value
                // constructor defined in the Lean prelude — emit
                // verbatim so the reference resolves to the prelude
                // definition.
                if type_name == "BranchPath" && field == "Root" {
                    return "BranchPath.Root".to_string();
                }
                // User-defined type variant access: Shape.Point
                if is_user_type(type_name, ctx) {
                    return format!("{}.{}", type_name, to_lower_first(field));
                }
            }
            // Check module-qualified reference
            if let Some(full_dotted) = expr_to_dotted_name(&expr.node)
                && let Some((prefix, bare)) = resolve_module_call(&full_dotted, ctx)
            {
                if let Some(dot_pos) = bare.find('.') {
                    let type_name = &bare[..dot_pos];
                    let variant = &bare[dot_pos + 1..];
                    if is_user_type(type_name, ctx) {
                        return format!("{}.{}", type_name, to_lower_first(variant));
                    }
                }
                let bare_lean = aver_name_to_lean(bare);
                if !ctx.modules.is_empty() {
                    return format!("{}.{}", prefix, bare_lean);
                }
                return bare_lean;
            }
            let obj_str = emit_expr(obj, ctx);
            let needs_parens = !matches!(&obj.node, Expr::Ident(_) | Expr::Attr(_, _));
            if needs_parens {
                format!("({}).{}", obj_str, aver_name_to_lean(field))
            } else {
                format!("{}.{}", obj_str, aver_name_to_lean(field))
            }
        }
        Expr::FnCall(fn_expr, args) => emit_fn_call(fn_expr, args, ctx),
        Expr::Neg(inner) => format!("(-{})", emit_expr(inner, ctx)),
        Expr::BinOp(op, left, right) => {
            let l = emit_expr(left, ctx);
            let r = emit_expr(right, ctx);
            let op_str = match op {
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
            };
            format!("({} {} {})", l, op_str, r)
        }
        Expr::Match { subject, arms } => emit_match(subject, arms, expr.line, ctx),
        Expr::Constructor(name, arg) => emit_constructor(name, arg, ctx),
        Expr::ErrorProp(inner) => {
            // ? operator — unwrap Except using withDefault
            let inner_str = emit_expr(inner, ctx);
            format!("(({}).withDefault default)", inner_str)
        }
        Expr::InterpolatedStr(parts) => emit_interpolated_str(parts, ctx),
        Expr::List(elements) => {
            if elements.is_empty() {
                "[]".to_string()
            } else {
                let parts: Vec<String> = elements.iter().map(|e| emit_expr(e, ctx)).collect();
                format!("[{}]", parts.join(", "))
            }
        }
        Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            // Oracle v1: passed through as a tuple; `?!` semantic fold
            // is deferred (coordinates with the outer `Result.Ok(...)`
            // wrapper the typechecker forces).
            let parts: Vec<String> = items.iter().map(|e| emit_expr(e, ctx)).collect();
            format!("({})", parts.join(", "))
        }
        Expr::MapLiteral(entries) => {
            if entries.is_empty() {
                "[]".to_string()
            } else if entries
                .iter()
                .all(|(_, v)| crate::codegen::common::is_unit_expr(&v.node))
            {
                // Map<T, Unit> literal → set literal
                let parts: Vec<String> = entries.iter().map(|(k, _)| emit_expr(k, ctx)).collect();
                format!("AverSet.ofList [{}]", parts.join(", "))
            } else {
                let parts: Vec<String> = entries
                    .iter()
                    .map(|(k, v)| format!("({}, {})", emit_expr(k, ctx), emit_expr(v, ctx)))
                    .collect();
                format!("[{}]", parts.join(", "))
            }
        }
        Expr::RecordCreate { type_name, fields } => {
            let parts: Vec<String> = fields
                .iter()
                .map(|(name, expr)| {
                    format!("{} := {}", aver_name_to_lean(name), emit_expr(expr, ctx))
                })
                .collect();
            // Dotted type names (e.g. `Terminal.Size`, `Tcp.Connection`)
            // map to underscored Lean structure names — same translation
            // as `lean::types`'s `Type::Named` rendering.
            let lean_type_name = type_name.replace('.', "_");
            format!("{{ {} : {} }}", parts.join(", "), lean_type_name)
        }
        Expr::RecordUpdate {
            type_name: _,
            base,
            updates,
        } => {
            let base_str = emit_expr(base, ctx);
            let parts: Vec<String> = updates
                .iter()
                .map(|(name, expr)| {
                    format!("{} := {}", aver_name_to_lean(name), emit_expr(expr, ctx))
                })
                .collect();
            format!("{{ {} with {} }}", base_str, parts.join(", "))
        }
        Expr::TailCall(boxed) => {
            // TailCall is an internal optimization — emit as regular call
            let TailCallData { target, args, .. } = boxed.as_ref();
            let parts: Vec<String> = args.iter().map(|a| emit_expr_atom(a, ctx)).collect();
            if parts.is_empty() {
                aver_name_to_lean(target)
            } else {
                format!("{} {}", aver_name_to_lean(target), parts.join(" "))
            }
        }
    }
}

/// Emit an expression wrapped in parens if it's a compound expression.
fn emit_expr_atom(expr: &Spanned<Expr>, ctx: &CodegenContext) -> String {
    let s = emit_expr(expr, ctx);
    match &expr.node {
        Expr::Literal(Literal::Int(i)) if *i < 0 => format!("({})", s),
        Expr::Literal(Literal::Float(f)) if *f < 0.0 => format!("({})", s),
        Expr::Literal(_)
        | Expr::Ident(_)
        | Expr::List(_)
        | Expr::Tuple(_)
        | Expr::IndependentProduct(_, _) => s,
        _ => {
            if s.starts_with('(')
                || s.starts_with('[')
                || s.starts_with('"')
                || s.starts_with('{')
                || !s.contains(' ')
            {
                s
            } else {
                format!("({})", s)
            }
        }
    }
}

fn emit_literal(lit: &Literal) -> String {
    match lit {
        Literal::Int(i) => format!("{}", i),
        Literal::Float(f) => {
            let s = f.to_string();
            if s.contains('.') {
                s
            } else {
                format!("{}.0", s)
            }
        }
        Literal::Str(s) => format!("\"{}\"", escape_lean_string(s)),
        Literal::Bool(b) => if *b { "true" } else { "false" }.to_string(),
        Literal::Unit => "()".to_string(),
    }
}

fn escape_lean_string(s: &str) -> String {
    crate::codegen::common::escape_string_literal(s)
}

fn emit_fn_call(fn_expr: &Spanned<Expr>, args: &[Spanned<Expr>], ctx: &CodegenContext) -> String {
    let fn_name = expr_to_dotted_name(&fn_expr.node);

    if let Some(name) = &fn_name {
        // Check builtin
        if let Some(lean_code) = builtins::emit_builtin_call(name, args, ctx) {
            return lean_code;
        }

        // Oracle v1: BranchPath constructors render through the structure
        // definitions in LEAN_PRELUDE_BRANCH_PATH.
        let arg_strs_owned: Vec<String> = args.iter().map(|a| emit_expr_atom(a, ctx)).collect();
        match name.as_str() {
            "BranchPath.child" if arg_strs_owned.len() == 2 => {
                return format!(
                    "BranchPath.child {} {}",
                    arg_strs_owned[0], arg_strs_owned[1]
                );
            }
            "BranchPath.parse" if arg_strs_owned.len() == 1 => {
                return format!("BranchPath.parse {}", arg_strs_owned[0]);
            }
            _ => {}
        }

        // Module-qualified call
        if let Some((prefix, bare)) = resolve_module_call(name, ctx) {
            if let Some(dot_pos) = bare.find('.') {
                let type_name = &bare[..dot_pos];
                let variant_name = &bare[dot_pos + 1..];
                if is_user_type(type_name, ctx) {
                    let arg_strs: Vec<String> =
                        args.iter().map(|a| emit_expr_atom(a, ctx)).collect();
                    return format!(
                        "{}.{} {}",
                        type_name,
                        to_lower_first(variant_name),
                        arg_strs.join(" ")
                    );
                }
            }
            let arg_strs: Vec<String> = args.iter().map(|a| emit_expr_atom(a, ctx)).collect();
            let qualified = if !ctx.modules.is_empty() {
                format!("{}.{}", prefix, aver_name_to_lean(bare))
            } else {
                aver_name_to_lean(bare)
            };
            if arg_strs.is_empty() {
                return qualified;
            }
            return format!("{} {}", qualified, arg_strs.join(" "));
        }

        // User-defined type constructor: Shape.Circle(r)
        if let Some(dot_pos) = name.find('.') {
            let type_name = &name[..dot_pos];
            let variant_name = &name[dot_pos + 1..];
            if is_user_type(type_name, ctx) {
                let arg_strs: Vec<String> = args.iter().map(|a| emit_expr_atom(a, ctx)).collect();
                if arg_strs.is_empty() {
                    return format!("{}.{}", type_name, to_lower_first(variant_name));
                }
                return format!(
                    "{}.{} {}",
                    type_name,
                    to_lower_first(variant_name),
                    arg_strs.join(" ")
                );
            }
        }
    }

    // Regular function call — curried application
    let func = emit_expr(fn_expr, ctx);
    let arg_strs: Vec<String> = args.iter().map(|a| emit_expr_atom(a, ctx)).collect();
    if arg_strs.is_empty() {
        func
    } else {
        format!("{} {}", func, arg_strs.join(" "))
    }
}

fn emit_constructor(name: &str, arg: &Option<Box<Spanned<Expr>>>, ctx: &CodegenContext) -> String {
    // Accept both bare and qualified forms — parser / lifter may
    // produce either. Aver's `Result.Ok(x)` and `Ok(x)` are the same
    // wrapper semantically; same for Option.
    match name {
        "Ok" | "Result.Ok" => {
            let inner = arg
                .as_ref()
                .map(|a| emit_expr_atom(a, ctx))
                .unwrap_or_else(|| "()".to_string());
            format!("Except.ok {}", inner)
        }
        "Err" | "Result.Err" => {
            let inner = arg
                .as_ref()
                .map(|a| emit_expr_atom(a, ctx))
                .unwrap_or_else(|| "()".to_string());
            format!("Except.error {}", inner)
        }
        "Some" | "Option.Some" => {
            let inner = arg
                .as_ref()
                .map(|a| emit_expr_atom(a, ctx))
                .unwrap_or_else(|| "()".to_string());
            format!("some {}", inner)
        }
        "None" | "Option.None" => "none".to_string(),
        _ => {
            let inner = arg
                .as_ref()
                .map(|a| emit_expr_atom(a, ctx))
                .unwrap_or_else(|| "()".to_string());
            format!("{} {}", name, inner)
        }
    }
}

fn emit_interpolated_str(parts: &[StrPart], ctx: &CodegenContext) -> String {
    if parts.is_empty() {
        return "\"\"".to_string();
    }

    let mut result = String::new();
    result.push_str("s!\"");
    for part in parts {
        match part {
            StrPart::Literal(s) => {
                result.push_str(&escape_lean_string(s));
            }
            StrPart::Parsed(expr) => {
                result.push('{');
                result.push_str(&emit_expr(expr, ctx));
                result.push('}');
            }
        }
    }
    result.push('"');
    result
}

fn emit_match(
    subject: &Spanned<Expr>,
    arms: &[MatchArm],
    line: usize,
    ctx: &CodegenContext,
) -> String {
    // Bool match → if/then/else (avoids Lean dependent elimination issues)
    if let Some((true_body, false_body)) = extract_bool_arms(arms) {
        let cond = emit_expr(subject, ctx);
        let t = emit_expr(true_body, ctx);
        let f = emit_expr(false_body, ctx);
        return format!("if {} then {}\n  else {}", cond, t, f);
    }

    let subj = emit_expr(subject, ctx);
    let mut arm_strs = Vec::new();
    for arm in arms {
        let pat = emit_pattern(&arm.pattern);
        let body = emit_expr(&arm.body, ctx);
        if body.contains('\n') {
            let body_lines: Vec<&str> = body.lines().collect();
            let mut rendered = vec![format!("  | {} => {}", pat, body_lines[0])];
            rendered.extend(
                body_lines
                    .iter()
                    .skip(1)
                    .map(|line| format!("    {}", line)),
            );
            arm_strs.push(rendered.join("\n"));
        } else {
            arm_strs.push(format!("  | {} => {}", pat, body));
        }
    }
    // Plain `match … with` (no `h_NN :` named hypothesis). The named
    // form was historically defensive — equation-style proofs could
    // refer to `h_NN` for hypothesis-driven reasoning — but the
    // generated output never actually uses those bindings, and the
    // shape blocks `simp` from reducing if-inside-match inside the
    // auto-proof tactic for wrapper-return cross-module laws. The
    // unused-variable linter was warning on them in every Lean
    // export. Drop them so `simp [hyp]` + `by_cases` actually closes
    // the resulting goal. Three fuel-helper emitters still call
    // `strip_match_eq_binders`; with this change that strip is a
    // no-op, but leaving the call in place keeps the older paths
    // tolerant of any future re-introduction of named binders.
    let _ = line;
    format!("match {} with\n{}", subj, arm_strs.join("\n"))
}

/// If all arms are `true -> expr` and `false -> expr`, return (true_body, false_body).
fn extract_bool_arms(arms: &[MatchArm]) -> Option<(&Spanned<Expr>, &Spanned<Expr>)> {
    if arms.len() != 2 {
        return None;
    }
    let mut true_body = None;
    let mut false_body = None;
    for arm in arms {
        match &arm.pattern {
            Pattern::Literal(Literal::Bool(true)) => true_body = Some(arm.body.as_ref()),
            Pattern::Literal(Literal::Bool(false)) => false_body = Some(arm.body.as_ref()),
            _ => return None,
        }
    }
    Some((true_body?, false_body?))
}

/// Emit a statement as Lean 4 code.
pub fn emit_stmt(stmt: &Stmt, ctx: &CodegenContext) -> String {
    match stmt {
        Stmt::Binding(name, type_ann, expr) => {
            let val = emit_expr(expr, ctx);
            let _ = type_ann;
            format!("let {} := {}", aver_name_to_lean(name), val)
        }
        Stmt::Expr(expr) => emit_expr(expr, ctx),
    }
}

/// Convert an Aver identifier to a valid Lean 4 identifier.
/// Lean 4 reserved words.
const LEAN_RESERVED: &[&str] = &[
    "abbrev",
    "axiom",
    "by",
    "calc",
    "class",
    "def",
    "decreasing_by",
    "deriving",
    "do",
    "else",
    "end",
    "example",
    "from",
    "fun",
    "have",
    "id",
    "if",
    "import",
    "in",
    "inductive",
    "infix",
    "infixl",
    "infixr",
    "instance",
    "let",
    "local",
    "macro",
    "match",
    "mutual",
    "namespace",
    "noncomputable",
    "nonrec",
    "opaque",
    "open",
    "partial",
    "postfix",
    "prefix",
    "priority",
    "private",
    "protected",
    "public",
    "repeat",
    "return",
    "scoped",
    "section",
    "show",
    "structure",
    "syntax",
    "termination_by",
    "then",
    "theorem",
    "toString",
    "unsafe",
    "where",
    "with",
];

pub fn aver_name_to_lean(name: &str) -> String {
    crate::codegen::common::escape_reserved_word(name, LEAN_RESERVED, "'")
}

#[cfg(test)]
mod tests {
    use super::{aver_name_to_lean, escape_lean_string};

    #[test]
    fn aver_name_to_lean_escapes_lean_reserved_words() {
        assert_eq!(aver_name_to_lean("repeat"), "repeat'");
        assert_eq!(aver_name_to_lean("from"), "from'");
        assert_eq!(aver_name_to_lean("by"), "by'");
        assert_eq!(aver_name_to_lean("termination_by"), "termination_by'");
        assert_eq!(aver_name_to_lean("value"), "value");
    }

    #[test]
    fn escape_lean_string_escapes_control_chars() {
        assert_eq!(escape_lean_string("\u{0008}\u{000C}"), "\\x08\\x0c");
        assert_eq!(escape_lean_string("a\n\t\"\\z"), "a\\n\\t\\\"\\\\z");
    }
}

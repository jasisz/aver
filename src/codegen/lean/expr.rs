/// Aver expressions → Lean 4 expression strings.
use super::builtins;
use super::pattern::emit_pattern;
use super::shared::to_lower_first;
use crate::ast::*;
use crate::codegen::CodegenContext;
use crate::codegen::common::{expr_to_dotted_name, is_user_type, resolve_module_call};

/// Emit a Lean 4 expression from an Aver Expr.
pub fn emit_expr(expr: &Expr, ctx: &CodegenContext) -> String {
    match expr {
        Expr::Literal(lit) => emit_literal(lit),
        Expr::Ident(name) => aver_name_to_lean(name),
        Expr::Resolved(slot) => {
            panic!(
                "Lean codegen: encountered resolver-only Expr::Resolved({slot}). \
                 Compile pipeline should emit source-level AST (Ident), not slot-indexed AST."
            )
        }
        Expr::Attr(obj, field) => {
            if let Expr::Ident(type_name) = obj.as_ref() {
                // Option.None → none
                if type_name == "Option" && field == "None" {
                    return "none".to_string();
                }
                // User-defined type variant access: Shape.Point
                if is_user_type(type_name, ctx) {
                    return format!("{}.{}", type_name, to_lower_first(field));
                }
            }
            // Check module-qualified reference
            if let Some(full_dotted) = expr_to_dotted_name(expr)
                && let Some(bare) = resolve_module_call(&full_dotted, ctx) {
                    if let Some(dot_pos) = bare.find('.') {
                        let type_name = &bare[..dot_pos];
                        let variant = &bare[dot_pos + 1..];
                        if is_user_type(type_name, ctx) {
                            return format!("{}.{}", type_name, to_lower_first(variant));
                        }
                    }
                    return aver_name_to_lean(&bare);
                }
            let obj_str = emit_expr(obj, ctx);
            format!("{}.{}", obj_str, aver_name_to_lean(field))
        }
        Expr::FnCall(fn_expr, args) => emit_fn_call(fn_expr, args, ctx),
        Expr::BinOp(op, left, right) => {
            // Unary minus
            if matches!(op, BinOp::Sub) && matches!(left.as_ref(), Expr::Literal(Literal::Int(0))) {
                let r = emit_expr(right, ctx);
                return format!("(-{})", r);
            }
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
        Expr::Match { subject, arms, .. } => emit_match(subject, arms, ctx),
        Expr::Pipe(left, right) => {
            let arg = emit_expr(left, ctx);
            // Check if the right side is a function call with additional args
            // Otherwise, pipe as function application
            match right.as_ref() {
                Expr::Ident(name) => {
                    format!("{} |> {}", arg, aver_name_to_lean(name))
                }
                Expr::Attr(..) => {
                    let func = emit_expr(right, ctx);
                    format!("{} |> {}", arg, func)
                }
                _ => {
                    let func = emit_expr(right, ctx);
                    format!("{} |> {}", arg, func)
                }
            }
        }
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
        Expr::Tuple(items) => {
            let parts: Vec<String> = items.iter().map(|e| emit_expr(e, ctx)).collect();
            format!("({})", parts.join(", "))
        }
        Expr::MapLiteral(entries) => {
            if entries.is_empty() {
                "[]".to_string()
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
            format!("{{ {} : {} }}", parts.join(", "), type_name)
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
            let (target, args) = boxed.as_ref();
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
fn emit_expr_atom(expr: &Expr, ctx: &CodegenContext) -> String {
    let s = emit_expr(expr, ctx);
    match expr {
        Expr::Literal(Literal::Int(i)) if *i < 0 => format!("({})", s),
        Expr::Literal(Literal::Float(f)) if *f < 0.0 => format!("({})", s),
        Expr::Literal(_) | Expr::Ident(_) | Expr::List(_) | Expr::Tuple(_) => s,
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
        Literal::Str(s) => format!("{:?}", s),
        Literal::Bool(b) => if *b { "true" } else { "false" }.to_string(),
    }
}

fn emit_fn_call(fn_expr: &Expr, args: &[Expr], ctx: &CodegenContext) -> String {
    let fn_name = expr_to_dotted_name(fn_expr);

    if let Some(name) = &fn_name {
        // Check builtin
        if let Some(lean_code) = builtins::emit_builtin_call(name, args, ctx) {
            return lean_code;
        }

        // Module-qualified call
        if let Some(bare) = resolve_module_call(name, ctx) {
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
            if arg_strs.is_empty() {
                return aver_name_to_lean(&bare);
            }
            return format!("{} {}", aver_name_to_lean(&bare), arg_strs.join(" "));
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

fn emit_constructor(name: &str, arg: &Option<Box<Expr>>, ctx: &CodegenContext) -> String {
    match name {
        "Ok" => {
            let inner = arg
                .as_ref()
                .map(|a| emit_expr_atom(a, ctx))
                .unwrap_or_else(|| "()".to_string());
            format!("Except.ok {}", inner)
        }
        "Err" => {
            let inner = arg
                .as_ref()
                .map(|a| emit_expr_atom(a, ctx))
                .unwrap_or_else(|| "()".to_string());
            format!("Except.error {}", inner)
        }
        "Some" => {
            let inner = arg
                .as_ref()
                .map(|a| emit_expr_atom(a, ctx))
                .unwrap_or_else(|| "()".to_string());
            format!("some {}", inner)
        }
        "None" => "none".to_string(),
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
                // Escape special chars for Lean s!"..." strings
                let escaped = s.replace('\\', "\\\\").replace('"', "\\\"");
                result.push_str(&escaped);
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

fn emit_match(subject: &Expr, arms: &[MatchArm], ctx: &CodegenContext) -> String {
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
        arm_strs.push(format!("  | {} => {}", pat, body));
    }
    format!("match {} with\n{}", subj, arm_strs.join("\n"))
}

/// If all arms are `true -> expr` and `false -> expr`, return (true_body, false_body).
fn extract_bool_arms(arms: &[MatchArm]) -> Option<(&Expr, &Expr)> {
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
        Stmt::Binding(name, _type_ann, expr) => {
            let val = emit_expr(expr, ctx);
            format!("let {} := {}", aver_name_to_lean(name), val)
        }
        Stmt::Expr(expr) => emit_expr(expr, ctx),
    }
}

/// Convert an Aver identifier to a valid Lean 4 identifier.
pub fn aver_name_to_lean(name: &str) -> String {
    // Lean reserved words
    match name {
        "def" => "def'".to_string(),
        "let" => "let'".to_string(),
        "where" => "where'".to_string(),
        "match" => "match'".to_string(),
        "with" => "with'".to_string(),
        "do" => "do'".to_string(),
        "if" => "if'".to_string(),
        "then" => "then'".to_string(),
        "else" => "else'".to_string(),
        "return" => "return'".to_string(),
        "in" => "in'".to_string(),
        "fun" => "fun'".to_string(),
        "theorem" => "theorem'".to_string(),
        "example" => "example'".to_string(),
        "class" => "class'".to_string(),
        "instance" => "instance'".to_string(),
        "structure" => "structure'".to_string(),
        "inductive" => "inductive'".to_string(),
        "namespace" => "namespace'".to_string(),
        "open" => "open'".to_string(),
        "section" => "section'".to_string(),
        "end" => "end'".to_string(),
        "import" => "import'".to_string(),
        "id" => "id'".to_string(),
        "mutual" => "mutual'".to_string(),
        "partial" => "partial'".to_string(),
        _ => name.to_string(),
    }
}

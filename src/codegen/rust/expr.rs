use super::builtins;
use super::liveness::{EmitCtx, collect_vars, compute_args_used_after};
use super::pattern::emit_pattern;
use crate::ast::*;
use crate::codegen::CodegenContext;
use crate::codegen::common::{expr_to_dotted_name, is_user_type, resolve_module_call};
/// Aver expressions → Rust expression strings.
use std::collections::HashSet;

/// Emit a Rust expression from an Aver Expr.
pub fn emit_expr(expr: &Expr, ctx: &CodegenContext, ectx: &EmitCtx) -> String {
    match expr {
        Expr::Literal(lit) => emit_literal(lit),
        Expr::Ident(name) => aver_name_to_rust(name),
        Expr::Resolved(slot) => {
            panic!(
                "Rust codegen: encountered resolver-only Expr::Resolved({slot}). \
                 Compile pipeline should emit source-level AST (Ident), not slot-indexed AST."
            )
        }
        Expr::Attr(obj, field) => {
            if let Expr::Ident(type_name) = obj.as_ref() {
                // Option.None → None
                if type_name == "Option" && field == "None" {
                    return "None".to_string();
                }
                // User-defined type constructor access: Shape.Point
                if is_user_type(type_name, ctx) {
                    return format!("{}::{}", type_name, field);
                }
            }
            // Check if this is a module-qualified reference: Examples.Fibonacci.fib
            if let Some(full_dotted) = expr_to_dotted_name(expr) {
                if let Some(bare) = resolve_module_call(&full_dotted, ctx) {
                    // Could be a simple function name or a type.variant
                    if let Some(dot_pos) = bare.find('.') {
                        let type_name = &bare[..dot_pos];
                        let variant = &bare[dot_pos + 1..];
                        if is_user_type(type_name, ctx) {
                            return format!("{}::{}", type_name, variant);
                        }
                    }
                    return aver_name_to_rust(&bare);
                }
            }
            let obj_str = emit_expr(obj, ctx, ectx);
            format!("{}.{}", obj_str, aver_name_to_rust(field))
        }
        Expr::FnCall(fn_expr, args) => emit_fn_call(fn_expr, args, ctx, ectx),
        Expr::BinOp(op, left, right) => {
            // Unary minus: `- expr` is parsed as `BinOp(Sub, Literal(Int(0)), expr)`.
            // Emit as `-expr` instead of `(0i64 - expr)` to avoid type mismatch
            // when the operand is Float.
            if matches!(op, BinOp::Sub) && matches!(left.as_ref(), Expr::Literal(Literal::Int(0))) {
                let r = emit_expr(right, ctx, ectx);
                return format!("(-{})", r);
            }
            // BinOp: left's used_after includes vars from right
            let right_vars = collect_vars(right);
            let left_ectx = ectx.with_used_after(&right_vars);
            let l = emit_expr(left, ctx, &left_ectx);
            let r = emit_expr(right, ctx, ectx);
            match op {
                BinOp::Add => {
                    // String + String doesn't compile in Rust; use (l + &r) which works
                    // for both String + &String (→ &str via Deref) and i64 + &i64.
                    format!("({} + &{})", l, r)
                }
                _ => {
                    let op_str = match op {
                        BinOp::Add => unreachable!(),
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
            }
        }
        Expr::Match { subject, arms, .. } => emit_match(subject, arms, ctx, ectx),
        Expr::Pipe(left, right) => {
            // a |> f → f(a)
            let right_vars = collect_vars(right);
            let left_ectx = ectx.with_used_after(&right_vars);
            let arg = clone_arg(left, ctx, &left_ectx);
            let func = emit_expr(right, ctx, ectx);
            format!("{}({})", func, arg)
        }
        Expr::Constructor(name, arg) => emit_constructor(name, arg, ctx, ectx),
        Expr::ErrorProp(inner) => {
            let inner_str = emit_expr(inner, ctx, ectx);
            format!("{}?", inner_str)
        }
        Expr::InterpolatedStr(parts) => emit_interpolated_str(parts, ctx, ectx),
        Expr::List(elements) => {
            if elements.is_empty() {
                "vec![]".to_string()
            } else {
                let parts: Vec<String> = elements.iter().map(|e| emit_expr(e, ctx, ectx)).collect();
                format!("vec![{}]", parts.join(", "))
            }
        }
        Expr::Tuple(items) => {
            let parts: Vec<String> = items.iter().map(|e| emit_expr(e, ctx, ectx)).collect();
            format!("({})", parts.join(", "))
        }
        Expr::MapLiteral(entries) => {
            if entries.is_empty() {
                "HashMap::new()".to_string()
            } else {
                let mut parts = Vec::new();
                for (k, v) in entries {
                    parts.push(format!(
                        "({}, {})",
                        emit_expr(k, ctx, ectx),
                        emit_expr(v, ctx, ectx)
                    ));
                }
                format!(
                    "vec![{}].into_iter().collect::<HashMap<_, _>>()",
                    parts.join(", ")
                )
            }
        }
        Expr::RecordCreate { type_name, fields } => {
            let field_exprs: Vec<Expr> = fields.iter().map(|(_, e)| e.clone()).collect();
            let field_ctxs =
                compute_args_used_after(&field_exprs, &ectx.used_after, &ectx.local_types);
            let parts: Vec<String> = fields
                .iter()
                .enumerate()
                .map(|(i, (name, expr))| {
                    format!(
                        "{}: {}",
                        aver_name_to_rust(name),
                        clone_arg(expr, ctx, &field_ctxs[i])
                    )
                })
                .collect();
            format!("{} {{ {} }}", type_name, parts.join(", "))
        }
        Expr::RecordUpdate {
            type_name,
            base,
            updates,
        } => {
            let base_str = emit_expr(base, ctx, ectx);
            let parts: Vec<String> = updates
                .iter()
                .map(|(name, expr)| {
                    format!(
                        "{}: {}",
                        aver_name_to_rust(name),
                        emit_expr(expr, ctx, ectx)
                    )
                })
                .collect();
            format!("{} {{ {}, ..{} }}", type_name, parts.join(", "), base_str)
        }
        Expr::TailCall(boxed) => {
            // TailCall outside of a TCO loop → emit as regular function call
            let (target, args) = boxed.as_ref();
            let arg_ctxs = compute_args_used_after(args, &ectx.used_after, &ectx.local_types);
            let parts: Vec<String> = args
                .iter()
                .zip(arg_ctxs.iter())
                .map(|(a, ac)| clone_arg(a, ctx, ac))
                .collect();
            format!("{}({})", aver_name_to_rust(target), parts.join(", "))
        }
    }
}

fn emit_literal(lit: &Literal) -> String {
    match lit {
        Literal::Int(i) => format!("{}i64", i),
        Literal::Float(f) => {
            let s = f.to_string();
            if s.contains('.') || s.contains('e') || s.contains('E') {
                format!("{}f64", s)
            } else {
                format!("{}.0f64", s)
            }
        }
        Literal::Str(s) => format!("{:?}.to_string()", s),
        Literal::Bool(b) => if *b { "true" } else { "false" }.to_string(),
    }
}

fn emit_fn_call(fn_expr: &Expr, args: &[Expr], ctx: &CodegenContext, ectx: &EmitCtx) -> String {
    // Check if this is a builtin call like Console.print, List.map, etc.
    let fn_name = expr_to_dotted_name(fn_expr);

    if let Some(name) = &fn_name {
        if let Some(rust_code) = builtins::emit_builtin_call(name, args, ctx, ectx) {
            return rust_code;
        }

        // Check module-qualified call: Examples.Fibonacci.fib → fib
        if let Some(bare) = resolve_module_call(name, ctx) {
            // Could be a simple function or a type constructor (e.g. Shape.Circle)
            if let Some(dot_pos) = bare.find('.') {
                let type_name = &bare[..dot_pos];
                let variant_name = &bare[dot_pos + 1..];
                if is_user_type(type_name, ctx) {
                    let arg_ctxs =
                        compute_args_used_after(args, &ectx.used_after, &ectx.local_types);
                    let arg_strs: Vec<String> = args
                        .iter()
                        .zip(arg_ctxs.iter())
                        .map(|(a, ac)| clone_arg(a, ctx, ac))
                        .collect();
                    return format!("{}::{}({})", type_name, variant_name, arg_strs.join(", "));
                }
            }
            let arg_ctxs = compute_args_used_after(args, &ectx.used_after, &ectx.local_types);
            let arg_strs: Vec<String> = args
                .iter()
                .zip(arg_ctxs.iter())
                .map(|(a, ac)| clone_arg(a, ctx, ac))
                .collect();
            return format!("{}({})", aver_name_to_rust(&bare), arg_strs.join(", "));
        }

        // Check if this is a user-defined type constructor: Shape.Circle(r)
        if let Some(dot_pos) = name.find('.') {
            let type_name = &name[..dot_pos];
            let variant_name = &name[dot_pos + 1..];
            if is_user_type(type_name, ctx) {
                let arg_ctxs = compute_args_used_after(args, &ectx.used_after, &ectx.local_types);
                let arg_strs: Vec<String> = args
                    .iter()
                    .zip(arg_ctxs.iter())
                    .map(|(a, ac)| clone_arg(a, ctx, ac))
                    .collect();
                return format!("{}::{}({})", type_name, variant_name, arg_strs.join(", "));
            }
        }
    }

    // Regular function call — compute per-arg used_after
    let func = emit_expr(fn_expr, ctx, ectx);
    let arg_ctxs = compute_args_used_after(args, &ectx.used_after, &ectx.local_types);
    let arg_strs: Vec<String> = args
        .iter()
        .zip(arg_ctxs.iter())
        .map(|(a, ac)| clone_arg(a, ctx, ac))
        .collect();
    format!("{}({})", func, arg_strs.join(", "))
}

/// Clone a value if it's a variable reference (to avoid move issues in generated Rust).
/// Literals and complex expressions don't need cloning.
/// Uses EmitCtx to skip cloning for last-use or Copy-type variables.
pub(super) fn maybe_clone(code: String, expr: &Expr, ectx: &EmitCtx) -> String {
    match expr {
        Expr::Ident(name) => {
            if ectx.skip_clone(name) {
                code
            } else {
                format!("{}.clone()", code)
            }
        }
        Expr::Resolved(slot) => {
            panic!(
                "Rust codegen: encountered resolver-only Expr::Resolved({slot}) in clone path. \
                 Compile pipeline should emit source-level AST (Ident), not slot-indexed AST."
            )
        }
        Expr::Attr(obj, _) => {
            // Record field access — clone it (partial moves too complex)
            if !matches!(obj.as_ref(), Expr::Ident(n) if is_builtin_namespace(n)) {
                format!("{}.clone()", code)
            } else {
                code
            }
        }
        _ => code,
    }
}

/// Emit an expression as a function argument, cloning variables to prevent move errors.
pub(super) fn clone_arg(expr: &Expr, ctx: &CodegenContext, ectx: &EmitCtx) -> String {
    let code = emit_expr(expr, ctx, ectx);
    maybe_clone(code, expr, ectx)
}

fn is_builtin_namespace(name: &str) -> bool {
    matches!(
        name,
        "Console"
            | "Disk"
            | "Http"
            | "HttpServer"
            | "Tcp"
            | "Int"
            | "Float"
            | "String"
            | "List"
            | "Map"
            | "Char"
            | "Byte"
            | "Result"
            | "Option"
    )
}

fn emit_constructor(
    name: &str,
    arg: &Option<Box<Expr>>,
    ctx: &CodegenContext,
    ectx: &EmitCtx,
) -> String {
    match name {
        "Ok" => {
            let inner = arg
                .as_ref()
                .map(|a| clone_arg(a, ctx, ectx))
                .unwrap_or_else(|| "()".to_string());
            format!("Ok({})", inner)
        }
        "Err" => {
            let inner = arg
                .as_ref()
                .map(|a| clone_arg(a, ctx, ectx))
                .unwrap_or_else(|| "()".to_string());
            format!("Err({})", inner)
        }
        "Some" => {
            let inner = arg
                .as_ref()
                .map(|a| clone_arg(a, ctx, ectx))
                .unwrap_or_else(|| "()".to_string());
            format!("Some({})", inner)
        }
        "None" => "None".to_string(),
        _ => {
            // Should not happen — constructors are FnCall via namespace
            let inner = arg
                .as_ref()
                .map(|a| clone_arg(a, ctx, ectx))
                .unwrap_or_else(|| "()".to_string());
            format!("{}({})", name, inner)
        }
    }
}

fn emit_interpolated_str(parts: &[StrPart], ctx: &CodegenContext, ectx: &EmitCtx) -> String {
    if parts.is_empty() {
        return "String::new()".to_string();
    }

    let mut fmt_str = String::new();
    let mut fmt_args = Vec::new();

    for part in parts {
        match part {
            StrPart::Literal(s) => {
                // Escape braces for format! macro
                let escaped = s.replace('{', "{{").replace('}', "}}");
                fmt_str.push_str(&escaped);
            }
            StrPart::Parsed(expr) => {
                fmt_str.push_str("{}");
                fmt_args.push(format!(
                    "aver_rt::aver_display(&{})",
                    emit_expr(expr, ctx, ectx)
                ));
            }
        }
    }

    if fmt_args.is_empty() {
        format!("{:?}.to_string()", fmt_str)
    } else {
        format!("format!({:?}, {})", fmt_str, fmt_args.join(", "))
    }
}

fn emit_match(subject: &Expr, arms: &[MatchArm], ctx: &CodegenContext, ectx: &EmitCtx) -> String {
    // Subject's used_after: all vars in arms + parent used_after
    let mut arms_vars = HashSet::new();
    for arm in arms {
        let mut arm_vars = collect_vars(&arm.body);
        let bindings = super::liveness::pattern_bindings(&arm.pattern);
        for b in &bindings {
            arm_vars.remove(b);
        }
        arms_vars.extend(arm_vars);
    }
    let subj_ectx = ectx.with_used_after(&arms_vars);
    let subj = emit_expr(subject, ctx, &subj_ectx);

    // Determine if subject needs special treatment
    let needs_as_str = subject_might_be_string(subject, ctx);
    let needs_as_slice = subject_might_be_list(subject, arms, ctx);

    let match_expr = if needs_as_str && has_string_literal_patterns(arms) {
        format!("{}.as_str()", subj)
    } else if needs_as_slice && has_list_patterns(arms) {
        format!("{}.as_slice()", subj)
    } else {
        subj
    };

    let mut arm_strs = Vec::new();
    for arm in arms {
        let pat = emit_pattern(&arm.pattern, needs_as_str, ctx);
        // Each arm body is independent — use parent's used_after
        let body = emit_expr(&arm.body, ctx, ectx);
        // For Cons patterns, rebind head and tail to owned types
        // (head is &T from slice, tail is &[T] from slice — need .clone() and .to_vec())
        let rebindings = if let Pattern::Cons(head, tail) = &arm.pattern {
            format!(
                "let {} = {}.clone();\n            let {} = {}.to_vec();\n            ",
                aver_name_to_rust(head),
                aver_name_to_rust(head),
                aver_name_to_rust(tail),
                aver_name_to_rust(tail)
            )
        } else {
            String::new()
        };
        arm_strs.push(format!(
            "        {} => {{\n            {}{}\n        }}",
            pat, rebindings, body
        ));
    }

    format!("match {} {{\n{}\n    }}", match_expr, arm_strs.join(",\n"))
}

fn subject_might_be_string(_subject: &Expr, _ctx: &CodegenContext) -> bool {
    // Heuristic: if subject is an ident, we can't tell at codegen time
    // We'll rely on the patterns to decide
    true
}

fn subject_might_be_list(_subject: &Expr, _arms: &[MatchArm], _ctx: &CodegenContext) -> bool {
    true
}

pub(super) fn has_string_literal_patterns(arms: &[MatchArm]) -> bool {
    arms.iter()
        .any(|arm| matches!(&arm.pattern, Pattern::Literal(Literal::Str(_))))
}

pub(super) fn has_list_patterns(arms: &[MatchArm]) -> bool {
    arms.iter()
        .any(|arm| matches!(&arm.pattern, Pattern::EmptyList | Pattern::Cons(_, _)))
}

/// Emit a statement as Rust code.
pub fn emit_stmt(stmt: &Stmt, ctx: &CodegenContext, ectx: &EmitCtx) -> String {
    match stmt {
        Stmt::Binding(name, _type_ann, expr) => {
            let val = emit_expr(expr, ctx, ectx);
            format!("let {} = {};", aver_name_to_rust(name), val)
        }
        Stmt::Expr(expr) => {
            let val = emit_expr(expr, ctx, ectx);
            format!("{};", val)
        }
    }
}

/// Convert an Aver identifier to a valid Rust identifier.
/// Handles snake_case and reserved words.
pub fn aver_name_to_rust(name: &str) -> String {
    // Rust reserved words that might conflict
    match name {
        "type" => "r#type".to_string(),
        "match" => "r#match".to_string(),
        "fn" => "r#fn".to_string(),
        "let" => "r#let".to_string(),
        "use" => "r#use".to_string(),
        "mod" => "r#mod".to_string(),
        "impl" => "r#impl".to_string(),
        "trait" => "r#trait".to_string(),
        "struct" => "r#struct".to_string(),
        "enum" => "r#enum".to_string(),
        "self" => "r#self".to_string(),
        "super" => "r#super".to_string(),
        "crate" => "r#crate".to_string(),
        "where" => "r#where".to_string(),
        "async" => "r#async".to_string(),
        "await" => "r#await".to_string(),
        "move" => "r#move".to_string(),
        "ref" => "r#ref".to_string(),
        "mut" => "r#mut".to_string(),
        "loop" => "r#loop".to_string(),
        "break" => "r#break".to_string(),
        "continue" => "r#continue".to_string(),
        "return" => "r#return".to_string(),
        "yield" => "r#yield".to_string(),
        "box" => "r#box".to_string(),
        "in" => "r#in".to_string(),
        _ => name.to_string(),
    }
}

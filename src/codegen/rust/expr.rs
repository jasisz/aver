use super::builtins;
use super::liveness::{EmitCtx, collect_vars, compute_args_used_after_with_rc};
use super::pattern::emit_pattern;
use crate::ast::*;
use crate::codegen::common::{
    expr_to_dotted_name, is_user_type, module_prefix_to_rust_path, resolve_module_call,
};
use crate::codegen::{CodegenContext, EmissionStyle};
use crate::ir::{
    BoolCompareOp, BoolSubjectPlan, CallLowerCtx, CallPlan, DispatchArmPlan, DispatchBindingPlan,
    DispatchDefaultPlan, DispatchLiteral, DispatchTableShape, LeafOp, ListMatchShape,
    MatchDispatchPlan, SemanticConstructor, SemanticDispatchPattern, TailCallPlan, WrapperKind,
    classify_bool_subject_plan, classify_call_plan, classify_constructor_name, classify_leaf_op,
    classify_list_match_shape, classify_match_dispatch_plan, classify_tail_call_plan,
    is_builtin_namespace,
};
use crate::types::Type;
/// Aver expressions → Rust expression strings.
use std::collections::HashSet;

pub use super::syntax::{aver_name_to_rust, emit_stmt};
pub(super) use super::syntax::{has_list_patterns, has_string_literal_patterns};

struct RustCallCtx<'a, 'b> {
    ctx: &'a CodegenContext,
    ectx: &'b EmitCtx,
}

impl CallLowerCtx for RustCallCtx<'_, '_> {
    fn is_local_value(&self, name: &str) -> bool {
        self.ectx.local_types.contains_key(name)
    }

    fn is_user_type(&self, name: &str) -> bool {
        is_user_type(name, self.ctx)
    }

    fn resolve_module_call<'a>(&self, dotted: &'a str) -> Option<(&'a str, &'a str)> {
        let mut best = None;
        for (dot_idx, _) in dotted.match_indices('.') {
            let prefix = &dotted[..dot_idx];
            let suffix = &dotted[dot_idx + 1..];
            if self.ctx.module_prefixes.contains(prefix)
                && best.is_none_or(|existing: (&str, &str)| prefix.len() > existing.0.len())
            {
                best = Some((prefix, suffix));
            }
        }
        best
    }
}

fn use_optimized_emission(ctx: &CodegenContext) -> bool {
    matches!(ctx.emission_style, EmissionStyle::Optimized)
}

pub(super) fn classify_dispatch_plan_for_rust(
    arms: &[MatchArm],
    ctx: &CodegenContext,
    ectx: &EmitCtx,
) -> Option<MatchDispatchPlan> {
    let lower_ctx = RustCallCtx { ctx, ectx };
    classify_match_dispatch_plan(arms, &lower_ctx)
}

/// Emit a Rust expression from an Aver Expr.
pub fn emit_expr(expr: &Expr, ctx: &CodegenContext, ectx: &EmitCtx) -> String {
    let lower_ctx = RustCallCtx { ctx, ectx };
    if use_optimized_emission(ctx)
        && let Some(leaf) = classify_leaf_op(expr, &lower_ctx)
    {
        return emit_leaf_op(leaf, ctx, ectx);
    }

    match expr {
        Expr::Literal(lit) => emit_literal(lit),
        Expr::Ident(name) => aver_name_to_rust(name),
        Expr::Resolved(slot) => emit_codegen_error_expr(format!(
            "Rust codegen: encountered resolver-only Expr::Resolved({slot}). \
             Compile pipeline should emit source-level AST (Ident), not slot-indexed AST."
        )),
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
            // Check if this is a module-qualified reference: Fibonacci.fib
            if let Some(full_dotted) = expr_to_dotted_name(expr)
                && let Some((prefix, bare)) = resolve_module_call(&full_dotted, ctx)
            {
                let module_path = module_prefix_to_rust_path(prefix);
                // Could be a simple function name or a type.variant
                if let Some(dot_pos) = bare.find('.') {
                    let type_name = &bare[..dot_pos];
                    let variant = &bare[dot_pos + 1..];
                    if is_user_type(type_name, ctx) {
                        return format!("{}::{}::{}", module_path, type_name, variant);
                    }
                }
                return format!("{}::{}", module_path, aver_name_to_rust(bare));
            }
            let obj_str = emit_expr(obj, ctx, ectx);
            let field_access = format!("{}.{}", obj_str, aver_name_to_rust(field));
            if matches!(obj.as_ref(), Expr::Ident(name) if is_builtin_namespace(name)) {
                field_access
            } else {
                format!("{}.clone()", field_access)
            }
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
                    // AverStr newtype implements Add<&AverStr>, so (l + &r) works
                    // for both numeric types and strings.
                    let l = maybe_clone(l, left, &left_ectx);
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
                    // For Eq/Neq with a string literal on either side, deref AverStr (Rc<str>)
                    // to &str for comparison since Rc<str> doesn't impl PartialEq<&str>.
                    if matches!(op, BinOp::Eq | BinOp::Neq) {
                        if let Expr::Literal(Literal::Str(s)) = right.as_ref() {
                            return format!("(&*{} {} {:?})", l, op_str, s);
                        }
                        if let Expr::Literal(Literal::Str(s)) = left.as_ref() {
                            return format!("({:?} {} &*{})", s, op_str, r);
                        }
                    }
                    format!("({} {} {})", l, op_str, r)
                }
            }
        }
        Expr::Match { subject, arms, .. } => emit_match(subject, arms, ctx, ectx),
        Expr::Constructor(name, arg) => emit_constructor(name, arg, ctx, ectx),
        Expr::ErrorProp(inner) => {
            let inner_str = emit_expr(inner, ctx, ectx);
            format!("{}?", inner_str)
        }
        Expr::InterpolatedStr(parts) => emit_interpolated_str(parts, ctx, ectx),
        Expr::List(elements) => {
            if elements.is_empty() {
                "aver_rt::AverList::empty()".to_string()
            } else {
                let elem_ctxs = compute_args_used_after_with_rc(
                    elements,
                    &ectx.used_after,
                    &ectx.local_types,
                    &ectx.rc_wrapped,
                );
                let parts: Vec<String> = elements
                    .iter()
                    .zip(elem_ctxs.iter())
                    .map(|(e, elem_ctx)| clone_arg(e, ctx, elem_ctx))
                    .collect();
                format!("aver_rt::AverList::from_vec(vec![{}])", parts.join(", "))
            }
        }
        Expr::Tuple(items) => {
            let item_ctxs = compute_args_used_after_with_rc(
                items,
                &ectx.used_after,
                &ectx.local_types,
                &ectx.rc_wrapped,
            );
            let parts: Vec<String> = items
                .iter()
                .zip(item_ctxs.iter())
                .map(|(e, item_ctx)| clone_arg(e, ctx, item_ctx))
                .collect();
            format!("({})", parts.join(", "))
        }
        Expr::MapLiteral(entries) => {
            if entries.is_empty() {
                "HashMap::new()".to_string()
            } else {
                let flat_exprs: Vec<Expr> = entries
                    .iter()
                    .flat_map(|(k, v)| [k.clone(), v.clone()])
                    .collect();
                let flat_ctxs = compute_args_used_after_with_rc(
                    &flat_exprs,
                    &ectx.used_after,
                    &ectx.local_types,
                    &ectx.rc_wrapped,
                );
                let mut parts = Vec::new();
                for (idx, (k, v)) in entries.iter().enumerate() {
                    let key_ctx = &flat_ctxs[idx * 2];
                    let value_ctx = &flat_ctxs[idx * 2 + 1];
                    parts.push(format!(
                        "({}, {})",
                        clone_arg(k, ctx, key_ctx),
                        clone_arg(v, ctx, value_ctx)
                    ));
                }
                format!(
                    "vec![{}].into_iter().collect::<HashMap<_, _>>()",
                    parts.join(", ")
                )
            }
        }
        Expr::RecordCreate { type_name, fields } => {
            let rust_type = if type_name == "Tcp.Connection" {
                "Tcp_Connection"
            } else {
                type_name
            };
            let field_exprs: Vec<Expr> = fields.iter().map(|(_, e)| e.clone()).collect();
            let field_ctxs = compute_args_used_after_with_rc(
                &field_exprs,
                &ectx.used_after,
                &ectx.local_types,
                &ectx.rc_wrapped,
            );
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
            format!("{} {{ {} }}", rust_type, parts.join(", "))
        }
        Expr::RecordUpdate {
            type_name,
            base,
            updates,
        } => {
            let rust_type = if type_name == "Tcp.Connection" {
                "Tcp_Connection"
            } else {
                type_name
            };
            let base_str = clone_arg(base, ctx, ectx);
            let update_exprs: Vec<Expr> = updates.iter().map(|(_, e)| e.clone()).collect();
            let update_ctxs = compute_args_used_after_with_rc(
                &update_exprs,
                &ectx.used_after,
                &ectx.local_types,
                &ectx.rc_wrapped,
            );
            let parts: Vec<String> = updates
                .iter()
                .zip(update_ctxs.iter())
                .map(|((name, expr), uctx)| {
                    format!(
                        "{}: {}",
                        aver_name_to_rust(name),
                        clone_arg(expr, ctx, uctx)
                    )
                })
                .collect();
            format!("{} {{ {}, ..{} }}", rust_type, parts.join(", "), base_str)
        }
        Expr::TailCall(boxed) => {
            // TailCall outside of a TCO loop → emit as regular function call
            let (target, args) = boxed.as_ref();
            let call_ctx = RustCallCtx { ctx, ectx };
            match classify_tail_call_plan(target, "", &call_ctx) {
                TailCallPlan::SelfCall | TailCallPlan::KnownFunction(_) => {
                    emit_named_function_call(target, args, ctx, ectx)
                }
                TailCallPlan::Unknown(name) => emit_codegen_error_expr(format!(
                    "Rust codegen: unknown tail call target {}",
                    name
                )),
            }
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
        Literal::Str(s) => format!("AverStr::from({:?})", s),
        Literal::Bool(b) => if *b { "true" } else { "false" }.to_string(),
        Literal::Unit => "()".to_string(),
    }
}

fn emit_codegen_error_expr(message: String) -> String {
    let message_lit = format!("{:?}", message);
    format!(
        "{{ compile_error!({}); unreachable!(\"unreachable after compile_error\") }}",
        message_lit
    )
}

fn emit_fn_call(fn_expr: &Expr, args: &[Expr], ctx: &CodegenContext, ectx: &EmitCtx) -> String {
    let call_ctx = RustCallCtx { ctx, ectx };
    match classify_call_plan(fn_expr, &call_ctx) {
        CallPlan::Builtin(name) => builtins::emit_builtin_call(&name, args, ctx, ectx)
            .unwrap_or_else(|| {
                emit_codegen_error_expr(format!(
                    "Rust codegen: unhandled builtin call lowering for {}",
                    name
                ))
            }),
        CallPlan::Wrapper(kind) => {
            let wrapper_name = match kind {
                WrapperKind::ResultOk => "Result.Ok",
                WrapperKind::ResultErr => "Result.Err",
                WrapperKind::OptionSome => "Option.Some",
            };
            builtins::emit_builtin_call(wrapper_name, args, ctx, ectx).unwrap_or_else(|| {
                emit_codegen_error_expr(format!(
                    "Rust codegen: missing wrapper lowering for {}",
                    wrapper_name
                ))
            })
        }
        CallPlan::NoneValue => {
            if args.is_empty() {
                "None".to_string()
            } else {
                emit_codegen_error_expr(
                    "Rust codegen: Option.None cannot be called with arguments".to_string(),
                )
            }
        }
        CallPlan::TypeConstructor {
            qualified_type_name,
            variant_name,
        } => emit_type_constructor_call(&qualified_type_name, &variant_name, args, ctx, ectx),
        CallPlan::Function(name) => emit_named_function_call(&name, args, ctx, ectx),
        CallPlan::Dynamic => {
            let func = emit_expr(fn_expr, ctx, ectx);
            let arg_ctxs = compute_args_used_after_with_rc(
                args,
                &ectx.used_after,
                &ectx.local_types,
                &ectx.rc_wrapped,
            );
            let arg_strs: Vec<String> = args
                .iter()
                .zip(arg_ctxs.iter())
                .map(|(a, ac)| clone_arg(a, ctx, ac))
                .collect();
            format!("{}({})", func, arg_strs.join(", "))
        }
    }
}

fn emit_leaf_op(leaf: LeafOp<'_>, ctx: &CodegenContext, ectx: &EmitCtx) -> String {
    match leaf {
        LeafOp::FieldAccess { object, field_name } => {
            let object = emit_expr(object, ctx, ectx);
            format!("{}.{}.clone()", object, aver_name_to_rust(field_name))
        }
        LeafOp::MapGet { map, key } => emit_leaf_builtin_call("Map.get", &[map, key], ctx, ectx),
        LeafOp::MapSet { map, key, value } => {
            emit_leaf_builtin_call("Map.set", &[map, key, value], ctx, ectx)
        }
        LeafOp::VectorNew { size, fill } => {
            emit_leaf_builtin_call("Vector.new", &[size, fill], ctx, ectx)
        }
        LeafOp::VectorSetOrDefaultSameVector {
            vector,
            index,
            value,
        } => {
            let inner_args = vec![vector.clone(), index.clone(), value.clone()];
            let arg_ctxs = compute_args_used_after_with_rc(
                &inner_args,
                &ectx.used_after,
                &ectx.local_types,
                &ectx.rc_wrapped,
            );
            let vector = clone_arg(vector, ctx, &arg_ctxs[0]);
            let index = emit_expr(index, ctx, &arg_ctxs[1]);
            let value = clone_arg(value, ctx, &arg_ctxs[2]);
            format!(
                "{{ let __vec = {}; let __idx = {} as usize; if __idx < __vec.len() {{ __vec.set_unchecked(__idx, {}) }} else {{ __vec }} }}",
                vector, index, value
            )
        }
        LeafOp::VectorGetOrDefaultLiteral {
            vector,
            index,
            default_literal,
        } => {
            let inner_args = vec![vector.clone(), index.clone()];
            let arg_ctxs = compute_args_used_after_with_rc(
                &inner_args,
                &ectx.used_after,
                &ectx.local_types,
                &ectx.rc_wrapped,
            );
            let vector = emit_expr(vector, ctx, &arg_ctxs[0]);
            let index = emit_expr(index, ctx, &arg_ctxs[1]);
            let default = emit_literal(default_literal);
            format!(
                "{}.get({} as usize).cloned().unwrap_or({})",
                vector, index, default
            )
        }
    }
}

fn emit_leaf_builtin_call(
    name: &str,
    args: &[&Expr],
    ctx: &CodegenContext,
    ectx: &EmitCtx,
) -> String {
    let owned_args = args.iter().map(|expr| (*expr).clone()).collect::<Vec<_>>();
    builtins::emit_builtin_call(name, &owned_args, ctx, ectx).unwrap_or_else(|| {
        emit_codegen_error_expr(format!(
            "Rust codegen: missing leaf builtin lowering for {}",
            name
        ))
    })
}

fn emit_named_function_call(
    name: &str,
    args: &[Expr],
    ctx: &CodegenContext,
    ectx: &EmitCtx,
) -> String {
    let arg_ctxs = compute_args_used_after_with_rc(
        args,
        &ectx.used_after,
        &ectx.local_types,
        &ectx.rc_wrapped,
    );
    let arg_strs: Vec<String> = args
        .iter()
        .zip(arg_ctxs.iter())
        .map(|(a, ac)| clone_arg(a, ctx, ac))
        .collect();

    if let Some((prefix, bare)) = resolve_module_call(name, ctx) {
        let module_path = module_prefix_to_rust_path(prefix);
        format!(
            "{}::{}({})",
            module_path,
            aver_name_to_rust(bare),
            arg_strs.join(", ")
        )
    } else {
        format!("{}({})", aver_name_to_rust(name), arg_strs.join(", "))
    }
}

fn emit_type_constructor_call(
    qualified_type_name: &str,
    variant_name: &str,
    args: &[Expr],
    ctx: &CodegenContext,
    ectx: &EmitCtx,
) -> String {
    let arg_ctxs = compute_args_used_after_with_rc(
        args,
        &ectx.used_after,
        &ectx.local_types,
        &ectx.rc_wrapped,
    );
    let ctor_name = format!("{}.{}", qualified_type_name, variant_name);
    let boxed_positions = constructor_boxed_positions(&ctor_name, ctx);
    let arg_strs: Vec<String> = args
        .iter()
        .enumerate()
        .zip(arg_ctxs.iter())
        .map(|((idx, a), ac)| {
            let arg = clone_arg(a, ctx, ac);
            if boxed_positions.contains(&idx) {
                format!("std::rc::Rc::new({})", arg)
            } else {
                arg
            }
        })
        .collect();

    if let Some((prefix, bare_type_name)) = resolve_module_call(qualified_type_name, ctx) {
        let module_path = module_prefix_to_rust_path(prefix);
        format!(
            "{}::{}::{}({})",
            module_path,
            bare_type_name,
            variant_name,
            arg_strs.join(", ")
        )
    } else {
        format!(
            "{}::{}({})",
            qualified_type_name,
            variant_name,
            arg_strs.join(", ")
        )
    }
}

/// Clone a value if it's a variable reference (to avoid move issues in generated Rust).
/// Literals and complex expressions don't need cloning.
pub(super) fn maybe_clone(code: String, expr: &Expr, ectx: &EmitCtx) -> String {
    match expr {
        Expr::Ident(name) => {
            if ectx.skip_clone(name) {
                code
            } else if ectx.is_rc_wrapped(name) {
                // Rc-wrapped pass-through param: deref then clone to get the inner T.
                // `(*param).clone()` produces T, not Rc<T>.
                format!("(*{}).clone()", code)
            } else {
                format!("{}.clone()", code)
            }
        }
        // `emit_expr` already encodes this as a compile_error! expression.
        Expr::Resolved(_) => code,
        // emit_expr already adds .clone() for non-builtin field access (line 56),
        // and returns bare paths for user-type constructors / module refs.
        // Adding another .clone() here would produce obj.field.clone().clone().
        Expr::Attr(_, _) => code,
        _ => code,
    }
}

/// Emit an expression as a function argument, cloning variables to prevent move errors.
pub(super) fn clone_arg(expr: &Expr, ctx: &CodegenContext, ectx: &EmitCtx) -> String {
    let code = emit_expr(expr, ctx, ectx);
    maybe_clone(code, expr, ectx)
}

enum DisplayKind {
    /// Rust's Display trait matches aver_display (String, i64, f64, bool)
    Native,
}

/// Check if an expression's type can use Rust's native Display instead of aver_display.
fn expr_display_type(expr: &Expr, ectx: &EmitCtx) -> Option<DisplayKind> {
    match expr {
        Expr::Literal(Literal::Int(_) | Literal::Float(_) | Literal::Bool(_) | Literal::Str(_)) => {
            Some(DisplayKind::Native)
        }
        Expr::Ident(name) => {
            let ty = ectx.local_types.get(name)?;
            if matches!(ty, Type::Str | Type::Int | Type::Float | Type::Bool) {
                Some(DisplayKind::Native)
            } else {
                None
            }
        }
        Expr::BinOp(op, _, _) => {
            // Comparison ops return bool, arithmetic returns the same type (unknown here)
            if matches!(
                op,
                BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte
            ) {
                Some(DisplayKind::Native)
            } else {
                None
            }
        }
        _ => None,
    }
}

fn emit_constructor(
    name: &str,
    arg: &Option<Box<Expr>>,
    ctx: &CodegenContext,
    ectx: &EmitCtx,
) -> String {
    let normalized_name = match name {
        "Ok" => "Result.Ok",
        "Err" => "Result.Err",
        "Some" => "Option.Some",
        "None" => "Option.None",
        other => other,
    };
    let args: &[Expr] = match arg {
        Some(inner) => std::slice::from_ref(inner.as_ref()),
        None => &[],
    };
    let lower_ctx = RustCallCtx { ctx, ectx };

    match classify_constructor_name(normalized_name, &lower_ctx) {
        SemanticConstructor::Wrapper(kind) => {
            let wrapper_name = match kind {
                WrapperKind::ResultOk => "Result.Ok",
                WrapperKind::ResultErr => "Result.Err",
                WrapperKind::OptionSome => "Option.Some",
            };
            builtins::emit_builtin_call(wrapper_name, args, ctx, ectx).unwrap_or_else(|| {
                emit_codegen_error_expr(format!(
                    "Rust codegen: missing constructor wrapper lowering for {}",
                    wrapper_name
                ))
            })
        }
        SemanticConstructor::NoneValue => "None".to_string(),
        SemanticConstructor::TypeConstructor {
            qualified_type_name,
            variant_name,
        } => emit_type_constructor_call(&qualified_type_name, &variant_name, args, ctx, ectx),
        SemanticConstructor::Unknown(_) => {
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
        return "AverStr::from(\"\")".to_string();
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
                let emitted = emit_expr(expr, ctx, ectx);
                // Skip aver_display when the expression type is known to be displayable natively.
                // String, Int (i64), Float (f64), Bool all have Display impls matching aver_display.
                let arg = match expr_display_type(expr, ectx) {
                    Some(DisplayKind::Native) => emitted,
                    _ => format!("aver_rt::aver_display(&{})", emitted),
                };
                fmt_args.push(arg);
            }
        }
    }

    if fmt_args.is_empty() {
        format!("AverStr::from({:?})", fmt_str)
    } else {
        format!(
            "AverStr::from(format!({:?}, {}))",
            fmt_str,
            fmt_args.join(", ")
        )
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
    let subj = clone_arg(subject, ctx, &subj_ectx);
    let optimized = use_optimized_emission(ctx);
    let dispatch_plan = if optimized {
        classify_dispatch_plan_for_rust(arms, ctx, ectx)
    } else {
        None
    };

    // Bool match → if/else: match expr { true => A, false => B } → if expr { A } else { B }
    if optimized
        && let Some(MatchDispatchPlan::Bool(shape)) = dispatch_plan.as_ref()
        && let Some(code) =
            try_emit_bool_if_else(subject, &subj, arms, *shape, ctx, &subj_ectx, ectx)
    {
        return code;
    }

    // Determine if subject needs special treatment
    let needs_as_str = subject_might_be_string(subject, ctx);
    let _needs_as_slice = subject_might_be_list(subject, arms, ctx);

    if has_list_patterns(arms) {
        let list_shape = match dispatch_plan.as_ref() {
            Some(MatchDispatchPlan::List(shape)) => Some(*shape),
            _ => None,
        };
        return emit_list_match(subj, arms, list_shape, optimized, ctx, |arm| {
            emit_expr(&arm.body, ctx, ectx)
        });
    }

    if optimized && let Some(MatchDispatchPlan::Table(shape)) = dispatch_plan.as_ref() {
        return emit_dispatch_table_match(subj, arms, shape, |arm| {
            maybe_clone(emit_expr(&arm.body, ctx, ectx), &arm.body, ectx)
        });
    }

    let match_expr = if needs_as_str && has_string_literal_patterns(arms) {
        format!("&*{}", subj)
    } else {
        subj
    };

    let mut arm_strs = Vec::new();
    for arm in arms {
        let pat = emit_pattern(&arm.pattern, needs_as_str, ctx);
        // Each arm body is independent — use parent's used_after
        // Clone the result if it's a bare ident that's still needed after the match
        let body = maybe_clone(emit_expr(&arm.body, ctx, ectx), &arm.body, ectx);
        let rebindings = emit_pattern_rebindings(&arm.pattern, ctx);
        arm_strs.push(format!(
            "        {} => {{\n            {}{}\n        }}",
            pat, rebindings, body
        ));
    }

    format!("match {} {{\n{}\n    }}", match_expr, arm_strs.join(",\n"))
}

/// If match has exactly two arms with `true` and `false` bool literal patterns,
/// emit `if subject { true_body } else { false_body }` instead of a match.
fn try_emit_bool_if_else(
    subject: &Expr,
    subj: &str,
    arms: &[MatchArm],
    shape: crate::ir::BoolMatchShape,
    ctx: &CodegenContext,
    subj_ectx: &EmitCtx,
    ectx: &EmitCtx,
) -> Option<String> {
    let (true_body, false_body, cond) = match classify_bool_subject_plan(subject) {
        BoolSubjectPlan::Expr(_) => (
            &arms[shape.true_arm_index].body,
            &arms[shape.false_arm_index].body,
            subj.to_string(),
        ),
        BoolSubjectPlan::Compare {
            lhs,
            rhs,
            op,
            invert,
        } => {
            let rhs_vars = collect_vars(rhs);
            let lhs_ectx = subj_ectx.with_used_after(&rhs_vars);
            let lhs_code = emit_expr(lhs, ctx, &lhs_ectx);
            let rhs_code = emit_expr(rhs, ctx, subj_ectx);
            let op_code = match op {
                BoolCompareOp::Eq => "==",
                BoolCompareOp::Lt => "<",
                BoolCompareOp::Gt => ">",
            };
            let cond = format!("({lhs_code} {op_code} {rhs_code})");
            if invert {
                (
                    &arms[shape.false_arm_index].body,
                    &arms[shape.true_arm_index].body,
                    cond,
                )
            } else {
                (
                    &arms[shape.true_arm_index].body,
                    &arms[shape.false_arm_index].body,
                    cond,
                )
            }
        }
    };
    let t = maybe_clone(emit_expr(true_body, ctx, ectx), true_body, ectx);
    let f = maybe_clone(emit_expr(false_body, ctx, ectx), false_body, ectx);
    Some(format!("if {} {{ {} }} else {{ {} }}", cond, t, f))
}

fn subject_might_be_string(_subject: &Expr, _ctx: &CodegenContext) -> bool {
    // Heuristic: if subject is an ident, we can't tell at codegen time
    // We'll rely on the patterns to decide
    true
}

fn subject_might_be_list(_subject: &Expr, _arms: &[MatchArm], _ctx: &CodegenContext) -> bool {
    true
}

pub(super) fn emit_dispatch_table_match<F>(
    subject: String,
    arms: &[MatchArm],
    shape: &DispatchTableShape,
    body_for_arm: F,
) -> String
where
    F: Fn(&MatchArm) -> String,
{
    let subject_name = "__dispatch_subject";
    let fallback = match &shape.default_arm {
        Some(default_arm) => emit_default_dispatch_arm(
            subject_name,
            &arms[default_arm.arm_index],
            default_arm,
            &body_for_arm,
        ),
        None => "panic!(\"Aver Rust codegen: non-exhaustive dispatch match\")".to_string(),
    };

    let body = shape
        .entries
        .iter()
        .rev()
        .fold(fallback, |else_branch, entry| {
            let arm = &arms[entry.arm_index];
            let cond = emit_dispatch_condition(subject_name, &entry.pattern);
            let body = emit_dispatch_arm_body(subject_name, arm, entry, &body_for_arm);
            format!("if {} {{ {} }} else {{ {} }}", cond, body, else_branch)
        });

    format!("{{ let {} = {}; {} }}", subject_name, subject, body)
}

fn emit_dispatch_condition(subject_name: &str, pattern: &SemanticDispatchPattern) -> String {
    match pattern {
        SemanticDispatchPattern::Literal(lit) => match lit {
            DispatchLiteral::Int(i) => format!("{subject_name} == {i}i64"),
            DispatchLiteral::Float(f) => format!("{subject_name} == {f}f64"),
            DispatchLiteral::Bool(b) => format!("{subject_name} == {b}"),
            DispatchLiteral::Str(s) => format!("&*{subject_name} == {:?}", s),
            DispatchLiteral::Unit => format!("{subject_name} == ()"),
        },
        SemanticDispatchPattern::EmptyList => format!("{subject_name}.is_empty()"),
        SemanticDispatchPattern::NoneValue => format!("{subject_name}.is_none()"),
        SemanticDispatchPattern::WrapperTag(kind) => match kind {
            WrapperKind::ResultOk => format!("{subject_name}.is_ok()"),
            WrapperKind::ResultErr => format!("{subject_name}.is_err()"),
            WrapperKind::OptionSome => format!("{subject_name}.is_some()"),
        },
    }
}

fn emit_dispatch_arm_body(
    subject_name: &str,
    arm: &MatchArm,
    entry: &DispatchArmPlan,
    body_for_arm: &impl Fn(&MatchArm) -> String,
) -> String {
    let body = body_for_arm(arm);
    match (&entry.pattern, &entry.binding) {
        (
            SemanticDispatchPattern::WrapperTag(kind),
            DispatchBindingPlan::WrapperPayload(binding_name),
        ) => {
            let binding = aver_name_to_rust(binding_name);
            let extractor = match kind {
                WrapperKind::ResultOk => "Ok",
                WrapperKind::ResultErr => "Err",
                WrapperKind::OptionSome => "Some",
            };
            format!(
                "{{ let {binding} = if let {extractor}({binding}) = &{subject_name} {{ {binding}.clone() }} else {{ unreachable!(\"Aver Rust codegen: dispatch tag mismatch\") }}; {body} }}"
            )
        }
        _ => body,
    }
}

fn emit_default_dispatch_arm(
    subject_name: &str,
    arm: &MatchArm,
    default_arm: &DispatchDefaultPlan,
    body_for_arm: &impl Fn(&MatchArm) -> String,
) -> String {
    let body = body_for_arm(arm);
    match &default_arm.binding_name {
        None => body,
        Some(name) => {
            let name = aver_name_to_rust(name);
            format!("{{ let {} = {}.clone(); {} }}", name, subject_name, body)
        }
    }
}

pub(super) fn emit_list_match<F>(
    subject: String,
    arms: &[MatchArm],
    list_shape: Option<ListMatchShape>,
    allow_fast_macro: bool,
    ctx: &CodegenContext,
    body_for_arm: F,
) -> String
where
    F: Fn(&MatchArm) -> String,
{
    // Fast path: exactly [] and [h, ..t] → use aver_list_match! macro
    if allow_fast_macro
        && let Some(code) =
            try_emit_list_match_macro(&subject, arms, list_shape, ctx, &body_for_arm)
    {
        return code;
    }
    let subject_name = "__list_subject";
    let arms_code = emit_list_match_arms(subject_name, arms, ctx, &body_for_arm);
    format!("{{ let {} = {}; {} }}", subject_name, subject, arms_code)
}

/// Emit the aver_list_match! macro for the common []/[h,..t] two-arm pattern.
fn try_emit_list_match_macro<F>(
    subject: &str,
    arms: &[MatchArm],
    list_shape: Option<ListMatchShape>,
    ctx: &CodegenContext,
    body_for_arm: &F,
) -> Option<String>
where
    F: Fn(&MatchArm) -> String,
{
    let shape = match list_shape {
        Some(shape) => shape,
        None => classify_list_match_shape(arms)?,
    };
    let empty_arm = &arms[shape.empty_arm_index];
    let cons_arm = &arms[shape.cons_arm_index];
    let Pattern::Cons(head, tail) = &cons_arm.pattern else {
        return None;
    };
    // Both names must be real idents (not _) for the macro binding
    if head == "_" || tail == "_" {
        return None;
    }
    let head_name = aver_name_to_rust(head);
    let tail_name = aver_name_to_rust(tail);
    let empty_body = emit_list_arm_body(empty_arm, ctx, body_for_arm(empty_arm));
    let cons_body = emit_list_arm_body(cons_arm, ctx, body_for_arm(cons_arm));
    Some(format!(
        "aver_list_match!({}, [] => {{ {} }}, [{}, {}] => {{ {} }})",
        subject, empty_body, head_name, tail_name, cons_body
    ))
}

fn emit_list_match_arms<F>(
    subject_name: &str,
    arms: &[MatchArm],
    ctx: &CodegenContext,
    body_for_arm: &F,
) -> String
where
    F: Fn(&MatchArm) -> String,
{
    let Some((first, rest)) = arms.split_first() else {
        return "panic!(\"Aver Rust codegen: empty list match\")".to_string();
    };

    let body = emit_list_arm_body(first, ctx, body_for_arm(first));
    let fallback = if rest.is_empty() {
        "panic!(\"Aver Rust codegen: non-exhaustive list match\")".to_string()
    } else {
        emit_list_match_arms(subject_name, rest, ctx, body_for_arm)
    };

    match &first.pattern {
        Pattern::EmptyList => format!(
            "if {}.is_empty() {{ {} }} else {{ {} }}",
            subject_name, body, fallback
        ),
        Pattern::Cons(head, tail) => {
            let head_pat = if head == "_" {
                "_".to_string()
            } else {
                aver_name_to_rust(head)
            };
            let tail_pat = if tail == "_" {
                "_".to_string()
            } else {
                aver_name_to_rust(tail)
            };
            format!(
                "if let Some(({}, {})) = aver_rt::list_uncons_cloned(&{}) {{ {} }} else {{ {} }}",
                head_pat, tail_pat, subject_name, body, fallback
            )
        }
        Pattern::Wildcard => body,
        Pattern::Ident(name) => {
            let name = aver_name_to_rust(name);
            format!("{{ let {} = {}.clone(); {} }}", name, subject_name, body)
        }
        other => {
            let pat = emit_pattern(other, false, ctx);
            format!(
                "match &{} {{ {} => {{ {} }}, _ => {{ {} }} }}",
                subject_name, pat, body, fallback
            )
        }
    }
}

fn emit_list_arm_body(arm: &MatchArm, ctx: &CodegenContext, body: String) -> String {
    let rebindings = emit_pattern_rebindings(&arm.pattern, ctx);
    if rebindings.is_empty() {
        body
    } else {
        format!("{{ {}{} }}", rebindings, body)
    }
}

pub(super) fn constructor_boxed_positions(name: &str, ctx: &CodegenContext) -> HashSet<usize> {
    let mut out = HashSet::new();
    let Some((params, ret, _)) = ctx.fn_sigs.get(name) else {
        return out;
    };
    let Type::Named(ret_name) = ret else {
        return out;
    };
    for (idx, param) in params.iter().enumerate() {
        if let Type::Named(param_name) = param
            && param_name == ret_name
        {
            out.insert(idx);
        }
    }
    out
}

pub(super) fn constructor_boxed_bindings(
    name: &str,
    bindings: &[String],
    ctx: &CodegenContext,
) -> Vec<String> {
    let mut sig_name = None;
    if ctx.fn_sigs.contains_key(name) {
        sig_name = Some(name.to_string());
    } else if !name.contains('.') {
        let suffix = format!(".{}", name);
        let mut matches = ctx
            .fn_sigs
            .keys()
            .filter(|k| k.ends_with(&suffix))
            .cloned()
            .collect::<Vec<_>>();
        matches.sort();
        if matches.len() == 1 {
            sig_name = matches.into_iter().next();
        }
    }
    let Some(sig_name) = sig_name else {
        return Vec::new();
    };
    let boxed = constructor_boxed_positions(&sig_name, ctx);
    bindings
        .iter()
        .enumerate()
        .filter_map(|(idx, b)| {
            if b != "_" && boxed.contains(&idx) {
                Some(b.clone())
            } else {
                None
            }
        })
        .collect()
}

fn emit_pattern_rebindings(pattern: &Pattern, ctx: &CodegenContext) -> String {
    let mut lines = Vec::new();
    if let Pattern::Constructor(name, bindings) = pattern {
        // Ok/Err/Some bindings are now moved (not ref), so no clone needed.
        // Only Box-wrapped fields (recursive types) need deref.
        for b in constructor_boxed_bindings(name, bindings, ctx) {
            let b = aver_name_to_rust(&b);
            lines.push(format!("let {} = (*{}).clone();", b, b));
        }
    }
    if lines.is_empty() {
        String::new()
    } else {
        format!("{}\n            ", lines.join("\n            "))
    }
}

use super::builtins;
use super::liveness::{EmitCtx, collect_vars, compute_args_used_after_full, should_borrow_param};
use super::pattern::emit_pattern;
use crate::ast::*;
use crate::codegen::CodegenContext;
use crate::codegen::common::{
    expr_to_dotted_name, is_user_type, module_prefix_to_rust_path, resolve_module_call,
};
use crate::ir::{
    BodyExprPlan, BodyPlan, BoolCompareOp, BoolSubjectPlan, CallLowerCtx, CallPlan,
    DispatchArmPlan, DispatchBindingPlan, DispatchDefaultPlan, DispatchLiteral, DispatchTableShape,
    ForwardArg, ForwardCallPlan, LeafOp, ListMatchShape, MatchDispatchPlan, SemanticConstructor,
    SemanticDispatchPattern, TailCallPlan, ThinBodyCtx, ThinBodyPlan, ThinKind, WrapperKind,
    classify_body_expr_plan, classify_body_plan, classify_bool_subject_plan, classify_call_plan,
    classify_constructor_name, classify_leaf_op, classify_list_match_shape,
    classify_match_dispatch_plan, classify_tail_call_plan, classify_thin_fn_def,
    is_builtin_namespace,
};
use crate::types::{self, Type};
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

impl ThinBodyCtx for RustCallCtx<'_, '_> {
    fn find_fn_def<'a>(&'a self, name: &str) -> Option<&'a FnDef> {
        if let Some((prefix, bare)) = resolve_module_call(name, self.ctx) {
            return self
                .ctx
                .modules
                .iter()
                .find(|module| module.prefix == prefix)
                .and_then(|module| module.fn_defs.iter().find(|fd| fd.name == bare));
        }

        self.ctx.fn_defs.iter().find(|fd| fd.name == name)
    }
}

pub(super) fn classify_dispatch_plan_for_rust(
    arms: &[MatchArm],
    ctx: &CodegenContext,
    ectx: &EmitCtx,
) -> Option<MatchDispatchPlan> {
    let lower_ctx = RustCallCtx { ctx, ectx };
    classify_match_dispatch_plan(arms, &lower_ctx)
}

pub(super) fn classify_body_plan_for_rust<'a>(
    body: &'a FnBody,
    ctx: &CodegenContext,
    ectx: &EmitCtx,
) -> Option<BodyPlan<'a>> {
    let lower_ctx = RustCallCtx { ctx, ectx };
    classify_body_plan(body, &lower_ctx)
}

pub(super) fn classify_body_expr_plan_for_rust<'a>(
    expr: &'a Expr,
    ctx: &CodegenContext,
    ectx: &EmitCtx,
) -> BodyExprPlan<'a> {
    let lower_ctx = RustCallCtx { ctx, ectx };
    classify_body_expr_plan(expr, &lower_ctx)
}

pub(super) fn classify_thin_fn_def_for_rust<'a>(
    fd: &'a FnDef,
    ctx: &'a CodegenContext,
    ectx: &'a EmitCtx,
) -> Option<ThinBodyPlan<'a>> {
    let lower_ctx = RustCallCtx { ctx, ectx };
    classify_thin_fn_def(fd, &lower_ctx)
}

pub(super) fn thin_body_plan_is_parent_thin_candidate(plan: &ThinBodyPlan<'_>) -> bool {
    matches!(
        plan.kind,
        ThinKind::Leaf | ThinKind::Direct | ThinKind::Forward | ThinKind::Dispatch
    )
}

/// Emit a Rust expression from an Aver Expr.
pub fn emit_expr(expr: &Expr, ctx: &CodegenContext, ectx: &EmitCtx) -> String {
    emit_expr_with_options(expr, ctx, ectx, true)
}

fn emit_expr_with_options(
    expr: &Expr,
    ctx: &CodegenContext,
    ectx: &EmitCtx,
    allow_callsite_inlining: bool,
) -> String {
    let lower_ctx = RustCallCtx { ctx, ectx };
    if let Some(leaf) = classify_leaf_op(expr, &lower_ctx) {
        return emit_leaf_op_with_options(leaf, ctx, ectx, allow_callsite_inlining);
    }

    match expr {
        Expr::Literal(lit) => emit_literal(lit),
        Expr::Ident(name) => aver_name_to_rust(name),
        Expr::Resolved(slot) => emit_codegen_error_expr(format!(
            "Rust codegen: encountered resolver-only Expr::Resolved({slot}). \
             Compile pipeline should emit source-level AST (Ident), not slot-indexed AST."
        )),
        Expr::Attr(obj, field) => {
            if let Expr::Ident(type_name) = &obj.node {
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
            let obj_str = emit_expr(&obj.node, ctx, ectx);
            format!("{}.{}", obj_str, aver_name_to_rust(field))
        }
        Expr::FnCall(fn_expr, args) => {
            let bare_args: Vec<Expr> = args.iter().map(|a| a.node.clone()).collect();
            emit_fn_call_with_options(
                &fn_expr.node,
                &bare_args,
                ctx,
                ectx,
                allow_callsite_inlining,
            )
        }
        Expr::BinOp(op, left, right) => {
            // Unary minus: `- expr` is parsed as `BinOp(Sub, Literal(Int(0)), expr)`.
            // Emit as `-expr` instead of `(0i64 - expr)` to avoid type mismatch
            // when the operand is Float.
            if matches!(op, BinOp::Sub) && matches!(&left.node, Expr::Literal(Literal::Int(0))) {
                let r = emit_expr(&right.node, ctx, ectx);
                return format!("(-{})", r);
            }
            // BinOp: left's used_after includes vars from right
            let right_vars = collect_vars(&right.node);
            let left_ectx = ectx.with_used_after(&right_vars);
            let l = emit_expr(&left.node, ctx, &left_ectx);
            let r = emit_expr(&right.node, ctx, ectx);
            match op {
                BinOp::Add => {
                    if expr_is_numeric(&left.node, &left_ectx) || expr_is_numeric(&right.node, ectx)
                    {
                        // Both sides are numeric (Int/Float) — plain value add.
                        format!("({} + {})", l, r)
                    } else {
                        // Might be AverStr concatenation: Add<&AverStr>.
                        let l = maybe_clone(l, &left.node, &left_ectx);
                        format!("({} + &{})", l, r)
                    }
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
                        if let Expr::Literal(Literal::Str(s)) = &right.node {
                            return format!("(&*{} {} {:?})", l, op_str, s);
                        }
                        if let Expr::Literal(Literal::Str(s)) = &left.node {
                            return format!("({:?} {} &*{})", s, op_str, r);
                        }
                    }
                    format!("({} {} {})", l, op_str, r)
                }
            }
        }
        Expr::Match { subject, arms, .. } => emit_match(&subject.node, arms, ctx, ectx),
        Expr::Constructor(name, arg) => {
            let bare_arg = arg.as_ref().map(|a| Box::new(a.node.clone()));
            emit_constructor(name, &bare_arg, ctx, ectx)
        }
        Expr::ErrorProp(inner) => {
            let inner_str = emit_expr(&inner.node, ctx, ectx);
            format!("{}?", inner_str)
        }
        Expr::InterpolatedStr(parts) => emit_interpolated_str(parts, ctx, ectx),
        Expr::List(elements) => {
            if elements.is_empty() {
                "aver_rt::AverList::empty()".to_string()
            } else {
                let bare_elems: Vec<Expr> = elements.iter().map(|e| e.node.clone()).collect();
                let elem_ctxs = compute_args_used_after_full(
                    &bare_elems,
                    &ectx.used_after,
                    &ectx.local_types,
                    &ectx.rc_wrapped,
                    &ectx.borrowed_params,
                );
                let parts: Vec<String> = bare_elems
                    .iter()
                    .zip(elem_ctxs.iter())
                    .map(|(e, elem_ctx)| clone_arg(e, ctx, elem_ctx))
                    .collect();
                format!("aver_rt::AverList::from_vec(vec![{}])", parts.join(", "))
            }
        }
        Expr::Tuple(items) => {
            let bare_items: Vec<Expr> = items.iter().map(|e| e.node.clone()).collect();
            let item_ctxs = compute_args_used_after_full(
                &bare_items,
                &ectx.used_after,
                &ectx.local_types,
                &ectx.rc_wrapped,
                &ectx.borrowed_params,
            );
            let parts: Vec<String> = bare_items
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
                    .flat_map(|(k, v)| [k.node.clone(), v.node.clone()])
                    .collect();
                let flat_ctxs = compute_args_used_after_full(
                    &flat_exprs,
                    &ectx.used_after,
                    &ectx.local_types,
                    &ectx.rc_wrapped,
                    &ectx.borrowed_params,
                );
                let mut parts = Vec::new();
                for (idx, (k, v)) in entries.iter().enumerate() {
                    let key_ctx = &flat_ctxs[idx * 2];
                    let value_ctx = &flat_ctxs[idx * 2 + 1];
                    parts.push(format!(
                        "({}, {})",
                        clone_arg(&k.node, ctx, key_ctx),
                        clone_arg(&v.node, ctx, value_ctx)
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
            let field_exprs: Vec<Expr> = fields.iter().map(|(_, e)| e.node.clone()).collect();
            let field_ctxs = compute_args_used_after_full(
                &field_exprs,
                &ectx.used_after,
                &ectx.local_types,
                &ectx.rc_wrapped,
                &ectx.borrowed_params,
            );
            let parts: Vec<String> = fields
                .iter()
                .enumerate()
                .map(|(i, (name, expr))| {
                    format!(
                        "{}: {}",
                        aver_name_to_rust(name),
                        clone_arg(&expr.node, ctx, &field_ctxs[i])
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
            let base_str = clone_arg(&base.node, ctx, ectx);
            let update_exprs: Vec<Expr> = updates.iter().map(|(_, e)| e.node.clone()).collect();
            let update_ctxs = compute_args_used_after_full(
                &update_exprs,
                &ectx.used_after,
                &ectx.local_types,
                &ectx.rc_wrapped,
                &ectx.borrowed_params,
            );
            let parts: Vec<String> = updates
                .iter()
                .zip(update_ctxs.iter())
                .map(|((name, expr), uctx)| {
                    format!(
                        "{}: {}",
                        aver_name_to_rust(name),
                        clone_arg(&expr.node, ctx, uctx)
                    )
                })
                .collect();
            format!("{} {{ {}, ..{} }}", rust_type, parts.join(", "), base_str)
        }
        Expr::TailCall(boxed) => {
            // TailCall outside of a TCO loop → emit as regular function call
            let (target, args) = boxed.as_ref();
            let bare_args: Vec<Expr> = args.iter().map(|a| a.node.clone()).collect();
            let call_ctx = RustCallCtx { ctx, ectx };
            match classify_tail_call_plan(target, "", &call_ctx) {
                TailCallPlan::SelfCall | TailCallPlan::KnownFunction(_) => {
                    emit_named_function_call(target, &bare_args, ctx, ectx)
                }
                TailCallPlan::Unknown(name) => emit_codegen_error_expr(format!(
                    "Rust codegen: unknown tail call target {}",
                    name
                )),
            }
        }
    }
}

pub(super) fn emit_body_plan_for_rust(
    plan: &BodyPlan<'_>,
    ctx: &CodegenContext,
    ectx: &EmitCtx,
) -> String {
    emit_body_plan_for_rust_with_options(plan, ctx, ectx, true)
}

fn emit_body_plan_for_rust_with_options(
    plan: &BodyPlan<'_>,
    ctx: &CodegenContext,
    ectx: &EmitCtx,
    allow_callsite_inlining: bool,
) -> String {
    match plan {
        BodyPlan::SingleExpr(body_expr) => {
            emit_body_expr_plan_with_options(body_expr, ctx, ectx, allow_callsite_inlining)
        }
        BodyPlan::Block {
            stmts,
            bindings,
            tail,
        } => {
            let stmt_ctxs = super::liveness::compute_block_used_after_full(
                stmts,
                &ectx.used_after,
                &ectx.local_types,
                &ectx.rc_wrapped,
                &ectx.borrowed_params,
            );
            let mut lines = Vec::with_capacity(stmts.len());
            for (binding, sctx) in bindings.iter().zip(stmt_ctxs.iter()) {
                lines.push(format!(
                    "let {} = {};",
                    aver_name_to_rust(binding.name),
                    emit_body_expr_plan_with_options(
                        &binding.expr,
                        ctx,
                        sctx,
                        allow_callsite_inlining,
                    )
                ));
            }
            let tail_ctx = stmt_ctxs.last().unwrap_or(ectx);
            lines.push(emit_body_expr_plan_with_options(
                tail,
                ctx,
                tail_ctx,
                allow_callsite_inlining,
            ));
            lines.join("\n    ")
        }
    }
}

fn emit_body_expr_plan_with_options(
    plan: &BodyExprPlan<'_>,
    ctx: &CodegenContext,
    ectx: &EmitCtx,
    allow_callsite_inlining: bool,
) -> String {
    match plan {
        BodyExprPlan::Expr(expr) => {
            emit_expr_with_options(expr, ctx, ectx, allow_callsite_inlining)
        }
        BodyExprPlan::Leaf(leaf) => {
            emit_leaf_op_with_options(*leaf, ctx, ectx, allow_callsite_inlining)
        }
        BodyExprPlan::Call { target, args } => {
            let bare_args: Vec<Expr> = args.iter().map(|a| a.node.clone()).collect();
            emit_call_plan_with_args_with_options(
                target,
                &bare_args,
                ctx,
                ectx,
                allow_callsite_inlining,
            )
        }
        BodyExprPlan::ForwardCall(plan) => {
            emit_forward_call_plan_with_options(plan, ctx, ectx, allow_callsite_inlining)
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

fn emit_fn_call_with_options(
    fn_expr: &Expr,
    args: &[Expr],
    ctx: &CodegenContext,
    ectx: &EmitCtx,
    allow_callsite_inlining: bool,
) -> String {
    let call_ctx = RustCallCtx { ctx, ectx };
    let plan = classify_call_plan(fn_expr, &call_ctx);
    match plan {
        CallPlan::Dynamic => {
            let func = emit_expr_with_options(fn_expr, ctx, ectx, allow_callsite_inlining);
            let arg_ctxs = compute_args_used_after_full(
                args,
                &ectx.used_after,
                &ectx.local_types,
                &ectx.rc_wrapped,
                &ectx.borrowed_params,
            );
            let arg_strs: Vec<String> = args
                .iter()
                .zip(arg_ctxs.iter())
                .map(|(a, ac)| clone_arg(a, ctx, ac))
                .collect();
            format!("{}({})", func, arg_strs.join(", "))
        }
        _ => emit_call_plan_with_args_with_options(&plan, args, ctx, ectx, allow_callsite_inlining),
    }
}

fn emit_call_plan_with_args_with_options(
    plan: &CallPlan,
    args: &[Expr],
    ctx: &CodegenContext,
    ectx: &EmitCtx,
    _allow_callsite_inlining: bool,
) -> String {
    match plan {
        CallPlan::Builtin(name) => builtins::emit_builtin_call(name, args, ctx, ectx)
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
        } => emit_type_constructor_call(qualified_type_name, variant_name, args, ctx, ectx),
        CallPlan::Function(name) => emit_named_function_call(name, args, ctx, ectx),
        CallPlan::Dynamic => emit_codegen_error_expr(
            "Rust codegen: dynamic call passed to direct call emitter".to_string(),
        ),
    }
}

fn emit_forward_call_plan_with_options(
    plan: &ForwardCallPlan,
    ctx: &CodegenContext,
    ectx: &EmitCtx,
    allow_callsite_inlining: bool,
) -> String {
    let args = plan
        .args
        .iter()
        .map(forward_arg_to_expr)
        .collect::<Option<Vec<_>>>();
    match args {
        Some(args) => emit_call_plan_with_args_with_options(
            &plan.target,
            &args,
            ctx,
            ectx,
            allow_callsite_inlining,
        ),
        None => emit_codegen_error_expr(
            "Rust codegen: unexpected slot-based ForwardCall in source-level emission".to_string(),
        ),
    }
}

fn forward_arg_to_expr(arg: &ForwardArg) -> Option<Expr> {
    match arg {
        ForwardArg::Local(name) => Some(Expr::Ident(name.clone())),
        ForwardArg::Slot(_) => None,
    }
}

fn emit_leaf_op_with_options(
    leaf: LeafOp<'_>,
    ctx: &CodegenContext,
    ectx: &EmitCtx,
    allow_callsite_inlining: bool,
) -> String {
    match leaf {
        LeafOp::FieldAccess { object, field_name } => {
            let object = emit_expr_with_options(&object.node, ctx, ectx, allow_callsite_inlining);
            format!("{}.{}", object, aver_name_to_rust(field_name))
        }
        LeafOp::MapGet { map, key } => emit_leaf_builtin_call_with_options(
            "Map.get",
            &[&map.node, &key.node],
            ctx,
            ectx,
            allow_callsite_inlining,
        ),
        LeafOp::MapSet { map, key, value } => emit_leaf_builtin_call_with_options(
            "Map.set",
            &[&map.node, &key.node, &value.node],
            ctx,
            ectx,
            allow_callsite_inlining,
        ),
        LeafOp::VectorNew { size, fill } => emit_leaf_builtin_call_with_options(
            "Vector.new",
            &[&size.node, &fill.node],
            ctx,
            ectx,
            allow_callsite_inlining,
        ),
        LeafOp::VectorSetOrDefaultSameVector {
            vector,
            index,
            value,
        } => {
            let inner_args = vec![vector.node.clone(), index.node.clone(), value.node.clone()];
            let arg_ctxs = compute_args_used_after_full(
                &inner_args,
                &ectx.used_after,
                &ectx.local_types,
                &ectx.rc_wrapped,
                &ectx.borrowed_params,
            );
            let vector =
                clone_arg_with_options(&vector.node, ctx, &arg_ctxs[0], allow_callsite_inlining);
            let index =
                emit_expr_with_options(&index.node, ctx, &arg_ctxs[1], allow_callsite_inlining);
            let value =
                clone_arg_with_options(&value.node, ctx, &arg_ctxs[2], allow_callsite_inlining);
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
            let inner_args = vec![vector.node.clone(), index.node.clone()];
            let arg_ctxs = compute_args_used_after_full(
                &inner_args,
                &ectx.used_after,
                &ectx.local_types,
                &ectx.rc_wrapped,
                &ectx.borrowed_params,
            );
            let vector =
                emit_expr_with_options(&vector.node, ctx, &arg_ctxs[0], allow_callsite_inlining);
            let index =
                emit_expr_with_options(&index.node, ctx, &arg_ctxs[1], allow_callsite_inlining);
            let default = emit_literal(default_literal);
            format!(
                "{}.get({} as usize).cloned().unwrap_or({})",
                vector, index, default
            )
        }
        LeafOp::IntModOrDefaultLiteral {
            a,
            b,
            default_literal,
        } => {
            // When divisor is a known non-zero literal, skip the zero check entirely.
            if let Expr::Literal(Literal::Int(n)) = &b.node
                && *n != 0
            {
                let inner_args = vec![a.node.clone(), b.node.clone()];
                let arg_ctxs = compute_args_used_after_full(
                    &inner_args,
                    &ectx.used_after,
                    &ectx.local_types,
                    &ectx.rc_wrapped,
                    &ectx.borrowed_params,
                );
                let a = emit_expr_with_options(&a.node, ctx, &arg_ctxs[0], allow_callsite_inlining);
                let b_str = emit_literal(&Literal::Int(*n));
                format!("({}).rem_euclid({})", a, b_str)
            } else {
                let inner_args = vec![a.node.clone(), b.node.clone()];
                let arg_ctxs = compute_args_used_after_full(
                    &inner_args,
                    &ectx.used_after,
                    &ectx.local_types,
                    &ectx.rc_wrapped,
                    &ectx.borrowed_params,
                );
                let a = emit_expr_with_options(&a.node, ctx, &arg_ctxs[0], allow_callsite_inlining);
                let b = emit_expr_with_options(&b.node, ctx, &arg_ctxs[1], allow_callsite_inlining);
                let default = emit_literal(default_literal);
                format!(
                    "{{ let __b = {}; if __b == 0i64 {{ {} }} else {{ ({}).rem_euclid(__b) }} }}",
                    b, default, a
                )
            }
        }
        LeafOp::ListIndexGet { list, index } => {
            let inner_args = vec![list.node.clone(), index.node.clone()];
            let arg_ctxs = compute_args_used_after_full(
                &inner_args,
                &ectx.used_after,
                &ectx.local_types,
                &ectx.rc_wrapped,
                &ectx.borrowed_params,
            );
            let list =
                emit_expr_with_options(&list.node, ctx, &arg_ctxs[0], allow_callsite_inlining);
            let index =
                emit_expr_with_options(&index.node, ctx, &arg_ctxs[1], allow_callsite_inlining);
            format!("{}.to_vec().get({} as usize).cloned()", list, index)
        }
    }
}

fn emit_leaf_builtin_call_with_options(
    name: &str,
    args: &[&Expr],
    ctx: &CodegenContext,
    ectx: &EmitCtx,
    _allow_callsite_inlining: bool,
) -> String {
    let owned_args = args.iter().map(|expr| (*expr).clone()).collect::<Vec<_>>();
    builtins::emit_builtin_call(name, &owned_args, ctx, ectx).unwrap_or_else(|| {
        emit_codegen_error_expr(format!(
            "Rust codegen: missing leaf builtin lowering for {}",
            name
        ))
    })
}

/// Check if a FnDef has self-tailcall (TCO) in its body.
/// Returns true only if the function has self-recursive tail calls and NO mutual tail calls.
/// Mutual-TCO functions get wrapper functions with `&T` params, so they should use borrow.
#[allow(dead_code)]
fn fn_def_has_only_self_tco(fd: &FnDef) -> bool {
    let has_self = fn_def_has_tco(fd);
    if !has_self {
        return false;
    }
    // Check if there are tail calls to OTHER functions (mutual TCO)
    !fn_def_has_mutual_tco(fd)
}

#[allow(dead_code)]
fn fn_def_has_mutual_tco(fd: &FnDef) -> bool {
    fn expr_has_other_tailcall(expr: &Expr, fn_name: &str) -> bool {
        match expr {
            Expr::TailCall(boxed) => boxed.as_ref().0 != fn_name,
            Expr::Match { arms, .. } => arms
                .iter()
                .any(|arm| expr_has_other_tailcall(&arm.body.node, fn_name)),
            _ => false,
        }
    }
    fd.body.stmts().iter().any(|s| match s {
        Stmt::Expr(e) => expr_has_other_tailcall(&e.node, &fd.name),
        Stmt::Binding(_, _, e) => expr_has_other_tailcall(&e.node, &fd.name),
    })
}

#[allow(dead_code)]
fn fn_def_has_tco(fd: &FnDef) -> bool {
    fn expr_has_self_tailcall(expr: &Expr, fn_name: &str) -> bool {
        match expr {
            Expr::TailCall(boxed) => boxed.as_ref().0 == fn_name,
            Expr::Match { arms, .. } => arms
                .iter()
                .any(|arm| expr_has_self_tailcall(&arm.body.node, fn_name)),
            _ => false,
        }
    }
    fd.body.stmts().iter().any(|s| match s {
        Stmt::Expr(e) => expr_has_self_tailcall(&e.node, &fd.name),
        Stmt::Binding(_, _, e) => expr_has_self_tailcall(&e.node, &fd.name),
    })
}

/// Compute borrow mask from a FnDef, checking for TCO.
/// Must mirror actual emitted signature:
/// - Mutual-TCO SCC members → wrapper with borrow-by-default (`&T`)
/// - Self-TCO only (not in SCC) → loop with `mut T`, all-false mask
/// - Non-TCO → borrow-by-default
fn borrow_mask_from_fn_def(fd: &FnDef, arg_count: usize, ctx: &CodegenContext) -> Vec<bool> {
    // Mutual-TCO members are emitted as wrappers with borrow-by-default,
    // even if they also have self-TailCalls. Don't skip borrow for them.
    if !ctx.mutual_tco_members.contains(&fd.name)
        && super::toplevel::body_has_self_tailcall(&fd.body, &fd.name)
    {
        return vec![false; arg_count];
    }
    fd.params
        .iter()
        .take(arg_count)
        .map(|(_, type_ann)| {
            let ty = crate::types::parse_type_str(type_ann);
            should_borrow_param(&ty)
        })
        .collect()
}

/// Look up whether the callee's i-th parameter is borrowed (`&T`).
/// Returns a Vec of booleans, one per parameter, indicating borrow status.
/// Uses fn_sigs first, falls back to FnDef type annotations from the AST.
/// TCO functions (self or mutual) never use borrow-by-default.
fn callee_borrow_mask(name: &str, arg_count: usize, ctx: &CodegenContext) -> Vec<bool> {
    // First, try to find the FnDef to check for TCO (which overrides everything)
    let fd = find_fn_def_by_name(name, ctx);
    if let Some(fd) = fd {
        return borrow_mask_from_fn_def(fd, arg_count, ctx);
    }

    // No FnDef found, try fn_sigs (type-checker resolved types)
    let lookup_name = if let Some((prefix, bare)) = resolve_module_call(name, ctx) {
        format!("{}.{}", prefix, bare)
    } else {
        name.to_string()
    };

    if let Some((param_types, _, _)) = ctx
        .fn_sigs
        .get(&lookup_name)
        .or_else(|| ctx.fn_sigs.get(name))
    {
        // Without FnDef we can't check TCO status, but self-TCO functions
        // always resolve via find_fn_def_by_name above, so this path is safe
        // for borrow-by-default on all non-scalar types including Named.
        return param_types
            .iter()
            .take(arg_count)
            .map(should_borrow_param)
            .collect();
    }

    // Unknown function -- don't borrow (conservative)
    vec![false; arg_count]
}

/// Find a FnDef by name, checking top-level, modules (qualified), and all modules (unqualified).
fn find_fn_def_by_name<'a>(name: &str, ctx: &'a CodegenContext) -> Option<&'a FnDef> {
    // Check top-level fn_defs
    if let Some(fd) = ctx.fn_defs.iter().find(|fd| fd.name == name) {
        return Some(fd);
    }

    // Check extra fn_defs (current module being emitted)
    if let Some(fd) = ctx.extra_fn_defs.iter().find(|fd| fd.name == name) {
        return Some(fd);
    }

    // Check module fn_defs for qualified calls
    if let Some((prefix, bare)) = resolve_module_call(name, ctx) {
        for module in &ctx.modules {
            if module.prefix == prefix
                && let Some(fd) = module.fn_defs.iter().find(|fd| fd.name == bare)
            {
                return Some(fd);
            }
        }
    }

    // Check all module fn_defs for unqualified calls (same-module calls)
    for module in &ctx.modules {
        if let Some(fd) = module.fn_defs.iter().find(|fd| fd.name == name) {
            return Some(fd);
        }
    }

    None
}

fn emit_named_function_call(
    name: &str,
    args: &[Expr],
    ctx: &CodegenContext,
    ectx: &EmitCtx,
) -> String {
    let arg_ctxs = compute_args_used_after_full(
        args,
        &ectx.used_after,
        &ectx.local_types,
        &ectx.rc_wrapped,
        &ectx.borrowed_params,
    );
    let borrow_mask = callee_borrow_mask(name, args.len(), ctx);
    let arg_strs: Vec<String> = args
        .iter()
        .enumerate()
        .zip(arg_ctxs.iter())
        .map(|((i, a), ac)| {
            if borrow_mask.get(i).copied().unwrap_or(false) {
                borrow_arg(a, ctx, ac)
            } else {
                clone_arg(a, ctx, ac)
            }
        })
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
    let arg_ctxs = compute_args_used_after_full(
        args,
        &ectx.used_after,
        &ectx.local_types,
        &ectx.rc_wrapped,
        &ectx.borrowed_params,
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
///
/// For borrowed params (`&T` from borrow-by-default), the default behavior is to
/// produce an owned clone. Call sites for user functions override this via `borrow_arg`.
pub(super) fn maybe_clone(code: String, expr: &Expr, ectx: &EmitCtx) -> String {
    match expr {
        Expr::Ident(name) => {
            if ectx.skip_clone(name) {
                code
            } else if ectx.is_rc_wrapped(name) {
                // Pass-through param (Rc<T> in self-TCO, &T in mutual-TCO trampoline).
                // For Rc<T>: `(*param).clone()` derefs Rc then clones inner T.
                // For &T: `(*param).clone()` derefs borrow then clones inner T.
                // Both produce owned T at the call site.
                format!("(*{}).clone()", code)
            } else if ectx.is_borrowed_param(name) {
                // Borrowed param: clone to produce owned T (e.g. for return position,
                // constructors, wrappers). User function calls use `borrow_arg` instead.
                format!("{}.clone()", code)
            } else {
                format!("{}.clone()", code)
            }
        }
        // `emit_expr` already encodes this as a compile_error! expression.
        Expr::Resolved(_) => code,
        // Field access on records: emit_expr produces `obj.field` without clone.
        // Clone here when ownership is needed (constructors, return, etc.).
        // When passed to a function expecting `&T`, `borrow_arg` handles it.
        Expr::Attr(obj, _field) => {
            // Builtin namespace access (e.g. Int.abs) → no clone needed
            if matches!(&obj.node, Expr::Ident(name) if is_builtin_namespace(name)) {
                code
            } else {
                format!("{}.clone()", code)
            }
        }
        _ => code,
    }
}

/// Is this expression known to produce a numeric (Int/Float) value?
/// Used to decide whether `+` needs `&` on the RHS (only strings do).
fn expr_is_numeric(expr: &Expr, ectx: &EmitCtx) -> bool {
    match expr {
        Expr::Literal(Literal::Int(_) | Literal::Float(_)) => true,
        Expr::Ident(name) => matches!(
            ectx.local_types.get(name.as_str()),
            Some(Type::Int | Type::Float)
        ),
        // Sub/Mul/Div always produce numeric results.
        Expr::BinOp(op, _, _) => !matches!(op, BinOp::Add),
        Expr::FnCall(fn_expr, _) => {
            if let Some(dotted) = expr_to_dotted_name(&fn_expr.node) {
                matches!(
                    dotted.as_str(),
                    "Int.abs"
                        | "Int.min"
                        | "Int.max"
                        | "Int.rem"
                        | "Float.abs"
                        | "Float.floor"
                        | "Float.ceil"
                        | "Float.round"
                        | "Float.min"
                        | "Float.max"
                        | "Float.sqrt"
                        | "Float.pow"
                        | "Float.sin"
                        | "Float.cos"
                        | "Float.atan2"
                        | "Float.fromInt"
                        | "Int.toFloat"
                        | "List.len"
                        | "Vector.len"
                        | "Map.len"
                        | "String.len"
                        | "String.byteLength"
                        | "Char.toCode"
                )
            } else {
                false
            }
        }
        _ => false,
    }
}

/// Is a record field access known to return a Copy type?
/// Looks up the record definition in the context to find the field's type.
fn attr_result_is_copy(obj: &Expr, field: &str, ctx: &CodegenContext, ectx: &EmitCtx) -> bool {
    let obj_type = match obj {
        Expr::Ident(name) => ectx.local_types.get(name),
        _ => None,
    };
    let record_name = match obj_type {
        Some(Type::Named(name)) => name.as_str(),
        _ => return false,
    };
    // Find record definition and look up field type.
    for td in ctx
        .type_defs
        .iter()
        .chain(ctx.modules.iter().flat_map(|m| m.type_defs.iter()))
    {
        if let TypeDef::Product { name, fields, .. } = td
            && name == record_name
            && let Some((_, type_ann)) = fields.iter().find(|(n, _)| n == field)
        {
            let ty = types::parse_type_str(type_ann);
            return super::liveness::is_copy_type(&ty);
        }
    }
    false
}

/// Emit an expression as a borrow for passing to a user function that takes `&T`.
/// For borrowed params: pass directly (already `&T`).
/// For owned locals: pass `&x`.
/// For Copy types: pass by value (no borrow needed).
/// For AverStr: pass by value (cheap clone).
pub(super) fn borrow_arg(expr: &Expr, ctx: &CodegenContext, ectx: &EmitCtx) -> String {
    let code = emit_expr(expr, ctx, ectx);
    match expr {
        Expr::Ident(name) => {
            if ectx.is_copy(name) {
                // Copy type: pass by value
                code
            } else if ectx
                .local_types
                .get(name)
                .is_some_and(|ty| matches!(ty, Type::Str))
            {
                // AverStr (Rc<str>): pass by value (cheap clone if needed)
                if ectx.can_move(name) || ectx.is_rc_wrapped(name) {
                    // If it's rc_wrapped, the outer code already handles deref
                    if ectx.is_rc_wrapped(name) {
                        format!("(*{}).clone()", code)
                    } else {
                        code
                    }
                } else {
                    format!("{}.clone()", code)
                }
            } else if ectx.is_borrowed_param(name) {
                // Already `&T` — pass directly
                code
            } else if ectx.is_rc_wrapped(name) {
                // Pass-through TCO param (Rc<T> or &T): deref to get &T
                format!("&*{}", code)
            } else {
                // Owned local: borrow it
                format!("&{}", code)
            }
        }
        _ => {
            // Complex expression: emit and borrow the result.
            // Check if the expression type needs borrowing by looking at what
            // the callee expects. For complex expressions, we produce a temporary
            // and borrow it.
            format!("&{}", code)
        }
    }
}

/// Emit an expression as a function argument, cloning variables to prevent move errors.
pub(super) fn clone_arg(expr: &Expr, ctx: &CodegenContext, ectx: &EmitCtx) -> String {
    clone_arg_with_options(expr, ctx, ectx, true)
}

fn clone_arg_with_options(
    expr: &Expr,
    ctx: &CodegenContext,
    ectx: &EmitCtx,
    allow_callsite_inlining: bool,
) -> String {
    let code = emit_expr_with_options(expr, ctx, ectx, allow_callsite_inlining);
    // Field access on record: check if the field type is Copy before cloning.
    if let Expr::Attr(obj, field) = expr
        && attr_result_is_copy(&obj.node, field, ctx, ectx)
    {
        return code;
    }
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
                let emitted = emit_expr(&expr.node, ctx, ectx);
                // Skip aver_display when the expression type is known to be displayable natively.
                // String, Int (i64), Float (f64), Bool all have Display impls matching aver_display.
                let arg = match expr_display_type(&expr.node, ectx) {
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

fn is_irrefutable_pattern(pat: &Pattern) -> bool {
    match pat {
        Pattern::Wildcard | Pattern::Ident(_) => true,
        Pattern::Tuple(pats) => pats.iter().all(is_irrefutable_pattern),
        _ => false,
    }
}

fn emit_match(subject: &Expr, arms: &[MatchArm], ctx: &CodegenContext, ectx: &EmitCtx) -> String {
    // Subject's used_after: all vars in arms + parent used_after
    let mut arms_vars = HashSet::new();
    for arm in arms {
        let mut arm_vars = collect_vars(&arm.body.node);
        let bindings = super::liveness::pattern_bindings(&arm.pattern);
        for b in &bindings {
            arm_vars.remove(b);
        }
        arms_vars.extend(arm_vars);
    }
    let subj_ectx = ectx.with_used_after(&arms_vars);

    // Single-arm irrefutable match → `let` destructuring instead of `match`.
    // e.g. `match x: (a, b) -> expr` → `{ let (a, b) = x; expr }`
    if arms.len() == 1 && is_irrefutable_pattern(&arms[0].pattern) {
        let arm = &arms[0];
        let subj = clone_arg(subject, ctx, &subj_ectx);
        let pat = emit_pattern(&arm.pattern, false, ctx);
        let body = maybe_clone(emit_expr(&arm.body.node, ctx, ectx), &arm.body.node, ectx);
        return match &arm.pattern {
            Pattern::Wildcard => body,
            Pattern::Ident(name) => {
                let name = aver_name_to_rust(name);
                format!("{{ let {} = {}; {} }}", name, subj, body)
            }
            _ => format!("{{ let {} = {}; {} }}", pat, subj, body),
        };
    }

    // For borrowed params, match on the reference directly instead of cloning,
    // but only when no arms have pattern bindings (destructuring produces &T refs
    // that would need clone rebindings across all match emission paths).
    let no_bindings = arms
        .iter()
        .all(|arm| super::liveness::pattern_bindings(&arm.pattern).is_empty());
    let match_on_ref =
        no_bindings && matches!(subject, Expr::Ident(name) if subj_ectx.is_borrowed_param(name));
    let subj = if match_on_ref {
        emit_expr(subject, ctx, &subj_ectx)
    } else {
        clone_arg(subject, ctx, &subj_ectx)
    };
    let dispatch_plan = classify_dispatch_plan_for_rust(arms, ctx, ectx);

    // Bool match → if/else: match expr { true => A, false => B } → if expr { A } else { B }
    // Always applied (not just optimized) since it generates strictly better code.
    if let Some(MatchDispatchPlan::Bool(shape)) = dispatch_plan.as_ref()
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
        return emit_list_match(subj, arms, list_shape, true, ctx, |arm| {
            maybe_clone(emit_expr(&arm.body.node, ctx, ectx), &arm.body.node, ectx)
        });
    }

    if let Some(MatchDispatchPlan::Table(shape)) = dispatch_plan.as_ref() {
        return emit_dispatch_table_match(subj, arms, shape, |arm| {
            maybe_clone(emit_expr(&arm.body.node, ctx, ectx), &arm.body.node, ectx)
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
        let body = maybe_clone(emit_expr(&arm.body.node, ctx, ectx), &arm.body.node, ectx);
        let mut rebindings = emit_pattern_rebindings(&arm.pattern, ctx);
        // When matching on a reference (borrowed param), bindings are &T.
        // Clone them to produce owned values expected by arm bodies.
        if match_on_ref {
            let ref_rebinds = emit_ref_match_rebindings(&arm.pattern);
            if !ref_rebinds.is_empty() {
                rebindings = format!("{}{}", ref_rebinds, rebindings);
            }
        }
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
            let rhs_vars = collect_vars(&rhs.node);
            let lhs_ectx = subj_ectx.with_used_after(&rhs_vars);
            let lhs_code = emit_expr(&lhs.node, ctx, &lhs_ectx);
            let rhs_code = emit_expr(&rhs.node, ctx, subj_ectx);
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
    let t = maybe_clone(emit_expr(&true_body.node, ctx, ectx), &true_body.node, ectx);
    let f = maybe_clone(
        emit_expr(&false_body.node, ctx, ectx),
        &false_body.node,
        ectx,
    );
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
    // Fast path: if all entries are wrapper tags (Result/Option), emit a Rust `match`
    // with move bindings instead of if-chain + clone.  This is both shorter and faster
    // because it avoids cloning the inner value.
    if let Some(code) = try_emit_wrapper_match(&subject, arms, shape, &body_for_arm) {
        return code;
    }

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

/// Emit `match subject { Ok(v) => ..., Err(e) => ... }` when ALL dispatch entries are
/// wrapper tags (Result.Ok/Err, Option.Some/None).  Bindings are by move (no clone).
fn try_emit_wrapper_match<F>(
    subject: &str,
    arms: &[MatchArm],
    shape: &DispatchTableShape,
    body_for_arm: &F,
) -> Option<String>
where
    F: Fn(&MatchArm) -> String,
{
    // All entries must be wrapper tags with payload bindings
    let all_wrappers = shape.entries.iter().all(|e| {
        matches!(
            e.pattern,
            SemanticDispatchPattern::WrapperTag(_) | SemanticDispatchPattern::NoneValue
        )
    });
    if !all_wrappers {
        return None;
    }

    let mut match_arms = Vec::new();
    for entry in &shape.entries {
        let arm = &arms[entry.arm_index];
        let body = body_for_arm(arm);
        match (&entry.pattern, &entry.binding) {
            (
                SemanticDispatchPattern::WrapperTag(kind),
                DispatchBindingPlan::WrapperPayload(name),
            ) => {
                let binding = aver_name_to_rust(name);
                let extractor = match kind {
                    WrapperKind::ResultOk => "Ok",
                    WrapperKind::ResultErr => "Err",
                    WrapperKind::OptionSome => "Some",
                };
                match_arms.push(format!("{extractor}({binding}) => {{ {body} }}"));
            }
            (SemanticDispatchPattern::WrapperTag(kind), DispatchBindingPlan::None) => {
                let pattern = match kind {
                    WrapperKind::ResultOk => "Ok(_)",
                    WrapperKind::ResultErr => "Err(_)",
                    WrapperKind::OptionSome => "Some(_)",
                };
                match_arms.push(format!("{pattern} => {{ {body} }}"));
            }
            (SemanticDispatchPattern::NoneValue, _) => {
                match_arms.push(format!("None => {{ {body} }}"));
            }
            _ => return None,
        }
    }

    // Default arm (wildcard)
    if let Some(default_arm) = &shape.default_arm {
        let arm = &arms[default_arm.arm_index];
        let body = body_for_arm(arm);
        if let Some(name) = &default_arm.binding_name {
            let binding = aver_name_to_rust(name);
            match_arms.push(format!("{binding} => {{ {body} }}"));
        } else {
            match_arms.push(format!("_ => {{ {body} }}"));
        }
    }

    Some(format!("match {} {{ {} }}", subject, match_arms.join(", ")))
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
    // Wrap arm bodies in braces only when they contain statements (return, let, etc.)
    // to avoid Rust's "unnecessary braces" warning on simple expressions.
    let wrap = |body: &str| -> String {
        if body.contains("return ") || body.contains("let ") || body.contains(';') {
            format!("{{ {} }}", body)
        } else {
            body.to_string()
        }
    };
    Some(format!(
        "aver_list_match!({}, [] => {}, [{}, {}] => {})",
        subject,
        wrap(&empty_body),
        head_name,
        tail_name,
        wrap(&cons_body)
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

/// When matching on a reference (`&T`), pattern bindings are `&Inner`.
/// Emit `let b = b.clone();` for each binding to produce owned values.
fn emit_ref_match_rebindings(pattern: &Pattern) -> String {
    let bindings = super::liveness::pattern_bindings(pattern);
    if bindings.is_empty() {
        return String::new();
    }
    let mut lines = Vec::new();
    for b in &bindings {
        let rb = super::syntax::aver_name_to_rust(b);
        lines.push(format!("let {} = {}.clone();", rb, rb));
    }
    format!("{}\n            ", lines.join("\n            "))
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

use super::expr::{aver_name_to_rust, clone_arg, emit_expr, emit_stmt};
use super::liveness::{EmitCtx, collect_vars, compute_args_used_after, compute_block_used_after};
use super::types::type_annotation_to_rust;
use crate::ast::*;
use crate::codegen::CodegenContext;
/// Top-level Aver items → Rust items (structs, enums, functions, tests).
use std::collections::HashMap;

/// Emit a Rust struct or enum from an Aver TypeDef.
pub fn emit_type_def(td: &TypeDef) -> String {
    match td {
        TypeDef::Sum { name, variants, .. } => emit_sum_type(name, variants),
        TypeDef::Product { name, fields, .. } => emit_product_type(name, fields),
    }
}

fn emit_sum_type(name: &str, variants: &[TypeVariant]) -> String {
    let mut lines = Vec::new();
    lines.push(format!(
        "#[derive(Clone, Debug, PartialEq)]\nenum {} {{",
        name
    ));
    for v in variants {
        if v.fields.is_empty() {
            lines.push(format!("    {},", v.name));
        } else {
            let field_types: Vec<String> = v
                .fields
                .iter()
                .map(|f| type_annotation_to_rust(f))
                .collect();
            lines.push(format!("    {}({}),", v.name, field_types.join(", ")));
        }
    }
    lines.push("}".to_string());

    // Generate AverDisplay impl
    lines.push(String::new());
    lines.push(format!("impl aver_rt::AverDisplay for {} {{", name));
    lines.push("    fn aver_display(&self) -> String {".to_string());
    lines.push("        match self {".to_string());
    for v in variants {
        if v.fields.is_empty() {
            lines.push(format!(
                "            {}::{} => \"{}\".to_string(),",
                name, v.name, v.name
            ));
        } else {
            let bindings: Vec<String> = (0..v.fields.len()).map(|i| format!("f{}", i)).collect();
            let display_parts: Vec<String> = bindings
                .iter()
                .map(|b| format!("{}.aver_display_inner()", b))
                .collect();
            lines.push(format!(
                "            {}::{}({}) => format!(\"{}({{}})\", vec![{}].join(\", \")),",
                name,
                v.name,
                bindings.join(", "),
                v.name,
                display_parts.join(", ")
            ));
        }
    }
    lines.push("        }".to_string());
    lines.push("    }".to_string());
    lines.push("    fn aver_display_inner(&self) -> String { self.aver_display() }".to_string());
    lines.push("}".to_string());

    lines.join("\n")
}

fn emit_product_type(name: &str, fields: &[(String, String)]) -> String {
    let mut lines = Vec::new();
    lines.push(format!(
        "#[derive(Clone, Debug, PartialEq)]\nstruct {} {{",
        name
    ));
    for (field_name, field_type) in fields {
        lines.push(format!(
            "    {}: {},",
            aver_name_to_rust(field_name),
            type_annotation_to_rust(field_type)
        ));
    }
    lines.push("}".to_string());

    // Generate AverDisplay impl
    lines.push(String::new());
    lines.push(format!("impl aver_rt::AverDisplay for {} {{", name));
    lines.push("    fn aver_display(&self) -> String {".to_string());
    let parts: Vec<String> = fields
        .iter()
        .map(|(field_name, _)| {
            format!(
                "format!(\"{}: {{}}\", self.{}.aver_display_inner())",
                field_name,
                aver_name_to_rust(field_name)
            )
        })
        .collect();
    lines.push(format!(
        "        format!(\"{}({{}})\", vec![{}].join(\", \"))",
        name,
        parts.join(", ")
    ));
    lines.push("    }".to_string());
    lines.push("    fn aver_display_inner(&self) -> String { self.aver_display() }".to_string());
    lines.push("}".to_string());

    lines.join("\n")
}

/// Build an EmitCtx for a function from its parameter types in fn_sigs.
fn build_fn_ectx(fd: &FnDef, ctx: &CodegenContext) -> EmitCtx {
    let mut local_types = HashMap::new();
    if let Some((param_types, _, _)) = ctx.fn_sigs.get(&fd.name) {
        for (i, (name, _)) in fd.params.iter().enumerate() {
            if let Some(ty) = param_types.get(i) {
                local_types.insert(name.clone(), ty.clone());
            }
        }
    } else {
        // Fallback: parse type annotations directly
        for (name, type_ann) in &fd.params {
            let ty = crate::types::parse_type_str(type_ann);
            local_types.insert(name.clone(), ty);
        }
    }
    EmitCtx::for_fn(local_types)
}

/// Emit a Rust function from an Aver FnDef.
pub fn emit_fn_def(fd: &FnDef, is_memo: bool, ctx: &CodegenContext) -> String {
    let mut lines = Vec::new();

    // Doc comment from description
    if let Some(desc) = &fd.desc {
        lines.push(format!("/// {}", desc));
    }

    // Check if function uses self-TCO (has TailCall to itself in body)
    let has_tco = body_has_self_tailcall(&fd.body, &fd.name);

    // Function signature
    let params = emit_fn_params(&fd.params, has_tco);
    let ret_type = if fd.return_type.is_empty() {
        "()".to_string()
    } else {
        type_annotation_to_rust(&fd.return_type)
    };

    let fn_name = aver_name_to_rust(&fd.name);

    let ectx = build_fn_ectx(fd, ctx);

    if is_memo {
        lines.push(emit_memo_fn(fd, &fn_name, &params, &ret_type, ctx, &ectx));
    } else if has_tco {
        lines.push(emit_tco_fn(fd, &fn_name, &ret_type, ctx, &ectx));
    } else {
        lines.push(format!("fn {}({}) -> {} {{", fn_name, params, ret_type));
        lines.push(emit_fn_body(&fd.body, ctx, &ectx));
        lines.push("}".to_string());
    }

    lines.join("\n")
}

fn emit_fn_params(params: &[(String, String)], mutable: bool) -> String {
    params
        .iter()
        .map(|(name, type_ann)| {
            let rust_type = type_annotation_to_rust(type_ann);
            let rust_name = aver_name_to_rust(name);
            if mutable {
                format!("mut {}: {}", rust_name, rust_type)
            } else {
                format!("{}: {}", rust_name, rust_type)
            }
        })
        .collect::<Vec<_>>()
        .join(", ")
}

fn emit_fn_body(body: &FnBody, ctx: &CodegenContext, ectx: &EmitCtx) -> String {
    match body {
        FnBody::Expr(expr) => {
            format!("    {}", emit_expr(expr, ctx, ectx))
        }
        FnBody::Block(stmts) => {
            // Compute per-statement used_after sets
            let stmt_ctxs = compute_block_used_after(stmts, &ectx.used_after, &ectx.local_types);
            let mut lines = Vec::new();
            for (i, stmt) in stmts.iter().enumerate() {
                let is_last = i == stmts.len() - 1;
                let sctx = &stmt_ctxs[i];
                match stmt {
                    Stmt::Binding(name, type_ann, _) => {
                        lines.push(format!("    {}", emit_stmt(stmt, ctx, sctx)));
                        // Track the binding type for subsequent statements
                        // (already handled by local_types propagation in sctx,
                        //  but we can enrich for bindings with type annotations)
                        let _ = (name, type_ann); // used for enrichment if needed
                    }
                    Stmt::Expr(expr) => {
                        if is_last {
                            // Last expression is the return value
                            lines.push(format!("    {}", emit_expr(expr, ctx, sctx)));
                        } else {
                            lines.push(format!("    {};", emit_expr(expr, ctx, sctx)));
                        }
                    }
                }
            }
            lines.join("\n")
        }
    }
}

/// Recursively check if an expression contains the `?` (ErrorProp) operator.
fn expr_uses_error_prop(expr: &Expr) -> bool {
    match expr {
        Expr::ErrorProp(_) => true,
        Expr::FnCall(f, args) => expr_uses_error_prop(f) || args.iter().any(expr_uses_error_prop),
        Expr::BinOp(_, l, r) => expr_uses_error_prop(l) || expr_uses_error_prop(r),
        Expr::Pipe(l, r) => expr_uses_error_prop(l) || expr_uses_error_prop(r),
        Expr::Match { subject, arms, .. } => {
            expr_uses_error_prop(subject) || arms.iter().any(|a| expr_uses_error_prop(&a.body))
        }
        Expr::List(es) => es.iter().any(expr_uses_error_prop),
        Expr::Tuple(es) => es.iter().any(expr_uses_error_prop),
        Expr::Attr(e, _) => expr_uses_error_prop(e),
        Expr::Constructor(_, Some(e)) => expr_uses_error_prop(e),
        Expr::InterpolatedStr(parts) => parts.iter().any(|p| match p {
            StrPart::Parsed(e) => expr_uses_error_prop(e),
            _ => false,
        }),
        Expr::RecordCreate { fields, .. } => fields.iter().any(|(_, e)| expr_uses_error_prop(e)),
        Expr::RecordUpdate { base, updates, .. } => {
            expr_uses_error_prop(base) || updates.iter().any(|(_, e)| expr_uses_error_prop(e))
        }
        _ => false,
    }
}

fn body_has_self_tailcall(body: &FnBody, fn_name: &str) -> bool {
    match body {
        FnBody::Expr(expr) => expr_has_self_tailcall(expr, fn_name),
        FnBody::Block(stmts) => stmts.iter().any(|s| match s {
            Stmt::Expr(e) => expr_has_self_tailcall(e, fn_name),
            Stmt::Binding(_, _, e) => expr_has_self_tailcall(e, fn_name),
        }),
    }
}

fn expr_has_self_tailcall(expr: &Expr, fn_name: &str) -> bool {
    match expr {
        Expr::TailCall(boxed) => {
            let (target, _) = boxed.as_ref();
            target == fn_name
        }
        Expr::Match { arms, .. } => arms
            .iter()
            .any(|arm| expr_has_self_tailcall(&arm.body, fn_name)),
        _ => false,
    }
}

/// Emit a function with TCO → loop rewrite.
fn emit_tco_fn(
    fd: &FnDef,
    fn_name: &str,
    ret_type: &str,
    ctx: &CodegenContext,
    ectx: &EmitCtx,
) -> String {
    let params = emit_fn_params(&fd.params, true);
    let mut lines = Vec::new();
    lines.push(format!("fn {}({}) -> {} {{", fn_name, params, ret_type));
    lines.push("    loop {".to_string());

    // Emit body with TailCall → { reassign; continue }
    let body_code = emit_tco_body(&fd.body, &fd.params, ctx, ectx);
    lines.push(body_code);

    lines.push("    }".to_string());
    lines.push("}".to_string());
    lines.join("\n")
}

fn emit_tco_body(
    body: &FnBody,
    params: &[(String, String)],
    ctx: &CodegenContext,
    ectx: &EmitCtx,
) -> String {
    match body {
        FnBody::Expr(expr) => {
            format!("        return {};", emit_tco_expr(expr, params, ctx, ectx))
        }
        FnBody::Block(stmts) => {
            // Compute per-statement used_after
            let stmt_ctxs = compute_block_used_after(stmts, &ectx.used_after, &ectx.local_types);
            let mut lines = Vec::new();
            for (i, stmt) in stmts.iter().enumerate() {
                let is_last = i == stmts.len() - 1;
                let sctx = &stmt_ctxs[i];
                match stmt {
                    Stmt::Binding(name, _, expr) => {
                        lines.push(format!(
                            "        let {} = {};",
                            aver_name_to_rust(name),
                            emit_expr(expr, ctx, sctx)
                        ));
                    }
                    Stmt::Expr(expr) => {
                        if is_last {
                            lines.push(format!(
                                "        return {};",
                                emit_tco_expr(expr, params, ctx, sctx)
                            ));
                        } else {
                            lines.push(format!("        {};", emit_expr(expr, ctx, sctx)));
                        }
                    }
                }
            }
            lines.join("\n")
        }
    }
}

fn emit_tco_expr(
    expr: &Expr,
    params: &[(String, String)],
    ctx: &CodegenContext,
    ectx: &EmitCtx,
) -> String {
    match expr {
        Expr::TailCall(boxed) => {
            let (target, args) = boxed.as_ref();

            // Mutual TCO (args count != params count) — emit as regular call
            if args.len() != params.len() {
                let func = aver_name_to_rust(target);
                let arg_ctxs = compute_args_used_after(args, &ectx.used_after, &ectx.local_types);
                let cloned: Vec<String> = args
                    .iter()
                    .zip(arg_ctxs.iter())
                    .map(|(a, ac)| clone_arg(a, ctx, ac))
                    .collect();
                return format!("return {}({})", func, cloned.join(", "));
            }

            // Self TCO — create temp vars, then reassign
            // For TailCall args: parameters will be overwritten, so they're NOT in used_after.
            // Only other args (to the right) contribute to used_after.
            let arg_ctxs =
                compute_args_used_after(args, &std::collections::HashSet::new(), &ectx.local_types);
            let arg_strs: Vec<String> = args
                .iter()
                .zip(arg_ctxs.iter())
                .map(|(a, ac)| emit_expr(a, ctx, ac))
                .collect();

            let mut lines = Vec::new();
            lines.push("{".to_string());
            for (i, arg_str) in arg_strs.iter().enumerate() {
                lines.push(format!("            let __tmp{} = {};", i, arg_str));
            }
            for (i, (name, _)) in params.iter().enumerate() {
                lines.push(format!(
                    "            {} = __tmp{};",
                    aver_name_to_rust(name),
                    i
                ));
            }
            lines.push("            continue;".to_string());
            lines.push("        }".to_string());
            lines.join("\n")
        }
        Expr::Match { subject, arms, .. } => {
            // Subject's used_after: all vars in arms + parent used_after
            let mut arms_vars = std::collections::HashSet::new();
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
            let needs_as_str = super::expr::has_string_literal_patterns(arms);
            let needs_as_slice = super::expr::has_list_patterns(arms);

            let match_expr = if needs_as_str {
                format!("{}.as_str()", subj)
            } else if needs_as_slice {
                format!("{}.as_slice()", subj)
            } else {
                subj
            };

            let mut arm_strs = Vec::new();
            for arm in arms {
                let pat = super::pattern::emit_pattern(&arm.pattern, needs_as_str, ctx);
                let body = emit_tco_expr(&arm.body, params, ctx, ectx);
                // For Cons patterns, rebind head and tail to owned types
                let rebindings = if let Pattern::Cons(head, tail) = &arm.pattern {
                    format!(
                        "{{ let {} = {}.clone(); let {} = {}.to_vec(); {} }}",
                        aver_name_to_rust(head),
                        aver_name_to_rust(head),
                        aver_name_to_rust(tail),
                        aver_name_to_rust(tail),
                        body
                    )
                } else {
                    body
                };
                arm_strs.push(format!("            {} => {}", pat, rebindings));
            }

            format!(
                "match {} {{\n{}\n        }}",
                match_expr,
                arm_strs.join(",\n")
            )
        }
        _ => emit_expr(expr, ctx, ectx),
    }
}

/// Emit a memoized function with thread_local cache.
fn emit_memo_fn(
    fd: &FnDef,
    fn_name: &str,
    _params_str: &str,
    ret_type: &str,
    ctx: &CodegenContext,
    ectx: &EmitCtx,
) -> String {
    let cache_name = fn_name.to_uppercase() + "_CACHE";

    // Build the key type and value type
    let param_types: Vec<String> = fd
        .params
        .iter()
        .map(|(_, ty)| type_annotation_to_rust(ty))
        .collect();

    let key_type = if param_types.len() == 1 {
        param_types[0].clone()
    } else {
        format!("({})", param_types.join(", "))
    };

    let param_names: Vec<String> = fd
        .params
        .iter()
        .map(|(n, _)| aver_name_to_rust(n))
        .collect();

    let key_expr = if param_names.len() == 1 {
        param_names[0].clone()
    } else {
        format!("({},)", param_names.join(", "))
    };

    let params = emit_fn_params(&fd.params, false);

    let mut lines = Vec::new();

    lines.push(format!(
        "thread_local! {{\n    static {}: std::cell::RefCell<HashMap<{}, {}>> = std::cell::RefCell::new(HashMap::new());\n}}",
        cache_name, key_type, ret_type
    ));
    lines.push(String::new());
    lines.push(format!("fn {}({}) -> {} {{", fn_name, params, ret_type));
    lines.push(format!("    {}.with(|cache| {{", cache_name));
    lines.push(format!(
        "        if let Some(r) = cache.borrow().get(&{}).cloned() {{ return r; }}",
        key_expr
    ));

    // Emit the actual body
    lines.push(format!(
        "        let __result = {{ {} }};",
        emit_memo_inner_body(&fd.body, ctx, ectx)
    ));
    lines.push(format!(
        "        cache.borrow_mut().insert({}, __result.clone());",
        key_expr
    ));
    lines.push("        __result".to_string());
    lines.push("    })".to_string());
    lines.push("}".to_string());

    lines.join("\n")
}

fn emit_memo_inner_body(body: &FnBody, ctx: &CodegenContext, ectx: &EmitCtx) -> String {
    match body {
        FnBody::Expr(expr) => emit_expr(expr, ctx, ectx),
        FnBody::Block(stmts) => {
            let stmt_ctxs = compute_block_used_after(stmts, &ectx.used_after, &ectx.local_types);
            let mut parts = Vec::new();
            for (i, stmt) in stmts.iter().enumerate() {
                let is_last = i == stmts.len() - 1;
                let sctx = &stmt_ctxs[i];
                match stmt {
                    Stmt::Binding(_, _, _) => parts.push(emit_stmt(stmt, ctx, sctx)),
                    Stmt::Expr(expr) => {
                        if is_last {
                            parts.push(emit_expr(expr, ctx, sctx));
                        } else {
                            parts.push(format!("{};", emit_expr(expr, ctx, sctx)));
                        }
                    }
                }
            }
            parts.join(" ")
        }
    }
}

/// Emit the main function, incorporating top-level statements.
pub fn emit_main(main_fn: Option<&FnDef>, top_stmts: &[&Stmt], ctx: &CodegenContext) -> String {
    let mut lines = Vec::new();
    let ectx = EmitCtx::empty();

    // Check if main returns a Result (needed for ? operator support)
    let returns_result = main_fn.map_or(false, |fd| fd.return_type.starts_with("Result<"));

    if returns_result {
        let ret_type = type_annotation_to_rust(&main_fn.unwrap().return_type);
        lines.push(format!("fn main() -> {} {{", ret_type));
    } else {
        lines.push("fn main() {".to_string());
    }

    // Top-level statements first
    for stmt in top_stmts {
        lines.push(format!("    {}", emit_stmt(stmt, ctx, &ectx)));
    }

    // Main function body
    if let Some(fd) = main_fn {
        let main_ectx = build_fn_ectx(fd, ctx);
        match &*fd.body {
            FnBody::Expr(expr) => {
                if returns_result {
                    lines.push(format!("    {}", emit_expr(expr, ctx, &main_ectx)));
                } else {
                    lines.push(format!("    {};", emit_expr(expr, ctx, &main_ectx)));
                }
            }
            FnBody::Block(stmts) => {
                let stmt_ctxs =
                    compute_block_used_after(stmts, &main_ectx.used_after, &main_ectx.local_types);
                for (i, stmt) in stmts.iter().enumerate() {
                    let is_last = i == stmts.len() - 1;
                    let sctx = &stmt_ctxs[i];
                    if is_last && returns_result {
                        // Last expression is the return value
                        match stmt {
                            Stmt::Binding(_, _, _) => {
                                lines.push(format!("    {}", emit_stmt(stmt, ctx, sctx)));
                            }
                            Stmt::Expr(expr) => {
                                lines.push(format!("    {}", emit_expr(expr, ctx, sctx)));
                            }
                        }
                    } else {
                        lines.push(format!("    {}", emit_stmt(stmt, ctx, sctx)));
                    }
                }
            }
        }
    }

    lines.push("}".to_string());
    lines.join("\n")
}

/// Emit verify blocks as Rust #[cfg(test)] module.
pub fn emit_verify_blocks(verify_blocks: &[&VerifyBlock], ctx: &CodegenContext) -> String {
    let mut lines = Vec::new();
    let ectx = EmitCtx::empty();

    lines.push("#[cfg(test)]".to_string());
    lines.push("mod tests {".to_string());
    lines.push("    use super::*;".to_string());
    lines.push(String::new());

    // Use per-function counters to handle multiple verify blocks for the same function
    let mut fn_counters: std::collections::HashMap<String, usize> =
        std::collections::HashMap::new();
    for vb in verify_blocks {
        for (_i, (left, right)) in vb.cases.iter().enumerate() {
            let fn_key = aver_name_to_rust(&vb.fn_name);
            let counter = fn_counters.entry(fn_key.clone()).or_insert(0);
            *counter += 1;
            let test_name = format!("test_{}_case_{}", fn_key, *counter);
            let left_str = emit_expr(left, ctx, &ectx);
            let right_str = emit_expr(right, ctx, &ectx);

            // Check if either side uses `?` operator
            let uses_error_prop = expr_uses_error_prop(left) || expr_uses_error_prop(right);

            lines.push("    #[test]".to_string());
            if uses_error_prop {
                lines.push(format!("    fn {}() -> Result<(), String> {{", test_name));
                lines.push(format!("        assert_eq!({}, {});", left_str, right_str));
                lines.push("        Ok(())".to_string());
            } else {
                lines.push(format!("    fn {}() {{", test_name));
                lines.push(format!("        assert_eq!({}, {});", left_str, right_str));
            }
            lines.push("    }".to_string());
            lines.push(String::new());
        }
    }

    lines.push("}".to_string());
    lines.join("\n")
}

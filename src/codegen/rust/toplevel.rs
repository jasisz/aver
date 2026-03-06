use super::expr::{aver_name_to_rust, clone_arg, emit_expr, emit_stmt};
use super::liveness::{EmitCtx, collect_vars, compute_args_used_after, compute_block_used_after};
use super::types::type_annotation_to_rust;
use crate::ast::*;
use crate::codegen::CodegenContext;
/// Top-level Aver items → Rust items (structs, enums, functions, tests).
use std::collections::HashMap;
use std::fmt::Write as _;

/// Emit a Rust struct or enum from an Aver TypeDef.
pub fn emit_type_def(td: &TypeDef) -> String {
    match td {
        TypeDef::Sum { name, variants, .. } => emit_sum_type(name, variants),
        TypeDef::Product { name, fields, .. } => emit_product_type(name, fields),
    }
}

fn emit_sum_type(name: &str, variants: &[TypeVariant]) -> String {
    let mut out = String::new();
    writeln!(out, "#[derive(Clone, Debug, PartialEq)]").unwrap();
    writeln!(out, "enum {} {{", name).unwrap();
    for v in variants {
        if v.fields.is_empty() {
            writeln!(out, "    {},", v.name).unwrap();
        } else {
            let field_types: Vec<String> = v
                .fields
                .iter()
                .map(|f| {
                    let rust_ty = type_annotation_to_rust(f);
                    if f == name {
                        format!("Box<{}>", rust_ty)
                    } else {
                        rust_ty
                    }
                })
                .collect();
            writeln!(out, "    {}({}),", v.name, field_types.join(", ")).unwrap();
        }
    }
    writeln!(out, "}}").unwrap();

    // Generate AverDisplay impl
    writeln!(out).unwrap();
    writeln!(out, "impl aver_rt::AverDisplay for {} {{", name).unwrap();
    writeln!(out, "    fn aver_display(&self) -> String {{").unwrap();
    writeln!(out, "        match self {{").unwrap();
    for v in variants {
        if v.fields.is_empty() {
            writeln!(
                out,
                "            {}::{} => \"{}\".to_string(),",
                name, v.name, v.name
            )
            .unwrap();
        } else {
            let bindings: Vec<String> = (0..v.fields.len()).map(|i| format!("f{}", i)).collect();
            let display_parts: Vec<String> = bindings
                .iter()
                .map(|b| format!("{}.aver_display_inner()", b))
                .collect();
            writeln!(
                out,
                "            {}::{}({}) => format!(\"{}({{}})\", vec![{}].join(\", \")),",
                name,
                v.name,
                bindings.join(", "),
                v.name,
                display_parts.join(", ")
            )
            .unwrap();
        }
    }
    writeln!(out, "        }}").unwrap();
    writeln!(out, "    }}").unwrap();
    writeln!(
        out,
        "    fn aver_display_inner(&self) -> String {{ self.aver_display() }}"
    )
    .unwrap();
    writeln!(out, "}}").unwrap();

    out.trim_end().to_string()
}

fn emit_product_type(name: &str, fields: &[(String, String)]) -> String {
    let mut out = String::new();
    writeln!(out, "#[derive(Clone, Debug, PartialEq)]").unwrap();
    writeln!(out, "struct {} {{", name).unwrap();
    for (field_name, field_type) in fields {
        writeln!(
            out,
            "    {}: {},",
            aver_name_to_rust(field_name),
            type_annotation_to_rust(field_type)
        )
        .unwrap();
    }
    writeln!(out, "}}").unwrap();

    // Generate AverDisplay impl
    writeln!(out).unwrap();
    writeln!(out, "impl aver_rt::AverDisplay for {} {{", name).unwrap();
    writeln!(out, "    fn aver_display(&self) -> String {{").unwrap();
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
    writeln!(
        out,
        "        format!(\"{}({{}})\", vec![{}].join(\", \"))",
        name,
        parts.join(", ")
    )
    .unwrap();
    writeln!(out, "    }}").unwrap();
    writeln!(
        out,
        "    fn aver_display_inner(&self) -> String {{ self.aver_display() }}"
    )
    .unwrap();
    writeln!(out, "}}").unwrap();

    out.trim_end().to_string()
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
                .map(|(a, ac)| clone_arg(a, ctx, ac))
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
            if super::expr::has_list_patterns(arms) {
                return super::expr::emit_list_match(subj, arms, ctx, |arm| {
                    emit_tco_expr(&arm.body, params, ctx, ectx)
                });
            }

            let match_expr = if needs_as_str {
                format!("{}.as_str()", subj)
            } else {
                subj
            };

            let mut arm_strs = Vec::new();
            for arm in arms {
                let pat = super::pattern::emit_pattern(&arm.pattern, needs_as_str, ctx);
                let body = emit_tco_expr(&arm.body, params, ctx, ectx);
                let mut rebinding_lines: Vec<String> = Vec::new();
                if let Pattern::Cons(head, tail) = &arm.pattern {
                    if head != "_" {
                        let h = aver_name_to_rust(head);
                        rebinding_lines.push(format!("let {} = {}.clone();", h, h));
                    }
                    let _ = tail;
                }
                if let Pattern::Constructor(name, bindings) = &arm.pattern {
                    for b in super::expr::constructor_boxed_bindings(name, bindings, ctx) {
                        let b = aver_name_to_rust(&b);
                        rebinding_lines.push(format!("let {} = (*{}).clone();", b, b));
                    }
                }
                let rebindings = if rebinding_lines.is_empty() {
                    body
                } else {
                    format!("{{ {} {} }}", rebinding_lines.join(" "), body)
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

    let mut out = String::new();
    writeln!(out, "thread_local! {{").unwrap();
    writeln!(
        out,
        "    static {}: std::cell::RefCell<HashMap<{}, {}>> = std::cell::RefCell::new(HashMap::new());",
        cache_name, key_type, ret_type
    )
    .unwrap();
    writeln!(out, "}}").unwrap();
    writeln!(out).unwrap();
    writeln!(out, "fn {}({}) -> {} {{", fn_name, params, ret_type).unwrap();
    writeln!(out, "    {}.with(|cache| {{", cache_name).unwrap();
    writeln!(
        out,
        "        if let Some(r) = cache.borrow().get(&{}).cloned() {{ return r; }}",
        key_expr
    )
    .unwrap();

    // Emit the actual body
    writeln!(
        out,
        "        let __result = {{ {} }};",
        emit_memo_inner_body(&fd.body, ctx, ectx)
    )
    .unwrap();
    writeln!(
        out,
        "        cache.borrow_mut().insert({}, __result.clone());",
        key_expr
    )
    .unwrap();
    writeln!(out, "        __result").unwrap();
    writeln!(out, "    }})").unwrap();
    writeln!(out, "}}").unwrap();

    out.trim_end().to_string()
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
    let mut out = String::new();
    let ectx = EmitCtx::empty();

    // Check if main returns a Result (needed for ? operator support)
    let returns_result = main_fn.is_some_and(|fd| fd.return_type.starts_with("Result<"));

    if returns_result {
        let ret_type = type_annotation_to_rust(&main_fn.unwrap().return_type);
        writeln!(out, "fn main() -> {} {{", ret_type).unwrap();
    } else {
        writeln!(out, "fn main() {{").unwrap();
    }

    // Top-level statements first
    for stmt in top_stmts {
        writeln!(out, "    {}", emit_stmt(stmt, ctx, &ectx)).unwrap();
    }

    // Main function body
    if let Some(fd) = main_fn {
        let main_ectx = build_fn_ectx(fd, ctx);
        match &*fd.body {
            FnBody::Expr(expr) => {
                if returns_result {
                    writeln!(out, "    {}", emit_expr(expr, ctx, &main_ectx)).unwrap();
                } else {
                    writeln!(out, "    {};", emit_expr(expr, ctx, &main_ectx)).unwrap();
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
                                writeln!(out, "    {}", emit_stmt(stmt, ctx, sctx)).unwrap();
                            }
                            Stmt::Expr(expr) => {
                                writeln!(out, "    {}", emit_expr(expr, ctx, sctx)).unwrap();
                            }
                        }
                    } else {
                        writeln!(out, "    {}", emit_stmt(stmt, ctx, sctx)).unwrap();
                    }
                }
            }
        }
    }

    writeln!(out, "}}").unwrap();
    out.trim_end().to_string()
}

/// Emit verify blocks as Rust #[cfg(test)] module.
pub fn emit_verify_blocks(verify_blocks: &[&VerifyBlock], ctx: &CodegenContext) -> String {
    let mut out = String::new();
    let ectx = EmitCtx::empty();

    writeln!(out, "#[cfg(test)]").unwrap();
    writeln!(out, "mod tests {{").unwrap();
    writeln!(out, "    use super::*;").unwrap();
    writeln!(out).unwrap();

    // Use per-function counters to handle multiple verify blocks for the same function
    let mut fn_counters: std::collections::HashMap<String, usize> =
        std::collections::HashMap::new();
    for vb in verify_blocks {
        for (left, right) in vb.cases.iter() {
            let fn_key = aver_name_to_rust(&vb.fn_name);
            let counter = fn_counters.entry(fn_key.clone()).or_insert(0);
            *counter += 1;
            let test_name = format!("test_{}_case_{}", fn_key, *counter);
            let left_str = emit_expr(left, ctx, &ectx);
            let right_str = emit_expr(right, ctx, &ectx);

            // Check if either side uses `?` operator
            let uses_error_prop = expr_uses_error_prop(left) || expr_uses_error_prop(right);

            writeln!(out, "    #[test]").unwrap();
            if uses_error_prop {
                writeln!(out, "    fn {}() -> Result<(), String> {{", test_name).unwrap();
                writeln!(out, "        assert_eq!({}, {});", left_str, right_str).unwrap();
                writeln!(out, "        Ok(())").unwrap();
            } else {
                writeln!(out, "    fn {}() {{", test_name).unwrap();
                writeln!(out, "        assert_eq!({}, {});", left_str, right_str).unwrap();
            }
            writeln!(out, "    }}").unwrap();
            writeln!(out).unwrap();
        }
    }

    writeln!(out, "}}").unwrap();
    out.trim_end().to_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinOp, Expr, FnBody, FnDef};
    use crate::codegen::CodegenContext;
    use std::collections::{HashMap, HashSet};
    use std::rc::Rc;

    fn empty_ctx() -> CodegenContext {
        CodegenContext {
            items: vec![],
            fn_sigs: HashMap::new(),
            memo_fns: HashSet::new(),
            memo_safe_types: HashSet::new(),
            type_defs: vec![],
            fn_defs: vec![],
            project_name: "test".to_string(),
            modules: vec![],
            module_prefixes: HashSet::new(),
            policy: None,
        }
    }

    fn list_param_fn(name: &str, params: Vec<(&str, &str)>) -> FnDef {
        FnDef {
            name: name.to_string(),
            line: 1,
            params: params
                .into_iter()
                .map(|(n, ty)| (n.to_string(), ty.to_string()))
                .collect(),
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::Expr(Expr::Literal(crate::ast::Literal::Int(0)))),
            resolution: None,
        }
    }

    #[test]
    fn self_tco_clones_param_reused_in_later_arg() {
        let ctx = empty_ctx();
        let fd = list_param_fn(
            "repeatSum",
            vec![("xs", "List<Int>"), ("remaining", "Int"), ("sink", "Int")],
        );
        let ectx = build_fn_ectx(&fd, &ctx);
        let expr = Expr::TailCall(Box::new((
            "repeatSum".to_string(),
            vec![
                Expr::Ident("xs".to_string()),
                Expr::BinOp(
                    BinOp::Sub,
                    Box::new(Expr::Ident("remaining".to_string())),
                    Box::new(Expr::Literal(crate::ast::Literal::Int(1))),
                ),
                Expr::BinOp(
                    BinOp::Add,
                    Box::new(Expr::Ident("sink".to_string())),
                    Box::new(Expr::FnCall(
                        Box::new(Expr::Ident("sumList".to_string())),
                        vec![
                            Expr::Ident("xs".to_string()),
                            Expr::Literal(crate::ast::Literal::Int(0)),
                        ],
                    )),
                ),
            ],
        )));

        let code = emit_tco_expr(&expr, &fd.params, &ctx, &ectx);
        assert!(code.contains("let __tmp0 = xs.clone();"));
        assert!(code.contains("let __tmp1 = (remaining - 1i64);"));
        assert!(code.contains("let __tmp2 = (sink + &sumList(xs, 0i64));"));
    }

    #[test]
    fn self_tco_clones_multiple_list_params_reused_in_later_arg() {
        let ctx = empty_ctx();
        let fd = list_param_fn(
            "repeatAppend",
            vec![
                ("a", "List<Int>"),
                ("b", "List<Int>"),
                ("remaining", "Int"),
                ("sink", "Int"),
            ],
        );
        let ectx = build_fn_ectx(&fd, &ctx);
        let expr = Expr::TailCall(Box::new((
            "repeatAppend".to_string(),
            vec![
                Expr::Ident("a".to_string()),
                Expr::Ident("b".to_string()),
                Expr::BinOp(
                    BinOp::Sub,
                    Box::new(Expr::Ident("remaining".to_string())),
                    Box::new(Expr::Literal(crate::ast::Literal::Int(1))),
                ),
                Expr::BinOp(
                    BinOp::Add,
                    Box::new(Expr::Ident("sink".to_string())),
                    Box::new(Expr::FnCall(
                        Box::new(Expr::Ident("List.len".to_string())),
                        vec![Expr::FnCall(
                            Box::new(Expr::Ident("appendLists".to_string())),
                            vec![Expr::Ident("a".to_string()), Expr::Ident("b".to_string())],
                        )],
                    )),
                ),
            ],
        )));

        let code = emit_tco_expr(&expr, &fd.params, &ctx, &ectx);
        assert!(code.contains("let __tmp0 = a.clone();"));
        assert!(code.contains("let __tmp1 = b.clone();"));
        assert!(code.contains("let __tmp3 = (sink + &(appendLists(a, b).len() as i64));"));
    }
}
